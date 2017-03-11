unit JD.Power.Server;

(*
  JD Power Server Thread
  - Runs on all client computers which may need to receive shutdown command
  - HTTP Server listening for incoming shutdown command
  - Monitors UPS battery power, watches for power loss
  - Automatically hibernates this computer and any others upon power loss
*)

interface

uses
  System.Classes, System.SysUtils, System.SyncObjs,
  Winapi.Windows, Winapi.ActiveX, Winapi.ShellAPI, Winapi.Messages,
  IdHTTP, IdHTTPServer, IdTCPConnection, IdCustomHTTPServer, IdTCPServer,
  IdCustomTCPServer, IdContext, IdGlobal, IdYarn, IdThread,
  SuperObject,
  JD.Power.Common,
  JD.Power.Monitor;

type
  TJDPowerServerContext = class;
  TJDPowerServerThread = class;



  TJDPowerServerThread = class(TThread)
  private
    FSvr: TIdHTTPServer;
    FCli: TIdHTTP;
    FConfig: ISuperObject;
    FMon: TPowerMonitor;
    FSrc: TPowerSource;
    FPerc: Single;
    FWndClass: WNDCLASS;
    procedure Init;
    procedure Uninit;
    procedure Process;
    procedure LoadConfig;
    procedure HandleCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetStatus(AContext: TJDPowerServerContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandlePostCommand(AContext: TJDPowerServerContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      const O: ISuperObject);
    function GetStatusObj: ISuperObject;
    procedure PowerBatteryPercent(Sender: TObject; const Perc: Single);
    procedure PowerSourceChange(Sender: TObject; const Src: TPowerSource);
    procedure DoHibernate;
    procedure CheckHibernate;
    procedure ProcessMessages;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TJDPowerServerContext = class(TIdServerContext)
  private

  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
      AList: TIdContextThreadList = nil); override;
  end;

implementation

{ TJDPowerServerThread }

constructor TJDPowerServerThread.Create;
begin
  inherited Create(True);
  FConfig:= nil;
end;

destructor TJDPowerServerThread.Destroy;
begin
  if Assigned(FConfig) then begin
    FConfig._Release;
    FConfig:= nil;
  end;
  inherited;
end;

procedure TJDPowerServerThread.Init;
begin
  CoInitialize(nil);
  FSvr:= TIdHTTPServer.Create(nil);
  FSvr.ContextClass:= TJDPowerServerContext;
  FSvr.OnCommandGet:= HandleCommand;
  FSvr.OnCommandOther:= HandleCommand;
  LoadConfig;
  FCli:= TIdHTTP.Create(nil);
  FCli.HandleRedirects:= True;
  FMon:= TPowerMonitor.Create(nil);
  FMon.OnSourceChange:= PowerSourceChange;
  FMon.OnBatteryPercent:= PowerBatteryPercent;
  FMon.Settings:= [psACDCPowerSource, psBatteryPercentage,
    psConsoleDisplayState, psGlobalUserPresence, psIdleBackgroundTask,
    psMonitorPower, psPowerSaving, psPowerSchemePersonality,
    psSessionDisplayStatus, psSessionUserPresence, psSystemAwayMode];
end;

procedure TJDPowerServerThread.Uninit;
begin
  FreeAndNil(FMon);
  FSvr.Active:= False;
  FreeAndNil(FCli);
  FreeAndNil(FSvr);
  CoUninitialize;
end;

procedure TJDPowerServerThread.PowerSourceChange(Sender: TObject;
  const Src: TPowerSource);
begin
  FSrc:= Src;
  CheckHibernate;
end;

procedure TJDPowerServerThread.PowerBatteryPercent(Sender: TObject;
  const Perc: Single);
begin
  FPerc:= Perc;
  CheckHibernate;
end;

procedure TJDPowerServerThread.CheckHibernate;
begin
  if FSrc <> TPowerSource.poAC then begin
    if FPerc <= FConfig.I['battery_threshold'] then begin
      //UPS is on DC power and battery is below threshold - need to hibernate
      DoHibernate;
    end;
  end;
end;

procedure TJDPowerServerThread.DoHibernate;
var
  A: TSuperArray;
  X: Integer;
  M: String;
begin
  //TODO: Loop through other computers and send command to them first
  A:= FConfig.A['machines'];
  if Assigned(A) then begin
    for X := 0 to A.Length-1 do begin
      M:= A.S[X];
      SendShutdownCmd(M, TShutdownCmd.scHibernate);
    end;
  end;

  //Then, send command to this computer to hibernate
  DoShutdownCmd(TShutdownCmd.scHibernate);

end;

procedure TJDPowerServerThread.LoadConfig;
var
  FN: String;
  L: TStringList;
  procedure SaveDef;
  begin
    FConfig:= SO;
    FConfig.S['display_name']:= 'Untitled Machine';
    FConfig.I['listen_port']:= CLIENT_PORT;
    FConfig.I['battery_threshold']:= 20;
    FConfig.O['machines']:= SA([]);

    L.Text:= FConfig.AsJSon(True);
    L.SaveToFile(FN);
  end;
begin
  FSvr.Active:= False;
  try
    if Assigned(FConfig) then begin
      FConfig._Release;
      FConfig:= nil;
    end;

    FN:= ExtractFilePath(ParamStr(0));
    FN:= IncludeTrailingPathDelimiter(FN);
    FN:= FN + 'JDPowerServer.json';
    L:= TStringList.Create;
    try
      if FileExists(FN) then begin
        L.LoadFromFile(FN);
        FConfig:= SO(L.Text);
        if not Assigned(FConfig) then begin
          SaveDef;
        end;
      end else begin
        SaveDef;
      end;
      FConfig._AddRef;

      FSvr.Bindings.Clear;
      FSvr.Bindings.Add.SetBinding('', FConfig.I['listen_port'], TIdIPVersion.Id_IPv4);

    finally
      FreeAndNil(L);
    end;
  finally
    FSvr.Active:= True;
  end;
end;

procedure TJDPowerServerThread.ProcessMessages;
var
  Msg: TMsg;
begin
  while GetMessage(Msg, FWnd, 0, 0) > 0 do
  begin
    if Msg.message = WM_DATA_AVA then begin
      MessageBox(0, 'Data Available', 'Test', 0);
    end else
    begin
      TranslateMessage(msg);
      DispatchMessage(msg)
    end;
  end;
end;

procedure TJDPowerServerThread.Execute;
var
  X: Integer;
  inMess: Msg;
begin
  if RegisterClass(FWndClass) = 0 then Exit;
  FWnd := CreateWindow(FWndClass.lpszClassName, PChar(FTitle), WS_DLGFRAME, XPos, YPos, 698, 517, 0, 0, HInstance, nil);
  if FWnd = 0 then Exit;

  Init;
  try
    while not Terminated do begin
      try
        ProcessMessages;
        Process;
      except
        on E: Exception do begin
          //TODO: Log exception
        end;
      end;
      for X := 1 to 5 do begin
        if Terminated then Break;
        Sleep(500);
        if Terminated then Break;
        Sleep(500);
      end;
    end;
  finally
    Uninit;
  end;
end;

procedure TJDPowerServerThread.HandleCommand(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  R: String;
  C: TJDPowerServerContext;
  O: ISuperObject;
  function IsReq(const S: String): Boolean;
  begin
    Result:= SameText(S, R);
  end;
begin
  //Received any type of command
  C:= TJDPowerServerContext(AContext);
  R:= ARequestInfo.Document + '/';
  Delete(R, 1, 1);
  R:= Copy(R, 1, Pos('/', R)-1);
  case ARequestInfo.CommandType of
    hcGET: begin
      if IsReq('Status') then begin
        HandleGetStatus(C, ARequestInfo, AResponseInfo);
      end else
      if IsReq('Drives') then begin

      end else
      if IsReq('Directory') then begin

      end else
      if IsReq('File') then begin

      end else begin
        //TODO: Handle invalid request
      end;
    end;
    hcPOST: begin
      O:= TSuperObject.ParseStream(ARequestInfo.PostStream, False);
      if IsReq('Command') then begin
        HandlePostCommand(C, ARequestInfo, AResponseInfo, O);
      end else begin
        //TODO: Handle invalid request
      end;
    end;
    else begin
      //Unsupported command
    end;
  end;
end;

procedure TJDPowerServerThread.HandleGetStatus(AContext: TJDPowerServerContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  R: ISuperObject;
begin
  R:= GetStatusObj;
  AResponseInfo.ContentText:= R.AsJSon(True);
  AResponseInfo.ContentType:= 'application/json';
end;

procedure TJDPowerServerThread.HandlePostCommand(
  AContext: TJDPowerServerContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; const O: ISuperObject);
var
  Cmd: String;
  R: ISuperObject;
  Suc: Boolean;
  function IsCmd(const S: String): Boolean;
  begin
    Result:= SameText(Cmd, S);
  end;
begin
  Suc:= False;

  if not Assigned(O) then begin
    //TODO: Log error - invalid JSON object
    Exit;
  end;


  R:= SO;
  try
    Cmd:= O.S['cmd'];
    if IsCmd('Shutdown') then begin
      Suc:= DoShutdownCmd(TShutdownCmd.scShutdown, O.B['hybrid'], O.B['force'],
        O.I['time'], O.S['comment'], TShutdownReason.srUnexpected, 0, 0, '');
    end else
    if IsCmd('Restart') then begin
      Suc:= DoShutdownCmd(TShutdownCmd.scRestart, O.B['hybrid'], O.B['force'],
        O.I['time'], O.S['comment'], TShutdownReason.srUnexpected, 0, 0, '');
    end else
    if IsCmd('Hibernate') then begin
      Suc:= DoShutdownCmd(TShutdownCmd.scHibernate, O.B['hybrid'], O.B['force'],
        O.I['time'], O.S['comment'], TShutdownReason.srUnexpected, 0, 0, '');
    end else
    if IsCmd('Sleep') then begin
      //TODO: Unsupported
    end else
    if IsCmd('LogOff') then begin
      //TODO: Unsupported
    end else
    if IsCmd('Lock') then begin
      //TODO: Unsupported
    end else begin
      //TODO: Unsupported
    end;

    R.B['success']:= Suc;

  finally
    AResponseInfo.ContentText:= R.AsJSon(True);
    AResponseInfo.ContentType:= 'application/json';
  end;

end;

function TJDPowerServerThread.GetStatusObj: ISuperObject;
begin
  //TODO: Return current snapshot of computer's status
  Result:= SO;
  Result.B['online']:= True;
  Result.D['timestamp']:= Now;
  //Result.S['machine']:= ''; //TODO
  //Result.S['ip']:= ''; //TODO
  //Result.D['uptime']:= 0; //TODO
  Result.S['display_name']:= FConfig.S['display_name'];
  //TODO


end;

procedure TJDPowerServerThread.Process;
begin

end;

{ TJDPowerServerContext }

constructor TJDPowerServerContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;

end;

end.
