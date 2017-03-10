unit JD.Power.Server;

(*
  JD Remote Shutdown Server Thread
  - Runs on all client computers which may need to receive shutdown command
  - HTTP Server listening for incoming shutdown command
*)

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Winapi.WIndows,
  ActiveX,
  IdHTTP, IdHTTPServer, IdTCPConnection, IdCustomHTTPServer, IdTCPServer, IdCustomTCPServer,
  IdContext, IdGlobal,
  IdYarn, IdThread,
  SuperObject,
  ShellAPI,
  JD.Power.Common,
  JD.Power.Monitor;

type
  TRemoteShutdownServerContext = class;
  TRemoteShutdownServer = class;



  TRemoteShutdownServer = class(TThread)
  private
    FSvr: TIdHTTPServer;
    FCli: TIdHTTP;
    FConfig: ISuperObject;
    FMon: TPowerMonitor;
    procedure Init;
    procedure Uninit;
    procedure Process;
    procedure LoadConfig;
    procedure HandleCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetStatus(AContext: TRemoteShutdownServerContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandlePostCommand(AContext: TRemoteShutdownServerContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      const O: ISuperObject);
    function GetStatusObj: ISuperObject;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function ServerURL(const ARes: String = ''): String;
  end;

  TRemoteShutdownServerContext = class(TIdServerContext)
  private

  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
      AList: TIdContextThreadList = nil); override;
  end;

implementation

{ TRemoteShutdownServer }

constructor TRemoteShutdownServer.Create;
begin
  inherited Create(True);
  FConfig:= nil;
end;

destructor TRemoteShutdownServer.Destroy;
begin
  if Assigned(FConfig) then begin
    FConfig._Release;
    FConfig:= nil;
  end;
  inherited;
end;

procedure TRemoteShutdownServer.Init;
begin
  CoInitialize(nil);
  FSvr:= TIdHTTPServer.Create(nil);
  FSvr.ContextClass:= TRemoteShutdownServerContext;
  FSvr.OnCommandGet:= HandleCommand;
  FSvr.OnCommandOther:= HandleCommand;
  LoadConfig;
  FSvr.Active:= True;
  FCli:= TIdHTTP.Create(nil);
  FCli.HandleRedirects:= True;
end;

procedure TRemoteShutdownServer.Uninit;
begin
  FSvr.Active:= False;
  FreeAndNil(FCli);
  FreeAndNil(FSvr);
  CoUninitialize;
end;

procedure TRemoteShutdownServer.LoadConfig;
var
  FN: String;
  L: TStringList;
  O: ISuperObject;
  procedure SaveDef;
  begin
    O:= SO;
    O.S['global_host']:= 'LocalHost';
    O.I['global_port']:= GLOBAL_PORT;
    O.S['display_name']:= 'Untitled Machine';
    O.I['listen_port']:= CLIENT_PORT;
    O.O['machines']:= SA([]);

    L.Text:= O.AsJSon(True);
    L.SaveToFile(FN);
  end;
begin
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
      O:= SO(L.Text);
      if not Assigned(O) then begin
        SaveDef;
      end;
    end else begin
      SaveDef;
    end;

    FConfig:= O;
    FConfig._AddRef;
    FSvr.Bindings.Clear;
    FSvr.Bindings.Add.SetBinding('', O.I['listen_port'], TIdIPVersion.Id_IPv4);

    //TODO: Load list of other machines which are to be shut down if this one is

  finally
    L.Free;
  end;
end;

procedure TRemoteShutdownServer.Execute;
var
  X: Integer;
begin
  Init;
  try
    while not Terminated do begin
      try
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

procedure TRemoteShutdownServer.HandleCommand(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  R: String;
  C: TRemoteShutdownServerContext;
  O: ISuperObject;
  function IsReq(const S: String): Boolean;
  begin
    Result:= SameText(S, R);
  end;
begin
  //Received any type of command from the global server
  C:= TRemoteShutdownServerContext(AContext);
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

procedure TRemoteShutdownServer.HandleGetStatus(AContext: TRemoteShutdownServerContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  R: ISuperObject;
begin
  R:= GetStatusObj;
  AResponseInfo.ContentText:= R.AsJSon(True);
  AResponseInfo.ContentType:= 'application/json';
end;

procedure TRemoteShutdownServer.HandlePostCommand(
  AContext: TRemoteShutdownServerContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; const O: ISuperObject);
var
  Cmd: String;
  R: ISuperObject;
  Suc: Boolean;
  A: TSuperArray;
  M: String;
  X: Integer;
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

  //TODO: Loop through other computers and send command to them first
  A:= FConfig.A['machines'];
  if Assigned(A) then begin
    for X := 0 to A.Length-1 do begin
      M:= A.S[X];
      //TODO: Forward command to "M" machine

      //Actually this should not be done here - this procedure is only
      //used when manually sending shutdown command remotely.
      //Instead, need to catch auto hibernate due to power loss,
      //and only forward to other machines in that situation.

    end;
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

function TRemoteShutdownServer.GetStatusObj: ISuperObject;
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

function TRemoteShutdownServer.ServerURL(const ARes: String = ''): String;
begin
  //Return base URL for global server
  Result:= 'http://' + FConfig.S['global_host'] + ':' + IntToStr(FConfig.I['global_port']) + '/';
  if ARes <> '' then
    Result:= Result + ARes + '/';
end;

procedure TRemoteShutdownServer.Process;
  {$IFDEF V2}
var
  O: ISuperObject;
  S: TMemoryStream;
  {$ENDIF}
begin
  {$IFDEF V2}
  O:= GetStatusObj;
  S:= TMemoryStream.Create;
  try
    O.SaveTo(S);
    S.Position:= 0;
    try
      FCli.Post(ServerURL('ping'), S);
    except
      on E: Exception do begin
        //TODO
      end;
    end;
  finally
    S.Free;
  end;
  {$ENDIF}
end;

{ TRemoteShutdownServerContext }

constructor TRemoteShutdownServerContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;

end;

end.
