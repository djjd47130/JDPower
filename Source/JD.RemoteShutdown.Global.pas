unit JD.RemoteShutdown.Global;

(*
  JD Remote Shutdown Global Thread
  - Runs on a single central server
  - HTTP Server listening for incoming commands
*)

interface

{ $DEFINE V2}

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Winapi.WIndows,
  Winapi.Messages,
  ActiveX,
  IdHTTP, IdHTTPServer, IdTCPConnection, IdCustomHTTPServer, IdTCPServer,
  IdCustomTCPServer, IdContext, IdGlobal, IdYarn, IdThread,
  SuperObject,
  ShellAPI,
  SyncObjs,
  JD.RemoteShutdown.Common;

type
  TRemoteShutdownGlobalContext = class;
  TRemoteShutdownGlobal = class;
  TRSMachine = class;



  TRemoteShutdownGlobal = class(TThread)
  private
    FSvr: TIdHTTPServer;
    FCli: TIdHTTP;
    {$IFDEF V2}
    FClients: TObjectList<TRSMachine>;
    FLock: TCriticalSection;
    {$ENDIF}
    procedure Init;
    procedure Uninit;
    procedure Process;
    procedure LoadConfig;
    procedure HandleCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetHomePage(AContext: TRemoteShutdownGlobalContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetStatus(AContext: TRemoteShutdownGlobalContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleGetClients(AContext: TRemoteShutdownGlobalContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandlePostCommand(AContext: TRemoteShutdownGlobalContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      const O: ISuperObject);
    procedure HandlePostPing(AContext: TRemoteShutdownGlobalContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      const O: ISuperObject);
    function GetHomePage: String;
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TRemoteShutdownGlobalContext = class(TIdServerContext)
  private
    //Represents any client connecting into global server
  public
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn;
      AList: TIdContextThreadList = nil); override;
  end;

  TRSMachine = class(TObject)
  private
    FDisplayName: String;
    FPort: Integer;
    FHost: String;
    FPing: TDateTime;
  public
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property DisplayName: String read FDisplayName write FDisplayName;
    property Ping: TDateTime read FPing write FPing;
  end;

implementation

uses
  DateUtils, StrUtils;

{ TRemoteShutdownGlobal }

constructor TRemoteShutdownGlobal.Create;
begin
  inherited Create(True);
  {$IFDEF V2}
  FClients:= TObjectList<TRSMachine>.Create(True);
  FLock:= TCriticalSection.Create;
  {$ENDIF}
end;

destructor TRemoteShutdownGlobal.Destroy;
begin
  {$IFDEF V2}
  FLock.Enter;
  try
    FClients.Clear;
  finally
    FLock.Leave;
  end;
  FreeAndNil(FLock);
  FreeAndNil(FClients);
  {$ENDIF}
  inherited;
end;

procedure TRemoteShutdownGlobal.Init;
begin
  CoInitialize(nil);
  FSvr:= TIdHTTPServer.Create(nil);
  FSvr.ContextClass:= TRemoteShutdownGlobalContext;
  FSvr.OnCommandGet:= HandleCommand;
  FSvr.OnCommandOther:= HandleCommand;
  LoadConfig;
  FSvr.Active:= True;
  FCli:= TIdHTTP.Create(nil);
  FCli.HandleRedirects:= True;



end;

procedure TRemoteShutdownGlobal.Uninit;
begin
  FSvr.Active:= False;
  FreeAndNil(FCli);
  FreeAndNil(FSvr);
  CoUninitialize;
end;

procedure TRemoteShutdownGlobal.LoadConfig;
var
  FN: String;
  L: TStringList;
  O: ISuperObject;
  procedure SaveDef;
  begin
    O:= SO;
    O.I['listen_port']:= GLOBAL_PORT;
    L.Text:= O.AsJSon(True);
    L.SaveToFile(FN);
  end;
begin
  FN:= ExtractFilePath(ParamStr(0));
  FN:= IncludeTrailingPathDelimiter(FN);
  FN:= FN + 'RemoteShutdownGlobal.json';
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

    //FSvr.DefaultPort:= O.I['listen_port'];

    FSvr.Bindings.Clear;
    FSvr.Bindings.Add.SetBinding('', O.I['listen_port'], TIdIPVersion.Id_IPv4);

  finally
    L.Free;
  end;
end;

procedure TRemoteShutdownGlobal.Execute;
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
      for X := 1 to 10 do begin
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

function TRemoteShutdownGlobal.GetHomePage: String;
  procedure A(const S: String);
  begin
    Result:= Result + S + sLineBreak;
  end;
var
  X: Integer;
  M: TRSMachine;
begin
  {$IFDEF V2}
  Result:= '';
  A('<html>');
  A('<head>');
  A('  <title>JD Remote Shutdown</title>');
  A('</head>');
  A('<body style="margin: 0px; padding: 0px;">');
  A('  <div style="width: 100%; height: 60px; background-color: Black; ');
  A('    color: White; margin: 0px; padding: 3px;">');
  A('    <h1>JD Remote Shutdown</h1>');
  A('  </div>');
  A('  <div style="padding: 5px;">');
  A('    <h3>Select which machines you wish to control</h3>');
  A('    <table style="font-size: 20px;">');
  A('      <tbody>');
  A('        <tr>');
  A('          <td><br /></td>');
  A('          <td><b>Machine</b></td>');
  A('          <td><b>IP Address</b></td>');
  A('        </tr>');
  A('      </tbody>');
  A('      <tbody>');
  FLock.Enter;
  try
    for X := 0 to FClients.Count-1 do begin
      M:= FClients[X];
      A('        <tr>');
      A('          <td><input id="chkItem'+IntToStr(X)+'" type="checkbox" style="width: 25px; height: 25px;" value="' + M.FHost + '"></input></td>');
      A('          <td><label for="chkItem'+IntToStr(X)+'">' + M.FDisplayName + '</label></td>');
      A('          <td><label for="chkItem'+IntToStr(X)+'">' + M.FHost + '</label></td>');
      A('        </tr>');
    end;
  finally
    FLock.Leave;
  end;
  A('      </tbody>');
  A('    </table>');
  A('    <br />');
  A('    <br />');
  A('    <select>');
  A('      <option value="Shutdown">Shut Down</option>');
  A('      <option value="Restart">Restart</option>');
  A('      <option value="Hibernate" selected="True">Hibernate</option>');
  A('    </select>');
  A('    <br />');
  A('    <br />');
  A('    <input type="button" value="Send Command" onclick="sendCommand()" />');
  A('  </div>');
  A('  <script type="text/javascript">');
  A('    function sendCommand() {');
  A('      '); //TODO: Read command from drop-down, read selected
  A('      '); //  machines, and send command to global server to
  A('      '); //  forwrad to machines.
  A('      ');
  A('    }');
  A('    ');
  A('    ');
  A('    ');
  A('  </script>');
  A('</body>');
  A('</html>');
  {$ENDIF}
end;

procedure TRemoteShutdownGlobal.HandleCommand(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  R: String;
  C: TRemoteShutdownGlobalContext;
  O: ISuperObject;
  function IsReq(const S: String): Boolean;
  begin
    Result:= SameText(S, R);
  end;
begin
  //Received any type of command from the global server
  C:= TRemoteShutdownGlobalContext(AContext);
  R:= ARequestInfo.Document + '/';
  Delete(R, 1, 1);
  R:= Copy(R, 1, Pos('/', R)-1);
  case ARequestInfo.CommandType of
    hcGET: begin
      if IsReq('Status') then begin
        HandleGetStatus(C, ARequestInfo, AResponseInfo);
      end else
      if IsReq('Clients') then begin
        HandleGetClients(C, ARequestInfo, AResponseInfo);
      end else
      if IsReq('') then begin
        HandleGetHomePage(C, ARequestInfo, AResponseInfo);
      end else begin
        //TODO: Handle invalid request
      end;
    end;
    hcPOST: begin
      O:= TSuperObject.ParseStream(ARequestInfo.PostStream, False);
      if IsReq('Command') then begin
        HandlePostCommand(C, ARequestInfo, AResponseInfo, O);
      end else
      if IsReq('Ping') then begin
        HandlePostPing(C, ARequestInfo, AResponseInfo, O);
      end else begin
        //TODO: Handle invalid request
      end;
    end;
    else begin
      //Unsupported command
    end;
  end;
end;

procedure TRemoteShutdownGlobal.HandleGetStatus(AContext: TRemoteShutdownGlobalContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  R: ISuperObject;
begin
  R:= SO; //TODO: Return global server status info
  AResponseInfo.ContentText:= R.AsJSon(True);
  AResponseInfo.ContentType:= 'application/json';
end;

procedure TRemoteShutdownGlobal.HandleGetClients(
  AContext: TRemoteShutdownGlobalContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  R, O: ISuperObject;
  X: Integer;
  M: TRSMachine;
begin
  R:= SA([]);
  try
    {$IFDEF V2}
    FLock.Enter;
    try
      for X := 0 to FClients.Count-1 do begin
        M:= FClients[X];
        O:= SO;
        O.S['host']:= M.Host;
        O.I['port']:= M.Port;
        O.S['display_name']:= M.DisplayName;
        O.D['ping']:= M.Ping;
        R.AsArray.Add(O);
      end;
    finally
      FLock.Leave;
    end;
    {$ENDIF}
  finally
    AResponseInfo.ContentText:= R.AsJSon(True);
    AResponseInfo.ContentType:= 'application/json';
  end;
end;

procedure TRemoteShutdownGlobal.HandleGetHomePage(
  AContext: TRemoteShutdownGlobalContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  R: String;
begin
  try
    R:= Self.GetHomePage;
  finally
    AResponseInfo.ContentText:= R;
    AResponseInfo.ContentType:= 'text/html';
  end;
end;

procedure TRemoteShutdownGlobal.HandlePostCommand(
  AContext: TRemoteShutdownGlobalContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; const O: ISuperObject);
var
  Cmd: String;
  CmdLine: String;
  R: ISuperObject;
  A: TSuperArray;
  M: ISuperObject;
  X: Integer;
  U: String;
  S: TMemoryStream;
  function IsCmd(const S: String): Boolean;
  begin
    Result:= SameText(Cmd, S);
  end;
begin
  //TODO: READ LIST OF COMPUTERS AND ITERATE AND PERFORM COMMAND FOR EACH

  if not Assigned(O) then begin
    //TODO: Log error - invalid JSON object
    Exit;
  end;

  R:= SO;
  try
    Cmd:= O.S['cmd'];
    if IsCmd('Shutdown') then begin
      CmdLine:= 'shutdown /s';
      if O.B['hybrid'] then
        CmdLine:= CmdLine + ' /hybrid';
      if O.I['time'] > 0 then
        CmdLine:= CmdLine + ' /t '+IntToStr(O.I['time']);
    end else
    if IsCmd('Restart') then begin
      CmdLine:= 'shutdown /r';
      if O.I['time'] > 0 then
        CmdLine:= CmdLine + ' /t '+IntToStr(O.I['time']);
    end else
    if IsCmd('Hibernate') then begin
      CmdLine:= 'shutdown /h';
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
      CmdLine:= '';
    end;

    if CmdLine <> '' then begin

      if O.B['force'] then
        CmdLine:= CmdLine + ' /f';

      R.S['cmd']:= CmdLine;

      try

          raise Exception.Create(O.AsJSon(True));

        A:= O.A['machines'];
        for X := 0 to A.Length-1 do begin
          //TODO: Broadcast command to all listed client machines
          M:= A.O[X];

          U:= 'http://' + M.AsString + ':65469/Command';

          S:= TMemoryStream.Create;
          try
            O.SaveTo(S, True);
            S.Position:= 0;
            Self.FCli.Post(U, S);
          finally
            S.Free;
          end;
        end;

        R.B['success']:= True;
      except
        on E: Exception do begin
          R.B['success']:= False;
          R.S['error']:= E.Message;
        end;
      end;

    end else begin
      R.S['error']:= 'Unable to determine command "'+Cmd+'"';
    end;
  finally
    AResponseInfo.ContentText:= R.AsJSon(True);
    AResponseInfo.ContentType:= 'application/json';
  end;
end;

procedure TRemoteShutdownGlobal.HandlePostPing(
  AContext: TRemoteShutdownGlobalContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo; const O: ISuperObject);
var
  R: ISuperObject;
  X: Integer;
  M: TRSMachine;
  E: Boolean;
begin
  R:= SO;
  try
    {$IFDEF V2}
    FLock.Enter;
    try
      E:= False;
      for X := FClients.Count-1 downto 0 do begin
        M:= FClients[X];
        if SameText(M.Host, AContext.Binding.PeerIP) then begin
          E:= True;
          M.Ping:= Now;
          Break;
        end;
      end;
      if not E then begin
        M:= TRSMachine.Create;
        try
          M.FHost:= AContext.Binding.PeerIP;
          M.FPort:= AContext.Binding.PeerPort;
          M.FDisplayName:= O.S['display_name'];
          M.FPing:= Now;
        finally
          FClients.Add(M);
        end;
      end;
    finally
      FLock.Leave;
    end;
    {$ENDIF}

    R.B['success']:= True;
  finally
    AResponseInfo.ContentText:= R.AsJSon(True);
    AResponseInfo.ContentType:= 'application/json';
  end;
end;

procedure TRemoteShutdownGlobal.Process;
const
  SEC_DELAY = 20; //20 seconds timeout
var
  M: TRSMachine;
  X: Integer;
begin
  {$IFDEF V2}
  FLock.Enter;
  try
    for X := FClients.Count-1 downto 0 do begin
      M:= FCLients[X];
      if DateUtils.IncSecond(M.Ping, SEC_DELAY) < Now then begin
        FClients.Delete(X);
      end;
    end;
  finally
    FLock.Leave;
  end;
  {$ENDIF}
end;

{ TRemoteShutdownGlobalContext }

constructor TRemoteShutdownGlobalContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;

end;

end.
