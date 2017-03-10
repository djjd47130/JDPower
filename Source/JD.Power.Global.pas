unit JD.Power.Global;

(*
  JD Remote Shutdown Global Thread
  - Runs on a single central server
  - HTTP Server listening for incoming commands
*)

interface

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
  JD.Power.Common;

type
  TRemoteShutdownGlobalContext = class;
  TRemoteShutdownGlobal = class;
  TRSMachine = class;

  TRemoteShutdownGlobal = class(TThread)
  private
    FSvr: TIdHTTPServer;
    FCli: TIdHTTP;
    procedure Init;
    procedure Uninit;
    procedure Process;
    procedure LoadConfig;
    procedure HandleCommand(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
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

end;

destructor TRemoteShutdownGlobal.Destroy;
begin

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
  FN:= FN + 'JDPowerGlobal.json';
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

      end else
      if IsReq('Clients') then begin

      end else
      if IsReq('') then begin

      end else begin
        //TODO: Handle invalid request
      end;
    end;
    hcPOST: begin
      O:= TSuperObject.ParseStream(ARequestInfo.PostStream, False);
      if IsReq('Command') then begin

      end else
      if IsReq('Ping') then begin

      end else begin
        //TODO: Handle invalid request
      end;
    end;
    else begin
      //Unsupported command
    end;
  end;
end;

procedure TRemoteShutdownGlobal.Process;
begin

end;

{ TRemoteShutdownGlobalContext }

constructor TRemoteShutdownGlobalContext.Create(AConnection: TIdTCPConnection;
  AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited;

end;

end.
