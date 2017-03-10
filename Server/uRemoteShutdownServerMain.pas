unit uRemoteShutdownServerMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.SvcMgr,
  Registry,
  JD.Power.Server;

type
  TJDRemoteShutdownSvr = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
  private
    FSvr: TRemoteShutdownServer;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  JDRemoteShutdownSvr: TJDRemoteShutdownSvr;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  JDRemoteShutdownSvr.Controller(CtrlCode);
end;

function TJDRemoteShutdownSvr.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TJDRemoteShutdownSvr.ServiceAfterInstall(Sender: TService);
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if R.OpenKey('SYSTEM\CurrentControlSet\Services\'+Name, True) then begin
      try
        R.WriteString('Description',
          'Waits for a remote command to shut down the system.');
      finally
        R.CloseKey;
      end;
    end;
  finally
    R.Free;
  end;
end;

procedure TJDRemoteShutdownSvr.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  FSvr:= TRemoteShutdownServer.Create;
  FSvr.Start;
end;

procedure TJDRemoteShutdownSvr.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  FSvr.Terminate;
  FSvr.WaitFor;
  FreeAndNil(FSvr);
end;

end.
