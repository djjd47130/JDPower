unit uPowerServerMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.SvcMgr,
  Registry,
  JD.Power.Server;

type
  TJDPowerSvr = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
  private
    FSvr: TJDPowerServerThread;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  JDPowerSvr: TJDPowerSvr;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  JDPowerSvr.Controller(CtrlCode);
end;

function TJDPowerSvr.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TJDPowerSvr.ServiceAfterInstall(Sender: TService);
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if R.OpenKey('SYSTEM\CurrentControlSet\Services\'+Name, True) then begin
      try
        R.WriteString('Description',
          'Waits for an incoming command to shut down the system.');
      finally
        R.CloseKey;
      end;
    end;
  finally
    R.Free;
  end;
end;

procedure TJDPowerSvr.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  FSvr:= TJDPowerServerThread.Create;
  FSvr.Start;
end;

procedure TJDPowerSvr.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  FSvr.Terminate;
  FSvr.WaitFor;
  FreeAndNil(FSvr);
end;

end.
