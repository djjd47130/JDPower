unit uPowerGlobalMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.SvcMgr,
  Registry,
  JD.Power.Global;

type
  TJDPowerGlobal = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FSvr: TRemoteShutdownGlobal;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  JDPowerGlobal: TJDPowerGlobal;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  JDPowerGlobal.Controller(CtrlCode);
end;

function TJDPowerGlobal.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TJDPowerGlobal.ServiceAfterInstall(Sender: TService);
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if R.OpenKey('SYSTEM\CurrentControlSet\Services\'+Name, True) then begin
      try
        R.WriteString('Description',
          'Central global management point for JD Power system.');
      finally
        R.CloseKey;
      end;
    end;
  finally
    R.Free;
  end;
end;

procedure TJDPowerGlobal.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  FSvr:= TRemoteShutdownGlobal.Create;
  FSvr.Start;
end;

procedure TJDPowerGlobal.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  FSvr.Terminate;
  FSvr.WaitFor;
  FreeAndNil(FSvr);
end;

end.
