unit uRemoteShutdownGlobalMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.SvcMgr,
  Registry,
  JD.Power.Global;

type
  TJDRemoteShutdownGlo = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FSvr: TRemoteShutdownGlobal;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  JDRemoteShutdownGlo: TJDRemoteShutdownGlo;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  JDRemoteShutdownGlo.Controller(CtrlCode);
end;

function TJDRemoteShutdownGlo.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TJDRemoteShutdownGlo.ServiceAfterInstall(Sender: TService);
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if R.OpenKey('SYSTEM\CurrentControlSet\Services\'+Name, True) then begin
      try
        R.WriteString('Description',
          'Central global management point for JD Remote Shutdown system.');
      finally
        R.CloseKey;
      end;
    end;
  finally
    R.Free;
  end;
end;

procedure TJDRemoteShutdownGlo.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  FSvr:= TRemoteShutdownGlobal.Create;
  FSvr.Start;
end;

procedure TJDRemoteShutdownGlo.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  FSvr.Terminate;
  FSvr.WaitFor;
  FreeAndNil(FSvr);
end;

end.
