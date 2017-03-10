program JDPowerServer;

uses
  uPowerServerMain in 'uPowerServerMain.pas' {JDPowerServer: TService},
  JD.Power.Server in '..\Source\JD.Power.Server.pas',
  ServerAppInit in 'ServerAppInit.pas',
  uPowerServerTest in 'uPowerServerTest.pas' {JDRemoteShutdownSvrTest},
  JD.Power.Common in '..\Source\JD.Power.Common.pas';

{$R *.RES}

begin
  RunApp;
end.
