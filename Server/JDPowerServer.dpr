program JDPowerServer;

uses
  uPowerServerMain in 'uPowerServerMain.pas' {JDPowerServer: TService},
  uPowerServerTest in 'uPowerServerTest.pas' {JDPowerServerTest},
  ServerAppInit in 'ServerAppInit.pas',
  JD.Power.Server in '..\Source\JD.Power.Server.pas',
  JD.Power.Common in '..\Source\JD.Power.Common.pas',
  JD.Power.Monitor in '..\Source\JD.Power.Monitor.pas',
  JD.SvcMgr in '..\Source\JD.SvcMgr.pas';

{$R *.RES}

begin
  RunApp;
end.
