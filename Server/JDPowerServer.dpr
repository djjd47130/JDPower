program JDPowerServer;

uses
  uPowerServerMain in 'uPowerServerMain.pas' {JDPowerServer: TService},
  ServerAppInit in 'ServerAppInit.pas',
  JD.Power.Server in '..\Source\JD.Power.Server.pas',
  JD.Power.Common in '..\Source\JD.Power.Common.pas',
  JD.Power.Monitor in '..\Source\JD.Power.Monitor.pas',
  uPowerServerTest in 'uPowerServerTest.pas' {JDPowerServerTest};

{$R *.RES}

begin
  RunApp;
end.
