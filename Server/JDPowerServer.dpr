program JDPowerServer;

uses
  uRemoteShutdownServerMain in 'uRemoteShutdownServerMain.pas' {JDRemoteShutdownSvr: TService},
  JD.Power.Server in '..\Source\JD.Power.Server.pas',
  RSServerAppInit in 'RSServerAppInit.pas',
  uRemoteShutdownServerTest in 'uRemoteShutdownServerTest.pas' {JDRemoteShutdownSvrTest},
  JD.Power.Common in '..\Source\JD.Power.Common.pas';

{$R *.RES}

begin
  RunApp;
end.
