program JDPowerGlobal;

uses
  uRemoteShutdownGlobalMain in 'uRemoteShutdownGlobalMain.pas' {JDRemoteShutdownGlo: TService},
  JD.Power.Global in '..\Source\JD.Power.Global.pas',
  JD.Power.Common in '..\Source\JD.Power.Common.pas',
  RSGlobalAppInit in 'RSGlobalAppInit.pas',
  uRemoteShutdownGlobalTest in 'uRemoteShutdownGlobalTest.pas' {JDRemoteShutdownGloTest};

{$R *.RES}

begin
  RunApp;
end.
