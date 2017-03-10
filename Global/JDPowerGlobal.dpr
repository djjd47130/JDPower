program JDPowerGlobal;

uses
  uPowerGlobalMain in 'uPowerGlobalMain.pas' {JDPowerGlobal: TService},
  JD.Power.Global in '..\Source\JD.Power.Global.pas',
  JD.Power.Common in '..\Source\JD.Power.Common.pas',
  GlobalAppInit in 'GlobalAppInit.pas',
  uPowerGlobalTest in 'uPowerGlobalTest.pas' {JDRemoteShutdownGloTest};

{$R *.RES}

begin
  RunApp;
end.
