program RemoteShutdownGlobal;

uses
  uRemoteShutdownGlobalMain in 'uRemoteShutdownGlobalMain.pas' {JDRemoteShutdownGlo: TService},
  JD.RemoteShutdown.Global in '..\Source\JD.RemoteShutdown.Global.pas',
  JD.RemoteShutdown.Common in '..\Source\JD.RemoteShutdown.Common.pas',
  RSGlobalAppInit in 'RSGlobalAppInit.pas',
  uRemoteShutdownGlobalTest in 'uRemoteShutdownGlobalTest.pas' {JDRemoteShutdownGloTest};

{$R *.RES}

begin
  RunApp;
end.
