program RemoteShutdownServer;

uses
  uRemoteShutdownServerMain in 'uRemoteShutdownServerMain.pas' {JDRemoteShutdownSvr: TService},
  JD.RemoteShutdown.Server in '..\Source\JD.RemoteShutdown.Server.pas',
  RSServerAppInit in 'RSServerAppInit.pas',
  uRemoteShutdownServerTest in 'uRemoteShutdownServerTest.pas' {JDRemoteShutdownSvrTest},
  JD.RemoteShutdown.Common in '..\Source\JD.RemoteShutdown.Common.pas';

{$R *.RES}

begin
  RunApp;
end.
