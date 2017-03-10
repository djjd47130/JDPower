program RemoteShutdownClient;

uses
  Vcl.Forms,
  uRemoteShutdownClientMain in 'uRemoteShutdownClientMain.pas' {frmCliMain},
  JD.RemoteShutdown.Client in '..\Source\JD.RemoteShutdown.Client.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'JD Remote Shutdown Client Manager';
  TStyleManager.TrySetStyle('Light');
  Application.CreateForm(TfrmCliMain, frmCliMain);
  Application.Run;
end.
