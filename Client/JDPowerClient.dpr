program JDPowerClient;

uses
  Vcl.Forms,
  uPowerClientMain in 'uPowerClientMain.pas' {frmCliMain},
  JD.Power.Client in '..\Source\JD.Power.Client.pas',
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
