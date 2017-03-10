program JDPowerTest;

uses
  Vcl.Forms,
  uTestMain in 'uTestMain.pas' {Form1},
  JD.Power.Monitor in '..\Source\JD.Power.Monitor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
