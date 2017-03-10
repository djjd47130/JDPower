program RemoteShutdownTest;

uses
  Vcl.Forms,
  uTestMain in 'uTestMain.pas' {Form1},
  JD.RemoteShutdown.PowerMonitor in '..\Source\JD.RemoteShutdown.PowerMonitor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
