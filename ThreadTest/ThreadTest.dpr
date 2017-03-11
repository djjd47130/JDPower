program ThreadTest;

uses
  Vcl.Forms,
  uThreadTestMain in 'uThreadTestMain.pas' {Form2},
  JD.ThreadTest in '..\Source\JD.ThreadTest.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
