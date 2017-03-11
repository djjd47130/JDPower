unit uThreadTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  JD.ThreadTest;

type
  TForm2 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FThread: TDataThread;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FThread.Terminate;
  //FThread.WaitFor;
  FreeAndNil(FThread);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FThread:= TDataThread.Create('Testing');
  FThread.Start;
end;

end.
