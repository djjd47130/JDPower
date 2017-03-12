unit uThreadTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  JD.ThreadTest, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThread: TDataThread;
    procedure ThreadEvent(Sender: TObject; Message: TMessage);
  protected

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FThread:= TDataThread.Create('Testing');
  FThread.OnMessage:= ThreadEvent;
  FThread.Start;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FThread.Terminate;
  //FThread.WaitFor;
  FreeAndNil(FThread);
end;

procedure TForm2.ThreadEvent(Sender: TObject; Message: TMessage);
begin
  Memo1.Lines.Append('Message ' + IntToStr(Message.Msg));
end;

end.
