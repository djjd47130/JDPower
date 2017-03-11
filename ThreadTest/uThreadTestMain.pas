unit uThreadTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  JD.ThreadTest, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThread: TDataThread;
  protected
    procedure WMPowerBroadcast(var Msg: TMsg); message WM_POWERBROADCAST;
  public

  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  FThread:= TDataThread.Create('Testing');
  FThread.Start;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FThread.Terminate;
  //FThread.WaitFor;
  FreeAndNil(FThread);
end;

procedure TForm2.WMPowerBroadcast(var Msg: TMsg);
var
  S: String;
begin
  S:= 'Testing';
end;

end.
