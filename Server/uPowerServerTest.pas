unit uPowerServerTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  JD.Power.Server;

type
  TJDPowerServerTest = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSvr: TJDPowerServerThread;
  public
    { Public declarations }
  end;

var
  JDPowerServerTest: TJDPowerServerTest;

implementation

{$R *.dfm}

procedure TJDPowerServerTest.FormCreate(Sender: TObject);
begin
  FSvr:= TJDPowerServerThread.Create;
  FSvr.Start;
end;

procedure TJDPowerServerTest.FormDestroy(Sender: TObject);
begin
  FSvr.Terminate;
  FSvr.WaitFor;
  FreeAndNil(FSvr);
end;

end.
