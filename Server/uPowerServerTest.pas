unit uPowerServerTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  JD.Power.Server;

type
  TJDRemoteShutdownSvrTest = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSvr: TRemoteShutdownServer;
  public
    { Public declarations }
  end;

var
  JDRemoteShutdownSvrTest: TJDRemoteShutdownSvrTest;

implementation

{$R *.dfm}

procedure TJDRemoteShutdownSvrTest.FormCreate(Sender: TObject);
begin
  FSvr:= TRemoteShutdownServer.Create;
  FSvr.Start;
end;

procedure TJDRemoteShutdownSvrTest.FormDestroy(Sender: TObject);
begin
  FSvr.Terminate;
  FSvr.WaitFor;
  FreeAndNil(FSvr);
end;

end.
