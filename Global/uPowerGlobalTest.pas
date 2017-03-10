unit uPowerGlobalTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  JD.Power.Global, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdCustomHTTPServer, IdHTTPServer;

type
  TJDRemoteShutdownGloTest = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSvr: TRemoteShutdownGlobal;
  public
    { Public declarations }
  end;

var
  JDRemoteShutdownGloTest: TJDRemoteShutdownGloTest;

implementation

{$R *.dfm}

procedure TJDRemoteShutdownGloTest.FormCreate(Sender: TObject);
begin
  FSvr:= TRemoteShutdownGlobal.Create;
  FSvr.Start;
end;

procedure TJDRemoteShutdownGloTest.FormDestroy(Sender: TObject);
begin
  FSvr.Terminate;
  FSvr.WaitFor;
  FreeAndNil(FSvr);
end;

end.
