unit uRemoteShutdownClientMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  SuperObject, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, Vcl.StdCtrls, Vcl.Buttons, Vcl.Samples.Spin;

type
  TfrmCliMain = class(TForm)
    lstClients: TListView;
    Panel1: TPanel;
    Web: TIdHTTP;
    cmdRefresh: TBitBtn;
    cmdSend: TBitBtn;
    cboCommand: TComboBox;
    chkForce: TCheckBox;
    txtComment: TMemo;
    Splitter1: TSplitter;
    chkHybrid: TCheckBox;
    txtTimeout: TSpinEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmdRefreshClick(Sender: TObject);
    procedure cmdSendClick(Sender: TObject);
  private
    function ServerUrl: String;
    procedure Refresh;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCliMain: TfrmCliMain;

implementation

{$R *.dfm}

function TfrmCliMain.ServerUrl: String;
begin
  Result:= 'http://INNO102:65468/';
end;

procedure TfrmCliMain.cmdRefreshClick(Sender: TObject);
begin
  Refresh;
end;

procedure TfrmCliMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  X: Integer;
begin
  for X := 0 to lstClients.Items.Count-1 do begin
    ISuperObject(lstClients.Items[X].Data)._Release;
  end;
  lstClients.Items.Clear;
end;

procedure TfrmCliMain.FormCreate(Sender: TObject);
begin

  lstClients.Align:= alClient;
  Refresh;
end;

procedure TfrmCliMain.Refresh;
var
  S: String;
  R, O: ISuperObject;
  A: TSuperArray;
  I: TListItem;
  X: Integer;
begin
  lstClients.Items.BeginUpdate;
  try
    for X := 0 to lstClients.Items.Count-1 do begin
      ISuperObject(lstClients.Items[X].Data)._Release;

    end;
    lstClients.Items.Clear;

    S:= Web.Get(ServerUrl+'clients');
    R:= SO(S);
    if Assigned(R) then begin

      A:= R.AsArray;
      for X := 0 to A.Length-1 do begin
        O:= A.O[X];
        O._AddRef;
        I:= lstClients.Items.Add;
        I.Caption:= O.S['display_name'];
        I.SubItems.Add(O.S['host']);
        I.Data:= Pointer(O);
      end;
    end;

  finally
    lstClients.Items.EndUpdate;
  end;
end;

procedure TfrmCliMain.cmdSendClick(Sender: TObject);
var
  Req: ISuperObject;
  A, O, M: ISuperObject;
  S: TMemoryStream;
  X: Integer;
  I: TListItem;
begin
  //TODO: Send command to global server to forward command to specific computers.

  //Command is structured PRECISELY the same as the JSON command global server
  //sends to each machine - but also includes an additional element:
  //Array of Strings which identifies which machines to forward the command to.

  Req:= SO;
  Req.S['cmd']:= cboCommand.Text;
  Req.B['hybrid']:= chkHybrid.Checked;
  Req.B['force']:= chkForce.Checked;
  Req.I['time']:= txtTimeout.Value;
  Req.S['comment']:= txtComment.Lines.Text;
  Req.O['reason']:= SO;
  Req.O['reason'].S['type']:= 'U'; //TODO
  Req.O['reason'].I['major']:= 0; //TODO
  Req.O['reason'].I['minor']:= 0; //TODO
  Req.O['reason'].S['msg']:= 'Power Loss'; //TODO

  A:= SA([]);
  try
    for X := 0 to lstClients.Items.Count-1 do begin
      I:= lstClients.Items[X];
      if I.Checked then begin
        M:= ISuperObject(I.Data);
        if Assigned(M) then begin
          A.AsArray.Add(SO(M.S['host']));
        end;
      end;
    end;
  finally
    Req.O['machines']:= A;
  end;

  S:= TMemoryStream.Create;
  try
    Req.SaveTo(S, True);
    S.Position:= 0;
    Web.Post(ServerUrl+'Command', S);
  finally
    S.Free;
  end;

  (*
    O.S['host']:= M.Host;
    O.I['port']:= M.Port;
    O.S['display_name']:= M.DisplayName;
    O.D['ping']:= M.Ping;
  *)

end;

end.
