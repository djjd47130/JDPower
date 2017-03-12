unit JD.ThreadTest;

interface

uses
  System.Classes, System.SysUtils, Winapi.Messages, Winapi.Windows;

type

  TMessageEvent = procedure(Sender: TObject; Message: TMessage) of object;

  TDataThread = class(TThread)
  private
    FTitle: String;
    FWnd: HWND;
    FWndClass: WNDCLASS;
    FOnMessage: TMessageEvent;
    FMsg: TMessage;
    procedure HandleMessage(var Message: TMessage);
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
    procedure DoOnMessage;
  public
    constructor Create(const Title: String); reintroduce;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

function DataThreadWndProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Thread: TDataThread;
  Message: TMessage;
begin
  if Msg = WM_NCCREATE then
  begin
    Thread := TDataThread(PCREATESTRUCT(lParam)^.lpCreateParams);
    SetWindowLongPtr(Wnd, GWLP_USERDATA, LONG_PTR(Thread));
  end else
    Thread := TDataThread(GetWindowLongPtr(Wnd, GWLP_USERDATA));

  if Thread <> nil then
  begin
    Message.Msg := Msg;
    Message.WParam := wParam;
    Message.LParam := lParam;
    Message.Result := 0;
    Thread.HandleMessage(Message);
    Result := Message.Result;
  end else
    Result := DefWindowProc(Wnd, Msg, wParam, lParam);
end;

constructor TDataThread.Create(const Title: String);
begin
  inherited Create(True);
  FTitle := Title;
  with FWndClass do
  begin
    Style := 0;
    lpfnWndProc := @DataThreadWndProc;
    cbClsExtra := 0;
    cbWndExtra := 0;
    hInstance := HInstance;
    hIcon := 0;
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := COLOR_WINDOW;
    lpszMenuName := nil;
    lpszClassName := 'TDataThread';
  end;
end;

procedure TDataThread.Execute;
var
  Msg: TMsg;
begin
  if Winapi.Windows.RegisterClass(FWndClass) = 0 then Exit;
  FWnd := CreateWindow(FWndClass.lpszClassName, PChar(FTitle), WS_DLGFRAME, 0, 0, 698, 517, 0, 0, HInstance, Self);
  if FWnd = 0 then Exit;
  while GetMessage(Msg, 0, 0, 0) do
  begin
    if Terminated then Exit;
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end;

procedure TDataThread.DoOnMessage;
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, FMsg);
end;

procedure TDataThread.DoTerminate;
begin
  if FWnd <> 0 then DestroyWindow(FWnd);
  Winapi.Windows.UnregisterClass(FWndClass.lpszClassName, HInstance);
  inherited;
end;

procedure TDataThread.HandleMessage(var Message: TMessage);
begin
  FMsg:= Message;
  Synchronize(DoOnMessage);
  case Message.Msg of
    WM_POWERBROADCAST:
    begin

    end;
  else
    Message.Result := DefWindowProc(FWnd, Message.Msg, Message.WParam, Message.LParam);
  end;
end;

end.
