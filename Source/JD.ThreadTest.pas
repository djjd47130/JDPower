unit JD.ThreadTest;

interface

uses
  System.Classes, Winapi.Messages, Winapi.Windows;

type
  TDataThread = class(TThread)
  private
    FTitle: String;
    FWnd: HWND;
    FWndClass: WNDCLASS;
    class function WndProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; static;
    procedure HandleMessage(var Message: TMessage);
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
   constructor Create(const Title: String); reintroduce;
  end;

implementation

constructor TDataThread.Create(const Title: String);
begin
  inherited Create(True);
  FTitle := Title;
  with FWndClass do begin
    Style := 0;
    lpfnWndProc := @WndProc;
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
  FWnd := CreateWindow(FWndClass.lpszClassName, PChar(FTitle), WS_DLGFRAME,
    0, 0, 698, 517, 0, 0, HInstance, nil);
  if FWnd = 0 then Exit;
  SetWindowLongPtr(FWnd, GWL_USERDATA, LONG_PTR(Self));
  while GetMessage(Msg, 0, 0, 0) do begin
    if Terminated then Exit;
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
end;

procedure TDataThread.DoTerminate;
begin
  if FWnd <> 0 then DestroyWindow(FWnd);
  Winapi.Windows.UnregisterClass(FWndClass.lpszClassName, HInstance);
  inherited;
end;

class function TDataThread.WndProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT;
var
  Message: TMessage;
begin
  Message.Msg := Msg;
  Message.WParam := wParam;
  Message.LParam := lParam;
  Message.Result := 0;
  TDataThread(GetWindowLongPtr(Wnd, GWL_USERDATA)).HandleMessage(Message);
  Result := Message.Result;
end;

procedure TDataThread.HandleMessage(var Message: TMessage);
var
  S: String;
begin
  case Message.Msg of
    WM_POWERBROADCAST: begin
      S:= 'Test';
    end;
    else begin
      Message.Result := DefWindowProc(FWnd, Message.Msg, Message.WParam, Message.LParam);
    end;
  end;
end;

end.
