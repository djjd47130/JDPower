unit JD.ThreadTest;

interface

uses
  System.Classes,
  Winapi.Messages,
  Winapi.Windows;

type
  TDataThread = class(TThread)
  private
    FTitle: String;
    FWnd: HWND;
    FWndClass: WNDCLASS;
    procedure HandlePower(AMsg: TMsg);
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(const Title:String); reintroduce;
  end;

implementation

constructor TDataThread.Create(const Title: String);
begin
  inherited Create(True);
  FTitle := Title;
  with FWndClass do begin
    Style := 0;
    lpfnWndProc := @DefWindowProc;
    cbClsExtra := 0;
    cbWndExtra := 0;
    hInstance := HInstance;
    hIcon := 0;
    hCursor := LoadCursor(0, IDC_ARROW);
    hbrBackground := COLOR_WINDOW;
    lpszMenuName := nil;
    lpszClassName := PChar(Self.ClassName);
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

  while not Terminated do begin
    while GetMessage(Msg, FWnd, 0, 0) = True do begin
      if Terminated then Break;      
      case Msg.message of
        WM_POWERBROADCAST: begin
          HandlePower(Msg);
        end;
        else begin
          TranslateMessage(msg);
          DispatchMessage(msg)
        end;
      end;
    end;
    Sleep(1);
  end;

end;

procedure TDataThread.HandlePower(AMsg: TMsg);
begin

end;

procedure TDataThread.DoTerminate;
begin
  if FWnd <> 0 then DestroyWindow(FWnd);
  Winapi.Windows.UnregisterClass(PChar(Self.ClassName), FWndClass.hInstance);
  inherited;
end;

end.
