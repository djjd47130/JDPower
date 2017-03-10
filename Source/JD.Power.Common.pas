unit JD.Power.Common;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  ShellApi,
  SuperObject,
  IdHTTP;

const
  CLIENT_PORT = 65469;

type
  TShutdownCmd = (scShutdown, scRestart, scHibernate, scSleep, scLogoff, scLock);

  TShutdownReason = (srExpected, srUnexpected, srPlanned, srCustomer);

function DoCmd(const Cmd: String): Boolean;
function DoShutdownCmd(const ACmd: TShutdownCmd; const AHybrid: Boolean = False;
  const AForce: Boolean = False; const ATime: Integer = 30;
  const AComment: String = ''; const AReason: TShutdownReason = srUnexpected;
  const AReasonMajor: Integer = 0; const AReasonMinor: Integer = 0;
  const AReasonMsg: String = ''): Boolean;
function SendShutdownCmd(const AMachine: String; const ACmd: TShutdownCmd;
  const AHybrid: Boolean = False;
  const AForce: Boolean = False; const ATime: Integer = 30;
  const AComment: String = ''; const AReason: TShutdownReason = srUnexpected;
  const AReasonMajor: Integer = 0; const AReasonMinor: Integer = 0;
  const AReasonMsg: String = ''): Boolean;

implementation

function DoCmd(const Cmd: String): Boolean;
begin
  //Performs command line execution on THIS computer
  try
    ShellExecute(0, nil, 'cmd.exe', PChar('/C '+Cmd), nil, SW_HIDE);
    //TODO: Check if command was actually successful, return result
    Result:= True;
  except
    Result:= False;
  end;
end;

function DoShutdownCmd(const ACmd: TShutdownCmd; const AHybrid: Boolean = False;
  const AForce: Boolean = False; const ATime: Integer = 30;
  const AComment: String = ''; const AReason: TShutdownReason = srUnexpected;
  const AReasonMajor: Integer = 0; const AReasonMinor: Integer = 0;
  const AReasonMsg: String = ''): Boolean;
var
  CmdLine: String;
begin
  //Performs shutdown command for THIS computer
  case ACmd of
    scShutdown: begin
      CmdLine:= 'shutdown /s';
      if AHybrid then
        CmdLine:= CmdLine + ' /hybrid';
      if ATime > 0 then
        CmdLine:= CmdLine + ' /t '+IntToStr(ATime);
    end;
    scRestart: begin
      CmdLine:= 'shutdown /r';
      if ATime > 0 then
        CmdLine:= CmdLine + ' /t '+IntToStr(ATime);
    end;
    scHibernate: begin
      CmdLine:= 'shutdown /h';
    end;
    else begin
      raise Exception.Create('Unsupported shutdown command.');
    end;
  end;
  if CmdLine <> '' then begin
    if AForce then
      CmdLine:= CmdLine + ' /f';
    try
      Result:= DoCmd(CmdLine); //PERFORM ACTUAL SHUTDOWN COMMAND
    except
      on E: Exception do begin
        raise Exception.Create('Failed to send command: ' + E.Message);
      end;
    end;
  end else begin
    raise Exception.Create('Unable to determine shutdown command.');
  end;
end;

function SendShutdownCmd(const AMachine: String; const ACmd: TShutdownCmd;
  const AHybrid: Boolean = False;
  const AForce: Boolean = False; const ATime: Integer = 30;
  const AComment: String = ''; const AReason: TShutdownReason = srUnexpected;
  const AReasonMajor: Integer = 0; const AReasonMinor: Integer = 0;
  const AReasonMsg: String = ''): Boolean;
var
  Req: ISuperObject;
  H: TIdHTTP;
  S: TMemoryStream;
begin
  //Sends shutdown command to another machine
  Req:= SO;
  try
    case ACmd of
      scShutdown:   Req.S['cmd']:= 'Shutdown';
      scRestart:    Req.S['cmd']:= 'Restart';
      scHibernate:  Req.S['cmd']:= 'Hibernate';
      scSleep:      Req.S['cmd']:= 'Sleep';
      scLogoff:     Req.S['cmd']:= 'Logoff';
      scLock:       Req.S['cmd']:= 'Lock';
    end;
    Req.B['hybrid']:= AHybrid;
    Req.B['force']:= AForce;
    Req.I['time']:= ATime;
    Req.S['comment']:= AComment;
    Req.O['reason']:= SO;
    case AReason of
      srExpected:   Req.O['reason'].S['type']:= 'U';
      srUnexpected: Req.O['reason'].S['type']:= 'E';
      srPlanned:    Req.O['reason'].S['type']:= 'P';
      srCustomer:   Req.O['reason'].S['type']:= 'C';
    end;
    Req.O['reason'].I['major']:= AReasonMajor;
    Req.O['reason'].I['minor']:= AReasonMinor;
    Req.O['reason'].S['msg']:= AReasonMsg;

    H:= TIdHTTP.Create(nil);
    try
      S:= TMemoryStream.Create;
      try
        Req.SaveTo(S);
        S.Position:= 0;
        H.Post('http://'+AMachine+':65469/Command', S);
        Result:= True;
      finally
        FreeAndNil(S);
      end;
    finally
      FreeAndNil(H);
    end;

  except
    on E: Exception do begin
      Result:= False;
    end;
  end;
end;

end.
