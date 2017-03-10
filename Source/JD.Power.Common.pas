unit JD.Power.Common;

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  ShellApi;

const
  GLOBAL_PORT = 65468;
  CLIENT_PORT = 65469;

type
  TShutdownCmd = (scShutdown, scRestart, scHibernate, scSleep, scLogoff, scLock);

  TShutdownReason = (srExpected, srUnexpected, srPlanned, srCustomer);

procedure DoCmd(const Cmd: String);
function DoShutdownCmd(const ACmd: TShutdownCmd; const AHybrid: Boolean = False;
  const AForce: Boolean = False; const ATime: Integer = 30;
  const AComment: String = ''; const AReason: TShutdownReason = srUnexpected;
  const AReasonMajor: Integer = 0; const AReasonMinor: Integer = 0;
  const AReasonMsg: String = ''): Boolean;

implementation

procedure DoCmd(const Cmd: String);
begin
  ShellExecute(0, nil, 'cmd.exe', PChar('/C '+Cmd), nil, SW_HIDE);
end;

function DoShutdownCmd(const ACmd: TShutdownCmd; const AHybrid: Boolean = False;
  const AForce: Boolean = False; const ATime: Integer = 30;
  const AComment: String = ''; const AReason: TShutdownReason = srUnexpected;
  const AReasonMajor: Integer = 0; const AReasonMinor: Integer = 0;
  const AReasonMsg: String = ''): Boolean;
var
  CmdLine: String;
begin
  //Result:= False;
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
      DoCmd(CmdLine); //PERFORM ACTUAL SHUTDOWN COMMAND
      Result:= True; //TODO: Result:= DoCmd(CmdLine);
    except
      on E: Exception do begin
        raise Exception.Create('Failed to send command: ' + E.Message);
      end;
    end;
  end else begin
    raise Exception.Create('Unable to determine shutdown command.');
  end;
end;

end.
