unit uTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  IdHTTP,
  SuperObject,
  JD.RemoteShutdown.PowerMonitor, Vcl.Buttons, Vcl.Samples.Spin;

type
  TForm1 = class(TForm)
    Log: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblAwayModeLabel: TLabel;
    lblBatteryPerc: TLabel;
    lblConsoleDisplay: TLabel;
    lblGlobalUser: TLabel;
    lblMonitorPower: TLabel;
    lblPowerSaver: TLabel;
    lblSessionDisplay: TLabel;
    lblSessionUser: TLabel;
    lblAwayMode: TLabel;
    Label9: TLabel;
    lblPowerSource: TLabel;
    BitBtn1: TBitBtn;
    Label10: TLabel;
    txtPerc: TSpinEdit;
    Label11: TLabel;
    lblPersonality: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    FMon: TPowerMonitor;
    FSrc: TPowerSource;
    procedure DoLog(const S: String);

    procedure PowerStatusChange(Sender: TObject);
    procedure PowerResumeAutomatic(Sender: TObject);
    procedure PowerResumeSuspend(Sender: TObject);
    procedure PowerSuspend(Sender: TObject);

    procedure PowerBatteryPercent(Sender: TObject;
      const Perc: Single);
    procedure PowerSourceChange(Sender: TObject;
      const Src: TPowerSource);
    procedure PowerConsoleDisplayStateChange(Sender: TObject;
      const State: TPowerDisplayState);
    procedure PowerGlobalUserPresence(Sender: TObject;
      const Presence: TPowerUserPresence);
    procedure PowerIdleBackgroundTask(Sender: TObject);
    procedure PowerMonitorPower(Sender: TObject;
      const State: TPowerDisplayState);
    procedure PowerSavingStatus(Sender: TObject;
      const Status: TPowerSavingStatus);
    procedure PowerSessionDisplayState(Sender: TObject;
      const State: TPowerDisplayState);
    procedure PowerSessionUserPresence(Sender: TObject;
      const Presence: TPowerUserPresence);
    procedure PowerAwayMode(Sender: TObject;
      const Mode: TPowerAwayMode);
    procedure PowerPersonality(Sender: TObject;
      const Personality: TPowerPersonality);
    procedure SendHibernate(const Machine: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin

  FMon:= TPowerMonitor.Create(nil);

  FMon.OnBatteryPercent:= PowerBatteryPercent;
  FMon.OnSourceChange:= PowerSourceChange;
  FMon.OnConsoleDisplayState:= PowerConsoleDisplayStateChange;
  FMon.OnGlobalUserPresence:= PowerGlobalUserPresence;
  FMon.OnIdleBackgroundTask:= PowerIdleBackgroundTask;
  FMon.OnMonitorPower:= PowerMonitorPower;
  FMon.OnPowerSavingStatus:= PowerSavingStatus;
  FMon.OnSessionDisplayState:= PowerSessionDisplayState;
  FMon.OnSessionUserPresence:= PowerSessionUserPresence;
  FMon.OnPowerStatusChange:= PowerStatusChange;
  FMon.OnResumeAutomatic:= PowerResumeAutomatic;
  FMon.OnResumeSuspend:= PowerResumeSuspend;
  FMon.OnSuspend:= PowerSuspend;
  FMon.OnAwayMode:= PowerAwayMode;
  FMon.OnPersonality:= PowerPersonality;

  FMon.Settings:= [psACDCPowerSource, psBatteryPercentage,
    psConsoleDisplayState, psGlobalUserPresence, psIdleBackgroundTask,
    psMonitorPower, psPowerSaving, psPowerSchemePersonality,
    psSessionDisplayStatus, psSessionUserPresence, psSystemAwayMode];
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  T: TSysCharSet;
begin
  FreeAndNil(FMon);
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Self.SendHibernate('LocalHost');
end;

procedure TForm1.DoLog(const S: String);
begin
  Log.Lines.Append(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' - ' + S);
end;

procedure TForm1.PowerStatusChange(Sender: TObject);
begin
  //DoLog('Power Status Changed');
end;

procedure TForm1.PowerSuspend(Sender: TObject);
begin
  DoLog('Suspended');
end;

procedure TForm1.PowerResumeAutomatic(Sender: TObject);
begin
  DoLog('Resume Automatic');
end;

procedure TForm1.PowerResumeSuspend(Sender: TObject);
begin
  DoLog('Resume Suspended');
end;

procedure TForm1.PowerIdleBackgroundTask(Sender: TObject);
begin
  //DoLog('Idle Background Task');
end;

procedure TForm1.PowerAwayMode(Sender: TObject; const Mode: TPowerAwayMode);
var
  S: String;
begin
  case Mode of
    paExiting: S:= 'Exiting';
    paEntering: S:= 'Entering';
  end;
  DoLog('Away Mode: ' + S);

  lblAwayMode.Caption:= S;
end;

procedure TForm1.PowerBatteryPercent(Sender: TObject; const Perc: Single);
begin
  DoLog('Battery Percentage: ' + FormatFloat('0%', Perc));

  lblBatteryPerc.Caption:= FormatFloat('0%', Perc);

  if (Perc <= txtPerc.Value) and (FSrc = TPowerSource.poDC) then begin


    //TODO: Change to send to all computers which actually need to be shut down.
    //These computers must be registered on this computer, and are supposed to
    //be the exact computers which are plugged into the same UPS.

    SendHibernate('INNO101');

    SendHibernate('LocalHost');

  end;

end;

procedure TForm1.SendHibernate(const Machine: String);
var
  H: TIdHTTP;
  S: TMemoryStream;
  O: ISuperObject;
begin
  H:= TIdHTTP.Create(nil);
  try
    O:= SO;
    O.S['cmd']:= 'Hibernate';
    O.B['hybrid']:= False;
    O.B['force']:= False;
    O.I['time']:= 30;
    O.S['comment']:= 'Fuck you, I''m shutting down';
    O.O['reason']:= SO;
    O.O['reason'].S['type']:= 'U';
    O.O['reason'].I['major']:= 0;
    O.O['reason'].I['minor']:= 0;
    O.O['reason'].S['msg']:= 'Power Loss';
    S:= TMemoryStream.Create;
    try
      O.SaveTo(S, True);
      S.Position:= 0;
      H.Post('http://'+Machine+':65469/Command', S);
    finally
      S.Free;
    end;
  finally
    H.Free;
  end;
end;

procedure TForm1.PowerSavingStatus(Sender: TObject;
  const Status: TPowerSavingStatus);
var
  S: String;
begin
  case Status of
    psSaverOff: S:= 'Off';
    psSaverOn: S:= 'On';
  end;
  DoLog('Power Saver Status: ' + S);

  lblPowerSaver.Caption:= S;
end;

procedure TForm1.PowerSessionDisplayState(Sender: TObject;
  const State: TPowerDisplayState);
var
  S: String;
begin
  case State of
    pdOff: S:= 'Off';
    pdOn: S:= 'On';
    pdDimmed: S:= 'Dimmed';
  end;
  DoLog('Session Display State Changed: ' + S);

  lblSessionDisplay.Caption:= S;
end;

procedure TForm1.PowerSessionUserPresence(Sender: TObject;
  const Presence: TPowerUserPresence);
var
  S: String;
begin
  case Presence of
    puPresent: S:= 'Present';
    puInactive: S:= 'Inactive';
  end;
  DoLog('Session User Presence: ' + S);

  lblSessionUser.Caption:= S;
end;

procedure TForm1.PowerSourceChange(Sender: TObject;
  const Src: TPowerSource);
var
  S: String;
begin
  FSrc:= Src;
  case Src of
    poAC: S:= 'AC Power';
    poDC: S:= 'DC Power';
    poHot: S:= 'Hot Power';
  end;
  DoLog('Power Source Changed: ' + S);

  lblPowerSource.Caption:= S;
end;

procedure TForm1.PowerConsoleDisplayStateChange(Sender: TObject;
  const State: TPowerDisplayState);
var
  S: String;
begin
  case State of
    pdOff: S:= 'Off';
    pdOn: S:= 'On';
    pdDimmed: S:= 'Dimmed';
  end;
  DoLog('Global Display State Changed: ' + S);

  lblConsoleDisplay.Caption:= S;
end;

procedure TForm1.PowerGlobalUserPresence(Sender: TObject;
  const Presence: TPowerUserPresence);
var
  S: String;
begin
  case Presence of
    puPresent: S:= 'Present';
    puInactive: S:= 'Inactive';
  end;
  DoLog('Global User Presence: ' + S);

  lblGlobalUser.Caption:= S;
end;

procedure TForm1.PowerMonitorPower(Sender: TObject;
  const State: TPowerDisplayState);
var
  S: String;
begin
  case State of
    pdOff: S:= 'Off';
    pdOn: S:= 'On';
    pdDimmed: S:= 'Dimmed';
  end;
  DoLog('Monitor Power Changed: ' + S);

  lblMonitorPower.Caption:= S;
end;

procedure TForm1.PowerPersonality(Sender: TObject;
  const Personality: TPowerPersonality);
var
  S: String;
begin
  case Personality of
    ppHighPerformance: S:= 'High Performance';
    ppPowerSaver: S:= 'Power Saver';
    ppAutomatic: S:= 'Automatic';
  end;
  DoLog('Power Personality Changed: ' + S);

  lblPersonality.Caption:= S;
end;

end.
