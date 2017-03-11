unit JD.Power.Monitor;

(*
  JD Power Monitor
  by Jerry Dodge

  Purpose: To monitor the current state of power on the computer, and trigger
  events when different power related changes occur.

  Component: TPowerMonitor
  - Create an instance of TPowerMonitor component
    - Recommended to only use one instance, not multiple
  - Choose desired power settings to get notified of using Settings property
  - Implement event handlers for those events you wish to monitor
  - Component automatically takes care of the rest of the work
*)

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Winapi.ActiveX, Winapi.Windows, Winapi.Messages;

type
  TPowerSetting = (psACDCPowerSource, psBatteryPercentage,
    psConsoleDisplayState, psGlobalUserPresence, psIdleBackgroundTask,
    psMonitorPower, psPowerSaving, psPowerSchemePersonality,
    psSessionDisplayStatus, psSessionUserPresence, psSystemAwayMode);
  TPowerSettings = set of TPowerSetting;

  TPowerSource = (poAC, poDC, poHot);

  TPowerDisplayState = (pdOff, pdOn, pdDimmed);

  TPowerUserPresence = (puPresent = 0, puInactive = 2);

  TPowerSavingStatus = (psSaverOff, psSaverOn);

  TPowerAwayMode = (paExiting, paEntering);

  TPowerPersonality = (ppHighPerformance, ppPowerSaver, ppAutomatic);

  TPowerMonitorSettingHandles = array[TPowerSetting] of HPOWERNOTIFY;

  TPowerQueryEndSessionEvent = procedure(Sender: TObject; var EndSession: Boolean) of object;

  TPowerEndSessionEvent = procedure(Sender: TObject) of object;

  TPowerSettingSourceChangeEvent = procedure(Sender: TObject;
    const Src: TPowerSource) of object;

  TPowerSettingBatteryPercentEvent = procedure(Sender: TObject;
    const Perc: Single) of object;

  TPowerSettingDisplayStateEvent = procedure(Sender: TObject;
    const State: TPowerDisplayState) of object;

  TPowerSettingUserPresenceEvent = procedure(Sender: TObject;
    const Presence: TPowerUserPresence) of object;

  TPowerSettingSavingEvent = procedure(Sender: TObject;
    const Status: TPowerSavingStatus) of object;

  TPowerAwayModeEvent = procedure(Sender: TObject;
    const Mode: TPowerAwayMode) of object;

  TPowerPersonalityEvent = procedure(Sender: TObject;
    const Personality: TPowerPersonality) of object;

  TPowerMonitor = class(TComponent)
  private
    FHandle: HWND;
    FSettingHandles: TPowerMonitorSettingHandles;
    FSettings: TPowerSettings;
    FBatteryPresent: Boolean;
    FOnQueryEndSession: TPowerQueryEndSessionEvent;
    FOnEndSession: TPowerEndSessionEvent;
    FOnPowerStatusChange: TNotifyEvent;
    FOnResumeAutomatic: TNotifyEvent;
    FOnResumeSuspend: TNotifyEvent;
    FOnSuspend: TNotifyEvent;
    FOnSourceChange: TPowerSettingSourceChangeEvent;
    FOnBatteryPercent: TPowerSettingBatteryPercentEvent;
    FOnConsoleDisplayState: TPowerSettingDisplayStateEvent;
    FOnGlobalUserPresence: TPowerSettingUserPresenceEvent;
    FOnIdleBackgroundTask: TNotifyEvent;
    FOnMonitorPower: TPowerSettingDisplayStateEvent;
    FOnPowerSavingStatus: TPowerSettingSavingEvent;
    FOnSessionDisplayState: TPowerSettingDisplayStateEvent;
    FOnSessionUserPresence: TPowerSettingUserPresenceEvent;
    FOnAwayMode: TPowerAwayModeEvent;
    FOnPersonality: TPowerPersonalityEvent;
    procedure UnregisterSettings;
    procedure RegisterSettings;
    procedure SetSettings(const Value: TPowerSettings);
  protected
    procedure HandlePowerSetting(const Val: PPowerBroadcastSetting);
    procedure WndMethod(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Settings: TPowerSettings read FSettings write SetSettings;
    property OnQueryEndSession: TPowerQueryEndSessionEvent
      read FOnQueryEndSession write FOnQueryEndSession;
    property OnEndSession: TPowerEndSessionEvent
      read FOnEndSession write FOnEndSession;
    property OnPowerStatusChange: TNotifyEvent
      read FOnPowerStatusChange write FOnPowerStatusChange;
    property OnResumeAutomatic: TNotifyEvent
      read FOnResumeAutomatic write FOnResumeAutomatic;
    property OnResumeSuspend: TNotifyEvent
      read FOnResumeSuspend write FOnResumeSuspend;
    property OnSuspend: TNotifyEvent
      read FOnSuspend write FOnSuspend;
    property OnSourceChange: TPowerSettingSourceChangeEvent
      read FOnSourceChange write FOnSourceChange;
    property OnBatteryPercent: TPowerSettingBatteryPercentEvent
      read FOnBatteryPercent write FOnBatteryPercent;
    property OnConsoleDisplayState: TPowerSettingDisplayStateEvent
      read FOnConsoleDisplayState write FOnConsoleDisplayState;
    property OnGlobalUserPresence: TPowerSettingUserPresenceEvent
      read FOnGlobalUserPresence write FOnGlobalUserPresence;
    property OnIdleBackgroundTask: TNotifyEvent
      read FOnIdleBackgroundTask write FOnIdleBackgroundTask;
    property OnMonitorPower: TPowerSettingDisplayStateEvent
      read FOnMonitorPower write FOnMonitorPower;
    property OnPowerSavingStatus: TPowerSettingSavingEvent
      read FOnPowerSavingStatus write FOnPowerSavingStatus;
    property OnSessionDisplayState: TPowerSettingDisplayStateEvent
      read FOnSessionDisplayState write FOnSessionDisplayState;
    property OnSessionUserPresence: TPowerSettingUserPresenceEvent
      read FOnSessionUserPresence write FOnSessionUserPresence;
    property OnAwayMode: TPowerAwayModeEvent
      read FOnAwayMode write FOnAwayMode;
    property OnPersonality: TPowerPersonalityEvent
      read FOnPersonality write FOnPersonality;
  end;

implementation

{ TPowerMonitor }

constructor TPowerMonitor.Create(AOwner: TComponent);
begin
  inherited;
  FBatteryPresent:= False;
  FHandle := AllocateHWnd(WndMethod);
end;

destructor TPowerMonitor.Destroy;
begin
  UnregisterSettings;
  DeallocateHWnd(FHandle);
  inherited;
end;

procedure TPowerMonitor.SetSettings(const Value: TPowerSettings);
begin
  UnregisterSettings;
  FSettings := Value;
  RegisterSettings;
end;

procedure TPowerMonitor.WndMethod(var Msg: TMessage);
var
  Handled: Boolean;
  MQ: TWMQueryEndSession;
  ME: TWMEndSession;
  B: Boolean;
begin
  Handled := True;
  case Msg.Msg of
    WM_QUERYENDSESSION: begin
      MQ:= TWMQueryEndSession(Msg);
      B:= True;
      //TODO: Test for any of the 3 possible values in mask
      //ENDSESSION_CLOSEAPP
      //ENDSESSION_CRITICAL
      //ENDSESSION_LOGOFF
      if Assigned(Self.FOnQueryEndSession) then
        FOnQueryEndSession(Self, B);
      if not B then
        MQ.Result:= 0; //Instructs Windows not to proceed shutting down
    end;
    WM_ENDSESSION: begin
      ME:= TWMEndSession(Msg);
      B:= True;
      case Msg.LParam of
        0: begin
          //System is shutting down or restarting, cannot determine which event...
        end;
        else begin
          //TODO: Test for any of the 3 possible values in mask
          //ENDSESSION_CLOSEAPP
          //ENDSESSION_CRITICAL
          //ENDSESSION_LOGOFF

        end;
      end;
    end;
    WM_POWERBROADCAST: begin
      //TODO: Why is this never received when inside of a thread?
      case Msg.WParam of
        PBT_APMPOWERSTATUSCHANGE: begin
          //Power status has changed.
          if Assigned(FOnPowerStatusChange) then
            FOnPowerStatusChange(Self);
        end;
        PBT_APMRESUMEAUTOMATIC: begin
          //Operation is resuming automatically from a low-power state.
          //This message is sent every time the system resumes.
          if Assigned(FOnResumeAutomatic) then
            FOnResumeAutomatic(Self);
        end;
        PBT_APMRESUMESUSPEND: begin
          //Operation is resuming from a low-power state. This message
          //is sent after PBT_APMRESUMEAUTOMATIC if the resume is triggered
          //by user input, such as pressing a key.
          if Assigned(FOnResumeSuspend) then
            FOnResumeSuspend(Self);
        end;
        PBT_APMSUSPEND: begin
          //System is suspending operation.
          if Assigned(FOnSuspend) then
            FOnSuspend(Self);
        end;
        PBT_POWERSETTINGCHANGE: begin
          //A power setting change event has been received.
          HandlePowerSetting(PPowerBroadcastSetting(Msg.LParam));
        end;
        else begin

        end;
      end;
    end
    else Handled := False;
  end;
  if Handled then
    Msg.Result := 0
  else
    Msg.Result := DefWindowProc(FHandle, Msg.Msg,
      Msg.WParam, Msg.LParam);
end;

procedure TPowerMonitor.HandlePowerSetting(const Val: PPowerBroadcastSetting);
var
  Pers: TPowerPersonality;
  function ValAsDWORD: DWORD;
  begin
    Result:= DWORD(Val.Data[0]);
  end;
  function ValAsGUID: TGUID;
  begin
    Result:= StringToGUID('{00000000-0000-0000-0000-000000000000}'); //Default
    if SizeOf(TGUID) = Val.DataLength then begin
      Move(Val.Data, Result, Val.DataLength);
    end;
  end;
  function IsVal(G: String): Boolean;
  begin
    Result:= Assigned(Val);
    if Result then
      Result:= IsEqualGUID(StringToGUID(G), Val.PowerSetting);
  end;
  function IsValGuid(G: String): Boolean;
  begin
    Result:= Assigned(Val);
    if Result then
      Result:= IsEqualGUID(StringToGUID(G), ValAsGUID);
  end;
begin
  if IsVal('{5d3e9a59-e9D5-4b00-a6bd-ff34ff516548}') then begin
    //GUID_ACDC_POWER_SOURCE
    if Assigned(FOnSourceChange) then
      FOnSourceChange(Self, TPowerSource(ValAsDWORD));
  end else
  if IsVal('{a7ad8041-b45a-4cae-87a3-eecbb468a9e1}') then begin
    //GUID_BATTERY_PERCENTAGE_REMAINING
    //We assume that if we get this message, that there is a battery connected.
    //Otherwise if this never occurs, then a battery is not present.
    //TODO: How to handle if battery is detached and no longer present?
    FBatteryPresent:= True;
    if Assigned(FOnBatteryPercent) then
      FOnBatteryPercent(Self, ValAsDWORD);
  end else
  if IsVal('{6fe69556-704a-47a0-8f24-c28d936fda47}') then begin
    //GUID_CONSOLE_DISPLAY_STATE
    if Assigned(FOnConsoleDisplayState) then
      FOnConsoleDisplayState(Self, TPowerDisplayState(ValAsDWORD));
  end else
  if IsVal('{786E8A1D-B427-4344-9207-09E70BDCBEA9}') then begin
    //GUID_GLOBAL_USER_PRESENCE
    if Assigned(FOnGlobalUserPresence) then
      FOnGlobalUserPresence(Self, TPowerUserPresence(ValAsDWORD));
  end else
  if IsVal('{515c31d8-f734-163d-a0fd-11a08c91e8f1}') then begin
    //GUID_IDLE_BACKGROUND_TASK
    if Assigned(FOnIdleBackgroundTask) then
      FOnIdleBackgroundTask(Self);
  end else
  if IsVal('{02731015-4510-4526-99e6-e5a17ebd1aea}') then begin
    //GUID_MONITOR_POWER_ON
    if Assigned(FOnMonitorPower) then
      FOnMonitorPower(Self, TPowerDisplayState(ValAsDWORD));
  end else
  if IsVal('{E00958C0-C213-4ACE-AC77-FECCED2EEEA5}') then begin
    //GUID_POWER_SAVING_STATUS
    if Assigned(FOnPowerSavingStatus) then
      FOnPowerSavingStatus(Self, TPowerSavingStatus(ValAsDWORD));
  end else
  if IsVal('{245d8541-3943-4422-b025-13A784F679B7}') then begin
    //GUID_POWERSCHEME_PERSONALITY
    if IsValGuid('{8c5e7fda-e8bf-4a96-9a85-a6e23a8c635c}') then begin
      Pers:= TPowerPersonality.ppHighPerformance;
    end else
    if IsValGuid('{a1841308-3541-4fab-bc81-f71556f20b4a}') then begin
      Pers:= TPowerPersonality.ppPowerSaver;
    end else
    if IsValGuid('{381b4222-f694-41f0-9685-ff5bb260df2e}') then begin
      Pers:= TPowerPersonality.ppAutomatic;
    end else begin
      //TODO: Handle unrecognized GUID
      Pers:= TPowerPersonality.ppAutomatic;
    end;
    if Assigned(FOnPersonality) then
      FOnPersonality(Self, Pers);
  end else
  if IsVal('{2B84C20E-AD23-4ddf-93DB-05FFBD7EFCA5}') then begin
    //GUID_SESSION_DISPLAY_STATUS
    if Assigned(FOnSessionDisplayState) then
      FOnSessionDisplayState(Self, TPowerDisplayState(ValAsDWORD));
  end else
  if IsVal('{3C0F4548-C03F-4c4d-B9F2-237EDE686376}') then begin
    //GUID_SESSION_USER_PRESENCE
    if Assigned(FOnSessionUserPresence) then
      FOnSessionUserPresence(Self, TPowerUserPresence(ValAsDWORD));
  end else
  if IsVal('{98a7f580-01f7-48aa-9c0f-44352c29e5C0}') then begin
    //GUID_SYSTEM_AWAYMODE
    if Assigned(FOnAwayMode) then
      FOnAwayMode(Self, TPowerAwayMode(ValAsDWORD));
  end else begin
    //TODO: Handle Unrecognized GUID
  end;
end;

function PowerSettingGUID(const Setting: TPowerSetting): TGUID;
begin
  case Setting of
    psACDCPowerSource: Result:= StringToGUID('{5d3e9a59-e9D5-4b00-a6bd-ff34ff516548}');
    psBatteryPercentage: Result:= StringToGUID('{a7ad8041-b45a-4cae-87a3-eecbb468a9e1}');
    psConsoleDisplayState: Result:= StringToGUID('{6fe69556-704a-47a0-8f24-c28d936fda47}');
    psGlobalUserPresence: Result:= StringToGUID('{786E8A1D-B427-4344-9207-09E70BDCBEA9}');
    psIdleBackgroundTask: Result:= StringToGUID('{515c31d8-f734-163d-a0fd-11a08c91e8f1}');
    psMonitorPower: Result:= StringToGUID('{02731015-4510-4526-99e6-e5a17ebd1aea}');
    psPowerSaving: Result:= StringToGUID('{E00958C0-C213-4ACE-AC77-FECCED2EEEA5}');
    psPowerSchemePersonality: Result:= StringToGUID('{245d8541-3943-4422-b025-13A784F679B7}');
    psSessionDisplayStatus: Result:= StringToGUID('{2B84C20E-AD23-4ddf-93DB-05FFBD7EFCA5}');
    psSessionUserPresence: Result:= StringToGUID('{3C0F4548-C03F-4c4d-B9F2-237EDE686376}');
    psSystemAwayMode: Result:= StringToGUID('{98a7f580-01f7-48aa-9c0f-44352c29e5C0}');
  end;
end;

procedure TPowerMonitor.RegisterSettings;
var
  V: TPowerSetting;
begin
  for V := Low(TPowerSetting) to High(TPowerSetting) do begin
    if V in FSettings then begin
      FSettingHandles[V]:= RegisterPowerSettingNotification(FHandle,
        PowerSettingGUID(V), 0);
    end;
  end;
end;

procedure TPowerMonitor.UnregisterSettings;
var
  V: TPowerSetting;
begin
  for V := Low(TPowerSetting) to High(TPowerSetting) do begin
    if V in FSettings then begin
      UnregisterPowerSettingNotification(FSettingHandles[V]);
    end;
  end;
end;

end.
