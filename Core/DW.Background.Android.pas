unit DW.Background.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.App,
  // DW
  DW.MultiReceiver.Android;

type
  TBackgroundMonitor = class;

  TBackgroundMonitorReceiver = class(TMultiReceiver)
  private
    FMonitor: TBackgroundMonitor;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AMonitor: TBackgroundMonitor);
  end;

  TBackgroundStateChangeEvent = procedure(Sender: TObject; const Value: Boolean) of object;

  TBackgroundMonitor = class(TObject)
  private
    FDozeAlarmIntent: JPendingIntent;
    FDozeAlarmInterval: Integer;
    FReceiver: TBackgroundMonitorReceiver;
    FServiceName: string;
    FOnDozeChange: TBackgroundStateChangeEvent;
    FOnScreenLockChange: TBackgroundStateChangeEvent;
    function CreateDozeAlarm(const AAction: string; const AAlarm: Int64): Boolean;
    procedure DozeModeChange(const ADozed: Boolean);
    procedure ScreenLockChange(const ALocked: Boolean);
    function GetIsDozed: Boolean;
    function GetIsScreenLocked: Boolean;
  protected
    procedure ReceiverReceive(intent: JIntent);
  public
    constructor Create;
    procedure StartDozeAlarm;
    procedure StopDozeAlarm;
    property DozeAlarmInterval: Integer read FDozeAlarmInterval write FDozeAlarmInterval;
    property IsDozed: Boolean read GetIsDozed;
    property IsScreenLocked: Boolean read GetIsScreenLocked;
    property ServiceName: string read FServiceName write FServiceName;
    property OnDozeChange: TBackgroundStateChangeEvent read FOnDozeChange write FOnDozeChange;
    property OnScreenLockChange: TBackgroundStateChangeEvent read FOnScreenLockChange write FOnScreenLockChange;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  AndroidApi.JNI.Os, Androidapi.Helpers, AndroidApi.JNI.JavaTypes,
  // DW
  DW.OSLog, DW.Android.Helpers, DW.Consts.Android, DW.OSDevice;

{ TBackgroundMonitorReceiver }

constructor TBackgroundMonitorReceiver.Create(const AMonitor: TBackgroundMonitor);
begin
  inherited Create;
  FMonitor := AMonitor;
end;

procedure TBackgroundMonitorReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJIntent.JavaClass.ACTION_SCREEN_ON);
  IntentFilter.addAction(TJIntent.JavaClass.ACTION_SCREEN_OFF);
  IntentFilter.addAction(TJIntent.JavaClass.ACTION_USER_PRESENT);
  if TOSVersion.Check(6) then
    IntentFilter.addAction(TJPowerManager.JavaClass.ACTION_DEVICE_IDLE_MODE_CHANGED);
end;

procedure TBackgroundMonitorReceiver.Receive(context: JContext; intent: JIntent);
begin
  FMonitor.ReceiverReceive(intent);
end;

{ TBackgroundMonitor }

constructor TBackgroundMonitor.Create;
begin
  inherited Create;
  FReceiver := TBackgroundMonitorReceiver.Create(Self);
end;

procedure TBackgroundMonitor.ReceiverReceive(intent: JIntent);
begin
  if intent.getAction.equals(TJIntent.JavaClass.ACTION_USER_PRESENT) or intent.getAction.equals(TJIntent.JavaClass.ACTION_SCREEN_OFF)
    or intent.getAction.equals(TJIntent.JavaClass.ACTION_SCREEN_ON) then
  begin
    ScreenLockChange(IsScreenLocked);
  end
  // Otherwise, check for "doze" mode changes
  else if TOSVersion.Check(6) and intent.getAction.equals(TJPowerManager.JavaClass.ACTION_DEVICE_IDLE_MODE_CHANGED) then
    DozeModeChange(IsDozed);
end;

procedure TBackgroundMonitor.ScreenLockChange(const ALocked: Boolean);
begin
  if Assigned(FOnScreenLockChange) then
    FOnScreenLockChange(Self, ALocked);
end;

procedure TBackgroundMonitor.DozeModeChange(const ADozed: Boolean);
begin
  if Assigned(FOnDozeChange) then
    FOnDozeChange(Self, ADozed);
  if ADozed then
    StartDozeAlarm
  else
    StopDozeAlarm;
end;

function TBackgroundMonitor.GetIsDozed: Boolean;
begin
  Result := TAndroidHelperEx.PowerManager.isDeviceIdleMode;
end;

function TBackgroundMonitor.GetIsScreenLocked: Boolean;
begin
  Result := TAndroidHelperEx.KeyguardManager.inKeyguardRestrictedInputMode;
end;

function TBackgroundMonitor.CreateDozeAlarm(const AAction: string; const AAlarm: Int64): Boolean;
var
  LIntent: JIntent;
begin
  Result := False;
  // Make doubly sure that the old alarm is removed
  StopDozeAlarm;
  LIntent := TJIntent.JavaClass.init(StringToJString(AAction));
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cDWBroadcastReceiverName));
  // The broadcast receiver that monitors for alarms needs to know whether it was requested by a service
  LIntent.putExtra(StringToJString('ServiceName'), StringToJString(FServiceName));
  FDozeAlarmIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LIntent, TJPendingIntent.JavaClass.FLAG_CANCEL_CURRENT);
  if FDozeAlarmIntent <> nil then
  begin
    TAndroidHelper.AlarmManager.setAndAllowWhileIdle(TJAlarmManager.JavaClass.RTC_WAKEUP, AAlarm, FDozeAlarmIntent);
    Result := True;
  end
  else
    TOSLog.d('Unable to create a pending intent for action: %s', [AAction]);
end;

procedure TBackgroundMonitor.StartDozeAlarm;
var
  LAlarm: Int64;
begin
  if (FDozeAlarmInterval > 0) and TOSVersion.Check(6) then
  begin
    LAlarm := TAndroidHelperEx.GetTimeFromNowInMillis(FDozeAlarmInterval);
    if CreateDozeAlarm(cDWBroadcastReceiverActionServiceAlarm, LAlarm) then
      TOSLog.d('Started doze alarm', True)
    else
      TOSLog.d('Unable to start doze alarm', True);
  end;
end;

procedure TBackgroundMonitor.StopDozeAlarm;
begin
  if FDozeAlarmIntent <> nil then
  begin
    TAndroidHelper.AlarmManager.cancel(FDozeAlarmIntent);
    TOSLog.d('Cancelled doze alarm', True);
  end;
  FDozeAlarmIntent := nil;
end;

end.
