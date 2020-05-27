unit DW.Notifications.Android;

// ***************** NOTE **************************
//      THIS UNIT IS CURRENTLY EXPERIMENTAL
//           USE AT YOUR OWN RISK!
//
// It may or may not be removed from the Kastri library

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
  Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  // DW
  DW.Notifications, DW.MultiReceiver.Android, DW.Androidapi.JNI.App, DW.Androidapi.JNI.SupportV4;

type
  TNotificationReceiver = class(TMultiReceiver)
  private
    FNotifications: TNotifications;
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    constructor Create(const ANotifications: TNotifications);
  end;

  TPlatformNotificationChannel = class(TCustomPlatformNotificationChannel)
  private
    FChannel: JNotificationChannel;
  protected
    procedure DoRegisterChannel; override;
    function GetDescription: string; override;
    function GetGroup: string; override;
    function GetId: string; override;
    function GetImportance: Integer; override;
    function GetLightColor: Integer; override;
    function GetLockScreenVisibility: Integer; override;
    function GetName: string; override;
    function GetSound: string; override;
    procedure SetDescription(const Value: string); override;
    procedure SetGroup(const Value: string); override;
    procedure SetImportance(const Value: Integer); override;
    procedure SetLightColor(const Value: Integer); override;
    procedure SetLockScreenVisibility(const Value: Integer); override;
    procedure SetName(const Value: string); override;
    procedure SetSound(const Value: string); override;
  public
    constructor Create(const AId, AName: string; const AImportance: Integer); override;
  end;

  TPlatformNotifications = class(TCustomPlatformNotifications)
  private
    class var FNotificationManager: JNotificationManager;
    class function GetNotificationManager: JNotificationManager; static;
  private
    FNotificationChannel: JNotificationChannel;
    FNotificationReceiver: TNotificationReceiver;
    FNotificationStore: JSharedPreferences;
    function GetNativeNotification(const ANotification: TNotification; const AID: Integer): JNotification;
    function GetNotificationPendingIntent(const ANotification: TNotification; const AID: Integer): JPendingIntent;
    function GetUniqueID: Integer;
    function GetNotificationIntent(const ANotification: TNotification; const AID: Integer): JPendingIntent;
    procedure RemoveNotification(const ANotification: TNotification);
    function RetrieveNotification(const AName: string; var ANotification: TNotification): Integer;
    procedure StoreNotification(const ANotification: TNotification; const AID: Integer);
  protected
    class property NotificationManager: JNotificationManager read GetNotificationManager;
  protected
    procedure CancelAll; override;
    procedure CancelNotification(const AName: string); override;
    procedure PresentNotification(const ANotification: TNotification); override;
    procedure ScheduleNotification(const ANotification: TNotification); override;
  public
    constructor Create(const ANotifications: TNotifications); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.DateUtils, System.TimeSpan,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Net, Androidapi.JNI.Os, Androidapi.JNI.Embarcadero,
  // REST
  REST.Json,
  // DW
  DW.Androidapi.JNI.DWMultiBroadcastReceiver, DW.Android.Helpers;

type
  TOpenNotifications = class(TNotifications);

{ TNotificationReceiver }

constructor TNotificationReceiver.Create(const ANotifications: TNotifications);
begin
  inherited Create(True);
  FNotifications := ANotifications;
end;

procedure TNotificationReceiver.ConfigureActions;
begin
  inherited;
  IntentFilter.addAction(TJDWMultiBroadcastReceiver.JavaClass.ACTION_NOTIFICATION);
end;

procedure TNotificationReceiver.Receive(context: JContext; intent: JIntent);
var
  LNativeNotification: JNotification;
  LNotification: TNotification;
begin
  LNativeNotification := TJNotification.Wrap(intent.getParcelableExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION));
  LNotification.Name := JStringToString(LNativeNotification.extras.getString(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_NAME));
  LNotification.Title := JCharSequenceToStr(LNativeNotification.extras.getCharSequence(TJNotification.JavaClass.EXTRA_TITLE));
  LNotification.AlertBody := JCharSequenceToStr(LNativeNotification.extras.getCharSequence(TJNotification.JavaClass.EXTRA_TEXT));
  LNotification.Number := LNativeNotification.number;
  LNotification.FireDate := Now;
  LNotification.RepeatInterval := TRepeatInterval(intent.getIntExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_REPEATINTERVAL, 0));
  TOpenNotifications(FNotifications).DoNotificationReceived(LNotification);
end;

{ TPlatformNotificationChannel }

constructor TPlatformNotificationChannel.Create(const AId, AName: string; const AImportance: Integer);
begin
  inherited;
  FChannel := TJNotificationChannel.JavaClass.init(StringToJString(AId), StrToJCharSequence(AName), AImportance);
end;

procedure TPlatformNotificationChannel.DoRegisterChannel;
begin
  TPlatformNotifications.NotificationManager.createNotificationChannel(FChannel);
end;

function TPlatformNotificationChannel.GetDescription: string;
begin
  Result := JStringToString(FChannel.getDescription);
end;

function TPlatformNotificationChannel.GetGroup: string;
begin
  Result := JStringToString(FChannel.getGroup);
end;

function TPlatformNotificationChannel.GetId: string;
begin
  Result := JStringToString(FChannel.getId);
end;

function TPlatformNotificationChannel.GetImportance: Integer;
begin
  Result := FChannel.getImportance;
end;

function TPlatformNotificationChannel.GetLightColor: Integer;
begin
  Result := FChannel.getLightColor;
end;

function TPlatformNotificationChannel.GetLockScreenVisibility: Integer;
begin
  Result := FChannel.getLockscreenVisibility;
end;

function TPlatformNotificationChannel.GetName: string;
begin
  Result := JCharSequenceToStr(FChannel.getName);
end;

function TPlatformNotificationChannel.GetSound: string;
begin
  Result := JURIToStr(FChannel.getSound);
end;

procedure TPlatformNotificationChannel.SetDescription(const Value: string);
begin
  FChannel.setDescription(StringToJString(Value));
end;

procedure TPlatformNotificationChannel.SetGroup(const Value: string);
begin
  //!!!! Cannot set this if already "registered", unless current value is blank
  FChannel.setDescription(StringToJString(Value));
end;

procedure TPlatformNotificationChannel.SetImportance(const Value: Integer);
begin
  FChannel.setImportance(Value);
end;

procedure TPlatformNotificationChannel.SetLightColor(const Value: Integer);
begin
  FChannel.setLightColor(Value);
end;

procedure TPlatformNotificationChannel.SetLockScreenVisibility(const Value: Integer);
begin
  FChannel.setLockscreenVisibility(Value);
end;

procedure TPlatformNotificationChannel.SetName(const Value: string);
begin
  FChannel.setName(StrToJCharSequence(Value));
end;

procedure TPlatformNotificationChannel.SetSound(const Value: string);
begin
  FChannel.setSound(StrToJURI(Value), FChannel.getAudioAttributes); //!!!!!
end;

{ TPlatformNotifications }

constructor TPlatformNotifications.Create(const ANotifications: TNotifications);
begin
  inherited;
  FNotificationStore := TAndroidHelper.Context.getSharedPreferences(StringToJString(ClassName), TJContext.JavaClass.MODE_PRIVATE);
  if TAndroidHelperEx.CheckBuildAndTarget(TAndroidHelperEx.OREO) then
  begin
    // TJNotificationChannel.JavaClass.DEFAULT_CHANNEL_ID
    FNotificationChannel := TJNotificationChannel.JavaClass.init(TAndroidHelper.Context.getPackageName, StrToJCharSequence('default'), 4);
    FNotificationChannel.enableLights(True);
    FNotificationChannel.enableVibration(True);
    FNotificationChannel.setLightColor(TJColor.JavaClass.GREEN);
    FNotificationChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
    NotificationManager.createNotificationChannel(FNotificationChannel);
  end;
  FNotificationReceiver := TNotificationReceiver.Create(ANotifications);
end;

destructor TPlatformNotifications.Destroy;
begin
  FNotificationStore := nil;
  FNotificationChannel := nil;
  FNotificationReceiver.Free;
  inherited;
end;

class function TPlatformNotifications.GetNotificationManager: JNotificationManager;
var
  LService: JObject;
begin
  if FNotificationManager = nil then
  begin
    LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
    FNotificationManager := TJNotificationManager.Wrap((LService as ILocalObject).GetObjectID);
  end;
  Result := FNotificationManager;
end;

function TPlatformNotifications.GetUniqueID: Integer;
const
  cUniqueIDKey = 'ZZZUniqueID';
var
  LEditor: JSharedPreferences_Editor;
begin
  Result := FNotificationStore.getInt(StringToJString(cUniqueIDKey), 1);
  LEditor := FNotificationStore.edit;
  try
    LEditor.putInt(StringToJString(cUniqueIDKey), Result + 1);
  finally
    LEditor.apply;
  end;
end;

function TPlatformNotifications.GetNotificationIntent(const ANotification: TNotification; const AID: Integer): JPendingIntent;
var
  LIntent: JIntent;
begin
  LIntent := TAndroidHelper.Context.getPackageManager().getLaunchIntentForPackage(TAndroidHelper.Context.getPackageName());
  LIntent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_SINGLE_TOP or TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_TOP);
  Result := TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, AID, LIntent, TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
end;

function TPlatformNotifications.GetNotificationPendingIntent(const ANotification: TNotification; const AID: Integer): JPendingIntent;
var
  LIntent: JIntent;
  LNotification: JNotification;
begin
  LNotification := GetNativeNotification(ANotification, AID);
  LNotification.extras.putString(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_NAME, StringToJString(ANotification.Name));
  LNotification.extras.putInt(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_REPEATINTERVAL, Integer(Ord(ANotification.RepeatInterval)));
  LIntent := TJIntent.Create;
  LIntent.setClass(TAndroidHelper.Context, TJDWMultiBroadcastReceiver.getClass);
  LIntent.setAction(TJDWMultiBroadcastReceiver.JavaClass.ACTION_NOTIFICATION);
  LIntent.putExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_ID, AID);
  LIntent.putExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION, TJParcelable.Wrap((LNotification as ILocalObject).GetObjectID));
  Result := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, AID, LIntent, TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT);
end;

function TPlatformNotifications.GetNativeNotification(const ANotification: TNotification; const AID: Integer): JNotification;
var
  LBuilder: JNotificationCompat_Builder;
begin
  LBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context)
    .setDefaults(TJNotification.JavaClass.DEFAULT_LIGHTS)
    .setSmallIcon(TAndroidHelperEx.GetDefaultIconID)
    .setContentTitle(StrToJCharSequence(ANotification.Title))
    .setContentText(StrToJCharSequence(ANotification.AlertBody))
    .setTicker(StrToJCharSequence(ANotification.AlertBody))
    .setContentIntent(GetNotificationIntent(ANotification, AID))
    .setNumber(ANotification.Number)
    .setAutoCancel(True)
    .setWhen(TJDate.Create.getTime);
  if FNotificationChannel <> nil then
    LBuilder := LBuilder.setChannelId(FNotificationChannel.getId);
  if ANotification.EnableSound then
  begin
    if ANotification.SoundName.IsEmpty then
      LBuilder := LBuilder.setSound(TAndroidHelperEx.GetDefaultNotificationSound)
    else
      LBuilder := LBuilder.setSound(StrToJURI(ANotification.SoundName));
  end;
  Result := LBuilder.Build;
end;

procedure TPlatformNotifications.CancelAll;
var
  LIterator: JIterator;
  LKeyObject: JObject;
begin
  LIterator := FNotificationStore.getAll.keySet.iterator;
  while LIterator.hasNext do
  begin
    LKeyObject := LIterator.next;
    if LKeyObject = nil then
      Continue;
    CancelNotification(JStringToString(LKeyObject.toString));
  end;
end;

procedure TPlatformNotifications.CancelNotification(const AName: string);
var
  LPendingIntent: JPendingIntent;
  LNotification: TNotification;
  LID: Integer;
begin
  if not AName.IsEmpty then
  begin
    LID := RetrieveNotification(AName, LNotification);
    if LID > -1 then
    begin
      LPendingIntent := GetNotificationPendingIntent(LNotification, LID);
      TAndroidHelper.AlarmManager.cancel(LPendingIntent);
      TAndroidHelperEx.NotificationManager.cancel(LID);
      RemoveNotification(LNotification);
    end;
  end;
end;

procedure TPlatformNotifications.PresentNotification(const ANotification: TNotification);
var
  LNotification: JNotification;
  LID: Integer;
begin
  LID := GetUniqueID;
  LNotification := GetNativeNotification(ANotification, LID);
  StoreNotification(ANotification, LID);
  NotificationManager.notify(LID, LNotification);
end;

procedure TPlatformNotifications.ScheduleNotification(const ANotification: TNotification);
var
  LTime: Int64;
  LPendingIntent: JPendingIntent;
  LID: Integer;
begin
  CancelNotification(ANotification.Name);
  LID := GetUniqueID;
  StoreNotification(ANotification, LID);
  LPendingIntent := GetNotificationPendingIntent(ANotification, LID);
  LTime := TAndroidHelperEx.GetTimeFromNowInMillis(SecondsBetween(Now, ANotification.FireDate));
  if TOSVersion.Check(6) then
    TAndroidHelper.AlarmManager.setExactAndAllowWhileIdle(TJAlarmManager.JavaClass.RTC_WAKEUP, LTime, LPendingIntent)
  else
    TAndroidHelper.AlarmManager.&set(TJAlarmManager.JavaClass.RTC_WAKEUP, LTime, LPendingIntent);
end;

procedure TPlatformNotifications.RemoveNotification(const ANotification: TNotification);
var
  LEditor: JSharedPreferences_Editor;
begin
  LEditor := FNotificationStore.edit;
  try
    LEditor.remove(StringToJString(ANotification.Name));
  finally
    LEditor.apply;
  end;
end;

function TPlatformNotifications.RetrieveNotification(const AName: string; var ANotification: TNotification): Integer;
begin
  Result := FNotificationStore.getInt(StringToJString(AName), -1);
end;

procedure TPlatformNotifications.StoreNotification(const ANotification: TNotification; const AID: Integer);
var
  LEditor: JSharedPreferences_Editor;
  LName: string;
begin
  LName := ANotification.Name;
  if LName.IsEmpty then
    LName := AID.ToString;
  LEditor := FNotificationStore.edit;
  try
    LEditor.putInt(StringToJString(LName), AID);
  finally
    LEditor.apply;
  end;
end;

end.
