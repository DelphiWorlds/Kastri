unit DW.Notifications.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  {$IF CompilerVersion > 32}
  Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNI.Media,
  {$ENDIF}
  // DW
  DW.Notifications, DW.MultiReceiver.Android, DW.Androidapi.JNI.App,
  {$IF CompilerVersion < 35} DW.Androidapi.JNI.SupportV4; {$ELSE} DW.Androidapi.JNI.AndroidX.App; {$ENDIF}

type
  {$IF CompilerVersion > 32}
  JNotificationChannel = interface;
  JNotificationChannelGroup = interface;
  JNotificationManager = interface;

  JNotificationChannelClass = interface(JObjectClass)
    ['{2C435911-AD3A-4461-8860-779ECA70D332}']
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function _GetDEFAULT_CHANNEL_ID: JString; cdecl;
    function init(id: JString; name: JCharSequence; importance: Integer): JNotificationChannel; cdecl;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
    property DEFAULT_CHANNEL_ID: JString read _GetDEFAULT_CHANNEL_ID;
  end;

  [JavaSignature('android/app/NotificationChannel')]
  JNotificationChannel = interface(JObject)
    ['{3AA9585E-BF01-40A2-933E-73DFEB241BB9}']
    function canBypassDnd: boolean; cdecl;
    function canShowBadge: boolean; cdecl;
    function describeContents: Integer; cdecl;
    procedure enableLights(lights: boolean); cdecl;
    procedure enableVibration(vibration: boolean); cdecl;
    function equals(o: JObject): boolean; cdecl;
    function getAudioAttributes: JAudioAttributes; cdecl;
    function getDescription: JString; cdecl;
    function getGroup: JString; cdecl;
    function getId: JString; cdecl;
    function getImportance: Integer; cdecl;
    function getLightColor: Integer; cdecl;
    function getLockscreenVisibility: Integer; cdecl;
    function getName: JCharSequence; cdecl;
    function getSound: Jnet_Uri; cdecl;
    function getVibrationPattern: TJavaArray<Int64>; cdecl;
    function hashCode: Integer; cdecl;
    procedure setBypassDnd(bypassDnd: boolean); cdecl;
    procedure setDescription(description: JString); cdecl;
    procedure setGroup(groupId: JString); cdecl;
    procedure setImportance(importance: Integer); cdecl;
    procedure setLightColor(argb: Integer); cdecl;
    procedure setLockscreenVisibility(lockscreenVisibility: Integer); cdecl;
    procedure setName(&name: JCharSequence); cdecl;
    procedure setShowBadge(showBadge: boolean); cdecl;
    procedure setSound(sound: Jnet_Uri; audioAttributes: JAudioAttributes); cdecl;
    procedure setVibrationPattern(vibrationPattern: TJavaArray<Int64>); cdecl;
    function shouldShowLights: boolean; cdecl;
    function shouldVibrate: boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJNotificationChannel = class(TJavaGenericImport<JNotificationChannelClass, JNotificationChannel>)
  end;

  JNotificationChannelGroupClass = interface(JObjectClass)
    ['{E62DA0EE-18A8-439E-BE8D-A9D0A89A2EF9}']
    function _GetCREATOR : JParcelable_Creator; cdecl;
    function init(id : JString; &name : JCharSequence) : JNotificationChannelGroup; cdecl;
    property CREATOR : JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/NotificationChannelGroup')]
  JNotificationChannelGroup = interface(JObject)
    ['{CA4F183A-9CD3-4261-805A-A0CCBC9A5389}']
    function clone: JNotificationChannelGroup; cdecl;
    function describeContents: Integer; cdecl;
    function equals(o: JObject): boolean; cdecl;
    function getChannels: JList; cdecl;
    function getDescription: JString; cdecl;
    function getId: JString; cdecl;
    function getName: JCharSequence; cdecl;
    function hashCode: Integer; cdecl;
    function isBlocked: Boolean; cdecl;
    procedure setDescription(description: JString); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJNotificationChannelGroup = class(TJavaGenericImport<JNotificationChannelGroupClass, JNotificationChannelGroup>)
  end;

  JNotificationManagerClass = interface(JObjectClass)
    ['{66101C50-DAE9-4C81-8186-81A0A43A73BD}']
    {class} function _GetACTION_INTERRUPTION_FILTER_CHANGED: JString; cdecl;
    {class} function _GetACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED: JString; cdecl;
    {class} function _GetACTION_NOTIFICATION_POLICY_CHANGED: JString; cdecl;
    {class} function _GetIMPORTANCE_DEFAULT: Integer; cdecl;
    {class} function _GetIMPORTANCE_HIGH: Integer; cdecl;
    {class} function _GetIMPORTANCE_LOW: Integer; cdecl;
    {class} function _GetIMPORTANCE_MAX: Integer; cdecl;
    {class} function _GetIMPORTANCE_MIN: Integer; cdecl;
    {class} function _GetIMPORTANCE_NONE: Integer; cdecl;
    {class} function _GetIMPORTANCE_UNSPECIFIED: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_ALARMS: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_ALL: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_NONE: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_PRIORITY: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_UNKNOWN: Integer; cdecl;
    {class} property ACTION_INTERRUPTION_FILTER_CHANGED: JString read _GetACTION_INTERRUPTION_FILTER_CHANGED;
    {class} property ACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED: JString read _GetACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED;
    {class} property ACTION_NOTIFICATION_POLICY_CHANGED: JString read _GetACTION_NOTIFICATION_POLICY_CHANGED;
    {class} property IMPORTANCE_DEFAULT: Integer read _GetIMPORTANCE_DEFAULT;
    {class} property IMPORTANCE_HIGH: Integer read _GetIMPORTANCE_HIGH;
    {class} property IMPORTANCE_LOW: Integer read _GetIMPORTANCE_LOW;
    {class} property IMPORTANCE_MAX: Integer read _GetIMPORTANCE_MAX;
    {class} property IMPORTANCE_MIN: Integer read _GetIMPORTANCE_MIN;
    {class} property IMPORTANCE_NONE: Integer read _GetIMPORTANCE_NONE;
    {class} property IMPORTANCE_UNSPECIFIED: Integer read _GetIMPORTANCE_UNSPECIFIED;
    {class} property INTERRUPTION_FILTER_ALARMS: Integer read _GetINTERRUPTION_FILTER_ALARMS;
    {class} property INTERRUPTION_FILTER_ALL: Integer read _GetINTERRUPTION_FILTER_ALL;
    {class} property INTERRUPTION_FILTER_NONE: Integer read _GetINTERRUPTION_FILTER_NONE;
    {class} property INTERRUPTION_FILTER_PRIORITY: Integer read _GetINTERRUPTION_FILTER_PRIORITY;
    {class} property INTERRUPTION_FILTER_UNKNOWN: Integer read _GetINTERRUPTION_FILTER_UNKNOWN;
  end;

  [JavaSignature('android/app/NotificationManager')]
  JNotificationManager = interface(Androidapi.JNI.App.JNotificationManager)
    ['{F2C96815-29C4-4A83-994A-4F49F30B8CF4}']
    procedure createNotificationChannel(channel: JNotificationChannel); cdecl;
    procedure createNotificationChannelGroup(group: JNotificationChannelGroup); cdecl;
    procedure createNotificationChannelGroups(groups: JList); cdecl;
    procedure createNotificationChannels(channels: JList); cdecl;
    procedure deleteNotificationChannel(channelId: JString); cdecl;
    procedure deleteNotificationChannelGroup(groupId: JString); cdecl;
  end;
  TJNotificationManager = class(TJavaGenericImport<JNotificationManagerClass, JNotificationManager>)
  end;
  {$ENDIF}

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
    function GetNativeNotification(const ANotification: TNotification; const AID: Integer; const AScheduled: Boolean): JNotification;
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
  System.SysUtils, System.DateUtils, System.TimeSpan, System.IOUtils,
  // Android
  {$IF CompilerVersion < 33}
  Androidapi.JNI.Net, Androidapi.JNI.Os,
  {$ENDIF}
  Androidapi.Helpers, Androidapi.JNI.Embarcadero, Androidapi.JNI.Widget,
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
  LNotification.Image := JStringToString(LNativeNotification.extras.getString(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_IMAGE));
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
begin
  if FNotificationManager = nil then
    FNotificationManager := TJNotificationManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE));
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
  LFlags: Integer;
begin
  LIntent := TAndroidHelper.Context.getPackageManager().getLaunchIntentForPackage(TAndroidHelper.Context.getPackageName());
  LIntent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_SINGLE_TOP or TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_TOP);
  LFlags := TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT or TJPendingIntent.JavaClass.FLAG_IMMUTABLE;
  Result := TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, AID, LIntent, LFlags);
end;

function TPlatformNotifications.GetNotificationPendingIntent(const ANotification: TNotification; const AID: Integer): JPendingIntent;
var
  LIntent: JIntent;
  LNotification: JNotification;
  LFlags: Integer;
begin
  LNotification := GetNativeNotification(ANotification, AID, True);
  LNotification.extras.putString(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_NAME, StringToJString(ANotification.Name));
  LNotification.extras.putInt(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_REPEATINTERVAL, Integer(Ord(ANotification.RepeatInterval)));
  LIntent := TJIntent.Create;
  LIntent.setClass(TAndroidHelper.Context, TJDWMultiBroadcastReceiver.getClass);
  LIntent.setAction(TJDWMultiBroadcastReceiver.JavaClass.ACTION_NOTIFICATION);
  LIntent.putExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_ID, AID);
  LIntent.putExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_IMAGE, StringToJString(ANotification.Image));
  LIntent.putExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION, TJParcelable.Wrap(TAndroidHelper.JObjectToID(LNotification)));
  LFlags := TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT or TJPendingIntent.JavaClass.FLAG_IMMUTABLE;
  Result := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, AID, LIntent, LFlags);
end;

function TPlatformNotifications.GetNativeNotification(const ANotification: TNotification; const AID: Integer; const AScheduled: Boolean): JNotification;
var
  LBuilder: JNotificationCompat_Builder;
  LContentSmall, LContentBig: JRemoteViews;
  LBitmap: JBitmap;
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
  if not AScheduled and TFile.Exists(ANotification.Image) then
  begin
    LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(ANotification.Image));
    LContentSmall := TJRemoteViews.JavaClass.init(TAndroidHelper.Context.getPackageName, TAndroidHelper.GetResourceID('layout/notification_custom'));
    LContentSmall.setTextViewText(TAndroidHelper.GetResourceID('id/notification_custom_title'), StrToJCharSequence(ANotification.Title));
    LContentSmall.setTextViewText(TAndroidHelper.GetResourceID('id/notification_custom_body'), StrToJCharSequence(ANotification.AlertBody));
    LContentSmall.setImageViewBitmap(TAndroidHelper.GetResourceID('id/notification_custom_image'), LBitmap);
    LContentBig := TJRemoteViews.JavaClass.init(TAndroidHelper.Context.getPackageName, TAndroidHelper.GetResourceID('layout/notification_custom_big'));
    LContentBig.setTextViewText(TAndroidHelper.GetResourceID('id/notification_custom_title'), StrToJCharSequence(ANotification.Title));
    LContentBig.setTextViewText(TAndroidHelper.GetResourceID('id/notification_custom_body'), StrToJCharSequence(ANotification.AlertBody));
    LContentBig.setImageViewBitmap(TAndroidHelper.GetResourceID('id/notification_custom_image'), LBitmap);
    LBuilder := LBuilder.setCustomContentView(LContentSmall)
      .setCustomBigContentView(LContentBig);
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
  LNotification := GetNativeNotification(ANotification, LID, False);
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
