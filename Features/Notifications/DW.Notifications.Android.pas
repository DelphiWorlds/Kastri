unit DW.Notifications.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Messaging,
  // Android
  Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  {$IF CompilerVersion > 32}
  Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNI.Media, Androidapi.JNI.Widget,
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

  TPlatformNotifications = class;

  TNotificationReceiver = class(TMultiReceiver)
  private
    FNotifications: TPlatformNotifications;
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    constructor Create(const ANotifications: TPlatformNotifications);
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
    FIntent: JIntent;
    FIsLaunching: Boolean;
    FNotificationChannel: JNotificationChannel;
    FNotificationReceiver: TNotificationReceiver;
    FNotificationStore: JSharedPreferences;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure CheckNotificationAction;
    function CreateRemoteViews(const ANotification: TNotification; const AID: Integer): JRemoteViews;
    function GetNativeNotification(const ANotification: TNotification; const AID: Integer; const AScheduled: Boolean): JNotification;
    function GetNotificationPendingIntent(const ANotification: TNotification; const AID: Integer): JPendingIntent;
    function GetUniqueID: Integer;
    function GetNotificationIntent(const ANotification: TNotification; const AID: Integer): JPendingIntent;
    procedure MessageReceivedNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure RemoveNotification(const ANotification: TNotification);
    function RetrieveNotification(const AName: string; var ANotification: TNotification): Integer;
    procedure SetRemoteViewsImage(const ARemoteViews: JRemoteViews; const AResourceName: string; const ABitmap: JBitmap);
    procedure SetRemoteViewsText(const ARemoteViews: JRemoteViews; const AResourceName, AValue: string);
    procedure StoreNotification(const ANotification: TNotification; const AID: Integer);
  protected
    class property NotificationManager: JNotificationManager read GetNotificationManager;
  protected
    procedure CancelAll; override;
    procedure CancelNotification(const AName: string); override;
    procedure DoNotificationReceived(const ANotification: TNotification); override;
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
  Androidapi.Helpers,
  // FMX
  FMX.Platform, FMX.Platform.Android,
  // DW
  DW.Consts.Android,
  DW.Androidapi.JNI.DWMultiBroadcastReceiver, DW.Android.Helpers;

type
  TNotificationActionResponseHelper = record helper for TNotificationActionResponse
    constructor Create(const AIntent: JIntent);
  end;

  TNotificationActionHelper = record helper for TNotificationAction
    function GetValue: string;
  end;

  TNotificationActionsHelper = record helper for TNotificationActions
    function GetExtraValue: JArrayList;
  end;

function GetResourceID(const AResourceName, AResourceType: string; const APackageName: string = ''): Integer; overload;
var
  LContext: JContext;
  LPackageName, LResourceType: JString;
begin
  LContext := TAndroidHelper.Context;
  if APackageName.IsEmpty then
    LPackageName := LContext.getPackageName
  else
    LPackageName := StringToJString(APackageName);
  if AResourceType.IsEmpty then
    LResourceType := nil
  else
    LResourceType := StringToJString(AResourceType);
  Result := LContext.getResources.getIdentifier(StringToJString(AResourceName), LResourceType, LPackageName);
end;

function GetAndroidResourceID(const AResourceName, AResourceType: string): Integer;
begin
  Result := GetResourceID(AResourceName, AResourceType, 'android');
end;

{ TNotificationActionResponseHelper }

constructor TNotificationActionResponseHelper.Create(const AIntent: JIntent);
begin
  NotificationID := AIntent.getIntExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_ID, 0);
  NotificationName := JStringToString(AIntent.getStringExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_NAME));
  ActionName := JStringToString(AIntent.getStringExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_ACTION_NAME));
end;

{ TNotificationActionHelper }

function TNotificationActionHelper.GetValue: string;
begin
  Result := ActionName + '=' + Text;
end;

{ TNotificationActionsHelper }

function TNotificationActionsHelper.GetExtraValue: JArrayList;
var
  LAction: TNotificationAction;
begin
  Result := TJArrayList.Create;
  for LAction in Self do
    Result.add(StringToJString(LAction.GetValue));
end;

{ TNotificationReceiver }

constructor TNotificationReceiver.Create(const ANotifications: TPlatformNotifications);
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
  FNotifications.DoNotificationReceived(LNotification);
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
  // Intent from launching the app from NOT RUNNING state
  FIntent := TAndroidHelper.Activity.getIntent;
  MainActivity.registerIntentAction(TJDWMultiBroadcastReceiver.JavaClass.ACTION_NOTIFICATION_ACTION);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationMessageHandler);
  FNotificationStore := TAndroidHelper.Context.getSharedPreferences(StringToJString(ClassName), TJContext.JavaClass.MODE_PRIVATE);
  if TAndroidHelperEx.CheckBuildAndTarget(TAndroidHelperEx.OREO) then
  begin
    FNotificationChannel := TJNotificationChannel.JavaClass.init(TAndroidHelper.Context.getPackageName, StrToJCharSequence('default'), 4);
    FNotificationChannel.enableLights(True);
    FNotificationChannel.enableVibration(True);
    FNotificationChannel.setLightColor(TJColor.JavaClass.GREEN);
    FNotificationChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
    FNotificationChannel.setImportance(TJNotificationManager.JavaClass.IMPORTANCE_HIGH);
    NotificationManager.createNotificationChannel(FNotificationChannel);
  end;
  FNotificationReceiver := TNotificationReceiver.Create(Self);
end;

destructor TPlatformNotifications.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedNotificationMessageHandler);
  FNotificationStore := nil;
  FNotificationChannel := nil;
  FNotificationReceiver.Free;
  inherited;
end;

procedure TPlatformNotifications.DoNotificationReceived(const ANotification: TNotification);
begin
  inherited;
end;

class function TPlatformNotifications.GetNotificationManager: JNotificationManager;
begin
  if FNotificationManager = nil then
    FNotificationManager := TJNotificationManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE));
  Result := FNotificationManager;
end;

procedure TPlatformNotifications.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.WillBecomeForeground:
      // App is being launched, or switched to from another app
      FIsLaunching := True;
    TApplicationEvent.BecameActive:
    begin
      if FIsLaunching then
      begin
        FIsLaunching := False;
        CheckNotificationAction;
      end;
      FIntent := nil;
    end;
  end;
end;

procedure TPlatformNotifications.MessageReceivedNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  // Intent from launching the app from RUNNING state
  FIntent := TMessageReceivedNotification(AMsg).Value;
  CheckNotificationAction;
end;

procedure TPlatformNotifications.CheckNotificationAction;
var
  LResponse: TNotificationActionResponse;
begin
  if (FIntent <> nil) and (FIntent.getAction <> nil) and FIntent.getAction.equals(TJDWMultiBroadcastReceiver.JavaClass.ACTION_NOTIFICATION_ACTION) then
  begin
    LResponse := TNotificationActionResponse.Create(FIntent);
    if LResponse.NotificationName.IsEmpty then
      TAndroidHelperEx.NotificationManager.cancel(LResponse.NotificationID)
    else
      CancelNotification(LResponse.NotificationName);
    DoNotificationActionResponse(LResponse);
    FIntent := nil;
  end;
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
  if ANotification.IsInsistent then
    LIntent.putExtra(StringToJString('isInsistent'), StringToJString('1'));
  if not ANotification.ResourceName.IsEmpty then
    LIntent.putExtra(StringToJString('resource_name'), StringToJString(ANotification.ResourceName));
  if Length(ANotification.Actions) > 0 then
    LIntent.putStringArrayListExtra(StringToJString('notification_actions'), ANotification.Actions.GetExtraValue);
  LFlags := TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT or TJPendingIntent.JavaClass.FLAG_IMMUTABLE;
  Result := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, AID, LIntent, LFlags);
end;

procedure TPlatformNotifications.SetRemoteViewsImage(const ARemoteViews: JRemoteViews; const AResourceName: string; const ABitmap: JBitmap);
var
  LResId: Integer;
begin
  LResId := GetResourceID(AResourceName, 'id');
  if LResId <> 0 then
    ARemoteViews.setImageViewBitmap(LResId, ABitmap);
end;

procedure TPlatformNotifications.SetRemoteViewsText(const ARemoteViews: JRemoteViews; const AResourceName, AValue: string);
var
  LResId: Integer;
begin
  LResId := GetResourceID(AResourceName, 'id');
  if LResId <> 0 then
    ARemoteViews.setTextViewText(LResId, StrToJCharSequence(AValue));
end;

function TPlatformNotifications.CreateRemoteViews(const ANotification: TNotification; const AID: Integer): JRemoteViews;
var
  LBitmap: JBitmap;
  LResourceName: string;
  LResId: Integer;
begin
  Result := nil;
  if TFile.Exists(ANotification.Image) then
    LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(ANotification.Image))
  else
    LBitmap := nil;
  if ANotification.ResourceName.IsEmpty then
    LResourceName := 'notification_custom'
  else
    LResourceName := ANotification.ResourceName;
  LResId := GetResourceID(LResourceName, 'layout');
  if LResId <> 0 then
  begin
    Result := TJRemoteViews.JavaClass.init(TAndroidHelper.Context.getPackageName, LResId);
    SetRemoteViewsText(Result, 'id/' + LResourceName + '_title', ANotification.Title);
    SetRemoteViewsText(Result, 'id/' + LResourceName + '_body', ANotification.AlertBody);
    if LBitmap <> nil then
      SetRemoteViewsImage(Result, 'id/' + LResourceName + '_image', LBitmap);
  end;
end;

function TPlatformNotifications.GetNativeNotification(const ANotification: TNotification; const AID: Integer; const AScheduled: Boolean): JNotification;
var
  LBuilder: JNotificationCompat_Builder;
  LCompactViews: JRemoteViews;
  LBitmap, LNilBitmap: JBitmap;
  LBigTextStyle: JNotificationCompat_BigTextStyle;
  LActionResId: Integer;
  LAction: TNotificationAction;
  LIntent: JIntent;
  LPendingIntent: JPendingIntent;
  LFlags: Integer;
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
  if not AScheduled and (TFile.Exists(ANotification.Image) or not ANotification.ResourceName.IsEmpty) then
  begin
    LBitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(ANotification.Image));
    LNilBitmap := nil;
    LCompactViews := CreateRemoteViews(ANotification, AID);
    LBuilder := LBuilder.setCustomContentView(LCompactViews)
      .setStyle(TJNotificationCompat_BigPictureStyle.JavaClass.init
        .bigPicture(LBitmap)
        .bigLargeIcon(LNilBitmap) // Hides compact thumbnail in expanded mode
        .setBigContentTitle(StrToJCharSequence(ANotification.Title))
        .setSummaryText(StrToJCharSequence(ANotification.AlertBody))
      );
  end
  else if ANotification.UseBigText then
  begin
    LBigTextStyle := TJNotificationCompat_BigTextStyle.JavaClass.init
	    .bigText(StrToJCharSequence(ANotification.AlertBody))
      .setBigContentTitle(StrToJCharSequence(ANotification.Title));
    LBuilder.setStyle(LBigTextStyle);
  end;
  for LAction in ANotification.Actions do
  begin
    LActionResId := GetAndroidResourceID('btn_default', 'drawable');
    if LActionResId <> 0 then
    begin
      LIntent := TJIntent.JavaClass.init(TJDWMultiBroadcastReceiver.JavaClass.ACTION_NOTIFICATION_ACTION);
      LIntent.setClassName(TAndroidHelper.Context, StringToJString(cEMBTFMXNativeActivity));
      LFlags := TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK or TJIntent.JavaClass.FLAG_ACTIVITY_CLEAR_TOP or TJIntent.JavaClass.FLAG_ACTIVITY_SINGLE_TOP;
      LIntent.setFlags(LFlags);
      LIntent.putExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_ID, AID);
      LIntent.putExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_NAME, StringToJString(ANotification.Name));
      LIntent.putExtra(TJDWMultiBroadcastReceiver.JavaClass.EXTRA_NOTIFICATION_ACTION_NAME, StringToJString(LAction.ActionName));
      LFlags := TJPendingIntent.JavaClass.FLAG_UPDATE_CURRENT or TJPendingIntent.JavaClass.FLAG_IMMUTABLE;
      LPendingIntent := TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, GetUniqueID, LIntent, LFlags);
      LBuilder.addAction(LActionResId, StrToJCharSequence(LAction.Text), LPendingIntent);
    end;
  end;
  Result := LBuilder.setPriority(TJNotificationCompat.JavaClass.PRIORITY_HIGH).build;
  if ANotification.IsInsistent then
    Result.flags := Result.flags or TJNotification.JavaClass.FLAG_INSISTENT;
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
