unit DW.FCMManager.Android;

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

implementation

uses
  // RTL
  System.SysUtils, System.Notification, System.PushNotification, System.Messaging, System.Permissions, System.StrUtils,
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.Firebase,
  Androidapi.JNI.App, Androidapi.JNIBridge, Androidapi.JNI,
  // FMX
  FMX.PushNotification.Android,
  // DW
  DW.OSLog,
  DW.FCMManager, DW.PushServiceNotification.Android, DW.Android.Helpers, DW.Androidapi.JNI.NotificationListenerService, DW.OSMetadata,
  DW.Consts.Android, DW.Permissions.Helpers;

const
  cMetadataFCMDefaultChannelId = 'com.google.firebase.messaging.default_notification_channel_id';
  cMetadataFCMDefaultNotificationIcon = 'com.google.firebase.messaging.default_notification_icon';

type
  JDWFirebaseMessagingService = interface;
  JDWNotificationPresenter = interface;

  [JavaSignature('android/app/NotificationManager')]
  JNotificationManagerEx = interface(JNotificationManager)
    ['{4C69F78D-79A8-414D-90A6-F234AB2DD7DD}']
    function getActiveNotifications: TJavaObjectArray<JStatusBarNotification>; cdecl;
  end;
  TJNotificationManagerEx = class(TJavaGenericImport<JNotificationManagerClass, JNotificationManagerEx>) end;

  JDWNotificationPresenterClass = interface(JObjectClass)
    ['{F7D03A16-5C6D-47C4-A2C9-1B9A826A610C}']
    {class} procedure presentNotification(context: JContext; intent: JIntent; channelId: JString; defaultSmallIcon: Integer); cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWNotificationPresenter')]
  JDWNotificationPresenter = interface(JObject)
    ['{6977BA6C-67AA-4AD5-A0DB-1E7EE441771E}']
  end;
  TJDWNotificationPresenter = class(TJavaGenericImport<JDWNotificationPresenterClass, JDWNotificationPresenter>) end;

  [JavaSignature('com/delphiworlds/kastri/DWFirebaseMessagingServiceCallback')]
  JDWFirebaseMessagingServiceCallback = interface(IJavaInstance)
    ['{F2EA3740-307A-43E0-B85B-A9582E15D967}']
    function getShowNotificationWhenForeground: Boolean; cdecl;
    procedure onNotificationReceived(intent: JIntent); cdecl;
  end;

  JDWFirebaseMessagingServiceClass = interface(JObjectClass)
    ['{B4946E5D-2BA4-4165-B2DC-65948EB41B7D}']
    {class} procedure setCallback(callback: JDWFirebaseMessagingServiceCallback); cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWFirebaseMessagingService')]
  JDWFirebaseMessagingService = interface(JObject)
    ['{3218915F-8C29-41DC-AA4B-D80D723BD6F4}']
  end;
  TJDWFirebaseMessagingService = class(TJavaGenericImport<JDWFirebaseMessagingServiceClass, JDWFirebaseMessagingService>) end;

  TPlatformFCMManager = class;

  TDWFirebaseMessagingServiceCallback = class(TJavaLocal, JDWFirebaseMessagingServiceCallback)
  private
    FFCMManager: TPlatformFCMManager;
  public
    { JDWFirebaseMessagingServiceCallback }
    function getShowNotificationWhenForeground: Boolean; cdecl;
    procedure onNotificationReceived(intent: JIntent); cdecl;
  public
    constructor Create(const AFCMManager: TPlatformFCMManager);
  end;

  TPlatformFCMManager = class(TCustomPlatformFCMManager, IFCMManager)
  private
    FChannelId: string;
    FFirebaseMessagingServiceCallback: TDWFirebaseMessagingServiceCallback;
    FMessageIDs: TArray<string>;
    function IsAndroidPushEnabled: Boolean;
    procedure MessageReceivedNotificationHandler(const Sender: TObject; const AMsg: TMessage);
  protected
    function CanHandleNotification(const AServiceNotification: TPushServiceNotification): Boolean; override;
    procedure CreateChannel; override;
    procedure ReceiveNotification(const AServiceNotification: TPushServiceNotification); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod); override;
    procedure RemoveNotifications; override;
    procedure Start; override;
    procedure SubscribeToTopic(const ATopic: string); override;
    procedure UnsubscribeFromTopic(const ATopic: string); override;
  end;

{ TDWFirebaseMessagingServiceCallback }

constructor TDWFirebaseMessagingServiceCallback.Create(const AFCMManager: TPlatformFCMManager);
begin
  inherited Create;
  FFCMManager := AFCMManager;
end;

function TDWFirebaseMessagingServiceCallback.getShowNotificationWhenForeground: Boolean;
begin
  Result := FFCMManager.ShowBannerIfForeground;
end;

procedure TDWFirebaseMessagingServiceCallback.onNotificationReceived(intent: JIntent);
var
  LExtras: JBundle;
  LNotification: TPushServiceNotification;
begin
  LExtras := intent.getExtras;
  if LExtras <> nil then
  begin
    LNotification := TAndroidPushServiceNotification.Create(LExtras);
    try
      FFCMManager.HandleNotification(LNotification);
    finally
      LNotification.Free;
    end;
  end;
end;

{ TPlatformFCMManager }

constructor TPlatformFCMManager.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  TOSMetadata.GetValue(cMetadataFCMDefaultChannelId, FChannelId);
  FFirebaseMessagingServiceCallback := TDWFirebaseMessagingServiceCallback.Create(Self);
  TJDWFirebaseMessagingService.JavaClass.setCallback(FFirebaseMessagingServiceCallback);
end;

destructor TPlatformFCMManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  FFirebaseMessagingServiceCallback.Free;
  inherited;
end;

procedure TPlatformFCMManager.CreateChannel;
var
  LNotificationCenter: TNotificationCenter;
  LChannel: TChannel;
begin
  if not FChannelId.IsEmpty then
  begin
    LNotificationCenter := TNotificationCenter.Create(nil);
    try
      LChannel := TChannel.Create;
      try
        LChannel.Id := FChannelId;
        LChannel.Title := FChannelId + ' FCM';
        LChannel.Description := '';
        // Required for appearing as a banner when the app is not running, or when in the foreground
        LChannel.Importance := TImportance.High;
        LNotificationCenter.CreateOrUpdateChannel(LChannel);
        // TOSLog.d('Channel created: %s', [FChannelId]);
      finally
        LChannel.Free;
      end;
    finally
      LNotificationCenter.Free;
    end;
  end;
end;

procedure TPlatformFCMManager.MessageReceivedNotificationHandler(const Sender: TObject; const AMsg: TMessage);
var
  LIcon: Integer;
  LIntent: JIntent;
begin
  LIntent := TMessageReceivedNotification(AMsg).Value;
  if ShowBannerIfForeground and IsForeground and LIntent.hasExtra(StringToJString('S.google.message_id')) then
  begin
    TOSMetadata.GetValue(cMetadataFCMDefaultNotificationIcon, LIcon);
    TJDWNotificationPresenter.JavaClass.presentNotification(TAndroidHelper.Context, LIntent, StringToJString(FChannelId), LIcon);
  end;
end;

function TPlatformFCMManager.IsAndroidPushEnabled: Boolean;
var
  LService: JObject;
  LNotificationManager: JNotificationManager;
  LChannels: JList;
  LChannel: JNotificationChannel;
  I: Integer;
begin
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
  LNotificationManager := TJNotificationManager.Wrap(TAndroidHelper.JObjectToID(LService));
  Result := LNotificationManager.areNotificationsEnabled;
  if Result and TAndroidHelperEx.CheckBuildAndTarget(26) then
  begin
    LChannels := LNotificationManager.getNotificationChannels;
    for I := 0 to LChannels.size - 1 do
    begin
      LChannel := TJNotificationChannel.Wrap(LChannels.get(I));
      if LChannel.getId.equals(StringToJString(FChannelId)) and (LChannel.getImportance = TJNotificationManager.JavaClass.IMPORTANCE_NONE) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TPlatformFCMManager.CanHandleNotification(const AServiceNotification: TPushServiceNotification): Boolean;
var
  LMessageID: string;
begin
  if AServiceNotification.Json <> nil then
  begin
    Result := False;
    if AServiceNotification.Json.TryGetValue('["google.message_id"]', LMessageID) then
    begin
      Result := not MatchText(LMessageID, FMessageIDs);
      FMessageIDs := FMessageIDs + [LMessageID];
    end;
  end
  else
    Result := inherited;
end;

procedure TPlatformFCMManager.CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
begin
  AHandler(IsAndroidPushEnabled);
end;

procedure TPlatformFCMManager.ReceiveNotification(const AServiceNotification: TPushServiceNotification);
var
  LAction: string;
  LCategory: INotificationCategory;
begin
  if AServiceNotification.Json.TryGetValue('click_action', LAction) and FindCategory(LAction, LCategory) then
    LCategory.Handler();
end;

procedure TPlatformFCMManager.RemoveNotifications;
var
  LNotifications: TJavaObjectArray<JStatusBarNotification>;
  I: Integer;
begin
  if TAndroidHelperEx.CheckBuildAndTarget(23) then
  begin
    LNotifications := TJNotificationManagerEx.Wrap(TAndroidHelperEx.NotificationManager).getActiveNotifications;
    if LNotifications <> nil then
    try
      for I := 0 to LNotifications.Length - 1 do
        TAndroidHelperEx.NotificationManager.cancel(LNotifications.Items[I].getId);
    finally
      LNotifications.Free;
    end;
  end
  else
    TAndroidHelperEx.NotificationManager.cancelAll;
end;

procedure TPlatformFCMManager.Start;
begin
  if TAndroidHelperEx.CheckBuildAndTarget(33) then
  begin
    PermissionsService.RequestPermissions([cPermissionPostNotifications],
      procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
      begin
        if AGrantResults.AreAllGranted then
          DoStart
        else
          TOSLog.d('> Permissions denied');
      end
    );
  end
  else
    DoStart;
end;

procedure TPlatformFCMManager.SubscribeToTopic(const ATopic: string);
begin
  TJFirebaseMessaging.JavaClass.getInstance.subscribeToTopic(StringToJString(ATopic));
end;

procedure TPlatformFCMManager.UnsubscribeFromTopic(const ATopic: string);
begin
  TJFirebaseMessaging.JavaClass.getInstance.unsubscribeFromTopic(StringToJString(ATopic));
end;

initialization
  FCM := TPlatformFCMManager.Create;

end.
