unit DW.FCMManager;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.PushNotification, System.Json, System.Classes;

type
  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TPushServiceNotification) of object;
  TMessageReceivedEvent = procedure(Sender: TObject; const JSON: TJSONObject) of object;
  TCheckPushEnabledMethod = reference to procedure(const Enabled: Boolean);

  IFCMManager = interface(IInterface)
    ['{961FE28D-76AA-466D-AA0D-3086FBE4D678}']
    /// <summary>
    ///   Checks if Push Notifications are enabled for the application
    /// </summary>
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
    function GetOnMessageReceived: TMessageReceivedEvent;
    function GetOnStatusChange: TNotifyEvent;
    function GetPushService: TPushService;
    function GetShowBannerIfForeground: Boolean;
    /// <summary>
    ///   Returns the FCM token
    /// </summary>
    function GetToken: string;
    /// <summary>
    ///   Indicates whether or not the push service has been started
    /// </summary>
    function IsStarted: Boolean;
    procedure SetOnMessageReceived(const AValue: TMessageReceivedEvent);
    procedure SetOnStatusChange(const AValue: TNotifyEvent);
    procedure SetShowBannerIfForeground(const AValue: Boolean);
    /// <summary>
    ///   Creates the push service
    /// </summary>
    procedure Start;
    /// <summary>
    ///   Subscribes the app to the specified topic. All messages with the specified topic will be sent to this app
    /// </summary>
    procedure SubscribeToTopic(const ATopic: string);
    /// <summary>
    ///   Unsubscribes the app from the specified topic
    /// </summary>
    procedure UnsubscribeFromTopic(const ATopic: string);
    /// <summary>
    ///   Provides access to the underlying push service
    /// </summary>
    property PushService: TPushService read GetPushService;
    /// <summary>
    ///   Indicates whether or not a notification banner should be presented when the app is in the foreground
    /// </summary>
    property ShowBannerIfForeground: Boolean read GetShowBannerIfForeground write SetShowBannerIfForeground;
    /// <summary>
    ///   This event is fired when the push service receives a message
    /// </summary>
    property OnMessageReceived: TMessageReceivedEvent read GetOnMessageReceived write SetOnMessageReceived;
    /// <summary>
    ///   This event is fired when the status of the push service changes
    /// </summary>
    property OnStatusChange: TNotifyEvent read GetOnStatusChange write SetOnStatusChange;
  end;

var
  FCM: IFCMManager;

implementation

uses
  // RTL
  System.SysUtils, System.Notification, System.Messaging, System.IOUtils, System.Permissions,
  {$IF Defined(ANDROID)}
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.Firebase,
  Androidapi.JNI.App, Androidapi.JNIBridge,
  // FMX
  FMX.PushNotification.Android,
  // DW
  DW.PushServiceNotification.Android, DW.Android.Helpers,
  {$ENDIF}
  {$IF Defined(IOS)}
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.FirebaseMessaging, iOSapi.UserNotifications,
  // FMX
  FMX.PushNotification.FCM.iOS,
  {$ENDIF}
  FMX.Platform,
  {$IF Defined(IOS)}
  // DW
  DW.UserDefaults.iOS,
  {$ENDIF}
  DW.OSLog,
  DW.OSMetadata, DW.Consts.Android, DW.Permissions.Helpers;

type
  {$IF Defined(ANDROID)}
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

  TFCMManager = class;

  TDWFirebaseMessagingServiceCallback = class(TJavaLocal, JDWFirebaseMessagingServiceCallback)
  private
    FFCMManager: TFCMManager;
  public
    { JDWFirebaseMessagingServiceCallback }
    procedure onNotificationReceived(intent: JIntent); cdecl;
  public
    constructor Create(const AFCMManager: TFCMManager);
  end;
  {$ENDIF}

  TFCMManager = class(TInterfacedObject, IFCMManager)
  private
    FChannelId: string;
    {$IF Defined(IOS)}
    FCheckPushEnabledHandler: TCheckPushEnabledMethod;
    {$ENDIF}
    FDeviceID: string;
    FDeviceToken: string;
    {$IF Defined(ANDROID)}
    FFirebaseMessagingServiceCallback: TDWFirebaseMessagingServiceCallback;
    {$ENDIF}
    FIsForeground: Boolean;
    FServiceConnection: TPushServiceConnection;
    FShowBannerIfForeground: Boolean;
    FOnMessageReceived: TMessageReceivedEvent;
    FOnNotificationReceived: TNotificationReceivedEvent;
    FOnStatusChange: TNotifyEvent;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    {$IF Defined(IOS)}
    procedure CheckPushEnabledCompletionHandler(settings: UNNotificationSettings);
    {$ENDIF}
    procedure CheckStartupNotifications;
    procedure CreateChannel;
    procedure CreateConnection;
    procedure DoStart;
    {$IF Defined(ANDROID)}
    function IsAndroidPushEnabled: Boolean;
    procedure MessageReceivedNotificationHandler(const Sender: TObject; const AMsg: TMessage);
    {$ENDIF}
    procedure PushDeviceTokenMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
    procedure ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
  protected
    procedure HandleNotification(const AServiceNotification: TPushServiceNotification);
  public
    { IFCMManager }
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
    function GetOnMessageReceived: TMessageReceivedEvent;
    function GetOnStatusChange: TNotifyEvent;
    function GetPushService: TPushService;
    function GetShowBannerIfForeground: Boolean;
    function GetToken: string;
    function IsStarted: Boolean;
    procedure SetOnMessageReceived(const AValue: TMessageReceivedEvent);
    procedure SetOnStatusChange(const AValue: TNotifyEvent);
    procedure SetShowBannerIfForeground(const AValue: Boolean);
    procedure Start;
    procedure SubscribeToTopic(const ATopic: string);
    procedure UnsubscribeFromTopic(const ATopic: string);
  public
    constructor Create;
    destructor Destroy; override;
    property DeviceID: string read FDeviceID;
    property DeviceToken: string read FDeviceToken;
  end;

const
  cMetadataFCMDefaultChannelId = 'com.google.firebase.messaging.default_notification_channel_id';
  cMetadataFCMDefaultNotificationIcon = 'com.google.firebase.messaging.default_notification_icon';

  UNAuthorizationStatusEphemeral = 4;

{$IF Defined(ANDROID)}
{ TDWFirebaseMessagingServiceCallback }

constructor TDWFirebaseMessagingServiceCallback.Create(const AFCMManager: TFCMManager);
begin
  inherited Create;
  FFCMManager := AFCMManager;
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
{$ENDIF}

{ TFCMManager }

constructor TFCMManager.Create;
begin
  inherited Create;
  TOSMetadata.GetValue(cMetadataFCMDefaultChannelId, FChannelId);
  FShowBannerIfForeground := True;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  {$IF Defined(ANDROID)}
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  FFirebaseMessagingServiceCallback := TDWFirebaseMessagingServiceCallback.Create(Self);
  TJDWFirebaseMessagingService.JavaClass.setCallback(FFirebaseMessagingServiceCallback);
  {$ENDIF}
end;

destructor TFCMManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  {$IF Defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  {$ENDIF}
  FServiceConnection.Free;
  inherited;
end;

procedure TFCMManager.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
      FIsForeground := True;
    TApplicationEvent.EnteredBackground:
      FIsForeground := False;
  end;
end;

procedure TFCMManager.CreateConnection;
var
  LPushService: TPushService;
begin
  LPushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.FCM);
  if LPushService <> nil then
  begin
    FServiceConnection := TPushServiceConnection.Create(LPushService);
    FServiceConnection.OnChange := ServiceConnectionChangeHandler;
    FServiceConnection.OnReceiveNotification := ReceiveNotificationHandler;
    FDeviceId := LPushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceId];
    FServiceConnection.Active := True;
  end;
end;

procedure TFCMManager.CreateChannel;
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
      finally
        LChannel.Free;
      end;
    finally
      LNotificationCenter.Free;
    end;
  end;
end;

{$IF Defined(IOS)}
procedure TFCMManager.CheckPushEnabledCompletionHandler(settings: UNNotificationSettings);
var
  LIsPushEnabled: Boolean;
begin
  LIsPushEnabled := settings.authorizationStatus in
    [UNAuthorizationStatusAuthorized, UNAuthorizationStatusProvisional, UNAuthorizationStatusEphemeral];
  TThread.Queue(nil, procedure begin FCheckPushEnabledHandler(LIsPushEnabled) end);
end;
{$ENDIF}

procedure TFCMManager.CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
begin
  {$IF Defined(ANDROID)}
  AHandler(IsAndroidPushEnabled)
  {$ELSEIF Defined(IOS)}
  FCheckPushEnabledHandler := AHandler;
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.getNotificationSettingsWithCompletionHandler(CheckPushEnabledCompletionHandler);
  {$ELSE}
  AHandler(True);
  {$ENDIF}
end;

{$IF Defined(ANDROID)}
function TFCMManager.IsAndroidPushEnabled: Boolean;
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
  if Result and (TJBuild_Version.JavaClass.SDK_INT >= 26) then
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
{$ENDIF}

function TFCMManager.GetOnMessageReceived: TMessageReceivedEvent;
begin
  Result := FOnMessageReceived;
end;

function TFCMManager.GetOnStatusChange: TNotifyEvent;
begin
  Result := FOnStatusChange;
end;

procedure TFCMManager.SetOnMessageReceived(const AValue: TMessageReceivedEvent);
begin
  FOnMessageReceived := AValue;
end;

procedure TFCMManager.SetOnStatusChange(const AValue: TNotifyEvent);
begin
  FOnStatusChange := AValue;
end;

procedure TFCMManager.SetShowBannerIfForeground(const AValue: Boolean);
begin
  FShowBannerIfForeground := AValue;
end;

procedure TFCMManager.CheckStartupNotifications;
var
  LNotification: TPushServiceNotification;
begin
  // Handle startup notifications
  if FServiceConnection <> nil then
  begin
    for LNotification in FServiceConnection.Service.StartupNotifications do
      ReceiveNotificationHandler(FServiceConnection, LNotification);
  end;
end;

function TFCMManager.GetPushService: TPushService;
begin
  if FServiceConnection <> nil then
    Result := FServiceConnection.Service
  else
    Result := nil;
end;

function TFCMManager.GetShowBannerIfForeground: Boolean;
begin
  Result := FShowBannerIfForeground;
end;

function TFCMManager.IsStarted: Boolean;
begin
  if FServiceConnection <> nil then
    Result := FServiceConnection.Service.Status = TPushService.TStatus.Started
  else
    Result := False;
end;

function TFCMManager.GetToken: string;
begin
  Result := FDeviceToken;
end;

{$IF Defined(ANDROID)}
procedure TFCMManager.MessageReceivedNotificationHandler(const Sender: TObject; const AMsg: TMessage);
var
  LIcon: string;
  LIntent: JIntent;
begin
  if FShowBannerIfForeground and FIsForeground then
  begin
    LIntent := TMessageReceivedNotification(AMsg).Value;
    TOSMetadata.GetValue(cMetadataFCMDefaultNotificationIcon, LIcon);
    TJDWNotificationPresenter.JavaClass.presentNotification(TAndroidHelper.Context, LIntent, StringToJString(FChannelId), StrToIntDef(LIcon, 0));
  end;
end;
{$ENDIF}

procedure TFCMManager.PushDeviceTokenMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  // APNs token is saved purely for reference and diagnostic purposes
  {$IF Defined(IOS)}
  TUserDefaults.SetValue('APNS', TPushDeviceTokenMessage(AMsg).Value.Token);
  TOSLog.d('> APNS Token: %s', [TUserDefaults.GetValue('APNS')]);
  {$ENDIF}
end;

procedure TFCMManager.HandleNotification(const AServiceNotification: TPushServiceNotification);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, procedure begin ReceiveNotificationHandler(Self, AServiceNotification) end)
  else
    ReceiveNotificationHandler(Self, AServiceNotification);
end;

procedure TFCMManager.ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
begin
  if Assigned(FOnNotificationReceived) then
    FOnNotificationReceived(Self, AServiceNotification);
  if AServiceNotification.Json <> nil then
  begin
    if Assigned(FOnMessageReceived) then
      FOnMessageReceived(Self, AServiceNotification.Json);
  end;
end;

procedure TFCMManager.ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
begin
  if TPushService.TChange.DeviceToken in APushChanges then
  begin
    FDeviceToken := FServiceConnection.Service.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];
    TOSLog.d('Device Token: %s', [FDeviceToken]);
  end;
  if TPushService.TChange.Status in APushChanges then
  begin
    if FServiceConnection.Service.Status = TPushService.TStatus.StartupError then
      TOSLog.d('Startup error: %s', [FServiceConnection.Service.StartupError]);
    if Assigned(FOnStatusChange) then
      FOnStatusChange(Self);
  end;
end;

procedure TFCMManager.DoStart;
begin
  CreateChannel;
  CreateConnection;
  CheckStartupNotifications;
end;

procedure TFCMManager.Start;
begin
  {$IF Defined(ANDROID)}
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
  {$ELSE}
  DoStart;
  {$ENDIF}
end;

procedure TFCMManager.SubscribeToTopic(const ATopic: string);
begin
  {$IF Defined(ANDROID)}
  TJFirebaseMessaging.JavaClass.getInstance.subscribeToTopic(StringToJString(ATopic));
  {$ENDIF}
  {$IF Defined(IOS)}
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).subscribeToTopic(StrToNSStr(ATopic));
  {$ENDIF}
end;

procedure TFCMManager.UnsubscribeFromTopic(const ATopic: string);
begin
  {$IF Defined(ANDROID)}
  TJFirebaseMessaging.JavaClass.getInstance.unsubscribeFromTopic(StringToJString(ATopic));
  {$ENDIF}
  {$IF Defined(IOS)}
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).unsubscribeFromTopic(StrToNSStr(ATopic));
  {$ENDIF}
end;

initialization
  FCM := TFCMManager.Create;

end.
