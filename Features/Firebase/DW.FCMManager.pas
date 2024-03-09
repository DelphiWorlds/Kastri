unit DW.FCMManager;

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
  // RTL
  System.PushNotification, System.Json, System.Classes;

type
  TTokenReceivedEvent = procedure(Sender: TObject; const Token: string) of object;
  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TPushServiceNotification) of object;
  TMessageReceivedEvent = procedure(Sender: TObject; const JSON: TJSONObject) of object;
  TCheckPushEnabledMethod = reference to procedure(const Enabled: Boolean);

  IFCMManager = interface(IInterface)
    ['{961FE28D-76AA-466D-AA0D-3086FBE4D678}']
    /// <summary>
    ///   Checks if Push Notifications are enabled for the application
    /// </summary>
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
    function GetAPNSToken: string;
    function GetOnMessageReceived: TMessageReceivedEvent;
    function GetOnStatusChange: TNotifyEvent;
    function GetOnStarted: TNotifyEvent;
    function GetOnTokenReceived: TTokenReceivedEvent;
    function GetPushService: TPushService;
    function GetShowBannerIfForeground: Boolean;
    function GetStatus: TPushService.TStatus;
    /// <summary>
    ///   Returns the FCM token
    /// </summary>
    function GetToken: string;
    /// <summary>
    ///   Indicates whether or not the push service has been started
    /// </summary>
    function IsStarted: Boolean;
    procedure SetOnMessageReceived(const AValue: TMessageReceivedEvent);
    procedure SetOnStarted(const AValue: TNotifyEvent);
    procedure SetOnStatusChange(const AValue: TNotifyEvent);
    procedure SetOnTokenReceived(const AValue: TTokenReceivedEvent);
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
    ///   Returns the status of the push service
    /// </summary>
    property Status: TPushService.TStatus read GetStatus;
    /// <summary>
    ///   This event is fired when the push service receives a message
    /// </summary>
    property OnMessageReceived: TMessageReceivedEvent read GetOnMessageReceived write SetOnMessageReceived;
    /// <summary>
    ///   This event is fired when the status of the push service changes
    /// </summary>
    property OnStatusChange: TNotifyEvent read GetOnStatusChange write SetOnStatusChange;
    /// <summary>
    ///   This event is fired when the push service is started
    /// </summary>
    property OnStarted: TNotifyEvent read GetOnStarted write SetOnStarted;
    /// <summary>
    ///   This event is fired when the push service receives a valid token
    /// </summary>
    property OnTokenReceived: TTokenReceivedEvent read GetOnTokenReceived write SetOnTokenReceived;
  end;

var
  FCM: IFCMManager;

implementation

uses
  // RTL
  System.SysUtils, System.Notification, System.Messaging, System.IOUtils, System.Permissions, System.StrUtils,
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
  iOSapi.UserNotifications, iOSapi.Foundation,
  // FMX
  // ** NOTE: As at Delphi 11.x and Delphi 12, FCMManager requires the following unit to be patched. **
  // Please refer to the readme, here: https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted
  FMX.PushNotification.FCM.iOS,
  {$ENDIF}
  FMX.Platform,
  {$IF Defined(IOS)}
  // DW
  DW.UserDefaults.iOS, DW.iOSapi.FirebaseMessaging, DW.iOSapi.FirebaseCore, DW.Firebase.Common.iOS,
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

  TFCMManager = class;

  TDWFirebaseMessagingServiceCallback = class(TJavaLocal, JDWFirebaseMessagingServiceCallback)
  private
    FFCMManager: TFCMManager;
  public
    { JDWFirebaseMessagingServiceCallback }
    function getShowNotificationWhenForeground: Boolean; cdecl;
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
    FIsStarted: Boolean;
    {$IF Defined(IOS)}
    FMessaging: FIRMessaging;
    {$ENDIF}
    FServiceConnection: TPushServiceConnection;
    FShowBannerIfForeground: Boolean;
    FOnMessageReceived: TMessageReceivedEvent;
    FOnNotificationReceived: TNotificationReceivedEvent;
    FOnStarted: TNotifyEvent;
    FOnStatusChange: TNotifyEvent;
    FOnTokenReceived: TTokenReceivedEvent;
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
    procedure Started;
    procedure StatusChange;
    procedure TokenReceived;
    {$IF Defined(IOS)}
    procedure SubscribeToTopicCompletionHandler(error: NSError);
    procedure UnsubscribeFromTopicCompletionHandler(error: NSError);
    {$ENDIF}
  protected
    procedure HandleNotification(const AServiceNotification: TPushServiceNotification);
    property ShowBannerIfForeground: Boolean read FShowBannerIfForeground;
  public
    { IFCMManager }
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
    function GetAPNSToken: string;
    function GetOnMessageReceived: TMessageReceivedEvent;
    function GetOnStarted: TNotifyEvent;
    function GetOnStatusChange: TNotifyEvent;
    function GetOnTokenReceived: TTokenReceivedEvent;
    function GetPushService: TPushService;
    function GetShowBannerIfForeground: Boolean;
    function GetStatus: TPushService.TStatus;
    function GetToken: string;
    function IsStarted: Boolean;
    procedure SetOnMessageReceived(const AValue: TMessageReceivedEvent);
    procedure SetOnStarted(const AValue: TNotifyEvent);
    procedure SetOnStatusChange(const AValue: TNotifyEvent);
    procedure SetOnTokenReceived(const AValue: TTokenReceivedEvent);
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


{$IF Defined(IOS)}
function HexStringToNSData(const AValue: string): NSData;
var
  LLength, I: Integer;
  LBytes: TBytes;
begin
  Result := nil;
  if not AValue.IsEmpty then
  begin
    LLength := Length(AValue) div 2;
    SetLength(LBytes, LLength);
    for I := 0 to LLength - 1 do
      LBytes[I] := StrToInt('$' + AValue.Substring(I * 2, 2));
    Result := TNSData.Wrap(TNSData.OCClass.dataWithBytes(@LBytes[0], Length(LBytes)));
  end;
end;
{$ENDIF}

{$IF Defined(ANDROID)}
{ TDWFirebaseMessagingServiceCallback }

constructor TDWFirebaseMessagingServiceCallback.Create(const AFCMManager: TFCMManager);
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
{$ENDIF}

{ TFCMManager }

constructor TFCMManager.Create;
begin
  inherited Create;
  TOSMetadata.GetValue(cMetadataFCMDefaultChannelId, FChannelId);
  FShowBannerIfForeground := True;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  // TMessageManager.DefaultManager.SubscribeToMessage(TPushRemoteNotificationMessage, PushDeviceTokenMessageHandler);
  {$IF Defined(ANDROID)}
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  FFirebaseMessagingServiceCallback := TDWFirebaseMessagingServiceCallback.Create(Self);
  TJDWFirebaseMessagingService.JavaClass.setCallback(FFirebaseMessagingServiceCallback);
  {$ENDIF}
  {$IF Defined(IOS)}
  if not TUserDefaults.GetValue('APNS').IsEmpty then
    TOSLog.d('APNS: %s', [TUserDefaults.GetValue('APNS')]);
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

procedure TFCMManager.SubscribeToTopicCompletionHandler(error: NSError);
begin
  if error <> nil then
    TOSLog.d('> Error - %d: %s', [error.code, NSStrToStr(error.localizedDescription)]);
end;

procedure TFCMManager.UnsubscribeFromTopicCompletionHandler(error: NSError);
begin
  if error <> nil then
    TOSLog.d('> Error - %d: %s', [error.code, NSStrToStr(error.localizedDescription)]);
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

function TFCMManager.GetAPNSToken: string;
begin
  {$IF Defined(IOS)}
  Result := TUserDefaults.GetValue('APNS');
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

function TFCMManager.GetOnMessageReceived: TMessageReceivedEvent;
begin
  Result := FOnMessageReceived;
end;

function TFCMManager.GetOnStarted: TNotifyEvent;
begin
  Result := FOnStarted;
end;

function TFCMManager.GetOnStatusChange: TNotifyEvent;
begin
  Result := FOnStatusChange;
end;

function TFCMManager.GetOnTokenReceived: TTokenReceivedEvent;
begin
  Result := FOnTokenReceived;
end;

procedure TFCMManager.SetOnMessageReceived(const AValue: TMessageReceivedEvent);
begin
  FOnMessageReceived := AValue;
end;

procedure TFCMManager.SetOnStarted(const AValue: TNotifyEvent);
begin
  FOnStarted := AValue;
end;

procedure TFCMManager.SetOnStatusChange(const AValue: TNotifyEvent);
begin
  FOnStatusChange := AValue;
end;

procedure TFCMManager.SetOnTokenReceived(const AValue: TTokenReceivedEvent);
begin
  FOnTokenReceived := AValue;
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

function TFCMManager.GetStatus: TPushService.TStatus;
begin
  Result := TPushService.TStatus.Stopped;
  if (FServiceConnection <> nil) and (FServiceConnection.Service <> nil) then
    Result := FServiceConnection.Service.Status;
end;

function TFCMManager.IsStarted: Boolean;
begin
  Result := FIsStarted;
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
  {$IF Defined(IOS)}
  TUserDefaults.SetValue('APNS', TPushDeviceTokenMessage(AMsg).Value.Token);
  TOSLog.d('Received APNS of: %s', [TUserDefaults.GetValue('APNS')]);
  if not FIsStarted then
    Started;
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
var
  LToken: string;
  LNeedsChange: Boolean;
begin
  LNeedsChange := False;
  if TPushService.TChange.DeviceToken in APushChanges then
  begin
    LToken := FServiceConnection.Service.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];
    if not LToken.Equals(FDeviceToken) then
    begin
      FDeviceToken := LToken;
      {$IF Defined(IOS)}
      if not GetAPNSToken.IsEmpty then
        TokenReceived;
      {$ELSE}
      TokenReceived;
      {$ENDIF}
      LNeedsChange := True;
    end;
  end;
  if TPushService.TChange.Status in APushChanges then
  begin
    case FServiceConnection.Service.Status of
      TPushService.TStatus.Started:
      begin
        {$IF Defined(IOS)}
        if not GetAPNSToken.IsEmpty then
          Started;
        {$ELSE}
        Started;
        {$ENDIF}
      end;
      TPushService.TStatus.StartupError:
      begin
        TOSLog.d('Startup error: %s', [FServiceConnection.Service.StartupError]);
        LNeedsChange := True;
      end;
    end;
  end;
  if LNeedsChange then
    StatusChange;
end;

procedure TFCMManager.TokenReceived;
begin
  if Assigned(FOnTokenReceived) then
    FOnTokenReceived(Self, FDeviceToken);
end;

procedure TFCMManager.Started;
begin
  FIsStarted := True;
  CheckStartupNotifications;
  if Assigned(FOnStarted) then
    FOnStarted(Self);
end;

procedure TFCMManager.StatusChange;
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

procedure TFCMManager.DoStart;
begin
  CreateChannel;
  CreateConnection;
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
  {$ELSEIF Defined(IOS)}
  FMessaging := TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging);
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
  FMessaging.setAPNSToken(HexStringToNSData(TUserDefaults.GetValue('APNS')));
  FMessaging.subscribeToTopic(StrToNSStr(ATopic), SubscribeToTopicCompletionHandler);
  {$ENDIF}
end;

procedure TFCMManager.UnsubscribeFromTopic(const ATopic: string);
begin
  {$IF Defined(ANDROID)}
  TJFirebaseMessaging.JavaClass.getInstance.unsubscribeFromTopic(StringToJString(ATopic));
  {$ENDIF}
  {$IF Defined(IOS)}
  FMessaging.setAPNSToken(HexStringToNSData(TUserDefaults.GetValue('APNS')));
  FMessaging.unsubscribeFromTopic(StrToNSStr(ATopic), UnsubscribeFromTopicCompletionHandler);
  {$ENDIF}
end;

initialization
  FCM := TFCMManager.Create;

end.
