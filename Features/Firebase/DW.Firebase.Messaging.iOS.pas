unit DW.Firebase.Messaging.iOS;

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
  // RTL
  System.TypInfo, System.Messaging,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.Firebase.Messaging, DW.iOSapi.UserNotifications, DW.iOSapi.Firebase; // DW.Notifications;

type
  TPlatformFirebaseMessaging = class;

  TFIRMessagingDelegate = class(TOCLocal, FIRMessagingDelegate)
  private
    FFirebaseMessaging: TPlatformFirebaseMessaging;
    procedure ReceivedMessage(remoteMessage: FIRMessagingRemoteMessage);
  public
    constructor Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
    procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl;
    [MethodName('messaging:didReceiveMessage:')]
    procedure didReceiveMessage(messaging: FIRMessaging; remoteMessage: FIRMessagingRemoteMessage); cdecl;
    [MethodName('messaging:didReceiveRegistrationToken:')]
    procedure didReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
    [MethodName('messaging:didRefreshRegistrationToken:')]
    procedure didRefreshRegistrationToken(messaging: FIRMessaging; fcmToken: NSString); cdecl;
  end;

  TPlatformFirebaseMessaging = class(TCustomPlatformFirebaseMessaging)
  private
    FAuthOptions: UNAuthorizationOptions;
    FFIRMessagingDelegate: TFIRMessagingDelegate;
    FMessaging: FIRMessaging;
    procedure CheckNotificationsAuthorizationHandler(settings: UNNotificationSettings);
    function GetUserDefaultsTokenKey: NSString;
    function Messaging: FIRMessaging;
    procedure PushDeviceTokenMessageHandler(const Sender: TObject; const M: TMessage);
    procedure PushStartupNotificationMessageMessageHandler(const Sender: TObject; const M: TMessage);
    procedure RegisterRemoteNotificationsIOS10OrLater;
    procedure RegisterRemoteNotificationsIOS7OrEarlier;
    procedure RegisterRemoteNotificationsIOS8OrLater;
    procedure RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure DoApplicationBecameActive; override;
    procedure DoApplicationEnteredBackground; override;
    function GetDeviceToken: string; override;
    procedure MessageReceived(const AJSON: string);
    procedure TokenReceived(const AToken: string);
    procedure RequestAuthorization; override;
    procedure SubscribeToTopic(const ATopicName: string); override;
    function Start: Boolean; override;
    procedure UnsubscribeFromTopic(const ATopicName: string); override;
  public
    constructor Create(const AFirebaseMessaging: TFirebaseMessaging); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes, System.IOUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjCRuntime,
  // iOS
  iOSapi.Helpers, iOSapi.UIKit,
  // FMX
  FMX.Platform,
  // DW
  DW.OSLog, DW.Macapi.ObjCRuntime, DW.iOSapi.Helpers, DW.Notifications.iOS, DW.Macapi.Helpers;

function StringToNSData(const AString: string): NSData;
begin
  Result := StrToNSStr(AString).dataUsingEncoding(NSUTF8StringEncoding);
end;

{ TFIRMessagingDelegate }

constructor TFIRMessagingDelegate.Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

procedure TFIRMessagingDelegate.applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage);
begin
  ReceivedMessage(remoteMessage);
end;

procedure TFIRMessagingDelegate.didReceiveMessage(messaging: FIRMessaging; remoteMessage: FIRMessagingRemoteMessage);
begin
  ReceivedMessage(remoteMessage);
end;

procedure TFIRMessagingDelegate.didReceiveRegistrationToken(messaging: FIRMessaging; fcmToken: NSString);
begin
  TOSLog.d('+TPlatformFirebaseMessaging.didReceiveRegistrationToken');
  FFirebaseMessaging.TokenReceived(NSStrToStr(fcmToken));
end;

procedure TFIRMessagingDelegate.didRefreshRegistrationToken(messaging: FIRMessaging; fcmToken: NSString);
begin
  TOSLog.d('+TPlatformFirebaseMessaging.didRefreshRegistrationToken');
  FFirebaseMessaging.TokenReceived(NSStrToStr(fcmToken));
end;

procedure TFIRMessagingDelegate.ReceivedMessage(remoteMessage: FIRMessagingRemoteMessage);
begin
  FFirebaseMessaging.MessageReceived(TiOSHelperEx.NSDictionaryToJSON(remoteMessage.appData));
end;

{ TPlatformFirebaseMessaging }

constructor TPlatformFirebaseMessaging.Create(const AFirebaseMessaging: TFirebaseMessaging);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TPushStartupNotificationMessage, PushStartupNotificationMessageMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
end;

destructor TPlatformFirebaseMessaging.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TPushStartupNotificationMessage, PushStartupNotificationMessageMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  inherited;
end;

function TPlatformFirebaseMessaging.Start: Boolean;
begin
  TOSLog.d('+TPlatformFirebaseMessaging.Start');
  Result := False;
  try
    TFIRApp.OCClass.configure;
    FFIRMessagingDelegate := TFIRMessagingDelegate.Create(self);
    Messaging.setDelegate(FFIRMessagingDelegate.GetObjectID);
    Result := True;
  except
    on E: Exception do
      DoException(E);
  end;
  TOSLog.d('-TPlatformFirebaseMessaging.Start');
end;

function TPlatformFirebaseMessaging.GetDeviceToken: string;
begin
  Result := NSStrToStr(TiOSHelperEx.StandardUserDefaults.stringForKey(GetUserDefaultsTokenKey));
end;

function TPlatformFirebaseMessaging.GetUserDefaultsTokenKey: NSString;
begin
  Result := StrToNSStr(NSStrToStr(TiOSHelper.MainBundle.bundleIdentifier) + '.DeviceToken');
end;

procedure TPlatformFirebaseMessaging.PushDeviceTokenMessageHandler(const Sender: TObject; const M: TMessage);
var
  LDeviceToken: string;
begin
  LDeviceToken := TPushDeviceTokenMessage(M).Value.Token;
  TOSLog.d('Received Device Token: %s', [LDeviceToken]);
  TiOSHelperEx.StandardUserDefaults.setObject(NSObjectToID(StrToNSStr(LDeviceToken)), GetUserDefaultsTokenKey);
end;

procedure TPlatformFirebaseMessaging.PushStartupNotificationMessageMessageHandler(const Sender: TObject; const M: TMessage);
begin
  MessageReceived(TPushStartupNotificationMessage(M).Value.Notification);
end;

procedure TPlatformFirebaseMessaging.Connect;
begin
  IsConnected := True;
end;

procedure TPlatformFirebaseMessaging.Disconnect;
begin
//  if IsConnected then
//    Messaging.disconnect;
  IsConnected := False;
end;

procedure TPlatformFirebaseMessaging.DoApplicationBecameActive;
begin
  if TOSVersion.Check(10) then
    Messaging.setShouldEstablishDirectChannel(True);
end;

procedure TPlatformFirebaseMessaging.DoApplicationEnteredBackground;
begin
  if TOSVersion.Check(10) then
    Messaging.setShouldEstablishDirectChannel(False);
end;

procedure TPlatformFirebaseMessaging.TokenReceived(const AToken: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      DoTokenReceived(AToken);
    end
  );
end;

procedure TPlatformFirebaseMessaging.MessageReceived(const AJSON: string);
begin
  TOSLog.d('Incoming Message: %s', [AJSON]);
  TMessageManager.DefaultManager.SendMessage(nil, TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(AJSON)));
end;

procedure TPlatformFirebaseMessaging.RegisterRemoteNotificationsIOS10OrLater;
begin
  UserNotificationCenter.getNotificationSettingsWithCompletionHandler(CheckNotificationsAuthorizationHandler);
end;

procedure TPlatformFirebaseMessaging.CheckNotificationsAuthorizationHandler(settings: UNNotificationSettings);
begin
  UserNotificationCenter.requestAuthorizationWithOptions(FAuthOptions, RequestAuthorizationWithOptionsCompletionHandler);
end;

procedure TPlatformFirebaseMessaging.RegisterRemoteNotificationsIOS7OrEarlier;
begin
  TiOSHelper.SharedApplication.registerForRemoteNotificationTypes(Addr(FAuthOptions));
  DoAuthorizationResult(True);
end;

procedure TPlatformFirebaseMessaging.RegisterRemoteNotificationsIOS8OrLater;
var
  LSettings: UIUserNotificationSettings;
begin
  LSettings := TUIUserNotificationSettings.Wrap(TUIUserNotificationSettings.OCClass.settingsForTypes(FAuthOptions, nil));
  TiOSHelper.SharedApplication.registerUserNotificationSettings(LSettings);
  DoAuthorizationResult(True);
end;

procedure TPlatformFirebaseMessaging.RequestAuthorization;
begin
  TOSLog.d('TPlatformFirebaseMessaging.RequestAuthorization');
  TPlatformNotifications.UpdateDelegate;
  FAuthOptions := UNAuthorizationOptionSound or UNAuthorizationOptionAlert or UNAuthorizationOptionBadge;
  if TOSVersion.Check(10) then
    RegisterRemoteNotificationsIOS10OrLater
  else if TOSVersion.Check(8) then
    RegisterRemoteNotificationsIOS8OrLater
  else
    RegisterRemoteNotificationsIOS7OrEarlier;
end;

procedure TPlatformFirebaseMessaging.RequestAuthorizationWithOptionsCompletionHandler(granted: Boolean; error: NSError);
begin
  if granted then
  begin
    TOSLog.d('Authorization GRANTED');
    if not TiOSHelperEx.SharedApplication.isRegisteredForRemoteNotifications then
    begin
      TiOSHelperEx.SharedApplication.registerForRemoteNotifications;
      TOSLog.d('Registered for remote notifications');
    end
    else
      TOSLog.d('ALREADY registered for remote notifications');
  end
  else
    TOSLog.d('Authorization NOT GRANTED');
  TThread.Queue(nil,
    procedure
    begin
      DoAuthorizationResult(granted);
    end
  );
end;

function TPlatformFirebaseMessaging.Messaging: FIRMessaging;
begin
  if FMessaging = nil then
    FMessaging := TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging);
  Result := FMessaging;
end;

procedure TPlatformFirebaseMessaging.SubscribeToTopic(const ATopicName: string);
begin
  Messaging.subscribeToTopic(StrToNSStr(ATopicName));
end;

procedure TPlatformFirebaseMessaging.UnsubscribeFromTopic(const ATopicName: string);
begin
  Messaging.unsubscribeFromTopic(StrToNSStr(ATopicName));
end;

end.
