unit DW.PushNotification;

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
  System.Classes, System.PushNotification, System.Json, System.Messaging, System.Notification;

type
  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TPushServiceNotification) of object;
  TMessageReceivedEvent = procedure(Sender: TObject; const JSON: TJSONObject) of object;

  TPushNotifications = class(TObject)
  private
    FChannelId: string;
    FChannelSoundName: string;
    FChannelTitle: string;
    FDeviceID: string;
    FDeviceToken: string;
    FIsForeground: Boolean;
    FServiceConnection: TPushServiceConnection;
    FShowBannerIfForeground: Boolean;
    FOnMessageReceived: TMessageReceivedEvent;
    FOnNotificationReceived: TNotificationReceivedEvent;
    FOnTokenReceived: TNotifyEvent;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    procedure CheckStartupNotifications;
    procedure CreateChannel;
    procedure CreateConnection;
    function GetChannelId: string;
    function GetPushService: TPushService;
    procedure PresentLocalNotification(const AJSON: TJSONObject);
    procedure ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
    procedure ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
  public
    constructor Create(const AChannelTitle: string; const AChannelSoundName: string = '');
    destructor Destroy; override;
    procedure AddChannel(const AChannel: TChannel);
    procedure Start;
    procedure SubscribeToTopic(const ATopic: string);
    procedure UnsubscribeFromTopic(const ATopic: string);
    property DeviceID: string read FDeviceID;
    property DeviceToken: string read FDeviceToken;
    property ShowBannerIfForeground: Boolean read FShowBannerIfForeground write FShowBannerIfForeground;
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    property OnNotificationReceived: TNotificationReceivedEvent read FOnNotificationReceived write FOnNotificationReceived;
    property OnTokenReceived: TNotifyEvent read FOnTokenReceived write FOnTokenReceived;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  // FMX
  FMX.Platform,
  {$IF Defined(ANDROID)}
  Androidapi.Helpers, Androidapi.JNI.Firebase, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net,
  FMX.PushNotification.Android,
  {$ENDIF}
  {$IF Defined(IOS)}
  Macapi.Helpers,
  iOSapi.FirebaseMessaging,
  FMX.PushNotification.FCM.iOS,
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.OSMetadata.Android,
  {$ENDIF}
  DW.Consts.Android, DW.OSDevice;

{ TPushNotifications }

constructor TPushNotifications.Create(const AChannelTitle: string; const AChannelSoundName: string = '');
begin
  inherited Create;
  FShowBannerIfForeground := True;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  FChannelTitle := AChannelTitle;
  FChannelSoundName := AChannelSoundName;
end;

destructor TPushNotifications.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FServiceConnection.Free;
  inherited;
end;

procedure TPushNotifications.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
      FIsForeground := True;
    TApplicationEvent.EnteredBackground:
      FIsForeground := False;
  end;
end;

procedure TPushNotifications.CreateConnection;
var
  LPushService: TPushService;
begin
  LPushService := GetPushService;
  FServiceConnection := TPushServiceConnection.Create(LPushService);
  FServiceConnection.Active := True;
  FServiceConnection.OnChange := ServiceConnectionChangeHandler;
  FServiceConnection.OnReceiveNotification := ReceiveNotificationHandler;
  FDeviceId := LPushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceId];
end;

procedure TPushNotifications.AddChannel(const AChannel: TChannel);
var
  LNotificationCenter: TNotificationCenter;
begin
  LNotificationCenter := TNotificationCenter.Create(nil);
  try
    {$IF Defined(ANDROID)}
    // Workaround for an issue regarding sound
    if not AChannel.SoundName.IsEmpty then
    begin
      if TPath.GetDirectoryName(AChannel.SoundName).IsEmpty then
        AChannel.SoundName := TPath.Combine(TPath.GetDocumentsPath, AChannel.SoundName);
      if TFile.Exists(AChannel.SoundName) then
        AChannel.SoundName := JStringToString(JFileToJURI(TJFile.JavaClass.init(StringToJString(AChannel.SoundName))).toString)
      else
        AChannel.SoundName := '';
    end;
    {$ENDIF}
    LNotificationCenter.CreateOrUpdateChannel(AChannel);
  finally
    LNotificationCenter.Free;
  end;
end;

procedure TPushNotifications.CreateChannel;
var
  LChannel: TChannel;
begin
  FChannelId := GetChannelId;
  if not FChannelId.IsEmpty and not FChannelTitle.IsEmpty then
  begin
    LChannel := TChannel.Create;
    try
      LChannel.Id := FChannelId;
      LChannel.Title := FChannelTitle;
      LChannel.Description := '';
      // Required for appearing as a banner when the app is not running, or when in the foreground
      LChannel.Importance := TImportance.High;
      LChannel.SoundName := FChannelSoundName;
      AddChannel(LChannel);
    finally
      LChannel.Free;
    end;
  end;
end;

function TPushNotifications.GetChannelId: string;
{$IF Defined(ANDROID)}
begin
  TPlatformOSMetadata.GetValue(cMetadataFCMDefaultChannelId, Result);
end;
{$ELSE}
begin
  Result := '';
end;
{$ENDIF}

procedure TPushNotifications.CheckStartupNotifications;
var
  LNotification: TPushServiceNotification;
begin
  // Handle startup notifications
  for LNotification in FServiceConnection.Service.StartupNotifications do
    ReceiveNotificationHandler(FServiceConnection, LNotification);
end;

function TPushNotifications.GetPushService: TPushService;
begin
  Result := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.FCM);
end;

procedure TPushNotifications.PresentLocalNotification(const AJSON: TJSONObject);
var
  LNotificationCenter: TNotificationCenter;
  LNotification: TNotification;
begin
  LNotificationCenter := TNotificationCenter.Create(nil);
  try
    LNotification := TNotification.Create;
    try
      if not AJSON.TryGetValue('android_channel_id', LNotification.ChannelId)
        and not AJSON.TryGetValue('["gcm.notification.android_channel_id"]', LNotification.ChannelId) then
      begin
        LNotification.ChannelId := FChannelId;
      end;
      if not AJSON.TryGetValue('title', LNotification.Title) then
        AJSON.TryGetValue('["gcm.notification.title"]', LNotification.Title);
      if not AJSON.TryGetValue('body', LNotification.AlertBody) then
        AJSON.TryGetValue('["gcm.notification.body"]', LNotification.AlertBody);
      LNotificationCenter.PresentNotification(LNotification);
    finally
      LNotification.Free;
    end;
  finally
    LNotificationCenter.Free;
  end;
end;

procedure TPushNotifications.ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
begin
  // An opportunity to handle the notification
  if Assigned(FOnNotificationReceived) then
    FOnNotificationReceived(Self, AServiceNotification);
  if AServiceNotification.Json <> nil then
  begin
    if TOSDevice.IsPlatform(TOSPlatform.pfAndroid) and FShowBannerIfForeground and FIsForeground then
      PresentLocalNotification(AServiceNotification.Json);
    if Assigned(FOnMessageReceived) then
      FOnMessageReceived(Self, AServiceNotification.Json);
  end;
end;

procedure TPushNotifications.ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
begin
  if TPushService.TChange.DeviceToken in APushChanges then
  begin
    FDeviceToken := GetPushService.DeviceTokenValue[TPushService.TDeviceTokenNames.DeviceToken];
    if Assigned(FOnTokenReceived) then
      FOnTokenReceived(Self);
  end;
end;

procedure TPushNotifications.Start;
begin
  CreateChannel;
  CreateConnection;
  CheckStartupNotifications;
end;

procedure TPushNotifications.SubscribeToTopic(const ATopic: string);
begin
  {$IF Defined(IOS)}
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).subscribeToTopic(StrToNSStr(ATopic));
  {$ENDIF}
  {$IF Defined(ANDROID)}
  TJFirebaseMessaging.JavaClass.getInstance.subscribeToTopic(StringToJString(ATopic));
  {$ENDIF}
end;

procedure TPushNotifications.UnsubscribeFromTopic(const ATopic: string);
begin
  {$IF Defined(IOS)}
  TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging).unsubscribeFromTopic(StrToNSStr(ATopic));
  {$ENDIF}
  {$IF Defined(ANDROID)}
  TJFirebaseMessaging.JavaClass.getInstance.unsubscribeFromTopic(StringToJString(ATopic));
  {$ENDIF}
end;

end.
