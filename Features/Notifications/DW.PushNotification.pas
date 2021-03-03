unit DW.PushNotification;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes, System.PushNotification, System.Json, System.Messaging;

type
  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TPushServiceNotification) of object;
  TMessageReceivedEvent = procedure(Sender: TObject; const JSON: TJSONObject) of object;

  TPushNotifications = class(TObject)
  private
    FChannelId: string;
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
    constructor Create(const AChannelTitle: string);
    destructor Destroy; override;
    procedure Start;
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
  System.SysUtils, System.Notification,
  // FMX
  FMX.Platform,
  {$IF Defined(ANDROID)}
  FMX.PushNotification.Android,
  {$ENDIF}
  {$IF Defined(IOS)}
  {$IF Declared(RTLVersion1042)} // i.e. Delphi 10.4.2 or later
  FMX.PushNotification.FCM.iOS,
  {$ELSE}
  DW.PushNotification.iOS;
  {$ENDIF}
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.OSMetadata.Android,
  {$ENDIF}
  DW.Consts.Android, DW.OSDevice;

{ TPushNotifications }

constructor TPushNotifications.Create(const AChannelTitle: string);
begin
  inherited Create;
  FShowBannerIfForeground := True;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  FChannelTitle := AChannelTitle;
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

procedure TPushNotifications.CreateChannel;
var
  LNotificationCenter: TNotificationCenter;
  LChannel: TChannel;
begin
  FChannelId := GetChannelId;
  if FChannelId.IsEmpty or FChannelTitle.IsEmpty then
    Exit; // <======
  LNotificationCenter := TNotificationCenter.Create(nil);
  try
    LChannel := TChannel.Create;
    try
      LChannel.Id := FChannelId;
      LChannel.Title := FChannelTitle;
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
      LNotification.ChannelId := FChannelId;
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

end.
