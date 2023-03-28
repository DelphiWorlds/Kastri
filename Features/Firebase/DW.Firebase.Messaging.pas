unit DW.Firebase.Messaging;

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
  System.Classes, System.Messaging, System.SysUtils;

type
  TFirebaseMessageReceivedEvent = procedure(Sender: TObject; const APayload: TStrings) of object;
  TFirebaseTokenReceivedEvent = procedure(Sender: TObject; const AToken: string) of object;

  TAuthorizationOption = (Badge, Sound, Alert, CarPlay, CriticalAlert, ProvidesAppNotificationSettings, Provisional);
  TAuthorizationOptions = set of TAuthorizationOption;

  TFirebaseMessaging = class;

  TCustomPlatformFirebaseMessaging = class(TObject)
  private
    FAuthorizationOptions: TAuthorizationOptions;
    FFirebaseMessaging: TFirebaseMessaging;
    FIsForeground: Boolean;
    FShowBannerWhenForeground: Boolean;
  protected
    procedure ApplicationBecameActive;
    procedure ApplicationEnteredBackground;
    procedure DoApplicationBecameActive; virtual;
    procedure DoApplicationEnteredBackground; virtual;
    procedure DoAuthorizationResult(const AGranted: Boolean);
    procedure DoException(const AException: Exception);
    procedure DoMessageReceived(const APayload: TStrings);
    procedure DoTokenReceived(const AToken: string);
    function GetDeviceToken: string; virtual;
    procedure RequestPermissions; virtual; abstract;
    procedure SubscribeToTopic(const ATopicName: string); virtual; abstract;
    function Start: Boolean; virtual;
    procedure UnsubscribeFromTopic(const ATopicName: string); virtual; abstract;
    property AuthorizationOptions: TAuthorizationOptions read FAuthorizationOptions write FAuthorizationOptions;
    property IsForeground: Boolean read FIsForeground write FIsForeground;
    property ShowBannerWhenForeground: Boolean read FShowBannerWhenForeground write FShowBannerWhenForeground;
  public
    constructor Create(const AFirebaseMessaging: TFirebaseMessaging); virtual;
    destructor Destroy; override;
  end;

  TAuthorizationResultEvent = procedure(Sender: TObject; const Granted: Boolean) of object;
  TFailedToRegisterEvent = procedure(Sender: TObject; const ErrorMessage: string) of object;

  TFirebaseMessaging = class(TObject)
  private
    FIsActive: Boolean;
    FPlatformFirebaseMessaging: TCustomPlatformFirebaseMessaging;
    FToken: string;
    FOnAuthorizationResult: TAuthorizationResultEvent;
    FOnFailedToRegister: TFailedToRegisterEvent;
    FOnMessageReceived: TFirebaseMessageReceivedEvent;
    FOnTokenReceived: TFirebaseTokenReceivedEvent;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    function GetAuthorizationOptions: TAuthorizationOptions;
    function GetDeviceToken: string;
    function GetShowBannerWhenForeground: Boolean;
    procedure PushFailToRegisterMessageHandler(const Sender: TObject; const M: TMessage);
    procedure PushRemoteNotificationMessageHandler(const Sender: TObject; const M: TMessage);
    procedure SetAuthorizationOptions(const Value: TAuthorizationOptions);
    procedure SetShowBannerWhenForeground(const Value: Boolean);
  protected
    procedure DoAuthorizationResult(const AGranted: Boolean);
    procedure DoFailedToRegister(const AErrorMessage: string);
    procedure DoMessageReceived(const APayload: TStrings);
    procedure DoTokenReceived(const AToken: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RequestPermissions;
    /// <summary>
    ///   Allows the device to receive messages targeting the specified topic
    /// </summary>
    procedure SubscribeToTopic(const ATopicName: string);
    /// <summary>
    ///   Call this method after assigning event handlers
    /// </summary>
    function Start: Boolean;
    /// <summary>
    ///   Removes the device from receiving messages for the specified topic
    /// </summary>
    procedure UnsubscribeFromTopic(const ATopicName: string);
    /// <summary>
    ///   Options for when a notification is displayed (iOS only)
    /// </summary>
    property AuthorizationOptions: TAuthorizationOptions read GetAuthorizationOptions write SetAuthorizationOptions;
    /// <summary>
    ///   Token associated with the device. On iOS, this is the APNs token
    /// </summary>
    property DeviceToken: string read GetDeviceToken;
    property IsActive: Boolean read FIsActive;
    /// <summary>
    ///   [Android only] A banner will be shown when a notification is received if the app is in the foreground
    /// </summary>
    property ShowBannerWhenForeground: Boolean read GetShowBannerWhenForeground write SetShowBannerWhenForeground;
    /// <summary>
    ///   FCM token
    /// </summary>
    property Token: string read FToken;
    property OnAuthorizationResult: TAuthorizationResultEvent read FOnAuthorizationResult write FOnAuthorizationResult;
    property OnFailedToRegister: TFailedToRegisterEvent read FOnFailedToRegister write FOnFailedToRegister;
    property OnMessageReceived: TFirebaseMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    property OnTokenReceived: TFirebaseTokenReceivedEvent read FOnTokenReceived write FOnTokenReceived;
  end;

implementation

uses
  // FMX
  FMX.Platform,
  // DW
  DW.OSLog,
  {$IF Defined(IOS)}
  DW.Firebase.Messaging.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.Firebase.Messaging.Android;
  {$ELSE}
  DW.Firebase.Default;
  {$ENDIF}

{ TCustomPlatformFirebaseMessaging }

constructor TCustomPlatformFirebaseMessaging.Create(const AFirebaseMessaging: TFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
  FShowBannerWhenForeground := True;
  FAuthorizationOptions := [TAuthorizationOption.Badge, TAuthorizationOption.Sound, TAuthorizationOption.Alert];
end;

destructor TCustomPlatformFirebaseMessaging.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformFirebaseMessaging.DoException(const AException: Exception);
begin
  TOSLog.d('Exception - %s: %s', [AException.ClassName, AException.Message]);
end;

procedure TCustomPlatformFirebaseMessaging.ApplicationBecameActive;
begin
  FIsForeground := True;
  DoApplicationBecameActive;
end;

procedure TCustomPlatformFirebaseMessaging.ApplicationEnteredBackground;
begin
  FIsForeground := False;
  DoApplicationEnteredBackground;
end;

procedure TCustomPlatformFirebaseMessaging.DoApplicationBecameActive;
begin
  //
end;

procedure TCustomPlatformFirebaseMessaging.DoApplicationEnteredBackground;
begin
  //
end;

procedure TCustomPlatformFirebaseMessaging.DoAuthorizationResult(const AGranted: Boolean);
begin
  FFirebaseMessaging.DoAuthorizationResult(AGranted);
end;

procedure TCustomPlatformFirebaseMessaging.DoMessageReceived(const APayload: TStrings);
begin
  FFirebaseMessaging.DoMessageReceived(APayload);
end;

procedure TCustomPlatformFirebaseMessaging.DoTokenReceived(const AToken: string);
begin
  FFirebaseMessaging.DoTokenReceived(AToken);
end;

function TCustomPlatformFirebaseMessaging.GetDeviceToken: string;
begin
  Result := '';
end;

function TCustomPlatformFirebaseMessaging.Start: Boolean;
begin
  Result := False;
end;

{ TFirebaseMessaging }

constructor TFirebaseMessaging.Create;
begin
  inherited;
  FPlatformFirebaseMessaging := TPlatformFirebaseMessaging.Create(Self);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushFailToRegisterMessage, PushFailToRegisterMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TPushRemoteNotificationMessage, PushRemoteNotificationMessageHandler);
end;

destructor TFirebaseMessaging.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushFailToRegisterMessage, PushFailToRegisterMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TPushRemoteNotificationMessage, PushRemoteNotificationMessageHandler);
  FPlatformFirebaseMessaging.Free;
  inherited;
end;

procedure TFirebaseMessaging.DoAuthorizationResult(const AGranted: Boolean);
begin
  if Assigned(FOnAuthorizationResult) then
    FOnAuthorizationResult(Self, AGranted);
end;

procedure TFirebaseMessaging.DoFailedToRegister(const AErrorMessage: string);
begin
  if Assigned(FOnFailedToRegister) then
    FOnFailedToRegister(Self, AErrorMessage);
end;

procedure TFirebaseMessaging.DoMessageReceived(const APayload: TStrings);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, APayload);
end;

procedure TFirebaseMessaging.DoTokenReceived(const AToken: string);
begin
  if not AToken.Equals(FToken) then
  begin
    FToken := AToken;
    TOSLog.d('FCM Token: %s', [FToken]);
    if Assigned(FOnTokenReceived) then
      FOnTokenReceived(Self, AToken);
  end;
end;

function TFirebaseMessaging.GetDeviceToken: string;
begin
  Result := FPlatformFirebaseMessaging.GetDeviceToken;
end;

function TFirebaseMessaging.GetShowBannerWhenForeground: Boolean;
begin
  Result := FPlatformFirebaseMessaging.ShowBannerWhenForeground;
end;

procedure TFirebaseMessaging.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
      FPlatformFirebaseMessaging.ApplicationBecameActive;
    TApplicationEvent.EnteredBackground:
      FPlatformFirebaseMessaging.ApplicationEnteredBackground;
  end;
end;

procedure TFirebaseMessaging.PushFailToRegisterMessageHandler(const Sender: TObject; const M: TMessage);
begin
  DoFailedToRegister(TPushFailToRegisterMessage(M).Value.ErrorMessage);
end;

procedure TFirebaseMessaging.PushRemoteNotificationMessageHandler(const Sender: TObject; const M: TMessage);
var
  LPayload: TStrings;
  LJSON: string;
begin
  LPayload := TStringList.Create;
  try
    if (M is TPushRemoteNotificationMessage) then
      LJSON := (M as TPushRemoteNotificationMessage).Value.Notification
    else if (M is TPushStartupNotificationMessage) then
      LJSON := (M as TPushStartupNotificationMessage).Value.Notification
    else
      LJSON := '';
    if LJSON <> '' then
    begin
      LPayload.Text := LJSON; // TODO: Formatting?
      DoMessageReceived(LPayload);
    end;
  finally
    LPayload.Free;
  end;
end;

procedure TFirebaseMessaging.RequestPermissions;
begin
  FPlatformFirebaseMessaging.RequestPermissions;
end;

function TFirebaseMessaging.GetAuthorizationOptions: TAuthorizationOptions;
begin
  Result := FPlatformFirebaseMessaging.AuthorizationOptions;
end;

procedure TFirebaseMessaging.SetAuthorizationOptions(const Value: TAuthorizationOptions);
begin
  FPlatformFirebaseMessaging.AuthorizationOptions := Value;
end;

procedure TFirebaseMessaging.SetShowBannerWhenForeground(const Value: Boolean);
begin
  FPlatformFirebaseMessaging.ShowBannerWhenForeground := Value;
end;

function TFirebaseMessaging.Start: Boolean;
begin
  Result := FIsActive;
  if not Result then
    Result := FPlatformFirebaseMessaging.Start;
  FIsActive := Result;
  if Result then
    RequestPermissions;
end;

procedure TFirebaseMessaging.SubscribeToTopic(const ATopicName: string);
begin
  FPlatformFirebaseMessaging.SubscribeToTopic(ATopicName);
end;

procedure TFirebaseMessaging.UnsubscribeFromTopic(const ATopicName: string);
begin
  FPlatformFirebaseMessaging.UnsubscribeFromTopic(ATopicName);
end;

end.
