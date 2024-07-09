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

{$SCOPEDENUMS ON}

uses
  // RTL
  System.PushNotification, System.Json, System.Classes, System.Messaging;

type
  TTokenReceivedEvent = procedure(Sender: TObject; const Token: string) of object;
  TNotificationReceivedEvent = procedure(Sender: TObject; const Notification: TPushServiceNotification) of object;
  TMessageReceivedEvent = procedure(Sender: TObject; const JSON: TJSONObject) of object;
  TCheckPushEnabledMethod = reference to procedure(const Enabled: Boolean);

  TNotificationActionProc = reference to procedure;

  TNotificationActionOption = (AuthenticationRequired, Destructive, Foreground);

  TNotificationActionOptions = set of TNotificationActionOption;

  INotificationAction = interface(IInterface)
    ['{22C26F7D-7E56-4534-837E-C271CE169309}']
    function GetHandler: TNotificationActionProc;
    function GetID: string;
    function GetOptions: TNotificationActionOptions;
    function GetTitle: string;
    property Handler: TNotificationActionProc read GetHandler;
    property ID: string read GetID;
    property Options: TNotificationActionOptions read GetOptions;
    property Title: string read GetTitle;
  end;

  TNotificationActions = TArray<INotificationAction>;

  TNotificationCategoryOption = (CustomDismissAction, AllowInCarPlay, HiddenPreviewsShowTitle, HiddenPreviewsShowSubtitle, AllowAnnouncement);

  TNotificationCategoryOptions = set of TNotificationCategoryOption;

  TNotificationCategoryProc = reference to procedure;

  INotificationCategory = interface(IInterface)
    ['{B84E19A4-082A-4AAC-82C4-BEF7B0DD37E3}']
    function AddAction(const AID, ATitle: string; const AHandler: TNotificationActionProc;
      const AOptions: TNotificationActionOptions = []): INotificationAction;
    function FindAction(const AID: string; out AAction: INotificationAction): Boolean;
    function GetAction(const AIndex: Integer): INotificationAction;
    function GetActionCount: Integer;
    function GetHandler: TNotificationCategoryProc;
    function GetID: string;
    function GetOptions: TNotificationCategoryOptions;
    property ActionCount: Integer read GetActionCount;
    property Actions[const AIndex: Integer]: INotificationAction read GetAction;
    property Handler: TNotificationCategoryProc read GetHandler;
    property ID: string read GetID;
    property Options: TNotificationCategoryOptions read GetOptions;
  end;

  TNotificationCategories = TArray<INotificationCategory>;

  TAuthOption = (Badge, Sound, Alert, CarPlay, CriticalAlert, ProvidesAppNotificationSettings, Provisional);
  TAuthOptions = set of TAuthOption;

  IFCMManager = interface(IInterface)
    ['{961FE28D-76AA-466D-AA0D-3086FBE4D678}']
    function AddCategory(const AID: string; const AHandler: TNotificationCategoryProc;
      const AOptions: TNotificationCategoryOptions = []): INotificationCategory; overload;
    function AddCategory(const AID: string; const AOptions: TNotificationCategoryOptions = []): INotificationCategory; overload;
    /// <summary>
    ///   Checks if Push Notifications are enabled for the application
    /// </summary>
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
    function GetAPNSToken: string;
    function GetAuthOptions: TAuthOptions;
    function GetNativeAuthOptions: LongInt;
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
    /// <summary>
    ///   Removes all notifications that are in the notification drawer for this app, and for Android, with the Channel Id
    /// </summary>
    procedure RemoveNotifications;
    procedure SetAuthOptions(const AValue: TAuthOptions);
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
    ///   Options requested for notifications - iOS ONLY
    /// </summary>
    property AuthOptions: TAuthOptions read GetAuthOptions write SetAuthOptions;
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

  TCustomPlatformFCMManager = class(TInterfacedObject)
  private
    FAuthOptions: TAuthOptions;
    FCategories: TNotificationCategories;
    FDeviceID: string;
    FDeviceToken: string;
    FIsForeground: Boolean;
    FIsStarted: Boolean;
    FServiceConnection: TPushServiceConnection;
    FShowBannerIfForeground: Boolean;
    FOnMessageReceived: TMessageReceivedEvent;
    FOnNotificationReceived: TNotificationReceivedEvent;
    FOnStarted: TNotifyEvent;
    FOnStatusChange: TNotifyEvent;
    FOnTokenReceived: TTokenReceivedEvent;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure CheckStartupNotifications;
    procedure CreateConnection;
    procedure ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
    procedure ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
    procedure StatusChange;
    procedure TokenReceived;
  protected
    procedure CreateChannel; virtual;
    procedure DoStart;
    function FindCategory(const AID: string; out ACategory: INotificationCategory): Boolean;
    procedure HandleNotification(const AServiceNotification: TPushServiceNotification);
    procedure Started;
    property Categories: TNotificationCategories read FCategories;
    property IsForeground: Boolean read FIsForeground;
    property ShowBannerIfForeground: Boolean read FShowBannerIfForeground;
    property AuthOptions: TAuthOptions read FAuthOptions;
  public
    function AddCategory(const AID: string; const AHandler: TNotificationCategoryProc;
      const AOptions: TNotificationCategoryOptions = []): INotificationCategory; overload;
    function AddCategory(const AID: string; const AOptions: TNotificationCategoryOptions = []): INotificationCategory; overload;
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod); virtual;
    function GetAPNSToken: string; virtual;
    function GetAuthOptions: TAuthOptions;
    function GetNativeAuthOptions: LongInt; virtual;
    function GetOnMessageReceived: TMessageReceivedEvent;
    function GetOnStarted: TNotifyEvent;
    function GetOnStatusChange: TNotifyEvent;
    function GetOnTokenReceived: TTokenReceivedEvent;
    function GetPushService: TPushService;
    function GetShowBannerIfForeground: Boolean;
    function GetStatus: TPushService.TStatus;
    function GetToken: string;
    function IsStarted: Boolean;
    procedure RemoveNotifications; virtual;
    procedure SetAuthOptions(const AValue: TAuthOptions);
    procedure SetOnMessageReceived(const AValue: TMessageReceivedEvent);
    procedure SetOnStarted(const AValue: TNotifyEvent);
    procedure SetOnStatusChange(const AValue: TNotifyEvent);
    procedure SetOnTokenReceived(const AValue: TTokenReceivedEvent);
    procedure SetShowBannerIfForeground(const AValue: Boolean);
    procedure Start; virtual;
    procedure SubscribeToTopic(const ATopic: string); virtual;
    procedure UnsubscribeFromTopic(const ATopic: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property DeviceID: string read FDeviceID;
    property DeviceToken: string read FDeviceToken;
  end;

var
  FCM: IFCMManager;

implementation

uses
  // RTL
  System.SysUtils, System.Notification, System.IOUtils, System.Permissions, System.StrUtils,
  // FMX
  FMX.Platform,
  // DW
  {$IF Defined(IOS)}
  DW.FCMManager.iOS,
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.FCMManager.Android,
  {$ENDIF}
  DW.OSLog;

{$IF not (Defined(ANDROID) or Defined(IOS))}
type
  TPlatformFCMManager = class(TCustomPlatformFCMManager, IFCMManager);
{$ENDIF}

type
  TNotificationAction = class(TInterfacedObject, INotificationAction)
  private
    FHandler: TNotificationActionProc;
    FID: string;
    FOptions: TNotificationActionOptions;
    FTitle: string;
  public
    { INotificationAction }
    function GetHandler: TNotificationActionProc;
    function GetID: string;
    function GetOptions: TNotificationActionOptions;
    function GetTitle: string;
  public
    constructor Create(const AID, ATitle: string; const AHandler: TNotificationActionProc; const AOptions: TNotificationActionOptions);
  end;

  TNotificationCategory = class(TInterfacedObject, INotificationCategory)
  private
    FActions: TNotificationActions;
    FHandler: TNotificationCategoryProc;
    FID: string;
    FOptions: TNotificationCategoryOptions;
  public
    { INotificationCategory }
    function AddAction(const AID, ATitle: string; const AHandler: TNotificationActionProc;
      const AOptions: TNotificationActionOptions = []): INotificationAction;
    function FindAction(const AID: string; out AAction: INotificationAction): Boolean;
    function GetAction(const AIndex: Integer): INotificationAction;
    function GetActionCount: Integer;
    function GetHandler: TNotificationCategoryProc;
    function GetID: string;
    function GetOptions: TNotificationCategoryOptions;
  public
    constructor Create(const AID: string; const AHandler: TNotificationCategoryProc; const AOptions: TNotificationCategoryOptions);
  end;

{ TNotificationAction }

constructor TNotificationAction.Create(const AID, ATitle: string; const AHandler: TNotificationActionProc;
  const AOptions: TNotificationActionOptions);
begin
  inherited Create;
  FID := AID;
  FTitle := ATitle;
  FHandler := AHandler;
  FOptions := AOptions;
end;

function TNotificationAction.GetHandler: TNotificationActionProc;
begin
  Result := FHandler;
end;

function TNotificationAction.GetID: string;
begin
  Result := FID;
end;

function TNotificationAction.GetOptions: TNotificationActionOptions;
begin
  Result := FOptions;
end;

function TNotificationAction.GetTitle: string;
begin
  Result := FTitle;
end;

{ TNotificationCategory }

constructor TNotificationCategory.Create(const AID: string; const AHandler: TNotificationCategoryProc; const AOptions: TNotificationCategoryOptions);
begin
  inherited Create;
  FID := AID;
  FHandler := AHandler;
  FOptions := AOptions;
end;

function TNotificationCategory.AddAction(const AID, ATitle: string; const AHandler: TNotificationActionProc;
  const AOptions: TNotificationActionOptions): INotificationAction;
begin
  if not FindAction(AID, Result) then
  begin
    Result := TNotificationAction.Create(AID, ATitle, AHandler, AOptions);
    FActions := FActions + [Result];
  end;
end;

function TNotificationCategory.FindAction(const AID: string; out AAction: INotificationAction): Boolean;
var
  LAction: INotificationAction;
begin
  Result := False;
  for LAction in FActions do
  begin
    if LAction.ID.Equals(AID) then
    begin
      AAction := LAction;
      Result := True;
      Break;
    end;
  end;
end;

function TNotificationCategory.GetAction(const AIndex: Integer): INotificationAction;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < GetActionCount) then
    Result := FActions[AIndex];
end;

function TNotificationCategory.GetActionCount: Integer;
begin
  Result := Length(FActions);
end;

function TNotificationCategory.GetHandler: TNotificationCategoryProc;
begin
  Result := FHandler;
end;

function TNotificationCategory.GetID: string;
begin
  Result := FID;
end;

function TNotificationCategory.GetOptions: TNotificationCategoryOptions;
begin
  Result := FOptions;
end;

{ TCustomPlatformFCMManager }

constructor TCustomPlatformFCMManager.Create;
begin
  inherited Create;
  FAuthOptions := [TAuthOption.Badge, TAuthOption.Sound, TAuthOption.Alert];
  FShowBannerIfForeground := True;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TCustomPlatformFCMManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FServiceConnection.Free;
  inherited;
end;

procedure TCustomPlatformFCMManager.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
      FIsForeground := True;
    TApplicationEvent.EnteredBackground:
      FIsForeground := False;
  end;
end;

procedure TCustomPlatformFCMManager.CreateChannel;
begin
  //
end;

procedure TCustomPlatformFCMManager.CreateConnection;
var
  LPushService: TPushService;
begin
  TOSLog.d('TCustomPlatformFCMManager.CreateConnection');
  LPushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.FCM);
  if LPushService <> nil then
  begin
    TOSLog.d('> Creating FServiceConnection..');
    FServiceConnection := TPushServiceConnection.Create(LPushService);
    FServiceConnection.OnChange := ServiceConnectionChangeHandler;
    FServiceConnection.OnReceiveNotification := ReceiveNotificationHandler;
    FDeviceId := LPushService.DeviceIDValue[TPushService.TDeviceIDNames.DeviceId];
    FServiceConnection.Active := True;
    TOSLog.d('> FServiceConnection set to active');
  end
  else
    TOSLog.d('> No PushService??');
end;

function TCustomPlatformFCMManager.AddCategory(const AID: string; const AOptions: TNotificationCategoryOptions): INotificationCategory;
begin
  Result := AddCategory(AID, nil, AOptions);
end;

function TCustomPlatformFCMManager.AddCategory(const AID: string; const AHandler: TNotificationCategoryProc;
  const AOptions: TNotificationCategoryOptions): INotificationCategory;
begin
  if not FindCategory(AID, Result) then
  begin
    Result := TNotificationCategory.Create(AID, AHandler, AOptions);
    FCategories := FCategories + [Result];
  end;
end;

function TCustomPlatformFCMManager.FindCategory(const AID: string; out ACategory: INotificationCategory): Boolean;
var
  LCategory: INotificationCategory;
begin
  Result := False;
  for LCategory in FCategories do
  begin
    if LCategory.ID.Equals(AID) then
    begin
      ACategory := LCategory;
      Result := True;
      Break;
    end;
  end;
end;

procedure TCustomPlatformFCMManager.CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
begin
  //
end;

function TCustomPlatformFCMManager.GetAPNSToken: string;
begin
  Result := '';
end;

function TCustomPlatformFCMManager.GetAuthOptions: TAuthOptions;
begin
  Result := FAuthOptions;
end;

function TCustomPlatformFCMManager.GetNativeAuthOptions: LongInt;
begin
  Result := 0;
end;

function TCustomPlatformFCMManager.GetOnMessageReceived: TMessageReceivedEvent;
begin
  Result := FOnMessageReceived;
end;

function TCustomPlatformFCMManager.GetOnStarted: TNotifyEvent;
begin
  Result := FOnStarted;
end;

function TCustomPlatformFCMManager.GetOnStatusChange: TNotifyEvent;
begin
  Result := FOnStatusChange;
end;

function TCustomPlatformFCMManager.GetOnTokenReceived: TTokenReceivedEvent;
begin
  Result := FOnTokenReceived;
end;

procedure TCustomPlatformFCMManager.SetAuthOptions(const AValue: TAuthOptions);
begin
  FAuthOptions := AValue;
end;

procedure TCustomPlatformFCMManager.SetOnMessageReceived(const AValue: TMessageReceivedEvent);
begin
  FOnMessageReceived := AValue;
end;

procedure TCustomPlatformFCMManager.SetOnStarted(const AValue: TNotifyEvent);
begin
  FOnStarted := AValue;
end;

procedure TCustomPlatformFCMManager.SetOnStatusChange(const AValue: TNotifyEvent);
begin
  FOnStatusChange := AValue;
end;

procedure TCustomPlatformFCMManager.SetOnTokenReceived(const AValue: TTokenReceivedEvent);
begin
  FOnTokenReceived := AValue;
end;

procedure TCustomPlatformFCMManager.SetShowBannerIfForeground(const AValue: Boolean);
begin
  FShowBannerIfForeground := AValue;
end;

procedure TCustomPlatformFCMManager.CheckStartupNotifications;
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

function TCustomPlatformFCMManager.GetPushService: TPushService;
begin
  if FServiceConnection <> nil then
    Result := FServiceConnection.Service
  else
    Result := nil;
end;

function TCustomPlatformFCMManager.GetShowBannerIfForeground: Boolean;
begin
  Result := FShowBannerIfForeground;
end;

function TCustomPlatformFCMManager.GetStatus: TPushService.TStatus;
begin
  Result := TPushService.TStatus.Stopped;
  if (FServiceConnection <> nil) and (FServiceConnection.Service <> nil) then
    Result := FServiceConnection.Service.Status;
end;

function TCustomPlatformFCMManager.IsStarted: Boolean;
begin
  Result := FIsStarted;
end;

function TCustomPlatformFCMManager.GetToken: string;
begin
  Result := FDeviceToken;
end;

procedure TCustomPlatformFCMManager.HandleNotification(const AServiceNotification: TPushServiceNotification);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, procedure begin ReceiveNotificationHandler(Self, AServiceNotification) end)
  else
    ReceiveNotificationHandler(Self, AServiceNotification);
end;

procedure TCustomPlatformFCMManager.ReceiveNotificationHandler(Sender: TObject; const AServiceNotification: TPushServiceNotification);
begin
  if Assigned(FOnNotificationReceived) then
    FOnNotificationReceived(Self, AServiceNotification);
  if AServiceNotification.Json <> nil then
  begin
    if Assigned(FOnMessageReceived) then
      FOnMessageReceived(Self, AServiceNotification.Json);
  end;
end;

procedure TCustomPlatformFCMManager.RemoveNotifications;
begin
  //
end;

procedure TCustomPlatformFCMManager.ServiceConnectionChangeHandler(Sender: TObject; APushChanges: TPushService.TChanges);
var
  LToken: string;
  LNeedsChange: Boolean;
begin
  TOSLog.d('TCustomPlatformFCMManager.ServiceConnectionChangeHandler');
  LNeedsChange := False;
  if TPushService.TChange.DeviceToken in APushChanges then
  begin
    TOSLog.d('> TPushService.TChange.DeviceToken');
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
    TOSLog.d('> TPushService.TChange.Status');
    case FServiceConnection.Service.Status of
      TPushService.TStatus.Started:
      begin
        TOSLog.d('> Started');
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

procedure TCustomPlatformFCMManager.TokenReceived;
begin
  if not FDeviceToken.IsEmpty and Assigned(FOnTokenReceived) then
    FOnTokenReceived(Self, FDeviceToken);
end;

procedure TCustomPlatformFCMManager.Started;
begin
  FIsStarted := True;
  CheckStartupNotifications;
  if Assigned(FOnStarted) then
    FOnStarted(Self);
end;

procedure TCustomPlatformFCMManager.StatusChange;
begin
  TOSLog.d('TCustomPlatformFCMManager.StatusChange');
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

procedure TCustomPlatformFCMManager.DoStart;
begin
  CreateChannel;
  CreateConnection;
end;

procedure TCustomPlatformFCMManager.Start;
begin
  //
end;

procedure TCustomPlatformFCMManager.SubscribeToTopic(const ATopic: string);
begin
  //
end;

procedure TCustomPlatformFCMManager.UnsubscribeFromTopic(const ATopic: string);
begin
  //
end;

{$IF not (Defined(ANDROID) or Defined(IOS))}
initialization
  FCM := TPlatformFCMManager.Create;
{$ENDIF}

end.
