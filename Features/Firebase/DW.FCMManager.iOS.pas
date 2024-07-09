unit DW.FCMManager.iOS;

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

implementation

uses
  // RTL
  System.Classes, System.PushNotification, System.Messaging, System.SysUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjectiveC,
  // iOS
  iOSapi.UserNotifications, iOSapi.Foundation,
  // FMX
  // ** NOTE: As at Delphi 11.x and Delphi 12, FCMManager requires the following unit to be patched. **
  // Please refer to the readme, here: https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted
  FMX.PushNotification.FCM.iOS,
  FMX.Platform, // Application event messsage
  DW.OSLog,
  DW.UserDefaults.iOS, DW.iOSapi.FirebaseMessaging, DW.iOSapi.FirebaseCore, DW.Firebase.Common.iOS,
  DW.FCMManager;

const
  UNAuthorizationStatusEphemeral = 4;

type
  TPlatformFCMManager = class(TCustomPlatformFCMManager, IFCMManager)
  private
    FCheckPushEnabledHandler: TCheckPushEnabledMethod;
    FMessaging: FIRMessaging;
    procedure AddCategories;
    procedure CheckPushEnabledCompletionHandler(settings: UNNotificationSettings);
    function GetNativeActionOptions(const AOptions: TNotificationActionOptions): UNNotificationActionOptions;
    function GetNativeCategoryOptions(const AOptions: TNotificationCategoryOptions): UNNotificationCategoryOptions;
    function NotificationCenter: UNUserNotificationCenter;
    procedure PushDeviceTokenMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure SubscribeToTopicCompletionHandler(error: NSError);
    procedure UNNotificationResponseMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure UnsubscribeFromTopicCompletionHandler(error: NSError);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckPushEnabled(const AHandler: TCheckPushEnabledMethod); override;
    function GetAPNSToken: string; override;
    function GetNativeAuthOptions: LongInt; override;
    procedure RemoveNotifications; override;
    procedure Start; override;
    procedure SubscribeToTopic(const ATopic: string); override;
    procedure UnsubscribeFromTopic(const ATopic: string); override;
  end;

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

{ TPlatformFCMManager }

constructor TPlatformFCMManager.Create;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TMessage<UNNotificationResponse>, UNNotificationResponseMessageHandler);
  if not TUserDefaults.GetValue('APNS').IsEmpty then
    TOSLog.d('APNS: %s', [TUserDefaults.GetValue('APNS')]);
end;

destructor TPlatformFCMManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TMessage<UNNotificationResponse>, UNNotificationResponseMessageHandler);
  inherited;
end;

function TPlatformFCMManager.NotificationCenter: UNUserNotificationCenter;
begin
  Result := TUNUserNotificationCenter.OCClass.currentNotificationCenter;
end;

function TPlatformFCMManager.GetAPNSToken: string;
begin
  Result := TUserDefaults.GetValue('APNS');
end;

function TPlatformFCMManager.GetNativeAuthOptions: LongInt;
const
  cAuthOptionValues: array[TAuthOption] of UNAuthorizationOptions = (
    UNAuthorizationOptionBadge, UNAuthorizationOptionSound, UNAuthorizationOptionAlert, UNAuthorizationOptionCarPlay,
    UNAuthorizationOptionCriticalAlert, UNAuthorizationOptionProvidesAppNotificationSettings, UNAuthorizationOptionProvisional
  );
var
  LOption: TAuthOption;
begin
  Result := 0;
  for LOption := Low(TAuthOption) to High(TAuthOption) do
  begin
    if LOption in AuthOptions then
      Result := Result or cAuthOptionValues[LOption];
  end;
end;

procedure TPlatformFCMManager.PushDeviceTokenMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  TUserDefaults.SetValue('APNS', TPushDeviceTokenMessage(AMsg).Value.Token);
  TOSLog.d('Received APNS of: %s', [TUserDefaults.GetValue('APNS')]);
  if not IsStarted then
    Started;
end;

procedure TPlatformFCMManager.RemoveNotifications;
begin
  NotificationCenter.removeAllDeliveredNotifications;
end;

procedure TPlatformFCMManager.CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
begin
  FCheckPushEnabledHandler := AHandler;
  NotificationCenter.getNotificationSettingsWithCompletionHandler(CheckPushEnabledCompletionHandler);
end;

procedure TPlatformFCMManager.CheckPushEnabledCompletionHandler(settings: UNNotificationSettings);
var
  LIsPushEnabled: Boolean;
begin
  LIsPushEnabled := settings.authorizationStatus in
    [UNAuthorizationStatusAuthorized, UNAuthorizationStatusProvisional, UNAuthorizationStatusEphemeral];
  TThread.Queue(nil, procedure begin FCheckPushEnabledHandler(LIsPushEnabled) end);
end;

function TPlatformFCMManager.GetNativeActionOptions(const AOptions: TNotificationActionOptions): UNNotificationActionOptions;
const
  cActionOptions: array[TNotificationActionOption] of UNNotificationActionOptions = (
    UNNotificationActionOptionAuthenticationRequired,
    UNNotificationActionOptionDestructive,
    UNNotificationActionOptionForeground
  );
var
  LOption: TNotificationActionOption;
begin
  Result := 0;
  for LOption := Low(TNotificationActionOption) to High(TNotificationActionOption) do
  begin
    if LOption in AOptions then
      Result := Result or cActionOptions[LOption];
  end;
end;

function TPlatformFCMManager.GetNativeCategoryOptions(const AOptions: TNotificationCategoryOptions): UNNotificationCategoryOptions;
const
  UNNotificationCategoryOptionAllowAnnouncement = 16;
  cCategoryOptions: array[TNotificationCategoryOption] of UNNotificationCategoryOptions = (
    UNNotificationCategoryOptionCustomDismissAction,
    UNNotificationCategoryOptionAllowInCarPlay,
    UNNotificationCategoryOptionHiddenPreviewsShowTitle,
    UNNotificationCategoryOptionHiddenPreviewsShowSubtitle,
    UNNotificationCategoryOptionAllowAnnouncement
  );
var
  LOption: TNotificationCategoryOption;
begin
  Result := 0;
  for LOption := Low(TNotificationCategoryOption) to High(TNotificationCategoryOption) do
  begin
    if LOption in AOptions then
      Result := Result or cCategoryOptions[LOption];
  end;
end;

procedure TPlatformFCMManager.AddCategories;
var
  LCategory: INotificationCategory;
  LAction: INotificationAction;
  LNativeCategory: UNNotificationCategory;
  LNativeAction: UNNotificationAction;
  LNativeActions, LNativeCategories: NSMutableArray;
  I: Integer;
begin
  LNativeCategories := TNSMutableArray.Create;
  for LCategory in Categories do
  begin
    LNativeActions := TNSMutableArray.Create;
    for I := 0 to LCategory.ActionCount - 1 do
    begin
      LAction := LCategory.Actions[I];
      LNativeAction := TUNNotificationAction.OCClass.actionWithIdentifier(StrToNSStr(LAction.ID), StrToNSStr(LAction.Title),
        GetNativeActionOptions(LAction.Options));
      LNativeActions.addObject(NSObjectToID(LNativeAction));
    end;
    LNativeCategory := TUNNotificationCategory.OCClass.categoryWithIdentifier(StrToNSStr(LCategory.ID), LNativeActions, nil,
      GetNativeCategoryOptions(LCategory.Options));
    LNativeCategories.addObject(NSObjectToID(LNativeCategory));
  end;
  NotificationCenter.setNotificationCategories(TNSSet.Wrap(TNSSet.OCClass.setWithArray(LNativeCategories)));
end;

procedure TPlatformFCMManager.Start;
begin
  if Length(Categories) > 0 then
    AddCategories;
  TFirebaseCommon.Configure;
  FMessaging := TFIRMessaging.Wrap(TFIRMessaging.OCClass.messaging);
  DoStart;
end;

procedure TPlatformFCMManager.SubscribeToTopic(const ATopic: string);
begin
  FMessaging.setAPNSToken(HexStringToNSData(TUserDefaults.GetValue('APNS')));
  FMessaging.subscribeToTopic(StrToNSStr(ATopic), SubscribeToTopicCompletionHandler);
end;

procedure TPlatformFCMManager.SubscribeToTopicCompletionHandler(error: NSError);
begin
  if error <> nil then
    TOSLog.d('> Error - %d: %s', [error.code, NSStrToStr(error.localizedDescription)]);
end;

procedure TPlatformFCMManager.UNNotificationResponseMessageHandler(const Sender: TObject; const AMsg: TMessage);
var
  LResponse: UNNotificationResponse;
  LCategory: INotificationCategory;
  LAction: INotificationAction;

  LID: string;
begin
  LResponse := TMessage<UNNotificationResponse>(AMsg).Value;
  LID := NSStrToStr(LResponse.notification.request.content.categoryIdentifier);
  if FindCategory(NSStrToStr(LResponse.notification.request.content.categoryIdentifier), LCategory) then
  begin
    LID := NSStrToStr(LResponse.actionIdentifier);
    if LCategory.FindAction(NSStrToStr(LResponse.actionIdentifier), LAction) and Assigned(LAction.Handler) then
      LAction.Handler()
    else if Assigned(LCategory.Handler) then
      LCategory.Handler();
  end;
end;

procedure TPlatformFCMManager.UnsubscribeFromTopic(const ATopic: string);
begin
  FMessaging.setAPNSToken(HexStringToNSData(TUserDefaults.GetValue('APNS')));
  FMessaging.unsubscribeFromTopic(StrToNSStr(ATopic), UnsubscribeFromTopicCompletionHandler);
end;

procedure TPlatformFCMManager.UnsubscribeFromTopicCompletionHandler(error: NSError);
begin
  if error <> nil then
    TOSLog.d('> Error - %d: %s', [error.code, NSStrToStr(error.localizedDescription)]);
end;

initialization
  FCM := TPlatformFCMManager.Create;

end.
