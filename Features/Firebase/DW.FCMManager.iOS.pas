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
  Macapi.Helpers,
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
    procedure CheckPushEnabledCompletionHandler(settings: UNNotificationSettings);
    procedure PushDeviceTokenMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure SubscribeToTopicCompletionHandler(error: NSError);
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
  if not TUserDefaults.GetValue('APNS').IsEmpty then
    TOSLog.d('APNS: %s', [TUserDefaults.GetValue('APNS')]);
end;

destructor TPlatformFCMManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TPushDeviceTokenMessage, PushDeviceTokenMessageHandler);
  inherited;
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
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.removeAllDeliveredNotifications;
end;

procedure TPlatformFCMManager.CheckPushEnabled(const AHandler: TCheckPushEnabledMethod);
begin
  FCheckPushEnabledHandler := AHandler;
  TUNUserNotificationCenter.OCClass.currentNotificationCenter.getNotificationSettingsWithCompletionHandler(CheckPushEnabledCompletionHandler);
end;

procedure TPlatformFCMManager.CheckPushEnabledCompletionHandler(settings: UNNotificationSettings);
var
  LIsPushEnabled: Boolean;
begin
  LIsPushEnabled := settings.authorizationStatus in
    [UNAuthorizationStatusAuthorized, UNAuthorizationStatusProvisional, UNAuthorizationStatusEphemeral];
  TThread.Queue(nil, procedure begin FCheckPushEnabledHandler(LIsPushEnabled) end);
end;

procedure TPlatformFCMManager.Start;
begin
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
