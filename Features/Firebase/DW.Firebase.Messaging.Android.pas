unit DW.Firebase.Messaging.Android;

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
  System.Messaging,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero,
  Androidapi.JNI.PlayServices.Tasks,
  // DW
  DW.Firebase.Messaging, DW.Android.Helpers, DW.MultiReceiver.Android;

type
  TPlatformFirebaseMessaging = class;

  TTokenTaskCompleteListener = class(TJavaLocal, JOnCompleteListener)
  private
    FFirebaseMessaging: TPlatformFirebaseMessaging;
  public
    { JOnCompleteListener }
    procedure onComplete(task: JTask); cdecl;
  public
    constructor Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
  end;

  TFirebaseMessagingReceiver = class(TMultiReceiver)
  private
    FFirebaseMessaging: TPlatformFirebaseMessaging;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
  end;

  TPlatformFirebaseMessaging = class(TCustomPlatformFirebaseMessaging)
  private
    FFirebaseMessagingReceiver: TFirebaseMessagingReceiver;
    FIntent: JIntent;
    FStartupIntentHandled: Boolean;
    FTokenTaskCompleteListener: JOnCompleteListener;
    procedure CreateChannel;
    procedure MessageReceivedNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
  protected
    procedure DoApplicationBecameActive; override;
    procedure DoApplicationEnteredBackground; override;
    procedure HandleMessageReceived(const data: JIntent; const AIsStartup: Boolean = False);
    procedure HandleNewToken(const data: JIntent);
    procedure PublishNotification(const data: JIntent);
    procedure RequestPermissions; override;
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
  System.SysUtils, System.Classes,
  // Android
  Androidapi.JNI.Os, Androidapi.Helpers, Androidapi.JNI.App,
  // FMX
  FMX.Platform.Android,
  // DW
  DW.OSLog,
  DW.Classes.Helpers, DW.Androidapi.JNI.DWFirebaseServiceHelpers, DW.FirebaseApp.Android, DW.Consts.Android, DW.OSMetadata,
  {$IF CompilerVersion < 35} DW.Androidapi.JNI.SupportV4, {$ELSE} DW.Androidapi.JNI.Androidx.LocalBroadcastManager, {$ENDIF}
  DW.Androidapi.JNI.Firebase, DW.Androidapi.JNI.Content;

{ TTokenTaskCompleteListener }

constructor TTokenTaskCompleteListener.Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

procedure TTokenTaskCompleteListener.onComplete(task: JTask);
begin
  if task.isSuccessful then
    FFirebaseMessaging.DoTokenReceived(JStringToString(TJString.Wrap(task.getResult)));
end;

{ TFirebaseMessagingReceiver }

constructor TFirebaseMessagingReceiver.Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
begin
  inherited Create(True);
  FFirebaseMessaging := AFirebaseMessaging;
end;

procedure TFirebaseMessagingReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJDWFirebaseMessagingService.JavaClass.ACTION_NEW_TOKEN);
  IntentFilter.addAction(TJDWFirebaseMessagingService.JavaClass.ACTION_MESSAGE_RECEIVED);
end;

procedure TFirebaseMessagingReceiver.Receive(context: JContext; intent: JIntent);
begin
  if intent <> nil then
  begin
    if intent.getAction.compareTo(TJDWFirebaseMessagingService.JavaClass.ACTION_NEW_TOKEN) = 0 then
      FFirebaseMessaging.HandleNewToken(intent)
    else if intent.getAction.compareTo(TJDWFirebaseMessagingService.JavaClass.ACTION_MESSAGE_RECEIVED) = 0 then
      FFirebaseMessaging.HandleMessageReceived(intent);
  end;
end;

{ TPlatformFirebaseMessaging }

constructor TPlatformFirebaseMessaging.Create(const AFirebaseMessaging: TFirebaseMessaging);
begin
  inherited;
  // Startup intent
  FIntent := MainActivity.getIntent;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationMessageHandler);
  FFirebaseMessagingReceiver := TFirebaseMessagingReceiver.Create(Self);
end;

destructor TPlatformFirebaseMessaging.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedNotificationMessageHandler);
  FFirebaseMessagingReceiver.Free;
  inherited;
end;

procedure TPlatformFirebaseMessaging.CreateChannel;
var
  LChannelId: string;
  LChannel: JNotificationChannel;
begin
  // Creates a channel only if one is named in the project options
  if TOSMetadata.GetValue(cMetadataFCMDefaultChannelId, LChannelId) then
  begin
    LChannel := TJNotificationChannel.JavaClass.init(StringToJString(LChannelId), StrToJCharSequence(LChannelId),
      TJNotificationManager.JavaClass.IMPORTANCE_HIGH);
    LChannel.enableLights(True);
    LChannel.enableVibration(True);
    // Conceal info only on secure lock screens
    LChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
    TAndroidHelperEx.NotificationManager.createNotificationChannel(LChannel);
  end;
end;

procedure TPlatformFirebaseMessaging.MessageReceivedNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  // "App switching" intent
  FIntent := TMessageReceivedNotification(AMsg).Value;
end;

function TPlatformFirebaseMessaging.Start: Boolean;
begin
  if TOSVersion.Check(8) then
    CreateChannel;
  TPlatformFirebaseApp.Start;
  {$IF CompilerVersion < 35}
  TJDWFirebaseMessagingService.JavaClass.queryToken(TAndroidHelper.Context);
  {$ELSE}
  FTokenTaskCompleteListener := TTokenTaskCompleteListener.Create(Self);
  TJFirebaseMessaging.JavaClass.getInstance.getToken.addOnCompleteListener(FTokenTaskCompleteListener);
  {$ENDIF}
  Result := True;
end;

procedure TPlatformFirebaseMessaging.DoApplicationBecameActive;
begin
  if not FStartupIntentHandled then
  begin
    if FIntent <> nil then
      HandleMessageReceived(FIntent, True);
    FStartupIntentHandled := True;
  end;
end;

procedure TPlatformFirebaseMessaging.DoApplicationEnteredBackground;
begin
  FStartupIntentHandled := False;
end;

procedure TPlatformFirebaseMessaging.HandleMessageReceived(const data: JIntent; const AIsStartup: Boolean = False);
const
  cGCMMessageIDKey = 'gcm.message_id';
  cGoogleMessageIDKey = 'google.message_id';
var
  LPayload: TStrings;
  LBundle: JBundle;
  LParser: TBundleParser;
begin
  LPayload := TStringList.Create;
  try
    LBundle := data.getExtras;
    if LBundle <> nil then
    begin
      LParser.Parse(LBundle);
      if (LParser.Count = 1) and LParser.Pairs[0].KeyAsString.Equals('fcm') then
        LParser.Parse(TJBundle.Wrap(LParser.Pairs[0].Value));
      LParser.ToStrings(LPayload);
    end;
    if (LPayload.IndexOfName(cGCMMessageIDKey) > -1) or (LPayload.IndexOfName(cGoogleMessageIDKey) > -1) then
    begin
      if not AIsStartup and (ShowBannerWhenForeground or not IsForeground) then
        PublishNotification(data);
      TThread.Synchronize(nil,
        procedure
        begin
          DoMessageReceived(LPayload);
        end
      );
    end;
  finally
    LPayload.Free;
  end;
end;

procedure TPlatformFirebaseMessaging.PublishNotification(const data: JIntent);
begin
  TDo.Run(
    procedure
    begin
      TJDWNotificationPublisher.JavaClass.sendNotification(TAndroidHelper.Context, data, True);
    end
  );
end;

procedure TPlatformFirebaseMessaging.HandleNewToken(const data: JIntent);
var
  LToken: string;
begin
  LToken := JStringToString(data.getStringExtra(StringToJString('token')));
  TThread.Queue(nil,
    procedure
    begin
      DoTokenReceived(LToken);
    end
  );
end;

procedure TPlatformFirebaseMessaging.RequestPermissions;
begin
  DoAuthorizationResult(True); // i.e. a NOP on Android
end;

procedure TPlatformFirebaseMessaging.SubscribeToTopic(const ATopicName: string);
begin
  TJFirebaseMessaging.JavaClass.getInstance.subscribeToTopic(StringToJString(ATopicName));
end;

procedure TPlatformFirebaseMessaging.UnsubscribeFromTopic(const ATopicName: string);
begin
  TJFirebaseMessaging.JavaClass.getInstance.unsubscribeFromTopic(StringToJString(ATopicName));
end;

end.
