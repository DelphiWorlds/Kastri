unit DW.Firebase.Messaging.Android;

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
  Androidapi.JNI.Os, Androidapi.Helpers,
  // FMX
  FMX.Platform.Android,
  // DW
  DW.OSLog,
  DW.Classes.Helpers, DW.Androidapi.JNI.DWFirebaseServiceHelpers, DW.FirebaseApp.Android,
  {$IF CompilerVersion < 35} DW.Androidapi.JNI.SupportV4, {$ELSE} DW.Androidapi.JNI.Androidx.LocalBroadcastManager, {$ENDIF}
  DW.Androidapi.JNI.Firebase, DW.Androidapi.JNI.Content;

type
  TBundlePair = record
    Key: JObject;
    Value: JObject;
    constructor Create(const AKey, AValue: JObject);
    function KeyAsString: string;
    function ValueAsString: string;
  end;

  TBundlePairs = TArray<TBundlePair>;

  TBundleParser = record
    Pairs: TBundlePairs;
    function Count: Integer;
    procedure Parse(const ABundle: JBundle);
    procedure ToStrings(const AStrings: TStrings);
  end;

{ TBundlePair }

constructor TBundlePair.Create(const AKey, AValue: JObject);
begin
  Key := AKey;
  Value := AValue;
end;

function TBundlePair.KeyAsString: string;
begin
  if Key <> nil then
    Result := JStringToString(Key.toString)
  else
    Result := '';
end;

function TBundlePair.ValueAsString: string;
begin
  if Value <> nil then
    Result := JStringToString(Value.toString)
  else
    Result := '';
end;

{ TBundleParser }

function TBundleParser.Count: Integer;
begin
  Result := Length(Pairs);
end;

procedure TBundleParser.Parse(const ABundle: JBundle);
var
  LIterator: JIterator;
  LKeyObject: JObject;
begin
  Pairs := [];
  LIterator := ABundle.keySet.iterator;
  while LIterator.hasNext do
  begin
    LKeyObject := LIterator.next;
    if LKeyObject <> nil then
      Pairs := Pairs + [TBundlePair.Create(LKeyObject, ABundle.&get(LKeyObject.toString))];
  end;
end;

procedure TBundleParser.ToStrings(const AStrings: TStrings);
var
  LPair: TBundlePair;
begin
  AStrings.Clear;
  for LPair in Pairs do
    AStrings.Values[LPair.KeyAsString] := LPair.ValueAsString;
end;

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

procedure TPlatformFirebaseMessaging.MessageReceivedNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  // "App switching" intent
  FIntent := TMessageReceivedNotification(AMsg).Value;
end;

function TPlatformFirebaseMessaging.Start: Boolean;
begin
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
