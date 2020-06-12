unit DW.Firebase.Messaging.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero,
  // DW
  DW.Firebase.Messaging, DW.Android.Helpers;

type
  TPlatformFirebaseMessaging = class;

  TFirebaseMessagingReceiverListener = class(TJavaLocal, JFMXBroadcastReceiverListener)
  private
    FFirebaseMessaging: TPlatformFirebaseMessaging;
  public
    { JFMXBroadcastReceiverListener }
    procedure onReceive(context: JContext; intent: JIntent); cdecl;
  public
    constructor Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
  end;

  TPlatformFirebaseMessaging = class(TCustomPlatformFirebaseMessaging)
  private
    FFirebaseMessagingBroadcastReceiver: JFMXBroadcastReceiver;
    FFirebaseMessagingReceiverListener: TFirebaseMessagingReceiverListener;
    FStartupIntentHandled: Boolean;
  protected
    procedure Connect; override;
    procedure Disconnect; override;
    procedure DoApplicationBecameActive; override;
    procedure HandleMessageReceived(const data: JIntent; const AIsStartup: Boolean = False);
    procedure HandleNewToken(const data: JIntent);
    procedure PublishNotification(const data: JIntent);
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
  System.SysUtils, System.Classes,
  // Android
  Androidapi.JNI.Os, Androidapi.Helpers,
  // FMX
  FMX.Platform.Android,
  // DW
  DW.OSLog,
  DW.Classes.Helpers, DW.Androidapi.JNI.DWFirebaseServiceHelpers, DW.FirebaseApp.Android,
  DW.Androidapi.JNI.SupportV4, DW.Androidapi.JNI.Firebase, DW.Androidapi.JNI.Content;

{ TFirebaseMessagingReceiverListener }

constructor TFirebaseMessagingReceiverListener.Create(const AFirebaseMessaging: TPlatformFirebaseMessaging);
begin
  inherited Create;
  FFirebaseMessaging := AFirebaseMessaging;
end;

procedure TFirebaseMessagingReceiverListener.onReceive(context: JContext; intent: JIntent);
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
  FFirebaseMessagingReceiverListener := TFirebaseMessagingReceiverListener.Create(Self);
  FFirebaseMessagingBroadcastReceiver := TJFMXBroadcastReceiver.JavaClass.init(FFirebaseMessagingReceiverListener);
end;

destructor TPlatformFirebaseMessaging.Destroy;
begin
  FFirebaseMessagingReceiverListener.Free;
  FFirebaseMessagingBroadcastReceiver := nil;
  inherited;
end;

function TPlatformFirebaseMessaging.Start: Boolean;
var
  LIntentFilter: JIntentFilter;
begin
  TPlatformFirebaseApp.Start;
  LIntentFilter := TJIntentFilter.JavaClass.init;
  LIntentFilter.addAction(TJDWFirebaseMessagingService.JavaClass.ACTION_NEW_TOKEN);
  LIntentFilter.addAction(TJDWFirebaseMessagingService.JavaClass.ACTION_MESSAGE_RECEIVED);
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).registerReceiver(FFirebaseMessagingBroadcastReceiver, LIntentFilter);
  Result := True;
end;

procedure TPlatformFirebaseMessaging.Connect;
begin
  IsConnected := True;
  TJDWFirebaseMessagingService.JavaClass.queryToken(TAndroidHelper.Context);
end;

procedure TPlatformFirebaseMessaging.Disconnect;
begin
  IsConnected := False;
end;

procedure TPlatformFirebaseMessaging.DoApplicationBecameActive;
begin
  if not FStartupIntentHandled then
  begin
    HandleMessageReceived(MainActivity.getIntent, True);
    FStartupIntentHandled := True;
  end;
end;

procedure TPlatformFirebaseMessaging.HandleMessageReceived(const data: JIntent; const AIsStartup: Boolean = False);
const
  cGCMMessageIDKey = 'gcm.message_id';
var
  LPayload: TStrings;
  LBundle: JBundle;
  LIterator: JIterator;
  LKeyObject: JObject;
  LValueObject: JObject;
  LValue: string;
begin
  TOSLog.d('+TPlatformFirebaseMessaging.HandleMessageReceived');
  LPayload := TStringList.Create;
  try
    LBundle := data.getExtras;
    if LBundle <> nil then
    begin
      LIterator := LBundle.keySet.iterator;
      while LIterator.hasNext do
      begin
        LKeyObject := LIterator.next;
        if LKeyObject = nil then
          Continue;
        LValueObject := LBundle.&get(LKeyObject.toString);
        if LValueObject = nil then
          LValue := ''
        else
          LValue := JStringToString(LValueObject.toString);
        LPayload.Values[JStringToString(LKeyObject.toString)] := LValue;
      end;
    end;
    if LPayload.IndexOfName(cGCMMessageIDKey) > -1 then
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
  TOSLog.d('-TPlatformFirebaseMessaging.HandleMessageReceived');
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

procedure TPlatformFirebaseMessaging.RequestAuthorization;
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
