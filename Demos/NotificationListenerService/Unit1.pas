unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  Androidapi.JNI.App,
  DW.NotificationListenerServiceReceiver.Android;  // Requires dw-kastri-base-2.0.0.jar

type
  TForm1 = class(TForm)
    NotificationsMemo: TMemo;
  private
    FReceiver: TNotificationListenerServiceReceiver;
    FHasShownRationale: Boolean;
    FIsShowingPermissions: Boolean;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure CheckNotificationListenerPermissions;
    function HasNotificationListenerPermissions: Boolean;
    procedure LogMessage(const AMsg: string);
    procedure NotificationListenerServiceReceiverNotificationHandler(Sender: TObject; const ANotification: JNotification;
      const AEventKind: TNotificationEventKind);
    procedure ShowNotificationListenerPermissions;
    procedure ShowNotificationListenerPermissionsRationale;
    procedure StartNotificationListenerService;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  DW.OSLog,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  FMX.Platform, FMX.DialogService.Async,
  DW.Android.Helpers, DW.OSDevice;

const
  cNotificationListenerServiceName = 'com.delphiworlds.kastri.DWNotificationListenerService';
  cNotificationListenerPermissionsMessageAndroid8 = 'This application requires access to notifications in order to work, so the ' +
    'Notification access screen will be shown next. Please:'#13#10#13#10'Switch the switch next to %s in the list, tap "Allow", then ' +
    'tap the back arrow to return to %s';
  cNotificationListenerPermissionsMessage = 'This application requires access to notifications in order to work, so the Device & App ' +
    'Notifications screen will be shown next. Please:'#13#10#13#10'Tap %s in the "Not Allowed" list, switch the "Allow notification access" ' +
    'switch, tap "Allow", then tap the back arrow at the top of the screen twice to return to %s';

function GetBundleValue(const ABundle: JBundle; const AKey: string; var AValue: string): Boolean;
var
  LKey: JString;
  LObject: JObject;
  LClassName: string;
begin
  Result := False;
  LKey := StringToJString(AKey);
  if ABundle.containsKey(LKey) then
  begin
    LObject := ABundle.get(LKey);
    if LObject <> nil then
    begin
      LClassName := JStringToString(LObject.getClass.getSimpleName);
      if LClassName.Equals('String') then
        AValue := JStringToString(TJString.Wrap(LObject))
      // SpannableString is compatible with CharSequence
      else if LClassName.Equals('SpannableString') then
        AValue := JCharSequenceToStr(ABundle.getCharSequence(LKey))
      else
        AValue := JStringToString(LObject.toString);
      Result := True;
    end;
  end;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  // Creating a receiver that receives local broadcasts from the notifier service
  FReceiver := TNotificationListenerServiceReceiver.Create;
  FReceiver.OnNotification := NotificationListenerServiceReceiverNotificationHandler;
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FReceiver.Free;
  inherited;
end;

function TForm1.HasNotificationListenerPermissions: Boolean;
begin
  // Checks whether or not the user has granted permissions to listen for notifications
  Result := TAndroidHelperEx.HasSecurePermissions('enabled_notification_listeners');
end;

procedure TForm1.LogMessage(const AMsg: string);
begin
  NotificationsMemo.Lines.Add(AMsg);
end;

procedure TForm1.NotificationListenerServiceReceiverNotificationHandler(Sender: TObject; const ANotification: JNotification;
  const AEventKind: TNotificationEventKind);
const
  cEventKindCaptions: array[TNotificationEventKind] of string = ('Status unknown', 'Posted', 'Removed');
var
  LBundleValue: string;
  LBundleParser: TBundleParser;
  LBundlePair: TBundlePair;
begin
  LogMessage(Format('Notification - %s:', [cEventKindCaptions[AEventKind]]));
  LBundleValue := '';
  if GetBundleValue(ANotification.extras, 'android.title', LBundleValue) then
    LogMessage(Format('Title: %s', [LBundleValue]));
  if GetBundleValue(ANotification.extras, 'android.conversationTitle', LBundleValue) then
    LogMessage(Format('ConversationTitle: %s', [LBundleValue]));
  if GetBundleValue(ANotification.extras, 'android.text', LBundleValue) then
    LogMessage(Format('Text: %s', [LBundleValue]));
  if GetBundleValue(ANotification.extras, 'android.subText', LBundleValue) then
    LogMessage(Format('SubText: %s', [LBundleValue]));
  // Uncomment these lines if you are interested in the complete information being sent
  // LogMessage('Complete bundle:');
  // LBundleParser.Parse(ANotification.extras);
  // for LBundlePair in LBundleParser.Pairs do
  //   LogMessage(Format('%s: %s', [LBundlePair.KeyAsString, LBundlePair.ValueAsString]));
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  // When the settings activity is closed and this app becomes active, a check is made to ensure the user actually granted permissions
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      TOSLog.d('TApplicationEvent.BecameActive');
      if FIsShowingPermissions then
        CheckNotificationListenerPermissions
      else if not HasNotificationListenerPermissions then
        ShowNotificationListenerPermissionsRationale
      else
        StartNotificationListenerService;
      FIsShowingPermissions := False;
    end;
  end;
end;

procedure TForm1.CheckNotificationListenerPermissions;
begin
  // If the user granted permissions, start the notifier service
  if HasNotificationListenerPermissions then
    StartNotificationListenerService;
end;

procedure TForm1.ShowNotificationListenerPermissionsRationale;
var
  LRationaleMessage, LAppName: string;
begin
  // Show the rationale only once per application run
  if not FHasShownRationale then
  begin
    FHasShownRationale := True;
    LAppName := TOSDevice.GetPackageName;
    if not LAppName.IsEmpty then
      LAppName := LAppName.Substring(LAppName.LastIndexOf('.') + 1)
    else
      LAppName := 'Unknown';
    if TOSVersion.Check(9) then
      LRationaleMessage := Format(cNotificationListenerPermissionsMessage, [LAppName, LAppName])
    else
      LRationaleMessage := Format(cNotificationListenerPermissionsMessageAndroid8, [LAppName, LAppName]);
    TDialogServiceAsync.MessageDialog(LRationaleMessage, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        ShowNotificationListenerPermissions;
      end
    );
  end;
end;

procedure TForm1.ShowNotificationListenerPermissions;
var
  LIntent: JIntent;
begin
  // Show the notification permissions activity
  LIntent := TJIntent.JavaClass.init(StringToJString('android.settings.ACTION_NOTIFICATION_LISTENER_SETTINGS'));
  TAndroidHelper.Context.startActivity(LIntent);
  // Setting this flag to indicate that it was shown when this application becomes active again
  FIsShowingPermissions := True;
end;

procedure TForm1.StartNotificationListenerService;
var
  LIntent: JIntent;
begin
  // Start the notifier service if it's not already running.
  // The service just listens for notifications being posted and removed, and makes a local broadcast that this app can listen for
  if not TAndroidHelperEx.IsServiceRunning(cNotificationListenerServiceName) then
  begin
    LIntent := TJIntent.JavaClass.init;
    LIntent.setClassName(TAndroidHelper.Context, StringToJString(cNotificationListenerServiceName));
    TAndroidHelper.Context.startService(LIntent);
  end;
end;

end.
