unit DW.NFC.Android;

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


// ****** NOTE: This is a work in progress, so don't expect miracles :-) *****

//

// A very big thank you to Brian Long and his article:

//   http://blong.com/Articles/DelphiXE7NFC/NFC.htm

// On which a lot of the work in this unit is based

interface

uses
  // RTL
  System.Messaging,
  // Android
  Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.NFC, DW.Androidapi.JNI.Nfc;

type
  TPlatformNFCReader = class(TCustomPlatformNFCReader)
  private
    class var FNfcAdapter: JNfcAdapter;
  private
    FPendingIntent: JPendingIntent;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    procedure EnableForegroundDispatch;
    procedure HandleNfcIntent(const AIntent: JIntent);
    procedure MessageReceivedNotificationHandler(const Sender: TObject; const M: TMessage);
    procedure RequestEnableNfc;
  protected
    procedure BeginSession; override;
    procedure EndSession; override;
  public
    class function IsSupported: Boolean; override;
  public
    constructor Create(const ANFCReader: TNFCReader); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,
  // FMX
  FMX.Platform.Android, FMX.Platform,
  // DW
  DW.Toast.Android, DW.OSLog;

function JavaBytesToString(const ABytes: TJavaArray<Byte>): string;
begin
  Result := TEncoding.UTF8.GetString(TAndroidHelper.TJavaArrayToTBytes(ABytes));
end;

{ TPlatformNFCReader }

constructor TPlatformNFCReader.Create(const ANFCReader: TNFCReader);
var
  LIntent: JIntent;
begin
  inherited;
  TOSLog.d('+TPlatformNFCReader.Create');
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  MainActivity.registerIntentAction(TJNfcAdapter.JavaClass.ACTION_NDEF_DISCOVERED);
  MainActivity.registerIntentAction(TJNfcAdapter.JavaClass.ACTION_TECH_DISCOVERED);
  MainActivity.registerIntentAction(TJNfcAdapter.JavaClass.ACTION_TAG_DISCOVERED);
  LIntent := TJIntent.JavaClass.init(TAndroidHelper.Context, TAndroidHelper.Activity.getClass);
  FPendingIntent := TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, 0, LIntent.addFlags(TJIntent.JavaClass.FLAG_ACTIVITY_SINGLE_TOP), 0);
  TOSLog.d('-TPlatformNFCReader.Create');
end;

destructor TPlatformNFCReader.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

class function TPlatformNFCReader.IsSupported: Boolean;
begin
  TOSLog.d('+TPlatformNFCReader.IsSupported');
  FNfcAdapter := TJNfcAdapter.JavaClass.getDefaultAdapter(TAndroidHelper.Context);
  Result := FNfcAdapter <> nil;
  TOSLog.d('-TPlatformNFCReader.IsSupported');
end;

procedure TPlatformNFCReader.EnableForegroundDispatch;
var
  LEnv: PJniEnv;
  LAdapterClass: JNIClass;
  LNfcAdapterObject, LPendingIntentObject: JNIObject;
  LMethodID: JNIMethodID;
  LJNIValues: TJNIValueArray;
begin
  TOSLog.d('+TPlatformNFCReader.EnableForegroundDispatch');
  LEnv := TJNIResolver.GetJNIEnv;
  LNfcAdapterObject := (FNfcAdapter as ILocalObject).GetObjectID;
  LPendingIntentObject := (FPendingIntent as ILocalObject).GetObjectID;
  LAdapterClass := LEnv^.GetObjectClass(LEnv, LNfcAdapterObject);
  LMethodID := LEnv^.GetMethodID(LEnv, LAdapterClass, 'enableForegroundDispatch',
    '(Landroid/app/Activity;Landroid/app/PendingIntent;[Landroid/content/IntentFilter;[[Ljava/lang/String;)V');
  LEnv^.DeleteLocalRef(LEnv, LAdapterClass);
  LJNIValues := ArgsToJNIValues([JavaContext, LPendingIntentObject, nil, nil]);
  LEnv^.CallVoidMethodA(LEnv, LNfcAdapterObject, LMethodID, PJNIValue(LJNIValues));
  TOSLog.d('-TPlatformNFCReader.EnableForegroundDispatch');
end;

procedure TPlatformNFCReader.BeginSession;
begin
  IsActive := False;
  if FNfcAdapter.isEnabled then
  begin
    EnableForegroundDispatch;
    IsActive := True;
  end
  else
    RequestEnableNfc;
end;

procedure TPlatformNFCReader.EndSession;
begin
  FNfcAdapter.disableForegroundDispatch(TAndroidHelper.Activity);
  IsActive := False;
end;

procedure TPlatformNFCReader.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
var
  LIntent: JIntent;
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      LIntent := TAndroidHelper.Activity.getIntent;
      if (LIntent <> nil) and not TJIntent.JavaClass.ACTION_MAIN.equals(LIntent.getAction) then
      begin
        TOSLog.d('TPlatformNFCReader.ApplicationEventMessageHandler HandleNfcIntent');
        HandleNfcIntent(LIntent);
      end;
    end;
  end;
end;

procedure TPlatformNFCReader.MessageReceivedNotificationHandler(const Sender: TObject; const M: TMessage);
var
  LIntent: JIntent;
begin
  LIntent := TMessageReceivedNotification(M).Value;
  if LIntent <> nil then
  begin
    if TJNfcAdapter.JavaClass.ACTION_NDEF_DISCOVERED.equals(LIntent.getAction) or
       TJNfcAdapter.JavaClass.ACTION_TECH_DISCOVERED.equals(LIntent.getAction) or
       TJNfcAdapter.JavaClass.ACTION_TAG_DISCOVERED.equals(LIntent.getAction) then
    begin
      TOSLog.d('TPlatformNFCReader.MessageReceivedNotificationHandler HandleNfcIntent');
      HandleNfcIntent(LIntent);
    end;
  end;
end;

procedure TPlatformNFCReader.RequestEnableNfc;
begin
  TToast.Make('Please enable NFC and press Back to return to the application', False);
  if TJBuild_VERSION.JavaClass.SDK_INT > 16 then
    TAndroidHelper.Activity.startActivity(TJIntent.JavaClass.init(StringToJString('android.settings.NFC_SETTINGS')))
  else
    TAndroidHelper.Activity.startActivity(TJIntent.JavaClass.init(StringToJString('android.settings.WIRELESS_SETTINGS')));
end;

procedure TPlatformNFCReader.HandleNfcIntent(const AIntent: JIntent);
var
  LMessages: TJavaObjectArray<JParcelable>;
  LRecords: TJavaObjectArray<JNdefRecord>;
  LRecord: JNdefRecord;
  I, J: Integer;
  LNFCMessages: TNFCMessages;
  LNFCPayload: TNFCPayload;
begin
  TOSLog.d('+TPlatformNFCReader.HandleNfcIntent');
  if (AIntent <> nil) and (AIntent.getAction <> nil) and AIntent.getAction.equals(TJNfcAdapter.JavaClass.ACTION_NDEF_DISCOVERED) then
  begin
    LMessages := AIntent.getParcelableArrayExtra(TJNfcAdapter.JavaClass.EXTRA_NDEF_MESSAGES);
    SetLength(LNFCMessages, LMessages.Length);
    TOSLog.d('Processing messages');
    for I := 0 to LMessages.Length - 1 do
    begin
      LRecords := TJNdefMessage.Wrap(LMessages.Items[I]).getRecords;
      SetLength(LNFCMessages[I].Payloads, LRecords.Length);
      for J := 0 to LRecords.Length - 1 do
      begin
        LRecord := TJNdefRecord.Wrap(LRecords.Items[J]);
        LNFCPayload := LNFCMessages[I].Payloads[J];
        LNFCPayload.Identifier := JavaBytesToString(LRecord.getId);
        LNFCPayload.Payload := JavaBytesToString(LRecord.getPayload);
        LNFCPayload.PayloadType := JavaBytesToString(LRecord.getType);
        LNFCPayload.TypeNameFormat := TNFCPayloadTypeNameFormat(LRecord.getTnf);
        LNFCMessages[I].Payloads[J] := LNFCPayload;
      end;
    end;
    if Length(LNFCMessages) > 0 then
      DoDetectedNDEFs(LNFCMessages);
{
    begin
      //!!!! On main thread anyway??
      TThread.Synchronize(nil,
        procedure
        begin
        end
      );
    end;
}
  end;
  TOSLog.d('-TPlatformNFCReader.HandleNfcIntent');
end;

end.
