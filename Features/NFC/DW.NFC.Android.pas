unit DW.NFC.Android;

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


// ****** NOTE: This is a work in progress, so don't expect miracles :-) *****
// A very big thank you to Brian Long and his articles:
//   http://blong.com/Articles/DelphiXE7NFC/NFC.htm
//   http://blong.com/Articles/Delphi10NFC/NFC.htm
// On which a lot of the work in this unit is based

interface

uses
  // RTL
  System.Messaging, System.Generics.Collections,
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
    function FindTagTechnology(const ATag: JTag; const AName: string; out ATechnology: TNFCTechnology): Boolean;
    procedure HandleNfcIntent(const AIntent: JIntent);
    function IsNFCIntent(const AIntent: JIntent): Boolean;
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

const
  cNFCAName = 'android.nfc.tech.NfcA';
  cNFCBName = 'android.nfc.tech.NfcB';
  cNFCFName = 'android.nfc.tech.NfcF';
  cNFCVName = 'android.nfc.tech.NfcV';
  cNFCNDefName = 'android.nfc.tech.Ndef';
  cNFCIsoDepName = 'android.nfc.tech.IsoDep';

  cNFCTechnologyNames: array[TNFCTechnologyKind] of string = (cNFCAName, cNFCBName, cNFCFName, cNFCVName, cNFCNDefName, cNFCIsoDepName);

function JavaBytesToString(const ABytes: TJavaArray<Byte>): string;
var
  LBytes: TBytes;
begin
  LBytes := TAndroidHelper.TJavaArrayToTBytes(ABytes);
  Result := TEncoding.UTF8.GetString(LBytes);
end;

function JavaBytesToHexString(const ABytes: TJavaArray<Byte>): string;
var
  LByte: Byte;
begin
  Result := '';
  for LByte in TAndroidHelper.TJavaArrayToTBytes(ABytes) do
    Result := Result + IntToHex(LByte, 2);
end;

{ TPlatformNFCReader }

constructor TPlatformNFCReader.Create(const ANFCReader: TNFCReader);
var
  LIntent: JIntent;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  MainActivity.registerIntentAction(TJNfcAdapter.JavaClass.ACTION_NDEF_DISCOVERED);
  MainActivity.registerIntentAction(TJNfcAdapter.JavaClass.ACTION_TECH_DISCOVERED);
  MainActivity.registerIntentAction(TJNfcAdapter.JavaClass.ACTION_TAG_DISCOVERED);
  LIntent := TJIntent.JavaClass.init(TAndroidHelper.Context, TAndroidHelper.Activity.getClass).addFlags(TJIntent.JavaClass.FLAG_ACTIVITY_SINGLE_TOP);
  FPendingIntent := TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, 0, LIntent, TJPendingIntent.JavaClass.FLAG_MUTABLE);
end;

destructor TPlatformNFCReader.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageReceivedNotification, MessageReceivedNotificationHandler);
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

function TPlatformNFCReader.IsNFCIntent(const AIntent: JIntent): Boolean;
begin
  Result := (AIntent <> nil) and (TJNfcAdapter.JavaClass.ACTION_NDEF_DISCOVERED.equals(AIntent.getAction) or
    TJNfcAdapter.JavaClass.ACTION_TECH_DISCOVERED.equals(AIntent.getAction) or
    TJNfcAdapter.JavaClass.ACTION_TAG_DISCOVERED.equals(AIntent.getAction));
end;

class function TPlatformNFCReader.IsSupported: Boolean;
begin
  FNfcAdapter := TJNfcAdapter.JavaClass.getDefaultAdapter(TAndroidHelper.Context);
  Result := FNfcAdapter <> nil;
end;

procedure TPlatformNFCReader.EnableForegroundDispatch;
var
  LEnv: PJniEnv;
  LAdapterClass: JNIClass;
  LNfcAdapterObject, LPendingIntentObject: JNIObject;
  LMethodID: JNIMethodID;
  LJNIValues: TJNIValueArray;
begin
  LEnv := TJNIResolver.GetJNIEnv;
  LNfcAdapterObject := (FNfcAdapter as ILocalObject).GetObjectID;
  LPendingIntentObject := (FPendingIntent as ILocalObject).GetObjectID;
  LAdapterClass := LEnv^.GetObjectClass(LEnv, LNfcAdapterObject);
  LMethodID := LEnv^.GetMethodID(LEnv, LAdapterClass, 'enableForegroundDispatch',
    '(Landroid/app/Activity;Landroid/app/PendingIntent;[Landroid/content/IntentFilter;[[Ljava/lang/String;)V');
  LEnv^.DeleteLocalRef(LEnv, LAdapterClass);
  LJNIValues := ArgsToJNIValues([JavaContext, LPendingIntentObject, nil, nil]);
  LEnv^.CallVoidMethodA(LEnv, LNfcAdapterObject, LMethodID, PJNIValue(LJNIValues));
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
      if (LIntent <> nil) and IsNFCIntent(LIntent) then
        HandleNfcIntent(LIntent);
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
    if IsNFCIntent(LIntent) then
      HandleNfcIntent(LIntent);
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

function TPlatformNFCReader.FindTagTechnology(const ATag: JTag; const AName: string; out ATechnology: TNFCTechnology): Boolean;
var
  LKind: TNFCTechnologyKind;
begin
  Result := False;
  for LKind := Low(TNFCTechnologyKind) to High(TNFCTechnologyKind) do
  begin
    if AName.Equals(cNFCTechnologyNames[LKind]) then
    begin
      ATechnology.Kind := LKind;
      Result := True;
      Break;
    end;
  end;
end;

procedure TPlatformNFCReader.HandleNfcIntent(const AIntent: JIntent);
var
  LMessages: TJavaObjectArray<JParcelable>;
  LRecords: TJavaObjectArray<JNdefRecord>;
  LRecord: JNdefRecord;
  I, J: Integer;
  LNFCResult: TNFCResult;
  LNFCMessages: TNFCMessages;
  LNFCPayload: TNFCPayload;
  LTagParcel: JParcelable;
  LTag: JTag;
  LId: TJavaArray<Byte>;
  LTechs: TJavaObjectArray<JString>;
  LTechnology: TNFCTechnology;
begin
  TOSLog.d('+TPlatformNFCReader.HandleNfcIntent');
  LTagParcel := AIntent.getParcelableExtra(TJNfcAdapter.JavaClass.EXTRA_TAG);
  if LTagParcel <> nil then
  begin
    LTag := TJTag.Wrap(LTagParcel);
    LId :=  LTag.getId;
    if LId <> nil then
    try
      LNFCResult.TagInfo.ID := JavaBytesToHexString(LTag.getId);
    finally
      LId.Free;
    end;
    LTechs := LTag.getTechList;
    if LTechs <> nil then
    try
      for I := 0 to LTechs.Length - 1 do
      begin
        if FindTagTechnology(LTag, JStringToString(LTechs.Items[I]), LTechnology) then
          LNFCResult.TagInfo.Technologies := LNFCResult.TagInfo.Technologies + [LTechnology];
      end;
    finally
      LTechs.Free;
    end;
  end;
  LMessages := AIntent.getParcelableArrayExtra(TJNfcAdapter.JavaClass.EXTRA_NDEF_MESSAGES);
  if LMessages <> nil then
  begin
    SetLength(LNFCMessages, LMessages.Length);
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
  end;
  LNFCResult.Messages := LNFCMessages;
  DoResult(LNFCResult);
  TOSLog.d('-TPlatformNFCReader.HandleNfcIntent');
end;

end.
