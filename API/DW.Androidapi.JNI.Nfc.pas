unit DW.Androidapi.JNI.Nfc;

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
  // Android
  AndroidAPI.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net, Androidapi.JNI.Os;

const
  TJNfcAdapterACTION_NDEF_DISCOVERED = 'android.nfc.action.NDEF_DISCOVERED';
  TJNfcAdapterACTION_TECH_DISCOVERED = 'android.nfc.action.TECH_DISCOVERED';
  TJNfcAdapterACTION_TAG_DISCOVERED = 'android.nfc.action.TAG_DISCOVERED';
  TJNfcAdapterEXTRA_TAG = 'android.nfc.extra.TAG';
  TJNfcAdapterEXTRA_NDEF_MESSAGES = 'android.nfc.extra.NDEF_MESSAGES';
  TJNfcAdapterEXTRA_ID = 'android.nfc.extra.ID';

  TJNdefRecordTNF_EMPTY = 0;
  TJNdefRecordTNF_WELL_KNOWN = 1;
  TJNdefRecordTNF_MIME_MEDIA = 2;
  TJNdefRecordTNF_ABSOLUTE_URI = 3;
  TJNdefRecordTNF_EXTERNAL_TYPE = 4;
  TJNdefRecordTNF_UNKNOWN = 5;
  TJNdefRecordTNF_UNCHANGED = 6;

  TJMifareClassicTYPE_UNKNOWN = -1;
  TJMifareClassicTYPE_CLASSIC = 0;
  TJMifareClassicTYPE_PLUS = 1;
  TJMifareClassicTYPE_PRO = 2;
  TJMifareClassicSIZE_1K = 1024;
  TJMifareClassicSIZE_2K = 2048;
  TJMifareClassicSIZE_4K = 4096;
  TJMifareClassicSIZE_MINI = 320;
  TJMifareClassicBLOCK_SIZE = 16;

  TJMifareUltralightTYPE_UNKNOWN = -1;
  TJMifareUltralightTYPE_ULTRALIGHT = 1;
  TJMifareUltralightTYPE_ULTRALIGHT_C = 2;
  TJMifareUltralightPAGE_SIZE = 4;

  TJNdefNFC_FORUM_TYPE_1 = 'org.nfcforum.ndef.type1';
  TJNdefNFC_FORUM_TYPE_2 = 'org.nfcforum.ndef.type2';
  TJNdefNFC_FORUM_TYPE_3 = 'org.nfcforum.ndef.type3';
  TJNdefNFC_FORUM_TYPE_4 = 'org.nfcforum.ndef.type4';
  TJNdefMIFARE_CLASSIC = 'com.nxp.ndef.mifareclassic';

type
  JNfcManager = interface;
  JNfcAdapter = interface;
  JNfcAdapter_CreateBeamUrisCallback = interface;
  JNfcAdapter_CreateNdefMessageCallback = interface;
  JNfcAdapter_OnNdefPushCompleteCallback = interface;
  JTag = interface;
  JNfcEvent = interface;
  JNdefMessage = interface;
  JNdefRecord = interface;
  JTagTechnology = interface;
  JNdef = interface;
  JNdefFormatable = interface;
  JNfcA = interface;
  JNfcB = interface;
  JNfcF = interface;
  JNfcV = interface;
  JIsoDep = interface;
  JMifareClassic = interface;
  JMifareUltralight = interface;

  JNfcManagerClass = interface(JObjectClass)
    ['{2D2927FF-A70B-4F1A-8CEC-AA85108D17EE}']
    function getDefaultAdapter: JNfcAdapter; cdecl;
  end;

  [JavaSignature('android/nfc/NfcManager')]
  JNfcManager = interface(JObject)
    ['{51D10989-1C94-4C11-AD2A-F8F9B29ADB18}']
    function getDefaultAdapter: JNfcAdapter; cdecl;
  end;
  TJNfcManager = class(TJavaGenericImport<JNfcManagerClass, JNfcManager>)
  end;

  JNfcAdapterClass = interface(JObjectClass)
    ['{576DB894-7A23-4125-ACBD-CFA8D236C90C}']
    function _GetACTION_NDEF_DISCOVERED: JString;
    function _GetACTION_TAG_DISCOVERED: JString;
    function _GetACTION_TECH_DISCOVERED: JString;
    function _GetEXTRA_ID: JString;
    function _GetEXTRA_NDEF_MESSAGES: JString;
    function _GetEXTRA_TAG: JString;
    function getDefaultAdapter(context: JContext): JNfcAdapter; cdecl;
    property ACTION_NDEF_DISCOVERED: JString read _GetACTION_NDEF_DISCOVERED;
    property ACTION_TAG_DISCOVERED: JString read _GetACTION_TAG_DISCOVERED;
    property ACTION_TECH_DISCOVERED: JString read _GetACTION_TECH_DISCOVERED;
    property EXTRA_ID: JString read _GetEXTRA_ID;
    property EXTRA_NDEF_MESSAGES: JString read _GetEXTRA_NDEF_MESSAGES;
    property EXTRA_TAG: JString read _GetEXTRA_TAG;
  end;

  [JavaSignature('android/nfc/NfcAdapter')]
  JNfcAdapter = interface(JObject)
    ['{1B95784D-3688-40C2-A805-9AFFFE9B2827}']
    function isEnabled: Boolean; cdecl;
    function isNdefPushEnabled: boolean; cdecl;
    procedure disableForegroundDispatch(activity: JActivity); cdecl;
    procedure disableForegroundNdefPush(activity: JActivity); deprecated; cdecl;
    procedure enableForegroundNdefPush(activity: JActivity; &message: JNdefMessage); deprecated; cdecl;
    procedure setBeamPushUris(uris: TJavaObjectArray<Jnet_Uri>; activity: JActivity); cdecl;
    procedure setBeamPushUrisCallback(callback: JNfcAdapter_CreateBeamUrisCallback; activity: JActivity); cdecl;
    procedure setNdefPushMessage(&message: JNdefMessage; activity: JActivity; activities: TJavaObjectArray<JActivity>); cdecl;
    procedure setNdefPushMessageCallback(callback: JNfcAdapter_CreateNdefMessageCallback; activity: JActivity; activities: TJavaObjectArray<JActivity>); cdecl;
    procedure setOnNdefPushCompleteCallback(callback: JNfcAdapter_OnNdefPushCompleteCallback; activity: JActivity; activities: TJavaObjectArray<JActivity>); cdecl;
  end;
  TJNfcAdapter = class(TJavaGenericImport<JNfcAdapterClass, JNfcAdapter>)
  end;

  JNfcAdapter_CreateBeamUrisCallbackClass = interface(JObjectClass)
    ['{6F399C75-107B-435D-BBE1-D4F32657E2D2}']
  end;

  [JavaSignature('android/nfc/NfcAdapter_CreateBeamUrisCallback')]
  JNfcAdapter_CreateBeamUrisCallback = interface(JObject)
    ['{507DA3B6-98EE-4496-BA90-EA452A4993F5}']
    function createBeamUris(JNfcEventparam0: JNfcEvent): TJavaObjectArray<Jnet_Uri>; cdecl;
  end;
  TJNfcAdapter_CreateBeamUrisCallback = class(TJavaGenericImport<JNfcAdapter_CreateBeamUrisCallbackClass, JNfcAdapter_CreateBeamUrisCallback>)
  end;

  JNfcAdapter_CreateNdefMessageCallbackClass = interface(JObjectClass)
    ['{CAB6845C-87EE-43E8-9114-4CD028E4F023}']
  end;

  [JavaSignature('android/nfc/NfcAdapter_CreateNdefMessageCallback')]
  JNfcAdapter_CreateNdefMessageCallback = interface(JObject)
    ['{40E88317-43A2-44F3-8E5F-8701EFC7F88E}']
    function createNdefMessage(JNfcEventparam0: JNfcEvent): JNdefMessage; cdecl;
  end;
  TJNfcAdapter_CreateNdefMessageCallback = class(TJavaGenericImport<JNfcAdapter_CreateNdefMessageCallbackClass, JNfcAdapter_CreateNdefMessageCallback>)
  end;

  JNfcAdapter_OnNdefPushCompleteCallbackClass = interface(JObjectClass)
    ['{329A1391-1B3A-44D4-A180-7B8CC355E274}']
  end;

  [JavaSignature('android/nfc/NfcAdapter_OnNdefPushCompleteCallback')]
  JNfcAdapter_OnNdefPushCompleteCallback = interface(JObject)
    ['{D58424EE-F0FC-4CE0-983C-D654EE99CEC7}']
    procedure onNdefPushComplete(JNfcEventparam0: JNfcEvent); cdecl;
  end;
  TJNfcAdapter_OnNdefPushCompleteCallback = class(TJavaGenericImport<JNfcAdapter_OnNdefPushCompleteCallbackClass, JNfcAdapter_OnNdefPushCompleteCallback>)
  end;

  JTagClass = interface(JObjectClass)
    ['{FF9E927B-3CBC-44E1-9CE1-47247AEEBA44}']
    function _GetCREATOR: JParcelable_Creator; cdecl;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/nfc/Tag')]
  JTag = interface(JObject)
    ['{D1CB91EF-94C4-4930-BCEF-A0A6C6712C0E}']
    function describeContents: Integer; cdecl;
    function getId: TJavaArray<Byte>; cdecl;
    function getTechList: TJavaObjectArray<JString>; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJTag = class(TJavaGenericImport<JTagClass, JTag>)
  end;

  JNfcEventClass = interface(JObjectClass)
    ['{83E040AF-F429-4B7E-839D-FCD116332D9F}']
    function _GetnfcAdapter: JNfcAdapter; cdecl;
    property nfcAdapter: JNfcAdapter read _GetnfcAdapter;
  end;

  [JavaSignature('android/nfc/NfcEvent')]
  JNfcEvent = interface(JObject)
    ['{F96B51E7-47F6-4195-9209-730D8088D8FD}']
  end;
  TJNfcEvent = class(TJavaGenericImport<JNfcEventClass, JNfcEvent>)
  end;

  JNdefMessageClass = interface(JObjectClass)
    ['{7CE2D708-6436-4C46-96A7-604C86828B27}']
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function init(&record: JNdefRecord; records: TJavaObjectArray<JNdefRecord>): JNdefMessage; cdecl; overload;
    function init(data: TJavaArray<Byte>): JNdefMessage; cdecl; overload;
    function init(records: TJavaObjectArray<JNdefRecord>): JNdefMessage; cdecl; overload;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/nfc/NdefMessage')]
  JNdefMessage = interface(JObject)
    ['{186B19E4-8AA0-41AA-ABB9-85D2C1C4D487}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): boolean; cdecl;
    function getByteArrayLength: Integer; cdecl;
    function getRecords: TJavaObjectArray<JNdefRecord>; cdecl;
    function hashCode: Integer; cdecl;
    function toByteArray: TJavaArray<Byte>; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJNdefMessage = class(TJavaGenericImport<JNdefMessageClass, JNdefMessage>)
  end;

  JNdefRecordClass = interface(JObjectClass)
    ['{1A923B8C-925A-4E62-BB42-954F780F2C73}']
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function _GetRTD_ALTERNATIVE_CARRIER: TJavaArray<Byte>; cdecl;
    function _GetRTD_HANDOVER_CARRIER: TJavaArray<Byte>; cdecl;
    function _GetRTD_HANDOVER_REQUEST: TJavaArray<Byte>; cdecl;
    function _GetRTD_HANDOVER_SELECT: TJavaArray<Byte>; cdecl;
    function _GetRTD_SMART_POSTER: TJavaArray<Byte>; cdecl;
    function _GetRTD_TEXT: TJavaArray<Byte>; cdecl;
    function _GetRTD_URI: TJavaArray<Byte>; cdecl;
    function _GetTNF_ABSOLUTE_URI: SmallInt; cdecl;
    function _GetTNF_EMPTY: SmallInt; cdecl;
    function _GetTNF_EXTERNAL_TYPE: SmallInt; cdecl;
    function _GetTNF_MIME_MEDIA: SmallInt; cdecl;
    function _GetTNF_UNCHANGED: SmallInt; cdecl;
    function _GetTNF_UNKNOWN: SmallInt; cdecl;
    function _GetTNF_WELL_KNOWN: SmallInt; cdecl;
    function createApplicationRecord(packageName: JString): JNdefRecord; cdecl;
    function createExternal(domain: JString; &type: JString; data: TJavaArray<Byte>): JNdefRecord; cdecl;
    function createMime(mimeType: JString; mimeData: TJavaArray<Byte>): JNdefRecord; cdecl;
    function createUri(uri: Jnet_Uri): JNdefRecord; cdecl; overload;
    function createUri(uriString: JString): JNdefRecord; cdecl; overload;
    function init(data: TJavaArray<Byte>): JNdefRecord; deprecated; cdecl; overload;
    function init(tnf: SmallInt; &type: TJavaArray<Byte>; id: TJavaArray<Byte>; payload: TJavaArray<Byte>): JNdefRecord; cdecl; overload;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
    property RTD_ALTERNATIVE_CARRIER: TJavaArray<Byte> read _GetRTD_ALTERNATIVE_CARRIER;
    property RTD_HANDOVER_CARRIER: TJavaArray<Byte> read _GetRTD_HANDOVER_CARRIER;
    property RTD_HANDOVER_REQUEST: TJavaArray<Byte> read _GetRTD_HANDOVER_REQUEST;
    property RTD_HANDOVER_SELECT: TJavaArray<Byte> read _GetRTD_HANDOVER_SELECT;
    property RTD_SMART_POSTER: TJavaArray<Byte> read _GetRTD_SMART_POSTER;
    property RTD_TEXT: TJavaArray<Byte> read _GetRTD_TEXT;
    property RTD_URI: TJavaArray<Byte> read _GetRTD_URI;
    property TNF_ABSOLUTE_URI: SmallInt read _GetTNF_ABSOLUTE_URI;
    property TNF_EMPTY: SmallInt read _GetTNF_EMPTY;
    property TNF_EXTERNAL_TYPE: SmallInt read _GetTNF_EXTERNAL_TYPE;
    property TNF_MIME_MEDIA: SmallInt read _GetTNF_MIME_MEDIA;
    property TNF_UNCHANGED: SmallInt read _GetTNF_UNCHANGED;
    property TNF_UNKNOWN: SmallInt read _GetTNF_UNKNOWN;
    property TNF_WELL_KNOWN: SmallInt read _GetTNF_WELL_KNOWN;
  end;

  [JavaSignature('android/nfc/NdefRecord')]
  JNdefRecord = interface(JObject)
    ['{9A055F41-0D6F-4833-AE58-4043EBCF7BD8}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): boolean; cdecl;
    function getId: TJavaArray<Byte>; cdecl;
    function getPayload: TJavaArray<Byte>; cdecl;
    function getTnf: SmallInt; cdecl;
    function getType: TJavaArray<Byte>; cdecl;
    function hashCode: Integer; cdecl;
    function toByteArray: TJavaArray<Byte>; deprecated; cdecl;
    function toMimeType: JString; cdecl;
    function toString: JString; cdecl;
    function toUri: Jnet_Uri; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJNdefRecord = class(TJavaGenericImport<JNdefRecordClass, JNdefRecord>)
  end;

  JTagTechnologyClass = interface(JObjectClass)
    ['{C91DB291-12DE-4327-B904-AA15BBF4D7EF}']
  end;

  [JavaSignature('android/nfc/tech/TagTechnology')]
  JTagTechnology = interface(JObject)
    ['{2523043E-945D-44B7-961F-9F418405D4A6}']
    function getTag: JTag; cdecl;
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
  end;
  TJTagTechnology = class(TJavaGenericImport<JTagTechnologyClass, JTagTechnology>)
  end;

  JNdefClass = interface(JObjectClass)
    ['{5D76EFDC-6A38-4CF9-A25C-C791B4905B52}']
    function _GetMIFARE_CLASSIC: JString; cdecl;
    function _GetNFC_FORUM_TYPE_1: JString; cdecl;
    function _GetNFC_FORUM_TYPE_2: JString; cdecl;
    function _GetNFC_FORUM_TYPE_3: JString; cdecl;
    function _GetNFC_FORUM_TYPE_4: JString; cdecl;
    function get(tag: JTag): JNdef; cdecl;
    property MIFARE_CLASSIC: JString read _GetMIFARE_CLASSIC;
    property NFC_FORUM_TYPE_1: JString read _GetNFC_FORUM_TYPE_1;
    property NFC_FORUM_TYPE_2: JString read _GetNFC_FORUM_TYPE_2;
    property NFC_FORUM_TYPE_3: JString read _GetNFC_FORUM_TYPE_3;
    property NFC_FORUM_TYPE_4: JString read _GetNFC_FORUM_TYPE_4;
  end;

  [JavaSignature('android/nfc/tech/Ndef')]
  JNdef = interface(JObject)
    ['{14D8736E-BAC3-479D-B480-312E232988D7}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    function canMakeReadOnly: boolean; cdecl;
    function getCachedNdefMessage: JNdefMessage; cdecl;
    function getMaxSize: Integer; cdecl;
    function getNdefMessage: JNdefMessage; cdecl;
    function getType: JString; cdecl;
    function isWritable: boolean; cdecl;
    function makeReadOnly: boolean; cdecl;
    procedure writeNdefMessage(msg: JNdefMessage); cdecl;
  end;
  TJNdef = class(TJavaGenericImport<JNdefClass, JNdef>)
  end;

  JNdefFormatableClass = interface(JObjectClass)
    ['{B0A70D1D-1C88-40B6-AAB8-CF6CB069B4E9}']
    function get(tag: JTag): JNdefFormatable; cdecl;
  end;

  [JavaSignature('android/nfc/tech/NdefFormatable')]
  JNdefFormatable = interface(JObject)
    ['{5E584A92-6E55-45CF-9A69-E8F79019F8E2}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    procedure format(firstMessage: JNdefMessage); cdecl;
    procedure formatReadOnly(firstMessage: JNdefMessage); cdecl;
  end;
  TJNdefFormatable = class(TJavaGenericImport<JNdefFormatableClass, JNdefFormatable>)
  end;

  JNfcAClass = interface(JObjectClass)
    ['{D92B1A7B-4870-4A93-A5B4-136E7938255C}']
    function get(tag : JTag) : JNfcA; cdecl;
  end;

  [JavaSignature('android/nfc/tech/NfcA')]
  JNfcA = interface(JObject)
    ['{2ABD243E-7D00-45C2-8C8F-BEC7505315F3}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    function getAtqa: TJavaArray<Byte>; cdecl;
    function getMaxTransceiveLength: Integer; cdecl;
    function getSak: SmallInt; cdecl;
    function getTimeout: Integer; cdecl;
    function transceive(data: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    procedure setTimeout(timeout: Integer); cdecl;
  end;
  TJNfcA = class(TJavaGenericImport<JNfcAClass, JNfcA>)
  end;

  JNfcBClass = interface(JObjectClass)
    ['{56A32A72-96E2-49A0-B943-822DF25ACEB5}']
    function get(tag : JTag) : JNfcB; cdecl;
  end;

  [JavaSignature('android/nfc/tech/NfcB')]
  JNfcB = interface(JObject)
    ['{FC5BB379-837C-4E05-93EC-9C61F21507D7}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    function getApplicationData: TJavaArray<Byte>; cdecl;
    function getMaxTransceiveLength: Integer; cdecl;
    function getProtocolInfo: TJavaArray<Byte>; cdecl;
    function transceive(data: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
  end;
  TJNfcB = class(TJavaGenericImport<JNfcBClass, JNfcB>)
  end;

  JNfcFClass = interface(JObjectClass)
    ['{78F8EA64-256C-4E02-86B3-4F15B8B7A21E}']
    function get(tag: JTag): JNfcF; cdecl;
  end;

  [JavaSignature('android/nfc/tech/NfcF')]
  JNfcF = interface(JObject)
    ['{469C5ECE-0BE1-4187-8A81-C42013EC8590}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    function getManufacturer: TJavaArray<Byte>; cdecl;
    function getMaxTransceiveLength: Integer; cdecl;
    function getSystemCode: TJavaArray<Byte>; cdecl;
    function getTimeout: Integer; cdecl;
    function transceive(data: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    procedure setTimeout(timeout: Integer); cdecl;
  end;
  TJNfcF = class(TJavaGenericImport<JNfcFClass, JNfcF>)
  end;

  JNfcVClass = interface(JObjectClass)
    ['{C947D99D-9D45-4114-8432-DC4B55B59C2D}']
    function get(tag : JTag) : JNfcV; cdecl;
  end;

  [JavaSignature('android/nfc/tech/NfcV')]
  JNfcV = interface(JObject)
    ['{CF53A4D0-9064-40F6-83D7-6555F8695D92}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    function getDsfId: Byte; cdecl;
    function getMaxTransceiveLength: Integer; cdecl;
    function getResponseFlags: Byte; cdecl;
    function transceive(data: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
  end;
  TJNfcV = class(TJavaGenericImport<JNfcVClass, JNfcV>)
  end;

  JIsoDepClass = interface(JObjectClass)
    ['{D2B7D418-4644-430F-AABF-68B67E71C218}']
    function get(tag: JTag): JIsoDep; cdecl;
  end;

  [JavaSignature('android/nfc/tech/IsoDep')]
  JIsoDep = interface(JObject)
    ['{280BF2C5-D6D9-45E1-A9C5-4533F82B6BCE}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    function getHiLayerResponse: TJavaArray<Byte>; cdecl;
    function getHistoricalBytes: TJavaArray<Byte>; cdecl;
    function getMaxTransceiveLength: Integer; cdecl;
    function getTimeout: Integer; cdecl;
    function isExtendedLengthApduSupported: boolean; cdecl;
    function transceive(data: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    procedure setTimeout(timeout: Integer); cdecl;
  end;
  TJIsoDep = class(TJavaGenericImport<JIsoDepClass, JIsoDep>)
  end;

  JMifareClassicClass = interface(JObjectClass)
    ['{E8D11EDF-FDD9-4CC1-9E0B-F539A63962C3}']
    function _GetBLOCK_SIZE: Integer; cdecl;
    function _GetKEY_DEFAULT: TJavaArray<Byte>; cdecl;
    function _GetKEY_MIFARE_APPLICATION_DIRECTORY: TJavaArray<Byte>; cdecl;
    function _GetKEY_NFC_FORUM: TJavaArray<Byte>; cdecl;
    function _GetSIZE_1K: Integer; cdecl;
    function _GetSIZE_2K: Integer; cdecl;
    function _GetSIZE_4K: Integer; cdecl;
    function _GetSIZE_MINI: Integer; cdecl;
    function _GetTYPE_CLASSIC: Integer; cdecl;
    function _GetTYPE_PLUS: Integer; cdecl;
    function _GetTYPE_PRO: Integer; cdecl;
    function _GetTYPE_UNKNOWN: Integer; cdecl;
    function get(tag: JTag): JMifareClassic; cdecl;
    property BLOCK_SIZE: Integer read _GetBLOCK_SIZE;
    property KEY_DEFAULT: TJavaArray<Byte> read _GetKEY_DEFAULT;
    property KEY_MIFARE_APPLICATION_DIRECTORY: TJavaArray<Byte> read _GetKEY_MIFARE_APPLICATION_DIRECTORY;
    property KEY_NFC_FORUM: TJavaArray<Byte> read _GetKEY_NFC_FORUM;
    property SIZE_1K: Integer read _GetSIZE_1K;
    property SIZE_2K: Integer read _GetSIZE_2K;
    property SIZE_4K: Integer read _GetSIZE_4K;
    property SIZE_MINI: Integer read _GetSIZE_MINI;
    property TYPE_CLASSIC: Integer read _GetTYPE_CLASSIC;
    property TYPE_PLUS: Integer read _GetTYPE_PLUS;
    property TYPE_PRO: Integer read _GetTYPE_PRO;
    property TYPE_UNKNOWN: Integer read _GetTYPE_UNKNOWN;
  end;

  [JavaSignature('android/nfc/tech/MifareClassic')]
  JMifareClassic = interface(JObject)
    ['{6B67383E-ED06-452C-BEA2-F84D9069702D}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    function authenticateSectorWithKeyA(sectorIndex: Integer; key: TJavaArray<Byte>): boolean; cdecl;
    function authenticateSectorWithKeyB(sectorIndex: Integer; key: TJavaArray<Byte>): boolean; cdecl;
    function blockToSector(blockIndex: Integer): Integer; cdecl;
    function getBlockCount: Integer; cdecl;
    function getBlockCountInSector(sectorIndex: Integer): Integer; cdecl;
    function getMaxTransceiveLength: Integer; cdecl;
    function getSectorCount: Integer; cdecl;
    function getSize: Integer; cdecl;
    function getTimeout: Integer; cdecl;
    function getType: Integer; cdecl;
    function readBlock(blockIndex: Integer): TJavaArray<Byte>; cdecl;
    function sectorToBlock(sectorIndex: Integer): Integer; cdecl;
    function transceive(data: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    procedure decrement(blockIndex: Integer; value: Integer); cdecl;
    procedure increment(blockIndex: Integer; value: Integer); cdecl;
    procedure restore(blockIndex: Integer); cdecl;
    procedure setTimeout(timeout: Integer); cdecl;
    procedure transfer(blockIndex: Integer); cdecl;
    procedure writeBlock(blockIndex: Integer; data: TJavaArray<Byte>); cdecl;
  end;
  TJMifareClassic = class(TJavaGenericImport<JMifareClassicClass, JMifareClassic>)
  end;

  JMifareUltralightClass = interface(JObjectClass)
    ['{334C777C-6099-4892-9E75-5974FDFDA6C2}']
    function _GetPAGE_SIZE: Integer; cdecl;
    function _GetTYPE_ULTRALIGHT: Integer; cdecl;
    function _GetTYPE_ULTRALIGHT_C: Integer; cdecl;
    function _GetTYPE_UNKNOWN: Integer; cdecl;
    function get(tag: JTag): JMifareUltralight; cdecl;
    property PAGE_SIZE: Integer read _GetPAGE_SIZE;
    property TYPE_ULTRALIGHT: Integer read _GetTYPE_ULTRALIGHT;
    property TYPE_ULTRALIGHT_C: Integer read _GetTYPE_ULTRALIGHT_C;
    property TYPE_UNKNOWN: Integer read _GetTYPE_UNKNOWN;
  end;

  [JavaSignature('android/nfc/tech/MifareUltralight')]
  JMifareUltralight = interface(JObject)
    ['{525CCFAA-17B5-4048-A631-DE3AA188FF1D}']
    function isConnected: boolean; cdecl;
    procedure close; cdecl;
    procedure connect; cdecl;
    function getMaxTransceiveLength: Integer; cdecl;
    function getTimeout: Integer; cdecl;
    function getType: Integer; cdecl;
    function readPages(pageOffset: Integer): TJavaArray<Byte>; cdecl;
    function transceive(data: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    procedure setTimeout(timeout: Integer); cdecl;
    procedure writePage(pageOffset: Integer; data: TJavaArray<Byte>); cdecl;
  end;
  TJMifareUltralight = class(TJavaGenericImport<JMifareUltralightClass, JMifareUltralight>)
  end;

implementation

end.
