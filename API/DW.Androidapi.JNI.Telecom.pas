unit DW.Androidapi.JNI.Telecom;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  AndroidAPI.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net;

type
  JPhoneAccount = interface;
  JPhoneAccount_Builder = interface;
  JPhoneAccountHandle = interface;
  JTelecomManager = interface;

  JPhoneAccountClass = interface(JObjectClass)
    ['{B16930DD-2D8E-4018-81A1-3F086B7228F5}']
    function _GetCAPABILITY_CALL_PROVIDER: Integer; cdecl;
    function _GetCAPABILITY_CALL_SUBJECT: Integer; cdecl;
    function _GetCAPABILITY_CONNECTION_MANAGER: Integer; cdecl;
    function _GetCAPABILITY_PLACE_EMERGENCY_CALLS: Integer; cdecl;
    function _GetCAPABILITY_RTT: Integer; cdecl;
    function _GetCAPABILITY_SELF_MANAGED: Integer; cdecl;
    function _GetCAPABILITY_SIM_SUBSCRIPTION: Integer; cdecl;
    function _GetCAPABILITY_SUPPORTS_VIDEO_CALLING: Integer; cdecl;
    function _GetCAPABILITY_VIDEO_CALLING: Integer; cdecl;
    function _GetCAPABILITY_VIDEO_CALLING_RELIES_ON_PRESENCE: Integer; cdecl;
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function _GetEXTRA_CALL_SUBJECT_CHARACTER_ENCODING: JString; cdecl;
    function _GetEXTRA_CALL_SUBJECT_MAX_LENGTH: JString; cdecl;
    function _GetEXTRA_LOG_SELF_MANAGED_CALLS: JString; cdecl;
    function _GetEXTRA_SUPPORTS_HANDOVER_FROM: JString; cdecl;
    function _GetEXTRA_SUPPORTS_HANDOVER_TO: JString; cdecl;
    function _GetNO_HIGHLIGHT_COLOR: Integer; cdecl;
    function _GetNO_RESOURCE_ID: Integer; cdecl;
    function _GetSCHEME_SIP: JString; cdecl;
    function _GetSCHEME_TEL: JString; cdecl;
    function _GetSCHEME_VOICEMAIL: JString; cdecl;
    function builder(accountHandle: JPhoneAccountHandle; &label: JCharSequence): JPhoneAccount_Builder; cdecl;
    property CAPABILITY_CALL_PROVIDER: Integer read _GetCAPABILITY_CALL_PROVIDER;
    property CAPABILITY_CALL_SUBJECT: Integer read _GetCAPABILITY_CALL_SUBJECT;
    property CAPABILITY_CONNECTION_MANAGER: Integer read _GetCAPABILITY_CONNECTION_MANAGER;
    property CAPABILITY_PLACE_EMERGENCY_CALLS: Integer read _GetCAPABILITY_PLACE_EMERGENCY_CALLS;
    property CAPABILITY_RTT: Integer read _GetCAPABILITY_RTT;
    property CAPABILITY_SELF_MANAGED: Integer read _GetCAPABILITY_SELF_MANAGED;
    property CAPABILITY_SIM_SUBSCRIPTION: Integer read _GetCAPABILITY_SIM_SUBSCRIPTION;
    property CAPABILITY_SUPPORTS_VIDEO_CALLING: Integer read _GetCAPABILITY_SUPPORTS_VIDEO_CALLING;
    property CAPABILITY_VIDEO_CALLING: Integer read _GetCAPABILITY_VIDEO_CALLING;
    property CAPABILITY_VIDEO_CALLING_RELIES_ON_PRESENCE: Integer read _GetCAPABILITY_VIDEO_CALLING_RELIES_ON_PRESENCE;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
    property EXTRA_CALL_SUBJECT_CHARACTER_ENCODING: JString read _GetEXTRA_CALL_SUBJECT_CHARACTER_ENCODING;
    property EXTRA_CALL_SUBJECT_MAX_LENGTH: JString read _GetEXTRA_CALL_SUBJECT_MAX_LENGTH;
    property EXTRA_LOG_SELF_MANAGED_CALLS: JString read _GetEXTRA_LOG_SELF_MANAGED_CALLS;
    property EXTRA_SUPPORTS_HANDOVER_FROM: JString read _GetEXTRA_SUPPORTS_HANDOVER_FROM;
    property EXTRA_SUPPORTS_HANDOVER_TO: JString read _GetEXTRA_SUPPORTS_HANDOVER_TO;
    property NO_HIGHLIGHT_COLOR: Integer read _GetNO_HIGHLIGHT_COLOR;
    property NO_RESOURCE_ID: Integer read _GetNO_RESOURCE_ID;
    property SCHEME_SIP: JString read _GetSCHEME_SIP;
    property SCHEME_TEL: JString read _GetSCHEME_TEL;
    property SCHEME_VOICEMAIL: JString read _GetSCHEME_VOICEMAIL;
  end;

  [JavaSignature('android/telecom/PhoneAccount')]
  JPhoneAccount = interface(JObject)
    ['{B93D1867-4E7C-45A0-A18A-F7BED8E0DDA2}']
    function describeContents: Integer; cdecl;
    function getAccountHandle: JPhoneAccountHandle; cdecl;
    function getAddress: Jnet_Uri; cdecl;
    function getCapabilities: Integer; cdecl;
    function getExtras: JBundle; cdecl;
    function getHighlightColor: Integer; cdecl;
    function getIcon: JIcon; cdecl;
    function getLabel: JCharSequence; cdecl;
    function getShortDescription: JCharSequence; cdecl;
    function getSubscriptionAddress: Jnet_Uri; cdecl;
    function getSupportedUriSchemes: JList; cdecl;
    function hasCapabilities(capability: Integer): boolean; cdecl;
    function isEnabled: boolean; cdecl;
    function supportsUriScheme(uriScheme: JString): boolean; cdecl;
    function toBuilder: JPhoneAccount_Builder; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(&out: JParcel; flags: Integer); cdecl;
  end;
  TJPhoneAccount = class(TJavaGenericImport<JPhoneAccountClass, JPhoneAccount>)
  end;

  JPhoneAccount_BuilderClass = interface(JObjectClass)
    ['{03A309D9-0BCE-462C-8BEA-60246C52BCD9}']
  end;

  [JavaSignature('android/telecom/PhoneAccount_Builder')]
  JPhoneAccount_Builder = interface(JObject)
    ['{9123CB8D-07E1-405F-87DC-A4B9045AA726}']
    function addSupportedUriScheme(uriScheme: JString): JPhoneAccount_Builder; cdecl;
    function build: JPhoneAccount; cdecl;
    function setAddress(value: Jnet_Uri): JPhoneAccount_Builder; cdecl;
    function setCapabilities(value: Integer): JPhoneAccount_Builder; cdecl;
    function setExtras(extras: JBundle): JPhoneAccount_Builder; cdecl;
    function setHighlightColor(value: Integer): JPhoneAccount_Builder; cdecl;
    function setIcon(icon: JIcon): JPhoneAccount_Builder; cdecl;
    function setShortDescription(value: JCharSequence): JPhoneAccount_Builder; cdecl;
    function setSubscriptionAddress(value: Jnet_Uri): JPhoneAccount_Builder; cdecl;
    function setSupportedUriSchemes(uriSchemes: JList): JPhoneAccount_Builder; cdecl;
  end;
  TJPhoneAccount_Builder = class(TJavaGenericImport<JPhoneAccount_BuilderClass, JPhoneAccount_Builder>)
  end;

  JPhoneAccountHandleClass = interface(JParcelableClass)
    ['{4346FC20-CA4A-4488-BBEB-E0076CD625D4}']
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function init(componentName: JComponentName; id: JString): JPhoneAccountHandle; cdecl; overload;
    function init(componentName: JComponentName; id: JString; userHandle: JUserHandle): JPhoneAccountHandle; cdecl; overload;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/telecom/PhoneAccountHandle')]
  JPhoneAccountHandle = interface(JParcelable)
    ['{94B296F6-3F9F-49B5-A333-3564F3B6C66C}']
    function describeContents: Integer; cdecl;
    function equals(other: JObject): boolean; cdecl;
    function getComponentName: JComponentName; cdecl;
    function getId: JString; cdecl;
    function getUserHandle: JUserHandle; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(&out: JParcel; flags: Integer); cdecl;
  end;
  TJPhoneAccountHandle = class(TJavaGenericImport<JPhoneAccountHandleClass, JPhoneAccountHandle>)
  end;

  JTelecomManagerClass = interface(JObjectClass)
    ['{EDA92BEF-0C83-48DE-9EC0-779E938EF06C}']
    function _GetACTION_CHANGE_DEFAULT_DIALER : JString; cdecl;
    function _GetACTION_CHANGE_PHONE_ACCOUNTS : JString; cdecl;
    function _GetACTION_CONFIGURE_PHONE_ACCOUNT : JString; cdecl;
    function _GetACTION_DEFAULT_DIALER_CHANGED : JString; cdecl;
    function _GetACTION_INCOMING_CALL : JString; cdecl;
    function _GetACTION_PHONE_ACCOUNT_REGISTERED : JString; cdecl;
    function _GetACTION_PHONE_ACCOUNT_UNREGISTERED : JString; cdecl;
    function _GetACTION_SHOW_CALL_ACCESSIBILITY_SETTINGS : JString; cdecl;
    function _GetACTION_SHOW_CALL_SETTINGS : JString; cdecl;
    function _GetACTION_SHOW_MISSED_CALLS_NOTIFICATION : JString; cdecl;
    function _GetACTION_SHOW_RESPOND_VIA_SMS_SETTINGS : JString; cdecl;
    function _GetDTMF_CHARACTER_PAUSE : Char; cdecl;
    function _GetDTMF_CHARACTER_WAIT : Char; cdecl;
    function _GetEXTRA_CALL_BACK_NUMBER : JString; cdecl;
    function _GetEXTRA_CALL_DISCONNECT_CAUSE : JString; cdecl;
    function _GetEXTRA_CALL_DISCONNECT_MESSAGE : JString; cdecl;
    function _GetEXTRA_CALL_SUBJECT : JString; cdecl;
    function _GetEXTRA_CHANGE_DEFAULT_DIALER_PACKAGE_NAME : JString; cdecl;
    function _GetEXTRA_INCOMING_CALL_ADDRESS : JString; cdecl;
    function _GetEXTRA_INCOMING_CALL_EXTRAS : JString; cdecl;
    function _GetEXTRA_INCOMING_VIDEO_STATE : JString; cdecl;
    function _GetEXTRA_NOTIFICATION_COUNT : JString; cdecl;
    function _GetEXTRA_NOTIFICATION_PHONE_NUMBER : JString; cdecl;
    function _GetEXTRA_OUTGOING_CALL_EXTRAS : JString; cdecl;
    function _GetEXTRA_PHONE_ACCOUNT_HANDLE : JString; cdecl;
    function _GetEXTRA_START_CALL_WITH_RTT : JString; cdecl;
    function _GetEXTRA_START_CALL_WITH_SPEAKERPHONE : JString; cdecl;
    function _GetEXTRA_START_CALL_WITH_VIDEO_STATE : JString; cdecl;
    function _GetGATEWAY_ORIGINAL_ADDRESS : JString; cdecl;
    function _GetGATEWAY_PROVIDER_PACKAGE : JString; cdecl;
    function _GetMETADATA_INCLUDE_EXTERNAL_CALLS : JString; cdecl;
    function _GetMETADATA_INCLUDE_SELF_MANAGED_CALLS : JString; cdecl;
    function _GetMETADATA_IN_CALL_SERVICE_RINGING : JString; cdecl;
    function _GetMETADATA_IN_CALL_SERVICE_UI : JString; cdecl;
    function _GetPRESENTATION_ALLOWED : Integer; cdecl;
    function _GetPRESENTATION_PAYPHONE : Integer; cdecl;
    function _GetPRESENTATION_RESTRICTED : Integer; cdecl;
    function _GetPRESENTATION_UNKNOWN : Integer; cdecl;
    property ACTION_CHANGE_DEFAULT_DIALER : JString read _GetACTION_CHANGE_DEFAULT_DIALER;
    property ACTION_CHANGE_PHONE_ACCOUNTS : JString read _GetACTION_CHANGE_PHONE_ACCOUNTS;
    property ACTION_CONFIGURE_PHONE_ACCOUNT : JString read _GetACTION_CONFIGURE_PHONE_ACCOUNT;
    property ACTION_DEFAULT_DIALER_CHANGED : JString read _GetACTION_DEFAULT_DIALER_CHANGED;
    property ACTION_INCOMING_CALL : JString read _GetACTION_INCOMING_CALL;
    property ACTION_PHONE_ACCOUNT_REGISTERED : JString read _GetACTION_PHONE_ACCOUNT_REGISTERED;
    property ACTION_PHONE_ACCOUNT_UNREGISTERED : JString read _GetACTION_PHONE_ACCOUNT_UNREGISTERED;
    property ACTION_SHOW_CALL_ACCESSIBILITY_SETTINGS : JString read _GetACTION_SHOW_CALL_ACCESSIBILITY_SETTINGS;
    property ACTION_SHOW_CALL_SETTINGS : JString read _GetACTION_SHOW_CALL_SETTINGS;
    property ACTION_SHOW_MISSED_CALLS_NOTIFICATION : JString read _GetACTION_SHOW_MISSED_CALLS_NOTIFICATION;
    property ACTION_SHOW_RESPOND_VIA_SMS_SETTINGS : JString read _GetACTION_SHOW_RESPOND_VIA_SMS_SETTINGS;
    property DTMF_CHARACTER_PAUSE : Char read _GetDTMF_CHARACTER_PAUSE;
    property DTMF_CHARACTER_WAIT : Char read _GetDTMF_CHARACTER_WAIT;
    property EXTRA_CALL_BACK_NUMBER : JString read _GetEXTRA_CALL_BACK_NUMBER;
    property EXTRA_CALL_DISCONNECT_CAUSE : JString read _GetEXTRA_CALL_DISCONNECT_CAUSE;
    property EXTRA_CALL_DISCONNECT_MESSAGE : JString read _GetEXTRA_CALL_DISCONNECT_MESSAGE;
    property EXTRA_CALL_SUBJECT : JString read _GetEXTRA_CALL_SUBJECT;
    property EXTRA_CHANGE_DEFAULT_DIALER_PACKAGE_NAME : JString read _GetEXTRA_CHANGE_DEFAULT_DIALER_PACKAGE_NAME;
    property EXTRA_INCOMING_CALL_ADDRESS : JString read _GetEXTRA_INCOMING_CALL_ADDRESS;
    property EXTRA_INCOMING_CALL_EXTRAS : JString read _GetEXTRA_INCOMING_CALL_EXTRAS;
    property EXTRA_INCOMING_VIDEO_STATE : JString read _GetEXTRA_INCOMING_VIDEO_STATE;
    property EXTRA_NOTIFICATION_COUNT : JString read _GetEXTRA_NOTIFICATION_COUNT;
    property EXTRA_NOTIFICATION_PHONE_NUMBER : JString read _GetEXTRA_NOTIFICATION_PHONE_NUMBER;
    property EXTRA_OUTGOING_CALL_EXTRAS : JString read _GetEXTRA_OUTGOING_CALL_EXTRAS;
    property EXTRA_PHONE_ACCOUNT_HANDLE : JString read _GetEXTRA_PHONE_ACCOUNT_HANDLE;
    property EXTRA_START_CALL_WITH_RTT : JString read _GetEXTRA_START_CALL_WITH_RTT;
    property EXTRA_START_CALL_WITH_SPEAKERPHONE : JString read _GetEXTRA_START_CALL_WITH_SPEAKERPHONE;
    property EXTRA_START_CALL_WITH_VIDEO_STATE : JString read _GetEXTRA_START_CALL_WITH_VIDEO_STATE;
    property GATEWAY_ORIGINAL_ADDRESS : JString read _GetGATEWAY_ORIGINAL_ADDRESS;
    property GATEWAY_PROVIDER_PACKAGE : JString read _GetGATEWAY_PROVIDER_PACKAGE;
    property METADATA_INCLUDE_EXTERNAL_CALLS : JString read _GetMETADATA_INCLUDE_EXTERNAL_CALLS;
    property METADATA_INCLUDE_SELF_MANAGED_CALLS : JString read _GetMETADATA_INCLUDE_SELF_MANAGED_CALLS;
    property METADATA_IN_CALL_SERVICE_RINGING : JString read _GetMETADATA_IN_CALL_SERVICE_RINGING;
    property METADATA_IN_CALL_SERVICE_UI : JString read _GetMETADATA_IN_CALL_SERVICE_UI;
    property PRESENTATION_ALLOWED : Integer read _GetPRESENTATION_ALLOWED;
    property PRESENTATION_PAYPHONE : Integer read _GetPRESENTATION_PAYPHONE;
    property PRESENTATION_RESTRICTED : Integer read _GetPRESENTATION_RESTRICTED;
    property PRESENTATION_UNKNOWN : Integer read _GetPRESENTATION_UNKNOWN;
  end;

  [JavaSignature('android/telecom/TelecomManager')]
  JTelecomManager = interface(JObject)
    ['{FC988BC3-1625-4E91-B322-45FBF90CEED8}']
    procedure acceptHandover(srcAddr: Jnet_Uri; videoState: Integer; destAcct: JPhoneAccountHandle); cdecl;
    procedure acceptRingingCall; cdecl; overload;
    procedure acceptRingingCall(videoState: Integer); cdecl; overload;
    procedure addNewIncomingCall(phoneAccount: JPhoneAccountHandle; extras: JBundle); cdecl;
    procedure cancelMissedCallsNotification; cdecl;
    function createManageBlockedNumbersIntent: JIntent; cdecl;
    function endCall: boolean; cdecl;
    function getAdnUriForPhoneAccount(accountHandle: JPhoneAccountHandle): Jnet_Uri; cdecl;
    function getCallCapablePhoneAccounts: JList; cdecl;
    function getDefaultDialerPackage: JString; cdecl;
    function getDefaultOutgoingPhoneAccount(uriScheme: JString): JPhoneAccountHandle; cdecl;
    function getLine1Number(accountHandle: JPhoneAccountHandle): JString; cdecl;
    function getPhoneAccount(account: JPhoneAccountHandle): JPhoneAccount; cdecl;
    function getSelfManagedPhoneAccounts: JList; cdecl;
    function getSimCallManager: JPhoneAccountHandle; cdecl;
    function getVoiceMailNumber(accountHandle: JPhoneAccountHandle): JString; cdecl;
    function handleMmi(dialString: JString): boolean; cdecl; overload;
    function handleMmi(dialString: JString; accountHandle: JPhoneAccountHandle): boolean; cdecl; overload;
    function isInCall: boolean; cdecl;
    function isInManagedCall: boolean; cdecl;
    function isIncomingCallPermitted(phoneAccountHandle: JPhoneAccountHandle): boolean; cdecl;
    function isOutgoingCallPermitted(phoneAccountHandle: JPhoneAccountHandle): boolean; cdecl;
    function isTtySupported: boolean; cdecl;
    function isVoiceMailNumber(accountHandle: JPhoneAccountHandle; number: JString): boolean; cdecl;
    procedure placeCall(address: Jnet_Uri; extras: JBundle); cdecl;
    procedure registerPhoneAccount(account: JPhoneAccount); cdecl;
    procedure showInCallScreen(showDialpad: boolean); cdecl;
    procedure silenceRinger; cdecl;
    procedure unregisterPhoneAccount(accountHandle: JPhoneAccountHandle); cdecl;
  end;
  TJTelecomManager = class(TJavaGenericImport<JTelecomManagerClass, JTelecomManager>)
  end;

implementation

end.
