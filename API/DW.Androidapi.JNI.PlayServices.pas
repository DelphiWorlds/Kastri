unit DW.Androidapi.JNI.PlayServices;

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

uses
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
  Androidapi.JNI.PlayServices;

type
  JAbstractDataBuffer = interface;
  JCommonStatusCodes = interface;
  JDataBuffer = interface;
  JDataHolder = interface;
  JDataHolder_Builder = interface;
  JEntityBuffer = interface;
  JGoogleApi = interface;
  JGoogleApiClient = interface;
  JListenableFuture = interface;
  JReleasable = interface;
  JScope = interface;

  JScopeClass = interface(JObjectClass)
    ['{92B24AEE-2F21-4421-BEA0-56D622112E1C}']
    {class} function init(scopeUri: JString): JScope; cdecl;
  end;

  [JavaSignature('com/google/android/gms/common/api/Scope')]
  JScope = interface(JObject)
    ['{B2FA592B-4FAF-4B58-9C57-C9AF95F95FB9}']
    function dD: JString; cdecl;
  end;
  TJScope = class(TJavaGenericImport<JScopeClass, JScope>) end;

  JCommonStatusCodesClass = interface(JObjectClass)
    ['{7E439158-D82B-4B03-9A23-6BBB88D90A29}']
    {class} function _GetDATE_INVALID: Integer; cdecl;
    {class} function _GetDEVELOPER_ERROR: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetINTERNAL_ERROR: Integer; cdecl;
    {class} function _GetINTERRUPTED: Integer; cdecl;
    {class} function _GetINVALID_ACCOUNT: Integer; cdecl;
    {class} function _GetLICENSE_CHECK_FAILED: Integer; cdecl;
    {class} function _GetNETWORK_ERROR: Integer; cdecl;
    {class} function _GetRESOLUTION_REQUIRED: Integer; cdecl;
    {class} function _GetSERVICE_DISABLED: Integer; cdecl;
    {class} function _GetSERVICE_INVALID: Integer; cdecl;
    {class} function _GetSERVICE_MISSING: Integer; cdecl;
    {class} function _GetSERVICE_VERSION_UPDATE_REQUIRED: Integer; cdecl;
    {class} function _GetSIGN_IN_REQUIRED: Integer; cdecl;
    {class} function _GetSUCCESS: Integer; cdecl;
    {class} function _GetSUCCESS_CACHE: Integer; cdecl;
    {class} function _GetTIMEOUT: Integer; cdecl;
    {class} function getStatusCodeString(statusCode: Integer): JString; cdecl;
    {class} function init: JCommonStatusCodes; cdecl;
    {class} property DATE_INVALID: Integer read _GetDATE_INVALID;
    {class} property DEVELOPER_ERROR: Integer read _GetDEVELOPER_ERROR;
    {class} property ERROR: Integer read _GetERROR;
    {class} property INTERNAL_ERROR: Integer read _GetINTERNAL_ERROR;
    {class} property INTERRUPTED: Integer read _GetINTERRUPTED;
    {class} property INVALID_ACCOUNT: Integer read _GetINVALID_ACCOUNT;
    {class} property LICENSE_CHECK_FAILED: Integer read _GetLICENSE_CHECK_FAILED;
    {class} property NETWORK_ERROR: Integer read _GetNETWORK_ERROR;
    {class} property RESOLUTION_REQUIRED: Integer read _GetRESOLUTION_REQUIRED;
    {class} property SERVICE_DISABLED: Integer read _GetSERVICE_DISABLED;
    {class} property SERVICE_INVALID: Integer read _GetSERVICE_INVALID;
    {class} property SERVICE_MISSING: Integer read _GetSERVICE_MISSING;
    {class} property SERVICE_VERSION_UPDATE_REQUIRED: Integer read _GetSERVICE_VERSION_UPDATE_REQUIRED;
    {class} property SIGN_IN_REQUIRED: Integer read _GetSIGN_IN_REQUIRED;
    {class} property SUCCESS: Integer read _GetSUCCESS;
    {class} property SUCCESS_CACHE: Integer read _GetSUCCESS_CACHE;
    {class} property TIMEOUT: Integer read _GetTIMEOUT;
  end;

  [JavaSignature('com/google/android/gms/common/api/CommonStatusCodes')]
  JCommonStatusCodes = interface(JObject)
    ['{BDB21EAA-F613-4B7F-A956-629E7F1F3CC9}']
  end;
  TJCommonStatusCodes = class(TJavaGenericImport<JCommonStatusCodesClass, JCommonStatusCodes>) end;

  JReleasableClass = interface(IJavaClass)
    ['{21A21E08-2423-474E-9BA7-77E267BB7F4E}']
  end;

  [JavaSignature('com/google/android/gms/common/api/Releasable')]
  JReleasable = interface(IJavaInstance)
    ['{8E1EDA1A-EFF2-4F94-8B56-D1AD561BFCD4}']
    procedure release; cdecl;
  end;
  TJReleasable = class(TJavaGenericImport<JReleasableClass, JReleasable>)
  end;

  JDataBufferClass = interface(JReleasableClass)
    ['{49C7D692-3653-40EB-B4D9-256184DAFDC3}']
  end;

  [JavaSignature('com/google/android/gms/common/data/DataBuffer')]
  JDataBuffer = interface(JReleasable)
    ['{E66E2584-412C-4878-B2BF-22E564A67E5B}']
    procedure close; cdecl;
    function &get(i: Integer): JObject; cdecl;
    function getCount: Integer; cdecl;
    function getMetadata: JBundle; cdecl;
    function isClosed: Boolean; cdecl;
    function iterator: JIterator; cdecl;
    procedure release; cdecl;
    function singleRefIterator: JIterator; cdecl;
  end;
  TJDataBuffer = class(TJavaGenericImport<JDataBufferClass, JDataBuffer>) end;

  JAbstractDataBufferClass = interface(JDataBufferClass)
    ['{B879D309-A87F-4408-83D7-8C1749CAEC10}']
    {class} function _GetmDataHolder: JDataHolder; cdecl;
    {class} property mDataHolder: JDataHolder read _GetmDataHolder;
  end;

  [JavaSignature('com/google/android/gms/common/data/AbstractDataBuffer')]
  JAbstractDataBuffer = interface(JDataBuffer)
    ['{BC41948D-601F-427D-B986-1E82BBF2DD92}']
    procedure close; cdecl;
    function &get(i: Integer): JObject; cdecl;
    function getCount: Integer; cdecl;
    function getMetadata: JBundle; cdecl;
    function isClosed: Boolean; cdecl;
    function iterator: JIterator; cdecl;
    procedure release; cdecl;
    function singleRefIterator: JIterator; cdecl;
  end;
  TJAbstractDataBuffer = class(TJavaGenericImport<JAbstractDataBufferClass, JAbstractDataBuffer>) end;

  JDataHolderClass = interface(JAbstractSafeParcelableClass)
    ['{A4454701-C03F-4AEC-B610-92DB2EF77521}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function builder(string_: TJavaObjectArray<JString>): JDataHolder_Builder; cdecl;
    {class} function empty(i: Integer): JDataHolder; cdecl;
    {class} function init(cursor: JCursor; i: Integer; bundle: JBundle): JDataHolder; cdecl; overload;
    {class} function init(string_: TJavaObjectArray<JString>; cursorWindow: TJavaObjectArray<JCursorWindow>; i: Integer;
      bundle: JBundle): JDataHolder; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/common/data/DataHolder')]
  JDataHolder = interface(JAbstractSafeParcelable)
    ['{D1CA2B4D-272B-428F-96B7-64EAFB770E51}']
    procedure close; cdecl;
    function getBoolean(string_: JString; i: Integer; i1: Integer): Boolean; cdecl;
    function getByteArray(string_: JString; i: Integer; i1: Integer): TJavaArray<Byte>; cdecl;
    function getCount: Integer; cdecl;
    function getInteger(string_: JString; i: Integer; i1: Integer): Integer; cdecl;
    function getLong(string_: JString; i: Integer; i1: Integer): Int64; cdecl;
    function getMetadata: JBundle; cdecl;
    function getStatusCode: Integer; cdecl;
    function getString(string_: JString; i: Integer; i1: Integer): JString; cdecl;
    function getWindowIndex(i: Integer): Integer; cdecl;
    function hasColumn(string_: JString): Boolean; cdecl;
    function hasNull(string_: JString; i: Integer; i1: Integer): Boolean; cdecl;
    function isClosed: Boolean; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJDataHolder = class(TJavaGenericImport<JDataHolderClass, JDataHolder>) end;

  JDataHolder_BuilderClass = interface(JObjectClass)
    ['{E93B37D3-B530-4551-A9C3-015AB867E2AF}']
  end;

  [JavaSignature('com/google/android/gms/common/data/DataHolder$Builder')]
  JDataHolder_Builder = interface(JObject)
    ['{AC62943F-A09F-4D21-9A7E-28B5EC92D603}']
    function build(i: Integer): JDataHolder; cdecl; overload;
    function build(i: Integer; bundle: JBundle): JDataHolder; cdecl; overload;
    function withRow(contentValues: JContentValues): JDataHolder_Builder; cdecl;
    function zaa(hashMap: JHashMap): JDataHolder_Builder; cdecl; overload;
  end;
  TJDataHolder_Builder = class(TJavaGenericImport<JDataHolder_BuilderClass, JDataHolder_Builder>) end;

  JEntityBufferClass = interface(JAbstractDataBufferClass)
    ['{BA9A749B-976C-4868-A57B-FC42A6F67F01}']
  end;

  [JavaSignature('com/google/android/gms/common/data/EntityBuffer')]
  JEntityBuffer = interface(JAbstractDataBuffer)
    ['{8A970E67-9B7B-4304-BED2-C8DFF657A55E}']
    function &get(i: Integer): JObject; cdecl;
    function getCount: Integer; cdecl;
  end;
  TJEntityBuffer = class(TJavaGenericImport<JEntityBufferClass, JEntityBuffer>) end;

  JGoogleApiClass = interface(JObjectClass)
    ['{28324BE6-4AEF-463E-8646-30A441837368}']
  end;

  [JavaSignature('com/google/android/gms/common/api/GoogleApi')]
  JGoogleApi = interface(JObject)
    ['{A2B4ED5C-541B-4147-A11A-C310C160EEB1}']
  end;
  TJGoogleApi = class(TJavaGenericImport<JGoogleApiClass, JGoogleApi>) end;

  JListenableFutureClass = interface(JFutureClass)
    ['{A6F66DCA-59FF-4816-A8BA-5BDD38CD4446}']
  end;

  [JavaSignature('com/google/common/util/concurrent/ListenableFuture')]
  JListenableFuture = interface(JFuture)
    ['{2DED2515-A293-499A-8B5C-243BC591051A}']
    procedure addListener(runnable: JRunnable; executor: JExecutor); cdecl;
  end;
  TJListenableFuture = class(TJavaGenericImport<JListenableFutureClass, JListenableFuture>) end;

  JGoogleApiClientClass = interface(JObjectClass)
    ['{39FBEEC6-35DC-4004-8C87-A46FF41D4B0C}']
    {class} function _GetDEFAULT_ACCOUNT: JString; cdecl;
    {class} function _GetSIGN_IN_MODE_OPTIONAL: Integer; cdecl;
    {class} function _GetSIGN_IN_MODE_REQUIRED: Integer; cdecl;
    {class} procedure dumpAll(string_: JString; fileDescriptor: JFileDescriptor; printWriter: JPrintWriter; string_1: TJavaObjectArray<JString>); cdecl;
    {class} function getAllClients: JSet; cdecl;
    {class} function init: JGoogleApiClient; cdecl;
    {class} property DEFAULT_ACCOUNT: JString read _GetDEFAULT_ACCOUNT;
    {class} property SIGN_IN_MODE_OPTIONAL: Integer read _GetSIGN_IN_MODE_OPTIONAL;
    {class} property SIGN_IN_MODE_REQUIRED: Integer read _GetSIGN_IN_MODE_REQUIRED;
  end;

  [JavaSignature('com/google/android/gms/common/api/GoogleApiClient')]
  JGoogleApiClient = interface(JObject)
    ['{79A91917-155C-4C04-8E26-2C497F3CE53D}']
    // **** Just a placeholder class, for now ****
  end;
  TJGoogleApiClient = class(TJavaGenericImport<JGoogleApiClientClass, JGoogleApiClient>) end;

implementation

end.
