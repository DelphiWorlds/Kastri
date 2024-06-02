unit DW.Androidapi.JNI.AndroidX.Media3.Datasource;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net,
  // DW
  DW.Androidapi.JNI.AndroidX.Media3.Common;

type
  JDataSource = interface;
  JDataSpec = interface;
  JDataSpec_Builder = interface;
  JTransferListener = interface;

  JTransferListenerClass = interface(IJavaClass)
    ['{AD65F04D-3D08-4501-932C-B1D04F3A6222}']
  end;

  [JavaSignature('androidx/media3/datasource/TransferListener')]
  JTransferListener = interface(IJavaInstance)
    ['{C99D4830-00E8-450F-AD1B-F90104BA9492}']
    procedure onBytesTransferred(datasource: JDataSource; dataspec: JDataSpec; boolean: Boolean; int_1: Integer); cdecl;
    procedure onTransferEnd(datasource: JDataSource; dataspec: JDataSpec; boolean: Boolean); cdecl;
    procedure onTransferInitializing(datasource: JDataSource; dataspec: JDataSpec; boolean: Boolean); cdecl;
    procedure onTransferStart(datasource: JDataSource; dataspec: JDataSpec; boolean: Boolean); cdecl;
  end;
  TJTransferListener = class(TJavaGenericImport<JTransferListenerClass, JTransferListener>) end;

  JDataSpecClass = interface(JObjectClass)
    ['{C7F888D8-C141-4FA0-A73D-72C218324F41}']
    {class} function _GetFLAG_ALLOW_CACHE_FRAGMENTATION: Integer; cdecl;
    {class} function _GetFLAG_ALLOW_GZIP: Integer; cdecl;
    {class} function _GetFLAG_DONT_CACHE_IF_LENGTH_UNKNOWN: Integer; cdecl;
    {class} function _GetFLAG_MIGHT_NOT_USE_FULL_NETWORK_SPEED: Integer; cdecl;
    {class} function _GetHTTP_METHOD_GET: Integer; cdecl;
    {class} function _GetHTTP_METHOD_HEAD: Integer; cdecl;
    {class} function _GetHTTP_METHOD_POST: Integer; cdecl;
    {class} function getStringForHttpMethod(int: Integer): JString; cdecl;
    {class} function init(uri: Jnet_Uri; long: Int64; long_1: Int64; string_1: JString; int: Integer): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri; long: Int64; long_1: Int64; long_2: Int64; string_1: JString; int: Integer): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri; long: Int64; long_1: Int64; string_1: JString; int: Integer; map: JMap): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri; int: Integer; bytes: TJavaArray<Byte>; long: Int64; long_1: Int64; long_2: Int64; string_1: JString; int_1: Integer): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri; bytes: TJavaArray<Byte>; long: Int64; long_1: Int64; long_2: Int64; string_1: JString; int: Integer): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri; int: Integer; bytes: TJavaArray<Byte>; long: Int64; long_1: Int64; long_2: Int64; string_1: JString; int_1: Integer; map: JMap): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri; long: Int64; long_1: Int64; string_1: JString): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri; long: Int64; long_1: Int64): JDataSpec; cdecl; overload;
    {class} function init(uri: Jnet_Uri; int: Integer): JDataSpec; cdecl; overload;
    {class} property FLAG_ALLOW_CACHE_FRAGMENTATION: Integer read _GetFLAG_ALLOW_CACHE_FRAGMENTATION;
    {class} property FLAG_ALLOW_GZIP: Integer read _GetFLAG_ALLOW_GZIP;
    {class} property FLAG_DONT_CACHE_IF_LENGTH_UNKNOWN: Integer read _GetFLAG_DONT_CACHE_IF_LENGTH_UNKNOWN;
    {class} property FLAG_MIGHT_NOT_USE_FULL_NETWORK_SPEED: Integer read _GetFLAG_MIGHT_NOT_USE_FULL_NETWORK_SPEED;
    {class} property HTTP_METHOD_GET: Integer read _GetHTTP_METHOD_GET;
    {class} property HTTP_METHOD_HEAD: Integer read _GetHTTP_METHOD_HEAD;
    {class} property HTTP_METHOD_POST: Integer read _GetHTTP_METHOD_POST;
  end;

  [JavaSignature('androidx/media3/datasource/DataSpec')]
  JDataSpec = interface(JObject)
    ['{78179D61-CAB7-44BD-8198-573348011058}']
    function _GetabsoluteStreamPosition: Int64; cdecl;
    function _GetcustomData: JObject; cdecl;
    function _Getflags: Integer; cdecl;
    function _GethttpBody: TJavaArray<Byte>; cdecl;
    function _GethttpMethod: Integer; cdecl;
    function _GethttpRequestHeaders: JMap; cdecl;
    function _Getkey: JString; cdecl;
    function _Getlength: Int64; cdecl;
    function _Getposition: Int64; cdecl;
    function _Geturi: Jnet_Uri; cdecl;
    function _GeturiPositionOffset: Int64; cdecl;
    function buildUpon: JDataSpec_Builder; cdecl;
    function getHttpMethodString: JString; cdecl;
    function isFlagSet(int: Integer): Boolean; cdecl;
    function subrange(long: Int64; long_1: Int64): JDataSpec; cdecl; overload;
    function subrange(long: Int64): JDataSpec; cdecl; overload;
    function toString: JString; cdecl;
    function withAdditionalHeaders(map: JMap): JDataSpec; cdecl;
    function withRequestHeaders(map: JMap): JDataSpec; cdecl;
    function withUri(uri: Jnet_Uri): JDataSpec; cdecl;
    property absoluteStreamPosition: Int64 read _GetabsoluteStreamPosition;
    property customData: JObject read _GetcustomData;
    property flags: Integer read _Getflags;
    property httpBody: TJavaArray<Byte> read _GethttpBody;
    property httpMethod: Integer read _GethttpMethod;
    property httpRequestHeaders: JMap read _GethttpRequestHeaders;
    property key: JString read _Getkey;
    property length: Int64 read _Getlength;
    property position: Int64 read _Getposition;
    property uri: Jnet_Uri read _Geturi;
    property uriPositionOffset: Int64 read _GeturiPositionOffset;
  end;
  TJDataSpec = class(TJavaGenericImport<JDataSpecClass, JDataSpec>) end;

  JDataSpec_BuilderClass = interface(JObjectClass)
    ['{D9CA7437-CF57-4A4D-BA65-42E876B981C5}']
    {class} function init: JDataSpec_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/datasource/DataSpec$Builder')]
  JDataSpec_Builder = interface(JObject)
    ['{BD4545D6-113B-44CD-B610-D3AF11636EB9}']
    function build: JDataSpec; cdecl;
    function setCustomData(object_1: JObject): JDataSpec_Builder; cdecl;
    function setFlags(int: Integer): JDataSpec_Builder; cdecl;
    function setHttpBody(bytes: TJavaArray<Byte>): JDataSpec_Builder; cdecl;
    function setHttpMethod(int: Integer): JDataSpec_Builder; cdecl;
    function setHttpRequestHeaders(map: JMap): JDataSpec_Builder; cdecl;
    function setKey(string_1: JString): JDataSpec_Builder; cdecl;
    function setLength(long: Int64): JDataSpec_Builder; cdecl;
    function setPosition(long: Int64): JDataSpec_Builder; cdecl;
    function setUri(string_1: JString): JDataSpec_Builder; cdecl; overload;
    function setUri(uri: Jnet_Uri): JDataSpec_Builder; cdecl; overload;
    function setUriPositionOffset(long: Int64): JDataSpec_Builder; cdecl;
  end;
  TJDataSpec_Builder = class(TJavaGenericImport<JDataSpec_BuilderClass, JDataSpec_Builder>) end;

  JDataSourceClass = interface(JDataReaderClass)
    ['{E57A2812-3012-4239-8A0B-57509B4332F1}']
  end;

  [JavaSignature('androidx/media3/datasource/DataSource')]
  JDataSource = interface(JDataReader)
    ['{718D6886-EA2A-4E59-B3F3-402CB9E2B6D9}']
    procedure addTransferListener(transferlistener: JTransferListener); cdecl;
    procedure close; cdecl;
    function getResponseHeaders: JMap; cdecl;
    function getUri: Jnet_Uri; cdecl;
    function open(dataspec: JDataSpec): Int64; cdecl;
  end;
  TJDataSource = class(TJavaGenericImport<JDataSourceClass, JDataSource>) end;

implementation

end.
