unit DW.Androidapi.JNI.Android.Util;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JAttributeSet = interface;
  JBase64 = interface;
  JLog = interface;
  JRational = interface;
  JSparseLongArray = interface;

  JAttributeSetClass = interface(IJavaClass)
    ['{78097460-34FF-4C38-AD82-8907F329DD5B}']
  end;

  [JavaSignature('android/util/AttributeSet')]
  JAttributeSet = interface(IJavaInstance)
    ['{714A2C49-A8B6-4ECB-B48F-A6EFC124CD03}']
    function getAttributeBooleanValue(string_1: JString; string_2: JString; boolean: Boolean): Boolean; cdecl; overload;
    function getAttributeBooleanValue(int: Integer; boolean: Boolean): Boolean; cdecl; overload;
    function getAttributeCount: Integer; cdecl;
    function getAttributeFloatValue(string_1: JString; string_2: JString; float: Single): Single; cdecl; overload;
    function getAttributeFloatValue(int: Integer; float: Single): Single; cdecl; overload;
    function getAttributeIntValue(string_1: JString; string_2: JString; int: Integer): Integer; cdecl; overload;
    function getAttributeIntValue(int: Integer; int_1: Integer): Integer; cdecl; overload;
    function getAttributeListValue(int: Integer; strings: TJavaObjectArray<JString>; int_1: Integer): Integer; cdecl; overload;
    function getAttributeListValue(string_1: JString; string_2: JString; strings: TJavaObjectArray<JString>; int: Integer): Integer; cdecl; overload;
    function getAttributeName(int: Integer): JString; cdecl;
    function getAttributeNameResource(int: Integer): Integer; cdecl;
    function getAttributeNamespace(int: Integer): JString; cdecl;
    function getAttributeResourceValue(string_1: JString; string_2: JString; int: Integer): Integer; cdecl; overload;
    function getAttributeResourceValue(int: Integer; int_1: Integer): Integer; cdecl; overload;
    function getAttributeUnsignedIntValue(int: Integer; int_1: Integer): Integer; cdecl; overload;
    function getAttributeUnsignedIntValue(string_1: JString; string_2: JString; int: Integer): Integer; cdecl; overload;
    function getAttributeValue(int: Integer): JString; cdecl; overload;
    function getAttributeValue(string_1: JString; string_2: JString): JString; cdecl; overload;
    function getClassAttribute: JString; cdecl;
    function getIdAttribute: JString; cdecl;
    function getIdAttributeResourceValue(int: Integer): Integer; cdecl;
    function getPositionDescription: JString; cdecl;
    function getStyleAttribute: Integer; cdecl;
  end;
  TJAttributeSet = class(TJavaGenericImport<JAttributeSetClass, JAttributeSet>) end;

  JBase64Class = interface(JObjectClass)
    ['{DA78E162-4CB0-455B-ACC0-BEDDE5484C85}']
    function _GetCRLF: Integer; cdecl;
    function _GetDEFAULT: Integer; cdecl;
    function _GetNO_CLOSE: Integer; cdecl;
    function _GetNO_PADDING: Integer; cdecl;
    function _GetNO_WRAP: Integer; cdecl;
    function _GetURL_SAFE: Integer; cdecl;
    function decode(input: TJavaArray<Byte>; flags: Integer): TJavaArray<Byte>; cdecl; overload;
    function decode(input: TJavaArray<Byte>; offset: Integer; len: Integer; flags: Integer): TJavaArray<Byte>; cdecl; overload;
    function decode(str: JString; flags: Integer): TJavaArray<Byte>; cdecl; overload;
    function encode(input: TJavaArray<Byte>; flags: Integer): TJavaArray<Byte>; cdecl; overload;
    function encode(input: TJavaArray<Byte>; offset: Integer; len: Integer; flags: Integer): TJavaArray<Byte>; cdecl; overload;
    function encodeToString(input: TJavaArray<Byte>; flags: Integer): JString; cdecl; overload;
    function encodeToString(input: TJavaArray<Byte>; offset: Integer; len: Integer; flags: Integer): JString; cdecl; overload;
    property &DEFAULT: Integer read _GetDEFAULT;
    property CRLF: Integer read _GetCRLF;
    property NO_CLOSE: Integer read _GetNO_CLOSE;
    property NO_PADDING: Integer read _GetNO_PADDING;
    property NO_WRAP: Integer read _GetNO_WRAP;
    property URL_SAFE: Integer read _GetURL_SAFE;
  end;

  [JavaSignature('android/util/Base64')]
  JBase64 = interface(JObject)
    ['{AC976256-7237-4F07-87CD-B57F5F28BF6A}']
  end;
  TJBase64 = class(TJavaGenericImport<JBase64Class, JBase64>)
  end;

  JLogClass = interface(JObjectClass)
    ['{62108FE8-1DBB-4C4F-A0C7-35D12BD116DC}']
    {class} function _GetASSERT: Integer; cdecl;
    {class} function _GetDEBUG: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetINFO: Integer; cdecl;
    {class} function _GetVERBOSE: Integer; cdecl;
    {class} function _GetWARN: Integer; cdecl;
    {class} function d(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function getStackTraceString(tr: JThrowable): JString; cdecl;
    {class} function i(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function isLoggable(tag: JString; level: Integer): Boolean; cdecl;
    {class} function println(priority: Integer; tag: JString; msg: JString): Integer; cdecl;
    {class} function v(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function v(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function wtf(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} property ASSERT: Integer read _GetASSERT;
    {class} property DEBUG: Integer read _GetDEBUG;
    {class} property ERROR: Integer read _GetERROR;
    {class} property INFO: Integer read _GetINFO;
    {class} property VERBOSE: Integer read _GetVERBOSE;
    {class} property WARN: Integer read _GetWARN;
  end;

  [JavaSignature('android/util/Log')]
  JLog = interface(JObject)
    ['{6A5EC34E-CB76-4AB0-A11D-7CCB3B40C571}']
  end;
  TJLog = class(TJavaGenericImport<JLogClass, JLog>) end;

  JRationalClass = interface(JNumberClass)
    ['{592776A4-0BC8-4829-88B1-714E90C34FE1}']
    {class} function _GetNEGATIVE_INFINITY: JRational; cdecl;
    {class} function _GetNaN: JRational; cdecl;
    {class} function _GetPOSITIVE_INFINITY: JRational; cdecl;
    {class} function _GetZERO: JRational; cdecl;
    {class} function init(numerator: Integer; denominator: Integer): JRational; cdecl;
    {class} function compareTo(another: JRational): Integer; cdecl;
    {class} function doubleValue: Double; cdecl;
    {class} function getNumerator: Integer; cdecl;
    {class} function hashCode: Integer; cdecl;
    {class} function intValue: Integer; cdecl;
    {class} function isZero: Boolean; cdecl;
    {class} function longValue: Int64; cdecl;
    {class} function parseRational(&string: JString): JRational; cdecl;
    {class} property NEGATIVE_INFINITY: JRational read _GetNEGATIVE_INFINITY;
    {class} property NaN: JRational read _GetNaN;
    {class} property POSITIVE_INFINITY: JRational read _GetPOSITIVE_INFINITY;
    {class} property ZERO: JRational read _GetZERO;
  end;

  [JavaSignature('android/util/Rational')]
  JRational = interface(JNumber)
    ['{5DEFEB1B-2D1F-4ADF-B69B-54B0498F8544}']
    function equals(obj: JObject): Boolean; cdecl;
    function floatValue: Single; cdecl;
    function getDenominator: Integer; cdecl;
    function isFinite: Boolean; cdecl;
    function isInfinite: Boolean; cdecl;
    function isNaN: Boolean; cdecl;
    function shortValue: SmallInt; cdecl;
    function toString: JString; cdecl;
  end;
  TJRational = class(TJavaGenericImport<JRationalClass, JRational>) end;

  JSparseLongArrayClass = interface(JObjectClass)
    ['{14F11EEB-F83B-448E-B265-2EB4505111F9}']
    function init: JSparseLongArray; cdecl; overload;
    function init(initialCapacity: Integer): JSparseLongArray; cdecl; overload;
  end;

  [JavaSignature('android/util/SparseLongArray')]
  JSparseLongArray = interface(JObject)
    ['{DB34075F-EE91-45AC-A3DF-8BBF41678CC2}']
    function clone: JSparseLongArray; cdecl;
    function get(key: Integer): Int64; cdecl; overload;
    function get(key: Integer; valueIfKeyNotFound: Int64): Int64; cdecl; overload;
    function indexOfKey(key: Integer): Integer; cdecl;
    function indexOfValue(value: Int64): Integer; cdecl;
    function keyAt(&index: Integer): Integer; cdecl;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
    function valueAt(&index: Integer): Int64; cdecl;
    procedure append(key: Integer; value: Int64); cdecl;
    procedure clear ; cdecl;
    procedure delete(key: Integer); cdecl;
    procedure put(key: Integer; value: Int64); cdecl;
    procedure removeAt(&index: Integer); cdecl;
  end;
  TJSparseLongArray = class(TJavaGenericImport<JSparseLongArrayClass, JSparseLongArray>) end;

implementation

end.
