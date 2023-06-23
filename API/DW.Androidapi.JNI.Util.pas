unit DW.Androidapi.JNI.Util;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JArrayDeque = interface;
  JBase64 = interface;
  JLinkedHashSet = interface;
  JMap_Entry = interface;
  JNavigableMap = interface;
  JNavigableSet = interface;
  JRational = interface;
  JSortedSet = interface;
  JTimerTask = interface;
  JTimer = interface;
  JTreeMap = interface;
  Jutil_Log = interface;

  JArrayDequeClass = interface(JAbstractCollectionClass)
    ['{53FE7FDB-6B49-4ABF-AC02-0871A9E52A05}']
    {class} function init: JArrayDeque; cdecl; overload;
    {class} function init(numElements: Integer): JArrayDeque; cdecl; overload;
    {class} function init(c: JCollection): JArrayDeque; cdecl; overload;
    {class} function add(e: JObject): Boolean; cdecl;
    {class} procedure addFirst(e: JObject); cdecl;
    {class} function &contains(o: JObject): Boolean; cdecl;
    {class} function descendingIterator: JIterator; cdecl;
    {class} function element: JObject; cdecl;
    {class} function isEmpty: Boolean; cdecl;
    {class} function iterator: JIterator; cdecl;
    {class} function offer(e: JObject): Boolean; cdecl;
    {class} function peekFirst: JObject; cdecl;
    {class} function peekLast: JObject; cdecl;
    {class} function poll: JObject; cdecl;
    {class} procedure push(e: JObject); cdecl;
    {class} function remove: JObject; cdecl; overload;
    {class} function remove(o: JObject): Boolean; cdecl; overload;
    {class} function removeLastOccurrence(o: JObject): Boolean; cdecl;
    {class} function size: Integer; cdecl;
    {class} function toArray: TJavaObjectArray<JObject>; cdecl; overload;
  end;

  [JavaSignature('java/util/ArrayDeque')]
  JArrayDeque = interface(JAbstractCollection)
    ['{889EBA35-40DC-40AC-923B-40E252FBA7FB}']
    procedure addLast(e: JObject); cdecl;
    procedure clear; cdecl;
    function clone: JArrayDeque; cdecl;
    function getFirst: JObject; cdecl;
    function getLast: JObject; cdecl;
    function offerFirst(e: JObject): Boolean; cdecl;
    function offerLast(e: JObject): Boolean; cdecl;
    function peek: JObject; cdecl;
    function pollFirst: JObject; cdecl;
    function pollLast: JObject; cdecl;
    function pop: JObject; cdecl;
    function removeFirst: JObject; cdecl;
    function removeFirstOccurrence(o: JObject): Boolean; cdecl;
    function removeLast: JObject; cdecl;
    function toArray(a: TJavaObjectArray<JObject>): TJavaObjectArray<JObject>; cdecl; overload;
  end;
  TJArrayDeque = class(TJavaGenericImport<JArrayDequeClass, JArrayDeque>) end;

  JSortedSetClass = interface(JSetClass)
    ['{D632DFD2-D924-463C-8111-EEB33B490B09}']
    {class} function last: JObject; cdecl;
    {class} function subSet(start: JObject; end_: JObject): JSortedSet; cdecl;
    {class} function tailSet(start: JObject): JSortedSet; cdecl;
  end;

  [JavaSignature('java/util/SortedSet')]
  JSortedSet = interface(JSet)
    ['{D812E9E6-D0D0-4974-9E17-20601495F86C}']
    function comparator: JComparator; cdecl;
    function first: JObject; cdecl;
    function headSet(end_: JObject): JSortedSet; cdecl;
  end;
  TJSortedSet = class(TJavaGenericImport<JSortedSetClass, JSortedSet>) end;

  JNavigableSetClass = interface(JSortedSetClass)
    ['{6BB98CBF-284D-4FC5-A792-09DBB1168402}']
    {class} function ceiling(e: JObject): JObject; cdecl;
    {class} function headSet(toElement: JObject; inclusive: Boolean): JNavigableSet; cdecl; overload;
    {class} function headSet(toElement: JObject): JSortedSet; cdecl; overload;
    {class} function pollFirst: JObject; cdecl;
    {class} function pollLast: JObject; cdecl;
    {class} function subSet(fromElement: JObject; fromInclusive: Boolean; toElement: JObject; toInclusive: Boolean): JNavigableSet; cdecl; overload;
  end;

  [JavaSignature('java/util/NavigableSet')]
  JNavigableSet = interface(JSortedSet)
    ['{FFE5FDC3-1071-413A-9D8D-4873ECBE3685}']
    function descendingIterator: JIterator; cdecl;
    function descendingSet: JNavigableSet; cdecl;
    function floor(e: JObject): JObject; cdecl;
    function higher(e: JObject): JObject; cdecl;
    function iterator: JIterator; cdecl;
    function lower(e: JObject): JObject; cdecl;
    function subSet(fromElement: JObject; toElement: JObject): JSortedSet; cdecl; overload;
    function tailSet(fromElement: JObject; inclusive: Boolean): JNavigableSet; cdecl; overload;
    function tailSet(fromElement: JObject): JSortedSet; cdecl; overload;
  end;
  TJNavigableSet = class(TJavaGenericImport<JNavigableSetClass, JNavigableSet>) end;

  JMap_EntryClass = interface(IJavaClass)
    ['{CC37442E-BEA1-4D74-827E-0C89FA654831}']
    {class} function equals(object_: JObject): Boolean; cdecl;
    {class} function getKey: JObject; cdecl;
    {class} function getValue: JObject; cdecl;
  end;

  [JavaSignature('java/util/Map$Entry')]
  JMap_Entry = interface(IJavaInstance)
    ['{9D6A0684-A3FC-4629-A08A-36EDDA31753D}']
    function hashCode: Integer; cdecl;
    function setValue(object_: JObject): JObject; cdecl;
  end;
  TJMap_Entry = class(TJavaGenericImport<JMap_EntryClass, JMap_Entry>) end;

  JNavigableMapClass = interface(JSortedMapClass)
    ['{27C8F820-A5B6-4C9E-AC50-46708A7506F5}']
    {class} function ceilingEntry(key: JObject): JMap_Entry; cdecl;
    {class} function ceilingKey(key: JObject): JObject; cdecl;
    {class} function floorEntry(key: JObject): JMap_Entry; cdecl;
    {class} function floorKey(key: JObject): JObject; cdecl;
    {class} function headMap(toKey: JObject; inclusive: Boolean): JNavigableMap; cdecl; overload;
    {class} function lastEntry: JMap_Entry; cdecl;
    {class} function lowerEntry(key: JObject): JMap_Entry; cdecl;
    {class} function lowerKey(key: JObject): JObject; cdecl;
    {class} function pollLastEntry: JMap_Entry; cdecl;
    {class} function subMap(fromKey: JObject; fromInclusive: Boolean; toKey: JObject; toInclusive: Boolean): JNavigableMap; cdecl; overload;
    {class} function subMap(fromKey: JObject; toKey: JObject): JSortedMap; cdecl; overload;
  end;

  [JavaSignature('java/util/NavigableMap')]
  JNavigableMap = interface(JSortedMap)
    ['{A922D082-DCAE-41C9-ABC7-6B01F0B2378C}']
    function descendingKeySet: JNavigableSet; cdecl;
    function descendingMap: JNavigableMap; cdecl;
    function firstEntry: JMap_Entry; cdecl;
    function headMap(toKey: JObject): JSortedMap; cdecl; overload;
    function higherEntry(key: JObject): JMap_Entry; cdecl;
    function higherKey(key: JObject): JObject; cdecl;
    function navigableKeySet: JNavigableSet; cdecl;
    function pollFirstEntry: JMap_Entry; cdecl;
    function tailMap(fromKey: JObject; inclusive: Boolean): JNavigableMap; cdecl; overload;
    function tailMap(fromKey: JObject): JSortedMap; cdecl; overload;
  end;
  TJNavigableMap = class(TJavaGenericImport<JNavigableMapClass, JNavigableMap>) end;

  JTreeMapClass = interface(JAbstractMapClass)
    ['{6EFAF8FF-6DD5-46E5-B4C7-BCD5E00B6173}']
    {class} function init: JTreeMap; cdecl; overload;
    {class} function init(copyFrom: JMap): JTreeMap; cdecl; overload;
    {class} function init(comparator: JComparator): JTreeMap; cdecl; overload;
    {class} function init(copyFrom: JSortedMap): JTreeMap; cdecl; overload;
    {class} function ceilingEntry(key: JObject): JMap_Entry; cdecl;
    {class} function comparator: JComparator; cdecl;
    {class} function containsKey(key: JObject): Boolean; cdecl;
    {class} function descendingKeySet: JNavigableSet; cdecl;
    {class} function firstKey: JObject; cdecl;
    {class} function floorEntry(key: JObject): JMap_Entry; cdecl;
    {class} function floorKey(key: JObject): JObject; cdecl;
    {class} function higherEntry(key: JObject): JMap_Entry; cdecl;
    {class} function higherKey(key: JObject): JObject; cdecl;
    {class} function isEmpty: Boolean; cdecl;
    {class} function lowerEntry(key: JObject): JMap_Entry; cdecl;
    {class} function lowerKey(key: JObject): JObject; cdecl;
    {class} function navigableKeySet: JNavigableSet; cdecl;
    {class} function put(key: JObject; value: JObject): JObject; cdecl;
    {class} function remove(key: JObject): JObject; cdecl;
    {class} function size: Integer; cdecl;
    {class} function tailMap(fromInclusive: JObject): JSortedMap; cdecl; overload;
  end;

  [JavaSignature('java/util/TreeMap')]
  JTreeMap = interface(JAbstractMap)
    ['{659B2DE2-814C-454C-B7B9-BB38FE5F836B}']
    function ceilingKey(key: JObject): JObject; cdecl;
    procedure clear; cdecl;
    function clone: JObject; cdecl;
    function descendingMap: JNavigableMap; cdecl;
    function entrySet: JSet; cdecl;
    function firstEntry: JMap_Entry; cdecl;
    function &get(key: JObject): JObject; cdecl;
    function headMap(to_: JObject; inclusive: Boolean): JNavigableMap; cdecl; overload;
    function headMap(toExclusive: JObject): JSortedMap; cdecl; overload;
    function keySet: JSet; cdecl;
    function lastEntry: JMap_Entry; cdecl;
    function lastKey: JObject; cdecl;
    function pollFirstEntry: JMap_Entry; cdecl;
    function pollLastEntry: JMap_Entry; cdecl;
    function subMap(from: JObject; fromInclusive: Boolean; to_: JObject; toInclusive: Boolean): JNavigableMap; cdecl; overload;
    function subMap(fromInclusive: JObject; toExclusive: JObject): JSortedMap; cdecl; overload;
    function tailMap(from: JObject; inclusive: Boolean): JNavigableMap; cdecl; overload;
  end;
  TJTreeMap = class(TJavaGenericImport<JTreeMapClass, JTreeMap>) end;

  JLinkedHashSetClass = interface(JHashSetClass)
    ['{55F18D87-F52A-42CF-A679-528B2A46B0AB}']
    {class} function init: JLinkedHashSet; cdecl; overload;
    {class} function init(capacity: Integer): JLinkedHashSet; cdecl; overload;
    {class} function init(capacity: Integer; loadFactor: Single): JLinkedHashSet; cdecl; overload;
    {class} function init(collection: JCollection): JLinkedHashSet; cdecl; overload;
  end;

  [JavaSignature('java/util/LinkedHashSet')]
  JLinkedHashSet = interface(JHashSet)
    ['{5B386475-6963-49C7-978E-77F8849ABC39}']
  end;
  TJLinkedHashSet = class(TJavaGenericImport<JLinkedHashSetClass, JLinkedHashSet>) end;

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

  JTimerClass = interface(JObjectClass)
    ['{07C8270D-52FF-4B70-B364-A4E86A4F3411}']
    function init: JTimer; cdecl; overload;
    function init(name: JString): JTimer; cdecl; overload;
    function init(name: JString; isDaemon: boolean): JTimer; cdecl; overload;
    function init(isDaemon: boolean): JTimer; cdecl; overload;
  end;

  [JavaSignature('java/util/Timer')]
  JTimer = interface(JObject)
    ['{25D25103-F3A3-417F-AE53-7B434258D54D}']
    procedure cancel; cdecl;
    function purge: Integer; cdecl;
    procedure schedule(task: JTimerTask; delay: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; delay: Int64; period: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; firstTime: JDate; period: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; time: JDate); cdecl; overload;
    procedure scheduleAtFixedRate(task: JTimerTask; delay: Int64; period: Int64); cdecl; overload;
    procedure scheduleAtFixedRate(task: JTimerTask; firstTime: JDate; period: Int64); cdecl; overload;
  end;
  TJTimer = class(TJavaGenericImport<JTimerClass, JTimer>)
  end;

  JTimerTaskClass = interface(JObjectClass)
    ['{8A91DFA8-92B7-49CE-88DB-931B6D4D679F}']
  end;

  [JavaSignature('java/util/TimerTask')]
  JTimerTask = interface(JObject)
    ['{E5CEEEE4-88C0-4488-9E7C-86249258C30E}']
    function cancel: boolean; cdecl;
    procedure run; cdecl;
    function scheduledExecutionTime: Int64; cdecl;
  end;
  TJTimerTask = class(TJavaGenericImport<JTimerTaskClass, JTimerTask>)
  end;

  Jutil_LogClass = interface(JObjectClass)
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
  Jutil_Log = interface(JObject)
    ['{6A5EC34E-CB76-4AB0-A11D-7CCB3B40C571}']
  end;
  TJutil_Log = class(TJavaGenericImport<Jutil_LogClass, Jutil_Log>) end;


implementation

end.
