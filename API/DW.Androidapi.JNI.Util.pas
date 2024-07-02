unit DW.Androidapi.JNI.Util;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.Os,
  // DW
  DW.Androidapi.JNI.Concurrent;

type
  JArrayDeque = interface;
  JExecutors = interface;
  JFormatter = interface;
  JInflater = interface;
  JLinkedHashSet = interface;
  JMap_Entry = interface;
  JNavigableMap = interface;
  JNavigableSet = interface;
  JSortedSet = interface;
  JTimerTask = interface;
  JTimer = interface;
  JTreeMap = interface;
  JStatus = interface;

  JExecutorsClass = interface(JObjectClass)
    ['{83F13D1F-378D-4541-A348-A863638BF5DF}']
    {class} function callable(task: JRunnable; result: JObject): JCallable; cdecl; overload;
    {class} function callable(task: JRunnable): JCallable; cdecl; overload;
    // {class} function callable(action: JPrivilegedAction): JCallable; cdecl; overload;
    // {class} function callable(action: JPrivilegedExceptionAction): JCallable; cdecl; overload;
    {class} function defaultThreadFactory: JThreadFactory; cdecl;
    {class} function newCachedThreadPool: JExecutorService; cdecl; overload;
    {class} function newCachedThreadPool(threadFactory: JThreadFactory): JExecutorService; cdecl; overload;
    {class} function newFixedThreadPool(nThreads: Integer): JExecutorService; cdecl; overload;
    {class} function newFixedThreadPool(nThreads: Integer; threadFactory: JThreadFactory): JExecutorService; cdecl; overload;
    {class} function newScheduledThreadPool(corePoolSize: Integer): JScheduledExecutorService; cdecl; overload;
    {class} function newScheduledThreadPool(corePoolSize: Integer; threadFactory: JThreadFactory): JScheduledExecutorService; cdecl; overload;
    {class} function newSingleThreadExecutor: JExecutorService; cdecl; overload;
    {class} function newSingleThreadExecutor(threadFactory: JThreadFactory): JExecutorService; cdecl; overload;
    {class} function newSingleThreadScheduledExecutor: JScheduledExecutorService; cdecl; overload;
    {class} function newSingleThreadScheduledExecutor(threadFactory: JThreadFactory): JScheduledExecutorService; cdecl; overload;
    {class} function privilegedCallable(callable: JCallable): JCallable; cdecl;
    {class} function privilegedCallableUsingCurrentClassLoader(callable: JCallable): JCallable; cdecl;
    {class} function privilegedThreadFactory: JThreadFactory; cdecl;
    {class} function unconfigurableExecutorService(executor: JExecutorService): JExecutorService; cdecl;
    {class} function unconfigurableScheduledExecutorService(executor: JScheduledExecutorService): JScheduledExecutorService; cdecl;
  end;

  [JavaSignature('java/util/concurrent/Executors')]
  JExecutors = interface(JObject)
    ['{D30B9FC6-F15A-4D3C-B7A3-306A992AC24F}']
  end;
  TJExecutors = class(TJavaGenericImport<JExecutorsClass, JExecutors>) end;

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

  JInflaterClass = interface(JObjectClass)
    ['{50E9050B-5927-4EEA-816F-ADC57D3BA62E}']
    {class} function init: JInflater; cdecl; overload;
    {class} function init(boolean: Boolean): JInflater; cdecl; overload;
  end;

  [JavaSignature('java/util/zip/Inflater')]
  JInflater = interface(JObject)
    ['{FBE4754B-620C-4917-8421-0264BEA5EA09}']
    procedure &end; cdecl;
    function finished: Boolean; cdecl;
    function getAdler: Integer; cdecl;
    function getBytesRead: Int64; cdecl;
    function getBytesWritten: Int64; cdecl;
    function getRemaining: Integer; cdecl;
    function getTotalIn: Integer; cdecl;
    function getTotalOut: Integer; cdecl;
    function inflate(bytes: TJavaArray<Byte>): Integer; cdecl; overload;
    function inflate(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer): Integer; cdecl; overload;
    function inflate(bytebuffer: JByteBuffer): Integer; cdecl; overload;
    function needsDictionary: Boolean; cdecl;
    function needsInput: Boolean; cdecl;
    procedure reset; cdecl;
    procedure setDictionary(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer); cdecl; overload;
    procedure setDictionary(bytes: TJavaArray<Byte>); cdecl; overload;
    procedure setDictionary(bytebuffer: JByteBuffer); cdecl; overload;
    procedure setInput(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer); cdecl; overload;
    procedure setInput(bytes: TJavaArray<Byte>); cdecl; overload;
    procedure setInput(bytebuffer: JByteBuffer); cdecl; overload;
  end;
  TJInflater = class(TJavaGenericImport<JInflaterClass, JInflater>) end;

  JFormatterClass = interface(JObjectClass)
    ['{B3FC1195-51B4-4118-BDDD-7B478079C41D}']
    {class} function init(outputstream: JOutputStream): JFormatter; cdecl; overload;
    {class} function init(printstream: JPrintStream): JFormatter; cdecl; overload;
    {class} function init(file_1: JFile; charset: JCharset; locale: JLocale): JFormatter; cdecl; overload;
    {class} function init(outputstream: JOutputStream; charset: JCharset; locale: JLocale): JFormatter; cdecl; overload;
    {class} function init(outputstream: JOutputStream; string_1: JString; locale: JLocale): JFormatter; cdecl; overload;
    {class} function init(outputstream: JOutputStream; string_1: JString): JFormatter; cdecl; overload;
    {class} function init: JFormatter; cdecl; overload;
    {class} function init(string_1: JString): JFormatter; cdecl; overload;
    {class} function init(appendable: JAppendable): JFormatter; cdecl; overload;
    {class} function init(appendable: JAppendable; locale: JLocale): JFormatter; cdecl; overload;
    {class} function init(locale: JLocale): JFormatter; cdecl; overload;
    {class} function init(string_1: JString; string_2: JString): JFormatter; cdecl; overload;
    {class} function init(file_1: JFile; string_1: JString; locale: JLocale): JFormatter; cdecl; overload;
    {class} function init(file_1: JFile; string_1: JString): JFormatter; cdecl; overload;
    {class} function init(file_1: JFile): JFormatter; cdecl; overload;
    {class} function init(string_1: JString; string_2: JString; locale: JLocale): JFormatter; cdecl; overload;
    {class} function init(string_1: JString; charset: JCharset; locale: JLocale): JFormatter; cdecl; overload;
  end;

  [JavaSignature('java/util/Formatter')]
  JFormatter = interface(JObject)
    ['{AF9F5943-D81D-4AD7-8CF6-21B1ADE3A84D}']
    function &out: JAppendable; cdecl;
    procedure close; cdecl;
    procedure flush; cdecl;
    function format(locale: JLocale; string_1: JString; object_1: JObject): JFormatter; cdecl; overload;
    function format(string_1: JString; object_1: JObject): JFormatter; cdecl; overload;
    function ioException: JIOException; cdecl;
    function locale: JLocale; cdecl;
    function toString: JString; cdecl;
  end;
  TJFormatter = class(TJavaGenericImport<JFormatterClass, JFormatter>) end;

  JStatusClass = interface(JObjectClass)
    ['{76253226-A9EE-401F-9BD3-C4C855024C93}']
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function _GetRESULT_CANCELED: JStatus; cdecl;
    function _GetRESULT_DEAD_CLIENT: JStatus; cdecl;
    function _GetRESULT_INTERNAL_ERROR: JStatus; cdecl;
    function _GetRESULT_INTERRUPTED: JStatus; cdecl;
    function _GetRESULT_SUCCESS: JStatus; cdecl;
    function _GetRESULT_TIMEOUT: JStatus; cdecl;
    function init(statusCode: Integer): JStatus; cdecl; overload;
    function init(statusCode: Integer; statusMessage: JString): JStatus; cdecl; overload;
    function init(statusCode: Integer; statusMessage: JString; pendingIntent: JPendingIntent): JStatus; cdecl; overload;
    // function init(connectionResult: JConnectionResult; statusMessage: JString): JStatus; cdecl; overload;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
    property RESULT_CANCELED: JStatus read _GetRESULT_CANCELED;
    property RESULT_DEAD_CLIENT: JStatus read _GetRESULT_DEAD_CLIENT;
    property RESULT_INTERNAL_ERROR: JStatus read _GetRESULT_INTERNAL_ERROR;
    property RESULT_INTERRUPTED: JStatus read _GetRESULT_INTERRUPTED;
    property RESULT_SUCCESS: JStatus read _GetRESULT_SUCCESS;
    property RESULT_TIMEOUT: JStatus read _GetRESULT_TIMEOUT;
  end;

  [JavaSignature('com/google/android/gms/common/api/Status')]
  JStatus = interface(JObject)
    ['{ED216A5D-56BA-4362-96E2-FDDADEF0E4AD}']
    function equals(obj: JObject): boolean; cdecl;
    // function getConnectionResult: JConnectionResult; cdecl;
    function getResolution: JPendingIntent; cdecl;
    function getStatus: JStatus; cdecl;
    function getStatusCode: Integer; cdecl;
    function getStatusMessage: JString; cdecl;
    function hasResolution: boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isCanceled: boolean; cdecl;
    function isInterrupted: boolean; cdecl;
    function isSuccess: boolean; cdecl;
    function toString: JString; cdecl;
    procedure startResolutionForResult(activity: JActivity; requestCode: Integer); cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJStatus = class(TJavaGenericImport<JStatusClass, JStatus>) end;

implementation

end.
