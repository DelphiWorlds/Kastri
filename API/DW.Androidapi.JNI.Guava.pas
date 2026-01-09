unit DW.Androidapi.JNI.Guava;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Java.Security, Androidapi.JNI.Java.Net
  {$IF CompilerVersion < 36}
  , DW.Androidapi.JNI.Util
  {$ENDIF}
  ;

type
  JAbstractMultimap = interface;
  JAsyncFunction = interface;
  JBaseImmutableMultimap = interface;
  JImmutableCollection = interface;
  JImmutableCollection_ArrayBasedBuilder = interface;
  JImmutableCollection_Builder = interface;
  JImmutableList = interface;
  JImmutableList_Builder = interface;
  JImmutableMap = interface;
  JImmutableMap_Builder = interface;
  JImmutableMultimap = interface;
  JImmutableMultimap_Builder = interface;
  JImmutableMultiset = interface;
  JImmutableMultiset_Builder = interface;
  JImmutableMultisetGwtSerializationDependencies = interface;
  JImmutableSet = interface;
  JImmutableSet_Builder = interface;
  JImmutableSetMultimap = interface;
  JImmutableSetMultimap_Builder = interface;
  JListenableFuture = interface;
  JMultimap = interface;
  JMultiset = interface;
  JUnmodifiableIterator = interface;
  JUnmodifiableListIterator = interface;

  JAbstractMultimapClass = interface(JObjectClass)
    ['{8FBC2664-F1CF-4FED-A890-5B213690821C}']
  end;

  [JavaSignature('com/google/common/collect/AbstractMultimap')]
  JAbstractMultimap = interface(JObject)
    ['{C35052BF-E53D-4320-92FC-80373D6CC608}']
    function asMap: JMap; cdecl;
    function containsEntry(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function containsValue(object_1: JObject): Boolean; cdecl;
    function entries: JCollection; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function keySet: JSet; cdecl;
    function keys: JMultiset; cdecl;
    function put(k: JObject; v: JObject): Boolean; cdecl;
    function putAll(k: JObject; iterable: JIterable): Boolean; cdecl; overload;
    function putAll(multimap: JMultimap): Boolean; cdecl; overload;
    function remove(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function replaceValues(k: JObject; iterable: JIterable): JCollection; cdecl;
    function toString: JString; cdecl;
    function values: JCollection; cdecl;
  end;
  TJAbstractMultimap = class(TJavaGenericImport<JAbstractMultimapClass, JAbstractMultimap>) end;

  JBaseImmutableMultimapClass = interface(JAbstractMultimapClass)
    ['{1C6B99FA-3F9F-4009-8F8A-8C5D7B3DF86A}']
  end;

  [JavaSignature('com/google/common/collect/BaseImmutableMultimap')]
  JBaseImmutableMultimap = interface(JAbstractMultimap)
    ['{8044E7BE-EA0F-41EE-A9AB-CCA262333C66}']
  end;
  TJBaseImmutableMultimap = class(TJavaGenericImport<JBaseImmutableMultimapClass, JBaseImmutableMultimap>) end;


  JListenableFutureClass = interface(JFutureClass)
    ['{18D6CB75-F5BA-429B-94F3-3277E742BC7E}']
  end;

  [JavaSignature('com/google/common/util/concurrent/ListenableFuture')]
  JListenableFuture = interface(JFuture)
    ['{EBBA0A7F-D1CF-443D-8217-0BD0037846C2}']
    procedure addListener(runnable: JRunnable; executor: JExecutor); cdecl;
  end;
  TJListenableFuture = class(TJavaGenericImport<JListenableFutureClass, JListenableFuture>) end;

  JAsyncFunctionClass = interface(IJavaClass)
    ['{7CCEB1EA-368B-4E8F-A2C0-7B9E02518734}']
  end;

  [JavaSignature('com/google/common/util/concurrent/AsyncFunction')]
  JAsyncFunction = interface(IJavaInstance)
    ['{5CAA8E9E-A144-4E22-BE32-B10C82F58F87}']
    function apply(i: JObject): JListenableFuture; cdecl;
  end;
  TJAsyncFunction = class(TJavaGenericImport<JAsyncFunctionClass, JAsyncFunction>) end;

  JUnmodifiableIteratorClass = interface(JObjectClass)
    ['{26EFF821-3B78-4080-B61E-3961BC331CDB}']
  end;

  [JavaSignature('com/google/common/collect/UnmodifiableIterator')]
  JUnmodifiableIterator = interface(JObject)
    ['{8B157F18-9627-43E3-A8E4-0F9FCC52480A}']
    procedure remove; cdecl;
  end;
  TJUnmodifiableIterator = class(TJavaGenericImport<JUnmodifiableIteratorClass, JUnmodifiableIterator>) end;

  JMultisetClass = interface(JCollectionClass)
    ['{E3CD0D2B-4FF4-4667-846C-C3C4B1E203EC}']
  end;

  [JavaSignature('com/google/common/collect/Multiset')]
  JMultiset = interface(JCollection)
    ['{B9D187E4-5961-47E6-859D-FFCB976F5A1F}']
    function add(e: JObject): Boolean; cdecl; overload;
    function add(e: JObject; int: Integer): Integer; cdecl; overload;
    function contains(object_1: JObject): Boolean; cdecl;
    function containsAll(collection: JCollection): Boolean; cdecl;
    function count(object_1: JObject): Integer; cdecl;
    function elementSet: JSet; cdecl;
    function entrySet: JSet; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function iterator: JIterator; cdecl;
    function remove(object_1: JObject; int: Integer): Integer; cdecl; overload;
    function remove(object_1: JObject): Boolean; cdecl; overload;
    function removeAll(collection: JCollection): Boolean; cdecl;
    function retainAll(collection: JCollection): Boolean; cdecl;
    function setCount(e: JObject; int: Integer; int_1: Integer): Boolean; cdecl; overload;
    function setCount(e: JObject; int: Integer): Integer; cdecl; overload;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJMultiset = class(TJavaGenericImport<JMultisetClass, JMultiset>) end;

  JMultimapClass = interface(IJavaClass)
    ['{DD722B9F-6713-4462-A3DE-72945ECC9DEE}']
  end;

  [JavaSignature('com/google/common/collect/Multimap')]
  JMultimap = interface(IJavaInstance)
    ['{3200D88C-BFF6-4A97-BBD5-AE084268B9D3}']
    function asMap: JMap; cdecl;
    procedure clear; cdecl;
    function containsEntry(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function containsKey(object_1: JObject): Boolean; cdecl;
    function containsValue(object_1: JObject): Boolean; cdecl;
    function entries: JCollection; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(k: JObject): JCollection; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function keySet: JSet; cdecl;
    function keys: JMultiset; cdecl;
    function put(k: JObject; v: JObject): Boolean; cdecl;
    function putAll(k: JObject; iterable: JIterable): Boolean; cdecl; overload;
    function putAll(multimap: JMultimap): Boolean; cdecl; overload;
    function remove(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function removeAll(object_1: JObject): JCollection; cdecl;
    function replaceValues(k: JObject; iterable: JIterable): JCollection; cdecl;
    function size: Integer; cdecl;
    function values: JCollection; cdecl;
  end;
  TJMultimap = class(TJavaGenericImport<JMultimapClass, JMultimap>) end;

  JImmutableCollectionClass = interface(JAbstractCollectionClass)
    ['{D56B34B5-3887-40E8-A24A-928EB9C676AD}']
  end;

  [JavaSignature('com/google/common/collect/ImmutableCollection')]
  JImmutableCollection = interface(JAbstractCollection)
    ['{D3CF4774-D090-4C79-9B96-C6871D6A8095}']
    function add(e: JObject): Boolean; cdecl;
    function addAll(collection: JCollection): Boolean; cdecl;
    function asList: JImmutableList; cdecl;
    procedure clear; cdecl;
    function contains(object_1: JObject): Boolean; cdecl;
    function iterator: JUnmodifiableIterator; cdecl; // overload;
    // function iterator: JIterator; cdecl; overload;
    function remove(object_1: JObject): Boolean; cdecl;
    function removeAll(collection: JCollection): Boolean; cdecl;
    function retainAll(collection: JCollection): Boolean; cdecl;
    function toArray: TJavaObjectArray<JObject>; cdecl; overload;
    function toArray(ts: TJavaObjectArray<JObject>): TJavaObjectArray<JObject>; cdecl; overload;
  end;
  TJImmutableCollection = class(TJavaGenericImport<JImmutableCollectionClass, JImmutableCollection>) end;

  JImmutableCollection_BuilderClass = interface(JObjectClass)
    ['{02FCE7C3-164E-4608-ABA4-D35FEB2CF529}']
  end;

  [JavaSignature('com/google/common/collect/ImmutableCollection$Builder')]
  JImmutableCollection_Builder = interface(JObject)
    ['{527C018D-8827-4819-8874-0AB3315C21A4}']
    function add(e: JObject): JImmutableCollection_Builder; cdecl; overload;
    // function add(e: JObject): JImmutableCollection_Builder; cdecl; overload;
    function addAll(iterator: JIterator): JImmutableCollection_Builder; cdecl; overload;
    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl; overload;
    function build: JImmutableCollection; cdecl;
  end;
  TJImmutableCollection_Builder = class(TJavaGenericImport<JImmutableCollection_BuilderClass, JImmutableCollection_Builder>) end;

  JImmutableMap_BuilderClass = interface(JObjectClass)
    ['{600D47F5-FFB2-494C-AF59-0CFF73DC51F8}']
    {class} function init: JImmutableMap_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMap$Builder')]
  JImmutableMap_Builder = interface(JObject)
    ['{4A047DCC-1753-49EB-838E-F30617E0C5B9}']
    function build: JImmutableMap; cdecl;
    function buildKeepingLast: JImmutableMap; cdecl;
    function buildOrThrow: JImmutableMap; cdecl;
    function orderEntriesByValue(comparator: JComparator): JImmutableMap_Builder; cdecl;
    function put(entry: JMap_Entry): JImmutableMap_Builder; cdecl; overload;
    function put(k: JObject; v: JObject): JImmutableMap_Builder; cdecl; overload;
    function putAll(iterable: JIterable): JImmutableMap_Builder; cdecl; overload;
    function putAll(map: JMap): JImmutableMap_Builder; cdecl; overload;
  end;
  TJImmutableMap_Builder = class(TJavaGenericImport<JImmutableMap_BuilderClass, JImmutableMap_Builder>) end;

  JImmutableMapClass = interface(JObjectClass)
    ['{77DEF41A-0E2D-4B5A-8C5F-ADD71C01FD72}']
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject;
      v_4: JObject; k_5: JObject; v_5: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject;
      v_4: JObject; k_5: JObject; v_5: JObject; k_6: JObject; v_6: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject;
      v_4: JObject; k_5: JObject; v_5: JObject; k_6: JObject; v_6: JObject; k_7: JObject; v_7: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject;
      v_4: JObject; k_5: JObject; v_5: JObject; k_6: JObject; v_6: JObject; k_7: JObject; v_7: JObject; k_8: JObject;
      v_8: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject;
      v_4: JObject; k_5: JObject; v_5: JObject; k_6: JObject; v_6: JObject; k_7: JObject; v_7: JObject; k_8: JObject; v_8: JObject; k_9: JObject;
      v_9: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject;
      v_4: JObject): JImmutableMap; cdecl; overload;
    {class} function &of: JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject): JImmutableMap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject;
      v_3: JObject): JImmutableMap; cdecl; overload;
    {class} function builder: JImmutableMap_Builder; cdecl;
    {class} function builderWithExpectedSize(int: Integer): JImmutableMap_Builder; cdecl;
    {class} function copyOf(map: JMap): JImmutableMap; cdecl; overload;
    {class} function copyOf(iterable: JIterable): JImmutableMap; cdecl; overload;
    {class} function ofEntries(entry: JMap_Entry): JImmutableMap; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMap')]
  JImmutableMap = interface(JObject)
    ['{7A1EBA50-63C2-4FED-9D88-06FA8E42CD33}']
    function asMultimap: JImmutableSetMultimap; cdecl;
    procedure clear; cdecl;
    function containsKey(object_1: JObject): Boolean; cdecl;
    function containsValue(object_1: JObject): Boolean; cdecl;
    function entrySet: JImmutableSet; cdecl; // overload;
    // function entrySet: JSet; cdecl; overload;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(object_1: JObject): JObject; cdecl;
    function getOrDefault(object_1: JObject; v: JObject): JObject; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function keySet: JSet; cdecl; // overload;
    // function keySet: JImmutableSet; cdecl; overload;
    function put(k: JObject; v: JObject): JObject; cdecl;
    procedure putAll(map: JMap); cdecl;
    function remove(object_1: JObject): JObject; cdecl;
    function toString: JString; cdecl;
    function values: JCollection; cdecl; // overload;
    // function values: JImmutableCollection; cdecl; overload;
  end;
  TJImmutableMap = class(TJavaGenericImport<JImmutableMapClass, JImmutableMap>) end;

  JImmutableSetClass = interface(JImmutableCollectionClass)
    ['{2FCA5D2E-FD4D-4521-86AD-3789E39AA403}']
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject;
      e_6: JObject): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject): JImmutableSet; cdecl; overload;
    {class} function &of: JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject): JImmutableSet; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject): JImmutableSet; cdecl; overload;
    {class} function builder: JImmutableSet_Builder; cdecl;
    {class} function builderWithExpectedSize(int: Integer): JImmutableSet_Builder; cdecl;
    {class} function copyOf(es: TJavaObjectArray<JObject>): JImmutableSet; cdecl; overload;
    {class} function copyOf(iterator: JIterator): JImmutableSet; cdecl; overload;
    {class} function copyOf(iterable: JIterable): JImmutableSet; cdecl; overload;
    {class} function copyOf(collection: JCollection): JImmutableSet; cdecl; overload;
  end;

  JImmutableMultisetGwtSerializationDependenciesClass = interface(JImmutableCollectionClass)
    ['{C1BDDF8D-BBF7-48FD-8021-EFE02EAA348F}']
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultisetGwtSerializationDependencies')]
  JImmutableMultisetGwtSerializationDependencies = interface(JImmutableCollection)
    ['{188DE14F-E090-4DC8-83B9-5A450D0095EE}']
  end;
  TJImmutableMultisetGwtSerializationDependencies = class(TJavaGenericImport<JImmutableMultisetGwtSerializationDependenciesClass,
    JImmutableMultisetGwtSerializationDependencies>) end;

  JImmutableCollection_ArrayBasedBuilderClass = interface(JImmutableCollection_BuilderClass)
    ['{3C33498E-E352-40F8-9770-D4D3E44AE7C1}']
  end;

  [JavaSignature('com/google/common/collect/ImmutableCollection$ArrayBasedBuilder')]
  JImmutableCollection_ArrayBasedBuilder = interface(JImmutableCollection_Builder)
    ['{87420E42-1A0C-4245-84C8-00A31524ECDF}']
    function add(object_1: JObject): JImmutableCollection_Builder; cdecl; overload;
    // function add(e: JObject): JImmutableCollection_Builder; cdecl; overload;
    // function add(e: JObject): JImmutableCollection_ArrayBasedBuilder; cdecl; overload;
    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl;
  end;
  TJImmutableCollection_ArrayBasedBuilder = class(TJavaGenericImport<JImmutableCollection_ArrayBasedBuilderClass, JImmutableCollection_ArrayBasedBuilder>) end;

  [JavaSignature('com/google/common/collect/ImmutableSet')]
  JImmutableSet = interface(JImmutableCollection)
    ['{0271B519-10E7-4281-9330-932176643FF1}']
    function asList: JImmutableList; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function iterator: JUnmodifiableIterator; cdecl; // overload;
    // function iterator: JIterator; cdecl; overload;
  end;
  TJImmutableSet = class(TJavaGenericImport<JImmutableSetClass, JImmutableSet>) end;

  JImmutableSet_BuilderClass = interface(JImmutableCollection_ArrayBasedBuilderClass)
    ['{1B516480-9AB2-4603-882A-E7DD4242CCE7}']
    {class} function init: JImmutableSet_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableSet$Builder')]
  JImmutableSet_Builder = interface(JImmutableCollection_ArrayBasedBuilder)
    ['{98C196AC-55DB-47DF-89C8-3E8EACF3E5D4}']
    function add(objects: TJavaObjectArray<JObject>): JImmutableCollection_Builder; cdecl; overload;
    function add(object_1: JObject): JImmutableCollection_ArrayBasedBuilder; cdecl; overload;
    // function add(e: JObject): JImmutableSet_Builder; cdecl; overload;
    // function add(object_1: JObject): JImmutableCollection_Builder; cdecl; overload;
    // function add(e: JObject): JImmutableSet_Builder; cdecl; overload;
    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl; overload;
    // function addAll(iterable: JIterable): JImmutableSet_Builder; cdecl; overload;
    function addAll(iterator: JIterator): JImmutableSet_Builder; cdecl; overload;
    // function addAll(iterator: JIterator): JImmutableCollection_Builder; cdecl; overload;
    function build: JImmutableCollection; cdecl; // overload;
    // function build: JImmutableSet; cdecl; overload;
  end;
  TJImmutableSet_Builder = class(TJavaGenericImport<JImmutableSet_BuilderClass, JImmutableSet_Builder>) end;

  JImmutableMultisetClass = interface(JImmutableMultisetGwtSerializationDependenciesClass)
    ['{B85672AE-2289-4D80-AB13-27B629EE2DA4}']
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of: JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject): JImmutableMultiset; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject): JImmutableMultiset; cdecl; overload;
    {class} function builder: JImmutableMultiset_Builder; cdecl;
    {class} function copyOf(es: TJavaObjectArray<JObject>): JImmutableMultiset; cdecl; overload;
    {class} function copyOf(iterable: JIterable): JImmutableMultiset; cdecl; overload;
    {class} function copyOf(iterator: JIterator): JImmutableMultiset; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultiset')]
  JImmutableMultiset = interface(JImmutableMultisetGwtSerializationDependencies)
    ['{9EEA7EEA-8C59-40FC-836A-8C3F97D2CB37}']
    function add(e: JObject; int: Integer): Integer; cdecl;
    function asList: JImmutableList; cdecl;
    function contains(object_1: JObject): Boolean; cdecl;
    function elementSet: JSet; cdecl; overload;
    // function elementSet: JImmutableSet; cdecl; overload;
    function entrySet: JImmutableSet; cdecl; overload;
    // function entrySet: JSet; cdecl; overload;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function iterator: JUnmodifiableIterator; cdecl; overload;
    // function iterator: JIterator; cdecl; overload;
    function remove(object_1: JObject; int: Integer): Integer; cdecl;
    function setCount(e: JObject; int: Integer; int_1: Integer): Boolean; cdecl; overload;
    function setCount(e: JObject; int: Integer): Integer; cdecl; overload;
    function toString: JString; cdecl;
  end;
  TJImmutableMultiset = class(TJavaGenericImport<JImmutableMultisetClass, JImmutableMultiset>) end;

  JImmutableMultiset_BuilderClass = interface(JImmutableCollection_BuilderClass)
    ['{34219B39-98E5-4151-B617-85CEEA067DEE}']
    {class} function init: JImmutableMultiset_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultiset$Builder')]
  JImmutableMultiset_Builder = interface(JImmutableCollection_Builder)
    ['{185ECAA9-6F0E-4310-99CE-93A1FFE9FA92}']
    function add(objects: TJavaObjectArray<JObject>): JImmutableCollection_Builder; cdecl; overload;
    function add(e: JObject): JImmutableMultiset_Builder; cdecl; overload;
    // function add(object_1: JObject): JImmutableCollection_Builder; cdecl; overload;
    // function add(e: JObject): JImmutableMultiset_Builder; cdecl; overload;
    function addAll(iterator: JIterator): JImmutableCollection_Builder; cdecl; overload;
    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl; overload;
    // function addAll(iterator: JIterator): JImmutableMultiset_Builder; cdecl; overload;
    // function addAll(iterable: JIterable): JImmutableMultiset_Builder; cdecl; overload;
    function addCopies(e: JObject; int: Integer): JImmutableMultiset_Builder; cdecl;
    function build: JImmutableMultiset; cdecl; overload;
    // function build: JImmutableCollection; cdecl; overload;
    function setCount(e: JObject; int: Integer): JImmutableMultiset_Builder; cdecl;
  end;
  TJImmutableMultiset_Builder = class(TJavaGenericImport<JImmutableMultiset_BuilderClass, JImmutableMultiset_Builder>) end;

  JImmutableListClass = interface(JImmutableCollectionClass)
    ['{B2336D9C-09F1-4A01-AECE-F5B3F538B400}']
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject;
      e_8: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject;
      e_7: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject;
      e_6: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject;
      e_8: JObject; e_9: JObject; e_10: JObject; e_11: JObject; e_12: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject; e_8: JObject;
      e_9: JObject; e_10: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject; e_6: JObject; e_7: JObject; e_8: JObject;
      e_9: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject; e_5: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject): JImmutableList; cdecl; overload;
    {class} function &of: JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject; e_4: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject; e_3: JObject): JImmutableList; cdecl; overload;
    {class} function &of(e: JObject; e_1: JObject; e_2: JObject): JImmutableList; cdecl; overload;
    {class} function builder: JImmutableList_Builder; cdecl;
    {class} function builderWithExpectedSize(int: Integer): JImmutableList_Builder; cdecl;
    {class} function copyOf(es: TJavaObjectArray<JObject>): JImmutableList; cdecl; overload;
    {class} function copyOf(iterator: JIterator): JImmutableList; cdecl; overload;
    {class} function copyOf(collection: JCollection): JImmutableList; cdecl; overload;
    {class} function copyOf(iterable: JIterable): JImmutableList; cdecl; overload;
    {class} function sortedCopyOf(iterable: JIterable): JImmutableList; cdecl; overload;
    {class} function sortedCopyOf(comparator: JComparator; iterable: JIterable): JImmutableList; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableList')]
  JImmutableList = interface(JImmutableCollection)
    ['{E40DD515-47C9-48E4-AA0D-36EF1264EA48}']
    function &set(int: Integer; e: JObject): JObject; cdecl;
    procedure add(int: Integer; e: JObject); cdecl;
    function addAll(int: Integer; collection: JCollection): Boolean; cdecl;
    function asList: JImmutableList; cdecl;
    function contains(object_1: JObject): Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function indexOf(object_1: JObject): Integer; cdecl;
    function iterator: JIterator; cdecl; overload;
    // function iterator: JUnmodifiableIterator; cdecl; overload;
    function lastIndexOf(object_1: JObject): Integer; cdecl;
    function listIterator: JUnmodifiableListIterator; cdecl; overload;
    // function listIterator: JListIterator; cdecl; overload;
    function listIterator(int: Integer): JListIterator; cdecl; overload;
    // function listIterator(int: Integer): JUnmodifiableListIterator; cdecl; overload;
    function remove(int: Integer): JObject; cdecl;
    function reverse: JImmutableList; cdecl;
    function subList(int: Integer; int_1: Integer): JList; cdecl; overload;
    // function subList(int: Integer; int_1: Integer): JImmutableList; cdecl; overload;
  end;
  TJImmutableList = class(TJavaGenericImport<JImmutableListClass, JImmutableList>) end;

  JImmutableList_BuilderClass = interface(JImmutableCollection_ArrayBasedBuilderClass)
    ['{BA43C8E3-C3E0-42F8-8721-3C5E60D54140}']
    {class} function init: JImmutableList_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableList$Builder')]
  JImmutableList_Builder = interface(JImmutableCollection_ArrayBasedBuilder)
    ['{54D161BC-CEEE-4A7C-87AB-FD481FD6D36F}']
    function add(objects: TJavaObjectArray<JObject>): JImmutableCollection_Builder; cdecl; overload;
    function add(object_1: JObject): JImmutableCollection_ArrayBasedBuilder; cdecl; overload;
    // function add(e: JObject): JImmutableList_Builder; cdecl; overload;
    // function add(object_1: JObject): JImmutableCollection_Builder; cdecl; overload;
    // function add(e: JObject): JImmutableList_Builder; cdecl; overload;
    function addAll(iterable: JIterable): JImmutableCollection_Builder; cdecl; overload;
    // function addAll(iterable: JIterable): JImmutableList_Builder; cdecl; overload;
    function addAll(iterator: JIterator): JImmutableList_Builder; cdecl; overload;
    // function addAll(iterator: JIterator): JImmutableCollection_Builder; cdecl; overload;
    function build: JImmutableCollection; cdecl; overload;
    // function build: JImmutableList; cdecl; overload;
  end;
  TJImmutableList_Builder = class(TJavaGenericImport<JImmutableList_BuilderClass, JImmutableList_Builder>) end;

  JImmutableMultimapClass = interface(JBaseImmutableMultimapClass)
    ['{F5A1AA00-945A-4C92-B506-E2171E19A80D}']
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject): JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject): JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject): JImmutableMultimap; cdecl; overload;
    {class} function &of: JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject): JImmutableMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject): JImmutableMultimap; cdecl; overload;
    {class} function builder: JImmutableMultimap_Builder; cdecl;
    {class} function copyOf(iterable: JIterable): JImmutableMultimap; cdecl; overload;
    {class} function copyOf(multimap: JMultimap): JImmutableMultimap; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultimap')]
  JImmutableMultimap = interface(JBaseImmutableMultimap)
    ['{E205A40A-3875-4520-AB3A-D5EC2CAC67BE}']
    function asMap: JMap; cdecl; overload;
    // function asMap: JImmutableMap; cdecl; overload;
    procedure clear; cdecl;
    function containsEntry(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function containsKey(object_1: JObject): Boolean; cdecl;
    function containsValue(object_1: JObject): Boolean; cdecl;
    function entries: JCollection; cdecl; overload;
    // function entries: JImmutableCollection; cdecl; overload;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(object_1: JObject): JCollection; cdecl; overload;
    // function get(k: JObject): JImmutableCollection; cdecl; overload;
    function hashCode: Integer; cdecl;
    function inverse: JImmutableMultimap; cdecl;
    function isEmpty: Boolean; cdecl;
    function keySet: JSet; cdecl; overload;
    // function keySet: JImmutableSet; cdecl; overload;
    function keys: JMultiset; cdecl; overload;
    // function keys: JImmutableMultiset; cdecl; overload;
    function put(k: JObject; v: JObject): Boolean; cdecl;
    function putAll(multimap: JMultimap): Boolean; cdecl; overload;
    function putAll(k: JObject; iterable: JIterable): Boolean; cdecl; overload;
    function remove(object_1: JObject; object_2: JObject): Boolean; cdecl;
    function removeAll(object_1: JObject): JCollection; cdecl; overload;
    // function removeAll(object_1: JObject): JImmutableCollection; cdecl; overload;
    function replaceValues(object_1: JObject; iterable: JIterable): JCollection; cdecl; overload;
    // function replaceValues(k: JObject; iterable: JIterable): JImmutableCollection; cdecl; overload;
    function size: Integer; cdecl;
    function toString: JString; cdecl;
    function values: JCollection; cdecl; overload;
    // function values: JImmutableCollection; cdecl; overload;
  end;
  TJImmutableMultimap = class(TJavaGenericImport<JImmutableMultimapClass, JImmutableMultimap>) end;

  JImmutableMultimap_BuilderClass = interface(JObjectClass)
    ['{797F856F-10B1-419C-85B1-74CB33263F61}']
    {class} function init: JImmutableMultimap_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableMultimap$Builder')]
  JImmutableMultimap_Builder = interface(JObject)
    ['{56C08388-6184-410C-AC04-19FE5E2B3E70}']
    function build: JImmutableMultimap; cdecl;
    function orderKeysBy(comparator: JComparator): JImmutableMultimap_Builder; cdecl;
    function orderValuesBy(comparator: JComparator): JImmutableMultimap_Builder; cdecl;
    function put(entry: JMap_Entry): JImmutableMultimap_Builder; cdecl; overload;
    function put(k: JObject; v: JObject): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(multimap: JMultimap): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(iterable: JIterable): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(k: JObject; iterable: JIterable): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(k: JObject; v: JObject): JImmutableMultimap_Builder; cdecl; overload;
  end;
  TJImmutableMultimap_Builder = class(TJavaGenericImport<JImmutableMultimap_BuilderClass, JImmutableMultimap_Builder>) end;

  JImmutableSetMultimapClass = interface(JImmutableMultimapClass)
    ['{102C5EB2-A320-4FC0-ACB3-60348FB21425}']
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject; k_2: JObject; v_2: JObject; k_3: JObject; v_3: JObject; k_4: JObject; v_4: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function &of: JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function &of(k: JObject; v: JObject; k_1: JObject; v_1: JObject): JImmutableSetMultimap; cdecl; overload;
    {class} function builder: JImmutableSetMultimap_Builder; cdecl;
    {class} function copyOf(iterable: JIterable): JImmutableSetMultimap; cdecl; overload;
    {class} function copyOf(multimap: JMultimap): JImmutableSetMultimap; cdecl; overload;
  end;

  [JavaSignature('com/google/common/collect/ImmutableSetMultimap')]
  JImmutableSetMultimap = interface(JImmutableMultimap)
    ['{043D9F0F-BA2C-4D53-BC07-48AD0FA6B54A}']
    function entries: JImmutableCollection; cdecl; overload;
    // function entries: JImmutableSet; cdecl; overload;
    // function entries: JCollection; cdecl; overload;
    // function entries: JSet; cdecl; overload;
    function get(object_1: JObject): JImmutableCollection; cdecl; overload;
    // function get(object_1: JObject): JCollection; cdecl; overload;
    // function get(object_1: JObject): JSet; cdecl; overload;
    // function get(k: JObject): JImmutableSet; cdecl; overload;
    function inverse: JImmutableSetMultimap; cdecl; overload;
    // function inverse: JImmutableMultimap; cdecl; overload;
    function removeAll(object_1: JObject): JCollection; cdecl; overload;
    // function removeAll(object_1: JObject): JImmutableSet; cdecl; overload;
    // function removeAll(object_1: JObject): JSet; cdecl; overload;
    // function removeAll(object_1: JObject): JImmutableCollection; cdecl; overload;
    function replaceValues(k: JObject; iterable: JIterable): JImmutableSet; cdecl; overload;
    // function replaceValues(object_1: JObject; iterable: JIterable): JSet; cdecl; overload;
    // function replaceValues(object_1: JObject; iterable: JIterable): JImmutableCollection; cdecl; overload;
    // function replaceValues(object_1: JObject; iterable: JIterable): JCollection; cdecl; overload;
  end;
  TJImmutableSetMultimap = class(TJavaGenericImport<JImmutableSetMultimapClass, JImmutableSetMultimap>) end;

  JImmutableSetMultimap_BuilderClass = interface(JImmutableMultimap_BuilderClass)
    ['{E7660B3C-30CA-4279-8F65-A87E2E4032FD}']
    {class} function init: JImmutableSetMultimap_Builder; cdecl;
  end;

  [JavaSignature('com/google/common/collect/ImmutableSetMultimap$Builder')]
  JImmutableSetMultimap_Builder = interface(JImmutableMultimap_Builder)
    ['{48823709-890E-47DA-8977-A0AD1009D4B3}']
    function build: JImmutableMultimap; cdecl; overload;
    // function build: JImmutableSetMultimap; cdecl; overload;
    function orderKeysBy(comparator: JComparator): JImmutableMultimap_Builder; cdecl; overload;
    // function orderKeysBy(comparator: JComparator): JImmutableSetMultimap_Builder; cdecl; overload;
    function orderValuesBy(comparator: JComparator): JImmutableSetMultimap_Builder; cdecl; overload;
    // function orderValuesBy(comparator: JComparator): JImmutableMultimap_Builder; cdecl; overload;
    function put(entry: JMap_Entry): JImmutableSetMultimap_Builder; cdecl; overload;
    function put(object_1: JObject; object_2: JObject): JImmutableMultimap_Builder; cdecl; overload;
    // function put(entry: JMap_Entry): JImmutableMultimap_Builder; cdecl; overload;
    // function put(k: JObject; v: JObject): JImmutableSetMultimap_Builder; cdecl; overload;
    function putAll(iterable: JIterable): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(object_1: JObject; iterable: JIterable): JImmutableMultimap_Builder; cdecl; overload;
    // function putAll(k: JObject; iterable: JIterable): JImmutableSetMultimap_Builder; cdecl; overload;
    // function putAll(iterable: JIterable): JImmutableSetMultimap_Builder; cdecl; overload;
    function putAll(multimap: JMultimap): JImmutableSetMultimap_Builder; cdecl; overload;
    function putAll(k: JObject; v: JObject): JImmutableSetMultimap_Builder; cdecl; overload;
    // function putAll(multimap: JMultimap): JImmutableMultimap_Builder; cdecl; overload;
    function putAll(object_1: JObject; objects: TJavaObjectArray<JObject>): JImmutableMultimap_Builder; cdecl; overload;
  end;
  TJImmutableSetMultimap_Builder = class(TJavaGenericImport<JImmutableSetMultimap_BuilderClass, JImmutableSetMultimap_Builder>) end;

  JUnmodifiableListIteratorClass = interface(JUnmodifiableIteratorClass)
    ['{98490CDD-FFE3-4DF3-9531-1BDA9C267EDA}']
  end;

  [JavaSignature('com/google/common/collect/UnmodifiableListIterator')]
  JUnmodifiableListIterator = interface(JUnmodifiableIterator)
    ['{0CA5924E-D7E1-4F2E-9EE1-4478B409D62C}']
    procedure &set(e: JObject); cdecl;
    procedure add(e: JObject); cdecl;
  end;
  TJUnmodifiableListIterator = class(TJavaGenericImport<JUnmodifiableListIteratorClass, JUnmodifiableListIterator>) end;

implementation

end.
