unit DW.Androidapi.JNI.Concurrent;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Java.Security;

type
  JAtomicBoolean = interface;
  JAtomicReference = interface;
  JExecutors = interface;
  JListenableFuture = interface;
  JScheduledExecutorService = interface;

  JAtomicBooleanClass = interface(JObjectClass)
    ['{D089034A-914B-47EF-A17E-A37E208E4746}']
    {class} function init(initialValue: Boolean): JAtomicBoolean; cdecl; overload;
    {class} function init: JAtomicBoolean; cdecl; overload;
    {class} procedure lazySet(newValue: Boolean); cdecl;
    {class} procedure &set(newValue: Boolean); cdecl;
    {class} function toString: JString; cdecl;
  end;

  [JavaSignature('java/util/concurrent/atomic/AtomicBoolean')]
  JAtomicBoolean = interface(JObject)
    ['{F1488386-E092-4CD9-BB53-639BC702F4D6}']
    function compareAndSet(expect: Boolean; update: Boolean): Boolean; cdecl;
    function &get: Boolean; cdecl;
    function getAndSet(newValue: Boolean): Boolean; cdecl;
    function weakCompareAndSet(expect: Boolean; update: Boolean): Boolean; cdecl;
  end;
  TJAtomicBoolean = class(TJavaGenericImport<JAtomicBooleanClass, JAtomicBoolean>) end;

  JAtomicReferenceClass = interface(JObjectClass)
    ['{862DE5E8-9EAB-449B-B693-4D589DF84C39}']
    {class} function init(initialValue: JObject): JAtomicReference; cdecl; overload;
    {class} function init: JAtomicReference; cdecl; overload;
    {class} function getAndSet(newValue: JObject): JObject; cdecl;
    {class} procedure lazySet(newValue: JObject); cdecl;
    {class} procedure &set(newValue: JObject); cdecl;
  end;

  [JavaSignature('java/util/concurrent/atomic/AtomicReference')]
  JAtomicReference = interface(JObject)
    ['{C313F0A2-2B01-4F0F-B53D-E60678D37A45}']
    function compareAndSet(expect: JObject; update: JObject): Boolean; cdecl;
    function &get: JObject; cdecl;
    function toString: JString; cdecl;
    function weakCompareAndSet(expect: JObject; update: JObject): Boolean; cdecl;
  end;
  TJAtomicReference = class(TJavaGenericImport<JAtomicReferenceClass, JAtomicReference>) end;

  JScheduledExecutorServiceClass = interface(IJavaClass)
    ['{A21DBE61-CA63-466F-9DF4-F48CD2F7AE5C}']
  end;

  [JavaSignature('java/util/concurrent/ScheduledExecutorService')]
  JScheduledExecutorService = interface(IJavaInstance)
    ['{B2021DAB-0B95-4767-86A8-CDE7C62D5B7F}']
    function schedule(callable: JCallable; delay: Int64; &unit: JTimeUnit): JScheduledFuture; cdecl; overload;
    function schedule(runnable: JRunnable; delay: Int64; &unit: JTimeUnit): JScheduledFuture; cdecl; overload;
    function scheduleAtFixedRate(runnable: JRunnable; initialDelay: Int64; period: Int64; &unit: JTimeUnit): JScheduledFuture; cdecl;
    function scheduleWithFixedDelay(runnable: JRunnable; initialDelay: Int64; period: Int64; &unit: JTimeUnit): JScheduledFuture; cdecl;
  end;
  TJScheduledExecutorService = class(TJavaGenericImport<JScheduledExecutorServiceClass, JScheduledExecutorService>)
  end;

  JExecutorsClass = interface(JObjectClass)
    ['{83F13D1F-378D-4541-A348-A863638BF5DF}']
    {class} function callable(task: JRunnable; result: JObject): JCallable; cdecl; overload;
    {class} function callable(task: JRunnable): JCallable; cdecl; overload;
    {class} function callable(action: JPrivilegedAction): JCallable; cdecl; overload;
    {class} function callable(action: JPrivilegedExceptionAction): JCallable; cdecl; overload;
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

  JListenableFutureClass = interface(JFutureClass)
    ['{DF69540B-BF18-48D3-B5EC-474094810D24}']
  end;

  [JavaSignature('com/google/common/util/concurrent/ListenableFuture')]
  JListenableFuture = interface(JFuture)
    ['{BB39381A-AD2C-496F-BE55-BFA377BEBD88}']
    procedure addListener(runnable: JRunnable; executor: JExecutor); cdecl;
  end;
  TJListenableFuture = class(TJavaGenericImport<JListenableFutureClass, JListenableFuture>) end;

implementation

end.
