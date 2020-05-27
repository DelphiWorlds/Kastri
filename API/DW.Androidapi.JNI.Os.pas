unit DW.Androidapi.JNI.Os;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText;

type
  JActivityManager_RunningServiceInfo = interface;
  JAsyncTask = interface;
  JAsyncTask_Status = interface;
  JBatteryManager = interface;
  JDebug = interface;
  JEnvironment = interface;
  JHandlerThread = interface;
  JStatFs = interface;
  JSystem = interface;

  JActivityManager_RunningServiceInfoClass = interface(JObjectClass)
  ['{4D839321-4528-4030-88D9-3142BFDAB323}']
    function _GetCREATOR: JParcelable_Creator;
    function _GetFLAG_FOREGROUND: Integer;
    function _GetFLAG_PERSISTENT_PROCESS: Integer;
    function _GetFLAG_STARTED: Integer;
    function _GetFLAG_SYSTEM_PROCESS: Integer;
    function init: JActivityManager_RunningServiceInfo; cdecl;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
    property FLAG_FOREGROUND: Integer read _GetFLAG_FOREGROUND;
    property FLAG_PERSISTENT_PROCESS: Integer read _GetFLAG_PERSISTENT_PROCESS;
    property FLAG_STARTED: Integer read _GetFLAG_STARTED;
    property FLAG_SYSTEM_PROCESS: Integer read _GetFLAG_SYSTEM_PROCESS;
  end;

  [JavaSignature('android/app/ActivityManager$RunningServiceInfo')]
  JActivityManager_RunningServiceInfo = interface(JObject)
  ['{CEECA783-977A-4E16-8907-C4F65F25D168}']
    function _GetactiveSince: Int64;
    function _GetclientCount: Integer;
    function _GetclientLabel: Integer;
    function _GetclientPackage: JString;
    function _GetcrashCount: Integer;
    function _Getflags: Integer;
    function _Getforeground: Boolean;
    function _GetlastActivityTime: Int64;
    function _Getpid: Integer;
    function _Getprocess: JString;
    function _Getrestarting: Int64;
    function _Getservice: JComponentName;
    function _Getstarted: Boolean;
    function _Getuid: Integer;
    procedure _SetactiveSince(Value: Int64);
    procedure _SetclientCount(Value: Integer);
    procedure _SetclientLabel(Value: Integer);
    procedure _SetclientPackage(Value: JString);
    procedure _SetcrashCount(Value: Integer);
    procedure _Setflags(Value: Integer);
    procedure _Setforeground(Value: Boolean);
    procedure _SetlastActivityTime(Value: Int64);
    procedure _Setpid(Value: Integer);
    procedure _Setprocess(Value: JString);
    procedure _Setrestarting(Value: Int64);
    procedure _Setservice(Value: JComponentName);
    procedure _Setstarted(Value: Boolean);
    procedure _Setuid(Value: Integer);
    function describeContents: Integer; cdecl;
    procedure readFromParcel(source: JParcel); cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    property activeSince: Int64 read _GetactiveSince write _SetactiveSince;
    property clientCount: Integer read _GetclientCount write _SetclientCount;
    property clientLabel: Integer read _GetclientLabel write _SetclientLabel;
    property clientPackage: JString read _GetclientPackage write _SetclientPackage;
    property crashCount: Integer read _GetcrashCount write _SetcrashCount;
    property flags: Integer read _Getflags write _Setflags;
    property foreground: Boolean read _Getforeground write _Setforeground;
    property lastActivityTime: Int64 read _GetlastActivityTime write _SetlastActivityTime;
    property pid: Integer read _Getpid write _Setpid;
    property process: JString read _Getprocess write _Setprocess;
    property restarting: Int64 read _Getrestarting write _Setrestarting;
    property service: JComponentName read _Getservice write _Setservice;
    property started: Boolean read _Getstarted write _Setstarted;
    property uid: Integer read _Getuid write _Setuid;
  end;
  TJActivityManager_RunningServiceInfo = class(TJavaGenericImport<JActivityManager_RunningServiceInfoClass, JActivityManager_RunningServiceInfo>)
  end;

  JAsyncTaskClass = interface(JObjectClass)
    ['{C42EC518-9EB8-4F81-943F-8B699D7E81A3}']
    {class} function _GetSERIAL_EXECUTOR: JExecutor; cdecl;
    {class} function _GetTHREAD_POOL_EXECUTOR: JExecutor; cdecl;
    {class} function init: JAsyncTask; cdecl;
    {class} property SERIAL_EXECUTOR: JExecutor read _GetSERIAL_EXECUTOR;
    {class} property THREAD_POOL_EXECUTOR: JExecutor read _GetTHREAD_POOL_EXECUTOR;
  end;

  [JavaSignature('android/os/AsyncTask')]
  JAsyncTask = interface(JObject)
    ['{A87FEC05-8A98-4957-BABD-D11BA361F045}']
    function cancel(mayInterruptIfRunning: Boolean): Boolean; cdecl;
    procedure execute(runnable: JRunnable); cdecl; overload;
    function get: JObject; cdecl; overload;
    function get(timeout: Int64; &unit: JTimeUnit): JObject; cdecl; overload;
    function getStatus: JAsyncTask_Status; cdecl;
    function isCancelled: Boolean; cdecl;
  end;
  TJAsyncTask = class(TJavaGenericImport<JAsyncTaskClass, JAsyncTask>) end;

  JAsyncTask_StatusClass = interface(JEnumClass)
    ['{9FCA442E-277F-4601-8D8E-241B24F15615}']
    {class} function _GetFINISHED: JAsyncTask_Status; cdecl;
    {class} function _GetPENDING: JAsyncTask_Status; cdecl;
    {class} function _GetRUNNING: JAsyncTask_Status; cdecl;
    {class} function valueOf(name: JString): JAsyncTask_Status; cdecl;
    {class} function values: TJavaObjectArray<JAsyncTask_Status>; cdecl;
    {class} property FINISHED: JAsyncTask_Status read _GetFINISHED;
    {class} property PENDING: JAsyncTask_Status read _GetPENDING;
    {class} property RUNNING: JAsyncTask_Status read _GetRUNNING;
  end;

  [JavaSignature('android/os/AsyncTask$Status')]
  JAsyncTask_Status = interface(JEnum)
    ['{DA6490CB-4B27-4CAF-9961-D9129CD65CE9}']
  end;
  TJAsyncTask_Status = class(TJavaGenericImport<JAsyncTask_StatusClass, JAsyncTask_Status>) end;

  JBatteryManagerClass = interface(JObjectClass)
    ['{2FA9FB55-F7F5-4A77-9AB5-53B0B28B0E5C}']
    {class} function _GetACTION_CHARGING: JString; cdecl;
    {class} function _GetACTION_DISCHARGING: JString; cdecl;
    {class} function _GetBATTERY_HEALTH_COLD: Integer; cdecl;
    {class} function _GetBATTERY_HEALTH_DEAD: Integer; cdecl;
    {class} function _GetBATTERY_HEALTH_GOOD: Integer; cdecl;
    {class} function _GetBATTERY_HEALTH_OVERHEAT: Integer; cdecl;
    {class} function _GetBATTERY_HEALTH_OVER_VOLTAGE: Integer; cdecl;
    {class} function _GetBATTERY_HEALTH_UNKNOWN: Integer; cdecl;
    {class} function _GetBATTERY_HEALTH_UNSPECIFIED_FAILURE: Integer; cdecl;
    {class} function _GetBATTERY_PLUGGED_AC: Integer; cdecl;
    {class} function _GetBATTERY_PLUGGED_USB: Integer; cdecl;
    {class} function _GetBATTERY_PLUGGED_WIRELESS: Integer; cdecl;
    {class} function _GetBATTERY_PROPERTY_CAPACITY: Integer; cdecl;
    {class} function _GetBATTERY_PROPERTY_CHARGE_COUNTER: Integer; cdecl;
    {class} function _GetBATTERY_PROPERTY_CURRENT_AVERAGE: Integer; cdecl;
    {class} function _GetBATTERY_PROPERTY_CURRENT_NOW: Integer; cdecl;
    {class} function _GetBATTERY_PROPERTY_ENERGY_COUNTER: Integer; cdecl;
    {class} function _GetBATTERY_STATUS_CHARGING: Integer; cdecl;
    {class} function _GetBATTERY_STATUS_DISCHARGING: Integer; cdecl;
    {class} function _GetBATTERY_STATUS_FULL: Integer; cdecl;
    {class} function _GetBATTERY_STATUS_NOT_CHARGING: Integer; cdecl;
    {class} function _GetBATTERY_STATUS_UNKNOWN: Integer; cdecl;
    {class} function _GetEXTRA_HEALTH: JString; cdecl;
    {class} function _GetEXTRA_ICON_SMALL: JString; cdecl;
    {class} function _GetEXTRA_LEVEL: JString; cdecl;
    {class} function _GetEXTRA_PLUGGED: JString; cdecl;
    {class} function _GetEXTRA_PRESENT: JString; cdecl;
    {class} function _GetEXTRA_SCALE: JString; cdecl;
    {class} function _GetEXTRA_STATUS: JString; cdecl;
    {class} function _GetEXTRA_TECHNOLOGY: JString; cdecl;
    {class} function _GetEXTRA_TEMPERATURE: JString; cdecl;
    {class} function _GetEXTRA_VOLTAGE: JString; cdecl;
    {class} function init: JBatteryManager; cdecl;
    {class} property ACTION_CHARGING: JString read _GetACTION_CHARGING;
    {class} property ACTION_DISCHARGING: JString read _GetACTION_DISCHARGING;
    {class} property BATTERY_HEALTH_COLD: Integer read _GetBATTERY_HEALTH_COLD;
    {class} property BATTERY_HEALTH_DEAD: Integer read _GetBATTERY_HEALTH_DEAD;
    {class} property BATTERY_HEALTH_GOOD: Integer read _GetBATTERY_HEALTH_GOOD;
    {class} property BATTERY_HEALTH_OVERHEAT: Integer read _GetBATTERY_HEALTH_OVERHEAT;
    {class} property BATTERY_HEALTH_OVER_VOLTAGE: Integer read _GetBATTERY_HEALTH_OVER_VOLTAGE;
    {class} property BATTERY_HEALTH_UNKNOWN: Integer read _GetBATTERY_HEALTH_UNKNOWN;
    {class} property BATTERY_HEALTH_UNSPECIFIED_FAILURE: Integer read _GetBATTERY_HEALTH_UNSPECIFIED_FAILURE;
    {class} property BATTERY_PLUGGED_AC: Integer read _GetBATTERY_PLUGGED_AC;
    {class} property BATTERY_PLUGGED_USB: Integer read _GetBATTERY_PLUGGED_USB;
    {class} property BATTERY_PLUGGED_WIRELESS: Integer read _GetBATTERY_PLUGGED_WIRELESS;
    {class} property BATTERY_PROPERTY_CAPACITY: Integer read _GetBATTERY_PROPERTY_CAPACITY;
    {class} property BATTERY_PROPERTY_CHARGE_COUNTER: Integer read _GetBATTERY_PROPERTY_CHARGE_COUNTER;
    {class} property BATTERY_PROPERTY_CURRENT_AVERAGE: Integer read _GetBATTERY_PROPERTY_CURRENT_AVERAGE;
    {class} property BATTERY_PROPERTY_CURRENT_NOW: Integer read _GetBATTERY_PROPERTY_CURRENT_NOW;
    {class} property BATTERY_PROPERTY_ENERGY_COUNTER: Integer read _GetBATTERY_PROPERTY_ENERGY_COUNTER;
    {class} property BATTERY_STATUS_CHARGING: Integer read _GetBATTERY_STATUS_CHARGING;
    {class} property BATTERY_STATUS_DISCHARGING: Integer read _GetBATTERY_STATUS_DISCHARGING;
    {class} property BATTERY_STATUS_FULL: Integer read _GetBATTERY_STATUS_FULL;
    {class} property BATTERY_STATUS_NOT_CHARGING: Integer read _GetBATTERY_STATUS_NOT_CHARGING;
    {class} property BATTERY_STATUS_UNKNOWN: Integer read _GetBATTERY_STATUS_UNKNOWN;
    {class} property EXTRA_HEALTH: JString read _GetEXTRA_HEALTH;
    {class} property EXTRA_ICON_SMALL: JString read _GetEXTRA_ICON_SMALL;
    {class} property EXTRA_LEVEL: JString read _GetEXTRA_LEVEL;
    {class} property EXTRA_PLUGGED: JString read _GetEXTRA_PLUGGED;
    {class} property EXTRA_PRESENT: JString read _GetEXTRA_PRESENT;
    {class} property EXTRA_SCALE: JString read _GetEXTRA_SCALE;
    {class} property EXTRA_STATUS: JString read _GetEXTRA_STATUS;
    {class} property EXTRA_TECHNOLOGY: JString read _GetEXTRA_TECHNOLOGY;
    {class} property EXTRA_TEMPERATURE: JString read _GetEXTRA_TEMPERATURE;
    {class} property EXTRA_VOLTAGE: JString read _GetEXTRA_VOLTAGE;
  end;

  [JavaSignature('android/os/BatteryManager')]
  JBatteryManager = interface(JObject)
    ['{DA84CB9E-2658-4D4A-A8B4-B7602138E2B7}']
    function getIntProperty(id: Integer): Integer; cdecl;
    function getLongProperty(id: Integer): Int64; cdecl;
    function isCharging: Boolean; cdecl;
  end;
  TJBatteryManager = class(TJavaGenericImport<JBatteryManagerClass, JBatteryManager>) end;

  JDebugClass = interface(JObjectClass)
    ['{5759F2AF-54E3-44F4-839A-008BD6964FC5}']
    {class} function _GetSHOW_CLASSLOADER: Integer; cdecl;
    {class} function _GetSHOW_FULL_DETAIL: Integer; cdecl;
    {class} function _GetSHOW_INITIALIZED: Integer; cdecl;
    {class} function _GetTRACE_COUNT_ALLOCS: Integer; cdecl;
    {class} procedure changeDebugPort(port: Integer); cdecl;
    {class} procedure dumpHprofData(fileName: JString); cdecl;
    {class} function dumpService(name: JString; fd: JFileDescriptor; args: TJavaObjectArray<JString>): Boolean; cdecl;
    {class} procedure enableEmulatorTraceOutput; cdecl;
    {class} function getBinderDeathObjectCount: Integer; cdecl;
    {class} function getBinderLocalObjectCount: Integer; cdecl;
    {class} function getBinderProxyObjectCount: Integer; cdecl;
    {class} function getBinderReceivedTransactions: Integer; cdecl;
    {class} function getBinderSentTransactions: Integer; cdecl;
    {class} function getGlobalAllocCount: Integer; cdecl;
    {class} function getGlobalAllocSize: Integer; cdecl;
    {class} function getGlobalClassInitCount: Integer; cdecl;
    {class} function getGlobalClassInitTime: Integer; cdecl;
    {class} function getGlobalExternalAllocCount: Integer; cdecl;
    {class} function getGlobalExternalAllocSize: Integer; cdecl;
    {class} function getGlobalExternalFreedCount: Integer; cdecl;
    {class} function getGlobalExternalFreedSize: Integer; cdecl;
    {class} function getGlobalFreedCount: Integer; cdecl;
    {class} function getGlobalFreedSize: Integer; cdecl;
    {class} function getGlobalGcInvocationCount: Integer; cdecl;
    {class} function getLoadedClassCount: Integer; cdecl;
    {class} procedure getMemoryInfo(memoryInfo: JDebug_MemoryInfo); cdecl;
    {class} function getNativeHeapAllocatedSize: Int64; cdecl;
    {class} function getNativeHeapFreeSize: Int64; cdecl;
    {class} function getNativeHeapSize: Int64; cdecl;
    {class} function getPss: Int64; cdecl;
    {class} function getRuntimeStat(statName: JString): JString; cdecl;
    {class} function getRuntimeStats: JMap; cdecl;
    {class} function getThreadAllocCount: Integer; cdecl;
    {class} function getThreadAllocSize: Integer; cdecl;
    {class} function getThreadExternalAllocCount: Integer; cdecl;
    {class} function getThreadExternalAllocSize: Integer; cdecl;
    {class} function getThreadGcInvocationCount: Integer; cdecl;
    {class} function isDebuggerConnected: Boolean; cdecl;
    {class} procedure printLoadedClasses(flags: Integer); cdecl;
    {class} procedure resetAllCounts; cdecl;
    {class} procedure resetGlobalAllocCount; cdecl;
    {class} procedure resetGlobalAllocSize; cdecl;
    {class} procedure resetGlobalClassInitCount; cdecl;
    {class} procedure resetGlobalClassInitTime; cdecl;
    {class} procedure resetGlobalExternalAllocCount; cdecl;
    {class} procedure resetGlobalExternalAllocSize; cdecl;
    {class} procedure resetGlobalExternalFreedCount; cdecl;
    {class} procedure resetGlobalExternalFreedSize; cdecl;
    {class} procedure resetGlobalFreedCount; cdecl;
    {class} procedure resetGlobalFreedSize; cdecl;
    {class} procedure resetGlobalGcInvocationCount; cdecl;
    {class} procedure resetThreadAllocCount; cdecl;
    {class} procedure resetThreadAllocSize; cdecl;
    {class} procedure resetThreadExternalAllocCount; cdecl;
    {class} procedure resetThreadExternalAllocSize; cdecl;
    {class} procedure resetThreadGcInvocationCount; cdecl;
    {class} function setAllocationLimit(limit: Integer): Integer; cdecl;
    {class} function setGlobalAllocationLimit(limit: Integer): Integer; cdecl;
    {class} procedure startAllocCounting; cdecl;
    {class} procedure startMethodTracing; cdecl; overload;
    {class} procedure startMethodTracing(traceName: JString); cdecl; overload;
    {class} procedure startMethodTracing(traceName: JString; bufferSize: Integer); cdecl; overload;
    {class} procedure startMethodTracing(traceName: JString; bufferSize: Integer; flags: Integer); cdecl; overload;
    {class} procedure startMethodTracingSampling(traceName: JString; bufferSize: Integer; intervalUs: Integer); cdecl;
    {class} procedure startNativeTracing; cdecl;
    {class} procedure stopAllocCounting; cdecl;
    {class} procedure stopMethodTracing; cdecl;
    {class} procedure stopNativeTracing; cdecl;
    {class} function threadCpuTimeNanos: Int64; cdecl;
    {class} procedure waitForDebugger; cdecl;
    {class} function waitingForDebugger: Boolean; cdecl;
    {class} property SHOW_CLASSLOADER: Integer read _GetSHOW_CLASSLOADER;
    {class} property SHOW_FULL_DETAIL: Integer read _GetSHOW_FULL_DETAIL;
    {class} property SHOW_INITIALIZED: Integer read _GetSHOW_INITIALIZED;
    {class} property TRACE_COUNT_ALLOCS: Integer read _GetTRACE_COUNT_ALLOCS;
  end;

  [JavaSignature('android/os/Debug')]
  JDebug = interface(JObject)
    ['{EE4BF6EE-020D-49EF-94F2-03A4787C3D3C}']
  end;
  TJDebug = class(TJavaGenericImport<JDebugClass, JDebug>) end;

  JSystemClass = interface(JObjectClass)
    ['{0CDDA5AF-A679-4D83-A1DF-1B7C9F355E7B}']
    {class} function _Geterr: JPrintStream; cdecl;
    {class} function _Getin: JInputStream; cdecl;
    {class} function _Getout: JPrintStream; cdecl;
    {class} procedure arraycopy(src: JObject; srcPos: Integer; dst: JObject; dstPos: Integer; length: Integer); cdecl;
    {class} function clearProperty(name: JString): JString; cdecl;
    // {class} function console: JConsole; cdecl;
    {class} function currentTimeMillis: Int64; cdecl;
    {class} procedure exit(code: Integer); cdecl;
    {class} procedure gc; cdecl;
    {class} function getProperties: JProperties; cdecl;
    {class} function getProperty(propertyName: JString): JString; cdecl; overload;
    {class} function getProperty(name: JString; defaultValue: JString): JString; cdecl; overload;
    // {class} function getSecurityManager: JSecurityManager; cdecl;
    {class} function getenv(name: JString): JString; cdecl; overload;
    {class} function getenv: JMap; cdecl; overload;
    {class} function identityHashCode(anObject: JObject): Integer; cdecl;
    {class} function inheritedChannel: JChannel; cdecl;
    {class} function lineSeparator: JString; cdecl;
    {class} procedure load(pathName: JString); cdecl;
    {class} procedure loadLibrary(libName: JString); cdecl;
    {class} function mapLibraryName(nickname: JString): JString; cdecl;
    {class} function nanoTime: Int64; cdecl;
    {class} procedure runFinalization; cdecl;
    {class} procedure runFinalizersOnExit(flag: Boolean); cdecl;
    {class} procedure setErr(newErr: JPrintStream); cdecl;
    {class} procedure setIn(newIn: JInputStream); cdecl;
    {class} procedure setOut(newOut: JPrintStream); cdecl;
    {class} procedure setProperties(p: JProperties); cdecl;
    {class} function setProperty(name: JString; value: JString): JString; cdecl;
    // {class} procedure setSecurityManager(sm: JSecurityManager); cdecl;
    {class} property err: JPrintStream read _Geterr;
    {class} property &in: JInputStream read _Getin;
    {class} property &out: JPrintStream read _Getout;
  end;

  [JavaSignature('java/lang/System')]
  JSystem = interface(JObject)
    ['{93E6C8D4-0481-439B-A258-870D01C85DF4}']
  end;
  TJSystem = class(TJavaGenericImport<JSystemClass, JSystem>) end;

  JHandlerThreadClass = interface(JThreadClass)
    ['{FAAD33B5-6400-4F38-B3F9-EE8C85500F15}']
    {class} function init(name: JString): JHandlerThread; cdecl; overload;
    {class} function init(name: JString; priority: Integer): JHandlerThread; cdecl; overload;
  end;

  [JavaSignature('android/os/HandlerThread')]
  JHandlerThread = interface(JThread)
    ['{BCBA93F8-F723-4299-97FA-DFD17D08135E}']
    function getLooper: JLooper; cdecl;
    function getThreadId: Integer; cdecl;
    function quit: Boolean; cdecl;
    function quitSafely: Boolean; cdecl;
    procedure run;
  end;
  TJHandlerThread = class(TJavaGenericImport<JHandlerThreadClass, JHandlerThread>) end;

  JEnvironmentClass = interface(JObjectClass)
    ['{847171A2-7B65-4251-9BD3-E0BC89DE31FD}']
    {class} function _GetDIRECTORY_ALARMS: JString; cdecl;
    {class} procedure _SetDIRECTORY_ALARMS(Value: JString); cdecl;
    {class} function _GetDIRECTORY_DCIM: JString; cdecl;
    {class} function _GetDIRECTORY_DOCUMENTS: JString; cdecl;
    {class} function _GetDIRECTORY_DOWNLOADS: JString; cdecl;
    {class} procedure _SetDIRECTORY_DOWNLOADS(Value: JString); cdecl;
    {class} function _GetDIRECTORY_MOVIES: JString; cdecl;
    {class} function _GetDIRECTORY_MUSIC: JString; cdecl;
    {class} procedure _SetDIRECTORY_MUSIC(Value: JString); cdecl;
    {class} function _GetDIRECTORY_NOTIFICATIONS: JString; cdecl;
    {class} procedure _SetDIRECTORY_NOTIFICATIONS(Value: JString); cdecl;
    {class} function _GetDIRECTORY_PICTURES: JString; cdecl;
    {class} function _GetDIRECTORY_PODCASTS: JString; cdecl;
    {class} procedure _SetDIRECTORY_PODCASTS(Value: JString); cdecl;
    {class} function _GetDIRECTORY_RINGTONES: JString; cdecl;
    {class} function _GetMEDIA_BAD_REMOVAL: JString; cdecl;
    {class} function _GetMEDIA_CHECKING: JString; cdecl;
    {class} function _GetMEDIA_EJECTING: JString; cdecl;
    {class} function _GetMEDIA_MOUNTED: JString; cdecl;
    {class} function _GetMEDIA_MOUNTED_READ_ONLY: JString; cdecl;
    {class} function _GetMEDIA_NOFS: JString; cdecl;
    {class} function _GetMEDIA_REMOVED: JString; cdecl;
    {class} function _GetMEDIA_SHARED: JString; cdecl;
    {class} function _GetMEDIA_UNKNOWN: JString; cdecl;
    {class} function _GetMEDIA_UNMOUNTABLE: JString; cdecl;
    {class} function _GetMEDIA_UNMOUNTED: JString; cdecl;
    {class} function init: JEnvironment; cdecl;
    {class} function getDataDirectory: JFile; cdecl;
    {class} function getDownloadCacheDirectory: JFile; cdecl;
    {class} function getExternalStorageDirectory: JFile; cdecl;
    {class} function getExternalStoragePublicDirectory(&type: JString): JFile; cdecl;
    {class} function getExternalStorageState: JString; cdecl; overload;
    {class} function getExternalStorageState(path: JFile): JString; cdecl; overload;
    {class} function getRootDirectory: JFile; cdecl;
    {class} function getStorageState(path: JFile): JString; cdecl;
    {class} function isExternalStorageEmulated: Boolean; cdecl; overload;
    {class} function isExternalStorageEmulated(path: JFile): Boolean; cdecl; overload;
    {class} function isExternalStorageRemovable: Boolean; cdecl; overload;
    {class} function isExternalStorageRemovable(path: JFile): Boolean; cdecl; overload;
    {class} property DIRECTORY_ALARMS: JString read _GetDIRECTORY_ALARMS write _SetDIRECTORY_ALARMS;
    {class} property DIRECTORY_DCIM: JString read _GetDIRECTORY_DCIM;
    {class} property DIRECTORY_DOCUMENTS: JString read _GetDIRECTORY_DOCUMENTS;
    {class} property DIRECTORY_DOWNLOADS: JString read _GetDIRECTORY_DOWNLOADS write _SetDIRECTORY_DOWNLOADS;
    {class} property DIRECTORY_MOVIES: JString read _GetDIRECTORY_MOVIES;
    {class} property DIRECTORY_MUSIC: JString read _GetDIRECTORY_MUSIC write _SetDIRECTORY_MUSIC;
    {class} property DIRECTORY_NOTIFICATIONS: JString read _GetDIRECTORY_NOTIFICATIONS write _SetDIRECTORY_NOTIFICATIONS;
    {class} property DIRECTORY_PICTURES: JString read _GetDIRECTORY_PICTURES;
    {class} property DIRECTORY_PODCASTS: JString read _GetDIRECTORY_PODCASTS write _SetDIRECTORY_PODCASTS;
    {class} property DIRECTORY_RINGTONES: JString read _GetDIRECTORY_RINGTONES;
    {class} property MEDIA_BAD_REMOVAL: JString read _GetMEDIA_BAD_REMOVAL;
    {class} property MEDIA_CHECKING: JString read _GetMEDIA_CHECKING;
    {class} property MEDIA_EJECTING: JString read _GetMEDIA_EJECTING;
    {class} property MEDIA_MOUNTED: JString read _GetMEDIA_MOUNTED;
    {class} property MEDIA_MOUNTED_READ_ONLY: JString read _GetMEDIA_MOUNTED_READ_ONLY;
    {class} property MEDIA_NOFS: JString read _GetMEDIA_NOFS;
    {class} property MEDIA_REMOVED: JString read _GetMEDIA_REMOVED;
    {class} property MEDIA_SHARED: JString read _GetMEDIA_SHARED;
    {class} property MEDIA_UNKNOWN: JString read _GetMEDIA_UNKNOWN;
    {class} property MEDIA_UNMOUNTABLE: JString read _GetMEDIA_UNMOUNTABLE;
    {class} property MEDIA_UNMOUNTED: JString read _GetMEDIA_UNMOUNTED;
  end;

  [JavaSignature('android/os/Environment')]
  JEnvironment = interface(JObject)
    ['{8A8591BC-BC01-4338-91D8-2671DAB231F8}']
  end;
  TJEnvironment = class(TJavaGenericImport<JEnvironmentClass, JEnvironment>) end;

  JStatFsClass = interface(JObjectClass)
    ['{F97A99DF-CDC1-4842-80F2-2EA53A906E3E}']
    {class} function init(path: JString): JStatFs; cdecl;
  end;

  [JavaSignature('android/os/StatFs')]
  JStatFs = interface(JObject)
    ['{C34856EE-443F-42CB-B25B-DEC0B8C938D0}']
    function getAvailableBlocks: Integer; cdecl;
    function getAvailableBlocksLong: Int64; cdecl;
    function getAvailableBytes: Int64; cdecl;
    function getBlockCount: Integer; cdecl;
    function getBlockCountLong: Int64; cdecl;
    function getBlockSize: Integer; cdecl;
    function getBlockSizeLong: Int64; cdecl;
    function getFreeBlocks: Integer; cdecl;
    function getFreeBlocksLong: Int64; cdecl;
    function getFreeBytes: Int64; cdecl;
    function getTotalBytes: Int64; cdecl;
    procedure restat(path: JString); cdecl;
  end;
  TJStatFs = class(TJavaGenericImport<JStatFsClass, JStatFs>) end;

  JSystemClockClass = interface(JObjectClass)
    ['{771C5E34-6252-4BA7-8292-DD6BC82AA9B8}']
    {class} function currentThreadTimeMillis: Int64; cdecl;
    {class} function elapsedRealtime: Int64; cdecl;
    {class} function elapsedRealtimeNanos: Int64; cdecl;
    {class} function setCurrentTimeMillis(millis: Int64): Boolean; cdecl;
    {class} procedure sleep(ms: Int64); cdecl;
    {class} function uptimeMillis: Int64; cdecl;
  end;

  [JavaSignature('android/os/SystemClock')]
  JSystemClock = interface(JObject)
    ['{6F88CF0F-2D6B-43D4-A23D-A04C1C56D88E}']
  end;
  TJSystemClock = class(TJavaGenericImport<JSystemClockClass, JSystemClock>) end;

implementation

end.
