unit DW.Androidapi.JNI.Os;

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
  JProcess = interface;
  JStatFs = interface;
  JSystem = interface;
  JUserManager = interface;

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
    {class} function _GetDIRECTORY_AUDIOBOOKS: JString; cdecl;
    {class} procedure _SetDIRECTORY_AUDIOBOOKS(Value: JString); cdecl;
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
    {class} function _GetDIRECTORY_SCREENSHOTS: JString; cdecl;
    {class} procedure _SetDIRECTORY_SCREENSHOTS(Value: JString); cdecl;
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
    {class} function getStorageDirectory: JFile; cdecl; // **** Android 11 ****
    {class} function isExternalStorageEmulated: Boolean; cdecl; overload;
    {class} function isExternalStorageEmulated(path: JFile): Boolean; cdecl; overload;
    {class} function isExternalStorageLegacy: Boolean; cdecl; overload; // **** Android 10 ****
    {class} function isExternalStorageLegacy(path: JFile): Boolean; cdecl; overload; // **** Android 10 ****
    {class} function isExternalStorageManager: Boolean; cdecl; overload; // **** Android 11 ****
    {class} function isExternalStorageManager(path: JFile): Boolean; cdecl; overload; // **** Android 11 ****
    {class} function isExternalStorageRemovable: Boolean; cdecl; overload;
    {class} function isExternalStorageRemovable(path: JFile): Boolean; cdecl; overload;
    {class} property DIRECTORY_ALARMS: JString read _GetDIRECTORY_ALARMS write _SetDIRECTORY_ALARMS;
    {class} property DIRECTORY_AUDIOBOOKS: JString read _GetDIRECTORY_AUDIOBOOKS write _SetDIRECTORY_AUDIOBOOKS; // **** Android 10 ****
    {class} property DIRECTORY_DCIM: JString read _GetDIRECTORY_DCIM;
    {class} property DIRECTORY_DOCUMENTS: JString read _GetDIRECTORY_DOCUMENTS;
    {class} property DIRECTORY_DOWNLOADS: JString read _GetDIRECTORY_DOWNLOADS write _SetDIRECTORY_DOWNLOADS;
    {class} property DIRECTORY_MOVIES: JString read _GetDIRECTORY_MOVIES;
    {class} property DIRECTORY_MUSIC: JString read _GetDIRECTORY_MUSIC write _SetDIRECTORY_MUSIC;
    {class} property DIRECTORY_NOTIFICATIONS: JString read _GetDIRECTORY_NOTIFICATIONS write _SetDIRECTORY_NOTIFICATIONS;
    {class} property DIRECTORY_PICTURES: JString read _GetDIRECTORY_PICTURES;
    {class} property DIRECTORY_PODCASTS: JString read _GetDIRECTORY_PODCASTS write _SetDIRECTORY_PODCASTS;
    {class} property DIRECTORY_RINGTONES: JString read _GetDIRECTORY_RINGTONES;
    {class} property DIRECTORY_SCREENSHOTS: JString read _GetDIRECTORY_SCREENSHOTS write _SetDIRECTORY_SCREENSHOTS; // **** Android 10 ****
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

  JProcessClass = interface(JObjectClass)
    ['{777EE25A-1E3F-4020-A358-ECDEB8E75497}']
    {class} function _GetFIRST_APPLICATION_UID: Integer; cdecl;
    {class} function _GetLAST_APPLICATION_UID: Integer; cdecl;
    {class} function _GetPHONE_UID: Integer; cdecl;
    {class} function _GetSIGNAL_KILL: Integer; cdecl;
    {class} function _GetSIGNAL_QUIT: Integer; cdecl;
    {class} function _GetSIGNAL_USR1: Integer; cdecl;
    {class} function _GetSYSTEM_UID: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_AUDIO: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_BACKGROUND: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_DEFAULT: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_DISPLAY: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_FOREGROUND: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_LESS_FAVORABLE: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_LOWEST: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_MORE_FAVORABLE: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_URGENT_AUDIO: Integer; cdecl;
    {class} function _GetTHREAD_PRIORITY_URGENT_DISPLAY: Integer; cdecl;
    {class} function init: JProcess; cdecl;
    {class} function getElapsedCpuTime: Int64; cdecl;
    {class} function getExclusiveCores: TJavaArray<Integer>; cdecl;
    {class} function getGidForName(name: JString): Integer; cdecl;
    {class} function getStartElapsedRealtime: Int64; cdecl;
    {class} function getStartRequestedElapsedRealtime: Int64; cdecl;
    {class} function getStartRequestedUptimeMillis: Int64; cdecl;
    {class} function getStartUptimeMillis: Int64; cdecl;
    {class} function getThreadPriority(tid: Integer): Integer; cdecl;
    {class} function getUidForName(name: JString): Integer; cdecl;
    {class} function is64Bit: Boolean; cdecl;
    {class} function isApplicationUid(uid: Integer): Boolean; cdecl;
    {class} function isIsolated: Boolean; cdecl;
    {class} function isSdkSandbox: Boolean; cdecl;
    {class} procedure killProcess(pid: Integer); cdecl;
    {class} function myPid: Integer; cdecl;
    {class} function myProcessName: JString; cdecl;
    {class} function myTid: Integer; cdecl;
    {class} function myUid: Integer; cdecl;
    {class} function myUserHandle: JUserHandle; cdecl;
    {class} procedure sendSignal(pid: Integer; signal: Integer); cdecl;
    {class} procedure setThreadPriority(tid: Integer; priority: Integer); cdecl; overload;
    {class} procedure setThreadPriority(priority: Integer); cdecl; overload;
    {class} function supportsProcesses: Boolean; cdecl;
    {class} property FIRST_APPLICATION_UID: Integer read _GetFIRST_APPLICATION_UID;
    {class} property LAST_APPLICATION_UID: Integer read _GetLAST_APPLICATION_UID;
    {class} property PHONE_UID: Integer read _GetPHONE_UID;
    {class} property SIGNAL_KILL: Integer read _GetSIGNAL_KILL;
    {class} property SIGNAL_QUIT: Integer read _GetSIGNAL_QUIT;
    {class} property SIGNAL_USR1: Integer read _GetSIGNAL_USR1;
    {class} property SYSTEM_UID: Integer read _GetSYSTEM_UID;
    {class} property THREAD_PRIORITY_AUDIO: Integer read _GetTHREAD_PRIORITY_AUDIO;
    {class} property THREAD_PRIORITY_BACKGROUND: Integer read _GetTHREAD_PRIORITY_BACKGROUND;
    {class} property THREAD_PRIORITY_DEFAULT: Integer read _GetTHREAD_PRIORITY_DEFAULT;
    {class} property THREAD_PRIORITY_DISPLAY: Integer read _GetTHREAD_PRIORITY_DISPLAY;
    {class} property THREAD_PRIORITY_FOREGROUND: Integer read _GetTHREAD_PRIORITY_FOREGROUND;
    {class} property THREAD_PRIORITY_LESS_FAVORABLE: Integer read _GetTHREAD_PRIORITY_LESS_FAVORABLE;
    {class} property THREAD_PRIORITY_LOWEST: Integer read _GetTHREAD_PRIORITY_LOWEST;
    {class} property THREAD_PRIORITY_MORE_FAVORABLE: Integer read _GetTHREAD_PRIORITY_MORE_FAVORABLE;
    {class} property THREAD_PRIORITY_URGENT_AUDIO: Integer read _GetTHREAD_PRIORITY_URGENT_AUDIO;
    {class} property THREAD_PRIORITY_URGENT_DISPLAY: Integer read _GetTHREAD_PRIORITY_URGENT_DISPLAY;
  end;

  [JavaSignature('android/os/Process')]
  JProcess = interface(JObject)
    ['{059FFAB5-0122-4794-8EC1-4AD499A58619}']
  end;
  TJProcess = class(TJavaGenericImport<JProcessClass, JProcess>) end;

  JUserManagerClass = interface(JObjectClass)
    ['{326A97A8-A3FD-433E-B8F6-49277E032EA4}']
    {class} function _GetALLOW_PARENT_PROFILE_APP_LINKING: JString; cdecl;
    {class} function _GetDISALLOW_ADD_MANAGED_PROFILE: JString; cdecl;
    {class} function _GetDISALLOW_ADD_PRIVATE_PROFILE: JString; cdecl;
    {class} function _GetDISALLOW_ADD_USER: JString; cdecl;
    {class} function _GetDISALLOW_ADD_WIFI_CONFIG: JString; cdecl;
    {class} function _GetDISALLOW_ADJUST_VOLUME: JString; cdecl;
    {class} function _GetDISALLOW_AIRPLANE_MODE: JString; cdecl;
    {class} function _GetDISALLOW_AMBIENT_DISPLAY: JString; cdecl;
    {class} function _GetDISALLOW_APPS_CONTROL: JString; cdecl;
    {class} function _GetDISALLOW_ASSIST_CONTENT: JString; cdecl;
    {class} function _GetDISALLOW_AUTOFILL: JString; cdecl;
    {class} function _GetDISALLOW_BLUETOOTH: JString; cdecl;
    {class} function _GetDISALLOW_BLUETOOTH_SHARING: JString; cdecl;
    {class} function _GetDISALLOW_CAMERA_TOGGLE: JString; cdecl;
    {class} function _GetDISALLOW_CELLULAR_2G: JString; cdecl;
    {class} function _GetDISALLOW_CHANGE_WIFI_STATE: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_BLUETOOTH: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_BRIGHTNESS: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_CELL_BROADCASTS: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_CREDENTIALS: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_DATE_TIME: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_DEFAULT_APPS: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_LOCALE: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_LOCATION: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_MOBILE_NETWORKS: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_PRIVATE_DNS: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_SCREEN_TIMEOUT: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_TETHERING: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_VPN: JString; cdecl;
    {class} function _GetDISALLOW_CONFIG_WIFI: JString; cdecl;
    {class} function _GetDISALLOW_CONTENT_CAPTURE: JString; cdecl;
    {class} function _GetDISALLOW_CONTENT_SUGGESTIONS: JString; cdecl;
    {class} function _GetDISALLOW_CREATE_WINDOWS: JString; cdecl;
    {class} function _GetDISALLOW_CROSS_PROFILE_COPY_PASTE: JString; cdecl;
    {class} function _GetDISALLOW_DATA_ROAMING: JString; cdecl;
    {class} function _GetDISALLOW_DEBUGGING_FEATURES: JString; cdecl;
    {class} function _GetDISALLOW_FACTORY_RESET: JString; cdecl;
    {class} function _GetDISALLOW_FUN: JString; cdecl;
    {class} function _GetDISALLOW_GRANT_ADMIN: JString; cdecl;
    {class} function _GetDISALLOW_INSTALL_APPS: JString; cdecl;
    {class} function _GetDISALLOW_INSTALL_UNKNOWN_SOURCES: JString; cdecl;
    {class} function _GetDISALLOW_INSTALL_UNKNOWN_SOURCES_GLOBALLY: JString; cdecl;
    {class} function _GetDISALLOW_MICROPHONE_TOGGLE: JString; cdecl;
    {class} function _GetDISALLOW_MODIFY_ACCOUNTS: JString; cdecl;
    {class} function _GetDISALLOW_MOUNT_PHYSICAL_MEDIA: JString; cdecl;
    {class} function _GetDISALLOW_NEAR_FIELD_COMMUNICATION_RADIO: JString; cdecl;
    {class} function _GetDISALLOW_NETWORK_RESET: JString; cdecl;
    {class} function _GetDISALLOW_OUTGOING_BEAM: JString; cdecl;
    {class} function _GetDISALLOW_OUTGOING_CALLS: JString; cdecl;
    {class} function _GetDISALLOW_PRINTING: JString; cdecl;
    {class} function _GetDISALLOW_REMOVE_MANAGED_PROFILE: JString; cdecl;
    {class} function _GetDISALLOW_REMOVE_USER: JString; cdecl;
    {class} function _GetDISALLOW_SAFE_BOOT: JString; cdecl;
    {class} function _GetDISALLOW_SET_USER_ICON: JString; cdecl;
    {class} function _GetDISALLOW_SET_WALLPAPER: JString; cdecl;
    {class} function _GetDISALLOW_SHARE_INTO_MANAGED_PROFILE: JString; cdecl;
    {class} function _GetDISALLOW_SHARE_LOCATION: JString; cdecl;
    {class} function _GetDISALLOW_SHARING_ADMIN_CONFIGURED_WIFI: JString; cdecl;
    {class} function _GetDISALLOW_SIM_GLOBALLY: JString; cdecl;
    {class} function _GetDISALLOW_SMS: JString; cdecl;
    {class} function _GetDISALLOW_SYSTEM_ERROR_DIALOGS: JString; cdecl;
    {class} function _GetDISALLOW_ULTRA_WIDEBAND_RADIO: JString; cdecl;
    {class} function _GetDISALLOW_UNIFIED_PASSWORD: JString; cdecl;
    {class} function _GetDISALLOW_UNINSTALL_APPS: JString; cdecl;
    {class} function _GetDISALLOW_UNMUTE_MICROPHONE: JString; cdecl;
    {class} function _GetDISALLOW_USB_FILE_TRANSFER: JString; cdecl;
    {class} function _GetDISALLOW_USER_SWITCH: JString; cdecl;
    {class} function _GetDISALLOW_WIFI_DIRECT: JString; cdecl;
    {class} function _GetDISALLOW_WIFI_TETHERING: JString; cdecl;
    {class} function _GetENSURE_VERIFY_APPS: JString; cdecl;
    {class} function _GetKEY_RESTRICTIONS_PENDING: JString; cdecl;
    {class} function _GetQUIET_MODE_DISABLE_ONLY_IF_CREDENTIAL_NOT_REQUIRED: Integer; cdecl;
    {class} function _GetUSER_CREATION_FAILED_NOT_PERMITTED: Integer; cdecl;
    {class} function _GetUSER_CREATION_FAILED_NO_MORE_USERS: Integer; cdecl;
    {class} function _GetUSER_OPERATION_ERROR_CURRENT_USER: Integer; cdecl;
    {class} function _GetUSER_OPERATION_ERROR_LOW_STORAGE: Integer; cdecl;
    {class} function _GetUSER_OPERATION_ERROR_MANAGED_PROFILE: Integer; cdecl;
    {class} function _GetUSER_OPERATION_ERROR_MAX_RUNNING_USERS: Integer; cdecl;
    {class} function _GetUSER_OPERATION_ERROR_MAX_USERS: Integer; cdecl;
    {class} function _GetUSER_OPERATION_ERROR_UNKNOWN: Integer; cdecl;
    {class} function _GetUSER_OPERATION_SUCCESS: Integer; cdecl;
    {class} function _GetUSER_TYPE_PROFILE_CLONE: JString; cdecl;
    {class} function _GetUSER_TYPE_PROFILE_MANAGED: JString; cdecl;
    {class} function _GetUSER_TYPE_PROFILE_PRIVATE: JString; cdecl;
    {class} function createUserCreationIntent(string_1: JString; string_2: JString; string_3: JString; persistablebundle: JPersistableBundle): JIntent; cdecl;
    {class} function isHeadlessSystemUserMode: Boolean; cdecl;
    {class} function supportsMultipleUsers: Boolean; cdecl;
    {class} property ALLOW_PARENT_PROFILE_APP_LINKING: JString read _GetALLOW_PARENT_PROFILE_APP_LINKING;
    {class} property DISALLOW_ADD_MANAGED_PROFILE: JString read _GetDISALLOW_ADD_MANAGED_PROFILE;
    {class} property DISALLOW_ADD_PRIVATE_PROFILE: JString read _GetDISALLOW_ADD_PRIVATE_PROFILE;
    {class} property DISALLOW_ADD_USER: JString read _GetDISALLOW_ADD_USER;
    {class} property DISALLOW_ADD_WIFI_CONFIG: JString read _GetDISALLOW_ADD_WIFI_CONFIG;
    {class} property DISALLOW_ADJUST_VOLUME: JString read _GetDISALLOW_ADJUST_VOLUME;
    {class} property DISALLOW_AIRPLANE_MODE: JString read _GetDISALLOW_AIRPLANE_MODE;
    {class} property DISALLOW_AMBIENT_DISPLAY: JString read _GetDISALLOW_AMBIENT_DISPLAY;
    {class} property DISALLOW_APPS_CONTROL: JString read _GetDISALLOW_APPS_CONTROL;
    {class} property DISALLOW_ASSIST_CONTENT: JString read _GetDISALLOW_ASSIST_CONTENT;
    {class} property DISALLOW_AUTOFILL: JString read _GetDISALLOW_AUTOFILL;
    {class} property DISALLOW_BLUETOOTH: JString read _GetDISALLOW_BLUETOOTH;
    {class} property DISALLOW_BLUETOOTH_SHARING: JString read _GetDISALLOW_BLUETOOTH_SHARING;
    {class} property DISALLOW_CAMERA_TOGGLE: JString read _GetDISALLOW_CAMERA_TOGGLE;
    {class} property DISALLOW_CELLULAR_2G: JString read _GetDISALLOW_CELLULAR_2G;
    {class} property DISALLOW_CHANGE_WIFI_STATE: JString read _GetDISALLOW_CHANGE_WIFI_STATE;
    {class} property DISALLOW_CONFIG_BLUETOOTH: JString read _GetDISALLOW_CONFIG_BLUETOOTH;
    {class} property DISALLOW_CONFIG_BRIGHTNESS: JString read _GetDISALLOW_CONFIG_BRIGHTNESS;
    {class} property DISALLOW_CONFIG_CELL_BROADCASTS: JString read _GetDISALLOW_CONFIG_CELL_BROADCASTS;
    {class} property DISALLOW_CONFIG_CREDENTIALS: JString read _GetDISALLOW_CONFIG_CREDENTIALS;
    {class} property DISALLOW_CONFIG_DATE_TIME: JString read _GetDISALLOW_CONFIG_DATE_TIME;
    {class} property DISALLOW_CONFIG_DEFAULT_APPS: JString read _GetDISALLOW_CONFIG_DEFAULT_APPS;
    {class} property DISALLOW_CONFIG_LOCALE: JString read _GetDISALLOW_CONFIG_LOCALE;
    {class} property DISALLOW_CONFIG_LOCATION: JString read _GetDISALLOW_CONFIG_LOCATION;
    {class} property DISALLOW_CONFIG_MOBILE_NETWORKS: JString read _GetDISALLOW_CONFIG_MOBILE_NETWORKS;
    {class} property DISALLOW_CONFIG_PRIVATE_DNS: JString read _GetDISALLOW_CONFIG_PRIVATE_DNS;
    {class} property DISALLOW_CONFIG_SCREEN_TIMEOUT: JString read _GetDISALLOW_CONFIG_SCREEN_TIMEOUT;
    {class} property DISALLOW_CONFIG_TETHERING: JString read _GetDISALLOW_CONFIG_TETHERING;
    {class} property DISALLOW_CONFIG_VPN: JString read _GetDISALLOW_CONFIG_VPN;
    {class} property DISALLOW_CONFIG_WIFI: JString read _GetDISALLOW_CONFIG_WIFI;
    {class} property DISALLOW_CONTENT_CAPTURE: JString read _GetDISALLOW_CONTENT_CAPTURE;
    {class} property DISALLOW_CONTENT_SUGGESTIONS: JString read _GetDISALLOW_CONTENT_SUGGESTIONS;
    {class} property DISALLOW_CREATE_WINDOWS: JString read _GetDISALLOW_CREATE_WINDOWS;
    {class} property DISALLOW_CROSS_PROFILE_COPY_PASTE: JString read _GetDISALLOW_CROSS_PROFILE_COPY_PASTE;
    {class} property DISALLOW_DATA_ROAMING: JString read _GetDISALLOW_DATA_ROAMING;
    {class} property DISALLOW_DEBUGGING_FEATURES: JString read _GetDISALLOW_DEBUGGING_FEATURES;
    {class} property DISALLOW_FACTORY_RESET: JString read _GetDISALLOW_FACTORY_RESET;
    {class} property DISALLOW_FUN: JString read _GetDISALLOW_FUN;
    {class} property DISALLOW_GRANT_ADMIN: JString read _GetDISALLOW_GRANT_ADMIN;
    {class} property DISALLOW_INSTALL_APPS: JString read _GetDISALLOW_INSTALL_APPS;
    {class} property DISALLOW_INSTALL_UNKNOWN_SOURCES: JString read _GetDISALLOW_INSTALL_UNKNOWN_SOURCES;
    {class} property DISALLOW_INSTALL_UNKNOWN_SOURCES_GLOBALLY: JString read _GetDISALLOW_INSTALL_UNKNOWN_SOURCES_GLOBALLY;
    {class} property DISALLOW_MICROPHONE_TOGGLE: JString read _GetDISALLOW_MICROPHONE_TOGGLE;
    {class} property DISALLOW_MODIFY_ACCOUNTS: JString read _GetDISALLOW_MODIFY_ACCOUNTS;
    {class} property DISALLOW_MOUNT_PHYSICAL_MEDIA: JString read _GetDISALLOW_MOUNT_PHYSICAL_MEDIA;
    {class} property DISALLOW_NEAR_FIELD_COMMUNICATION_RADIO: JString read _GetDISALLOW_NEAR_FIELD_COMMUNICATION_RADIO;
    {class} property DISALLOW_NETWORK_RESET: JString read _GetDISALLOW_NETWORK_RESET;
    {class} property DISALLOW_OUTGOING_BEAM: JString read _GetDISALLOW_OUTGOING_BEAM;
    {class} property DISALLOW_OUTGOING_CALLS: JString read _GetDISALLOW_OUTGOING_CALLS;
    {class} property DISALLOW_PRINTING: JString read _GetDISALLOW_PRINTING;
    {class} property DISALLOW_REMOVE_MANAGED_PROFILE: JString read _GetDISALLOW_REMOVE_MANAGED_PROFILE;
    {class} property DISALLOW_REMOVE_USER: JString read _GetDISALLOW_REMOVE_USER;
    {class} property DISALLOW_SAFE_BOOT: JString read _GetDISALLOW_SAFE_BOOT;
    {class} property DISALLOW_SET_USER_ICON: JString read _GetDISALLOW_SET_USER_ICON;
    {class} property DISALLOW_SET_WALLPAPER: JString read _GetDISALLOW_SET_WALLPAPER;
    {class} property DISALLOW_SHARE_INTO_MANAGED_PROFILE: JString read _GetDISALLOW_SHARE_INTO_MANAGED_PROFILE;
    {class} property DISALLOW_SHARE_LOCATION: JString read _GetDISALLOW_SHARE_LOCATION;
    {class} property DISALLOW_SHARING_ADMIN_CONFIGURED_WIFI: JString read _GetDISALLOW_SHARING_ADMIN_CONFIGURED_WIFI;
    {class} property DISALLOW_SIM_GLOBALLY: JString read _GetDISALLOW_SIM_GLOBALLY;
    {class} property DISALLOW_SMS: JString read _GetDISALLOW_SMS;
    {class} property DISALLOW_SYSTEM_ERROR_DIALOGS: JString read _GetDISALLOW_SYSTEM_ERROR_DIALOGS;
    {class} property DISALLOW_ULTRA_WIDEBAND_RADIO: JString read _GetDISALLOW_ULTRA_WIDEBAND_RADIO;
    {class} property DISALLOW_UNIFIED_PASSWORD: JString read _GetDISALLOW_UNIFIED_PASSWORD;
    {class} property DISALLOW_UNINSTALL_APPS: JString read _GetDISALLOW_UNINSTALL_APPS;
    {class} property DISALLOW_UNMUTE_MICROPHONE: JString read _GetDISALLOW_UNMUTE_MICROPHONE;
    {class} property DISALLOW_USB_FILE_TRANSFER: JString read _GetDISALLOW_USB_FILE_TRANSFER;
    {class} property DISALLOW_USER_SWITCH: JString read _GetDISALLOW_USER_SWITCH;
    {class} property DISALLOW_WIFI_DIRECT: JString read _GetDISALLOW_WIFI_DIRECT;
    {class} property DISALLOW_WIFI_TETHERING: JString read _GetDISALLOW_WIFI_TETHERING;
    {class} property ENSURE_VERIFY_APPS: JString read _GetENSURE_VERIFY_APPS;
    {class} property KEY_RESTRICTIONS_PENDING: JString read _GetKEY_RESTRICTIONS_PENDING;
    {class} property QUIET_MODE_DISABLE_ONLY_IF_CREDENTIAL_NOT_REQUIRED: Integer read _GetQUIET_MODE_DISABLE_ONLY_IF_CREDENTIAL_NOT_REQUIRED;
    {class} property USER_CREATION_FAILED_NOT_PERMITTED: Integer read _GetUSER_CREATION_FAILED_NOT_PERMITTED;
    {class} property USER_CREATION_FAILED_NO_MORE_USERS: Integer read _GetUSER_CREATION_FAILED_NO_MORE_USERS;
    {class} property USER_OPERATION_ERROR_CURRENT_USER: Integer read _GetUSER_OPERATION_ERROR_CURRENT_USER;
    {class} property USER_OPERATION_ERROR_LOW_STORAGE: Integer read _GetUSER_OPERATION_ERROR_LOW_STORAGE;
    {class} property USER_OPERATION_ERROR_MANAGED_PROFILE: Integer read _GetUSER_OPERATION_ERROR_MANAGED_PROFILE;
    {class} property USER_OPERATION_ERROR_MAX_RUNNING_USERS: Integer read _GetUSER_OPERATION_ERROR_MAX_RUNNING_USERS;
    {class} property USER_OPERATION_ERROR_MAX_USERS: Integer read _GetUSER_OPERATION_ERROR_MAX_USERS;
    {class} property USER_OPERATION_ERROR_UNKNOWN: Integer read _GetUSER_OPERATION_ERROR_UNKNOWN;
    {class} property USER_OPERATION_SUCCESS: Integer read _GetUSER_OPERATION_SUCCESS;
    {class} property USER_TYPE_PROFILE_CLONE: JString read _GetUSER_TYPE_PROFILE_CLONE;
    {class} property USER_TYPE_PROFILE_MANAGED: JString read _GetUSER_TYPE_PROFILE_MANAGED;
    {class} property USER_TYPE_PROFILE_PRIVATE: JString read _GetUSER_TYPE_PROFILE_PRIVATE;
  end;

  [JavaSignature('android/os/UserManager')]
  JUserManager = interface(JObject)
    ['{C74DA311-0C6E-49FC-A51E-211106A1DDCE}']
    function getApplicationRestrictions(string_1: JString): JBundle; cdecl;
    function getSerialNumberForUser(userhandle: JUserHandle): Int64; cdecl;
    function getUserCount: Integer; cdecl;
    function getUserCreationTime(userhandle: JUserHandle): Int64; cdecl;
    function getUserForSerialNumber(long: Int64): JUserHandle; cdecl;
    function getUserName: JString; cdecl;
    function getUserProfiles: JList; cdecl;
    function getUserRestrictions: JBundle; overload; cdecl;
    function getUserRestrictions(userhandle: JUserHandle): JBundle; overload; cdecl;
    function hasUserRestriction(string_1: JString): Boolean; cdecl;
    function isAdminUser: Boolean; cdecl;
    function isDemoUser: Boolean; cdecl;
    function isManagedProfile: Boolean; cdecl;
    function isProfile: Boolean; cdecl;
    function isQuietModeEnabled(userhandle: JUserHandle): Boolean; cdecl;
    function isSystemUser: Boolean; cdecl;
    function isUserAGoat: Boolean; cdecl;
    function isUserForeground: Boolean; cdecl;
    function isUserRunning(userhandle: JUserHandle): Boolean; cdecl;
    function isUserRunningOrStopping(userhandle: JUserHandle): Boolean; cdecl;
    function isUserUnlocked(userhandle: JUserHandle): Boolean; overload; cdecl;
    function isUserUnlocked: Boolean; overload; cdecl;
    function requestQuietModeEnabled(boolean: Boolean; userhandle_1: JUserHandle): Boolean; overload; cdecl;
    function requestQuietModeEnabled(boolean: Boolean; userhandle_1: JUserHandle; int: Integer): Boolean; overload; cdecl;
    function setRestrictionsChallenge(string_1: JString): Boolean; cdecl;
    procedure setUserRestriction(string_1: JString; boolean: Boolean); cdecl;
    procedure setUserRestrictions(bundle: JBundle); overload; cdecl;
    procedure setUserRestrictions(bundle: JBundle; userhandle: JUserHandle); overload; cdecl;
  end;
  TJUserManager = class(TJavaGenericImport<JUserManagerClass, JUserManager>) end;

implementation

end.
