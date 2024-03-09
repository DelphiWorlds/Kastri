unit DW.Androidapi.JNI.Content;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText;

type
  JContextWrapper = interface;
  JPackageManager_ResolveInfoFlags = interface;

  JContextWrapperClass = interface(JContextClass)
    ['{EA8706C6-B2D2-41C0-935D-838BB8704209}']
    {class} function init(base: JContext): JContextWrapper; cdecl;
  end;

  [JavaSignature('android/content/ContextWrapper')]
  JContextWrapper = interface(JContext)
    ['{D742A401-5631-42C5-9B65-6F1F46811A40}']
    function bindService(service: JIntent; conn: JServiceConnection; flags: Integer): Boolean; cdecl;
    function checkCallingOrSelfPermission(permission: JString): Integer; cdecl;
    function checkCallingOrSelfUriPermission(uri: Jnet_Uri; modeFlags: Integer): Integer; cdecl;
    function checkCallingPermission(permission: JString): Integer; cdecl;
    function checkCallingUriPermission(uri: Jnet_Uri; modeFlags: Integer): Integer; cdecl;
    function checkPermission(permission: JString; pid: Integer; uid: Integer): Integer; cdecl;
    function checkSelfPermission(permission: JString): Integer; cdecl;
    function checkUriPermission(uri: Jnet_Uri; pid: Integer; uid: Integer; modeFlags: Integer): Integer; cdecl; overload;
    function checkUriPermission(uri: Jnet_Uri; readPermission: JString; writePermission: JString; pid: Integer; uid: Integer;
      modeFlags: Integer): Integer; cdecl; overload;
    procedure clearWallpaper; cdecl;
    function createConfigurationContext(overrideConfiguration: JConfiguration): JContext; cdecl;
    function createDisplayContext(display: JDisplay): JContext; cdecl;
    function createPackageContext(packageName: JString; flags: Integer): JContext; cdecl;
    function databaseList: TJavaObjectArray<JString>; cdecl;
    function deleteDatabase(name: JString): Boolean; cdecl;
    function deleteFile(name: JString): Boolean; cdecl;
    procedure enforceCallingOrSelfPermission(permission: JString; message: JString); cdecl;
    procedure enforceCallingOrSelfUriPermission(uri: Jnet_Uri; modeFlags: Integer; message: JString); cdecl;
    procedure enforceCallingPermission(permission: JString; message: JString); cdecl;
    procedure enforceCallingUriPermission(uri: Jnet_Uri; modeFlags: Integer; message: JString); cdecl;
    procedure enforcePermission(permission: JString; pid: Integer; uid: Integer; message: JString); cdecl;
    procedure enforceUriPermission(uri: Jnet_Uri; pid: Integer; uid: Integer; modeFlags: Integer; message: JString); cdecl; overload;
    procedure enforceUriPermission(uri: Jnet_Uri; readPermission: JString; writePermission: JString; pid: Integer; uid: Integer; modeFlags: Integer;
      message: JString); cdecl; overload;
    function fileList: TJavaObjectArray<JString>; cdecl;
    function getApplicationContext: JContext; cdecl;
    function getApplicationInfo: JApplicationInfo; cdecl;
    function getAssets: JAssetManager; cdecl;
    function getBaseContext: JContext; cdecl;
    function getCacheDir: JFile; cdecl;
    function getClassLoader: JClassLoader; cdecl;
    function getCodeCacheDir: JFile; cdecl;
    function getContentResolver: JContentResolver; cdecl;
    function getDatabasePath(name: JString): JFile; cdecl;
    function getDir(name: JString; mode: Integer): JFile; cdecl;
    function getExternalCacheDir: JFile; cdecl;
    function getExternalCacheDirs: TJavaObjectArray<JFile>; cdecl;
    function getExternalFilesDir(type_: JString): JFile; cdecl;
    function getExternalFilesDirs(type_: JString): TJavaObjectArray<JFile>; cdecl;
    function getExternalMediaDirs: TJavaObjectArray<JFile>; cdecl;
    function getFileStreamPath(name: JString): JFile; cdecl;
    function getFilesDir: JFile; cdecl;
    function getMainLooper: JLooper; cdecl;
    function getNoBackupFilesDir: JFile; cdecl;
    function getObbDir: JFile; cdecl;
    function getObbDirs: TJavaObjectArray<JFile>; cdecl;
    function getPackageCodePath: JString; cdecl;
    function getPackageManager: JPackageManager; cdecl;
    function getPackageName: JString; cdecl;
    function getPackageResourcePath: JString; cdecl;
    function getResources: JResources; cdecl;
    function getSharedPreferences(name: JString; mode: Integer): JSharedPreferences; cdecl;
    function getSystemService(name: JString): JObject; cdecl;
    function getSystemServiceName(serviceClass: Jlang_Class): JString; cdecl;
    function getTheme: JResources_Theme; cdecl;
    function getWallpaper: JDrawable; cdecl;
    function getWallpaperDesiredMinimumHeight: Integer; cdecl;
    function getWallpaperDesiredMinimumWidth: Integer; cdecl;
    procedure grantUriPermission(toPackage: JString; uri: Jnet_Uri; modeFlags: Integer); cdecl;
    function isRestricted: Boolean; cdecl;
    function openFileInput(name: JString): JFileInputStream; cdecl;
    function openFileOutput(name: JString; mode: Integer): JFileOutputStream; cdecl;
    function openOrCreateDatabase(name: JString; mode: Integer; factory: JSQLiteDatabase_CursorFactory): JSQLiteDatabase; cdecl; overload;
    function openOrCreateDatabase(name: JString; mode: Integer; factory: JSQLiteDatabase_CursorFactory;
      errorHandler: JDatabaseErrorHandler): JSQLiteDatabase; cdecl; overload;
    function peekWallpaper: JDrawable; cdecl;
    function registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter): JIntent; cdecl; overload;
    function registerReceiver(receiver: JBroadcastReceiver; filter: JIntentFilter; broadcastPermission: JString;
      scheduler: JHandler): JIntent; cdecl; overload;
    procedure removeStickyBroadcast(intent: JIntent); cdecl;
    procedure removeStickyBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl;
    procedure revokeUriPermission(uri: Jnet_Uri; modeFlags: Integer); cdecl;
    procedure sendBroadcast(intent: JIntent); cdecl; overload;
    procedure sendBroadcast(intent: JIntent; receiverPermission: JString); cdecl; overload;
    procedure sendBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl; overload;
    procedure sendBroadcastAsUser(intent: JIntent; user: JUserHandle; receiverPermission: JString); cdecl; overload;
    procedure sendOrderedBroadcast(intent: JIntent; receiverPermission: JString); cdecl; overload;
    procedure sendOrderedBroadcast(intent: JIntent; receiverPermission: JString; resultReceiver: JBroadcastReceiver; scheduler: JHandler;
      initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl; overload;
    procedure sendOrderedBroadcastAsUser(intent: JIntent; user: JUserHandle; receiverPermission: JString; resultReceiver: JBroadcastReceiver;
      scheduler: JHandler; initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl;
    procedure sendStickyBroadcast(intent: JIntent); cdecl;
    procedure sendStickyBroadcastAsUser(intent: JIntent; user: JUserHandle); cdecl;
    procedure sendStickyOrderedBroadcast(intent: JIntent; resultReceiver: JBroadcastReceiver; scheduler: JHandler; initialCode: Integer;
      initialData: JString; initialExtras: JBundle); cdecl;
    procedure sendStickyOrderedBroadcastAsUser(intent: JIntent; user: JUserHandle; resultReceiver: JBroadcastReceiver; scheduler: JHandler;
      initialCode: Integer; initialData: JString; initialExtras: JBundle); cdecl;
    procedure setTheme(resid: Integer); cdecl;
    procedure setWallpaper(bitmap: JBitmap); cdecl; overload;
    procedure setWallpaper(data: JInputStream); cdecl; overload;
    procedure startActivities(intents: TJavaObjectArray<JIntent>); cdecl; overload;
    procedure startActivities(intents: TJavaObjectArray<JIntent>; options: JBundle); cdecl; overload;
    procedure startActivity(intent: JIntent); cdecl; overload;
    procedure startActivity(intent: JIntent; options: JBundle); cdecl; overload;
    function startForegroundService(service: JIntent): JComponentName; cdecl;
    function startInstrumentation(className: JComponentName; profileFile: JString; arguments: JBundle): Boolean; cdecl;
    procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer;
      extraFlags: Integer); cdecl; overload;
    procedure startIntentSender(intent: JIntentSender; fillInIntent: JIntent; flagsMask: Integer; flagsValues: Integer; extraFlags: Integer;
      options: JBundle); cdecl; overload;
    function startService(service: JIntent): JComponentName; cdecl;
    function stopService(name: JIntent): Boolean; cdecl;
    procedure unbindService(conn: JServiceConnection); cdecl;
    procedure unregisterReceiver(receiver: JBroadcastReceiver); cdecl;
  end;
  TJContextWrapper = class(TJavaGenericImport<JContextWrapperClass, JContextWrapper>) end;

  JPackageManager_ResolveInfoFlagsClass = interface(JObjectClass)
    ['{2CC19E0E-D9BF-4D4C-80B9-56CE237AFF4E}']
    {class} function &of(value: Int64): JPackageManager_ResolveInfoFlags; cdecl;
  end;

  [JavaSignature('android/content/pm/PackageManager$ResolveInfoFlags')]
  JPackageManager_ResolveInfoFlags = interface(JObject)
    ['{38063245-43B2-40BF-98B5-97FA314C9B64}']
    function getValue: Int64; cdecl;
  end;
  TJPackageManager_ResolveInfoFlags = class(TJavaGenericImport<JPackageManager_ResolveInfoFlagsClass, JPackageManager_ResolveInfoFlags>) end;

  [JavaSignature('android/content/pm/PackageManager')]
  JPackageManagerEx = interface(JPackageManager)
    ['{B725AB6E-18B1-4B2D-B31F-494249F77EE3}']
    function queryIntentActivities(intent: JIntent; flags: JPackageManager_ResolveInfoFlags): JList; cdecl;
  end;
  TJPackageManagerEx = class(TJavaGenericImport<JPackageManagerClass, JPackageManagerEx>) end;

implementation

end.
