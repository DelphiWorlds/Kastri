unit DW.Androidapi.JNI.AndroidX.Content;

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
  Androidapi.JNIBridge, Androidapi.Jni.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os;

type
  JContextCompat = interface;
  JLocusIdCompat = interface;
  JShortcutInfoCompat = interface;

  JLocusIdCompatClass = interface(JObjectClass)
    ['{1DF4C941-18DE-4AE2-ACB5-A9347F426677}']
    {class} function init(string_: JString): JLocusIdCompat; cdecl;
  end;

  [JavaSignature('androidx/core/content/LocusIdCompat')]
  JLocusIdCompat = interface(JObject)
    ['{4728A252-E681-4F0B-96AF-960EDC50654F}']
    function equals(object_: JObject): Boolean; cdecl;
    function getId: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJLocusIdCompat = class(TJavaGenericImport<JLocusIdCompatClass, JLocusIdCompat>) end;

  JShortcutInfoCompatClass = interface(JObjectClass)
    ['{38D9871E-9287-4205-9473-D2A84A26C75D}']
  end;

  [JavaSignature('androidx/core/content/pm/ShortcutInfoCompat')]
  JShortcutInfoCompat = interface(JObject)
    ['{0F462C6A-3EEA-4561-AB06-25BB6EB30106}']
    function getActivity: JComponentName; cdecl;
    function getCategories: JSet; cdecl;
    function getDisabledMessage: JCharSequence; cdecl;
    function getDisabledReason: Integer; cdecl;
    function getExtras: JPersistableBundle; cdecl;
    // function getIcon: Jdrawable_IconCompat; cdecl;
    function getId: JString; cdecl;
    function getIntent: JIntent; cdecl;
    function getIntents: TJavaObjectArray<JIntent>; cdecl;
    function getLastChangedTimestamp: Int64; cdecl;
    function getLocusId: JLocusIdCompat; cdecl; overload;
    function getLongLabel: JCharSequence; cdecl;
    function getPackage: JString; cdecl;
    function getRank: Integer; cdecl;
    function getShortLabel: JCharSequence; cdecl;
    function getUserHandle: JUserHandle; cdecl;
    function hasKeyFieldsOnly: Boolean; cdecl;
    function isCached: Boolean; cdecl;
    function isDeclaredInManifest: Boolean; cdecl;
    function isDynamic: Boolean; cdecl;
    function isEnabled: Boolean; cdecl;
    function isImmutable: Boolean; cdecl;
    function isPinned: Boolean; cdecl;
    function toShortcutInfo: JShortcutInfo; cdecl;
  end;
  TShortcutInfoCompat = class(TJavaGenericImport<JShortcutInfoCompatClass, JShortcutInfoCompat>) end;

  JContextCompatClass = interface(JObjectClass)
    ['{22C73BA4-04C6-462F-B66F-8E86A643A905}']
    {class} function _GetRECEIVER_EXPORTED: Integer; cdecl;
    {class} function _GetRECEIVER_NOT_EXPORTED: Integer; cdecl;
    {class} function _GetRECEIVER_VISIBLE_TO_INSTANT_APPS: Integer; cdecl;
    {class} function checkSelfPermission(context: JContext; string_1: JString): Integer; cdecl;
    {class} function createDeviceProtectedStorageContext(context: JContext): JContext; cdecl;
    {class} function getAttributionTag(context: JContext): JString; cdecl;
    {class} function getCodeCacheDir(context: JContext): JFile; cdecl;
    {class} function getColor(context: JContext; int: Integer): Integer; cdecl;
    {class} function getColorStateList(context: JContext; int: Integer): JColorStateList; cdecl;
    {class} function getDataDir(context: JContext): JFile; cdecl;
    {class} function getDrawable(context: JContext; int: Integer): JDrawable; cdecl;
    {class} function getExternalCacheDirs(context: JContext): TJavaObjectArray<JFile>; cdecl;
    {class} function getExternalFilesDirs(context: JContext; string_1: JString): TJavaObjectArray<JFile>; cdecl;
    {class} function getMainExecutor(context: JContext): JExecutor; cdecl;
    {class} function getNoBackupFilesDir(context: JContext): JFile; cdecl;
    {class} function getObbDirs(context: JContext): TJavaObjectArray<JFile>; cdecl;
    {class} function getSystemService(context: JContext; class_1: Jlang_Class): JObject; cdecl;
    {class} function getSystemServiceName(context: JContext; class_1: Jlang_Class): JString; cdecl;
    {class} function isDeviceProtectedStorage(context: JContext): Boolean; cdecl;
    {class} function registerReceiver(context: JContext; broadcastreceiver: JBroadcastReceiver; intentfilter: JIntentFilter; string_1: JString;
      handler: JHandler; int: Integer): JIntent; cdecl; overload;
    {class} function registerReceiver(context: JContext; broadcastreceiver: JBroadcastReceiver;
      intentfilter: JIntentFilter; int: Integer): JIntent; cdecl; overload;
    {class} function startActivities(context: JContext; intents: TJavaObjectArray<JIntent>; bundle: JBundle): Boolean; cdecl; overload;
    {class} function startActivities(context: JContext; intents: TJavaObjectArray<JIntent>): Boolean; cdecl; overload;
    {class} procedure startActivity(context: JContext; intent: JIntent; bundle: JBundle); cdecl;
    {class} procedure startForegroundService(context: JContext; intent: JIntent); cdecl;
    {class} property RECEIVER_EXPORTED: Integer read _GetRECEIVER_EXPORTED;
    {class} property RECEIVER_NOT_EXPORTED: Integer read _GetRECEIVER_NOT_EXPORTED;
    {class} property RECEIVER_VISIBLE_TO_INSTANT_APPS: Integer read _GetRECEIVER_VISIBLE_TO_INSTANT_APPS;
  end;

  [JavaSignature('androidx/core/content/ContextCompat')]
  JContextCompat = interface(JObject)
    ['{CC80935D-4FE7-4903-B95B-1DA276764FC3}']
  end;
  TJContextCompat = class(TJavaGenericImport<JContextCompatClass, JContextCompat>) end;

implementation

end.
