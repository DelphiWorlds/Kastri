unit DW.Androidapi.JNI.App;

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
  AndroidAPI.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNI.Media, Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Util;

type
  JDownloadManager = interface;
  JDownloadManager_Query = interface;
  JDownloadManager_Request = interface;
  JKeyguardManager = interface;
  JKeyguardManager_KeyguardLock = interface;
  JKeyguardManager_KeyguardDismissCallback = interface;
  JKeyguardManager_OnKeyguardExitResult = interface;
  {$IF CompilerVersion < 33}
  JLocalTime = interface;
  JNotificationChannel = interface;
  JNotificationChannelGroup = interface;
  {$ENDIF}
  JUiModeManager = interface;
  JWallpaperColors = interface;
  JWallpaperInfo = interface;
  JWallpaperManager = interface;
  JWallpaperManager_OnColorsChangedListener = interface;

  // Placeholder import to support compiling with Delphi 10.2.x
  {$IF CompilerVersion < 33}
  JLocalTimeClass = interface(JObjectClass)
    ['{4540274D-F993-4054-A204-B71C90237913}']
  end;

  [JavaSignature('java/time/LocalTime')]
  JLocalTime = interface(JObject)
    ['{B73384F1-C48B-4785-AAE6-C5D0FA12E104}']
  end;
  TJLocalTime = class(TJavaGenericImport<JLocalTimeClass, JLocalTime>) end;
  {$ENDIF}

  JKeyguardManagerClass = interface(JObjectClass)
    ['{44F9A483-0683-4391-94D1-F0BB6DB7340B}']
  end;

  [JavaSignature('android/app/KeyguardManager')]
  JKeyguardManager = interface(JObject)
    ['{6AECC681-2C45-48B7-87D9-5A255840BDFA}']
    function createConfirmDeviceCredentialIntent(title: JCharSequence; description: JCharSequence): JIntent; cdecl;
    procedure exitKeyguardSecurely(callback: JKeyguardManager_OnKeyguardExitResult); cdecl;
    function inKeyguardRestrictedInputMode: Boolean; cdecl;
    function isDeviceLocked: Boolean; cdecl;
    function isDeviceSecure: Boolean; cdecl;
    function isKeyguardLocked: Boolean; cdecl;
    function isKeyguardSecure: Boolean; cdecl;
    function newKeyguardLock(tag: JString): JKeyguardManager_KeyguardLock; cdecl;
    procedure requestDismissKeyguard(activity: JActivity; callback: JKeyguardManager_KeyguardDismissCallback); cdecl;
  end;
  TJKeyguardManager = class(TJavaGenericImport<JKeyguardManagerClass, JKeyguardManager>) end;

  JKeyguardManager_KeyguardDismissCallbackClass = interface(JObjectClass)
    ['{05ED1989-6EBC-4F43-9333-D1D14101475C}']
  end;

  [JavaSignature('android/app/KeyguardManager$KeyguardDismissCallback')]
  JKeyguardManager_KeyguardDismissCallback = interface(JObject)
    ['{5B237B1E-7B6A-4B0E-B2F3-74078B6298D9}']
    procedure onDismissCancelled; cdecl;
    procedure onDismissError; cdecl;
    procedure onDismissSucceeded; cdecl;
  end;

  JKeyguardManager_KeyguardLockClass = interface(JObjectClass)
    ['{98306533-81FC-4498-91E6-19F58EA32BD7}']
    {class} procedure disableKeyguard; cdecl;
    {class} procedure reenableKeyguard; cdecl;
  end;

  [JavaSignature('android/app/KeyguardManager$KeyguardLock')]
  JKeyguardManager_KeyguardLock = interface(JObject)
    ['{6061FAD3-A869-40A5-BEAB-CFC481B8EC0B}']
  end;
  TJKeyguardManager_KeyguardLock = class(TJavaGenericImport<JKeyguardManager_KeyguardLockClass, JKeyguardManager_KeyguardLock>) end;

  JKeyguardManager_OnKeyguardExitResultClass = interface(IJavaClass)
    ['{40CF0367-E275-475B-8566-A6337DD56217}']
  end;

  [JavaSignature('android/app/KeyguardManager$OnKeyguardExitResult')]
  JKeyguardManager_OnKeyguardExitResult = interface(IJavaInstance)
    ['{A2E86C9B-0384-4A3A-8E4B-823923FBD083}']
    procedure onKeyguardExitResult(success: Boolean); cdecl;
  end;
  TJKeyguardManager_OnKeyguardExitResult = class(TJavaGenericImport<JKeyguardManager_OnKeyguardExitResultClass, JKeyguardManager_OnKeyguardExitResult>) end;

  {$IF CompilerVersion < 33}
  JNotificationChannelClass = interface(JObjectClass)
    ['{2C435911-AD3A-4461-8860-779ECA70D332}']
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function _GetDEFAULT_CHANNEL_ID: JString; cdecl;
    function init(id: JString; name: JCharSequence; importance: Integer): JNotificationChannel; cdecl;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
    property DEFAULT_CHANNEL_ID: JString read _GetDEFAULT_CHANNEL_ID;
  end;

  [JavaSignature('android/app/NotificationChannel')]
  JNotificationChannel = interface(JObject)
    ['{3AA9585E-BF01-40A2-933E-73DFEB241BB9}']
    function canBypassDnd: boolean; cdecl;
    function canShowBadge: boolean; cdecl;
    function describeContents: Integer; cdecl;
    procedure enableLights(lights: boolean); cdecl;
    procedure enableVibration(vibration: boolean); cdecl;
    function equals(o: JObject): boolean; cdecl;
    function getAudioAttributes: JAudioAttributes; cdecl;
    function getDescription: JString; cdecl;
    function getGroup: JString; cdecl;
    function getId: JString; cdecl;
    function getImportance: Integer; cdecl;
    function getLightColor: Integer; cdecl;
    function getLockscreenVisibility: Integer; cdecl;
    function getName: JCharSequence; cdecl;
    function getSound: Jnet_Uri; cdecl;
    function getVibrationPattern: TJavaArray<Int64>; cdecl;
    function hashCode: Integer; cdecl;
    procedure setBypassDnd(bypassDnd: boolean); cdecl;
    procedure setDescription(description: JString); cdecl;
    procedure setGroup(groupId: JString); cdecl;
    procedure setImportance(importance: Integer); cdecl;
    procedure setLightColor(argb: Integer); cdecl;
    procedure setLockscreenVisibility(lockscreenVisibility: Integer); cdecl;
    procedure setName(&name: JCharSequence); cdecl;
    procedure setShowBadge(showBadge: boolean); cdecl;
    procedure setSound(sound: Jnet_Uri; audioAttributes: JAudioAttributes); cdecl;
    procedure setVibrationPattern(vibrationPattern: TJavaArray<Int64>); cdecl;
    function shouldShowLights: boolean; cdecl;
    function shouldVibrate: boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJNotificationChannel = class(TJavaGenericImport<JNotificationChannelClass, JNotificationChannel>)
  end;

  JNotificationChannelGroupClass = interface(JObjectClass)
    ['{E62DA0EE-18A8-439E-BE8D-A9D0A89A2EF9}']
    function _GetCREATOR : JParcelable_Creator; cdecl;
    function init(id : JString; &name : JCharSequence) : JNotificationChannelGroup; cdecl;
    property CREATOR : JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/NotificationChannelGroup')]
  JNotificationChannelGroup = interface(JObject)
    ['{CA4F183A-9CD3-4261-805A-A0CCBC9A5389}']
    function clone: JNotificationChannelGroup; cdecl;
    function describeContents: Integer; cdecl;
    function equals(o: JObject): boolean; cdecl;
    function getChannels: JList; cdecl;
    function getDescription: JString; cdecl;
    function getId: JString; cdecl;
    function getName: JCharSequence; cdecl;
    function hashCode: Integer; cdecl;
    function isBlocked: Boolean; cdecl;
    procedure setDescription(description: JString); cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJNotificationChannelGroup = class(TJavaGenericImport<JNotificationChannelGroupClass, JNotificationChannelGroup>)
  end;

  JNotificationManagerClass = interface(JObjectClass)
    ['{66101C50-DAE9-4C81-8186-81A0A43A73BD}']
    {class} function _GetACTION_INTERRUPTION_FILTER_CHANGED: JString; cdecl;
    {class} function _GetACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED: JString; cdecl;
    {class} function _GetACTION_NOTIFICATION_POLICY_CHANGED: JString; cdecl;
    {class} function _GetIMPORTANCE_DEFAULT: Integer; cdecl;
    {class} function _GetIMPORTANCE_HIGH: Integer; cdecl;
    {class} function _GetIMPORTANCE_LOW: Integer; cdecl;
    {class} function _GetIMPORTANCE_MAX: Integer; cdecl;
    {class} function _GetIMPORTANCE_MIN: Integer; cdecl;
    {class} function _GetIMPORTANCE_NONE: Integer; cdecl;
    {class} function _GetIMPORTANCE_UNSPECIFIED: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_ALARMS: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_ALL: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_NONE: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_PRIORITY: Integer; cdecl;
    {class} function _GetINTERRUPTION_FILTER_UNKNOWN: Integer; cdecl;
    {class} property ACTION_INTERRUPTION_FILTER_CHANGED: JString read _GetACTION_INTERRUPTION_FILTER_CHANGED;
    {class} property ACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED: JString read _GetACTION_NOTIFICATION_POLICY_ACCESS_GRANTED_CHANGED;
    {class} property ACTION_NOTIFICATION_POLICY_CHANGED: JString read _GetACTION_NOTIFICATION_POLICY_CHANGED;
    {class} property IMPORTANCE_DEFAULT: Integer read _GetIMPORTANCE_DEFAULT;
    {class} property IMPORTANCE_HIGH: Integer read _GetIMPORTANCE_HIGH;
    {class} property IMPORTANCE_LOW: Integer read _GetIMPORTANCE_LOW;
    {class} property IMPORTANCE_MAX: Integer read _GetIMPORTANCE_MAX;
    {class} property IMPORTANCE_MIN: Integer read _GetIMPORTANCE_MIN;
    {class} property IMPORTANCE_NONE: Integer read _GetIMPORTANCE_NONE;
    {class} property IMPORTANCE_UNSPECIFIED: Integer read _GetIMPORTANCE_UNSPECIFIED;
    {class} property INTERRUPTION_FILTER_ALARMS: Integer read _GetINTERRUPTION_FILTER_ALARMS;
    {class} property INTERRUPTION_FILTER_ALL: Integer read _GetINTERRUPTION_FILTER_ALL;
    {class} property INTERRUPTION_FILTER_NONE: Integer read _GetINTERRUPTION_FILTER_NONE;
    {class} property INTERRUPTION_FILTER_PRIORITY: Integer read _GetINTERRUPTION_FILTER_PRIORITY;
    {class} property INTERRUPTION_FILTER_UNKNOWN: Integer read _GetINTERRUPTION_FILTER_UNKNOWN;
  end;

  [JavaSignature('android/app/NotificationManager')]
  JNotificationManager = interface(Androidapi.JNI.App.JNotificationManager)
    ['{F2C96815-29C4-4A83-994A-4F49F30B8CF4}']
    procedure createNotificationChannel(channel: JNotificationChannel); cdecl;
    procedure createNotificationChannelGroup(group: JNotificationChannelGroup); cdecl;
    procedure createNotificationChannelGroups(groups: JList); cdecl;
    procedure createNotificationChannels(channels: JList); cdecl;
    procedure deleteNotificationChannel(channelId: JString); cdecl;
    procedure deleteNotificationChannelGroup(groupId: JString); cdecl;
  end;
  TJNotificationManager = class(TJavaGenericImport<JNotificationManagerClass, JNotificationManager>)
  end;
  {$ENDIF}

  JUiModeManagerClass = interface(JObjectClass)
    ['{68E79491-08CD-4AEB-9540-D0642FA1BDB3}']
    function _GetACTION_ENTER_CAR_MODE: JString; cdecl;
    function _GetACTION_ENTER_DESK_MODE: JString; cdecl;
    function _GetACTION_EXIT_CAR_MODE: JString; cdecl;
    function _GetACTION_EXIT_DESK_MODE: JString; cdecl;
    function _GetDISABLE_CAR_MODE_GO_HOME: Integer; cdecl;
    function _GetENABLE_CAR_MODE_ALLOW_SLEEP: Integer; cdecl;
    function _GetENABLE_CAR_MODE_GO_CAR_HOME: Integer; cdecl;
    function _GetMODE_NIGHT_AUTO: Integer; cdecl;
    function _GetMODE_NIGHT_CUSTOM: Integer; cdecl;
    function _GetMODE_NIGHT_NO: Integer; cdecl;
    function _GetMODE_NIGHT_YES: Integer; cdecl;
    property ACTION_ENTER_CAR_MODE: JString read _GetACTION_ENTER_CAR_MODE;
    property ACTION_ENTER_DESK_MODE: JString read _GetACTION_ENTER_DESK_MODE;
    property ACTION_EXIT_CAR_MODE: JString read _GetACTION_EXIT_CAR_MODE;
    property ACTION_EXIT_DESK_MODE: JString read _GetACTION_EXIT_DESK_MODE;
    property DISABLE_CAR_MODE_GO_HOME: Integer read _GetDISABLE_CAR_MODE_GO_HOME;
    property ENABLE_CAR_MODE_ALLOW_SLEEP: Integer read _GetENABLE_CAR_MODE_ALLOW_SLEEP;
    property ENABLE_CAR_MODE_GO_CAR_HOME: Integer read _GetENABLE_CAR_MODE_GO_CAR_HOME;
    property MODE_NIGHT_AUTO: Integer read _GetMODE_NIGHT_AUTO;
    property MODE_NIGHT_CUSTOM: Integer read _GetMODE_NIGHT_CUSTOM;
    property MODE_NIGHT_NO: Integer read _GetMODE_NIGHT_NO;
    property MODE_NIGHT_YES: Integer read _GetMODE_NIGHT_YES;
  end;

  [JavaSignature('android/app/UiModeManager')]
  JUiModeManager = interface(JObject)
    ['{8BFF307B-DFE7-499C-95BD-4A3FE549BFDE}']
    function getCurrentModeType: Integer; cdecl;
    function getCustomNightModeEnd: JLocalTime; cdecl;
    function getCustomNightModeStart: JLocalTime; cdecl;
    function getNightMode: Integer; cdecl;
    procedure disableCarMode(flags: Integer); cdecl;
    procedure enableCarMode(flags: Integer); cdecl;
    procedure setCustomNightModeEnd(time: JLocalTime); cdecl;
    procedure setCustomNightModeStart(time: JLocalTime); cdecl;
    procedure setNightMode(mode: Integer); cdecl;
  end;
  TJUiModeManager = class(TJavaGenericImport<JUiModeManagerClass, JUiModeManager>)
  end;

  JWallpaperColorsClass = interface(JObjectClass)
    ['{D33DAC27-33AE-4047-9C4E-136711E0630F}']
    function fromBitmap(bitmap: JBitmap): JWallpaperColors; cdecl;
    function fromDrawable(drawable: JDrawable): JWallpaperColors; cdecl;
    function init(parcel: JParcel): JWallpaperColors; cdecl; overload;
    function init(primaryColor: JColor; secondaryColor: JColor; tertiaryColor: JColor): JWallpaperColors; cdecl; overload;
    function init(primaryColor: JColor; secondaryColor: JColor; tertiaryColor: JColor; colorHints: Integer): JWallpaperColors; cdecl; overload;
  end;

  [JavaSignature('android/app/WallpaperColors')]
  JWallpaperColors = interface(JObject)
    ['{0A1C6300-9392-42EE-B4E6-D228A663E601}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): boolean; cdecl;
    function getColorHints: Integer; cdecl;
    function getPrimaryColor: JColor; cdecl;
    function getSecondaryColor: JColor; cdecl;
    function getTertiaryColor: JColor; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJWallpaperColors = class(TJavaGenericImport<JWallpaperColorsClass, JWallpaperColors>)
  end;

  JWallpaperInfoClass = interface(JObjectClass)
    ['{38C90129-35B2-414F-87A5-D7F7D2B23D9E}']
    function init(context: JContext; service: JResolveInfo): JWallpaperInfo; cdecl;
  end;

  [JavaSignature('android/app/WallpaperInfo')]
  JWallpaperInfo = interface(JObject)
    ['{C9FA4FEA-1B76-4CBF-859D-C6FA37C9BD01}']
    function describeContents: Integer; cdecl;
    procedure dump(pw: JPrinter; prefix: JString); cdecl;
    function getComponent: JComponentName; cdecl;
    function getPackageName: JString; cdecl;
    function getServiceInfo: JServiceInfo; cdecl;
    function getServiceName: JString; cdecl;
    function getSettingsActivity: JString; cdecl;
    function getSettingsSliceUri: Jnet_Uri; cdecl;
    function getShowMetadataInPreview: boolean; cdecl;
    function loadAuthor(pm: JPackageManager): JCharSequence; cdecl;
    function loadContextDescription(pm: JPackageManager): JCharSequence; cdecl;
    function loadContextUri(pm: JPackageManager): Jnet_Uri; cdecl;
    function loadDescription(pm: JPackageManager): JCharSequence; cdecl;
    function loadIcon(pm: JPackageManager): JDrawable; cdecl;
    function loadLabel(pm: JPackageManager): JCharSequence; cdecl;
    function loadThumbnail(pm: JPackageManager): JDrawable; cdecl;
    function shouldUseDefaultUnfoldTransition: Boolean; cdecl;
    function supportsMultipleDisplays: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJWallpaperInfo = class(TJavaGenericImport<JWallpaperInfoClass, JWallpaperInfo>)
  end;

  JWallpaperManagerClass = interface(JObjectClass)
    ['{C8F0FD68-3263-4620-B1E9-2EA40CE71754}']
    function _GetACTION_CHANGE_LIVE_WALLPAPER: JString; cdecl;
    function _GetACTION_CROP_AND_SET_WALLPAPER: JString; cdecl;
    function _GetACTION_LIVE_WALLPAPER_CHOOSER: JString; cdecl;
    function _GetCOMMAND_DROP: JString; cdecl;
    function _GetCOMMAND_SECONDARY_TAP: JString; cdecl;
    function _GetCOMMAND_TAP: JString; cdecl;
    function _GetEXTRA_LIVE_WALLPAPER_COMPONENT: JString; cdecl;
    function _GetFLAG_LOCK: Integer; cdecl;
    function _GetFLAG_SYSTEM: Integer; cdecl;
    function _GetWALLPAPER_PREVIEW_META_DATA: JString; cdecl;
    function getInstance(context: JContext): JWallpaperManager; cdecl;
    property ACTION_CHANGE_LIVE_WALLPAPER: JString read _GetACTION_CHANGE_LIVE_WALLPAPER;
    property ACTION_CROP_AND_SET_WALLPAPER: JString read _GetACTION_CROP_AND_SET_WALLPAPER;
    property ACTION_LIVE_WALLPAPER_CHOOSER: JString read _GetACTION_LIVE_WALLPAPER_CHOOSER;
    property COMMAND_DROP: JString read _GetCOMMAND_DROP;
    property COMMAND_SECONDARY_TAP: JString read _GetCOMMAND_SECONDARY_TAP;
    property COMMAND_TAP: JString read _GetCOMMAND_TAP;
    property EXTRA_LIVE_WALLPAPER_COMPONENT: JString read _GetEXTRA_LIVE_WALLPAPER_COMPONENT;
    property FLAG_LOCK: Integer read _GetFLAG_LOCK;
    property FLAG_SYSTEM: Integer read _GetFLAG_SYSTEM;
    property WALLPAPER_PREVIEW_META_DATA: JString read _GetWALLPAPER_PREVIEW_META_DATA;
  end;

  [JavaSignature('android/app/WallpaperManager')]
  JWallpaperManager = interface(JObject)
    ['{70425A01-A43B-4499-B0D5-DF80C34E3F7A}']
    function getBuiltInDrawable: JDrawable; cdecl; overload;
    function getBuiltInDrawable(outWidth: Integer; outHeight: Integer; scaleToFit: boolean; horizontalAlignment: Single; verticalAlignment: Single): JDrawable; cdecl; overload;
    function getBuiltInDrawable(outWidth: Integer; outHeight: Integer; scaleToFit: boolean; horizontalAlignment: Single; verticalAlignment: Single; which: Integer): JDrawable; cdecl; overload;
    function getBuiltInDrawable(which: Integer): JDrawable; cdecl; overload;
    function getCropAndSetWallpaperIntent(imageUri: Jnet_Uri): JIntent; cdecl;
    function getDesiredMinimumHeight: Integer; cdecl;
    function getDesiredMinimumWidth: Integer; cdecl;
    function getDrawable: JDrawable; cdecl;
    function getFastDrawable: JDrawable; cdecl;
    function getWallpaperColors(which: Integer): JWallpaperColors; cdecl;
    function getWallpaperFile(which: Integer): JParcelFileDescriptor; cdecl;
    function getWallpaperId(which: Integer): Integer; cdecl;
    function getWallpaperInfo: JWallpaperInfo; cdecl;
    function hasResourceWallpaper(resid: Integer): boolean; cdecl;
    function isSetWallpaperAllowed: boolean; cdecl;
    function isWallpaperSupported: boolean; cdecl;
    function peekDrawable: JDrawable; cdecl;
    function peekFastDrawable: JDrawable; cdecl;
    function setBitmap(fullImage: JBitmap; visibleCropHint: JRect; allowBackup: boolean): Integer; cdecl; overload;
    function setBitmap(fullImage: JBitmap; visibleCropHint: JRect; allowBackup: boolean; which: Integer): Integer; cdecl; overload;
    function setResource(resid: Integer; which: Integer): Integer; cdecl; overload;
    function setStream(bitmapData: JInputStream; visibleCropHint: JRect; allowBackup: boolean): Integer; cdecl; overload;
    function setStream(bitmapData: JInputStream; visibleCropHint: JRect; allowBackup: boolean; which: Integer): Integer; cdecl; overload;
    procedure addOnColorsChangedListener(listener: JWallpaperManager_OnColorsChangedListener; handler: JHandler); cdecl;
    procedure clear; cdecl; overload;
    procedure clear(which: Integer); cdecl; overload;
    procedure clearWallpaper; cdecl;
    procedure clearWallpaperOffsets(windowToken: JIBinder); cdecl;
    procedure forgetLoadedWallpaper; cdecl;
    procedure removeOnColorsChangedListener(callback: JWallpaperManager_OnColorsChangedListener); cdecl;
    procedure sendWallpaperCommand(windowToken: JIBinder; action: JString; x: Integer; y: Integer; z: Integer; extras: JBundle); cdecl;
    procedure setBitmap(bitmap: JBitmap); cdecl; overload;
    procedure setDisplayPadding(padding: JRect); cdecl;
    procedure setResource(resid: Integer); cdecl; overload;
    procedure setStream(bitmapData: JInputStream); cdecl; overload;
    procedure setWallpaperOffsetSteps(xStep: Single; yStep: Single); cdecl;
    procedure setWallpaperOffsets(windowToken: JIBinder; xOffset: Single; yOffset: Single); cdecl;
    procedure suggestDesiredDimensions(minimumWidth: Integer; minimumHeight: Integer); cdecl;
  end;
  TJWallpaperManager = class(TJavaGenericImport<JWallpaperManagerClass, JWallpaperManager>)
  end;

  JWallpaperManager_OnColorsChangedListenerClass = interface(IJavaClass)
    ['{4F240A03-576C-4F60-895B-8775E817E86B}']
  end;

  [JavaSignature('android/app/WallpaperManager_OnColorsChangedListener')]
  JWallpaperManager_OnColorsChangedListener = interface(IJavaInstance)
    ['{EC8760FB-1DDD-4ADC-80A5-FDE4CF3E8792}']
    procedure onColorsChanged(colors: JWallpaperColors; which: Integer); cdecl;
  end;
  TJWallpaperManager_OnColorsChangedListener = class(TJavaGenericImport<JWallpaperManager_OnColorsChangedListenerClass,
    JWallpaperManager_OnColorsChangedListener>)
  end;

  JDownloadManager_RequestClass = interface(JObjectClass)
    ['{BBBEFE89-63E2-44A1-A266-FADC4AD023B0}']
    {class} function _GetNETWORK_MOBILE: Integer; cdecl;
    {class} function _GetNETWORK_WIFI: Integer; cdecl;
    {class} function _GetVISIBILITY_HIDDEN: Integer; cdecl;
    {class} function _GetVISIBILITY_VISIBLE: Integer; cdecl;
    {class} function _GetVISIBILITY_VISIBLE_NOTIFY_COMPLETED: Integer; cdecl;
    {class} function _GetVISIBILITY_VISIBLE_NOTIFY_ONLY_COMPLETION: Integer; cdecl;
    {class} function init(uri: Jnet_Uri): JDownloadManager_Request; cdecl;
    {class} property NETWORK_MOBILE: Integer read _GetNETWORK_MOBILE;
    {class} property NETWORK_WIFI: Integer read _GetNETWORK_WIFI;
    {class} property VISIBILITY_HIDDEN: Integer read _GetVISIBILITY_HIDDEN;
    {class} property VISIBILITY_VISIBLE: Integer read _GetVISIBILITY_VISIBLE;
    {class} property VISIBILITY_VISIBLE_NOTIFY_COMPLETED: Integer read _GetVISIBILITY_VISIBLE_NOTIFY_COMPLETED;
    {class} property VISIBILITY_VISIBLE_NOTIFY_ONLY_COMPLETION: Integer read _GetVISIBILITY_VISIBLE_NOTIFY_ONLY_COMPLETION;
  end;

  [JavaSignature('android/app/DownloadManager$Request')]
  JDownloadManager_Request = interface(JObject)
    ['{18F28F88-F356-4C44-AC95-B83B1BF82E32}']
    function addRequestHeader(string_1: JString; string_2: JString): JDownloadManager_Request; cdecl;
    procedure allowScanningByMediaScanner; cdecl;
    function setAllowedNetworkTypes(int: Integer): JDownloadManager_Request; cdecl;
    function setAllowedOverMetered(boolean: Boolean): JDownloadManager_Request; cdecl;
    function setAllowedOverRoaming(boolean: Boolean): JDownloadManager_Request; cdecl;
    function setDescription(charsequence: JCharSequence): JDownloadManager_Request; cdecl;
    function setDestinationInExternalFilesDir(context: JContext; string_1: JString; string_2: JString): JDownloadManager_Request; cdecl;
    function setDestinationInExternalPublicDir(string_1: JString; string_2: JString): JDownloadManager_Request; cdecl;
    function setDestinationUri(uri: Jnet_Uri): JDownloadManager_Request; cdecl;
    function setMimeType(string_1: JString): JDownloadManager_Request; cdecl;
    function setNotificationVisibility(int: Integer): JDownloadManager_Request; cdecl;
    function setRequiresCharging(boolean: Boolean): JDownloadManager_Request; cdecl;
    function setRequiresDeviceIdle(boolean: Boolean): JDownloadManager_Request; cdecl;
    function setShowRunningNotification(boolean: Boolean): JDownloadManager_Request; cdecl;
    function setTitle(charsequence: JCharSequence): JDownloadManager_Request; cdecl;
    function setVisibleInDownloadsUi(boolean: Boolean): JDownloadManager_Request; cdecl;
  end;
  TJDownloadManager_Request = class(TJavaGenericImport<JDownloadManager_RequestClass, JDownloadManager_Request>) end;

  JDownloadManagerClass = interface(JObjectClass)
    ['{3EC21155-52FF-473A-85C0-420C95EB374C}']
    {class} function _GetACTION_DOWNLOAD_COMPLETE: JString; cdecl;
    {class} function _GetACTION_NOTIFICATION_CLICKED: JString; cdecl;
    {class} function _GetACTION_VIEW_DOWNLOADS: JString; cdecl;
    {class} function _GetCOLUMN_BYTES_DOWNLOADED_SO_FAR: JString; cdecl;
    {class} function _GetCOLUMN_DESCRIPTION: JString; cdecl;
    {class} function _GetCOLUMN_ID: JString; cdecl;
    {class} function _GetCOLUMN_LAST_MODIFIED_TIMESTAMP: JString; cdecl;
    {class} function _GetCOLUMN_LOCAL_FILENAME: JString; cdecl;
    {class} function _GetCOLUMN_LOCAL_URI: JString; cdecl;
    {class} function _GetCOLUMN_MEDIAPROVIDER_URI: JString; cdecl;
    {class} function _GetCOLUMN_MEDIA_TYPE: JString; cdecl;
    {class} function _GetCOLUMN_REASON: JString; cdecl;
    {class} function _GetCOLUMN_STATUS: JString; cdecl;
    {class} function _GetCOLUMN_TITLE: JString; cdecl;
    {class} function _GetCOLUMN_TOTAL_SIZE_BYTES: JString; cdecl;
    {class} function _GetCOLUMN_URI: JString; cdecl;
    {class} function _GetERROR_CANNOT_RESUME: Integer; cdecl;
    {class} function _GetERROR_DEVICE_NOT_FOUND: Integer; cdecl;
    {class} function _GetERROR_FILE_ALREADY_EXISTS: Integer; cdecl;
    {class} function _GetERROR_FILE_ERROR: Integer; cdecl;
    {class} function _GetERROR_HTTP_DATA_ERROR: Integer; cdecl;
    {class} function _GetERROR_INSUFFICIENT_SPACE: Integer; cdecl;
    {class} function _GetERROR_TOO_MANY_REDIRECTS: Integer; cdecl;
    {class} function _GetERROR_UNHANDLED_HTTP_CODE: Integer; cdecl;
    {class} function _GetERROR_UNKNOWN: Integer; cdecl;
    {class} function _GetEXTRA_DOWNLOAD_ID: JString; cdecl;
    {class} function _GetEXTRA_NOTIFICATION_CLICK_DOWNLOAD_IDS: JString; cdecl;
    {class} function _GetINTENT_EXTRAS_SORT_BY_SIZE: JString; cdecl;
    {class} function _GetPAUSED_QUEUED_FOR_WIFI: Integer; cdecl;
    {class} function _GetPAUSED_UNKNOWN: Integer; cdecl;
    {class} function _GetPAUSED_WAITING_FOR_NETWORK: Integer; cdecl;
    {class} function _GetPAUSED_WAITING_TO_RETRY: Integer; cdecl;
    {class} function _GetSTATUS_FAILED: Integer; cdecl;
    {class} function _GetSTATUS_PAUSED: Integer; cdecl;
    {class} function _GetSTATUS_PENDING: Integer; cdecl;
    {class} function _GetSTATUS_RUNNING: Integer; cdecl;
    {class} function _GetSTATUS_SUCCESSFUL: Integer; cdecl;
    {class} function getMaxBytesOverMobile(context: JContext): JLong; cdecl;
    {class} function getRecommendedMaxBytesOverMobile(context: JContext): JLong; cdecl;
    {class} property ACTION_DOWNLOAD_COMPLETE: JString read _GetACTION_DOWNLOAD_COMPLETE;
    {class} property ACTION_NOTIFICATION_CLICKED: JString read _GetACTION_NOTIFICATION_CLICKED;
    {class} property ACTION_VIEW_DOWNLOADS: JString read _GetACTION_VIEW_DOWNLOADS;
    {class} property COLUMN_BYTES_DOWNLOADED_SO_FAR: JString read _GetCOLUMN_BYTES_DOWNLOADED_SO_FAR;
    {class} property COLUMN_DESCRIPTION: JString read _GetCOLUMN_DESCRIPTION;
    {class} property COLUMN_ID: JString read _GetCOLUMN_ID;
    {class} property COLUMN_LAST_MODIFIED_TIMESTAMP: JString read _GetCOLUMN_LAST_MODIFIED_TIMESTAMP;
    {class} property COLUMN_LOCAL_FILENAME: JString read _GetCOLUMN_LOCAL_FILENAME;
    {class} property COLUMN_LOCAL_URI: JString read _GetCOLUMN_LOCAL_URI;
    {class} property COLUMN_MEDIAPROVIDER_URI: JString read _GetCOLUMN_MEDIAPROVIDER_URI;
    {class} property COLUMN_MEDIA_TYPE: JString read _GetCOLUMN_MEDIA_TYPE;
    {class} property COLUMN_REASON: JString read _GetCOLUMN_REASON;
    {class} property COLUMN_STATUS: JString read _GetCOLUMN_STATUS;
    {class} property COLUMN_TITLE: JString read _GetCOLUMN_TITLE;
    {class} property COLUMN_TOTAL_SIZE_BYTES: JString read _GetCOLUMN_TOTAL_SIZE_BYTES;
    {class} property COLUMN_URI: JString read _GetCOLUMN_URI;
    {class} property ERROR_CANNOT_RESUME: Integer read _GetERROR_CANNOT_RESUME;
    {class} property ERROR_DEVICE_NOT_FOUND: Integer read _GetERROR_DEVICE_NOT_FOUND;
    {class} property ERROR_FILE_ALREADY_EXISTS: Integer read _GetERROR_FILE_ALREADY_EXISTS;
    {class} property ERROR_FILE_ERROR: Integer read _GetERROR_FILE_ERROR;
    {class} property ERROR_HTTP_DATA_ERROR: Integer read _GetERROR_HTTP_DATA_ERROR;
    {class} property ERROR_INSUFFICIENT_SPACE: Integer read _GetERROR_INSUFFICIENT_SPACE;
    {class} property ERROR_TOO_MANY_REDIRECTS: Integer read _GetERROR_TOO_MANY_REDIRECTS;
    {class} property ERROR_UNHANDLED_HTTP_CODE: Integer read _GetERROR_UNHANDLED_HTTP_CODE;
    {class} property ERROR_UNKNOWN: Integer read _GetERROR_UNKNOWN;
    {class} property EXTRA_DOWNLOAD_ID: JString read _GetEXTRA_DOWNLOAD_ID;
    {class} property EXTRA_NOTIFICATION_CLICK_DOWNLOAD_IDS: JString read _GetEXTRA_NOTIFICATION_CLICK_DOWNLOAD_IDS;
    {class} property INTENT_EXTRAS_SORT_BY_SIZE: JString read _GetINTENT_EXTRAS_SORT_BY_SIZE;
    {class} property PAUSED_QUEUED_FOR_WIFI: Integer read _GetPAUSED_QUEUED_FOR_WIFI;
    {class} property PAUSED_UNKNOWN: Integer read _GetPAUSED_UNKNOWN;
    {class} property PAUSED_WAITING_FOR_NETWORK: Integer read _GetPAUSED_WAITING_FOR_NETWORK;
    {class} property PAUSED_WAITING_TO_RETRY: Integer read _GetPAUSED_WAITING_TO_RETRY;
    {class} property STATUS_FAILED: Integer read _GetSTATUS_FAILED;
    {class} property STATUS_PAUSED: Integer read _GetSTATUS_PAUSED;
    {class} property STATUS_PENDING: Integer read _GetSTATUS_PENDING;
    {class} property STATUS_RUNNING: Integer read _GetSTATUS_RUNNING;
    {class} property STATUS_SUCCESSFUL: Integer read _GetSTATUS_SUCCESSFUL;
  end;

  [JavaSignature('android/app/DownloadManager')]
  JDownloadManager = interface(JObject)
    ['{74D647AC-CA0B-49DE-80E5-45BD9C324ABD}']
    function addCompletedDownload(string_1: JString; string_2: JString; boolean: Boolean; string_3: JString; string_4: JString; long: Int64;
      boolean_1: Boolean): Int64; cdecl; overload;
    function addCompletedDownload(string_1: JString; string_2: JString; boolean: Boolean; string_3: JString; string_4: JString; long: Int64;
      boolean_1: Boolean; uri: Jnet_Uri; uri_1: Jnet_Uri): Int64; cdecl; overload;
    function enqueue(request: JDownloadManager_Request): Int64; cdecl;
    function getMimeTypeForDownloadedFile(long: Int64): JString; cdecl;
    function getUriForDownloadedFile(long: Int64): Jnet_Uri; cdecl;
    function openDownloadedFile(long: Int64): JParcelFileDescriptor; cdecl;
    function query(query: JDownloadManager_Query): JCursor; cdecl;
    function remove(long: Int64): Integer; cdecl;
  end;
  TJDownloadManager = class(TJavaGenericImport<JDownloadManagerClass, JDownloadManager>) end;

  JDownloadManager_QueryClass = interface(JObjectClass)
    ['{FA8E1375-4BAB-4A3D-B20C-47657182189D}']
    {class} function init: JDownloadManager_Query; cdecl;
  end;

  [JavaSignature('android/app/DownloadManager$Query')]
  JDownloadManager_Query = interface(JObject)
    ['{BD4605FD-02DC-4DCC-8E40-E9150F0EC349}']
    function setFilterById(ids: TJavaArray<Int64>): JDownloadManager_Query; cdecl;
    function setFilterByStatus(int: Integer): JDownloadManager_Query; cdecl;
  end;
  TJDownloadManager_Query = class(TJavaGenericImport<JDownloadManager_QueryClass, JDownloadManager_Query>) end;

implementation

end.
