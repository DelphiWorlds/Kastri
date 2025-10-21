unit DW.Androidapi.JNI.App;

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
  AndroidAPI.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNI.Media, Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Util, Androidapi.JNI.Telephony, Androidapi.JNI.Java.Security,
  // DW
  DW.Androidapi.JNI.Security;

type
  JActivityOptions = interface;
  JDevicePolicyManager = interface;
  JDevicePolicyManager_InstallSystemUpdateCallback = interface;
  JDevicePolicyManager_OnClearApplicationUserDataListener = interface;
  JDevicePolicyResourcesManager = interface;
  JDownloadManager = interface;
  JDownloadManager_Query = interface;
  JDownloadManager_Request = interface;
  JFactoryResetProtectionPolicy = interface;
  JKeyguardManager = interface;
  JKeyguardManager_KeyguardLock = interface;
  JKeyguardManager_KeyguardDismissCallback = interface;
  JKeyguardManager_OnKeyguardExitResult = interface;
  JManagedSubscriptionsPolicy = interface;
  {$IF CompilerVersion < 33}
  JLocalTime = interface;
  JNotificationChannel = interface;
  JNotificationChannelGroup = interface;
  {$ENDIF}
  JPackagePolicy = interface;
  JSystemUpdateInfo = interface;
  JSystemUpdatePolicy = interface;
  JUiModeManager = interface;
  JWallpaperColors = interface;
  JWallpaperInfo = interface;
  JWallpaperManager = interface;
  JWallpaperManager_OnColorsChangedListener = interface;
  JWifiSsidPolicy = interface;

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

  JActivityOptionsClass = interface(JObjectClass)
    ['{F9045869-DA11-4E08-9B85-95051A0916A4}']
    {class} function _GetEXTRA_USAGE_TIME_REPORT: JString; cdecl;
    {class} function _GetEXTRA_USAGE_TIME_REPORT_PACKAGES: JString; cdecl;
    {class} function _GetMODE_BACKGROUND_ACTIVITY_START_ALLOWED: Integer; cdecl;
    {class} function _GetMODE_BACKGROUND_ACTIVITY_START_DENIED: Integer; cdecl;
    {class} function _GetMODE_BACKGROUND_ACTIVITY_START_SYSTEM_DEFINED: Integer; cdecl;
    {class} function makeBasic: JActivityOptions; cdecl;
    {class} function makeClipRevealAnimation(view: JView; int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): JActivityOptions; cdecl;
    {class} function makeCustomAnimation(context: JContext; int: Integer; int_1: Integer): JActivityOptions; overload; cdecl;
    {class} function makeCustomAnimation(context: JContext; int: Integer; int_1: Integer; int_2: Integer): JActivityOptions; overload; cdecl;
    {class} function makeLaunchIntoPip(pictureinpictureparams: JPictureInPictureParams): JActivityOptions; cdecl;
    {class} function makeScaleUpAnimation(view: JView; int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): JActivityOptions; cdecl;
    {class} function makeSceneTransitionAnimation(activity: JActivity; pair: JPair): JActivityOptions; overload; cdecl;
    {class} function makeSceneTransitionAnimation(activity: JActivity; view: JView; string_1: JString): JActivityOptions; overload; cdecl;
    {class} function makeTaskLaunchBehind: JActivityOptions; cdecl;
    {class} function makeThumbnailScaleUpAnimation(view: JView; bitmap: JBitmap; int: Integer; int_1: Integer): JActivityOptions; cdecl;
    {class} property EXTRA_USAGE_TIME_REPORT: JString read _GetEXTRA_USAGE_TIME_REPORT;
    {class} property EXTRA_USAGE_TIME_REPORT_PACKAGES: JString read _GetEXTRA_USAGE_TIME_REPORT_PACKAGES;
    {class} property MODE_BACKGROUND_ACTIVITY_START_ALLOWED: Integer read _GetMODE_BACKGROUND_ACTIVITY_START_ALLOWED;
    {class} property MODE_BACKGROUND_ACTIVITY_START_DENIED: Integer read _GetMODE_BACKGROUND_ACTIVITY_START_DENIED;
    {class} property MODE_BACKGROUND_ACTIVITY_START_SYSTEM_DEFINED: Integer read _GetMODE_BACKGROUND_ACTIVITY_START_SYSTEM_DEFINED;
  end;

  [JavaSignature('android/app/ActivityOptions')]
  JActivityOptions = interface(JObject)
    ['{CFDF856C-23C3-4E74-BA7E-6AB087CDB5AB}']
    function getLaunchBounds: JRect; cdecl;
    function getLaunchDisplayId: Integer; cdecl;
    function getLockTaskMode: Boolean; cdecl;
    function getPendingIntentBackgroundActivityStartMode: Integer; cdecl;
    function getPendingIntentCreatorBackgroundActivityStartMode: Integer; cdecl;
    function getSplashScreenStyle: Integer; cdecl;
    function isPendingIntentBackgroundActivityLaunchAllowed: Boolean; cdecl;
    function isShareIdentityEnabled: Boolean; cdecl;
    procedure requestUsageTimeReport(pendingintent: JPendingIntent); cdecl;
    function setAppVerificationBundle(bundle: JBundle): JActivityOptions; cdecl;
    function setLaunchBounds(rect: JRect): JActivityOptions; cdecl;
    function setLaunchDisplayId(int: Integer): JActivityOptions; cdecl;
    function setLockTaskEnabled(boolean: Boolean): JActivityOptions; cdecl;
    procedure setPendingIntentBackgroundActivityLaunchAllowed(boolean: Boolean); cdecl;
    function setPendingIntentBackgroundActivityStartMode(int: Integer): JActivityOptions; cdecl;
    function setPendingIntentCreatorBackgroundActivityStartMode(int: Integer): JActivityOptions; cdecl;
    function setShareIdentityEnabled(boolean: Boolean): JActivityOptions; cdecl;
    function setSplashScreenStyle(int: Integer): JActivityOptions; cdecl;
    function toBundle: JBundle; cdecl;
    function toString: JString; cdecl;
    procedure update(activityoptions: JActivityOptions); cdecl;
  end;
  TJActivityOptions = class(TJavaGenericImport<JActivityOptionsClass, JActivityOptions>) end;

  JDevicePolicyManager_OnClearApplicationUserDataListenerClass = interface(IJavaClass)
    ['{F40B1E32-8A89-4804-8301-F69A573675C5}']
  end;

  [JavaSignature('android/app/admin/DevicePolicyManager$OnClearApplicationUserDataListener')]
  JDevicePolicyManager_OnClearApplicationUserDataListener = interface(IJavaInstance)
    ['{83C11100-E381-4676-8E35-2A5080239321}']
    procedure onApplicationUserDataCleared(string_1: JString; boolean: Boolean); cdecl;
  end;
  TJDevicePolicyManager_OnClearApplicationUserDataListener = class(TJavaGenericImport<JDevicePolicyManager_OnClearApplicationUserDataListenerClass,
    JDevicePolicyManager_OnClearApplicationUserDataListener>) end;

  JDevicePolicyManagerClass = interface(JObjectClass)
    ['{A7B9475F-21B5-4FB9-A401-4C6FBA6E3372}']
    {class} function _GetACTION_ADD_DEVICE_ADMIN: JString; cdecl;
    {class} function _GetACTION_ADMIN_POLICY_COMPLIANCE: JString; cdecl;
    {class} function _GetACTION_APPLICATION_DELEGATION_SCOPES_CHANGED: JString; cdecl;
    {class} function _GetACTION_CHECK_POLICY_COMPLIANCE: JString; cdecl;
    {class} function _GetACTION_DEVICE_ADMIN_SERVICE: JString; cdecl;
    {class} function _GetACTION_DEVICE_FINANCING_STATE_CHANGED: JString; cdecl;
    {class} function _GetACTION_DEVICE_OWNER_CHANGED: JString; cdecl;
    {class} function _GetACTION_DEVICE_POLICY_RESOURCE_UPDATED: JString; cdecl;
    {class} function _GetACTION_GET_PROVISIONING_MODE: JString; cdecl;
    {class} function _GetACTION_MANAGED_PROFILE_PROVISIONED: JString; cdecl;
    {class} function _GetACTION_PROFILE_OWNER_CHANGED: JString; cdecl;
    {class} function _GetACTION_PROVISIONING_SUCCESSFUL: JString; cdecl;
    {class} function _GetACTION_PROVISION_MANAGED_DEVICE: JString; cdecl;
    {class} function _GetACTION_PROVISION_MANAGED_PROFILE: JString; cdecl;
    {class} function _GetACTION_SET_NEW_PARENT_PROFILE_PASSWORD: JString; cdecl;
    {class} function _GetACTION_SET_NEW_PASSWORD: JString; cdecl;
    {class} function _GetACTION_START_ENCRYPTION: JString; cdecl;
    {class} function _GetACTION_SYSTEM_UPDATE_POLICY_CHANGED: JString; cdecl;
    {class} function _GetCONTENT_PROTECTION_DISABLED: Integer; cdecl;
    {class} function _GetCONTENT_PROTECTION_ENABLED: Integer; cdecl;
    {class} function _GetCONTENT_PROTECTION_NOT_CONTROLLED_BY_POLICY: Integer; cdecl;
    {class} function _GetDELEGATION_APP_RESTRICTIONS: JString; cdecl;
    {class} function _GetDELEGATION_BLOCK_UNINSTALL: JString; cdecl;
    {class} function _GetDELEGATION_CERT_INSTALL: JString; cdecl;
    {class} function _GetDELEGATION_CERT_SELECTION: JString; cdecl;
    {class} function _GetDELEGATION_ENABLE_SYSTEM_APP: JString; cdecl;
    {class} function _GetDELEGATION_INSTALL_EXISTING_PACKAGE: JString; cdecl;
    {class} function _GetDELEGATION_KEEP_UNINSTALLED_PACKAGES: JString; cdecl;
    {class} function _GetDELEGATION_NETWORK_LOGGING: JString; cdecl;
    {class} function _GetDELEGATION_PACKAGE_ACCESS: JString; cdecl;
    {class} function _GetDELEGATION_PERMISSION_GRANT: JString; cdecl;
    {class} function _GetDELEGATION_SECURITY_LOGGING: JString; cdecl;
    {class} function _GetENCRYPTION_STATUS_ACTIVATING: Integer; cdecl;
    {class} function _GetENCRYPTION_STATUS_ACTIVE: Integer; cdecl;
    {class} function _GetENCRYPTION_STATUS_ACTIVE_DEFAULT_KEY: Integer; cdecl;
    {class} function _GetENCRYPTION_STATUS_ACTIVE_PER_USER: Integer; cdecl;
    {class} function _GetENCRYPTION_STATUS_INACTIVE: Integer; cdecl;
    {class} function _GetENCRYPTION_STATUS_UNSUPPORTED: Integer; cdecl;
    {class} function _GetEXTRA_ADD_EXPLANATION: JString; cdecl;
    {class} function _GetEXTRA_DELEGATION_SCOPES: JString; cdecl;
    {class} function _GetEXTRA_DEVICE_ADMIN: JString; cdecl;
    {class} function _GetEXTRA_DEVICE_PASSWORD_REQUIREMENT_ONLY: JString; cdecl;
    {class} function _GetEXTRA_PASSWORD_COMPLEXITY: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_ACCOUNT_TO_MIGRATE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_ADMIN_EXTRAS_BUNDLE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_ALLOWED_PROVISIONING_MODES: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_ALLOW_OFFLINE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DEVICE_ADMIN_COMPONENT_NAME: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DEVICE_ADMIN_MINIMUM_VERSION_CODE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_CHECKSUM: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_DOWNLOAD_COOKIE_HEADER: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_DOWNLOAD_LOCATION: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_NAME: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DEVICE_ADMIN_SIGNATURE_CHECKSUM: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DISCLAIMERS: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DISCLAIMER_CONTENT: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_DISCLAIMER_HEADER: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_EMAIL_ADDRESS: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_IMEI: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_KEEP_ACCOUNT_ON_MIGRATION: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_KEEP_SCREEN_ON: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_LEAVE_ALL_SYSTEM_APPS_ENABLED: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_LOCALE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_LOCAL_TIME: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_LOGO_URI: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_MAIN_COLOR: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_MODE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_SENSORS_PERMISSION_GRANT_OPT_OUT: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_SERIAL_NUMBER: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_SHOULD_LAUNCH_RESULT_INTENT: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_SKIP_EDUCATION_SCREENS: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_SKIP_ENCRYPTION: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_SKIP_USER_CONSENT: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_TIME_ZONE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_USE_MOBILE_DATA: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_ANONYMOUS_IDENTITY: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_CA_CERTIFICATE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_DOMAIN: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_EAP_METHOD: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_HIDDEN: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_IDENTITY: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_PAC_URL: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_PASSWORD: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_PHASE2_AUTH: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_PROXY_BYPASS: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_PROXY_HOST: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_PROXY_PORT: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_SECURITY_TYPE: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_SSID: JString; cdecl;
    {class} function _GetEXTRA_PROVISIONING_WIFI_USER_CERTIFICATE: JString; cdecl;
    {class} function _GetEXTRA_RESOURCE_IDS: JString; cdecl;
    {class} function _GetEXTRA_RESOURCE_TYPE: JString; cdecl;
    {class} function _GetEXTRA_RESOURCE_TYPE_DRAWABLE: Integer; cdecl;
    {class} function _GetEXTRA_RESOURCE_TYPE_STRING: Integer; cdecl;
    {class} function _GetEXTRA_RESULT_LAUNCH_INTENT: JString; cdecl;
    {class} function _GetFLAG_EVICT_CREDENTIAL_ENCRYPTION_KEY: Integer; cdecl;
    {class} function _GetFLAG_MANAGED_CAN_ACCESS_PARENT: Integer; cdecl;
    {class} function _GetFLAG_PARENT_CAN_ACCESS_MANAGED: Integer; cdecl;
    {class} function _GetID_TYPE_BASE_INFO: Integer; cdecl;
    {class} function _GetID_TYPE_IMEI: Integer; cdecl;
    {class} function _GetID_TYPE_INDIVIDUAL_ATTESTATION: Integer; cdecl;
    {class} function _GetID_TYPE_MEID: Integer; cdecl;
    {class} function _GetID_TYPE_SERIAL: Integer; cdecl;
    {class} function _GetINSTALLKEY_REQUEST_CREDENTIALS_ACCESS: Integer; cdecl;
    {class} function _GetINSTALLKEY_SET_USER_SELECTABLE: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_BIOMETRICS: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_FACE: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_FEATURES_ALL: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_FEATURES_NONE: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_FINGERPRINT: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_IRIS: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_REMOTE_INPUT: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_SECURE_CAMERA: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_SECURE_NOTIFICATIONS: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_SHORTCUTS_ALL: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_TRUST_AGENTS: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_UNREDACTED_NOTIFICATIONS: Integer; cdecl;
    {class} function _GetKEYGUARD_DISABLE_WIDGETS_ALL: Integer; cdecl;
    {class} function _GetLEAVE_ALL_SYSTEM_APPS_ENABLED: Integer; cdecl;
    {class} function _GetLOCK_TASK_FEATURE_BLOCK_ACTIVITY_START_IN_TASK: Integer; cdecl;
    {class} function _GetLOCK_TASK_FEATURE_GLOBAL_ACTIONS: Integer; cdecl;
    {class} function _GetLOCK_TASK_FEATURE_HOME: Integer; cdecl;
    {class} function _GetLOCK_TASK_FEATURE_KEYGUARD: Integer; cdecl;
    {class} function _GetLOCK_TASK_FEATURE_NONE: Integer; cdecl;
    {class} function _GetLOCK_TASK_FEATURE_NOTIFICATIONS: Integer; cdecl;
    {class} function _GetLOCK_TASK_FEATURE_OVERVIEW: Integer; cdecl;
    {class} function _GetLOCK_TASK_FEATURE_SYSTEM_INFO: Integer; cdecl;
    {class} function _GetMAKE_USER_EPHEMERAL: Integer; cdecl;
    {class} function _GetMIME_TYPE_PROVISIONING_NFC: JString; cdecl;
    {class} function _GetMTE_DISABLED: Integer; cdecl;
    {class} function _GetMTE_ENABLED: Integer; cdecl;
    {class} function _GetMTE_NOT_CONTROLLED_BY_POLICY: Integer; cdecl;
    {class} function _GetNEARBY_STREAMING_DISABLED: Integer; cdecl;
    {class} function _GetNEARBY_STREAMING_ENABLED: Integer; cdecl;
    {class} function _GetNEARBY_STREAMING_NOT_CONTROLLED_BY_POLICY: Integer; cdecl;
    {class} function _GetNEARBY_STREAMING_SAME_MANAGED_ACCOUNT_ONLY: Integer; cdecl;
    {class} function _GetOPERATION_SAFETY_REASON_DRIVING_DISTRACTION: Integer; cdecl;
    {class} function _GetPASSWORD_COMPLEXITY_HIGH: Integer; cdecl;
    {class} function _GetPASSWORD_COMPLEXITY_LOW: Integer; cdecl;
    {class} function _GetPASSWORD_COMPLEXITY_MEDIUM: Integer; cdecl;
    {class} function _GetPASSWORD_COMPLEXITY_NONE: Integer; cdecl;
    {class} function _GetPASSWORD_QUALITY_ALPHABETIC: Integer; cdecl;
    {class} function _GetPASSWORD_QUALITY_ALPHANUMERIC: Integer; cdecl;
    {class} function _GetPASSWORD_QUALITY_BIOMETRIC_WEAK: Integer; cdecl;
    {class} function _GetPASSWORD_QUALITY_COMPLEX: Integer; cdecl;
    {class} function _GetPASSWORD_QUALITY_NUMERIC: Integer; cdecl;
    {class} function _GetPASSWORD_QUALITY_NUMERIC_COMPLEX: Integer; cdecl;
    {class} function _GetPASSWORD_QUALITY_SOMETHING: Integer; cdecl;
    {class} function _GetPASSWORD_QUALITY_UNSPECIFIED: Integer; cdecl;
    {class} function _GetPERMISSION_GRANT_STATE_DEFAULT: Integer; cdecl;
    {class} function _GetPERMISSION_GRANT_STATE_DENIED: Integer; cdecl;
    {class} function _GetPERMISSION_GRANT_STATE_GRANTED: Integer; cdecl;
    {class} function _GetPERMISSION_POLICY_AUTO_DENY: Integer; cdecl;
    {class} function _GetPERMISSION_POLICY_AUTO_GRANT: Integer; cdecl;
    {class} function _GetPERMISSION_POLICY_PROMPT: Integer; cdecl;
    {class} function _GetPERSONAL_APPS_NOT_SUSPENDED: Integer; cdecl;
    {class} function _GetPERSONAL_APPS_SUSPENDED_EXPLICITLY: Integer; cdecl;
    {class} function _GetPERSONAL_APPS_SUSPENDED_PROFILE_TIMEOUT: Integer; cdecl;
    {class} function _GetPOLICY_DISABLE_CAMERA: JString; cdecl;
    {class} function _GetPOLICY_DISABLE_SCREEN_CAPTURE: JString; cdecl;
    {class} function _GetPRIVATE_DNS_MODE_OFF: Integer; cdecl;
    {class} function _GetPRIVATE_DNS_MODE_OPPORTUNISTIC: Integer; cdecl;
    {class} function _GetPRIVATE_DNS_MODE_PROVIDER_HOSTNAME: Integer; cdecl;
    {class} function _GetPRIVATE_DNS_MODE_UNKNOWN: Integer; cdecl;
    {class} function _GetPRIVATE_DNS_SET_ERROR_FAILURE_SETTING: Integer; cdecl;
    {class} function _GetPRIVATE_DNS_SET_ERROR_HOST_NOT_SERVING: Integer; cdecl;
    {class} function _GetPRIVATE_DNS_SET_NO_ERROR: Integer; cdecl;
    {class} function _GetPROVISIONING_MODE_FULLY_MANAGED_DEVICE: Integer; cdecl;
    {class} function _GetPROVISIONING_MODE_MANAGED_PROFILE: Integer; cdecl;
    {class} function _GetPROVISIONING_MODE_MANAGED_PROFILE_ON_PERSONAL_DEVICE: Integer; cdecl;
    {class} function _GetRESET_PASSWORD_DO_NOT_ASK_CREDENTIALS_ON_BOOT: Integer; cdecl;
    {class} function _GetRESET_PASSWORD_REQUIRE_ENTRY: Integer; cdecl;
    {class} function _GetSKIP_SETUP_WIZARD: Integer; cdecl;
    {class} function _GetWIFI_SECURITY_ENTERPRISE_192: Integer; cdecl;
    {class} function _GetWIFI_SECURITY_ENTERPRISE_EAP: Integer; cdecl;
    {class} function _GetWIFI_SECURITY_OPEN: Integer; cdecl;
    {class} function _GetWIFI_SECURITY_PERSONAL: Integer; cdecl;
    {class} function _GetWIPE_EUICC: Integer; cdecl;
    {class} function _GetWIPE_EXTERNAL_STORAGE: Integer; cdecl;
    {class} function _GetWIPE_RESET_PROTECTION_DATA: Integer; cdecl;
    {class} function _GetWIPE_SILENTLY: Integer; cdecl;
    {class} function isMtePolicyEnforced: Boolean; cdecl;
    {class} property ACTION_ADD_DEVICE_ADMIN: JString read _GetACTION_ADD_DEVICE_ADMIN;
    {class} property ACTION_ADMIN_POLICY_COMPLIANCE: JString read _GetACTION_ADMIN_POLICY_COMPLIANCE;
    {class} property ACTION_APPLICATION_DELEGATION_SCOPES_CHANGED: JString read _GetACTION_APPLICATION_DELEGATION_SCOPES_CHANGED;
    {class} property ACTION_CHECK_POLICY_COMPLIANCE: JString read _GetACTION_CHECK_POLICY_COMPLIANCE;
    {class} property ACTION_DEVICE_ADMIN_SERVICE: JString read _GetACTION_DEVICE_ADMIN_SERVICE;
    {class} property ACTION_DEVICE_FINANCING_STATE_CHANGED: JString read _GetACTION_DEVICE_FINANCING_STATE_CHANGED;
    {class} property ACTION_DEVICE_OWNER_CHANGED: JString read _GetACTION_DEVICE_OWNER_CHANGED;
    {class} property ACTION_DEVICE_POLICY_RESOURCE_UPDATED: JString read _GetACTION_DEVICE_POLICY_RESOURCE_UPDATED;
    {class} property ACTION_GET_PROVISIONING_MODE: JString read _GetACTION_GET_PROVISIONING_MODE;
    {class} property ACTION_MANAGED_PROFILE_PROVISIONED: JString read _GetACTION_MANAGED_PROFILE_PROVISIONED;
    {class} property ACTION_PROFILE_OWNER_CHANGED: JString read _GetACTION_PROFILE_OWNER_CHANGED;
    {class} property ACTION_PROVISIONING_SUCCESSFUL: JString read _GetACTION_PROVISIONING_SUCCESSFUL;
    {class} property ACTION_PROVISION_MANAGED_DEVICE: JString read _GetACTION_PROVISION_MANAGED_DEVICE;
    {class} property ACTION_PROVISION_MANAGED_PROFILE: JString read _GetACTION_PROVISION_MANAGED_PROFILE;
    {class} property ACTION_SET_NEW_PARENT_PROFILE_PASSWORD: JString read _GetACTION_SET_NEW_PARENT_PROFILE_PASSWORD;
    {class} property ACTION_SET_NEW_PASSWORD: JString read _GetACTION_SET_NEW_PASSWORD;
    {class} property ACTION_START_ENCRYPTION: JString read _GetACTION_START_ENCRYPTION;
    {class} property ACTION_SYSTEM_UPDATE_POLICY_CHANGED: JString read _GetACTION_SYSTEM_UPDATE_POLICY_CHANGED;
    {class} property CONTENT_PROTECTION_DISABLED: Integer read _GetCONTENT_PROTECTION_DISABLED;
    {class} property CONTENT_PROTECTION_ENABLED: Integer read _GetCONTENT_PROTECTION_ENABLED;
    {class} property CONTENT_PROTECTION_NOT_CONTROLLED_BY_POLICY: Integer read _GetCONTENT_PROTECTION_NOT_CONTROLLED_BY_POLICY;
    {class} property DELEGATION_APP_RESTRICTIONS: JString read _GetDELEGATION_APP_RESTRICTIONS;
    {class} property DELEGATION_BLOCK_UNINSTALL: JString read _GetDELEGATION_BLOCK_UNINSTALL;
    {class} property DELEGATION_CERT_INSTALL: JString read _GetDELEGATION_CERT_INSTALL;
    {class} property DELEGATION_CERT_SELECTION: JString read _GetDELEGATION_CERT_SELECTION;
    {class} property DELEGATION_ENABLE_SYSTEM_APP: JString read _GetDELEGATION_ENABLE_SYSTEM_APP;
    {class} property DELEGATION_INSTALL_EXISTING_PACKAGE: JString read _GetDELEGATION_INSTALL_EXISTING_PACKAGE;
    {class} property DELEGATION_KEEP_UNINSTALLED_PACKAGES: JString read _GetDELEGATION_KEEP_UNINSTALLED_PACKAGES;
    {class} property DELEGATION_NETWORK_LOGGING: JString read _GetDELEGATION_NETWORK_LOGGING;
    {class} property DELEGATION_PACKAGE_ACCESS: JString read _GetDELEGATION_PACKAGE_ACCESS;
    {class} property DELEGATION_PERMISSION_GRANT: JString read _GetDELEGATION_PERMISSION_GRANT;
    {class} property DELEGATION_SECURITY_LOGGING: JString read _GetDELEGATION_SECURITY_LOGGING;
    {class} property ENCRYPTION_STATUS_ACTIVATING: Integer read _GetENCRYPTION_STATUS_ACTIVATING;
    {class} property ENCRYPTION_STATUS_ACTIVE: Integer read _GetENCRYPTION_STATUS_ACTIVE;
    {class} property ENCRYPTION_STATUS_ACTIVE_DEFAULT_KEY: Integer read _GetENCRYPTION_STATUS_ACTIVE_DEFAULT_KEY;
    {class} property ENCRYPTION_STATUS_ACTIVE_PER_USER: Integer read _GetENCRYPTION_STATUS_ACTIVE_PER_USER;
    {class} property ENCRYPTION_STATUS_INACTIVE: Integer read _GetENCRYPTION_STATUS_INACTIVE;
    {class} property ENCRYPTION_STATUS_UNSUPPORTED: Integer read _GetENCRYPTION_STATUS_UNSUPPORTED;
    {class} property EXTRA_ADD_EXPLANATION: JString read _GetEXTRA_ADD_EXPLANATION;
    {class} property EXTRA_DELEGATION_SCOPES: JString read _GetEXTRA_DELEGATION_SCOPES;
    {class} property EXTRA_DEVICE_ADMIN: JString read _GetEXTRA_DEVICE_ADMIN;
    {class} property EXTRA_DEVICE_PASSWORD_REQUIREMENT_ONLY: JString read _GetEXTRA_DEVICE_PASSWORD_REQUIREMENT_ONLY;
    {class} property EXTRA_PASSWORD_COMPLEXITY: JString read _GetEXTRA_PASSWORD_COMPLEXITY;
    {class} property EXTRA_PROVISIONING_ACCOUNT_TO_MIGRATE: JString read _GetEXTRA_PROVISIONING_ACCOUNT_TO_MIGRATE;
    {class} property EXTRA_PROVISIONING_ADMIN_EXTRAS_BUNDLE: JString read _GetEXTRA_PROVISIONING_ADMIN_EXTRAS_BUNDLE;
    {class} property EXTRA_PROVISIONING_ALLOWED_PROVISIONING_MODES: JString read _GetEXTRA_PROVISIONING_ALLOWED_PROVISIONING_MODES;
    {class} property EXTRA_PROVISIONING_ALLOW_OFFLINE: JString read _GetEXTRA_PROVISIONING_ALLOW_OFFLINE;
    {class} property EXTRA_PROVISIONING_DEVICE_ADMIN_COMPONENT_NAME: JString read _GetEXTRA_PROVISIONING_DEVICE_ADMIN_COMPONENT_NAME;
    {class} property EXTRA_PROVISIONING_DEVICE_ADMIN_MINIMUM_VERSION_CODE: JString read _GetEXTRA_PROVISIONING_DEVICE_ADMIN_MINIMUM_VERSION_CODE;
    {class} property EXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_CHECKSUM: JString read _GetEXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_CHECKSUM;
    {class} property EXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_DOWNLOAD_COOKIE_HEADER: JString read _GetEXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_DOWNLOAD_COOKIE_HEADER;
    {class} property EXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_DOWNLOAD_LOCATION: JString read _GetEXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_DOWNLOAD_LOCATION;
    {class} property EXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_NAME: JString read _GetEXTRA_PROVISIONING_DEVICE_ADMIN_PACKAGE_NAME;
    {class} property EXTRA_PROVISIONING_DEVICE_ADMIN_SIGNATURE_CHECKSUM: JString read _GetEXTRA_PROVISIONING_DEVICE_ADMIN_SIGNATURE_CHECKSUM;
    {class} property EXTRA_PROVISIONING_DISCLAIMERS: JString read _GetEXTRA_PROVISIONING_DISCLAIMERS;
    {class} property EXTRA_PROVISIONING_DISCLAIMER_CONTENT: JString read _GetEXTRA_PROVISIONING_DISCLAIMER_CONTENT;
    {class} property EXTRA_PROVISIONING_DISCLAIMER_HEADER: JString read _GetEXTRA_PROVISIONING_DISCLAIMER_HEADER;
    {class} property EXTRA_PROVISIONING_EMAIL_ADDRESS: JString read _GetEXTRA_PROVISIONING_EMAIL_ADDRESS;
    {class} property EXTRA_PROVISIONING_IMEI: JString read _GetEXTRA_PROVISIONING_IMEI;
    {class} property EXTRA_PROVISIONING_KEEP_ACCOUNT_ON_MIGRATION: JString read _GetEXTRA_PROVISIONING_KEEP_ACCOUNT_ON_MIGRATION;
    {class} property EXTRA_PROVISIONING_KEEP_SCREEN_ON: JString read _GetEXTRA_PROVISIONING_KEEP_SCREEN_ON;
    {class} property EXTRA_PROVISIONING_LEAVE_ALL_SYSTEM_APPS_ENABLED: JString read _GetEXTRA_PROVISIONING_LEAVE_ALL_SYSTEM_APPS_ENABLED;
    {class} property EXTRA_PROVISIONING_LOCALE: JString read _GetEXTRA_PROVISIONING_LOCALE;
    {class} property EXTRA_PROVISIONING_LOCAL_TIME: JString read _GetEXTRA_PROVISIONING_LOCAL_TIME;
    {class} property EXTRA_PROVISIONING_LOGO_URI: JString read _GetEXTRA_PROVISIONING_LOGO_URI;
    {class} property EXTRA_PROVISIONING_MAIN_COLOR: JString read _GetEXTRA_PROVISIONING_MAIN_COLOR;
    {class} property EXTRA_PROVISIONING_MODE: JString read _GetEXTRA_PROVISIONING_MODE;
    {class} property EXTRA_PROVISIONING_SENSORS_PERMISSION_GRANT_OPT_OUT: JString read _GetEXTRA_PROVISIONING_SENSORS_PERMISSION_GRANT_OPT_OUT;
    {class} property EXTRA_PROVISIONING_SERIAL_NUMBER: JString read _GetEXTRA_PROVISIONING_SERIAL_NUMBER;
    {class} property EXTRA_PROVISIONING_SHOULD_LAUNCH_RESULT_INTENT: JString read _GetEXTRA_PROVISIONING_SHOULD_LAUNCH_RESULT_INTENT;
    {class} property EXTRA_PROVISIONING_SKIP_EDUCATION_SCREENS: JString read _GetEXTRA_PROVISIONING_SKIP_EDUCATION_SCREENS;
    {class} property EXTRA_PROVISIONING_SKIP_ENCRYPTION: JString read _GetEXTRA_PROVISIONING_SKIP_ENCRYPTION;
    {class} property EXTRA_PROVISIONING_SKIP_USER_CONSENT: JString read _GetEXTRA_PROVISIONING_SKIP_USER_CONSENT;
    {class} property EXTRA_PROVISIONING_TIME_ZONE: JString read _GetEXTRA_PROVISIONING_TIME_ZONE;
    {class} property EXTRA_PROVISIONING_USE_MOBILE_DATA: JString read _GetEXTRA_PROVISIONING_USE_MOBILE_DATA;
    {class} property EXTRA_PROVISIONING_WIFI_ANONYMOUS_IDENTITY: JString read _GetEXTRA_PROVISIONING_WIFI_ANONYMOUS_IDENTITY;
    {class} property EXTRA_PROVISIONING_WIFI_CA_CERTIFICATE: JString read _GetEXTRA_PROVISIONING_WIFI_CA_CERTIFICATE;
    {class} property EXTRA_PROVISIONING_WIFI_DOMAIN: JString read _GetEXTRA_PROVISIONING_WIFI_DOMAIN;
    {class} property EXTRA_PROVISIONING_WIFI_EAP_METHOD: JString read _GetEXTRA_PROVISIONING_WIFI_EAP_METHOD;
    {class} property EXTRA_PROVISIONING_WIFI_HIDDEN: JString read _GetEXTRA_PROVISIONING_WIFI_HIDDEN;
    {class} property EXTRA_PROVISIONING_WIFI_IDENTITY: JString read _GetEXTRA_PROVISIONING_WIFI_IDENTITY;
    {class} property EXTRA_PROVISIONING_WIFI_PAC_URL: JString read _GetEXTRA_PROVISIONING_WIFI_PAC_URL;
    {class} property EXTRA_PROVISIONING_WIFI_PASSWORD: JString read _GetEXTRA_PROVISIONING_WIFI_PASSWORD;
    {class} property EXTRA_PROVISIONING_WIFI_PHASE2_AUTH: JString read _GetEXTRA_PROVISIONING_WIFI_PHASE2_AUTH;
    {class} property EXTRA_PROVISIONING_WIFI_PROXY_BYPASS: JString read _GetEXTRA_PROVISIONING_WIFI_PROXY_BYPASS;
    {class} property EXTRA_PROVISIONING_WIFI_PROXY_HOST: JString read _GetEXTRA_PROVISIONING_WIFI_PROXY_HOST;
    {class} property EXTRA_PROVISIONING_WIFI_PROXY_PORT: JString read _GetEXTRA_PROVISIONING_WIFI_PROXY_PORT;
    {class} property EXTRA_PROVISIONING_WIFI_SECURITY_TYPE: JString read _GetEXTRA_PROVISIONING_WIFI_SECURITY_TYPE;
    {class} property EXTRA_PROVISIONING_WIFI_SSID: JString read _GetEXTRA_PROVISIONING_WIFI_SSID;
    {class} property EXTRA_PROVISIONING_WIFI_USER_CERTIFICATE: JString read _GetEXTRA_PROVISIONING_WIFI_USER_CERTIFICATE;
    {class} property EXTRA_RESOURCE_IDS: JString read _GetEXTRA_RESOURCE_IDS;
    {class} property EXTRA_RESOURCE_TYPE: JString read _GetEXTRA_RESOURCE_TYPE;
    {class} property EXTRA_RESOURCE_TYPE_DRAWABLE: Integer read _GetEXTRA_RESOURCE_TYPE_DRAWABLE;
    {class} property EXTRA_RESOURCE_TYPE_STRING: Integer read _GetEXTRA_RESOURCE_TYPE_STRING;
    {class} property EXTRA_RESULT_LAUNCH_INTENT: JString read _GetEXTRA_RESULT_LAUNCH_INTENT;
    {class} property FLAG_EVICT_CREDENTIAL_ENCRYPTION_KEY: Integer read _GetFLAG_EVICT_CREDENTIAL_ENCRYPTION_KEY;
    {class} property FLAG_MANAGED_CAN_ACCESS_PARENT: Integer read _GetFLAG_MANAGED_CAN_ACCESS_PARENT;
    {class} property FLAG_PARENT_CAN_ACCESS_MANAGED: Integer read _GetFLAG_PARENT_CAN_ACCESS_MANAGED;
    {class} property ID_TYPE_BASE_INFO: Integer read _GetID_TYPE_BASE_INFO;
    {class} property ID_TYPE_IMEI: Integer read _GetID_TYPE_IMEI;
    {class} property ID_TYPE_INDIVIDUAL_ATTESTATION: Integer read _GetID_TYPE_INDIVIDUAL_ATTESTATION;
    {class} property ID_TYPE_MEID: Integer read _GetID_TYPE_MEID;
    {class} property ID_TYPE_SERIAL: Integer read _GetID_TYPE_SERIAL;
    {class} property INSTALLKEY_REQUEST_CREDENTIALS_ACCESS: Integer read _GetINSTALLKEY_REQUEST_CREDENTIALS_ACCESS;
    {class} property INSTALLKEY_SET_USER_SELECTABLE: Integer read _GetINSTALLKEY_SET_USER_SELECTABLE;
    {class} property KEYGUARD_DISABLE_BIOMETRICS: Integer read _GetKEYGUARD_DISABLE_BIOMETRICS;
    {class} property KEYGUARD_DISABLE_FACE: Integer read _GetKEYGUARD_DISABLE_FACE;
    {class} property KEYGUARD_DISABLE_FEATURES_ALL: Integer read _GetKEYGUARD_DISABLE_FEATURES_ALL;
    {class} property KEYGUARD_DISABLE_FEATURES_NONE: Integer read _GetKEYGUARD_DISABLE_FEATURES_NONE;
    {class} property KEYGUARD_DISABLE_FINGERPRINT: Integer read _GetKEYGUARD_DISABLE_FINGERPRINT;
    {class} property KEYGUARD_DISABLE_IRIS: Integer read _GetKEYGUARD_DISABLE_IRIS;
    {class} property KEYGUARD_DISABLE_REMOTE_INPUT: Integer read _GetKEYGUARD_DISABLE_REMOTE_INPUT;
    {class} property KEYGUARD_DISABLE_SECURE_CAMERA: Integer read _GetKEYGUARD_DISABLE_SECURE_CAMERA;
    {class} property KEYGUARD_DISABLE_SECURE_NOTIFICATIONS: Integer read _GetKEYGUARD_DISABLE_SECURE_NOTIFICATIONS;
    {class} property KEYGUARD_DISABLE_SHORTCUTS_ALL: Integer read _GetKEYGUARD_DISABLE_SHORTCUTS_ALL;
    {class} property KEYGUARD_DISABLE_TRUST_AGENTS: Integer read _GetKEYGUARD_DISABLE_TRUST_AGENTS;
    {class} property KEYGUARD_DISABLE_UNREDACTED_NOTIFICATIONS: Integer read _GetKEYGUARD_DISABLE_UNREDACTED_NOTIFICATIONS;
    {class} property KEYGUARD_DISABLE_WIDGETS_ALL: Integer read _GetKEYGUARD_DISABLE_WIDGETS_ALL;
    {class} property LEAVE_ALL_SYSTEM_APPS_ENABLED: Integer read _GetLEAVE_ALL_SYSTEM_APPS_ENABLED;
    {class} property LOCK_TASK_FEATURE_BLOCK_ACTIVITY_START_IN_TASK: Integer read _GetLOCK_TASK_FEATURE_BLOCK_ACTIVITY_START_IN_TASK;
    {class} property LOCK_TASK_FEATURE_GLOBAL_ACTIONS: Integer read _GetLOCK_TASK_FEATURE_GLOBAL_ACTIONS;
    {class} property LOCK_TASK_FEATURE_HOME: Integer read _GetLOCK_TASK_FEATURE_HOME;
    {class} property LOCK_TASK_FEATURE_KEYGUARD: Integer read _GetLOCK_TASK_FEATURE_KEYGUARD;
    {class} property LOCK_TASK_FEATURE_NONE: Integer read _GetLOCK_TASK_FEATURE_NONE;
    {class} property LOCK_TASK_FEATURE_NOTIFICATIONS: Integer read _GetLOCK_TASK_FEATURE_NOTIFICATIONS;
    {class} property LOCK_TASK_FEATURE_OVERVIEW: Integer read _GetLOCK_TASK_FEATURE_OVERVIEW;
    {class} property LOCK_TASK_FEATURE_SYSTEM_INFO: Integer read _GetLOCK_TASK_FEATURE_SYSTEM_INFO;
    {class} property MAKE_USER_EPHEMERAL: Integer read _GetMAKE_USER_EPHEMERAL;
    {class} property MIME_TYPE_PROVISIONING_NFC: JString read _GetMIME_TYPE_PROVISIONING_NFC;
    {class} property MTE_DISABLED: Integer read _GetMTE_DISABLED;
    {class} property MTE_ENABLED: Integer read _GetMTE_ENABLED;
    {class} property MTE_NOT_CONTROLLED_BY_POLICY: Integer read _GetMTE_NOT_CONTROLLED_BY_POLICY;
    {class} property NEARBY_STREAMING_DISABLED: Integer read _GetNEARBY_STREAMING_DISABLED;
    {class} property NEARBY_STREAMING_ENABLED: Integer read _GetNEARBY_STREAMING_ENABLED;
    {class} property NEARBY_STREAMING_NOT_CONTROLLED_BY_POLICY: Integer read _GetNEARBY_STREAMING_NOT_CONTROLLED_BY_POLICY;
    {class} property NEARBY_STREAMING_SAME_MANAGED_ACCOUNT_ONLY: Integer read _GetNEARBY_STREAMING_SAME_MANAGED_ACCOUNT_ONLY;
    {class} property OPERATION_SAFETY_REASON_DRIVING_DISTRACTION: Integer read _GetOPERATION_SAFETY_REASON_DRIVING_DISTRACTION;
    {class} property PASSWORD_COMPLEXITY_HIGH: Integer read _GetPASSWORD_COMPLEXITY_HIGH;
    {class} property PASSWORD_COMPLEXITY_LOW: Integer read _GetPASSWORD_COMPLEXITY_LOW;
    {class} property PASSWORD_COMPLEXITY_MEDIUM: Integer read _GetPASSWORD_COMPLEXITY_MEDIUM;
    {class} property PASSWORD_COMPLEXITY_NONE: Integer read _GetPASSWORD_COMPLEXITY_NONE;
    {class} property PASSWORD_QUALITY_ALPHABETIC: Integer read _GetPASSWORD_QUALITY_ALPHABETIC;
    {class} property PASSWORD_QUALITY_ALPHANUMERIC: Integer read _GetPASSWORD_QUALITY_ALPHANUMERIC;
    {class} property PASSWORD_QUALITY_BIOMETRIC_WEAK: Integer read _GetPASSWORD_QUALITY_BIOMETRIC_WEAK;
    {class} property PASSWORD_QUALITY_COMPLEX: Integer read _GetPASSWORD_QUALITY_COMPLEX;
    {class} property PASSWORD_QUALITY_NUMERIC: Integer read _GetPASSWORD_QUALITY_NUMERIC;
    {class} property PASSWORD_QUALITY_NUMERIC_COMPLEX: Integer read _GetPASSWORD_QUALITY_NUMERIC_COMPLEX;
    {class} property PASSWORD_QUALITY_SOMETHING: Integer read _GetPASSWORD_QUALITY_SOMETHING;
    {class} property PASSWORD_QUALITY_UNSPECIFIED: Integer read _GetPASSWORD_QUALITY_UNSPECIFIED;
    {class} property PERMISSION_GRANT_STATE_DEFAULT: Integer read _GetPERMISSION_GRANT_STATE_DEFAULT;
    {class} property PERMISSION_GRANT_STATE_DENIED: Integer read _GetPERMISSION_GRANT_STATE_DENIED;
    {class} property PERMISSION_GRANT_STATE_GRANTED: Integer read _GetPERMISSION_GRANT_STATE_GRANTED;
    {class} property PERMISSION_POLICY_AUTO_DENY: Integer read _GetPERMISSION_POLICY_AUTO_DENY;
    {class} property PERMISSION_POLICY_AUTO_GRANT: Integer read _GetPERMISSION_POLICY_AUTO_GRANT;
    {class} property PERMISSION_POLICY_PROMPT: Integer read _GetPERMISSION_POLICY_PROMPT;
    {class} property PERSONAL_APPS_NOT_SUSPENDED: Integer read _GetPERSONAL_APPS_NOT_SUSPENDED;
    {class} property PERSONAL_APPS_SUSPENDED_EXPLICITLY: Integer read _GetPERSONAL_APPS_SUSPENDED_EXPLICITLY;
    {class} property PERSONAL_APPS_SUSPENDED_PROFILE_TIMEOUT: Integer read _GetPERSONAL_APPS_SUSPENDED_PROFILE_TIMEOUT;
    {class} property POLICY_DISABLE_CAMERA: JString read _GetPOLICY_DISABLE_CAMERA;
    {class} property POLICY_DISABLE_SCREEN_CAPTURE: JString read _GetPOLICY_DISABLE_SCREEN_CAPTURE;
    {class} property PRIVATE_DNS_MODE_OFF: Integer read _GetPRIVATE_DNS_MODE_OFF;
    {class} property PRIVATE_DNS_MODE_OPPORTUNISTIC: Integer read _GetPRIVATE_DNS_MODE_OPPORTUNISTIC;
    {class} property PRIVATE_DNS_MODE_PROVIDER_HOSTNAME: Integer read _GetPRIVATE_DNS_MODE_PROVIDER_HOSTNAME;
    {class} property PRIVATE_DNS_MODE_UNKNOWN: Integer read _GetPRIVATE_DNS_MODE_UNKNOWN;
    {class} property PRIVATE_DNS_SET_ERROR_FAILURE_SETTING: Integer read _GetPRIVATE_DNS_SET_ERROR_FAILURE_SETTING;
    {class} property PRIVATE_DNS_SET_ERROR_HOST_NOT_SERVING: Integer read _GetPRIVATE_DNS_SET_ERROR_HOST_NOT_SERVING;
    {class} property PRIVATE_DNS_SET_NO_ERROR: Integer read _GetPRIVATE_DNS_SET_NO_ERROR;
    {class} property PROVISIONING_MODE_FULLY_MANAGED_DEVICE: Integer read _GetPROVISIONING_MODE_FULLY_MANAGED_DEVICE;
    {class} property PROVISIONING_MODE_MANAGED_PROFILE: Integer read _GetPROVISIONING_MODE_MANAGED_PROFILE;
    {class} property PROVISIONING_MODE_MANAGED_PROFILE_ON_PERSONAL_DEVICE: Integer read _GetPROVISIONING_MODE_MANAGED_PROFILE_ON_PERSONAL_DEVICE;
    {class} property RESET_PASSWORD_DO_NOT_ASK_CREDENTIALS_ON_BOOT: Integer read _GetRESET_PASSWORD_DO_NOT_ASK_CREDENTIALS_ON_BOOT;
    {class} property RESET_PASSWORD_REQUIRE_ENTRY: Integer read _GetRESET_PASSWORD_REQUIRE_ENTRY;
    {class} property SKIP_SETUP_WIZARD: Integer read _GetSKIP_SETUP_WIZARD;
    {class} property WIFI_SECURITY_ENTERPRISE_192: Integer read _GetWIFI_SECURITY_ENTERPRISE_192;
    {class} property WIFI_SECURITY_ENTERPRISE_EAP: Integer read _GetWIFI_SECURITY_ENTERPRISE_EAP;
    {class} property WIFI_SECURITY_OPEN: Integer read _GetWIFI_SECURITY_OPEN;
    {class} property WIFI_SECURITY_PERSONAL: Integer read _GetWIFI_SECURITY_PERSONAL;
    {class} property WIPE_EUICC: Integer read _GetWIPE_EUICC;
    {class} property WIPE_EXTERNAL_STORAGE: Integer read _GetWIPE_EXTERNAL_STORAGE;
    {class} property WIPE_RESET_PROTECTION_DATA: Integer read _GetWIPE_RESET_PROTECTION_DATA;
    {class} property WIPE_SILENTLY: Integer read _GetWIPE_SILENTLY;
  end;

  [JavaSignature('android/app/admin/DevicePolicyManager')]
  JDevicePolicyManager = interface(JObject)
    ['{06B6B8D0-7D8E-4145-B75B-0B936534EEE9}']
    procedure acknowledgeDeviceCompliant; cdecl;
    procedure addCrossProfileIntentFilter(componentname: JComponentName; intentfilter: JIntentFilter; int: Integer); cdecl;
    function addCrossProfileWidgetProvider(componentname: JComponentName; string_1: JString): Boolean; cdecl;
    function addOverrideApn(componentname: JComponentName; apnsetting: JApnSetting): Integer; cdecl;
    procedure addPersistentPreferredActivity(componentname: JComponentName; intentfilter: JIntentFilter; componentname_1: JComponentName); cdecl;
    procedure addUserRestriction(componentname: JComponentName; string_1: JString); cdecl;
    procedure addUserRestrictionGlobally(string_1: JString); cdecl;
    function bindDeviceAdminServiceAsUser(componentname: JComponentName; intent: JIntent; serviceconnection: JServiceConnection; int: Integer;
      userhandle: JUserHandle): Boolean; overload; cdecl;
    function bindDeviceAdminServiceAsUser(componentname: JComponentName; intent: JIntent; serviceconnection: JServiceConnection;
      bindserviceflags: JContext_BindServiceFlags; userhandle: JUserHandle): Boolean; overload; cdecl;
    function canAdminGrantSensorsPermissions: Boolean; cdecl;
    function canUsbDataSignalingBeDisabled: Boolean; cdecl;
    procedure clearApplicationUserData(componentname: JComponentName; string_1: JString; executor: JExecutor;
      onclearapplicationuserdatalistener: JDevicePolicyManager_OnClearApplicationUserDataListener); cdecl;
    procedure clearCrossProfileIntentFilters(componentname: JComponentName); cdecl;
    procedure clearDeviceOwnerApp(string_1: JString); cdecl;
    procedure clearPackagePersistentPreferredActivities(componentname: JComponentName; string_1: JString); cdecl;
    procedure clearProfileOwner(componentname: JComponentName); cdecl;
    function clearResetPasswordToken(componentname: JComponentName): Boolean; cdecl;
    procedure clearUserRestriction(componentname: JComponentName; string_1: JString); cdecl;
    function createAdminSupportIntent(string_1: JString): JIntent; cdecl;
    function createAndManageUser(componentname: JComponentName; string_1: JString; componentname_1: JComponentName;
      persistablebundle: JPersistableBundle; int: Integer): JUserHandle; cdecl;
    function enableSystemApp(componentname: JComponentName; intent: JIntent): Integer; overload; cdecl;
    procedure enableSystemApp(componentname: JComponentName; string_1: JString); overload; cdecl;
    function generateKeyPair(componentname: JComponentName; string_1: JString; keygenparameterspec: JKeyGenParameterSpec;
      int: Integer): JAttestedKeyPair; cdecl;
    function getAccountTypesWithManagementDisabled: TJavaObjectArray<JString>; cdecl;
    function getActiveAdmins: JList; cdecl;
    function getAffiliationIds(componentname: JComponentName): JSet; cdecl;
    function getAlwaysOnVpnLockdownWhitelist(componentname: JComponentName): JSet; cdecl;
    function getAlwaysOnVpnPackage(componentname: JComponentName): JString; cdecl;
    function getApplicationRestrictions(componentname: JComponentName; string_1: JString): JBundle; cdecl;
    function getApplicationRestrictionsManagingPackage(componentname: JComponentName): JString; cdecl;
    function getAutoTimeEnabled(componentname: JComponentName): Boolean; cdecl;
    function getAutoTimeRequired: Boolean; cdecl;
    function getAutoTimeZoneEnabled(componentname: JComponentName): Boolean; cdecl;
    function getBindDeviceAdminTargetUsers(componentname: JComponentName): JList; cdecl;
    function getBluetoothContactSharingDisabled(componentname: JComponentName): Boolean; cdecl;
    function getCameraDisabled(componentname: JComponentName): Boolean; cdecl;
    function getCertInstallerPackage(componentname: JComponentName): JString; cdecl;
    function getContentProtectionPolicy(componentname: JComponentName): Integer; cdecl;
    function getCredentialManagerPolicy: JPackagePolicy; cdecl;
    function getCrossProfileCalendarPackages(componentname: JComponentName): JSet; cdecl;
    function getCrossProfileCallerIdDisabled(componentname: JComponentName): Boolean; cdecl;
    function getCrossProfileContactsSearchDisabled(componentname: JComponentName): Boolean; cdecl;
    function getCrossProfilePackages(componentname: JComponentName): JSet; cdecl;
    function getCrossProfileWidgetProviders(componentname: JComponentName): JList; cdecl;
    function getCurrentFailedPasswordAttempts: Integer; cdecl;
    function getDelegatePackages(componentname: JComponentName; string_1: JString): JList; cdecl;
    function getDelegatedScopes(componentname: JComponentName; string_1: JString): JList; cdecl;
    function getDeviceOwnerLockScreenInfo: JCharSequence; cdecl;
    function getDevicePolicyManagementRoleHolderPackage: JString; cdecl;
    function getEndUserSessionMessage(componentname: JComponentName): JCharSequence; cdecl;
    function getEnrollmentSpecificId: JString; cdecl;
    function getFactoryResetProtectionPolicy(componentname: JComponentName): JFactoryResetProtectionPolicy; cdecl;
    function getGlobalPrivateDnsHost(componentname: JComponentName): JString; cdecl;
    function getGlobalPrivateDnsMode(componentname: JComponentName): Integer; cdecl;
    function getInstalledCaCerts(componentname: JComponentName): JList; cdecl;
    function getKeepUninstalledPackages(componentname: JComponentName): JList; cdecl;
    function getKeyPairGrants(string_1: JString): JMap; cdecl;
    function getKeyguardDisabledFeatures(componentname: JComponentName): Integer; cdecl;
    function getLockTaskFeatures(componentname: JComponentName): Integer; cdecl;
    function getLockTaskPackages(componentname: JComponentName): TJavaObjectArray<JString>; cdecl;
    function getLongSupportMessage(componentname: JComponentName): JCharSequence; cdecl;
    function getManagedProfileCallerIdAccessPolicy: JPackagePolicy; cdecl;
    function getManagedProfileContactsAccessPolicy: JPackagePolicy; cdecl;
    function getManagedProfileMaximumTimeOff(componentname: JComponentName): Int64; cdecl;
    function getManagedSubscriptionsPolicy: JManagedSubscriptionsPolicy; cdecl;
    function getMaximumFailedPasswordsForWipe(componentname: JComponentName): Integer; cdecl;
    function getMaximumTimeToLock(componentname: JComponentName): Int64; cdecl;
    function getMeteredDataDisabledPackages(componentname: JComponentName): JList; cdecl;
    function getMinimumRequiredWifiSecurityLevel: Integer; cdecl;
    function getMtePolicy: Integer; cdecl;
    function getNearbyAppStreamingPolicy: Integer; cdecl;
    function getNearbyNotificationStreamingPolicy: Integer; cdecl;
    function getOrganizationColor(componentname: JComponentName): Integer; cdecl;
    function getOrganizationName(componentname: JComponentName): JCharSequence; cdecl;
    function getOverrideApns(componentname: JComponentName): JList; cdecl;
    function getParentProfileInstance(componentname: JComponentName): JDevicePolicyManager; cdecl;
    function getPasswordComplexity: Integer; cdecl;
    function getPasswordExpiration(componentname: JComponentName): Int64; cdecl;
    function getPasswordExpirationTimeout(componentname: JComponentName): Int64; cdecl;
    function getPasswordHistoryLength(componentname: JComponentName): Integer; cdecl;
    function getPasswordMaximumLength(int: Integer): Integer; cdecl;
    function getPasswordMinimumLength(componentname: JComponentName): Integer; cdecl;
    function getPasswordMinimumLetters(componentname: JComponentName): Integer; cdecl;
    function getPasswordMinimumLowerCase(componentname: JComponentName): Integer; cdecl;
    function getPasswordMinimumNonLetter(componentname: JComponentName): Integer; cdecl;
    function getPasswordMinimumNumeric(componentname: JComponentName): Integer; cdecl;
    function getPasswordMinimumSymbols(componentname: JComponentName): Integer; cdecl;
    function getPasswordMinimumUpperCase(componentname: JComponentName): Integer; cdecl;
    function getPasswordQuality(componentname: JComponentName): Integer; cdecl;
    function getPendingSystemUpdate(componentname: JComponentName): JSystemUpdateInfo; cdecl;
    function getPermissionGrantState(componentname: JComponentName; string_1: JString; string_2: JString): Integer; cdecl;
    function getPermissionPolicy(componentname: JComponentName): Integer; cdecl;
    function getPermittedAccessibilityServices(componentname: JComponentName): JList; cdecl;
    function getPermittedCrossProfileNotificationListeners(componentname: JComponentName): JList; cdecl;
    function getPermittedInputMethods(componentname: JComponentName): JList; cdecl;
    function getPersonalAppsSuspendedReasons(componentname: JComponentName): Integer; cdecl;
    function getPreferentialNetworkServiceConfigs: JList; cdecl;
    function getRequiredPasswordComplexity: Integer; cdecl;
    function getRequiredStrongAuthTimeout(componentname: JComponentName): Int64; cdecl;
    function getResources: JDevicePolicyResourcesManager; cdecl;
    function getScreenCaptureDisabled(componentname: JComponentName): Boolean; cdecl;
    function getSecondaryUsers(componentname: JComponentName): JList; cdecl;
    function getShortSupportMessage(componentname: JComponentName): JCharSequence; cdecl;
    function getStartUserSessionMessage(componentname: JComponentName): JCharSequence; cdecl;
    function getStorageEncryption(componentname: JComponentName): Boolean; cdecl;
    function getStorageEncryptionStatus: Integer; cdecl;
    function getSubscriptionIds: JSet; cdecl;
    function getSystemUpdatePolicy: JSystemUpdatePolicy; cdecl;
    function getTransferOwnershipBundle: JPersistableBundle; cdecl;
    function getTrustAgentConfiguration(componentname: JComponentName; componentname_1: JComponentName): JList; cdecl;
    function getUserControlDisabledPackages(componentname: JComponentName): JList; cdecl;
    function getUserRestrictions(componentname: JComponentName): JBundle; cdecl;
    function getUserRestrictionsGlobally: JBundle; cdecl;
    function getWifiMacAddress(componentname: JComponentName): JString; cdecl;
    function getWifiSsidPolicy: JWifiSsidPolicy; cdecl;
    function grantKeyPairToApp(componentname: JComponentName; string_1: JString; string_2: JString): Boolean; cdecl;
    function grantKeyPairToWifiAuth(string_1: JString): Boolean; cdecl;
    function hasCaCertInstalled(componentname: JComponentName; bytes: TJavaArray<Byte>): Boolean; cdecl;
    function hasGrantedPolicy(componentname: JComponentName; int: Integer): Boolean; cdecl;
    function hasKeyPair(string_1: JString): Boolean; cdecl;
    function hasLockdownAdminConfiguredNetworks(componentname: JComponentName): Boolean; cdecl;
    function installCaCert(componentname: JComponentName; bytes: TJavaArray<Byte>): Boolean; cdecl;
    function installExistingPackage(componentname: JComponentName; string_1: JString): Boolean; cdecl;
    function installKeyPair(componentname: JComponentName; privatekey: JPrivateKey; certificate: JCertificate;
      string_1: JString): Boolean; overload; cdecl;
    function installKeyPair(componentname: JComponentName; privatekey: JPrivateKey; certificates: TJavaObjectArray<JCertificate>;
      string_1: JString; int: Integer): Boolean; overload; cdecl;
    function installKeyPair(componentname: JComponentName; privatekey: JPrivateKey; certificates: TJavaObjectArray<JCertificate>;
      string_1: JString; boolean: Boolean): Boolean; overload; cdecl;
    procedure installSystemUpdate(componentname: JComponentName; uri: Jnet_Uri; executor: JExecutor;
      installsystemupdatecallback: JDevicePolicyManager_InstallSystemUpdateCallback); cdecl;
    function isActivePasswordSufficient: Boolean; cdecl;
    function isActivePasswordSufficientForDeviceRequirement: Boolean; cdecl;
    function isAdminActive(componentname: JComponentName): Boolean; cdecl;
    function isAffiliatedUser: Boolean; cdecl;
    function isAlwaysOnVpnLockdownEnabled(componentname: JComponentName): Boolean; cdecl;
    function isApplicationHidden(componentname: JComponentName; string_1: JString): Boolean; cdecl;
    function isBackupServiceEnabled(componentname: JComponentName): Boolean; cdecl;
    function isCallerApplicationRestrictionsManagingPackage: Boolean; cdecl;
    function isCommonCriteriaModeEnabled(componentname: JComponentName): Boolean; cdecl;
    function isComplianceAcknowledgementRequired: Boolean; cdecl;
    function isDeviceFinanced: Boolean; cdecl;
    function isDeviceIdAttestationSupported: Boolean; cdecl;
    function isDeviceOwnerApp(string_1: JString): Boolean; cdecl;
    function isEphemeralUser(componentname: JComponentName): Boolean; cdecl;
    function isKeyPairGrantedToWifiAuth(string_1: JString): Boolean; cdecl;
    function isLockTaskPermitted(string_1: JString): Boolean; cdecl;
    function isLogoutEnabled: Boolean; cdecl;
    function isManagedProfile(componentname: JComponentName): Boolean; cdecl;
    function isMasterVolumeMuted(componentname: JComponentName): Boolean; cdecl;
    function isNetworkLoggingEnabled(componentname: JComponentName): Boolean; cdecl;
    function isOrganizationOwnedDeviceWithManagedProfile: Boolean; cdecl;
    function isOverrideApnEnabled(componentname: JComponentName): Boolean; cdecl;
    function isPackageSuspended(componentname: JComponentName; string_1: JString): Boolean; cdecl;
    function isPreferentialNetworkServiceEnabled: Boolean; cdecl;
    function isProfileOwnerApp(string_1: JString): Boolean; cdecl;
    function isProvisioningAllowed(string_1: JString): Boolean; cdecl;
    function isResetPasswordTokenActive(componentname: JComponentName): Boolean; cdecl;
    function isSafeOperation(int: Integer): Boolean; cdecl;
    function isSecurityLoggingEnabled(componentname: JComponentName): Boolean; cdecl;
    function isStatusBarDisabled: Boolean; cdecl;
    function isUninstallBlocked(componentname: JComponentName; string_1: JString): Boolean; cdecl;
    function isUniqueDeviceAttestationSupported: Boolean; cdecl;
    function isUsbDataSignalingEnabled: Boolean; cdecl;
    function isUsingUnifiedPassword(componentname: JComponentName): Boolean; cdecl;
    function listForegroundAffiliatedUsers: JList; cdecl;
    procedure lockNow; overload; cdecl;
    procedure lockNow(int: Integer); overload; cdecl;
    function logoutUser(componentname: JComponentName): Integer; cdecl;
    procedure reboot(componentname: JComponentName); cdecl;
    procedure removeActiveAdmin(componentname: JComponentName); cdecl;
    function removeCrossProfileWidgetProvider(componentname: JComponentName; string_1: JString): Boolean; cdecl;
    function removeKeyPair(componentname: JComponentName; string_1: JString): Boolean; cdecl;
    function removeOverrideApn(componentname: JComponentName; int: Integer): Boolean; cdecl;
    function removeUser(componentname: JComponentName; userhandle: JUserHandle): Boolean; cdecl;
    function requestBugreport(componentname: JComponentName): Boolean; cdecl;
    function resetPassword(string_1: JString; int: Integer): Boolean; cdecl;
    function resetPasswordWithToken(componentname: JComponentName; string_1: JString; bytes: TJavaArray<Byte>; int: Integer): Boolean; cdecl;
    function retrieveNetworkLogs(componentname: JComponentName; long: Int64): JList; cdecl;
    function retrievePreRebootSecurityLogs(componentname: JComponentName): JList; cdecl;
    function retrieveSecurityLogs(componentname: JComponentName): JList; cdecl;
    function revokeKeyPairFromApp(componentname: JComponentName; string_1: JString; string_2: JString): Boolean; cdecl;
    function revokeKeyPairFromWifiAuth(string_1: JString): Boolean; cdecl;
    procedure setAccountManagementDisabled(componentname: JComponentName; string_1: JString; boolean: Boolean); cdecl;
    procedure setAffiliationIds(componentname: JComponentName; set_1: JSet); cdecl;
    procedure setAlwaysOnVpnPackage(componentname: JComponentName; string_1: JString; boolean: Boolean; set_1: JSet); overload; cdecl;
    procedure setAlwaysOnVpnPackage(componentname: JComponentName; string_1: JString; boolean: Boolean); overload; cdecl;
    function setApplicationHidden(componentname: JComponentName; string_1: JString; boolean: Boolean): Boolean; cdecl;
    procedure setApplicationRestrictions(componentname: JComponentName; string_1: JString; bundle: JBundle); cdecl;
    procedure setApplicationRestrictionsManagingPackage(componentname: JComponentName; string_1: JString); cdecl;
    procedure setAutoTimeEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setAutoTimeRequired(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setAutoTimeZoneEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setBackupServiceEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setBluetoothContactSharingDisabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setCameraDisabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setCertInstallerPackage(componentname: JComponentName; string_1: JString); cdecl;
    procedure setCommonCriteriaModeEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setConfiguredNetworksLockdownState(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setContentProtectionPolicy(componentname: JComponentName; int: Integer); cdecl;
    procedure setCredentialManagerPolicy(packagepolicy: JPackagePolicy); cdecl;
    procedure setCrossProfileCalendarPackages(componentname: JComponentName; set_1: JSet); cdecl;
    procedure setCrossProfileCallerIdDisabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setCrossProfileContactsSearchDisabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setCrossProfilePackages(componentname: JComponentName; set_1: JSet); cdecl;
    procedure setDefaultDialerApplication(string_1: JString); cdecl;
    procedure setDefaultSmsApplication(componentname: JComponentName; string_1: JString); cdecl;
    procedure setDelegatedScopes(componentname: JComponentName; string_1: JString; list: JList); cdecl;
    procedure setDeviceOwnerLockScreenInfo(componentname: JComponentName; charsequence: JCharSequence); cdecl;
    procedure setEndUserSessionMessage(componentname: JComponentName; charsequence: JCharSequence); cdecl;
    procedure setFactoryResetProtectionPolicy(componentname: JComponentName; factoryresetprotectionpolicy: JFactoryResetProtectionPolicy); cdecl;
    function setGlobalPrivateDnsModeOpportunistic(componentname: JComponentName): Integer; cdecl;
    function setGlobalPrivateDnsModeSpecifiedHost(componentname: JComponentName; string_1: JString): Integer; cdecl;
    procedure setGlobalSetting(componentname: JComponentName; string_1: JString; string_2: JString); cdecl;
    procedure setKeepUninstalledPackages(componentname: JComponentName; list: JList); cdecl;
    function setKeyPairCertificate(componentname: JComponentName; string_1: JString; list: JList; boolean: Boolean): Boolean; cdecl;
    function setKeyguardDisabled(componentname: JComponentName; boolean: Boolean): Boolean; cdecl;
    procedure setKeyguardDisabledFeatures(componentname: JComponentName; int: Integer); cdecl;
    procedure setLocationEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setLockTaskFeatures(componentname: JComponentName; int: Integer); cdecl;
    procedure setLockTaskPackages(componentname: JComponentName; strings: TJavaObjectArray<JString>); cdecl;
    procedure setLogoutEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setLongSupportMessage(componentname: JComponentName; charsequence: JCharSequence); cdecl;
    procedure setManagedProfileCallerIdAccessPolicy(packagepolicy: JPackagePolicy); cdecl;
    procedure setManagedProfileContactsAccessPolicy(packagepolicy: JPackagePolicy); cdecl;
    procedure setManagedProfileMaximumTimeOff(componentname: JComponentName; long: Int64); cdecl;
    procedure setManagedSubscriptionsPolicy(managedsubscriptionspolicy: JManagedSubscriptionsPolicy); cdecl;
    procedure setMasterVolumeMuted(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setMaximumFailedPasswordsForWipe(componentname: JComponentName; int: Integer); cdecl;
    procedure setMaximumTimeToLock(componentname: JComponentName; long: Int64); cdecl;
    function setMeteredDataDisabledPackages(componentname: JComponentName; list: JList): JList; cdecl;
    procedure setMinimumRequiredWifiSecurityLevel(int: Integer); cdecl;
    procedure setMtePolicy(int: Integer); cdecl;
    procedure setNearbyAppStreamingPolicy(int: Integer); cdecl;
    procedure setNearbyNotificationStreamingPolicy(int: Integer); cdecl;
    procedure setNetworkLoggingEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setOrganizationColor(componentname: JComponentName; int: Integer); cdecl;
    procedure setOrganizationId(string_1: JString); cdecl;
    procedure setOrganizationName(componentname: JComponentName; charsequence: JCharSequence); cdecl;
    procedure setOverrideApnsEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    function setPackagesSuspended(componentname: JComponentName; strings: TJavaObjectArray<JString>; boolean: Boolean): TJavaObjectArray<JString>; cdecl;
    procedure setPasswordExpirationTimeout(componentname: JComponentName; long: Int64); cdecl;
    procedure setPasswordHistoryLength(componentname: JComponentName; int: Integer); cdecl;
    procedure setPasswordMinimumLength(componentname: JComponentName; int: Integer); cdecl;
    procedure setPasswordMinimumLetters(componentname: JComponentName; int: Integer); cdecl;
    procedure setPasswordMinimumLowerCase(componentname: JComponentName; int: Integer); cdecl;
    procedure setPasswordMinimumNonLetter(componentname: JComponentName; int: Integer); cdecl;
    procedure setPasswordMinimumNumeric(componentname: JComponentName; int: Integer); cdecl;
    procedure setPasswordMinimumSymbols(componentname: JComponentName; int: Integer); cdecl;
    procedure setPasswordMinimumUpperCase(componentname: JComponentName; int: Integer); cdecl;
    procedure setPasswordQuality(componentname: JComponentName; int: Integer); cdecl;
    function setPermissionGrantState(componentname: JComponentName; string_1: JString; string_2: JString; int: Integer): Boolean; cdecl;
    procedure setPermissionPolicy(componentname: JComponentName; int: Integer); cdecl;
    function setPermittedAccessibilityServices(componentname: JComponentName; list: JList): Boolean; cdecl;
    function setPermittedCrossProfileNotificationListeners(componentname: JComponentName; list: JList): Boolean; cdecl;
    function setPermittedInputMethods(componentname: JComponentName; list: JList): Boolean; cdecl;
    procedure setPersonalAppsSuspended(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setPreferentialNetworkServiceConfigs(list: JList); cdecl;
    procedure setPreferentialNetworkServiceEnabled(boolean: Boolean); cdecl;
    procedure setProfileEnabled(componentname: JComponentName); cdecl;
    procedure setProfileName(componentname: JComponentName; string_1: JString); cdecl;
    procedure setRecommendedGlobalProxy(componentname: JComponentName; proxyinfo: JProxyInfo); cdecl;
    procedure setRequiredPasswordComplexity(int: Integer); cdecl;
    procedure setRequiredStrongAuthTimeout(componentname: JComponentName; long: Int64); cdecl;
    function setResetPasswordToken(componentname: JComponentName; bytes: TJavaArray<Byte>): Boolean; cdecl;
    procedure setRestrictionsProvider(componentname: JComponentName; componentname_1: JComponentName); cdecl;
    procedure setScreenCaptureDisabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setSecureSetting(componentname: JComponentName; string_1: JString; string_2: JString); cdecl;
    procedure setSecurityLoggingEnabled(componentname: JComponentName; boolean: Boolean); cdecl;
    procedure setShortSupportMessage(componentname: JComponentName; charsequence: JCharSequence); cdecl;
    procedure setStartUserSessionMessage(componentname: JComponentName; charsequence: JCharSequence); cdecl;
    function setStatusBarDisabled(componentname: JComponentName; boolean: Boolean): Boolean; cdecl;
    function setStorageEncryption(componentname: JComponentName; boolean: Boolean): Integer; cdecl;
    procedure setSystemSetting(componentname: JComponentName; string_1: JString; string_2: JString); cdecl;
    procedure setSystemUpdatePolicy(componentname: JComponentName; systemupdatepolicy: JSystemUpdatePolicy); cdecl;
    function setTime(componentname: JComponentName; long: Int64): Boolean; cdecl;
    function setTimeZone(componentname: JComponentName; string_1: JString): Boolean; cdecl;
    procedure setTrustAgentConfiguration(componentname: JComponentName; componentname_1: JComponentName; persistablebundle: JPersistableBundle); cdecl;
    procedure setUninstallBlocked(componentname: JComponentName; string_1: JString; boolean: Boolean); cdecl;
    procedure setUsbDataSignalingEnabled(boolean: Boolean); cdecl;
    procedure setUserControlDisabledPackages(componentname: JComponentName; list: JList); cdecl;
    procedure setUserIcon(componentname: JComponentName; bitmap: JBitmap); cdecl;
    procedure setWifiSsidPolicy(wifissidpolicy: JWifiSsidPolicy); cdecl;
    function startUserInBackground(componentname: JComponentName; userhandle: JUserHandle): Integer; cdecl;
    function stopUser(componentname: JComponentName; userhandle: JUserHandle): Integer; cdecl;
    function switchUser(componentname: JComponentName; userhandle: JUserHandle): Boolean; cdecl;
    procedure transferOwnership(componentname: JComponentName; componentname_1: JComponentName; persistablebundle: JPersistableBundle); cdecl;
    procedure uninstallAllUserCaCerts(componentname: JComponentName); cdecl;
    procedure uninstallCaCert(componentname: JComponentName; bytes: TJavaArray<Byte>); cdecl;
    function updateOverrideApn(componentname: JComponentName; int: Integer; apnsetting: JApnSetting): Boolean; cdecl;
    procedure wipeData(int: Integer; charsequence: JCharSequence); overload; cdecl;
    procedure wipeData(int: Integer); overload; cdecl;
    procedure wipeDevice(int: Integer); cdecl;
  end;
  TJDevicePolicyManager = class(TJavaGenericImport<JDevicePolicyManagerClass, JDevicePolicyManager>) end;

  JDevicePolicyManager_InstallSystemUpdateCallbackClass = interface(JObjectClass)
    ['{076F1600-7977-4C28-A5BF-EB9B2ACD19F7}']
    {class} function _GetUPDATE_ERROR_BATTERY_LOW: Integer; cdecl;
    {class} function _GetUPDATE_ERROR_FILE_NOT_FOUND: Integer; cdecl;
    {class} function _GetUPDATE_ERROR_INCORRECT_OS_VERSION: Integer; cdecl;
    {class} function _GetUPDATE_ERROR_UNKNOWN: Integer; cdecl;
    {class} function _GetUPDATE_ERROR_UPDATE_FILE_INVALID: Integer; cdecl;
    {class} function init: JDevicePolicyManager_InstallSystemUpdateCallback; cdecl;
    {class} property UPDATE_ERROR_BATTERY_LOW: Integer read _GetUPDATE_ERROR_BATTERY_LOW;
    {class} property UPDATE_ERROR_FILE_NOT_FOUND: Integer read _GetUPDATE_ERROR_FILE_NOT_FOUND;
    {class} property UPDATE_ERROR_INCORRECT_OS_VERSION: Integer read _GetUPDATE_ERROR_INCORRECT_OS_VERSION;
    {class} property UPDATE_ERROR_UNKNOWN: Integer read _GetUPDATE_ERROR_UNKNOWN;
    {class} property UPDATE_ERROR_UPDATE_FILE_INVALID: Integer read _GetUPDATE_ERROR_UPDATE_FILE_INVALID;
  end;

  [JavaSignature('android/app/admin/DevicePolicyManager$InstallSystemUpdateCallback')]
  JDevicePolicyManager_InstallSystemUpdateCallback = interface(JObject)
    ['{48934C48-FC8A-4D11-8A9A-BD8B39E8B424}']
    procedure onInstallUpdateError(int: Integer; string_1: JString); cdecl;
  end;
  TJDevicePolicyManager_InstallSystemUpdateCallback = class(TJavaGenericImport<JDevicePolicyManager_InstallSystemUpdateCallbackClass,
    JDevicePolicyManager_InstallSystemUpdateCallback>) end;

  JDevicePolicyResourcesManagerClass = interface(JObjectClass)
    ['{0BC2BD06-32FA-44D5-81FA-19FA673A3971}']
  end;

  [JavaSignature('android/app/admin/DevicePolicyResourcesManager')]
  JDevicePolicyResourcesManager = interface(JObject)
    ['{BD71E94A-A692-4394-9658-12520EA3DF5D}']
    function getDrawable(string_1: JString; string_2: JString; string_3: JString; supplier: JSupplier): JDrawable; overload; cdecl;
    function getDrawable(string_1: JString; string_2: JString; supplier: JSupplier): JDrawable; overload; cdecl;
    function getDrawableAsIcon(string_1: JString; string_2: JString; string_3: JString; icon: JIcon): JIcon; overload; cdecl;
    function getDrawableAsIcon(string_1: JString; string_2: JString; icon: JIcon): JIcon; overload; cdecl;
    function getDrawableForDensity(string_1: JString; string_2: JString; int: Integer; supplier: JSupplier): JDrawable; overload; cdecl;
    function getDrawableForDensity(string_1: JString; string_2: JString; string_3: JString; int: Integer; supplier: JSupplier): JDrawable; overload; cdecl;
    function getString(string_1: JString; supplier: JSupplier; objects: TJavaObjectArray<JObject>): JString; overload; cdecl;
    function getString(string_1: JString; supplier: JSupplier): JString; overload; cdecl;
  end;
  TJDevicePolicyResourcesManager = class(TJavaGenericImport<JDevicePolicyResourcesManagerClass, JDevicePolicyResourcesManager>) end;

  JFactoryResetProtectionPolicyClass = interface(JObjectClass)
    ['{6F005AA7-FCF9-49AD-95D9-303D5A2DD3C4}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/app/admin/FactoryResetProtectionPolicy')]
  JFactoryResetProtectionPolicy = interface(JObject)
    ['{02037A16-BB56-4A9A-88B3-46939BBED106}']
    function describeContents: Integer; cdecl;
    function getFactoryResetProtectionAccounts: JList; cdecl;
    function isFactoryResetProtectionEnabled: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJFactoryResetProtectionPolicy = class(TJavaGenericImport<JFactoryResetProtectionPolicyClass, JFactoryResetProtectionPolicy>) end;

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

  JManagedSubscriptionsPolicyClass = interface(JObjectClass)
    ['{F5E00FAD-3819-45D2-AF16-2120BE884086}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetTYPE_ALL_MANAGED_SUBSCRIPTIONS: Integer; cdecl;
    {class} function _GetTYPE_ALL_PERSONAL_SUBSCRIPTIONS: Integer; cdecl;
    {class} function init(int: Integer): JManagedSubscriptionsPolicy; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property TYPE_ALL_MANAGED_SUBSCRIPTIONS: Integer read _GetTYPE_ALL_MANAGED_SUBSCRIPTIONS;
    {class} property TYPE_ALL_PERSONAL_SUBSCRIPTIONS: Integer read _GetTYPE_ALL_PERSONAL_SUBSCRIPTIONS;
  end;

  [JavaSignature('android/app/admin/ManagedSubscriptionsPolicy')]
  JManagedSubscriptionsPolicy = interface(JObject)
    ['{46091C4B-44B3-4B29-A5A5-A58C38570A9A}']
    function describeContents: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getPolicyType: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJManagedSubscriptionsPolicy = class(TJavaGenericImport<JManagedSubscriptionsPolicyClass, JManagedSubscriptionsPolicy>) end;

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

  JPackagePolicyClass = interface(JObjectClass)
    ['{7DED0B32-C032-4E1D-8288-DA2C62AF9525}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetPACKAGE_POLICY_ALLOWLIST: Integer; cdecl;
    {class} function _GetPACKAGE_POLICY_ALLOWLIST_AND_SYSTEM: Integer; cdecl;
    {class} function _GetPACKAGE_POLICY_BLOCKLIST: Integer; cdecl;
    {class} function init(int: Integer): JPackagePolicy; overload; cdecl;
    {class} function init(int: Integer; set_1: JSet): JPackagePolicy; overload; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property PACKAGE_POLICY_ALLOWLIST: Integer read _GetPACKAGE_POLICY_ALLOWLIST;
    {class} property PACKAGE_POLICY_ALLOWLIST_AND_SYSTEM: Integer read _GetPACKAGE_POLICY_ALLOWLIST_AND_SYSTEM;
    {class} property PACKAGE_POLICY_BLOCKLIST: Integer read _GetPACKAGE_POLICY_BLOCKLIST;
  end;

  [JavaSignature('android/app/admin/PackagePolicy')]
  JPackagePolicy = interface(JObject)
    ['{14667962-8A97-4EB5-AD5A-26EDC74BB70F}']
    function describeContents: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getPackageNames: JSet; cdecl;
    function getPolicyType: Integer; cdecl;
    function hashCode: Integer; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJPackagePolicy = class(TJavaGenericImport<JPackagePolicyClass, JPackagePolicy>) end;

  JSystemUpdateInfoClass = interface(JObjectClass)
    ['{BD3B306D-A659-437C-AABB-563CFD5A6FB3}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetSECURITY_PATCH_STATE_FALSE: Integer; cdecl;
    {class} function _GetSECURITY_PATCH_STATE_TRUE: Integer; cdecl;
    {class} function _GetSECURITY_PATCH_STATE_UNKNOWN: Integer; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property SECURITY_PATCH_STATE_FALSE: Integer read _GetSECURITY_PATCH_STATE_FALSE;
    {class} property SECURITY_PATCH_STATE_TRUE: Integer read _GetSECURITY_PATCH_STATE_TRUE;
    {class} property SECURITY_PATCH_STATE_UNKNOWN: Integer read _GetSECURITY_PATCH_STATE_UNKNOWN;
  end;

  [JavaSignature('android/app/admin/SystemUpdateInfo')]
  JSystemUpdateInfo = interface(JObject)
    ['{DDE39DEE-33F7-4EAE-9770-271704100EB6}']
    function describeContents: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getReceivedTime: Int64; cdecl;
    function getSecurityPatchState: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJSystemUpdateInfo = class(TJavaGenericImport<JSystemUpdateInfoClass, JSystemUpdateInfo>) end;

  JSystemUpdatePolicyClass = interface(JObjectClass)
    ['{0572781A-2125-4643-9A52-8B707B578BD6}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetTYPE_INSTALL_AUTOMATIC: Integer; cdecl;
    {class} function _GetTYPE_INSTALL_WINDOWED: Integer; cdecl;
    {class} function _GetTYPE_POSTPONE: Integer; cdecl;
    {class} function createAutomaticInstallPolicy: JSystemUpdatePolicy; cdecl;
    {class} function createPostponeInstallPolicy: JSystemUpdatePolicy; cdecl;
    {class} function createWindowedInstallPolicy(int: Integer; int_1: Integer): JSystemUpdatePolicy; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property TYPE_INSTALL_AUTOMATIC: Integer read _GetTYPE_INSTALL_AUTOMATIC;
    {class} property TYPE_INSTALL_WINDOWED: Integer read _GetTYPE_INSTALL_WINDOWED;
    {class} property TYPE_POSTPONE: Integer read _GetTYPE_POSTPONE;
  end;

  [JavaSignature('android/app/admin/SystemUpdatePolicy')]
  JSystemUpdatePolicy = interface(JObject)
    ['{CC5B958C-88D3-434E-B091-4545857C7018}']
    function describeContents: Integer; cdecl;
    function getFreezePeriods: JList; cdecl;
    function getInstallWindowEnd: Integer; cdecl;
    function getInstallWindowStart: Integer; cdecl;
    function getPolicyType: Integer; cdecl;
    function setFreezePeriods(list: JList): JSystemUpdatePolicy; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJSystemUpdatePolicy = class(TJavaGenericImport<JSystemUpdatePolicyClass, JSystemUpdatePolicy>) end;

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

  JWifiSsidPolicyClass = interface(JObjectClass)
    ['{3070DF6D-A20A-4BC9-94EE-EFB7D2D8C586}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetWIFI_SSID_POLICY_TYPE_ALLOWLIST: Integer; cdecl;
    {class} function _GetWIFI_SSID_POLICY_TYPE_DENYLIST: Integer; cdecl;
    {class} function init(int: Integer; set_1: JSet): JWifiSsidPolicy; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property WIFI_SSID_POLICY_TYPE_ALLOWLIST: Integer read _GetWIFI_SSID_POLICY_TYPE_ALLOWLIST;
    {class} property WIFI_SSID_POLICY_TYPE_DENYLIST: Integer read _GetWIFI_SSID_POLICY_TYPE_DENYLIST;
  end;

  [JavaSignature('android/app/admin/WifiSsidPolicy')]
  JWifiSsidPolicy = interface(JObject)
    ['{549C8B13-73BD-40BC-B96E-01F4FFD6C55C}']
    function describeContents: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getPolicyType: Integer; cdecl;
    function getSsids: JSet; cdecl;
    function hashCode: Integer; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJWifiSsidPolicy = class(TJavaGenericImport<JWifiSsidPolicyClass, JWifiSsidPolicy>) end;

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
