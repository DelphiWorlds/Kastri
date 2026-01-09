unit DW.Androidapi.JNI.PlayServices.AppUpdate;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, AndroidApi.JNI.PlayServices.Tasks, Androidapi.JNI.App,
  Androidapi.JNI.Support, Androidapi.JNI.Os;

type
  JActivityResult = interface;
  JAppUpdateInfo = interface;
  JAppUpdateManager = interface;
  JAppUpdateManagerFactory = interface;
  JAppUpdateOptions = interface;
  JAppUpdateOptions_Builder = interface;
  JAppUpdateType = interface;
  JFakeAppUpdateManager = interface;
  JInstallErrorCode = interface;
  JInstallState = interface;
  JInstallStateUpdatedListener = interface;
  JInstallStatus = interface;
  JIntentSenderForResultStarter = interface;
  JStateUpdatedListener = interface;
  JUpdateAvailability = interface;
  JUpdatePrecondition = interface;

  JUpdatePreconditionClass = interface(JAnnotationClass)
    ['{147BDB29-8011-4A5E-9ECD-0D22054B80A4}']
    {class} function _GetAPP_VERSION_FRESH: Integer; cdecl;
    {class} function _GetCANNOT_DISPLAY: Integer; cdecl;
    {class} function _GetDEVICE_STATUS: Integer; cdecl;
    {class} function _GetINSUFFICIENT_STORAGE: Integer; cdecl;
    {class} function _GetNEED_STORE_TO_PROCEED: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} property APP_VERSION_FRESH: Integer read _GetAPP_VERSION_FRESH;
    {class} property CANNOT_DISPLAY: Integer read _GetCANNOT_DISPLAY;
    {class} property DEVICE_STATUS: Integer read _GetDEVICE_STATUS;
    {class} property INSUFFICIENT_STORAGE: Integer read _GetINSUFFICIENT_STORAGE;
    {class} property NEED_STORE_TO_PROCEED: Integer read _GetNEED_STORE_TO_PROCEED;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
  end;

  [JavaSignature('com/google/android/play/core/install/model/UpdatePrecondition')]
  JUpdatePrecondition = interface(JAnnotation)
    ['{5C197367-D4B7-4606-B9E6-A7F92BAF7FD9}']
  end;
  TJUpdatePrecondition = class(TJavaGenericImport<JUpdatePreconditionClass, JUpdatePrecondition>) end;

  JUpdateAvailabilityClass = interface(JAnnotationClass)
    ['{E97E76FB-8E52-4AB3-A618-A463DFCF96B7}']
    {class} function _GetDEVELOPER_TRIGGERED_UPDATE_IN_PROGRESS: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} function _GetUPDATE_AVAILABLE: Integer; cdecl;
    {class} function _GetUPDATE_NOT_AVAILABLE: Integer; cdecl;
    {class} property DEVELOPER_TRIGGERED_UPDATE_IN_PROGRESS: Integer read _GetDEVELOPER_TRIGGERED_UPDATE_IN_PROGRESS;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
    {class} property UPDATE_AVAILABLE: Integer read _GetUPDATE_AVAILABLE;
    {class} property UPDATE_NOT_AVAILABLE: Integer read _GetUPDATE_NOT_AVAILABLE;
  end;

  [JavaSignature('com/google/android/play/core/install/model/UpdateAvailability')]
  JUpdateAvailability = interface(JAnnotation)
    ['{E5676E05-5B37-4278-B49B-88A329CB0327}']
  end;
  TJUpdateAvailability = class(TJavaGenericImport<JUpdateAvailabilityClass, JUpdateAvailability>) end;

  JInstallStatusClass = interface(JAnnotationClass)
    ['{3908F33F-6A60-4340-A396-65D8DCCD5F37}']
    {class} function _GetCANCELED: Integer; cdecl;
    {class} function _GetDOWNLOADED: Integer; cdecl;
    {class} function _GetDOWNLOADING: Integer; cdecl;
    {class} function _GetFAILED: Integer; cdecl;
    {class} function _GetINSTALLED: Integer; cdecl;
    {class} function _GetINSTALLING: Integer; cdecl;
    {class} function _GetPENDING: Integer; cdecl;
    {class} function _GetREQUIRES_UI_INTENT: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} property CANCELED: Integer read _GetCANCELED;
    {class} property DOWNLOADED: Integer read _GetDOWNLOADED;
    {class} property DOWNLOADING: Integer read _GetDOWNLOADING;
    {class} property FAILED: Integer read _GetFAILED;
    {class} property INSTALLED: Integer read _GetINSTALLED;
    {class} property INSTALLING: Integer read _GetINSTALLING;
    {class} property PENDING: Integer read _GetPENDING;
    {class} property REQUIRES_UI_INTENT: Integer read _GetREQUIRES_UI_INTENT;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
  end;

  [JavaSignature('com/google/android/play/core/install/model/InstallStatus')]
  JInstallStatus = interface(JAnnotation)
    ['{EE919E0D-A000-4809-8BD3-63980B17512D}']
  end;
  TJInstallStatus = class(TJavaGenericImport<JInstallStatusClass, JInstallStatus>) end;

  JInstallErrorCodeClass = interface(JAnnotationClass)
    ['{C3320DB2-8F99-49E5-A038-A91663779388}']
    {class} function _GetERROR_API_NOT_AVAILABLE: Integer; cdecl;
    {class} function _GetERROR_APP_NOT_OWNED: Integer; cdecl;
    {class} function _GetERROR_DOWNLOAD_NOT_PRESENT: Integer; cdecl;
    {class} function _GetERROR_INSTALL_NOT_ALLOWED: Integer; cdecl;
    {class} function _GetERROR_INSTALL_UNAVAILABLE: Integer; cdecl;
    {class} function _GetERROR_INTERNAL_ERROR: Integer; cdecl;
    {class} function _GetERROR_INVALID_REQUEST: Integer; cdecl;
    {class} function _GetERROR_PLAY_STORE_NOT_FOUND: Integer; cdecl;
    {class} function _GetERROR_UNKNOWN: Integer; cdecl;
    {class} function _GetNO_ERROR: Integer; cdecl;
    {class} function _GetNO_ERROR_PARTIALLY_ALLOWED: Integer; cdecl;
    {class} property ERROR_API_NOT_AVAILABLE: Integer read _GetERROR_API_NOT_AVAILABLE;
    {class} property ERROR_APP_NOT_OWNED: Integer read _GetERROR_APP_NOT_OWNED;
    {class} property ERROR_DOWNLOAD_NOT_PRESENT: Integer read _GetERROR_DOWNLOAD_NOT_PRESENT;
    {class} property ERROR_INSTALL_NOT_ALLOWED: Integer read _GetERROR_INSTALL_NOT_ALLOWED;
    {class} property ERROR_INSTALL_UNAVAILABLE: Integer read _GetERROR_INSTALL_UNAVAILABLE;
    {class} property ERROR_INTERNAL_ERROR: Integer read _GetERROR_INTERNAL_ERROR;
    {class} property ERROR_INVALID_REQUEST: Integer read _GetERROR_INVALID_REQUEST;
    {class} property ERROR_PLAY_STORE_NOT_FOUND: Integer read _GetERROR_PLAY_STORE_NOT_FOUND;
    {class} property ERROR_UNKNOWN: Integer read _GetERROR_UNKNOWN;
    {class} property NO_ERROR: Integer read _GetNO_ERROR;
    {class} property NO_ERROR_PARTIALLY_ALLOWED: Integer read _GetNO_ERROR_PARTIALLY_ALLOWED;
  end;

  [JavaSignature('com/google/android/play/core/install/model/InstallErrorCode')]
  JInstallErrorCode = interface(JAnnotation)
    ['{593441DE-3413-4BAA-B2F5-2A364BF6333C}']
  end;
  TJInstallErrorCode = class(TJavaGenericImport<JInstallErrorCodeClass, JInstallErrorCode>) end;

  JAppUpdateTypeClass = interface(JAnnotationClass)
    ['{5A55CBA8-7254-474E-83B2-F97FC1A8C85B}']
    {class} function _GetFLEXIBLE: Integer; cdecl;
    {class} function _GetIMMEDIATE: Integer; cdecl;
    {class} property FLEXIBLE: Integer read _GetFLEXIBLE;
    {class} property IMMEDIATE: Integer read _GetIMMEDIATE;
  end;

  [JavaSignature('com/google/android/play/core/install/model/AppUpdateType')]
  JAppUpdateType = interface(JAnnotation)
    ['{A18A7A88-92F7-435C-B8D1-728DA865A49D}']
  end;
  TJAppUpdateType = class(TJavaGenericImport<JAppUpdateTypeClass, JAppUpdateType>) end;

  JActivityResultClass = interface(JObjectClass)
    ['{1C787975-DBC7-4CF9-B6C0-76A84127108C}']
    {class} function _GetRESULT_IN_APP_UPDATE_FAILED: Integer; cdecl;
    {class} property RESULT_IN_APP_UPDATE_FAILED: Integer read _GetRESULT_IN_APP_UPDATE_FAILED;
  end;

  [JavaSignature('com/google/android/play/core/install/model/ActivityResult')]
  JActivityResult = interface(JObject)
    ['{200D05D0-2D07-401F-984B-D6193182A683}']
  end;
  TJActivityResult = class(TJavaGenericImport<JActivityResultClass, JActivityResult>) end;

  JStateUpdatedListenerClass = interface(IJavaClass)
    ['{BC9C42E8-13AB-4BD5-A3AB-DACA8B5B0A6C}']
  end;

  [JavaSignature('com/google/android/play/core/listener/StateUpdatedListener')]
  JStateUpdatedListener = interface(IJavaInstance)
    ['{77BA1EEE-82AC-4905-9B87-1E3215301A5B}']
  end;
  TJStateUpdatedListener = class(TJavaGenericImport<JStateUpdatedListenerClass, JStateUpdatedListener>) end;

  JInstallStateUpdatedListenerClass = interface(JStateUpdatedListenerClass)
    ['{15ACA850-AC05-410E-B37D-D327DF780A90}']
  end;

  [JavaSignature('com/google/android/play/core/install/InstallStateUpdatedListener')]
  JInstallStateUpdatedListener = interface(JStateUpdatedListener)
    ['{D08275AF-7393-4A34-ACF3-2F04C3B14D07}']
  end;
  TJInstallStateUpdatedListener = class(TJavaGenericImport<JInstallStateUpdatedListenerClass, JInstallStateUpdatedListener>) end;

  JInstallStateClass = interface(JObjectClass)
    ['{D4AF63F7-1332-45B2-822A-66E158BB92FD}']
    {class} function init: JInstallState; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/install/InstallState')]
  JInstallState = interface(JObject)
    ['{2AF827B3-50D8-4365-9649-44621EF7ED51}']
    function bytesDownloaded: Int64; cdecl;
    function installErrorCode: Integer; cdecl;
    function installStatus: Integer; cdecl;
    function packageName: JString; cdecl;
    function totalBytesToDownload: Int64; cdecl;
  end;
  TJInstallState = class(TJavaGenericImport<JInstallStateClass, JInstallState>) end;

  JIntentSenderForResultStarterClass = interface(IJavaClass)
    ['{8629E554-C550-43A2-85A3-AA329981C10C}']
  end;

  [JavaSignature('com/google/android/play/core/common/IntentSenderForResultStarter')]
  JIntentSenderForResultStarter = interface(IJavaInstance)
    ['{33155FAE-8272-4B6A-A2AA-DDCEC4C5AF53}']
    procedure startIntentSenderForResult(intent: JIntentSender; requestCode: Integer; fillInIntent: JIntent; flagsMask: Integer;
      flagsValues: Integer; extraFlags: Integer; options: JBundle); cdecl;
  end;
  TJIntentSenderForResultStarter = class(TJavaGenericImport<JIntentSenderForResultStarterClass, JIntentSenderForResultStarter>) end;

  JFakeAppUpdateManagerClass = interface(JObjectClass)
    ['{E9695EAF-68AA-4470-B5EE-56F2FEB7449E}']
    {class} function init(context: JContext): JFakeAppUpdateManager; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/testing/FakeAppUpdateManager')]
  JFakeAppUpdateManager = interface(JObject)
    ['{C7FC782E-4F4A-4F36-967E-18356471A9D8}']
    function completeUpdate: JTask; cdecl;
    procedure downloadCompletes; cdecl;
    procedure downloadFails; cdecl;
    procedure downloadStarts; cdecl;
    function getAppUpdateInfo: JTask; cdecl;
    function getTypeForUpdateInProgress: JInteger; cdecl;
    procedure installCompletes; cdecl;
    procedure installFails; cdecl;
    function isConfirmationDialogVisible: Boolean; cdecl;
    function isImmediateFlowVisible: Boolean; cdecl;
    function isInstallSplashScreenVisible: Boolean; cdecl;
    procedure registerListener(installStateUpdatedListener: JInstallStateUpdatedListener); cdecl;
    procedure setBytesDownloaded(long: Int64); cdecl;
    procedure setClientVersionStalenessDays(integer: JInteger); cdecl;
    procedure setInstallErrorCode(int: Integer); cdecl;
    procedure setTotalBytesToDownload(long: Int64); cdecl;
    procedure setUpdateAvailable(int: Integer); overload; cdecl;
    procedure setUpdateAvailable(int: Integer; int_1: Integer); overload; cdecl;
    procedure setUpdateNotAvailable; cdecl;
    procedure setUpdatePriority(int: Integer); cdecl;
    function startUpdateFlow(appUpdateInfo: JAppUpdateInfo; activity: JActivity; appUpdateOptions: JAppUpdateOptions): JTask; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; int: Integer; activity: JActivity; int_1: Integer): Boolean; overload; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; int: Integer; intentSenderForResultStarter: JIntentSenderForResultStarter;
      int_1: Integer): Boolean; overload; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; activity: JActivity; appUpdateOptions: JAppUpdateOptions;
      int: Integer): Boolean; overload; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; intentSenderForResultStarter: JIntentSenderForResultStarter;
      appUpdateOptions: JAppUpdateOptions; int: Integer): Boolean; overload; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; activityResultLauncher: JActivityResultLauncher;
      appUpdateOptions: JAppUpdateOptions): Boolean; overload; cdecl;
    procedure unregisterListener(installStateUpdatedListener: JInstallStateUpdatedListener); cdecl;
    procedure userAcceptsUpdate; cdecl;
    procedure userCancelsDownload; cdecl;
    procedure userRejectsUpdate; cdecl;
  end;
  TJFakeAppUpdateManager = class(TJavaGenericImport<JFakeAppUpdateManagerClass, JFakeAppUpdateManager>) end;

  JAppUpdateOptionsClass = interface(JObjectClass)
    ['{0F1B5893-3F5F-4A41-80E9-4932C5B6F6EB}']
    {class} function defaultOptions(int: Integer): JAppUpdateOptions; cdecl;
    {class} function init: JAppUpdateOptions; cdecl;
    {class} function newBuilder(int: Integer): JAppUpdateOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateOptions')]
  JAppUpdateOptions = interface(JObject)
    ['{E4B19526-BAB0-48AF-BF13-0BA02433299E}']
    function allowAssetPackDeletion: Boolean; cdecl;
    function appUpdateType: Integer; cdecl;
  end;
  TJAppUpdateOptions = class(TJavaGenericImport<JAppUpdateOptionsClass, JAppUpdateOptions>) end;

  JAppUpdateOptions_BuilderClass = interface(JObjectClass)
    ['{21B1FBCC-7E7C-4ED4-8A28-DA744D26F770}']
    {class} function init: JAppUpdateOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateOptions$Builder')]
  JAppUpdateOptions_Builder = interface(JObject)
    ['{8B7FD54F-D7ED-4AF1-9A80-01AF665422BA}']
    function build: JAppUpdateOptions; cdecl;
    function setAllowAssetPackDeletion(boolean: Boolean): JAppUpdateOptions_Builder; cdecl;
    function setAppUpdateType(int: Integer): JAppUpdateOptions_Builder; cdecl;
  end;
  TJAppUpdateOptions_Builder = class(TJavaGenericImport<JAppUpdateOptions_BuilderClass, JAppUpdateOptions_Builder>) end;

  JAppUpdateManagerFactoryClass = interface(JObjectClass)
    ['{F3B0B192-5A26-40BE-9514-6E38A175B044}']
    {class} function create(context: JContext): JAppUpdateManager; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateManagerFactory')]
  JAppUpdateManagerFactory = interface(JObject)
    ['{4EE6C745-521F-4DAB-9535-44BBA53A25D2}']
  end;
  TJAppUpdateManagerFactory = class(TJavaGenericImport<JAppUpdateManagerFactoryClass, JAppUpdateManagerFactory>) end;

  JAppUpdateManagerClass = interface(IJavaClass)
    ['{F68F1028-9561-4EB3-BED5-C1BFA66517C6}']
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateManager')]
  JAppUpdateManager = interface(IJavaInstance)
    ['{9B8AFBEC-0D52-4C5F-91FC-CD1A9C150855}']
    function completeUpdate: JTask; cdecl;
    function getAppUpdateInfo: JTask; cdecl;
    procedure registerListener(installStateUpdatedListener: JInstallStateUpdatedListener); cdecl;
    function startUpdateFlow(appUpdateInfo: JAppUpdateInfo; activity: JActivity; appUpdateOptions: JAppUpdateOptions): JTask; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; int: Integer; intentSenderForResultStarter: JIntentSenderForResultStarter;
      int_1: Integer): Boolean; overload; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; activity: JActivity; appUpdateOptions: JAppUpdateOptions;
      int: Integer): Boolean; overload; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; int: Integer; activity: JActivity; int_1: Integer): Boolean; overload; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; activityResultLauncher: JActivityResultLauncher;
      appUpdateOptions: JAppUpdateOptions): Boolean; overload; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; intentSenderForResultStarter: JIntentSenderForResultStarter;
      appUpdateOptions: JAppUpdateOptions; int: Integer): Boolean; overload; cdecl;
    procedure unregisterListener(installStateUpdatedListener: JInstallStateUpdatedListener); cdecl;
  end;
  TJAppUpdateManager = class(TJavaGenericImport<JAppUpdateManagerClass, JAppUpdateManager>) end;

  JAppUpdateInfoClass = interface(JObjectClass)
    ['{DAADE1DB-645F-4F19-8818-1336D5C3BAF6}']
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateInfo')]
  JAppUpdateInfo = interface(JObject)
    ['{949F4D97-BDB0-4575-949B-145EDF52F1BA}']
    function availableVersionCode: Integer; cdecl;
    function bytesDownloaded: Int64; cdecl;
    function clientVersionStalenessDays: JInteger; cdecl;
    function getFailedUpdatePreconditions(appUpdateOptions: JAppUpdateOptions): JSet; cdecl;
    function installStatus: Integer; cdecl;
    function isUpdateTypeAllowed(appUpdateOptions: JAppUpdateOptions): Boolean; overload; cdecl;
    function isUpdateTypeAllowed(int: Integer): Boolean; overload; cdecl;
    function packageName: JString; cdecl;
    function totalBytesToDownload: Int64; cdecl;
    function updateAvailability: Integer; cdecl;
    function updatePriority: Integer; cdecl;
  end;
  TJAppUpdateInfo = class(TJavaGenericImport<JAppUpdateInfoClass, JAppUpdateInfo>) end;

implementation

end.
