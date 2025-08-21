unit DW.Androidapi.JNI.PlayCore;

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


// Imported from https://mvnrepository.com/artifact/com.google.android.play/core/1.10.0

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Java.Security,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.Util,
  // DW
  DW.Androidapi.JNI.AndroidX.Activity;

type
  JActivityResult = interface;
  JAppUpdateInfo = interface;
  JAppUpdateManager = interface;
  JAppUpdateManagerFactory = interface;
  JAppUpdateOptions = interface;
  JAppUpdateOptions_Builder = interface;
  JAppUpdateType = interface;
  JAssetLocation = interface;
  JAssetPackErrorCode = interface;
  JAssetPackLocation = interface;
  JAssetPackManager = interface;
  JAssetPackManagerFactory = interface;
  JAssetPackState = interface;
  JAssetPackStates = interface;
  JAssetPackStateUpdateListener = interface;
  JAssetPackStatus = interface;
  JAssetPackStorageMethod = interface;
  JAssetPackUpdateAvailability = interface;
  JFakeAppUpdateManager = interface;
  JFakeSplitInstallManager = interface;
  JFakeSplitInstallManagerFactory = interface;
  JFakeReviewManager = interface;
  JIntentSenderForResultStarter = interface;
  JInstallErrorCode = interface;
  JInstallState = interface;
  JInstallStatus = interface;
  JInstallStateUpdatedListener = interface;
  JInternalFrameworkListenerExtensions = interface;
  JMissingSplitsManager = interface;
  JMissingSplitsManagerFactory = interface;
  JOnCompleteListener = interface;
  JOnFailureListener = interface;
  JOnSuccessListener = interface;
  JReviewErrorCode = interface;
  JReviewInfo = interface;
  JReviewManager = interface;
  JReviewManagerFactory = interface;
  JSplitInstallErrorCode = interface;
  JSplitInstallHelper = interface;
  JSplitInstallManager = interface;
  JSplitInstallManagerFactory = interface;
  JSplitInstallRequest = interface;
  JSplitInstallRequest_Builder = interface;
  JSplitInstallSessionState = interface;
  JSplitInstallSessionStatus = interface;
  JSplitInstallStateUpdatedListener = interface;
  JStateUpdatedListener = interface;
  JTask = interface;
  JTaskExecutors = interface;
  JTasks = interface;
  JUpdateAvailability = interface;

  JAppUpdateInfoClass = interface(JObjectClass)
    ['{8D779FDD-F605-4E4D-BD92-7CEB7ACB23FF}']
    {class} function init: JAppUpdateInfo; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateInfo')]
  JAppUpdateInfo = interface(JObject)
    ['{82ABB137-F871-43D8-A7D5-3D066874A333}']
    function availableVersionCode: Integer; cdecl;
    function bytesDownloaded: Int64; cdecl;
    function clientVersionStalenessDays: JInteger; cdecl;
    function installStatus: Integer; cdecl;
    function isUpdateTypeAllowed(appUpdateType: Integer): Boolean; cdecl; overload;
    function isUpdateTypeAllowed(options: JAppUpdateOptions): Boolean; cdecl; overload;
    function packageName: JString; cdecl;
    function totalBytesToDownload: Int64; cdecl;
    function updateAvailability: Integer; cdecl;
    function updatePriority: Integer; cdecl;
  end;
  TJAppUpdateInfo = class(TJavaGenericImport<JAppUpdateInfoClass, JAppUpdateInfo>) end;

  JAppUpdateManagerClass = interface(IJavaClass)
    ['{614C44CA-44E2-44B2-A72A-5D96E680A079}']
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateManager')]
  JAppUpdateManager = interface(IJavaInstance)
    ['{822173B2-3D6C-4DA3-BA6F-D37FE8608B3C}']
    function completeUpdate: JTask; cdecl;
    function getAppUpdateInfo: JTask; cdecl;
    procedure registerListener(listener: JInstallStateUpdatedListener); cdecl;
    function startUpdateFlow(appUpdateInfo: JAppUpdateInfo; activity: JActivity; options: JAppUpdateOptions): JTask; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; starter: JIntentSenderForResultStarter; options: JAppUpdateOptions;
      requestCode: Integer): Boolean; cdecl; overload;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; appUpdateType: Integer; starter: JIntentSenderForResultStarter;
      requestCode: Integer): Boolean; cdecl; overload;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; appUpdateType: Integer; activity: JActivity;
      requestCode: Integer): Boolean; cdecl; overload;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; activity: JActivity; options: JAppUpdateOptions;
      requestCode: Integer): Boolean; cdecl; overload;
    procedure unregisterListener(listener: JInstallStateUpdatedListener); cdecl;
  end;
  TJAppUpdateManager = class(TJavaGenericImport<JAppUpdateManagerClass, JAppUpdateManager>) end;

  JAppUpdateManagerFactoryClass = interface(JObjectClass)
    ['{AFB9F969-A3D5-4781-9469-278ED4C46085}']
    {class} function create(context: JContext): JAppUpdateManager; cdecl;
    {class} function init: JAppUpdateManagerFactory; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateManagerFactory')]
  JAppUpdateManagerFactory = interface(JObject)
    ['{1CEB5556-BADF-454E-A5FE-AFCEEFB8B795}']
  end;
  TJAppUpdateManagerFactory = class(TJavaGenericImport<JAppUpdateManagerFactoryClass, JAppUpdateManagerFactory>) end;

  JAppUpdateOptionsClass = interface(JObjectClass)
    ['{E192A5E4-65EC-4116-A016-DF9ADEBA0F2F}']
    {class} function defaultOptions(appUpdateType: Integer): JAppUpdateOptions; cdecl;
    {class} function init: JAppUpdateOptions; cdecl;
    {class} function newBuilder(appUpdateType: Integer): JAppUpdateOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateOptions')]
  JAppUpdateOptions = interface(JObject)
    ['{C753BE54-E9E9-4B51-A4A2-9F14BAA92766}']
    function allowAssetPackDeletion: Boolean; cdecl;
    function appUpdateType: Integer; cdecl;
  end;
  TJAppUpdateOptions = class(TJavaGenericImport<JAppUpdateOptionsClass, JAppUpdateOptions>) end;

  JAppUpdateOptions_BuilderClass = interface(JObjectClass)
    ['{A745909F-91CF-4AAE-9FA5-95444EB11EDB}']
    {class} function init: JAppUpdateOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/AppUpdateOptions$Builder')]
  JAppUpdateOptions_Builder = interface(JObject)
    ['{7023C524-59F6-42A8-B6CC-4F3D4742F1B3}']
    function build: JAppUpdateOptions; cdecl;
    function setAllowAssetPackDeletion(value: Boolean): JAppUpdateOptions_Builder; cdecl;
    function setAppUpdateType(value: Integer): JAppUpdateOptions_Builder; cdecl;
  end;
  TJAppUpdateOptions_Builder = class(TJavaGenericImport<JAppUpdateOptions_BuilderClass, JAppUpdateOptions_Builder>) end;

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

  JFakeAppUpdateManagerClass = interface(JAppUpdateManagerClass)
    ['{964FD05F-C048-4EBB-85E1-4CCE007A42E2}']
    {class} function init(context: JContext): JFakeAppUpdateManager; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/appupdate/testing/FakeAppUpdateManager')]
  JFakeAppUpdateManager = interface(JAppUpdateManager)
    ['{275CCEE6-8CCD-4546-820E-9D1CA6C66D7C}']
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
    procedure registerListener(listener: JInstallStateUpdatedListener); cdecl;
    procedure setBytesDownloaded(bytesDownloaded: Int64); cdecl;
    procedure setClientVersionStalenessDays(clientVersionStaleness: JInteger); cdecl;
    procedure setInstallErrorCode(installErrorCode: Integer); cdecl;
    procedure setTotalBytesToDownload(totalBytesToDownload: Int64); cdecl;
    procedure setUpdateAvailable(availableVersionCode: Integer); cdecl; overload;
    procedure setUpdateAvailable(availableVersionCode: Integer; appUpdateType: Integer); cdecl; overload;
    procedure setUpdateNotAvailable; cdecl;
    procedure setUpdatePriority(updatePriority: Integer); cdecl;
    function startUpdateFlow(appUpdateInfo: JAppUpdateInfo; activity: JActivity; options: JAppUpdateOptions): JTask; cdecl;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; appUpdateType: Integer; starter: JIntentSenderForResultStarter;
      requestCode: Integer): Boolean; cdecl; overload;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; activity: JActivity; options: JAppUpdateOptions;
      requestCode: Integer): Boolean; cdecl; overload;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; appUpdateType: Integer; activity: JActivity;
      requestCode: Integer): Boolean; cdecl; overload;
    function startUpdateFlowForResult(appUpdateInfo: JAppUpdateInfo; starter: JIntentSenderForResultStarter; options: JAppUpdateOptions;
      requestCode: Integer): Boolean; cdecl; overload;
    procedure unregisterListener(listener: JInstallStateUpdatedListener); cdecl;
    procedure userAcceptsUpdate; cdecl;
    procedure userCancelsDownload; cdecl;
    procedure userRejectsUpdate; cdecl;
  end;
  TJFakeAppUpdateManager = class(TJavaGenericImport<JFakeAppUpdateManagerClass, JFakeAppUpdateManager>) end;

  JAssetPackUpdateAvailabilityClass = interface(JAnnotationClass)
    ['{BD7D8124-7F27-4F93-845B-0BF54A327B7C}']
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} function _GetUPDATE_AVAILABLE: Integer; cdecl;
    {class} function _GetUPDATE_NOT_AVAILABLE: Integer; cdecl;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
    {class} property UPDATE_AVAILABLE: Integer read _GetUPDATE_AVAILABLE;
    {class} property UPDATE_NOT_AVAILABLE: Integer read _GetUPDATE_NOT_AVAILABLE;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/model/AssetPackUpdateAvailability')]
  JAssetPackUpdateAvailability = interface(JAnnotation)
    ['{0BF75B0A-EEA5-4499-B6C4-ED4FF83C3A81}']
  end;
  TJAssetPackUpdateAvailability = class(TJavaGenericImport<JAssetPackUpdateAvailabilityClass, JAssetPackUpdateAvailability>) end;

  JAssetLocationClass = interface(JObjectClass)
    ['{CA677523-263C-43C6-9E58-C1E1DCE35F90}']
    {class} function init: JAssetLocation; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetLocation')]
  JAssetLocation = interface(JObject)
    ['{8101ECD4-DFAF-4730-8D9A-66FB42F958BD}']
    function offset: Int64; cdecl;
    function path: JString; cdecl;
    function size: Int64; cdecl;
  end;
  TJAssetLocation = class(TJavaGenericImport<JAssetLocationClass, JAssetLocation>) end;

  JAssetPackLocationClass = interface(JObjectClass)
    ['{9E159634-0EAC-425D-BA50-E19CF0A97B6C}']
    {class} function init: JAssetPackLocation; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackLocation')]
  JAssetPackLocation = interface(JObject)
    ['{7CE899B4-0815-4A93-A27E-30AC1E571E4D}']
    function assetsPath: JString; cdecl;
    function packStorageMethod: Integer; cdecl;
    function path: JString; cdecl;
  end;
  TJAssetPackLocation = class(TJavaGenericImport<JAssetPackLocationClass, JAssetPackLocation>) end;

  JAssetPackManagerClass = interface(IJavaClass)
    ['{F9704478-9F2C-4476-A397-686552C006DD}']
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackManager')]
  JAssetPackManager = interface(IJavaInstance)
    ['{6B3CE2E9-BACD-4895-A1EC-6EDF78822551}']
    function cancel(packNames: JList): JAssetPackStates; cdecl;
    procedure clearListeners; cdecl;
    function fetch(packNames: JList): JTask; cdecl;
    function getAssetLocation(packName: JString; assetPath: JString): JAssetLocation; cdecl;
    function getPackLocation(packName: JString): JAssetPackLocation; cdecl;
    function getPackLocations: JMap; cdecl;
    function getPackStates(packNames: JList): JTask; cdecl;
    procedure registerListener(listener: JAssetPackStateUpdateListener); cdecl;
    function removePack(packName: JString): JTask; cdecl;
    function showCellularDataConfirmation(activityresultlauncher: JActivityResultLauncher): Boolean; cdecl; overload;
    function showCellularDataConfirmation(activity: JActivity): JTask; cdecl; overload;
    function showConfirmationDialog(activityresultlauncher: JActivityResultLauncher): Boolean; cdecl; overload;
    function showConfirmationDialog(activity: JActivity): JTask; cdecl; overload;
    procedure unregisterListener(assetpackstateupdatelistener: JAssetPackStateUpdateListener); cdecl;
  end;
  TJAssetPackManager = class(TJavaGenericImport<JAssetPackManagerClass, JAssetPackManager>) end;

  JAssetPackManagerFactoryClass = interface(JObjectClass)
    ['{647D4AA5-4B8C-4072-BB16-442AB9C99E5B}']
    {class} function getInstance(context: JContext): JAssetPackManager; cdecl;
    {class} function init: JAssetPackManagerFactory; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackManagerFactory')]
  JAssetPackManagerFactory = interface(JObject)
    ['{A9BB2336-B398-4AEF-BF28-48C9EA32DFD1}']
  end;
  TJAssetPackManagerFactory = class(TJavaGenericImport<JAssetPackManagerFactoryClass, JAssetPackManagerFactory>) end;

  JAssetPackStateClass = interface(JObjectClass)
    ['{D32D3F23-0520-44FC-8343-9085DA27A7AD}']
    {class} function init: JAssetPackState; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackState')]
  JAssetPackState = interface(JObject)
    ['{7FA9AC47-01E9-4E9E-B2B2-0B4C8171CA4E}']
    function availableVersionTag: JString; cdecl;
    function bytesDownloaded: Int64; cdecl;
    function errorCode: Integer; cdecl;
    function installedVersionTag: JString; cdecl;
    function name: JString; cdecl;
    function status: Integer; cdecl;
    function totalBytesToDownload: Int64; cdecl;
    function transferProgressPercentage: Integer; cdecl;
    function updateAvailability: Integer; cdecl;
  end;
  TJAssetPackState = class(TJavaGenericImport<JAssetPackStateClass, JAssetPackState>) end;

  JStateUpdatedListenerClass = interface(IJavaClass)
    ['{BC9C42E8-13AB-4BD5-A3AB-DACA8B5B0A6C}']
  end;

  [JavaSignature('com/google/android/play/core/listener/StateUpdatedListener')]
  JStateUpdatedListener = interface(IJavaInstance)
    ['{77BA1EEE-82AC-4905-9B87-1E3215301A5B}']
  end;
  TJStateUpdatedListener = class(TJavaGenericImport<JStateUpdatedListenerClass, JStateUpdatedListener>) end;

  JAssetPackStateUpdateListenerClass = interface(JStateUpdatedListenerClass)
    ['{05ACC835-14A8-47F1-AE24-8D33CCDAAD5B}']
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackStateUpdateListener')]
  JAssetPackStateUpdateListener = interface(JStateUpdatedListener)
    ['{F49FE6C1-FD95-4A05-87BA-50DEAC6E0523}']
    procedure onStateUpdate(state: JAssetPackState); cdecl;
  end;
  TJAssetPackStateUpdateListener = class(TJavaGenericImport<JAssetPackStateUpdateListenerClass, JAssetPackStateUpdateListener>) end;

  JAssetPackStatesClass = interface(JObjectClass)
    ['{BF5A3D8E-79B1-4EF0-A5B9-42002964BD2F}']
    {class} function init: JAssetPackStates; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackStates')]
  JAssetPackStates = interface(JObject)
    ['{2E2F5815-ED9F-44C6-AB6F-EF29C94D0D5B}']
    function packStates: JMap; cdecl;
    function totalBytes: Int64; cdecl;
  end;
  TJAssetPackStates = class(TJavaGenericImport<JAssetPackStatesClass, JAssetPackStates>) end;

  JOnSuccessListenerClass = interface(IJavaClass)
    ['{DDE1B8A1-0B50-4E5B-83CB-8A763C170394}']
  end;

  [JavaSignature('com/google/android/play/core/tasks/OnSuccessListener')]
  JOnSuccessListener = interface(IJavaInstance)
    ['{9ABAA432-D55B-4C5C-8735-CB137B9E7DBE}']
    procedure onSuccess(result: JObject); cdecl;
  end;
  TJOnSuccessListener = class(TJavaGenericImport<JOnSuccessListenerClass, JOnSuccessListener>) end;

  JOnFailureListenerClass = interface(IJavaClass)
    ['{A25C349C-B33E-44D1-B25B-E4CC8BC8BF27}']
  end;

  [JavaSignature('com/google/android/play/core/tasks/OnFailureListener')]
  JOnFailureListener = interface(IJavaInstance)
    ['{7DD87905-0804-40EE-A8BD-CBDD1045B9A0}']
    procedure onFailure(exception: JException); cdecl;
  end;
  TJOnFailureListener = class(TJavaGenericImport<JOnFailureListenerClass, JOnFailureListener>) end;

  JAssetPackErrorCodeClass = interface(JAnnotationClass)
    ['{B0A1F08A-DE91-4055-AFC7-EDAE415AD141}']
    {class} function _GetACCESS_DENIED: Integer; cdecl;
    {class} function _GetAPI_NOT_AVAILABLE: Integer; cdecl;
    {class} function _GetAPP_NOT_OWNED: Integer; cdecl;
    {class} function _GetAPP_UNAVAILABLE: Integer; cdecl;
    {class} function _GetCONFIRMATION_NOT_REQUIRED: Integer; cdecl;
    {class} function _GetDOWNLOAD_NOT_FOUND: Integer; cdecl;
    {class} function _GetINSUFFICIENT_STORAGE: Integer; cdecl;
    {class} function _GetINTERNAL_ERROR: Integer; cdecl;
    {class} function _GetINVALID_REQUEST: Integer; cdecl;
    {class} function _GetNETWORK_ERROR: Integer; cdecl;
    {class} function _GetNO_ERROR: Integer; cdecl;
    {class} function _GetPACK_UNAVAILABLE: Integer; cdecl;
    {class} function _GetUNRECOGNIZED_INSTALLATION: Integer; cdecl;
    {class} property ACCESS_DENIED: Integer read _GetACCESS_DENIED;
    {class} property API_NOT_AVAILABLE: Integer read _GetAPI_NOT_AVAILABLE;
    {class} property APP_NOT_OWNED: Integer read _GetAPP_NOT_OWNED;
    {class} property APP_UNAVAILABLE: Integer read _GetAPP_UNAVAILABLE;
    {class} property CONFIRMATION_NOT_REQUIRED: Integer read _GetCONFIRMATION_NOT_REQUIRED;
    {class} property DOWNLOAD_NOT_FOUND: Integer read _GetDOWNLOAD_NOT_FOUND;
    {class} property INSUFFICIENT_STORAGE: Integer read _GetINSUFFICIENT_STORAGE;
    {class} property INTERNAL_ERROR: Integer read _GetINTERNAL_ERROR;
    {class} property INVALID_REQUEST: Integer read _GetINVALID_REQUEST;
    {class} property NETWORK_ERROR: Integer read _GetNETWORK_ERROR;
    {class} property NO_ERROR: Integer read _GetNO_ERROR;
    {class} property PACK_UNAVAILABLE: Integer read _GetPACK_UNAVAILABLE;
    {class} property UNRECOGNIZED_INSTALLATION: Integer read _GetUNRECOGNIZED_INSTALLATION;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/model/AssetPackErrorCode')]
  JAssetPackErrorCode = interface(JAnnotation)
    ['{9EC31729-C2B6-43E7-9C92-3FE75046612A}']
  end;
  TJAssetPackErrorCode = class(TJavaGenericImport<JAssetPackErrorCodeClass, JAssetPackErrorCode>) end;

  JAssetPackStatusClass = interface(JAnnotationClass)
    ['{5ED3BBAD-0F3D-4DFA-9713-EA09ADEE092A}']
    {class} function _GetCANCELED: Integer; cdecl;
    {class} function _GetCOMPLETED: Integer; cdecl;
    {class} function _GetDOWNLOADING: Integer; cdecl;
    {class} function _GetFAILED: Integer; cdecl;
    {class} function _GetNOT_INSTALLED: Integer; cdecl;
    {class} function _GetPENDING: Integer; cdecl;
    {class} function _GetREQUIRES_USER_CONFIRMATION: Integer; cdecl;
    {class} function _GetTRANSFERRING: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} function _GetWAITING_FOR_WIFI: Integer; cdecl;
    {class} property CANCELED: Integer read _GetCANCELED;
    {class} property COMPLETED: Integer read _GetCOMPLETED;
    {class} property DOWNLOADING: Integer read _GetDOWNLOADING;
    {class} property FAILED: Integer read _GetFAILED;
    {class} property NOT_INSTALLED: Integer read _GetNOT_INSTALLED;
    {class} property PENDING: Integer read _GetPENDING;
    {class} property REQUIRES_USER_CONFIRMATION: Integer read _GetREQUIRES_USER_CONFIRMATION;
    {class} property TRANSFERRING: Integer read _GetTRANSFERRING;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
    {class} property WAITING_FOR_WIFI: Integer read _GetWAITING_FOR_WIFI;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/model/AssetPackStatus')]
  JAssetPackStatus = interface(JAnnotation)
    ['{7D96F4B2-18AD-41E4-9250-3DB6CC52FD17}']
  end;
  TJAssetPackStatus = class(TJavaGenericImport<JAssetPackStatusClass, JAssetPackStatus>) end;

  JAssetPackStorageMethodClass = interface(JAnnotationClass)
    ['{CE550007-9E61-4034-BF5E-D4DB22EAD167}']
    {class} function _GetAPK_ASSETS: Integer; cdecl;
    {class} function _GetSTORAGE_FILES: Integer; cdecl;
    {class} property APK_ASSETS: Integer read _GetAPK_ASSETS;
    {class} property STORAGE_FILES: Integer read _GetSTORAGE_FILES;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/model/AssetPackStorageMethod')]
  JAssetPackStorageMethod = interface(JAnnotation)
    ['{0F6F0A16-29A4-41C8-982F-F41F5BFC608D}']
  end;
  TJAssetPackStorageMethod = class(TJavaGenericImport<JAssetPackStorageMethodClass, JAssetPackStorageMethod>) end;

  JInstallStateClass = interface(JObjectClass)
    ['{997DAEC0-97D9-47C5-BDB8-9E68ABFB2AEC}']
    {class} function init: JInstallState; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/install/InstallState')]
  JInstallState = interface(JObject)
    ['{85DCCD5C-7B50-4F6E-9027-C42E4F3D782C}']
    function bytesDownloaded: Int64; cdecl;
    function installErrorCode: Integer; cdecl;
    function installStatus: Integer; cdecl;
    function packageName: JString; cdecl;
    function totalBytesToDownload: Int64; cdecl;
  end;
  TJInstallState = class(TJavaGenericImport<JInstallStateClass, JInstallState>) end;

  JInstallStateUpdatedListenerClass = interface(JStateUpdatedListenerClass)
    ['{4CBBB52F-BFD4-4E64-B3B0-9583C5673CD1}']
  end;

  [JavaSignature('com/google/android/play/core/install/InstallStateUpdatedListener')]
  JInstallStateUpdatedListener = interface(JStateUpdatedListener)
    ['{74F16778-9563-4C55-8600-4C9C0EDDF541}']
  end;
  TJInstallStateUpdatedListener = class(TJavaGenericImport<JInstallStateUpdatedListenerClass, JInstallStateUpdatedListener>) end;

  JActivityResultClass = interface(JObjectClass)
    ['{5621610B-68F8-41A4-9C6E-68DCA095DC00}']
    {class} function _GetRESULT_IN_APP_UPDATE_FAILED: Integer; cdecl;
    {class} function init: JActivityResult; cdecl;
    {class} property RESULT_IN_APP_UPDATE_FAILED: Integer read _GetRESULT_IN_APP_UPDATE_FAILED;
  end;

  [JavaSignature('com/google/android/play/core/install/model/ActivityResult')]
  JActivityResult = interface(JObject)
    ['{6055E8D5-4D26-4FF6-B151-CACA07535192}']
  end;
  TJActivityResult = class(TJavaGenericImport<JActivityResultClass, JActivityResult>) end;

  JAppUpdateTypeClass = interface(JAnnotationClass)
    ['{254F54BE-5FFE-41D6-81FA-B585F01555CF}']
    {class} function _GetFLEXIBLE: Integer; cdecl;
    {class} function _GetIMMEDIATE: Integer; cdecl;
    {class} property FLEXIBLE: Integer read _GetFLEXIBLE;
    {class} property IMMEDIATE: Integer read _GetIMMEDIATE;
  end;

  [JavaSignature('com/google/android/play/core/install/model/AppUpdateType')]
  JAppUpdateType = interface(JAnnotation)
    ['{7043C0BC-1D33-470F-8D87-6078D7EDAC5C}']
  end;
  TJAppUpdateType = class(TJavaGenericImport<JAppUpdateTypeClass, JAppUpdateType>) end;

  JInstallErrorCodeClass = interface(JAnnotationClass)
    ['{D891C873-5E5F-4AE5-9CB0-2F1586B4F2D3}']
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
    ['{4CC5E99A-EEDC-42A9-BDCD-C4F0830799A0}']
  end;
  TJInstallErrorCode = class(TJavaGenericImport<JInstallErrorCodeClass, JInstallErrorCode>) end;

  JInstallStatusClass = interface(JAnnotationClass)
    ['{12334C49-7B3F-4CF3-93F6-5C3F6BED427B}']
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
    ['{71A8B9E3-BA70-4484-9727-C5B1CE647CA0}']
  end;
  TJInstallStatus = class(TJavaGenericImport<JInstallStatusClass, JInstallStatus>) end;

  JUpdateAvailabilityClass = interface(JAnnotationClass)
    ['{7F037D2D-7B98-4CBE-AADD-36568572B1DA}']
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
    ['{C3492705-C8E9-47B9-ACA9-C458B70A06B2}']
  end;
  TJUpdateAvailability = class(TJavaGenericImport<JUpdateAvailabilityClass, JUpdateAvailability>) end;

  JMissingSplitsManagerClass = interface(IJavaClass)
    ['{634D738A-13A3-429A-AF6B-DF3F39260553}']
  end;

  [JavaSignature('com/google/android/play/core/missingsplits/MissingSplitsManager')]
  JMissingSplitsManager = interface(IJavaInstance)
    ['{A915EEAE-1846-42F2-9871-A58EEC16C6C2}']
    function disableAppIfMissingRequiredSplits: Boolean; cdecl;
    function isMissingRequiredSplits: Boolean; cdecl;
  end;
  TJMissingSplitsManager = class(TJavaGenericImport<JMissingSplitsManagerClass, JMissingSplitsManager>) end;

  JMissingSplitsManagerFactoryClass = interface(JObjectClass)
    ['{5E537DF9-3E9C-4040-8249-72B811A19C6C}']
    {class} function create(context: JContext): JMissingSplitsManager; cdecl;
    {class} function init: JMissingSplitsManagerFactory; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/missingsplits/MissingSplitsManagerFactory')]
  JMissingSplitsManagerFactory = interface(JObject)
    ['{DEFC6D22-9C87-44F0-98B0-91F90B2F5A71}']
  end;
  TJMissingSplitsManagerFactory = class(TJavaGenericImport<JMissingSplitsManagerFactoryClass, JMissingSplitsManagerFactory>) end;

  JReviewInfoClass = interface(JParcelableClass)
    ['{4D101938-3A39-4899-BB11-265D3FBB502F}']
    {class} function init: JReviewInfo; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/review/ReviewInfo')]
  JReviewInfo = interface(JParcelable)
    ['{CD1DF79D-699A-4200-8A7A-F4A330103FA0}']
  end;
  TJReviewInfo = class(TJavaGenericImport<JReviewInfoClass, JReviewInfo>) end;

  JReviewManagerClass = interface(IJavaClass)
    ['{B44F8F1E-57F9-435A-A633-D0658D0DFA9D}']
  end;

  [JavaSignature('com/google/android/play/core/review/ReviewManager')]
  JReviewManager = interface(IJavaInstance)
    ['{F2F01D73-3822-4FB3-8B09-6BEA7589C330}']
    function launchReviewFlow(activity: JActivity; reviewInfo: JReviewInfo): JTask; cdecl;
    function requestReviewFlow: JTask; cdecl;
  end;
  TJReviewManager = class(TJavaGenericImport<JReviewManagerClass, JReviewManager>) end;

  JReviewManagerFactoryClass = interface(JObjectClass)
    ['{396F5DA7-CDA3-4E8F-B9BB-303DF8EE6CC4}']
    {class} function create(context: JContext): JReviewManager; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/review/ReviewManagerFactory')]
  JReviewManagerFactory = interface(JObject)
    ['{E121646A-3443-4D20-9E81-48FFAEDBFD5B}']
  end;
  TJReviewManagerFactory = class(TJavaGenericImport<JReviewManagerFactoryClass, JReviewManagerFactory>) end;

  JReviewErrorCodeClass = interface(JAnnotationClass)
    ['{207C5115-D56D-4528-8670-81D205439BBB}']
    {class} function _GetNO_ERROR: Integer; cdecl;
    {class} function _GetPLAY_STORE_NOT_FOUND: Integer; cdecl;
    {class} property NO_ERROR: Integer read _GetNO_ERROR;
    {class} property PLAY_STORE_NOT_FOUND: Integer read _GetPLAY_STORE_NOT_FOUND;
  end;

  [JavaSignature('com/google/android/play/core/review/model/ReviewErrorCode')]
  JReviewErrorCode = interface(JAnnotation)
    ['{0253BD29-D3BB-46D0-9AC7-D2C6E94A6836}']
  end;
  TJReviewErrorCode = class(TJavaGenericImport<JReviewErrorCodeClass, JReviewErrorCode>) end;

  JFakeReviewManagerClass = interface(JReviewManagerClass)
    ['{C1F25CF4-09B2-42BC-9A86-E874E7815EFA}']
    {class} function init(context: JContext): JFakeReviewManager; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/review/testing/FakeReviewManager')]
  JFakeReviewManager = interface(JReviewManager)
    ['{0D1CE2A7-162C-417C-B697-F356521FBA9B}']
    function launchReviewFlow(activity: JActivity; reviewInfo: JReviewInfo): JTask; cdecl;
    function requestReviewFlow: JTask; cdecl;
  end;
  TJFakeReviewManager = class(TJavaGenericImport<JFakeReviewManagerClass, JFakeReviewManager>) end;

  JInternalFrameworkListenerExtensionsClass = interface(JObjectClass)
    ['{DA240AF4-5A26-4CC3-88EB-FA6D1322E371}']
    {class} procedure registerFrameworkListener(context: JContext; listener: JSplitInstallStateUpdatedListener); cdecl;
    {class} procedure unregisterFrameworkListener(context: JContext; listener: JSplitInstallStateUpdatedListener); cdecl;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/InternalFrameworkListenerExtensions')]
  JInternalFrameworkListenerExtensions = interface(JObject)
    ['{90C70745-CDB2-4C92-802C-49B6C0087434}']
  end;
  TJInternalFrameworkListenerExtensions = class(TJavaGenericImport<JInternalFrameworkListenerExtensionsClass, JInternalFrameworkListenerExtensions>) end;

  JSplitInstallHelperClass = interface(JObjectClass)
    ['{4E693A70-E1FA-4741-A7CC-88146F742F80}']
    {class} procedure loadLibrary(context: JContext; libName: JString); cdecl;
    {class} procedure updateAppInfo(context: JContext); cdecl;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/SplitInstallHelper')]
  JSplitInstallHelper = interface(JObject)
    ['{5DCC514D-4892-4830-8636-1B0F0494CDFA}']
  end;
  TJSplitInstallHelper = class(TJavaGenericImport<JSplitInstallHelperClass, JSplitInstallHelper>) end;

  JSplitInstallManagerClass = interface(IJavaClass)
    ['{559B63A5-0BA4-4BFE-8ED4-C8F127C84968}']
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/SplitInstallManager')]
  JSplitInstallManager = interface(IJavaInstance)
    ['{ED5DA975-BEA4-47F3-957B-613C5126ECE8}']
    function cancelInstall(sessionId: Integer): JTask; cdecl;
    function deferredInstall(moduleNames: JList): JTask; cdecl;
    function deferredLanguageInstall(languages: JList): JTask; cdecl;
    function deferredLanguageUninstall(languages: JList): JTask; cdecl;
    function deferredUninstall(moduleNames: JList): JTask; cdecl;
    function getInstalledLanguages: JSet; cdecl;
    function getInstalledModules: JSet; cdecl;
    function getSessionState(sessionId: Integer): JTask; cdecl;
    function getSessionStates: JTask; cdecl;
    procedure registerListener(listener: JSplitInstallStateUpdatedListener); cdecl;
    function startConfirmationDialogForResult(splitInstallSessionState: JSplitInstallSessionState; activity: JActivity;
      requestCode: Integer): Boolean; cdecl; overload;
    function startConfirmationDialogForResult(splitInstallSessionState: JSplitInstallSessionState; starter: JIntentSenderForResultStarter;
      requestCode: Integer): Boolean; cdecl; overload;
    function startInstall(request: JSplitInstallRequest): JTask; cdecl;
    procedure unregisterListener(listener: JSplitInstallStateUpdatedListener); cdecl;
  end;
  TJSplitInstallManager = class(TJavaGenericImport<JSplitInstallManagerClass, JSplitInstallManager>) end;

  JSplitInstallManagerFactoryClass = interface(JObjectClass)
    ['{2CBD791F-9375-48C9-AADB-B43411B3BD1D}']
    {class} function create(context: JContext): JSplitInstallManager; cdecl;
    {class} function init: JSplitInstallManagerFactory; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/SplitInstallManagerFactory')]
  JSplitInstallManagerFactory = interface(JObject)
    ['{80048992-FF6D-41B1-AE3B-C0DEB748832A}']
  end;
  TJSplitInstallManagerFactory = class(TJavaGenericImport<JSplitInstallManagerFactoryClass, JSplitInstallManagerFactory>) end;

  JSplitInstallRequestClass = interface(JObjectClass)
    ['{AA098B99-86DD-4C13-A515-89576FC488F7}']
    {class} function init(builder: JSplitInstallRequest_Builder): JSplitInstallRequest; cdecl;
    {class} function newBuilder: JSplitInstallRequest_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/SplitInstallRequest')]
  JSplitInstallRequest = interface(JObject)
    ['{7012F8FF-6717-4999-B63E-66B26A5D620F}']
    function getLanguages: JList; cdecl;
    function getModuleNames: JList; cdecl;
    function toString: JString; cdecl;
  end;
  TJSplitInstallRequest = class(TJavaGenericImport<JSplitInstallRequestClass, JSplitInstallRequest>) end;

  JSplitInstallRequest_BuilderClass = interface(JObjectClass)
    ['{641BA498-6308-4BB2-AB8E-963C58DF1E2D}']
    {class} function init: JSplitInstallRequest_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/SplitInstallRequest$Builder')]
  JSplitInstallRequest_Builder = interface(JObject)
    ['{486E3FF5-C224-4BBA-8C1E-7AD74FE88053}']
    function addLanguage(locale: JLocale): JSplitInstallRequest_Builder; cdecl;
    function addModule(string_: JString): JSplitInstallRequest_Builder; cdecl;
    function build: JSplitInstallRequest; cdecl;
  end;
  TJSplitInstallRequest_Builder = class(TJavaGenericImport<JSplitInstallRequest_BuilderClass, JSplitInstallRequest_Builder>) end;

  JSplitInstallSessionStateClass = interface(JObjectClass)
    ['{02A8DF9F-C9BE-4418-A99E-1C162CC54347}']
    {class} function create(sessionId: Integer; status: Integer; errorCode: Integer; bytesDownloaded: Int64; totalBytesToDownload: Int64;
      moduleNames: JList; languages: JList): JSplitInstallSessionState; cdecl;
    {class} function init: JSplitInstallSessionState; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/SplitInstallSessionState')]
  JSplitInstallSessionState = interface(JObject)
    ['{7D7898B3-4D7B-4149-8546-E75FA8322D03}']
    function bytesDownloaded: Int64; cdecl;
    function errorCode: Integer; cdecl;
    function hasTerminalStatus: Boolean; cdecl;
    function languages: JList; cdecl;
    function moduleNames: JList; cdecl;
    function resolutionIntent: JPendingIntent; cdecl;
    function sessionId: Integer; cdecl;
    function status: Integer; cdecl;
    function totalBytesToDownload: Int64; cdecl;
  end;
  TJSplitInstallSessionState = class(TJavaGenericImport<JSplitInstallSessionStateClass, JSplitInstallSessionState>) end;

  JSplitInstallStateUpdatedListenerClass = interface(JStateUpdatedListenerClass)
    ['{26E55A31-E28C-424E-ADFF-DE17FC9F508D}']
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/SplitInstallStateUpdatedListener')]
  JSplitInstallStateUpdatedListener = interface(JStateUpdatedListener)
    ['{D695DC21-6626-447B-9560-E57267E24C6E}']
  end;
  TJSplitInstallStateUpdatedListener = class(TJavaGenericImport<JSplitInstallStateUpdatedListenerClass, JSplitInstallStateUpdatedListener>) end;

  JSplitInstallErrorCodeClass = interface(JAnnotationClass)
    ['{170DCD8B-35FE-4468-9D14-C4EB2E04A315}']
    {class} function _GetACCESS_DENIED: Integer; cdecl;
    {class} function _GetACTIVE_SESSIONS_LIMIT_EXCEEDED: Integer; cdecl;
    {class} function _GetAPI_NOT_AVAILABLE: Integer; cdecl;
    {class} function _GetAPP_NOT_OWNED: Integer; cdecl;
    {class} function _GetINCOMPATIBLE_WITH_EXISTING_SESSION: Integer; cdecl;
    {class} function _GetINSUFFICIENT_STORAGE: Integer; cdecl;
    {class} function _GetINTERNAL_ERROR: Integer; cdecl;
    {class} function _GetINVALID_REQUEST: Integer; cdecl;
    {class} function _GetMODULE_UNAVAILABLE: Integer; cdecl;
    {class} function _GetNETWORK_ERROR: Integer; cdecl;
    {class} function _GetNO_ERROR: Integer; cdecl;
    {class} function _GetPLAY_STORE_NOT_FOUND: Integer; cdecl;
    {class} function _GetSERVICE_DIED: Integer; cdecl;
    {class} function _GetSESSION_NOT_FOUND: Integer; cdecl;
    {class} function _GetSPLITCOMPAT_COPY_ERROR: Integer; cdecl;
    {class} function _GetSPLITCOMPAT_EMULATION_ERROR: Integer; cdecl;
    {class} function _GetSPLITCOMPAT_VERIFICATION_ERROR: Integer; cdecl;
    {class} property ACCESS_DENIED: Integer read _GetACCESS_DENIED;
    {class} property ACTIVE_SESSIONS_LIMIT_EXCEEDED: Integer read _GetACTIVE_SESSIONS_LIMIT_EXCEEDED;
    {class} property API_NOT_AVAILABLE: Integer read _GetAPI_NOT_AVAILABLE;
    {class} property APP_NOT_OWNED: Integer read _GetAPP_NOT_OWNED;
    {class} property INCOMPATIBLE_WITH_EXISTING_SESSION: Integer read _GetINCOMPATIBLE_WITH_EXISTING_SESSION;
    {class} property INSUFFICIENT_STORAGE: Integer read _GetINSUFFICIENT_STORAGE;
    {class} property INTERNAL_ERROR: Integer read _GetINTERNAL_ERROR;
    {class} property INVALID_REQUEST: Integer read _GetINVALID_REQUEST;
    {class} property MODULE_UNAVAILABLE: Integer read _GetMODULE_UNAVAILABLE;
    {class} property NETWORK_ERROR: Integer read _GetNETWORK_ERROR;
    {class} property NO_ERROR: Integer read _GetNO_ERROR;
    {class} property PLAY_STORE_NOT_FOUND: Integer read _GetPLAY_STORE_NOT_FOUND;
    {class} property SERVICE_DIED: Integer read _GetSERVICE_DIED;
    {class} property SESSION_NOT_FOUND: Integer read _GetSESSION_NOT_FOUND;
    {class} property SPLITCOMPAT_COPY_ERROR: Integer read _GetSPLITCOMPAT_COPY_ERROR;
    {class} property SPLITCOMPAT_EMULATION_ERROR: Integer read _GetSPLITCOMPAT_EMULATION_ERROR;
    {class} property SPLITCOMPAT_VERIFICATION_ERROR: Integer read _GetSPLITCOMPAT_VERIFICATION_ERROR;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/model/SplitInstallErrorCode')]
  JSplitInstallErrorCode = interface(JAnnotation)
    ['{33DC458B-FED6-48F4-B947-CD5F73A78E7A}']
  end;
  TJSplitInstallErrorCode = class(TJavaGenericImport<JSplitInstallErrorCodeClass, JSplitInstallErrorCode>) end;

  JSplitInstallSessionStatusClass = interface(JAnnotationClass)
    ['{087D66F9-EDB0-4498-BD41-2FDE115EC704}']
    {class} function _GetCANCELED: Integer; cdecl;
    {class} function _GetCANCELING: Integer; cdecl;
    {class} function _GetDOWNLOADED: Integer; cdecl;
    {class} function _GetDOWNLOADING: Integer; cdecl;
    {class} function _GetFAILED: Integer; cdecl;
    {class} function _GetINSTALLED: Integer; cdecl;
    {class} function _GetINSTALLING: Integer; cdecl;
    {class} function _GetPENDING: Integer; cdecl;
    {class} function _GetREQUIRES_USER_CONFIRMATION: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} property CANCELED: Integer read _GetCANCELED;
    {class} property CANCELING: Integer read _GetCANCELING;
    {class} property DOWNLOADED: Integer read _GetDOWNLOADED;
    {class} property DOWNLOADING: Integer read _GetDOWNLOADING;
    {class} property FAILED: Integer read _GetFAILED;
    {class} property INSTALLED: Integer read _GetINSTALLED;
    {class} property INSTALLING: Integer read _GetINSTALLING;
    {class} property PENDING: Integer read _GetPENDING;
    {class} property REQUIRES_USER_CONFIRMATION: Integer read _GetREQUIRES_USER_CONFIRMATION;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/model/SplitInstallSessionStatus')]
  JSplitInstallSessionStatus = interface(JAnnotation)
    ['{043765E9-116E-49AD-A52F-450B56C3D015}']
  end;
  TJSplitInstallSessionStatus = class(TJavaGenericImport<JSplitInstallSessionStatusClass, JSplitInstallSessionStatus>) end;

  JFakeSplitInstallManagerClass = interface(JSplitInstallManagerClass)
    ['{5E9CF804-57D5-40FE-90A4-472DB3EFAE93}']
    {class} function init(context: JContext; file_: JFile): JFakeSplitInstallManager; cdecl; overload;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/testing/FakeSplitInstallManager')]
  JFakeSplitInstallManager = interface(JSplitInstallManager)
    ['{6EFADEA0-A91C-4D6F-873D-237045E0324A}']
    function cancelInstall(sessionId: Integer): JTask; cdecl;
    function deferredInstall(moduleNames: JList): JTask; cdecl;
    function deferredLanguageInstall(languages: JList): JTask; cdecl;
    function deferredLanguageUninstall(languages: JList): JTask; cdecl;
    function deferredUninstall(moduleNames: JList): JTask; cdecl;
    function getInstalledLanguages: JSet; cdecl;
    function getInstalledModules: JSet; cdecl;
    function getSessionState(sessionId: Integer): JTask; cdecl;
    function getSessionStates: JTask; cdecl;
    procedure registerListener(listener: JSplitInstallStateUpdatedListener); cdecl;
    procedure setShouldNetworkError(value: Boolean); cdecl;
    function startConfirmationDialogForResult(splitInstallSessionState: JSplitInstallSessionState; activity: JActivity;
      requestCode: Integer): Boolean; cdecl; overload;
    function startConfirmationDialogForResult(splitInstallSessionState: JSplitInstallSessionState; starter: JIntentSenderForResultStarter;
      requestCode: Integer): Boolean; cdecl; overload;
    function startInstall(request: JSplitInstallRequest): JTask; cdecl;
    procedure unregisterListener(listener: JSplitInstallStateUpdatedListener); cdecl;
  end;
  TJFakeSplitInstallManager = class(TJavaGenericImport<JFakeSplitInstallManagerClass, JFakeSplitInstallManager>) end;

  JFakeSplitInstallManagerFactoryClass = interface(JObjectClass)
    ['{5571C06A-A303-4D65-ACDC-9E571D7E5FF0}']
    {class} function create(context: JContext): JFakeSplitInstallManager; cdecl; overload;
    {class} function create(context: JContext; modulesDirectory: JFile): JFakeSplitInstallManager; cdecl; overload;
    {class} function createNewInstance(context: JContext; modulesDirectory: JFile): JFakeSplitInstallManager; cdecl;
    {class} function init: JFakeSplitInstallManagerFactory; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/splitinstall/testing/FakeSplitInstallManagerFactory')]
  JFakeSplitInstallManagerFactory = interface(JObject)
    ['{8709BD48-A013-4460-A96F-80945E9575E1}']
  end;
  TJFakeSplitInstallManagerFactory = class(TJavaGenericImport<JFakeSplitInstallManagerFactoryClass, JFakeSplitInstallManagerFactory>) end;

  JOnCompleteListenerClass = interface(IJavaClass)
    ['{9275A91D-D30A-44DD-A6DA-45A2C3BE04AF}']
  end;

  [JavaSignature('com/google/android/play/core/tasks/OnCompleteListener')]
  JOnCompleteListener = interface(IJavaInstance)
    ['{8EB0A488-3A59-4762-9F73-C780F2538E9E}']
    procedure onComplete(task: JTask); cdecl;
  end;
  TJOnCompleteListener = class(TJavaGenericImport<JOnCompleteListenerClass, JOnCompleteListener>) end;

  JTaskClass = interface(JObjectClass)
    ['{9FD241CC-6AAB-411D-A245-C6B21B5B8DAA}']
    {class} function init: JTask; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/tasks/Task')]
  JTask = interface(JObject)
    ['{C2E00876-9821-4F23-85B6-508C5D049737}']
    function addOnCompleteListener(listener: JOnCompleteListener): JTask; cdecl; overload;
    function addOnCompleteListener(executor: JExecutor; listener: JOnCompleteListener): JTask; cdecl; overload;
    function addOnFailureListener(listener: JOnFailureListener): JTask; cdecl; overload;
    function addOnFailureListener(executor: JExecutor; listener: JOnFailureListener): JTask; cdecl; overload;
    function addOnSuccessListener(listener: JOnSuccessListener): JTask; cdecl; overload;
    function addOnSuccessListener(executor: JExecutor; listener: JOnSuccessListener): JTask; cdecl; overload;
    function getException: JException; cdecl;
    function getResult: JObject; cdecl; overload;
    function getResult(exceptionType: Jlang_Class): JObject; cdecl; overload;
    function isComplete: Boolean; cdecl;
    function isSuccessful: Boolean; cdecl;
  end;
  TJTask = class(TJavaGenericImport<JTaskClass, JTask>) end;

  JTaskExecutorsClass = interface(JObjectClass)
    ['{6EB3CA52-246C-4559-B69E-30F3409EDAAB}']
    {class} function _GetMAIN_THREAD: JExecutor; cdecl;
    {class} property MAIN_THREAD: JExecutor read _GetMAIN_THREAD;
  end;

  [JavaSignature('com/google/android/play/core/tasks/TaskExecutors')]
  JTaskExecutors = interface(JObject)
    ['{169D374B-8A51-41D7-91C5-BCC9749FAAD7}']
  end;
  TJTaskExecutors = class(TJavaGenericImport<JTaskExecutorsClass, JTaskExecutors>) end;

  JTasksClass = interface(JObjectClass)
    ['{46308976-B28E-4FFB-821A-E7354A13E4CA}']
    {class} function await(task: JTask): JObject; cdecl; overload;
    {class} function await(task: JTask; timeout: Int64; timeUnit: JTimeUnit): JObject; cdecl; overload;
    {class} // function whenAll - could not import
  end;

  [JavaSignature('com/google/android/play/core/tasks/Tasks')]
  JTasks = interface(JObject)
    ['{1E383C88-6B76-4171-8C95-41E653C0CE2D}']
  end;
  TJTasks = class(TJavaGenericImport<JTasksClass, JTasks>) end;

implementation

end.
