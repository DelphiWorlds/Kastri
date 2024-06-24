unit DW.Androidapi.JNI.AssetDelivery;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.App, Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.PlayServices.Tasks, Androidapi.JNI.Support;

type
  JAssetLocation = interface;
  JAssetPackErrorCode = interface;
  JAssetPackLocation = interface;
  JAssetPackManager = interface;
  JAssetPackManagerFactory = interface;
  JAssetPackState = interface;
  JAssetPackStateUpdateListener = interface;
  JAssetPackStates = interface;
  JAssetPackStatus = interface;
  JAssetPackStorageMethod = interface;
  JAssetPackUpdateAvailability = interface;
  JStateUpdatedListener = interface;

  JStateUpdatedListenerClass = interface(IJavaClass)
    ['{BC9C42E8-13AB-4BD5-A3AB-DACA8B5B0A6C}']
  end;

  [JavaSignature('com/google/android/play/core/listener/StateUpdatedListener')]
  JStateUpdatedListener = interface(IJavaInstance)
    ['{77BA1EEE-82AC-4905-9B87-1E3215301A5B}']
  end;
  TJStateUpdatedListener = class(TJavaGenericImport<JStateUpdatedListenerClass, JStateUpdatedListener>) end;

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

  JAssetPackStorageMethodClass = interface(JAnnotationClass)
    ['{7F118576-4D8D-46A9-9A07-0B10488D2F64}']
    {class} function _GetAPK_ASSETS: Integer; cdecl;
    {class} function _GetSTORAGE_FILES: Integer; cdecl;
    {class} property APK_ASSETS: Integer read _GetAPK_ASSETS;
    {class} property STORAGE_FILES: Integer read _GetSTORAGE_FILES;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/model/AssetPackStorageMethod')]
  JAssetPackStorageMethod = interface(JAnnotation)
    ['{C20E0D4C-AE42-4A80-BC22-56E787AB2671}']
  end;
  TJAssetPackStorageMethod = class(TJavaGenericImport<JAssetPackStorageMethodClass, JAssetPackStorageMethod>) end;

  JAssetPackStatusClass = interface(JAnnotationClass)
    ['{65C890C4-681D-4C0D-8D80-1620DA178B8D}']
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
    ['{5D984248-4DA9-4222-A301-38CDDADBB6BC}']
  end;
  TJAssetPackStatus = class(TJavaGenericImport<JAssetPackStatusClass, JAssetPackStatus>) end;

  JAssetPackErrorCodeClass = interface(JAnnotationClass)
    ['{18C56F87-E836-4FFD-9825-FD2EE20EB63C}']
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
    ['{9731527E-0C72-4DF8-A976-3565CC2537BD}']
  end;
  TJAssetPackErrorCode = class(TJavaGenericImport<JAssetPackErrorCodeClass, JAssetPackErrorCode>) end;

  JAssetPackStatesClass = interface(JObjectClass)
    ['{63C3AA19-739F-4B4F-A3AD-A39AB845846B}']
    {class} function init: JAssetPackStates; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackStates')]
  JAssetPackStates = interface(JObject)
    ['{E0E2E984-EEDC-4DCE-852B-2F211A20E0B3}']
    function packStates: JMap; cdecl;
    function totalBytes: Int64; cdecl;
  end;
  TJAssetPackStates = class(TJavaGenericImport<JAssetPackStatesClass, JAssetPackStates>) end;

  JAssetPackStateUpdateListenerClass = interface(JStateUpdatedListenerClass)
    ['{723E6E02-CDC7-4315-838F-07171D0B8D27}']
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackStateUpdateListener')]
  JAssetPackStateUpdateListener = interface(JStateUpdatedListener)
    ['{AB4BAD0E-5032-40EF-8DD3-997461B62FF2}']
    procedure onStateUpdate(state: JObject); cdecl;
  end;
  TJAssetPackStateUpdateListener = class(TJavaGenericImport<JAssetPackStateUpdateListenerClass, JAssetPackStateUpdateListener>) end;

  JAssetPackStateClass = interface(JObjectClass)
    ['{AC096953-4703-43AD-AD5E-6B580C451631}']
    {class} function init: JAssetPackState; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackState')]
  JAssetPackState = interface(JObject)
    ['{9F1E35D2-9E31-48AE-AD44-853D3502C129}']
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

  JAssetPackManagerFactoryClass = interface(JObjectClass)
    ['{6F3127AC-AC87-4977-AC98-8C425A8D9249}']
    {class} function getInstance(context: JContext): JAssetPackManager; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackManagerFactory')]
  JAssetPackManagerFactory = interface(JObject)
    ['{02CDBC08-7345-4BD7-89BE-BDDC1FCEAFCB}']
  end;
  TJAssetPackManagerFactory = class(TJavaGenericImport<JAssetPackManagerFactoryClass, JAssetPackManagerFactory>) end;

  JAssetPackManagerClass = interface(IJavaClass)
    ['{4C383145-DD0E-46C3-937F-4E369D316859}']
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackManager')]
  JAssetPackManager = interface(IJavaInstance)
    ['{C44B7236-770A-4DBA-8C7F-0EFB90AF3042}']
    function cancel(list: JList): JAssetPackStates; cdecl;
    procedure clearListeners; cdecl;
    function fetch(list: JList): JTask; cdecl;
    function getAssetLocation(string_1: JString; string_2: JString): JAssetLocation; cdecl;
    function getPackLocation(string_1: JString): JAssetPackLocation; cdecl;
    function getPackLocations: JMap; cdecl;
    function getPackStates(list: JList): JTask; cdecl;
    procedure registerListener(assetpackstateupdatelistener: JAssetPackStateUpdateListener); cdecl;
    function removePack(string_1: JString): JTask; cdecl;
    function showCellularDataConfirmation(activityresultlauncher: JActivityResultLauncher): Boolean; cdecl; overload;
    function showCellularDataConfirmation(activity: JActivity): JTask; cdecl; overload;
    function showConfirmationDialog(activityresultlauncher: JActivityResultLauncher): Boolean; cdecl; overload;
    function showConfirmationDialog(activity: JActivity): JTask; cdecl; overload;
    procedure unregisterListener(assetpackstateupdatelistener: JAssetPackStateUpdateListener); cdecl;
  end;
  TJAssetPackManager = class(TJavaGenericImport<JAssetPackManagerClass, JAssetPackManager>) end;

  JAssetPackLocationClass = interface(JObjectClass)
    ['{91DA78D4-A857-4A40-BEC8-5D6075ED6EC4}']
    {class} function init: JAssetPackLocation; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetPackLocation')]
  JAssetPackLocation = interface(JObject)
    ['{91A2A640-44B5-417D-89F0-B209992DA733}']
    function assetsPath: JString; cdecl;
    function packStorageMethod: Integer; cdecl;
    function path: JString; cdecl;
  end;
  TJAssetPackLocation = class(TJavaGenericImport<JAssetPackLocationClass, JAssetPackLocation>) end;

  JAssetLocationClass = interface(JObjectClass)
    ['{F1AC8B62-3809-4973-9056-F4679904A6B8}']
    {class} function init: JAssetLocation; cdecl;
  end;

  [JavaSignature('com/google/android/play/core/assetpacks/AssetLocation')]
  JAssetLocation = interface(JObject)
    ['{E30DC159-D66C-4192-9511-B3BD83CE34C4}']
    function offset: Int64; cdecl;
    function path: JString; cdecl;
    function size: Int64; cdecl;
  end;
  TJAssetLocation = class(TJavaGenericImport<JAssetLocationClass, JAssetLocation>) end;

implementation

end.
