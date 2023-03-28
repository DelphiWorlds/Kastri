unit DW.Androidapi.JNI.AndroidX.Camera;

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

// (androidx-)camera-core-1.0.1
// (androidx-)camera-view-1.0.0-alpha25.jar

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Location, Androidapi.JNI.Media,
  Androidapi.JNI.Net, Androidapi.JNI.Os, Androidapi.JNI.Util, Androidapi.JNI.Widget,
  // DW
  DW.Androidapi.JNI.Util, DW.Androidapi.JNI.Hardware.Camera2, DW.Androidapi.JNI.View, DW.Androidapi.JNI.Concurrent,
  DW.Androidapi.JNI.AndroidX.Lifecycle;

type
  JImageAnalysis = interface;
  JImageAnalysis_Analyzer = interface;
  JCaptureProcessor = interface;
  JCaptureStage = interface;
  JCaptureStage_DefaultCaptureStage = interface;
  JCameraCaptureCallback = interface;
  JCameraCaptureFailure = interface;
  JCameraCaptureFailure_Reason = interface;
  JCameraCaptureMetaData_AeState = interface;
  JCameraCaptureMetaData_AfMode = interface;
  JCameraCaptureMetaData_AfState = interface;
  JCameraCaptureMetaData_AwbState = interface;
  JCameraCaptureMetaData_FlashState = interface;
  JCameraCaptureResult = interface;
  JCameraConfig = interface;
  JCamera2Config = interface;
  JCameraControl = interface;
  JCameraControlInternal = interface;
  JCameraController = interface;
  JCameraDeviceSurfaceManager = interface;
  JCameraDeviceSurfaceManager_Provider = interface;
  JCameraFactory = interface;
  JCameraFactory_Provider = interface;
  JCameraFilter = interface;
  JCameraInfo = interface;
  JCameraInfoInternal = interface;
  JCameraInternal = interface;
  JCameraSelector = interface;
  JCameraSelector_Builder = interface;
  JCameraThreadConfig = interface;
  JCameraUseCaseAdapter = interface;
  JCameraUseCaseAdapter_CameraId = interface;
  JCameraXConfig = interface;
  JCameraXConfig_Builder = interface;
  JCameraXConfig_Provider = interface;
  JCaptureConfig = interface;
  JCaptureConfig_Builder = interface;
  JCaptureConfig_OptionUnpacker = interface;
  Jcore_Camera = interface;
  JConfig_Option = interface;
  JConfig_OptionMatcher = interface;
  JConfig_OptionPriority = interface;
  JConfigProvider = interface;
  JDeferrableSurface = interface;
  JExifData_Builder = interface;
  JExifData_WhiteBalanceMode = interface;
  JExposureState = interface;
  JFocusMeteringAction = interface;
  JFocusMeteringAction_Builder = interface;
  JFocusMeteringAction_MeteringMode = interface;
  JFocusMeteringResult = interface;
  JImageAnalysis_Defaults = interface;
  JImageAnalysisConfig = interface;
  JImageCaptureException = interface;
  JImageCapture_Metadata = interface;
  JImageCapture_OnImageCapturedCallback = interface;
  JImageCapture_OnImageSavedCallback = interface;
  JImageCapture_OutputFileOptions = interface;
  JImageCapture_OutputFileResults = interface;
  JImageInfo = interface;
  JImageInfoProcessor = interface;
  JImageProxy = interface;
  JImageProxyBundle = interface;
  JImageProxy_PlaneProxy = interface;
  JImageReaderProxy = interface;
  JImageReaderProxy_OnImageAvailableListener = interface;
  JImageReaderProxyProvider = interface;
  Jimpl_Config = interface;
  Jimpl_Observable = interface;
  JMetadata_Builder = interface;
  JMeteringPoint = interface;
  JMeteringPointFactory = interface;
  JMutableConfig = interface;
  JObservable_Observer = interface;
  JOptionsBundle = interface;
  JOnVideoSavedCallback = interface;
  JOutputFileOptions = interface;
  JOutputFileResults = interface;
  JOutputTransform = interface;
  JPreview = interface;
  JPreview_Builder = interface;
  JPreviewConfig = interface;
  JPreview_Defaults = interface;
  JPreview_SurfaceProvider = interface;
  JPreviewTransformation = interface;
  JPreviewView = interface;
  JPreviewView_ImplementationMode = interface;
  JPreviewView_ScaleType = interface;
  JPreviewViewImplementation = interface;
  JPreviewViewImplementation_OnSurfaceNotInUseListener = interface;
  JPreviewViewMeteringPointFactory = interface;
  JQuirk = interface;
  JQuirks = interface;
  JReadableConfig = interface;
  JSensorRotationListener = interface;
  JSessionConfig = interface;
  JSessionConfig_BaseBuilder = interface;
  JSessionConfig_Builder = interface;
  JSessionConfig_ErrorListener = interface;
  JSessionConfig_SessionError = interface;
  JSessionConfig_OptionUnpacker = interface;
  JSessionConfig_ValidatingBuilder = interface;
  JSurfaceConfig = interface;
  JSurfaceConfig_ConfigSize = interface;
  JSurfaceConfig_ConfigType = interface;
  JSurfaceRequest = interface;
  JSurfaceRequest_TransformationInfo = interface;
  JSurfaceRequest_TransformationInfoListener = interface;
  JTagBundle = interface;
  JTargetConfig = interface;
  JTargetConfig_Builder = interface;
  JUseCase = interface;
  JUseCaseConfig = interface;
  JUseCaseConfig_Builder = interface;
  JUseCaseConfigFactory = interface;
  JUseCaseConfigFactory_CaptureType = interface;
  JUseCaseConfigFactory_Provider = interface;
  JUseCase_EventCallback = interface;
  JUseCaseGroup = interface;
  JUseCaseGroup_Builder = interface;
  JVideoCapture = interface;
  JVideoCaptureConfig = interface;
  JVideoCapture_Defaults = interface;
  JVideoCapture_Metadata = interface;
  JVideoCapture_OnVideoSavedCallback = interface;
  JVideoCapture_OutputFileOptions = interface;
  JVideoCapture_OutputFileResults = interface;
  Jvideo_Metadata = interface;
  Jvideo_OutputFileOptions_Builder = interface;
  JViewPort = interface;

  JDeferrableSurfaceClass = interface(JObjectClass)
    ['{8AE93568-A583-4995-ABC6-25DBA891FFAE}']
    {class} function init: JDeferrableSurface; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/DeferrableSurface')]
  JDeferrableSurface = interface(JObject)
    ['{F34C870A-6F57-4160-B2C2-4D2CA03C2A62}']
    procedure close; cdecl;
    procedure decrementUseCount; cdecl;
    function getSurface: JListenableFuture; cdecl;
    function getTerminationFuture: JListenableFuture; cdecl;
    function getUseCount: Integer; cdecl;
    procedure incrementUseCount; cdecl;
  end;
  TJDeferrableSurface = class(TJavaGenericImport<JDeferrableSurfaceClass, JDeferrableSurface>) end;

  JObservable_ObserverClass = interface(IJavaClass)
    ['{D3D9DDD9-335F-4AF0-A6D9-DAFE1622E10D}']
  end;

  [JavaSignature('androidx/camera/core/impl/Observable$Observer')]
  JObservable_Observer = interface(IJavaInstance)
    ['{DE55005B-6366-419E-B157-7C44BFD493F8}']
    procedure onError(throwable: JThrowable); cdecl;
    procedure onNewData(object_: JObject); cdecl;
  end;
  TJObservable_Observer = class(TJavaGenericImport<JObservable_ObserverClass, JObservable_Observer>) end;

  Jimpl_ObservableClass = interface(IJavaClass)
    ['{163506D1-7865-4945-BBA8-A4DECBE576AF}']
  end;

  [JavaSignature('androidx/camera/core/impl/Observable')]
  Jimpl_Observable = interface(IJavaInstance)
    ['{0FDDFB33-EEB7-4733-B2F7-A011D4890B1D}']
    procedure addObserver(executor: JExecutor; observer: JObservable_Observer); cdecl;
    function fetchData: JListenableFuture; cdecl;
    procedure removeObserver(observer: JObservable_Observer); cdecl;
  end;
  TJimpl_Observable = class(TJavaGenericImport<Jimpl_ObservableClass, Jimpl_Observable>) end;

  JQuirkClass = interface(IJavaClass)
    ['{907B188D-530C-4950-A5EC-BAE06CC5B29C}']
  end;

  [JavaSignature('androidx/camera/core/impl/Quirk')]
  JQuirk = interface(IJavaInstance)
    ['{180FC005-A0ED-4537-A94E-F755588EF64B}']
  end;
  TJQuirk = class(TJavaGenericImport<JQuirkClass, JQuirk>) end;

  JQuirksClass = interface(JObjectClass)
    ['{4E83A3D8-9030-4F23-B052-1566F8F83DB2}']
    {class} function init(list: JList): JQuirks; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/Quirks')]
  JQuirks = interface(JObject)
    ['{DF646D0E-6FCD-4332-8492-4F46101BC607}']
    function &contains(class_: Jlang_Class): Boolean; cdecl;
    function &get(class_: Jlang_Class): JQuirk; cdecl;
  end;
  TJQuirks = class(TJavaGenericImport<JQuirksClass, JQuirks>) end;

  JExifData_WhiteBalanceModeClass = interface(JEnumClass)
    ['{570C2A1E-AC59-436F-9BE4-9B257FFF2E24}']
    {class} function _GetAUTO: JExifData_WhiteBalanceMode; cdecl;
    {class} function _GetMANUAL: JExifData_WhiteBalanceMode; cdecl;
    {class} function valueOf(string_: JString): JExifData_WhiteBalanceMode; cdecl;
    {class} function values: TJavaObjectArray<JExifData_WhiteBalanceMode>; cdecl;
    {class} property AUTO: JExifData_WhiteBalanceMode read _GetAUTO;
    {class} property MANUAL: JExifData_WhiteBalanceMode read _GetMANUAL;
  end;

  [JavaSignature('androidx/camera/core/impl/utils/ExifData$WhiteBalanceMode')]
  JExifData_WhiteBalanceMode = interface(JEnum)
    ['{D6072304-34D3-4FD8-BBB8-1DB406C1734F}']
  end;
  TJExifData_WhiteBalanceMode = class(TJavaGenericImport<JExifData_WhiteBalanceModeClass, JExifData_WhiteBalanceMode>) end;

  JExifDataClass = interface(JObjectClass)
    ['{DB4AD031-E8AC-46B8-AD17-3C67A4F0E6EF}']
    {class} function _GetIFD_FORMAT_NAMES: TJavaObjectArray<JString>; cdecl;
    {class} function builderForDevice: JExifData_Builder; cdecl;
    {class} property IFD_FORMAT_NAMES: TJavaObjectArray<JString> read _GetIFD_FORMAT_NAMES;
  end;

  [JavaSignature('androidx/camera/core/impl/utils/ExifData')]
  JExifData = interface(JObject)
    ['{89F3ED48-4238-4D20-B9DD-8619D9AC4906}']
    function getAttribute(string_: JString): JString; cdecl;
    function getByteOrder: JByteOrder; cdecl;
  end;
  TJExifData = class(TJavaGenericImport<JExifDataClass, JExifData>) end;

  JExifData_BuilderClass = interface(JObjectClass)
    ['{D157ACDD-F7C4-45A5-B13F-15C6F6A35E4C}']
    {class} function _GetsExifTagMapsForWriting: JList; cdecl;
    {class} function init(byteOrder: JByteOrder): JExifData_Builder; cdecl;
    {class} property sExifTagMapsForWriting: JList read _GetsExifTagMapsForWriting;
  end;

  [JavaSignature('androidx/camera/core/impl/utils/ExifData$Builder')]
  JExifData_Builder = interface(JObject)
    ['{06C2C4D9-BAF4-4872-B7B6-9015A1E4111D}']
    function build: JExifData; cdecl;
    function removeAttribute(string_: JString): JExifData_Builder; cdecl;
    function setAttribute(string_: JString; string_1: JString): JExifData_Builder; cdecl;
    function setExposureTimeNanos(l: Int64): JExifData_Builder; cdecl;
    function setFlashState(flashState: JCameraCaptureMetaData_FlashState): JExifData_Builder; cdecl;
    function setFocalLength(f: Single): JExifData_Builder; cdecl;
    function setImageHeight(i: Integer): JExifData_Builder; cdecl;
    function setImageWidth(i: Integer): JExifData_Builder; cdecl;
    function setIso(i: Integer): JExifData_Builder; cdecl;
    function setLensFNumber(f: Single): JExifData_Builder; cdecl;
    function setOrientationDegrees(i: Integer): JExifData_Builder; cdecl;
    function setWhiteBalanceMode(whiteBalanceMode: JExifData_WhiteBalanceMode): JExifData_Builder; cdecl;
  end;
  TJExifData_Builder = class(TJavaGenericImport<JExifData_BuilderClass, JExifData_Builder>) end;

  JTagBundleClass = interface(JObjectClass)
    ['{5AA06C2D-B04D-4441-8BA2-33BE80CA661A}']
    {class} function create(pair: JPair): JTagBundle; cdecl;
    {class} function emptyBundle: JTagBundle; cdecl;
    {class} function from(tagBundle: JTagBundle): JTagBundle; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/TagBundle')]
  JTagBundle = interface(JObject)
    ['{08E449B9-4EAF-41A0-8412-40D6995453EE}']
    function _GetmTagMap: JMap; cdecl;
    function getTag(string_: JString): JInteger; cdecl;
    function listKeys: JSet; cdecl;
    property mTagMap: JMap read _GetmTagMap;
  end;
  TJTagBundle = class(TJavaGenericImport<JTagBundleClass, JTagBundle>) end;

  JCameraCaptureFailure_ReasonClass = interface(JEnumClass)
    ['{C6BAFC53-29DC-4693-BB14-104BFE9AF79C}']
    {class} function _GetERROR: JCameraCaptureFailure_Reason; cdecl;
    {class} function valueOf(string_: JString): JCameraCaptureFailure_Reason; cdecl;
    {class} function values: TJavaObjectArray<JCameraCaptureFailure_Reason>; cdecl;
    {class} property ERROR: JCameraCaptureFailure_Reason read _GetERROR;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureFailure$Reason')]
  JCameraCaptureFailure_Reason = interface(JEnum)
    ['{BED6C23B-F74B-4E50-A7AB-9E757AC0F02D}']
  end;
  TJCameraCaptureFailure_Reason = class(TJavaGenericImport<JCameraCaptureFailure_ReasonClass, JCameraCaptureFailure_Reason>) end;

  JCameraCaptureFailureClass = interface(JObjectClass)
    ['{57A773C8-2C1C-4E32-9ED8-B7FC3484E64D}']
    {class} function init(reason: JCameraCaptureFailure_Reason): JCameraCaptureFailure; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureFailure')]
  JCameraCaptureFailure = interface(JObject)
    ['{7917F446-1461-44DC-8C16-043526600D07}']
    function getReason: JCameraCaptureFailure_Reason; cdecl;
  end;
  TJCameraCaptureFailure = class(TJavaGenericImport<JCameraCaptureFailureClass, JCameraCaptureFailure>) end;

  JCameraCaptureMetaData_AeStateClass = interface(JEnumClass)
    ['{B688BC4C-552C-4D7A-AB38-DF8144E15F50}']
    {class} function _GetCONVERGED: JCameraCaptureMetaData_AeState; cdecl;
    {class} function _GetFLASH_REQUIRED: JCameraCaptureMetaData_AeState; cdecl;
    {class} function _GetINACTIVE: JCameraCaptureMetaData_AeState; cdecl;
    {class} function _GetLOCKED: JCameraCaptureMetaData_AeState; cdecl;
    {class} function _GetSEARCHING: JCameraCaptureMetaData_AeState; cdecl;
    {class} function _GetUNKNOWN: JCameraCaptureMetaData_AeState; cdecl;
    {class} function valueOf(string_: JString): JCameraCaptureMetaData_AeState; cdecl;
    {class} function values: TJavaObjectArray<JCameraCaptureMetaData_AeState>; cdecl;
    {class} property CONVERGED: JCameraCaptureMetaData_AeState read _GetCONVERGED;
    {class} property FLASH_REQUIRED: JCameraCaptureMetaData_AeState read _GetFLASH_REQUIRED;
    {class} property INACTIVE: JCameraCaptureMetaData_AeState read _GetINACTIVE;
    {class} property LOCKED: JCameraCaptureMetaData_AeState read _GetLOCKED;
    {class} property SEARCHING: JCameraCaptureMetaData_AeState read _GetSEARCHING;
    {class} property UNKNOWN: JCameraCaptureMetaData_AeState read _GetUNKNOWN;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureMetaData$AeState')]
  JCameraCaptureMetaData_AeState = interface(JEnum)
    ['{1C8376C3-0CD2-4ED6-8772-D411307226B4}']
  end;
  TJCameraCaptureMetaData_AeState = class(TJavaGenericImport<JCameraCaptureMetaData_AeStateClass, JCameraCaptureMetaData_AeState>) end;

  JCameraCaptureMetaData_AfModeClass = interface(JEnumClass)
    ['{E723EA76-193A-4E4C-908D-A8870F67A983}']
    {class} function _GetOFF: JCameraCaptureMetaData_AfMode; cdecl;
    {class} function _GetON_CONTINUOUS_AUTO: JCameraCaptureMetaData_AfMode; cdecl;
    {class} function _GetON_MANUAL_AUTO: JCameraCaptureMetaData_AfMode; cdecl;
    {class} function _GetUNKNOWN: JCameraCaptureMetaData_AfMode; cdecl;
    {class} function valueOf(string_: JString): JCameraCaptureMetaData_AfMode; cdecl;
    {class} function values: TJavaObjectArray<JCameraCaptureMetaData_AfMode>; cdecl;
    {class} property OFF: JCameraCaptureMetaData_AfMode read _GetOFF;
    {class} property ON_CONTINUOUS_AUTO: JCameraCaptureMetaData_AfMode read _GetON_CONTINUOUS_AUTO;
    {class} property ON_MANUAL_AUTO: JCameraCaptureMetaData_AfMode read _GetON_MANUAL_AUTO;
    {class} property UNKNOWN: JCameraCaptureMetaData_AfMode read _GetUNKNOWN;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureMetaData$AfMode')]
  JCameraCaptureMetaData_AfMode = interface(JEnum)
    ['{E80CC115-576A-4C02-BFA5-BEE53FD2A9F5}']
  end;
  TJCameraCaptureMetaData_AfMode = class(TJavaGenericImport<JCameraCaptureMetaData_AfModeClass, JCameraCaptureMetaData_AfMode>) end;

  JCameraCaptureMetaData_AfStateClass = interface(JEnumClass)
    ['{8AB89F45-F5F0-406D-B678-20AFEC87BA75}']
    {class} function _GetFOCUSED: JCameraCaptureMetaData_AfState; cdecl;
    {class} function _GetINACTIVE: JCameraCaptureMetaData_AfState; cdecl;
    {class} function _GetLOCKED_FOCUSED: JCameraCaptureMetaData_AfState; cdecl;
    {class} function _GetLOCKED_NOT_FOCUSED: JCameraCaptureMetaData_AfState; cdecl;
    {class} function _GetSCANNING: JCameraCaptureMetaData_AfState; cdecl;
    {class} function _GetUNKNOWN: JCameraCaptureMetaData_AfState; cdecl;
    {class} function valueOf(string_: JString): JCameraCaptureMetaData_AfState; cdecl;
    {class} function values: TJavaObjectArray<JCameraCaptureMetaData_AfState>; cdecl;
    {class} property FOCUSED: JCameraCaptureMetaData_AfState read _GetFOCUSED;
    {class} property INACTIVE: JCameraCaptureMetaData_AfState read _GetINACTIVE;
    {class} property LOCKED_FOCUSED: JCameraCaptureMetaData_AfState read _GetLOCKED_FOCUSED;
    {class} property LOCKED_NOT_FOCUSED: JCameraCaptureMetaData_AfState read _GetLOCKED_NOT_FOCUSED;
    {class} property SCANNING: JCameraCaptureMetaData_AfState read _GetSCANNING;
    {class} property UNKNOWN: JCameraCaptureMetaData_AfState read _GetUNKNOWN;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureMetaData$AfState')]
  JCameraCaptureMetaData_AfState = interface(JEnum)
    ['{35C571A4-5071-4AEA-BF6E-E1010F30B952}']
  end;
  TJCameraCaptureMetaData_AfState = class(TJavaGenericImport<JCameraCaptureMetaData_AfStateClass, JCameraCaptureMetaData_AfState>) end;

  JCameraCaptureMetaData_AwbStateClass = interface(JEnumClass)
    ['{E1529960-97F1-401D-833E-90316A53036D}']
    {class} function _GetCONVERGED: JCameraCaptureMetaData_AwbState; cdecl;
    {class} function _GetINACTIVE: JCameraCaptureMetaData_AwbState; cdecl;
    {class} function _GetLOCKED: JCameraCaptureMetaData_AwbState; cdecl;
    {class} function _GetMETERING: JCameraCaptureMetaData_AwbState; cdecl;
    {class} function _GetUNKNOWN: JCameraCaptureMetaData_AwbState; cdecl;
    {class} function valueOf(string_: JString): JCameraCaptureMetaData_AwbState; cdecl;
    {class} function values: TJavaObjectArray<JCameraCaptureMetaData_AwbState>; cdecl;
    {class} property CONVERGED: JCameraCaptureMetaData_AwbState read _GetCONVERGED;
    {class} property INACTIVE: JCameraCaptureMetaData_AwbState read _GetINACTIVE;
    {class} property LOCKED: JCameraCaptureMetaData_AwbState read _GetLOCKED;
    {class} property METERING: JCameraCaptureMetaData_AwbState read _GetMETERING;
    {class} property UNKNOWN: JCameraCaptureMetaData_AwbState read _GetUNKNOWN;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureMetaData$AwbState')]
  JCameraCaptureMetaData_AwbState = interface(JEnum)
    ['{E2DDD849-ECF0-4534-88C1-FE06E29EF0D0}']
  end;
  TJCameraCaptureMetaData_AwbState = class(TJavaGenericImport<JCameraCaptureMetaData_AwbStateClass, JCameraCaptureMetaData_AwbState>) end;

  JCameraCaptureMetaData_FlashStateClass = interface(JEnumClass)
    ['{6EBD45B4-1B78-472C-BDBA-036A6446A933}']
    {class} function _GetFIRED: JCameraCaptureMetaData_FlashState; cdecl;
    {class} function _GetNONE: JCameraCaptureMetaData_FlashState; cdecl;
    {class} function _GetREADY: JCameraCaptureMetaData_FlashState; cdecl;
    {class} function _GetUNKNOWN: JCameraCaptureMetaData_FlashState; cdecl;
    {class} function valueOf(string_: JString): JCameraCaptureMetaData_FlashState; cdecl;
    {class} function values: TJavaObjectArray<JCameraCaptureMetaData_FlashState>; cdecl;
    {class} property FIRED: JCameraCaptureMetaData_FlashState read _GetFIRED;
    {class} property NONE: JCameraCaptureMetaData_FlashState read _GetNONE;
    {class} property READY: JCameraCaptureMetaData_FlashState read _GetREADY;
    {class} property UNKNOWN: JCameraCaptureMetaData_FlashState read _GetUNKNOWN;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureMetaData$FlashState')]
  JCameraCaptureMetaData_FlashState = interface(JEnum)
    ['{7027A78A-4713-4973-B919-3D33FCC99EC8}']
  end;
  TJCameraCaptureMetaData_FlashState = class(TJavaGenericImport<JCameraCaptureMetaData_FlashStateClass, JCameraCaptureMetaData_FlashState>) end;

  JCameraCaptureResultClass = interface(IJavaClass)
    ['{0B54E727-E902-49F6-9E37-9C76016170C7}']
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureResult')]
  JCameraCaptureResult = interface(IJavaInstance)
    ['{1DE21CA2-10EA-49DF-B831-E9CE05213559}']
    function getAeState: JCameraCaptureMetaData_AeState; cdecl;
    function getAfMode: JCameraCaptureMetaData_AfMode; cdecl;
    function getAfState: JCameraCaptureMetaData_AfState; cdecl;
    function getAwbState: JCameraCaptureMetaData_AwbState; cdecl;
    function getFlashState: JCameraCaptureMetaData_FlashState; cdecl;
    function getTagBundle: JTagBundle; cdecl;
    function getTimestamp: Int64; cdecl;
    procedure populateExifData(builder: JExifData_Builder); cdecl;
  end;
  TJCameraCaptureResult = class(TJavaGenericImport<JCameraCaptureResultClass, JCameraCaptureResult>) end;

  JCameraCaptureCallbackClass = interface(JObjectClass)
    ['{D475B18A-8DEF-4A81-B35C-25DA8236462C}']
    {class} function init: JCameraCaptureCallback; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraCaptureCallback')]
  JCameraCaptureCallback = interface(JObject)
    ['{BF43CE48-5F3C-449F-B1F2-E540332857B2}']
    procedure onCaptureCancelled; cdecl;
    procedure onCaptureCompleted(cameraCaptureResult: JCameraCaptureResult); cdecl;
    procedure onCaptureFailed(cameraCaptureFailure: JCameraCaptureFailure); cdecl;
  end;
  TJCameraCaptureCallback = class(TJavaGenericImport<JCameraCaptureCallbackClass, JCameraCaptureCallback>) end;

  JUseCaseConfigFactory_CaptureTypeClass = interface(JEnumClass)
    ['{261F78A4-E592-425A-A7D4-A3DC566266CD}']
    {class} function _GetIMAGE_ANALYSIS: JUseCaseConfigFactory_CaptureType; cdecl;
    {class} function _GetIMAGE_CAPTURE: JUseCaseConfigFactory_CaptureType; cdecl;
    {class} function _GetPREVIEW: JUseCaseConfigFactory_CaptureType; cdecl;
    {class} function _GetVIDEO_CAPTURE: JUseCaseConfigFactory_CaptureType; cdecl;
    {class} function valueOf(string_: JString): JUseCaseConfigFactory_CaptureType; cdecl;
    {class} function values: TJavaObjectArray<JUseCaseConfigFactory_CaptureType>; cdecl;
    {class} property IMAGE_ANALYSIS: JUseCaseConfigFactory_CaptureType read _GetIMAGE_ANALYSIS;
    {class} property IMAGE_CAPTURE: JUseCaseConfigFactory_CaptureType read _GetIMAGE_CAPTURE;
    {class} property PREVIEW: JUseCaseConfigFactory_CaptureType read _GetPREVIEW;
    {class} property VIDEO_CAPTURE: JUseCaseConfigFactory_CaptureType read _GetVIDEO_CAPTURE;
  end;

  [JavaSignature('androidx/camera/core/impl/UseCaseConfigFactory$CaptureType')]
  JUseCaseConfigFactory_CaptureType = interface(JEnum)
    ['{9F900D17-A9F7-4874-B3E6-9073D128FE5F}']
  end;
  TJUseCaseConfigFactory_CaptureType = class(TJavaGenericImport<JUseCaseConfigFactory_CaptureTypeClass, JUseCaseConfigFactory_CaptureType>) end;

  JUseCaseConfigFactoryClass = interface(IJavaClass)
    ['{AD844E26-C04E-49A7-B1D7-248AF162DFA4}']
  end;

  [JavaSignature('androidx/camera/core/impl/UseCaseConfigFactory')]
  JUseCaseConfigFactory = interface(IJavaInstance)
    ['{5901699E-9F3D-469E-AF89-7565ADD412B8}']
    function getConfig(captureType: JUseCaseConfigFactory_CaptureType): Jimpl_Config; cdecl;
  end;
  TJUseCaseConfigFactory = class(TJavaGenericImport<JUseCaseConfigFactoryClass, JUseCaseConfigFactory>) end;

  JCameraFilterClass = interface(IJavaClass)
    ['{D6F0190D-08D7-4262-AA04-10D8C237E3B3}']
  end;

  [JavaSignature('androidx/camera/core/CameraFilter')]
  JCameraFilter = interface(IJavaInstance)
    ['{2E4BE7A4-16A5-4065-9451-22473BD0E67C}']
    function filter(list: JList): JList; cdecl;
  end;
  TJCameraFilter = class(TJavaGenericImport<JCameraFilterClass, JCameraFilter>) end;

  JConfig_OptionMatcherClass = interface(IJavaClass)
    ['{46DD511F-4081-4B15-A770-21DE34311880}']
  end;

  [JavaSignature('androidx/camera/core/impl/Config$OptionMatcher')]
  JConfig_OptionMatcher = interface(IJavaInstance)
    ['{207941C5-4DC5-48E9-95E2-37CFE3A15421}']
    function onOptionMatched(option: JConfig_Option): Boolean; cdecl;
  end;
  TJConfig_OptionMatcher = class(TJavaGenericImport<JConfig_OptionMatcherClass, JConfig_OptionMatcher>) end;

  JConfig_OptionPriorityClass = interface(JEnumClass)
    ['{D959651F-825F-4511-AEAA-9F7F584FC836}']
    {class} function _GetALWAYS_OVERRIDE: JConfig_OptionPriority; cdecl;
    {class} function _GetOPTIONAL: JConfig_OptionPriority; cdecl;
    {class} function _GetREQUIRED: JConfig_OptionPriority; cdecl;
    {class} function valueOf(string_: JString): JConfig_OptionPriority; cdecl;
    {class} function values: TJavaObjectArray<JConfig_OptionPriority>; cdecl;
    {class} property ALWAYS_OVERRIDE: JConfig_OptionPriority read _GetALWAYS_OVERRIDE;
    {class} property OPTIONAL: JConfig_OptionPriority read _GetOPTIONAL;
    {class} property REQUIRED: JConfig_OptionPriority read _GetREQUIRED;
  end;

  [JavaSignature('androidx/camera/core/impl/Config$OptionPriority')]
  JConfig_OptionPriority = interface(JEnum)
    ['{CF05D5C5-BB5C-4E14-B378-5D894745135C}']
  end;
  TJConfig_OptionPriority = class(TJavaGenericImport<JConfig_OptionPriorityClass, JConfig_OptionPriority>) end;

  JConfig_OptionClass = interface(JObjectClass)
    ['{01B4B397-8DD6-4FD3-97F2-FEF5DCCF90A2}']
    {class} function create(string_: JString; class_: Jlang_Class): JConfig_Option; cdecl; overload;
    {class} function create(string_: JString; class_: Jlang_Class; object_: JObject): JConfig_Option; cdecl; overload;
    {class} function init: JConfig_Option; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/Config$Option')]
  JConfig_Option = interface(JObject)
    ['{F0EAB933-7B27-43CC-A5FC-F3720903696E}']
    function getId: JString; cdecl;
    function getToken: JObject; cdecl;
    function getValueClass: Jlang_Class; cdecl;
  end;
  TJConfig_Option = class(TJavaGenericImport<JConfig_OptionClass, JConfig_Option>) end;

  Jimpl_ConfigClass = interface(IJavaClass)
    ['{262842CB-26AF-4FB4-AAC0-36394E6EF818}']
    {class} function hasConflict(optionPriority: JConfig_OptionPriority; optionPriority1: JConfig_OptionPriority): Boolean; cdecl;
    {class} function mergeConfigs(config: Jimpl_Config; config1: Jimpl_Config): Jimpl_Config; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/Config')]
  Jimpl_Config = interface(IJavaInstance)
    ['{0FD2BCA7-0C55-46E4-BE17-B1C64FCFCF8E}']
    function containsOption(option: JConfig_Option): Boolean; cdecl;
    procedure findOptions(string_: JString; optionMatcher: JConfig_OptionMatcher); cdecl;
    function getOptionPriority(option: JConfig_Option): JConfig_OptionPriority; cdecl;
    function getPriorities(option: JConfig_Option): JSet; cdecl;
    function listOptions: JSet; cdecl;
    //*** function retrieveOption(option: JConfig_Option): J; cdecl; overload;
    //*** function retrieveOption(option: JConfig_Option; valueT: J): J; cdecl; overload;
    //*** function retrieveOptionWithPriority(option: JConfig_Option; optionPriority: JConfig_OptionPriority): J; cdecl;
  end;
  TJimpl_Config = class(TJavaGenericImport<Jimpl_ConfigClass, Jimpl_Config>) end;

  JReadableConfigClass = interface(Jimpl_ConfigClass)
    ['{618CFFC5-A2BF-4F1E-BB00-750595B118EE}']
  end;

  [JavaSignature('androidx/camera/core/impl/ReadableConfig')]
  JReadableConfig = interface(Jimpl_Config)
    ['{9559C133-E1F4-41F2-A1C8-48DC113EFDA6}']
    function containsOption(option: JConfig_Option): Boolean; cdecl;
    procedure findOptions(string_: JString; optionMatcher: JConfig_OptionMatcher); cdecl;
    function getConfig: Jimpl_Config; cdecl;
    function getOptionPriority(option: JConfig_Option): JConfig_OptionPriority; cdecl;
    function getPriorities(option: JConfig_Option): JSet; cdecl;
    function listOptions: JSet; cdecl;
    //*** function retrieveOption(option: JConfig_Option): J; cdecl; overload;
    //*** function retrieveOption(option: JConfig_Option; valueT: J): J; cdecl; overload;
    //*** function retrieveOptionWithPriority(option: JConfig_Option; optionPriority: JConfig_OptionPriority): J; cdecl;
  end;
  TJReadableConfig = class(TJavaGenericImport<JReadableConfigClass, JReadableConfig>) end;

  JTargetConfigClass = interface(JReadableConfigClass)
    ['{A78FA339-BA6A-42A4-A1F1-F6B196983D72}']
    {class} function _GetOPTION_TARGET_CLASS: JConfig_Option; cdecl;
    {class} function _GetOPTION_TARGET_NAME: JConfig_Option; cdecl;
    {class} function getTargetClass(class_: Jlang_Class): Jlang_Class; cdecl; overload;
    {class} property OPTION_TARGET_CLASS: JConfig_Option read _GetOPTION_TARGET_CLASS;
    {class} property OPTION_TARGET_NAME: JConfig_Option read _GetOPTION_TARGET_NAME;
  end;

  [JavaSignature('androidx/camera/core/internal/TargetConfig')]
  JTargetConfig = interface(JReadableConfig)
    ['{2BE90874-5DFF-4502-A2A7-AF5B6DF4D584}']
    function getTargetClass: Jlang_Class; cdecl; overload;
    function getTargetName: JString; cdecl; overload;
    function getTargetName(string_: JString): JString; cdecl; overload;
  end;
  TJTargetConfig = class(TJavaGenericImport<JTargetConfigClass, JTargetConfig>) end;

  JCameraThreadConfigClass = interface(JObjectClass)
    ['{6F14B91D-1385-48A6-8E5D-DD5F7B148433}']
    {class} function create(executor: JExecutor; handler: JHandler): JCameraThreadConfig; cdecl;
    {class} function init: JCameraThreadConfig; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraThreadConfig')]
  JCameraThreadConfig = interface(JObject)
    ['{019DD342-1F6E-44A6-BA79-8C35807F24AB}']
    function getCameraExecutor: JExecutor; cdecl;
    function getSchedulerHandler: JHandler; cdecl;
  end;
  TJCameraThreadConfig = class(TJavaGenericImport<JCameraThreadConfigClass, JCameraThreadConfig>) end;

  JCameraFactoryClass = interface(IJavaClass)
    ['{7136DAC2-4BE0-4736-80D1-EF1AD2F188F8}']
  end;

  [JavaSignature('androidx/camera/core/impl/CameraFactory')]
  JCameraFactory = interface(IJavaInstance)
    ['{A673DC89-04CB-49F4-A6AD-F3C3FA623517}']
    function getAvailableCameraIds: JSet; cdecl;
    function getCamera(string_: JString): JCameraInternal; cdecl;
    function getCameraManager: JObject; cdecl;
  end;
  TJCameraFactory = class(TJavaGenericImport<JCameraFactoryClass, JCameraFactory>) end;

  JCameraFactory_ProviderClass = interface(IJavaClass)
    ['{5DDF0E65-767A-416A-B703-FDEA2D8812AE}']
  end;

  [JavaSignature('androidx/camera/core/impl/CameraFactory$Provider')]
  JCameraFactory_Provider = interface(IJavaInstance)
    ['{3970BFD8-B5DD-426B-B17A-29290273C475}']
    function newInstance(context: JContext; cameraThreadConfig: JCameraThreadConfig; cameraSelector: JCameraSelector): JCameraFactory; cdecl;
  end;
  TJCameraFactory_Provider = class(TJavaGenericImport<JCameraFactory_ProviderClass, JCameraFactory_Provider>) end;

  JSurfaceConfig_ConfigSizeClass = interface(JEnumClass)
    ['{60E4FF57-89E4-411C-AA54-E28D4626F91C}']
    {class} function _GetANALYSIS: JSurfaceConfig_ConfigSize; cdecl;
    {class} function _GetMAXIMUM: JSurfaceConfig_ConfigSize; cdecl;
    {class} function _GetNOT_SUPPORT: JSurfaceConfig_ConfigSize; cdecl;
    {class} function _GetPREVIEW: JSurfaceConfig_ConfigSize; cdecl;
    {class} function _GetRECORD: JSurfaceConfig_ConfigSize; cdecl;
    {class} function valueOf(string_: JString): JSurfaceConfig_ConfigSize; cdecl;
    {class} function values: TJavaObjectArray<JSurfaceConfig_ConfigSize>; cdecl;//Deprecated
    {class} property ANALYSIS: JSurfaceConfig_ConfigSize read _GetANALYSIS;
    {class} property MAXIMUM: JSurfaceConfig_ConfigSize read _GetMAXIMUM;
    {class} property NOT_SUPPORT: JSurfaceConfig_ConfigSize read _GetNOT_SUPPORT;
    {class} property PREVIEW: JSurfaceConfig_ConfigSize read _GetPREVIEW;
    {class} property &RECORD: JSurfaceConfig_ConfigSize read _GetRECORD;
  end;

  [JavaSignature('androidx/camera/core/impl/SurfaceConfig$ConfigSize')]
  JSurfaceConfig_ConfigSize = interface(JEnum)
    ['{32C43FC6-CE89-4E48-B000-3C1A911703FF}']
  end;
  TJSurfaceConfig_ConfigSize = class(TJavaGenericImport<JSurfaceConfig_ConfigSizeClass, JSurfaceConfig_ConfigSize>) end;

  JSurfaceConfig_ConfigTypeClass = interface(JEnumClass)
    ['{2C2696C5-D278-4640-864C-D56D4A26AE0C}']
    {class} function _GetJPEG: JSurfaceConfig_ConfigType; cdecl;
    {class} function _GetPRIV: JSurfaceConfig_ConfigType; cdecl;
    {class} function _GetRAW: JSurfaceConfig_ConfigType; cdecl;
    {class} function _GetYUV: JSurfaceConfig_ConfigType; cdecl;
    {class} function valueOf(string_: JString): JSurfaceConfig_ConfigType; cdecl;
    {class} function values: TJavaObjectArray<JSurfaceConfig_ConfigType>; cdecl;//Deprecated
    {class} property JPEG: JSurfaceConfig_ConfigType read _GetJPEG;
    {class} property PRIV: JSurfaceConfig_ConfigType read _GetPRIV;
    {class} property RAW: JSurfaceConfig_ConfigType read _GetRAW;
    {class} property YUV: JSurfaceConfig_ConfigType read _GetYUV;
  end;

  [JavaSignature('androidx/camera/core/impl/SurfaceConfig$ConfigType')]
  JSurfaceConfig_ConfigType = interface(JEnum)
    ['{9CC669EE-8F9B-4586-BBA3-AE3A6AEB3E5A}']
  end;
  TJSurfaceConfig_ConfigType = class(TJavaGenericImport<JSurfaceConfig_ConfigTypeClass, JSurfaceConfig_ConfigType>) end;

  JSurfaceConfigClass = interface(JObjectClass)
    ['{69FEB2CD-0A1E-4AF8-B2E3-813E7DEFB091}']
    {class} function create(configType: JSurfaceConfig_ConfigType; configSize: JSurfaceConfig_ConfigSize): JSurfaceConfig; cdecl;
    {class} function init: JSurfaceConfig; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/SurfaceConfig')]
  JSurfaceConfig = interface(JObject)
    ['{4041D3CF-F6F0-4ECE-87C5-9A8EC557307D}']
    function getConfigSize: JSurfaceConfig_ConfigSize; cdecl;
    function getConfigType: JSurfaceConfig_ConfigType; cdecl;
    function isSupported(surfaceConfig: JSurfaceConfig): Boolean; cdecl;
  end;
  TJSurfaceConfig = class(TJavaGenericImport<JSurfaceConfigClass, JSurfaceConfig>) end;

  JCameraDeviceSurfaceManagerClass = interface(IJavaClass)
    ['{071E7841-8FD6-4C40-84F5-3B9E348C1A6A}']
  end;

  [JavaSignature('androidx/camera/core/impl/CameraDeviceSurfaceManager')]
  JCameraDeviceSurfaceManager = interface(IJavaInstance)
    ['{80F14369-E39C-469F-B0C6-F72522D48FD2}']
    function checkSupported(string_: JString; list: JList): Boolean; cdecl;
    function getSuggestedResolutions(string_: JString; list: JList; list1: JList): JMap; cdecl;
    function transformSurfaceConfig(string_: JString; i: Integer; size: Jutil_Size): JSurfaceConfig; cdecl;
  end;
  TJCameraDeviceSurfaceManager = class(TJavaGenericImport<JCameraDeviceSurfaceManagerClass, JCameraDeviceSurfaceManager>) end;

  JCameraDeviceSurfaceManager_ProviderClass = interface(IJavaClass)
    ['{ED4F3C12-3702-4221-839B-6220FA9DBA9C}']
  end;

  [JavaSignature('androidx/camera/core/impl/CameraDeviceSurfaceManager$Provider')]
  JCameraDeviceSurfaceManager_Provider = interface(IJavaInstance)
    ['{958A3143-28FD-48FF-95A5-DBE177E46FCC}']
    function newInstance(context: JContext; object_: JObject; set_: JSet): JCameraDeviceSurfaceManager; cdecl;
  end;
  TJCameraDeviceSurfaceManager_Provider = class(TJavaGenericImport<JCameraDeviceSurfaceManager_ProviderClass, JCameraDeviceSurfaceManager_Provider>) end;

  JUseCaseConfigFactory_ProviderClass = interface(IJavaClass)
    ['{D80D5CE9-94BA-45F9-AFF7-105E6E60AC48}']
  end;

  [JavaSignature('androidx/camera/core/impl/UseCaseConfigFactory$Provider')]
  JUseCaseConfigFactory_Provider = interface(IJavaInstance)
    ['{23450F4C-1FDC-4773-A426-2D165C12D209}']
    function newInstance(context: JContext): JUseCaseConfigFactory; cdecl;
  end;
  TJUseCaseConfigFactory_Provider = class(TJavaGenericImport<JUseCaseConfigFactory_ProviderClass, JUseCaseConfigFactory_Provider>) end;

  JTargetConfig_BuilderClass = interface(IJavaClass)
    ['{6013CD65-CF88-45DC-89AB-65D697749DA5}']
  end;

  [JavaSignature('androidx/camera/core/internal/TargetConfig$Builder')]
  JTargetConfig_Builder = interface(IJavaInstance)
    ['{00E744C7-FE93-484B-A85F-75ED6AA260EE}']
    function setTargetClass(class_: Jlang_Class): JObject; cdecl;
    function setTargetName(string_: JString): JObject; cdecl;
  end;
  TJTargetConfig_Builder = class(TJavaGenericImport<JTargetConfig_BuilderClass, JTargetConfig_Builder>) end;

  JCameraXConfigClass = interface(JTargetConfigClass)
    ['{511298E3-5196-46EB-82CD-2D2F95BC326F}']
    {class} function _GetOPTION_CAMERA_FACTORY_PROVIDER: JConfig_Option; cdecl;
    {class} property OPTION_CAMERA_FACTORY_PROVIDER: JConfig_Option read _GetOPTION_CAMERA_FACTORY_PROVIDER;
  end;

  [JavaSignature('androidx/camera/core/CameraXConfig')]
  JCameraXConfig = interface(JTargetConfig)
    ['{261CD743-210C-49A9-B6F8-4BE3B4C12636}']
    function getAvailableCamerasLimiter(cameraSelector: JCameraSelector): JCameraSelector; cdecl;
    function getCameraExecutor(executor: JExecutor): JExecutor; cdecl;
    function getCameraFactoryProvider(provider: JCameraFactory_Provider): JCameraFactory_Provider; cdecl;
    function getConfig: Jimpl_Config; cdecl;
    function getDeviceSurfaceManagerProvider(provider: JCameraDeviceSurfaceManager_Provider): JCameraDeviceSurfaceManager_Provider; cdecl;
    function getMinimumLoggingLevel: Integer; cdecl;
    function getSchedulerHandler(handler: JHandler): JHandler; cdecl;
    function getUseCaseConfigFactoryProvider(provider: JUseCaseConfigFactory_Provider): JUseCaseConfigFactory_Provider; cdecl;
  end;
  TJCameraXConfig = class(TJavaGenericImport<JCameraXConfigClass, JCameraXConfig>) end;

  JCameraConfigClass = interface(JReadableConfigClass)
    ['{B1670484-7064-4651-B9F8-1041D65CBEC6}']
    {class} function _GetOPTION_CAMERA_FILTER: JConfig_Option; cdecl;
    {class} function _GetOPTION_USECASE_CONFIG_FACTORY: JConfig_Option; cdecl;
    {class} function getCameraFilter: JCameraFilter; cdecl;
    {class} property OPTION_CAMERA_FILTER: JConfig_Option read _GetOPTION_CAMERA_FILTER;
    {class} property OPTION_USECASE_CONFIG_FACTORY: JConfig_Option read _GetOPTION_USECASE_CONFIG_FACTORY;
  end;

  JCameraXConfig_BuilderClass = interface(JTargetConfig_BuilderClass)
    ['{0BF5591D-A397-4608-97C1-32A7EFF09CBE}']
    {class} function fromConfig(cameraXConfig: JCameraXConfig): JCameraXConfig_Builder; cdecl;
    {class} function init: JCameraXConfig_Builder; cdecl;//Deprecated
  end;

  [JavaSignature('androidx/camera/core/CameraXConfig$Builder')]
  JCameraXConfig_Builder = interface(JTargetConfig_Builder)
    ['{A9750925-27AA-4997-8277-040EA3EE240B}']
    function build: JCameraXConfig; cdecl;
    function setAvailableCamerasLimiter(cameraSelector: JCameraSelector): JCameraXConfig_Builder; cdecl;
    function setCameraExecutor(executor: JExecutor): JCameraXConfig_Builder; cdecl;
    function setCameraFactoryProvider(provider: JCameraFactory_Provider): JCameraXConfig_Builder; cdecl;
    function setDeviceSurfaceManagerProvider(provider: JCameraDeviceSurfaceManager_Provider): JCameraXConfig_Builder; cdecl;
    function setMinimumLoggingLevel(i: Integer): JCameraXConfig_Builder; cdecl;
    function setSchedulerHandler(handler: JHandler): JCameraXConfig_Builder; cdecl;
    function setTargetClass(class_: Jlang_Class): JCameraXConfig_Builder; cdecl;
    function setTargetName(string_: JString): JCameraXConfig_Builder; cdecl;
    function setUseCaseConfigFactoryProvider(provider: JUseCaseConfigFactory_Provider): JCameraXConfig_Builder; cdecl;
  end;
  TJCameraXConfig_Builder = class(TJavaGenericImport<JCameraXConfig_BuilderClass, JCameraXConfig_Builder>) end;

  JCameraXConfig_ProviderClass = interface(IJavaClass)
    ['{4E2DB5E3-E2E6-4983-93F4-0E9525AB44E8}']
  end;

  [JavaSignature('androidx/camera/core/CameraXConfig$Provider')]
  JCameraXConfig_Provider = interface(IJavaInstance)
    ['{32882921-3C62-4208-B1EE-ADC0AA2990C8}']
    function getCameraXConfig: JCameraXConfig; cdecl;
  end;
  TJCameraXConfig_Provider = class(TJavaGenericImport<JCameraXConfig_ProviderClass, JCameraXConfig_Provider>) end;

  [JavaSignature('androidx/camera/core/impl/CameraConfig')]
  JCameraConfig = interface(JReadableConfig)
    ['{74A827EE-7AD0-4114-BCD6-61A9EFA96CC6}']
    function getUseCaseConfigFactory: JUseCaseConfigFactory; cdecl;
  end;
  TJCameraConfig = class(TJavaGenericImport<JCameraConfigClass, JCameraConfig>) end;

  JCameraConfig_BuilderClass = interface(IJavaClass)
    ['{4F39DC17-D5F5-4094-9CAC-F19E7CA514C5}']
  end;

  [JavaSignature('androidx/camera/core/impl/CameraConfig$Builder')]
  JCameraConfig_Builder = interface(IJavaInstance)
    ['{3B48DBBA-80A7-4997-9EB1-DC633E10CECE}']
    function setCameraFilter(cameraFilter: JCameraFilter): JObject; cdecl;
    function setUseCaseConfigFactory(useCaseConfigFactory: JUseCaseConfigFactory): JObject; cdecl;
  end;
  TJCameraConfig_Builder = class(TJavaGenericImport<JCameraConfig_BuilderClass, JCameraConfig_Builder>) end;

  JExposureStateClass = interface(IJavaClass)
    ['{80DE2726-BDC5-4D36-8C62-7A369260EE0D}']
  end;

  [JavaSignature('androidx/camera/core/ExposureState')]
  JExposureState = interface(IJavaInstance)
    ['{BCCDC44B-4B2B-4531-AC9E-C8AE4F42E6A0}']
    function getExposureCompensationIndex: Integer; cdecl;
    function getExposureCompensationRange: JRange; cdecl;
    function getExposureCompensationStep: JRational; cdecl;
    function isExposureCompensationSupported: Boolean; cdecl;
  end;
  TJExposureState = class(TJavaGenericImport<JExposureStateClass, JExposureState>) end;

  JFocusMeteringActionClass = interface(JObjectClass)
    ['{0CDEDBD8-3375-429D-9318-7507BD195245}']
    {class} function _GetFLAG_AE: Integer; cdecl;
    {class} function _GetFLAG_AF: Integer; cdecl;
    {class} function _GetFLAG_AWB: Integer; cdecl;
    {class} property FLAG_AE: Integer read _GetFLAG_AE;
    {class} property FLAG_AF: Integer read _GetFLAG_AF;
    {class} property FLAG_AWB: Integer read _GetFLAG_AWB;
  end;

  [JavaSignature('androidx/camera/core/FocusMeteringAction')]
  JFocusMeteringAction = interface(JObject)
    ['{A8C303F1-C875-437B-836B-3A12DE6D8853}']
    function getAutoCancelDurationInMillis: Int64; cdecl;
    function getMeteringPointsAe: JList; cdecl;
    function getMeteringPointsAf: JList; cdecl;
    function getMeteringPointsAwb: JList; cdecl;
    function isAutoCancelEnabled: Boolean; cdecl;
  end;
  TJFocusMeteringAction = class(TJavaGenericImport<JFocusMeteringActionClass, JFocusMeteringAction>) end;

  JFocusMeteringAction_BuilderClass = interface(JObjectClass)
    ['{4930FA71-6ABD-4690-B783-48F5CE36D52C}']
    {class} function init(meteringPoint: JMeteringPoint): JFocusMeteringAction_Builder; cdecl; overload;
    {class} function init(meteringPoint: JMeteringPoint; i: Integer): JFocusMeteringAction_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/camera/core/FocusMeteringAction$Builder')]
  JFocusMeteringAction_Builder = interface(JObject)
    ['{BD1B0A97-6CEC-451C-BA46-FC0C1E0E5003}']
    function addPoint(meteringPoint: JMeteringPoint): JFocusMeteringAction_Builder; cdecl; overload;
    function addPoint(meteringPoint: JMeteringPoint; i: Integer): JFocusMeteringAction_Builder; cdecl; overload;
    function build: JFocusMeteringAction; cdecl;
    function disableAutoCancel: JFocusMeteringAction_Builder; cdecl;
    function setAutoCancelDuration(l: Int64; timeUnit: JTimeUnit): JFocusMeteringAction_Builder; cdecl;
  end;
  TJFocusMeteringAction_Builder = class(TJavaGenericImport<JFocusMeteringAction_BuilderClass, JFocusMeteringAction_Builder>) end;

  JFocusMeteringAction_MeteringModeClass = interface(JAnnotationClass)
    ['{B445A491-B0CF-4FD7-A512-F1093FE5BACC}']
  end;

  [JavaSignature('androidx/camera/core/FocusMeteringAction$MeteringMode')]
  JFocusMeteringAction_MeteringMode = interface(JAnnotation)
    ['{A5435B0E-25CA-4ECB-B873-227BC56778E0}']
  end;
  TJFocusMeteringAction_MeteringMode = class(TJavaGenericImport<JFocusMeteringAction_MeteringModeClass, JFocusMeteringAction_MeteringMode>) end;

  JFocusMeteringResultClass = interface(JObjectClass)
    ['{D05B7AB5-443D-40F1-ADD6-7EAD7712171F}']
    {class} function create(b: Boolean): JFocusMeteringResult; cdecl;
    {class} function emptyInstance: JFocusMeteringResult; cdecl;
  end;

  [JavaSignature('androidx/camera/core/FocusMeteringResult')]
  JFocusMeteringResult = interface(JObject)
    ['{F44777EA-53C0-4C14-978D-15345993868B}']
    function isFocusSuccessful: Boolean; cdecl;
  end;
  TJFocusMeteringResult = class(TJavaGenericImport<JFocusMeteringResultClass, JFocusMeteringResult>) end;

  Jcore_CameraClass = interface(IJavaClass)
    ['{E50B099B-639B-4004-B67A-436D3CC5ECD8}']
  end;

  [JavaSignature('androidx/camera/core/Camera')]
  Jcore_Camera = interface(IJavaInstance)
    ['{E51B91AD-AF69-43C8-9348-BA60E0701BE4}']
    function getCameraControl: JCameraControl; cdecl;
    function getCameraInfo: JCameraInfo; cdecl;
    function getCameraInternals: JLinkedHashSet; cdecl;
    function getExtendedConfig: JCameraConfig; cdecl;
    procedure setExtendedConfig(cameraConfig: JCameraConfig); cdecl;
  end;
  TJcore_Camera = class(TJavaGenericImport<Jcore_CameraClass, Jcore_Camera>) end;

  JCameraUseCaseAdapterClass = interface(Jcore_CameraClass)
    ['{E72C8528-6FA6-4B53-B856-48391E0DE87F}']
    {class} function generateCameraId(linkedHashSet: JLinkedHashSet): JCameraUseCaseAdapter_CameraId; cdecl;
    {class} function init(linkedHashSet: JLinkedHashSet; cameraDeviceSurfaceManager: JCameraDeviceSurfaceManager; useCaseConfigFactory: JUseCaseConfigFactory): JCameraUseCaseAdapter; cdecl;//Deprecated
  end;

  [JavaSignature('androidx/camera/core/internal/CameraUseCaseAdapter')]
  JCameraUseCaseAdapter = interface(Jcore_Camera)
    ['{922B0EBD-274B-4F50-8851-D9D26A460C7C}']
    procedure addUseCases(collection: JCollection); cdecl;
    procedure attachUseCases; cdecl;
    procedure checkAttachUseCases(list: JList); cdecl;
    procedure detachUseCases; cdecl;
    function getCameraControl: JCameraControl; cdecl;
    function getCameraId: JCameraUseCaseAdapter_CameraId; cdecl;
    function getCameraInfo: JCameraInfo; cdecl;
    function getCameraInternals: JLinkedHashSet; cdecl;
    function getExtendedConfig: JCameraConfig; cdecl;
    function getUseCases: JList; cdecl;
    function isEquivalent(cameraUseCaseAdapter: JCameraUseCaseAdapter): Boolean; cdecl;
    procedure removeUseCases(collection: JCollection); cdecl;
    procedure setExtendedConfig(cameraConfig: JCameraConfig); cdecl;
    procedure setViewPort(viewPort: JViewPort); cdecl;
  end;
  TJCameraUseCaseAdapter = class(TJavaGenericImport<JCameraUseCaseAdapterClass, JCameraUseCaseAdapter>) end;

  JCameraControlClass = interface(IJavaClass)
    ['{9DCD94AE-41C7-4512-9CB4-4947038211D3}']
  end;

  [JavaSignature('androidx/camera/core/CameraControl')]
  JCameraControl = interface(IJavaInstance)
    ['{DB61AE1B-9182-4734-B882-52E88CF46DDF}']
    function cancelFocusAndMetering: JListenableFuture; cdecl;
    function enableTorch(b: Boolean): JListenableFuture; cdecl;
    function setExposureCompensationIndex(i: Integer): JListenableFuture; cdecl;
    function setLinearZoom(f: Single): JListenableFuture; cdecl;
    function setZoomRatio(f: Single): JListenableFuture; cdecl;
    function startFocusAndMetering(focusMeteringAction: JFocusMeteringAction): JListenableFuture; cdecl;
  end;
  TJCameraControl = class(TJavaGenericImport<JCameraControlClass, JCameraControl>) end;

  JCameraControlInternalClass = interface(JCameraControlClass)
    ['{18CE5F7B-4FBD-47A8-B185-C6FA24EAD0E7}']
    {class} function _GetDEFAULT_EMPTY_INSTANCE: JCameraControlInternal; cdecl;
    {class} property DEFAULT_EMPTY_INSTANCE: JCameraControlInternal read _GetDEFAULT_EMPTY_INSTANCE;
  end;

  [JavaSignature('androidx/camera/core/impl/CameraControlInternal')]
  JCameraControlInternal = interface(JCameraControl)
    ['{D4A69ACC-5F54-4B3C-9D74-8F300EA316E8}']
    procedure addInteropConfig(config: Jimpl_Config); cdecl;
    procedure cancelAfAeTrigger(b: Boolean; b1: Boolean); cdecl;
    procedure clearInteropConfig; cdecl;
    function getFlashMode: Integer; cdecl;
    function getInteropConfig: Jimpl_Config; cdecl;
    function getSensorRect: JRect; cdecl;
    function setExposureCompensationIndex(i: Integer): JListenableFuture; cdecl;
    procedure setFlashMode(i: Integer); cdecl;
    procedure submitCaptureRequests(list: JList); cdecl;
    function triggerAePrecapture: JListenableFuture; cdecl;
    function triggerAf: JListenableFuture; cdecl;
  end;
  TJCameraControlInternal = class(TJavaGenericImport<JCameraControlInternalClass, JCameraControlInternal>) end;

  JCameraInfoClass = interface(IJavaClass)
    ['{48694E71-37C7-4781-9C29-D78C0FB5B5F9}']
    {class} function _GetIMPLEMENTATION_TYPE_CAMERA2: JString; cdecl;
    {class} function _GetIMPLEMENTATION_TYPE_CAMERA2_LEGACY: JString; cdecl;
    {class} function _GetIMPLEMENTATION_TYPE_FAKE: JString; cdecl;
    {class} function _GetIMPLEMENTATION_TYPE_UNKNOWN: JString; cdecl;
    {class} function getSensorRotationDegrees: Integer; cdecl; overload;
    {class} property IMPLEMENTATION_TYPE_CAMERA2: JString read _GetIMPLEMENTATION_TYPE_CAMERA2;
    {class} property IMPLEMENTATION_TYPE_CAMERA2_LEGACY: JString read _GetIMPLEMENTATION_TYPE_CAMERA2_LEGACY;
    {class} property IMPLEMENTATION_TYPE_FAKE: JString read _GetIMPLEMENTATION_TYPE_FAKE;
    {class} property IMPLEMENTATION_TYPE_UNKNOWN: JString read _GetIMPLEMENTATION_TYPE_UNKNOWN;
  end;

  [JavaSignature('androidx/camera/core/CameraInfo')]
  JCameraInfo = interface(IJavaInstance)
    ['{126F6634-3DA5-495B-9370-369D7CA7E150}']
    function getExposureState: JExposureState; cdecl;
    function getImplementationType: JString; cdecl;
    function getSensorRotationDegrees(i: Integer): Integer; cdecl; overload;
    function getTorchState: JLiveData; cdecl;
    function getZoomState: JLiveData; cdecl;
    function hasFlashUnit: Boolean; cdecl;
  end;
  TJCameraInfo = class(TJavaGenericImport<JCameraInfoClass, JCameraInfo>) end;

  JCameraInfoInternalClass = interface(JCameraInfoClass)
    ['{D11DF848-2BAC-4C02-892F-428C098ECC59}']
  end;

  [JavaSignature('androidx/camera/core/impl/CameraInfoInternal')]
  JCameraInfoInternal = interface(JCameraInfo)
    ['{8CA422A9-6405-47CC-AC8E-AE08CF0371D6}']
    procedure addSessionCaptureCallback(executor: JExecutor; cameraCaptureCallback: JCameraCaptureCallback); cdecl;
    function getCameraId: JString; cdecl;
    function getCameraQuirks: JQuirks; cdecl;
    function getLensFacing: JInteger; cdecl;
    procedure removeSessionCaptureCallback(cameraCaptureCallback: JCameraCaptureCallback); cdecl;
  end;
  TJCameraInfoInternal = class(TJavaGenericImport<JCameraInfoInternalClass, JCameraInfoInternal>) end;

  JCameraInternalClass = interface(Jcore_CameraClass)
    ['{BD6248C7-CCA8-4D69-A45F-BA382CB163BF}']
  end;

  [JavaSignature('androidx/camera/core/impl/CameraInternal')]
  JCameraInternal = interface(Jcore_Camera)
    ['{8EC32BD6-FD2D-49C7-A1BF-3B81B136C0EF}']
    procedure attachUseCases(collection: JCollection); cdecl;
    procedure close; cdecl;
    procedure detachUseCases(collection: JCollection); cdecl;
    function getCameraControl: JCameraControl; cdecl;
    function getCameraControlInternal: JCameraControlInternal; cdecl;
    function getCameraInfo: JCameraInfo; cdecl;
    function getCameraInfoInternal: JCameraInfoInternal; cdecl;
    function getCameraInternals: JLinkedHashSet; cdecl;
    function getCameraState: Jimpl_Observable; cdecl;
    function getExtendedConfig: JCameraConfig; cdecl;
    procedure open; cdecl;
    function release: JListenableFuture; cdecl;
    procedure setExtendedConfig(cameraConfig: JCameraConfig); cdecl;
  end;
  TJCameraInternal = class(TJavaGenericImport<JCameraInternalClass, JCameraInternal>) end;

  JSurfaceRequest_TransformationInfoClass = interface(JObjectClass)
    ['{B8AECD50-CA29-40DD-A8BE-8D974E5D9C5D}']
    {class} function &of(rect: JRect; i: Integer; i1: Integer): JSurfaceRequest_TransformationInfo; cdecl;
  end;

  [JavaSignature('androidx/camera/core/SurfaceRequest$TransformationInfo')]
  JSurfaceRequest_TransformationInfo = interface(JObject)
    ['{6EC7D788-9242-4BBD-99F1-0FA202C4DCFD}']
    function getCropRect: JRect; cdecl;
    function getRotationDegrees: Integer; cdecl;
    function getTargetRotation: Integer; cdecl;
  end;
  TJSurfaceRequest_TransformationInfo = class(TJavaGenericImport<JSurfaceRequest_TransformationInfoClass, JSurfaceRequest_TransformationInfo>) end;

  JSurfaceRequest_TransformationInfoListenerClass = interface(IJavaClass)
    ['{C290328A-0CC4-4055-B891-C2741EA5129A}']
  end;

  [JavaSignature('androidx/camera/core/SurfaceRequest$TransformationInfoListener')]
  JSurfaceRequest_TransformationInfoListener = interface(IJavaInstance)
    ['{94F4E785-E213-41E2-81CD-78B8B662EE26}']
    procedure onTransformationInfoUpdate(transformationInfo: JSurfaceRequest_TransformationInfo); cdecl;
  end;
  TJSurfaceRequest_TransformationInfoListener = class(TJavaGenericImport<JSurfaceRequest_TransformationInfoListenerClass, JSurfaceRequest_TransformationInfoListener>) end;

  JSurfaceRequestClass = interface(JObjectClass)
    ['{4F999284-4AB1-4879-B8D5-8C7294378EAB}']
    {class} function init(size: Jutil_Size; cameraInternal: JCameraInternal; b: Boolean): JSurfaceRequest; cdecl;
  end;

  [JavaSignature('androidx/camera/core/SurfaceRequest')]
  JSurfaceRequest = interface(JObject)
    ['{52226695-756D-4F80-800F-2D10DB80B43E}']
    function _GetmSurfaceFuture: JListenableFuture; cdecl;
    procedure addRequestCancellationListener(executor: JExecutor; runnable: JRunnable); cdecl;
    procedure clearTransformationInfoListener; cdecl;
    function getCamera: JCameraInternal; cdecl;
    function getDeferrableSurface: JDeferrableSurface; cdecl;
    function getResolution: Jutil_Size; cdecl;
    function isRGBA8888Required: Boolean; cdecl;
    //### procedure provideSurface(surface: JSurface; executor: JExecutor; consumer: Jutil_Consumer); cdecl; // androidx.core.util.Consumer
    procedure setTransformationInfoListener(executor: JExecutor; transformationInfoListener: JSurfaceRequest_TransformationInfoListener); cdecl;
    procedure updateTransformationInfo(transformationInfo: JSurfaceRequest_TransformationInfo); cdecl;
    function willNotProvideSurface: Boolean; cdecl;
    property mSurfaceFuture: JListenableFuture read _GetmSurfaceFuture;
  end;
  TJSurfaceRequest = class(TJavaGenericImport<JSurfaceRequestClass, JSurfaceRequest>) end;

  JPreviewTransformationClass = interface(JObjectClass)
    ['{EA73EE65-C468-4165-A7A6-CBD40128C027}']
    {class} function init: JPreviewTransformation; cdecl;
  end;

  [JavaSignature('androidx/camera/view/PreviewTransformation')]
  JPreviewTransformation = interface(JObject)
    ['{A7144EE8-FB00-46CA-99FA-3213504ED06A}']
    function createTransformedBitmap(bitmap: JBitmap; size: Jutil_Size; i: Integer): JBitmap; cdecl;
    function getPreviewViewToNormalizedSurfaceMatrix(size: Jutil_Size; i: Integer): JMatrix; cdecl;
    function getPreviewViewViewportRectForMismatchedAspectRatios(size: Jutil_Size; i: Integer): JRectF; cdecl;
    function getSurfaceToPreviewViewMatrix(size: Jutil_Size; i: Integer): JMatrix; cdecl;
    function getTextureViewCorrectionMatrix: JMatrix; cdecl;
    function isViewportAspectRatioMatchPreviewView(size: Jutil_Size): Boolean; cdecl;
    procedure setScaleType(scaleType: JPreviewView_ScaleType); cdecl;
    procedure transformView(size: Jutil_Size; i: Integer; view: JView); cdecl;
  end;
  TJPreviewTransformation = class(TJavaGenericImport<JPreviewTransformationClass, JPreviewTransformation>) end;

  JCameraSelectorClass = interface(JObjectClass)
    ['{D4EAE632-C5B7-4175-B00C-B5E5420FC23A}']
    {class} function _GetDEFAULT_BACK_CAMERA: JCameraSelector; cdecl;
    {class} function _GetDEFAULT_FRONT_CAMERA: JCameraSelector; cdecl;
    {class} function _GetLENS_FACING_BACK: Integer; cdecl;
    {class} function _GetLENS_FACING_FRONT: Integer; cdecl;
    {class} property DEFAULT_BACK_CAMERA: JCameraSelector read _GetDEFAULT_BACK_CAMERA;
    {class} property DEFAULT_FRONT_CAMERA: JCameraSelector read _GetDEFAULT_FRONT_CAMERA;
    {class} property LENS_FACING_BACK: Integer read _GetLENS_FACING_BACK;
    {class} property LENS_FACING_FRONT: Integer read _GetLENS_FACING_FRONT;
  end;

  [JavaSignature('androidx/camera/core/CameraSelector')]
  JCameraSelector = interface(JObject)
    ['{9D9452C1-018E-40A0-A300-A4AA449E1854}']
    function filter(linkedHashSet: JLinkedHashSet): JLinkedHashSet; cdecl; overload;
    function filter(list: JList): JList; cdecl; overload;
    function getCameraFilterSet: JLinkedHashSet; cdecl;
    function getLensFacing: JInteger; cdecl;
    function select(linkedHashSet: JLinkedHashSet): JCameraInternal; cdecl;
  end;
  TJCameraSelector = class(TJavaGenericImport<JCameraSelectorClass, JCameraSelector>) end;

  JCameraSelector_BuilderClass = interface(JObjectClass)
    ['{F35F86D0-5246-4C56-88AE-E4ABA064FC2E}']
    {class} function fromSelector(cameraSelector: JCameraSelector): JCameraSelector_Builder; cdecl;
    {class} function init: JCameraSelector_Builder; cdecl;
  end;

  [JavaSignature('androidx/camera/core/CameraSelector$Builder')]
  JCameraSelector_Builder = interface(JObject)
    ['{949628FB-9006-4B2E-8B11-7EB24FE0AB7F}']
    function addCameraFilter(cameraFilter: JCameraFilter): JCameraSelector_Builder; cdecl;
    function build: JCameraSelector; cdecl;
    function requireLensFacing(i: Integer): JCameraSelector_Builder; cdecl;
  end;
  TJCameraSelector_Builder = class(TJavaGenericImport<JCameraSelector_BuilderClass, JCameraSelector_Builder>) end;

  JCameraSelector_LensFacingClass = interface(JAnnotationClass)
    ['{E8139714-CE8C-492D-9784-B0CD93BF50F0}']
  end;

  [JavaSignature('androidx/camera/core/CameraSelector$LensFacing')]
  JCameraSelector_LensFacing = interface(JAnnotation)
    ['{025B2128-0A3C-42DF-8BE6-56E65D999F5E}']
  end;
  TJCameraSelector_LensFacing = class(TJavaGenericImport<JCameraSelector_LensFacingClass, JCameraSelector_LensFacing>) end;

  JCaptureConfigClass = interface(JObjectClass)
    ['{044B86C7-196B-41E0-8199-C940A0561833}']
    {class} function _GetOPTION_JPEG_QUALITY: JConfig_Option; cdecl;
    {class} function _GetOPTION_ROTATION: JConfig_Option; cdecl;
    {class} function defaultEmptyCaptureConfig: JCaptureConfig; cdecl;
    {class} property OPTION_JPEG_QUALITY: JConfig_Option read _GetOPTION_JPEG_QUALITY;
    {class} property OPTION_ROTATION: JConfig_Option read _GetOPTION_ROTATION;
  end;

  [JavaSignature('androidx/camera/core/impl/CaptureConfig')]
  JCaptureConfig = interface(JObject)
    ['{4E944726-1773-4F7F-88EC-B57FA7E6D7E2}']
    function getCameraCaptureCallbacks: JList; cdecl;
    function getImplementationOptions: Jimpl_Config; cdecl;
    function getSurfaces: JList; cdecl;
    function getTagBundle: JTagBundle; cdecl;
    function getTemplateType: Integer; cdecl;
    function isUseRepeatingSurface: Boolean; cdecl;
  end;
  TJCaptureConfig = class(TJavaGenericImport<JCaptureConfigClass, JCaptureConfig>) end;

  JCaptureConfig_BuilderClass = interface(JObjectClass)
    ['{E50A10D7-6DEB-4B31-8D6E-C9C5453078DE}']
    {class} function createFrom(useCaseConfig: JUseCaseConfig): JCaptureConfig_Builder; cdecl;
    {class} function from(captureConfig: JCaptureConfig): JCaptureConfig_Builder; cdecl;
    {class} function init: JCaptureConfig_Builder; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/CaptureConfig$Builder')]
  JCaptureConfig_Builder = interface(JObject)
    ['{E548E0C3-A95E-40E2-8C26-319D0D667D7F}']
    procedure addAllCameraCaptureCallbacks(collection: JCollection); cdecl;
    procedure addAllTags(tagBundle: JTagBundle); cdecl;
    procedure addCameraCaptureCallback(cameraCaptureCallback: JCameraCaptureCallback); cdecl;
    procedure addImplementationOption(option: JConfig_Option; object_: JObject); cdecl;
    procedure addImplementationOptions(config: Jimpl_Config); cdecl;
    procedure addSurface(deferrableSurface: JDeferrableSurface); cdecl;
    procedure addTag(string_: JString; integer_: JInteger); cdecl;
    function build: JCaptureConfig; cdecl;
    procedure clearSurfaces; cdecl;
    function getImplementationOptions: Jimpl_Config; cdecl;
    function getSurfaces: JSet; cdecl;
    function getTag(string_: JString): JInteger; cdecl;
    function getTemplateType: Integer; cdecl;
    procedure removeSurface(deferrableSurface: JDeferrableSurface); cdecl;
    procedure setImplementationOptions(config: Jimpl_Config); cdecl;
    procedure setTemplateType(i: Integer); cdecl;
    procedure setUseRepeatingSurface(b: Boolean); cdecl;
  end;
  TJCaptureConfig_Builder = class(TJavaGenericImport<JCaptureConfig_BuilderClass, JCaptureConfig_Builder>) end;

  JCaptureConfig_OptionUnpackerClass = interface(IJavaClass)
    ['{BFC21987-2A8F-4356-A6B7-99585B736DD8}']
  end;

  [JavaSignature('androidx/camera/core/impl/CaptureConfig$OptionUnpacker')]
  JCaptureConfig_OptionUnpacker = interface(IJavaInstance)
    ['{B2FFC9DE-A943-4DB5-B620-34DEB6A634C8}']
    procedure unpack(useCaseConfig: JUseCaseConfig; builder: JCaptureConfig_Builder); cdecl;
  end;
  TJCaptureConfig_OptionUnpacker = class(TJavaGenericImport<JCaptureConfig_OptionUnpackerClass, JCaptureConfig_OptionUnpacker>) end;

  JSessionConfig_OptionUnpackerClass = interface(IJavaClass)
    ['{26CD315D-2728-48C4-AAAA-F15E44198394}']
  end;

  [JavaSignature('androidx/camera/core/impl/SessionConfig$OptionUnpacker')]
  JSessionConfig_OptionUnpacker = interface(IJavaInstance)
    ['{E52FA4FD-8299-4A93-953C-2F62C1A577B3}']
    procedure unpack(useCaseConfig: JUseCaseConfig; builder: JSessionConfig_Builder); cdecl;
  end;
  TJSessionConfig_OptionUnpacker = class(TJavaGenericImport<JSessionConfig_OptionUnpackerClass, JSessionConfig_OptionUnpacker>) end;

  JSessionConfig_SessionErrorClass = interface(JEnumClass)
    ['{BEFF1CAC-CC08-4A3C-A0FC-C631FEC846BA}']
    {class} function _GetSESSION_ERROR_SURFACE_NEEDS_RESET: JSessionConfig_SessionError; cdecl;
    {class} function _GetSESSION_ERROR_UNKNOWN: JSessionConfig_SessionError; cdecl;
    {class} function valueOf(string_: JString): JSessionConfig_SessionError; cdecl;
    {class} function values: TJavaObjectArray<JSessionConfig_SessionError>; cdecl;
    {class} property SESSION_ERROR_SURFACE_NEEDS_RESET: JSessionConfig_SessionError read _GetSESSION_ERROR_SURFACE_NEEDS_RESET;
    {class} property SESSION_ERROR_UNKNOWN: JSessionConfig_SessionError read _GetSESSION_ERROR_UNKNOWN;
  end;

  [JavaSignature('androidx/camera/core/impl/SessionConfig$SessionError')]
  JSessionConfig_SessionError = interface(JEnum)
    ['{190BEB5A-7CBC-4615-8182-210A960BCFF5}']
  end;
  TJSessionConfig_SessionError = class(TJavaGenericImport<JSessionConfig_SessionErrorClass, JSessionConfig_SessionError>) end;

  JSessionConfig_ErrorListenerClass = interface(IJavaClass)
    ['{BF37072C-710A-4FFB-B683-1BA10E284613}']
  end;

  [JavaSignature('androidx/camera/core/impl/SessionConfig$ErrorListener')]
  JSessionConfig_ErrorListener = interface(IJavaInstance)
    ['{94E763A9-02A9-4794-95B6-4366FFEB034F}']
    procedure onError(sessionConfig: JSessionConfig; sessionError: JSessionConfig_SessionError); cdecl;
  end;
  TJSessionConfig_ErrorListener = class(TJavaGenericImport<JSessionConfig_ErrorListenerClass, JSessionConfig_ErrorListener>) end;

  JSessionConfigClass = interface(JObjectClass)
    ['{D2750E86-0E0F-4B1B-BD0E-9E70AA04B648}']
    {class} function defaultEmptySessionConfig: JSessionConfig; cdecl;
    {class} function init(list: JList; list1: JList; list2: JList; list3: JList; list4: JList; captureConfig: JCaptureConfig): JSessionConfig; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/SessionConfig')]
  JSessionConfig = interface(JObject)
    ['{5B0A313C-50EB-4F9C-B7C0-4AB0435E450D}']
    function getDeviceStateCallbacks: JList; cdecl;
    function getErrorListeners: JList; cdecl;
    function getImplementationOptions: Jimpl_Config; cdecl;
    function getRepeatingCameraCaptureCallbacks: JList; cdecl;
    function getRepeatingCaptureConfig: JCaptureConfig; cdecl;
    function getSessionStateCallbacks: JList; cdecl;
    function getSingleCameraCaptureCallbacks: JList; cdecl;
    function getSurfaces: JList; cdecl;
    function getTemplateType: Integer; cdecl;
  end;
  TJSessionConfig = class(TJavaGenericImport<JSessionConfigClass, JSessionConfig>) end;

  JSessionConfig_BaseBuilderClass = interface(JObjectClass)
    ['{D451A3EF-D3DD-4C17-AE9A-BF71A23C2A44}']
    {class} function _GetmDeviceStateCallbacks: JList; cdecl;
    {class} function _GetmErrorListeners: JList; cdecl;
    {class} function _GetmSessionStateCallbacks: JList; cdecl;
    {class} function init: JSessionConfig_BaseBuilder; cdecl;
    {class} property mDeviceStateCallbacks: JList read _GetmDeviceStateCallbacks;
    {class} property mErrorListeners: JList read _GetmErrorListeners;
    {class} property mSessionStateCallbacks: JList read _GetmSessionStateCallbacks;
  end;

  [JavaSignature('androidx/camera/core/impl/SessionConfig$BaseBuilder')]
  JSessionConfig_BaseBuilder = interface(JObject)
    ['{45F3C2AF-6C31-432C-B25C-27402121C85B}']
  end;
  TJSessionConfig_BaseBuilder = class(TJavaGenericImport<JSessionConfig_BaseBuilderClass, JSessionConfig_BaseBuilder>) end;

  JSessionConfig_BuilderClass = interface(JSessionConfig_BaseBuilderClass)
    ['{526F755B-916A-4533-9E3D-B6210A69BF7D}']
    {class} function createFrom(useCaseConfig: JUseCaseConfig): JSessionConfig_Builder; cdecl;
    {class} function init: JSessionConfig_Builder; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/SessionConfig$Builder')]
  JSessionConfig_Builder = interface(JSessionConfig_BaseBuilder)
    ['{BCE039EF-1CA7-4F9E-B9D9-F8D58FE4144C}']
    procedure addAllCameraCaptureCallbacks(collection: JCollection); cdecl;
    procedure addAllDeviceStateCallbacks(collection: JCollection); cdecl;
    procedure addAllRepeatingCameraCaptureCallbacks(collection: JCollection); cdecl;
    procedure addAllSessionStateCallbacks(list: JList); cdecl;
    procedure addCameraCaptureCallback(cameraCaptureCallback: JCameraCaptureCallback); cdecl;
    procedure addDeviceStateCallback(stateCallback: JCameraDevice_StateCallback); cdecl;
    procedure addErrorListener(errorListener: JSessionConfig_ErrorListener); cdecl;
    procedure addImplementationOptions(config: Jimpl_Config); cdecl;
    procedure addNonRepeatingSurface(deferrableSurface: JDeferrableSurface); cdecl;
    procedure addRepeatingCameraCaptureCallback(cameraCaptureCallback: JCameraCaptureCallback); cdecl;
    procedure addSessionStateCallback(stateCallback: JCameraCaptureSession_StateCallback); cdecl;
    procedure addSurface(deferrableSurface: JDeferrableSurface); cdecl;
    procedure addTag(string_: JString; integer_: JInteger); cdecl;
    function build: JSessionConfig; cdecl;
    procedure clearSurfaces; cdecl;
    function getSingleCameraCaptureCallbacks: JList; cdecl;
    procedure removeSurface(deferrableSurface: JDeferrableSurface); cdecl;
    procedure setImplementationOptions(config: Jimpl_Config); cdecl;
    procedure setTemplateType(i: Integer); cdecl;
  end;
  TJSessionConfig_Builder = class(TJavaGenericImport<JSessionConfig_BuilderClass, JSessionConfig_Builder>) end;

  JSessionConfig_ValidatingBuilderClass = interface(JSessionConfig_BaseBuilderClass)
    ['{97856634-18EA-4C80-87C7-E9B4B196C8B8}']
    {class} function init: JSessionConfig_ValidatingBuilder; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/SessionConfig$ValidatingBuilder')]
  JSessionConfig_ValidatingBuilder = interface(JSessionConfig_BaseBuilder)
    ['{0055AED1-49EF-48C2-A29B-0CA8602E4AD0}']
    procedure add(sessionConfig: JSessionConfig); cdecl;
    function build: JSessionConfig; cdecl;
    function isValid: Boolean; cdecl;
  end;
  TJSessionConfig_ValidatingBuilder = class(TJavaGenericImport<JSessionConfig_ValidatingBuilderClass, JSessionConfig_ValidatingBuilder>) end;

  JCameraUseCaseAdapter_CameraIdClass = interface(JObjectClass)
    ['{84FFFBD4-2815-4E1F-A653-A25FBA439023}']
  end;

  [JavaSignature('androidx/camera/core/internal/CameraUseCaseAdapter$CameraId')]
  JCameraUseCaseAdapter_CameraId = interface(JObject)
    ['{D1EF75AC-8F51-428D-8BD9-3C03C129A7F3}']
    function equals(object_: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
  end;
  TJCameraUseCaseAdapter_CameraId = class(TJavaGenericImport<JCameraUseCaseAdapter_CameraIdClass, JCameraUseCaseAdapter_CameraId>) end;

  JUseCaseConfig_BuilderClass = interface(JTargetConfig_BuilderClass)
    ['{6479E798-5A3A-4203-91B4-C5268F9FFC9B}']
  end;

  [JavaSignature('androidx/camera/core/impl/UseCaseConfig$Builder')]
  JUseCaseConfig_Builder = interface(JTargetConfig_Builder)
    ['{6C07564A-A6FF-4D7A-9024-54C13D41E957}']
    function getUseCaseConfig: JUseCaseConfig; cdecl;
    function setCameraSelector(cameraSelector: JCameraSelector): JObject; cdecl;
    function setCaptureOptionUnpacker(optionUnpacker: JCaptureConfig_OptionUnpacker): JObject; cdecl;
    function setDefaultCaptureConfig(captureConfig: JCaptureConfig): JObject; cdecl;
    function setDefaultSessionConfig(sessionConfig: JSessionConfig): JObject; cdecl;
    function setSessionOptionUnpacker(optionUnpacker: JSessionConfig_OptionUnpacker): JObject; cdecl;
    function setSurfaceOccupancyPriority(i: Integer): JObject; cdecl;
  end;
  TJUseCaseConfig_Builder = class(TJavaGenericImport<JUseCaseConfig_BuilderClass, JUseCaseConfig_Builder>) end;

  JUseCaseConfigClass = interface(JTargetConfigClass)
    ['{3997EDEC-B7D3-45B8-AA70-FF810152DD12}']
    {class} function _GetOPTION_CAMERA_SELECTOR: JConfig_Option; cdecl;
    {class} function _GetOPTION_CAPTURE_CONFIG_UNPACKER: JConfig_Option; cdecl;
    {class} function _GetOPTION_DEFAULT_CAPTURE_CONFIG: JConfig_Option; cdecl;
    {class} function _GetOPTION_DEFAULT_SESSION_CONFIG: JConfig_Option; cdecl;
    {class} function _GetOPTION_SESSION_CONFIG_UNPACKER: JConfig_Option; cdecl;
    {class} function _GetOPTION_SURFACE_OCCUPANCY_PRIORITY: JConfig_Option; cdecl;
    {class} function getDefaultSessionConfig(sessionConfig: JSessionConfig): JSessionConfig; cdecl; overload;
    {class} property OPTION_CAMERA_SELECTOR: JConfig_Option read _GetOPTION_CAMERA_SELECTOR;
    {class} property OPTION_CAPTURE_CONFIG_UNPACKER: JConfig_Option read _GetOPTION_CAPTURE_CONFIG_UNPACKER;
    {class} property OPTION_DEFAULT_CAPTURE_CONFIG: JConfig_Option read _GetOPTION_DEFAULT_CAPTURE_CONFIG;
    {class} property OPTION_DEFAULT_SESSION_CONFIG: JConfig_Option read _GetOPTION_DEFAULT_SESSION_CONFIG;
    {class} property OPTION_SESSION_CONFIG_UNPACKER: JConfig_Option read _GetOPTION_SESSION_CONFIG_UNPACKER;
    {class} property OPTION_SURFACE_OCCUPANCY_PRIORITY: JConfig_Option read _GetOPTION_SURFACE_OCCUPANCY_PRIORITY;
  end;

  [JavaSignature('androidx/camera/core/impl/UseCaseConfig')]
  JUseCaseConfig = interface(JTargetConfig)
    ['{6C348979-85ED-40DB-9DF9-CF2EE31D07D2}']
    function getCameraSelector: JCameraSelector; cdecl; overload;
    function getCameraSelector(cameraSelector: JCameraSelector): JCameraSelector; cdecl; overload;
    function getCaptureOptionUnpacker: JCaptureConfig_OptionUnpacker; cdecl; overload;
    function getCaptureOptionUnpacker(optionUnpacker: JCaptureConfig_OptionUnpacker): JCaptureConfig_OptionUnpacker; cdecl; overload;
    function getDefaultCaptureConfig: JCaptureConfig; cdecl; overload;
    function getDefaultCaptureConfig(captureConfig: JCaptureConfig): JCaptureConfig; cdecl; overload;
    function getDefaultSessionConfig: JSessionConfig; cdecl; overload;
    function getSessionOptionUnpacker: JSessionConfig_OptionUnpacker; cdecl; overload;
    function getSessionOptionUnpacker(optionUnpacker: JSessionConfig_OptionUnpacker): JSessionConfig_OptionUnpacker; cdecl; overload;
    function getSurfaceOccupancyPriority: Integer; cdecl; overload;
    function getSurfaceOccupancyPriority(i: Integer): Integer; cdecl; overload;
  end;
  TJUseCaseConfig = class(TJavaGenericImport<JUseCaseConfigClass, JUseCaseConfig>) end;

  JUseCaseGroupClass = interface(JObjectClass)
    ['{8D3BE908-1A95-48A9-98B8-972795FDBFBF}']
    {class} function init(viewPort: JViewPort; list: JList): JUseCaseGroup; cdecl;
  end;

  [JavaSignature('androidx/camera/core/UseCaseGroup')]
  JUseCaseGroup = interface(JObject)
    ['{0CEEE939-2831-410E-B222-0EF51A34C0D0}']
    function getUseCases: JList; cdecl;
    function getViewPort: JViewPort; cdecl;
  end;
  TJUseCaseGroup = class(TJavaGenericImport<JUseCaseGroupClass, JUseCaseGroup>) end;

  JUseCaseGroup_BuilderClass = interface(JObjectClass)
    ['{187F00A6-E6D2-4132-803B-E99EFA0D29A2}']
    {class} function init: JUseCaseGroup_Builder; cdecl;
  end;

  [JavaSignature('androidx/camera/core/UseCaseGroup$Builder')]
  JUseCaseGroup_Builder = interface(JObject)
    ['{4BEE97A3-1EE2-4342-9AB0-85668262F10D}']
    function addUseCase(useCase: JUseCase): JUseCaseGroup_Builder; cdecl;
    function build: JUseCaseGroup; cdecl;
    function setViewPort(viewPort: JViewPort): JUseCaseGroup_Builder; cdecl;
  end;
  TJUseCaseGroup_Builder = class(TJavaGenericImport<JUseCaseGroup_BuilderClass, JUseCaseGroup_Builder>) end;

  JUseCaseClass = interface(JObjectClass)
    ['{2F96A63F-22B6-45DE-9195-8F21D56A8B6F}']
    {class} function init(useCaseConfig: JUseCaseConfig): JUseCase; cdecl;
  end;

  [JavaSignature('androidx/camera/core/UseCase')]
  JUseCase = interface(JObject)
    ['{2E465B3F-E490-4737-9B6A-C191AEAB42E1}']
    function getAttachedSurfaceResolution: Jutil_Size; cdecl;
    function getCamera: JCameraInternal; cdecl;
    function getCurrentConfig: JUseCaseConfig; cdecl;
    function getDefaultConfig(b: Boolean; useCaseConfigFactory: JUseCaseConfigFactory): JUseCaseConfig; cdecl;
    function getImageFormat: Integer; cdecl;
    function getName: JString; cdecl;
    function getSessionConfig: JSessionConfig; cdecl;
    function getUseCaseConfigBuilder(config: Jimpl_Config): JUseCaseConfig_Builder; cdecl;
    function getViewPortCropRect: JRect; cdecl;
    function mergeConfigs(cameraInfoInternal: JCameraInfoInternal; useCaseConfig: JUseCaseConfig; useCaseConfig1: JUseCaseConfig): JUseCaseConfig; cdecl;
    procedure notifyState; cdecl;
    procedure onAttach(cameraInternal: JCameraInternal; useCaseConfig: JUseCaseConfig; useCaseConfig1: JUseCaseConfig); cdecl;
    procedure onAttached; cdecl;
    procedure onDetach(cameraInternal: JCameraInternal); cdecl;
    procedure onDetached; cdecl;
    procedure onStateAttached; cdecl;
    procedure onStateDetached; cdecl;
    procedure setViewPortCropRect(rect: JRect); cdecl;
    procedure updateSuggestedResolution(size: Jutil_Size); cdecl;
  end;
  TJUseCase = class(TJavaGenericImport<JUseCaseClass, JUseCase>) end;

  JConfigProviderClass = interface(IJavaClass)
    ['{857160E3-621A-44B1-B7DC-98B1B2E440DA}']
  end;

  [JavaSignature('androidx/camera/core/impl/ConfigProvider')]
  JConfigProvider = interface(IJavaInstance)
    ['{CD2995CA-076E-44DF-B9DB-CD71FD11A114}']
    function getConfig: Jimpl_Config; cdecl;
  end;
  TJConfigProvider = class(TJavaGenericImport<JConfigProviderClass, JConfigProvider>) end;

  JOptionsBundleClass = interface(Jimpl_ConfigClass)
    ['{290FFE07-D100-4675-8569-E0937E144C64}']
    {class} function _GetID_COMPARE: JComparator; cdecl;
    {class} function emptyBundle: JOptionsBundle; cdecl;
    {class} function from(config: Jimpl_Config): JOptionsBundle; cdecl;
    {class} function init(treeMap: JTreeMap): JOptionsBundle; cdecl;
    {class} property ID_COMPARE: JComparator read _GetID_COMPARE;
  end;

  [JavaSignature('androidx/camera/core/impl/OptionsBundle')]
  JOptionsBundle = interface(Jimpl_Config)
    ['{2FAEF8B4-7DA3-4709-BBDD-6C0338DF3C33}']
    function containsOption(option: JConfig_Option): Boolean; cdecl;
    procedure findOptions(string_: JString; optionMatcher: JConfig_OptionMatcher); cdecl;
    function getOptionPriority(option: JConfig_Option): JConfig_OptionPriority; cdecl;
    function getPriorities(option: JConfig_Option): JSet; cdecl;
    function listOptions: JSet; cdecl;
    //*** function retrieveOption(option: JConfig_Option): J; cdecl; overload;
    //*** function retrieveOption(option: JConfig_Option; valueT: J): J; cdecl; overload;
    //*** function retrieveOptionWithPriority(option: JConfig_Option; optionPriority: JConfig_OptionPriority): J; cdecl;
  end;
  TJOptionsBundle = class(TJavaGenericImport<JOptionsBundleClass, JOptionsBundle>) end;

  JImageInfoClass = interface(IJavaClass)
    ['{9C3C9E8A-28C2-49E6-B06B-A2ABAB5D7371}']
  end;

  [JavaSignature('androidx/camera/core/ImageInfo')]
  JImageInfo = interface(IJavaInstance)
    ['{593FFF66-C4AB-4F91-AEEE-8041AD278436}']
    function getRotationDegrees: Integer; cdecl;
    function getTagBundle: JTagBundle; cdecl;
    function getTimestamp: Int64; cdecl;
    procedure populateExifData(builder: JExifData_Builder); cdecl;
  end;
  TJImageInfo = class(TJavaGenericImport<JImageInfoClass, JImageInfo>) end;

  JImageProxy_PlaneProxyClass = interface(IJavaClass)
    ['{CC844332-A09F-4F11-92A7-3AC274F07ECA}']
  end;

  [JavaSignature('androidx/camera/core/ImageProxy$PlaneProxy')]
  JImageProxy_PlaneProxy = interface(IJavaInstance)
    ['{ED3CA64F-D3E5-433B-A683-3F45F92ACCA8}']
    function getBuffer: JByteBuffer; cdecl;
    function getPixelStride: Integer; cdecl;
    function getRowStride: Integer; cdecl;
  end;
  TJImageProxy_PlaneProxy = class(TJavaGenericImport<JImageProxy_PlaneProxyClass, JImageProxy_PlaneProxy>) end;

  JImageProxyClass = interface(JAutoCloseableClass)
    ['{9D2C3E97-B385-49E1-9AC3-6299A967F267}']
  end;

  [JavaSignature('androidx/camera/core/ImageProxy')]
  JImageProxy = interface(JAutoCloseable)
    ['{4031DC99-096D-4808-A5EE-A615F17F1E17}']
    procedure close; cdecl;
    function getCropRect: JRect; cdecl;
    function getFormat: Integer; cdecl;
    function getHeight: Integer; cdecl;
    function getImage: JImage; cdecl;
    function getImageInfo: JImageInfo; cdecl;
    function getPlanes: TJavaObjectArray<JImageProxy_PlaneProxy>; cdecl;
    function getWidth: Integer; cdecl;
    procedure setCropRect(rect: JRect); cdecl;
  end;
  TJImageProxy = class(TJavaGenericImport<JImageProxyClass, JImageProxy>) end;

  JImageReaderProxy_OnImageAvailableListenerClass = interface(IJavaClass)
    ['{6FBAC53D-4519-436A-8C92-A6C88B69696B}']
  end;

  [JavaSignature('androidx/camera/core/impl/ImageReaderProxy$OnImageAvailableListener')]
  JImageReaderProxy_OnImageAvailableListener = interface(IJavaInstance)
    ['{70DBEF32-D131-4038-AE59-5A8CBAC71208}']
    procedure onImageAvailable(imageReaderProxy: JImageReaderProxy); cdecl;
  end;
  TJImageReaderProxy_OnImageAvailableListener = class(TJavaGenericImport<JImageReaderProxy_OnImageAvailableListenerClass,
    JImageReaderProxy_OnImageAvailableListener>) end;

  JImageReaderProxyClass = interface(IJavaClass)
    ['{E8DB0872-D3C0-4496-972B-D79413D25B4D}']
  end;

  [JavaSignature('androidx/camera/core/impl/ImageReaderProxy')]
  JImageReaderProxy = interface(IJavaInstance)
    ['{FC68E830-E846-4B80-A22A-9E26A1D10D4A}']
    function acquireLatestImage: JImageProxy; cdecl;
    function acquireNextImage: JImageProxy; cdecl;
    procedure clearOnImageAvailableListener; cdecl;
    procedure close; cdecl;
    function getHeight: Integer; cdecl;
    function getImageFormat: Integer; cdecl;
    function getMaxImages: Integer; cdecl;
    function getSurface: JSurface; cdecl;
    function getWidth: Integer; cdecl;
    procedure setOnImageAvailableListener(onImageAvailableListener: JImageReaderProxy_OnImageAvailableListener; executor: JExecutor); cdecl;
  end;
  TJImageReaderProxy = class(TJavaGenericImport<JImageReaderProxyClass, JImageReaderProxy>) end;

  JImageReaderProxyProviderClass = interface(IJavaClass)
    ['{459C649C-F865-49CC-B0B7-A54FC8F5693B}']
  end;

  [JavaSignature('androidx/camera/core/ImageReaderProxyProvider')]
  JImageReaderProxyProvider = interface(IJavaInstance)
    ['{76697501-E958-4A9B-AF60-E41B8CC72118}']
    function newInstance(i: Integer; i1: Integer; i2: Integer; i3: Integer; l: Int64): JImageReaderProxy; cdecl;
  end;
  TJImageReaderProxyProvider = class(TJavaGenericImport<JImageReaderProxyProviderClass, JImageReaderProxyProvider>) end;

  JImageAnalysisConfigClass = interface(JUseCaseConfigClass)
    ['{F4E5DE97-C0AC-4CDF-9E5C-9910602CEF54}']
    {class} function _GetOPTION_BACKPRESSURE_STRATEGY: JConfig_Option; cdecl;
    {class} function _GetOPTION_IMAGE_QUEUE_DEPTH: JConfig_Option; cdecl;
    {class} function _GetOPTION_IMAGE_READER_PROXY_PROVIDER: JConfig_Option; cdecl;
    {class} function init(optionsBundle: JOptionsBundle): JImageAnalysisConfig; cdecl;
    {class} property OPTION_BACKPRESSURE_STRATEGY: JConfig_Option read _GetOPTION_BACKPRESSURE_STRATEGY;
    {class} property OPTION_IMAGE_QUEUE_DEPTH: JConfig_Option read _GetOPTION_IMAGE_QUEUE_DEPTH;
    {class} property OPTION_IMAGE_READER_PROXY_PROVIDER: JConfig_Option read _GetOPTION_IMAGE_READER_PROXY_PROVIDER;
  end;

  [JavaSignature('androidx/camera/core/impl/ImageAnalysisConfig')]
  JImageAnalysisConfig = interface(JUseCaseConfig)
    ['{4474385F-B41A-448A-BB01-6C8727DC28DE}']
    function getBackpressureStrategy: Integer; cdecl; overload;
    function getBackpressureStrategy(i: Integer): Integer; cdecl; overload;
    function getConfig: Jimpl_Config; cdecl;
    function getImageQueueDepth: Integer; cdecl; overload;
    function getImageQueueDepth(i: Integer): Integer; cdecl; overload;
    function getImageReaderProxyProvider: JImageReaderProxyProvider; cdecl;
    function getInputFormat: Integer; cdecl;
  end;
  TJImageAnalysisConfig = class(TJavaGenericImport<JImageAnalysisConfigClass, JImageAnalysisConfig>) end;

  JImageAnalysis_DefaultsClass = interface(JConfigProviderClass)
    ['{09952CD7-3433-467A-93A3-6636DB52CBBF}']
    {class} function init: JImageAnalysis_Defaults; cdecl;
  end;

  [JavaSignature('androidx/camera/core/ImageAnalysis$Defaults')]
  JImageAnalysis_Defaults = interface(JConfigProvider)
    ['{78EB6EDF-97C6-456D-A351-37AE5FC97C35}']
    function getConfig: JImageAnalysisConfig; cdecl;
  end;
  TJImageAnalysis_Defaults = class(TJavaGenericImport<JImageAnalysis_DefaultsClass, JImageAnalysis_Defaults>) end;

  JImageAnalysisClass = interface(JUseCaseClass)
    ['{95B5792F-751E-407B-BEAA-9928E975165A}']
    {class} function _GetDEFAULT_CONFIG: JImageAnalysis_Defaults; cdecl;
    {class} function _GetSTRATEGY_BLOCK_PRODUCER: Integer; cdecl;
    {class} function _GetSTRATEGY_KEEP_ONLY_LATEST: Integer; cdecl;
    {class} property DEFAULT_CONFIG: JImageAnalysis_Defaults read _GetDEFAULT_CONFIG;
    {class} property STRATEGY_BLOCK_PRODUCER: Integer read _GetSTRATEGY_BLOCK_PRODUCER;
    {class} property STRATEGY_KEEP_ONLY_LATEST: Integer read _GetSTRATEGY_KEEP_ONLY_LATEST;
  end;

  [JavaSignature('androidx/camera/core/ImageAnalysis')]
  JImageAnalysis = interface(JUseCase)
    ['{65804D09-21A8-4844-BC64-DAF45552947C}']
    procedure clearAnalyzer; cdecl;
    function getBackpressureStrategy: Integer; cdecl;
    function getDefaultConfig(b: Boolean; useCaseConfigFactory: JUseCaseConfigFactory): JUseCaseConfig; cdecl;
    function getImageQueueDepth: Integer; cdecl;
    function getTargetRotation: Integer; cdecl;
    function getUseCaseConfigBuilder(config: Jimpl_Config): JUseCaseConfig_Builder; cdecl;
    procedure onAttached; cdecl;
    procedure onDetached; cdecl;
    procedure setAnalyzer(executor: JExecutor; analyzer: JImageAnalysis_Analyzer); cdecl;
    procedure setTargetRotation(i: Integer); cdecl;
    function toString: JString; cdecl;
  end;
  TJImageAnalysis = class(TJavaGenericImport<JImageAnalysisClass, JImageAnalysis>) end;

  JImageAnalysis_AnalyzerClass = interface(IJavaClass)
    ['{129420BA-521A-4EDE-9743-44F2585C290A}']
  end;

  [JavaSignature('androidx/camera/core/ImageAnalysis$Analyzer')]
  JImageAnalysis_Analyzer = interface(IJavaInstance)
    ['{1DFB4851-3801-48EB-BE68-E1D5BBAC72DC}']
    procedure analyze(imageProxy: JImageProxy); cdecl;
  end;
  TJImageAnalysis_Analyzer = class(TJavaGenericImport<JImageAnalysis_AnalyzerClass, JImageAnalysis_Analyzer>) end;

  JPreview_DefaultsClass = interface(JConfigProviderClass)
    ['{A4A4D5D0-32A3-4A2B-8F0C-E71E38CF2827}']
    {class} function init: JPreview_Defaults; cdecl;
  end;

  [JavaSignature('androidx/camera/core/Preview$Defaults')]
  JPreview_Defaults = interface(JConfigProvider)
    ['{BD1D9756-6375-46AB-9483-8A49DFB75101}']
    function getConfig: JPreviewConfig; cdecl;
  end;
  TJPreview_Defaults = class(TJavaGenericImport<JPreview_DefaultsClass, JPreview_Defaults>) end;

  JPreview_SurfaceProviderClass = interface(IJavaClass)
    ['{6AB79422-047E-405B-9885-A8997AA1E241}']
  end;

  [JavaSignature('androidx/camera/core/Preview$SurfaceProvider')]
  JPreview_SurfaceProvider = interface(IJavaInstance)
    ['{3ABE5C7A-0E7E-4A3F-AD06-FA0B1BA8D038}']
    procedure onSurfaceRequested(surfaceRequest: JSurfaceRequest); cdecl;
  end;
  TJPreview_SurfaceProvider = class(TJavaGenericImport<JPreview_SurfaceProviderClass, JPreview_SurfaceProvider>) end;

  JImageProxyBundleClass = interface(IJavaClass)
    ['{5D27BBEA-3941-4F20-9C93-4F87151DF22B}']
  end;

  [JavaSignature('androidx/camera/core/impl/ImageProxyBundle')]
  JImageProxyBundle = interface(IJavaInstance)
    ['{45E47642-457D-4EAB-B3E9-01EC136F2B10}']
    function getCaptureIds: JList; cdecl;
    function getImageProxy(i: Integer): JListenableFuture; cdecl;
  end;
  TJImageProxyBundle = class(TJavaGenericImport<JImageProxyBundleClass, JImageProxyBundle>) end;

  JCaptureProcessorClass = interface(IJavaClass)
    ['{B2ECD678-325B-41C0-8514-302BF4439275}']
  end;

  [JavaSignature('androidx/camera/core/impl/CaptureProcessor')]
  JCaptureProcessor = interface(IJavaInstance)
    ['{D910DA4D-949B-46F4-9E45-6588135D36BC}']
    procedure onOutputSurface(surface: JSurface; i: Integer); cdecl;
    procedure onResolutionUpdate(size: Jutil_Size); cdecl;
    procedure process(imageProxyBundle: JImageProxyBundle); cdecl;
  end;
  TJCaptureProcessor = class(TJavaGenericImport<JCaptureProcessorClass, JCaptureProcessor>) end;

  JCaptureStageClass = interface(IJavaClass)
    ['{D891164B-E9DD-4857-8BE7-5B0A9FDADF11}']
  end;

  [JavaSignature('androidx/camera/core/impl/CaptureStage')]
  JCaptureStage = interface(IJavaInstance)
    ['{87A55C81-A97C-4B43-8AE0-DF47BED5CF89}']
    function getCaptureConfig: JCaptureConfig; cdecl;
    function getId: Integer; cdecl;
  end;
  TJCaptureStage = class(TJavaGenericImport<JCaptureStageClass, JCaptureStage>) end;

  JCaptureStage_DefaultCaptureStageClass = interface(JCaptureStageClass)
    ['{F614063B-DEE8-49B0-86C0-5772A43EBD36}']
    {class} function init: JCaptureStage_DefaultCaptureStage; cdecl;
  end;

  [JavaSignature('androidx/camera/core/impl/CaptureStage$DefaultCaptureStage')]
  JCaptureStage_DefaultCaptureStage = interface(JCaptureStage)
    ['{7E44A2A1-0F29-4714-9E5E-67E445B9366B}']
    function getCaptureConfig: JCaptureConfig; cdecl;
    function getId: Integer; cdecl;
  end;
  TJCaptureStage_DefaultCaptureStage = class(TJavaGenericImport<JCaptureStage_DefaultCaptureStageClass, JCaptureStage_DefaultCaptureStage>) end;

  JImageInfoProcessorClass = interface(IJavaClass)
    ['{65A3825A-FCA0-4764-B973-E1749246F541}']
  end;

  [JavaSignature('androidx/camera/core/impl/ImageInfoProcessor')]
  JImageInfoProcessor = interface(IJavaInstance)
    ['{F53D32FC-4FAC-4BEE-8EEE-A86EB5259966}']
    function getCaptureStage: JCaptureStage; cdecl;
    function process(imageInfo: JImageInfo): Boolean; cdecl;
  end;
  TJImageInfoProcessor = class(TJavaGenericImport<JImageInfoProcessorClass, JImageInfoProcessor>) end;

  JPreviewConfigClass = interface(JUseCaseConfigClass)
    ['{C0D52F73-67F6-42F9-B47A-A77DE8DE1520}']
    {class} function _GetIMAGE_INFO_PROCESSOR: JConfig_Option; cdecl;
    {class} function _GetOPTION_PREVIEW_CAPTURE_PROCESSOR: JConfig_Option; cdecl;
    {class} function init(optionsBundle: JOptionsBundle): JPreviewConfig; cdecl;
    {class} property IMAGE_INFO_PROCESSOR: JConfig_Option read _GetIMAGE_INFO_PROCESSOR;
    {class} property OPTION_PREVIEW_CAPTURE_PROCESSOR: JConfig_Option read _GetOPTION_PREVIEW_CAPTURE_PROCESSOR;
  end;

  [JavaSignature('androidx/camera/core/impl/PreviewConfig')]
  JPreviewConfig = interface(JUseCaseConfig)
    ['{14B01865-A2ED-4F3A-8344-F7EB4697AFB1}']
    function getCaptureProcessor: JCaptureProcessor; cdecl; overload;
    function getCaptureProcessor(captureProcessor: JCaptureProcessor): JCaptureProcessor; cdecl; overload;
    function getConfig: Jimpl_Config; cdecl;
    function getImageInfoProcessor(imageInfoProcessor: JImageInfoProcessor): JImageInfoProcessor; cdecl; overload;
    function getInputFormat: Integer; cdecl;
  end;
  TJPreviewConfig = class(TJavaGenericImport<JPreviewConfigClass, JPreviewConfig>) end;

  JMutableConfigClass = interface(Jimpl_ConfigClass)
    ['{F652B48A-DBC0-46D0-83A1-0114D1C417D7}']
  end;

  [JavaSignature('androidx/camera/core/impl/MutableConfig')]
  JMutableConfig = interface(Jimpl_Config)
    ['{47F70878-734F-449F-936D-3BD3DFA73A9F}']
    //*** procedure insertOption(option: JConfig_Option; valueT: J); cdecl; overload;
    //*** procedure insertOption(option: JConfig_Option; optionPriority: JConfig_OptionPriority; valueT: J); cdecl; overload;
    //*** function removeOption(option: JConfig_Option): J; cdecl;
  end;
  TJMutableConfig = class(TJavaGenericImport<JMutableConfigClass, JMutableConfig>) end;

  JUseCase_EventCallbackClass = interface(IJavaClass)
    ['{8100448F-0DA5-4CBA-B941-01FABDD80057}']
  end;

  [JavaSignature('androidx/camera/core/UseCase$EventCallback')]
  JUseCase_EventCallback = interface(IJavaInstance)
    ['{0630F513-D3A1-444B-A08B-17E2A7F8CE19}']
    procedure onAttach(cameraInfo: JCameraInfo); cdecl;
    procedure onDetach; cdecl;
  end;
  TJUseCase_EventCallback = class(TJavaGenericImport<JUseCase_EventCallbackClass, JUseCase_EventCallback>) end;

  JPreview_BuilderClass = interface(JUseCaseConfig_BuilderClass)
    ['{F29B5E39-7DEC-4529-84AA-9DB42BA20F14}']
    {class} function fromConfig(previewConfig: JPreviewConfig): JPreview_Builder; cdecl; overload;
    {class} function init: JPreview_Builder; cdecl;//Deprecated
  end;

  [JavaSignature('androidx/camera/core/Preview$Builder')]
  JPreview_Builder = interface(JUseCaseConfig_Builder)
    ['{17C26CEF-D983-414C-ABA4-8031B8421E16}']
    function build: JPreview; cdecl;
    function getMutableConfig: JMutableConfig; cdecl;
    function getUseCaseConfig: JPreviewConfig; cdecl;
    function setBackgroundExecutor(executor: JExecutor): JPreview_Builder; cdecl;
    function setCameraSelector(cameraSelector: JCameraSelector): JPreview_Builder; cdecl;
    function setCaptureOptionUnpacker(optionUnpacker: JCaptureConfig_OptionUnpacker): JPreview_Builder; cdecl;
    function setCaptureProcessor(captureProcessor: JCaptureProcessor): JPreview_Builder; cdecl;
    function setDefaultCaptureConfig(captureConfig: JCaptureConfig): JPreview_Builder; cdecl;
    function setDefaultResolution(size: Jutil_Size): JPreview_Builder; cdecl;
    function setDefaultSessionConfig(sessionConfig: JSessionConfig): JPreview_Builder; cdecl;
    function setImageInfoProcessor(imageInfoProcessor: JImageInfoProcessor): JPreview_Builder; cdecl;
    function setMaxResolution(size: Jutil_Size): JPreview_Builder; cdecl;
    function setSessionOptionUnpacker(optionUnpacker: JSessionConfig_OptionUnpacker): JPreview_Builder; cdecl;
    function setSupportedResolutions(list: JList): JPreview_Builder; cdecl;
    function setSurfaceOccupancyPriority(i: Integer): JPreview_Builder; cdecl;
    function setTargetAspectRatio(i: Integer): JPreview_Builder; cdecl;
    function setTargetClass(class_: Jlang_Class): JPreview_Builder; cdecl;
    function setTargetName(string_: JString): JPreview_Builder; cdecl;
    function setTargetResolution(size: Jutil_Size): JPreview_Builder; cdecl;
    function setTargetRotation(i: Integer): JPreview_Builder; cdecl;
    function setUseCaseEventCallback(eventCallback: JUseCase_EventCallback): JPreview_Builder; cdecl;
  end;
  TJPreview_Builder = class(TJavaGenericImport<JPreview_BuilderClass, JPreview_Builder>) end;

  JPreviewClass = interface(JUseCaseClass)
    ['{FB87F1F5-273B-41A7-B7F9-8BC936325393}']
    {class} function _GetDEFAULT_CONFIG: JPreview_Defaults; cdecl;
    {class} function init(previewConfig: JPreviewConfig): JPreview; cdecl;
    {class} property DEFAULT_CONFIG: JPreview_Defaults read _GetDEFAULT_CONFIG;
  end;

  [JavaSignature('androidx/camera/core/Preview')]
  JPreview = interface(JUseCase)
    ['{D5E10B37-F54F-47B2-AA7F-FCF291EEB9DC}']
    function getDefaultConfig(b: Boolean; useCaseConfigFactory: JUseCaseConfigFactory): JUseCaseConfig; cdecl;
    function getTargetRotation: Integer; cdecl;
    function getUseCaseConfigBuilder(config: Jimpl_Config): JUseCaseConfig_Builder; cdecl;
    procedure onDetached; cdecl;
    procedure setSurfaceProvider(surfaceProvider: JPreview_SurfaceProvider); cdecl; overload;
    procedure setSurfaceProvider(executor: JExecutor; surfaceProvider: JPreview_SurfaceProvider); cdecl; overload;
    procedure setTargetRotation(i: Integer); cdecl;
    procedure setViewPortCropRect(rect: JRect); cdecl;
    function toString: JString; cdecl;
  end;
  TJPreview = class(TJavaGenericImport<JPreviewClass, JPreview>) end;

  JSensorRotationListenerClass = interface(JOrientationEventListenerClass)
    ['{648495BB-1286-4176-A218-C9F1CFCE4FDA}']
    {class} function _GetINVALID_SURFACE_ROTATION: Integer; cdecl;
    {class} function init(context: JContext): JSensorRotationListener; cdecl;
    {class} property INVALID_SURFACE_ROTATION: Integer read _GetINVALID_SURFACE_ROTATION;
  end;

  [JavaSignature('androidx/camera/view/SensorRotationListener')]
  JSensorRotationListener = interface(JOrientationEventListener)
    ['{31D93916-2106-4F86-B8D4-F36DB6BBB38C}']
    procedure onOrientationChanged(i: Integer); cdecl;
    procedure onRotationChanged(i: Integer); cdecl;
  end;
  TJSensorRotationListener = class(TJavaGenericImport<JSensorRotationListenerClass, JSensorRotationListener>) end;

  JVideoCaptureConfigClass = interface(JUseCaseConfigClass)
    ['{CC78BAB5-E6ED-4E54-8D1A-0F63CA918FAF}']
    {class} function _GetOPTION_AUDIO_BIT_RATE: JConfig_Option; cdecl;
    {class} function _GetOPTION_AUDIO_CHANNEL_COUNT: JConfig_Option; cdecl;
    {class} function _GetOPTION_AUDIO_MIN_BUFFER_SIZE: JConfig_Option; cdecl;
    {class} function _GetOPTION_AUDIO_RECORD_SOURCE: JConfig_Option; cdecl;
    {class} function _GetOPTION_AUDIO_SAMPLE_RATE: JConfig_Option; cdecl;
    {class} function _GetOPTION_BIT_RATE: JConfig_Option; cdecl;
    {class} function _GetOPTION_INTRA_FRAME_INTERVAL: JConfig_Option; cdecl;
    {class} function _GetOPTION_VIDEO_FRAME_RATE: JConfig_Option; cdecl;
    {class} function init(optionsBundle: JOptionsBundle): JVideoCaptureConfig; cdecl;
    {class} property OPTION_AUDIO_BIT_RATE: JConfig_Option read _GetOPTION_AUDIO_BIT_RATE;
    {class} property OPTION_AUDIO_CHANNEL_COUNT: JConfig_Option read _GetOPTION_AUDIO_CHANNEL_COUNT;
    {class} property OPTION_AUDIO_MIN_BUFFER_SIZE: JConfig_Option read _GetOPTION_AUDIO_MIN_BUFFER_SIZE;
    {class} property OPTION_AUDIO_RECORD_SOURCE: JConfig_Option read _GetOPTION_AUDIO_RECORD_SOURCE;
    {class} property OPTION_AUDIO_SAMPLE_RATE: JConfig_Option read _GetOPTION_AUDIO_SAMPLE_RATE;
    {class} property OPTION_BIT_RATE: JConfig_Option read _GetOPTION_BIT_RATE;
    {class} property OPTION_INTRA_FRAME_INTERVAL: JConfig_Option read _GetOPTION_INTRA_FRAME_INTERVAL;
    {class} property OPTION_VIDEO_FRAME_RATE: JConfig_Option read _GetOPTION_VIDEO_FRAME_RATE;
  end;

  [JavaSignature('androidx/camera/core/impl/VideoCaptureConfig')]
  JVideoCaptureConfig = interface(JUseCaseConfig)
    ['{955E18BF-6B5F-4160-BBF9-EB2F95A4F672}']
    function getAudioBitRate: Integer; cdecl; overload;
    function getAudioBitRate(i: Integer): Integer; cdecl; overload;
    function getAudioChannelCount: Integer; cdecl; overload;
    function getAudioChannelCount(i: Integer): Integer; cdecl; overload;
    function getAudioMinBufferSize: Integer; cdecl; overload;
    function getAudioMinBufferSize(i: Integer): Integer; cdecl; overload;
    function getAudioRecordSource: Integer; cdecl; overload;
    function getAudioRecordSource(i: Integer): Integer; cdecl; overload;
    function getAudioSampleRate: Integer; cdecl; overload;
    function getAudioSampleRate(i: Integer): Integer; cdecl; overload;
    function getBitRate: Integer; cdecl; overload;
    function getBitRate(i: Integer): Integer; cdecl; overload;
    function getConfig: Jimpl_Config; cdecl;
    function getIFrameInterval: Integer; cdecl; overload;
    function getIFrameInterval(i: Integer): Integer; cdecl; overload;
    function getInputFormat: Integer; cdecl;
    function getVideoFrameRate: Integer; cdecl; overload;
    function getVideoFrameRate(i: Integer): Integer; cdecl; overload;
  end;
  TJVideoCaptureConfig = class(TJavaGenericImport<JVideoCaptureConfigClass, JVideoCaptureConfig>) end;

  JVideoCapture_DefaultsClass = interface(JConfigProviderClass)
    ['{71CF86B3-6307-4267-A4B4-DBD94C7B989C}']
    {class} function init: JVideoCapture_Defaults; cdecl;
  end;

  [JavaSignature('androidx/camera/core/VideoCapture$Defaults')]
  JVideoCapture_Defaults = interface(JConfigProvider)
    ['{1EEEB344-EF82-4EEB-BED2-A0D2E58B1928}']
    function getConfig: JVideoCaptureConfig; cdecl;
  end;
  TJVideoCapture_Defaults = class(TJavaGenericImport<JVideoCapture_DefaultsClass, JVideoCapture_Defaults>) end;

  JVideoCapture_MetadataClass = interface(JObjectClass)
    ['{DD370690-5FBF-4749-8F03-04AEADF41D26}']
    {class} function init: JVideoCapture_Metadata; cdecl;
  end;

  [JavaSignature('androidx/camera/core/VideoCapture$Metadata')]
  JVideoCapture_Metadata = interface(JObject)
    ['{38C5E54F-7E93-4DC4-B506-F4135AA47AD9}']
    function _Getlocation: JLocation; cdecl;
    procedure _Setlocation(Value: JLocation); cdecl;
    property location: JLocation read _Getlocation write _Setlocation;
  end;
  TJVideoCapture_Metadata = class(TJavaGenericImport<JVideoCapture_MetadataClass, JVideoCapture_Metadata>) end;

  JVideoCapture_OutputFileResultsClass = interface(JObjectClass)
    ['{B901858A-CB96-4438-93A3-ADF68E84DD5F}']
    {class} function init(uri: Jnet_Uri): JVideoCapture_OutputFileResults; cdecl;
  end;

  [JavaSignature('androidx/camera/core/VideoCapture$OutputFileResults')]
  JVideoCapture_OutputFileResults = interface(JObject)
    ['{B601CE06-3165-416D-83E7-0F2CB868F9B2}']
    function getSavedUri: Jnet_Uri; cdecl;
  end;
  TJVideoCapture_OutputFileResults = class(TJavaGenericImport<JVideoCapture_OutputFileResultsClass, JVideoCapture_OutputFileResults>) end;

  JVideoCapture_OnVideoSavedCallbackClass = interface(IJavaClass)
    ['{8FA452BF-7569-45FF-B9BB-02B989814556}']
  end;

  [JavaSignature('androidx/camera/core/VideoCapture$OnVideoSavedCallback')]
  JVideoCapture_OnVideoSavedCallback = interface(IJavaInstance)
    ['{B0DD7A52-685F-47C4-992B-A43C9C83B4E9}']
    procedure onError(i: Integer; string_: JString; throwable: JThrowable); cdecl;
    procedure onVideoSaved(outputFileResults: JVideoCapture_OutputFileResults); cdecl;
  end;
  TJVideoCapture_OnVideoSavedCallback = class(TJavaGenericImport<JVideoCapture_OnVideoSavedCallbackClass, JVideoCapture_OnVideoSavedCallback>) end;

  JVideoCapture_OutputFileOptionsClass = interface(JObjectClass)
    ['{4FF79B23-39D9-4EEF-A1EA-8DA9229CCB85}']
    {class} function init(file_: JFile; fileDescriptor: JFileDescriptor; contentResolver: JContentResolver; uri: Jnet_Uri;
      contentValues: JContentValues; metadata: JVideoCapture_Metadata): JVideoCapture_OutputFileOptions; cdecl;
  end;

  [JavaSignature('androidx/camera/core/VideoCapture$OutputFileOptions')]
  JVideoCapture_OutputFileOptions = interface(JObject)
    ['{7A886A8E-65A6-4D8F-A2F6-B311A68E1E48}']
  end;
  TJVideoCapture_OutputFileOptions = class(TJavaGenericImport<JVideoCapture_OutputFileOptionsClass, JVideoCapture_OutputFileOptions>) end;

  JVideoCaptureClass = interface(JUseCaseClass)
    ['{41FF09D4-A44E-46CD-8DC4-B34F358C46D3}']
    {class} function _GetDEFAULT_CONFIG: JVideoCapture_Defaults; cdecl;
    {class} function _GetERROR_ENCODER: Integer; cdecl;
    {class} function _GetERROR_FILE_IO: Integer; cdecl;
    {class} function _GetERROR_INVALID_CAMERA: Integer; cdecl;
    {class} function _GetERROR_MUXER: Integer; cdecl;
    {class} function _GetERROR_RECORDING_IN_PROGRESS: Integer; cdecl;
    {class} function _GetERROR_UNKNOWN: Integer; cdecl;
    {class} property DEFAULT_CONFIG: JVideoCapture_Defaults read _GetDEFAULT_CONFIG;
    {class} property ERROR_ENCODER: Integer read _GetERROR_ENCODER;
    {class} property ERROR_FILE_IO: Integer read _GetERROR_FILE_IO;
    {class} property ERROR_INVALID_CAMERA: Integer read _GetERROR_INVALID_CAMERA;
    {class} property ERROR_MUXER: Integer read _GetERROR_MUXER;
    {class} property ERROR_RECORDING_IN_PROGRESS: Integer read _GetERROR_RECORDING_IN_PROGRESS;
    {class} property ERROR_UNKNOWN: Integer read _GetERROR_UNKNOWN;
  end;

  [JavaSignature('androidx/camera/core/VideoCapture')]
  JVideoCapture = interface(JUseCase)
    ['{0292187B-C438-46E2-921A-054DC00EB0CD}']
    function getDefaultConfig(b: Boolean; useCaseConfigFactory: JUseCaseConfigFactory): JUseCaseConfig; cdecl;
    function getUseCaseConfigBuilder(config: Jimpl_Config): JUseCaseConfig_Builder; cdecl;
    procedure onAttached; cdecl;
    procedure onDetached; cdecl;
    procedure onStateDetached; cdecl;
    procedure setTargetRotation(i: Integer); cdecl;
    procedure startRecording(outputFileOptions: JVideoCapture_OutputFileOptions; executor: JExecutor;
      onVideoSavedCallback: JVideoCapture_OnVideoSavedCallback); cdecl;
    procedure stopRecording; cdecl;
  end;
  TJVideoCapture = class(TJavaGenericImport<JVideoCaptureClass, JVideoCapture>) end;

  JViewPortClass = interface(JObjectClass)
    ['{0588591D-F87E-4D2F-991F-BBBF997F88FC}']
    {class} function _GetFILL_CENTER: Integer; cdecl;
    {class} function _GetFILL_END: Integer; cdecl;
    {class} function _GetFILL_START: Integer; cdecl;
    {class} function _GetFIT: Integer; cdecl;
    {class} property FILL_CENTER: Integer read _GetFILL_CENTER;
    {class} property FILL_END: Integer read _GetFILL_END;
    {class} property FILL_START: Integer read _GetFILL_START;
    {class} property FIT: Integer read _GetFIT;
  end;

  [JavaSignature('androidx/camera/core/ViewPort')]
  JViewPort = interface(JObject)
    ['{CDBCFEA5-972E-4038-A86B-988B5CAD2E60}']
    function getAspectRatio: JRational; cdecl;
    function getLayoutDirection: Integer; cdecl;
    function getRotation: Integer; cdecl;
    function getScaleType: Integer; cdecl;
  end;
  TJViewPort = class(TJavaGenericImport<JViewPortClass, JViewPort>) end;

  JMetadata_BuilderClass = interface(JObjectClass)
    ['{65E5FE24-4018-4B9D-9B33-D5AC2A0F7E12}']
  end;

  [JavaSignature('androidx/camera/view/video/Metadata$Builder')]
  JMetadata_Builder = interface(JObject)
    ['{89DF1B8A-C188-4609-8FB6-E59CA8147C37}']
    function build: Jvideo_Metadata; cdecl;
    function setLocation(location: JLocation): JMetadata_Builder; cdecl;
  end;
  TJMetadata_Builder = class(TJavaGenericImport<JMetadata_BuilderClass, JMetadata_Builder>) end;

  Jvideo_MetadataClass = interface(JObjectClass)
    ['{B4257BBB-1C34-4AA9-9021-F5F1625EDB13}']
    {class} function builder: JMetadata_Builder; cdecl;
  end;

  [JavaSignature('androidx/camera/view/video/Metadata')]
  Jvideo_Metadata = interface(JObject)
    ['{FDF19FD1-6AFE-4E55-91C7-97CB66174AB8}']
    function getLocation: JLocation; cdecl;
  end;
  TJvideo_Metadata = class(TJavaGenericImport<Jvideo_MetadataClass, Jvideo_Metadata>) end;

  Jvideo_OutputFileOptions_BuilderClass = interface(JObjectClass)
    ['{5BEACAB0-95DE-4B38-96F0-8FE7420BC5E5}']
    {class} function init: Jvideo_OutputFileOptions_Builder; cdecl;
  end;

  [JavaSignature('androidx/camera/view/video/OutputFileOptions$Builder')]
  Jvideo_OutputFileOptions_Builder = interface(JObject)
    ['{B2C43CDC-AF45-4642-B71F-67A8BAD0ED3E}']
    function build: JOutputFileOptions; cdecl;
    function setMetadata(metadata: Jvideo_Metadata): Jvideo_OutputFileOptions_Builder; cdecl;
  end;
  TJvideo_OutputFileOptions_Builder = class(TJavaGenericImport<Jvideo_OutputFileOptions_BuilderClass, Jvideo_OutputFileOptions_Builder>) end;

  JOutputFileOptionsClass = interface(JObjectClass)
    ['{0044BBC3-CECB-4171-88EC-E90C12A1760D}']
    {class} function builder(parcelFileDescriptor: JParcelFileDescriptor): Jvideo_OutputFileOptions_Builder; cdecl; overload;
    {class} function builder(file_: JFile): Jvideo_OutputFileOptions_Builder; cdecl; overload;
    {class} function builder(contentResolver: JContentResolver; uri: Jnet_Uri;
      contentValues: JContentValues): Jvideo_OutputFileOptions_Builder; cdecl; overload;
    {class} function init: JOutputFileOptions; cdecl;
  end;

  [JavaSignature('androidx/camera/view/video/OutputFileOptions')]
  JOutputFileOptions = interface(JObject)
    ['{E69D6246-23CB-47D0-BCE2-8FAF7D44A796}']
    function getMetadata: Jvideo_Metadata; cdecl;
    function toVideoCaptureOutputFileOptions: JVideoCapture_OutputFileOptions; cdecl;
  end;
  TJOutputFileOptions = class(TJavaGenericImport<JOutputFileOptionsClass, JOutputFileOptions>) end;

  JOutputFileResultsClass = interface(JObjectClass)
    ['{D56C80F4-EC53-403A-A234-DAD075AD22AA}']
    {class} function create(uri: Jnet_Uri): JOutputFileResults; cdecl;
    {class} function init: JOutputFileResults; cdecl;
  end;

  [JavaSignature('androidx/camera/view/video/OutputFileResults')]
  JOutputFileResults = interface(JObject)
    ['{16091112-F950-45E1-823A-D86A2C7A5ECB}']
    function getSavedUri: Jnet_Uri; cdecl;
  end;
  TJOutputFileResults = class(TJavaGenericImport<JOutputFileResultsClass, JOutputFileResults>) end;

  JOnVideoSavedCallbackClass = interface(IJavaClass)
    ['{DD4C2588-8646-46AC-9392-C3BE7C43FC79}']
    {class} function _GetERROR_ENCODER: Integer; cdecl;
    {class} function _GetERROR_FILE_IO: Integer; cdecl;
    {class} function _GetERROR_INVALID_CAMERA: Integer; cdecl;
    {class} function _GetERROR_MUXER: Integer; cdecl;
    {class} function _GetERROR_RECORDING_IN_PROGRESS: Integer; cdecl;
    {class} function _GetERROR_UNKNOWN: Integer; cdecl;
    {class} procedure onVideoSaved(outputFileResults: JOutputFileResults); cdecl;
    {class} property ERROR_ENCODER: Integer read _GetERROR_ENCODER;
    {class} property ERROR_FILE_IO: Integer read _GetERROR_FILE_IO;
    {class} property ERROR_INVALID_CAMERA: Integer read _GetERROR_INVALID_CAMERA;
    {class} property ERROR_MUXER: Integer read _GetERROR_MUXER;
    {class} property ERROR_RECORDING_IN_PROGRESS: Integer read _GetERROR_RECORDING_IN_PROGRESS;
    {class} property ERROR_UNKNOWN: Integer read _GetERROR_UNKNOWN;
  end;

  [JavaSignature('androidx/camera/view/video/OnVideoSavedCallback')]
  JOnVideoSavedCallback = interface(IJavaInstance)
    ['{49017DED-89CB-46F3-8DFF-6D66CF8DB9A7}']
    procedure onError(i: Integer; string_: JString; throwable: JThrowable); cdecl;
  end;
  TJOnVideoSavedCallback = class(TJavaGenericImport<JOnVideoSavedCallbackClass, JOnVideoSavedCallback>) end;

  JImageCaptureExceptionClass = interface(JExceptionClass)
    ['{9B0A1BBC-F3B6-44BA-A427-58FA1D0FAFDC}']
    {class} function init(i: Integer; string_: JString; throwable: JThrowable): JImageCaptureException; cdecl;
  end;

  [JavaSignature('androidx/camera/core/ImageCaptureException')]
  JImageCaptureException = interface(JException)
    ['{21D72D0E-B5F9-4828-A591-B4AAD935E27B}']
    function getImageCaptureError: Integer; cdecl;
  end;
  TJImageCaptureException = class(TJavaGenericImport<JImageCaptureExceptionClass, JImageCaptureException>) end;

  JImageCapture_OnImageCapturedCallbackClass = interface(JObjectClass)
    ['{9BBCFD88-D895-485C-B4C3-74A11C31C083}']
    {class} function init: JImageCapture_OnImageCapturedCallback; cdecl;
  end;

  [JavaSignature('androidx/camera/core/ImageCapture$OnImageCapturedCallback')]
  JImageCapture_OnImageCapturedCallback = interface(JObject)
    ['{C7A78F6C-B752-4342-B8FB-823E93B807DF}']
    procedure onCaptureSuccess(imageProxy: JImageProxy); cdecl;
    procedure onError(imageCaptureException: JImageCaptureException); cdecl;
  end;
  TJImageCapture_OnImageCapturedCallback = class(TJavaGenericImport<JImageCapture_OnImageCapturedCallbackClass,
    JImageCapture_OnImageCapturedCallback>) end;

  JImageCapture_MetadataClass = interface(JObjectClass)
    ['{7722D26D-1F9A-49DB-82E3-414960FEBAAB}']
    {class} function init: JImageCapture_Metadata; cdecl;
  end;

  [JavaSignature('androidx/camera/core/ImageCapture$Metadata')]
  JImageCapture_Metadata = interface(JObject)
    ['{CE8BF7DA-FD98-4745-8F46-A0E3E4097465}']
    function getLocation: JLocation; cdecl;
    function isReversedHorizontal: Boolean; cdecl;
    function isReversedHorizontalSet: Boolean; cdecl;
    function isReversedVertical: Boolean; cdecl;
    procedure setLocation(location: JLocation); cdecl;
    procedure setReversedHorizontal(b: Boolean); cdecl;
    procedure setReversedVertical(b: Boolean); cdecl;
  end;
  TJImageCapture_Metadata = class(TJavaGenericImport<JImageCapture_MetadataClass, JImageCapture_Metadata>) end;

  JImageCapture_OutputFileOptionsClass = interface(JObjectClass)
    ['{E6894986-2598-4BBE-9968-7121F2143D51}']
    {class} function init(file_: JFile; contentResolver: JContentResolver; uri: Jnet_Uri; contentValues: JContentValues; outputStream: JOutputStream;
      metadata: JImageCapture_Metadata): JImageCapture_OutputFileOptions; cdecl;
  end;

  [JavaSignature('androidx/camera/core/ImageCapture$OutputFileOptions')]
  JImageCapture_OutputFileOptions = interface(JObject)
    ['{6E108D26-2BFB-49B1-9615-846B611B688F}']
    function getMetadata: JImageCapture_Metadata; cdecl;
  end;
  TJImageCapture_OutputFileOptions = class(TJavaGenericImport<JImageCapture_OutputFileOptionsClass, JImageCapture_OutputFileOptions>) end;

  JImageCapture_OutputFileResultsClass = interface(JObjectClass)
    ['{ACA512F0-5798-4855-AFCC-324C9478E912}']
    {class} function init(uri: Jnet_Uri): JImageCapture_OutputFileResults; cdecl;
  end;

  [JavaSignature('androidx/camera/core/ImageCapture$OutputFileResults')]
  JImageCapture_OutputFileResults = interface(JObject)
    ['{806CCA4E-1B47-412A-8B71-DD9AD57B7E2F}']
    function getSavedUri: Jnet_Uri; cdecl;
  end;
  TJImageCapture_OutputFileResults = class(TJavaGenericImport<JImageCapture_OutputFileResultsClass, JImageCapture_OutputFileResults>) end;

  JImageCapture_OnImageSavedCallbackClass = interface(IJavaClass)
    ['{3E2BAC04-87E3-412C-91C8-918C202332BF}']
  end;

  [JavaSignature('androidx/camera/core/ImageCapture$OnImageSavedCallback')]
  JImageCapture_OnImageSavedCallback = interface(IJavaInstance)
    ['{4E79EEB4-7395-4770-9A54-934F52492C5C}']
    procedure onError(imageCaptureException: JImageCaptureException); cdecl;
    procedure onImageSaved(outputFileResults: JImageCapture_OutputFileResults); cdecl;
  end;
  TJImageCapture_OnImageSavedCallback = class(TJavaGenericImport<JImageCapture_OnImageSavedCallbackClass, JImageCapture_OnImageSavedCallback>) end;

  JCameraControllerClass = interface(JObjectClass)
    ['{121DB08B-8CFE-4730-9854-85839B63FDC8}']
    {class} function _GetIMAGE_ANALYSIS: Integer; cdecl;
    {class} function _GetIMAGE_CAPTURE: Integer; cdecl;
    {class} function _GetVIDEO_CAPTURE: Integer; cdecl;
    // {class} function _GetmCameraProvider: JProcessCameraProvider; cdecl; // Internal??
    {class} function _GetmCameraSelector: JCameraSelector; cdecl;
    {class} function _GetmImageAnalysis: JImageAnalysis; cdecl;
    {class} function _GetmPreview: JPreview; cdecl;
    {class} function _GetmSensorRotationListener: JSensorRotationListener; cdecl;
    {class} function _GetmSurfaceProvider: JPreview_SurfaceProvider; cdecl;
    {class} function _GetmVideoCapture: JVideoCapture; cdecl;
    {class} function _GetmVideoIsRecording: JAtomicBoolean; cdecl;
    {class} function _GetmViewPort: JViewPort; cdecl;
    {class} function init(context: JContext): JCameraController; cdecl;
    {class} property IMAGE_ANALYSIS: Integer read _GetIMAGE_ANALYSIS;
    {class} property IMAGE_CAPTURE: Integer read _GetIMAGE_CAPTURE;
    {class} property VIDEO_CAPTURE: Integer read _GetVIDEO_CAPTURE;
    // {class} property mCameraProvider: JProcessCameraProvider read _GetmCameraProvider;
    {class} property mCameraSelector: JCameraSelector read _GetmCameraSelector;
    {class} property mImageAnalysis: JImageAnalysis read _GetmImageAnalysis;
    {class} property mPreview: JPreview read _GetmPreview;
    {class} property mSensorRotationListener: JSensorRotationListener read _GetmSensorRotationListener;
    {class} property mSurfaceProvider: JPreview_SurfaceProvider read _GetmSurfaceProvider;
    {class} property mVideoCapture: JVideoCapture read _GetmVideoCapture;
    {class} property mVideoIsRecording: JAtomicBoolean read _GetmVideoIsRecording;
    {class} property mViewPort: JViewPort read _GetmViewPort;
  end;

  [JavaSignature('androidx/camera/view/CameraController')]
  JCameraController = interface(JObject)
    ['{D3BB3AF0-198B-4C70-B0FC-A6C2A0AACFA2}']
    procedure clearImageAnalysisAnalyzer; cdecl;
    function enableTorch(b: Boolean): JListenableFuture; cdecl;
    function getCameraInfo: JCameraInfo; cdecl;
    function getCameraSelector: JCameraSelector; cdecl;
    function getImageAnalysisBackpressureStrategy: Integer; cdecl;
    function getImageAnalysisImageQueueDepth: Integer; cdecl;
    function getImageCaptureFlashMode: Integer; cdecl;
    function getInitializationFuture: JListenableFuture; cdecl;
    function getTorchState: JLiveData; cdecl;
    function getZoomState: JLiveData; cdecl;
    function hasCamera(cameraSelector: JCameraSelector): Boolean; cdecl;
    function isImageAnalysisEnabled: Boolean; cdecl;
    function isImageCaptureEnabled: Boolean; cdecl;
    function isPinchToZoomEnabled: Boolean; cdecl;
    function isRecording: Boolean; cdecl;
    function isTapToFocusEnabled: Boolean; cdecl;
    function isVideoCaptureEnabled: Boolean; cdecl;
    procedure setCameraSelector(cameraSelector: JCameraSelector); cdecl;
    procedure setEnabledUseCases(i: Integer); cdecl;
    procedure setImageAnalysisAnalyzer(executor: JExecutor; analyzer: JImageAnalysis_Analyzer); cdecl;
    procedure setImageAnalysisBackpressureStrategy(i: Integer); cdecl;
    procedure setImageAnalysisImageQueueDepth(i: Integer); cdecl;
    procedure setImageCaptureFlashMode(i: Integer); cdecl;
    function setLinearZoom(f: Single): JListenableFuture; cdecl;
    procedure setPinchToZoomEnabled(b: Boolean); cdecl;
    procedure setTapToFocusEnabled(b: Boolean); cdecl;
    function setZoomRatio(f: Single): JListenableFuture; cdecl;
    procedure startRecording(outputFileOptions: JOutputFileOptions; executor: JExecutor; onVideoSavedCallback: JOnVideoSavedCallback); cdecl;
    procedure stopRecording; cdecl;
    procedure takePicture(executor: JExecutor; onImageCapturedCallback: JImageCapture_OnImageCapturedCallback); cdecl; overload;
    procedure takePicture(outputFileOptions: JImageCapture_OutputFileOptions; executor: JExecutor;
      onImageSavedCallback: JImageCapture_OnImageSavedCallback); cdecl; overload;
  end;
  TJCameraController = class(TJavaGenericImport<JCameraControllerClass, JCameraController>) end;

  JMeteringPointClass = interface(JObjectClass)
    ['{B1B77DB6-F762-454F-93E4-1BE9FDE67271}']
    {class} function init(f: Single; f1: Single; f2: Single; rational: JRational): JMeteringPoint; cdecl;
  end;

  [JavaSignature('androidx/camera/core/MeteringPoint')]
  JMeteringPoint = interface(JObject)
    ['{5918C179-9D74-4F6E-A789-94D298369E87}']
    function getSize: Single; cdecl;
    function getSurfaceAspectRatio: JRational; cdecl;
    function getX: Single; cdecl;
    function getY: Single; cdecl;
  end;
  TJMeteringPoint = class(TJavaGenericImport<JMeteringPointClass, JMeteringPoint>) end;

  JMeteringPointFactoryClass = interface(JObjectClass)
    ['{64EBC821-3E46-41A9-9688-DE0FC32F5096}']
    {class} function getDefaultPointSize: Single; cdecl;
    {class} function init: JMeteringPointFactory; cdecl; overload;
    {class} function init(rational: JRational): JMeteringPointFactory; cdecl; overload;
  end;

  [JavaSignature('androidx/camera/core/MeteringPointFactory')]
  JMeteringPointFactory = interface(JObject)
    ['{188391D4-F5E5-4A8A-B798-6F81E53FC259}']
    function createPoint(f: Single; f1: Single): JMeteringPoint; cdecl; overload;
    function createPoint(f: Single; f1: Single; f2: Single): JMeteringPoint; cdecl; overload;
  end;
  TJMeteringPointFactory = class(TJavaGenericImport<JMeteringPointFactoryClass, JMeteringPointFactory>) end;

  JOutputTransformClass = interface(JObjectClass)
    ['{1BA933F0-D894-474D-BB2D-C25713C0F345}']
    {class} function init(matrix: JMatrix; size: Jutil_Size): JOutputTransform; cdecl;
  end;

  [JavaSignature('androidx/camera/view/transform/OutputTransform')]
  JOutputTransform = interface(JObject)
    ['{FC66A118-AE60-4896-92BC-B42B4E7236CA}']
    function _GetmMatrix: JMatrix; cdecl;
    property mMatrix: JMatrix read _GetmMatrix;
  end;
  TJOutputTransform = class(TJavaGenericImport<JOutputTransformClass, JOutputTransform>) end;

  JPreviewViewClass = interface(JFrameLayoutClass)
    ['{7AD36528-10D1-4217-99ED-722640D54C81}']
    {class} function _GetDEFAULT_BACKGROUND_COLOR: Integer; cdecl;
    {class} function init(context: JContext): JPreviewView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JPreviewView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet; i: Integer): JPreviewView; cdecl; overload;
    {class} function init(context: JContext; attributeSet: JAttributeSet; i: Integer; i1: Integer): JPreviewView; cdecl; overload;
    {class} function shouldUseTextureView(surfaceRequest: JSurfaceRequest; implementationMode: JPreviewView_ImplementationMode): Boolean; cdecl;
    {class} property DEFAULT_BACKGROUND_COLOR: Integer read _GetDEFAULT_BACKGROUND_COLOR;
  end;

  [JavaSignature('androidx/camera/view/PreviewView')]
  JPreviewView = interface(JFrameLayout)
    ['{6AC63BF0-C7D6-4498-9998-EF15FBD2DFD2}']
    function getBitmap: JBitmap; cdecl;
    function getController: JCameraController; cdecl;
    function getImplementationMode: JPreviewView_ImplementationMode; cdecl;
    function getMeteringPointFactory: JMeteringPointFactory; cdecl;
    function getOutputTransform: JOutputTransform; cdecl;
    function getPreviewStreamState: JLiveData; cdecl;
    function getScaleType: JPreviewView_ScaleType; cdecl;
    function getSurfaceProvider: JPreview_SurfaceProvider; cdecl;
    function getViewPort: JViewPort; cdecl; overload;
    function getViewPort(i: Integer): JViewPort; cdecl; overload;
    procedure onAttachedToWindow; cdecl;
    function onTouchEvent(motionEvent: JMotionEvent): Boolean; cdecl;
    function performClick: Boolean; cdecl;
    procedure redrawPreview; cdecl;
    procedure setController(cameraController: JCameraController); cdecl;
    procedure setImplementationMode(implementationMode: JPreviewView_ImplementationMode); cdecl;
    procedure setScaleType(scaleType: JPreviewView_ScaleType); cdecl;
  end;
  TJPreviewView = class(TJavaGenericImport<JPreviewViewClass, JPreviewView>) end;

  JPreviewView_ImplementationModeClass = interface(JEnumClass)
    ['{A87096F3-F670-4108-B257-516A2C6DF683}']
    {class} function _GetCOMPATIBLE: JPreviewView_ImplementationMode; cdecl;
    {class} function _GetPERFORMANCE: JPreviewView_ImplementationMode; cdecl;
    {class} function fromId(i: Integer): JPreviewView_ImplementationMode; cdecl;
    {class} function valueOf(string_: JString): JPreviewView_ImplementationMode; cdecl;
    {class} function values: TJavaObjectArray<JPreviewView_ImplementationMode>; cdecl;
    {class} property COMPATIBLE: JPreviewView_ImplementationMode read _GetCOMPATIBLE;
    {class} property PERFORMANCE: JPreviewView_ImplementationMode read _GetPERFORMANCE;
  end;

  [JavaSignature('androidx/camera/view/PreviewView$ImplementationMode')]
  JPreviewView_ImplementationMode = interface(JEnum)
    ['{B493E3EF-C5AC-4D55-9CF6-ABCD4AF00927}']
    function getId: Integer; cdecl;
  end;
  TJPreviewView_ImplementationMode = class(TJavaGenericImport<JPreviewView_ImplementationModeClass, JPreviewView_ImplementationMode>) end;

  JPreviewView_ScaleTypeClass = interface(JEnumClass)
    ['{2C257D4D-58F8-4FCA-930A-9003C343D029}']
    {class} function _GetFILL_CENTER: JPreviewView_ScaleType; cdecl;
    {class} function _GetFILL_END: JPreviewView_ScaleType; cdecl;
    {class} function _GetFILL_START: JPreviewView_ScaleType; cdecl;
    {class} function _GetFIT_CENTER: JPreviewView_ScaleType; cdecl;
    {class} function _GetFIT_END: JPreviewView_ScaleType; cdecl;
    {class} function _GetFIT_START: JPreviewView_ScaleType; cdecl;
    {class} function fromId(i: Integer): JPreviewView_ScaleType; cdecl;
    {class} function valueOf(string_: JString): JPreviewView_ScaleType; cdecl;
    {class} function values: TJavaObjectArray<JPreviewView_ScaleType>; cdecl;
    {class} property FILL_CENTER: JPreviewView_ScaleType read _GetFILL_CENTER;
    {class} property FILL_END: JPreviewView_ScaleType read _GetFILL_END;
    {class} property FILL_START: JPreviewView_ScaleType read _GetFILL_START;
    {class} property FIT_CENTER: JPreviewView_ScaleType read _GetFIT_CENTER;
    {class} property FIT_END: JPreviewView_ScaleType read _GetFIT_END;
    {class} property FIT_START: JPreviewView_ScaleType read _GetFIT_START;
  end;

  [JavaSignature('androidx/camera/view/PreviewView$ScaleType')]
  JPreviewView_ScaleType = interface(JEnum)
    ['{25CE4223-260F-4F04-9ACA-803A253C3988}']
    function getId: Integer; cdecl;
  end;
  TJPreviewView_ScaleType = class(TJavaGenericImport<JPreviewView_ScaleTypeClass, JPreviewView_ScaleType>) end;

  JPreviewViewImplementationClass = interface(JObjectClass)
    ['{8D985848-1247-46F7-9FAF-E7B874C910BD}']
    {class} function init(frameLayout: JFrameLayout; previewTransformation: JPreviewTransformation): JPreviewViewImplementation; cdecl;
  end;

  [JavaSignature('androidx/camera/view/PreviewViewImplementation')]
  JPreviewViewImplementation = interface(JObject)
    ['{36668EFC-5B77-44BE-91EE-F42229993033}']
    function getPreviewBitmap: JBitmap; cdecl;
    procedure onAttachedToWindow; cdecl;
    procedure onDetachedFromWindow; cdecl;
    procedure onSurfaceRequested(surfaceRequest: JSurfaceRequest;
      onSurfaceNotInUseListener: JPreviewViewImplementation_OnSurfaceNotInUseListener); cdecl;
    procedure redrawPreview; cdecl;
    function waitForNextFrame: JListenableFuture; cdecl;
  end;
  TJPreviewViewImplementation = class(TJavaGenericImport<JPreviewViewImplementationClass, JPreviewViewImplementation>) end;

  JPreviewViewImplementation_OnSurfaceNotInUseListenerClass = interface(IJavaClass)
    ['{03B280B8-B02F-4BA8-A1ED-DBF3FD8C5193}']
  end;

  [JavaSignature('androidx/camera/view/PreviewViewImplementation$OnSurfaceNotInUseListener')]
  JPreviewViewImplementation_OnSurfaceNotInUseListener = interface(IJavaInstance)
    ['{9FD279BD-3E3A-43D6-A862-E5BE794FAD53}']
    procedure onSurfaceNotInUse; cdecl;
  end;
  TJPreviewViewImplementation_OnSurfaceNotInUseListener = class(TJavaGenericImport<JPreviewViewImplementation_OnSurfaceNotInUseListenerClass,
    JPreviewViewImplementation_OnSurfaceNotInUseListener>) end;

  JPreviewViewMeteringPointFactoryClass = interface(JMeteringPointFactoryClass)
    ['{D1FA5130-422C-4AB5-BD5C-AE0513D0541A}']
    {class} function _GetINVALID_POINT: JPointF; cdecl;
    {class} property INVALID_POINT: JPointF read _GetINVALID_POINT;
  end;

  [JavaSignature('androidx/camera/view/PreviewViewMeteringPointFactory')]
  JPreviewViewMeteringPointFactory = interface(JMeteringPointFactory)
    ['{A481CC56-667C-42A0-9A58-8E63746A2D37}']
    procedure recalculate(size: Jutil_Size; i: Integer); cdecl;
  end;
  TJPreviewViewMeteringPointFactory = class(TJavaGenericImport<JPreviewViewMeteringPointFactoryClass, JPreviewViewMeteringPointFactory>) end;

  JCamera2ConfigClass = interface(JObjectClass)
    ['{832A16D0-9313-4DA3-95B8-6607A4F21035}']
    {class} function defaultConfig: JCameraXConfig; cdecl;
  end;

  [JavaSignature('androidx/camera/camera2/Camera2Config')]
  JCamera2Config = interface(JObject)
    ['{F7A8B1EC-8A8A-47CB-B701-3CA908F8CF2C}']
  end;
  TJCamera2Config = class(TJavaGenericImport<JCamera2ConfigClass, JCamera2Config>) end;

implementation

end.
