unit DW.Androidapi.JNI.Hardware.Camera2;

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
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,Androidapi.JNI.Location,
  Androidapi.JNI.Media, Androidapi.JNI.Os, Androidapi.JNI.Util;

type
  JCameraAccessException = interface;
  JCameraCaptureSession = interface;
  JCameraCaptureSession_CaptureCallback = interface;
  JCameraCaptureSession_StateCallback = interface;
  JCameraMetadata = interface;
  JCameraCharacteristics = interface;
  JCameraCharacteristics_Key = interface;
  JCameraConstrainedHighSpeedCaptureSession = interface;
  JCameraDevice = interface;
  JCameraDevice_StateCallback = interface;
  JCameraManager = interface;
  JCameraManager_AvailabilityCallback = interface;
  JCameraManager_TorchCallback = interface;
  JCaptureFailure = interface;
  JCaptureRequest = interface;
  JCaptureRequest_Builder = interface;
  JCaptureRequest_Key = interface;
  JCaptureResult = interface;
  JCaptureResult_Key = interface;
  JDngCreator = interface;
  JTotalCaptureResult = interface;
  JBlackLevelPattern = interface;
  JColorSpaceTransform = interface;
  JFace = interface;
  JInputConfiguration = interface;
  JLensShadingMap = interface;
  JMeteringRectangle = interface;
  JRggbChannelVector = interface;
  JStreamConfigurationMap = interface;
  JTonemapCurve = interface;
  JRational = interface;

  JCameraAccessExceptionClass = interface(JAndroidExceptionClass)
    ['{6D89B741-CFAB-4BF8-9D49-46781A24DA26}']
    {class} function _GetCAMERA_DISABLED: Integer; cdecl;
    {class} function _GetCAMERA_DISCONNECTED: Integer; cdecl;
    {class} function _GetCAMERA_ERROR: Integer; cdecl;
    {class} function _GetCAMERA_IN_USE: Integer; cdecl;
    {class} function _GetMAX_CAMERAS_IN_USE: Integer; cdecl;
    {class} function init(problem: Integer): JCameraAccessException; cdecl; overload;
    {class} function init(problem: Integer; message: JString): JCameraAccessException; cdecl; overload;
    {class} function init(problem: Integer; message: JString; cause: JThrowable): JCameraAccessException; cdecl; overload;
    {class} function init(problem: Integer; cause: JThrowable): JCameraAccessException; cdecl; overload;
    {class} function getReason: Integer; cdecl;
    {class} property CAMERA_DISABLED: Integer read _GetCAMERA_DISABLED;
    {class} property CAMERA_DISCONNECTED: Integer read _GetCAMERA_DISCONNECTED;
    {class} property CAMERA_ERROR: Integer read _GetCAMERA_ERROR;
    {class} property CAMERA_IN_USE: Integer read _GetCAMERA_IN_USE;
    {class} property MAX_CAMERAS_IN_USE: Integer read _GetMAX_CAMERAS_IN_USE;
  end;

  [JavaSignature('android/hardware/camera2/CameraAccessException')]
  JCameraAccessException = interface(JAndroidException)
    ['{E60D1813-ED97-4FB8-83F1-2A5AC6D85CDF}']
  end;
  TJCameraAccessException = class(TJavaGenericImport<JCameraAccessExceptionClass, JCameraAccessException>) end;

  JCameraCaptureSessionClass = interface(JObjectClass)
    ['{4535BAC2-8738-41EB-BFDA-06C7FE8C7DEA}']
    {class} function init: JCameraCaptureSession; cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CameraCaptureSession')]
  JCameraCaptureSession = interface(JObject)
    ['{2D604E80-CFA6-44D5-BB3F-1F21C2A46D06}']
    procedure abortCaptures; cdecl;
    function capture(request: JCaptureRequest; listener: JCameraCaptureSession_CaptureCallback; handler: JHandler): Integer; cdecl;
    function captureBurst(requests: JList; listener: JCameraCaptureSession_CaptureCallback; handler: JHandler): Integer; cdecl;
    procedure close; cdecl;
    function getDevice: JCameraDevice; cdecl;
    function getInputSurface: JSurface; cdecl;
    function isReprocessable: Boolean; cdecl;
    procedure prepare(surface: JSurface); cdecl;
    function setRepeatingBurst(requests: JList; listener: JCameraCaptureSession_CaptureCallback; handler: JHandler): Integer; cdecl;
    function setRepeatingRequest(request: JCaptureRequest; listener: JCameraCaptureSession_CaptureCallback; handler: JHandler): Integer; cdecl;
    procedure stopRepeating; cdecl;
  end;
  TJCameraCaptureSession = class(TJavaGenericImport<JCameraCaptureSessionClass, JCameraCaptureSession>) end;

  JCameraCaptureSession_CaptureCallbackClass = interface(JObjectClass)
    ['{E92D21AF-B49C-4892-AD5D-EC5B8FAAE3D4}']
    {class} function init: JCameraCaptureSession_CaptureCallback; cdecl;
    {class} procedure onCaptureSequenceAborted(session: JCameraCaptureSession; sequenceId: Integer); cdecl;
    {class} procedure onCaptureSequenceCompleted(session: JCameraCaptureSession; sequenceId: Integer; frameNumber: Int64); cdecl;
    {class} procedure onCaptureStarted(session: JCameraCaptureSession; request: JCaptureRequest; timestamp: Int64; frameNumber: Int64); cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CameraCaptureSession$CaptureCallback')]
  JCameraCaptureSession_CaptureCallback = interface(JObject)
    ['{6D2B5468-FB83-44A9-9F12-B06821B25906}']
    procedure onCaptureCompleted(session: JCameraCaptureSession; request: JCaptureRequest; result: JTotalCaptureResult); cdecl;
    procedure onCaptureFailed(session: JCameraCaptureSession; request: JCaptureRequest; failure: JCaptureFailure); cdecl;
    procedure onCaptureProgressed(session: JCameraCaptureSession; request: JCaptureRequest; partialResult: JCaptureResult); cdecl;
  end;
  TJCameraCaptureSession_CaptureCallback = class(TJavaGenericImport<JCameraCaptureSession_CaptureCallbackClass, JCameraCaptureSession_CaptureCallback>) end;

  JCameraCaptureSession_StateCallbackClass = interface(JObjectClass)
    ['{70073DE1-D53A-42EA-AAC3-443C024DC8F1}']
    {class} function init: JCameraCaptureSession_StateCallback; cdecl;
    {class} procedure onActive(session: JCameraCaptureSession); cdecl;
    {class} procedure onClosed(session: JCameraCaptureSession); cdecl;
    {class} procedure onConfigureFailed(session: JCameraCaptureSession); cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CameraCaptureSession$StateCallback')]
  JCameraCaptureSession_StateCallback = interface(JObject)
    ['{63DAF493-8EAF-4FF1-B4F3-A13630F37910}']
    procedure onConfigured(session: JCameraCaptureSession); cdecl;
    procedure onReady(session: JCameraCaptureSession); cdecl;
    procedure onSurfacePrepared(session: JCameraCaptureSession; surface: JSurface); cdecl;
  end;
  TJCameraCaptureSession_StateCallback = class(TJavaGenericImport<JCameraCaptureSession_StateCallbackClass, JCameraCaptureSession_StateCallback>) end;

  JCameraMetadataClass = interface(JObjectClass)
    ['{D30721F9-A2C4-4C69-BB9D-EFCBB3779AA6}']
    {class} function _GetCOLOR_CORRECTION_ABERRATION_MODE_FAST: Integer; cdecl;
    {class} function _GetCOLOR_CORRECTION_ABERRATION_MODE_HIGH_QUALITY: Integer; cdecl;
    {class} function _GetCOLOR_CORRECTION_ABERRATION_MODE_OFF: Integer; cdecl;
    {class} function _GetCOLOR_CORRECTION_MODE_FAST: Integer; cdecl;
    {class} function _GetCOLOR_CORRECTION_MODE_HIGH_QUALITY: Integer; cdecl;
    {class} function _GetCOLOR_CORRECTION_MODE_TRANSFORM_MATRIX: Integer; cdecl;
    {class} function _GetCONTROL_AE_ANTIBANDING_MODE_50HZ: Integer; cdecl;
    {class} function _GetCONTROL_AE_ANTIBANDING_MODE_60HZ: Integer; cdecl;
    {class} function _GetCONTROL_AE_ANTIBANDING_MODE_AUTO: Integer; cdecl;
    {class} function _GetCONTROL_AE_ANTIBANDING_MODE_OFF: Integer; cdecl;
    {class} function _GetCONTROL_AE_MODE_OFF: Integer; cdecl;
    {class} function _GetCONTROL_AE_MODE_ON: Integer; cdecl;
    {class} function _GetCONTROL_AE_MODE_ON_ALWAYS_FLASH: Integer; cdecl;
    {class} function _GetCONTROL_AE_MODE_ON_AUTO_FLASH: Integer; cdecl;
    {class} function _GetCONTROL_AE_MODE_ON_AUTO_FLASH_REDEYE: Integer; cdecl;
    {class} function _GetCONTROL_AE_PRECAPTURE_TRIGGER_CANCEL: Integer; cdecl;
    {class} function _GetCONTROL_AE_PRECAPTURE_TRIGGER_IDLE: Integer; cdecl;
    {class} function _GetCONTROL_AE_PRECAPTURE_TRIGGER_START: Integer; cdecl;
    {class} function _GetCONTROL_AE_STATE_CONVERGED: Integer; cdecl;
    {class} function _GetCONTROL_AE_STATE_FLASH_REQUIRED: Integer; cdecl;
    {class} function _GetCONTROL_AE_STATE_INACTIVE: Integer; cdecl;
    {class} function _GetCONTROL_AE_STATE_LOCKED: Integer; cdecl;
    {class} function _GetCONTROL_AE_STATE_PRECAPTURE: Integer; cdecl;
    {class} function _GetCONTROL_AE_STATE_SEARCHING: Integer; cdecl;
    {class} function _GetCONTROL_AF_MODE_AUTO: Integer; cdecl;
    {class} function _GetCONTROL_AF_MODE_CONTINUOUS_PICTURE: Integer; cdecl;
    {class} function _GetCONTROL_AF_MODE_CONTINUOUS_VIDEO: Integer; cdecl;
    {class} function _GetCONTROL_AF_MODE_EDOF: Integer; cdecl;
    {class} function _GetCONTROL_AF_MODE_MACRO: Integer; cdecl;
    {class} function _GetCONTROL_AF_MODE_OFF: Integer; cdecl;
    {class} function _GetCONTROL_AF_STATE_ACTIVE_SCAN: Integer; cdecl;
    {class} function _GetCONTROL_AF_STATE_FOCUSED_LOCKED: Integer; cdecl;
    {class} function _GetCONTROL_AF_STATE_INACTIVE: Integer; cdecl;
    {class} function _GetCONTROL_AF_STATE_NOT_FOCUSED_LOCKED: Integer; cdecl;
    {class} function _GetCONTROL_AF_STATE_PASSIVE_FOCUSED: Integer; cdecl;
    {class} function _GetCONTROL_AF_STATE_PASSIVE_SCAN: Integer; cdecl;
    {class} function _GetCONTROL_AF_STATE_PASSIVE_UNFOCUSED: Integer; cdecl;
    {class} function _GetCONTROL_AF_TRIGGER_CANCEL: Integer; cdecl;
    {class} function _GetCONTROL_AF_TRIGGER_IDLE: Integer; cdecl;
    {class} function _GetCONTROL_AF_TRIGGER_START: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_AUTO: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_CLOUDY_DAYLIGHT: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_DAYLIGHT: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_FLUORESCENT: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_INCANDESCENT: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_OFF: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_SHADE: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_TWILIGHT: Integer; cdecl;
    {class} function _GetCONTROL_AWB_MODE_WARM_FLUORESCENT: Integer; cdecl;
    {class} function _GetCONTROL_AWB_STATE_CONVERGED: Integer; cdecl;
    {class} function _GetCONTROL_AWB_STATE_INACTIVE: Integer; cdecl;
    {class} function _GetCONTROL_AWB_STATE_LOCKED: Integer; cdecl;
    {class} function _GetCONTROL_AWB_STATE_SEARCHING: Integer; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT_CUSTOM: Integer; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT_MANUAL: Integer; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT_PREVIEW: Integer; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT_STILL_CAPTURE: Integer; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT_VIDEO_RECORD: Integer; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT_VIDEO_SNAPSHOT: Integer; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT_ZERO_SHUTTER_LAG: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_AQUA: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_BLACKBOARD: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_MONO: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_NEGATIVE: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_OFF: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_POSTERIZE: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_SEPIA: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_SOLARIZE: Integer; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE_WHITEBOARD: Integer; cdecl;
    {class} function _GetCONTROL_MODE_AUTO: Integer; cdecl;
    {class} function _GetCONTROL_MODE_OFF: Integer; cdecl;
    {class} function _GetCONTROL_MODE_OFF_KEEP_STATE: Integer; cdecl;
    {class} function _GetCONTROL_MODE_USE_SCENE_MODE: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_ACTION: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_BARCODE: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_BEACH: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_CANDLELIGHT: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_DISABLED: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_FACE_PRIORITY: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_FIREWORKS: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_HDR: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_HIGH_SPEED_VIDEO: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_LANDSCAPE: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_NIGHT: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_NIGHT_PORTRAIT: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_PARTY: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_PORTRAIT: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_SNOW: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_SPORTS: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_STEADYPHOTO: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_SUNSET: Integer; cdecl;
    {class} function _GetCONTROL_SCENE_MODE_THEATRE: Integer; cdecl;
    {class} function _GetCONTROL_VIDEO_STABILIZATION_MODE_OFF: Integer; cdecl;
    {class} function _GetCONTROL_VIDEO_STABILIZATION_MODE_ON: Integer; cdecl;
    {class} function _GetEDGE_MODE_FAST: Integer; cdecl;
    {class} function _GetEDGE_MODE_HIGH_QUALITY: Integer; cdecl;
    {class} function _GetEDGE_MODE_OFF: Integer; cdecl;
    {class} function _GetEDGE_MODE_ZERO_SHUTTER_LAG: Integer; cdecl;
    {class} function _GetFLASH_MODE_OFF: Integer; cdecl;
    {class} function _GetFLASH_MODE_SINGLE: Integer; cdecl;
    {class} function _GetFLASH_MODE_TORCH: Integer; cdecl;
    {class} function _GetFLASH_STATE_CHARGING: Integer; cdecl;
    {class} function _GetFLASH_STATE_FIRED: Integer; cdecl;
    {class} function _GetFLASH_STATE_PARTIAL: Integer; cdecl;
    {class} function _GetFLASH_STATE_READY: Integer; cdecl;
    {class} function _GetFLASH_STATE_UNAVAILABLE: Integer; cdecl;
    {class} function _GetHOT_PIXEL_MODE_FAST: Integer; cdecl;
    {class} function _GetHOT_PIXEL_MODE_HIGH_QUALITY: Integer; cdecl;
    {class} function _GetHOT_PIXEL_MODE_OFF: Integer; cdecl;
    {class} function _GetINFO_SUPPORTED_HARDWARE_LEVEL_FULL: Integer; cdecl;
    {class} function _GetINFO_SUPPORTED_HARDWARE_LEVEL_LEGACY: Integer; cdecl;
    {class} function _GetINFO_SUPPORTED_HARDWARE_LEVEL_LIMITED: Integer; cdecl;
    {class} function _GetLENS_FACING_BACK: Integer; cdecl;
    {class} function _GetLENS_FACING_EXTERNAL: Integer; cdecl;
    {class} function _GetLENS_FACING_FRONT: Integer; cdecl;
    {class} function _GetLENS_INFO_FOCUS_DISTANCE_CALIBRATION_APPROXIMATE: Integer; cdecl;
    {class} function _GetLENS_INFO_FOCUS_DISTANCE_CALIBRATION_CALIBRATED: Integer; cdecl;
    {class} function _GetLENS_INFO_FOCUS_DISTANCE_CALIBRATION_UNCALIBRATED: Integer; cdecl;
    {class} function _GetLENS_OPTICAL_STABILIZATION_MODE_OFF: Integer; cdecl;
    {class} function _GetLENS_OPTICAL_STABILIZATION_MODE_ON: Integer; cdecl;
    {class} function _GetLENS_STATE_MOVING: Integer; cdecl;
    {class} function _GetLENS_STATE_STATIONARY: Integer; cdecl;
    {class} function _GetNOISE_REDUCTION_MODE_FAST: Integer; cdecl;
    {class} function _GetNOISE_REDUCTION_MODE_HIGH_QUALITY: Integer; cdecl;
    {class} function _GetNOISE_REDUCTION_MODE_MINIMAL: Integer; cdecl;
    {class} function _GetNOISE_REDUCTION_MODE_OFF: Integer; cdecl;
    {class} function _GetNOISE_REDUCTION_MODE_ZERO_SHUTTER_LAG: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_BACKWARD_COMPATIBLE: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_BURST_CAPTURE: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_CONSTRAINED_HIGH_SPEED_VIDEO: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_DEPTH_OUTPUT: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_MANUAL_POST_PROCESSING: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_MANUAL_SENSOR: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_PRIVATE_REPROCESSING: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_RAW: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_READ_SENSOR_SETTINGS: Integer; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES_YUV_REPROCESSING: Integer; cdecl;
    {class} function _GetSCALER_CROPPING_TYPE_CENTER_ONLY: Integer; cdecl;
    {class} function _GetSCALER_CROPPING_TYPE_FREEFORM: Integer; cdecl;
    {class} function _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_BGGR: Integer; cdecl;
    {class} function _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_GBRG: Integer; cdecl;
    {class} function _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_GRBG: Integer; cdecl;
    {class} function _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_RGB: Integer; cdecl;
    {class} function _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_RGGB: Integer; cdecl;
    {class} function _GetSENSOR_INFO_TIMESTAMP_SOURCE_REALTIME: Integer; cdecl;
    {class} function _GetSENSOR_INFO_TIMESTAMP_SOURCE_UNKNOWN: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_CLOUDY_WEATHER: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_COOL_WHITE_FLUORESCENT: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_D50: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_D55: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_D65: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_D75: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_DAYLIGHT: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_DAYLIGHT_FLUORESCENT: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_DAY_WHITE_FLUORESCENT: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_FINE_WEATHER: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_FLASH: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_FLUORESCENT: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_ISO_STUDIO_TUNGSTEN: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_SHADE: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_STANDARD_A: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_STANDARD_B: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_STANDARD_C: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_TUNGSTEN: Integer; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1_WHITE_FLUORESCENT: Integer; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_MODE_COLOR_BARS: Integer; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_MODE_COLOR_BARS_FADE_TO_GRAY: Integer; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_MODE_CUSTOM1: Integer; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_MODE_OFF: Integer; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_MODE_PN9: Integer; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_MODE_SOLID_COLOR: Integer; cdecl;
    {class} function _GetSHADING_MODE_FAST: Integer; cdecl;
    {class} function _GetSHADING_MODE_HIGH_QUALITY: Integer; cdecl;
    {class} function _GetSHADING_MODE_OFF: Integer; cdecl;
    {class} function _GetSTATISTICS_FACE_DETECT_MODE_FULL: Integer; cdecl;
    {class} function _GetSTATISTICS_FACE_DETECT_MODE_OFF: Integer; cdecl;
    {class} function _GetSTATISTICS_FACE_DETECT_MODE_SIMPLE: Integer; cdecl;
    {class} function _GetSTATISTICS_LENS_SHADING_MAP_MODE_OFF: Integer; cdecl;
    {class} function _GetSTATISTICS_LENS_SHADING_MAP_MODE_ON: Integer; cdecl;
    {class} function _GetSTATISTICS_SCENE_FLICKER_50HZ: Integer; cdecl;
    {class} function _GetSTATISTICS_SCENE_FLICKER_60HZ: Integer; cdecl;
    {class} function _GetSTATISTICS_SCENE_FLICKER_NONE: Integer; cdecl;
    {class} function _GetSYNC_MAX_LATENCY_PER_FRAME_CONTROL: Integer; cdecl;
    {class} function _GetSYNC_MAX_LATENCY_UNKNOWN: Integer; cdecl;
    {class} function _GetTONEMAP_MODE_CONTRAST_CURVE: Integer; cdecl;
    {class} function _GetTONEMAP_MODE_FAST: Integer; cdecl;
    {class} function _GetTONEMAP_MODE_GAMMA_VALUE: Integer; cdecl;
    {class} function _GetTONEMAP_MODE_HIGH_QUALITY: Integer; cdecl;
    {class} function _GetTONEMAP_MODE_PRESET_CURVE: Integer; cdecl;
    {class} function _GetTONEMAP_PRESET_CURVE_REC709: Integer; cdecl;
    {class} function _GetTONEMAP_PRESET_CURVE_SRGB: Integer; cdecl;
    {class} function getKeys: JList; cdecl;
    {class} property COLOR_CORRECTION_ABERRATION_MODE_FAST: Integer read _GetCOLOR_CORRECTION_ABERRATION_MODE_FAST;
    {class} property COLOR_CORRECTION_ABERRATION_MODE_HIGH_QUALITY: Integer read _GetCOLOR_CORRECTION_ABERRATION_MODE_HIGH_QUALITY;
    {class} property COLOR_CORRECTION_ABERRATION_MODE_OFF: Integer read _GetCOLOR_CORRECTION_ABERRATION_MODE_OFF;
    {class} property COLOR_CORRECTION_MODE_FAST: Integer read _GetCOLOR_CORRECTION_MODE_FAST;
    {class} property COLOR_CORRECTION_MODE_HIGH_QUALITY: Integer read _GetCOLOR_CORRECTION_MODE_HIGH_QUALITY;
    {class} property COLOR_CORRECTION_MODE_TRANSFORM_MATRIX: Integer read _GetCOLOR_CORRECTION_MODE_TRANSFORM_MATRIX;
    {class} property CONTROL_AE_ANTIBANDING_MODE_50HZ: Integer read _GetCONTROL_AE_ANTIBANDING_MODE_50HZ;
    {class} property CONTROL_AE_ANTIBANDING_MODE_60HZ: Integer read _GetCONTROL_AE_ANTIBANDING_MODE_60HZ;
    {class} property CONTROL_AE_ANTIBANDING_MODE_AUTO: Integer read _GetCONTROL_AE_ANTIBANDING_MODE_AUTO;
    {class} property CONTROL_AE_ANTIBANDING_MODE_OFF: Integer read _GetCONTROL_AE_ANTIBANDING_MODE_OFF;
    {class} property CONTROL_AE_MODE_OFF: Integer read _GetCONTROL_AE_MODE_OFF;
    {class} property CONTROL_AE_MODE_ON: Integer read _GetCONTROL_AE_MODE_ON;
    {class} property CONTROL_AE_MODE_ON_ALWAYS_FLASH: Integer read _GetCONTROL_AE_MODE_ON_ALWAYS_FLASH;
    {class} property CONTROL_AE_MODE_ON_AUTO_FLASH: Integer read _GetCONTROL_AE_MODE_ON_AUTO_FLASH;
    {class} property CONTROL_AE_MODE_ON_AUTO_FLASH_REDEYE: Integer read _GetCONTROL_AE_MODE_ON_AUTO_FLASH_REDEYE;
    {class} property CONTROL_AE_PRECAPTURE_TRIGGER_CANCEL: Integer read _GetCONTROL_AE_PRECAPTURE_TRIGGER_CANCEL;
    {class} property CONTROL_AE_PRECAPTURE_TRIGGER_IDLE: Integer read _GetCONTROL_AE_PRECAPTURE_TRIGGER_IDLE;
    {class} property CONTROL_AE_PRECAPTURE_TRIGGER_START: Integer read _GetCONTROL_AE_PRECAPTURE_TRIGGER_START;
    {class} property CONTROL_AE_STATE_CONVERGED: Integer read _GetCONTROL_AE_STATE_CONVERGED;
    {class} property CONTROL_AE_STATE_FLASH_REQUIRED: Integer read _GetCONTROL_AE_STATE_FLASH_REQUIRED;
    {class} property CONTROL_AE_STATE_INACTIVE: Integer read _GetCONTROL_AE_STATE_INACTIVE;
    {class} property CONTROL_AE_STATE_LOCKED: Integer read _GetCONTROL_AE_STATE_LOCKED;
    {class} property CONTROL_AE_STATE_PRECAPTURE: Integer read _GetCONTROL_AE_STATE_PRECAPTURE;
    {class} property CONTROL_AE_STATE_SEARCHING: Integer read _GetCONTROL_AE_STATE_SEARCHING;
    {class} property CONTROL_AF_MODE_AUTO: Integer read _GetCONTROL_AF_MODE_AUTO;
    {class} property CONTROL_AF_MODE_CONTINUOUS_PICTURE: Integer read _GetCONTROL_AF_MODE_CONTINUOUS_PICTURE;
    {class} property CONTROL_AF_MODE_CONTINUOUS_VIDEO: Integer read _GetCONTROL_AF_MODE_CONTINUOUS_VIDEO;
    {class} property CONTROL_AF_MODE_EDOF: Integer read _GetCONTROL_AF_MODE_EDOF;
    {class} property CONTROL_AF_MODE_MACRO: Integer read _GetCONTROL_AF_MODE_MACRO;
    {class} property CONTROL_AF_MODE_OFF: Integer read _GetCONTROL_AF_MODE_OFF;
    {class} property CONTROL_AF_STATE_ACTIVE_SCAN: Integer read _GetCONTROL_AF_STATE_ACTIVE_SCAN;
    {class} property CONTROL_AF_STATE_FOCUSED_LOCKED: Integer read _GetCONTROL_AF_STATE_FOCUSED_LOCKED;
    {class} property CONTROL_AF_STATE_INACTIVE: Integer read _GetCONTROL_AF_STATE_INACTIVE;
    {class} property CONTROL_AF_STATE_NOT_FOCUSED_LOCKED: Integer read _GetCONTROL_AF_STATE_NOT_FOCUSED_LOCKED;
    {class} property CONTROL_AF_STATE_PASSIVE_FOCUSED: Integer read _GetCONTROL_AF_STATE_PASSIVE_FOCUSED;
    {class} property CONTROL_AF_STATE_PASSIVE_SCAN: Integer read _GetCONTROL_AF_STATE_PASSIVE_SCAN;
    {class} property CONTROL_AF_STATE_PASSIVE_UNFOCUSED: Integer read _GetCONTROL_AF_STATE_PASSIVE_UNFOCUSED;
    {class} property CONTROL_AF_TRIGGER_CANCEL: Integer read _GetCONTROL_AF_TRIGGER_CANCEL;
    {class} property CONTROL_AF_TRIGGER_IDLE: Integer read _GetCONTROL_AF_TRIGGER_IDLE;
    {class} property CONTROL_AF_TRIGGER_START: Integer read _GetCONTROL_AF_TRIGGER_START;
    {class} property CONTROL_AWB_MODE_AUTO: Integer read _GetCONTROL_AWB_MODE_AUTO;
    {class} property CONTROL_AWB_MODE_CLOUDY_DAYLIGHT: Integer read _GetCONTROL_AWB_MODE_CLOUDY_DAYLIGHT;
    {class} property CONTROL_AWB_MODE_DAYLIGHT: Integer read _GetCONTROL_AWB_MODE_DAYLIGHT;
    {class} property CONTROL_AWB_MODE_FLUORESCENT: Integer read _GetCONTROL_AWB_MODE_FLUORESCENT;
    {class} property CONTROL_AWB_MODE_INCANDESCENT: Integer read _GetCONTROL_AWB_MODE_INCANDESCENT;
    {class} property CONTROL_AWB_MODE_OFF: Integer read _GetCONTROL_AWB_MODE_OFF;
    {class} property CONTROL_AWB_MODE_SHADE: Integer read _GetCONTROL_AWB_MODE_SHADE;
    {class} property CONTROL_AWB_MODE_TWILIGHT: Integer read _GetCONTROL_AWB_MODE_TWILIGHT;
    {class} property CONTROL_AWB_MODE_WARM_FLUORESCENT: Integer read _GetCONTROL_AWB_MODE_WARM_FLUORESCENT;
    {class} property CONTROL_AWB_STATE_CONVERGED: Integer read _GetCONTROL_AWB_STATE_CONVERGED;
    {class} property CONTROL_AWB_STATE_INACTIVE: Integer read _GetCONTROL_AWB_STATE_INACTIVE;
    {class} property CONTROL_AWB_STATE_LOCKED: Integer read _GetCONTROL_AWB_STATE_LOCKED;
    {class} property CONTROL_AWB_STATE_SEARCHING: Integer read _GetCONTROL_AWB_STATE_SEARCHING;
    {class} property CONTROL_CAPTURE_INTENT_CUSTOM: Integer read _GetCONTROL_CAPTURE_INTENT_CUSTOM;
    {class} property CONTROL_CAPTURE_INTENT_MANUAL: Integer read _GetCONTROL_CAPTURE_INTENT_MANUAL;
    {class} property CONTROL_CAPTURE_INTENT_PREVIEW: Integer read _GetCONTROL_CAPTURE_INTENT_PREVIEW;
    {class} property CONTROL_CAPTURE_INTENT_STILL_CAPTURE: Integer read _GetCONTROL_CAPTURE_INTENT_STILL_CAPTURE;
    {class} property CONTROL_CAPTURE_INTENT_VIDEO_RECORD: Integer read _GetCONTROL_CAPTURE_INTENT_VIDEO_RECORD;
    {class} property CONTROL_CAPTURE_INTENT_VIDEO_SNAPSHOT: Integer read _GetCONTROL_CAPTURE_INTENT_VIDEO_SNAPSHOT;
    {class} property CONTROL_CAPTURE_INTENT_ZERO_SHUTTER_LAG: Integer read _GetCONTROL_CAPTURE_INTENT_ZERO_SHUTTER_LAG;
    {class} property CONTROL_EFFECT_MODE_AQUA: Integer read _GetCONTROL_EFFECT_MODE_AQUA;
    {class} property CONTROL_EFFECT_MODE_BLACKBOARD: Integer read _GetCONTROL_EFFECT_MODE_BLACKBOARD;
    {class} property CONTROL_EFFECT_MODE_MONO: Integer read _GetCONTROL_EFFECT_MODE_MONO;
    {class} property CONTROL_EFFECT_MODE_NEGATIVE: Integer read _GetCONTROL_EFFECT_MODE_NEGATIVE;
    {class} property CONTROL_EFFECT_MODE_OFF: Integer read _GetCONTROL_EFFECT_MODE_OFF;
    {class} property CONTROL_EFFECT_MODE_POSTERIZE: Integer read _GetCONTROL_EFFECT_MODE_POSTERIZE;
    {class} property CONTROL_EFFECT_MODE_SEPIA: Integer read _GetCONTROL_EFFECT_MODE_SEPIA;
    {class} property CONTROL_EFFECT_MODE_SOLARIZE: Integer read _GetCONTROL_EFFECT_MODE_SOLARIZE;
    {class} property CONTROL_EFFECT_MODE_WHITEBOARD: Integer read _GetCONTROL_EFFECT_MODE_WHITEBOARD;
    {class} property CONTROL_MODE_AUTO: Integer read _GetCONTROL_MODE_AUTO;
    {class} property CONTROL_MODE_OFF: Integer read _GetCONTROL_MODE_OFF;
    {class} property CONTROL_MODE_OFF_KEEP_STATE: Integer read _GetCONTROL_MODE_OFF_KEEP_STATE;
    {class} property CONTROL_MODE_USE_SCENE_MODE: Integer read _GetCONTROL_MODE_USE_SCENE_MODE;
    {class} property CONTROL_SCENE_MODE_ACTION: Integer read _GetCONTROL_SCENE_MODE_ACTION;
    {class} property CONTROL_SCENE_MODE_BARCODE: Integer read _GetCONTROL_SCENE_MODE_BARCODE;
    {class} property CONTROL_SCENE_MODE_BEACH: Integer read _GetCONTROL_SCENE_MODE_BEACH;
    {class} property CONTROL_SCENE_MODE_CANDLELIGHT: Integer read _GetCONTROL_SCENE_MODE_CANDLELIGHT;
    {class} property CONTROL_SCENE_MODE_DISABLED: Integer read _GetCONTROL_SCENE_MODE_DISABLED;
    {class} property CONTROL_SCENE_MODE_FACE_PRIORITY: Integer read _GetCONTROL_SCENE_MODE_FACE_PRIORITY;
    {class} property CONTROL_SCENE_MODE_FIREWORKS: Integer read _GetCONTROL_SCENE_MODE_FIREWORKS;
    {class} property CONTROL_SCENE_MODE_HDR: Integer read _GetCONTROL_SCENE_MODE_HDR;
    {class} property CONTROL_SCENE_MODE_HIGH_SPEED_VIDEO: Integer read _GetCONTROL_SCENE_MODE_HIGH_SPEED_VIDEO;
    {class} property CONTROL_SCENE_MODE_LANDSCAPE: Integer read _GetCONTROL_SCENE_MODE_LANDSCAPE;
    {class} property CONTROL_SCENE_MODE_NIGHT: Integer read _GetCONTROL_SCENE_MODE_NIGHT;
    {class} property CONTROL_SCENE_MODE_NIGHT_PORTRAIT: Integer read _GetCONTROL_SCENE_MODE_NIGHT_PORTRAIT;
    {class} property CONTROL_SCENE_MODE_PARTY: Integer read _GetCONTROL_SCENE_MODE_PARTY;
    {class} property CONTROL_SCENE_MODE_PORTRAIT: Integer read _GetCONTROL_SCENE_MODE_PORTRAIT;
    {class} property CONTROL_SCENE_MODE_SNOW: Integer read _GetCONTROL_SCENE_MODE_SNOW;
    {class} property CONTROL_SCENE_MODE_SPORTS: Integer read _GetCONTROL_SCENE_MODE_SPORTS;
    {class} property CONTROL_SCENE_MODE_STEADYPHOTO: Integer read _GetCONTROL_SCENE_MODE_STEADYPHOTO;
    {class} property CONTROL_SCENE_MODE_SUNSET: Integer read _GetCONTROL_SCENE_MODE_SUNSET;
    {class} property CONTROL_SCENE_MODE_THEATRE: Integer read _GetCONTROL_SCENE_MODE_THEATRE;
    {class} property CONTROL_VIDEO_STABILIZATION_MODE_OFF: Integer read _GetCONTROL_VIDEO_STABILIZATION_MODE_OFF;
    {class} property CONTROL_VIDEO_STABILIZATION_MODE_ON: Integer read _GetCONTROL_VIDEO_STABILIZATION_MODE_ON;
    {class} property EDGE_MODE_FAST: Integer read _GetEDGE_MODE_FAST;
    {class} property EDGE_MODE_HIGH_QUALITY: Integer read _GetEDGE_MODE_HIGH_QUALITY;
    {class} property EDGE_MODE_OFF: Integer read _GetEDGE_MODE_OFF;
    {class} property EDGE_MODE_ZERO_SHUTTER_LAG: Integer read _GetEDGE_MODE_ZERO_SHUTTER_LAG;
    {class} property FLASH_MODE_OFF: Integer read _GetFLASH_MODE_OFF;
    {class} property FLASH_MODE_SINGLE: Integer read _GetFLASH_MODE_SINGLE;
    {class} property FLASH_MODE_TORCH: Integer read _GetFLASH_MODE_TORCH;
    {class} property FLASH_STATE_CHARGING: Integer read _GetFLASH_STATE_CHARGING;
    {class} property FLASH_STATE_FIRED: Integer read _GetFLASH_STATE_FIRED;
    {class} property FLASH_STATE_PARTIAL: Integer read _GetFLASH_STATE_PARTIAL;
    {class} property FLASH_STATE_READY: Integer read _GetFLASH_STATE_READY;
    {class} property FLASH_STATE_UNAVAILABLE: Integer read _GetFLASH_STATE_UNAVAILABLE;
    {class} property HOT_PIXEL_MODE_FAST: Integer read _GetHOT_PIXEL_MODE_FAST;
    {class} property HOT_PIXEL_MODE_HIGH_QUALITY: Integer read _GetHOT_PIXEL_MODE_HIGH_QUALITY;
    {class} property HOT_PIXEL_MODE_OFF: Integer read _GetHOT_PIXEL_MODE_OFF;
    {class} property INFO_SUPPORTED_HARDWARE_LEVEL_FULL: Integer read _GetINFO_SUPPORTED_HARDWARE_LEVEL_FULL;
    {class} property INFO_SUPPORTED_HARDWARE_LEVEL_LEGACY: Integer read _GetINFO_SUPPORTED_HARDWARE_LEVEL_LEGACY;
    {class} property INFO_SUPPORTED_HARDWARE_LEVEL_LIMITED: Integer read _GetINFO_SUPPORTED_HARDWARE_LEVEL_LIMITED;
    {class} property LENS_FACING_BACK: Integer read _GetLENS_FACING_BACK;
    {class} property LENS_FACING_EXTERNAL: Integer read _GetLENS_FACING_EXTERNAL;
    {class} property LENS_FACING_FRONT: Integer read _GetLENS_FACING_FRONT;
    {class} property LENS_INFO_FOCUS_DISTANCE_CALIBRATION_APPROXIMATE: Integer read _GetLENS_INFO_FOCUS_DISTANCE_CALIBRATION_APPROXIMATE;
    {class} property LENS_INFO_FOCUS_DISTANCE_CALIBRATION_CALIBRATED: Integer read _GetLENS_INFO_FOCUS_DISTANCE_CALIBRATION_CALIBRATED;
    {class} property LENS_INFO_FOCUS_DISTANCE_CALIBRATION_UNCALIBRATED: Integer read _GetLENS_INFO_FOCUS_DISTANCE_CALIBRATION_UNCALIBRATED;
    {class} property LENS_OPTICAL_STABILIZATION_MODE_OFF: Integer read _GetLENS_OPTICAL_STABILIZATION_MODE_OFF;
    {class} property LENS_OPTICAL_STABILIZATION_MODE_ON: Integer read _GetLENS_OPTICAL_STABILIZATION_MODE_ON;
    {class} property LENS_STATE_MOVING: Integer read _GetLENS_STATE_MOVING;
    {class} property LENS_STATE_STATIONARY: Integer read _GetLENS_STATE_STATIONARY;
    {class} property NOISE_REDUCTION_MODE_FAST: Integer read _GetNOISE_REDUCTION_MODE_FAST;
    {class} property NOISE_REDUCTION_MODE_HIGH_QUALITY: Integer read _GetNOISE_REDUCTION_MODE_HIGH_QUALITY;
    {class} property NOISE_REDUCTION_MODE_MINIMAL: Integer read _GetNOISE_REDUCTION_MODE_MINIMAL;
    {class} property NOISE_REDUCTION_MODE_OFF: Integer read _GetNOISE_REDUCTION_MODE_OFF;
    {class} property NOISE_REDUCTION_MODE_ZERO_SHUTTER_LAG: Integer read _GetNOISE_REDUCTION_MODE_ZERO_SHUTTER_LAG;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_BACKWARD_COMPATIBLE: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_BACKWARD_COMPATIBLE;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_BURST_CAPTURE: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_BURST_CAPTURE;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_CONSTRAINED_HIGH_SPEED_VIDEO: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_CONSTRAINED_HIGH_SPEED_VIDEO;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_DEPTH_OUTPUT: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_DEPTH_OUTPUT;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_MANUAL_POST_PROCESSING: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_MANUAL_POST_PROCESSING;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_MANUAL_SENSOR: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_MANUAL_SENSOR;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_PRIVATE_REPROCESSING: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_PRIVATE_REPROCESSING;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_RAW: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_RAW;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_READ_SENSOR_SETTINGS: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_READ_SENSOR_SETTINGS;
    {class} property REQUEST_AVAILABLE_CAPABILITIES_YUV_REPROCESSING: Integer read _GetREQUEST_AVAILABLE_CAPABILITIES_YUV_REPROCESSING;
    {class} property SCALER_CROPPING_TYPE_CENTER_ONLY: Integer read _GetSCALER_CROPPING_TYPE_CENTER_ONLY;
    {class} property SCALER_CROPPING_TYPE_FREEFORM: Integer read _GetSCALER_CROPPING_TYPE_FREEFORM;
    {class} property SENSOR_INFO_COLOR_FILTER_ARRANGEMENT_BGGR: Integer read _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_BGGR;
    {class} property SENSOR_INFO_COLOR_FILTER_ARRANGEMENT_GBRG: Integer read _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_GBRG;
    {class} property SENSOR_INFO_COLOR_FILTER_ARRANGEMENT_GRBG: Integer read _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_GRBG;
    {class} property SENSOR_INFO_COLOR_FILTER_ARRANGEMENT_RGB: Integer read _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_RGB;
    {class} property SENSOR_INFO_COLOR_FILTER_ARRANGEMENT_RGGB: Integer read _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT_RGGB;
    {class} property SENSOR_INFO_TIMESTAMP_SOURCE_REALTIME: Integer read _GetSENSOR_INFO_TIMESTAMP_SOURCE_REALTIME;
    {class} property SENSOR_INFO_TIMESTAMP_SOURCE_UNKNOWN: Integer read _GetSENSOR_INFO_TIMESTAMP_SOURCE_UNKNOWN;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_CLOUDY_WEATHER: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_CLOUDY_WEATHER;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_COOL_WHITE_FLUORESCENT: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_COOL_WHITE_FLUORESCENT;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_D50: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_D50;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_D55: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_D55;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_D65: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_D65;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_D75: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_D75;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_DAYLIGHT: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_DAYLIGHT;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_DAYLIGHT_FLUORESCENT: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_DAYLIGHT_FLUORESCENT;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_DAY_WHITE_FLUORESCENT: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_DAY_WHITE_FLUORESCENT;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_FINE_WEATHER: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_FINE_WEATHER;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_FLASH: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_FLASH;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_FLUORESCENT: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_FLUORESCENT;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_ISO_STUDIO_TUNGSTEN: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_ISO_STUDIO_TUNGSTEN;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_SHADE: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_SHADE;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_STANDARD_A: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_STANDARD_A;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_STANDARD_B: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_STANDARD_B;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_STANDARD_C: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_STANDARD_C;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_TUNGSTEN: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_TUNGSTEN;
    {class} property SENSOR_REFERENCE_ILLUMINANT1_WHITE_FLUORESCENT: Integer read _GetSENSOR_REFERENCE_ILLUMINANT1_WHITE_FLUORESCENT;
    {class} property SENSOR_TEST_PATTERN_MODE_COLOR_BARS: Integer read _GetSENSOR_TEST_PATTERN_MODE_COLOR_BARS;
    {class} property SENSOR_TEST_PATTERN_MODE_COLOR_BARS_FADE_TO_GRAY: Integer read _GetSENSOR_TEST_PATTERN_MODE_COLOR_BARS_FADE_TO_GRAY;
    {class} property SENSOR_TEST_PATTERN_MODE_CUSTOM1: Integer read _GetSENSOR_TEST_PATTERN_MODE_CUSTOM1;
    {class} property SENSOR_TEST_PATTERN_MODE_OFF: Integer read _GetSENSOR_TEST_PATTERN_MODE_OFF;
    {class} property SENSOR_TEST_PATTERN_MODE_PN9: Integer read _GetSENSOR_TEST_PATTERN_MODE_PN9;
    {class} property SENSOR_TEST_PATTERN_MODE_SOLID_COLOR: Integer read _GetSENSOR_TEST_PATTERN_MODE_SOLID_COLOR;
    {class} property SHADING_MODE_FAST: Integer read _GetSHADING_MODE_FAST;
    {class} property SHADING_MODE_HIGH_QUALITY: Integer read _GetSHADING_MODE_HIGH_QUALITY;
    {class} property SHADING_MODE_OFF: Integer read _GetSHADING_MODE_OFF;
    {class} property STATISTICS_FACE_DETECT_MODE_FULL: Integer read _GetSTATISTICS_FACE_DETECT_MODE_FULL;
    {class} property STATISTICS_FACE_DETECT_MODE_OFF: Integer read _GetSTATISTICS_FACE_DETECT_MODE_OFF;
    {class} property STATISTICS_FACE_DETECT_MODE_SIMPLE: Integer read _GetSTATISTICS_FACE_DETECT_MODE_SIMPLE;
    {class} property STATISTICS_LENS_SHADING_MAP_MODE_OFF: Integer read _GetSTATISTICS_LENS_SHADING_MAP_MODE_OFF;
    {class} property STATISTICS_LENS_SHADING_MAP_MODE_ON: Integer read _GetSTATISTICS_LENS_SHADING_MAP_MODE_ON;
    {class} property STATISTICS_SCENE_FLICKER_50HZ: Integer read _GetSTATISTICS_SCENE_FLICKER_50HZ;
    {class} property STATISTICS_SCENE_FLICKER_60HZ: Integer read _GetSTATISTICS_SCENE_FLICKER_60HZ;
    {class} property STATISTICS_SCENE_FLICKER_NONE: Integer read _GetSTATISTICS_SCENE_FLICKER_NONE;
    {class} property SYNC_MAX_LATENCY_PER_FRAME_CONTROL: Integer read _GetSYNC_MAX_LATENCY_PER_FRAME_CONTROL;
    {class} property SYNC_MAX_LATENCY_UNKNOWN: Integer read _GetSYNC_MAX_LATENCY_UNKNOWN;
    {class} property TONEMAP_MODE_CONTRAST_CURVE: Integer read _GetTONEMAP_MODE_CONTRAST_CURVE;
    {class} property TONEMAP_MODE_FAST: Integer read _GetTONEMAP_MODE_FAST;
    {class} property TONEMAP_MODE_GAMMA_VALUE: Integer read _GetTONEMAP_MODE_GAMMA_VALUE;
    {class} property TONEMAP_MODE_HIGH_QUALITY: Integer read _GetTONEMAP_MODE_HIGH_QUALITY;
    {class} property TONEMAP_MODE_PRESET_CURVE: Integer read _GetTONEMAP_MODE_PRESET_CURVE;
    {class} property TONEMAP_PRESET_CURVE_REC709: Integer read _GetTONEMAP_PRESET_CURVE_REC709;
    {class} property TONEMAP_PRESET_CURVE_SRGB: Integer read _GetTONEMAP_PRESET_CURVE_SRGB;
  end;

  [JavaSignature('android/hardware/camera2/CameraMetadata')]
  JCameraMetadata = interface(JObject)
    ['{BF7374BF-826F-4549-9633-98323E0D680E}']
  end;
  TJCameraMetadata = class(TJavaGenericImport<JCameraMetadataClass, JCameraMetadata>) end;

  JCameraCharacteristicsClass = interface(JCameraMetadataClass)
    ['{0B3B7191-A80A-47A5-84E3-8FAC877D7C41}']
    {class} function _GetCOLOR_CORRECTION_AVAILABLE_ABERRATION_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AE_AVAILABLE_ANTIBANDING_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AE_AVAILABLE_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AE_AVAILABLE_TARGET_FPS_RANGES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AE_COMPENSATION_RANGE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AE_COMPENSATION_STEP: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AE_LOCK_AVAILABLE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AF_AVAILABLE_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AVAILABLE_EFFECTS: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AVAILABLE_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AVAILABLE_SCENE_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AVAILABLE_VIDEO_STABILIZATION_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AWB_AVAILABLE_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_AWB_LOCK_AVAILABLE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_MAX_REGIONS_AE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_MAX_REGIONS_AF: JCameraCharacteristics_Key; cdecl;
    {class} function _GetCONTROL_MAX_REGIONS_AWB: JCameraCharacteristics_Key; cdecl;
    {class} function _GetDEPTH_DEPTH_IS_EXCLUSIVE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetEDGE_AVAILABLE_EDGE_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetFLASH_INFO_AVAILABLE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetHOT_PIXEL_AVAILABLE_HOT_PIXEL_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetINFO_SUPPORTED_HARDWARE_LEVEL: JCameraCharacteristics_Key; cdecl;
    {class} function _GetJPEG_AVAILABLE_THUMBNAIL_SIZES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_FACING: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_INFO_AVAILABLE_APERTURES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_INFO_AVAILABLE_FILTER_DENSITIES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_INFO_AVAILABLE_FOCAL_LENGTHS: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_INFO_AVAILABLE_OPTICAL_STABILIZATION: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_INFO_FOCUS_DISTANCE_CALIBRATION: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_INFO_HYPERFOCAL_DISTANCE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_INFO_MINIMUM_FOCUS_DISTANCE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_INTRINSIC_CALIBRATION: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_POSE_ROTATION: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_POSE_TRANSLATION: JCameraCharacteristics_Key; cdecl;
    {class} function _GetLENS_RADIAL_DISTORTION: JCameraCharacteristics_Key; cdecl;
    {class} function _GetNOISE_REDUCTION_AVAILABLE_NOISE_REDUCTION_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetREPROCESS_MAX_CAPTURE_STALL: JCameraCharacteristics_Key; cdecl;
    {class} function _GetREQUEST_AVAILABLE_CAPABILITIES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetREQUEST_MAX_NUM_INPUT_STREAMS: JCameraCharacteristics_Key; cdecl;
    {class} function _GetREQUEST_MAX_NUM_OUTPUT_PROC: JCameraCharacteristics_Key; cdecl;
    {class} function _GetREQUEST_MAX_NUM_OUTPUT_PROC_STALLING: JCameraCharacteristics_Key; cdecl;
    {class} function _GetREQUEST_MAX_NUM_OUTPUT_RAW: JCameraCharacteristics_Key; cdecl;
    {class} function _GetREQUEST_PARTIAL_RESULT_COUNT: JCameraCharacteristics_Key; cdecl;
    {class} function _GetREQUEST_PIPELINE_MAX_DEPTH: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSCALER_AVAILABLE_MAX_DIGITAL_ZOOM: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSCALER_CROPPING_TYPE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSCALER_STREAM_CONFIGURATION_MAP: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_AVAILABLE_TEST_PATTERN_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_BLACK_LEVEL_PATTERN: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_CALIBRATION_TRANSFORM1: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_CALIBRATION_TRANSFORM2: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_COLOR_TRANSFORM1: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_COLOR_TRANSFORM2: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_FORWARD_MATRIX1: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_FORWARD_MATRIX2: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_ACTIVE_ARRAY_SIZE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_EXPOSURE_TIME_RANGE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_LENS_SHADING_APPLIED: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_MAX_FRAME_DURATION: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_PHYSICAL_SIZE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_PIXEL_ARRAY_SIZE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_PRE_CORRECTION_ACTIVE_ARRAY_SIZE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_SENSITIVITY_RANGE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_TIMESTAMP_SOURCE: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_INFO_WHITE_LEVEL: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_MAX_ANALOG_SENSITIVITY: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_ORIENTATION: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT1: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSENSOR_REFERENCE_ILLUMINANT2: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSHADING_AVAILABLE_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSTATISTICS_INFO_AVAILABLE_FACE_DETECT_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSTATISTICS_INFO_AVAILABLE_HOT_PIXEL_MAP_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSTATISTICS_INFO_AVAILABLE_LENS_SHADING_MAP_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSTATISTICS_INFO_MAX_FACE_COUNT: JCameraCharacteristics_Key; cdecl;
    {class} function _GetSYNC_MAX_LATENCY: JCameraCharacteristics_Key; cdecl;
    {class} function _GetTONEMAP_AVAILABLE_TONE_MAP_MODES: JCameraCharacteristics_Key; cdecl;
    {class} function _GetTONEMAP_MAX_CURVE_POINTS: JCameraCharacteristics_Key; cdecl;
    {class} property COLOR_CORRECTION_AVAILABLE_ABERRATION_MODES: JCameraCharacteristics_Key read _GetCOLOR_CORRECTION_AVAILABLE_ABERRATION_MODES;
    {class} property CONTROL_AE_AVAILABLE_ANTIBANDING_MODES: JCameraCharacteristics_Key read _GetCONTROL_AE_AVAILABLE_ANTIBANDING_MODES;
    {class} property CONTROL_AE_AVAILABLE_MODES: JCameraCharacteristics_Key read _GetCONTROL_AE_AVAILABLE_MODES;
    {class} property CONTROL_AE_AVAILABLE_TARGET_FPS_RANGES: JCameraCharacteristics_Key read _GetCONTROL_AE_AVAILABLE_TARGET_FPS_RANGES;
    {class} property CONTROL_AE_COMPENSATION_RANGE: JCameraCharacteristics_Key read _GetCONTROL_AE_COMPENSATION_RANGE;
    {class} property CONTROL_AE_COMPENSATION_STEP: JCameraCharacteristics_Key read _GetCONTROL_AE_COMPENSATION_STEP;
    {class} property CONTROL_AE_LOCK_AVAILABLE: JCameraCharacteristics_Key read _GetCONTROL_AE_LOCK_AVAILABLE;
    {class} property CONTROL_AF_AVAILABLE_MODES: JCameraCharacteristics_Key read _GetCONTROL_AF_AVAILABLE_MODES;
    {class} property CONTROL_AVAILABLE_EFFECTS: JCameraCharacteristics_Key read _GetCONTROL_AVAILABLE_EFFECTS;
    {class} property CONTROL_AVAILABLE_MODES: JCameraCharacteristics_Key read _GetCONTROL_AVAILABLE_MODES;
    {class} property CONTROL_AVAILABLE_SCENE_MODES: JCameraCharacteristics_Key read _GetCONTROL_AVAILABLE_SCENE_MODES;
    {class} property CONTROL_AVAILABLE_VIDEO_STABILIZATION_MODES: JCameraCharacteristics_Key read _GetCONTROL_AVAILABLE_VIDEO_STABILIZATION_MODES;
    {class} property CONTROL_AWB_AVAILABLE_MODES: JCameraCharacteristics_Key read _GetCONTROL_AWB_AVAILABLE_MODES;
    {class} property CONTROL_AWB_LOCK_AVAILABLE: JCameraCharacteristics_Key read _GetCONTROL_AWB_LOCK_AVAILABLE;
    {class} property CONTROL_MAX_REGIONS_AE: JCameraCharacteristics_Key read _GetCONTROL_MAX_REGIONS_AE;
    {class} property CONTROL_MAX_REGIONS_AF: JCameraCharacteristics_Key read _GetCONTROL_MAX_REGIONS_AF;
    {class} property CONTROL_MAX_REGIONS_AWB: JCameraCharacteristics_Key read _GetCONTROL_MAX_REGIONS_AWB;
    {class} property DEPTH_DEPTH_IS_EXCLUSIVE: JCameraCharacteristics_Key read _GetDEPTH_DEPTH_IS_EXCLUSIVE;
    {class} property EDGE_AVAILABLE_EDGE_MODES: JCameraCharacteristics_Key read _GetEDGE_AVAILABLE_EDGE_MODES;
    {class} property FLASH_INFO_AVAILABLE: JCameraCharacteristics_Key read _GetFLASH_INFO_AVAILABLE;
    {class} property HOT_PIXEL_AVAILABLE_HOT_PIXEL_MODES: JCameraCharacteristics_Key read _GetHOT_PIXEL_AVAILABLE_HOT_PIXEL_MODES;
    {class} property INFO_SUPPORTED_HARDWARE_LEVEL: JCameraCharacteristics_Key read _GetINFO_SUPPORTED_HARDWARE_LEVEL;
    {class} property JPEG_AVAILABLE_THUMBNAIL_SIZES: JCameraCharacteristics_Key read _GetJPEG_AVAILABLE_THUMBNAIL_SIZES;
    {class} property LENS_FACING: JCameraCharacteristics_Key read _GetLENS_FACING;
    {class} property LENS_INFO_AVAILABLE_APERTURES: JCameraCharacteristics_Key read _GetLENS_INFO_AVAILABLE_APERTURES;
    {class} property LENS_INFO_AVAILABLE_FILTER_DENSITIES: JCameraCharacteristics_Key read _GetLENS_INFO_AVAILABLE_FILTER_DENSITIES;
    {class} property LENS_INFO_AVAILABLE_FOCAL_LENGTHS: JCameraCharacteristics_Key read _GetLENS_INFO_AVAILABLE_FOCAL_LENGTHS;
    {class} property LENS_INFO_AVAILABLE_OPTICAL_STABILIZATION: JCameraCharacteristics_Key read _GetLENS_INFO_AVAILABLE_OPTICAL_STABILIZATION;
    {class} property LENS_INFO_FOCUS_DISTANCE_CALIBRATION: JCameraCharacteristics_Key read _GetLENS_INFO_FOCUS_DISTANCE_CALIBRATION;
    {class} property LENS_INFO_HYPERFOCAL_DISTANCE: JCameraCharacteristics_Key read _GetLENS_INFO_HYPERFOCAL_DISTANCE;
    {class} property LENS_INFO_MINIMUM_FOCUS_DISTANCE: JCameraCharacteristics_Key read _GetLENS_INFO_MINIMUM_FOCUS_DISTANCE;
    {class} property LENS_INTRINSIC_CALIBRATION: JCameraCharacteristics_Key read _GetLENS_INTRINSIC_CALIBRATION;
    {class} property LENS_POSE_ROTATION: JCameraCharacteristics_Key read _GetLENS_POSE_ROTATION;
    {class} property LENS_POSE_TRANSLATION: JCameraCharacteristics_Key read _GetLENS_POSE_TRANSLATION;
    {class} property LENS_RADIAL_DISTORTION: JCameraCharacteristics_Key read _GetLENS_RADIAL_DISTORTION;
    {class} property NOISE_REDUCTION_AVAILABLE_NOISE_REDUCTION_MODES: JCameraCharacteristics_Key read _GetNOISE_REDUCTION_AVAILABLE_NOISE_REDUCTION_MODES;
    {class} property REPROCESS_MAX_CAPTURE_STALL: JCameraCharacteristics_Key read _GetREPROCESS_MAX_CAPTURE_STALL;
    {class} property REQUEST_AVAILABLE_CAPABILITIES: JCameraCharacteristics_Key read _GetREQUEST_AVAILABLE_CAPABILITIES;
    {class} property REQUEST_MAX_NUM_INPUT_STREAMS: JCameraCharacteristics_Key read _GetREQUEST_MAX_NUM_INPUT_STREAMS;
    {class} property REQUEST_MAX_NUM_OUTPUT_PROC: JCameraCharacteristics_Key read _GetREQUEST_MAX_NUM_OUTPUT_PROC;
    {class} property REQUEST_MAX_NUM_OUTPUT_PROC_STALLING: JCameraCharacteristics_Key read _GetREQUEST_MAX_NUM_OUTPUT_PROC_STALLING;
    {class} property REQUEST_MAX_NUM_OUTPUT_RAW: JCameraCharacteristics_Key read _GetREQUEST_MAX_NUM_OUTPUT_RAW;
    {class} property REQUEST_PARTIAL_RESULT_COUNT: JCameraCharacteristics_Key read _GetREQUEST_PARTIAL_RESULT_COUNT;
    {class} property REQUEST_PIPELINE_MAX_DEPTH: JCameraCharacteristics_Key read _GetREQUEST_PIPELINE_MAX_DEPTH;
    {class} property SCALER_AVAILABLE_MAX_DIGITAL_ZOOM: JCameraCharacteristics_Key read _GetSCALER_AVAILABLE_MAX_DIGITAL_ZOOM;
    {class} property SCALER_CROPPING_TYPE: JCameraCharacteristics_Key read _GetSCALER_CROPPING_TYPE;
    {class} property SCALER_STREAM_CONFIGURATION_MAP: JCameraCharacteristics_Key read _GetSCALER_STREAM_CONFIGURATION_MAP;
    {class} property SENSOR_AVAILABLE_TEST_PATTERN_MODES: JCameraCharacteristics_Key read _GetSENSOR_AVAILABLE_TEST_PATTERN_MODES;
    {class} property SENSOR_BLACK_LEVEL_PATTERN: JCameraCharacteristics_Key read _GetSENSOR_BLACK_LEVEL_PATTERN;
    {class} property SENSOR_CALIBRATION_TRANSFORM1: JCameraCharacteristics_Key read _GetSENSOR_CALIBRATION_TRANSFORM1;
    {class} property SENSOR_CALIBRATION_TRANSFORM2: JCameraCharacteristics_Key read _GetSENSOR_CALIBRATION_TRANSFORM2;
    {class} property SENSOR_COLOR_TRANSFORM1: JCameraCharacteristics_Key read _GetSENSOR_COLOR_TRANSFORM1;
    {class} property SENSOR_COLOR_TRANSFORM2: JCameraCharacteristics_Key read _GetSENSOR_COLOR_TRANSFORM2;
    {class} property SENSOR_FORWARD_MATRIX1: JCameraCharacteristics_Key read _GetSENSOR_FORWARD_MATRIX1;
    {class} property SENSOR_FORWARD_MATRIX2: JCameraCharacteristics_Key read _GetSENSOR_FORWARD_MATRIX2;
    {class} property SENSOR_INFO_ACTIVE_ARRAY_SIZE: JCameraCharacteristics_Key read _GetSENSOR_INFO_ACTIVE_ARRAY_SIZE;
    {class} property SENSOR_INFO_COLOR_FILTER_ARRANGEMENT: JCameraCharacteristics_Key read _GetSENSOR_INFO_COLOR_FILTER_ARRANGEMENT;
    {class} property SENSOR_INFO_EXPOSURE_TIME_RANGE: JCameraCharacteristics_Key read _GetSENSOR_INFO_EXPOSURE_TIME_RANGE;
    {class} property SENSOR_INFO_LENS_SHADING_APPLIED: JCameraCharacteristics_Key read _GetSENSOR_INFO_LENS_SHADING_APPLIED;
    {class} property SENSOR_INFO_MAX_FRAME_DURATION: JCameraCharacteristics_Key read _GetSENSOR_INFO_MAX_FRAME_DURATION;
    {class} property SENSOR_INFO_PHYSICAL_SIZE: JCameraCharacteristics_Key read _GetSENSOR_INFO_PHYSICAL_SIZE;
    {class} property SENSOR_INFO_PIXEL_ARRAY_SIZE: JCameraCharacteristics_Key read _GetSENSOR_INFO_PIXEL_ARRAY_SIZE;
    {class} property SENSOR_INFO_PRE_CORRECTION_ACTIVE_ARRAY_SIZE: JCameraCharacteristics_Key read _GetSENSOR_INFO_PRE_CORRECTION_ACTIVE_ARRAY_SIZE;
    {class} property SENSOR_INFO_SENSITIVITY_RANGE: JCameraCharacteristics_Key read _GetSENSOR_INFO_SENSITIVITY_RANGE;
    {class} property SENSOR_INFO_TIMESTAMP_SOURCE: JCameraCharacteristics_Key read _GetSENSOR_INFO_TIMESTAMP_SOURCE;
    {class} property SENSOR_INFO_WHITE_LEVEL: JCameraCharacteristics_Key read _GetSENSOR_INFO_WHITE_LEVEL;
    {class} property SENSOR_MAX_ANALOG_SENSITIVITY: JCameraCharacteristics_Key read _GetSENSOR_MAX_ANALOG_SENSITIVITY;
    {class} property SENSOR_ORIENTATION: JCameraCharacteristics_Key read _GetSENSOR_ORIENTATION;
    {class} property SENSOR_REFERENCE_ILLUMINANT1: JCameraCharacteristics_Key read _GetSENSOR_REFERENCE_ILLUMINANT1;
    {class} property SENSOR_REFERENCE_ILLUMINANT2: JCameraCharacteristics_Key read _GetSENSOR_REFERENCE_ILLUMINANT2;
    {class} property SHADING_AVAILABLE_MODES: JCameraCharacteristics_Key read _GetSHADING_AVAILABLE_MODES;
    {class} property STATISTICS_INFO_AVAILABLE_FACE_DETECT_MODES: JCameraCharacteristics_Key read _GetSTATISTICS_INFO_AVAILABLE_FACE_DETECT_MODES;
    {class} property STATISTICS_INFO_AVAILABLE_HOT_PIXEL_MAP_MODES: JCameraCharacteristics_Key read _GetSTATISTICS_INFO_AVAILABLE_HOT_PIXEL_MAP_MODES;
    {class} property STATISTICS_INFO_AVAILABLE_LENS_SHADING_MAP_MODES: JCameraCharacteristics_Key read _GetSTATISTICS_INFO_AVAILABLE_LENS_SHADING_MAP_MODES;
    {class} property STATISTICS_INFO_MAX_FACE_COUNT: JCameraCharacteristics_Key read _GetSTATISTICS_INFO_MAX_FACE_COUNT;
    {class} property SYNC_MAX_LATENCY: JCameraCharacteristics_Key read _GetSYNC_MAX_LATENCY;
    {class} property TONEMAP_AVAILABLE_TONE_MAP_MODES: JCameraCharacteristics_Key read _GetTONEMAP_AVAILABLE_TONE_MAP_MODES;
    {class} property TONEMAP_MAX_CURVE_POINTS: JCameraCharacteristics_Key read _GetTONEMAP_MAX_CURVE_POINTS;
  end;

  [JavaSignature('android/hardware/camera2/CameraCharacteristics')]
  JCameraCharacteristics = interface(JCameraMetadata)
    ['{A5378142-5841-4FD1-9808-861EC6EC4D88}']
    function get(key: JCameraCharacteristics_Key): JObject; cdecl;
    function getAvailableCaptureRequestKeys: JList; cdecl;
    function getAvailableCaptureResultKeys: JList; cdecl;
    function getKeys: JList; cdecl;
  end;
  TJCameraCharacteristics = class(TJavaGenericImport<JCameraCharacteristicsClass, JCameraCharacteristics>) end;

  JCameraCharacteristics_KeyClass = interface(JObjectClass)
    ['{DBE0A343-F4E7-4965-A051-47C968ED5E27}']
    {class} function toString: JString; cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CameraCharacteristics$Key')]
  JCameraCharacteristics_Key = interface(JObject)
    ['{875B838B-C146-4ECE-ABFD-A39B92933488}']
    function equals(o: JObject): Boolean; cdecl;
    function getName: JString; cdecl;
    function hashCode: Integer; cdecl;
  end;
  TJCameraCharacteristics_Key = class(TJavaGenericImport<JCameraCharacteristics_KeyClass, JCameraCharacteristics_Key>) end;

  JCameraConstrainedHighSpeedCaptureSessionClass = interface(JCameraCaptureSessionClass)
    ['{D0C81A1D-C928-49C1-A71B-F1EAB1B1CB35}']
    {class} function init: JCameraConstrainedHighSpeedCaptureSession; cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CameraConstrainedHighSpeedCaptureSession')]
  JCameraConstrainedHighSpeedCaptureSession = interface(JCameraCaptureSession)
    ['{F4547CBE-0CB1-4AB2-AB76-7688EFBCFC58}']
    function createHighSpeedRequestList(request: JCaptureRequest): JList; cdecl;
  end;
  TJCameraConstrainedHighSpeedCaptureSession = class(TJavaGenericImport<JCameraConstrainedHighSpeedCaptureSessionClass, JCameraConstrainedHighSpeedCaptureSession>) end;

  JCameraDeviceClass = interface(JObjectClass)
    ['{CBCACCCF-2350-4394-A6D0-59EC8AC7C029}']
    {class} function _GetTEMPLATE_MANUAL: Integer; cdecl;
    {class} function _GetTEMPLATE_PREVIEW: Integer; cdecl;
    {class} function _GetTEMPLATE_RECORD: Integer; cdecl;
    {class} function _GetTEMPLATE_STILL_CAPTURE: Integer; cdecl;
    {class} function _GetTEMPLATE_VIDEO_SNAPSHOT: Integer; cdecl;
    {class} function _GetTEMPLATE_ZERO_SHUTTER_LAG: Integer; cdecl;
    {class} property TEMPLATE_MANUAL: Integer read _GetTEMPLATE_MANUAL;
    {class} property TEMPLATE_PREVIEW: Integer read _GetTEMPLATE_PREVIEW;
    {class} property TEMPLATE_RECORD: Integer read _GetTEMPLATE_RECORD;
    {class} property TEMPLATE_STILL_CAPTURE: Integer read _GetTEMPLATE_STILL_CAPTURE;
    {class} property TEMPLATE_VIDEO_SNAPSHOT: Integer read _GetTEMPLATE_VIDEO_SNAPSHOT;
    {class} property TEMPLATE_ZERO_SHUTTER_LAG: Integer read _GetTEMPLATE_ZERO_SHUTTER_LAG;
  end;

  [JavaSignature('android/hardware/camera2/CameraDevice')]
  JCameraDevice = interface(JObject)
    ['{55E0BBAE-4805-4CCE-8DC3-A5C0D7FE0BA2}']
    procedure close; cdecl;
    function createCaptureRequest(templateType: Integer): JCaptureRequest_Builder; cdecl;
    procedure createCaptureSession(outputs: JList; callback: JCameraCaptureSession_StateCallback; handler: JHandler); cdecl;
    procedure createConstrainedHighSpeedCaptureSession(outputs: JList; callback: JCameraCaptureSession_StateCallback; handler: JHandler); cdecl;
    function createReprocessCaptureRequest(inputResult: JTotalCaptureResult): JCaptureRequest_Builder; cdecl;
    procedure createReprocessableCaptureSession(inputConfig: JInputConfiguration; outputs: JList; callback: JCameraCaptureSession_StateCallback; handler: JHandler); cdecl;
    function getId: JString; cdecl;
  end;
  TJCameraDevice = class(TJavaGenericImport<JCameraDeviceClass, JCameraDevice>) end;

  JCameraDevice_StateCallbackClass = interface(JObjectClass)
    ['{3F5A7394-FD15-439C-9BFB-DF8D43F9F930}']
    {class} function _GetERROR_CAMERA_DEVICE: Integer; cdecl;
    {class} function _GetERROR_CAMERA_DISABLED: Integer; cdecl;
    {class} function _GetERROR_CAMERA_IN_USE: Integer; cdecl;
    {class} function _GetERROR_CAMERA_SERVICE: Integer; cdecl;
    {class} function _GetERROR_MAX_CAMERAS_IN_USE: Integer; cdecl;
    {class} function init: JCameraDevice_StateCallback; cdecl;
    {class} procedure onError(camera: JCameraDevice; error: Integer); cdecl;
    {class} procedure onOpened(camera: JCameraDevice); cdecl;
    {class} property ERROR_CAMERA_DEVICE: Integer read _GetERROR_CAMERA_DEVICE;
    {class} property ERROR_CAMERA_DISABLED: Integer read _GetERROR_CAMERA_DISABLED;
    {class} property ERROR_CAMERA_IN_USE: Integer read _GetERROR_CAMERA_IN_USE;
    {class} property ERROR_CAMERA_SERVICE: Integer read _GetERROR_CAMERA_SERVICE;
    {class} property ERROR_MAX_CAMERAS_IN_USE: Integer read _GetERROR_MAX_CAMERAS_IN_USE;
  end;

  [JavaSignature('android/hardware/camera2/CameraDevice$StateCallback')]
  JCameraDevice_StateCallback = interface(JObject)
    ['{3A3944F5-A71F-4CD6-98C6-04B8D65C3B52}']
    procedure onClosed(camera: JCameraDevice); cdecl;
    procedure onDisconnected(camera: JCameraDevice); cdecl;
  end;
  TJCameraDevice_StateCallback = class(TJavaGenericImport<JCameraDevice_StateCallbackClass, JCameraDevice_StateCallback>) end;

  JCameraManagerClass = interface(JObjectClass)
    ['{9B3685E8-7FF7-4BEA-B7EB-C7765CD865F8}']
  end;

  [JavaSignature('android/hardware/camera2/CameraManager')]
  JCameraManager = interface(JObject)
    ['{79AE6849-D700-430F-930C-58731FCAAEEB}']
    function getCameraCharacteristics(cameraId: JString): JCameraCharacteristics; cdecl;
    function getCameraIdList: TJavaObjectArray<JString>; cdecl;
    procedure setTorchMode(cameraId: JString; enabled: Boolean); cdecl;
    procedure unregisterAvailabilityCallback(callback: JCameraManager_AvailabilityCallback); cdecl;
    procedure unregisterTorchCallback(callback: JCameraManager_TorchCallback); cdecl;
    procedure openCamera(cameraId: JString; callback: JCameraDevice_StateCallback; handler: JHandler); cdecl;
    procedure registerAvailabilityCallback(callback: JCameraManager_AvailabilityCallback; handler: JHandler); cdecl;
    procedure registerTorchCallback(callback: JCameraManager_TorchCallback; handler: JHandler); cdecl;
  end;
  TJCameraManager = class(TJavaGenericImport<JCameraManagerClass, JCameraManager>) end;

  JCameraManager_AvailabilityCallbackClass = interface(JObjectClass)
    ['{DE180B62-3411-4A21-861D-BF5C487BF9B1}']
    {class} function init: JCameraManager_AvailabilityCallback; cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CameraManager$AvailabilityCallback')]
  JCameraManager_AvailabilityCallback = interface(JObject)
    ['{9C202B78-3DB4-4A9A-B064-1E34930D2E9D}']
    procedure onCameraAvailable(cameraId: JString); cdecl;
    procedure onCameraUnavailable(cameraId: JString); cdecl;
  end;
  TJCameraManager_AvailabilityCallback = class(TJavaGenericImport<JCameraManager_AvailabilityCallbackClass, JCameraManager_AvailabilityCallback>) end;

  JCameraManager_TorchCallbackClass = interface(JObjectClass)
    ['{06A923D6-2657-4D96-9777-384D881C8A04}']
    {class} function init: JCameraManager_TorchCallback; cdecl;
    {class} procedure onTorchModeChanged(cameraId: JString; enabled: Boolean); cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CameraManager$TorchCallback')]
  JCameraManager_TorchCallback = interface(JObject)
    ['{84B1B2B1-897A-486A-AB22-4465F130CCAA}']
    procedure onTorchModeUnavailable(cameraId: JString); cdecl;
  end;
  TJCameraManager_TorchCallback = class(TJavaGenericImport<JCameraManager_TorchCallbackClass, JCameraManager_TorchCallback>) end;

  JCaptureFailureClass = interface(JObjectClass)
    ['{B0E2E99F-47E7-4EB8-90DD-59EA2CA3A775}']
    {class} function _GetREASON_ERROR: Integer; cdecl;
    {class} function _GetREASON_FLUSHED: Integer; cdecl;
    {class} function getRequest: JCaptureRequest; cdecl;
    {class} function getSequenceId: Integer; cdecl;
    {class} property REASON_ERROR: Integer read _GetREASON_ERROR;
    {class} property REASON_FLUSHED: Integer read _GetREASON_FLUSHED;
  end;

  [JavaSignature('android/hardware/camera2/CaptureFailure')]
  JCaptureFailure = interface(JObject)
    ['{BF8BFA4D-8FA9-4B31-8CA6-0C94E02C0FDA}']
    function getFrameNumber: Int64; cdecl;
    function getReason: Integer; cdecl;
    function wasImageCaptured: Boolean; cdecl;
  end;
  TJCaptureFailure = class(TJavaGenericImport<JCaptureFailureClass, JCaptureFailure>) end;

  JCaptureRequestClass = interface(JCameraMetadataClass)
    ['{C7327514-600F-4721-A69A-828937A4D87F}']
    {class} function _GetBLACK_LEVEL_LOCK: JCaptureRequest_Key; cdecl;
    {class} function _GetCOLOR_CORRECTION_ABERRATION_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCOLOR_CORRECTION_GAINS: JCaptureRequest_Key; cdecl;
    {class} function _GetCOLOR_CORRECTION_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCOLOR_CORRECTION_TRANSFORM: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AE_ANTIBANDING_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AE_EXPOSURE_COMPENSATION: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AE_LOCK: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AE_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AE_PRECAPTURE_TRIGGER: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AE_REGIONS: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AE_TARGET_FPS_RANGE: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AF_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AF_REGIONS: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AF_TRIGGER: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AWB_LOCK: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AWB_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_AWB_REGIONS: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_SCENE_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCONTROL_VIDEO_STABILIZATION_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetEDGE_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetFLASH_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetHOT_PIXEL_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetJPEG_GPS_LOCATION: JCaptureRequest_Key; cdecl;
    {class} function _GetJPEG_ORIENTATION: JCaptureRequest_Key; cdecl;
    {class} function _GetJPEG_QUALITY: JCaptureRequest_Key; cdecl;
    {class} function _GetJPEG_THUMBNAIL_QUALITY: JCaptureRequest_Key; cdecl;
    {class} function _GetJPEG_THUMBNAIL_SIZE: JCaptureRequest_Key; cdecl;
    {class} function _GetLENS_APERTURE: JCaptureRequest_Key; cdecl;
    {class} function _GetLENS_FILTER_DENSITY: JCaptureRequest_Key; cdecl;
    {class} function _GetLENS_FOCAL_LENGTH: JCaptureRequest_Key; cdecl;
    {class} function _GetLENS_FOCUS_DISTANCE: JCaptureRequest_Key; cdecl;
    {class} function _GetLENS_OPTICAL_STABILIZATION_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetNOISE_REDUCTION_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetREPROCESS_EFFECTIVE_EXPOSURE_FACTOR: JCaptureRequest_Key; cdecl;
    {class} function _GetSCALER_CROP_REGION: JCaptureRequest_Key; cdecl;
    {class} function _GetSENSOR_EXPOSURE_TIME: JCaptureRequest_Key; cdecl;
    {class} function _GetSENSOR_FRAME_DURATION: JCaptureRequest_Key; cdecl;
    {class} function _GetSENSOR_SENSITIVITY: JCaptureRequest_Key; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_DATA: JCaptureRequest_Key; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetSHADING_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetSTATISTICS_FACE_DETECT_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetSTATISTICS_HOT_PIXEL_MAP_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetSTATISTICS_LENS_SHADING_MAP_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetTONEMAP_CURVE: JCaptureRequest_Key; cdecl;
    {class} function _GetTONEMAP_GAMMA: JCaptureRequest_Key; cdecl;
    {class} function _GetTONEMAP_MODE: JCaptureRequest_Key; cdecl;
    {class} function _GetTONEMAP_PRESET_CURVE: JCaptureRequest_Key; cdecl;
    {class} function describeContents: Integer; cdecl;
    {class} function equals(other: JObject): Boolean; cdecl;
    {class} function hashCode: Integer; cdecl;
    {class} function isReprocess: Boolean; cdecl;
    {class} procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    {class} property BLACK_LEVEL_LOCK: JCaptureRequest_Key read _GetBLACK_LEVEL_LOCK;
    {class} property COLOR_CORRECTION_ABERRATION_MODE: JCaptureRequest_Key read _GetCOLOR_CORRECTION_ABERRATION_MODE;
    {class} property COLOR_CORRECTION_GAINS: JCaptureRequest_Key read _GetCOLOR_CORRECTION_GAINS;
    {class} property COLOR_CORRECTION_MODE: JCaptureRequest_Key read _GetCOLOR_CORRECTION_MODE;
    {class} property COLOR_CORRECTION_TRANSFORM: JCaptureRequest_Key read _GetCOLOR_CORRECTION_TRANSFORM;
    {class} property CONTROL_AE_ANTIBANDING_MODE: JCaptureRequest_Key read _GetCONTROL_AE_ANTIBANDING_MODE;
    {class} property CONTROL_AE_EXPOSURE_COMPENSATION: JCaptureRequest_Key read _GetCONTROL_AE_EXPOSURE_COMPENSATION;
    {class} property CONTROL_AE_LOCK: JCaptureRequest_Key read _GetCONTROL_AE_LOCK;
    {class} property CONTROL_AE_MODE: JCaptureRequest_Key read _GetCONTROL_AE_MODE;
    {class} property CONTROL_AE_PRECAPTURE_TRIGGER: JCaptureRequest_Key read _GetCONTROL_AE_PRECAPTURE_TRIGGER;
    {class} property CONTROL_AE_REGIONS: JCaptureRequest_Key read _GetCONTROL_AE_REGIONS;
    {class} property CONTROL_AE_TARGET_FPS_RANGE: JCaptureRequest_Key read _GetCONTROL_AE_TARGET_FPS_RANGE;
    {class} property CONTROL_AF_MODE: JCaptureRequest_Key read _GetCONTROL_AF_MODE;
    {class} property CONTROL_AF_REGIONS: JCaptureRequest_Key read _GetCONTROL_AF_REGIONS;
    {class} property CONTROL_AF_TRIGGER: JCaptureRequest_Key read _GetCONTROL_AF_TRIGGER;
    {class} property CONTROL_AWB_LOCK: JCaptureRequest_Key read _GetCONTROL_AWB_LOCK;
    {class} property CONTROL_AWB_MODE: JCaptureRequest_Key read _GetCONTROL_AWB_MODE;
    {class} property CONTROL_AWB_REGIONS: JCaptureRequest_Key read _GetCONTROL_AWB_REGIONS;
    {class} property CONTROL_CAPTURE_INTENT: JCaptureRequest_Key read _GetCONTROL_CAPTURE_INTENT;
    {class} property CONTROL_EFFECT_MODE: JCaptureRequest_Key read _GetCONTROL_EFFECT_MODE;
    {class} property CONTROL_MODE: JCaptureRequest_Key read _GetCONTROL_MODE;
    {class} property CONTROL_SCENE_MODE: JCaptureRequest_Key read _GetCONTROL_SCENE_MODE;
    {class} property CONTROL_VIDEO_STABILIZATION_MODE: JCaptureRequest_Key read _GetCONTROL_VIDEO_STABILIZATION_MODE;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property EDGE_MODE: JCaptureRequest_Key read _GetEDGE_MODE;
    {class} property FLASH_MODE: JCaptureRequest_Key read _GetFLASH_MODE;
    {class} property HOT_PIXEL_MODE: JCaptureRequest_Key read _GetHOT_PIXEL_MODE;
    {class} property JPEG_GPS_LOCATION: JCaptureRequest_Key read _GetJPEG_GPS_LOCATION;
    {class} property JPEG_ORIENTATION: JCaptureRequest_Key read _GetJPEG_ORIENTATION;
    {class} property JPEG_QUALITY: JCaptureRequest_Key read _GetJPEG_QUALITY;
    {class} property JPEG_THUMBNAIL_QUALITY: JCaptureRequest_Key read _GetJPEG_THUMBNAIL_QUALITY;
    {class} property JPEG_THUMBNAIL_SIZE: JCaptureRequest_Key read _GetJPEG_THUMBNAIL_SIZE;
    {class} property LENS_APERTURE: JCaptureRequest_Key read _GetLENS_APERTURE;
    {class} property LENS_FILTER_DENSITY: JCaptureRequest_Key read _GetLENS_FILTER_DENSITY;
    {class} property LENS_FOCAL_LENGTH: JCaptureRequest_Key read _GetLENS_FOCAL_LENGTH;
    {class} property LENS_FOCUS_DISTANCE: JCaptureRequest_Key read _GetLENS_FOCUS_DISTANCE;
    {class} property LENS_OPTICAL_STABILIZATION_MODE: JCaptureRequest_Key read _GetLENS_OPTICAL_STABILIZATION_MODE;
    {class} property NOISE_REDUCTION_MODE: JCaptureRequest_Key read _GetNOISE_REDUCTION_MODE;
    {class} property REPROCESS_EFFECTIVE_EXPOSURE_FACTOR: JCaptureRequest_Key read _GetREPROCESS_EFFECTIVE_EXPOSURE_FACTOR;
    {class} property SCALER_CROP_REGION: JCaptureRequest_Key read _GetSCALER_CROP_REGION;
    {class} property SENSOR_EXPOSURE_TIME: JCaptureRequest_Key read _GetSENSOR_EXPOSURE_TIME;
    {class} property SENSOR_FRAME_DURATION: JCaptureRequest_Key read _GetSENSOR_FRAME_DURATION;
    {class} property SENSOR_SENSITIVITY: JCaptureRequest_Key read _GetSENSOR_SENSITIVITY;
    {class} property SENSOR_TEST_PATTERN_DATA: JCaptureRequest_Key read _GetSENSOR_TEST_PATTERN_DATA;
    {class} property SENSOR_TEST_PATTERN_MODE: JCaptureRequest_Key read _GetSENSOR_TEST_PATTERN_MODE;
    {class} property SHADING_MODE: JCaptureRequest_Key read _GetSHADING_MODE;
    {class} property STATISTICS_FACE_DETECT_MODE: JCaptureRequest_Key read _GetSTATISTICS_FACE_DETECT_MODE;
    {class} property STATISTICS_HOT_PIXEL_MAP_MODE: JCaptureRequest_Key read _GetSTATISTICS_HOT_PIXEL_MAP_MODE;
    {class} property STATISTICS_LENS_SHADING_MAP_MODE: JCaptureRequest_Key read _GetSTATISTICS_LENS_SHADING_MAP_MODE;
    {class} property TONEMAP_CURVE: JCaptureRequest_Key read _GetTONEMAP_CURVE;
    {class} property TONEMAP_GAMMA: JCaptureRequest_Key read _GetTONEMAP_GAMMA;
    {class} property TONEMAP_MODE: JCaptureRequest_Key read _GetTONEMAP_MODE;
    {class} property TONEMAP_PRESET_CURVE: JCaptureRequest_Key read _GetTONEMAP_PRESET_CURVE;
  end;

  [JavaSignature('android/hardware/camera2/CaptureRequest')]
  JCaptureRequest = interface(JCameraMetadata)
    ['{DE5A982E-E7CE-43F7-B901-D46AC7058B3D}']
    function get(key: JCaptureRequest_Key): JObject; cdecl;
    function getKeys: JList; cdecl;
    function getTag: JObject; cdecl;
  end;
  TJCaptureRequest = class(TJavaGenericImport<JCaptureRequestClass, JCaptureRequest>) end;

  JCaptureRequest_BuilderClass = interface(JObjectClass)
    ['{D0A84716-0BA3-4C99-BC48-C14835311511}']
  end;

  [JavaSignature('android/hardware/camera2/CaptureRequest$Builder')]
  JCaptureRequest_Builder = interface(JObject)
    ['{CE14F4D1-CDCF-479F-AC52-0C2E08C4FAE2}']
    procedure addTarget(outputTarget: JSurface); cdecl;
    function build: JCaptureRequest; cdecl;
    function get(key: JCaptureRequest_Key): JObject; cdecl;
    procedure removeTarget(outputTarget: JSurface); cdecl;
    procedure &set(key: JCaptureRequest_Key; value: JObject); cdecl;
    procedure setTag(tag: JObject); cdecl;
  end;
  TJCaptureRequest_Builder = class(TJavaGenericImport<JCaptureRequest_BuilderClass, JCaptureRequest_Builder>) end;

  JCaptureRequest_KeyClass = interface(JObjectClass)
    ['{84C9C919-2DBB-4C67-8AAB-BEA8BAC60687}']
    {class} function equals(o: JObject): Boolean; cdecl;
    {class} function getName: JString; cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CaptureRequest$Key')]
  JCaptureRequest_Key = interface(JObject)
    ['{FC062E90-A80B-4BFD-9B43-4F2D8B499851}']
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJCaptureRequest_Key = class(TJavaGenericImport<JCaptureRequest_KeyClass, JCaptureRequest_Key>) end;

  JCaptureResultClass = interface(JCameraMetadataClass)
    ['{4E2A72A0-82B6-4E73-BC2E-569B1C9BB761}']
    {class} function _GetBLACK_LEVEL_LOCK: JCaptureResult_Key; cdecl;
    {class} function _GetCOLOR_CORRECTION_ABERRATION_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCOLOR_CORRECTION_GAINS: JCaptureResult_Key; cdecl;
    {class} function _GetCOLOR_CORRECTION_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCOLOR_CORRECTION_TRANSFORM: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AE_ANTIBANDING_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AE_EXPOSURE_COMPENSATION: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AE_LOCK: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AE_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AE_PRECAPTURE_TRIGGER: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AE_REGIONS: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AE_STATE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AE_TARGET_FPS_RANGE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AF_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AF_REGIONS: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AF_STATE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AF_TRIGGER: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AWB_LOCK: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AWB_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AWB_REGIONS: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_AWB_STATE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_CAPTURE_INTENT: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_EFFECT_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_SCENE_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetCONTROL_VIDEO_STABILIZATION_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetEDGE_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetFLASH_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetFLASH_STATE: JCaptureResult_Key; cdecl;
    {class} function _GetHOT_PIXEL_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetJPEG_GPS_LOCATION: JCaptureResult_Key; cdecl;
    {class} function _GetJPEG_ORIENTATION: JCaptureResult_Key; cdecl;
    {class} function _GetJPEG_QUALITY: JCaptureResult_Key; cdecl;
    {class} function _GetJPEG_THUMBNAIL_QUALITY: JCaptureResult_Key; cdecl;
    {class} function _GetJPEG_THUMBNAIL_SIZE: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_APERTURE: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_FILTER_DENSITY: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_FOCAL_LENGTH: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_FOCUS_DISTANCE: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_FOCUS_RANGE: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_INTRINSIC_CALIBRATION: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_OPTICAL_STABILIZATION_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_POSE_ROTATION: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_POSE_TRANSLATION: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_RADIAL_DISTORTION: JCaptureResult_Key; cdecl;
    {class} function _GetLENS_STATE: JCaptureResult_Key; cdecl;
    {class} function _GetNOISE_REDUCTION_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetREPROCESS_EFFECTIVE_EXPOSURE_FACTOR: JCaptureResult_Key; cdecl;
    {class} function _GetREQUEST_PIPELINE_DEPTH: JCaptureResult_Key; cdecl;
    {class} function _GetSCALER_CROP_REGION: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_EXPOSURE_TIME: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_FRAME_DURATION: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_GREEN_SPLIT: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_NEUTRAL_COLOR_POINT: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_NOISE_PROFILE: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_ROLLING_SHUTTER_SKEW: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_SENSITIVITY: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_DATA: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_TEST_PATTERN_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetSENSOR_TIMESTAMP: JCaptureResult_Key; cdecl;
    {class} function _GetSHADING_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetSTATISTICS_FACES: JCaptureResult_Key; cdecl;
    {class} function _GetSTATISTICS_FACE_DETECT_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetSTATISTICS_HOT_PIXEL_MAP: JCaptureResult_Key; cdecl;
    {class} function _GetSTATISTICS_HOT_PIXEL_MAP_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetSTATISTICS_LENS_SHADING_CORRECTION_MAP: JCaptureResult_Key; cdecl;
    {class} function _GetSTATISTICS_LENS_SHADING_MAP_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetSTATISTICS_SCENE_FLICKER: JCaptureResult_Key; cdecl;
    {class} function _GetTONEMAP_CURVE: JCaptureResult_Key; cdecl;
    {class} function _GetTONEMAP_GAMMA: JCaptureResult_Key; cdecl;
    {class} function _GetTONEMAP_MODE: JCaptureResult_Key; cdecl;
    {class} function _GetTONEMAP_PRESET_CURVE: JCaptureResult_Key; cdecl;
    {class} property BLACK_LEVEL_LOCK: JCaptureResult_Key read _GetBLACK_LEVEL_LOCK;
    {class} property COLOR_CORRECTION_ABERRATION_MODE: JCaptureResult_Key read _GetCOLOR_CORRECTION_ABERRATION_MODE;
    {class} property COLOR_CORRECTION_GAINS: JCaptureResult_Key read _GetCOLOR_CORRECTION_GAINS;
    {class} property COLOR_CORRECTION_MODE: JCaptureResult_Key read _GetCOLOR_CORRECTION_MODE;
    {class} property COLOR_CORRECTION_TRANSFORM: JCaptureResult_Key read _GetCOLOR_CORRECTION_TRANSFORM;
    {class} property CONTROL_AE_ANTIBANDING_MODE: JCaptureResult_Key read _GetCONTROL_AE_ANTIBANDING_MODE;
    {class} property CONTROL_AE_EXPOSURE_COMPENSATION: JCaptureResult_Key read _GetCONTROL_AE_EXPOSURE_COMPENSATION;
    {class} property CONTROL_AE_LOCK: JCaptureResult_Key read _GetCONTROL_AE_LOCK;
    {class} property CONTROL_AE_MODE: JCaptureResult_Key read _GetCONTROL_AE_MODE;
    {class} property CONTROL_AE_PRECAPTURE_TRIGGER: JCaptureResult_Key read _GetCONTROL_AE_PRECAPTURE_TRIGGER;
    {class} property CONTROL_AE_REGIONS: JCaptureResult_Key read _GetCONTROL_AE_REGIONS;
    {class} property CONTROL_AE_STATE: JCaptureResult_Key read _GetCONTROL_AE_STATE;
    {class} property CONTROL_AE_TARGET_FPS_RANGE: JCaptureResult_Key read _GetCONTROL_AE_TARGET_FPS_RANGE;
    {class} property CONTROL_AF_MODE: JCaptureResult_Key read _GetCONTROL_AF_MODE;
    {class} property CONTROL_AF_REGIONS: JCaptureResult_Key read _GetCONTROL_AF_REGIONS;
    {class} property CONTROL_AF_STATE: JCaptureResult_Key read _GetCONTROL_AF_STATE;
    {class} property CONTROL_AF_TRIGGER: JCaptureResult_Key read _GetCONTROL_AF_TRIGGER;
    {class} property CONTROL_AWB_LOCK: JCaptureResult_Key read _GetCONTROL_AWB_LOCK;
    {class} property CONTROL_AWB_MODE: JCaptureResult_Key read _GetCONTROL_AWB_MODE;
    {class} property CONTROL_AWB_REGIONS: JCaptureResult_Key read _GetCONTROL_AWB_REGIONS;
    {class} property CONTROL_AWB_STATE: JCaptureResult_Key read _GetCONTROL_AWB_STATE;
    {class} property CONTROL_CAPTURE_INTENT: JCaptureResult_Key read _GetCONTROL_CAPTURE_INTENT;
    {class} property CONTROL_EFFECT_MODE: JCaptureResult_Key read _GetCONTROL_EFFECT_MODE;
    {class} property CONTROL_MODE: JCaptureResult_Key read _GetCONTROL_MODE;
    {class} property CONTROL_SCENE_MODE: JCaptureResult_Key read _GetCONTROL_SCENE_MODE;
    {class} property CONTROL_VIDEO_STABILIZATION_MODE: JCaptureResult_Key read _GetCONTROL_VIDEO_STABILIZATION_MODE;
    {class} property EDGE_MODE: JCaptureResult_Key read _GetEDGE_MODE;
    {class} property FLASH_MODE: JCaptureResult_Key read _GetFLASH_MODE;
    {class} property FLASH_STATE: JCaptureResult_Key read _GetFLASH_STATE;
    {class} property HOT_PIXEL_MODE: JCaptureResult_Key read _GetHOT_PIXEL_MODE;
    {class} property JPEG_GPS_LOCATION: JCaptureResult_Key read _GetJPEG_GPS_LOCATION;
    {class} property JPEG_ORIENTATION: JCaptureResult_Key read _GetJPEG_ORIENTATION;
    {class} property JPEG_QUALITY: JCaptureResult_Key read _GetJPEG_QUALITY;
    {class} property JPEG_THUMBNAIL_QUALITY: JCaptureResult_Key read _GetJPEG_THUMBNAIL_QUALITY;
    {class} property JPEG_THUMBNAIL_SIZE: JCaptureResult_Key read _GetJPEG_THUMBNAIL_SIZE;
    {class} property LENS_APERTURE: JCaptureResult_Key read _GetLENS_APERTURE;
    {class} property LENS_FILTER_DENSITY: JCaptureResult_Key read _GetLENS_FILTER_DENSITY;
    {class} property LENS_FOCAL_LENGTH: JCaptureResult_Key read _GetLENS_FOCAL_LENGTH;
    {class} property LENS_FOCUS_DISTANCE: JCaptureResult_Key read _GetLENS_FOCUS_DISTANCE;
    {class} property LENS_FOCUS_RANGE: JCaptureResult_Key read _GetLENS_FOCUS_RANGE;
    {class} property LENS_INTRINSIC_CALIBRATION: JCaptureResult_Key read _GetLENS_INTRINSIC_CALIBRATION;
    {class} property LENS_OPTICAL_STABILIZATION_MODE: JCaptureResult_Key read _GetLENS_OPTICAL_STABILIZATION_MODE;
    {class} property LENS_POSE_ROTATION: JCaptureResult_Key read _GetLENS_POSE_ROTATION;
    {class} property LENS_POSE_TRANSLATION: JCaptureResult_Key read _GetLENS_POSE_TRANSLATION;
    {class} property LENS_RADIAL_DISTORTION: JCaptureResult_Key read _GetLENS_RADIAL_DISTORTION;
    {class} property LENS_STATE: JCaptureResult_Key read _GetLENS_STATE;
    {class} property NOISE_REDUCTION_MODE: JCaptureResult_Key read _GetNOISE_REDUCTION_MODE;
    {class} property REPROCESS_EFFECTIVE_EXPOSURE_FACTOR: JCaptureResult_Key read _GetREPROCESS_EFFECTIVE_EXPOSURE_FACTOR;
    {class} property REQUEST_PIPELINE_DEPTH: JCaptureResult_Key read _GetREQUEST_PIPELINE_DEPTH;
    {class} property SCALER_CROP_REGION: JCaptureResult_Key read _GetSCALER_CROP_REGION;
    {class} property SENSOR_EXPOSURE_TIME: JCaptureResult_Key read _GetSENSOR_EXPOSURE_TIME;
    {class} property SENSOR_FRAME_DURATION: JCaptureResult_Key read _GetSENSOR_FRAME_DURATION;
    {class} property SENSOR_GREEN_SPLIT: JCaptureResult_Key read _GetSENSOR_GREEN_SPLIT;
    {class} property SENSOR_NEUTRAL_COLOR_POINT: JCaptureResult_Key read _GetSENSOR_NEUTRAL_COLOR_POINT;
    {class} property SENSOR_NOISE_PROFILE: JCaptureResult_Key read _GetSENSOR_NOISE_PROFILE;
    {class} property SENSOR_ROLLING_SHUTTER_SKEW: JCaptureResult_Key read _GetSENSOR_ROLLING_SHUTTER_SKEW;
    {class} property SENSOR_SENSITIVITY: JCaptureResult_Key read _GetSENSOR_SENSITIVITY;
    {class} property SENSOR_TEST_PATTERN_DATA: JCaptureResult_Key read _GetSENSOR_TEST_PATTERN_DATA;
    {class} property SENSOR_TEST_PATTERN_MODE: JCaptureResult_Key read _GetSENSOR_TEST_PATTERN_MODE;
    {class} property SENSOR_TIMESTAMP: JCaptureResult_Key read _GetSENSOR_TIMESTAMP;
    {class} property SHADING_MODE: JCaptureResult_Key read _GetSHADING_MODE;
    {class} property STATISTICS_FACES: JCaptureResult_Key read _GetSTATISTICS_FACES;
    {class} property STATISTICS_FACE_DETECT_MODE: JCaptureResult_Key read _GetSTATISTICS_FACE_DETECT_MODE;
    {class} property STATISTICS_HOT_PIXEL_MAP: JCaptureResult_Key read _GetSTATISTICS_HOT_PIXEL_MAP;
    {class} property STATISTICS_HOT_PIXEL_MAP_MODE: JCaptureResult_Key read _GetSTATISTICS_HOT_PIXEL_MAP_MODE;
    {class} property STATISTICS_LENS_SHADING_CORRECTION_MAP: JCaptureResult_Key read _GetSTATISTICS_LENS_SHADING_CORRECTION_MAP;
    {class} property STATISTICS_LENS_SHADING_MAP_MODE: JCaptureResult_Key read _GetSTATISTICS_LENS_SHADING_MAP_MODE;
    {class} property STATISTICS_SCENE_FLICKER: JCaptureResult_Key read _GetSTATISTICS_SCENE_FLICKER;
    {class} property TONEMAP_CURVE: JCaptureResult_Key read _GetTONEMAP_CURVE;
    {class} property TONEMAP_GAMMA: JCaptureResult_Key read _GetTONEMAP_GAMMA;
    {class} property TONEMAP_MODE: JCaptureResult_Key read _GetTONEMAP_MODE;
    {class} property TONEMAP_PRESET_CURVE: JCaptureResult_Key read _GetTONEMAP_PRESET_CURVE;
  end;

  [JavaSignature('android/hardware/camera2/CaptureResult')]
  JCaptureResult = interface(JCameraMetadata)
    ['{E5534E3A-5D79-43EC-9FCB-9D9FA5149C9A}']
    function get(key: JCaptureResult_Key): JObject; cdecl;
    function getFrameNumber: Int64; cdecl;
    function getKeys: JList; cdecl;
    function getRequest: JCaptureRequest; cdecl;
    function getSequenceId: Integer; cdecl;
  end;
  TJCaptureResult = class(TJavaGenericImport<JCaptureResultClass, JCaptureResult>) end;

  JCaptureResult_KeyClass = interface(JObjectClass)
    ['{A2BC4CC7-561D-4F1A-BD48-0A6728298924}']
    {class} function equals(o: JObject): Boolean; cdecl;
  end;

  [JavaSignature('android/hardware/camera2/CaptureResult$Key')]
  JCaptureResult_Key = interface(JObject)
    ['{556A23EF-0A3C-4187-8E73-6D0B63EA4C1A}']
    function getName: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJCaptureResult_Key = class(TJavaGenericImport<JCaptureResult_KeyClass, JCaptureResult_Key>) end;

  JDngCreatorClass = interface(JObjectClass)
    ['{5F70047B-9CB4-45D5-81BC-039C975D52B9}']
    {class} function _GetMAX_THUMBNAIL_DIMENSION: Integer; cdecl;
    {class} function init(characteristics: JCameraCharacteristics; metadata: JCaptureResult): JDngCreator; cdecl;
    {class} function setOrientation(orientation: Integer): JDngCreator; cdecl;
    {class} function setThumbnail(pixels: JBitmap): JDngCreator; cdecl; overload;
    {class} function setThumbnail(pixels: JImage): JDngCreator; cdecl; overload;
    {class} property MAX_THUMBNAIL_DIMENSION: Integer read _GetMAX_THUMBNAIL_DIMENSION;
  end;

  [JavaSignature('android/hardware/camera2/DngCreator')]
  JDngCreator = interface(JObject)
    ['{FB37296C-3D1F-4509-9721-6C011B7EFCDE}']
    procedure close; cdecl;
    function setDescription(description: JString): JDngCreator; cdecl;
    function setLocation(location: JLocation): JDngCreator; cdecl;
    procedure writeByteBuffer(dngOutput: JOutputStream; size: Jutil_Size; pixels: JByteBuffer; offset: Int64); cdecl;
    procedure writeImage(dngOutput: JOutputStream; pixels: JImage); cdecl;
    procedure writeInputStream(dngOutput: JOutputStream; size: Jutil_Size; pixels: JInputStream; offset: Int64); cdecl;
  end;
  TJDngCreator = class(TJavaGenericImport<JDngCreatorClass, JDngCreator>) end;

  JTotalCaptureResultClass = interface(JCaptureResultClass)
    ['{640855D0-2252-4CDB-A16B-CAFD58000AC4}']
  end;

  [JavaSignature('android/hardware/camera2/TotalCaptureResult')]
  JTotalCaptureResult = interface(JCaptureResult)
    ['{141D5DEB-330D-418E-B23C-0A1DD9CAABE0}']
    function getPartialResults: JList; cdecl;
  end;
  TJTotalCaptureResult = class(TJavaGenericImport<JTotalCaptureResultClass, JTotalCaptureResult>) end;

  JBlackLevelPatternClass = interface(JObjectClass)
    ['{5A47E4E9-14AE-4198-9FC1-58AB9AABB3E7}']
    {class} function _GetCOUNT: Integer; cdecl;
    {class} function hashCode: Integer; cdecl;
    {class} function toString: JString; cdecl;
    {class} property COUNT: Integer read _GetCOUNT;
  end;

  [JavaSignature('android/hardware/camera2/params/BlackLevelPattern')]
  JBlackLevelPattern = interface(JObject)
    ['{6DE320FA-9C1B-4D63-8444-0B0C747EEFF6}']
    procedure copyTo(destination: TJavaArray<Integer>; offset: Integer); cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getOffsetForIndex(column: Integer; row: Integer): Integer; cdecl;
  end;
  TJBlackLevelPattern = class(TJavaGenericImport<JBlackLevelPatternClass, JBlackLevelPattern>) end;

  JColorSpaceTransformClass = interface(JObjectClass)
    ['{C1131D24-CF71-48CA-9412-2899AB60EC1F}']
    {class} function init(elements: TJavaObjectArray<JRational>): JColorSpaceTransform; cdecl; overload;
    {class} function init(elements: TJavaArray<Integer>): JColorSpaceTransform; cdecl; overload;
    {class} function equals(obj: JObject): Boolean; cdecl;
    {class} function getElement(column: Integer; row: Integer): JRational; cdecl;
    {class} function hashCode: Integer; cdecl;
  end;

  [JavaSignature('android/hardware/camera2/params/ColorSpaceTransform')]
  JColorSpaceTransform = interface(JObject)
    ['{C40D1D86-48BF-4BDB-8AC7-8098498EB47E}']
    procedure copyElements(destination: TJavaObjectArray<JRational>; offset: Integer); cdecl; overload;
    procedure copyElements(destination: TJavaArray<Integer>; offset: Integer); cdecl; overload;
    function toString: JString; cdecl;
  end;
  TJColorSpaceTransform = class(TJavaGenericImport<JColorSpaceTransformClass, JColorSpaceTransform>) end;

  JFaceClass = interface(JObjectClass)
    ['{C923DBCB-4AFD-44F6-AF45-4071D1ED8599}']
    {class} function _GetID_UNSUPPORTED: Integer; cdecl;
    {class} function _GetSCORE_MAX: Integer; cdecl;
    {class} function _GetSCORE_MIN: Integer; cdecl;
    {class} property ID_UNSUPPORTED: Integer read _GetID_UNSUPPORTED;
    {class} property SCORE_MAX: Integer read _GetSCORE_MAX;
    {class} property SCORE_MIN: Integer read _GetSCORE_MIN;
  end;

  [JavaSignature('android/hardware/camera2/params/Face')]
  JFace = interface(JObject)
    ['{02C94216-1ADA-45AC-98DD-47F44FE07D32}']
    function getBounds: JRect; cdecl;
    function getId: Integer; cdecl;
    function getLeftEyePosition: JPoint; cdecl;
    function getMouthPosition: JPoint; cdecl;
    function getRightEyePosition: JPoint; cdecl;
    function getScore: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJFace = class(TJavaGenericImport<JFaceClass, JFace>) end;

  JInputConfigurationClass = interface(JObjectClass)
    ['{AEDEAB04-ABC7-4082-B819-72B02B0144D1}']
    {class} function init(width: Integer; height: Integer; format: Integer): JInputConfiguration; cdecl;
    {class} function equals(obj: JObject): Boolean; cdecl;
    {class} function getFormat: Integer; cdecl;
    {class} function toString: JString; cdecl;
  end;

  [JavaSignature('android/hardware/camera2/params/InputConfiguration')]
  JInputConfiguration = interface(JObject)
    ['{CA1DB38A-7A2C-465C-9D2D-992E95CDDE9E}']
    function getHeight: Integer; cdecl;
    function getWidth: Integer; cdecl;
    function hashCode: Integer; cdecl;
  end;
  TJInputConfiguration = class(TJavaGenericImport<JInputConfigurationClass, JInputConfiguration>) end;

  JLensShadingMapClass = interface(JObjectClass)
    ['{37531873-023F-425D-88FF-7E93C2939539}']
    {class} function _GetMINIMUM_GAIN_FACTOR: Single; cdecl;
    {class} function equals(obj: JObject): Boolean; cdecl;
    {class} function getColumnCount: Integer; cdecl;
    {class} function getRowCount: Integer; cdecl;
    {class} function hashCode: Integer; cdecl;
    {class} function toString: JString; cdecl;
    {class} property MINIMUM_GAIN_FACTOR: Single read _GetMINIMUM_GAIN_FACTOR;
  end;

  [JavaSignature('android/hardware/camera2/params/LensShadingMap')]
  JLensShadingMap = interface(JObject)
    ['{07CFEA4F-633D-43DE-9589-7458E22C1E59}']
    procedure copyGainFactors(destination: TJavaArray<Single>; offset: Integer); cdecl;
    function getGainFactor(colorChannel: Integer; column: Integer; row: Integer): Single; cdecl;
    function getGainFactorCount: Integer; cdecl;
    function getGainFactorVector(column: Integer; row: Integer): JRggbChannelVector; cdecl;
  end;
  TJLensShadingMap = class(TJavaGenericImport<JLensShadingMapClass, JLensShadingMap>) end;

  JMeteringRectangleClass = interface(JObjectClass)
    ['{9D651E5E-AE2C-4FA2-A42A-4E8C25888C22}']
    {class} function _GetMETERING_WEIGHT_DONT_CARE: Integer; cdecl;
    {class} function _GetMETERING_WEIGHT_MAX: Integer; cdecl;
    {class} function _GetMETERING_WEIGHT_MIN: Integer; cdecl;
    {class} function init(x: Integer; y: Integer; width: Integer; height: Integer; meteringWeight: Integer): JMeteringRectangle; cdecl; overload;
    {class} function init(xy: JPoint; dimensions: Jutil_Size; meteringWeight: Integer): JMeteringRectangle; cdecl; overload;
    {class} function init(rect: JRect; meteringWeight: Integer): JMeteringRectangle; cdecl; overload;
    {class} function getMeteringWeight: Integer; cdecl;
    {class} function getRect: JRect; cdecl;
    {class} function getSize: Jutil_Size; cdecl;
    {class} function getY: Integer; cdecl;
    {class} function hashCode: Integer; cdecl;
    {class} function toString: JString; cdecl;
    {class} property METERING_WEIGHT_DONT_CARE: Integer read _GetMETERING_WEIGHT_DONT_CARE;
    {class} property METERING_WEIGHT_MAX: Integer read _GetMETERING_WEIGHT_MAX;
    {class} property METERING_WEIGHT_MIN: Integer read _GetMETERING_WEIGHT_MIN;
  end;

  [JavaSignature('android/hardware/camera2/params/MeteringRectangle')]
  JMeteringRectangle = interface(JObject)
    ['{1CB8E2C0-AE64-4A54-A025-D1E04AB5F415}']
    function equals(other: JObject): Boolean; cdecl; overload;
    function equals(other: JMeteringRectangle): Boolean; cdecl; overload;
    function getHeight: Integer; cdecl;
    function getUpperLeftPoint: JPoint; cdecl;
    function getWidth: Integer; cdecl;
    function getX: Integer; cdecl;
  end;
  TJMeteringRectangle = class(TJavaGenericImport<JMeteringRectangleClass, JMeteringRectangle>) end;

  JRggbChannelVectorClass = interface(JObjectClass)
    ['{C4209DA1-1B55-433D-AB66-C744DE3B07D2}']
    {class} function _GetBLUE: Integer; cdecl;
    {class} function _GetCOUNT: Integer; cdecl;
    {class} function _GetGREEN_EVEN: Integer; cdecl;
    {class} function _GetGREEN_ODD: Integer; cdecl;
    {class} function _GetRED: Integer; cdecl;
    {class} function init(red: Single; greenEven: Single; greenOdd: Single; blue: Single): JRggbChannelVector; cdecl;
    {class} procedure copyTo(destination: TJavaArray<Single>; offset: Integer); cdecl;
    {class} function getGreenEven: Single; cdecl;
    {class} function getGreenOdd: Single; cdecl;
    {class} function getRed: Single; cdecl;
    {class} property BLUE: Integer read _GetBLUE;
    {class} property COUNT: Integer read _GetCOUNT;
    {class} property GREEN_EVEN: Integer read _GetGREEN_EVEN;
    {class} property GREEN_ODD: Integer read _GetGREEN_ODD;
    {class} property RED: Integer read _GetRED;
  end;

  [JavaSignature('android/hardware/camera2/params/RggbChannelVector')]
  JRggbChannelVector = interface(JObject)
    ['{834FE93D-F479-4551-9C26-C1FD527BE288}']
    function equals(obj: JObject): Boolean; cdecl;
    function getBlue: Single; cdecl;
    function getComponent(colorChannel: Integer): Single; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJRggbChannelVector = class(TJavaGenericImport<JRggbChannelVectorClass, JRggbChannelVector>) end;

  JStreamConfigurationMapClass = interface(JObjectClass)
    ['{3AA22CBD-F254-411E-BBD1-CBC5595B811F}']
    {class} function isOutputSupportedFor(klass: Jlang_Class): Boolean; cdecl; overload;
  end;

  [JavaSignature('android/hardware/camera2/params/StreamConfigurationMap')]
  JStreamConfigurationMap = interface(JObject)
    ['{513210A3-ED36-4CBE-8339-E23DB9000594}']
    function equals(obj: JObject): Boolean; cdecl;
    function getHighResolutionOutputSizes(format: Integer): TJavaObjectArray<Jutil_Size>; cdecl;
    function getHighSpeedVideoSizesFor(fpsRange: JRange): TJavaObjectArray<Jutil_Size>; cdecl;
    function getHighSpeedVideoFpsRanges: TJavaObjectArray<JRange>; cdecl;
    function getHighSpeedVideoFpsRangesFor(size: Jutil_Size): TJavaObjectArray<JRange>; cdecl;
    function getHighSpeedVideoSizes: TJavaObjectArray<Jutil_Size>; cdecl;
    function getInputFormats: TJavaArray<Integer>; cdecl;
    function getInputSizes(format: Integer): TJavaObjectArray<Jutil_Size>; cdecl;
    function getOutputFormats: TJavaArray<Integer>; cdecl;
    function getOutputMinFrameDuration(format: Integer; size: Jutil_Size): Int64; cdecl; overload;
    function getOutputMinFrameDuration(klass: Jlang_Class; size: Jutil_Size): Int64; cdecl; overload;
    function getOutputSizes(klass: Jlang_Class): TJavaObjectArray<Jutil_Size>; cdecl; overload;
    function getOutputSizes(format: Integer): TJavaObjectArray<Jutil_Size>; cdecl; overload;
    function getOutputStallDuration(format: Integer; size: Jutil_Size): Int64; cdecl; overload;
    function getOutputStallDuration(klass: Jlang_Class; size: Jutil_Size): Int64; cdecl; overload;
    function getValidOutputFormatsForInput(inputFormat: Integer): TJavaArray<Integer>; cdecl;
    function hashCode: Integer; cdecl;
    function isOutputSupportedFor(format: Integer): Boolean; cdecl; overload;
    function isOutputSupportedFor(surface: JSurface): Boolean; cdecl; overload;
    function toString: JString; cdecl;
  end;
  TJStreamConfigurationMap = class(TJavaGenericImport<JStreamConfigurationMapClass, JStreamConfigurationMap>) end;

  JTonemapCurveClass = interface(JObjectClass)
    ['{3A11EAEA-4CB6-4824-B313-C7B22A297017}']
    {class} function _GetCHANNEL_BLUE: Integer; cdecl;
    {class} function _GetCHANNEL_GREEN: Integer; cdecl;
    {class} function _GetCHANNEL_RED: Integer; cdecl;
    {class} function _GetLEVEL_BLACK: Single; cdecl;
    {class} function _GetLEVEL_WHITE: Single; cdecl;
    {class} function _GetPOINT_SIZE: Integer; cdecl;
    {class} function init(red: TJavaArray<Single>; green: TJavaArray<Single>; blue: TJavaArray<Single>): JTonemapCurve; cdecl;
    {class} procedure copyColorCurve(colorChannel: Integer; destination: TJavaArray<Single>; offset: Integer); cdecl;
    {class} function equals(obj: JObject): Boolean; cdecl;
    {class} function toString: JString; cdecl;
    {class} property CHANNEL_BLUE: Integer read _GetCHANNEL_BLUE;
    {class} property CHANNEL_GREEN: Integer read _GetCHANNEL_GREEN;
    {class} property CHANNEL_RED: Integer read _GetCHANNEL_RED;
    {class} property LEVEL_BLACK: Single read _GetLEVEL_BLACK;
    {class} property LEVEL_WHITE: Single read _GetLEVEL_WHITE;
    {class} property POINT_SIZE: Integer read _GetPOINT_SIZE;
  end;

  [JavaSignature('android/hardware/camera2/params/TonemapCurve')]
  JTonemapCurve = interface(JObject)
    ['{8F24BDA6-2B3D-48B1-BE2F-9E7E237C9A8F}']
    function getPoint(colorChannel: Integer; index: Integer): JPointF; cdecl;
    function getPointCount(colorChannel: Integer): Integer; cdecl;
    function hashCode: Integer; cdecl;
  end;
  TJTonemapCurve = class(TJavaGenericImport<JTonemapCurveClass, JTonemapCurve>) end;

  JRationalClass = interface(JNumberClass)
    ['{592776A4-0BC8-4829-88B1-714E90C34FE1}']
    {class} function _GetNEGATIVE_INFINITY: JRational; cdecl;
    {class} function _GetNaN: JRational; cdecl;
    {class} function _GetPOSITIVE_INFINITY: JRational; cdecl;
    {class} function _GetZERO: JRational; cdecl;
    {class} function init(numerator: Integer; denominator: Integer): JRational; cdecl;
    {class} function compareTo(another: JRational): Integer; cdecl;
    {class} function doubleValue: Double; cdecl;
    {class} function getNumerator: Integer; cdecl;
    {class} function hashCode: Integer; cdecl;
    {class} function intValue: Integer; cdecl;
    {class} function isZero: Boolean; cdecl;
    {class} function longValue: Int64; cdecl;
    {class} function parseRational(&string: JString): JRational; cdecl;
    {class} property NEGATIVE_INFINITY: JRational read _GetNEGATIVE_INFINITY;
    {class} property NaN: JRational read _GetNaN;
    {class} property POSITIVE_INFINITY: JRational read _GetPOSITIVE_INFINITY;
    {class} property ZERO: JRational read _GetZERO;
  end;

  [JavaSignature('android/util/Rational')]
  JRational = interface(JNumber)
    ['{5DEFEB1B-2D1F-4ADF-B69B-54B0498F8544}']
    function equals(obj: JObject): Boolean; cdecl;
    function floatValue: Single; cdecl;
    function getDenominator: Integer; cdecl;
    function isFinite: Boolean; cdecl;
    function isInfinite: Boolean; cdecl;
    function isNaN: Boolean; cdecl;
    function shortValue: SmallInt; cdecl;
    function toString: JString; cdecl;
  end;
  TJRational = class(TJavaGenericImport<JRationalClass, JRational>) end;

implementation

end.

