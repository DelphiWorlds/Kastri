unit DW.Androidapi.JNI.DWCameraHelpers;

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

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.Util,
  Androidapi.JNI.OpenGL,
  // DW
  DW.Androidapi.JNI.Hardware.Camera2, DW.Androidapi.JNI.View;

type
  JDWCameraCharacteristicsHelper = interface;
  JDWCaptureRequestBuilderHelper = interface;
  JDWCaptureResultHelper = interface;
  JDWCameraCaptureSessionStateCallback = interface;
  JDWCameraCaptureSessionStateCallbackDelegate = interface;
  JDWCameraDeviceStateCallback = interface;
  JDWCameraDeviceStateCallbackDelegate = interface;
  JDWCameraCaptureSessionCaptureCallback = interface;
  JDWCameraCaptureSessionCaptureCallbackDelegate = interface;
  JDWCameraView = interface;
  JDWCameraView_StateDelegate = interface;
  JDWGLCameraView = interface;
  JDWGLCameraView_ViewDelegate = interface;

  JDWCameraCharacteristicsHelperClass = interface(JObjectClass)
    ['{546D0D05-92AF-4D33-8AC5-0EC46022E85D}']
    {class} function init: JDWCameraCharacteristicsHelper; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraCharacteristicsHelper')]
  JDWCameraCharacteristicsHelper = interface(JObject)
    ['{66872D0B-17E3-4AF1-8B27-1D1731A40DE9}']
    function getControlAEAvailableModes: TJavaArray<Integer>; cdecl;
    function getFaceDetectModes: TJavaArray<Integer>; cdecl;
    function getLensFacing: Integer; cdecl;
    function getMap: JStreamConfigurationMap; cdecl;
    function getSensorExposureTimeLower: Int64; cdecl;
    function getSensorExposureTimeUpper: Int64; cdecl;
    function getSensorOrientation: Integer; cdecl;
    function getSensorSensitivityLower: Integer; cdecl;
    function getSensorSensitivityUpper: Integer; cdecl;
    procedure setCameraCharacteristics(characteristics: JCameraCharacteristics); cdecl;
  end;
  TJDWCameraCharacteristicsHelper = class(TJavaGenericImport<JDWCameraCharacteristicsHelperClass, JDWCameraCharacteristicsHelper>) end;

  JDWCaptureRequestBuilderHelperClass = interface(JObjectClass)
    ['{2A88D2A3-036C-4F46-88DF-297241184A70}']
    {class} function init: JDWCaptureRequestBuilderHelper; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCaptureRequestBuilderHelper')]
  JDWCaptureRequestBuilderHelper = interface(JObject)
    ['{ECDB6573-589C-4C0E-B8B9-D68F43425D98}']
    function getIntegerValue(key: JCaptureRequest_Key): Integer; cdecl;
    function getLongValue(key: JCaptureRequest_Key): Int64; cdecl;
    procedure setCaptureRequestBuilder(builder: JCaptureRequest_Builder); cdecl;
    procedure setFaceDetectMode(mode: Integer); cdecl;
    procedure setIntegerValue(key: JCaptureRequest_Key; value: Integer); cdecl;
    procedure setLongValue(key: JCaptureRequest_Key; value: Int64); cdecl;
    procedure setOrientation(orientation: Integer); cdecl;
  end;
  TJDWCaptureRequestBuilderHelper = class(TJavaGenericImport<JDWCaptureRequestBuilderHelperClass, JDWCaptureRequestBuilderHelper>) end;

  JDWCaptureResultHelperClass = interface(JObjectClass)
    ['{D6B024D2-1244-4BEF-A2F9-DE5FC1171E95}']
    {class} function init: JDWCaptureResultHelper; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCaptureResultHelper')]
  JDWCaptureResultHelper = interface(JObject)
    ['{88F9C12C-DB60-4E77-A511-F7BB25DCB38B}']
    function getFaceDetectMode: Integer; cdecl;
    function getFaces: TJavaObjectArray<JFace>; cdecl;
    procedure setCaptureResult(captureResult: JCaptureResult); cdecl;
  end;
  TJDWCaptureResultHelper = class(TJavaGenericImport<JDWCaptureResultHelperClass, JDWCaptureResultHelper>) end;

  JDWCameraCaptureSessionStateCallbackClass = interface(JCameraCaptureSession_StateCallbackClass)
    ['{D8539E16-3066-4BF1-8483-DA4B95854D39}']
    {class} function init(delegate: JDWCameraCaptureSessionStateCallbackDelegate): JDWCameraCaptureSessionStateCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraCaptureSessionStateCallback')]
  JDWCameraCaptureSessionStateCallback = interface(JCameraCaptureSession_StateCallback)
    ['{AB18B118-492E-4ED4-856A-1BC58F655BB3}']
    procedure onConfigured(session: JCameraCaptureSession); cdecl;
    procedure onConfigureFailed(session: JCameraCaptureSession); cdecl;
  end;
  TJDWCameraCaptureSessionStateCallback = class(TJavaGenericImport<JDWCameraCaptureSessionStateCallbackClass,
    JDWCameraCaptureSessionStateCallback>) end;

  JDWCameraCaptureSessionStateCallbackDelegateClass = interface(IJavaClass)
    ['{0275BFB6-4DB5-4A8A-B8AC-925A0DA09A69}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraCaptureSessionStateCallbackDelegate')]
  JDWCameraCaptureSessionStateCallbackDelegate = interface(IJavaInstance)
    ['{29110A8F-CD76-443F-9311-BBF17EB82C8A}']
    procedure onConfigured(session: JCameraCaptureSession); cdecl;
    procedure onConfigureFailed(session: JCameraCaptureSession); cdecl;
  end;
  TJDWCameraCaptureSessionStateCallbackDelegate = class(TJavaGenericImport<JDWCameraCaptureSessionStateCallbackDelegateClass,
    JDWCameraCaptureSessionStateCallbackDelegate>) end;

  JDWCameraCaptureSessionCaptureCallbackClass = interface(JCameraCaptureSession_CaptureCallbackClass)
    ['{5DF98741-D7C1-451F-BCD9-767A2AF1F7A7}']
    {class} function init(delegate: JDWCameraCaptureSessionCaptureCallbackDelegate): JDWCameraCaptureSessionCaptureCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraCaptureSessionCaptureCallback')]
  JDWCameraCaptureSessionCaptureCallback = interface(JCameraCaptureSession_CaptureCallback)
    ['{8684B527-A48D-4F16-BBEA-B07336EB3295}']
    procedure onCaptureCompleted(session: JCameraCaptureSession; request: JCaptureRequest; result: JTotalCaptureResult); cdecl;
    procedure onCaptureProgressed(session: JCameraCaptureSession; request: JCaptureRequest; partialResult: JCaptureResult); cdecl;
  end;
  TJDWCameraCaptureSessionCaptureCallback = class(TJavaGenericImport<JDWCameraCaptureSessionCaptureCallbackClass,
    JDWCameraCaptureSessionCaptureCallback>) end;

  JDWCameraCaptureSessionCaptureCallbackDelegateClass = interface(IJavaClass)
    ['{F672F04E-A294-43A9-A2E2-14D4986E9F66}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraCaptureSessionCaptureCallbackDelegate')]
  JDWCameraCaptureSessionCaptureCallbackDelegate = interface(IJavaInstance)
    ['{A4783EF9-ABEB-4342-9453-1C6AF15C7E2A}']
    procedure onCaptureCompleted(session: JCameraCaptureSession; request: JCaptureRequest; result: JTotalCaptureResult); cdecl;
    procedure onCaptureProgressed(session: JCameraCaptureSession; request: JCaptureRequest; partialResult: JCaptureResult); cdecl;
  end;
  TJDWCameraCaptureSessionCaptureCallbackDelegate = class(TJavaGenericImport<JDWCameraCaptureSessionCaptureCallbackDelegateClass,
    JDWCameraCaptureSessionCaptureCallbackDelegate>) end;

  JDWCameraDeviceStateCallbackClass = interface(JCameraDevice_StateCallbackClass)
    ['{2569DA5F-9292-4F17-8153-E0B657A0A704}']
    {class} function init(delegate: JDWCameraDeviceStateCallbackDelegate): JDWCameraDeviceStateCallback; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraDeviceStateCallback')]
  JDWCameraDeviceStateCallback = interface(JCameraDevice_StateCallback)
    ['{E29208A3-94CA-4383-82AF-527090ED2B5B}']
    procedure onDisconnected(camera: JCameraDevice); cdecl;
    procedure onError(camera: JCameraDevice; error: Integer); cdecl;
    procedure onOpened(camera: JCameraDevice); cdecl;
  end;
  TJDWCameraDeviceStateCallback = class(TJavaGenericImport<JDWCameraDeviceStateCallbackClass, JDWCameraDeviceStateCallback>) end;

  JDWCameraDeviceStateCallbackDelegateClass = interface(IJavaClass)
    ['{189B3248-8E4F-49DA-ADAF-5B1A2A7569D1}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraDeviceStateCallbackDelegate')]
  JDWCameraDeviceStateCallbackDelegate = interface(IJavaInstance)
    ['{CF9CD6DA-55B3-4EC6-9DC3-84FB2F73722E}']
    procedure onDisconnected(camera: JCameraDevice); cdecl;
    procedure onError(camera: JCameraDevice; error: Integer); cdecl;
    procedure onOpened(camera: JCameraDevice); cdecl;
  end;
  TJDWCameraDeviceStateCallbackDelegate = class(TJavaGenericImport<JDWCameraDeviceStateCallbackDelegateClass,
    JDWCameraDeviceStateCallbackDelegate>) end;

  JDWCameraViewClass = interface(JTextureViewClass)
    ['{730B7098-A6C2-4105-B721-7845A76685E7}']
    {class} function init(context: JContext): JDWCameraView; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraView')]
  JDWCameraView = interface(JTextureView)
    ['{520DC031-2F59-4AFE-BDF0-9DA6C11035B2}']
    function getPreviewSize: Jutil_Size; cdecl;
    procedure onSurfaceTextureAvailable(texture: JSurfaceTexture; i: Integer; i1: Integer); cdecl;
    function onSurfaceTextureDestroyed(texture: JSurfaceTexture): Boolean; cdecl;
    procedure onSurfaceTextureSizeChanged(texture: JSurfaceTexture; i: Integer; i1: Integer); cdecl;
    procedure onSurfaceTextureUpdated(texture: JSurfaceTexture); cdecl;
    procedure setMirror(mirror: Boolean); cdecl;
    procedure setPreviewSize(previewSize: Jutil_Size); cdecl;
    procedure setStateDelegate(delegate: JDWCameraView_StateDelegate); cdecl;
  end;
  TJDWCameraView = class(TJavaGenericImport<JDWCameraViewClass, JDWCameraView>) end;

  JDWCameraView_StateDelegateClass = interface(IJavaClass)
    ['{C817506F-6A64-41EB-85F2-DD63B85C56F4}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWCameraView$StateDelegate')]
  JDWCameraView_StateDelegate = interface(IJavaInstance)
    ['{E11F0129-2DC9-4806-85CF-D9B9C08EA397}']
    procedure onDestroyed(view: JDWCameraView); cdecl;
    procedure onReady(view: JDWCameraView); cdecl;
  end;
  TJDWCameraView_StateDelegate = class(TJavaGenericImport<JDWCameraView_StateDelegateClass, JDWCameraView_StateDelegate>) end;

  JDWGLCameraViewClass = interface(JGLSurfaceViewClass)
    ['{7F9B7F01-079E-4F57-8CBC-FE9FA003A344}']
    {class} function init(context: JContext): JDWGLCameraView; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWGLCameraView')]
  JDWGLCameraView = interface(JGLSurfaceView)
    ['{9BC9F706-03DF-4427-A09D-A40E88FD425D}']
    // procedure setDefaultSize(defaultSize: Jutil_Size); cdecl;
    procedure setViewDelegate(delegate: JDWGLCameraView_ViewDelegate); cdecl;
    procedure setCameraRotation(rotation: Integer); cdecl;
    procedure setCaptureFPS(fps: Integer); cdecl;
  end;
  TJDWGLCameraView = class(TJavaGenericImport<JDWGLCameraViewClass, JDWGLCameraView>) end;

  JDWGLCameraView_ViewDelegateClass = interface(IJavaClass)
    ['{7464C525-5E2E-4CA8-AAD8-4B4E2B2298C7}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWGLCameraView$ViewDelegate')]
  JDWGLCameraView_ViewDelegate = interface(IJavaInstance)
    ['{8BD270FF-0D3A-4F66-A2C0-6A2D0903F559}']
    procedure onFrameAvailable(frame: JBitmap); cdecl;
    procedure onSurfaceTextureAvailable(texture: JSurfaceTexture); cdecl;
  end;
  TJGLDWCameraView_ViewDelegate = class(TJavaGenericImport<JDWGLCameraView_ViewDelegateClass, JDWGLCameraView_ViewDelegate>) end;

implementation

end.
