unit DW.Camera.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2022 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.Util, Androidapi.Gles, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.Media,
  // FMX
  FMX.Controls, FMX.Graphics, FMX.Types,
  // DW
  DW.Camera, DW.Androidapi.JNI.Os, DW.Androidapi.JNI.Hardware.Camera2, DW.Androidapi.JNI.DWCameraHelpers, DW.Androidapi.JNI.View, DW.CameraPreview;

type
  TPlatformCamera = class;

  TCameraCaptureSession = class(TObject)
  private
    FCameraView: JDWCameraView;
    // FCameraView: JDWGLCameraView;
    // FCameraViewDelegate: JDWGLCameraView_ViewDelegate;
    FCaptureSessionCaptureCallback: JDWCameraCaptureSessionCaptureCallback;
    FCaptureSessionCaptureCallbackDelegate: JDWCameraCaptureSessionCaptureCallbackDelegate;
    FCaptureSessionStateCallback: JDWCameraCaptureSessionStateCallback;
    FCaptureSessionStateCallbackDelegate: JDWCameraCaptureSessionStateCallbackDelegate;
    FContinuousImageAvailableListener: JImageReader_OnImageAvailableListener;
    FIsCapturing: Boolean;
    FIsStarting: Boolean;
    FHandler: JHandler;
    FPlatformCamera: TPlatformCamera;
    FPreview: TCameraPreview;
    FPreviewRequestBuilder: JCaptureRequest_Builder;
    FPreviewSurface: JSurface;
    FRequestedOrientation: Integer;
    FRequestHelper: JDWCaptureRequestBuilderHelper;
    FSession: JCameraCaptureSession;
    FStillImageAvailableListener: JImageReader_OnImageAvailableListener;
    FStillImageReader: JImageReader;
    FStillImageOrientation: Integer;
    FSurfaceTexture: JSurfaceTexture;
    FSurfaceTextureListener: JTextureView_SurfaceTextureListener;
    FThread: JHandlerThread;
    procedure AddMetadata(const AStream: TMemoryStream);
    procedure CaptureCompleted(session: JCameraCaptureSession; request: JCaptureRequest; result: JTotalCaptureResult); virtual;
    procedure CaptureProgressed(session: JCameraCaptureSession; request: JCaptureRequest; partialResult: JCaptureResult); virtual;
    procedure CaptureSessionConfigured(session: JCameraCaptureSession); virtual;
    procedure CaptureSessionConfigureFailed(session: JCameraCaptureSession); virtual;
    procedure CheckFaces(const AHelper: JDWCaptureResultHelper);
    procedure CreateStillReader;
    function GetSurfaceRotation(const ARotation: Integer): Integer;
    function GetOrientation(const ARotation: Integer): Integer;
    function GetPreviewControl: TControl;
    procedure InternalStartSession;
    procedure OrientationChangeHandler(Sender: TObject);
    procedure SizeChangeHandler(Sender: TObject);
    procedure StartThread;
    procedure StopThread;
    procedure UpdatePreview;
    procedure UpdatePreviewRequest;
  protected
    procedure CameraSettingChanged;
    procedure CaptureStillImage;
    procedure ContinuousImageAvailableHandler(const reader: JImageReader);
    procedure StartSession;
    procedure StillImageAvailableHandler(const reader: JImageReader);
    procedure StopSession;
    procedure SurfaceTextureAvailable(texture: JSurfaceTexture);
    // procedure SurfaceTextureAvailable(texture: JSurfaceTexture; width: Integer; height: Integer);
    procedure SurfaceTextureDestroyed(texture: JSurfaceTexture);
    property Handler: JHandler read FHandler;
    property IsCapturing: Boolean read FIsCapturing;
    property PreviewControl: TControl read GetPreviewControl;
    property PlatformCamera: TPlatformCamera read FPlatformCamera;
    property Session: JCameraCaptureSession read FSession;
  public
    constructor Create(const APlatformCamera: TPlatformCamera);
    destructor Destroy; override;
  end;

  TIntegerRange = record
    Lower: Integer;
    Upper: Integer;
  end;

  TInt64Range = record
    Lower: Int64;
    Upper: Int64;
  end;

  TPlatformCamera = class(TCustomPlatformCamera)
  private
    FAvailableViewSizes: TJavaObjectArray<Jutil_Size>;
    FCameraDevice: JCameraDevice;
    FCameraManager: JCameraManager;
    FCameraOrientation: Integer;
    FCaptureSession: TCameraCaptureSession;
    FDetectionDateTime: TDateTime;
    FFaces: TFaces;
    FFacesDetected: Boolean;
    FDeviceStateCallback: JDWCameraDeviceStateCallback;
    FDeviceStateCallbackDelegate: JDWCameraDeviceStateCallbackDelegate;
    FHandler: JHandler;
    FSensorExposureTimeRange: TInt64Range;
    FSensorSensitivityRange: TIntegerRange;
    FViewSize: Jutil_Size;
    procedure CheckBarcode(const frame: JBitmap);
    procedure DoOpenCamera;
    function GetExposureTime: Int64;
    function GetISO: Integer;
    function GetIsSwapping: Boolean;
    procedure UpdateViewSize;
  protected
    procedure CameraDisconnected(camera: JCameraDevice);
    procedure CameraError(camera: JCameraDevice; error: Integer);
    procedure CameraOpened(camera: JCameraDevice);
    procedure CameraSettingChanged; override;
    procedure CaptureStateChanged;
    procedure CapturedStillImage(const AImageStream: TStream);
    procedure CloseCamera; override;
    procedure ContinuousCaptureChanged; override;
    procedure DetectedFaces(const AFaces: TJavaObjectArray<JFace>);
    procedure DoCaptureImage; override;
    procedure FrameAvailable(frame: JBitmap);
    function GetCameraOrientation: Integer; override; //!!!!
    function GetHighestFaceDetectMode: TFaceDetectMode;
    function GetPreviewControl: TControl; override;
    function GetResolutionHeight: Integer; override;
    function GetResolutionWidth: Integer; override;
    procedure OpenCamera; override;
    procedure RequestPermission; override;
    procedure StartCapture; override;
    procedure StopCapture; override;
    procedure StillCaptureFailed;
    function SizeFitsInPreview(const ASize: Jutil_Size): Boolean;
    property CameraDevice: JCameraDevice read FCameraDevice;
    property CameraOrientation: Integer read FCameraOrientation;
    property ExposureTime: Int64 read GetExposureTime;
    property ISO: Integer read GetISO;
    property IsSwapping: Boolean read GetIsSwapping;
    property ViewSize: Jutil_Size read FViewSize;
  public
    constructor Create(const ACamera: TCamera); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Types, System.DateUtils, System.Math, System.Permissions, System.IOUtils, System.Sensors,
  // Android
  Androidapi.Helpers, Androidapi.JNI, Androidapi.JNI.App, Androidapi.JNI.JavaTypes,
  // FMX
  FMX.Forms, FMX.Media,
  // DW
  DW.OSLog,
  DW.CameraPreview.Android, DW.Android.Helpers, DW.Consts.Android, DW.UIHelper, DW.Types, DW.Permissions.Helpers, DW.Graphics.Helpers.Android; // ,
  // DW.Barcode.Types, DW.BarcodeHelper.Android, DW.Androidapi.JNI.VisionBarcode;

type
  TDWGLCameraViewDelegate = class(TJavaLocal, JDWGLCameraView_ViewDelegate)
  private
    FCaptureSession: TCameraCaptureSession;
  public
    { JDWGLCameraView_ViewDelegate }
    procedure onFrameAvailable(frame: JBitmap); cdecl;
    procedure onSurfaceTextureAvailable(texture: JSurfaceTexture); cdecl;
  public
    constructor Create(const ACaptureSession: TCameraCaptureSession);
  end;

  TDWCameraDeviceStateCallbackDelegate = class(TJavaLocal, JDWCameraDeviceStateCallbackDelegate)
  private
    FPlatformCamera: TPlatformCamera;
  public
    { JDWCameraDeviceStateCallbackDelegate }
    procedure onDisconnected(camera: JCameraDevice); cdecl;
    procedure onError(camera: JCameraDevice; error: Integer); cdecl;
    procedure onOpened(camera: JCameraDevice); cdecl;
  public
    constructor Create(const APlatformCamera: TPlatformCamera);
  end;

  TDWCameraCaptureSessionCaptureCallbackDelegate = class(TJavaLocal, JDWCameraCaptureSessionCaptureCallbackDelegate)
  private
    FCaptureSession: TCameraCaptureSession;
  public
    { JDWCameraCaptureSessionCaptureCallbackDelegate }
    procedure onCaptureProgressed(session: JCameraCaptureSession; request: JCaptureRequest; partialResult: JCaptureResult); cdecl;
    procedure onCaptureCompleted(session: JCameraCaptureSession; request: JCaptureRequest; result: JTotalCaptureResult); cdecl;
  public
    constructor Create(const ACaptureSession: TCameraCaptureSession);
  end;

  TDWCameraCaptureSessionStateCallbackDelegate = class(TJavaLocal, JDWCameraCaptureSessionStateCallbackDelegate)
  private
    FCaptureSession: TCameraCaptureSession;
  public
    { JDWCameraCaptureSessionStateCallbackDelegate }
    procedure onConfigureFailed(session: JCameraCaptureSession); cdecl;
    procedure onConfigured(session: JCameraCaptureSession); cdecl;
  public
    constructor Create(const ACaptureSession: TCameraCaptureSession);
  end;

  TImageAvailableProc = procedure(const reader: JImageReader) of object;

  TImageAvailableListener = class(TJavaLocal, JImageReader_OnImageAvailableListener)
  private
    FImageAvailableProc: TImageAvailableProc;
  public
    { JImageReader_OnImageAvailableListener }
    procedure onImageAvailable(reader: JImageReader); cdecl;
  public
    constructor Create(const AProc: TImageAvailableProc);
  end;

  TSurfaceTextureListener = class(TJavaLocal, JTextureView_SurfaceTextureListener)
  private
    FCaptureSession: TCameraCaptureSession;
    // FImageType: TImageType;
  public
    { JTextureView_SurfaceTextureListener }
    procedure onSurfaceTextureAvailable(texture: JSurfaceTexture; width, height: Integer); cdecl;
    function onSurfaceTextureDestroyed(texture: JSurfaceTexture): Boolean; cdecl;
    procedure onSurfaceTextureUpdated(texture: JSurfaceTexture); cdecl;
    procedure onSurfaceTextureSizeChanged(texture: JSurfaceTexture; width: Integer; height: Integer); cdecl;
  public
    constructor Create(const ACaptureSession: TCameraCaptureSession);
  end;

function GetSizeArea(const ASize: Jutil_Size): Integer;
begin
  Result := ASize.getHeight * ASize.getWidth;
end;

function GetImageFormat(const AFormat: Integer): string;
begin
  Result := 'Unknown';
  if AFormat = TJImageFormat.JavaClass.JPEG then
    Result := 'JPEG'
  else if AFormat = TJImageFormat.JavaClass.YUV_420_888 then
    Result := 'YUV_420_888';
end;

function ValueToDegrees(const AValue: Double): string;
var
  LValue: Double;
  LDegrees, LMinutes: Integer;
begin
  LValue := Abs(AValue);
  LDegrees := Trunc(LValue);
  LValue := (LValue - LDegrees) * 60;
  LMinutes := Trunc(LValue);
  LValue := (LValue - LMinutes) * 60;
  Result := Format('%d/1,%d/1,%d/1000', [LDegrees, LMinutes, Round(LValue)]);
end;

{ TDWGLCameraViewDelegate }

constructor TDWGLCameraViewDelegate.Create(const ACaptureSession: TCameraCaptureSession);
begin
  inherited Create;
  FCaptureSession := ACaptureSession;
end;

procedure TDWGLCameraViewDelegate.onFrameAvailable(frame: JBitmap);
begin
  FCaptureSession.PlatformCamera.FrameAvailable(frame);
end;

procedure TDWGLCameraViewDelegate.onSurfaceTextureAvailable(texture: JSurfaceTexture);
begin
  FCaptureSession.SurfaceTextureAvailable(texture);
end;

{ TDWCameraDeviceStateCallbackDelegate }

constructor TDWCameraDeviceStateCallbackDelegate.Create(const APlatformCamera: TPlatformCamera);
begin
  inherited Create;
  FPlatformCamera := APlatformCamera;
end;

procedure TDWCameraDeviceStateCallbackDelegate.onDisconnected(camera: JCameraDevice);
begin
  FPlatformCamera.CameraDisconnected(camera);
end;

procedure TDWCameraDeviceStateCallbackDelegate.onError(camera: JCameraDevice; error: Integer);
begin
  FPlatformCamera.CameraError(camera, error);
end;

procedure TDWCameraDeviceStateCallbackDelegate.onOpened(camera: JCameraDevice);
begin
  FPlatformCamera.CameraOpened(camera);
end;

{ TDWCameraCaptureSessionCaptureCallbackDelegate }

constructor TDWCameraCaptureSessionCaptureCallbackDelegate.Create(const ACaptureSession: TCameraCaptureSession);
begin
  inherited Create;
  FCaptureSession := ACaptureSession;
end;

procedure TDWCameraCaptureSessionCaptureCallbackDelegate.onCaptureCompleted(session: JCameraCaptureSession; request: JCaptureRequest;
  result: JTotalCaptureResult);
begin
  FCaptureSession.CaptureCompleted(session, request, result);
end;

procedure TDWCameraCaptureSessionCaptureCallbackDelegate.onCaptureProgressed(session: JCameraCaptureSession; request: JCaptureRequest;
  partialResult: JCaptureResult);
begin
  FCaptureSession.CaptureProgressed(session, request, partialResult);
end;

{ TDWCameraCaptureSessionStateCallbackDelegate }

constructor TDWCameraCaptureSessionStateCallbackDelegate.Create(const ACaptureSession: TCameraCaptureSession);
begin
  inherited Create;
  FCaptureSession := ACaptureSession;
end;

procedure TDWCameraCaptureSessionStateCallbackDelegate.onConfigured(session: JCameraCaptureSession);
begin
  TOSLog.d('+TDWCameraCaptureSessionStateCallbackDelegate.onConfigured');
  FCaptureSession.CaptureSessionConfigured(session);
  TOSLog.d('-TDWCameraCaptureSessionStateCallbackDelegate.onConfigured');
end;

procedure TDWCameraCaptureSessionStateCallbackDelegate.onConfigureFailed(session: JCameraCaptureSession);
begin
  FCaptureSession.CaptureSessionConfigureFailed(session);
end;

{ TImageAvailableListener }

constructor TImageAvailableListener.Create(const AProc: TImageAvailableProc);
begin
  inherited Create;
  FImageAvailableProc := AProc;
end;

procedure TImageAvailableListener.onImageAvailable(reader: JImageReader);
begin
  FImageAvailableProc(reader);
end;

{ TSurfaceTextureListener }

constructor TSurfaceTextureListener.Create(const ACaptureSession: TCameraCaptureSession);
begin
  inherited Create;
  FCaptureSession := ACaptureSession;
end;

procedure TSurfaceTextureListener.onSurfaceTextureAvailable(texture: JSurfaceTexture; width, height: Integer);
begin
  FCaptureSession.SurfaceTextureAvailable(texture);
end;

function TSurfaceTextureListener.onSurfaceTextureDestroyed(texture: JSurfaceTexture): Boolean;
begin
  FCaptureSession.SurfaceTextureDestroyed(texture);
  Result := True;
end;

procedure TSurfaceTextureListener.onSurfaceTextureSizeChanged(texture: JSurfaceTexture; width: Integer; height: Integer);
begin
  //
end;

procedure TSurfaceTextureListener.onSurfaceTextureUpdated(texture: JSurfaceTexture);
begin
  //
end;

{ TCameraCaptureSession }

constructor TCameraCaptureSession.Create(const APlatformCamera: TPlatformCamera);
begin
  inherited Create;
  FPlatformCamera := APlatformCamera;
  FRequestHelper := TJDWCaptureRequestBuilderHelper.JavaClass.init;
  FCaptureSessionCaptureCallbackDelegate := TDWCameraCaptureSessionCaptureCallbackDelegate.Create(Self);
  FCaptureSessionCaptureCallback := TJDWCameraCaptureSessionCaptureCallback.JavaClass.init(FCaptureSessionCaptureCallbackDelegate);
  FCaptureSessionStateCallbackDelegate := TDWCameraCaptureSessionStateCallbackDelegate.Create(Self);
  FCaptureSessionStateCallback := TJDWCameraCaptureSessionStateCallback.JavaClass.init(FCaptureSessionStateCallbackDelegate);
  FPreview := TCameraPreview.Create(nil);
  FPreview.Align := TAlignLayout.Center;
  FPreview.OnOrientationChange := OrientationChangeHandler;
  FPreview.OnSizeChange := SizeChangeHandler;
  FSurfaceTextureListener := TSurfaceTextureListener.Create(Self);

  // FCameraViewDelegate := TDWGLCameraViewDelegate.Create(Self);
  FCameraView := TAndroidCameraPreview(FPreview.Presentation).View;
  FCameraView.setSurfaceTextureListener(FSurfaceTextureListener);
  // FCameraView.setCaptureFPS(1); // Needs to be controlled from FPlatformCamera
  // FCameraView.setViewDelegate(FCameraViewDelegate);

  StartThread;
end;

destructor TCameraCaptureSession.Destroy;
begin
  FPreview.Free;
  StopThread;
  inherited;
end;

// Based on the question/answers, here:
//   https://stackoverflow.com/questions/48406497/camera2-understanding-the-sensor-and-device-orientations
function TCameraCaptureSession.GetOrientation(const ARotation: Integer): Integer;
var
  LRotation: Integer;
begin
  LRotation := GetSurfaceRotation(ARotation);
  TOSLog.d('> Surface Rotation: %d', [LRotation]);
  Result := (FPlatformCamera.CameraOrientation - LRotation + 360) mod 360;
end;

//   https://stackoverflow.com/questions/48406497/camera2-understanding-the-sensor-and-device-orientations
function TCameraCaptureSession.GetSurfaceRotation(const ARotation: Integer): Integer;
begin
  if ARotation = TJSurface.JavaClass.ROTATION_0 then
    Result := 0
  else if ARotation = TJSurface.JavaClass.ROTATION_90 then
    Result := 90
  else if ARotation = TJSurface.JavaClass.ROTATION_270 then
    Result := 180
  else if ARotation = TJSurface.JavaClass.ROTATION_180 then
    Result := 270
  else
    Result := 0;
end;

function TCameraCaptureSession.GetPreviewControl: TControl;
begin
  Result := FPreview;
end;

procedure TCameraCaptureSession.StartThread;
begin
  FThread := TJHandlerThread.JavaClass.init(StringToJString('CameraPreview'));
  FThread.start;
  FHandler := TJHandler.JavaClass.init(FThread.getLooper);
end;

procedure TCameraCaptureSession.StopThread;
begin
  FThread.quitSafely;
  FThread.join;
  FThread := nil;
  FHandler := nil;
end;

procedure TCameraCaptureSession.SurfaceTextureAvailable(texture: JSurfaceTexture); // ; width, height: Integer);
begin
  FPreviewSurface := nil;
  FSurfaceTexture := texture;
  FSurfaceTexture.setDefaultBufferSize(PlatformCamera.ViewSize.getWidth, PlatformCamera.ViewSize.getHeight);
  FPreviewSurface := TJSurface.JavaClass.init(FSurfaceTexture);
//  UpdatePreview;
  TThread.Synchronize(nil, UpdatePreview); // SurfaceTextureAvailable is coming from the view's render thread
  if FIsStarting then
    InternalStartSession;
end;

procedure TCameraCaptureSession.SurfaceTextureDestroyed(texture: JSurfaceTexture);
begin
  FSurfaceTexture := nil;
end;

procedure TCameraCaptureSession.OrientationChangeHandler(Sender: TObject);
begin
  //
end;

procedure TCameraCaptureSession.SizeChangeHandler(Sender: TObject);
begin
  if FIsCapturing then
    UpdatePreview;
end;

procedure TCameraCaptureSession.UpdatePreview;
var
  LScale, LScreenScale: Single;
  LSize: TSizeF;
  LViewSize, LPreviewSize: TSize;
  LIsPortrait: Boolean;
begin
  LIsPortrait := Screen.Height > Screen.Width;
  if LIsPortrait then
    LViewSize := TSize.Create(PlatformCamera.ViewSize.getWidth, PlatformCamera.ViewSize.getHeight)
  else
    LViewSize := TSize.Create(PlatformCamera.ViewSize.getHeight, PlatformCamera.ViewSize.getWidth);
  if LIsPortrait then
    LSize := TSizeF.Create(FPreview.ParentControl.Height * (LViewSize.cy / LViewSize.cx), FPreview.ParentControl.Height)
  else
    LSize := TSizeF.Create(FPreview.ParentControl.Width, FPreview.ParentControl.Width * (LViewSize.cx / LViewSize.cy));
  FPreview.Size.Size := LSize;
  // FCameraView.setCameraRotation(GetSurfaceRotation(TAndroidHelper.Activity.getWindowManager.getDefaultDisplay.getRotation));
  LScreenScale := TAndroidCameraPreview(FPreview.Presentation).ScreenScale;
  LPreviewSize := TSize.Create(Round(FPreview.Size.Size.cx * LScreenScale), Round(FPreview.Size.Size.cy * LScreenScale));
  FCameraView.setPreviewSize(TJutil_Size.JavaClass.init(LPreviewSize.cx, LPreviewSize.cy));
end;

procedure TCameraCaptureSession.CreateStillReader;
begin
  FStillImageAvailableListener := nil;
  FStillImageAvailableListener := TImageAvailableListener.Create(StillImageAvailableHandler);
  FStillImageReader := nil;
  TOSLog.d('Creating a still reader with dimensions of %d x %d', [PlatformCamera.ViewSize.getWidth, PlatformCamera.ViewSize.getHeight]);
  FStillImageReader := TJImageReader.JavaClass.newInstance(PlatformCamera.ViewSize.getWidth, PlatformCamera.ViewSize.getHeight,
    TJImageFormat.JavaClass.JPEG, 1);
  FStillImageReader.setOnImageAvailableListener(FStillImageAvailableListener, Handler);
end;

procedure TCameraCaptureSession.InternalStartSession;
var
  LOutputs: JArrayList;
begin
  FIsStarting := False;
  LOutputs := TJArrayList.JavaClass.init(2);
  LOutputs.add(FPreviewSurface);
  LOutputs.add(FStillImageReader.getSurface); // will not need this if GLSurfaceView can provide image via onFrameReady
  TOSLog.d('Creating capture session');
  FPlatformCamera.CameraDevice.createCaptureSession(TJList.Wrap(LOutputs), FCaptureSessionStateCallback, FHandler);
  TOSLog.d('Created capture session');
end;

procedure TCameraCaptureSession.StartSession;
begin
  if FPreview.ParentControl <> nil then
  begin
    FPreview.Visible := True;
    CreateStillReader;
    if FSurfaceTexture <> nil then
      InternalStartSession
    else
      FIsStarting := True;
  end;
end;

procedure TCameraCaptureSession.StopSession;
begin
  if FSession <> nil then
  begin
    FSession.close;
    FSession := nil;
  end;
  FPreviewRequestBuilder := nil;
  if not FPlatformCamera.IsSwapping then
    FPreview.Visible := False;
  FIsCapturing := False;
  FPlatformCamera.CaptureStateChanged;
end;

procedure TCameraCaptureSession.CaptureStillImage;
var
  LBuilder: JCaptureRequest_Builder;
  LRequestHelper: JDWCaptureRequestBuilderHelper;
begin
  LBuilder := FPlatformCamera.CameraDevice.createCaptureRequest(TJCameraDevice.JavaClass.TEMPLATE_STILL_CAPTURE);
  LBuilder.addTarget(FStillImageReader.getSurface);
  LRequestHelper := TJDWCaptureRequestBuilderHelper.JavaClass.init;
  LRequestHelper.setCaptureRequestBuilder(LBuilder);
  FRequestedOrientation := GetOrientation(TAndroidHelper.Activity.getWindowManager.getDefaultDisplay.getRotation);
  LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.JPEG_ORIENTATION, FRequestedOrientation);
  case FPlatformCamera.FlashMode of
    TFlashMode.FlashOff:
    begin
      LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_AE_MODE, TJCameraMetadata.JavaClass.CONTROL_AE_MODE_ON);
      LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.FLASH_MODE, TJCaptureRequest.JavaClass.FLASH_MODE_OFF);
    end;
    TFlashMode.FlashOn:
    begin
      LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_MODE, TJCameraMetadata.JavaClass.CONTROL_MODE_AUTO);
      LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_AE_MODE, TJCameraMetadata.JavaClass.CONTROL_AE_MODE_ON_ALWAYS_FLASH);
    end;
    TFlashMode.AutoFlash:
    begin
      LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_MODE, TJCameraMetadata.JavaClass.CONTROL_MODE_AUTO);
      LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_AE_MODE, TJCameraMetadata.JavaClass.CONTROL_AE_MODE_ON_AUTO_FLASH);
    end;
  end;
  if FPlatformCamera.ISO > -1 then
  begin
    LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_AE_MODE, TJCaptureRequest.JavaClass.CONTROL_AE_MODE_OFF);
    LRequestHelper.setLongValue(TJCaptureRequest.JavaClass.SENSOR_EXPOSURE_TIME, FPlatformCamera.ExposureTime);
    LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.SENSOR_SENSITIVITY, FPlatformCamera.ISO);
  end
  else
    LRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_AE_MODE, TJCaptureRequest.JavaClass.CONTROL_AE_MODE_ON);
  FSession.capture(LBuilder.build, FCaptureSessionCaptureCallback, FHandler);
end;

procedure TCameraCaptureSession.AddMetadata(const AStream: TMemoryStream);
const
  cRefLatitude: array[Boolean] of string = ('N', 'S');
  cRefLongitude: array[Boolean] of string = ('W', 'E');
var
  LFileName: string;
  LEXIF: JExifInterface;
  LLocation: TLocationCoord2D;
begin
  LLocation := PlatformCamera.Camera.Location;
  LFileName := TPath.GetTempFileName;
  AStream.SaveToFile(LFileName);
  LEXIF := TJExifInterface.JavaClass.init(StringToJString(LFileName));
  if TMetadataOption.GPS in FPlatformCamera.MetadataOptions then
  begin
    LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_LATITUDE, StringToJString(ValueToDegrees(LLocation.Latitude)));
    LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_LATITUDE_REF, StringToJString(cRefLatitude[LLocation.Latitude < 0]));
    LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_LONGITUDE, StringToJString(ValueToDegrees(LLocation.Longitude)));
    LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_LONGITUDE_REF, StringToJString(cRefLongitude[LLocation.Longitude < 0]));
  end;
  if TMetadataOption.Orientation in FPlatformCamera.MetadataOptions then
    LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_ORIENTATION, StringToJString(TJExifInterface.JavaClass.ORIENTATION_NORMAL.ToString));
  LEXIF.saveAttributes;
  AStream.LoadFromFile(LFileName);
  TFile.Delete(LFileName);
end;

procedure TCameraCaptureSession.ContinuousImageAvailableHandler(const reader: JImageReader);
begin
  //
end;

procedure TCameraCaptureSession.StillImageAvailableHandler(const reader: JImageReader);
var
  LImage: JImage;
  LStream: TMemoryStream;
  LRotation: Integer;
  LBytes: TBytes;
begin
  // From: http://stackoverflow.com/questions/41775968/how-to-convert-android-media-image-to-bitmap-object
  LImage := reader.acquireNextImage;
  try
    TOSLog.d('Image available for still request in %s format, size: %d x %d', [GetImageFormat(LImage.getFormat), LImage.getWidth, LImage.getHeight]);
    LRotation := 0;
//      if ((PlatformCamera.CameraOrientation mod 180) <> 0) and (PlatformCamera.ViewSize.getWidth <> LImage.getHeight) then
//        LRotation := FRequestedOrientation;
    LBytes := TJImageHelper.JImageToBytes(LImage, LRotation); // <---- This is where it can be slow, especially if it needs to rotate
  finally
    LImage.close;
  end;
  LStream := TMemoryStream.Create;
  try
    LStream.Write(LBytes, Length(LBytes));
    if PlatformCamera.Camera.MetadataOptions <> [] then
      AddMetadata(LStream);
    PlatformCamera.CapturedStillImage(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TCameraCaptureSession.CheckFaces(const AHelper: JDWCaptureResultHelper);
var
  LFaces: TJavaObjectArray<JFace>;
begin
  LFaces := AHelper.getFaces;
  if (LFaces <> nil) and (LFaces.Length > 0) then
    PlatformCamera.DetectedFaces(LFaces);
end;

procedure TCameraCaptureSession.CaptureCompleted(session: JCameraCaptureSession; request: JCaptureRequest; result: JTotalCaptureResult);
var
  LHelper: JDWCaptureResultHelper;
begin
  LHelper := TJDWCaptureResultHelper.JavaClass.init;
  LHelper.setCaptureResult(result);
  // Check what kind of CaptureRequest from request (eg still or continuous)
//  case FCaptureRequest of
//    TCaptureMode.Faces:
//      CheckFaces(LHelper);
//  end;
end;

procedure TCameraCaptureSession.CaptureProgressed(session: JCameraCaptureSession; request: JCaptureRequest; partialResult: JCaptureResult);
begin
  //
end;

procedure TCameraCaptureSession.CaptureSessionConfigured(session: JCameraCaptureSession);
begin
  FSession := session;
  FPreviewRequestBuilder := FPlatformCamera.CameraDevice.createCaptureRequest(TJCameraDevice.JavaClass.TEMPLATE_PREVIEW);
  FPreviewRequestBuilder.addTarget(FPreviewSurface);
  FRequestHelper.setCaptureRequestBuilder(FPreviewRequestBuilder);
  UpdatePreviewRequest;
end;

procedure TCameraCaptureSession.CaptureSessionConfigureFailed(session: JCameraCaptureSession);
begin
  //
end;

procedure TCameraCaptureSession.CameraSettingChanged;
begin
  if FIsCapturing then
    UpdatePreviewRequest;
end;

procedure TCameraCaptureSession.UpdatePreviewRequest;
begin
  if FPlatformCamera.ISO > -1 then
  begin
    FRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_AE_MODE, TJCameraMetadata.JavaClass.CONTROL_AE_MODE_OFF);
    FRequestHelper.setLongValue(TJCaptureRequest.JavaClass.SENSOR_EXPOSURE_TIME, FPlatformCamera.ExposureTime);
    FRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.SENSOR_SENSITIVITY, FPlatformCamera.ISO);
  end
  else
    FRequestHelper.setIntegerValue(TJCaptureRequest.JavaClass.CONTROL_AE_MODE, TJCameraMetadata.JavaClass.CONTROL_AE_MODE_ON);
  FRequestHelper.setFaceDetectMode(Ord(FPlatformCamera.GetHighestFaceDetectMode));
  FSession.setRepeatingRequest(FPreviewRequestBuilder.build, FCaptureSessionCaptureCallback, FHandler);
  FIsCapturing := True;
  FPlatformCamera.CaptureStateChanged;
end;

{ TPlatformCamera }

constructor TPlatformCamera.Create(const ACamera: TCamera);
begin
  inherited;
  FCameraManager := TJCameraManager.Wrap(TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.CAMERA_SERVICE));
  FDeviceStateCallbackDelegate := TDWCameraDeviceStateCallbackDelegate.Create(Self);
  FDeviceStateCallback := TJDWCameraDeviceStateCallback.JavaClass.init(FDeviceStateCallbackDelegate);
  FCaptureSession := TCameraCaptureSession.Create(Self);
  FHandler := TJHandler.JavaClass.init(TJLooper.JavaClass.getMainLooper);
end;

destructor TPlatformCamera.Destroy;
begin
  FDeviceStateCallbackDelegate := nil;
  FDeviceStateCallback := nil;
  FCaptureSession.Free;
  FHandler := nil;
  inherited;
end;

procedure TPlatformCamera.DetectedFaces(const AFaces: TJavaObjectArray<JFace>);
var
  I: Integer;
  LFace: JFace;
  LPoint: JPoint;
  LRect: JRect;
begin
  if not FFacesDetected and (MilliSecondsBetween(Now, FDetectionDateTime) > 1000) then
  begin
    FFacesDetected := True;
    FDetectionDateTime := Now;
    SetLength(FFaces, AFaces.Length);
    for I := 0 to AFaces.Length - 1 do
    begin
      LFace := AFaces.Items[I];
      LRect := LFace.getBounds;
      if LRect <> nil then
        FFaces[I].Bounds := TRectF.Create(LRect.left, LRect.top, LRect.right, LRect.bottom);
      LPoint := LFace.getLeftEyePosition;
      if LPoint <> nil then
        FFaces[I].LeftEyePosition := TPointF.Create(LPoint.x, LPoint.y);
      LPoint := LFace.getRightEyePosition;
      if LPoint <> nil then
        FFaces[I].RightEyePosition := TPointF.Create(LPoint.x, LPoint.y);
      LPoint := LFace.getMouthPosition;
      if LPoint <> nil then
        FFaces[I].MouthPosition := TPointF.Create(LPoint.x, LPoint.y);
    end;
    FCaptureSession.CaptureStillImage; //!!!!!
  end;
end;

procedure TPlatformCamera.DoCaptureImage;
begin
  FCaptureSession.CaptureStillImage;
end;

procedure TPlatformCamera.StillCaptureFailed;
begin
  FFacesDetected := False;
end;

function TPlatformCamera.GetHighestFaceDetectMode: TFaceDetectMode;
var
  LMode: TFaceDetectMode;
begin
  Result := TFaceDetectMode.None;
  for LMode := High(TFaceDetectMode) downto Low(TFaceDetectMode) do
  begin
    // Set the highest available mode, or none if none selected
    if (LMode in FAvailableFaceDetectModes) and (FaceDetectMode >= LMode) then
    begin
      // TOSLog.d('Highest face detect mode of: %d', [Ord(LMode)]);
      Result := LMode;
      Break;
    end;
  end;
end;

function TPlatformCamera.GetCameraOrientation: Integer;
begin
  Result := FCameraOrientation;
end;

function TPlatformCamera.GetExposureTime: Int64;
begin
  // Result := Round((0.2 * (FSensorExposureTimeRange.Upper - FSensorExposureTimeRange.Lower)) + FSensorExposureTimeRange.Lower);
  Result := Round(FSensorExposureTimeRange.Lower * 25.0);
  TOSLog.d('TPlatformCamera.GetExposureTime > Lower: %d, Upper: %d, Result: %d', [FSensorExposureTimeRange.Lower, FSensorExposureTimeRange.Upper, Result]);
end;

function TPlatformCamera.GetISO: Integer;
var
  LFactor: Single;
begin
  if Exposure > -1 then
  begin
    LFactor := Round(Exposure * 100) / 100;
    Result := Round((LFactor * (FSensorSensitivityRange.Upper - FSensorSensitivityRange.Lower)) + FSensorSensitivityRange.Lower);
  end
  else
    Result := -1;
end;

function TPlatformCamera.GetIsSwapping: Boolean;
begin
  Result := FIsSwapping;
end;

function TPlatformCamera.GetPreviewControl: TControl;
begin
  Result := FCaptureSession.PreviewControl;
end;

function TPlatformCamera.GetResolutionHeight: Integer;
begin
  if FViewSize <> nil then
    Result := FViewSize.getHeight
  else
    Result := 0;
end;

function TPlatformCamera.GetResolutionWidth: Integer;
begin
  if FViewSize <> nil then
    Result := FViewSize.getWidth
  else
    Result := 0;
end;

procedure TPlatformCamera.DoOpenCamera;
var
  LCameraIDList: TJavaObjectArray<JString>;
  LItem, LCameraID: JString;
  LLensFacing: Integer;
  LCharacteristics: JCameraCharacteristics;
  LHelper: JDWCameraCharacteristicsHelper;
  LMap: JStreamConfigurationMap;
  LFaceDetectModes: TJavaArray<Integer>;
  I: Integer;
begin
  TOSLog.d('+TPlatformCamera.DoOpenCamera');
  FSensorExposureTimeRange.Lower := 0;
  FSensorExposureTimeRange.Upper := 0;
  InternalSetExposure(0.15);
  FCameraOrientation := 0;
  LFaceDetectModes := nil;
  FViewSize := nil;
  LCameraIDList := FCameraManager.getCameraIdList;
  LCameraID := nil;
  LMap := nil;
  LHelper := TJDWCameraCharacteristicsHelper.JavaClass.init;
  for I := 0 to LCameraIDList.Length - 1 do
  begin
    TOSLog.d('> Found %d cameras', [LCameraIDList.Length ]);
    LItem := LCameraIDList.Items[I];
	  LCharacteristics := FCameraManager.getCameraCharacteristics(LItem);
    LHelper.setCameraCharacteristics(LCharacteristics);
    LLensFacing := LHelper.getLensFacing;
    case CameraPosition of
      TDevicePosition.Back:
      begin
        if LLensFacing = TJCameraMetadata.JavaClass.LENS_FACING_BACK then
        begin
          TOSLog.d('> Found BACK facing camera');
          LCameraID := LItem;
        end;
      end;
      TDevicePosition.Front:
      begin
        if LLensFacing = TJCameraMetadata.JavaClass.LENS_FACING_FRONT then
        begin
          TOSLog.d('> Found FRONT facing camera');
          LCameraID := LItem;
        end;
      end;
    end;
    if LCameraID <> nil then
    begin
      FCameraOrientation := LHelper.getSensorOrientation;
      TOSLog.d('> Camera orientation: %d', [FCameraOrientation]);
      LMap := LHelper.getMap;
      Break;
    end;
  end;
  if (LCameraID = nil) or (LMap = nil) then
  begin
    TOSLog.d('> No cameras available?');
    Exit; // <======
  end;
  TOSLog.d('> Obtained ID and map');
  LCharacteristics := FCameraManager.getCameraCharacteristics(LCameraID);
  LHelper.setCameraCharacteristics(LCharacteristics);
  FSensorExposureTimeRange.Lower := LHelper.getSensorExposureTimeLower;
  FSensorExposureTimeRange.Upper := LHelper.getSensorExposureTimeUpper;
  FSensorSensitivityRange.Lower := LHelper.getSensorSensitivityLower;
  FSensorSensitivityRange.Upper := LHelper.getSensorSensitivityUpper;
  FAvailableFaceDetectModes := [];
  LFaceDetectModes := LHelper.getFaceDetectModes;
  if LFaceDetectModes <> nil then
  begin
    for I := 0 to LFaceDetectModes.Length - 1 do
      Include(FAvailableFaceDetectModes, TFaceDetectMode(LFaceDetectModes.Items[I]));
  end;
  // May need to rethink this - largest preview size may not be appropriate, except for stills
  FAvailableViewSizes := LMap.getOutputSizes(TJImageFormat.JavaClass.RAW_SENSOR);
  if FAvailableViewSizes = nil then
    FAvailableViewSizes := LMap.getOutputSizes(TJImageFormat.JavaClass.JPEG);
  if FAvailableViewSizes <> nil then
  begin
    UpdateViewSize;
    TOSLog.d('> openCamera');
    FCameraManager.openCamera(LCameraID, FDeviceStateCallback, FHandler);
  end
  else
    TOSLog.d('> No view sizes available');
  TOSLog.d('-TPlatformCamera.DoOpenCamera');
end;

procedure TPlatformCamera.OpenCamera;
begin
  DoOpenCamera;
end;

procedure TPlatformCamera.RequestPermission;
const
  cStatus: array[Boolean] of TAuthorizationStatus = (TAuthorizationStatus.Denied, TAuthorizationStatus.Authorized);
begin
  PermissionsService.RequestPermissions([cPermissionCamera],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      QueueAuthorizationStatus(cStatus[AGrantResults[0] = TPermissionStatus.Granted]);
    end
  );
end;

procedure TPlatformCamera.CloseCamera;
begin
  StopCapture;
  if FCameraDevice <> nil then
    FCameraDevice.close;
  FCameraDevice := nil;
  FAvailableFaceDetectModes := [];
  SetFaceDetectMode(TFaceDetectMode.None);
  InternalSetActive(False);
end;

procedure TPlatformCamera.ContinuousCaptureChanged;
begin
  // Create/Free repeating capture request
//  if IsActive and ContinuousCapture then
//    FCaptureTimer.Enabled := True
//  else
//    FCaptureTimer.Enabled := False;
end;

procedure TPlatformCamera.UpdateViewSize;
var
  I: Integer;
  LSize: Jutil_Size;
begin
  // Find the maximum view size
  FViewSize := nil;
  for I := 0 to FAvailableViewSizes.Length - 1 do
  begin
    LSize := FAvailableViewSizes.Items[I];
    // TOSLog.d('View size: %d x %d', [LSize.getWidth, LSize.getHeight]);
    if ((FViewSize = nil) or (GetSizeArea(LSize) > GetSizeArea(FViewSize))) then
      FViewSize := LSize;
  end;
  if (FViewSize <> nil) and ((FCameraOrientation mod 180) <> 0) then
    FViewSize := TJutil_Size.JavaClass.init(FViewSize.getWidth, FViewSize.getHeight);
end;

function TPlatformCamera.SizeFitsInPreview(const ASize: Jutil_Size): Boolean;
begin
  Result := True;
end;

procedure TPlatformCamera.StartCapture;
begin
  if FCaptureSession <> nil then
    FCaptureSession.StartSession;
end;

procedure TPlatformCamera.CaptureStateChanged;
begin
  FIsCapturing := FCaptureSession.IsCapturing;
  TThread.Synchronize(nil, DoStatusChange);
end;

procedure TPlatformCamera.StopCapture;
begin
  FCaptureSession.StopSession;
  FIsCapturing := False;
end;

procedure TPlatformCamera.CameraDisconnected(camera: JCameraDevice);
begin
  FCameraDevice := nil;
  FIsCapturing := False;
  InternalSetActive(False);
end;

procedure TPlatformCamera.CameraError(camera: JCameraDevice; error: Integer);
begin
  TOSLog.d('TPlatformCamera.CameraError - Error: %d', [error]);
end;

procedure TPlatformCamera.CameraOpened(camera: JCameraDevice);
begin
  FCameraDevice := camera;
  InternalSetActive(True);
  StartCapture;
end;

procedure TPlatformCamera.CameraSettingChanged;
begin
  FCaptureSession.CameraSettingChanged;
end;

procedure TPlatformCamera.CheckBarcode(const frame: JBitmap);
{
var
  LDetectorBuilder: JBarcodeDetector_Builder;
  LFrameBuilder: JFrame_Builder;
  LResults: JSparseArray;
  LJBarcode: JBarcode;
  LObject: JObject;
  I: Integer;
  LBarcode: TBarcode;
  LBarcodes: TBarcodes;
  LError: string;
}
begin
{
  LDetectorBuilder := TJBarcodeDetector_Builder.JavaClass.init(TAndroidHelper.Context);
  //!!!!!! Camera property: BarcodeFormats
  LDetectorBuilder.setBarcodeFormats(TJBarcode.JavaClass.ALL_FORMATS);
  LFrameBuilder := TJFrame_Builder.Create;
  LFrameBuilder.setBitmap(frame);
  LResults := LDetectorBuilder.build.detect(LFrameBuilder.build);
  for I := 0 to LResults.size - 1 do
  begin
    LJBarcode := TJBarcode.Wrap(LResults.get(LResults.keyAt(0)));
    LBarcode.Value := JStringToString(LJBarcode.displayValue);
    LBarcode.Format := TAndroidBarcodeHelper.GetBarcodeFormat(LJBarcode.format);
    LBarcodes := LBarcodes + [LBarcode];
  end;
  if LResults.size = 0 then
    LError := 'No barcodes detected'
  else
    LError := '';
  // DoBarcodes(LBarcodes, LError);
}
end;

procedure TPlatformCamera.FrameAvailable(frame: JBitmap);
{
var
  LBitmap: TBitmap;
begin
  // Could "build in" barcode detection?
  LBitmap := TBitmap.Create;
  try
    LBitmap.FromJBitmap(frame);
    DoFrameAvailable(LBitmap);
  finally
    LBitmap.Free;
  end;
end;
}
begin
  // Have this as an option
  // CheckBarcode(frame);
end;

procedure TPlatformCamera.CapturedStillImage(const AImageStream: TStream);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      // Change this into using Faces mode of the Camera object
//      case ACaptureRequest of
//        TCaptureMode.Still:
          DoCapturedImage(AImageStream);
//        TCaptureMode.Faces:
//          DoDetectedFaces(AImageStream, FFaces);
//      end;
    end
  );
  FFacesDetected := False;
end;

end.
