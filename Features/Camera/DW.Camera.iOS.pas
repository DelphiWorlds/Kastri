unit DW.Camera.iOS;

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
  // RTL
  System.TypInfo, System.Classes,
  // Mac
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.AVFoundation, iOSapi.QuartzCore, iOSapi.CoreMedia, iOSapi.UIKit, iOSapi.CoreVideo, iOSapi.CoreImage,
  // FMX
  FMX.Controls, FMX.Graphics,
  // DW
  DW.Camera, DW.CameraPreview, DW.CameraPreview.iOS, DW.iOSapi.Vision, DW.Types;

type
  // Filling in some blanks
  TCaptureStillImageCompletionHandler = procedure(buffer: CMSampleBufferRef; error: NSError) of object;

  AVCaptureStillImageOutput = interface(iOSapi.AVFoundation.AVCaptureStillImageOutput)
    ['{3A63B2C2-604B-4F24-977E-477126987D55}']
    procedure captureStillImageAsynchronouslyFromConnection(connection: AVCaptureConnection;
      completionHandler: TCaptureStillImageCompletionHandler); cdecl;
  end;
  TAVCaptureStillImageOutput = class(TOCGenericImport<AVCaptureStillImageOutputClass, AVCaptureStillImageOutput>)
  end;

  TFaceDetector = class(TObject)
  private
    FDetectionRequest: VNDetectFaceLandmarksRequest;
    FFaces: TFaces;
    FImage: CIImage;
    // FRequestHandler: VNImageRequestHandler;
    FOnDetectComplete: TNotifyEvent;
    procedure DetectionRequestCompletionHandler(request: VNRequest; error: NSError);
    procedure DoDetectComplete;
  public
    function DetectFaces(const AImage: CIImage): Boolean;
    property Faces: TFaces read FFaces;
    property Image: CIImage read FImage;
    property OnDetectComplete: TNotifyEvent read FOnDetectComplete write FOnDetectComplete;
  end;

  TPlatformCamera = class(TCustomPlatformCamera)
  private
    FDevice: AVCaptureDevice;
    FISODefault: Single;
    FPreview: TCameraPreview;
    FPreviewNative: TiOSCameraPreview;
    FSession: AVCaptureSession;
    FStillImageOutput: AVCaptureStillImageOutput;
    procedure CaptureStillImageCompletionHandler(buffer: CMSampleBufferRef; error: NSError);
    procedure CheckFaces(const AImage: CIImage);
    procedure FaceDetectorDetectCompleteHandler(Sender: TObject);
    function GetCorrectedImage(const AImage: CIImage): CIImage;
    function GetExifMetadata: NSDictionary;
    function GetImageData(const AImage: CIImage): NSData;
    function GetGPSMetadata: NSDictionary;
    function GetResolutionDimensions: CMVideoDimensions;
    function GetVideoAuthorizationStatus: TAuthorizationStatus;
    procedure PreviewOrientationChangeHandler(Sender: TObject);
    procedure RequestRecordVideoHandler(granted: Boolean);
    procedure UpdateDeviceModes;
    procedure UpdateExposureMode;
    procedure UpdateExposureValue;
    procedure UpdateFlashMode;
    procedure UpdateSession;
  protected
    class function GetDeviceRotation: Double;
  protected
    procedure CameraSettingChanged; override;
    function CanControlExposure: Boolean; override;
    procedure CloseCamera; override;
    procedure DoCaptureImage; override;
    function GetPreviewControl: TControl; override;
    function GetResolutionHeight: Integer; override;
    function GetResolutionWidth: Integer; override;
    procedure OpenCamera; override;
    procedure RequestPermission; override;
    procedure StartCapture; override;
    procedure StopCapture; override;
  public
    constructor Create(const ACamera: TCamera); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Types, System.SysUtils, System.Math,
  // Mac
  Macapi.Helpers, Macapi.ObjCRuntime, Macapi.CoreFoundation,
  // iOS
  iOSapi.CoreGraphics, iOSapi.Helpers, iOSapi.CocoaTypes,
  // DW
  DW.OSLog,
  {$IF CompilerVersion < 34}
  DW.iOSapi.AVFoundation,
  {$ENDIF}
  // FMX
  FMX.Forms, FMX.Platform.iOS, FMX.Media, FMX.Helpers.iOS, FMX.Types, FMX.Graphics.iOS;

const
  libImageIO = '/System/Library/Frameworks/ImageIO.framework/ImageIO';

  // Had to obtain these using Xcode - they're not exported?
  kCGImagePropertyOrientationUp = 1;
  kCGImagePropertyOrientationUpMirrored = 2;
  kCGImagePropertyOrientationDown = 3;
  kCGImagePropertyOrientationDownMirrored = 4;
  kCGImagePropertyOrientationLeftMirrored = 5;
  kCGImagePropertyOrientationRight = 6;
  kCGImagePropertyOrientationRightMirrored = 7;
  kCGImagePropertyOrientationLeft = 8;

function kCGImageDestinationLossyCompressionQuality: Pointer;
begin
  Result := CocoaPointerConst(libImageIO, 'kCGImageDestinationLossyCompressionQuality');
end;

function kCGImagePropertyExifDictionary: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyExifDictionary');
end;

function kCGImagePropertyGPSDictionary: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSDictionary');
end;

function kCGImagePropertyGPSLatitudeRef: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSLatitudeRef');
end;

function kCGImagePropertyOrientation: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyOrientation');
end;

function kCGImagePropertyGPSLatitude: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSLatitude');
end;

function kCGImagePropertyGPSLongitudeRef: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSLongitudeRef');
end;

function kCGImagePropertyGPSLongitude: NSString;
begin
  Result := CocoaNSStringConst(libImageIO, 'kCGImagePropertyGPSLongitude');
end;

function AVLayerVideoGravityResizeAspectFill: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVLayerVideoGravityResizeAspectFill');
end;

function AVVideoCodecKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVVideoCodecKey');
end;

function AVVideoCodecJPEG: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVVideoCodecJPEG');
end;

function AVCaptureISOCurrent: CGFloat;
begin
  Result := CocoaDoubleConst(libAVFoundation, 'AVCaptureISOCurrent');
end;

type
  NSObjectEx = interface(NSObject)
    ['{07DD60D7-65A5-4BA6-AF2B-F91C1360D8B8}']
    function copy: Pointer; cdecl;
  end;
  TNSObjectEx = class(TOCGenericImport<NSObjectClass, NSObjectEx>) end;

  CIImageEx = interface(iOSapi.CoreImage.CIImage)
    ['{83FA6D81-6FC7-4EED-AFDA-58B1965E1C99}']
    function imageByApplyingCGOrientation(orientation: CGImagePropertyOrientation): CIImage; cdecl;
    function imageBySettingProperties(properties: NSDictionary): CIImage; cdecl;
  end;
  TCIImageEx = class(TOCGenericImport<CIImageClass, CIImageEx>) end;

  CIContextEx = interface(iOSapi.CoreImage.CIContext)
    ['{2F25A037-50CA-4295-B776-2791EB35525C}']
    [MethodName('HEIFRepresentationOfImage:format:colorSpace:options:')]
    function HEIFRepresentationOfImage(image: CIImage; format: CIFormat; colorSpace: CGColorSpaceRef; options: NSDictionary): NSData; cdecl;
    [MethodName('JPEGRepresentationOfImage:colorSpace:options:')]
    function JPEGRepresentationOfImage(image: CIImage; colorSpace: CGColorSpaceRef; options: NSDictionary): NSData; cdecl;
    [MethodName('PNGRepresentationOfImage:format:colorSpace:options:')]
    function PNGRepresentationOfImage(image: CIImage; format: CIFormat; colorSpace: CGColorSpaceRef; options: NSDictionary): NSData; cdecl;
  end;
  TCIContextEx = class(TOCGenericImport<CIImageClass, CIContextEx>) end;

  TExposureModeCompletionHandler = procedure(syncTime: CMTime) of object;

  AVCaptureDeviceEx = interface(AVCaptureDevice)
    ['{3219C149-9640-4E2F-8B78-22DD4D3EC953}']
    function exposureDuration: CMTime; cdecl;
    procedure setExposureModeCustomWithDuration(duration: CMTime; ISO: Single; completionHandler: TExposureModeCompletionHandler); cdecl;
    function ISO: Single; cdecl;
  end;
  TAVCaptureDeviceEx = class(TOCGenericImport<AVCaptureDeviceClass, AVCaptureDeviceEx>) end;

  AVCaptureDeviceFormatEx = interface(AVCaptureDeviceFormat)
    ['{B0781EFB-06DC-4447-9ED2-7961366BC38A}']
    function maxISO: Single; cdecl;
    function minISO: Single; cdecl;
  end;
  TAVCaptureDeviceFormatEx = class(TOCGenericImport<AVCaptureDeviceFormatClass, AVCaptureDeviceFormatEx>) end;

{ TFaceDetector }

function TFaceDetector.DetectFaces(const AImage: CIImage): Boolean;
var
  LRequestHandler: VNImageRequestHandler;
  LRequests: NSArray;
  LErrorPtr: Pointer;
begin
  FImage := TCIImage.Wrap(TNSObjectEx.Wrap(NSObjectToID(AImage)).copy);
  FDetectionRequest := TVNDetectFaceLandmarksRequest.Wrap(TVNDetectFaceLandmarksRequest.OCClass.alloc);
  FDetectionRequest := TVNDetectFaceLandmarksRequest.Wrap(FDetectionRequest.initWithCompletionHandler(DetectionRequestCompletionHandler));
  LRequestHandler := TVNImageRequestHandler.Wrap(TVNImageRequestHandler.OCClass.alloc);
  LRequestHandler := TVNImageRequestHandler.Wrap(LRequestHandler.initWithCIImage(FImage, nil));
  LRequests := TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(NSObjectToID(FDetectionRequest)));
  Result := LRequestHandler.performRequests(LRequests, @LErrorPtr);
end;

procedure TFaceDetector.DetectionRequestCompletionHandler(request: VNRequest; error: NSError);
var
  I: Integer;
  LFaceObservation: VNFaceObservation;
  LFace: TFace;
  LSize: TPoint;
begin
  LSize.X := Round(FImage.extent.size.width);
  LSize.Y := Round(FImage.extent.size.height);
  for I := 0 to request.results.count - 1 do
  begin
    LFaceObservation := TVNFaceObservation.Wrap(request.results.objectAtIndex(I));
    LFace.Bounds := VNImageRectForNormalizedRect(LFaceObservation.boundingBox, LSize.X, LSize.Y).ToRectF;
    if LFaceObservation.faceCaptureQuality <> nil then
      LFace.Score := Round(LFaceObservation.faceCaptureQuality.floatValue * 100);
    LFace.LeftEyePosition := VNImagePointForNormalizedPoint(LFaceObservation.landmarks.leftEye.normalizedPoints^, LSize.X, LSize.Y).ToPointF;
    LFace.RightEyePosition := VNImagePointForNormalizedPoint(LFaceObservation.landmarks.rightEye.normalizedPoints^, LSize.X, LSize.Y).ToPointF;
    LFace.MouthPosition := VNImagePointForNormalizedPoint(LFaceObservation.landmarks.outerLips.normalizedPoints^, LSize.X, LSize.Y).ToPointF;
    FFaces := FFaces + [LFace];
  end;
  DoDetectComplete;
end;

procedure TFaceDetector.DoDetectComplete;
begin
  if Assigned(FOnDetectComplete) then
    FOnDetectComplete(Self);
end;

{ TPlatformCamera }

constructor TPlatformCamera.Create(const ACamera: TCamera);
begin
  inherited;
  FISODefault := -1;
  DoAuthorizationStatus(GetVideoAuthorizationStatus);
  FPreview := TCameraPreview.Create(nil);
  FPreview.Align := TAlignLayout.Contents;
  FPreview.OnOrientationChange := PreviewOrientationChangeHandler;
  FPreviewNative := TiOSCameraPreview(FPreview.Presentation);
end;

destructor TPlatformCamera.Destroy;
begin
  StopCapture;
  FPreview.Free;
  FSession := nil;
  inherited;
end;

function TPlatformCamera.GetGPSMetadata: NSDictionary;
const
  cLatRefs: array[Boolean] of string = ('S', 'N');
  cLngRefs: array[Boolean] of string = ('W', 'E');
var
  LGPS: NSMutableDictionary;
begin
  LGPS := TNSMutableDictionary.Create;
  LGPS.setObject(TNSNumber.OCClass.numberWithDouble(Abs(Camera.Location.Latitude)), NSObjectToID(kCGImagePropertyGPSLatitude));
  LGPS.setObject(StringToID(cLatRefs[Camera.Location.Latitude >= 0]), NSObjectToID(kCGImagePropertyGPSLatitudeRef));
  LGPS.setObject(TNSNumber.OCClass.numberWithDouble(Abs(Camera.Location.Longitude)), NSObjectToID(kCGImagePropertyGPSLongitude));
  LGPS.setObject(StringToID(cLngRefs[Camera.Location.Longitude >= 0]), NSObjectToID(kCGImagePropertyGPSLongitudeRef));
  Result := LGPS;
end;

function TPlatformCamera.GetExifMetadata: NSDictionary;
var
  LEXIF: NSMutableDictionary;
begin
  LEXIF := TNSMutableDictionary.Create;
  if TMetadataOption.Orientation in MetadataOptions then
    LEXIF.setObject(TNSNumber.OCClass.numberWithInt(kCGImagePropertyOrientationUp), NSObjectToID(kCGImagePropertyOrientation));
  Result := LEXIF;
end;

function TPlatformCamera.GetImageData(const AImage: CIImage): NSData;
var
  LCIContext: CIContextEx;
  LColorSpace: CGColorSpaceRef;
begin
  LCIContext := TCIContextEx.Wrap(TCIContext.OCClass.contextWithOptions(nil));
  LColorSpace := CGColorSpaceCreateDeviceRGB;
  try
    Result := LCIContext.JPEGRepresentationOfImage(AImage, LColorSpace, nil);
  finally
    CGColorSpaceRelease(LColorSpace);
  end;
end;

function TPlatformCamera.GetCorrectedImage(const AImage: CIImage): CIImage;
var
  LRotation: Double;
begin
  Result := AImage;
  // NOTE: The following will work ONLY on iOS 11+. If you need support for lower, please find an alternate algorithm
  if TOSVersion.Check(11) then
  begin
    LRotation := GetDeviceRotation;
    if LRotation = 90 then
      Result := TCIImageEx.Wrap(NSObjectToID(AImage)).imageByApplyingCGOrientation(kCGImagePropertyOrientationRight);
  end;
end;

procedure TPlatformCamera.DoCaptureImage;
var
  LTargetConnection: AVCaptureConnection;
begin
  LTargetConnection := FStillImageOutput.connectionWithMediaType(AVMediaTypeVideo);
  if LTargetConnection <> nil then
    FStillImageOutput.captureStillImageAsynchronouslyFromConnection(LTargetConnection, CaptureStillImageCompletionHandler);
end;

procedure TPlatformCamera.CameraSettingChanged;
begin
  if FDevice <> nil then
    UpdateDeviceModes;
end;

function TPlatformCamera.CanControlExposure: Boolean;
begin
  Result := True;
end;

procedure TPlatformCamera.CaptureStillImageCompletionHandler(buffer: CMSampleBufferRef; error: NSError);
var
  LImageBuffer: CVPixelBufferRef;
  LCIImage: CIImage;
  LStream: TMemoryStream;
  LData: NSData;
  LProperties: NSMutableDictionary;
begin
  LImageBuffer := CMSampleBufferGetImageBuffer(buffer);
  // https://stackoverflow.com/a/41829499/3164070
  CVBufferRetain(LImageBuffer);
  try
    LCIImage := GetCorrectedImage(TCIImage.Wrap(TCIImage.OCClass.imageWithCVPixelBuffer(LImageBuffer)));
  finally
    CVBufferRelease(LImageBuffer);
  end;
  // Check faces after rotation correction
  if FaceDetectMode <> TFaceDetectMode.None then
    CheckFaces(LCIImage);
  LProperties := TNSMutableDictionary.Create;
  LProperties.setDictionary(LCIImage.properties);
  if TMetadataOption.GPS in MetadataOptions then
    LProperties.setObject(NSObjectToID(GetGPSMetadata), NSObjectToID(kCGImagePropertyGPSDictionary));
  if TMetadataOption.Orientation in MetadataOptions then
    LProperties.setObject(NSObjectToID(GetExifMetadata), NSObjectToID(kCGImagePropertyExifDictionary));
  // **** Requires iOS 10+ ****
  LCIImage := TCIImageEx.Wrap(NSObjectToID(LCIImage)).imageBySettingProperties(LProperties);
  LData := GetImageData(LCIImage);
  LStream := TMemoryStream.Create;
  try
    LStream.Write(LData.bytes^, LData.length);
    TThread.Synchronize(nil,
      procedure
      begin
        DoCapturedImage(LStream);
      end
    );
  finally
    LStream.Free;
  end;
end;

procedure TPlatformCamera.CheckFaces(const AImage: CIImage);
var
  LFaceDetector: TFaceDetector;
begin
  LFaceDetector := TFaceDetector.Create;
  LFaceDetector.OnDetectComplete := FaceDetectorDetectCompleteHandler;
  LFaceDetector.DetectFaces(AImage);
end;

procedure TPlatformCamera.FaceDetectorDetectCompleteHandler(Sender: TObject);
var
  LFaceDetector: TFaceDetector;
  LStream: TMemoryStream;
  LData: NSData;
begin
  LFaceDetector := TFaceDetector(Sender);
  if Length(LFaceDetector.Faces) > 0 then
  begin
    LData := GetImageData(GetCorrectedImage(LFaceDetector.Image));
    LStream := TMemoryStream.Create;
    try
      LStream.Write(LData.bytes^, LData.length);
      TThread.Synchronize(nil,
        procedure
        begin
          DoDetectedFaces(LStream, LFaceDetector.Faces);
        end
      );
    finally
      LStream.Free;
    end;
  end;
  LFaceDetector.Free;
end;

procedure TPlatformCamera.PreviewOrientationChangeHandler(Sender: TObject);
begin
  if IsActive then
    FPreviewNative.UpdatePreview;
end;

function TPlatformCamera.GetVideoAuthorizationStatus: TAuthorizationStatus;
begin
  case TAVCaptureDevice.OCClass.authorizationStatusForMediaType(AVMediaTypeVideo) of
    AVAuthorizationStatusAuthorized:
      Result := TAuthorizationStatus.Authorized;
    AVAuthorizationStatusDenied:
      Result := TAuthorizationStatus.Denied;
    AVAuthorizationStatusRestricted:
      Result := TAuthorizationStatus.Restricted;
    AVAuthorizationStatusNotDetermined:
      Result := TAuthorizationStatus.NotDetermined;
  else
    Result := TAuthorizationStatus.NotDetermined;
  end;
end;

procedure TPlatformCamera.RequestPermission;
begin
  TAVCaptureDevice.OCClass.requestAccessForMediaType(AVMediaTypeVideo, RequestRecordVideoHandler);
end;

procedure TPlatformCamera.RequestRecordVideoHandler(granted: Boolean);
begin
  QueueAuthorizationStatus(GetVideoAuthorizationStatus);
end;

procedure TPlatformCamera.UpdateDeviceModes;
begin
  FDevice.lockForConfiguration(nil);
  try
    UpdateFlashMode;
    UpdateExposureMode;
  finally
    FDevice.unlockForConfiguration;
  end;
end;

procedure TPlatformCamera.UpdateExposureMode;
var
  LDevice: AVCaptureDeviceEx;
  LFormat: AVCaptureDeviceFormatEx;
  LISO: Single;
begin
  LDevice := TAVCaptureDeviceEx.Wrap(NSObjectToID(FDevice));
  if LDevice.activeFormat <> nil then
  begin
    LFormat := TAVCaptureDeviceFormatEx.Wrap(NSObjectToID(LDevice.activeFormat));
    if Exposure > -1 then
      LISO := LFormat.minISO + (Exposure * (LFormat.maxISO - LFormat.minISO))
    else
      LISO := FISODefault;
    if (LISO >= LFormat.minISO) and (LISO <= LFormat.maxISO) then
      LDevice.setExposureModeCustomWithDuration(LDevice.exposureDuration, LISO, nil);
  end;
end;

procedure TPlatformCamera.UpdateExposureValue;
var
  LDevice: AVCaptureDeviceEx;
  LFormat: AVCaptureDeviceFormatEx;
begin
  LDevice := TAVCaptureDeviceEx.Wrap(NSObjectToID(FDevice));
  if LDevice.activeFormat <> nil then
  begin
    LFormat := TAVCaptureDeviceFormatEx.Wrap(NSObjectToID(LDevice.activeFormat));
    if FISODefault = -1 then
      FISODefault := LDevice.ISO;
    InternalSetExposure((FISODefault - LFormat.minISO) / (LFormat.maxISO - LFormat.minISO));
  end;
end;

procedure TPlatformCamera.UpdateFlashMode;
begin
  if FDevice.hasFlash then
  begin
    case FlashMode of
      TFlashMode.AutoFlash:
      begin
        FDevice.setFlashMode(AVCaptureFlashModeAuto);
      end;
      TFlashMode.FlashOff:
      begin
        FDevice.setFlashMode(AVCaptureFlashModeOff);
      end;
      TFlashMode.FlashOn:
      begin
        FDevice.setFlashMode(AVCaptureFlashModeOn);
      end;
    end;
  end;
end;

class function TPlatformCamera.GetDeviceRotation: Double;
begin
  if TiOSHelper.SharedApplication.statusBarOrientation = UIDeviceOrientationPortrait then
    Result := 90
  else
    Result := 0;
end;

function TPlatformCamera.GetPreviewControl: TControl;
begin
  Result := FPreview;
end;

function TPlatformCamera.GetResolutionDimensions: CMVideoDimensions;
begin
  Result.width := 0;
  Result.height := 0;
  if (FDevice <> nil) and (FDevice.activeFormat <> nil) then
    Result := CMVideoFormatDescriptionGetDimensions(FDevice.activeFormat.formatDescription);
end;

function TPlatformCamera.GetResolutionHeight: Integer;
begin
  Result := GetResolutionDimensions.height;
end;

function TPlatformCamera.GetResolutionWidth: Integer;
begin
  Result := GetResolutionDimensions.width;
end;

procedure TPlatformCamera.OpenCamera;
var
  LDevices: NSArray;
  LDevice: AVCaptureDevice;
  I: Integer;
begin
  FDevice := nil;
  LDevices := TAVCaptureDevice.OCClass.devicesWithMediaType(AVMediaTypeVideo);
  if LDevices.count > 0 then
  begin
    for I := 0 to LDevices.count - 1 do
    begin
      LDevice := TAVCaptureDevice.Wrap(LDevices.objectAtIndex(I));
      case CameraPosition of
        TDevicePosition.Back:
          if LDevice.position = AVCaptureDevicePositionBack then
            FDevice := LDevice;
        TDevicePosition.Front:
          if LDevice.position = AVCaptureDevicePositionFront then
            FDevice := LDevice;
      end;
    end;
  end;
  if FDevice <> nil then
  begin
    UpdateExposureValue;
    UpdateDeviceModes;
    StartCapture;
    InternalSetActive(True);
  end;
end;

procedure TPlatformCamera.CloseCamera;
begin
  StopCapture;
  if FIsSwapping then
    FISODefault := -1;
  FDevice := nil;
  FAvailableFaceDetectModes := [];
  SetFaceDetectMode(TFaceDetectMode.None);
  InternalSetActive(False);
end;

procedure TPlatformCamera.UpdateSession;
var
  LInput: AVCaptureDeviceInput;
  LError: NSError;
  LOutputSettings: NSMutableDictionary;
begin
  if FSession = nil then
    FSession := TAVCaptureSession.Create;
  if FSession.inputs.count > 0 then
    FSession.removeInput(TAVCaptureDeviceInput.Wrap(FSession.inputs.objectAtIndex(0)));
  LError := nil;
  LInput := TAVCaptureDeviceInput.Wrap(TAVCaptureDeviceInput.OCClass.deviceInputWithDevice(FDevice, @LError));
  if LInput <> nil then
  begin
    FSession.addInput(LInput);
    if FStillImageOutput = nil then
    begin
      LOutputSettings := TNSMutableDictionary.Create;
      LOutputSettings.setObject(NSObjectToID(AVVideoCodecJPEG), NSObjectToID(AVVideoCodecKey));
      LOutputSettings.setObject(TNSNumber.OCClass.numberWithInt(kCVPixelFormatType_32BGRA), Pointer(kCVPixelBufferPixelFormatTypeKey));
      FStillImageOutput := TAVCaptureStillImageOutput.Create;
      FStillImageOutput.setOutputSettings(LOutputSettings);
      FSession.addOutput(FStillImageOutput);
    end;
  end;
end;

procedure TPlatformCamera.StartCapture;
begin
  StopCapture;
  UpdateSession;
  FSession.startRunning;
  FPreviewNative.StartPreview(FSession);
  FIsCapturing := FSession.isRunning;
end;

procedure TPlatformCamera.StopCapture;
begin
  if (FSession <> nil) and FSession.isRunning then
    FSession.stopRunning;
  if not FIsSwapping then
    FPreviewNative.StopPreview;
  FIsCapturing := False;
end;

end.
