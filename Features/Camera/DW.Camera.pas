unit DW.Camera;

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
  System.Classes, System.Types, System.Messaging, System.Sensors,
  // FMX
  FMX.Controls, FMX.Media, FMX.Graphics,
  // DW
  DW.Types;

type
  TMetadataOption = (GPS, Orientation);

  TMetadataOptions = set of TMetadataOption;

  TFaceDetectMode = (None, Simple, Full);

  TFaceDetectModes = set of TFaceDetectMode;

  TFace = record
    Bounds: TRectF;
    LeftEyePosition: TPointF;
    MouthPosition: TPointF;
    RightEyePosition: TPointF;
    Score: Integer;
  end;

  TFaces = array of TFace;

  TDetectedFacesEvent = procedure(Sender: TObject; const ImageStream: TStream; const Faces: TFaces) of object;

  TFrameAvailableEvent = procedure(Sender: TObject; const Frame: TBitmap) of object;
  TImageAvailableEvent = procedure(Sender: TObject; const ImageStream: TStream) of object;

  TCamera = class;

  TCustomPlatformCamera = class(TObject)
  private
    FCamera: TCamera;
    FCameraPosition: TDevicePosition;
    FContinuousCapture: Boolean;
    FExposure: Single;
    FFaceDetectMode: TFaceDetectMode;
    FFlashMode: TFlashMode;
    FWasActive: Boolean;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    function GetExposure: Single;
    function GetIsActive: Boolean;
    function GetCameraPosition: TDevicePosition;
    function GetMetadataOptions: TMetadataOptions;
    procedure ResetCamera;
    procedure ResignCamera;
    procedure RestoreCamera;
    procedure SetCameraPosition(const Value: TDevicePosition);
    procedure SetContinuousCapture(const Value: Boolean);
    procedure SetExposure(const Value: Single);
    procedure SetIsActive(const Value: Boolean);
  protected
    FAvailableFaceDetectModes: TFaceDetectModes;
    FIsActive: Boolean;
    FIsCapturing: Boolean;
    FIsFaceDetectActive: Boolean;
    FIsSwapping: Boolean;
    procedure CameraSettingChanged; virtual;
    procedure CaptureImage;
    procedure CloseCamera; virtual;
    procedure ContinuousCaptureChanged; virtual;
    procedure DoAuthorizationStatus(const AStatus: TAuthorizationStatus);
    procedure DoCaptureImage; virtual;
    procedure DoCapturedImage(const AImageStream: TStream);
    procedure DoDetectedFaces(const AImageStream: TStream; const AFaces: TFaces);
    procedure DoFrameAvailable(const AFrame: TBitmap);
    procedure DoStatusChange;
    function GetCameraOrientation: Integer; virtual; //!!!!
    function GetFlashMode: TFlashMode;
    function GetPreviewControl: TControl; virtual;
    function GetResolutionHeight: Integer; virtual;
    function GetResolutionWidth: Integer; virtual;
    procedure InternalSetActive(const AValue: Boolean);
    procedure InternalSetExposure(const AValue: Single);
    procedure OpenCamera; virtual;
    procedure QueueAuthorizationStatus(const AStatus: TAuthorizationStatus);
    procedure RequestPermission; virtual; abstract;
    procedure SetFaceDetectMode(const Value: TFaceDetectMode); virtual;
    procedure SetFlashMode(const Value: TFlashMode);
    procedure StartCapture; virtual;
    procedure StopCapture; virtual;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property Camera: TCamera read FCamera;
    property CameraPosition: TDevicePosition read GetCameraPosition write SetCameraPosition;
    property ContinuousCapture: Boolean read FContinuousCapture write SetContinuousCapture;
    property Exposure: Single read GetExposure write SetExposure;
    property FaceDetectMode: TFaceDetectMode read FFaceDetectMode write SetFaceDetectMode;
    property FlashMode: TFlashMode read GetFlashMode write SetFlashMode;
    property MetadataOptions: TMetadataOptions read GetMetadataOptions;
  public
    constructor Create(const ACamera: TCamera); virtual;
    destructor Destroy; override;
    property PreviewControl: TControl read GetPreviewControl;
  end;

  TCamera = class(TObject)
  private
    FAuthorizationStatus: TAuthorizationStatus;
    FLocation: TLocationCoord2D;
    FMetadataOptions: TMetadataOptions;
    FPlatformCamera: TCustomPlatformCamera;
    FOnAuthorizationStatus: TAuthorizationStatusEvent;
    FOnDetectedFaces: TDetectedFacesEvent;
    FOnFrameAvailable: TFrameAvailableEvent;
    FOnImageCaptured: TImageAvailableEvent;
    FOnStatusChange: TNotifyEvent;
    function GetIsActive: Boolean;
    function GetAvailableFaceDetectModes: TFaceDetectModes;
    function GetCameraPosition: TDevicePosition;
    function GetContinuousCapture: Boolean;
    function GetExposure: Single;
    function GetFaceDetectMode: TFaceDetectMode;
    function GetFlashMode: TFlashMode;
    function GetPreviewControl: TControl;
    function GetResolutionHeight: Integer;
    function GetResolutionWidth: Integer;
    procedure SetContinuousCapture(const Value: Boolean);
    procedure SetIsActive(const Value: Boolean);
    procedure SetCameraPosition(const Value: TDevicePosition);
    procedure SetExposure(const Value: Single);
    procedure SetFaceDetectMode(const Value: TFaceDetectMode);
    procedure SetFlashMode(const Value: TFlashMode);
    function GetCameraOrientation: Integer; //!!!!
    function GetIncludeLocation: Boolean;
    procedure SetIncludeLocation(const Value: Boolean);
    function GetIsCapturing: Boolean;
  protected
    procedure DoAuthorizationStatus(const AStatus: TAuthorizationStatus);
    procedure DoCapturedImage(const AImageStream: TStream);
    procedure DoDetectedFaces(const AImageStream: TStream; const AFaces: TFaces);
    procedure DoFrameAvailable(const AFrame: TBitmap);
    procedure DoStatusChange;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Captures a still image, returned in OnImageCaptured
    /// </summary>
    procedure CaptureImage;
    /// <summary>
    ///   Requests camera permissions, returned in OnAuthorizationStatus
    /// </summary>
    procedure RequestPermission;
    /// <summary>
    ///   Modes that are available for face detection
    /// </summary>
    property AvailableFaceDetectModes: TFaceDetectModes read GetAvailableFaceDetectModes;
    /// <summary>
    ///   Current authorization status
    /// </summary>
    property AuthorizationStatus: TAuthorizationStatus read FAuthorizationStatus;
    property CameraOrientation: Integer read GetCameraOrientation; //!!!!
    /// <summary>
    ///   Position of the currently selected camera, i.e. Front or Back
    /// </summary>
    property CameraPosition: TDevicePosition read GetCameraPosition write SetCameraPosition;
    property ContinuousCapture: Boolean read GetContinuousCapture write SetContinuousCapture;
    /// <summary>
    ///   //
    /// </summary>
    property Exposure: Single read GetExposure write SetExposure;
    /// <summary>
    ///   Currently selected mode of face detection
    /// </summary>
    property FaceDetectMode: TFaceDetectMode read GetFaceDetectMode write SetFaceDetectMode;
    /// <summary>
    ///   Currently selected flash mode
    /// </summary>    
    property FlashMode: TFlashMode read GetFlashMode write SetFlashMode;
    /// <summary>
    ///   Include location data with the captured image. See also Location property
    /// </summary>
    property IncludeLocation: Boolean read GetIncludeLocation write SetIncludeLocation;
    /// <summary>
    ///   Signifies whether or not the camera is active
    /// </summary>
    property IsActive: Boolean read GetIsActive write SetIsActive;
    /// <summary>
    ///   Signifies whether or not the camera is capturing video
    /// </summary>
    property IsCapturing: Boolean read GetIsCapturing;
    /// <summary>
    ///   Location data to be included with the captured image.
    /// </summary>
    /// <remarks>
    ///   Set this value before calling CaptureImage
    /// </remarks>
    property Location: TLocationCoord2D read FLocation write FLocation;
    /// <summary>
    ///   Include location data with the captured image. See also Location property
    /// </summary>
    property MetadataOptions: TMetadataOptions read FMetadataOptions write FMetadataOptions;
    /// <summary>
    ///   The control in which to show the camera preview
    /// </summary>      
    property PreviewControl: TControl read GetPreviewControl;
    /// <summary>
    ///   Vertical resolution
    /// </summary>      
    property ResolutionHeight: Integer read GetResolutionHeight;
    /// <summary>
    ///   Horizontal resolution
    /// </summary>   
    property ResolutionWidth: Integer read GetResolutionWidth;
    /// <summary>
    ///   Event fired when an authorization request has returned
    /// </summary>
    property OnAuthorizationStatus: TAuthorizationStatusEvent read FOnAuthorizationStatus write FOnAuthorizationStatus;
    /// <summary>
    ///   Event fired when faces are detected
    /// </summary>
    property OnDetectedFaces: TDetectedFacesEvent read FOnDetectedFaces write FOnDetectedFaces;
    /// <summary>
    ///   Event fired when a frame is available
    /// </summary>
    property OnFrameAvailable: TFrameAvailableEvent read FOnFrameAvailable write FOnFrameAvailable;
    /// <summary>
    ///   Event fired when a still image is captured
    /// </summary>
    property OnImageCaptured: TImageAvailableEvent read FOnImageCaptured write FOnImageCaptured;
    /// <summary>
    ///   Event fired when the status of the camera changes
    /// </summary>
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;
  end;

implementation

uses
  // FMX
  FMX.Platform,
  // DW
{$IF Defined(ANDROID)}
  DW.Camera.Android,
{$ENDIF}
{$IF Defined(IOS)}
  DW.Camera.iOS, DW.iOSapi.Helpers,
{$ENDIF}
  DW.OSLog,
  DW.Messaging;

type
  TPlatformCameraDefault = class(TCustomPlatformCamera)
  private
    FPreviewControl: TControl;
  protected
    function GetPreviewControl: TControl; override;
  public
    constructor Create(const ACamera: TCamera); override;
    destructor Destroy; override;
  end;

{$IF Defined(MSWINDOWS)}
   TPlatformCamera = TPlatformCameraDefault;
{$ENDIF}

{ TPlatformCameraDefault }

constructor TPlatformCameraDefault.Create(const ACamera: TCamera);
begin
  inherited;
  FPreviewControl := TControl.Create(nil);
end;

destructor TPlatformCameraDefault.Destroy;
begin
  FPreviewControl.Free;
  inherited;
end;

function TPlatformCameraDefault.GetPreviewControl: TControl;
begin
  Result := FPreviewControl;
end;

{ TCustomPlatformCamera }

constructor TCustomPlatformCamera.Create(const ACamera: TCamera);
begin
  inherited Create;
  FCamera := ACamera;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TCustomPlatformCamera.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TCustomPlatformCamera.InternalSetActive(const AValue: Boolean);
begin
  FIsActive := AValue;
  TThread.Synchronize(nil,
    procedure
    begin
      DoStatusChange;
    end
  );
end;

procedure TCustomPlatformCamera.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
var
  LEvent: TApplicationEvent;
begin
  LEvent := TApplicationEventMessage(M).Value.Event;
  case LEvent of
    TApplicationEvent.EnteredBackground:
      ResignCamera;
    TApplicationEvent.BecameActive:
      RestoreCamera;
  end;
end;

procedure TCustomPlatformCamera.CameraSettingChanged;
begin
  //
end;

procedure TCustomPlatformCamera.CaptureImage;
begin
  DoCaptureImage;
end;

procedure TCustomPlatformCamera.CloseCamera;
begin
  //
end;

procedure TCustomPlatformCamera.ContinuousCaptureChanged;
begin
  //
end;

procedure TCustomPlatformCamera.DoAuthorizationStatus(const AStatus: TAuthorizationStatus);
begin
  FCamera.DoAuthorizationStatus(AStatus);
end;

procedure TCustomPlatformCamera.DoCapturedImage(const AImageStream: TStream);
begin
  AImageStream.Position := 0;
  FCamera.DoCapturedImage(AImageStream);
end;

procedure TCustomPlatformCamera.DoCaptureImage;
begin
  //
end;

procedure TCustomPlatformCamera.DoDetectedFaces(const AImageStream: TStream; const AFaces: TFaces);
begin
  AImageStream.Position := 0;
  FCamera.DoDetectedFaces(AImageStream, AFaces);
end;

procedure TCustomPlatformCamera.DoFrameAvailable(const AFrame: TBitmap);
begin
  FCamera.DoFrameAvailable(AFrame);
end;

procedure TCustomPlatformCamera.DoStatusChange;
begin
  FCamera.DoStatusChange;
end;

function TCustomPlatformCamera.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

function TCustomPlatformCamera.GetMetadataOptions: TMetadataOptions;
begin
  Result := FCamera.MetadataOptions;
end;

function TCustomPlatformCamera.GetCameraOrientation: Integer;
begin
  Result := -1;
end;

function TCustomPlatformCamera.GetCameraPosition: TDevicePosition;
begin
  Result := FCameraPosition;
end;

function TCustomPlatformCamera.GetExposure: Single;
begin
  Result := FExposure;
end;

function TCustomPlatformCamera.GetFlashMode: TFlashMode;
begin
  Result := FFlashMode;
end;

function TCustomPlatformCamera.GetPreviewControl: TControl;
begin
  Result := nil;
end;

function TCustomPlatformCamera.GetResolutionHeight: Integer;
begin
  Result := 0;
end;

function TCustomPlatformCamera.GetResolutionWidth: Integer;
begin
  Result := 0;
end;

procedure TCustomPlatformCamera.OpenCamera;
begin
  //
end;

procedure TCustomPlatformCamera.QueueAuthorizationStatus(const AStatus: TAuthorizationStatus);
begin
  TThread.Queue(nil,
    procedure
    begin
      DoAuthorizationStatus(AStatus);
    end
  );
end;

procedure TCustomPlatformCamera.ResetCamera;
begin
  if FIsActive then
  begin
    CloseCamera;
    OpenCamera;
    FIsSwapping := False;
  end;
end;

procedure TCustomPlatformCamera.ResignCamera;
begin
  FWasActive := FIsActive;
  if FIsActive then
    CloseCamera;
end;

procedure TCustomPlatformCamera.RestoreCamera;
begin
  if FWasActive then
    OpenCamera;
end;

procedure TCustomPlatformCamera.SetIsActive(const Value: Boolean);
begin
  if FIsActive <> Value then
  begin
    if Value then
      OpenCamera
    else
      CloseCamera;
  end;
end;

procedure TCustomPlatformCamera.SetCameraPosition(const Value: TDevicePosition);
begin
  if Value <> FCameraPosition then
  begin
    FCameraPosition := Value;
    FIsSwapping := True;
    ResetCamera;
  end;
end;

procedure TCustomPlatformCamera.SetContinuousCapture(const Value: Boolean);
begin
  if FContinuousCapture <> Value then
  begin
    FContinuousCapture := Value;
    ContinuousCaptureChanged;
  end;
end;

procedure TCustomPlatformCamera.InternalSetExposure(const AValue: Single);
begin
  FExposure := AValue;
end;

procedure TCustomPlatformCamera.SetExposure(const Value: Single);
begin
  if FExposure <> Value then
  begin
    InternalSetExposure(Value);
    CameraSettingChanged;
  end;
end;

procedure TCustomPlatformCamera.SetFaceDetectMode(const Value: TFaceDetectMode);
begin
  FFaceDetectMode := Value;
end;

procedure TCustomPlatformCamera.SetFlashMode(const Value: TFlashMode);
begin
  if FFlashMode <> Value then
  begin
    FFlashMode := Value;
    CameraSettingChanged;
  end;
end;

procedure TCustomPlatformCamera.StartCapture;
begin
  //
end;

procedure TCustomPlatformCamera.StopCapture;
begin
  //
end;

{ TCamera }

constructor TCamera.Create;
begin
  inherited;
  FPlatformCamera := TPlatformCamera.Create(Self);
end;

destructor TCamera.Destroy;
begin
  FPlatformCamera.Free;
  inherited;
end;

procedure TCamera.DoAuthorizationStatus(const AStatus: TAuthorizationStatus);
begin
  FAuthorizationStatus := AStatus;
  if Assigned(FOnAuthorizationStatus) then
    FOnAuthorizationStatus(Self, FAuthorizationStatus);
end;

procedure TCamera.DoCapturedImage(const AImageStream: TStream);
begin
  if Assigned(FOnImageCaptured) then
    FOnImageCaptured(Self, AImageStream);
end;

procedure TCamera.DoDetectedFaces(const AImageStream: TStream; const AFaces: TFaces);
begin
  if Assigned(FOnDetectedFaces) then
    FOnDetectedFaces(Self, AImageStream, AFaces);
end;

procedure TCamera.DoFrameAvailable(const AFrame: TBitmap);
begin
  if Assigned(FOnFrameAvailable) then
    FOnFrameAvailable(Self, AFrame);
end;

procedure TCamera.DoStatusChange;
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self);
end;

function TCamera.GetIncludeLocation: Boolean;
begin
  Result := TMetadataOption.GPS in FMetadataOptions;
end;

function TCamera.GetIsActive: Boolean;
begin
  Result := FPlatformCamera.IsActive;
end;

function TCamera.GetIsCapturing: Boolean;
begin
  Result := FPlatformCamera.FIsCapturing;
end;

function TCamera.GetAvailableFaceDetectModes: TFaceDetectModes;
begin
  Result := FPlatformCamera.FAvailableFaceDetectModes;
end;

function TCamera.GetCameraOrientation: Integer;
begin
  Result := FPlatformCamera.GetCameraOrientation;
end;

function TCamera.GetCameraPosition: TDevicePosition;
begin
  Result := FPlatformCamera.CameraPosition;
end;

function TCamera.GetContinuousCapture: Boolean;
begin
  Result := FPlatformCamera.ContinuousCapture;
end;

function TCamera.GetExposure: Single;
begin
  Result := FPlatformCamera.Exposure;
end;

function TCamera.GetFaceDetectMode: TFaceDetectMode;
begin
  Result := FPlatformCamera.FaceDetectMode;
end;

function TCamera.GetFlashMode: TFlashMode;
begin
  Result := FPlatformCamera.FlashMode;
end;

function TCamera.GetPreviewControl: TControl;
begin
  Result := FPlatformCamera.PreviewControl;
end;

function TCamera.GetResolutionHeight: Integer;
begin
  Result := FPlatformCamera.GetResolutionHeight;
end;

function TCamera.GetResolutionWidth: Integer;
begin
  Result := FPlatformCamera.GetResolutionWidth;
end;

procedure TCamera.RequestPermission;
begin
  FPlatformCamera.RequestPermission;
end;

procedure TCamera.SetIncludeLocation(const Value: Boolean);
begin
  if Value then
    Include(FMetadataOptions, TMetadataOption.GPS)
  else
    Exclude(FMetadataOptions, TMetadataOption.GPS);
end;

procedure TCamera.SetIsActive(const Value: Boolean);
begin
  FPlatformCamera.IsActive := Value;
end;

procedure TCamera.SetCameraPosition(const Value: TDevicePosition);
begin
  FPlatformCamera.CameraPosition := Value;
end;

procedure TCamera.SetContinuousCapture(const Value: Boolean);
begin
  FPlatformCamera.ContinuousCapture := Value;
end;

procedure TCamera.SetExposure(const Value: Single);
begin
  FPlatformCamera.Exposure := Value;
end;

procedure TCamera.SetFaceDetectMode(const Value: TFaceDetectMode);
begin
  FPlatformCamera.FaceDetectMode := Value;
end;

procedure TCamera.SetFlashMode(const Value: TFlashMode);
begin
  FPlatformCamera.FlashMode := Value;
end;

procedure TCamera.CaptureImage;
begin
  if IsActive then
    FPlatformCamera.CaptureImage;
end;

end.
