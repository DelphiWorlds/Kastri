unit DW.Camera.Win;

interface

uses
  FMX.Graphics, FMX.Controls,
  DW.Camera;

type
  TPlatformCamera = class(TCustomPlatformCamera)
  private
    FDetectionDateTime: TDateTime;
    FFaces: TFacesArray;
    FFacesDetected: Boolean;
  protected
    procedure CapturedStillImage(const ABitmap: TBitmap);
    // procedure DetectedFaces(const AFaces: TJavaObjectArray<JFace>);
    function GetHighestFaceDetectMode: TFaceDetectMode;
    function GetPreviewControl: TControl; override;
    procedure StillCaptureFailed;
  protected
    procedure CloseCamera; override;
    procedure OpenCamera; override;
    procedure StartCapture; override;
    procedure StopCapture; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  DW.MediaFoundation.Objects;

{ TPlatformCamera }

constructor TPlatformCamera.Create;
begin
  inherited;

end;

destructor TPlatformCamera.Destroy;
begin

  inherited;
end;

function TPlatformCamera.GetHighestFaceDetectMode: TFaceDetectMode;
begin

end;

function TPlatformCamera.GetPreviewControl: TControl;
begin

end;

procedure TPlatformCamera.CapturedStillImage(const ABitmap: TBitmap);
begin

end;

procedure TPlatformCamera.OpenCamera;
begin
  // Windows (and probably Mac) can have multiple sources. Might want to change TCamera to support that

end;

procedure TPlatformCamera.CloseCamera;
begin

end;

procedure TPlatformCamera.StartCapture;
begin
  inherited;

end;

procedure TPlatformCamera.StillCaptureFailed;
begin

end;

procedure TPlatformCamera.StopCapture;
begin
  inherited;

end;

end.
