unit CD.View.Camera;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.TabControl, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.Edit,
  DW.NativeImage, DW.Camera, DW.Types, DW.NativeSlider, DW.NativeShape,
  CD.Types;

type
  TExposureProperties = record
    IsSliderVisible: Boolean;
    Value: Single;
  end;

  TCameraView = class(TForm)
    RootLayout: TLayout;
    TabControl: TTabControl;
    PreviewTab: TTabItem;
    BottomLayout: TLayout;
    CaptureTab: TTabItem;
    CaptureButtonLayout: TLayout;
    PreviewLayout: TLayout;
    TopLayout: TLayout;
    MidLayout: TLayout;
    AcceptImage: TNativeImage;
    CancelImage: TNativeImage;
    CaptureImage: TNativeImage;
    Circle: TNativeEllipse;
    CameraSwapImage: TNativeImage;
    CameraImage: TNativeImage;
    ExposureSlider: TNativeSlider;
    procedure RootLayoutResized(Sender: TObject);
    procedure CameraImageClick(Sender: TObject);
    procedure CameraSwapImageClick(Sender: TObject);
    procedure CircleClick(Sender: TObject);
    procedure ExposureSliderValueChange(Sender: TObject);
  private
    FCamera: TCamera;
    FExposureProps: TExposureProperties;
    FImageStream: TMemoryStream;
    FWasActive: Boolean;
    procedure CameraAuthorizationStatusHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
    procedure CameraImageCapturedHandler(Sender: TObject; const AImageStream: TStream);
    procedure CameraStatusChangeHandler(Sender: TObject);
    procedure CameraFrameAvailableHandler(Sender: TObject; const AFrame: TBitmap);
    procedure CreateCamera;
    procedure EnableButtons(const AEnable: Boolean);
    procedure InternalShowPreview;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowPreview(const AShow: Boolean);
    property Camera: TCamera read FCamera;
    property ImageStream: TMemoryStream read FImageStream;
  end;

var
  CameraView: TCameraView;

implementation

{$R *.fmx}

uses
  FMX.Media,
  DW.OSLog,
  DW.UIHelper;

procedure LoadBitmapFromResource(const ABitmap: TBitmap; const AName: string);
var
  LStream: TStream;
begin
  if FindResource(HInstance, PChar(AName), RT_RCDATA) > 0 then
  begin
    LStream := TResourceStream.Create(HInstance, AName, RT_RCDATA);
    try
      ABitmap.LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;
end;

{ TCameraView }

constructor TCameraView.Create(AOwner: TComponent);
begin
  inherited Create(Application);
  FImageStream := TMemoryStream.Create;
  CreateCamera;
  RootLayout.Visible := False;
  RootLayout.Parent := TFmxObject(AOwner);
  // ExposureSlider.Margins.Rect := RectF(0, MidLayout.Height * 0.3, 2, MidLayout.Height * 0.3);
  ExposureSlider.Visible := False;
end;

destructor TCameraView.Destroy;
begin
  FImageStream.Free;
  inherited;
end;

procedure TCameraView.RootLayoutResized(Sender: TObject);
begin
  TopLayout.Margins.Top := TUIHelper.GetOffsetRect.Top;
end;

procedure TCameraView.CreateCamera;
begin
  FCamera := TCamera.Create;
  FCamera.CameraPosition := TDevicePosition.Back;
  FCamera.OnImageCaptured := CameraImageCapturedHandler;
  FCamera.OnAuthorizationStatus := CameraAuthorizationStatusHandler;
  FCamera.OnStatusChange := CameraStatusChangeHandler;
  FCamera.OnFrameAvailable := CameraFrameAvailableHandler;
  FCamera.PreviewControl.Parent := PreviewLayout;
end;

procedure TCameraView.CameraImageClick(Sender: TObject);
begin
  if not AcceptImage.Visible then
  begin
    EnableButtons(False);
    FCamera.CaptureImage;
  end
  else
  begin
    ExposureSlider.Visible := FExposureProps.IsSliderVisible;
    ShowPreview(True);
  end;
end;

procedure TCameraView.CameraAuthorizationStatusHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
begin
  if AStatus = TAuthorizationStatus.Authorized  then
    InternalShowPreview;
end;

procedure TCameraView.CameraFrameAvailableHandler(Sender: TObject; const AFrame: TBitmap);
begin
  // Camera frame available as a bitmap, here. Only works with the conditional define: USEGL
end;

procedure TCameraView.CameraImageCapturedHandler(Sender: TObject; const AImageStream: TStream);
begin
  FImageStream.Clear;
  FImageStream.CopyFrom(AImageStream, AImageStream.Size);
  CaptureImage.LoadFromStream(FImageStream);
  TabControl.ActiveTab := CaptureTab;
  EnableButtons(True);
  AcceptImage.Visible := True;
  CameraSwapImage.Visible := False;
  FExposureProps.IsSliderVisible := ExposureSlider.Visible;
  ExposureSlider.Visible := False;
end;

procedure TCameraView.CameraStatusChangeHandler(Sender: TObject);
begin
  if FCamera.IsCapturing then
  begin
    ExposureSlider.Value := FCamera.Exposure;
    if not FWasActive then
    begin
      FExposureProps.Value := FCamera.Exposure;
      if not ExposureSlider.Visible then
        FCamera.Exposure := -1;
    end;
    FWasActive := FCamera.IsActive;
  end;
end;

procedure TCameraView.CameraSwapImageClick(Sender: TObject);
begin
  if FCamera.CameraPosition = TDevicePosition.Front then
    FCamera.CameraPosition := TDevicePosition.Back
  else
    FCamera.CameraPosition := TDevicePosition.Front;
end;

procedure TCameraView.CircleClick(Sender: TObject);
begin
  if ExposureSlider.Visible then
  begin
    // Save the current value
    FExposureProps.Value := ExposureSlider.Value;
    FCamera.Exposure := -1; // Auto
    ExposureSlider.Visible := False;
  end
  else
  begin
    ExposureSlider.Visible := True;
    if FExposureProps.Value > -1 then
      FCamera.Exposure := FExposureProps.Value
    else
      FExposureProps.Value := FCamera.Exposure;
    ExposureSlider.Value := FExposureProps.Value;
  end;
  FExposureProps.IsSliderVisible := ExposureSlider.Visible;
end;

procedure TCameraView.EnableButtons(const AEnable: Boolean);
begin
  CameraImage.Enabled := AEnable;
  CameraSwapImage.Enabled := AEnable;
  CancelImage.Enabled := AEnable;
end;

procedure TCameraView.ExposureSliderValueChange(Sender: TObject);
begin
  FCamera.Exposure := ExposureSlider.Value;
end;

procedure TCameraView.ShowPreview(const AShow: Boolean);
begin
  if AShow then
    FCamera.RequestPermission
  else
  begin
    RootLayout.Visible := False;
    FCamera.IsActive := False;
  end;
end;

procedure TCameraView.InternalShowPreview;
begin
  FWasActive := FCamera.IsActive;
  AcceptImage.Visible := False;
  TabControl.ActiveTab := PreviewTab;
  CameraSwapImage.Visible := True;
  RootLayout.Visible := True;
  Circle.BringToFront;
  FCamera.IsActive := True;
end;

end.
