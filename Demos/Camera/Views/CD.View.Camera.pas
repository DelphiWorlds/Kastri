unit CD.View.Camera;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.TabControl, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  DW.NativeImage, DW.Camera, DW.Types,
  CD.Types;

type
  TCameraView = class(TForm)
    RootLayout: TLayout;
    TabControl: TTabControl;
    PreviewTab: TTabItem;
    BottomLayout: TLayout;
    CaptureTab: TTabItem;
    CaptureButtonLayout: TLayout;
    PreviewLayout: TLayout;
    TopLayout: TLayout;
    procedure RootLayoutResized(Sender: TObject);
  private
    FAcceptImage: TNativeImage;
    FCamera: TCamera;
    FCancelImage: TNativeImage;
    FCameraImage: TNativeImage;
    FCameraSwapImage: TNativeImage;
    FCaptureImage: TNativeImage;
    FImageStream: TMemoryStream;
    procedure CameraAuthorizationStatusHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
    procedure CameraImageCapturedHandler(Sender: TObject; const AImageStream: TStream);
    procedure CameraImageClickHandler(Sender: TObject);
    procedure CameraStatusChange(Sender: TObject);
    procedure CameraSwapImageClickHandler(Sender: TObject);
    procedure CreateAcceptImage;
    procedure CreateCamera;
    procedure CreateCancelImage;
    procedure CreateCameraImage;
    procedure CreateCameraSwapImage;
    procedure CreateCaptureImage;
    procedure EnableButtons(const AEnable: Boolean);
    procedure InternalShowPreview;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowPreview(const AShow: Boolean);
    property Camera: TCamera read FCamera;
    property AcceptImage: TNativeImage read FAcceptImage;
    property CancelImage: TNativeImage read FCancelImage;
    property CaptureImage: TNativeImage read FCaptureImage;
    property ImageStream: TMemoryStream read FImageStream;
  end;

var
  CameraView: TCameraView;

implementation

{$R *.fmx}

uses
  FMX.Media,
  DW.UIHelper;

{ TCameraView }

constructor TCameraView.Create(AOwner: TComponent);
begin
  inherited Create(Application);
  FImageStream := TMemoryStream.Create;
  CreateAcceptImage;
  CreateCancelImage;
  CreateCaptureImage;
  CreateCameraImage;
  CreateCameraSwapImage;
  CreateCamera;
  RootLayout.Visible := False;
  RootLayout.Parent := TFmxObject(AOwner);
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

procedure TCameraView.CreateAcceptImage;
begin
  FAcceptImage := TNativeImage.Create(Self);
  FAcceptImage.Height := 64;
  FAcceptImage.Width := 64;
  FAcceptImage.Align := TAlignLayout.Right;
  FAcceptImage.LoadFromResource('AcceptPlainGreen64');
  FAcceptImage.Visible := False;
  FAcceptImage.Parent := TopLayout;
end;

procedure TCameraView.CreateCancelImage;
begin
  FCancelImage := TNativeImage.Create(Self);
  FCancelImage.Height := 64;
  FCancelImage.Width := 64;
  FCancelImage.Align := TAlignLayout.Left;
  FCancelImage.LoadFromResource('CancelPlainRed64');
  FCancelImage.Parent := TopLayout;
end;

procedure TCameraView.CreateCaptureImage;
begin
  FCaptureImage := TNativeImage.Create(Self);
  FCaptureImage.Align := TAlignLayout.Contents;
  FCaptureImage.Parent := CaptureTab;
end;

procedure TCameraView.CreateCameraImage;
begin
  FCameraImage := TNativeImage.Create(Self);
  FCameraImage.OnClick := CameraImageClickHandler;
  FCameraImage.Height := 64;
  FCameraImage.Width := 64;
  FCameraImage.Align := TAlignLayout.Center;
  FCameraImage.LoadFromResource('CameraPlainBlue64');
  FCameraImage.Parent := CaptureButtonLayout;
end;

procedure TCameraView.CreateCameraSwapImage;
begin
  FCameraSwapImage := TNativeImage.Create(Self);
  FCameraSwapImage.OnClick := CameraSwapImageClickHandler;
  FCameraSwapImage.Height := 64;
  FCameraSwapImage.Width := 64;
  FCameraSwapImage.Margins.Right := 4;
  FCameraSwapImage.Align := TAlignLayout.Right;
  FCameraSwapImage.LoadFromResource('CameraSwapPlainPurple64');
  FCameraSwapImage.Parent := BottomLayout;
end;

procedure TCameraView.CreateCamera;
begin
  FCamera := TCamera.Create;
  FCamera.CameraPosition := TDevicePosition.Back;
  FCamera.OnImageCaptured := CameraImageCapturedHandler;
  FCamera.OnAuthorizationStatus := CameraAuthorizationStatusHandler;
  FCamera.OnStatusChange := CameraStatusChange;
  FCamera.PreviewControl.Parent := PreviewLayout;
end;

procedure TCameraView.CameraImageClickHandler(Sender: TObject);
begin
  if not FAcceptImage.Visible then
  begin
    EnableButtons(False);
    FCamera.CaptureImage;
  end
  else
    ShowPreview(True);
end;

procedure TCameraView.CameraAuthorizationStatusHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
begin
  if AStatus = TAuthorizationStatus.Authorized  then
    InternalShowPreview;
end;

procedure TCameraView.CameraImageCapturedHandler(Sender: TObject; const AImageStream: TStream);
begin
  FImageStream.Clear;
  FImageStream.CopyFrom(AImageStream);
  FCaptureImage.LoadFromStream(FImageStream);
  TabControl.ActiveTab := CaptureTab;
  EnableButtons(True);
  FAcceptImage.Visible := True;
  FCameraSwapImage.Visible := False;
end;

procedure TCameraView.CameraStatusChange(Sender: TObject);
begin
  //
end;

procedure TCameraView.CameraSwapImageClickHandler(Sender: TObject);
begin
  if FCamera.CameraPosition = TDevicePosition.Front then
    FCamera.CameraPosition := TDevicePosition.Back
  else
    FCamera.CameraPosition := TDevicePosition.Front;
end;

procedure TCameraView.EnableButtons(const AEnable: Boolean);
begin
  FCameraImage.Enabled := AEnable;
  FCameraSwapImage.Enabled := AEnable;
  FCancelImage.Enabled := AEnable;
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
  FAcceptImage.Visible := False;
  TabControl.ActiveTab := PreviewTab;
  FCameraSwapImage.Visible := True;
  RootLayout.Visible := True;
  FCamera.IsActive := True;
end;

end.
