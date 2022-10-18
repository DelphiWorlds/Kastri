unit BC.Forms.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.Objects, FMX.Layouts, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  FMX.Controls.Presentation, DW.NativeImage, DW.BarcodeReader, DW.Camera,
  System.Permissions, DW.Permissions.Helpers, DW.Types;

type
  TfrmMain = class(TForm)
    BottomLayout: TLayout;
    txtFlash: TText;
    ShadowEffect1: TShadowEffect;
    txtSwap: TText;
    ShadowEffect2: TShadowEffect;
    txtScans: TMemo;
    loCamera: TLayout;
    tmrCapture: TTimer;
    procedure txtSwapClick(Sender: TObject);
    procedure txtFlashClick(Sender: TObject);
    procedure txtScansClick(Sender: TObject);
    procedure tmrCaptureTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCamera : TCamera;
    FIsScanning: Boolean;
    FProcessTime: TDateTime;
    procedure ReaderBarcodeHandler(Sender: TObject; const ABarcodes: TBarcodes; const AError: string);
    procedure StartCamera;
    procedure StopCamera;
    procedure TryToStartCamera;
    procedure CameraAuthorizationStatusHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
    procedure CameraImageCapturedHandler(Sender: TObject; const AImageStream: TStream);
    procedure CameraStatusChangeHandler(Sender: TObject);
    procedure CameraFrameAvailableHandler(Sender: TObject; const AFrame: TBitmap);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  System.DateUtils, DW.Consts.Android, FMX.Media, FMX.Clipboard, FMX.Platform;

const
  cProcessImageRateDefault = 500; // Every 500 milliseconds


procedure TfrmMain.CameraAuthorizationStatusHandler(Sender: TObject;
  const AStatus: TAuthorizationStatus);
begin
  if AStatus = TAuthorizationStatus.Authorized  then
    StartCamera;
end;

procedure TfrmMain.CameraFrameAvailableHandler(Sender: TObject;
  const AFrame: TBitmap);
begin
  // Camera frame available as a bitmap, here. Only works with the conditional define: USEGL
end;

procedure TfrmMain.CameraImageCapturedHandler(Sender: TObject;
  const AImageStream: TStream);
begin
  var bmp := TBitmap.Create;
  try
    try
      bmp.LoadFromStream(AImageStream);
      try
        if (bmp.Width = 0) or (bmp.Height = 0) then
          exit;
        var bc := TBarcodeReader.Create;
        try
          bc.Formats := [TBarcodeFormat.All];
          bc.OnBarcode := ReaderBarcodeHandler;
          bc.Scan(bmp);
        finally
          bc.Free;
        end;
      finally
        tmrCapture.Enabled := True;
      end;
    except
      on e: exception do
        ShowMessage(e.Message);
    end;
  finally
    bmp.Free;
  end;
end;

procedure TfrmMain.CameraStatusChangeHandler(Sender: TObject);
begin
//
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FCamera := TCamera.Create;
  FCamera.CameraPosition := TDevicePosition.Back;
  FCamera.OnImageCaptured := CameraImageCapturedHandler;
  FCamera.OnAuthorizationStatus := CameraAuthorizationStatusHandler;
  FCamera.OnStatusChange := CameraStatusChangeHandler;
  FCamera.OnFrameAvailable := CameraFrameAvailableHandler;
  FCamera.PreviewControl.Parent := loCamera;
end;

destructor TfrmMain.Destroy;
begin
  StopCamera;
  FCamera.Free;
  inherited;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopCamera;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  TryToStartCamera;
end;

procedure TfrmMain.ReaderBarcodeHandler(Sender: TObject;
  const ABarcodes: TBarcodes; const AError: string);
begin
  if Length(ABarcodes) > 0 then
  begin
    var
      LBarcode: TBarcode;
      for LBarcode in ABarcodes do
        txtScans.Lines.Insert(0,Format('%s - (%s)', [LBarcode.Value, LBarcode.FormatDescription]));
  end;
end;

procedure TfrmMain.StartCamera;
begin
  if FIsScanning then
    exit;

  FCamera.IsActive := True;
  FIsScanning := True;
  tmrCapture.Enabled := True;
end;

procedure TfrmMain.StopCamera;
begin
  if not FIsScanning then
    exit;
  FCamera.IsActive := False;
  FIsScanning := False;
end;

procedure TfrmMain.tmrCaptureTimer(Sender: TObject);
begin
  tmrCapture.Enabled := False;
  if FCamera.IsActive then
    FCamera.CaptureImage
  else
    tmrCapture.Enabled := True;
end;

procedure TfrmMain.TryToStartCamera;
begin
  if not FIsScanning then
    FCamera.RequestPermission
end;

procedure TfrmMain.txtFlashClick(Sender: TObject);
begin
  if FCamera.FlashMode = TFlashMode.FlashOn then
    FCamera.FlashMode := TFlashMode.FlashOff
  else
    FCamera.FlashMode := TFlashMode.FlashOn;
end;

procedure TfrmMain.txtScansClick(Sender: TObject);
var
  clip : IFMXExtendedClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, clip) then
    clip.SetText(txtScans.Text);

end;

procedure TfrmMain.txtSwapClick(Sender: TObject);
begin
  if FCamera.CameraPosition = TDevicePosition.Front then
    FCamera.CameraPosition := TDevicePosition.Back
  else
    FCamera.CameraPosition := TDevicePosition.Front;
end;

end.
