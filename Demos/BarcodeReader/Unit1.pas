unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Objects, FMX.TabControl, FMX.Media,
  DW.BarcodeReader;

type
  TForm1 = class(TForm)
    ButtonLayout: TLayout;
    ScanButton: TButton;
    ScanImage: TImage;
    Memo: TMemo;
    Camera: TCameraComponent;
    BracketsLayout: TLayout;
    BracketLeftRectangle: TRectangle;
    BracketBottomLeftRectangle: TRectangle;
    BracketTopRightRectangle: TRectangle;
    BracketBottomRightRectangle: TRectangle;
    procedure ScanButtonClick(Sender: TObject);
    procedure TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
    procedure CameraSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
  private
    FIsScanning: Boolean;
    FReader: TBarcodeReader;
    FProcessTime: TDateTime;
    FSectionBitmap: TBitmap;
    procedure ReaderBarcodeHandler(Sender: TObject; const ABarcodes: TBarcodes; const AError: string);
    procedure StartCamera;
    procedure StopCamera;
    procedure UpdateScanButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Permissions, System.DateUtils,
  DW.Consts.Android, DW.Permissions.Helpers;

const
  cProcessImageRateDefault = 500; // Every 500 milliseconds

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FSectionBitmap := TBitmap.Create;
  FReader := TBarcodeReader.Create;
  FReader.Formats := [TBarcodeFormat.All];
  FReader.OnBarcode := ReaderBarcodeHandler;
end;

destructor TForm1.Destroy;
begin
  FReader.Free;
  inherited;
end;

procedure TForm1.ReaderBarcodeHandler(Sender: TObject; const ABarcodes: TBarcodes; const AError: string);
var
  LBarcode: TBarcode;
begin
  if Length(ABarcodes) > 0 then
  begin
    StopCamera;
    Camera.Active := False;
    for LBarcode in ABarcodes do
      Memo.Lines.Add(Format('%s - (%s)', [LBarcode.Value, LBarcode.FormatDescription]));
  end;
end;

procedure TForm1.ScanButtonClick(Sender: TObject);
begin
  if not FIsScanning then
  begin
    PermissionsService.RequestPermissions([cPermissionCamera],
      procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
      begin
        if AGrantResults.AreAllGranted then
          StartCamera;
      end
    );
  end
  else
    StopCamera;
end;

procedure TForm1.StartCamera;
begin
  Memo.Lines.Clear;
  Camera.FocusMode := TFocusMode.ContinuousAutoFocus;
  Camera.Active := True;
  FIsScanning := True;
  UpdateScanButton;
end;

procedure TForm1.StopCamera;
begin
  Camera.Active := False;
  FIsScanning := False;
  UpdateScanButton;
end;

procedure TForm1.TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
begin
  ScanImage.Bitmap.Assign(Image);
  FReader.Scan(Image);
end;

procedure TForm1.UpdateScanButton;
const
  cButtonCaptions: array[Boolean] of string = ('Scan', 'Stop');
begin
  ScanButton.Text := cButtonCaptions[FIsScanning];
end;

procedure TForm1.CameraSampleBufferReady(Sender: TObject; const ATime: TMediaTime);
var
  LMillisDiff: Integer;
  LScaleRect, LCopyRect: TRectF;
begin
  Camera.SampleBufferToBitmap(ScanImage.Bitmap, True);
  LMillisDiff := -1;
  if FProcessTime > 0 then
    LMillisDiff := MilliSecondsBetween(FProcessTime, Now);
  // i.e. Do not process too many images per second
  if (LMillisDiff = -1) or (LMillisDiff >= cProcessImageRateDefault) then
  begin
    FProcessTime := Now;
    // Work out the bitmap rect from the relative sizes
    LScaleRect.Left := BracketsLayout.Position.X / ScanImage.Width;
    LScaleRect.Top :=  BracketsLayout.Position.Y / ScanImage.Height;
    LScaleRect.Right := BracketsLayout.Width / ScanImage.Width;
    LScaleRect.Bottom := BracketsLayout.Height / ScanImage.Height;
    LCopyRect := TRectF.Create(PointF(LScaleRect.Left * ScanImage.Bitmap.Width, LScaleRect.Top * ScanImage.Bitmap.Height),
    LScaleRect.Right * ScanImage.Bitmap.Width, LScaleRect.Bottom * ScanImage.Bitmap.Height);
    // Copy the bracketed part of the bitmap
    FSectionBitmap.SetSize(Trunc(LCopyRect.Width), Trunc(LCopyRect.Height));
    FSectionBitmap.CopyFromBitmap(ScanImage.Bitmap, LCopyRect.Truncate, 0, 0);
    FReader.Scan(FSectionBitmap);
  end;
end;

end.
