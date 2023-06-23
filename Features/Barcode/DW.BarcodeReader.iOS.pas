unit DW.BarcodeReader.iOS;

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
  // iOS
  iOSapi.Foundation,
  // FMX
  FMX.Graphics,
  // DW
  DW.iOSapi.MLKitBarcodeScanning, DW.BarcodeReader;

type
  TPlatformBarcodeReader = class(TCustomPlatformBarcodeReader)
  private
    FScanner: MLKBarcodeScanner;
    function GetCodeFormats: MLKBarcodeFormat;
    function GetBarcodeFormat(const AFormat: MLKBarcodeFormat): TBarcodeFormat;
    procedure ScannerProcessImageCompletionHandler(barcodes: NSArray; error: NSError);
  protected
    procedure Scan(const ABitmap: TBitmap); override;
  public
    constructor Create(const AReader: TBarcodeReader); override;
    destructor Destroy; override;
  end;

implementation

uses
  // macOS
  Macapi.Helpers,
  // FMX
  FMX.Helpers.iOS,
  // DW
  DW.iOSapi.MLKitVision;

const
  cFormatValues: array[TBarcodeFormat] of MLKBarcodeFormat = (
    MLKBarcodeFormatAll,
    MLKBarcodeFormatAztec,
    MLKBarcodeFormatCodaBar,
    MLKBarcodeFormatCode128,
    MLKBarcodeFormatCode39,
    MLKBarcodeFormatCode93,
    MLKBarcodeFormatDataMatrix,
    MLKBarcodeFormatEAN8,
    MLKBarcodeFormatEAN13,
    MLKBarcodeFormatITF,
    MLKBarcodeFormatPDF417,
    MLKBarcodeFormatQRCode,
    MLKBarcodeFormatUPCA,
    MLKBarcodeFormatUPCE,
    MLKBarcodeFormatUnknown
  );

{ TPlatformBarcodeReader }

constructor TPlatformBarcodeReader.Create(const AReader: TBarcodeReader);
begin
  inherited;
  //
end;

destructor TPlatformBarcodeReader.Destroy;
begin
  //
  inherited;
end;

function TPlatformBarcodeReader.GetBarcodeFormat(const AFormat: MLKBarcodeFormat): TBarcodeFormat;
var
  LBarcodeFormat: TBarcodeFormat;
begin
  Result := TBarcodeFormat.Unknown;
  for LBarcodeFormat := Succ(Low(TBarcodeFormat)) to Pred(High(TBarcodeFormat)) do
  begin
    if AFormat = cFormatValues[LBarcodeFormat] then
    begin
      Result := LBarcodeFormat;
      Break;
    end;
  end;
end;

function TPlatformBarcodeReader.GetCodeFormats: MLKBarcodeFormat;
var
  LBarcodeFormat: TBarcodeFormat;
begin
  Result := MLKBarcodeFormatUnknown;
  if not (TBarcodeFormat.All in Formats) then
  begin
    for LBarcodeFormat := Succ(Low(TBarcodeFormat)) to Pred(High(TBarcodeFormat)) do
    begin
      if LBarcodeFormat in Formats then
        Result := Result or cFormatValues[LBarcodeFormat];
    end;
  end
  else
    Result := MLKBarcodeFormatAll;
end;

procedure TPlatformBarcodeReader.Scan(const ABitmap: TBitmap);
var
  LOptions: MLKBarcodeScannerOptions;
  LImage: MLKVisionImage;
begin
  FScanner := nil;
  LOptions := TMLKBarcodeScannerOptions.Create;
  LOptions := TMLKBarcodeScannerOptions.Wrap(LOptions.initWithFormats(GetCodeFormats));
  LImage := TMLKVisionImage.Create;
  LImage := TMLKVisionImage.Wrap(LImage.initWithImage(BitmapToUIImage(ABitmap)));
  FScanner := TMLKBarcodeScanner.Wrap(TMLKBarcodeScanner.OCClass.barcodeScannerWithOptions(LOptions));
  FScanner.processImage(LImage, ScannerProcessImageCompletionHandler);
end;

procedure TPlatformBarcodeReader.ScannerProcessImageCompletionHandler(barcodes: NSArray; error: NSError);
var
  LMLKBarcode: MLKBarcode;
  LBarcode: TBarcode;
  LBarcodes: TBarcodes;
  I: Integer;
  LError: string;
begin
  if error = nil then
  begin
    LError := '';
    for I := 0 to barcodes.count - 1 do
    begin
      LMLKBarcode := TMLKBarcode.Wrap(barcodes.objectAtIndex(I));
      LBarcode.Value := NSStrToStr(LMLKBarcode.displayValue);
      LBarcode.Format := GetBarcodeFormat(LMLKBarcode.format);
      LBarcodes := LBarcodes + [LBarcode];
    end;
  end
  else
    LError := NSStrToStr(error.localizedDescription);
  DoBarcodes(LBarcodes, LError);
end;

end.
