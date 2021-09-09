unit DW.BarcodeReader.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  // FMX
  FMX.Graphics,
  // DW
  DW.BarcodeReader, DW.Androidapi.JNI.VisionBarcode;

type
  TPlatformBarcodeReader = class(TCustomPlatformBarcodeReader)
  private
    function GetBarcodeFormat(const AFormat: Integer): TBarcodeFormat;
    function GetFormats: Integer;
  protected
    procedure Scan(const ABitmap: TBitmap); override;
  public
    constructor Create(const AReader: TBarcodeReader); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Util,
  // DW
  DW.Graphics.Helpers.Android;

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

function TPlatformBarcodeReader.GetBarcodeFormat(const AFormat: Integer): TBarcodeFormat;
begin
  Result := TBarcodeFormat.Unknown;
  if AFormat = TJBarcode.JavaClass.AZTEC then
    Result := TBarcodeFormat.Aztec
  else if AFormat = TJBarcode.JavaClass.CODABAR then
    Result := TBarcodeFormat.Codabar
  else if AFormat = TJBarcode.JavaClass.CODE_128 then
    Result := TBarcodeFormat.Code128
  else if AFormat = TJBarcode.JavaClass.CODE_39 then
    Result := TBarcodeFormat.Code39
  else if AFormat = TJBarcode.JavaClass.CODE_93 then
    Result := TBarcodeFormat.Code93
  else if AFormat = TJBarcode.JavaClass.DATA_MATRIX then
    Result := TBarcodeFormat.DataMatrix
  else if AFormat = TJBarcode.JavaClass.EAN_13 then
    Result := TBarcodeFormat.EAN13
  else if AFormat = TJBarcode.JavaClass.EAN_8 then
    Result := TBarcodeFormat.EAN8
  else if AFormat = TJBarcode.JavaClass.ITF then
    Result := TBarcodeFormat.ITF
  else if AFormat = TJBarcode.JavaClass.PDF417 then
    Result := TBarcodeFormat.PDF417
  else if AFormat = TJBarcode.JavaClass.QR_CODE then
    Result := TBarcodeFormat.QR
  else if AFormat = TJBarcode.JavaClass.UPC_A then
    Result := TBarcodeFormat.UPCA
  else if AFormat = TJBarcode.JavaClass.UPC_E then
    Result := TBarcodeFormat.UPCE;
end;

function TPlatformBarcodeReader.GetFormats: Integer;
begin
  Result := 0;
  if TBarcodeFormat.Aztec in Formats then
    Result := Result or TJBarcode.JavaClass.AZTEC;
  if TBarcodeFormat.Codabar in Formats then
    Result := Result or TJBarcode.JavaClass.CODABAR;
  if TBarcodeFormat.Code128 in Formats then
    Result := Result or TJBarcode.JavaClass.CODE_128;
  if TBarcodeFormat.Code39 in Formats then
    Result := Result or TJBarcode.JavaClass.CODE_39;
  if TBarcodeFormat.Code93 in Formats then
    Result := Result or TJBarcode.JavaClass.CODE_93;
  if TBarcodeFormat.DataMatrix in Formats then
    Result := Result or TJBarcode.JavaClass.DATA_MATRIX;
  if TBarcodeFormat.EAN13 in Formats then
    Result := Result or TJBarcode.JavaClass.EAN_13;
  if TBarcodeFormat.EAN8 in Formats then
    Result := Result or TJBarcode.JavaClass.EAN_8;
  if TBarcodeFormat.ITF in Formats then
    Result := Result or TJBarcode.JavaClass.ITF;
  if TBarcodeFormat.PDF417 in Formats then
    Result := Result or TJBarcode.JavaClass.PDF417;
  if TBarcodeFormat.QR in Formats then
    Result := Result or TJBarcode.JavaClass.QR_CODE;
  if TBarcodeFormat.UPCA in Formats then
    Result := Result or TJBarcode.JavaClass.UPC_A;
  if TBarcodeFormat.UPCE in Formats then
    Result := Result or TJBarcode.JavaClass.UPC_E;
end;

procedure TPlatformBarcodeReader.Scan(const ABitmap: TBitmap);
var
  LDetectorBuilder: JBarcodeDetector_Builder;
  LFrameBuilder: JFrame_Builder;
  LResults: JSparseArray;
  LJBarcode: JBarcode;
  I: Integer;
  LBarcode: TBarcode;
  LBarcodes: TBarcodes;
  LError: string;
begin
  LDetectorBuilder := TJBarcodeDetector_Builder.JavaClass.init(TAndroidHelper.Context);
  LDetectorBuilder.setBarcodeFormats(GetFormats);
  LFrameBuilder := TJFrame_Builder.Create;
  LFrameBuilder.setBitmap(ABitmap.ToJBitmap);
  LResults := LDetectorBuilder.build.detect(LFrameBuilder.build);
  for I := 0 to LResults.size - 1 do
  begin
    LJBarcode := TJBarcode.Wrap(TAndroidHelper.JObjectToID(LResults.get(LResults.keyAt(0))));
    LBarcode.Value := JStringToString(LJBarcode.displayValue);
    LBarcode.Format := GetBarcodeFormat(LJBarcode.format);
    LBarcodes := LBarcodes + [LBarcode];
  end;
  if LResults.size = 0 then
    LError := 'No barcodes detected'
  else
    LError := '';
  DoBarcodes(LBarcodes, LError);
end;

end.
