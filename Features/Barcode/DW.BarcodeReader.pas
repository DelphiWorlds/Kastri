unit DW.BarcodeReader;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

uses
  // FMX
  FMX.Controls, FMX.Graphics;

type
  TBarcodeFormat = (All, Aztec, Codabar, Code128, Code39, Code93, DataMatrix, EAN13, EAN8, ITF, PDF417, QR, UPCA, UPCE, Unknown);
  TBarcodeFormats = set of TBarcodeFormat;

  TBarcode = record
    Format: TBarcodeFormat;
    Value: string;
    function FormatDescription: string;
  end;

  TBarcodes = TArray<TBarcode>;

  TBarcodeReader = class;

  TCustomPlatformBarcodeReader = class(TObject)
  private
    FReader: TBarcodeReader;
    FFormats: TBarcodeFormats;
  protected
    procedure DoBarcodes(const ABarcodes: TBarcodes; const AError: string);
    procedure Scan(const ABitmap: TBitmap); virtual;
    property Formats: TBarcodeFormats read FFormats write FFormats;
    property Reader: TBarcodeReader read FReader;
  public
    constructor Create(const AReader: TBarcodeReader); virtual;
    destructor Destroy; override;
  end;

  TBarcodeEvent = procedure(Sender: TObject; const Barcodes: TBarcodes; const Error: string) of object;

  TBarcodeReader = class(TObject)
  private
    FPlatformReader: TCustomPlatformBarcodeReader;
    FOnBarcode: TBarcodeEvent;
    function GetFormats: TBarcodeFormats;
    procedure SetFormats(const Value: TBarcodeFormats);
  protected
    procedure DoBarcodes(const ABarcodes: TBarcodes; const AError: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Scan(const ABitmap: TBitmap);
    property Formats: TBarcodeFormats read GetFormats write SetFormats;
    property OnBarcode: TBarcodeEvent read FOnBarcode write FOnBarcode;
  end;

implementation

uses
  // DW
  {$IF Defined(IOS)}
  DW.BarcodeReader.iOS;
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.BarcodeReader.Android;
  {$ENDIF}

const
  cFormatDescriptions: array[TBarcodeFormat] of string = (
    'All', 'Aztec', 'Codabar', 'Code-128', 'Code-39', 'Code-93', 'DataMatrix', 'EAN-13', 'EAN-8', 'ITF', 'PDF-417', 'QR Code', 'UPC-A', 'UPC-E', 'Unknown'
  );

{ TBarcode }

function TBarcode.FormatDescription: string;
begin
  Result := cFormatDescriptions[Format];
end;

{ TCustomPlatformBarcodeReader }

constructor TCustomPlatformBarcodeReader.Create(const AReader: TBarcodeReader);
begin
  inherited Create;
  FReader := AReader;
end;

destructor TCustomPlatformBarcodeReader.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformBarcodeReader.DoBarcodes(const ABarcodes: TBarcodes; const AError: string);
begin
  FReader.DoBarcodes(ABarcodes, AError);
end;

procedure TCustomPlatformBarcodeReader.Scan(const ABitmap: TBitmap);
begin
  //
end;

{ TBarcodeReader }

constructor TBarcodeReader.Create;
begin
  inherited;
  FPlatformReader := TPlatformBarcodeReader.Create(Self);
end;

destructor TBarcodeReader.Destroy;
begin
  FPlatformReader.Free;
  inherited;
end;

procedure TBarcodeReader.DoBarcodes(const ABarcodes: TBarcodes; const AError: string);
begin
  if Assigned(FOnBarcode) then
    FOnBarcode(Self, ABarcodes, AError);
end;

function TBarcodeReader.GetFormats: TBarcodeFormats;
begin
  Result := FPlatformReader.Formats;
end;

procedure TBarcodeReader.Scan(const ABitmap: TBitmap);
begin
  FPlatformReader.Scan(ABitmap);
end;

procedure TBarcodeReader.SetFormats(const Value: TBarcodeFormats);
begin
  FPlatformReader.Formats := Value;
end;

end.
