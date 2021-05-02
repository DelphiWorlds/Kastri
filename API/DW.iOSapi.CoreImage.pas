unit DW.iOSapi.CoreImage;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS 
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreVideo;

const
  CIQRCodeErrorCorrectionLevelL = 76;
  CIQRCodeErrorCorrectionLevelM = 77;
  CIQRCodeErrorCorrectionLevelQ = 81;
  CIQRCodeErrorCorrectionLevelH = 72;
  CIDataMatrixCodeECCVersion000 = 0;
  CIDataMatrixCodeECCVersion050 = 50;
  CIDataMatrixCodeECCVersion080 = 80;
  CIDataMatrixCodeECCVersion100 = 100;
  CIDataMatrixCodeECCVersion140 = 140;
  CIDataMatrixCodeECCVersion200 = 200;

type
  CIBarcodeDescriptor = interface;
  CIQRCodeDescriptor = interface;
  CIAztecCodeDescriptor = interface;
  CIPDF417CodeDescriptor = interface;
  CIDataMatrixCodeDescriptor = interface;

  CIQRCodeErrorCorrectionLevel = NSInteger;
  CIDataMatrixCodeECCVersion = NSInteger;

  CIBarcodeDescriptorClass = interface(NSObjectClass)
    ['{20B5172F-C16C-4CEE-B2C8-5904273C4EC8}']
  end;

  CIBarcodeDescriptor = interface(NSObject)
    ['{D692ADDC-FCA2-443B-8C62-53316DD878DB}']
  end;
  TCIBarcodeDescriptor = class(TOCGenericImport<CIBarcodeDescriptorClass, CIBarcodeDescriptor>) end;

  CIQRCodeDescriptorClass = interface(CIBarcodeDescriptorClass)
    ['{7FF9671D-37D6-4BF0-B2D6-7D3B28CE5307}']
    [MethodName('descriptorWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:')]
    {class} function descriptorWithPayload(errorCorrectedPayload: NSData; symbolVersion: NSInteger; maskPattern: UInt8;
      errorCorrectionLevel: CIQRCodeErrorCorrectionLevel): Pointer; cdecl;
  end;

  CIQRCodeDescriptor = interface(CIBarcodeDescriptor)
    ['{D5CA608E-F702-44D8-BE5C-2EB73D263421}']
    function errorCorrectedPayload: NSData; cdecl;
    function errorCorrectionLevel: CIQRCodeErrorCorrectionLevel; cdecl;
    [MethodName('initWithPayload:symbolVersion:maskPattern:errorCorrectionLevel:')]
    function initWithPayload(errorCorrectedPayload: NSData; symbolVersion: NSInteger; maskPattern: UInt8;
      errorCorrectionLevel: CIQRCodeErrorCorrectionLevel): Pointer; cdecl;
    function maskPattern: UInt8; cdecl;
    function symbolVersion: NSInteger; cdecl;
  end;
  TCIQRCodeDescriptor = class(TOCGenericImport<CIQRCodeDescriptorClass, CIQRCodeDescriptor>) end;

  CIAztecCodeDescriptorClass = interface(CIBarcodeDescriptorClass)
    ['{2F0A8DC9-A776-49FA-BC7E-566D99F124D7}']
    [MethodName('descriptorWithPayload:isCompact:layerCount:dataCodewordCount:')]
    {class} function descriptorWithPayload(errorCorrectedPayload: NSData; isCompact: Boolean; layerCount: NSInteger;
      dataCodewordCount: NSInteger): Pointer; cdecl;
  end;

  CIAztecCodeDescriptor = interface(CIBarcodeDescriptor)
    ['{291CE11D-9392-4386-96CD-F88ADD17326B}']
    function dataCodewordCount: NSInteger; cdecl;
    function errorCorrectedPayload: NSData; cdecl;
    [MethodName('initWithPayload:isCompact:layerCount:dataCodewordCount:')]
    function initWithPayload(errorCorrectedPayload: NSData; isCompact: Boolean; layerCount: NSInteger; dataCodewordCount: NSInteger): Pointer; cdecl;
    function isCompact: Boolean; cdecl;
    function layerCount: NSInteger; cdecl;
  end;
  TCIAztecCodeDescriptor = class(TOCGenericImport<CIAztecCodeDescriptorClass, CIAztecCodeDescriptor>) end;

  CIPDF417CodeDescriptorClass = interface(CIBarcodeDescriptorClass)
    ['{73E7E543-16B5-4621-904D-4DE17A061C29}']
    [MethodName('descriptorWithPayload:isCompact:rowCount:columnCount:')]
    {class} function descriptorWithPayload(errorCorrectedPayload: NSData; isCompact: Boolean; rowCount: NSInteger;
      columnCount: NSInteger): Pointer; cdecl;
  end;

  CIPDF417CodeDescriptor = interface(CIBarcodeDescriptor)
    ['{6F434B26-DB3F-49D7-8EAE-4FA2601DD337}']
    function columnCount: NSInteger; cdecl;
    function errorCorrectedPayload: NSData; cdecl;
    [MethodName('initWithPayload:isCompact:rowCount:columnCount:')]
    function initWithPayload(errorCorrectedPayload: NSData; isCompact: Boolean; rowCount: NSInteger; columnCount: NSInteger): Pointer; cdecl;
    function isCompact: Boolean; cdecl;
    function rowCount: NSInteger; cdecl;
  end;
  TCIPDF417CodeDescriptor = class(TOCGenericImport<CIPDF417CodeDescriptorClass, CIPDF417CodeDescriptor>) end;

  CIDataMatrixCodeDescriptorClass = interface(CIBarcodeDescriptorClass)
    ['{DF51E28E-ABB1-4203-A7EC-D3975FCD6466}']
    [MethodName('descriptorWithPayload:rowCount:columnCount:eccVersion:')]
    {class} function descriptorWithPayload(errorCorrectedPayload: NSData; rowCount: NSInteger; columnCount: NSInteger;
      eccVersion: CIDataMatrixCodeECCVersion): Pointer; cdecl;
  end;

  CIDataMatrixCodeDescriptor = interface(CIBarcodeDescriptor)
    ['{EAB96705-3B53-457F-926B-48A592309F46}']
    function columnCount: NSInteger; cdecl;
    function eccVersion: CIDataMatrixCodeECCVersion; cdecl;
    function errorCorrectedPayload: NSData; cdecl;
    [MethodName('initWithPayload:rowCount:columnCount:eccVersion:')]
    function initWithPayload(errorCorrectedPayload: NSData; rowCount: NSInteger; columnCount: NSInteger;
      eccVersion: CIDataMatrixCodeECCVersion): Pointer; cdecl;
    function rowCount: NSInteger; cdecl;
  end;
  TCIDataMatrixCodeDescriptor = class(TOCGenericImport<CIDataMatrixCodeDescriptorClass, CIDataMatrixCodeDescriptor>) end;

function CIDetectorTypeFace: NSString;
function CIDetectorTypeRectangle: NSString;
function CIDetectorTypeQRCode: NSString;
function CIDetectorTypeText: NSString;
function CIDetectorAccuracy: NSString;
function CIDetectorAccuracyLow: NSString;
function CIDetectorAccuracyHigh: NSString;
function CIDetectorTracking: NSString;
function CIDetectorMinFeatureSize: NSString;
function CIDetectorMaxFeatureCount: NSString;
function CIDetectorNumberOfAngles: NSString;
function CIDetectorImageOrientation: NSString;
function CIDetectorEyeBlink: NSString;
function CIDetectorSmile: NSString;
function CIDetectorFocalLength: NSString;
function CIDetectorAspectRatio: NSString;
function CIDetectorReturnSubFeatures: NSString;
function CIFeatureTypeFace: NSString;
function CIFeatureTypeRectangle: NSString;
function CIFeatureTypeQRCode: NSString;
function CIFeatureTypeText: NSString;

const
  libCoreImage = '/System/Library/Frameworks/CoreImage.framework/CoreImage';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  CoreImageModule: THandle;
{$ENDIF}

function CIDetectorTypeFace: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorTypeFace');
end;

function CIDetectorTypeRectangle: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorTypeRectangle');
end;

function CIDetectorTypeQRCode: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorTypeQRCode');
end;

function CIDetectorTypeText: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorTypeText');
end;

function CIDetectorAccuracy: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorAccuracy');
end;

function CIDetectorAccuracyLow: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorAccuracyLow');
end;

function CIDetectorAccuracyHigh: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorAccuracyHigh');
end;

function CIDetectorTracking: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorTracking');
end;

function CIDetectorMinFeatureSize: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorMinFeatureSize');
end;

function CIDetectorMaxFeatureCount: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorMaxFeatureCount');
end;

function CIDetectorNumberOfAngles: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorNumberOfAngles');
end;

function CIDetectorImageOrientation: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorImageOrientation');
end;

function CIDetectorEyeBlink: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorEyeBlink');
end;

function CIDetectorSmile: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorSmile');
end;

function CIDetectorFocalLength: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorFocalLength');
end;

function CIDetectorAspectRatio: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorAspectRatio');
end;

function CIDetectorReturnSubFeatures: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorReturnSubFeatures');
end;

function CIFeatureTypeFace: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIFeatureTypeFace');
end;

function CIFeatureTypeRectangle: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIFeatureTypeRectangle');
end;

function CIFeatureTypeQRCode: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIFeatureTypeQRCode');
end;

function CIFeatureTypeText: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIFeatureTypeText');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  CoreImageModule := dlopen(MarshaledAString(libCoreImage), RTLD_LAZY);

finalization
  dlclose(CoreImageModule)
{$ENDIF}

end.