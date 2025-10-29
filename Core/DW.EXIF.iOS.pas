unit DW.EXIF.iOS;

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

uses
  // Mac
  Macapi.CoreFoundation,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.EXIF, DW.iOSapi.ImageIO;

type
  TPlatformEXIF = record
  private
    class function GetCoordRefOffset(const ACoordRef: string): Integer; static;
    class function GetImageSourceRef(const AFileName: NSString): CGImageSourceRef; static;
    class function GetMetadata(const AFileName: NSString): CFDictionaryRef; overload; static;
    class function GetMetadata(const AImageSourceRef: CGImageSourceRef): CFDictionaryRef; overload; static;
    class function GetOrientation(const AValue: Integer): TEXIFOrientation; static;
  public
    class function GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean; static;
    class function SetGPS(const AFileName: string; const AGPSDetails: TGPSDetails): Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Mac
  Macapi.Helpers, Macapi.ObjectiveC,
  // iOS
  iOSapi.CoreGraphics,
  // DW
  DW.Macapi.Helpers;

type
  NSDictionaryEx = interface(NSDictionary)
    ['{7D84A059-CA48-484A-A1CD-2278FC573546}']
    function mutableCopy: Pointer; cdecl;
  end;
  TNSDictionaryEx = class(TOCGenericImport<NSDictionaryClass, NSDictionaryEx>) end;

function MutableDictionary(const ADictionary: NSDictionary): NSMutableDictionary;
var
  LDictionaryEx: NSDictionaryEx;
begin
  LDictionaryEx := TNSDictionaryEx.Wrap(NSObjectToID(ADictionary));
  Result := TNSMutableDictionary.Wrap(LDictionaryEx.mutableCopy);
end;

function MutableDictionaryOrNew(const APointer: Pointer): NSMutableDictionary;
begin
  if APointer <> nil then
    Result := MutableDictionary(TNSDictionary.Wrap(APointer))
  else
    Result := TNSMutableDictionary.Create;
end;

function GetDictionaryIntegerValue(const ADictionary: NSDictionary; const AKey: NSString; const ADefault: Integer = 0): Integer;
var
  LValuePtr: Pointer;
begin
  Result := ADefault;
  LValuePtr := ADictionary.valueForKey(AKey);
  if LValuePtr <> nil then
    Result := TNSNumber.Wrap(LValuePtr).integerValue;
end;

{ TPlatformEXIF }

class function TPlatformEXIF.GetCoordRefOffset(const ACoordRef: string): Integer;
begin
  if ACoordRef.Equals('W') or ACoordRef.Equals('S') then
    Result := -1
  else
    Result := 1;
end;

class function TPlatformEXIF.GetImageSourceRef(const AFileName: NSString): CGImageSourceRef;
var
  LDataRef: CFDataRef;
begin
  Result := nil;
  {$IF CompilerVersion < 37}
  LDataRef := TNSData.OCClass.dataWithContentsOfURL(TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(AFileName)));
  {$ELSE}
  LDataRef := TNSData.OCClass.dataWithContentsOfURL(TNSURL.OCClass.fileURLWithPath(AFileName));
  {$ENDIF}
  if LDataRef <> nil then
    Result := CGImageSourceCreateWithData(LDataRef, nil);
end;

class function TPlatformEXIF.GetMetadata(const AImageSourceRef: CGImageSourceRef): CFDictionaryRef;
begin
  Result := CGImageSourceCopyPropertiesAtIndex(AImageSourceRef, 0, nil);
end;

class function TPlatformEXIF.GetMetadata(const AFileName: NSString): CFDictionaryRef;
var
  LImageSourceRef: CGImageSourceRef;
begin
  Result := nil;
  LImageSourceRef := GetImageSourceRef(AFileName);
  if LImageSourceRef <> nil then
  try
    Result := GetMetadata(LImageSourceRef);
  finally
    CFRelease(LImageSourceRef);
  end;
end;

class function TPlatformEXIF.GetOrientation(const AValue: Integer): TEXIFOrientation;
begin
  case AValue of
    0, kCGImagePropertyOrientationUp:
      Result := TEXIFOrientation.Normal;
    kCGImagePropertyOrientationUpMirrored:
      Result := TEXIFOrientation.FlipHorizontal;
    kCGImagePropertyOrientationDown:
      Result := TEXIFOrientation.Rotate180;
    kCGImagePropertyOrientationDownMirrored:
      Result := TEXIFOrientation.FlipVertical;
    kCGImagePropertyOrientationRight:
      Result := TEXIFOrientation.Rotate270;
    kCGImagePropertyOrientationRightMirrored:
      Result := TEXIFOrientation.Transpose;
    kCGImagePropertyOrientationLeft:
      Result := TEXIFOrientation.Rotate90;
    kCGImagePropertyOrientationLeftMirrored:
      Result := TEXIFOrientation.Transverse;
  else
    Result := TEXIFOrientation.Unknown;
  end;
end;

class function TPlatformEXIF.GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean;
var
  LMetadataRef: CFDictionaryRef;
  LMetadata, LEXIF, LGPS, LTIFF: NSDictionary;
  LCoordValue: Double;
  LAltitudeRef: Integer;
  LCoordRef: string;
begin
  Result := False;
  LMetadataRef := GetMetadata(StrToNSStr(AFileName));
  if LMetadataRef <> nil then
  try
    LMetadata := TNSDictionary.Wrap(LMetadataRef);
    Result := True;
    AProperties.Orientation := GetOrientation(Round(GetDictionaryNumberValue(LMetadata, kCGImagePropertyOrientation)));
    LEXIF := TNSDictionary.Wrap(LMetadata.valueForKey(kCGImagePropertyExifDictionary));
    if LEXIF <> nil then
      AProperties.DateTaken := GetDictionaryStringValue(LEXIF, kCGImagePropertyExifDateTimeOriginal);
    LGPS := TNSDictionary.Wrap(LMetadata.valueForKey(kCGImagePropertyGPSDictionary));
    if LGPS <> nil then
    begin
      AProperties.Altitude := GetDictionaryNumberValue(LGPS, kCGImagePropertyGPSAltitude);
      LAltitudeRef := GetDictionaryIntegerValue(LGPS, kCGImagePropertyGPSAltitudeRef);
      if LAltitudeRef = 1 then
        AProperties.Altitude := -AProperties.Altitude;
      LCoordValue := GetDictionaryNumberValue(LGPS, kCGImagePropertyGPSLatitude);
      LCoordRef := GetDictionaryStringValue(LGPS, kCGImagePropertyGPSLatitudeRef);
      if not LCoordRef.IsEmpty then
        AProperties.Latitude := GetCoordRefOffset(LCoordRef) * LCoordValue;
      LCoordValue := GetDictionaryNumberValue(LGPS, kCGImagePropertyGPSLongitude);
      LCoordRef := GetDictionaryStringValue(LGPS, kCGImagePropertyGPSLongitudeRef);
      if not LCoordRef.IsEmpty then
        AProperties.Longitude := GetCoordRefOffset(LCoordRef) * LCoordValue;
    end;
    LTIFF := TNSDictionary.Wrap(LMetadata.valueForKey(kCGImagePropertyTIFFDictionary));
    if LTIFF <> nil then
    begin
      AProperties.CameraMake := GetDictionaryStringValue(LTIFF, kCGImagePropertyTIFFMake);
      AProperties.CameraModel := GetDictionaryStringValue(LTIFF, kCGImagePropertyTIFFModel);
    end;
  finally
    CFRelease(LMetadataRef);
  end;
end;

class function TPlatformEXIF.SetGPS(const AFileName: string; const AGPSDetails: TGPSDetails): Boolean;
const
  cCoordRefLatitude: array[Boolean] of string = ('S', 'N');
  cCoordRefLongitude: array[Boolean] of string = ('W', 'E');
  cAltitudeRef: array[Boolean] of string = ('0', '1');
var
  LImageSourceRef: CGImageSourceRef;
  LMetadataRef: CFDictionaryRef;
  LMetadata: NSMutableDictionary;
  LGPSHelper: TNSMutableDictionaryHelper;
  LUTType: CFStringRef;
  LDestinationRef: CGImageDestinationRef;
  LFileName: NSString;
begin
  Result := False;
  LFileName := StrToNSStr(AFileName);
  LImageSourceRef := GetImageSourceRef(StrToNSStr(AFileName));
  if LImageSourceRef <> nil then
  try
    LMetadataRef := GetMetadata(LImageSourceRef);
    if LMetadataRef <> nil then
    try
      LMetadata := MutableDictionary(TNSDictionary.Wrap(LMetadataRef));
      LGPSHelper := TNSMutableDictionaryHelper.Create(MutableDictionaryOrNew(LMetadata.valueForKey(kCGImagePropertyGPSDictionary)));
      LGPSHelper.SetValue(Abs(AGPSDetails.Altitude), kCGImagePropertyGPSAltitude);
      LGPSHelper.SetValue(Ord(AGPSDetails.Altitude < 0), kCGImagePropertyGPSAltitudeRef);
      LGPSHelper.SetValue(AGPSDetails.Latitude, kCGImagePropertyGPSLatitude);
      LGPSHelper.SetValue(cCoordRefLatitude[AGPSDetails.Latitude >= 0], kCGImagePropertyGPSLatitudeRef);
      LGPSHelper.SetValue(AGPSDetails.Longitude, kCGImagePropertyGPSLongitude);
      LGPSHelper.SetValue(cCoordRefLongitude[AGPSDetails.Longitude >= 0], kCGImagePropertyGPSLongitudeRef);
      LMetadata.setObject(NSObjectToID(LGPSHelper.Dictionary), NSObjectToID(kCGImagePropertyGPSDictionary));
      LUTType := CGImageSourceGetType(LImageSourceRef);
      {$IF CompilerVersion < 37}
      LDestination := CGImageDestinationCreateWithURL(TNSURL.OCClass.fileURLWithPath(LFileName)), LUTType, 1, nil);
      {$ELSE}
      LDestinationRef := CGImageDestinationCreateWithURL(NSObjectToID(TNSURL.OCClass.fileURLWithPath(LFileName)), LUTType, 1, nil);
      {$ENDIF}
      if LDestinationRef <> nil then
      try
        CGImageDestinationAddImageFromSource(LDestinationRef, LImageSourceRef, 0, NSObjectToID(LMetadata));
        Result := CGImageDestinationFinalize(LDestinationRef) <> 0;
      finally
        CFRelease(LDestinationRef);
      end;
    finally
      CFRelease(LMetadataRef);
    end;
  finally
    CFRelease(LImageSourceRef);
  end;
end;

end.
