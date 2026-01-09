unit DW.EXIF.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
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
    class function GetMetadataRef(const AFileName: NSString): CFDictionaryRef; overload; static;
    class function GetMetadataRef(const AImageSourceRef: CGImageSourceRef): CFDictionaryRef; overload; static;
    class function GetOrientation(const AValue: Integer): TEXIFOrientation; static;
    class function GetOrientationValue(const AOrientation: TEXIFOrientation): Integer; static;
    class procedure InternalSetGPS(const AMetadata: NSMutableDictionary; const AGPSDetails: TGPSDetails); static;
    class function SaveImageMetadata(const AFileName: NSString; const AImageSourceRef: CGImageSourceRef; const AMetadata: NSDictionary): Boolean; static;
  public
    class function GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean; static;
    class function SetEXIF(const AFileName: string; const AProperties: TEXIFProperties): Boolean; static;
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

function Dictionary(const ADictionaryRef: CFDictionaryRef): NSDictionary;
begin
  if ADictionaryRef <> nil then
    Result := TNSDictionary.Wrap(ADictionaryRef)
  else
    Result := nil;
end;

function MutableDictionary(const ADictionary: NSDictionary): NSMutableDictionary; overload;
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

class function TPlatformEXIF.GetMetadataRef(const AImageSourceRef: CGImageSourceRef): CFDictionaryRef;
begin
  Result := CGImageSourceCopyPropertiesAtIndex(AImageSourceRef, 0, nil);
end;

class function TPlatformEXIF.GetMetadataRef(const AFileName: NSString): CFDictionaryRef;
var
  LImageSourceRef: CGImageSourceRef;
begin
  Result := nil;
  LImageSourceRef := GetImageSourceRef(AFileName);
  if LImageSourceRef <> nil then
  try
    Result := GetMetadataRef(LImageSourceRef);
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

class function TPlatformEXIF.GetOrientationValue(const AOrientation: TEXIFOrientation): Integer;
begin
  case AOrientation of
    TEXIFOrientation.Normal:
      Result := kCGImagePropertyOrientationUp;
    TEXIFOrientation.FlipHorizontal:
      Result := kCGImagePropertyOrientationUpMirrored;
    TEXIFOrientation.Rotate180:
      Result := kCGImagePropertyOrientationDown;
    TEXIFOrientation.FlipVertical:
      Result := kCGImagePropertyOrientationDownMirrored;
    TEXIFOrientation.Rotate270:
      Result := kCGImagePropertyOrientationRight;
    TEXIFOrientation.Transpose:
      Result := kCGImagePropertyOrientationRightMirrored;
    TEXIFOrientation.Rotate90:
      Result := kCGImagePropertyOrientationLeft;
    TEXIFOrientation.Transverse:
      Result := kCGImagePropertyOrientationLeftMirrored;
  else
    Result := kCGImagePropertyOrientationUp;
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
  LMetadataRef := GetMetadataRef(StrToNSStr(AFileName));
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

class function TPlatformEXIF.SetEXIF(const AFileName: string; const AProperties: TEXIFProperties): Boolean;
var
  LImageSourceRef: CGImageSourceRef;
  LMetadataRef: CFDictionaryRef;
  LMetadata, LTIFF, LEXIF: NSMutableDictionary;
  LMetadataHelper: TNSMutableDictionaryHelper;
  LFileName: NSString;
  LGPSDetails: TGPSDetails;
begin
  Result := False;
  LFileName := StrToNSStr(AFileName);
  LImageSourceRef := GetImageSourceRef(StrToNSStr(AFileName));
  if LImageSourceRef <> nil then
  try
    LMetadataRef := GetMetadataRef(LImageSourceRef);
    if LMetadataRef <> nil then
    try
      LMetadata := MutableDictionary(TNSDictionary.Wrap(LMetadataRef));
      LMetadataHelper := TNSMutableDictionaryHelper.Create(LMetadata);
      LMetadataHelper.SetValue(GetOrientationValue(AProperties.Orientation), kCGImagePropertyOrientation);
      LGPSDetails.Altitude := AProperties.Altitude;
      LGPSDetails.Latitude := AProperties.Latitude;
      LGPSDetails.Longitude := AProperties.Longitude;
      InternalSetGPS(LMetadata, LGPSDetails);
      LTIFF := MutableDictionaryOrNew(LMetadata.valueForKey(kCGImagePropertyTIFFDictionary));
      LMetadataHelper := TNSMutableDictionaryHelper.Create(LTIFF);
      LMetadataHelper.SetValue(AProperties.CameraMake, kCGImagePropertyTIFFMake);
      LMetadataHelper.SetValue(AProperties.CameraModel, kCGImagePropertyTIFFModel);
      LMetadata.setObject(NSObjectToID(LMetadataHelper.Dictionary), NSObjectToID(kCGImagePropertyTIFFDictionary));
      LEXIF := MutableDictionaryOrNew(LMetadata.valueForKey(kCGImagePropertyEXIFDictionary));
      LMetadataHelper := TNSMutableDictionaryHelper.Create(LEXIF);
      LMetadataHelper.SetValue(AProperties.DateTaken, kCGImagePropertyExifDateTimeOriginal);
      LMetadata.setObject(NSObjectToID(LMetadataHelper.Dictionary), NSObjectToID(kCGImagePropertyEXIFDictionary));
      Result := SaveImageMetadata(LFileName, LImageSourceRef, LMetadata);
    finally
      CFRelease(LMetadataRef);
    end;
  finally
    CFRelease(LImageSourceRef);
  end;
end;

class procedure TPlatformEXIF.InternalSetGPS(const AMetadata: NSMutableDictionary; const AGPSDetails: TGPSDetails);
const
  cCoordRefLatitude: array[Boolean] of string = ('S', 'N');
  cCoordRefLongitude: array[Boolean] of string = ('W', 'E');
  cAltitudeRef: array[Boolean] of string = ('0', '1');
var
  LGPSHelper: TNSMutableDictionaryHelper;
begin
  LGPSHelper := TNSMutableDictionaryHelper.Create(MutableDictionaryOrNew(AMetadata.valueForKey(kCGImagePropertyGPSDictionary)));
  LGPSHelper.SetValue(Abs(AGPSDetails.Altitude), kCGImagePropertyGPSAltitude);
  LGPSHelper.SetValue(Ord(AGPSDetails.Altitude < 0), kCGImagePropertyGPSAltitudeRef);
  LGPSHelper.SetValue(AGPSDetails.Latitude, kCGImagePropertyGPSLatitude);
  LGPSHelper.SetValue(cCoordRefLatitude[AGPSDetails.Latitude >= 0], kCGImagePropertyGPSLatitudeRef);
  LGPSHelper.SetValue(AGPSDetails.Longitude, kCGImagePropertyGPSLongitude);
  LGPSHelper.SetValue(cCoordRefLongitude[AGPSDetails.Longitude >= 0], kCGImagePropertyGPSLongitudeRef);
  AMetadata.setObject(NSObjectToID(LGPSHelper.Dictionary), NSObjectToID(kCGImagePropertyGPSDictionary));
end;

class function TPlatformEXIF.SetGPS(const AFileName: string; const AGPSDetails: TGPSDetails): Boolean;
var
  LImageSourceRef: CGImageSourceRef;
  LMetadataRef: CFDictionaryRef;
  LMetadata: NSMutableDictionary;
  LFileName: NSString;
begin
  Result := False;
  LFileName := StrToNSStr(AFileName);
  LImageSourceRef := GetImageSourceRef(StrToNSStr(AFileName));
  if LImageSourceRef <> nil then
  try
    LMetadataRef := GetMetadataRef(LImageSourceRef);
    if LMetadataRef <> nil then
    try
      LMetadata := MutableDictionary(TNSDictionary.Wrap(LMetadataRef));
      InternalSetGPS(LMetadata, AGPSDetails);
      Result := SaveImageMetadata(LFileName, LImageSourceRef, LMetadata);
    finally
      CFRelease(LMetadataRef);
    end;
  finally
    CFRelease(LImageSourceRef);
  end;
end;

class function TPlatformEXIF.SaveImageMetadata(const AFileName: NSString; const AImageSourceRef: CGImageSourceRef;
  const AMetadata: NSDictionary): Boolean;
var
  LUTType: CFStringRef;
  LDestinationRef: CGImageDestinationRef;
begin
  LUTType := CGImageSourceGetType(AImageSourceRef);
  {$IF CompilerVersion < 37}
  LDestination := CGImageDestinationCreateWithURL(TNSURL.OCClass.fileURLWithPath(LFileName)), LUTType, 1, nil);
  {$ELSE}
  LDestinationRef := CGImageDestinationCreateWithURL(NSObjectToID(TNSURL.OCClass.fileURLWithPath(AFileName)), LUTType, 1, nil);
  {$ENDIF}
  if LDestinationRef <> nil then
  try
    CGImageDestinationAddImageFromSource(LDestinationRef, AImageSourceRef, 0, NSObjectToID(AMetadata));
    Result := CGImageDestinationFinalize(LDestinationRef) <> 0;
  finally
    CFRelease(LDestinationRef);
  end;
end;

end.
