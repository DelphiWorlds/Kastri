unit DW.EXIF.iOS;

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
  // Mac
  Macapi.CoreFoundation,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.EXIF;

type
  TPlatformEXIF = record
  private
    class function GetCoordRefOffset(const ACoordRef: string): Integer; static;
    class function GetMetadata(const AFileName: NSString): CFDictionaryRef; static;
    class function GetOrientation(const AValue: Integer): TEXIFOrientation; static;
  public
    class function GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Mac
  Macapi.Helpers,
  // iOS
  iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.ImageIO, DW.Macapi.Helpers;

{ TPlatformEXIF }

class function TPlatformEXIF.GetCoordRefOffset(const ACoordRef: string): Integer;
begin
  if ACoordRef.Equals('E') or ACoordRef.Equals('S') then
    Result := -1
  else
    Result := 1;
end;

class function TPlatformEXIF.GetMetadata(const AFileName: NSString): CFDictionaryRef;
var
  LDataRef: CFDataRef;
  LImageSourceRef: CGImageSourceRef;
begin
  Result := nil;
  LDataRef := TNSData.OCClass.dataWithContentsOfURL(TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(AFileName)));
  if LDataRef <> nil then
  begin
    LImageSourceRef := CGImageSourceCreateWithData(LDataRef, nil);
    if LImageSourceRef <> nil then
    try
      Result := CGImageSourceCopyPropertiesAtIndex(LImageSourceRef, 0, nil);
    finally
      CFRelease(LImageSourceRef);
    end;
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
  LCoordRef: string;
begin
  Result := False;
  LMetadata := nil;
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

end.
