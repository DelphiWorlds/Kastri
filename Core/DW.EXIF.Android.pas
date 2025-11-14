unit DW.EXIF.Android;

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
  // Android
  Androidapi.JNI.JavaTypes,
  // DW
  DW.EXIF;

type
  TPlatformEXIF = record
  private
    class function GetCoordString(const AValue: Double): JString; static;
    class function GetOrientation(const AValue: Integer): TEXIFOrientation; static;
    class function GetOrientationValue(const AOrientation: TEXIFOrientation): Integer; static;
    class function GetRationalString(const AValue: Double; const ADivisor: Integer): JString; static;
  public
    class function GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean; static;
    class function SetEXIF(const AFileName: string; const AProperties: TEXIFProperties): Boolean; static;
    class function SetGPS(const AFileName: string; const AGPSDetails: TGPSDetails): Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNI.Media, Androidapi.JNIBridge, Androidapi.Helpers;

{ TPlatformEXIF }

class function TPlatformEXIF.GetCoordString(const AValue: Double): JString;
var
  LCoord, LSeconds: Double;
  LDegrees, LMinutes: Integer;
begin
  LCoord := Abs(AValue);
  LDegrees := Trunc(LCoord);
  LCoord := (LCoord - LDegrees) * 60;
  LMinutes := Trunc(LCoord);
  LSeconds := (LCoord - LMinutes) * 60;
  Result := StringToJString(Format('%d/1,%d/1,%d/1000', [LDegrees, Trunc(LMinutes), Trunc(LSeconds * 1000)]));
end;

class function TPlatformEXIF.GetOrientation(const AValue: Integer): TEXIFOrientation;
begin
  if AValue = 0 then
    Result := TEXIFOrientation.Normal
  else if AValue = TJExifInterface.JavaClass.ORIENTATION_NORMAL then
    Result := TEXIFOrientation.Normal
  else if AValue = TJExifInterface.JavaClass.ORIENTATION_FLIP_HORIZONTAL then
    Result := TEXIFOrientation.FlipHorizontal
  else if AValue = TJExifInterface.JavaClass.ORIENTATION_ROTATE_180 then
    Result := TEXIFOrientation.Rotate180
  else if AValue = TJExifInterface.JavaClass.ORIENTATION_FLIP_VERTICAL then
    Result := TEXIFOrientation.FlipVertical
  else if AValue = TJExifInterface.JavaClass.ORIENTATION_TRANSPOSE then
    Result := TEXIFOrientation.Transpose
  else if AValue = TJExifInterface.JavaClass.ORIENTATION_ROTATE_270 then
    Result := TEXIFOrientation.Rotate90
  else if AValue = TJExifInterface.JavaClass.ORIENTATION_TRANSVERSE then
    Result := TEXIFOrientation.Transverse
  else if AValue = TJExifInterface.JavaClass.ORIENTATION_ROTATE_90 then
    Result := TEXIFOrientation.Rotate270
  else
    Result := TEXIFOrientation.Unknown;
end;

class function TPlatformEXIF.GetOrientationValue(const AOrientation: TEXIFOrientation): Integer;
begin
  case AOrientation of
    TEXIFOrientation.Normal:
      Result := TJExifInterface.JavaClass.ORIENTATION_NORMAL;
    TEXIFOrientation.FlipHorizontal:
      Result := TJExifInterface.JavaClass.ORIENTATION_FLIP_HORIZONTAL;
    TEXIFOrientation.Rotate180:
      Result := TJExifInterface.JavaClass.ORIENTATION_ROTATE_180;
    TEXIFOrientation.FlipVertical:
      Result := TJExifInterface.JavaClass.ORIENTATION_FLIP_VERTICAL;
    TEXIFOrientation.Rotate270:
      Result := TJExifInterface.JavaClass.ORIENTATION_ROTATE_90;
    TEXIFOrientation.Transpose:
      Result := TJExifInterface.JavaClass.ORIENTATION_TRANSPOSE;
    TEXIFOrientation.Rotate90:
      Result := TJExifInterface.JavaClass.ORIENTATION_ROTATE_270;
    TEXIFOrientation.Transverse:
      Result := TJExifInterface.JavaClass.ORIENTATION_TRANSVERSE;
  else
    Result := TJExifInterface.JavaClass.ORIENTATION_NORMAL;
  end;
end;

class function TPlatformEXIF.GetRationalString(const AValue: Double; const ADivisor: Integer): JString;
begin
  Result := StringToJString(Format('%d/%d', [Trunc(AValue * ADivisor), ADivisor]));
end;

class function TPlatformEXIF.SetEXIF(const AFileName: string; const AProperties: TEXIFProperties): Boolean;
var
  LGPSDetails: TGPSDetails;
  LEXIF: JExifInterface;
begin
  LGPSDetails.Altitude := AProperties.Altitude;
  LGPSDetails.Latitude := AProperties.Latitude;
  LGPSDetails.Longitude := AProperties.Longitude;
  SetGPS(AFileName, LGPSDetails);
  LEXIF := TJExifInterface.JavaClass.init(StringToJString(AFileName));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_DATETIME, StringToJString(AProperties.DateTaken));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_MAKE, StringToJString(AProperties.CameraMake));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_MODEL, StringToJString(AProperties.CameraModel));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_ORIENTATION, StringToJString(IntToStr(GetOrientationValue(AProperties.Orientation))));
  LEXIF.saveAttributes;
  Result := True;
end;

class function TPlatformEXIF.SetGPS(const AFileName: string; const AGPSDetails: TGPSDetails): Boolean;
const
  cCoordRefLatitude: array[Boolean] of string = ('S', 'N');
  cCoordRefLongitude: array[Boolean] of string = ('W', 'E');
  cAltitudeRef: array[Boolean] of string = ('0', '1');
var
  LEXIF: JExifInterface;
  LLatLong: TJavaArray<Single>;
begin
  LEXIF := TJExifInterface.JavaClass.init(StringToJString(AFileName));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_ALTITUDE, GetRationalString(Abs(AGPSDetails.Altitude), 100));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_ALTITUDE_REF, StringToJString(cAltitudeRef[AGPSDetails.Altitude >= 0]));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_LATITUDE, GetCoordString(Abs(AGPSDetails.Latitude)));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_LATITUDE_REF, StringToJString(cCoordRefLatitude[AGPSDetails.Latitude >= 0]));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_LONGITUDE, GetCoordString(Abs(AGPSDetails.Longitude)));
  LEXIF.setAttribute(TJExifInterface.JavaClass.TAG_GPS_LONGITUDE_REF, StringToJString(cCoordRefLongitude[AGPSDetails.Longitude >= 0]));
  LEXIF.saveAttributes;
  Result := True;
end;

class function TPlatformEXIF.GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean;
var
  LEXIF: JExifInterface;
  LLatLong: TJavaArray<Single>;
begin
  LEXIF := TJExifInterface.JavaClass.init(StringToJString(AFileName));
  AProperties.DateTaken := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_DATETIME));
  AProperties.CameraMake := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_MAKE));
  AProperties.CameraModel := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_MODEL));
  AProperties.Orientation := GetOrientation(StrToIntDef(JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_ORIENTATION)), -1));
  AProperties.Altitude := LEXIF.getAltitude(0);
  LLatLong := TJavaArray<Single>.Create(2);
  try
    if LEXIF.getLatLong(LLatLong) then
    begin
      AProperties.Latitude := LLatLong.Items[0];
      AProperties.Longitude := LLatLong.Items[1];
    end;
  finally
    LLatLong.Free;
  end;
  Result := True;
end;

end.
