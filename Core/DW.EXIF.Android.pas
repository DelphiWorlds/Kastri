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
  // DW
  DW.EXIF;

type
  TPlatformEXIF = record
  private
    class function GetOrientation(const AValue: Integer): TEXIFOrientation; static;
  public
    class function GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNI.Media, Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.JavaTypes;

{ TPlatformEXIF }

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
