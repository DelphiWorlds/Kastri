unit DW.EXIF.Android;

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
  // DW
  DW.EXIF;

type
  TPlatformEXIF = record
  public
    class function GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean; static;
  end;

implementation

uses
  // Android
  Androidapi.JNI.Media, Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.JavaTypes;

{ TPlatformEXIF }

class function TPlatformEXIF.GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean;
var
  LEXIF: JExifInterface;
  LLatLong: TJavaArray<Single>;
begin
  LEXIF := TJExifInterface.JavaClass.init(StringToJString(AFileName));
  AProperties.DateTaken := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_DATETIME));
  AProperties.CameraMake := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_MAKE));
  AProperties.CameraModel := JStringToString(LEXIF.getAttribute(TJExifInterface.JavaClass.TAG_MODEL));
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
