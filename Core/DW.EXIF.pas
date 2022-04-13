unit DW.EXIF;

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

type
  TEXIFOrientation = (Unknown, Normal, FlipHorizontal, Rotate180, FlipVertical, Transpose, Rotate90, Transverse, Rotate270);

  TEXIFProperties = record
    Altitude: Double;
    CameraMake: string;
    CameraModel: string;
    DateTaken: string;
    Longitude: Double;
    Latitude: Double;
    Orientation: TEXIFOrientation;
  end;

  /// <summary>
  ///   Helper for extracting EXIF data on Android and iOS
  /// </summary>
  TEXIF = record
  public
    /// <summary>
    ///   Extract EXIF data from the specified file
    /// </summary>
    class function GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean; static;
  end;

implementation

// DW
{$IF Defined(IOS)}
uses
  DW.EXIF.iOS;
{$ENDIF}
{$IF Defined(ANDROID)}
uses
  DW.EXIF.Android;
{$ENDIF}

{ TEXIF }

class function TEXIF.GetEXIF(const AFileName: string; out AProperties: TEXIFProperties): Boolean;
begin
  AProperties.Latitude := 0;
  AProperties.Longitude := 0;
  Result := TPlatformEXIF.GetEXIF(AFileName, AProperties);
end;

end.
