unit DW.Geodetic;

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
  // RTL
  System.Sensors;

type
  TGeodetic = record
  public
    /// <summary>
    ///   Calculates distance between two locations, in metres
    /// </summary>
    /// <remarks>
    ///   At present, untested. Result may vary between OS's, and might change in future if their algorithm is changed
    /// </remarks>
    class function DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double; overload; static;
    class function DistanceBetween(const ALocationFrom: TLocationCoord2D; const ALocationTo: TLocationCoord2D): Double; overload; static;
  end;

implementation

uses
  {$IF Defined(ANDROID)}
  DW.Geodetic.Android;
  {$ELSEIF Defined(IOS)}
  DW.Geodetic.iOS;
  {$ELSE}
  DW.Geodetic.Default;
  {$ENDIF}

{ TGeodetic }

class function TGeodetic.DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double;
begin
  Result := TPlatformGeodetic.DistanceBetween(ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo);
end;

class function TGeodetic.DistanceBetween(const ALocationFrom, ALocationTo: TLocationCoord2D): Double;
begin
  Result := DistanceBetween(ALocationFrom.Latitude, ALocationFrom.Longitude, ALocationTo.Latitude, ALocationTo.Longitude);
end;

end.
