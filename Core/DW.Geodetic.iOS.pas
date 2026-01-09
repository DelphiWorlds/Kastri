unit DW.Geodetic.iOS;

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

type
  TPlatformGeodetic = record
  public
    class function DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double; static;
  end;

implementation

uses
  // iOS
  iOSapi.CoreLocation;

{ TPlatformGeodetic }

class function TPlatformGeodetic.DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double;
var
  LLocationFrom, LLocationTo: CLLocation;
begin
  LLocationFrom := TCLLocation.Create;
  LLocationFrom.initWithLatitude(ALatitudeFrom, ALongitudeFrom);
  LLocationTo := TCLLocation.Create;
  LLocationTo.initWithLatitude(ALatitudeTo, ALongitudeTo);
  Result := LLocationTo.distanceFromLocation(LLocationFrom);
end;

end.
