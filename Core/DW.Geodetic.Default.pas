unit DW.Geodetic.Default;

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
  // RTL
  System.Math;

{ TPlatformGeodetic }

class function TPlatformGeodetic.DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double;
const
  cDiameter = 2 * 6372800;
var
  LDX, LDY, LDZ: Double;
  LLatRadFrom, LLatRadTo, LLongRadDiff: Double;
begin
  LLongRadDiff := DegToRad(ALongitudeFrom - ALongitudeTo);
  LLatRadFrom := DegToRad(ALatitudeFrom);
  LLatRadTo := DegToRad(ALatitudeTo);
  LDZ := Sin(LLatRadFrom) - Sin(LLatRadTo);
  LDX := Cos(LLongRadDiff) * Cos(LLatRadFrom) - Cos(LLatRadTo);
  LDY := Sin(LLongRadDiff) * Cos(LLatRadFrom);
  Result := ArcSin(Sqrt(Sqr(LDX) + Sqr(LDY) + Sqr(LDZ)) / 2) * cDiameter;
end;

end.
