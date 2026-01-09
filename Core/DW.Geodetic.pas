unit DW.Geodetic;

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
    /// <summary>
    ///   Finds the centre of a polygon of points. Uses triangulation
    /// </summary>
    /// <remarks>
    ///   Ensure that points are in the correct sequence, following the "right-hand" rule
    /// </remarks>
    class function GetCenter(const ALocations: TArray<TLocationCoord2D>): TLocationCoord2D; static;
    /// <summary>
    ///   Calculates whether a location is inside a polygon of location points
    /// </summary>
    /// <remarks>
    ///   Ensure that points are in the correct sequence, following the "right-hand" rule
    /// </remarks>
    class function ContainsLocation(const ALocation: TLocationCoord2D; const APolygon: TArray<TLocationCoord2D>): Boolean; static;
  end;

implementation

uses
  // RTL
  System.Math,
  // DW
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

class function TGeodetic.ContainsLocation(const ALocation: TLocationCoord2D; const APolygon: TArray<TLocationCoord2D>): Boolean;
var
  I, J, LCount: Integer;
begin
  // Based on PointInPolygon, found in this library: http://www.partow.net/projects/fastgeo/index.html
  Result := False;
  LCount := Length(APolygon);
  if LCount > 2 then
  begin
    J := LCount - 1;
    for I := 0 to LCount - 1 do
    begin
      if ((APolygon[I].Latitude <= ALocation.Latitude) and (ALocation.Latitude < APolygon[J].Latitude)) or    // an upward crossing
        ((APolygon[J].Latitude <= ALocation.Latitude) and (ALocation.Latitude < APolygon[J].Latitude)) then  // a downward crossing
      begin
        // Compute the edge-ray intersect @ the x-coordinate
        if (ALocation.Longitude - APolygon[I].Longitude < ((APolygon[J].Longitude - APolygon[I].Longitude) *
          (ALocation.Latitude - APolygon[I].Latitude) / (APolygon[J].Latitude - APolygon[I].Latitude))) then
        begin
          Result := not Result;
        end;
      end;
      J := I;
    end;
  end;
end;

class function TGeodetic.DistanceBetween(const ALocationFrom, ALocationTo: TLocationCoord2D): Double;
begin
  Result := DistanceBetween(ALocationFrom.Latitude, ALocationFrom.Longitude, ALocationTo.Latitude, ALocationTo.Longitude);
end;

class function TGeodetic.GetCenter(const ALocations: TArray<TLocationCoord2D>): TLocationCoord2D;
var
  LCount: Integer;
  X, Y, Z: Double;
  LLatitude, LLongitude, LCentralLatitude, LCentralLongitude, LCentralSquareRoot: Double;
  LLocation: TLocationCoord2D;
begin
  // https://stackoverflow.com/a/14231286/3164070
  LCount := Length(ALocations);
  if LCount > 1 then
  begin
    X := 0;
    Y := 0;
    Z := 0;
    for LLocation in ALocations do
    begin
      LLatitude := LLocation.Latitude * Pi / 180;
      LLongitude := LLocation.Longitude * Pi / 180;
      X := X + Cos(LLatitude) * Cos(LLongitude);
      Y := Y + Cos(LLatitude) * Sin(LLongitude);
      Z := Z + Sin(LLatitude);
    end;
    X := X / LCount;
    Y := Y / LCount;
    Z := Z / LCount;
    LCentralLongitude := ArcTan2(Y, X);
    LCentralSquareRoot := Sqrt((X * X) + (Y * Y));
    LCentralLatitude := ArcTan2(Z, LCentralSquareRoot);
    Result := TLocationCoord2D.Create(LCentralLatitude * 180 / Pi, LCentralLongitude * 180 / Pi);
  end
  else
    Result := ALocations[0];
end;

end.
