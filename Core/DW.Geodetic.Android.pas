unit DW.Geodetic.Android;

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
  TPlatformGeodetic = record
  public
    class function DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double; static;
  end;

implementation

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.Location;

{ TPlatformGeodetic }

class function TPlatformGeodetic.DistanceBetween(const ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo: Double): Double;
var
  LResults: TJavaArray<Single>;
begin
  LResults := TJavaArray<Single>.Create(1);
  try
    TJLocation.JavaClass.distanceBetween(ALatitudeFrom, ALongitudeFrom, ALatitudeTo, ALongitudeTo, LResults);
    Result := LResults[0];
  finally
    LResults.Free;
  end;
end;

end.
