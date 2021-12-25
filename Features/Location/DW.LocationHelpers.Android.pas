unit DW.LocationHelpers.Android;

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
  // Android
  Androidapi.JNI.Location,
  // DW
  DW.Location.Types;

type
  TLocationHelper = record
  private
    FData: TLocationData;
  public
    function GetLocationData(const ALocation: JLocation): TLocationData;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Sensors, System.DateUtils,
  // DW
  DW.Geodetic;

function TLocationHelper.GetLocationData(const ALocation: JLocation): TLocationData;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Location := TLocationCoord2D.Create(ALocation.getLatitude, ALocation.getLongitude);
  if ALocation.hasAccuracy then
  begin
    Include(Result.Flags, TLocationDataFlag.Accuracy);
    Result.Accuracy := ALocation.getAccuracy;
  end;
  if ALocation.hasAltitude then
  begin
    Include(Result.Flags, TLocationDataFlag.Altitude);
    Result.Altitude := ALocation.getAltitude;
  end;
  if ALocation.hasBearing then
  begin
    Include(Result.Flags, TLocationDataFlag.Bearing);
    Result.Bearing := ALocation.getBearing;
  end;
  if ALocation.hasSpeed then
  begin
    Include(Result.Flags, TLocationDataFlag.Speed);
    Result.Speed := ALocation.getSpeed;
  end
  else if (FData.DateTime > 0) and (FData.Location.Latitude <> cInvalidLatitude) then
  begin
    Include(Result.Flags, TLocationDataFlag.Speed);
    Result.Speed := TGeodetic.DistanceBetween(FData.Location, Result.Location) / SecondsBetween(Now, FData.DateTime);
  end;
  FData := Result;
end;


end.
