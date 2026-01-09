unit DW.LocationHelpers.Android;

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
  // Android
  Androidapi.JNI.Location,
  // DW
  DW.Location.Types;

const
  cActionLocation = 'ACTION_LOCATION';
  cExtraLocationData = 'EXTRA_LOCATION_DATA';

type
  TLocationHelper = record
  public
    Data: TLocationData;
    procedure BroadcastLocationData(const ALocation: JLocation; const AIsCached: Boolean = False);
    function GetLocationData(const ALocation: JLocation; const AIsCached: Boolean = False): TLocationData;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Sensors, System.DateUtils,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  // DW
  {$IF CompilerVersion < 35}
  DW.Androidapi.JNI.SupportV4,
  {$ELSE}
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager,
  {$ENDIF}
  DW.Geodetic, DW.Android.Helpers;

procedure TLocationHelper.BroadcastLocationData(const ALocation: JLocation; const AIsCached: Boolean = False);
var
  LIntent: JIntent;
  LData: TLocationData;
begin
  LData := GetLocationData(ALocation, AIsCached);
  LData.IsCached := AIsCached;
  LIntent := TJIntent.JavaClass.init(StringToJString(cActionLocation));
  LIntent.putExtra(StringToJString(cExtraLocationData), StringToJString(LData.ToJSON));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

function TLocationHelper.GetLocationData(const ALocation: JLocation; const AIsCached: Boolean = False): TLocationData;
begin
  Result.Reset;
  if not TAndroidHelperEx.IsActivityForeground then
    Result.ApplicationState := TLocationApplicationState.Background;
  if TAndroidHelperEx.PowerManager.isDeviceIdleMode then
    Result.ApplicationState := TLocationApplicationState.Doze;
  Result.Location := TLocationCoord2D.Create(ALocation.getLatitude, ALocation.getLongitude);
  Result.IsMocked := ALocation.isFromMockProvider;
  Result.IsCached := AIsCached;
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
  else if (Data.DateTime > 0) and (Data.Location.Latitude <> cInvalidLatitude) then
  begin
    Include(Result.Flags, TLocationDataFlag.Speed);
    Result.Speed := TGeodetic.DistanceBetween(Data.Location, Result.Location) / SecondsBetween(Now, Data.DateTime);
  end;
  Result.DateTime := UnixToDateTime(Round(ALocation.getTime / 1000));
  Data := Result;
end;


end.
