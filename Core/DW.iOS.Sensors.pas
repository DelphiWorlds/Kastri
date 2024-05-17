unit DW.iOS.Sensors;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

// This is a "patch-in" unit to be used in conjunction with patches to System.iOS.Sensors

interface

uses
  // RTL
  System.Sensors,
  // Mac
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CoreLocation,
  // DW
  DW.Sensors;

type
  CLCircularRegionClass = interface(CLRegionClass)
    ['{B2E71730-FB37-4DB4-9D49-8A004BB6C62C}']
  end;

  CLCircularRegion = interface(CLRegion)
    ['{FF4DCF91-376B-41BB-B60A-880BEBB5B4EE}']
    function initWithCenter(center: CLLocationCoordinate2D; radius: CLLocationDistance; identifier: Pointer): Pointer; cdecl;
    function center: CLLocationCoordinate2D; cdecl;
    function radius: CLLocationDistance; cdecl;
    function containsCoordinate(coordinate: CLLocationCoordinate2D): Boolean; cdecl;
  end;
  TCLCircularRegion = class(TOCGenericImport<CLCircularRegionClass, CLCircularRegion>)  end;

  TiOSLocationSensorHelper = record
  private
    class var FLocater: CLLocationManager;
    class var FUsage: TLocationUsage;
  public
    class function IsLocationEnabled: Boolean; static;
    class procedure SetBackgroundMonitor(const AValue: Boolean); static;
    class procedure SetLocater(const ALocater: CLLocationManager); static;
    class procedure SetLocationActivity(const AActivity: TLocationActivity); static;
    class procedure SetUsage(const AUsage: TLocationUsage); static;
    class procedure SuspendedStart; static;
    class procedure UpdateUsage; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Mac
  Macapi.Helpers,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.OSLog, DW.iOSapi.Helpers;

{ TiOSLocationSensorHelper }

class procedure TiOSLocationSensorHelper.SetLocater(const ALocater: CLLocationManager);
begin
  if FLocater <> nil then
  begin
    FLocater.release;
    FLocater := nil;
  end;
  FLocater := ALocater;
end;

class procedure TiOSLocationSensorHelper.SetLocationActivity(const AActivity: TLocationActivity);
const
  cActivityValues: array[TLocationActivity] of CLActivityType = (
    CLActivityTypeOther, CLActivityTypeAutomotiveNavigation, CLActivityTypeFitness, CLActivityTypeOtherNavigation
  );
begin
  if FLocater <> nil then
    FLocater.setActivityType(cActivityValues[AActivity]);
end;

class function TiOSLocationSensorHelper.IsLocationEnabled: Boolean;
begin
  if FLocater = nil then
  begin
    FLocater := TCLLocationManager.Create;
    FLocater.retain;
  end;
  Result := FLocater.locationServicesEnabled;
end;

class procedure TiOSLocationSensorHelper.SetBackgroundMonitor(const AValue: Boolean);
begin
  if FLocater <> nil then
  begin
    TOSLog.d('TiOSLocationSensorHelper.SetBackgroundMonitor: %s', [BoolToStr(AValue, True)]);
    FLocater.setPausesLocationUpdatesAutomatically(not AValue);
    if AValue then
      FLocater.startMonitoringSignificantLocationChanges
    else
      FLocater.startUpdatingLocation;
  end;
end;

class procedure TiOSLocationSensorHelper.SetUsage(const AUsage: TLocationUsage);
begin
  FUsage := AUsage;
  UpdateUsage;
  // Result := TiOSHelperEx.GetLocationManagerAuthorization = TAuthorizationType.atAuthorized
end;

class procedure TiOSLocationSensorHelper.SuspendedStart;
begin
  if FLocater <> nil then
    FLocater.startUpdatingLocation;
end;

class procedure TiOSLocationSensorHelper.UpdateUsage;
begin
  if FLocater <> nil then
  begin
    case FUsage of
      TLocationUsage.WhenInUse:
        FLocater.requestWhenInUseAuthorization;
      TLocationUsage.Always:
        FLocater.requestAlwaysAuthorization;
    end;
  end;
end;

end.
