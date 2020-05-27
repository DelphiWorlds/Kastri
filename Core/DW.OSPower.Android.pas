unit DW.OSPower.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // DW
  DW.OSPower, DW.Androidapi.JNI.Os;

type
  TPlatformOSPower = record
  private
    class var Manager: JBatteryManager;
    class procedure CreateManager; static;
  public
    class function GetBatteryChargeTime: Integer; static;
    class function GetBatteryLevel: Integer; static;
    class function GetBatteryLifeTime: Integer; static;
    class function GetBatteryStatus: TBatteryStatus; static;
    class function GetDevicePowerStatus: TDevicePowerStatus; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge;

const
  BATTERY_PROPERTY_STATUS = 6; // Class property did not import for some reason?

{ TPlatformOSPower }

class procedure TPlatformOSPower.CreateManager;
var
  LService: JObject;
begin
  if Manager = nil then
  begin
    LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.BATTERY_SERVICE);
    Manager := TJBatteryManager.Wrap(TAndroidHelper.JObjectToID(LService));
  end;
end;

class function TPlatformOSPower.GetBatteryChargeTime: Integer;
begin
  Result := -1; // Not sure if this is possible with Android
end;

class function TPlatformOSPower.GetBatteryLevel: Integer;
begin
  TPlatformOSPower.CreateManager;
  Result := Manager.getIntProperty(TJBatteryManager.JavaClass.BATTERY_PROPERTY_CAPACITY);
end;

class function TPlatformOSPower.GetBatteryLifeTime: Integer;
begin
  Result := -1; // Not sure if this is possible with Android. Possibly use BATTERY_PROPERTY_ENERGY_COUNTER (API level 21)
end;

// Uses the same percentage values as Windows
class function TPlatformOSPower.GetBatteryStatus: TBatteryStatus;
var
  LLevel: Integer;
begin
  LLevel := TPlatformOSPower.GetBatteryLevel;
  if LLevel > 66 then
    Result := TBatteryStatus.BatteryHealthy
  else if LLevel > 33 then
    Result := TBatteryStatus.BatteryNormal
  else if LLevel > 5 then
    Result := TBatteryStatus.BatteryLow
  else
    Result := TBatteryStatus.BatteryCritical;
end;

class function TPlatformOSPower.GetDevicePowerStatus: TDevicePowerStatus;
var
  LStatus: Integer;
begin
  TPlatformOSPower.CreateManager;
  Result := TDevicePowerStatus.Unknown;
  LStatus := Manager.getIntProperty(BATTERY_PROPERTY_STATUS)     ;
  if LStatus = TJBatteryManager.JavaClass.BATTERY_STATUS_CHARGING then
    Result := TDevicePowerStatus.DeviceCharging
  else if LStatus = TJBatteryManager.JavaClass.BATTERY_STATUS_NOT_CHARGING then
    Result := TDevicePowerStatus.DeviceNotCharging;
end;

end.
