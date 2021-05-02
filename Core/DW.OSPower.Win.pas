unit DW.OSPower.Win;

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
  // DW
  DW.OSPower;

type
  TPlatformOSPower = record
  public
    class function GetBatteryChargeTime: Integer; static;
    class function GetBatteryLevel: Integer; static;
    class function GetBatteryLifeTime: Integer; static;
    class function GetBatteryStatus: TBatteryStatus; static;
    class function GetDevicePowerStatus: TDevicePowerStatus; static;
  end;

implementation

uses
  // Windows
  Winapi.Windows;

{ TPlatformOSPower }

class function TPlatformOSPower.GetBatteryChargeTime: Integer;
var
  LStatus: TSystemPowerStatus;
begin
  Result := -1; // Unknown
  if GetSystemPowerStatus(LStatus) then
    Result := LStatus.BatteryFullLifeTime;
end;

class function TPlatformOSPower.GetBatteryLevel: Integer;
var
  LStatus: TSystemPowerStatus;
begin
  Result := -1; // Unknown
  if GetSystemPowerStatus(LStatus) then
    Result := LStatus.BatteryLifePercent;
end;

class function TPlatformOSPower.GetBatteryLifeTime: Integer;
var
  LStatus: TSystemPowerStatus;
begin
  Result := -1; // Unknown
  if GetSystemPowerStatus(LStatus) then
    Result := LStatus.BatteryLifeTime;
end;

class function TPlatformOSPower.GetBatteryStatus: TBatteryStatus;
var
  LStatus: TSystemPowerStatus;
begin
  Result := TBatteryStatus.Unknown;
  if GetSystemPowerStatus(LStatus) then
  begin
    Result := TBatteryStatus.BatteryNormal;
    if (LStatus.BatteryFlag and 255) = 255 then
      Result := TBatteryStatus.Unknown
    else if (LStatus.BatteryFlag and 128) = 128 then
      Result := TBatteryStatus.NoBattery
    else if (LStatus.BatteryFlag and 1) = 1 then
      Result := TBatteryStatus.BatteryHealthy
    else if (LStatus.BatteryFlag and 2) = 2 then
      Result := TBatteryStatus.BatteryLow
    else if (LStatus.BatteryFlag and 4) = 4 then
      Result := TBatteryStatus.BatteryCritical;
  end;
end;

// Can a device be not on AC power *and* charging?
class function TPlatformOSPower.GetDevicePowerStatus: TDevicePowerStatus;
var
  LStatus: TSystemPowerStatus;
begin
  Result := TDevicePowerStatus.Unknown;
  if GetSystemPowerStatus(LStatus) then
  begin
    if Boolean(LStatus.ACLineStatus) then
      Result := TDevicePowerStatus.DeviceOnACPower
    else if (LStatus.BatteryFlag and 8) = 8 then
      Result := TDevicePowerStatus.DeviceCharging
    else
      Result := TDevicePowerStatus.DeviceNotCharging;
  end;
end;

end.
