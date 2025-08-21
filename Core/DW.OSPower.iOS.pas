unit DW.OSPower.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // DW
  DW.OSPower;

type
  TPlatformOSPower = record
  private
    class procedure EnableMonitoring; static;
  public
    class function GetBatteryChargeTime: Integer; static;
    class function GetBatteryLevel: Integer; static;
    class function GetBatteryLifeTime: Integer; static;
    class function GetBatteryStatus: TBatteryStatus; static;
    class function GetDevicePowerStatus: TDevicePowerStatus; static;
  end;

implementation

uses
  // iOS
  iOSapi.Helpers, iOSapi.UIKit;

{ TPlatformOSPower }

class procedure TPlatformOSPower.EnableMonitoring;
begin
  TiOSHelper.CurrentDevice.setBatteryMonitoringEnabled(True);
end;

class function TPlatformOSPower.GetBatteryChargeTime: Integer;
begin
  Result := -1; // Unable to determine on iOS
end;

class function TPlatformOSPower.GetBatteryLevel: Integer;
begin
  EnableMonitoring;
  Result := Round(TiOSHelper.CurrentDevice.batteryLevel * 100);
end;

class function TPlatformOSPower.GetBatteryLifeTime: Integer;
begin
  Result := -1; // Unable to determine on iOS
end;

//!!!! Might need more work. Uses Windows levels
class function TPlatformOSPower.GetBatteryStatus: TBatteryStatus;
var
  LLevel: Integer;
begin
  EnableMonitoring;
  Result := TBatteryStatus.Unknown;
  if TiOSHelper.CurrentDevice.batteryState <> UIDeviceBatteryStateUnknown then
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
end;

class function TPlatformOSPower.GetDevicePowerStatus: TDevicePowerStatus;
begin
  EnableMonitoring;
  Result := TDevicePowerStatus.Unknown;
  case TiOSHelper.CurrentDevice.batteryState of
    UIDeviceBatteryStateUnknown:
      Result := TDevicePowerStatus.Unknown;
    UIDeviceBatteryStateCharging:
      Result := TDevicePowerStatus.DeviceCharging;
    UIDeviceBatteryStateUnplugged, UIDeviceBatteryStateFull:
      Result := TDevicePowerStatus.DeviceNotCharging;
  end;
end;

// This is apparently for notifications only
//initialization
//  TiOSHelper.CurrentDevice.setBatteryMonitoringEnabled(True);

end.
