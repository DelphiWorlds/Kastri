unit DW.OSPower;

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

{$SCOPEDENUMS ON}

type
  TDevicePowerStatus = (Unknown, DeviceCharging, DeviceNotCharging, DeviceOnACPower);

  TBatteryStatus = (Unknown, NoBattery, BatteryHealthy, BatteryNormal, BatteryLow, BatteryCritical);

  /// <summary>
  ///   Operating System power functions that operate below FMX
  /// </summary>
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TOSPower = record
  public
    /// <summary>
    ///   Returns battery remaining charging time, in seconds. Note: This may be possible on Windows only
    /// </summary>
    class function GetBatteryChargeTime: Integer; static;
    /// <summary>
    ///   Returns battery level
    /// </summary>
    class function GetBatteryLevel: Integer; static;
    /// <summary>
    ///   Returns battery life time, in seconds. Note: This may be possible on Windows only
    /// </summary>
    class function GetBatteryLifeTime: Integer; static;
    /// <summary>
    ///   Returns battery charging status
    /// </summary>
    class function GetBatteryStatus: TBatteryStatus; static;
    /// <summary>
    ///   Returns device charging status
    /// </summary>
    class function GetDevicePowerStatus: TDevicePowerStatus; static;
  end;

implementation

uses
  {$IF Defined(MSWINDOWS)}
  DW.OSPower.Win;
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.OSPower.Android;
  {$ENDIF}
  {$IF Defined(IOS)}
  DW.OSPower.iOS;
  {$ELSEIF Defined(MACOS)}
  DW.OSPower.Mac;
  {$ENDIF}

{ TOSPower }

class function TOSPower.GetBatteryChargeTime: Integer;
begin
  Result := TPlatformOSPower.GetBatteryChargeTime;
end;

class function TOSPower.GetBatteryLevel: Integer;
begin
  Result := TPlatformOSPower.GetBatteryLevel;
end;

class function TOSPower.GetBatteryLifeTime: Integer;
begin
  Result := TPlatformOSPower.GetBatteryLifeTime;
end;

class function TOSPower.GetBatteryStatus: TBatteryStatus;
begin
  Result := TPlatformOSPower.GetBatteryStatus;
end;

class function TOSPower.GetDevicePowerStatus: TDevicePowerStatus;
begin
  Result := TPlatformOSPower.GetDevicePowerStatus;
end;

end.
