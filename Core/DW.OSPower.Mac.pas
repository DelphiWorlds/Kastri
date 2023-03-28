unit DW.OSPower.Mac;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
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
  private
    type
      TPowerInfo = record
        BatteryChargeTime: Integer;
        BatteryLevel: Integer;
        BatteryLifeTime: Integer;
        BatteryStatus: TBatteryStatus;
        DevicePowerStatus: TDevicePowerStatus;
      end;
  private
    class var FPowerInfo: TPowerInfo;
    class function GetIntegerValue(const AValue: Pointer; const ADefault: Integer = -1): Integer; static;
    class procedure UpdateDevicePowerStatus(const AStatus: Pointer); static;
    class procedure UpdatePowerInfo; static;
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
  System.StrUtils,
  // macOS
  Macapi.CoreFoundation, Macapi.Helpers,
  // DW
  DW.Macapi.IOKit;

{ TPlatformOSPower }

class procedure TPlatformOSPower.UpdatePowerInfo;
var
  LBlob: CFTypeRef;
  LSources: CFArrayRef;
  LSource: CFDictionaryRef;
  LSourcesCount, I: Integer;
  LCurrentCapacity, LMaxCapacity: Integer;
begin
  FPowerInfo.BatteryChargeTime := -1;
  FPowerInfo.BatteryLifeTime := -1;
  FPowerInfo.BatteryLevel := -1;
  FPowerInfo.BatteryStatus := TBatteryStatus.Unknown;
  FPowerInfo.DevicePowerStatus := TDevicePowerStatus.Unknown;
  LBlob := IOPSCopyPowerSourcesInfo;
  try
    LSources := IOPSCopyPowerSourcesList(LBlob);
    try
      LSourcesCount := CFArrayGetCount(LSources);
      for I := 0 to LSourcesCount - 1 do
      begin
        LSource := IOPSGetPowerSourceDescription(LBlob, CFArrayGetValueAtIndex(LSources, I));
        if LSource <> nil then
        try
          UpdateDevicePowerStatus(CFDictionaryGetValue(LSource, kIOPSPowerSourceStateKey));
          FPowerInfo.BatteryChargeTime := GetIntegerValue(CFDictionaryGetValue(LSource, kIOPSTimeToFullChargeKey));
          FPowerInfo.BatteryLifeTime := GetIntegerValue(CFDictionaryGetValue(LSource, kIOPSTimeToEmptyKey));
          LCurrentCapacity := GetIntegerValue(CFDictionaryGetValue(LSource, kIOPSCurrentCapacityKey));
          LMaxCapacity := GetIntegerValue(CFDictionaryGetValue(LSource, kIOPSMaxCapacityKey));
          if (LCurrentCapacity > -1) and (LMaxCapacity > 0) then
            FPowerInfo.BatteryLevel := Round(LCurrentCapacity / LMaxCapacity * 100);
          if FPowerInfo.BatteryLevel > -1 then
          begin
            if FPowerInfo.BatteryLevel > 66 then
              FPowerInfo.BatteryStatus := TBatteryStatus.BatteryHealthy
            else if FPowerInfo.BatteryLevel > 33 then
              FPowerInfo.BatteryStatus := TBatteryStatus.BatteryNormal
            else if FPowerInfo.BatteryLevel > 5 then
              FPowerInfo.BatteryStatus := TBatteryStatus.BatteryLow
            else
              FPowerInfo.BatteryStatus := TBatteryStatus.BatteryCritical;
          end;
        finally
          CFRelease(LSource);
        end;
      end;
    finally
      CFRelease(LSources);
    end;
  finally
    CFRelease(LBlob);
  end;
end;

class function TPlatformOSPower.GetIntegerValue(const AValue: Pointer; const ADefault: Integer = -1): Integer;
begin
  Result := ADefault;
  if AValue <> nil then
    CFNumberGetValue(AValue, kCFNumberSInt32Type, @Result);
end;

class procedure TPlatformOSPower.UpdateDevicePowerStatus(const AStatus: Pointer);
var
  LIndex: Integer;
begin
  FPowerInfo.DevicePowerStatus := TDevicePowerStatus.Unknown;
  if AStatus <> nil then
  begin
    LIndex := IndexText(CFStringRefToStr(AStatus), ['AC Power', 'Battery Power']);
    if LIndex > -1 then
      FPowerInfo.DevicePowerStatus := TDevicePowerStatus(LIndex + 1);
  end;
end;

class function TPlatformOSPower.GetBatteryChargeTime: Integer;
begin
  UpdatePowerInfo;
  Result := FPowerInfo.BatteryChargeTime;
end;

class function TPlatformOSPower.GetBatteryLevel: Integer;
begin
  UpdatePowerInfo;
  Result := FPowerInfo.BatteryLevel;
end;

class function TPlatformOSPower.GetBatteryLifeTime: Integer;
begin
  UpdatePowerInfo;
  Result := FPowerInfo.BatteryLifeTime;
end;

class function TPlatformOSPower.GetBatteryStatus: TBatteryStatus;
begin
  UpdatePowerInfo;
  Result := FPowerInfo.BatteryStatus;
end;

class function TPlatformOSPower.GetDevicePowerStatus: TDevicePowerStatus;
begin
  UpdatePowerInfo;
  Result := FPowerInfo.DevicePowerStatus;
end;

end.
