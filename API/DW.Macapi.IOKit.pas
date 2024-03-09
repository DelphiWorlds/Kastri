unit DW.Macapi.IOKit;

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

interface

uses
  // macOS
  Macapi.CoreFoundation, Macapi.IOKit;

// Backfills for Macapi.IOKit
// Info gleaned from:
//   https://github.com/hans/universal-remote/blob/master/IOKit.framework/Versions/A/Headers/ps/IOPowerSources.h
//   https://github.com/hans/universal-remote/blob/master/IOKit.framework/Versions/A/Headers/ps/IOPSKeys.h
//   https://github.com/nanotech/iphoneheaders/blob/master/IOKit/IOKitKeys.h

function IODisplayCreateInfoDictionary(framebuffer: io_service_t; options: IOOptionBits): CFDictionaryRef; cdecl;
  external libIOKit name _PU + 'IODisplayCreateInfoDictionary';
function IOPSCopyPowerSourcesInfo: CFTypeRef; cdecl; external libIOKit name _PU + 'IOPSCopyPowerSourcesInfo';
function IOPSCopyPowerSourcesList(blob: CFTypeRef): CFArrayRef; cdecl; external libIOKit name _PU + 'IOPSCopyPowerSourcesList';
function IOPSGetPowerSourceDescription(blob: CFTypeRef; ps: CFTypeRef): CFDictionaryRef; cdecl;
  external libIOKit name _PU + 'IOPSGetPowerSourceDescription';

// Used to obtain a CFStringRef value
function kIOPSNameKey: CFStringRef;
// Used to obtain a CFString, value is kIOPSACPowerValue, kIOPSBatteryPowerValue, or kIOPSOffLineValue
function kIOPSPowerSourceStateKey: CFStringRef;
// Used to obtain a CFNumber (signed integer), units are relative to "Max Capacity"
function kIOPSCurrentCapacityKey: CFStringRef;
// Used to obtain a CFNumber (signed integer), units are %
function kIOPSMaxCapacityKey: CFStringRef;
// Only valid if the power source is running off its own power. That's when the kIOPSPowerSourceStateKey has value kIOPSBatteryPowerValue,
// and the value of kIOPSIsChargingKey is kCFBooleanFalse.
// Used to obtain a CFNumber (signed integer), units are minutes
// A value of -1 indicates "Still Calculating the Time", otherwise estimated minutes left on the battery
function kIOPSTimeToEmptyKey: CFStringRef;
// Only valid if the value of kIOPSIsChargingKey is kCFBooleanTrue.
// Used to obtain a CFNumber (signed integer), units are minutes
// A value of -1 indicates "Still Calculating the Time", otherwise estimated minutes until fully charged.
function kIOPSTimeToFullChargeKey: CFStringRef;
// Used to obtain a CFBoolean - kCFBooleanTrue or kCFBooleanFalse
function kIOPSIsChargingKey: CFStringRef;
// Used to obtain a CFStringRef
function kIOPSBatteryHealthKey: CFStringRef;
// Used to obtain a CFStringRef
function kIOPlatformSerialNumberKey: CFStringRef;

implementation

uses
  Macapi.Foundation;

function kIOPSNameKey: CFStringRef;
begin
  Result := CFSTR('Name');
end;

function kIOPSPowerSourceStateKey: CFStringRef;
begin
  Result := CFSTR('Power Source State');
end;

function kIOPSCurrentCapacityKey: CFStringRef;
begin
  Result := CFSTR('Current Capacity');
end;

function kIOPSMaxCapacityKey: CFStringRef;
begin
  Result := CFSTR('Max Capacity');
end;

function kIOPSTimeToEmptyKey: CFStringRef;
begin
  Result := CFSTR('Time to Empty');
end;

function kIOPSTimeToFullChargeKey: CFStringRef;
begin
  Result := CFSTR('Time to Full Charge');
end;

function kIOPSIsChargingKey: CFStringRef;
begin
  Result := CFSTR('Is Charging');
end;

function kIOPSBatteryHealthKey: CFStringRef;
begin
  Result := CFSTR('BatteryHealth');
end;

function kIOPlatformSerialNumberKey: CFStringRef;
begin
  Result := CFSTR('IOPlatformSerialNumber');
end;

end.
