unit DW.OSDevice.Mac;

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
  // macOS
  Macapi.Foundation, Macapi.AppKit,
  // DW
  DW.OSDevice;

type
  TPlatformOSDevice = record
  private
    class function SharedWorkspace: NSWorkspace; static;
    class procedure OpenNSURL(const AURL: NSURL); static;
  public
    class function GetCurrentLocaleInfo: TLocaleInfo; static;
    class function GetDeviceModel: string; static;
    class function GetDeviceName: string; static;
    class function GetOSBuild: string; static;
    class function GetPackageID: string; static;
    class function GetPackageName: string; static;
    class function GetPackageVersion: string; static;
    class function GetUniqueDeviceID: string; static;
    class function GetUsername: string; static;
    class function IsTouchDevice: Boolean; static;
    class procedure OpenURL(const AURL: string); static;
    class procedure ShowFilesInFolder(const AFileNames: array of string); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.CoreFoundation, Macapi.Helpers, Macapi.IOKit,
  // Posix
  Posix.SysUtsname, Posix.SysSysctl,
  // DW
  DW.Macapi.IOKit, DW.Macapi.Helpers, DW.Macapi.Foundation;

{ TPlatformOSDevice }

class function TPlatformOSDevice.GetCurrentLocaleInfo: TLocaleInfo;
var
  LLocale: NSLocale;
begin
  LLocale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
  Result.LanguageCode := NSStrToStr(LLocale.languageCode);
  Result.LanguageDisplayName := NSStrToStr(LLocale.localizedStringForLanguageCode(LLocale.languageCode));
  Result.CountryCode := NSStrToStr(LLocale.countryCode);
  Result.CountryDisplayName := NSStrToStr(LLocale.localizedStringForCountryCode(LLocale.countryCode));
  Result.CurrencySymbol := NSStrToStr(LLocale.currencySymbol);
end;

class function TPlatformOSDevice.GetDeviceModel: string;
var
  LUtsName: utsname;
begin
  Result := '';
  if uname(LUtsName) <> -1 then
    Result := string(UTF8String(LUtsName.machine));
end;

class function TPlatformOSDevice.GetDeviceName: string;
begin
  Result := NSStrToStr(TNSHost.Wrap(TNSHost.OCClass.currentHost).localizedName);
end;

class function TPlatformOSDevice.GetOSBuild: string;
var
  LBuffer: TArray<Byte>;
  LLength: LongWord;
begin
  sysctlbyname(MarshaledAString('kern.osversion'), nil, @LLength, nil, 0);
  SetLength(LBuffer, LLength);
  sysctlbyname(MarshaledAString('kern.osversion'), @LBuffer[0], @LLength, nil, 0);
  Result := string(MarshaledAString(@LBuffer[0]));
end;

class function TPlatformOSDevice.GetUniqueDeviceID: string;
var
  LService: io_service_t;
  LSerialRef: CFTypeRef;
begin
  Result := '';
  LService := IOServiceGetMatchingService(kIOMasterPortDefault, CFDictionaryRef(IOServiceMatching('IOPlatformExpertDevice')));
  if LService > 0 then
  try
    LSerialRef := IORegistryEntryCreateCFProperty(LService, kIOPlatformSerialNumberKey, kCFAllocatorDefault, 0);
    if LSerialRef <> nil then
      Result := CFStringRefToStr(LSerialRef);
  finally
    IOObjectRelease(LService);
  end;
end;

class function TPlatformOSDevice.GetUsername: string;
begin
  Result := NSStrToStr(TNSString.Wrap(NSUserName));
end;

class function TPlatformOSDevice.IsTouchDevice: Boolean;
begin
  Result := False;
end;

class procedure TPlatformOSDevice.OpenNSURL(const AURL: NSURL);
begin
  SharedWorkspace.openURL(AURL);
end;

class procedure TPlatformOSDevice.OpenURL(const AURL: string);
begin
  OpenNSURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(AURL))));
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := TMacHelperEx.GetBundleValue('CFBundleIdentifier');
end;

class function TPlatformOSDevice.GetPackageName: string;
begin
  Result := TMacHelperEx.GetBundleValue('CFBundleName');
end;

class function TPlatformOSDevice.GetPackageVersion: string;
begin
  Result := TMacHelperEx.GetBundleValue('CFBundleVersion');
end;

class function TPlatformOSDevice.SharedWorkspace: NSWorkspace;
begin
  Result := TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace);
end;

class procedure TPlatformOSDevice.ShowFilesInFolder(const AFileNames: array of string);
var
  LArray: array of Pointer;
  LNSArray: NSArray;
  I: Integer;
begin
  SetLength(LArray, Length(AFileNames));
  for I := 0 to Length(AFileNames) - 1 do
    LArray[I] := TNSURL.OCClass.fileURLWithPath(StrToNSStr(AFileNames[I]));
  LNSArray := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LArray[0], Length(LArray)));
  TNSWorkspace.wrap(TNSWorkspace.OCClass.sharedWorkspace).activateFileViewerSelectingURLs(LNSArray);
end;

end.



