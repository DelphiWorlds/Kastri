unit DW.OSDevice.iOS;

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
  // iOS
  iOSapi.Foundation,
  // DW
  DW.OSDevice;

type
  TPlatformOSDevice = record
  private
    class procedure OpenNSURL(const AURL: NSURL); static;
  public
    class function EnableTorch(const AEnable: Boolean): Boolean; static;
    class function GetCurrentLocaleInfo: TLocaleInfo; static;
    class function GetDeviceModel: string; static;
    class function GetDeviceName: string; static;
    class function GetPackageID: string; static;
    class function GetPackageVersion: string; static;
    class function GetUniqueDeviceID: string; static;
    class function IsScreenLocked: Boolean; static;
    class function IsTouchDevice: Boolean; static;
    class procedure OpenURL(const AURL: string); static;
    class procedure OpenAppSettings; static;
    class procedure SetPreventScreenLock(const AValue: Boolean); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.Helpers, iOSapi.UIKit, iOSapi.AVFoundation,
  // Posix
  Posix.SysUtsname,
  // DW
  DW.Macapi.Helpers, DW.iOSapi.Foundation, DW.iOSapi.UIKit;

{ TPlatformOSDevice }

class function TPlatformOSDevice.EnableTorch(const AEnable: Boolean): Boolean;
var
  LDevice: AVCaptureDevice;
  LMode: AVCaptureTorchMode;
begin
  Result := False;
  LDevice := TAVCaptureDevice.Wrap(TAVCaptureDevice.OCClass.defaultDeviceWithMediaType(AVMediaTypeVideo));
  if (LDevice <> nil) and LDevice.hasTorch and LDevice.lockForConfiguration then
  try
    if AEnable then
      LMode := AVCaptureTorchModeOn
    else
      LMode := AVCaptureTorchModeOff;
    LDevice.setTorchMode(LMode);
    Result := LDevice.torchMode = LMode;
  finally
    LDevice.unlockForConfiguration;
  end;
end;

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
  Result := NSStrToStr(TiOSHelper.CurrentDevice.name);
end;

class function TPlatformOSDevice.GetUniqueDeviceID: string;
begin
  Result := NSStrToStr(TiOSHelper.CurrentDevice.identifierForVendor.UUIDString);
end;

class function TPlatformOSDevice.IsScreenLocked: Boolean;
begin
  Result := False; // To be implemented
end;

class function TPlatformOSDevice.IsTouchDevice: Boolean;
begin
  Result := True;
end;

class procedure TPlatformOSDevice.OpenNSURL(const AURL: NSURL);
begin
  TiOSHelper.SharedApplication.openURL(AURL);
end;

class procedure TPlatformOSDevice.OpenAppSettings;
begin
  OpenNSURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(UIApplicationOpenSettingsURLString)));
end;

class procedure TPlatformOSDevice.OpenURL(const AURL: string);
begin
  OpenNSURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(AURL))));
end;

class procedure TPlatformOSDevice.SetPreventScreenLock(const AValue: Boolean);
begin
  TiOSHelper.SharedApplication.setIdleTimerDisabled(AValue);
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := TMacHelperEx.GetBundleValue('CFBundleIdentifier');
end;

class function TPlatformOSDevice.GetPackageVersion: string;
begin
  Result := TMacHelperEx.GetBundleValue('CFBundleVersion');
end;

end.
