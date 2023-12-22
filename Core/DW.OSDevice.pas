unit DW.OSDevice;

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
  // RTL
  System.SysUtils, System.Types, System.Classes;

type
  TOSPlatform = TOSVersion.TPlatform;

  TLocaleInfo = record
    CountryCode: string;
    CountryDisplayName: string;
    CurrencySymbol: string;
    LanguageCode: string;
    LanguageDisplayName: string;
    function Culture: string;
  end;

  TUIMode = (Appliance, Car, Desk, Normal, Television, VRHeadset, Undefined, Watch);

  /// <summary>
  ///   Operating System specific functions that operate below FMX
  /// </summary>
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TOSDevice = record
  public
    class function CanWriteSettings: Boolean; static;
    /// <summary>
    ///   Turns the torch on/off, if available
    /// </summary>
    class function EnableTorch(const AEnable: Boolean): Boolean; static;
    /// <summary>
    ///   Returns locale info
    /// </summary>
    class function GetCurrentLocaleInfo: TLocaleInfo; static;
    /// <summary>
    ///   Returns the model of the device, if available
    /// </summary>
    class function GetDeviceModel: string; static;
    /// <summary>
    ///   Returns the name of the device, whether it is mobile or desktop
    /// </summary>
    class function GetDeviceName: string; static;
    /// <summary>
    ///   Returns a summary of information about the device/application, including Package ID, Version, Device Name and Device ID
    /// </summary>
    class function GetDeviceSummary: string; static;
    /// <summary>
    ///   Returns environment vars
    /// </summary>
    class procedure GetEnvironmentVars(const AVars: TStrings); static;
    /// <summary>
    ///   Returns the maker of the device
    /// </summary>
    class function GetManufacturer: string; static;
    /// <summary>
    ///   Returns build for the application package, if any exists
    /// </summary>
    class function GetPackageBuild: string; static;
    /// <summary>
    ///   Returns the version that the app should display (Can be different from the package version)
    /// </summary>
    class function GetPackageDisplayVersion: string; static;
    /// <summary>
    ///   Returns the filename of the binary
    /// </summary>
    class function GetPackageFileName: string; static;
    /// <summary>
    ///   Returns id for the application package, if any exists
    /// </summary>
    class function GetPackageID: string; static;
    /// <summary>
    ///   Returns the name of the binary
    /// </summary>
    class function GetPackageName: string; static;
    /// <summary>
    ///   Returns the version for the application package, if any exists
    /// </summary>
    class function GetPackageVersion: string; static;
    /// <summary>
    ///   Returns UI mode for an Android device, or Undefined
    /// </summary>
    class function GetUIMode: TUIMode; static;
    /// <summary>
    ///   Returns the unique id for the device, if any exists
    /// </summary>
    class function GetUniqueDeviceID: string; static;
    class function GetUsername: string; static;
    /// <summary>
    ///   Returns whether the application is a beta version
    /// </summary>
    class function IsBeta: Boolean; static;
    /// <summary>
    ///   Returns whether the location service is enabled
    /// </summary>
    class function IsLocationServiceEnabled: Boolean; static;
    /// <summary>
    ///   Returns whether the device is a mobile device
    /// </summary>
    class function IsMobile: Boolean; static;
    /// <summary>
    ///   Returns whether the platform matches
    /// </summary>
    class function IsPlatform(const APlatform: TOSPlatform): Boolean; static;
    /// <summary>
    ///   Returns whether the screen is locked
    /// </summary>
    class function IsScreenLocked: Boolean; static;
    /// <summary>
    ///   Returns whether the device is a tablet (e.g. large Android device or iPad)
    /// </summary>
    class function IsTablet: Boolean; static;
    /// <summary>
    ///   Returns whether the device has touch capability
    /// </summary>
    class function IsTouchDevice: Boolean; static;
    /// <summary>
    ///   Opens the default browser with the URL
    /// </summary>
    class procedure OpenURL(const AURL: string); static;
    class procedure OpenAppSettings; static;
    class function OpenWriteSettingsPermissions: Boolean; static;
    /// <summary>
    ///   Prevent the screen from locking (Android and iOS)
    /// </summary>
    class procedure SetPreventScreenLock(const AValue: Boolean); static;
    /// <summary>
    ///   Shows the folder that contains the nominated files
    /// </summary>
    class procedure ShowFilesInFolder(const AFileNames: array of string); static;
  end;

implementation

uses
  // DW
  DW.OSLog,
  {$IF Defined(POSIX)}
  DW.OSDevice.Posix,
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.OSDevice.Android;
  {$ELSEIF Defined(IOS)}
  DW.OSDevice.iOS;
  {$ELSEIF Defined(MACOS)}
  DW.OSDevice.Mac;
  {$ELSEIF Defined(MSWINDOWS)}
  DW.Winapi.Helpers,
  DW.OSDevice.Win;
  {$ELSEIF Defined(LINUX)}
  DW.OSDevice.Linux;
  {$ENDIF}

{ TOSDevice }

class function TOSDevice.CanWriteSettings: Boolean;
begin
  {$IF Defined(ANDROID)}
  Result := TOSDevice.CanWriteSettings;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;


class function TOSDevice.EnableTorch(const AEnable: Boolean): Boolean;
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  Result := TPlatformOSDevice.EnableTorch(AEnable);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TOSDevice.GetCurrentLocaleInfo: TLocaleInfo;
begin
  {$IF not Defined(LINUX64)}
  Result := TPlatformOSDevice.GetCurrentLocaleInfo;
  {$ENDIF}
end;

class function TOSDevice.GetDeviceModel: string;
begin
  {$IF Defined(MACOS) or Defined(ANDROID)}
  Result := TPlatformOSDevice.GetDeviceModel;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class function TOSDevice.GetDeviceName: string;
begin
  Result := TPlatformOSDevice.GetDeviceName;
end;

class function TOSDevice.GetDeviceSummary: string;
begin
  Result := Format('%s Version: %s, Device: %s (%s)', [GetPackageID, GetPackageVersion, GetDeviceName, GetUniqueDeviceID]);
end;

class procedure TOSDevice.GetEnvironmentVars(const AVars: TStrings);
begin
  {$IF Defined(MSWINDOWS)}
  TWinapiHelper.GetEnvironmentVars(AVars, True);
  {$ELSEIF Defined(POSIX)}
  TPosixOSDevice.GetEnvironmentVars(AVars);
  {$ENDIF}
end;

class function TOSDevice.GetManufacturer: string;
begin
  {$IF Defined(MACOS)}
  Result := 'Apple'; // Do not localize
  {$ELSEIF Defined(ANDROID)}
  Result := TPlatformOSDevice.GetManufacturer;
  {$ELSE}
  Result := ''; // To do
  {$ENDIF}
end;

class function TOSDevice.GetPackageBuild: string;
begin
  {$IF Defined(MSWINDOWS)}
  Result := TPlatformOSDevice.GetPackageBuild;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class function TOSDevice.GetPackageDisplayVersion: string;
begin
  {$IF not Defined(IOS)}
  Result := TPlatformOSDevice.GetPackageVersion;
  {$ELSE}
  Result := TPlatformOSDevice.GetPackageDisplayVersion;
  {$ENDIF}
end;

class function TOSDevice.GetPackageFileName: string;
begin
  Result := GetModuleName(HInstance);
end;

class function TOSDevice.GetPackageID: string;
begin
  Result := TPlatformOSDevice.GetPackageID;
end;

class function TOSDevice.GetPackageName: string;
begin
  Result := TPlatformOSDevice.GetPackageName;
end;

class function TOSDevice.GetPackageVersion: string;
begin
  Result := TPlatformOSDevice.GetPackageVersion;
end;

class function TOSDevice.GetUniqueDeviceID: string;
begin
  Result := TPlatformOSDevice.GetUniqueDeviceID;
end;

class function TOSDevice.GetUsername: string;
begin
  {$IF Defined(MSWINDOWS) or Defined(OSX)}
  Result := TPlatformOSDevice.GetUsername;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class function TOSDevice.IsBeta: Boolean;
begin
  {$IF Defined(MSWINDOWS)}
  Result := TPlatformOSDevice.IsBeta;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TOSDevice.IsLocationServiceEnabled: Boolean;
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  Result := TPlatformOSDevice.IsLocationServiceEnabled;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TOSDevice.IsMobile: Boolean;
begin
  Result := TOSVersion.Platform in [TOSVersion.TPlatform.pfiOS, TOSVersion.TPlatform.pfAndroid];
end;

class function TOSDevice.IsPlatform(const APlatform: TOSPlatform): Boolean;
begin
  Result := TOSVersion.Platform = APlatform;
end;

class function TOSDevice.IsScreenLocked: Boolean;
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  Result := TPlatformOSDevice.IsScreenLocked;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TOSDevice.IsTablet: Boolean;
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  Result := TPlatformOSDevice.IsTablet;
  {$ELSE}
  // TODO: Windows Surface devices, for example
  Result := False;
  {$ENDIF}
end;

class function TOSDevice.IsTouchDevice: Boolean;
begin
  Result := TPlatformOSDevice.IsTouchDevice;
end;

class function TOSDevice.GetUIMode: TUIMode;
begin
  {$IF Defined(ANDROID)}
  Result := TPlatformOSDevice.GetUIMode;
  {$ELSE}
  Result := TUIMode.Undefined;
  {$ENDIF}
end;

class procedure TOSDevice.OpenAppSettings;
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  TPlatformOSDevice.OpenAppSettings;
  {$ENDIF}
end;

class procedure TOSDevice.OpenURL(const AURL: string);
begin
  {$IF Defined(MACOS) or Defined(ANDROID) or Defined(MSWINDOWS)}
  TPlatformOSDevice.OpenURL(AURL);
  {$ENDIF}
end;

class function TOSDevice.OpenWriteSettingsPermissions: Boolean;
begin
  {$IF Defined(ANDROID)}
  Result := TPlatformOSDevice.OpenWriteSettingsPermissions;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class procedure TOSDevice.SetPreventScreenLock(const AValue: Boolean);
begin
  {$IF Defined(IOS) or Defined(ANDROID)}
  TPlatformOSDevice.SetPreventScreenLock(AValue);
  {$ENDIF}
end;

class procedure TOSDevice.ShowFilesInFolder(const AFileNames: array of string);
begin
  {$IF Defined(MSWINDOWS) or Defined(OSX)}
  TPlatformOSDevice.ShowFilesInFolder(AFileNames);
  {$ENDIF}
end;

{ TLocaleInfo }

function TLocaleInfo.Culture: string;
begin
  Result := ''; // WIP
end;

end.
