unit DW.OSDevice.Android;

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
  DW.OSDevice;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSDevice = record
  public
    class function EnableTorch(const AEnable: Boolean): Boolean; static;
    class function GetCurrentLocaleInfo: TLocaleInfo; static;
    class function GetDeviceName: string; static;
    class function GetPackageID: string; static;
    class function GetPackageVersion: string; static;
    class function GetUniqueDeviceID: string; static;
    class function HasHardwareKeyboard: Boolean; static;
    class function IsScreenLocked: Boolean; static;
    class function IsTouchDevice: Boolean; static;
    class procedure OpenAppSettings; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Provider, Androidapi.JNI.Os,  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net, Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.Hardware.Camera2, DW.Android.Helpers;

{ TPlatformOSDevice }

class function TPlatformOSDevice.EnableTorch(const AEnable: Boolean): Boolean;
var
  LCameraManager: JCameraManager;
  LIDList: TJavaObjectArray<JString>;
begin
  Result := False;
  LCameraManager := TJCameraManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.CAMERA_SERVICE));
  if LCameraManager <> nil then
  begin
    LIDList := LCameraManager.getCameraIdList;
    try
      LCameraManager.setTorchMode(LIDList.Items[0], AEnable);
      Result := True;
    finally
      LIDList.Free;
    end;
  end;
end;

class function TPlatformOSDevice.GetCurrentLocaleInfo: TLocaleInfo;
var
  LLocale: JLocale;
begin
  LLocale := TJLocale.JavaClass.getDefault;
  Result.LanguageCode := JStringToString(LLocale.getISO3Language);
  if Length(Result.LanguageCode) > 2 then
    Delete(Result.LanguageCode, 3, MaxInt);
  Result.LanguageDisplayName := JStringToString(LLocale.getDisplayLanguage);
  Result.CountryCode := JStringToString(LLocale.getCountry);
  Result.CountryDisplayName := JStringToString(LLocale.getDisplayCountry);
end;

class function TPlatformOSDevice.GetDeviceName: string;
begin
  Result := JStringToString(TJBuild.JavaClass.MODEL);
  if Result.IsEmpty then
    Result := Format('%s %s', [JStringToString(TJBuild.JavaClass.MANUFACTURER), JStringToString(TJBuild.JavaClass.PRODUCT)]);
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := JStringToString(TAndroidHelper.Context.getPackageName);
end;

class function TPlatformOSDevice.GetPackageVersion: string;
var
  LPackageInfo: JPackageInfo;
begin
  LPackageInfo := TAndroidHelper.Context.getPackageManager.getPackageInfo(TAndroidHelper.Context.getPackageName, 0);
  Result := JStringToString(LPackageInfo.versionName);
end;

// **** NOTE: Use this value with care, as it is reset if the device is rooted, or wiped
class function TPlatformOSDevice.GetUniqueDeviceID: string;
var
  LName: JString;
begin
  LName := TJSettings_Secure.JavaClass.ANDROID_ID;
  Result := JStringToString(TJSettings_Secure.JavaClass.getString(TAndroidHelper.ContentResolver, LName));
end;

class function TPlatformOSDevice.HasHardwareKeyboard: Boolean;
var
  LResources: JResources;
  LConfiguration: JConfiguration;
begin
  Result := False;
  LResources := TAndroidHelper.Context.getResources;
  if LResources <> nil then
  begin
    LConfiguration := LResources.getConfiguration;
    if LConfiguration <> nil then
      Result := LConfiguration.keyboard <> TJConfiguration.JavaClass.KEYBOARD_NOKEYS;
  end;
end;

class function TPlatformOSDevice.IsScreenLocked: Boolean;
begin
  Result := TAndroidHelperEx.KeyguardManager.inKeyguardRestrictedInputMode;
end;

// **** NOTE: Use this value with care, as devices that do not have touch support, but are connected to another screen, will report True
class function TPlatformOSDevice.IsTouchDevice: Boolean;
begin
  Result := TAndroidHelper.Context.getPackageManager.hasSystemFeature(StringToJString('android.hardware.touchscreen'));
end;

class procedure TPlatformOSDevice.OpenAppSettings;
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.Create;
  LIntent.setAction(TJSettings.javaClass.ACTION_APPLICATION_DETAILS_SETTINGS);
  LIntent.setData(TJnet_Uri.JavaClass.parse(StringToJString('package:' + JStringtoString(TAndroidHelper.Context.getPackageName()))));
  TAndroidHelper.Context.startActivity(LIntent);
end;

end.



