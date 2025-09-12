unit DW.OSDevice.Android;

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
  DW.OSDevice;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSDevice = record
  public
    class function CanWriteSettings: Boolean; static;
    class function EnableTorch(const AEnable: Boolean): Boolean; static;
    class function GetCurrentLocaleInfo: TLocaleInfo; static;
    class function GetDeviceModel: string; static;
    class function GetDeviceName: string; static;
    class function GetManufacturer: string; static;
    class function GetPackageID: string; static;
    class function GetPackageName: string; static;
    class function GetPackageVersion: string; static;
    class function GetUIMode: TUIMode; static;
    class function GetUniqueDeviceID: string; static;
    class function HasHardwareKeyboard: Boolean; static;
    class function IsLocationServiceEnabled: Boolean; static;
    class function IsScreenLocked: Boolean; static;
    class function IsTablet: Boolean; static;
    class function IsTouchDevice: Boolean; static;
    class procedure OpenAppSettings; static;
    class procedure OpenURL(const AURL: string); static;
    class function OpenWriteSettingsPermissions: Boolean; static;
    class procedure SetPreventScreenLock(const AValue: Boolean); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Provider, Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net, Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.Location,
  // DW
  DW.Androidapi.JNI.Hardware.Camera2, DW.Android.Helpers, DW.Androidapi.JNI.App;

type
  [JavaSignature('android/location/LocationManager')]
  JLocationManager = interface(Androidapi.JNI.Location.JLocationManager)
    ['{5F8A5E7D-7205-4423-9F13-70F2E0738822}']
    function isLocationEnabled: Boolean;
  end;
  TJLocationManager = class(TJavaGenericImport<JLocationManagerClass, JLocationManager>) end;

{ TPlatformOSDevice }

class function TPlatformOSDevice.CanWriteSettings: Boolean;
begin
  Result := True;
  if TJBuild_Version.JavaClass.SDK_INT >= 23 then
    Result :=  TJSettings_System.JavaClass.canWrite(TAndroidHelper.Context);
end;

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
  Result.LanguageCode := JStringToString(LLocale.getLanguage);
  Result.LanguageCodeISO639_2 := JStringToString(LLocale.getISO3Language);
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

class function TPlatformOSDevice.GetManufacturer: string;
begin
  Result := JStringToString(TJBuild.JavaClass.MANUFACTURER);
end;

class function TPlatformOSDevice.GetDeviceModel: string;
begin
  Result := JStringToString(TJBuild.JavaClass.MODEL);
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := JStringToString(TAndroidHelper.Context.getPackageName);
end;

class function TPlatformOSDevice.GetPackageName: string;
var
  LInfo: JApplicationInfo;
begin
  // https://stackoverflow.com/a/15114434/3164070
  LInfo := TAndroidHelper.Context.getApplicationInfo;
  if LInfo <> nil then
  begin
    if LInfo.labelRes = 0 then
    begin
      if LInfo.nonLocalizedLabel <> nil then
        Result := JStringToString(LInfo.nonLocalizedLabel.toString)
      else if LInfo.packageName <> nil then
        Result := JStringToString(LInfo.packageName);
    end
    else
      Result := JStringToString(TAndroidHelper.Context.getString(LInfo.labelRes));
  end;
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

class function TPlatformOSDevice.IsLocationServiceEnabled: Boolean;
var
  LObject: JObject;
  LModeOff: Integer;
begin
  if TJBuild_Version.JavaClass.SDK_INT >= 28 then
  begin
    LObject := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE);
    Result := TJLocationManager.Wrap(LObject).isLocationEnabled;
  end
  else
  begin
    LModeOff := TJSettings_Secure.JavaClass.LOCATION_MODE_OFF;
    Result := TJSettings_Secure.JavaClass.getInt(TAndroidHelper.ContentResolver, TJSettings_Secure.JavaClass.LOCATION_MODE, LModeOff) <> LModeOff;
  end;
end;

class function TPlatformOSDevice.IsScreenLocked: Boolean;
begin
  Result := TAndroidHelperEx.KeyguardManager.inKeyguardRestrictedInputMode;
end;

class function TPlatformOSDevice.IsTablet: Boolean;
begin
  Result := (TAndroidHelper.Context.getResources.getConfiguration.screenLayout and TJConfiguration.JavaClass.SCREENLAYOUT_SIZE_MASK)
    >= TJConfiguration.JavaClass.SCREENLAYOUT_SIZE_LARGE;
end;

// **** NOTE: Use this value with care, as devices that do not have touch support, but are connected to another screen, will report True
class function TPlatformOSDevice.IsTouchDevice: Boolean;
begin
  Result := TAndroidHelper.Context.getPackageManager.hasSystemFeature(StringToJString('android.hardware.touchscreen'));
end;

class function TPlatformOSDevice.GetUIMode: TUIMode;
var
  LUiModeManager: JUiModeManager;
  LMode: Integer;
begin
  Result := TUIMode.Undefined;
  LUiModeManager := TJUiModeManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.UI_MODE_SERVICE));
  if LUiModeManager <> nil then
  begin
    LMode := LUiModeManager.getCurrentModeType;
    if LMode = TJConfiguration.JavaClass.UI_MODE_TYPE_APPLIANCE then
      Result := TUIMode.Appliance
    else if LMode = TJConfiguration.JavaClass.UI_MODE_TYPE_CAR then
      Result := TUIMode.Car
    else if LMode = TJConfiguration.JavaClass.UI_MODE_TYPE_DESK then
      Result := TUIMode.Desk
    else if LMode = TJConfiguration.JavaClass.UI_MODE_TYPE_NORMAL then
      Result := TUIMode.Normal
    else if LMode = TJConfiguration.JavaClass.UI_MODE_TYPE_TELEVISION then
      Result := TUIMode.Television
    else if LMode = TJConfiguration.JavaClass.UI_MODE_TYPE_VR_HEADSET then
      Result := TUIMode.VRHeadset
    else if LMode = TJConfiguration.JavaClass.UI_MODE_TYPE_WATCH then
      Result := TUIMode.Watch;
  end;
end;

class procedure TPlatformOSDevice.OpenAppSettings;
var
  LIntent: JIntent;
  LUri: Jnet_Uri;
begin
  LUri := TJnet_Uri.JavaClass.fromParts(StringToJString('package'), TAndroidHelper.Context.getPackageName, nil);
  LIntent := TJIntent.JavaClass.init(TJSettings.JavaClass.ACTION_APPLICATION_DETAILS_SETTINGS, LUri);
  LIntent.addFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK);
  TAndroidHelper.Context.startActivity(LIntent);
end;

class procedure TPlatformOSDevice.OpenURL(const AURL: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString('android.intent.action.VIEW'));
  LIntent.setData(TJnet_Uri.JavaClass.parse(StringToJString(AURL)));
  TAndroidHelper.Activity.startActivity(LIntent);
end;

class function TPlatformOSDevice.OpenWriteSettingsPermissions: Boolean;
var
  LIntent: JIntent;
begin
  Result := False;
  if not CanWriteSettings then
  begin
    Result := True;
    LIntent := TJIntent.JavaClass.init(TJSettings.JavaClass.ACTION_MANAGE_WRITE_SETTINGS);
    LIntent.setData(TJnet_Uri.JavaClass.parse(StringToJString('package:').concat(TAndroidHelper.Context.getPackageName)));
    TAndroidHelper.Context.startActivity(LIntent);
  end;
end;

class procedure TPlatformOSDevice.SetPreventScreenLock(const AValue: Boolean);
begin
  if System.DelphiActivity <> nil then
  begin
    if AValue then
      TAndroidHelper.Activity.getWindow.addFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_KEEP_SCREEN_ON)
    else
      TAndroidHelper.Activity.getWindow.clearFlags(TJWindowManager_LayoutParams.JavaClass.FLAG_KEEP_SCREEN_ON);
  end;
end;

end.



