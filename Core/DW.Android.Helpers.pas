unit DW.Android.Helpers;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes,
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.App, Androidapi.JNI.Media,
  Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.App, DW.Androidapi.JNI.Os;

const
  cMultiBroadcastReceiverClassName = 'DWMultiBroadcastReceiver';
  cMultiBroadcastReceiverName = 'com.delphiworlds.kastri.' + cMultiBroadcastReceiverClassName;

type
  TAndroidHelperEx = record
  private
    class var FKeyguardManager: JKeyguardManager;
    class var FNotificationManager: JNotificationManager;
    class var FPowerManager: JPowerManager;
    class var FWakeLock: JPowerManager_WakeLock;
  public
    const
      ICE_CREAM_SANDWICH = 14;
      ICE_CREAM_SANDWICH_MR1 = 15;
      JELLY_BEAN = 16;
      JELLY_BEAN_MR1 = 17;
      JELLY_BEAN_MR2 = 18;
      KITKAT = 19;
      KITKAT_MR1 = 20;
      LOLLIPOP = 21;
      LOLLIPOP_MR1 = 22;
      MARSHMALLOW = 23;
      NOUGAT = 24;
      NOUGAT_MR1 = 25;
      OREO = 26;
      OREO_MR1 = 27;
      PIE = 28;
      Q = 29;
    /// <summary>
    ///   Checks if both build and target are greater or equal to the tested value
    /// </summary>
    class function CheckBuildAndTarget(const AValue: Integer): Boolean; static;
    /// <summary>
    ///   Enables/disables the Wake Lock. Needs Wake Lock checked in the Permissions section of the Project Options
    /// </summary>
    class procedure EnableWakeLock(const AEnable: Boolean); static;
    /// <summary>
    ///   Returns the equivalent of "AndroidClass.class"
    /// </summary>
    class function GetClass(const APackageClassName: string): Jlang_Class; static;
    /// <summary>
    ///   Returns the application default icon ID
    /// </summary>
    class function GetDefaultIconID: Integer; static;
    /// <summary>
    ///   Returns a URI to the notification sound
    /// </summary>
    class function GetDefaultNotificationSound: Jnet_Uri; static;
    /// <summary>
    ///   Returns target Sdk version
    /// </summary>
    class function GetTargetSdkVersion: Integer; static;
    /// <summary>
    ///   Returns the time from now, plus the ASecondsFromNow
    /// </summary>
    class function GetTimeFromNowInMillis(const ASecondsFromNow: Int64): Int64; static;
    /// <summary>
    ///   Returns installed Sdk version
    /// </summary>
    class function GetBuildSdkVersion: Integer; static;
    /// <summary>
    ///   Returns information about a running service, if the service is running
    /// </summary>
    class function GetRunningServiceInfo(const AServiceName: string): JActivityManager_RunningServiceInfo; static;
    /// <summary>
    ///   Returns whether the activity is running foreground
    /// </summary>
    /// <remarks>
    ///   Useful from within a service to determine whether or not the service needs to run in foreground mode
    /// </remarks>
    class function IsActivityForeground: Boolean; static;
    /// <summary>
    ///   Returns whether or not battery optimizations are being ignored
    /// </summary>
    class function IsIgnoringBatteryOptimizations: Boolean; static;
    /// <summary>
    ///   Returns whether a service is running foreground
    /// </summary>
    class function IsServiceForeground(const AServiceName: string): Boolean; static;
    /// <summary>
    ///   Returns whether or not a service is running
    /// </summary>
    class function IsServiceRunning(const AServiceName: string): Boolean; static;
    /// <summary>
    ///   Returns the keyguard manager
    /// </summary>
    class function KeyguardManager: JKeyguardManager; static;
    /// <summary>
    ///   Returns the notification manager
    /// </summary>
    class function NotificationManager: JNotificationManager; static;
    /// <summary>
    ///   Returns the power manager
    /// </summary>
    class function PowerManager: JPowerManager; static;
    /// <summary>
    ///   Restarts the app if it is not ignoring battery optimizations.
    /// </summary>
    /// <remarks>
    ///   Needs this in the manifest: <uses-permission android:name="android.permission.REQUEST_IGNORE_BATTERY_OPTIMIZATIONS"/>
    /// </remarks>
    class procedure RestartIfNotIgnoringBatteryOptimizations; static;
    /// <summary>
    ///   Call this to start an activity from an alarm
    /// </summary>
    /// <remarks>
    ///   Used in conjunction with dw-multireceiver.jar
    /// </remarks>
    class procedure SetStartAlarm(const AAlarm: TDateTime; const AStartFromLock: Boolean); static;
    /// <summary>
    ///   Converts file to uri, using FileProvider if target API >= 24
    /// </summary>
    /// <remarks>
    ///   Use this only when accessing files with an "external" URI
    /// </remarks>
    class function UriFromFile(const AFile: JFile): Jnet_Uri; static;
    /// <summary>
    ///   Converts filename to uri, using FileProvider if target API >= 24
    /// </summary>
    /// <remarks>
    ///   Use this only when accessing files with an "external" URI
    /// </remarks>
    class function UriFromFileName(const AFileName: string): Jnet_Uri; static;
  end;

  TJImageHelper = record
  private
    class function RotateBytes(const ABytes: TJavaArray<Byte>; const ARotation: Integer): TJavaArray<Byte>; static;
    class function JImageToByteArray(const AImage: JImage): TJavaArray<Byte>; static;
    class function NV21ToJPEG(const ABytes: TJavaArray<Byte>; const AWidth, AHeight: Integer): TJavaArray<Byte>; static;
    class function YUV_420_888ToNV21(const AImage: JImage): TJavaArray<Byte>; static;
  public
    /// <summary>
    ///   Converts a JImage to a JBitmap
    /// </summary>
    /// <remarks>
    ///   Uses the private JImageToByteArray method to convert from the appropriate image format
    /// </remarks>
    class function JImageToJBitmap(const AImage: JImage): JBitmap; static;
    /// <summary>
    ///   Converts a JImage to a JStream, applying rotation (if any)
    /// </summary>
    /// <remarks>
    ///   Used by the Camera support to orient the captured image
    /// </remarks>
    class function JImageToStream(const AImage: JImage; const ARotation: Integer = 0): TStream; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.DateUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Provider, Androidapi.JNI,
  // DW
  DW.Androidapi.JNI.SupportV4;

const
  cActionStartAlarm = cMultiBroadcastReceiverName + '.ACTION_START_ALARM';
  cExtraStartUnlock = cMultiBroadcastReceiverClassName + '.EXTRA_START_UNLOCK';

{ TAndroidHelperEx }

class function TAndroidHelperEx.CheckBuildAndTarget(const AValue: Integer): Boolean;
begin
  Result := (GetBuildSdkVersion >= AValue) and (GetTargetSdkVersion >= AValue);
end;

class procedure TAndroidHelperEx.EnableWakeLock(const AEnable: Boolean);
var
  LTag: string;
begin
  if AEnable then
  begin
    if FWakeLock = nil then
    begin
      LTag := JStringToString(TAndroidHelper.Context.getPackageName) + '.wakelock';
      FWakeLock := PowerManager.newWakeLock(TJPowerManager.JavaClass.PARTIAL_WAKE_LOCK, StringToJString(LTag));
    end;
    if not FWakeLock.isHeld then
      FWakeLock.acquire;
  end
  else
  begin
    if (FWakeLock <> nil) and FWakeLock.isHeld then
      FWakeLock.release;
    FWakeLock := nil;
  end;
end;

class function TAndroidHelperEx.GetBuildSdkVersion: Integer;
begin
   Result := TJBuild_VERSION.JavaClass.SDK_INT;
end;

class function TAndroidHelperEx.GetTimeFromNowInMillis(const ASecondsFromNow: Int64): Int64;
begin
  Result := TJSystem.JavaClass.currentTimeMillis + (ASecondsFromNow * 1000);
end;

class function TAndroidHelperEx.GetClass(const APackageClassName: string): Jlang_Class;
begin
  Result := TJLang_Class.JavaClass.forName(StringToJString(APackageClassName), True, TAndroidHelper.Context.getClassLoader);
end;

class function TAndroidHelperEx.GetDefaultIconID: Integer;
begin
  Result := TAndroidHelper.Context.getApplicationInfo.icon;
end;

class function TAndroidHelperEx.GetDefaultNotificationSound: Jnet_Uri;
begin
  Result := TJRingtoneManager.JavaClass.getDefaultUri(TJRingtoneManager.JavaClass.TYPE_NOTIFICATION);
end;

class function TAndroidHelperEx.UriFromFile(const AFile: JFile): Jnet_Uri;
var
  LAuthority: JString;
begin
  if CheckBuildAndTarget(NOUGAT) then
  begin
    LAuthority := StringToJString(JStringToString(TAndroidHelper.Context.getApplicationContext.getPackageName) + '.fileprovider');
    Result := TJFileProvider.JavaClass.getUriForFile(TAndroidHelper.Context, LAuthority, AFile);
  end
  else
    Result := TJnet_uri.JavaClass.fromFile(AFile);
end;

class function TAndroidHelperEx.UriFromFileName(const AFileName: string): Jnet_Uri;
begin
  Result := UriFromFile(TJFile.JavaClass.init(StringToJString(AFileName)));
end;

class function TAndroidHelperEx.GetTargetSdkVersion: Integer;
var
  LApplicationInfo: JApplicationInfo;
begin
  LApplicationInfo := TAndroidHelper.Context.getPackageManager.getApplicationInfo(TAndroidHelper.Context.getPackageName, 0);
  Result := LApplicationInfo.targetSdkVersion;
end;

class function TAndroidHelperEx.IsIgnoringBatteryOptimizations: Boolean;
begin
  Result := PowerManager.isIgnoringBatteryOptimizations(TAndroidHelper.Context.getPackageName);
end;

class function TAndroidHelperEx.GetRunningServiceInfo(const AServiceName: string): JActivityManager_RunningServiceInfo;
var
  LService: JObject;
  LRunningServices: JList;
  LServiceInfo: JActivityManager_RunningServiceInfo;
  I: Integer;
begin
  Result := nil;
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.ACTIVITY_SERVICE);
  LRunningServices := TJActivityManager.Wrap(TAndroidHelper.JObjectToID(LService)).getRunningServices(MaxInt);
  for I := 0 to LRunningServices.size - 1 do
  begin
    LServiceInfo := TJActivityManager_RunningServiceInfo.Wrap(TAndroidHelper.JObjectToID(LRunningServices.get(I)));
    if AServiceName.Equals(JStringToString(LServiceInfo.service.getClassName)) then
      Exit(LServiceInfo);
  end;
end;

class function TAndroidHelperEx.IsActivityForeground: Boolean;
var
  LService: JObject;
  LRunningApps: JList;
  LAppInfo: JActivityManager_RunningAppProcessInfo;
  I: Integer;
begin
  Result := False;
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.ACTIVITY_SERVICE);
  LRunningApps := TJActivityManager.Wrap(TAndroidHelper.JObjectToID(LService)).getRunningAppProcesses;
  for I := 0 to LRunningApps.size - 1 do
  begin
    LAppInfo := TJActivityManager_RunningAppProcessInfo.Wrap(TAndroidHelper.JObjectToID(LRunningApps.get(I)));
    if LAppInfo.importance = 100 then
    begin
      if LAppInfo.importanceReasonComponent <> nil then
      begin
        if LAppInfo.importanceReasonComponent.getPackageName.equals(TAndroidHelper.Context.getPackageName) then
          Exit(True);
      end
      else if LRunningApps.size = 1 then
        Exit(True);
    end;
  end;
end;

class function TAndroidHelperEx.IsServiceForeground(const AServiceName: string): Boolean;
var
  LServiceInfo: JActivityManager_RunningServiceInfo;
begin
  LServiceInfo := GetRunningServiceInfo(AServiceName);
  Result := (LServiceInfo <> nil) and LServiceInfo.foreground;
end;

class function TAndroidHelperEx.IsServiceRunning(const AServiceName: string): Boolean;
begin
  Result := GetRunningServiceInfo(AServiceName) <> nil;
end;

class function TAndroidHelperEx.KeyguardManager: JKeyguardManager;
var
  LService: JObject;
begin
  if FKeyguardManager = nil then
  begin
    LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.KEYGUARD_SERVICE);
    if LService <> nil then
      FKeyguardManager := TJKeyguardManager.Wrap(TAndroidHelper.JObjectToID(LService));
  end;
  Result := FKeyguardManager;
end;

class function TAndroidHelperEx.NotificationManager: JNotificationManager;
var
  LService: JObject;
begin
  if FNotificationManager = nil then
  begin
    LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE);
    FNotificationManager := TJNotificationManager.Wrap(TAndroidHelper.JObjectToID(LService));
  end;
  Result := FNotificationManager;
end;

class function TAndroidHelperEx.PowerManager: JPowerManager;
var
  LService: JObject;
begin
  if FPowerManager = nil then
  begin
    LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.POWER_SERVICE);
    if LService <> nil then
      FPowerManager := TJPowerManager.Wrap(TAndroidHelper.JObjectToID(LService));
  end;
  Result := FPowerManager;
end;

class procedure TAndroidHelperEx.RestartIfNotIgnoringBatteryOptimizations;
var
  LIntent: JIntent;
begin
  if not IsIgnoringBatteryOptimizations then
  begin
    LIntent := TJIntent.Create;
    LIntent.setAction(TJSettings.javaClass.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
    LIntent.setData(TJnet_Uri.JavaClass.parse(StringToJString('package:' + JStringtoString(TAndroidHelper.Context.getPackageName()))));
    // Restart app with action request
    TAndroidHelper.Context.startActivity(LIntent);
  end;
end;

function GetTimeFromNowInMillis(const ASecondsFromNow: Integer): Int64;
var
  LCalendar: JCalendar;
begin
  LCalendar := TJCalendar.JavaClass.getInstance;
  if ASecondsFromNow > 0 then
    LCalendar.add(TJCalendar.JavaClass.SECOND, ASecondsFromNow);
  Result := LCalendar.getTimeInMillis;
end;

class procedure TAndroidHelperEx.SetStartAlarm(const AAlarm: TDateTime; const AStartFromLock: Boolean);
var
  LActionIntent: JIntent;
  LAlarmIntent: JPendingIntent;
  LStartAt: Int64;
begin
  LActionIntent := TJIntent.JavaClass.init(StringToJString(cActionStartAlarm));
  LActionIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cMultiBroadcastReceiverName));
  LActionIntent.putExtra(StringToJString(cExtraStartUnlock), AStartFromLock);
  LAlarmIntent := TJPendingIntent.JavaClass.getBroadcast(TAndroidHelper.Context, 0, LActionIntent, TJPendingIntent.JavaClass.FLAG_CANCEL_CURRENT);
  LStartAt := GetTimeFromNowInMillis(SecondsBetween(Now, AAlarm));
  // Allow for alarms while in "doze" mode
  if TOSVersion.Check(6) then
    TAndroidHelper.AlarmManager.setExactAndAllowWhileIdle(TJAlarmManager.JavaClass.RTC_WAKEUP, LStartAt, LAlarmIntent)
  else
    TAndroidHelper.AlarmManager.&set(TJAlarmManager.JavaClass.RTC_WAKEUP, LStartAt, LAlarmIntent);
end;

{ TJImageHelper }

class function TJImageHelper.RotateBytes(const ABytes: TJavaArray<Byte>; const ARotation: Integer): TJavaArray<Byte>;
var
  LMatrix: JMatrix;
  LBitmap, LRotatedBitmap: JBitmap;
  LOutputStream: JByteArrayOutputStream;
begin
  LMatrix := TJMatrix.JavaClass.init;
  LMatrix.postRotate(ARotation);
  LBitmap := TJBitmapFactory.JavaClass.decodeByteArray(ABytes, 0, ABytes.Length);
  try
    try
      LRotatedBitmap := TJBitmap.JavaClass.createBitmap(LBitmap, 0, 0, LBitmap.getWidth, LBitmap.getHeight, LMatrix, True);
      LOutputStream := TJByteArrayOutputStream.JavaClass.init(0);
      LRotatedBitmap.compress(TJBitmap_CompressFormat.JavaClass.JPEG, 100, LOutputStream);
      Result := LOutputStream.toByteArray;
    finally
      LRotatedBitmap.recycle;
    end;
  finally
    LBitmap.recycle;
  end;
end;

class function TJImageHelper.JImageToByteArray(const AImage: JImage): TJavaArray<Byte>;
var
  LBuffer: JByteBuffer;
begin
  Result := nil;
  if AImage.getFormat = TJImageFormat.JavaClass.JPEG then
  begin
    LBuffer := AImage.getPlanes.Items[0].getBuffer;
    Result := TJavaArray<Byte>.Create(LBuffer.capacity);
    LBuffer.get(Result);
  end
  else if AImage.getFormat = TJImageFormat.JavaClass.YUV_420_888 then
    Result := NV21ToJPEG(YUV_420_888ToNV21(AImage), AImage.getWidth, AImage.getHeight);
end;

class function TJImageHelper.JImageToJBitmap(const AImage: JImage): JBitmap;
var
  LBytes: TJavaArray<Byte>;
begin
  LBytes := JImageToByteArray(AImage);
  Result := TJBitmapFactory.JavaClass.decodeByteArray(LBytes, 0, LBytes.Length);
end;

class function TJImageHelper.JImageToStream(const AImage: JImage; const ARotation: Integer = 0): TStream;
var
  LBytes: TJavaArray<Byte>;
begin
  LBytes := JImageToByteArray(AImage);
  if ARotation > 0 then
    LBytes := RotateBytes(LBytes, ARotation);
  Result := TMemoryStream.Create;
  Result.WriteBuffer(LBytes.Data^, LBytes.Length);
end;

class function TJImageHelper.NV21ToJPEG(const ABytes: TJavaArray<Byte>; const AWidth, AHeight: Integer): TJavaArray<Byte>;
var
  LStream: JByteArrayOutputStream;
  LYUV21Image: JYuvImage;
begin
  LStream := TJByteArrayOutputStream.Create;
  LYUV21Image := TJYuvImage.JavaClass.init(ABytes, TJImageFormat.JavaClass.NV21, AWidth, AHeight, nil);
  LYUV21Image.compressToJpeg(TJRect.JavaClass.init(0, 0, AWidth, AHeight), 100, LStream);
  Result := LStream.toByteArray;
end;

class function TJImageHelper.YUV_420_888ToNV21(const AImage: JImage): TJavaArray<Byte>;
var
  LYBuffer, LUBuffer, LVBuffer: JByteBuffer;
  LYSize, LUSize, LVSize: Integer;
begin
  LYBuffer := AImage.getPlanes.Items[0].getBuffer;
  LUBuffer := AImage.getPlanes.Items[1].getBuffer;
  LVBuffer := AImage.getPlanes.Items[2].getBuffer;
  LYSize := LYBuffer.remaining;
  LUSize := LUBuffer.remaining;
  LVSize := LVBuffer.remaining;
  Result := TJavaArray<Byte>.Create(LYSize + LUSize + LVSize);
  LYBuffer.get(Result, 0, LYSize);
  LVBuffer.get(Result, LYSize, LVSize);
  LUBuffer.get(Result, LYSize + LVSize, LUSize);
end;

end.
