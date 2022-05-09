package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2021 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Handler;
import android.provider.Settings;
import android.util.Log;
import android.view.WindowManager;
import java.lang.Runnable;
import java.util.HashMap;
import java.util.Map;

public class DWUtility {

  private static final String TAG = "DWUtility";

  public static boolean isPermissionInManifest(Context context, String permission) {
    try {    
      PackageInfo info = context.getPackageManager().getPackageInfo(context.getPackageName(), PackageManager.GET_PERMISSIONS);
      if (info.requestedPermissions != null) {
        for (String requestedPermission: info.requestedPermissions) {
          if (requestedPermission.equals(permission))
            return true;
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }        
    return false;
  } 

  public static boolean isPackageInstalled(Context context, String packageName) {
    try {    
      PackageInfo info = context.getPackageManager().getPackageInfo(packageName, 0);
      Log.d(TAG, "Package " + packageName + " is installed");
      return true;
    } catch (PackageManager.NameNotFoundException e) {
      Log.d(TAG, "Package " + packageName + " is not installed");
      return false;
    }        
  }

  public static float getScreenBrightness(Activity activity) throws Settings.SettingNotFoundException {
    float value = activity.getWindow().getAttributes().screenBrightness;
    if ((value < 0) || (value > 1))
      value = Settings.System.getInt(activity.getContentResolver(), Settings.System.SCREEN_BRIGHTNESS) / 255f;
    return value;
  }

  public static void setScreenBrightness(Activity activity, float brightness) {
    WindowManager.LayoutParams params = activity.getWindow().getAttributes();
    params.screenBrightness = brightness;
    activity.getWindow().setAttributes(params);
  }

  public static Map<Object, Object> createObjectMap() {
    return new HashMap<Object, Object>();
  }

  public static void crashTest() {
    // Exception needs to be delayed so as to bypass JNI call exception handler
    Handler handler = new Handler();
    handler.postDelayed(new Runnable() {
      @Override
      public void run() {
        throw new NullPointerException();
      }
    }, 100);
  }
}