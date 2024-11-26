package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2024 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.ImageFormat;
import android.graphics.YuvImage;
import android.media.Image;
import android.os.Handler;
import android.provider.Settings;
import android.util.Log;
import android.view.WindowManager;
import java.io.ByteArrayOutputStream;
import java.lang.Runnable;
import java.nio.ByteBuffer;
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

  private static Bitmap jpegImageToBitmap(Image image) {
    byte[] bytes = jpegImageToBytes(image);
    return BitmapFactory.decodeByteArray(bytes, 0, bytes.length, null);
  }

  private static Bitmap yuvImageToBitmap(Image image) {
    ByteBuffer yBuffer = image.getPlanes()[0].getBuffer();
    ByteBuffer uBuffer = image.getPlanes()[1].getBuffer();
    ByteBuffer vBuffer = image.getPlanes()[2].getBuffer();
    int ySize = yBuffer.remaining();
    int uSize = uBuffer.remaining();
    int vSize = vBuffer.remaining();
    byte[] nv21 = new byte[ySize + uSize + vSize];
    yBuffer.get(nv21, 0, ySize);
    vBuffer.get(nv21, ySize, vSize);
    uBuffer.get(nv21, ySize + vSize, uSize);
    YuvImage yuvImage = new YuvImage(nv21, ImageFormat.NV21, image.getWidth(), image.getHeight(), null);
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    yuvImage.compressToJpeg(new android.graphics.Rect(0, 0, image.getWidth(), image.getHeight()), 100, out);
    byte[] jpegBytes = out.toByteArray();
    return BitmapFactory.decodeByteArray(jpegBytes, 0, jpegBytes.length);    
  }

  public static Bitmap imageToBitmap(Image image) {
    int format = image.getFormat();
    if (format == ImageFormat.YUV_420_888)
      return yuvImageToBitmap(image);
    else if (format == ImageFormat.JPEG)
      return jpegImageToBitmap(image);
    return null;
  }

  public static byte[] jpegImageToBytes(Image image) {
    ByteBuffer buffer = image.getPlanes()[0].getBuffer();
    byte[] bytes = new byte[buffer.remaining()];
    buffer.get(bytes); 
    return bytes;
  }
}