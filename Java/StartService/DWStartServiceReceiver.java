package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2026 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.util.Log;

public class DWStartServiceReceiver extends BroadcastReceiver {

  private static final String TAG = "DWStartServiceReceiver";

  private void startService(Context context, ServiceInfo serviceInfo) {
    Log.d(TAG, "Attempting to start: " + serviceInfo.name);
    Intent serviceIntent = new Intent();
    serviceIntent.setClassName(context.getPackageName(), serviceInfo.name);
    serviceIntent.putExtra("startAtBoot", true);
    if (Build.VERSION.SDK_INT >= 26)
      context.startForegroundService(serviceIntent); // Service MUST call startForeground when it starts
    else
      context.startService(serviceIntent); 
  }

  @Override
  public void onReceive(Context context, Intent intent) {
    if (intent.getAction().equals(Intent.ACTION_BOOT_COMPLETED)) {
      Log.d(TAG, "ACTION_BOOT_COMPLETED received");
      PackageInfo packageInfo = null;
      try {
        packageInfo = context.getPackageManager().getPackageInfo(context.getPackageName(), PackageManager.GET_SERVICES | PackageManager.GET_META_DATA);
      } catch (PackageManager.NameNotFoundException exception) {
        // Do nothing
      } 
      if ((packageInfo != null) && (packageInfo.services != null))  {  
        for (ServiceInfo serviceInfo : packageInfo.services) {
          Log.d(TAG, "Service: " + serviceInfo.name);
          if (serviceInfo.metaData != null) {
            if (serviceInfo.metaData.getBoolean("startAtBoot"))
              startService(context, serviceInfo);        
          }
          else
            Log.d(TAG, "No service metaData found");
        }
      }
      else
        Log.d(TAG, "Either no PackageInfo, or no services");
    }
  }
}
