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

/*
  DWMultiBroadcastReceiver can assist with starting your app after an update, or on boot. In order for the action handlers to start your app,
  you will need to add the corresponding permission, receiver and metadata items in the manifest, e.g:

  <!-- **** Required for start on boot  **** -->
  <uses-permission android:name="android.permission.RECEIVE_BOOT_COMPLETED" />
  <!-- **** Required for start with unlock  **** -->
  <uses-permission android:name="android.permission.WAKE_LOCK" />
  <uses-permission android:name="android.permission.DISABLE_KEYGUARD" />

  <!-- The application tag should already exist -->
  <application android:icon="@drawable/icon" android:label="@string/app_name">
    <activity android:name=".MainMenu" android:label="@string/app_name">
      <intent-filter>
        <action android:name="android.intent.action.MAIN" />
          <category android:name="android.intent.category.LAUNCHER" />
        </intent-filter>
    </activity>

    <!-- **** This is the metadata **** -->
    <meta-data android:name="DWMultiBroadcastReceiver.KEY_RESTART_AFTER_REPLACE" android:value="true" />
    <meta-data android:name="DWMultiBroadcastReceiver.KEY_START_ON_BOOT" android:value="true" />
    <meta-data android:name="DWMultiBroadcastReceiver.KEY_START_SERVICE_ON_BOOT" android:value="[yourservicename]>" />
    <!-- Required if the device is to wake up when a notification is sent -->
    <meta-data android:name="DWMultiBroadcastReceiver.WAKE_ON_NOTIFICATION" android:value="true" />

    <!-- **** This is the BroadcastReceiver. It handles the intents listed in the intent-filter tag. 
      Note: QUICKBOOT_POWERON is the intent for a restart, as opposed to a cold boot **** -->
    <receiver android:name="com.delphiworlds.kastri.DWMultiBroadcastReceiver">
      <intent-filter>
        <action android:name="android.intent.action.MY_PACKAGE_REPLACED"/>
        <action android:name="android.intent.action.BOOT_COMPLETED"/>
        <action android:name="android.intent.action.QUICKBOOT_POWERON" />
        <action android:name="com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_SERVICE_ALARM" />
      </intent-filter>
    </receiver>
  </application>
*/

import android.app.AlarmManager;
import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.app.KeyguardManager;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.os.Build;
import android.os.Bundle;
import android.os.PowerManager;
import android.util.Log;

import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.app.JobIntentService;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.util.Calendar;
import java.util.List;
import java.util.Locale;

import javax.naming.NameNotFoundException;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class DWMultiBroadcastReceiver extends BroadcastReceiver {

  private static final String TAG = "DWMultiBroadcastReceiver";

  private static final String KEY_RESTART_AFTER_REPLACE = "DWMultiBroadcastReceiver.KEY_RESTART_AFTER_REPLACE"; // true or false
  private static final String KEY_START_ON_BOOT = "DWMultiBroadcastReceiver.KEY_START_ON_BOOT"; // true or false
  private static final String KEY_START_ON_BOOT_CHANNEL_ID = "DWMultiBroadcastReceiver.KEY_START_ON_BOOT_CHANNEL_ID";
  private static final String KEY_START_SERVICE_ON_BOOT = "DWMultiBroadcastReceiver.KEY_START_SERVICE_ON_BOOT"; // string = service name
  private static final String WAKE_LOCK_ID = "DWMultiBroadcastReceiver.WAKE_LOCK";
  private static final String WAKE_ON_NOTIFICATION = "DWMultiBroadcastReceiver.WAKE_ON_NOTIFICATION";

  public static final String ACTION_SERVICE_ALARM = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_SERVICE_ALARM";
  public static final String ACTION_ALARM_TIMER = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_ALARM_TIMER";
  public static final String ACTION_SERVICE_RESTART = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_SERVICE_RESTART";
  public static final String ACTION_START_ALARM = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_START_ALARM";
  public static final String ACTION_NOTIFICATION = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_NOTIFICATION";
  public static final String ACTION_NOTIFICATION_ACTION = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_NOTIFICATION_BUTTON";
  public static final String EXTRA_BUTTON_NAME = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_BUTTON_NAME";
  public static final String EXTRA_NOTIFICATION = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_NOTIFICATION";
  public static final String EXTRA_NOTIFICATION_ACTION_NAME = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_NOTIFICATION_ACTION_NAME";
  public static final String EXTRA_NOTIFICATION_ID = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_NOTIFICATION_ID";
  public static final String EXTRA_NOTIFICATION_IMAGE = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_NOTIFICATION_IMAGE";
  public static final String EXTRA_NOTIFICATION_NAME = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_NOTIFICATION_NAME";
  public static final String EXTRA_NOTIFICATION_REPEATINTERVAL = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_NOTIFICATION_REPEATINTERVAL";
  public static final String EXTRA_SERVICE_RESTART = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_SERVICE_RESTART";
  public static final String EXTRA_SERVICE_CLASS_NAME = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_SERVICE_CLASS_NAME";
  public static final String EXTRA_JOB_ID = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_JOB_ID";
  public static final String EXTRA_START_UNLOCK = "com.delphiworlds.kastri.DWMultiBroadcastReceiver.EXTRA_START_UNLOCK";

	private DWMultiBroadcastReceiverDelegate mDelegate;
	
	public DWMultiBroadcastReceiver() {
    //
  }

	public DWMultiBroadcastReceiver(DWMultiBroadcastReceiverDelegate delegate) {
		mDelegate = delegate;
  }

  private static void enqueueWork(Context context, Intent intent, String serviceClassName, int jobId) {
    doEnqueueWork(context, intent, serviceClassName, jobId);
  }

  private static void doEnqueueWork(Context context, Intent intent, String serviceClassName, int jobId) {
    try {
      Log.d(TAG, "Calling enqueueWork for: " + serviceClassName + " with jobId: " + String.valueOf(jobId));
      JobIntentService.enqueueWork(context, Class.forName(serviceClassName), jobId, intent);
    } catch (ClassNotFoundException e) {
      Log.e(TAG, "Could not find service: " + serviceClassName);
    }
  }

  private boolean startApp(Context context) {
    context.startActivity(context.getPackageManager().getLaunchIntentForPackage(context.getPackageName()));
    return true;
  }

  private static String rawResourceToString(Context context, String resourceName) {
    int resourceId = context.getResources().getIdentifier(resourceName, "raw", context.getPackageName());
    if (resourceId == 0) {
      Log.d(TAG, "resourceName: " + resourceName + " not present");
      return null;
    }
    InputStream input = context.getResources().openRawResource(resourceId);
    BufferedReader reader = new BufferedReader(new InputStreamReader(input));
    String line;
    StringBuilder sb = new StringBuilder();
    try {
      while ((line = reader.readLine()) != null) {
        sb.append(line);
        sb.append('\n');
      }
    } catch (IOException e) {
      return null;
    }
    return sb.toString();
  }

  private static String getCurrentLocaleIdentifier(Context context) {
    Configuration configuration = context.getResources().getConfiguration();
    Locale locale;
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N)
      locale = configuration.getLocales().get(0);
    else
      locale = configuration.locale;
    return locale.toString();
  }

  private static int getDefaultAppIconId(Context context) {
    try {
      return context.getPackageManager().getApplicationInfo(context.getPackageName(), 0).icon;
    } catch (PackageManager.NameNotFoundException e) {
        e.printStackTrace();
    }
    return 0;
  }

  private boolean sendStartupNotification(Context context, String channelId) {
    Log.d(TAG, "Sending startup notification");
    String defaultTitle = "Start At Boot";
    String defaultBody = "Please tap this notification for the application to start";
    String title = null;
    String body = null;
    String json = rawResourceToString(context, "startupnotification");
    if (json != null) {
      String currentLocale = getCurrentLocaleIdentifier(context);
      try {
        JSONArray jsonArray = new JSONArray(json);
        for (int i = 0; i < jsonArray.length(); i++) {
          JSONObject jsonObject = jsonArray.getJSONObject(i);
          // First values are the default
          if (i == 0) {
            JSONObject localeObject = jsonObject.getJSONObject("default");
            if (localeObject != null) {
              defaultTitle = localeObject.getString("title");
              defaultBody = localeObject.getString("body");
            }
          } else if (jsonObject.has(currentLocale)) {
            JSONObject localeObject = jsonObject.getJSONObject(currentLocale);
            title = localeObject.getString("title");
            body = localeObject.getString("body");
            break;
          }
        }
      } catch (JSONException e) {
          Log.e(TAG, "Error reading startupnotification json", e);
      }
    }
    Intent intent = new Intent();
    intent.putExtra("title", (title != null) ? title : defaultTitle);
    intent.putExtra("body", (body != null) ? body : defaultBody);
    intent.putExtra("priority", Integer.toString(NotificationCompat.PRIORITY_HIGH));
    intent.putExtra("isFullScreen", "1");
    DWNotificationPresenter.presentNotification(context, intent, channelId, getDefaultAppIconId(context));
    return true;
  }

  private int getTargetSdkVersion(Context context) {
    try {
      return context.getPackageManager().getApplicationInfo(context.getPackageName(), 0).targetSdkVersion;
    } catch (PackageManager.NameNotFoundException exception) {
      return 0; // this should never happen :-)
    }
  }

  private boolean isAppForeground(Context context) {
    ActivityManager am = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
    List<ActivityManager.RunningAppProcessInfo> processes = am.getRunningAppProcesses();
    if (processes == null) return false;

    String packageName = context.getPackageName();
    for (ActivityManager.RunningAppProcessInfo info : processes) {
        if (info.processName.equals(packageName) && info.importance == RunningAppProcessInfo.IMPORTANCE_FOREGROUND) {
            return true;
        }
    }
    return false;
  }

  private boolean checkBuildAndTarget(Context context, int value) {
    return (getTargetSdkVersion(context) >= value) && (Build.VERSION.SDK_INT >= value);
  }

  private boolean checkStartupIntent(Context context, Intent intent) {

    // Retrieve the metadata
    Bundle metaData = null;
    try {
      metaData = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA).metaData;
    } catch (PackageManager.NameNotFoundException exception) {
      return false;
    }
    Boolean handled = false;

    // This action will start the application if it has been updated, e.g. when "side-loaded".
    // Note: If your app is on Google Play, you will likely want to omit having the option in the metadata
    if (intent.getAction().equals(Intent.ACTION_MY_PACKAGE_REPLACED)) {
      if ((metaData != null) && metaData.containsKey(KEY_RESTART_AFTER_REPLACE)) {
        if (metaData.getBoolean(KEY_RESTART_AFTER_REPLACE))
          return startApp(context);
      }
    }

    // This action will start the application or service on bootup of the device
    // Note: This action also needs <uses-permission android:name="android.permission.RECEIVE_BOOT_COMPLETED" />  in the manifest    
    if (intent.getAction().equals(Intent.ACTION_BOOT_COMPLETED)) {
      Log.d(TAG, "Intent.ACTION_BOOT_COMPLETED");
      if ((metaData != null) && metaData.containsKey(KEY_START_ON_BOOT)) {
        if (metaData.getBoolean(KEY_START_ON_BOOT)) {
          if (Build.VERSION.SDK_INT < 29)
            return startApp(context);
          else {
            String channelId = metaData.containsKey(KEY_START_ON_BOOT_CHANNEL_ID) ? metaData.getString(KEY_START_ON_BOOT_CHANNEL_ID) : null;
            return sendStartupNotification(context, channelId);
          }
        }
      }
      if ((metaData != null) && metaData.containsKey(KEY_START_SERVICE_ON_BOOT)) {
        String serviceName = "com.embarcadero.services." + metaData.getString(KEY_START_SERVICE_ON_BOOT);
        Log.d(TAG, "Attempting to start service from boot: " + serviceName);
        Intent serviceIntent = new Intent();
        serviceIntent.setClassName(context.getPackageName(), serviceName);
        if (checkBuildAndTarget(context, 26) && !isAppForeground(context)) 
          context.startForegroundService(serviceIntent); // Service MUST call startForeground when it starts
        else
          context.startService(serviceIntent);
        return true;
      }
    }

    // Starting a service from an alarm or restart. The intent should already have the class name set in the intent
    if (intent.getAction().equals(ACTION_SERVICE_ALARM) || intent.getAction().equals(ACTION_SERVICE_RESTART)) {
      Log.d(TAG, "Attempting to restart service or start service from alarm");
      boolean isForeground = isAppForeground(context);
      // Start a job service
      int jobId = intent.getIntExtra(EXTRA_JOB_ID, 0);
      String serviceClassName = intent.getStringExtra(EXTRA_SERVICE_CLASS_NAME);
      if (serviceClassName != null) {
        if (jobId == 0)
          intent.setClassName(context.getPackageName(), serviceClassName);
        // Some restart cases require the service not to be started in foreground mode
        boolean startNormal = intent.getIntExtra("MustStartNormal", 0) == 1;
        if (intent.getAction().equals(ACTION_SERVICE_RESTART))
          intent.putExtra(EXTRA_SERVICE_RESTART, 1);
        if (jobId != 0) {
          enqueueWork(context, intent, serviceClassName, jobId);
        } 
        else if (!startNormal && checkBuildAndTarget(context, 26) && !isForeground) {
          Log.d(TAG, "Calling startForegroundService for: " + serviceClassName);
          context.startForegroundService(intent); // Service MUST call startForeground when it starts if the app is in the background or not running
        }
        else if (isForeground || !checkBuildAndTarget(context, 26)) {
          Log.d(TAG, "Calling startService for: " + serviceClassName);
          context.startService(intent); // Can only start the service "normally" if the app is in the foreground or lower than Android 8
        }
        else
          Log.w(TAG, "Cannot start service in any mode");
      }
      else
        Log.e(TAG, "Action is service related, but no service is specified (" + EXTRA_SERVICE_CLASS_NAME + " is missing)");
      return true;
    }

    // Starting the application from an alarm. 
    if (intent.getAction().equals(ACTION_START_ALARM)) {
      Log.d(TAG, "Attempting to start the application from an alarm");
      PowerManager.WakeLock wakeLock = null;
      if (intent.getBooleanExtra(EXTRA_START_UNLOCK, false)) {
        Log.d(TAG, "Attempting to start from lock screen (if locked)");
        PowerManager powerManager = (PowerManager) context.getSystemService(Context.POWER_SERVICE);
        int wakeLevel = PowerManager.SCREEN_BRIGHT_WAKE_LOCK | PowerManager.FULL_WAKE_LOCK | PowerManager.ACQUIRE_CAUSES_WAKEUP;
        wakeLock = powerManager.newWakeLock(wakeLevel, WAKE_LOCK_ID);
        wakeLock.acquire();
        Log.d(TAG, "Acquired WakeLock");
        KeyguardManager manager = (KeyguardManager) context.getSystemService(Context.KEYGUARD_SERVICE);
        KeyguardManager.KeyguardLock keyguardLock = manager.newKeyguardLock(TAG);
        keyguardLock.disableKeyguard();
        Log.d(TAG, "Disabled Keyguard");
      }
      boolean result = startApp(context);
      if (wakeLock != null)
        wakeLock.release();
      return result;
    }

    return false;
  }

  private static long getAlarmTime(int repeatInterval) {
    Calendar calendar = Calendar.getInstance();
    switch (repeatInterval) {
      case 1: { 
        calendar.add(Calendar.SECOND, 1);
        return calendar.getTimeInMillis();
      }
      case 2: { 
        calendar.add(Calendar.MINUTE, 1);
        return calendar.getTimeInMillis();
      }
      case 3: {
        calendar.add(Calendar.HOUR, 1);
        return calendar.getTimeInMillis();
      }
      case 4: {
        calendar.add(Calendar.DATE, 1);
        return calendar.getTimeInMillis();
      }
      case 5: {		
        calendar.add(Calendar.DATE, 7);
        return calendar.getTimeInMillis();
      }
      case 6: {
        return 0;
      }
      case 7: {
        calendar.add(Calendar.MONTH, 1);
        return calendar.getTimeInMillis();
      }
      case 8: {
        calendar.add(Calendar.MONTH, 3);
        return calendar.getTimeInMillis();
      }
      case 9: {
        calendar.add(Calendar.YEAR, 1);
        return calendar.getTimeInMillis();
      }
      case 10: {
        calendar.add(Calendar.ERA, 1);
        return calendar.getTimeInMillis();
      }
    default:
      return 0;
    }		
  }

  private void setRepeatAlarm(Context context, Intent intent, long alarmTime) {
    int id = intent.getIntExtra(EXTRA_NOTIFICATION_ID, 0);
    Log.d(TAG, "Setting repeat alarm for id: " + id);
    PendingIntent pendingIntent = PendingIntent.getBroadcast(context, id, intent, PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);
		AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
    if (Build.VERSION.SDK_INT >= 21)
      alarmManager.setExactAndAllowWhileIdle(AlarmManager.RTC_WAKEUP, alarmTime, pendingIntent);
    else
		  alarmManager.set(AlarmManager.RTC_WAKEUP, alarmTime, pendingIntent);
  }

  private static String getNotificationTitle(Notification notification) {
    return notification.extras.getString(Notification.EXTRA_TITLE);
  }

  private static String getNotificationText(Notification notification) {
    CharSequence text = notification.extras.getCharSequence(Notification.EXTRA_TEXT);
    return text != null ? text.toString() : null;
  }

  @Override
  public void onReceive(Context context, Intent intent) {
    if (mDelegate == null) {
      Log.d(TAG, "Received intent with action: " + intent.getAction());
      if (ACTION_NOTIFICATION.equals(intent.getAction())) {
        // Broadcast to the app to handle the notification if the app is running
        LocalBroadcastManager.getInstance(context).sendBroadcast(intent);
        DWWakeUp.checkWakeUp(context, WAKE_ON_NOTIFICATION, false);
        Notification notification = intent.getParcelableExtra(EXTRA_NOTIFICATION);
        Intent notificationIntent = new Intent();
        notificationIntent.putExtra("title", getNotificationTitle(notification));
        notificationIntent.putExtra("body", getNotificationText(notification));
        notificationIntent.putExtra("notifyId", intent.getIntExtra(EXTRA_NOTIFICATION_ID, 0));
        if (intent.hasExtra("resource_name"))
          notificationIntent.putExtra("resource_name", intent.getStringExtra("resource_name"));
        if (intent.hasExtra("notification_actions"))
          notificationIntent.putStringArrayListExtra("notification_actions", intent.getStringArrayListExtra("notification_actions"));
        if (intent.hasExtra("isInsistent"))
          notificationIntent.putExtra("isInsistent", intent.getStringExtra("isInsistent"));
        if (intent.hasExtra(EXTRA_NOTIFICATION_IMAGE)) {
          String imagePath = intent.getStringExtra(EXTRA_NOTIFICATION_IMAGE);
          if (imagePath != null && !imagePath.isEmpty()) {
            Log.d(TAG, "imagePath: " + imagePath);
            File file = new File(imagePath);
            try {
              notificationIntent.putExtra("imageUrl", file.toURI().toURL().toString());
            } catch (MalformedURLException e) {
              Log.e(TAG, "Image path invalid: " + imagePath);
            }
          }
        }
        DWNotificationPresenter.presentNotification(context, notificationIntent, notification.getChannelId(), notification.getSmallIcon().getResId());
        // Set alarm if repeating
        long alarmTime = getAlarmTime(notification.extras.getInt(EXTRA_NOTIFICATION_REPEATINTERVAL, 0));
        if (alarmTime != 0)
          setRepeatAlarm(context, intent, alarmTime);
      } else if (intent.getAction().equals(ACTION_ALARM_TIMER)) {
        LocalBroadcastManager.getInstance(context).sendBroadcast(intent);
      } else if (!checkStartupIntent(context, intent))
        // Simply forward on the intent in a local broadcast
        LocalBroadcastManager.getInstance(context).sendBroadcast(intent);
    }
    else
      mDelegate.onReceive(context, intent);
  }

}