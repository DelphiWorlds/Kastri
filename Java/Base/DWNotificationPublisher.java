package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 *   Copyright 2020 Dave Nottage under MIT license     *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

 // Example metadata: <meta-data android:name="DWNotificationPublisher.WAKE_ON_NOTIFICATION" android:value="true" />

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.support.v4.app.NotificationCompat;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import java.io.IOException;
import java.net.URL;

public class DWNotificationPublisher {

  private static final String TAG = "DWNotificationPublisher";
  private static int mUniqueId = 0;
  private static int mDefaultPriority = -1; // Not present
  private static int mDefaultSmallIcon = -1; // Not present
  private static NotificationChannel mDefaultChannel;
  private static NotificationManager mNotificationManager = null;
  private static final String WAKE_ON_NOTIFICATION = "DWNotificationPublisher.WAKE_ON_NOTIFICATION";
  private static final String NOTIFICATION_DEFAULT_PRIORITY = "DWNotificationPublisher.NOTIFICATION_DEFAULT_PRIORITY";
  private static final String FIREBASE_DEFAULT_CHANNEL_ID = "com.google.firebase.messaging.default_notification_channel_id";
  private static final String FIREBASE_DEFAULT_SMALL_ICON = "com.google.firebase.messaging.default_notification_icon";

  private static void initialize(Context context) {
      if (mNotificationManager != null)
        return;
      Log.w(TAG, "+initialize");
      Bundle metaData = null;
      try {
        metaData = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA).metaData;
      } catch (PackageManager.NameNotFoundException exception) {
        Log.w(TAG, "Unable to load metadata");
      }
      String channelId = "default";
      if (metaData != null) {
        if (metaData.containsKey(NOTIFICATION_DEFAULT_PRIORITY))
          mDefaultPriority = metaData.getInt(NOTIFICATION_DEFAULT_PRIORITY);
        if (metaData.containsKey(FIREBASE_DEFAULT_CHANNEL_ID))
          channelId = metaData.getString(FIREBASE_DEFAULT_CHANNEL_ID);
        if (metaData.containsKey(FIREBASE_DEFAULT_SMALL_ICON))
          mDefaultSmallIcon = metaData.getInt(FIREBASE_DEFAULT_SMALL_ICON);
      }
      mNotificationManager = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
      if (Build.VERSION.SDK_INT < 26)
        return; 
      mDefaultChannel = new NotificationChannel(channelId, channelId + " FCM", 4);
      mDefaultChannel.setName(channelId);
      mDefaultChannel.enableLights(true);
      mDefaultChannel.enableVibration(true);
      mDefaultChannel.setLightColor(Color.GREEN);
      mDefaultChannel.setLockscreenVisibility(Notification.VISIBILITY_PRIVATE);
      mDefaultChannel.setImportance(NotificationManager.IMPORTANCE_HIGH);
      mNotificationManager.createNotificationChannel(mDefaultChannel);
      Log.w(TAG, "Created channel with id: " + channelId);
      Log.w(TAG, "-initialize");
  }

  private static void getLargeIcon(URL url, NotificationCompat.Builder builder) throws IOException {
    Bitmap bitmap = BitmapFactory.decodeStream(url.openConnection().getInputStream());
    if (bitmap != null) {
      int width;
      if (bitmap.getWidth() < bitmap.getHeight()) { 
        width = bitmap.getWidth(); }
      else { 
        width = bitmap.getHeight(); 
      }
      Bitmap bitmapCropped = Bitmap.createBitmap(bitmap, (bitmap.getWidth() - width) / 2, (bitmap.getHeight() - width) / 2, width, width, null, true);
      if (!bitmap.sameAs(bitmapCropped)) 
        bitmap.recycle(); 
      builder = builder.setLargeIcon(bitmapCropped); 
    } 
  }

  public static void sendNotification(Context context, Intent intent, boolean pending) {
    Log.v(TAG, "+sendNotification");
    initialize(context);
    String channelId = "";
    if (Build.VERSION.SDK_INT >= 26) {
      channelId = mDefaultChannel.getId();
      if (intent.hasExtra("android_channel_id")) {
        String id = intent.getStringExtra("android_channel_id");
        NotificationChannel channel = mNotificationManager.getNotificationChannel(id);
        if (mNotificationManager.getNotificationChannel(id) != null)
          channelId = id;
        else
          Log.v(TAG, "Channel not registered: " + id);
      }
    }
    NotificationCompat.Builder builder = new NotificationCompat.Builder(context, channelId);
    if (intent.hasExtra("notification_color")) { 
      builder = builder.setColor(Integer.parseInt(intent.getStringExtra("notification_color")));
    }
    String text = null;
    if (intent.hasExtra("body")) {
      text = intent.getStringExtra("body");      
    }
    builder = builder.setContentText(text);
    if (intent.hasExtra("big_text") && (Integer.parseInt(intent.getStringExtra("big_text")) == 1)) {
      Log.v(TAG, "Notification has big text flag");
      builder = builder.setStyle(new NotificationCompat.BigTextStyle().bigText(text));
    }
    if (intent.hasExtra("title")) { 
      builder = builder.setContentTitle(intent.getStringExtra("title"));
    }
/*
    String smallIconIdent = null;
    if (intent.hasExtra("icon")) { 
      smallIconIdent = intent.getStringExtra("icon");
    }
    int smallIcon = 0;
    if (smallIconIdent != null)
      smallIcon = context.getResources().getIdentifier(smallIconIdent, "drawable", context.getPackageName());
    if (smallIcon == 0)
      smallIcon = mDefaultSmallIcon;
*/
    builder.setSmallIcon(mDefaultSmallIcon);
    if (intent.hasExtra("image")) { 
      try {
        URL url = new URL(intent.getStringExtra("image"));
        DWNotificationPublisher.getLargeIcon(url, builder);    
      } catch(Throwable e) { 
        Log.e(TAG, "Exception", e); 
      }
    }
    if (intent.hasExtra("onlyalertonce") && intent.getStringExtra("onlyalertonce").equals("1")) { 
      builder = builder.setOnlyAlertOnce(true);
    } 
    if (intent.hasExtra("ticker")) { 
      builder = builder.setTicker(intent.getStringExtra("ticker"));
    }
    if (intent.hasExtra("vibrate") && intent.getStringExtra("vibrate").equals("1")) { 
      builder = builder.setVibrate(new long[] { 0, 1200 });
    } 
    if (intent.hasExtra("visibility")) { 
      builder = builder.setVisibility(Integer.parseInt(intent.getStringExtra("visibility")));
    }
    if (intent.hasExtra("priority")) {  
      builder = builder.setPriority(Integer.parseInt(intent.getStringExtra("priority")));
    }
    if (mDefaultPriority > -1)
      builder = builder.setPriority(mDefaultPriority);
    if (pending) {
      intent.setClassName(context, "com.embarcadero.firemonkey.FMXNativeActivity");
      intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
      mUniqueId = mUniqueId + 1;
      PendingIntent pendingIntent = PendingIntent.getActivity(context, mUniqueId, intent, PendingIntent.FLAG_UPDATE_CURRENT);
      builder = builder.setContentIntent(pendingIntent); 
    }
    builder = builder.setDefaults(NotificationCompat.DEFAULT_LIGHTS)
      .setWhen(System.currentTimeMillis())
      .setShowWhen(true)
      .setAutoCancel(true);
    // if (intent.hasExtra("notification_badgecount")) 
    //  ShortcutBadger.applyCount(this.getApplicationContext(), Integer.parseInt(intent.getStringExtra("notification_badgecount")));
    DWWakeUp.checkWakeUp(context, WAKE_ON_NOTIFICATION);
    mNotificationManager.notify(mUniqueId, builder.build());
    Log.v(TAG, "-sendNotification");
  }
}