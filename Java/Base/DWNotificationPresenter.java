package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2023 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.util.Log;
import android.view.View;
import android.widget.RemoteViews;
import androidx.core.app.NotificationCompat;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

public class DWNotificationPresenter
{
  private static final String TAG = "DWNotificationPresenter";
  private static int mUniqueId = 0;
  
  private static Bitmap getBitmap(URL url) {
    Bitmap bitmap = null;
    try {
      bitmap = BitmapFactory.decodeStream(url.openConnection().getInputStream());
    } catch (IOException iOException) {
      Log.w(TAG, "Could not retrieve image from " + url);
    } 
    if (bitmap != null) {
      int i;
      if (bitmap.getWidth() < bitmap.getHeight()) {
        i = bitmap.getWidth();
      } else {
        i = bitmap.getHeight();
      } 
      Bitmap compareBitmap = Bitmap.createBitmap(bitmap, (bitmap.getWidth() - i) / 2, (bitmap.getHeight() - i) / 2, i, i, null, true);
      if (!bitmap.sameAs(compareBitmap))
        bitmap.recycle(); 
      return compareBitmap;
    }  
    Log.d(TAG, "Failed to retrieve or decode image");
    return null;
  }

  private static int getResourceId(Context context, String id) { 
    return context.getResources().getIdentifier(id, null, context.getPackageName()); 
  }
 
  private static RemoteViews getCustomContentView(Context context, String title, String body, String imageUrl) {
    int layoutId = getResourceId(context, "layout/notification_custom");
    RemoteViews remoteViews = null;
    if (layoutId > 0) {
      remoteViews = new RemoteViews(context.getPackageName(), layoutId);
      remoteViews.setTextViewText(getResourceId(context, "id/notification_custom_title"), title);
      remoteViews.setTextViewText(getResourceId(context, "id/notification_custom_body"), body);
      Bitmap bitmap = null;
      if (imageUrl != null && !imageUrl.isEmpty()) {
        try {
          bitmap = getBitmap(new URL(imageUrl));
        } catch (MalformedURLException e) {
          Log.w("DWNotificationPresenter", "imageUrl invalid: " + imageUrl);
        } 
      }
      int imageId = getResourceId(context, "id/notification_custom_image");
      if (bitmap != null) {
        remoteViews.setImageViewBitmap(imageId, bitmap);
      } else {
        remoteViews.setViewVisibility(imageId, View.GONE);
      } 
    } else {
      Log.w("DWNotificationPresenter", "Unable to locate resource notification_custom");
    }  return remoteViews;
  }

  private static String getChannelId(Intent intent, String defaultChannelId) {
    String channelId = defaultChannelId;
    if (intent.hasExtra("gcm.notification.android_channel_id"))
      channelId = intent.getStringExtra("gcm.notification.android_channel_id");
    else if (intent.hasExtra("channel_id"))
      channelId = intent.getStringExtra("channel_id");
    return channelId;
  }

  private static String getDefaultChannelId(NotificationManager notificationManager) {
    String channelId = "default";
    NotificationChannel channel = notificationManager.getNotificationChannel(channelId);
    if (channel == null) {
      channel = new NotificationChannel(channelId, "Default notifications", 4);
      channel.enableLights(true);
      channel.enableVibration(true);
      channel.setLightColor(Color.GREEN);
      channel.setLockscreenVisibility(Notification.VISIBILITY_PRIVATE);
      channel.setImportance(NotificationManager.IMPORTANCE_HIGH);
      notificationManager.createNotificationChannel(channel);
    }
    return channel.getId();
  }
  
  public static void presentNotification(Context context, Intent intent, String channelId, int iconId) {
    // presentNotification can be called from more than one origin, so if it is silent, stop it here..
    if (intent.hasExtra("isSilent") && intent.getStringExtra("isSilent").equals("1"))
      return;
    mUniqueId++;
    intent.setClassName(context, "com.embarcadero.firemonkey.FMXNativeActivity");
    intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
    PendingIntent pendingIntent = PendingIntent.getActivity(context, mUniqueId, intent, PendingIntent.FLAG_IMMUTABLE | PendingIntent.FLAG_UPDATE_CURRENT);
    String title = intent.hasExtra("title") ? intent.getStringExtra("title") : "";
    String body = intent.hasExtra("body") ? intent.getStringExtra("body") : "";
    String imageUrl = intent.hasExtra("imageUrl") ? intent.getStringExtra("imageUrl") : "";
    RemoteViews remoteViews = getCustomContentView(context, title, body, imageUrl);
    NotificationManager notificationManager = (NotificationManager)context.getSystemService("notification");
    String notificationChannelId = DWNotificationPresenter.getChannelId(intent, channelId);
    notificationChannelId = (notificationChannelId != null) ? notificationChannelId : getDefaultChannelId(notificationManager);
    NotificationCompat.Builder builder = new NotificationCompat.Builder(context, notificationChannelId)
      .setContentTitle(title)
      .setContentText(body)
      .setSmallIcon(iconId)
      .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
      .setContentIntent(pendingIntent)
      .setWhen(System.currentTimeMillis())
      .setShowWhen(true)
      .setAutoCancel(true);
    if (remoteViews != null)
      builder = builder.setStyle(new NotificationCompat.DecoratedCustomViewStyle()).setCustomContentView(remoteViews);
    if (intent.hasExtra("isFullScreen"))
      builder = builder.setFullScreenIntent(pendingIntent, true); 
    notificationManager.notify(mUniqueId, builder.build());
  }
}
