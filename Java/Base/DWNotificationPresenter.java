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
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;

public class DWNotificationPresenter
{
  private static final String TAG = "DWNotificationPresenter";
  private static final String FMX_NATIVE_ACTIVITY = "com.embarcadero.firemonkey.FMXNativeActivity";
  private static int mUniqueId = 0;
  private static int mRequestCode = 0;
  
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

  private static int getResourceId(Context context, String id, String type) { 
    return context.getResources().getIdentifier(id, type, context.getPackageName()); 
  }

  private static int getAndroidResourceId(Context context, String id, String type) { 
    return context.getResources().getIdentifier(id, type, "android"); 
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

  private static PendingIntent createActionPendingIntent(Context context, String actionName, int notificationId) {
    Intent intent = new Intent(DWMultiBroadcastReceiver.ACTION_NOTIFICATION_ACTION);
    intent.setClassName(context, FMX_NATIVE_ACTIVITY);
    intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_SINGLE_TOP);
    intent.putExtra(DWMultiBroadcastReceiver.EXTRA_NOTIFICATION_ID, notificationId);
    intent.putExtra(DWMultiBroadcastReceiver.EXTRA_NOTIFICATION_ACTION_NAME, actionName);
    PendingIntent pendingIntent = PendingIntent.getActivity(context, mRequestCode, intent, PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE);
    mRequestCode++;
    Log.i(TAG, "Created pending intent for: " + actionName);
    return pendingIntent;
  }
  
  private static RemoteViews getCustomContentView(Context context, String resourceName, String title, String body, Bitmap image) {
    int layoutId = getResourceId(context, resourceName, "layout");
    RemoteViews remoteViews = null;
    if (layoutId > 0) {
      remoteViews = new RemoteViews(context.getPackageName(), layoutId);
      remoteViews.setTextViewText(getResourceId(context, resourceName + "_title", "id"), title);
      remoteViews.setTextViewText(getResourceId(context, resourceName + "_body", "id"), body);
      int imageId = getResourceId(context, resourceName + "_image", "id");
      if (imageId != 0) {
        if (image != null)
          remoteViews.setImageViewBitmap(imageId, image);
        else
          remoteViews.setViewVisibility(imageId, View.GONE);
      } 
    }
    return remoteViews;
  }

  public static void presentNotification(Context context, Intent intent, String channelId, int iconId) {
    // presentNotification can be called from more than one origin, so if it is silent, stop it here..
    if (intent.hasExtra("isSilent") && intent.getStringExtra("isSilent").equals("1"))
      return;
    int notifyId = intent.getIntExtra("notifyId", 0);
    if (notifyId == 0) {
      mUniqueId++;
      notifyId = mUniqueId;
    }
    intent.setClassName(context, FMX_NATIVE_ACTIVITY);
    intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
    PendingIntent pendingIntent = PendingIntent.getActivity(context, mUniqueId, intent, PendingIntent.FLAG_IMMUTABLE | PendingIntent.FLAG_UPDATE_CURRENT);
    String resourceName = intent.hasExtra("resource_name") ? intent.getStringExtra("resource_name") : "notification_custom";
    String title = intent.hasExtra("title") ? intent.getStringExtra("title") : "";
    String body = intent.hasExtra("body") ? intent.getStringExtra("body") : "";
    Bitmap image = null;
    String imageUrl = intent.hasExtra("imageUrl") ? intent.getStringExtra("imageUrl") : null;
    if (imageUrl != null) {
      try {
        image = getBitmap(new URI(imageUrl).toURL());
      } catch (Exception e) {
        Log.w(TAG, "imageUrl invalid: " + imageUrl);
      } 
    }
    RemoteViews smallView = getCustomContentView(context, resourceName, title, body, image);
    NotificationManager notificationManager = (NotificationManager)context.getSystemService("notification");
    String notificationChannelId = DWNotificationPresenter.getChannelId(intent, channelId);
    notificationChannelId = (notificationChannelId != null) ? notificationChannelId : getDefaultChannelId(notificationManager);
    Bitmap nullBitmap = null;
    NotificationCompat.Builder builder = new NotificationCompat.Builder(context, notificationChannelId)
      .setContentTitle(title)
      .setContentText(body)
      .setSmallIcon(iconId)
      .setDefaults(NotificationCompat.DEFAULT_LIGHTS)
      .setContentIntent(pendingIntent)
      .setWhen(System.currentTimeMillis())
      .setShowWhen(true)
      .setAutoCancel(true)
      .setStyle(new NotificationCompat.BigPictureStyle()
        .bigPicture(image)
        .bigLargeIcon(nullBitmap) // Hides compact thumbnail in expanded mode
        .setBigContentTitle(title) 
        .setSummaryText(body)
      );
    if (smallView != null)
      builder = builder.setCustomContentView(smallView);
    if (intent.hasExtra("isFullScreen"))
      builder = builder.setFullScreenIntent(pendingIntent, true);
    ArrayList<String> actions = intent.getStringArrayListExtra("notification_actions");
    if (actions != null && !actions.isEmpty()) {
      for (String action : actions) {
        String[] actionParts = action.split("=", 2); // actionname=text
        int actionResId = getAndroidResourceId(context, "btn_default", "drawable");
        if (actionResId != 0) {
          PendingIntent actionPendingIntent = createActionPendingIntent(context, actionParts[0], notifyId);
          builder.addAction(actionResId, actionParts[1], actionPendingIntent);
        }
        else
          Log.e(TAG, "Drawable resource: btn_default not found. Pending intent not created");
      }
    }
    else
      Log.i(TAG, "notification_actions extra not found or is empty");
    Notification notification = builder.build();
    if (intent.hasExtra("isInsistent"))
      notification.flags |= Notification.FLAG_INSISTENT;
    notificationManager.notify(notifyId, notification);
  }
}
