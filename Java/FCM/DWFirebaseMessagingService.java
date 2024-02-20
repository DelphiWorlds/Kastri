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

import android.app.ActivityManager;
import android.app.ActivityManager.RunningAppProcessInfo;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.util.Log;
import androidx.core.app.JobIntentService;
import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;

public class DWFirebaseMessagingService extends FirebaseMessagingService {

  private static final String TAG = "DWFirebaseMessagingService";
  private static final String FIREBASE_DEFAULT_CHANNEL_ID = "com.google.firebase.messaging.default_notification_channel_id";
  private static final String FIREBASE_DEFAULT_SMALL_ICON = "com.google.firebase.messaging.default_notification_icon";
  private static final String FIREBASE_RELAY_SERVICE = "com.delphiworlds.kastri.FIREBASE_RELAY_SERVICE";
  private static final String FIREBASE_RELAY_SERVICE_JOB_ID = "com.delphiworlds.kastri.FIREBASE_RELAY_SERVICE_JOB_ID";
  private static Bundle mMetaData = null;
  private static DWFirebaseMessagingServiceCallback mCallback = null;

  public static void setCallback(DWFirebaseMessagingServiceCallback callback) {
    mCallback = callback;
  }

  public void onMessageReceived(RemoteMessage message) {
    Log.d(TAG, "+onMessageReceived");
    String channelId = null;
    String relayService = null;
    int icon = 0;
    int jobID = 0;
    Intent intent = message.toIntent();
    Log.d(TAG, "Intent: " + intent.toUri(0));
    if (mMetaData == null) {
      try {
        mMetaData = getPackageManager().getApplicationInfo(getPackageName(), PackageManager.GET_META_DATA).metaData;
      } catch (PackageManager.NameNotFoundException e) {
        Log.w(TAG, "Unable to load metadata");
      } 
    }
    if (mMetaData != null) {
      Log.d(TAG, "> Got metadata");
      if (mMetaData.containsKey(DWFirebaseMessagingService.FIREBASE_DEFAULT_CHANNEL_ID))
        channelId = mMetaData.getString(DWFirebaseMessagingService.FIREBASE_DEFAULT_CHANNEL_ID); 
      if (mMetaData.containsKey(DWFirebaseMessagingService.FIREBASE_DEFAULT_SMALL_ICON))
        icon = mMetaData.getInt(DWFirebaseMessagingService.FIREBASE_DEFAULT_SMALL_ICON); 
      if (mMetaData.containsKey(DWFirebaseMessagingService.FIREBASE_RELAY_SERVICE)) {
        relayService = mMetaData.getString(DWFirebaseMessagingService.FIREBASE_RELAY_SERVICE);
        if (mMetaData.containsKey(DWFirebaseMessagingService.FIREBASE_RELAY_SERVICE_JOB_ID))
          jobID = mMetaData.getInt(DWFirebaseMessagingService.FIREBASE_RELAY_SERVICE_JOB_ID);
        if (jobID != 0) {
          try {
            Log.d(TAG, "> Enqueuing work for: " + relayService);
            JobIntentService.enqueueWork(this, Class.forName(relayService), jobID, intent);
          } catch (ClassNotFoundException e) {
            Log.e(TAG, "Could not find service: " + relayService);
          } catch (Exception e) {
            Log.e(TAG, "Unhandled exception: " + e.getMessage());
          }
        } 
        else
          Log.e(TAG, "No job id specified for " + relayService);
      }
    }
    RunningAppProcessInfo info = new RunningAppProcessInfo();
    ActivityManager.getMyMemoryState(info);
    boolean isAppForeground = info.importance == RunningAppProcessInfo.IMPORTANCE_FOREGROUND;
    // If no callback, or app is foreground and needs the notification, present it
    if ((mCallback == null) || (mCallback.getShowNotificationWhenForeground() && isAppForeground))
      DWNotificationPresenter.presentNotification(this, intent, channelId, icon);
    if (mCallback != null) {
      Log.d(TAG, "> mCallback.onNotificationReceived");
      mCallback.onNotificationReceived(intent);
    }
    Log.d(TAG, "-onMessageReceived");  
  }
}