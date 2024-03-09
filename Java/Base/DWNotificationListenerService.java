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
 
import android.content.Intent;
import android.service.notification.NotificationListenerService;
import android.service.notification.StatusBarNotification;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

public class DWNotificationListenerService extends NotificationListenerService {

  private static final String TAG = "DWNotificationListenerService";

  public static final String ACTION_NOTIFICATION_POSTED = "ACTION_NOTIFICATION_POSTED";
  public static final String ACTION_NOTIFICATION_REMOVED = "ACTION_NOTIFICATION_REMOVED";
  public static final String EXTRA_NOTIFICATION = "EXTRA_NOTIFICATION";

  @Override
  public void onListenerConnected() {
    
  }

  @Override
  public void onListenerDisconnected() {
    
  }

  public void sendNotification(String action, StatusBarNotification sbn) {
    Intent intent = new Intent(action);
    intent.putExtra(DWNotificationListenerService.EXTRA_NOTIFICATION, sbn.getNotification().extras);
    LocalBroadcastManager.getInstance(getApplicationContext()).sendBroadcast(intent);
  }

  @Override
  public void onNotificationPosted(StatusBarNotification sbn) {
    sendNotification(DWNotificationListenerService.ACTION_NOTIFICATION_POSTED, sbn);
  }

  @Override
  public void onNotificationRemoved(StatusBarNotification sbn) {
    sendNotification(DWNotificationListenerService.ACTION_NOTIFICATION_REMOVED, sbn);
  }

}