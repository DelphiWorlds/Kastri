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

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.Service;
import android.content.Intent;
import android.os.Build;
import android.os.IBinder;
import androidx.core.app.NotificationCompat;

public class DWMediaProjectionService extends Service {

  private static final String CHANNEL_ID = "ScreenCapChannel";

  private void createNotificationChannel() {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
      NotificationChannel channel = new NotificationChannel(CHANNEL_ID, "Screen Cap Channel", NotificationManager.IMPORTANCE_DEFAULT);
      channel.setLockscreenVisibility(Notification.VISIBILITY_PRIVATE);
      NotificationManager manager = getSystemService(NotificationManager.class);
      if (manager != null)
          manager.createNotificationChannel(channel);
    }
  }

  @Override
  public void onCreate() {
    super.onCreate();
    createNotificationChannel();
  }

  @Override
  public int onStartCommand(Intent intent, int flags, int startId) {
    Notification notification = new NotificationCompat.Builder(this, CHANNEL_ID)
      .setContentTitle("Screen Capture Service")
      .setContentText("Service is running")
      .setSmallIcon(this.getApplicationInfo().icon)
      .build();
    startForeground(1, notification);
    return START_NOT_STICKY;
  }

  @Override
  public IBinder onBind(Intent intent) {
      return null;
  }
}
