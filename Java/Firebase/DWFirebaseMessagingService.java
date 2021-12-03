package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                  Kastri Free                        *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 *******************************************************/

import android.content.Context;
import android.content.Intent;
// Delphi 10.4.2 or earlier
// import android.support.v4.content.LocalBroadcastManager;

import android.util.Log;

// Delphi 11 and later
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

// Delphi 10.4.2 or earlier - begin
/*
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
import com.google.firebase.iid.FirebaseInstanceId;
import com.google.firebase.iid.InstanceIdResult;
*/
// Delphi 10.4.2 or earlier - end

import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;
import java.util.HashMap;
import java.util.Map;

public class DWFirebaseMessagingService extends FirebaseMessagingService {

  private static final String TAG = "DWFirebaseMessagingService";
  private static int mUniqueId = 0;
  public static final String ACTION_NEW_TOKEN = "com.delphiworlds.kastri.DWFirebaseMessagingService.ACTION_NEW_TOKEN";
  public static final String ACTION_MESSAGE_RECEIVED = "com.delphiworlds.kastri.DWFirebaseMessagingService.ACTION_MESSAGE_RECEIVED";

  private void addExtras(Intent intent, Map<String, String> data) {
    for (Map.Entry<String, String> entry : data.entrySet()) {
      if (entry.getValue() != null)
        intent.putExtra(entry.getKey(), entry.getValue());
    }
  }

  private static String priorityToString(int priority) {
    if (priority == -1) 
      return "low";
    else if (priority == 1)
      return "high";
    else if (priority == 2)
      return "max";
    else  
      return "none";
  }

  public static void sendTokenBroadcast(Context context, String token) {
    Intent intent = new Intent(ACTION_NEW_TOKEN);
    intent.putExtra("token", token);
    try {
      Log.v(TAG, "Sending token broadcast");
      LocalBroadcastManager.getInstance(context).sendBroadcast(intent);
    } catch (Throwable e) {
      // no exception handling
    }  
  }

  // Delphi 10.4.2 and earlier - begin
  /*
  public static void queryToken(final Context context) {
    FirebaseInstanceId.getInstance().getInstanceId().addOnSuccessListener(new OnSuccessListener<InstanceIdResult>() {
      @Override
      public void onSuccess(InstanceIdResult instanceIdResult) {
        sendTokenBroadcast(context, instanceIdResult.getToken());
      }
    });
  }
  */
  // Delphi 10.4.2 and earlier - end

  @Override
  public void onCreate() {
    // Log.d(TAG, "onCreate");
  }

  @Override
  public void onNewToken(String token) {
    // Log.d(TAG, "onNewToken - " + token);
    sendTokenBroadcast(this, token);
  }

  @Override
  public void onMessageReceived(RemoteMessage remoteMessage) {
    Map<String, String> data = null;
    Intent intent = new Intent(ACTION_MESSAGE_RECEIVED);
    // Process either data section (if present) or notification, but not both
    if ((remoteMessage.getData() != null) && !remoteMessage.getData().isEmpty()) {
      Log.d(TAG, "Message has data: " + data); 
      data = remoteMessage.getData();
      this.addExtras(intent, data);
    } 
    else if (remoteMessage.getNotification() != null) {
      Log.d(TAG, "Message has notification");
      RemoteMessage.Notification notification = remoteMessage.getNotification();
      data = new HashMap<String, String>();
      // data.put("android_channel_id", notification.getChannelId()); 18.0.0
      data.put("body", notification.getBody());
      data.put("color", notification.getColor());
      data.put("icon", notification.getIcon());
      if (notification.getLink() != null)
        data.put("link", notification.getLink().toString());
      data.put("sound", notification.getSound());
      data.put("title", notification.getTitle());
      // Delphi 11 and later - begin
      if (notification.getImageUrl() != null)
        data.put("image", notification.getImageUrl().toString());
      if (notification.getNotificationPriority() != null)
        data.put("priority", notification.getNotificationPriority().toString());
      if (notification.getVisibility() != null)
        data.put("visibility", notification.getVisibility().toString());
      // Delphi 11 and later - end
      this.addExtras(intent, data);
    }
    else
      Log.d(TAG, "Message has either no data or is empty, and no notification");
    intent.putExtra("google.from", remoteMessage.getFrom());
    intent.putExtra("google.message_id", remoteMessage.getMessageId());
    intent.putExtra("google.message_type", remoteMessage.getMessageType());
    intent.putExtra("google.sent_time", remoteMessage.getSentTime());
    intent.putExtra("google.to", remoteMessage.getTo());
    intent.putExtra("google.ttl", remoteMessage.getTtl());
    intent.putExtra("google.original_priority", DWFirebaseMessagingService.priorityToString(remoteMessage.getOriginalPriority()));
    intent.putExtra("google.delivered_priority", DWFirebaseMessagingService.priorityToString(remoteMessage.getPriority()));
    boolean hasReceiver = false;
    try {
      hasReceiver = LocalBroadcastManager.getInstance(this).sendBroadcast(intent);
    } catch (Throwable e) {
      // No exception handling
    }
    // App is not running if no receiver
    if (!hasReceiver) {
      Log.d(TAG, "App not running - posting notification");
      DWNotificationPublisher.sendNotification(this, intent, true);
      // Launch the application if launchapp is 1
      if (intent.hasExtra("launchapp")) {
        String launchapp = intent.getStringExtra("launchapp");
        // Log.v(TAG, "Payload has launchapp flag set to: " + launchapp);
        if (launchapp.equals("1")) {
          // Log.v(TAG, "Launching..");
          this.startActivity(this.getPackageManager().getLaunchIntentForPackage(this.getPackageName()));
        }
      }
    }
  }
}