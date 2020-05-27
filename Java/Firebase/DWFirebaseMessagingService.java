package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                  Kastri Free                        *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 *******************************************************/

import android.support.v4.content.LocalBroadcastManager;
import android.content.Context;
import android.content.Intent;
import android.util.Log;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
import com.google.firebase.iid.FirebaseInstanceId;
import com.google.firebase.iid.InstanceIdResult;
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

  public static void queryToken(final Context context) {
    FirebaseInstanceId.getInstance().getInstanceId().addOnSuccessListener(new OnSuccessListener<InstanceIdResult>() {
      @Override
      public void onSuccess(InstanceIdResult instanceIdResult) {
        sendTokenBroadcast(context, instanceIdResult.getToken());
      }
    });
  }

  @Override
  public void onCreate() {
    Log.v(TAG, "onCreate");
  }

  @Override
  public void onNewToken(String token) {
    Log.v(TAG, "onNewToken - " + token);
    sendTokenBroadcast(this, token);
  }

  @Override
  public void onMessageReceived(RemoteMessage remoteMessage) {
    Log.v(TAG, "+onMessageReceived");
    Map<String, String> data = null;
    Intent intent = new Intent(ACTION_MESSAGE_RECEIVED);
    // Process either data section (if present) or notification, but not both
    if (remoteMessage.getData() != null) {
      data = remoteMessage.getData();
      this.addExtras(intent, data);
    } 
    else if (remoteMessage.getNotification() != null) {
      RemoteMessage.Notification notification = remoteMessage.getNotification();
      data = new HashMap<String, String>();
      // data.put("android_channel_id", notification.getChannelId()); 18.0.0
      data.put("body", notification.getBody());
      data.put("color", notification.getColor());
      data.put("icon", notification.getIcon());
      // data.put("image", notification.getImageUrl().getPath()); 18.0.0
      data.put("link", notification.getLink().getPath());
      data.put("sound", notification.getSound());
      // data.put("priority", notification.getNotificationPriority().toString()); 20.0.0
      data.put("title", notification.getTitle());
      // data.put("visibility", notification.getVisibility().toString()); 20.0.0
      this.addExtras(intent, data);
    }
    if (data != null)
      Log.d(TAG, "Message data: " + data); 
    intent.putExtra("gcm.from", remoteMessage.getFrom()); /* String */
    intent.putExtra("gcm.message_id", remoteMessage.getMessageId()); /* String */
    intent.putExtra("gcm.message_type", remoteMessage.getMessageType()); /* String */
    intent.putExtra("gcm.sent_time", remoteMessage.getSentTime()); /* long */
    intent.putExtra("gcm.to", remoteMessage.getTo()); /* String */
    intent.putExtra("gcm.ttl", remoteMessage.getTtl()); /* int */
    boolean hasReceiver = false;
    try {
      hasReceiver = LocalBroadcastManager.getInstance(this).sendBroadcast(intent);
    } catch (Throwable e){
      // No exception handling
    }
    // App is not running if no receiver
    if (!hasReceiver) {
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
    Log.v(TAG, "-onMessageReceived");
  }
}