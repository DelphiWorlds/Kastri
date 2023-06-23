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

// Some code here based on: 
//   https://github.com/android/location-samples/blob/master/Geofencing/app/src/main/java/com/google/android/gms/location/sample/geofencing/GeofenceTransitionsJobIntentService.java

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.SharedPreferences;
import android.location.Location;
import android.os.Bundle;

// Delphi 10.4.2
// import android.support.v4.app.JobIntentService;
// import android.support.v4.content.LocalBroadcastManager;

import android.text.TextUtils;
import android.util.Log;

// Delphi 11
import androidx.core.app.JobIntentService;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

// play-services-location.jar  (17.0.0 may be compatible with Delphi 10.4.x)
import com.google.android.gms.location.Geofence;
import com.google.android.gms.location.GeofencingEvent;
import com.google.android.gms.location.GeofenceStatusCodes;

import java.util.ArrayList;
import java.util.List;

/**
 * Receiver for geofence transition INTENTS. Registered as the receiver in the constructor of GeofenceManager 
 *   NOTE: This is not the receiver that you modify for your own purposes. Please use GeofenceBroadcastReceiver for that
 * <p>
 * Receives geofence transition events from Location Services in the form of an Intent containing
 * the transition type and geofence id(s) that triggered the transition. Creates a JobIntentService descendant
 * that will handle the intent in the background.
 */
public class GeofenceIntentReceiver extends BroadcastReceiver {

  private static final String TAG = "GeofenceIntentReceiver";
  private static final int DELPHI_SERVICE_JOB_ID = 535;  // Just a random number
  private GeofenceManager mGeofenceManager;
  public static final String ACTION_GEOFENCE_TRANSITION = "com.delphiworlds.kastri.GeofenceIntentReceiver.ACTION_GEOFENCE_TRANSITION";
  public static final String EXTRA_TRANSITION_IDS = "com.delphiworlds.kastri.GeofenceIntentReceiver.EXTRA_TRANSITION_IDS";
  public static final String EXTRA_TRANSITION_TYPE = "com.delphiworlds.kastri.GeofenceIntentReceiver.EXTRA_TRANSITION_TYPE";
  public static final String EXTRA_TRANSITION_LATITUDE = "com.delphiworlds.kastri.GeofenceIntentReceiver.EXTRA_TRANSITION_LATITUDE";
  public static final String EXTRA_TRANSITION_LONGITUDE = "com.delphiworlds.kastri.GeofenceIntentReceiver.EXTRA_TRANSITION_LONGITUDE";
  public static final String SERVICE_CLASS_NAME = "com.delphiworlds.kastri.GeofenceIntentReceiver.SERVICE_CLASS_NAME";

  private static String getErrorString(int errorCode) {
    switch (errorCode) {
      case GeofenceStatusCodes.GEOFENCE_NOT_AVAILABLE:
        return "Geofence not available";  // mResources.getString(R.string.geofence_not_available);
      case GeofenceStatusCodes.GEOFENCE_TOO_MANY_GEOFENCES:
        return "Too many geofences"; // mResources.getString(R.string.geofence_too_many_geofences);
      case GeofenceStatusCodes.GEOFENCE_TOO_MANY_PENDING_INTENTS:
        return "Too many pending intents"; // mResources.getString(R.string.geofence_too_many_pending_intents);
      default:
        return "Unknown geofence error"; // mResources.getString(R.string.unknown_geofence_error);
    }
  }

  private String getServiceClassName(Context context) {
    String serviceClassName = null;
    try {
      Bundle metaData = context.getPackageManager().getApplicationInfo(context.getPackageName(), PackageManager.GET_META_DATA).metaData;
      if (metaData.containsKey(SERVICE_CLASS_NAME))
        serviceClassName = metaData.getString(SERVICE_CLASS_NAME);
    } catch (PackageManager.NameNotFoundException exception) {
      // Do nothing
    }
    return serviceClassName;      
  }

  private void enqueueTransition(Context context, Intent intent) {
    String serviceClassName = getServiceClassName(context);
    if (serviceClassName != null) {
      try {
        JobIntentService.enqueueWork(context, Class.forName(serviceClassName), DELPHI_SERVICE_JOB_ID, intent);
      } catch (ClassNotFoundException e) {
        Log.e(TAG, "Could not find service: " + serviceClassName);
      }
    }
  } 

  /**
   * Receives incoming intents.
   *
   * @param context the application context.
   * @param intent  sent by Location Services. This Intent is provided to Location
   *                Services (inside a PendingIntent) when addGeofences() is called.
   */
  @Override
  public void onReceive(Context context, Intent intent) {
    GeofencingEvent geofencingEvent = GeofencingEvent.fromIntent(intent);
    if (geofencingEvent.hasError()) {
      String errorMessage = getErrorString(geofencingEvent.getErrorCode());
      Log.e(TAG, errorMessage);
      return;
    }
    // Get the transition type.
    int geofenceTransition = geofencingEvent.getGeofenceTransition();
    Location location = geofencingEvent.getTriggeringLocation();
    // Test that the reported transition was of interest.
    if ((geofenceTransition == Geofence.GEOFENCE_TRANSITION_ENTER) || (geofenceTransition == Geofence.GEOFENCE_TRANSITION_EXIT)) {
      if (mGeofenceManager == null)
        mGeofenceManager = new GeofenceManager(context, null);
      GeofenceRegions regions = mGeofenceManager.getRegions();
      regions.load();
      // Get the geofences that were triggered. A single event can trigger multiple geofences.
      List<Geofence> geofences = geofencingEvent.getTriggeringGeofences();
      ArrayList<String> idsList = new ArrayList<>();
      for (Geofence geofence : geofences) {
        GeofenceRegions.Region region = regions.get(geofence.getRequestId());
        if ((region != null) && ((region.getTransitionTypes() & geofenceTransition) > 0))
          idsList.add(geofence.getRequestId()); // same as id in regions
      }
      String ids = TextUtils.join(", ", idsList);
      Log.i(TAG, "Transition type: " + Integer.toString(geofenceTransition) + " for: " + ids);
      Intent transitionIntent = new Intent(GeofenceIntentReceiver.ACTION_GEOFENCE_TRANSITION);
      transitionIntent.putExtra(GeofenceIntentReceiver.EXTRA_TRANSITION_TYPE, geofenceTransition);
      transitionIntent.putExtra(GeofenceIntentReceiver.EXTRA_TRANSITION_IDS, ids);
      transitionIntent.putExtra(GeofenceIntentReceiver.EXTRA_TRANSITION_LATITUDE, location.getLatitude());
      transitionIntent.putExtra(GeofenceIntentReceiver.EXTRA_TRANSITION_LONGITUDE, location.getLongitude());
      // A broadcast that can be received by the application, either just for info, or if the transitions are handled by the app
      LocalBroadcastManager.getInstance(context).sendBroadcast(transitionIntent);
      enqueueTransition(context, transitionIntent);
    } else {
      // Log the error.
      Log.e(TAG, "Unknown geofence transition type: " + String.valueOf(geofenceTransition));
    }
    // Enqueues a JobIntentService descendant passing the context and intent as parameters
    // GeofenceTransitionsService.enqueueWork(context, intent);
  }
}