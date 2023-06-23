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

// Based on: https://github.com/android/location-samples/blob/master/Geofencing/app/src/main/java/com/google/android/gms/location/sample/geofencing/MainActivity.java
// Manifest: https://github.com/android/location-samples/blob/master/Geofencing/app/src/main/AndroidManifest.xml

import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.SharedPreferences;
import android.Manifest;
import android.util.Log;

// androidx-annotation.jar
import androidx.annotation.NonNull;
// Delphi 10.4.2
// import android.support.v4.app.ActivityCompat;
// Delphi 11
import androidx.core.app.ActivityCompat;

import com.google.android.gms.common.api.ApiException;

// play-services-location.jar  (17.0.0 may be compatible with 10.4.1)
import com.google.android.gms.location.Geofence;
import com.google.android.gms.location.GeofencingClient;
import com.google.android.gms.location.GeofencingRequest;
import com.google.android.gms.location.LocationServices;

import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.Task;

import java.util.ArrayList;
import java.util.Map;

public class GeofenceManager implements OnCompleteListener<Void> {

  private static final String TAG = "GeofenceManager";

  private class PendingGeofenceTask {
    static final int NONE = 0, ADD = 1, REMOVE = 2;
  }

  private class GeofenceTaskResult {
    static final int SUCCESS = 0, TASK_ERROR = 1, NO_REGIONS_ERROR = 2, NO_PERMISSIONS_ERROR = 3, NO_MONITORING_ERROR = 4, ALREADY_MONITORING_ERROR = 5;
  }

  private GeofencingClient mClient;
  private Context mContext;
  private GeofenceManagerDelegate mDelegate;
  private int mPendingGeofenceTask = PendingGeofenceTask.NONE;
  private PendingIntent mPendingIntent;
  private double mRadius;
  private GeofenceRegions mRegions;

  private ArrayList<Geofence> getGeofences() {
    ArrayList<Geofence> geofences = new ArrayList<>();
    for (Map.Entry<String, GeofenceRegions.Region> entry : mRegions.getItems().entrySet()) {
      GeofenceRegions.Region region = entry.getValue();
      geofences.add(new Geofence.Builder()
        // Set the request ID of the geofence. This is a string to identify this geofence.
        .setRequestId(entry.getKey())
        // Set the circular region of this geofence.
        .setCircularRegion(region.getCoords().latitude, region.getCoords().longitude, (float) region.getRadius())
        // Set the expiration duration of the geofence. This geofence gets automatically removed after this period of time.
        .setExpirationDuration(-1)  // Never expire
        // Set the transition types of interest. Alerts are only generated for these transition.
        .setTransitionTypes(Geofence.GEOFENCE_TRANSITION_ENTER | Geofence.GEOFENCE_TRANSITION_EXIT)
        // Create the geofence.
        .build());
    }
    return geofences;
  }

  private GeofencingRequest getGeofencingRequest() {
    GeofencingRequest.Builder builder = new GeofencingRequest.Builder();
    // The INITIAL_TRIGGER_ENTER flag indicates that geofencing service should trigger a
    // GEOFENCE_TRANSITION_ENTER notification when the geofence is added and if the device
    // is already inside that geofence.
    builder.setInitialTrigger(GeofencingRequest.INITIAL_TRIGGER_ENTER);
    // Add the geofences to be monitored by geofencing service.
    builder.addGeofences(getGeofences());
    // Return a GeofencingRequest.
    return builder.build();
  }

  private boolean hasPermissions() {
    int permissionState = ActivityCompat.checkSelfPermission(mContext, Manifest.permission.ACCESS_FINE_LOCATION);
    return permissionState == PackageManager.PERMISSION_GRANTED;
  }

  private String getStringProperty(String key) {
    SharedPreferences pref = mContext.getSharedPreferences("Geofence", Context.MODE_PRIVATE);
    return pref.getString(key, "");
  }

  private void setStringProperty(String key, String value) {
    SharedPreferences pref = mContext.getSharedPreferences("Geofence", Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = pref.edit();
    editor.putString(key, value);
    editor.commit();
  }

  private void setBooleanProperty(String key, boolean value) {
    SharedPreferences pref = mContext.getSharedPreferences("Geofence", Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = pref.edit();
    editor.putBoolean(key, value);
    editor.commit();
  }

  private void setIntProperty(String key, int value) {
    SharedPreferences pref = mContext.getSharedPreferences("Geofence", Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = pref.edit();
    editor.putInt(key, value);
    editor.commit();
  }

  private void setIsMonitoring(boolean value) {
    setBooleanProperty("IsMonitoring", value);
  }

  public GeofenceManager(Context context, GeofenceManagerDelegate delegate) {
    mContext = context;
    mDelegate = delegate;
    mRadius = 25; // metres
    mClient = LocationServices.getGeofencingClient(mContext);
    mRegions = GeofenceRegions.getInstance(mContext);
    Intent intent = new Intent(mContext, GeofenceIntentReceiver.class);
    // intent.setAction(GeofenceIntentReceiver.ACTION_RECEIVE_GEOFENCE);
    mPendingIntent = PendingIntent.getBroadcast(mContext, 0, intent, PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_MUTABLE);
  }

  // OnCompleteListener
  @Override
  public void onComplete(@NonNull Task<Void> task) {
    boolean success = task.isSuccessful();
    int taskResult = 0;
    if (success && (mPendingGeofenceTask == PendingGeofenceTask.ADD))
      setIsMonitoring(true);
    else if (success && (mPendingGeofenceTask == PendingGeofenceTask.REMOVE))
      setIsMonitoring(false);
    String errorMessage = null;
    if (!success) {
      Exception e = task.getException();
      if (e instanceof ApiException)
        errorMessage = "ApiException code " + Integer.toString(((ApiException) e).getStatusCode());
      else
        errorMessage = e.getMessage();
      taskResult = GeofenceTaskResult.TASK_ERROR;
    }
    if (errorMessage != null)
      Log.w(TAG, "onComplete - Error: " + errorMessage);
    else
      errorMessage = "";
    int currentTask = mPendingGeofenceTask;
    mPendingGeofenceTask = PendingGeofenceTask.NONE;
    mDelegate.onGeofenceActionComplete(currentTask, taskResult, errorMessage);
  }

  public boolean getIsMonitoring() {
    SharedPreferences pref = mContext.getSharedPreferences("Geofence", Context.MODE_PRIVATE);
    return pref.getBoolean("IsMonitoring", false);
  }

  public GeofenceRegions getRegions() {
    return mRegions;
  }

  public double getRadius() {
    return mRadius;
  }

  public void setRadius(double value) {
    mRadius = value;
  }

  public void start() {
    if (!getIsMonitoring()) {
      mRegions.load();
      if (mRegions.getItems().isEmpty())
        mDelegate.onGeofenceActionComplete(PendingGeofenceTask.ADD, GeofenceTaskResult.NO_REGIONS_ERROR, "Please add regions before calling start");
      if (!hasPermissions()) {
        mDelegate.onGeofenceActionComplete(PendingGeofenceTask.ADD, GeofenceTaskResult.NO_PERMISSIONS_ERROR, "Requires ACCESS_FINE_LOCATION permission");
        return;
      }
      mPendingGeofenceTask = PendingGeofenceTask.ADD;
      mClient.addGeofences(getGeofencingRequest(), mPendingIntent).addOnCompleteListener(this);
    }
    else
      mDelegate.onGeofenceActionComplete(PendingGeofenceTask.ADD, GeofenceTaskResult.ALREADY_MONITORING_ERROR, "Already monitoring regions");
  }

  public void stop() {
    if (getIsMonitoring()) {
      mPendingGeofenceTask = PendingGeofenceTask.REMOVE;
      mClient.removeGeofences(mPendingIntent).addOnCompleteListener(this);
    }
    else
      mDelegate.onGeofenceActionComplete(PendingGeofenceTask.REMOVE, GeofenceTaskResult.NO_MONITORING_ERROR, "No geofences being monitored");
  }
}