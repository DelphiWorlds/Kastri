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

// Inspired by:
// https://github.com/android/location-samples/blob/master/LocationUpdates/app/src/main/java/com/google/android/gms/location/sample/locationupdates/MainActivity.java

import android.app.Activity;
import android.content.Context;
import android.content.IntentSender;
import android.location.Location;
import android.location.LocationManager;
import android.os.Looper;
import android.util.Log;
import android.widget.Toast;
import androidx.annotation.NonNull;
import com.google.android.gms.common.api.ApiException;
import com.google.android.gms.common.api.ResolvableApiException;
import com.google.android.gms.location.FusedLocationProviderClient;
import com.google.android.gms.location.LocationCallback;
import com.google.android.gms.location.LocationRequest;
import com.google.android.gms.location.LocationResult;
import com.google.android.gms.location.LocationServices;
import com.google.android.gms.location.LocationSettingsRequest;
import com.google.android.gms.location.LocationSettingsResponse;
import com.google.android.gms.location.LocationSettingsStatusCodes;
import com.google.android.gms.location.SettingsClient;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.OnFailureListener;
import com.google.android.gms.tasks.OnSuccessListener;
import com.google.android.gms.tasks.Task;
import java.util.Date;

public class DWFusedLocationClient {

  private static final int REQUEST_CHECK_SETTINGS = 1;
  private static final String TAG = "DWFusedLocationClient";

  private Context mContext;
  private DWFusedLocationClientDelegate mDelegate;
  private long mFastestInterval = 10000; // Milliseconds
  private long mInterval = 10000; // Milliseconds
  private boolean mIsMockMode = false;
  private DWLocationCallback mLocationCallback;
  private FusedLocationProviderClient mLocationClient;
  private LocationRequest mLocationRequest;
  private int mPriority = LocationRequest.PRIORITY_HIGH_ACCURACY;
  private SettingsClient mSettingsClient;
  private LocationSettingsRequest mSettingsRequest;
  private float mSmallestDisplacement = 0; // Metres

	public DWFusedLocationClient(Context context, DWFusedLocationClientDelegate delegate) {
    mContext = context;
    mDelegate = delegate;
    mLocationClient = LocationServices.getFusedLocationProviderClient(mContext);
    mSettingsClient = LocationServices.getSettingsClient(mContext);
    mLocationCallback = new DWLocationCallback(mDelegate);
  }

  private void createSettingsRequest() {
    mLocationRequest = new LocationRequest();
    mLocationRequest.setFastestInterval(mFastestInterval);
    mLocationRequest.setInterval(mInterval);
    mLocationRequest.setPriority(mPriority);
    mLocationRequest.setSmallestDisplacement(mSmallestDisplacement);
    mSettingsRequest = new LocationSettingsRequest.Builder()
     .addLocationRequest(mLocationRequest)
     .build();
  }

  public long getFastestInterval() {
    return mFastestInterval;
  }

  public void setFastestInterval(long interval) {
    // Sets the fastest rate for active location updates. This interval is exact, and your
    // application will never receive updates faster than this value.
    mFastestInterval = interval;
  }

  public void setSmallestDisplacement(float value) {
    mSmallestDisplacement = value;
  }

  public float getSmallestDisplacement() {
    return mSmallestDisplacement;
  }

  public long getInterval() {
    return mInterval;
  }

  public void setInterval(long interval) {
    // Sets the desired interval for active location updates. This interval is
    // inexact. You may not receive updates at all if no location sources are available, or
    // you may receive them slower than requested. You may also receive updates faster than
    // requested if other applications are requesting location at a faster interval.
    mInterval = interval;
  }

  public boolean getIsMockMode() {
      return mIsMockMode;
  }

  public int getPriority() {
      return mPriority;
  }

  public void setPriority(int priority) {
      mPriority = priority;
  }

  public void requestLastKnownLocation() {
    mLocationClient.getLastLocation().addOnSuccessListener(new OnSuccessListener<Location>() {
      @Override
      public void onSuccess(Location location) {
        // Got last known location. In some rare situations this can be null.
        if (location != null) {
            mDelegate.onLocation(location);
        }
      }
    });
  }

  private void handleTaskException(Exception e, String taskDescription) {
    String errorMessage = null;
    if (e instanceof ApiException) 
      errorMessage = "ApiException code " + Integer.toString(((ApiException) e).getStatusCode());
    else
      errorMessage = e.getMessage();
    Log.e(TAG, "Could not set mock mode - " + errorMessage);
  }

  private void internalSetMockLocation(Location location) {
    mLocationClient.setMockLocation(location).addOnCompleteListener(new OnCompleteListener<Void>() {
      @Override
      public void onComplete(@NonNull Task<Void> task) {
        if (task.isSuccessful()) {
          Log.d(TAG, "Set mock location successful");
          mDelegate.onSetMockLocationResult(location);
        }
        else {
          handleTaskException(task.getException(), "Could not set mock location");
          mDelegate.onSetMockLocationResult(null);
        }
      }
    });    
  }

  public void setMockLocation(double latitude, double longitude) {
    Location location = new Location(LocationManager.NETWORK_PROVIDER);
    location.setLatitude(latitude);
    location.setLongitude(longitude);
    location.setTime(new Date().getTime());
    location.setAccuracy(3.0f);   
    if (!mIsMockMode) {
      internalSetMockMode(true, location);
    } else
      internalSetMockLocation(location);
  }

  private void internalSetMockMode(boolean isMockMode, Location location) {
    mLocationClient.setMockMode(isMockMode).addOnCompleteListener(new OnCompleteListener<Void>() {
      @Override
      public void onComplete(@NonNull Task<Void> task) {
        mIsMockMode = task.isSuccessful();
        mDelegate.onSetMockModeResult(mIsMockMode);
        if (!mIsMockMode) {
          handleTaskException(task.getException(), "Could not set mock mode");
        } else {
          Log.d(TAG, "Set mock mode successful");
          if (location != null)
            internalSetMockLocation(location);
        }
      }
    });
  }

  public void setMockMode(boolean isMockMode) {
    if (!mIsMockMode)
      internalSetMockMode(isMockMode, null);
    else
      mDelegate.onSetMockModeResult(mIsMockMode);
  }

  public void startLocationUpdates() {
    createSettingsRequest();
    // Begin by checking if the device has the necessary location settings.
    Log.d(TAG, "checkLocationSettings");
    mSettingsClient.checkLocationSettings(mSettingsRequest)
      .addOnSuccessListener(new OnSuccessListener<LocationSettingsResponse>() {
        @Override
        public void onSuccess(LocationSettingsResponse locationSettingsResponse) {
          Log.i(TAG, "All location settings are satisfied.");
          //noinspection MissingPermission
          mLocationClient.requestLocationUpdates(mLocationRequest, mLocationCallback, Looper.myLooper());
          Log.i(TAG, "Calling mDelegate.onLocationUpdatesChange");
          mDelegate.onLocationUpdatesChange(true); // i.e. started
        }
      })
      .addOnFailureListener(new OnFailureListener() {
        @Override
        public void onFailure(@NonNull Exception e) {
          int statusCode = ((ApiException) e).getStatusCode();
          switch (statusCode) {
            case LocationSettingsStatusCodes.RESOLUTION_REQUIRED:
// Changed this part to show a toast until a solution about startResolutionForResult from a service is worked out
/*
              Log.i(TAG, "Location settings are not satisfied. Attempting to upgrade location settings ");
              try {
                // Show the dialog by calling startResolutionForResult(), and check the
                // result in onActivityResult().
                ResolvableApiException rae = (ResolvableApiException) e;
                rae.startResolutionForResult((Activity) mContext, REQUEST_CHECK_SETTINGS);
              } catch (IntentSender.SendIntentException sie) {
                Log.i(TAG, "PendingIntent unable to execute request.");
              }
              break;
*/
            case LocationSettingsStatusCodes.SETTINGS_CHANGE_UNAVAILABLE:
              String errorMessage = "Location settings are inadequate, and cannot be fixed here. Please fix in Settings.";
              Log.e(TAG, errorMessage);
              Toast.makeText(mContext, errorMessage, Toast.LENGTH_LONG).show();
              // mRequestingLocationUpdates = false;
              break;
            }
            mDelegate.onLocationUpdatesChange(false);
          }
        });
  }

  public void stopLocationUpdates() {
    mLocationClient.removeLocationUpdates(mLocationCallback)
      .addOnCompleteListener(new OnCompleteListener<Void>() {
        @Override
        public void onComplete(@NonNull Task<Void> task) {
          // mRequestingLocationUpdates = false;
          Log.d(TAG, "removeLocationUpdates onComplete");
          mLocationRequest = null;
          mSettingsRequest = null;
          mDelegate.onLocationUpdatesChange(false); // i.e. stopped
        }
      });
  }

  private class DWLocationCallback extends LocationCallback {

    private DWFusedLocationClientDelegate mDelegate;

    public DWLocationCallback(DWFusedLocationClientDelegate delegate) {
      mDelegate = delegate;
    }

    @Override
    public void onLocationResult(LocationResult locationResult) {
      super.onLocationResult(locationResult);
      // Log.d(TAG, "DWLocationCallback onLocationResult");
      mDelegate.onLocation(locationResult.getLastLocation());
    }

  }
}