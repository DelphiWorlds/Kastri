# Android Geofencing demo

## Description

Demonstrates the Android Geofence implementation in Kastri. The main files used in the implementation are in the `Features\Geofence` folder and `Java\Geofence` folder

**NOTE**

There are 2 project groups, one named `AGDemoAndroidGroup` and the other `AGDemoAndroidGroupD11`. The former is for Delphi 10.4.2 (and possibly below), and the latter is for Delphi 11 and above **only**. Each group has a corresponding application project (`AGDemo` and `AGDemo11`), however they use the **same** service project (`AGDemoService`). If you build the demo using `AGDemoAndroidGroup`, then later use `AGDemoAndroidGroupD11` you will need first to delete the `.java` files in the `Service` folder

## Configuration

If you are creating your own project:

### Java libraries

Geofencing support relies on:

* Delphi 12.x: 

  `dw-fusedlocation-3.0.0.jar`

  `dw-geofence-3.0.0.jar`

  `dw-kastri-base-3.0.0.jar`

  `gson-2.8.6.jar`

* Delphi 11.x: 

  `dw-fusedlocation.jar`
  
  `dw-geofence-2.0.0.jar`
  
  `dw-kastri-base-2.0.0.jar`
  
  `gson-2.8.6.jar`
 
from the `Lib` folder in Kastri, so add them to the `Libraries` node under the Android 32-bit platform in Project Manager.

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will need to either apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

### Build Event/Android Manifest

**Delphi 12.1:**

Due to changes in the Android build process:

* **Remove** the Build Events in Project Options for Android 32-bit and Android 64-bit 
* Deploy the project *at least once* - this will create `AndroidManifest.template.xml`
* Modify `AndroidManifest.template.xml` to add *after* `<%application-meta-data%>`

  ```
    <meta-data android:name="com.google.android.gms.version" android:value="12451000" />
    <meta-data android:name="com.delphiworlds.kastri.GeofenceIntentReceiver.SERVICE_CLASS_NAME" android:value="com.embarcadero.services.AGDemoService" />
  ```

  and add *after* `<%receivers%>`:

  ```
    <receiver android:name="com.delphiworlds.kastri.GeofenceIntentReceiver" android:enabled="true" android:exported="true" />
  ``` 

**Delphi 12.0 or earlier:**

The demo project has a file called `AndroidManifest.merge.xml`, and there is a `Post-Build` build event in the Project Options (Under Building > Build Events) that uses the `manifestmerge` tool (from the `Tools` folder in Kastri) to merge the entries into the final `AndroidManifest.xml` file.

The above changes adds a metadata entry to indicate the class name of the service to which the geofence transition intents will be sent. This is required for when geofence transitions need to be monitored when the application is in the background, and when the application is not running (see [Monitoring geofence transitions](#monitoring-geofence-transitions), below)

## Background location permissions

In order for monitoring to work when the app is in the background or not running, the application requires background location permissions. This is why the background permission is requested (at runtime) in the demo.

## Monitoring geofence transitions

Geofence transitions can be monitored in the background, or when the app is not running, by implementing code in the `OnHandleIntent` event in the service (AGDemoService in the demo). The demo shows how to obtain what type of transition is occuring (by using the `TPlatformGeofenceManager.GetTransition` method) and which region(s) apply. The region list is a comma separated value, however typically there will be only one region involved. 

Geofence transitions can also be monitored in the application (as opposed to the service), however this will apply only when the application is in the foreground.

## Setting up regions

As per the `AddRegions` method in the `AG.View.Main` unit, regions can be added by calling the `AddRegion` method of the `Regions` property in `TGeofenceManager`. Regions are persisted until they are removed using the `RemoveRegion` method, or the application is uninstalled.

**NOTE: Regions added or removed will not be included in monitoring until monitoring has been restarted, so call the `Stop` method (if not stopped already) before making any changes, then call `Start` once regions have been modified.**

## "Home" region

The first region added in `TMainView.AddRegions` is intended to represent wherever you are located, so if you choose to use it, please replace the latitude and longitude values (of 0, 0) with those for your location.

## Mocking locations

Geofence transitions for the test regions can be tested by using a mock locations application. I recommend using [GPS Emulator](https://play.google.com/store/apps/details?id=com.rosteam.gpsemulator). The free version does not allow you to search on coordinates, however I have provided the actual addresses in the demo code for convenience.

