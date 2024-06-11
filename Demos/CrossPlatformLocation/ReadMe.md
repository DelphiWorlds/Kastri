# Cross Platform Location demo

## Description

This demo is intended to provide a means of having location updates occur whether the app is not running, or is running in the background, or foreground

The demo has been tested using Delphi 12.x and 11.x (10.4.2 is now considered legacy)

In the demo, location information should appear in the memo, as location updates occur

## Android 

### Java libraries

The demo has been updated from earlier versions to include support for the [Fused Location API](https://developers.google.com/location-context/fused-location-provider). Previously, it used regular location providers, however it has been found that using these can be unreliable especially when the app is not running (the service handles location updates), or when the device is in "doze" mode.

In order to use the Fused Location API, it was necessary to write some Java code, and you will find 2 jar files have been added to the project:

* `dw-kastri-base-3.0.0.jar` (or `dw-kastri-base-2.0.0.jar` for 11.x), located in the `Lib` folder of Kastri (this is also in the original project), for doze alarm and start at boot support
* `dw-fusedlocation-3.0.0.jar` (or `dw-fusedlocation-2.0.0.jar` for 11.x), also located in the `Lib` folder, for Fused Location API support

If you are creating a new project (i.e. other than the demo) you will need to add these jars to the `Libraries` node under the Android platform in Project Manager, for the support that the respective jars provide

### Building the application

When building the application, please ensure that the Target Platform for **both the application and service match**, i.e. if you have the application set to Android 32-bit, ensure that the service is also Android 32-bit. The same applies to Android 64-bit.

### Build Event/Android Manifest

**Delphi 12.1 ONLY**

Please see [this link](../../Delphi12.1.AndroidManifestIssue.md).

**Delphi 12.0 and 11.x**

When configuring your **own** project for Android, configure Build Events in Project Options to add a Post-Build event with the command:  

```
[kastri]\Tools\manifestmerge AndroidManifest.merge.xml $(Platform)\$(Config)\AndroidManifest.xml
```  
Where `[kastri]` is the path to the Kastri library. Do this for each required Android platform target (i.e. 32-bit and/or 64-bit)

### Permissions

Please ensure that the following permissions are checked in the Project Options:

* Foreground service
* Foreground servuce location (Delphi 12.x)
* Access background location (Dangerous permissions, Delphi 11.x and up)
* Access coarse location (Dangerous permissions)
* Access fine location (Dangerous permissions)

## iOS

On iOS, the main thing to be aware of is that the `UIBackgroundModes` values in the Version Info of the Project Options needs to include `location`
  
**Note:** For Delphi 10.4 and 10.4.1 users, if you wish to use location services on iOS, **you will need to [patch the System.iOS.Sensors unit as per this report](https://quality.embarcadero.com/browse/RSP-29859?focusedCommentId=85109&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-85109)**, in order for it to work properly. This has been fixed in Delphi 10.4.2, and highlights why **developers should stay current with the version of Delphi**

## Using the demo code in your own project

When you are creating your own project:

### Android

Please pay attention to the jar and manifest changes as described above, as well as ensuring that the Foreground service and Receive boot completed permissions are enabled in the Project Options