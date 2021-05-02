# Cross Platform Location demo

## Description

This demo is intended to provide a means of having location updates occur whether the app is not running, or is running in the background, or foreground

The demo has been tested using Delphi 10.4.2, however it may work on earlier versions.

In the demo, location information should appear in the memo, as location updates occur

## Android 

The demo has been updated from earlier versions to include support for the [Fused Location API](https://developers.google.com/location-context/fused-location-provider). Previously, it used regular location providers, however it has been found that using these can be unreliable especially when the app is not running (the service handles location updates), or when the device is in "doze" mode.

In order to use the Fused Location API, it was necessary to write some Java code, and you will find 3 jar files have been added to the project:

* dw-kastri-base.jar, located in the `Lib` folder of Kastri (this is also in the original project), for doze alarm and start at boot support
* dw-fusedlocation.jar, also located in the `Lib` folder, for Fused Location API support
* play-services-location.16.0.0.jar, located in the `ThirdParty\Android` folder, also for Fused Location API support

If you are creating a new project (i.e. other than the demo) you will need to add these jars to the `Libraries` node under the Android platform in Project Manager, for the support that the respective jars provide

You will also need to modify AndroidManifest.template.xml as per the modifications (which are commented) in the same file in the Application project in the demo

## iOS

On iOS, the main thing to be aware of is that the `UIBackgroundModes` values in the Version Info of the Project Options needs to include `location`
  
**Note:** For Delphi 10.4 and 10.4.1 users, if you wish to use location services on iOS, **you will need to [patch the System.iOS.Sensors unit as per this report](https://quality.embarcadero.com/browse/RSP-29859?focusedCommentId=85109&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-85109)**, in order for it to work properly. This has been fixed in Delphi 10.4.2, and highlights why **developers should stay current with the version of Delphi**