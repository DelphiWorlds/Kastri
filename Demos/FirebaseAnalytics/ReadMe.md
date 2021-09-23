# Firebase Analytics demo

## Description

Demonstrates the Firebase Analytics features implemented in Kastri.

**NOTE**: **The Android implementation will work only in Delphi 11 or higher.** The technical requirements for supporting Delphi 10.4.2 or lower are quite substantial. 

## IMPORTANT

Please read the Android Notes and iOS Notes sections below - they are important for configuration, especially if you are creating your own project and wish to include the Firebase Analytics implementation

## Firebase console setup

You will need to create a project in [Firebase Console](https://console.firebase.google.com/), and add the platforms to the Firebase project you intend to support. 

### Tutorial video

[This video](https://www.youtube.com/watch?v=dRYnm_k3w1w) is a guide for creating/managing Firebase projects. Here are the important sections:

* 2m 31s - Creating the project
* 5m 07s - Analytics is discussed, which is **enabled by default**
* 5m 58s - Adding Android to the Firebase project
* 7m 20s - Since we're using Delphi, just save the `google-services.json` file somewhere convenient, like the root folder of your Delphi project source

The rest of the video can be ignored, since it deals specifically with Android Studio, however if you're going to support Firebase Analytics in iOS, you'll need to add iOS to the Firebase project, which is similar to the steps for adding Android. In this instance, you will need to save the `GoogleService-Info.plist` file. In the demo, Deployment Manager is configured to expect this file in the root folder of the Delphi project.

## Delphi Project setup

### Application identifiers

In Project Options of the Delphi project, select the Version Info section, and select the applicable target. For Android, update the `Package` property to match the package name specified for Android in the Firebase project. For iOS, update the `CFBundleIdentifier` value to match the App ID specified for iOS in the Firebase project.

### Android configuration

#### Import the `google-services.json` file

In Project Options of the Delphi project, select the Services section and select Android as the target. Click the `Import` button and select the `google-services.json` file that was saved earlier. Do this step for Android 32 bit and Android 64 bit if you are targeting both.

#### Entitlements

In the EntitlementList section of Project Options, select the `Receive push notifications` option. This may appear unusual, however it is necessary for the required entries to appear the manifest for Android.

## Android Notes

Official Firebase Analytics support for Android is yet to be added to Delphi (though the foundations are present in Delphi 11), so a couple of "tweaks" are required to make it work.

### Merging of the manifest 

A Post-Build build event for the Android target has been added to the demo project. This event uses the `manifestmerge` command line tool to merge the entries in the `AndroidManifest.merge.xml` file that is included with the demo, in order to add entries required for Firebase Analytics.

### Additional jar files

There are 4 additional jar files added to the project, which are necessary for Firebase Analytics to work. They have already been added to the Kastri repo and the demo has been configured to use them, however when creating your own project you will need to add them to the Libraries node of the Android target in Project Manager. **The rest of the details here are for information only.**

The following 3 were obtained from the [Maven Repository](https://mvnrepository.com/):

* `firebase-iid-21.1.0.jar`
* `play-services-measurement-api.18.0.0.jar`
* `play-services-measurement-impl.18.0.0.jar`

The other file, `play-services-basement-17.6.0.R.jar` is compiled from files in the Android Archive file (`aar`) located [here](https://mvnrepository.com/artifact/com.google.android.gms/play-services-basement/17.6.0)

## iOS Notes

### Firebase iOS SDK

The default Firebase SDK used is 8.7.0, which you can download from [here](https://github.com/firebase/firebase-ios-sdk/releases/download/8.7.0/Firebase.zip). 

You will need to either go to Tools|Options, IDE > Environment Variables, and create a User Override called Firebase_8_7 that points to the root of the Firebase SDK, or modify the Framework Search Path value in the Project Options so that the folders match the Firebase SDK you wish to compile against, if it is compatible (known to be compatible with version 8.2.0 - 8.7.0).

### Linker option

As is the case for other Firebase projects that target iOS, in the `Linker` options of the Delphi project, the `Options passed to the LD linker` option has a value of `-ObjC`

### GoogleService-info.plist

As per the notes above, in the demo **this file needs to be placed in the root folder of the project.**
