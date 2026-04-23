# Firebase Crashlytics demo

## Description

Demonstrates the Firebase Crashytics features implemented in Kastri.

## IMPORTANT

Please read the Android Notes sections below - they are important for configuration, especially if you are creating your own project and wish to include the Firebase Crashytics implementation

## Firebase console setup

You will need to create a project in [Firebase Console](https://console.firebase.google.com/), and add the platforms to the Firebase project you intend to support. 

### Tutorial video

[This video](https://www.youtube.com/watch?v=dRYnm_k3w1w) is a guide for creating/managing Firebase projects. Here are the important sections:

* 2m 31s - Creating the project
* 5m 07s - Analytics is discussed, which is **enabled by default**
* 5m 58s - Adding Android to the Firebase project
* 7m 20s - Since we're using Delphi, just save the `google-services.json` file somewhere convenient, like the root folder of your Delphi project source

The rest of the video can be ignored, since it deals specifically with Android Studio.

## Delphi Project setup

### Application identifiers

In Project Options of the Delphi project, select the Version Info section, and select the applicable target. For Android, update the `Package` property to match the package name specified for Android in the Firebase project.

### Android

#### Import the `google-services.json` file

In Project Options of the Delphi project, select the Services section and select Android as the target. Click the `Import` button and select the `google-services.json` file that was saved earlier. Do this step for Android 32 bit and Android 64 bit if you are targeting both.

#### Deployment

In Deployment of the Delphi project include `Resources/Values/stringsCrashlytics.xml` resource. Set remote_path to `res\values\`

String resource `com.crashlytics.android.build_id`(value not important) has to be included in the resource file, because build_id is generated when you build app with a gradle.
Delphi do not use gradle, so we don't need uploading mapping file nedeed for R8 obfuscation pipeline, but we have to put some value for build_id:
<string name="com.crashlytics.android.build_id">some value</string>

For example: <string name="com.crashlytics.android.build_id">com.delphi.crash.firebase.build.id</string>

#### Entitlements

In the EntitlementList section of Project Options, select the `Receive push notifications` option. This may appear unusual, however it is necessary for the required entries to appear the manifest for Android.

## Android Notes

Official Firebase Analytics support for Android is yet to be added to Delphi (though the foundations are present in Delphi 11), so a couple of "tweaks" are required to make it work.

#### Build Event/Android Manifest

**Delphi 12.1 ONLY**

Please see [this link](../../Delphi12.1.AndroidManifestIssue.md).

**Delphi 12.0 or earlier:**

Configure Build Events in Project Options to add a Post-Build event with the command:  

```
  [kastri]\Tools\manifestmerge AndroidManifest.merge.xml $(Platform)\$(Config)\AndroidManifest.xml
```  
Where `[kastri]` is the path to the Kastri library. Do this for each required Android platform target (i.e. 32-bit and/or 64-bit)

`AndroidManifest.merge.xml` can be found in the root folder of the respective demo (i.e. FCMBaseDemo and FCMRelayDemo), and should be copied to the root folder of your project

### Additional jar files

There are 3 additional jar files added to the project, which are necessary for Firebase Crashlytics to work. Jar files locations are in project `lib` and `\Kastri\ThirdParty\Firebase\Android` folders and the demo has been configured to use them,
however when creating your own project you will need to add them to the Libraries node of the Android target in Project Manager. **The rest of the details here are for information only.**

The following 2 were obtained from the [Maven Repository](https://mvnrepository.com/):

* `firebase-crashlytics-18.2.4.jar`
* `play-services-measurement-api.20.1.2.jar`

The third jar file, `FirebaseCrashlyticsDemoD12.R.jar` is compiled from files in Android\Debug\FirebaseCrashlyticsDemoD12\res folder.

### Usage

To see Crashlytics events in the Firebase console, you need to restart application after simulating crash. (Only `Native Runtime crash` is visible in console immediately).

### iOS

### Not yet implemented


