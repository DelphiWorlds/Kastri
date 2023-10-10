# App Update Demo

## Description

Demonstrates implementation of [in-app updating of Play Store apps on Android](https://developer.android.com/guide/playcore/in-app-updates). As such, this demo really only applies to Android.

Please refer to the demo as a guide on how to use `TAppUpdate`.

* `CheckForUpdate` is an **asynchronous** call to check if an update is available, and returns the info in the `OnAppUpdateInfo` event
* `StartUpdate` is an **asynchronous** call to start the update flow for the specified type. The `OnAppUpdateStartedFlow` event is called with the `Started` parameter indicating whether or not the flow did actually start. The `OnAppUpdateResult` event is called when the flow finishes, and the `UpdateResult` parameter indicates whether or not the update succeeded, or the user canceled.

## Supported Delphi versions

Delphi 12, Delphi 11.x. It _should_ also work in Delphi 10.4.2, and perhaps earlier.

## Project Configuration

These are instructions for configuring your own project to use the App Update feature

### Android libraries

This feature is dependent on the Play Core library, which does not ship with Delphi. Add `play-core-1.10.0.jar` from the `ThirdParty\Android` folder in Kastri to the Libraries node of the Android 32-bit target in Project Manager.

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

### Build Events

Please refer to the Build Events section of the Project Options. There are two commands in a Post-Build event:

* Merge `AndroidManifest.template.xml` using the `manifestmerge` tool in the Tools folder of Kastri
* If Delphi generates a `styles.xml` file (as it does when you enable the splash screen), the file `Resources\styles.merge.xml` is merged (using `resmerge` in the Tools folder) with the Delphi generated file into the `Resources\Overrides\res\values` folder. This file is included in the Deployment (see Deployment section below)

**You may need to modify these commands to suit your environment**.

### Deployment

As per the Build Events section above, the merged `styles.xml` is added to the deployment. As this file is an "override", the Delphi generated styles.xml deployment **needs to be disabled** (if one exists): 

<img src="Screenshots/Deployment.png" alt="logo" height="200">

## Thanks

Thanks to Igor Costa who helped with testing and provided valuable feedback



