# App Update Demo

## Description

Demonstrates implementation of [in-app updating of Play Store apps on Android](https://developer.android.com/guide/playcore/in-app-updates). As such, this demo really only applies to Android.

**The code/demo was built for Delphi 11, however it might be able to be modified to work in Delphi 10.4.2**

Please refer to the demo as a guide on how to use `TAppUpdate`.

* `CheckForUpdate` is an **asynchronous** call to check if an update is available, and returns the info in the `OnAppUpdateInfo` event
* `StartUpdate` is an **asynchronous** call to start the update flow for the specified type. The `OnAppUpdateStartedFlow` event is called with the `Started` parameter indicating whether or not the flow did actually start. The `OnAppUpdateResult` event is called when the flow finishes, and the `UpdateResult` parameter indicates whether or not the update succeeded, or the user canceled.

## Project Configuration

These are instructions for configuring your own project to use the App Update feature

### Android libraries

This feature is dependent on the Play Core library, which does not ship with Delphi. Add the [`play-core-1.10.0.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/ThirdParty/Android/play-core-1.10.0.jar) library to the Libraries node, under the Android target in Project Manager.

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



