# Firebase Analytics demo

## Description

Demonstrates the Firebase Analytics features implemented in Kastri.

## Supported Delphi versions

Delphi 12.2, 13.x

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

## Delphi Project Configuration

### Application identifiers

In Project Options of the Delphi project, select the Version Info section, and select the applicable target. For Android, update the `Package` property to match the package name specified for Android in the Firebase project. For iOS, update the `CFBundleIdentifier` value to match the App ID specified for iOS in the Firebase project.

### Android

#### Add Dependent Packages

In Project Manager, expand the Target Platforms node and:

1. For Android 32-bit **ONLY**, right-click the Libraries node, click `Add`, and add the following files from the ThirdParty\Android folder in Kastri:
   * `play-services-basement-18.4.0.aar`
   * `play-services-measurement-api-20.1.0.aar`
   
   **Note that these are `.aar` files**
2. For **BOTH** Android 32-bit **and** Android 64-bit, expand the Libraries node, right click `play-services-basement-18.4.0.dex.jar`, and click **Disable**. This is required since the equivalent `.aar` file was added in the previous step.

#### Import the `google-services.json` file

In Project Options of the Delphi project, select the Services section and select Android as the target. Click the `Import` button and select the `google-services.json` file that was saved earlier. Do this step for Android 32 bit and Android 64 bit if you are targeting both.

#### Entitlements

In order to have Firebase Analytics work on Android, the easiest way to ensure that the necessary entries are in the Android manifest:

* In the Entitlements section of the  Project Options, enable the `Receive Push Notifications` entitlement

This is regardless of whether or not your app will support Push Notifications.

### iOS

#### Firebase SDK

A [release of pre-built binaries](https://github.com/DelphiWorlds/Binaries/releases/tag/iOS-Arm64-Firebase-Messaging-12.6.0-ML-Kit-Barcode-Scanning-9.0.0-v1.0.0) which ensures compatibility with barcode scanning (even if you do not use that feature) has been made available in the Binaries repo.

Unzip it to somewhere convenient, and in the Project Options for the iOS Device - 64 bit platform:

1. In the **Framework Search Path** value, include the folder where the binaries were unzipped to
2. In the Linker Options, set the Minimum iOS Version value to 15.5, if you wish to avoid linker warnings.

#### Swift Compatibility Libraries

If using Delphi 12.2, you must add [Swift Support Files](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSwiftSupport) via Delphi's SDK Manager.

#### Deployment of GoogleServices-info.plist

Download the `GoogleServices-info.plist` file from your project configured in [Firebase Console](https://console.firebase.google.com/), and save it to the Resources folder in the demo. Add `GoogleServices-info.plist` to the deployment, as per the demo.



