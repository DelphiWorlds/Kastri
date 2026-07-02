# EMBTFCM Demo

## Description

This demo makes use of Embarcadero's FCM implementations for Android and iOS and packages them into something more convenient

## Supported Delphi versions

The demo should compile and work for at least versions 12.2 and later

## Supported Platforms

Supported platforms are: Android and iOS

## Project Configuration

Although [this link](http://docwiki.embarcadero.com/RADStudio/Florence/en/Firebase_Android_Support) relates to Android support for FCM in Delphi, the first 5 steps covers Firebase configuration that applies to **both Android and iOS**

### iOS

Ensure that the `CFBundleIdentifier` value in the Version Info section of the Project Options matches the App ID that your provisioning profile is configured for.

Ensure that the `GoogleServices-info.plist` file (from your Firebase Console project) is added to the deployment.

As at Delphi 13.1, the officially supported Firebase iOS SDK is v12.7.0, which can be obtained via the GetIt package manager in the IDE.

The `Framework search path` value in the `Building > Delphi Compiler` section of the Project Options should be updated to:

```
$(Firebase_EMBT)\FirebaseAnalytics\FBLPromises.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseAnalytics\FirebaseAnalytics.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseAnalytics\FirebaseCore.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseAnalytics\FirebaseCoreInternal.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseAnalytics\FirebaseInstallations.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseAnalytics\GoogleAppMeasurement.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseAnalytics\GoogleUtilities.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseAnalytics\nanoPB.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseMessaging\GoogleDataTransport.xcframework/ios-arm64;$(Firebase_EMBT)\FirebaseMessaging\FirebaseMessaging.xcframework/ios-arm64
```

Where `$(Firebase_EMBT)` is either configured as a [User System Override in the IDE options](https://docwiki.embarcadero.com/RADStudio/Florence/en/Environment_Variables) to point to the root of the Firebase SDK, **OR** replaced with the SDK path.

### Android

Complete steps 6 to 8 of the instructions [in the docwiki](http://docwiki.embarcadero.com/RADStudio/Florence/en/Firebase_Android_Support).

Ensure that the `package` value in the Version Info section of the Project Options matches the identifier you configured for your project in Firebase Console.

#### Sound

Using this implementation:

* The Secure File Sharing checkbox in the Entitlements section of the Project Options **MUST** be checked
* The sound file should be added to the deployment with a Remote Path that is accessible via secure file sharing, e.g. `.\assets\internal` or a subfolder thereof e.g. `.\assets\internal\sounds`
* Specify the filename with extension when creating the instance of `TPushNotifications`, e.g. as per the demo:
  ```delphi
  FPushNotifications := TPushNotifications.Create('EMBTFCM Push Notifications', 'RedAlert.mp3');
  ```
  If no path is specified in the sound name, it is assumed that sound files have been deployed to `.\assets\internal`. If a path **is** specified in the sound name, it **must** be the _full path_ to the file.
* If additional channels are required (each channel can have only one sound), use the `AddChannel` method of the `TPushNotifications` instance. This ensures that the `SoundName` value is compatible when the channel is created. See the `CreateCustomChannel` method of the main form in the demo for an example.

These measures were added to work around an issue with notification sounds in the EMBT implementation