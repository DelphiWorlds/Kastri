# AdMob Demo

## Description

Advertising implementation, specifically for AdMob (Google Mobile Ads)

Support for User Messsaging Platform (UMP) has been added, which is required by Google in some geographic locations. This is **enabled by default**, and will require configuration in your [AdMob settings](https://apps.admob.com).

See [below](#user-messaging-platform-ump-support) for further information about the UMP support.

## Supported Delphi versions

Delphi 13.x, 12.x

**NOTE: The project for Delphi 12 or later is `AdTestD12.dproj` and the project for Delphi 11.x is `AdTest.dproj`** (Delphi 11.x is considered legacy here)

## Usage Notes

The "full screen" ad types:

* TInterstitialAd
* TRewardedInterstitialAd
* TRewardedAd

..have an option of performing the load of an ad, and showing an ad, all in one call by calling `Load` with no parameters, **OR** the load can be performed separately by calling `Load(False)`, and once the ad is loaded (which happens in the background), the `OnAdLoaded` event is called, and at that point the `Show` method can be called to show the ad.  

The latter method means that ads can be "preloaded" so that when it comes time to show an ad, it shows **immediately**.

The former method may result in a poor user experience, given that it takes a little while for the ad to load first, then be shown.

## Project configuration

### Component

**Please install the TAdMobBannerAd component first** (before even loading the demo)

A package containing the component is available as [KastriAdMob.dproj](https://github.com/DelphiWorlds/Kastri/blob/master/Packages/KastriAdMob.dproj) in the Packages folder of Kastri.

### User Messaging Platform (UMP) support

As [required by Google](https://support.google.com/admob/answer/10114014), if using AdMob, your app must show a consent message before ads can be shown. Kastri supports this via the `DW.AdMob` unit. Call RequestConsent when your app is ready to request constent.

```delphi
  AdMob.RequestConsent; // Call RequestConsent(True) to bypass UMP
```

While you are testing your app, you can use the following methods:

* `AdMob.SetDebugGeography` - this is used to simulate being in certain geographic locations. Presently there is just: `EEA` (European Economic Area) and `NonEEA` (aside from the `Disabled` option)
* `AdMob.SetTestDeviceHashedId` - sets an identifer used by UMP for testing. 
  
Before you actually use `SetTestDeviceHashedId`, when you run your app the value to be used will appear in the OS log messages which can be viewed using a log viewer. See [this link](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/LogViewers) for information about using log viewers. For Android, the message will appear in the logcat messages like this:

```
Use new ConsentDebugSettings.Builder().addTestDeviceHashedId("33BE2250B43518CCDA7DE426D04EE231") to set this as a debug device.
```

For iOS, the message will appear in the Console app like this:

```
<UMP SDK>To enable debug mode for this device, set: UMPDebugSettings.testDeviceIdentifiers = @[2077ef9a63d2b398840261c8221a0c9b]
```

In both cases, **all you need are the actual values** - please disregard the code examples in the messages.

You can use the `AdMob.ResetConsent` method to simulate a "first install" experience.

See the `TForm1.Create` method in the demo for examples.

### Android

#### Adding dependent libraries

If creating your own project, you will need to add the supporting library:

* For Delphi 11.x (considered legacy, here): [`dw-admob.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-admob.jar) 
* For Delphi 12.x or later: [`dw-admob-3.0.0.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-admob-3.0.0.jar)

to the Libraries node under the Android 32-bit platform in Project Manager. To do this, in Project Manager:

1. Expand the Target Platforms
2. Expand Android 32-bit
3. Right-click the Libraries node and click Add..
4. Select the relevant .jar file and click Open

#### Android Entitlements

Ensure your project has the `AdMob Service` enabled in the Entitlement List in Project Options. This adds Google Play services metadata and the Ads activity to the manifest.

#### Android Permission

Ensure your project has the `Access Network State` permission in Project Options

#### Generating a jar that contains R classes

This process is required **ONLY if you are using UMP support** ([see above](#user-messaging-platform-ump-support))

User Messaging Platform (UMP) requires `R` classes associated with the play-services-ads-22.2.0.jar file that is part of the "default" libraries used by Delphi 12.x **or later**. When using Android Studio, these are generated automatically; with Delphi, they need to be generated separately. 

This process could be done manually via the command line or in a batch file, however an easier way is through the use of [Codex](https://github.com/DelphiWorlds/Codex). Once you install Codex, you can follow these steps:

1. Build and deploy your project **at least once**. This step is important for merging the resources Delphi creates, with resources in the Play Services Base library
2. From the Codex menu in Delphi, in the Android Tools section, use the Download Package function:

   <img src="./Screenshots/CodexPackageDownload.png" alt="Codex Package Download" height="200">
   
   ..to download/extract play-services-basement, using this value in the Packages edit:

   ```
   com.google.android.gms:play-services-ads:22.2.0
   ```

   <img src="./Screenshots/PackageDownloadPlayServicesBase.png" alt="Package Download Play Services Base" height="400">
   
   ..and click `Extract`

3. Right-click the project in Project Manager, and click `Add Android Package`:
   
   <img src="./Screenshots/CodexAddAndroidPackageMenu.png" alt="Codex Add Android Package Menu" height="200">

   ..and add the folder that the package was extracted to in step 2:
   
   <img src="./Screenshots/CodexAddAndroidPackage.png" alt="Codex Add Android Package" height="200">

   ..and click `Execute` to build the R classes for play-services-base and add the resulting jar to the project
   
4. Rebuild/deploy your project

This adds a library with the same name as the project, with an extension of `.R.jar`, to the Libraries node of the Android 32-bit target in Project Manager. If you compile for Android 64-bit, the jar will still be compiled in with your app.

#### Ad UnitIds

If you are using Ad UnitIds that are **not** for testing, please see [this section](#using-non-test-ad-unitids).

### iOS

#### Firebase SDK

Delphi 12.2 has an updated linker, which means that newer iOS SDKs can now successfully be linked with Delphi code. Download the Firebase iOS SDK from one of these links:

* Delphi 12.2 - [Firebase iOS SDK 11.2.0](https://github.com/firebase/firebase-ios-sdk/releases/download/11.2.0/Firebase.zip)
* Delphi 12.1 and earlier - [Firebase iOS SDK 10.8.0](https://github.com/firebase/firebase-ios-sdk/releases/download/10.8.0/Firebase-10.8.0.zip)

..and unzip it somewhere, preferably in a folder that can be common to other projects that use the SDK. 

Create an [Environment Variable User System Override](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Environment_Variables) called `Firebase`, and set it to the folder where the SDK was unzipped to. This corresponds to the `$(Firebase)` macro in the Project Options of the demo. You can use the framework search path value from the Project Options in your own project.

**NOTE:** Using GoogleMobileAds from Firebase iOS SDK 11.2.0 requires that the **minimum version of iOS be set to 12.0** (see [Linker Options](#linker-options))

In order to compile successfully for iOS, it's also necessary to:

1. Add [Swift Support Files in Delphi's SDK Manager](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSwiftSupport) (follow the link for instructions)
2. Add the following frameworks to the iOS SDK in Delphi's SDK Manager (if they are not already added):

* Accessibility
* AdServices
* AppTrackingTransparency
* Combine
* CoreMotion
* CoreTransferable
* DataDetection
* MarketplaceKit (if using Firebase iOS SDK v11.2.0)
* Network (if using Firebase iOS SDK v11.2.0)
* SwiftCore
* SwiftUI
* UniformTypeIdentifiers

Remember that using Firebase iOS SDK v11.2.0 requires **Delphi 12.2 or later**

[This link](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSDKFrameworks) describes how to add the frameworks.

### Project Options

If creating your own project:

#### Framework search path

In Project Options, set the Framework Search Path value for iOS Device 64-bit target to:

When using Firebase iOS SDK 10.8.0:

```
$(Firebase)\FirebaseAnalytics\FBLPromises.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\GoogleAppMeasurement.xcframework\ios-arm64_armv7;$(Firebase)\FirebaseAnalytics\GoogleAppMeasurementIdentitySupport.xcframework\ios-arm64_armv7;$(Firebase)\FirebaseAnalytics\GoogleUtilities.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\nanopb.xcframework\ios-arm64;$(Firebase)\Google-Mobile-Ads-SDK\GoogleMobileAds.xcframework\ios-arm64_armv7;$(Firebase)\Google-Mobile-Ads-SDK\UserMessagingPlatform.xcframework\ios-arm64_armv7
```

When using Firebase iOS SDK 11.2.0 - **Delphi 12.2 or later**:

```
$(Firebase)\FirebaseAnalytics\FBLPromises.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\GoogleAppMeasurement.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\GoogleAppMeasurementIdentitySupport.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\GoogleUtilities.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\nanopb.xcframework\ios-arm64;$(Firebase)\Google-Mobile-Ads-SDK\GoogleMobileAds.xcframework\ios-arm64;$(Firebase)\Google-Mobile-Ads-SDK\UserMessagingPlatform.xcframework\ios-arm64
```

#### Linker Options

For `Minimum iOS version supported`, when using **Firebase iOS SDK 11.2.0 - Delphi 12.2 or later**, ensure the value is set to: `12.0`

For the `Options passed to the LD linker` option in the Project Options for iOS Device 64-bit, ensure you have a value of: 
```
-ObjC -rpath /usr/lib/swift -weak_library /usr/lib/swift/libswift_Concurrency.dylib -weak_library /usr/lib/swift/libswift_StringProcessing.dylib -weak_library /usr/lib/swift/libswiftDataDetection.dylib -weak_library /usr/lib/swift/libswiftFileProvider.dylib -weak_library /usr/lib/swift/libswiftOSLog.dylib -weak_library /usr/lib/swift/libswiftXPC.dylib
``` 

The `-weak_library` entries ensure compatibility with iOS 15 or lower.

#### NSUserTrackingUsageDescription

Add a `NSUserTrackingUsageDescription` key and value to the Version Info section of the project options. The value should be indicative of why your app requires user tracking. Since this is for AdMob, an example of this could be:

```
This app uses tracking data to help deliver ads that are relevant to you
```

### Using non-test Ad UnitIds

When configuring your app for non-test Ad UnitIds, you will need to:

* For Android: modify `AndroidManifest.template.xml` to add meta-data that includes your AdMob Application ID, as per part 3 of [these instructions](https://developers.google.com/admob/android/quick-start?hl=en-US#import_the_mobile_ads_sdk). Please refer to the image below for an example of where to put the meta-data. 
  
<img style="margin-left: 3em;" src="./Screenshots/AndroidManifestTemplateAppId.png" alt="PM" height="300"/>

* For iOS: modify your projects `info.plist.TemplateiOS.xml` file to add the `GADApplicationIdentifier` key **and the array below it** from the `info.plist.TemplateiOS.xml` file _in the demo_, but change the _value_ for `GADApplicationIdentifier` to your AdMob Application ID. Please refer to the image below for an example of what to change. 
  
<img style="margin-left: 3em;" src="./Screenshots/iOSInfoPListTemplateAppId.png" alt="PM" height="150"/>

* Ensure that the `TestMode` property of the instances of AdMob classes is set to False, and the AdUnitId is set to a valid AdUnitId for your application.



