# AdMob Demo

## Description

Advertising implementation, specifically for AdMob (at present)

**NOTE: The demo has been updated (on Nov 8th, 2023) to align it with the Firebase iOS SDK v10.8.0**. See the [Libraries > iOS](#ios) section below.

## Supported Delphi versions

Delphi 12, Delphi 11.x.

## Project configuration

### Component

**Please install the TAdMobBannerAd component first** (before even loading the demo)

A package containing the component is available [here](https://github.com/DelphiWorlds/Kastri/blob/master/Packages/KastriAdMob.dproj)

### Libraries

#### Android

If creating your own project, you will need to add the supporting library:

* For Delphi 11.x: [`dw-admob.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-admob.jar) 
* For Delphi 12: [`dw-admob-3.0.0.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-admob-3.0.0.jar) 

to the Libraries node under the Android platform in Project Manager

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

#### iOS

** UPDATED Nov 8th, 2023 **

AdMob support in Kastri has now been aligned with the latest compatible version of the Firebase SDK for iOS, which is version 10.8.0. Please [download the SDK from here](https://github.com/firebase/firebase-ios-sdk/releases/download/10.8.0/Firebase-10.8.0.zip), and unzip it somewhere, preferably in a folder that can be common to other projects that use the SDK. Create an [Environment Variable User System Override](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Environment_Variables) called `Firebase`, and set it to the folder where the SDK was unzipped to. This corresponds to the `$(Firebase)` macro in the Project Options of the demo.

In order to compile successfully for iOS, it's also necessary to:

1. Add [Swift Support Files in Delphi's SDK Manager](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSwiftSupport) (follow the link for instructions)
2. Add the following frameworks to the iOS SDK in Delphi's SDK Manager (if they are not already added):

* Accessibility
* Combine
* AdServices
* CoreMotion
* DataDetection
* UniformTypeIdentifiers

[This link](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSDKFrameworks) describes how to add the frameworks.

### Project Options

If creating your own project:

1. Set the Framework Search Path value for iOS Device 64 target to:
   ```
   $(Firebase)\FirebaseAnalytics\FBLPromises.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\GoogleAppMeasurement.xcframework\ios-arm64_armv7;$(Firebase)\FirebaseAnalytics\GoogleAppMeasurementIdentitySupport.xcframework\ios-arm64_armv7;$(Firebase)\FirebaseAnalytics\GoogleUtilities.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\nanopb.xcframework\ios-arm64;$(Firebase)\Google-Mobile-Ads-SDK\GoogleMobileAds.xcframework\ios-arm64_armv7;$(Firebase)\Google-Mobile-Ads-SDK\UserMessagingPlatform.xcframework\ios-arm64_armv7
   ```

2. Ensure you have a value of: `-ObjC -rpath /usr/lib/swift` for the `Options passed to the LD linker` option in the Project Options for iOS Device 64-bit:

   <img src="./Screenshots/ObjCLinkerOption.png" alt="ObjC linker option" height="400">

### Android Entitlements

Ensure your project has the `AdMob Service` enabled. This adds Google Play services metadata and the Ads activity to the manifest.

### Android Permission

Ensure your project has the `Access Network State` permission in Project Options

### Using non-test Ad UnitIds

When configuring your app for non-test Ad UnitIds, you will need to:

* For Android: modify `AndroidManifest.template.xml` to add meta-data that includes your AdMob Application ID, as per part 3 of [these instructions](https://developers.google.com/admob/android/quick-start?hl=en-US#import_the_mobile_ads_sdk). Please refer to the image below for an example of where to put the meta-data. 
  
<img style="margin-left: 3em;" src="./Screenshots/AndroidManifestTemplateAppId.png" alt="PM" height="300"/>

* For iOS: modify your projects `info.plist.TemplateiOS.xml` file to add the `GADApplicationIdentifier` key **and the array below it** from the `info.plist.TemplateiOS.xml` file _in the demo_, but change the _value_ for `GADApplicationIdentifier` to your AdMob Application ID. Please refer to the image below for an example of what to change. 
  
<img style="margin-left: 3em;" src="./Screenshots/iOSInfoPListTemplateAppId.png" alt="PM" height="150"/>

* Ensure that the `TestMode` property of the instances of AdMob classes is set to False, and the AdUnitId is set to a valid AdUnitId for your application.



