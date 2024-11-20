# Barcode reader demo

## Description

This demonstrates the use of the [Google MLVision API to scan barcodes](https://developers.google.com/ml-kit/vision/barcode-scanning/android). 

## Supported Delphi versions

Delphi 12.x, Delphi 11.3 (limited support)

## Project Configuration

These are instructions for configuring your own project. The demo has already been configured.

### Android 

### Vision libraries

The following jar files are used by the project:

* play-services-vision-17.0.2.jar
* play-services-vision-common-17.0.2.jar

Both are located in the [`ThirdParty\Android`](https://github.com/DelphiWorlds/Kastri/tree/master/ThirdParty/Android) folder

You will need to add these jars to the `Libraries` node under the Android 32-bit platform in Project Manager

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

#### Manifest

You will also need to modify `AndroidManifest.template.xml` - under `<%application-meta-data%>`, add this line:

```xml
<meta-data android:name="com.google.android.gms.version" android:value="12451000" />
```

### iOS

#### Firebase iOS SDK

Delphi 12.2 has an updated linker, which means that newer iOS SDKs can now successfully be linked with Delphi code. Please download the Firebase iOS SDK depending on your version of Delphi. 

**NOTE**: If you are combining the [**Firebase Cloud Messaging** support from Kastri](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted) (i.e. **not** the built-in FCM support that comes with Delphi) with your project, unfortunately due to library conflicts, **you MUST use Firebase iOS SDK 10.8.0** (see also [Conditional Defines](#conditional-defines))

* Delphi 12.2 - [Firebase iOS SDK 11.2.0](https://github.com/firebase/firebase-ios-sdk/releases/download/11.2.0/Firebase.zip)
* Delphi 12.1 and earlier - [Firebase iOS SDK 10.8.0](https://github.com/firebase/firebase-ios-sdk/releases/download/10.8.0/Firebase-10.8.0.zip)

..and unzip it somewhere, preferably in a folder that can be common to other projects that use the SDK. 

**NOTE:** Using Barcode Scanning with Firebase iOS SDK 11.2.0 requires that the **minimum version of iOS be set to 12.0** (see [Linker Options](#linker-options))

Create an [Environment Variable User System Override](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Environment_Variables) called `Firebase`, and set it to the folder where the SDK was unzipped to. This corresponds to the `$(Firebase)` macro in the Project Options of the demo. Alternatively, you will need to modify the [Framework search path](#framework-search-path) value to use the folder where the SDK is unzipped to.

#### Swift Compatibility libraries

If you use Firebase iOS SDK 11.2.0, it's also necessary to add [Swift Support Files in Delphi's SDK Manager](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSwiftSupport) (follow the link for instructions)

#### ML Kit Libraries

The Barcode Scanner feature is dependent on prebuilt libraries in the [`ThirdParty\iOS`](https://github.com/DelphiWorlds/Kastri/tree/master/ThirdParty/iOS) folder

The MLKitBarcodeScanning framework is over 100MB, so it has been zipped (`MLKitBarcodeScanning.framework.zip`). **It will need to be unzipped in place, in order for the app to compile**

#### Conditional defines

If you are combining the Barcode Reader feature with other features dependent on Firebase (such as the [FCM implementation](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted)), add a conditional define of `FIREBASE` in the Delphi Compiler section of the Project Options:

<img src="./Screenshots/FirebaseConditionalDefine.png" alt="logo" height="150">

#### Framework Search Path

The Framework search path value needs to include:

* A path to the `ThirdParty\iOS` folder in Kastri, which includes the ML Kit frameworks
* Paths to the framework folders in the Firebase iOS SDK

The value in the demo is:

```
..\..\ThirdParty\iOS;$(Firebase)\FirebaseAnalytics\FBLPromises.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\nanoPB.xcframework\ios-arm64;$(Firebase)\FirebaseMessaging\GoogleDataTransport.xcframework\ios-arm64;$(Firebase)\GoogleSignIn\GTMSessionFetcher.xcframework\ios-arm64
```

#### Project Search Path

The Search path value needs to include:

* A path to the `ThirdParty\iOS` folder in Kastri, which includes additional binaries for ML Kit
* Paths to dependent code from Kastri

The value in the demo is:

```
..\..\ThirdParty\iOS;..\..\API;..\..\Core;..\..\Include;..\..\Features\Barcode
```

#### Linker Options

For `Minimum iOS version supported`, when using **Firebase iOS SDK 11.2.0 - Delphi 12.2 or later**, ensure the value is set to: `12.0`

For the `Options passed to the LD linker` option in the Project Options for iOS Device 64-bit, ensure you have a value of: 

```
-ObjC -rpath /usr/lib/swift -weak_library /usr/lib/swift/libswift_Concurrency.dylib -weak_library /usr/lib/swift/libswift_StringProcessing.dylib -weak_library /usr/lib/swift/libswiftDataDetection.dylib -weak_library /usr/lib/swift/libswiftFileProvider.dylib -weak_library /usr/lib/swift/libswiftOSLog.dylib -weak_library /usr/lib/swift/libswiftXPC.dylib
``` 

The `-weak_library` entries ensure compatibility with iOS 15 or lower.

## Example Scan Result:

<img src="./Screenshots/BarcodeScanExample.png" alt="logo" height="1200">

