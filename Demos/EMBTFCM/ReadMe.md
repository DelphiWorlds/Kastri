# EMBTFCM Demo

## **NOTE**

**This implementation is now considered legacy, and should be used only in Delphi 10.4.x or earlier.**

You can find the newer implementation [here](https://github.com/DelphiWorlds/Playground/tree/main/Demos/FCMRebooted).

_No further changes will be made to this demo_

## Description

This demo makes use of Embarcadero's FCM implementations for Android and iOS and packages them into something more convenient

For iOS, Embarcadero has implemented FCM in Delphi 10.4.2 or later. For 10.4.1 or earlier, **this demo** uses the implementation from Kastri.

**NOTE: For Delphi 10.4.2 or later, you will need to patch the `iOSapi.FirebaseMessaging` unit in order to use Firebase iOS SDK 7.0 or later. See below for details**

### Supported Delphi versions

The demo should compile and work for at least versions 10.3.3 and 10.4.x

### Supported Platforms

Supported platforms are: Android and iOS

### Configuration

For iOS, please ensure that you put your `GoogleServices-info.plist` file (from your Firebase Console project) in the `Resources` folder of the demo.

For Android, please follow the instructions as per the [Embarcadero documentation](http://docwiki.embarcadero.com/RADStudio/Sydney/en/Firebase_Android_Support)

Please ensure that you change the values for `CFBundleIdentifier` (for iOS) and `Package` (for Android) in the Version Info of the Project Options to match the identifier you have created for your FCM app

### Firebase iOS SDK and Framework search path for the project

At the time of writing, the current version of the Firebase iOS SDK is v7.7.0. You can download the current version using [this link](https://firebase.google.com/download/ios)

The `Framework search path` value in the Project Options is configured to use an environment variable, `$(Firebase)`, that points to the Firebase iOS SDK

You can set up an Environment Variable in Delphi - Tools|Options, IDE > Environment Variables, User System Overrides, name it `Firebase` and select the path to the SDK.

For Firebase iOS SDK earlier than version 7.0, use the paths recommended by Embarcadero

### Using Firebase iOS SDK 7.0 or later requires patching the `iOSapi.FirebaseMessaging` unit

Embarcadero's implementation of FCM for iOS (introduced in Delphi 10.4.2) was tested with Firebase iOS SDK v6.18.0

In order to use the newer versions of the Firebase iOS SDK, copy `iOSapi.FirebaseMessaging.pas` from $(BDS)\source\rtl\ios (where $(BDS) is where Delphi is installed) to the project directory, and comment out this line (just before the end of the unit):

```
procedure ProtobufLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'Protobuf';
```

As this framework is now incorporated into another part of the Firebase iOS SDK
