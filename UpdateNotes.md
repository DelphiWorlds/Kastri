# Update Notes

This documents updates to Kastri which may adversely impact users, and describes what steps to take.

## Oct 2nd, 2024

* Updated linker options in `FCMBaseDemoD12` project to resolve **crashes on earlier versions of iOS (e.g. 15.7.1)**
  
  Specifically, added: `-weak_library /usr/lib/swift/libswiftXPC.dylib`, which prevents the app from loading the dylib if it does not exist.

  If any of your users have an earlier version of iOS, you will want to **update your apps with this change**.

## Sep 23rd, 2024

* Updated `API\DW.iOSapi.MLKitCommon.pas` to add support for Firebase iOS SDK v11.2.0 and retain compatibility with v10.8.0, which is required when combining Firebase Cloud Messaging support
  
  Please read the [readme in the demo](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/BarcodeReader) to ensure that your project configuration is correct

## Sep 21st, 2024

* Updated Firebase Cloud Messaging (FCM) and AdMob (Google Mobile Ads) implementations to support Firebase iOS SDK 11.2.0
  
  Firebase iOS SDK 11.2.0 requires additional iOS SDK frameworks, so when compiling you may receive linker errors when compiling. Please refer to the iOS section of the project configuration sections as to which frameworks need to be added.

  For FCM, see [here](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted#firebase-sdk)

  For AdMob, see [here](https://github.com/DelphiWorlds/Kastri/blob/master/Demos/AdMob/ReadMe.md#firebase-sdk)


 

