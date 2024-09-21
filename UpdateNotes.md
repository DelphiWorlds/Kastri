# Update Notes

This documents updates to Kastri which may adversely impact users, and describes what steps to take.

## Sep 21st, 2024

* Updated Firebase Cloud Messaging (FCM) and AdMob (Google Mobile Ads) implementations to support Firebase iOS SDK 11.2.0
  
  Firebase iOS SDK 11.2.0 requires additional iOS SDK frameworks, so when compiling you may receive linker errors when compiling. Please refer to the iOS section of the project configuration sections as to which frameworks need to be added.

  For FCM, see [here](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted#firebase-sdk)

  For AdMob, see [here](https://github.com/DelphiWorlds/Kastri/blob/master/Demos/AdMob/ReadMe.md#firebase-sdk)


 

