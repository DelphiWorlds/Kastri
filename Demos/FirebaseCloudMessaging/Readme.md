# Firebase Cloud Messaging (FCM) Demo

## IMPORTANT change for iOS users

**FCM support in Kastri has been updated to be compatible with Firebase iOS SDK v8.7.0**

Please see the Demo Configuration section below

## Using the demo

Please configure for a platform only where you **require** support for that platform

In order to use this demo, you will need to:

### [Apple Developer portal](http://developer.apple.com/account) (iOS)

* Create an APNs key for use with Firebase Cloud Messaging in your Firebase project
* Create an App ID and enable Push Notifications for it
* Create a Provisioning Profile that has the App ID attached to it
* Download the Provisioning Profile on to the Mac

### [Firebase Console](https://console.firebase.google.com) (both platforms)

* Create a Firebase project that you can use for FCM
* Add iOS support to the project
* Add the APNs key to the Cloud Messaging iOS support
* Add Android support to the project

### Demo configuration

#### iOS

**Note that these configuration instructions were changed on Sept 30th, 2021 to support a more recent Firebase SDK**

* Go to [this link](https://github.com/firebase/firebase-ios-sdk/releases/tag/8.7.0), download Firebase.zip, and extract it somewhere convenient
* In the IDE Options > Environment Variables, create a user override called Firebase_8_7 which points to the unzipped SDK, **or** modify `Framework search path` in the Project Options for iOS Device 64-bit to point to the unzipped SDK
* In the Project Options for iOS Device 64-bit, modify the CFBundleIndentifier value  in the Version Info section to use the identifier that corresponds to your App ID
* Put your Google-Services.info.plist file from Firebase Console into the Resources folder of the demo

#### Android (32 bit and 64 bit where required)

In the Project Options:

* Modify the Package value in the Version Info section to be the value specified in your Firebase Project
* Import the google-services.json file from Firebase Console in the Services section

## Sending messages

[PushIt](https://github.com/DelphiWorlds/PushIt) is a crossplatform (macOS and Windows) app developed for testing FCM. It supports a number of features, including sending 'notification', 'data' or both types of messages, as well as specifying notification channel name (Android), 'content-available' flag, and more!

### PushItServer

PushItServer is a framework being developed to help create push servers that manage registration of tokens, creation of groups, sending of the correct types of messages etc. Stay tuned to [Delphi Worlds](https://github.com/sponsors/DelphiWorlds) to find out more

## Integration into a new or existing project (not the demo)

* Please follow the steps as per the Using the demo section
* In the Project Manager, under the Android target, right-click the Libraries node and add the dw-kastri-base.jar and dw-firebase-messaging.jar files from the Kastri\Lib folder
* In the Project Options, ensure that you have a Post Build event for merging the manifest, as per the demo
* In the Project Options, ensure that the search path for **all** platforms includes folders from the Kastri repo as per the demo
* In the Project Options, ensure that the search path for iOS Device 64-bit includes the Firebase SDK paths as per the demo








