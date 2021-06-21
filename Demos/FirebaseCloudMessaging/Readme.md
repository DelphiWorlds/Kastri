# Firebase Cloud Messaging (FCM) Demo

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

* Unzip the latest Firebase SDK in the ThirdParty\Firebase\iOS folder of Kastri
* In the IDE Options > Environment Variables, create a user override called Firebase which points to the unzipped SDK, **or** modify the search paths in the Project Options for iOS Device 64-bit to point to the unzipped SDK
* In the Project Options for iOS Device 64-bit, modify the CFBundleIndentifier value  in the Version Info section to use the identifier that corresponds to your App ID
* Put your Google-Services.info.plist file from Firebase Console into the Resources folder of the demo
* Unzip the Firebase-6.18.zip file in the Kastri\ThirdParty\Firebase\iOS folder in-place

#### Android (32 bit and 64 bit where required)

In the Project Options:

* Modify the Package value in the Version Info section to be the value specified in your Firebase Project
* Import the google-services.json file from Firebase Console in the Services section

## Sending messages

### [PushIt](https://github.com/DelphiWorlds/PushIt)

PushIt is a crossplatform (macOS and Windows) app developed for testing FCM. It supports a number of features, including sending 'notification', 'data' or both types of messages, as well as specifying notification channel name (Android), 'content-available' flag, and more!

### Android

In this implementation of FCM, it is recommended that you send **only** 'data' messages to Android devices. This is because of how FCM behaves in the following scenarios:

App is:

* In the foreground: 'notification' messages are received, however there is no usable information. 'data' messages are received, and the code will present a banner if ShowBannerWhenForeground is True (default)
* In the background, or not running: 'notification' messages will result in a banner, however there is no message event triggered. 'data' messages will trigger a message event, and a banner will be presented. Messages with **both** 'notification' **and** 'data' are treated as 'notification' only messages, so no message event

### iOS

Send only 'notification' messages to iOS devices. From testing recently carried out, including 'data' in a message has no use.

### PushItServer

PushItServer is a framework being developed to help create push servers that manage registration of tokens, creation of groups, sending of the correct types of messages etc. Stay tuned to [Delphi Worlds](https://github.com/sponsors/DelphiWorlds) to find out more

## Integration into a new or existing project (not the demo)

* Please follow the steps as per the Using the demo section
* In the Project Manager, under the Android target, right-click the Libraries node and add the dw-kastri-base.jar and dw-firebase-messaging.jar files from the Kastri\Lib folder
* In the Project Options, ensure that you have a Post Build event for merging the manifest, as per the demo
* In the Project Options, ensure that the search path for **all** platforms includes folders from the Kastri repo as per the demo
* In the Project Options, ensure that the search path for iOS Device 64-bit includes the Firebase SDK paths as per the demo








