# FCM Rebooted Demo

## Description

This incarnation of FCM support is a substantial reworking of the original FCM support in Kastri, however some elements remain.

In this implementation, support has been added for a customised notification on Android, using RemoteViews, similar to the [CustomNotification demo in the Playground repo](https://github.com/DelphiWorlds/Playground/tree/main/Demos/CustomNotification). This allows multiple lines of text in the notification banner, and an optional image, placed to the right of the text.

The unit `DW.FCMManager` handles management of FCM, which is exposed as a reference to an interface: `IFCMManager`. Now you do not need to create any classes; just assign event handlers, and call the `Start` method on the `FCM` reference.

There are 2 demos: 

* A regular FCM demo, and
* A demo of using a service to handle the message regardless of whether or not the app is running

**In order for messages to be received properly on Android, FCM message payloads will need to omit `notification` elements, and include `title`, `body` and `imageUrl` (if needed) properties in the `data` element.** Please refer to the [Sending test messages](#sending-test-messages) section.

## Supported Versions

At present, the only supported version is Delphi 11.x. Support for 10.4.x may come later, however it is unlikely.

## Project Configuration

For setup in Firebase Console and the Apple Developer site, please refer to the [instructions in the original FCM demo](https://github.com/DelphiWorlds/Kastri/blob/master/Demos/FirebaseCloudMessaging/Readme.md).

### iOS

FCM Rebooted can be used with the latest of the 8.x versions of the Firebase iOS SDK, which is v8.15.0. Due to technical issues as [reported here](https://quality.embarcadero.com/browse/RSP-38700), there is presently no support for v9.x or later, however it may come later.

Please download the [Firebase iOS SDK from here](https://github.com/firebase/firebase-ios-sdk/releases/download/v8.15.0/Firebase.zip), and for the demo, extract it into the `ThirdParty` folder, so that the path matches the framework search paths in the project, i.e. folders starting with `ThirdParty\Firebase`.

Download the `GoogleServices-info.plist` file from your project configured in [Firebase Console](https://console.firebase.google.com/), and save it to the Resources folder in the demo.

If you are creating your own project:

Note that the `iOSapi.FirebaseMessaging` unit needed to be "patched" in order to work with this version of the Firebase iOS SDK, so that patched unit will need to be included with your project.

Ensure that a value of `-ObjC` for the `Options passed to the LD linker` option is in the `Linking` section of the Project Options.

Add `GoogleServices-info.plist` to the deployment, as per the demo, as described above.

### Android

If you are creating your own project:

FCM Rebooted relies on `dw-kastri-base-2.0.0.jar` and `dw-fcm-2.0.0.jar` from the `Lib` folder, so add them to the `Libraries` node under the `Android 32 bit` platform in Project Manager.

**Note if using Delphi 11.3** (as at Jun 3rd, 2023):

    Due to a bug in Delphi 11.3, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the required jar files to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file to Android 64-bit does _not_ work)

### Relay Demo

The "Relay" demo uses metadata that is merged into the Android manifest (using `AndroidManifest.merge.xml` in that demo), to specify the service to relay the notification to - in this case: `com.embarcadero.services.FCMRelayService`, i.e. the fully qualified name of the service.
Use the `AndroidIntentServiceHandleIntent` event in the service to handle the notification.

**Note that `dw-fcm-2.0.0.jar` is not (yet) the same as `dw-firebase-messaging.jar`, and you do not need `dw-firebase-messaging.jar`**

Please ensure that the `Receive push notifications` checkbox is checked in the Entitlements Section of the Project Options.

In the `Build Events` section of Project Options, please ensure that you configure the `Post Build` event to execute the `manifestmerge` tool (as per the demo), which ensures that the correct overridden `FirebaseMessageingService` is configured in the manifest when the project is built.

For your own project, **or** the demo:

In the `Services` section of the Project Options, import the google-services.json file that you download from your project configured in [Firebase Console](https://console.firebase.google.com/).

## Sending test messages

Test messages can be sent using the [PushIt](https://github.com/DelphiWorlds/PushIt) tool. Please take note of the section on how to obtain the service account file from the [Cloud Console](https://console.cloud.google.com/iam-admin/serviceaccounts).

**On Android, for this implementation of FCM to work, you will need to check the "Data Only Notification" checkbox.** If you do not check this checkbox, the title and body of the notification will be blank.

This is due to a limitation in FCM where messages that contain a "notification" property (regardless of whether it contains a "data" property) do not cause the customized FCM service to be invoked when the app is not running or is in the background. 

Added bonus: images can be included in the customised notification (they will appear on the left). Use the Image URL edit box to include a URL to an image, e.g: https://sd.keepcalms.com/i/keep-calm-and-love-delphi-26.png

With PushIt, you can see what the resulting payload looks like by selecting the `JSON` tab, which can help guide you in constructing message payloads in your server.

## Troubleshooting

### iOS

If your app is receiving a Firebase token, but does not appear to be receiving messages, it is likely to be either:

* The App ID being used for the Provisioning Profile does not have Push Notifications enabled
* The payload being used for the message being sent is incorrect

You can check that the App ID/Provisioning Profile is correct by examining the `AppName.entitlements` file that Delphi creates when deploying the app (where `AppName` is the name of your app) in the `iOSDevice64\Config` folder (where `Config` is the active config e.g. `Debug` or `Release`). It should contain the following:

```
<key>aps-environment</key>
<string>development</string>
```

Where `development` will be replaced by `production` when using an App Store Provisioning Profile. For info regarding provisioning profile configuration, please refer to the [instructions in the original FCM demo](https://github.com/DelphiWorlds/Kastri/blob/master/Demos/FirebaseCloudMessaging/Readme.md)













