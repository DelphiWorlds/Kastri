# FCM Rebooted Demo

## Description

This project demonstrates the implementation of Firebase Cloud Messaging (FCM) in Kastri, which extends the built-in support in Delphi by offering additional functionality.

Key features include:
- Customized notifications on Android using `RemoteViews` (e.g., multiple lines of text, optional image).
- Support for **images in iOS notifications** (refer to [this documentation](NotificationImagesOnIOS.md)).

The core unit, `DW.FCMManager`, simplifies FCM management by providing an interface (`IFCMManager`). You no longer need to create any classes—just assign event handlers and call the `Start` method on the `FCM` reference.

> **Note:** The `DW.FCMManager` unit requires a patch to the Delphi `FMX.PushNotification.FCM.iOS` unit. See [Delphi Source Patch](#delphi-source-patch) for details.

Two demos are included:
- **FCMBaseDemo:** A standard FCM implementation.
- **FCMRelayDemo:** Uses a service to handle messages, even when the app is not running.

**Important:** Android FCM messages must omit the `notification` element and include the `title`, `body`, and `imageUrl` (if needed) properties in the `data` element. Refer to the [Sending Test Messages](#sending-test-messages) section for details.

---

## Supported Delphi Versions

- **Delphi 12.x**
- **Delphi 11.3** (limited support)

---

## Project Configuration

### iOS

#### Apple Developer Portal

1. Create an APNs key for use with Firebase Cloud Messaging in your Firebase project.
2. Create an App ID and enable Push Notifications for it.
3. Create a Provisioning Profile linked to the App ID.
4. Download the Provisioning Profile to your Mac.

#### Firebase SDK

To use FCM, download the appropriate Firebase iOS SDK based on your Delphi version:

- **Delphi 12.2:** [Firebase iOS SDK 11.2.0](https://github.com/firebase/firebase-ios-sdk/releases/download/11.2.0/Firebase.zip)
- **Delphi 12.1 and earlier:** [Firebase iOS SDK 10.8.0](https://github.com/firebase/firebase-ios-sdk/releases/download/10.8.0/Firebase-10.8.0.zip)

Unzip the SDK to a shared folder and create an [Environment Variable Override](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Environment_Variables) named `Firebase` pointing to the folder. Alternatively, update the [Framework Search Path](#framework-search-path).

> **Note:** For Firebase iOS SDK 11.2.0, the minimum iOS version must be set to **13.0**.

#### Delphi Source Patch

Due to updates in the Firebase SDK, patching the Delphi `FMX.PushNotification.FCM.iOS` unit is necessary. To apply the patch:

1. Navigate to `<Kastri>\Features\Firebase`.
2. Run the command:
   ```bash
   FCMPatch.cmd
   ```

For manual patching instructions, refer to [FCMManualPatch.md](../../Features/Firebase/FCMManualPatch.md).

> **Note:** Place the patched file in the project folder or a location in the search path. Avoid modifying the Delphi source folder directly.

#### Framework Search Path

Configure the framework paths based on the Firebase SDK version:

**For Firebase iOS SDK 10.8.0**:
```text
$(Firebase)\FirebaseAnalytics\FBLPromises.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\FirebaseAnalytics.xcframework\ios-arm64_armv7;$(Firebase)\FirebaseAnalytics\FirebaseCore.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\FirebaseCoreInternal.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\FirebaseInstallations.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\GoogleAppMeasurement.xcframework\ios-arm64_armv7;$(Firebase)\FirebaseAnalytics\GoogleUtilities.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\nanoPB.xcframework\ios-arm64;$(Firebase)\FirebaseMessaging\FirebaseMessaging.xcframework\ios-arm64;$(Firebase)\FirebaseMessaging\GoogleDataTransport.xcframework\ios-arm64
```

**For Firebase iOS SDK 11.2.0 (Delphi 12.2 or later)**:
```text
$(Firebase)\FirebaseAnalytics\FBLPromises.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\FirebaseAnalytics.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\FirebaseCore.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\FirebaseCoreInternal.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\FirebaseInstallations.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\GoogleAppMeasurement.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\GoogleUtilities.xcframework\ios-arm64;$(Firebase)\FirebaseAnalytics\nanoPB.xcframework\ios-arm64;$(Firebase)\FirebaseMessaging\FirebaseMessaging.xcframework\ios-arm64;$(Firebase)\FirebaseMessaging\GoogleDataTransport.xcframework\ios-arm64
```

For **iOS Simulator** paths, use `ios-arm64_x86_64-simulator` instead of `ios-arm64`.

#### Additional Frameworks

Add the following frameworks in Delphi's SDK Manager:
- CoreTransferable
- DeveloperToolsSupport
- SwiftUI
- SwiftUICore

Refer to [this guide](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSDKFrameworks) for instructions.

#### Deployment of `GoogleServices-info.plist`

1. Download the `GoogleServices-info.plist` file from Firebase Console.
2. Add it to the project’s Resources folder.
3. Include it in the deployment settings.

#### Linker Options

For Firebase iOS SDK 11.2.0:
- Set the **Minimum iOS version supported** to `13.0` (or `14.0` for iOS Simulator).
- Add the following linker options:
  ```text
  -ObjC -rpath /usr/lib/swift -weak_library /usr/lib/swift/libswift_Concurrency.dylib -weak_library /usr/lib/swift/libswift_StringProcessing.dylib -weak_library /usr/lib/swift/libswiftDataDetection.dylib -weak_library /usr/lib/swift/libswiftFileProvider.dylib -weak_library /usr/lib/swift/libswiftOSLog.dylib -weak_library /usr/lib/swift/libswiftXPC.dylib
  ```

---

### Android

#### Java Libraries

Include the appropriate Kastri JAR files in the project:
- Delphi 12.x: `dw-kastri-base-3.0.0.jar`, `dw-fcm-3.0.0.jar`
- Delphi 11.x: `dw-kastri-base-2.0.0.jar`, `dw-fcm-2.0.0.jar`

Add these to the **Libraries** node under the Android platform in Project Manager.

> **Note:** Delphi 11.3 users compiling for Android 64-bit may need [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled).

#### Android Manifest (Delphi 12.1 or Later)

For Delphi 12.1 or later, the manifest merge is broken. Manually apply the changes by including required permissions and configuration entries. Refer to the official documentation or the demo for an example.

---

## Relay Demo

The **Relay Demo** handles push notifications using a service (`FCMRelayService`), even when the app is inactive. For details on setting up the service, refer to the demo’s documentation.

Example push notification payload for the Relay Demo:
```json
{
  "message": {
    "topic": "FCMRebooted",
    "data": {
      "SMSText": "Test Message",
      "SMSDest": "+610499999999",
      "title": "Test SMS",
      "body": "This is a test"
    }
  }
}
```

---

## Sending Test Messages

Use [PushIt](https://github.com/DelphiWorlds/PushIt) to send test messages. Ensure the **Data Only Notification** checkbox is selected for Android.

---

## Troubleshooting

### iOS
- **Compile error** - `[DCC Error] E2597 ld: file not found: PromisesObjC`: The source has not been patched as per [these instructions](#delphi-source-patch). 
- **No token received**: Verify the Provisioning Profile and ensure Push Notifications are enabled.
- **Compiler errors**: Check framework paths and Firebase SDK compatibility.

### Android
- **Messages not received**: Ensure the payload format is correct and omit the `notification` element.
