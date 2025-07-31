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
- **FCMRelayDemo:** Uses a service to handle messages so that you can do **custom processing** of messages when the app is not running. 

**Important:** With this implementation, Android FCM messages must omit the `notification` element and include the `title`, `body`, and `imageUrl` (if needed) properties in the `data` element. Refer to the [Sending Test Messages](#sending-test-messages) section for details.

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

In order to compile successfully for iOS, it's also necessary to:

1. Add [Swift Support Files in Delphi's SDK Manager](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSwiftSupport) (follow the link for instructions)
2. Add the following frameworks in Delphi's SDK Manager:
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

#### Entitlement List

In the Entitlement List section of the Project Options, ensure that `Receive push notifications` is checked.

#### Android Manifest

In AndroidManifest.template.xml, make the following changes:

**Replace** the `<%services%>` tag with:

```xml
  <service android:exported="false" android:name="com.delphiworlds.kastri.DWFirebaseMessagingService">
      <intent-filter>
          <action android:name="com.google.firebase.MESSAGING_EVENT" />
      </intent-filter>
  </service>
  <service
      android:name="com.google.firebase.messaging.FirebaseMessagingService"
      android:directBootAware="true"
      android:exported="false" >
      <intent-filter android:priority="-500" >
          <action android:name="com.google.firebase.MESSAGING_EVENT" />
      </intent-filter>
  </service>
  <service
      android:name="com.google.firebase.components.ComponentDiscoveryService"
      android:exported="false" >
      <meta-data
          android:name="com.google.firebase.components:com.google.firebase.messaging.FirebaseMessagingRegistrar"
          android:value="com.google.firebase.components.ComponentRegistrar" />
      <meta-data
          android:name="com.google.firebase.components:com.google.firebase.datatransport.TransportRegistrar"
          android:value="com.google.firebase.components.ComponentRegistrar" />
      <meta-data
          android:name="com.google.firebase.components:com.google.firebase.installations.FirebaseInstallationsRegistrar"
          android:value="com.google.firebase.components.ComponentRegistrar" />
  </service>
```

The reason for making this change is so that the Embarcadero Firebase Messaging proxy service (`com.embarcadero.firebase.messaging.ProxyFirebaseMessagingService`) is "overridden", since it does not implement some of the functionality that the one from Kastri does.

---

## Relay Demo

**NOTE: If you are using Delphi 12.3** please ensure that you have applied all patches. See [this report in the Quality Portal](https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-3108) for a workaround to the compiler error you will receive, if you have not applied the patch.

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
- **Other Compiler errors**: Check framework paths and Firebase SDK compatibility.
- **No token received**: Verify the Provisioning Profile and ensure Push Notifications are enabled.

  In the project output folder, Delphi generates a file called <project>.entitlements. Inside this file (which is just xml) you should be able to see an entry with a `key` value of: `aps-environment`. If not, the provisioning profile that was used does not use an Application ID that has Push Notifications enabled.

- **App crashes on start**: Ensure that your GoogleServices-info.plist being deployed as per [this section](#deployment-of-google-services-info-plist).
  
### Android
- **Messages not received**: Ensure the payload format is correct and omit the `notification` element. Also ensure that the [changes to `AndroidManifest.template.xml`](#android-manifest) are correct.
