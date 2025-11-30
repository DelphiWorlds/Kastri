# Notifications Demo

## Description

This code demonstrates use of the Notifications feature of Kastri which is designed to be a total replacement for the default local notifications support in Delphi.

That means that you should not have to use the NotificationCenter component in Delphi, at least for local notifications.

Note that support with this feature is currently for **Android and iOS only**.

## Supported Delphi versions

Delphi 13.0, Delphi 12.x. May or may not work in earlier versions.

## Project Configuration

The following instructions are for using the notification support *in your own project*. The required modifications are already applied in the demo

### Android and iOS

If you plan to include images in your notifications, you will need to ensure that the image file exists. In the demo, an example image is added to the deployment (AndroidGuy.png) to the default documents directory which is obtained using `TPath.GetDocumentsPath`.

### Android library

The notifications feature relies on:

* Delphi 13.0, 12.x: 

  `dw-kastri-base-3.0.0.jar`
 
from the `Lib` folder in Kastri, so add them to the `Libraries` node under the Android 32-bit platform in Project Manager.

### Android Manifest

* Deploy the project *at least once* - this will create `AndroidManifest.template.xml`
* Modify `AndroidManifest.template.xml` to add the following *just before* `<%receivers%>`

  ```
    <receiver
      android:name="com.delphiworlds.kastri.DWMultiBroadcastReceiver"
      android:exported="true">
      <intent-filter>
        <action android:name="com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_NOTIFICATION"/>
      </intent-filter>
    </receiver>
  ```

### Notification layout files

If you wish to use yor own layouts, you will need to add them to the deployment. See the [Android custom notifications](#android-custom-notifications) section.

## Android custom notifications

This requires a notification layout to be deployed. In the demo, there are two files are in the `Resources\layout` folder:

* `notification_custom.xml` is the original file that is fairly plain, and has the image aligned to the right
* `notification_special.xml` is an updated layout with a bit more attention to margins, and aligns the image to the left

For custom notifications to work, the layout files need to be deployed with a **Remote Path value of: `res\layout`**

If you are daring enough, and want to change the layout, modify these files. There's a tutorial about [layouts in Android here](https://www.tutorialspoint.com/android/android_user_interface_layouts.htm).

**NOTE - The custom notification implementation has been altered significantly (on Dec 1st, 2025)**:

Using both "compact" and "big" layouts is considered deprecated, so this is no longer supported in Kastri. Instead, use is made of [BigPictureStyle](https://developer.android.com/reference/androidx/core/app/NotificationCompat.BigPictureStyle?hl=en) which requires much less code, is easier to manage, and is considered to be the "defacto standard", being used in major apps such as GMail, WhatsApp, Telegram, Instagram etc.

## Android notification actions

To add an action, call the `AddAction` method of `TNotification`, e.g.

```delphi
  LNotification.AddAction('Call', 'Call Number');
```

The parameters are:

* ActionName - a name to identify the action. In a single notification, this should be unique 
* Text - This is displayed in the action

This example will display an action at the bottom of the notification with the text "Call Number".

When the user taps the action, the `OnNotificationActionResponse` event of `TNotifications` is called, passing a record containing:

* ActionName - as above. Helps to identify which action was executed
* NotificationID - A unique identifier for the notification
* NotificationName - The Name property of the notification when it was presented, or scheduled









