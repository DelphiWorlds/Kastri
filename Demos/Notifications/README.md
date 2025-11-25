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
### Android custom notifications

This requires two files to be deployed, namely:

```
notification_custom.xml
notification_custom_big.xml
```

`notification_custom.xml` is used for when the unexpanded notification is displayed and `notification_custom_big.xml` when the notification is expanded. If you are daring enough, and want to change the layout, modify these files. There's a tutorial about [layouts in Android here](https://www.tutorialspoint.com/android/android_user_interface_layouts.htm).

In the demo, the files are in the `Resources\layout` folder. **The files need to be deployed with a Remote Path value of: `res\layout`**






