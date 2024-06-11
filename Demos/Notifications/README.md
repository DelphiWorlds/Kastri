# Notifications Demo

## Description

This code demonstrates use of the Notifications feature of Kastri which is designed to be a total replacement for the default local notifications support in Delphi.

That means that you should not have to use the NotificationCenter component in Delphi, at least for local notifications.

Note that support with this feature is currently for **Android and iOS only**.

## Supported Delphi versions

Delphi 12.x, Delphi 11.x. May or may not work in earlier versions.

## Project Configuration

### Android and iOS

If you plan to include images in your notifications, you will need to ensure that the image file exists. In the demo, an example image is added to the deployment (AndroidGuy.png) to the default documents directory which is obtained using `TPath.GetDocumentsPath`.

### Android library

The notifications feature relies on:

* Delphi 12.x: 

  `dw-kastri-base-3.0.0.jar`

* Delphi 11.x: 
  
  `dw-kastri-base-2.0.0.jar`
 
from the `Lib` folder in Kastri, so add them to the `Libraries` node under the Android 32-bit platform in Project Manager.

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will need to either apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

### Build Event/Android Manifest

**Delphi 12.1, when not [using Codex 2.3.1](../../Delphi12.1.AndroidManifestIssue.md)**

Due to changes in the Android build process:

* **Remove** the Build Events in Project Options for Android 32-bit and Android 64-bit 
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

**Delphi 12.0 or earlier:**

Configure Build Events in Project Options to add a Post-Build event with the command:  

```
  [kastri]\Tools\manifestmerge AndroidManifest.merge.xml $(Platform)\$(Config)\AndroidManifest.xml
```  
Where `[kastri]` is the path to the Kastri library. Do this for each required Android platform target (i.e. 32-bit and/or 64-bit)

`AndroidManifest.merge.xml` can be found in the root folder of the demo, and should be copied to the root folder of your project

### Android custom notifications

Using the notifications feature in Kastri, it requires two files to be deployed, namely:

```
notification_custom.xml
notification_custom_big.xml
```

`notification_custom.xml` is used for when the unexpanded notification is displayed and `notification_custom_big.xml` when the notification is expanded. If you are daring enough, and want to change the layout, modify these files. There's a tutorial about [layouts in Android here](https://www.tutorialspoint.com/android/android_user_interface_layouts.htm).

In the demo, the files are in the `Resources\layout` folder. **The files need to be deployed with a Remote Path value of: `res\layout`**






