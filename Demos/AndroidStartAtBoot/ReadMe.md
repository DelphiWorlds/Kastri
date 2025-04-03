# Android Start At Boot Demo

## Description

Demonstrates implementation of starting an app at boot time on Android.

## Supported Delphi versions

Delphi 12, Delphi 11.x.

## Project Configuration

These are instructions for configuring your **own project**. They do not need to be applied to the demo.

### Permissions

Start at boot requires these permissions to be checked in the Project Options:

* Receive boot completed
* Use full screen intent

### Android libraries

Add the Kastri base jar:

* Delphi 11.x: [`dw-kastri-base-2.0.0.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-kastri-base-2.0.0.jar) 
* Delphi 12.x: [`dw-kastri-base-3.0.0.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-kastri-base-3.0.0.jar) 
 
..to the Libraries node, under the Android target in Project Manager.

### Android Manifest

* Deploy the project *at least once* - this will create `AndroidManifest.template.xml`
* Modify `AndroidManifest.template.xml` to add *after* `<%application-meta-data%>`

  ```
    <meta-data android:name="DWMultiBroadcastReceiver.KEY_START_ON_BOOT" android:value="true" />
    <meta-data android:name="DWMultiBroadcastReceiver.KEY_RESTART_AFTER_REPLACE" android:value="true" />
  ```

  and add *after* `<%receivers%>`:

  ```
    <receiver
      android:name="com.delphiworlds.kastri.DWMultiBroadcastReceiver"
      android:exported="true">
      <intent-filter>
        <action android:name="android.intent.action.MY_PACKAGE_REPLACED"/>
        <action android:name="android.intent.action.BOOT_COMPLETED"/>
        <action android:name="android.intent.action.QUICKBOOT_POWERON" />
      </intent-filter>
    </receiver>
  ``` 

### Startup notification resource

By default, the notification sent at startup (required on Android 8 or higher) is: 

* Title: "Start At Boot"
* Body: "Please tap this notification for the application to start"

If you need to customise these values (this process is optional), construct a json file called `startupnotification.json`, and add it to the project deployment using the Deployment Manager in Delphi, specifying a remote path of: `res/raw`. There is an example file in the `Resources\res\raw` folder of the demo, however the file contains an array of JSON values that contain the locale identifier that the values apply to, and the title and body values, e.g.:

```
[
  {
    "default" : { 
      "title" : "Start At Boot",
      "body" : "Please tap this notification to start the application" 
    }
  },
  {
    "fr_FR" : {
      "title" : "Démarrer au démarrage",
      "body" : "Veuillez appuyer sur cette notification pour démarrer l'application"
    } 
  }
]
```

The first set of values **must** have the name "default" - these are used in case there is no matching set of values that has the locale that the device has been set to. The name for the values (e.g. "fr_FR" above) **must** match the locales supported on Android - [this Stack Overflow answer has a complete list](https://stackoverflow.com/a/30028371/3164070) as of Android 5.1. 

This method of using a JSON file in a resource has been used because at present, it is difficult to use Androids resource system with Delphi without having to do a lot more work.









