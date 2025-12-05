# ABLSticky (Android Background Location Sticky) Demo

## Description

Demonstrates a means of having location updates occur whether the app is not running, or is running in the background, or foreground

This demo was featured in the "Modern Android Services" session at CodeRage 2025, so you may wish to watch the replay video (link to the replay is coming soon)

As the name implies, the demo is for **Android ONLY**

This demo provides a foundation for being able to monitor location updates, however it is left up to the developer what to do with the location updates as they are received, in the `LocationReceived` method of the service. In the demo, it simply broadcasts the information as JSON which is received on the application side.  

## Supported Delphi Versions

- **Delphi 13.x**
- **Delphi 12.x**

## Project Configuration

The following outlines configuration when implementing the demo code in _your own project_. The changes have already been applied to the demo.

### Permissions

Please ensure that the following permissions are checked in the Project Options:

* Foreground service
* Foreground service location
* Access background location (Dangerous permissions section)
* Access fine location (Dangerous permissions section)

### Java libraries

The following Java libraries will need to be added to the _application_ project:

* `dw-fusedlocation-3.0.0.jar` - required for the Fused Location API
* `dw-kastri-base-3.0.0.jar` - required if using the message receiver (as per the application project of the demo)

These files are located in the `Lib` folder of Kastri. 

To add them to your project, in Project Manager expand the relevant Android target, right click the `Libraries` node, click `Add..` and select the relevant files.

**NOTE:** If compiling for both Android 32-bit and Android 64-bit, add the .jar file to the Android 32-bit target **ONLY** - it will still be compiled for 64-bit

### Android Manifest

Location services require metadata indicating the Google Play Services version. Unless your app is already configured to use a Google Play Services feature (in the Entitlements section of the Project Options, such as AdMob, Biometric, In App Purchase, Maps or Push Notifications), you will need to add this entry (just after the `<%application-meta-data%>` tag is a good spot):

```xml
    <meta-data android:name="com.google.android.gms.version" android:value="12451000" />
```

Delphi is yet to support specifying foreground service type, so it is necessary to replace the `<%services%>` tag with the services that your application uses. In the case of the demo, it's just ABLStickyService, so the entry will need to look like this:

```xml
    <service android:name="com.embarcadero.services.ABLStickyService" android:exported="false" android:foregroundServiceType="location" />
```

If your own project uses a different name for the service, change the `android:name` attribute accordingly.

## Technical Notes

### Service

As per the demo, the service uses a couple of convenience classes to make life easier.

#### TServiceManager

Helps with:

* Putting a service into the foreground
* Updating the persistent notification (if required - this is not the case in the demo)

Constructing an instance defaults to a service foreground service type of: location. If your service does something else, such as plays media, the code might look like this:

```delphi
  FServiceManager := TServiceManager.Create(JavaService, TJServiceInfo.JavaClass.FOREGROUND_SERVICE_TYPE_MEDIA_PLAYBACK);
```

In this case, if the media player associated with the service changes to a different item (audio or video), the persistent notification would necessarily change to indicate which item is playing, e.g.:

```delphi
  FServiceManager.UpdateNotification('Now Playing', 'Kick In The Door - Skunkhour', 'MediaPlayerService');
```

i.e. the notification caption is 'Now Playing', the text is: ['Kick In The Door - Skunkhour'](https://www.youtube.com/watch?v=oHA_392tDSs), and the channel name would be the same as for when the service was put into the foreground (in this case, 'MediaPlayerService')

#### LocalBroadcast

A "created on initialization" reference that provides a generic way of broadcasting messages that the application can receive. The application needs a corresponding "receiver" that can receive the messages (see [`TServiceMessageReceiver`](#tservicemessagereceiver) in the [Application](#application) section)

The following are the currently available methods:

* SendMessage - broadcasts an intent with an "extra" of `'MESSAGE'` that has a string value containing the message
* SendCommand - similar to `SendMessage` where the extra is `'COMMAND'`. The application receiving it could perform some action based on the string value
* SendState - broadcasts an intent with an "extra" of `'STATE'` that can indicate a state that the service is in, e.g. running, playing media, or whatever

As can be seen, these are (intentionally) very generic to allow flexibility in what can be sent

### Application

#### LocationPermissions

Another "created on initialization" reference that provides a generic way of requesting permissions required for location services (both Android and iOS).

There are properties to help customization of the requests depending on your needs:

* `NeedsBackgroundLocation` - defaults to True. Setting this to False omits the request for the "Access Background Location" on Android, and on iOS calls `requestWhenInUseAuthorization` as opposed to `requestAlwaysAuthorization`.
* `BackgroundPermissionMessage` (used on Android only) - defaults to an English version explaining to the user what they need to do when the background permissions prompt is presented. Change this property if you want a different message and/or would like to present a localized message.

The methods available are:

* Request - starts the request permissions process from the "base" permissions (e.g. `ACCESS_FINE_LOCATION` on Android). This call can include additional permissions that are required for the application to function, e.g. `BLUETOOTH_SCAN` which requires location permissions.
* RequestBackground - Usually called if a user has already granted location permissions, however they did not allow background location permissions when prompted

#### TServiceMessageReceiver

This class is used to receive broadcast messages from the service. Its ancestor `TMultiReceiver` uses Java code "under the hood", which is contained in `dw-kastr-base-3.0.0.jar`, which is added to the application project. The Java code is necessary because as yet, Delphi cannot implement descendants of Java classes. What it _can_ do however, is implement Java _interfaces_. 

The Java code in the .jar file for the receiver contains a class definition ([`DWMultiBroadcastReceiver`](https://github.com/DelphiWorlds/Kastri/blob/master/Java/Base/DWMultiBroadcastReceiver.java)) and an interface definition ([`DWMultiBroadcastReceiverDelegate`](https://github.com/DelphiWorlds/Kastri/blob/master/Java/Base/DWMultiBroadcastReceiverDelegate.java)). The `DWMultiBroadcastReceiverDelegate` interface is implemented in Delphi code in the `TMultiBroadcastReceiverDelegate` class, and its `onReceive` method forwards the received intent to the `Receive` method of the `TMultiReceiverAndroid` instance. This method is overridden in `TServiceMessageReceiver` in the demo app.

#### TServiceClient

Another service-related convenience class that makes it easy to start or stop a service where customization may be required for the intent passed to the service. (Delphi's `TServiceConnection` currently does not provide this).

Methods available are:

* IsServiceRunning - determines whether or not the service is running
* Start - starts the service, optionally with an intent. If an intent is passed, the `setClassName` method is called for you to set the service class name that was passed when the `TServiceClient` was created.
* Stop - stops the service

