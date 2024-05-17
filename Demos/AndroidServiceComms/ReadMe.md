# Android Service Comms Demo

## Description

Demonstrates implementations of communication between an Android app, and a service used by the app.

The demo uses two forms:

1. Via service "binding", which is supported "out of the box" in Delphi, and appears in RAD Studio demos provided by Embarcadero
2. Via local broadcasts

Checking/unchecking the checkbox at the top of the demo determines which is used.

There may be other means that can be used for communication, however the two above are simple, and work.

There is a [ChatGPT conversation here](https://github.com/DelphiWorlds/HowTo/blob/main/ChatGPTConversations/AndroidBindServiceVsStartService.md) about the merits of using `bindService` (used in method 1) vs using `startService` (used in method 2)

## Supported Delphi versions

Delphi 12, Delphi 11.x.

## Project Configuration

If you are starting your own Android project that contains a service, [this is a recommended video](https://www.youtube.com/watch?v=0mD3WLK8FYc) to watch. It's from 2015, however the process is pretty much the same. The important things are that the application and service should be part of a project group, that you build the service at least once before using the `Add Service` menu item in Project Manager to add the service to the application.

Communication method 2 (local broadcasts) requires the addition of the Kastri base library: `dw-kastri-base-2.0.0.jar` for Delphi 11.x and `dw-kastri-base-3.0.0.jar` for Delphi 12. These libraries are in the `Lib` folder of Kastri. Add it to the `Libraries` node under the Android platform in Project Manager.

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will need to either apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

## Service "binding"

Service binding is achieved in the demo by creating an instance of `TLocalServiceConnection`, setting the `OnConnected` and `OnDisconnected` event handlers, and calling `BindService` (in the `ConnectButtonClick` handler). Once the `OnConnected` event occurs (handled by `ServiceConnectedHandler`) a reference to the service instance can be obtained, and its `OnSendData` event handler can be assigned. If this event is assigned, the service uses it to send data to the application.

## Local Broadcasts

If local broadcasts are used, the service needs to be started (via `ConnectButtonClick`). When the service starts (see `AndroidServiceStartCommand` in the service), a message is sent via local broadcast to notify the application that the service has started. Both the service and application use a local broadcast receiver to receive messages from each other.

## Messages in the demo

Messages are constructed using the `TDataMessage` record type (in the `ASC.Common` unit), which are serialized to/deserialized from JSON, so that the local broadcast can use a plain string in the extras of the intent being sent.

Messages that are "commands" have a value of `2` for `DataType` and the `Data` is a plain string. Messages that are just a response have a value of `1`. Messages that contain image data (as a base64 string) have a value of `3` for `DataType`

## Expanding beyond simple data

As described above, the communication here contains very rudimentary information, but could be expanded to handle more complex data.


