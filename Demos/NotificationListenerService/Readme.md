# NotificationListenerService demo

## Description

Demonstrates the NotificationListenerService implementation in Kastri. The main files used in the implementation are in the `Core` folder (`DW.NotificationListenerServiceReceiver.Android.pas`) and `Java\Base` folder

## Configuration

### Manifest merge

The demo project has a file called `AndroidManifest.merge.xml`, and there is a `Post-Build` build event in the Project Options (Under Building > Build Events) for both Android 32-bit and Android 64-bit, that uses the `manifestmerge` tool (from the `Tools` folder in Kastri) to merge the entries into the final `AndroidManifest.xml` file.

The merge adds a `service` entry for the service called `DWNotificationListenerService`, which handles the notification events. The service sends a local broadcast that the application receives and processes.

### Kastri java library

There is also a java library, `dw-kastri-base-2.0.0.jar` (from the `Lib` folder) added to the project. This contains the java code that includes `DWNotificationListenerService`.


