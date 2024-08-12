# NotificationListenerService demo

## Description

Demonstrates the NotificationListenerService implementation in Kastri. The main files used in the implementation are in the `Core` folder (`DW.NotificationListenerServiceReceiver.Android.pas`) and `Java\Base` folder

## Configuration

### Manifest merge (Delphi 12.0 and earlier)

The demo project has a file called `AndroidManifest.merge.xml`, and there is a `Post-Build` build event in the Project Options (Under Building > Build Events) for both Android 32-bit and Android 64-bit, that uses the `manifestmerge` tool (from the `Tools` folder in Kastri) to merge the entries into the final `AndroidManifest.xml` file.

The merge adds a `service` entry for the service called `DWNotificationListenerService`, which handles the notification events. The service sends a local broadcast that the application receives and processes.

**For Delphi 12.1 or later**, see [this link](https://github.com/DelphiWorlds/Kastri/blob/master/Delphi12.1.AndroidManifestIssue.md).

### Kastri java library

If you are creating your own project, the NotificationListenerService feature in Kastri relies on:

* Delphi 12.x: `dw-kastri-base-3.0.0.jar`
* Delphi 11.x: `dw-kastri-base-2.0.0.jar`

from the Lib folder, so add them to the Libraries node under the Android 32-bit platform target in Project Manager.

This contains the java code that includes `DWNotificationListenerService`.

Note:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will need to either apply this workaround (which will apply to all projects), OR copy the .jar file(s) to another folder, and add them to the Libraries node of the Android 64-bit target. (Adding the same .jar file(s) to Android 64-bit does not work)
There is also a java library, `dw-kastri-base-2.0.0.jar` (Delphi 11.x) or `dw-kastri-base-3.0.0.jar` (Delphi 12.x) (from the `Lib` folder) added to the project. 

