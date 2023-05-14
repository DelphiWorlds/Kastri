# Android Start At Boot Demo

## Description

Demonstrates implementation of starting an app at boot time on Android.

There are 2 projects, one for Delphi 10.4.2 or earlier (`AndroidStartAtBoot.dproj`), and one for Delphi 11.x or later. (`AndroidStartAtBootD11.dproj`)

## Project Configuration

These are instructions for configuring your **own project**. They do not need to be applied to the demo.

### Permissions

Start at boot requires these permissions to be checked in the Project Options:

* Receive boot completed
* Use full screen intent

### Android libraries

Add the [`dw-kastri-base-2.0.0.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-kastri-base-2.0.0.jar) library to the Libraries node, under the Android target in Project Manager.

If using Delphi 10.4.2 or earlier, add [`dw-kastri-base.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-kastri-base.jar) instead.

### Build Events

Please refer to the Build Events section of the Project Options of the demo. The command merges `AndroidManifest.template.xml` using the `manifestmerge` tool in the Tools folder of Kastri. You may need to modify the command in the Project Options of your own project to suit your file locations.




