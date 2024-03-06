# Connectivity Demo

This demo shows how you can check whether a device has an internet connection, and if such a connection is via Wifi

## Supported Delphi versions

Delphi 12, Delphi 11.x. It _should_ also work in Delphi 10.4.2, and perhaps earlier.

## Supported Platforms

Supported platforms are: Windows, macOS, iOS and Android

## Project Configuration

### Android

If you are creating your own project, the Connectivity feature in Kastri relies on:

* Delphi 12: `dw-kastri-base-3.0.0.jar`
* Delphi 11.x: `dw-kastri-base-2.0.0.jar`

from the `Lib` folder, so add them to the `Libraries` node under the `Android 32-bit` platform target in Project Manager.

**Note:**

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will need to either apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to all projects), OR copy the `.jar` file(s) to *another* folder, and add them to the `Libraries` node of the `Android 64-bit` target. (Adding the same `.jar` file(s) to Android 64-bit does not work)

## macOS and/or iOS SDK

Regardless of whether you are creating your own project, depending on your version of Delphi, you may need to add the SystemConfiguration framework to the macOS and/or iOS SDK. There are some instructions on [how to add a framework here](https://delphiworlds.com/2013/10/adding-other-ios-frameworks-to-the-sdk-manager) (needs updating, however it should still work)
