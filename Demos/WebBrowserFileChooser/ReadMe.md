# WebBrowser File Chooser Demo

## Description

Demonstrates the implementation of handling choosing of files when tapping an upload link in a page in `TWebBrowser`, on Android

## Supported Delphi versions

Delphi 12, Delphi 11.x. May also work 10.4.x or earlier.

## Project Configuration

When configuring your own project for Android, please add `dw-webchromeclient.jar` from the `Lib` folder in Kastri to the Libraries node of the Android target in Project Manager.

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

## Usage

Please see the `TForm1.Create` method in `Unit1.pas` of the demo for [an example](https://github.com/DelphiWorlds/Kastri/blob/4e72ad7891a09ef02da7cc6f7782b80cc456862d/Demos/WebBrowserFileChooser/Unit1.pas#L36) of how to create `TWebChromeClientManager`, and configure it for use, including assigning an event handler for `OnFileChooser`.

See also the `TForm1.ManagerFileChooserHandler` method in the same unit for [an example](https://github.com/DelphiWorlds/Kastri/blob/4e72ad7891a09ef02da7cc6f7782b80cc456862d/Demos/WebBrowserFileChooser/Unit1.pas#LL47C11-L47C43) of how to handle different mime types, and setting of `AFileChooserKind` for example to use the camera, instead of selecting from a gallery.







