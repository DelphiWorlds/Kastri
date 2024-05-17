# App Review Demo

## Description

Demonstrates implementation of app review frameworks on Android and iOS.

## Supported Delphi versions

Delphi 12, Delphi 11.x. It _should_ also work in Delphi 10.4.2, and perhaps earlier.

**Note:** As I do not have a current app on Google Play Store, I am _unable to test_ what the review dialog looks like. The `AndroidManifest.merge.xml` file contains information about how the dialog is themed, and may need to be "tweaked", or other measures may be needed.

## Project Configuration

When configuring your **own** project for Android:

1. Configure Build Events in Project Options to add a Post-Build event with the command:  
    ```
    [kastri]\Tools\manifestmerge AndroidManifest.merge.xml $(Platform)\$(Config)\AndroidManifest.xml
    ```  
    Where `[kastri]` is the path to the Kastri library. Do this for each required Android platform target (i.e. 32-bit and/or 64-bit)

2. Add `play-core-1.10.0.jar` from the `ThirdParty\Android` folder in Kastri to the Libraries node of the Android 32-bit target in Project Manager.

    **Note**:

    Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

## Usage

It's as simple as adding the `DW.AppReview` unit and this one line of code:

```
  AppReview.RequestReview;
```

Unfortunately, neither Android nor iOS appear to support any reliable way of knowing whether a user has already submitted a review, however if the user has done so already, calling `RequestReview` should do nothing at all.


