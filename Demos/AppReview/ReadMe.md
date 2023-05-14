# App Review Demo

## Description

Demonstrates implementation of app review frameworks on Android and iOS.

The code/demo was built for Delphi 11.x, however it should also work in Delphi 10.4.2, and perhaps earlier.

## Project Configuration

When configuring your own project for Android, please add `play-core-1.10.0.jar` from the `ThirdParty\Android` folder in Kastri to the Libraries node of the Android 32-bit target in Project Manager.

**Note if using Delphi 11.3** (as at May 15th, 2023):

Due to a bug in Delphi 11.3, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy `play-core-1.10.0.jar` to _another folder_, and add it to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file to Android 64-bit does _not_ work)

## Usage

It's as simple as adding the `DW.AppReview` unit and this one line of code:

```
  AppReview.RequestReview;
```

Unfortunately, neither Android nor iOS appear to support any reliable way of knowing whether a user has already submitted a review, however if the user has done so already, calling `RequestReview` should do nothing at all.


