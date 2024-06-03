# Kastri

<img src="kastri-logo.png" alt="logo" height="250">

From [Delphi Worlds](http://www.delphiworlds.com)

[![Twitter](https://img.shields.io/badge/-Twitter-1DA1F2?style=plastic&logo=Twitter)](https://twitter.com/DelphiWorlds)  [![Facebook](https://img.shields.io/badge/-Facebook-green?style=plastic&logo=Facebook)](https://www.facebook.com/DelphiWorlds)  [![Instagram](https://img.shields.io/badge/-Instagram-833AB4?style=plastic&logo=Instagram)](https://www.instagram.com/delphiworlds)

Compatible with [Embarcadero](https://wwww.embarcadero.com) [Delphi](https://www.embarcadero.com/products/delphi). Please note: Only the **last two major releases** (e.g. at present that includes Delphi 12.x and Delphi 11.x) are "officially" supported. 

Development of Kastri can be helped along with [Github Sponsorship](https://github.com/sponsors/DelphiWorlds), so please consider sponsoring today!

Alternatively, you might like to make a donation via Stripe*: 

<a href="https://donate.stripe.com/5kAdRecDkf7J30k3cc">
    <img src="https://kinsta.com/wp-content/uploads/2018/05/stripe-donate-button-1-1-360x180.png" height="50">
</a>

(*Donations were previously being made via another service which I no longer use, as they were unable to keep my funds secure. My apologies for any inconvenience)

Please star this repo by clicking the Star box in the top right corner if you find it useful!

If you're looking to [learn Delphi, please visit this link](https://learndelphi.org)

## Kastri

* Is a cross-platform library which builds upon the existing RTL, FMX and VCL libraries in Delphi
* Supports a number of newer APIs that you won't find in FMX/RTL, and "backfills" for missing APIs
* Is structured in a way so as to avoid creating unnecessary dependencies
* Follows strict coding standards to ensure consistency and readability
* Is kept as simple as practicable
* Is named after the [place in Greece](https://en.wikipedia.org/wiki/Kastri,_Phocis) where [Delphi](https://en.wikipedia.org/wiki/Delphi) once stood

## Playground

[Playground](https://github.com/DelphiWorlds/Playground) is a separate repository that serves as a testing ground for features that may or may not be included in Kastri. Please visit the Playground for features/demos that are on the bleeding edge!

## Delphi 11/12 changes

With the introduction of Delphi 11 and Delphi 12, there have been some changes related to Android. You may notice new jar files in the `Lib` folder where the name is the same as an existing jar, with a suffix of `2.0.0` or `3.0.0` e.g. `dw-kastri-base-3.0.0.jar`. Jars with a `2.0.0` suffix will work **only with Delphi 11**, and those with a suffix of `3.0.0` will work **only with Delphi 12**. If a jar does not have a corresponding jar with a `2.0.0` or `3.0.0` suffix, it should also work with Delphi 11 or Delphi 12. Please also see the Demos section below.

**Due to the above changes to Delphi 11 and Delphi 12, the existing demos that were created with Delphi 10.4 or Delphi 11 when using them with a later version of Delphi may require the [Android Libraries fix](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AndroidLibraries)** 

## Intro video

[This is a link to an intro video for Kastri](https://delphiworlds.s3-us-west-2.amazonaws.com/kastri-sponsor-video.mp4)

## How To Use Kastri

For the most part, with Kastri there are no components to install. Simply clone the repo ([Fork](https://git-fork.com/) is a highly recommended Git client), or download/unzip and ensure that your Project (recommended) or IDE paths point to the folders of the units that you use.

The [Native Controls suite (Kastri FMX)](./Controls) is a set of components that can be [installed into Delphi](./Packages) and used at design-time

## Pull Requests

Pull requests are welcome, however before making any, please read the [Coding Standards readme](https://github.com/DelphiWorlds/Kastri/blob/master/CodingStandards.md) and ensure that the change adheres to the standards, and that the **Description** section of the request is filled out, noting the purpose and reasoning for the change.

In regards to coding standards, in particular observe things like naming, ensuring fields/methods are kept in alphabetical order (which makes them easier to find as the number of methods grow), and that structured statements follow the standard.

## Support

### Issues page

If you encounter an issue, or want to request an enhancement, please [visit the issues page](https://github.com/DelphiWorlds/Kastri/issues) to report it.

### Slack Channel

The Delphi Worlds Slack workspace has a channel (#kastri) devoted to discussing Kastri. There you will find Dave Nottage (when he is available) and other developers who are passionate about Kastri to discuss how to use Kastri, what is in it etc.

[Sponsors](https://github.com/sponsors/DelphiWorlds) of Kastri can receive priority support via the Slack workspace, and/or via email.

If you would like to join the Delphi Worlds Slack workspace, [please visit this self-invite link](https://slack.delphiworlds.com)

## What Is In Kastri?

### Demos

You'll find demo applications in the [Demos](./Demos) folder, including those associated with articles posted on the [Delphi Worlds blog](http://www.delphiworlds.com/blog)

Note: Most demos have a readme associated with them, and **they may have vital configuration information that should be applied when using the feature(s) in your own projects**

**Delphi 11/12 users please note**: Some of the demos that have Android support have been updated in line with changes in Delphi 11/12, and now have separate project (`.dproj`) and group project (`.groupproj`) (where applicable) files, which have been suffixed with `D11` and `D12`. For demos that do not have a suffix in the name, you may need to open Project Manager and for each Android target you wish to use, right-click the `Libraries` node and click `Revery System Files to Default` before building the project. 

### API imports

In the API folder you will find imports for a number of iOS, macOS, Android and Linux APIs that are either not included with Delphi, serve to complete missing APIs, or are imports for Java code contained in Kastri. 
Most of these are used in the Kastri Features and/or demos however some are included due to interest from developers.

### Core units

Units in the Core folder contain code of a wealth of cross-platform related implementations. Units that have platform specific code are suffixed with one of:

* .Android.pas
* .iOS.pas
* .Linux.pas
* .Mac.pas - can be for both iOS and macOS
* .Posix.pas - for Posix-based platforms (iOS, macOS, Android, Linux)
* .Win.pas

Implementations include (but are not limited to):

* DW.OSLog - logging code that serves to replace Log.d, which relies on FMX, and cannot be used in an Android service
* DW.OSDevice - code that returns information that is mostly device-specific
* DW.OSTimer - leaner, less complex implementation of a timer. Also does not rely on FMX, so can be used in an Android service

### Features

The Features folder contains more feature-rich cross-platform implementations of APIs. These are structured in such a way so as to shield your cross-platform code from the platform-specific implementation.
There are demos for each of the features in the Demos folder
Features include (amongst others):

#### AdMob

Support for Google Ads on Android and iOS

#### AndroidPDF 

PDF Renderer for Android

#### AppReview

In-app invocation of App Review for the relevant app store.

#### AppUpdate

In-app updating of the application. (Android only)

#### AudioPlayer

Play audio files on Android, iOS and Windows. Specifically constructed to allow synchronization of audio and visual display.

#### Barcode

Based on [Google ML Vision APIs](https://developers.google.com/ml-kit/vision), it allows you to scan an image and detect barcodes within it

#### Biometric

Android and iOS implementations of biometric functions, namely: Fingerprint recognition on Android, and Fingerprint and FaceID on iOS

#### Camera

Android and iOS implementations of native camera support

#### Connectivity

Android, macOS, iOS and Windows implementations of code for checking network connectivity of a device

#### Files Selector

Uses native APIs to allow users to select one or more files on the device, for Android and iOS

#### Firebase

Android and iOS implementations of Firebase Cloud Messaging using their native libraries

#### Geofencing support on Android

Works a little like the regions support does for iOS with [TLocationSensor](http://docwiki.embarcadero.com/Libraries/Sydney/en/System.Sensors.Components.TLocationSensor). Set up geographical regions (each region is a location with a specified radius), and events fire when the user passes in/out of those regions

#### Google SignIn

Google SignIn impolementation for Android and iOS, however as at June 3rd, 2024 the iOS implementation is yet to be functional.

#### Location

Android and iOS framework made specifically for location updates when the application is in any state (e.g. even when it is not running)

#### NFC

Near-Field Communication implementations for Android and iOS

#### Notifications

Framework that is alternative to TNotificationCenter (that ships with Delphi) for Notifications on Android and iOS

#### Proximity

Support for the proximity sensor on Android and iOS, including switching the screen on/off e.g. when the user puts the device to their ear

#### Share Items

A more feature-rich alternative for the Share Sheet implementation provided with Delphi, for Android and iOS

#### SMS

Support for sending SMS messages on Android and iOS

#### Sound Player

For playing short sounds such as audio effects in games. Support for Android, iOS and macOS

#### Speech Recognition

Android and iOS implementations for speech-to-text services. On Android, the actual speech API is used, rather than via Intents

#### Symbol Scanner

Scanning of codes specifically for popular scanning devices such as Zebra, Symbol and Honeywell (Android)

#### Text To Speech

Allows your device to speak from the supplied text. Support for Android, iOS, macOS and Windows

#### Universal Links

Allows users to follow links to content inside your app from your website.

#### Video Player

Plays video using the latest technologies on the respective platforms, e.g. ExoPlayer on Android. Support for Android and iOS. macOS and Windows support are in development.

#### Web Browser Ext

Adds and improves functionality of TWebBrowser, such as asynchronous bitmap capture, asynchronous JavaScript execution with results handling, Cookies/Cache clearing, Element click handling, and more!

The above features take time and resources to develop. Your [sponsorship](https://github.com/sponsors/DelphiWorlds) can help make more/improved features a reality!

## License

Kastri is licensed under MIT, and the license file is included in this folder.


![](https://tokei.rs/b1/github/DelphiWorlds/Kastri)

