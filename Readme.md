# Kastri

From [Delphi Worlds](http://www.delphiworlds.com)

Development of Kastri can be helped along with [Github Sponsorship](https://github.com/sponsors/DelphiWorlds), so please consider sponsoring today!

## Kastri

* Is a cross-platform library which builds upon the existing RTL, FMX and VCL libraries in Delphi
* Supports a number of newer APIs that you won't find in FMX/RTL, and "backfills" for missing APIs
* Is structured in a way so as to avoid creating unnecessary dependencies
* Follows strict coding standards to ensure consistency and readability
* Is kept as simple as practicable
* Is named after the [place in Greece](https://en.wikipedia.org/wiki/Kastri,_Phocis) where [Delphi](https://en.wikipedia.org/wiki/Delphi) once stood

## How To Use Kastri

With Kastri, there are no components to install. Simply clone the repo ([Fork](https://git-fork.com/) is a highly recommended Git client), or download/unzip and ensure that your Project (recommended) or IDE paths point to the folders of the units that you use.

## Support

### Issues page

If you encounter an issue, or want to request an enhancement, please [visit the issues page](https://github.com/DelphiWorlds/Kastri/issues) to report it.

### Slack Channel

The Delphi Worlds Slack team has a channel (#kastri) devoted to discussing Kastri. There you will find Dave Nottage (when he is available) and other developers who are passionate about Kastri to discuss how to use Kastri, what is in it etc.

[Sponsors](https://github.com/sponsors/DelphiWorlds) of Kastri have access to the private #sponsors channel where they receive priority support.

If you would like to join the Slack team, [please visit this self-invite link](https://slack.delphiworlds.com)

## What Is In Kastri?

### Demos

You'll find demo applications in the Demos folder, including those associated with articles posted on the [Delphi Worlds blog](http://www.delphiworlds.com/blog)

### API imports

In the API folder you will find imports for a number of iOS and Android and Linux APIs that are either not included with Delphi, serve to complete missing APIs, or are imports for Java code contained in Kastri
Most of these are used in the Kastri Features and/or demos however some are included due to interest from developers.

### Core units

Units in the Core folder contain code of a wealth of cross-platform related implementations. Units that have platform specific code are suffixed with one of:

* .Android.pas
* .iOS.pas
* .Linux.pas
* .Mac.pas - can be for both iOS and macOS
* .Posix.pas - for Posix-based platforms (iOS, macOS, Android, Linux)
* .Win.pas

Implementations include:

* DW.OSLog - logging code that serves to replace Log.d, which relies on FMX, and cannot be used in an Android service
* DW.OSDevice - code that returns information that is mostly device-specific
* DW.OSTimer - leaner, less complex implementation of a timer. Also does not rely on FMX, so can be used in an Android service

### Features

The Features folder contains more feature-rich cross-platform implementations of APIs. These are structured in such a way so as to shield your cross-platform code from the platform-specific implementation.
There are demos for each of the features in the Demos folder
Features include:

#### Biometric

Android and iOS implementations of biometric functions, namely: Fingerprint recognition on Android, and Fingerprint and FaceID on iOS

#### Connecitivity

Android and iOS implementations of code for checking network connectivity of a device

#### Firebase

Android and iOS implementations of Firebase Cloud Messaging using their native libraries

#### NFC

Near-Field Communication implementations for Android and iOS

#### Notifications

Framework that is alternative to TNotificationCenter (that ships with Delphi) for Notifications on Android and iOS

#### Speech Recognition

Android and iOS implementations for speech-to-text services. On Android, the actual speech API is used, rather than via Intents

### Planned Features

In addition to the features listed above, several other features are planned for Kastri, namely:

#### Apple Sign-In

From April 30th of this year, iOS apps on the App Store that use **any** social media based login must also offer Apple Sign-In. This implementation will use native APIs and conform to Apple's requirements

#### Camera

Native camera implementation for Android and iOS. Performance that is leagues above that of TCameraComponent that ships with Delphi. Uses the latest native APIs

#### Photos Library

For accessing photos stored on the device via the albums on Android and iOS. Photos selector implementation with multiple-select capability (similar to Facebook's implementation)

#### Universal Links

Allows users to intelligently follow links to content inside your app from your website. Implemented natively on iOS

The above features take time and resources to develop. Your help via [sponsorship](https://github.com/sponsors/DelphiWorlds) can help make them a reality!

## License

Kastri is licensed under MIT, and the license file is included in this folder.


![](https://tokei.rs/b1/github/DelphiWorlds/Kastri)

