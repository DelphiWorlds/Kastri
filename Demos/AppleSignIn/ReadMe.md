# Apple SignIn Demo

## Description

Demonstrates the use of the Apple SignIn feature

## Supported Delphi versions

Delphi 12, Delphi 11.x. It _should_ also work in Delphi 10.4.2, and perhaps earlier.

## Project configuration

In order to use this feature, you will need to:

1. Create an app id in Apple Developer (https://developer.apple.com/account/) and enable Signin With Apple, or use an app id that has this enabled
2. Create or use a Provisioning Profile that uses an app id where Signin With Apple is enabled

You will also need to add the AuthenticationServices framework to the iOS SDK. Please refer to:

  https://www.delphiworlds.com/2013/10/adding-other-ios-frameworks-to-the-sdk-manager/ 

When testing, if you need to revoke Apple Signin for the app, follow these steps:

1. On the device, go to Settings
2. At the very top, tap the Apple ID, iCloud, iTunes & App Store item
3. Tap: Password & Security
4. Tap: Apple ID Logins
5. Tap: the application you are testing
6. Tap: Stop using Apple ID



