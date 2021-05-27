# Camera demo

## Existing camera support in Delphi

As many of you are probably aware, the existing camera support in Delphi is not very performant, especially on Android. Erik Van Bilsen of [Grijjy](https://blog.grijjy.com/) has [provided some hints and workarounds](https://quality.embarcadero.com/browse/RSP-10592) to increase performance, however it still has a way to go.

## Camera support in Kastri

Camera support in Kastri builds on the original standalone camera project, and adds support for iOS. On both platforms, instead of using the built-in camera app on devices, native APIs are used. Native controls are also used for displaying the camera preview, so the result is an huge improvement over the existing support in Delphi.

## Compiling for iOS

The camera support in Kastri is tied to the Vision framework from the iOS SDK. If you compile without importing the framework, you may receive this error:

`[DCC Error] E2597 ld: file not found: /System/Library/Frameworks/Vision.framework/Vision`

This is because the framework is not normally imported, so it will need to be imported manually, as per the [instructions in this article](https://delphiworlds.com/2013/10/adding-other-ios-frameworks-to-the-sdk-manager/).

## Some caveats

Please be aware that this is a “first release” of camera support. It provides basic turn on/off of the preview, capturing of a still image, and inclusion of location metadata. Other features will be added over time, including:

*   Face detection - some of the code is already present; it just needs to be finished/tested
*   Providing a list of supported resolutions
*   Flash mode on/off
*   Focus control

etc.

Also be aware that the Android support **requires Android 6.0 or greater**, and iOS support **requires iOS 10.0 or greater**.

## Known issues

When using Delphi 10.3.3 to compile, attempting to start the camera causes a crash. [This issue is being investigated.](https://github.com/DelphiWorlds/Kastri/issues/20)

When loading the main view in the demo using Delphi 10.3.3, you will receive warnings about missing properties in TLocationSensor. You can safely ignore these as they apply only to Delphi 10.4.1.

## Demo

The demo is dependent on the Kastri library. When you run the demo you may note that when it is in preview mode, there are controls that appear above the preview. These are image controls that use native APIs to display the images, which makes it possible for them to overlay the native preview control. At present, these controls are created at runtime, however the plan is to change them into components that can install into Delphi.

[![](https://i0.wp.com/delphiworlds.com/wp-content/uploads/2020/11/Screen-Shot-2020-11-06-at-1.01.39-am.png?resize=348%2C751&ssl=1)](https://i0.wp.com/delphiworlds.com/wp-content/uploads/2020/11/Screen-Shot-2020-11-06-at-1.01.39-am.png?ssl=1)

Switching the Include Location switch starts the location sensor, and will prompt for permissions. Tapping the Start button starts the camera preview. Tapping the Camera button in the middle at the bottom captures an image. Tapping the Swap Camera button in the bottom right changes between front and back cameras.

Once an image is captured, tapping the green Accept button in the top right takes you back to the start, and fills the image control with the captured image. Tapping the red Cancel button in the top right takes you back to the start without filling the image control.

## Capturing an image, and Including location metadata

When capturing a still image, the image data is returned in a stream, rather than a bitmap. This is so that metadata can be included, because bitmaps do not have metadata. If you wish to include location data in the image, set the IncludeLocation property of the TCamera instance to True, and before the still is captured, set the Location property to the current coordinates.

You can use Delphi’s TLocationSensor to obtain the users location. Remember that it takes a few seconds for the first location event to fire, start the sensor well before the user is able to capture an image. Please use the demo as a guide.

Remember also that when assigning the image stream to a bitmap, the location data will be lost in the bitmap, as it does not store it.

**Note:** For Delphi 10.4 and 10.4.1 users, if you wish to use location services on iOS, **you will need to [patch the System.iOS.Sensors unit as per this report](https://quality.embarcadero.com/browse/RSP-29859?focusedCommentId=85109&page=com.atlassian.jira.plugin.system.issuetabpanels:comment-tabpanel#comment-85109)**, in order for it to work properly.