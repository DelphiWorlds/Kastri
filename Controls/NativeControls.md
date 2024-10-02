# Kastri Native Controls

## Purpose

These controls have been created to support proper z-order with other "native" controls supplied with Delphi, such as TMapView and TWebBrowser, i.e. they can overlay the other native controls

Note that at present, most of the controls will function only on Android and iOS, however the controls are rendered in the designer close to what they will look like on the target platform.

<br>

## Installation

Simply open the `KastriFMX.dproj` project in the folder under the `Packages` folder and Compile and Install. The components will appear in the Kastri FMX section on the Component Pallette

<br>

## Demo

A very basic demo is in the [`Demos\NativeControls`](../Demos/NativeControls) folder.

## Controls

### TNativeSlider

TNativeSlider is implemented as [SeekBar](https://developer.android.com/reference/android/widget/SeekBar) on Android (with the added bonus of being able to use it vertically as well as horizontal), and as a [UISlider](https://developer.apple.com/documentation/uikit/uislider) on iOS

**NOTE:** It was necessary to create a descendant of the Java class (in order to implement a vertical SeekBar), so you will need to add the Kastri base jar to the Libraries node under the Android 32-bit platform (it will compile for both 32-bit and 64-bit) in Project Manager in your project. Which `.jar` file to add will depend on the version of Delphi you have:

* Delphi 11.x: `dw-kastri-base-2.0.0.jar`
* Delphi 12.x: `dw-kastri-base-3.0.0.jar`


### TNativeButton

TNativeButton is implemented as [Button](https://developer.android.com/reference/android/widget/Button) on Android, and as [UIButton](https://developer.apple.com/documentation/uikit/uibutton?language=objc) on iOS. 

At present, TNativeButton provides only very basic support, e.g. the various states, titles and image support for UIButton are yet to be supported.


### TNativeEllipse

TNativeEllipse is like TEllipse in FMX

Both TNativeRectangle and TNativeEllipse use [ImageView](https://developer.android.com/reference/android/widget/ImageView) and [GradientDrawable](https://developer.android.com/reference/android/graphics/drawable/GradientDrawable) on Android, and [CAShapeLayer](https://developer.apple.com/documentation/quartzcore/cashapelayer?language=objc) and [UIBezierPath](https://developer.apple.com/documentation/uikit/uibezierpath?language=objc) on iOS for rendering the shape

The Stroke property has very basic support - only Color, Kind (`Solid` or `None`) and Thickness are currently supported

### TNativeImage

TNativeImage is implemented as [ImageView](https://developer.android.com/reference/android/widget/ImageView) on Android and [UIImageView](https://developer.apple.com/documentation/uikit/uiimageview?language=objc) on iOS.

It has a Text property that can serve as a caption that overlays the image.

Note: On **Android**, in order to use the `LoadFromFile` method, you will need to enable **Secure File Sharing** in the Entitlements section of Project Options

### TNativeRectangle

TNativeRectangle is like TRectangle in FMX. At present, on iOS rounded corners are not supported (however this is planned to be supported in the future).






