# Biometric Demo

## Description

Implementation of Biometric authentication for Android and iOS

For Android on Delphi 10.4.x, it makes use of [FingerprintManager](https://developer.android.com/reference/android/hardware/fingerprint/FingerprintManager) (now deprecated)

For Android on Delphi 11 and above, it makes use of [BiometricManager](https://developer.android.com/reference/android/hardware/biometrics/BiometricManager). There is _no support for Delphi 10.4.x using BiometricManager_ because it requires the use of [AndroidX](https://developer.android.com/jetpack/androidx), which is not supported in Delphi 10.4.x 

## Biometric Prompt properties

The properties prefixed with `Prompt` (e.g. `PromptDescription`) apply only to **Delphi 11 and above**, and to **Android** only. 

These properties determine what is displayed when the prompt appears, and whether or not confirmation is required (`PromptConfirmationRequired`).

## KeyName property

This property applies only to **Delphi 10.4.x**, to **Android** only, and defaults to the last part of the package name (e.g. `BiometricDemo` in `com.delphiworlds.BiometricDemo`), so you will likely never need to set this property.

## Configuration in your own project

These instructions apply to when you wish to use Biometric support in your own project (i.e. not the demo)

### Dependent jars (Android only)

For Delphi 11.x, you will need to add the dependent jar libraries and disable the corresponding older versions, found in the `Libraries` node under the Android 32-Bit platform target in the Delphi Project Manager.

The jars to be disabled, are:

* fragment-1.0.0.dex.jar
* lifecycle-common-2.0.0.dex.jar
* lifecycle-livedata-2.0.0.dex.jar
* lifecycle-livedata-core-2.0.0.dex.jar
* lifecycle-runtime-2.0.0.dex.jar
* lifecycle-viewmodel-2.0.0.dex.jar

To disable these right-click each jar and click `Disable`

The jars to be added from the `ThirdParty\Android` folder are:

* androidx-activity-1.3.1.jar
* androidx-appcompat-1.0.2.jar
* androidx-biometric-1.1.0.jar
* androidx-savedstate-1.1.0.jar
* lifecycle-common-2.3.1.jar
* lifecycle-livedata-2.3.1.jar
* lifecycle-livedata-core-2.3.1.jar
* lifecycle-runtime-2.3.1.jar
* lifecycle-viewmodel-2.2.0.jar
* lifecycle-viewmodel-savedstate-2.3.1.jar

The jars to be added from the `Lib` folder are:

* dw-biometric-2.0.0.jar
* dw-kastri-base-2.0.0.jar

To add these, right click the `Libraries` node and click `Add..`, then select the relevant jar file(s)

** NOTE: If you use the `Revert System Files to Default` option (for whatever reason), you will need to re-disable the jars that need to be disabled. **

For Delphi 10.4.x, you will only need to add:

* dw-biometric.jar

### Build Events (Delphi 11.x and Android only, for both 32-bit and 64-bit)

The Android manifest needs additional entries, and the demo project has a Post-Build event to achieve this:

`..\..\Tools\manifestmerge AndroidManifest.merge.xml $(Platform)\$(Config)\AndroidManifest.xml`

As per this command, `manifestmerge` is found in the `Tools` folder of Kastri. It merges the entries found in the `AndroidManifest.merge.xml` file (in the root of the demo project), with the `AndroidManifest.xml` file which Delphi generates when compiling the project. If your output folder is different from the default (`$(Platform)\$(Config)`) you will need to change this value.










