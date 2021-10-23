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












