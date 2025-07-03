# Speech Recognition Demo

This demo allows you to record your speech and convert it into text

## Supported Delphi versions

The demo should compile and work for at least versions 11.x and 12.x

When moving between Delphi versions, for Android please make sure you right-click the Libraries node under Target Platforms > Android (32 or 64-bit) and click "Revert System files to default"

## Supported Platforms

Supported platforms are iOS and Android

## Project Configuration

### Android Manifest

* Deploy the project *at least once* - this will create `AndroidManifest.template.xml`
* Modify `AndroidManifest.template.xml` to add the following **inside of** `<queries>`

  ```
    <intent>
        <action android:name="android.speech.RecognitionService" />
    </intent>
  ```

NOTE: This change is already done for the demo.

## Compiling for iOS

You will need to add the Speech framework to the iOS SDK. There are some instructions on [how to add a framework here](https://delphiworlds.com/2013/10/adding-other-ios-frameworks-to-the-sdk-manager) (needs updating, however it should still work)
