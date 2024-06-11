# Speech Recognition Demo

This demo allows you to record your speech and convert it into text

## Supported Delphi versions

The demo should compile and work for at least versions 10.3.3, 10.4.x and 11

When moving between Delphi versions, for Android please make sure you right-click the Libraries node under Target Platforms > Android (32 or 64-bit) and click "Revert System files to default"

## Supported Platforms

Supported platforms are iOS and Android

## Project Configuration

### Build Event/Android Manifest

**Delphi 12.1, when not [using Codex 2.3.1](../../Delphi12.1.AndroidManifestIssue.md)**

Due to changes in the Android build process:

* **Remove** the Build Events in Project Options for Android 32-bit and Android 64-bit 
* Deploy the project *at least once* - this will create `AndroidManifest.template.xml`
* Modify `AndroidManifest.template.xml` to add the following **inside of** `<queries>`

  ```
    <intent>
        <action android:name="android.speech.RecognitionService" />
    </intent>
  ```

**Delphi 12.0 or earlier:**

Configure Build Events in Project Options to add a Post-Build event with the command:  

```
  [kastri]\Tools\manifestmerge AndroidManifest.merge.xml $(Platform)\$(Config)\AndroidManifest.xml
```  
Where `[kastri]` is the path to the Kastri library. Do this for each required Android platform target (i.e. 32-bit and/or 64-bit)

## Compiling for iOS

You will need to add the Speech framework to the iOS SDK. There are some instructions on [how to add a framework here](https://delphiworlds.com/2013/10/adding-other-ios-frameworks-to-the-sdk-manager) (needs updating, however it should still work)
