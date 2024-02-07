# Google SignIn Demo

## Description

A demo of the _very_ basic (so far) support in Kastri for Google SignIn on Android and iOS

## Supported Delphi versions

Delphi 12, Delphi 11.x. It _should_ also work in Delphi 10.4.2, and perhaps earlier.

## Project Configuration

In order to use this demo, you will need to follow the steps at the ["Getting Started with the Google SignIn SDK"](https://github.com/grijjy/DelphiGoogleSignIn#getting-started-with-the-google-signin-sdk) section of Grijjy's readme. Note that at the time of writing, the Grijjy implementation is based on Delphi 10.2, and is a few years out of date, so please just focus on that section.

In `Unit1.pas` of the demo, replace the values for `cClientID` with the respective ClientID for Android and iOS. As per the Grijjy documentation, the Client ID value for Android is in the `google-services.json` file in the `oauth_client` member where `client_type` has a value of `3`. The Client ID value for iOS is at the Google Cloud Credentials page, in the OAuth 2.0 Client IDs section, where the name will be of the format:

```
iOS client for com.yourdomain.projectname (auto created by Google Service)`
```

where `com.yourdomain.projectname` is the identifier you used for the project in Firebase Console.

### Android (32 bit and 64 bit where required)

* In the Project Options, modify the `Package` value in the Version Info section to be the value specified in your Firebase Project

### iOS

1. Go to [this link](https://github.com/firebase/firebase-ios-sdk/releases/tag/v8.15.0), download Firebase.zip, and extract it somewhere convenient
2. In the IDE Options > Environment Variables, create a user override called `Firebase` which points to the unzipped SDK, **or** modify `Framework search path` in the Project Options for iOS Device 64-bit to point to the unzipped SDK
3. In the Project Options for iOS Device 64-bit, modify the `CFBundleIndentifier` value in the Version Info section to use the identifier that corresponds to your App ID
4. Modify `info.plist.TemplateiOS.xml` to replace the string value inside the array for the `CFBundleURLSchemes` key with your iOS ClientID **however in REVERSE notation**. (See the example in the demo files)
5. Add the following iOS SDK frameworks as per [these instructions](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSDKFrameworks#readme):
   
   AuthenticationServices


## Integration into your own project

* Please follow the steps as per the [Configuration](#configuration) section
* In the Project Manager, under the Android target, right-click the Libraries node and add (from the Kastri files):
    ```
    Lib\dw-google-signin.jar
    ThirdParty\Android\play-services-auth-19.2.0.jar
    ThirdParty\Android\play-services-auth-base-17.1.3.jar
    ```
* In the Project Options, ensure that you have a Post Build event for merging the manifest, as per the demo. You may need to modify the command to suit your file locations.
* In the Project Options, ensure that the search path for **all** platforms includes folders from the Kastri repo _as per the demo_
* In the Project Options, ensure that the `Framework search path` for iOS Device 64-bit includes the Firebase SDK paths _as per the demo_

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)






