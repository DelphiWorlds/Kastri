# Google SignIn Demo

## Description

A demo of the _very_ basic (so far) support in Kastri for Google SignIn on Android and iOS

## Supported Delphi versions

Delphi 13, Delphi 12.x. It *might* also work in earlier versions.

## Project Configuration

In order to use this demo, you will need to follow the steps at the ["Getting Started with the Google SignIn SDK"](https://github.com/grijjy/DelphiGoogleSignIn#getting-started-with-the-google-signin-sdk) section of Grijjy's readme. Note that at the time of writing, the Grijjy implementation is based on Delphi 10.2, and is a few years out of date, so please just focus on that section.

In `Unit1.pas` of the demo, replace the value for `cClientID` with the respective ClientID for Android. As per the Grijjy documentation, the Client ID value for Android is in the `google-services.json` file in the `oauth_client` member where `client_type` has a value of `3`. The Client ID value for iOS is at the Google Cloud Credentials page, in the OAuth 2.0 Client IDs section, where the name will be of the format:

```
iOS client for com.yourdomain.projectname (auto created by Google Service)`
```

where `com.yourdomain.projectname` is the identifier you used for the project in Firebase Console.

### Android

1. Deploy the project *at least once* - this will create `AndroidManifest.template.xml`
2. Modify `AndroidManifest.template.xml` to add the following *just before* `<%application-metadata%>`

   ```
   <meta-data android:name="com.google.android.gms.version" android:value="12451000" />
   ```

   ..add the following just *after* `<%services%>`:

   ```
   <service android:name="com.google.android.gms.auth.api.signin.RevocationBoundService"
      android:exported="true"
      android:permission="com.google.android.gms.auth.api.signin.permission.REVOCATION_NOTIFICATION"
      android:visibleToInstantApps="true" />
   ```

   ..add the following just *after* the `</application>` end tag:

   ```
     <activity android:name="com.google.android.gms.auth.api.signin.internal.SignInHubActivity"
       android:excludeFromRecents="true"
       android:exported="false"
       android:theme="@android:style/Theme.Translucent.NoTitleBar" />
   ```
3. In the Project Options, modify the `Package` value in the Version Info section to be the value specified in your Firebase Project
4. In the Project Manager, under the Android target, right-click the Libraries node and add (from the Kastri files):
   ```
   Lib\dw-google-signin.jar
   ThirdParty\Android\play-services-auth-19.2.0.jar
   ThirdParty\Android\play-services-auth-base-17.1.3.jar
   ```
   *These have already been added in the demo*.

### iOS

#### Firebase SDK

Delphi 12.2 has an updated linker, which means that newer iOS SDKs can now successfully be linked with Delphi code. Download the Firebase iOS SDK from one of these links:

* Delphi 12.2 - [Firebase iOS SDK 11.2.0](https://github.com/firebase/firebase-ios-sdk/releases/download/11.2.0/Firebase.zip)
* Delphi 12.1 and earlier - [Firebase iOS SDK 10.8.0](https://github.com/firebase/firebase-ios-sdk/releases/download/10.8.0/Firebase-10.8.0.zip)

..and unzip it somewhere, preferably in a folder that can be common to other projects that use the SDK.

In order to compile successfully for iOS, it's also necessary to:

1. Add [Swift Support Files in Delphi's SDK Manager](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSwiftSupport) (follow the link for instructions)
2. Add the following frameworks to the iOS SDK in Delphi's SDK Manager (if they are not already added):
   ```
   AuthenticationServices
   DeviceCheck
   ```
  
#### Project Options

1. For the `Framework search path` value, you can either:
   * Create an [Environment Variable User System Override](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Environment_Variables) called `Firebase`, and set it to the folder where the SDK was unzipped to. This corresponds to the `$(Firebase)` macro in the Project Options of the demo.
  
    **OR**
   * Modify the **`Framework Search path`** value in the Project Options from the demo to use the path to the root of the Firebase SDK.

2. In the Version Info section for iOS Device 64-bit, modify the `CFBundleIndentifier` value in the Version Info section to use the identifier that corresponds to your App ID

#### info.plist

Modify the `info.plist.TemplateiOS.xml` file in the root of your project. Inside the `<dict>` key (e.g. just below `<%StoryboardInfoPListKey%>`), add:
```xml
<key>GIDClientID</key>
<string>YOUR_CLIENT_ID</string>
<key>CFBundleURLTypes</key>
<array>
  <dict>
    <key>CFBundleURLSchemes</key>
    <array>
      <string>YOUR_CLIENT_ID_REVERSED</string>
    </array>
  </dict>
</array>
```
Where `YOUR_CLIENT_ID` is the client ID value mentioned [in the Grijjy instructions](https://github.com/grijjy/DelphiGoogleSignIn?tab=readme-ov-file#getting-started-with-the-google-signin-sdk), and `YOUR_CLIENT_ID_REVERSED` is the same value **REVERSED** e.g if you have a client ID of:
```
1234567890123-ojgronkhr7acm8adc3hacljfse19or88.apps.googleusercontent.com
```
the *reverse* will be:
```
com.googleusercontent.apps.1234567890123-ojgronkhr7acm8adc3hacljfse19or88
```








