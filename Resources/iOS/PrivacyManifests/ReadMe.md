# Privacy Manifest files for iOS

## Description

This folder contains `.xcprivacy` (Privacy Manifest) files that can be deployed with iOS apps, for compliance with [Apple's data privacy requirements](https://developer.apple.com/documentation/bundleresources/privacy_manifest_files?language=objc).

Files are named according to which third party SDKs are used, e.g.

```
PrivacyInfo.BasePlusFCM.xcprivacy
```

Provides data privacy information for a **basic** Delphi app that also uses Firebase Cloud Messaging. 

## Future changes in Delphi

When Delphi adds (perhaps some time in the future) other affected system APIs, the file would need to be modified accordingly. 

## Fixing missing entries

When uploading an app using Transporter, if the privacy manifest file is insufficient, it will issue an error (or errors) with the details of which APIs are being used that do not have entries in the privacy manifest. You can use these following links to determine what information is required:

* [Describing data use](https://developer.apple.com/documentation/bundleresources/privacy_manifest_files/describing_data_use_in_privacy_manifests?language=objc)

* [Describing usage reasons](https://developer.apple.com/documentation/bundleresources/privacy_manifest_files/describing_use_of_required_reason_api?language=objc)

If you have difficulty making required changes, you can use the issues section with a title starting with [Privacy], or [join the Delphi Worlds Slack workspace](https://slack.delphiworlds.com) and ask a question there.