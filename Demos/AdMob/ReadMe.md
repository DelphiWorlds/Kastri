# AdMob Demo

## Description

Advertising implementation, specifically for AdMob (at present)

## Project Configuration

### Component

**Please install the TAdMobBannerAd component first** (before even loading the demo)

The package can be found [here](https://github.com/DelphiWorlds/Kastri/tree/main/Packages/D110) 

### Libraries

If creating your own project, you will need to add the [`dw-admob.jar`](https://github.com/DelphiWorlds/Ksstri/blob/master/Lib/dw-admob.jar) file to the Libraries node under the Android platform in Project Manager

When using Delphi 10.4.x, you will need to add other libraries and disable some default libraries, as per this screenshot:

<img src="./Screenshots/D104ProjectManager.png" alt="PM" height="500">

### Android Entitlements

Ensure your project has the `AdMob Service` enabled. This adds Google Play services metadata and the Ads activity to the manifest.

### Android Permission

Ensure your project has the `Access Network State` permission in Project Options

### Using non-test Ad UnitIds

When configuring your app for non-test Ad UnitIds, you will need to:

* For Android: modify `AndroidManifest.template.xml` to add meta-data that includes your AdMob Application ID, as per part 3 of [these instructions](https://developers.google.com/admob/android/quick-start?hl=en-US#import_the_mobile_ads_sdk). Please refer to the image below for an example of where to put the meta-data. 
  
<img style="margin-left: 3em;" src="./Screenshots/AndroidManifestTemplateAppId.png" alt="PM" height="300"/>

* For iOS: modify `info.plist.TemplateiOS.xml` to change the value for the `GADApplicationIdentifier` key. Please refer to the image below for an example of what to change. 
  
<img style="margin-left: 3em;" src="./Screenshots/iOSInfoPListTemplateAppId.png" alt="PM" height="150"/>

* Ensure that the `TestMode` property of the instances of AdMob classes is set to False, and the AdUnitId is set to a valid AdUnitId for your application.

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

## Delphi 10.4.x support

A [package has been created for Delphi 10.4.x](https://github.com/DelphiWorlds/Kastri/tree/main/Packages/D104), as well as a [demo](https://github.com/DelphiWorlds/Kastri/tree/main/Demos/AdMob/D104). Only RewardedAd has had very basic testing. **It is possible that something else might be broken, as some jars needed to be disabled.**


