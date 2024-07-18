# Symbol Scanner Demo

## Description

This demonstrates using the Symbol Scanner feature of Kastri. I have used the name Symbol because the devices I develop with have this brand, although the library now supports Honeywell

The feature applies to *Android only*, and only where devices have the EMDK installed, or is a Honeywell device. For EMDK devices, please refer to [this list](https://www.zebra.com/us/en/support-downloads/software/developer-tools/emdk-for-android.html).

## Supported Delphi versions

The demo should compile and work for at least versions 10.3.3, 10.4.x and 11.x. When you load the demo, please ensure that the Android libraries are correct, as outlined by [these instructions](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AndroidLibraries).

## Configuration

### Symbol support

There are 3 entries required in the Android manifest, and these are reflected in AndroidManifest.template.xml supplied with the demo:

This entry should be inserted just below the `<%uses-permission%>` entry in the manifest template:

`<uses-permission android:name="com.symbol.emdk.permission.EMDK" />`

This entry should be inserted just below the `<%uses-libraries%>` entry:

`<uses-library android:name="com.symbol.emdk" android:required="false"/>`

This entry should be inserted just inside the `<queries>` tag:

`<package android:name="com.symbol.emdk.emdkservice" />`

### Honeywell support

One entry is required in the Android manifest, and this is reflected in AndroidManifest.template.xml supplied with the demo:

This entry should be inserted just below the `<%uses-permission%>` entry in the manifest template:

`<uses-permission android:name="com.honeywell.decode.permission.DECODE" />`