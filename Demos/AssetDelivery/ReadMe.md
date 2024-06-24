# Asset Delivery Demo

## Description

Demonstrates the use of the [Play Asset Delivery](https://developer.android.com/guide/playcore/asset-delivery) feature in Kastri

## Supported Delphi versions

Delphi 12.x, Delphi 11.x

## Asset Packs

### Manual process (for a much easier method, see the [next section](#automating-building-of-asset-packs-using-codex))

In order to simplify the build process, you should organise asset packs starting with a root folder (in this case, the AssetPacks folder), and have subfolders for each of the packs. In this demo there are subfolders called InstallTime, FastFollow and OnDemand, representing each of the asset pack types

Under each asset pack subfolder, add a file called AndroidManifest.xml, using a specific format - this is an _example_:

```
  <manifest xmlns:android="http://schemas.android.com/apk/res/android" xmlns:dist="http://schemas.android.com/apk/distribution" 
    package="com.embarcadero.AssetDeliveryDemo" split="fastfollow_assetpack">
    <dist:module dist:type="asset-pack">
      <dist:fusing dist:include="true" />    
      <dist:delivery>
        <dist:fast-follow/>
      </dist:delivery>
    </dist:module>
  </manifest>
```

**The above is just an _example_, and for your asset pack will need to be modified as per these details:**

* The `package` attribute needs to match the package name of your application (package value in the Version Info of Project Options)
* The `split` attribute is the name of the asset pack which can be used when accessing files in the pack
* The `<dist:fast-follow/>` element designates the type of asset pack. Valid values are: `install-time`, `fast-follow` and `on-demand`

Under each asset pack subfolder, add a subfolder called `pack`. This is used as the base when asset pack modules are built. Under the `pack` folder, add a folder called `assets`, which will contain the assets (such as video files etc) for the pack. You can organise the files under `assets` into a folder structure if you wish, bearing in mind that you will need to take this structure into account when loading files at runtime.

Please use the example folders under `AssetPacks` in this demo as a reference

### Automating building of asset packs using Codex 

If you use [Codex](https://github.com/DelphiWorlds/Codex) 1.5.1 or higher, this process is made **substantially easier** by using Tools > Codex > Android Tools > Build Asset Packs, or by right-clicking the project in Project Manager and clicking the Build Asset Packs item.

If your project already has a subfolder called AssetPacks, Codex will automatically search the subfolders for valid asset packs and add them to the list:

<img src="https://raw.githubusercontent.com/DelphiWorlds/Codex/master/Screenshots/AssetPacks.png" alt="Asset Packs" height="400">

You can add and modify packs using the `Add` and `Edit` buttons:

<img src="https://raw.githubusercontent.com/DelphiWorlds/Codex/master/Screenshots/ManageAssetPack.png" alt="Manage Asset Pack" height="250">

After adding your assets under the `pack\assets` subfolder of the pack, you can follow **steps 1-4** of the [Build Procedure](#build-procedure) and then click the `Build` button to re-build the app with the asset packs:

<img src="https://raw.githubusercontent.com/DelphiWorlds/Codex/master/Screenshots/BuildAssetPacks.png" alt="Build Asset Packs" height="250">

Codex will automatically fill the `Keystore File` and `Alias` edits, however you will need to enter the passwords before clicking OK.

After Codex has re-built the application, you can use the `Install` button to install the application to your device for **local testing**.

## Play Core Library

In the demo, libraries have been added to the `Libraries` node of the `Android 32-bit` target in the project, namely:

```
  asset-delivery-2.2.2.jar
  play-core-common-2.0.3.jar
```

These provide Play Asset Delivery support to the application, and is used by the Kastri code to manage the asset packs

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will either need to apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` files to Android 64-bit does _not_ work)

## Manifest Changes

In order to support Play Asset Delivery, changes have been made to AndroidManifest.template.xml. The changes have been highlighted with comments in the XML, e.g:

```
<!-- ******* Additions to support Play Asset Delivery - START ******* -->
```

## Permissions

Play Asset Delivery requires the `Foreground Service` and `Foreground Service Data Sync` permissions, so these options are checked in the `Uses Permissions` section of the project

## Building the application

Delphi does not normally include asset packs, so there is a custom process for building the application

### Pre-requisites

If you are using the manual build process (as opposed to [using Codex](#automating-building-of-asset-packs-using-codex)), ensure that the path to the JDK you are using is in the `PATH` environment variable, e.g. `C:\Program Files\Eclipse Adoptium\jdk-17.0.9.9-hotspot\bin`

A useful tool for editing the `PATH` variable is [Rapid Environment Editor](https://www.rapidee.com) (which was written in Delphi)

### Build Procedure

When you are ready to test the asset pack features of your application:

1. Switch the build configuration to `Release`
2. Switch the deploy configuration to `Application Store`
3. Use `Project|Build (appname)` in the Delphi menu to build the application
4. Use `Project|Deploy (appname)` in the Delphi menu to create a deployment 

(**If you are using [Codex](https://github.com/DelphiWorlds/Codex) 1.5.1 or higher, see the [Automating building section](#automating-building-of-asset-packs-using-codex) for next steps** which are **not necessary** if using Codex)

5. Open a command line window and navigate to the `Scripts` folder of the demo
6. Execute `BuildBundle.bat` with parameters relevant to your project. The parameters for `BuildBundle.bat` are:

* `ProjectName` - the project name as it appears in the Project Manager for the Android platform, e.g. for this demo: `AssetDeliveryDemo`
* `TargetDir` - the folder that the IDE outputs the compiled results of the project in the selected configuration, e.g. for this demo: `(Kastri)\Demos\AssetDelivery\Android64\Release`
* `KeyStoreFile` - the keystore file that you use for the project (configured in the Provisioning section of the Project Options)
* `Alias` - the keystore alias
* `KeyPass` - the password for the key
* `StorePass` - the password for the keystore file
* `AssetPacksDir` - the path to the root of the asset pack folders, e.g. for this demo: `(Kastri)\Demos\AssetDelivery\AssetPacks`
* `SDKDir` - the root folder of the Android SDK being used
*`APILevel` - the API level to be used from the Android SDK. This will usually be the same API level as in the Android SDK configuration in the SDK Manager options in Delphi.

`BuildBundle.bat` compiles the asset packs into asset modules, uses `BundleTool` to combine them with the application, and then signs the bundle

### Install Procedure

(if you are using [Codex](https://github.com/DelphiWorlds/Codex) 1.5.1 or higher, see the [Automating building section](#automating-building-of-asset-packs-using-codex))

1. Open a command line window and navigate to the `Scripts` folder of the demo
2. Execute `InstallBundle.bat` with parameters relevant to your project. The parameters for `InstallBundle.bat` are:

* `ProjectName` - the project name as it appears in the Project Manager for the Android platform, e.g. for this demo: `AssetDeliveryDemo`
* `TargetDir` - the folder that the IDE outputs the compiled results of the project in the selected configuration, e.g. for this demo: `(Kastri)* \Demos\AssetDelivery\Android64\Release`
* `KeyStoreFile` - the keystore file that you use for the project (configured in the Provisioning section of the Project Options)
* `Alias` - the keystore alias
* `KeyPass` - the password for the key
* `StorePass` - the password for the keystore file
* `Serial` - the serial number of the Android device to install to. **This option can be omitted if there is only one device connected**.

`InstallBundle.bat` generates apks from the app bundle using the `--local-testing` flag so that the packs can be tested locally

### Run The Application

Once the app has been installed, run the application. In the demo, once the asset packs been "installed", there should be 2 files in the list box.


