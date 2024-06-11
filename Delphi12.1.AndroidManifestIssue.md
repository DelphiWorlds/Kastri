# Delphi 12.1 Android Manifest Issue

This readme has been created to describe an issue that affects multiple projects in Kastri, related to `AndroidManifest.xml`.

## Build Events

Several demo projects in Kastri (such as [FCMRebooted](https://github.com/DelphiWorlds/Kastri/tree/master/Demos/FCMRebooted)) use Build Events to merge additional information into the resulting `AndroidManifest.xml` that is packaged into the Android app. This is done using the manifestmerge tool in the Tools folder of Kastri, for example the build event in the Project Options would look like this:

```
..\..\Tools\manifestmerge AndroidManifest.merge.xml $(Platform)\$(Config)\AndroidManifest.xml
```

This method is used because some of the Delphi-generated elements of AndroidManifest.xml need to be altered in order for the customizations that Kastri provides. For example in FCMRebooted, Kastri "overrides" the FirebaseMessagingService that Delphi provides, in order to work around a couple of issues, so the merge replaces that entry, rather than customizing `AndroidManifest.template.xml`

## The problem

In Delphi 12.1, the Android build system was changed so that `AndroidManifest.xml` is created when the app is **deployed** rather than at *compile time*, thus using Build Events for this purpose is no longer feasible.

Unfortunately, now there is no opportunity whatsoever (by traditional means) to alter what Delphi generates in `AndroidManifest.xml`. 

To compound the issue, Delphi 12.1 also uses the **generated** `AndroidManifest.xml` when packaging the application, rather than the **deployed** `AndroidManifest.xml`, so creating your own and adding that to the deployment in order to "override" it does not work, either.

## Solutions

### "Standard"

Without using any "trickery" (described below), these would be the steps to solve the issue:

1. Ensure you have all the relevant Project Options configured (Entitlement List, mostly)
2. Compile your app
3. Deploy the app at least once
4. Open `AndroidManifest.xml` from the project output folder into an editor
5. Copy all elements **inside** the `<application>` tag (but not including the `application` tag itself), **except** for the `<activity>` tag (which rarely needs to be modified)
6. Make a backup of `AndroidManifest.template.xml` in your project folder
7. Open the original `AndroidManifest.template.xml` into an editor, and replace the elements copied in step 5. This means *overwriting* all the "template" tags e.g. `<%application-meta-data%>`, `<%services%>` etc.
8. Make the customizations required for the feature(s) of Kastri you are using (by examining the `AndroidManifest.merge.xml` file) and save `AndroidManifest.template.xml`
9. Redeploy

As you might infer from these steps, it's a lot of messing around, however these steps would need repeating only if any of the relevant Android project options change.

### Using [Codex](https://github.com/DelphiWorlds/Codex)

Codex 2.3.1 uses some "trickery" to achieve what is essentially the same as for when Build Events worked. It monitors changes in the project output folder, and as soon as `AndroidManifest.xml` is updated or changed, it checks whether there is a `AndroidManifest.merge.xml` file in the project folder, and merges the changes from that. So far in testing, the merge completes before Delphi starts the packaging process, so the changes make it into the app.

Of course, this approach works only if you are deploying from the IDE, so it won't work if you are using an external build system.

## The Future

Apparently the build system for Android is going to be changed again in a [future version of Delphi](https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-775) (See Marco's comment). Hopefully, it will allow the opportunity to *easily* customize `AndroidManifest.xml`


