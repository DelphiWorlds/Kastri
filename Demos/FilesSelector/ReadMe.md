# Files Selector Demo

This demo shows an implementation for the equivalent of TFileOpenDialog, for Android and iOS

## Android

On Android, the "file names" returned are not actual file names, however they are references (URIs) that can be used to access the underlying file.

It is necessary to access the files using a [`ContentResolver`](https://developer.android.com/reference/android/content/ContentResolver), by calling the [`openInputStream`](https://developer.android.com/reference/android/content/ContentResolver#openInputStream(android.net.Uri)) method then dealing with the resulting [InputStream](https://developer.android.com/reference/java/io/InputStream). The `RawPath` member of `TSelectedFile` is a string respresentation of the URI.

In [`DW.Android.Helpers`](https://github.com/DelphiWorlds/Kastri/blob/master/Core/DW.Android.Helpers.pas) there is a method of `TAndroidHelperEx` called `ImportFile` that you can use to copy the selected file somewhere, e.g. in the demo code you could do this:

```
  TAndroidHelperEx.ImportFile(FSelector.SelectedFiles[AIndex].RawPath, TPath.Combine(TPath.GetDocumentsPath, FSelector.SelectedFiles[AIndex].DisplayName))
```

Where `AIndex` is the index of one of the files returned in `FSelector.SelectedFiles`

## iOS

On iOS, with the document picker it is not entirely obvious how to make multiple selections. The following screenshots should help:

<img src="./Screenshots/PickeriOSMenuButton.png" alt="Menu Button" height="750">
<br/>
<br/>
<img src="./Screenshots/PickeriOSMenuSelect.png" alt="Menu Select" height="750">
<br/>
<br/>
<img src="./Screenshots/PickeriOSSelectOpen.png" alt="Select Open" height="750">
