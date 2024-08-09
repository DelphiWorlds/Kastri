# Printing demo

## Description

This demonstrates the use of [`Printing`](https://github.com/DelphiWorlds/Kastri/tree/master/Features/Printing).

## Supported Delphi versions

Delphi 12

## Usage

To print a file of a supported type:

```
  Printing.Print(AFileName);
```

Where AFileName is a file of the supported types, which are:

* Android: PDF, or any file that can be printed via an instance of [WebView](https://developer.android.com/reference/android/webkit/WebView), which is what Delphi uses as the underlying Android class for `TWebBrowser`
* iOS: PDF or any image supported by the [ImageIO framework](https://developer.apple.com/documentation/imageio?language=objc), which currently includes: `BMP`, `GIF`, `HEIF`, `JPEG`, `PNG`, `TIFF`, `WebP`

Printing also supports passing an adapter of the supported types:

* Android: A descendant of [PrintDocumentAdapter](https://developer.android.com/reference/android/print/PrintDocumentAdapter?hl=en)
* iOS: An descendant of [UIPrintFormatter](https://developer.apple.com/documentation/uikit/uiprintformatter?language=objc)

e.g.

```
  Printing.Print(AAdapter);
```

For an example of how this is used, please refer to the [WebBrowserExt demo/feature](https://github.com/DelphiWorlds/Kastri/blob/18925445235701eb65d1d60efb563bbfd779e9e7/Demos/WebBrowserExt/Unit1.pas#L178)
