# WebBrowserExt demo

## Description

Demonstrates some of the usage of `TWebBrowserExt`, found in the `Features\WebBrowserExt` folder.

**NOTE**: This demo/feature is work in progress, and _may change substantially_. Also, some features may or may not be present on each platform.

## Features

* Properly asynchronous CaptureBitmap method - the default FMX code relies on `Application.ProcessMessages` to wait for the request to complete.
* ExecuteJavaScript method for asynchronously executing JavaScript and returning a result
* Convenient GetElementValueByName method that returns the value for the given element name
* GetPrintAdapter method (currently Android and iOS) that returns a reference to a native class that can be used with printing support in Kastri (coming soon)
* FlushCookies with optional parameter for removing all cookies (currently Android and iOS)
* Control over zooming (currently Android only)
* OnElementClick (currently Android, iOS, macOS) for intercepting clicks on elements
* JavaScript event handling (demo of this coming soon)

## Known Issues

There is an outstanding [issue for `TWebBrowser` that affects the Windows platform](https://quality.embarcadero.com/browse/RSP-38165). For TWebBrowserExt to work on Windows, please follow these steps:

1. Make a copy of `FMX.WebBrowser.Win.pas` from the `fmx` folder in the Delphi source and put it somewhere in the project's search path
2. Modify the `TWinNativeWebBrowser.QueryInterface` method to look like this:

    ```
    function TWinNativeWebBrowser.QueryInterface(const IID: TGUID; out Obj): HResult;
    begin
      Result := inherited QueryInterface(IID, Obj);
      if (FWebView <> nil) and (Result <> S_OK) then
      begin
        ICoreWebView2(Obj) := FWebView;
        Result := S_OK;
      end;
    end;
    ```

As per my comment dated 19th April 2023, the other parts of the issue have been fixed.

