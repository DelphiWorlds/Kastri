# WebBrowserExt demo

## Description

Demonstrates some of the usage of `TWebBrowserExt`, found in the `Features\WebBrowserExt` folder.

**NOTE**: This demo/feature is work in progress, and _may change substantially_. Also, some features may or may not be present on each platform.

## Features

* Allows interception of downloads so that they can be controlled programmatically
* Properly asynchronous CaptureBitmap method - the default FMX code relies on `Application.ProcessMessages` to wait for the request to complete.
* ExecuteJavaScript method for asynchronously executing JavaScript and returning a result
* Convenient GetElementValueByName method that returns the value for the given element name
* GetPrintAdapter method (currently Android and iOS) that returns a reference to a native class that can be used with printing support in Kastri
* FlushCookies with optional parameter for removing all cookies (currently Android and iOS)
* Control over zooming (currently Android only)
* OnElementClick (currently Android, iOS, macOS) for intercepting clicks on elements
* JavaScript event handling (demo of this coming soon)

### Interception of downloads

`TWebBrowserExt` has the following properties/events to allow control over downloading of files - presently supported on **iOS/macOS and Android**:

* DefaultDownloadsFolder - property for indicating the default folder for downloads to be saved in. If the filename returned in `OnDownloadStart` does not include a folder, this one will be used. If this property is blank, the system default will be used. On mobile, this value defaults to: `<Documents>\Downloads`, otherwise: `<SharedDocuments>\<AppName>\Downloads`.
* OnDownloadStart - event invoked when a downloadable link is clicked. Passes the full uri of the link, a suggested file extension, and a variable to return the filename, which can include the desired path if the app has rights to access it
* OnDownloadStateChange - event invoked when a download state changes, such as when a download is complete. The full path of the download is included in the `FileName` parameter 

## Known Issues

**NOTE: The issue below has been fixed in Delphi 12**

There is an outstanding [issue in Delphi 11.x for `TWebBrowser` that affects the Windows platform](https://quality.embarcadero.com/browse/RSP-38165). For TWebBrowserExt to work on Windows, please follow these steps:

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

As per my comment dated 19th April 2023 on the report, the other parts of the issue have been fixed.
