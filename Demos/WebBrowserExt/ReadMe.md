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
* JavaScript message handling (Android ONLY)

## Supported Delphi versions

Delphi 12.3, Delphi 13.x (May also work in earlier versions of Delphi 12)

## Project Configuration

When using `TWebBrowserExt` in your own code, you will need to add `dw-kastri-base-3.0.0.jar` (from the `Lib` folder in Kastri) to the project (this is already done in the demo). 

To do so, in Project Manager expand the Android 32-bit target (it will still be included when compiling for 64-bit), right click the `Libraries` node, click `Add..`, select the file and click `Open`.

## JavaScript message handling 

**Requires the androidx webkit .jar file - see below**

For Android ONLY, `TWebBrowserExt` supports messages from JavaScript in the web page via the `SetWebListenerParams` method and the `OnStringMessagePosted` event.

In your hosted (or file-based) web page, a message can be sent using JavaScript like this:

```javascript
  function sendMessage(data) {
    if (window.WBExt && window.WBExt.postMessage)
      window.WBExt.postMessage(data);
  }  
```

Here, `WBExt` is a JavaScript object created when `SetWebListenerParams` is called. e.g. In the Delphi code:

```delphi
  FWebBrowserExt.SetWebListenerParams('WBExt', ['https://delpiworlds.com']);
```

The second parameter is a list of allowed origins which must be in a valid scheme format, e.g. `file:///` for local files, `https://*.example.com`, which matches any sub-domain (and ONLY subdomains) e.g. `https://app.example.com`.

The underlying code adds a web message listener using the JavaScript object name and the allowed origins. 

When a message is posted from the web page via `postMessage`, the data is received by `TWebBrowserExt`, and calls the `OnStringMessagePosted` event. The string data in the message is passed to this event, and if a reply is required, it the `Reply` parameter of the event handler should be populated.

To receive a reply in the web page, the `onmessage` handler of the JavaScript object needs to be set, e.g.

```javascript
  document.addEventListener("DOMContentLoaded", () => {
    if (window.WBExt) {
      window.WBExt.onmessage = function(event) {
        console.log('Reply from Android: ', event.data)
      }
    }
  });
```

The JavaScript message handling support requires adding the androidx webkit .jar file to the Delphi project. At time of writing, this is `webkit-1.15.0.jar`, which can be found in the `ThirdParty\Android` folder of Kastri. 

To do so, in Project Manager expand the Android 32-bit target (it will still be included when compiling for 64-bit), right click the `Libraries` node, click `Add..`, select the file and click `Open`.

## Interception of downloads

`TWebBrowserExt` has the following properties/events to allow control over downloading of files - presently supported on **iOS/macOS and Android**:

* DefaultDownloadsFolder - property for indicating the default folder for downloads to be saved in. If the filename returned in `OnDownloadStart` does not include a folder, this one will be used. If this property is blank, the system default will be used. On mobile, this value defaults to: `<Documents>\Downloads`, otherwise: `<SharedDocuments>\<AppName>\Downloads`.
* OnDownloadStart - event invoked when a downloadable link is clicked. Passes the full uri of the link, a suggested file extension, and a variable to return the filename, which can include the desired path if the app has rights to access it
* OnDownloadStateChange - event invoked when a download state changes, such as when a download is complete. The full path of the download is included in the `FileName` parameter 

