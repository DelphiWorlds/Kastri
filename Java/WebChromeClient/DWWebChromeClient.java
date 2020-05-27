package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 *   Copyright 2020 Dave Nottage under MIT license     *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.content.Intent;
import android.net.Uri;
import android.webkit.MimeTypeMap;
import android.webkit.WebChromeClient;
import android.webkit.WebChromeClient.FileChooserParams;
import android.webkit.WebView;
import android.webkit.PermissionRequest;
import android.webkit.ValueCallback;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DWWebChromeClient extends WebChromeClient {

  private DWWebChromeClientDelegate mDelegate;
  private ValueCallback<Uri[]> mFilePathCallback;

  public DWWebChromeClient(DWWebChromeClientDelegate delegate) {
    mDelegate = delegate;
  }

  // https://stackoverflow.com/a/55449804/3164070
  private List<String> extractValidMimeTypes(String[] mimeTypes) {
    List<String> results = new ArrayList<String>();
    List<String> mimes;
    if (mimeTypes.length == 1 && mimeTypes[0].contains(",")) {
      mimes = Arrays.asList(mimeTypes[0].split(","));
    } else {
      mimes = Arrays.asList(mimeTypes);
    }
    MimeTypeMap mtm = MimeTypeMap.getSingleton();
    for (String mime : mimes) {
      if (mime != null && mime.trim().startsWith(".")) {
        String extensionWithoutDot = mime.trim().substring(1, mime.trim().length());
        String derivedMime = mtm.getMimeTypeFromExtension(extensionWithoutDot);
        if (derivedMime != null && !results.contains(derivedMime)) {
          // Adds valid mime type derived from the file extension
          results.add(derivedMime);
        }
      } else if (mtm.getExtensionFromMimeType(mime) != null && !results.contains(mime)) {
        // Adds valid mime type checked agains file extensions mappings
        results.add(mime);
      }
    }
    return results;
  }

  @Override
  public void onPermissionRequest(final PermissionRequest request) {
    request.grant(request.getResources());
  }

  // Gleaned from: https://github.com/anthonycr/Lightning-Browser/issues/253
  @Override
  public boolean onShowFileChooser(WebView webView, ValueCallback<Uri[]> filePathCallback, FileChooserParams fileChooserParams) {
    mFilePathCallback = filePathCallback;
    Intent intent = fileChooserParams.createIntent();
    List<String> validMimeTypes = extractValidMimeTypes(fileChooserParams.getAcceptTypes());
    if (!validMimeTypes.isEmpty())
      intent.setType(String.join(" ", validMimeTypes));
    return mDelegate.onFileChooserIntent(intent);
  } 

  public void handleFileChooserResult(Intent intent, int resultCode) {
    if (mFilePathCallback != null)
      mFilePathCallback.onReceiveValue(WebChromeClient.FileChooserParams.parseResult(resultCode, intent));
    mFilePathCallback = null;
  }
}