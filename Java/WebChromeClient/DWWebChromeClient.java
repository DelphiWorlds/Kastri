package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2026 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.app.Activity;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.net.Uri;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.MimeTypeMap;
import android.webkit.WebChromeClient;
import android.webkit.WebChromeClient.FileChooserParams;
import android.webkit.WebView;
import android.webkit.PermissionRequest;
import android.webkit.ValueCallback;
import android.widget.FrameLayout;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DWWebChromeClient extends WebChromeClient {

  private static final int FULL_SCREEN_SETTING = View.SYSTEM_UI_FLAG_FULLSCREEN |
    View.SYSTEM_UI_FLAG_HIDE_NAVIGATION |
    View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION |
    View.SYSTEM_UI_FLAG_LAYOUT_STABLE |
    View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN |
    View.SYSTEM_UI_FLAG_IMMERSIVE;
  private Activity mActivity;
  private View mCustomView;
  private WebChromeClient.CustomViewCallback mCustomViewCallback;
  private DWWebChromeClientDelegate mDelegate;
  private ValueCallback<Uri[]> mFilePathCallback;
  private int mOriginalOrientation;
  private int mOriginalSystemUiVisibility;

  public DWWebChromeClient(DWWebChromeClientDelegate delegate) {
    mDelegate = delegate;
  }

  public DWWebChromeClient(DWWebChromeClientDelegate delegate, Activity activity) {
    mDelegate = delegate;
    mActivity = activity;
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
  public void onHideCustomView() {
    ((FrameLayout) mActivity.getWindow().getDecorView()).removeView(mCustomView);
    mCustomView = null;
    mActivity.getWindow().getDecorView().setSystemUiVisibility(mOriginalSystemUiVisibility);
    mActivity.setRequestedOrientation(mOriginalOrientation);
    mCustomViewCallback.onCustomViewHidden();
    mCustomViewCallback = null;
    mActivity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_USER);
  }

  @Override
  public void onPermissionRequest(final PermissionRequest request) {
    request.grant(request.getResources());
  }

  @Override
  public void onShowCustomView(View webView, WebChromeClient.CustomViewCallback viewCallback) {
    if (mActivity == null)
      return;
    if (mCustomView != null) {
      onHideCustomView();
      return;
    }
    mCustomView = webView;
    mOriginalSystemUiVisibility = mActivity.getWindow().getDecorView().getSystemUiVisibility();
    mOriginalOrientation = mActivity.getRequestedOrientation();
    mCustomViewCallback = viewCallback;
    ((FrameLayout) mActivity.getWindow().getDecorView())
      .addView(mCustomView, new FrameLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT));
    mActivity.getWindow().getDecorView().setSystemUiVisibility(FULL_SCREEN_SETTING);
    mActivity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_USER);
    mCustomView.setOnSystemUiVisibilityChangeListener(new View.OnSystemUiVisibilityChangeListener() {
      @Override
      public void onSystemUiVisibilityChange(int visibility) {
        updateControls();
      }
    }); // visibility -> updateControls() // Supported in Java 1.8+
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

  private void updateControls() {
    FrameLayout.LayoutParams params = (FrameLayout.LayoutParams) mCustomView.getLayoutParams();
    params.bottomMargin = 0;
    params.topMargin = 0;
    params.leftMargin = 0;
    params.rightMargin = 0;
    params.height = ViewGroup.LayoutParams.MATCH_PARENT;
    params.width = ViewGroup.LayoutParams.MATCH_PARENT;
    mCustomView.setLayoutParams(params);
    mActivity.getWindow().getDecorView().setSystemUiVisibility(FULL_SCREEN_SETTING);
  }
}