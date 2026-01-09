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

import android.graphics.Bitmap;
import android.net.http.SslError;
import android.view.KeyEvent;
import android.webkit.HttpAuthHandler;
import android.webkit.SslErrorHandler;
import android.webkit.WebResourceRequest;
import android.webkit.WebView;
import android.webkit.WebViewClient;

public class DWWebViewClient extends WebViewClient {

  private DWWebViewClientDelegate mDelegate;

  public DWWebViewClient(DWWebViewClientDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onPageFinished(WebView view, String url) {
    mDelegate.onPageFinished(view, url);
  }

  @Override
  public void onPageStarted(WebView view, String url, Bitmap favicon) {
    mDelegate.onPageStarted(view, url, favicon);
  }

  @Override
  public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
    mDelegate.onReceivedError(view, errorCode, description, failingUrl);
  }

  @Override
  public void onReceivedHttpAuthRequest(WebView view, HttpAuthHandler handler, String host, String realm) {
    mDelegate.onReceivedHttpAuthRequest(view, handler, host, realm);
  }

  @Override
  public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
    mDelegate.onReceivedSslError(view, handler, error);
    // https://github.com/DelphiWorlds/Kastri/issues/268
    handler.cancel();
  }

  @Override
  public boolean shouldOverrideKeyEvent(WebView view, KeyEvent event) {
    return mDelegate.shouldOverrideKeyEvent(view, event);
  }

  @Override
  public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request) { 
	  return mDelegate.shouldOverrideUrlLoading(view, request);
  }

  @Override
  public boolean shouldOverrideUrlLoading(WebView view, String url) { 
		return mDelegate.shouldOverrideUrlLoading(view, url);
  }
}
