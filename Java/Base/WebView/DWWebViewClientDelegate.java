  package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2024 Dave Nottage under MIT license  *
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
 
public interface DWWebViewClientDelegate {
	
  public void onPageFinished(WebView view, String url);
  public void onPageStarted(WebView view, String url, Bitmap favicon); 
  public void onReceivedError(WebView view, int errorCode, String description, String failingUrl);
  public void onReceivedHttpAuthRequest(WebView view, HttpAuthHandler handler, String host, String realm);
  public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error);
  public boolean shouldOverrideKeyEvent(WebView view, KeyEvent event);
  public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest request);
  public boolean shouldOverrideUrlLoading(WebView view, String url);

}