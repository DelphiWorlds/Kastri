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

// import android.util.Log;
import com.google.android.gms.ads.AdListener;
import com.google.android.gms.ads.LoadAdError;

public class DWAdListener extends AdListener {

  private static final String TAG = "DWAdListener";
  private DWAdListenerDelegate mDelegate;

  public DWAdListener(DWAdListenerDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onAdClicked() {
    mDelegate.onAdClicked();
  }

  @Override
  public void onAdClosed() {
    mDelegate.onAdClosed();
  }

  @Override
  public void onAdFailedToLoad(LoadAdError adError) {
    mDelegate.onAdFailedToLoad(adError);
  }

  @Override
  public void onAdImpression() {
    mDelegate.onAdImpression();
  }

  @Override
  public void onAdLoaded() {
    mDelegate.onAdLoaded();
  }

  @Override
  public void onAdOpened() {
    mDelegate.onAdOpened();
  }

}