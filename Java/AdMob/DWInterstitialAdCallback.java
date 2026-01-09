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

import androidx.annotation.NonNull;
import com.google.android.gms.ads.AdError;
import com.google.android.gms.ads.FullScreenContentCallback;
import com.google.android.gms.ads.interstitial.InterstitialAd;
import com.google.android.gms.ads.interstitial.InterstitialAdLoadCallback;
import com.google.android.gms.ads.LoadAdError;

public class DWInterstitialAdCallback extends InterstitialAdLoadCallback {

  private static final String TAG = "DWInterstitialAdCallback";
  private DWInterstitialAdCallbackDelegate mDelegate;

  public DWInterstitialAdCallback(DWInterstitialAdCallbackDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onAdFailedToLoad(LoadAdError adError) {
    mDelegate.onAdFailedToLoad(adError);
  }

  @Override
  public void onAdLoaded(@NonNull InterstitialAd interstitialAd) {
    mDelegate.onAdLoaded(interstitialAd);
    interstitialAd.setFullScreenContentCallback(
      new FullScreenContentCallback() {
        @Override
        public void onAdDismissedFullScreenContent() {
          // Called when fullscreen content is dismissed.
          // Make sure to set your reference to null so you don't
          // show it a second time.
          mDelegate.onAdDismissedFullScreenContent();
        }

        @Override
        public void onAdFailedToShowFullScreenContent(AdError adError) {
          // Called when fullscreen content failed to show.
          // Make sure to set your reference to null so you don't
          // show it a second time.
          mDelegate.onAdFailedToShowFullScreenContent(adError);
        }

        @Override
        public void onAdShowedFullScreenContent() {
          // Called when fullscreen content is shown.
          mDelegate.onAdShowedFullScreenContent();
        }
      });
   }
}