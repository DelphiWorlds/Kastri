package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2022 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

// import android.util.Log;
import androidx.annotation.NonNull;
import com.google.android.gms.ads.AdError;
import com.google.android.gms.ads.appopen.AppOpenAd;
import com.google.android.gms.ads.FullScreenContentCallback;
import com.google.android.gms.ads.LoadAdError;

public class DWAppOpenAdLoadCallback extends AppOpenAd.AppOpenAdLoadCallback {

  private static final String TAG = "DWAppOpenAdLoadCallback";
  private DWAppOpenAdLoadCallbackDelegate mDelegate;

  public DWAppOpenAdLoadCallback(DWAppOpenAdLoadCallbackDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onAdFailedToLoad(LoadAdError adError) {
    mDelegate.onAdFailedToLoad(adError);
  }

  public void onAdLoaded(@NonNull AppOpenAd appOpenAd) {
    appOpenAd.setFullScreenContentCallback(
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
    mDelegate.onAdLoaded(appOpenAd);
  }

}