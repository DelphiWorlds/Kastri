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
import com.google.android.gms.ads.AdError;
import com.google.android.gms.ads.FullScreenContentCallback;
import com.google.android.gms.ads.LoadAdError;
import com.google.android.gms.ads.rewarded.RewardedAd;
import com.google.android.gms.ads.rewarded.RewardedAdLoadCallback;

public class DWRewardedAdLoadCallback extends RewardedAdLoadCallback {

  private static final String TAG = "DWRewardedAdLoadCallback";
  private DWRewardedAdLoadCallbackDelegate mDelegate;
  
  public DWRewardedAdLoadCallback(DWRewardedAdLoadCallbackDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onAdFailedToLoad(LoadAdError adError) {
    mDelegate.onAdFailedToLoad(adError);
  }

  @Override
  public void onAdLoaded(RewardedAd rewardedAd) {
    rewardedAd.setFullScreenContentCallback(
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
    mDelegate.onAdLoaded(rewardedAd);
   }
}