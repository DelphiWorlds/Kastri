package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2023 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import com.google.android.gms.ads.AdError;
import com.google.android.gms.ads.LoadAdError;
import com.google.android.gms.ads.rewarded.RewardedAd;

public interface DWRewardedAdLoadCallbackDelegate {

  void onAdDismissedFullScreenContent();
  void onAdFailedToLoad(LoadAdError adError);
  void onAdFailedToShowFullScreenContent(AdError adError);
  void onAdLoaded(RewardedAd rewardedAd);
  void onAdShowedFullScreenContent();
  
}