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
import com.google.android.gms.ads.interstitial.InterstitialAd;
import com.google.android.gms.ads.LoadAdError;

public interface DWInterstitialAdCallbackDelegate {

  void onAdLoaded(InterstitialAd interstitialAd);
  void onAdDismissedFullScreenContent();
  void onAdFailedToLoad(LoadAdError adError);
  void onAdFailedToShowFullScreenContent(AdError adError);
  void onAdShowedFullScreenContent();
  
}