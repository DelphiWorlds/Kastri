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

import com.google.android.gms.ads.AdError;
import com.google.android.gms.ads.appopen.AppOpenAd;
import com.google.android.gms.ads.LoadAdError;

public interface DWAppOpenAdLoadCallbackDelegate { 

  void onAdDismissedFullScreenContent();
  void onAdFailedToLoad(LoadAdError adError);
  void onAdFailedToShowFullScreenContent(AdError adError);
  void onAdLoaded(AppOpenAd appOpenAd);
  void onAdShowedFullScreenContent();

}