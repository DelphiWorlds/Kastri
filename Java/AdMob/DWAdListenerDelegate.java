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

import com.google.android.gms.ads.LoadAdError;

public interface DWAdListenerDelegate { 

  void onAdClicked();
  void onAdClosed();
  void onAdFailedToLoad(LoadAdError adError);
  void onAdImpression();
  void onAdLoaded();
  void onAdOpened();

}