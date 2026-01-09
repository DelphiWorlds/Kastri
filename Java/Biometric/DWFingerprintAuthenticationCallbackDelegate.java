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

import android.hardware.fingerprint.FingerprintManager;

public interface DWFingerprintAuthenticationCallbackDelegate {

  void onAuthenticationError(int errMsgId, CharSequence errString);

  void onAuthenticationHelp(int helpMsgId, CharSequence helpString);

  void onAuthenticationFailed();
  
  void onAuthenticationSucceeded(FingerprintManager.AuthenticationResult result);
    
}