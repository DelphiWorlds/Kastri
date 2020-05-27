package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 *   Copyright 2020 Dave Nottage under MIT license     *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.hardware.fingerprint.FingerprintManager;

public class DWFingerprintAuthenticationCallback extends FingerprintManager.AuthenticationCallback {

  static final String TAG = "DWFingerprintAuthenticationCallback";
  
  private DWFingerprintAuthenticationCallbackDelegate mDelegate;

	public DWFingerprintAuthenticationCallback(DWFingerprintAuthenticationCallbackDelegate delegate) {
	  this.mDelegate = delegate;
	}

	@Override
	public void onAuthenticationError(int errMsgId, CharSequence errString) {               
		mDelegate.onAuthenticationError(errMsgId, errString);
	}

	@Override
	public void onAuthenticationHelp(int helpMsgId, CharSequence helpString) {
		mDelegate.onAuthenticationHelp(helpMsgId, helpString);
	}

	@Override
	public void onAuthenticationFailed() {
		mDelegate.onAuthenticationFailed();
	}

	@Override
	public void onAuthenticationSucceeded(FingerprintManager.AuthenticationResult result) {
		mDelegate.onAuthenticationSucceeded(result);
	}

}