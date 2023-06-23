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

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import androidx.core.content.ContextCompat; 
import androidx.fragment.app.FragmentActivity;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;
import androidx.annotation.Nullable;
import androidx.biometric.BiometricPrompt;

public class DWBiometricFragmentActivity extends FragmentActivity {

  private static final String TAG = "DWBiometricFragmentActivity";
  private static final String ACTION_AUTHENTICATION = "ACTION_AUTHENTICATION";
  private static final String EXTRA_AUTHENTICATION_RESULT = "EXTRA_AUTHENTICATION_RESULT";
  private static final String EXTRA_AUTHENTICATION_ERROR_CODE = "EXTRA_AUTHENTICATION_ERROR_CODE";
  private static final String EXTRA_AUTHENTICATION_ERROR_MESSAGE = "EXTRA_AUTHENTICATION_ERROR_MESSAGE";
  private static final String EXTRA_PROMPT_ALLOW_DEVICE_CREDENTIAL = "EXTRA_PROMPT_ALLOW_DEVICE_CREDENTIAL";
  private static final String EXTRA_PROMPT_CANCEL_BUTTON_TEXT = "EXTRA_PROMPT_CANCEL_BUTTON_TEXT";
  private static final String EXTRA_PROMPT_DESCRIPTION = "EXTRA_PROMPT_DESCRIPTION";
  private static final String EXTRA_PROMPT_SUBTITLE = "EXTRA_PROMPT_SUBTITLE";
  private static final String EXTRA_PROMPT_TITLE = "EXTRA_PROMPT_TITLE";
  private static final String EXTRA_PROMPT_CONFIRMATION_REQUIRED = "EXTRA_CONFIRMATION_REQUIRED";
  private static final int AUTHENTICATION_RESULT_SUCCESS = 0;
  private static final int AUTHENTICATION_RESULT_ERROR = 1;
  private static final int AUTHENTICATION_RESULT_FAILED = 2;
  private boolean mIsStarted = false;

  public static void start(Context context, Intent intent) {
    intent.setClass(context, DWBiometricFragmentActivity.class);
    context.startActivity(intent);
  }

  private void notifyAuthenticationResult(int result, int errorCode, CharSequence errString) {
    Log.d(TAG, "Notifying authentication result");
    Intent intent = new Intent(ACTION_AUTHENTICATION);
    intent.putExtra(EXTRA_AUTHENTICATION_RESULT, result);
    if (result == AUTHENTICATION_RESULT_ERROR) {
      intent.putExtra(EXTRA_AUTHENTICATION_ERROR_CODE, errorCode);
      intent.putExtra(EXTRA_AUTHENTICATION_ERROR_MESSAGE, errString);
    }
    LocalBroadcastManager.getInstance(this).sendBroadcast(intent);
    finish();
  }

  private void authenticate() {
    Intent intent = getIntent();
    boolean canAllowDeviceCredential = intent.getBooleanExtra(EXTRA_PROMPT_ALLOW_DEVICE_CREDENTIAL, false);
    boolean confirmationRequired = intent.getBooleanExtra(EXTRA_PROMPT_CONFIRMATION_REQUIRED, false);
    BiometricPrompt.PromptInfo.Builder builder = new BiometricPrompt.PromptInfo.Builder()
      .setDescription(intent.getCharSequenceExtra(EXTRA_PROMPT_DESCRIPTION))
      .setSubtitle(intent.getCharSequenceExtra(EXTRA_PROMPT_SUBTITLE))
      .setTitle(intent.getCharSequenceExtra(EXTRA_PROMPT_TITLE))
      .setConfirmationRequired(confirmationRequired)
      .setDeviceCredentialAllowed(canAllowDeviceCredential);
    if (!canAllowDeviceCredential)
      builder.setNegativeButtonText(intent.getCharSequenceExtra(EXTRA_PROMPT_CANCEL_BUTTON_TEXT));
    BiometricPrompt prompt = new BiometricPrompt(this, ContextCompat.getMainExecutor(this), new BiometricPrompt.AuthenticationCallback() {
      @Override
      public void onAuthenticationError(int errorCode, CharSequence errString) {               
        Log.d(TAG, "onAuthenticationError");
        notifyAuthenticationResult(AUTHENTICATION_RESULT_ERROR, errorCode, errString);
      }

      @Override
      public void onAuthenticationFailed() {
        Log.d(TAG, "onAuthenticationFailed");
        notifyAuthenticationResult(AUTHENTICATION_RESULT_FAILED, 0, "");
      }

      @Override
      public void onAuthenticationSucceeded(BiometricPrompt.AuthenticationResult result) {
        Log.d(TAG, "onAuthenticationSucceeded");
        notifyAuthenticationResult(AUTHENTICATION_RESULT_SUCCESS, 0, "");
      }
    });
    Log.d(TAG, "Authenticating..");
    prompt.authenticate(builder.build());
  }

  @Override
  protected void onCreate(@Nullable Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
  }

  @Override
  public void onResume() {
    super.onResume();
    if (!mIsStarted) {
      mIsStarted = true;
      Log.d(TAG, "Activity started");
      if (getIntent() != null)
        authenticate();
    }
  }
}