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

import android.content.Context;
import android.content.Intent;
import android.util.Log;

import androidx.annotation.NonNull;

import com.google.android.gms.auth.api.signin.GoogleSignIn;
import com.google.android.gms.auth.api.signin.GoogleSignInAccount;
import com.google.android.gms.auth.api.signin.GoogleSignInClient;
import com.google.android.gms.auth.api.signin.GoogleSignInOptions;
import com.google.android.gms.common.api.ApiException;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.tasks.OnCompleteListener;
import com.google.android.gms.tasks.Task;

public class DWGoogleSignIn {

  private static final String TAG = "DWGoogleSignin";
  public static final int RC_SIGN_IN = 9001;

  private DWGoogleSignInDelegate mDelegate;
  private GoogleSignInClient mClient;
  private Context mContext;

  public DWGoogleSignIn(Context context, DWGoogleSignInDelegate delegate) {
    mContext = context;
    mDelegate = delegate;
  }

  public void setupClient(String clientId, String[] scopes, boolean requestServerAuthCode) {
    GoogleSignInOptions.Builder builder = new GoogleSignInOptions.Builder(GoogleSignInOptions.DEFAULT_SIGN_IN)
      .requestIdToken(clientId)
      .requestEmail();
    for (String scope: scopes)
      builder.requestScopes(new Scope(scope));
    if (requestServerAuthCode)
      builder.requestServerAuthCode(clientId, false);
    mClient = GoogleSignIn.getClient(mContext, builder.build());
  }

  private void signInComplete(Task<GoogleSignInAccount> task, String action) {
    try {
      GoogleSignInAccount account = task.getResult(ApiException.class);
      mDelegate.signInComplete(account, 0, "");           
    } catch (ApiException e) {
        // The ApiException status code indicates the detailed failure reason.
        // Please refer to the GoogleSignInStatusCodes class reference for more information.
        mDelegate.signInComplete(null, e.getStatusCode(), e.getMessage());
        Log.w(TAG, action + " - failed code: " + e.getStatusCode());
    } 
  }

  public void signIn() {
    Intent signInIntent = mClient.getSignInIntent();
    mDelegate.startActivityForResult(signInIntent, RC_SIGN_IN);
  }

  public void signOut() {
    mClient.signOut().addOnCompleteListener(new OnCompleteListener<Void>() {
      @Override
      public void onComplete(@NonNull Task<Void> task) {
        mDelegate.signOutComplete();
      }
    });
  }

  public void silentSignIn() {
    Task<GoogleSignInAccount> task = mClient.silentSignIn();
    if (task.isSuccessful())
      signInComplete(task, "silentSignIn");
    else {
      task.addOnCompleteListener(new OnCompleteListener<GoogleSignInAccount>() {
        @Override
        public void onComplete(@NonNull Task<GoogleSignInAccount> task) {
          signInComplete(task, "silentSignIn");
        }
      });
    }
  }

  public void revokeAccess() {
    GoogleSignInClient client = GoogleSignIn.getClient(mContext, GoogleSignInOptions.DEFAULT_SIGN_IN);
    client.revokeAccess().addOnCompleteListener(new OnCompleteListener<Void>() {
      @Override
      public void onComplete(@NonNull Task<Void> task) {
        mDelegate.revokeAccessComplete();
      }
    });
  }

  public void handleSignInResult(Intent data) {
    Task<GoogleSignInAccount> task = GoogleSignIn.getSignedInAccountFromIntent(data);
    signInComplete(task, "handleSignInResult");
  }
}