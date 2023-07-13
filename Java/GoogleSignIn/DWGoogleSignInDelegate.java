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

import android.content.Intent;

import com.google.android.gms.auth.api.signin.GoogleSignInAccount;

public interface DWGoogleSignInDelegate {

  public void revokeAccessComplete();

  public void signInComplete(GoogleSignInAccount account, int statusCode, String statusMessage);

  public void signOutComplete();

  public void startActivityForResult(Intent intent, int requestCode);

}