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

import android.content.Intent;

public interface DWFirebaseMessagingServiceCallback {

  boolean getShowNotificationWhenForeground();
  void onNotificationReceived(Intent intent);

}