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

public interface DWMediaProjectionCallbackDelegate {

  public void onCapturedContentResize(int width, int height);
  public void onCapturedContentVisibilityChanged(boolean isVisible);
  public void onStop();

}