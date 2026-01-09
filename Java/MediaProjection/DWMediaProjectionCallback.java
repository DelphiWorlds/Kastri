package com.delphiworlds.kastri;

import com.delphiworlds.kastri.DWMediaProjectionCallbackDelegate;

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

import android.media.projection.MediaProjection;

public class DWMediaProjectionCallback extends MediaProjection.Callback {
  
  private DWMediaProjectionCallbackDelegate mDelegate;
  
  public DWMediaProjectionCallback(DWMediaProjectionCallbackDelegate delegate) {
    mDelegate = delegate;
  }  

  @Override
  public void onCapturedContentResize(int width, int height) {
    mDelegate.onCapturedContentResize(width, height);
  }

  @Override
  public void onCapturedContentVisibilityChanged(boolean isVisible) {
    mDelegate.onCapturedContentVisibilityChanged(isVisible);
  }

  @Override
  public void onStop() {
    mDelegate.onStop();
  }
}
