package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2021 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.hardware.camera2.CameraCaptureSession;

public class DWCameraCaptureSessionStateCallback extends CameraCaptureSession.StateCallback {

  private DWCameraCaptureSessionStateCallbackDelegate mDelegate;

  public DWCameraCaptureSessionStateCallback(DWCameraCaptureSessionStateCallbackDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onConfigured(CameraCaptureSession session) {
    mDelegate.onConfigured(session);
  }
  
  @Override
  public void onConfigureFailed(CameraCaptureSession session) {
    mDelegate.onConfigureFailed(session);
  }

}