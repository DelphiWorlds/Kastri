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
import android.hardware.camera2.CaptureRequest;
import android.hardware.camera2.CaptureResult;
import android.hardware.camera2.TotalCaptureResult;

public class DWCameraCaptureSessionCaptureCallback extends CameraCaptureSession.CaptureCallback {

  private DWCameraCaptureSessionCaptureCallbackDelegate mDelegate;

  public DWCameraCaptureSessionCaptureCallback(DWCameraCaptureSessionCaptureCallbackDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onCaptureProgressed(CameraCaptureSession session, CaptureRequest request, CaptureResult partialResult) {
    mDelegate.onCaptureProgressed(session, request, partialResult);
  }

  @Override
  public void onCaptureCompleted(CameraCaptureSession session, CaptureRequest request, TotalCaptureResult result) {
    mDelegate.onCaptureCompleted(session, request, result);
  }

}