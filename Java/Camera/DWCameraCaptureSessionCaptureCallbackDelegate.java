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

public interface DWCameraCaptureSessionCaptureCallbackDelegate {

  void onCaptureCompleted(CameraCaptureSession session, CaptureRequest request, TotalCaptureResult result);
  void onCaptureProgressed(CameraCaptureSession session, CaptureRequest request, CaptureResult partialResult);
  
}