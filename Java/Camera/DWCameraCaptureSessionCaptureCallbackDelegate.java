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
 
import android.hardware.camera2.CameraCaptureSession;
import android.hardware.camera2.CaptureRequest;
import android.hardware.camera2.CaptureResult;
import android.hardware.camera2.TotalCaptureResult;

public interface DWCameraCaptureSessionCaptureCallbackDelegate {

  void CaptureProgressed(CameraCaptureSession session, CaptureRequest request, CaptureResult partialResult);

  void CaptureCompleted(CameraCaptureSession session, CaptureRequest request, TotalCaptureResult result);
  
}