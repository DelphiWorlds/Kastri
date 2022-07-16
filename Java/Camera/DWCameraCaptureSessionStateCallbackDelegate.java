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

public interface DWCameraCaptureSessionStateCallbackDelegate {

  void onConfigured(CameraCaptureSession session);
  void onConfigureFailed(CameraCaptureSession session);
  
}