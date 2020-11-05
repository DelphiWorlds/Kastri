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

import android.hardware.camera2.CameraDevice;

public interface DWCameraDeviceStateCallbackDelegate {

  void Opened(CameraDevice camera);

  void Disconnected(CameraDevice camera);

  void Error(CameraDevice camera, int error);
  
}