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

import android.hardware.camera2.CameraDevice;

public interface DWCameraDeviceStateCallbackDelegate {

  void onDisconnected(CameraDevice camera);
  void onError(CameraDevice camera, int error);
  void onOpened(CameraDevice camera);
  
}