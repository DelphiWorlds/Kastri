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

public class DWCameraDeviceStateCallback extends CameraDevice.StateCallback {

  private DWCameraDeviceStateCallbackDelegate mDelegate;

  public DWCameraDeviceStateCallback(DWCameraDeviceStateCallbackDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void onOpened(CameraDevice camera) {
    mDelegate.onOpened(camera);
  }
      
  @Override
  public void onError(CameraDevice camera, int error) {
    mDelegate.onError(camera, error);
  }
  
  @Override
  public void onDisconnected(CameraDevice camera) {
    mDelegate.onDisconnected(camera); 
  }

}