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

import android.hardware.camera2.CaptureRequest;
import android.hardware.camera2.CameraMetadata;

public class DWCaptureRequestBuilderHelper {

  private CaptureRequest.Builder mBuilder;

  public void setCaptureRequestBuilder(CaptureRequest.Builder builder) {
    mBuilder = builder;
  }

  public void setFaceDetectMode(int mode) {
    switch (mode) {
      case 0:
        mBuilder.set(CaptureRequest.STATISTICS_FACE_DETECT_MODE, CameraMetadata.STATISTICS_FACE_DETECT_MODE_OFF);
        break;
      case 1:
        mBuilder.set(CaptureRequest.STATISTICS_FACE_DETECT_MODE, CameraMetadata.STATISTICS_FACE_DETECT_MODE_SIMPLE);
        break;
      case 2:
        mBuilder.set(CaptureRequest.STATISTICS_FACE_DETECT_MODE, CameraMetadata.STATISTICS_FACE_DETECT_MODE_FULL);
        break;
    }
  }

  public int getIntegerValue(CaptureRequest.Key<Integer> key) {
    Integer value = mBuilder.get(key);
    if (value != null)
      return value.intValue();
    else
      return Integer.MIN_VALUE;
  }

  public void setIntegerValue(CaptureRequest.Key<Integer> key, int value) {
    mBuilder.set(key, value);
  }
}