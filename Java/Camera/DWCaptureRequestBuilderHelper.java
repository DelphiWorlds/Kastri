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
		if (key != null) {
	    Integer value = mBuilder.get(key);
	    if (value != null)
	      return value.intValue();
    }
    return Integer.MIN_VALUE;
  }

  public long getLongValue(CaptureRequest.Key<Long> key) {
	  if (key != null) {
	    Long value = mBuilder.get(key);
	    if (value != null)
	      return value.longValue();
		};
    return Long.MIN_VALUE;
  }

  public void setIntegerValue(CaptureRequest.Key<Integer> key, int value) {
	  if (key != null)
      mBuilder.set(key, value);
  }

  public void setLongValue(CaptureRequest.Key<Long> key, long value) {
	  if (key != null)
      mBuilder.set(key, value);
  }
}