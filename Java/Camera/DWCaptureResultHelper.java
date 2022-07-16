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

import android.hardware.camera2.CaptureResult;
import android.hardware.camera2.params.Face;

public class DWCaptureResultHelper {

  private CaptureResult mCaptureResult;

  public Face[] getFaces() {
    return mCaptureResult.get(CaptureResult.STATISTICS_FACES);
  }

  public int getFaceDetectMode() {
    return mCaptureResult.get(CaptureResult.STATISTICS_FACE_DETECT_MODE);
  }

  public void setCaptureResult(CaptureResult captureResult) {
    mCaptureResult = captureResult;
  }

}