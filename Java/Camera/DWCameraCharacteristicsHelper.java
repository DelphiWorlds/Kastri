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

import android.hardware.camera2.CameraCharacteristics;
import android.hardware.camera2.params.StreamConfigurationMap;
import android.util.Range;

public class DWCameraCharacteristicsHelper {

  private CameraCharacteristics mCharacteristics;

  public void setCameraCharacteristics(CameraCharacteristics characteristics) {
    mCharacteristics = characteristics;
  }

  public int getLensFacing() {
    return mCharacteristics.get(CameraCharacteristics.LENS_FACING);
  }

  public long getSensorExposureTimeLower() {
    Range<Long> range;
    range = mCharacteristics.get(CameraCharacteristics.SENSOR_INFO_EXPOSURE_TIME_RANGE);
		if (range != null)
      return range.getLower();
		else
      return 0;
  }

  public long getSensorExposureTimeUpper() {
    Range<Long> range;
    range = mCharacteristics.get(CameraCharacteristics.SENSOR_INFO_EXPOSURE_TIME_RANGE);
		if (range != null)
      return range.getUpper();
		else
      return 0;
  }
  
  public int getSensorOrientation() {
    return mCharacteristics.get(CameraCharacteristics.SENSOR_ORIENTATION);    
  }

  public int getSensorSensitivityLower() {
    Range<Integer> range;
    range = mCharacteristics.get(CameraCharacteristics.SENSOR_INFO_SENSITIVITY_RANGE);
		if (range != null)
      return range.getLower();
		else
      return 0;
  }

  public int getSensorSensitivityUpper() {
    Range<Integer> range;
    range = mCharacteristics.get(CameraCharacteristics.SENSOR_INFO_SENSITIVITY_RANGE);
		if (range != null)
      return range.getUpper();
		else
      return 0;
  }

  public int[] getFaceDetectModes() {
    return mCharacteristics.get(CameraCharacteristics.STATISTICS_INFO_AVAILABLE_FACE_DETECT_MODES);
  }

  public int[] getControlAEAvailableModes() {
    return mCharacteristics.get(CameraCharacteristics.CONTROL_AE_AVAILABLE_MODES);
	}

  public StreamConfigurationMap getMap() {
    return mCharacteristics.get(CameraCharacteristics.SCALER_STREAM_CONFIGURATION_MAP);
  }

}

