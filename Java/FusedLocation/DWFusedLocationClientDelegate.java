package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2026 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.location.Location;

public interface DWFusedLocationClientDelegate {

  public void onLocation(Location location);

  public void onLocationUpdatesChange(boolean active);

  public void onSetMockLocationResult(Location location);

  public void onSetMockModeResult(boolean success);

}