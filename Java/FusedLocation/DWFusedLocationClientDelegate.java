package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                  Kastri Free                        *
 *                                                     *
 *         DelphiWorlds Cross-Platform Library         *
 *                                                     *
 *******************************************************/

import android.location.Location;

public interface DWFusedLocationClientDelegate {

  public void onLocation(Location location);

  public void onLocationUpdatesChange(boolean active);

  public void onSetMockLocationResult(Location location);

  public void onSetMockModeResult(boolean success);

}