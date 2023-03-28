package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2023 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import android.util.Log;
import java.util.TimerTask;

public class DWTimerTask extends TimerTask {

  private static final String TAG = "DWTimerTask";
  private DWTimerTaskDelegate mDelegate;

  public DWTimerTask(DWTimerTaskDelegate delegate) {
    mDelegate = delegate;
  }

  @Override
  public void run() {
    // Log.v(TAG, "DWTimerTask.run");
    mDelegate.run();
  }
}