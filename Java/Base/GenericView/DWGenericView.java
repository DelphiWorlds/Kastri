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

import android.content.Context;
import android.view.InputDevice;
import android.view.MotionEvent;
import android.view.View;

public class DWGenericView extends View {

  private DWGenericViewDelegate mDelegate;

  public DWGenericView(Context context, DWGenericViewDelegate delegate) {
    super(context);
    mDelegate = delegate;
  }

  @Override
  public boolean onGenericMotionEvent(MotionEvent event) {
    if (event.isFromSource(InputDevice.SOURCE_CLASS_POINTER)) {
      switch (event.getAction()) {
        case MotionEvent.ACTION_SCROLL:
          mDelegate.onMouseWheel(event);
          return true;
      }
    }
    return super.onGenericMotionEvent(event);
  }

}
