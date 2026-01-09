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

import android.content.Context;
import android.graphics.Canvas;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.widget.SeekBar;

public class DWSeekBar extends SeekBar {

  private static final String TAG = "DWSeekBar";
  // Clockwise
  public static final int ROTATION_0 = 0;   // min on the left
  public static final int ROTATION_90 = 1;  // min is at top
  public static final int ROTATION_180 = 2; // min is on the right
  public static final int ROTATION_270 = 3; // min is at bottom

  private int mRotation = 0;
  private OnSeekBarChangeListener mOnSeekBarChangeListener;

  public DWSeekBar(Context context) {
    super(context);
  }

  public DWSeekBar(Context context, AttributeSet attrs, int defStyle) {
    super(context, attrs, defStyle);
  }

  public DWSeekBar(Context context, AttributeSet attrs) {
    super(context, attrs);
  }

  protected void onSizeChanged(int w, int h, int oldw, int oldh) {
    switch (mRotation) {
      case ROTATION_0:
      case ROTATION_180:
        super.onSizeChanged(w, h, oldw, oldh);
        break;
      case ROTATION_90:
      case ROTATION_270:
        super.onSizeChanged(h, w, oldh, oldw);
        break;
    }
  }

  @Override
  protected synchronized void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
    switch (mRotation) {
      case ROTATION_0:
      case ROTATION_180:
        super.onMeasure(widthMeasureSpec, heightMeasureSpec);
        setMeasuredDimension(getMeasuredWidth(), getMeasuredHeight());
        break;
      case ROTATION_90:
      case ROTATION_270:
        super.onMeasure(heightMeasureSpec, widthMeasureSpec);
        setMeasuredDimension(getMeasuredHeight(), getMeasuredWidth());
        break;
    }
  }

  protected void onDraw(Canvas c) {
    switch (mRotation) {
      case ROTATION_90:
        c.rotate(90);
        c.translate(0, -getWidth());
        break;
      case ROTATION_180:
        c.rotate(180);
        break;
      case ROTATION_270:
        c.rotate(-90);
        c.translate(-getHeight(), 0);
        break;
    }
    super.onDraw(c);
  }

  private void setProgressFromTouch(int progress) {
    setProgress(progress);
    if (mOnSeekBarChangeListener != null)
      mOnSeekBarChangeListener.onProgressChanged(this, progress, true);
  }

  @Override
  public synchronized void setProgress(int progress) { 
    super.setProgress(progress); 
    onSizeChanged(getWidth(), getHeight(), 0, 0); 
  }
  
  @Override
  public boolean onTouchEvent(MotionEvent event) {
    if ((mRotation == ROTATION_0) || (mRotation == ROTATION_180))
      return super.onTouchEvent(event);

    if (!isEnabled()) {
      return false;
    }

    switch (event.getAction()) {
      case MotionEvent.ACTION_DOWN:
      case MotionEvent.ACTION_MOVE:
      case MotionEvent.ACTION_UP:
        switch (mRotation) {
          case ROTATION_90:
          case ROTATION_270:
            int i = getMax() - (int) (getMax() * event.getY() / getHeight());
            if ((i >= getMin()) && (i <= getMax()))
                setProgressFromTouch(i);
            break;
        }
        break;
      case MotionEvent.ACTION_CANCEL:
        break;
    }
    return true;
  }

  @Override
  public void setOnSeekBarChangeListener(OnSeekBarChangeListener l) {
    mOnSeekBarChangeListener = l;
    super.setOnSeekBarChangeListener(l);
  }

  public void setRotation(int rotation) {
    mRotation = rotation;
  }

}