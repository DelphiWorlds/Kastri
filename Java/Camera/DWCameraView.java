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

import android.app.Activity;
import android.content.Context;
import android.graphics.Matrix;
import android.graphics.RectF;
import android.graphics.SurfaceTexture;
import android.util.Log;
import android.util.Size;
import android.view.Surface;
import android.view.TextureView;
import java.lang.Math;

public class DWCameraView extends TextureView implements TextureView.SurfaceTextureListener {
  
  public interface StateDelegate {
    void onReady(DWCameraView view);
    void onDestroyed(DWCameraView view) throws Exception;
  }

  private Size mPreviewSize;
  private StateDelegate mStateDelegate;
  private boolean mMirror = false;
  private Size mViewSize = new Size(0, 0);

  public DWCameraView(Context context) {
    super(context, null);
    setSurfaceTextureListener(this);
  }

  public Size getPreviewSize() {
    return mPreviewSize;
  }

  public void setPreviewSize(Size previewSize) {
    mPreviewSize = previewSize;
    adjustView();
    requestLayout();
  }

  @Override
  protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
    super.onMeasure(widthMeasureSpec, heightMeasureSpec);
    int width = MeasureSpec.getSize(widthMeasureSpec);
    int height = MeasureSpec.getSize(heightMeasureSpec);
    if (mPreviewSize != null) { 
      width = mPreviewSize.getWidth();
      height = mPreviewSize.getHeight();   
    }
    setMeasuredDimension(width, height);
  }

  public void setStateDelegate(StateDelegate Delegate) {
    mStateDelegate = Delegate;
  }

  public void setMirror(boolean mirror) {
    mMirror = mirror;
  }

  @Override
  public void onSurfaceTextureAvailable(SurfaceTexture surfaceTexture, int i, int i1) {
    if (mStateDelegate != null) {
      mStateDelegate.onReady(this);
    }
  }

  @Override
  public void onSurfaceTextureSizeChanged(SurfaceTexture surfaceTexture, int i, int i1) {
    adjustView();
  }

  @Override
  public boolean onSurfaceTextureDestroyed(SurfaceTexture surfaceTexture) {
    if (mStateDelegate != null) {
      try {
        mStateDelegate.onDestroyed(this);
      }
      catch (Exception e) {
        Log.e(getClass().getSimpleName(), "Exception destroying state", e);
      }
    }
    return false;
  }

  @Override
  public void onSurfaceTextureUpdated(SurfaceTexture surfaceTexture) {

  }

  private void adjustView() {
    if (mPreviewSize != null) {
      adjustViewRotation(mPreviewSize.getWidth(), mPreviewSize.getHeight(),
        ((Activity)getContext()).getWindowManager().getDefaultDisplay().getRotation());
    }
  }

  private void adjustViewRotation(int previewWidth, int previewHeight, int rotation) {
    Matrix txform = new Matrix();
    int viewWidth = getWidth();
    int viewHeight = getHeight();
    RectF rectView = new RectF(0, 0, viewWidth, viewHeight);
    float viewCenterX = rectView.centerX();
    float viewCenterY = rectView.centerY();
    RectF rectPreview = new RectF(0, 0, previewHeight, previewWidth);
    float previewCenterX = rectPreview.centerX();
    float previewCenterY = rectPreview.centerY();

    if (Surface.ROTATION_90 == rotation || Surface.ROTATION_270 == rotation) {
      rectPreview.offset(viewCenterX-previewCenterX, viewCenterY-previewCenterY);
      txform.setRectToRect(rectView, rectPreview, Matrix.ScaleToFit.FILL);
      float scale = Math.max((float)viewHeight/previewHeight, (float)viewWidth/previewWidth);
      txform.postScale(scale, scale, viewCenterX, viewCenterY);
      txform.postRotate(90*(rotation-2), viewCenterX, viewCenterY);
    } else {
      if (Surface.ROTATION_180 == rotation) {
        txform.postRotate(180, viewCenterX, viewCenterY);
      }
    }
    if (mMirror) {
      txform.postScale(-1, 1, viewCenterX, viewCenterY);
    }
    setTransform(txform);
  }
}