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

 // Based on: https://github.com/koy14400/Camera2/blob/master/src/main/java/com/asus/simplecamera/PreviewGLSurfaceView.java

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.SurfaceTexture;
import android.graphics.SurfaceTexture.OnFrameAvailableListener;
import android.opengl.GLES11Ext;
import android.opengl.GLES20;
import android.opengl.GLSurfaceView;
import android.opengl.GLSurfaceView.Renderer;
import android.opengl.Matrix;
import android.os.AsyncTask;
import android.os.SystemClock;
import android.util.Log;

import java.io.IOException;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.opengles.GL10;

public class DWGLCameraView extends GLSurfaceView implements Renderer, OnFrameAvailableListener {

    private static final String TAG = "DWGLCameraView";

    private DWGLDrawer mDrawer;
    private boolean mIsNewTexture = true;
    private SurfaceTexture mSurfaceTexture;
    private ViewDelegate mViewDelegate;
    private boolean mWantFrames = false;
    private long mStartTime = 0;
    private long mCaptureTime = 0;
    private long mFrameCount = 0;
    private long mMillisecondsPerCapture = 0;
    private int mCaptureFPS = 0;
    private int mActualFPS = 0;
    private int mRotation = 0;

    public interface ViewDelegate {
        void onFrameAvailable(Bitmap frame);
        // Size onGetPreviewSize();
        void onSurfaceTextureAvailable(SurfaceTexture texture);
    }

    private static class CreateFrameTask extends AsyncTask<Void, Integer, Boolean> {
        ByteBuffer mBuffer;
        int mWidth, mHeight;
        Bitmap mFrame;
        ViewDelegate mViewDelegate;

        public CreateFrameTask(ByteBuffer buffer, int width, int height, ViewDelegate delegate) {
            mBuffer = buffer;
            mWidth = width;
            mHeight = height;
            mViewDelegate = delegate;
        }

        @Override
        protected Boolean doInBackground(Void... params) {
            mBuffer.rewind();
            mFrame = Bitmap.createBitmap(mWidth, mHeight, Bitmap.Config.ARGB_8888);
            mFrame.copyPixelsFromBuffer(mBuffer);
            return true;
        }

        @Override
        protected void onPostExecute(Boolean result) {
            super.onPostExecute(result);
            mViewDelegate.onFrameAvailable(mFrame);
        }
    }

    public DWGLCameraView(Context context) { // , Size defaultSize, ViewDelegate delegate) {
        super(context);
        setEGLContextClientVersion(2);
        setRenderer(this);
    }

    private int createTextureID() {
        int[] texture = new int[1];
        GLES20.glGenTextures(1, texture, 0);
        GLES20.glBindTexture(GLES11Ext.GL_TEXTURE_EXTERNAL_OES, texture[0]);
        GLES20.glTexParameterf(GLES11Ext.GL_TEXTURE_EXTERNAL_OES,
                GL10.GL_TEXTURE_MIN_FILTER, GL10.GL_LINEAR);
        GLES20.glTexParameterf(GLES11Ext.GL_TEXTURE_EXTERNAL_OES,
                GL10.GL_TEXTURE_MAG_FILTER, GL10.GL_LINEAR);
        GLES20.glTexParameteri(GLES11Ext.GL_TEXTURE_EXTERNAL_OES,
                GL10.GL_TEXTURE_WRAP_S, GL10.GL_CLAMP_TO_EDGE);
        GLES20.glTexParameteri(GLES11Ext.GL_TEXTURE_EXTERNAL_OES,
                GL10.GL_TEXTURE_WRAP_T, GL10.GL_CLAMP_TO_EDGE);
        return texture[0];
    }

    // OnFrameAvailableListener
    public void onFrameAvailable(SurfaceTexture surfaceTexture) {
        requestRender();
    }

    // Renderer
    public void onSurfaceCreated(GL10 gl, EGLConfig config) {
        mIsNewTexture = true;
        mStartTime = SystemClock.elapsedRealtime();
        mCaptureTime = 0;
        mFrameCount = 0;
        int textureId = createTextureID();
        mSurfaceTexture = new SurfaceTexture(textureId);
        // mSurfaceTexture.setDefaultBufferSize(getWidth(), getHeight()); // This is not done in the other example
        mDrawer = new DWGLDrawer(textureId);
        mSurfaceTexture.setOnFrameAvailableListener(this);
    }

    // Renderer
    public void onSurfaceChanged(GL10 gl, int width, int height) {
        GLES20.glViewport(0, 0, width, height);
    }

    // Renderer
    public void onDrawFrame(GL10 gl) {
        if ((mViewDelegate != null) && mIsNewTexture) {
          mIsNewTexture = false;
          mViewDelegate.onSurfaceTextureAvailable(mSurfaceTexture); 
        }

        // From the com.otaliastudios.cameraview code
        // private float[] mTextureTransform = Egloo.IDENTITY_MATRIX.clone();
        // final float[] transform = mOutputTextureDrawer.getTextureTransform();
        // mInputSurfaceTexture.updateTexImage();
        // mInputSurfaceTexture.getTransformMatrix(transform);

        float[] mtx = new float[16];
        mSurfaceTexture.updateTexImage();
        mSurfaceTexture.getTransformMatrix(mtx);
        // Still does not work!!! :-(
        if (mRotation != 0) {
            // My old code:
            // Matrix.setIdentityM(mtx, 0);
            // Matrix.rotateM(mtx, 0, mRotation, 0f, 0f, -1f);

            // Next attempt:
            // Matrix.translateM(mtx, 0, 0.5F, 0.5F, 0);
            // Matrix.rotateM(mtx, 0, mRotation, 0, 0, 1);
            // Matrix.translateM(mtx, 0, -0.5F, -0.5F, 0);

            // Per Marcos' comment Feb 24 https://stackoverflow.com/questions/66323994/applying-rotation-to-glsurfaceview
            Matrix.rotateM(mtx, 0, mRotation, 1f, 1f, 0f);
        }
        //GLES20.glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
        //GLES20.glClear(GLES20.GL_COLOR_BUFFER_BIT | GLES20.GL_DEPTH_BUFFER_BIT);
        mDrawer.draw(mtx); // Could just move the DWGLDrawer code to here?
        if ((mViewDelegate != null) && (mCaptureFPS > 0)) {
            long elapsed = 0;
            if (mCaptureTime > 0)
              elapsed = SystemClock.elapsedRealtime() - mCaptureTime; // Difference from the last capture, in milliseconds
            if ((mCaptureTime == 0) || (elapsed > mMillisecondsPerCapture)) {
              // Based on code in: https://github.com/google/grafika/blob/b1df331e89cffeab621f02b102d4c2c25eb6088a/app/src/main/java/com/android/grafika/gles/EglSurfaceBase.java#L157
              int width = getWidth();
              int height = getHeight();
              ByteBuffer buf = ByteBuffer.allocateDirect(width * height * 4);
              buf.order(ByteOrder.LITTLE_ENDIAN);
              GLES20.glReadPixels(0, 0, width, height, GLES20.GL_RGBA, GLES20.GL_UNSIGNED_BYTE, buf);
              // GlUtil.checkGlError("glReadPixels"); // https://github.com/google/grafika/blob/b1df331e89cffeab621f02b102d4c2c25eb6088a/app/src/main/java/com/android/grafika/gles/GlUtil.java#L106
              /*
              buf.rewind();
              Bitmap frame = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
              frame.copyPixelsFromBuffer(buf);
              */
              new CreateFrameTask(buf, width, height, mViewDelegate).execute();
              mCaptureTime = SystemClock.elapsedRealtime();
              // mViewDelegate.onFrameAvailable(frame);
            }
            // mFrameCount++;
            // mActualFPS = (int) (mFrameCount / ((SystemClock.elapsedRealtime() - mStartTime) * 1000));
        }
    }

    public void setViewDelegate(ViewDelegate delegate) {
        mViewDelegate = delegate;
    }

    public void setCaptureFPS(int fps) {
      mCaptureFPS = fps;
      if (mCaptureFPS > 0)
        mMillisecondsPerCapture = (long) (1000 / mCaptureFPS);  // e.g. 60fps = 16.67ms per capture
      else 
        mMillisecondsPerCapture = 0;
    }

    public void setCameraRotation(int rotation) {
      mRotation = rotation;
    }    
}
