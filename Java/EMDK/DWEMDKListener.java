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
import android.util.Log;
import com.symbol.emdk.EMDKManager;
import com.symbol.emdk.EMDKManager.EMDKListener;
import com.symbol.emdk.EMDKManager.FEATURE_TYPE;
import com.symbol.emdk.EMDKResults;
import com.symbol.emdk.EMDKResults.STATUS_CODE;
import com.symbol.emdk.barcode.BarcodeManager;
import com.symbol.emdk.barcode.BarcodeManager.DeviceIdentifier;
import com.symbol.emdk.barcode.ScanDataCollection;
import com.symbol.emdk.barcode.Scanner;
import com.symbol.emdk.barcode.Scanner.DataListener;
import com.symbol.emdk.barcode.Scanner.StatusListener;
import com.symbol.emdk.barcode.ScannerException;
import com.symbol.emdk.barcode.StatusData;

public class DWEMDKListener implements EMDKListener, StatusListener, DataListener {
    private static final String TAG = "DWEMDKListener";
    private BarcodeManager mBarcodeManager;
    private EMDKManager mEMDKManager;
    private Scanner mScanner;
    private DWEMDKListenerDelegate mDelegate;

    DWEMDKListener(Context context, DWEMDKListenerDelegate delegate) {
        mDelegate = delegate;
        Log.d(TAG, "Attempting to acquire EMDKManager..");
        EMDKResults results = EMDKManager.getEMDKManager(context.getApplicationContext(), this);
        if (results.statusCode != STATUS_CODE.SUCCESS) 
          Log.e(TAG, "Failed to acquire EMDKManager - statusCode: " + String.valueOf(results.statusCode));
    }

    public Scanner getScanner() {
        return mScanner;
    }

    private boolean acquireScanner() {        
        if ((mScanner == null) && (mBarcodeManager != null)) {
            Log.d(TAG, "Acquiring scanner");
            mScanner = mBarcodeManager.getDevice(DeviceIdentifier.DEFAULT);
            if (mScanner != null) {
                Log.d(TAG, "Adding listeners to scanner");
                mScanner.addDataListener(this);
                mScanner.addStatusListener(this);
                try {
                    mScanner.enable();
                } catch (ScannerException e) {
                    Log.e(TAG, "acquireScanner enable failed: " + e.getMessage());
                    releaseScanner();
                }
            }
        }
        return mScanner != null;        
    }

    private void releaseScanner() {
        if (mScanner != null) {
            try{
                mScanner.disable();
            } catch (Exception e) {
                Log.e(TAG, "releaseScanner disable failed: " + e.getMessage());
            }

            try {
                mScanner.removeDataListener(this);
                mScanner.removeStatusListener(this);
            } catch (Exception e) {
                Log.e(TAG, "releaseScanner remove listeners failed: " + e.getMessage());
            }

            try{
                mScanner.release();
            } catch (Exception e) {
                Log.e(TAG, "releaseScanner release failed: " + e.getMessage());
            }
            mScanner = null;
        }        
    }

    public EMDKManager getManager() {
        return mEMDKManager;
    }

    public boolean enableScanner(boolean enable) {
        if (enable) {
            mBarcodeManager = (BarcodeManager) mEMDKManager.getInstance(FEATURE_TYPE.BARCODE);
            return acquireScanner();
        } else {
            releaseScanner();
            if (mEMDKManager != null) {
                mEMDKManager.release(FEATURE_TYPE.BARCODE);
                mBarcodeManager = null;
            }
            return true; 
        }
    }

    // EMDKListener
    public void onClosed() {
        mDelegate.managerClosed();
        if (mEMDKManager != null) {
            mEMDKManager.release();
            mEMDKManager = null;
        }
    }

    // EMDKListener
    public void onOpened(EMDKManager manager) {
        mEMDKManager = manager;
        if (mEMDKManager == null)
            Log.d(TAG, "onOpened but manager is NULL");
        if (mEMDKManager != null)
            mDelegate.managerOpened();
    }

    // DataListener
    public void onData(ScanDataCollection scanDataCollection) {
        mDelegate.scannerData(scanDataCollection);
    }

    // StatusListener
    public void onStatus(StatusData statusData) {
        mDelegate.scannerStatus(statusData);
    }
}