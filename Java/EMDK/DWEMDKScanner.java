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

import android.util.Log;
import android.os.Bundle;
import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import com.symbol.emdk.barcode.Scanner;
import com.symbol.emdk.barcode.ScannerException;

public class DWEMDKScanner {  
    
    final static String TAG = "DWEMDKScanner";
    private DWEMDKListener mListener;

    DWEMDKScanner(Context context, DWEMDKListenerDelegate delegate) {     
        mListener = new DWEMDKListener(context, delegate);   
    }

    public static boolean isEMDKInstalled(Context context) {
        try {    
            String emdkPkgName = "com.symbol.emdk.emdkservice";
            PackageInfo pinfo = context.getPackageManager().getPackageInfo(emdkPkgName, 0);
            Log.d(TAG, "EMDK is installed");
            return true;
        } catch (PackageManager.NameNotFoundException e) {
            Log.d(TAG, "EMDK is NOT installed");
            return false;
        }        
    }

    public boolean getIsInitialized() {
        return (mListener.getManager() != null);
    }

    public Scanner getScanner() throws ScannerException {
        return mListener.getScanner();
    }

    public boolean enableScanner(boolean enable) {
        return mListener.enableScanner(enable);
    }

}

