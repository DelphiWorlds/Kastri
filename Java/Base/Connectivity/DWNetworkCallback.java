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

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.Network;
import android.net.NetworkRequest;

public class DWNetworkCallback extends ConnectivityManager.NetworkCallback {

    private static final String TAG = "DWNetworkCallback";
    private DWNetworkCallbackDelegate mDelegate;

    public DWNetworkCallback(Context context, DWNetworkCallbackDelegate delegate) {
        mDelegate = delegate;
        ConnectivityManager connectivityManager = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkRequest.Builder builder = new NetworkRequest.Builder();
        connectivityManager.registerNetworkCallback(builder.build(), this);
    }

    @Override
    public void onAvailable(Network network) {
        mDelegate.onAvailable(network);
    }

    @Override
    public void onLost(Network network) {
        mDelegate.onLost(network);
    }

    @Override
    public void onUnavailable() {
        mDelegate.onUnavailable();
    }
}