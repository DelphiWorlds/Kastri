package com.delphiworlds.kastri;

/*******************************************************
 *                                                     *
 *                     Kastri                          *
 *                                                     *
 *        Delphi Worlds Cross-Platform Library         *
 *                                                     *
 * Copyright 2020-2024 Dave Nottage under MIT license  *
 * which is located in the root folder of this library *
 *                                                     *
 *******************************************************/

import com.symbol.emdk.barcode.ScanDataCollection;
import com.symbol.emdk.barcode.StatusData;

public interface DWEMDKListenerDelegate {

    void managerClosed();

    void managerOpened();

    void scannerData(ScanDataCollection scanDataCollection);

    void scannerStatus(StatusData statusData);

}