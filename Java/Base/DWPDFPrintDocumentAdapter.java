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
import android.graphics.pdf.PdfRenderer;
import android.os.Bundle;
import android.os.CancellationSignal;
import android.os.ParcelFileDescriptor;
import android.print.PageRange;
import android.print.PrintAttributes;
import android.print.PrintDocumentAdapter;
import android.print.PrintDocumentInfo;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * PrintDocumentAdapter that sends an existing PDF file to the Android print
 * framework unchanged. The destination of onWrite() is itself a PDF, so the
 * original file is copied byte-for-byte. Vector text and line art are preserved.
 */
public class DWPDFPrintDocumentAdapter extends PrintDocumentAdapter {
  private final Context mContext;
  private final File mPdfFile;
  private int mPageCount = PrintDocumentInfo.PAGE_COUNT_UNKNOWN;

  public DWPDFPrintDocumentAdapter(Context context, File pdfFile) {
    mContext = context;
    mPdfFile = pdfFile;
    mPageCount = queryPageCount(pdfFile);
  }

  private static int queryPageCount(File file) {
    if (file == null || !file.exists()) {
      return PrintDocumentInfo.PAGE_COUNT_UNKNOWN;
    }
    ParcelFileDescriptor fd = null;
    PdfRenderer renderer = null;
    try {
      fd = ParcelFileDescriptor.open(file, ParcelFileDescriptor.MODE_READ_ONLY);
      renderer = new PdfRenderer(fd);
      return renderer.getPageCount();
    } catch (IOException e) {
      e.printStackTrace();
      return PrintDocumentInfo.PAGE_COUNT_UNKNOWN;
    } finally {
      if (renderer != null) {
        renderer.close();
      }
      if (fd != null) {
        try {
          fd.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
  }

  @Override
  public void onLayout(PrintAttributes oldAttributes, PrintAttributes newAttributes,
      CancellationSignal cancellationSignal, LayoutResultCallback callback, Bundle extras) {
    if (cancellationSignal.isCanceled()) {
      callback.onLayoutCancelled();
      return;
    }
    if (mPdfFile == null || !mPdfFile.exists()) {
      callback.onLayoutFailed("PDF file is missing");
      return;
    }
    PrintDocumentInfo info = new PrintDocumentInfo.Builder(mPdfFile.getName())
        .setContentType(PrintDocumentInfo.CONTENT_TYPE_DOCUMENT)
        .setPageCount(mPageCount)
        .build();
    // Content is the original file; attributes do not change what we write.
    boolean layoutChanged = oldAttributes == null || !oldAttributes.equals(newAttributes);
    callback.onLayoutFinished(info, layoutChanged);
  }

  @Override
  public void onWrite(PageRange[] pages, ParcelFileDescriptor destination,
      CancellationSignal cancellationSignal, WriteResultCallback callback) {
    if (cancellationSignal.isCanceled()) {
      callback.onWriteCancelled();
      return;
    }
    if (mPdfFile == null || !mPdfFile.exists()) {
      callback.onWriteFailed("PDF file is missing");
      return;
    }

    InputStream in = null;
    OutputStream out = null;
    try {
      in = new FileInputStream(mPdfFile);
      out = new FileOutputStream(destination.getFileDescriptor());
      byte[] buffer = new byte[8192];
      int read;
      while ((read = in.read(buffer)) >= 0) {
        if (cancellationSignal.isCanceled()) {
          callback.onWriteCancelled();
          return;
        }
        out.write(buffer, 0, read);
      }
      out.flush();
      callback.onWriteFinished(new PageRange[] { PageRange.ALL_PAGES });
    } catch (IOException e) {
      callback.onWriteFailed(e.toString());
    } finally {
      if (in != null) {
        try {
          in.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
      if (out != null) {
        try {
          out.close();
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
  }
}
