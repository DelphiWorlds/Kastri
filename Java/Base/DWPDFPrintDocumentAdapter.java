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
import android.graphics.Bitmap;
import android.graphics.pdf.PdfRenderer;
import android.os.Bundle;
import android.os.CancellationSignal;
import android.os.ParcelFileDescriptor;
import android.print.PageRange;
import android.print.PrintAttributes;
import android.print.PrintDocumentAdapter;
import android.print.PrintDocumentInfo;
import android.print.PrintManager;
import android.print.pdf.PrintedPdfDocument;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;


public class DWPDFPrintDocumentAdapter extends PrintDocumentAdapter {
  private Context context;
  private ParcelFileDescriptor fileDescriptor;
  private PdfRenderer pdfRenderer;
  private PrintedPdfDocument printedPdfDocument;

  public DWPDFPrintDocumentAdapter(Context context, File pdfFile) {
    this.context = context;
    try {
      openRenderer(pdfFile);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private void openRenderer(File file) throws IOException {
    fileDescriptor = ParcelFileDescriptor.open(file, ParcelFileDescriptor.MODE_READ_ONLY);
    pdfRenderer = new PdfRenderer(fileDescriptor);
  } 
  
  private void closeRenderer() throws IOException {
    pdfRenderer.close();
    fileDescriptor.close();
  }

  private Bitmap renderPageToBitmap(int pageIndex) {
    PdfRenderer.Page page = pdfRenderer.openPage(pageIndex);
    Bitmap bitmap = Bitmap.createBitmap(page.getWidth(), page.getHeight(), Bitmap.Config.ARGB_8888);
    page.render(bitmap, null, null, PdfRenderer.Page.RENDER_MODE_FOR_DISPLAY);
    page.close();
    return bitmap;
  }

  @Override
  public void onLayout(PrintAttributes oldAttributes, PrintAttributes newAttributes, CancellationSignal cancellationSignal, LayoutResultCallback callback, Bundle extras) {
    printedPdfDocument = new PrintedPdfDocument(context, newAttributes);
    if (cancellationSignal.isCanceled()) {
      callback.onLayoutCancelled();
      return;
    }
    PrintDocumentInfo info = new PrintDocumentInfo.Builder("pdf_print")
      .setContentType(PrintDocumentInfo.CONTENT_TYPE_DOCUMENT)
      .setPageCount(pdfRenderer.getPageCount())
      .build();
    callback.onLayoutFinished(info, true);
  }

  @Override
  public void onWrite(PageRange[] pages, ParcelFileDescriptor destination, CancellationSignal cancellationSignal, WriteResultCallback callback) {
    for (int i = 0; i < pdfRenderer.getPageCount(); i++) {
      if (cancellationSignal.isCanceled()) {
        callback.onWriteCancelled();
        printedPdfDocument.close();
        printedPdfDocument = null;
        return;
      }
      PrintedPdfDocument.Page page = printedPdfDocument.startPage(i);
      Bitmap bitmap = renderPageToBitmap(i);
      page.getCanvas().drawBitmap(bitmap, 0, 0, null);
      printedPdfDocument.finishPage(page);
    }
    try (FileOutputStream out = new FileOutputStream(destination.getFileDescriptor())) {
      printedPdfDocument.writeTo(out);
    } catch (IOException e) {
      callback.onWriteFailed(e.toString());
      return;
    } finally {
      printedPdfDocument.close();
      printedPdfDocument = null;
    }
    callback.onWriteFinished(new PageRange[]{PageRange.ALL_PAGES});
  }

  @Override
  public void onFinish() {
    try {
      closeRenderer();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}