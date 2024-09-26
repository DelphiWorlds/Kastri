package com.delphiworlds.kastri;

import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.ColorFilter;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.drawable.Drawable;

public class DWRectangleDrawable extends Drawable {

  private final Paint fill;
  private final Paint stroke;
  private int strokeWidth;
  private int visibleSides;
  public static final int SIDE_NONE = 0;
  public static final int SIDE_LEFT = 2;
  public static final int SIDE_TOP = 4;
  public static final int SIDE_RIGHT = 8;
  public static final int SIDE_BOTTOM = 16;

  public DWRectangleDrawable() {
    this.fill = new Paint();
    this.fill.setStyle(Paint.Style.FILL);
    this.stroke = new Paint();
    this.stroke.setStyle(Paint.Style.FILL);    
    this.strokeWidth = 5;
    this.visibleSides = SIDE_LEFT | SIDE_TOP | SIDE_RIGHT | SIDE_BOTTOM;
  }

  public void setSides(int sides) {
    this.visibleSides = sides;
    invalidateSelf();
  }

  public void setStroke(int strokeWidth, int strokeColor) {
    this.strokeWidth = strokeWidth;
    this.stroke.setColor(strokeColor);
    invalidateSelf();
  }

  @Override
  public void draw(Canvas canvas) {
    canvas.drawPaint(fill);
    Rect bounds = getBounds();
    if ((visibleSides & SIDE_TOP) == SIDE_TOP) {
      canvas.drawRect(bounds.left, bounds.top, bounds.right, bounds.top + strokeWidth, stroke);
    }
    if ((visibleSides & SIDE_BOTTOM) == SIDE_BOTTOM) {
      canvas.drawRect(bounds.left, bounds.bottom - strokeWidth, bounds.right, bounds.bottom, stroke);
    }
    if ((visibleSides & SIDE_LEFT) == SIDE_LEFT) {
      canvas.drawRect(bounds.left, bounds.top, bounds.left + strokeWidth, bounds.bottom, stroke);
    }
    if ((visibleSides & SIDE_RIGHT) == SIDE_RIGHT) {
      canvas.drawRect(bounds.right - strokeWidth, bounds.top, bounds.right, bounds.bottom, stroke);
    }
  }

  @Override
  public void setAlpha(int alpha) {
    fill.setAlpha(alpha);
    stroke.setAlpha(alpha);
    invalidateSelf();
  }

  @Override
  public void setColorFilter(ColorFilter colorFilter) {
    fill.setColorFilter(colorFilter);
    stroke.setColorFilter(colorFilter);
    invalidateSelf();
  }

  @Override
  public int getOpacity() {
    return fill.getAlpha();
  }

  public Paint getFill() {
    return this.fill;
  }
}
