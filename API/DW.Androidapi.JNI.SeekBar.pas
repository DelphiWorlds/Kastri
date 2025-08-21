unit DW.Androidapi.JNI.SeekBar;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Util, Androidapi.JNI.Os;

type
  JAbsSeekBar = interface;
  JProgressBar = interface;
  JSeekBar = interface;
  JSeekBar_OnSeekBarChangeListener = interface;

  JProgressBarClass = interface(JViewClass)
    ['{7C866902-4A8B-4C41-9FC7-AD082A5EA0D5}']
    {class} function init(context: JContext): JProgressBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JProgressBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JProgressBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JProgressBar; cdecl; overload;
  end;

  [JavaSignature('android/widget/ProgressBar')]
  JProgressBar = interface(JView)
    ['{FBB449B5-8CFC-42A1-B004-16A4FDFFD32D}']
    procedure drawableHotspotChanged(x: Single; y: Single); cdecl;
    function getAccessibilityClassName: JCharSequence; cdecl;
    function getIndeterminateDrawable: JDrawable; cdecl;
    function getIndeterminateTintList: JColorStateList; cdecl;
    function getIndeterminateTintMode: JPorterDuff_Mode; cdecl;
    function getMax: Integer; cdecl;
    function getMin: Integer; cdecl; // API 26
    function getProgress: Integer; cdecl;
    function getProgressBackgroundTintList: JColorStateList; cdecl;
    function getProgressBackgroundTintMode: JPorterDuff_Mode; cdecl;
    function getProgressDrawable: JDrawable; cdecl;
    function getProgressTintList: JColorStateList; cdecl;
    function getProgressTintMode: JPorterDuff_Mode; cdecl;
    function getSecondaryProgress: Integer; cdecl;
    function getSecondaryProgressTintList: JColorStateList; cdecl;
    function getSecondaryProgressTintMode: JPorterDuff_Mode; cdecl;
    procedure incrementProgressBy(diff: Integer); cdecl;
    procedure incrementSecondaryProgressBy(diff: Integer); cdecl;
    procedure invalidateDrawable(dr: JDrawable); cdecl;
    function isAnimating: boolean; cdecl;
    procedure jumpDrawablesToCurrentState ; cdecl;
    procedure onRestoreInstanceState(state: JParcelable); cdecl;
    function onSaveInstanceState: JParcelable; cdecl;
    procedure onVisibilityAggregated(isVisible: boolean); cdecl;
    procedure postInvalidate ; cdecl;
    procedure setIndeterminateDrawable(d: JDrawable); cdecl;
    procedure setIndeterminateDrawableTiled(d: JDrawable); cdecl;
    procedure setIndeterminateTintList(tint: JColorStateList); cdecl;
    procedure setIndeterminateTintMode(tintMode: JPorterDuff_Mode); cdecl;
    procedure setInterpolator(context: JContext; resID: Integer); cdecl; overload;
    procedure setMax(max: Integer); cdecl;
    procedure setMin(min: Integer); cdecl; // API 26
    procedure setProgress(progress: Integer); cdecl; overload;
    procedure setProgress(progress: Integer; animate: boolean); cdecl; overload;
    procedure setProgressBackgroundTintList(tint: JColorStateList); cdecl;
    procedure setProgressBackgroundTintMode(tintMode: JPorterDuff_Mode); cdecl;
    procedure setProgressDrawable(d: JDrawable); cdecl;
    procedure setProgressDrawableTiled(d: JDrawable); cdecl;
    procedure setProgressTintList(tint: JColorStateList); cdecl;
    procedure setProgressTintMode(tintMode: JPorterDuff_Mode); cdecl;
    procedure setSecondaryProgressTintList(tint: JColorStateList); cdecl;
    procedure setSecondaryProgressTintMode(tintMode: JPorterDuff_Mode); cdecl;
  end;
  TJProgressBar = class(TJavaGenericImport<JProgressBarClass, JProgressBar>)
  end;

  JAbsSeekBarClass = interface(JProgressBarClass)
    ['{5C813246-82AA-41BE-9123-1780D19A39BC}']
    {class} function init(context: JContext): JAbsSeekBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JAbsSeekBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JAbsSeekBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JAbsSeekBar; cdecl; overload;
  end;

  [JavaSignature('android/widget/AbsSeekBar')]
  JAbsSeekBar = interface(JProgressBar)
    ['{262D8227-656D-45C0-8F52-0E3728728243}']
    procedure drawableHotspotChanged(x: Single; y: Single); cdecl;
    function getAccessibilityClassName: JCharSequence; cdecl;
    function getKeyProgressIncrement: Integer; cdecl;
    function getSplitTrack: boolean; cdecl;
    function getThumb: JDrawable; cdecl;
    function getThumbOffset: Integer; cdecl;
    function getThumbTintList: JColorStateList; cdecl;
    function getThumbTintMode: JPorterDuff_Mode; cdecl;
    function getTickMark: JDrawable; cdecl;
    function getTickMarkTintList: JColorStateList; cdecl;
    function getTickMarkTintMode: JPorterDuff_Mode; cdecl;
    procedure jumpDrawablesToCurrentState ; cdecl;
    function onKeyDown(keyCode: Integer; event: JKeyEvent): boolean; cdecl;
    function onTouchEvent(event: JMotionEvent): boolean; cdecl;
    procedure onRtlPropertiesChanged(layoutDirection: Integer); cdecl;
    procedure setKeyProgressIncrement(increment: Integer); cdecl;
    procedure setSplitTrack(splitTrack: boolean); cdecl;
    procedure setThumb(thumb: JDrawable); cdecl;
    procedure setThumbOffset(thumbOffset: Integer); cdecl;
    procedure setThumbTintList(tint: JColorStateList); cdecl;
    procedure setThumbTintMode(tintMode: JPorterDuff_Mode); cdecl;
    procedure setTickMark(tickMark: JDrawable); cdecl;
    procedure setTickMarkTintList(tint: JColorStateList); cdecl;
    procedure setTickMarkTintMode(tintMode: JPorterDuff_Mode); cdecl;
  end;
  TJAbsSeekBar = class(TJavaGenericImport<JAbsSeekBarClass, JAbsSeekBar>)
  end;

  JSeekBarClass = interface(JAbsSeekBarClass)
    ['{F0985754-B85A-4194-B001-D8374E6AB3BD}']
    {class} function init(context: JContext): JSeekBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JSeekBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer): JSeekBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyleAttr: Integer; defStyleRes: Integer): JSeekBar; cdecl; overload;
  end;

  [JavaSignature('android/widget/SeekBar')]
  JSeekBar = interface(JAbsSeekBar)
    ['{D1B7E80F-CF29-41B6-82B8-89E0FDABD6D5}']
    function getAccessibilityClassName: JCharSequence; cdecl;
    procedure setOnSeekBarChangeListener(listener: JSeekBar_OnSeekBarChangeListener); cdecl;
  end;
  TJSeekBar = class(TJavaGenericImport<JSeekBarClass, JSeekBar>)
  end;

  JSeekBar_OnSeekBarChangeListenerClass = interface(IJavaClass)
    ['{4200BA9D-6E89-4F7E-8A44-00EF76E51A5F}']
  end;

  [JavaSignature('android/widget/SeekBar$OnSeekBarChangeListener')]
  JSeekBar_OnSeekBarChangeListener = interface(IJavaInstance)
    ['{E7BFB9FD-D617-4737-9B22-B6F1F1BF174D}']
    procedure onProgressChanged(seekbar: JSeekBar; progress: Integer; fromUser: boolean); cdecl;
    procedure onStartTrackingTouch(seekbar: JSeekBar); cdecl;
    procedure onStopTrackingTouch(seekbar: JSeekBar); cdecl;
  end;
  TJSeekBar_OnSeekBarChangeListener = class(TJavaGenericImport<JSeekBar_OnSeekBarChangeListenerClass, JSeekBar_OnSeekBarChangeListener>)
  end;

implementation

end.
