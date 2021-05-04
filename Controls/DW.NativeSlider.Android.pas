unit DW.NativeSlider.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

implementation

uses
  // RTL
  System.Types,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Widget,
  Androidapi.JNI.Util,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.Android, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  FMX.Graphics, FMX.Types,
  // DW
  DW.OSLog,
  DW.Androidapi.JNI.SeekBar, DW.NativeSlider, DW.Graphics.Helpers.Android;

type
  JDWSeekBar = interface;

  JDWSeekBarClass = interface(JSeekBarClass)
    ['{D273A6D6-56EE-46E7-BDD1-552648941C7D}']
    {class} function _GetROTATION_0: Integer; cdecl;
    {class} function _GetROTATION_180: Integer; cdecl;
    {class} function _GetROTATION_270: Integer; cdecl;
    {class} function _GetROTATION_90: Integer; cdecl;
    {class} function init(context: JContext): JDWSeekBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet): JDWSeekBar; cdecl; overload;
    {class} function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JDWSeekBar; cdecl; overload;
    {class} property ROTATION_0: Integer read _GetROTATION_0;
    {class} property ROTATION_180: Integer read _GetROTATION_180;
    {class} property ROTATION_270: Integer read _GetROTATION_270;
    {class} property ROTATION_90: Integer read _GetROTATION_90;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWSeekBar')]
  JDWSeekBar = interface(JSeekBar)
    ['{6402C296-C05B-4B6C-969E-0C36E662DD23}']
    procedure setCenterOffsetFactor(factor: Double); cdecl;
    procedure setRotation(rotation: Integer); cdecl;
  end;
  TJDWSeekBar = class(TJavaGenericImport<JDWSeekBarClass, JDWSeekBar>)
  end;

  TAndroidNativeSlider = class;

  TSeekBarChangeListener = class(TJavaLocal, JSeekBar_OnSeekBarChangeListener)
  private
    FSlider: TAndroidNativeSlider;
  public
    { JSeekBar_OnSeekBarChangeListener }
    procedure onProgressChanged(seekbar: JSeekBar; progress: Integer; fromUser: boolean) ; cdecl;
    procedure onStartTrackingTouch(seekbar: JSeekBar) ; cdecl;
    procedure onStopTrackingTouch(seekbar: JSeekBar) ; cdecl;
  public
    constructor Create(const ASlider: TAndroidNativeSlider);
  end;

  TAndroidNativeSlider = class(TAndroidNativeView)
  private
    FChangeListener: JSeekBar_OnSeekBarChangeListener;
    FImageMax: JImageView;
    FImageMin: JImageView;
    FOrientation: TSliderOrientation;
    FSeekBar: JDWSeekBar;
    FView: JLinearLayout;
    procedure AlignView(const AView: JView; const AWidth, AHeight: Single; const AAlign: TAlignLayout);
    function GetModel: TCustomNativeSliderModel; overload;
    procedure MMGetMaxValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_GETMAXVALUE;
    procedure MMGetMinValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_GETMINVALUE;
    procedure MMGetValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_GETVALUE;
    procedure MMSetMaxImage(var AMessage: TDispatchMessageWithValue<TBitmap>); message MM_NATIVESLIDER_SETMAXIMAGE;
    procedure MMSetMaxValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_SETMAXVALUE;
    procedure MMSetMinImage(var AMessage: TDispatchMessageWithValue<TBitmap>); message MM_NATIVESLIDER_SETMINIMAGE;
    procedure MMSetMinValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_SETMINVALUE;
    procedure MMSetOrientation(var AMessage: TDispatchMessageWithValue<TSliderOrientation>); message MM_NATIVESLIDER_SETORIENTATION;
    procedure MMSetValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_SETVALUE;
    procedure UpdateAlignment(const ASize: TSizeF);
  protected
    function CreateView: JView; override;
    function DefineModelClass: TDataModelClass; override;
    function ProcessTouch(view: JView; event: JMotionEvent): Boolean; override;
    procedure ProgressChanged(const AProgress: Integer);
    procedure SetSize(const ASize: TSizeF); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeSliderModel read GetModel;
    property View: JLinearLayout read FView;
  end;

{ TSeekBarChangeListener }

constructor TSeekBarChangeListener.Create(const ASlider: TAndroidNativeSlider);
begin
  inherited Create;
  FSlider := ASlider;
end;

procedure TSeekBarChangeListener.onProgressChanged(seekbar: JSeekBar; progress: Integer; fromUser: boolean);
begin
  if fromUser then
    FSlider.ProgressChanged(progress);
end;

procedure TSeekBarChangeListener.onStartTrackingTouch(seekbar: JSeekBar);
begin
  //
end;

procedure TSeekBarChangeListener.onStopTrackingTouch(seekbar: JSeekBar);
begin
  //
end;

{ TAndroidNativeSlider }

constructor TAndroidNativeSlider.Create;
begin
  inherited;
  //
end;

destructor TAndroidNativeSlider.Destroy;
begin
  //
  inherited;
end;

function TAndroidNativeSlider.CreateView: JView;
begin
  FView := TJLinearLayout.JavaClass.init(TAndroidHelper.Context);
  FImageMax := TJImageView.JavaClass.init(TAndroidHelper.Context);
  FImageMax.setScaleType(TJImageView_ScaleType.JavaClass.FIT_CENTER);
  FImageMin := TJImageView.JavaClass.init(TAndroidHelper.Context);
  FImageMin.setScaleType(TJImageView_ScaleType.JavaClass.FIT_CENTER);
  if FChangeListener = nil then
    FChangeListener := TSeekBarChangeListener.Create(Self);
  FSeekBar:= TJDWSeekBar.JavaClass.init(TAndroidHelper.Context);
  FSeekBar.setOnSeekBarChangeListener(FChangeListener);
  Result := FView;
end;

function TAndroidNativeSlider.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeSliderModel;
end;

function TAndroidNativeSlider.GetModel: TCustomNativeSliderModel;
begin
  Result := inherited GetModel<TCustomNativeSliderModel>;
end;

function TAndroidNativeSlider.ProcessTouch(view: JView; event: JMotionEvent): Boolean;
begin
  inherited;
  Result := False;
end;

procedure TAndroidNativeSlider.ProgressChanged(const AProgress: Integer);
begin
  Model.ValueChanged(AProgress / FSeekBar.getMax);
end;

procedure TAndroidNativeSlider.MMGetMaxValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  AMessage.Value := FSeekBar.getMax;
end;

procedure TAndroidNativeSlider.MMGetMinValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  // AMessage.Value := View.getMin; // API 26 - else 0?
  AMessage.Value := 0;
end;

procedure TAndroidNativeSlider.MMGetValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  AMessage.Value := FSeekBar.getProgress / FSeekBar.getMax;
end;

procedure TAndroidNativeSlider.MMSetValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  FSeekBar.setProgress(Trunc(AMessage.Value * FSeekBar.getMax));
end;

procedure TAndroidNativeSlider.MMSetMaxValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  FSeekBar.setMax(Trunc(AMessage.Value * 100));
end;

procedure TAndroidNativeSlider.MMSetMaxImage(var AMessage: TDispatchMessageWithValue<TBitmap>);
begin
  FImageMax.setImageBitmap(AMessage.Value.ToJBitmap);
end;

procedure TAndroidNativeSlider.MMSetMinImage(var AMessage: TDispatchMessageWithValue<TBitmap>);
begin
  FImageMin.setImageBitmap(AMessage.Value.ToJBitmap);
end;

procedure TAndroidNativeSlider.MMSetMinValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  // View.setMin(AMessage.Value);
end;

procedure TAndroidNativeSlider.AlignView(const AView: JView; const AWidth, AHeight: Single; const AAlign: TAlignLayout);
var
  LLayoutParams: JLinearLayout_LayoutParams;
  LLayoutWidth, LLayoutHeight, LWeight: Integer;
begin
  LWeight := 0;
  LLayoutWidth := Round(AWidth * ScreenScale);
  LLayoutHeight := Round(AHeight * ScreenScale);
  if AAlign in [TAlignLayout.Top, TAlignLayout.Bottom] then
    LLayoutWidth := TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT;
  if AAlign in [TAlignLayout.Left, TAlignLayout.Right] then
    LLayoutHeight := TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT;
  if AAlign = TAlignLayout.Client then
  begin
    LWeight := 1;
    case FOrientation of
      TSliderOrientation.Horizontal:
      begin
        LLayoutWidth := TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT;
        LLayoutHeight := TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT;
      end;
      TSliderOrientation.Vertical:
      begin
        LLayoutWidth := TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT;
        LLayoutHeight := TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT;
      end;
    end;
  end;
  LLayoutParams := TJLinearLayout_LayoutParams.JavaClass.init(LLayoutWidth, LLayoutHeight, LWeight);
  LLayoutParams.gravity := TJGravity.JavaClass.CENTER;
  AView.setLayoutParams(LLayoutParams);
end;

procedure TAndroidNativeSlider.MMSetOrientation(var AMessage: TDispatchMessageWithValue<TSliderOrientation>);
begin
  FOrientation := AMessage.Value;
  if Control.Parent <> nil then
  begin
    UpdateAlignment(Size);
    UpdateFrame;
  end;
end;

procedure TAndroidNativeSlider.UpdateAlignment(const ASize: TSizeF);
begin
  FView.removeAllViews;
  case FOrientation of
    TSliderOrientation.Horizontal:
    begin
      FView.setOrientation(TJLinearLayout.JavaClass.HORIZONTAL);
      FSeekBar.setRotation(TJDWSeekBar.JavaClass.ROTATION_0);
      FView.addView(FImageMin);
      FView.addView(FSeekBar);
      FView.addView(FImageMax);
      AlignView(FImageMin, ASize.cy, ASize.cy, TAlignLayout.Left);
      AlignView(FImageMax, ASize.cy, ASize.cy, TAlignLayout.Right);
      AlignView(FSeekBar, ASize.cx, ASize.cy, TAlignLayout.Client);
    end;
    TSliderOrientation.Vertical:
    begin
      FView.setOrientation(TJLinearLayout.JavaClass.VERTICAL);
      FSeekBar.setRotation(TJDWSeekBar.JavaClass.ROTATION_270);
      FView.addView(FImageMax);
      FView.addView(FSeekBar);
      FView.addView(FImageMin);
      AlignView(FImageMax, ASize.cx, ASize.cx, TAlignLayout.Top);
      AlignView(FImageMin, ASize.cx, ASize.cx, TAlignLayout.Bottom);
      AlignView(FSeekBar, ASize.cx, ASize.cy, TAlignLayout.Client);
    end;
  end;
end;

procedure TAndroidNativeSlider.SetSize(const ASize: TSizeF);
begin
  if Control.Parent <> nil then
    UpdateAlignment(ASize);
  inherited;
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeSlider, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeSlider>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeSlider, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeSlider>);

end.
