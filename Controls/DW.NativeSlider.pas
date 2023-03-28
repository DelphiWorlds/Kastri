unit DW.NativeSlider;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes, System.Types,
  // FMX
  FMX.Controls.Presentation, FMX.Controls.Model, FMX.Graphics;

const
  MM_NATIVESLIDER_GETMAXVALUE = MM_USER + 1;
  MM_NATIVESLIDER_GETMINVALUE = MM_USER + 2;
  MM_NATIVESLIDER_GETVALUE = MM_USER + 3;
  MM_NATIVESLIDER_SETMAXIMAGE = MM_USER + 4;
  MM_NATIVESLIDER_SETMAXVALUE = MM_USER + 5;
  MM_NATIVESLIDER_SETMINIMAGE = MM_USER + 6;
  MM_NATIVESLIDER_SETMINVALUE = MM_USER + 7;
  MM_NATIVESLIDER_SETORIENTATION = MM_USER + 9;
  MM_NATIVESLIDER_SETTHUMBIMAGE = MM_USER + 10;
  MM_NATIVESLIDER_SETVALUE = MM_USER + 11;
  MM_NATIVESLIDER_ENABLED_CHANGED = MM_USER + 12;

type
  TSliderOrientation = (Horizontal, Vertical);

  TSliderImageType = (Minimum, Maximum);

  TCustomNativeSliderModel = class(TDataModel)
  private
    FOrientation: TSliderOrientation;
    FMaxImage: TBitmap;
    FMinImage: TBitmap;
    FValue: Single;
    FOnValueChange: TNotifyEvent;
    procedure SetOrientation(const Value: TSliderOrientation);
    procedure SetValue(const Value: Single);
    procedure SetMaxImage(const Value: TBitmap);
    procedure SetMinImage(const Value: TBitmap);
  protected
    procedure EnabledChanged;
    procedure SliderLoaded;
    property MaxImage: TBitmap read FMaxImage write SetMaxImage;
    property MinImage: TBitmap read FMinImage write SetMinImage;
    property Orientation: TSliderOrientation read FOrientation write SetOrientation;
    property Value: Single read FValue write SetValue;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ValueChanged(const AValue: Single);
  end;

  TCustomNativeSlider = class(TPresentedControl)
  private
    function GetMaxImage: TBitmap;
    function GetMinImage: TBitmap;
    function GetModel: TCustomNativeSliderModel; overload;
    function GetValue: Single;
    function GetOnValueChange: TNotifyEvent;
    function GetOrientation: TSliderOrientation;
    procedure PaintImages;
    procedure PaintSlider;
    procedure SetMaxImage(const Value: TBitmap);
    procedure SetMinImage(const Value: TBitmap);
    procedure SetOnValueChange(const Value: TNotifyEvent);
    procedure SetOrientation(const Value: TSliderOrientation);
    procedure SetValue(const Value: Single);
  protected
    procedure AfterPaint; override;
    function DefineModelClass: TDataModelClass; override;
    procedure EnabledChanged; override;
    procedure Loaded; override;
    procedure Paint; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    property MaxImage: TBitmap read GetMaxImage write SetMaxImage;
    property MinImage: TBitmap read GetMinImage write SetMinImage;
    property Model: TCustomNativeSliderModel read GetModel;
    property Orientation: TSliderOrientation read GetOrientation write SetOrientation;
    property Value: Single read GetValue write SetValue;
    property OnValueChange: TNotifyEvent read GetOnValueChange write SetOnValueChange;
  end;

  {$IF CompilerVersion < 35}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroid32Arm or pidAndroid64Arm)]
  {$ELSE}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroidArm32 or pidAndroidArm64)]
  {$ENDIF}
  TNativeSlider = class(TCustomNativeSlider)
  published
    property Align;
    property Anchors;
    property Height;
    property Margins;
    property MaxImage;
    property MinImage;
    property Orientation;
    property Position;
    property Size;
    property Value;
    property Visible default True;
    property Width;
    property OnValueChange;
  end;

procedure Register;

implementation

uses
  // FMX
  FMX.Controls,
{$IF Defined(IOS)}
  DW.NativeSlider.iOS,
{$ENDIF}
{$IF Defined(ANDROID)}
  DW.NativeSlider.Android,
{$ENDIF}
  System.UITypes;

procedure Register;
begin
  RegisterComponents('Kastri FMX', [TNativeSlider]);
end;

{ TCustomNativeSliderModel }

constructor TCustomNativeSliderModel.Create(const AOwner: TComponent);
begin
  inherited;
  FMaxImage := TBitmap.Create;
  FMinImage := TBitmap.Create;
end;

destructor TCustomNativeSliderModel.Destroy;
begin
  FMaxImage.Free;
  FMinImage.Free;
  inherited;
end;

procedure TCustomNativeSliderModel.EnabledChanged;
begin
  SendMessage(MM_NATIVESLIDER_ENABLED_CHANGED);
end;

procedure TCustomNativeSliderModel.SliderLoaded;
begin
  if not FMaxImage.IsEmpty then
    SendMessage<TBitmap>(MM_NATIVESLIDER_SETMAXIMAGE, FMaxImage);
  if not FMinImage.IsEmpty then
    SendMessage<TBitmap>(MM_NATIVESLIDER_SETMINIMAGE, FMinImage);
end;

procedure TCustomNativeSliderModel.SetMaxImage(const Value: TBitmap);
begin
  FMaxImage.Assign(Value);
  SendMessage<TBitmap>(MM_NATIVESLIDER_SETMAXIMAGE, FMaxImage);
end;

procedure TCustomNativeSliderModel.SetMinImage(const Value: TBitmap);
begin
  FMinImage.Assign(Value);
  SendMessage<TBitmap>(MM_NATIVESLIDER_SETMINIMAGE, FMinImage);
end;

procedure TCustomNativeSliderModel.SetOrientation(const Value: TSliderOrientation);
begin
  if Value <> FOrientation then
  begin
    FOrientation := Value;
    SendMessage<TSliderOrientation>(MM_NATIVESLIDER_SETORIENTATION, FOrientation);
  end;
end;

procedure TCustomNativeSliderModel.SetValue(const Value: Single);
begin
  if (Value <> FValue) and (Value >= 0) and (Value <= 1) then
  begin
    FValue := Value;
    SendMessage<Single>(MM_NATIVESLIDER_SETVALUE, FValue);
  end;
end;

procedure TCustomNativeSliderModel.ValueChanged(const AValue: Single);
begin
  FValue := AValue;
  if Assigned(FOnValueChange) then
    FOnValueChange(Self);
end;

{ TCustomNativeSlider }

constructor TCustomNativeSlider.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
end;

function TCustomNativeSlider.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeSliderModel;
end;

function TCustomNativeSlider.GetMaxImage: TBitmap;
begin
  Result := Model.MaxImage;
end;

function TCustomNativeSlider.GetMinImage: TBitmap;
begin
  Result := Model.MinImage;
end;

function TCustomNativeSlider.GetModel: TCustomNativeSliderModel;
begin
  Result := inherited GetModel<TCustomNativeSliderModel>;
end;

function TCustomNativeSlider.GetOnValueChange: TNotifyEvent;
begin
  Result := Model.OnValueChange;
end;

function TCustomNativeSlider.GetOrientation: TSliderOrientation;
begin
  Result := Model.Orientation;
end;

function TCustomNativeSlider.GetValue: Single;
begin
  Result := Model.Value;
end;

procedure TCustomNativeSlider.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Model.SliderLoaded;
end;

procedure TCustomNativeSlider.AfterPaint;
begin
  //
end;

procedure TCustomNativeSlider.Paint;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    Canvas.BeginScene;
    try
      PaintSlider;
      PaintImages;
    finally
      Canvas.EndScene;
    end;
  end;
end;

procedure TCustomNativeSlider.PaintImages;
var
  LRect: TRectF;
  LMinMargin: Single;
begin
  case Orientation of
    TSliderOrientation.Vertical:
    begin
      LMinMargin := Width - (Width * 0.9);
      LRect := TRectF.Create(0, 0, MaxImage.Width, MaxImage.Height);
      Canvas.DrawBitmap(MaxImage, LRect, LRect.FitInto(TRectF.Create(0, 0, Width, Width)), AbsoluteOpacity);
      LRect := TRectF.Create(0, 0, MinImage.Width, MinImage.Height);
      Canvas.DrawBitmap(MinImage, LRect, LRect.FitInto(TRectF.Create(LMinMargin, (Height - Width) + LMinMargin, Width - LMinMargin, Height- LMinMargin)), AbsoluteOpacity);
    end;
    TSliderOrientation.Horizontal:
    begin
      LMinMargin := Height - (Height * 0.9);
      LRect := TRectF.Create(0, 0, MinImage.Width, MinImage.Height);
      Canvas.DrawBitmap(MinImage, LRect, LRect.FitInto(TRectF.Create(LMinMargin, LMinMargin, Height - LMinMargin, Height - LMinMargin)), AbsoluteOpacity);
      LRect := TRectF.Create(0, 0, MaxImage.Width, MaxImage.Height);
      Canvas.DrawBitmap(MaxImage, LRect, LRect.FitInto(TRectF.Create(Width - Height, 0, Width, Height)), AbsoluteOpacity);
    end;
  end;
end;

procedure TCustomNativeSlider.PaintSlider;
var
  LLineLength, LValuePos: Single;
  LRect: TRectF;
begin
  case Orientation of
    TSliderOrientation.Vertical:
    begin
      LLineLength := Height - (Width * 2) - 8;
      LRect.Left := (Width / 2) - 2;
      LRect.Right := LRect.Left + 4;
      LRect.Top := Width + 4;
      LRect.Bottom := LRect.Top + (LLineLength * (1 - Value));
      Canvas.Fill.Color := TAlphaColors.Lightgrey;
      Canvas.FillRect(LRect, 1);
      LRect.Top := LRect.Bottom;
      LRect.Bottom := LRect.Top + (LLineLength * Value) - 4;
      Canvas.Fill.Color := TAlphaColors.Royalblue;
      Canvas.FillRect(LRect, 1);
      LRect.Top := Width + (LLineLength * (1 - Value)) - 12;
      LRect.Bottom := LRect.Top + 16;
      LRect.Left := (Width / 2) - 8;
      LRect.Right := LRect.Left + 16;
      Canvas.FillEllipse(LRect, 1);
    end;
    TSliderOrientation.Horizontal:
    begin
      LRect.Top := (Height / 2) - 2;
      LRect.Bottom := LRect.Top + 4;
      LLineLength := Width - (Height * 2) - 4;
      LRect.Left := Height + 2;
      LValuePos := LRect.Left + (LLineLength * Value);
      LRect.Right := LValuePos;
      Canvas.Fill.Color := TAlphaColors.Royalblue;
      Canvas.FillRect(LRect, 1);
      LRect.Left := LRect.Right;
      LRect.Right := LRect.Left + (LLineLength * (1 - Value)) - 2;
      Canvas.Fill.Color := TAlphaColors.Lightgrey;
      Canvas.FillRect(LRect, 1);
      LRect.Top := (Height / 2) - 8;
      LRect.Bottom := LRect.Top + 16;
      LRect.Left := LValuePos - 8;
      LRect.Right := LRect.Left + 16;
      Canvas.Fill.Color := TAlphaColors.Royalblue;
      Canvas.FillEllipse(LRect, 1);
    end;
  end;
end;

function TCustomNativeSlider.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

procedure TCustomNativeSlider.EnabledChanged;
begin
  inherited;
  Model.EnabledChanged;
end;

procedure TCustomNativeSlider.SetMaxImage(const Value: TBitmap);
begin
  Model.MaxImage := Value;
end;

procedure TCustomNativeSlider.SetMinImage(const Value: TBitmap);
begin
  Model.MinImage := Value;
end;

procedure TCustomNativeSlider.SetOnValueChange(const Value: TNotifyEvent);
begin
  Model.OnValueChange := Value;
end;

procedure TCustomNativeSlider.SetOrientation(const Value: TSliderOrientation);
begin
  Model.Orientation := Value;
end;

procedure TCustomNativeSlider.SetValue(const Value: Single);
begin
  Model.Value := Value;
  if csDesigning in ComponentState then
    Repaint;
end;

end.
