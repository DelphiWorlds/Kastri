unit DW.NativeSlider.iOS;

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
  System.TypInfo, System.Classes, System.Types, System.Math,
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation, iOSapi.CoreGraphics,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.iOS, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  FMX.Graphics, FMX.Helpers.iOS, FMX.Filter.Effects,
  // DW
  DW.NativeSlider;

type
  INativeSlider = interface(UISlider)
    ['{9258442E-A4D6-46A9-9340-E7EADC9FFEA2}']
    procedure ValueChanged(sender: UISlider); cdecl;
  end;

  TiOSNativeSlider = class(TiOSNativeControl)
  private
    FContrast: TFilterContrast;
    FDisabledImage: TBitmap;
    FOrientation: TSliderOrientation;
    function GetModel: TCustomNativeSliderModel; overload;
    function GetScaledBitmap(const ABitmap: TBitmap; const ASize: Integer): TBitmap;
    function GetView: UISlider;
    procedure MMEnabledChanged(var AMessage: TDispatchMessage); message MM_NATIVESLIDER_ENABLED_CHANGED;
    procedure MMGetMaxValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_GETMAXVALUE;
    procedure MMGetMinValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_GETMINVALUE;
    procedure MMGetValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_GETVALUE;
    procedure MMSetMaxImage(var AMessage: TDispatchMessageWithValue<TBitmap>); message MM_NATIVESLIDER_SETMAXIMAGE;
    procedure MMSetMaxValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_SETMAXVALUE;
    procedure MMSetMinImage(var AMessage: TDispatchMessageWithValue<TBitmap>); message MM_NATIVESLIDER_SETMINIMAGE;
    procedure MMSetMinValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_SETMINVALUE;
    procedure MMSetOrientation(var AMessage: TDispatchMessageWithValue<TSliderOrientation>); message MM_NATIVESLIDER_SETORIENTATION;
    procedure MMSetValue(var AMessage: TDispatchMessageWithValue<Single>); message MM_NATIVESLIDER_SETVALUE;
  protected
    function DefineModelClass: TDataModelClass; override;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    { INativeSlider }
    procedure ValueChanged(sender: UISlider); cdecl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeSliderModel read GetModel;
    property View: UISlider read GetView;
  end;

{ TiOSNativeSlider }

constructor TiOSNativeSlider.Create;
begin
  inherited;
  FContrast := TFilterContrast.Create(nil);
  FContrast.Brightness := -0.5;
  FDisabledImage := TBitmap.Create;
  RegisterNativeEventHandler('ValueChanged:', UIControlEventValueChanged);
end;

destructor TiOSNativeSlider.Destroy;
begin
  FContrast.Free;
  FDisabledImage.Free;
  inherited;
end;

function TiOSNativeSlider.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeSliderModel;
end;

function TiOSNativeSlider.GetModel: TCustomNativeSliderModel;
begin
  Result := inherited GetModel<TCustomNativeSliderModel>;
end;

function TiOSNativeSlider.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(INativeSlider)
end;

function TiOSNativeSlider.GetView: UISlider;
begin
  Result := inherited GetView<UISlider>;
end;

procedure TiOSNativeSlider.MMEnabledChanged(var AMessage: TDispatchMessage);
begin
  GetView.setEnabled(TNativeSlider(Control).Enabled);
end;

procedure TiOSNativeSlider.MMGetMaxValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  AMessage.Value := View.maximumValue;
end;

procedure TiOSNativeSlider.MMGetMinValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  AMessage.Value := View.minimumValue;
end;

procedure TiOSNativeSlider.MMGetValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  AMessage.Value := View.value;
end;

function TiOSNativeSlider.GetScaledBitmap(const ABitmap: TBitmap; const ASize: Integer): TBitmap;
var
  LScale: Single;
begin
  LScale := Min(ASize / ABitmap.Width, ASize / ABitmap.Height);
  ABitmap.Resize(Round(ABitmap.Width * LScale), Round(ABitmap.Height * LScale));
  Result := ABitmap;
end;

procedure TiOSNativeSlider.MMSetMaxImage(var AMessage: TDispatchMessageWithValue<TBitmap>);
begin
  View.setMaximumValueImage(BitmapToUIImage(GetScaledBitmap(AMessage.Value, 40)));
end;

procedure TiOSNativeSlider.MMSetMaxValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  View.setMaximumValue(AMessage.Value);
end;

procedure TiOSNativeSlider.MMSetMinImage(var AMessage: TDispatchMessageWithValue<TBitmap>);
begin
  View.setMinimumValueImage(BitmapToUIImage(GetScaledBitmap(AMessage.Value, 25)));
end;

procedure TiOSNativeSlider.MMSetMinValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  View.setMinimumValue(AMessage.Value);
end;

procedure TiOSNativeSlider.MMSetOrientation(var AMessage: TDispatchMessageWithValue<TSliderOrientation>);
begin
  FOrientation := AMessage.Value;
  case FOrientation of
    TSliderOrientation.Horizontal:
      View.setTransform(CGAffineTransformMakeRotation(cPI * -0.5));
    TSliderOrientation.Vertical:
      View.setTransform(CGAffineTransformScale(CGAffineTransformMakeRotation(cPI * 0.5), -1, 1));
  end;
end;

procedure TiOSNativeSlider.MMSetValue(var AMessage: TDispatchMessageWithValue<Single>);
begin
  View.setValue(AMessage.Value);
end;

procedure TiOSNativeSlider.ValueChanged(sender: UISlider);
begin
  Model.ValueChanged(sender.value);
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeSlider, TControlType.Platform, TiOSPresentationProxy<TiOSNativeSlider>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeSlider, TControlType.Platform, TiOSPresentationProxy<TiOSNativeSlider>);

end.
