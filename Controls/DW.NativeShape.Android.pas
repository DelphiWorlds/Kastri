unit DW.NativeShape.Android;

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
  System.TypInfo, System.Classes, System.SysUtils, System.UITypes,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Widget, Androidapi.Helpers, Androidapi.JNI.Net, Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge, Androidapi.JNI, Androidapi.JNI.Support,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.Android, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  // DW
  DW.NativeShape;

type
  TAndroidNativeShape = class(TAndroidNativeView)
  private
    FBackground: JGradientDrawable;
    FView: JImageView;
    function GetShapeControl: TCustomNativeShape;
    function GetModel: TCustomNativeShapeModel; overload;
    procedure MMFillChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_FILL_CHANGED;
    procedure MMStrokeChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_STROKE_CHANGED;
  protected
    function CreateView: JView; override;
    procedure FillChanged; virtual;
    procedure StrokeChanged; virtual;
    function DefineModelClass: TDataModelClass; override;
    property ShapeControl: TCustomNativeShape read GetShapeControl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeShapeModel read GetModel;
    property View: JImageView read FView;
  end;

  TAndroidNativeEllipse = class(TAndroidNativeShape)
  private
    function GetModel: TCustomNativeEllipseModel; overload;
  protected
    function DefineModelClass: TDataModelClass; override;
  public
    constructor Create; override;
    property Model: TCustomNativeEllipseModel read GetModel;
  end;

  TAndroidNativeRectangle = class(TAndroidNativeShape)
  private
    function GetModel: TCustomNativeRectangleModel; overload;
    procedure MMCornersChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_CORNERS_CHANGED;
    procedure MMCornerTypeChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_CORNERTYPE_CHANGED;
    procedure MMRadiusChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_RADIUS_CHANGED;
    procedure MMSidesChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_SIDES_CHANGED;
  protected
    procedure CornersChanged; virtual;
    procedure CornerTypeChanged; virtual;
    function DefineModelClass: TDataModelClass; override;
    procedure RadiusChanged; virtual;
    procedure SidesChanged; virtual;
  public
    constructor Create; override;
    property Model: TCustomNativeRectangleModel read GetModel;
  end;

{ TAndroidNativeShape }

constructor TAndroidNativeShape.Create;
begin
  inherited;
  //
end;

destructor TAndroidNativeShape.Destroy;
begin
  //
  inherited;
end;

function TAndroidNativeShape.CreateView: JView;
begin
  FBackground := TJGradientDrawable.JavaClass.init;
  FView := TJImageView.JavaClass.init(TAndroidHelper.Context);
  // FView.setScaleType(TJImageView_ScaleType.JavaClass.CENTER_CROP); //??
  FView.setBackground(FBackground);
  Result := FView;
end;

function TAndroidNativeShape.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeShapeModel;
end;

function TAndroidNativeShape.GetShapeControl: TCustomNativeShape;
begin
  Result := TCustomNativeShape(Control)
end;

procedure TAndroidNativeShape.MMFillChanged(var AMessage: TDispatchMessage);
begin
  FillChanged;
end;

procedure TAndroidNativeShape.MMStrokeChanged(var AMessage: TDispatchMessage);
begin
  StrokeChanged;
end;

procedure TAndroidNativeShape.FillChanged;
begin
  FBackground.setColor(TAndroidHelper.AlphaColorToJColor(Model.Fill.Color));
end;

procedure TAndroidNativeShape.StrokeChanged;
begin
  FBackground.setStroke(Round(Model.Stroke.Thickness * ScreenScale), TAndroidHelper.AlphaColorToJColor(Model.Stroke.Color));
end;

function TAndroidNativeShape.GetModel: TCustomNativeShapeModel;
begin
  Result := inherited GetModel<TCustomNativeShapeModel>;
end;

{ TAndroidNativeEllipse }

constructor TAndroidNativeEllipse.Create;
begin
  inherited;
  FBackground.setShape(TJGradientDrawable.JavaClass.OVAL);
end;

function TAndroidNativeEllipse.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeEllipseModel;
end;

function TAndroidNativeEllipse.GetModel: TCustomNativeEllipseModel;
begin
  Result := inherited GetModel<TCustomNativeEllipseModel>;
end;

{ TAndroidNativeRectangle }

constructor TAndroidNativeRectangle.Create;
begin
  inherited;
  FBackground.setShape(TJGradientDrawable.JavaClass.RECTANGLE);
end;

function TAndroidNativeRectangle.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeRectangleModel;
end;

function TAndroidNativeRectangle.GetModel: TCustomNativeRectangleModel;
begin
  Result := inherited GetModel<TCustomNativeRectangleModel>;
end;

procedure TAndroidNativeRectangle.MMCornersChanged(var AMessage: TDispatchMessage);
begin
  CornersChanged;
end;

procedure TAndroidNativeRectangle.MMCornerTypeChanged(var AMessage: TDispatchMessage);
begin
  CornerTypeChanged;
end;

procedure TAndroidNativeRectangle.MMRadiusChanged(var AMessage: TDispatchMessage);
begin
  RadiusChanged;
end;

procedure TAndroidNativeRectangle.MMSidesChanged(var AMessage: TDispatchMessage);
begin
  SidesChanged;
end;

procedure TAndroidNativeRectangle.CornersChanged;
begin

end;

procedure TAndroidNativeRectangle.CornerTypeChanged;
begin

end;

procedure TAndroidNativeRectangle.RadiusChanged;
var
  LRadii: TJavaArray<Single>;
  I: Integer;
begin
  LRadii := TJavaArray<Single>.Create(8);
  try
    for I := 0 to LRadii.Length - 1 do
    begin
      LRadii.Items[I] := Model.XRadius;
      LRadii.Items[I + 1] := Model.YRadius;
    end;
    FBackground.setCornerRadii(LRadii);
  finally
    LRadii.Free;
  end;
end;

procedure TAndroidNativeRectangle.SidesChanged;
begin
  // Not supported yet
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeEllipse, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeEllipse>);
  TPresentationProxyFactory.Current.Register(TNativeRectangle, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeRectangle>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeEllipse, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeEllipse>);
  TPresentationProxyFactory.Current.Unregister(TNativeRectangle, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeRectangle>);

end.
