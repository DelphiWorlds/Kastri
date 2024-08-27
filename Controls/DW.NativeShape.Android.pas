unit DW.NativeShape.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

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
  FMX.Graphics, FMX.Types,
  // DW
  DW.OSLog,
  DW.NativeShape, DW.NativeControl.Android;

type
  JDWRectangleDrawable = interface;

  JDWRectangleDrawableClass = interface(JDrawableClass)
    ['{E2C91459-7CF6-4B29-AFF1-14F67A7D1C87}']
    {class} function _GetSIDE_BOTTOM: Integer; cdecl;
    {class} function _GetSIDE_LEFT: Integer; cdecl;
    {class} function _GetSIDE_NONE: Integer; cdecl;
    {class} function _GetSIDE_RIGHT: Integer; cdecl;
    {class} function _GetSIDE_TOP: Integer; cdecl;
    {class} function init: JDWRectangleDrawable; cdecl;
    {class} property SIDE_BOTTOM: Integer read _GetSIDE_BOTTOM;
    {class} property SIDE_LEFT: Integer read _GetSIDE_LEFT;
    {class} property SIDE_NONE: Integer read _GetSIDE_NONE;
    {class} property SIDE_RIGHT: Integer read _GetSIDE_RIGHT;
    {class} property SIDE_TOP: Integer read _GetSIDE_TOP;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWRectangleDrawable')]
  JDWRectangleDrawable = interface(JShapeDrawable)
    ['{6D84FA16-F0CB-4B8F-8B72-8B2D7E65B4AD}']
    procedure setSides(sides: Integer); cdecl;
    procedure setStroke(strokeWidth: Integer; strokeColor: Integer); cdecl;
  end;
  TJDWRectangleDrawable = class(TJavaGenericImport<JDWRectangleDrawableClass, JDWRectangleDrawable>)
  end;

  TAndroidNativeShape = class(TNativeControl)
  private
    FView: JViewGroup;
    function GetShapeControl: TCustomNativeShape;
    function GetModel: TCustomNativeShapeModel; overload;
    procedure MMFillChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_FILL_CHANGED;
    procedure MMOpacityChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_OPACITY_CHANGED;
    procedure MMStrokeChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_STROKE_CHANGED;
  protected
    function CreateView: JView; override;
    function DefineModelClass: TDataModelClass; override;
    procedure FillChanged; virtual;
    function GetFillNativeColor: Integer;
    function GetStrokeNativeColor: Integer;
    function GetStrokeNativeThickness: Integer;
    procedure StrokeChanged; virtual;
    procedure ViewCreated; virtual;
    property ShapeControl: TCustomNativeShape read GetShapeControl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeShapeModel read GetModel;
    property View: JViewGroup read FView;
  end;

  TAndroidNativeEllipse = class(TAndroidNativeShape)
  private
    FEllipse: JGradientDrawable;
    function GetModel: TCustomNativeEllipseModel; overload;
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure FillChanged; override;
    procedure StrokeChanged; override;
    procedure ViewCreated; override;
  public
    constructor Create; override;
    property Model: TCustomNativeEllipseModel read GetModel;
  end;

  TAndroidNativeRectangle = class(TAndroidNativeShape)
  private
    FRectangle: JDWRectangleDrawable;
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
    procedure StrokeChanged; override;
    procedure ViewCreated; override;
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
var
  LLayout: JLinearLayout;
begin
  LLayout := TJLinearLayout.JavaClass.init(TAndroidHelper.Context);
  LLayout.setOrientation(TJLinearLayout.JavaClass.HORIZONTAL);
  FView := LLayout;
  Result := FView;
  ViewCreated;
end;

function TAndroidNativeShape.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeShapeModel;
end;

function TAndroidNativeShape.GetShapeControl: TCustomNativeShape;
begin
  Result := TCustomNativeShape(Control)
end;

function TAndroidNativeShape.GetStrokeNativeColor: Integer;
var
  LColor: TAlphaColorRec;
begin
  if (Model.Stroke.Color = TAlphaColors.Null) or (Model.Stroke.Kind = TBrushKind.None) or (Model.Opacity = 0) then
    Result := TJColor.JavaClass.TRANSPARENT
  else
  begin
    LColor := TAlphaColorRec(Model.Stroke.Color);
    LColor.A := Round(Model.Opacity * 255);
    Result := TAndroidHelper.AlphaColorToJColor(LColor.Color);
  end;
end;

function TAndroidNativeShape.GetStrokeNativeThickness: Integer;
begin
  Result := Round(Model.Stroke.Thickness * ScreenScale);
end;

procedure TAndroidNativeShape.MMFillChanged(var AMessage: TDispatchMessage);
begin
  FillChanged;
end;

procedure TAndroidNativeShape.MMOpacityChanged(var AMessage: TDispatchMessage);
begin
  FillChanged;
  StrokeChanged;
end;

procedure TAndroidNativeShape.MMStrokeChanged(var AMessage: TDispatchMessage);
begin
  StrokeChanged;
end;

procedure TAndroidNativeShape.FillChanged;
begin
  //
end;

procedure TAndroidNativeShape.StrokeChanged;
begin
  //
end;

procedure TAndroidNativeShape.ViewCreated;
begin
  //
end;

function TAndroidNativeShape.GetFillNativeColor: Integer;
var
  LColor: TAlphaColorRec;
begin
  if (Model.Fill.Color = TAlphaColors.Null) or (Model.Fill.Kind = TBrushKind.None) or (Model.Opacity = 0) then
    Result := TJColor.JavaClass.TRANSPARENT
  else
  begin
    LColor := TAlphaColorRec(Model.Fill.Color);
    LColor.A := Round(Model.Opacity * 255);
    Result := TAndroidHelper.AlphaColorToJColor(LColor.Color);
  end;
end;

function TAndroidNativeShape.GetModel: TCustomNativeShapeModel;
begin
  Result := inherited GetModel<TCustomNativeShapeModel>;
end;

{ TAndroidNativeEllipse }

constructor TAndroidNativeEllipse.Create;
begin
  inherited;
  //
end;

function TAndroidNativeEllipse.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeEllipseModel;
end;

procedure TAndroidNativeEllipse.ViewCreated;
begin
  FEllipse := TJGradientDrawable.JavaClass.init;
  FEllipse.setShape(TJGradientDrawable.JavaClass.OVAL);
  View.setBackground(FEllipse);
end;

function TAndroidNativeEllipse.GetModel: TCustomNativeEllipseModel;
begin
  Result := inherited GetModel<TCustomNativeEllipseModel>;
end;

procedure TAndroidNativeEllipse.FillChanged;
begin
  FEllipse.setColor(GetFillNativeColor);
end;

procedure TAndroidNativeEllipse.StrokeChanged;
begin
  FEllipse.setStroke(GetStrokeNativeThickness, GetStrokeNativeColor);
end;

{ TAndroidNativeRectangle }

constructor TAndroidNativeRectangle.Create;
begin
  inherited;
  //
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
  //
end;

procedure TAndroidNativeRectangle.CornerTypeChanged;
begin
  //
end;

procedure TAndroidNativeRectangle.RadiusChanged;
//var
//  LRadii: TJavaArray<Single>;
//  I: Integer;
begin
//  LRadii := TJavaArray<Single>.Create(8);
//  try
//    for I := 0 to LRadii.Length - 1 do
//    begin
//      LRadii.Items[I] := Model.XRadius;
//      LRadii.Items[I + 1] := Model.YRadius;
//    end;
//    FBackground.setCornerRadii(LRadii);
//  finally
//    LRadii.Free;
//  end;
end;

procedure TAndroidNativeRectangle.SidesChanged;
var
  LSides: Integer;
begin
  LSides := TJDWRectangleDrawable.JavaClass.SIDE_NONE;
  if TSide.Bottom in Model.Sides then
    LSides := LSides or TJDWRectangleDrawable.JavaClass.SIDE_BOTTOM;
  if TSide.Left in Model.Sides then
    LSides := LSides or TJDWRectangleDrawable.JavaClass.SIDE_LEFT;
  if TSide.Right in Model.Sides then
    LSides := LSides or TJDWRectangleDrawable.JavaClass.SIDE_RIGHT;
  if TSide.Top in Model.Sides then
    LSides := LSides or TJDWRectangleDrawable.JavaClass.SIDE_TOP;
  FRectangle.setSides(LSides);
end;

procedure TAndroidNativeRectangle.StrokeChanged;
begin
  FRectangle.setStroke(GetStrokeNativeThickness, GetStrokeNativeColor);
end;

procedure TAndroidNativeRectangle.ViewCreated;
begin
  FRectangle := TJDWRectangleDrawable.JavaClass.init;
  View.setBackground(FRectangle);
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeEllipse, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeEllipse>);
  TPresentationProxyFactory.Current.Register(TNativeRectangle, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeRectangle>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeEllipse, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeEllipse>);
  TPresentationProxyFactory.Current.Unregister(TNativeRectangle, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeRectangle>);

end.
