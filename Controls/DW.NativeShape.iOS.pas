unit DW.NativeShape.iOS;

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
  System.TypInfo, System.Classes, System.SysUtils, System.Math, System.Types, System.UITypes,
  // macOS
  Macapi.Helpers, Macapi.CoreFoundation,
  // iOS
  iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.QuartzCore, iOSapi.Foundation, iOSapi.CocoaTypes,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.iOS, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  FMX.Helpers.iOS, FMX.Types,
  // DW
  DW.NativeShape;

type
  TiOSNativeShape = class(TiOSNativeControl)
  private
    FFillPath: UIBezierPath;
    FFillLayer: CAShapeLayer;
    FShapeLayer: CAShapeLayer;
    FShapePath: UIBezierPath;
    function GetCGColorRef(const AColor: TAlphaColor): CGColorRef;
    function GetShapeControl: TCustomNativeShape;
    function GetView: UIView;
    function GetModel: TCustomNativeShapeModel; overload;
    procedure MMFillChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_FILL_CHANGED;
    procedure MMStrokeChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_STROKE_CHANGED;
  protected
    procedure FillChanged; virtual;
    procedure PathChanged;
    procedure StrokeChanged; virtual;
    function DefineModelClass: TDataModelClass; override;
    property FillPath: UIBezierPath read FFillPath write FFillPath;
    property ShapePath: UIBezierPath read FShapePath;
    property ShapeControl: TCustomNativeShape read GetShapeControl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeShapeModel read GetModel;
    property View: UIView read GetView;
  end;

  INativeEllipse = interface(UIView)
    ['{86101D06-0949-4ADF-961A-66D2ECB00CC0}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  TiOSNativeEllipse = class(TiOSNativeShape)
  private
    function GetModel: TCustomNativeEllipseModel; overload;
  protected
    function DefineModelClass: TDataModelClass; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure SetSize(const ASize: TSizeF); override;
  public
    constructor Create; override;
    property Model: TCustomNativeEllipseModel read GetModel;
  end;

  INativeRectangle = interface(UIView)
    ['{2E9C5100-8635-4EED-9EDC-CBE40347C926}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  TiOSNativeRectangle = class(TiOSNativeShape)
  private
    function GetModel: TCustomNativeRectangleModel; overload;
    procedure MMCornersChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_CORNERS_CHANGED;
    procedure MMCornerTypeChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_CORNERTYPE_CHANGED;
    procedure MMRadiusChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_RADIUS_CHANGED;
    procedure MMSidesChanged(var AMessage: TDispatchMessage); message MM_NATIVESHAPE_SIDES_CHANGED;
    procedure UpdatePath;
  protected
    function DefineModelClass: TDataModelClass; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure SetSize(const ASize: TSizeF); override;
  public
    constructor Create; override;
    property Model: TCustomNativeRectangleModel read GetModel;
  end;

const
  cPI = 3.141592654;
  cTopRightStartAngle = cPI * 3 / 2;
  cTopRightEndAngle = 0;
  cBottomRightStartAngle = 0;
  cBottomRightEndAngle = cPI / 2;
  cBottomLeftStartAngle = cPI / 2;
  cBottomLeftEndAngle = cPI;
  cTopLeftStartAngle = cPI;
  cTopLeftEndAngle = cPI * 3 / 2;

{ TiOSNativeShape }

constructor TiOSNativeShape.Create;
begin
  inherited;
  FFillPath := TUIBezierPath.Wrap(TUIBezierPath.OCClass.bezierPath);
  FFillLayer := TCAShapeLayer.Create;
  View.layer.addSublayer(FFillLayer);
  FShapePath := TUIBezierPath.Wrap(TUIBezierPath.OCClass.bezierPath);
  FShapeLayer := TCAShapeLayer.Create;
  FShapeLayer.setFillColor(nil);
  View.layer.addSublayer(FShapeLayer);
end;

destructor TiOSNativeShape.Destroy;
begin
  //
  inherited;
end;

function TiOSNativeShape.GetCGColorRef(const AColor: TAlphaColor): CGColorRef;
begin
  if AColor = TAlphaColors.Null then
    Result := nil
  else
    Result := AlphaColorToUIColor(AColor).CGColor;
end;

function TiOSNativeShape.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeShapeModel;
end;

function TiOSNativeShape.GetShapeControl: TCustomNativeShape;
begin
  Result := TCustomNativeShape(Control)
end;

function TiOSNativeShape.GetView: UIView;
begin
  Result := inherited GetView<UIView>;
end;

procedure TiOSNativeShape.MMFillChanged(var AMessage: TDispatchMessage);
begin
  FillChanged;
end;

procedure TiOSNativeShape.MMStrokeChanged(var AMessage: TDispatchMessage);
begin
  StrokeChanged;
end;

procedure TiOSNativeShape.FillChanged;
begin
  FFillLayer.setFillColor(GetCGColorRef(Model.Fill.Color));
end;

procedure TiOSNativeShape.PathChanged;
begin
  FShapeLayer.setPath(FShapePath.CGPath);
  FFillLayer.setPath(FFillPath.CGPath);
  FillChanged;
  StrokeChanged;
end;

procedure TiOSNativeShape.StrokeChanged;
begin
  FShapeLayer.setStrokeColor(GetCGColorRef(Model.Stroke.Color));
  FShapeLayer.setLineWidth(Model.Stroke.Thickness);
end;

function TiOSNativeShape.GetModel: TCustomNativeShapeModel;
begin
  Result := inherited GetModel<TCustomNativeShapeModel>;
end;

{ TiOSNativeEllipse }

constructor TiOSNativeEllipse.Create;
begin
  inherited;
  //
end;

function TiOSNativeEllipse.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(INativeEllipse);
end;

function TiOSNativeEllipse.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeEllipseModel;
end;

function TiOSNativeEllipse.GetModel: TCustomNativeEllipseModel;
begin
  Result := inherited GetModel<TCustomNativeEllipseModel>;
end;

procedure TiOSNativeEllipse.SetSize(const ASize: TSizeF);
var
  LPoint: CGPoint;
begin
  inherited;
  FillPath.removeAllPoints;
  ShapePath.removeAllPoints;
  LPoint.x := Size.cx / 2;
  LPoint.y := Size.cy / 2;
  FillPath.addArcWithCenter(LPoint, Min(LPoint.x, LPoint.y), 0, cPI * 2, True);
  ShapePath.addArcWithCenter(LPoint, Min(LPoint.x, LPoint.y), 0, cPI * 2, True);
  PathChanged;
end;

{ TiOSNativeRectangle }

constructor TiOSNativeRectangle.Create;
begin
  inherited;
  //
end;

function TiOSNativeRectangle.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(INativeRectangle);
end;

function TiOSNativeRectangle.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeRectangleModel;
end;

function TiOSNativeRectangle.GetModel: TCustomNativeRectangleModel;
begin
  Result := inherited GetModel<TCustomNativeRectangleModel>;
end;

procedure TiOSNativeRectangle.MMCornersChanged(var AMessage: TDispatchMessage);
begin
  UpdatePath;
end;

procedure TiOSNativeRectangle.MMCornerTypeChanged(var AMessage: TDispatchMessage);
begin
  UpdatePath;
end;

procedure TiOSNativeRectangle.MMRadiusChanged(var AMessage: TDispatchMessage);
begin
  UpdatePath;
end;

procedure TiOSNativeRectangle.MMSidesChanged(var AMessage: TDispatchMessage);
begin
  UpdatePath;
end;

procedure TiOSNativeRectangle.SetSize(const ASize: TSizeF);
begin
  inherited;
  UpdatePath;
end;

procedure TiOSNativeRectangle.UpdatePath;
var
  LXR, LYR: Single;
begin
  LXR := Model.XRadius;
  LYR := Model.YRadius;
  ShapePath.removeAllPoints;
  FillPath := TUIBezierPath.Wrap(TUIBezierPath.OCClass.bezierPathWithRect(CGRectMake(0, 0, Size.cx, Size.cy)));
  // FillPath := TUIBezierPath.Wrap(TUIBezierPath.OCClass.bezierPathWithRoundedRect(CGRectMake(0, 0, Size.cx, Size.cy), LCorners, LRadii));
  if TSide.Top in Model.Sides then
  begin
    ShapePath.moveToPoint(CGPointMake(0, 0));
    ShapePath.addLineToPoint(CGPointMake(Size.cx, 0));
    // Top Right corner
    // ShapePath.addArcWithCenter(CGPointMake(Size.cx - LXR, LYR), - cPI / 2, 0, True);
  end;
  if TSide.Left in Model.Sides then
  begin
    ShapePath.moveToPoint(CGPointMake(0, 0));
    ShapePath.addLineToPoint(CGPointMake(0, Size.cy));
  end;
  if TSide.Right in Model.Sides then
  begin
    ShapePath.moveToPoint(CGPointMake(Size.cx, 0));
    ShapePath.addLineToPoint(CGPointMake(Size.cx, Size.cy));
  end;
  if TSide.Bottom in Model.Sides then
  begin
    ShapePath.moveToPoint(CGPointMake(0, Size.cy));
    ShapePath.addLineToPoint(CGPointMake(Size.cx, Size.cy));
  end;
  PathChanged;
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeEllipse, TControlType.Platform, TiOSPresentationProxy<TiOSNativeEllipse>);
  TPresentationProxyFactory.Current.Register(TNativeRectangle, TControlType.Platform, TiOSPresentationProxy<TiOSNativeRectangle>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeEllipse, TControlType.Platform, TiOSPresentationProxy<TiOSNativeEllipse>);
  TPresentationProxyFactory.Current.Unregister(TNativeRectangle, TControlType.Platform, TiOSPresentationProxy<TiOSNativeRectangle>);

end.
