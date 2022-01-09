unit DW.NativeShape;

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

uses
  // RTL
  System.Classes, System.Types,
  // FMX
  FMX.Controls.Presentation, FMX.Controls.Model, FMX.Graphics, FMX.Types;

const
  MM_NATIVESHAPE_FILL_CHANGED = MM_USER + 1;
  MM_NATIVESHAPE_STROKE_CHANGED = MM_USER + 2;
  MM_NATIVESHAPE_SIDES_CHANGED = MM_USER + 3;
  MM_NATIVESHAPE_CORNERS_CHANGED = MM_USER + 4;
  MM_NATIVESHAPE_RADIUS_CHANGED = MM_USER + 5;
  MM_NATIVESHAPE_CORNERTYPE_CHANGED = MM_USER + 6;
  MM_NATIVESHAPE_OPACITY_CHANGED = MM_USER + 7;

type
  TCustomNativeShapeModel = class(TDataModel)
  private
    FFill: TBrush;
    FOpacity: Single;
    FStroke: TStrokeBrush;
    FOnChange: TNotifyEvent;
    procedure DoChange;
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure SetFill(const Value: TBrush);
    procedure SetOpacity(const Value: Single);
    procedure SetStroke(const Value: TStrokeBrush);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    property Fill: TBrush read FFill write SetFill;
    property Opacity: Single read FOpacity;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
  end;

  TCustomNativeShape = class(TPresentedControl)
  private
    function GetFill: TBrush;
    function GetModel: TCustomNativeShapeModel; overload;
    function GetStroke: TStrokeBrush;
    procedure ModelChangeHandler(Sender: TObject);
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TStrokeBrush);
  protected
    function CanPaint: Boolean; virtual;
    procedure AfterPaint; override;
    function DefineModelClass: TDataModelClass; override;
    function GetShapeRect: TRectF;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RecalcOpacity; override;
    property Fill: TBrush read GetFill write SetFill;
    property Model: TCustomNativeShapeModel read GetModel;
    property Stroke: TStrokeBrush read GetStroke write SetStroke;
  end;

  TCustomNativeEllipseModel = class(TCustomNativeShapeModel);

  TCustomNativeEllipse = class(TCustomNativeShape)
  private
    function GetModel: TCustomNativeEllipseModel; overload;
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Model: TCustomNativeEllipseModel read GetModel;
  end;

  {$IF CompilerVersion < 35}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroid32Arm or pidAndroid64Arm or pidWin32 or pidWin64)]
  {$ELSE}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroidArm32 or pidAndroidArm64 or pidWin32 or pidWin64)]
  {$ENDIF}
  TNativeEllipse = class(TCustomNativeEllipse)
  published
    property Align;
    property Anchors;
    property Fill;
    property Height;
    property Margins;
    property Opacity;
    property Position;
    property Size;
    property Stroke;
    property Visible default True;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnResized;
  end;

  TCustomNativeRectangleModel = class(TCustomNativeShapeModel)
  private
    FYRadius: Single;
    FXRadius: Single;
    FCorners: TCorners;
    FCornerType: TCornerType;
    FSides: TSides;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetCornerType(const Value: TCornerType); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
  public
    constructor Create(const AOwner: TComponent); override;
    property Corners: TCorners read FCorners write SetCorners;
    property CornerType: TCornerType read FCornerType write SetCornerType;
    property Sides: TSides read FSides write SetSides;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
  end;

  TCustomNativeRectangle = class(TCustomNativeShape)
  private
    function GetCorners: TCorners;
    function GetCornerType: TCornerType;
    function GetSides: TSides;
    function GetXRadius: Single;
    function GetYRadius: Single;
    procedure SetCorners(const Value: TCorners);
    procedure SetCornerType(const Value: TCornerType);
    procedure SetSides(const Value: TSides);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
  protected
    function DefineModelClass: TDataModelClass; override;
    function GetModel: TCustomNativeRectangleModel; overload;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Corners: TCorners read GetCorners write SetCorners;
    property CornerType: TCornerType read GetCornerType write SetCornerType;
    property Model: TCustomNativeRectangleModel read GetModel;
    property Sides: TSides read GetSides write SetSides;
    property XRadius: Single read GetXRadius write SetXRadius;
    property YRadius: Single read GetYRadius write SetYRadius;
  end;

  {$IF CompilerVersion < 35}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroid32Arm or pidAndroid64Arm or pidWin32 or pidWin64)]
  {$ELSE}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroidArm32 or pidAndroidArm64 or pidWin32 or pidWin64)]
  {$ENDIF}
  TNativeRectangle = class(TCustomNativeRectangle)
  published
    property Align;
    property Anchors;
    property Corners stored IsCornersStored;
    property CornerType default TCornerType.Round;
    property Fill;
    property Height;
    property Margins;
    property Opacity;
    property Position;
    property Sides stored IsSidesStored;
    property Size;
    property Stroke;
    property Visible default True;
    property Width;
    property XRadius;
    property YRadius;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnResized;
  end;

procedure Register;

implementation

uses
  // FMX
  FMX.Controls,
  {$IF Defined(IOS)}
  DW.NativeShape.iOS,
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.NativeShape.Android,
  {$ENDIF}
  // RTL
  System.SysUtils, System.IOUtils, System.UITypes, System.Math,
  // FMX
  FMX.Objects;

procedure Register;
begin
  RegisterComponents('Kastri FMX', [TNativeEllipse, TNativeRectangle]);
end;

function GetDrawingShapeRectAndSetThickness(const AShape: TCustomNativeShape; const Fit: Boolean; var FillShape, DrawShape: Boolean;
  var StrokeThicknessRestoreValue: Single): TRectF;
const
  MinRectAreaSize = 0.01;
begin
  FillShape := (AShape.Fill <> nil) and (AShape.Fill.Kind <> TBrushKind.None);
  DrawShape := (AShape.Stroke <> nil) and (AShape.Stroke.Kind <> TBrushKind.None);

  if Fit then
    Result := TRectF.Create(0, 0, 1, 1).FitInto(AShape.LocalRect)
  else
    Result := AShape.LocalRect;

  if DrawShape then
  begin
    if Result.Width < AShape.Stroke.Thickness then
    begin
      StrokeThicknessRestoreValue := AShape.Stroke.Thickness;
      FillShape := False;
      AShape.Stroke.Thickness := Min(Result.Width, Result.Height);
      Result.Left := (Result.Right + Result.Left) * 0.5;
      Result.Right := Result.Left + MinRectAreaSize;
    end
    else
      Result.Inflate(-AShape.Stroke.Thickness * 0.5, 0);

    if Result.Height < AShape.Stroke.Thickness then
    begin
      if StrokeThicknessRestoreValue < 0.0 then
        StrokeThicknessRestoreValue := AShape.Stroke.Thickness;
      FillShape := False;
      AShape.Stroke.Thickness := Min(Result.Width, Result.Height);
      Result.Top := (Result.Bottom + Result.Top) * 0.5;
      Result.Bottom := Result.Top + MinRectAreaSize;
    end
    else
      Result.Inflate(0, -AShape.Stroke.Thickness * 0.5);
  end;
end;

{ TCustomNativeShapeModel }

constructor TCustomNativeShapeModel.Create(const AOwner: TComponent);
begin
  inherited;
  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColors.Null);
  FFill.OnChanged := FillChanged;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
  FStroke.OnChanged := StrokeChanged;
end;

destructor TCustomNativeShapeModel.Destroy;
begin
  FStroke.Free;
  FFill.Free;
  inherited;
end;

procedure TCustomNativeShapeModel.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomNativeShapeModel.FillChanged(Sender: TObject);
begin
  DoChange;
  SendMessage(MM_NATIVESHAPE_FILL_CHANGED);
end;

procedure TCustomNativeShapeModel.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TCustomNativeShapeModel.SetOpacity(const Value: Single);
begin
  FOpacity := Value;
  DoChange;
  SendMessage(MM_NATIVESHAPE_OPACITY_CHANGED);
end;

procedure TCustomNativeShapeModel.SetStroke(const Value: TStrokeBrush);
begin
  FStroke.Assign(Value);
end;

procedure TCustomNativeShapeModel.StrokeChanged(Sender: TObject);
begin
  DoChange;
  SendMessage(MM_NATIVESHAPE_STROKE_CHANGED);
end;

{ TCustomNativeShape }

constructor TCustomNativeShape.Create(AOwner: TComponent);
begin
  inherited;
  Model.OnChange := ModelChangeHandler;
end;

destructor TCustomNativeShape.Destroy;
begin
  //
  inherited;
end;

procedure TCustomNativeShape.AfterPaint;
begin
  //
end;

function TCustomNativeShape.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeShapeModel;
end;

procedure TCustomNativeShape.ModelChangeHandler(Sender: TObject);
begin
  Repaint;
end;

function TCustomNativeShape.GetFill: TBrush;
begin
  Result := Model.Fill;
end;

function TCustomNativeShape.GetModel: TCustomNativeShapeModel;
begin
  Result := inherited GetModel<TCustomNativeShapeModel>;
end;

function TCustomNativeShape.GetStroke: TStrokeBrush;
begin
  Result := Model.Stroke;
end;

procedure TCustomNativeShape.SetFill(const Value: TBrush);
begin
  Model.Fill := Value;
end;

procedure TCustomNativeShape.SetStroke(const Value: TStrokeBrush);
begin
  Model.Stroke := Value;
end;

procedure TCustomNativeShape.RecalcOpacity;
begin
  Model.SetOpacity(Opacity);
end;

function TCustomNativeShape.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

function TCustomNativeShape.CanPaint: Boolean;
begin
  Result := (csDesigning in ComponentState) and not Locked and not FInPaintTo;
end;

function TCustomNativeShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
  if Stroke.Kind <> TBrushKind.None then
    InflateRect(Result, -(Stroke.Thickness / 2), -(Stroke.Thickness / 2));
end;

{ TCustomNativeEllipse }

constructor TCustomNativeEllipse.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
end;

function TCustomNativeEllipse.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeEllipseModel;
end;

function TCustomNativeEllipse.GetModel: TCustomNativeEllipseModel;
begin
  Result := inherited GetModel<TCustomNativeEllipseModel>;
end;

procedure TCustomNativeEllipse.Paint;
begin
  if CanPaint then
  begin
    Canvas.Fill.Assign(Model.Fill);
    Canvas.Stroke.Assign(Model.Stroke);
    Canvas.FillEllipse(TRectF.Create(0, 0, Width, Height), Opacity);
    Canvas.DrawEllipse(TRectF.Create(0, 0, Width, Height), Opacity);
  end;
end;

{ TCustomNativeRectangleModel }

constructor TCustomNativeRectangleModel.Create(const AOwner: TComponent);
begin
  inherited;
  FSides := AllSides;
end;

procedure TCustomNativeRectangleModel.SetCorners(const Value: TCorners);
begin
  if Value <> FCorners then
  begin
    FCorners := Value;
    DoChange;
    SendMessage(MM_NATIVESHAPE_CORNERS_CHANGED);
  end;
end;

procedure TCustomNativeRectangleModel.SetCornerType(const Value: TCornerType);
begin
  if Value <> FCornerType then
  begin
    FCornerType := Value;
    SendMessage(MM_NATIVESHAPE_CORNERTYPE_CHANGED);
    DoChange;
  end;
end;

procedure TCustomNativeRectangleModel.SetSides(const Value: TSides);
begin
  if Value <> FSides then
  begin
    FSides := Value;
    SendMessage(MM_NATIVESHAPE_SIDES_CHANGED);
    DoChange;
  end;
end;

procedure TCustomNativeRectangleModel.SetXRadius(const Value: Single);
begin
  if Value <> FXRadius then
  begin
    FXRadius := Value;
    SendMessage(MM_NATIVESHAPE_RADIUS_CHANGED);
    DoChange;
  end;
end;

procedure TCustomNativeRectangleModel.SetYRadius(const Value: Single);
begin
  if Value <> FYRadius then
  begin
    FYRadius := Value;
    SendMessage(MM_NATIVESHAPE_RADIUS_CHANGED);
    DoChange;
  end;
end;

{ TCustomNativeRectangle }

constructor TCustomNativeRectangle.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
  Model.OnChange := ModelChangeHandler;
end;

function TCustomNativeRectangle.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeRectangleModel;
end;

function TCustomNativeRectangle.GetModel: TCustomNativeRectangleModel;
begin
  Result := inherited GetModel<TCustomNativeRectangleModel>;
end;

function TCustomNativeRectangle.GetCorners: TCorners;
begin
  Result := Model.Corners;
end;

function TCustomNativeRectangle.GetCornerType: TCornerType;
begin
  Result := Model.CornerType;
end;

function TCustomNativeRectangle.GetSides: TSides;
begin
  Result := Model.Sides;
end;

function TCustomNativeRectangle.GetXRadius: Single;
begin
  Result := Model.XRadius;
end;

function TCustomNativeRectangle.GetYRadius: Single;
begin
  Result := Model.YRadius;
end;

function TCustomNativeRectangle.IsCornersStored: Boolean;
begin
  Result := Model.Corners <> AllCorners;
end;

function TCustomNativeRectangle.IsSidesStored: Boolean;
begin
  Result := Model.Sides * AllSides <> AllSides
end;

procedure TCustomNativeRectangle.Paint;
var
  LShapeRect: TRectF;
  LOffset: Single;
  LThickness: Single;
  LNeedsFillRect, LNeedsDrawRect: Boolean;
begin
  if CanPaint then
  begin
    LThickness := Stroke.Thickness;
    try
      LShapeRect := GetDrawingShapeRectAndSetThickness(Self, False, LNeedsFillRect, LNeedsDrawRect, LThickness);
      if Sides <> AllSides then
      begin
        LOffset := LShapeRect.Left;
        if not (TSide.Top in Sides) then
          LShapeRect.Top := LShapeRect.Top - LOffset;
        if not (TSide.Left in Sides) then
          LShapeRect.Left := LShapeRect.Left - LOffset;
        if not (TSide.Bottom in Sides) then
          LShapeRect.Bottom := LShapeRect.Bottom + LOffset;
        if not (TSide.Right in Sides) then
          LShapeRect.Right := LShapeRect.Right + LOffset;
        if LNeedsFillRect then
          Canvas.FillRect(LShapeRect, XRadius, YRadius, Corners, Opacity, Fill, CornerType);
        if LNeedsDrawRect then
          Canvas.DrawRectSides(GetShapeRect, XRadius, YRadius, Corners,  Opacity, Sides, Stroke, CornerType);
      end
      else
      begin
        if LNeedsFillRect then
          Canvas.FillRect(LShapeRect, XRadius, YRadius, Corners, Opacity, Fill, CornerType);
        if LNeedsDrawRect then
          Canvas.DrawRect(LShapeRect, XRadius, YRadius, Corners, Opacity, Stroke, CornerType);
      end;
    finally
      Stroke.Thickness := LThickness;
    end;
  end;
end;


procedure TCustomNativeRectangle.SetCorners(const Value: TCorners);
begin
  Model.Corners := Value;
end;

procedure TCustomNativeRectangle.SetCornerType(const Value: TCornerType);
begin
  Model.CornerType := Value;
end;

procedure TCustomNativeRectangle.SetSides(const Value: TSides);
begin
  Model.Sides := Value;
end;

procedure TCustomNativeRectangle.SetXRadius(const Value: Single);
begin
  Model.XRadius := Value;
end;

procedure TCustomNativeRectangle.SetYRadius(const Value: Single);
begin
  Model.YRadius := Value;
end;

end.
