unit DW.ImagePan;

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


// Based partly on the code that appears during this video:
//   https://www.youtube.com/watch?v=eoKq23M4iwI&ab_channel=QuarkCube
// Note that QuarkCube do not provide source with their demos, so the code has been transcribed from what was presented

interface

uses
  // RTL
  System.Classes, System.Types,
  // FMX
  FMX.Types, FMX.Layouts, FMX.Graphics;

type
  TImagePanOption = (ReverseMouseWheel, ZoomAtMousePos, ResetOnDoubleClick);

  TImagePanOptions = set of TImagePanOption;

  TImagePan = class(TScrollBox)
  private
    FBitmapNorm: TPointF;
    FBitmapScale: Single;
    FImage: TBitmap;
    FImageCenter: TPointF;
    FImageLayout: TLayout;
    FImageSize: TPointF;
    FMousePos: TPointF;
    FMouseService: IFMXMouseService;
    FOptions: TImagePanOptions;
    FZoomDistance: Integer;
    FZoomTimer: TTimer;
    function GetCenter: TPointF;
    function GetImageCenter: TPointF;
    procedure ImageChangeHandler(Sender: TObject);
    procedure ImageLayoutPaintingHandler(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure NormalizeBitmap;
    procedure RecalcCenter;
    procedure SetBitmapScale(const Value: Single; const AIsZoom: Boolean);
    procedure SetImage(const Value: TBitmap);
    procedure ZoomTimerHandler(Sender: TObject);
  protected
    procedure DblClick; override;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure DoResized; override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    property Image: TBitmap read FImage write SetImage;
    property Options: TImagePanOptions read FOptions write FOptions;
  end;

implementation

uses
  // RTL
  System.Math, System.UITypes,
  // FMX
  FMX.Controls, FMX.Platform;

type
  TOpenControl = class(TControl);

{ TImagePan }

constructor TImagePan.Create(AOwner: TComponent);
var
  LCalc: TScrollCalculations;
begin
  inherited;
  TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, FMouseService);
  Touch.InteractiveGestures := [TInteractiveGesture.Zoom, TInteractiveGesture.Pan, TInteractiveGesture.DoubleTap];
  FBitmapScale := 1;
  FZoomTimer := TTimer.Create(Self);
  FZoomTimer.Enabled := False;
  FZoomTimer.Interval := 100;
  FZoomTimer.OnTimer := ZoomTimerHandler;
  FImage := TBitmap.Create;
  FImage.OnChange := ImageChangeHandler;
  FImageLayout := TLayout.Create(Self);
  FImageLayout.OnPainting := ImageLayoutPaintingHandler;
  FImageLayout.Align := TAlignLayout.Center;
  FImageLayout.Parent := Self;
  LCalc := TScrollCalculations.Create(Self);
  try
    LCalc.BoundsAnimation := False;
    LCalc.Animation := True;
    LCalc.AutoShowing := False;
    LCalc.Averaging := False;
    LCalc.TouchTracking := [ttVertical, ttHorizontal];
    AniCalculations.Assign(LCalc);
  finally
    LCalc.Free;
  end;
  DisableMouseWheel := True;
  ShowScrollBars := False;
  FOptions := [TImagePanOption.ZoomAtMousePos, TImagePanOption.ResetOnDoubleClick];
end;

procedure TImagePan.DblClick;
begin
  inherited;
  if TImagePanOption.ResetOnDoubleClick in FOptions then
    Reset;
end;

destructor TImagePan.Destroy;
begin
  FImage.Free;
  inherited;
end;

procedure TImagePan.ZoomTimerHandler(Sender: TObject);
begin
  FZoomTimer.Enabled := False;
  AniCalculations.TouchTracking := [ttVertical, ttHorizontal];
end;

procedure TImagePan.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  LFactor: Single;
begin
  inherited;
  if EventInfo.GestureID = igiZoom then
  begin
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
    begin
      FZoomDistance := EventInfo.Distance;
      // Turn off tracking to stop the "jitters" while zooming
      AniCalculations.TouchTracking := [];
    end;
    if not (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) and not (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
    begin
      FMousePos := PointF(0, 0);
      LFactor := (((EventInfo.Distance - FZoomDistance) * FBitmapScale) / PointF(Width, Height).Length) * 1.2;
      FZoomDistance := EventInfo.Distance;
      SetBitmapScale(FBitmapScale + LFactor, True);
      FZoomTimer.Enabled := True;
    end;
    // This flag does not appear to be set at all on iOS?? (Thus the use of the timer)
    if TInteractiveGestureFlag.gfEnd in EventInfo.Flags then
      AniCalculations.TouchTracking := [ttVertical, ttHorizontal];
  end;
end;

procedure TImagePan.ImageChangeHandler(Sender: TObject);
begin
  NormalizeBitmap;
end;

procedure TImagePan.ImageLayoutPaintingHandler(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  Canvas.DrawBitmap(FImage, RectF(0, 0, FBitmapNorm.X, FBitmapNorm.Y), ARect, 1);
end;

procedure TImagePan.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  inherited;
  if not (TImagePanOption.ReverseMouseWheel in FOptions) then
    WheelDelta := -WheelDelta;
  SetBitmapScale(FBitmapScale + ((WheelDelta * FBitmapScale) / PointF(Width, Height).Length), True);
end;

procedure TImagePan.NormalizeBitmap;
var
  LRect: TRectF;
  LRatio: Single;
begin
  FImageSize := PointF(0, 0);
  FBitmapNorm := PointF(FImage.Width, FImage.Height);
  LRect := RectF(0, 0, FBitmapNorm.X, FBitmapNorm.Y);
  LRect.FitInto(BoundsRect, LRatio);
  SetBitmapScale(1 / LRatio, False);
  Repaint;
end;

function TImagePan.GetCenter: TPointF;
begin
  if (TImagePanOption.ZoomAtMousePos in FOptions) and (FMouseService <> nil) then
    Result := TOpenControl(FImageLayout).ScreenToLocal(FMouseService.GetMousePos)
  else
    Result := FImageCenter;
end;

function TImagePan.GetImageCenter: TPointF;
begin
  Result := FImageLayout.AbsoluteToLocal(LocalToAbsolute(BoundsRect.CenterPoint));
end;

procedure TImagePan.RecalcCenter;
begin
  FImageCenter := GetImageCenter;
end;

procedure TImagePan.Reset;
begin
  NormalizeBitmap;
end;

procedure TImagePan.DoResized;
begin
  inherited;
  SetBitmapScale(FBitmapScale, False);
  RecalcCenter;
end;

procedure TImagePan.ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF; const ContentSizeChanged: boolean);
begin
  RecalcCenter;
  inherited;
end;

procedure TImagePan.SetBitmapScale(const Value: Single; const AIsZoom: Boolean);
var
  LScale, LCurrentScale, LSmallest: Single;
  LCurrentCenter: TPointF;
begin
  LSmallest := Min(Width / FImage.Width, Height / FImage.Height);
  LScale := Min(Max(Value, LSmallest), 20);  // 0.01
  LCurrentScale := FBitmapScale;
  FBitmapScale := LScale;
  if AIsZoom then
    LCurrentCenter := GetCenter * (FBitmapScale / LCurrentScale);
  FImageLayout.BeginUpdate;
  try
    FImageLayout.Width := FBitmapNorm.X * FBitmapScale;
    FImageLayout.Height := FBitmapNorm.Y * FBitmapScale;
  finally
    FImageLayout.EndUpdate;
  end;
  if AIsZoom then
    AniCalculations.ViewportPositionF := AniCalculations.ViewportPositionF - (GetCenter - LCurrentCenter);
end;

procedure TImagePan.SetImage(const Value: TBitmap);
begin
  FImage.Assign(Value);
end;

end.
