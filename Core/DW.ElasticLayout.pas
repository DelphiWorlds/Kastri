unit DW.ElasticLayout;

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

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Types,
  // FMX
  FMX.Types, FMX.Controls, FMX.Layouts;

type
  TElasticDimension = (Width, Height);

  TElasticDimensions = set of TElasticDimension;

  /// <summary>
  ///   Layout that adjusts its size to accomodate the controls within it
  /// </summary>
  /// <remarks>
  ///   Place the unit name *after* FMX.Layouts in the uses clause of the unit where you have layouts that you want to be elastic
  /// </remarks>
  TLayout = class(FMX.Layouts.TLayout)
  private
    FElasticDimensions: TElasticDimensions;
    FIsElastic: Boolean;
    procedure SetIsElastic(const Value: Boolean);
    procedure SetElasticDimensions(const Value: TElasticDimensions);
  protected
    procedure DoRealign; override;
  public
    /// <summary>
    ///   Set this property to True when you want the layout to adjust its size automatically
    /// </summary>
    /// <remarks>
    ///   This property has been DEPRECATED and may be removed later. Please use ElasticDimensions instead
    /// </remarks>
    property IsElastic: Boolean read FIsElastic write SetIsElastic;
    /// <summary>
    ///   Use this property to indicate which dimensions of the layout should have the size adjusted automatically
    /// </summary>
    property ElasticDimensions: TElasticDimensions read FElasticDimensions write SetElasticDimensions;
  end;

  TFlowLayout = class(FMX.Layouts.TFlowLayout)
  private
    FElasticDimensions: TElasticDimensions;
    FIsElastic: Boolean;
    procedure SetIsElastic(const Value: Boolean);
    procedure SetElasticDimensions(const Value: TElasticDimensions);
  protected
    procedure DoRealign; override;
  public
    property IsElastic: Boolean read FIsElastic write SetIsElastic;
    property ElasticDimensions: TElasticDimensions read FElasticDimensions write SetElasticDimensions;
   end;

  TGridLayout = class(FMX.Layouts.TGridLayout)
  private
    FElasticDimensions: TElasticDimensions;
    FIsElastic: Boolean;
    procedure SetIsElastic(const Value: Boolean);
    procedure SetElasticDimensions(const Value: TElasticDimensions);
  protected
    procedure DoRealign; override;
  public
    property IsElastic: Boolean read FIsElastic write SetIsElastic;
    property ElasticDimensions: TElasticDimensions read FElasticDimensions write SetElasticDimensions;
  end;

  TGridPanelLayout = class(FMX.Layouts.TGridPanelLayout)
  private
    FElasticDimensions: TElasticDimensions;
    FIsElastic: Boolean;
    procedure SetIsElastic(const Value: Boolean);
    procedure SetElasticDimensions(const Value: TElasticDimensions);
  protected
    procedure DoRealign; override;
  public
    property IsElastic: Boolean read FIsElastic write SetIsElastic;
    property ElasticDimensions: TElasticDimensions read FElasticDimensions write SetElasticDimensions;
  end;

  TElasticLayoutHelper = record
  private
    class function GetChildrenOnlyRect(const AControl: TControl): TRectF; static;
  public
    class procedure ElasticRealign(const ALayout: TGridPanelLayout); overload; static;
    class procedure ElasticRealign(const AControl: TControl; const ADimensions: TElasticDimensions); overload; static;
    class procedure ElasticRealign(const AControl: TControl); overload; static;
  end;

implementation

uses
  System.Classes;

type
  TOpenControl = class(TControl);
  TOpenCellItem = class(TGridPanelLayout.TCellItem);

{ TElasticLayoutHelper }

class function TElasticLayoutHelper.GetChildrenOnlyRect(const AControl: TControl): TRectF;
var
  I: Integer;
  LChildControl: TControl;
  LChildrenRect: TRectF;
  LControl: TOpenControl;
begin
  Result := TRectF.Empty;
  LControl := TOpenControl(AControl);
  if not AControl.ClipChildren and (AControl.Controls <> nil) then
  begin
    for I := LControl.GetFirstVisibleObjectIndex to LControl.GetLastVisibleObjectIndex - 1 do
    begin
      LChildControl := LControl.Controls[I];
      if LChildControl.Visible and not (LChildControl.Align in [TAlignLayout.Contents, TAlignLayout.Client]) then
      begin
        LChildrenRect := LChildControl.ChildrenRect;
        LChildrenRect.Top := LChildrenRect.Top - LChildControl.Margins.Top;
        LChildrenRect.Left := LChildrenRect.Left - LChildControl.Margins.Left;
        LChildrenRect.Bottom := LChildrenRect.Bottom + LChildControl.Margins.Bottom;
        LChildrenRect.Right := LChildrenRect.Right + LChildControl.Margins.Right;
        if Result.IsEmpty then
          Result := LChildrenRect
        else
          UnionRectF(Result, Result, LChildrenRect);
      end;
    end
  end;
end;

class procedure TElasticLayoutHelper.ElasticRealign(const ALayout: TGridPanelLayout);
var
  I: Integer;
  LTotalHeight: Single;
begin
  LTotalHeight := 0;
  for I := 0 to ALayout.RowCollection.Count - 1 do
    LTotalHeight := LTotalHeight + TOpenCellItem(ALayout.RowCollection.Items[I]).Size;
  ALayout.Height := LTotalHeight + ALayout.Margins.Rect.Height;
end;

class procedure TElasticLayoutHelper.ElasticRealign(const AControl: TControl);
begin
  ElasticRealign(AControl, [TElasticDimension.Width, TElasticDimension.Height]);
end;

class procedure TElasticLayoutHelper.ElasticRealign(const AControl: TControl; const ADimensions: TElasticDimensions);
var
  LRect: TRectF;
begin
  LRect := GetChildrenOnlyRect(AControl);
  LRect.Inflate(AControl.Padding.Left, AControl.Padding.Top, AControl.Padding.Right, AControl.Padding.Bottom);
  TOpenControl(AControl).FDisableAlign := True;
  try
    if (TElasticDimension.Width in ADimensions) and not (AControl.Align in [TAlignLayout.Top, TAlignLayout.MostTop, TAlignLayout.Bottom, TAlignLayout.MostBottom]) then
      AControl.Width := LRect.Width;
    if (TElasticDimension.Height in ADimensions) and not (AControl.Align in [TAlignLayout.Left, TAlignLayout.MostLeft, TAlignLayout.Right, TAlignLayout.MostRight]) then
      AControl.Height := LRect.Height;
  finally
    TOpenControl(AControl).FDisableAlign := False;
  end;
end;

{ TLayout }

procedure TLayout.DoRealign;
begin
  inherited;
  TElasticLayoutHelper.ElasticRealign(Self, FElasticDimensions);
end;

procedure TLayout.SetElasticDimensions(const Value: TElasticDimensions);
begin
  FElasticDimensions := Value;
  Realign;
end;

procedure TLayout.SetIsElastic(const Value: Boolean);
begin
  if Value <> FIsElastic then
  begin
    FIsElastic := Value;
    if FIsElastic then
      FElasticDimensions := [TElasticDimension.Width, TElasticDimension.Height]
    else
      FElasticDimensions := [];
    Realign;
  end;
end;

{ TFlowLayout }

procedure TFlowLayout.DoRealign;
begin
  inherited;
  if FElasticDimensions <> [] then
    TElasticLayoutHelper.ElasticRealign(Self, FElasticDimensions);
end;

procedure TFlowLayout.SetElasticDimensions(const Value: TElasticDimensions);
begin
  FElasticDimensions := Value;
  Realign;
end;

procedure TFlowLayout.SetIsElastic(const Value: Boolean);
begin
  if Value <> FIsElastic then
  begin
    FIsElastic := Value;
    if FIsElastic then
      FElasticDimensions := [TElasticDimension.Width, TElasticDimension.Height]
    else
      FElasticDimensions := [];
    Realign;
  end;
end;

{ TGridPanelLayout }

procedure TGridPanelLayout.DoRealign;
begin
  inherited;
  if FElasticDimensions <> [] then
    TElasticLayoutHelper.ElasticRealign(Self, FElasticDimensions);
end;

procedure TGridPanelLayout.SetElasticDimensions(const Value: TElasticDimensions);
begin
  FElasticDimensions := Value;
  Realign;
end;

procedure TGridPanelLayout.SetIsElastic(const Value: Boolean);
begin
  if Value <> FIsElastic then
  begin
    FIsElastic := Value;
    if FIsElastic then
      FElasticDimensions := [TElasticDimension.Width, TElasticDimension.Height]
    else
      FElasticDimensions := [];
    Realign;
  end;
end;

{ TGridLayout }

procedure TGridLayout.DoRealign;
begin
  inherited;
  TElasticLayoutHelper.ElasticRealign(Self, FElasticDimensions);
end;

procedure TGridLayout.SetElasticDimensions(const Value: TElasticDimensions);
begin
  FElasticDimensions := Value;
  Realign;
end;

procedure TGridLayout.SetIsElastic(const Value: Boolean);
begin
  if Value <> FIsElastic then
  begin
    FIsElastic := Value;
    if FIsElastic then
      FElasticDimensions := [TElasticDimension.Width, TElasticDimension.Height]
    else
      FElasticDimensions := [];
    Realign;
  end;
end;

end.
