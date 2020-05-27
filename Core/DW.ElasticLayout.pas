unit DW.ElasticLayout;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Types,
  // FMX
  FMX.Types, FMX.Controls, FMX.Layouts;

type
  /// <summary>
  ///   Layout that adjusts its size to accomodate the controls within it
  /// </summary>
  /// <remarks>
  ///   Place the unit name *after* FMX.Layouts in the uses clause of the unit where you have layouts that you want to be elastic
  /// </remarks>
  TLayout = class(FMX.Layouts.TLayout)
  private
    FIsElastic: Boolean;
    procedure SetIsElastic(const Value: Boolean);
  protected
    procedure DoRealign; override;
  public
    /// <summary>
    ///   Set this property to True when you want the layout to adjust its size automatically
    /// </summary>
    property IsElastic: Boolean read FIsElastic write SetIsElastic;
  end;

  TFlowLayout = class(FMX.Layouts.TFlowLayout)
  private
    FIsElastic: Boolean;
    procedure SetIsElastic(const Value: Boolean);
  protected
    procedure DoRealign; override;
  public
    property IsElastic: Boolean read FIsElastic write SetIsElastic;
  end;

  TElasticLayoutHelper = class helper for TControl
  private
    function GetChildrenOnlyRect: TRectF;
  protected
    procedure ElasticRealign;
  end;

implementation

{ TElasticLayoutHelper }

function TElasticLayoutHelper.GetChildrenOnlyRect: TRectF;
var
  I: Integer;
  Control: TControl;
  LChildrenRect: TRectF;
begin
  Result := TRectF.Empty;
  if not ClipChildren and (Controls <> nil) then
  begin
    for I := GetFirstVisibleObjectIndex to GetLastVisibleObjectIndex - 1 do
    begin
      Control := Controls[I];
      if Control.Visible and not (Control.Align in [TAlignLayout.Contents, TAlignLayout.Client]) then
      begin
        LChildrenRect := Control.ChildrenRect;
        LChildrenRect.Top := LChildrenRect.Top - Control.Margins.Top;
        LChildrenRect.Left := LChildrenRect.Left - Control.Margins.Left;
        LChildrenRect.Bottom := LChildrenRect.Bottom + Control.Margins.Bottom;
        LChildrenRect.Right := LChildrenRect.Right + Control.Margins.Right;
        if Result.IsEmpty then
          Result := LChildrenRect
        else
          UnionRectF(Result, Result, LChildrenRect);
      end;
    end
  end;
end;

procedure TElasticLayoutHelper.ElasticRealign;
var
  LRect: TRectF;
begin
  LRect := GetChildrenOnlyRect;
  LRect.Inflate(Padding.Left, Padding.Top, Padding.Right, Padding.Bottom);
  FDisableAlign := True;
  try
    if not (Align in [TAlignLayout.Top, TAlignLayout.MostTop, TAlignLayout.Bottom, TAlignLayout.MostBottom]) then
      Width := LRect.Width;
    if not (Align in [TAlignLayout.Left, TAlignLayout.MostLeft, TAlignLayout.Right, TAlignLayout.MostRight]) then
      Height := LRect.Height;
  finally
    FDisableAlign := False;
  end;
end;

{ TLayout }

procedure TLayout.DoRealign;
begin
  inherited;
  if FIsElastic then
    ElasticRealign;
end;

procedure TLayout.SetIsElastic(const Value: Boolean);
begin
  if Value = FIsElastic then
    Exit; // <======
  FIsElastic := Value;
  Realign;
end;

{ TFlowLayout }

procedure TFlowLayout.DoRealign;
begin
  inherited;
  if FIsElastic then
    ElasticRealign;
end;

procedure TFlowLayout.SetIsElastic(const Value: Boolean);
begin
  if Value = FIsElastic then
    Exit; // <======
  FIsElastic := Value;
  Realign;
end;

end.
