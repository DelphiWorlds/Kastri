unit DW.ListView.Helper;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{          Copyright(c) 2020 David Nottage              }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Types, System.Classes,
  // FMX
  FMX.ListView, FMX.ListView.Appearances, FMX.ListView.Types, FMX.StdCtrls;

type
  TListView = class(FMX.ListView.TListView)
  private
    FNoItemsLabel: TLabel;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EndUpdate; override;
    property NoItemsLabel: TLabel read FNoItemsLabel;
  end;

  TListViewHelper = class helper for TListView
  private
    function ItemWidth: Single;
    function GetObjectAbsoluteX(const AObject: TCommonObjectAppearance): Single;
    procedure InternalStretchObject(const AObject: TCommonObjectAppearance; const AStretchTo: Single);
  public
    /// <summary>
    ///   Sets the search filter manually
    /// </summary>
    procedure ApplySearchFilter(const AFilter: string);
    /// <summary>
    ///   Returns the number of rows that will be needed to fill the current height of the listview
    /// </summary>
    function FillRowCount: Integer;
    function FindAppearanceObject(const AName: string; var AObject: TCommonObjectAppearance): Boolean;
    /// <summary>
    ///   Gets the item at the given point (if there is one)
    /// </summary>
    function GetItemAt(const APoint: TPointF): TListViewItem;
    /// <summary>
    ///   Returns the total height of individual items, including the header and footer
    /// </summary>
    function ItemTotalHeight: Integer;
    /// <summary>
    ///   Select the first item (optionally if no other item selected)
    /// </summary>
    procedure SelectFirst(const AForce: Boolean = True);
    /// <summary>
    ///   Select the next item
    /// </summary>
    procedure SelectNext(const ACanCycle: Boolean = True);
    /// <summary>
    ///   Select the prior item
    /// </summary>
    procedure SelectPrior(const ACanCycle: Boolean = True);
    /// <summary>
    ///   Returns the maximum scroll view position
    /// </summary>
    function ScrollViewPosMax: Single;
    procedure StretchObject(const AName: string; const AToName: string = '');
  end;

  TListViewItemHelper = class helper for TListViewItem
  public
    /// <summary>
    ///   Convenience function for retrieving text items
    /// </summary>
    function ItemText(const AName: string): TListItemText;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // FMX
  FMX.Types;

type
  TOpenListView = class(TListView);

{ TListView }

constructor TListView.Create(AOwner: TComponent);
begin
  inherited;
  FNoItemsLabel := TLabel.Create(Self);
  FNoItemsLabel.Align := TAlignLayout.Center;
  FNoItemsLabel.TextSettings.Font.Size := 14;
  FNoItemsLabel.TextSettings.HorzAlign := TTextAlign.Center;
  FNoItemsLabel.Visible := False;
  FNoItemsLabel.Parent := Self;
end;

procedure TListView.EndUpdate;
begin
  inherited;
  if not (csDestroying in ComponentState) and (FNoItemsLabel <> nil) then
    FNoItemsLabel.Visible := Items.Count = 0;
end;

procedure TListView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FNoItemsLabel) and (Operation = TOperation.opRemove) then
    FNoItemsLabel := nil;
end;

{ TListViewHelper }

procedure TListViewHelper.ApplySearchFilter(const AFilter: string);
begin
  TOpenListView(Self).SetSearchFilter(AFilter);
end;

function TListViewHelper.FillRowCount: Integer;
begin
  if ItemTotalHeight > 0 then
    Result := Round(Height / ItemTotalHeight)
  else
    Result := 0;
end;

function TListViewHelper.GetItemAt(const APoint: TPointF): TListViewItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if GetItemRect(I).Contains(APoint) then
       Exit(Items[I]);
  end;
end;

function TListViewHelper.ItemTotalHeight: Integer;
begin
  Result := {ItemAppearance.HeaderHeight + } ItemAppearance.ItemHeight {+ ItemAppearance.FooterHeight};
end;

function TListViewHelper.ScrollViewPosMax: Single;
begin
  Result := (Items.Count * ItemTotalHeight) - Height;
end;

procedure TListViewHelper.SelectFirst(const AForce: Boolean = True);
begin
  if (Items.Count > 0) and (AForce or (Selected = nil)) then
    Selected := Items[0];
end;

procedure TListViewHelper.SelectNext(const ACanCycle: Boolean = True);
begin
  if Items.Count > 0 then
  begin
    if Selected <> nil then
    begin
      // If selected is not the last item, select the next one
      if Selected.Index < Items.Count - 1 then
        Selected := Items[Selected.Index + 1]
      // Otherwise if can cycle, then select the first one
      else if ACanCycle then
        Selected := Items[0];
    end
    else
      Selected := Items[0];
  end;
end;

procedure TListViewHelper.SelectPrior(const ACanCycle: Boolean);
begin
  if Items.Count > 0 then
  begin
    if Selected <> nil then
    begin
      // If selected is not the first item, select the prior one
      if Selected.Index > 0 then
        Selected := Items[Selected.Index - 1]
      // Otherwise if can cycle, then select the last one
      else if ACanCycle then
        Selected := Items[Items.Count - 1];
    end
    else
      Selected := Items[Items.Count - 1];
  end;
end;

function TListViewHelper.FindAppearanceObject(const AName: string; var AObject: TCommonObjectAppearance): Boolean;
var
  LObject: TCommonObjectAppearance;
begin
  Result := False;
  for LObject in ItemAppearanceObjects.ItemObjects.Objects do
  begin
    if LObject.Name.Equals(AName) then
    begin
      AObject := LObject;
      Result := True;
      Break;
    end;
  end;
end;

// TODO: Unit Test
function TListViewHelper.GetObjectAbsoluteX(const AObject: TCommonObjectAppearance): Single;
begin
  case AObject.Align of
    TListItemAlign.Leading:
      Result := AObject.PlaceOffset.X;
    TListItemAlign.Center:
      Result := (ItemWidth / 2) - (AObject.Width / 2) + AObject.PlaceOffset.X;
    TListItemAlign.Trailing:
      Result := ItemWidth + AObject.PlaceOffset.X - AObject.Width;
  else
    Result := AObject.PlaceOffset.X;
  end;
end;

function TListViewHelper.ItemWidth: Single;
begin
  Result := Width - 10; //!!! Need to determine item width properly
end;

procedure TListViewHelper.InternalStretchObject(const AObject: TCommonObjectAppearance; const AStretchTo: Single);
begin
  case AObject.Align of
    TListItemAlign.Leading:
      AObject.Width := AStretchTo - AObject.PlaceOffset.X - 10;
    // Implement the others when needed
  end;
end;

procedure TListViewHelper.StretchObject(const AName: string; const AToName: string = '');
var
  LObject, LToObject: TCommonObjectAppearance;
  LStretchTo: Single;
begin
  if FindAppearanceObject(AName, LObject) and (AToName.IsEmpty or FindAppearanceObject(AToName, LToObject)) then
  begin
    if LToObject <> nil then
      LStretchTo := GetObjectAbsoluteX(LToObject)
    else
      LStretchTo := ItemWidth;
    InternalStretchObject(LObject, LStretchTo);
  end;
end;

{ TListViewItemHelper }

function TListViewItemHelper.ItemText(const AName: string): TListItemText;
begin
  Result := Objects.FindObjectT<TListItemText>(AName);
end;

end.
