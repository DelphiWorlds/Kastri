unit VPD.ListView.Appearances;

interface

uses
  System.UITypes,
  FMX.ListView.Appearances, FMX.ListView, FMX.ListView.Types;

type
  TIDListViewItem = class(TListViewItem)
  private
    function GetID: string;
    function GetIDItemText: TListItemText;
    procedure SetID(const Value: string);
  protected
    function GetListItemImage(const AItemName: string; const AOwnsBitmap: Boolean = True): TListItemImage;
  public
    class function FindItem(const AListView: TListView; const AID: string; out AItem: TListViewItem): Boolean;
    class function LastItem(const AListView: TListView): string;
    class function SelectItem(const AListView: TListView; const AID: string): Boolean;
  public
    procedure SetTextColor(const AColor: TAlphaColor);
    property ID: string read GetID write SetID;
  end;

implementation

uses
  DW.OSLog,
  System.SysUtils,
  FMX.Graphics;

{ TIDListViewItem }

class function TIDListViewItem.FindItem(const AListView: TListView; const AID: string; out AItem: TListViewItem): Boolean;
var
  I: Integer;
begin
  for I := 0 to AListView.Items.Count - 1 do
  begin
    if TIDListViewItem(AListView.Items[I]).ID.Equals(AID) then
    begin
      AItem := AListView.Items[I];
      Exit(True);
    end;
  end;
  Result := False;
end;

function TIDListViewItem.GetID: string;
begin
  Result := GetIDItemText.Text;
end;

function TIDListViewItem.GetIDItemText: TListItemText;
begin
  Result := Objects.FindObjectT<TListItemText>('ID');
end;

function TIDListViewItem.GetListItemImage(const AItemName: string; const AOwnsBitmap: Boolean = True): TListItemImage;
begin
  Result := Objects.FindObjectT<TListItemImage>(AItemName);
  if AOwnsBitmap and (Result.Bitmap = nil) then
    Result.Bitmap := TBitmap.Create;
  Result.OwnsBitmap := AOwnsBitmap;
end;

class function TIDListViewItem.LastItem(const AListView: TListView): string;
begin
  Result := '';
  if AListView.Items.Count > 0 then
    Result := TIDListViewItem(AListView.Items[AListView.Items.Count - 1]).ID;
end;

class function TIDListViewItem.SelectItem(const AListView: TListView; const AID: string): Boolean;
var
  LItem: TListViewItem;
begin
  Result := False;
  if FindItem(AListView, AID, LItem) then
  begin
    AListView.Selected := LItem;
    Result := True;
  end;
end;

procedure TIDListViewItem.SetID(const Value: string);
begin
  GetIDItemText.Text := Value;
end;

procedure TIDListViewItem.SetTextColor(const AColor: TAlphaColor);
var
  I: Integer;
begin
  for I := 0 to Objects.Count - 1 do
  begin
    if Objects.Drawables[I] is TListItemText then
      TListItemText(Objects.Drawables[I]).TextColor := AColor;
  end;
end;

end.
