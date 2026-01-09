unit DW.Vcl.ListBoxHelper;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // VCL
  Vcl.StdCtrls;

type
  TListBoxHelper = class helper for TCustomListBox
  public
    function CheckedCount(const AMustBeEnabled: Boolean = False): Integer;
    function CheckedItems: TArray<string>;
    function DeletedSelected: Boolean;
    function HasChecked: Boolean;
    function HasSelections: Boolean;
    function SelectedCount: Integer;
    function SelectedItem: string;
    function SelectItem(const AItem: string): Integer;
    function SelectLastItem: Boolean;
    procedure ToggleChecked(const AIndex: Integer = -1);
    procedure UncheckAll;
  end;

implementation

uses
  // VCL
  Vcl.CheckLst;

{ TListBoxHelper }

function TListBoxHelper.CheckedCount(const AMustBeEnabled: Boolean = False): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Self is TCheckListBox then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      if TCheckListBox(Self).Checked[I] and (not AMustBeEnabled or TCheckListBox(Self).ItemEnabled[I]) then
        Inc(Result);
    end;
  end;
end;

function TListBoxHelper.DeletedSelected: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Count - 1 downto 0 do
  begin
    if Selected[I] then
      Items.Delete(I);
  end;
end;

function TListBoxHelper.HasChecked: Boolean;
begin
  Result := CheckedCount > 0;
end;

function TListBoxHelper.HasSelections: Boolean;
begin
  Result := SelectedCount > 0;
end;

function TListBoxHelper.SelectedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if Selected[I] then
      Inc(Result);
  end;
end;

function TListBoxHelper.SelectedItem: string;
begin
  Result := '';
  if ItemIndex > -1 then
    Result := Items[ItemIndex];
end;

function TListBoxHelper.CheckedItems: TArray<string>;
var
  I: Integer;
begin
  Result := [];
  if Self is TCheckListBox then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      if TCheckListBox(Self).Checked[I] then
        Result := Result + [Items[I]];
    end;
  end;
end;

function TListBoxHelper.SelectItem(const AItem: string): Integer;
begin
  Result := Items.IndexOf(AItem);
  if Result > -1 then
    ItemIndex := Result;
end;

function TListBoxHelper.SelectLastItem: Boolean;
begin
  if ItemIndex = -1 then
    ItemIndex := Count - 1;
  Result := ItemIndex > -1;
end;

procedure TListBoxHelper.ToggleChecked(const AIndex: Integer = -1);
var
  I, LCheckedCount: Integer;
begin
  if Self is TCheckListBox then
  begin
    if AIndex = -1 then
    begin
      LCheckedCount := CheckedCount;
      for I := 0 to Count - 1 do
        TCheckListBox(Self).Checked[I] := LCheckedCount < (Count div 2);
    end
    else if (AIndex > -1) and (AIndex < Items.Count) then
      TCheckListBox(Self).Checked[AIndex] := not TCheckListBox(Self).Checked[AIndex];
  end;
end;

procedure TListBoxHelper.UncheckAll;
var
  I: Integer;
begin
  if Self is TCheckListBox then
  begin
    for I := 0 to Count - 1 do
      TCheckListBox(Self).Checked[I] := False;
  end;
end;

end.
