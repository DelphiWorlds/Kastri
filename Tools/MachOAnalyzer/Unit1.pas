unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.TreeView, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TMainView = class(TForm)
    MachOTreeView: TTreeView;
    BottomLayout: TLayout;
    CloseButton: TButton;
    SelectButton: TButton;
    MachoOpenDialog: TOpenDialog;
    MachOLabel: TLabel;
    procedure SelectButtonClick(Sender: TObject);
  private
    procedure AddDylibItems(const ARootItem: TTreeViewItem; const ADylibs: TArray<string>);
    function AddRootItem(const AText: string): TTreeViewItem;
    procedure ScanFile(const AFileName: string);
  public
    { Public declarations }
  end;

var
  MainView: TMainView;

implementation

{$R *.fmx}

uses
  System.Generics.Collections, System.Generics.Defaults,
  DW.MachOReader;

function ShortenPath(const APath: string; AMaxLength: Integer): string;
const
  cEllipsis = '...';
var
  LLeftPart, LRightPart: string;
  LMidIndex, LEllipsisLen: Integer;
begin
  if Length(APath) > AMaxLength then
  begin
    LEllipsisLen := Length(cEllipsis);
    LMidIndex := (AMaxLength - LEllipsisLen) div 2;
    LLeftPart := Copy(APath, 1, LMidIndex);
    LRightPart := Copy(APath, Length(APath) - LMidIndex + 1, LMidIndex);
    Result := LLeftPart + cEllipsis + LRightPart;
  end
  else
    Result := APath;
end;

{ TMainView }

procedure TMainView.AddDylibItems(const ARootItem: TTreeViewItem; const ADylibs: TArray<string>);
var
  LDylib: string;
  LItem: TTreeViewItem;
  LSorted: TArray<string>;
begin
  LSorted := Copy(ADylibs);
  TArray.Sort<string>(LSorted,
    TComparer<string>.Construct(
      function(const ALeft, ARight: string): Integer
      begin
        if ALeft.StartsWith('@') and not ARight.StartsWith('@') then
          Result := -1
        else if not ALeft.StartsWith('@') and ARight.StartsWith('@') then
          Result := 1
        else
          Result := CompareStr(ALeft, ARight);
      end
    )
  );
  for LDylib in LSorted do
  begin
    LItem := TTreeViewItem.Create(Self);
    LItem.Text := LDylib;
    ARootItem.AddObject(LItem);
  end;
end;

function TMainView.AddRootItem(const AText: string): TTreeViewItem;
var
  I: Integer;
  LItem: TTreeViewItem;
begin
  Result := nil;
  for I := 0 to MachOTreeView.Count - 1 do
  begin
    LItem := MachOTreeView.Items[I];
    if (LItem.Parent = MachOTreeView) and LItem.Text.Equals(AText)  then
    begin
      Result := LItem;
      Break;
    end;
  end;
  if Result = nil then
  begin
    Result := TTreeViewItem.Create(Self);
    Result.Text := AText;
    Result.IsExpanded := True;
    MachOTreeView.AddObject(Result);
  end;
end;

procedure TMainView.ScanFile(const AFileName: string);
var
  LReader: TMachOReader;
  LItem: TMachoItem;
  LRootItem: TTreeViewItem;
begin
  MachOTreeView.Clear;
  LReader := TMachOReader.Create(AFileName);
  try
    if Length(LReader.Items) > 0 then
    begin
      MachOTreeView.BeginUpdate;
      try
        for LItem in LReader.Items do
        begin
          LRootItem := AddRootItem(LItem.CPUType);
          AddDylibItems(LRootItem, LItem.Dylibs);
        end;
      finally
        MachOTreeView.EndUpdate;
      end;
    end
    else
      AddRootItem('No LC_LOAD_DYLIB entries found!');
  finally
    LReader.Free;
  end;
end;

procedure TMainView.SelectButtonClick(Sender: TObject);
begin
  if MachoOpenDialog.Execute then
  begin
    MachOLabel.Text := ShortenPath(MachoOpenDialog.FileName, 100);
    ScanFile(MachoOpenDialog.FileName);
  end;
end;

end.
