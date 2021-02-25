unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  DW.FilesSelector;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    DisplayNameLabel: TLabel;
    BottomLayout: TLayout;
    DisplayNameValueLabel: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
  private
    FSelector: TFilesSelector;
    procedure SelectorCompleteHandler(Sender: TObject; const AOK: Boolean);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  FMX.TextLayout,
  DW.UIHelper;

function MeasureTextWidth(const AFont: TFont; const AText: string): Single;
var
  LLayout: TTextLayout;
begin
  LLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    LLayout.BeginUpdate;
    try
      LLayout.WordWrap := False;
      LLayout.Font.Assign(AFont);
      LLayout.Text := AText;
    finally
      LLayout.EndUpdate;
    end;
    Result := LLayout.TextWidth;
  finally
    LLayout.Free;
  end;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FSelector := TFilesSelector.Create;
  FSelector.Title := 'Select a file';
  FSelector.OnComplete := SelectorCompleteHandler;
end;

destructor TForm1.Destroy;
begin
  FSelector.Free;
  inherited;
end;

procedure TForm1.ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  DisplayNameValueLabel.Text := FSelector.SelectedFiles[Item.Index].DisplayName;
end;

procedure TForm1.Resize;
begin
  inherited;
  BottomLayout.Margins.Bottom := TUIHelper.GetOffsetRect.Bottom;
end;

procedure TForm1.SelectorCompleteHandler(Sender: TObject; const AOK: Boolean);
var
  LFileName: string;
  LMaxChars, LDiff, LHalf, I: Integer;
  LFudge: Single;
begin
  ListBox1.Clear;
  ListBox1.Items.Add('W');
  {$IF Defined(IOS)}
  LFudge := 2;
  {$ELSE}
  LFudge := 1;
  {$ENDIF}
  LMaxChars := Round((ListBox1.Width * LFudge) / MeasureTextWidth(ListBox1.ListItems[0].TextSettings.Font, 'W'));
  ListBox1.Clear;
  for I := 0 to Length(FSelector.SelectedFiles) - 1 do
  begin
    LFileName := FSelector.SelectedFiles[I].DecodedPath; // **** NOTE: Use FSelector.SelectedFiles[I].RawPath for use in the file system!! ****
    if Length(LFileName) > LMaxChars then
    begin
      LDiff := (Length(LFileName) - LMaxChars) + 3;
      LHalf := Length(LFileName) div 2;
      LFileName := LFileName.Substring(0, LHalf - (LDiff div 2)) + '...' + LFileName.Substring(LHalf + (LDiff div 2));
    end;
    ListBox1.Items.Add(LFileName);
  end;
  if ListBox1.Items.Count > 0 then
  begin
    ListBox1.ItemIndex := 0;
    ListBox1ItemClick(ListBox1, ListBox1.ListItems[0]);
  end;
  // NOTE: On Android, for some of the filenames returned you will need to access the files
  // via a ContentResolver, i.e. you will not be able to simply load the files using normal mechanisms in Delphi
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FSelector.Select;
end;

end.
