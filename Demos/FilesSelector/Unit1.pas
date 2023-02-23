unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.TabControl, FMX.Objects,
  DW.FilesSelector;

type
  TForm1 = class(TForm)
    SelectImagesButton: TButton;
    ListBox1: TListBox;
    DisplayNameLabel: TLabel;
    BottomLayout: TLayout;
    DisplayNameValueLabel: TLabel;
    ButtonsLayout: TLayout;
    SelectImagesAndMoviesButton: TButton;
    SelectTextButton: TButton;
    TabControl: TTabControl;
    FilesTab: TTabItem;
    ImageTab: TTabItem;
    Image: TImage;
    procedure SelectImagesButtonClick(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure SelectImagesAndMoviesButtonClick(Sender: TObject);
    procedure SelectTextButtonClick(Sender: TObject);
  private
    FSelector: TFilesSelector;
    procedure LoadImage(const AFilePath: string);
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
  {$IF Defined(ANDROID)}
  DW.Android.Helpers,
  {$ENDIF}
  System.Permissions,
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
  TabControl.ActiveTab := FilesTab;
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
var
  LDisplayName: string;
begin
  LDisplayName := FSelector.SelectedFiles[Item.Index].DisplayName;
  DisplayNameValueLabel.Text := LDisplayName;
  if LDisplayName.EndsWith('jpg', True) or LDisplayName.EndsWith('jpeg', True) or LDisplayName.EndsWith('bmp', True) or LDisplayName.EndsWith('png', True) then
    LoadImage(FSelector.SelectedFiles[Item.Index].RawPath);
end;

procedure TForm1.LoadImage(const AFilePath: string);
{$IF Defined(ANDROID)}
var
  LFileStream: TAndroidFileStream;
begin
  LFileStream := TAndroidFileStream.Create(AFilePath);
  try
    Image.Bitmap.LoadFromStream(LFileStream);
    TabControl.ActiveTab := ImageTab;
  finally
    LFileStream.Free;
  end;
end;
{$ELSE}
begin
  // TODO: Implement for other platforms
end;
{$ENDIF}

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
  TabControl.ActiveTab := FilesTab;
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

procedure TForm1.SelectTextButtonClick(Sender: TObject);
begin
  FSelector.FileKinds := [TFileKind.Text];
  FSelector.Select(TSelectionMode.Content);
end;

procedure TForm1.SelectImagesButtonClick(Sender: TObject);
begin
  FSelector.FileKinds := [TFileKind.Image];
  FSelector.Select(TSelectionMode.Content);
end;

procedure TForm1.SelectImagesAndMoviesButtonClick(Sender: TObject);
begin
  FSelector.FileKinds := [TFileKind.Image, TFileKind.Movie];
  FSelector.Select(TSelectionMode.Content);
end;

end.
