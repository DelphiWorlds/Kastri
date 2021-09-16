unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  DW.SoundPlayer;

type
  TForm1 = class(TForm)
    SoundsListBox: TListBox;
    Layout1: TLayout;
    PlaySoundButton: TButton;
    Layout2: TLayout;
    procedure PlaySoundButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FPlayer: TSoundPlayer;
    procedure AddSounds;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  DW.IOUtils.Helpers, DW.UIHelper;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FPlayer := TSoundPlayer.Create;
  AddSounds;
end;

destructor TForm1.Destroy;
begin
  FPlayer.Free;
  inherited;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Layout2.Margins.Bottom := TUIHelper.GetOffsetRect.Height;
end;

procedure TForm1.AddSounds;
var
  LFileName, LFilesPath: string;
  I: Integer;
begin
  for LFileName in TDirectory.GetFiles(TPathHelper.GetResourcesPath('Sounds'), '*.mp3', TSearchOption.soTopDirectoryOnly) do
    FPlayer.AddSound(LFileName);
  for I := 0 to Length(FPlayer.SoundItems) - 1 do
    SoundsListBox.Items.Add(FPlayer.SoundItems[I].Name);
  if SoundsListBox.Items.Count > 0 then
    SoundsListBox.ItemIndex := 0;
end;

procedure TForm1.PlaySoundButtonClick(Sender: TObject);
begin
  if SoundsListBox.ItemIndex > -1 then
    FPlayer.PlaySound(SoundsListBox.ItemIndex);
end;

end.
