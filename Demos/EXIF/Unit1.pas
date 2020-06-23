unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Layouts, System.Actions, FMX.ActnList;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Memo1: TMemo;
    Layout1: TLayout;
    PrevButton: TButton;
    NextButton: TButton;
    procedure PrevButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FFiles: TArray<string>;
    FIndex: Integer;
    procedure SelectedImage;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  DW.EXIF, DW.UIHelper;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FIndex := -1;
  FFiles := TDirectory.GetFiles(TPath.Combine(TPath.GetDocumentsPath, 'Images'), '*.jpg', TSearchOption.soTopDirectoryOnly);
  if Length(FFiles) > 0 then
  begin
    FIndex := 0;
    PrevButton.Enabled := True;
    NextButton.Enabled := True;
    SelectedImage;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Layout1.Margins.Bottom := TUIHelper.GetOffsetRect.Bottom;
end;

procedure TForm1.NextButtonClick(Sender: TObject);
begin
  if FIndex = Length(FFiles) - 1 then
    FIndex := 0
  else
    FIndex := FIndex + 1;
  SelectedImage;
end;

procedure TForm1.PrevButtonClick(Sender: TObject);
begin
  if FIndex = 0 then
    FIndex := Length(FFiles) - 1
  else
    FIndex := FIndex - 1;
  SelectedImage;
end;

procedure TForm1.SelectedImage;
var
  LProperties: TEXIFProperties;
  LFileName: string;
begin
  LFileName := FFiles[FIndex];
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Image: ' + TPath.GetFileName(LFileName));
  Image1.Bitmap.LoadFromFile(LFileName);
  if TEXIF.GetEXIF(LFileName, LProperties) then
  begin
    Memo1.Lines.Add('Camera Make: ' + LProperties.CameraMake);
    Memo1.Lines.Add('Camera Model: ' + LProperties.CameraModel);
    Memo1.Lines.Add('Date Taken: ' + LProperties.DateTaken);
    Memo1.Lines.Add(Format('GPS : %.5f, %.5f', [LProperties.Latitude, LProperties.Longitude]));
  end
  else
    Memo1.Lines.Add('Unable to obtain EXIF data');
end;

end.
