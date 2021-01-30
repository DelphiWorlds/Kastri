unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.QuickLook.iOS;

type
  TForm1 = class(TForm)
    OpenButton: TButton;
    procedure OpenButtonClick(Sender: TObject);
  private
    FPreview: TQLPreview;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FPreview := TQLPreview.Create;
end;

destructor TForm1.Destroy;
begin
  FPreview.Free;
  inherited;
end;

procedure TForm1.OpenButtonClick(Sender: TObject);
begin
  FPreview.OpenDocument(TPath.Combine(TPath.GetDocumentsPath, 'rad-studio-feature-matrix.pdf'));
end;

end.
