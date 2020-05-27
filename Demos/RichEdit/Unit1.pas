unit Unit1;

// Basic demo of the "FMX-ified" RichEdit control
// NOTE: The control is currently for WINDOWS ONLY

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.RichEdit;

type
  TForm1 = class(TForm)
    RichEditLayout: TLayout;
    LoadFileButton: TButton;
    Layout1: TLayout;
    OpenDialog: TOpenDialog;
    procedure LoadFileButtonClick(Sender: TObject);
  private
    FRichEdit: TRichEdit;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

procedure TForm1.LoadFileButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    FRichEdit.LoadFromFile(OpenDialog.FileName, nil);
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FRichEdit := TRichEdit.Create(Self);
  // The color is set to black in order to contrast the colors in the demo .rtf supplied with this demo, i.e. Birthday.rtf
  // You may wish to remove this line, or allow the user to select a color, or calculate a contrasting color
  FRichEdit.Color := TAlphaColorRec.Black;
  FRichEdit.ShowScrollBars := False;
  FRichEdit.Align := TAlignLayout.Client;
  FRichEdit.Parent := RichEditLayout;
end;

end.
