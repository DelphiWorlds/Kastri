unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    PrintPDFButton: TButton;
    procedure PrintPDFButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  DW.Printing;

procedure TForm1.PrintPDFButtonClick(Sender: TObject);
begin
  Printing.Print(TPath.Combine(TPath.GetDocumentsPath, 'Test.pdf'));
end;

end.
