unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, DW.NativeShape, FMX.WebBrowser, FMX.Edit, FMX.StdCtrls,
  FMX.Layouts, DW.NativeImage, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, DW.NativeSlider;

type
  TForm1 = class(TForm)
    WebBrowser1: TWebBrowser;
    TopLayout: TLayout;
    GoButton: TButton;
    URLEdit: TEdit;
    Memo1: TMemo;
    NativeImage1: TNativeImage;
    procedure FormCreate(Sender: TObject);
    procedure GoButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GoButtonClick(GoButton);
end;

procedure TForm1.GoButtonClick(Sender: TObject);
begin
  WebBrowser1.Navigate(URLEdit.Text);
end;

end.
