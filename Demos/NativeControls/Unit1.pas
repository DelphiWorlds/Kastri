unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  DW.NativeSlider, DW.NativeButton, DW.NativeImage, DW.NativeShape;

type
  TForm1 = class(TForm)
    ContentLayout: TLayout;
    Slider2Label: TLabel;
    NativeButton1: TNativeButton;
    NativeImage1: TNativeImage;
    NativeSlider2: TNativeSlider;
    NativeImage2: TNativeImage;
    NativeSlider1: TNativeSlider;
    Slider1Label: TLabel;
    NativeEllipse1: TNativeEllipse;
    NativeRectangle1: TNativeRectangle;
    procedure NativeButton1Click(Sender: TObject);
    procedure NativeImage1Click(Sender: TObject);
    procedure NativeSlider1ValueChange(Sender: TObject);
    procedure NativeSlider2ValueChange(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

procedure TForm1.NativeButton1Click(Sender: TObject);
begin
  NativeButton1.Text := 'Clicked!';
  // This is just to demo that text settings are changed just like regular FMX controls
  NativeImage1.StyledSettings := NativeImage1.StyledSettings - [TStyledSetting.FontColor];
  NativeImage1.TextSettings.FontColor := TAlphaColors.Red;
end;

procedure TForm1.NativeImage1Click(Sender: TObject);
begin
  ShowMessage('Image clicked!');
end;

procedure TForm1.NativeSlider1ValueChange(Sender: TObject);
begin
  Slider1Label.Text := Format('%.5f', [NativeSlider1.Value]);
end;

procedure TForm1.NativeSlider2ValueChange(Sender: TObject);
begin
  Slider2Label.Text := Format('%.5f', [NativeSlider2.Value]);
end;

end.
