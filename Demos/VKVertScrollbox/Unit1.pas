unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Objects,
  DW.VKVertScrollbox;

type
  TfrmMain = class(TForm)
    VertScrollBox: TVertScrollBox;
    Memo1: TMemo;
    VertScrollboxLayout: TLayout;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

{ TForm1 }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  VertScrollBox.ControlsLayout := VertScrollboxLayout;
end;

end.
