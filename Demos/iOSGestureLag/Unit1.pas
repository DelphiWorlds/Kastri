unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.Objects;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Button1: TButton;
    Rectangle1: TRectangle;
    CheckBox1: TCheckBox;
    procedure Button1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure CheckBox1Change(Sender: TObject);
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
  DW.ScreenEdgeManager;

procedure TForm1.Button1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if Rectangle1.Tag = 0 then
  begin
    Rectangle1.Fill.Color := TAlphaColors.Red;
    Rectangle1.Tag := 1;
  end
  else
  begin
    Rectangle1.Fill.Color := TAlphaColors.Green;
    Rectangle1.Tag := 0;
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  if CheckBox1.IsChecked then
    TScreenEdgeManager.SetPreferredScreenEdges([TScreenEdge.Bottom])
  else
    TScreenEdgeManager.SetPreferredScreenEdges([]);
end;

end.
