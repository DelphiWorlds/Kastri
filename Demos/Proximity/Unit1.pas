unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.Proximity;

type
  TForm1 = class(TForm)
    Label1: TLabel;
  private
    FProximity: TProximity;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FProximity := TProximity.Create;
  FProximity.BlankScreenWhenNear := True;
end;

destructor TForm1.Destroy;
begin
  FProximity.Free;
  inherited;
end;

end.
