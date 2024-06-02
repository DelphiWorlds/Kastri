unit VPD.HostedView;

interface

uses
  FMX.Types, FMX.Forms;

type
  TForm = class(FMX.Forms.TForm)
  private
    FRootLayout: TFmxObject;
  protected
    procedure Embed(const AHost: TFmxObject);
  end;

implementation

uses
//  System.SysUtils, System.TypInfo,
  FMX.Controls;
//  PS.Core;

{ TForm }

procedure TForm.Embed(const AHost: TFmxObject);
var
  LControl: TControl;
begin
  FRootLayout := nil;
  LControl := TControl(FindComponent('RootLayout'));
  if LControl <> nil then
  begin
    FRootLayout := LControl;
    LControl.HitTest := True;
    LControl.Align := TAlignLayout.Contents;
    LControl.Parent := AHost;
  end;
end;

end.
