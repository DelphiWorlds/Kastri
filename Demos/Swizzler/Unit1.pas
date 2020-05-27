unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Tabs, Vcl.StdCtrls,
  DW.Swizzler.Win;

type
  TForm1 = class(TForm)
    TabSet1: TTabSet;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TTabSetHacked = class(TObject)
  protected
    class var FSetStartMarginSwizzle: TSwizzler;
    class destructor DestroyClass;
    class procedure Swizzle;
  protected
    procedure SetStartMargin(Value: Integer);
  end;

{ TTabSetHacked }

class procedure TTabSetHacked.Swizzle;
begin
  // Since this application is built with runtime packages, it needs to swizzle the method in the appropriate package
  FSetStartMarginSwizzle := TSwizzler.Create('vcl260.bpl', '@Vcl@Tabs@TTabSet@SetStartMargin$qqri', @TTabSetHacked.SetStartMargin);
end;

class destructor TTabSetHacked.DestroyClass;
begin
  FSetStartMarginSwizzle.Free;
end;

procedure TTabSetHacked.SetStartMargin(Value: Integer);
var
  LSetStartMargin: procedure(Value: Integer) of object;
begin
  // Only "override" for a component with a particular name
  if TTabSet(Self).Name = 'TabSet1' then
    Value := 5;
  // Disable the swizzling while the original method is called, otherwise it will cause it to be recursive
  FSetStartMarginSwizzle.Disable;
  try
    TMethod(LSetStartMargin).Data := Self;
    TMethod(LSetStartMargin).Code := FSetStartMarginSwizzle.OldProc;
    // Call the original method with the "correct" value
    LSetStartMargin(Value);
  finally
    FSetStartMarginSwizzle.Enable;
  end;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  // Swizzle called before inherited so that the streamed StartMargin property is "overridden"
  TTabSetHacked.Swizzle;
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Now that the method has been redirected, setting the property from here will have no effect
  TabSet1.StartMargin := 50;
end;

end.
