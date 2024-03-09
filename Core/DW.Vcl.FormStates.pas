unit DW.Vcl.FormStates;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Types,
  // VCL
  Vcl.Forms;

type
  TFormState = record
    FormName: string;
    WindowState: TWindowState;
    BoundsRect: TRect;
  end;

  TFormStates = TArray<TFormState>;

  TFormStatesHelper = record helper for TFormStates
  public
    function Count: Integer;
    function IndexOf(const AForm: TForm): Integer;
    function Load(const AForm: TForm): Boolean;
    procedure Save(const AForm: TForm);
  end;

implementation

uses
  // RTL
  System.SysUtils;

{ TFormStatesHelper }

function TFormStatesHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TFormStatesHelper.IndexOf(const AForm: TForm): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Self[I].FormName.Equals(string(AForm.Name)) then
      Exit(I);
  end;
end;

function TFormStatesHelper.Load(const AForm: TForm): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOf(AForm);
  if LIndex > -1 then
  begin
    AForm.BoundsRect := Self[LIndex].BoundsRect;
    AForm.WindowState := Self[LIndex].WindowState;
    Result := True;
  end;
end;

procedure TFormStatesHelper.Save(const AForm: TForm);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AForm);
  if LIndex = -1 then
  begin
    SetLength(Self, Count + 1);
    LIndex := Count - 1;
  end;
  Self[LIndex].FormName := AForm.Name;
  Self[LIndex].WindowState := AForm.WindowState;
  Self[LIndex].BoundsRect := AForm.BoundsRect;
end;

end.
