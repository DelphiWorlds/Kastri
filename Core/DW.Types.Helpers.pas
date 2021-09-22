unit DW.Types.Helpers;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Types, System.Classes, System.SysUtils, System.JSON;

type
  TStringDynArrayHelper = record helper for TStringDynArray
  private
    function GetValue(const AName: string): string;
    procedure SetValue(const AName, AValue: string);
  public
    function Add(const AValue: string; const AAllowDupes: Boolean = True): Integer;
    procedure AssignToStrings(const AStrings: TStrings; const AOverwrite: Boolean = True);
    procedure Clear;
    function Clone: TStringDynArray;
    function Count: Integer;
    procedure Delete(const AIndex: Integer);
    procedure FromJsonValue(const AValue: TJSONValue);
    function IndexOf(const AValue: string): Integer;
    function IndexOfName(const AName: string): Integer;
    procedure LoadFromStrings(const AStrings: TStrings; const AOverwrite: Boolean = True);
    procedure LoadText(const AText: string);
    procedure Remove(const AValue: string);
    procedure Reverse;
    procedure Sort(const ACaseSensitive: Boolean = False; const AReverse: Boolean = False);
    function ToBytes(const AEncoding: TEncoding = nil): TBytes; overload;
    function ToBytes(const AIncludeLineBreaks: Boolean; const AEncoding: TEncoding = nil): TBytes; overload;
    function ToJSONValue: TJSONValue;
    function ToText(const ASeparator: string): string;
    property Values[const AName: string]: string read GetValue write SetValue;
  end;

implementation

uses
  // RTL
  System.Generics.Collections, System.Generics.Defaults;

type
  TStringComparerEx = class(TComparer<String>)
  private
    FCaseSensitive: Boolean;
    FReverse: Boolean;
  public
    constructor Create(const ACaseSensitive: Boolean; const AReverse: Boolean);
    function Compare(const Left, Right: String): Integer; override;
  end;

{ TStringComparerEx }

function TStringComparerEx.Compare(const Left, Right: String): Integer;
begin
  if FCaseSensitive then
    Result := CompareStr(Left, Right)
  else
    Result := CompareText(Left, Right);
  if FReverse then
    Result := not Result;
end;

constructor TStringComparerEx.Create(const ACaseSensitive, AReverse: Boolean);
begin
  inherited Create;
  FCaseSensitive := ACaseSensitive;
  FReverse := AReverse;
end;

{ TStringDynArrayHelper }

function TStringDynArrayHelper.Add(const AValue: string; const AAllowDupes: Boolean): Integer;
begin
  Result := IndexOf(AValue);
  if AAllowDupes or (Result = -1) then
  begin
    SetLength(Self, Count + 1);
    Self[Count - 1] := AValue;
    Result := Count - 1;
  end
  else
    Result := -1;
end;

procedure TStringDynArrayHelper.AssignToStrings(const AStrings: TStrings; const AOverwrite: Boolean = True);
var
  LValue: string;
begin
  AStrings.BeginUpdate;
  try
    if AOverwrite then
      AStrings.Clear;
    for LValue in Self do
      AStrings.Add(LValue);
  finally
    AStrings.EndUpdate;
  end;
end;

procedure TStringDynArrayHelper.Clear;
begin
  SetLength(Self, 0);
end;

function TStringDynArrayHelper.Clone: TStringDynArray;
var
  LItem: string;
begin
  for LItem in Self do
    Result := Result + [LItem];
end;

function TStringDynArrayHelper.Count: Integer;
begin
  Result := Length(Self);
end;

procedure TStringDynArrayHelper.Delete(const AIndex: Integer);
begin
  if (AIndex > -1) and (AIndex < Count) then
    System.Delete(Self, AIndex, 1);
end;

function TStringDynArrayHelper.IndexOf(const AValue: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Self[I].Equals(AValue) then
      Exit(I);
  end;
end;

function TStringDynArrayHelper.IndexOfName(const AName: string): Integer;
var
  I: Integer;
  LParts: TArray<string>;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    LParts := Self[I].Split(['=']);
    if (LParts.Count = 2) and SameText(AName, LParts[0]) then
      Exit(I);
  end;
end;

procedure TStringDynArrayHelper.LoadFromStrings(const AStrings: TStrings; const AOverwrite: Boolean = True);
var
  LValue: string;
begin
  if AOverwrite then
    Clear;
  for LValue in AStrings.ToStringArray do
    Add(LValue);
end;

procedure TStringDynArrayHelper.LoadText(const AText: string);
var
  LStrings: TStrings;
  I: Integer;
begin
  LStrings := TStringList.Create;
  try
    LStrings.Text := AText;
    for I := 0 to LStrings.Count - 1 do
      Add(LStrings[I]);
  finally
    LStrings.Free;
  end;
end;

procedure TStringDynArrayHelper.Remove(const AValue: string);
begin
  Delete(IndexOf(AValue));
end;

procedure TStringDynArrayHelper.SetValue(const AName, AValue: string);
var
  I: Integer;
  LParts: TArray<string>;
begin
  for I := 0 to Count - 1 do
  begin
    LParts := Self[I].Split(['=']);
    if (LParts.Count = 2) and SameText(AName, LParts[0]) then
      Self[I] := LParts[0] + '=' + AValue;
  end;
end;

procedure TStringDynArrayHelper.Sort(const ACaseSensitive: Boolean = False; const AReverse: Boolean = False);
begin
  // TArray.Sort<string>(Self, TStringComparerEx.Create(ACaseSensitive, AReverse));
  if ACaseSensitive then
    TArray.Sort<string>(Self, TIStringComparer.Ordinal)
  else
    TArray.Sort<string>(Self, TStringComparer.Ordinal);
  if AReverse then
    Reverse;
end;

procedure TStringDynArrayHelper.Reverse;
var
  I: Integer;
  LTemp: string;
begin
  if Count > 1 then
  begin
    for I := 0 to Count - 1 do
    begin
      LTemp := Self[I];
      Self[I] := Self[(Count - 1) - I];
      Self[(Count - 1) - I] := LTemp;
      if I > ((Count - 1) div 2) - 1 then
        Break;
    end;
  end;
end;

function TStringDynArrayHelper.GetValue(const AName: string): string;
var
  I: Integer;
  LParts: TArray<string>;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    LParts := Self[I].Split(['=']);
    if (LParts.Count = 2) and SameText(AName, LParts[0]) then
      Result := LParts[1];
  end;
end;

function TStringDynArrayHelper.ToBytes(const AEncoding: TEncoding = nil): TBytes;
begin
  Result := ToBytes(True, AEncoding);
end;

function TStringDynArrayHelper.ToBytes(const AIncludeLineBreaks: Boolean; const AEncoding: TEncoding): TBytes;
var
  I: Integer;
  LValue: string;
  LEncoding: TEncoding;
begin
  LEncoding := AEncoding;
  if LEncoding = nil then
    LEncoding := TEncoding.UTF8;
  for I := 0 to Count - 1 do
  begin
    if AIncludeLineBreaks and (I < Count - 1) then
      LValue := Self[I] + #13#10
    else
      LValue := Self[I];
    Result := Concat(Result, LEncoding.GetBytes(LValue));
  end;
end;

procedure TStringDynArrayHelper.FromJsonValue(const AValue: TJSONValue);
var
  LJSON: TJSONArray;
  I: Integer;
begin
  Self := [];
  if AValue is TJSONArray then
  begin
    LJSON := TJSONArray(AValue);
    for I := 0 to LJSON.Count - 1 do
      Self := Self + [LJSON.Items[I].ToString.DeQuotedString('"')];
  end;
end;

function TStringDynArrayHelper.ToJSONValue: TJSONValue;
var
  LJSONArray: TJSONArray;
  I: Integer;
begin
  LJSONArray := TJSONArray.Create;
  for I := 0 to Length(Self) - 1 do
    LJSONArray.Add(Self[I]);
  Result := LJSONArray;
end;

function TStringDynArrayHelper.ToText(const ASeparator: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I > 0 then
      Result := Result + ASeparator;
    Result := Result + Self[I];
  end;
end;

end.
