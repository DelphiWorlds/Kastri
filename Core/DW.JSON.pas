unit DW.JSON;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.JSON;

type
  TJSONHelper = record
    class function IsJSON(const AJSON: string): Boolean; static;
    class function JSONEncode(const AValue: TJSONValue): string; overload; static;
    class function JSONEncode(const AValue: string): string; overload; static;
    class function Tidy(const AValue: TJsonValue; const AIndentSize: Integer = 2): string; overload; static;
    class function Tidy(const AValue: string; const AIndentSize: Integer = 2): string; overload; static;
    class function ToJSON(const AValue: string; const AKey: string = ''): string; overload; static;
    class function ToJSON(const AValues: TArray<string>): string; overload; static;
    class function ToJSON(const AValues: TArray<string>; const AKey: string): string; overload; static;
    class function ToJSONValue(const AValues: TArray<string>): TJSONValue; static;
    class function ToStringArray(const AValues: TJSONArray): TArray<string>; static;
  end;

  TJSONValueHelper = class helper for TJSONValue
    function TryGetISO8601Date(const APath: string; out ADate: TDateTime; const AReturnUTC: Boolean = True): Boolean;
  end;

implementation

uses
  // RTL
  System.Character, System.SysUtils, System.DateUtils;

class function TJSONHelper.IsJson(const AJson: string): Boolean;
var
  LJsonValue: TJSONValue;
begin
  Result := False;
  LJsonValue := TJSONObject.ParseJSONValue(AJson);
  if LJsonValue <> nil then
  try
    Result := True;
  finally
    LJsonValue.Free;
  end;
end;

class function TJSONHelper.JSONEncode(const AValue: TJSONValue): string;
begin
  Result := AValue.ToJSON;
end;

class function TJSONHelper.JSONEncode(const AValue: string): string;
var
  LValue: TJSONValue;
  LStr: string;
begin
  LStr := AnsiQuotedStr(AValue, '\');
  LStr := Copy(LStr, 2, Length(LStr) - 2);
  LValue := TJSONObject.ParseJSONValue(LStr, False, True);
  try
    Result := JSONEncode(LValue);
  finally
    LValue.Free;
  end;
end;

class function TJSONHelper.Tidy(const AValue: TJsonValue; const AIndentSize: Integer = 2): string;
begin
  Result := Tidy(AValue.ToString, AIndentSize);
end;

// Now based on: https://pastebin.com/Juks92Y2 (if the link still exists), by Lars Fosdal
class function TJSONHelper.Tidy(const AValue: string; const AIndentSize: Integer = 2): string;
const
  cEOL = #13#10;
var
  LChar: Char;
  LIsInString: boolean;
  LIsEscape: boolean;
  LIsHandled: boolean;
  LIndent: Integer;
begin
  Result := '';
  LIndent := 0;
  LIsInString := False;
  LIsEscape := False;
  for LChar in AValue do
  begin
    if not LIsInString then
    begin
      LIsHandled := False;
      if (LChar = '{') or (LChar = '[') then
      begin
        Inc(LIndent);
        Result := Result + LChar + cEOL + StringOfChar(' ', LIndent * AIndentSize);
        LIsHandled := True;
      end
      else if LChar = ',' then
      begin
        Result := Result + LChar + cEOL + StringOfChar(' ', LIndent * AIndentSize);
        LIsHandled := True;
      end
      else if (LChar = '}') or (LChar = ']') then
      begin
        Dec(LIndent);
        Result := Result + cEOL + StringOfChar(' ', LIndent * AIndentSize) + LChar;
        LIsHandled := True;
      end;
      if not LIsHandled and not LChar.IsWhiteSpace then
        Result := Result + LChar;
    end
    else
      Result := Result + LChar;
    if not LIsEscape and (LChar = '"') then
      LIsInString := not LIsInString;
    LIsEscape := (LChar = '\') and not LIsEscape;
  end;
end;

class function TJSONHelper.ToJSON(const AValues: TArray<string>): string;
var
  LJSON: TJSONValue;
begin
  LJSON := ToJSONValue(AValues);
  try
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

class function TJSONHelper.ToJSON(const AValues: TArray<string>; const AKey: string): string;
var
  LJSON: TJSONObject;
begin
  LJSON := TJSONObject.Create;
  try
    LJSON.AddPair(AKey, ToJSONValue(AValues));
    Result := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;
end;

class function TJSONHelper.ToJSONValue(const AValues: TArray<string>): TJSONValue;
var
  LValues: TJSONArray;
  LValue: string;
begin
  LValues := TJSONArray.Create;
  for LValue in AValues do
    LValues.AddElement(TJSONString.Create(LValue));
  Result := LValues;
end;

class function TJSONHelper.ToJSON(const AValue: string; const AKey: string = ''): string;
var
  LValue: TJSONObject;
begin
  LValue := TJSONObject.Create;
  try
    if AKey.IsEmpty then
      LValue.AddPair('value', TJSONString.Create(AValue))
    else
      LValue.AddPair(AKey, TJSONString.Create(AValue));
    Result := LValue.ToJSON;
  finally
    LValue.Free;
  end;
end;

class function TJSONHelper.ToStringArray(const AValues: TJSONArray): TArray<string>;
var
  LValue: TJSONValue;
begin
  for LValue in AValues do
    Result := Result + [LValue.Value];
end;

{ TJSONValueHelper }

function TJSONValueHelper.TryGetISO8601Date(const APath: string; out ADate: TDateTime; const AReturnUTC: Boolean = True): Boolean;
var
  LValue: string;
begin
  Result := TryGetValue(APath, LValue);
  if Result then
    ADate := ISO8601ToDate(LValue, AReturnUTC)
  else
    ADate := 0;
end;

end.
