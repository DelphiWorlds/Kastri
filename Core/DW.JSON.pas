unit DW.JSON;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Json;

type
  TJsonHelper = record
    class function IsJson(const AJson: string): Boolean; static;
    class function JsonEncode(const AJsonValue: TJSONValue): string; overload; static;
    class function JsonEncode(const AJsonString: string): string; overload; static;
    class function Tidy(const AJsonValue: TJsonValue; const AIndentSize: Integer = 2): string; overload; static;
    class function Tidy(const AJson: string; const AIndentSize: Integer = 2): string; overload; static;
  end;

implementation

uses
  // RTL
  System.Character, System.SysUtils;

class function TJsonHelper.IsJson(const AJson: string): Boolean;
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

class function TJsonHelper.JsonEncode(const AJsonValue: TJSONValue): string;
begin
  Result := AJsonValue.ToJSON;
end;

class function TJsonHelper.JsonEncode(const AJsonString: string): string;
var
  LJsonValue: TJSONValue;
  LStr: string;
begin
  LStr := AnsiQuotedStr(AJsonString, '\');
  LStr := Copy(LStr, 2, Length(LStr) - 2);
  LJsonValue := TJSONObject.ParseJSONValue(LStr, False, True);
  try
    Result := JsonEncode(LJsonValue);
  finally
    LJsonValue.Free;
  end;
end;

class function TJsonHelper.Tidy(const AJsonValue: TJsonValue; const AIndentSize: Integer = 2): string;
begin
  Result := Tidy(AJsonValue.ToString, AIndentSize);
end;

// Now based on: https://pastebin.com/Juks92Y2 (if the link still exists), by Lars Fosdal
class function TJsonHelper.Tidy(const AJson: string; const AIndentSize: Integer = 2): string;
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
  for LChar in AJson do
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

end.
