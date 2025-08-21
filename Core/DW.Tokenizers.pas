unit DW.Tokenizers;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  System.SysUtils;

type
  TTokenProc = reference to procedure(const AToken: string);
  TTokenFunc = reference to function(const AToken: string): Boolean;
  TTextReplaceFunc = reference to function(const AText: string): string;

  TTokenizer = class(TObject)
  private
    const cDefaultWhitespace: TCharArray = [#0, #9, #10, #13, #32, #160];
  private
    FText: string;
    FWhitespace: TCharArray;
    function IsWhitespace(const AChar: Char): Boolean;
  public
    constructor Create; overload;
    constructor Create(const AText: string); overload;
    procedure Tokenize(const ATokenProc: TTokenProc); overload;
    function Tokenize(const ATokenFunc: TTokenFunc): Boolean; overload;
    function Replace(const AReplaceFunction: TTextReplaceFunc): string;
    property Text: string read FText write FText;
    property Whitespace: TCharArray read FWhitespace write FWhitespace;
  end;

implementation

uses
  System.Character, System.Math;

{ TTokenizer }

constructor TTokenizer.Create;
begin
  inherited;
  FWhitespace := cDefaultWhitespace;
end;

constructor TTokenizer.Create(const AText: string);
begin
  inherited Create;
  FWhitespace := cDefaultWhitespace;
  FText := AText;
end;

function TTokenizer.IsWhitespace(const AChar: Char): Boolean;
begin
  Result := AChar.IsInArray(FWhitespace);
end;

procedure TTokenizer.Tokenize(const ATokenProc: TTokenProc);
var
  LIndex, LLastIndex, LStartIndex: Integer;
begin
  if not Text.Trim.IsEmpty then
  begin
    LIndex := 0;
    LLastIndex := Length(Text);
    while LIndex < LLastIndex do
    begin
      while IsWhitespace(Text.Chars[LIndex]) and (LIndex < LLastIndex) do
        Inc(LIndex);
      if LIndex < LLastIndex then
      begin
        LStartIndex := LIndex;
        while not IsWhitespace(Text.Chars[LIndex]) and (LIndex < Length(Text)) do
          Inc(LIndex);
        ATokenProc(Text.Substring(LStartIndex, LIndex - LStartIndex));
      end;
    end;
  end;
end;

function TTokenizer.Tokenize(const ATokenFunc: TTokenFunc): Boolean;
var
  LIndex, LLastIndex, LStartIndex: Integer;
begin
  Result := False;
  if not Text.Trim.IsEmpty then
  begin
    Result := True;
    LIndex := 0;
    LLastIndex := Length(Text);
    while LIndex < LLastIndex do
    begin
      while IsWhitespace(Text.Chars[LIndex]) and (LIndex < LLastIndex) do
        Inc(LIndex);
      if LIndex < LLastIndex then
      begin
        LStartIndex := LIndex;
        while not IsWhitespace(Text.Chars[LIndex]) and (LIndex < Length(Text)) do
          Inc(LIndex);
        if not ATokenFunc(Text.Substring(LStartIndex, LIndex - LStartIndex)) then
          Result := False;
      end;
      if not Result then
        Break;
    end;
  end;
end;

function TTokenizer.Replace(const AReplaceFunction: TTextReplaceFunc): string;
var
  LIndex, LStartIndex: Integer;
  LToken, LReplaceWith: string;
begin
  Result := Text;
  if not Text.Trim.IsEmpty then
  begin
    LIndex := 0;
    while LIndex < Length(Result) do
    begin
      while IsWhitespace(Result.Chars[LIndex]) and (LIndex < Length(Result)) do
        Inc(LIndex);
      if LIndex < Length(Result) then
      begin
        LStartIndex := LIndex;
        while not IsWhitespace(Result.Chars[LIndex]) and (LIndex < Length(Result)) do
          Inc(LIndex);
        LToken := Result.Substring(LStartIndex, LIndex - LStartIndex);
        LReplaceWith := AReplaceFunction(LToken);
        Result := Result.Remove(LStartIndex, Length(LToken));
        Result := Result.Insert(LStartIndex, LReplaceWith);
        Inc(LIndex, Length(LReplaceWith) - Length(LToken));
      end;
    end;
  end;
end;

end.
