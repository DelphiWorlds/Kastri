unit DW.TokenReader;

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
  System.SysUtils;

type
  TTokenReader = record
  private
    FAbsoluteIndex: Int64;
    FColIndex: Integer;
    FConsumedWhitespace: string;
    FIsEOL: Boolean;
    FLineIndex: Integer;
    FLines: TArray<string>;
    FQuoteChar: Char;
    FWhitespace: TCharArray;
    function Count: Integer;
    function IsWhitespace(const AChar: Char): Boolean;
  public
    constructor Create(const AText: string); overload;
    constructor Create(const AText: TArray<string>); overload;
    function EOF: Boolean;
    function EOL: Boolean;
    function Next: string;
    procedure Seek(const AToken: string);
    procedure SeekEOL;
    property AbsoluteIndex: Int64 read FAbsoluteIndex;
    property ColIndex: Integer read FColIndex;
    property ConsumedWhitespace: string read FConsumedWhitespace;
    property LineIndex: Integer read FLineIndex;
    property Lines: TArray<string> read FLines;
    property QuoteChar: Char read FQuoteChar write FQuoteChar;
    property Whitespace: TCharArray read FWhitespace write FWhitespace;
  end;

implementation

uses
  // RTL
  System.Character, System.Math;

const
  cDefaultWhitespace: TCharArray = [#0, #9, #10, #13, #32, #160];

{ TTokenReader }

constructor TTokenReader.Create(const AText: string);
begin
  Create([AText]);
end;

function TTokenReader.EOL: Boolean;
begin
  Result := FIsEOL;
end;

function TTokenReader.Count: Integer;
begin
  Result := Length(FLines);
end;

constructor TTokenReader.Create(const AText: TArray<string>);
begin
  FQuoteChar := '"';
  FLines := AText;
  FWhitespace := cDefaultWhitespace;
  FAbsoluteIndex := 0;
  FLineIndex := 0;
  FColIndex := 0;
end;

function TTokenReader.EOF: Boolean;
begin
  Result := (Count = 0) or ((FLineIndex = Count - 1) and (FColIndex = Length(FLines[FLineIndex])));
end;

function TTokenReader.IsWhitespace(const AChar: Char): Boolean;
begin
  Result := AChar.IsInArray(FWhitespace);
end;

function TTokenReader.Next: string;
var
  LLastIndex, LStartIndex, LIndex: Integer;
  LText: string;
  LIsQuoted: Boolean;
begin
  Result := '';
  FConsumedWhitespace := '';
  if Count > 0 then
  begin
    LText := FLines[FLineIndex];
    LLastIndex := Length(LText);
    if FColIndex < LLastIndex then
    begin
      LIndex := FColIndex;
      while FColIndex < LLastIndex do
      begin
        while IsWhitespace(LText.Chars[FColIndex]) and (FColIndex < LLastIndex) do
        begin
          FConsumedWhitespace := FConsumedWhitespace + LText.Chars[FColIndex];
          Inc(FColIndex);
        end;
        if FColIndex < LLastIndex then
        begin
          LIsQuoted := (FQuoteChar <> #0) and (LText.Chars[FColIndex] = FQuoteChar);
          LStartIndex := FColIndex;
          if LIsQuoted then
            Inc(FColIndex);
          while (FColIndex < LLastIndex) and ((not LIsQuoted and not IsWhitespace(LText.Chars[FColIndex])) or (LIsQuoted and (LText.Chars[FColIndex] <> FQuoteChar))) do
            Inc(FColIndex);
          if LIsQuoted and (FColIndex < LLastIndex) then
            Inc(FColIndex);
          Result := LText.Substring(LStartIndex, FColIndex - LStartIndex);
          Break;
        end;
      end;
      Inc(FAbsoluteIndex, FColIndex - LIndex);
    end
    else if FLineIndex < Count - 1 then
    begin
      Inc(FLineIndex);
      FColIndex := 0;
      Result := Next;
    end;
  end;
  FIsEOL := LText.IsEmpty or (FColIndex = Length(LText)) or LText.Chars[FColIndex + 1].IsInArray([#13, #10]);
end;

procedure TTokenReader.Seek(const AToken: string);
begin
  while not EOF do
  begin
    if Next.Equals(AToken) then
      Break;
  end;
end;

procedure TTokenReader.SeekEOL;
begin
  while not (EOL or EOF) do
    Next;
end;

end.
