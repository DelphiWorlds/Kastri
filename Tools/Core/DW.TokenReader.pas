unit DW.TokenReader;

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
  System.SysUtils;

type
  TTokenReader = record
  private
    FColIndex: Integer;
    FLineIndex: Integer;
    FLines: TArray<string>;
    FWhitespace: TCharArray;
    function Count: Integer;
    function IsWhitespace(const AChar: Char): Boolean;
  public
    constructor Create(const AText: string); overload;
    constructor Create(const AText: TArray<string>); overload;
    function EOF: Boolean;
    function Next: string;
    procedure Seek(const AToken: string);
    property ColIndex: Integer read FColIndex;
    property LineIndex: Integer read FLineIndex;
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

function TTokenReader.Count: Integer;
begin
  Result := Length(FLines);
end;

constructor TTokenReader.Create(const AText: TArray<string>);
begin
  FLines := AText;
  FWhitespace := cDefaultWhitespace;
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
  LLastIndex, LStartIndex: Integer;
  LText: string;
begin
  Result := '';
  if Count > 0 then
  begin
    LText := FLines[FLineIndex];
    LLastIndex := Length(LText);
    if FColIndex < LLastIndex then
    begin
      while FColIndex < LLastIndex do
      begin
        while IsWhitespace(LText.Chars[FColIndex]) and (FColIndex < LLastIndex) do
          Inc(FColIndex);
        if FColIndex < LLastIndex then
        begin
          LStartIndex := FColIndex;
          while not IsWhitespace(LText.Chars[FColIndex]) and (FColIndex < LLastIndex) do
            Inc(FColIndex);
          Result := LText.Substring(LStartIndex, FColIndex - LStartIndex);
          Break;
        end;
      end;
    end
    else if FLineIndex < Count - 1 then
    begin
      Inc(FLineIndex);
      FColIndex := 0;
      Result := Next;
    end;
  end;
end;

procedure TTokenReader.Seek(const AToken: string);
begin
  while not EOF do
  begin
    if Next.Equals(AToken) then
      Break;
  end;
end;

end.
