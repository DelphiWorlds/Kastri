unit DW.HTMLParser;

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


// This unit is based on the code found here:
//   https://github.com/sandbil/HTML-Parser
// Modified/extended according to the original license:

{
MIT License

Copyright (c) 2023 SandBil

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}

interface

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Classes;

type
  TCharStreamReader = class(TStreamReader)
  private
    FPosition: Int64;
    function OffsetSize: Integer;
    procedure Offset(const AOffset: Integer);
    procedure SetPosition(const Value: Int64);
  public
    procedure Reverse(const AOffset: Integer = -1);
    procedure ReadChar(out LChar: Char);
    procedure Rewind; override;
    property Position: Int64 read FPosition write SetPosition;
  end;

  TNodeList = class;

  TNodeType = (Root, Tag, CloseTag, Text, EOF, Error, MissedCloseTag, Comment, Meta, CData, XML);

  TNode = class(TCollectionItem)
  private
    FAttributes: TStrings;
    FBlockLength: Integer;
    FChildren: TNodeList;
    FData: Pointer;
    FNodeType: TNodeType;
    FPosition: Integer;
    FTag: string;
    FTagLength: Integer;
    procedure DoParse(const AReader: TCharStreamReader);
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TNode;
    function GetParent: TNode;
    function GetPath: string;
    function GetText: string;
    procedure SetItem(const Index: Integer; const Value: TNode);
  protected
    procedure SetNodeType(const AType: TNodeType);
    property BlockLength: Integer read FBlockLength;
    property Data: Pointer read FData;
    property Position: Integer read FPosition;
    property TagLength: Integer read FTagLength;
  public
    constructor Create(Collection: TCollection = nil); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetNextSibling: TNode;
    function MoveTo(const ParentNode: TNode; const Position: Integer): TNode;
    procedure Parse(const AHTML: string); overload;
    procedure Parse(const AStream: TStream); overload;
    property Attributes: TStrings read FAttributes;
    property Children: TNodeList read FChildren;
    property Items[const AIndex: Integer]: TNode read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property NodeType: TNodeType read FNodeType;
    property Parent: TNode read GetParent;
    property Path: string read GetPath;
    property Tag: string read FTag;
    property Text: string read GetText;
  end;

  TNodeList = class(TCollection)
  protected
    function GetItem(const Index: Integer): TNode;
    procedure SetItem(const Index: Integer; const Value: TNode);
  public
    Owner: TNode;
    constructor Create(AOwner: TNode);
    function Find(const Tag:string; Count: Integer = 1): TNode;
    property Items[const Index: Integer]: TNode read GetItem write SetItem; default;
  end;

  TNodes = TArray<TNode>;

  TNodeFinder = record
  private
    function InternalFind(const ANode: TNode; const ATag: string; var ANodes: TNodes; const AClass: string): Boolean;
  public
    function Find(const AParent: TNode; const ATag: string; var ANodes: TNodes; const AClass: string = ''): Boolean;
    function FindAttribute(const AParent: TNode; const ATag, AName, AValue: string; var ANode: TNode): Boolean;
    function FindFirst(const AParent: TNode; const ATag: string; var ANode: TNode; const AClass: string = ''): Boolean;
    function FindID(const AParent: TNode; const AID: string; var ANode: TNode): Boolean;
    function FindText(const AParent: TNode; const ATag, AText: string; var ANode: TNode): Boolean;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.StrUtils;

const
  cWhitespace = [' ', #13, #10, #9];
  cWordChars = ['a'..'z', 'A'..'Z', '0'..'9', '_', '-', ':'];
  cEmptyTags : array[1..11] of string = ('BR', 'AREA', 'LINK', 'IMG', 'PARAM', 'HR', 'INPUT', 'COL', 'BASE', 'META', 'FRAME');
  cRepeatableTags : array[1..4] of string = ('P', 'TR', 'TD', 'LI');

{ TCharStreamReader }

function TCharStreamReader.OffsetSize: Integer;
begin
  Result := Ord(CurrentEncoding is TUnicodeEncoding) + 1;
end;

procedure TCharStreamReader.ReadChar(out LChar: Char);
begin
  if Peek >= 0 then
  begin
    LChar := Char(Read);
    Inc(FPosition, OffsetSize);
  end
  else
    LChar := #0;
end;

procedure TCharStreamReader.Reverse(const AOffset: Integer = -1);
begin
  Offset(AOffset);
end;

procedure TCharStreamReader.Offset(const AOffset: Integer);
begin
  SetPosition(Position + (AOffset * OffsetSize));
end;

procedure TCharStreamReader.Rewind;
begin
  inherited;
  SetPosition(0);
end;

procedure TCharStreamReader.SetPosition(const Value: Int64);
begin
  FPosition := Value;
  BaseStream.Position := Value;
  DiscardBufferedData;
end;

{ TNodeFinder }

function TNodeFinder.InternalFind(const ANode: TNode; const ATag: string; var ANodes: TNodes; const AClass: string): Boolean;
var
  I: Integer;
  LClasses: TArray<string>;
begin
  Result := False;
  for I := 0 to ANode.Children.Count - 1 do
  begin
    LClasses := ANode.Children[I].Attributes.Values['class'].Split([' ']);
    if (ANode.Children[I].Tag.ToLower = ATag.ToLower) and (AClass.IsEmpty or (IndexStr(AClass, LClasses) > -1)) then
    begin
      Result := True;
      ANodes := ANodes + [ANode.Children[I]];
    end;
    if InternalFind(ANode.Children[I], ATag, ANodes, AClass) then
      Result := True;
  end;
end;

function TNodeFinder.Find(const AParent: TNode; const ATag: string; var ANodes: TNodes; const AClass: string = ''): Boolean;
begin
  ANodes := [];
  Result := InternalFind(AParent, ATag, ANodes, AClass);
end;

function TNodeFinder.FindID(const AParent: TNode; const AID: string; var ANode: TNode): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not AParent.Attributes.Values['id'].Equals(AID) then
  begin
    for I := 0 to AParent.Children.Count - 1 do
    begin
      if FindID(AParent.Children[I], AID, ANode) then
      begin
        Result := True;
        Break;
      end;
    end;
  end
  else
  begin
    ANode := AParent;
    Result := True;
  end;
end;

function TNodeFinder.FindText(const AParent: TNode; const ATag, AText: string; var ANode: TNode): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AParent.Tag.Equals(ATag) and AParent.Text.Equals(AText) then
  begin
    ANode := AParent;
    Result := True;
  end
  else
  begin
    for I := 0 to AParent.Children.Count - 1 do
    begin
      if FindText(AParent.Children[I], ATag, AText, ANode) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TNodeFinder.FindAttribute(const AParent: TNode; const ATag, AName, AValue: string; var ANode: TNode): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ATag.IsEmpty or AParent.Tag.Equals(ATag)) and AParent.Attributes.Values[AName].Equals(AValue) then
  begin
    ANode := AParent;
    Result := True;
  end
  else
  begin
    for I := 0 to AParent.Children.Count - 1 do
    begin
      if FindAttribute(AParent.Children[I], ATag, AName, AValue, ANode) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

function TNodeFinder.FindFirst(const AParent: TNode; const ATag: string; var ANode: TNode; const AClass: string = ''): Boolean;
var
  LNodes: TNodes;
begin
  Result := False;
  LNodes := [];
  if InternalFind(AParent, ATag, LNodes, AClass) then
  begin
    ANode := LNodes[0];
    Result := True;
  end;
end;

{ TNodeList }

constructor TNodeList.Create(AOwner: TNode);
begin
  inherited Create(TNode);
  Owner := AOwner;
end;

function TNodeList.Find(const Tag: string; Count: Integer = 1): TNode;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if UpperCase(TNode(Items[I]).Tag) = UpperCase(Tag) then
    begin
      Dec(Count);
      if Count = 0 then
      begin
        Result := TNode(Items[I]);
        Break;
      end;
    end;
  end;
end;

function TNodeList.GetItem(const Index: Integer): TNode;
begin
  Result := inherited GetItem(Index) as TNode;
end;

procedure TNodeList.SetItem(const Index: Integer; const Value: TNode);
begin
  inherited SetItem(Index, Value);
end;

{ TNode }

constructor TNode.Create(Collection: TCollection = nil);
begin
  inherited Create(Collection);
  FAttributes := TStringList.Create;
  FChildren := TNodeList.Create(Self);
end;

destructor TNode.Destroy;
begin
  FAttributes.Free;
  FChildren.Free;
  inherited Destroy;
end;

procedure TNode.Parse(const AHTML: string);
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create(AHTML, TEncoding.Unicode);
  try
    LStream.Position := 0;
    Parse(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TNode.Parse(const AStream: TStream);
var
  LNode: TNode;
  LReader: TCharStreamReader;
  LEncoding: TEncoding;
begin
  AStream.Position := 0;
  if AStream is TStringStream then
    LEncoding := TStringStream(AStream).Encoding
  else
    LEncoding := TEncoding.Default;
  LReader := TCharStreamReader.Create(AStream, LEncoding);
  try
    FNodeType := TNodeType.Root;
    repeat
      LNode := TNode(FChildren.Add);
      LNode.DoParse(LReader);
    until LReader.EndOfStream or (LNode.NodeType in [TNodeType.EOF, TNodeType.Error]);
  finally
    LReader.Free;
  end;
end;

function TNode.GetItem(const AIndex: Integer): TNode;
begin
  Result := FChildren.GetItem(Index);
end;

function TNode.GetNextSibling: TNode;
begin
  Result := nil;
  if (Parent <> nil) and (Parent.Children.Count > Index) then
    Result := Parent.Children[Index + 1]; 
end;

procedure TNode.SetItem(const Index: Integer; const Value: TNode);
begin
  FChildren.SetItem(Index, Value);
end;

procedure TNode.SetNodeType(const AType: TNodeType);
begin
  FNodeType := AType;
end;

function TNode.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TNode.GetPath: string;
var
  I, LIndex: Integer;
begin
  Result := '';
  LIndex := 0;
  if Collection <> nil then
  begin
    for I := 0 to Collection.Count - 1 do
    begin
      if TNode(Collection.Items[I]).Tag = Tag then
        Inc(LIndex);
      if Collection.Items[I] = Self then
        Break;
    end;
    Result := Tag;
    if LIndex <> 1 then
      Result := IntToStr(LIndex) + ' ' + Result;
    Result := TNodeList(Collection).Owner.Path + '\' + Result;
  end;
end;

function TNode.GetText: string;
var
  I: Integer;
begin
  Result := '';
  if NodeType <> TNodeType.Text then
  begin
    for I := 0 to Children.Count - 1 do
      Result := Result + Children[I].Text;
  end
  else
    Result := FTag;
end;

function TNode.GetParent: TNode;
begin
  if Collection = nil then
    Result := nil
  else
    Result := TNodeList(Collection).Owner;
end;

procedure TNode.Assign(Source: TPersistent);
begin
  if Source is TNode then
  begin
    FTag := TNode(Source).Tag;
    FAttributes.Assign(TNode(Source).Attributes);
    FChildren.Assign(TNode(Source).Children);
    FNodeType := TNode(Source).NodeType;
    FPosition := TNode(Source).Position;
    FTagLength := TNode(Source).TagLength;
    FBlockLength := TNode(Source).BlockLength;
    FData := TNode(Source).Data;
  end
  else
    inherited Assign(Source);
end;

function TNode.MoveTo(const ParentNode: TNode; const Position: Integer): TNode;
var
  LNode: TNode;
begin
  LNode := TNode(ParentNode.Children.Insert(Position));
  LNode.Assign(Self);
  Result := LNode;
  Free;
end;

procedure TNode.DoParse(const AReader: TCharStreamReader);
var
  LChar, LQuoteChar: Char;
  LText, LAttrName, LAttrValue: string;
  LNode, LChildNode: TNode;
  LFound, LClosedTag: Boolean;
  I: Integer;
begin
  try
    FPosition := AReader.Position;
    FTagLength := 0;
    FBlockLength := 0;
    repeat
      AReader.ReadChar(LChar);
    until not CharInSet(LChar, cWhitespace + [#0]);
    if LChar <> '<' then
    begin
      LText := '';
      repeat
        LText := LText + LChar;
        AReader.ReadChar(LChar);
        if LChar = #0 then
          Break;
        // It's valid HTML to have a "<" inside of Javascript, so..
        if (Parent <> nil) and Parent.Tag.Equals('SCRIPT') and (LChar = '<') then
        begin
          // ..make sure it's a closing tag
          AReader.ReadChar(LChar);
          if LChar = '/' then
          begin
            AReader.Reverse;
            LChar := '<';
          end;
        end;
      until LChar = '<';
      if LChar = '<' then
        AReader.Reverse;

      FNodeType := TNodeType.Text;
      FTag := LText.Trim([' ', #13, #10, #9]);
      FTagLength := AReader.Position - Position;
      FBlockLength := AReader.Position - Position;
    end
    else
    begin
      FNodeType := TNodeType.Tag;
      LText := '';
      AReader.ReadChar(LChar);
      if LChar = '!' then
      begin
        AReader.ReadChar(LChar);
        if LChar = '-' then
        begin
          AReader.ReadChar(LChar);
          if LChar = '-' then      // <!--....-->
          begin
            LText := '';
            repeat
              AReader.ReadChar(LChar);
              LText := LText + LChar;
            until Copy(LText, Length(LText) - 2, 3) = '-->';
            FTag := Copy(LText, 1, Length(LText) - 3);
            FNodeType := TNodeType.Comment;
            FTagLength := AReader.Position - Position;
            FBlockLength := AReader.Position - Position;
            Exit;
          end
          else
            raise Exception.Create(LChar + ' was not expected here (expecting -).');
        end
        else if LChar = '[' then
        begin              // <![CDATA[.....]]>
          LText := '';
          repeat
            AReader.ReadChar(LChar);
            LText := LText + LChar;
          until LChar = '[';
          if LText <> 'CDATA[' then
            raise Exception.Create(LText + ' was not expected here (expecting CDATA[).');
          LText := '';
          repeat
            AReader.ReadChar(LChar);
            LText := LText + LChar;
          until (Length(LText) >= 3) and (Copy(LText, Length(LText) - 2, 3) = ']]>');
          FTag := Copy(LText, 1, Length(LText) - 3);
          FNodeType := TNodeType.CData;
          FTagLength := AReader.Position - Position;
          FBlockLength := AReader.Position - Position;
          Exit;
        end
        else
        begin              // <!.....>
          LText := '';
          repeat
            LText := LText + LChar;
            AReader.ReadChar(LChar);
          until CharInSet(LChar, cWhitespace + ['>']); //  LChar = '>';
          FTag := LText;
          if LChar <> '>' then
          begin
            repeat
              AReader.ReadChar(LChar);
            until LChar = '>';
          end;
          FNodeType := TNodeType.Meta;                     // what's the proper name for this entity...?
          FTagLength := AReader.Position - Position;
          FBlockLength := AReader.Position - Position;
          Exit;
        end;
      end
      else if LChar = '?' then
      begin                // <?.....?>
        LText := '';
        repeat
          AReader.ReadChar(LChar);
          LText := LText + LChar;
        until Copy(LText, Length(LText) - 1, 2) = '?>';
        FTag := Copy(LText, 1, Length(LText) - 2);
        FNodeType := TNodeType.XML;    // or is it just some PHP?
        FTagLength := AReader.Position - Position;
        FBlockLength := AReader.Position - Position;
        Exit;
      end;

      repeat
        LText := LText + UpCase(LChar);
        AReader.ReadChar(LChar);
      until not CharInSet(LChar, cWordChars);

      if LText[1] = '/' then
      begin
        while CharInSet(LChar, cWhitespace) do
          AReader.ReadChar(LChar);
        if LChar <> '>' then
          raise Exception.Create(LChar + ' was not expected here (expecting >).');
        FTag := Copy(LText, 2, Length(LText) - 1);
        FNodeType := TNodeType.CloseTag;
        FTagLength := AReader.Position - Position;
        FBlockLength := AReader.Position - Position;
        if not AReader.EndOfStream then
        begin
          repeat
            AReader.ReadChar(LChar);
          until not CharInSet(LChar, cWhitespace) or AReader.EndOfStream;
          if not CharInSet(LChar, cWhitespace) then
            AReader.Reverse;
        end;
        Exit;
      end;

      FTag := LText;
      LClosedTag := False;

      for I := 1 to High(cEmptyTags) do
      begin
        if Tag = cEmptyTags[I] then
          LClosedTag := True;
      end;

      repeat
        while CharInSet(LChar, cWhitespace) do
          AReader.ReadChar(LChar);
        case LChar of
          '/':
            begin
              AReader.ReadChar(LChar);
              if LChar <> '>' then
                // raise Exception.Create('/' + LChar + ' was not expected here (expecting > after /).');
                raise Exception.Create('Expecting > after /');
              LClosedTag := True;
              Break;
            end;
          '>':
            Break;
        else
          begin
            if not CharInSet(LChar, cWordChars) then
              // raise Exception.Create(LChar + ' was not expected here.');
              raise Exception.Create('Unexpected character');
            LAttrName := '';
            repeat
              LAttrName := LAttrName + LChar;
              AReader.ReadChar(LChar);
            until not CharInSet(LChar, cWordChars);

            while CharInSet(LChar, cWhitespace) do
              AReader.ReadChar(LChar);

            if LChar = '=' then
            begin
              AReader.ReadChar(LChar);
              while (LChar <> '>') and CharInSet(LChar, cWhitespace) do
                AReader.ReadChar(LChar);
              LAttrValue := '';
              if LChar <> '>' then
              begin
                LAttrValue := '';
                if CharInSet(LChar, ['''', '"']) then
                begin
                  LQuoteChar := LChar;
                  AReader.ReadChar(LChar);
                  while LChar <> LQuoteChar do
                  begin
                    LAttrValue := LAttrValue + LChar;
                    AReader.ReadChar(LChar);
                  end;
                  AReader.ReadChar(LChar);
                end
                else
                begin
                  repeat
                    LAttrValue := LAttrValue + LChar;
                    AReader.ReadChar(LChar);
                  until CharInSet(LChar, cWhitespace + ['>']);
                end;
              end;
              Attributes.Add(LAttrName.ToLower + '=' + LAttrValue);
            end
            else if CharInSet(LChar, cWordChars + ['/', '>']) then
            begin
              Attributes.Add(LAttrName.ToLower);
              Continue;
            end
            else
              raise Exception.Create(LChar + ' was not expected here (property ' + LText + ').');
          end;
        end;
      until False;

      FTagLength := AReader.Position - Position;

      if LClosedTag then
      begin
        FBlockLength := AReader.Position - Position;
        Exit;
      end;

      repeat
        LChildNode := TNode(Children.Add);
        LChildNode.DoParse(AReader);
        if LChildNode.NodeType = TNodeType.CloseTag then
        begin
          if LChildNode.Tag = Tag then  // closing our own tag
          begin
            Children.Delete(LChildNode.Index);
            FBlockLength := AReader.Position - Position;
            Exit
          end;

          LFound := False;                     // in EmptyTags?
          for I := 1 to High(cEmptyTags) do
            if LChildNode.Tag = cEmptyTags[I] then
              LFound := True;
          if LFound then                       // if so, just ignore the close (closing these tags is optional)
          begin
            Children.Delete(LChildNode.Index);
            Continue;
          end;

          LFound := False;
          LNode := Parent;
          while LNode <> nil do
          begin
            if LNode.Tag = LChildNode.Tag then
            begin
              LFound := True;
              Break
            end;
            LNode := LNode.Parent;
          end;
          if LFound then
          begin
            AReader.Position := LChildNode.Position;
            LChildNode.SetNodeType(TNodeType.MissedCloseTag);
            FBlockLength := AReader.Position - Position;
            Exit;
          end;
          // unmatched closing tag found, ignoring
        end;

        if (LChildNode.NodeType = TNodeType.Tag) and (LChildNode.Tag = Tag) then
        begin
          LFound := False;
          for I := 1 to High(cRepeatableTags) do
            if LChildNode.Tag = cRepeatableTags[I] then
              LFound := True;

          if LFound then
          begin
            FBlockLength := LChildNode.Position - Position;
            LNode := Self;
            while LNode.Parent <> nil do
            begin
              if LNode.Parent.Tag <> LChildNode.Tag then
              begin
                LChildNode.MoveTo(LNode.Parent, LNode.Index + 1);
                Break
              end;
              LNode := LNode.Parent;
            end;
            Exit;
          end;
        end;
      until AReader.EndOfStream;
    end;
  except
    on E: EAbort do
      FNodeType := TNodeType.EOF;
    on E: EReadError do
      FNodeType := TNodeType.EOF;
    on E: Exception do
    begin
      FNodeType := TNodeType.Error;
      FTag := E.Message;
      FTagLength := AReader.Position - Position;
      FBlockLength := AReader.Position - Position;
    end;
  end;
end;

end.
