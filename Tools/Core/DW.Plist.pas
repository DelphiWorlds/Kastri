unit DW.Plist;

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
  // Xml
  Xml.XMLIntf, Xml.XMLDoc;

type
  TValueType = (Unknown, StringValue, IntegerValue, FloatValue, BooleanValue, DateValue, DataValue, ArrayValue, DictionaryValue);

  IPlistKey = interface(IInterface)
    ['{C5B31483-0F2A-4BAB-927F-A0E97BAE8E3B}']
    function GetValueType: TValueType;
    function GetValue(out AValue: string): Boolean; // overload;
  end;

  IPlistDict = interface(IInterface)
    ['{FE5AE414-FDC4-4AFD-AC42-915F46913A36}']
    function FindKey(const AName: string; out AKey: IPlistKey): Boolean;
    function GetKey(const AIndex: Integer): IPlistKey;
    function GetKeyCount: Integer;
  end;

  IPlist = interface(IInterface)
    ['{F00914BE-10B1-4B4F-9A6E-E94AC084D45B}']
    function GetDict: IPlistDict;
    procedure LoadFromFile(const AFileName: string);
  end;

  TPlist = class(TInterfacedObject, IPlist)
  private
    FDocument: IXMLDocument;
    FIPlistDict: IPlistDict;
    FNode: IXMLNode;
  public
    { IPlist }
    function GetDict: IPlistDict;
    procedure LoadFromFile(const AFileName: string);
  public
    constructor Create(const AFileName: string);
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  Xml.Win.msxmldom;

type
  TCustomPlistNode = class(TInterfacedObject)
  private
    FNode: IXMLNode;
  protected
    property Node: IXMLNode read FNode;
  public
    constructor Create(const ANode: IXMLNode); virtual;
  end;

  TPlistDict = class(TCustomPlistNode, IPlistDict)
  public
    { IPlistDict }
    function FindKey(const AName: string; out AKey: IPlistKey): Boolean;
    function GetKey(const AIndex: Integer): IPlistKey;
    function GetKeyCount: Integer;
  end;

  TPlistKey = class(TCustomPlistNode, IPlistKey)
  private
    FValueNode: IXMLNode;
  public
    { IPlistKey }
    function GetValueType: TValueType;
    function GetValue(out AValue: string): Boolean;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

{ TCustomPlistNode }

constructor TCustomPlistNode.Create(const ANode: IXMLNode);
begin
  inherited Create;
  FNode := ANode;
end;

{ TPlist }

constructor TPlist.Create(const AFileName: string);
begin
  inherited Create;
  {$IF Defined(MSWINDOWS)}
  MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);
  {$ENDIF}
  LoadFromFile(AFileName);
end;

procedure TPlist.LoadFromFile(const AFileName: string);
begin
  FNode := nil;
  FDocument := nil;
  if TFile.Exists(AFileName) then
  begin
    FDocument := LoadXMLDocument(AFileName);
    FNode := FDocument.DocumentElement;
  end;
end;

function TPlist.GetDict: IPlistDict;
var
  LNode: IXMLNode;
begin
  if FIPlistDict = nil then
  begin
    LNode := FNode.ChildNodes.FindNode('dict');
    if LNode <> nil then
      FIPlistDict := TPlistDict.Create(LNode);
  end;
  Result := FIPlistDict;
end;

{ TPlistDict }

function TPlistDict.FindKey(const AName: string; out AKey: IPlistKey): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    if FNode.ChildNodes[I].NodeName.Equals('key') and SameText(FNode.ChildNodes[I].Text, AName) then
    begin
      AKey := TPlistKey.Create(FNode.ChildNodes[I]);
      Result := True;
      Break;
    end;
  end;
end;

function TPlistDict.GetKey(const AIndex: Integer): IPlistKey;
var
  I, LIndex: Integer;
begin
  Result := nil;
  LIndex := -1;
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    if FNode.ChildNodes[I].NodeName.Equals('key') then
    begin
      Inc(LIndex);
      if AIndex = LIndex then
      begin
        Result := TPlistKey.Create(FNode.ChildNodes[I]);
        Break;
      end;
    end;
  end;
end;

function TPlistDict.GetKeyCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    if FNode.ChildNodes[I].NodeName.Equals('key') then
      Inc(Result);
  end;
end;

{ TPlistKey }

constructor TPlistKey.Create(const ANode: IXMLNode);
begin
  inherited;
  FValueNode := ANode.NextSibling;
end;

function TPlistKey.GetValue(out AValue: string): Boolean;
begin
  Result := False;
  if GetValueType = TValueType.StringValue then
  begin
    AValue := FValueNode.Text;
    Result := True;
  end;
end;

function TPlistKey.GetValueType: TValueType;
begin
  Result := TValueType.Unknown;
  if FValueNode <> nil then
  begin
    if FValueNode.NodeName.Equals('false') or FValueNode.NodeName.Equals('true') then
      Result := TValueType.BooleanValue
    else if FValueNode.NodeName.Equals('string') then
      Result := TValueType.StringValue
    else if FValueNode.NodeName.Equals('integer') then
      Result := TValueType.IntegerValue
    else if FValueNode.NodeName.Equals('real') then
      Result := TValueType.FloatValue
    else if FValueNode.NodeName.Equals('date') then
      Result := TValueType.StringValue
    else if FValueNode.NodeName.Equals('data') then
      Result := TValueType.DataValue
    else if FValueNode.NodeName.Equals('array') then
      Result := TValueType.ArrayValue
    else if FValueNode.NodeName.Equals('dict') then
      Result := TValueType.DictionaryValue;
  end;
end;

end.
