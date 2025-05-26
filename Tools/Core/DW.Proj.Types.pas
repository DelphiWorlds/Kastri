unit DW.Proj.Types;

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
  Xml.XMLIntf;

const
  cItemGroup = 'ItemGroup';
  cPropertyGroup = 'PropertyGroup';

type
  TNameValuePair = record
    Name: string;
    Value: string;
  end;

  TNameValuePairs = TArray<TNameValuePair>;

  TNameValuePairsHelper = record helper for TNameValuePairs
    procedure Add(const AName, AValue: string);
    function AsString(const ALineDelimiter: string; const AValueDelimiter: string = '='): string;
    function Count: Integer;
    function FindValue(const AName: string; out AValue: string): Boolean;
    procedure Parse(const AText, ALineDelimiter: string; const AValueDelimiter: string = '=');
    function Remove(const AName: string): Boolean;
  end;

  TProjConfig = record
    Name: string;
    Ident: string;
    constructor Create(const AName: string; const AIdent: string);
  end;

  TProjConfigs = TArray<TProjConfig>;

  TProjCompilerOption = record
    Name: string;
    Switch: string;
    Value: string;
  end;

  TProjCompilerOptions = TArray<TProjCompilerOption>;

  IProjNode = interface(IInterface)
    ['{6892E6D3-DE9C-4C8A-8A66-0D462BA08FC5}']
    function ChildCount: Integer;
    function GetChild(const AIndex: Integer): IXMLNode;
  end;

  TCustomProjNode = class(TInterfacedObject, IProjNode)
  private
    FNode: IXMLNode;
  protected
    function GetAttributeText(const AAttributeName: string): string;
    function GetChildBoolean(const AChildNodeName: string): Boolean;
    function GetChildInteger(const AChildNodeName: string; const ADefault: Integer = 0): Integer;
    function GetChildText(const AChildNodeName: string): string;
    property Node: IXMLNode read FNode;
  public
    { IProjNode }
    function ChildCount: Integer;
    function GetChild(const AIndex: Integer): IXMLNode;
  public
    constructor Create(const ANode: IXMLNode); virtual;
  end;

  IProjPropertyGroup =  interface(IProjNode)
    ['{06A05376-B340-4FE1-B402-E60101756B7B}']
    function DeleteValue(const AName: string): Boolean;
    function FindValue(const AName: string; out AValue: string): Boolean;
    function GetCondition: string;
    function GetValues: TNameValuePairs;
    procedure SetValue(const AName, AValue: string);
    property Values: TNameValuePairs read GetValues;
  end;

  ICustomProj = interface(IInterface)
    ['{82316815-4F91-41A1-9750-D9104FBF56AD}']
    function GetPropertyGroup(const AIndex: Integer): IProjPropertyGroup;
    function GetPropertyGroupCount: Integer;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string = ''); overload;
    procedure SaveToFile(const AFormatted: Boolean; const AFileName: string = ''); overload;
  end;

  TCustomProj = class(TInterfacedObject, ICustomProj)
  private
    FDocument: IXMLDocument;
    FFileName: string;
    FNode: IXMLNode;
  protected
    procedure DocumentLoad; virtual;
    property Node: IXMLNode read FNode;
  public
    { IProj }
    function GetPropertyGroup(const AIndex: Integer): IProjPropertyGroup;
    function GetPropertyGroupCount: Integer;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string = ''); overload;
    procedure SaveToFile(const AFormatted: Boolean; const AFileName: string = ''); overload;
  public
    constructor Create(const AFileName: string);
  end;

  TEmptyPropertyGroup = class(TCustomProjNode, IProjPropertyGroup)
  private
    FValues: TNameValuePairs;
  public
    { IProjPropertyGroup }
    function DeleteValue(const AName: string): Boolean; virtual;
    function FindValue(const AName: string; out AValue: string): Boolean; virtual;
    function GetCondition: string; virtual;
    function GetValues: TNameValuePairs; virtual;
    procedure SetValue(const AName, AValue: string); virtual;
  public
    constructor Create; reintroduce; overload;
  end;

  TProjPropertyGroup = class(TEmptyPropertyGroup)
  private
    FCondition: string;
    procedure ReadValues;
  public
    function DeleteValue(const AName: string): Boolean; override;
    function FindValue(const AName: string; out AValue: string): Boolean; override;
    function GetCondition: string; override;
    procedure SetValue(const AName, AValue: string); override;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

implementation

uses
  System.SysUtils, System.Variants, System.IOUtils,
  Xml.XMLDoc;

{ TNameValuePairsHelper }

procedure TNameValuePairsHelper.Add(const AName, AValue: string);
var
  LPair: TNameValuePair;
begin
  LPair.Name := AName;
  LPair.Value := AValue;
  Self := Self + [LPair];
end;

function TNameValuePairsHelper.AsString(const ALineDelimiter: string; const AValueDelimiter: string = '='): string;
var
  LPair: TNameValuePair;
begin
  Result := '';
  for LPair in Self do
  begin
    if not Result.IsEmpty then
      Result := Result + ALineDelimiter;
    Result := Result + LPair.Name + AValueDelimiter + LPair.Value;
  end;
end;

function TNameValuePairsHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TNameValuePairsHelper.FindValue(const AName: string; out AValue: string): Boolean;
var
  LPair: TNameValuePair;
begin
  Result := False;
  for LPair in Self do
  begin
    if LPair.Name.Equals(AName) then
    begin
      AValue := LPair.Value;
      Result := True;
      Break;
    end;
  end;
end;

procedure TNameValuePairsHelper.Parse(const AText, ALineDelimiter: string; const AValueDelimiter: string = '=');
var
  LLines, LPair: TArray<string>;
  I: Integer;
begin
  Self := [];
  LLines := AText.Split([ALineDelimiter]);
  SetLength(Self, Length(LLines));
  for I := 0 to Length(LLines) - 1 do
  begin
    LPair := LLines[I].Split([AValueDelimiter]);
    if Length(LPair) = 2 then
      Add(LPair[0], LPair[1]);
  end;
end;

function TNameValuePairsHelper.Remove(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    if Self[I].Name.Equals(AName) then
    begin
      Delete(Self, I, 1);
      Break;
    end;
  end;
end;

{ TProjPropertyGroup }

constructor TProjPropertyGroup.Create(const ANode: IXMLNode);
begin
  inherited;
  FCondition := GetAttributeText('Condition');
  ReadValues;
end;

procedure TProjPropertyGroup.ReadValues;
var
  LChildNode: IXMLNode;
  I: Integer;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    LChildNode := Node.ChildNodes[I];
    FValues.Add(LChildNode.NodeName, LChildNode.Text);
  end;
end;

procedure TProjPropertyGroup.SetValue(const AName, AValue: string);
var
  LChildNode: IXMLNode;
  I: Integer;
begin
  LChildNode := nil;
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    if Node.ChildNodes[I].NodeName.Equals(AName) then
    begin
      LChildNode := Node.ChildNodes[I];
      Break;
    end;
  end;
  if LChildNode <> nil then
    LChildNode.Text := AValue;
  // TODO: else add the node, etc
end;

function TProjPropertyGroup.DeleteValue(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    if Node.ChildNodes[I].NodeName.Equals(AName) then
    begin
      Node.ChildNodes.Delete(I);
      Result := True;
      Break;
    end;
  end;
end;

function TProjPropertyGroup.FindValue(const AName: string; out AValue: string): Boolean;
begin
  Result := FValues.FindValue(AName, AValue);
end;

function TProjPropertyGroup.GetCondition: string;
begin
  Result := FCondition;
end;

{ TProjConfig }

constructor TProjConfig.Create(const AName, AIdent: string);
begin
  Name := AName;
  Ident := AIdent;
end;

{ TCustomProjNode }

constructor TCustomProjNode.Create(const ANode: IXMLNode);
begin
  inherited Create;
  FNode := ANode;
end;

function TCustomProjNode.GetAttributeText(const AAttributeName: string): string;
begin
  Result := VarToStrDef(FNode.Attributes[AAttributeName], '');
end;

function TCustomProjNode.GetChild(const AIndex: Integer): IXMLNode;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < ChildCount) then
    Result := FNode.ChildNodes[AIndex];
end;

function TCustomProjNode.GetChildBoolean(const AChildNodeName: string): Boolean;
var
  LNode: IXMLNode;
begin
  Result := False;
  LNode := FNode.ChildNodes.FindNode(AChildNodeName);
  if LNode <> nil then
    Result := SameText(LNode.Text, 'true');
end;

function TCustomProjNode.GetChildInteger(const AChildNodeName: string; const ADefault: Integer = 0): Integer;
var
  LNode: IXMLNode;
begin
  Result := ADefault;
  LNode := FNode.ChildNodes.FindNode(AChildNodeName);
  if LNode <> nil then
    Result := StrToIntDef(LNode.Text, ADefault);
end;

function TCustomProjNode.GetChildText(const AChildNodeName: string): string;
var
  LNode: IXMLNode;
begin
  Result := '';
  LNode := FNode.ChildNodes.FindNode(AChildNodeName);
  if LNode <> nil then
    Result := LNode.Text;
end;

function TCustomProjNode.ChildCount: Integer;
begin
  Result := FNode.ChildNodes.Count;
end;

{ TCustomProj }

constructor TCustomProj.Create(const AFileName: string);
begin
  inherited Create;
  LoadFromFile(AFileName);
end;

procedure TCustomProj.LoadFromFile(const AFileName: string);
begin
  FNode := nil;
  FDocument := nil;
  if TFile.Exists(AFileName) then
  begin
    FDocument := TXMLDocument.Create(nil);
    FDocument.ParseOptions := FDocument.ParseOptions + [poPreserveWhiteSpace];
    FDocument.LoadFromFile(AFileName);
    FNode := FDocument.DocumentElement;
  end;
  DocumentLoad;
end;

procedure TCustomProj.SaveToFile(const AFormatted: Boolean; const AFileName: string = '');
var
  LXML: string;
begin
  if not AFileName.IsEmpty or TFile.Exists(FFileName) then
  begin
    if not AFileName.IsEmpty then
      FFileName := AFileName;
    LXML := FDocument.XML.Text;
    if AFormatted then
      LXML := FormatXMLData(LXML);
    TFile.WriteAllText(FFileName, LXML);
  end;
end;

procedure TCustomProj.SaveToFile(const AFileName: string = '');
begin
  SaveToFile(False, AFileName);
end;

procedure TCustomProj.DocumentLoad;
begin
  //
end;

function TCustomProj.GetPropertyGroup(const AIndex: Integer): IProjPropertyGroup;
var
  I, LIndex: Integer;
begin
  Result := nil;
  LIndex := -1;
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    if FNode.ChildNodes[I].NodeName.Equals(cPropertyGroup) then
    begin
      Inc(LIndex);
      if AIndex = LIndex then
      begin
        Result := TProjPropertyGroup.Create(FNode.ChildNodes[I]);
        Break;
      end;
    end;
  end;
end;

function TCustomProj.GetPropertyGroupCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    if FNode.ChildNodes[I].NodeName.Equals(cPropertyGroup) then
      Inc(Result);
  end;
end;

{ TEmptyPropertyGroup }

constructor TEmptyPropertyGroup.Create;
begin
  inherited Create(nil);
end;

function TEmptyPropertyGroup.DeleteValue(const AName: string): Boolean;
begin
  Result := False;
end;

function TEmptyPropertyGroup.FindValue(const AName: string; out AValue: string): Boolean;
begin
  Result := False;
  AValue := '';
end;

function TEmptyPropertyGroup.GetCondition: string;
begin
  Result := '';
end;

function TEmptyPropertyGroup.GetValues: TNameValuePairs;
begin
  Result := FValues;
end;

procedure TEmptyPropertyGroup.SetValue(const AName, AValue: string);
begin
  //
end;

end.
