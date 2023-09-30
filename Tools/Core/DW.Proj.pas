unit DW.Proj;

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
  // Xml
  Xml.XMLIntf, Xml.XMLDoc,
  // DW
  DW.Proj.Types;

type
  IProjPlatform =  interface(IInterface)
    ['{B1E0FDEF-74AA-4A59-B242-7455F00E7B6F}']
    function GetName: string;
    function GetIsEnabled: Boolean;
  end;

  IProjBorlandProject = interface(IInterface)
    ['{6A337219-BC7A-4561-82C1-8F65F80066AA}']
    function GetPlatform(const AIndex: Integer): IProjPlatform;
    function GetPlatformCount: Integer;
  end;

  IProjProjectExtensions = interface(IInterface)
    ['{27561288-BBBF-438D-8451-85FB8E1D0710}']
    function GetBorlandProject: IProjBorlandProject;
  end;

  IProjPropertyGroup =  interface(IInterface)
    ['{06A05376-B340-4FE1-B402-E60101756B7B}']
    function FindValue(const AName: string; out AValue: string): Boolean;
    function GetChildNodeName(const AIndex: Integer): string;
    function GetCondition: string;
  end;

  IProjItemGroup =  interface(IInterface)
    ['{DEA593F6-78E9-4E54-9FA3-AC25CF7911F3}']
  end;

  IProj = interface(IInterface)
    ['{3FF84EDB-86BD-461F-9CA7-94C47997D067}']
    function FindPropertyGroup(const AConfigIdent, APlatform: string; out AGroup: IProjPropertyGroup): Boolean;
    function GetConfigs: TProjConfigs;
    function GetPlatforms(const AEnabledOnly: Boolean): TArray<string>;
    function GetProjectPaths(const APlatform: string = ''; const AConfig: string = ''): TArray<string>;
    function GetProjectExtensions: IProjProjectExtensions;
    function GetPropertyGroup(const AIndex: Integer): IProjPropertyGroup;
    function GetPropertyGroupCount: Integer;
    procedure LoadFromFile(const AFileName: string);
  end;

  TProj = class(TInterfacedObject, IProj)
  private
    FDocument: IXMLDocument;
    FNode: IXMLNode;
    FProjectExtensions: IProjProjectExtensions;
    function FindConfigIdent(const AConfig: string; out AIdent: string): Boolean;
    function InternalFindPropertyGroup(const ACondition: string; out AGroup: IProjPropertyGroup): Boolean;
    function InternalGetProjectPaths(const ACondition: string): TArray<string>;
  public
    { IProj }
    function FindPropertyGroup(const AConfigIdent, APlatform: string; out AGroup: IProjPropertyGroup): Boolean;
    function GetConfigs: TProjConfigs;
    function GetPlatforms(const AEnabledOnly: Boolean = False): TArray<string>;
    function GetProjectPaths(const APlatform: string = ''; const AConfig: string = ''): TArray<string>;
    function GetProjectExtensions: IProjProjectExtensions;
    function GetPropertyGroup(const AIndex: Integer): IProjPropertyGroup;
    function GetPropertyGroupCount: Integer;
    procedure LoadFromFile(const AFileName: string);
  public
    constructor Create(const AFileName: string);
  end;

implementation

uses
  // RTL
  System.IOUtils, System.SysUtils, System.StrUtils, System.Variants;

const
  cConditionBase = '''$(Base)''!=''''';
  cConditionBasePlatformTemplate = '''$(Base_%s)''!=''''';
  cConditionConfigTemplate = '''$(Config)''==''%s''';
  cConditionConfigPlatformTemplate = '''$(%s_%s)''!=''''';

  cConditionConfigStartsWith = '''$(Config)''==';

  cPropertyGroupUnitSearchPathName = 'DCC_UnitSearchPath';
  cDCC_UnitSearchPathMacro = '$(DCC_UnitSearchPath)';

type
  TCustomProjNode = class(TInterfacedObject)
  private
    FNode: IXMLNode;
  protected
    property Node: IXMLNode read FNode;
  public
    constructor Create(const ANode: IXMLNode); virtual;
  end;

  TProjProjectExtensions = class(TCustomProjNode, IProjProjectExtensions)
  private
    FBorlandProject: IProjBorlandProject;
  public
    { IProjProjectExtensions }
    function GetBorlandProject: IProjBorlandProject;
  end;

  TProjPropertyGroup = class(TCustomProjNode, IProjPropertyGroup)
  private
    FCondition: string;
  public
    { IProjPropertyGroup }
    function GetChildNodeName(const AIndex: Integer): string;
    function GetCondition: string;
    function FindValue(const AName: string; out AValue: string): Boolean;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

  TProjBorlandProject = class(TCustomProjNode, IProjBorlandProject)
  public
    { IProjBorlandProject }
    function GetPlatform(const AIndex: Integer): IProjPlatform;
    function GetPlatformCount: Integer;
  end;

  TProjPlatform = class(TCustomProjNode, IProjPlatform)
  public
    { IProjPlatform }
    function GetName: string;
    function GetIsEnabled: Boolean;
  end;

{ TProj }

constructor TProj.Create(const AFileName: string);
begin
  inherited Create;
  LoadFromFile(AFileName);
end;

function TProj.FindPropertyGroup(const AConfigIdent, APlatform: string; out AGroup: IProjPropertyGroup): Boolean;
begin
  Result := InternalFindPropertyGroup(Format(cConditionConfigPlatformTemplate, [AConfigIdent, APlatform]), AGroup);
end;

function TProj.InternalFindPropertyGroup(const ACondition: string; out AGroup: IProjPropertyGroup): Boolean;
var
  I: Integer;
  LChildNode: IXMLNode;
  LCondition: string;
begin
  Result := False;
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    LChildNode := FNode.ChildNodes[I];
    LCondition := VarToStrDef(LChildNode.Attributes['Condition'], '');
    if not LCondition.IsEmpty and LChildNode.NodeName.Equals('PropertyGroup') and SameText(ACondition, LCondition) then
    begin
      AGroup := TProjPropertyGroup.Create(LChildNode);
      Result := True;
      Break;
    end;
  end;
end;

function TProj.InternalGetProjectPaths(const ACondition: string): TArray<string>;
var
  LGroup: IProjPropertyGroup;
  LValue: string;
  LMacroIndex: Integer;
begin
  if InternalFindPropertyGroup(ACondition, LGroup) and LGroup.FindValue(cPropertyGroupUnitSearchPathName, LValue) then
  begin
    Result := LValue.Split([';']);
    LMacroIndex := IndexStr(cDCC_UnitSearchPathMacro, Result);
    if LMacroIndex > -1 then
      Delete(Result, LMacroIndex, 1);
  end;
end;

function TProj.GetConfigs: TProjConfigs;
var
  I: Integer;
  LChildNode: IXMLNode;
  LCondition: string;
  LParts: TArray<string>;
begin
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    LChildNode := FNode.ChildNodes[I];
    LCondition := VarToStrDef(LChildNode.Attributes['Condition'], '');
    if not LCondition.IsEmpty and ((LChildNode.ChildNodes.Count > 0) and SameText(LChildNode.ChildNodes[0].Text, 'true'))
      and LChildNode.NodeName.Equals('PropertyGroup') and LCondition.StartsWith(cConditionConfigStartsWith, True) then
    begin
      // e.g. Debug will look like this: '$(Config)'=='Debug' or '$(Cfg_1)'!=''
      LParts := LCondition.Split([' ']);
      if Length(LParts) > 0 then
      begin
        LParts := LParts[0].Replace('''', '').Split(['==']);
        if Length(LParts) > 1 then
          Result := Result + [TProjConfig.Create(LParts[1], LChildNode.ChildNodes[0].NodeName)];
      end;
    end;
  end;
end;

function TProj.GetPlatforms(const AEnabledOnly: Boolean = False): TArray<string>;
var
  LExtensions: IProjProjectExtensions;
  LBorlandProject: IProjBorlandProject;
  LPlatform: IProjPlatform;
  I: Integer;
begin
  LExtensions := GetProjectExtensions;
  if LExtensions <> nil then
  begin
    LBorlandProject := LExtensions.GetBorlandProject;
    if LBorlandProject <> nil then
    begin
      for I := 0 to LBorlandProject.GetPlatformCount - 1 do
      begin
        LPlatform := LBorlandProject.GetPlatform(I);
        if not AEnabledOnly or LPlatform.GetIsEnabled then
          Result := Result + [LPlatform.GetName];
      end;
    end;
  end;
end;

function TProj.GetProjectPaths(const APlatform: string = ''; const AConfig: string = ''): TArray<string>;
var
  LConditionPlatform, LConditionPlatformConfig: string;
  LConfigIdent: string;
begin
  Result := InternalGetProjectPaths(cConditionBase);
  LConditionPlatform := '';
  LConditionPlatformConfig := '';
  if not (APlatform.IsEmpty and AConfig.IsEmpty) then
  begin
    if not APlatform.IsEmpty then
    begin
      LConditionPlatform := Format(cConditionBasePlatformTemplate, [APlatform]);
      if not AConfig.IsEmpty and FindConfigIdent(AConfig, LConfigIdent) then
        LConditionPlatformConfig := Format(cConditionConfigPlatformTemplate, [LConfigIdent, APlatform]);
    end;
  end;
  if not LConditionPlatform.IsEmpty and not SameText(LConditionPlatformConfig, LConditionPlatform) then
    Result := Result + InternalGetProjectPaths(LConditionPlatform);
  if not LConditionPlatformConfig.IsEmpty then
    Result := Result + InternalGetProjectPaths(LConditionPlatformConfig);
end;

function TProj.FindConfigIdent(const AConfig: string; out AIdent: string): Boolean;
var
  I: Integer;
  LGroup: IProjPropertyGroup;
begin
  Result := False;
  for I := 0 to GetPropertyGroupCount - 1 do
  begin
    LGroup := GetPropertyGroup(I);
    if LGroup.GetCondition.StartsWith(Format(cConditionConfigTemplate, [AConfig])) then
    begin
      AIdent := LGroup.GetChildNodeName(0);
      Result := not AIdent.IsEmpty;
      Break;
    end;
  end;
end;

function TProj.GetProjectExtensions: IProjProjectExtensions;
var
  LNode: IXMLNode;
begin
  if FProjectExtensions = nil then
  begin
    LNode := FNode.ChildNodes.FindNode('ProjectExtensions');
    if LNode <> nil then
      FProjectExtensions := TProjProjectExtensions.Create(LNode);
  end;
  Result := FProjectExtensions;
end;

function TProj.GetPropertyGroup(const AIndex: Integer): IProjPropertyGroup;
var
  I, LIndex: Integer;
begin
  Result := nil;
  LIndex := -1;
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    if FNode.ChildNodes[I].NodeName.Equals('PropertyGroup') then
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

function TProj.GetPropertyGroupCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FNode.ChildNodes.Count - 1 do
  begin
    if FNode.ChildNodes[I].NodeName.Equals('PropertyGroup') then
      Inc(Result);
  end;
end;

procedure TProj.LoadFromFile(const AFileName: string);
begin
  FNode := nil;
  FDocument := nil;
  if TFile.Exists(AFileName) then
  begin
    FDocument := LoadXMLDocument(AFileName);
    FNode := FDocument.DocumentElement;
  end;
end;

{ TCustomProjNode }

constructor TCustomProjNode.Create(const ANode: IXMLNode);
begin
  inherited Create;
  FNode := ANode;
end;

{ TProjProjectExtensions }

function TProjProjectExtensions.GetBorlandProject: IProjBorlandProject;
var
  LNode: IXMLNode;
begin
  if FBorlandProject = nil then
  begin
    LNode := FNode.ChildNodes.FindNode('BorlandProject');
    if LNode <> nil then
      FBorlandProject := TProjBorlandProject.Create(LNode);
  end;
  Result := FBorlandProject;
end;

{ TProjBorlandProject }

function TProjBorlandProject.GetPlatform(const AIndex: Integer): IProjPlatform;
var
  LPlatformsNode: IXMLNode;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < GetPlatformCount) then
  begin
    LPlatformsNode := Node.ChildNodes.FindNode('Platforms');
    if LPlatformsNode <> nil then
      Result := TProjPlatform.Create(LPlatformsNode.ChildNodes[AIndex]);
  end;
end;

function TProjBorlandProject.GetPlatformCount: Integer;
var
  LPlatformsNode: IXMLNode;
begin
  Result := 0;
  LPlatformsNode := Node.ChildNodes.FindNode('Platforms');
  if LPlatformsNode <> nil then
    Result := LPlatformsNode.ChildNodes.Count;
end;

{ TProjPlatform }

function TProjPlatform.GetIsEnabled: Boolean;
begin
  Result := Node.Text.Equals('True');
end;

function TProjPlatform.GetName: string;
begin
  Result := VarToStrDef(Node.Attributes['value'], '');
end;

{ TProjPropertyGroup }

constructor TProjPropertyGroup.Create(const ANode: IXMLNode);
begin
  inherited;
  FCondition := VarToStrDef(ANode.Attributes['Condition'], '');
end;

function TProjPropertyGroup.FindValue(const AName: string; out AValue: string): Boolean;
var
  LChildNode: IXMLNode;
  I: Integer;
begin
  Result := False;
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    LChildNode := Node.ChildNodes[I];
    if LChildNode.NodeName.Equals(AName) then
    begin
      AValue := LChildNode.Text;
      Result := True;
      Break;
    end;
  end;
end;

function TProjPropertyGroup.GetChildNodeName(const AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < Node.ChildNodes.Count) then
    Result := Node.ChildNodes[AIndex].NodeName;
end;

function TProjPropertyGroup.GetCondition: string;
begin
  Result := FCondition;
end;

end.
