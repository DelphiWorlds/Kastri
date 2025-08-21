unit DW.Proj;

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

  IProjDeployClassPlatform =  interface(IInterface)
    ['{D980E242-A0A1-40B2-B391-3C6587F5EBD6}']
    function GetName: string;
    function GetOperation: Integer;
    function GetRemoteDir: string;
  end;

  IProjDeployClass =  interface(IInterface)
    ['{9362924C-9E0A-4A22-81B9-D7EE72EEEC29}']
    function GetName: string;
    function GetPlatform(const AIndex: Integer): IProjDeployClassPlatform; overload;
    function GetPlatform(const AName: string): IProjDeployClassPlatform; overload;
    function GetPlatformCount: Integer;
  end;

  IProjDeployFilePlatform =  interface(IInterface)
    ['{A0C25429-6526-4AE2-A899-6224E2CEFB1E}']
    function GetName: string;
    function GetOverwrite: Boolean;
    function GetRemoteDir: string;
    function GetRemoteName: string;
    function HasRemote(const AClass: IProjDeployClass; const ARemoteDir, ARemoteName: string): Boolean;
  end;

  IProjDeployFile =  interface(IInterface)
    ['{D8433D68-974D-4FB8-BA75-10FCB555296D}']
    function GetConfiguration: string;
    function GetDeployClassName: string;
    function GetLocalName: string;
    function GetPlatform(const AIndex: Integer): IProjDeployFilePlatform; overload;
    function GetPlatform(const AName: string): IProjDeployFilePlatform; overload;
    function GetPlatformCount: Integer;
    function HasRemote(const AClass: IProjDeployClass; const APlatform, AConfig, ARemoteDir, ARemoteName: string): Boolean;
  end;

  IProjDeployment =  interface(IInterface)
    ['{11CAA93F-F95B-42E8-BA5C-B45B920EB7DE}']
    function GetDeployClass(const AIndex: Integer): IProjDeployClass; overload;
    function GetDeployClass(const AName: string): IProjDeployClass; overload;
    function GetDeployClassCount: Integer;
    function GetDeployFile(const AIndex: Integer): IProjDeployFile;
    function GetDeployFileCount: Integer;
    function HasDeployFile(const APlatform, AConfig, ARemoteDir, ARemoteName: string): Boolean;
  end;

  IProjBorlandProject = interface(IInterface)
    ['{6A337219-BC7A-4561-82C1-8F65F80066AA}']
    function GetDeployment: IProjDeployment;
    function GetPlatform(const AIndex: Integer): IProjPlatform;
    function GetPlatformCount: Integer;
  end;

  IProjProjectExtensions = interface(IInterface)
    ['{27561288-BBBF-438D-8451-85FB8E1D0710}']
    function GetBorlandProject: IProjBorlandProject;
  end;

(*
  IProjPropertyGroup =  interface(IProjNode)
    ['{06A05376-B340-4FE1-B402-E60101756B7B}']
    function FindValue(const AName: string; out AValue: string): Boolean;
    function GetCondition: string;
  end;
*)

  IProjItemGroup =  interface(IProjNode)
    ['{DEA593F6-78E9-4E54-9FA3-AC25CF7911F3}']
  end;

  IProj = interface(ICustomProj)
    ['{3FF84EDB-86BD-461F-9CA7-94C47997D067}']
    function FindBasePropertyGroup(out AGroup: IProjPropertyGroup): Boolean;
    function FindPropertyGroup(const AConfigIdent, APlatform: string; out AGroup: IProjPropertyGroup): Boolean;
    function GetCompilerOptions(const APlatform: string = ''; const AConfig: string = ''): TProjCompilerOptions;
    function GetConfigs: TProjConfigs;
    function GetPlatforms(const AEnabledOnly: Boolean): TArray<string>;
    function GetProjectDeployment: IProjDeployment;
    function GetProjectExtensions: IProjProjectExtensions;
    function GetProjectPaths(const APlatform: string = ''; const AConfig: string = ''): TArray<string>;
  end;

  TProj = class(TCustomProj, IProj)
  private
    FProjectExtensions: IProjProjectExtensions;
    function FindConfigIdent(const AConfig: string; out AIdent: string): Boolean;
    function InternalFindPropertyGroup(const ACondition: string; out AGroup: IProjPropertyGroup): Boolean;
    function InternalGetCompilerOptions(const ACondition: string; const AParentOptions: TProjCompilerOptions): TProjCompilerOptions;
    function InternalGetProjectPaths(const ACondition: string): TArray<string>;
  public
    { IProj }
    function FindBasePropertyGroup(out AGroup: IProjPropertyGroup): Boolean;
    function FindPropertyGroup(const AConfigIdent, APlatform: string; out AGroup: IProjPropertyGroup): Boolean;
    function GetCompilerOptions(const APlatform: string = ''; const AConfig: string = ''): TProjCompilerOptions;
    function GetConfigs: TProjConfigs;
    function GetPlatforms(const AEnabledOnly: Boolean = False): TArray<string>;
    function GetProjectDeployment: IProjDeployment;
    function GetProjectExtensions: IProjProjectExtensions;
    function GetProjectPaths(const APlatform: string = ''; const AConfig: string = ''): TArray<string>;
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
  cDCCPrefix = 'DCC_';

type
  TProjCompilerOptionsHelper = record helper for TProjCompilerOptions
    procedure Add(const ACompilerOption: TProjCompilerOption);
    function Count: Integer;
    function IndexOf(const ACompilerOption: TProjCompilerOption): Integer;
  end;

  TProjProjectExtensions = class(TCustomProjNode, IProjProjectExtensions)
  private
    FBorlandProject: IProjBorlandProject;
  public
    { IProjProjectExtensions }
    function GetBorlandProject: IProjBorlandProject;
  end;

  TProjPlatforms = TArray<IProjPlatform>;

  TProjBorlandProject = class(TCustomProjNode, IProjBorlandProject)
  private
    FDeployment: IProjDeployment;
    FPlatforms: TProjPlatforms;
    procedure GetPlatforms;
  public
    { IProjBorlandProject }
    function GetDeployment: IProjDeployment;
    function GetPlatform(const AIndex: Integer): IProjPlatform;
    function GetPlatformCount: Integer;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

  TProjPlatform = class(TCustomProjNode, IProjPlatform)
  private
    FName: string;
    FIsEnabled: Boolean;
  public
    { IProjPlatform }
    function GetName: string;
    function GetIsEnabled: Boolean;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

  TProjDeployClasses = TArray<IProjDeployClass>;
  TProjDeployFiles = TArray<IProjDeployFile>;

  TProjDeployment = class(TCustomProjNode, IProjDeployment)
  private
    FDeployClasses: TProjDeployClasses;
    FDeployFiles: TProjDeployFiles;
    procedure GetDeployClasses;
    procedure GetDeployFiles;
  public
    { IProjDeployment }
    function GetDeployClass(const AIndex: Integer): IProjDeployClass; overload;
    function GetDeployClass(const AName: string): IProjDeployClass; overload;
    function GetDeployClassCount: Integer;
    function GetDeployFile(const AIndex: Integer): IProjDeployFile;
    function GetDeployFileCount: Integer;
    function HasDeployFile(const APlatform, AConfig, ARemoteDir, ARemoteName: string): Boolean;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

  TProjDeployClassPlatforms = TArray<IProjDeployClassPlatform>;

  TProjDeployClass = class(TCustomProjNode, IProjDeployClass)
  private
    FName: string;
    FPlatforms: TProjDeployClassPlatforms;
    procedure GetPlatforms;
  public
    { IProjDeployClass }
    function GetName: string;
    function GetPlatform(const AIndex: Integer): IProjDeployClassPlatform; overload;
    function GetPlatform(const AName: string): IProjDeployClassPlatform; overload;
    function GetPlatformCount: Integer;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

  TProjDeployClassPlatform = class(TCustomProjNode, IProjDeployClassPlatform)
  private
    FName: string;
    FOperation: Integer;
    FRemoteDir: string;
  public
    { IProjDeployClassPlatform }
    function GetName: string;
    function GetOperation: Integer;
    function GetRemoteDir: string;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

  TProjDeployFilePlatforms = TArray<IProjDeployFilePlatform>;

  TProjDeployFile = class(TCustomProjNode, IProjDeployFile)
  private
    FConfiguration: string;
    FDeployClassName: string;
    FLocalName: string;
    FPlatforms: TProjDeployFilePlatforms;
    procedure GetPlatforms;
  public
    { IProjDeployFile }
    function GetConfiguration: string;
    function GetDeployClassName: string;
    function GetLocalName: string;
    function GetPlatform(const AIndex: Integer): IProjDeployFilePlatform; overload;
    function GetPlatform(const AName: string): IProjDeployFilePlatform; overload;
    function GetPlatformCount: Integer;
    function HasRemote(const AClass: IProjDeployClass; const APlatform, AConfig, ARemoteDir, ARemoteName: string): Boolean;
    // function Matches(const APlatform, AConfig, ARemoteDir, ARemoteName: string): Boolean;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

  TProjDeployFilePlatform = class(TCustomProjNode, IProjDeployFilePlatform)
  private
    FName: string;
    FOverwrite: Boolean;
    FRemoteDir: string;
    FRemoteName: string;
  public
    { IProjDeployFilePlatform }
    function GetName: string;
    function GetOverwrite: Boolean;
    function GetRemoteDir: string;
    function GetRemoteName: string;
    function HasRemote(const AClass: IProjDeployClass; const ARemoteDir, ARemoteName: string): Boolean;
  public
    constructor Create(const ANode: IXMLNode); override;
  end;

function SamePath(const APath, ACompareTo: string): Boolean;
begin
  Result := SameText(APath.TrimRight(['\']), ACompareTo.TrimRight(['\']));
end;

{ TProjCompilerOptionsHelper }

procedure TProjCompilerOptionsHelper.Add(const ACompilerOption: TProjCompilerOption);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ACompilerOption);
  if LIndex > -1 then
    Self[LIndex] := ACompilerOption
  else
    Self := Self + [ACompilerOption];
end;

function TProjCompilerOptionsHelper.Count: Integer;
begin
  Result := Length(Self);
end;

function TProjCompilerOptionsHelper.IndexOf(const ACompilerOption: TProjCompilerOption): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Self[I].Name.Equals(ACompilerOption.Name) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{ TProj }

function TProj.FindBasePropertyGroup(out AGroup: IProjPropertyGroup): Boolean;
begin
  Result := InternalFindPropertyGroup(cConditionBase, AGroup);
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
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    LChildNode := Node.ChildNodes[I];
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

function TProj.GetCompilerOptions(const APlatform, AConfig: string): TProjCompilerOptions;
var
  LConditionPlatform, LConditionPlatformConfig: string;
  LConfigIdent: string;
begin
  Result := InternalGetCompilerOptions(cConditionBase, []);
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
    Result := InternalGetCompilerOptions(LConditionPlatform, Result);
  if not LConditionPlatformConfig.IsEmpty then
    Result := InternalGetCompilerOptions(LConditionPlatformConfig, Result);
end;

function TProj.InternalGetCompilerOptions(const ACondition: string; const AParentOptions: TProjCompilerOptions): TProjCompilerOptions;
var
  LGroup: IProjPropertyGroup;
  LCompilerOption: TProjCompilerOption;
  LChild: IXMLNode;
  I: Integer;
begin
  Result := AParentOptions;
  if InternalFindPropertyGroup(ACondition, LGroup) then
  begin
    for I := 0 to LGroup.ChildCount - 1 do
    begin
      LChild := LGroup.GetChild(I);
      if LChild.NodeName.StartsWith(cDCCPrefix, True) then
      begin
        LCompilerOption := Default(TProjCompilerOption);
        LCompilerOption.Name := LChild.NodeName;
        LCompilerOption.Value := LChild.Text;
        // Next line needs a lookup
        // LCompilerOption.Switch :=
        Result.Add(LCompilerOption);
      end;
    end;
  end;
end;

function TProj.GetConfigs: TProjConfigs;
var
  I: Integer;
  LChildNode: IXMLNode;
  LCondition: string;
  LParts: TArray<string>;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    LChildNode := Node.ChildNodes[I];
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
      AIdent := LGroup.GetChild(0).NodeName;
      Result := not AIdent.IsEmpty;
      Break;
    end;
  end;
end;

function TProj.GetProjectDeployment: IProjDeployment;
begin
  if (GetProjectExtensions <> nil) and (GetProjectExtensions.GetBorlandProject <> nil) then
    Result := GetProjectExtensions.GetBorlandProject.GetDeployment;
end;

function TProj.GetProjectExtensions: IProjProjectExtensions;
var
  LNode: IXMLNode;
  I: Integer;
begin
  if FProjectExtensions = nil then
  begin
    for I := 0 to Node.ChildNodes.Count - 1 do
    begin
      LNode := Node.ChildNodes[I];
      if LNode.NodeName.Equals('ProjectExtensions') and (LNode.ChildNodes.Count > 0) then
      begin
        FProjectExtensions := TProjProjectExtensions.Create(LNode);
        Break;
      end;
    end;
  end;
  Result := FProjectExtensions;
end;

{ TProjProjectExtensions }

function TProjProjectExtensions.GetBorlandProject: IProjBorlandProject;
var
  LNode: IXMLNode;
begin
  if FBorlandProject = nil then
  begin
    LNode := Node.ChildNodes.FindNode('BorlandProject');
    if LNode <> nil then
      FBorlandProject := TProjBorlandProject.Create(LNode);
  end;
  Result := FBorlandProject;
end;

{ TProjDeployment }

constructor TProjDeployment.Create(const ANode: IXMLNode);
begin
  inherited;
  GetDeployClasses;
  GetDeployFiles;
end;

procedure TProjDeployment.GetDeployFiles;
var
  I: Integer;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    if Node.ChildNodes[I].NodeName.Equals('DeployFile') then
      FDeployFiles := FDeployFiles + [TProjDeployFile.Create(Node.ChildNodes[I])];
  end;
end;

procedure TProjDeployment.GetDeployClasses;
var
  I: Integer;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    if Node.ChildNodes[I].NodeName.Equals('DeployClass') then
      FDeployClasses := FDeployClasses + [TProjDeployClass.Create(Node.ChildNodes[I])];
  end;
end;

function TProjDeployment.GetDeployClass(const AIndex: Integer): IProjDeployClass;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < GetDeployClassCount) then
    Result := FDeployClasses[AIndex];
end;

function TProjDeployment.GetDeployClass(const AName: string): IProjDeployClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to GetDeployClassCount - 1 do
  begin
    if SameText(FDeployClasses[I].GetName, AName) then
    begin
      Result := FDeployClasses[I];
      Break;
    end;
  end;
end;

function TProjDeployment.GetDeployClassCount: Integer;
begin
  Result := Length(FDeployClasses);
end;

function TProjDeployment.GetDeployFile(const AIndex: Integer): IProjDeployFile;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < GetDeployFileCount) then
    Result := FDeployFiles[AIndex];
end;

function TProjDeployment.GetDeployFileCount: Integer;
begin
  Result := Length(FDeployFiles);
end;

function TProjDeployment.HasDeployFile(const APlatform, AConfig, ARemoteDir, ARemoteName: string): Boolean;
var
  LDeployFile: IProjDeployFile;
  LDeployClass: IProjDeployClass;
  I: Integer;
begin
  Result := False;
  for I := 0 to GetDeployFileCount - 1 do
  begin
    LDeployFile := GetDeployFile(I);
    LDeployClass := GetDeployClass(LDeployFile.GetDeployClassName);
    if LDeployFile.HasRemote(LDeployClass, APlatform, AConfig, ARemoteDir, ARemoteName) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TProjBorlandProject }

constructor TProjBorlandProject.Create(const ANode: IXMLNode);
begin
  inherited;
  FDeployment := TProjDeployment.Create(Node.ChildNodes.FindNode('Deployment'));
  GetPlatforms;
end;

function TProjBorlandProject.GetDeployment: IProjDeployment;
begin
  Result := FDeployment;
end;

function TProjBorlandProject.GetPlatform(const AIndex: Integer): IProjPlatform;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < GetPlatformCount) then
    Result := FPlatforms[AIndex];
end;

function TProjBorlandProject.GetPlatformCount: Integer;
begin
  Result := Length(FPlatforms);
end;

procedure TProjBorlandProject.GetPlatforms;
var
  I: Integer;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    if Node.ChildNodes[I].NodeName.Equals('Platform') then
      FPlatforms := FPlatforms +  [TProjPlatform.Create(Node.ChildNodes[I])];
  end;
end;

{ TProjPlatform }

constructor TProjPlatform.Create(const ANode: IXMLNode);
begin
  inherited;
  FName := GetAttributeText('value');
  FIsEnabled := Node.Text.Equals('True');
end;

function TProjPlatform.GetIsEnabled: Boolean;
begin
  Result := FIsEnabled;
end;

function TProjPlatform.GetName: string;
begin
  Result := FName;
end;

{ TProjDeployClass }

constructor TProjDeployClass.Create(const ANode: IXMLNode);
begin
  inherited;
  FName := GetAttributeText('Name');
  GetPlatforms;
end;

procedure TProjDeployClass.GetPlatforms;
var
  I: Integer;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    if Node.ChildNodes[I].NodeName.Equals('Platform') then
      FPlatforms := FPlatforms + [TProjDeployClassPlatform.Create(Node.ChildNodes[I])];
  end;
end;

function TProjDeployClass.GetPlatform(const AIndex: Integer): IProjDeployClassPlatform;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < GetPlatformCount) then
    Result := FPlatforms[AIndex];
end;

function TProjDeployClass.GetName: string;
begin
  Result := FName;
end;

function TProjDeployClass.GetPlatform(const AName: string): IProjDeployClassPlatform;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to GetPlatformCount - 1 do
  begin
    if SameText(FPlatforms[I].GetName, AName) then
    begin
      Result := FPlatforms[I];
      Break;
    end;
  end;
end;

function TProjDeployClass.GetPlatformCount: Integer;
begin
  Result := Length(FPlatforms);
end;

{ TProjDeployFile }

constructor TProjDeployFile.Create(const ANode: IXMLNode);
begin
  inherited;
  FConfiguration := GetAttributeText('Configuration');
  FDeployClassName := GetAttributeText('Class');
  FLocalName := GetAttributeText('LocalName');
  GetPlatforms;
end;

function TProjDeployFile.GetConfiguration: string;
begin
  Result := FConfiguration;
end;

function TProjDeployFile.GetDeployClassName: string;
begin
  Result := FDeployClassName;
end;

function TProjDeployFile.GetLocalName: string;
begin
  Result := FLocalName;
end;

procedure TProjDeployFile.GetPlatforms;
var
  I: Integer;
begin
  for I := 0 to Node.ChildNodes.Count - 1 do
  begin
    if Node.ChildNodes[I].NodeName.Equals('Platform') then
      FPlatforms := FPlatforms +  [TProjDeployFilePlatform.Create(Node.ChildNodes[I])];
  end;
end;

function TProjDeployFile.GetPlatform(const AIndex: Integer): IProjDeployFilePlatform;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < GetPlatformCount) then
    Result := FPlatforms[AIndex];
end;

function TProjDeployFile.GetPlatform(const AName: string): IProjDeployFilePlatform;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to GetPlatformCount - 1 do
  begin
    if SameText(FPlatforms[I].GetName, AName) then
    begin
      Result := FPlatforms[I];
      Break;
    end;
  end;
end;

function TProjDeployFile.GetPlatformCount: Integer;
begin
  Result := Length(FPlatforms);
end;

function TProjDeployFile.HasRemote(const AClass: IProjDeployClass; const APlatform, AConfig, ARemoteDir, ARemoteName: string): Boolean;
var
  LFilePlatform: IProjDeployFilePlatform;
begin
  Result := False;
  if SameText(GetConfiguration, AConfig) then
  begin
    LFilePlatform := GetPlatform(APlatform);
    if (LFilePlatform <> nil) and LFilePlatform.HasRemote(AClass, ARemoteDir, ARemoteName) then
      Result := True;
  end;
end;

{ TProjDeployFilePlatform }

constructor TProjDeployFilePlatform.Create(const ANode: IXMLNode);
begin
  inherited;
  FName := VarToStrDef(Node.Attributes['Name'], '');
  FOverwrite := GetChildBoolean('Overwrite');
  FRemoteDir := GetChildText('RemoteDir');
  FRemoteName := GetChildText('RemoteName');
end;

function TProjDeployFilePlatform.GetName: string;
begin
  Result := FName;
end;

function TProjDeployFilePlatform.GetOverwrite: Boolean;
begin
  Result := FOverwrite;
end;

function TProjDeployFilePlatform.GetRemoteDir: string;
begin
  Result := FRemoteDir;
end;

function TProjDeployFilePlatform.GetRemoteName: string;
begin
  Result := FRemoteName;
end;

function TProjDeployFilePlatform.HasRemote(const AClass: IProjDeployClass; const ARemoteDir, ARemoteName: string): Boolean;
var
  LRemoteDir: string;
  LClassPlatform: IProjDeployClassPlatform;
begin
  LRemoteDir := GetRemoteDir;
  if LRemoteDir.IsEmpty and (AClass <> nil) then
  begin
    LClassPlatform := AClass.GetPlatform(GetName);
    if LClassPlatform <> nil then
      LRemoteDir := LClassPlatform.GetRemoteDir;
  end;
  Result := SamePath(LRemoteDir, ARemoteDir) and SameText(GetRemoteName, ARemoteName);
end;

{ TProjDeployClassPlatform }

constructor TProjDeployClassPlatform.Create(const ANode: IXMLNode);
begin
  inherited;
  FName := GetAttributeText('Name');
  FOperation := GetChildInteger('Operation', -1);
  FRemoteDir := GetChildText('RemoteDir');
end;

function TProjDeployClassPlatform.GetName: string;
begin
  Result := FName;
end;

function TProjDeployClassPlatform.GetOperation: Integer;
begin
  Result := FOperation;
end;

function TProjDeployClassPlatform.GetRemoteDir: string;
begin
  Result := FRemoteDir;
end;

end.
