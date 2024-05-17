unit DW.DelphiAST;

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

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Classes,
  // DelphiAST
  DelphiAST, DelphiAST.Classes, DelphiAST.Consts;

type
  TSilentPasSyntaxTreeBuilder = class(TPasSyntaxTreeBuilder)
  private
    function RunNoMessages(const ASource: TStream): TSyntaxNode;
  public
    class function RunFile(const AFileName: string): TSyntaxNode;
  end;

  TSyntaxNodeProc = reference to procedure(const Node: TSyntaxNode);

  TSyntaxNodeEnumerateResult = (Continue, Stop);
  TSyntaxNodeEnumerateFunc = reference to function(const Node: TSyntaxNode): TSyntaxNodeEnumerateResult;

  TSyntaxNodeHelper = class helper for TSyntaxNode
  private
    procedure InternalGetDependentTypeNames(var ANames: TArray<string>);
  public
    function Count: Integer;
    procedure Enumerate(const ANodeType: TSyntaxNodeType; const AHandler: TSyntaxNodeEnumerateFunc); overload;
    procedure Enumerate(const AParentNodeType, ANodeType: TSyntaxNodeType; const AHandler: TSyntaxNodeEnumerateFunc); overload;
    function FindAttributes(out ANode: TSyntaxNode): Boolean;
    function FindDeclaration(const AName: string; out ANode: TSyntaxNode; const AIsForward: Boolean = False): Boolean; overload;
    function FindDeclaration(const AName: string; const ASection: TSyntaxNode; out ANode: TSyntaxNode;
      const AIsForward: Boolean = False): Boolean; overload;
    function FindJavaSignature(out ASignature: string): Boolean;
    function FindRoot: TSyntaxNode;
    procedure ForEach(const ANodeType: TSyntaxNodeType; const AHandler: TSyntaxNodeProc); overload;
    procedure ForEach(const AParentNodeType, ANodeType: TSyntaxNodeType; const AHandler: TSyntaxNodeProc); overload;
    function GetName: string;
    function GetType: string;
    function GetParentType: string;
    function GetDependentTypeNames: TArray<string>; overload;
    procedure GetDependentTypeNames(var ANames: TArray<string>); overload;
    function IsClass: Boolean;
    function IsForward: Boolean;
    function IsInterface: Boolean;
  end;

  TDelphiWriter = class(TStreamWriter)
  private
    FLevel: Integer;
    function GetTypeArgs(const ANode: TSyntaxNode): string;
    function Spaces: string;
    procedure WriteAttribute(const ANode: TSyntaxNode);
    procedure WriteGUID(const ANode: TSyntaxNode);
    procedure WriteMethod(const ANode: TSyntaxNode; const AIsPseudoClass: Boolean = False);
    procedure WriteParameter(const ANode: TSyntaxNode);
    procedure WriteReturnType(const ANode: TSyntaxNode);
  public
    procedure Indent;
    procedure Outdent;
    procedure WriteAttributes(const ANode: TSyntaxNode);
    procedure WriteType(const ANode: TSyntaxNode; const AIsPseudoClass: Boolean = False);
  end;

implementation

uses
  // RTL
  System.SysUtils, System.StrUtils;

{ TSilentPasSyntaxTreeBuilder }

class function TSilentPasSyntaxTreeBuilder.RunFile(const AFileName: string): TSyntaxNode;
var
  LStream: TMemoryStream;
  LBuilder: TSilentPasSyntaxTreeBuilder;
begin
  LBuilder := TSilentPasSyntaxTreeBuilder.Create;
  try
    LBuilder.InitDefinesDefinedByCompiler;
    LStream := TMemoryStream.Create;
    try
      LStream.LoadFromFile(AFileName);
      Result := LBuilder.RunNoMessages(LStream);
    finally
      LStream.Free;
    end;
  finally
    LBuilder.Free;
  end;
end;

function TSilentPasSyntaxTreeBuilder.RunNoMessages(const ASource: TStream): TSyntaxNode;
begin
  Result := TSyntaxNode.Create(ntUnit);
  FStack.Clear;
  FStack.Push(Result);
  try
    inherited Run('', ASource);
  finally
    FStack.Pop;
  end;
end;

{ TSyntaxNodeHelper }

function TSyntaxNodeHelper.Count: Integer;
begin
  Result := Length(ChildNodes);
end;

procedure TSyntaxNodeHelper.Enumerate(const AParentNodeType, ANodeType: TSyntaxNodeType; const AHandler: TSyntaxNodeEnumerateFunc);
var
  LNode, LTargetNode: TSyntaxNode;
begin
  LTargetNode := nil;
  for LNode in ChildNodes do
  begin
    if LNode.Typ = AParentNodeType then
    begin
      LNode.Enumerate(ANodeType,
        function(const ANode: TSyntaxNode): TSyntaxNodeEnumerateResult
        begin
          if AHandler(ANode) = TSyntaxNodeEnumerateResult.Stop then
          begin
            LTargetNode := ANode;
            Result := TSyntaxNodeEnumerateResult.Stop;
          end
          else
            Result := TSyntaxNodeEnumerateResult.Continue;
        end
      );
    end;
    if LTargetNode <> nil then
      Break;
  end;
end;

procedure TSyntaxNodeHelper.Enumerate(const ANodeType: TSyntaxNodeType; const AHandler: TSyntaxNodeEnumerateFunc);
var
  LNode: TSyntaxNode;
begin
  for LNode in ChildNodes do
  begin
    if (LNode.Typ = ANodeType) and (AHandler(LNode) = TSyntaxNodeEnumerateResult.Stop) then
      Break;
  end;
end;

function TSyntaxNodeHelper.FindAttributes(out ANode: TSyntaxNode): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ParentNode <> nil then
  begin
    for I := 0 to ParentNode.Count - 1 do
    begin
      if ParentNode.ChildNodes[I] = Self then
      begin
        if (I > 0) and (ParentNode.ChildNodes[I - 1].Typ = TSyntaxNodeType.ntAttributes) then
        begin
          ANode := ParentNode.ChildNodes[I - 1];
          Result := True;
        end;
        Break;
      end;
    end;
  end;
end;

function TSyntaxNodeHelper.FindDeclaration(const AName: string; const ASection: TSyntaxNode; out ANode: TSyntaxNode;
  const AIsForward: Boolean = False): Boolean;
var
  LTargetNode: TSyntaxNode;
begin
  LTargetNode := nil;
  if ASection <> nil then
  begin
    ASection.Enumerate(TSyntaxNodeType.ntTypeSection, TSyntaxNodeType.ntTypeDecl,
      function(const ANode: TSyntaxNode): TSyntaxNodeEnumerateResult
      begin
        if (ANode.IsForward = AIsForward) and ANode.GetName.Equals(AName) then
        begin
          LTargetNode := ANode;
          Result := TSyntaxNodeEnumerateResult.Stop
        end
        else
          Result := TSyntaxNodeEnumerateResult.Continue;
      end
    );
  end;
  ANode := LTargetNode;
  Result := ANode <> nil;
end;

function TSyntaxNodeHelper.FindJavaSignature(out ASignature: string): Boolean;
var
  LAttributes, LAttribute, LName, LArguments, LArgument, LLiteral: TSyntaxNode;
begin
  Result := False;
  if FindAttributes(LAttributes) then
  begin
    for LAttribute in LAttributes.ChildNodes do
    begin
      LName := LAttribute.FindNode(TSyntaxNodeType.ntName);
      if (LName <> nil) and (LName is TValuedSyntaxNode) and TValuedSyntaxNode(LName).Value.Equals('JavaSignature') then
      begin
        LArguments := LAttribute.FindNode(TSyntaxNodeType.ntArguments);
        if LArguments <> nil then
        begin
          for LArgument in LArguments.ChildNodes do
          begin
            LLiteral := LArgument.FindNode([TSyntaxNodeType.ntValue, TSyntaxNodeType.ntExpression, TSyntaxNodeType.ntLiteral]);
            if (LLiteral <> nil) and (LLiteral is TValuedSyntaxNode) then
            begin
              ASignature := TValuedSyntaxNode(LLiteral).Value;
              Result := True;
              Break;
            end;
          end;
        end;
      end;
      if Result then
        Break;
    end;
  end;
end;

function TSyntaxNodeHelper.FindDeclaration(const AName: string; out ANode: TSyntaxNode; const AIsForward: Boolean = False): Boolean;
var
  LRoot: TSyntaxNode;
begin
  LRoot := FindRoot;
  Result := FindDeclaration(AName, LRoot.FindNode(TSyntaxNodeType.ntInterface), ANode, AIsForward);
  if not Result and not AIsForward then
    Result := FindDeclaration(AName, LRoot.FindNode(TSyntaxNodeType.ntImplementation), ANode, AIsForward);
end;

function TSyntaxNodeHelper.FindRoot: TSyntaxNode;
begin
  Result := Self;
  while Result.ParentNode <> nil do
    Result := Result.ParentNode;
end;

procedure TSyntaxNodeHelper.ForEach(const AParentNodeType, ANodeType: TSyntaxNodeType; const AHandler: TSyntaxNodeProc);
var
  LParent: TSyntaxNode;
begin
  LParent := FindNode(AParentNodeType);
  if LParent <> nil then
    LParent.ForEach(ANodeType, AHandler);
end;

procedure TSyntaxNodeHelper.ForEach(const ANodeType: TSyntaxNodeType; const AHandler: TSyntaxNodeProc);
var
  LNode, LChild: TSyntaxNode;
begin
  for LNode in ChildNodes do
  begin
    if LNode.Typ = ANodeType then
    begin
      for LChild in LNode.ChildNodes do
        AHandler(LChild);
    end;
  end;
end;

function TSyntaxNodeHelper.IsClass: Boolean;
begin
  Result := GetAttribute(TAttributeName.anType).Equals('class');
end;

function TSyntaxNodeHelper.IsInterface: Boolean;
begin
  Result := GetAttribute(TAttributeName.anType).Equals('interface');
end;

function TSyntaxNodeHelper.IsForward: Boolean;
begin
  Result := GetAttribute(TAttributeName.anForwarded).Equals('true');
end;

procedure TSyntaxNodeHelper.InternalGetDependentTypeNames(var ANames: TArray<string>);
var
  LBaseNode, LNode: TSyntaxNode;
  LName: string;
begin
  if Typ = TSyntaxNodeType.ntTypeDecl then
    LBaseNode := FindNode(TSyntaxNodeType.ntType)
  else
    LBaseNode := Self;
  if LBaseNode <> nil then
  begin
    for LNode in LBaseNode.ChildNodes do
    begin
      case LNode.Typ of
        TSyntaxNodeType.ntType:
        begin
          if LNode.ParentNode.Typ = TSyntaxNodeType.ntType then
          begin
            LName := LNode.GetAttribute(TAttributeName.anName);
            if IndexStr(LName, ANames) = -1 then
              ANames := ANames + [LName];
          end;
        end;
        TSyntaxNodeType.ntMethod, ntParameters:
          LNode.InternalGetDependentTypeNames(ANames);
        TSyntaxNodeType.ntParameter, TSyntaxNodeType.ntReturnType:
        begin
          LName := LNode.FindNode(TSyntaxNodeType.ntType).GetAttribute(TAttributeName.anName);
          if IndexStr(LName, ANames) = -1 then
            ANames := ANames + [LName];
        end;
        // TODO: Properties etc
      end;
    end;
  end;
end;

function TSyntaxNodeHelper.GetDependentTypeNames: TArray<string>;
begin
  InternalGetDependentTypeNames(Result);
end;

procedure TSyntaxNodeHelper.GetDependentTypeNames(var ANames: TArray<string>);
begin
  InternalGetDependentTypeNames(ANames);
end;

function TSyntaxNodeHelper.GetName: string;
begin
  Result := GetAttribute(TAttributeName.anName);
end;

function TSyntaxNodeHelper.GetType: string;
var
  LTypeNode: TSyntaxNode;
begin
  LTypeNode := FindNode(TSyntaxNodeType.ntType);
  Result := LTypeNode.GetAttribute(TAttributeName.anType);
  LTypeNode := LTypeNode.FindNode(TSyntaxNodeType.ntType);
  if LTypeNode <> nil then
    Result := Format('%s(%s)', [Result, LTypeNode.GetAttribute(TAttributeName.anName)]);
end;

function TSyntaxNodeHelper.GetParentType: string;
var
  LTypeNode: TSyntaxNode;
begin
  LTypeNode := FindNode(TSyntaxNodeType.ntType);
  LTypeNode := LTypeNode.FindNode(TSyntaxNodeType.ntType);
  if LTypeNode <> nil then
    Result := LTypeNode.GetAttribute(TAttributeName.anName)
  else
    Result := '';
end;

{ TDelphiWriter }

function TDelphiWriter.GetTypeArgs(const ANode: TSyntaxNode): string;
var
  LArgs: TArray<string>;
  LType: TSyntaxNode;
begin
  for LType in ANode.ChildNodes do
    LArgs := LArgs + [LType.GetAttribute(TAttributeName.anName)];
  Result := string.Join(',', LArgs);
  if not Result.IsEmpty then
    Result := '<' + Result + '>';
end;

procedure TDelphiWriter.Indent;
begin
  Inc(FLevel);
end;

procedure TDelphiWriter.Outdent;
begin
  if FLevel > 0 then
    Dec(FLevel);
end;

function TDelphiWriter.Spaces: string;
begin
  Result := StringOfChar(' ', FLevel * 2);
end;

procedure TDelphiWriter.WriteAttribute(const ANode: TSyntaxNode);
var
  LPositionalArgument, LLiteral: TSyntaxNode;
  LName: TValuedSyntaxNode;
begin
  // Not concerned with other types of attributes yet
  LName := TValuedSyntaxNode(ANode.FindNode(TSyntaxNodeType.ntName));
  Write(Spaces + Format('[%s(', [LName.Value]));
  for LPositionalArgument in ANode.FindNode(TSyntaxNodeType.ntArguments).ChildNodes do
  begin
    LLiteral := LPositionalArgument.FindNode(TSyntaxNodeType.ntValue)
      .FindNode(TSyntaxNodeType.ntExpression).FindNode(TSyntaxNodeType.ntLiteral);
    Write(Format('''%s''', [TValuedSyntaxNode(LLiteral).Value]));
  end;
  WriteLine(')]');
end;

procedure TDelphiWriter.WriteAttributes(const ANode: TSyntaxNode);
var
  LAttribute: TSyntaxNode;
begin
  for LAttribute in ANode.ChildNodes do
    WriteAttribute(LAttribute);
end;

procedure TDelphiWriter.WriteGUID(const ANode: TSyntaxNode);
var
  LNode: TSyntaxNode;
begin
  LNode := ANode.FindNode(TSyntaxNodeType.ntLiteral);
  WriteLine(Spaces + Format('[''%s'']', [TValuedSyntaxNode(LNode).Value]));
end;

procedure TDelphiWriter.WriteMethod(const ANode: TSyntaxNode; const AIsPseudoClass: Boolean = False);
var
  LParams, LReturnType: TSyntaxNode;
  I: Integer;
  LAttribute, LClassComment: string;
begin
  if AIsPseudoClass then
    LClassComment := '{class} '
  else
    LClassComment := '';
  Write(Spaces + Format('%s%s %s', [LClassComment, ANode.GetAttribute(TAttributeName.anKind), ANode.GetAttribute(TAttributeName.anName)]));
  LParams := ANode.FindNode(TSyntaxNodeType.ntParameters);
  if LParams <> nil then
  begin
    Write('(');
    for I := 0 to LParams.Count - 1 do
    begin
      if I > 0 then
        Write('; ');
      WriteParameter(LParams.ChildNodes[I]);
    end;
    Write(')');
  end;
  LReturnType := ANode.FindNode(TSyntaxNodeType.ntReturnType);
  if LReturnType <> nil then
    WriteReturnType(LReturnType);
  LAttribute := ANode.GetAttribute(TAttributeName.anCallingConvention);
  if not LAttribute.IsEmpty then
    Write('; ' + LAttribute);
  LAttribute := ANode.GetAttribute(TAttributeName.anOverload);
  if LAttribute.Equals('true') then
    Write('; overload');
  WriteLine(';');
end;

procedure TDelphiWriter.WriteParameter(const ANode: TSyntaxNode);
var
  LName, LType, LTypeArgs: TSyntaxNode;
  LArgs: string;
begin
  LName := ANode.FindNode(TSyntaxNodeType.ntName);
  LType := ANode.FindNode(TSyntaxNodeType.ntType);
  LTypeArgs := LType.FindNode(TSyntaxNodeType.ntTypeArgs);
  if LTypeArgs <> nil then
    LArgs := GetTypeArgs(LTypeArgs)
  else
    LArgs := '';
  //TODO: Need to cater for modifiers e.g. const, var, out, and typeless
  Write(Format('%s: %s%s', [TValuedSyntaxNode(LName).Value, LType.GetAttribute(TAttributeName.anName), LArgs]));
end;

procedure TDelphiWriter.WriteReturnType(const ANode: TSyntaxNode);
var
  LType, LTypeArgs: TSyntaxNode;
  LArgs: string;
begin
  LType := ANode.FindNode(TSyntaxNodeType.ntType);
  LTypeArgs := LType.FindNode(TSyntaxNodeType.ntTypeArgs);
  if LTypeArgs <> nil then
    LArgs := GetTypeArgs(LTypeArgs)
  else
    LArgs := '';
  Write(Format(': %s%s', [LType.GetAttribute(TAttributeName.anName), LArgs]));
end;

procedure TDelphiWriter.WriteType(const ANode: TSyntaxNode; const AIsPseudoClass: Boolean = False);
var
  LTypeNode, LNode: TSyntaxNode;
begin
  WriteLine(Spaces + Format('%s = %s', [ANode.GetName, ANode.GetType]));
  Indent;
  LTypeNode := ANode.FindNode(TSyntaxNodeType.ntType);
  if LTypeNode <> nil then
  begin
    for LNode in LTypeNode.ChildNodes do
    begin
      case LNode.Typ of
        TSyntaxNodeType.ntGuid:
          WriteGUID(LNode);
        TSyntaxNodeType.ntMethod:
          WriteMethod(LNode, AIsPseudoClass);
        // TODO: Properties
      end;
    end;
  end;
  Outdent;
  WriteLine(Spaces + 'end;');
end;

end.
