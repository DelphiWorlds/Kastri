unit DW.UnitScan;

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
  System.IOUtils,
  // DelphiAST
  DelphiAST.Classes;

type
  TUnitMap = record
    UnitFileName: string;
    UnitName: string;
    Symbols: TArray<string>;
    Error: string;
    procedure AddSymbol(const AName: string);
    function FindSignature(const ASignature: string; out AName: string): Boolean;
    function HasSymbol(const AName: string): Boolean;
    function IsEmpty: Boolean;
  end;

  TUnitMaps = TArray<TUnitMap>;

  // TSymbolKind = (Consts, Types, Subtypes, TypeMethods);

  // TSymbolKinds = set of TSymbolKind;

  TSymbolScannerOptions = record
  public
    class function AndroidOnly: TSymbolScannerOptions; static;
  public
    // UnitPrefixes: TArray<string>; // e.g. Androidapi.
    // Kinds: TSymbolKinds; // e.g. [Types] when creating symbol maps purely for Android "boot" classes
    IsAndroidOnly: Boolean;
  end;

  TSymbolUnitMaps = record
    Items: TUnitMaps;
    procedure AddItem(const AItem: TUnitMap);
    function Count: Integer;
    function FindSymbol(const ASignature: string; out ASymbol: string): Boolean;
    function FindUnits(const ASymbolName: string): TArray<string>;
  end;

  TUnitSymbolScanner = record
  private
    class procedure ScanClass(const AClassName: string; const ANode: TSyntaxNode; var AMap: TUnitMap); static;
  public
    class function Scan(const AUnitFileName: string): TUnitMap; overload; static;
    class function Scan(const AUnitFileName: string; const AOptions: TSymbolScannerOptions): TUnitMap; overload; static;
    class procedure ScanFolder(const AFolder: string; var AMaps: TSymbolUnitMaps;
      const ASearchOption: TSearchOption = TSearchOption.soTopDirectoryOnly); overload; static;
    class procedure ScanFolder(const AFolder: string; var AMaps: TSymbolUnitMaps; const AOptions: TSymbolScannerOptions;
      const ASearchOption: TSearchOption = TSearchOption.soTopDirectoryOnly); overload; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.StrUtils,
  // DelphiAST
  DelphiAST.Consts,
  // DW
  DW.DelphiAST;

{ TUnitMap }

procedure TUnitMap.AddSymbol(const AName: string);
begin
  Symbols := Symbols + [AName];
end;

function TUnitMap.FindSignature(const ASignature: string; out AName: string): Boolean;
var
  I: Integer;
  LParts: TArray<string>;
begin
  Result := False;
  for I := 0 to Length(Symbols) - 1 do
  begin
    LParts := Symbols[I].Split(['|']);
    if (Length(LParts) > 1) and LParts[0].Equals(ASignature) then
    begin
      AName := LParts[1];
      Result := True;
    end;
  end;
end;

function TUnitMap.HasSymbol(const AName: string): Boolean;
begin
  Result := IndexStr(AName, Symbols) > -1;
end;

function TUnitMap.IsEmpty: Boolean;
begin
  Result := Length(Symbols) = 0;
end;

{ TSymbolUnitMaps }

procedure TSymbolUnitMaps.AddItem(const AItem: TUnitMap);
begin
  Items := Items + [AItem];
end;

function TSymbolUnitMaps.Count: Integer;
begin
  Result := Length(Items);
end;

function TSymbolUnitMaps.FindSymbol(const ASignature: string; out ASymbol: string): Boolean;
var
  LMap: TUnitMap;
  LName: string;
begin
  Result := False;
  for LMap in Items do
  begin
    if LMap.FindSignature(ASignature, LName) then
    begin
      ASymbol := LName + '|' + LMap.UnitName;
      Result := True;
      Break;
    end;
  end;
end;

function TSymbolUnitMaps.FindUnits(const ASymbolName: string): TArray<string>;
var
  LMap: TUnitMap;
  LIsJavaSignature: Boolean;
  LName, LValue: string;
begin
  Result := [];
  LIsJavaSignature := ASymbolName.Contains('/');
  for LMap in Items do
  begin
    if not LIsJavaSignature then
    begin
      if LMap.HasSymbol(ASymbolName) and not MatchStr(LMap.UnitName, Result) then
        Result := Result + [LMap.UnitName];
    end
    else
    begin
      if LMap.FindSignature(ASymbolName, LName) then
      begin
        LValue := LName + '|' + LMap.UnitName;
        if not MatchStr(LMap.UnitName, Result) then
          Result := Result + [LValue];
      end;
    end;
  end;
end;

{ TUnitSymbolScanner }

class procedure TUnitSymbolScanner.ScanClass(const AClassName: string; const ANode: TSyntaxNode; var AMap: TUnitMap);
var
  LPublic: TSyntaxNode;
  LMap: TUnitMap;
begin
  LMap := AMap;
  LPublic := ANode.FindNode(TSyntaxNodeType.ntPublic);
  if LPublic <> nil then
  begin
    LPublic.ForEach(TSyntaxNodeType.ntTypeSection,
      procedure(const ANode: TSyntaxNode)
      begin
        LMap.AddSymbol(AClassName + '.' + ANode.GetAttribute(TAttributeName.anName));
      end
    );
  end;
  AMap := LMap;
end;

class function TUnitSymbolScanner.Scan(const AUnitFileName: string): TUnitMap;
var
  LOptions: TSymbolScannerOptions;
begin
  LOptions.IsAndroidOnly := False;
  Result := Scan(AUnitFileName, LOptions);
end;

class function TUnitSymbolScanner.Scan(const AUnitFileName: string; const AOptions: TSymbolScannerOptions): TUnitMap;
var
  LUnitNode: TSyntaxNode;
  LMap: TUnitMap;
  LType: TSyntaxNode;
begin
  LMap.UnitFileName := AUnitFileName;
  LMap.UnitName := TPath.GetFileNameWithoutExtension(TPath.GetFileName(AUnitFileName));
  try
    LUnitNode := TSilentPasSyntaxTreeBuilder.RunFile(AUnitFileName);
  except
    on E: Exception do
    begin
      LMap.Error := Format('%s - %s: %s', [AUnitFileName, E.ClassName, E.Message]);
      LUnitNode := nil;
    end;
  end;
  if LUnitNode <> nil then
  try
    LUnitNode.ForEach(TSyntaxNodeType.ntInterface, TSyntaxNodeType.ntTypeSection,
      procedure(const ANode: TSyntaxNode)
      var
        LTypeName, LJavaSignature: string;
      begin
        if not ANode.IsForward and (ANode.Typ =TSyntaxNodeType.ntTypeDecl) then
        begin
          LTypeName := ANode.GetAttribute(TAttributeName.anName);
          LType := ANode.FindNode(TSyntaxNodeType.ntType);
          if AOptions.IsAndroidOnly then
          begin
            if (LType <> nil) and LType.IsInterface and ANode.FindJavaSignature(LJavaSignature) then
            begin
              LTypeName := LJavaSignature + '|' + LTypeName;
              LMap.AddSymbol(LTypeName);
            end;
          end
          else
          begin
            LMap.AddSymbol(LTypeName);
            if (LType <> nil) and LType.IsClass then
              ScanClass(LTypeName, LType, LMap);
          end;
        end;
      end
    );
    // Omit constants if this is indexing for Android
    if not AOptions.IsAndroidOnly then
    begin
      LUnitNode.ForEach(TSyntaxNodeType.ntInterface, TSyntaxNodeType.ntConstants,
        procedure(const ANode: TSyntaxNode)
        var
          LNameNode: TValuedSyntaxNode;
        begin
          LNameNode := TValuedSyntaxNode(ANode.FindNode(TSyntaxNodeType.ntName));
          LMap.AddSymbol(LNameNode.Value);
        end
      );
    end;
  finally
    LUnitNode.Free;
  end;
  Result := LMap;
end;

class procedure TUnitSymbolScanner.ScanFolder(const AFolder: string; var AMaps: TSymbolUnitMaps;
  const ASearchOption: TSearchOption = TSearchOption.soTopDirectoryOnly);
var
  LOptions: TSymbolScannerOptions;
begin
  LOptions.IsAndroidOnly := False;
  ScanFolder(AFolder, AMaps, LOptions, ASearchOption);
end;

class procedure TUnitSymbolScanner.ScanFolder(const AFolder: string; var AMaps: TSymbolUnitMaps; const AOptions: TSymbolScannerOptions;
  const ASearchOption: TSearchOption = TSearchOption.soTopDirectoryOnly);
var
  LFileName: string;
  LMap: TUnitMap;
begin
  if not AFolder.IsEmpty and TDirectory.Exists(AFolder) then
  begin
    for LFileName in TDirectory.GetFiles(AFolder, '*.pas', ASearchOption) do
    begin
      LMap := Scan(LFileName, AOptions);
      if not LMap.IsEmpty then
        AMaps.AddItem(LMap);
    end;
  end;
end;

{ TSymbolScannerOptions }

class function TSymbolScannerOptions.AndroidOnly: TSymbolScannerOptions;
begin
  Result.IsAndroidOnly := True;
end;

end.
