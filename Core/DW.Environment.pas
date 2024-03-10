unit DW.Environment;

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
  System.Classes, System.Win.Registry;

type
  TEnvironmentKey = (User, Machine);

  TEnvironment = class(TObject)
  private
    FRegistry: TRegistry;
    FRootKeyNames: TStrings;
    procedure LoadRootKeyNames;
    function GetRootKeyCount: Integer;
  protected
    function GetConfigRoot: string; virtual; abstract;
    function GetKey: TEnvironmentKey; virtual; abstract;
    function GetValue(const AValueKey: string; const ARelativePath: string = ''): string;
    procedure GetValues(const AValuesPath: string; const AValues: TStrings; const ARelativePath: string = '');
    procedure RootKeyNamesLoaded; virtual;
  public
    constructor Create(const AWriteAccess: Boolean = False); virtual;
    destructor Destroy; override;
    procedure GetKeys(const AKeys: TStrings; const ARelativePath: string = '');
    procedure Refresh;
    property Registry: TRegistry read FRegistry;
    property RootKeyCount: Integer read GetRootKeyCount;
    property RootKeyNames: TStrings read FRootKeyNames;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

{ TEnvironment }

constructor TEnvironment.Create(const AWriteAccess: Boolean = False);
var
  LAccess: Cardinal;
begin
  inherited Create;
  FRootKeyNames := TStringList.Create;
  LAccess := KEY_READ;
  if AWriteAccess then
    LAccess := LAccess or KEY_WRITE;
  if TOSVersion.Architecture = TOSVersion.TArchitecture.arIntelX64 then
    LAccess := LAccess or KEY_WOW64_64KEY
  else
    LAccess := LAccess or KEY_WOW64_32KEY;
  FRegistry := TRegistry.Create(LAccess);
  case GetKey of
    TEnvironmentKey.Machine:
      FRegistry.RootKey := HKEY_LOCAL_MACHINE;
    TEnvironmentKey.User:
      FRegistry.RootKey := HKEY_CURRENT_USER;
  end;
  LoadRootKeyNames;
end;

destructor TEnvironment.Destroy;
begin
  FRootKeyNames.Free;
  FRegistry.Free;
  inherited;
end;

procedure TEnvironment.GetKeys(const AKeys: TStrings; const ARelativePath: string = '');
var
  LPath: string;
begin
  AKeys.Clear;
  LPath := GetConfigRoot;
  if ARelativePath <> '' then
    LPath := LPath + '\' + ARelativePath;
  if FRegistry.OpenKey(LPath, False) then
  try
    FRegistry.GetKeyNames(AKeys);
  finally
    FRegistry.CloseKey;
  end;
end;

function TEnvironment.GetRootKeyCount: Integer;
begin
  Result := FRootKeyNames.Count;
end;

function TEnvironment.GetValue(const AValueKey: string; const ARelativePath: string = ''): string;
var
  LPath: string;
begin
  Result := '';
  LPath := GetConfigRoot;
  if ARelativePath <> '' then
    LPath := LPath + '\' + ARelativePath;
  if FRegistry.OpenKey(LPath, False) then
  try
    Result := FRegistry.ReadString(AValueKey);
  finally
    FRegistry.CloseKey;
  end;
end;

procedure TEnvironment.GetValues(const AValuesPath: string; const AValues: TStrings; const ARelativePath: string);
var
  LPath: string;
  LNames: TStrings;
  I: Integer;
begin
  LPath := GetConfigRoot;
  if ARelativePath <> '' then
    LPath := LPath + '\' + ARelativePath;
  if FRegistry.OpenKey(LPath + '\' + AValuesPath, False) then
  try
    LNames := TStringList.Create;
    try
      FRegistry.GetValueNames(LNames);
      for I := 0 to LNames.Count - 1 do
        AValues.Values[LNames[I]] := FRegistry.ReadString(LNames[I]);
    finally
      LNames.Free;
    end;
  finally
    FRegistry.CloseKey;
  end;
end;

procedure TEnvironment.LoadRootKeyNames;
begin
  FRootKeyNames.Clear;
  if FRegistry.OpenKey(GetConfigRoot, False) then
  try
    FRegistry.GetKeyNames(FRootKeyNames);
  finally
    FRegistry.CloseKey;
  end;
  RootKeyNamesLoaded;
end;

procedure TEnvironment.Refresh;
begin
  LoadRootKeyNames;
end;

procedure TEnvironment.RootKeyNamesLoaded;
begin
  // Override in descendants
end;

end.
