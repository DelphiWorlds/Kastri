unit DW.Environment.BDS;

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
  System.Classes,
  DW.Environment;

type
  TBDSEnvironment = class(TEnvironment)
  protected
    function GetConfigRoot: string; override;
    function GetKey: TEnvironmentKey; override;
    procedure RootKeyNamesLoaded; override;
  public
    procedure CloseKey;
    function GetAndroidSDKPath(const AVersion: string): string;
    function GetAndroidLibPath(const AVersion: string): string;
    function GetBDSCommonDir(const AVersion: string): string; overload;
    function GetBDSEXEPath(const AIndex: Integer): string; overload;
    function GetBDSPath(const AVersion: string): string; overload;
    function GetBDSPath(const AIndex: Integer): string; overload;
    procedure GetBDSValues(const AIndex: Integer; const APath: string; const AValues: TStrings);
    function GetDefaultSDKName(const AVersion, APlatform: string): string;
    function GetDefaultSDKValue(const AVersion, APlatform, AKey: string): string;
    procedure GetSDKNames(const AVersion: string; const ASDKNames: TStrings);
    procedure GetSearchPaths(const AVersion, APlatform: string; const AVars: TStrings);
    procedure GetVariables(const AVersion: string; const AVars: TStrings);
    function GetVersionName(const AIndex: Integer): string; overload;
    function GetVersionName(const AVersion: string): string; overload;
    function GetVersion(const AIndex: Integer): string;
    function OpenKey(const AIndex: Integer; const APath: string; const ACanCreate: Boolean = False): Boolean;
    property ConfigRoot: string read GetConfigRoot;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

const
  cKnownVersions: array[0..8] of string = (
    '15.0', '16.0', '17.0', '18.0', '19.0', '20.0', '21.0', '22.0', '23.0'
  );
  cKnownVersionNames: array[0..8] of string = (
    'Delphi XE7', 'Delphi XE8', 'Delphi 10 Seattle', 'Delphi 10.1 Berlin', 'Delphi 10.2 Tokyo', 'Delphi 10.3 Rio',
    'Delphi 10.4 Sydney', 'Delphi 11 Alexandria', 'Delphi 12 Athens'
  );

{ TBDSEnvironment }

function TBDSEnvironment.GetConfigRoot: string;
begin
  Result := 'SOFTWARE\Embarcadero\BDS';
end;

function TBDSEnvironment.GetKey: TEnvironmentKey;
begin
  Result := TEnvironmentKey.User;
end;

function TBDSEnvironment.GetVersionName(const AIndex: Integer): string;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < RootKeyNames.Count) then
    Result := GetVersionName(RootKeyNames[AIndex]);
end;

procedure TBDSEnvironment.GetVariables(const AVersion: string; const AVars: TStrings);
begin
  GetValues(AVersion + '\Environment Variables', AVars);
  AVars.Values['DEMOSDIR'] := '';
  AVars.Values['IBREDISTDIR'] := '';
  AVars.Values['BDSCOMMONDIR'] := ''; // Special for those have had their BDS install screw up like mine did
  AVars.Values['Path'] := ''; // Ignore overridden path
end;

procedure TBDSEnvironment.GetSDKNames(const AVersion: string; const ASDKNames: TStrings);
begin
  GetValues(AVersion + '\PlatformSDKs', ASDKNames);
end;

procedure TBDSEnvironment.GetSearchPaths(const AVersion, APlatform: string; const AVars: TStrings);
var
  LPath: string;
begin
  AVars.Clear;
  LPath := GetValue('SearchPath', AVersion + '\Library\' + APlatform);
  for LPath in LPath.Split([';']) do
    AVars.Add(LPath);
end;

function TBDSEnvironment.GetDefaultSDKName(const AVersion, APlatform: string): string;
begin
  Result := GetValue('Default_' + APlatform, AVersion + '\PlatformSDKs');
end;

function TBDSEnvironment.GetDefaultSDKValue(const AVersion, APlatform, AKey: string): string;
var
  LDefaultSDKName: string;
begin
  LDefaultSDKName := GetDefaultSDKName(AVersion, APlatform);
  Result := GetValue(AKey, AVersion + '\PlatformSDKs\' + LDefaultSDKName);
end;

procedure TBDSEnvironment.GetBDSValues(const AIndex: Integer; const APath: string; const AValues: TStrings);
begin
  AValues.Clear;
  GetValues(GetVersion(AIndex) + '\' + APath, AValues);
end;

function TBDSEnvironment.GetVersion(const AIndex: Integer): string;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < RootKeyNames.Count) then
    Result := RootKeyNames[AIndex];
end;

function TBDSEnvironment.GetVersionName(const AVersion: string): string;
var
  I: Integer;
begin
  Result := Format('BDS %s (Unknown Delphi version name)', [AVersion]);
  for I := Low(cKnownVersions) to High(cKnownVersions) do
  begin
    if cKnownVersions[I].Equals(AVersion) then
      Result := cKnownVersionNames[I];
  end;
end;

function TBDSEnvironment.OpenKey(const AIndex: Integer; const APath: string; const ACanCreate: Boolean = False): Boolean;
begin
  Result := Registry.OpenKey(GetConfigRoot + '\' + GetVersion(AIndex) + '\' + APath, ACanCreate);
end;

procedure TBDSEnvironment.RootKeyNamesLoaded;
var
  I: Integer;
begin
  for I := RootKeyNames.Count - 1 downto 0 do
  begin
    if not TFile.Exists(GetValue('App', RootKeyNames[I])) then
      RootKeyNames.Delete(I);
  end;
end;

procedure TBDSEnvironment.CloseKey;
begin
  Registry.CloseKey;
end;

function TBDSEnvironment.GetAndroidLibPath(const AVersion: string): string;
begin
  Result := TPath.Combine(GetBDSPath(AVersion), 'lib\android\release');
end;

function TBDSEnvironment.GetAndroidSDKPath(const AVersion: string): string;
var
  LDefaultAndroid: string;
begin
  Result := '';
  LDefaultAndroid := GetValue('Default_Android', AVersion + '\PlatformSDKs');
  if not LDefaultAndroid.IsEmpty then
    Result := GetValue('SystemRoot', AVersion + '\PlatformSDKs\' + LDefaultAndroid);
end;

function TBDSEnvironment.GetBDSPath(const AVersion: string): string;
begin
  Result := GetValue('RootDir', AVersion);
end;

function TBDSEnvironment.GetBDSCommonDir(const AVersion: string): string;
begin
  Result := GetValue('BDSCOMMONDIR', AVersion + '\Environment Variables');
end;

function TBDSEnvironment.GetBDSEXEPath(const AIndex: Integer): string;
begin
  Result := GetValue('App', GetVersion(AIndex));
end;

function TBDSEnvironment.GetBDSPath(const AIndex: Integer): string;
begin
  Result := GetValue('RootDir', GetVersion(AIndex));
end;

end.
