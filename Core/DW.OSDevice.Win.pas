unit DW.OSDevice.Win;

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
  // DW
  DW.OSDevice, DW.FileVersionInfo.Win;

type
  TPlatformOSDevice = class(TObject)
  private
    class var FFileVersionInfo: TFileVersionInfo;
    class destructor DestroyClass;
    class function GetFileVersionInfo: TFileVersionInfo; static;
  public
    class function GetCurrentLocaleInfo: TLocaleInfo; static;
    class function GetDeviceName: string; static;
    class function GetPackageBuild: string; static;
    class function GetPackageID: string; static;
    class function GetPackageVersion: string; static;
    class function GetPackageName: string; static;
    class function GetUniqueDeviceID: string; static;
    class function GetUsername: string; static;
    class function IsBeta: Boolean; static;
    class function IsScreenLocked: Boolean; static;
    class function IsTouchDevice: Boolean; static;
    class procedure OpenURL(const AURL: string); static;
    class procedure ShowFilesInFolder(const AFileNames: array of string); static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Win.Registry, System.IOUtils,
  // Windows
  Winapi.Windows, Winapi.ShlObj, Winapi.ShellAPI;

const
  cMicrosoftCryptographyKey = 'SOFTWARE\Microsoft\Cryptography';
  cMachineGuidValueName = 'MachineGuid';

{ TPlatformOSDevice }

class destructor TPlatformOSDevice.DestroyClass;
begin
  FFileVersionInfo.Free;
end;

class function TPlatformOSDevice.GetCurrentLocaleInfo: TLocaleInfo;
var
  LBuffer: array[0..255] of Char;
  LLength: Integer;
begin
  LLength := GetLocaleInfo(GetUserDefaultLCID, LOCALE_SISO639LANGNAME, LBuffer, Length(LBuffer));
  if LLength > 0 then
  begin
    SetString(Result.LanguageCode, LBuffer, LLength - 1);
    Result.LanguageCode := Result.LanguageCode.Substring(0, 2);
  end
  else
    Result.LanguageCode := 'en';
  LLength := GetLocaleInfo(GetUserDefaultLCID, LOCALE_SLANGUAGE, LBuffer, Length(LBuffer));
  if LLength > 0 then
    SetString(Result.LanguageDisplayName, LBuffer, LLength - 1)
  else
    Result.LanguageDisplayName := '';
  LLength := GetLocaleInfo(GetUserDefaultLCID, LOCALE_SISO3166CTRYNAME, LBuffer, Length(LBuffer));
  if LLength > 0 then
    SetString(Result.CountryCode, LBuffer, LLength - 1)
  else
    Result.CountryCode := '';
  LLength := GetLocaleInfo(GetUserDefaultLCID, LOCALE_SCOUNTRY, LBuffer, Length(LBuffer));
  if LLength > 0 then
    SetString(Result.CountryDisplayName, LBuffer, LLength - 1)
  else
    Result.CountryDisplayName := '';
end;

class function TPlatformOSDevice.GetDeviceName: string;
var
  LComputerName: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  LSize: DWORD;
begin
  LSize := Length(LComputerName);
  if GetComputerName(LComputerName, LSize) then
    SetString(Result, LComputerName, LSize)
  else
    Result := '';
end;

class function TPlatformOSDevice.GetFileVersionInfo: TFileVersionInfo;
begin
  if FFileVersionInfo = nil then
    FFileVersionInfo := TFileVersionInfo.Create(GetModuleName(HInstance));
  Result := FFileVersionInfo;
end;

class function TPlatformOSDevice.GetPackageID: string;
begin
  Result := GetFileVersionInfo.InternalName;
end;

class function TPlatformOSDevice.GetPackageName: string;
begin
  Result := GetFileVersionInfo.ProductName;
end;

class function TPlatformOSDevice.GetPackageVersion: string;
var
  LVersion: TLongVersion;
begin
  LVersion := GetFileVersionInfo.FileLongVersion;
  Result := Format('%d.%d.%d', [LVersion.All[2], LVersion.All[1], LVersion.All[4]]);
end;

class function TPlatformOSDevice.GetPackageBuild: string;
begin
  Result := GetFileVersionInfo.FileLongVersion.All[3].ToString;
end;

class function TPlatformOSDevice.GetUniqueDeviceID: string;
var
  LRegistry: TRegistry;
  LAccess: Cardinal;
begin
  // **** BEWARE!!!! ****
  // VM's that are clones will have identical IDs.
  Result := '';
  LAccess := KEY_READ;
  if TOSVersion.Architecture = TOSVersion.TArchitecture.arIntelX86 then
    LAccess := LAccess or KEY_WOW64_32KEY
  else
    LAccess := LAccess or KEY_WOW64_64KEY;
  LRegistry := TRegistry.Create(LAccess);
  try
    LRegistry.RootKey := HKEY_LOCAL_MACHINE;
    if LRegistry.OpenKey(cMicrosoftCryptographyKey, False) then
      Result := LRegistry.ReadString(cMachineGuidValueName);
  finally
    LRegistry.Free;
  end;
end;

class function TPlatformOSDevice.GetUsername: string;
var
  LLen: DWord;
begin
  LLen := 1024;
  SetLength(Result, LLen);
  if Winapi.Windows.GetUserName(PChar(Result), LLen) then
  begin
    SetLength(Result, LLen - 1)
  end
  else
    Result := '';
end;

class function TPlatformOSDevice.IsBeta: Boolean;
begin
  Result := TFileFlag.PreRelease in GetFileVersionInfo.FileFlags;
end;

class function TPlatformOSDevice.IsScreenLocked: Boolean;
begin
  Result := False; // To be implemented
end;

class function TPlatformOSDevice.IsTouchDevice: Boolean;
var
  LValue: Integer;
begin
  LValue := GetSystemMetrics(SM_DIGITIZER);
  Result := ((LValue and NID_READY) = NID_READY) and (((LValue and NID_MULTI_INPUT) = NID_MULTI_INPUT));
end;

class procedure TPlatformOSDevice.OpenURL(const AURL: string);
begin
  ShellExecute(0, 'open', PChar(AURL), nil, nil, SW_SHOWNORMAL);
end;

class procedure TPlatformOSDevice.ShowFilesInFolder(const AFileNames: array of string);
var
  LPIDLList: array of PItemIDList;
  LBasePIDL: PItemIDList;
  I: Integer;
  LFolder: string;
begin
  LFolder := '';
  for I := Low(AFileNames) to High(AFileNames) do
  begin
    if LFolder.IsEmpty then
      LFolder := TPath.GetDirectoryName(AFileNames[I]);
    if LFolder.Equals(TPath.GetDirectoryName(AFileNames[I])) then
    begin
      SetLength(LPIDLList, Length(LPIDLList) + 1);
      LPIDLList[Length(LPIDLList) - 1] := ILCreateFromPath(PChar(AFileNames[I]));
    end;
  end;
  if not LFolder.IsEmpty and (Length(LPIDLList) > 0) then
  begin
    LBasePIDL := ILCreateFromPath(PChar(LFolder));
    try
      SHOpenFolderAndSelectItems(LBasePIDL, Length(LPIDLList), LPIDLList[0], 0);
    finally
      ILFree(LBasePIDL);
      for I := 0 to Length(LPIDLList) - 1 do
        ILFree(LPIDLList[I]);
    end;
  end;
end;

end.



