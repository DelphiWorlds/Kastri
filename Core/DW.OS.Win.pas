unit DW.OS.Win;

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
  // RTL
  System.Classes, System.Types;

type
  TPlatformOS = record
  public
    class function GetEnvironmentVariable(const AName: string): string; static;
    class procedure SetEnvironmentVariable(const AName, AValue: string); static;
    class function SetTokenPrivilege(const APrivilege: string; const AEnable: Boolean): Boolean; static;
    class procedure UpdatePathVariable(const AIncludePath: string); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Windows
  Winapi.Windows;

function StringsToMultiPChar(var ADest: PChar; const ASource: TStrings): PChar;
var
  I, LLength: Integer;
  LMarker: PChar;
begin
  LLength := 1;
  for I := 0 to ASource.Count - 1 do
  begin
    if not ASource[I].IsEmpty then
      Inc(LLength, StrLen(PChar(ASource[I])) + 1);
  end;
  GetMem(ADest, LLength * SizeOf(Char));
  LMarker := ADest;
  for I := 0 to ASource.Count - 1 do
  begin
    LMarker := StrECopy(LMarker, PChar(ASource[I]));
    Inc(LMarker);
  end;
  LMarker^ := #0;
  Result := ADest;
end;

function Win32Check(RetVal: BOOL): BOOL;
begin
  if not RetVal then
    RaiseLastOSError;
  Result := RetVal;
end;

{ TPlatformOS }

class function TPlatformOS.GetEnvironmentVariable(const AName: string): string;
const
  cBufSize = 1024;
var
  LLen: Integer;
  LBuffer: array[0..cBufSize - 1] of Char;
begin
  Result := '';
  LLen := Winapi.Windows.GetEnvironmentVariable(PChar(AName), @LBuffer, cBufSize);
  if LLen < cBufSize then
    SetString(Result, PChar(@LBuffer), LLen)
  else
  begin
    SetLength(Result, LLen - 1);
    Winapi.Windows.GetEnvironmentVariable(PChar(AName), PChar(Result), LLen);
  end;
end;

class procedure TPlatformOS.SetEnvironmentVariable(const AName, AValue: string);
begin
  Winapi.Windows.SetEnvironmentVariable(PChar(AName), PChar(AValue));
end;

class function TPlatformOS.SetTokenPrivilege(const APrivilege: string; const AEnable: Boolean): Boolean;
var
  LToken: THandle;
  LTokenPriv: TOKEN_PRIVILEGES;
  LPrevTokenPriv: TOKEN_PRIVILEGES;
  LLength: Cardinal;
begin
  Result := False;
  if OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, LToken) then
  try
    // Get the locally unique identifier (LUID).
    if LookupPrivilegeValue(nil, PChar(APrivilege), LTokenPriv.Privileges[0].Luid) then
    begin
      LTokenPriv.PrivilegeCount := 1; // one privilege to set
      case AEnable of
        True: LTokenPriv.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        False: LTokenPriv.Privileges[0].Attributes := 0;
      end;
      LPrevTokenPriv := LTokenPriv;
      // Enable or disable the privilege
      Result := AdjustTokenPrivileges(LToken, False, LTokenPriv, SizeOf(LPrevTokenPriv), LPrevTokenPriv, LLength);
    end;
  finally
    CloseHandle(LToken);
  end;
end;

class procedure TPlatformOS.UpdatePathVariable(const AIncludePath: string);
var
  LPath: string;
begin
  LPath := GetEnvironmentVariable('PATH');
  if not LPath.ToUpper.Contains(AIncludePath.ToUpper) then
    SetEnvironmentVariable('PATH', AIncludePath + ';%PATH%');
end;

end.



