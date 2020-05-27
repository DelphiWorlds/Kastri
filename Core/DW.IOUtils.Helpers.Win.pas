unit DW.IOUtils.Helpers.Win;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

type
  TPlatformPath = record
    class function GetSharedDocumentsPath: string; static;
  end;

  TPlatformFile = record
    class function GetFileSize(const AFileName: string): Int64; static;
    class function IsInUse(const AFileName: string): Boolean; static;
  end;

  TPlatformDirectory = record
    class function Delete(const ADirectory: string): Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  // Win
  Winapi.Windows, Winapi.SHFolder, Winapi.ShellAPI;

{ TPlatformPath }

class function TPlatformPath.GetSharedDocumentsPath: string;
var
  LPath: array[0 .. MAX_PATH] of Char;
begin
  Result := '';
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_COMMON_DOCUMENTS, THandle(-1), 0, @LPath) = S_OK then
    Result := LPath;
end;

{ TPlatformFile }

class function TPlatformFile.GetFileSize(const AFileName: string): Int64;
var
  LAttributes: TWin32FileAttributeData;
begin
  if GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @LAttributes) then
  begin
    Int64Rec(Result).Lo := LAttributes.nFileSizeLow;
    Int64Rec(Result).Hi := LAttributes.nFileSizeHigh;
  end
  else
    raise EInOutError.Create(SysErrorMessage(GetLastError));
end;

class function TPlatformFile.IsInUse(const AFileName: string): Boolean;
var
  LHandle: HFILE;
begin
  LHandle := CreateFile(PChar(AFileName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := LHandle = INVALID_HANDLE_VALUE;
  if not Result then
    CloseHandle(LHandle);
end;

{ TPlatformDirectory }

class function TPlatformDirectory.Delete(const ADirectory: string): Boolean;
var
  LShFileOp: TSHFileOpStruct;
begin
  // Don't use this function in a service!!
  LShFileOp.Wnd := GetDesktopWindow;
  LShFileOp.wFunc := FO_DELETE;
  LShFileOp.pFrom := PChar(ADirectory + #0);
  LShFileOp.pTo := nil;
  LShFileOp.fFlags := FOF_NO_UI;
  Result := SHFileOperation(LShFileOp) = 0;
end;

end.
