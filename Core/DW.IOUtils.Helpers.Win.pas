unit DW.IOUtils.Helpers.Win;

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

type
  TPlatformPath = record
  public
    class function CopyFiles(const AFiles: TArray<string>; const ADestination: string; const AConfirm: Boolean = True): Boolean; static;
    class function GetSharedDocumentsPath: string; static;
  end;

  TPlatformFile = record
  public
    class function GetFileSize(const AFileName: string): Int64; static;
    class function IsInUse(const AFileName: string): Boolean; static;
  end;

  TPlatformDirectory = record
  public
    class function Delete(const ADirectory: string; const ACanRecycle: Boolean): Boolean; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  // Win
  Winapi.Windows, Winapi.SHFolder, Winapi.ShellAPI, Winapi.ActiveX, Winapi.ShlObj;

{ TPlatformPath }

class function TPlatformPath.CopyFiles(const AFiles: TArray<string>; const ADestination: string; const AConfirm: Boolean = True): Boolean;
var
  LFileOp: IFileOperation;
  LSrcFileItem: IShellItem;
  LDestFolderItem: IShellItem;
  LFileName: string;
  LOpFlags: DWORD;
begin
  Result := False;
  if Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE)) then
  begin
    LOpFlags := FOFX_NOMINIMIZEBOX;
    if not AConfirm then
      LOpFlags := LOpFlags or FOF_NOCONFIRMATION;
    if Succeeded(CoCreateInstance(CLSID_FileOperation, nil, CLSCTX_ALL, IFileOperation, LFileOp)) and
      Succeeded(LFileOp.SetOperationFlags(LOpFlags)) then
    begin
      if ForceDirectories(ADestination) and Succeeded(SHCreateItemFromParsingName(PChar(ADestination), nil, IShellItem, LDestFolderItem)) then
      begin
        for LFileName in AFiles do
        begin
          if Succeeded(SHCreateItemFromParsingName(PChar(LFileName), nil, IShellItem, LSrcFileItem)) then
            LFileOp.CopyItem(LSrcFileItem, LDestFolderItem, PChar(TPath.GetFileName(LFileName)), nil);
        end;
        Result := Succeeded(LFileOp.PerformOperations);
      end;
    end;
    CoUninitialize;
  end;
end;

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

class function TPlatformDirectory.Delete(const ADirectory: string; const ACanRecycle: Boolean): Boolean;
var
  LShFileOp: TSHFileOpStruct;
begin
  FillChar(LShFileOp, SizeOf(LShFileOp), 0);
  LShFileOp.wFunc := FO_DELETE;
  LShFileOp.pFrom := PChar(ADirectory + #0);
  LShFileOp.pTo := nil;
  if ACanRecycle then
    LShFileOp.fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT
  else
    LShFileOp.fFlags := FOF_NO_UI;
  Result := SHFileOperation(LShFileOp) = 0;
end;

end.
