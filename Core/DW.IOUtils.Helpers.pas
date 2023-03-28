unit DW.IOUtils.Helpers;

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
  // RTL
  System.IOUtils;

type
  TPathHelper = record
  public
    /// <summary>
    ///   Combines all paths in the array into a single path
    /// </summary>
    class function CombineAll(const APaths: array of string): string; static;
    /// <summary>
    ///   Returns the specified filename in the documents folder, or appends the app folder if not on mobile
    /// </summary>
    class function GetAppDocumentsFile(const AFileName: string; const AAppFolder: string = ''): string; static;
    /// <summary>
    ///   Returns the documents folder, or appends the app folder if not on mobile
    /// </summary>
    class function GetAppDocumentsPath(const AAppFolder: string = ''): string; static;
    /// <summary>
    ///   Returns the name of the executable, minus any extension
    /// </summary>
    class function GetAppName: string; static;
    /// <summary>
    ///   Returns the folder where the application executable is
    /// </summary>
    /// <remarks>
    ///   On MacOS, this is inside the .app package. The standard folder structure is: <appfolder>\<appname>.app\Contents\MacOS\
    ///   Where <appfolder> is the folder in which the application package resides (usually: Applications)
    /// </remarks>
    class function GetAppPath(const AFolder: string = ''): string; static;
    /// <summary>
    ///   Returns the specified filename in the application support folder, and appends the folder if present
    /// </summary>
    class function GetAppSupportFile(const AFileName: string; const AFolder: string = ''): string; static;
    /// <summary>
    ///   Returns the folder where application support files can be stored, such as app user settings etc
    /// </summary>
    /// <remarks>
    ///   This is distinct from GetDocumentsPath, which are files that the user should be able to access
    /// </remarks>
    class function GetAppSupportPath(const AFolder: string = ''): string; static;
    class function GetDirectoryName(const AFileName: string): string; static;
    /// <summary>
    ///   Returns the path to a file in deployed resources (e.g. on macOS, to Contents\Resources), and appends the folder if present
    /// </summary>
    class function GetResourcesFile(const AFileName: string; const AFolder: string = ''): string; static;
    /// <summary>
    ///   Returns the path to deployed resources (e.g. on macOS, to Contents\Resources)
    /// </summary>
    class function GetResourcesPath(const AFolder: string = ''): string; static;
    /// <summary>
    ///   Returns the path to shared documents (e.g. on Windows, to: \Users\Public\Public Documents)
    /// </summary>
    /// <remarks>
    ///   This function works around a bug in System.IOUtils where the function fails if called from a DLL
    /// </remarks>
    class function GetSharedDocumentsPath: string; static;
    /// <summary>
    ///   Returns the file name where the path is known to be a Windows path
    /// </summary>
    /// <remarks>
    ///   Useful for obtaining file names from design-time paths
    /// </remarks>
    class function GetWindowsFileName(const AFileName: string): string; static;
    /// <summary>
    ///   Returns whether or not the path exists. APath can refer to a directory, or file
    /// </summary>
    class function PathExists(const APath: string): Boolean; static;
  end;

  TFileHelper = record
    class function Backup(const AFileName: string; const AExt: string = 'bak'): Boolean; static;
    /// <summary>
    ///   Appends the source file to the end of the dest file
    /// </summary>
    class procedure Concat(const ASourceName, ADestName: string); static;
    /// <summary>
    ///   Copies the source file to dest file if it is newer
    /// </summary>
    class function CopyIfNewer(const ASourceName, ADestName: string; const AOverwrite: Boolean): Boolean; static;
    class function GetBackupFileName(const AFileName: string; const AExt: string = 'bak'): string; static;
    /// <summary>
    ///   Checks if the file is in use
    /// </summary>
    /// <remarks>
    ///   ***NOTE***
    ///   Use this function with extreme caution. In between calling this and attempting an operation on the file, another process
    ///   may acquire exclusive access
    /// </remarks>
    class function IsInUse(const AFileName: string): Boolean; static;
    /// <summary>
    ///   Checks if the source file is newer than the destination
    /// </summary>
    class function IsNewer(const ASourceName, ADestName: string): Boolean; static;
    /// <summary>
    ///   Returns the contents of the file as a string
    /// </summary>
    class function ReadString(const AFileName: string): string; static;
    /// <summary>
    ///   Sets/unsets a file as read-only
    /// </summary>
    class procedure SetReadOnly(const AFileName: string; const AReadOnly: Boolean); static;
    /// <summary>
    ///   Returns the size of the file, in bytes
    /// </summary>
    class function Size(const AFileName: string): Int64; static;
    /// <summary>
    ///   Saves the contents of the string to a file
    /// </summary>
    class procedure WriteString(const AFileName, AContent: string); static;
  end;

  TDirectoryHelper = record
    class function Copy(const ASrcDirectory, ADestDirectory: string): Boolean; static;
     /// <summary>
    ///   Deletes files based on multiple search patterns
    /// </summary>
    class procedure DeleteFiles(const APath: string; const APatterns: TArray<string>); overload; static;
    /// <summary>
    ///   Deletes the directory if it exists. Won't fail if the value passed is blank
    /// </summary>
    class function Delete(const ADirectory: string): Boolean; static;
    /// <summary>
    ///   Checks if the directory exists. Won't fail if the value passed is blank
    /// </summary>
    class function Exists(const ADirectory: string): Boolean; static;
    class function GetDirectories(const APath: string; const APatterns: TArray<string>;
      const ASearchOption: TSearchOption = TSearchOption.soTopDirectoryOnly): TArray<string>; static;
    /// <summary>
    ///   Retrieves files based on multiple search patterns
    /// </summary>
    class function GetFiles(const APath: string; const APatterns: TArray<string>;
      const ASearchOption: TSearchOption = TSearchOption.soTopDirectoryOnly): TArray<string>; overload; static;
    /// <summary>
    ///   Retrieves first sub-folder under APath
    /// </summary>
    class function GetFirstFolder(const APath: string): string; static;
    /// <summary>
    ///   Returns the size of the directory, in bytes
    /// </summary>
    class function Size(const APath: string): Int64; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes,
  // DW
  {$IF Defined(MSWINDOWS)}
  DW.IOUtils.Helpers.Win,
  {$ENDIF}
  {$IF Defined(MACOS)}
  DW.IOUtils.Helpers.Mac,
  {$ENDIF}
  {$IF Defined(POSIX)}
  DW.IOUtils.Helpers.Posix,
  {$ENDIF}
  DW.OSDevice;

{ TPathHelper }

class function TPathHelper.GetAppName: string;
var
  LExeName: string;
begin
  {$IF Defined(MSWINDOWS)}
  LExeName := GetModuleName(HInstance);
  {$ELSE}
  LExeName := ParamStr(0);
  {$ENDIF}
  Result := TPath.GetFileNameWithoutExtension(LExeName);
end;

class function TPathHelper.CombineAll(const APaths: array of string): string;
var
  I: Integer;
begin
  Result := APaths[Low(APaths)];
  for I := Succ(Low(APaths)) to High(APaths) do
    Result := TPath.Combine(Result, APaths[I]);
end;

class function TPathHelper.GetSharedDocumentsPath: string;
begin
{$IF Defined(MSWINDOWS)}
  Result := TPlatformPath.GetSharedDocumentsPath;
{$ELSE}
  Result := TPath.GetSharedDocumentsPath;
{$ENDIF}
end;

class function TPathHelper.GetAppDocumentsPath(const AAppFolder: string = ''): string;
begin
  if TOSDevice.IsMobile then
    Result := TPath.Combine(TPath.GetDocumentsPath, AAppFolder)
  else
    Result := GetSharedDocumentsPath;
  if not TOSDevice.IsMobile then
  begin
    Result := CombineAll([Result, GetAppName, AAppFolder]);
    ForceDirectories(Result);
  end;
end;

class function TPathHelper.GetAppPath(const AFolder: string = ''): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), AFolder);
end;

class function TPathHelper.GetAppSupportFile(const AFileName, AFolder: string): string;
begin
  Result := TPath.Combine(GetAppSupportPath(AFolder), AFilename);
end;

class function TPathHelper.GetAppSupportPath(const AFolder: string = ''): string;
{$IF Defined(MACOS)}
begin
  Result := TPlatformPath.GetAppSupportPath(AFolder);
end;
{$ELSE}
begin
  Result := GetAppDocumentsPath(AFolder);
end;
{$ENDIF}

class function TPathHelper.GetDirectoryName(const AFileName: string): string;
begin
  Result := '';
  if AFileName <> '' then
    Result := TPath.GetDirectoryName(AFileName);
end;

class function TPathHelper.GetAppDocumentsFile(const AFileName, AAppFolder: string): string;
begin
  Result := TPath.Combine(GetAppDocumentsPath(AAppFolder), AFileName);
end;

class function TPathHelper.GetResourcesFile(const AFileName, AFolder: string): string;
begin
  Result := TPath.Combine(GetResourcesPath(AFolder), AFileName);
end;

class function TPathHelper.GetResourcesPath(const AFolder: string = ''): string;
{$IF Defined(MACOS)}
begin
  Result := TPlatformPath.GetResourcesPath(AFolder);
end;
{$ELSE}
begin
  if TOSVersion.Platform = TOSVersion.TPlatform.pfAndroid then
    Result := GetAppDocumentsPath(AFolder)
  else
    Result := GetAppPath(AFolder);
end;
{$ENDIF}

class function TPathHelper.GetWindowsFileName(const AFileName: string): string;
var
  LIndex: Integer;
begin
  LIndex := LastDelimiter('\', AFileName);
  if LIndex > 0 then
    Result := AFileName.Substring(LIndex, Length(AFileName))
  else
    Result := AFileName;
end;

class function TPathHelper.PathExists(const APath: string): Boolean;
begin
  Result := not APath.IsEmpty and (TDirectory.Exists(APath) or TFile.Exists(APath));
end;

{ TFileHelper }

class function TFileHelper.Backup(const AFileName: string; const AExt: string = 'bak'): Boolean;
var
  LBackupFileName: string;
begin
  Result := False;
  LBackupFileName := GetBackupFileName(AFileName, AExt);
  if not LBackupFileName.IsEmpty and not TFile.Exists(LBackupFileName) then
  try
    TFile.Copy(AFileName, LBackupFileName);
    Result := True;
  except
    // Just eat the exception here
  end;
end;

class procedure TFileHelper.Concat(const ASourceName, ADestName: string);
var
  LSourceStream, LDestStream: TFileStream;
  LAppend: Boolean;
begin
  if TFIle.Exists(ASourceName) then
  begin
    LAppend := TFile.Exists(ADestName);
    if LAppend then
      LDestStream := TFileStream.Create(ADestName, fmOpenReadWrite)
    else
      LDestStream := TFileStream.Create(ADestName, fmCreate);
    try
      if LAppend then
        LDestStream.Position := LDestStream.Size;
      LSourceStream := TFileStream.Create(ASourceName, fmOpenRead);
      try
        LDestStream.CopyFrom(LSourceStream, LSourceStream.Size);
      finally
        LSourceStream.Free;
      end;
    finally
      LDestStream.Free;
    end;
  end;
end;

class function TFileHelper.CopyIfNewer(const ASourceName, ADestName: string; const AOverwrite: Boolean): Boolean;
var
  LDestDateTime: TDateTime;
begin
  Result := False;
  if TFile.Exists(ASourceName) then
  begin
    LDestDateTime := 0;
    if TFile.Exists(ADestName) then
      LDestDateTime := TFile.GetLastWriteTime(ADestName);
    if TFile.GetLastWriteTime(ASourceName) > LDestDateTime then
    begin
      TFile.Copy(ASourceName, ADestName, AOverwrite);
      Result := True;
    end;
  end;
end;

class function TFileHelper.GetBackupFileName(const AFileName: string; const AExt: string = 'bak'): string;
var
  LNumber: Integer;
  LBackupFileName, LExt: string;
begin
  LExt := AExt;
  if LExt.IsEmpty then
    LExt := 'bak';
  LNumber := 0;
  repeat
    if LNumber > 0 then
      LExt := LNumber.ToString + '.' + AExt;
    LBackupFileName := TPath.ChangeExtension(AFileName, LExt);
    Inc(LNumber);
  until not TFile.Exists(LBackupFileName) or (LNumber = MaxInt);
  if LNumber < MaxInt then
    Result := LBackupFileName;
end;

class function TFileHelper.IsInUse(const AFileName: string): Boolean;
{$IF Defined(MSWINDOWS)}
begin
  Result := TFile.Exists(AFileName) and TPlatformFile.IsInUse(AFileName);
end;
{$ELSE}
begin
  raise Exception.Create('Yet to be implemented');
end;
{$ENDIF}

class function TFileHelper.IsNewer(const ASourceName, ADestName: string): Boolean;
begin
  Result := not TFile.Exists(ADestName) or (TFile.GetLastWriteTime(ASourceName) > TFile.GetLastWriteTime(ADestName));
end;

class function TFileHelper.ReadString(const AFileName: string): string;
begin
  Result := '';
  if TFile.Exists(AFileName) then
    Result := TFile.ReadAllText(AFileName);
end;

class procedure TFileHelper.WriteString(const AFileName, AContent: string);
begin
  TFile.WriteAllText(AFileName, AContent);
end;

{$WARN SYMBOL_PLATFORM OFF}
class procedure TFileHelper.SetReadOnly(const AFileName: string; const AReadOnly: Boolean);
{$IF Defined(MSWINDOWS)}
var
  LAttributes: TFileAttributes;
begin
  // TODO: Implement for Posix systems
  LAttributes := TFile.GetAttributes(AFileName);
  if AReadOnly then
    Include(LAttributes, TFileAttribute.faReadOnly)
  else
    Exclude(LAttributes, TFileAttribute.faReadOnly);
  TFile.SetAttributes(AFileName, LAttributes);
end;
{$ELSE}
begin
end;
{$ENDIF}
{$WARN SYMBOL_PLATFORM ON}

class function TFileHelper.Size(const AFileName: string): Int64;
begin
  Result := TPlatformFile.GetFileSize(AFileName);
end;

{ TDirectoryHelper }

class function TDirectoryHelper.Copy(const ASrcDirectory, ADestDirectory: string): Boolean;
begin
  Result := False;
  if Exists(ASrcDirectory) then
  begin
    {$IF Defined(MACOS)}
      Result := TPlatformDirectory.Copy(ASrcDirectory, ADestDirectory);
    {$ELSE}
      TDirectory.Copy(ASrcDirectory, ADestDirectory);
      Result := True;
    {$ENDIF}
  end;
end;

class function TDirectoryHelper.Delete(const ADirectory: string): Boolean;
begin
  Result := not Exists(ADirectory);
  if not Result then
  begin
    {$IF Defined(MSWINDOWS) or Defined(MACOS)}
      Result := TPlatformDirectory.Delete(ADirectory);
    {$ELSE}
      Result := False;
    {$ENDIF}
  end;
end;

class procedure TDirectoryHelper.DeleteFiles(const APath: string; const APatterns: TArray<string>);
var
  LFiles: TArray<string>;
  LFile: string;
begin
  LFiles := GetFiles(APath, APatterns);
  for LFile in LFiles do
  try
    TFile.Delete(LFile);
  except
    // Eats any exception raised by deletion
  end;
end;

class function TDirectoryHelper.Exists(const ADirectory: string): Boolean;
begin
  Result := False;
  if not ADirectory.IsEmpty then
    Result := TDirectory.Exists(ADirectory);
end;

class function TDirectoryHelper.GetDirectories(const APath: string; const APatterns: TArray<string>;
  const ASearchOption: TSearchOption = TSearchOption.soTopDirectoryOnly): TArray<string>;
var
  LResult: TArray<string>;
  LPattern: string;
begin
  for LPattern in APatterns do
  begin
    LResult := TDirectory.GetDirectories(APath, LPattern, ASearchOption);
    Result := Concat(Result, LResult);
  end;
end;

class function TDirectoryHelper.GetFiles(const APath: string; const APatterns: TArray<string>;
  const ASearchOption: TSearchOption = TSearchOption.soTopDirectoryOnly): TArray<string>;
var
  LResult: TArray<string>;
  LPattern: string;
begin
  for LPattern in APatterns do
  begin
    LResult := TDirectory.GetFiles(APath, LPattern, ASearchOption);
    Result := Concat(Result, LResult);
  end;
end;

class function TDirectoryHelper.GetFirstFolder(const APath: string): string;
var
  LSearchRec: TSearchRec;
  LStop: Boolean;
  LName: string;
begin
  if FindFirst(TPath.Combine(APath, '*'), faAnyFile, LSearchRec) = 0 then
  try
    LStop := False;
    repeat
      if (LSearchRec.Attr and System.SysUtils.faDirectory) <> 0 then
      begin
        LName := LSearchRec.Name;
        if not (LName.Equals('.') or LName.Equals('..')) then
        begin
          Result := TPath.Combine(APath, LName);
          LStop := True;
        end;
      end;
    until LStop or (FindNext(LSearchRec) <> 0);
  finally
    FindClose(LSearchRec);
  end;
end;

class function TDirectoryHelper.Size(const APath: string): Int64;
var
  LSearchRec: TSearchRec;
  LFullPath: string;
begin
  Result := 0;
  if FindFirst(TPath.Combine(APath, '*'), faAnyFile, LSearchRec) = 0 then
  try
    repeat
      LFullPath := TPath.Combine(APath, LSearchRec.Name);
      if (LSearchRec.Attr and faDirectory <> 0) then
      begin
        if (LSearchRec.Name <> '.') and (LSearchRec.Name <> '..') then
          Inc(Result, Size(LFullPath));
      end
      else
        Inc(Result, TFileHelper.Size(LFullPath));
    until FindNext(LSearchRec) <> 0;
  finally
    FindClose(LSearchRec);
  end;
end;

end.
