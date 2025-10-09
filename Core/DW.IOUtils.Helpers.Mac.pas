unit DW.IOUtils.Helpers.Mac;

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
    class function GetAppSupportPath(const AFolder: string): string; static;
    class function GetResourcesPath(const AFolder: string): string; static;
  end;

  TPlatformDirectory = record
  public
    class function Copy(const ASrcDirectory, ADestDirectory: string): Boolean; static;
    class function Delete(const ADirectory: string; const ACanRecycle: Boolean): Boolean; static;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.SysUtils,
  // DW
  DW.IOUtils.Helpers,
  // macOS
  {$IF Defined(IOS)}
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  {$ELSEIF Defined(MACOS)}
  Macapi.Foundation,
  {$ENDIF}
  Macapi.Helpers;

{$IF (CompilerVersion < 37) and Defined(IOS)}
type
  NSFileManager = interface(iOSapi.Foundation.NSFileManager)
    ['{238C346D-D21F-4CC7-9966-125C68F46259}']
    function trashItemAtURL(url: NSURL; resultingItemURL: NSURL; error: PPointer = nil): Boolean; cdecl;
  end;
  TNSFileManager = class(TOCGenericImport<NSFileManagerClass, NSFileManager>)  end;
{$ENDIF}

function FileManager: NSFileManager;
begin
  {$IF (CompilerVersion < 37) or Defined(OSX)}
  Result := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  {$ELSE}
  Result := TNSFileManager.OCClass.defaultManager;
  {$ENDIF}
end;

function MainBundle: NSBundle;
begin
  {$IF (CompilerVersion < 37) or Defined(OSX)}
  Result := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
  {$ELSE}
  Result := TNSBundle.OCClass.mainBundle;
  {$ENDIF}
end;

{ TPlatformPath }

// Based on:
//   https://stackoverflow.com/questions/16204988/ios-cant-save-file-to-application-support-folder-but-can-to-documents
class function TPlatformPath.GetAppSupportPath(const AFolder: string): string;
var
  LURLs: NSArray;
  LFileManager: NSFileManager;
  LPath: NSString;
begin
  LURLs := FileManager.URLsForDirectory(NSApplicationSupportDirectory, NSUserDomainMask);
  LPath := TNSURL.Wrap(LURLs.lastObject).path;
  Result := NSStrToStr(LPath);
  if TOSVersion.Platform = TOSVersion.TPlatform.pfMacOS then
    Result := TPath.Combine(Result, TPathHelper.GetAppName);
  if AFolder <> '' then
    Result := TPath.Combine(Result, AFolder);
  ForceDirectories(Result);
end;

class function TPlatformPath.GetResourcesPath(const AFolder: string): string;
begin
  Result := NSStrToStr(MainBundle.bundlePath);
  if TOSVersion.Platform = TOSVersion.TPlatform.pfMacOS then
    Result := TPathHelper.CombineAll([Result, 'Contents', 'Resources', AFolder])
  else
    Result := TPath.Combine(Result, AFolder);
end;

{ TPlatformDirectory }

class function TPlatformDirectory.Copy(const ASrcDirectory, ADestDirectory: string): Boolean;
var
  LError: Pointer;
  LMessage: string;
begin
  LError := nil;
  FileManager.copyItemAtPath(StrToNSStr(ASrcDirectory).stringByResolvingSymlinksInPath, StrToNSStr(ADestDirectory), @LError);
  Result := LError = nil;
  if not Result then
    LMessage := NSStrToStr(TNSError.Wrap(LError).localizedDescription);
end;

class function TPlatformDirectory.Delete(const ADirectory: string; const ACanRecycle: Boolean): Boolean;
var
  LError: Pointer;
  LURL: NSURL;
begin
  // LError needs to be initialized to nil, because if the call succeeds, it does not touch the reference
  LError := nil;
  {$IF (CompilerVersion < 37) or Defined(OSX)}
  LURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(ADirectory), True));
  {$ELSE}
  LURL := TNSURL.OCClass.fileURLWithPath(StrToNSStr(ADirectory), True);
  {$ENDIF}
  if ACanRecycle then
    FileManager.trashItemAtURL(LURL, nil, @LError)
  else
    FileManager.removeItemAtURL(LURL, @LError);
  Result := LError = nil;
end;

end.
