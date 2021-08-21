unit DW.IOUtils.Helpers.Mac;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

type
  TPlatformPath = record
    class function GetAppSupportPath(const AFolder: string): string; static;
    class function GetResourcesPath(const AFolder: string): string; static;
  end;

  TPlatformDirectory = record
  public
    class function Copy(const ASrcDirectory, ADestDirectory: string): Boolean; static;
    class function Delete(const ADirectory: string): Boolean; static;
  end;

implementation

uses
  // RTL
  System.IOUtils, System.SysUtils,
  // DW
  DW.IOUtils.Helpers,
  // macOS
  {$IF Defined(MACDEV)}
  Macapi.Foundation,
  {$ENDIF}
  {$IF Defined(IOS)}
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  {$ENDIF}
  Macapi.Helpers;

{$IF Defined(IOS)}
type
  NSFileManager = interface(iOSapi.Foundation.NSFileManager)
    ['{238C346D-D21F-4CC7-9966-125C68F46259}']
    function trashItemAtURL(url: NSURL; resultingItemURL: NSURL; error: PPointer = nil): Boolean; cdecl;
  end;

  TNSFileManager = class(TOCGenericImport<NSFileManagerClass, NSFileManager>)  end;
{$ENDIF}

function FileManager: NSFileManager;
begin
  Result := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
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
  LFileManager := TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager);
  LURLs := LFileManager.URLsForDirectory(NSApplicationSupportDirectory, NSUserDomainMask);
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
  Result := NSStrToStr(TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).bundlePath);
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

class function TPlatformDirectory.Delete(const ADirectory: string): Boolean;
var
  LError: Pointer;
begin
  // LError needs to be initialized to nil, because if trashItemAtURL succeeds, it does not touch the reference
  LError := nil;
  FileManager.trashItemAtURL(TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(ADirectory), True)), nil, @LError);
  Result := LError = nil;
end;

end.
