unit DW.IOUtils.Helpers.Mac;

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
    class function GetAppSupportPath(const AFolder: string): string; static;
    class function GetResourcesPath(const AFolder: string): string; static;
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
  iOSapi.Foundation,
  {$ENDIF}
  Macapi.Helpers;

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

end.
