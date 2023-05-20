unit DW.AppUpdate.iOS;

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
  FMX.WebBrowser,
  DW.AppUpdate, DW.WebBrowserExt;

type
  TPlatformAppUpdate = class(TCustomPlatformAppUpdate)
  private
    FWebBrowser: TWebBrowser;
    FWebBrowserExt: TWebBrowserExt;
    procedure WebBrowserDidFinishLoadHandler(Sender: TObject);
  protected
    procedure CheckForUpdate; override;
  public
    constructor Create(const AAppUpdate: TAppUpdate); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.IOUtils, System.SysUtils,
  DW.IOUtils.Helpers, DW.OSLog,
  Macapi.Helpers;

const
  cGetVersionJavaScriptFileName = 'applestore-getversion.js';

{ TPlatformAppUpdate }

constructor TPlatformAppUpdate.Create(const AAppUpdate: TAppUpdate);
begin
  inherited;
  FWebBrowser := TWebBrowser.Create(nil);
  FWebBrowserExt := TWebBrowserExt.Create(FWebBrowser);
  FWebBrowser.OnDidFinishLoad := WebBrowserDidFinishLoadHandler;
end;

destructor TPlatformAppUpdate.Destroy;
begin
  FWebBrowser.Free;
  inherited;
end;

procedure TPlatformAppUpdate.WebBrowserDidFinishLoadHandler(Sender: TObject);
var
  LScript, LScriptFileName: string;
  LInfo: TAppUpdateInfo;
begin
  LScriptFileName := TPathHelper.GetResourcesFile(cGetVersionJavaScriptFileName);
  if TFile.Exists(LScriptFileName) then
  begin
    LScript := TFile.ReadAllText(LScriptFileName);
    FWebBrowserExt.ExecuteJavaScript(LScript,
      procedure(const AJavaScriptResult: string; const AErrorCode: Integer)
      begin
        LInfo.Reset;
        if AErrorCode = 0 then
        begin
          LInfo.AvailableVersionString := AJavaScriptResult;
          if LInfo.AvailableVersionString.IsEmpty then
            LInfo.Error := TAppUpdateInfoError.VersionNotFound
          else
            LInfo.Error := TAppUpdateInfoError.Success;
        end
        else
        begin
          LInfo.Error := TAppUpdateInfoError.JavaScript;
          TOSLog.w('%s - JavaScript error code: %d', [cGetVersionJavaScriptFileName, AErrorCode]);
        end;
        DoAppUpdateInfo(LInfo);
      end
    );
  end
  else
  begin
    TOSLog.w('Cannot find %s. Please ensure you have deployed it with your app', [cGetVersionJavaScriptFileName]);
    LInfo.Reset;
    LInfo.Error := TAppUpdateInfoError.ScriptNotFound;
    DoAppUpdateInfo(LInfo);
  end;
end;

procedure TPlatformAppUpdate.CheckForUpdate;
begin
  FWebBrowser.Navigate(UpdateURL);
end;

end.
