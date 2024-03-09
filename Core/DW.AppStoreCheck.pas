unit DW.AppStoreCheck;

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

type
  TAppStore = (iTunes);

  TAppStoreInfo = record
    AppName: string;
    BundleId: string;
    Currency: string;
    FileSizeBytes: Int64;
    MinimumOSVersion: string;
    Price: Double;
    ReleaseDate: TDateTime;
    SellerName: string;
    Version: string;
    procedure FromJSON(const AJSON: string);
    procedure Reset;
  end;

  TAppStoreCheckResult = record
    AppStoreInfo: TAppStoreInfo;
    StatusCode: Integer;
    Succeeded: Boolean;
    ErrorMessage: string;
    procedure Reset;
  end;

  TAppStoreCheckResultProc = reference to procedure(const AppStoreCheckResult: TAppStoreCheckResult);

  TAppStoreCheck = record
  private
    class procedure DoGet(const AURL: string; const AHandler: TAppStoreCheckResultProc); static;
    class procedure Get(const AURL: string; const AHandler: TAppStoreCheckResultProc); static;
    class procedure HandleResult(const AAppStoreCheckResult: TAppStoreCheckResult; const AHandler: TAppStoreCheckResultProc); static;
    class procedure InternalGet(const AURL: string; const AHandler: TAppStoreCheckResultProc); static;
  public
    class procedure Check(const AAppStore: TAppStore; const AIdent: string; const AHandler: TAppStoreCheckResultProc); static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes, System.Net.HttpClient, System.JSON, System.DateUtils, System.Generics.Collections;

const
  cITunesQueryURLTemplate = 'https://itunes.apple.com/lookup?id=%s';

{ TAppStoreInfo }

procedure TAppStoreInfo.FromJSON(const AJSON: string);
var
  LResponse, LAppInfo: TJSONValue;
  LResults: TJSONArray;
  LReleaseDate, LFileSizeBytes: string;
begin
  LResponse := TJSONObject.ParseJSONValue(AJSON);
  if LResponse <> nil then
  try
    if LResponse.TryGetValue('results', LResults) and (LResults.Count > 0) then
    begin
      LAppInfo := LResults.Items[0];
      LAppInfo.TryGetValue('trackName', AppName);
      LAppInfo.TryGetValue('bundleId', BundleId);
      LAppInfo.TryGetValue('currency', Currency);
      LAppInfo.TryGetValue('minimumOsVersion', MinimumOSVersion);
      LAppInfo.TryGetValue('price', Price);
      LAppInfo.TryGetValue('sellerName', SellerName);
      LAppInfo.TryGetValue('version', Version);
      if LAppInfo.TryGetValue('fileSizeBytes', LFileSizeBytes) then
        FileSizeBytes := StrToInt64Def(LFileSizeBytes, 0);
      if LAppInfo.TryGetValue('currentVersionReleaseDate', LReleaseDate) then
        ReleaseDate := ISO8601ToDate(LReleaseDate);
    end;
  finally
    LResponse.Free;
  end;
end;

procedure TAppStoreInfo.Reset;
begin
  AppName := '';
  BundleId := '';
  Currency := '';
  FileSizeBytes := 0;
  MinimumOSVersion := '';
  Price := 0;
  ReleaseDate := 0;
  SellerName := '';
  Version := '';
end;

{ TAppStoreCheckResult }

procedure TAppStoreCheckResult.Reset;
begin
  AppStoreInfo.Reset;
  StatusCode := -1;
  Succeeded := False;
  ErrorMessage := '';
end;

{ TAppStoreCheck }

class procedure TAppStoreCheck.HandleResult(const AAppStoreCheckResult: TAppStoreCheckResult; const AHandler: TAppStoreCheckResultProc);
begin
  TThread.Synchronize(nil, procedure begin AHandler(AAppStoreCheckResult) end)
end;

class procedure TAppStoreCheck.DoGet(const AURL: string; const AHandler: TAppStoreCheckResultProc);
var
  LHTTP: THTTPClient;
  LResponse: IHTTPResponse;
  LAppStoreCheckResult: TAppStoreCheckResult;
begin
  LHTTP := THTTPClient.Create;
  try
    LResponse := LHTTP.Get(AURL);
    LAppStoreCheckResult.StatusCode := LResponse.StatusCode;
    if LResponse.StatusCode = 200 then
    begin
      LAppStoreCheckResult.Succeeded := True;
      LAppStoreCheckResult.AppStoreInfo.FromJSON(LResponse.ContentAsString);
    end
    else
      LAppStoreCheckResult.ErrorMessage := LResponse.StatusText;
    HandleResult(LAppStoreCheckResult, AHandler);
  finally
    LHTTP.Free;
  end;
end;

class procedure TAppStoreCheck.InternalGet(const AURL: string; const AHandler: TAppStoreCheckResultProc);
var
  LAppStoreCheckResult: TAppStoreCheckResult;
begin
  try
    DoGet(AURL, AHandler);
  except
    on E: Exception do
    begin
      LAppStoreCheckResult.Reset;
      LAppStoreCheckResult.ErrorMessage := Format('%s: %s', [E.ClassName, E.Message]);
      HandleResult(LAppStoreCheckResult, AHandler);
    end;
  end;
end;

class procedure TAppStoreCheck.Get(const AURL: string; const AHandler: TAppStoreCheckResultProc);
begin
  TThread.CreateAnonymousThread(procedure begin InternalGet(AURL, AHandler) end).Start;
end;

class procedure TAppStoreCheck.Check(const AAppStore: TAppStore; const AIdent: string; const AHandler: TAppStoreCheckResultProc);
var
  LURL: string;
  LResult: TAppStoreCheckResult;
begin
  LResult.Succeeded := False;
  if AAppStore = TAppStore.iTunes then
  begin
    LURL := Format(cITunesQueryURLTemplate, [AIdent]);
    Get(LURL, AHandler);
  end
  else
    AHandler(LResult); // Only iTunes supported at present
end;

end.
