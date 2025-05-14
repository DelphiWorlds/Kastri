unit DW.WebBrowserExt.Cocoa;

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
  System.Generics.Collections,
  // macOS
  Macapi.ObjectiveC,
  {$IF Defined(IOS)}
  iOSapi.Foundation,
  {$ELSE}
  Macapi.Foundation,
  {$ENDIF}
  // DW
  DW.WebBrowserExt;

type
  TDownloadTasks = TDictionary<Pointer, string>;

  NSURLSessionTaskDelegateSlim = interface(IObjectiveC)
    ['{DC0BAFFE-0A21-48E0-B2AC-DBB464CCA936}']
    [MethodName('URLSession:task:didCompleteWithError:')]
    procedure URLSessionTaskDidCompleteWithError(session: NSURLSession; task: NSURLSessionTask; didCompleteWithError: NSError); cdecl;
  end;

  NSURLSessionDownloadDelegateSlim = interface(IObjectiveC)
    ['{8B2DF6EA-B789-44BE-B63B-1A403CFB8133}']
    [MethodName('URLSession:downloadTask:didFinishDownloadingToURL:')]
    procedure URLSessionDownloadTaskDidFinishDownloadingToURL(session: NSURLSession; downloadTask: NSURLSessionDownloadTask;
      didFinishDownloadingToURL: NSURL); cdecl;
  end;

  TPlatformCocoaWebBrowserExt = class;

  TURLSessionDelegate = class(TOCLocal, NSURLSessionTaskDelegateSlim, NSURLSessionDownloadDelegateSlim)
  private
    FPlatformWebBrowserExt: TPlatformCocoaWebBrowserExt;
  public
    { NSURLSessionTaskDelegateSlim }
    [MethodName('URLSession:task:didCompleteWithError:')]
    procedure URLSessionTaskDidCompleteWithError(session: NSURLSession; task: NSURLSessionTask;
      didCompleteWithError: NSError); cdecl;
    { NSURLSessionDownloadDelegateSlim }
    [MethodName('URLSession:downloadTask:didFinishDownloadingToURL:')]
    procedure URLSessionDownloadTaskDidFinishDownloadingToURL(session: NSURLSession;
      downloadTask: NSURLSessionDownloadTask; didFinishDownloadingToURL: NSURL); cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformCocoaWebBrowserExt);
  end;

  TPlatformCocoaWebBrowserExt = class(TCustomPlatformWebBrowserExt)
  private
    FDownloadableMimeTypes: TArray<string>;
    FDownloadSession: NSURLSession;
    FDownloadTasks: TDownloadTasks;
    FURLSessionDelegate: TURLSessionDelegate;
    procedure CreateDownloadSession;
  protected
    function CanDownload(const AMimeType: string): Boolean;
    procedure DownloadStateChange(const ATask: NSURLSessionTask; const AURL: NSURL; const AError: NSError);
    function WillDownload(const AResponse: NSURLResponse): Boolean;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.StrUtils, System.SysUtils, System.IOUtils, System.Classes,
  // macOS
  Macapi.Helpers,
  // DW
  DW.OSLog;

{ TURLSessionDelegate }

constructor TURLSessionDelegate.Create(const APlatformWebBrowserExt: TPlatformCocoaWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
end;

procedure TURLSessionDelegate.URLSessionDownloadTaskDidFinishDownloadingToURL(session: NSURLSession; downloadTask: NSURLSessionDownloadTask;
  didFinishDownloadingToURL: NSURL);
begin
  FPlatformWebBrowserExt.DownloadStateChange(downloadTask, didFinishDownloadingToURL, nil);
end;

procedure TURLSessionDelegate.URLSessionTaskDidCompleteWithError(session: NSURLSession; task: NSURLSessionTask; didCompleteWithError: NSError);
begin
  if didCompleteWithError.code <> 0 then
    FPlatformWebBrowserExt.DownloadStateChange(task, nil, didCompleteWithError);
end;


{ TPlatformCocoaWebBrowserExt }

constructor TPlatformCocoaWebBrowserExt.Create(const AWebBrowserExt: TWebBrowserExt);
begin
  inherited;
  FDownloadTasks := TDownloadTasks.Create;
  CreateDownloadSession;
end;

destructor TPlatformCocoaWebBrowserExt.Destroy;
begin
  FDownloadTasks.Free;
  inherited;
end;

procedure TPlatformCocoaWebBrowserExt.CreateDownloadSession;
var
  LConfig: NSURLSessionConfiguration;
begin
  FURLSessionDelegate := TURLSessionDelegate.Create(Self);
  LConfig := TNSURLSessionConfiguration.OCClass.defaultSessionConfiguration;
  FDownloadSession := TNSURLSession.OCClass.sessionWithConfigurationDelegateDelegateQueue(LConfig, FURLSessionDelegate.GetObjectID, nil);
end;

function TPlatformCocoaWebBrowserExt.CanDownload(const AMimeType: string): Boolean;
begin
  Result := IndexText(AMimeType, DownloadableMimeTypes) > -1;
end;

procedure TPlatformCocoaWebBrowserExt.DownloadStateChange(const ATask: NSURLSessionTask; const AURL: NSURL; const AError: NSError);
var
  LFileName: string;
  LState: TDownloadState;
  LFileURL: NSURL;
begin
  if FDownloadTasks.TryGetValue(NSObjectToID(ATask), LFileName) then
  begin
    FDownloadTasks.Remove(NSObjectToID(ATask));
    if AError <> nil then
    begin
      TOSLog.e('> Error: %d - %s', [AError.code, NSStrToStr(AError.localizedDescription)]);
      LState := TDownloadState.Failed;
    end
    else
      LState := TDownloadState.Completed;
    if AURL <> nil then
    begin
      if TPath.GetDirectoryName(LFileName).IsEmpty and not DefaultDownloadsFolder.IsEmpty and ForceDirectories(DefaultDownloadsFolder) then
        LFileName := TPath.Combine(DefaultDownloadsFolder, LFileName);
      if not TPath.GetDirectoryName(LFileName).IsEmpty then
      begin
        if TFile.Exists(LFileName) then
          TFile.Delete(LFileName);
        LFileURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(LFileName)));
        if not TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager).moveItemAtURL(AURL, LFileURL) then
          LFileName := NSStrToStr(AURL.absoluteString);
      end
      else
        LFileName := NSStrToStr(AURL.absoluteString);
    end;
    TThread.Queue(nil, procedure begin DoDownloadStateChange(LFileName, LState); end);
  end
  else
    TOSLog.e('> Download task appears to be awol');
end;

function TPlatformCocoaWebBrowserExt.WillDownload(const AResponse: NSURLResponse): Boolean;
var
  LFileName, LMimeType: string;
  LTask: NSURLSessionDownloadTask;
begin
  Result := False;
  LMimeType := NSStrToStr(AResponse.MIMEType);
  if CanDownload(LMimeType) then
  begin
    DoDownloadStart(NSStrToStr(AResponse.URL.absoluteString), LMimeType, LFileName);
    Result := not LFileName.IsEmpty;
    if Result then
    begin
      LTask := FDownloadSession.downloadTaskWithURL(AResponse.URL);
      FDownloadTasks.Add(NSObjectToID(LTask), LFileName);
      LTask.resume;
    end;
  end;
end;

end.
