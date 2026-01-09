unit DW.WebBrowserExt.Cocoa;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
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
  iOSapi.Foundation, iOSapi.WebKit, iOSapi.CocoaTypes,
  {$ELSE}
  Macapi.Foundation, Macapi.WebKit, Macapi.CocoaTypes,
  {$ENDIF}
  // DW
  DW.WebBrowserExt;

const
  WKNavigationActionPolicyDownload = 2;

type
  TDownloadTasks = TDictionary<Pointer, string>;

  WKContentMode = NSInteger;
  WKWebpagePreferencesUpgradeToHTTPSPolicy = NSInteger;

  TWKDownloadBlockMethod1 = procedure(resumeData: NSData) of object;

  WKDownloadClass = interface(NSObjectClass)
    ['{696F3140-1ABA-4623-B2FB-05FA0409AFC2}']
  end;

  WKDownload = interface(NSObject)
    ['{E8D9AA2C-790E-4CF5-90CE-B238964922CE}']
    procedure cancel(completionHandler: TWKDownloadBlockMethod1); cdecl;
    function delegate: Pointer; cdecl;
    function isUserInitiated: Boolean; cdecl;
    function originalRequest: NSURLRequest; cdecl;
    function originatingFrame: WKFrameInfo; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function webView: WKWebView; cdecl;
  end;
  TWKDownload = class(TOCGenericImport<WKDownloadClass, WKDownload>) end;

  WKWebpagePreferencesClass = interface(NSObjectClass)
    ['{3CCA0D97-EC78-407D-9FF6-BA467F44801B}']
  end;

  WKWebpagePreferences = interface(NSObject)
    ['{148A209E-EF61-4E08-AD63-0B3E0D6ED39A}']
    function allowsContentJavaScript: Boolean; cdecl;
    function isLockdownModeEnabled: Boolean; cdecl;
    function preferredContentMode: WKContentMode; cdecl;
    function preferredHTTPSNavigationPolicy: WKWebpagePreferencesUpgradeToHTTPSPolicy; cdecl;
    procedure setAllowsContentJavaScript(allowsContentJavaScript: Boolean); cdecl;
    procedure setLockdownModeEnabled(lockdownModeEnabled: Boolean); cdecl;
    procedure setPreferredContentMode(preferredContentMode: WKContentMode); cdecl;
    procedure setPreferredHTTPSNavigationPolicy(preferredHTTPSNavigationPolicy: WKWebpagePreferencesUpgradeToHTTPSPolicy); cdecl;
  end;
  TWKWebpagePreferences = class(TOCGenericImport<WKWebpagePreferencesClass, WKWebpagePreferences>) end;

  WKNavigationDelegateEx = interface(IObjectiveC)
    ['{1185A816-5AD8-4178-A137-32C1A59CA249}']
    procedure webView(webView: WKWebView; didFailProvisionalNavigation: WKNavigation; withError: NSError); overload; cdecl;
    procedure webView(webView: WKWebView; navigationResponse: WKNavigationResponse; didBecomeDownload: WKDownload); overload; cdecl;
    procedure webView(webView: WKWebView; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer); overload; cdecl;
    procedure webView(webView: WKWebView; navigationAction: WKNavigationAction; didBecomeDownload: WKDownload); overload; cdecl;
    // procedure webView(webView: WKWebView; shouldGoToBackForwardListItem: WKBackForwardListItem; willUseInstantBack: Boolean;
    //   completionHandler: Pointer); overload; cdecl;
    procedure webView(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); overload; cdecl;
    procedure webView(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; preferences: WKWebpagePreferences;
      decisionHandler: Pointer); overload; cdecl;
    procedure webView(webView: WKWebView; decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer); overload; cdecl;
    // procedure webView(webView: WKWebView; didStartProvisionalNavigation: WKNavigation); overload; cdecl;
    // [MethodName('webView:authenticationChallenge:shouldAllowDeprecatedTLS:')]
    // procedure webViewAuthenticationChallenge(webView: WKWebView; authenticationChallenge: NSURLAuthenticationChallenge;
    //   shouldAllowDeprecatedTLS: Pointer); cdecl;
    [MethodName('webView:didCommitNavigation:')]
    procedure webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
    [MethodName('webView:didReceiveServerRedirectForProvisionalNavigation:')]
    procedure webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView;
      didReceiveServerRedirectForProvisionalNavigation: WKNavigation); cdecl;
    // procedure webViewWebContentProcessDidTerminate(webView: WKWebView); cdecl;
  end;

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

  WKDownloadDelegate = interface(IObjectiveC)
    ['{10655688-E3C3-4ECD-9BC3-462CCB12D287}']
    procedure download(download: WKDownload; didFailWithError: NSError; resumeData: NSData); overload; cdecl;
    // procedure download(download: WKDownload; decidePlaceholderPolicy: Pointer); overload; cdecl;
    // procedure download(download: WKDownload; didReceivePlaceholderURL: NSURL; completionHandler: Pointer); overload; cdecl;
    // procedure download(download: WKDownload; didReceiveFinalURL: NSURL); overload; cdecl;
    procedure download(download: WKDownload; decideDestinationUsingResponse: NSURLResponse; suggestedFilename: NSString;
      completionHandler: Pointer); overload; cdecl;
    // procedure download(download: WKDownload; willPerformHTTPRedirection: NSHTTPURLResponse; newRequest: NSURLRequest;
    //   decisionHandler: Pointer); overload; cdecl;
    // procedure download(download: WKDownload; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge;
    //   completionHandler: Pointer); overload; cdecl;
    procedure downloadDidFinish(download: WKDownload); cdecl;
  end;

  WKNavigationActionEx = interface(WKNavigationAction)
    ['{FDEE0FAD-BCA6-4EE2-92CE-F3AB8F31E60B}']
    function shouldPerformDownload: Boolean; cdecl;
  end;
  TWKNavigationActionEx = class(TOCGenericImport<WKNavigationActionClass, WKNavigationActionEx>) end;

  TPlatformCocoaWebBrowserExt = class;

  TDownloadDelegate = class(TOCLocal, WKDownloadDelegate)
  private
    FFileName: string;
    FPlatformWebBrowserExt: TPlatformCocoaWebBrowserExt;
  public
    { WKDownloadDelegate }
    procedure download(download: WKDownload; didFailWithError: NSError; resumeData: NSData); overload; cdecl;
    procedure download(download: WKDownload; decideDestinationUsingResponse: NSURLResponse; suggestedFilename: NSString;
      completionHandler: Pointer); overload; cdecl;
    procedure downloadDidFinish(download: WKDownload); cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformCocoaWebBrowserExt);
    property ObjectID: Pointer read GetObjectID;
  end;

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
    FDownloadSession: NSURLSession;
    FDownloadTasks: TDownloadTasks;
    FURLSessionDelegate: TURLSessionDelegate;
    procedure CreateDownloadSession;
  protected
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
  Macapi.Helpers, Macapi.ObjCRuntime,
  // DW
  DW.OSLog;

{ TDownloadDelegate }

constructor TDownloadDelegate.Create(const APlatformWebBrowserExt: TPlatformCocoaWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
end;

procedure TDownloadDelegate.download(download: WKDownload; didFailWithError: NSError; resumeData: NSData);
begin
  FPlatformWebBrowserExt.DoDownloadStateChange(FFileName, TDownloadState.Failed);
end;

procedure TDownloadDelegate.download(download: WKDownload; decideDestinationUsingResponse: NSURLResponse; suggestedFilename: NSString;
  completionHandler: Pointer);
var
  LBlockImp: procedure(url: Pointer); cdecl;
  LFilePath: NSString;
  LURL: NSURL;
  LMimeType, LFolder: string;
begin
  LMimeType := NSStrToStr(decideDestinationUsingResponse.MIMEType);
  LFilePath := StrToNSStr(FPlatformWebBrowserExt.DefaultDownloadsFolder).stringByAppendingPathComponent(suggestedFilename);
  FFileName := NSStrToStr(LFilePath);
  FPlatformWebBrowserExt.DoDownloadStart(NSStrToStr(decideDestinationUsingResponse.URL.absoluteString), LMimeType, FFileName);
  LFolder := TPath.GetDirectoryName(FFileName);
  if not LFolder.IsEmpty and ForceDirectories(LFolder) then
  begin
    {$IF (CompilerVersion > 36) and not Defined(OSX)}
    LURL := TNSURL.OCClass.fileURLWithPath(StrToNSStr(FFileName));
    {$ELSE}
    LURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(FFileName)));
    {$ENDIF}
    @LBlockImp := imp_implementationWithBlock(completionHandler);
    LBlockImp(NSObjectToID(LURL));
    imp_removeBlock(@LBlockImp);
  end
  else
    TOSLog.d('Invalid path for filename: %s', [FFileName]);
end;

procedure TDownloadDelegate.downloadDidFinish(download: WKDownload);
begin
  FPlatformWebBrowserExt.DoDownloadStateChange(FFileName, TDownloadState.Completed);
end;

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
  {$IF (CompilerVersion < 37) or Defined(OSX)} //!!!!!
  FDownloadSession := TNSURLSession.OCClass.sessionWithConfigurationDelegateDelegateQueue(LConfig, FURLSessionDelegate.GetObjectID, nil);
  {$ELSE}
  FDownloadSession := TNSURLSession.OCClass.sessionWithConfiguration(LConfig, FURLSessionDelegate.GetObjectID, nil);
  {$ENDIF}
end;

procedure TPlatformCocoaWebBrowserExt.DownloadStateChange(const ATask: NSURLSessionTask; const AURL: NSURL; const AError: NSError);
var
  LFileName: string;
  LState: TDownloadState;
  LFileURL: NSURL;
  {$IF CompilerVersion > 36}
  LError: Pointer;
  {$ENDIF}
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
        {$IF (CompilerVersion > 36) and not Defined(OSX)}
        LError := nil;
        LFileURL := TNSURL.OCClass.fileURLWithPath(StrToNSStr(LFileName));
        if not TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager).moveItemAtURL(AURL, LFileURL, @LError) then
          LFileName := NSStrToStr(AURL.absoluteString);
        {$ELSE}
        LFileURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(LFileName)));
        if not TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager).moveItemAtURL(AURL, LFileURL) then
          LFileName := NSStrToStr(AURL.absoluteString);
        {$ENDIF}
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
//  TOSLog.d('+TPlatformCocoaWebBrowserExt.WillDownload');
  Result := False;
  LMimeType := NSStrToStr(AResponse.MIMEType);
  if IsMatchingMimeType(LMimeType) then
  begin
    TOSLog.d('> DoDownloadStart');
    DoDownloadStart(NSStrToStr(AResponse.URL.absoluteString), LMimeType, LFileName);
    Result := not LFileName.IsEmpty;
    if Result then
    begin
      LTask := FDownloadSession.downloadTaskWithURL(AResponse.URL);
      FDownloadTasks.Add(NSObjectToID(LTask), LFileName);
      LTask.resume;
    end;
  end;
//  TOSLog.d('-TPlatformCocoaWebBrowserExt.WillDownload');
end;

end.
