unit DW.WebBrowserExt.Mac;

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
  System.Classes,
  // macOS
  Macapi.WebKit, Macapi.Foundation, Macapi.ObjectiveC,
  // FMX
  FMX.WebBrowser.Delegate.Mac,
  // DW
  DW.WebBrowserExt, DW.WebBrowserExt.Cocoa, DW.JavaScript;

type
  TPlatformWebBrowserExt = class;

  TWebBrowserExtNavigationDelegate = class(TOCLocal, WKNavigationDelegateEx)
  private
    FDownloadDelegate: TDownloadDelegate;
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
    FWebViewNavigationDelegate: WKNavigationDelegate;
  public
    { WKNavigationDelegateEx }
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
    procedure webView(webView: WKWebView; didStartProvisionalNavigation: WKNavigation); overload; cdecl;
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
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
    destructor Destroy; override;
  end;

  TPlatformWebBrowserExt = class(TPlatformCocoaWebBrowserExt)
  private
    FJavaScriptResultHandler: TJavaScriptResultProc;
    FNavigationDelegate: TWebBrowserExtNavigationDelegate;
    FWebView: WKWebView;
    procedure JavaScriptCompletionHandler(obj: Pointer; error: NSError);
  protected
    procedure ClearCache(const ADataKinds: TCacheDataKinds); override;
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc); override;
    procedure Navigate(const AURL: string); override;
    property WebView: WKWebView read FWebView;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); override;
    destructor Destroy; override;
  end;

implementation

uses
  SimpleLog.Log,
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers, Macapi.CocoaTypes, Macapi.ObjCRuntime;

// Declarations missing from Macapi.WebKit

type
  WKHTTPCookieStore = interface;
  WKWebsiteDataStore = interface;

  WKCookiePolicy = NSInteger;

  TWKHTTPCookieStoreBlockMethod1 = procedure(param1: NSArray) of object;
  TWKHTTPCookieStoreBlockMethod2 = procedure of object;
  TWKHTTPCookieStoreBlockMethod3 = procedure(param1: WKCookiePolicy) of object;
  TWKWebsiteDataStoreBlockMethod1 = procedure(param1: NSArray) of object;
  TWKWebsiteDataStoreBlockMethod2 = procedure of object;
  TWKWebsiteDataStoreBlockMethod3 = procedure(param1: NSError) of object;

  WKHTTPCookieStoreClass = interface(NSObjectClass)
    ['{CC5DF6E5-4DDF-4B25-A615-889E7B77B192}']
  end;

  WKHTTPCookieStore = interface(NSObject)
    ['{BDF3FF79-052F-41F3-B51F-E3508D723041}']
    procedure addObserver(observer: Pointer); cdecl;
    procedure deleteCookie(cookie: NSHTTPCookie; completionHandler: TWKHTTPCookieStoreBlockMethod2); cdecl;
    procedure getAllCookies(completionHandler: TWKHTTPCookieStoreBlockMethod1); cdecl;
    procedure getCookiePolicy(completionHandler: TWKHTTPCookieStoreBlockMethod3); cdecl;
    procedure removeObserver(observer: Pointer); cdecl;
    procedure setCookie(cookie: NSHTTPCookie; completionHandler: TWKHTTPCookieStoreBlockMethod2); cdecl;
    procedure setCookiePolicy(policy: WKCookiePolicy; completionHandler: TWKHTTPCookieStoreBlockMethod2); cdecl;
  end;
  TWKHTTPCookieStore = class(TOCGenericImport<WKHTTPCookieStoreClass, WKHTTPCookieStore>) end;

  WKWebsiteDataStoreClass = interface(NSObjectClass)
    ['{61837FC5-A13F-43CA-BA73-44B0FFEA474D}']
    {class} function allWebsiteDataTypes: NSSet; cdecl;
    {class} function dataStoreForIdentifier(identifier: NSUUID): WKWebsiteDataStore; cdecl;
    {class} function defaultDataStore: WKWebsiteDataStore; cdecl;
    {class} procedure fetchAllDataStoreIdentifiers(completionHandler: TWKWebsiteDataStoreBlockMethod1); cdecl;
    {class} function nonPersistentDataStore: WKWebsiteDataStore; cdecl;
    {class} procedure removeDataStoreForIdentifier(identifier: NSUUID; completionHandler: TWKWebsiteDataStoreBlockMethod3); cdecl;
  end;

  WKWebsiteDataStore = interface(NSObject)
    ['{C1E2A83B-844A-4FC4-90A6-7D86991776E6}']
    procedure fetchDataRecordsOfTypes(dataTypes: NSSet; completionHandler: TWKWebsiteDataStoreBlockMethod1); cdecl;
    function httpCookieStore: WKHTTPCookieStore; cdecl;
    function identifier: NSUUID; cdecl;
    function isPersistent: Boolean; cdecl;
    function new: Pointer; cdecl;
    procedure removeDataOfTypes(dataTypes: NSSet; modifiedSince: NSDate; completionHandler: TWKWebsiteDataStoreBlockMethod2); overload; cdecl;
    procedure removeDataOfTypes(dataTypes: NSSet; forDataRecords: NSArray; completionHandler: TWKWebsiteDataStoreBlockMethod2); overload; cdecl;
  end;
  TWKWebsiteDataStore = class(TOCGenericImport<WKWebsiteDataStoreClass, WKWebsiteDataStore>) end;

function WKWebsiteDataTypeCookies: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeCookies');
end;

function WKWebsiteDataTypeSessionStorage: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeSessionStorage');
end;

function WKWebsiteDataTypeLocalStorage: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeLocalStorage');
end;

function WKWebsiteDataTypeWebSQLDatabases: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeWebSQLDatabases');
end;

function WKWebsiteDataTypeIndexedDBDatabases: NSString;
begin
  Result := CocoaNSStringConst(libWebKit, 'WKWebsiteDataTypeIndexedDBDatabases');
end;

{ TWebBrowserExtNavigationDelegate }

constructor TWebBrowserExtNavigationDelegate.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
  // The following line converts the delegate object id into the desired interface
  WrapInterface(FPlatformWebBrowserExt.WebView.navigationDelegate, TypeInfo(WKNavigationDelegate), FWebViewNavigationDelegate);
  FDownloadDelegate := TDownloadDelegate.Create(FPlatformWebBrowserExt);
end;

destructor TWebBrowserExtNavigationDelegate.Destroy;
begin
  FDownloadDelegate.Free;
  inherited;
end;

//procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; shouldGoToBackForwardListItem: WKBackForwardListItem;
//  willUseInstantBack: Boolean; completionHandler: Pointer);
//begin
//
//end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
  LURL: string;
  LPreventDefault: Boolean;
  LPolicy: WKNavigationActionPolicy;
begin
  // Just highjacking this method
  LURL := NSStrToStr(decidePolicyForNavigationAction.request.URL.absoluteString);
  LPreventDefault := False;
  if decidePolicyForNavigationAction.navigationType = WKNavigationTypeLinkActivated then
    FPlatformWebBrowserExt.DoElementClick(THitTestKind.SrcAnchor, LURL, LPreventDefault);
  LPolicy := WKNavigationActionPolicyAllow;
  if LPreventDefault then
    LPolicy := WKNavigationActionPolicyCancel
  else if TWKNavigationActionEx.Wrap(decidePolicyForNavigationAction).shouldPerformDownload then
    LPolicy := WKNavigationActionPolicyDownload;
  // If preventing the default, tell the OS to cancel
  // If a download should be performed, do that
  // Otherwise, fall through to the "hijacked" delegate
  if LPolicy <> WKNavigationActionPolicyAllow then
  begin
    @LBlockImp := imp_implementationWithBlock(decisionHandler);
    LBlockImp(LPolicy);
    imp_removeBlock(@LBlockImp);
  end
  else
    FWebViewNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler(webView, decidePolicyForNavigationAction, decisionHandler);
end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; decidePolicyForNavigationAction: WKNavigationAction;
  preferences: WKWebpagePreferences; decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy; ignored: Pointer; preferences: Pointer); cdecl;
  LPolicy: WKNavigationActionPolicy;
  LURL: string;
  LPreventDefault: Boolean;
begin
  // Basically repeat the one above, but pass preferences - Delphi is yet to support this, so do not forward to "hijacked" delegate
  LURL := NSStrToStr(decidePolicyForNavigationAction.request.URL.absoluteString);
  LPreventDefault := False;
  if decidePolicyForNavigationAction.navigationType = WKNavigationTypeLinkActivated then
    FPlatformWebBrowserExt.DoElementClick(THitTestKind.SrcAnchor, LURL, LPreventDefault);
  LPolicy := WKNavigationActionPolicyAllow;
  if LPreventDefault then
    LPolicy := WKNavigationActionPolicyCancel
  else if TWKNavigationActionEx.Wrap(decidePolicyForNavigationAction).shouldPerformDownload then
    LPolicy := WKNavigationActionPolicyDownload;
  @LBlockImp := imp_implementationWithBlock(decisionHandler);
  LBlockImp(LPolicy, nil, NSObjectToID(preferences));
  imp_removeBlock(@LBlockImp);
end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; navigationAction: WKNavigationAction; didBecomeDownload: WKDownload);
begin
  didBecomeDownload.setDelegate(FDownloadDelegate.ObjectID);
end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; navigationResponse: WKNavigationResponse; didBecomeDownload: WKDownload);
begin
  didBecomeDownload.setDelegate(FDownloadDelegate.ObjectID);
end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; didFailProvisionalNavigation: WKNavigation; withError: NSError);
begin
  FWebViewNavigationDelegate.webViewDidFailProvisionalNavigationWithError(webView, didFailProvisionalNavigation, withError);
end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge;
  completionHandler: Pointer);
begin
  FWebViewNavigationDelegate.webViewDidReceiveAuthenticationChallengeCompletionHandler(webView, didReceiveAuthenticationChallenge, completionHandler);
end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; didStartProvisionalNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidStartProvisionalNavigation(webView, didStartProvisionalNavigation);
end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; decidePolicyForNavigationResponse: WKNavigationResponse;
  decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
begin
//  Log.d('decidePolicyForNavigationResponse > WillDownload');
  if FPlatformWebBrowserExt.WillDownload(decidePolicyForNavigationResponse.response) then
  begin
    @LBlockImp := imp_implementationWithBlock(decisionHandler);
    LBlockImp(WKNavigationActionPolicyCancel);
    imp_removeBlock(@LBlockImp);
  end
  else
    FWebViewNavigationDelegate.webViewDecidePolicyForNavigationResponseDecisionHandler(webView, decidePolicyForNavigationResponse, decisionHandler);
end;

//procedure TWebBrowserExtNavigationDelegate.webViewAuthenticationChallenge(webView: WKWebView; authenticationChallenge: NSURLAuthenticationChallenge;
//  shouldAllowDeprecatedTLS: Pointer);
//begin
//
//end;

procedure TWebBrowserExtNavigationDelegate.webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidCommitNavigation(webView, didCommitNavigation);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFailNavigation(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError);
begin
  FWebViewNavigationDelegate.webViewDidFailNavigationWithError(webView, didFailNavigation, withError);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidFinishNavigation(webView, didFinishNavigation);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView;
  didReceiveServerRedirectForProvisionalNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView, didReceiveServerRedirectForProvisionalNavigation);
end;

//procedure TWebBrowserExtNavigationDelegate.webViewWebContentProcessDidTerminate(webView: WKWebView);
//begin
//
//end;

{ TPlatformWebBrowserExt }

constructor TPlatformWebBrowserExt.Create(const AWebBrowserExt: TWebBrowserExt);
begin
  inherited;
  Supports(Browser, WKWebView, FWebView);
  // Hijack the original navigation delegate
  FNavigationDelegate := TWebBrowserExtNavigationDelegate.Create(Self);
  FWebView.setNavigationDelegate(FNavigationDelegate.GetObjectID);
end;

destructor TPlatformWebBrowserExt.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformWebBrowserExt.ClearCache(const ADataKinds: TCacheDataKinds);
var
  LDataTypes: NSSet;
  LArray: NSMutableArray;
  LKind: TCacheDataKind;
  LValue: NSString;
  LSinceDate: NSDate;
begin
  LArray := TNSMutableArray.Create;
  for LKind := Low(TCacheDataKind) to High(TCacheDataKind) do
  begin
    if (ADataKinds = []) or (LKind in ADataKinds) then
    begin
      LValue := nil;
      case LKind of
        TCacheDataKind.Cookies:
          LValue := WKWebsiteDataTypeCookies;
        TCacheDataKind.LocalStorage:
          LValue := WKWebsiteDataTypeLocalStorage;
        TCacheDataKind.SessionStorage:
          LValue := WKWebsiteDataTypeSessionStorage;
        TCacheDataKind.IndexedDBDatabases:
          LValue := WKWebsiteDataTypeIndexedDBDatabases;
        TCacheDataKind.WebSQLDatabases:
          LValue := WKWebsiteDataTypeWebSQLDatabases;
      end;
      if LValue <> nil then
        LArray.addObject(NSObjectToID(WKWebsiteDataTypeCookies))
    end;
  end;
  LDataTypes := TNSSet.Wrap(TNSSet.OCClass.setWithArray(LArray));
  LSinceDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSince1970(0));
  TWKWebsiteDataStore.OCClass.defaultDataStore.removeDataOfTypes(LDataTypes, LSinceDate, nil);
end;

procedure TPlatformWebBrowserExt.ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc);
begin
  if FWebView <> nil then
  begin
    FJavaScriptResultHandler := AHandler;
    FWebView.evaluateJavaScript(StrToNSStr(AJavaScript), JavaScriptCompletionHandler);
  end
  else if Assigned(AHandler) then
    AHandler(cJavaScriptNullResult, -1);
end;

procedure TPlatformWebBrowserExt.JavaScriptCompletionHandler(obj: Pointer; error: NSError);
var
  LJavaScriptResult: string;
  LCode: Integer;
begin
  if Assigned(FJavaScriptResultHandler) then
  begin
    if obj <> nil then
      LJavaScriptResult := NSStrToStr(TNSString.Wrap(obj))
    else
      LJavaScriptResult := cJavaScriptNullResult;
    if error = nil then
      LCode := 0
    else
      LCode := error.code;
    FJavaScriptResultHandler(LJavaScriptResult, LCode);
  end;
end;

procedure TPlatformWebBrowserExt.Navigate(const AURL: string);
var
  LURL: NSURL;
  LRequest: NSURLRequest;
  LCachePolicy: NSURLRequestCachePolicy;
begin
  if FWebView <> nil then
  begin
    if Browser.EnableCaching then
      LCachePolicy := NSURLRequestReloadRevalidatingCacheData
    else
      LCachePolicy := NSURLRequestReloadIgnoringLocalCacheData;
    LURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(AURL)));
    LRequest:= TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(LURL, LCachePolicy, 0));
    FWebView.loadRequest(LRequest);
  end;
end;

end.
