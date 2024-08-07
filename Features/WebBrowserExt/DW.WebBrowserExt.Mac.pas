unit DW.WebBrowserExt.Mac;

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
  System.Classes,
  // macOS
  Macapi.WebKit, Macapi.Foundation, Macapi.ObjectiveC,
  // FMX
  FMX.WebBrowser.Delegate.Mac,
  // DW
  DW.WebBrowserExt, DW.JavaScript;

type
  TPlatformWebBrowserExt = class;

  TWebBrowserExtNavigationDelegate = class(TOCLocal, WKNavigationDelegate)
  private
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
    FWebViewNavigationDelegate: WKNavigationDelegate;
  public
    { WKNavigationDelegate }
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationActionDecisionHandler(webView: WKWebView;
      decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer); cdecl;
    [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationResponseDecisionHandler(webView: WKWebView;
      decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer); cdecl;
    [MethodName('webView:didStartProvisionalNavigation:')]
    procedure webViewDidStartProvisionalNavigation(webView: WKWebView;
      didStartProvisionalNavigation: WKNavigation); cdecl;
    [MethodName('webView:didReceiveServerRedirectForProvisionalNavigation:')]
    procedure webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView;
      didReceiveServerRedirectForProvisionalNavigation: WKNavigation); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisionalNavigationWithError(webView: WKWebView;
      didFailProvisionalNavigation: WKNavigation; withError: NSError); cdecl;
    [MethodName('webView:didCommitNavigation:')]
    procedure webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigationWithError(webView: WKWebView; didFailNavigation: WKNavigation;
      withError: NSError); cdecl;
    [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
    procedure webViewDidReceiveAuthenticationChallengeCompletionHandler(webView: WKWebView;
      didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge;
      completionHandler: Pointer); cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
  end;

  TPlatformWebBrowserExt = class(TCustomPlatformWebBrowserExt)
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
end;

procedure TWebBrowserExtNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler(webView: WKWebView;
  decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
  LURL: string;
  LPreventDefault: Boolean;
begin
  // Just highjacking this method
  LURL := NSStrToStr(decidePolicyForNavigationAction.request.URL.absoluteString);
  LPreventDefault := False;
  if decidePolicyForNavigationAction.navigationType = WKNavigationTypeLinkActivated then
    FPlatformWebBrowserExt.DoElementClick(THitTestKind.SrcAnchor, LURL, LPreventDefault);
  // If preventing the default, tell the OS to cancel, otherwise use the default delegate's method
  if LPreventDefault then
  begin
    @LBlockImp := imp_implementationWithBlock(decisionHandler);
    LBlockImp(WKNavigationActionPolicyCancel);
    imp_removeBlock(@LBlockImp);
  end
  else
    FWebViewNavigationDelegate.webViewDecidePolicyForNavigationActionDecisionHandler(webView, decidePolicyForNavigationAction, decisionHandler);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDecidePolicyForNavigationResponseDecisionHandler(webView: WKWebView;
  decidePolicyForNavigationResponse: WKNavigationResponse; decisionHandler: Pointer);
begin
  FWebViewNavigationDelegate.webViewDecidePolicyForNavigationResponseDecisionHandler(webView, decidePolicyForNavigationResponse, decisionHandler);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidCommitNavigation(webView, didCommitNavigation);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFailNavigationWithError(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError);
begin
  FWebViewNavigationDelegate.webViewDidFailNavigationWithError(webView, didFailNavigation, withError);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFailProvisionalNavigationWithError(webView: WKWebView;
  didFailProvisionalNavigation: WKNavigation; withError: NSError);
begin
  FWebViewNavigationDelegate.webViewDidFailProvisionalNavigationWithError(webView, didFailProvisionalNavigation, withError);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidFinishNavigation(webView, didFinishNavigation);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidReceiveAuthenticationChallengeCompletionHandler(webView: WKWebView;
  didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge; completionHandler: Pointer);
begin
  FWebViewNavigationDelegate.webViewDidReceiveAuthenticationChallengeCompletionHandler(webView, didReceiveAuthenticationChallenge, completionHandler);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView;
  didReceiveServerRedirectForProvisionalNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView, didReceiveServerRedirectForProvisionalNavigation);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidStartProvisionalNavigation(webView: WKWebView; didStartProvisionalNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidStartProvisionalNavigation(webView, didStartProvisionalNavigation);
end;

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
