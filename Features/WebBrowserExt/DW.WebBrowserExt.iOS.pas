unit DW.WebBrowserExt.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.WebKit, iOSapi.Foundation, iOSapi.UIKit,
  //
  FMX.WebBrowser.Delegate.iOS,
  // DW
  DW.WebBrowserExt, DW.JavaScript;

type
  TPlatformWebBrowserExt = class;

  TWebBrowserExtNavigationDelegate = class(TOCLocal, WKNavigationDelegateSlim)
  private
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
    FWebViewNavigationDelegate: WKNavigationDelegateSlim;
  public
    { WKNavigationDelegateSlim }
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
      decisionHandler: Pointer); overload; cdecl;
    [MethodName('webView:decidePolicyForNavigationResponse:decisionHandler:')]
    procedure webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse;
      decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
    [MethodName('webView:didReceiveAuthenticationChallenge:completionHandler:')]
    procedure webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge;
      completionHandler: Pointer); cdecl;
    [MethodName('webView:didStartProvisionalNavigation:')]
    procedure webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation); cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
  end;

  TPlatformWebBrowserExt = class(TCustomPlatformWebBrowserExt)
  private
    FJavaScriptResultHandler: TJavaScriptResultProc;
    FNavigationDelegate: TWebBrowserExtNavigationDelegate;
    FWebView: WKWebView;
    procedure JavaScriptCompletionHandler(obj: Pointer; error: NSError);
    procedure NOPCompletionHandler;
    procedure TakeSnapshotCompletionHandler(snapshotImage: UIImage; error: NSError);
  protected
    procedure DoCaptureBitmap; override;
    procedure DoElementClick(const AHitTestKind: THitTestKind; const AExtra: string; var APreventDefault: Boolean); override;
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc); override;
    procedure FlushCookies(const ARemove: Boolean); override;
    function GetPrintAdapter(const ADocumentName: string): IInterface; override;
    procedure Navigate(const AURL: string); override;
    property  WebView: WKWebView read FWebView;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjCRuntime,
  // iOS
  iOSapi.CoreGraphics,
  // FMX
  FMX.Graphics,
  // DW
  DW.OSDevice, DW.Graphics.Helpers.Mac;

{ TWebBrowserExtNavigationDelegate }

constructor TWebBrowserExtNavigationDelegate.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
  // The following line converts the delegate object id into the desired interface
  WrapInterFace(FPlatformWebBrowserExt.WebView.navigationDelegate, TypeInfo(WKNavigationDelegateSlim), FWebViewNavigationDelegate);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
  decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
  LURL: string;
  LPreventDefault: Boolean;
begin
  // Just highjacking this method
  LURL := NSStrToStr(navigationAction.request.URL.absoluteString);
  LPreventDefault := False;
  if navigationAction.navigationType = WKNavigationTypeLinkActivated then
    FPlatformWebBrowserExt.DoElementClick(THitTestKind.SrcAnchor, LURL, LPreventDefault);
  // If preventing the default, tell the OS to cancel, otherwise use the default delegate's method
  if LPreventDefault then
  begin
    @LBlockImp := imp_implementationWithBlock(decisionHandler);
    LBlockImp(WKNavigationActionPolicyCancel);
    imp_removeBlock(@LBlockImp);
  end
  else
    FWebViewNavigationDelegate.webViewDecidePolicyForNavigationAction(webView, navigationAction, decisionHandler);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDecidePolicyForNavigationResponse(webView: WKWebView; navigationResponse: WKNavigationResponse;
  decisionHandler: Pointer);
begin
  FWebViewNavigationDelegate.webViewDecidePolicyForNavigationResponse(webView, navigationResponse, decisionHandler);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFailNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError);
begin
  FWebViewNavigationDelegate.webViewDidFailNavigation(webView, navigation, error);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFailProvisionalNavigation(webView: WKWebView; navigation: WKNavigation; error: NSError);
begin
  FWebViewNavigationDelegate.webViewDidFailProvisionalNavigation(webView, navigation, error);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFinishNavigation(webView: WKWebView; navigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidFinishNavigation(webView, navigation);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidReceiveAuthenticationChallenge(webView: WKWebView; challenge: NSURLAuthenticationChallenge;
  completionHandler: Pointer);
begin
  FWebViewNavigationDelegate.webViewDidReceiveAuthenticationChallenge(webView, challenge, completionHandler);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidStartProvisionalNavigation(webView: WKWebView; navigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidStartProvisionalNavigation(webView, navigation);
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
  FNavigationDelegate.Free;
  inherited;
end;

procedure TPlatformWebBrowserExt.TakeSnapshotCompletionHandler(snapshotImage: UIImage; error: NSError);
var
  LBitmap: TBitmap;
begin
  LBitmap := TBitmap.Create;
  try
    LBitmap.FromCGImageRef(snapshotImage.CGImage);
    BitmapCaptured(LBitmap);
  finally
    LBitmap.Free;
  end;
end;

procedure TPlatformWebBrowserExt.DoCaptureBitmap;
var
  LSnapshotConfiguration: WKSnapshotConfiguration;
begin
  if FWebView <> nil then
  begin
    LSnapshotConfiguration := TWKSnapshotConfiguration.Create;
    LSnapshotConfiguration.setRect(CGRectMake(0, 0, FWebView.frame.size.width, FWebView.frame.size.height));
    FWebView.takeSnapshotWithConfiguration(LSnapshotConfiguration, TakeSnapshotCompletionHandler);
  end
  else
    inherited;
end;

procedure TPlatformWebBrowserExt.DoElementClick(const AHitTestKind: THitTestKind; const AExtra: string; var APreventDefault: Boolean);
begin
  inherited;
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

procedure TPlatformWebBrowserExt.NOPCompletionHandler;
begin
  // NOP = No OPeration, i.e. do nothing
end;

procedure TPlatformWebBrowserExt.FlushCookies(const ARemove: Boolean);
var
  LDataStore: WKWebsiteDataStore;
  LDataTypes: NSSet;
  LDateFrom: NSDate;
  LCookieStorage: NSHTTPCookieStorage;
  I: Integer;
begin
  LDataStore := TWKWebsiteDataStore.OCClass.defaultDataStore;
  LDataTypes := TNSSet.Wrap(TNSSet.OCClass.setWithObject(NSObjectToID(WKWebsiteDataTypeCookies)));
  LDateFrom := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSince1970(0));
  LDataStore.removeDataOfTypes(LDataTypes, LDateFrom, NOPCompletionHandler);
  if ARemove then
  begin
    LCookieStorage := TNSHTTPCookieStorage.Wrap(TNSHTTPCookieStorage.OCClass.sharedHTTPCookieStorage);
    for I := LCookieStorage.cookies.count - 1 downto 0 do
      LCookieStorage.deleteCookie(TNSHTTPCookie.Wrap(LCookieStorage.cookies.objectAtIndex(I)));
  end;
end;

function TPlatformWebBrowserExt.GetPrintAdapter(const ADocumentName: string): IInterface;
begin
  if FWebView <> nil then
    Result := FWebView.viewPrintFormatter
  else
    Result := nil;
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
