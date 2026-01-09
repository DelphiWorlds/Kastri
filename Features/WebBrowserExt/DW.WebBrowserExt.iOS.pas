unit DW.WebBrowserExt.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.WebKit, iOSapi.Foundation, iOSapi.UIKit,
  //
  FMX.WebBrowser.Delegate.iOS,
  // DW
  DW.WebBrowserExt, DW.WebBrowserExt.Cocoa, DW.JavaScript;

type
  TPlatformWebBrowserExt = class;

  TWebBrowserExtNavigationDelegate = class(TOCLocal, WKNavigationDelegateEx)
  private
    FDownloadDelegate: TDownloadDelegate;
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
    FWebViewNavigationDelegate: WKNavigationDelegateSlim;
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

  // The following class is yet to be in use:
  TScriptMessageHandler = class(TOCLocal, WKScriptMessageHandler)
  private
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
  public
    { WKScriptMessageHandler }
    [MethodName('userContentController:didReceiveScriptMessage:')]
    procedure userContentController(userContentController: WKUserContentController; message: WKScriptMessage); cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
  end;

  TPlatformWebBrowserExt = class(TPlatformCocoaWebBrowserExt)
  private
    FJavaScriptResultHandler: TJavaScriptResultProc;
    FNavigationDelegate: TWebBrowserExtNavigationDelegate;
    FScriptMessageHandler: TScriptMessageHandler;
    FWebView: WKWebView;
    procedure JavaScriptCompletionHandler(obj: Pointer; error: NSError);
    procedure NOPCompletionHandler;
    procedure TakeSnapshotCompletionHandler(snapshotImage: UIImage; error: NSError);
  protected
    procedure ClearCache(const ADataKinds: TCacheDataKinds); override;
    procedure DidReceiveScriptMessage(const AMessage: WKScriptMessage);
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
  System.SysUtils, System.Classes, System.IOUtils, System.StrUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjCRuntime,
  // iOS
  iOSapi.CoreGraphics,
  // FMX
  FMX.Graphics,
  // DW
  DW.OSLog, DW.OSDevice, DW.Graphics.Helpers.Mac;

type
  WKWebViewEx = interface(WKWebView)
    ['{2CAA2A17-458F-4CEB-8C34-5C35C8026B9D}']
    function isInspectable: Boolean; cdecl;
    procedure setInspectable(inspectable: Boolean); cdecl;
  end;
  TWKWebViewEx = class(TOCGenericImport<WKWebViewClass, WKWebViewEx>) end;

{ TWebBrowserExtNavigationDelegate }

constructor TWebBrowserExtNavigationDelegate.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
  // The following line converts the delegate object id into the desired interface
  WrapInterface(FPlatformWebBrowserExt.WebView.navigationDelegate, TypeInfo(WKNavigationDelegateSlim), FWebViewNavigationDelegate);
  FDownloadDelegate := TDownloadDelegate.Create(FPlatformWebBrowserExt);
end;

destructor TWebBrowserExtNavigationDelegate.Destroy;
begin
  FDownloadDelegate.Free;
  inherited;
end;

(*
procedure TWebBrowserExtNavigationDelegate.webViewDecidePolicyForNavigationAction(webView: WKWebView; navigationAction: WKNavigationAction;
  decisionHandler: Pointer);
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
  LURL: string;
  LPreventDefault: Boolean;
begin
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
var
  LBlockImp: procedure(policy: WKNavigationActionPolicy); cdecl;
begin
  if FPlatformWebBrowserExt.WillDownload(navigationResponse.response) then
  begin
    @LBlockImp := imp_implementationWithBlock(decisionHandler);
    LBlockImp(WKNavigationActionPolicyCancel);
    imp_removeBlock(@LBlockImp);
  end
  else
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
*)

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
    FWebViewNavigationDelegate.webViewDecidePolicyForNavigationAction(webView, decidePolicyForNavigationAction, decisionHandler);
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
  FWebViewNavigationDelegate.webViewDidFailProvisionalNavigation(webView, didFailProvisionalNavigation, withError);
end;

procedure TWebBrowserExtNavigationDelegate.webView(webView: WKWebView; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge;
  completionHandler: Pointer);
begin
  FWebViewNavigationDelegate.webViewDidReceiveAuthenticationChallenge(webView, didReceiveAuthenticationChallenge, completionHandler);
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
    FWebViewNavigationDelegate.webViewDecidePolicyForNavigationResponse(webView, decidePolicyForNavigationResponse, decisionHandler);
end;

//procedure TWebBrowserExtNavigationDelegate.webViewAuthenticationChallenge(webView: WKWebView; authenticationChallenge: NSURLAuthenticationChallenge;
//  shouldAllowDeprecatedTLS: Pointer);
//begin
//
//end;

procedure TWebBrowserExtNavigationDelegate.webViewDidCommitNavigation(webView: WKWebView; didCommitNavigation: WKNavigation);
begin
  // "Default" delegate for iOS does not support this
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFailNavigation(webView: WKWebView; didFailNavigation: WKNavigation; withError: NSError);
begin
  FWebViewNavigationDelegate.webViewDidFailNavigation(webView, didFailNavigation, withError);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidFinishNavigation(webView: WKWebView; didFinishNavigation: WKNavigation);
begin
  FWebViewNavigationDelegate.webViewDidFinishNavigation(webView, didFinishNavigation);
end;

procedure TWebBrowserExtNavigationDelegate.webViewDidReceiveServerRedirectForProvisionalNavigation(webView: WKWebView;
  didReceiveServerRedirectForProvisionalNavigation: WKNavigation);
begin
  // "Default" delegate for iOS does not support this
end;

//procedure TWebBrowserExtNavigationDelegate.webViewWebContentProcessDidTerminate(webView: WKWebView);
//begin
//
//end;
{ TScriptMessageHandler }

constructor TScriptMessageHandler.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
end;

procedure TScriptMessageHandler.userContentController(userContentController: WKUserContentController; message: WKScriptMessage);
begin
  FPlatformWebBrowserExt.DidReceiveScriptMessage(message);
end;

{ TPlatformWebBrowserExt }

constructor TPlatformWebBrowserExt.Create(const AWebBrowserExt: TWebBrowserExt);
begin
  inherited;
  Supports(Browser, WKWebView, FWebView);
  // Hijack the original navigation delegate
  FNavigationDelegate := TWebBrowserExtNavigationDelegate.Create(Self);
  FWebView.setNavigationDelegate(FNavigationDelegate.GetObjectID);

// Apparently WKScriptMessageHandler works ONLY with Javascript that is executed in the app code, e.g.: window.webkit.messageHandlers.logger.postMessage(message);
//  FScriptMessageHandler := TScriptMessageHandler.Create(Self);
//  FWebView.configuration.userContentController.addScriptMessageHandler(FScriptMessageHandler.GetObjectID, StrToNSStr('ScriptMessageHandler'));

// The code at the end of this method makes the app inspectable in Safari on a Mac, however the following steps need to be performed in order for that to happen:

// Enable Developer Mode on your iOS Device:
//   Go to Settings > Privacy & Security.
//   Scroll down and tap on Developer Mode.
//   Turn on Developer Mode and follow any prompts to authorize this mode.

// Enable Web Inspector in Safari Settings:
//   On your iOS device, open Settings.
//   Scroll down and select Safari.
//   Scroll to the bottom to find the Advanced section.
//   Toggle on the Web Inspector option.

// Connect your iOS device to a Mac via a cable.
// Open Safari on your Mac. Ensure you have the Develop menu enabled:
//   Go to Safari > Preferences > Advanced.
//   Check the box at the bottom that says "Show Develop menu in menu bar".
// In Safari on your Mac, go to the Develop menu.
//   You should see your iOS device listed in the menu. Hover over it, and it should list all open WKWebViews in your app.
//   Select the app you want to inspect. This will open a Web Inspector window similar to Safari's developer tools on macOS.

  if TOSVersion.Check(16, 4) then
    TWKWebViewEx.Wrap(NSObjectToID(FWebView)).setInspectable(True);
end;

destructor TPlatformWebBrowserExt.Destroy;
begin
  FNavigationDelegate.Free;
  FScriptMessageHandler.Free;
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
  TWKWebsiteDataStore.OCClass.defaultDataStore.removeDataOfTypes(LDataTypes, LSinceDate, NOPCompletionHandler);
end;

procedure TPlatformWebBrowserExt.DidReceiveScriptMessage(const AMessage: WKScriptMessage);
begin
  TOSLog.d('WBExt Console Message: %s', [NSStrToStr(TNSString.Wrap(AMessage.body))]);
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
    {$IF (CompilerVersion < 37) or Defined(OSX)}
    LCookieStorage := TNSHTTPCookieStorage.Wrap(TNSHTTPCookieStorage.OCClass.sharedHTTPCookieStorage);
    {$ELSE}
    LCookieStorage := TNSHTTPCookieStorage.OCClass.sharedHTTPCookieStorage;
    {$ENDIF}
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
    LRequest := TNSURLRequest.Wrap(TNSURLRequest.OCClass.requestWithURL(LURL, LCachePolicy, 0));
    FWebView.loadRequest(LRequest);
  end;
end;

end.
