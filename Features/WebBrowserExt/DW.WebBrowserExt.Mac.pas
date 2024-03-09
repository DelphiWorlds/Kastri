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
  // DW
  DW.WebBrowserExt;

type
  TPlatformWebBrowserExt = class(TCustomPlatformWebBrowserExt)
  private
    FJavaScriptResultHandler: TJavaScriptResultProc;
    FWebView: WKWebView;
    procedure JavaScriptCompletionHandler(obj: Pointer; error: NSError);
  protected
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc); override;
    procedure Navigate(const AURL: string); override;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers, Macapi.CocoaTypes;

{ TPlatformWebBrowserExt }

constructor TPlatformWebBrowserExt.Create(const AWebBrowserExt: TWebBrowserExt);
begin
  inherited;
  Supports(Browser, WKWebView, FWebView);
end;

destructor TPlatformWebBrowserExt.Destroy;
begin
  //
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
