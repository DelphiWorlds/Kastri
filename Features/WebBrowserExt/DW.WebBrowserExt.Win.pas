unit DW.WebBrowserExt.Win;

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
  // DW
  DW.WebBrowserExt, DW.JavaScript;

type
  TPlatformWebBrowserExt = class(TCustomPlatformWebBrowserExt)
  protected
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc); override;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // FMX
  FMX.WebBrowser,
  // Win
  Winapi.WebView2,
  // DW
  DW.JavaScript.WebView2;

{ TPlatformWebBrowserExt }

constructor TPlatformWebBrowserExt.Create(const AWebBrowserExt: TWebBrowserExt);
begin
  inherited;
  //
end;

destructor TPlatformWebBrowserExt.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformWebBrowserExt.ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc);
var
  LWebView: ICoreWebView2;
begin
  if Supports(Browser, ICoreWebView2, LWebView) then
    LWebView.ExecuteScript(PChar(AJavaScript), TJavaScriptResultCallback.Create(AHandler))
  else
    AHandler(cJavaScriptNullResult, -1);
end;

end.
