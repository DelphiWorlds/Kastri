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
  DW.WebBrowserExt;

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
  Winapi.WebView2;

type
  TJavaScriptResultCallback = class(TInterfacedObject, ICoreWebView2ExecuteScriptCompletedHandler)
  private
    FHandler: TJavaScriptResultProc;
  public
    { ICoreWebView2ExecuteScriptCompletedHandler }
    function Invoke(errorCode: HResult; resultObjectAsJson: PWideChar): HResult; stdcall;
  public
    constructor Create(const AHandler: TJavaScriptResultProc);
  end;

{ TJavaScriptResultCallback }

constructor TJavaScriptResultCallback.Create(const AHandler: TJavaScriptResultProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

function TJavaScriptResultCallback.Invoke(errorCode: HResult; resultObjectAsJson: PWideChar): HResult;
begin
  Result := S_OK;
  if Assigned(FHandler) then
    FHandler(string(resultObjectAsJson), errorCode);
end;

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
