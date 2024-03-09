unit DW.WebBrowserExt.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.WebKit, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero,
  Androidapi.JNI.Os, Androidapi.JNI.Net,
  // FMX
  FMX.Graphics,
  // DW
  DW.WebBrowserExt;

type
  TPlatformWebBrowserExt = class;

  TValueCallback = class(TJavaLocal, JValueCallback)
  private
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
    FHandler: TJavaScriptResultProc;
  public
    { JValueCallback }
    procedure onReceiveValue(value: JObject); cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt; const AHandler: TJavaScriptResultProc);
  end;

  TTouchListener = class(TJavaLocal, JView_OnTouchListener)
  private
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
  public
    { JView_OnTouchListener }
    function onTouch(v: JView; event: JMotionEvent): Boolean; cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
  end;

  TPlatformWebBrowserExt = class(TCustomPlatformWebBrowserExt)
  private
    class function GetHitTestKind(const AType: Integer): THitTestKind;
  private
    FTouchListener: JView_OnTouchListener;
    FValueCallbacks: TArray<JValueCallback>;
    FWebView: JWebView;
    function CreateValueCallback(const AHandler: TJavaScriptResultProc): JValueCallback;
    procedure RemoveValueCallback(const ACallback: JValueCallback);
  protected
    procedure DoCaptureBitmap; override;
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc); override;
    procedure FlushCookies(const ARemove: Boolean); override;
    function GetPrintAdapter(const ADocumentName: string): IInterface; override;
    procedure Navigate(const AURL: string); override;
    procedure ResetZoom; override;
    procedure SetAllowZoom(const AValue: Boolean); override;
    procedure SetInitialScale(const AValue: Integer); override;
    function WebViewTouch(const AView: JView; const AEvent: JMotionEvent): Boolean;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Print,
  // DW
  DW.Graphics.Helpers.Android;

{ TValueCallback }

constructor TValueCallback.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt; const AHandler: TJavaScriptResultProc);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
  FHandler := AHandler;
end;

procedure TValueCallback.onReceiveValue(value: JObject);
begin
  if Assigned(FHandler) then
    FHandler(JStringToString(TJString.Wrap(value)).DeQuotedString('"'), 0);
  FPlatformWebBrowserExt.RemoveValueCallback(Self);
end;

{ TTouchListener }

constructor TTouchListener.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
end;

function TTouchListener.onTouch(v: JView; event: JMotionEvent): Boolean;
begin
  Result := FPlatformWebBrowserExt.WebViewTouch(v, event);
end;

{ TPlatformWebBrowserExt }

constructor TPlatformWebBrowserExt.Create(const AWebBrowserExt: TWebBrowserExt);
begin
  inherited;
  Supports(Browser, JWebView, FWebView);
  if FWebView <> nil then
  begin
    FTouchListener := TTouchListener.Create(Self);
    FWebView.setOnTouchListener(FTouchListener);
  end;
end;

destructor TPlatformWebBrowserExt.Destroy;
begin
  //
  inherited;
end;

function TPlatformWebBrowserExt.CreateValueCallback(const AHandler: TJavaScriptResultProc): JValueCallback;
begin
  Result := TValueCallback.Create(Self, AHandler);
  FValueCallbacks := FValueCallbacks + [Result];
end;

procedure TPlatformWebBrowserExt.RemoveValueCallback(const ACallback: JValueCallback);
var
  I: Integer;
begin
  for I := 0 to Length(FValueCallbacks) - 1 do
  begin
    if FValueCallbacks[I] = ACallback then
    begin
      FValueCallbacks[I] := nil;
      Delete(FValueCallbacks, I, 1);
      Break;
    end;
  end;
end;

procedure TPlatformWebBrowserExt.ResetZoom;
begin
  if FWebView <> nil then
  begin
    FWebView.getSettings.setLoadWithOverviewMode(False);
    FWebView.getSettings.setLoadWithOverviewMode(True);
  end;
end;

// https://stackoverflow.com/a/35959284/3164070
procedure TPlatformWebBrowserExt.DoCaptureBitmap;
var
  LBitmap: TBitmap;
  LJBitmap: JBitmap;
  LHeight: Integer;
begin
  if FWebView <> nil then
  begin
    LHeight := Round(FWebView.getContentHeight * FWebView.getScale + 0.5);
    LJBitmap := TJBitmap.JavaClass.createBitmap(FWebView.getWidth, LHeight, TJBitmap_Config.JavaClass.ARGB_8888);
    FWebView.draw(TJCanvas.JavaClass.init(LJBitmap));
    LBitmap := TBitmap.Create;
    try
      LBitmap.FromJBitmap(LJBitmap);
      BitmapCaptured(LBitmap);
    finally
      LBitmap.Free;
    end;
  end
  else
    inherited;
end;

procedure TPlatformWebBrowserExt.SetAllowZoom(const AValue: Boolean);
begin
  if FWebView <> nil then
    FWebView.getSettings.setUseWideViewPort(AValue);
end;

procedure TPlatformWebBrowserExt.SetInitialScale(const AValue: Integer);
begin
  if FWebView <> nil then
    FWebView.setInitialScale(AValue);
end;

procedure TPlatformWebBrowserExt.ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc);
begin
  if FWebView <> nil then
    FWebView.evaluateJavascript(StringToJString(AJavaScript), CreateValueCallback(AHandler))
  else
    AHandler(cJavaScriptNullResult, -1);
end;

// https://stackoverflow.com/a/70711583/3164070
procedure TPlatformWebBrowserExt.FlushCookies(const ARemove: Boolean);
var
  LCookieManager: JCookieManager;
begin
  LCookieManager := TJCookieManager.JavaClass.getInstance;
  if ARemove then
    LCookieManager.removeAllCookies(nil);
  LCookieManager.flush;
end;

procedure TPlatformWebBrowserExt.Navigate(const AURL: string);
begin
  if FWebView <> nil then
    FWebView.loadUrl(StringToJString(AURL));
end;

class function TPlatformWebBrowserExt.GetHitTestKind(const AType: Integer): THitTestKind;
begin
  if AType = TJWebView_HitTestResult.JavaClass.EDIT_TEXT_TYPE then
    Result := THitTestKind.EditText
  else if AType = TJWebView_HitTestResult.JavaClass.EMAIL_TYPE then
    Result := THitTestKind.Email
  else if AType = TJWebView_HitTestResult.JavaClass.GEO_TYPE then
    Result := THitTestKind.Geo
  else if AType = TJWebView_HitTestResult.JavaClass.IMAGE_TYPE then
    Result := THitTestKind.Image
  else if AType = TJWebView_HitTestResult.JavaClass.PHONE_TYPE then
    Result := THitTestKind.Phone
  else if AType = TJWebView_HitTestResult.JavaClass.SRC_ANCHOR_TYPE then
    Result := THitTestKind.SrcAnchor
  else if AType = TJWebView_HitTestResult.JavaClass.SRC_IMAGE_ANCHOR_TYPE then
    Result := THitTestKind.SrcImageAnchor
  else
    Result := THitTestKind.Unknown;
end;

function TPlatformWebBrowserExt.GetPrintAdapter(const ADocumentName: string): IInterface;
begin
  if FWebView <> nil then
    Result := FWebView.createPrintDocumentAdapter(StringToJString(ADocumentName))
  else
    Result := nil;
end;

function TPlatformWebBrowserExt.WebViewTouch(const AView: JView; const AEvent: JMotionEvent): Boolean;
var
  LHitTestResult: JWebView_HitTestResult;
  LPreventDefault: Boolean;
  LExtra: string;
begin
  Result := False;
  if AEvent.getAction = TJMotionEvent.JavaClass.ACTION_UP then
  begin
    LPreventDefault := False;
    LHitTestResult := TJWebView.Wrap(AView).getHitTestResult;
    if LHitTestResult <> nil then
    begin
      LExtra := '';
      if LHitTestResult.getExtra <> nil then
        LExtra := JStringToString(LHitTestResult.getExtra);
      DoElementClick(GetHitTestKind(LHitTestResult.getType), LExtra, LPreventDefault);
    end
    else
      DoElementClick(THitTestKind.Unknown, '', LPreventDefault);
    Result := LPreventDefault; // i.e. the app has handled the event itself
  end;
end;

end.
