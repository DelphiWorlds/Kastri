unit DW.WebBrowserExt;

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

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Classes,
  // FMX
  FMX.WebBrowser, FMX.Graphics,
  // DW
  DW.JavaScript;

type
  TPrintJobStatus = (None, Blocked, Cancelled, Completed, Failed, Queued, Started);

  THitTestKind = (Unknown, EditText, Email, Geo, Image, Phone, SrcAnchor, SrcImageAnchor);

  TCacheDataKind = (Cookies, LocalStorage, SessionStorage, IndexedDBDatabases, WebSQLDatabases);

  TCacheDataKinds = set of TCacheDataKind;

  TCaptureBitmapProc = reference to procedure(const ABitmap: TBitmap);

  TWebBrowserExt = class;

  TCustomPlatformWebBrowserExt = class(TObject)
  private
    FCaptureBitmapHandler: TCaptureBitmapProc;
    FWebBrowserExt: TWebBrowserExt;
    function GetBrowser: TWebBrowser;
  protected
    procedure BitmapCaptured(const ABitmap: TBitmap);
    procedure CaptureBitmap(const AHandler: TCaptureBitmapProc);
    procedure ClearCache(const ADataKinds: TCacheDataKinds); virtual;
    procedure DoCaptureBitmap; virtual;
    procedure DoElementClick(const AHitTestKind: THitTestKind; const AExtra: string; var APreventDefault: Boolean); virtual;
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc); virtual;
    procedure FlushCookies(const ARemove: Boolean); virtual;
    procedure GetElementValueByName(const AName: string; const AHandler: TJavaScriptResultProc); virtual;
    function GetPrintAdapter(const ADocumentName: string): IInterface; virtual;
    procedure Navigate(const AURL: string); virtual;
    procedure ResetZoom; virtual;
    procedure SetAllowZoom(const AValue: Boolean); virtual;
    procedure SetInitialScale(const AValue: Integer); virtual;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); virtual;
    property Browser: TWebBrowser read GetBrowser;
  end;

  TElementClickEvent = procedure(Sender: TObject; const HitTestKind: THitTestKind; const Extra: string; var PreventDefault: Boolean) of object;

  TWebBrowserExt = class(TComponent)
  private
    FBrowser: TWebBrowser;
    FPlatformWebBrowserExt: TCustomPlatformWebBrowserExt;
    FOnElementClick: TElementClickEvent;
  protected
    procedure DoElementClick(const AHitTestKind: THitTestKind; const AExtra: string; var APreventDefault: Boolean);
    property Browser: TWebBrowser read FBrowser;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CaptureBitmap(const AHandler: TCaptureBitmapProc);
    procedure ClearCache(const ADataKinds: TCacheDataKinds = []);
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc = nil);
    procedure GetElementValueByName(const AName: string; const AHandler: TJavaScriptResultProc);
    function GetPrintAdapter(const ADocumentName: string): IInterface;
    procedure FlushCookies(const ARemove: Boolean = False);
    procedure Navigate(const AURL: string);
    procedure ResetZoom;
    procedure SetAllowZoom(const AValue: Boolean);
    procedure SetInitialScale(const AValue: Integer);
    property OnElementClick: TElementClickEvent read FOnElementClick write FOnElementClick;
  end;

implementation

uses
  System.SysUtils,
{$IF Defined(ANDROID)}
  DW.WebBrowserExt.Android;
{$ENDIF}
{$IF Defined(IOS)}
  DW.WebBrowserExt.iOS;
{$ELSEIF Defined(MACOS)}
  DW.WebBrowserExt.Mac;
{$ENDIF}
{$IF Defined(MSWINDOWS)}
  DW.WebBrowserExt.Win;
{$ENDIF}

{ TCustomPlatformWebBrowserExt }

constructor TCustomPlatformWebBrowserExt.Create(const AWebBrowserExt: TWebBrowserExt);
begin
  inherited Create;
  FWebBrowserExt := AWebBrowserExt;
end;

procedure TCustomPlatformWebBrowserExt.BitmapCaptured(const ABitmap: TBitmap);
begin
  if Assigned(FCaptureBitmapHandler) then
    FCaptureBitmapHandler(ABitmap);
  FCaptureBitmapHandler := nil;
end;

procedure TCustomPlatformWebBrowserExt.CaptureBitmap(const AHandler: TCaptureBitmapProc);
begin
  if not Assigned(FCaptureBitmapHandler) then
  begin
    FCaptureBitmapHandler := AHandler;
    DoCaptureBitmap;
  end;
end;

procedure TCustomPlatformWebBrowserExt.ClearCache(const ADataKinds: TCacheDataKinds);
begin
  //
end;

procedure TCustomPlatformWebBrowserExt.DoCaptureBitmap;
begin
  BitmapCaptured(nil);
end;

procedure TCustomPlatformWebBrowserExt.DoElementClick(const AHitTestKind: THitTestKind; const AExtra: string; var APreventDefault: Boolean);
begin
  FWebBrowserExt.DoElementClick(AHitTestKind, AExtra, APreventDefault);
end;

procedure TCustomPlatformWebBrowserExt.ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc);
begin
  //
end;

procedure TCustomPlatformWebBrowserExt.FlushCookies(const ARemove: Boolean);
begin
  //
end;

function TCustomPlatformWebBrowserExt.GetBrowser: TWebBrowser;
begin
  Result := FWebBrowserExt.Browser;
end;

procedure TCustomPlatformWebBrowserExt.GetElementValueByName(const AName: string; const AHandler: TJavaScriptResultProc);
begin
  ExecuteJavaScript(Format(cJavaScriptGetInputValueByName, [AName]), AHandler);
end;

procedure TCustomPlatformWebBrowserExt.Navigate(const AURL: string);
begin
  // If not overridden in descendants, do the default
  if not AURL.ToLower.Equals('about:blank') then
    Browser.Navigate(AURL);
end;

function TCustomPlatformWebBrowserExt.GetPrintAdapter(const ADocumentName: string): IInterface;
begin
  Result := nil;
end;

procedure TCustomPlatformWebBrowserExt.ResetZoom;
begin
  //
end;

procedure TCustomPlatformWebBrowserExt.SetAllowZoom(const AValue: Boolean);
begin
  //
end;

procedure TCustomPlatformWebBrowserExt.SetInitialScale(const AValue: Integer);
begin
  //
end;

{ TWebBrowserExt }

procedure TWebBrowserExt.CaptureBitmap(const AHandler: TCaptureBitmapProc);
begin
  FPlatformWebBrowserExt.CaptureBitmap(AHandler);
end;

procedure TWebBrowserExt.ClearCache(const ADataKinds: TCacheDataKinds = []);
begin
  FPlatformWebBrowserExt.ClearCache(ADataKinds);
end;

constructor TWebBrowserExt.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TWebBrowser then
    FBrowser := TWebBrowser(AOwner);
  FPlatformWebBrowserExt := TPlatformWebBrowserExt.Create(Self);
end;

procedure TWebBrowserExt.DoElementClick(const AHitTestKind: THitTestKind; const AExtra: string; var APreventDefault: Boolean);
begin
  if Assigned(FOnElementClick) then
    FOnElementClick(Self, AHitTestKind, AExtra, APreventDefault);
end;

procedure TWebBrowserExt.ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc = nil);
begin
  FPlatformWebBrowserExt.ExecuteJavaScript(AJavaScript, AHandler);
end;

procedure TWebBrowserExt.FlushCookies(const ARemove: Boolean = False);
begin
  FPlatformWebBrowserExt.FlushCookies(ARemove);
end;

procedure TWebBrowserExt.GetElementValueByName(const AName: string; const AHandler: TJavaScriptResultProc);
begin
  ExecuteJavaScript(Format(cJavaScriptGetInputValueByName, [AName]), AHandler);
end;

procedure TWebBrowserExt.Navigate(const AURL: string);
begin
  FPlatformWebBrowserExt.Navigate(AURL);
end;

function TWebBrowserExt.GetPrintAdapter(const ADocumentName: string): IInterface;
begin
  Result := FPlatformWebBrowserExt.GetPrintAdapter(ADocumentName);
end;

procedure TWebBrowserExt.ResetZoom;
begin
  FPlatformWebBrowserExt.ResetZoom;
end;

procedure TWebBrowserExt.SetAllowZoom(const AValue: Boolean);
begin
  FPlatformWebBrowserExt.SetAllowZoom(AValue);
end;

procedure TWebBrowserExt.SetInitialScale(const AValue: Integer);
begin
  FPlatformWebBrowserExt.SetInitialScale(AValue);
end;

end.
