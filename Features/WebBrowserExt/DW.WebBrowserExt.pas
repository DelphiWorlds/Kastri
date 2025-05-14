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
  System.Classes, System.Types,
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

  TDownloadState = (Completed, Failed, Paused, Pending, Running, Unknown);

  TDownloadableMimeTypes = record
  private
    Values: TArray<string>;
  public
    procedure Add(const AValues: TArray<string>);
    procedure Clear;
    procedure Remove(const AValues: TArray<string>);
  end;

  TCustomPlatformWebBrowserExt = class(TObject)
  private
    FCaptureBitmapHandler: TCaptureBitmapProc;
    FWebBrowserExt: TWebBrowserExt;
    function GetBrowser: TWebBrowser;
    function GetDefaultDownloadsFolder: string;
    function GetDownloadableMimeTypes: TArray<string>;
  protected
    procedure BitmapCaptured(const ABitmap: TBitmap);
    procedure CaptureBitmap(const AHandler: TCaptureBitmapProc);
    procedure ClearCache(const ADataKinds: TCacheDataKinds); virtual;
    procedure DoCaptureBitmap; virtual;
    procedure DoDownloadStateChange(const AFileName: string; const AState: TDownloadState);
    procedure DoDownloadStart(const AUri, AMimeType: string; var AFileName: string);
    procedure DoElementClick(const AHitTestKind: THitTestKind; const AExtra: string; var APreventDefault: Boolean); virtual;
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc); virtual;
    procedure FlushCookies(const ARemove: Boolean); virtual;
    procedure GetElementValueByName(const AName: string; const AHandler: TJavaScriptResultProc); virtual;
    function GetPrintAdapter(const ADocumentName: string): IInterface; virtual;
    procedure Navigate(const AURL: string); virtual;
    procedure ResetZoom; virtual;
    procedure SetAllowZoom(const AValue: Boolean); virtual;
    procedure SetInitialScale(const AValue: Integer); virtual;
    procedure SimulateClick(const APoint: TPointF); virtual;
    property DefaultDownloadsFolder: string read GetDefaultDownloadsFolder;
    property DownloadableMimeTypes: TArray<string> read GetDownloadableMimeTypes;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); virtual;
    property Browser: TWebBrowser read GetBrowser;
  end;

  TElementClickEvent = procedure(Sender: TObject; const HitTestKind: THitTestKind; const Extra: string; var PreventDefault: Boolean) of object;
  TDownloadStartEvent = procedure(Sender: TObject; const Uri, FileExt: string; var FileName: string) of object;
  TDownloadStateChangeEvent = procedure(Sender: TObject; const FileName: string; const State: TDownloadState) of object;

  TWebBrowserExt = class(TComponent)
  private
    FBrowser: TWebBrowser;
    FDefaultDownloadsFolder: string;
    FDownloadableMimeTypes: TDownloadableMimeTypes;
    FPlatformWebBrowserExt: TCustomPlatformWebBrowserExt;
    FOnDownloadStateChange: TDownloadStateChangeEvent;
    FOnDownloadStart: TDownloadStartEvent;
    FOnElementClick: TElementClickEvent;
    procedure AddDownloadableMimeTypes;
  protected
    procedure DoDownloadStateChange(const AFileName: string; const AState: TDownloadState);
    procedure DoDownloadStart(const AUri, AFileExt: string; var AFileName: string);
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
    procedure SimulateClick(const APoint: TPointF);
    property DefaultDownloadsFolder: string read FDefaultDownloadsFolder write FDefaultDownloadsFolder;
    /// <summary>
    ///   Mime types used in macOS implementation for determining whether the content should be included for downloads
    /// </summary>
    property DownloadableMimeTypes: TDownloadableMimeTypes read FDownloadableMimeTypes;
    property OnDownloadStart: TDownloadStartEvent read FOnDownloadStart write FOnDownloadStart;
    property OnDownloadStateChange: TDownloadStateChangeEvent read FOnDownloadStateChange write FOnDownloadStateChange;
    property OnElementClick: TElementClickEvent read FOnElementClick write FOnElementClick;
  end;

implementation

uses
  System.SysUtils, System.Net.Mime, System.IOUtils, System.StrUtils,

  DW.OSLog,

  DW.IOUtils.Helpers,
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

{ TDownloadableMimeTypes }

procedure TDownloadableMimeTypes.Add(const AValues: TArray<string>);
var
  LValue: string;
begin
  for LValue in AValues do
  begin
    if not MatchText(LValue, Values) then
      Values := Values + [LValue];
  end;
end;

procedure TDownloadableMimeTypes.Clear;
begin
  Values := [];
end;

procedure TDownloadableMimeTypes.Remove(const AValues: TArray<string>);
var
  LValue: string;
  LIndex: Integer;
begin
  for LValue in AValues do
  begin
    LIndex := IndexText(LValue, Values);
    if LIndex > -1 then
      Delete(Values, LIndex, 1);
  end;
end;

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

procedure TCustomPlatformWebBrowserExt.DoDownloadStateChange(const AFileName: string; const AState: TDownloadState);
begin
  FWebBrowserExt.DoDownloadStateChange(AFileName, AState);
end;

procedure TCustomPlatformWebBrowserExt.DoDownloadStart(const AUri, AMimeType: string; var AFileName: string);
var
  LKind: TMimeTypes.TKind;
  LFileExt: string;
begin
  if TMimeTypes.Default.GetTypeInfo(AMimeType, LFileExt, LKind) then
    LFileExt := '.' + LFileExt;
  FWebBrowserExt.DoDownloadStart(AUri, LFileExt, AFileName);
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

function TCustomPlatformWebBrowserExt.GetDefaultDownloadsFolder: string;
begin
  Result := FWebBrowserExt.DefaultDownloadsFolder;
end;

function TCustomPlatformWebBrowserExt.GetDownloadableMimeTypes: TArray<string>;
begin
  Result := FWebBrowserExt.DownloadableMimeTypes.Values;
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

procedure TCustomPlatformWebBrowserExt.SimulateClick(const APoint: TPointF);
var
  LNativePoint: TPoint;
begin
  LNativePoint := APoint.Round;
  ExecuteJavaScript(Format(cJavaScriptClickAtXY, [LNativePoint.X, LNativePoint.Y]), nil);
end;

{ TWebBrowserExt }

constructor TWebBrowserExt.Create(AOwner: TComponent);
begin
  inherited;
  if AOwner is TWebBrowser then
    FBrowser := TWebBrowser(AOwner);
  FPlatformWebBrowserExt := TPlatformWebBrowserExt.Create(Self);
  {$IF Defined(IOS) or Defined(ANDROID)}
  FDefaultDownloadsFolder := TPath.Combine(TPath.GetDocumentsPath, 'Downloads');
  {$ELSE}
  FDefaultDownloadsFolder := TPath.Combine(TPath.Combine(TPath.GetSharedDocumentsPath, TPathHelper.GetAppName), 'Downloads');
  {$ENDIF}
end;

procedure TWebBrowserExt.AddDownloadableMimeTypes;
begin
  FDownloadableMimeTypes.Values := [
    'application/gzip',
    'application/java-archive',
    'application/msword',
    'application/octet-stream',
    'application/pdf',
    'application/rtf',
    'application/vnd.android.package-archive',
    'application/vnd.ms-excel',
    'application/vnd.ms-powerpoint',
    'application/vnd.oasis.opendocument.spreadsheet',
    'application/vnd.oasis.opendocument.text',
    'application/vnd.openxmlformats-officedocument.presentationml.presentation',
    'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
    'application/x-bzip2',
    'application/x-csh',
    'application/x-disk-image',
    'application/x-font-otf',
    'application/x-font-ttf',
    'application/x-iso9660-image',
    'application/x-msdownload',
    'application/x-rar-compressed',
    'application/x-sh',
    'application/x-tar',
    'application/x-7z-compressed',
    'application/zip',
    'audio/flac',
    'audio/mpeg',
    'audio/ogg',
    'audio/wav',
    'audio/x-aac',
    'font/woff',
    'font/woff2',
    'image/bmp',
    'image/gif',
    'image/jpeg',
    'image/png',
    'image/tiff',
    'image/webp',
    'text/csv',
    'video/mp4',
    'video/quicktime',
    'video/webm',
    'video/x-flv',
    'video/x-matroska',
    'video/x-msvideo'
  ];
end;

procedure TWebBrowserExt.CaptureBitmap(const AHandler: TCaptureBitmapProc);
begin
  FPlatformWebBrowserExt.CaptureBitmap(AHandler);
end;

procedure TWebBrowserExt.ClearCache(const ADataKinds: TCacheDataKinds = []);
begin
  FPlatformWebBrowserExt.ClearCache(ADataKinds);
end;

procedure TWebBrowserExt.DoDownloadStateChange(const AFileName: string; const AState: TDownloadState);
begin
  if Assigned(FOnDownloadStateChange) then
    FOnDownloadStateChange(Self, AFileName, AState);
end;

procedure TWebBrowserExt.DoDownloadStart(const AUri, AFileExt: string; var AFileName: string);
begin
  if Assigned(FOnDownloadStart) then
    FOnDownloadStart(Self, AUri, AFileExt, AFileName);
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

procedure TWebBrowserExt.SimulateClick(const APoint: TPointF);
begin
  FPlatformWebBrowserExt.SimulateClick(APoint);
end;

end.
