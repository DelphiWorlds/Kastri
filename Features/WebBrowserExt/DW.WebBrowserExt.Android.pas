unit DW.WebBrowserExt.Android;

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
  System.Classes, System.Generics.Collections,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.WebKit, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero,
  Androidapi.JNI.Os, Androidapi.JNI.Net,
  // FMX
  FMX.Graphics,
  // DW
  DW.WebBrowserExt, DW.JavaScript, DW.Androidapi.JNI.App, DW.MultiReceiver.Android, DW.Androidapi.JNI.AndroidX.WebKit;

type
  TPlatformWebBrowserExt = class;

  TWebMessageListener = class(TJavaLocal, JWebViewCompat_WebMessageListener)
  private
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
  public
    { JWebViewCompat_WebMessageListener }
    procedure onPostMessage(webView: JWebView; webMessageCompat: JWebMessageCompat; uri: Jnet_Uri; b: Boolean; javaScriptReplyProxy: JJavaScriptReplyProxy); cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
  end;

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

  TDownloadListener = class(TJavaLocal, JDownloadListener)
  private
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
  public
    { JDownloadListener }
    procedure onDownloadStart(url: JString; userAgent: JString; contentDisposition: JString; mimetype: JString; contentLength: Int64); cdecl;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
  end;

  TDownloadCompleteReceiver = class(TMultiReceiver)
  private
    FPlatformWebBrowserExt: TPlatformWebBrowserExt;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
  end;

  TDownloads = TDictionary<Int64, String>;

  TPlatformWebBrowserExt = class(TCustomPlatformWebBrowserExt)
  private
    class function GetHitTestKind(const AType: Integer): THitTestKind;
  private
    FDownloadCompleteReceiver: TDownloadCompleteReceiver;
    FDownloadListener: JDownloadListener;
    FDownloadManager: JDownloadManager;
    FDownloads: TDownloads;
    FTouchListener: JView_OnTouchListener;
    FValueCallbacks: TArray<JValueCallback>;
    FWebView: JWebView;
    FWebMessageListener: JWebViewCompat_WebMessageListener;
    function CreateValueCallback(const AHandler: TJavaScriptResultProc): JValueCallback;
    procedure RemoveValueCallback(const ACallback: JValueCallback);
  protected
    procedure ClearCache(const ADataKinds: TCacheDataKinds); override;
    procedure DoCaptureBitmap; override;
    procedure DownloadStateChange(const AContext: JContext; const AIntent: JIntent);
    procedure DownloadStart(const AUrl, AUserAgent, AContentDisposition, AMimetype: JString; const AContentLength: Int64);
    procedure ExecuteJavaScript(const AJavaScript: string; const AHandler: TJavaScriptResultProc); override;
    procedure FlushCookies(const ARemove: Boolean); override;
    function GetPrintAdapter(const ADocumentName: string): IInterface; override;
    procedure Navigate(const AURL: string); override;
    procedure ResetZoom; override;
    procedure SetAllowZoom(const AValue: Boolean); override;
    procedure SetInitialScale(const AValue: Integer); override;
    procedure WebListenerParamsUpdated; override;
    procedure WebViewPostMessage(const AView: JWebView; const AMessage: JWebMessageCompat; const ASourceOrigin: Jnet_Uri; const AIsMainFrame: Boolean;
      const AReplyProxy: JJavaScriptReplyProxy);
    function WebViewTouch(const AView: JView; const AEvent: JMotionEvent): Boolean;
  public
    constructor Create(const AWebBrowserExt: TWebBrowserExt); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Print,
  // DW
  DW.OSLog, System.TypInfo,
  DW.Graphics.Helpers.Android;

{ TWebMessageListener }

constructor TWebMessageListener.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
end;

procedure TWebMessageListener.onPostMessage(webView: JWebView; webMessageCompat: JWebMessageCompat; uri: Jnet_Uri; b: Boolean;
  javaScriptReplyProxy: JJavaScriptReplyProxy);
begin
  FPlatformWebBrowserExt.WebViewPostMessage(webView, webMessageCompat, uri, b, javaScriptReplyProxy);
end;

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

{ TDownloadListener }

constructor TDownloadListener.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
end;

procedure TDownloadListener.onDownloadStart(url, userAgent, contentDisposition, mimetype: JString; contentLength: Int64);
begin
  FPlatformWebBrowserExt.DownloadStart(url, userAgent, contentDisposition, mimetype, contentLength);
end;

{ TDownloadCompleteReceiver }

constructor TDownloadCompleteReceiver.Create(const APlatformWebBrowserExt: TPlatformWebBrowserExt);
begin
  inherited Create;
  FPlatformWebBrowserExt := APlatformWebBrowserExt;
end;

procedure TDownloadCompleteReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJDownloadManager.JavaClass.ACTION_DOWNLOAD_COMPLETE);
end;

procedure TDownloadCompleteReceiver.Receive(context: JContext; intent: JIntent);
begin
  FPlatformWebBrowserExt.DownloadStateChange(context, intent);
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
    FDownloadManager := TJDownloadManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.DOWNLOAD_SERVICE));
    FDownloads := TDownloads.Create;
    FDownloadCompleteReceiver := TDownloadCompleteReceiver.Create(Self);
    FDownloadListener := TDownloadListener.Create(Self);
    FWebView.setDownloadListener(FDownloadListener);
  end;
end;

destructor TPlatformWebBrowserExt.Destroy;
begin
  FDownloadCompleteReceiver.Free;
  FDownloads.Free;
  inherited;
end;

procedure TPlatformWebBrowserExt.WebListenerParamsUpdated;
var
  LAllowedOrigins: JHashSet;
  LOrigin: string;
begin
  if FWebMessageListener <> nil then
    TJWebViewCompat.JavaClass.removeWebMessageListener(FWebView, StringToJString(JavaScriptObjectName));
  FWebMessageListener := nil;
  if TJWebViewFeature.JavaClass.isFeatureSupported(TJWebViewFeature.JavaClass.WEB_MESSAGE_LISTENER) then
  begin
    FWebMessageListener := TWebMessageListener.Create(Self);
    LAllowedOrigins := TJHashSet.JavaClass.init;
    for LOrigin in AllowedMessageOrigins do
      LAllowedOrigins.add(StringToJString(LOrigin));
    TJWebViewCompat.JavaClass.addWebMessageListener(FWebView, StringToJString(JavaScriptObjectName), TJSet.Wrap(LAllowedOrigins), FWebMessageListener);
  end
  else
    TOSLog.e('WEB_MESSAGE_LISTENER is not supported');
end;

procedure TPlatformWebBrowserExt.WebViewPostMessage(const AView: JWebView; const AMessage: JWebMessageCompat; const ASourceOrigin: Jnet_Uri;
  const AIsMainFrame: Boolean; const AReplyProxy: JJavaScriptReplyProxy);
var
  LDataString, LReply: string;
begin
  // AMessage.getPorts // <---- Requires Java code to create a descendant of WebMessagePortCompat.WebMessageCallbackCompat
  // AMessage.getArrayBuffer // <---- Not handled here. Maybe in the future
  if AMessage.getType = TJWebMessageCompat.JavaClass.TYPE_STRING then
  begin
    LDataString := JStringToString(AMessage.getData);
    LReply := '';
    DoStringMessagePosted(LDataString, LReply);
    if not LReply.IsEmpty then
      AReplyProxy.postMessage(StringToJString(LReply));
  end;
end;

procedure TPlatformWebBrowserExt.ClearCache(const ADataKinds: TCacheDataKinds);
var
  LHasAll, LHasAnyStorage: Boolean;
begin
  if FWebView <> nil then
  begin
    LHasAll := ADataKinds = [];
    if not LHasAll then
    begin
      if TCacheDataKind.Cookies in ADataKinds then
        FlushCookies(True);
      LHasAnyStorage := (ADataKinds * [TCacheDataKind.LocalStorage, TCacheDataKind.SessionStorage,
        TCacheDataKind.IndexedDBDatabases, TCacheDataKind.WebSQLDatabases]) <> [];
      if LHasAnyStorage then
        TJWebStorage.JavaClass.getInstance.deleteAllData;
    end
    else
      FWebView.clearCache(True);
  end;
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

procedure TPlatformWebBrowserExt.DownloadStart(const AUrl, AUserAgent, AContentDisposition, AMimetype: JString; const AContentLength: Int64);
var
  LDownloadRequest: JDownloadManager_Request;
  LFile, LDir: JFile;
  LFileName: string;
begin
  LFileName := '';
  DoDownloadStart(JStringToString(AUrl), JStringToString(AMimeType), LFileName);
  if not LFileName.IsEmpty then
  begin
    LDownloadRequest := TJDownloadManager_Request.JavaClass.init(TJnet_uri.JavaClass.parse(AUrl));
    // Might not want this visible? Use a property in TWebBrowserExt?
    LDownloadRequest.setNotificationVisibility(TJDownloadManager_Request.JavaClass.VISIBILITY_VISIBLE_NOTIFY_COMPLETED);
    if TOSVersion.Check(10) then
      LDir := TAndroidHelper.Context.getExternalFilesDir(TJEnvironment.JavaClass.DIRECTORY_DOWNLOADS)
    else
      LDir := TJEnvironment.JavaClass.getExternalStoragePublicDirectory(TJEnvironment.JavaClass.DIRECTORY_DOWNLOADS);
    LFile := TJFile.JavaClass.init(LDir, StringToJString(TPath.GetFileName(LFileName)));
    LDownloadRequest.setDestinationUri(TJnet_Uri.JavaClass.fromFile(LFile));
    FDownloads.Add(FDownloadManager.enqueue(LDownloadRequest), LFileName);
  end;
end;

procedure TPlatformWebBrowserExt.DownloadStateChange(const AContext: JContext; const AIntent: JIntent);
var
  LDownloadID: Int64;
  LFileName, LLocalFileName: string;
  LState: TDownloadState;
  LCursor: JCursor;
  LQuery: JDownloadManager_Query;
  LStatus: Integer;
  LUriString: JString;
  LIds: TJavaArray<Int64>;
begin
  LDownloadID := AIntent.getLongExtra(TJDownloadManager.JavaClass.EXTRA_DOWNLOAD_ID, -1);
  if FDownloads.TryGetValue(LDownloadID, LFileName) then
  begin
    LState := TDownloadState.Unknown;
    LQuery := TJDownloadManager_Query.JavaClass.init;
    LIds := TJavaArray<Int64>.Create(1);
    try
      LIds.Items[0] := LDownloadID;
      LQuery := LQuery.setFilterById(LIds);
    finally
      LIds.Free;
    end;
    LCursor := FDownloadManager.query(LQuery);
    try
      if LCursor.moveToFirst then
      begin
        LStatus := LCursor.getInt(LCursor.getColumnIndex(TJDownloadManager.JavaClass.COLUMN_STATUS));
        if LStatus = TJDownloadManager.JavaClass.STATUS_SUCCESSFUL then
          LState := TDownloadState.Completed
        else if LStatus = TJDownloadManager.JavaClass.STATUS_PAUSED then
          LState := TDownloadState.Paused
        else if LStatus = TJDownloadManager.JavaClass.STATUS_FAILED then
          LState := TDownloadState.Failed
        else if LStatus = TJDownloadManager.JavaClass.STATUS_PENDING then
          LState := TDownloadState.Pending
        else if LStatus = TJDownloadManager.JavaClass.STATUS_RUNNING then
          LState := TDownloadState.Running;
        LUriString := LCursor.getString(LCursor.getColumnIndex(TJDownloadManager.JavaClass.COLUMN_LOCAL_URI));
      end;
    finally
      LCursor.close;
    end;
    if LState = TDownloadState.Completed then
    begin
      LLocalFileName := JStringToString(TJnet_Uri.JavaClass.parse(LUriString).getPath);
      if TPath.GetDirectoryName(LFileName).IsEmpty and not DefaultDownloadsFolder.IsEmpty and ForceDirectories(DefaultDownloadsFolder) then
        LFileName := TPath.Combine(DefaultDownloadsFolder, LFileName);
      if not TPath.GetDirectoryName(LFileName).IsEmpty then
      begin
        try
          TFile.Copy(LLocalFileName, LFileName, True);
          if TFile.Exists(LFileName) then
            TFile.Delete(LLocalFileName);
        except
          // If the "move" fails, the path will be what the download uri is
        end;
        if not TFile.Exists(LFileName) then
          LFileName := LLocalFileName;
      end;
      FDownloads.Remove(LDownloadID);
    end;
    DoDownloadStateChange(LFileName, LState);
  end;
  // else no matching download id???
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
var
  LHeaders: JMap;
begin
  if FWebView <> nil then
  begin
    // https://stackoverflow.com/a/14545315/3164070
    if not Browser.EnableCaching then
    begin
      LHeaders := TJMap.Wrap(TJHashMap.JavaClass.init(2));
      LHeaders.put(StringToJString('Pragma'), StringToJString('no-cache'));
      LHeaders.put(StringToJString('Cache-Control'), StringToJString('no-cache'));
      FWebView.loadUrl(StringToJString(AURL), LHeaders);
    end
    else
      FWebView.loadUrl(StringToJString(AURL));
  end;
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
