unit DW.Printing.Android;

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
  // Android
  Androidapi.JNI.Print, Androidapi.JNIBridge, Androidapi.JNI.Os, Androidapi.JNI.Net, Androidapi.JNI.Webkit, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
	// DW
  DW.Printing, DW.Androidapi.JNI.Print;

type
  JDWWebViewClient = interface;
  JDWWebViewClientDelegate = interface;

  JDWWebViewClientClass = interface(JWebViewClientClass)
   ['{8CA4282E-E8AA-4CDD-83F8-5826485AB88D}']
   function init(delegate: JDWWebViewClientDelegate): JDWWebViewClient; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWWebViewClient')]
  JDWWebViewClient = interface(JWebViewClient)
    ['{51AA207D-E6C8-4235-ABD2-E33036084CC4}']
  end;
  TJDWWebViewClient = class(TJavaGenericImport<JDWWebViewClientClass, JDWWebViewClient>) end;

  JDWWebViewClientDelegateClass = interface(IJavaClass)
   ['{F9B335B5-BC4D-4FC9-B205-30AB616B3AFB}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWWebViewClientDelegate')]
  JDWWebViewClientDelegate = interface(IJavaInstance)
    ['{7C52C597-319C-44CE-93C0-60C7D010640A}']
    procedure doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean); cdecl;
    procedure onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage); cdecl;
    procedure onLoadResource(view: JWebView; url: JString); cdecl;
    procedure onPageFinished(view: JWebView; url: JString); cdecl;
    procedure onPageStarted(view: JWebView; url: JString; favicon: JBitmap); cdecl;
    procedure onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString); cdecl;
    procedure onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString); cdecl;
    procedure onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError); cdecl;
    procedure onScaleChanged(view: JWebView; oldScale: Single; newScale: Single); cdecl;
    procedure onUnhandledKeyEvent(view: JWebView; event: JKeyEvent); cdecl;
    function shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean; cdecl;
    function shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean; cdecl;
  end;
  TJDWWebViewClientDelegate = class(TJavaGenericImport<JDWWebViewClientDelegateClass, JDWWebViewClientDelegate>) end;

  TPlatformPrinting = class(TInterfacedObject, IPrinting)
  private
    FDocumentName: string;
    FPrintManager: JPrintManager;
    FPrintJobs: TArray<JPrintJob>;
    FURL: JString;
    FWebView: JWebView;
    FWebViewClient: JWebViewClient;
    FWebViewClientDelegate: JDWWebViewClientDelegate;
    procedure CreatePrintManager;
    procedure PrintPDF(const AFileName: string);
    procedure WebViewPageFinished(const AURL: JString);
  public
    constructor Create;
    function GetPrintStatus(const AIndex: Integer): TPrintJobStatus;
    function Print(const AAdapter: IInterface): Integer; overload;
    function Print(const AFileName: string): Integer; overload;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
	// Android
  Androidapi.Helpers,
	// DW
  DW.OSLog,
  DW.OSDevice;

type
  TWebViewClientDelegate = class(TJavaLocal, JDWWebViewClientDelegate)
  private
    FPlatformPrinting: TPlatformPrinting;
  public
    { JDWWebViewClientDelegate }
    procedure doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean); cdecl;
    procedure onFormResubmission(view: JWebView; dontResend: JMessage; resend: JMessage); cdecl;
    procedure onLoadResource(view: JWebView; url: JString); cdecl;
    procedure onPageFinished(view: JWebView; url: JString); cdecl;
    procedure onPageStarted(view: JWebView; url: JString; favicon: JBitmap); cdecl;
    procedure onReceivedError(view: JWebView; errorCode: Integer; description: JString; failingUrl: JString); cdecl;
    procedure onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host: JString; realm: JString); cdecl;
    procedure onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError); cdecl;
    procedure onScaleChanged(view: JWebView; oldScale: Single; newScale: Single); cdecl;
    procedure onUnhandledKeyEvent(view: JWebView; event: JKeyEvent); cdecl;
    function shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean; cdecl;
    function shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean; cdecl;
  public
    constructor Create(const APlatformPrinting: TPlatformPrinting);
  end;

{ TWebViewClientDelegate }

constructor TWebViewClientDelegate.Create(const APlatformPrinting: TPlatformPrinting);
begin
  inherited Create;
  FPlatformPrinting := APlatformPrinting;
end;

procedure TWebViewClientDelegate.doUpdateVisitedHistory(view: JWebView; url: JString; isReload: Boolean);
begin
  //
end;

procedure TWebViewClientDelegate.onFormResubmission(view: JWebView; dontResend, resend: JMessage);
begin
  //
end;

procedure TWebViewClientDelegate.onLoadResource(view: JWebView; url: JString);
begin
  //
end;

procedure TWebViewClientDelegate.onPageFinished(view: JWebView; url: JString);
begin
  TOSLog.d('TWebViewClientDelegate.onPageFinished: %s', [JStringToString(url)]);
  FPlatformPrinting.WebViewPageFinished(url);
end;

procedure TWebViewClientDelegate.onPageStarted(view: JWebView; url: JString; favicon: JBitmap);
begin
  //
end;

procedure TWebViewClientDelegate.onReceivedError(view: JWebView; errorCode: Integer; description, failingUrl: JString);
begin
  TOSLog.d('TWebViewClientDelegate.onReceivedError: %s', [JStringToString(description)]);
end;

procedure TWebViewClientDelegate.onReceivedHttpAuthRequest(view: JWebView; handler: JHttpAuthHandler; host, realm: JString);
begin
  //
end;

procedure TWebViewClientDelegate.onReceivedSslError(view: JWebView; handler: JSslErrorHandler; error: JSslError);
begin
  //
end;

procedure TWebViewClientDelegate.onScaleChanged(view: JWebView; oldScale, newScale: Single);
begin
  //
end;

procedure TWebViewClientDelegate.onUnhandledKeyEvent(view: JWebView; event: JKeyEvent);
begin
  //
end;

function TWebViewClientDelegate.shouldOverrideKeyEvent(view: JWebView; event: JKeyEvent): Boolean;
begin
  Result := False;
end;

function TWebViewClientDelegate.shouldOverrideUrlLoading(view: JWebView; url: JString): Boolean;
begin
  TOSLog.d('TWebViewClientDelegate.shouldOverrideUrlLoading: %s', [JStringToString(url)]);
  Result := False;
end;

{ TPlatformPrinting }

constructor TPlatformPrinting.Create;
begin
  inherited Create;
  FWebViewClientDelegate := TWebViewClientDelegate.Create(Self);
  FWebViewClient := TJDWWebViewClient.JavaClass.init(FWebViewClientDelegate);
  FWebView := TJWebView.JavaClass.init(TAndroidHelper.Context);
  FWebView.setWebViewClient(FWebViewClient);
  // FWebView.getSettings.setJavaScriptEnabled(True);
  FWebView.getSettings.setAllowFileAccess(True);
  FWebView.getSettings.setAllowFileAccessFromFileURLs(True);
  FWebView.getSettings.setAllowUniversalAccessFromFileURLs(True);
end;

procedure TPlatformPrinting.CreatePrintManager;
begin
  if FPrintManager = nil then
    FPrintManager := TJPrintManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.PRINT_SERVICE));
end;

function TPlatformPrinting.GetPrintStatus(const AIndex: Integer): TPrintJobStatus;
var
  LJob: JPrintJob;
begin
  Result := TPrintJobStatus.None;
  if (AIndex > -1) and (AIndex < Length(FPrintJobs)) then
  begin
    LJob := FPrintJobs[AIndex];
    if LJob.isBlocked then
      Result := TPrintJobStatus.Blocked
    else if LJob.isCancelled then
      Result := TPrintJobStatus.Cancelled
    else if LJob.isCompleted then
      Result := TPrintJobStatus.Completed
    else if LJob.isFailed then
      Result := TPrintJobStatus.Failed
    else if LJob.isQueued then
      Result := TPrintJobStatus.Queued
    else if LJob.isStarted then
      Result := TPrintJobStatus.Started;
  end;
end;

function TPlatformPrinting.Print(const AFileName: string): Integer;
begin
  Result := -1;
  if not AFileName.EndsWith('.pdf', True) then
  begin
    CreatePrintManager;
    Result := Length(FPrintJobs);
    FDocumentName := TPath.GetFileName(AFileName);
    FURL := StringToJString('file://' + AFileName);
    TOSLog.d('Loading %s', [JStringToString(FURL)]);
    FWebView.loadUrl(FURL);
  end
  else
    PrintPDF(AFileName);
end;

procedure TPlatformPrinting.PrintPDF(const AFileName: string);
begin
  //
end;

procedure TPlatformPrinting.WebViewPageFinished(const AURL: JString);
begin
  TOSLog.d('Finished loading');
  // If AURL matches FURL?
  Print(FWebView.createPrintDocumentAdapter(StringToJString(FDocumentName)));
end;

function TPlatformPrinting.Print(const AAdapter: IInterface): Integer;
var
  LJob: JPrintJob;
  LAttributesBuilder: JPrintAttributes_Builder;
  LJobName: string;
begin
  if AAdapter <> nil then
  begin
    CreatePrintManager;
    Result := Length(FPrintJobs);
    LJobName := Format('%s Print Job %d', [TOSDevice.GetDeviceName, Result]);
    LAttributesBuilder := TJPrintAttributes_Builder.Create;
    //  .setDuplexMode()  // etc etc
    LJob := FPrintManager.print(StringToJString(LJobName), JPrintDocumentAdapter(AAdapter), LAttributesBuilder.build);
    FPrintJobs := FPrintJobs + [LJob];
  end
  else
    Result := -1;
end;

end.
