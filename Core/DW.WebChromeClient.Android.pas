unit DW.WebChromeClient.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes, System.Messaging,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.WebKit, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes,
  // FMX
  FMX.WebBrowser,
  // DW
  DW.Androidapi.JNI.DWWebChromeClient, DW.WebChromeClient;

type
  TWebChromeClientManager = class;

  TWebChromeClientDelegate = class(TJavaLocal, JDWWebChromeClientDelegate)
  private
    FManager: TWebChromeClientManager;
  public
    { JDWWebChromeClientDelegate }
    function onFileChooserIntent(intent: JIntent): Boolean; cdecl;
    function onShouldOverrideUrlLoading(url: JString): Boolean; cdecl;
  public
    constructor Create(const AManager: TWebChromeClientManager);
  end;

  TWebChromeClientManager = class(TCustomPlatformWebChromeClientManager)
  private
    FWebChromeClient: JDWWebChromeClient;
    FDelegate: JDWWebChromeClientDelegate;
    procedure MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
  protected
    procedure FlushCookies; override;
    function HandleFileChooserIntent(const AIntent: JIntent): Boolean;
    function ShouldOverrideUrlLoading(const AUrl: JString): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPlatformWebChromeClientManager = class(TWebChromeClientManager);

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App;

const
  cFileChooserRequestCode = 9999;

{ TWebChromeClientDelegate }

constructor TWebChromeClientDelegate.Create(const AManager: TWebChromeClientManager);
begin
  inherited Create;
  FManager := AManager;
end;

function TWebChromeClientDelegate.onFileChooserIntent(intent: JIntent): Boolean;
begin
  Result := FManager.HandleFileChooserIntent(intent);
end;

function TWebChromeClientDelegate.onShouldOverrideUrlLoading(url: JString): Boolean;
begin
  Result := FManager.ShouldOverrideUrlLoading(url);
end;

{ TWebChromeClientManager }

constructor TWebChromeClientManager.Create(AOwner: TComponent);
var
  LWebView: JWebView;
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationHandler);
  if Supports(AOwner, JWebView, LWebView) then
  begin
    FDelegate := TWebChromeClientDelegate.Create(Self);
    FWebChromeClient := TJDWWebChromeClient.JavaClass.init(FDelegate, TAndroidHelper.Activity);
    LWebView.setWebChromeClient(FWebChromeClient);
  end;
end;

destructor TWebChromeClientManager.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationHandler);
  inherited;
end;

function TWebChromeClientManager.HandleFileChooserIntent(const AIntent: JIntent): Boolean;
begin
  TAndroidHelper.Activity.startActivityForResult(AIntent, cFileChooserRequestCode);
  Result := True;
end;

procedure TWebChromeClientManager.MessageResultNotificationHandler(const Sender: TObject; const M: TMessage);
var
  LResult: TMessageResultNotification;
begin
  if M is TMessageResultNotification then
  begin
    LResult := TMessageResultNotification(M);
    if LResult.RequestCode = cFileChooserRequestCode then
      FWebChromeClient.handleFileChooserResult(LResult.Value, LResult.ResultCode);
  end;
end;

function TWebChromeClientManager.ShouldOverrideUrlLoading(const AUrl: JString): Boolean;
begin
  Result := False;
  DoShouldOverrideUrl(JStringToString(AUrl), Result);
end;

procedure TWebChromeClientManager.FlushCookies;
begin
  TJCookieManager.JavaClass.getInstance.Flush;
end;

end.
