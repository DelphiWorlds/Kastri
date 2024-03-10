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
  FMX.WebBrowser, FMX.Graphics;

const
  cJavaScriptGetInputValueByName = '(function() { return document.getElementsByName("%s")[0].value; })()';
  cJavaScriptGetPageContents = '(function() { return document.getElementsByTagName("html")[0].innerHTML; })()';
  cJavaScriptNullResult = 'null';
  cJavaScriptSetInputValueByName = '(function() { document.getElementById("%s").value = "%s"; })()';

const
  cJSEventScheme = 'about'; // This is being used so as to support Android (EMBT's code otherwise interferes)
  cJSEventProtocol = cJSEventScheme + '://';

type
  TJSEventParam = record
    Name: string;
    Value: string;
    constructor Create(const AName, AValue: string);
    function ToString: string;
  end;

  TJSEventParams = TArray<TJSEventParam>;

  TJSEvent = record
    Name: string;
    Params: TJSEventParams;
    function IndexOfParam(const AParamName: string): Integer;
    function TryGetValue(const AParamName: string; out AValue: Double): Boolean; overload;
    function TryGetValue(const AParamName: string; out AValue: Integer): Boolean; overload;
    function TryGetValue(const AParamName: string; out AValue: string): Boolean; overload;
    function ParseEvent(const AURL: string): Boolean;
    function ToString: string;
  end;

  TJavaScriptResultProc = reference to procedure(const JavaScriptResult: string; const ErrorCode: Integer);

  TPrintJobStatus = (None, Blocked, Cancelled, Completed, Failed, Queued, Started);

  THitTestKind = (Unknown, EditText, Email, Geo, Image, Phone, SrcAnchor, SrcImageAnchor);

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

{ TJSEventParam }

constructor TJSEventParam.Create(const AName, AValue: string);
begin
  Name := AName.Replace('/', '');
  Value := AValue.Replace('/', '');
end;

function TJSEventParam.ToString: string;
begin
  Result := Format('%s: %s', [Name, Value]);
end;

{ TJSEvent }

function TJSEvent.ToString: string;
var
  LLines: TArray<string>;
  LParam: TJSEventParam;
begin
  LLines := [Format('Name: %s', [Name])];
  if Length(Params) > 0 then
    LLines := LLines + ['Params:'];
  for LParam in Params do
    LLines := LLines + [LParam.ToString];
  Result := string.Join(#13#10, LLines);
end;

function TJSEvent.TryGetValue(const AParamName: string; out AValue: string): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOfParam(AParamName);
  if LIndex > -1 then
  begin
    AValue := Params[LIndex].Value;
    Result := True;
  end;
end;

function TJSEvent.TryGetValue(const AParamName: string; out AValue: Double): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOfParam(AParamName);
  if LIndex > -1 then
  begin
    if TryStrToFloat(Params[LIndex].Value, AValue) then
      Result := True;
  end;
end;

function TJSEvent.TryGetValue(const AParamName: string; out AValue: Integer): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  LIndex := IndexOfParam(AParamName);
  if LIndex > -1 then
  begin
    if TryStrToInt(Params[LIndex].Value, AValue) then
      Result := True;
  end;
end;

function TJSEvent.IndexOfParam(const AParamName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Params) to High(Params) do
  begin
    if Params[I].Name.Equals(AParamName) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TJSEvent.ParseEvent(const AURL: string): Boolean;
var
  LParts, LArgs: TArray<string>;
  LArg: string;
begin
  Result := False;
  Name := '';
  Params := [];
  if AURL.ToLower.StartsWith(cJSEventProtocol) then
  begin
    LParts := AURL.Substring(cJSEventProtocol.Length).Split([':']);
    if Length(LParts) > 0 then
    begin
      Name := LParts[0].Replace('/', '');
      if Length(LParts) > 1 then
      begin
        LArgs := LParts[1].Split(['#']);
        for LArg in LArgs do
        begin
          LParts := LArg.Split(['=']);
          if Length(LParts) > 1 then
            Params := Params + [TJSEventParam.Create(LParts[0], LParts[1])];
        end;
      end;
      Result := True;
    end;
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
