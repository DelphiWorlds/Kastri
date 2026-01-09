unit DW.WebBroker.HandlerWebModule;

// **NOTE**: The WebBroker feature in Kastri is being deprecated - no further changes will be made.

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
  System.SysUtils, System.Classes,
  // Web
  Web.HTTPApp,
  // DW
  DW.WebBroker.Handler;

type
  TWebBrokerHandlers = TArray<TWebBrokerHandler>;

  /// <summary>
  ///   "Global" web module that manages a list of handlers that do the work of handling web requests
  /// </summary>
  THandlerWebModule = class(TWebModule)
    procedure WebModuleDefaultHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    class var
      FAllowCORS: Boolean;
      FSimulatedDelay: Integer;
  private
    FHandlers: TWebBrokerHandlers;
    procedure CreateHandlers;
    procedure DestroyHandlers;
    function HandleCORS(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
    function HandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
  protected
    procedure SetError(const AResponse: TWebResponse); virtual;
    procedure SetNotHandled(const AResponse: TWebResponse); virtual;
  public
    /// <summary>
    ///   Controls whether or not Cross-Origin requests are allowed
    /// </summary>
    class property AllowCORS: Boolean read FAllowCORS write FAllowCORS;
    /// <summary>
    ///   Imposes a delay when servicing requests. Value is in milliseconds
    /// </summary>
    /// <remarks>
    ///   Can be useful when debugging
    /// </remarks>
    class property SimulatedDelay: Integer read FSimulatedDelay write FSimulatedDelay;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  WebModuleClass: TComponentClass = THandlerWebModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  // Web
  Web.WebReq;

type
  TOpenWebBrokerHandler = class(TWebBrokerHandler);

constructor THandlerWebModule.Create(AOwner: TComponent);
begin
  inherited;
  CreateHandlers;
end;

destructor THandlerWebModule.Destroy;
begin
  DestroyHandlers;
  inherited;
end;

procedure THandlerWebModule.CreateHandlers;
var
  LHandlerClass: TWebBrokerHandlerClass;
begin
  for LHandlerClass in TWebBrokerHandlerRegistry.Handlers do
    FHandlers := FHandlers + [LHandlerClass.Create];
end;

procedure THandlerWebModule.DestroyHandlers;
var
  LHandler: TWebBrokerHandler;
begin
  for LHandler in FHandlers do
    LHandler.Free;
end;

procedure THandlerWebModule.SetError(const AResponse: TWebResponse);
begin
  AResponse.ContentType := 'text/html';
  AResponse.StatusCode := 500;
end;

procedure THandlerWebModule.SetNotHandled(const AResponse: TWebResponse);
begin
  AResponse.ContentType := 'text/html';
  AResponse.StatusCode := 404;
end;

function THandlerWebModule.HandleCORS(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
var
  LRequestHeaders: string;
begin
  Result := False;
  if FAllowCORS then
  begin
    AResponse.SetCustomHeader('Access-Control-Allow-Origin','*');
    LRequestHeaders := ARequest.GetFieldByName('Access-Control-Request-Headers').Trim;
    if not LRequestHeaders.IsEmpty then
    begin
      AResponse.SetCustomHeader('Access-Control-Allow-Headers', LRequestHeaders);
      Result := True;
    end;
  end;
end;

function THandlerWebModule.HandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
var
  LHandler: TWebBrokerHandler;
begin
  Sleep(FSimulatedDelay);
  Result := False;
  for LHandler in FHandlers do
  begin
    if TOpenWebBrokerHandler(LHandler).HandleRequest(Request, Response) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure THandlerWebModule.WebModuleDefaultHandlerAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    Handled := HandleCORS(Request, Response) or HandleRequest(Request, Response);
    if Handled then
    begin
      if Response.StatusCode = 500 then
        SetError(Response);
    end
    else
    begin
      SetNotHandled(Response);
      Handled := True;
    end;
  except
    on E: Exception do
    begin
      Response.Content := Format('%s: %s', [E.ClassName, E.Message]);
      SetError(Response);
    end;
  end;
end;

initialization
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;

end.
