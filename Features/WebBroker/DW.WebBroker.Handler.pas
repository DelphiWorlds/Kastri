unit DW.WebBroker.Handler;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2022 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes, System.Generics.Collections, System.JSON,
  // Web
  Web.HTTPApp;

type
  TMethodKinds = set of TMethodType;

  TRouteHandlerFunc = reference to function(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;

  TRouteHandler = record
    NeedsAuth: Boolean;
    MethodKinds: TMethodKinds;
    Handler: TRouteHandlerFunc;
    function CanHandle(const AMethodKind: TMethodType): Boolean;
  end;

  TRouteHandlers = TDictionary<string, TRouteHandler>;

  /// <summary>
  ///    Web request handler class that provides useful functionality that may be common amongst handlers
  /// </summary>
  /// <remarks>
  ///    Handler methods for child paths can be added using the AddHandler method
  /// </remarks>
  TWebBrokerHandler = class(TObject)
  private
    FHandlers: TRouteHandlers;
    FMethodKinds: TMethodKinds;
    FPath: string;
    FPathSections: TArray<string>;
    function CanHandleMethod(const AMethodKind: TMethodType): Boolean;
    function GetPathSection(const AIndex: Integer): string;
    function HandleRoutes(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
    procedure SetNotHandled(const AResponse: TWebResponse);
    procedure SplitPath(const APath: string);
  protected
    /// <summary>
    ///    Adds a handler method for a child route for this web broker handler
    /// </summary>
    procedure AddHandler(const APath: string; const AMethodKinds: TMethodKinds; const ANeedsAuth: Boolean;
      const AHandler: TRouteHandlerFunc);
    /// <summary>
    ///    Override this method to perform any processing after the request is handled
    /// </summary>
    /// <remarks>
    ///    As an example, TWebBrokerRESTHandler overrides this method to free the JSON object created in the BeforeHandle method
    /// </remarks>
    procedure AfterHandle(const ARequest: TWebRequest; const AResponse: TWebResponse); virtual;
    /// <summary>
    ///    Override this method to perform any processing before the request is handled
    /// </summary>
    /// <remarks>
    ///    As an example, TWebBrokerRESTHandler overrides this method to parse the content of the request into a JSON object
    /// </remarks>
    procedure BeforeHandle(const ARequest: TWebRequest; const AResponse: TWebResponse); virtual;
    function CanHandlePath(const APath: string): Boolean; virtual;
    /// <summary>
    ///    Override this method to set the Path and MethodKinds properties, and to add any child route handlers
    /// </summary>
    procedure Configure; virtual;
    /// <summary>
    ///    Override this method to handle the request. Return True if the request was valid for the path etc
    /// </summary>
    function Handle(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean; virtual;
    function HandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
    /// <summary>
    ///    Override this method to check authentication, if necessary
    /// </summary>
    function IsAuthorized(const ARequest: TWebRequest): Boolean; virtual;
    /// <summary>
    ///    Sets the response to indicate that the content of the request is invalid (422)
    /// </summary>
    procedure SetInvalidContent(const AResponse: TWebResponse);
    /// <summary>
    ///    Sets the response to indicate that authorization details are missing or invalid (401)
    /// </summary>
    /// <remarks>
    ///    This will be called automatically if IsAuthorized returns False
    /// </remarks>
    procedure SetNotAuthorized(const AResponse: TWebResponse);
    /// <summary>
    ///    Returns the section of the path indicated by Index
    /// </summary>
    function TryGetPathSection(const AIndex: Integer; out AValue: string): Boolean; overload;
    /// <summary>
    ///    Returns the section of the path indicated by Index as an Integer, if it is a valid Integer
    /// </summary>
    function TryGetPathSection(const AIndex: Integer; out AValue: Integer): Boolean; overload;
    /// <summary>
    ///    Override this method to perform any special processing if the route supplied is not handled
    /// </summary>
    function UnhandledRoute(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean; virtual;
    property MethodKinds: TMethodKinds read FMethodKinds write FMethodKinds;
    property Path: string read FPath write FPath;
    property PathSections[const AIndex: Integer]: string read GetPathSection;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TWebBrokerHandlerClass = class of TWebBrokerHandler;

  TWebBrokerHandlerClasses = TArray<TWebBrokerHandlerClass>;

  TWebBrokerHandlerRegistry = record
  public
    class var Handlers: TWebBrokerHandlerClasses;
    class procedure RegisterClass(const AHandlerClass: TWebBrokerHandlerClass); static;
  end;

implementation

uses
 System.SysUtils;

function FixupPath(const APath: string): string;
begin
  if not APath.EndsWith('/') then
    Result := APath + '/'
  else
    Result := APath;
end;

{ TRouteHandler }

function TRouteHandler.CanHandle(const AMethodKind: TMethodType): Boolean;
begin
  Result := (MethodKinds = [TMethodType.mtAny]) or (AMethodKind in MethodKinds);
end;

{ TWebBrokerHandler }

constructor TWebBrokerHandler.Create;
begin
  inherited;
  FHandlers := TRouteHandlers.Create;
  FMethodKinds := [TMethodType.mtAny];
  Configure;
end;

destructor TWebBrokerHandler.Destroy;
begin
  FHandlers.Free;
  inherited;
end;

function TWebBrokerHandler.GetPathSection(const AIndex: Integer): string;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < Length(FPathSections)) then
    Result := FPathSections[AIndex];
end;

function TWebBrokerHandler.TryGetPathSection(const AIndex: Integer; out AValue: Integer): Boolean;
var
  LValue: string;
begin
  Result := TryGetPathSection(AIndex, LValue) and TryStrToInt(LValue, AValue);
end;

function TWebBrokerHandler.UnhandledRoute(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
begin
  Result := False;
end;

function TWebBrokerHandler.TryGetPathSection(const AIndex: Integer; out AValue: string): Boolean;
begin
  AValue := PathSections[AIndex];
  Result := not AValue.IsEmpty;
end;

procedure TWebBrokerHandler.AddHandler(const APath: string; const AMethodKinds: TMethodKinds; const ANeedsAuth: Boolean;
  const AHandler: TRouteHandlerFunc);
var
  LRouteHandler: TRouteHandler;
begin
  LRouteHandler.MethodKinds := AMethodKinds;
  LRouteHandler.NeedsAuth := ANeedsAuth;
  LRouteHandler.Handler := AHandler;
  FHandlers.Add(APath, LRouteHandler);
end;

procedure TWebBrokerHandler.AfterHandle(const ARequest: TWebRequest; const AResponse: TWebResponse);
begin
  //
end;

procedure TWebBrokerHandler.BeforeHandle(const ARequest: TWebRequest; const AResponse: TWebResponse);
begin
  //
end;

function TWebBrokerHandler.CanHandleMethod(const AMethodKind: TMethodType): Boolean;
begin
  Result := (MethodKinds = [TMethodType.mtAny]) or (AMethodKind in MethodKinds);
end;

function TWebBrokerHandler.Handle(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
begin
  Result := False;
end;

function TWebBrokerHandler.CanHandlePath(const APath: string): Boolean;
begin
  Result := APath.Equals(FPath) or (FPath.EndsWith('/*') and FixupPath(APath).StartsWith(FPath.Substring(0, Length(FPath) - 1)))
    or ((FHandlers.Count > 0) and APath.StartsWith(FPath + '/'));
end;

procedure TWebBrokerHandler.Configure;
begin
  //
end;

function TWebBrokerHandler.HandleRoutes(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
var
  LAction: string;
  LRouteHandler: TRouteHandler;
begin
  Result := False;
  for LAction in FHandlers.Keys.ToArray do
  begin
    if FixupPath(ARequest.PathInfo).StartsWith(FixupPath(Path + LAction)) and FHandlers.TryGetValue(LAction, LRouteHandler) then
    begin
      Result := True;
      if LRouteHandler.CanHandle(ARequest.MethodType) then
      begin
        if not LRouteHandler.NeedsAuth or IsAuthorized(ARequest) then
          Result := LRouteHandler.Handler(ARequest, AResponse)
        else
          SetNotAuthorized(AResponse);
      end
      else
        SetNotHandled(AResponse);
      Break;
    end;
  end;
  if not Result then
    Result := UnhandledRoute(ARequest, AResponse);
end;

procedure TWebBrokerHandler.SplitPath(const APath: string);
var
  LRequestPath, LPath: string;
begin
  if FPath.EndsWith('/*') then
    LPath := FPath.Substring(0, Length(FPath) - 2)
  else
    LPath := FPath;
  if LPath.Equals('*') then
    LRequestPath := APath
  else
    LRequestPath := APath.Substring(Length(LPath) + 1);
  FPathSections := LRequestPath.Split(['/']);
end;

function TWebBrokerHandler.HandleRequest(const ARequest: TWebRequest; const AResponse: TWebResponse): Boolean;
begin
  Result := False;
  // Can this handler handle the specified path?
  if CanHandlePath(ARequest.PathInfo) then
  begin
    SplitPath(ARequest.PathInfo);
    // Do any pre-processing
    BeforeHandle(ARequest, AResponse);
    try
      Result := True;
      // If there are no sub-routes configured..
      if not HandleRoutes(ARequest, AResponse) then
      begin
        // Check if this handler can handle the method
        if CanHandleMethod(ARequest.MethodType) then
        begin
          // Method is OK, check authorization
          if IsAuthorized(ARequest) then
            Result := Handle(ARequest, AResponse)
          else
            SetNotAuthorized(AResponse);
        end
        else
          SetNotHandled(AResponse);
      end;
    finally
      // Perform any cleanup required
      AfterHandle(ARequest, AResponse);
    end;
  end;
end;

function TWebBrokerHandler.IsAuthorized(const ARequest: TWebRequest): Boolean;
begin
  Result := False;
end;

procedure TWebBrokerHandler.SetInvalidContent(const AResponse: TWebResponse);
begin
  AResponse.StatusCode := 422; // Valid path and authorization, but content has something missing
end;

procedure TWebBrokerHandler.SetNotAuthorized(const AResponse: TWebResponse);
begin
  AResponse.StatusCode := 401; // Can handle the path and method, but not authorized
end;

procedure TWebBrokerHandler.SetNotHandled(const AResponse: TWebResponse);
begin
  AResponse.StatusCode := 403; // Can handle the path, but not the method
end;

{ TWebBrokerHandlerRegistry }

class procedure TWebBrokerHandlerRegistry.RegisterClass(const AHandlerClass: TWebBrokerHandlerClass);
begin
  Handlers := Handlers + [AHandlerClass];
end;

end.
