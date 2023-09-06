unit DW.WebBroker.WebServer;

// **NOTE**: The WebBroker feature in Kastri is being deprecated - no further changes will be made.

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Classes,
  // Indy
  IdHTTPWebBrokerBridge, IdContext;

type
  /// <summary>
  ///   Component for acting as a standalone webserver
  /// </summary>
  TWebServer = class(TComponent)
  private
    FServer: TIdHTTPWebBrokerBridge;
    FUseOwnAuthentication: Boolean;
    FOnActiveChanged: TNotifyEvent;
    procedure DoActiveChanged;
    function GetIsActive: Boolean;
    function GetLocalIPAddresses: TArray<string>;
    function GetPort: Integer;
    procedure ParseAuthenticationHandler(AContext: TIdContext; const AAuthType, AAuthData: string; var AUsername, APassword: string;
      var AHandled: Boolean);
    procedure SetIsActive(const AValue: Boolean);
    procedure SetPort(const AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetBoundIPAddresses: TArray<string>;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property Port: Integer read GetPort write SetPort;
    /// <summary>
    ///   Controls whether or not the webserver handles authentication
    /// </summary>
    /// <remarks>
    ///   When set to True, the webserver code should use its own authentication, for example actions in a web module could handle the authentication
    /// </remarks>
    property UseOwnAuthentication: Boolean read FUseOwnAuthentication write FUseOwnAuthentication;
    property OnActiveChanged: TNotifyEvent read FOnActiveChanged write FOnActiveChanged;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Indy
  IdStack, IdGlobal;

{ TWebServer }

constructor TWebServer.Create(AOwner: TComponent);
begin
  inherited;
  FServer := TIdHTTPWebBrokerBridge.Create(Self);
  FServer.OnParseAuthentication := ParseAuthenticationHandler;
end;

destructor TWebServer.Destroy;
begin
  //
  inherited;
end;

procedure TWebServer.DoActiveChanged;
begin
  if Assigned(FOnActiveChanged) then
    FOnActiveChanged(Self);
end;

function TWebServer.GetBoundIPAddresses: TArray<string>;
var
  I: Integer;
  LAddress: string;
begin
  if IsActive then
  begin
    for I := 0 to FServer.Bindings.Count - 1 do
    begin
      // Just do IPv4 for now
      if FServer.Bindings[I].IPVersion = TIdIPVersion.Id_IPv4 then
      begin
        LAddress := FServer.Bindings[I].IP;
        if LAddress.Equals('0.0.0.0') then
          Result := Result + GetLocalIPAddresses
        else
          Result := Result + [LAddress];
      end;
    end;
  end;
end;

function TWebServer.GetIsActive: Boolean;
begin
  Result := FServer.Active;
end;

function TWebServer.GetLocalIPAddresses: TArray<string>;
var
  I: Integer;
  LAddresses: TIdStackLocalAddressList;
begin
  LAddresses := TIdStackLocalAddressList.Create;
  try
    GStack.GetLocalAddressList(LAddresses);
    for I := 0 to LAddresses.Count - 1 do
    begin
      // Just do IPv4 for now
      if LAddresses[I].IPVersion = TIdIPVersion.Id_IPv4 then
        Result := Result + [LAddresses[I].IPAddress];
    end;
  finally
    LAddresses.Free;
  end;
end;

function TWebServer.GetPort: Integer;
begin
  Result := FServer.DefaultPort;
end;

procedure TWebServer.ParseAuthenticationHandler(AContext: TIdContext; const AAuthType, AAuthData: string; var AUsername, APassword: string;
  var AHandled: Boolean);
begin
  AHandled := FUseOwnAuthentication;
end;

procedure TWebServer.SetIsActive(const AValue: Boolean);
var
  LActive: Boolean;
begin
  LActive := FServer.Active;
  try
    FServer.Active := AValue;
  except
    // TODO: Have an option for not swallowing exception
  end;
  if FServer.Active <> LActive then
    DoActiveChanged;
end;

procedure TWebServer.SetPort(const AValue: Integer);
var
  LSavedActive: Boolean;
begin
  if FServer.DefaultPort <> AValue then
  begin
    LSavedActive := IsActive;
    if LSavedActive then
      IsActive := False;
    FServer.DefaultPort := AValue;
    IsActive := LSavedActive;
  end;
end;

end.
