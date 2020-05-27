unit DW.Connectivity.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Net.Socket,
  // macOS
  Macapi.CoreFoundation,
  // DW
  DW.iOSapi.SystemConfiguration, DW.Connectivity;

type
  TPlatformConnectivity = class(TObject)
  private
    FConnectivity: TConnectivity;
  public
    class function IsConnectedToInternet: Boolean;
    class function IsWifiInternetConnection: Boolean;
  public
    constructor Create(const AConnectivity: TConnectivity);
  end;

implementation

uses
  // Posix
  Posix.SysSocket, Posix.NetinetIn, Posix.ArpaInet;

type
  TReachability = class(TObject)
  private
    class var FCurrent: TReachability;
    class constructor CreateClass;
    class destructor DestroyClass;
    class procedure ReachabilityCallback(reachability: SCNetworkReachabilityRef; flags: SCNetworkReachabilityFlags; info: Pointer); cdecl; static;
  private
    FConnectivity: TConnectivity;
    FIsConnectedToInternet: Boolean;
    FIsWifiInternetConnection: Boolean;
    FReachabilityRef: SCNetworkReachabilityRef;
    procedure ReachabilityChanged(const AFlags: SCNetworkReachabilityFlags);
    function Start: Boolean;
    procedure Stop;
    procedure UpdateReachability(const AFlags: SCNetworkReachabilityFlags);
  public
    class property Current: TReachability read FCurrent;
  public
    constructor Create;
    destructor Destroy; override;
    property Connectivity: TConnectivity read FConnectivity write FConnectivity;
    property IsConnectedToInternet: Boolean read FIsConnectedToInternet;
    property IsWifiInternetConnection: Boolean read FIsWifiInternetConnection;
  end;

  TOpenConnectivity = class(TConnectivity);

constructor TPlatformConnectivity.Create(const AConnectivity: TConnectivity);
begin
  inherited Create;
  FConnectivity := AConnectivity;
  TReachability.Current.Connectivity := FConnectivity;
end;

class function TPlatformConnectivity.IsConnectedToInternet: Boolean;
begin
  Result := TReachability.Current.IsConnectedToInternet;
end;

class function TPlatformConnectivity.IsWifiInternetConnection: Boolean;
begin
  Result := TReachability.Current.IsWifiInternetConnection;
end;

{ TReachability }

class constructor TReachability.CreateClass;
begin
  FCurrent := TReachability.Create;
end;

class destructor TReachability.DestroyClass;
begin
  FCurrent.Free;
end;

constructor TReachability.Create;
var
  LEndPoint: TNetEndpoint;
  LRawAddress: sockaddr;
  LFlags: SCNetworkReachabilityFlags;
begin
  inherited;
  LEndPoint := TNetEndpoint.Create(TIPAddress.Any, 0);
  LRawAddress := LEndPoint;
  FReachabilityRef := SCNetworkReachabilityCreateWithAddress(kCFAllocatorDefault, @LRawAddress);
  SCNetworkReachabilityGetFlags(FReachabilityRef, @LFlags);
  UpdateReachability(LFlags);
  Start;
end;

destructor TReachability.Destroy;
begin
  Stop;
  CFRelease(FReachabilityRef);
  FReachabilityRef := nil;
  inherited;
end;

class procedure TReachability.ReachabilityCallback(reachability: SCNetworkReachabilityRef; flags: SCNetworkReachabilityFlags; info: Pointer);
begin
  TReachability(info).ReachabilityChanged(flags);
end;

procedure TReachability.UpdateReachability(const AFlags: SCNetworkReachabilityFlags);
begin
  FIsConnectedToInternet := (AFlags and kSCNetworkReachabilityFlagsReachable) > 0;
  FIsWifiInternetConnection := FIsConnectedToInternet and ((AFlags and kSCNetworkReachabilityFlagsIsWWAN) = 0);
end;

procedure TReachability.ReachabilityChanged(const AFlags: SCNetworkReachabilityFlags);
begin
  UpdateReachability(AFlags);
  if FConnectivity <> nil then
    TOpenConnectivity(FConnectivity).DoConnectivityChange(FIsConnectedToInternet);
end;

function TReachability.Start: Boolean;
var
  LContext: SCNetworkReachabilityContext;
begin
  LContext := Default(SCNetworkReachabilityContext);
  LContext.info := Self;
  Result := SCNetworkReachabilitySetCallback(FReachabilityRef, @ReachabilityCallback, @LContext);
  Result := Result and SCNetworkReachabilityScheduleWithRunLoop(FReachabilityRef, CFRunLoopGetCurrent, kCFRunLoopDefaultMode);
end;

procedure TReachability.Stop;
begin
  SCNetworkReachabilityUnscheduleFromRunLoop(FReachabilityRef, CFRunLoopGetCurrent, kCFRunLoopDefaultMode);
end;

end.
