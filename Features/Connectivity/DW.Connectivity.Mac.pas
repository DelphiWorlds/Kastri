unit DW.Connectivity.Mac;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.Net.Socket,
  // macOS
  Macapi.CoreFoundation,
  // DW
  {$IF Defined(IOS)}
  DW.iOSapi.SystemConfiguration,
  {$ELSE}
  DW.Macapi.SystemConfiguration,
  {$ENDIF}
  DW.Connectivity;

type
  TPlatformConnectivity = class(TObject)
  private
    FConnectivity: TConnectivity;
  public
    class function GetLocalAddresses: TIPAddresses;
    class function IsConnectedToInternet: Boolean;
    class function IsWifiEnabled: Boolean;
    class function IsWifiInternetConnection: Boolean;
  public
    constructor Create(const AConnectivity: TConnectivity);
  end;

implementation

uses
  // macOS
  Macapi.Helpers,
  {$IF Defined(IOS)}
  iOSapi.Foundation,
  {$ELSE}
  Macapi.Foundation,
  {$ENDIF}
  // Posix
  Posix.Base, Posix.SysSocket, Posix.NetinetIn, Posix.ArpaInet, Posix.NetIf;

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

const
  cWifiInterfaceName = 'awdl0'; // <----- That's a small L (l), not a one (1)

function getifaddrs(var ifap: pifaddrs): Integer; cdecl; external libc name _PU + 'getifaddrs'; {do not localize}
procedure freeifaddrs(ifap: pifaddrs); cdecl; external libc name _PU + 'freeifaddrs'; {do not localize}

function IsEthernet(const AFlags: Cardinal): Boolean;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  Result := ((AFlags and IFF_UP) = IFF_UP) and ((AFlags and IFF_LOOPBACK) = 0) and (AFlags and IFF_BROADCAST <> 0) and
    (AFlags and IFF_MULTICAST <> 0);
  {$WARN SYMBOL_PLATFORM ON}
end;

{ TPlatformConnectivity }

constructor TPlatformConnectivity.Create(const AConnectivity: TConnectivity);
begin
  inherited Create;
  FConnectivity := AConnectivity;
  TReachability.Current.Connectivity := FConnectivity;
end;

class function TPlatformConnectivity.GetLocalAddresses: TIPAddresses;
var
  LAddrList, LAddrInfo: pifaddrs;
  LIPAddress: TIPAddress;
  LAddress: array[0..45] of AnsiChar;
begin
  Result := [];
  if getifaddrs(LAddrList) = 0 then
  try
    LAddrInfo := LAddrList;
    repeat
      LIPAddress := Default(TIPAddress);
      if IsEthernet(LAddrInfo.ifa_flags) then
      begin
        LIPAddress.InterfaceName := string(UTF8String(MarshaledAString(LAddrInfo.ifa_name)));
        case LAddrInfo.ifa_addr.sa_family of
          AF_INET:
          begin
            LIPAddress.Version := TIPVersion.IPv4;
            LIPAddress.IP := string(AnsiString(inet_ntoa(Psockaddr_in(LAddrInfo.ifa_addr).sin_addr)));
            Result := Result + [LIPAddress];
          end;
          AF_INET6:
          begin
            LIPAddress.Version := TIPVersion.IPv6;
            inet_ntop(AF_INET6, @Psockaddr_in6(LAddrInfo.ifa_addr).sin6_addr, LAddress, SizeOf(LAddress));
            LIPAddress.IP := string(LAddress);
            Result := Result + [LIPAddress];
          end;
        end;
      end;
      LAddrInfo := LAddrInfo^.ifa_next;
    until LAddrInfo = nil;
  finally
    freeifaddrs(LAddrList);
  end;
end;

class function TPlatformConnectivity.IsConnectedToInternet: Boolean;
begin
  Result := TReachability.Current.IsConnectedToInternet;
end;

class function TPlatformConnectivity.IsWifiInternetConnection: Boolean;
begin
  Result := TReachability.Current.IsWifiInternetConnection;
end;

class function TPlatformConnectivity.IsWifiEnabled: Boolean;
var
  LAddrList, LAddrInfo: pifaddrs;
  LSet: NSCountedSet;
begin
  Result := False;
  if getifaddrs(LAddrList) = 0 then
  try
    LSet := TNSCountedSet.Create;
    LAddrInfo := LAddrList;
    repeat
      {$WARN SYMBOL_PLATFORM OFF}
      if (LAddrInfo.ifa_flags and IFF_UP) = IFF_UP then
        LSet.addObject(TNSString.OCClass.stringWithUTF8String(LAddrInfo.ifa_name));
      {$WARN SYMBOL_PLATFORM ON}
      LAddrInfo := LAddrInfo^.ifa_next;
    until LAddrInfo = nil;
    Result := LSet.countForObject(StringToID(cWifiInterfaceName)) > 1;
  finally
    freeifaddrs(LAddrList);
  end;
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
  LEndPoint := TNetEndpoint.Create(System.Net.Socket.TIPAddress.Any, 0);
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
