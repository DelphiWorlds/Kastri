unit DW.Connectivity.iOS;

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
  System.Net.Socket,
  // Mac
  Macapi.CoreFoundation,
  // DW
  DW.iOSapi.SystemConfiguration, DW.Connectivity;

type
  TPlatformConnectivity = class(TObject)
  private
    FConnectivity: TConnectivity;
  public
    class function IsConnectedToInternet: Boolean;
    class function IsWifiEnabled: Boolean;
    class function IsWifiInternetConnection: Boolean;
  public
    constructor Create(const AConnectivity: TConnectivity);
  end;

implementation

uses
  // Mac
  Macapi.Helpers,
  // iOS
  iOSapi.Foundation,
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
