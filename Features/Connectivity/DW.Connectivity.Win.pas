unit DW.Connectivity.Win;

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
  // DW
  DW.Connectivity;

type
  TPlatformConnectivity = class(TObject)
  private
    FConnectivity: TConnectivity;
    FNetwork: TObject;
  public
    class function GetLocalAddresses: TIPAddresses;
    class function IsConnectedToInternet: Boolean;
    class function IsWifiInternetConnection: Boolean;
  public
    constructor Create(const AConnectivity: TConnectivity);
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Classes,
  // Windows
  Winapi.ActiveX, Winapi.Windows, Winapi.IpTypes, Winapi.IpHlpApi, Winapi.WinSock, Winapi.WinSock2,
  // DW
  DW.Winapi.NetworkList_TLB;

type
  TNetwork = class(TInterfacedObject, INetworkEvents)
  private
    class function CoInitBegin: Boolean;
    class procedure CoInitEnd(const ACoInit: Boolean);
    class function GetWifiAdapterNames: TArray<string>;
  private
    FCookie: Longint;
    FNetworkListManager: INetworkListManager;
    FIsConnectedToInternet: Boolean;
    procedure Start;
    procedure Stop;
  protected
    class function GetIsConnectedToInternet: Boolean;
    class function GetHasWifiInternetConnection: Boolean;
  public
    { INetworkEvents }
    function NetworkAdded(networkId: TGUID): HResult; stdcall;
    function NetworkDeleted(networkId: TGUID): HResult; stdcall;
    function NetworkConnectivityChanged(networkId: TGUID; newConnectivity: NLM_CONNECTIVITY): HResult; stdcall;
    function NetworkPropertyChanged(networkId: TGUID; Flags: NLM_NETWORK_PROPERTY_CHANGE): HResult; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TOpenConnectivity = class(TConnectivity);

  IN6_ADDR = record
    case Integer of
      0: (Byte: array [0..15] of UCHAR);
      1: (Word: array [0..7] of USHORT);
  end;

  SOCKADDR_IN6 = record
    sin6_family: short;
    sin6_port: u_short;
    sin6_flowinfo: u_long;
    sin6_addr: IN6_ADDR;
    sin6_scope_id: u_long;
  end;
  PSOCKADDR_IN6 = ^SOCKADDR_IN6;

const
  IID_IConnectionPointContainer: TGUID = (D1:$B196B284;D2:$BAB4;D3:$101A;D4:($B6,$9C,$00,$AA,$00,$34,$1D,$07));
  IF_TYPE_SOFTWARE_LOOPBACK = 24;
  IF_TYPE_IEEE80211 = 71;

var
  Connectivity: TConnectivity;

{ TPlatformConnectivity }

constructor TPlatformConnectivity.Create(const AConnectivity: TConnectivity);
begin
  inherited Create;
  FNetwork := TNetwork.Create;
  FConnectivity := AConnectivity;
  Connectivity := FConnectivity;
end;

destructor TPlatformConnectivity.Destroy;
begin
  Connectivity := nil;
  TNetwork(FNetwork).Stop;
  FNetwork := nil;
  inherited;
end;

class function TPlatformConnectivity.GetLocalAddresses: TIPAddresses;
var
  LRes: DWORD;
  LBufLen: ULONG;
  LAdapters, LAdapter: PIP_ADAPTER_ADDRESSES;
  LUnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
  LIPAddress: TIPAddress;
  LAddress: array[0..45] of AnsiChar;
  LBuffer: TBytes;
begin
  Result := [];
  LBufLen := 0;
  LRes := GetAdaptersAddresses(PF_UNSPEC, GAA_FLAG_SKIP_ANYCAST or GAA_FLAG_SKIP_MULTICAST or GAA_FLAG_SKIP_DNS_SERVER, nil, nil, @LBufLen);
  if LRes = ERROR_BUFFER_OVERFLOW then
  begin
    SetLength(LBuffer, LBufLen);
    LAdapters := PIP_ADAPTER_ADDRESSES(@LBuffer[0]);
    LRes := GetAdaptersAddresses(PF_UNSPEC, GAA_FLAG_SKIP_ANYCAST or GAA_FLAG_SKIP_MULTICAST or GAA_FLAG_SKIP_DNS_SERVER, nil, LAdapters, @LBufLen);
    if LRes = ERROR_SUCCESS then
    begin
      LAdapter := LAdapters;
      while LAdapter <> nil do
      begin
        if (LAdapter.IfType <> IF_TYPE_SOFTWARE_LOOPBACK) and ((LAdapter.Flags and IP_ADAPTER_RECEIVE_ONLY) = 0) then
        begin
          LUnicastAddress := LAdapter.FirstUnicastAddress;
          while LUnicastAddress <> nil do
          begin
            if LUnicastAddress.DadState = IpDadStatePreferred then
            begin
              case LUnicastAddress.Address.lpSockaddr.sin_family of
                AF_INET:
                begin
                  LIPAddress.Version := TIPVersion.IPv4;
                  inet_ntop(AF_INET, @PSOCKADDR_IN(LUnicastAddress.Address.lpSockaddr).sin_addr, LAddress, SizeOf(LAddress));
                  LIPAddress.IP := string(AnsiString(LAddress));
                  Result := Result + [LIPAddress];
                end;
                AF_INET6:
                begin
                  LIPAddress.Version := TIPVersion.IPv6;
                  inet_ntop(AF_INET6, @PSOCKADDR_IN6(LUnicastAddress.Address.lpSockaddr).sin6_addr, LAddress, SizeOf(LAddress));
                  LIPAddress.IP := string(AnsiString(LAddress));
                  Result := Result + [LIPAddress];
                end;
              end;
            end;
            LUnicastAddress := LUnicastAddress.Next;
          end;
        end;
        LAdapter := LAdapter^.Next;
      end;
    end;
  end;
end;

class function TPlatformConnectivity.IsConnectedToInternet: Boolean;
begin
  Result := TNetwork.GetIsConnectedToInternet;
end;

class function TPlatformConnectivity.IsWifiInternetConnection: Boolean;
begin
  Result := TNetwork.GetHasWifiInternetConnection;
end;

{ TNetwork }

constructor TNetwork.Create;
begin
  inherited;
  CoInitialize(nil);
  FNetworkListManager := CoNetworkListManager.Create;
  FIsConnectedToInternet := GetIsConnectedToInternet;
  Start;
end;

destructor TNetwork.Destroy;
begin
  FNetworkListManager := nil;
  CoUninitialize;
  inherited;
end;

class function TNetwork.GetWifiAdapterNames: TArray<string>;
var
  LRes: DWORD;
  LBufLen: ULONG;
  LAdapters: TArray<IP_ADAPTER_INFO>;
  LAdapter: PIP_ADAPTER_INFO;
begin
  Result := [];
  SetLength(LAdapters, 128);
  LBufLen := Length(LAdapters) * SizeOf(IP_ADAPTER_INFO);
  LRes := GetAdaptersInfo(@LAdapters[0], LBufLen);
  if LRes = ERROR_SUCCESS  then
  begin
    LAdapter := @LAdapters[0];
    repeat
      if LAdapter^.Type_ = IF_TYPE_IEEE80211 then
        Result := Result + [string(LAdapter^.AdapterName)];
      LAdapter := LAdapter^.Next;
    until LAdapter = nil;
  end;
end;

class function TNetwork.GetHasWifiInternetConnection: Boolean;
var
  LConnections: IEnumNetworkConnections;
  LConnection: INetworkConnection;
  LFetched: LongWord;
  LWifiAdapterNames: TArray<string>;
  LAdapterName: string;
  LAdapterID: string;
  LCoInit: Boolean;
  LManager: INetworkListManager;
begin
  Result := False;
  LWifiAdapterNames := GetWifiAdapterNames;
  if Length(LWifiAdapterNames) > 0 then
  begin
    LCoInit := CoInitBegin;
    try
      LManager := CoNetworkListManager.Create;
      LConnections := LManager.GetNetworkConnections;
      repeat
        LConnections.Next(1, LConnection, LFetched);
        if LFetched > 0 then
        begin
          if LConnection.IsConnectedToInternet then
          begin
            LAdapterID := LConnection.GetAdapterId.ToString;
            for LAdapterName in LWifiAdapterNames do
            begin
              if LAdapterName.Equals(LAdapterID) then
              begin
                Result := True;
                Break;
              end;
            end;
          end;
        end;
      until Result or (LFetched = 0);
    finally
      CoInitEnd(LCoInit);
    end;
  end;
end;

class function TNetwork.CoInitBegin: Boolean;
begin
  Result := TThread.CurrentThread.ThreadID <> MainThreadID;
  if Result then
    CoInitialize(nil);
end;

class procedure TNetwork.CoInitEnd(const ACoInit: Boolean);
begin
  if ACoInit then
    CoUninitialize;
end;

class function TNetwork.GetIsConnectedToInternet: Boolean;
var
  LCoInit: Boolean;
  LManager: INetworkListManager;
begin
  LCoInit := CoInitBegin;
  try
    LManager := CoNetworkListManager.Create;
    Result := LManager.IsConnectedToInternet;
  finally
    CoInitEnd(LCoInit);
  end;
end;

function TNetwork.NetworkAdded(networkId: TGUID): HResult;
begin
  Result := S_OK;
end;

function TNetwork.NetworkConnectivityChanged(networkId: TGUID; newConnectivity: NLM_CONNECTIVITY): HResult;
var
  LIsConnected: Boolean;
begin
  if Connectivity <> nil then
  begin
    LIsConnected := GetIsConnectedToInternet;
    if LIsConnected <> FIsConnectedToInternet then
    begin
      FIsConnectedToInternet := LIsConnected;
      TOpenConnectivity(Connectivity).DoConnectivityChange(FIsConnectedToInternet);
    end;
  end;
  Result := S_OK;
end;

function TNetwork.NetworkDeleted(networkId: TGUID): HResult;
begin
  Result := S_OK;
end;

function TNetwork.NetworkPropertyChanged(networkId: TGUID; Flags: NLM_NETWORK_PROPERTY_CHANGE): HResult;
begin
  Result := S_OK;
end;

procedure TNetwork.Start;
var
  LConnectionPointContainer: IConnectionPointContainer;
  LConnectionPoint: IConnectionPoint;
begin
  if (FCookie = 0) and Succeeded(FNetworkListManager.QueryInterface(IID_IConnectionPointContainer, LConnectionPointContainer)) then
  begin
    if Succeeded(LConnectionPointContainer.FindConnectionPoint(IID_INetworkEvents, LConnectionPoint)) then
    begin
      LConnectionPoint.Advise(Self as IUnknown, FCookie);
      LConnectionPoint := nil;
    end;
  end;
end;

procedure TNetwork.Stop;
var
  LConnectionPointContainer: IConnectionPointContainer;
  LConnectionPoint: IConnectionPoint;
begin
  if (FCookie > 0) and Succeeded(FNetworkListManager.QueryInterface(IID_IConnectionPointContainer, LConnectionPointContainer)) then
  begin
    if Succeeded(LConnectionPointContainer.FindConnectionPoint(IID_INetworkEvents, LConnectionPoint)) then
    begin
      LConnectionPoint.Unadvise(FCookie);
      LConnectionPoint := nil;
    end;
  end;
end;

end.
