unit DW.Connectivity.Win;

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
  // DW
  DW.Connectivity;

type
  TPlatformConnectivity = class(TObject)
  private
    FConnectivity: TConnectivity;
  public
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
  Winapi.ActiveX, Winapi.Windows, Winapi.IpTypes, Winapi.IpHlpApi,
  // DW
  DW.Winapi.NetworkList_TLB;

type
  TNetwork = class(TInterfacedObject, INetworkEvents)
  private
    class var FCurrent: TNetwork;
    class constructor CreateClass;
    class destructor DestroyClass;
  private
    FCookie: Longint;
    FNetworkListManager: INetworkListManager;
    FIsConnectedToInternet: Boolean;
    function CoInitBegin: Boolean;
    procedure CoInitEnd(const ACoInit: Boolean);
    function GetWifiAdapterNames: TArray<string>;
    procedure Start;
    procedure Stop;
  protected
    function GetIsConnectedToInternet: Boolean;
    function GetHasWifiInternetConnection: Boolean;
  public
    class property Current: TNetwork read FCurrent;
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

const
  IID_IConnectionPointContainer: TGUID = (D1:$B196B284;D2:$BAB4;D3:$101A;D4:($B6,$9C,$00,$AA,$00,$34,$1D,$07));
  IF_TYPE_IEEE80211 = 71;

var
  Connectivity: TConnectivity;

{ TPlatformConnectivity }

constructor TPlatformConnectivity.Create(const AConnectivity: TConnectivity);
begin
  inherited Create;
  FConnectivity := AConnectivity;
  Connectivity := FConnectivity;
end;

destructor TPlatformConnectivity.Destroy;
begin
  Connectivity := nil;
  inherited;
end;

class function TPlatformConnectivity.IsConnectedToInternet: Boolean;
begin
  Result := TNetwork.Current.GetIsConnectedToInternet;
end;

class function TPlatformConnectivity.IsWifiInternetConnection: Boolean;
begin
  Result := TNetwork.Current.GetHasWifiInternetConnection;
end;

{ TNetwork }

class constructor TNetwork.CreateClass;
begin
  FCurrent := TNetwork.Create;
end;

constructor TNetwork.Create;
begin
  inherited;
  CoInitialize(nil);
  FNetworkListManager := CoNetworkListManager.Create;
  FIsConnectedToInternet := GetIsConnectedToInternet;
end;

destructor TNetwork.Destroy;
begin
  Stop;
  FNetworkListManager := nil;
  CoUninitialize;
  inherited;
end;

class destructor TNetwork.DestroyClass;
begin
  FCurrent.Free;
end;

function TNetwork.GetWifiAdapterNames: TArray<string>;
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

function TNetwork.GetHasWifiInternetConnection: Boolean;
var
  LConnections: IEnumNetworkConnections;
  LConnection: INetworkConnection;
  LFetched: LongWord;
  LWifiAdapterNames: TArray<string>;
  LAdapterName: string;
  LAdapterID: string;
  LCoInit: Boolean;
begin
  Result := False;
  LWifiAdapterNames := GetWifiAdapterNames;
  if Length(LWifiAdapterNames) > 0 then
  begin
    LCoInit := CoInitBegin;
    try
      LConnections := FNetworkListManager.GetNetworkConnections;
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

function TNetwork.CoInitBegin: Boolean;
begin
  Result := TThread.CurrentThread.ThreadID <> MainThreadID;
  if Result then
    CoInitialize(nil);
end;

procedure TNetwork.CoInitEnd(const ACoInit: Boolean);
begin
  if ACoInit then
    CoUninitialize;
end;

function TNetwork.GetIsConnectedToInternet: Boolean;
var
  LCoInit: Boolean;
begin
  LCoInit := CoInitBegin;
  try
    Result := FNetworkListManager.IsConnectedToInternet;
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
