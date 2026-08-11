unit DW.Connectivity.Android;

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

// **** NOTE **** This unit targets API level 29 (Android 10) or higher.
// You need to add dw-kastri-base.jar from the Lib folder to the Libraries node of the
// Android platform of the project in Project Manager.
// On devices with API level < 29 the connectivity monitoring and checks do nothing
// (IsConnectedToInternet / IsWifiInternetConnection return False).

interface

uses
  // Android
  Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.DWNetworkCallback, DW.Connectivity;

type
  TPlatformConnectivity = class;

  TNetworkCallbackDelegate = class(TJavaLocal, JDWNetworkCallbackDelegate)
  private
    class var FConnectivityManager: JConnectivityManager;
  private
    FCallback: JDWNetworkCallback;
    FPlatformConnectivity: TPlatformConnectivity;
    procedure ConnectivityChange;
    procedure CheckConnectivityChange;
    function IsConnectedToInternet: Boolean;
  protected
    class function ConnectivityManager: JConnectivityManager; static;
    /// <summary>
    ///   Returns True if the given network has internet capability (and is validated
    ///   unless ASkipValidation is True). Also requires NET_CAPABILITY_NOT_VPN.
    /// </summary>
    class function IsNetworkConnected(const ANetwork: JNetwork; const ASkipValidation: Boolean): Boolean; static;
    /// <summary>
    ///   Checks the active/default network for internet connectivity using non-deprecated APIs.
    /// </summary>
    class function HasInternetConnection(const ASkipValidation: Boolean): Boolean; static;
    /// <summary>
    ///   Checks whether the active network is Wi-Fi and has internet capability.
    /// </summary>
    class function HasWifiInternetConnection(const ASkipValidation: Boolean): Boolean; static;
  public
    { JDWNetworkCallbackDelegate }
    procedure onAvailable(network: JNetwork); cdecl;
    procedure onLost(network: JNetwork); cdecl;
    procedure onUnavailable; cdecl;
  public
    constructor Create(const APlatformConnectivity: TPlatformConnectivity);
  end;

  TPlatformConnectivity = class(TObject)
  private
    FCallbackDelegate: JDWNetworkCallbackDelegate;
    FConnectivity: TConnectivity;
    FIsConnectedToInternet: Boolean;
  protected
    procedure ConnectivityChange(const AIsConnected: Boolean);
    function SkipValidation: Boolean;
  public
    class function GetLocalAddresses: TIPAddresses;
    class function IsConnectedToInternet: Boolean; static;
    class function IsWifiInternetConnection: Boolean; static;
  public
    constructor Create(const AConnectivity: TConnectivity);
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.SysUtils, System.Classes,
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI, Androidapi.JNI.Java.Net;

type
  TOpenConnectivity = class(TConnectivity);

{ TNetworkCallbackDelegate }

constructor TNetworkCallbackDelegate.Create(const APlatformConnectivity: TPlatformConnectivity);
begin
  inherited Create;
  FCallback := TJDWNetworkCallback.JavaClass.init(TAndroidHelper.Context, Self, False);
  FPlatformConnectivity := APlatformConnectivity;
end;

class function TNetworkCallbackDelegate.ConnectivityManager: JConnectivityManager;
var
  LService: JObject;
begin
  if FConnectivityManager = nil then
  begin
    LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.CONNECTIVITY_SERVICE);
    FConnectivityManager := TJConnectivityManager.Wrap(LService);
  end;
  Result := FConnectivityManager;
end;

class function TNetworkCallbackDelegate.IsNetworkConnected(const ANetwork: JNetwork;
  const ASkipValidation: Boolean): Boolean;
var
  LCapabilities: JNetworkCapabilities;
begin
  Result := False;
  if ANetwork <> nil then
  begin
    LCapabilities := ConnectivityManager.getNetworkCapabilities(ANetwork);
    if (LCapabilities <> nil) and
       LCapabilities.hasCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_INTERNET) and
       LCapabilities.hasCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_NOT_VPN) then
    begin
      if ASkipValidation or LCapabilities.hasCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_VALIDATED) then
        Result := True;
    end;
  end;
end;

class function TNetworkCallbackDelegate.HasInternetConnection(const ASkipValidation: Boolean): Boolean;
var
  LNetwork: JNetwork;
begin
  Result := False;
  if TJBuild_VERSION.JavaClass.SDK_INT >= 29 then
  begin
    LNetwork := ConnectivityManager.getActiveNetwork;
    Result := IsNetworkConnected(LNetwork, ASkipValidation);
  end;
end;

class function TNetworkCallbackDelegate.HasWifiInternetConnection(const ASkipValidation: Boolean): Boolean;
var
  LNetwork: JNetwork;
  LCapabilities: JNetworkCapabilities;
begin
  Result := False;
  if TJBuild_VERSION.JavaClass.SDK_INT >= 29 then
  begin
    LNetwork := ConnectivityManager.getActiveNetwork;
    if LNetwork <> nil then
    begin
      LCapabilities := ConnectivityManager.getNetworkCapabilities(LNetwork);
      if (LCapabilities <> nil) and
         LCapabilities.hasTransport(TJNetworkCapabilities.JavaClass.TRANSPORT_WIFI) and
         LCapabilities.hasCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_INTERNET) and
         LCapabilities.hasCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_NOT_VPN) then
      begin
        if ASkipValidation or LCapabilities.hasCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_VALIDATED) then
          Result := True;
      end;
    end;
  end;
end;

function TNetworkCallbackDelegate.IsConnectedToInternet: Boolean;
begin
  Result := HasInternetConnection(FPlatformConnectivity.SkipValidation);
end;

procedure TNetworkCallbackDelegate.CheckConnectivityChange;
begin
  Sleep(500);
  TThread.Synchronize(nil, ConnectivityChange);
end;

procedure TNetworkCallbackDelegate.ConnectivityChange;
begin
  FPlatformConnectivity.ConnectivityChange(IsConnectedToInternet);
end;

procedure TNetworkCallbackDelegate.onAvailable(network: JNetwork);
begin
  TOSLog.d('TDWNetworkCallbackDelegate.onAvailable');
  TThread.CreateAnonymousThread(CheckConnectivityChange).Start;
end;

procedure TNetworkCallbackDelegate.onLost(network: JNetwork);
begin
  TOSLog.d('TDWNetworkCallbackDelegate.onLost');
  FPlatformConnectivity.ConnectivityChange(IsConnectedToInternet);
end;

procedure TNetworkCallbackDelegate.onUnavailable;
begin
  //
end;

{ TPlatformConnectivity }

constructor TPlatformConnectivity.Create(const AConnectivity: TConnectivity);
begin
  inherited Create;
  FConnectivity := AConnectivity;
  if TJBuild_VERSION.JavaClass.SDK_INT >= 29 then
  begin
    FIsConnectedToInternet := IsConnectedToInternet;
    FCallbackDelegate := TNetworkCallbackDelegate.Create(Self);
  end
  else
    TOSLog.w('API < 29: connectivity monitoring disabled');
end;

class function TPlatformConnectivity.GetLocalAddresses: TIPAddresses;
var
  LInterfaces, LAddresses: JEnumeration;
  LAddress: JInetAddress;
  LIPAddress: TIPAddress;
  LClassName: string;
begin
  Result := [];
  LInterfaces := TJNetworkInterface.JavaClass.getNetworkInterfaces;
  if LInterfaces <> nil then
  begin
    while LInterfaces.hasMoreElements do
    begin
      var LInterface := TJNetworkInterface.Wrap(LInterfaces.nextElement);
      LAddresses := LInterface.getInetAddresses;
      while LAddresses.hasMoreElements do
      begin
        LAddress := TJInetAddress.Wrap(LAddresses.nextElement);
        if not LAddress.isLoopbackAddress then
        begin
          LClassName := JStringToString(LAddress.getClass.getName);
          LIPAddress.InterfaceName := JStringToString(LInterface.getName);
          if LClassName.Contains('Inet4Address') then
            LIPAddress.Version := TIPVersion.IPv4
          else if LClassName.Contains('Inet6Address') then
            LIPAddress.Version := TIPVersion.IPv6
          else
            Continue;
          LIPAddress.IP := JStringToString(LAddress.getHostAddress);
          if LIPAddress.IP.IndexOf('%') > -1 then
            LIPAddress.IP := LIPAddress.IP.Substring(0, LIPAddress.IP.IndexOf('%'));
          Result := Result + [LIPAddress];
        end;
      end;
    end;
  end;
end;

class function TPlatformConnectivity.IsConnectedToInternet: Boolean;
begin
  Result := TNetworkCallbackDelegate.HasInternetConnection(False);
end;

class function TPlatformConnectivity.IsWifiInternetConnection: Boolean;
begin
  Result := TNetworkCallbackDelegate.HasWifiInternetConnection(False);
end;

function TPlatformConnectivity.SkipValidation: Boolean;
begin
  Result := FConnectivity.SkipValidation;
end;

procedure TPlatformConnectivity.ConnectivityChange(const AIsConnected: Boolean);
begin
  if FIsConnectedToInternet <> AIsConnected then
  begin
    FIsConnectedToInternet := AIsConnected;
    TOpenConnectivity(FConnectivity).DoConnectivityChange(FIsConnectedToInternet);
  end;
end;

end.

