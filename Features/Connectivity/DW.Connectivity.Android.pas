unit DW.Connectivity.Android;

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

// **** NOTE **** If the target device has API level 21 or greater, you need to add dw-kastri-base.jar from the Lib folder to the Libraries node of the 
// Android platform of the project in Project Manager

interface

uses
  // Android
  Androidapi.JNI.Net, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.DWNetworkCallback, DW.MultiReceiver.Android, DW.Connectivity;

type
  TPlatformConnectivity = class;

  TNetworks = TArray<JNetwork>;

  TNetworkCallbackDelegate = class(TJavaLocal, JDWNetworkCallbackDelegate)
  private
    class var FConnectivityManager: JConnectivityManager;
  private
    FCallback: JDWNetworkCallback;
    FConnectedNetworks: TNetworks;
    FIsPendingAvailable: Boolean;
    FPlatformConnectivity: TPlatformConnectivity;
    procedure ConnectivityChange;
    procedure CheckConnectivityChange;
    function IsConnectedToInternet: Boolean;
  protected
    class function ConnectivityManager: JConnectivityManager; static;
    class function GetConnectedNetworkInfoFromNetwork(const ANetwork: JNetwork; const ASkipValidation: Boolean): JNetworkInfo; static;
    class function GetConnectedNetworkInfo(const ASkipValidation: Boolean): JNetworkInfo; static;
  public
    { JDWNetworkCallbackDelegate }
    procedure onAvailable(network: JNetwork); cdecl;
    procedure onLost(network: JNetwork); cdecl;
    procedure onUnavailable; cdecl;
  public
    constructor Create(const APlatformConnectivity: TPlatformConnectivity);
  end;

  TConnectivityReceiver = class(TMultiReceiver)
  private
    FPlatformConnectivity: TPlatformConnectivity;
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    constructor Create(const APlatformConnectivity: TPlatformConnectivity);
  end;

  TPlatformConnectivity = class(TObject)
  private
    FCallbackDelegate: JDWNetworkCallbackDelegate;
    FConnectivity: TConnectivity;
    FIsConnectedToInternet: Boolean;
    FReceiver: TConnectivityReceiver;
  protected
    procedure ConnectivityChange(const AIsConnected: Boolean);
    function SkipValidation: Boolean;
  public
    class function IsConnectedToInternet: Boolean; static;
    class function IsWifiInternetConnection: Boolean; static;
  public
    constructor Create(const AConnectivity: TConnectivity);
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.SysUtils, System.Classes,
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI;

type
  TOpenConnectivity = class(TConnectivity);

{ TNetworkCallbackDelegate }

constructor TNetworkCallbackDelegate.Create(const APlatformConnectivity: TPlatformConnectivity);
begin
  inherited Create;
  FCallback := TJDWNetworkCallback.JavaClass.init(TAndroidHelper.Context, Self);
  FPlatformConnectivity := APlatformConnectivity;
end;

function TNetworkCallbackDelegate.IsConnectedToInternet: Boolean;
var
  LNetworks: TJavaObjectArray<JNetwork>;
  I: Integer;
begin
  Result := False;
  LNetworks := ConnectivityManager.getAllNetworks;
  try
    for I := 0 to LNetworks.Length - 1 do
    begin
      if GetConnectedNetworkInfoFromNetwork(LNetworks[I], FPlatformConnectivity.SkipValidation) <> nil then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    LNetworks.Sync;
  end;
end;

procedure TNetworkCallbackDelegate.CheckConnectivityChange;
begin
  Sleep(250);
  if FIsPendingAvailable then
    TThread.Synchronize(nil, ConnectivityChange);
end;

procedure TNetworkCallbackDelegate.ConnectivityChange;
begin
  FPlatformConnectivity.ConnectivityChange(IsConnectedToInternet);
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

procedure TNetworkCallbackDelegate.onAvailable(network: JNetwork);
begin
  TOSLog.d('TDWNetworkCallbackDelegate.onAvailable');
  FIsPendingAvailable := True;
  TThread.CreateAnonymousThread(CheckConnectivityChange).Start;
end;

procedure TNetworkCallbackDelegate.onLost(network: JNetwork);
begin
  TOSLog.d('TDWNetworkCallbackDelegate.onLost');
  FIsPendingAvailable := False;
  FPlatformConnectivity.ConnectivityChange(IsConnectedToInternet);
end;

procedure TNetworkCallbackDelegate.onUnavailable;
begin
  //
end;

// Based on: https://github.com/jamesmontemagno/ConnectivityPlugin/issues/56
class function TNetworkCallbackDelegate.GetConnectedNetworkInfo(const ASkipValidation: Boolean): JNetworkInfo;
var
  LAllNetworks: TJavaObjectArray<JNetwork>;
  LAllNetworkInfo: TJavaObjectArray<JNetworkInfo>;
  LInfo: JNetworkInfo;
  I: Integer;
begin
  Result := nil;
  if TJBuild_VERSION.JavaClass.SDK_INT >= 21 then
  begin
    LAllNetworks := ConnectivityManager.getAllNetworks;
    try
      for I := 0 to LAllNetworks.Length - 1 do
      begin
        LInfo := GetConnectedNetworkInfoFromNetwork(LAllNetworks[I], ASkipValidation);
        if LInfo <> nil then
        begin
          Result := LInfo;
          Break;
        end;
      end;
    finally
      LAllNetworks.Sync;
    end;
  end
  else
  begin
    LAllNetworkInfo := ConnectivityManager.getAllNetworkInfo;
    try
      for I := 0 to LAllNetworkInfo.Length - 1 do
      begin
        LInfo := LAllNetworkInfo[I];
        if (LInfo <> nil) and LInfo.isAvailable and LInfo.isConnected then
        begin
          Result := LInfo;
          Break;
        end;
      end;
    finally
      LAllNetworkInfo.Sync;
    end;
  end;
end;

class function TNetworkCallbackDelegate.GetConnectedNetworkInfoFromNetwork(const ANetwork: JNetwork; const ASkipValidation: Boolean): JNetworkInfo;
var
  LCapabilities: JNetworkCapabilities;
  LInfo: JNetworkInfo;
begin
  LInfo := nil;
  LCapabilities := ConnectivityManager.getNetworkCapabilities(ANetwork);
  // Check if the network has internet capability
  if (LCapabilities <> nil) and LCapabilities.hasCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_INTERNET) then
  begin
    // ..and is Validated or SDK < 23
    if ASkipValidation or (TJBuild_VERSION.JavaClass.SDK_INT < 23) or LCapabilities.hasCapability(TJNetworkCapabilities.JavaClass.NET_CAPABILITY_VALIDATED) then
    begin
      LInfo := ConnectivityManager.getNetworkInfo(ANetwork);
      if (LInfo <> nil) and LInfo.isAvailable and LInfo.isConnected then
        Result := LInfo;
    end;
    // else
    //   TOSLog.d('Not validated');
  end;
end;

{ TConnectivityReceiver }

constructor TConnectivityReceiver.Create(const APlatformConnectivity: TPlatformConnectivity);
begin
  inherited Create;
  FPlatformConnectivity := APlatformConnectivity;
end;

procedure TConnectivityReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJConnectivityManager.JavaClass.CONNECTIVITY_ACTION);
end;

procedure TConnectivityReceiver.Receive(context: JContext; intent: JIntent);
begin
  if TJBuild_VERSION.JavaClass.SDK_INT < 21 then
  begin
    TOSLog.d('TConnectivityReceiver.Receive');
    FPlatformConnectivity.ConnectivityChange(TNetworkCallbackDelegate.GetConnectedNetworkInfo(FPlatformConnectivity.SkipValidation) <> nil);
  end;
end;

{ TPlatformConnectivity }

constructor TPlatformConnectivity.Create(const AConnectivity: TConnectivity);
begin
  inherited Create;
  FConnectivity := AConnectivity;
  FIsConnectedToInternet := IsConnectedToInternet;
  if TJBuild_VERSION.JavaClass.SDK_INT >= 21 then
    FCallbackDelegate := TNetworkCallbackDelegate.Create(Self);
  FReceiver := TConnectivityReceiver.Create(Self);
  TOSLog.d('TPlatformConnectivity.Create > Connected: %s', [BoolToStr(FIsConnectedToInternet, True)]);
end;

destructor TPlatformConnectivity.Destroy;
begin
  // FCallbackDelegate.Free;
  FReceiver.Free;
  inherited;
end;

class function TPlatformConnectivity.IsConnectedToInternet: Boolean;
begin
  Result := TNetworkCallbackDelegate.GetConnectedNetworkInfo(False) <> nil;
end;

class function TPlatformConnectivity.IsWifiInternetConnection: Boolean;
var
  LInfo: JNetworkInfo;
begin
  LInfo := TNetworkCallbackDelegate.GetConnectedNetworkInfo(False);
  Result := (LInfo <> nil) and (LInfo.getType = TJConnectivityManager.JavaClass.TYPE_WIFI);
end;

function TPlatformConnectivity.SkipValidation: Boolean;
begin
  Result := FConnectivity.SkipValidation;
end;

procedure TPlatformConnectivity.ConnectivityChange(const AIsConnected: Boolean);
begin
  TOSLog.d('TPlatformConnectivity.ConnectivityChange(%s)', [BoolToStr(AIsConnected, True)]);
  if FIsConnectedToInternet <> AIsConnected then
  begin
    TOSLog.d('> Changed from %s', [BoolToStr(FIsConnectedToInternet, True)]);
    FIsConnectedToInternet := AIsConnected;
    TOpenConnectivity(FConnectivity).DoConnectivityChange(FIsConnectedToInternet);
  end;
end;

end.
