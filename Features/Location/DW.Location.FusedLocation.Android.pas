unit DW.Location.FusedLocation.Android;

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

// *** NOTE ***: Requires dw-fusedlocation.jar (in the Lib folder) to be added to the Libraries node under the Android platform in Project Manager

interface

uses
  // RTL
  System.Sensors, System.Classes,
  // Android
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.JNI.Location, Androidapi.JNI.JavaTypes,
  // DW
  DW.Androidapi.JNI.DWFusedLocation, DW.Location, DW.Location.Types;

type
  TLocation = class;

  TFusedLocationClientDelegate = class(TJavaLocal, JDWFusedLocationClientDelegate)
  private
    FLocation: TLocation;
  public
    { JDWFusedLocationClientDelegate }
    procedure onLocation(location: JLocation); cdecl;
    procedure onLocationUpdatesChange(active: Boolean); cdecl;
    procedure onSetMockLocationResult(location: JLocation); cdecl;
    procedure onSetMockModeResult(success: Boolean); cdecl;
  public
    constructor Create(const ALocation: TLocation);
  end;

  TNmeaMessageListener = class(TJavaLocal, JOnNmeaMessageListener)
  private
    FLocation: TLocation;
  public
    { JOnNmeaMessageListener }
    procedure onNmeaMessage(message: JString; timestamp: Int64); cdecl;
  public
    constructor Create(const ALocation: TLocation);
  end;

  TLocationChangeEvent = procedure(Sender: TObject; const Data: TLocationData) of object;
  TNmeaMessageEvent = procedure(Sender: TObject; const Msg: string; const Timestamp: Int64) of object;
  TSetMockLocationResultEvent = procedure(Sender: TObject; const Success: Boolean; const Location: TLocationCoord2D) of object;
  TSetMockModeResultEvent = procedure(Sender: TObject; const Success: Boolean) of object;

  TLocation = class(TObject)
  private
    FClient: JDWFusedLocationClient;
    FDelegate: JDWFusedLocationClientDelegate;
    FIsPaused: Boolean;
    FLastData: TLocationData;
    FLocationManager: JLocationManager;
    FNeedsBackgroundAccess: Boolean;
    FNmeaMessageListener: JOnNmeaMessageListener;
    FOnLocationChange: TLocationChangeEvent;
    FOnNmeaMessage: TNmeaMessageEvent;
    FOnSetMockLocationResult: TSetMockLocationResultEvent;
    FOnSetMockModeResult: TSetMockModeResultEvent;
    FOnStateChange: TNotifyEvent;
    procedure BroadcastLocation(const AData: TLocationData);
    procedure DoStateChange;
    function GetFastestInterval: Int64;
    function GetInterval: Int64;
    function GetIsMockMode: Boolean;
    function GetLocationData(const ALocation: JLocation): TLocationData;
    // function GetLocationMode: Integer;
    function GetPriority: Integer;
    function HasPermissions: Boolean;
    procedure SetFastestInterval(const Value: Int64);
    procedure SetInterval(const Value: Int64);
    procedure SetPriority(const Value: Integer);
  protected
    procedure LocationChange(const ALocation: JLocation);
    procedure NmeaMessage(const AMsg: JString; const ATimestamp: Int64);
    procedure SetIsPaused(const AValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Pauses location updates
    /// </summary>
    procedure Pause;
    /// <summary>
    ///   Requests the last known location for the device. If successful, the location will be passed to the OnLocationChange event
    /// </summary>
    procedure RequestLastKnownLocation;
    /// <summary>
    ///   Starts, or resumes, location updates
    /// </summary>
    procedure Resume;
    /// <summary>
    ///   Enables mock mode (if not already enabled) and sets the location. Calls the OnSetMockModeResult event (if not enabled already) and OnSetMockLocationResult
    /// </summary>
    procedure SetMockLocation(const ALocation: TLocationCoord2D);
    /// <summary>
    ///   Enables mock mode (if not already). Calls the OnSetMockModeResult event indicating whether or not it was successful
    /// </summary>
    procedure SetMockMode(const AIsMockMode: Boolean);
    property FastestInterval: Int64 read GetFastestInterval write SetFastestInterval;
    property Interval: Int64 read GetInterval write SetInterval;
    /// <summary>
    ///   Indicates whether or not mock mode is enabled
    /// </summary>
    property IsMockMode: Boolean read GetIsMockMode;
    property IsPaused: Boolean read FIsPaused;
    /// <summary>
    ///   Determines whether or not location services needs background access
    /// </summary>
    property NeedsBackgroundAccess: Boolean read FNeedsBackgroundAccess write FNeedsBackgroundAccess;
    property Priority: Integer read GetPriority write SetPriority;
    property OnLocationChange: TLocationChangeEvent read FOnLocationChange write FOnLocationChange;
    property OnNmeaMessage: TNmeaMessageEvent read FOnNmeaMessage write FOnNmeaMessage;
    /// <summary>
    ///   Called when SetMockLocation is called. If successful, Success will be true, so the Location parameter will be valid
    /// </summary>
    property OnSetMockLocationResult: TSetMockLocationResultEvent read FOnSetMockLocationResult write FOnSetMockLocationResult;
    property OnSetMockModeResult: TSetMockModeResultEvent read FOnSetMockModeResult write FOnSetMockModeResult;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
  end;

implementation

uses
  // RTL
  System.Permissions, System.SysUtils, System.DateUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.Provider,
  // DW
  DW.OSLog, DW.Consts.Android, DW.Geodetic,
  {$IF CompilerVersion < 35} DW.Androidapi.JNI.SupportV4 {$ELSE} DW.Androidapi.JNI.AndroidX.LocalBroadcastManager {$ENDIF};

const
  cDefaultLocationInterval = 10000;
  cDefaultLocationFastestInterval = 5000;

{ TFusedLocationClientDelegate }

constructor TFusedLocationClientDelegate.Create(const ALocation: TLocation);
begin
  inherited Create;
  FLocation := ALocation;
end;

procedure TFusedLocationClientDelegate.onLocation(location: JLocation);
begin
  FLocation.LocationChange(location);
end;

procedure TFusedLocationClientDelegate.onLocationUpdatesChange(active: Boolean);
begin
  TOSLog.d('Location updates active: %s', [BoolToStr(active, True)]);
  FLocation.SetIsPaused(not active);
end;

procedure TFusedLocationClientDelegate.onSetMockLocationResult(location: JLocation);
begin
  //
end;

procedure TFusedLocationClientDelegate.onSetMockModeResult(success: Boolean);
begin
  //
end;

{ TNmeaMessageListener }

constructor TNmeaMessageListener.Create(const ALocation: TLocation);
begin
  inherited Create;
  FLocation := ALocation;
end;

procedure TNmeaMessageListener.onNmeaMessage(message: JString; timestamp: Int64);
begin
  FLocation.NmeaMessage(message, timestamp);
end;

{ TLocation }

constructor TLocation.Create;
begin
  inherited;
  FIsPaused := True;
  FLastData.DateTime := 0;
  FLastData.Location := TLocationCoord2D.Create(cInvalidLatitude, cInvalidLongitude);
  FDelegate := TFusedLocationClientDelegate.Create(Self);
  FClient := TJDWFusedLocationClient.JavaClass.init(TAndroidHelper.Context, FDelegate);
  FClient.setInterval(cDefaultLocationInterval);
  FClient.setFastestInterval(cDefaultLocationFastestInterval);
  FClient.setPriority(cLocationPriorityHighAccuracy);
  FNmeaMessageListener := TNmeaMessageListener.Create(Self);
  FLocationManager := TJLocationManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE));
  FLocationManager.addNmeaListener(FNmeaMessageListener);
end;

destructor TLocation.Destroy;
begin
  //
  inherited;
end;

procedure TLocation.BroadcastLocation(const AData: TLocationData);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cLocationBroadcastAction));
  LIntent.putExtra(StringToJString(cLocationBroadcastExtraLatitude), AData.Location.Latitude);
  LIntent.putExtra(StringToJString(cLocationBroadcastExtraLongitude), AData.Location.Longitude);
  LIntent.putExtra(StringToJString(cLocationBroadcastExtraAccuracy), AData.Accuracy);
  LIntent.putExtra(StringToJString(cLocationBroadcastExtraAltitude), AData.Altitude);
  LIntent.putExtra(StringToJString(cLocationBroadcastExtraBearing), AData.Bearing);
  LIntent.putExtra(StringToJString(cLocationBroadcastExtraSpeed), AData.Speed);
  LIntent.putExtra(StringToJString(cLocationBroadcastExtraFlags), Byte(AData.Flags));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

function TLocation.HasPermissions: Boolean;
begin
  if TOSVersion.Check(10) and FNeedsBackgroundAccess then
    Result := PermissionsService.IsEveryPermissionGranted([cPermissionAccessBackgroundLocation, cPermissionAccessCoarseLocation, cPermissionAccessFineLocation])
  else
    Result := PermissionsService.IsEveryPermissionGranted([cPermissionAccessCoarseLocation, cPermissionAccessFineLocation]);
end;

{
function TLocation.GetLocationMode: Integer;
begin
  Result := TJSettings_Secure.JavaClass.getInt(TAndroidHelper.ContentResolver, TJSettings_Secure.JavaClass.LOCATION_MODE);
end;
}

procedure TLocation.RequestLastKnownLocation;
begin
  FClient.requestLastKnownLocation;
end;

function TLocation.GetFastestInterval: Int64;
begin
  Result := FClient.getFastestInterval;
end;

function TLocation.GetInterval: Int64;
begin
  Result := FClient.getInterval;
end;

function TLocation.GetIsMockMode: Boolean;
begin
  Result := FClient.getIsMockMode;
end;

function TLocation.GetPriority: Integer;
begin
  Result := FClient.getPriority;
end;

procedure TLocation.SetFastestInterval(const Value: Int64);
begin
  if Value <> FastestInterval then
    FClient.setFastestInterval(Value);
end;

procedure TLocation.SetInterval(const Value: Int64);
begin
  if Value <> Interval then
    FClient.setInterval(Value);
end;

procedure TLocation.SetPriority(const Value: Integer);
begin
  if Value <> Priority then
    FClient.setPriority(Value);
end;

procedure TLocation.SetIsPaused(const AValue: Boolean);
begin
  if FIsPaused <> AValue then
  begin
    FIsPaused := AValue;
    DoStateChange;
  end;
end;

procedure TLocation.SetMockLocation(const ALocation: TLocationCoord2D);
begin
  FClient.setMockLocation(ALocation.Latitude, ALocation.Longitude);
end;

procedure TLocation.SetMockMode(const AIsMockMode: Boolean);
begin
  FClient.setMockMode(AIsMockMode);
end;

procedure TLocation.DoStateChange;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

function TLocation.GetLocationData(const ALocation: JLocation): TLocationData;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Location := TLocationCoord2D.Create(ALocation.getLatitude, ALocation.getLongitude);
  Result.IsMocked := ALocation.isFromMockProvider;
  if ALocation.hasAccuracy then
  begin
    Include(Result.Flags, TLocationDataFlag.Accuracy);
    Result.Accuracy := ALocation.getAccuracy;
  end;
  if ALocation.hasAltitude then
  begin
    Include(Result.Flags, TLocationDataFlag.Altitude);
    Result.Altitude := ALocation.getAltitude;
  end;
  if ALocation.hasBearing then
  begin
    Include(Result.Flags, TLocationDataFlag.Bearing);
    Result.Bearing := ALocation.getBearing;
  end;
  if ALocation.hasSpeed then
  begin
    Include(Result.Flags, TLocationDataFlag.Speed);
    Result.Speed := ALocation.getSpeed;
  end
  else if (FLastData.DateTime > 0) and (FLastData.Location.Latitude <> cInvalidLatitude) then
  begin
    Include(Result.Flags, TLocationDataFlag.Speed);
    Result.Speed := TGeodetic.DistanceBetween(FLastData.Location, Result.Location) / SecondsBetween(Now, FLastData.DateTime);
  end;
end;

procedure TLocation.LocationChange(const ALocation: JLocation);
var
  LData: TLocationData;
begin
  LData := GetLocationData(ALocation);
  BroadcastLocation(LData);
  if Assigned(FOnLocationChange) then
    FOnLocationChange(Self, LData);
end;

procedure TLocation.NmeaMessage(const AMsg: JString; const ATimestamp: Int64);
begin
  if Assigned(FOnNmeaMessage) then
    FOnNmeaMessage(Self, JStringToString(AMsg), ATimestamp);
end;

procedure TLocation.Pause;
begin
  TOSLog.d('TLocation.Pause calling stopLocationUpdates');
  FClient.stopLocationUpdates;
end;

procedure TLocation.Resume;
begin
  FIsPaused := True;
  if HasPermissions then
    FClient.startLocationUpdates
  else
    TOSLog.d('Insufficient permissions to start location updates');
end;

end.
