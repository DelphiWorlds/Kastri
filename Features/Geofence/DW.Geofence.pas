unit DW.Geofence;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

{$SCOPEDENUMS ON}

// ************ DO NOT ADD ANY FMX UNITS (or any that depend on them) HERE ***********
uses
  // RTL
  System.Classes, System.Sensors, System.Generics.Collections, System.SysUtils;

type
  TGeofenceTransition = (RegionEnter, RegionExit);

  TGeofenceTransitions = set of TGeofenceTransition;

const
  cTransitionsAll: TGeofenceTransitions = [TGeofenceTransition.RegionEnter, TGeofenceTransition.RegionExit];

type
  TGeofenceRegion = record
    ID: string;
    Coords: TLocationCoord2D;
    Radius: Double;
    Transitions: TGeofenceTransitions;
  end;

  TCustomPlatformGeofenceManager = class;

  TGeofenceRegions = class(TList<TGeofenceRegion>)
  private
    FManager: TCustomPlatformGeofenceManager;
    function IndexOfID(const AID: string): Integer;
  protected
    procedure Notify(const Item: TGeofenceRegion; Action: TCollectionNotification); override;
  public
    constructor Create(const AManager: TCustomPlatformGeofenceManager);
    function AddRegion(const AID: string; const ALatitude: Double; const ALongitude: Double; const ARadius: Double): Integer; overload;
    function AddRegion(const AID: string; const ALatitude: Double; const ALongitude: Double; const ARadius: Double;
      const ATransitions: TGeofenceTransitions): Integer; overload;
    procedure RemoveRegion(const AID: string);
    function ToJson: string;
  end;

  TGeofenceManager = class;

  TRegionChange = (Add, Remove);

  TCustomPlatformGeofenceManager = class(TObject)
  private
    FManager: TGeofenceManager;
    FRegions: TGeofenceRegions;
  protected
    procedure ApplicationBecameActive; virtual;
    procedure DoGeofenceActionComplete(const AAction: Integer; const AResult: Integer; const AErrorMessage: string); virtual;
    procedure DoGeofenceTransition(const ATransition: TGeofenceTransition; const ARegionIds: TArray<string>); virtual;
    function GetRegionsJson: string; virtual;
    function IsMonitoring: Boolean; virtual;
    procedure RegionChange(const ARegion: TGeofenceRegion; const AChange: TRegionChange); virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
    property Regions: TGeofenceRegions read FRegions;
  public
    constructor Create(const AManager: TGeofenceManager); virtual;
    destructor Destroy; override;
  end;

  TGeofenceActionCompleteEvent = procedure(Sender: TObject; const Action: Integer; const Result: Integer; const ErrorMessage: string) of object;
  TGeofenceTransitionEvent = procedure(Sender: TObject; const Transition: TGeofenceTransition; const RegionIds: TArray<string>) of object;
  TShowPermissionsEvent = procedure(Sender: TObject; const CompletionHandler: TProc) of object;

  TGeofenceManager = class(TObject)
  private
    FIsShowingAppSettings: Boolean;
    FPlatformManager: TCustomPlatformGeofenceManager;
    FOnGeofenceActionComplete: TGeofenceActionCompleteEvent;
    FOnGeofenceTransition: TGeofenceTransitionEvent;
    FOnPermissionsResult: TNotifyEvent;
    FOnShowBackgroundPermissions: TShowPermissionsEvent;
    procedure DoStart;
    function GetRegions: TGeofenceRegions;
    function GetIsMonitoring: Boolean;
    procedure DoPermissionsResult;
    procedure DoRequestBackgroundLocationPermission;
    procedure RequestBackgroundLocationPermission;
    procedure RequestForegroundLocationPermission;
    procedure RequestPermissions;
  protected
    procedure ApplicationBecameActive;
    procedure DoGeofenceActionComplete(const AAction: Integer; const AResult: Integer; const AErrorMessage: string);
    procedure DoGeofenceTransition(const ATransition: TGeofenceTransition; const ARegionIds: TArray<string>);
  public
    constructor Create;
    function HasPermissions: Boolean;
    procedure ShowAppSettings;
    procedure Start;
    procedure Stop;
    property IsMonitoring: Boolean read GetIsMonitoring;
    property Regions: TGeofenceRegions read GetRegions;
    property OnGeofenceActionComplete: TGeofenceActionCompleteEvent read FOnGeofenceActionComplete write FOnGeofenceActionComplete;
    property OnGeofenceTransition: TGeofenceTransitionEvent read FOnGeofenceTransition write FOnGeofenceTransition;
    property OnPermissionsResult: TNotifyEvent read FOnPermissionsResult write FOnPermissionsResult;
    property OnShowBackgroundPermissions: TShowPermissionsEvent read FOnShowBackgroundPermissions write FOnShowBackgroundPermissions;
  end;

implementation

// ************ DO NOT ADD ANY FMX UNITS (or any that depend on them) HERE ***********
uses
  // RTL
  System.Permissions,
  // DW
{$IF Defined(ANDROID)}
  DW.Geofence.Android,
{$ENDIF}
  DW.Consts.Android, DW.Permissions.Helpers, DW.OSDevice;

{$IF not Defined(ANDROID)}
type
  TPlatformGeofenceManager = class(TCustomPlatformGeofenceManager);
{$ENDIF}

{ TGeofenceRegions }

constructor TGeofenceRegions.Create(const AManager: TCustomPlatformGeofenceManager);
begin
  inherited Create;
  FManager := AManager;
end;

function TGeofenceRegions.IndexOfID(const AID: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].ID.Equals(AID) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TGeofenceRegions.AddRegion(const AID: string; const ALatitude, ALongitude, ARadius: Double): Integer;
begin
  Result := AddRegion(AID, ALatitude, ALongitude, ARadius, cTransitionsAll);
end;

function TGeofenceRegions.AddRegion(const AID: string; const ALatitude, ALongitude, ARadius: Double;
  const ATransitions: TGeofenceTransitions): Integer;
var
  LIndex: Integer;
  LRegion: TGeofenceRegion;
begin
  LIndex := IndexOfID(AID);
  if LIndex > -1 then
    Delete(LIndex);
  LRegion.ID := AID;
  LRegion.Coords := TLocationCoord2D.Create(ALatitude, ALongitude);
  LRegion.Radius := ARadius;
  LRegion.Transitions := ATransitions;
  Result := Add(LRegion);
end;

procedure TGeofenceRegions.Notify(const Item: TGeofenceRegion; Action: TCollectionNotification);
begin
  inherited;
  case Action of
    TCollectionNotification.cnAdded:
      FManager.RegionChange(Item, TRegionChange.Add);
    TCollectionNotification.cnRemoved:
      FManager.RegionChange(Item, TRegionChange.Remove);
  end;
end;

procedure TGeofenceRegions.RemoveRegion(const AID: string);
var
  LIndex: Integer;
begin
  LIndex := IndexOfID(AID);
  if LIndex > -1 then
    Delete(LIndex);
end;

function TGeofenceRegions.ToJson: string;
begin
  Result := FManager.GetRegionsJson;
end;

{ TCustomPlatformGeofenceManager }

constructor TCustomPlatformGeofenceManager.Create(const AManager: TGeofenceManager);
begin
  inherited Create;
  FManager := AManager;
  FRegions := TGeofenceRegions.Create(Self);
end;

destructor TCustomPlatformGeofenceManager.Destroy;
begin
  FRegions.Free;
  inherited;
end;

procedure TCustomPlatformGeofenceManager.ApplicationBecameActive;
begin
  FManager.ApplicationBecameActive;
end;

procedure TCustomPlatformGeofenceManager.DoGeofenceActionComplete(const AAction, AResult: Integer; const AErrorMessage: string);
begin
  FManager.DoGeofenceActionComplete(AAction, AResult, AErrorMessage);
end;

procedure TCustomPlatformGeofenceManager.DoGeofenceTransition(const ATransition: TGeofenceTransition; const ARegionIds: TArray<string>);
begin
  FManager.DoGeofenceTransition(ATransition, ARegionIds);
end;

function TCustomPlatformGeofenceManager.GetRegionsJson: string;
begin
  Result := '';
end;

function TCustomPlatformGeofenceManager.IsMonitoring: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformGeofenceManager.RegionChange(const ARegion: TGeofenceRegion; const AChange: TRegionChange);
begin
  //
end;

procedure TCustomPlatformGeofenceManager.Start;
begin
  //
end;

procedure TCustomPlatformGeofenceManager.Stop;
begin
  //
end;

{ TGeofenceManager }

constructor TGeofenceManager.Create;
begin
  inherited Create;
  FPlatformManager := TPlatformGeofenceManager.Create(Self);
end;

procedure TGeofenceManager.DoGeofenceActionComplete(const AAction, AResult: Integer; const AErrorMessage: string);
begin
  if Assigned(FOnGeofenceActionComplete) then
    FOnGeofenceActionComplete(Self, AAction, AResult, AErrorMessage);
end;

procedure TGeofenceManager.DoGeofenceTransition(const ATransition: TGeofenceTransition; const ARegionIds: TArray<string>);
begin
  if Assigned(FOnGeofenceTransition) then
    FOnGeofenceTransition(Self, ATransition, ARegionIds);
end;

function TGeofenceManager.GetIsMonitoring: Boolean;
begin
  Result := FPlatformManager.IsMonitoring;
end;

function TGeofenceManager.GetRegions: TGeofenceRegions;
begin
  Result := FPlatformManager.Regions;
end;

function TGeofenceManager.HasPermissions: Boolean;
begin
  Result := PermissionsService.IsEveryPermissionGranted([cPermissionAccessFineLocation, cPermissionAccessBackgroundLocation]);
end;

procedure TGeofenceManager.DoPermissionsResult;
begin
  if Assigned(FOnPermissionsResult) then
    FOnPermissionsResult(Self);
end;

procedure TGeofenceManager.DoRequestBackgroundLocationPermission;
begin
  PermissionsService.RequestPermissions([cPermissionAccessBackgroundLocation],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        DoStart;
      DoPermissionsResult;
    end
  );
end;

procedure TGeofenceManager.RequestBackgroundLocationPermission;
begin
  if Assigned(FOnShowBackgroundPermissions) then
    FOnShowBackgroundPermissions(Self, DoRequestBackgroundLocationPermission)
  else
    DoRequestBackgroundLocationPermission;
end;

procedure TGeofenceManager.RequestForegroundLocationPermission;
begin
  PermissionsService.RequestPermissions([cPermissionAccessFineLocation],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        RequestBackgroundLocationPermission
      else
        DoPermissionsResult;
    end
  );
end;

procedure TGeofenceManager.RequestPermissions;
begin
  if PermissionsService.IsPermissionGranted(cPermissionAccessFineLocation) then
    RequestBackgroundLocationPermission
  else
    RequestForegroundLocationPermission;
end;

procedure TGeofenceManager.ShowAppSettings;
begin
  TOSDevice.OpenAppSettings;
  FIsShowingAppSettings := True;
end;

procedure TGeofenceManager.ApplicationBecameActive;
begin
  if FIsShowingAppSettings and HasPermissions and not IsMonitoring then
    DoStart;
  FIsShowingAppSettings := False;
end;

procedure TGeofenceManager.Start;
begin
  if HasPermissions then
    DoStart
  else
    RequestPermissions;
end;

procedure TGeofenceManager.DoStart;
begin
  FPlatformManager.Start;
end;

procedure TGeofenceManager.Stop;
begin
  FPlatformManager.Stop;
end;

end.
