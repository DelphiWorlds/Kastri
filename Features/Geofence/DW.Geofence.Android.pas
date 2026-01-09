unit DW.Geofence.Android;

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

// ************ DO NOT ADD ANY FMX UNITS (or any that depend on them) HERE ***********
uses
  // Android
  Androidapi.AppGlue, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.Geofence, DW.Androidapi.JNI.DWGeofence, DW.MultiReceiver.Android;

type
  TPlatformGeofenceManager = class;

  TGeofenceTransitionReceiver = class(TMultiReceiver)
  private
    FManager: TPlatformGeofenceManager;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AManager: TPlatformGeofenceManager);
  end;

  TGeofenceManagerDelegate = class(TJavaLocal, JGeofenceManagerDelegate)
  private
    FManager: TPlatformGeofenceManager;
  public
    { JGeofenceManagerDelegate }
    procedure onGeofenceActionComplete(action: Integer; result: Integer; errorMessage: JString); cdecl;
  public
    constructor Create(const AManager: TPlatformGeofenceManager);
  end;

  TPlatformGeofenceManager = class(TCustomPlatformGeofenceManager)
  private
    FDelegate: JGeofenceManagerDelegate;
    FGeofence: JGeofenceManager;
    FIsLoading: Boolean;
    FTransitionReceiver: TGeofenceTransitionReceiver;
    FSavedApplicationCommandEventHandler: TOnApplicationCommand;
    procedure ApplicationCommandEventHandler(const AAppGlue: TAndroidApplicationGlue; const ACommand: TAndroidApplicationCommand);
    function GetTransitionTypes(const ATransitions: TGeofenceTransitions): Integer;
    procedure HookApplicationCommandEvent;
    procedure LoadRegions;
    procedure UnhookApplicationCommandEvent;
  protected
    procedure DoGeofenceActionComplete(const AAction: Integer; const AResult: Integer; const AErrorMessage: string); override;
    procedure DoGeofenceTransition(const ATransition: TGeofenceTransition; const ARegionIds: TArray<string>); override;
    function GetRegionsJson: string; override;
    function IsMonitoring: Boolean; override;
    procedure RegionChange(const ARegion: TGeofenceRegion; const AChange: TRegionChange); override;
    procedure Start; override;
    procedure Stop; override;
  public
    class function GetTransition(const ATransitionType: Integer): TGeofenceTransition;
  public
    constructor Create(const AManager: TGeofenceManager); override;
    destructor Destroy; override;
  end;

implementation

// ************ DO NOT ADD ANY FMX UNITS (or any that depend on them) HERE ***********
uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.NativeActivity,
  // DW
  DW.Android.Helpers;

{ TGeofenceTransitionReceiver }

constructor TGeofenceTransitionReceiver.Create(const AManager: TPlatformGeofenceManager);
begin
  inherited Create(True);
  FManager := AManager;
end;

procedure TGeofenceTransitionReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJGeofenceIntentReceiver.JavaClass.ACTION_GEOFENCE_TRANSITION);
end;

procedure TGeofenceTransitionReceiver.Receive(context: JContext; intent: JIntent);
var
  LRegionIds: TArray<string>;
  LTransition: TGeofenceTransition;
begin
  LTransition := TPlatformGeofenceManager.GetTransition(intent.getIntExtra(TJGeofenceIntentReceiver.JavaClass.EXTRA_TRANSITION_TYPE, 1));
  LRegionIds :=JStringToString(intent.getStringExtra(TJGeofenceIntentReceiver.JavaClass.EXTRA_TRANSITION_IDS)).Split([',']);
  FManager.DoGeofenceTransition(LTransition, LRegionIds);
end;

{ TGeofenceManagerDelegate }

constructor TGeofenceManagerDelegate.Create(const AManager: TPlatformGeofenceManager);
begin
  inherited Create;
  FManager := AManager;
end;

procedure TGeofenceManagerDelegate.onGeofenceActionComplete(action, result: Integer; errorMessage: JString);
begin
  FManager.DoGeofenceActionComplete(action, result, JStringToString(errorMessage));
end;

{ TPlatformGeofenceManager }

constructor TPlatformGeofenceManager.Create(const AManager: TGeofenceManager);
begin
  inherited;
  HookApplicationCommandEvent;
  FTransitionReceiver := TGeofenceTransitionReceiver.Create(Self);
  FDelegate := TGeofenceManagerDelegate.Create(Self);
  FGeofence := TJGeofenceManager.JavaClass.init(TAndroidHelper.Context, FDelegate);
  FIsLoading := True;
  try
    LoadRegions;
  finally
    FIsLoading := False;
  end;
end;

destructor TPlatformGeofenceManager.Destroy;
begin
  FTransitionReceiver.Free;
  UnhookApplicationCommandEvent;
  inherited;
end;

procedure TPlatformGeofenceManager.HookApplicationCommandEvent;
var
  LAppGlue: TAndroidApplicationGlue;
begin
  if System.DelphiActivity <> nil then
  begin
    LAppGlue := PANativeActivity(System.DelphiActivity)^.instance;
    FSavedApplicationCommandEventHandler := LAppGlue.OnApplicationCommandEvent;
    LAppGlue.OnApplicationCommandEvent := ApplicationCommandEventHandler;
  end;
end;

procedure TPlatformGeofenceManager.UnhookApplicationCommandEvent;
var
  LAppGlue: TAndroidApplicationGlue;
begin
  // Hooking App Glue here so as to not introduce any FMX units
  if System.DelphiActivity <> nil then
  begin
    LAppGlue := PANativeActivity(System.DelphiActivity)^.instance;
    LAppGlue.OnApplicationCommandEvent := FSavedApplicationCommandEventHandler;
    FSavedApplicationCommandEventHandler := nil;
  end;
end;

procedure TPlatformGeofenceManager.ApplicationCommandEventHandler(const AAppGlue: TAndroidApplicationGlue;
  const ACommand: TAndroidApplicationCommand);
begin
  if ACommand = TAndroidApplicationCommand.Resume then
    ApplicationBecameActive;
  if Assigned(FSavedApplicationCommandEventHandler) then
    FSavedApplicationCommandEventHandler(AAppGlue, ACommand);
end;

class function TPlatformGeofenceManager.GetTransition(const ATransitionType: Integer): TGeofenceTransition;
begin
  case ATransitionType of
    1:
      Result := TGeofenceTransition.RegionEnter;
    2:
      Result := TGeofenceTransition.RegionExit;
  else
    Result := TGeofenceTransition.RegionEnter;
  end;
end;

procedure TPlatformGeofenceManager.DoGeofenceActionComplete(const AAction: Integer; const AResult: Integer; const AErrorMessage: string);
begin
  inherited;
end;

procedure TPlatformGeofenceManager.DoGeofenceTransition(const ATransition: TGeofenceTransition; const ARegionIds: TArray<string>);
begin
  inherited;
end;

function TPlatformGeofenceManager.GetRegionsJson: string;
begin
  Result := JStringToString(FGeofence.getRegions.toJson);
end;

function TPlatformGeofenceManager.GetTransitionTypes(const ATransitions: TGeofenceTransitions): Integer;
begin
  Result := 0;
  if TGeofenceTransition.RegionEnter in ATransitions then
    Result := Result or 1;
  if TGeofenceTransition.RegionExit in ATransitions then
    Result := Result or 2;
end;

function TPlatformGeofenceManager.IsMonitoring: Boolean;
begin
  Result := FGeofence.getIsMonitoring;
end;

procedure TPlatformGeofenceManager.LoadRegions;
var
  LIterator: JIterator;
  LJRegion: JGeofenceRegions_Region;
  LRegion: TGeofenceRegion;
begin
  LIterator := FGeofence.getRegions.getItems.entrySet.iterator;
  while LIterator.hasNext do
  begin
    LJRegion := TJGeofenceRegions_Region.Wrap(LIterator.next);
    LRegion.ID := JStringToString(LJRegion.getId);
    LRegion.Coords.Latitude := LJRegion.getCoords.latitude;
    LRegion.Coords.Longitude := LJRegion.getCoords.longitude;
    LRegion.Radius := LJRegion.getRadius;
    Regions.Add(LRegion);
  end;
end;

procedure TPlatformGeofenceManager.RegionChange(const ARegion: TGeofenceRegion; const AChange: TRegionChange);
begin
  if not FIsLoading then
  begin
    case AChange of
      TRegionChange.Add:
      begin
        FGeofence.getRegions.add(StringToJString(ARegion.ID), ARegion.Coords.Latitude, ARegion.Coords.Longitude, ARegion.Radius,
          GetTransitionTypes(ARegion.Transitions));
      end;
      TRegionChange.Remove:
        FGeofence.getRegions.remove(StringToJString(ARegion.ID));
    end;
  end;
end;

procedure TPlatformGeofenceManager.Start;
begin
  FGeofence.start;
end;

procedure TPlatformGeofenceManager.Stop;
begin
  FGeofence.stop;
end;

end.
