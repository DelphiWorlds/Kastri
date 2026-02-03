unit DW.LocationPermissions.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CoreLocation,
  // DW
  DW.LocationPermissions;

type
  ILocationManagerPermissionsDelegateOwner = interface
    ['{409B526B-E42E-4582-9AC2-BC5D9D950D89}']
    procedure DidChangeAuthorization(const AManager: CLLocationManager);
  end;

  CLLocationManagerPermissionsDelegate = interface(IObjectiveC)
    ['{4CE6C297-3696-41B0-9DF2-3A84754BC6FF}']
    [MethodName('locationManager:didChangeAuthorizationStatus:')]
    procedure locationManagerDidChangeAuthorization(manager: CLLocationManager); cdecl;
  end;

  TLocationManagerPermissionsDelegate = class(TOCLocal, CLLocationManagerPermissionsDelegate)
  private
    FOwner: ILocationManagerPermissionsDelegateOwner;
  public
    { CLLocationManagerPermissionsDelegate }
    procedure locationManagerDidChangeAuthorization(manager: CLLocationManager); cdecl;
  public
    constructor Create(const AOwner: ILocationManagerPermissionsDelegateOwner);
  end;

  TLocationPermissions = class(TCustomLocationPermissions, ILocationManagerPermissionsDelegateOwner)
  private
    FLocationManager: CLLocationManager;
    FLocationManagerPermissionsDelegate: TLocationManagerPermissionsDelegate;
  public
    { ILocationPermissions }
    function HasRequiredPermissions: Boolean;
    procedure Request(const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure Request(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure RequestBackground(const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure RequestBackground(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload; override;
    { ILocationManagerPermissionsDelegateOwner }
    procedure DidChangeAuthorization(const AManager: CLLocationManager);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TLocationManagerPermissionsDelegate }

constructor TLocationManagerPermissionsDelegate.Create(const AOwner: ILocationManagerPermissionsDelegateOwner);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TLocationManagerPermissionsDelegate.locationManagerDidChangeAuthorization(manager: CLLocationManager);
begin
  FOwner.DidChangeAuthorization(manager);
end;

{ TLocationPermissions }

constructor TLocationPermissions.Create;
begin
  inherited Create;
  FLocationManagerPermissionsDelegate := TLocationManagerPermissionsDelegate.Create(Self);
  FLocationManager := TCLLocationManager.Create;
  FLocationManager.setDelegate(FLocationManagerPermissionsDelegate.GetObjectID);
end;

destructor TLocationPermissions.Destroy;
begin
  FLocationManagerPermissionsDelegate.Free;
  inherited;
end;

procedure TLocationPermissions.DidChangeAuthorization(const AManager: CLLocationManager);
var
  LState: TPermissionsState;
begin
  LState := TPermissionsState.Denied;
  case FLocationManager.authorizationStatus of
    kCLAuthorizationStatusAuthorizedWhenInUse:
    begin
      if GetNeedsBackgroundLocation then
        LState := TPermissionsState.NoBackground
      else
        LState := TPermissionsState.Granted;
   end;
   kCLAuthorizationStatusAuthorizedAlways:
     LState := TPermissionsState.Granted;
   kCLAuthorizationStatusRestricted:
     LState := TPermissionsState.Restricted;
  end;
  PermissionsRequestComplete(LState);
end;

function TLocationPermissions.HasRequiredPermissions: Boolean;
begin
  Result := FLocationManager.authorizationStatus = kCLAuthorizationStatusAuthorizedWhenInUse;
  if GetNeedsBackgroundLocation then
    Result := FLocationManager.authorizationStatus = kCLAuthorizationStatusAuthorizedAlways;
end;

procedure TLocationPermissions.Request(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
begin
  Request(ACompletion);
end;

procedure TLocationPermissions.Request(const ACompletion: TPermissionsCompleteProc);
begin
  if not Assigned(FCompletion) then
  begin
    FCompletion := ACompletion;
    case FLocationManager.authorizationStatus of
      kCLAuthorizationStatusNotDetermined, kCLAuthorizationStatusAuthorizedWhenInUse:
      begin
        if GetNeedsBackgroundLocation then
        begin
          FLocationManager.requestAlwaysAuthorization;
          // iOS no longer prompts the user immediately, so a state of RequestedAlways is needed. Sheesh
          PermissionsRequestComplete(TPermissionsState.RequestedAlways);
        end
        else if FLocationManager.authorizationStatus = kCLAuthorizationStatusAuthorizedWhenInUse then
          PermissionsRequestComplete(TPermissionsState.Granted)
        else
          FLocationManager.requestWhenInUseAuthorization;
      end;
      kCLAuthorizationStatusAuthorizedAlways:
        PermissionsRequestComplete(TPermissionsState.Granted);
      kCLAuthorizationStatusRestricted:
        PermissionsRequestComplete(TPermissionsState.Restricted);
      kCLAuthorizationStatusDenied:
        PermissionsRequestComplete(TPermissionsState.Denied);
    end;
  end;
end;

procedure TLocationPermissions.RequestBackground(const ACompletion: TPermissionsCompleteProc);
begin
  SetNeedsBackgroundLocation(True);
  Request(ACompletion);
end;

procedure TLocationPermissions.RequestBackground(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
begin
  RequestBackground(ACompletion);
end;

initialization
  LocationPermissions := TLocationPermissions.Create;

end.
