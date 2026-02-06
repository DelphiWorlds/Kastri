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
  // RTL
  System.Messaging, System.SysUtils,
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
    FGrantCompletionHandler: TGrantCompletionProc;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
  public
    { ILocationPermissions }
    function HasRequiredPermissions: Boolean;
    procedure Request(const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure Request(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure RequestBackground(const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure RequestBackground(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure ShowPermissionPrompt(const ACompletionHandler: TGrantCompletionProc); override;
    { ILocationManagerPermissionsDelegateOwner }
    procedure DidChangeAuthorization(const AManager: CLLocationManager);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.UITypes,
  // FMX
  FMX.Platform, FMX.DialogService.Async,
  // DW
  SimpleLog.Log,
  DW.OSDevice;

const
  cTapOKMessage = 'Tap OK to continue to the app settings page.'#13#10'After changing the settings, switch back to this app';
  cBackgroundPermissionMessageDefault = 'This app needs "Always" location access in order to work when the app is not active'#13#10 + cTapOKMessage;
  cLocationPermissionMessageDefault = 'This app needs location access in order to work'#13#10 + cTapOKMessage;

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
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  FLocationManagerPermissionsDelegate := TLocationManagerPermissionsDelegate.Create(Self);
  FLocationManager := TCLLocationManager.Create;
  FLocationManager.setDelegate(FLocationManagerPermissionsDelegate.GetObjectID);
end;

destructor TLocationPermissions.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
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

procedure TLocationPermissions.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
var
  LHandler: TGrantCompletionProc;
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      Log.d('TLocationPermissions.ApplicationEventMessageHandler > BecameActive');
      if Assigned(FGrantCompletionHandler) then
      begin
        Log.d('> Assigned(FGrantCompletionHandler)');
        LHandler := FGrantCompletionHandler;
        FGrantCompletionHandler := nil;
        LHandler(HasRequiredPermissions);
      end;
    end;
  end;
end;

procedure TLocationPermissions.ShowPermissionPrompt(const ACompletionHandler: TGrantCompletionProc);
var
  LMessage: string;
begin
  if not HasRequiredPermissions then
  begin
    LMessage := GetBackgroundPermissionMessage;
    if LMessage.IsEmpty then
    begin
      if GetNeedsBackgroundLocation then
        LMessage := cBackgroundPermissionMessageDefault
      else
        LMessage := cLocationPermissionMessageDefault
    end;
    TDialogServiceAsync.MessageDialog(LMessage, TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrOK then
        begin
          FGrantCompletionHandler := ACompletionHandler;
          TOSDevice.OpenAppSettings;
        end;
      end
    );
  end
  else
    ACompletionHandler(True);
end;

initialization
  LocationPermissions := TLocationPermissions.Create;

end.
