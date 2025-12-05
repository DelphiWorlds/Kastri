unit DW.LocationPermissions.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

implementation

uses
  // RTL
  System.Permissions, System.SysUtils, System.UITypes,
  // Android
  Androidapi.JNI.Support, Androidapi.Helpers,
  // FMX
  FMX.DialogService.Async,
  // DW
  DW.LocationPermissions, DW.Consts.Android, DW.Permissions.Helpers;

const
  cBackgroundPermissionMessageDefault = 'This application requires access to location updates in the background'#13#10#13#10 +
    'When prompted, please tap the "Allow in settings" option and select "Allow all the time"';

type
  TLocationPermissions = class(TCustomLocationPermissions)
  private
    procedure CheckRestricted(const APermissions: TPermissionArray);
    procedure DoRequestBackgroundLocationPermission;
    function GetBasePermissions: TArray<string>;
    procedure InternalRequest(const APermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
    procedure InternalRequestBackground(const APermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
    procedure RequestBackgroundLocationPermission;
    procedure RequestForegroundPermissions(const APermissions: TArray<string>);
    function ShouldShowRationale(const APermission: string): Boolean;
    procedure ShowBackgroundPermissionRationale(const APostRationaleProc: TProc);
  public
    function HasRequiredPermissions: Boolean; override;
    procedure Request(const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure Request(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure RequestBackground(const ACompletion: TPermissionsCompleteProc); overload; override;
    procedure RequestBackground(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload; override;
  end;

{ TLocationPermissions }

function TLocationPermissions.GetBasePermissions: TArray<string>;
begin
  Result := [cPermissionAccessFineLocation];
end;

function TLocationPermissions.HasRequiredPermissions: Boolean;
var
  LPermissions: TArray<string>;
begin
  LPermissions := GetBasePermissions;
  if GetNeedsBackgroundLocation then
    LPermissions := LPermissions + [cPermissionAccessBackgroundLocation];
  Result := PermissionsService.IsEveryPermissionGranted(LPermissions);
end;

procedure TLocationPermissions.InternalRequest(const APermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
begin
  if not Assigned(FCompletion) then
  begin
    FCompletion := ACompletion;
    RequestForegroundPermissions(APermissions);
  end;
end;

procedure TLocationPermissions.Request(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
begin
  InternalRequest(GetBasePermissions + AAdditionalPermissions, ACompletion);
end;

procedure TLocationPermissions.Request(const ACompletion: TPermissionsCompleteProc);
begin
  InternalRequest(GetBasePermissions, ACompletion);
end;

procedure TLocationPermissions.InternalRequestBackground(const APermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
begin
  if not Assigned(FCompletion) then
  begin
    FCompletion := ACompletion;
    if PermissionsService.IsEveryPermissionGranted(APermissions) then
      RequestBackgroundLocationPermission
    else
      RequestForegroundPermissions(APermissions);
  end;
end;

function TLocationPermissions.ShouldShowRationale(const APermission: string): Boolean;
begin
  Result := TJapp_ActivityCompat.JavaClass.shouldShowRequestPermissionRationale(TAndroidHelper.Activity, StringToJString(APermission));
end;

procedure TLocationPermissions.RequestBackground(const ACompletion: TPermissionsCompleteProc);
begin
  InternalRequestBackground(GetBasePermissions, ACompletion);
end;

procedure TLocationPermissions.RequestBackground(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
begin
  InternalRequestBackground(GetBasePermissions + AAdditionalPermissions, ACompletion);
end;

procedure TLocationPermissions.RequestBackgroundLocationPermission;
begin
  if TOSVersion.Check(10) then
    ShowBackgroundPermissionRationale(DoRequestBackgroundLocationPermission)
  else
    PermissionsRequestComplete(TPermissionsState.Granted);
end;

procedure TLocationPermissions.DoRequestBackgroundLocationPermission;
begin
  PermissionsService.RequestPermissions([cPermissionAccessBackgroundLocation],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
        PermissionsRequestComplete(TPermissionsState.Granted)
      else
        PermissionsRequestComplete(TPermissionsState.NoBackground);
    end
  );
end;

procedure TLocationPermissions.RequestForegroundPermissions(const APermissions: TArray<string>);
begin
  PermissionsService.RequestPermissions(APermissions,
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if AGrantResults.AreAllGranted then
      begin
        if GetNeedsBackgroundLocation then
          RequestBackgroundLocationPermission
        else
          PermissionsRequestComplete(TPermissionsState.Granted);
      end
      else
        CheckRestricted(APermissions);
    end
  );
end;

procedure TLocationPermissions.CheckRestricted(const APermissions: TPermissionArray);
var
  I: Integer;
  LState: TPermissionsState;
begin
  LState := TPermissionsState.Denied;
  for I := Low(APermissions) to High(APermissions) do
  begin
    if not ShouldShowRationale(APermissions[I]) then
    begin
      LState := TPermissionsState.Restricted;
      Break;
    end;
  end;
  PermissionsRequestComplete(LState);
end;

procedure TLocationPermissions.ShowBackgroundPermissionRationale(const APostRationaleProc: TProc);
var
  LMessage: string;
begin
  if ShouldShowRationale(cPermissionAccessBackgroundLocation) and not PermissionsService.IsPermissionGranted(cPermissionAccessBackgroundLocation) then
  begin
    LMessage := GetBackgroundPermissionMessage;
    if LMessage.IsEmpty then
      LMessage := cBackgroundPermissionMessageDefault;
    TDialogServiceAsync.MessageDialog(LMessage, TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0,
      procedure(const AResult: TModalResult)
      begin
        APostRationaleProc;
      end
    );
  end
  else
    APostRationaleProc;
end;

initialization
  LocationPermissions := TLocationPermissions.Create;

end.
