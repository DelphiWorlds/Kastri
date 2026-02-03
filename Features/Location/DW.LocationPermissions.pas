unit DW.LocationPermissions;

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

type
  TPermissionsState = (Denied, Restricted, Granted, NoBackground, RequestedAlways);

  TPermissionsStateHelper = record helper for TPermissionsState
    function AsString: string;
  end;

  TPermissionsCompleteProc = reference to procedure(const State: TPermissionsState);

  ILocationPermissions = interface(IInterface)
    ['{6DF2A4C1-92AB-4D68-BC82-275F59EA8B79}']
    function GetBackgroundPermissionMessage: string;
    function GetNeedsBackgroundLocation: Boolean;
    function HasRequiredPermissions: Boolean;
    procedure Request(const ACompletion: TPermissionsCompleteProc); overload;
    procedure Request(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload;
    procedure RequestBackground(const ACompletion: TPermissionsCompleteProc); overload;
    procedure RequestBackground(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload;
    procedure SetBackgroundPermissionMessage(const Value: string);
    procedure SetNeedsBackgroundLocation(const Value: Boolean);
    property BackgroundPermissionMessage: string read GetBackgroundPermissionMessage write SetBackgroundPermissionMessage;
    property NeedsBackgroundLocation: Boolean read GetNeedsBackgroundLocation write SetNeedsBackgroundLocation;
  end;

  TCustomLocationPermissions = class(TInterfacedObject, ILocationPermissions)
  private
    FBackgroundPermissionMessage: string;
    FNeedsBackgroundLocation: Boolean;
  protected
    FCompletion: TPermissionsCompleteProc;
    procedure PermissionsRequestComplete(const AState: TPermissionsState);
  public
    { ILocationPermissions }
    function GetBackgroundPermissionMessage: string;
    function GetNeedsBackgroundLocation: Boolean;
    function HasRequiredPermissions: Boolean; virtual;
    procedure Request(const ACompletion: TPermissionsCompleteProc); overload; virtual;
    procedure Request(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload; virtual;
    procedure RequestBackground(const ACompletion: TPermissionsCompleteProc); overload; virtual;
    procedure RequestBackground(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc); overload; virtual;
    procedure SetBackgroundPermissionMessage(const Value: string);
    procedure SetNeedsBackgroundLocation(const Value: Boolean);
  public
    constructor Create;
  end;

var
  LocationPermissions: ILocationPermissions;

implementation

uses
{$IF Defined(IOS)}
  DW.LocationPermissions.iOS;
{$ENDIF}
{$IF Defined(ANDROID)}
  DW.LocationPermissions.Android;
{$ENDIF}

{ TPermissionsStateHelper }

function TPermissionsStateHelper.AsString: string;
const
  cPermissionsStateCaptions: array[TPermissionsState] of string = (
    'Denied', 'Restricted', 'Granted', 'No Background', 'Requested Always'
  );
begin
  Result := cPermissionsStateCaptions[Self];
end;

{ TCustomLocationPermissions }

constructor TCustomLocationPermissions.Create;
begin
  inherited Create;
  FNeedsBackgroundLocation := True;
end;

function TCustomLocationPermissions.GetBackgroundPermissionMessage: string;
begin
  Result := FBackgroundPermissionMessage;
end;

function TCustomLocationPermissions.GetNeedsBackgroundLocation: Boolean;
begin
  Result := FNeedsBackgroundLocation;
end;

function TCustomLocationPermissions.HasRequiredPermissions: Boolean;
begin
  Result := False;
end;

procedure TCustomLocationPermissions.Request(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
begin
  //
end;

procedure TCustomLocationPermissions.Request(const ACompletion: TPermissionsCompleteProc);
begin
  //
end;

procedure TCustomLocationPermissions.RequestBackground(const ACompletion: TPermissionsCompleteProc);
begin
  //
end;

procedure TCustomLocationPermissions.RequestBackground(const AAdditionalPermissions: TArray<string>; const ACompletion: TPermissionsCompleteProc);
begin
  //
end;

procedure TCustomLocationPermissions.PermissionsRequestComplete(const AState: TPermissionsState);
begin
  if Assigned(FCompletion) then
    FCompletion(AState);
  FCompletion := nil;
end;

procedure TCustomLocationPermissions.SetBackgroundPermissionMessage(const Value: string);
begin
  FBackgroundPermissionMessage := Value
end;

procedure TCustomLocationPermissions.SetNeedsBackgroundLocation(const Value: Boolean);
begin
  FNeedsBackgroundLocation := Value;
end;

end.
