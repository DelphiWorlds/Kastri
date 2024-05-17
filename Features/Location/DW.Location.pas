unit DW.Location;

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

uses
  // RTL
  System.Sensors,
  // DW
  DW.Sensors, DW.Location.Types;

type
  TLocation = class;

  TCustomPlatformLocation = class(TObject)
  private
    FLocation: TLocation;
  protected
    FActivity: TLocationActivity;
    FUsage: TLocationUsage;
    procedure DoLocationChanged(const AData: TLocationData);
    function GetIsActive: Boolean; virtual;
    procedure SetActivity(const Value: TLocationActivity); virtual;
    procedure SetIsActive(const AValue: Boolean); virtual;
    procedure SetUsage(const Value: TLocationUsage); virtual;
  public
    constructor Create(const ALocation: TLocation); virtual;
  end;

  TLocationChangedEvent = procedure(Sender: TObject; const Data: TLocationData) of object;

  TLocation = class(TObject)
  private
    FPlatformLocation: TCustomPlatformLocation;
    FOnLocationChanged: TLocationChangedEvent;
    function GetActivity: TLocationActivity;
    function GetIsActive: Boolean;
    function GetUsage: TLocationUsage;
    procedure SetActivity(const Value: TLocationActivity);
    procedure SetIsActive(const Value: Boolean);
    procedure SetUsage(const Value: TLocationUsage);
  protected
    procedure DoLocationChanged(const AData: TLocationData);
  public
    constructor Create;
    destructor Destroy; override;
    property Activity: TLocationActivity read GetActivity write SetActivity;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property Usage: TLocationUsage read GetUsage write SetUsage;
    property OnLocationChanged: TLocationChangedEvent read FOnLocationChanged write FOnLocationChanged;
  end;

implementation

{$IF Defined(IOS)}
uses
  DW.Location.iOS;
{$ELSEIF Defined(ANDROID) and Defined(LOCATIONRECEIVER)}
uses
  DW.LocationReceiver.Android;
{$ELSE}
type
  TPlatformLocation = class(TCustomPlatformLocation);
{$ENDIF}

{ TCustomPlatformLocation }

constructor TCustomPlatformLocation.Create(const ALocation: TLocation);
begin
  inherited Create;
  FLocation := ALocation;
end;

procedure TCustomPlatformLocation.DoLocationChanged(const AData: TLocationData);
begin
  FLocation.DoLocationChanged(AData);
end;

function TCustomPlatformLocation.GetIsActive: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformLocation.SetActivity(const Value: TLocationActivity);
begin
  //
end;

procedure TCustomPlatformLocation.SetIsActive(const AValue: Boolean);
begin
  //
end;

procedure TCustomPlatformLocation.SetUsage(const Value: TLocationUsage);
begin
  //
end;

{ TLocation }

constructor TLocation.Create;
begin
  inherited;
  FPlatformLocation := TPlatformLocation.Create(Self);
end;

destructor TLocation.Destroy;
begin
  FPlatformLocation.Free;
  inherited;
end;

procedure TLocation.DoLocationChanged(const AData: TLocationData);
begin
  if Assigned(FOnLocationChanged) then
    FOnLocationChanged(Self, AData);
end;

function TLocation.GetActivity: TLocationActivity;
begin
  Result := FPlatformLocation.FActivity
end;

function TLocation.GetIsActive: Boolean;
begin
  Result := FPlatformLocation.GetIsActive;
end;

function TLocation.GetUsage: TLocationUsage;
begin
  Result := FPlatformLocation.FUsage;
end;

procedure TLocation.SetActivity(const Value: TLocationActivity);
begin
  FPlatformLocation.SetActivity(Value);
end;

procedure TLocation.SetIsActive(const Value: Boolean);
begin
  FPlatformLocation.SetIsActive(Value);
end;

procedure TLocation.SetUsage(const Value: TLocationUsage);
begin
  FPlatformLocation.SetUsage(Value);
end;

end.
