unit DW.Location.iOS;

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
  System.Sensors, System.Sensors.Components, System.Messaging,
  // DW
  DW.Location, DW.Sensors, DW.Location.Types;

type
  TPlatformLocation = class(TCustomPlatformLocation)
  private
    FIsBackground: Boolean;
    FSensor: TLocationSensor;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
    procedure SensorLocationChangedHandler(Sender: TObject; const AOldLocation, ANewLocation: TLocationCoord2D);
    procedure UpdateActivity;
    procedure UpdateBackgroundMonitor;
  protected
    function GetIsActive: Boolean; override;
    procedure SetActivity(const Value: TLocationActivity); override;
    procedure SetIsActive(const AValue: Boolean); override;
    procedure SetUsage(const Value: TLocationUsage); override;
  public
    constructor Create(const ALocation: TLocation); override;
    destructor Destroy; override;
  end;

implementation

uses
  // FMX
  FMX.Platform,
  // DW
  DW.iOS.Sensors, DW.iOSapi.Helpers;

{ TPlatformLocation }

constructor TPlatformLocation.Create(const ALocation: TLocation);
begin
  inherited;
  FIsBackground := TiOSHelperEx.IsBackground;
  FSensor := TLocationSensor.Create(nil);
  FSensor.OnLocationChanged := SensorLocationChangedHandler;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TPlatformLocation.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  FSensor.Free;
  inherited;
end;

procedure TPlatformLocation.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
var
  LMessage: TApplicationEventMessage;
begin
  LMessage := TApplicationEventMessage(M);
  case LMessage.Value.Event of
    TApplicationEvent.BecameActive:
    begin
      FIsBackground := False;
      UpdateBackgroundMonitor;
    end;
    TApplicationEvent.EnteredBackground:
    begin
      FIsBackground := True;
      UpdateBackgroundMonitor;
    end;
  end;
end;

function TPlatformLocation.GetIsActive: Boolean;
begin
  Result := FSensor.Active;
end;

procedure TPlatformLocation.SetActivity(const Value: TLocationActivity);
begin
  FActivity := Value;
  UpdateActivity;
end;

procedure TPlatformLocation.SetIsActive(const AValue: Boolean);
begin
  FSensor.Active := AValue;
  UpdateActivity;
  UpdateBackgroundMonitor;
end;

procedure TPlatformLocation.SetUsage(const Value: TLocationUsage);
begin
  FUsage := Value;
  TiOSLocationSensorHelper.SetUsage(FUsage);
end;

procedure TPlatformLocation.UpdateActivity;
begin
  if FSensor.Active then
    TiOSLocationSensorHelper.SetLocationActivity(FActivity);
end;

procedure TPlatformLocation.UpdateBackgroundMonitor;
begin
  if FSensor.Active then
    TiOSLocationSensorHelper.SetBackgroundMonitor(FIsBackground);
end;

procedure TPlatformLocation.SensorLocationChangedHandler(Sender: TObject; const AOldLocation, ANewLocation: TLocationCoord2D);
var
  LData: TLocationData;
begin
  LData.Location := ANewLocation;
  if FSensor.Sensor <> nil then
  begin
    Include(LData.Flags, TLocationDataFlag.Accuracy);
    LData.Accuracy := FSensor.Sensor.Accuracy;
    if TCustomLocationSensor.TProperty.Altitude in FSensor.Sensor.AvailableProperties then
    begin
      Include(LData.Flags, TLocationDataFlag.Altitude);
      LData.Altitude := FSensor.Sensor.Altitude;
    end;
    if TCustomLocationSensor.TProperty.TrueHeading in FSensor.Sensor.AvailableProperties then
    begin
      Include(LData.Flags, TLocationDataFlag.Bearing);
      LData.Bearing := FSensor.Sensor.TrueHeading;
    end;
    if TCustomLocationSensor.TProperty.Speed in FSensor.Sensor.AvailableProperties then
    begin
      Include(LData.Flags, TLocationDataFlag.Speed);
      LData.Speed := FSensor.Sensor.Speed;
    end;
  end;
  DoLocationChanged(LData);
end;

end.
