unit DW.Proximity.Android;

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

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.Os,
  // DW
  DW.Androidapi.JNI.Hardware.Sensor, DW.Proximity;

type
  TPlatformProximity = class;

  TSensorEventListener = class(TJavaLocal, JSensorEventListener)
  private
    FPlatformProximity: TPlatformProximity;
  public
    { JSensorEventListener }
    procedure onAccuracyChanged(sensor: JSensor; accuracy: Integer); cdecl;
    procedure onSensorChanged(event: JSensorEvent); cdecl;
  public
    constructor Create(const APlatformProximity: TPlatformProximity);
  end;

  TPlatformProximity = class(TCustomPlatformProximity)
  private
    class var FSensorManager: JSensorManager;
    class function SensorManager: JSensorManager; static;
  private
    FListener: JSensorEventListener;
    FMaxRange: Double;
    FWakeLock: JPowerManager_WakeLock;
    procedure AcquireWakeLock;
    procedure GetWakeLock;
    procedure ReleaseWakeLock;
    procedure ResetWakeLock;
  protected
    procedure AccuracyChanged(sensor: JSensor; accuracy: Integer);
    procedure SensorChanged(const event: JSensorEvent);
    procedure SetBlankScreenWhenNear(const Value: Boolean); override;
  public
    constructor Create(const AProximity: TProximity); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers;

{ TSensorEventListener }

constructor TSensorEventListener.Create(const APlatformProximity: TPlatformProximity);
begin
  inherited Create;
  FPlatformProximity := APlatformProximity;
end;

procedure TSensorEventListener.onAccuracyChanged(sensor: JSensor; accuracy: Integer);
begin
  FPlatformProximity.AccuracyChanged(sensor, accuracy);
end;

procedure TSensorEventListener.onSensorChanged(event: JSensorEvent);
begin
  FPlatformProximity.SensorChanged(event);
end;

{ TPlatformProximity }

constructor TPlatformProximity.Create(const AProximity: TProximity);
var
  LSensor: JSensor;
begin
  inherited;
  FListener := TSensorEventListener.Create(Self);
  LSensor := SensorManager.getDefaultSensor(TJSensor.JavaClass.TYPE_PROXIMITY);
  FMaxRange := LSensor.getMaximumRange;
  SensorManager.registerListener(FListener, LSensor, TJSensorManager.JavaClass.SENSOR_DELAY_NORMAL);
end;

destructor TPlatformProximity.Destroy;
begin
  SensorManager.unregisterListener(FListener);
  ReleaseWakeLock;
  inherited;
end;

procedure TPlatformProximity.AcquireWakeLock;
begin
  if FWakeLock = nil then
    GetWakeLock;
  if not FWakeLock.isHeld then
    FWakeLock.acquire;
end;

procedure TPlatformProximity.GetWakeLock;
var
  LService: JObject;
  LPowerManager: JPowerManager;
begin
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.POWER_SERVICE);
  LPowerManager := TJPowerManager.Wrap(TAndroidHelper.JObjectToID(LService));
  FWakeLock := LPowerManager.newWakeLock(TJPowerManager.JavaClass.PROXIMITY_SCREEN_OFF_WAKE_LOCK, StringToJString('TProximity'));
end;

procedure TPlatformProximity.ReleaseWakeLock;
begin
  if (FWakeLock <> nil) and FWakeLock.isHeld then
    FWakeLock.release;
end;

procedure TPlatformProximity.ResetWakeLock;
begin
  ReleaseWakeLock;
  AcquireWakeLock;
end;

class function TPlatformProximity.SensorManager: JSensorManager;
var
  LService: JObject;
begin
  if FSensorManager = nil then
  begin
    LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.SENSOR_SERVICE);
    FSensorManager := TJSensorManager.Wrap(TAndroidHelper.JObjectToID(LService));
  end;
  Result := FSensorManager;
end;

procedure TPlatformProximity.SetBlankScreenWhenNear(const Value: Boolean);
begin
  inherited;
  if Value then
    AcquireWakeLock
  else
  begin
    ReleaseWakeLock;
    FWakeLock := nil;
  end;
end;

procedure TPlatformProximity.AccuracyChanged(sensor: JSensor; accuracy: Integer);
begin
  //
end;

procedure TPlatformProximity.SensorChanged(const event: JSensorEvent);
var
  LEventValues: TJavaArray<Single>;
  LIsNear: Boolean;
begin
//  TOSLog.d('TPlatformProximity.SensorChanged');
  LEventValues := event.values;
  try
//    TOSLog.d('> Value: %.4f, Max Range: %.4f', [LEventValues[0], FMaxRange]);
    LIsNear := LEventValues[0] < FMaxRange;
    if LIsNear and BlankScreenWhenNear then
      ResetWakeLock;
    DoProximityChange(LIsNear);
  finally
    LEventValues.Free;
  end;
end;

end.
