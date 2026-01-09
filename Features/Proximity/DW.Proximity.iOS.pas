unit DW.Proximity.iOS;

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
  System.TypInfo,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.Proximity;

type
  IProximityNotification = interface(NSObject)
    ['{2DBCEE10-97B3-4C63-AE7D-16B6555280E1}']
    procedure onProximityStateDidChange(notification: Pointer); cdecl;
  end;

  TPlatformProximity = class;

  TProximityNotificationListener = class(TOCLocal)
  private
    FPlatformProximity: TPlatformProximity;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const APlatformProximity: TPlatformProximity);
    procedure onProximityStateDidChange(notification: Pointer); cdecl;
  end;

  TPlatformProximity = class(TCustomPlatformProximity)
  private
    FListener: TProximityNotificationListener;
  protected
    procedure DoProximityChange(const AIsNear: Boolean); override;
  public
    constructor Create(const AProximity: TProximity); override;
    destructor Destroy; override;
  end;

implementation

uses
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers,
  // iOS
  iOSapi.Helpers,
  // DW
  DW.Macapi.Helpers;

{ TProximityNotificationListener }

constructor TProximityNotificationListener.Create(const APlatformProximity: TPlatformProximity);
begin
  inherited Create;
  FPlatformProximity := APlatformProximity;
  {$IF CompilerVersion < 37}
  TiOSHelper.DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('onProximityStateDidChange:'),
    StringToID('UIDeviceProximityStateDidChangeNotification'), nil);
  {$ELSE}
  TiOSHelper.DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('onProximityStateDidChange:'),
    StrToNSStr('UIDeviceProximityStateDidChangeNotification'), nil);
  {$ENDIF}
end;

function TProximityNotificationListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IProximityNotification);
end;

procedure TProximityNotificationListener.onProximityStateDidChange(notification: Pointer);
begin
  FPlatformProximity.DoProximityChange(TiOSHelper.CurrentDevice.proximityState);
end;

{ TPlatformProximity }

constructor TPlatformProximity.Create(const AProximity: TProximity);
begin
  inherited;
  FListener := TProximityNotificationListener.Create(Self);
  TiOSHelper.CurrentDevice.setProximityMonitoringEnabled(True);
end;

destructor TPlatformProximity.Destroy;
begin
  TiOSHelper.CurrentDevice.setProximityMonitoringEnabled(False);
  FListener.Free;
  inherited;
end;

procedure TPlatformProximity.DoProximityChange(const AIsNear: Boolean);
begin
  if BlankScreenWhenNear then
    TiOSHelper.SharedApplication.setIdleTimerDisabled(AIsNear);
  inherited;
end;

end.
