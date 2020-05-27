unit DW.LocationReceiver.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.Location, DW.MultiReceiver.Android;

type
  TPlatformLocation = class;

  TLocationReceiver = class(TMultiReceiver)
  private
    FPlatformLocation: TPlatformLocation;
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    constructor Create(const APlatformLocation: TPlatformLocation);
  end;

  TPlatformLocation = class(TCustomPlatformLocation)
  private
    FReceiver: TLocationReceiver;
  protected
    procedure LocationReceiverReceive(intent: JIntent);
  public
    constructor Create(const ALocation: TLocation); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Sensors,
  // Android
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  // DW
  DW.Consts.Android;

{ TLocationReceiver }

constructor TLocationReceiver.Create(const APlatformLocation: TPlatformLocation);
begin
  inherited Create(True);
  FPlatformLocation := APlatformLocation;
end;

procedure TLocationReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cLocationBroadcastAction));
end;

procedure TLocationReceiver.Receive(context: JContext; intent: JIntent);
begin
  FPlatformLocation.LocationReceiverReceive(intent);
end;

{ TPlatformLocation }

constructor TPlatformLocation.Create(const ALocation: TLocation);
begin
  inherited;
  FReceiver := TLocationReceiver.Create(Self);
end;

destructor TPlatformLocation.Destroy;
begin
  FReceiver.Free;
  inherited;
end;

procedure TPlatformLocation.LocationReceiverReceive(intent: JIntent);
var
  LLocation: TLocationCoord2D;
begin
  if intent.getAction.equals(StringToJString(cLocationBroadcastAction)) then
  begin
    LLocation.Latitude := intent.getDoubleExtra(StringToJString(cLocationBroadcastExtraLatitude), 0);
    LLocation.Longitude := intent.getDoubleExtra(StringToJString(cLocationBroadcastExtraLongitude), 0);
    DoLocationChanged(LLocation);
  end;
end;

end.
