unit ABLS.ServiceModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os,
  DW.ServiceManager.Android,
  Androidapi.JNI.Location, DW.FusedLocation.Android;

type
  TServiceModule = class(TAndroidService, IFusedLocationOwner)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    FFusedLocation: IFusedLocation;
    FServiceManager: IServiceManager;
    procedure LocationReceived(const ALocation: JLocation);
    procedure LocationUpdatesChange(const AIsActive: Boolean);
  public
    { Public declarations }
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  Androidapi.JNI.App,
  DW.LocalBroadcast.Android;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  if FServiceManager = nil then
    FServiceManager := TServiceManager.Create(JavaService);
  FServiceManager.StartForeground('ABLSticky Demo', 'Foreground service', 'ABLSticky', 'ABLSticky Channel');
  if FFusedLocation = nil then
  begin
    // Using the "callback" method, so location updates are called via LocationReceived
    FFusedLocation := TFusedLocation.Create(Self, True);
    // Note that this starts location updates with the DEFAULT settings
    FFusedLocation.Start;
  end;
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TServiceModule.LocationReceived(const ALocation: JLocation);
begin
  LocalBroadcast.SendMessage(Format('{ "lat": %.5f, "lng": %.5f}', [ALocation.getLatitude, ALocation.getLongitude]));
end;

procedure TServiceModule.LocationUpdatesChange(const AIsActive: Boolean);
begin
  // For now, do nothing
end;

end.
