unit AG.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os;

type
  TServiceModule = class(TAndroidIntentService)
    procedure AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.Sensors,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.OSLog, DW.Geofence, DW.Geofence.Android, DW.Androidapi.JNI.DWGeofence;

const
  cGeofenceTransitionCaptions: array[TGeofenceTransition] of string = ('Arrived at', 'Departed from');

procedure TServiceModule.AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
var
  LRegionIds: string;
  LTransition: TGeofenceTransition;
  LCoords: TLocationCoord2D;
begin
  LRegionIds := JStringToString(AnIntent.getStringExtra(TJGeofenceIntentReceiver.JavaClass.EXTRA_TRANSITION_IDS));
  LTransition := TPlatformGeofenceManager.GetTransition(AnIntent.getIntExtra(TJGeofenceIntentReceiver.JavaClass.EXTRA_TRANSITION_TYPE, 1));
  LCoords.Latitude := AnIntent.getDoubleExtra(TJGeofenceIntentReceiver.JavaClass.EXTRA_TRANSITION_LATITUDE, 91);
  LCoords.Longitude := AnIntent.getDoubleExtra(TJGeofenceIntentReceiver.JavaClass.EXTRA_TRANSITION_LONGITUDE, 181);
  TOSLog.d('AGDemoService - %s: %s @ %.6f, %.6f', [cGeofenceTransitionCaptions[LTransition], LRegionIds, LCoords.Latitude, LCoords.Longitude]);
  // Here you can handle the transition information in Delphi code, even if the app is not running
end;

end.
