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
  Androidapi.Helpers,
  DW.OSLog, DW.Geofence, DW.Geofence.Android, DW.Androidapi.JNI.DWGeofence;

const
  cGeofenceTransitionCaptions: array[TGeofenceTransition] of string = ('Arrived at', 'Departed from');

procedure TServiceModule.AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
var
  LRegionIds: string;
  LTransition: TGeofenceTransition;
begin
  LRegionIds := JStringToString(AnIntent.getStringExtra(TJGeofenceIntentReceiver.JavaClass.EXTRA_TRANSITION_IDS));
  LTransition := TPlatformGeofenceManager.GetTransition(AnIntent.getIntExtra(TJGeofenceIntentReceiver.JavaClass.EXTRA_TRANSITION_TYPE, 1));
  TOSLog.d('AGDemoSrvice - %s: %s', [cGeofenceTransitionCaptions[LTransition], LRegionIds]);
  // Here you can handle the transition information in Delphi code, even if the app is not running
end;

end.
