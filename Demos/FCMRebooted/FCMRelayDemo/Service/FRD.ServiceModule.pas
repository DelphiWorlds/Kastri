unit FRD.ServiceModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os;

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
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.PushServiceNotification.Android,
  DW.OSLog;

procedure TServiceModule.AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
var
  LExtras: JBundle;
  LNotification: TAndroidPushServiceNotification;
begin
  TOSLog.d('+TServiceModule.AndroidIntentServiceHandleIntent');
  LExtras := AnIntent.getExtras;
  if LExtras <> nil then
  begin
    LNotification := TAndroidPushServiceNotification.Create(LExtras);
    try
      // Handle notification here
      TOSLog.d('> Notification:');
      TOSLog.d(LNotification.Json.ToJSON);
    finally
      LNotification.Free;
    end;
  end;
  TOSLog.d('-TServiceModule.AndroidIntentServiceHandleIntent');
end;

end.
