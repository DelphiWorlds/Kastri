unit FRD.ServiceModule;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os,
  DW.SMS;

type
  TServiceModule = class(TAndroidIntentService)
    procedure AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
    procedure AndroidIntentServiceDestroy(Sender: TObject);
  private
    FSMS: TSMS;
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
  DW.PushServiceNotification.Android, DW.Types,
  DW.OSLog;

procedure TServiceModule.AndroidIntentServiceDestroy(Sender: TObject);
begin
  FSMS.Free;
end;

procedure TServiceModule.AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
var
  LExtras: JBundle;
  LNotification: TAndroidPushServiceNotification;
  LText, LDestinations: string;
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
      // An example of what might be possible with this service - send an SMS from the device
      // Needs SMS authorization within the app
      if FSMS = nil then
        FSMS := TSMS.Create;
      if FSMS.GetAuthorizationStatus = TAuthorizationStatus.Authorized then
      begin
        if LNotification.Json.TryGetValue('SMSText', LText) and LNotification.Json.TryGetValue('SMSDest', LDestinations) then
          FSMS.SendTextMessage(LText, LDestinations.Split([';']))
        else
          TOSLog.e('One or more required fields (SMSText and SMSDest) are not present');
      end
      else
        TOSLog.e('Does not have SMS authorization');
    finally
      LNotification.Free;
    end;
  end;
  TOSLog.d('-TServiceModule.AndroidIntentServiceHandleIntent');
end;

end.
