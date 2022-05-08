unit FSD.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.App,
  DW.MultiReceiver.Android;

type
  TServiceModule = class;

  TLocalReceiver = class(TMultiReceiver)
  private
    FService: TServiceModule;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const AService: TServiceModule);
  end;

  TServiceModule = class(TAndroidService)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceDestroy(Sender: TObject);
  private
    FIsForeground: Boolean;
    FLocalReceiver: TLocalReceiver;
    FNotificationChannel: JNotificationChannel;
    FService: JService;
    procedure CreateNotificationChannel;
    procedure StartForeground;
    procedure StopForeground;
  protected
    procedure LocalReceiverReceive(intent: JIntent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  Androidapi.JNI.Support, Androidapi.Helpers,
  FSD.Consts,
  DW.OSLog,
  DW.Android.Helpers, DW.Androidapi.JNI.AndroidX.App;

const
  cServiceForegroundId = 3987; // Just a random number
  cNotificationChannelName = 'ForegroundServiceDemo';
  cServiceNotificationCaption = 'Foreground Service Demo';
  cServiceNotificationText = 'Foreground Service Demo';


{ TLocalReceiver }

constructor TLocalReceiver.Create(const AService: TServiceModule);
begin
  inherited Create(True);
  FService := AService;
end;

procedure TLocalReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceCommandAction));
end;

procedure TLocalReceiver.Receive(context: JContext; intent: JIntent);
begin
  FService.LocalReceiverReceive(intent);
end;

{ TServiceModule }

constructor TServiceModule.Create(AOwner: TComponent);
begin
  inherited;
  FLocalReceiver := TLocalReceiver.Create(Self);
  FService := TJService.Wrap(System.JavaContext);
  CreateNotificationChannel;
end;

destructor TServiceModule.Destroy;
begin
  // Do not use an overridden Destroy for cleanup in a service - use the OnDestroy event
  inherited;
end;

procedure TServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  FLocalReceiver.Free;
end;

procedure TServiceModule.CreateNotificationChannel;
begin
  FNotificationChannel := TJNotificationChannel.JavaClass.init(TAndroidHelper.Context.getPackageName, StrToJCharSequence(cNotificationChannelName),
    TJNotificationManager.JavaClass.IMPORTANCE_NONE);
  FNotificationChannel.setLightColor(TJColor.JavaClass.BLUE);
  FNotificationChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
  TAndroidHelperEx.NotificationManager.createNotificationChannel(FNotificationChannel);
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TServiceModule.LocalReceiverReceive(intent: JIntent);
var
  LCommand: Integer;
begin
  if intent.getAction.equals(StringToJString(cServiceCommandAction)) then
  begin
    LCommand := intent.getIntExtra(StringToJString(cServiceBroadcastParamCommand), 0);
    TOSLog.d('TServiceModule.LocalReceiverReceive received command: %d', [LCommand]);
    // Commands from the app
    case intent.getIntExtra(StringToJString(cServiceBroadcastParamCommand), 0) of
      cServiceCommandAppBecameActive:
        StopForeground;
      cServiceCommandAppEnteredBackground:
        StartForeground;
    end;
  end;
  // DoStatus;
end;

procedure TServiceModule.StartForeground;
var
  LBuilder: JNotificationCompat_Builder;
begin
  if FIsForeground or not TAndroidHelperEx.CheckBuildAndTarget(TAndroidHelperEx.OREO) then
    Exit; // <======
  TOSLog.d('TServiceModule.StartForeground');
  LBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context, TAndroidHelper.Context.getPackageName);
  LBuilder.setAutoCancel(True);
  LBuilder.setContentTitle(StrToJCharSequence(cServiceNotificationCaption));
  LBuilder.setContentText(StrToJCharSequence(cServiceNotificationText));
  LBuilder.setSmallIcon(TAndroidHelper.Context.getApplicationInfo.icon);
  LBuilder.setTicker(StrToJCharSequence(cServiceNotificationCaption));
  FService.startForeground(cServiceForegroundId, LBuilder.build);
  FIsForeground := True;
end;

procedure TServiceModule.StopForeground;
begin
  TOSLog.d('TServiceModule.StopForeground');
  if FIsForeground then
  begin
    FService.stopForeground(True);
    FIsForeground := False;
  end;
end;

end.
