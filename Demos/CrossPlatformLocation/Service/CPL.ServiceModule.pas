unit CPL.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service, System.Sensors,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.App,
  DW.Location.Android, DW.Background.Android, DW.MultiReceiver.Android,
  CPL.LocationUpdater;

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
    FBackgroundMonitor: TBackgroundMonitor;
    FLocalReceiver: TLocalReceiver;
    FLocation: TLocation;
    FNotificationChannel: JNotificationChannel;
    FUpdater: TLocationUpdater;
    procedure BackgroundMonitorDozeChange(Sender: TObject; const AIsDozed: Boolean);
    procedure BackgroundMonitorScreenLockChange(Sender: TObject; const AIsScreenLocked: Boolean);
    procedure ConnectToDatabase;
    procedure CreateNotificationChannel;
    procedure CreateObjects;
    procedure DestroyObjects;
    function IsForeground: Boolean;
    procedure LocationChangeHandler(Sender: TObject; const ALocation: TLocationCoord2D; const ASource: TLocationSource);
    procedure RestartService;
    procedure StartForeground;
    procedure StopForeground;
  protected
    procedure LocalReceiverReceive(intent: JIntent);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  System.Permissions, System.IOUtils,
  Androidapi.Helpers, Androidapi.JNI.Support, AndroidApi.JNI.JavaTypes,
  {$IF Defined(CLOUDLOGGING)}
  Grijjy.CloudLogging,
  {$ENDIF}
  DW.OSLog,
  DW.Android.Helpers, DW.Consts.Android, DW.OSDevice,
  CPL.LocationsDataModule,
  CPL.Consts;

const
  cServiceNameFull = cEMBTJavaServicePrefix + cServiceName;
  cServiceNotificationCaption = 'Cross Platform Location Demo';
  cServiceNotificationText = 'Monitoring location changes';
  cServiceForegroundId = 3988; // Just a random number

  cNotificationChannelName = 'CrossPlatformLocationService';

  cMinimumDozeAlarmIntervalSecs = 5 * 60; // Once per 9 minutes is supposed to be the minimum when "dozed", apparently
  // cMinimumDozeAlarmIntervalSecs = 60; //!!!!!! Test value !!!!!!!
  cMinimumChangeInterval = 90; // Seconds

  cLocationTimerInterval = 120000; // Milliseconds

  cUpdaterLocationURL = ''; // Replace this with the URL you send updates to

{ TLocalReceiver }

constructor TLocalReceiver.Create(const AService: TServiceModule);
begin
  inherited Create(True);
  FService := AService;
end;

procedure TLocalReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceCommandAction));
  // IntentFilter.addAction(StringToJString(cDWBroadcastReceiverActionAlarmTimer));
end;

procedure TLocalReceiver.Receive(context: JContext; intent: JIntent);
begin
  FService.LocalReceiverReceive(intent);
end;

{ TServiceModule }

constructor TServiceModule.Create(AOwner: TComponent);
begin
  inherited;
  ConnectToDatabase;
  CreateObjects;
  if TAndroidHelperEx.IsServiceRunning(cServiceNameFull) then
    FLocation.Resume;
end;

procedure TServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  DestroyObjects;
  RestartService;
end;

procedure TServiceModule.ConnectToDatabase;
begin
  LocationsDataModule := TLocationsDataModule.Create(Self);
  LocationsDataModule.Connect;
end;

procedure TServiceModule.CreateObjects;
begin
  if FLocalReceiver = nil then
    FLocalReceiver := TLocalReceiver.Create(Self);
  if FBackgroundMonitor = nil then
  begin
    FBackgroundMonitor := TBackgroundMonitor.Create;
    FBackgroundMonitor.ServiceName := cServiceNameFull;
    FBackgroundMonitor.OnDozeChange := BackgroundMonitorDozeChange;
    FBackgroundMonitor.OnScreenLockChange := BackgroundMonitorScreenLockChange;
    FBackgroundMonitor.DozeAlarmInterval := cMinimumDozeAlarmIntervalSecs;
  end;
  if FLocation = nil then
  begin
    FLocation := TLocation.Create;
    FLocation.NeedsBackground := True;
    FLocation.MinimumChangeInterval := cMinimumChangeInterval;
    FLocation.OnLocationChange := LocationChangeHandler;
    FLocation.TimerTask.Schedule(cLocationTimerInterval);
  end;
  if FUpdater = nil then
  begin
    FUpdater := TLocationUpdater.Create;
    FUpdater.URL := cUpdaterLocationURL;
  end;
end;

procedure TServiceModule.DestroyObjects;
begin
  FLocalReceiver.DisposeOf;
  FLocalReceiver := nil;
  FBackgroundMonitor.DisposeOf;
  FBackgroundMonitor := nil;
  FLocation.DisposeOf;
  FLocation := nil;
end;

procedure TServiceModule.RestartService;
var
  LIntent: JIntent;
begin
  TOSLog.d('Sending restart intent');
  LIntent := TJIntent.JavaClass.init(StringToJString(cDWBroadcastReceiverActionServiceRestart));
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cDWBroadcastReceiverName));
  LIntent.putExtra(StringToJString('ServiceName'), StringToJString(cServiceNameFull));
  TAndroidHelper.Context.sendBroadcast(LIntent);
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
var
  LAlarm: Boolean;
begin
  {$IF Defined(CLOUDLOGGING)}
  GrijjyLog.SetLogLevel(TgoLogLevel.Info);
  GrijjyLog.Connect(cCloudLoggingHost, cCloudLoggingName);
  {$ENDIF}
  if not TAndroidHelperEx.IsActivityForeground then
    StartForeground;
  CreateObjects;
  // The broadcast receiver will send a start command when the doze alarm goes off, so there is a check here to see if that is why it was "started"
  LAlarm := (Intent <> nil) and JStringToString(Intent.getAction).Equals(cDWBroadcastReceiverActionServiceAlarm);
  if LAlarm and FBackgroundMonitor.IsDozed then
  begin
    TOSLog.d('Service started from doze alarm');
    FLocation.RequestLastKnownLocation;
    // Starts the next alarm
    FBackgroundMonitor.StartDozeAlarm;
  end
  else
    FLocation.Resume;
  Result := TJService.JavaClass.START_STICKY;
end;

procedure TServiceModule.LocalReceiverReceive(intent: JIntent);
var
  LCommand: Integer;
begin
  if intent.getAction.equals(StringToJString(cServiceCommandAction)) then
  begin
    LCommand := intent.getIntExtra(StringToJString(cServiceBroadcastParamCommand), 0);
    TOSLog.d('LocalReceiverReceive received command: %d', [LCommand]);
    // Commands from the app
    case intent.getIntExtra(StringToJString(cServiceBroadcastParamCommand), 0) of
      cServiceCommandAppBecameActive:
        StopForeground;
      cServiceCommandAppEnteredBackground:
        StartForeground;
      cServiceCommandStartLocationUpdates:
        FLocation.Resume;
      cServiceCommandStopLocationUpdates:
        FLocation.Pause;
    end;
  end;
end;

procedure TServiceModule.BackgroundMonitorDozeChange(Sender: TObject; const AIsDozed: Boolean);
var
  LMsg: string;
begin
  LMsg := Format('Doze Change: %s', [BoolToStr(AIsDozed, True)]);
  TOSLog.d(LMsg);
  // Update doze alarm interval if required, and dozed
  {$IF Defined(CLOUDLOGGING)}
  GrijjyLog.Send(LMsg, TgoLogLevel.Info);
  {$ENDIF}
end;

procedure TServiceModule.BackgroundMonitorScreenLockChange(Sender: TObject; const AIsScreenLocked: Boolean);
begin
  TOSLog.d('Screen Lock Change: %s', [BoolToStr(AIsScreenLocked, True)]);
  // If the screen is being locked, put the service into foreground mode so that it can still have network access
  if AIsScreenLocked then
    StartForeground
  else
    StopForeground;
end;

procedure TServiceModule.CreateNotificationChannel;
begin
  FNotificationChannel := TJNotificationChannel.JavaClass.init(TAndroidHelper.Context.getPackageName, StrToJCharSequence(cNotificationChannelName),
    TJNotificationManager.JavaClass.IMPORTANCE_NONE);
  FNotificationChannel.setLightColor(TJColor.JavaClass.BLUE);
  FNotificationChannel.setLockscreenVisibility(TJNotification.JavaClass.VISIBILITY_PRIVATE);
  TAndroidHelperEx.NotificationManager.createNotificationChannel(FNotificationChannel);
end;

function TServiceModule.IsForeground: Boolean;
begin
  Result := TAndroidHelperEx.IsServiceForeground(cServiceNameFull);
end;

procedure TServiceModule.StartForeground;
var
  LBuilder: JNotificationCompat_Builder;
begin
  if not TOSVersion.Check(8) or IsForeground then
    Exit; // <======
  TOSLog.d('StartForeground');
  if FNotificationChannel = nil then
    CreateNotificationChannel;
  JavaService.stopForeground(True);
  LBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context, TAndroidHelper.Context.getPackageName);
  LBuilder.setAutoCancel(True);
  LBuilder.setContentTitle(StrToJCharSequence(cServiceNotificationCaption));
  LBuilder.setContentText(StrToJCharSequence(cServiceNotificationText));
  LBuilder.setSmallIcon(TAndroidHelper.Context.getApplicationInfo.icon);
  LBuilder.setTicker(StrToJCharSequence(cServiceNotificationCaption));
  LBuilder.setPriority(TJNotification.JavaClass.PRIORITY_MIN);
  JavaService.startForeground(cServiceForegroundId, LBuilder.build);
end;

procedure TServiceModule.StopForeground;
begin
  // Do not stop foreground service if the app is not in the foreground, or if the service is not foreground already
  if not TOSVersion.Check(8) or not TAndroidHelperEx.IsActivityForeground or not IsForeground then
    Exit; // <======
  TOSLog.d('StopForeground');
  JavaService.stopForeground(True);
end;

procedure TServiceModule.LocationChangeHandler(Sender: TObject; const ALocation: TLocationCoord2D; const ASource: TLocationSource);
var
  LStatus: Integer;
begin
  // This is where to use the location data to update a server, or to a database when not connected to the internet and the data is to be sent later
  LStatus := 0;
  // Service in foreground means app is inactive
  if IsForeground then
    LStatus := 1;
  if FBackgroundMonitor.IsDozed then
    LStatus := 2;
//  FUpdater.SendLocation(ALocation, LStatus);
  LocationsDataModule.AddLocation(ALocation, LStatus);
end;

end.
