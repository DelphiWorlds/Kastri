unit CPL.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service, System.Sensors,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.App,
  DW.Location, DW.Background.Android, DW.MultiReceiver.Android, DW.JsonConfig,
  {$IF Defined(USE_FUSED_LOCATION)}
  DW.Location.FusedLocation.Android,
  {$ELSE}
  DW.Location.Android,
  {$ENDIF}
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

  TServiceLocationSettings = class(TJsonConfig)
  private
    FIsPaused: Boolean;
  public
    class function GetConfigFileName: string; override;
  public
    property IsPaused: Boolean read FIsPaused write FIsPaused;
  end;

  TServiceModule = class(TAndroidService)
    function AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
    procedure AndroidServiceDestroy(Sender: TObject);
  private
    FBackgroundMonitor: TBackgroundMonitor;
    FLocalReceiver: TLocalReceiver;
    FLocation: TLocation;
    FLocationSettings: TServiceLocationSettings;
    FNotificationChannel: JNotificationChannel;
    FTerminatedFlagFileName: string;
    FUpdater: TLocationUpdater;
    procedure BackgroundMonitorDozeChange(Sender: TObject; const AIsDozed: Boolean);
    procedure BackgroundMonitorScreenLockChange(Sender: TObject; const AIsScreenLocked: Boolean);
    procedure ChangeUpdates(const APause: Boolean);
    procedure ConnectToDatabase;
    procedure CreateNotificationChannel;
    procedure CreateObjects;
    procedure DestroyObjects;
    procedure DoMessage(const AMsg: string);
    procedure DoState(const AState: Integer);
    function HasBackgroundPermission: Boolean;
    function IsForeground: Boolean;
    {$IF Defined(USE_FUSED_LOCATION)}
    procedure LocationChangeHandler(Sender: TObject; const AData: TLocationData);
    procedure LocationStateChangeHandler(Sender: TObject);
    procedure LocationNmeaMessageHandler(Sender: TObject; const AMsg: string; const ATimestamp: Int64);
    {$ELSE}
    procedure LocationChangeHandler(Sender: TObject; const AData: TLocationData; const ASource: TLocationSource);
    {$ENDIF}
    procedure RestartService;
    procedure SendState;
    procedure StartForeground(const AMustStartForeground: Boolean);
    procedure StopForeground(const AIsStarting: Boolean);
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
  System.Permissions, System.IOUtils, System.DateUtils,
  Androidapi.Helpers, Androidapi.JNI.Support, AndroidApi.JNI.JavaTypes,
  {$IF Defined(CLOUDLOGGING)}
  Grijjy.CloudLogging,
  {$ENDIF}
  DW.OSLog,
  DW.Android.Helpers, DW.Consts.Android, DW.OSDevice,
  {$IF CompilerVersion < 35}
  DW.Androidapi.JNI.SupportV4,
  {$ELSE}
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager, DW.Androidapi.JNI.AndroidX.App,
  {$ENDIF}
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

function GetLogTime: string;
begin
  Result := FormatDateTime('mm-dd hh:nn:ss.zzz', Now);
end;

{ TServiceLocationSettings }

class function TServiceLocationSettings.GetConfigFileName: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'locationsettings.json');
end;

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
  CreateObjects;
  ConnectToDatabase;
  if not FLocationSettings.IsPaused and TAndroidHelperEx.IsServiceRunning(cServiceNameFull) then
    ChangeUpdates(False);
end;

procedure TServiceModule.AndroidServiceDestroy(Sender: TObject);
begin
  DestroyObjects;
  RestartService;
end;

procedure TServiceModule.ChangeUpdates(const APause: Boolean);
begin
  if APause then
    FLocation.Pause
  else
    FLocation.Resume;
  if FLocationSettings.IsPaused <> APause  then
  begin
    FLocationSettings.IsPaused := APause;
    FLocationSettings.Save;
  end;
end;

procedure TServiceModule.ConnectToDatabase;
begin
  LocationsDataModule := TLocationsDataModule.Create(Self);
  LocationsDataModule.Connect;
end;

procedure TServiceModule.CreateObjects;
begin
  TServiceLocationSettings.CreateFromFile(FLocationSettings);
  if FLocalReceiver = nil then
    FLocalReceiver := TLocalReceiver.Create(Self);
  if FBackgroundMonitor = nil then
  begin
    FBackgroundMonitor := TBackgroundMonitor.Create;
    // Set IsActive to false if the service does not appear to need background monitoring
    FBackgroundMonitor.IsActive := False;
    FBackgroundMonitor.ServiceName := cServiceNameFull;
    FBackgroundMonitor.OnDozeChange := BackgroundMonitorDozeChange;
    FBackgroundMonitor.OnScreenLockChange := BackgroundMonitorScreenLockChange;
    FBackgroundMonitor.DozeAlarmInterval := cMinimumDozeAlarmIntervalSecs;
  end;
  if FLocation = nil then
  begin
    FLocation := TLocation.Create;
    // *** Set this to True if the app cannot do without background access. Make sure the user is informed of this from within the app ***
    FLocation.NeedsBackgroundAccess := False;
    {$IF not Defined(USE_FUSED_LOCATION)}
    FLocation.MinimumChangeInterval := cMinimumChangeInterval;
    FLocation.TimerTask.Schedule(cLocationTimerInterval);
    {$ELSE}
    FLocation.OnStateChange := LocationStateChangeHandler;
    FLocation.OnNmeaMessage := LocationNmeaMessageHandler;
    // Uncomment this line and use the desired value from DW.Consts.Android
    // FLocation.Priority := cLocationPriorityBalancedPowerAccuracy;
    {$ENDIF}
    FLocation.OnLocationChange := LocationChangeHandler;
  end;
  if FUpdater = nil then
  begin
    FUpdater := TLocationUpdater.Create;
    FUpdater.URL := cUpdaterLocationURL;
  end;
end;

procedure TServiceModule.DestroyObjects;
begin
  FLocalReceiver.Free;
  FLocalReceiver := nil;
  FBackgroundMonitor.Free;
  FBackgroundMonitor := nil;
  FLocation.Free;
  FLocation := nil;
end;

function TServiceModule.HasBackgroundPermission: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(cPermissionAccessBackgroundLocation);
end;

procedure TServiceModule.DoMessage(const AMsg: string);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceMessageAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamMessage), StringToJString(GetLogTime + ': ' + AMsg));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

procedure TServiceModule.DoState(const AState: Integer);
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceStateAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamState), AState);
  TOSLog.d('Sending state broadcast: %s', [JStringToString(LIntent.toURI)]);
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

procedure TServiceModule.RestartService;
var
  LIntent: JIntent;
  LMustStartNormal: Integer;
begin
  TOSLog.d('Sending restart intent');
  LMustStartNormal := Ord(not HasBackgroundPermission);
  LIntent := TJIntent.JavaClass.init(StringToJString(cDWBroadcastReceiverActionServiceRestart));
  LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString(cDWBroadcastReceiverName));
  LIntent.putExtra(StringToJString('ServiceName'), StringToJString(cServiceNameFull));
  LIntent.putExtra(StringToJString('MustStartNormal'), LMustStartNormal);
  TAndroidHelper.Context.sendBroadcast(LIntent);
end;

function TServiceModule.AndroidServiceStartCommand(const Sender: TObject; const Intent: JIntent; Flags, StartId: Integer): Integer;
var
  LAlarm: Boolean;
begin
  CreateObjects;
  {$IF Defined(CLOUDLOGGING)}
  GrijjyLog.SetLogLevel(TgoLogLevel.Info);
  GrijjyLog.Connect(cCloudLoggingHost, cCloudLoggingName);
  {$ENDIF}
  if not TAndroidHelperEx.IsActivityForeground then
    StartForeground(False);
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
    ChangeUpdates(FLocationSettings.IsPaused);
  SendState;
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
    case LCommand of
      cServiceCommandAppBecameActive:
        StopForeground(False);
      // If the app is requesting permissions, the service *must* be put into the foreground (if Android requires it)
      cServiceCommandAppEnteredBackground, cServiceCommandAppIsRequestingPermissions:
        StartForeground(LCommand = cServiceCommandAppIsRequestingPermissions);
      cServiceCommandStartLocationUpdates:
        ChangeUpdates(False);
      cServiceCommandStopLocationUpdates:
        ChangeUpdates(True);
      cServiceCommandCheckState:
        SendState;
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
    StartForeground(False)
  else
    StopForeground(False);
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

procedure TServiceModule.StartForeground(const AMustStartForeground: Boolean);
var
  LBuilder: JNotificationCompat_Builder;
  LIsForegroundMandatory: Boolean;
  LIntent: JIntent;
begin
  LIsForegroundMandatory := not TAndroidHelperEx.IsActivityForeground and AMustStartForeground;
  // Only allow the service to start in the foreground if it needs to. One case is where the Android permissions dialog is showing!
  if not IsForeground and TOSVersion.Check(8) and (LIsForegroundMandatory or (not TOSVersion.Check(10) or HasBackgroundPermission)) then
  begin
    TOSLog.d('Starting foreground..');
    if FNotificationChannel = nil then
      CreateNotificationChannel;
    JavaService.stopForeground(True);
    // Create an intent for starting the app if the user taps the notification
    LIntent := TJIntent.Create;
    LIntent.setClassName(TAndroidHelper.Context.getPackageName, StringToJString('com.embarcadero.firemonkey.FMXNativeActivity'));
    LBuilder := TJNotificationCompat_Builder.JavaClass.init(TAndroidHelper.Context, TAndroidHelper.Context.getPackageName);
    LBuilder.setAutoCancel(True);
    LBuilder.setContentTitle(StrToJCharSequence(cServiceNotificationCaption));
    LBuilder.setContentText(StrToJCharSequence(cServiceNotificationText));
    LBuilder.setSmallIcon(TAndroidHelperEx.GetDefaultIconID);
    LBuilder.setTicker(StrToJCharSequence(cServiceNotificationCaption));
    LBuilder.setPriority(TJNotification.JavaClass.PRIORITY_MIN);
    LBuilder.setContentIntent(TJPendingIntent.JavaClass.getActivity(TAndroidHelper.Context, 0, LIntent, TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK));
    JavaService.startForeground(cServiceForegroundId, LBuilder.build);
    DoMessage('Service entered foreground mode');
  end;
end;

procedure TServiceModule.StopForeground(const AIsStarting: Boolean);
begin
  // Stop foreground service if the app is in the foreground, or if the service is foreground already
  if TOSVersion.Check(8) and IsForeground then
  begin
    TOSLog.d('Stopping foreground..');
    JavaService.stopForeground(True);
    if not AIsStarting then
      DoMessage('Service exited foreground mode');
  end;
end;

{$IF Defined(USE_FUSED_LOCATION)}
procedure TServiceModule.LocationStateChangeHandler(Sender: TObject);
begin
  TOSLog.d('TServiceModule.LocationStateChangeHandler > FLocation.IsPaused: %s', [BoolToStr(FLocation.IsPaused, True)]);
  SendState;
end;

procedure TServiceModule.LocationNmeaMessageHandler(Sender: TObject; const AMsg: string; const ATimestamp: Int64);
begin
  // There can be a lot of these, so use logging wisely
  TOSLog.d('TServiceModule.LocationNmeaMessageHandler > %s: %s', [FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', UnixToDateTime(ATimestamp div 1000, False)), AMsg]);
end;
{$ENDIF}

procedure TServiceModule.SendState;
begin
  if FLocation.IsPaused then
    DoState(cServiceStateLocationUpdatesPaused)
  else
    DoState(cServiceStateLocationUpdatesResumed);
end;

{$IF Defined(USE_FUSED_LOCATION)}
procedure TServiceModule.LocationChangeHandler(Sender: TObject; const AData: TLocationData);
{$ELSE}
procedure TServiceModule.LocationChangeHandler(Sender: TObject; const AData: TLocationData; const ASource: TLocationSource);
{$ENDIF}
var
  LStatus: Integer;
begin
  TOSLog.d('TServiceModule.LocationChangeHandler');
  // This is where to use the location data to update a server, or to a database when not connected to the internet and the data is to be sent later
  LStatus := 0;
  // Service in foreground means app is inactive
  if IsForeground then
    LStatus := 1;
  if FBackgroundMonitor.IsDozed then
    LStatus := 2;
//  FUpdater.SendLocation(ALocation, LStatus);
  LocationsDataModule.AddLocation(AData, LStatus);
end;

end.
