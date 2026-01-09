unit DW.BackgroundTasks.iOS;

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
  System.Messaging,
  // iOS
  iOSapi.Foundation,
  // DW
  DW.iOSapi.BackgroundTasks;

type
  TBackgroundRefreshStatus = (Unknown, Available, Denied, Restricted);

  TBackgroundRefreshStatusChangeMessage = class(TMessage<TBackgroundRefreshStatus>);

  IBackgroundTask = interface(IInterface)
    ['{852CB60F-DBCC-4F87-A407-CFC352C85540}']
    function GetInterval: Double;
    procedure SetInterval(const AInterval: Double);
    property Interval: Double read GetInterval write SetInterval;
  end;

  TBackgroundTaskKind = (AppRefresh, Processing);

  TCustomBackgroundTask = class(TInterfacedObject, IBackgroundTask)
  private
    FInterval: Double;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure DoScheduleError(const AError: string);
    function GetIdentifierNative: NSString;
    procedure PerformTask(const task: BGTask);
    procedure Reschedule;
    procedure ScheduleAppRefreshTask;
    procedure ScheduleNext;
    procedure ScheduleProcessingTask;
    procedure ScheduleTask(const ARequest: BGTaskRequest);
    function SharedScheduler: BGTaskScheduler;
    procedure TaskComplete(const task: BGTask; const ASuccess: Boolean);
    procedure TaskHandler(task: BGTask);
  protected
    procedure Cancel; virtual;
    function Execute: Boolean; virtual; abstract;
    function GetIdentifier: string; virtual; abstract;
    function GetRequiresExternalPower: Boolean; virtual;
    function GetRequiresNetworkConnectivity: Boolean; virtual;
    function GetTaskKind: TBackgroundTaskKind; virtual; abstract;
    procedure ScheduleError(const AError: string); virtual;
  public
    { IBackgroundTask }
    function GetInterval: Double;
    procedure SetInterval(const AInterval: Double);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TBackgroundTaskRequirement = (Connectivity, ExternalPower);
  TBackgroundTaskRequirements = set of TBackgroundTaskRequirement;

  TBackgroundTaskProc = reference to function: Boolean;

  TBackgroundRefreshStatusChangeEvent = procedure(Sender: TObject; const AStatus: TBackgroundRefreshStatus) of object;

  IBackgroundTaskManager = interface(IInterface)
    ['{C6B41CF5-9356-4D48-B83C-39C555CDE523}']
    function GetBackgroundRefreshStatus: TBackgroundRefreshStatus;
    function GetOnBackgroundRefreshStatusChange: TBackgroundRefreshStatusChangeEvent;
    function ScheduleTask(const AIdentifier: string; const ATaskKind: TBackgroundTaskKind; const AInterval: Double;
      const AHandler: TBackgroundTaskProc; const ARequirements: TBackgroundTaskRequirements = []): IBackgroundTask;
    procedure SetOnBackgroundRefreshStatusChange(const AValue: TBackgroundRefreshStatusChangeEvent);
    property BackgroundRefreshStatus: TBackgroundRefreshStatus read GetBackgroundRefreshStatus;
    property OnBackgroundRefreshStatusChange: TBackgroundRefreshStatusChangeEvent read GetOnBackgroundRefreshStatusChange
      write SetOnBackgroundRefreshStatusChange;
  end;

var
  BackgroundTaskManager: IBackgroundTaskManager;

implementation

uses
  // RTL
  System.TypInfo, System.Classes, System.SysUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjectiveC, Macapi.ObjCRuntime,
  // iOS
  iOSapi.Helpers, iOSapi.UIKit,
  // FMX
  FMX.Platform,
  // DW
  DW.OSLog;

type
  TBackgroundTask = class(TCustomBackgroundTask)
  private
    FHandler: TBackgroundTaskProc;
    FIdentifier: string;
    FInterval: Double;
    FTaskKind: TBackgroundTaskKind;
    FRequirements: TBackgroundTaskRequirements;
  protected
    function Execute: Boolean; override;
    function GetIdentifier: string; override;
    function GetRequiresExternalPower: Boolean; override;
    function GetRequiresNetworkConnectivity: Boolean; override;
    function GetTaskKind: TBackgroundTaskKind; override;
  public
    constructor Create(const AIdentifier: string; const ATaskKind: TBackgroundTaskKind; const AInterval: Double; const AHandler: TBackgroundTaskProc;
      const ARequirements: TBackgroundTaskRequirements);
  end;

  IBackgroundRefreshStatusNotification = interface(NSObject)
    ['{D8EE8419-FAE7-41D3-8DE9-91F1F5D4E007}']
    procedure backgroundRefreshStatusDidChange(notification: Pointer); cdecl;
  end;

  TBackgroundTaskManager = class;

  TBackgroundRefreshStatusNotificationListener = class(TOCLocal)
  private
    FBackgroundTaskManager: TBackgroundTaskManager;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const ABackgroundTaskManager: TBackgroundTaskManager);
    procedure backgroundRefreshStatusDidChange(notification: Pointer); cdecl;
  end;

  TBackgroundTaskManager = class(TInterfacedObject, IBackgroundTaskManager)
  private
    const cDefaultRequirements = [TBackgroundTaskRequirement.Connectivity, TBackgroundTaskRequirement.ExternalPower];
  private
    FOnBackgroundRefreshStatusChange: TBackgroundRefreshStatusChangeEvent;
    FBackgroundRefreshStatusNotificationListener: TBackgroundRefreshStatusNotificationListener;
  protected
    procedure BackgroundRefreshStatusChange;
  public
    { IBackgroundTaskManager }
    function GetBackgroundRefreshStatus: TBackgroundRefreshStatus;
    function GetOnBackgroundRefreshStatusChange: TBackgroundRefreshStatusChangeEvent;
    function ScheduleTask(const AIdentifier: string; const ATaskKind: TBackgroundTaskKind; const AInterval: Double;
      const AHandler: TBackgroundTaskProc; const ARequirements: TBackgroundTaskRequirements = []): IBackgroundTask;
    procedure SetOnBackgroundRefreshStatusChange(const AValue: TBackgroundRefreshStatusChangeEvent);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TCustomBackgroundTask }

constructor TCustomBackgroundTask.Create;
begin
  inherited Create;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  // NOTE: This will work on iOS 13.0+ ONLY
  SharedScheduler.registerForTaskWithIdentifier(GetIdentifierNative, 0, TaskHandler);
end;

destructor TCustomBackgroundTask.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TCustomBackgroundTask.DoScheduleError(const AError: string);
begin
  ScheduleError(AError);
end;

procedure TCustomBackgroundTask.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.EnteredBackground:
      Reschedule;
  end;
end;

function TCustomBackgroundTask.SharedScheduler: BGTaskScheduler;
begin
  Result := TBGTaskScheduler.OCClass.sharedScheduler;
end;

function TCustomBackgroundTask.GetIdentifierNative: NSString;
begin
  Result := StrToNSStr(GetIdentifier);
end;

procedure TCustomBackgroundTask.Cancel;
begin
  // Override and do any cleanup here, if necessary
end;

function TCustomBackgroundTask.GetInterval: Double;
begin
  Result := FInterval;
end;

function TCustomBackgroundTask.GetRequiresExternalPower: Boolean;
begin
  Result := True;
end;

function TCustomBackgroundTask.GetRequiresNetworkConnectivity: Boolean;
begin
  Result := True;
end;

procedure TCustomBackgroundTask.ScheduleError(const AError: string);
begin
  // Override to discover what error occurred, if any
end;

procedure TCustomBackgroundTask.ScheduleNext;
begin
  case GetTaskKind of
    TBackgroundTaskKind.AppRefresh:
      ScheduleAppRefreshTask;
    TBackgroundTaskKind.Processing:
      ScheduleProcessingTask;
  end;
end;

procedure TCustomBackgroundTask.SetInterval(const AInterval: Double);
begin
  FInterval := AInterval;
end;

procedure TCustomBackgroundTask.Reschedule;
begin
  SharedScheduler.cancelTaskRequestWithIdentifier(GetIdentifierNative);
  ScheduleNext;
end;

procedure TCustomBackgroundTask.ScheduleTask(const ARequest: BGTaskRequest);
var
  LPointer: Pointer;
  LError: NSError;
begin
  if not SharedScheduler.submitTaskRequest(ARequest, @LPointer) then
  begin
    LError := TNSError.Wrap(LPointer);
    DoScheduleError(NSStrToStr(LError.localizedDescription));
  end;
end;

procedure TCustomBackgroundTask.ScheduleAppRefreshTask;
var
  LRequest: BGAppRefreshTaskRequest;
begin
  LRequest := TBGAppRefreshTaskRequest.Wrap(TBGAppRefreshTaskRequest.Alloc.initWithIdentifier(GetIdentifierNative));
  LRequest.setEarliestBeginDate(TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(GetInterval)));
  ScheduleTask(LRequest);
end;

procedure TCustomBackgroundTask.ScheduleProcessingTask;
var
  LRequest: BGProcessingTaskRequest;
begin
  LRequest := TBGProcessingTaskRequest.Wrap(TBGProcessingTaskRequest.Alloc.initWithIdentifier(GetIdentifierNative));
  LRequest.setEarliestBeginDate(TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(GetInterval)));
  LRequest.setRequiresExternalPower(GetRequiresExternalPower);
  LRequest.setRequiresNetworkConnectivity(GetRequiresNetworkConnectivity);
  ScheduleTask(LRequest);
end;

procedure TCustomBackgroundTask.PerformTask(const task: BGTask);
var
  LSuccess: Boolean;
begin
  LSuccess := False;
  try
    LSuccess := Execute;
  except
    on E: Exception do
      TOSLog.e('> %s task execution caused an exception - %s: %s', [GetIdentifier, E.ClassName, E.Message]);
  end;
  TaskComplete(task, LSuccess);
end;

procedure TCustomBackgroundTask.TaskComplete(const task: BGTask; const ASuccess: Boolean);
begin
  task.setTaskCompletedWithSuccess(ASuccess);
end;

procedure TCustomBackgroundTask.TaskHandler(task: BGTask);
begin
  ScheduleNext;
  task.setExpirationHandler(Cancel);
  PerformTask(task);
end;

{ TBackgroundTask }

constructor TBackgroundTask.Create(const AIdentifier: string; const ATaskKind: TBackgroundTaskKind; const AInterval: Double;
  const AHandler: TBackgroundTaskProc; const ARequirements: TBackgroundTaskRequirements);
begin
  // Identifier **must** be set before inherited Create
  FIdentifier := AIdentifier;
  inherited Create;
  FInterval := AInterval;
  FTaskKind := ATaskKind;
  FHandler := AHandler;
  FRequirements := ARequirements;
end;

function TBackgroundTask.Execute: Boolean;
begin
  Result := FHandler();
end;

function TBackgroundTask.GetIdentifier: string;
begin
  Result := FIdentifier;
end;

function TBackgroundTask.GetRequiresExternalPower: Boolean;
begin
  Result := TBackgroundTaskRequirement.ExternalPower in FRequirements;
end;

function TBackgroundTask.GetRequiresNetworkConnectivity: Boolean;
begin
  Result := TBackgroundTaskRequirement.Connectivity in FRequirements;
end;

function TBackgroundTask.GetTaskKind: TBackgroundTaskKind;
begin
  Result := FTaskKind;
end;

{ TBackgroundTaskManager }

constructor TBackgroundTaskManager.Create;
begin
  inherited Create;
  FBackgroundRefreshStatusNotificationListener := TBackgroundRefreshStatusNotificationListener.Create(Self);
end;

destructor TBackgroundTaskManager.Destroy;
begin
  FBackgroundRefreshStatusNotificationListener.Free;
  inherited;
end;

procedure TBackgroundTaskManager.BackgroundRefreshStatusChange;
begin
  if Assigned(FOnBackgroundRefreshStatusChange) then
    FOnBackgroundRefreshStatusChange(Self, GetBackgroundRefreshStatus);
end;

function TBackgroundTaskManager.GetBackgroundRefreshStatus: TBackgroundRefreshStatus;
begin
  case TiOSHelper.SharedApplication.backgroundRefreshStatus of
    UIBackgroundRefreshStatusAvailable:
      Result := TBackgroundRefreshStatus.Available;
    UIBackgroundRefreshStatusDenied:
      Result := TBackgroundRefreshStatus.Denied;
    UIBackgroundRefreshStatusRestricted:
      Result := TBackgroundRefreshStatus.Restricted;
  else
    Result := TBackgroundRefreshStatus.Unknown;
  end;
end;

function TBackgroundTaskManager.GetOnBackgroundRefreshStatusChange: TBackgroundRefreshStatusChangeEvent;
begin
  Result := FOnBackgroundRefreshStatusChange;
end;

function TBackgroundTaskManager.ScheduleTask(const AIdentifier: string; const ATaskKind: TBackgroundTaskKind; const AInterval: Double;
  const AHandler: TBackgroundTaskProc; const ARequirements: TBackgroundTaskRequirements): IBackgroundTask;
begin
  Result := TBackgroundTask.Create(AIdentifier, ATaskKind, AInterval, AHandler, ARequirements);
end;

procedure TBackgroundTaskManager.SetOnBackgroundRefreshStatusChange(const AValue: TBackgroundRefreshStatusChangeEvent);
begin
  FOnBackgroundRefreshStatusChange := AValue;
end;

{ TBackgroundRefreshStatusNotificationListener }

constructor TBackgroundRefreshStatusNotificationListener.Create(const ABackgroundTaskManager: TBackgroundTaskManager);
begin
  inherited Create;
  FBackgroundTaskManager := ABackgroundTaskManager;
  TiOSHelper.DefaultNotificationCenter.addObserver(GetObjectID, sel_getUid('backgroundRefreshStatusDidChange:'),
    NSObjectToID(UIApplicationBackgroundRefreshStatusDidChangeNotification), nil);
end;

procedure TBackgroundRefreshStatusNotificationListener.backgroundRefreshStatusDidChange(notification: Pointer);
begin
  FBackgroundTaskManager.BackgroundRefreshStatusChange;
end;

function TBackgroundRefreshStatusNotificationListener.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IBackgroundRefreshStatusNotification);
end;

initialization
  BackgroundTaskManager := TBackgroundTaskManager.Create;

end.
