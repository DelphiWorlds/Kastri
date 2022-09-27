unit DW.AppUpdate.Android;

interface

uses
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  DW.Androidapi.JNI.PlayCore,
  DW.AppUpdate;

type
  TTaskSuccessProc = reference to procedure(const Obj: JObject);

  TTaskSuccessListener = class(TJavaLocal, JOnSuccessListener)
  private
    FHandler: TTaskSuccessProc;
  public
    { JOnSuccessListener }
    procedure onSuccess(result: JObject); cdecl;
  public
    constructor Create(const AHandler: TTaskSuccessProc);
  end;

  TTaskCompleteProc = reference to procedure(const Task: JTask);

  TTaskCompleteListener = class(TJavaLocal, JOnCompleteListener)
  private
    FHandler: TTaskCompleteProc;
  public
    { JOnCompleteListener }
    procedure onComplete(task: JTask); cdecl;
  public
    constructor Create(const AHandler: TTaskCompleteProc);
  end;

  TPlatformAppUpdate = class(TCustomPlatformAppUpdate)
  private
    FAppUpdateManager: JAppUpdateManager;
    FAppUpdateType: Integer;
    FCheckForUpdateSuccessListener: JOnSuccessListener;
    FIsCheckingUpdate: Boolean;
    FIsStartingUpdate: Boolean;
    FIsUpdating: Boolean;
    FStartUpdateSuccessListener: JOnSuccessListener;
    FStartUpdateFlowCompleteListener: JOnCompleteListener;
    procedure CheckForUpdateSuccessHandler(const AObject: JObject);
    procedure StartUpdateSuccessHandler(const AObject: JObject);
    procedure StartUpdateFlowCompleteHandler(const ATask: JTask);
  protected
    procedure CheckForUpdate; override;
    procedure StartUpdate(const AUpdateType: TAppUpdateType); override;
  public
    constructor Create(const AAppUpdate: TAppUpdate); override;
  end;

implementation

uses
  System.SysUtils,
  Androidapi.Helpers, Androidapi.JNI, Androidapi.JNI.App;

{ TTaskSuccessListener }

constructor TTaskSuccessListener.Create(const AHandler: TTaskSuccessProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TTaskSuccessListener.onSuccess(result: JObject);
begin
  if Assigned(FHandler) then
    FHandler(result);
end;

{ TTaskCompleteListener }

constructor TTaskCompleteListener.Create(const AHandler: TTaskCompleteProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TTaskCompleteListener.onComplete(task: JTask);
begin
  if Assigned(FHandler) then
    FHandler(task);
end;

{ TPlatformAppUpdate }

constructor TPlatformAppUpdate.Create(const AAppUpdate: TAppUpdate);
begin
  inherited;
  FAppUpdateManager := TJAppUpdateManagerFactory.JavaClass.create(TAndroidHelper.Context);
  FCheckForUpdateSuccessListener := TTaskSuccessListener.Create(CheckForUpdateSuccessHandler);
  FStartUpdateSuccessListener := TTaskSuccessListener.Create(StartUpdateSuccessHandler);
  FStartUpdateFlowCompleteListener := TTaskCompleteListener.Create(StartUpdateFlowCompleteHandler);
end;

procedure TPlatformAppUpdate.CheckForUpdateSuccessHandler(const AObject: JObject);
var
  LInfo: JAppUpdateInfo;
  LUpdateInfo: TAppUpdateInfo;
begin
  if TJNIResolver.IsInstanceOf(AObject, TJAppUpdateInfo.GetClsID) then
  begin
    LInfo := TJAppUpdateInfo.Wrap(AObject);
    LUpdateInfo.Reset;
    LUpdateInfo.Available := LInfo.updateAvailability = TJUpdateAvailability.JavaClass.UPDATE_AVAILABLE;
    if LUpdateInfo.Available then
    begin
      LUpdateInfo.Priority := LInfo.updatePriority;
      LUpdateInfo.Immediate := LInfo.isUpdateTypeAllowed(TJAppUpdateType.JavaClass.IMMEDIATE);
      LUpdateInfo.Flexible := LInfo.isUpdateTypeAllowed(TJAppUpdateType.JavaClass.FLEXIBLE);
      if LUpdateInfo.Flexible and (LInfo.clientVersionStalenessDays <> nil) then
        LUpdateInfo.StalenessDays := LInfo.clientVersionStalenessDays.intValue;
      LUpdateInfo.TotalBytesToDownload := LInfo.totalBytesToDownload;
    end;
    DoAppUpdateInfo(LUpdateInfo);
  end;
  FIsCheckingUpdate := False;
end;

procedure TPlatformAppUpdate.CheckForUpdate;
begin
  if not FIsCheckingUpdate then
  begin
    FIsCheckingUpdate := True;
    FAppUpdateManager.getAppUpdateInfo.addOnSuccessListener(FCheckForUpdateSuccessListener);
  end;
end;

procedure TPlatformAppUpdate.StartUpdate(const AUpdateType: TAppUpdateType);
begin
  if not (FIsStartingUpdate or FIsUpdating) then
  begin
    FIsStartingUpdate := True;
    if AUpdateType = TAppUpdateType.Flexible then
      FAppUpdateType := TJAppUpdateType.JavaClass.FLEXIBLE
    else
      FAppUpdateType := TJAppUpdateType.JavaClass.IMMEDIATE;
    FAppUpdateManager.getAppUpdateInfo.addOnSuccessListener(FStartUpdateSuccessListener);
  end;
end;

procedure TPlatformAppUpdate.StartUpdateSuccessHandler(const AObject: JObject);
var
  LOptions: JAppUpdateOptions;
  LTask: JTask;
begin
  if TJNIResolver.IsInstanceOf(AObject, TJAppUpdateInfo.GetClsID) then
  begin
    LOptions := TJAppUpdateOptions.JavaClass.defaultOptions(FAppUpdateType);
    LTask := FAppUpdateManager.startUpdateFlow(TJAppUpdateInfo.Wrap(AObject), TAndroidHelper.Activity, LOptions);
    LTask.addOnCompleteListener(FStartUpdateFlowCompleteListener);
    DoStartedUpdateFlow(LTask.isSuccessful);
    FIsUpdating := LTask.isSuccessful;
  end;
  FIsStartingUpdate := False;
end;

procedure TPlatformAppUpdate.StartUpdateFlowCompleteHandler(const ATask: JTask);
var
  LTaskResult: Integer;
  LUpdateResult: TAppUpdateResult;
begin
  if TJNIResolver.IsInstanceOf(ATask.getResult, TJInteger.GetClsID) then
  begin
    LTaskResult := TJInteger.Wrap(ATask.getResult).intValue;
    if LTaskResult = TJActivity.JavaClass.RESULT_OK then
      LUpdateResult := TAppUpdateResult.Succeeded
    else if LTaskResult = TJActivity.JavaClass.RESULT_CANCELED then
      LUpdateResult := TAppUpdateResult.Canceled
    else
      LUpdateResult := TAppUpdateResult.Failed; // TJActivityResult.JavaClass.RESULT_IN_APP_UPDATE_FAILED;
    DoAppUpdateResult(LUpdateResult);
  end;
  FIsUpdating := False;
end;

end.
