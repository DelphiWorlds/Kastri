unit DW.PlayTasksEvents.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNI.PlayServices.Tasks, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge;

type
  TPlayTasksOnCompleteProc = reference to procedure(const task: JTask);
  TPlayTasksOnSuccessProc = reference to procedure(const obj: JObject);
  TPlayTasksOnFailureProc = reference to procedure(const exception: JException);

  TPlayTasksOnCompleteListener = class(TJavaLocal, JOnCompleteListener)
  private
    FHandler: TPlayTasksOnCompleteProc;
  public
    { JOnSuccessListener }
    procedure onComplete(task: JTask); cdecl;
  public
    constructor Create(const AHandler: TPlayTasksOnCompleteProc);
  end;

  TPlayTasksOnSuccessListener = class(TJavaLocal, JOnSuccessListener)
  private
    FHandler: TPlayTasksOnSuccessProc;
  public
    { JOnSuccessListener }
    procedure onSuccess(result: JObject); cdecl;
  public
    constructor Create(const AHandler: TPlayTasksOnSuccessProc);
  end;

  TPlayTasksOnFailureListener = class(TJavaLocal, JOnFailureListener)
  private
    FHandler: TPlayTasksOnFailureProc;
  public
    { JOnFailureListener }
    procedure onFailure(exception: JException); cdecl;
  public
    constructor Create(const AHandler: TPlayTasksOnFailureProc);
  end;

  IPlayTasksEvents = interface(IInterface)
    ['{7A0285D9-BA6D-4184-A09C-BB68500CD634}']
    function OnComplete(const AHandler: TPlayTasksOnCompleteProc): IPlayTasksEvents;
    function OnFailure(const AHandler: TPlayTasksOnFailureProc): IPlayTasksEvents;
    function OnSuccess(const AHandler: TPlayTasksOnSuccessProc): IPlayTasksEvents;
    function SetTask(const ATask: JTask): IPlayTasksEvents;
  end;

  TPlayTasksEvents = class(TInterfacedObject, IPlayTasksEvents)
  private
    FCompleteListener: JOnCompleteListener;
    FFailureListener: JOnFailureListener;
    FSuccessListener: JOnSuccessListener;
    FTask: JTask;
  public
    { IPlayTasksEvents }
    function OnComplete(const AHandler: TPlayTasksOnCompleteProc): IPlayTasksEvents;
    function OnFailure(const AHandler: TPlayTasksOnFailureProc): IPlayTasksEvents;
    function OnSuccess(const AHandler: TPlayTasksOnSuccessProc): IPlayTasksEvents;
    function SetTask(const ATask: JTask): IPlayTasksEvents;
  end;

implementation

{ TPlayTasksOnCompleteListener }

constructor TPlayTasksOnCompleteListener.Create(const AHandler: TPlayTasksOnCompleteProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TPlayTasksOnCompleteListener.onComplete(task: JTask);
begin
  FHandler(task);
end;

{ TPlayTasksOnSuccessListener }

constructor TPlayTasksOnSuccessListener.Create(const AHandler: TPlayTasksOnSuccessProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TPlayTasksOnSuccessListener.onSuccess(result: JObject);
begin
  FHandler(result);
end;

{ TPlayTasksOnFailureListener }

constructor TPlayTasksOnFailureListener.Create(const AHandler: TPlayTasksOnFailureProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TPlayTasksOnFailureListener.onFailure(exception: JException);
begin
  FHandler(exception);
end;

{ TPlayTasksEvents }

function TPlayTasksEvents.OnComplete(const AHandler: TPlayTasksOnCompleteProc): IPlayTasksEvents;
begin
  if FTask <> nil then
  begin
    if FCompleteListener = nil then
      FCompleteListener := TPlayTasksOnCompleteListener.Create(AHandler);
    FTask.addOnCompleteListener(FCompleteListener);
  end;
  Result := Self;
end;

function TPlayTasksEvents.OnFailure(const AHandler: TPlayTasksOnFailureProc): IPlayTasksEvents;
begin
  if FTask <> nil then
  begin
    if FFailureListener = nil then
      FFailureListener := TPlayTasksOnFailureListener.Create(AHandler);
    FTask.addOnFailureListener(FFailureListener);
  end;
  Result := Self;
end;

function TPlayTasksEvents.OnSuccess(const AHandler: TPlayTasksOnSuccessProc): IPlayTasksEvents;
begin
  if FTask <> nil then
  begin
    if FSuccessListener = nil then
      FSuccessListener := TPlayTasksOnSuccessListener.Create(AHandler);
    FTask.addOnSuccessListener(FSuccessListener);
  end;
  Result := Self;
end;

function TPlayTasksEvents.SetTask(const ATask: JTask): IPlayTasksEvents;
begin
  if FTask <> ATask then
  begin
    if FTask <> nil then
      FTask := nil;
    FTask := ATask;
  end;
  Result := Self;
end;

end.
