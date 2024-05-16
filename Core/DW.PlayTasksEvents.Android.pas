unit DW.PlayTasksEvents.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNI.PlayServices.Tasks, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge;

type
  IPlayTasksEvents = interface(IInterface)
    ['{06EC5680-709C-4E72-BDAC-53FE06AE66FC}']
    function GetOnFailureListener: JOnFailureListener;
    function GetOnSuccessListener: JOnSuccessListener;
  end;

  TPlayTasksOnSuccessProc = reference to procedure(const Obj: JObject);
  TPlayTasksOnFailureProc = reference to procedure(const exception: JException);

  TPlayTasksHandlers = record
    OnFailure: TPlayTasksOnFailureProc;
    OnSuccess: TPlayTasksOnSuccessProc;
  end;

  TPlayTasksEvents = class(TInterfacedObject, IPlayTasksEvents)
  private
    FHandlers: TPlayTasksHandlers;
    FFailureListener: JOnFailureListener;
    FSuccessListener: JOnSuccessListener;
    procedure FailureListenerHandler(const AException: JException);
    procedure SuccessListenerHandler(const AObj: JObject);
  public
    { IPlayTasksEvents }
    function GetOnFailureListener: JOnFailureListener;
    function GetOnSuccessListener: JOnSuccessListener;
  public
    constructor Create(const AHandlers: TPlayTasksHandlers);
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

implementation

{ TPlayTasksEvents }

constructor TPlayTasksEvents.Create(const AHandlers: TPlayTasksHandlers);
begin
  inherited Create;
  FHandlers := AHandlers;
  FFailureListener := TPlayTasksOnFailureListener.Create(FailureListenerHandler);
  FSuccessListener := TPlayTasksOnSuccessListener.Create(SuccessListenerHandler);
end;

function TPlayTasksEvents.GetOnFailureListener: JOnFailureListener;
begin
  Result := FFailureListener;
end;

function TPlayTasksEvents.GetOnSuccessListener: JOnSuccessListener;
begin
  Result := FSuccessListener;
end;

procedure TPlayTasksEvents.FailureListenerHandler(const AException: JException);
begin
  if Assigned(FHandlers.OnFailure) then
    FHandlers.OnFailure(AException);
end;

procedure TPlayTasksEvents.SuccessListenerHandler(const AObj: JObject);
begin
  if Assigned(FHandlers.OnSuccess) then
    FHandlers.OnSuccess(AObj);
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

end.
