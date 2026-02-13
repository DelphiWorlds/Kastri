unit DW.AppReview.Android;

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

implementation

uses
  // RTL
  System.TypInfo,
  // Android
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  // DW
  DW.Androidapi.JNI.PlayCore, DW.Android.Helpers, DW.AppReview;

type
  TPlatformAppReview = class(TInterfacedObject, IAppReview)
  private
    FLaunchReviewFlowOnCompleteListener: JOnCompleteListener;
    FRequestReviewFlowOnCompleteListener: JOnCompleteListener;
    FReviewManager: JReviewManager;
    procedure LaunchReviewFlowCompleted(const ATask: JTask);
    procedure RequestReviewCompleted(const ATask: JTask);
  public
    { IAppReview }
    procedure RequestReview;
  end;

  TPlayCoreOnCompleteProc = reference to procedure(const ATask: JTask);

  TPlayCoreOnCompleteListener = class(TJavaLocal, JOnCompleteListener)
  private
    FHandler: TPlayCoreOnCompleteProc;
  public
    { JOnCompleteListener }
    procedure onComplete(task: JTask); cdecl;
  public
    constructor Create(const AHandler: TPlayCoreOnCompleteProc);
  end;

  TPlayCoreOnSuccessProc = reference to procedure(const Obj: JObject);

  TPlayCoreOnSuccessListener = class(TJavaLocal, JOnSuccessListener)
  private
    FHandler: TPlayCoreOnSuccessProc;
  public
    { JOnSuccessListener }
    procedure onSuccess(result: JObject); cdecl;
  public
    constructor Create(const AHandler: TPlayCoreOnSuccessProc);
  end;

  TPlayCoreOnFailureProc = reference to procedure(const exception: JException);

  TPlayCoreOnFailureListener = class(TJavaLocal, JOnFailureListener)
  private
    FHandler: TPlayCoreOnFailureProc;
  public
    { JOnFailureListener }
    procedure onFailure(exception: JException); cdecl;
  public
    constructor Create(const AHandler: TPlayCoreOnFailureProc);
  end;

const
  cPlayCoreLibraryName = 'play-core-1.10.0.jar';

{ TAppReview }

procedure TPlatformAppReview.RequestReview;
var
  LTask: JTask;
begin
  if TAndroidHelperEx.HasClass(TypeInfo(JReviewManager), cPlayCoreLibraryName) then
  begin
    if FReviewManager = nil then
      FReviewManager := TJReviewManagerFactory.JavaClass.create(TAndroidHelper.Context);
    if FRequestReviewFlowOnCompleteListener = nil then
      FRequestReviewFlowOnCompleteListener := TPlayCoreOnCompleteListener.Create(RequestReviewCompleted);
    LTask := FReviewManager.requestReviewFlow.addOnCompleteListener(FRequestReviewFlowOnCompleteListener);
  end;
end;

procedure TPlatformAppReview.RequestReviewCompleted(const ATask: JTask);
begin
  if ATask.isSuccessful then
  begin
    if FLaunchReviewFlowOnCompleteListener = nil then
      FLaunchReviewFlowOnCompleteListener := TPlayCoreOnCompleteListener.Create(LaunchReviewFlowCompleted);
    FReviewManager.launchReviewFlow(TAndroidHelper.Activity, TJReviewInfo.Wrap(ATask.getResult))
      .addOnCompleteListener(FLaunchReviewFlowOnCompleteListener);
  end;
end;

procedure TPlatformAppReview.LaunchReviewFlowCompleted(const ATask: JTask);
begin
  //
end;

{ TPlayCoreOnCompleteListener }

constructor TPlayCoreOnCompleteListener.Create(const AHandler: TPlayCoreOnCompleteProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TPlayCoreOnCompleteListener.onComplete(task: JTask);
begin
  if Assigned(FHandler) then
    FHandler(task);
end;

{ TPlayCoreSuccessListener }

constructor TPlayCoreOnSuccessListener.Create(const AHandler: TPlayCoreOnSuccessProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TPlayCoreOnSuccessListener.onSuccess(result: JObject);
begin
  if Assigned(FHandler) then
    FHandler(result);
end;

{ TPlayCoreOnFailureListener }

constructor TPlayCoreOnFailureListener.Create(const AHandler: TPlayCoreOnFailureProc);
begin
  inherited Create;
  FHandler := AHandler;
end;

procedure TPlayCoreOnFailureListener.onFailure(exception: JException);
begin
  if Assigned(FHandler) then
    FHandler(exception);
end;

initialization
  AppReview := TPlatformAppReview.Create;

end.
