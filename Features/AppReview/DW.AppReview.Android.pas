unit DW.AppReview.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

implementation

uses
  // Android
  Androidapi.Helpers,
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

{ TAppReview }

procedure TPlatformAppReview.RequestReview;
var
  LTask: JTask;
begin
  if FReviewManager = nil then
    FReviewManager := TJReviewManagerFactory.JavaClass.create(TAndroidHelper.Context);
  if FRequestReviewFlowOnCompleteListener = nil then
    FRequestReviewFlowOnCompleteListener := TPlayCoreOnCompleteListener.Create(RequestReviewCompleted);
  LTask := FReviewManager.requestReviewFlow.addOnCompleteListener(FRequestReviewFlowOnCompleteListener);
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

initialization
  AppReview := TPlatformAppReview.Create;

end.
