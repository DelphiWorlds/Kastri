unit DW.AdMob.Android;

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
  System.SysUtils, System.Classes,
  // Android
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.AdMob, Androidapi.JNI.JavaTypes,
  // DW
  DW.OSLog,
  DW.AdMob, DW.Androidapi.JNI.UserMessagingPlatform;

type
  TPlatformAdMob = class;

  TAdMobListener = class(TJavaLocal)
  private
    FAndroidAdMob: TPlatformAdMob;
  protected
    property AndroidAdMob: TPlatformAdMob read FAndroidAdMob;
  public
    constructor Create(const AAndroidAdMob: TPlatformAdMob);
  end;

  TConsentInfoUpdateSuccessListener = class(TAdMobListener, JConsentInformation_OnConsentInfoUpdateSuccessListener)
  public
    { JConsentInformation_OnConsentInfoUpdateSuccessListener }
    procedure onConsentInfoUpdateSuccess; cdecl;
  end;

  TConsentInfoUpdateFailureListener = class(TAdMobListener, JConsentInformation_OnConsentInfoUpdateFailureListener)
  public
    { JConsentInformation_OnConsentInfoUpdateSuccessListener }
    procedure onConsentInfoUpdateFailure(formError: JFormError); cdecl;
  end;

  TConsentFormDismissedListener = class(TAdMobListener, JConsentForm_OnConsentFormDismissedListener)
  public
    { JConsentForm_OnConsentFormDismissedListener }
    procedure onConsentFormDismissed(formError: JFormError); cdecl;
  end;

  TConsentFormLoadSuccessListener = class(TAdMobListener, JUserMessagingPlatform_OnConsentFormLoadSuccessListener)
  public
    { JUserMessagingPlatform_OnConsentFormLoadSuccessListener }
    procedure onConsentFormLoadSuccess(consentForm: JConsentForm); cdecl;
  end;

  TConsentFormLoadFailureListener = class(TAdMobListener, JUserMessagingPlatform_OnConsentFormLoadFailureListener)
  public
    { JUserMessagingPlatform_OnConsentFormLoadFailureListener }
    procedure onConsentFormLoadFailure(formError: JFormError); cdecl;
  end;

  TPlatformAdMob = class(TAdMob)
  private
    FConsentFormDismissedListener: JConsentForm_OnConsentFormDismissedListener;
    FConsentFormLoadFailureListener: JUserMessagingPlatform_OnConsentFormLoadFailureListener;
    FConsentFormLoadSuccessListener: JUserMessagingPlatform_OnConsentFormLoadSuccessListener;
    FConsentInfoUpdateFailureListener: JConsentInformation_OnConsentInfoUpdateFailureListener;
    FConsentInfoUpdateSuccessListener: JConsentInformation_OnConsentInfoUpdateSuccessListener;
    function ConsentInformation: JConsentInformation;
    procedure CreateListeners;
    function GetDebugGeography: Integer;
    procedure HandleError(const AOrigin: string; const AFormError: JFormError);
    procedure StartAds;
    function UserMessagingPlatform: JUserMessagingPlatformClass;
  protected
    procedure ConsentFormDismissed(const AFormError: JFormError);
    procedure ConsentFormLoad(const AForm: JConsentForm; const AFormError: JFormError);
    procedure ConsentInfoUpdate(const AFormError: JFormError);
    procedure DoRequestConsent; override;
  public
    function CanRequestAds: Boolean; override;
    function ConsentStatus: TConsentStatus; override;
    procedure DoResetConsent; override;
  end;

{ TAdMobListener }

constructor TAdMobListener.Create(const AAndroidAdMob: TPlatformAdMob);
begin
  inherited Create;
  FAndroidAdMob := AAndroidAdMob;
end;

{ TConsentInfoUpdateSuccessListener }

procedure TConsentInfoUpdateSuccessListener.onConsentInfoUpdateSuccess;
begin
  AndroidAdMob.ConsentInfoUpdate(nil);
end;

{ TConsentInfoUpdateFailureListener }

procedure TConsentInfoUpdateFailureListener.onConsentInfoUpdateFailure(formError: JFormError);
begin
  AndroidAdMob.ConsentInfoUpdate(formError);
end;

{ TConsentFormDismissedListener }

procedure TConsentFormDismissedListener.onConsentFormDismissed(formError: JFormError);
begin
  AndroidAdMob.ConsentFormDismissed(formError);
end;

{ TConsentFormLoadSuccessListener }

procedure TConsentFormLoadSuccessListener.onConsentFormLoadSuccess(consentForm: JConsentForm);
begin
  AndroidAdMob.ConsentFormLoad(consentForm, nil);
end;

{ TConsentFormLoadFailureListener }

procedure TConsentFormLoadFailureListener.onConsentFormLoadFailure(formError: JFormError);
begin
  AndroidAdMob.ConsentFormLoad(nil, formError);
end;

{ TPlatformAdMob }

procedure TPlatformAdMob.CreateListeners;
begin
  FConsentFormDismissedListener := TConsentFormDismissedListener.Create(Self);
  FConsentFormLoadFailureListener :=  TConsentFormLoadFailureListener.Create(Self);
  FConsentFormLoadSuccessListener :=  TConsentFormLoadSuccessListener.Create(Self);
  FConsentInfoUpdateSuccessListener := TConsentInfoUpdateSuccessListener.Create(Self);
  FConsentInfoUpdateFailureListener := TConsentInfoUpdateFailureListener.Create(Self);
end;

function TPlatformAdMob.ConsentStatus: TConsentStatus;
var
  LStatus: Integer;
begin
  LStatus := ConsentInformation.getConsentStatus;
  if LStatus = TJConsentInformation_ConsentStatus.JavaClass.OBTAINED then
    Result := TConsentStatus.Obtained
  else if LStatus = TJConsentInformation_ConsentStatus.JavaClass.NOT_REQUIRED then
    Result := TConsentStatus.NotRequired
  else if LStatus = TJConsentInformation_ConsentStatus.JavaClass.REQUIRED then
    Result := TConsentStatus.Required
  else
    Result := TConsentStatus.Unknown;
end;

function TPlatformAdMob.GetDebugGeography: Integer;
begin
  case DebugGeography of
    TDebugGeography.EEA:
      Result := TJConsentDebugSettings_DebugGeography.JavaClass.DEBUG_GEOGRAPHY_EEA;
    TDebugGeography.NotEEA:
      Result := TJConsentDebugSettings_DebugGeography.JavaClass.DEBUG_GEOGRAPHY_NOT_EEA;
  else
    Result := TJConsentDebugSettings_DebugGeography.JavaClass.DEBUG_GEOGRAPHY_DISABLED;
  end;
end;

function TPlatformAdMob.ConsentInformation: JConsentInformation;
begin
  Result := TJUserMessagingPlatform.JavaClass.getConsentInformation(TAndroidHelper.Context);
end;

function TPlatformAdMob.UserMessagingPlatform: JUserMessagingPlatformClass;
begin
  Result := TJUserMessagingPlatform.JavaClass;
end;

procedure TPlatformAdMob.DoRequestConsent;
var
  LParams: JConsentRequestParameters_Builder;
  LDebugSettings: JConsentDebugSettings;
begin
  if FConsentFormDismissedListener = nil then
    CreateListeners;
  LDebugSettings := nil;
  if NeedsDebugSettings then
  begin
    LDebugSettings := TJConsentDebugSettings_Builder.JavaClass.init(TAndroidHelper.Context)
      .addTestDeviceHashedId(StringToJString(TestDeviceHashedId))
      .setDebugGeography(GetDebugGeography)
      .build;
  end;
  LParams := TJConsentRequestParameters_Builder.Create
    .setTagForUnderAgeOfConsent(False);
  if (LDebugSettings <> nil) then
    LParams := LParams.setConsentDebugSettings(LDebugSettings);
  ConsentInformation.requestConsentInfoUpdate(TAndroidHelper.Activity, LParams.build, FConsentInfoUpdateSuccessListener,
    FConsentInfoUpdateFailureListener);
  if CanRequestAds then
    StartAds;
end;

procedure TPlatformAdMob.DoResetConsent;
begin
  ConsentInformation.reset;
end;

procedure TPlatformAdMob.ConsentInfoUpdate(const AFormError: JFormError);
begin
  if AFormError = nil then
  begin
    {$IF Defined(UMP_210)}
    UserMessagingPlatform.loadAndShowConsentFormIfRequired(TAndroidHelper.Activity, FConsentFormDismissedListener);
    {$ELSE}
    if (ConsentStatus = TConsentStatus.Required) and ConsentInformation.isConsentFormAvailable then
      UserMessagingPlatform.loadConsentForm(TAndroidHelper.Context, FConsentFormLoadSuccessListener, FConsentFormLoadFailureListener)
    else if CanRequestAds then
      StartAds;
    // else ????
    {$ENDIF}
  end
  else
    HandleError('ConsentInfoUpdate', AFormError);
end;

function TPlatformAdMob.CanRequestAds: Boolean;
begin
  {$IF Defined(UMP_210)}
  Result := ConsentInformation.canRequestAds;
  {$ELSE}
  Result := ConsentStatus in [TConsentStatus.Obtained, TConsentStatus.NotRequired];
  {$ENDIF}
end;

function StringArrayToJList(const AValues: TArray<string>): JList;
var
  LValue: string;
  LArrayList: JArrayList;
begin
  LArrayList := TJArrayList.Create;
  for LValue in AValues do
    LArrayList.add(StringToJString(LValue));
  Result := TJList.Wrap(LArrayList);
end;

procedure TPlatformAdMob.StartAds;
var
  LConfiguration: JRequestConfiguration;
begin
  if not IsStarted then
  begin
    if not TestDeviceHashedId.IsEmpty then
    begin
      LConfiguration := TJRequestConfiguration_Builder.JavaClass.init.setTestDeviceIds(StringArrayToJList([TestDeviceHashedId])).build;
      TJMobileAds.JavaClass.setRequestConfiguration(LConfiguration);
    end;
    TJMobileAds.JavaClass.initialize(TAndroidHelper.Context);
    AdsStarted;
  end;
end;

procedure TPlatformAdMob.HandleError(const AOrigin: string; const AFormError: JFormError);
begin
  ConsentError(TConsentError.Create(AFormError.getErrorCode, JStringToString(AFormError.getMessage), AOrigin));
end;

procedure TPlatformAdMob.ConsentFormDismissed(const AFormError: JFormError);
begin
  if AFormError = nil then
  begin
    StartAds;
  end
  else
    HandleError('ConsentFormDismissed', AFormError);
end;

procedure TPlatformAdMob.ConsentFormLoad(const AForm: JConsentForm; const AFormError: JFormError);
begin
  if AForm <> nil then
    AForm.show(TAndroidHelper.Activity, FConsentFormDismissedListener)
  else
    HandleError('ConsentFormLoad', AFormError);
end;

initialization
  AdMob := TPlatformAdMob.Create;

end.
