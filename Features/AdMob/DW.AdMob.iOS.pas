unit DW.AdMob.iOS;

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

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.Foundation, iOSapi.Helpers,
  // DW
  DW.OSLog,
  DW.AdMob, DW.iOSapi.GoogleMobileAds, DW.iOSapi.UserMessagingPlatform, DW.Macapi.Helpers, DW.iOSapi.AppTrackingTransparency,
  DW.OSMetadata;

type
  TPlatformAdMob = class(TAdMob)
  private
    procedure CheckATT;
    function ConsentInformation: UMPConsentInformation;
    procedure ConsentFormPresentCompletionHandler(error: NSError);
    procedure ConsentFormLoadCompletionHandler(consentForm: UMPConsentForm; error: NSError);
    procedure ConsentInformationUpdateCompletionHandler(error: NSError);
    function GetDebugGeography: UMPDebugGeography;
    procedure HandleError(const AOrigin: string; const AError: NSError); overload;
    procedure HandleError(const AOrigin, AMessage: string); overload;
    procedure StartAds;
    procedure TrackingAuthorizationCompletionHandler(status: ATTrackingManagerAuthorizationStatus);
  protected
    procedure DoRequestConsent; override;
  public
    function CanRequestAds: Boolean; override;
    function ConsentStatus: TConsentStatus; override;
    procedure DoResetConsent; override;
  end;

const
  cUserTrackingUsageKey = 'NSUserTrackingUsageDescription';

{ TPlatformAdMob }

function TPlatformAdMob.ConsentInformation: UMPConsentInformation;
begin
  Result := TUMPConsentInformation.OCClass.sharedInstance;
end;

procedure TPlatformAdMob.HandleError(const AOrigin: string; const AError: NSError);
begin
  ConsentError(TConsentError.Create(AError.code, NSStrToStr(AError.localizedDescription), AOrigin));
end;

procedure TPlatformAdMob.HandleError(const AOrigin, AMessage: string);
begin
  ConsentError(TConsentError.Create(-1, AMessage, AOrigin));
end;

procedure TPlatformAdMob.ConsentFormPresentCompletionHandler(error: NSError);
begin
  TOSLog.d('TPlatformAdMob.ConsentFormPresentCompletionHandler');
  if error = nil then
    CheckATT
  else
    HandleError('ConsentFormPresentCompletionHandler', error);
end;

procedure TPlatformAdMob.ConsentInformationUpdateCompletionHandler(error: NSError);
begin
  TOSLog.d('TPlatformAdMob.ConsentInformationUpdateCompletionHandler');
  if error = nil then
  begin
    if (ConsentStatus = TConsentStatus.Required) and (ConsentInformation.formStatus = UMPFormStatusAvailable) then
      TUMPConsentForm.OCClass.loadWithCompletionHandler(ConsentFormLoadCompletionHandler)
    else if CanRequestAds then
      StartAds;
    // else ???
//    TUMPConsentForm.OCClass.loadAndPresentIfRequiredFromViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController,
//      ConsentFormPresentCompletionHandler);
  end
  else
    HandleError('ConsentInformationUpdateCompletionHandler', error);
end;

function TPlatformAdMob.ConsentStatus: TConsentStatus;
begin
  case ConsentInformation.consentStatus of
    UMPConsentStatusObtained:
      Result := TConsentStatus.Obtained;
    UMPConsentStatusRequired:
      Result := TConsentStatus.Required;
    UMPConsentStatusNotRequired:
      Result := TConsentStatus.NotRequired;
  else
    Result := TConsentStatus.Unknown;
  end;
end;

procedure TPlatformAdMob.ConsentFormLoadCompletionHandler(consentForm: UMPConsentForm; error: NSError);
begin
  if error = nil then
    consentForm.presentFromViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController, ConsentFormPresentCompletionHandler)
  else
    HandleError('ConsentFormLoadCompletionHandler', error);
end;

function TPlatformAdMob.GetDebugGeography: UMPDebugGeography;
begin
  case DebugGeography of
    TDebugGeography.EEA:
      Result := UMPDebugGeographyEEA;
    TDebugGeography.NotEEA:
      Result := UMPDebugGeographyNotEEA;
  else
    Result := UMPDebugGeographyDisabled;
  end;
end;

procedure TPlatformAdMob.DoRequestConsent;
var
  LParams: UMPRequestParameters;
  LDebugSettings: UMPDebugSettings;
begin
  TOSLog.d('TPlatformAdMob.DoRequestConsent');
  LParams := TUMPRequestParameters.Create;
  LParams.setTagForUnderAgeOfConsent(False);
  if NeedsDebugSettings then
  begin
    LDebugSettings := TUMPDebugSettings.Create;
    LDebugSettings.setTestDeviceIdentifiers(StringArrayToNSArray([TestDeviceHashedId]));
    LDebugSettings.setGeography(GetDebugGeography);
    LParams.setDebugSettings(LDebugSettings);
  end;
  ConsentInformation.requestConsentInfoUpdateWithParameters(LParams, ConsentInformationUpdateCompletionHandler);
  if CanRequestAds then
    CheckATT;
end;

procedure TPlatformAdMob.DoResetConsent;
begin
  ConsentInformation.reset;
end;

function TPlatformAdMob.CanRequestAds: Boolean;
begin
  // Result := ConsentInformation.canRequestAds; // later versions
  Result := (ConsentInformation.consentStatus = UMPConsentStatusObtained) or (ConsentInformation.consentStatus = UMPConsentStatusNotRequired);
end;

procedure TPlatformAdMob.CheckATT;
begin
  if TOSVersion.Check(14, 5) and ShowATTPrompt then
  begin
    if TOSMetadata.ContainsKey(cUserTrackingUsageKey) then
      TATTrackingManager.OCClass.requestTrackingAuthorizationWithCompletionHandler(TrackingAuthorizationCompletionHandler)
    else
      HandleError('CheckATT', 'Application version info is missing usage key:  ' + cUserTrackingUsageKey);
  end
  else
    StartAds;
end;

procedure TPlatformAdMob.TrackingAuthorizationCompletionHandler(status: ATTrackingManagerAuthorizationStatus);
begin
  StartAds;
end;

procedure TPlatformAdMob.StartAds;
begin
  if not IsStarted then
  begin
    TGADMobileAds.OCClass.sharedInstance.startWithCompletionHandler(nil);
    AdsStarted;
  end;
end;

initialization
  AdMob := TPlatformAdMob.Create;

end.
