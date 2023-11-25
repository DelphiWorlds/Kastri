unit DW.AdMob.iOS;

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
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.Foundation, iOSapi.Helpers,
  // DW
  DW.OSLog,
  DW.AdMob, DW.iOSapi.GoogleMobileAds, DW.iOSapi.UserMessagingPlatform, DW.Macapi.Helpers;

type
  TPlatformAdMob = class(TAdMob)
  private
    function CanRequestAds: Boolean;
    procedure CheckConsent;
    function ConsentInformation: UMPConsentInformation;
    procedure ConsentFormPresentCompletionHandler(error: NSError);
    procedure ConsentFormLoadCompletionHandler(consentForm: UMPConsentForm; error: NSError);
    procedure ConsentInformationUpdateCompletionHandler(error: NSError);
    function GetDebugGeography: UMPDebugGeography;
    procedure HandleError(const AOrigin: string; const AError: NSError);
  protected
    procedure RequestConsent; override;
  public
    procedure ResetConsent; override;
  end;

{ TPlatformAdMob }

function TPlatformAdMob.ConsentInformation: UMPConsentInformation;
begin
  Result := TUMPConsentInformation.OCClass.sharedInstance;
end;

procedure TPlatformAdMob.HandleError(const AOrigin: string; const AError: NSError);
begin
  ConsentError(TConsentError.Create(AError.code, NSStrToStr(AError.localizedDescription), AOrigin));
end;

procedure TPlatformAdMob.ConsentFormPresentCompletionHandler(error: NSError);
begin
  if error = nil then
    CheckConsent
  else
    HandleError('ConsentFormPresentCompletionHandler', error);
end;

procedure TPlatformAdMob.ConsentInformationUpdateCompletionHandler(error: NSError);
begin
  if error = nil then
  begin
    TUMPConsentForm.OCClass.loadWithCompletionHandler(ConsentFormLoadCompletionHandler);
//    TUMPConsentForm.OCClass.loadAndPresentIfRequiredFromViewController(TiOSHelper.SharedApplication.keyWindow.rootViewController,
//      ConsentFormPresentCompletionHandler);
  end
  else
    HandleError('ConsentInformationUpdateCompletionHandler', error);
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

procedure TPlatformAdMob.RequestConsent;
var
  LParams: UMPRequestParameters;
  LDebugSettings: UMPDebugSettings;
begin
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
  CheckConsent;
end;

procedure TPlatformAdMob.ResetConsent;
begin
  ConsentInformation.reset;
end;

function TPlatformAdMob.CanRequestAds: Boolean;
begin
 // Result := ConsentInformation.canRequestAds; // later versions
 Result := (ConsentInformation.consentStatus = UMPConsentStatusObtained) or (ConsentInformation.consentStatus = UMPConsentStatusNotRequired);
end;

procedure TPlatformAdMob.CheckConsent;
begin
  if CanRequestAds then
  begin
    TGADMobileAds.OCClass.sharedInstance.startWithCompletionHandler(nil);
    AdsStarted;
  end;
end;

initialization
  AdMob := TPlatformAdMob.Create;

end.
