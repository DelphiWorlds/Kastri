unit DW.Androidapi.JNI.UserMessagingPlatform;

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
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes;

type
  JUserMessagingPlatform_OnConsentFormLoadSuccessListener = interface;
  JConsentForm = interface;
  JUserMessagingPlatform_OnConsentFormLoadFailureListener = interface;
  JConsentInformation_OnConsentInfoUpdateSuccessListener = interface;
  JConsentInformation_OnConsentInfoUpdateFailureListener = interface;
  JConsentInformation = interface;
  JConsentDebugSettings = interface;
  JConsentDebugSettings_Builder = interface;
  JConsentDebugSettings_DebugGeography = interface;
  JConsentForm_OnConsentFormDismissedListener = interface;
  JConsentInformation_ConsentStatus = interface;
  JConsentInformation_PrivacyOptionsRequirementStatus = interface;
  JConsentRequestParameters = interface;
  JConsentRequestParameters_Builder = interface;
  JFormError = interface;
  JFormError_ErrorCode = interface;
  JUserMessagingPlatform = interface;

  JUserMessagingPlatform_OnConsentFormLoadSuccessListenerClass = interface(IJavaClass)
    ['{23139EBF-6A12-493D-A587-0311A47E0EF3}']
  end;

  [JavaSignature('com/google/android/ump/UserMessagingPlatform$OnConsentFormLoadSuccessListener')]
  JUserMessagingPlatform_OnConsentFormLoadSuccessListener = interface(IJavaInstance)
    ['{A979F14B-D332-4D30-B335-D02EB1B12A38}']
    procedure onConsentFormLoadSuccess(consentForm: JConsentForm); cdecl;
  end;
  TJUserMessagingPlatform_OnConsentFormLoadSuccessListener = class(TJavaGenericImport<JUserMessagingPlatform_OnConsentFormLoadSuccessListenerClass,
    JUserMessagingPlatform_OnConsentFormLoadSuccessListener>) end;

  JConsentFormClass = interface(IJavaClass)
    ['{FADA135A-17FA-4EB9-AA24-B89EE2C4E27F}']
  end;

  [JavaSignature('com/google/android/ump/ConsentForm')]
  JConsentForm = interface(IJavaInstance)
    ['{299E4880-203B-469E-B58A-3BA7FF5D3422}']
    procedure show(activity: JActivity; onConsentFormDismissedListener: JConsentForm_OnConsentFormDismissedListener); cdecl;
  end;
  TJConsentForm = class(TJavaGenericImport<JConsentFormClass, JConsentForm>) end;

  JUserMessagingPlatform_OnConsentFormLoadFailureListenerClass = interface(IJavaClass)
    ['{48871992-63D5-49CE-8443-D47A7C3FC06C}']
  end;

  [JavaSignature('com/google/android/ump/UserMessagingPlatform$OnConsentFormLoadFailureListener')]
  JUserMessagingPlatform_OnConsentFormLoadFailureListener = interface(IJavaInstance)
    ['{9AF1AD0A-09C8-489C-B865-8875A88A87B6}']
    procedure onConsentFormLoadFailure(formError: JFormError); cdecl;
  end;
  TJUserMessagingPlatform_OnConsentFormLoadFailureListener = class(TJavaGenericImport<JUserMessagingPlatform_OnConsentFormLoadFailureListenerClass,
    JUserMessagingPlatform_OnConsentFormLoadFailureListener>) end;

  JConsentInformation_OnConsentInfoUpdateSuccessListenerClass = interface(IJavaClass)
    ['{189B01ED-E5A9-42B0-A4DB-D86B687EFB08}']
  end;

  [JavaSignature('com/google/android/ump/ConsentInformation$OnConsentInfoUpdateSuccessListener')]
  JConsentInformation_OnConsentInfoUpdateSuccessListener = interface(IJavaInstance)
    ['{DCBE9C7E-76E8-4C4D-B42E-F807EBF2065F}']
    procedure onConsentInfoUpdateSuccess; cdecl;
  end;
  TJConsentInformation_OnConsentInfoUpdateSuccessListener = class(TJavaGenericImport<JConsentInformation_OnConsentInfoUpdateSuccessListenerClass,
    JConsentInformation_OnConsentInfoUpdateSuccessListener>) end;

  JConsentInformation_OnConsentInfoUpdateFailureListenerClass = interface(IJavaClass)
    ['{B77A35E3-EBDB-4653-9B53-9C7BD5B3BD8A}']
  end;

  [JavaSignature('com/google/android/ump/ConsentInformation$OnConsentInfoUpdateFailureListener')]
  JConsentInformation_OnConsentInfoUpdateFailureListener = interface(IJavaInstance)
    ['{533258F8-5725-4D63-B5FD-4BB9B5D82CE8}']
    procedure onConsentInfoUpdateFailure(formError: JFormError); cdecl;
  end;
  TJConsentInformation_OnConsentInfoUpdateFailureListener = class(TJavaGenericImport<JConsentInformation_OnConsentInfoUpdateFailureListenerClass,
    JConsentInformation_OnConsentInfoUpdateFailureListener>) end;

  JConsentInformationClass = interface(IJavaClass)
    ['{A508AA61-F866-4316-8D5D-36D4675444D4}']
  end;

  [JavaSignature('com/google/android/ump/ConsentInformation')]
  JConsentInformation = interface(IJavaInstance)
    ['{AAB9F6E5-77F2-4A4D-B6F7-CFD3E421378E}']
    function canRequestAds: Boolean; cdecl;
    function getConsentStatus: Integer; cdecl;
    function getPrivacyOptionsRequirementStatus: JConsentInformation_PrivacyOptionsRequirementStatus; cdecl;
    function isConsentFormAvailable: Boolean; cdecl;
    procedure requestConsentInfoUpdate(activity: JActivity; consentRequestParameters: JConsentRequestParameters;
      onConsentInfoUpdateSuccessListener: JConsentInformation_OnConsentInfoUpdateSuccessListener;
      onConsentInfoUpdateFailureListener: JConsentInformation_OnConsentInfoUpdateFailureListener); cdecl;
    procedure reset; cdecl;
  end;
  TJConsentInformation = class(TJavaGenericImport<JConsentInformationClass, JConsentInformation>) end;

  JConsentDebugSettingsClass = interface(JObjectClass)
    ['{A7DD6B43-274E-418F-A61D-2592E325C0FA}']
    {class} function getDebugGeography: Integer; cdecl;
  end;

  [JavaSignature('com/google/android/ump/ConsentDebugSettings')]
  JConsentDebugSettings = interface(JObject)
    ['{EE61383F-3BEA-4DAA-868B-458A5C580B8F}']
    function isTestDevice: Boolean; cdecl;
  end;
  TJConsentDebugSettings = class(TJavaGenericImport<JConsentDebugSettingsClass, JConsentDebugSettings>) end;

  JConsentDebugSettings_BuilderClass = interface(JObjectClass)
    ['{23F1D463-50D6-4A14-AC52-AEF4198AC5A1}']
    {class} function init(context: JContext): JConsentDebugSettings_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/ump/ConsentDebugSettings$Builder')]
  JConsentDebugSettings_Builder = interface(JObject)
    ['{705FFA70-BD9A-4450-9B90-FBE41EE84786}']
    function addTestDeviceHashedId(string_: JString): JConsentDebugSettings_Builder; cdecl;
    function build: JConsentDebugSettings; cdecl;
    function setDebugGeography(i: Integer): JConsentDebugSettings_Builder; cdecl;
    function setForceTesting(b: Boolean): JConsentDebugSettings_Builder; cdecl;
  end;
  TJConsentDebugSettings_Builder = class(TJavaGenericImport<JConsentDebugSettings_BuilderClass, JConsentDebugSettings_Builder>) end;

  JConsentDebugSettings_DebugGeographyClass = interface(JAnnotationClass)
    ['{28A4C45B-B81B-44CB-8BFB-D93CF93C3015}']
    {class} function _GetDEBUG_GEOGRAPHY_DISABLED: Integer; cdecl;
    {class} function _GetDEBUG_GEOGRAPHY_EEA: Integer; cdecl;
    {class} function _GetDEBUG_GEOGRAPHY_NOT_EEA: Integer; cdecl;
    {class} property DEBUG_GEOGRAPHY_DISABLED: Integer read _GetDEBUG_GEOGRAPHY_DISABLED;
    {class} property DEBUG_GEOGRAPHY_EEA: Integer read _GetDEBUG_GEOGRAPHY_EEA;
    {class} property DEBUG_GEOGRAPHY_NOT_EEA: Integer read _GetDEBUG_GEOGRAPHY_NOT_EEA;
  end;

  [JavaSignature('com/google/android/ump/ConsentDebugSettings$DebugGeography')]
  JConsentDebugSettings_DebugGeography = interface(JAnnotation)
    ['{171AE55A-42A9-45F6-8751-4059F67644D4}']
  end;
  TJConsentDebugSettings_DebugGeography = class(TJavaGenericImport<JConsentDebugSettings_DebugGeographyClass,
    JConsentDebugSettings_DebugGeography>) end;

  JConsentForm_OnConsentFormDismissedListenerClass = interface(IJavaClass)
    ['{7A7C6686-63F1-4ABD-A0D2-8CE94CC0E665}']
  end;

  [JavaSignature('com/google/android/ump/ConsentForm$OnConsentFormDismissedListener')]
  JConsentForm_OnConsentFormDismissedListener = interface(IJavaInstance)
    ['{BCC2F823-7583-4DC3-B5B8-AC9F37E4CB0F}']
    procedure onConsentFormDismissed(formError: JFormError); cdecl;
  end;
  TJConsentForm_OnConsentFormDismissedListener = class(TJavaGenericImport<JConsentForm_OnConsentFormDismissedListenerClass,
    JConsentForm_OnConsentFormDismissedListener>) end;

  JConsentInformation_ConsentStatusClass = interface(JAnnotationClass)
    ['{4AFE6D44-F5D9-4D41-9366-9C6B75EAA62E}']
    {class} function _GetNOT_REQUIRED: Integer; cdecl;
    {class} function _GetOBTAINED: Integer; cdecl;
    {class} function _GetREQUIRED: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} property NOT_REQUIRED: Integer read _GetNOT_REQUIRED;
    {class} property OBTAINED: Integer read _GetOBTAINED;
    {class} property REQUIRED: Integer read _GetREQUIRED;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
  end;

  [JavaSignature('com/google/android/ump/ConsentInformation$ConsentStatus')]
  JConsentInformation_ConsentStatus = interface(JAnnotation)
    ['{09B32BA5-DA45-4BA3-808D-78374D64F41B}']
  end;
  TJConsentInformation_ConsentStatus = class(TJavaGenericImport<JConsentInformation_ConsentStatusClass, JConsentInformation_ConsentStatus>) end;

  JConsentInformation_PrivacyOptionsRequirementStatusClass = interface(JEnumClass)
    ['{6A31E41A-896E-45B9-B52B-9F455DF967F3}']
    {class} function _GetNOT_REQUIRED: JConsentInformation_PrivacyOptionsRequirementStatus; cdecl;
    {class} function _GetREQUIRED: JConsentInformation_PrivacyOptionsRequirementStatus; cdecl;
    {class} function _GetUNKNOWN: JConsentInformation_PrivacyOptionsRequirementStatus; cdecl;
    {class} function valueOf(string_: JString): JConsentInformation_PrivacyOptionsRequirementStatus; cdecl;
    {class} function values: TJavaObjectArray<JConsentInformation_PrivacyOptionsRequirementStatus>; cdecl;
    {class} property NOT_REQUIRED: JConsentInformation_PrivacyOptionsRequirementStatus read _GetNOT_REQUIRED;
    {class} property REQUIRED: JConsentInformation_PrivacyOptionsRequirementStatus read _GetREQUIRED;
    {class} property UNKNOWN: JConsentInformation_PrivacyOptionsRequirementStatus read _GetUNKNOWN;
  end;

  [JavaSignature('com/google/android/ump/ConsentInformation$PrivacyOptionsRequirementStatus')]
  JConsentInformation_PrivacyOptionsRequirementStatus = interface(JEnum)
    ['{FDBE40B8-DFCA-4E5A-A8AA-BE50F3F55C86}']
  end;
  TJConsentInformation_PrivacyOptionsRequirementStatus = class(TJavaGenericImport<JConsentInformation_PrivacyOptionsRequirementStatusClass,
    JConsentInformation_PrivacyOptionsRequirementStatus>) end;

  JConsentRequestParametersClass = interface(JObjectClass)
    ['{80881356-CB49-41CA-BECE-0904F2CFF1FD}']
  end;

  [JavaSignature('com/google/android/ump/ConsentRequestParameters')]
  JConsentRequestParameters = interface(JObject)
    ['{E9016BAC-948C-403D-B50C-B8BFB188B0D4}']
    function getConsentDebugSettings: JConsentDebugSettings; cdecl;
    function isTagForUnderAgeOfConsent: Boolean; cdecl;
  end;
  TJConsentRequestParameters = class(TJavaGenericImport<JConsentRequestParametersClass, JConsentRequestParameters>) end;

  JConsentRequestParameters_BuilderClass = interface(JObjectClass)
    ['{24BF2021-8386-41FB-B0CB-48301157320F}']
    {class} function init: JConsentRequestParameters_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/ump/ConsentRequestParameters$Builder')]
  JConsentRequestParameters_Builder = interface(JObject)
    ['{23F29DD8-F624-42E6-928F-FC8E5DA31248}']
    function build: JConsentRequestParameters; cdecl;
    function setAdMobAppId(string_: JString): JConsentRequestParameters_Builder; cdecl;
    function setConsentDebugSettings(consentDebugSettings: JConsentDebugSettings): JConsentRequestParameters_Builder; cdecl;
    function setTagForUnderAgeOfConsent(b: Boolean): JConsentRequestParameters_Builder; cdecl;
  end;
  TJConsentRequestParameters_Builder = class(TJavaGenericImport<JConsentRequestParameters_BuilderClass, JConsentRequestParameters_Builder>) end;

  JFormErrorClass = interface(JObjectClass)
    ['{CCFE8553-8782-4939-BD12-0BE670236B49}']
    {class} function init(i: Integer; string_: JString): JFormError; cdecl;
  end;

  [JavaSignature('com/google/android/ump/FormError')]
  JFormError = interface(JObject)
    ['{DFC3374C-8667-430A-86BD-88F8DCE54C40}']
    function getErrorCode: Integer; cdecl;
    function getMessage: JString; cdecl;
  end;
  TJFormError = class(TJavaGenericImport<JFormErrorClass, JFormError>) end;

  JFormError_ErrorCodeClass = interface(JAnnotationClass)
    ['{EADFEAAA-28FF-40EB-BC76-CEDDA15C9E28}']
    {class} function _GetINTERNAL_ERROR: Integer; cdecl;
    {class} function _GetINTERNET_ERROR: Integer; cdecl;
    {class} function _GetINVALID_OPERATION: Integer; cdecl;
    {class} function _GetTIME_OUT: Integer; cdecl;
    {class} property INTERNAL_ERROR: Integer read _GetINTERNAL_ERROR;
    {class} property INTERNET_ERROR: Integer read _GetINTERNET_ERROR;
    {class} property INVALID_OPERATION: Integer read _GetINVALID_OPERATION;
    {class} property TIME_OUT: Integer read _GetTIME_OUT;
  end;

  [JavaSignature('com/google/android/ump/FormError$ErrorCode')]
  JFormError_ErrorCode = interface(JAnnotation)
    ['{938F6901-9864-45EE-B519-23639AC08450}']
  end;
  TJFormError_ErrorCode = class(TJavaGenericImport<JFormError_ErrorCodeClass, JFormError_ErrorCode>) end;

  JUserMessagingPlatformClass = interface(JObjectClass)
    ['{4BEAC8F0-5936-458A-A465-1D065A2663F2}']
    {class} function getConsentInformation(context: JContext): JConsentInformation; cdecl;
    {class} procedure loadAndShowConsentFormIfRequired(activity: JActivity;
      onConsentFormDismissedListener: JConsentForm_OnConsentFormDismissedListener); cdecl;
    {class} procedure loadConsentForm(context: JContext; onConsentFormLoadSuccessListener: JUserMessagingPlatform_OnConsentFormLoadSuccessListener;
      onConsentFormLoadFailureListener: JUserMessagingPlatform_OnConsentFormLoadFailureListener); cdecl;
    {class} procedure showPrivacyOptionsForm(activity: JActivity; onConsentFormDismissedListener: JConsentForm_OnConsentFormDismissedListener); cdecl;
  end;

  [JavaSignature('com/google/android/ump/UserMessagingPlatform')]
  JUserMessagingPlatform = interface(JObject)
    ['{7CE344C0-D49D-4647-8CC4-E2416E6F2F5B}']
  end;
  TJUserMessagingPlatform = class(TJavaGenericImport<JUserMessagingPlatformClass, JUserMessagingPlatform>) end;

implementation

end.

