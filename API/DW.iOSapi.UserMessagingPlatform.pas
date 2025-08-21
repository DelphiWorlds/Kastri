unit DW.iOSapi.UserMessagingPlatform;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit;

const
  UMPDebugGeographyDisabled = 0;
  UMPDebugGeographyEEA = 1;
  UMPDebugGeographyNotEEA = 2;
  UMPConsentStatusUnknown = 0;
  UMPConsentStatusRequired = 1;
  UMPConsentStatusNotRequired = 2;
  UMPConsentStatusObtained = 3;
  UMPFormStatusUnknown = 0;
  UMPFormStatusAvailable = 1;
  UMPFormStatusUnavailable = 2;
  UMPRequestErrorCodeInternal = 1;
  UMPRequestErrorCodeInvalidAppID = 2;
  UMPRequestErrorCodeNetwork = 3;
  UMPRequestErrorCodeMisconfiguration = 4;
  UMPFormErrorCodeInternal = 5;
  UMPFormErrorCodeAlreadyUsed = 6;
  UMPFormErrorCodeUnavailable = 7;
  UMPFormErrorCodeTimeout = 8;

type
  UMPDebugSettings = interface;
  UMPRequestParameters = interface;
  UMPConsentInformation = interface;
  UMPConsentForm = interface;

  UMPDebugGeography = NSInteger;
  UMPConsentStatus = NSInteger;
  UMPFormStatus = NSInteger;

  UMPConsentInformationUpdateCompletionHandler = procedure(error: NSError) of object;

  UMPConsentFormLoadCompletionHandler = procedure(consentForm: UMPConsentForm; error: NSError) of object;

  UMPConsentFormPresentCompletionHandler = procedure(error: NSError) of object;
  UMPRequestErrorCode = NSInteger;
  UMPFormErrorCode = NSInteger;

  UMPDebugSettingsClass = interface(NSObjectClass)
    ['{1DA6A452-44A0-4878-BB3C-45C77191BE84}']
  end;

  UMPDebugSettings = interface(NSObject)
    ['{6F03479D-092B-471D-AD1C-40B1667E39A3}']
    function geography: UMPDebugGeography; cdecl;
    procedure setGeography(geography: UMPDebugGeography); cdecl;
    procedure setTestDeviceIdentifiers(testDeviceIdentifiers: NSArray); cdecl;
    function testDeviceIdentifiers: NSArray; cdecl;
  end;
  TUMPDebugSettings = class(TOCGenericImport<UMPDebugSettingsClass, UMPDebugSettings>) end;

  UMPRequestParametersClass = interface(NSObjectClass)
    ['{D095F6D4-0723-4B60-A3BD-E88E0A5C44C2}']
  end;

  UMPRequestParameters = interface(NSObject)
    ['{BB4E84D3-D839-4759-BDF7-2FCCA158DBDC}']
    function debugSettings: UMPDebugSettings; cdecl;
    procedure setDebugSettings(debugSettings: UMPDebugSettings); cdecl;
    procedure setTagForUnderAgeOfConsent(tagForUnderAgeOfConsent: Boolean); cdecl;
    function tagForUnderAgeOfConsent: Boolean; cdecl;
  end;
  TUMPRequestParameters = class(TOCGenericImport<UMPRequestParametersClass, UMPRequestParameters>) end;

  UMPConsentInformationClass = interface(NSObjectClass)
    ['{ECBCCE0E-A3D1-49E4-BF2D-064AF1121C07}']
    {class} function sharedInstance: UMPConsentInformation; cdecl;
  end;

  UMPConsentInformation = interface(NSObject)
    ['{DA65165D-70A3-4E44-AD44-F7FD3E55F147}']
    function consentStatus: UMPConsentStatus; cdecl;
    function formStatus: UMPFormStatus; cdecl;
    procedure requestConsentInfoUpdateWithParameters(parameters: UMPRequestParameters; completionHandler: UMPConsentInformationUpdateCompletionHandler); cdecl;
    procedure reset; cdecl;
  end;
  TUMPConsentInformation = class(TOCGenericImport<UMPConsentInformationClass, UMPConsentInformation>) end;

  UMPConsentFormClass = interface(NSObjectClass)
    ['{31A61A37-706C-4024-A629-E40C99C6F915}']
    {class} procedure loadWithCompletionHandler(completionHandler: UMPConsentFormLoadCompletionHandler); cdecl;
  end;

  UMPConsentForm = interface(NSObject)
    ['{2EDC2A6E-8803-4CDB-AD09-8B12E4983F19}']
    procedure presentFromViewController(viewController: UIViewController; completionHandler: UMPConsentFormPresentCompletionHandler); cdecl;
  end;
  TUMPConsentForm = class(TOCGenericImport<UMPConsentFormClass, UMPConsentForm>) end;

implementation

end.