unit DW.Macapi.LocalAuthentication;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.Security;

const
  LAPolicyDeviceOwnerAuthenticationWithBiometrics = 1;
  LAPolicyDeviceOwnerAuthentication = 2;
  LAPolicyDeviceOwnerAuthenticationWithWatch = 3;
  LAPolicyDeviceOwnerAuthenticationWithBiometricsOrWatch = 4;
  LAPolicyDeviceOwnerAuthenticationWithWristDetection = 5;
  LACredentialTypeApplicationPassword = 0;
  LACredentialTypeSmartCardPIN = -3;
  LAAccessControlOperationCreateItem = 0;
  LAAccessControlOperationUseItem = 1;
  LAAccessControlOperationCreateKey = 2;
  LAAccessControlOperationUseKeySign = 3;
  LAAccessControlOperationUseKeyDecrypt = 4;
  LAAccessControlOperationUseKeyKeyExchange = 5;
  LABiometryTypeNone = 0;
  LABiometryNone = LABiometryTypeNone;
  LABiometryTypeTouchID = 1;
  LABiometryTypeFaceID = 2;
  LABiometryTypeOpticID = 4;
  LAErrorAuthenticationFailed = -1;
  LAErrorUserCancel = -2;
  LAErrorUserFallback = -3;
  LAErrorSystemCancel = -4;
  LAErrorPasscodeNotSet = -5;
  LAErrorTouchIDNotAvailable = -6;
  LAErrorTouchIDNotEnrolled = -7;
  LAErrorTouchIDLockout = -8;
  LAErrorAppCancel = -9;
  LAErrorInvalidContext = -10;
  LAErrorBiometryNotAvailable = -6;
  LAErrorBiometryNotEnrolled = -7;
  LAErrorBiometryLockout = -8;
  LAErrorNotInteractive = -1004;
  LAErrorWatchNotAvailable = -11;
  LAErrorBiometryNotPaired = -12;
  LAErrorBiometryDisconnected = -13;
  LAErrorInvalidDimensions = -14;
  LARightStateUnknown = 0;
  LARightStateAuthorizing = 1;
  LARightStateAuthorized = 2;
  LARightStateNotAuthorized = 3;

type
  LAContext = interface;
  LARight = interface;
  LAPersistedRight = interface;
  LAPrivateKey = interface;
  LAPublicKey = interface;
  LAAuthenticationRequirement = interface;
  LABiometryFallbackRequirement = interface;
  LARightStore = interface;
  LASecret = interface;

  LAPolicy = NSInteger;
  LACredentialType = NSInteger;
  LAAccessControlOperation = NSInteger;
  LABiometryType = NSInteger;
  LAError = NSInteger;
  LARightState = NSInteger;
  // ** Security framework **
  SecAccessControlRef = Pointer;
  SecKeyAlgorithm = CFStringRef;

  TLAContextBlockMethod1 = procedure(success: Boolean; error: NSError) of object;
  TLARightBlockMethod1 = procedure(error: NSError) of object;
  TLARightBlockMethod2 = procedure of object;
  TLAPrivateKeyBlockMethod1 = procedure(param1: NSData; param2: NSError) of object;
  TLAPublicKeyBlockMethod1 = procedure(param1: NSData; param2: NSError) of object;
  TLAPublicKeyBlockMethod2 = procedure(param1: NSError) of object;
  TLARightStoreBlockMethod1 = procedure(param1: LAPersistedRight; param2: NSError) of object;
  TLARightStoreBlockMethod2 = procedure(error: NSError) of object;
  TLASecretBlockMethod1 = procedure(param1: NSData; param2: NSError) of object;

  LAContextClass = interface(NSObjectClass)
    ['{62B9ABA4-15BF-4BBD-A23E-4C3766FED385}']
  end;

  LAContext = interface(NSObject)
    ['{D571FC0E-AC3C-4033-81C3-2F31BB09F031}']
    function biometryType: LABiometryType; cdecl;
    function canEvaluatePolicy(policy: LAPolicy; error: PPointer): Boolean; cdecl;
    procedure evaluateAccessControl(accessControl: SecAccessControlRef; operation: LAAccessControlOperation; localizedReason: NSString; reply: TLAContextBlockMethod1); cdecl;
    function evaluatedPolicyDomainState: NSData; cdecl;
    procedure evaluatePolicy(policy: LAPolicy; localizedReason: NSString; reply: TLAContextBlockMethod1); cdecl;
    function interactionNotAllowed: Boolean; cdecl;
    procedure invalidate; cdecl;
    function isCredentialSet(&type: LACredentialType): Boolean; cdecl;
    function localizedCancelTitle: NSString; cdecl;
    function localizedFallbackTitle: NSString; cdecl;
    function localizedReason: NSString; cdecl;
    function maxBiometryFailures: NSNumber; cdecl; // API_DEPRECATED("No longer supported", ios(8.3, 9.0), macos(10.10.3, 10.11))
    function setCredential(credential: NSData; &type: LACredentialType): Boolean; cdecl;
    procedure setInteractionNotAllowed(interactionNotAllowed: Boolean); cdecl;
    procedure setLocalizedCancelTitle(localizedCancelTitle: NSString); cdecl;
    procedure setLocalizedFallbackTitle(localizedFallbackTitle: NSString); cdecl;
    procedure setLocalizedReason(localizedReason: NSString); cdecl;
    procedure setMaxBiometryFailures(maxBiometryFailures: NSNumber); cdecl; // API_DEPRECATED("No longer supported", ios(8.3, 9.0), macos(10.10.3, 10.11))
    procedure setTouchIDAuthenticationAllowableReuseDuration(touchIDAuthenticationAllowableReuseDuration: NSTimeInterval); cdecl;
    function touchIDAuthenticationAllowableReuseDuration: NSTimeInterval; cdecl;
  end;
  TLAContext = class(TOCGenericImport<LAContextClass, LAContext>) end;

  LARightClass = interface(NSObjectClass)
    ['{2E822E8C-5C12-407A-8DD7-B61EE1F14C33}']
  end;

  LARight = interface(NSObject)
    ['{99353BB2-B3F0-4829-A375-6A97DFA4C1CE}']
    procedure authorizeWithLocalizedReason(localizedReason: NSString; completion: TLARightBlockMethod1); cdecl;
    procedure checkCanAuthorizeWithCompletion(handler: TLARightBlockMethod1); cdecl;
    procedure deauthorizeWithCompletion(handler: TLARightBlockMethod2); cdecl;
    function initWithRequirement(requirement: LAAuthenticationRequirement): Pointer; cdecl;
    procedure setTag(tag: NSInteger); cdecl;
    function state: LARightState; cdecl;
    function tag: NSInteger; cdecl;
  end;
  TLARight = class(TOCGenericImport<LARightClass, LARight>) end;

  LAPersistedRightClass = interface(LARightClass)
    ['{5F39D0D4-E724-46BD-A5D5-36DEC907AEFA}']
    {class} function new: Pointer; cdecl;
  end;

  LAPersistedRight = interface(LARight)
    ['{E681896F-6AF0-44F3-BC64-6E6666133839}']
    function key: LAPrivateKey; cdecl;
    function secret: LASecret; cdecl;
  end;
  TLAPersistedRight = class(TOCGenericImport<LAPersistedRightClass, LAPersistedRight>) end;

  LAPrivateKeyClass = interface(NSObjectClass)
    ['{4686EA65-93ED-4BF2-AA78-7A0900B40094}']
    {class} function new: Pointer; cdecl;
  end;

  LAPrivateKey = interface(NSObject)
    ['{9006B4E2-7202-41F4-9951-95ACC66E5458}']
    function canDecryptUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    function canExchangeKeysUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    function canSignUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    procedure decryptData(data: NSData; secKeyAlgorithm: SecKeyAlgorithm; completion: TLAPrivateKeyBlockMethod1); cdecl;
    procedure exchangeKeysWithPublicKey(publicKey: NSData; secKeyAlgorithm: SecKeyAlgorithm; secKeyParameters: NSDictionary; completion: TLAPrivateKeyBlockMethod1); cdecl;
    function publicKey: LAPublicKey; cdecl;
    procedure signData(data: NSData; secKeyAlgorithm: SecKeyAlgorithm; completion: TLAPrivateKeyBlockMethod1); cdecl;
  end;
  TLAPrivateKey = class(TOCGenericImport<LAPrivateKeyClass, LAPrivateKey>) end;

  LAPublicKeyClass = interface(NSObjectClass)
    ['{1AFD374E-8400-4A52-8492-981B6D1FA387}']
    {class} function new: Pointer; cdecl;
  end;

  LAPublicKey = interface(NSObject)
    ['{C59A64DE-7647-4B2C-BD21-99E3FC5D176F}']
    function canEncryptUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    function canVerifyUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    procedure encryptData(data: NSData; secKeyAlgorithm: SecKeyAlgorithm; completion: TLAPublicKeyBlockMethod1); cdecl;
    procedure exportBytesWithCompletion(handler: TLAPublicKeyBlockMethod1); cdecl;
    procedure verifyData(signedData: NSData; signature: NSData; secKeyAlgorithm: SecKeyAlgorithm; completion: TLAPublicKeyBlockMethod2); cdecl;
  end;
  TLAPublicKey = class(TOCGenericImport<LAPublicKeyClass, LAPublicKey>) end;

  LAAuthenticationRequirementClass = interface(NSObjectClass)
    ['{8D9AFF4B-292D-46D2-A949-9810358BA084}']
    {class} function biometryCurrentSetRequirement: LAAuthenticationRequirement; cdecl;
    {class} function biometryRequirement: LAAuthenticationRequirement; cdecl;
    {class} function biometryRequirementWithFallback(fallback: LABiometryFallbackRequirement): Pointer; cdecl;
    {class} function defaultRequirement: LAAuthenticationRequirement; cdecl;
  end;

  LAAuthenticationRequirement = interface(NSObject)
    ['{FA00FEF2-B79B-45F0-B1E1-9D4FAD2D8E66}']
  end;
  TLAAuthenticationRequirement = class(TOCGenericImport<LAAuthenticationRequirementClass, LAAuthenticationRequirement>) end;

  LABiometryFallbackRequirementClass = interface(NSObjectClass)
    ['{77DCEADD-B375-484E-A108-B21DF1C4B743}']
    {class} function defaultRequirement: LABiometryFallbackRequirement; cdecl;
    {class} function devicePasscodeRequirement: LABiometryFallbackRequirement; cdecl;
  end;

  LABiometryFallbackRequirement = interface(NSObject)
    ['{25BC64B0-B210-4A6E-9D36-D7DC6635B260}']
  end;
  TLABiometryFallbackRequirement = class(TOCGenericImport<LABiometryFallbackRequirementClass, LABiometryFallbackRequirement>) end;

  LARightStoreClass = interface(NSObjectClass)
    ['{1A499EA9-A901-4D9D-A285-CBD584FA16EE}']
    {class} function new: Pointer; cdecl;
    {class} function sharedStore: LARightStore; cdecl;
  end;

  LARightStore = interface(NSObject)
    ['{1E3824EB-C04F-499D-BBC0-4947F86053B7}']
    procedure removeAllRightsWithCompletion(handler: TLARightStoreBlockMethod2); cdecl;
    procedure removeRight(right: LAPersistedRight; completion: TLARightStoreBlockMethod2); cdecl;
    procedure removeRightForIdentifier(identifier: NSString; completion: TLARightStoreBlockMethod2); cdecl;
    procedure rightForIdentifier(identifier: NSString; completion: TLARightStoreBlockMethod1); cdecl;
    procedure saveRight(right: LARight; identifier: NSString; secret: NSData; completion: TLARightStoreBlockMethod1); overload; cdecl;
    procedure saveRight(right: LARight; identifier: NSString; completion: TLARightStoreBlockMethod1); overload; cdecl;
  end;
  TLARightStore = class(TOCGenericImport<LARightStoreClass, LARightStore>) end;

  LASecretClass = interface(NSObjectClass)
    ['{608413FF-11D7-4BF6-A9B8-21BFBF30AF72}']
    {class} function new: Pointer; cdecl;
  end;

  LASecret = interface(NSObject)
    ['{CEC8990F-A589-4FBC-8934-277DB9AB40FB}']
    procedure loadDataWithCompletion(handler: TLASecretBlockMethod1); cdecl;
  end;
  TLASecret = class(TOCGenericImport<LASecretClass, LASecret>) end;

function LATouchIDAuthenticationMaximumAllowableReuseDuration: NSTimeInterval;
function LAErrorDomain: NSString;

const
  libLocalAuthentication = '/System/Library/Frameworks/LocalAuthentication.framework/LocalAuthentication';

implementation

uses
  System.SysUtils;

var
  LocalAuthenticationModule: THandle;

function LATouchIDAuthenticationMaximumAllowableReuseDuration: NSTimeInterval;
begin
  Result := CocoaDoubleConst(libLocalAuthentication, 'LATouchIDAuthenticationMaximumAllowableReuseDuration');
end;

function LAErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libLocalAuthentication, 'LAErrorDomain');
end;

initialization
  LocalAuthenticationModule := LoadLibrary(libLocalAuthentication);

finalization
  if LocalAuthenticationModule <> 0 then
    FreeLibrary(LocalAuthenticationModule);

end.