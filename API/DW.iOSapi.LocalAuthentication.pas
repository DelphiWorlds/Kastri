unit DW.iOSapi.LocalAuthentication;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.Security;

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
    ['{D88393B6-82F6-4B37-BDEC-7C928AC532E9}']
  end;

  LAContext = interface(NSObject)
    ['{70E977AD-F560-4A46-AA97-65DDC1DFB551}']
    function biometryType: LABiometryType; cdecl;
    function canEvaluatePolicy(policy: LAPolicy; error: PPointer): Boolean; cdecl;
    procedure evaluateAccessControl(accessControl: SecAccessControlRef; operation: LAAccessControlOperation; localizedReason: NSString;
      reply: TLAContextBlockMethod1); cdecl;
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
    ['{9DD61749-3F01-4BCD-8EAC-D82166E384D6}']
  end;

  LARight = interface(NSObject)
    ['{06C58D76-618C-4FB9-8FB4-1131CB4814E2}']
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
    ['{3EEC3D0C-25CE-4F5A-8BB4-E1255C04673F}']
    {class} function new: Pointer; cdecl;
  end;

  LAPersistedRight = interface(LARight)
    ['{252C3DDB-94DD-42F0-ACB6-A9233AE4BE99}']
    function key: LAPrivateKey; cdecl;
    function secret: LASecret; cdecl;
  end;
  TLAPersistedRight = class(TOCGenericImport<LAPersistedRightClass, LAPersistedRight>) end;

  LAPrivateKeyClass = interface(NSObjectClass)
    ['{11A18FFB-20D2-4498-BF7B-430F134C538C}']
    {class} function new: Pointer; cdecl;
  end;

  LAPrivateKey = interface(NSObject)
    ['{84192C62-D0C3-4F6E-AB00-2A3FB6A301E7}']
    function canDecryptUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    function canExchangeKeysUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    function canSignUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    procedure decryptData(data: NSData; secKeyAlgorithm: SecKeyAlgorithm; completion: TLAPrivateKeyBlockMethod1); cdecl;
    procedure exchangeKeysWithPublicKey(publicKey: NSData; secKeyAlgorithm: SecKeyAlgorithm; secKeyParameters: NSDictionary;
      completion: TLAPrivateKeyBlockMethod1); cdecl;
    function publicKey: LAPublicKey; cdecl;
    procedure signData(data: NSData; secKeyAlgorithm: SecKeyAlgorithm; completion: TLAPrivateKeyBlockMethod1); cdecl;
  end;
  TLAPrivateKey = class(TOCGenericImport<LAPrivateKeyClass, LAPrivateKey>) end;

  LAPublicKeyClass = interface(NSObjectClass)
    ['{BAD6DB46-78AC-48A8-B645-68798D9FE49F}']
    {class} function new: Pointer; cdecl;
  end;

  LAPublicKey = interface(NSObject)
    ['{CA893B8A-63E6-43AE-97D9-BE05AA41CC1B}']
    function canEncryptUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    function canVerifyUsingSecKeyAlgorithm(algorithm: SecKeyAlgorithm): Boolean; cdecl;
    procedure encryptData(data: NSData; secKeyAlgorithm: SecKeyAlgorithm; completion: TLAPublicKeyBlockMethod1); cdecl;
    procedure exportBytesWithCompletion(handler: TLAPublicKeyBlockMethod1); cdecl;
    procedure verifyData(signedData: NSData; signature: NSData; secKeyAlgorithm: SecKeyAlgorithm; completion: TLAPublicKeyBlockMethod2); cdecl;
  end;
  TLAPublicKey = class(TOCGenericImport<LAPublicKeyClass, LAPublicKey>) end;

  LAAuthenticationRequirementClass = interface(NSObjectClass)
    ['{99590523-0988-4784-BCF8-DE89655AF269}']
    {class} function biometryCurrentSetRequirement: LAAuthenticationRequirement; cdecl;
    {class} function biometryRequirement: LAAuthenticationRequirement; cdecl;
    {class} function biometryRequirementWithFallback(fallback: LABiometryFallbackRequirement): Pointer; cdecl;
    {class} function defaultRequirement: LAAuthenticationRequirement; cdecl;
  end;

  LAAuthenticationRequirement = interface(NSObject)
    ['{98981A94-F9A6-4B52-A3B8-77A83031B6A6}']
  end;
  TLAAuthenticationRequirement = class(TOCGenericImport<LAAuthenticationRequirementClass, LAAuthenticationRequirement>) end;

  LABiometryFallbackRequirementClass = interface(NSObjectClass)
    ['{BF321329-95FB-4119-A9B8-BA084B95F343}']
    {class} function defaultRequirement: LABiometryFallbackRequirement; cdecl;
    {class} function devicePasscodeRequirement: LABiometryFallbackRequirement; cdecl;
  end;

  LABiometryFallbackRequirement = interface(NSObject)
    ['{A02EDE75-F835-45C8-9D9F-E6DB274D9982}']
  end;
  TLABiometryFallbackRequirement = class(TOCGenericImport<LABiometryFallbackRequirementClass, LABiometryFallbackRequirement>) end;

  LARightStoreClass = interface(NSObjectClass)
    ['{B2EA078A-665B-429B-A292-BD4FD5C7B495}']
    {class} function new: Pointer; cdecl;
    {class} function sharedStore: LARightStore; cdecl;
  end;

  LARightStore = interface(NSObject)
    ['{3C0E46E7-F7A6-4C0A-9FD8-8BA2CF13C37F}']
    procedure removeAllRightsWithCompletion(handler: TLARightStoreBlockMethod2); cdecl;
    procedure removeRight(right: LAPersistedRight; completion: TLARightStoreBlockMethod2); cdecl;
    procedure removeRightForIdentifier(identifier: NSString; completion: TLARightStoreBlockMethod2); cdecl;
    procedure rightForIdentifier(identifier: NSString; completion: TLARightStoreBlockMethod1); cdecl;
    procedure saveRight(right: LARight; identifier: NSString; secret: NSData; completion: TLARightStoreBlockMethod1); overload; cdecl;
    procedure saveRight(right: LARight; identifier: NSString; completion: TLARightStoreBlockMethod1); overload; cdecl;
  end;
  TLARightStore = class(TOCGenericImport<LARightStoreClass, LARightStore>) end;

  LASecretClass = interface(NSObjectClass)
    ['{69565A3D-F2F7-4CD4-9E0C-6E7CA6E43258}']
    {class} function new: Pointer; cdecl;
  end;

  LASecret = interface(NSObject)
    ['{D25CBD9A-AC07-478F-96C3-3CE407012937}']
    procedure loadDataWithCompletion(handler: TLASecretBlockMethod1); cdecl;
  end;
  TLASecret = class(TOCGenericImport<LASecretClass, LASecret>) end;

function LATouchIDAuthenticationMaximumAllowableReuseDuration: NSTimeInterval;
function LAErrorDomain: NSString;

const
  libLocalAuthentication = '/System/Library/Frameworks/LocalAuthentication.framework/LocalAuthentication';

implementation

uses
  Posix.Dlfcn;

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
  LocalAuthenticationModule := dlopen(MarshaledAString(libLocalAuthentication), RTLD_LAZY);

finalization
  dlclose(LocalAuthenticationModule);

end.