unit DW.Macapi.AuthenticationServices;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit, Macapi.Security,
  // DW
  DW.Macapi.Foundation, DW.Macapi.LocalAuthentication;

const
  ASWebAuthenticationSessionErrorCodeCanceledLogin = 1;
  ASWebAuthenticationSessionErrorCodePresentationContextNotProvided = 2;
  ASWebAuthenticationSessionErrorCodePresentationContextInvalid = 3;
  ASCredentialIdentityStoreErrorCodeInternalError = 0;
  ASCredentialIdentityStoreErrorCodeStoreDisabled = 1;
  ASCredentialIdentityStoreErrorCodeStoreBusy = 2;
  ASCredentialIdentityTypesAll = 0;
  ASCredentialIdentityTypesPassword = 1;
  ASCredentialIdentityTypesPasskey = 2;
  ASExtensionErrorCodeFailed = 0;
  ASExtensionErrorCodeUserCanceled = 1;
  ASExtensionErrorCodeUserInteractionRequired = 100;
  ASExtensionErrorCodeCredentialIdentityNotFound = 101;
  ASCredentialServiceIdentifierTypeDomain = 0;
  ASCredentialServiceIdentifierTypeURL = 1;
  ASUserDetectionStatusUnsupported = 0;
  ASUserDetectionStatusUnknown = 1;
  ASUserDetectionStatusLikelyReal = 2;
  ASUserAgeRangeUnknown = 0;
  ASUserAgeRangeChild = 1;
  ASUserAgeRangeNotChild = 2;
  ASAuthorizationAppleIDProviderCredentialRevoked = 0;
  ASAuthorizationAppleIDProviderCredentialAuthorized = 1;
  ASAuthorizationAppleIDProviderCredentialNotFound = 2;
  ASAuthorizationAppleIDProviderCredentialTransferred = 3;
  ASAuthorizationControllerRequestOptionPreferImmediatelyAvailableCredentials = 1;
  ASAuthorizationErrorUnknown = 1000;
  ASAuthorizationErrorCanceled = 1001;
  ASAuthorizationErrorInvalidResponse = 1002;
  ASAuthorizationErrorNotHandled = 1003;
  ASAuthorizationErrorFailed = 1004;
  ASAuthorizationErrorNotInteractive = 1005;
  ASAuthorizationPublicKeyCredentialAttachmentPlatform = 0;
  ASAuthorizationPublicKeyCredentialAttachmentCrossPlatform = 1;
  ASCredentialRequestTypePassword = 0;
  ASCredentialRequestTypePasskeyAssertion = 1;
  ASAuthorizationAppleIDButtonTypeSignIn = 0;
  ASAuthorizationAppleIDButtonTypeContinue = 1;
  ASAuthorizationAppleIDButtonTypeSignUp = 2;
  ASAuthorizationAppleIDButtonTypeDefault = ASAuthorizationAppleIDButtonTypeSignIn;
  ASAuthorizationAppleIDButtonStyleWhite = 0;
  ASAuthorizationAppleIDButtonStyleWhiteOutline = 1;
  ASAuthorizationAppleIDButtonStyleBlack = 2;
  ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperationRead = 0;
  ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperationWrite = 1;
  ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirementRequired = 0;
  ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirementPreferred = 1;
  ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateAuthorized = 0;
  ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateDenied = 1;
  ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationStateNotDetermined = 2;
  ASAuthorizationProviderExtensionFederationTypeNone = 0;
  ASAuthorizationProviderExtensionFederationTypeWSTrust = 1;
  ASAuthorizationProviderExtensionFederationTypeDynamicWSTrust = 2;
  ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyNone = 0;
  ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyTouchIDOrWatchCurrentSet = 1;
  ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyTouchIDOrWatchAny = 2;
  ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyReuseDuringUnlock = 4;
  ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicyPasswordFallback = 8;
  ASAuthorizationProviderExtensionKeyTypeUserDeviceSigning = 1;
  ASAuthorizationProviderExtensionKeyTypeUserDeviceEncryption = 2;
  ASAuthorizationProviderExtensionKeyTypeUserSecureEnclaveKey = 3;
  ASAuthorizationProviderExtensionKeyTypeSharedDeviceSigning = 4;
  ASAuthorizationProviderExtensionKeyTypeSharedDeviceEncryption = 5;
  ASAuthorizationProviderExtensionKeyTypeCurrentDeviceSigning = 10;
  ASAuthorizationProviderExtensionKeyTypeCurrentDeviceEncryption = 11;
  ASAuthorizationProviderExtensionKeyTypeUserSmartCard = 20;
  ASAuthorizationProviderExtensionAuthenticationMethodPassword = 1;
  ASAuthorizationProviderExtensionAuthenticationMethodUserSecureEnclaveKey = 2;
  ASAuthorizationProviderExtensionAuthenticationMethodSmartCard = 3;
  ASAuthorizationProviderExtensionRequestOptionsNone = 0;
  ASAuthorizationProviderExtensionRequestOptionsUserInteractionEnabled = 1;
  ASAuthorizationProviderExtensionRequestOptionsRegistrationRepair = 2;
  ASAuthorizationProviderExtensionRequestOptionsRegistrationSharedDeviceKeys = 4;
  ASAuthorizationProviderExtensionRequestOptionsRegistrationDeviceKeyMigration = 8;
  ASAuthorizationProviderExtensionRequestOptionsUserKeyInvalid = 32;
  ASAuthorizationProviderExtensionRegistrationResultSuccess = 0;
  ASAuthorizationProviderExtensionRegistrationResultFailed = 1;
  ASAuthorizationProviderExtensionRegistrationResultUserInterfaceRequired = 2;
  ASAuthorizationProviderExtensionRegistrationResultFailedNoRetry = 3;
  ASAuthorizationProviderExtensionSupportedGrantTypesNone = 0;
  ASAuthorizationProviderExtensionSupportedGrantTypesPassword = 1;
  ASAuthorizationProviderExtensionSupportedGrantTypesJWTBearer = 2;
  ASAuthorizationProviderExtensionSupportedGrantTypesSAML1_1 = 4;
  ASAuthorizationProviderExtensionSupportedGrantTypesSAML2_0 = 8;
  ASAuthorizationProviderExtensionPlatformSSOProtocolVersion1_0 = 0;
  ASAuthorizationProviderExtensionPlatformSSOProtocolVersion2_0 = 1;
  ASPublicKeyCredentialClientDataCrossOriginValueNotSet = 0;
  ASPublicKeyCredentialClientDataCrossOriginValueCrossOrigin = 1;
  ASPublicKeyCredentialClientDataCrossOriginValueSameOriginWithAncestors = 2;

type
  ASWebAuthenticationSession = interface;
  ASWebAuthenticationPresentationContextProviding = interface;
  ASWebAuthenticationSessionRequestDelegate = interface;
  ASWebAuthenticationSessionRequest = interface;
  ASWebAuthenticationSessionWebBrowserSessionHandling = interface;
  ASWebAuthenticationSessionWebBrowserSessionManager = interface;
  ASCredentialIdentityStore = interface;
  ASCredentialIdentityStoreState = interface;
  // ASCredentialProviderExtensionContext = interface;
  ASCredentialServiceIdentifier = interface;
  ASAuthorizationCredential = interface;
  ASPasswordCredential = interface;
  ASCredentialIdentity = interface;
  ASPasswordCredentialIdentity = interface;
  ASAuthorization = interface;
  ASAuthorizationAppleIDCredential = interface;
  ASAuthorizationProvider = interface;
  ASAuthorizationRequest = interface;
  ASAuthorizationOpenIDRequest = interface;
  ASAuthorizationAppleIDRequest = interface;
  ASAuthorizationAppleIDProvider = interface;
  ASAuthorizationControllerDelegate = interface;
  ASAuthorizationControllerPresentationContextProviding = interface;
  ASAuthorizationController = interface;
  ASAuthorizationPasswordRequest = interface;
  ASAuthorizationPasswordProvider = interface;
  ASAuthorizationSingleSignOnCredential = interface;
  ASAuthorizationSingleSignOnRequest = interface;
  ASAuthorizationSingleSignOnProvider = interface;
  ASAuthorizationProviderExtensionAuthorizationRequestHandler = interface;
  ASAuthorizationProviderExtensionAuthorizationRequest = interface;
  ASAuthorizationProviderExtensionAuthorizationResult = interface;
  ASAuthorizationPublicKeyCredentialRegistrationRequest = interface;
  ASCredentialRequest = interface;
  ASPasskeyCredentialRequestParameters = interface;
  // ASCredentialProviderViewController = interface;
  ASAccountAuthenticationModificationControllerDelegate = interface;
  ASAccountAuthenticationModificationControllerPresentationContextProviding = interface;
  ASAccountAuthenticationModificationController = interface;
  // ASAccountAuthenticationModificationExtensionContext = interface;
  ASAccountAuthenticationModificationRequest = interface;
  ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest = interface;
  ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest = interface;
  // ASAccountAuthenticationModificationViewController = interface;
  ASAuthorizationAppleIDButton = interface;
  ASPublicKeyCredential = interface;
  ASAuthorizationPublicKeyCredentialAssertion = interface;
  ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput = interface;
  ASAuthorizationPlatformPublicKeyCredentialAssertion = interface;
  ASAuthorizationPublicKeyCredentialDescriptor = interface;
  ASAuthorizationPlatformPublicKeyCredentialDescriptor = interface;
  ASAuthorizationPublicKeyCredentialAssertionRequest = interface;
  ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput = interface;
  ASAuthorizationWebBrowserExternallyAuthenticatableRequest = interface;
  ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest = interface;
  ASAuthorizationPlatformPublicKeyCredentialAssertionRequest = interface;
  ASAuthorizationWebBrowserPlatformPublicKeyCredentialProvider = interface;
  ASAuthorizationPlatformPublicKeyCredentialProvider = interface;
  ASAuthorizationPublicKeyCredentialRegistration = interface;
  ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput = interface;
  ASAuthorizationPlatformPublicKeyCredentialRegistration = interface;
  ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput = interface;
  ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest = interface;
  ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest = interface;
  ASAuthorizationPublicKeyCredentialParameters = interface;
  ASAuthorizationSecurityKeyPublicKeyCredentialAssertion = interface;
  ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor = interface;
  ASAuthorizationWebBrowserSecurityKeyPublicKeyCredentialAssertionRequest = interface;
  ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest = interface;
  ASAuthorizationWebBrowserSecurityKeyPublicKeyCredentialProvider = interface;
  ASAuthorizationSecurityKeyPublicKeyCredentialProvider = interface;
  ASAuthorizationSecurityKeyPublicKeyCredentialRegistration = interface;
  ASAuthorizationWebBrowserSecurityKeyPublicKeyCredentialRegistrationRequest = interface;
  ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest = interface;
  ASAuthorizationWebBrowserPlatformPublicKeyCredential = interface;
  ASAuthorizationWebBrowserPublicKeyCredentialManager = interface;
  ASAuthorizationProviderExtensionKerberosMapping = interface;
  ASAuthorizationProviderExtensionLoginConfiguration = interface;
  ASAuthorizationProviderExtensionLoginManager = interface;
  ASAuthorizationProviderExtensionRegistrationHandler = interface;
  ASAuthorizationProviderExtensionUserLoginConfiguration = interface;
  ASPasskeyAssertionCredential = interface;
  ASPasskeyCredentialIdentity = interface;
  ASPasskeyCredentialRequest = interface;
  ASPasskeyRegistrationCredential = interface;
  ASPasswordCredentialRequest = interface;
  ASPublicKeyCredentialClientData = interface;
  ASSettingsHelper = interface;
  ASWebAuthenticationSessionCallback = interface;

  ASPresentationAnchor = NSWindow;
  ASViewController = NSViewController;
  ASImage = NSImage;
  ASWebAuthenticationSessionErrorCode = NSInteger;

  ASWebAuthenticationSessionCompletionHandler = procedure(callbackURL: NSURL; error: NSError) of object;
  ASCredentialIdentityStoreErrorCode = NSInteger;
  ASCredentialIdentityTypes = NSInteger;
  ASExtensionErrorCode = NSInteger;
  ASCredentialServiceIdentifierType = NSInteger;
  ASAuthorizationScope = NSString;
  ASUserDetectionStatus = NSInteger;
  ASUserAgeRange = NSInteger;
  ASAuthorizationOpenIDOperation = NSString;
  ASAuthorizationAppleIDProviderCredentialState = NSInteger;
  ASAuthorizationCustomMethod = NSString;
  ASAuthorizationControllerRequestOptions = NSInteger;
  ASAuthorizationError = NSInteger;
  ASAuthorizationProviderAuthorizationOperation = NSString;
  ASAuthorizationPublicKeyCredentialUserVerificationPreference = NSString;
  ASAuthorizationPublicKeyCredentialAttestationKind = NSString;
  ASAuthorizationPublicKeyCredentialResidentKeyPreference = NSString;
  ASAuthorizationPublicKeyCredentialAttachment = NSInteger;
  ASCredentialRequestType = NSInteger;
  ASAuthorizationAppleIDButtonType = NSInteger;
  ASAuthorizationAppleIDButtonStyle = NSInteger;
  ASCOSEAlgorithmIdentifier = NSInteger;
  ASCOSEEllipticCurveIdentifier = NSInteger;
  ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation = NSInteger;
  ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement = NSInteger;
  ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransport = NSString;
  ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState = NSInteger;
  ASAuthorizationProviderExtensionFederationType = NSInteger;
  ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy = NSInteger;
  ASAuthorizationProviderExtensionKeyType = NSInteger;
  ASAuthorizationProviderExtensionAuthenticationMethod = NSInteger;
  ASAuthorizationProviderExtensionRequestOptions = NSInteger;
  ASAuthorizationProviderExtensionRegistrationResult = NSInteger;
  ASAuthorizationProviderExtensionSupportedGrantTypes = NSInteger;
  ASAuthorizationProviderExtensionPlatformSSOProtocolVersion = NSInteger;
  ASPublicKeyCredentialClientDataCrossOriginValue = NSInteger;
  TASCredentialIdentityStoreBlockMethod1 = procedure(state: ASCredentialIdentityStoreState) of object;
  TASCredentialIdentityStoreBlockMethod2 = procedure(param1: NSArray) of object;
  TASCredentialIdentityStoreBlockMethod3 = procedure(success: Boolean; error: NSError) of object;
  TASCredentialProviderExtensionContextBlockMethod1 = procedure(expired: Boolean) of object;
  TASCredentialProviderExtensionContextBlockMethod2 = procedure(param1: Boolean) of object;
  TASAuthorizationAppleIDProviderBlockMethod1 = procedure(credentialState: ASAuthorizationAppleIDProviderCredentialState; error: NSError) of object;
  TASAuthorizationProviderExtensionAuthorizationRequestBlockMethod1 = procedure(success: Boolean; error: NSError) of object;
  TASAccountAuthenticationModificationExtensionContextBlockMethod1 = procedure(authorization: ASAuthorizationAppleIDCredential; error: NSError) of object;
  TASAuthorizationWebBrowserPublicKeyCredentialManagerBlockMethod1 = procedure(authorizationState: ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState) of object;
  TASAuthorizationWebBrowserPublicKeyCredentialManagerBlockMethod2 = procedure(param1: NSArray) of object;
  TASAuthorizationProviderExtensionLoginConfigurationBlockMethod1 = procedure(loginConfiguration: ASAuthorizationProviderExtensionLoginConfiguration; error: NSError) of object;
  TASAuthorizationProviderExtensionLoginManagerBlockMethod1 = procedure(error: NSError) of object;
  TASAuthorizationProviderExtensionRegistrationHandlerBlockMethod1 = procedure(result: ASAuthorizationProviderExtensionRegistrationResult) of object;
  TASSettingsHelperBlockMethod1 = procedure(error: NSError) of object;

  ASWebAuthenticationSessionClass = interface(NSObjectClass)
    ['{4D2A4557-0618-4735-BEB5-FF7C9C0600D6}']
    {class} function new: Pointer; cdecl;
  end;

  ASWebAuthenticationSession = interface(NSObject)
    ['{14B618FE-2FEC-4CC3-BCA3-2606ACBAB370}']
    function additionalHeaderFields: NSDictionary; cdecl;
    procedure cancel; cdecl;
    function canStart: Boolean; cdecl;
    function initWithURL(URL: NSURL; callbackURLScheme: NSString; completionHandler: ASWebAuthenticationSessionCompletionHandler): Pointer; overload; cdecl; // API_DEPRECATED("Use initWithURL:callback:completionHandler: instead", ios(12.0, API_TO_BE_DEPRECATED), macCatalyst(13.0, API_TO_BE_DEPRECATED), macos(10.15, API_TO_BE_DEPRECATED), watchos(6.2, API_TO_BE_DEPRECATED), tvos(16.0, API_TO_BE_DEPRECATED))
    function initWithURL(URL: NSURL; callback: ASWebAuthenticationSessionCallback; completionHandler: ASWebAuthenticationSessionCompletionHandler): Pointer; overload; cdecl;
    function prefersEphemeralWebBrowserSession: Boolean; cdecl;
    function presentationContextProvider: Pointer; cdecl;
    procedure setAdditionalHeaderFields(additionalHeaderFields: NSDictionary); cdecl;
    procedure setPrefersEphemeralWebBrowserSession(prefersEphemeralWebBrowserSession: Boolean); cdecl;
    procedure setPresentationContextProvider(presentationContextProvider: Pointer); cdecl;
    function start: Boolean; cdecl;
  end;
  TASWebAuthenticationSession = class(TOCGenericImport<ASWebAuthenticationSessionClass, ASWebAuthenticationSession>) end;

  ASWebAuthenticationPresentationContextProviding = interface(IObjectiveC)
    ['{2BDA3C06-4B4C-4A35-BD6E-9CFE328D1D29}']
    function presentationAnchorForWebAuthenticationSession(session: ASWebAuthenticationSession): ASPresentationAnchor; cdecl;
  end;

  ASWebAuthenticationSessionRequestDelegate = interface(IObjectiveC)
    ['{6A0280A2-E632-4528-A22D-A2DF82B3AE87}']
    procedure authenticationSessionRequest(authenticationSessionRequest: ASWebAuthenticationSessionRequest; didCompleteWithCallbackURL: NSURL); overload; cdecl;
    procedure authenticationSessionRequest(authenticationSessionRequest: ASWebAuthenticationSessionRequest; didCancelWithError: NSError); overload; cdecl;
  end;

  ASWebAuthenticationSessionRequestClass = interface(NSObjectClass)
    ['{1F9AD909-9165-4BC6-8A5E-5BACE5826F71}']
    {class} function new: Pointer; cdecl;
  end;

  ASWebAuthenticationSessionRequest = interface(NSObject)
    ['{9026D169-9C58-4218-B838-C749D89114A7}']
    function additionalHeaderFields: NSDictionary; cdecl;
    function callback: ASWebAuthenticationSessionCallback; cdecl;
    function callbackURLScheme: NSString; cdecl; // API_DEPRECATED("Use `callback` to match all callback types.", macCatalyst(13.0, 17.4), macosx(10.15, 14.4))
    procedure cancelWithError(error: NSError); cdecl;
    procedure completeWithCallbackURL(url: NSURL); cdecl;
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function shouldUseEphemeralSession: Boolean; cdecl;
    function URL: NSURL; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TASWebAuthenticationSessionRequest = class(TOCGenericImport<ASWebAuthenticationSessionRequestClass, ASWebAuthenticationSessionRequest>) end;

  ASWebAuthenticationSessionWebBrowserSessionHandling = interface(IObjectiveC)
    ['{D1B2C5C0-06D9-4880-9272-6FA4C0042F49}']
    procedure beginHandlingWebAuthenticationSessionRequest(request: ASWebAuthenticationSessionRequest); cdecl;
    procedure cancelWebAuthenticationSessionRequest(request: ASWebAuthenticationSessionRequest); cdecl;
  end;

  ASWebAuthenticationSessionWebBrowserSessionManagerClass = interface(NSObjectClass)
    ['{1C3D3E71-7744-4773-9B03-34A291EF1397}']
    {class} function sharedManager: ASWebAuthenticationSessionWebBrowserSessionManager; cdecl;
  end;

  ASWebAuthenticationSessionWebBrowserSessionManager = interface(NSObject)
    ['{62D3C835-7D7B-41CE-A2E5-E0E15E235EF0}']
    function sessionHandler: Pointer; cdecl;
    procedure setSessionHandler(sessionHandler: Pointer); cdecl;
    function wasLaunchedByAuthenticationServices: Boolean; cdecl;
  end;
  TASWebAuthenticationSessionWebBrowserSessionManager = class(TOCGenericImport<ASWebAuthenticationSessionWebBrowserSessionManagerClass, ASWebAuthenticationSessionWebBrowserSessionManager>) end;

  ASCredentialIdentityStoreClass = interface(NSObjectClass)
    ['{257C92E1-1762-4832-9014-3BDDB041894C}']
    {class} function sharedStore: ASCredentialIdentityStore; cdecl;
  end;

  ASCredentialIdentityStore = interface(NSObject)
    ['{1FCCB724-F376-4FF3-A4CA-AE64CB407384}']
    procedure getCredentialIdentitiesForService(serviceIdentifier: ASCredentialServiceIdentifier; credentialIdentityTypes: ASCredentialIdentityTypes; completionHandler: TASCredentialIdentityStoreBlockMethod2); cdecl;
    procedure getCredentialIdentityStoreStateWithCompletion(completion: TASCredentialIdentityStoreBlockMethod1); cdecl;
    procedure removeAllCredentialIdentitiesWithCompletion(completion: TASCredentialIdentityStoreBlockMethod3); cdecl;
    procedure removeCredentialIdentities(credentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod3); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("removeCredentialIdentityEntries:completion:", ios(12.0, 17.0), macos(11.0, 14.0))
    procedure removeCredentialIdentityEntries(credentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod3); cdecl;
    procedure replaceCredentialIdentitiesWithIdentities(newCredentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod3); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("replaceCredentialIdentityEntries:completion:", ios(12.0, 17.0), macos(11.0, 14.0))
    procedure replaceCredentialIdentityEntries(newCredentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod3); cdecl;
    procedure saveCredentialIdentities(credentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod3); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("saveCredentialIdentityEntries:completion:", ios(12.0, 17.0), macos(11.0, 14.0))
    procedure saveCredentialIdentityEntries(credentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod3); cdecl;
  end;
  TASCredentialIdentityStore = class(TOCGenericImport<ASCredentialIdentityStoreClass, ASCredentialIdentityStore>) end;

  ASCredentialIdentityStoreStateClass = interface(NSObjectClass)
    ['{EAEFF456-1BD7-4E6C-B0D9-CE4EB3E4C296}']
  end;

  ASCredentialIdentityStoreState = interface(NSObject)
    ['{639FA9F3-221F-40B3-A61E-625521A1FFDF}']
    function isEnabled: Boolean; cdecl;
    function supportsIncrementalUpdates: Boolean; cdecl;
  end;
  TASCredentialIdentityStoreState = class(TOCGenericImport<ASCredentialIdentityStoreStateClass, ASCredentialIdentityStoreState>) end;

  // ***** Could not find class reference for declaration: @interface ASCredentialProviderExtensionContext : NSExtensionContext  
  // (ObjCInterfaceDecl - ObjCInterface) (ASCredentialProviderExtensionContext.h - 21:1 to 67:5)

  ASCredentialServiceIdentifierClass = interface(NSObjectClass)
    ['{4C9C3513-AECC-4C8E-A34E-00D82B46CAE9}']
  end;

  ASCredentialServiceIdentifier = interface(NSObject)
    ['{869AD201-AFF3-4562-87E1-A45EEA57C918}']
    function &type: ASCredentialServiceIdentifierType; cdecl;
    function identifier: NSString; cdecl;
    function initWithIdentifier(identifier: NSString; &type: ASCredentialServiceIdentifierType): Pointer; cdecl;
  end;
  TASCredentialServiceIdentifier = class(TOCGenericImport<ASCredentialServiceIdentifierClass, ASCredentialServiceIdentifier>) end;

  ASAuthorizationCredential = interface(IObjectiveC)
    ['{8B3DBA57-2D45-4A7E-AC0D-0C0A165F5FA1}']
  end;

  ASPasswordCredentialClass = interface(NSObjectClass)
    ['{5A244D9F-3C53-4F71-9E82-4A43489B8B1D}']
    {class} function credentialWithUser(user: NSString; password: NSString): Pointer; cdecl;
  end;

  ASPasswordCredential = interface(NSObject)
    ['{6C420F9F-A8B2-4511-9AEF-38F6462EA90A}']
    function initWithUser(user: NSString; password: NSString): Pointer; cdecl;
    function password: NSString; cdecl;
    function user: NSString; cdecl;
  end;
  TASPasswordCredential = class(TOCGenericImport<ASPasswordCredentialClass, ASPasswordCredential>) end;

  ASCredentialIdentity = interface(IObjectiveC)
    ['{57FEAC0C-CF69-4CD9-A8E5-687205E2286F}']
    function rank: NSInteger; cdecl;
    function recordIdentifier: NSString; cdecl;
    function serviceIdentifier: ASCredentialServiceIdentifier; cdecl;
    procedure setRank(rank: NSInteger); cdecl;
    function user: NSString; cdecl;
  end;

  ASPasswordCredentialIdentityClass = interface(NSObjectClass)
    ['{C9EB4A0B-FCBF-49E9-B6D6-3D64FA64AD22}']
    {class} function identityWithServiceIdentifier(serviceIdentifier: ASCredentialServiceIdentifier; user: NSString; recordIdentifier: NSString): Pointer; cdecl;
  end;

  ASPasswordCredentialIdentity = interface(NSObject)
    ['{5B270ED6-64EC-48A1-8044-C267BE05CC89}']
    function initWithServiceIdentifier(serviceIdentifier: ASCredentialServiceIdentifier; user: NSString; recordIdentifier: NSString): Pointer; cdecl;
    function rank: NSInteger; cdecl;
    function recordIdentifier: NSString; cdecl;
    function serviceIdentifier: ASCredentialServiceIdentifier; cdecl;
    procedure setRank(rank: NSInteger); cdecl;
    function user: NSString; cdecl;
  end;
  TASPasswordCredentialIdentity = class(TOCGenericImport<ASPasswordCredentialIdentityClass, ASPasswordCredentialIdentity>) end;

  ASAuthorizationClass = interface(NSObjectClass)
    ['{37C406A8-DB93-4E22-9890-6F7418B07C57}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorization = interface(NSObject)
    ['{3E7B1273-CDEB-4437-A842-AA4C4D843DC9}']
    function credential: Pointer; cdecl;
    function provider: Pointer; cdecl;
  end;
  TASAuthorization = class(TOCGenericImport<ASAuthorizationClass, ASAuthorization>) end;

  ASAuthorizationAppleIDCredentialClass = interface(NSObjectClass)
    ['{69D1079A-7F8B-4214-A363-909580BC846C}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationAppleIDCredential = interface(NSObject)
    ['{CDAC02C4-034E-4127-B441-D12390ADD700}']
    function authorizationCode: NSData; cdecl;
    function authorizedScopes: NSArray; cdecl;
    function email: NSString; cdecl;
    function fullName: NSPersonNameComponents; cdecl;
    function identityToken: NSData; cdecl;
    function realUserStatus: ASUserDetectionStatus; cdecl;
    function state: NSString; cdecl;
    function user: NSString; cdecl;
    function userAgeRange: ASUserAgeRange; cdecl;
  end;
  TASAuthorizationAppleIDCredential = class(TOCGenericImport<ASAuthorizationAppleIDCredentialClass, ASAuthorizationAppleIDCredential>) end;

  ASAuthorizationProvider = interface(IObjectiveC)
    ['{F7197CFC-78AA-4564-9103-84088C76374B}']
  end;

  ASAuthorizationRequestClass = interface(NSObjectClass)
    ['{358BCFA0-D35B-4350-AFA6-AA28E1D49EA8}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationRequest = interface(NSObject)
    ['{F066746A-A32A-4B87-ABD2-B4AE305199D8}']
    function provider: Pointer; cdecl;
  end;
  TASAuthorizationRequest = class(TOCGenericImport<ASAuthorizationRequestClass, ASAuthorizationRequest>) end;

  ASAuthorizationOpenIDRequestClass = interface(ASAuthorizationRequestClass)
    ['{BEBB8CA3-4269-4375-A4DA-B9380F29976F}']
  end;

  ASAuthorizationOpenIDRequest = interface(ASAuthorizationRequest)
    ['{B9776C63-F81D-4690-9BA9-085679093922}']
    function nonce: NSString; cdecl;
    function requestedOperation: ASAuthorizationOpenIDOperation; cdecl;
    function requestedScopes: NSArray; cdecl;
    procedure setNonce(nonce: NSString); cdecl;
    procedure setRequestedOperation(requestedOperation: ASAuthorizationOpenIDOperation); cdecl;
    procedure setRequestedScopes(requestedScopes: NSArray); cdecl;
    procedure setState(state: NSString); cdecl;
    function state: NSString; cdecl;
  end;
  TASAuthorizationOpenIDRequest = class(TOCGenericImport<ASAuthorizationOpenIDRequestClass, ASAuthorizationOpenIDRequest>) end;

  ASAuthorizationAppleIDRequestClass = interface(ASAuthorizationOpenIDRequestClass)
    ['{CBEB0D09-2576-4AAA-BC95-3FC094D790A6}']
  end;

  ASAuthorizationAppleIDRequest = interface(ASAuthorizationOpenIDRequest)
    ['{AB4C744D-3A10-4326-90A6-0D16039D9F05}']
    procedure setUser(user: NSString); cdecl;
    function user: NSString; cdecl;
  end;
  TASAuthorizationAppleIDRequest = class(TOCGenericImport<ASAuthorizationAppleIDRequestClass, ASAuthorizationAppleIDRequest>) end;

  ASAuthorizationAppleIDProviderClass = interface(NSObjectClass)
    ['{8D567616-AA4D-41A3-862B-97330B146598}']
  end;

  ASAuthorizationAppleIDProvider = interface(NSObject)
    ['{A0C159A4-66C3-436A-9119-D29C2D5B3BBD}']
    function createRequest: ASAuthorizationAppleIDRequest; cdecl;
    procedure getCredentialStateForUserID(userID: NSString; completion: TASAuthorizationAppleIDProviderBlockMethod1); cdecl;
  end;
  TASAuthorizationAppleIDProvider = class(TOCGenericImport<ASAuthorizationAppleIDProviderClass, ASAuthorizationAppleIDProvider>) end;

  ASAuthorizationControllerDelegate = interface(IObjectiveC)
    ['{39F10797-44E1-4D7B-8129-9FBDC0131403}']
    procedure authorizationController(controller: ASAuthorizationController; didCompleteWithCustomMethod: ASAuthorizationCustomMethod); overload; cdecl;
    procedure authorizationController(controller: ASAuthorizationController; didCompleteWithError: NSError); overload; cdecl;
    procedure authorizationController(controller: ASAuthorizationController; didCompleteWithAuthorization: ASAuthorization); overload; cdecl;
  end;

  ASAuthorizationControllerPresentationContextProviding = interface(IObjectiveC)
    ['{F89FF1D3-07DA-467D-9759-10B1D1AFF81D}']
    function presentationAnchorForAuthorizationController(controller: ASAuthorizationController): ASPresentationAnchor; cdecl;
  end;

  ASAuthorizationControllerClass = interface(NSObjectClass)
    ['{20583131-9A26-46CA-9248-B3D74A87EAFB}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationController = interface(NSObject)
    ['{58CF3EBC-067A-47AB-9FD5-B635B29C3E02}']
    function authorizationRequests: NSArray; cdecl;
    procedure cancel; cdecl;
    function customAuthorizationMethods: NSArray; cdecl;
    function delegate: Pointer; cdecl;
    function initWithAuthorizationRequests(authorizationRequests: NSArray): Pointer; cdecl;
    procedure performAutoFillAssistedRequests; cdecl;
    procedure performRequests; cdecl;
    procedure performRequestsWithOptions(options: ASAuthorizationControllerRequestOptions); cdecl;
    function presentationContextProvider: Pointer; cdecl;
    procedure setCustomAuthorizationMethods(customAuthorizationMethods: NSArray); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPresentationContextProvider(presentationContextProvider: Pointer); cdecl;
  end;
  TASAuthorizationController = class(TOCGenericImport<ASAuthorizationControllerClass, ASAuthorizationController>) end;

  ASAuthorizationPasswordRequestClass = interface(ASAuthorizationRequestClass)
    ['{D29AE885-9C6C-4890-B5E2-36AC0B09D4A9}']
  end;

  ASAuthorizationPasswordRequest = interface(ASAuthorizationRequest)
    ['{2DD4BD76-6997-435C-9C5F-E2CC2C67CC1A}']
  end;
  TASAuthorizationPasswordRequest = class(TOCGenericImport<ASAuthorizationPasswordRequestClass, ASAuthorizationPasswordRequest>) end;

  ASAuthorizationPasswordProviderClass = interface(NSObjectClass)
    ['{C43701F3-FFC4-4D05-B19E-F9C7ABE651CC}']
  end;

  ASAuthorizationPasswordProvider = interface(NSObject)
    ['{A8958DC7-A4D8-46C6-930C-8C7723D26118}']
    function createRequest: ASAuthorizationPasswordRequest; cdecl;
  end;
  TASAuthorizationPasswordProvider = class(TOCGenericImport<ASAuthorizationPasswordProviderClass, ASAuthorizationPasswordProvider>) end;

  ASAuthorizationSingleSignOnCredentialClass = interface(NSObjectClass)
    ['{AB79E678-8186-4876-BABC-1E47C35CA194}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationSingleSignOnCredential = interface(NSObject)
    ['{9D0EEBF8-9A5D-46EF-AC98-381CA652A074}']
    function accessToken: NSData; cdecl;
    function authenticatedResponse: NSHTTPURLResponse; cdecl;
    function authorizedScopes: NSArray; cdecl;
    function identityToken: NSData; cdecl;
    function privateKeys: NSArray; cdecl;
    function state: NSString; cdecl;
  end;
  TASAuthorizationSingleSignOnCredential = class(TOCGenericImport<ASAuthorizationSingleSignOnCredentialClass, ASAuthorizationSingleSignOnCredential>) end;

  ASAuthorizationSingleSignOnRequestClass = interface(ASAuthorizationOpenIDRequestClass)
    ['{575C9DB1-E256-4630-A555-08420FAECC9F}']
  end;

  ASAuthorizationSingleSignOnRequest = interface(ASAuthorizationOpenIDRequest)
    ['{89DDDC24-3F1B-4044-9DF2-EB8306DE3DD5}']
    function authorizationOptions: NSArray; cdecl;
    function isUserInterfaceEnabled: Boolean; cdecl;
    procedure setAuthorizationOptions(authorizationOptions: NSArray); cdecl;
    procedure setUserInterfaceEnabled(userInterfaceEnabled: Boolean); cdecl;
  end;
  TASAuthorizationSingleSignOnRequest = class(TOCGenericImport<ASAuthorizationSingleSignOnRequestClass, ASAuthorizationSingleSignOnRequest>) end;

  ASAuthorizationSingleSignOnProviderClass = interface(NSObjectClass)
    ['{DF63C8B8-C198-4698-9D90-B8A1ACE05589}']
    {class} function authorizationProviderWithIdentityProviderURL(url: NSURL): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationSingleSignOnProvider = interface(NSObject)
    ['{9F493FDF-6591-49FA-8275-1D0E9358D3E4}']
    function canPerformAuthorization: Boolean; cdecl;
    function createRequest: ASAuthorizationSingleSignOnRequest; cdecl;
    function url: NSURL; cdecl;
  end;
  TASAuthorizationSingleSignOnProvider = class(TOCGenericImport<ASAuthorizationSingleSignOnProviderClass, ASAuthorizationSingleSignOnProvider>) end;

  ASAuthorizationProviderExtensionAuthorizationRequestHandler = interface(IObjectiveC)
    ['{69D44EA2-003D-48A0-8309-E4645A404CCB}']
    procedure beginAuthorizationWithRequest(request: ASAuthorizationProviderExtensionAuthorizationRequest); cdecl;
    procedure cancelAuthorizationWithRequest(request: ASAuthorizationProviderExtensionAuthorizationRequest); cdecl;
  end;

  ASAuthorizationProviderExtensionAuthorizationRequestClass = interface(NSObjectClass)
    ['{AFA47A53-1FFE-435E-88DC-3734BF1402FB}']
  end;

  ASAuthorizationProviderExtensionAuthorizationRequest = interface(NSObject)
    ['{F208C238-5B10-47A0-A6DC-DA041B0C5581}']
    function authorizationOptions: NSDictionary; cdecl;
    function callerAuditToken: NSData; cdecl;
    function callerBundleIdentifier: NSString; cdecl;
    function callerTeamIdentifier: NSString; cdecl;
    procedure cancel; cdecl;
    procedure complete; cdecl;
    procedure completeWithAuthorizationResult(authorizationResult: ASAuthorizationProviderExtensionAuthorizationResult); cdecl;
    procedure completeWithError(error: NSError); cdecl;
    procedure completeWithHTTPAuthorizationHeaders(httpAuthorizationHeaders: NSDictionary); cdecl;
    procedure completeWithHTTPResponse(httpResponse: NSHTTPURLResponse; httpBody: NSData); cdecl;
    procedure doNotHandle; cdecl;
    function extensionData: NSDictionary; cdecl;
    function httpBody: NSData; cdecl;
    function httpHeaders: NSDictionary; cdecl;
    function isCallerManaged: Boolean; cdecl;
    function isUserInterfaceEnabled: Boolean; cdecl;
    function localizedCallerDisplayName: NSString; cdecl;
    function loginManager: ASAuthorizationProviderExtensionLoginManager; cdecl;
    procedure presentAuthorizationViewControllerWithCompletion(completion: TASAuthorizationProviderExtensionAuthorizationRequestBlockMethod1); cdecl;
    function realm: NSString; cdecl;
    function requestedOperation: ASAuthorizationProviderAuthorizationOperation; cdecl;
    function url: NSURL; cdecl;
  end;
  TASAuthorizationProviderExtensionAuthorizationRequest = class(TOCGenericImport<ASAuthorizationProviderExtensionAuthorizationRequestClass, ASAuthorizationProviderExtensionAuthorizationRequest>) end;

  ASAuthorizationProviderExtensionAuthorizationResultClass = interface(NSObjectClass)
    ['{FC96E3DA-7FCA-47AD-A84E-E98426B6252D}']
  end;

  ASAuthorizationProviderExtensionAuthorizationResult = interface(NSObject)
    ['{522824B0-A953-4343-B2BD-71AECE4C8D19}']
    function httpAuthorizationHeaders: NSDictionary; cdecl;
    function httpBody: NSData; cdecl;
    function httpResponse: NSHTTPURLResponse; cdecl;
    function initWithHTTPAuthorizationHeaders(httpAuthorizationHeaders: NSDictionary): Pointer; cdecl;
    function initWithHTTPResponse(httpResponse: NSHTTPURLResponse; httpBody: NSData): Pointer; cdecl;
    function privateKeys: NSArray; cdecl;
    procedure setHttpAuthorizationHeaders(httpAuthorizationHeaders: NSDictionary); cdecl;
    procedure setHttpBody(httpBody: NSData); cdecl;
    procedure setHttpResponse(httpResponse: NSHTTPURLResponse); cdecl;
    procedure setPrivateKeys(privateKeys: NSArray); cdecl;
  end;
  TASAuthorizationProviderExtensionAuthorizationResult = class(TOCGenericImport<ASAuthorizationProviderExtensionAuthorizationResultClass, ASAuthorizationProviderExtensionAuthorizationResult>) end;

  ASAuthorizationPublicKeyCredentialRegistrationRequest = interface(IObjectiveC)
    ['{5629D8F9-3E3F-4F51-AE67-8AB0AF18459F}']
    function attestationPreference: ASAuthorizationPublicKeyCredentialAttestationKind; cdecl;
    function challenge: NSData; cdecl;
    function displayName: NSString; cdecl;
    function name: NSString; cdecl;
    function relyingPartyIdentifier: NSString; cdecl;
    procedure setAttestationPreference(attestationPreference: ASAuthorizationPublicKeyCredentialAttestationKind); cdecl;
    procedure setChallenge(challenge: NSData); cdecl;
    procedure setDisplayName(displayName: NSString); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setUserID(userID: NSData); cdecl;
    procedure setUserVerificationPreference(userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference); cdecl;
    function userID: NSData; cdecl;
    function userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference; cdecl;
  end;

  ASCredentialRequest = interface(IObjectiveC)
    ['{A49D199A-16C5-466F-8E7E-B2DC565D1EEC}']
    function &type: ASCredentialRequestType; cdecl;
    function credentialIdentity: Pointer; cdecl;
  end;

  ASPasskeyCredentialRequestParametersClass = interface(NSObjectClass)
    ['{0CD31154-5B96-44EB-91FF-5988D16B7F3F}']
  end;

  ASPasskeyCredentialRequestParameters = interface(NSObject)
    ['{580021A0-4537-4302-BBC3-5BCDDF0A377F}']
    function allowedCredentials: NSArray; cdecl;
    function clientDataHash: NSData; cdecl;
    function relyingPartyIdentifier: NSString; cdecl;
    function userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference; cdecl;
  end;
  TASPasskeyCredentialRequestParameters = class(TOCGenericImport<ASPasskeyCredentialRequestParametersClass, ASPasskeyCredentialRequestParameters>) end;

  // ***** Could not find class reference for declaration: @interface ASCredentialProviderViewController : ASViewController
  // (ObjCInterfaceDecl - ObjCInterface) (ASCredentialProviderViewController.h - 22:1 to 163:5)

  ASAccountAuthenticationModificationControllerDelegate = interface(IObjectiveC)
    ['{1556B32B-6F43-4720-AF8B-E5A37C310DAC}']
    procedure accountAuthenticationModificationController(controller: ASAccountAuthenticationModificationController; didSuccessfullyCompleteRequest: ASAccountAuthenticationModificationRequest; withUserInfo: NSDictionary); overload; cdecl;
    procedure accountAuthenticationModificationController(controller: ASAccountAuthenticationModificationController; didFailRequest: ASAccountAuthenticationModificationRequest; withError: NSError); overload; cdecl;
  end;

  ASAccountAuthenticationModificationControllerPresentationContextProviding = interface(IObjectiveC)
    ['{B86DDE08-6797-4A75-B2B4-648B0D027F9B}']
    function presentationAnchorForAccountAuthenticationModificationController(controller: ASAccountAuthenticationModificationController): ASPresentationAnchor; cdecl;
  end;

  ASAccountAuthenticationModificationControllerClass = interface(NSObjectClass)
    ['{A704A437-4D96-44BB-9F07-092C34D1F05B}']
  end;

  ASAccountAuthenticationModificationController = interface(NSObject)
    ['{A5DD9BC5-63F5-46BC-B2B2-86EF7711FCF6}']
    function delegate: Pointer; cdecl;
    procedure performRequest(request: ASAccountAuthenticationModificationRequest); cdecl;
    function presentationContextProvider: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPresentationContextProvider(presentationContextProvider: Pointer); cdecl;
  end;
  TASAccountAuthenticationModificationController = class(TOCGenericImport<ASAccountAuthenticationModificationControllerClass, ASAccountAuthenticationModificationController>) end;

  // ***** Could not find class reference for declaration: @interface ASAccountAuthenticationModificationExtensionContext : NSExtensionContext
  // (ObjCInterfaceDecl - ObjCInterface) (ASAccountAuthenticationModificationExtensionContext.h - 18:1 to 51:5)

  ASAccountAuthenticationModificationRequestClass = interface(NSObjectClass)
    ['{926E2A33-012E-4059-BC5E-237867374455}']
  end;

  ASAccountAuthenticationModificationRequest = interface(NSObject)
    ['{617F3925-FCA2-4FE6-A82D-37F9B3A1C151}']
  end;
  TASAccountAuthenticationModificationRequest = class(TOCGenericImport<ASAccountAuthenticationModificationRequestClass, ASAccountAuthenticationModificationRequest>) end;

  ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequestClass = interface(ASAccountAuthenticationModificationRequestClass)
    ['{0935CF36-95B5-4999-B33C-281AC932DE6D}']
  end;

  ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest = interface(ASAccountAuthenticationModificationRequest)
    ['{C7E52545-B6EB-405F-8717-F84A04173260}']
    function initWithUser(user: NSString; serviceIdentifier: ASCredentialServiceIdentifier; userInfo: NSDictionary): Pointer; cdecl;
    function serviceIdentifier: ASCredentialServiceIdentifier; cdecl;
    function user: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest = class(TOCGenericImport<ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequestClass, ASAccountAuthenticationModificationReplacePasswordWithSignInWithAppleRequest>) end;

  ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequestClass = interface(ASAccountAuthenticationModificationRequestClass)
    ['{E125177F-7602-4BDC-B6F3-5EE7C2B4BA6B}']
  end;

  ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest = interface(ASAccountAuthenticationModificationRequest)
    ['{423D94A2-1D30-4DEF-BB2E-300AB6D98C17}']
    function initWithUser(user: NSString; serviceIdentifier: ASCredentialServiceIdentifier; userInfo: NSDictionary): Pointer; cdecl;
    function serviceIdentifier: ASCredentialServiceIdentifier; cdecl;
    function user: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest = class(TOCGenericImport<ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequestClass, ASAccountAuthenticationModificationUpgradePasswordToStrongPasswordRequest>) end;

  // ***** Could not find class reference for declaration: @interface ASAccountAuthenticationModificationViewController : ASViewController  
  // (ObjCInterfaceDecl - ObjCInterface) (ASAccountAuthenticationModificationViewController.h - 17:1 to 90:5)

  ASAuthorizationAppleIDButtonClass = interface(NSControlClass)
    ['{1ED46EEB-0387-4B45-A1D8-97E88BB29FD1}']
    {class} function buttonWithType(&type: ASAuthorizationAppleIDButtonType; style: ASAuthorizationAppleIDButtonStyle): Pointer; cdecl;
  end;

  ASAuthorizationAppleIDButton = interface(NSControl)
    ['{446E49D3-0862-458D-8A8C-1FD47AD5A452}']
    function cornerRadius: CGFloat; cdecl;
    function initWithAuthorizationButtonType(&type: ASAuthorizationAppleIDButtonType; authorizationButtonStyle: ASAuthorizationAppleIDButtonStyle): Pointer; cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
  end;
  TASAuthorizationAppleIDButton = class(TOCGenericImport<ASAuthorizationAppleIDButtonClass, ASAuthorizationAppleIDButton>) end;

  ASPublicKeyCredential = interface(IObjectiveC)
    ['{AAD3ADD6-5A3E-4B75-B983-4ADFF709FC96}']
    function credentialID: NSData; cdecl;
    function rawClientDataJSON: NSData; cdecl;
  end;

  ASAuthorizationPublicKeyCredentialAssertion = interface(IObjectiveC)
    ['{4637E2AB-7E0C-45A0-B15D-9C184782047B}']
    function rawAuthenticatorData: NSData; cdecl;
    function signature: NSData; cdecl;
    function userID: NSData; cdecl;
  end;

  ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutputClass = interface(NSObjectClass)
    ['{1322E8A6-EF12-4D17-AEBF-C4D261AF6C30}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput = interface(NSObject)
    ['{E5A2D1E1-68B8-47A1-B7DC-10BA71F2580E}']
    function didWrite: Boolean; cdecl;
    function readData: NSData; cdecl;
  end;
  TASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput = class(TOCGenericImport<ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutputClass, ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput>) end;

  ASAuthorizationPlatformPublicKeyCredentialAssertionClass = interface(NSObjectClass)
    ['{8CB2F0E3-0E9C-4B37-B61D-F4A0659D1780}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialAssertion = interface(NSObject)
    ['{1AE19205-9760-4877-A098-3C74FDADD50A}']
    function attachment: ASAuthorizationPublicKeyCredentialAttachment; cdecl;
    function largeBlob: ASAuthorizationPublicKeyCredentialLargeBlobAssertionOutput; cdecl;
  end;
  TASAuthorizationPlatformPublicKeyCredentialAssertion = class(TOCGenericImport<ASAuthorizationPlatformPublicKeyCredentialAssertionClass, ASAuthorizationPlatformPublicKeyCredentialAssertion>) end;

  ASAuthorizationPublicKeyCredentialDescriptor = interface(IObjectiveC)
    ['{B384C2F2-8457-4DDC-8D12-9CE105015140}']
    function credentialID: NSData; cdecl;
    procedure setCredentialID(credentialID: NSData); cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialDescriptorClass = interface(NSObjectClass)
    ['{AE508B4E-68CE-4478-95C7-5EE29F0CF078}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialDescriptor = interface(NSObject)
    ['{24FC8B6B-CB1A-4BAD-BEEC-9CE05E438C1C}']
    function initWithCredentialID(credentialID: NSData): Pointer; cdecl;
  end;
  TASAuthorizationPlatformPublicKeyCredentialDescriptor = class(TOCGenericImport<ASAuthorizationPlatformPublicKeyCredentialDescriptorClass, ASAuthorizationPlatformPublicKeyCredentialDescriptor>) end;

  ASAuthorizationPublicKeyCredentialAssertionRequest = interface(IObjectiveC)
    ['{BD01D1F7-EBEF-46E9-B1A7-CE719E07276F}']
    function allowedCredentials: NSArray; cdecl;
    function challenge: NSData; cdecl;
    function relyingPartyIdentifier: NSString; cdecl;
    procedure setAllowedCredentials(allowedCredentials: NSArray); cdecl;
    procedure setChallenge(challenge: NSData); cdecl;
    procedure setRelyingPartyIdentifier(relyingPartyIdentifier: NSString); cdecl;
    procedure setUserVerificationPreference(userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference); cdecl;
    function userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference; cdecl;
  end;

  ASAuthorizationPublicKeyCredentialLargeBlobAssertionInputClass = interface(NSObjectClass)
    ['{0E9BCC42-855C-4435-A20E-3E4933840E13}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput = interface(NSObject)
    ['{5F1B502D-1BAB-4D93-AD2A-E9DEECE280B4}']
    function dataToWrite: NSData; cdecl;
    function initWithOperation(operation: ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation): Pointer; cdecl;
    function operation: ASAuthorizationPublicKeyCredentialLargeBlobAssertionOperation; cdecl;
    procedure setDataToWrite(dataToWrite: NSData); cdecl;
  end;
  TASAuthorizationPublicKeyCredentialLargeBlobAssertionInput = class(TOCGenericImport<ASAuthorizationPublicKeyCredentialLargeBlobAssertionInputClass, ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput>) end;

  ASAuthorizationWebBrowserExternallyAuthenticatableRequest = interface(IObjectiveC)
    ['{C28E6D60-9236-4224-BB52-1F3AC4CF404E}']
    function authenticatedContext: LAContext; cdecl;
    procedure setAuthenticatedContext(authenticatedContext: LAContext); cdecl;
  end;

  ASAuthorizationWebBrowserPlatformPublicKeyCredentialAssertionRequest = interface(IObjectiveC)
    ['{0F455036-4894-4735-84F3-A61E79AB5224}']
    function clientData: ASPublicKeyCredentialClientData; cdecl;
    procedure setShouldShowHybridTransport(shouldShowHybridTransport: Boolean); cdecl;
    function shouldShowHybridTransport: Boolean; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialAssertionRequestClass = interface(ASAuthorizationRequestClass)
    ['{C43D2015-4931-4D66-8937-A1212576B1CB}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialAssertionRequest = interface(ASAuthorizationRequest)
    ['{A722054B-C9B9-449E-81A4-B469CDF04CEE}']
    function allowedCredentials: NSArray; cdecl;
    function largeBlob: ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput; cdecl;
    procedure setAllowedCredentials(allowedCredentials: NSArray); cdecl;
    procedure setLargeBlob(largeBlob: ASAuthorizationPublicKeyCredentialLargeBlobAssertionInput); cdecl;
  end;
  TASAuthorizationPlatformPublicKeyCredentialAssertionRequest = class(TOCGenericImport<ASAuthorizationPlatformPublicKeyCredentialAssertionRequestClass, ASAuthorizationPlatformPublicKeyCredentialAssertionRequest>) end;

  ASAuthorizationWebBrowserPlatformPublicKeyCredentialProvider = interface(IObjectiveC)
    ['{39C96650-9BCC-45CA-98F1-46C10CD8DA62}']
    function createCredentialAssertionRequestWithClientData(clientData: ASPublicKeyCredentialClientData): ASAuthorizationPlatformPublicKeyCredentialAssertionRequest; cdecl;
    function createCredentialRegistrationRequestWithClientData(clientData: ASPublicKeyCredentialClientData; name: NSString; userID: NSData): ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialProviderClass = interface(NSObjectClass)
    ['{9F84BF47-2286-4ACE-B56E-3A6F218F94E9}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialProvider = interface(NSObject)
    ['{5F17CECC-4EE4-4F65-B0D2-4E3C9BD03CA5}']
    function createCredentialAssertionRequestWithChallenge(challenge: NSData): ASAuthorizationPlatformPublicKeyCredentialAssertionRequest; cdecl;
    function createCredentialRegistrationRequestWithChallenge(challenge: NSData; name: NSString; userID: NSData): ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest; cdecl;
    function initWithRelyingPartyIdentifier(relyingPartyIdentifier: NSString): Pointer; cdecl;
    function relyingPartyIdentifier: NSString; cdecl;
  end;
  TASAuthorizationPlatformPublicKeyCredentialProvider = class(TOCGenericImport<ASAuthorizationPlatformPublicKeyCredentialProviderClass, ASAuthorizationPlatformPublicKeyCredentialProvider>) end;

  ASAuthorizationPublicKeyCredentialRegistration = interface(IObjectiveC)
    ['{7089A644-1F0C-4E3A-BAC5-31FA28D4771C}']
    function rawAttestationObject: NSData; cdecl;
  end;

  ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutputClass = interface(NSObjectClass)
    ['{ACB757E8-90FC-4523-A678-18DC8D8E63B4}']
  end;

  ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput = interface(NSObject)
    ['{5849A9E1-7471-4BF2-B882-1EBE80D4B7F5}']
    function isSupported: Boolean; cdecl;
  end;
  TASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput = class(TOCGenericImport<ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutputClass, ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput>) end;

  ASAuthorizationPlatformPublicKeyCredentialRegistrationClass = interface(NSObjectClass)
    ['{84971D69-AA27-4D71-A250-20436C464216}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialRegistration = interface(NSObject)
    ['{E24701C3-4D5D-467C-9910-4D77B7B01D26}']
    function attachment: ASAuthorizationPublicKeyCredentialAttachment; cdecl;
    function largeBlob: ASAuthorizationPublicKeyCredentialLargeBlobRegistrationOutput; cdecl;
  end;
  TASAuthorizationPlatformPublicKeyCredentialRegistration = class(TOCGenericImport<ASAuthorizationPlatformPublicKeyCredentialRegistrationClass, ASAuthorizationPlatformPublicKeyCredentialRegistration>) end;

  ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInputClass = interface(NSObjectClass)
    ['{556659B6-56C1-4A1B-9E56-419F6EA80589}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput = interface(NSObject)
    ['{52013101-CDDE-4D72-8AD4-0707E4F489EB}']
    function initWithSupportRequirement(requirement: ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement): Pointer; cdecl;
    procedure setSupportRequirement(supportRequirement: ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement); cdecl;
    function supportRequirement: ASAuthorizationPublicKeyCredentialLargeBlobSupportRequirement; cdecl;
  end;
  TASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput = class(TOCGenericImport<ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInputClass, ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput>) end;

  ASAuthorizationWebBrowserPlatformPublicKeyCredentialRegistrationRequest = interface(IObjectiveC)
    ['{FD34FC43-75A1-47B1-9CC1-0ACB0DAE9FB1}']
    function clientData: ASPublicKeyCredentialClientData; cdecl;
    function excludedCredentials: NSArray; cdecl;
    procedure setExcludedCredentials(excludedCredentials: NSArray); cdecl;
    procedure setShouldShowHybridTransport(shouldShowHybridTransport: Boolean); cdecl;
    function shouldShowHybridTransport: Boolean; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestClass = interface(ASAuthorizationRequestClass)
    ['{56F9451E-5EF7-495E-971E-102E79A1E949}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest = interface(ASAuthorizationRequest)
    ['{C06F0759-5F68-435D-9F90-6C9221C90639}']
    function largeBlob: ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput; cdecl;
    procedure setLargeBlob(largeBlob: ASAuthorizationPublicKeyCredentialLargeBlobRegistrationInput); cdecl;
  end;
  TASAuthorizationPlatformPublicKeyCredentialRegistrationRequest = class(TOCGenericImport<ASAuthorizationPlatformPublicKeyCredentialRegistrationRequestClass, ASAuthorizationPlatformPublicKeyCredentialRegistrationRequest>) end;

  ASAuthorizationPublicKeyCredentialParametersClass = interface(NSObjectClass)
    ['{1984AA8D-2F96-4513-A98A-6DA7B1145C16}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationPublicKeyCredentialParameters = interface(NSObject)
    ['{4C2D0F1B-AD84-42EC-A995-295E921F5417}']
    function algorithm: ASCOSEAlgorithmIdentifier; cdecl;
    function initWithAlgorithm(algorithm: ASCOSEAlgorithmIdentifier): Pointer; cdecl;
  end;
  TASAuthorizationPublicKeyCredentialParameters = class(TOCGenericImport<ASAuthorizationPublicKeyCredentialParametersClass, ASAuthorizationPublicKeyCredentialParameters>) end;

  ASAuthorizationSecurityKeyPublicKeyCredentialAssertionClass = interface(NSObjectClass)
    ['{3A879982-9381-4606-AD04-B9DD1F84E9D2}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialAssertion = interface(NSObject)
    ['{2414DB15-91C5-4321-8410-E11D5BE51A6A}']
  end;
  TASAuthorizationSecurityKeyPublicKeyCredentialAssertion = class(TOCGenericImport<ASAuthorizationSecurityKeyPublicKeyCredentialAssertionClass, ASAuthorizationSecurityKeyPublicKeyCredentialAssertion>) end;

  ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorClass = interface(NSObjectClass)
    ['{35DB9663-759F-48B3-ACA1-16DF7FF739FE}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor = interface(NSObject)
    ['{84A44B07-D00A-4535-94ED-07046A688C13}']
    function initWithCredentialID(credentialID: NSData; transports: NSArray): Pointer; cdecl;
    procedure setTransports(transports: NSArray); cdecl;
    function transports: NSArray; cdecl;
  end;
  TASAuthorizationSecurityKeyPublicKeyCredentialDescriptor = class(TOCGenericImport<ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorClass, ASAuthorizationSecurityKeyPublicKeyCredentialDescriptor>) end;

  ASAuthorizationWebBrowserSecurityKeyPublicKeyCredentialAssertionRequest = interface(IObjectiveC)
    ['{02A6CE87-0EB2-4161-ABDE-399873DFCFB6}']
    function clientData: ASPublicKeyCredentialClientData; cdecl;
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequestClass = interface(ASAuthorizationRequestClass)
    ['{B04D40A7-394C-4D13-A2DB-F32D8A3C2394}']
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest = interface(ASAuthorizationRequest)
    ['{2FA268D6-DA9F-42E2-AA2C-7C38E78CE78B}']
    function allowedCredentials: NSArray; cdecl;
    procedure setAllowedCredentials(allowedCredentials: NSArray); cdecl;
  end;
  TASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest = class(TOCGenericImport<ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequestClass, ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest>) end;

  ASAuthorizationWebBrowserSecurityKeyPublicKeyCredentialProvider = interface(IObjectiveC)
    ['{A1A5E828-FE45-4DBE-94C6-392864DF10B5}']
    function createCredentialAssertionRequestWithClientData(clientData: ASPublicKeyCredentialClientData): ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest; cdecl;
    function createCredentialRegistrationRequestWithClientData(clientData: ASPublicKeyCredentialClientData; displayName: NSString; name: NSString; userID: NSData): ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest; cdecl;
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialProviderClass = interface(NSObjectClass)
    ['{9ADB4A0E-8E40-43AA-993D-F849505F2584}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialProvider = interface(NSObject)
    ['{5411E9F9-DF08-4F80-A179-BB9B606E9509}']
    function createCredentialAssertionRequestWithChallenge(challenge: NSData): ASAuthorizationSecurityKeyPublicKeyCredentialAssertionRequest; cdecl;
    function createCredentialRegistrationRequestWithChallenge(challenge: NSData; displayName: NSString; name: NSString; userID: NSData): ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest; cdecl;
    function initWithRelyingPartyIdentifier(relyingPartyIdentifier: NSString): Pointer; cdecl;
    function relyingPartyIdentifier: NSString; cdecl;
  end;
  TASAuthorizationSecurityKeyPublicKeyCredentialProvider = class(TOCGenericImport<ASAuthorizationSecurityKeyPublicKeyCredentialProviderClass, ASAuthorizationSecurityKeyPublicKeyCredentialProvider>) end;

  ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationClass = interface(NSObjectClass)
    ['{F676CB54-0065-478F-8D5B-DF3716C2E1A8}']
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialRegistration = interface(NSObject)
    ['{D00D6112-0D69-42F5-B2AD-3B58AF7ADDC0}']
  end;
  TASAuthorizationSecurityKeyPublicKeyCredentialRegistration = class(TOCGenericImport<ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationClass, ASAuthorizationSecurityKeyPublicKeyCredentialRegistration>) end;

  ASAuthorizationWebBrowserSecurityKeyPublicKeyCredentialRegistrationRequest = interface(IObjectiveC)
    ['{C8CFDE6B-AC51-42AC-8B86-5D4C7FB4114D}']
    function clientData: ASPublicKeyCredentialClientData; cdecl;
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequestClass = interface(ASAuthorizationRequestClass)
    ['{F524146E-C0B8-43C6-89FF-7574FD1E687B}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest = interface(ASAuthorizationRequest)
    ['{156B8BCF-0B23-489B-8DEB-EDD531676169}']
    function credentialParameters: NSArray; cdecl;
    function excludedCredentials: NSArray; cdecl;
    function residentKeyPreference: ASAuthorizationPublicKeyCredentialResidentKeyPreference; cdecl;
    procedure setCredentialParameters(credentialParameters: NSArray); cdecl;
    procedure setExcludedCredentials(excludedCredentials: NSArray); cdecl;
    procedure setResidentKeyPreference(residentKeyPreference: ASAuthorizationPublicKeyCredentialResidentKeyPreference); cdecl;
  end;
  TASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest = class(TOCGenericImport<ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequestClass, ASAuthorizationSecurityKeyPublicKeyCredentialRegistrationRequest>) end;

  ASAuthorizationWebBrowserPlatformPublicKeyCredentialClass = interface(NSObjectClass)
    ['{3E6245CC-DA9D-40A6-899D-CF4E16928F2F}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationWebBrowserPlatformPublicKeyCredential = interface(NSObject)
    ['{64323771-57CA-43BD-AF3E-A073F2F5F7C1}']
    function credentialID: NSData; cdecl;
    function customTitle: NSString; cdecl;
    function name: NSString; cdecl;
    function providerName: NSString; cdecl;
    function relyingParty: NSString; cdecl;
    function userHandle: NSData; cdecl;
  end;
  TASAuthorizationWebBrowserPlatformPublicKeyCredential = class(TOCGenericImport<ASAuthorizationWebBrowserPlatformPublicKeyCredentialClass, ASAuthorizationWebBrowserPlatformPublicKeyCredential>) end;

  ASAuthorizationWebBrowserPublicKeyCredentialManagerClass = interface(NSObjectClass)
    ['{2544134E-4632-4839-8A95-9538C853C938}']
  end;

  ASAuthorizationWebBrowserPublicKeyCredentialManager = interface(NSObject)
    ['{6072D7CA-9835-4A18-B907-2DD3E914F70E}']
    function authorizationStateForPlatformCredentials: ASAuthorizationWebBrowserPublicKeyCredentialManagerAuthorizationState; cdecl;
    procedure platformCredentialsForRelyingParty(relyingParty: NSString; completionHandler: TASAuthorizationWebBrowserPublicKeyCredentialManagerBlockMethod2); cdecl;
    procedure requestAuthorizationForPublicKeyCredentials(completionHandler: TASAuthorizationWebBrowserPublicKeyCredentialManagerBlockMethod1); cdecl;
  end;
  TASAuthorizationWebBrowserPublicKeyCredentialManager = class(TOCGenericImport<ASAuthorizationWebBrowserPublicKeyCredentialManagerClass, ASAuthorizationWebBrowserPublicKeyCredentialManager>) end;

  ASAuthorizationProviderExtensionKerberosMappingClass = interface(NSObjectClass)
    ['{5CD3AD12-376A-4233-A526-FB9A0867CF45}']
  end;

  ASAuthorizationProviderExtensionKerberosMapping = interface(NSObject)
    ['{9C946130-FC88-4D54-A10E-B52B17B8B2E9}']
    function clientNameKeyName: NSString; cdecl;
    function encryptionKeyTypeKeyName: NSString; cdecl;
    function messageBufferKeyName: NSString; cdecl;
    function realmKeyName: NSString; cdecl;
    function serviceNameKeyName: NSString; cdecl;
    function sessionKeyKeyName: NSString; cdecl;
    procedure setClientNameKeyName(clientNameKeyName: NSString); cdecl;
    procedure setEncryptionKeyTypeKeyName(encryptionKeyTypeKeyName: NSString); cdecl;
    procedure setMessageBufferKeyName(messageBufferKeyName: NSString); cdecl;
    procedure setRealmKeyName(realmKeyName: NSString); cdecl;
    procedure setServiceNameKeyName(serviceNameKeyName: NSString); cdecl;
    procedure setSessionKeyKeyName(sessionKeyKeyName: NSString); cdecl;
    procedure setTicketKeyPath(ticketKeyPath: NSString); cdecl;
    function ticketKeyPath: NSString; cdecl;
  end;
  TASAuthorizationProviderExtensionKerberosMapping = class(TOCGenericImport<ASAuthorizationProviderExtensionKerberosMappingClass, ASAuthorizationProviderExtensionKerberosMapping>) end;

  ASAuthorizationProviderExtensionLoginConfigurationClass = interface(NSObjectClass)
    ['{2248A8D9-EED7-4241-A001-32A79859F4C9}']
    {class} procedure configurationWithOpenIDConfigurationURL(openIDConfigurationURL: NSURL; clientID: NSString; issuer: NSString; completion: TASAuthorizationProviderExtensionLoginConfigurationBlockMethod1); cdecl;
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationProviderExtensionLoginConfiguration = interface(NSObject)
    ['{78A6A2BD-5289-4BA5-A3D7-C749CDEB5D04}']
    function accountDisplayName: NSString; cdecl;
    function additionalAuthorizationScopes: NSString; cdecl;
    function additionalScopes: NSString; cdecl;
    function audience: NSString; cdecl;
    function clientID: NSString; cdecl;
    function customFederationUserPreauthenticationRequestValues: NSArray; cdecl;
    function customKeyExchangeRequestValues: NSArray; cdecl;
    function customKeyRequestValues: NSArray; cdecl;
    function customLoginRequestValues: NSArray; cdecl;
    function customNonceRequestValues: NSArray; cdecl;
    function customRefreshRequestValues: NSArray; cdecl;
    function customRequestJWTParameterName: NSString; cdecl;
    function deviceContext: NSData; cdecl;
    function federationMEXURL: NSURL; cdecl;
    function federationMEXURLKeypath: NSString; cdecl;
    function federationPredicate: NSString; cdecl;
    function federationRequestURN: NSString; cdecl;
    function federationType: ASAuthorizationProviderExtensionFederationType; cdecl;
    function federationUserPreauthenticationURL: NSURL; cdecl;
    function groupRequestClaimName: NSString; cdecl;
    function groupResponseClaimName: NSString; cdecl;
    function includePreviousRefreshTokenInLoginRequest: Boolean; cdecl;
    function initWithClientID(clientID: NSString; issuer: NSString; tokenEndpointURL: NSURL; jwksEndpointURL: NSURL; audience: NSString): Pointer; cdecl;
    function invalidCredentialPredicate: NSString; cdecl;
    function issuer: NSString; cdecl;
    function jwksEndpointURL: NSURL; cdecl;
    function jwksTrustedRootCertificates: NSArray; cdecl;
    function kerberosTicketMappings: NSArray; cdecl;
    function keyEndpointURL: NSURL; cdecl;
    function loginRequestEncryptionAPVPrefix: NSData; cdecl;
    function loginRequestEncryptionPublicKey: SecKeyRef; cdecl;
    function nonceEndpointURL: NSURL; cdecl;
    function nonceResponseKeypath: NSString; cdecl;
    function previousRefreshTokenClaimName: NSString; cdecl;
    function refreshEndpointURL: NSURL; cdecl;
    function serverNonceClaimName: NSString; cdecl;
    procedure setAccountDisplayName(accountDisplayName: NSString); cdecl;
    procedure setAdditionalAuthorizationScopes(additionalAuthorizationScopes: NSString); cdecl;
    procedure setAdditionalScopes(additionalScopes: NSString); cdecl;
    procedure setAudience(audience: NSString); cdecl;
    function setCustomAssertionRequestBodyClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    function setCustomAssertionRequestHeaderClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    procedure setCustomFederationUserPreauthenticationRequestValues(customFederationUserPreauthenticationRequestValues: NSArray); cdecl;
    function setCustomKeyExchangeRequestBodyClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    function setCustomKeyExchangeRequestHeaderClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    procedure setCustomKeyExchangeRequestValues(customKeyExchangeRequestValues: NSArray); cdecl;
    function setCustomKeyRequestBodyClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    function setCustomKeyRequestHeaderClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    procedure setCustomKeyRequestValues(customKeyRequestValues: NSArray); cdecl;
    function setCustomLoginRequestBodyClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    function setCustomLoginRequestHeaderClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    procedure setCustomLoginRequestValues(customLoginRequestValues: NSArray); cdecl;
    procedure setCustomNonceRequestValues(customNonceRequestValues: NSArray); cdecl;
    function setCustomRefreshRequestBodyClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    function setCustomRefreshRequestHeaderClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    procedure setCustomRefreshRequestValues(customRefreshRequestValues: NSArray); cdecl;
    procedure setCustomRequestJWTParameterName(customRequestJWTParameterName: NSString); cdecl;
    procedure setDeviceContext(deviceContext: NSData); cdecl;
    procedure setFederationMEXURL(federationMEXURL: NSURL); cdecl;
    procedure setFederationMEXURLKeypath(federationMEXURLKeypath: NSString); cdecl;
    procedure setFederationPredicate(federationPredicate: NSString); cdecl;
    procedure setFederationRequestURN(federationRequestURN: NSString); cdecl;
    procedure setFederationType(federationType: ASAuthorizationProviderExtensionFederationType); cdecl;
    procedure setFederationUserPreauthenticationURL(federationUserPreauthenticationURL: NSURL); cdecl;
    procedure setGroupRequestClaimName(groupRequestClaimName: NSString); cdecl;
    procedure setGroupResponseClaimName(groupResponseClaimName: NSString); cdecl;
    procedure setIncludePreviousRefreshTokenInLoginRequest(includePreviousRefreshTokenInLoginRequest: Boolean); cdecl;
    procedure setInvalidCredentialPredicate(invalidCredentialPredicate: NSString); cdecl;
    procedure setJwksEndpointURL(jwksEndpointURL: NSURL); cdecl;
    procedure setJwksTrustedRootCertificates(jwksTrustedRootCertificates: NSArray); cdecl;
    procedure setKerberosTicketMappings(kerberosTicketMappings: NSArray); cdecl;
    procedure setKeyEndpointURL(keyEndpointURL: NSURL); cdecl;
    procedure setLoginRequestEncryptionAPVPrefix(loginRequestEncryptionAPVPrefix: NSData); cdecl;
    procedure setLoginRequestEncryptionPublicKey(loginRequestEncryptionPublicKey: SecKeyRef); cdecl;
    procedure setNonceEndpointURL(nonceEndpointURL: NSURL); cdecl;
    procedure setNonceResponseKeypath(nonceResponseKeypath: NSString); cdecl;
    procedure setPreviousRefreshTokenClaimName(previousRefreshTokenClaimName: NSString); cdecl;
    procedure setRefreshEndpointURL(refreshEndpointURL: NSURL); cdecl;
    procedure setServerNonceClaimName(serverNonceClaimName: NSString); cdecl;
    procedure setTokenEndpointURL(tokenEndpointURL: NSURL); cdecl;
    procedure setUniqueIdentifierClaimName(uniqueIdentifierClaimName: NSString); cdecl;
    procedure setUserSecureEnclaveKeyBiometricPolicy(userSecureEnclaveKeyBiometricPolicy: ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy); cdecl;
    function tokenEndpointURL: NSURL; cdecl;
    function uniqueIdentifierClaimName: NSString; cdecl;
    function userSecureEnclaveKeyBiometricPolicy: ASAuthorizationProviderExtensionUserSecureEnclaveKeyBiometricPolicy; cdecl;
  end;
  TASAuthorizationProviderExtensionLoginConfiguration = class(TOCGenericImport<ASAuthorizationProviderExtensionLoginConfigurationClass, ASAuthorizationProviderExtensionLoginConfiguration>) end;

  ASAuthorizationProviderExtensionLoginManagerClass = interface(NSObjectClass)
    ['{31F30D9D-E4A2-40FF-B0A2-12BEBA22855A}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationProviderExtensionLoginManager = interface(NSObject)
    ['{3147BD7E-1C58-47B6-90D7-2DE42D32618D}']
    function copyIdentityForKeyType(keyType: ASAuthorizationProviderExtensionKeyType): SecIdentityRef; cdecl;
    function copyKeyForKeyType(keyType: ASAuthorizationProviderExtensionKeyType): SecKeyRef; cdecl;
    procedure decryptionKeysNeedRepair; cdecl;
    procedure deviceRegistrationsNeedsRepair; cdecl;
    function extensionData: NSDictionary; cdecl;
    function isDeviceRegistered: Boolean; cdecl;
    function isUserRegistered: Boolean; cdecl;
    function loginConfiguration: ASAuthorizationProviderExtensionLoginConfiguration; cdecl;
    function loginUserName: NSString; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("userLoginConfiguration.loginUserName", macos(13.0, 14.0))
    procedure presentRegistrationViewControllerWithCompletion(completion: TASAuthorizationProviderExtensionLoginManagerBlockMethod1); cdecl;
    function registrationToken: NSString; cdecl;
    procedure resetDeviceKeys; cdecl;
    procedure resetKeys; cdecl;
    procedure resetUserSecureEnclaveKey; cdecl;
    procedure saveCertificate(certificate: SecCertificateRef; keyType: ASAuthorizationProviderExtensionKeyType); cdecl;
    function saveLoginConfiguration(loginConfiguration: ASAuthorizationProviderExtensionLoginConfiguration; error: PPointer): Boolean; cdecl;
    function saveUserLoginConfiguration(userLoginConfiguration: ASAuthorizationProviderExtensionUserLoginConfiguration; error: PPointer): Boolean; cdecl;
    procedure setLoginUserName(loginUserName: NSString); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("userLoginConfiguration.loginUserName", macos(13.0, 14.0))
    procedure setSsoTokens(ssoTokens: NSDictionary); cdecl;
    function ssoTokens: NSDictionary; cdecl;
    function userLoginConfiguration: ASAuthorizationProviderExtensionUserLoginConfiguration; cdecl;
    procedure userNeedsReauthenticationWithCompletion(completion: TASAuthorizationProviderExtensionLoginManagerBlockMethod1); cdecl;
    procedure userRegistrationsNeedsRepair; cdecl;
  end;
  TASAuthorizationProviderExtensionLoginManager = class(TOCGenericImport<ASAuthorizationProviderExtensionLoginManagerClass, ASAuthorizationProviderExtensionLoginManager>) end;

  ASAuthorizationProviderExtensionRegistrationHandler = interface(IObjectiveC)
    ['{7CD3752F-06A0-4655-A885-C431C7D87EAC}']
    procedure beginDeviceRegistrationUsingLoginManager(loginManager: ASAuthorizationProviderExtensionLoginManager; options: ASAuthorizationProviderExtensionRequestOptions; completion: Pointer); cdecl;
    procedure beginUserRegistrationUsingLoginManager(loginManager: ASAuthorizationProviderExtensionLoginManager; userName: NSString; authenticationMethod: ASAuthorizationProviderExtensionAuthenticationMethod; options: ASAuthorizationProviderExtensionRequestOptions; completion: Pointer); cdecl;
    function protocolVersion: ASAuthorizationProviderExtensionPlatformSSOProtocolVersion; cdecl;
    procedure registrationDidCancel; cdecl;
    procedure registrationDidComplete; cdecl;
    function supportedGrantTypes: ASAuthorizationProviderExtensionSupportedGrantTypes; cdecl;
  end;

  ASAuthorizationProviderExtensionUserLoginConfigurationClass = interface(NSObjectClass)
    ['{691C35AD-2E02-4739-AD96-337ACB7AF7E4}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationProviderExtensionUserLoginConfiguration = interface(NSObject)
    ['{6FCE3D36-AB05-483A-BB02-D0B98049EDBB}']
    function initWithLoginUserName(loginUserName: NSString): Pointer; cdecl;
    function loginUserName: NSString; cdecl;
    function setCustomAssertionRequestBodyClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    function setCustomAssertionRequestHeaderClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    function setCustomLoginRequestBodyClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    function setCustomLoginRequestHeaderClaims(claims: NSDictionary; returningError: PPointer): Boolean; cdecl;
    procedure setLoginUserName(loginUserName: NSString); cdecl;
  end;
  TASAuthorizationProviderExtensionUserLoginConfiguration = class(TOCGenericImport<ASAuthorizationProviderExtensionUserLoginConfigurationClass, ASAuthorizationProviderExtensionUserLoginConfiguration>) end;

  ASPasskeyAssertionCredentialClass = interface(NSObjectClass)
    ['{4B08808F-F6CA-4974-8146-8F0E01EB1C6E}']
    {class} function credentialWithUserHandle(userHandle: NSData; relyingParty: NSString; signature: NSData; clientDataHash: NSData; authenticatorData: NSData; credentialID: NSData): Pointer; cdecl;
  end;

  ASPasskeyAssertionCredential = interface(NSObject)
    ['{EF8B9519-DD30-4C2B-8E46-788DFDB113BD}']
    function authenticatorData: NSData; cdecl;
    function clientDataHash: NSData; cdecl;
    function credentialID: NSData; cdecl;
    function initWithUserHandle(userHandle: NSData; relyingParty: NSString; signature: NSData; clientDataHash: NSData; authenticatorData: NSData; credentialID: NSData): Pointer; cdecl;
    function relyingParty: NSString; cdecl;
    function signature: NSData; cdecl;
    function userHandle: NSData; cdecl;
  end;
  TASPasskeyAssertionCredential = class(TOCGenericImport<ASPasskeyAssertionCredentialClass, ASPasskeyAssertionCredential>) end;

  ASPasskeyCredentialIdentityClass = interface(NSObjectClass)
    ['{0B54273F-8808-4DFD-960C-E293212B6A75}']
    {class} function identityWithRelyingPartyIdentifier(relyingPartyIdentifier: NSString; userName: NSString; credentialID: NSData; userHandle: NSData; recordIdentifier: NSString): Pointer; cdecl;
  end;

  ASPasskeyCredentialIdentity = interface(NSObject)
    ['{7E7469B9-E67C-49D1-A295-3C08D3C2F0CC}']
    function credentialID: NSData; cdecl;
    function initWithRelyingPartyIdentifier(relyingPartyIdentifier: NSString; userName: NSString; credentialID: NSData; userHandle: NSData; recordIdentifier: NSString): Pointer; cdecl;
    function rank: NSInteger; cdecl;
    function recordIdentifier: NSString; cdecl;
    function relyingPartyIdentifier: NSString; cdecl;
    procedure setRank(rank: NSInteger); cdecl;
    function userHandle: NSData; cdecl;
    function userName: NSString; cdecl;
  end;
  TASPasskeyCredentialIdentity = class(TOCGenericImport<ASPasskeyCredentialIdentityClass, ASPasskeyCredentialIdentity>) end;

  ASPasskeyCredentialRequestClass = interface(NSObjectClass)
    ['{66543282-ABF7-4F79-B636-2D515E9B023F}']
    {class} function requestWithCredentialIdentity(credentialIdentity: ASPasskeyCredentialIdentity; clientDataHash: NSData; userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference; supportedAlgorithms: NSArray): Pointer; cdecl;
  end;

  ASPasskeyCredentialRequest = interface(NSObject)
    ['{006C784D-5171-4B74-A4D5-CE7E6978300B}']
    function clientDataHash: NSData; cdecl;
    function initWithCredentialIdentity(credentialIdentity: ASPasskeyCredentialIdentity; clientDataHash: NSData; userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference; supportedAlgorithms: NSArray): Pointer; cdecl;
    procedure setUserVerificationPreference(userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference); cdecl;
    function supportedAlgorithms: NSArray; cdecl;
    function userVerificationPreference: ASAuthorizationPublicKeyCredentialUserVerificationPreference; cdecl;
  end;
  TASPasskeyCredentialRequest = class(TOCGenericImport<ASPasskeyCredentialRequestClass, ASPasskeyCredentialRequest>) end;

  ASPasskeyRegistrationCredentialClass = interface(NSObjectClass)
    ['{0EC933D0-1D7E-4271-A51A-D416330B1A85}']
    {class} function credentialWithRelyingParty(relyingParty: NSString; clientDataHash: NSData; credentialID: NSData; attestationObject: NSData): Pointer; cdecl;
  end;

  ASPasskeyRegistrationCredential = interface(NSObject)
    ['{BB627EDF-6D06-4AF8-B690-4B35E90BE44F}']
    function attestationObject: NSData; cdecl;
    function clientDataHash: NSData; cdecl;
    function credentialID: NSData; cdecl;
    function initWithRelyingParty(relyingParty: NSString; clientDataHash: NSData; credentialID: NSData; attestationObject: NSData): Pointer; cdecl;
    function relyingParty: NSString; cdecl;
  end;
  TASPasskeyRegistrationCredential = class(TOCGenericImport<ASPasskeyRegistrationCredentialClass, ASPasskeyRegistrationCredential>) end;

  ASPasswordCredentialRequestClass = interface(NSObjectClass)
    ['{2B6646F9-7E06-4415-94C1-0247F6915637}']
    {class} function requestWithCredentialIdentity(credentialIdentity: ASPasswordCredentialIdentity): Pointer; cdecl;
  end;

  ASPasswordCredentialRequest = interface(NSObject)
    ['{808DBF53-0451-4BEE-A2BE-358B7072EAF3}']
    function initWithCredentialIdentity(credentialIdentity: ASPasswordCredentialIdentity): Pointer; cdecl;
  end;
  TASPasswordCredentialRequest = class(TOCGenericImport<ASPasswordCredentialRequestClass, ASPasswordCredentialRequest>) end;

  ASPublicKeyCredentialClientDataClass = interface(NSObjectClass)
    ['{30F29509-33DE-49BC-9C2F-90A5A4832889}']
    {class} function new: Pointer; cdecl;
  end;

  ASPublicKeyCredentialClientData = interface(NSObject)
    ['{DB1500DF-0CAE-472F-85D5-6867C2DC0689}']
    function challenge: NSData; cdecl;
    function crossOrigin: ASPublicKeyCredentialClientDataCrossOriginValue; cdecl;
    function initWithChallenge(challenge: NSData; origin: NSString): Pointer; cdecl;
    function origin: NSString; cdecl;
    procedure setChallenge(challenge: NSData); cdecl;
    procedure setCrossOrigin(crossOrigin: ASPublicKeyCredentialClientDataCrossOriginValue); cdecl;
    procedure setOrigin(origin: NSString); cdecl;
    procedure setTopOrigin(topOrigin: NSString); cdecl;
    function topOrigin: NSString; cdecl;
  end;
  TASPublicKeyCredentialClientData = class(TOCGenericImport<ASPublicKeyCredentialClientDataClass, ASPublicKeyCredentialClientData>) end;

  ASSettingsHelperClass = interface(NSObjectClass)
    ['{C39E5106-2BDD-4298-B442-3E9E457FBD22}']
    {class} function new: Pointer; cdecl;
    {class} procedure openCredentialProviderAppSettingsWithCompletionHandler(completionHandler: TASSettingsHelperBlockMethod1); cdecl;
    {class} procedure openVerificationCodeAppSettingsWithCompletionHandler(completionHandler: TASSettingsHelperBlockMethod1); cdecl;
  end;

  ASSettingsHelper = interface(NSObject)
    ['{625F02AA-9495-4827-B282-9DE1E3E27A27}']
  end;
  TASSettingsHelper = class(TOCGenericImport<ASSettingsHelperClass, ASSettingsHelper>) end;

  ASWebAuthenticationSessionCallbackClass = interface(NSObjectClass)
    ['{90D44DD6-4584-428C-BC76-D21D7996FBF4}']
    {class} function callbackWithCustomScheme(customScheme: NSString): Pointer; cdecl;
    {class} function callbackWithHTTPSHost(host: NSString; path: NSString): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  ASWebAuthenticationSessionCallback = interface(NSObject)
    ['{4B6466A6-1EE8-4279-B62D-5C2BC54A9219}']
    function matchesURL(url: NSURL): Boolean; cdecl;
  end;
  TASWebAuthenticationSessionCallback = class(TOCGenericImport<ASWebAuthenticationSessionCallbackClass, ASWebAuthenticationSessionCallback>) end;

function ASWebAuthenticationSessionErrorDomain: NSErrorDomain;
function ASCredentialIdentityStoreErrorDomain: NSErrorDomain;
function ASExtensionErrorDomain: NSErrorDomain;
function ASExtensionLocalizedFailureReasonErrorKey: NSErrorUserInfoKey;
function ASAuthorizationScopeFullName: ASAuthorizationScope;
function ASAuthorizationScopeEmail: ASAuthorizationScope;
function ASAuthorizationOperationImplicit: ASAuthorizationOpenIDOperation;
function ASAuthorizationOperationLogin: ASAuthorizationOpenIDOperation;
function ASAuthorizationOperationRefresh: ASAuthorizationOpenIDOperation;
function ASAuthorizationOperationLogout: ASAuthorizationOpenIDOperation;
function ASAuthorizationAppleIDProviderCredentialRevokedNotification: NSNotificationName;
function ASAuthorizationCustomMethodVideoSubscriberAccount: ASAuthorizationCustomMethod;
function ASAuthorizationCustomMethodRestorePurchase: ASAuthorizationCustomMethod;
function ASAuthorizationCustomMethodOther: ASAuthorizationCustomMethod;
function ASAuthorizationErrorDomain: NSErrorDomain;
function ASAuthorizationProviderAuthorizationOperationConfigurationRemoved: ASAuthorizationProviderAuthorizationOperation;
function ASAuthorizationProviderAuthorizationOperationDirectRequest: ASAuthorizationProviderAuthorizationOperation;
function ASAuthorizationPublicKeyCredentialUserVerificationPreferencePreferred: ASAuthorizationPublicKeyCredentialUserVerificationPreference;
function ASAuthorizationPublicKeyCredentialUserVerificationPreferenceRequired: ASAuthorizationPublicKeyCredentialUserVerificationPreference;
function ASAuthorizationPublicKeyCredentialUserVerificationPreferenceDiscouraged: ASAuthorizationPublicKeyCredentialUserVerificationPreference;
function ASAuthorizationPublicKeyCredentialAttestationKindNone: ASAuthorizationPublicKeyCredentialAttestationKind;
function ASAuthorizationPublicKeyCredentialAttestationKindDirect: ASAuthorizationPublicKeyCredentialAttestationKind;
function ASAuthorizationPublicKeyCredentialAttestationKindIndirect: ASAuthorizationPublicKeyCredentialAttestationKind;
function ASAuthorizationPublicKeyCredentialAttestationKindEnterprise: ASAuthorizationPublicKeyCredentialAttestationKind;
function ASAuthorizationPublicKeyCredentialResidentKeyPreferenceDiscouraged: ASAuthorizationPublicKeyCredentialResidentKeyPreference;
function ASAuthorizationPublicKeyCredentialResidentKeyPreferencePreferred: ASAuthorizationPublicKeyCredentialResidentKeyPreference;
function ASAuthorizationPublicKeyCredentialResidentKeyPreferenceRequired: ASAuthorizationPublicKeyCredentialResidentKeyPreference;
function ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportUSB: ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransport;
function ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportNFC: ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransport;
function ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportBluetooth: ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransport;

const
  libAuthenticationServices = '/System/Library/Frameworks/AuthenticationServices.framework/AuthenticationServices';

function ASAuthorizationAllSupportedPublicKeyCredentialDescriptorTransports: NSArray; cdecl;
  external libAuthenticationServices name _PU + 'ASAuthorizationAllSupportedPublicKeyCredentialDescriptorTransports';

implementation

uses
  System.SysUtils;

var
  AuthenticationServicesModule: THandle;

function ASWebAuthenticationSessionErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASWebAuthenticationSessionErrorDomain');
end;

function ASCredentialIdentityStoreErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASCredentialIdentityStoreErrorDomain');
end;

function ASExtensionErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASExtensionErrorDomain');
end;

function ASExtensionLocalizedFailureReasonErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASExtensionLocalizedFailureReasonErrorKey');
end;

function ASAuthorizationScopeFullName: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationScopeFullName');
end;

function ASAuthorizationScopeEmail: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationScopeEmail');
end;

function ASAuthorizationOperationImplicit: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationOperationImplicit');
end;

function ASAuthorizationOperationLogin: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationOperationLogin');
end;

function ASAuthorizationOperationRefresh: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationOperationRefresh');
end;

function ASAuthorizationOperationLogout: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationOperationLogout');
end;

function ASAuthorizationAppleIDProviderCredentialRevokedNotification: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationAppleIDProviderCredentialRevokedNotification');
end;

function ASAuthorizationCustomMethodVideoSubscriberAccount: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationCustomMethodVideoSubscriberAccount');
end;

function ASAuthorizationCustomMethodRestorePurchase: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationCustomMethodRestorePurchase');
end;

function ASAuthorizationCustomMethodOther: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationCustomMethodOther');
end;

function ASAuthorizationErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationErrorDomain');
end;

function ASAuthorizationProviderAuthorizationOperationConfigurationRemoved: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationProviderAuthorizationOperationConfigurationRemoved');
end;

function ASAuthorizationProviderAuthorizationOperationDirectRequest: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationProviderAuthorizationOperationDirectRequest');
end;

function ASAuthorizationPublicKeyCredentialUserVerificationPreferencePreferred: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialUserVerificationPreferencePreferred');
end;

function ASAuthorizationPublicKeyCredentialUserVerificationPreferenceRequired: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialUserVerificationPreferenceRequired');
end;

function ASAuthorizationPublicKeyCredentialUserVerificationPreferenceDiscouraged: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialUserVerificationPreferenceDiscouraged');
end;

function ASAuthorizationPublicKeyCredentialAttestationKindNone: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialAttestationKindNone');
end;

function ASAuthorizationPublicKeyCredentialAttestationKindDirect: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialAttestationKindDirect');
end;

function ASAuthorizationPublicKeyCredentialAttestationKindIndirect: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialAttestationKindIndirect');
end;

function ASAuthorizationPublicKeyCredentialAttestationKindEnterprise: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialAttestationKindEnterprise');
end;

function ASAuthorizationPublicKeyCredentialResidentKeyPreferenceDiscouraged: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialResidentKeyPreferenceDiscouraged');
end;

function ASAuthorizationPublicKeyCredentialResidentKeyPreferencePreferred: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialResidentKeyPreferencePreferred');
end;

function ASAuthorizationPublicKeyCredentialResidentKeyPreferenceRequired: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationPublicKeyCredentialResidentKeyPreferenceRequired');
end;

function ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportUSB: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportUSB');
end;

function ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportNFC: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportNFC');
end;

function ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportBluetooth: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationSecurityKeyPublicKeyCredentialDescriptorTransportBluetooth');
end;

initialization
  AuthenticationServicesModule := LoadLibrary(libAuthenticationServices);

finalization
  if AuthenticationServicesModule <> 0 then
    FreeLibrary(AuthenticationServicesModule);

end.