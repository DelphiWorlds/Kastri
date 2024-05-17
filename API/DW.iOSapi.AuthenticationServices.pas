unit DW.iOSapi.AuthenticationServices;

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
  Macapi.ObjectiveC, iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit,
  DW.iOSapi.Foundation;

const
  ASAuthorizationAppleIDButtonTypeSignIn = 0;
  ASAuthorizationAppleIDButtonTypeContinue = 1;
  ASAuthorizationAppleIDButtonTypeSignUp = 2;
  ASAuthorizationAppleIDButtonTypeDefault = ASAuthorizationAppleIDButtonTypeSignIn;
  ASAuthorizationAppleIDButtonStyleWhite = 0;
  ASAuthorizationAppleIDButtonStyleWhiteOutline = 1;
  ASAuthorizationAppleIDButtonStyleBlack = 2;
  ASUserDetectionStatusUnsupported = 0;
  ASUserDetectionStatusUnknown = 1;
  ASUserDetectionStatusLikelyReal = 2;
  ASAuthorizationAppleIDProviderCredentialRevoked = 0;
  ASAuthorizationAppleIDProviderCredentialAuthorized = 1;
  ASAuthorizationAppleIDProviderCredentialNotFound = 2;
  ASAuthorizationAppleIDProviderCredentialTransferred = 3;
  ASAuthorizationErrorUnknown = 1000;
  ASAuthorizationErrorCanceled = 1001;
  ASAuthorizationErrorInvalidResponse = 1002;
  ASAuthorizationErrorNotHandled = 1003;
  ASAuthorizationErrorFailed = 1004;
  ASCredentialIdentityStoreErrorCodeInternalError = 0;
  ASCredentialIdentityStoreErrorCodeStoreDisabled = 1;
  ASCredentialIdentityStoreErrorCodeStoreBusy = 2;
  ASExtensionErrorCodeFailed = 0;
  ASExtensionErrorCodeUserCanceled = 1;
  ASExtensionErrorCodeUserInteractionRequired = 100;
  ASExtensionErrorCodeCredentialIdentityNotFound = 101;
  ASCredentialServiceIdentifierTypeDomain = 0;
  ASCredentialServiceIdentifierTypeURL = 1;
  ASWebAuthenticationSessionErrorCodeCanceledLogin = 1;
  ASWebAuthenticationSessionErrorCodePresentationContextNotProvided = 2;
  ASWebAuthenticationSessionErrorCodePresentationContextInvalid = 3;

type
  ASAuthorization = interface;
  ASAuthorizationAppleIDButton = interface;
  ASAuthorizationCredential = interface;
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
  ASAuthorizationProviderExtensionAuthorizationRequestHandler = interface;
  ASAuthorizationProviderExtensionAuthorizationRequest = interface;
  ASAuthorizationSingleSignOnCredential = interface;
  ASAuthorizationSingleSignOnRequest = interface;
  ASAuthorizationSingleSignOnProvider = interface;
  ASCredentialIdentityStore = interface;
  ASCredentialIdentityStoreState = interface;
  ASCredentialProviderExtensionContext = interface;
  ASCredentialServiceIdentifier = interface;
  ASPasswordCredentialIdentity = interface;
  ASCredentialProviderViewController = interface;
  ASPasswordCredential = interface;
  ASWebAuthenticationSession = interface;
  ASWebAuthenticationPresentationContextProviding = interface;
  ASWebAuthenticationSessionRequestDelegate = interface;
  ASWebAuthenticationSessionRequest = interface;
  ASWebAuthenticationSessionWebBrowserSessionHandling = interface;
  ASWebAuthenticationSessionWebBrowserSessionManager = interface;

  ASPresentationAnchor = UIWindow;
  ASAuthorizationScope = NSString;
  ASAuthorizationAppleIDButtonType = NSInteger;
  ASAuthorizationAppleIDButtonStyle = NSInteger;
  ASUserDetectionStatus = NSInteger;
  ASAuthorizationOpenIDOperation = NSString;
  ASAuthorizationAppleIDProviderCredentialState = NSInteger;
  ASAuthorizationError = NSInteger;
  ASAuthorizationProviderAuthorizationOperation = NSString;
  ASCredentialIdentityStoreErrorCode = NSInteger;
  ASExtensionErrorCode = NSInteger;
  ASCredentialServiceIdentifierType = NSInteger;
  ASWebAuthenticationSessionErrorCode = NSInteger;

  ASWebAuthenticationSessionCompletionHandler = procedure(callbackURL: NSURL; error: NSError) of object;
  TASAuthorizationAppleIDProviderBlockMethod1 = procedure(credentialState: ASAuthorizationAppleIDProviderCredentialState; error: NSError) of object;
  TASAuthorizationProviderExtensionAuthorizationRequestBlockMethod1 = procedure(success: Boolean; error: NSError) of object;
  TASCredentialIdentityStoreBlockMethod1 = procedure(state: ASCredentialIdentityStoreState) of object;
  TASCredentialIdentityStoreBlockMethod2 = procedure(success: Boolean; error: NSError) of object;
  TASCredentialProviderExtensionContextBlockMethod1 = procedure(expired: Boolean) of object;
  TASCredentialProviderExtensionContextBlockMethod2 = procedure(param1: Boolean) of object;

  ASAuthorizationClass = interface(NSObjectClass)
    ['{88D42D76-C71B-45F0-95EC-B3C9552F18AB}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorization = interface(NSObject)
    ['{F6FCFCAD-0396-422F-A97B-C15909C5CEBF}']
    function credential: Pointer; cdecl;
    function provider: Pointer; cdecl;
  end;
  TASAuthorization = class(TOCGenericImport<ASAuthorizationClass, ASAuthorization>) end;

  ASAuthorizationAppleIDButtonClass = interface(UIControlClass)
    ['{DD5ED5C0-8B1E-49EB-AD9F-FE704278C0A1}']
    [MethodName('buttonWithType:style:')]
    {class} function buttonWithType(&type: ASAuthorizationAppleIDButtonType; style: ASAuthorizationAppleIDButtonStyle): Pointer; cdecl;
  end;

  ASAuthorizationAppleIDButton = interface(UIControl)
    ['{725AF22A-821A-4A2D-B039-7B92FB0ABADA}']
    function cornerRadius: CGFloat; cdecl;
    [MethodName('initWithAuthorizationButtonType:authorizationButtonStyle:')]
    function initWithAuthorizationButtonType(&type: ASAuthorizationAppleIDButtonType; style: ASAuthorizationAppleIDButtonStyle): Pointer; cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
  end;
  TASAuthorizationAppleIDButton = class(TOCGenericImport<ASAuthorizationAppleIDButtonClass, ASAuthorizationAppleIDButton>) end;

  ASAuthorizationCredential = interface(IObjectiveC)
    ['{766BE094-B8BD-4BF2-8720-6449D330EC99}']
  end;

  ASAuthorizationAppleIDCredentialClass = interface(NSObjectClass)
    ['{2F169AAA-8BC8-4B38-B6D7-FA990C0DF697}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationAppleIDCredential = interface(NSObject)
    ['{E966D338-E56C-44CF-B387-578CC5AC305A}']
    function authorizationCode: NSData; cdecl;
    function authorizedScopes: NSArray; cdecl;
    function email: NSString; cdecl;
    function fullName: NSPersonNameComponents; cdecl;
    function identityToken: NSData; cdecl;
    function realUserStatus: ASUserDetectionStatus; cdecl;
    function state: NSString; cdecl;
    function user: NSString; cdecl;
  end;
  TASAuthorizationAppleIDCredential = class(TOCGenericImport<ASAuthorizationAppleIDCredentialClass, ASAuthorizationAppleIDCredential>) end;

  ASAuthorizationProvider = interface(IObjectiveC)
    ['{2383B67E-C2EF-4932-AD6D-11B20E26DE27}']
  end;

  ASAuthorizationRequestClass = interface(NSObjectClass)
    ['{7565CA16-9A5A-4D16-B48F-5D274E86B0B0}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationRequest = interface(NSObject)
    ['{04540A4D-0BCA-489B-B4C8-91A91745FB33}']
    function provider: Pointer; cdecl;
  end;
  TASAuthorizationRequest = class(TOCGenericImport<ASAuthorizationRequestClass, ASAuthorizationRequest>) end;

  ASAuthorizationOpenIDRequestClass = interface(ASAuthorizationRequestClass)
    ['{070AF760-CC4F-4DFE-ADC8-9C488E19D2F5}']
  end;

  ASAuthorizationOpenIDRequest = interface(ASAuthorizationRequest)
    ['{CB45A889-4D5B-4B30-BEA4-08B1622D01AC}']
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
    ['{DE3A7DD5-90A5-4874-BF26-1105DDF8C0FD}']
  end;

  ASAuthorizationAppleIDRequest = interface(ASAuthorizationOpenIDRequest)
    ['{52F54FD0-BDF3-4808-AB3E-9E119976A46F}']
    procedure setUser(user: NSString); cdecl;
    function user: NSString; cdecl;
  end;
  TASAuthorizationAppleIDRequest = class(TOCGenericImport<ASAuthorizationAppleIDRequestClass, ASAuthorizationAppleIDRequest>) end;

  ASAuthorizationAppleIDProviderClass = interface(NSObjectClass)
    ['{B6096382-0F9B-4CB9-B58E-5DA15BB58B12}']
  end;

  ASAuthorizationAppleIDProvider = interface(NSObject)
    ['{387E96B8-BFA2-45A6-A7BC-D01E410C4678}']
    function createRequest: ASAuthorizationAppleIDRequest; cdecl;
    [MethodName('getCredentialStateForUserID:completion:')]
    procedure getCredentialStateForUserID(userID: NSString; completion: TASAuthorizationAppleIDProviderBlockMethod1); cdecl;
  end;
  TASAuthorizationAppleIDProvider = class(TOCGenericImport<ASAuthorizationAppleIDProviderClass, ASAuthorizationAppleIDProvider>) end;

  ASAuthorizationControllerDelegate = interface(IObjectiveC)
    ['{A49B1EEA-4FC2-478F-9DDC-9FD35F6CFBE2}']
    [MethodName('authorizationController:didCompleteWithAuthorization:')]
    procedure authorizationController(controller: ASAuthorizationController; authorization: ASAuthorization); overload; cdecl;
    [MethodName('authorizationController:didCompleteWithError:')]
    procedure authorizationController(controller: ASAuthorizationController; error: NSError); overload; cdecl;
  end;

  ASAuthorizationControllerPresentationContextProviding = interface(IObjectiveC)
    ['{D0BFF8A1-B170-4988-BD5A-96C1769E3B66}']
    function presentationAnchorForAuthorizationController(controller: ASAuthorizationController): ASPresentationAnchor; cdecl;
  end;

  ASAuthorizationControllerClass = interface(NSObjectClass)
    ['{399B407E-5F9D-4F26-884A-8D224946340E}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationController = interface(NSObject)
    ['{44683454-F2B9-4C19-A757-214861B07475}']
    function authorizationRequests: NSArray; cdecl;
    function delegate: Pointer; cdecl;
    function initWithAuthorizationRequests(authorizationRequests: NSArray): Pointer; cdecl;
    procedure performRequests; cdecl;
    function presentationContextProvider: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPresentationContextProvider(presentationContextProvider: Pointer); cdecl;
  end;
  TASAuthorizationController = class(TOCGenericImport<ASAuthorizationControllerClass, ASAuthorizationController>) end;

  ASAuthorizationPasswordRequestClass = interface(ASAuthorizationRequestClass)
    ['{F914EED8-F2F4-4143-B206-B61D30BCFE11}']
  end;

  ASAuthorizationPasswordRequest = interface(ASAuthorizationRequest)
    ['{C50DA791-FE0B-46C4-847B-0803C066BEF1}']
  end;
  TASAuthorizationPasswordRequest = class(TOCGenericImport<ASAuthorizationPasswordRequestClass, ASAuthorizationPasswordRequest>) end;

  ASAuthorizationPasswordProviderClass = interface(NSObjectClass)
    ['{5F1CCAE3-114D-4377-825A-9114F9D17C7D}']
  end;

  ASAuthorizationPasswordProvider = interface(NSObject)
    ['{A9D0DABE-B99C-4D0F-84BE-95969B958AD4}']
    function createRequest: ASAuthorizationPasswordRequest; cdecl;
  end;
  TASAuthorizationPasswordProvider = class(TOCGenericImport<ASAuthorizationPasswordProviderClass, ASAuthorizationPasswordProvider>) end;

  ASAuthorizationProviderExtensionAuthorizationRequestHandler = interface(IObjectiveC)
    ['{342DCA08-839E-4332-8A5C-15626ECEDBDB}']
    procedure beginAuthorizationWithRequest(request: ASAuthorizationProviderExtensionAuthorizationRequest); cdecl;
    procedure cancelAuthorizationWithRequest(request: ASAuthorizationProviderExtensionAuthorizationRequest); cdecl;
  end;

  ASAuthorizationProviderExtensionAuthorizationRequestClass = interface(NSObjectClass)
    ['{5C87B254-2645-4023-B987-42ABA81FA7D3}']
  end;

  ASAuthorizationProviderExtensionAuthorizationRequest = interface(NSObject)
    ['{B89E7B8E-A5FC-4394-AF8A-E5650C10B9D0}']
    function authorizationOptions: NSDictionary; cdecl;
    function callerBundleIdentifier: NSString; cdecl;
    procedure cancel; cdecl;
    procedure complete; cdecl;
    procedure completeWithError(error: NSError); cdecl;
    procedure completeWithHTTPAuthorizationHeaders(httpAuthorizationHeaders: NSDictionary); cdecl;
    [MethodName('completeWithHTTPResponse:httpBody:')]
    procedure completeWithHTTPResponse(httpResponse: NSHTTPURLResponse; httpBody: NSData); cdecl;
    procedure doNotHandle; cdecl;
    function extensionData: NSDictionary; cdecl;
    function httpBody: NSData; cdecl;
    function httpHeaders: NSDictionary; cdecl;
    procedure presentAuthorizationViewControllerWithCompletion(completion: TASAuthorizationProviderExtensionAuthorizationRequestBlockMethod1); cdecl;
    function realm: NSString; cdecl;
    function requestedOperation: ASAuthorizationProviderAuthorizationOperation; cdecl;
    function url: NSURL; cdecl;
  end;
  TASAuthorizationProviderExtensionAuthorizationRequest = class(TOCGenericImport<ASAuthorizationProviderExtensionAuthorizationRequestClass,
    ASAuthorizationProviderExtensionAuthorizationRequest>) end;

  ASAuthorizationSingleSignOnCredentialClass = interface(NSObjectClass)
    ['{D769A7F1-89AD-4385-A28D-CB122D1D7CC4}']
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationSingleSignOnCredential = interface(NSObject)
    ['{578C231A-2E5B-47C1-BCBE-C152A45F5544}']
    function accessToken: NSData; cdecl;
    function authenticatedResponse: NSHTTPURLResponse; cdecl;
    function authorizedScopes: NSArray; cdecl;
    function identityToken: NSData; cdecl;
    function state: NSString; cdecl;
  end;
  TASAuthorizationSingleSignOnCredential = class(TOCGenericImport<ASAuthorizationSingleSignOnCredentialClass,
    ASAuthorizationSingleSignOnCredential>) end;

  ASAuthorizationSingleSignOnRequestClass = interface(ASAuthorizationOpenIDRequestClass)
    ['{FFE2A4E2-419A-452D-A368-545BAEA5A05C}']
  end;

  ASAuthorizationSingleSignOnRequest = interface(ASAuthorizationOpenIDRequest)
    ['{6471D2B9-EBC8-4C00-A7D8-CB7E8119B9ED}']
    function authorizationOptions: NSArray; cdecl;
    procedure setAuthorizationOptions(authorizationOptions: NSArray); cdecl;
  end;
  TASAuthorizationSingleSignOnRequest = class(TOCGenericImport<ASAuthorizationSingleSignOnRequestClass, ASAuthorizationSingleSignOnRequest>) end;

  ASAuthorizationSingleSignOnProviderClass = interface(NSObjectClass)
    ['{5D8834E6-BE00-4DC9-A553-84BD943B920E}']
    {class} function authorizationProviderWithIdentityProviderURL(url: NSURL): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  ASAuthorizationSingleSignOnProvider = interface(NSObject)
    ['{EE9B2638-624D-4B8A-A9A0-DA0585BAAD24}']
    function canPerformAuthorization: Boolean; cdecl;
    function createRequest: ASAuthorizationSingleSignOnRequest; cdecl;
    function url: NSURL; cdecl;
  end;
  TASAuthorizationSingleSignOnProvider = class(TOCGenericImport<ASAuthorizationSingleSignOnProviderClass, ASAuthorizationSingleSignOnProvider>) end;

  ASCredentialIdentityStoreClass = interface(NSObjectClass)
    ['{932AB249-6E25-44E0-A199-0A67FAA05A9A}']
    {class} function sharedStore: ASCredentialIdentityStore; cdecl;
  end;

  ASCredentialIdentityStore = interface(NSObject)
    ['{3E04517A-9D10-4973-B7E1-D98B1CCC9DE3}']
    procedure getCredentialIdentityStoreStateWithCompletion(completion: TASCredentialIdentityStoreBlockMethod1); cdecl;
    procedure removeAllCredentialIdentitiesWithCompletion(completion: TASCredentialIdentityStoreBlockMethod2); cdecl;
    [MethodName('removeCredentialIdentities:completion:')]
    procedure removeCredentialIdentities(credentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod2); cdecl;
    [MethodName('replaceCredentialIdentitiesWithIdentities:completion:')]
    procedure replaceCredentialIdentitiesWithIdentities(newCredentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod2); cdecl;
    [MethodName('saveCredentialIdentities:completion:')]
    procedure saveCredentialIdentities(credentialIdentities: NSArray; completion: TASCredentialIdentityStoreBlockMethod2); cdecl;
  end;
  TASCredentialIdentityStore = class(TOCGenericImport<ASCredentialIdentityStoreClass, ASCredentialIdentityStore>) end;

  ASCredentialIdentityStoreStateClass = interface(NSObjectClass)
    ['{18F90A2C-E379-431B-94AF-6C0C0B36FAAC}']
  end;

  ASCredentialIdentityStoreState = interface(NSObject)
    ['{6360B318-FC34-4FE3-931E-309CF5DC3D5E}']
    function isEnabled: Boolean; cdecl;
    function supportsIncrementalUpdates: Boolean; cdecl;
  end;
  TASCredentialIdentityStoreState = class(TOCGenericImport<ASCredentialIdentityStoreStateClass, ASCredentialIdentityStoreState>) end;

  ASCredentialProviderExtensionContextClass = interface(NSExtensionContextClass)
    ['{C68494F7-4B16-4350-ABBD-210C9907E131}']
  end;

  ASCredentialProviderExtensionContext = interface(NSExtensionContext)
    ['{D3E02E76-57FD-4BE2-8409-A6071D9057E9}']
    procedure cancelRequestWithError(error: NSError); cdecl;
    procedure completeExtensionConfigurationRequest; cdecl;
    [MethodName('completeRequestReturningItems:completionHandler:')]
    procedure completeRequestReturningItems(items: NSArray; completionHandler: TASCredentialProviderExtensionContextBlockMethod2); cdecl;
    [MethodName('completeRequestWithSelectedCredential:completionHandler:')]
    procedure completeRequestWithSelectedCredential(credential: ASPasswordCredential;
      completionHandler: TASCredentialProviderExtensionContextBlockMethod1); cdecl;
  end;
  TASCredentialProviderExtensionContext = class(TOCGenericImport<ASCredentialProviderExtensionContextClass, ASCredentialProviderExtensionContext>) end;

  ASCredentialServiceIdentifierClass = interface(NSObjectClass)
    ['{4CE9A5DE-C471-41A6-91A8-97F87D0A3548}']
  end;

  ASCredentialServiceIdentifier = interface(NSObject)
    ['{4670ABB3-906C-4739-9857-521DCC9A2C4B}']
    function &type: ASCredentialServiceIdentifierType; cdecl;
    function identifier: NSString; cdecl;
    [MethodName('initWithIdentifier:type:')]
    function initWithIdentifier(identifier: NSString; &type: ASCredentialServiceIdentifierType): Pointer; cdecl;
  end;
  TASCredentialServiceIdentifier = class(TOCGenericImport<ASCredentialServiceIdentifierClass, ASCredentialServiceIdentifier>) end;

  ASPasswordCredentialIdentityClass = interface(NSObjectClass)
    ['{A533A0A4-F109-4862-878F-BF12F70B932C}']
    [MethodName('identityWithServiceIdentifier:user:recordIdentifier:')]
    {class} function identityWithServiceIdentifier(serviceIdentifier: ASCredentialServiceIdentifier; user: NSString;
      recordIdentifier: NSString): Pointer; cdecl;
  end;

  ASPasswordCredentialIdentity = interface(NSObject)
    ['{F9DF0117-AD97-4170-BF02-D0A8E0596EC0}']
    [MethodName('initWithServiceIdentifier:user:recordIdentifier:')]
    function initWithServiceIdentifier(serviceIdentifier: ASCredentialServiceIdentifier; user: NSString; recordIdentifier: NSString): Pointer; cdecl;
    function rank: NSInteger; cdecl;
    function recordIdentifier: NSString; cdecl;
    function serviceIdentifier: ASCredentialServiceIdentifier; cdecl;
    procedure setRank(rank: NSInteger); cdecl;
    function user: NSString; cdecl;
  end;
  TASPasswordCredentialIdentity = class(TOCGenericImport<ASPasswordCredentialIdentityClass, ASPasswordCredentialIdentity>) end;

  ASCredentialProviderViewControllerClass = interface(UIViewControllerClass)
    ['{46E226E4-9F46-4E39-99C5-36C7307B8B1D}']
  end;

  ASCredentialProviderViewController = interface(UIViewController)
    ['{1062E367-ABD5-4C71-A0EB-E31509C5DB98}']
    function extensionContext: ASCredentialProviderExtensionContext; cdecl;
    procedure prepareCredentialListForServiceIdentifiers(serviceIdentifiers: NSArray); cdecl;
    procedure prepareInterfaceForExtensionConfiguration; cdecl;
    procedure prepareInterfaceToProvideCredentialForIdentity(credentialIdentity: ASPasswordCredentialIdentity); cdecl;
    procedure provideCredentialWithoutUserInteractionForIdentity(credentialIdentity: ASPasswordCredentialIdentity); cdecl;
  end;
  TASCredentialProviderViewController = class(TOCGenericImport<ASCredentialProviderViewControllerClass, ASCredentialProviderViewController>) end;

  ASPasswordCredentialClass = interface(NSObjectClass)
    ['{DD6B0CEA-E87B-4CC3-B519-5844AFE95EF9}']
    [MethodName('credentialWithUser:password:')]
    {class} function credentialWithUser(user: NSString; password: NSString): Pointer; cdecl;
  end;

  ASPasswordCredential = interface(NSObject)
    ['{911DF418-91EF-43BA-B8B9-982DD92D9F4B}']
    [MethodName('initWithUser:password:')]
    function initWithUser(user: NSString; password: NSString): Pointer; cdecl;
    function password: NSString; cdecl;
    function user: NSString; cdecl;
  end;
  TASPasswordCredential = class(TOCGenericImport<ASPasswordCredentialClass, ASPasswordCredential>) end;

  ASWebAuthenticationSessionClass = interface(NSObjectClass)
    ['{43ADB9C5-43AB-4346-9A20-0B40573D0286}']
    {class} function new: Pointer; cdecl;
  end;

  ASWebAuthenticationSession = interface(NSObject)
    ['{86238BFC-F06E-4440-906A-F65F3CE29A29}']
    procedure cancel; cdecl;
    [MethodName('initWithURL:callbackURLScheme:completionHandler:')]
    function initWithURL(URL: NSURL; callbackURLScheme: NSString; completionHandler: ASWebAuthenticationSessionCompletionHandler): Pointer; cdecl;
    function prefersEphemeralWebBrowserSession: Boolean; cdecl;
    function presentationContextProvider: Pointer; cdecl;
    procedure setPrefersEphemeralWebBrowserSession(prefersEphemeralWebBrowserSession: Boolean); cdecl;
    procedure setPresentationContextProvider(presentationContextProvider: Pointer); cdecl;
    function start: Boolean; cdecl;
  end;
  TASWebAuthenticationSession = class(TOCGenericImport<ASWebAuthenticationSessionClass, ASWebAuthenticationSession>) end;

  ASWebAuthenticationPresentationContextProviding = interface(IObjectiveC)
    ['{4F02C9CC-97BB-467A-8040-180B218548CC}']
    function presentationAnchorForWebAuthenticationSession(session: ASWebAuthenticationSession): ASPresentationAnchor; cdecl;
  end;

  ASWebAuthenticationSessionRequestDelegate = interface(IObjectiveC)
    ['{2896A409-1422-4D36-9F03-26F903BE6C51}']
    [MethodName('authenticationSessionRequest:didCompleteWithCallbackURL:')]
    procedure authenticationSessionRequest(authenticationSessionRequest: ASWebAuthenticationSessionRequest; callbackURL: NSURL); overload; cdecl;
    [MethodName('authenticationSessionRequest:didCancelWithError:')]
    procedure authenticationSessionRequest(authenticationSessionRequest: ASWebAuthenticationSessionRequest; error: NSError); overload; cdecl;
  end;

  ASWebAuthenticationSessionRequestClass = interface(NSObjectClass)
    ['{C6BA3CD6-5870-4EFC-B931-8284352F3647}']
    {class} function new: Pointer; cdecl;
  end;

  ASWebAuthenticationSessionRequest = interface(NSObject)
    ['{D34B4C3D-CCF5-4E69-AE6B-F6C90B4C6A0D}']
    function callbackURLScheme: NSString; cdecl;
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
    ['{626FAB2D-7F58-4EF9-A833-AAB1B950A83D}']
    procedure beginHandlingWebAuthenticationSessionRequest(request: ASWebAuthenticationSessionRequest); cdecl;
    procedure cancelWebAuthenticationSessionRequest(request: ASWebAuthenticationSessionRequest); cdecl;
  end;

  ASWebAuthenticationSessionWebBrowserSessionManagerClass = interface(NSObjectClass)
    ['{01594A11-8297-4C1F-B6E4-3F28E18F0770}']
    {class} function sharedManager: ASWebAuthenticationSessionWebBrowserSessionManager; cdecl;
  end;

  ASWebAuthenticationSessionWebBrowserSessionManager = interface(NSObject)
    ['{645EBBA7-8D5C-4C19-BE31-3D15D02C7864}']
    function sessionHandler: Pointer; cdecl;
    procedure setSessionHandler(sessionHandler: Pointer); cdecl;
    function wasLaunchedByAuthenticationServices: Boolean; cdecl;
  end;
  TASWebAuthenticationSessionWebBrowserSessionManager = class(TOCGenericImport<ASWebAuthenticationSessionWebBrowserSessionManagerClass,
    ASWebAuthenticationSessionWebBrowserSessionManager>) end;

function ASAuthorizationScopeFullName: ASAuthorizationScope;
function ASAuthorizationScopeEmail: ASAuthorizationScope;
function ASAuthorizationOperationImplicit: ASAuthorizationOpenIDOperation;
function ASAuthorizationOperationLogin: ASAuthorizationOpenIDOperation;
function ASAuthorizationOperationRefresh: ASAuthorizationOpenIDOperation;
function ASAuthorizationOperationLogout: ASAuthorizationOpenIDOperation;
function ASAuthorizationAppleIDProviderCredentialRevokedNotification: NSNotificationName;
function ASAuthorizationErrorDomain: NSErrorDomain;
function ASCredentialIdentityStoreErrorDomain: NSErrorDomain;
function ASExtensionErrorDomain: NSErrorDomain;
function ASWebAuthenticationSessionErrorDomain: NSErrorDomain;

const
  libAuthenticationServices = '/System/Library/Frameworks/AuthenticationServices.framework/AuthenticationServices';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  AuthenticationServicesModule: THandle;
{$ELSE}

procedure AuthenticationServicesLoader; cdecl; external libAuthenticationServices;

{$ENDIF}

function ASAuthorizationScopeFullName: ASAuthorizationScope;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationScopeFullName');
end;

function ASAuthorizationScopeEmail: ASAuthorizationScope;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationScopeEmail');
end;

function ASAuthorizationOperationImplicit: ASAuthorizationOpenIDOperation;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationOperationImplicit');
end;

function ASAuthorizationOperationLogin: ASAuthorizationOpenIDOperation;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationOperationLogin');
end;

function ASAuthorizationOperationRefresh: ASAuthorizationOpenIDOperation;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationOperationRefresh');
end;

function ASAuthorizationOperationLogout: ASAuthorizationOpenIDOperation;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationOperationLogout');
end;

function ASAuthorizationAppleIDProviderCredentialRevokedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationAppleIDProviderCredentialRevokedNotification');
end;

function ASAuthorizationErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationErrorDomain');
end;

function ASCredentialIdentityStoreErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASCredentialIdentityStoreErrorDomain');
end;

function ASExtensionErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASExtensionErrorDomain');
end;

function ASWebAuthenticationSessionErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASWebAuthenticationSessionErrorDomain');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  AuthenticationServicesModule := dlopen(MarshaledAString(libAuthenticationServices), RTLD_LAZY);

finalization
  dlclose(AuthenticationServicesModule)
{$ENDIF}

end.