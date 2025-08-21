unit DW.iOSapi.FBSDKCoreKit;

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


// Imported using Facebook iOS SDK v11.1.0

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.WebKit, iOSapi.UIKit, iOSapi.CoreGraphics,
  // DW
  DW.Macapi.Dispatch, DW.iOSapi.Foundation;

const
  FBSDKAdvertisingTrackingAllowed = 0;
  FBSDKAdvertisingTrackingDisallowed = 1;
  FBSDKAdvertisingTrackingUnspecified = 2;
  FBSDKGraphRequestFlagNone = 0;
  FBSDKGraphRequestFlagSkipClientToken = 2;
  FBSDKGraphRequestFlagDoNotInvalidateTokenOnError = 4;
  FBSDKGraphRequestFlagDisableErrorRecovery = 8;
  FBSDKAppEventsFlushBehaviorAuto = 0;
  FBSDKAppEventsFlushBehaviorExplicitOnly = 1;
  FBSDKProductAvailabilityInStock = 0;
  FBSDKProductAvailabilityOutOfStock = 1;
  FBSDKProductAvailabilityPreOrder = 2;
  FBSDKProductAvailabilityAvailableForOrder = 3;
  FBSDKProductAvailabilityDiscontinued = 4;
  FBSDKProductConditionNew = 0;
  FBSDKProductConditionRefurbished = 1;
  FBSDKProductConditionUsed = 2;
  FBSDKErrorReserved = 0;
  FBSDKErrorEncryption = 1;
  FBSDKErrorInvalidArgument = 2;
  FBSDKErrorUnknown = 3;
  FBSDKErrorNetwork = 4;
  FBSDKErrorAppEventsFlush = 5;
  FBSDKErrorGraphRequestNonTextMimeTypeReturned = 6;
  FBSDKErrorGraphRequestProtocolMismatch = 7;
  FBSDKErrorGraphRequestGraphAPI = 8;
  FBSDKErrorDialogUnavailable = 9;
  FBSDKErrorAccessTokenRequired = 10;
  FBSDKErrorAppVersionUnsupported = 11;
  FBSDKErrorBrowserUnavailable = 12;
  FBSDKErrorBridgeAPIInterruption = 13;
  FBSDKErrorBridgeAPIResponse = 14;
  FBSDKGraphRequestErrorOther = 0;
  FBSDKGraphRequestErrorTransient = 1;
  FBSDKGraphRequestErrorRecoverable = 2;
  FBSDKFeatureNone = 0;
  FBSDKFeatureCore = 16777216;
  FBSDKFeatureAppEvents = 16842752;
  FBSDKFeatureCodelessEvents = 16843008;
  FBSDKFeatureRestrictiveDataFiltering = 16843264;
  FBSDKFeatureAAM = 16843520;
  FBSDKFeaturePrivacyProtection = 16843776;
  FBSDKFeatureSuggestedEvents = 16843777;
  FBSDKFeatureIntelligentIntegrity = 16843778;
  FBSDKFeatureModelRequest = 16843779;
  FBSDKFeatureEventDeactivation = 16844032;
  FBSDKFeatureSKAdNetwork = 16844288;
  FBSDKFeatureSKAdNetworkConversionValue = 16844289;
  FBSDKFeatureATELogging = 16844544;
  FBSDKFeatureAEM = 16844800;
  FBSDKFeatureInstrument = 16908288;
  FBSDKFeatureCrashReport = 16908544;
  FBSDKFeatureCrashShield = 16908545;
  FBSDKFeatureErrorReport = 16908800;
  FBSDKFeatureLogin = 33554432;
  FBSDKFeatureShare = 50331648;
  FBSDKFeatureGamingServices = 67108864;
  FBSDKAppLinkNavigationTypeFailure = 0;
  FBSDKAppLinkNavigationTypeBrowser = 1;
  FBSDKAppLinkNavigationTypeApp = 2;
  FBSDKBridgeAPIProtocolTypeNative = 0;
  FBSDKBridgeAPIProtocolTypeWeb = 1;
  FBSDKProfilePictureModeSquare = 0;
  FBSDKProfilePictureModeNormal = 1;
  FBSDKProfilePictureModeAlbum = 2;
  FBSDKProfilePictureModeSmall = 3;
  FBSDKProfilePictureModeLarge = 4;

type
  FBSDKGraphRequestConnectionDelegate = interface;
  FBSDKGraphRequestConnection = interface;
  FBSDKTokenCaching = interface;
  FBSDKAccessToken = interface;
  FBSDKAccessTokenProviding = interface;
  FBSDKAccessTokenSetting = interface;
  FBSDKGraphRequest = interface;
  FBSDKAppEvents = interface;
  FBSDKApplicationObserving = interface;
  FBSDKApplicationDelegate = interface;
  FBSDKAuthenticationToken = interface;
  FBSDKAuthenticationTokenClaims = interface;
  FBSDKImpressionTrackingButton = interface;
  FBSDKButton = interface;
  FBSDKButtonImpressionTracking = interface;
  FBSDKErrorRecoveryAttempting = interface;
  FBSDKError = interface;
  FBSDKFeatureChecking = interface;
  FBSDKGraphRequestConnecting = interface;
  FBSDKGraphRequestConnectionProviding = interface;
  FBSDKGraphRequestConnectionFactory = interface;
  FBSDKGraphRequestDataAttachment = interface;
  FBSDKInternalUtility = interface;
  FBSDKLocation = interface;
  FBSDKSettingsLogging = interface;
  FBSDKSettings = interface;
  FBSDKUserAgeRange = interface;
  FBSDKUtility = interface;
  FBSDKAppLinkTarget = interface;
  FBSDKAppLink = interface;
  FBSDKAppLinkResolving = interface;
  FBSDKAppLinkNavigation = interface;
  FBSDKAppLinkResolver = interface;
  FBSDKAppLinkResolverRequestBuilder = interface;
  FBSDKAppLinkUtility = interface;
  FBSDKURLOpening = interface;
  FBSDKGraphErrorRecoveryProcessorDelegate = interface;
  FBSDKGraphErrorRecoveryProcessor = interface;
  FBSDKMeasurementEvent = interface;
  FBSDKMutableCopying = interface;
  FBSDKProfilePictureView = interface;
  FBSDKProfile = interface;
  FBSDKURL = interface;
  FBSDKWebDialog = interface;
  FBSDKWebDialogDelegate = interface;
  FBSDKWebDialogView = interface;
  FBSDKWebDialogViewDelegate = interface;
  FBSDKWebViewAppLinkResolver = interface;
  FBSDKWindowFinding = interface;

  FBSDKGraphRequestCompletion = procedure(connection: Pointer; result: Pointer; error: NSError) of object;

  FBSDKGraphRequestBlock = procedure(connection: FBSDKGraphRequestConnection; result: Pointer; error: NSError) of object;
  FBSDKAdvertisingTrackingStatus = NSInteger;
  FBSDKAppEventName = NSString;
  FBSDKAppEventParameterName = NSString;
  FBSDKHTTPMethod = NSString;
  FBSDKGraphRequestFlags = NSInteger;


  FBSDKAppEventsFlushBehavior = NSInteger;
  FBSDKProductAvailability = NSInteger;
  FBSDKProductCondition = NSInteger;
  FBSDKAppEventUserDataType = NSString;
  FBSDKAppEventParameterProduct = NSString;
  FBSDKAppEventParameterValue = NSString;

  FBSDKCodeBlock = procedure of object;

  FBSDKErrorBlock = procedure(error: NSError) of object;

  FBSDKSuccessBlock = procedure(success: Boolean; error: NSError) of object;
  FBSDKCoreError = NSInteger;
  FBSDKGraphRequestError = NSInteger;
  FBSDKFeature = NSInteger;

  FBSDKFeatureManagerBlock = procedure(enabled: Boolean) of object;

  FBSDKLoggingBehavior = NSString;

  FBSDKAppLinkBlock = procedure(appLink: FBSDKAppLink; error: NSError) of object;
  FBSDKAppLinkNavigationType = NSInteger;

  FBSDKAppLinkNavigationBlock = procedure(navType: FBSDKAppLinkNavigationType; error: NSError) of object;

  FBSDKAppLinksBlock = procedure(appLinks: NSDictionary; error: NSError) of object;

  FBSDKURLBlock = procedure(url: NSURL; error: NSError) of object;
  FBSDKBridgeAPIProtocolType = NSInteger;

  FBSDKAuthenticationCompletionHandler = procedure(callbackURL: NSURL; error: NSError) of object;
  FBSDKProfilePictureMode = NSInteger;

  FBSDKProfileBlock = procedure(profile: FBSDKProfile; error: NSError) of object;
  FBSDKUserIdentifier = NSString;
  TFBSDKErrorRecoveryAttemptingBlockMethod1 = procedure(didRecover: Boolean) of object;

  FBSDKGraphRequestConnectionDelegate = interface(IObjectiveC)
    ['{73C13B73-6633-46AA-991E-2CFC39C51899}']
    procedure requestConnection(connection: Pointer; didSendBodyData: NSInteger; totalBytesWritten: NSInteger; totalBytesExpectedToWrite: NSInteger); overload; cdecl;
    procedure requestConnection(connection: Pointer; didFailWithError: NSError); overload; cdecl;
    procedure requestConnectionDidFinishLoading(connection: Pointer); cdecl;
    procedure requestConnectionWillBeginLoading(connection: Pointer); cdecl;
  end;

  FBSDKGraphRequestConnectionClass = interface(NSObjectClass)
    ['{F2A6C49F-2780-4B70-BD69-4FA231EFF89B}']
    {class} function defaultConnectionTimeout: NSTimeInterval; cdecl;
    {class} procedure setDefaultConnectionTimeout(defaultConnectionTimeout: NSTimeInterval); cdecl;
  end;

  FBSDKGraphRequestConnection = interface(NSObject)
    ['{7BECFB26-7E70-48AE-A422-4264E41C721D}']
    procedure addRequest(request: Pointer; name: NSString; completion: FBSDKGraphRequestCompletion); overload; cdecl;
    procedure addRequest(request: Pointer; batchParameters: NSDictionary; completionHandler: FBSDKGraphRequestBlock); overload; cdecl;
    procedure addRequest(request: Pointer; parameters: NSDictionary; completion: FBSDKGraphRequestCompletion); overload; cdecl;
    procedure addRequest(request: Pointer; completionHandler: FBSDKGraphRequestBlock); overload; cdecl;
    procedure addRequest(request: Pointer; completion: FBSDKGraphRequestCompletion); overload; cdecl;
    procedure addRequest(request: Pointer; batchEntryName: NSString; completionHandler: FBSDKGraphRequestBlock); overload; cdecl;
    procedure cancel; cdecl;
    function delegate: Pointer; cdecl;
    function delegateQueue: NSOperationQueue; cdecl;
    procedure overrideGraphAPIVersion(version: NSString); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDelegateQueue(delegateQueue: NSOperationQueue); cdecl;
    procedure setTimeout(timeout: NSTimeInterval); cdecl;
    procedure start; cdecl;
    function timeout: NSTimeInterval; cdecl;
    function urlResponse: NSHTTPURLResponse; cdecl;
  end;
  TFBSDKGraphRequestConnection = class(TOCGenericImport<FBSDKGraphRequestConnectionClass, FBSDKGraphRequestConnection>) end;

  FBSDKTokenCaching = interface(IObjectiveC)
    ['{ACC3119B-EC1C-4389-A22C-A5D341F3B43F}']
    function accessToken: FBSDKAccessToken; cdecl;
    function authenticationToken: FBSDKAuthenticationToken; cdecl;
    procedure setAccessToken(accessToken: FBSDKAccessToken); cdecl;
    procedure setAuthenticationToken(authenticationToken: FBSDKAuthenticationToken); cdecl;
  end;

  FBSDKAccessTokenClass = interface(NSObjectClass)
    ['{1551248D-30AE-4C9A-AEC4-BB0BB0AD1965}']
    {class} function currentAccessToken: FBSDKAccessToken; cdecl;
    {class} function isCurrentAccessTokenActive: Boolean; cdecl;
    {class} function new: Pointer; cdecl;
    {class} procedure refreshCurrentAccessToken(completionHandler: FBSDKGraphRequestBlock); cdecl;
    {class} procedure refreshCurrentAccessTokenWithCompletion(completion: FBSDKGraphRequestCompletion); cdecl;
    {class} procedure setCurrentAccessToken(currentAccessToken: FBSDKAccessToken); cdecl;
    {class} procedure setTokenCache(tokenCache: Pointer); cdecl;
    {class} function tokenCache: Pointer; cdecl;
  end;

  FBSDKAccessToken = interface(NSObject)
    ['{88D8656F-67F1-43FF-9B6D-BBC18E30F2D5}']
    function appID: NSString; cdecl;
    function dataAccessExpirationDate: NSDate; cdecl;
    function declinedPermissions: NSSet; cdecl;
    function expirationDate: NSDate; cdecl;
    function expiredPermissions: NSSet; cdecl;
    function graphDomain: NSString; cdecl;
    function hasGranted(permission: NSString): Boolean; cdecl;
    function initWithTokenString(tokenString: NSString; permissions: NSArray; declinedPermissions: NSArray; expiredPermissions: NSArray;
      appID: NSString; userID: NSString; expirationDate: NSDate; refreshDate: NSDate; dataAccessExpirationDate: NSDate): Pointer; overload; cdecl;
    function initWithTokenString(tokenString: NSString; permissions: NSArray; declinedPermissions: NSArray; expiredPermissions: NSArray;
      appID: NSString; userID: NSString; expirationDate: NSDate; refreshDate: NSDate; dataAccessExpirationDate: NSDate;
      graphDomain: NSString): Pointer; overload; cdecl;
    function isDataAccessExpired: Boolean; cdecl;
    function isEqualToAccessToken(token: FBSDKAccessToken): Boolean; cdecl;
    function isExpired: Boolean; cdecl;
    function permissions: NSSet; cdecl;
    function refreshDate: NSDate; cdecl;
    function tokenString: NSString; cdecl;
    function userID: NSString; cdecl;
  end;
  TFBSDKAccessToken = class(TOCGenericImport<FBSDKAccessTokenClass, FBSDKAccessToken>) end;

  FBSDKAccessTokenProviding = interface(IObjectiveC)
    ['{00A5104B-AE9D-4094-AC3E-7B8B192E2344}']
    {class} function currentAccessToken: FBSDKAccessToken; cdecl;
    {class} procedure setTokenCache(tokenCache: Pointer); cdecl;
    {class} function tokenCache: Pointer; cdecl;
  end;

  FBSDKAccessTokenSetting = interface(IObjectiveC)
    ['{D936839B-DABC-4C33-B5BF-D7861282ACCD}']
    {class} function currentAccessToken: FBSDKAccessToken; cdecl;
    {class} procedure setCurrentAccessToken(currentAccessToken: FBSDKAccessToken); cdecl;
  end;

  FBSDKGraphRequestClass = interface(NSObjectClass)
    ['{640F19DA-DC81-498F-ADEC-2CFA6B96DC14}']
    {class} function new: Pointer; cdecl;
  end;

  FBSDKGraphRequest = interface(NSObject)
    ['{D3C57036-4FB1-446D-BB70-DBDA14B06605}']
    function graphPath: NSString; cdecl;
    function HTTPMethod: NSString; cdecl;
    function initWithGraphPath(graphPath: NSString; parameters: NSDictionary; tokenString: NSString; version: NSString;
      HTTPMethod: FBSDKHTTPMethod): Pointer; overload; cdecl;
    function initWithGraphPath(graphPath: NSString; parameters: NSDictionary; HTTPMethod: FBSDKHTTPMethod): Pointer; overload; cdecl;
    function initWithGraphPath(graphPath: NSString): Pointer; overload; cdecl;
    function initWithGraphPath(graphPath: NSString; HTTPMethod: FBSDKHTTPMethod): Pointer; overload; cdecl;
    function initWithGraphPath(graphPath: NSString; parameters: NSDictionary): Pointer; overload; cdecl;
    function parameters: NSDictionary; cdecl;
    procedure setGraphErrorRecoveryDisabled(disable: Boolean); cdecl;
    procedure setParameters(parameters: NSDictionary); cdecl;
    function startWithCompletion(completion: FBSDKGraphRequestCompletion): Pointer; cdecl;
    function startWithCompletionHandler(handler: FBSDKGraphRequestBlock): Pointer; cdecl;
    function tokenString: NSString; cdecl;
    function version: NSString; cdecl;
  end;
  TFBSDKGraphRequest = class(TOCGenericImport<FBSDKGraphRequestClass, FBSDKGraphRequest>) end;

  FBSDKAppEventsClass = interface(NSObjectClass)
    ['{C81C801D-D0B9-471A-905F-3AAA59CC83CE}']
    {class} procedure activateApp; cdecl;
    {class} function anonymousID: NSString; cdecl;
    {class} procedure augmentHybridWKWebView(webView: WKWebView); cdecl;
    {class} procedure clearUserData; cdecl;
    {class} procedure clearUserDataForType(&type: FBSDKAppEventUserDataType); cdecl;
    {class} procedure clearUserID; cdecl;
    {class} procedure flush; cdecl;
    {class} function flushBehavior: FBSDKAppEventsFlushBehavior; cdecl;
    {class} function getUserData: NSString; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName; valueToSum: Double; parameters: NSDictionary); overload; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName); overload; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName; valueToSum: Double); overload; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName; parameters: NSDictionary); overload; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName; valueToSum: NSNumber; parameters: NSDictionary;
      accessToken: FBSDKAccessToken); overload; cdecl;
    {class} function loggingOverrideAppID: NSString; cdecl;
    {class} procedure logInternalEvent(eventName: FBSDKAppEventName; parameters: NSDictionary; isImplicitlyLogged: Boolean;
      accessToken: FBSDKAccessToken); overload; cdecl;
    {class} procedure logInternalEvent(eventName: FBSDKAppEventName; parameters: NSDictionary; isImplicitlyLogged: Boolean); overload; cdecl;
    {class} procedure logProductItem(itemID: NSString; availability: FBSDKProductAvailability; condition: FBSDKProductCondition;
      description: NSString; imageLink: NSString; link: NSString; title: NSString; priceAmount: Double; currency: NSString; gtin: NSString;
      mpn: NSString; brand: NSString; parameters: NSDictionary); cdecl;
    {class} procedure logPurchase(purchaseAmount: Double; currency: NSString); overload; cdecl;
    {class} procedure logPurchase(purchaseAmount: Double; currency: NSString; parameters: NSDictionary;
      accessToken: FBSDKAccessToken); overload; cdecl;
    {class} procedure logPurchase(purchaseAmount: Double; currency: NSString; parameters: NSDictionary); overload; cdecl;
    {class} procedure logPushNotificationOpen(payload: NSDictionary); overload; cdecl;
    {class} procedure logPushNotificationOpen(payload: NSDictionary; action: NSString); overload; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function requestForCustomAudienceThirdPartyIDWithAccessToken(accessToken: FBSDKAccessToken): FBSDKGraphRequest; cdecl;
    {class} procedure sendEventBindingsToUnity; cdecl;
    {class} procedure setFlushBehavior(flushBehavior: FBSDKAppEventsFlushBehavior); cdecl;
    {class} procedure setIsUnityInit(isUnityInit: Boolean); cdecl;
    {class} procedure setLoggingOverrideAppID(loggingOverrideAppID: NSString); cdecl;
    {class} procedure setPushNotificationsDeviceToken(deviceToken: NSData); cdecl;
    {class} procedure setPushNotificationsDeviceTokenString(deviceTokenString: NSString); cdecl;
    {class} procedure setUserData(data: NSString; forType: FBSDKAppEventUserDataType); cdecl;
    {class} procedure setUserEmail(email: NSString; firstName: NSString; lastName: NSString; phone: NSString; dateOfBirth: NSString;
      gender: NSString; city: NSString; state: NSString; zip: NSString; country: NSString); cdecl;
    {class} procedure setUserID(userID: NSString); cdecl;
    {class} function singleton: FBSDKAppEvents; cdecl;
    {class} function userID: NSString; cdecl;
  end;

  FBSDKAppEvents = interface(NSObject)
    ['{9EB9BA58-947A-4DDC-8AE1-475D668B94D6}']
    procedure activateApp; cdecl;
  end;
  TFBSDKAppEvents = class(TOCGenericImport<FBSDKAppEventsClass, FBSDKAppEvents>) end;

  FBSDKApplicationObserving = interface(IObjectiveC)
    ['{F63CCE1D-9519-4688-A45E-2EB9B8FE2C47}']
    function application(application: UIApplication; didFinishLaunchingWithOptions: NSDictionary): Boolean; overload; cdecl;
    function application(application: UIApplication; openURL: NSURL; sourceApplication: NSString; annotation: Pointer): Boolean; overload; cdecl;
    procedure applicationDidBecomeActive(application: UIApplication); cdecl;
    procedure applicationDidEnterBackground(application: UIApplication); cdecl;
    procedure applicationWillResignActive(application: UIApplication); cdecl;
  end;

  FBSDKApplicationDelegateClass = interface(NSObjectClass)
    ['{E0CCB638-E7CD-4FE2-9640-FABC2D1C656D}']
    {class} function new: Pointer; cdecl;
    {class} function sharedInstance: FBSDKApplicationDelegate; cdecl;
  end;

  FBSDKApplicationDelegate = interface(NSObject)
    ['{7486D83B-C242-47AC-843E-1E801EBE30E4}']
    procedure addObserver(observer: Pointer); cdecl;
    function application(application: UIApplication; didFinishLaunchingWithOptions: NSDictionary): Boolean; overload; cdecl;
    function application(application: UIApplication; openURL: NSURL; sourceApplication: NSString; annotation: Pointer): Boolean; overload; cdecl;
    procedure initializeSDK; cdecl;
    procedure removeObserver(observer: Pointer); cdecl;
  end;
  TFBSDKApplicationDelegate = class(TOCGenericImport<FBSDKApplicationDelegateClass, FBSDKApplicationDelegate>) end;

  FBSDKAuthenticationTokenClass = interface(NSObjectClass)
    ['{E3FA20B2-6484-4F9D-ADFE-9E85B8C7377C}']
    {class} function currentAuthenticationToken: FBSDKAuthenticationToken; cdecl;
    {class} function new: Pointer; cdecl;
    {class} procedure setCurrentAuthenticationToken(currentAuthenticationToken: FBSDKAuthenticationToken); cdecl;
    {class} procedure setTokenCache(tokenCache: Pointer); cdecl;
    {class} function tokenCache: Pointer; cdecl;
  end;

  FBSDKAuthenticationToken = interface(NSObject)
    ['{59B630A1-0790-4C97-B0BD-568D9F83F6D2}']
    function claims: FBSDKAuthenticationTokenClaims; cdecl;
    function graphDomain: NSString; cdecl;
    function nonce: NSString; cdecl;
    function tokenString: NSString; cdecl;
  end;
  TFBSDKAuthenticationToken = class(TOCGenericImport<FBSDKAuthenticationTokenClass, FBSDKAuthenticationToken>) end;

  FBSDKAuthenticationTokenClaimsClass = interface(NSObjectClass)
    ['{6E06FF44-A98C-4554-9184-8727145F9341}']
    {class} function new: Pointer; cdecl;
  end;

  FBSDKAuthenticationTokenClaims = interface(NSObject)
    ['{AA1E6293-C860-4FC8-9B15-E944E2F09030}']
    function aud: NSString; cdecl;
    function email: NSString; cdecl;
    function exp: NSTimeInterval; cdecl;
    function familyName: NSString; cdecl;
    function givenName: NSString; cdecl;
    function iat: NSTimeInterval; cdecl;
    function iss: NSString; cdecl;
    function jti: NSString; cdecl;
    function middleName: NSString; cdecl;
    function name: NSString; cdecl;
    function nonce: NSString; cdecl;
    function picture: NSString; cdecl;
    function sub: NSString; cdecl;
    function userAgeRange: NSDictionary; cdecl;
    function userBirthday: NSString; cdecl;
    function userFriends: NSArray; cdecl;
    function userGender: NSString; cdecl;
    function userHometown: NSDictionary; cdecl;
    function userLink: NSString; cdecl;
    function userLocation: NSDictionary; cdecl;
  end;
  TFBSDKAuthenticationTokenClaims = class(TOCGenericImport<FBSDKAuthenticationTokenClaimsClass, FBSDKAuthenticationTokenClaims>) end;

  FBSDKImpressionTrackingButtonClass = interface(UIButtonClass)
    ['{889B64D9-F1B0-47CE-9F0A-5AC3C6E06860}']
  end;

  FBSDKImpressionTrackingButton = interface(UIButton)
    ['{C92FD2AE-BCBF-4C0B-B216-6B2E31CEC3B6}']
  end;
  TFBSDKImpressionTrackingButton = class(TOCGenericImport<FBSDKImpressionTrackingButtonClass, FBSDKImpressionTrackingButton>) end;

  FBSDKButtonClass = interface(FBSDKImpressionTrackingButtonClass)
    ['{D2C7F99C-383A-485D-A203-4650107F2544}']
  end;

  FBSDKButton = interface(FBSDKImpressionTrackingButton)
    ['{12EB6EBA-EF06-4BB1-9EEE-5638F97BBE8F}']
  end;
  TFBSDKButton = class(TOCGenericImport<FBSDKButtonClass, FBSDKButton>) end;

  FBSDKButtonImpressionTracking = interface(IObjectiveC)
    ['{B3A2A6B3-491D-4F4B-9B3D-DE5805696FE9}']
    function analyticsParameters: NSDictionary; cdecl;
    function impressionTrackingEventName: NSString; cdecl;
    function impressionTrackingIdentifier: NSString; cdecl;
  end;

  FBSDKErrorRecoveryAttempting = interface(IObjectiveC)
    ['{557E9AB6-1DBA-42FB-8816-4ACA6F46E918}']
    procedure attemptRecoveryFromError(error: NSError; optionIndex: NSUInteger; completionHandler: TFBSDKErrorRecoveryAttemptingBlockMethod1); cdecl;
  end;

  FBSDKErrorClass = interface(NSObjectClass)
    ['{B343EC78-54DD-4164-B639-D22CEC87C1C6}']
    {class} function errorWithCode(code: NSInteger; message: NSString): NSError; overload; cdecl;
    {class} function errorWithCode(code: NSInteger; message: NSString; underlyingError: NSError): NSError; overload; cdecl;
    {class} function errorWithDomain(domain: NSErrorDomain; code: NSInteger; userInfo: NSDictionary; message: NSString;
      underlyingError: NSError): NSError; overload; cdecl;
    {class} function errorWithDomain(domain: NSErrorDomain; code: NSInteger; message: NSString; underlyingError: NSError): NSError; overload; cdecl;
    {class} function errorWithDomain(domain: NSErrorDomain; code: NSInteger; message: NSString): NSError; overload; cdecl;
    {class} function invalidArgumentErrorWithDomain(domain: NSErrorDomain; name: NSString; value: Pointer;
      message: NSString): NSError; overload; cdecl;
    {class} function invalidArgumentErrorWithDomain(domain: NSErrorDomain; name: NSString; value: Pointer; message: NSString;
      underlyingError: NSError): NSError; overload; cdecl;
    {class} function invalidArgumentErrorWithName(name: NSString; value: Pointer; message: NSString): NSError; cdecl;
    {class} function isNetworkError(error: NSError): Boolean; cdecl;
    {class} function requiredArgumentErrorWithDomain(domain: NSErrorDomain; name: NSString; message: NSString): NSError; cdecl;
    {class} function unknownErrorWithMessage(message: NSString): NSError; cdecl;
  end;

  FBSDKError = interface(NSObject)
    ['{5F2866EB-4D6F-4B94-BB0F-916CDE492A64}']
  end;
  TFBSDKError = class(TOCGenericImport<FBSDKErrorClass, FBSDKError>) end;

  FBSDKFeatureChecking = interface(IObjectiveC)
    ['{7F4E5CFD-C269-4047-9A1E-D48E33DA776C}']
    procedure checkFeature(feature: FBSDKFeature; completionBlock: FBSDKFeatureManagerBlock); cdecl;
    function isEnabled(feature: FBSDKFeature): Boolean; cdecl;
  end;

  FBSDKGraphRequestConnecting = interface(IObjectiveC)
    ['{5D829D02-AA56-4F45-922C-1A2849637CAE}']
    procedure addRequest(request: Pointer; completion: FBSDKGraphRequestCompletion); cdecl;
    procedure cancel; cdecl;
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setTimeout(timeout: NSTimeInterval); cdecl;
    procedure start; cdecl;
    function timeout: NSTimeInterval; cdecl;
  end;

  FBSDKGraphRequestConnectionProviding = interface(IObjectiveC)
    ['{D3C1F507-8AD3-441E-8D89-C5153A06A65F}']
    function createGraphRequestConnection: Pointer; cdecl;
  end;

  FBSDKGraphRequestConnectionFactoryClass = interface(NSObjectClass)
    ['{5FD9D340-3640-4A78-80CA-C585344DC345}']
  end;

  FBSDKGraphRequestConnectionFactory = interface(NSObject)
    ['{CFC054ED-BAD2-4973-A33D-230B7F776BED}']
  end;
  TFBSDKGraphRequestConnectionFactory = class(TOCGenericImport<FBSDKGraphRequestConnectionFactoryClass, FBSDKGraphRequestConnectionFactory>) end;

  FBSDKGraphRequestDataAttachmentClass = interface(NSObjectClass)
    ['{1583A863-DDC4-4A58-94AB-162792F20CE2}']
    {class} function new: Pointer; cdecl;
  end;

  FBSDKGraphRequestDataAttachment = interface(NSObject)
    ['{6BCC073D-EDB6-407D-B860-5C1266959677}']
    function contentType: NSString; cdecl;
    function data: NSData; cdecl;
    function filename: NSString; cdecl;
    function initWithData(data: NSData; filename: NSString; contentType: NSString): Pointer; cdecl;
  end;
  TFBSDKGraphRequestDataAttachment = class(TOCGenericImport<FBSDKGraphRequestDataAttachmentClass, FBSDKGraphRequestDataAttachment>) end;

  FBSDKInternalUtilityClass = interface(NSObjectClass)
    ['{06FEC1BC-9014-47CA-8FCC-4464ED71E1D9}']
    {class} function new: Pointer; cdecl;
    {class} function sharedUtility: FBSDKInternalUtility; cdecl;
  end;

  FBSDKInternalUtility = interface(NSObject)
    ['{3F9C1AB1-EF3F-4E01-807F-3308540F4C1A}']
    [MethodName('object:isEqualToObject:')]
    function &object(&object: Pointer; isEqualToObject: Pointer): Boolean; cdecl;
    function appURLWithHost(host: NSString; path: NSString; queryParameters: NSDictionary; error: PPointer): NSURL; cdecl;
    function bundleForStrings: NSBundle; cdecl;
    procedure checkRegisteredCanOpenURLScheme(urlScheme: NSString); cdecl;
    procedure extractPermissionsFromResponse(responseObject: NSDictionary; grantedPermissions: NSMutableSet; declinedPermissions: NSMutableSet;
      expiredPermissions: NSMutableSet); cdecl;
    function facebookURLWithHostPrefix(hostPrefix: NSString; path: NSString; queryParameters: NSDictionary; error: PPointer): NSURL; cdecl;
    function isBrowserURL(URL: NSURL): Boolean; cdecl;
    function isFacebookAppInstalled: Boolean; cdecl;
    function isMessengerAppInstalled: Boolean; cdecl;
    function isMSQRDPlayerAppInstalled: Boolean; cdecl;
    function isRegisteredCanOpenURLScheme(urlScheme: NSString): Boolean; cdecl;
    function isRegisteredURLScheme(urlScheme: NSString): Boolean; cdecl;
    function parametersFromFBURL(url: NSURL): NSDictionary; cdecl;
    procedure registerTransientObject(&object: Pointer); cdecl;
    function topMostViewController: UIViewController; cdecl;
    procedure unregisterTransientObject(&object: Pointer); cdecl;
    function URLWithScheme(scheme: NSString; host: NSString; path: NSString; queryParameters: NSDictionary; error: PPointer): NSURL; cdecl;
    procedure validateAppID; cdecl;
    function validateRequiredClientAccessToken: NSString; cdecl;
    procedure validateURLSchemes; cdecl;
    function viewControllerForView(view: UIView): UIViewController; cdecl;
  end;
  TFBSDKInternalUtility = class(TOCGenericImport<FBSDKInternalUtilityClass, FBSDKInternalUtility>) end;

  FBSDKLocationClass = interface(NSObjectClass)
    ['{CD7FF8F4-1E92-4BFE-A935-520E0DAB46C4}']
    {class} function locationFromDictionary(dictionary: NSDictionary): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  FBSDKLocation = interface(NSObject)
    ['{AA2607D4-BBF7-46E8-90D2-B43344596934}']
    function id: NSString; cdecl;
    function name: NSString; cdecl;
  end;
  TFBSDKLocation = class(TOCGenericImport<FBSDKLocationClass, FBSDKLocation>) end;

  FBSDKSettingsClass = interface(NSObjectClass)
    ['{2136CD59-322B-49DF-BAC5-50878ED54E83}']
    {class} function appID: NSString; cdecl;
    {class} function appURLSchemeSuffix: NSString; cdecl;
    {class} function clientToken: NSString; cdecl;
    {class} function defaultGraphAPIVersion: NSString; cdecl;
    {class} procedure disableLoggingBehavior(loggingBehavior: FBSDKLoggingBehavior); cdecl;
    {class} function displayName: NSString; cdecl;
    {class} procedure enableLoggingBehavior(loggingBehavior: FBSDKLoggingBehavior); cdecl;
    {class} function facebookDomainPart: NSString; cdecl;
    {class} function graphAPIVersion: NSString; cdecl;
    {class} function isAdvertiserIDCollectionEnabled: Boolean; cdecl;
    {class} function isAdvertiserTrackingEnabled: Boolean; cdecl;
    {class} function isAutoLogAppEventsEnabled: Boolean; cdecl;
    {class} function isCodelessDebugLogEnabled: Boolean; cdecl;
    {class} function isGraphErrorRecoveryEnabled: Boolean; cdecl;
    {class} function isSKAdNetworkReportEnabled: Boolean; cdecl;
    {class} function JPEGCompressionQuality: CGFloat; cdecl;
    {class} function loggingBehaviors: NSSet; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function sdkVersion: NSString; cdecl;
    {class} procedure setAdvertiserIDCollectionEnabled(advertiserIDCollectionEnabled: Boolean); cdecl;
    {class} function setAdvertiserTrackingEnabled(advertiserTrackingEnabled: Boolean): Boolean; cdecl;
    {class} procedure setAppID(appID: NSString); cdecl;
    {class} procedure setAppURLSchemeSuffix(appURLSchemeSuffix: NSString); cdecl;
    {class} procedure setAutoLogAppEventsEnabled(autoLogAppEventsEnabled: Boolean); cdecl;
    {class} procedure setClientToken(clientToken: NSString); cdecl;
    {class} procedure setCodelessDebugLogEnabled(codelessDebugLogEnabled: Boolean); cdecl;
    {class} procedure setDataProcessingOptions(options: NSArray); overload; cdecl;
    {class} procedure setDataProcessingOptions(options: NSArray; country: Integer; state: Integer); overload; cdecl;
    {class} procedure setDisplayName(displayName: NSString); cdecl;
    {class} procedure setFacebookDomainPart(facebookDomainPart: NSString); cdecl;
    {class} procedure setGraphAPIVersion(graphAPIVersion: NSString); cdecl;
    {class} procedure setGraphErrorRecoveryEnabled(graphErrorRecoveryEnabled: Boolean); cdecl;
    {class} procedure setJPEGCompressionQuality(JPEGCompressionQuality: CGFloat); cdecl;
    {class} procedure setLimitEventAndDataUsage(limitEventAndDataUsage: Boolean); cdecl;
    {class} procedure setLoggingBehaviors(loggingBehaviors: NSSet); cdecl;
    {class} procedure setShouldUseCachedValuesForExpensiveMetadata(shouldUseCachedValuesForExpensiveMetadata: Boolean); cdecl;
    {class} procedure setSKAdNetworkReportEnabled(SKAdNetworkReportEnabled: Boolean); cdecl;
    {class} function shouldLimitEventAndDataUsage: Boolean; cdecl;
    {class} function shouldUseCachedValuesForExpensiveMetadata: Boolean; cdecl;
  end;

  FBSDKSettings = interface(NSObject)
    ['{D2E62529-585E-46D7-B2A5-7C44C2257E09}']
  end;
  TFBSDKSettings = class(TOCGenericImport<FBSDKSettingsClass, FBSDKSettings>) end;

  FBSDKSettingsLogging = interface(IObjectiveC)
    ['{5D78A38B-C4F6-4D3E-8A73-C54505F0835E}']
    procedure logIfSDKSettingsChanged; cdecl;
    procedure logWarnings; cdecl;
    procedure recordInstall; cdecl;
  end;

  FBSDKUserAgeRangeClass = interface(NSObjectClass)
    ['{21FC37C5-1A99-4094-A536-12DDE7D7F6D5}']
    {class} function ageRangeFromDictionary(dictionary: NSDictionary): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  FBSDKUserAgeRange = interface(NSObject)
    ['{F4ECF311-B4D5-4F40-8D4E-B641A3652B3F}']
    function max: NSNumber; cdecl;
    function min: NSNumber; cdecl;
  end;
  TFBSDKUserAgeRange = class(TOCGenericImport<FBSDKUserAgeRangeClass, FBSDKUserAgeRange>) end;

  FBSDKUtilityClass = interface(NSObjectClass)
    ['{329B012D-F6E8-46ED-8D44-7CD04E8D27CD}']
    {class} function dictionaryWithQueryString(queryString: NSString): NSDictionary; cdecl;
    {class} function getGraphDomainFromToken: NSString; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function queryStringWithDictionary(dictionary: NSDictionary; error: PPointer): NSString; cdecl;
    {class} function SHA256Hash(input: NSObject): NSString; cdecl;
    {class} function startGCDTimerWithInterval(interval: Double; block: dispatch_block_t): dispatch_source_t; cdecl;
    {class} procedure stopGCDTimer(timer: dispatch_source_t); cdecl;
    {class} function unversionedFacebookURLWithHostPrefix(hostPrefix: NSString; path: NSString; queryParameters: NSDictionary;
      error: PPointer): NSURL; cdecl;
    {class} function URLDecode(value: NSString): NSString; cdecl;
    {class} function URLEncode(value: NSString): NSString; cdecl;
  end;

  FBSDKUtility = interface(NSObject)
    ['{E46F2B3C-251C-4E27-8BB5-D19E8297A395}']
  end;
  TFBSDKUtility = class(TOCGenericImport<FBSDKUtilityClass, FBSDKUtility>) end;

  FBSDKAppLinkTargetClass = interface(NSObjectClass)
    ['{69412EFA-2C16-4EDE-84A3-64BB592FA50B}']
    {class} function appLinkTargetWithURL(url: NSURL; appStoreId: NSString; appName: NSString): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  FBSDKAppLinkTarget = interface(NSObject)
    ['{5BE4133F-2D3C-4062-84DF-D7AD2097062D}']
    function appName: NSString; cdecl;
    function appStoreId: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TFBSDKAppLinkTarget = class(TOCGenericImport<FBSDKAppLinkTargetClass, FBSDKAppLinkTarget>) end;

  FBSDKAppLinkClass = interface(NSObjectClass)
    ['{49609765-3037-46D8-9D06-F6D0C088CC39}']
    {class} function appLinkWithSourceURL(sourceURL: NSURL; targets: NSArray; webURL: NSURL): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  FBSDKAppLink = interface(NSObject)
    ['{96066AE7-141E-44EF-9EB4-56112EC30354}']
    function sourceURL: NSURL; cdecl;
    function targets: NSArray; cdecl;
    function webURL: NSURL; cdecl;
  end;
  TFBSDKAppLink = class(TOCGenericImport<FBSDKAppLinkClass, FBSDKAppLink>) end;

  FBSDKAppLinkResolving = interface(IObjectiveC)
    ['{5E09CF44-9E72-491A-BDBE-DBCC437B6786}']
    procedure appLinkFromURL(url: NSURL; handler: FBSDKAppLinkBlock); cdecl;
  end;

  FBSDKAppLinkNavigationClass = interface(NSObjectClass)
    ['{0775AEFA-0446-4DC5-A315-FF148DABF57B}']
    {class} function callbackAppLinkDataForAppWithName(appName: NSString; url: NSString): NSDictionary; cdecl;
    {class} function defaultResolver: Pointer; cdecl;
    {class} function navigateToAppLink(link: FBSDKAppLink; error: PPointer): FBSDKAppLinkNavigationType; cdecl;
    {class} procedure navigateToURL(destination: NSURL; handler: FBSDKAppLinkNavigationBlock); overload; cdecl;
    {class} procedure navigateToURL(destination: NSURL; resolver: Pointer; handler: FBSDKAppLinkNavigationBlock); overload; cdecl;
    {class} function navigationTypeForLink(link: FBSDKAppLink): FBSDKAppLinkNavigationType; cdecl;
    {class} function navigationWithAppLink(appLink: FBSDKAppLink; extras: NSDictionary; appLinkData: NSDictionary): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
    {class} procedure resolveAppLink(destination: NSURL; handler: FBSDKAppLinkBlock); overload; cdecl;
    {class} procedure resolveAppLink(destination: NSURL; resolver: Pointer; handler: FBSDKAppLinkBlock); overload; cdecl;
    {class} procedure setDefaultResolver(defaultResolver: Pointer); cdecl;
  end;

  FBSDKAppLinkNavigation = interface(NSObject)
    ['{1E64C43C-2EBE-4E25-BDFB-01D273BB40E1}']
    function appLink: FBSDKAppLink; cdecl;
    function appLinkData: NSDictionary; cdecl;
    function extras: NSDictionary; cdecl;
    function navigate(error: PPointer): FBSDKAppLinkNavigationType; cdecl;
    function navigationType: FBSDKAppLinkNavigationType; cdecl;
  end;
  TFBSDKAppLinkNavigation = class(TOCGenericImport<FBSDKAppLinkNavigationClass, FBSDKAppLinkNavigation>) end;

  FBSDKAppLinkResolverClass = interface(NSObjectClass)
    ['{27957F30-BC54-4CDF-B94B-FB365DFC31D5}']
    {class} function new: Pointer; cdecl;
    {class} function resolver: Pointer; cdecl;
  end;

  FBSDKAppLinkResolver = interface(NSObject)
    ['{366DC185-A063-402D-8ACF-3E92CB19F4C0}']
    procedure appLinksFromURLs(urls: NSArray; handler: FBSDKAppLinksBlock); cdecl;
  end;
  TFBSDKAppLinkResolver = class(TOCGenericImport<FBSDKAppLinkResolverClass, FBSDKAppLinkResolver>) end;

  FBSDKAppLinkResolverRequestBuilderClass = interface(NSObjectClass)
    ['{051FDD32-2230-406F-A530-D1BEAC1339E3}']
  end;

  FBSDKAppLinkResolverRequestBuilder = interface(NSObject)
    ['{A8D1FD0D-99F8-4BEA-AD85-1B4FC588C9CE}']
    function getIdiomSpecificField: NSString; cdecl;
    function requestForURLs(urls: NSArray): FBSDKGraphRequest; cdecl;
  end;
  TFBSDKAppLinkResolverRequestBuilder = class(TOCGenericImport<FBSDKAppLinkResolverRequestBuilderClass, FBSDKAppLinkResolverRequestBuilder>) end;

  FBSDKAppLinkUtilityClass = interface(NSObjectClass)
    ['{9AECB199-5D90-4275-AC34-9A5B2A5E6F06}']
    {class} function appInvitePromotionCodeFromURL(url: NSURL): NSString; cdecl;
    {class} procedure fetchDeferredAppLink(handler: FBSDKURLBlock); cdecl;
    {class} function isMatchURLScheme(scheme: NSString): Boolean; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  FBSDKAppLinkUtility = interface(NSObject)
    ['{6AE59FA7-5709-4B88-9BA6-7E847699761C}']
  end;
  TFBSDKAppLinkUtility = class(TOCGenericImport<FBSDKAppLinkUtilityClass, FBSDKAppLinkUtility>) end;

  FBSDKURLOpening = interface(IObjectiveC)
    ['{8EC4FB1F-98B5-4880-90D8-719571E1003D}']
    function application(application: UIApplication; openURL: NSURL; sourceApplication: NSString; annotation: Pointer): Boolean; cdecl;
    procedure applicationDidBecomeActive(application: UIApplication); cdecl;
    function canOpenURL(url: NSURL; forApplication: UIApplication; sourceApplication: NSString; annotation: Pointer): Boolean; cdecl;
    function isAuthenticationURL(url: NSURL): Boolean; cdecl;
    function shouldStopPropagationOfURL(url: NSURL): Boolean; cdecl;
  end;

  FBSDKGraphErrorRecoveryProcessorDelegate = interface(IObjectiveC)
    ['{A9972DE6-5F0E-447D-8C20-501567D2CB69}']
    procedure processorDidAttemptRecovery(processor: FBSDKGraphErrorRecoveryProcessor; didRecover: Boolean; error: NSError); cdecl;
    function processorWillProcessError(processor: FBSDKGraphErrorRecoveryProcessor; error: NSError): Boolean; cdecl;
  end;

  FBSDKGraphErrorRecoveryProcessorClass = interface(NSObjectClass)
    ['{E06BA7D3-731E-4A16-AAD1-6C0A6664E3B5}']
  end;

  FBSDKGraphErrorRecoveryProcessor = interface(NSObject)
    ['{D7C1205C-8D6F-4C24-8370-B868AE1F8DCE}']
    function processError(error: NSError; request: Pointer; delegate: Pointer): Boolean; cdecl;
  end;
  TFBSDKGraphErrorRecoveryProcessor = class(TOCGenericImport<FBSDKGraphErrorRecoveryProcessorClass, FBSDKGraphErrorRecoveryProcessor>) end;

  FBSDKMeasurementEventClass = interface(NSObjectClass)
    ['{A6983CC2-9AAA-4EF6-94C0-6A8D4CCEEA16}']
  end;

  FBSDKMeasurementEvent = interface(NSObject)
    ['{FB4FD41C-544F-46B6-98C6-23F9D16772D6}']
  end;
  TFBSDKMeasurementEvent = class(TOCGenericImport<FBSDKMeasurementEventClass, FBSDKMeasurementEvent>) end;

  FBSDKMutableCopying = interface(IObjectiveC)
    ['{BEB20403-F3E4-4222-8727-7ED7A24FA303}']
    function mutableCopy: Pointer; cdecl;
  end;

  FBSDKProfilePictureViewClass = interface(UIViewClass)
    ['{639D6DF0-45FC-4479-ADD6-93C6CAD19FFF}']
  end;

  FBSDKProfilePictureView = interface(UIView)
    ['{5C2C40E9-D05F-4930-8BBD-2C77FFF6E87B}']
    function initWithFrame(frame: CGRect; profile: FBSDKProfile): Pointer; cdecl;
    function initWithProfile(profile: FBSDKProfile): Pointer; cdecl;
    function pictureMode: FBSDKProfilePictureMode; cdecl;
    function profileID: NSString; cdecl;
    procedure setNeedsImageUpdate; cdecl;
    procedure setPictureMode(pictureMode: FBSDKProfilePictureMode); cdecl;
    procedure setProfileID(profileID: NSString); cdecl;
  end;
  TFBSDKProfilePictureView = class(TOCGenericImport<FBSDKProfilePictureViewClass, FBSDKProfilePictureView>) end;

  FBSDKProfileClass = interface(NSObjectClass)
    ['{DA41350E-0966-4898-918D-D071F19213F2}']
    {class} function currentProfile: FBSDKProfile; cdecl;
    {class} procedure enableUpdatesOnAccessTokenChange(enable: Boolean); cdecl;
    {class} procedure loadCurrentProfileWithCompletion(completion: FBSDKProfileBlock); cdecl;
    {class} function new: Pointer; cdecl;
    {class} procedure setCurrentProfile(currentProfile: FBSDKProfile); cdecl;
  end;

  FBSDKProfile = interface(NSObject)
    ['{B7D0ABEF-1002-4760-A09F-A64D05E87AF6}']
    function ageRange: FBSDKUserAgeRange; cdecl;
    function birthday: NSDate; cdecl;
    function email: NSString; cdecl;
    function firstName: NSString; cdecl;
    function friendIDs: NSArray; cdecl;
    function gender: NSString; cdecl;
    function hometown: FBSDKLocation; cdecl;
    function imageURL: NSURL; cdecl;
    function imageURLForPictureMode(mode: FBSDKProfilePictureMode; size: CGSize): NSURL; cdecl;
    function initWithUserID(userID: FBSDKUserIdentifier; firstName: NSString; middleName: NSString; lastName: NSString; name: NSString;
      linkURL: NSURL; refreshDate: NSDate): Pointer; overload; cdecl;
    function initWithUserID(userID: FBSDKUserIdentifier; firstName: NSString; middleName: NSString; lastName: NSString; name: NSString;
      linkURL: NSURL; refreshDate: NSDate; imageURL: NSURL; email: NSString; friendIDs: NSArray; birthday: NSDate; ageRange: FBSDKUserAgeRange;
      hometown: FBSDKLocation; location: FBSDKLocation; gender: NSString): Pointer; overload; cdecl;
    function initWithUserID(userID: FBSDKUserIdentifier; firstName: NSString; middleName: NSString; lastName: NSString; name: NSString;
      linkURL: NSURL; refreshDate: NSDate; imageURL: NSURL; email: NSString; friendIDs: NSArray; birthday: NSDate; ageRange: FBSDKUserAgeRange;
      hometown: FBSDKLocation; location: FBSDKLocation; gender: NSString; isLimited: Boolean): Pointer; overload; cdecl;
    function isEqualToProfile(profile: FBSDKProfile): Boolean; cdecl;
    function lastName: NSString; cdecl;
    function linkURL: NSURL; cdecl;
    function location: FBSDKLocation; cdecl;
    function middleName: NSString; cdecl;
    function name: NSString; cdecl;
    function refreshDate: NSDate; cdecl;
    function userID: FBSDKUserIdentifier; cdecl;
  end;
  TFBSDKProfile = class(TOCGenericImport<FBSDKProfileClass, FBSDKProfile>) end;

  FBSDKURLClass = interface(NSObjectClass)
    ['{D83878C7-BD16-4BC8-8CBF-4DD2A9478D09}']
    {class} function new: Pointer; cdecl;
    {class} function URLWithInboundURL(url: NSURL; sourceApplication: NSString): Pointer; cdecl;
    {class} function URLWithURL(url: NSURL): Pointer; cdecl;
  end;

  FBSDKURL = interface(NSObject)
    ['{BEC6BD4C-BDF1-4AA8-AC54-75305BF20B92}']
    function appLinkData: NSDictionary; cdecl;
    function appLinkExtras: NSDictionary; cdecl;
    function appLinkReferer: FBSDKAppLink; cdecl;
    function inputQueryParameters: NSDictionary; cdecl;
    function inputURL: NSURL; cdecl;
    function isAutoAppLink: Boolean; cdecl;
    function targetQueryParameters: NSDictionary; cdecl;
    function targetURL: NSURL; cdecl;
  end;
  TFBSDKURL = class(TOCGenericImport<FBSDKURLClass, FBSDKURL>) end;

  FBSDKWebDialogClass = interface(NSObjectClass)
    ['{781A6395-6616-4692-89FD-4C33C0D81969}']
    {class} function createAndShow(name: NSString; parameters: NSDictionary; frame: CGRect; delegate: Pointer; windowFinder: Pointer): Pointer; cdecl;
    {class} function dialogWithName(name: NSString; delegate: Pointer): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function showWithName(name: NSString; parameters: NSDictionary; delegate: Pointer): Pointer; cdecl;
  end;

  FBSDKWebDialog = interface(NSObject)
    ['{3E7BD388-25E5-4266-99C6-8810A5E03A26}']
    procedure setShouldDeferVisibility(shouldDeferVisibility: Boolean); cdecl;
    procedure setWindowFinder(windowFinder: Pointer); cdecl;
    function shouldDeferVisibility: Boolean; cdecl;
    function windowFinder: Pointer; cdecl;
  end;
  TFBSDKWebDialog = class(TOCGenericImport<FBSDKWebDialogClass, FBSDKWebDialog>) end;

  FBSDKWebDialogDelegate = interface(IObjectiveC)
    ['{6FE70F5B-EC7C-4D2B-8BB3-527AA460B6BF}']
    procedure webDialog(webDialog: FBSDKWebDialog; didFailWithError: NSError); overload; cdecl;
    procedure webDialog(webDialog: FBSDKWebDialog; didCompleteWithResults: NSDictionary); overload; cdecl;
    procedure webDialogDidCancel(webDialog: FBSDKWebDialog); cdecl;
  end;

  FBSDKWebDialogViewClass = interface(UIViewClass)
    ['{E9B9D1A1-60F6-46FA-8850-EBDD09829242}']
    {class} procedure configureWithWebViewProvider(provider: Pointer; urlOpener: Pointer); cdecl;
  end;

  FBSDKWebDialogView = interface(UIView)
    ['{B8B93CAA-6018-4612-B9D2-09E560ACFC00}']
    function delegate: Pointer; cdecl;
    procedure loadURL(URL: NSURL); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure stopLoading; cdecl;
  end;
  TFBSDKWebDialogView = class(TOCGenericImport<FBSDKWebDialogViewClass, FBSDKWebDialogView>) end;

  FBSDKWebDialogViewDelegate = interface(IObjectiveC)
    ['{7D4EE5DC-ED09-4A18-AD1E-8A6C99E2C00C}']
    procedure webDialogView(webDialogView: FBSDKWebDialogView; didFailWithError: NSError); overload; cdecl;
    procedure webDialogView(webDialogView: FBSDKWebDialogView; didCompleteWithResults: NSDictionary); overload; cdecl;
    procedure webDialogViewDidCancel(webDialogView: FBSDKWebDialogView); cdecl;
    procedure webDialogViewDidFinishLoad(webDialogView: FBSDKWebDialogView); cdecl;
  end;

  FBSDKWebViewAppLinkResolverClass = interface(NSObjectClass)
    ['{D577DD33-FB97-4F26-8AC5-F59481808672}']
    {class} function sharedInstance: FBSDKWebViewAppLinkResolver; cdecl;
  end;

  FBSDKWebViewAppLinkResolver = interface(NSObject)
    ['{118C15C1-E9AB-4D6E-B00A-884A76846EFF}']
  end;
  TFBSDKWebViewAppLinkResolver = class(TOCGenericImport<FBSDKWebViewAppLinkResolverClass, FBSDKWebViewAppLinkResolver>) end;

  FBSDKWindowFinding = interface(IObjectiveC)
    ['{4D34C434-B3D9-4284-BFBC-FF93E0108BCA}']
    function findWindow: UIWindow; cdecl;
  end;

const
  libFBAEMKit = 'FBAEMKit.a';
  libFBSDKCoreKit = 'FBSDKCoreKit.a';

function fb_randomString(numberOfBytes: NSUInteger): NSString; cdecl;
  external libFBSDKCoreKit name _PU + 'fb_randomString';

implementation

//procedure FBAEMKitLoader; cdecl; external framework libFBAEMKit;
//procedure FBSDKCoreKitLoader; cdecl; external framework libFBSDKCoreKit;
procedure AccelerateLoader; cdecl; external framework 'Accelerate';
procedure ClangRTLoader; cdecl; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
procedure FBSDKCoreKitLoader; cdecl; external libFBSDKCoreKit dependency 'c++';

end.