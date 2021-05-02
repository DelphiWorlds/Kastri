unit DW.iOSapi.FBSDKCoreKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.WebKit,
  // DW
  DW.Macapi.Dispatch;

const
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
  FBSDKGraphRequestErrorOther = 0;
  FBSDKGraphRequestErrorTransient = 1;
  FBSDKGraphRequestErrorRecoverable = 2;
  FBSDKAppLinkNavigationTypeFailure = 0;
  FBSDKAppLinkNavigationTypeBrowser = 1;
  FBSDKAppLinkNavigationTypeApp = 2;
  FBSDKIncludeStatusBarInSizeNever = 0;
  FBSDKIncludeStatusBarInSizeAlways = 1;
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
  FBSDKProfilePictureModeSquare = 0;
  FBSDKProfilePictureModeNormal = 1;
  FBSDKProfilePictureModeAlbum = 2;
  FBSDKProfilePictureModeSmall = 3;
  FBSDKProfilePictureModeLarge = 4;

type
  FBSDKAppLinkResolving = interface;
  FBSDKWebViewAppLinkResolver = interface;
  FBSDKErrorRecoveryAttempting = interface;
  FBSDKTestUsersManager = interface;
  FBSDKAppLinkTarget = interface;
  FBSDKAppLink = interface;
  FBSDKAppLinkNavigation = interface;
  FBSDKAppLinkReturnToRefererViewDelegate = interface;
  FBSDKAppLinkReturnToRefererView = interface;
  FBSDKGraphRequestConnectionDelegate = interface;
  FBSDKGraphRequestConnection = interface;
  FBSDKAppEvents = interface;
  FBSDKCopying = interface;
  FBSDKAccessToken = interface;
  FBSDKURL = interface;
  FBSDKUtility = interface;
  FBSDKSettings = interface;
  FBSDKAppLinkReturnToRefererControllerDelegate = interface;
  FBSDKAppLinkReturnToRefererController = interface;
  FBSDKAppLinkUtility = interface;
  FBSDKMeasurementEvent = interface;
  FBSDKApplicationDelegate = interface;
  FBSDKAuthenticationToken = interface;
  FBSDKButton = interface;
  FBSDKGraphRequest = interface;
  FBSDKGraphRequestDataAttachment = interface;
  FBSDKAppLinkResolver = interface;
  FBSDKAppLinkResolverRequestBuilder = interface;
  FBSDKGraphErrorRecoveryProcessorDelegate = interface;
  FBSDKGraphErrorRecoveryProcessor = interface;
  FBSDKMutableCopying = interface;
  FBSDKProfilePictureView = interface;
  FBSDKProfile = interface;

  FBSDKAppLinkBlock = procedure(appLink: FBSDKAppLink; error: NSError) of object;

  FBSDKCodeBlock = procedure of object;

  FBSDKErrorBlock = procedure(error: NSError) of object;

  FBSDKSuccessBlock = procedure(success: Boolean; error: NSError) of object;
  FBSDKCoreError = NSInteger;
  FBSDKGraphRequestError = NSInteger;

  FBSDKAccessTokensBlock = procedure(tokens: NSArray; error: NSError) of object;
  FBSDKAppLinkNavigationType = NSInteger;

  FBSDKAppLinkNavigationBlock = procedure(navType: FBSDKAppLinkNavigationType; error: NSError) of object;
  FBSDKIncludeStatusBarInSize = NSInteger;

  FBSDKGraphRequestBlock = procedure(connection: FBSDKGraphRequestConnection; result: Pointer; error: NSError) of object;
  FBSDKAppEventsFlushBehavior = NSInteger;
  FBSDKProductAvailability = NSInteger;
  FBSDKProductCondition = NSInteger;
  FBSDKAppEventName = NSString;
  FBSDKAppEventParameterName = NSString;
  FBSDKAppEventParameterProduct = NSString;
  FBSDKAppEventParameterValue = NSString;
  FBSDKAppEventUserDataType = NSString;
  FBSDKLoggingBehavior = NSString;

  FBSDKURLBlock = procedure(url: NSURL; error: NSError) of object;
  FBSDKHTTPMethod = NSString;

  FBSDKAppLinksBlock = procedure(appLinks: NSDictionary; error: NSError) of object;
  FBSDKProfilePictureMode = NSInteger;

  FBSDKProfileBlock = procedure(profile: FBSDKProfile; error: NSError) of object;

  FBSDKAppLinkResolving = interface(IObjectiveC)
    ['{28B96DB9-78D5-4C31-A1AB-009F089AC4D3}']
    procedure appLinkFromURL(url: NSURL; handler: FBSDKAppLinkBlock); cdecl;
  end;

  FBSDKWebViewAppLinkResolverClass = interface(NSObjectClass)
    ['{FBA875E9-6A52-4FDE-B7CB-16E9EA3E2723}']
    {class} function sharedInstance: FBSDKWebViewAppLinkResolver; cdecl;
  end;

  FBSDKWebViewAppLinkResolver = interface(NSObject)
    ['{DDF2D16B-5412-4C53-A190-730136B0A682}']
  end;
  TFBSDKWebViewAppLinkResolver = class(TOCGenericImport<FBSDKWebViewAppLinkResolverClass, FBSDKWebViewAppLinkResolver>) end;

  FBSDKErrorRecoveryAttempting = interface(IObjectiveC)
    ['{746DED57-9A10-483D-B19A-AF3E09AFA697}']
    procedure attemptRecoveryFromError(error: NSError; optionIndex: NSUInteger; delegate: Pointer; didRecoverSelector: Pointer;
      contextInfo: Pointer); cdecl;
  end;

  FBSDKTestUsersManagerClass = interface(NSObjectClass)
    ['{0040C4C2-3C8A-4295-8349-331ADDEF3780}']
    {class} function new: Pointer; cdecl;
    {class} function sharedInstanceForAppID(appID: NSString; appSecret: NSString): Pointer; cdecl;
  end;

  FBSDKTestUsersManager = interface(NSObject)
    ['{BC172EFF-B9D5-47B3-B1CC-9D94EBAB5FA6}']
    procedure addTestAccountWithPermissions(permissions: NSSet; completionHandler: FBSDKAccessTokensBlock); cdecl;
    procedure makeFriendsWithFirst(first: FBSDKAccessToken; second: FBSDKAccessToken; callback: FBSDKErrorBlock); cdecl;
    procedure removeTestAccount(userId: NSString; completionHandler: FBSDKErrorBlock); cdecl;
    procedure requestTestAccountTokensWithArraysOfPermissions(arraysOfPermissions: NSArray; createIfNotFound: Boolean;
      completionHandler: FBSDKAccessTokensBlock); cdecl;
  end;
  TFBSDKTestUsersManager = class(TOCGenericImport<FBSDKTestUsersManagerClass, FBSDKTestUsersManager>) end;

  FBSDKAppLinkTargetClass = interface(NSObjectClass)
    ['{3D701BDF-F8C2-41FC-A52F-636ECD2B77A4}']
    {class} function appLinkTargetWithURL(url: NSURL; appStoreId: NSString; appName: NSString): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  FBSDKAppLinkTarget = interface(NSObject)
    ['{1F023126-AAE1-4F6E-AABE-3EC7BAEAC012}']
    function appName: NSString; cdecl;
    function appStoreId: NSString; cdecl;
    function URL: NSURL; cdecl;
  end;
  TFBSDKAppLinkTarget = class(TOCGenericImport<FBSDKAppLinkTargetClass, FBSDKAppLinkTarget>) end;

  FBSDKAppLinkClass = interface(NSObjectClass)
    ['{8DFB9DAB-E462-412F-B459-BB5C9BDD876E}']
    {class} function appLinkWithSourceURL(sourceURL: NSURL; targets: NSArray; webURL: NSURL): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  FBSDKAppLink = interface(NSObject)
    ['{040D7036-F6A7-401F-BC2A-24320086DA29}']
    function sourceURL: NSURL; cdecl;
    function targets: NSArray; cdecl;
    function webURL: NSURL; cdecl;
  end;
  TFBSDKAppLink = class(TOCGenericImport<FBSDKAppLinkClass, FBSDKAppLink>) end;

  FBSDKAppLinkNavigationClass = interface(NSObjectClass)
    ['{6B9938E4-D286-4393-BD60-2C4E372A1502}']
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
    ['{D9611CFB-BCE6-4406-B70E-DB05C6A49924}']
    function appLink: FBSDKAppLink; cdecl;
    function appLinkData: NSDictionary; cdecl;
    function extras: NSDictionary; cdecl;
    function navigate(error: PPointer): FBSDKAppLinkNavigationType; cdecl;
    function navigationType: FBSDKAppLinkNavigationType; cdecl;
  end;
  TFBSDKAppLinkNavigation = class(TOCGenericImport<FBSDKAppLinkNavigationClass, FBSDKAppLinkNavigation>) end;

  FBSDKAppLinkReturnToRefererViewDelegate = interface(IObjectiveC)
    ['{EAE7846F-76A4-4C20-A2DB-D9E8AA349BF3}']
    procedure returnToRefererViewDidTapInsideCloseButton(view: FBSDKAppLinkReturnToRefererView); cdecl;
    procedure returnToRefererViewDidTapInsideLink(view: FBSDKAppLinkReturnToRefererView; link: FBSDKAppLink); cdecl;
  end;

  FBSDKAppLinkReturnToRefererViewClass = interface(UIViewClass)
    ['{483E63C6-1E64-432B-B3B3-A450FF30EFD0}']
  end;

  FBSDKAppLinkReturnToRefererView = interface(UIView)
    ['{3F750687-8615-423F-8525-1E3C142D2CD1}']
    function delegate: Pointer; cdecl;
    function includeStatusBarInSize: FBSDKIncludeStatusBarInSize; cdecl;
    function isClosed: Boolean; cdecl;
    function refererAppLink: FBSDKAppLink; cdecl;
    procedure setClosed(closed: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setIncludeStatusBarInSize(includeStatusBarInSize: FBSDKIncludeStatusBarInSize); cdecl;
    procedure setRefererAppLink(refererAppLink: FBSDKAppLink); cdecl;
    procedure setTextColor(textColor: UIColor); cdecl;
    function textColor: UIColor; cdecl;
  end;
  TFBSDKAppLinkReturnToRefererView = class(TOCGenericImport<FBSDKAppLinkReturnToRefererViewClass, FBSDKAppLinkReturnToRefererView>) end;

  FBSDKGraphRequestConnectionDelegate = interface(IObjectiveC)
    ['{A1A10E31-899E-47F8-B7E0-769D17AA009C}']
    procedure requestConnection(connection: FBSDKGraphRequestConnection; didSendBodyData: NSInteger; totalBytesWritten: NSInteger;
      totalBytesExpectedToWrite: NSInteger); overload; cdecl;
    procedure requestConnection(connection: FBSDKGraphRequestConnection; didFailWithError: NSError); overload; cdecl;
    procedure requestConnectionDidFinishLoading(connection: FBSDKGraphRequestConnection); cdecl;
    procedure requestConnectionWillBeginLoading(connection: FBSDKGraphRequestConnection); cdecl;
  end;

  FBSDKGraphRequestConnectionClass = interface(NSObjectClass)
    ['{68DDECBC-EB88-4360-9C0A-3DAD1D89A532}']
    {class} function defaultConnectionTimeout: NSTimeInterval; cdecl;
    {class} procedure setDefaultConnectionTimeout(defaultConnectionTimeout: NSTimeInterval); cdecl;
  end;

  FBSDKGraphRequestConnection = interface(NSObject)
    ['{8E671212-4840-4B45-85AF-2ECD3D1B1E0A}']
    procedure addRequest(request: FBSDKGraphRequest; batchParameters: NSDictionary; completionHandler: FBSDKGraphRequestBlock); overload; cdecl;
    procedure addRequest(request: FBSDKGraphRequest; batchEntryName: NSString; completionHandler: FBSDKGraphRequestBlock); overload; cdecl;
    procedure addRequest(request: FBSDKGraphRequest; completionHandler: FBSDKGraphRequestBlock); overload; cdecl;
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

  FBSDKAppEventsClass = interface(NSObjectClass)
    ['{840B0331-CFCE-4B0F-9DEF-D0F6CD4185C1}']
    {class} procedure activateApp; cdecl;
    {class} function anonymousID: NSString; cdecl;
    {class} procedure augmentHybridWKWebView(webView: WKWebView); cdecl;
    {class} procedure clearUserData; cdecl;
    {class} procedure clearUserDataForType(&type: FBSDKAppEventUserDataType); cdecl;
    {class} procedure clearUserID; cdecl;
    {class} procedure flush; cdecl;
    {class} function flushBehavior: FBSDKAppEventsFlushBehavior; cdecl;
    {class} function getUserData: NSString; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName; valueToSum: Double); overload; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName); overload; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName; valueToSum: Double; parameters: NSDictionary); overload; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName; valueToSum: NSNumber; parameters: NSDictionary;
      accessToken: FBSDKAccessToken); overload; cdecl;
    {class} procedure logEvent(eventName: FBSDKAppEventName; parameters: NSDictionary); overload; cdecl;
    {class} function loggingOverrideAppID: NSString; cdecl;
    {class} procedure logProductItem(itemID: NSString; availability: FBSDKProductAvailability; condition: FBSDKProductCondition;
      description: NSString; imageLink: NSString; link: NSString; title: NSString; priceAmount: Double; currency: NSString; gtin: NSString;
      mpn: NSString; brand: NSString; parameters: NSDictionary); cdecl;
    {class} procedure logPurchase(purchaseAmount: Double; currency: NSString); overload; cdecl;
    {class} procedure logPurchase(purchaseAmount: Double; currency: NSString; parameters: NSDictionary); overload; cdecl;
    {class} procedure logPurchase(purchaseAmount: Double; currency: NSString; parameters: NSDictionary;
      accessToken: FBSDKAccessToken); overload; cdecl;
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
    {class} procedure setUserEmail(email: NSString; firstName: NSString; lastName: NSString; phone: NSString; dateOfBirth: NSString; gender: NSString;
      city: NSString; state: NSString; zip: NSString; country: NSString); cdecl;
    {class} procedure setUserID(userID: NSString); cdecl;
    {class} procedure updateUserProperties(properties: NSDictionary; handler: FBSDKGraphRequestBlock); cdecl;
    {class} function userID: NSString; cdecl;
  end;

  FBSDKAppEvents = interface(NSObject)
    ['{2C8A00B8-D797-479A-B716-199403B65589}']
  end;
  TFBSDKAppEvents = class(TOCGenericImport<FBSDKAppEventsClass, FBSDKAppEvents>) end;

  FBSDKCopying = interface(IObjectiveC)
    ['{30BF0DA8-82E9-4534-B824-A512DD39B576}']
    function copy: Pointer; cdecl;
  end;

  FBSDKAccessTokenClass = interface(NSObjectClass)
    ['{90087E52-78D3-446F-A567-E0CB7FEC008B}']
    {class} function currentAccessToken: FBSDKAccessToken; cdecl;
    {class} function isCurrentAccessTokenActive: Boolean; cdecl;
    {class} function new: Pointer; cdecl;
    {class} procedure refreshCurrentAccessToken(completionHandler: FBSDKGraphRequestBlock); cdecl;
    {class} procedure setCurrentAccessToken(currentAccessToken: FBSDKAccessToken); cdecl;
  end;

  FBSDKAccessToken = interface(NSObject)
    ['{129C8E21-4322-4DE1-BC77-05D767B3DA97}']
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

  FBSDKURLClass = interface(NSObjectClass)
    ['{2F9A3975-FF91-4EDB-8690-A14301CA1604}']
    {class} function new: Pointer; cdecl;
    {class} function URLWithInboundURL(url: NSURL; sourceApplication: NSString): Pointer; cdecl;
    {class} function URLWithURL(url: NSURL): Pointer; cdecl;
  end;

  FBSDKURL = interface(NSObject)
    ['{8AE28432-6B24-4A49-81A2-BEEAF446EECD}']
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

  FBSDKUtilityClass = interface(NSObjectClass)
    ['{149F31DF-4A75-470F-8E9F-0ECD4DE2E910}']
    {class} function dictionaryWithQueryString(queryString: NSString): NSDictionary; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function queryStringWithDictionary(dictionary: NSDictionary; error: PPointer): NSString; cdecl;
    {class} function SHA256Hash(input: NSObject): NSString; cdecl;
    {class} function startGCDTimerWithInterval(interval: Double; block: dispatch_block_t): dispatch_source_t; cdecl;
    {class} procedure stopGCDTimer(timer: dispatch_source_t); cdecl;
    {class} function URLDecode(value: NSString): NSString; cdecl;
    {class} function URLEncode(value: NSString): NSString; cdecl;
  end;

  FBSDKUtility = interface(NSObject)
    ['{017A5FED-108B-4A60-9352-5F13A92878DC}']
  end;
  TFBSDKUtility = class(TOCGenericImport<FBSDKUtilityClass, FBSDKUtility>) end;

  FBSDKSettingsClass = interface(NSObjectClass)
    ['{E284822A-836A-49F3-99A5-8EAE80F9162C}']
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
    {class} procedure setSKAdNetworkReportEnabled(SKAdNetworkReportEnabled: Boolean); cdecl;
    {class} function shouldLimitEventAndDataUsage: Boolean; cdecl;
  end;

  FBSDKSettings = interface(NSObject)
    ['{D02BCB3F-0EE7-4D0D-A8DD-11648D764725}']
  end;
  TFBSDKSettings = class(TOCGenericImport<FBSDKSettingsClass, FBSDKSettings>) end;

  FBSDKAppLinkReturnToRefererControllerDelegate = interface(IObjectiveC)
    ['{712F243E-EA36-4E22-AA00-F769A2576CF6}']
    procedure returnToRefererController(controller: FBSDKAppLinkReturnToRefererController; willNavigateToAppLink: FBSDKAppLink); overload; cdecl;
    procedure returnToRefererController(controller: FBSDKAppLinkReturnToRefererController; didNavigateToAppLink: FBSDKAppLink;
      &type: FBSDKAppLinkNavigationType); overload; cdecl;
  end;

  FBSDKAppLinkReturnToRefererControllerClass = interface(NSObjectClass)
    ['{96B6D02E-A773-4A4D-B723-80639B4BBA4A}']
  end;

  FBSDKAppLinkReturnToRefererController = interface(NSObject)
    ['{7805DBC3-2797-4457-8720-4AEC4E6B9980}']
    procedure closeViewAnimated(animated: Boolean); cdecl;
    function delegate: Pointer; cdecl;
    function initForDisplayAboveNavController(navController: UINavigationController): Pointer; cdecl;
    procedure removeFromNavController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setView(view: FBSDKAppLinkReturnToRefererView); cdecl;
    procedure showViewForRefererAppLink(refererAppLink: FBSDKAppLink); cdecl;
    procedure showViewForRefererURL(url: NSURL); cdecl;
    function view: FBSDKAppLinkReturnToRefererView; cdecl;
  end;
  TFBSDKAppLinkReturnToRefererController = class(TOCGenericImport<FBSDKAppLinkReturnToRefererControllerClass,
    FBSDKAppLinkReturnToRefererController>) end;

  FBSDKAppLinkUtilityClass = interface(NSObjectClass)
    ['{8563ABC8-DBE1-4E9D-8214-0B885273E4DE}']
    {class} function appInvitePromotionCodeFromURL(url: NSURL): NSString; cdecl;
    {class} procedure fetchDeferredAppLink(handler: FBSDKURLBlock); cdecl;
    {class} function isMatchURLScheme(scheme: NSString): Boolean; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  FBSDKAppLinkUtility = interface(NSObject)
    ['{103FD572-3AE7-4AE6-87A9-FF01892BA5BD}']
  end;
  TFBSDKAppLinkUtility = class(TOCGenericImport<FBSDKAppLinkUtilityClass, FBSDKAppLinkUtility>) end;

  FBSDKMeasurementEventClass = interface(NSObjectClass)
    ['{24D8BF45-8F30-47C1-A122-29540DDEFB2C}']
  end;

  FBSDKMeasurementEvent = interface(NSObject)
    ['{2CF4B716-33A2-4C96-A4DE-832845A013B3}']
  end;
  TFBSDKMeasurementEvent = class(TOCGenericImport<FBSDKMeasurementEventClass, FBSDKMeasurementEvent>) end;

  FBSDKApplicationDelegateClass = interface(NSObjectClass)
    ['{30368E11-39DD-4210-B06E-B21421408FE5}']
    {class} procedure initializeSDK(launchOptions: NSDictionary); cdecl;
    {class} function new: Pointer; cdecl;
    {class} function sharedInstance: FBSDKApplicationDelegate; cdecl;
  end;

  FBSDKApplicationDelegate = interface(NSObject)
    ['{F5885CBF-B380-47F7-9122-C649DE220A4F}']
    function application(application: UIApplication; openURL: NSURL; sourceApplication: NSString; annotation: Pointer): Boolean; overload; cdecl;
    function application(application: UIApplication; didFinishLaunchingWithOptions: NSDictionary): Boolean; overload; cdecl;
  end;
  TFBSDKApplicationDelegate = class(TOCGenericImport<FBSDKApplicationDelegateClass, FBSDKApplicationDelegate>) end;

  FBSDKAuthenticationTokenClass = interface(NSObjectClass)
    ['{6BA230A7-C16C-4284-9D10-897B5D463E0D}']
    {class} function currentAuthenticationToken: FBSDKAuthenticationToken; cdecl;
    {class} function new: Pointer; cdecl;
    {class} procedure setCurrentAuthenticationToken(currentAuthenticationToken: FBSDKAuthenticationToken); cdecl;
  end;

  FBSDKAuthenticationToken = interface(NSObject)
    ['{FB65E6F7-422E-4002-AA18-CD419FA96C92}']
    function graphDomain: NSString; cdecl;
    function nonce: NSString; cdecl;
    function tokenString: NSString; cdecl;
  end;
  TFBSDKAuthenticationToken = class(TOCGenericImport<FBSDKAuthenticationTokenClass, FBSDKAuthenticationToken>) end;

  FBSDKButtonClass = interface(UIButtonClass)
    ['{2947F553-6C44-4323-8FAA-5A8CDD4856B3}']
  end;

  FBSDKButton = interface(UIButton)
    ['{F83A996E-F322-455F-83B7-0732984B4088}']
  end;
  TFBSDKButton = class(TOCGenericImport<FBSDKButtonClass, FBSDKButton>) end;

  FBSDKGraphRequestClass = interface(NSObjectClass)
    ['{92968D88-83F2-4C31-B563-9C783200CD26}']
    {class} function new: Pointer; cdecl;
  end;

  FBSDKGraphRequest = interface(NSObject)
    ['{C7660944-41F1-4AD3-9E5F-96F15E297012}']
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
    function startWithCompletionHandler(handler: FBSDKGraphRequestBlock): FBSDKGraphRequestConnection; cdecl;
    function tokenString: NSString; cdecl;
    function version: NSString; cdecl;
  end;
  TFBSDKGraphRequest = class(TOCGenericImport<FBSDKGraphRequestClass, FBSDKGraphRequest>) end;

  FBSDKGraphRequestDataAttachmentClass = interface(NSObjectClass)
    ['{E1536F2D-9491-4FBB-A071-AD8B281259F1}']
    {class} function new: Pointer; cdecl;
  end;

  FBSDKGraphRequestDataAttachment = interface(NSObject)
    ['{F392E941-A4ED-4920-BB02-31223DD3AF7D}']
    function contentType: NSString; cdecl;
    function data: NSData; cdecl;
    function filename: NSString; cdecl;
    function initWithData(data: NSData; filename: NSString; contentType: NSString): Pointer; cdecl;
  end;
  TFBSDKGraphRequestDataAttachment = class(TOCGenericImport<FBSDKGraphRequestDataAttachmentClass, FBSDKGraphRequestDataAttachment>) end;

  FBSDKAppLinkResolverClass = interface(NSObjectClass)
    ['{C09B4534-05F9-484E-BDCB-7BCCD2D219B7}']
    {class} function new: Pointer; cdecl;
    {class} function resolver: Pointer; cdecl;
  end;

  FBSDKAppLinkResolver = interface(NSObject)
    ['{C7859CF0-9980-47B4-A1C6-E4810FDDA305}']
    procedure appLinksFromURLs(urls: NSArray; handler: FBSDKAppLinksBlock); cdecl;
  end;
  TFBSDKAppLinkResolver = class(TOCGenericImport<FBSDKAppLinkResolverClass, FBSDKAppLinkResolver>) end;

  FBSDKAppLinkResolverRequestBuilderClass = interface(NSObjectClass)
    ['{FA054392-5745-4BEF-A2E8-DEE0B9E5255C}']
  end;

  FBSDKAppLinkResolverRequestBuilder = interface(NSObject)
    ['{99067AA0-3E6D-4F67-A36C-2C43C838267F}']
    function getIdiomSpecificField: NSString; cdecl;
    function requestForURLs(urls: NSArray): FBSDKGraphRequest; cdecl;
  end;
  TFBSDKAppLinkResolverRequestBuilder = class(TOCGenericImport<FBSDKAppLinkResolverRequestBuilderClass, FBSDKAppLinkResolverRequestBuilder>) end;

  FBSDKGraphErrorRecoveryProcessorDelegate = interface(IObjectiveC)
    ['{EFF83A5B-A951-4112-AF1F-410442E8DD70}']
    procedure processorDidAttemptRecovery(processor: FBSDKGraphErrorRecoveryProcessor; didRecover: Boolean; error: NSError); cdecl;
    function processorWillProcessError(processor: FBSDKGraphErrorRecoveryProcessor; error: NSError): Boolean; cdecl;
  end;

  FBSDKGraphErrorRecoveryProcessorClass = interface(NSObjectClass)
    ['{36EC7F03-E16F-4CF0-B166-B0AEACFA2C5E}']
  end;

  FBSDKGraphErrorRecoveryProcessor = interface(NSObject)
    ['{6E2323FF-EB9C-4C1C-A155-8857C97A5818}']
    function delegate: Pointer; cdecl;
    procedure didPresentErrorWithRecovery(didRecover: Boolean; contextInfo: Pointer); cdecl;
    function processError(error: NSError; request: FBSDKGraphRequest; delegate: Pointer): Boolean; cdecl;
  end;
  TFBSDKGraphErrorRecoveryProcessor = class(TOCGenericImport<FBSDKGraphErrorRecoveryProcessorClass, FBSDKGraphErrorRecoveryProcessor>) end;

  FBSDKMutableCopying = interface(IObjectiveC)
    ['{03896F40-1439-41B8-8404-0A52AC6A6822}']
    function mutableCopy: Pointer; cdecl;
  end;

  FBSDKProfilePictureViewClass = interface(UIViewClass)
    ['{0AD56DFA-588C-41E9-B816-F8BCAFF3F3C6}']
  end;

  FBSDKProfilePictureView = interface(UIView)
    ['{ED3AD8D3-E6EF-41DB-9FAA-905A116FDB7D}']
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
    ['{DE408DBB-EAC5-4494-9799-4B12F272CABA}']
    {class} function currentProfile: FBSDKProfile; cdecl;
    {class} procedure enableUpdatesOnAccessTokenChange(enable: Boolean); cdecl;
    {class} procedure loadCurrentProfileWithCompletion(completion: FBSDKProfileBlock); cdecl;
    {class} function new: Pointer; cdecl;
    {class} procedure setCurrentProfile(currentProfile: FBSDKProfile); cdecl;
  end;

  FBSDKProfile = interface(NSObject)
    ['{55A4E8FF-5505-474A-A504-0B3C0E312456}']
    function email: NSString; cdecl;
    function firstName: NSString; cdecl;
    function imageURL: NSURL; cdecl;
    function imageURLForPictureMode(mode: FBSDKProfilePictureMode; size: CGSize): NSURL; cdecl;
    function initWithUserID(userID: NSString; firstName: NSString; middleName: NSString; lastName: NSString; name: NSString; linkURL: NSURL;
      refreshDate: NSDate): Pointer; overload; cdecl;
    function initWithUserID(userID: NSString; firstName: NSString; middleName: NSString; lastName: NSString; name: NSString; linkURL: NSURL;
      refreshDate: NSDate; imageURL: NSURL; email: NSString): Pointer; overload; cdecl;
    function isEqualToProfile(profile: FBSDKProfile): Boolean; cdecl;
    function lastName: NSString; cdecl;
    function linkURL: NSURL; cdecl;
    function middleName: NSString; cdecl;
    function name: NSString; cdecl;
    function refreshDate: NSDate; cdecl;
    function userID: NSString; cdecl;
  end;
  TFBSDKProfile = class(TOCGenericImport<FBSDKProfileClass, FBSDKProfile>) end;

const
  libFBSDKCoreKit = 'FBSDKCoreKit';

implementation

procedure FBSDKCoreLoader; cdecl; external framework libFBSDKCoreKit;

end.