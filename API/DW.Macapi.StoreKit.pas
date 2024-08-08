unit DW.Macapi.StoreKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit, Macapi.CoreGraphics;

const
  SKANErrorImpressionMissingRequiredValue = 0;
  SKANErrorUnsupported = 1;
  SKANErrorAdNetworkIdMissing = 2;
  SKANErrorMismatchedSourceAppId = 3;
  SKANErrorImpressionNotFound = 4;
  SKANErrorInvalidCampaignId = 5;
  SKANErrorInvalidConversionValue = 6;
  SKANErrorInvalidSourceAppId = 7;
  SKANErrorInvalidAdvertisedAppId = 8;
  SKANErrorInvalidVersion = 9;
  SKANErrorUnknown = 10;
  SKANErrorImpressionTooShort = 11;
  SKCloudServiceAuthorizationStatusNotDetermined = 0;
  SKCloudServiceAuthorizationStatusDenied = 1;
  SKCloudServiceAuthorizationStatusRestricted = 2;
  SKCloudServiceAuthorizationStatusAuthorized = 3;
  SKCloudServiceCapabilityNone = 0;
  SKCloudServiceCapabilityMusicCatalogPlayback = 1;
  SKCloudServiceCapabilityMusicCatalogSubscriptionEligible = 2;
  SKCloudServiceCapabilityAddToCloudMusicLibrary = 256;
  SKDownloadStateWaiting = 0;
  SKDownloadStateActive = 1;
  SKDownloadStatePaused = 2;
  SKDownloadStateFinished = 3;
  SKDownloadStateFailed = 4;
  SKDownloadStateCancelled = 5;
  SKErrorUnknown = 0;
  SKErrorClientInvalid = 1;
  SKErrorPaymentCancelled = 2;
  SKErrorPaymentInvalid = 3;
  SKErrorPaymentNotAllowed = 4;
  SKErrorStoreProductNotAvailable = 5;
  SKErrorCloudServicePermissionDenied = 6;
  SKErrorCloudServiceNetworkConnectionFailed = 7;
  SKErrorCloudServiceRevoked = 8;
  SKErrorPrivacyAcknowledgementRequired = 9;
  SKErrorUnauthorizedRequestData = 10;
  SKErrorInvalidOfferIdentifier = 11;
  SKErrorInvalidSignature = 12;
  SKErrorMissingOfferParams = 13;
  SKErrorInvalidOfferPrice = 14;
  SKErrorOverlayCancelled = 15;
  SKErrorOverlayInvalidConfiguration = 16;
  SKErrorOverlayTimeout = 17;
  SKErrorIneligibleForOffer = 18;
  SKErrorUnsupportedPlatform = 19;
  SKErrorOverlayPresentedInBackgroundScene = 20;
  SKOverlayPositionBottom = 0;
  SKOverlayPositionBottomRaised = 1;
  SKPaymentTransactionStatePurchasing = 0;
  SKPaymentTransactionStatePurchased = 1;
  SKPaymentTransactionStateFailed = 2;
  SKPaymentTransactionStateRestored = 3;
  SKPaymentTransactionStateDeferred = 4;
  SKProductPeriodUnitDay = 0;
  SKProductPeriodUnitWeek = 1;
  SKProductPeriodUnitMonth = 2;
  SKProductPeriodUnitYear = 3;
  SKProductDiscountPaymentModePayAsYouGo = 0;
  SKProductDiscountPaymentModePayUpFront = 1;
  SKProductDiscountPaymentModeFreeTrial = 2;
  SKProductDiscountTypeIntroductory = 0;
  SKProductDiscountTypeSubscription = 1;
  SKProductStorePromotionVisibilityDefault = 0;
  SKProductStorePromotionVisibilityShow = 1;
  SKProductStorePromotionVisibilityHide = 2;

type
  SKAdImpression = interface;
  SKAdNetwork = interface;
  SKArcadeService = interface;
  SKCloudServiceController = interface;
  SKCloudServiceSetupViewController = interface;
  SKCloudServiceSetupViewControllerDelegate = interface;
  SKDownload = interface;
  SKOverlayDelegate = interface;
  SKOverlay = interface;
  SKOverlayConfiguration = interface;
  SKOverlayAppConfiguration = interface;
  SKOverlayAppClipConfiguration = interface;
  SKOverlayTransitionContext = interface;
  SKPayment = interface;
  SKMutablePayment = interface;
  SKPaymentDiscount = interface;
  SKPaymentQueue = interface;
  SKPaymentQueueDelegate = interface;
  SKPaymentTransactionObserver = interface;
  SKPaymentTransaction = interface;
  SKProductSubscriptionPeriod = interface;
  SKProduct = interface;
  SKProductDiscount = interface;
  SKRequest = interface;
  SKRequestDelegate = interface;
  SKProductsRequestDelegate = interface;
  SKProductsRequest = interface;
  SKProductsResponse = interface;
  SKProductStorePromotionController = interface;
  SKReceiptRefreshRequest = interface;
  SKStorefront = interface;
  SKStoreProductViewController = interface;
  SKStoreProductViewControllerDelegate = interface;
  SKStoreReviewController = interface;

  SKAdNetworkCoarseConversionValue = NSString;
  SKANError = NSInteger;
  SKCloudServiceAuthorizationStatus = NSInteger;
  SKCloudServiceCapability = NSInteger;
  SKCloudServiceSetupOptionsKey = NSString;
  SKCloudServiceSetupAction = NSString;
  SKCloudServiceSetupMessageIdentifier = NSString;
  SKDownloadState = NSInteger;
  SKErrorCode = NSInteger;
  SKOverlayPosition = NSInteger;
  SKPaymentTransactionState = NSInteger;
  SKProductPeriodUnit = NSInteger;
  SKProductDiscountPaymentMode = NSInteger;
  SKProductDiscountType = NSInteger;
  SKProductStorePromotionVisibility = NSInteger;
  TSKAdNetworkBlockMethod1 = procedure(error: NSError) of object;
  TSKArcadeServiceBlockMethod1 = procedure(randomFromFP: NSData; randomFromFPLength: UInt32; cmacOfAppPID: NSData; cmacOfAppPIDLength: UInt32; error: NSError) of object;
  TSKArcadeServiceBlockMethod2 = procedure(subscriptionStatus: NSData; subscriptionStatusLength: UInt32; cmacOfNonce: NSData; cmacOfNonceLength: UInt32; error: NSError) of object;
  TSKCloudServiceControllerBlockMethod1 = procedure(authorizationStatus: SKCloudServiceAuthorizationStatus) of object;
  TSKCloudServiceControllerBlockMethod2 = procedure(capabilities: SKCloudServiceCapability; error: NSError) of object;
  TSKCloudServiceControllerBlockMethod3 = procedure(storefrontCountryCode: NSString; error: NSError) of object;
  TSKCloudServiceControllerBlockMethod4 = procedure(storefrontIdentifier: NSString; error: NSError) of object;
  TSKCloudServiceControllerBlockMethod5 = procedure(userToken: NSString; error: NSError) of object;
  TSKCloudServiceControllerBlockMethod6 = procedure(personalizationToken: NSString; error: NSError) of object;
  TSKCloudServiceSetupViewControllerBlockMethod1 = procedure(result: Boolean; error: NSError) of object;
  TSKOverlayTransitionContextBlockMethod1 = procedure of object;
  TSKProductStorePromotionControllerBlockMethod1 = procedure(storePromotionVisibility: SKProductStorePromotionVisibility; error: NSError) of object;
  TSKProductStorePromotionControllerBlockMethod2 = procedure(error: NSError) of object;
  TSKProductStorePromotionControllerBlockMethod3 = procedure(promotionOrder: NSArray; error: NSError) of object;
  TSKStoreProductViewControllerBlockMethod1 = procedure(result: Boolean; error: NSError) of object;

  SKAdImpressionClass = interface(NSObjectClass)
    ['{F141F35F-60DF-465B-AACF-513569685168}']
  end;

  SKAdImpression = interface(NSObject)
    ['{194AF4C1-1245-4149-8507-5EC2FCF45A34}']
    function adCampaignIdentifier: NSNumber; cdecl;
    function adDescription: NSString; cdecl;
    function adImpressionIdentifier: NSString; cdecl;
    function adNetworkIdentifier: NSString; cdecl;
    function adPurchaserName: NSString; cdecl;
    function adType: NSString; cdecl;
    function advertisedAppStoreItemIdentifier: NSNumber; cdecl;
    procedure setAdCampaignIdentifier(adCampaignIdentifier: NSNumber); cdecl;
    procedure setAdDescription(adDescription: NSString); cdecl;
    procedure setAdImpressionIdentifier(adImpressionIdentifier: NSString); cdecl;
    procedure setAdNetworkIdentifier(adNetworkIdentifier: NSString); cdecl;
    procedure setAdPurchaserName(adPurchaserName: NSString); cdecl;
    procedure setAdType(adType: NSString); cdecl;
    procedure setAdvertisedAppStoreItemIdentifier(advertisedAppStoreItemIdentifier: NSNumber); cdecl;
    procedure setSignature(signature: NSString); cdecl;
    procedure setSourceAppStoreItemIdentifier(sourceAppStoreItemIdentifier: NSNumber); cdecl;
    procedure setSourceIdentifier(sourceIdentifier: NSNumber); cdecl;
    procedure setTimestamp(timestamp: NSNumber); cdecl;
    procedure setVersion(version: NSString); cdecl;
    function signature: NSString; cdecl;
    function sourceAppStoreItemIdentifier: NSNumber; cdecl;
    function sourceIdentifier: NSNumber; cdecl;
    function timestamp: NSNumber; cdecl;
    function version: NSString; cdecl;
  end;
  TSKAdImpression = class(TOCGenericImport<SKAdImpressionClass, SKAdImpression>) end;

  SKAdNetworkClass = interface(NSObjectClass)
    ['{B74BE2CF-2E4D-4F2B-A2EC-8D499BBEACEA}']
    {class} procedure endImpression(impression: SKAdImpression; completionHandler: TSKAdNetworkBlockMethod1); cdecl;
    {class} procedure registerAppForAdNetworkAttribution; cdecl; // API_DEPRECATED("Use updatePostbackConversionValue:completionHandler: instead", ios(11.3, 15.4))
    {class} procedure startImpression(impression: SKAdImpression; completionHandler: TSKAdNetworkBlockMethod1); cdecl;
    {class} procedure updateConversionValue(conversionValue: NSInteger); cdecl; // API_DEPRECATED("Use updatePostbackConversionValue:completionHandler: instead", ios(14.0, 15.4))
    {class} procedure updatePostbackConversionValue(fineValue: NSInteger; coarseValue: SKAdNetworkCoarseConversionValue; lockWindow: Boolean;
      completionHandler: TSKAdNetworkBlockMethod1); overload; cdecl;
    {class} procedure updatePostbackConversionValue(fineValue: NSInteger; coarseValue: SKAdNetworkCoarseConversionValue;
      completionHandler: TSKAdNetworkBlockMethod1); overload; cdecl;
    {class} procedure updatePostbackConversionValue(conversionValue: NSInteger; completionHandler: TSKAdNetworkBlockMethod1); overload; cdecl;
  end;

  SKAdNetwork = interface(NSObject)
    ['{60D6A07D-59EC-4588-967F-BFFB318BC725}']
  end;
  TSKAdNetwork = class(TOCGenericImport<SKAdNetworkClass, SKAdNetwork>) end;

  SKArcadeServiceClass = interface(NSObjectClass)
    ['{7D6B8CAC-5CD1-4438-B8B6-26077F9B501B}']
    {class} procedure arcadeSubscriptionStatusWithNonce(nonce: UInt64; resultHandler: TSKArcadeServiceBlockMethod2); cdecl;
    {class} procedure registerArcadeAppWithRandomFromLib(randomFromLib: NSData; randomFromLibLength: UInt32;
      resultHandler: TSKArcadeServiceBlockMethod1); cdecl;
    {class} procedure repairArcadeApp; cdecl;
  end;

  SKArcadeService = interface(NSObject)
    ['{F0787AC6-A29E-43C3-A7FF-89A6E8239509}']
  end;
  TSKArcadeService = class(TOCGenericImport<SKArcadeServiceClass, SKArcadeService>) end;

  SKCloudServiceControllerClass = interface(NSObjectClass)
    ['{8AC05AA7-0D55-4834-A9FC-0A3523E7D50A}']
    {class} function authorizationStatus: SKCloudServiceAuthorizationStatus; cdecl;
    {class} procedure requestAuthorization(completionHandler: TSKCloudServiceControllerBlockMethod1); cdecl;
  end;

  SKCloudServiceController = interface(NSObject)
    ['{DD268213-1512-4883-A4C6-8BD2A634279F}']
    procedure requestCapabilitiesWithCompletionHandler(completionHandler: TSKCloudServiceControllerBlockMethod2); cdecl;
    procedure requestPersonalizationTokenForClientToken(clientToken: NSString; withCompletionHandler: TSKCloudServiceControllerBlockMethod6); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("requestUserTokenForDeveloperToken:completionHandler:", ios(10.3, 11.0), tvos(10.3, 11.0))
    procedure requestStorefrontCountryCodeWithCompletionHandler(completionHandler: TSKCloudServiceControllerBlockMethod3); cdecl;
    procedure requestStorefrontIdentifierWithCompletionHandler(completionHandler: TSKCloudServiceControllerBlockMethod4); cdecl;
    procedure requestUserTokenForDeveloperToken(developerToken: NSString; completionHandler: TSKCloudServiceControllerBlockMethod5); cdecl;
  end;
  TSKCloudServiceController = class(TOCGenericImport<SKCloudServiceControllerClass, SKCloudServiceController>) end;

  SKCloudServiceSetupViewControllerClass = interface(NSViewControllerClass)
    ['{5B02867B-FAC6-4FC9-9C2A-DEEBA18A60B4}']
  end;

  SKCloudServiceSetupViewController = interface(NSViewController)
    ['{0E5F35BE-9957-4715-95DE-17A0AD7F2864}']
    function delegate: Pointer; cdecl;
    procedure loadWithOptions(options: NSDictionary; completionHandler: TSKCloudServiceSetupViewControllerBlockMethod1); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKCloudServiceSetupViewController = class(TOCGenericImport<SKCloudServiceSetupViewControllerClass, SKCloudServiceSetupViewController>) end;

  SKCloudServiceSetupViewControllerDelegate = interface(IObjectiveC)
    ['{68716D96-EE6F-4B76-A1A8-F26F52BBC1BE}']
    procedure cloudServiceSetupViewControllerDidDismiss(cloudServiceSetupViewController: SKCloudServiceSetupViewController); cdecl;
  end;

  SKDownloadClass = interface(NSObjectClass)
    ['{5EB9140F-7377-4FDA-959F-EC06D4F20653}']
    {class} function contentURLForProductID(productID: NSString): NSURL; cdecl; // API_DEPRECATED("Hosted content is no longer supported", macos(10.8, 13.0), macCatalyst(13.0, 16.0))
    {class} procedure deleteContentForProductID(productID: NSString); cdecl; // API_DEPRECATED("Hosted content is no longer supported", macos(10.8, 13.0), macCatalyst(13.0, 16.0))
  end;

  SKDownload = interface(NSObject)
    ['{D0EAB426-C76F-448A-8E2E-FF5E35D2E8F6}']
    function contentIdentifier: NSString; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function contentLength: NSNumber; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[SKDownload expectedContentLength]", macos(10.8, 10.15))
    function contentURL: NSURL; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function contentVersion: NSString; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function downloadState: SKDownloadState; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[SKDownload state]", ios(6.0, 12.0))
    function error: NSError; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function expectedContentLength: Int64; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(13.0, 16.0), macos(10.15, 13.0), tvos(13.0, 16.0), watchos(6.2, 9.0))
    function progress: Single; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function state: SKDownloadState; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(12.0, 16.0), macos(10.8, 13.0), tvos(12.0, 16.0), watchos(6.2, 9.0))
    function timeRemaining: NSTimeInterval; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function transaction: SKPaymentTransaction; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.11, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
  end;
  TSKDownload = class(TOCGenericImport<SKDownloadClass, SKDownload>) end;

  SKOverlayDelegate = interface(IObjectiveC)
    ['{75C3FCEC-11CF-4344-AF8C-85AB2048C2DA}']
    procedure storeOverlay(overlay: SKOverlay; willStartPresentation: SKOverlayTransitionContext); overload; cdecl;
    procedure storeOverlay(overlay: SKOverlay; didFailToLoadWithError: NSError); overload; cdecl;
    [MethodName('storeOverlay:didFinishDismissal:')]
    procedure storeOverlayDidFinishDismissal(overlay: SKOverlay; didFinishDismissal: SKOverlayTransitionContext); cdecl;
    [MethodName('storeOverlay:didFinishPresentation:')]
    procedure storeOverlayDidFinishPresentation(overlay: SKOverlay; didFinishPresentation: SKOverlayTransitionContext); cdecl;
    [MethodName('storeOverlay:willStartDismissal:')]
    procedure storeOverlayWillStartDismissal(overlay: SKOverlay; willStartDismissal: SKOverlayTransitionContext); cdecl;
  end;

  SKOverlayClass = interface(NSObjectClass)
    ['{B5083AB0-0868-483C-AA1E-F2D8F5C227FE}']
    {class} procedure dismissOverlayInScene(scene: Pointer); cdecl;
    {class} function new: Pointer; cdecl;
  end;

  SKOverlay = interface(NSObject)
    ['{99A2DEA8-B4F1-4494-80AD-E5FA669AEE62}']
    function configuration: SKOverlayConfiguration; cdecl;
    function delegate: Pointer; cdecl;
    function initWithConfiguration(configuration: SKOverlayConfiguration): Pointer; cdecl;
    procedure presentInScene(scene: Pointer); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKOverlay = class(TOCGenericImport<SKOverlayClass, SKOverlay>) end;

  SKOverlayConfigurationClass = interface(NSObjectClass)
    ['{7C8001D9-65FC-4A91-9B9C-2FFB7C14EF23}']
    {class} function new: Pointer; cdecl;
  end;

  SKOverlayConfiguration = interface(NSObject)
    ['{61B65F33-5C94-43D4-9EA5-F59FA9C8DCFE}']
  end;
  TSKOverlayConfiguration = class(TOCGenericImport<SKOverlayConfigurationClass, SKOverlayConfiguration>) end;

  SKOverlayAppConfigurationClass = interface(SKOverlayConfigurationClass)
    ['{6BB4C608-B989-4540-9D18-3CB5B673D07E}']
    {class} function new: Pointer; cdecl;
  end;

  SKOverlayAppConfiguration = interface(SKOverlayConfiguration)
    ['{E8726F48-0523-48E9-A328-D360FE8FDA80}']
    function additionalValueForKey(key: NSString): Pointer; cdecl;
    function appIdentifier: NSString; cdecl;
    function campaignToken: NSString; cdecl;
    function customProductPageIdentifier: NSString; cdecl;
    function initWithAppIdentifier(appIdentifier: NSString; position: SKOverlayPosition): Pointer; cdecl;
    function latestReleaseID: NSString; cdecl;
    function position: SKOverlayPosition; cdecl;
    function providerToken: NSString; cdecl;
    procedure setAdditionalValue(value: Pointer; forKey: NSString); cdecl;
    procedure setAdImpression(impression: SKAdImpression); cdecl;
    procedure setAppIdentifier(appIdentifier: NSString); cdecl;
    procedure setCampaignToken(campaignToken: NSString); cdecl;
    procedure setCustomProductPageIdentifier(customProductPageIdentifier: NSString); cdecl;
    procedure setLatestReleaseID(latestReleaseID: NSString); cdecl;
    procedure setPosition(position: SKOverlayPosition); cdecl;
    procedure setProviderToken(providerToken: NSString); cdecl;
    procedure setUserDismissible(userDismissible: Boolean); cdecl;
    function userDismissible: Boolean; cdecl;
  end;
  TSKOverlayAppConfiguration = class(TOCGenericImport<SKOverlayAppConfigurationClass, SKOverlayAppConfiguration>) end;

  SKOverlayAppClipConfigurationClass = interface(SKOverlayConfigurationClass)
    ['{DE02CE5F-9E94-47E7-85E7-7329BB5BB656}']
    {class} function new: Pointer; cdecl;
  end;

  SKOverlayAppClipConfiguration = interface(SKOverlayConfiguration)
    ['{15EEA04A-7605-4BA9-8188-C5AABC419F33}']
    function additionalValueForKey(key: NSString): Pointer; cdecl;
    function campaignToken: NSString; cdecl;
    function customProductPageIdentifier: NSString; cdecl;
    function initWithPosition(position: SKOverlayPosition): Pointer; cdecl;
    function latestReleaseID: NSString; cdecl;
    function position: SKOverlayPosition; cdecl;
    function providerToken: NSString; cdecl;
    procedure setAdditionalValue(value: Pointer; forKey: NSString); cdecl;
    procedure setCampaignToken(campaignToken: NSString); cdecl;
    procedure setCustomProductPageIdentifier(customProductPageIdentifier: NSString); cdecl;
    procedure setLatestReleaseID(latestReleaseID: NSString); cdecl;
    procedure setPosition(position: SKOverlayPosition); cdecl;
    procedure setProviderToken(providerToken: NSString); cdecl;
  end;
  TSKOverlayAppClipConfiguration = class(TOCGenericImport<SKOverlayAppClipConfigurationClass, SKOverlayAppClipConfiguration>) end;

  SKOverlayTransitionContextClass = interface(NSObjectClass)
    ['{7020BE81-64D7-4CEE-A3A9-0D02AB15D864}']
    {class} function new: Pointer; cdecl;
  end;

  SKOverlayTransitionContext = interface(NSObject)
    ['{184E2CB8-53D2-4737-A389-56B34640C90A}']
    procedure addAnimationBlock(block: TSKOverlayTransitionContextBlockMethod1); cdecl;
    function endFrame: CGRect; cdecl;
    function startFrame: CGRect; cdecl;
  end;
  TSKOverlayTransitionContext = class(TOCGenericImport<SKOverlayTransitionContextClass, SKOverlayTransitionContext>) end;

  SKPaymentClass = interface(NSObjectClass)
    ['{CE851BA8-1DA8-4A62-8B9F-CA70AA12C3CF}']
    {class} function paymentWithProduct(product: SKProduct): Pointer; cdecl;
    {class} function paymentWithProductIdentifier(identifier: NSString): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+[SKPayment paymentWithProduct:]", ios(3.0, 5.0), macCatalyst(13.0, 13.0))
  end;

  SKPayment = interface(NSObject)
    ['{08667CE6-0AA1-486C-B9FF-626B92FED203}']
    function applicationUsername: NSString; cdecl;
    function paymentDiscount: SKPaymentDiscount; cdecl;
    function productIdentifier: NSString; cdecl;
    function quantity: NSInteger; cdecl;
    function requestData: NSData; cdecl;
    function simulatesAskToBuyInSandbox: Boolean; cdecl;
  end;
  TSKPayment = class(TOCGenericImport<SKPaymentClass, SKPayment>) end;

  SKMutablePaymentClass = interface(SKPaymentClass)
    ['{A19BA81E-CE32-483B-8C6A-A24E9D0A5753}']
  end;

  SKMutablePayment = interface(SKPayment)
    ['{EA23C926-9354-4C8A-B069-2FC9C815A267}']
    function applicationUsername: NSString; cdecl;
    function paymentDiscount: SKPaymentDiscount; cdecl;
    function productIdentifier: NSString; cdecl;
    function quantity: NSInteger; cdecl;
    function requestData: NSData; cdecl;
    procedure setApplicationUsername(applicationUsername: NSString); cdecl;
    procedure setPaymentDiscount(paymentDiscount: SKPaymentDiscount); cdecl;
    procedure setProductIdentifier(productIdentifier: NSString); cdecl;
    procedure setQuantity(quantity: NSInteger); cdecl;
    procedure setRequestData(requestData: NSData); cdecl;
    procedure setSimulatesAskToBuyInSandbox(simulatesAskToBuyInSandbox: Boolean); cdecl;
    function simulatesAskToBuyInSandbox: Boolean; cdecl;
  end;
  TSKMutablePayment = class(TOCGenericImport<SKMutablePaymentClass, SKMutablePayment>) end;

  SKPaymentDiscountClass = interface(NSObjectClass)
    ['{1F2A88C0-785D-48DB-A7A9-39192E9324EF}']
  end;

  SKPaymentDiscount = interface(NSObject)
    ['{475A98C5-6DFB-4DEE-8B85-9A08D8FEDBA5}']
    function identifier: NSString; cdecl;
    function initWithIdentifier(identifier: NSString; keyIdentifier: NSString; nonce: NSUUID; signature: NSString; timestamp: NSNumber): Pointer; cdecl;
    function keyIdentifier: NSString; cdecl;
    function nonce: NSUUID; cdecl;
    function signature: NSString; cdecl;
    function timestamp: NSNumber; cdecl;
  end;
  TSKPaymentDiscount = class(TOCGenericImport<SKPaymentDiscountClass, SKPaymentDiscount>) end;

  SKPaymentQueueClass = interface(NSObjectClass)
    ['{6CFD2170-227F-4AC1-959D-90A2130CEB89}']
    {class} function canMakePayments: Boolean; cdecl;
    {class} function defaultQueue: Pointer; cdecl;
  end;

  SKPaymentQueue = interface(NSObject)
    ['{E79E6403-AFD2-42CD-A031-96A458902E63}']
    procedure addPayment(payment: SKPayment); cdecl;
    procedure addTransactionObserver(observer: Pointer); cdecl;
    procedure cancelDownloads(downloads: NSArray); cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function delegate: Pointer; cdecl;
    procedure finishTransaction(transaction: SKPaymentTransaction); cdecl;
    procedure pauseDownloads(downloads: NSArray); cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    procedure presentCodeRedemptionSheet; cdecl;
    procedure removeTransactionObserver(observer: Pointer); cdecl;
    procedure restoreCompletedTransactions; cdecl;
    procedure restoreCompletedTransactionsWithApplicationUsername(username: NSString); cdecl;
    procedure resumeDownloads(downloads: NSArray); cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure showPriceConsentIfNeeded; cdecl;
    procedure startDownloads(downloads: NSArray); cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function storefront: SKStorefront; cdecl;
    function transactionObservers: NSArray; cdecl;
    function transactions: NSArray; cdecl;
  end;
  TSKPaymentQueue = class(TOCGenericImport<SKPaymentQueueClass, SKPaymentQueue>) end;

  SKPaymentQueueDelegate = interface(IObjectiveC)
    ['{5F97617B-F289-487D-9833-4471B4DF516E}']
    function paymentQueue(paymentQueue: SKPaymentQueue; shouldContinueTransaction: SKPaymentTransaction; inStorefront: SKStorefront): Boolean; cdecl;
    function paymentQueueShouldShowPriceConsent(paymentQueue: SKPaymentQueue): Boolean; cdecl;
  end;

  SKPaymentTransactionObserver = interface(IObjectiveC)
    ['{9C3B4990-03DA-472A-9D28-F917D8EA8DA2}']
    function paymentQueue(queue: SKPaymentQueue; shouldAddStorePayment: SKPayment; forProduct: SKProduct): Boolean; overload; cdecl;
    procedure paymentQueue(queue: SKPaymentQueue; restoreCompletedTransactionsFailedWithError: NSError); overload; cdecl;
    procedure paymentQueue(queue: SKPaymentQueue; updatedTransactions: NSArray); overload; cdecl;
    procedure paymentQueueDidChangeStorefront(queue: SKPaymentQueue); cdecl;
    [MethodName('paymentQueue:didRevokeEntitlementsForProductIdentifiers:')]
    procedure paymentQueueDidRevokeEntitlementsForProductIdentifiers(queue: SKPaymentQueue; didRevokeEntitlementsForProductIdentifiers: NSArray); cdecl;
    [MethodName('paymentQueue:removedTransactions:')]
    procedure paymentQueueRemovedTransactions(queue: SKPaymentQueue; removedTransactions: NSArray); cdecl;
    procedure paymentQueueRestoreCompletedTransactionsFinished(queue: SKPaymentQueue); cdecl;
    [MethodName('paymentQueue:updatedDownloads:')]
    procedure paymentQueueUpdatedDownloads(queue: SKPaymentQueue; updatedDownloads: NSArray); cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
  end;

  SKPaymentTransactionClass = interface(NSObjectClass)
    ['{7C12F5BD-62C3-4B0B-8F88-BF10123FD19C}']
  end;

  SKPaymentTransaction = interface(NSObject)
    ['{9CF32412-8957-4D62-B802-FE5DC89D8E17}']
    function downloads: NSArray; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function error: NSError; cdecl;
    function originalTransaction: SKPaymentTransaction; cdecl;
    function payment: SKPayment; cdecl;
    function transactionDate: NSDate; cdecl;
    function transactionIdentifier: NSString; cdecl;
    function transactionReceipt: NSData; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[NSBundle appStoreReceiptURL]", ios(3.0, 7.0), macCatalyst(13.0, 13.0))
    function transactionState: SKPaymentTransactionState; cdecl;
  end;
  TSKPaymentTransaction = class(TOCGenericImport<SKPaymentTransactionClass, SKPaymentTransaction>) end;

  SKProductSubscriptionPeriodClass = interface(NSObjectClass)
    ['{11ABD6CA-C0E5-4B9C-9889-FD00AE52786A}']
  end;

  SKProductSubscriptionPeriod = interface(NSObject)
    ['{218F525A-9C6A-40A6-AA64-7D1C14298B25}']
    function &unit: SKProductPeriodUnit; cdecl;
    function numberOfUnits: NSUInteger; cdecl;
  end;
  TSKProductSubscriptionPeriod = class(TOCGenericImport<SKProductSubscriptionPeriodClass, SKProductSubscriptionPeriod>) end;

  SKProductClass = interface(NSObjectClass)
    ['{DDA7A349-E700-43EE-9112-BD8C209A0F8F}']
  end;

  SKProduct = interface(NSObject)
    ['{B36E515A-0314-4E0D-9677-673EA4EBC18E}']
    function contentLengths: NSArray; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[SKProduct downloadContentLengths]", macos(10.8, 10.14))
    function contentVersion: NSString; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[SKProduct downloadContentVersion]", macos(10.8, 10.14))
    function discounts: NSArray; cdecl;
    function downloadable: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[SKProduct isDownloadable]", macos(10.8, 10.15))
    function downloadContentLengths: NSArray; cdecl;
    function downloadContentVersion: NSString; cdecl;
    function introductoryPrice: SKProductDiscount; cdecl;
    function isDownloadable: Boolean; cdecl;
    function isFamilyShareable: Boolean; cdecl;
    function localizedDescription: NSString; cdecl;
    function localizedTitle: NSString; cdecl;
    function price: NSDecimalNumber; cdecl;
    function priceLocale: NSLocale; cdecl;
    function productIdentifier: NSString; cdecl;
    function subscriptionGroupIdentifier: NSString; cdecl;
    function subscriptionPeriod: SKProductSubscriptionPeriod; cdecl;
  end;
  TSKProduct = class(TOCGenericImport<SKProductClass, SKProduct>) end;

  SKProductDiscountClass = interface(NSObjectClass)
    ['{33F3CE69-EB5C-4390-806D-A4A21168C3A9}']
  end;

  SKProductDiscount = interface(NSObject)
    ['{3DC33A26-BC2F-49B6-BB9B-3295377C54D0}']
    function &type: SKProductDiscountType; cdecl;
    function identifier: NSString; cdecl;
    function numberOfPeriods: NSUInteger; cdecl;
    function paymentMode: SKProductDiscountPaymentMode; cdecl;
    function price: NSDecimalNumber; cdecl;
    function priceLocale: NSLocale; cdecl;
    function subscriptionPeriod: SKProductSubscriptionPeriod; cdecl;
  end;
  TSKProductDiscount = class(TOCGenericImport<SKProductDiscountClass, SKProductDiscount>) end;

  SKRequestClass = interface(NSObjectClass)
    ['{551D8182-EDBD-4FFB-BEDD-AE6FD8DDF2C8}']
  end;

  SKRequest = interface(NSObject)
    ['{6679721A-D9E3-401D-B5D4-C375C4FBA1BC}']
    procedure cancel; cdecl;
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure start; cdecl;
  end;
  TSKRequest = class(TOCGenericImport<SKRequestClass, SKRequest>) end;

  SKRequestDelegate = interface(IObjectiveC)
    ['{AB171B3D-A198-4F02-A57A-47CB749A75E7}']
    procedure request(request: SKRequest; didFailWithError: NSError); cdecl;
    procedure requestDidFinish(request: SKRequest); cdecl;
  end;

  SKProductsRequestDelegate = interface(IObjectiveC)
    ['{B41423F7-0C0B-41C3-8809-F5C0FCCDABA1}']
    procedure productsRequest(request: SKProductsRequest; didReceiveResponse: SKProductsResponse); cdecl;
  end;

  SKProductsRequestClass = interface(SKRequestClass)
    ['{B82CB289-12D6-4652-867A-FE8491976E17}']
  end;

  SKProductsRequest = interface(SKRequest)
    ['{D2D87AF4-62D6-498F-8F16-723409DC08A4}']
    function delegate: Pointer; cdecl;
    function initWithProductIdentifiers(productIdentifiers: NSSet): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKProductsRequest = class(TOCGenericImport<SKProductsRequestClass, SKProductsRequest>) end;

  SKProductsResponseClass = interface(NSObjectClass)
    ['{FFAFA093-B18E-4A2B-88C1-545C9E6D24FF}']
  end;

  SKProductsResponse = interface(NSObject)
    ['{A4FCDCE3-8B15-40B5-914F-6A1DF362DBA4}']
    function invalidProductIdentifiers: NSArray; cdecl;
    function products: NSArray; cdecl;
  end;
  TSKProductsResponse = class(TOCGenericImport<SKProductsResponseClass, SKProductsResponse>) end;

  SKProductStorePromotionControllerClass = interface(NSObjectClass)
    ['{970A84F4-5EDE-4E47-BC01-C677E4C886DF}']
    {class} function defaultController: Pointer; cdecl;
  end;

  SKProductStorePromotionController = interface(NSObject)
    ['{A9F02059-BBB4-4631-BBD4-DC39C74B989C}']
    procedure fetchStorePromotionOrderWithCompletionHandler(completionHandler: TSKProductStorePromotionControllerBlockMethod3); cdecl;
    procedure fetchStorePromotionVisibilityForProduct(product: SKProduct; completionHandler: TSKProductStorePromotionControllerBlockMethod1); cdecl;
    procedure updateStorePromotionOrder(promotionOrder: NSArray; completionHandler: TSKProductStorePromotionControllerBlockMethod2); cdecl;
    procedure updateStorePromotionVisibility(promotionVisibility: SKProductStorePromotionVisibility; forProduct: SKProduct;
      completionHandler: TSKProductStorePromotionControllerBlockMethod2); cdecl;
  end;
  TSKProductStorePromotionController = class(TOCGenericImport<SKProductStorePromotionControllerClass, SKProductStorePromotionController>) end;

  SKReceiptRefreshRequestClass = interface(SKRequestClass)
    ['{A9425BA3-6E8F-4176-9AD8-1F9FD1A6BD9F}']
  end;

  SKReceiptRefreshRequest = interface(SKRequest)
    ['{EBF025DC-8730-48BD-9F04-06304E31423F}']
    function initWithReceiptProperties(properties: NSDictionary): Pointer; cdecl;
    function receiptProperties: NSDictionary; cdecl;
  end;
  TSKReceiptRefreshRequest = class(TOCGenericImport<SKReceiptRefreshRequestClass, SKReceiptRefreshRequest>) end;

  SKStorefrontClass = interface(NSObjectClass)
    ['{74CD78A6-3FEF-4025-8F1D-814A1FFBA758}']
  end;

  SKStorefront = interface(NSObject)
    ['{DBD3B2F9-56BD-42E3-8127-682DB41BFA52}']
    function countryCode: NSString; cdecl;
    function identifier: NSString; cdecl;
  end;
  TSKStorefront = class(TOCGenericImport<SKStorefrontClass, SKStorefront>) end;

  SKStoreProductViewControllerClass = interface(NSViewControllerClass)
    ['{D1AB8ACA-F058-4AE4-AC2F-6395451CB57C}']
  end;

  SKStoreProductViewController = interface(NSViewController)
    ['{F01D51A2-CF0C-43DA-8990-1E755F29B4EA}']
    function delegate: Pointer; cdecl;
    procedure loadProductWithParameters(parameters: NSDictionary; impression: SKAdImpression;
      completionBlock: TSKStoreProductViewControllerBlockMethod1); overload; cdecl;
    procedure loadProductWithParameters(parameters: NSDictionary; completionBlock: TSKStoreProductViewControllerBlockMethod1); overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKStoreProductViewController = class(TOCGenericImport<SKStoreProductViewControllerClass, SKStoreProductViewController>) end;

  SKStoreProductViewControllerDelegate = interface(IObjectiveC)
    ['{7D5E7661-A2F3-44F9-B7B6-1CD1BFD47BA0}']
    procedure productViewControllerDidFinish(viewController: SKStoreProductViewController); cdecl;
  end;

  SKStoreReviewControllerClass = interface(NSObjectClass)
    ['{434A651F-2210-4DF6-B9B6-258CFFD32F4A}']
    {class} procedure requestReview; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[SKStoreReviewController requestReviewInScene:]", ios(10.3, 14.0))
  end;

  SKStoreReviewController = interface(NSObject)
    ['{09081100-9EB3-4C65-99F6-F9B5E5455270}']
  end;
  TSKStoreReviewController = class(TOCGenericImport<SKStoreReviewControllerClass, SKStoreReviewController>) end;

function SKAdNetworkCoarseConversionValueHigh: SKAdNetworkCoarseConversionValue;
function SKAdNetworkCoarseConversionValueMedium: SKAdNetworkCoarseConversionValue;
function SKAdNetworkCoarseConversionValueLow: SKAdNetworkCoarseConversionValue;
function SKStoreProductParameterAdNetworkAttributionSignature: NSString;
function SKStoreProductParameterAdNetworkCampaignIdentifier: NSString;
function SKStoreProductParameterAdNetworkSourceIdentifier: NSString;
function SKStoreProductParameterAdNetworkIdentifier: NSString;
function SKStoreProductParameterAdNetworkNonce: NSString;
function SKStoreProductParameterAdNetworkTimestamp: NSString;
function SKStoreProductParameterAdNetworkSourceAppStoreIdentifier: NSString;
function SKStoreProductParameterAdNetworkVersion: NSString;
function SKANErrorDomain: NSString;
function SKCloudServiceCapabilitiesDidChangeNotification: NSNotificationName;
function SKStorefrontCountryCodeDidChangeNotification: NSNotificationName;
function SKStorefrontIdentifierDidChangeNotification: NSNotificationName;
function SKCloudServiceSetupOptionsActionKey: SKCloudServiceSetupOptionsKey;
function SKCloudServiceSetupOptionsITunesItemIdentifierKey: SKCloudServiceSetupOptionsKey;
function SKCloudServiceSetupOptionsAffiliateTokenKey: SKCloudServiceSetupOptionsKey;
function SKCloudServiceSetupOptionsCampaignTokenKey: SKCloudServiceSetupOptionsKey;
function SKCloudServiceSetupOptionsMessageIdentifierKey: SKCloudServiceSetupOptionsKey;
function SKCloudServiceSetupActionSubscribe: SKCloudServiceSetupAction;
function SKCloudServiceSetupMessageIdentifierJoin: SKCloudServiceSetupMessageIdentifier;
function SKCloudServiceSetupMessageIdentifierConnect: SKCloudServiceSetupMessageIdentifier;
function SKCloudServiceSetupMessageIdentifierAddMusic: SKCloudServiceSetupMessageIdentifier;
function SKCloudServiceSetupMessageIdentifierPlayMusic: SKCloudServiceSetupMessageIdentifier;
function SKDownloadTimeRemainingUnknown: NSTimeInterval;
function SKErrorDomain: NSString;
function SKReceiptPropertyIsExpired: NSString;
function SKReceiptPropertyIsRevoked: NSString;
function SKReceiptPropertyIsVolumePurchase: NSString;
function SKStoreProductParameterITunesItemIdentifier: NSString;
function SKStoreProductParameterProductIdentifier: NSString;
function SKStoreProductParameterCustomProductPageIdentifier: NSString;
function SKStoreProductParameterAffiliateToken: NSString;
function SKStoreProductParameterCampaignToken: NSString;
function SKStoreProductParameterProviderToken: NSString;
function SKStoreProductParameterAdvertisingPartnerToken: NSString;

const
  libStoreKit = '/System/Library/Frameworks/StoreKit.framework/StoreKit';

procedure SKTerminateForInvalidReceipt; cdecl;
  external libStoreKit name _PU + 'SKTerminateForInvalidReceipt';

implementation

uses
  System.SysUtils;

var
  StoreKitModule: THandle;

function SKAdNetworkCoarseConversionValueHigh: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKAdNetworkCoarseConversionValueHigh');
end;

function SKAdNetworkCoarseConversionValueMedium: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKAdNetworkCoarseConversionValueMedium');
end;

function SKAdNetworkCoarseConversionValueLow: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKAdNetworkCoarseConversionValueLow');
end;

function SKStoreProductParameterAdNetworkAttributionSignature: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdNetworkAttributionSignature');
end;

function SKStoreProductParameterAdNetworkCampaignIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdNetworkCampaignIdentifier');
end;

function SKStoreProductParameterAdNetworkSourceIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdNetworkSourceIdentifier');
end;

function SKStoreProductParameterAdNetworkIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdNetworkIdentifier');
end;

function SKStoreProductParameterAdNetworkNonce: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdNetworkNonce');
end;

function SKStoreProductParameterAdNetworkTimestamp: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdNetworkTimestamp');
end;

function SKStoreProductParameterAdNetworkSourceAppStoreIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdNetworkSourceAppStoreIdentifier');
end;

function SKStoreProductParameterAdNetworkVersion: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdNetworkVersion');
end;

function SKANErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKANErrorDomain');
end;

function SKCloudServiceCapabilitiesDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceCapabilitiesDidChangeNotification');
end;

function SKStorefrontCountryCodeDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStorefrontCountryCodeDidChangeNotification');
end;

function SKStorefrontIdentifierDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStorefrontIdentifierDidChangeNotification');
end;

function SKCloudServiceSetupOptionsActionKey: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsActionKey');
end;

function SKCloudServiceSetupOptionsITunesItemIdentifierKey: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsITunesItemIdentifierKey');
end;

function SKCloudServiceSetupOptionsAffiliateTokenKey: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsAffiliateTokenKey');
end;

function SKCloudServiceSetupOptionsCampaignTokenKey: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsCampaignTokenKey');
end;

function SKCloudServiceSetupOptionsMessageIdentifierKey: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsMessageIdentifierKey');
end;

function SKCloudServiceSetupActionSubscribe: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupActionSubscribe');
end;

function SKCloudServiceSetupMessageIdentifierJoin: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupMessageIdentifierJoin');
end;

function SKCloudServiceSetupMessageIdentifierConnect: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupMessageIdentifierConnect');
end;

function SKCloudServiceSetupMessageIdentifierAddMusic: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupMessageIdentifierAddMusic');
end;

function SKCloudServiceSetupMessageIdentifierPlayMusic: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupMessageIdentifierPlayMusic');
end;

function SKDownloadTimeRemainingUnknown: NSTimeInterval;
begin
  Result := CocoaDoubleConst(libStoreKit, 'SKDownloadTimeRemainingUnknown');
end;

function SKErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKErrorDomain');
end;

function SKReceiptPropertyIsExpired: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKReceiptPropertyIsExpired');
end;

function SKReceiptPropertyIsRevoked: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKReceiptPropertyIsRevoked');
end;

function SKReceiptPropertyIsVolumePurchase: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKReceiptPropertyIsVolumePurchase');
end;

function SKStoreProductParameterITunesItemIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterITunesItemIdentifier');
end;

function SKStoreProductParameterProductIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterProductIdentifier');
end;

function SKStoreProductParameterCustomProductPageIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterCustomProductPageIdentifier');
end;

function SKStoreProductParameterAffiliateToken: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAffiliateToken');
end;

function SKStoreProductParameterCampaignToken: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterCampaignToken');
end;

function SKStoreProductParameterProviderToken: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterProviderToken');
end;

function SKStoreProductParameterAdvertisingPartnerToken: NSString;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStoreProductParameterAdvertisingPartnerToken');
end;

initialization
  StoreKitModule := LoadLibrary(libStoreKit);

finalization
  if StoreKitModule <> 0 then
    FreeLibrary(StoreKitModule);

end.