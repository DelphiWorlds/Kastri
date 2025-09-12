unit DW.iOSapi.StoreKit;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics
  // DW
  {$IF CompilerVersion < 37}, DW.iOSapi.UIKit {$ENDIF};

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
  TSKArcadeServiceBlockMethod1 = procedure(randomFromFP: NSData; randomFromFPLength: UInt32; cmacOfAppPID: NSData; cmacOfAppPIDLength: UInt32;
    error: NSError) of object;
  TSKArcadeServiceBlockMethod2 = procedure(subscriptionStatus: NSData; subscriptionStatusLength: UInt32; cmacOfNonce: NSData;
    cmacOfNonceLength: UInt32; error: NSError) of object;
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
    ['{EDC97766-4659-4B21-A050-9E14B98E3796}']
  end;

  SKAdImpression = interface(NSObject)
    ['{1D09C1CF-506E-49BA-9F90-03DAFFFC7DE0}']
    function adCampaignIdentifier: NSNumber; cdecl;
    function adDescription: NSString; cdecl;
    function adImpressionIdentifier: NSString; cdecl;
    function adNetworkIdentifier: NSString; cdecl;
    function adPurchaserName: NSString; cdecl;
    function adType: NSString; cdecl;
    function advertisedAppStoreItemIdentifier: NSNumber; cdecl;
    function initWithSourceAppStoreItemIdentifier(sourceAppStoreItemIdentifier: NSNumber; advertisedAppStoreItemIdentifier: NSNumber;
      adNetworkIdentifier: NSString; adCampaignIdentifier: NSNumber; adImpressionIdentifier: NSString; timestamp: NSNumber; signature: NSString;
      version: NSString): Pointer; cdecl;
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
    ['{3ABA1D2B-B554-4BE9-801A-2F7286211A31}']
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
    ['{B3DC41D3-F60B-4735-9A9F-DC281DD7E7EB}']
  end;
  TSKAdNetwork = class(TOCGenericImport<SKAdNetworkClass, SKAdNetwork>) end;

  SKArcadeServiceClass = interface(NSObjectClass)
    ['{1953D4CD-4B78-418A-867A-20FB75CE2693}']
    {class} procedure arcadeSubscriptionStatusWithNonce(nonce: UInt64; resultHandler: TSKArcadeServiceBlockMethod2); cdecl;
    {class} procedure registerArcadeAppWithRandomFromLib(randomFromLib: NSData; randomFromLibLength: UInt32;
      resultHandler: TSKArcadeServiceBlockMethod1); cdecl;
    {class} procedure repairArcadeApp; cdecl;
  end;

  SKArcadeService = interface(NSObject)
    ['{4E6F5ACA-4D19-4967-88D7-F215426CF805}']
  end;
  TSKArcadeService = class(TOCGenericImport<SKArcadeServiceClass, SKArcadeService>) end;

  SKCloudServiceControllerClass = interface(NSObjectClass)
    ['{A424D819-A4F0-4FA7-B3E5-BE20B66D9BFF}']
    {class} function authorizationStatus: SKCloudServiceAuthorizationStatus; cdecl;
    {class} procedure requestAuthorization(completionHandler: TSKCloudServiceControllerBlockMethod1); cdecl;
  end;

  SKCloudServiceController = interface(NSObject)
    ['{1D4A9C69-D3EF-4F86-A789-D67900C66D82}']
    procedure requestCapabilitiesWithCompletionHandler(completionHandler: TSKCloudServiceControllerBlockMethod2); cdecl;
    procedure requestPersonalizationTokenForClientToken(clientToken: NSString; withCompletionHandler: TSKCloudServiceControllerBlockMethod6); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("requestUserTokenForDeveloperToken:completionHandler:", ios(10.3, 11.0), tvos(10.3, 11.0))
    procedure requestStorefrontCountryCodeWithCompletionHandler(completionHandler: TSKCloudServiceControllerBlockMethod3); cdecl;
    procedure requestStorefrontIdentifierWithCompletionHandler(completionHandler: TSKCloudServiceControllerBlockMethod4); cdecl;
    procedure requestUserTokenForDeveloperToken(developerToken: NSString; completionHandler: TSKCloudServiceControllerBlockMethod5); cdecl;
  end;
  TSKCloudServiceController = class(TOCGenericImport<SKCloudServiceControllerClass, SKCloudServiceController>) end;

  SKCloudServiceSetupViewControllerClass = interface(UIViewControllerClass)
    ['{8261AACE-4AD6-47D0-BBB5-F6389E9A6395}']
  end;

  SKCloudServiceSetupViewController = interface(UIViewController)
    ['{8A8BE663-1570-4EE6-ACB1-562B66F699C6}']
    function delegate: Pointer; cdecl;
    procedure loadWithOptions(options: NSDictionary; completionHandler: TSKCloudServiceSetupViewControllerBlockMethod1); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKCloudServiceSetupViewController = class(TOCGenericImport<SKCloudServiceSetupViewControllerClass, SKCloudServiceSetupViewController>) end;

  SKCloudServiceSetupViewControllerDelegate = interface(IObjectiveC)
    ['{AD46CAE0-F03D-49E4-957B-B1595C729B43}']
    procedure cloudServiceSetupViewControllerDidDismiss(cloudServiceSetupViewController: SKCloudServiceSetupViewController); cdecl;
  end;

  SKDownloadClass = interface(NSObjectClass)
    ['{697710E7-C56D-4F78-9908-64144B5C6B43}']
    {class} function contentURLForProductID(productID: NSString): NSURL; cdecl; // API_DEPRECATED("Hosted content is no longer supported", macos(10.8, 13.0), macCatalyst(13.0, 16.0))
    {class} procedure deleteContentForProductID(productID: NSString); cdecl; // API_DEPRECATED("Hosted content is no longer supported", macos(10.8, 13.0), macCatalyst(13.0, 16.0))
  end;

  SKDownload = interface(NSObject)
    ['{C701012C-4940-42D5-9E1F-B6BB85D071D7}']
    function contentIdentifier: NSString; cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    function contentLength: Int64; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[SKDownload expectedContentLength]", ios(6.0, 13.0), tvos(9.0, 13.0), macCatalyst(13.0, 13.0))
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
    ['{61D7D8A2-8C8C-4E76-AB96-3A40B0FC47C2}']
    [MethodName('storeOverlay:didFailToLoadWithError:')]
    procedure storeOverlayDidFailToLoadWithError(overlay: SKOverlay; didFailToLoadWithError: NSError); cdecl;
    [MethodName('storeOverlay:didFinishDismissal:')]
    procedure storeOverlayDidFinishDismissal(overlay: SKOverlay; didFinishDismissal: SKOverlayTransitionContext); cdecl;
    [MethodName('storeOverlay:didFinishPresentation:')]
    procedure storeOverlayDidFinishPresentation(overlay: SKOverlay; didFinishPresentation: SKOverlayTransitionContext); cdecl;
    [MethodName('storeOverlay:willStartDismissal:')]
    procedure storeOverlayWillStartDismissal(overlay: SKOverlay; willStartDismissal: SKOverlayTransitionContext); cdecl;
    [MethodName('storeOverlay:willStartPresentation:')]
    procedure storeOverlayWillStartPresentation(overlay: SKOverlay; willStartPresentation: SKOverlayTransitionContext); cdecl;
  end;

  SKOverlayClass = interface(NSObjectClass)
    ['{3639057E-935D-4A5D-9227-EDA321357AD4}']
    {class} procedure dismissOverlayInScene(scene: UIWindowScene); cdecl;
    {class} function new: Pointer; cdecl;
  end;

  SKOverlay = interface(NSObject)
    ['{69660B82-A320-4A69-B553-4C5D9782A084}']
    function configuration: SKOverlayConfiguration; cdecl;
    function delegate: Pointer; cdecl;
    function initWithConfiguration(configuration: SKOverlayConfiguration): Pointer; cdecl;
    procedure presentInScene(scene: UIWindowScene); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKOverlay = class(TOCGenericImport<SKOverlayClass, SKOverlay>) end;

  SKOverlayConfigurationClass = interface(NSObjectClass)
    ['{B13BC68B-E766-4EF3-972E-F3E11768F487}']
    {class} function new: Pointer; cdecl;
  end;

  SKOverlayConfiguration = interface(NSObject)
    ['{7486C3BC-31A1-4388-9820-412F625A9440}']
  end;
  TSKOverlayConfiguration = class(TOCGenericImport<SKOverlayConfigurationClass, SKOverlayConfiguration>) end;

  SKOverlayAppConfigurationClass = interface(SKOverlayConfigurationClass)
    ['{795ED000-7308-4866-BBBC-88661591D7DA}']
    {class} function new: Pointer; cdecl;
  end;

  SKOverlayAppConfiguration = interface(SKOverlayConfiguration)
    ['{5DE97755-A70D-42D2-9F4D-9FC313943A91}']
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
    ['{06F1CB5E-787E-43A8-A8B2-B2AFC8F9B1A8}']
    {class} function new: Pointer; cdecl;
  end;

  SKOverlayAppClipConfiguration = interface(SKOverlayConfiguration)
    ['{6904EC63-22C5-42DE-9134-999F96414E66}']
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
    ['{F7797344-3EFC-4A08-B96A-4E92202DC532}']
    {class} function new: Pointer; cdecl;
  end;

  SKOverlayTransitionContext = interface(NSObject)
    ['{5E6BA45A-02B9-4C38-8DFA-942EC1E6B46C}']
    procedure addAnimationBlock(block: TSKOverlayTransitionContextBlockMethod1); cdecl;
    function endFrame: CGRect; cdecl;
    function startFrame: CGRect; cdecl;
  end;
  TSKOverlayTransitionContext = class(TOCGenericImport<SKOverlayTransitionContextClass, SKOverlayTransitionContext>) end;

  SKPaymentClass = interface(NSObjectClass)
    ['{74695D19-43F2-45AD-9F7C-9F01D8567C40}']
    {class} function paymentWithProduct(product: SKProduct): Pointer; cdecl;
    {class} function paymentWithProductIdentifier(identifier: NSString): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+[SKPayment paymentWithProduct:]", ios(3.0, 5.0), macCatalyst(13.0, 13.0))
  end;

  SKPayment = interface(NSObject)
    ['{9A98B5CF-12F5-4F7B-B7DE-1F2C6C042635}']
    function applicationUsername: NSString; cdecl;
    function paymentDiscount: SKPaymentDiscount; cdecl;
    function productIdentifier: NSString; cdecl;
    function quantity: NSInteger; cdecl;
    function requestData: NSData; cdecl;
    function simulatesAskToBuyInSandbox: Boolean; cdecl;
  end;
  TSKPayment = class(TOCGenericImport<SKPaymentClass, SKPayment>) end;

  SKMutablePaymentClass = interface(SKPaymentClass)
    ['{33E1AA2A-6FDE-4DB1-A9CD-022484C674FD}']
  end;

  SKMutablePayment = interface(SKPayment)
    ['{1370D11D-455C-4298-BF28-F5CE1EBE8635}']
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
    ['{AA83FE2F-73DC-4E3F-88FC-9E4E2ED14BC5}']
  end;

  SKPaymentDiscount = interface(NSObject)
    ['{BBBDC21D-6661-42E9-998B-C71BC3823220}']
    function identifier: NSString; cdecl;
    function initWithIdentifier(identifier: NSString; keyIdentifier: NSString; nonce: NSUUID; signature: NSString; timestamp: NSNumber): Pointer; cdecl;
    function keyIdentifier: NSString; cdecl;
    function nonce: NSUUID; cdecl;
    function signature: NSString; cdecl;
    function timestamp: NSNumber; cdecl;
  end;
  TSKPaymentDiscount = class(TOCGenericImport<SKPaymentDiscountClass, SKPaymentDiscount>) end;

  SKPaymentQueueClass = interface(NSObjectClass)
    ['{AAB26414-E71A-4584-93D1-AFECD19F4DE1}']
    {class} function canMakePayments: Boolean; cdecl;
    {class} function defaultQueue: Pointer; cdecl;
  end;

  SKPaymentQueue = interface(NSObject)
    ['{0D0E8980-BB8A-43D3-BA96-55C02363011C}']
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
    ['{8321E604-45E8-4F2B-9B65-25D51661D549}']
    function paymentQueue(paymentQueue: SKPaymentQueue; shouldContinueTransaction: SKPaymentTransaction; inStorefront: SKStorefront): Boolean; cdecl;
    function paymentQueueShouldShowPriceConsent(paymentQueue: SKPaymentQueue): Boolean; cdecl;
  end;

  SKPaymentTransactionObserver = interface(IObjectiveC)
    ['{D151ABFC-5FB5-4E65-8D57-8428F615C29B}']
    procedure paymentQueueDidChangeStorefront(queue: SKPaymentQueue); cdecl;
    [MethodName('paymentQueue:didRevokeEntitlementsForProductIdentifiers:')]
    procedure paymentQueueDidRevokeEntitlementsForProductIdentifiers(queue: SKPaymentQueue;
      didRevokeEntitlementsForProductIdentifiers: NSArray); cdecl;
    [MethodName('paymentQueue:removedTransactions:')]
    procedure paymentQueueRemovedTransactions(queue: SKPaymentQueue; removedTransactions: NSArray); cdecl;
    [MethodName('paymentQueue:restoreCompletedTransactionsFailedWithError:')]
    procedure paymentQueueRestoreCompletedTransactionsFailedWithError(queue: SKPaymentQueue;
      restoreCompletedTransactionsFailedWithError: NSError); cdecl;
    procedure paymentQueueRestoreCompletedTransactionsFinished(queue: SKPaymentQueue); cdecl;
    [MethodName('paymentQueue:shouldAddStorePayment:forProduct:')]
    function paymentQueueShouldAddStorePayment(queue: SKPaymentQueue; shouldAddStorePayment: SKPayment; forProduct: SKProduct): Boolean; cdecl;
    [MethodName('paymentQueue:updatedDownloads:')]
    procedure paymentQueueUpdatedDownloads(queue: SKPaymentQueue; updatedDownloads: NSArray); cdecl; // API_DEPRECATED("Hosted content is no longer supported", ios(6.0, 16.0), macos(10.8, 13.0), tvos(9.0, 16.0), watchos(6.2, 9.0))
    [MethodName('paymentQueue:updatedTransactions:')]
    procedure paymentQueueUpdatedTransactions(queue: SKPaymentQueue; updatedTransactions: NSArray); cdecl;
  end;

  SKPaymentTransactionClass = interface(NSObjectClass)
    ['{94201B22-2DAB-4803-9A80-3F4CDC513FED}']
  end;

  SKPaymentTransaction = interface(NSObject)
    ['{86315BB3-1133-49E9-AE67-75B47CA6BFC5}']
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
    ['{D2186C40-EC64-464A-9492-23809DD20FEF}']
  end;

  SKProductSubscriptionPeriod = interface(NSObject)
    ['{4994D098-C961-4048-860D-E0FA426B6471}']
    function &unit: SKProductPeriodUnit; cdecl;
    function numberOfUnits: NSUInteger; cdecl;
  end;
  TSKProductSubscriptionPeriod = class(TOCGenericImport<SKProductSubscriptionPeriodClass, SKProductSubscriptionPeriod>) end;

  SKProductClass = interface(NSObjectClass)
    ['{305845AB-8F96-48BC-8A3A-5FC989206F02}']
  end;

  SKProduct = interface(NSObject)
    ['{8924A89B-1910-468B-A919-6F1E75B2E621}']
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
    ['{2F40A7AD-F5A1-4AB7-B2D3-2DFC4EB497BC}']
  end;

  SKProductDiscount = interface(NSObject)
    ['{A12042D0-DABC-4B32-A976-EA6C696120CA}']
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
    ['{A9354AFD-956F-4BD5-8FB2-9D100156685C}']
  end;

  SKRequest = interface(NSObject)
    ['{0B779E25-9847-4C59-BFFA-9514E4603276}']
    procedure cancel; cdecl;
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure start; cdecl;
  end;
  TSKRequest = class(TOCGenericImport<SKRequestClass, SKRequest>) end;

  SKRequestDelegate = interface(IObjectiveC)
    ['{D605EF71-A77A-44B6-8F3C-0BB6821A0005}']
    procedure request(request: SKRequest; didFailWithError: NSError); cdecl;
    procedure requestDidFinish(request: SKRequest); cdecl;
  end;

  SKProductsRequestDelegate = interface(IObjectiveC)
    ['{6C1B402D-00D6-4663-B693-0EACC7BBF0CB}']
    procedure productsRequest(request: SKProductsRequest; didReceiveResponse: SKProductsResponse); cdecl;
  end;

  SKProductsRequestClass = interface(SKRequestClass)
    ['{5BEDF72D-AD57-444D-BE39-2F1ACD6F65BB}']
  end;

  SKProductsRequest = interface(SKRequest)
    ['{EE45904B-307C-4A6D-8B30-4704D07AF075}']
    function delegate: Pointer; cdecl;
    function initWithProductIdentifiers(productIdentifiers: NSSet): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKProductsRequest = class(TOCGenericImport<SKProductsRequestClass, SKProductsRequest>) end;

  SKProductsResponseClass = interface(NSObjectClass)
    ['{C7A23B57-B88A-46C1-98BC-C47D8C779199}']
  end;

  SKProductsResponse = interface(NSObject)
    ['{FE91F243-0517-44D5-AE3A-49235C5F7C87}']
    function invalidProductIdentifiers: NSArray; cdecl;
    function products: NSArray; cdecl;
  end;
  TSKProductsResponse = class(TOCGenericImport<SKProductsResponseClass, SKProductsResponse>) end;

  SKProductStorePromotionControllerClass = interface(NSObjectClass)
    ['{00F4ECEA-2EF1-4293-B373-4C41B1DB6904}']
    {class} function defaultController: Pointer; cdecl;
  end;

  SKProductStorePromotionController = interface(NSObject)
    ['{C9052232-39D9-4405-A173-2F842C9CFB51}']
    procedure fetchStorePromotionOrderWithCompletionHandler(completionHandler: TSKProductStorePromotionControllerBlockMethod3); cdecl;
    procedure fetchStorePromotionVisibilityForProduct(product: SKProduct; completionHandler: TSKProductStorePromotionControllerBlockMethod1); cdecl;
    procedure updateStorePromotionOrder(promotionOrder: NSArray; completionHandler: TSKProductStorePromotionControllerBlockMethod2); cdecl;
    procedure updateStorePromotionVisibility(promotionVisibility: SKProductStorePromotionVisibility; forProduct: SKProduct;
      completionHandler: TSKProductStorePromotionControllerBlockMethod2); cdecl;
  end;
  TSKProductStorePromotionController = class(TOCGenericImport<SKProductStorePromotionControllerClass, SKProductStorePromotionController>) end;

  SKReceiptRefreshRequestClass = interface(SKRequestClass)
    ['{5E67C7A6-94E0-461F-9380-69CFAAB888C7}']
  end;

  SKReceiptRefreshRequest = interface(SKRequest)
    ['{904720FA-4F98-49DC-9CE5-A1D9EDE81F5B}']
    function initWithReceiptProperties(properties: NSDictionary): Pointer; cdecl;
    function receiptProperties: NSDictionary; cdecl;
  end;
  TSKReceiptRefreshRequest = class(TOCGenericImport<SKReceiptRefreshRequestClass, SKReceiptRefreshRequest>) end;

  SKStorefrontClass = interface(NSObjectClass)
    ['{B5E01871-50F1-4CE4-BEAA-E29B332BDE07}']
  end;

  SKStorefront = interface(NSObject)
    ['{FED8AC9A-49FB-492D-88B9-24F61162A5B1}']
    function countryCode: NSString; cdecl;
    function identifier: NSString; cdecl;
  end;
  TSKStorefront = class(TOCGenericImport<SKStorefrontClass, SKStorefront>) end;

  SKStoreProductViewControllerClass = interface(UIViewControllerClass)
    ['{2DDBD880-AF41-4DF3-965C-0BBB61D62DDD}']
  end;

  SKStoreProductViewController = interface(UIViewController)
    ['{ADFD19CA-CF5A-4380-B40B-127F36CC3086}']
    function delegate: Pointer; cdecl;
    procedure loadProductWithParameters(parameters: NSDictionary; impression: SKAdImpression;
      completionBlock: TSKStoreProductViewControllerBlockMethod1); overload; cdecl;
    procedure loadProductWithParameters(parameters: NSDictionary; completionBlock: TSKStoreProductViewControllerBlockMethod1); overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKStoreProductViewController = class(TOCGenericImport<SKStoreProductViewControllerClass, SKStoreProductViewController>) end;

  SKStoreProductViewControllerDelegate = interface(IObjectiveC)
    ['{B2896B8D-84BE-40F0-942C-1FC8E10C8BE8}']
    procedure productViewControllerDidFinish(viewController: SKStoreProductViewController); cdecl;
  end;

  SKStoreReviewControllerClass = interface(NSObjectClass)
    ['{B471AF4D-B754-4ED1-B6DB-60C90899AB32}']
    {class} procedure requestReview; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-[SKStoreReviewController requestReviewInScene:]", ios(10.3, 14.0))
    {class} procedure requestReviewInScene(windowScene: UIWindowScene); cdecl;
  end;

  SKStoreReviewController = interface(NSObject)
    ['{79812456-1F0B-4895-8E37-723124831F97}']
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
  // Posix
  Posix.Dlfcn;

var
  StoreKitModule: THandle;

function SKAdNetworkCoarseConversionValueHigh: SKAdNetworkCoarseConversionValue;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKAdNetworkCoarseConversionValueHigh');
end;

function SKAdNetworkCoarseConversionValueMedium: SKAdNetworkCoarseConversionValue;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKAdNetworkCoarseConversionValueMedium');
end;

function SKAdNetworkCoarseConversionValueLow: SKAdNetworkCoarseConversionValue;
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

function SKCloudServiceCapabilitiesDidChangeNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceCapabilitiesDidChangeNotification');
end;

function SKStorefrontCountryCodeDidChangeNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStorefrontCountryCodeDidChangeNotification');
end;

function SKStorefrontIdentifierDidChangeNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKStorefrontIdentifierDidChangeNotification');
end;

function SKCloudServiceSetupOptionsActionKey: SKCloudServiceSetupOptionsKey;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsActionKey');
end;

function SKCloudServiceSetupOptionsITunesItemIdentifierKey: SKCloudServiceSetupOptionsKey;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsITunesItemIdentifierKey');
end;

function SKCloudServiceSetupOptionsAffiliateTokenKey: SKCloudServiceSetupOptionsKey;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsAffiliateTokenKey');
end;

function SKCloudServiceSetupOptionsCampaignTokenKey: SKCloudServiceSetupOptionsKey;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsCampaignTokenKey');
end;

function SKCloudServiceSetupOptionsMessageIdentifierKey: SKCloudServiceSetupOptionsKey;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupOptionsMessageIdentifierKey');
end;

function SKCloudServiceSetupActionSubscribe: SKCloudServiceSetupAction;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupActionSubscribe');
end;

function SKCloudServiceSetupMessageIdentifierJoin: SKCloudServiceSetupMessageIdentifier;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupMessageIdentifierJoin');
end;

function SKCloudServiceSetupMessageIdentifierConnect: SKCloudServiceSetupMessageIdentifier;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupMessageIdentifierConnect');
end;

function SKCloudServiceSetupMessageIdentifierAddMusic: SKCloudServiceSetupMessageIdentifier;
begin
  Result := CocoaNSStringConst(libStoreKit, 'SKCloudServiceSetupMessageIdentifierAddMusic');
end;

function SKCloudServiceSetupMessageIdentifierPlayMusic: SKCloudServiceSetupMessageIdentifier;
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
  StoreKitModule := dlopen(MarshaledAString(libStoreKit), RTLD_LAZY);

finalization
  dlclose(StoreKitModule);

end.