
unit DW.iOSapi.GoogleMobileAds;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.WebKit
  // DW
  {$IF CompilerVersion < 37}, DW.iOSapi.UIKit {$ENDIF};

const
  GADAdChoicesPositionTopRightCorner = 0;
  GADAdChoicesPositionTopLeftCorner = 1;
  GADAdChoicesPositionBottomRightCorner = 2;
  GADAdChoicesPositionBottomLeftCorner = 3;
  GADAdFormatBanner = 0;
  GADAdFormatInterstitial = 1;
  GADAdFormatRewarded = 2;
  GADAdFormatNative = 3;
  GADAdFormatRewardedInterstitial = 4;
  GADAdFormatAppOpen = 6;
  GADAdValuePrecisionUnknown = 0;
  GADAdValuePrecisionEstimated = 1;
  GADAdValuePrecisionPublisherProvided = 2;
  GADAdValuePrecisionPrecise = 3;
  GADAdapterInitializationStateNotReady = 0;
  GADAdapterInitializationStateReady = 1;
  GADMediaAspectRatioUnknown = 0;
  GADMediaAspectRatioAny = 1;
  GADMediaAspectRatioLandscape = 2;
  GADMediaAspectRatioPortrait = 3;
  GADMediaAspectRatioSquare = 4;
  GADPublisherPrivacyPersonalizationStateDefault = 0;
  GADPublisherPrivacyPersonalizationStateEnabled = 1;
  GADPublisherPrivacyPersonalizationStateDisabled = 2;
  GADErrorInvalidRequest = 0;
  GADErrorNoFill = 1;
  GADErrorNetworkError = 2;
  GADErrorServerError = 3;
  GADErrorOSVersionTooLow = 4;
  GADErrorTimeout = 5;
  GADErrorMediationDataError = 7;
  GADErrorMediationAdapterError = 8;
  GADErrorMediationInvalidAdSize = 10;
  GADErrorInternalError = 11;
  GADErrorInvalidArgument = 12;
  GADErrorAdAlreadyUsed = 19;
  GADErrorApplicationIdentifierMissing = 20;
  GADErrorReceivedInvalidAdString = 21;
  GADPresentationErrorCodeAdNotReady = 15;
  GADPresentationErrorCodeAdTooLarge = 16;
  GADPresentationErrorCodeInternal = 17;
  GADPresentationErrorCodeAdAlreadyUsed = 18;
  GADPresentationErrorNotMainThread = 21;
  GADPresentationErrorMediation = 22;
  GADMBannerAnimationTypeNone = 0;
  GADMBannerAnimationTypeFlipFromLeft = 1;
  GADMBannerAnimationTypeFlipFromRight = 2;
  GADMBannerAnimationTypeCurlUp = 3;
  GADMBannerAnimationTypeCurlDown = 4;
  GADMBannerAnimationTypeSlideFromLeft = 5;
  GADMBannerAnimationTypeSlideFromRight = 6;
  GADMBannerAnimationTypeFadeIn = 7;
  GADMBannerAnimationTypeRandom = 8;
  kGADMBannerAnimationTypeNone = 0;
  kGADMBannerAnimationTypeFlipFromLeft = 1;
  kGADMBannerAnimationTypeFlipFromRight = 2;
  kGADMBannerAnimationTypeCurlUp = 3;
  kGADMBannerAnimationTypeCurlDown = 4;
  kGADMBannerAnimationTypeSlideFromLeft = 5;
  kGADMBannerAnimationTypeSlideFromRight = 6;
  kGADMBannerAnimationTypeFadeIn = 7;
  kGADMBannerAnimationTypeRandom = 8;
  kGADErrorInvalidRequest = 0;
  kGADErrorNoFill = 1;
  kGADErrorNetworkError = 2;
  kGADErrorServerError = 3;
  kGADErrorOSVersionTooLow = 4;
  kGADErrorTimeout = 5;
  kGADErrorInterstitialAlreadyUsed = 6;
  kGADErrorMediationDataError = 7;
  kGADErrorMediationAdapterError = 8;
  kGADErrorMediationInvalidAdSize = 10;
  kGADErrorInternalError = 11;
  kGADErrorInvalidArgument = 12;
  kGADErrorReceivedInvalidResponse = 13;
  kGADErrorRewardedAdAlreadyUsed = 14;
  kGADErrorMediationNoFill = 9;
  kGADErrorAdAlreadyUsed = 19;
  kGADErrorApplicationIdentifierMissing = 20;

type
  GADAdChoicesView = interface;
  GADAdLoaderDelegate = interface;
  GADAdNetworkExtras = interface;
  GADRequest = interface;
  GADAdLoaderOptions = interface;
  GADAdLoader = interface;
  GADAdMetadataProvider = interface;
  GADAdMetadataDelegate = interface;
  GADAdReward = interface;
  GADAdSizeDelegate = interface;
  GADAdValue = interface;
  GADAppEventDelegate = interface;
  GADFullScreenPresentingAd = interface;
  GADFullScreenContentDelegate = interface;
  GADAdNetworkResponseInfo = interface;
  GADResponseInfo = interface;
  GADAppOpenAd = interface;
  GADAudioVideoManagerDelegate = interface;
  GADAudioVideoManager = interface;
  GADBannerViewDelegate = interface;
  GADBannerView = interface;
  GADCustomEventBannerDelegate = interface;
  GADCustomEventRequest = interface;
  GADCustomEventBanner = interface;
  GADCustomEventExtras = interface;
  GADCustomEventInterstitialDelegate = interface;
  GADCustomEventInterstitial = interface;
  GADCustomEventNativeAd = interface;
  GADNativeAdImage = interface;
  GADMediatedUnifiedNativeAd = interface;
  GADCustomEventNativeAdDelegate = interface;
  GADDisplayAdMeasurement = interface;
  GADVideoController = interface;
  GADMediaContent = interface;
  GADMediaView = interface;
  GADCustomNativeAd = interface;
  GADCustomNativeAdLoaderDelegate = interface;
  GADCustomNativeAdDelegate = interface;
  GADDebugOptionsViewControllerDelegate = interface;
  GADDebugOptionsViewController = interface;
  GADExtras = interface;
  GADAdapterStatus = interface;
  GADInitializationStatus = interface;
  GADServerSideVerificationOptions = interface;
  GADInterstitialAd = interface;
  GADRequestConfiguration = interface;
  GADSignal = interface;
  GADSignalRequest = interface;
  GADMobileAds = interface;
  GADMultipleAdsAdLoaderOptions = interface;
  GADMuteThisAdReason = interface;
  GADNativeAdDelegate = interface;
  GADNativeAd = interface;
  GADNativeAdLoaderDelegate = interface;
  GADNativeAdView = interface;
  GADNativeAdUnconfirmedClickDelegate = interface;
  GADNativeAdCustomClickGestureOptions = interface;
  GADNativeAdImageAdLoaderOptions = interface;
  GADNativeAdMediaAdLoaderOptions = interface;
  GADNativeAdViewAdOptions = interface;
  GADNativeMuteThisAdLoaderOptions = interface;
  GADQueryInfo = interface;
  GADRewardedAd = interface;
  GADRewardedInterstitialAd = interface;
  GADVideoControllerDelegate = interface;
  GADVideoOptions = interface;
  GAMBannerAdLoaderDelegate = interface;
  GAMBannerView = interface;
  GAMBannerViewOptions = interface;
  GAMRequest = interface;
  GAMInterstitialAd = interface;
  GADMediationAdRequest = interface;
  GADMAdNetworkConnector = interface;
  GADMAdNetworkAdapter = interface;
  GADMediatedUnifiedNativeAdNotificationSource = interface;
  GADMediationAd = interface;
  GADMediationCredentials = interface;
  GADMediationServerConfiguration = interface;
  GADMediationAdConfiguration = interface;
  GADMediationAdEventDelegate = interface;
  GADMediationBannerAdEventDelegate = interface;
  GADMediationInterstitialAdEventDelegate = interface;
  GADMediationNativeAdEventDelegate = interface;
  GADMediationRewardedAdEventDelegate = interface;
  GADMediationAppOpenAdEventDelegate = interface;
  GADMediationAppOpenAd = interface;
  GADMediationAppOpenAdConfiguration = interface;
  GADMediationBannerAd = interface;
  GADMediationInterscrollerAd = interface;
  GADMediationBannerAdConfiguration = interface;
  GADMediationInterstitialAd = interface;
  GADMediationInterstitialAdConfiguration = interface;
  GADMediationNativeAd = interface;
  GADMediationNativeAdConfiguration = interface;
  GADMediationRewardedAd = interface;
  GADMediationRewardedAdConfiguration = interface;
  GADMediationAdapter = interface;
  GADRTBMediationSignalsConfiguration = interface;
  GADRTBRequestParameters = interface;
  GADRTBAdapter = interface;
  GADAppOpenSignalRequest = interface;
  GADBannerSignalRequest = interface;
  GADInterstitialSignalRequest = interface;
  GADNativeSignalRequest = interface;
  GADRewardedInterstitialSignalRequest = interface;
  GADRewardedSignalRequest = interface;

  PGADAdSize = ^GADAdSize;
  PGADVersionNumber = ^GADVersionNumber;

  GADAdSize = record
    size: CGSize;
    flags: NSUInteger;
  end;

  GADVersionNumber = record
    majorVersion: NSInteger;
    minorVersion: NSInteger;
    patchVersion: NSInteger;
  end;

  GADAdChoicesPosition = NSInteger;
  GADAdFormat = NSInteger;
  GADAdLoaderAdType = NSString;
  GADAdMetadataKey = NSString;

  GADUserDidEarnRewardHandler = procedure of object;
  GADAdValuePrecision = NSInteger;

  GADPaidEventHandler = procedure(value: GADAdValue) of object;

  GADAppOpenAdLoadCompletionHandler = procedure(appOpenAd: GADAppOpenAd; error: NSError) of object;
  GADNativeAssetIdentifier = NSString;

  GADNativeAdCustomClickHandler = procedure(assetID: NSString) of object;
  GADAdapterInitializationState = NSInteger;

  GADInterstitialAdLoadCompletionHandler = procedure(interstitialAd: GADInterstitialAd; error: NSError) of object;
  GADMediaAspectRatio = NSInteger;
  GADMaxAdContentRating = NSString;
  GADPublisherPrivacyPersonalizationState = NSInteger;

  GADInitializationCompletionHandler = procedure(status: GADInitializationStatus) of object;

  GADAdInspectorCompletionHandler = procedure(error: NSError) of object;

  GADSignalCompletionHandler = procedure(signal: GADSignal; error: NSError) of object;
  GADErrorCode = NSInteger;
  GADPresentationErrorCode = NSInteger;

  GADQueryInfoCreationCompletionHandler = procedure(queryInfo: GADQueryInfo; error: NSError) of object;

  GADRewardedAdLoadCompletionHandler = procedure(rewardedAd: GADRewardedAd; error: NSError) of object;

  GADRewardedInterstitialAdLoadCompletionHandler = procedure(rewardedInterstitialAd: GADRewardedInterstitialAd; error: NSError) of object;

  GAMInterstitialAdLoadCompletionHandler = procedure(interstitialAd: GAMInterstitialAd; error: NSError) of object;
  GADMBannerAnimationType = NSInteger;

  GADMediationBannerLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationInterscrollerAdLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationInterstitialLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationNativeLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationRewardedLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationAppOpenLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationAdapterSetUpCompletionBlock = procedure(error: NSError) of object;

  GADRTBSignalCompletionHandler = procedure(signals: NSString; error: NSError) of object;

  GADAdChoicesViewClass = interface(UIViewClass)
    ['{1236BA25-B023-40FF-BBEA-BBCFF7974830}']
  end;

  GADAdChoicesView = interface(UIView)
    ['{DDA90CEA-4310-4BD7-AF88-EF82E4B5AF35}']
  end;
  TGADAdChoicesView = class(TOCGenericImport<GADAdChoicesViewClass, GADAdChoicesView>) end;

  GADAdLoaderDelegate = interface(IObjectiveC)
    ['{AD076E08-8F47-4125-A1E2-A833D029C59C}']
    procedure adLoader(adLoader: GADAdLoader; didFailToReceiveAdWithError: NSError); cdecl;
    procedure adLoaderDidFinishLoading(adLoader: GADAdLoader); cdecl;
  end;

  GADAdNetworkExtras = interface(IObjectiveC)
    ['{B507994D-B82C-4EC1-AD98-C1A407A1E235}']
  end;

  GADRequestClass = interface(NSObjectClass)
    ['{145759D3-F67B-4889-9762-5E46A6002BE7}']
    {class} function request: Pointer; cdecl;
  end;

  GADRequest = interface(NSObject)
    ['{E0A662DA-9893-47E6-A2D2-36AD9F32CE56}']
    function adNetworkExtrasFor(aClass: Pointer): Pointer; cdecl;
    function adString: NSString; cdecl;
    function contentURL: NSString; cdecl;
    function customTargeting: NSDictionary; cdecl;
    function keywords: NSArray; cdecl;
    function neighboringContentURLStrings: NSArray; cdecl;
    function placementID: Int64; cdecl;
    procedure registerAdNetworkExtras(extras: Pointer); cdecl;
    procedure removeAdNetworkExtrasFor(aClass: Pointer); cdecl;
    function requestAgent: NSString; cdecl;
    function scene: UIWindowScene; cdecl;
    procedure setAdString(adString: NSString); cdecl;
    procedure setContentURL(contentURL: NSString); cdecl;
    procedure setCustomTargeting(customTargeting: NSDictionary); cdecl;
    procedure setKeywords(keywords: NSArray); cdecl;
    procedure setNeighboringContentURLStrings(neighboringContentURLStrings: NSArray); cdecl;
    procedure setPlacementID(placementID: Int64); cdecl;
    procedure setRequestAgent(requestAgent: NSString); cdecl;
    procedure setScene(scene: UIWindowScene); cdecl;
  end;
  TGADRequest = class(TOCGenericImport<GADRequestClass, GADRequest>) end;

  GADAdLoaderOptionsClass = interface(NSObjectClass)
    ['{3528ECDF-8573-4E0D-B1AF-5A9D5ECAF0CE}']
  end;

  GADAdLoaderOptions = interface(NSObject)
    ['{5B3B440E-CAF7-4402-ACED-0619527C8EB9}']
  end;
  TGADAdLoaderOptions = class(TOCGenericImport<GADAdLoaderOptionsClass, GADAdLoaderOptions>) end;

  GADAdLoaderClass = interface(NSObjectClass)
    ['{0E9B12DF-20A8-4DE4-BAE3-D33CE98A7AB6}']
  end;

  GADAdLoader = interface(NSObject)
    ['{38A9F51E-BFC1-4B69-ACE1-04CC38450381}']
    function adUnitID: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function initWithAdUnitID(adUnitID: NSString; rootViewController: UIViewController; adTypes: NSArray; options: NSArray): Pointer; cdecl;
    function initWithRootViewController(rootViewController: UIViewController): Pointer; cdecl;
    function isLoading: Boolean; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    procedure loadWithAdResponseString(adResponseString: NSString); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADAdLoader = class(TOCGenericImport<GADAdLoaderClass, GADAdLoader>) end;

  GADAdMetadataProvider = interface(IObjectiveC)
    ['{CAEC5FDC-5C20-40A9-9540-E225378D0732}']
    function adMetadata: NSDictionary; cdecl;
    function adMetadataDelegate: Pointer; cdecl;
    procedure setAdMetadataDelegate(adMetadataDelegate: Pointer); cdecl;
  end;

  GADAdMetadataDelegate = interface(IObjectiveC)
    ['{17985274-2CD8-48FB-8AA3-224E58B46485}']
    procedure adMetadataDidChange(ad: Pointer); cdecl;
  end;

  GADAdRewardClass = interface(NSObjectClass)
    ['{331568D2-C043-48EE-AB61-B38A5E6335B0}']
  end;

  GADAdReward = interface(NSObject)
    ['{C9C94844-71FC-4C75-9E0A-A2866F78F491}']
    function &type: NSString; cdecl;
    function amount: NSDecimalNumber; cdecl;
    function initWithRewardType(rewardType: NSString; rewardAmount: NSDecimalNumber): Pointer; cdecl;
  end;
  TGADAdReward = class(TOCGenericImport<GADAdRewardClass, GADAdReward>) end;

  GADAdSizeDelegate = interface(IObjectiveC)
    ['{A2BD535C-422B-4B13-B176-558016F19431}']
    procedure adView(bannerView: GADBannerView; willChangeAdSizeTo: GADAdSize); cdecl;
  end;

  GADAdValueClass = interface(NSObjectClass)
    ['{3B676751-033D-4FA2-9C4F-20BD9C6AFA9B}']
  end;

  GADAdValue = interface(NSObject)
    ['{43B70515-0A85-4286-BABF-8E366C8B16AD}']
    function currencyCode: NSString; cdecl;
    function precision: GADAdValuePrecision; cdecl;
    function value: NSDecimalNumber; cdecl;
  end;
  TGADAdValue = class(TOCGenericImport<GADAdValueClass, GADAdValue>) end;

  GADAppEventDelegate = interface(IObjectiveC)
    ['{611DE2AE-E272-411E-B1E6-B51D09B40D8B}']
    procedure adView(banner: GADBannerView; didReceiveAppEvent: NSString; withInfo: NSString); cdecl;
    procedure interstitialAd(interstitialAd: GADInterstitialAd; didReceiveAppEvent: NSString; withInfo: NSString); cdecl;
  end;

  GADFullScreenPresentingAd = interface(IObjectiveC)
    ['{1E1437B3-5413-40CC-A820-6D1D5BC9CC78}']
    function fullScreenContentDelegate: Pointer; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
  end;

  GADFullScreenContentDelegate = interface(IObjectiveC)
    ['{DEBAE99A-38C8-4F87-AF49-DD33D706668B}']
    procedure ad(ad: Pointer; didFailToPresentFullScreenContentWithError: NSError); cdecl;
    procedure adDidDismissFullScreenContent(ad: Pointer); cdecl;
    procedure adDidPresentFullScreenContent(ad: Pointer); cdecl;
    procedure adDidRecordClick(ad: Pointer); cdecl;
    procedure adDidRecordImpression(ad: Pointer); cdecl;
    procedure adWillDismissFullScreenContent(ad: Pointer); cdecl;
    procedure adWillPresentFullScreenContent(ad: Pointer); cdecl;
  end;

  GADAdNetworkResponseInfoClass = interface(NSObjectClass)
    ['{28C92765-3E1B-4EDA-B873-A6D227AEC9C8}']
  end;

  GADAdNetworkResponseInfo = interface(NSObject)
    ['{804032BA-553E-401D-B8AA-E581B71DF01C}']
    function adNetworkClassName: NSString; cdecl;
    function adSourceID: NSString; cdecl;
    function adSourceInstanceID: NSString; cdecl;
    function adSourceInstanceName: NSString; cdecl;
    function adSourceName: NSString; cdecl;
    function adUnitMapping: NSDictionary; cdecl;
    function dictionaryRepresentation: NSDictionary; cdecl;
    function error: NSError; cdecl;
    function latency: NSTimeInterval; cdecl;
  end;
  TGADAdNetworkResponseInfo = class(TOCGenericImport<GADAdNetworkResponseInfoClass, GADAdNetworkResponseInfo>) end;

  GADResponseInfoClass = interface(NSObjectClass)
    ['{21F8F2C3-4861-41B1-BA27-B02322D2F71F}']
  end;

  GADResponseInfo = interface(NSObject)
    ['{BD73ACE4-A86B-4BB2-B708-5882778B90E7}']
    function adNetworkInfoArray: NSArray; cdecl;
    function dictionaryRepresentation: NSDictionary; cdecl;
    function extrasDictionary: NSDictionary; cdecl;
    function loadedAdNetworkResponseInfo: GADAdNetworkResponseInfo; cdecl;
    function responseIdentifier: NSString; cdecl;
  end;
  TGADResponseInfo = class(TOCGenericImport<GADResponseInfoClass, GADResponseInfo>) end;

  GADAppOpenAdClass = interface(NSObjectClass)
    ['{9C44651D-1B2A-46CE-B75D-BB794C62E676}']
    {class} procedure loadWithAdResponseString(adResponseString: NSString; completionHandler: GADAppOpenAdLoadCompletionHandler); cdecl;
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; completionHandler: GADAppOpenAdLoadCompletionHandler); cdecl;
  end;

  GADAppOpenAd = interface(NSObject)
    ['{A5C32F79-1AF2-43C9-ABBC-CF61453FBDB6}']
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PPointer): Boolean; cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function placementID: Int64; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setPlacementID(placementID: Int64); cdecl;
  end;
  TGADAppOpenAd = class(TOCGenericImport<GADAppOpenAdClass, GADAppOpenAd>) end;

  GADAudioVideoManagerDelegate = interface(IObjectiveC)
    ['{41926002-B5A8-482C-A476-DFD326EE89F9}']
    procedure audioVideoManagerDidPauseAllVideo(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerDidStopPlayingAudio(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerWillPlayAudio(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerWillPlayVideo(audioVideoManager: GADAudioVideoManager); cdecl;
  end;

  GADAudioVideoManagerClass = interface(NSObjectClass)
    ['{DF4487CD-1249-4920-8774-5229AADD24A4}']
  end;

  GADAudioVideoManager = interface(NSObject)
    ['{35CC97BC-1677-44EF-9769-028A0EB68664}']
    function audioSessionIsApplicationManaged: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    procedure setAudioSessionIsApplicationManaged(audioSessionIsApplicationManaged: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADAudioVideoManager = class(TOCGenericImport<GADAudioVideoManagerClass, GADAudioVideoManager>) end;

  GADBannerViewDelegate = interface(IObjectiveC)
    ['{DD6990BB-CFA9-477F-83D8-454C3037ED5E}']
    procedure bannerView(bannerView: GADBannerView; didFailToReceiveAdWithError: NSError); cdecl;
    procedure bannerViewDidDismissScreen(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidReceiveAd(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidRecordClick(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidRecordImpression(bannerView: GADBannerView); cdecl;
    procedure bannerViewWillDismissScreen(bannerView: GADBannerView); cdecl;
    procedure bannerViewWillPresentScreen(bannerView: GADBannerView); cdecl;
  end;

  GADBannerViewClass = interface(UIViewClass)
    ['{D218A307-6EF6-446F-B8C5-F6679AA6BC32}']
  end;

  GADBannerView = interface(UIView)
    ['{DED108A4-A8F9-48A0-B5C7-1DE63948DB58}']
    function adSize: GADAdSize; cdecl;
    function adSizeDelegate: Pointer; cdecl;
    function adUnitID: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function initWithAdSize(adSize: GADAdSize; origin: CGPoint): Pointer; overload; cdecl;
    function initWithAdSize(adSize: GADAdSize): Pointer; overload; cdecl;
    function isAutoloadEnabled: Boolean; cdecl;
    function isCollapsible: Boolean; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    procedure loadWithAdResponseString(adResponseString: NSString); cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function placementID: Int64; cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setAdSize(adSize: GADAdSize); cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    procedure setAutoloadEnabled(autoloadEnabled: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setPlacementID(placementID: Int64); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
  end;
  TGADBannerView = class(TOCGenericImport<GADBannerViewClass, GADBannerView>) end;

  GADCustomEventBannerDelegate = interface(IObjectiveC)
    ['{9CD64841-39DF-4534-9690-A20B4351F989}']
    procedure customEventBanner(customEvent: Pointer; didFailAd: NSError); overload; cdecl;
    procedure customEventBanner(customEvent: Pointer; didReceiveAd: UIView); overload; cdecl;
    [MethodName('customEventBanner:clickDidOccurInAd:')]
    procedure customEventBannerClickDidOccurInAd(customEvent: Pointer; clickDidOccurInAd: UIView); cdecl;
    procedure customEventBannerDidDismissModal(customEvent: Pointer); cdecl;
    procedure customEventBannerWasClicked(customEvent: Pointer); cdecl;
    procedure customEventBannerWillDismissModal(customEvent: Pointer); cdecl;
    procedure customEventBannerWillLeaveApplication(customEvent: Pointer); cdecl;
    procedure customEventBannerWillPresentModal(customEvent: Pointer); cdecl;
    function viewControllerForPresentingModalView: UIViewController; cdecl;
  end;

  GADCustomEventRequestClass = interface(NSObjectClass)
    ['{B5BCC55C-B7DD-430B-B724-354651FEAF46}']
  end;

  GADCustomEventRequest = interface(NSObject)
    ['{76A8BEDE-7B26-49CD-8783-5DD086166415}']
    function additionalParameters: NSDictionary; cdecl;
    function isTesting: Boolean; cdecl;
    function userKeywords: NSArray; cdecl;
  end;
  TGADCustomEventRequest = class(TOCGenericImport<GADCustomEventRequestClass, GADCustomEventRequest>) end;

  GADCustomEventBanner = interface(IObjectiveC)
    ['{6EAFF8B1-5C27-4C11-AA8E-BA1ACF6E275B}']
    function delegate: Pointer; cdecl;
    function init: Pointer; cdecl;
    procedure requestBannerAd(adSize: GADAdSize; parameter: NSString; &label: NSString; request: GADCustomEventRequest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADCustomEventExtrasClass = interface(NSObjectClass)
    ['{8FC63CDB-E1C1-4921-9166-F0DD159D43C8}']
  end;

  GADCustomEventExtras = interface(NSObject)
    ['{CA2EE9E8-24E1-4B04-B439-301B380DF8CA}']
    function allExtras: NSDictionary; cdecl;
    function extrasForLabel(&label: NSString): NSDictionary; cdecl;
    procedure removeAllExtras; cdecl;
    procedure setExtras(extras: NSDictionary; forLabel: NSString); cdecl;
  end;
  TGADCustomEventExtras = class(TOCGenericImport<GADCustomEventExtrasClass, GADCustomEventExtras>) end;

  GADCustomEventInterstitialDelegate = interface(IObjectiveC)
    ['{E36705B8-B329-4CFC-AF83-35E0EA86C723}']
    procedure customEventInterstitial(customEvent: Pointer; didReceiveAd: NSObject); overload; cdecl;
    procedure customEventInterstitial(customEvent: Pointer; didFailAd: NSError); overload; cdecl;
    procedure customEventInterstitialDidDismiss(customEvent: Pointer); cdecl;
    procedure customEventInterstitialDidReceiveAd(customEvent: Pointer); cdecl;
    procedure customEventInterstitialWasClicked(customEvent: Pointer); cdecl;
    procedure customEventInterstitialWillDismiss(customEvent: Pointer); cdecl;
    procedure customEventInterstitialWillLeaveApplication(customEvent: Pointer); cdecl;
    procedure customEventInterstitialWillPresent(customEvent: Pointer); cdecl;
  end;

  GADCustomEventInterstitial = interface(IObjectiveC)
    ['{3F0E35B7-9796-415B-8102-BBD429F9ED8A}']
    function delegate: Pointer; cdecl;
    function init: Pointer; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
    procedure requestInterstitialAdWithParameter(serverParameter: NSString; &label: NSString; request: GADCustomEventRequest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADCustomEventNativeAd = interface(IObjectiveC)
    ['{7A238D02-9DB5-4D48-9F9B-515E3AFB69A1}']
    function delegate: Pointer; cdecl;
    function handlesUserClicks: Boolean; cdecl;
    function handlesUserImpressions: Boolean; cdecl;
    function init: Pointer; cdecl;
    procedure requestNativeAdWithParameter(serverParameter: NSString; request: GADCustomEventRequest; adTypes: NSArray; options: NSArray; rootViewController: UIViewController); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADNativeAdImageClass = interface(NSObjectClass)
    ['{CBB7D15C-3525-473C-836B-191CE10375A2}']
  end;

  GADNativeAdImage = interface(NSObject)
    ['{90B1C173-DE8E-4529-B69F-8655F6ACAE84}']
    function image: UIImage; cdecl;
    function imageURL: NSURL; cdecl;
    function initWithImage(image: UIImage): Pointer; cdecl;
    function initWithURL(URL: NSURL; scale: CGFloat): Pointer; cdecl;
    function scale: CGFloat; cdecl;
  end;
  TGADNativeAdImage = class(TOCGenericImport<GADNativeAdImageClass, GADNativeAdImage>) end;

  GADMediatedUnifiedNativeAd = interface(IObjectiveC)
    ['{531BF539-56DE-443C-A5CD-186B6CD39DDE}']
    function adChoicesView: UIView; cdecl;
    function advertiser: NSString; cdecl;
    function body: NSString; cdecl;
    function callToAction: NSString; cdecl;
    function currentTime: NSTimeInterval; cdecl;
    procedure didRecordClickOnAssetWithName(assetName: GADNativeAssetIdentifier; view: UIView; viewController: UIViewController); cdecl;
    procedure didRecordImpression; cdecl;
    procedure didRenderInView(view: UIView; clickableAssetViews: NSDictionary; nonclickableAssetViews: NSDictionary; viewController: UIViewController); cdecl;
    procedure didUntrackView(view: UIView); cdecl;
    function duration: NSTimeInterval; cdecl;
    function extraAssets: NSDictionary; cdecl;
    function hasVideoContent: Boolean; cdecl;
    function headline: NSString; cdecl;
    function icon: GADNativeAdImage; cdecl;
    function images: NSArray; cdecl;
    function mediaContentAspectRatio: CGFloat; cdecl;
    function mediaView: UIView; cdecl;
    function price: NSString; cdecl;
    function starRating: NSDecimalNumber; cdecl;
    function store: NSString; cdecl;
  end;

  GADCustomEventNativeAdDelegate = interface(IObjectiveC)
    ['{D69C0E6C-FAC7-469D-97EC-B5B5A9922DA9}']
    procedure customEventNativeAd(customEventNativeAd: Pointer; didFailToLoadWithError: NSError); overload; cdecl;
    procedure customEventNativeAd(customEventNativeAd: Pointer; didReceiveMediatedUnifiedNativeAd: Pointer); overload; cdecl;
  end;

  GADDisplayAdMeasurementClass = interface(NSObjectClass)
    ['{C6518C04-FC8B-46DA-9778-9AD0BB5B1503}']
  end;

  GADDisplayAdMeasurement = interface(NSObject)
    ['{A4997730-EDF1-4792-8C70-B9D1A0D5127C}']
    procedure setView(view: UIView); cdecl;
    function startWithError(error: PPointer): Boolean; cdecl;
    function view: UIView; cdecl;
  end;
  TGADDisplayAdMeasurement = class(TOCGenericImport<GADDisplayAdMeasurementClass, GADDisplayAdMeasurement>) end;

  GADVideoControllerClass = interface(NSObjectClass)
    ['{EC7E191D-8E53-4386-B7B0-D76F0874FDA1}']
  end;

  GADVideoController = interface(NSObject)
    ['{40E70371-C4CB-48D1-96A9-5990CA7A06B6}']
    function areCustomControlsEnabled: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function isClickToExpandEnabled: Boolean; cdecl;
    function isMuted: Boolean; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMuted(muted: Boolean); cdecl;
    procedure stop; cdecl;
  end;
  TGADVideoController = class(TOCGenericImport<GADVideoControllerClass, GADVideoController>) end;

  GADMediaContentClass = interface(NSObjectClass)
    ['{A745D152-9E39-4192-83F3-47D91B9794BD}']
  end;

  GADMediaContent = interface(NSObject)
    ['{C5D38DDB-3971-45D0-A5C9-BFE8C1D695C5}']
    function aspectRatio: CGFloat; cdecl;
    function currentTime: NSTimeInterval; cdecl;
    function duration: NSTimeInterval; cdecl;
    function hasVideoContent: Boolean; cdecl;
    function mainImage: UIImage; cdecl;
    procedure setMainImage(mainImage: UIImage); cdecl;
    function videoController: GADVideoController; cdecl;
  end;
  TGADMediaContent = class(TOCGenericImport<GADMediaContentClass, GADMediaContent>) end;

  GADMediaViewClass = interface(UIViewClass)
    ['{38796C2E-439B-4A4E-B7DC-79BCF6DDB0C4}']
  end;

  GADMediaView = interface(UIView)
    ['{669AA125-D653-4A61-A442-EA386E31697C}']
    function mediaContent: GADMediaContent; cdecl;
    procedure setMediaContent(mediaContent: GADMediaContent); cdecl;
  end;
  TGADMediaView = class(TOCGenericImport<GADMediaViewClass, GADMediaView>) end;

  GADCustomNativeAdClass = interface(NSObjectClass)
    ['{7A5AEDF1-5490-430A-A069-A8519A81E932}']
  end;

  GADCustomNativeAd = interface(NSObject)
    ['{7AC3A27D-798F-4E3B-A331-A2F0D2453151}']
    function availableAssetKeys: NSArray; cdecl;
    function customClickHandler: GADNativeAdCustomClickHandler; cdecl;
    function delegate: Pointer; cdecl;
    function displayAdMeasurement: GADDisplayAdMeasurement; cdecl;
    function formatID: NSString; cdecl;
    function imageForKey(key: NSString): GADNativeAdImage; cdecl;
    function mediaContent: GADMediaContent; cdecl;
    procedure performClickOnAssetWithKey(assetKey: NSString); cdecl;
    procedure recordImpression; cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setCustomClickHandler(customClickHandler: GADNativeAdCustomClickHandler); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
    function stringForKey(key: NSString): NSString; cdecl;
  end;
  TGADCustomNativeAd = class(TOCGenericImport<GADCustomNativeAdClass, GADCustomNativeAd>) end;

  GADCustomNativeAdLoaderDelegate = interface(IObjectiveC)
    ['{6F9BAD16-A2BC-4224-B56E-60BA444375F2}']
    procedure adLoader(adLoader: GADAdLoader; didReceiveCustomNativeAd: GADCustomNativeAd); cdecl;
    function customNativeAdFormatIDsForAdLoader(adLoader: GADAdLoader): NSArray; cdecl;
  end;

  GADCustomNativeAdDelegate = interface(IObjectiveC)
    ['{A695B6A1-F189-45A4-BBD7-17397ED161E5}']
    procedure customNativeAdDidDismissScreen(nativeAd: GADCustomNativeAd); cdecl;
    procedure customNativeAdDidRecordClick(nativeAd: GADCustomNativeAd); cdecl;
    procedure customNativeAdDidRecordImpression(nativeAd: GADCustomNativeAd); cdecl;
    procedure customNativeAdWillDismissScreen(nativeAd: GADCustomNativeAd); cdecl;
    procedure customNativeAdWillPresentScreen(nativeAd: GADCustomNativeAd); cdecl;
  end;

  GADDebugOptionsViewControllerDelegate = interface(IObjectiveC)
    ['{043C2802-76FB-4DCE-924D-8AF3D99B478B}']
    procedure debugOptionsViewControllerDidDismiss(controller: GADDebugOptionsViewController); cdecl;
  end;

  GADDebugOptionsViewControllerClass = interface(UIViewControllerClass)
    ['{3B1E485B-A447-4219-BEDA-72DCD6F4B575}']
    {class} function debugOptionsViewControllerWithAdUnitID(adUnitID: NSString): Pointer; cdecl;
  end;

  GADDebugOptionsViewController = interface(UIViewController)
    ['{08EA4720-298E-4FC0-86F0-452341149847}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADDebugOptionsViewController = class(TOCGenericImport<GADDebugOptionsViewControllerClass, GADDebugOptionsViewController>) end;

  GADExtrasClass = interface(NSObjectClass)
    ['{1CFFAFAF-CED0-420B-8104-3413E1F7582F}']
  end;

  GADExtras = interface(NSObject)
    ['{BF935DE1-ED1D-44E9-8BB8-953E6E55021E}']
    function additionalParameters: NSDictionary; cdecl;
    procedure setAdditionalParameters(additionalParameters: NSDictionary); cdecl;
  end;
  TGADExtras = class(TOCGenericImport<GADExtrasClass, GADExtras>) end;

  GADAdapterStatusClass = interface(NSObjectClass)
    ['{D4F43554-0540-4FC2-98FE-440B4680269C}']
  end;

  GADAdapterStatus = interface(NSObject)
    ['{2B752D0D-A47F-426F-8818-633834FA9644}']
    function description: NSString; cdecl;
    function latency: NSTimeInterval; cdecl;
    function state: GADAdapterInitializationState; cdecl;
  end;
  TGADAdapterStatus = class(TOCGenericImport<GADAdapterStatusClass, GADAdapterStatus>) end;

  GADInitializationStatusClass = interface(NSObjectClass)
    ['{DBA267EF-BB18-4C0B-A219-E3476CC85206}']
  end;

  GADInitializationStatus = interface(NSObject)
    ['{E844B62B-E14D-423A-9585-9BA24D75623D}']
    function adapterStatusesByClassName: NSDictionary; cdecl;
  end;
  TGADInitializationStatus = class(TOCGenericImport<GADInitializationStatusClass, GADInitializationStatus>) end;

  GADServerSideVerificationOptionsClass = interface(NSObjectClass)
    ['{BDCD2AAC-3C99-4C25-A3D1-7BD3D814F3EC}']
  end;

  GADServerSideVerificationOptions = interface(NSObject)
    ['{A4CF34F2-8B49-4E38-8669-D726E67C0CC7}']
    function customRewardString: NSString; cdecl;
    procedure setCustomRewardString(customRewardString: NSString); cdecl;
    procedure setUserIdentifier(userIdentifier: NSString); cdecl;
    function userIdentifier: NSString; cdecl;
  end;
  TGADServerSideVerificationOptions = class(TOCGenericImport<GADServerSideVerificationOptionsClass, GADServerSideVerificationOptions>) end;

  GADInterstitialAdClass = interface(NSObjectClass)
    ['{7EFCB3FA-96D3-4ACB-88D7-43E1D885E3FA}']
    {class} procedure loadWithAdResponseString(adResponseString: NSString; completionHandler: GADInterstitialAdLoadCompletionHandler); cdecl;
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; completionHandler: GADInterstitialAdLoadCompletionHandler); cdecl;
  end;

  GADInterstitialAd = interface(NSObject)
    ['{C289CAE5-CDDD-45B2-AFED-FAE162BE77A3}']
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PPointer): Boolean; cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function placementID: Int64; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setPlacementID(placementID: Int64); cdecl;
  end;
  TGADInterstitialAd = class(TOCGenericImport<GADInterstitialAdClass, GADInterstitialAd>) end;

  GADRequestConfigurationClass = interface(NSObjectClass)
    ['{D36DA312-3FE9-4B4B-92E2-6825702B5AC8}']
  end;

  GADRequestConfiguration = interface(NSObject)
    ['{E02A9846-B146-47D4-A734-4854E0436F08}']
    function maxAdContentRating: GADMaxAdContentRating; cdecl;
    function publisherPrivacyPersonalizationState: GADPublisherPrivacyPersonalizationState; cdecl;
    procedure setMaxAdContentRating(maxAdContentRating: GADMaxAdContentRating); cdecl;
    procedure setPublisherFirstPartyIDEnabled(enabled: Boolean); cdecl;
    procedure setPublisherPrivacyPersonalizationState(publisherPrivacyPersonalizationState: GADPublisherPrivacyPersonalizationState); cdecl;
    procedure setTagForChildDirectedTreatment(tagForChildDirectedTreatment: NSNumber); cdecl;
    procedure setTagForUnderAgeOfConsent(tagForUnderAgeOfConsent: NSNumber); cdecl;
    procedure setTestDeviceIdentifiers(testDeviceIdentifiers: NSArray); cdecl;
    function tagForChildDirectedTreatment: NSNumber; cdecl;
    function tagForUnderAgeOfConsent: NSNumber; cdecl;
    function testDeviceIdentifiers: NSArray; cdecl;
  end;
  TGADRequestConfiguration = class(TOCGenericImport<GADRequestConfigurationClass, GADRequestConfiguration>) end;

  GADSignalClass = interface(NSObjectClass)
    ['{A3E13A00-EB0E-4692-8811-392B22257D0E}']
  end;

  GADSignal = interface(NSObject)
    ['{58E7094F-755C-42B0-917F-BD055C398E19}']
    function signalString: NSString; cdecl;
  end;
  TGADSignal = class(TOCGenericImport<GADSignalClass, GADSignal>) end;

  GADSignalRequestClass = interface(NSObjectClass)
    ['{59B97B3E-D31C-4375-BE53-D6F389B4B0E0}']
  end;

  GADSignalRequest = interface(NSObject)
    ['{E36976D1-F499-4A61-A27B-7618527849E4}']
    function adNetworkExtrasFor(aClass: Pointer): Pointer; cdecl;
    function adUnitID: NSString; cdecl;
    function categoryExclusions: NSArray; cdecl;
    function contentURL: NSString; cdecl;
    function customTargeting: NSDictionary; cdecl;
    function keywords: NSArray; cdecl;
    function neighboringContentURLStrings: NSArray; cdecl;
    function publisherProvidedID: NSString; cdecl;
    procedure registerAdNetworkExtras(extras: Pointer); cdecl;
    procedure removeAdNetworkExtrasFor(aClass: Pointer); cdecl;
    function requestAgent: NSString; cdecl;
    function scene: UIWindowScene; cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    procedure setCategoryExclusions(categoryExclusions: NSArray); cdecl;
    procedure setContentURL(contentURL: NSString); cdecl;
    procedure setCustomTargeting(customTargeting: NSDictionary); cdecl;
    procedure setKeywords(keywords: NSArray); cdecl;
    procedure setNeighboringContentURLStrings(neighboringContentURLStrings: NSArray); cdecl;
    procedure setPublisherProvidedID(publisherProvidedID: NSString); cdecl;
    procedure setRequestAgent(requestAgent: NSString); cdecl;
    procedure setScene(scene: UIWindowScene); cdecl;
  end;
  TGADSignalRequest = class(TOCGenericImport<GADSignalRequestClass, GADSignalRequest>) end;

  GADMobileAdsClass = interface(NSObjectClass)
    ['{C5374052-1814-443E-8C6B-4E3EDB2A4F46}']
    {class} procedure generateSignal(request: GADSignalRequest; completionHandler: GADSignalCompletionHandler); cdecl;
    {class} function sharedInstance: GADMobileAds; cdecl;
  end;

  GADMobileAds = interface(NSObject)
    ['{1AA23650-4426-4E3E-BF1F-68975A81210B}']
    function applicationVolume: Single; cdecl;
    function audioVideoManager: GADAudioVideoManager; cdecl;
    procedure disableMediationInitialization; cdecl;
    procedure disableSDKCrashReporting; cdecl;
    function initializationStatus: GADInitializationStatus; cdecl;
    function isApplicationMuted: Boolean; cdecl;
    function isSDKVersionAtLeastMajor(major: NSInteger; minor: NSInteger; patch: NSInteger): Boolean; cdecl;
    procedure presentAdInspectorFromViewController(viewController: UIViewController; completionHandler: GADAdInspectorCompletionHandler); cdecl;
    procedure registerWebView(webView: WKWebView); cdecl;
    function requestConfiguration: GADRequestConfiguration; cdecl;
    procedure setApplicationMuted(applicationMuted: Boolean); cdecl;
    procedure setApplicationVolume(applicationVolume: Single); cdecl;
    procedure startWithCompletionHandler(completionHandler: GADInitializationCompletionHandler); cdecl;
    function versionNumber: GADVersionNumber; cdecl;
  end;
  TGADMobileAds = class(TOCGenericImport<GADMobileAdsClass, GADMobileAds>) end;

  GADMultipleAdsAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{352AF47A-3719-4190-A047-E082E04C3ADA}']
  end;

  GADMultipleAdsAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{53C5B328-3C7B-48EF-9181-AAEF5596EE93}']
    function numberOfAds: NSInteger; cdecl;
    procedure setNumberOfAds(numberOfAds: NSInteger); cdecl;
  end;
  TGADMultipleAdsAdLoaderOptions = class(TOCGenericImport<GADMultipleAdsAdLoaderOptionsClass, GADMultipleAdsAdLoaderOptions>) end;

  GADMuteThisAdReasonClass = interface(NSObjectClass)
    ['{992A6E5E-AAFE-4D94-BD39-A6FCE95F6692}']
  end;

  GADMuteThisAdReason = interface(NSObject)
    ['{99E07DAB-E24F-4F82-A60C-82588334848A}']
    function reasonDescription: NSString; cdecl;
  end;
  TGADMuteThisAdReason = class(TOCGenericImport<GADMuteThisAdReasonClass, GADMuteThisAdReason>) end;

  GADNativeAdDelegate = interface(IObjectiveC)
    ['{456DA93F-F68A-4BC9-8CBB-DF88CD9F609F}']
    procedure nativeAdDidDismissScreen(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdDidRecordClick(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdDidRecordImpression(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdDidRecordSwipeGestureClick(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdIsMuted(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdWillDismissScreen(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdWillPresentScreen(nativeAd: GADNativeAd); cdecl;
  end;

  GADNativeAdClass = interface(NSObjectClass)
    ['{9A828B4C-C3EB-456D-8CDE-31061661DE29}']
  end;

  GADNativeAd = interface(NSObject)
    ['{2EF08681-D097-467F-BD1B-7B68DAC69974}']
    function advertiser: NSString; cdecl;
    function body: NSString; cdecl;
    function callToAction: NSString; cdecl;
    procedure cancelUnconfirmedClick; cdecl;
    function delegate: Pointer; cdecl;
    procedure enableCustomClickGestures; cdecl;
    function extraAssets: NSDictionary; cdecl;
    function headline: NSString; cdecl;
    function icon: GADNativeAdImage; cdecl;
    function images: NSArray; cdecl;
    function isCustomClickGestureEnabled: Boolean; cdecl;
    function isCustomMuteThisAdAvailable: Boolean; cdecl;
    function mediaContent: GADMediaContent; cdecl;
    function muteThisAdReasons: NSArray; cdecl;
    procedure muteThisAdWithReason(reason: GADMuteThisAdReason); cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function placementID: Int64; cdecl;
    function price: NSString; cdecl;
    procedure recordCustomClickGesture; cdecl;
    procedure registerAdView(adView: UIView; clickableAssetViews: NSDictionary; nonclickableAssetViews: NSDictionary); cdecl;
    procedure registerClickConfirmingView(view: UIView); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setPlacementID(placementID: Int64); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
    procedure setUnconfirmedClickDelegate(unconfirmedClickDelegate: Pointer); cdecl;
    function starRating: NSDecimalNumber; cdecl;
    function store: NSString; cdecl;
    function unconfirmedClickDelegate: Pointer; cdecl;
    procedure unregisterAdView; cdecl;
  end;
  TGADNativeAd = class(TOCGenericImport<GADNativeAdClass, GADNativeAd>) end;

  GADNativeAdLoaderDelegate = interface(IObjectiveC)
    ['{10A47342-3CC7-4537-94F1-E7291A426586}']
    procedure adLoader(adLoader: GADAdLoader; didReceiveNativeAd: GADNativeAd); cdecl;
  end;

  GADNativeAdViewClass = interface(UIViewClass)
    ['{39D95EE9-01EC-4669-A0CB-0ACF64359739}']
  end;

  GADNativeAdView = interface(UIView)
    ['{C0F8D220-AF5C-464A-AD0C-66138CB072DD}']
    function adChoicesView: GADAdChoicesView; cdecl;
    function advertiserView: UIView; cdecl;
    function bodyView: UIView; cdecl;
    function callToActionView: UIView; cdecl;
    function headlineView: UIView; cdecl;
    function iconView: UIView; cdecl;
    function imageView: UIView; cdecl;
    function mediaView: GADMediaView; cdecl;
    function nativeAd: GADNativeAd; cdecl;
    function priceView: UIView; cdecl;
    procedure setAdChoicesView(adChoicesView: GADAdChoicesView); cdecl;
    procedure setAdvertiserView(advertiserView: UIView); cdecl;
    procedure setBodyView(bodyView: UIView); cdecl;
    procedure setCallToActionView(callToActionView: UIView); cdecl;
    procedure setHeadlineView(headlineView: UIView); cdecl;
    procedure setIconView(iconView: UIView); cdecl;
    procedure setImageView(imageView: UIView); cdecl;
    procedure setMediaView(mediaView: GADMediaView); cdecl;
    procedure setNativeAd(nativeAd: GADNativeAd); cdecl;
    procedure setPriceView(priceView: UIView); cdecl;
    procedure setStarRatingView(starRatingView: UIView); cdecl;
    procedure setStoreView(storeView: UIView); cdecl;
    function starRatingView: UIView; cdecl;
    function storeView: UIView; cdecl;
  end;
  TGADNativeAdView = class(TOCGenericImport<GADNativeAdViewClass, GADNativeAdView>) end;

  GADNativeAdUnconfirmedClickDelegate = interface(IObjectiveC)
    ['{CBE740ED-76E7-4928-9D8F-12EC08F30FF0}']
    procedure nativeAd(nativeAd: GADNativeAd; didReceiveUnconfirmedClickOnAssetID: GADNativeAssetIdentifier); cdecl;
    procedure nativeAdDidCancelUnconfirmedClick(nativeAd: GADNativeAd); cdecl;
  end;

  GADNativeAdCustomClickGestureOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{3489D483-D318-4379-8B18-3A58D1811029}']
  end;

  GADNativeAdCustomClickGestureOptions = interface(GADAdLoaderOptions)
    ['{203A41C7-0692-4996-B711-17F2A75FEEF1}']
    function areTapsAllowed: Boolean; cdecl;
    function initWithSwipeGestureDirection(direction: UISwipeGestureRecognizerDirection; tapsAllowed: Boolean): Pointer; cdecl;
    procedure setSwipeGestureDirection(swipeGestureDirection: UISwipeGestureRecognizerDirection); cdecl;
    procedure setTapsAllowed(tapsAllowed: Boolean); cdecl;
    function swipeGestureDirection: UISwipeGestureRecognizerDirection; cdecl;
  end;
  TGADNativeAdCustomClickGestureOptions = class(TOCGenericImport<GADNativeAdCustomClickGestureOptionsClass, GADNativeAdCustomClickGestureOptions>) end;

  GADNativeAdImageAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{4AC42573-A799-430F-B6DB-D80CEE695D73}']
  end;

  GADNativeAdImageAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{F61CCAC9-78B9-45D0-915B-A6D52D07B7CF}']
    function disableImageLoading: Boolean; cdecl;
    procedure setDisableImageLoading(disableImageLoading: Boolean); cdecl;
    procedure setShouldRequestMultipleImages(shouldRequestMultipleImages: Boolean); cdecl;
    function shouldRequestMultipleImages: Boolean; cdecl;
  end;
  TGADNativeAdImageAdLoaderOptions = class(TOCGenericImport<GADNativeAdImageAdLoaderOptionsClass, GADNativeAdImageAdLoaderOptions>) end;

  GADNativeAdMediaAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{7125C4E3-A9AC-4A85-8B62-59111F553931}']
  end;

  GADNativeAdMediaAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{C0683185-979E-49D2-8EDE-39816E39FBAE}']
    function mediaAspectRatio: GADMediaAspectRatio; cdecl;
    procedure setMediaAspectRatio(mediaAspectRatio: GADMediaAspectRatio); cdecl;
  end;
  TGADNativeAdMediaAdLoaderOptions = class(TOCGenericImport<GADNativeAdMediaAdLoaderOptionsClass, GADNativeAdMediaAdLoaderOptions>) end;

  GADNativeAdViewAdOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{5AFFDFC0-7D2D-4E65-B815-1AACBBD58D79}']
  end;

  GADNativeAdViewAdOptions = interface(GADAdLoaderOptions)
    ['{A7D18F80-A483-420C-A3B6-52A24CDC68E9}']
    function preferredAdChoicesPosition: GADAdChoicesPosition; cdecl;
    procedure setPreferredAdChoicesPosition(preferredAdChoicesPosition: GADAdChoicesPosition); cdecl;
  end;
  TGADNativeAdViewAdOptions = class(TOCGenericImport<GADNativeAdViewAdOptionsClass, GADNativeAdViewAdOptions>) end;

  GADNativeMuteThisAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{481EE7E1-A800-4287-8C6F-B2381AC751F2}']
  end;

  GADNativeMuteThisAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{B8CDFBA9-7226-40B6-B62C-D952E5464A61}']
    function customMuteThisAdRequested: Boolean; cdecl;
    procedure setCustomMuteThisAdRequested(customMuteThisAdRequested: Boolean); cdecl;
  end;
  TGADNativeMuteThisAdLoaderOptions = class(TOCGenericImport<GADNativeMuteThisAdLoaderOptionsClass, GADNativeMuteThisAdLoaderOptions>) end;

  GADQueryInfoClass = interface(NSObjectClass)
    ['{9054D8F2-23E6-4229-8467-7DF6E17D44D5}']
    {class} procedure createQueryInfoWithRequest(request: GADRequest; adFormat: GADAdFormat; completionHandler: GADQueryInfoCreationCompletionHandler); overload; cdecl;
    {class} procedure createQueryInfoWithRequest(request: GADRequest; adFormat: GADAdFormat; adUnitID: NSString; completionHandler: GADQueryInfoCreationCompletionHandler); overload; cdecl;
  end;

  GADQueryInfo = interface(NSObject)
    ['{1FF2E749-ED98-4750-AD47-6E852AC65915}']
    function query: NSString; cdecl;
  end;
  TGADQueryInfo = class(TOCGenericImport<GADQueryInfoClass, GADQueryInfo>) end;

  GADRewardedAdClass = interface(NSObjectClass)
    ['{300B55B1-3448-4203-A65C-D67D5F361DFD}']
    {class} procedure loadWithAdResponseString(adResponseString: NSString; completionHandler: GADRewardedAdLoadCompletionHandler); cdecl;
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; completionHandler: GADRewardedAdLoadCompletionHandler); cdecl;
  end;

  GADRewardedAd = interface(NSObject)
    ['{46212665-62FC-479B-A991-0D8B4B87D806}']
    function adReward: GADAdReward; cdecl;
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PPointer): Boolean; cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function placementID: Int64; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController; userDidEarnRewardHandler: GADUserDidEarnRewardHandler); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function serverSideVerificationOptions: GADServerSideVerificationOptions; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setPlacementID(placementID: Int64); cdecl;
    procedure setServerSideVerificationOptions(serverSideVerificationOptions: GADServerSideVerificationOptions); cdecl;
  end;
  TGADRewardedAd = class(TOCGenericImport<GADRewardedAdClass, GADRewardedAd>) end;

  GADRewardedInterstitialAdClass = interface(NSObjectClass)
    ['{0EA1927B-0477-4ECF-ABE4-A277F84F80CD}']
    {class} procedure loadWithAdResponseString(adResponseString: NSString; completionHandler: GADRewardedInterstitialAdLoadCompletionHandler); cdecl;
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; completionHandler: GADRewardedInterstitialAdLoadCompletionHandler); cdecl;
  end;

  GADRewardedInterstitialAd = interface(NSObject)
    ['{A81F70A4-000A-422A-BA89-7D12E69E93A3}']
    function adReward: GADAdReward; cdecl;
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PPointer): Boolean; cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function placementID: Int64; cdecl;
    procedure presentFromRootViewController(viewController: UIViewController; userDidEarnRewardHandler: GADUserDidEarnRewardHandler); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function serverSideVerificationOptions: GADServerSideVerificationOptions; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setPlacementID(placementID: Int64); cdecl;
    procedure setServerSideVerificationOptions(serverSideVerificationOptions: GADServerSideVerificationOptions); cdecl;
  end;
  TGADRewardedInterstitialAd = class(TOCGenericImport<GADRewardedInterstitialAdClass, GADRewardedInterstitialAd>) end;

  GADVideoControllerDelegate = interface(IObjectiveC)
    ['{3F36BBC9-27A5-42ED-998B-963878E6ADFD}']
    procedure videoControllerDidEndVideoPlayback(videoController: GADVideoController); cdecl;
    procedure videoControllerDidMuteVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidPauseVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidPlayVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidUnmuteVideo(videoController: GADVideoController); cdecl;
  end;

  GADVideoOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{A3AD640E-BC65-4D39-81C9-BB4E10F7CCA0}']
  end;

  GADVideoOptions = interface(GADAdLoaderOptions)
    ['{42A97B7E-2D7D-4D3E-AB36-397676F2FAAF}']
    function clickToExpandRequested: Boolean; cdecl;
    function customControlsRequested: Boolean; cdecl;
    procedure setClickToExpandRequested(clickToExpandRequested: Boolean); cdecl;
    procedure setCustomControlsRequested(customControlsRequested: Boolean); cdecl;
    procedure setStartMuted(startMuted: Boolean); cdecl;
    function startMuted: Boolean; cdecl;
  end;
  TGADVideoOptions = class(TOCGenericImport<GADVideoOptionsClass, GADVideoOptions>) end;

  GAMBannerAdLoaderDelegate = interface(IObjectiveC)
    ['{3010E2F3-38E5-43F4-BF16-2396448CC6BA}']
    procedure adLoader(adLoader: GADAdLoader; didReceiveGAMBannerView: GAMBannerView); cdecl;
    function validBannerSizesForAdLoader(adLoader: GADAdLoader): NSArray; cdecl;
  end;

  GAMBannerViewClass = interface(GADBannerViewClass)
    ['{5F8CE886-0400-495C-9849-B4101CD2825B}']
  end;

  GAMBannerView = interface(GADBannerView)
    ['{664C0767-ECAF-41B9-A569-AAA42B5CAF27}']
    function adSizeDelegate: Pointer; cdecl;
    function adUnitID: NSString; cdecl;
    function appEventDelegate: Pointer; cdecl;
    function enableManualImpressions: Boolean; cdecl;
    procedure recordImpression; cdecl;
    procedure resize(size: GADAdSize); cdecl;
    procedure setAdOptions(adOptions: NSArray); cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    procedure setAppEventDelegate(appEventDelegate: Pointer); cdecl;
    procedure setEnableManualImpressions(enableManualImpressions: Boolean); cdecl;
    procedure setValidAdSizes(validAdSizes: NSArray); cdecl;
    function validAdSizes: NSArray; cdecl;
    function videoController: GADVideoController; cdecl;
  end;
  TGAMBannerView = class(TOCGenericImport<GAMBannerViewClass, GAMBannerView>) end;

  GAMBannerViewOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{11DE7ACD-4681-418A-B453-4C558A733EFA}']
  end;

  GAMBannerViewOptions = interface(GADAdLoaderOptions)
    ['{B84B7950-3EC1-4D82-876A-C819F3389112}']
    function isManualImpressionEnabled: Boolean; cdecl;
    procedure setManualImpressionEnabled(manualImpressionEnabled: Boolean); cdecl;
  end;
  TGAMBannerViewOptions = class(TOCGenericImport<GAMBannerViewOptionsClass, GAMBannerViewOptions>) end;

  GAMRequestClass = interface(GADRequestClass)
    ['{58466B46-D519-429D-82D6-CB58FEB80938}']
  end;

  GAMRequest = interface(GADRequest)
    ['{A422B4FB-68A5-452C-96C1-5BB526B7675A}']
    function categoryExclusions: NSArray; cdecl;
    function publisherProvidedID: NSString; cdecl;
    procedure setCategoryExclusions(categoryExclusions: NSArray); cdecl;
    procedure setPublisherProvidedID(publisherProvidedID: NSString); cdecl;
  end;
  TGAMRequest = class(TOCGenericImport<GAMRequestClass, GAMRequest>) end;

  GAMInterstitialAdClass = interface(GADInterstitialAdClass)
    ['{06FF5D15-8B1D-46D8-97AC-154B5FD58EB6}']
    {class} procedure loadWithAdManagerAdUnitID(adUnitID: NSString; request: GAMRequest; completionHandler: GAMInterstitialAdLoadCompletionHandler); cdecl;
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; completionHandler: GADInterstitialAdLoadCompletionHandler); cdecl;
  end;

  GAMInterstitialAd = interface(GADInterstitialAd)
    ['{6425226D-797E-40B3-AED3-3E92B445CD12}']
    function appEventDelegate: Pointer; cdecl;
    procedure setAppEventDelegate(appEventDelegate: Pointer); cdecl;
  end;
  TGAMInterstitialAd = class(TOCGenericImport<GAMInterstitialAdClass, GAMInterstitialAd>) end;

  GADMediationAdRequest = interface(IObjectiveC)
    ['{E0AB71C7-D81B-4AB3-887F-B937ABF3BEA9}']
    function childDirectedTreatment: NSNumber; cdecl;
    function credentials: NSDictionary; cdecl;
    function maxAdContentRating: GADMaxAdContentRating; cdecl;
    function networkExtras: Pointer; cdecl;
    function publisherId: NSString; cdecl;
    function testMode: Boolean; cdecl;
    function underAgeOfConsent: NSNumber; cdecl;
    function userKeywords: NSArray; cdecl;
  end;

  GADMAdNetworkConnector = interface(IObjectiveC)
    ['{5D246C11-B6B7-4C5E-ACBE-6DEF48E23C30}']
    procedure adapter(adapter: Pointer; didReceiveAdView: UIView); overload; cdecl;
    procedure adapter(adapter: Pointer; didReceiveMediatedUnifiedNativeAd: Pointer); overload; cdecl;
    procedure adapter(adapter: Pointer; didReceiveInterstitial: NSObject); overload; cdecl;
    procedure adapter(adapter: Pointer; didFailAd: NSError); overload; cdecl;
    [MethodName('adapter:clickDidOccurInBanner:')]
    procedure adapterClickDidOccurInBanner(adapter: Pointer; clickDidOccurInBanner: UIView); cdecl;
    procedure adapterDidDismissFullScreenModal(adapter: Pointer); cdecl;
    procedure adapterDidDismissInterstitial(adapter: Pointer); cdecl;
    [MethodName('adapter:didFailInterstitial:')]
    procedure adapterDidFailInterstitial(adapter: Pointer; didFailInterstitial: NSError); cdecl;
    procedure adapterDidGetAdClick(adapter: Pointer); cdecl;
    procedure adapterDidReceiveInterstitial(adapter: Pointer); cdecl;
    procedure adapterWillDismissFullScreenModal(adapter: Pointer); cdecl;
    procedure adapterWillDismissInterstitial(adapter: Pointer); cdecl;
    procedure adapterWillLeaveApplication(adapter: Pointer); cdecl;
    procedure adapterWillPresentFullScreenModal(adapter: Pointer); cdecl;
    procedure adapterWillPresentInterstitial(adapter: Pointer); cdecl;
    function adMuted: Boolean; cdecl;
    function adVolume: Single; cdecl;
    function viewControllerForPresentingModalView: UIViewController; cdecl;
  end;

  GADMAdNetworkAdapter = interface(IObjectiveC)
    ['{D99501F6-1862-4849-8EF2-83751B6B5EA7}']
    {class} function adapterVersion: NSString; cdecl;
    procedure changeAdSizeTo(adSize: GADAdSize); cdecl;
    procedure getBannerWithSize(adSize: GADAdSize); cdecl;
    procedure getInterstitial; cdecl;
    procedure getNativeAdWithAdTypes(adTypes: NSArray; options: NSArray); cdecl;
    function handlesUserClicks: Boolean; cdecl;
    function handlesUserImpressions: Boolean; cdecl;
    function initWithGADMAdNetworkConnector(connector: Pointer): Pointer; cdecl;
    {class} function networkExtrasClass: Pointer; cdecl;
    procedure presentInterstitialFromRootViewController(rootViewController: UIViewController); cdecl;
    procedure stopBeingDelegate; cdecl;
  end;

  GADMediatedUnifiedNativeAdNotificationSourceClass = interface(NSObjectClass)
    ['{11F19744-B85C-44DC-86AE-A3D3F05831D6}']
    {class} procedure mediatedNativeAdDidDismissScreen(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidEndVideoPlayback(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidPauseVideo(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidPlayVideo(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidRecordClick(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidRecordImpression(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdWillDismissScreen(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdWillPresentScreen(mediatedNativeAd: Pointer); cdecl;
  end;

  GADMediatedUnifiedNativeAdNotificationSource = interface(NSObject)
    ['{0D3E9F8C-1840-4EF9-BCBC-B157063CE4CA}']
  end;
  TGADMediatedUnifiedNativeAdNotificationSource = class(TOCGenericImport<GADMediatedUnifiedNativeAdNotificationSourceClass, GADMediatedUnifiedNativeAdNotificationSource>) end;

  GADMediationAd = interface(IObjectiveC)
    ['{E3043E1A-022C-49A6-8832-87D166768111}']
  end;

  GADMediationCredentialsClass = interface(NSObjectClass)
    ['{1E1D98ED-4CE4-4036-A459-D40B93558FCC}']
  end;

  GADMediationCredentials = interface(NSObject)
    ['{823EB02A-A9F5-41ED-A7B2-A22FF564EE85}']
    function format: GADAdFormat; cdecl;
    function settings: NSDictionary; cdecl;
  end;
  TGADMediationCredentials = class(TOCGenericImport<GADMediationCredentialsClass, GADMediationCredentials>) end;

  GADMediationServerConfigurationClass = interface(NSObjectClass)
    ['{6F704D45-DE82-406E-964A-0963F08E57CC}']
  end;

  GADMediationServerConfiguration = interface(NSObject)
    ['{A01AE406-C559-4D95-ACDA-6C35174B00F0}']
    function credentials: NSArray; cdecl;
  end;
  TGADMediationServerConfiguration = class(TOCGenericImport<GADMediationServerConfigurationClass, GADMediationServerConfiguration>) end;

  GADMediationAdConfigurationClass = interface(NSObjectClass)
    ['{EE7CE6E4-9C4D-4120-BBCE-6E5764264E77}']
  end;

  GADMediationAdConfiguration = interface(NSObject)
    ['{DBD4F962-47D6-4A38-988A-2EDB5E25FF25}']
    function bidResponse: NSString; cdecl;
    function credentials: GADMediationCredentials; cdecl;
    function extras: Pointer; cdecl;
    function isTestRequest: Boolean; cdecl;
    function topViewController: UIViewController; cdecl;
    function watermark: NSData; cdecl;
  end;
  TGADMediationAdConfiguration = class(TOCGenericImport<GADMediationAdConfigurationClass, GADMediationAdConfiguration>) end;

  GADMediationAdEventDelegate = interface(IObjectiveC)
    ['{65262027-B520-43D7-887E-8E2A607B1DF1}']
    procedure didDismissFullScreenView; cdecl;
    procedure didFailToPresentWithError(error: NSError); cdecl;
    procedure reportClick; cdecl;
    procedure reportImpression; cdecl;
    procedure willDismissFullScreenView; cdecl;
    procedure willPresentFullScreenView; cdecl;
  end;

  GADMediationBannerAdEventDelegate = interface(IObjectiveC)
    ['{E06F223A-BD23-42F4-BAE5-A3B4A104D39A}']
  end;

  GADMediationInterstitialAdEventDelegate = interface(IObjectiveC)
    ['{938C8D90-52A6-4CE1-A5FA-B606FBB6D06C}']
  end;

  GADMediationNativeAdEventDelegate = interface(IObjectiveC)
    ['{A1EEEF0D-058B-4DB4-B85A-BA6C54B4C252}']
    procedure didEndVideo; cdecl;
    procedure didMuteVideo; cdecl;
    procedure didPauseVideo; cdecl;
    procedure didPlayVideo; cdecl;
    procedure didUnmuteVideo; cdecl;
  end;

  GADMediationRewardedAdEventDelegate = interface(IObjectiveC)
    ['{A6EC9577-EBA5-475F-A303-9B68779720AA}']
    procedure didEndVideo; cdecl;
    procedure didRewardUser; cdecl;
    procedure didStartVideo; cdecl;
  end;

  GADMediationAppOpenAdEventDelegate = interface(IObjectiveC)
    ['{935B77F1-B4BA-4C34-B4D2-0F0CB40A1369}']
  end;

  GADMediationAppOpenAd = interface(IObjectiveC)
    ['{2B5E0927-D323-40F9-ABC0-1AB5DDABE34C}']
    procedure presentFromViewController(viewController: UIViewController); cdecl;
  end;

  GADMediationAppOpenAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{FF7A9BEA-191E-45D0-8A96-AAE70DA8FA70}']
  end;

  GADMediationAppOpenAdConfiguration = interface(GADMediationAdConfiguration)
    ['{1AE2D77F-FD51-4CE0-BEE2-72245317CFF3}']
  end;
  TGADMediationAppOpenAdConfiguration = class(TOCGenericImport<GADMediationAppOpenAdConfigurationClass, GADMediationAppOpenAdConfiguration>) end;

  GADMediationBannerAd = interface(IObjectiveC)
    ['{CA61D2C6-6E92-4AD8-BDFF-4B47E9563C1F}']
    procedure changeAdSizeTo(adSize: GADAdSize); cdecl;
    function view: UIView; cdecl;
  end;

  GADMediationInterscrollerAd = interface(IObjectiveC)
    ['{B511D142-3AF6-413B-8721-D2F3FD2F1BD6}']
    function delegateInterscrollerEffect: Boolean; cdecl;
    procedure setDelegateInterscrollerEffect(delegateInterscrollerEffect: Boolean); cdecl;
  end;

  GADMediationBannerAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{47FB5B7E-29F5-474D-985B-F9FEF2DDB67A}']
  end;

  GADMediationBannerAdConfiguration = interface(GADMediationAdConfiguration)
    ['{FF3DA615-E88F-493B-9CEB-46A6550C45B6}']
    function adSize: GADAdSize; cdecl;
  end;
  TGADMediationBannerAdConfiguration = class(TOCGenericImport<GADMediationBannerAdConfigurationClass, GADMediationBannerAdConfiguration>) end;

  GADMediationInterstitialAd = interface(IObjectiveC)
    ['{0A93AE1C-241F-4354-924C-3ADCF06EF3B6}']
    procedure presentFromViewController(viewController: UIViewController); cdecl;
  end;

  GADMediationInterstitialAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{A7F66483-52FF-4289-812D-F33EDF06B6AF}']
  end;

  GADMediationInterstitialAdConfiguration = interface(GADMediationAdConfiguration)
    ['{5CD51814-52B7-4D64-9EFF-D069A352BD2B}']
  end;
  TGADMediationInterstitialAdConfiguration = class(TOCGenericImport<GADMediationInterstitialAdConfigurationClass, GADMediationInterstitialAdConfiguration>) end;

  GADMediationNativeAd = interface(IObjectiveC)
    ['{CEBF67C4-44A3-4336-BB95-3DAA1C7AE7EA}']
    function handlesUserClicks: Boolean; cdecl;
    function handlesUserImpressions: Boolean; cdecl;
  end;

  GADMediationNativeAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{5D66170D-9105-4675-BDB9-BBBDA2B6F0CD}']
  end;

  GADMediationNativeAdConfiguration = interface(GADMediationAdConfiguration)
    ['{F88B892D-F624-42C0-B549-B79EB0196A55}']
    function options: NSArray; cdecl;
  end;
  TGADMediationNativeAdConfiguration = class(TOCGenericImport<GADMediationNativeAdConfigurationClass, GADMediationNativeAdConfiguration>) end;

  GADMediationRewardedAd = interface(IObjectiveC)
    ['{9460845B-46DE-4D61-8BC3-E262F68FFA2F}']
    procedure presentFromViewController(viewController: UIViewController); cdecl;
  end;

  GADMediationRewardedAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{848ADE83-3B72-422E-B999-CBFCFBDC93EE}']
  end;

  GADMediationRewardedAdConfiguration = interface(GADMediationAdConfiguration)
    ['{0049A0D7-9357-4320-BC47-AD2118AEA9F3}']
  end;
  TGADMediationRewardedAdConfiguration = class(TOCGenericImport<GADMediationRewardedAdConfigurationClass, GADMediationRewardedAdConfiguration>) end;

  GADMediationAdapter = interface(IObjectiveC)
    ['{F9385DA8-71F4-452B-B0A2-41F8B2B5802D}']
    {class} function adapterVersion: GADVersionNumber; cdecl;
    {class} function adSDKVersion: GADVersionNumber; cdecl;
    function init: Pointer; cdecl;
    procedure loadAppOpenAdForAdConfiguration(adConfiguration: GADMediationAppOpenAdConfiguration; completionHandler: GADMediationAppOpenLoadCompletionHandler); cdecl;
    procedure loadBannerForAdConfiguration(adConfiguration: GADMediationBannerAdConfiguration; completionHandler: GADMediationBannerLoadCompletionHandler); cdecl;
    procedure loadInterscrollerAdForAdConfiguration(adConfiguration: GADMediationBannerAdConfiguration; completionHandler: GADMediationInterscrollerAdLoadCompletionHandler); cdecl;
    procedure loadInterstitialForAdConfiguration(adConfiguration: GADMediationInterstitialAdConfiguration; completionHandler: GADMediationInterstitialLoadCompletionHandler); cdecl;
    procedure loadNativeAdForAdConfiguration(adConfiguration: GADMediationNativeAdConfiguration; completionHandler: GADMediationNativeLoadCompletionHandler); cdecl;
    procedure loadRewardedAdForAdConfiguration(adConfiguration: GADMediationRewardedAdConfiguration; completionHandler: GADMediationRewardedLoadCompletionHandler); cdecl;
    procedure loadRewardedInterstitialAdForAdConfiguration(adConfiguration: GADMediationRewardedAdConfiguration; completionHandler: GADMediationRewardedLoadCompletionHandler); cdecl;
    {class} function networkExtrasClass: Pointer; cdecl;
    {class} procedure setUpWithConfiguration(configuration: GADMediationServerConfiguration; completionHandler: GADMediationAdapterSetUpCompletionBlock); cdecl;
  end;

  GADRTBMediationSignalsConfigurationClass = interface(NSObjectClass)
    ['{9E83C67B-587F-477A-8033-E8CCA21AB9B0}']
  end;

  GADRTBMediationSignalsConfiguration = interface(NSObject)
    ['{B253D91F-5794-400B-B425-024B95C8B9EC}']
    function credentials: NSArray; cdecl;
  end;
  TGADRTBMediationSignalsConfiguration = class(TOCGenericImport<GADRTBMediationSignalsConfigurationClass, GADRTBMediationSignalsConfiguration>) end;

  GADRTBRequestParametersClass = interface(NSObjectClass)
    ['{48BDD685-D63E-43D4-9660-2827267B365A}']
  end;

  GADRTBRequestParameters = interface(NSObject)
    ['{93E06AE0-A3C3-4BF4-9776-7B97631333AA}']
    function adSize: GADAdSize; cdecl;
    function configuration: GADRTBMediationSignalsConfiguration; cdecl;
    function extras: Pointer; cdecl;
  end;
  TGADRTBRequestParameters = class(TOCGenericImport<GADRTBRequestParametersClass, GADRTBRequestParameters>) end;

  GADRTBAdapter = interface(IObjectiveC)
    ['{C819F0EE-2B31-4222-A25C-75C6485A221F}']
    procedure collectSignalsForRequestParameters(params: GADRTBRequestParameters; completionHandler: GADRTBSignalCompletionHandler); cdecl;
    function init: Pointer; cdecl;
  end;

  GADAppOpenSignalRequestClass = interface(GADSignalRequestClass)
    ['{A327B87A-61FE-4811-B1EB-1C67C124C566}']
  end;

  GADAppOpenSignalRequest = interface(GADSignalRequest)
    ['{F07D34A2-1837-4236-81DE-735B9EC029AA}']
    function initWithSignalType(signalType: NSString): Pointer; cdecl;
  end;
  TGADAppOpenSignalRequest = class(TOCGenericImport<GADAppOpenSignalRequestClass, GADAppOpenSignalRequest>) end;

  GADBannerSignalRequestClass = interface(GADSignalRequestClass)
    ['{417B1737-26A8-40F9-9B6C-8A28CF8F4B8E}']
  end;

  GADBannerSignalRequest = interface(GADSignalRequest)
    ['{17C71A55-2C38-4DE8-89B4-41587E5FF2A2}']
    function adSize: GADAdSize; cdecl;
    function adSizes: NSArray; cdecl;
    function initWithSignalType(signalType: NSString): Pointer; cdecl;
    function isManualImpressionEnabled: Boolean; cdecl;
    procedure setAdSize(adSize: GADAdSize); cdecl;
    procedure setAdSizes(adSizes: NSArray); cdecl;
    procedure setManualImpressionEnabled(manualImpressionEnabled: Boolean); cdecl;
    procedure setVideoOptions(videoOptions: GADVideoOptions); cdecl;
    function videoOptions: GADVideoOptions; cdecl;
  end;
  TGADBannerSignalRequest = class(TOCGenericImport<GADBannerSignalRequestClass, GADBannerSignalRequest>) end;

  GADInterstitialSignalRequestClass = interface(GADSignalRequestClass)
    ['{DF180F98-9DBA-47B6-9DAE-EDA06FEFF345}']
  end;

  GADInterstitialSignalRequest = interface(GADSignalRequest)
    ['{7732C4A3-0C78-400E-AC8D-B53A21E25962}']
    function initWithSignalType(signalType: NSString): Pointer; cdecl;
  end;
  TGADInterstitialSignalRequest = class(TOCGenericImport<GADInterstitialSignalRequestClass, GADInterstitialSignalRequest>) end;

  GADNativeSignalRequestClass = interface(GADSignalRequestClass)
    ['{0E3318DF-3C19-4669-9B40-BB1DAAEC3F09}']
  end;

  GADNativeSignalRequest = interface(GADSignalRequest)
    ['{95DC6BEA-E31F-43E8-B241-CB308DC37417}']
    function adLoaderAdTypes: NSSet; cdecl;
    function adSizes: NSArray; cdecl;
    function customNativeAdFormatIDs: NSArray; cdecl;
    function enableManualImpressions: Boolean; cdecl;
    procedure enableSwipeGestureDirection(direction: UISwipeGestureRecognizerDirection; tapsAllowed: Boolean); cdecl;
    function initWithSignalType(signalType: NSString): Pointer; cdecl;
    function isCustomMuteThisAdRequested: Boolean; cdecl;
    function isImageLoadingDisabled: Boolean; cdecl;
    function mediaAspectRatio: GADMediaAspectRatio; cdecl;
    function numberOfAds: NSInteger; cdecl;
    function preferredAdChoicesPosition: GADAdChoicesPosition; cdecl;
    procedure setAdLoaderAdTypes(adLoaderAdTypes: NSSet); cdecl;
    procedure setAdSizes(adSizes: NSArray); cdecl;
    procedure setCustomMuteThisAdRequested(customMuteThisAdRequested: Boolean); cdecl;
    procedure setCustomNativeAdFormatIDs(customNativeAdFormatIDs: NSArray); cdecl;
    procedure setDisableImageLoading(disableImageLoading: Boolean); cdecl;
    procedure setEnableManualImpressions(enableManualImpressions: Boolean); cdecl;
    procedure setMediaAspectRatio(mediaAspectRatio: GADMediaAspectRatio); cdecl;
    procedure setNumberOfAds(numberOfAds: NSInteger); cdecl;
    procedure setPreferredAdChoicesPosition(preferredAdChoicesPosition: GADAdChoicesPosition); cdecl;
    procedure setShouldRequestMultipleImages(shouldRequestMultipleImages: Boolean); cdecl;
    procedure setVideoOptions(videoOptions: GADVideoOptions); cdecl;
    function shouldRequestMultipleImages: Boolean; cdecl;
    function videoOptions: GADVideoOptions; cdecl;
  end;
  TGADNativeSignalRequest = class(TOCGenericImport<GADNativeSignalRequestClass, GADNativeSignalRequest>) end;

  GADRewardedInterstitialSignalRequestClass = interface(GADSignalRequestClass)
    ['{AD3D37DA-BA9B-42FD-865E-0F518B4CB07D}']
  end;

  GADRewardedInterstitialSignalRequest = interface(GADSignalRequest)
    ['{9F0B1498-A03E-461E-85AB-445145DEFD8B}']
    function initWithSignalType(signalType: NSString): Pointer; cdecl;
  end;
  TGADRewardedInterstitialSignalRequest = class(TOCGenericImport<GADRewardedInterstitialSignalRequestClass, GADRewardedInterstitialSignalRequest>) end;

  GADRewardedSignalRequestClass = interface(GADSignalRequestClass)
    ['{570696ED-1DE4-4BA2-962A-65E276EBEB58}']
  end;

  GADRewardedSignalRequest = interface(GADSignalRequest)
    ['{01235EE6-C206-4A24-BE70-196A7F4ED0B2}']
    function initWithSignalType(signalType: NSString): Pointer; cdecl;
  end;
  TGADRewardedSignalRequest = class(TOCGenericImport<GADRewardedSignalRequestClass, GADRewardedSignalRequest>) end;

const
  libGoogleMobileAds = 'GoogleMobileAds';

function GADPortraitInlineAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADPortraitInlineAdaptiveBannerAdSizeWithWidth';

function GADLandscapeInlineAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADLandscapeInlineAdaptiveBannerAdSizeWithWidth';

function GADCurrentOrientationInlineAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADCurrentOrientationInlineAdaptiveBannerAdSizeWithWidth';

function GADInlineAdaptiveBannerAdSizeWithWidthAndMaxHeight(width: CGFloat; maxHeight: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADInlineAdaptiveBannerAdSizeWithWidthAndMaxHeight';

function GADPortraitAnchoredAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADPortraitAnchoredAdaptiveBannerAdSizeWithWidth';

function GADLandscapeAnchoredAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADLandscapeAnchoredAdaptiveBannerAdSizeWithWidth';

function GADCurrentOrientationAnchoredAdaptiveBannerAdSizeWithWidth(width: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADCurrentOrientationAnchoredAdaptiveBannerAdSizeWithWidth';

function GADAdSizeFromCGSize(size: CGSize): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADAdSizeFromCGSize';

function GADAdSizeFullWidthPortraitWithHeight(height: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADAdSizeFullWidthPortraitWithHeight';

function GADAdSizeFullWidthLandscapeWithHeight(height: CGFloat): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADAdSizeFullWidthLandscapeWithHeight';

function GADAdSizeEqualToSize(size1: GADAdSize; size2: GADAdSize): Boolean; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADAdSizeEqualToSize';

function IsGADAdSizeValid(size: GADAdSize): Boolean; cdecl;
  external framework libGoogleMobileAds name _PU + 'IsGADAdSizeValid';

function GADAdSizeIsFluid(size: GADAdSize): Boolean; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADAdSizeIsFluid';

function CGSizeFromGADAdSize(size: GADAdSize): CGSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'CGSizeFromGADAdSize';

function NSStringFromGADAdSize(size: GADAdSize): NSString; cdecl;
  external framework libGoogleMobileAds name _PU + 'NSStringFromGADAdSize';

function NSValueFromGADAdSize(size: GADAdSize): NSValue; cdecl;
  external framework libGoogleMobileAds name _PU + 'NSValueFromGADAdSize';

function GADAdSizeFromNSValue(value: NSValue): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADAdSizeFromNSValue';

function GADGetStringFromVersionNumber(version: GADVersionNumber): NSString; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADGetStringFromVersionNumber';

function GADClosestValidSizeForAdSizes(original: GADAdSize; possibleAdSizes: NSArray): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADClosestValidSizeForAdSizes';

function kGADAdSizeBanner: GADAdSize;
function kGADAdSizeLargeBanner: GADAdSize;
function kGADAdSizeMediumRectangle: GADAdSize;
function kGADAdSizeFullBanner: GADAdSize;
function kGADAdSizeLeaderboard: GADAdSize;

implementation

uses
  // DW
  DW.iOSapi.SwiftCompat;

function kGADAdSizeBanner: GADAdSize;
begin
  Result.size.width := 320;
  Result.size.height := 50;
end;

function kGADAdSizeLargeBanner: GADAdSize;
begin
  Result.size.width := 320;
  Result.size.height := 100;
end;

function kGADAdSizeMediumRectangle: GADAdSize;
begin
  Result.size.width := 300;
  Result.size.height := 250;
end;

function kGADAdSizeFullBanner: GADAdSize;
begin
  Result.size.width := 468;
  Result.size.height := 60;
end;

function kGADAdSizeLeaderboard: GADAdSize;
begin
  Result.size.width := 728;
  Result.size.height := 90;
end;

procedure CLangRTLoader; cdecl;
  {$IF not Defined(IOSSIMULATOR)}
  external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a'; // Fixes linker error: ___isOSVersionAtLeast missing (iOS SDK 12.x)
  {$ELSE}
  external '/usr/lib/clang/lib/darwin/libclang_rt.iossim.a'; // Fixes linker error: ___isOSVersionAtLeast missing (iOS SDK 12.x)
  {$ENDIF}
procedure JavaScriptCoreLoader; cdecl; external framework 'JavaScriptCore';
procedure UserMessagingPlatformLoader; cdecl; external framework 'UserMessagingPlatform';

end.