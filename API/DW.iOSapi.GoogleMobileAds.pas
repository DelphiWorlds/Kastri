unit DW.iOSapi.GoogleMobileAds;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.UIKit;

const
  GADAdFormatBanner = 0;
  GADAdFormatInterstitial = 1;
  GADAdFormatRewarded = 2;
  GADAdFormatNative = 3;
  GADAdFormatRewardedInterstitial = 4;
  GADAdFormatUnknown = 5;
  kGADGenderUnknown = 0;
  kGADGenderMale = 1;
  kGADGenderFemale = 2;
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
  GADAdChoicesPositionTopRightCorner = 0;
  GADAdChoicesPositionTopLeftCorner = 1;
  GADAdChoicesPositionBottomRightCorner = 2;
  GADAdChoicesPositionBottomLeftCorner = 3;
  GADPresentationErrorCodeAdNotReady = 15;
  GADPresentationErrorCodeAdTooLarge = 16;
  GADPresentationErrorCodeInternal = 17;
  GADPresentationErrorCodeAdAlreadyUsed = 18;
  GADPresentationErrorNotMainThread = 21;
  GADPresentationErrorMediation = 22;
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
  GADErrorReceivedInvalidResponse = 13;
  GADErrorMediationNoFill = 9;
  GADErrorAdAlreadyUsed = 19;
  GADErrorApplicationIdentifierMissing = 20;
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
  GADDynamicHeightSearchRequest = interface;
  GADExtras = interface;
  GADAdapterStatus = interface;
  GADInitializationStatus = interface;
  GADServerSideVerificationOptions = interface;
  GADInterstitialAd = interface;
  GADRequestConfiguration = interface;
  GADMobileAds = interface;
  GADMultipleAdsAdLoaderOptions = interface;
  GADMuteThisAdReason = interface;
  GADNativeAdDelegate = interface;
  GADNativeAd = interface;
  GADNativeAdLoaderDelegate = interface;
  GADNativeAdView = interface;
  GADNativeAdUnconfirmedClickDelegate = interface;
  GADNativeAdImageAdLoaderOptions = interface;
  GADNativeAdMediaAdLoaderOptions = interface;
  GADNativeAdViewAdOptions = interface;
  GADNativeMuteThisAdLoaderOptions = interface;
  GADRewardedAd = interface;
  GADRewardedInterstitialAd = interface;
  GADSearchBannerView = interface;
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
  GADMRewardBasedVideoAdNetworkConnector = interface;
  GADMRewardBasedVideoAdNetworkAdapter = interface;
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

  PGADAdSize = ^GADAdSize;
  PGADVersionNumber = ^GADVersionNumber;

  GADAdFormat = NSInteger;
  GADAdLoaderAdType = NSString;
  GADGender = NSInteger;
  GADAdMetadataKey = NSString;

  GADUserDidEarnRewardHandler = procedure of object;

  GADAdSize = record
    size: CGSize;
    flags: NSUInteger;
  end;

  GADAdValuePrecision = NSInteger;

  GADPaidEventHandler = procedure(value: GADAdValue) of object;

  GADAppOpenAdLoadCompletionHandler = procedure(appOpenAd: GADAppOpenAd; error: NSError) of object;
  GADNativeAssetIdentifier = NSString;

  GADNativeAdCustomClickHandler = procedure(assetID: NSString) of object;
  GADAdapterInitializationState = NSInteger;

  GADInterstitialAdLoadCompletionHandler = procedure(interstitialAd: GADInterstitialAd; error: NSError) of object;
  GADMediaAspectRatio = NSInteger;
  GADMaxAdContentRating = NSString;

  GADInitializationCompletionHandler = procedure(status: GADInitializationStatus) of object;

  GADAdInspectorCompletionHandler = procedure(error: NSError) of object;
  GADAdChoicesPosition = NSInteger;
  GADPresentationErrorCode = NSInteger;
  GADErrorCode = NSInteger;

  GADRewardedAdLoadCompletionHandler = procedure(rewardedAd: GADRewardedAd; error: NSError) of object;

  GADRewardedInterstitialAdLoadCompletionHandler = procedure(rewardedInterstitialAd: GADRewardedInterstitialAd; error: NSError) of object;

  GAMInterstitialAdLoadCompletionHandler = procedure(interstitialAd: GAMInterstitialAd; error: NSError) of object;
  GADMBannerAnimationType = NSInteger;

  GADVersionNumber = record
    majorVersion: NSInteger;
    minorVersion: NSInteger;
    patchVersion: NSInteger;
  end;

  GADMediationBannerLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationInterscrollerAdLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationInterstitialLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationNativeLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationRewardedLoadCompletionHandler = function(ad: Pointer; error: NSError): Pointer of object;

  GADMediationAdapterSetUpCompletionBlock = procedure(error: NSError) of object;
  GADUnifiedNativeAssetIdentifier = NSString;
  GADMediationErrorCode = NSInteger;

  GADRTBSignalCompletionHandler = procedure(signals: NSString; error: NSError) of object;

  GADAdChoicesViewClass = interface(UIViewClass)
    ['{6FF599B9-F829-435F-B44D-CF663B4B0886}']
  end;

  GADAdChoicesView = interface(UIView)
    ['{A3D13AE3-09A7-45ED-B6E7-620D44BD5C08}']
  end;
  TGADAdChoicesView = class(TOCGenericImport<GADAdChoicesViewClass, GADAdChoicesView>) end;

  GADAdLoaderDelegate = interface(IObjectiveC)
    ['{78FC372D-0174-4922-880A-58FCD879337B}']
    procedure adLoader(adLoader: GADAdLoader; didFailToReceiveAdWithError: NSError); cdecl;
    procedure adLoaderDidFinishLoading(adLoader: GADAdLoader); cdecl;
  end;

  GADAdNetworkExtras = interface(IObjectiveC)
    ['{73DCEC9D-C536-4666-A015-44D73F368BCE}']
  end;

  GADRequestClass = interface(NSObjectClass)
    ['{C3C0ACC7-722D-4546-8E4E-6C5E1B40376E}']
    {class} function request: Pointer; cdecl;
  end;

  GADRequest = interface(NSObject)
    ['{815A23D4-C999-49A7-B0B5-E2A9824549C8}']
    function adNetworkExtrasFor(aClass: Pointer): Pointer; cdecl;
    function contentURL: NSString; cdecl;
    function keywords: NSArray; cdecl;
    function neighboringContentURLStrings: NSArray; cdecl;
    procedure registerAdNetworkExtras(extras: Pointer); cdecl;
    procedure removeAdNetworkExtrasFor(aClass: Pointer); cdecl;
    function requestAgent: NSString; cdecl;
    function scene: UIWindowScene; cdecl;
    procedure setContentURL(contentURL: NSString); cdecl;
    procedure setKeywords(keywords: NSArray); cdecl;
    procedure setLocationWithLatitude(latitude: CGFloat; longitude: CGFloat; accuracy: CGFloat); cdecl;
    procedure setNeighboringContentURLStrings(neighboringContentURLStrings: NSArray); cdecl;
    procedure setRequestAgent(requestAgent: NSString); cdecl;
    procedure setScene(scene: UIWindowScene); cdecl;
  end;
  TGADRequest = class(TOCGenericImport<GADRequestClass, GADRequest>) end;

  GADAdLoaderOptionsClass = interface(NSObjectClass)
    ['{75BD24AB-A504-4F9E-9C3F-D51ACDA5DD60}']
  end;

  GADAdLoaderOptions = interface(NSObject)
    ['{4C584110-CE8F-4A88-9123-EC317056BB43}']
  end;
  TGADAdLoaderOptions = class(TOCGenericImport<GADAdLoaderOptionsClass, GADAdLoaderOptions>) end;

  GADAdLoaderClass = interface(NSObjectClass)
    ['{9BF7A9BD-02C3-46DE-B669-E8A5D9065E2D}']
  end;

  GADAdLoader = interface(NSObject)
    ['{FF4CD438-0049-48FF-83A0-ED798907C37F}']
    function adUnitID: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function initWithAdUnitID(adUnitID: NSString; rootViewController: UIViewController; adTypes: NSArray; options: NSArray): Pointer; cdecl;
    function isLoading: Boolean; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADAdLoader = class(TOCGenericImport<GADAdLoaderClass, GADAdLoader>) end;

  GADAdMetadataProvider = interface(IObjectiveC)
    ['{202D590A-A8BE-48B5-A1CF-0AEF16AFB2BB}']
    function adMetadata: NSDictionary; cdecl;
    function adMetadataDelegate: Pointer; cdecl;
    procedure setAdMetadataDelegate(adMetadataDelegate: Pointer); cdecl;
  end;

  GADAdMetadataDelegate = interface(IObjectiveC)
    ['{2A250894-2A87-4E67-B0FA-E3526A8050A5}']
    procedure adMetadataDidChange(ad: Pointer); cdecl;
  end;

  GADAdRewardClass = interface(NSObjectClass)
    ['{0D116530-41E0-4C71-95B8-25DF4BB19FB1}']
  end;

  GADAdReward = interface(NSObject)
    ['{0BA4EAD6-08A8-412A-BDBC-7301233DFAD0}']
    [MethodName('type')]
    function &type: NSString; cdecl;
    function amount: NSDecimalNumber; cdecl;
    function initWithRewardType(rewardType: NSString; rewardAmount: NSDecimalNumber): Pointer; cdecl;
  end;
  TGADAdReward = class(TOCGenericImport<GADAdRewardClass, GADAdReward>) end;

  GADAdSizeDelegate = interface(IObjectiveC)
    ['{D9FB194E-FE8B-4584-A1AC-5EECAC191FE2}']
    procedure adView(bannerView: GADBannerView; willChangeAdSizeTo: GADAdSize); cdecl;
  end;

  GADAdValueClass = interface(NSObjectClass)
    ['{D64FDF53-4E3E-4CC2-9202-166D7085544E}']
  end;

  GADAdValue = interface(NSObject)
    ['{AB038075-0486-4ACC-8C74-9CAD853BFA1C}']
    function currencyCode: NSString; cdecl;
    function precision: GADAdValuePrecision; cdecl;
    function value: NSDecimalNumber; cdecl;
  end;
  TGADAdValue = class(TOCGenericImport<GADAdValueClass, GADAdValue>) end;

  GADAppEventDelegate = interface(IObjectiveC)
    ['{6E9C30D8-ACC3-4EE1-AB8E-1030EF31457C}']
    procedure adView(banner: GADBannerView; didReceiveAppEvent: NSString; withInfo: NSString); cdecl;
    procedure interstitialAd(interstitialAd: GADInterstitialAd; didReceiveAppEvent: NSString; withInfo: NSString); cdecl;
  end;

  GADFullScreenPresentingAd = interface(IObjectiveC)
    ['{38576B76-B1C7-4971-BED1-05841C41EBF6}']
    function fullScreenContentDelegate: Pointer; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
  end;

  GADFullScreenContentDelegate = interface(IObjectiveC)
    ['{83A89A10-42F6-46B1-A5A0-551DA5AD17F6}']
    procedure ad(ad: Pointer; didFailToPresentFullScreenContentWithError: NSError); cdecl;
    procedure adDidDismissFullScreenContent(ad: Pointer); cdecl;
    procedure adDidPresentFullScreenContent(ad: Pointer); cdecl;
    procedure adDidRecordClick(ad: Pointer); cdecl;
    procedure adDidRecordImpression(ad: Pointer); cdecl;
    procedure adWillDismissFullScreenContent(ad: Pointer); cdecl;
    procedure adWillPresentFullScreenContent(ad: Pointer); cdecl;
  end;

  GADAdNetworkResponseInfoClass = interface(NSObjectClass)
    ['{40AA7666-7800-4627-B15C-58FBA3D9FD13}']
  end;

  GADAdNetworkResponseInfo = interface(NSObject)
    ['{4E68A387-1205-4F33-9D27-2A9DC5554E9C}']
    function adNetworkClassName: NSString; cdecl;
    function adUnitMapping: NSDictionary; cdecl;
    function credentials: NSDictionary; cdecl;
    function dictionaryRepresentation: NSDictionary; cdecl;
    function error: NSError; cdecl;
    function latency: NSTimeInterval; cdecl;
  end;
  TGADAdNetworkResponseInfo = class(TOCGenericImport<GADAdNetworkResponseInfoClass, GADAdNetworkResponseInfo>) end;

  GADResponseInfoClass = interface(NSObjectClass)
    ['{5AD79271-6584-4D3D-9410-45B7EF49EC44}']
  end;

  GADResponseInfo = interface(NSObject)
    ['{3E4D6A0D-BE9F-4FFD-844B-7DF9E52797FB}']
    function adNetworkClassName: NSString; cdecl;
    function adNetworkInfoArray: NSArray; cdecl;
    function dictionaryRepresentation: NSDictionary; cdecl;
    function responseIdentifier: NSString; cdecl;
  end;
  TGADResponseInfo = class(TOCGenericImport<GADResponseInfoClass, GADResponseInfo>) end;

  GADAppOpenAdClass = interface(NSObjectClass)
    ['{ED2E83CA-DC5B-42E4-A14B-C2974D3F7DCD}']
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; orientation: UIInterfaceOrientation;
      completionHandler: GADAppOpenAdLoadCompletionHandler); cdecl;
  end;

  GADAppOpenAd = interface(NSObject)
    ['{CD8EC1C6-0EB7-4010-A496-43A1B82C24F8}']
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PPointer): Boolean; cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
  end;
  TGADAppOpenAd = class(TOCGenericImport<GADAppOpenAdClass, GADAppOpenAd>) end;

  GADAudioVideoManagerDelegate = interface(IObjectiveC)
    ['{13726481-7282-43C8-9C79-1BA9F27CF00A}']
    procedure audioVideoManagerDidPauseAllVideo(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerDidStopPlayingAudio(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerWillPlayAudio(audioVideoManager: GADAudioVideoManager); cdecl;
    procedure audioVideoManagerWillPlayVideo(audioVideoManager: GADAudioVideoManager); cdecl;
  end;

  GADAudioVideoManagerClass = interface(NSObjectClass)
    ['{6461A68F-5669-4C0C-A492-EAD7855C6241}']
  end;

  GADAudioVideoManager = interface(NSObject)
    ['{CDB10661-56E1-44C2-AC6C-9CA7F2D6AD9E}']
    function audioSessionIsApplicationManaged: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    procedure setAudioSessionIsApplicationManaged(audioSessionIsApplicationManaged: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADAudioVideoManager = class(TOCGenericImport<GADAudioVideoManagerClass, GADAudioVideoManager>) end;

  GADBannerViewDelegate = interface(IObjectiveC)
    ['{755843F1-A503-449A-AE80-BFAABD1E26F5}']
    procedure bannerView(bannerView: GADBannerView; didFailToReceiveAdWithError: NSError); cdecl;
    procedure bannerViewDidDismissScreen(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidReceiveAd(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidRecordClick(bannerView: GADBannerView); cdecl;
    procedure bannerViewDidRecordImpression(bannerView: GADBannerView); cdecl;
    procedure bannerViewWillDismissScreen(bannerView: GADBannerView); cdecl;
    procedure bannerViewWillPresentScreen(bannerView: GADBannerView); cdecl;
  end;

  GADBannerViewClass = interface(UIViewClass)
    ['{FA204418-2B2B-48DB-B84E-B61788E2302F}']
  end;

  GADBannerView = interface(UIView)
    ['{743659BD-1460-4057-9ABF-B9344B4FD6C7}']
    function adSize: GADAdSize; cdecl;
    function adSizeDelegate: Pointer; cdecl;
    function adUnitID: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function initWithAdSize(adSize: GADAdSize; origin: CGPoint): Pointer; overload; cdecl;
    function initWithAdSize(adSize: GADAdSize): Pointer; overload; cdecl;
    function isAutoloadEnabled: Boolean; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setAdSize(adSize: GADAdSize); cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    procedure setAutoloadEnabled(autoloadEnabled: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
  end;
  TGADBannerView = class(TOCGenericImport<GADBannerViewClass, GADBannerView>) end;

  GADCustomEventBannerDelegate = interface(IObjectiveC)
    ['{D931F3B9-F9FA-49F5-9C08-A96CC25D1D99}']
    [MethodName('customEventBanner:clickDidOccurInAd:')]
    procedure customEventBannerClickDidOccurInAd(customEvent: Pointer; clickDidOccurInAd: UIView); cdecl;
    procedure customEventBannerDidDismissModal(customEvent: Pointer); cdecl;
    [MethodName('customEventBanner:didFailAd:')]
    procedure customEventBannerDidFailAd(customEvent: Pointer; didFailAd: NSError); cdecl;
    [MethodName('customEventBanner:didReceiveAd:')]
    procedure customEventBannerDidReceiveAd(customEvent: Pointer; didReceiveAd: UIView); cdecl;
    procedure customEventBannerWasClicked(customEvent: Pointer); cdecl;
    procedure customEventBannerWillDismissModal(customEvent: Pointer); cdecl;
    procedure customEventBannerWillLeaveApplication(customEvent: Pointer); cdecl;
    procedure customEventBannerWillPresentModal(customEvent: Pointer); cdecl;
    function viewControllerForPresentingModalView: UIViewController; cdecl;
  end;

  GADCustomEventRequestClass = interface(NSObjectClass)
    ['{64C1AF53-F4E3-445D-A375-A55108F94EE3}']
  end;

  GADCustomEventRequest = interface(NSObject)
    ['{DD187ED1-0CA7-431C-AA2E-BE4245CD1D74}']
    function additionalParameters: NSDictionary; cdecl;
    function isTesting: Boolean; cdecl;
    function userHasLocation: Boolean; cdecl;
    function userKeywords: NSArray; cdecl;
    function userLatitude: CGFloat; cdecl;
    function userLocationAccuracyInMeters: CGFloat; cdecl;
    function userLocationDescription: NSString; cdecl;
    function userLongitude: CGFloat; cdecl;
  end;
  TGADCustomEventRequest = class(TOCGenericImport<GADCustomEventRequestClass, GADCustomEventRequest>) end;

  GADCustomEventBanner = interface(IObjectiveC)
    ['{8339C440-83A3-4243-AF25-1ACE3A8DCB08}']
    function delegate: Pointer; cdecl;
    function init: Pointer; cdecl;
    procedure requestBannerAd(adSize: GADAdSize; parameter: NSString; &label: NSString; request: GADCustomEventRequest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADCustomEventExtrasClass = interface(NSObjectClass)
    ['{21023E2B-5B2A-41D1-98B9-0355859233A0}']
  end;

  GADCustomEventExtras = interface(NSObject)
    ['{89493422-5985-48B2-BBAD-58B9B103EEFE}']
    function allExtras: NSDictionary; cdecl;
    function extrasForLabel(&label: NSString): NSDictionary; cdecl;
    procedure removeAllExtras; cdecl;
    procedure setExtras(extras: NSDictionary; forLabel: NSString); cdecl;
  end;
  TGADCustomEventExtras = class(TOCGenericImport<GADCustomEventExtrasClass, GADCustomEventExtras>) end;

  GADCustomEventInterstitialDelegate = interface(IObjectiveC)
    ['{DE8C2CF1-EE18-4836-BF8C-5C9FEB4B18D8}']
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
    ['{727554E5-387E-4572-820A-91A5E18B8317}']
    function delegate: Pointer; cdecl;
    function init: Pointer; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
    procedure requestInterstitialAdWithParameter(serverParameter: NSString; &label: NSString; request: GADCustomEventRequest); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADCustomEventNativeAd = interface(IObjectiveC)
    ['{B79E8BE4-0059-419D-AF45-2D418C99F54A}']
    function delegate: Pointer; cdecl;
    function handlesUserClicks: Boolean; cdecl;
    function handlesUserImpressions: Boolean; cdecl;
    function init: Pointer; cdecl;
    procedure requestNativeAdWithParameter(serverParameter: NSString; request: GADCustomEventRequest; adTypes: NSArray; options: NSArray;
      rootViewController: UIViewController); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;

  GADNativeAdImageClass = interface(NSObjectClass)
    ['{893BCE54-CBCF-47DD-8B53-008EA08475BD}']
  end;

  GADNativeAdImage = interface(NSObject)
    ['{90B42456-1221-43FB-9563-A219F08A2D85}']
    function image: UIImage; cdecl;
    function imageURL: NSURL; cdecl;
    function initWithImage(image: UIImage): Pointer; cdecl;
    function initWithURL(URL: NSURL; scale: CGFloat): Pointer; cdecl;
    function scale: CGFloat; cdecl;
  end;
  TGADNativeAdImage = class(TOCGenericImport<GADNativeAdImageClass, GADNativeAdImage>) end;

  GADMediatedUnifiedNativeAd = interface(IObjectiveC)
    ['{0E441FBB-D440-4AAA-98A0-7E37454000C7}']
    function adChoicesView: UIView; cdecl;
    function advertiser: NSString; cdecl;
    function body: NSString; cdecl;
    function callToAction: NSString; cdecl;
    function currentTime: NSTimeInterval; cdecl;
    procedure didRecordClickOnAssetWithName(assetName: GADNativeAssetIdentifier; view: UIView; viewController: UIViewController); cdecl;
    procedure didRecordImpression; cdecl;
    procedure didRenderInView(view: UIView; clickableAssetViews: NSDictionary; nonclickableAssetViews: NSDictionary;
      viewController: UIViewController); cdecl;
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
    ['{95BDF2A1-E914-44B3-AFE1-449786A9E862}']
    procedure customEventNativeAd(customEventNativeAd: Pointer; didFailToLoadWithError: NSError); overload; cdecl;
    procedure customEventNativeAd(customEventNativeAd: Pointer; didReceiveMediatedUnifiedNativeAd: Pointer); overload; cdecl;
  end;

  GADDisplayAdMeasurementClass = interface(NSObjectClass)
    ['{B54CFE0B-F61E-4580-8863-A8A39580ABA9}']
  end;

  GADDisplayAdMeasurement = interface(NSObject)
    ['{36A91D31-48D1-4953-BBB0-8835DA18E88A}']
    procedure setView(view: UIView); cdecl;
    function startWithError(error: PPointer): Boolean; cdecl;
    function view: UIView; cdecl;
  end;
  TGADDisplayAdMeasurement = class(TOCGenericImport<GADDisplayAdMeasurementClass, GADDisplayAdMeasurement>) end;

  GADVideoControllerClass = interface(NSObjectClass)
    ['{339E1475-30B9-4E51-953D-C94402B724E5}']
  end;

  GADVideoController = interface(NSObject)
    ['{4644E9DA-A837-42AA-867C-90A4256B6036}']
    function clickToExpandEnabled: Boolean; cdecl;
    function customControlsEnabled: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMute(mute: Boolean); cdecl;
    procedure stop; cdecl;
  end;
  TGADVideoController = class(TOCGenericImport<GADVideoControllerClass, GADVideoController>) end;

  GADMediaContentClass = interface(NSObjectClass)
    ['{85E5E6B6-1FE7-4655-AF7C-D49803CE459B}']
  end;

  GADMediaContent = interface(NSObject)
    ['{20205A02-AF47-4813-89F0-0859A645C7B4}']
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
    ['{E19C21D3-328C-479B-9961-2FF47DA71CB3}']
  end;

  GADMediaView = interface(UIView)
    ['{37DC41EC-21CF-4450-94FD-AB034EA7A5DF}']
    function mediaContent: GADMediaContent; cdecl;
    procedure setMediaContent(mediaContent: GADMediaContent); cdecl;
  end;
  TGADMediaView = class(TOCGenericImport<GADMediaViewClass, GADMediaView>) end;

  GADCustomNativeAdClass = interface(NSObjectClass)
    ['{871BB6AE-FFC5-45B0-B88E-E7365C75B7E0}']
  end;

  GADCustomNativeAd = interface(NSObject)
    ['{32C97BEF-9BB7-4C91-A03F-A7E9C5680821}']
    function availableAssetKeys: NSArray; cdecl;
    function customClickHandler: GADNativeAdCustomClickHandler; cdecl;
    function delegate: Pointer; cdecl;
    function displayAdMeasurement: GADDisplayAdMeasurement; cdecl;
    function formatID: NSString; cdecl;
    function imageForKey(key: NSString): GADNativeAdImage; cdecl;
    function mediaContent: GADMediaContent; cdecl;
    function mediaView: GADMediaView; cdecl;
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
    ['{73882B63-FA17-47D2-B364-23BA161122F7}']
    procedure adLoader(adLoader: GADAdLoader; didReceiveCustomNativeAd: GADCustomNativeAd); cdecl;
    function customNativeAdFormatIDsForAdLoader(adLoader: GADAdLoader): NSArray; cdecl;
  end;

  GADCustomNativeAdDelegate = interface(IObjectiveC)
    ['{0E14F9A4-4239-4536-BCC0-E52408FDA043}']
    procedure customNativeAdDidDismissScreen(nativeAd: GADCustomNativeAd); cdecl;
    procedure customNativeAdDidRecordClick(nativeAd: GADCustomNativeAd); cdecl;
    procedure customNativeAdDidRecordImpression(nativeAd: GADCustomNativeAd); cdecl;
    procedure customNativeAdWillDismissScreen(nativeAd: GADCustomNativeAd); cdecl;
    procedure customNativeAdWillPresentScreen(nativeAd: GADCustomNativeAd); cdecl;
  end;

  GADDebugOptionsViewControllerDelegate = interface(IObjectiveC)
    ['{CC225BC2-7DF5-4BA6-A97F-FB58D8C3E04D}']
    procedure debugOptionsViewControllerDidDismiss(controller: GADDebugOptionsViewController); cdecl;
  end;

  GADDebugOptionsViewControllerClass = interface(UIViewControllerClass)
    ['{ADC1EAD6-DC13-4FC7-B65F-A74B2B9F03F5}']
    {class} function debugOptionsViewControllerWithAdUnitID(adUnitID: NSString): Pointer; cdecl;
  end;

  GADDebugOptionsViewController = interface(UIViewController)
    ['{D7222B2D-9828-4A04-98B4-B78265938A20}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TGADDebugOptionsViewController = class(TOCGenericImport<GADDebugOptionsViewControllerClass, GADDebugOptionsViewController>) end;

  GADDynamicHeightSearchRequestClass = interface(GADRequestClass)
    ['{7159A116-0023-421C-B08C-F1DE5E4A9A9A}']
  end;

  GADDynamicHeightSearchRequest = interface(GADRequest)
    ['{E6F06255-9508-4046-9B66-B7B0D9C51B93}']
    function adBorderColor: NSString; cdecl;
    function adBorderCSSSelections: NSString; cdecl;
    function adjustableLineHeight: CGFloat; cdecl;
    function adPage: NSInteger; cdecl;
    function adSeparatorColor: NSString; cdecl;
    function adTestEnabled: Boolean; cdecl;
    function annotationFontSize: CGFloat; cdecl;
    function annotationTextColor: NSString; cdecl;
    function attributionBottomSpacing: CGFloat; cdecl;
    function attributionFontFamily: NSString; cdecl;
    function attributionFontSize: CGFloat; cdecl;
    function attributionTextColor: NSString; cdecl;
    function backgroundColor: NSString; cdecl;
    function boldTitleEnabled: Boolean; cdecl;
    function borderColor: NSString; cdecl;
    function borderCSSSelections: NSString; cdecl;
    function channel: NSString; cdecl;
    function clickToCallExtensionEnabled: Boolean; cdecl;
    function CSSWidth: NSString; cdecl;
    function descriptionFontSize: CGFloat; cdecl;
    function detailedAttributionExtensionEnabled: Boolean; cdecl;
    function domainLinkColor: NSString; cdecl;
    function domainLinkFontSize: CGFloat; cdecl;
    function fontFamily: NSString; cdecl;
    function hostLanguage: NSString; cdecl;
    function locationExtensionEnabled: Boolean; cdecl;
    function locationExtensionFontSize: CGFloat; cdecl;
    function locationExtensionTextColor: NSString; cdecl;
    function longerHeadlinesExtensionEnabled: Boolean; cdecl;
    function numberOfAds: NSInteger; cdecl;
    function plusOnesExtensionEnabled: Boolean; cdecl;
    function query: NSString; cdecl;
    function sellerRatingsExtensionEnabled: Boolean; cdecl;
    procedure setAdBorderColor(adBorderColor: NSString); cdecl;
    procedure setAdBorderCSSSelections(adBorderCSSSelections: NSString); cdecl;
    procedure setAdjustableLineHeight(adjustableLineHeight: CGFloat); cdecl;
    procedure setAdPage(adPage: NSInteger); cdecl;
    procedure setAdSeparatorColor(adSeparatorColor: NSString); cdecl;
    procedure setAdTestEnabled(adTestEnabled: Boolean); cdecl;
    procedure setAdvancedOptionValue(value: Pointer; forKey: NSString); cdecl;
    procedure setAnnotationFontSize(annotationFontSize: CGFloat); cdecl;
    procedure setAnnotationTextColor(annotationTextColor: NSString); cdecl;
    procedure setAttributionBottomSpacing(attributionBottomSpacing: CGFloat); cdecl;
    procedure setAttributionFontFamily(attributionFontFamily: NSString); cdecl;
    procedure setAttributionFontSize(attributionFontSize: CGFloat); cdecl;
    procedure setAttributionTextColor(attributionTextColor: NSString); cdecl;
    procedure setBackgroundColor(backgroundColor: NSString); cdecl;
    procedure setBoldTitleEnabled(boldTitleEnabled: Boolean); cdecl;
    procedure setBorderColor(borderColor: NSString); cdecl;
    procedure setBorderCSSSelections(borderCSSSelections: NSString); cdecl;
    procedure setChannel(channel: NSString); cdecl;
    procedure setClickToCallExtensionEnabled(clickToCallExtensionEnabled: Boolean); cdecl;
    procedure setCSSWidth(CSSWidth: NSString); cdecl;
    procedure setDescriptionFontSize(descriptionFontSize: CGFloat); cdecl;
    procedure setDetailedAttributionExtensionEnabled(detailedAttributionExtensionEnabled: Boolean); cdecl;
    procedure setDomainLinkColor(domainLinkColor: NSString); cdecl;
    procedure setDomainLinkFontSize(domainLinkFontSize: CGFloat); cdecl;
    procedure setFontFamily(fontFamily: NSString); cdecl;
    procedure setHostLanguage(hostLanguage: NSString); cdecl;
    procedure setLocationExtensionEnabled(locationExtensionEnabled: Boolean); cdecl;
    procedure setLocationExtensionFontSize(locationExtensionFontSize: CGFloat); cdecl;
    procedure setLocationExtensionTextColor(locationExtensionTextColor: NSString); cdecl;
    procedure setLongerHeadlinesExtensionEnabled(longerHeadlinesExtensionEnabled: Boolean); cdecl;
    procedure setNumberOfAds(numberOfAds: NSInteger); cdecl;
    procedure setPlusOnesExtensionEnabled(plusOnesExtensionEnabled: Boolean); cdecl;
    procedure setQuery(query: NSString); cdecl;
    procedure setSellerRatingsExtensionEnabled(sellerRatingsExtensionEnabled: Boolean); cdecl;
    procedure setSiteLinksExtensionEnabled(siteLinksExtensionEnabled: Boolean); cdecl;
    procedure setStyleID(styleID: NSString); cdecl;
    procedure setTextColor(textColor: NSString); cdecl;
    procedure setTitleFontSize(titleFontSize: CGFloat); cdecl;
    procedure setTitleLinkColor(titleLinkColor: NSString); cdecl;
    procedure setTitleUnderlineHidden(titleUnderlineHidden: Boolean); cdecl;
    procedure setVerticalSpacing(verticalSpacing: CGFloat); cdecl;
    function siteLinksExtensionEnabled: Boolean; cdecl;
    function styleID: NSString; cdecl;
    function textColor: NSString; cdecl;
    function titleFontSize: CGFloat; cdecl;
    function titleLinkColor: NSString; cdecl;
    function titleUnderlineHidden: Boolean; cdecl;
    function verticalSpacing: CGFloat; cdecl;
  end;
  TGADDynamicHeightSearchRequest = class(TOCGenericImport<GADDynamicHeightSearchRequestClass, GADDynamicHeightSearchRequest>) end;

  GADExtrasClass = interface(NSObjectClass)
    ['{6C32E0F9-F0FC-42BE-A392-C422FC9046EB}']
  end;

  GADExtras = interface(NSObject)
    ['{9EB8845F-03BB-409A-B8C6-92FAF65C8C69}']
    function additionalParameters: NSDictionary; cdecl;
    procedure setAdditionalParameters(additionalParameters: NSDictionary); cdecl;
  end;
  TGADExtras = class(TOCGenericImport<GADExtrasClass, GADExtras>) end;

  GADAdapterStatusClass = interface(NSObjectClass)
    ['{8D0905C2-563C-4FBA-8215-4A3589E20003}']
  end;

  GADAdapterStatus = interface(NSObject)
    ['{CD2E5E11-9B5B-49D9-904B-456894F62045}']
    function description: NSString; cdecl;
    function latency: NSTimeInterval; cdecl;
    function state: GADAdapterInitializationState; cdecl;
  end;
  TGADAdapterStatus = class(TOCGenericImport<GADAdapterStatusClass, GADAdapterStatus>) end;

  GADInitializationStatusClass = interface(NSObjectClass)
    ['{ADE58E54-B3E1-4A84-BE63-3A56D74262E5}']
  end;

  GADInitializationStatus = interface(NSObject)
    ['{41DCCF18-A759-411A-9F73-533F48778934}']
    function adapterStatusesByClassName: NSDictionary; cdecl;
  end;
  TGADInitializationStatus = class(TOCGenericImport<GADInitializationStatusClass, GADInitializationStatus>) end;

  GADServerSideVerificationOptionsClass = interface(NSObjectClass)
    ['{E8FD0F95-8BB9-4563-A692-0C73F3292A45}']
  end;

  GADServerSideVerificationOptions = interface(NSObject)
    ['{A996AA2A-FFBC-4ECB-9A3E-33817B0C9401}']
    function customRewardString: NSString; cdecl;
    procedure setCustomRewardString(customRewardString: NSString); cdecl;
    procedure setUserIdentifier(userIdentifier: NSString); cdecl;
    function userIdentifier: NSString; cdecl;
  end;
  TGADServerSideVerificationOptions = class(TOCGenericImport<GADServerSideVerificationOptionsClass, GADServerSideVerificationOptions>) end;

  GADInterstitialAdClass = interface(NSObjectClass)
    ['{99302291-38F1-419D-8692-801444C2B90E}']
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; completionHandler: GADInterstitialAdLoadCompletionHandler); cdecl;
  end;

  GADInterstitialAd = interface(NSObject)
    ['{B17AE10B-046B-4A68-9AD2-3E3A6B3162EA}']
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PPointer): Boolean; cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
  end;
  TGADInterstitialAd = class(TOCGenericImport<GADInterstitialAdClass, GADInterstitialAd>) end;

  GADRequestConfigurationClass = interface(NSObjectClass)
    ['{1EFC9F9F-8EB6-4BA6-A5FA-43CE3DFA9A53}']
  end;

  GADRequestConfiguration = interface(NSObject)
    ['{69D64739-40A7-476C-967F-CDEF78C034C9}']
    function maxAdContentRating: GADMaxAdContentRating; cdecl;
    procedure setMaxAdContentRating(maxAdContentRating: GADMaxAdContentRating); cdecl;
    procedure setSameAppKeyEnabled(enabled: Boolean); cdecl;
    procedure setTestDeviceIdentifiers(testDeviceIdentifiers: NSArray); cdecl;
    procedure tagForChildDirectedTreatment(childDirectedTreatment: Boolean); cdecl;
    procedure tagForUnderAgeOfConsent(underAgeOfConsent: Boolean); cdecl;
    function testDeviceIdentifiers: NSArray; cdecl;
  end;
  TGADRequestConfiguration = class(TOCGenericImport<GADRequestConfigurationClass, GADRequestConfiguration>) end;

  GADMobileAdsClass = interface(NSObjectClass)
    ['{9EB00FF6-4637-48F4-A131-5E2AEB0983F5}']
    {class} function sharedInstance: GADMobileAds; cdecl;
  end;

  GADMobileAds = interface(NSObject)
    ['{9D00C87E-66D2-4660-B492-F593556EF33F}']
    function applicationMuted: Boolean; cdecl;
    function applicationVolume: Single; cdecl;
    function audioVideoManager: GADAudioVideoManager; cdecl;
    procedure disableAutomatedInAppPurchaseReporting; cdecl;
    procedure disableMediationInitialization; cdecl;
    procedure disableSDKCrashReporting; cdecl;
    procedure enableAutomatedInAppPurchaseReporting; cdecl;
    function initializationStatus: GADInitializationStatus; cdecl;
    function isSDKVersionAtLeastMajor(major: NSInteger; minor: NSInteger; patch: NSInteger): Boolean; cdecl;
    procedure presentAdInspectorFromViewController(viewController: UIViewController; completionHandler: GADAdInspectorCompletionHandler); cdecl;
    function requestConfiguration: GADRequestConfiguration; cdecl;
    function sdkVersion: NSString; cdecl;
    procedure setApplicationMuted(applicationMuted: Boolean); cdecl;
    procedure setApplicationVolume(applicationVolume: Single); cdecl;
    procedure startWithCompletionHandler(completionHandler: GADInitializationCompletionHandler); cdecl;
  end;
  TGADMobileAds = class(TOCGenericImport<GADMobileAdsClass, GADMobileAds>) end;

  GADMultipleAdsAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{561FA3A6-CA61-47A6-B4E9-A00141EDA3C3}']
  end;

  GADMultipleAdsAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{43766C4B-4E14-4796-8952-DAA298BB5ED5}']
    function numberOfAds: NSInteger; cdecl;
    procedure setNumberOfAds(numberOfAds: NSInteger); cdecl;
  end;
  TGADMultipleAdsAdLoaderOptions = class(TOCGenericImport<GADMultipleAdsAdLoaderOptionsClass, GADMultipleAdsAdLoaderOptions>) end;

  GADMuteThisAdReasonClass = interface(NSObjectClass)
    ['{F3EB1BFA-C7B0-4D80-B0DB-12A1A0C72A10}']
  end;

  GADMuteThisAdReason = interface(NSObject)
    ['{B6F0249A-C857-44C2-B60A-E9DFBA060721}']
    function reasonDescription: NSString; cdecl;
  end;
  TGADMuteThisAdReason = class(TOCGenericImport<GADMuteThisAdReasonClass, GADMuteThisAdReason>) end;

  GADNativeAdDelegate = interface(IObjectiveC)
    ['{236704E8-A3A8-4D57-9238-48FF75E94D03}']
    procedure nativeAdDidDismissScreen(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdDidRecordClick(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdDidRecordImpression(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdIsMuted(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdWillDismissScreen(nativeAd: GADNativeAd); cdecl;
    procedure nativeAdWillPresentScreen(nativeAd: GADNativeAd); cdecl;
  end;

  GADNativeAdClass = interface(NSObjectClass)
    ['{2DDFEF2C-4DCF-4D72-A6CF-4CAA615523FA}']
  end;

  GADNativeAd = interface(NSObject)
    ['{4C9C0E5D-FF39-4A7D-8AFB-2E2ADD33E4F5}']
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
    function price: NSString; cdecl;
    procedure recordCustomClickGesture; cdecl;
    procedure registerAdView(adView: UIView; clickableAssetViews: NSDictionary; nonclickableAssetViews: NSDictionary); cdecl;
    procedure registerClickConfirmingView(view: UIView); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
    procedure setUnconfirmedClickDelegate(unconfirmedClickDelegate: Pointer); cdecl;
    function starRating: NSDecimalNumber; cdecl;
    function store: NSString; cdecl;
    function unconfirmedClickDelegate: Pointer; cdecl;
    procedure unregisterAdView; cdecl;
  end;
  TGADNativeAd = class(TOCGenericImport<GADNativeAdClass, GADNativeAd>) end;

  GADNativeAdLoaderDelegate = interface(IObjectiveC)
    ['{A0EF2429-273A-4378-A61D-87B8C37A8952}']
    procedure adLoader(adLoader: GADAdLoader; didReceiveNativeAd: GADNativeAd); cdecl;
  end;

  GADNativeAdViewClass = interface(UIViewClass)
    ['{F9C2689F-41F1-4BA6-8622-3F7234C613DB}']
  end;

  GADNativeAdView = interface(UIView)
    ['{9195CD84-BB72-436A-9D8B-C4312E91FC0B}']
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
    ['{EE4CD6E7-7E98-4E58-8CAD-2AE3C2C13C2E}']
    procedure nativeAd(nativeAd: GADNativeAd; didReceiveUnconfirmedClickOnAssetID: GADNativeAssetIdentifier); cdecl;
    procedure nativeAdDidCancelUnconfirmedClick(nativeAd: GADNativeAd); cdecl;
  end;

  GADNativeAdImageAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{170FE6BE-401A-46FD-8369-9071056133B8}']
  end;

  GADNativeAdImageAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{075A302A-931B-4D55-BD1F-ECF69B2130C4}']
    function disableImageLoading: Boolean; cdecl;
    procedure setDisableImageLoading(disableImageLoading: Boolean); cdecl;
    procedure setShouldRequestMultipleImages(shouldRequestMultipleImages: Boolean); cdecl;
    function shouldRequestMultipleImages: Boolean; cdecl;
  end;
  TGADNativeAdImageAdLoaderOptions = class(TOCGenericImport<GADNativeAdImageAdLoaderOptionsClass, GADNativeAdImageAdLoaderOptions>) end;

  GADNativeAdMediaAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{866CC15D-53C6-4892-B3BF-99082B6AB969}']
  end;

  GADNativeAdMediaAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{A3AB8589-830C-4D31-888D-1F62BAB7D1AE}']
    function mediaAspectRatio: GADMediaAspectRatio; cdecl;
    procedure setMediaAspectRatio(mediaAspectRatio: GADMediaAspectRatio); cdecl;
  end;
  TGADNativeAdMediaAdLoaderOptions = class(TOCGenericImport<GADNativeAdMediaAdLoaderOptionsClass, GADNativeAdMediaAdLoaderOptions>) end;

  GADNativeAdViewAdOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{3136EEFD-11BF-4F35-8E94-6E5C2DC617D2}']
  end;

  GADNativeAdViewAdOptions = interface(GADAdLoaderOptions)
    ['{9075B9ED-C9E9-4312-BCFE-B44A60C990F3}']
    function preferredAdChoicesPosition: GADAdChoicesPosition; cdecl;
    procedure setPreferredAdChoicesPosition(preferredAdChoicesPosition: GADAdChoicesPosition); cdecl;
  end;
  TGADNativeAdViewAdOptions = class(TOCGenericImport<GADNativeAdViewAdOptionsClass, GADNativeAdViewAdOptions>) end;

  GADNativeMuteThisAdLoaderOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{1545D261-4AD7-4E11-B013-CFA7DE634CDF}']
  end;

  GADNativeMuteThisAdLoaderOptions = interface(GADAdLoaderOptions)
    ['{5F7E05D9-9FFA-4B13-B5BF-1790AEF91FAA}']
    function customMuteThisAdRequested: Boolean; cdecl;
    procedure setCustomMuteThisAdRequested(customMuteThisAdRequested: Boolean); cdecl;
  end;
  TGADNativeMuteThisAdLoaderOptions = class(TOCGenericImport<GADNativeMuteThisAdLoaderOptionsClass, GADNativeMuteThisAdLoaderOptions>) end;

  GADRewardedAdClass = interface(NSObjectClass)
    ['{453BD0BB-3975-4538-BBC1-FF84B5F196F6}']
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest; completionHandler: GADRewardedAdLoadCompletionHandler); cdecl;
  end;

  GADRewardedAd = interface(NSObject)
    ['{D12C8AB9-94B6-4366-A971-0FB800FDCB80}']
    function adReward: GADAdReward; cdecl;
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PPointer): Boolean; cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController; userDidEarnRewardHandler: GADUserDidEarnRewardHandler); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function serverSideVerificationOptions: GADServerSideVerificationOptions; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setServerSideVerificationOptions(serverSideVerificationOptions: GADServerSideVerificationOptions); cdecl;
  end;
  TGADRewardedAd = class(TOCGenericImport<GADRewardedAdClass, GADRewardedAd>) end;

  GADRewardedInterstitialAdClass = interface(NSObjectClass)
    ['{A61824DC-5CC7-4629-8D46-6B2904D34D48}']
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest;
      completionHandler: GADRewardedInterstitialAdLoadCompletionHandler); cdecl;
  end;

  GADRewardedInterstitialAd = interface(NSObject)
    ['{8806D5D9-DF54-4908-B833-8B107E0A27F3}']
    function adReward: GADAdReward; cdecl;
    function adUnitID: NSString; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: PPointer): Boolean; cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function paidEventHandler: GADPaidEventHandler; cdecl;
    procedure presentFromRootViewController(viewController: UIViewController; userDidEarnRewardHandler: GADUserDidEarnRewardHandler); cdecl;
    function responseInfo: GADResponseInfo; cdecl;
    function serverSideVerificationOptions: GADServerSideVerificationOptions; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    procedure setPaidEventHandler(paidEventHandler: GADPaidEventHandler); cdecl;
    procedure setServerSideVerificationOptions(serverSideVerificationOptions: GADServerSideVerificationOptions); cdecl;
  end;
  TGADRewardedInterstitialAd = class(TOCGenericImport<GADRewardedInterstitialAdClass, GADRewardedInterstitialAd>) end;

  GADSearchBannerViewClass = interface(GADBannerViewClass)
    ['{8E08E291-2706-45E0-AFF7-2332A8AB0509}']
  end;

  GADSearchBannerView = interface(GADBannerView)
    ['{83E3A060-11E9-4296-90DA-FCF494FE88B6}']
    function adSizeDelegate: Pointer; cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
  end;
  TGADSearchBannerView = class(TOCGenericImport<GADSearchBannerViewClass, GADSearchBannerView>) end;

  GADVideoControllerDelegate = interface(IObjectiveC)
    ['{4F07D9B7-372D-4B5B-86BA-6B7D469B3BAB}']
    procedure videoControllerDidEndVideoPlayback(videoController: GADVideoController); cdecl;
    procedure videoControllerDidMuteVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidPauseVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidPlayVideo(videoController: GADVideoController); cdecl;
    procedure videoControllerDidUnmuteVideo(videoController: GADVideoController); cdecl;
  end;

  GADVideoOptionsClass = interface(GADAdLoaderOptionsClass)
    ['{EB293BB0-66FB-48EC-80C4-4126FC55A6A8}']
  end;

  GADVideoOptions = interface(GADAdLoaderOptions)
    ['{6D10A7EE-847C-4496-93CF-F2502A86BF37}']
    function clickToExpandRequested: Boolean; cdecl;
    function customControlsRequested: Boolean; cdecl;
    procedure setClickToExpandRequested(clickToExpandRequested: Boolean); cdecl;
    procedure setCustomControlsRequested(customControlsRequested: Boolean); cdecl;
    procedure setStartMuted(startMuted: Boolean); cdecl;
    function startMuted: Boolean; cdecl;
  end;
  TGADVideoOptions = class(TOCGenericImport<GADVideoOptionsClass, GADVideoOptions>) end;

  GAMBannerAdLoaderDelegate = interface(IObjectiveC)
    ['{509C663B-1AF1-42FE-ABAB-B6B9CD232DA4}']
    procedure adLoader(adLoader: GADAdLoader; didReceiveGAMBannerView: GAMBannerView); cdecl;
    function validBannerSizesForAdLoader(adLoader: GADAdLoader): NSArray; cdecl;
  end;

  GAMBannerViewClass = interface(GADBannerViewClass)
    ['{725FD897-1114-41AC-92C7-A5DF01C95A04}']
  end;

  GAMBannerView = interface(GADBannerView)
    ['{4D3C49FE-C8D2-4F7C-8711-7FF8170DB926}']
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
    ['{C92CF1CB-AE90-42B5-AFB7-A0744FD7AD21}']
  end;

  GAMBannerViewOptions = interface(GADAdLoaderOptions)
    ['{53557A15-E87B-4EB3-8193-08C1BA6A5E53}']
    function enableManualImpressions: Boolean; cdecl;
    procedure setEnableManualImpressions(enableManualImpressions: Boolean); cdecl;
  end;
  TGAMBannerViewOptions = class(TOCGenericImport<GAMBannerViewOptionsClass, GAMBannerViewOptions>) end;

  GAMRequestClass = interface(GADRequestClass)
    ['{ACB36F44-4B3B-4314-980B-CEE778CE478A}']
  end;

  GAMRequest = interface(GADRequest)
    ['{02F2F1E8-F11B-407B-9ED9-B9ECE0ECE7EB}']
    function categoryExclusions: NSArray; cdecl;
    function customTargeting: NSDictionary; cdecl;
    function publisherProvidedID: NSString; cdecl;
    procedure setCategoryExclusions(categoryExclusions: NSArray); cdecl;
    procedure setCustomTargeting(customTargeting: NSDictionary); cdecl;
    procedure setPublisherProvidedID(publisherProvidedID: NSString); cdecl;
  end;
  TGAMRequest = class(TOCGenericImport<GAMRequestClass, GAMRequest>) end;

  GAMInterstitialAdClass = interface(GADInterstitialAdClass)
    ['{BA8DDBF5-6702-4178-AB7E-26E18482E405}']
    {class} procedure loadWithAdManagerAdUnitID(adUnitID: NSString; request: GAMRequest;
      completionHandler: GAMInterstitialAdLoadCompletionHandler); cdecl;
    {class} procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest;
      completionHandler: GADInterstitialAdLoadCompletionHandler); cdecl;
  end;

  GAMInterstitialAd = interface(GADInterstitialAd)
    ['{8CB547D8-14C5-41E4-8BCC-67B67D4DF84C}']
    function appEventDelegate: Pointer; cdecl;
    procedure setAppEventDelegate(appEventDelegate: Pointer); cdecl;
  end;
  TGAMInterstitialAd = class(TOCGenericImport<GAMInterstitialAdClass, GAMInterstitialAd>) end;

  GADMediationAdRequest = interface(IObjectiveC)
    ['{34D06044-36CA-47A4-9D12-DA889A9E5B12}']
    function childDirectedTreatment: NSNumber; cdecl;
    function credentials: NSDictionary; cdecl;
    function maxAdContentRating: GADMaxAdContentRating; cdecl;
    function networkExtras: Pointer; cdecl;
    function publisherId: NSString; cdecl;
    function testMode: Boolean; cdecl;
    function underAgeOfConsent: NSNumber; cdecl;
    function userBirthday: NSDate; cdecl;
    function userGender: GADGender; cdecl;
    function userHasLocation: Boolean; cdecl;
    function userKeywords: NSArray; cdecl;
    function userLatitude: CGFloat; cdecl;
    function userLocationAccuracyInMeters: CGFloat; cdecl;
    function userLocationDescription: NSString; cdecl;
    function userLongitude: CGFloat; cdecl;
  end;

  GADMAdNetworkConnector = interface(IObjectiveC)
    ['{C111C8E5-4AFC-45FC-A365-4A4FF1C842D3}']
    [MethodName('adapter:clickDidOccurInBanner:')]
    procedure adapterClickDidOccurInBanner(adapter: Pointer; clickDidOccurInBanner: UIView); cdecl;
    procedure adapterDidDismissFullScreenModal(adapter: Pointer); cdecl;
    procedure adapterDidDismissInterstitial(adapter: Pointer); cdecl;
    [MethodName('adapter:didFailAd:')]
    procedure adapterDidFailAd(adapter: Pointer; didFailAd: NSError); cdecl;
    [MethodName('adapter:didFailInterstitial:')]
    procedure adapterDidFailInterstitial(adapter: Pointer; didFailInterstitial: NSError); cdecl;
    procedure adapterDidGetAdClick(adapter: Pointer); cdecl;
    [MethodName('adapter:didReceiveAdView:')]
    procedure adapterDidReceiveAdView(adapter: Pointer; didReceiveAdView: UIView); cdecl;
    procedure adapterDidReceiveInterstitial(adapter: Pointer); overload; cdecl;
    [MethodName('adapter:didReceiveInterstitial:')]
    procedure adapterDidReceiveInterstitial(adapter: Pointer; didReceiveInterstitial: NSObject); overload; cdecl;
    [MethodName('adapter:didReceiveMediatedUnifiedNativeAd:')]
    procedure adapterDidReceiveMediatedUnifiedNativeAd(adapter: Pointer; didReceiveMediatedUnifiedNativeAd: Pointer); cdecl;
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
    ['{725E6AF6-0DC0-4950-B177-3E3A458E965E}']
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

  GADMRewardBasedVideoAdNetworkConnector = interface(IObjectiveC)
    ['{2EB8175E-D5E6-49DE-B6C5-972F56368E69}']
    procedure adapterDidCloseRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterDidCompletePlayingRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    [MethodName('adapter:didFailToLoadRewardBasedVideoAdwithError:')]
    procedure adapterDidFailToLoadRewardBasedVideoAdwithError(rewardBasedVideoAdAdapter: Pointer;
      didFailToLoadRewardBasedVideoAdwithError: NSError); cdecl;
    [MethodName('adapter:didFailToSetUpRewardBasedVideoAdWithError:')]
    procedure adapterDidFailToSetUpRewardBasedVideoAdWithError(rewardBasedVideoAdAdapter: Pointer;
      didFailToSetUpRewardBasedVideoAdWithError: NSError); cdecl;
    procedure adapterDidGetAdClick(adapter: Pointer); cdecl;
    procedure adapterDidOpenRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterDidReceiveRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    [MethodName('adapter:didRewardUserWithReward:')]
    procedure adapterDidRewardUserWithReward(rewardBasedVideoAd: Pointer; didRewardUserWithReward: GADAdReward); cdecl;
    procedure adapterDidSetUpRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterDidStartPlayingRewardBasedVideoAd(rewardBasedVideoAdAdapter: Pointer); cdecl;
    procedure adapterWillLeaveApplication(rewardBasedVideoAdAdapter: Pointer); cdecl;
  end;

  GADMRewardBasedVideoAdNetworkAdapter = interface(IObjectiveC)
    ['{2DE4FC3A-AF96-4445-BBBD-286344B5D437}']
    {class} function adapterVersion: NSString; cdecl;
    function initWithGADMAdNetworkConnector(connector: Pointer): Pointer; cdecl;
    function initWithRewardBasedVideoAdNetworkConnector(connector: Pointer; credentials: NSArray): Pointer; overload; cdecl;
    function initWithRewardBasedVideoAdNetworkConnector(connector: Pointer): Pointer; overload; cdecl;
    {class} function networkExtrasClass: Pointer; cdecl;
    procedure presentRewardBasedVideoAdWithRootViewController(viewController: UIViewController); cdecl;
    procedure requestRewardBasedVideoAd; cdecl;
    procedure setUp; cdecl;
    procedure setUpWithUserID(userID: NSString); cdecl;
    procedure stopBeingDelegate; cdecl;
  end;

  GADMediatedUnifiedNativeAdNotificationSourceClass = interface(NSObjectClass)
    ['{8396A768-E848-4DBF-AD71-A2CE7266220A}']
    {class} procedure mediatedNativeAdDidDismissScreen(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidEndVideoPlayback(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidPauseVideo(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidPlayVideo(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidRecordClick(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdDidRecordImpression(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdWillDismissScreen(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdWillLeaveApplication(mediatedNativeAd: Pointer); cdecl;
    {class} procedure mediatedNativeAdWillPresentScreen(mediatedNativeAd: Pointer); cdecl;
  end;

  GADMediatedUnifiedNativeAdNotificationSource = interface(NSObject)
    ['{FE55E5B1-8AC2-4356-AEC0-E19E07A72B2B}']
  end;
  TGADMediatedUnifiedNativeAdNotificationSource = class(TOCGenericImport<GADMediatedUnifiedNativeAdNotificationSourceClass,
    GADMediatedUnifiedNativeAdNotificationSource>) end;

  GADMediationAd = interface(IObjectiveC)
    ['{C647E77E-7B0B-48EC-901B-C07205C61CCD}']
  end;

  GADMediationCredentialsClass = interface(NSObjectClass)
    ['{2F1C61DC-0C2A-46B2-807C-6722C7548192}']
  end;

  GADMediationCredentials = interface(NSObject)
    ['{458237B6-DCF7-4A05-8DAB-AC9277C0CF90}']
    function format: GADAdFormat; cdecl;
    function settings: NSDictionary; cdecl;
  end;
  TGADMediationCredentials = class(TOCGenericImport<GADMediationCredentialsClass, GADMediationCredentials>) end;

  GADMediationServerConfigurationClass = interface(NSObjectClass)
    ['{4E90685C-5400-44AD-A07D-B9B52DE55172}']
  end;

  GADMediationServerConfiguration = interface(NSObject)
    ['{3464F033-5B4A-4367-B01E-B0DF64A36D0C}']
    function credentials: NSArray; cdecl;
  end;
  TGADMediationServerConfiguration = class(TOCGenericImport<GADMediationServerConfigurationClass, GADMediationServerConfiguration>) end;

  GADMediationAdConfigurationClass = interface(NSObjectClass)
    ['{71904540-F9F7-46FD-B93A-5B409FAAFAE2}']
  end;

  GADMediationAdConfiguration = interface(NSObject)
    ['{DD5E558A-F44F-4503-8D68-333C1DC69B18}']
    function bidResponse: NSString; cdecl;
    function childDirectedTreatment: NSNumber; cdecl;
    function credentials: GADMediationCredentials; cdecl;
    function extras: Pointer; cdecl;
    function hasUserLocation: Boolean; cdecl;
    function isTestRequest: Boolean; cdecl;
    function topViewController: UIViewController; cdecl;
    function userLatitude: CGFloat; cdecl;
    function userLocationAccuracyInMeters: CGFloat; cdecl;
    function userLongitude: CGFloat; cdecl;
    function watermark: NSData; cdecl;
  end;
  TGADMediationAdConfiguration = class(TOCGenericImport<GADMediationAdConfigurationClass, GADMediationAdConfiguration>) end;

  GADMediationAdEventDelegate = interface(IObjectiveC)
    ['{52D9A135-3284-481B-ACAC-77DD4902756B}']
    procedure didDismissFullScreenView; cdecl;
    procedure didFailToPresentWithError(error: NSError); cdecl;
    procedure reportClick; cdecl;
    procedure reportImpression; cdecl;
    procedure willDismissFullScreenView; cdecl;
    procedure willPresentFullScreenView; cdecl;
  end;

  GADMediationBannerAdEventDelegate = interface(IObjectiveC)
    ['{2A0ABECE-9404-4111-ADA5-7045D460C59E}']
    procedure willBackgroundApplication; cdecl;
  end;

  GADMediationInterstitialAdEventDelegate = interface(IObjectiveC)
    ['{B4F0899D-D682-4058-B95C-080F3D241030}']
    procedure willBackgroundApplication; cdecl;
  end;

  GADMediationNativeAdEventDelegate = interface(IObjectiveC)
    ['{D10130C9-4245-49FA-AD23-4A57C6DAFA93}']
    procedure didEndVideo; cdecl;
    procedure didMuteVideo; cdecl;
    procedure didPauseVideo; cdecl;
    procedure didPlayVideo; cdecl;
    procedure didUnmuteVideo; cdecl;
    procedure willBackgroundApplication; cdecl;
  end;

  GADMediationRewardedAdEventDelegate = interface(IObjectiveC)
    ['{01A0DDE2-F0A7-4AD6-8A92-045E832B5276}']
    procedure didEndVideo; cdecl;
    procedure didRewardUserWithReward(reward: GADAdReward); cdecl;
    procedure didStartVideo; cdecl;
  end;

  GADMediationBannerAd = interface(IObjectiveC)
    ['{ECA447E8-704E-43AD-869F-13D85C3D0FC0}']
    procedure changeAdSizeTo(adSize: GADAdSize); cdecl;
    function view: UIView; cdecl;
  end;

  GADMediationInterscrollerAd = interface(IObjectiveC)
    ['{A239863E-3B63-44ED-844F-7323E900DBD8}']
    function delegateInterscrollerEffect: Boolean; cdecl;
    procedure setDelegateInterscrollerEffect(delegateInterscrollerEffect: Boolean); cdecl;
  end;

  GADMediationBannerAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{1A06EE25-F398-42D8-B1BC-D10B34D00299}']
  end;

  GADMediationBannerAdConfiguration = interface(GADMediationAdConfiguration)
    ['{D753791C-0908-4B9C-AEE3-25DE8A3FBFDD}']
    function adSize: GADAdSize; cdecl;
  end;
  TGADMediationBannerAdConfiguration = class(TOCGenericImport<GADMediationBannerAdConfigurationClass, GADMediationBannerAdConfiguration>) end;

  GADMediationInterstitialAd = interface(IObjectiveC)
    ['{57041879-33EA-4BF3-AB48-756C5D94E65F}']
    procedure presentFromViewController(viewController: UIViewController); cdecl;
  end;

  GADMediationInterstitialAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{2E874B0B-37C0-428B-9EE1-C229FE7D7070}']
  end;

  GADMediationInterstitialAdConfiguration = interface(GADMediationAdConfiguration)
    ['{15F99702-6146-4D6C-860B-3E8144B566B3}']
  end;
  TGADMediationInterstitialAdConfiguration = class(TOCGenericImport<GADMediationInterstitialAdConfigurationClass,
    GADMediationInterstitialAdConfiguration>) end;

  GADMediationNativeAd = interface(IObjectiveC)
    ['{9427E40C-FF72-403D-8965-AC28243FD48C}']
    function handlesUserClicks: Boolean; cdecl;
    function handlesUserImpressions: Boolean; cdecl;
  end;

  GADMediationNativeAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{F2573B35-B704-4D2A-91BA-D61B5CA7BF40}']
  end;

  GADMediationNativeAdConfiguration = interface(GADMediationAdConfiguration)
    ['{BD1175C3-A2A2-4032-B807-205DFD269204}']
    function options: NSArray; cdecl;
  end;
  TGADMediationNativeAdConfiguration = class(TOCGenericImport<GADMediationNativeAdConfigurationClass, GADMediationNativeAdConfiguration>) end;

  GADMediationRewardedAd = interface(IObjectiveC)
    ['{C337406B-2508-4809-9CE2-9C5952DF5320}']
    procedure presentFromViewController(viewController: UIViewController); cdecl;
  end;

  GADMediationRewardedAdConfigurationClass = interface(GADMediationAdConfigurationClass)
    ['{86CB9258-F9EF-4021-B258-3C84CE26EDD4}']
  end;

  GADMediationRewardedAdConfiguration = interface(GADMediationAdConfiguration)
    ['{F82E3165-A512-4342-9E0F-22D81A67DE12}']
  end;
  TGADMediationRewardedAdConfiguration = class(TOCGenericImport<GADMediationRewardedAdConfigurationClass, GADMediationRewardedAdConfiguration>) end;

  GADMediationAdapter = interface(IObjectiveC)
    ['{79703A3D-8E21-4AA0-B110-63397641D768}']
    {class} function adapterVersion: GADVersionNumber; cdecl;
    {class} function adSDKVersion: GADVersionNumber; cdecl;
    function init: Pointer; cdecl;
    procedure loadBannerForAdConfiguration(adConfiguration: GADMediationBannerAdConfiguration;
      completionHandler: GADMediationBannerLoadCompletionHandler); cdecl;
    procedure loadInterscrollerAdForAdConfiguration(adConfiguration: GADMediationBannerAdConfiguration;
      completionHandler: GADMediationInterscrollerAdLoadCompletionHandler); cdecl;
    procedure loadInterstitialForAdConfiguration(adConfiguration: GADMediationInterstitialAdConfiguration;
      completionHandler: GADMediationInterstitialLoadCompletionHandler); cdecl;
    procedure loadNativeAdForAdConfiguration(adConfiguration: GADMediationNativeAdConfiguration;
      completionHandler: GADMediationNativeLoadCompletionHandler); cdecl;
    procedure loadRewardedAdForAdConfiguration(adConfiguration: GADMediationRewardedAdConfiguration;
      completionHandler: GADMediationRewardedLoadCompletionHandler); cdecl;
    procedure loadRewardedInterstitialAdForAdConfiguration(adConfiguration: GADMediationRewardedAdConfiguration;
      completionHandler: GADMediationRewardedLoadCompletionHandler); cdecl;
    {class} function networkExtrasClass: Pointer; cdecl;
    {class} procedure setUpWithConfiguration(configuration: GADMediationServerConfiguration;
      completionHandler: GADMediationAdapterSetUpCompletionBlock); cdecl;
  end;

  GADRTBMediationSignalsConfigurationClass = interface(NSObjectClass)
    ['{92BEEA4B-C4E4-4344-99EF-58AD1C3D38FF}']
  end;

  GADRTBMediationSignalsConfiguration = interface(NSObject)
    ['{5887FD8D-5517-4EAB-8CED-473CAD000F39}']
    function credentials: NSArray; cdecl;
  end;
  TGADRTBMediationSignalsConfiguration = class(TOCGenericImport<GADRTBMediationSignalsConfigurationClass, GADRTBMediationSignalsConfiguration>) end;

  GADRTBRequestParametersClass = interface(NSObjectClass)
    ['{7F36403F-A411-4FBC-ACE4-11E166BEE81F}']
  end;

  GADRTBRequestParameters = interface(NSObject)
    ['{8B1E25D3-7F06-4BD6-8FE9-D3F627FC4655}']
    function adSize: GADAdSize; cdecl;
    function configuration: GADRTBMediationSignalsConfiguration; cdecl;
    function extras: Pointer; cdecl;
  end;
  TGADRTBRequestParameters = class(TOCGenericImport<GADRTBRequestParametersClass, GADRTBRequestParameters>) end;

  GADRTBAdapter = interface(IObjectiveC)
    ['{09728F48-0D46-4D47-BA45-764CA135ECD0}']
    procedure collectSignalsForRequestParameters(params: GADRTBRequestParameters; completionHandler: GADRTBSignalCompletionHandler); cdecl;
    function init: Pointer; cdecl;
  end;

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

function CGSizeFromGADAdSize(size: GADAdSize): CGSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'CGSizeFromGADAdSize';

function IsGADAdSizeValid(size: GADAdSize): Boolean; cdecl;
  external framework libGoogleMobileAds name _PU + 'IsGADAdSizeValid';

function GADAdSizeIsFluid(size: GADAdSize): Boolean; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADAdSizeIsFluid';

function NSStringFromGADAdSize(size: GADAdSize): NSString; cdecl;
  external framework libGoogleMobileAds name _PU + 'NSStringFromGADAdSize';

function NSValueFromGADAdSize(size: GADAdSize): NSValue; cdecl;
  external framework libGoogleMobileAds name _PU + 'NSValueFromGADAdSize';

function GADAdSizeFromNSValue(value: NSValue): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADAdSizeFromNSValue';

function GADClosestValidSizeForAdSizes(original: GADAdSize; possibleAdSizes: NSArray): GADAdSize; cdecl;
  external framework libGoogleMobileAds name _PU + 'GADClosestValidSizeForAdSizes';

function kGADAdSizeBanner: GADAdSize;
function kGADAdSizeLargeBanner: GADAdSize;
function kGADAdSizeMediumRectangle: GADAdSize;
function kGADAdSizeFullBanner: GADAdSize;
function kGADAdSizeLeaderboard: GADAdSize;

implementation

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

procedure CLangRTLoader; cdecl; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
procedure GoogleAppMeasurementLoader; cdecl; external framework 'GoogleAppMeasurement' dependency 'sqlite3';
procedure GoogleAppMeasurementIdentitySupportLoader; cdecl; external framework 'GoogleAppMeasurementIdentitySupport';
procedure GoogleUtilitiesLoader; cdecl; external framework 'GoogleUtilities';
procedure JavaScriptCoreLoader; cdecl; external framework 'JavaScriptCore';
procedure nanoPBLoader; cdecl; external framework 'nanoPB';
procedure PromisesObjCLoader; cdecl; external framework 'PromisesObjC';
procedure UserMessagingPlatformLoader; cdecl; external framework 'UserMessagingPlatform';

end.