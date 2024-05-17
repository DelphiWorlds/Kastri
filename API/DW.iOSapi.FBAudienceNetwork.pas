unit DW.iOSapi.FBAudienceNetwork;

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

// Imported from Facebook iOS Audience Network SDK 5.3.2
//
// *** NOTE: This unit has not been verified as being 100% correct.
// Please report issues to: https://github.com/DelphiWorlds/KastriFree/issues

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.CoreMedia;

const
  FBNativeAdViewTagIcon = 5;
  FBNativeAdViewTagTitle = 6;
  FBNativeAdViewTagCoverImage = 7;
  FBNativeAdViewTagSubtitle = 8;
  FBNativeAdViewTagBody = 9;
  FBNativeAdViewTagCallToAction = 10;
  FBNativeAdViewTagSocialContext = 11;
  FBNativeAdViewTagChoicesIcon = 12;
  FBNativeAdViewTagMedia = 13;
  FBAdLogLevelNone = 0;
  FBAdLogLevelNotification = 1;
  FBAdLogLevelError = 2;
  FBAdLogLevelWarning = 3;
  FBAdLogLevelLog = 4;
  FBAdLogLevelDebug = 5;
  FBAdLogLevelVerbose = 6;
  FBMediaViewRenderingMethodDefault = 0;
  FBMediaViewRenderingMethodMetal = 1;
  FBMediaViewRenderingMethodOpenGL = 2;
  FBMediaViewRenderingMethodSoftware = 3;
  FBAdTestAdType_Default = 0;
  FBAdTestAdType_Img_16_9_App_Install = 1;
  FBAdTestAdType_Img_16_9_Link = 2;
  FBAdTestAdType_Vid_HD_16_9_46s_App_Install = 3;
  FBAdTestAdType_Vid_HD_16_9_46s_Link = 4;
  FBAdTestAdType_Vid_HD_16_9_15s_App_Install = 5;
  FBAdTestAdType_Vid_HD_16_9_15s_Link = 6;
  FBAdTestAdType_Vid_HD_9_16_39s_App_Install = 7;
  FBAdTestAdType_Vid_HD_9_16_39s_Link = 8;
  FBAdTestAdType_Carousel_Img_Square_App_Install = 9;
  FBAdTestAdType_Carousel_Img_Square_Link = 10;
  FBAdFormatTypeNameUnknown = 0;
  FBAdFormatTypeNameBanner = 1;
  FBAdFormatTypeNameInterstitial = 2;
  FBAdFormatTypeNameInstream = 3;
  FBAdFormatTypeNameNative = 4;
  FBAdFormatTypeNameNativeBanner = 5;
  FBAdFormatTypeNameRewardedVideo = 6;
  FBAdFormatTypeUnknown = 0;
  FBAdFormatTypeImage = 1;
  FBAdFormatTypeVideo = 2;
  FBAdFormatTypeCarousel = 3;
  FBNativeAdsCachePolicyNone = 0;
  FBNativeAdsCachePolicyAll = 1;
  FBNativeAdViewTypeGenericHeight300 = 3;
  FBNativeAdViewTypeGenericHeight400 = 4;
  FBNativeAdViewTypeDynamic = 6;
  FBNativeBannerAdViewTypeGenericHeight100 = 1;
  FBNativeBannerAdViewTypeGenericHeight120 = 2;
  FBNativeBannerAdViewTypeGenericHeight50 = 5;

type
  FBAdChoicesView = interface;
  FBAdExtraHint = interface;
  FBMediaViewVideoRenderer = interface;
  FBMediaView = interface;
  FBMediaViewDelegate = interface;
  FBAdIconView = interface;
  FBAdImage = interface;
  FBAdOptionsView = interface;
  FBAdSettings = interface;
  FBAdLoggingDelegate = interface;
  FBAdView = interface;
  FBAdViewDelegate = interface;
  FBAdInitSettings = interface;
  FBAdInitResults = interface;
  FBAudienceNetworkAds = interface;
  FBInstreamAdView = interface;
  FBInstreamAdViewDelegate = interface;
  FBInterstitialAd = interface;
  FBInterstitialAdDelegate = interface;
  FBNativeAdBase = interface;
  FBNativeAd = interface;
  FBNativeAdDelegate = interface;
  FBNativeAdsManagerDelegate = interface;
  FBNativeAdsManager = interface;
  FBNativeAdCollectionViewAdProvider = interface;
  FBNativeAdBaseView = interface;
  FBNativeAdViewAttributes = interface;
  FBNativeAdView = interface;
  FBNativeAdCollectionViewCellProvider = interface;
  FBNativeAdScrollView = interface;
  FBNativeAdTableViewAdProvider = interface;
  FBNativeAdTableViewCellProvider = interface;
  FBNativeBannerAd = interface;
  FBNativeBannerAdDelegate = interface;
  FBNativeBannerAdView = interface;
  FBRewardedVideoAd = interface;
  FBRewardedVideoAdDelegate = interface;

  FBNativeAdViewTag = NSInteger;
  FBAdExtraHintKeyword = NSString;
  FBAdLogLevel = NSInteger;
  FBMediaViewRenderingMethod = NSInteger;
  FBAdTestAdType = NSInteger;
  FBAdFormatTypeName = NSInteger;
  FBAdFormatType = NSInteger;
  FBNativeAdsCachePolicy = NSInteger;
  FBNativeAdViewType = NSInteger;
  FBNativeBannerAdViewType = NSInteger;

  FBAdSize = record
    size: CGSize;
  end;

  TFBMediaViewVideoRendererBlockMethod1 = procedure(time: CMTime) of object;
  TFBAdImageBlockMethod1 = procedure(image: UIImage) of object;
  TFBAudienceNetworkAdsBlockMethod1 = procedure(results: FBAdInitResults) of object;
  TFBNativeAdScrollViewBlockMethod1 = procedure(nativeAd: FBNativeAd; position: NSUInteger) of object;

  FBAdChoicesViewClass = interface(UIViewClass)
    ['{A25D68AE-DF5F-4280-929B-E6E95805DE8D}']
  end;

  FBAdChoicesView = interface(UIView)
    ['{23D9FB85-D405-4191-A0C7-7E179073C309}']
    function &label: UILabel; cdecl;
    function corner: UIRectCorner; cdecl;
    [MethodName('initWithNativeAd:expandable:attributes:')]
    function initWithNativeAd(nativeAd: FBNativeAdBase; expandable: Boolean; attributes: FBNativeAdViewAttributes): Pointer; overload; cdecl;
    [MethodName('initWithNativeAd:expandable:')]
    function initWithNativeAd(nativeAd: FBNativeAdBase; expandable: Boolean): Pointer; overload; cdecl;
    function initWithNativeAd(nativeAd: FBNativeAdBase): Pointer; overload; cdecl;
    function insets: UIEdgeInsets; cdecl;
    function isBackgroundShown: Boolean; cdecl;
    function isExpandable: Boolean; cdecl;
    function nativeAd: FBNativeAdBase; cdecl;
    function nativeAdViewTag: FBNativeAdViewTag; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setBackgroundShown(backgroundShown: Boolean); cdecl;
    procedure setCorner(corner: UIRectCorner); cdecl;
    procedure setInsets(insets: UIEdgeInsets); cdecl;
    procedure setNativeAd(nativeAd: FBNativeAdBase); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
    [MethodName('updateFrameFromSuperview:insets:')]
    procedure updateFrameFromSuperview(corner: UIRectCorner; insets: UIEdgeInsets); overload; cdecl;
    procedure updateFrameFromSuperview; overload; cdecl;
    procedure updateFrameFromSuperview(corner: UIRectCorner); overload; cdecl;
  end;
  TFBAdChoicesView = class(TOCGenericImport<FBAdChoicesViewClass, FBAdChoicesView>) end;

  FBAdExtraHintClass = interface(NSObjectClass)
    ['{D334B249-FBFB-4D8A-9FD1-F9434DEC5D2C}']
  end;

  FBAdExtraHint = interface(NSObject)
    ['{ED1F6CF2-610D-4F17-B9E4-C2B30ED79E72}']
    procedure addKeyword(keyword: FBAdExtraHintKeyword); cdecl;
    function contentURL: NSString; cdecl;
    function extraData: NSString; cdecl;
    function initWithKeywords(keywords: NSArray): Pointer; cdecl;
    function mediationData: NSString; cdecl;
    procedure removeKeyword(keyword: FBAdExtraHintKeyword); cdecl;
    procedure setContentURL(contentURL: NSString); cdecl;
    procedure setExtraData(extraData: NSString); cdecl;
    procedure setMediationData(mediationData: NSString); cdecl;
  end;
  TFBAdExtraHint = class(TOCGenericImport<FBAdExtraHintClass, FBAdExtraHint>) end;

  FBMediaViewVideoRendererClass = interface(UIViewClass)
    ['{FE364F09-F1C6-4CBE-A37F-1FD7A97FC073}']
  end;

  FBMediaViewVideoRenderer = interface(UIView)
    ['{5BB9535D-BD91-442F-808B-225A8B49DC16}']
    [MethodName('addPeriodicTimeObserverForInterval:queue:usingBlock:')]
    function addPeriodicTimeObserverForInterval(interval: CMTime; queue: dispatch_queue_t;
      block: TFBMediaViewVideoRendererBlockMethod1): Pointer; cdecl;
    function aspectRatio: CGFloat; cdecl;
    function currentTime: CMTime; cdecl;
    procedure disengageVideoSeek; cdecl;
    function duration: CMTime; cdecl;
    procedure engageVideoSeek; cdecl;
    function isPlaying: Boolean; cdecl;
    procedure pauseVideo; cdecl;
    procedure playVideo; cdecl;
    procedure removeTimeObserver(observer: Pointer); cdecl;
    procedure seekVideoToTime(time: CMTime); cdecl;
    procedure setVolume(volume: Single); cdecl;
    procedure videoDidChangeVolume; cdecl;
    procedure videoDidDisengageSeek; cdecl;
    procedure videoDidEnd; cdecl;
    procedure videoDidEngageSeek; cdecl;
    procedure videoDidFailWithError(error: NSError); cdecl;
    procedure videoDidLoad; cdecl;
    procedure videoDidPause; cdecl;
    procedure videoDidPlay; cdecl;
    procedure videoDidSeek; cdecl;
    function volume: Single; cdecl;
  end;
  TFBMediaViewVideoRenderer = class(TOCGenericImport<FBMediaViewVideoRendererClass, FBMediaViewVideoRenderer>) end;

  FBMediaViewClass = interface(UIViewClass)
    ['{CA969C27-3FF5-453A-B741-59B5C822E532}']
  end;

  FBMediaView = interface(UIView)
    ['{A3DC9923-4DDA-4D8E-BA81-CEA30B48E8C0}']
    procedure applyNaturalHeight; cdecl;
    procedure applyNaturalWidth; cdecl;
    function aspectRatio: CGFloat; cdecl;
    function delegate: Pointer; cdecl;
    function isAutoplayEnabled: Boolean; cdecl;
    function nativeAdViewTag: FBNativeAdViewTag; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setVideoRenderer(videoRenderer: FBMediaViewVideoRenderer); cdecl;
    function videoRenderer: FBMediaViewVideoRenderer; cdecl;
    function volume: Single; cdecl;
  end;
  TFBMediaView = class(TOCGenericImport<FBMediaViewClass, FBMediaView>) end;

  FBMediaViewDelegate = interface(IObjectiveC)
    ['{042CC350-4D3C-4570-81E7-87CAB7E79E34}']
    [MethodName('mediaView:videoVolumeDidChange:')]
    procedure mediaView(mediaView: FBMediaView; volume: Single); cdecl;
    procedure mediaViewDidExitFullscreen(mediaView: FBMediaView); cdecl;
    procedure mediaViewDidLoad(mediaView: FBMediaView); cdecl;
    procedure mediaViewVideoDidComplete(mediaView: FBMediaView); cdecl;
    procedure mediaViewVideoDidPause(mediaView: FBMediaView); cdecl;
    procedure mediaViewVideoDidPlay(mediaView: FBMediaView); cdecl;
    procedure mediaViewWillEnterFullscreen(mediaView: FBMediaView); cdecl;
  end;

  FBAdIconViewClass = interface(FBMediaViewClass)
    ['{C3DD6A2C-2FFC-4C12-A66E-1C27D866A3EB}']
  end;

  FBAdIconView = interface(FBMediaView)
    ['{9A571F5F-C184-4AAE-99BC-3A2D25552970}']
    function nativeAdViewTag: FBNativeAdViewTag; cdecl;
  end;
  TFBAdIconView = class(TOCGenericImport<FBAdIconViewClass, FBAdIconView>) end;

  FBAdImageClass = interface(NSObjectClass)
    ['{62EAF218-2B5D-477B-B112-CBA3E4478AEC}']
  end;

  FBAdImage = interface(NSObject)
    ['{7E47F2F8-F30A-4CF5-80F8-B479FC74E9C9}']
    function height: NSInteger; cdecl;
    [MethodName('initWithURL:width:height:')]
    function initWithURL(url: NSURL; width: NSInteger; height: NSInteger): Pointer; cdecl;
    procedure loadImageAsyncWithBlock(block: TFBAdImageBlockMethod1); cdecl;
    function url: NSURL; cdecl;
    function width: NSInteger; cdecl;
  end;
  TFBAdImage = class(TOCGenericImport<FBAdImageClass, FBAdImage>) end;

  FBAdOptionsViewClass = interface(UIViewClass)
    ['{3C2FA9D4-36ED-4904-84DC-293C74FF71C2}']
  end;

  FBAdOptionsView = interface(UIView)
    ['{64A0F330-6DB4-4D0B-BD32-8BEA5532017E}']
    function foregroundColor: UIColor; cdecl;
    function nativeAd: FBNativeAdBase; cdecl;
    procedure setForegroundColor(foregroundColor: UIColor); cdecl;
    procedure setNativeAd(nativeAd: FBNativeAdBase); cdecl;
    procedure setUseSingleIcon(useSingleIcon: Boolean); cdecl;
    function useSingleIcon: Boolean; cdecl;
  end;
  TFBAdOptionsView = class(TOCGenericImport<FBAdOptionsViewClass, FBAdOptionsView>) end;

  FBAdSettingsClass = interface(NSObjectClass)
    ['{DE300CC4-EF44-4A7A-BE72-AB34BE4704E6}']
    {class} procedure addTestDevice(deviceHash: NSString); cdecl;
    {class} procedure addTestDevices(devicesHash: NSArray); cdecl;
    {class} function bidderToken: NSString; cdecl;
    {class} procedure clearTestDevice(deviceHash: NSString); cdecl;
    {class} procedure clearTestDevices; cdecl;
    {class} function getLogLevel: FBAdLogLevel; cdecl;
    {class} function isBackgroundVideoPlaybackAllowed: Boolean; cdecl;
    {class} function isTestMode: Boolean; cdecl;
    {class} function loggingDelegate: Pointer; cdecl;
    {class} function mediaViewRenderingMethod: FBMediaViewRenderingMethod; cdecl;
    {class} function routingToken: NSString; cdecl;
    {class} procedure setBackgroundVideoPlaybackAllowed(backgroundVideoPlaybackAllowed: Boolean); cdecl;
    {class} procedure setIsChildDirected(isChildDirected: Boolean); cdecl;
    {class} procedure setLoggingDelegate(loggingDelegate: Pointer); cdecl;
    {class} procedure setLogLevel(level: FBAdLogLevel); cdecl;
    {class} procedure setMediationService(service: NSString); cdecl;
    {class} procedure setMediaViewRenderingMethod(mediaViewRenderingMethod: FBMediaViewRenderingMethod); cdecl;
    {class} procedure setTestAdType(testAdType: FBAdTestAdType); cdecl;
    {class} procedure setUrlPrefix(urlPrefix: NSString); cdecl;
    {class} function testAdType: FBAdTestAdType; cdecl;
    {class} function testDeviceHash: NSString; cdecl;
    {class} function urlPrefix: NSString; cdecl;
  end;

  FBAdSettings = interface(NSObject)
    ['{09AC90E1-3B02-498D-9C27-CFBBBBAFF7C6}']
  end;
  TFBAdSettings = class(TOCGenericImport<FBAdSettingsClass, FBAdSettings>) end;

  FBAdLoggingDelegate = interface(IObjectiveC)
    ['{C4186E22-07E6-4FD0-94DF-AAAD0DC0D6CD}']
    [MethodName('logAtLevel:withFileName:withLineNumber:withThreadId:withBody:')]
    procedure logAtLevel(level: FBAdLogLevel; fileName: NSString; lineNumber: Integer; threadId: Integer; body: NSString); cdecl;
  end;

  FBAdViewClass = interface(UIViewClass)
    ['{64408004-25BC-4350-9A89-434DB65D5D08}']
  end;

  FBAdView = interface(UIView)
    ['{14BD43AC-6D93-48A5-9BC7-865439E970DC}']
    function delegate: Pointer; cdecl;
    procedure disableAutoRefresh; cdecl;
    function extraHint: FBAdExtraHint; cdecl;
    [MethodName('initWithPlacementID:adSize:rootViewController:')]
    function initWithPlacementID(placementID: NSString; adSize: FBAdSize; rootViewController: UIViewController): Pointer; overload; cdecl;
    [MethodName('initWithPlacementID:bidPayload:rootViewController:error:')]
    function initWithPlacementID(placementID: NSString; bidPayload: NSString; rootViewController: UIViewController;
      error: PPointer): Pointer; overload; cdecl;
    function isAdValid: Boolean; cdecl;
    procedure loadAd; cdecl;
    procedure loadAdWithBidPayload(bidPayload: NSString); cdecl;
    function placementID: NSString; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setExtraHint(extraHint: FBAdExtraHint); cdecl;
  end;
  TFBAdView = class(TOCGenericImport<FBAdViewClass, FBAdView>) end;

  FBAdViewDelegate = interface(IObjectiveC)
    ['{176F4030-D00E-4655-A0DD-DEBD06C41804}']
    [MethodName('adView:didFailWithError:')]
    procedure adView(adView: FBAdView; error: NSError); cdecl;
    procedure adViewDidClick(adView: FBAdView); cdecl;
    procedure adViewDidFinishHandlingClick(adView: FBAdView); cdecl;
    procedure adViewDidLoad(adView: FBAdView); cdecl;
    procedure adViewWillLogImpression(adView: FBAdView); cdecl;
    function viewControllerForPresentingModalView: UIViewController; cdecl;
  end;

  FBAdInitSettingsClass = interface(NSObjectClass)
    ['{BB65FCBC-9E0D-4F5A-80B5-035A8069DA9D}']
  end;

  FBAdInitSettings = interface(NSObject)
    ['{A0E3D325-A1D9-4533-9DF5-BCEF8BE448F7}']
    [MethodName('initWithPlacementIDs:mediationService:')]
    function initWithPlacementIDs(placementIDs: NSArray; mediationService: NSString): Pointer; cdecl;
    function mediationService: NSString; cdecl;
    function placementIDs: NSArray; cdecl;
  end;
  TFBAdInitSettings = class(TOCGenericImport<FBAdInitSettingsClass, FBAdInitSettings>) end;

  FBAdInitResultsClass = interface(NSObjectClass)
    ['{38FBF56B-109E-4C79-A5A1-36AE5768F18B}']
  end;

  FBAdInitResults = interface(NSObject)
    ['{7F667B27-2C0C-4639-ABAB-1DD7258D3CA9}']
    function isSuccess: Boolean; cdecl;
    function message: NSString; cdecl;
  end;
  TFBAdInitResults = class(TOCGenericImport<FBAdInitResultsClass, FBAdInitResults>) end;

  FBAudienceNetworkAdsClass = interface(NSObjectClass)
    ['{0EA52FAD-BBD2-4D66-A0C6-F9D7E3F69702}']
    {class} function adFormatTypeNameForPlacementId(placementId: NSString): FBAdFormatTypeName; cdecl;
    [MethodName('initializeWithSettings:completionHandler:')]
    {class} procedure initializeWithSettings(settings: FBAdInitSettings; completionHandler: TFBAudienceNetworkAdsBlockMethod1); cdecl;
  end;

  FBAudienceNetworkAds = interface(NSObject)
    ['{D50CD6EB-63E8-4E2E-BB76-9A886CD3FF96}']
  end;
  TFBAudienceNetworkAds = class(TOCGenericImport<FBAudienceNetworkAdsClass, FBAudienceNetworkAds>) end;

  FBInstreamAdViewClass = interface(UIViewClass)
    ['{ED56AE84-C775-4674-B9CE-68ABD1F384E6}']
  end;

  FBInstreamAdView = interface(UIView)
    ['{F1E6C10E-765B-4E8F-B96C-32ADF53AD65F}']
    function delegate: Pointer; cdecl;
    function extraHint: FBAdExtraHint; cdecl;
    function initWithPlacementID(placementID: NSString): Pointer; cdecl;
    function isAdValid: Boolean; cdecl;
    procedure loadAd; cdecl;
    procedure loadAdWithBidPayload(bidPayload: NSString); cdecl;
    function placementID: NSString; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setExtraHint(extraHint: FBAdExtraHint); cdecl;
    function showAdFromRootViewController(rootViewController: UIViewController): Boolean; cdecl;
  end;
  TFBInstreamAdView = class(TOCGenericImport<FBInstreamAdViewClass, FBInstreamAdView>) end;

  FBInstreamAdViewDelegate = interface(IObjectiveC)
    ['{C13353E0-56EF-4B7F-A500-5834882F463C}']
    [MethodName('adView:didFailWithError:')]
    procedure adView(adView: FBInstreamAdView; error: NSError); cdecl;
    procedure adViewDidClick(adView: FBInstreamAdView); cdecl;
    procedure adViewDidEnd(adView: FBInstreamAdView); cdecl;
    procedure adViewDidLoad(adView: FBInstreamAdView); cdecl;
    procedure adViewWillLogImpression(adView: FBInstreamAdView); cdecl;
  end;

  FBInterstitialAdClass = interface(NSObjectClass)
    ['{289D5A4C-7C15-4F58-94EF-B95EB6581EAF}']
  end;

  FBInterstitialAd = interface(NSObject)
    ['{16A71117-2105-4BE1-92AA-80392AFBBA48}']
    function delegate: Pointer; cdecl;
    function extraHint: FBAdExtraHint; cdecl;
    function initWithPlacementID(placementID: NSString): Pointer; cdecl;
    function isAdValid: Boolean; cdecl;
    procedure loadAd; cdecl;
    procedure loadAdWithBidPayload(bidPayload: NSString); cdecl;
    function placementID: NSString; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setExtraHint(extraHint: FBAdExtraHint); cdecl;
    function showAdFromRootViewController(rootViewController: UIViewController): Boolean; cdecl;
  end;
  TFBInterstitialAd = class(TOCGenericImport<FBInterstitialAdClass, FBInterstitialAd>) end;

  FBInterstitialAdDelegate = interface(IObjectiveC)
    ['{0A2C6458-8714-4C1D-9722-60F683D9C2DD}']
    [MethodName('interstitialAd:didFailWithError:')]
    procedure interstitialAd(interstitialAd: FBInterstitialAd; error: NSError); cdecl;
    procedure interstitialAdDidClick(interstitialAd: FBInterstitialAd); cdecl;
    procedure interstitialAdDidClose(interstitialAd: FBInterstitialAd); cdecl;
    procedure interstitialAdDidLoad(interstitialAd: FBInterstitialAd); cdecl;
    procedure interstitialAdWillClose(interstitialAd: FBInterstitialAd); cdecl;
    procedure interstitialAdWillLogImpression(interstitialAd: FBInterstitialAd); cdecl;
  end;

  FBNativeAdBaseClass = interface(NSObjectClass)
    ['{99833914-5ABA-4D42-849E-7B0EE96C2061}']
    [MethodName('nativeAdWithPlacementId:bidPayload:error:')]
    {class} function nativeAdWithPlacementId(placementId: NSString; bidPayload: NSString; error: PPointer): Pointer; cdecl;
  end;

  FBNativeAdBase = interface(NSObject)
    ['{9C8A7F61-4C4A-4EAF-832A-A8FCF208C057}']
    function adChoicesIcon: FBAdImage; cdecl;
    function adChoicesLinkURL: NSURL; cdecl;
    function adChoicesText: NSString; cdecl;
    function adFormatType: FBAdFormatType; cdecl;
    function adTranslation: NSString; cdecl;
    function advertiserName: NSString; cdecl;
    function aspectRatio: CGFloat; cdecl;
    function bodyText: NSString; cdecl;
    function callToAction: NSString; cdecl;
    function extraHint: FBAdExtraHint; cdecl;
    function headline: NSString; cdecl;
    function isAdValid: Boolean; cdecl;
    function isRegistered: Boolean; cdecl;
    function linkDescription: NSString; cdecl;
    procedure loadAd; cdecl;
    procedure loadAdWithBidPayload(bidPayload: NSString); overload; cdecl;
    [MethodName('loadAdWithBidPayload:mediaCachePolicy:')]
    procedure loadAdWithBidPayload(bidPayload: NSString; mediaCachePolicy: FBNativeAdsCachePolicy); overload; cdecl;
    procedure loadAdWithMediaCachePolicy(mediaCachePolicy: FBNativeAdsCachePolicy); cdecl;
    function mediaCachePolicy: FBNativeAdsCachePolicy; cdecl;
    function placementID: NSString; cdecl;
    function promotedTranslation: NSString; cdecl;
    function rawBodyText: NSString; cdecl;
    procedure setExtraHint(extraHint: FBAdExtraHint); cdecl;
    function socialContext: NSString; cdecl;
    function sponsoredTranslation: NSString; cdecl;
    procedure unregisterView; cdecl;
  end;
  TFBNativeAdBase = class(TOCGenericImport<FBNativeAdBaseClass, FBNativeAdBase>) end;

  FBNativeAdClass = interface(FBNativeAdBaseClass)
    ['{CC03A93A-7DEB-4944-94F3-8512FDCCCDC9}']
  end;

  FBNativeAd = interface(FBNativeAdBase)
    ['{B7400386-12F2-4DF6-B2AA-5E9B94E29BAA}']
    function delegate: Pointer; cdecl;
    procedure downloadMedia; cdecl;
    function initWithPlacementID(placementID: NSString): Pointer; cdecl;
    [MethodName('registerViewForInteraction:mediaView:iconImageView:viewController:clickableViews:')]
    procedure registerViewForInteraction(view: UIView; mediaView: FBMediaView; iconImageView: UIImageView;
      viewController: UIViewController; clickableViews: NSArray); overload; cdecl;
    [MethodName('registerViewForInteraction:mediaView:iconView:viewController:')]
    procedure registerViewForInteraction(view: UIView; mediaView: FBMediaView; iconView: FBMediaView;
      viewController: UIViewController); overload; cdecl;
    [MethodName('registerViewForInteraction:mediaView:iconView:viewController:clickableViews:')]
    procedure registerViewForInteraction(view: UIView; mediaView: FBMediaView; iconView: FBMediaView;
      viewController: UIViewController; clickableViews: NSArray); overload; cdecl;
    [MethodName('registerViewForInteraction:mediaView:iconImageView:viewController:')]
    procedure registerViewForInteraction(view: UIView; mediaView: FBMediaView; iconImageView: UIImageView;
      viewController: UIViewController); overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TFBNativeAd = class(TOCGenericImport<FBNativeAdClass, FBNativeAd>) end;

  FBNativeAdDelegate = interface(IObjectiveC)
    ['{F5A1271D-13A9-41E6-8252-32FA668E65AC}']
    [MethodName('nativeAd:didFailWithError:')]
    procedure nativeAd(nativeAd: FBNativeAd; error: NSError); cdecl;
    procedure nativeAdDidClick(nativeAd: FBNativeAd); cdecl;
    procedure nativeAdDidDownloadMedia(nativeAd: FBNativeAd); cdecl;
    procedure nativeAdDidFinishHandlingClick(nativeAd: FBNativeAd); cdecl;
    procedure nativeAdDidLoad(nativeAd: FBNativeAd); cdecl;
    procedure nativeAdWillLogImpression(nativeAd: FBNativeAd); cdecl;
  end;

  FBNativeAdsManagerDelegate = interface(IObjectiveC)
    ['{182462C8-0ADD-4E62-9F92-D76A473D098D}']
    procedure nativeAdsFailedToLoadWithError(error: NSError); cdecl;
    procedure nativeAdsLoaded; cdecl;
  end;

  FBNativeAdsManagerClass = interface(NSObjectClass)
    ['{C33F9C09-3A44-4C75-A137-BBE7EF580069}']
  end;

  FBNativeAdsManager = interface(NSObject)
    ['{359D6C69-82A8-42DE-9E53-849D534FD760}']
    function delegate: Pointer; cdecl;
    procedure disableAutoRefresh; cdecl;
    function extraHint: FBAdExtraHint; cdecl;
    [MethodName('initWithPlacementID:forNumAdsRequested:')]
    function initWithPlacementID(placementID: NSString; numAdsRequested: NSUInteger): Pointer; cdecl;
    function isValid: Boolean; cdecl;
    procedure loadAds; cdecl;
    function mediaCachePolicy: FBNativeAdsCachePolicy; cdecl;
    function nextNativeAd: FBNativeAd; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setExtraHint(extraHint: FBAdExtraHint); cdecl;
    procedure setMediaCachePolicy(mediaCachePolicy: FBNativeAdsCachePolicy); cdecl;
    function uniqueNativeAdCount: NSUInteger; cdecl;
  end;
  TFBNativeAdsManager = class(TOCGenericImport<FBNativeAdsManagerClass, FBNativeAdsManager>) end;

  FBNativeAdCollectionViewAdProviderClass = interface(NSObjectClass)
    ['{83E5E88A-5814-4B3C-A670-58A21D6FB7EA}']
  end;

  FBNativeAdCollectionViewAdProvider = interface(NSObject)
    ['{986BC18D-E4CB-4572-BD56-192FED5345F5}']
    [MethodName('adjustCount:forStride:')]
    function adjustCount(count: NSUInteger; stride: NSUInteger): NSUInteger; cdecl;
    [MethodName('adjustNonAdCellIndexPath:forStride:')]
    function adjustNonAdCellIndexPath(indexPath: NSIndexPath; stride: NSUInteger): NSIndexPath; cdecl;
    [MethodName('collectionView:nativeAdForRowAtIndexPath:')]
    function collectionView(collectionView: UICollectionView; indexPath: NSIndexPath): FBNativeAd; cdecl;
    function delegate: Pointer; cdecl;
    function extraHint: FBAdExtraHint; cdecl;
    function initWithManager(manager: FBNativeAdsManager): Pointer; cdecl;
    [MethodName('isAdCellAtIndexPath:forStride:')]
    function isAdCellAtIndexPath(indexPath: NSIndexPath; stride: NSUInteger): Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setExtraHint(extraHint: FBAdExtraHint); cdecl;
  end;
  TFBNativeAdCollectionViewAdProvider = class(TOCGenericImport<FBNativeAdCollectionViewAdProviderClass, FBNativeAdCollectionViewAdProvider>) end;

  FBNativeAdBaseViewClass = interface(UIViewClass)
    ['{ACC05483-DC6A-4AD8-AF46-A3DB96F8256D}']
  end;

  FBNativeAdBaseView = interface(UIView)
    ['{88B643E5-527D-4E7F-9A41-94E9C662FB28}']
    function rootViewController: UIViewController; cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
  end;
  TFBNativeAdBaseView = class(TOCGenericImport<FBNativeAdBaseViewClass, FBNativeAdBaseView>) end;

  FBNativeAdViewAttributesClass = interface(NSObjectClass)
    ['{EA388DE5-8ABA-447A-BFC5-23A834BE0970}']
    {class} function defaultAttributesForBannerType(&type: FBNativeBannerAdViewType): Pointer; cdecl;
    {class} function defaultAttributesForType(&type: FBNativeAdViewType): Pointer; cdecl;
  end;

  FBNativeAdViewAttributes = interface(NSObject)
    ['{BE6582FC-0F05-4CAC-9232-144CD812650C}']
    function adChoicesForegroundColor: UIColor; cdecl;
    function advertiserNameColor: UIColor; cdecl;
    function backgroundColor: UIColor; cdecl;
    function buttonBorderColor: UIColor; cdecl;
    function buttonColor: UIColor; cdecl;
    function buttonTitleColor: UIColor; cdecl;
    function buttonTitleFont: UIFont; cdecl;
    function descriptionColor: UIColor; cdecl;
    function descriptionFont: UIFont; cdecl;
    function initWithDictionary(dict: NSDictionary): Pointer; cdecl;
    function isAutoplayEnabled: Boolean; cdecl;
    procedure setAdChoicesForegroundColor(adChoicesForegroundColor: UIColor); cdecl;
    procedure setAdvertiserNameColor(advertiserNameColor: UIColor); cdecl;
    procedure setAutoplayEnabled(autoplayEnabled: Boolean); cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setButtonBorderColor(buttonBorderColor: UIColor); cdecl;
    procedure setButtonColor(buttonColor: UIColor); cdecl;
    procedure setButtonTitleColor(buttonTitleColor: UIColor); cdecl;
    procedure setButtonTitleFont(buttonTitleFont: UIFont); cdecl;
    procedure setDescriptionColor(descriptionColor: UIColor); cdecl;
    procedure setDescriptionFont(descriptionFont: UIFont); cdecl;
    procedure setTitleColor(titleColor: UIColor); cdecl;
    procedure setTitleFont(titleFont: UIFont); cdecl;
    function titleColor: UIColor; cdecl;
    function titleFont: UIFont; cdecl;
  end;
  TFBNativeAdViewAttributes = class(TOCGenericImport<FBNativeAdViewAttributesClass, FBNativeAdViewAttributes>) end;

  FBNativeAdViewClass = interface(FBNativeAdBaseViewClass)
    ['{1A6C3280-C1E2-4536-8566-EF39BC2DDDB5}']
    [MethodName('nativeAdViewWithNativeAd:withType:withAttributes:')]
    {class} function nativeAdViewWithNativeAd(nativeAd: FBNativeAd; &type: FBNativeAdViewType;
      attributes: FBNativeAdViewAttributes): Pointer; overload; cdecl;
    [MethodName('nativeAdViewWithNativeAd:withType:')]
    {class} function nativeAdViewWithNativeAd(nativeAd: FBNativeAd; &type: FBNativeAdViewType): Pointer; overload; cdecl;
    [MethodName('nativeAdViewWithNativeAd:withAttributes:')]
    {class} function nativeAdViewWithNativeAd(nativeAd: FBNativeAd; attributes: FBNativeAdViewAttributes): Pointer; overload; cdecl;
    {class} function nativeAdViewWithNativeAd(nativeAd: FBNativeAd): Pointer; overload; cdecl;
  end;

  FBNativeAdView = interface(FBNativeAdBaseView)
    ['{F89E2193-4CE3-4EBC-A1F1-9F250A3DEFE3}']
    function &type: FBNativeAdViewType; cdecl;
  end;
  TFBNativeAdView = class(TOCGenericImport<FBNativeAdViewClass, FBNativeAdView>) end;

  FBNativeAdCollectionViewCellProviderClass = interface(FBNativeAdCollectionViewAdProviderClass)
    ['{132312A4-59D0-44D5-8EF1-3C3C1EA3FDA7}']
  end;

  FBNativeAdCollectionViewCellProvider = interface(FBNativeAdCollectionViewAdProvider)
    ['{D74D9357-2603-4045-B52E-A1DEB097B610}']
    [MethodName('collectionView:cellForItemAtIndexPath:')]
    function collectionViewCellForItemAtIndexPath(collectionView: UICollectionView; indexPath: NSIndexPath): UICollectionViewCell; cdecl;
    [MethodName('collectionView:heightForRowAtIndexPath:')]
    function collectionViewHeightForRowAtIndexPath(collectionView: UICollectionView; indexPath: NSIndexPath): CGFloat; cdecl;
    [MethodName('initWithManager:forType:forAttributes:')]
    function initWithManager(manager: FBNativeAdsManager; &type: FBNativeAdViewType; attributes: FBNativeAdViewAttributes): Pointer; overload; cdecl;
    [MethodName('initWithManager:forType:')]
    function initWithManager(manager: FBNativeAdsManager; &type: FBNativeAdViewType): Pointer; overload; cdecl;
  end;
  TFBNativeAdCollectionViewCellProvider = class(TOCGenericImport<FBNativeAdCollectionViewCellProviderClass, FBNativeAdCollectionViewCellProvider>) end;

  FBNativeAdScrollViewClass = interface(UIViewClass)
    ['{7A15A1DF-1DFB-4D7B-ACC3-D77D1F25D920}']
  end;

  FBNativeAdScrollView = interface(UIView)
    ['{6C859BB5-54DD-44DD-85D9-129AE4664955}']
    function delegate: Pointer; cdecl;
    [MethodName('initWithNativeAdsManager:withViewProvider:')]
    function initWithNativeAdsManager(manager: FBNativeAdsManager; childViewProvider: TFBNativeAdScrollViewBlockMethod1): Pointer; overload; cdecl;
    [MethodName('initWithNativeAdsManager:withViewProvider:withMaximum:')]
    function initWithNativeAdsManager(manager: FBNativeAdsManager; childViewProvider: TFBNativeAdScrollViewBlockMethod1;
      maximumNativeAdCount: NSUInteger): Pointer; overload; cdecl;
    [MethodName('initWithNativeAdsManager:withType:')]
    function initWithNativeAdsManager(manager: FBNativeAdsManager; &type: FBNativeAdViewType): Pointer; overload; cdecl;
    [MethodName('initWithNativeAdsManager:withType:withAttributes:')]
    function initWithNativeAdsManager(manager: FBNativeAdsManager; &type: FBNativeAdViewType;
      attributes: FBNativeAdViewAttributes): Pointer; overload; cdecl;
    [MethodName('initWithNativeAdsManager:withType:withAttributes:withMaximum:')]
    function initWithNativeAdsManager(manager: FBNativeAdsManager; &type: FBNativeAdViewType;
      attributes: FBNativeAdViewAttributes; maximumNativeAdCount: NSUInteger): Pointer; overload; cdecl;
    function isAdPersistenceEnabled: Boolean; cdecl;
    function isAnimationEnabled: Boolean; cdecl;
    function maximumNativeAdCount: NSUInteger; cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setAdPersistenceEnabled(adPersistenceEnabled: Boolean); cdecl;
    procedure setAnimationEnabled(animationEnabled: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
    procedure setXInset(xInset: CGFloat); cdecl;
    function xInset: CGFloat; cdecl;
  end;
  TFBNativeAdScrollView = class(TOCGenericImport<FBNativeAdScrollViewClass, FBNativeAdScrollView>) end;

  FBNativeAdTableViewAdProviderClass = interface(NSObjectClass)
    ['{2004885A-EFC7-4E9F-9BFD-5D366B9C6B3D}']
  end;

  FBNativeAdTableViewAdProvider = interface(NSObject)
    ['{C770D5AD-7CE5-456B-AB6E-919F65BC82B4}']
    [MethodName('adjustCount:forStride:')]
    function adjustCount(count: NSUInteger; stride: NSUInteger): NSUInteger; cdecl;
    [MethodName('adjustNonAdCellIndexPath:forStride:')]
    function adjustNonAdCellIndexPath(indexPath: NSIndexPath; stride: NSUInteger): NSIndexPath; cdecl;
    function delegate: Pointer; cdecl;
    function extraHint: FBAdExtraHint; cdecl;
    function initWithManager(manager: FBNativeAdsManager): Pointer; cdecl;
    [MethodName('isAdCellAtIndexPath:forStride:')]
    function isAdCellAtIndexPath(indexPath: NSIndexPath; stride: NSUInteger): Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setExtraHint(extraHint: FBAdExtraHint); cdecl;
    [MethodName('tableView:nativeAdForRowAtIndexPath:')]
    function tableView(tableView: UITableView; indexPath: NSIndexPath): FBNativeAd; cdecl;
  end;
  TFBNativeAdTableViewAdProvider = class(TOCGenericImport<FBNativeAdTableViewAdProviderClass, FBNativeAdTableViewAdProvider>) end;

  FBNativeAdTableViewCellProviderClass = interface(FBNativeAdTableViewAdProviderClass)
    ['{96175F90-19E8-415F-8062-2BF391006F4E}']
  end;

  FBNativeAdTableViewCellProvider = interface(FBNativeAdTableViewAdProvider)
    ['{1D5DAC94-F1FA-4ABA-B9C4-14181EA32F0D}']
    [MethodName('initWithManager:forType:')]
    function initWithManager(manager: FBNativeAdsManager; &type: FBNativeAdViewType): Pointer; overload; cdecl;
    [MethodName('initWithManager:forType:forAttributes:')]
    function initWithManager(manager: FBNativeAdsManager; &type: FBNativeAdViewType; attributes: FBNativeAdViewAttributes): Pointer; overload; cdecl;
    [MethodName('tableView:cellForRowAtIndexPath:')]
    function tableViewCellForRowAtIndexPath(tableView: UITableView; indexPath: NSIndexPath): UITableViewCell; cdecl;
    [MethodName('tableView:estimatedHeightForRowAtIndexPath:')]
    function tableViewEstimatedHeightForRowAtIndexPath(tableView: UITableView; indexPath: NSIndexPath): CGFloat; cdecl;
    [MethodName('tableView:heightForRowAtIndexPath:')]
    function tableViewHeightForRowAtIndexPath(tableView: UITableView; indexPath: NSIndexPath): CGFloat; cdecl;
  end;
  TFBNativeAdTableViewCellProvider = class(TOCGenericImport<FBNativeAdTableViewCellProviderClass, FBNativeAdTableViewCellProvider>) end;

  FBNativeBannerAdClass = interface(FBNativeAdBaseClass)
    ['{FDA11DC3-B82E-416F-A936-D749128F5EAB}']
  end;

  FBNativeBannerAd = interface(FBNativeAdBase)
    ['{11F5A1D7-5365-4750-B768-01D346B95DAD}']
    function delegate: Pointer; cdecl;
    procedure downloadMedia; cdecl;
    function initWithPlacementID(placementID: NSString): Pointer; cdecl;
    [MethodName('registerViewForInteraction:iconView:viewController:')]
    procedure registerViewForInteraction(view: UIView; iconView: FBMediaView; viewController: UIViewController); overload; cdecl;
    [MethodName('registerViewForInteraction:iconView:viewController:clickableViews:')]
    procedure registerViewForInteraction(view: UIView; iconView: FBMediaView; viewController: UIViewController;
      clickableViews: NSArray); overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TFBNativeBannerAd = class(TOCGenericImport<FBNativeBannerAdClass, FBNativeBannerAd>) end;

  FBNativeBannerAdDelegate = interface(IObjectiveC)
    ['{D83EF64F-6C80-41BF-A901-5F094E1AF7B5}']
    [MethodName('nativeBannerAd:didFailWithError:')]
    procedure nativeBannerAd(nativeBannerAd: FBNativeBannerAd; error: NSError); cdecl;
    procedure nativeBannerAdDidClick(nativeBannerAd: FBNativeBannerAd); cdecl;
    procedure nativeBannerAdDidDownloadMedia(nativeBannerAd: FBNativeBannerAd); cdecl;
    procedure nativeBannerAdDidFinishHandlingClick(nativeBannerAd: FBNativeBannerAd); cdecl;
    procedure nativeBannerAdDidLoad(nativeBannerAd: FBNativeBannerAd); cdecl;
    procedure nativeBannerAdWillLogImpression(nativeBannerAd: FBNativeBannerAd); cdecl;
  end;

  FBNativeBannerAdViewClass = interface(FBNativeAdBaseViewClass)
    ['{CCA23F35-A448-4F8F-84CF-5302ECE13F7D}']
    [MethodName('nativeBannerAdViewWithNativeBannerAd:withType:')]
    {class} function nativeBannerAdViewWithNativeBannerAd(nativeBannerAd: FBNativeBannerAd; &type: FBNativeBannerAdViewType): Pointer; overload; cdecl;
    [MethodName('nativeBannerAdViewWithNativeBannerAd:withType:withAttributes:')]
    {class} function nativeBannerAdViewWithNativeBannerAd(nativeBannerAd: FBNativeBannerAd; &type: FBNativeBannerAdViewType;
      attributes: FBNativeAdViewAttributes): Pointer; overload; cdecl;
  end;

  FBNativeBannerAdView = interface(FBNativeAdBaseView)
    ['{80071CBF-AE4C-4FEA-A81E-A9588B8CCEE8}']
    function &type: FBNativeBannerAdViewType; cdecl;
  end;
  TFBNativeBannerAdView = class(TOCGenericImport<FBNativeBannerAdViewClass, FBNativeBannerAdView>) end;

  FBRewardedVideoAdClass = interface(NSObjectClass)
    ['{1D084029-F64F-4B56-B008-89C27EFE2BD9}']
  end;

  FBRewardedVideoAd = interface(NSObject)
    ['{263A745B-7A82-4E45-A93F-CC235ECF92DB}']
    function delegate: Pointer; cdecl;
    function duration: CMTime; cdecl;
    function extraHint: FBAdExtraHint; cdecl;
    function initWithPlacementID(placementID: NSString): Pointer; overload; cdecl;
    [MethodName('initWithPlacementID:withUserID:withCurrency:')]
    function initWithPlacementID(placementID: NSString; userID: NSString; currency: NSString): Pointer; overload; cdecl;
    function isAdValid: Boolean; cdecl;
    procedure loadAd; cdecl;
    procedure loadAdWithBidPayload(bidPayload: NSString); cdecl;
    function placementID: NSString; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setExtraHint(extraHint: FBAdExtraHint); cdecl;
    [MethodName('setRewardDataWithUserID:withCurrency:')]
    function setRewardDataWithUserID(userID: NSString; currency: NSString): Boolean; cdecl;
    [MethodName('showAdFromRootViewController:animated:')]
    function showAdFromRootViewController(rootViewController: UIViewController; flag: Boolean): Boolean; overload; cdecl;
    function showAdFromRootViewController(rootViewController: UIViewController): Boolean; overload; cdecl;
  end;
  TFBRewardedVideoAd = class(TOCGenericImport<FBRewardedVideoAdClass, FBRewardedVideoAd>) end;

  FBRewardedVideoAdDelegate = interface(IObjectiveC)
    ['{BBE9C824-F6E9-4DCA-ADB7-1D05C2FAD215}']
    [MethodName('rewardedVideoAd:didFailWithError:')]
    procedure rewardedVideoAd(rewardedVideoAd: FBRewardedVideoAd; error: NSError); cdecl;
    procedure rewardedVideoAdDidClick(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdDidClose(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdDidLoad(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdServerRewardDidFail(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdServerRewardDidSucceed(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdVideoComplete(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdWillClose(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdWillLogImpression(rewardedVideoAd: FBRewardedVideoAd); cdecl;
  end;

const
  libFBAudienceNetwork = 'FBAudienceNetwork';

implementation

procedure FBAudienceNetworkLoader; cdecl; external libFBAudienceNetwork;

end.