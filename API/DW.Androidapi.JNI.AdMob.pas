unit DW.Androidapi.JNI.AdMob;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.Net,
  Androidapi.JNI.Widget, Androidapi.JNI.App, Androidapi.JNI.AdMob, Androidapi.JNI.Util, Androidapi.JNI.Location,
  Androidapi.JNI.PlayServices, Androidapi.JNI.Webkit;

type
  JAbstractAdRequestBuilder = interface;
  JAbstractAdViewAdapter = interface;
  JAdActivity = interface;
  JAdError = interface;
  JAdFormat = interface;
  JAdInspectorError = interface;
  JAdInspectorError_AdInspectorErrorCode = interface;
  JAdListener = interface;
  JAdLoadCallback = interface;
  JAdLoader = interface;
  JAdLoader_Builder = interface;
  JAdManagerAdRequest = interface;
  JAdManagerAdRequest_Builder = interface;
  JAdManagerAdView = interface;
  JAdManagerAdViewOptions = interface;
  JAdManagerAdViewOptions_Builder = interface;
  JAdManagerInterstitialAd = interface;
  JAdManagerInterstitialAdLoadCallback = interface;
  JAdMobAdapter = interface;
  JAdRequest = interface;
  JAdRequest_Builder = interface;
  JAdService = interface;
  JAdSize = interface;
  JAdValue = interface;
  JAdValue_PrecisionType = interface;
  JAdView = interface;
  JAdapter = interface;
  JAdapterResponseInfo = interface;
  JAdapterStatus = interface;
  JAdapterStatus_State = interface;
  JAgeRestrictedTreatment = interface;
  JAppEventListener = interface;
  JAppOpenAd = interface;
  JAppOpenAdPreloader = interface;
  JAppOpenAd_AppOpenAdLoadCallback = interface;
  JBaseAdView = interface;
  JContextualSignals = interface;
  JCustomEvent = interface;
  JCustomEventAdapter = interface;
  JCustomEventBanner = interface;
  JCustomEventBannerListener = interface;
  JCustomEventExtras = interface;
  JCustomEventInterstitial = interface;
  JCustomEventInterstitialListener = interface;
  JCustomEventListener = interface;
  JCustomEventNative = interface;
  JCustomEventNativeListener = interface;
  JCustomTabsCallback = interface;
  JCustomTabsClient = interface;
  JCustomTabsServiceConnection = interface;
  JCustomTabsSession = interface;
  JCustomTabsSession_PendingSession = interface;
  JData = interface;
  JEngagementSignalsCallback = interface;
  JForegroundInfo = interface;
  JForegroundUpdater = interface;
  JFullScreenContentCallback = interface;
  JIObjectWrapper = interface;
  JInitializationCompleteCallback = interface;
  JInitializationStatus = interface;
  JInterstitialAd = interface;
  JInterstitialAdLoadCallback = interface;
  JInterstitialAdPreloader = interface;
  JListenableFuture = interface;
  JListenableWorker = interface;
  JListenableWorker_Result = interface;
  JLoadAdError = interface;
  JMediaAspectRatio = interface;
  JMediaContent = interface;
  JMediaView = interface;
  JMediationAdCallback = interface;
  JMediationAdConfiguration = interface;
  JMediationAdConfiguration_TagForChildDirectedTreatment = interface;
  JMediationAdLoadCallback = interface;
  JMediationAdRequest = interface;
  JMediationAdapter = interface;
  JMediationAppOpenAd = interface;
  JMediationAppOpenAdCallback = interface;
  JMediationAppOpenAdConfiguration = interface;
  JMediationBannerAd = interface;
  JMediationBannerAdCallback = interface;
  JMediationBannerAdConfiguration = interface;
  JMediationBannerAdapter = interface;
  JMediationBannerListener = interface;
  JMediationConfiguration = interface;
  JMediationExtrasReceiver = interface;
  JMediationInterscrollerAd = interface;
  JMediationInterstitialAd = interface;
  JMediationInterstitialAdCallback = interface;
  JMediationInterstitialAdConfiguration = interface;
  JMediationInterstitialAdapter = interface;
  JMediationInterstitialListener = interface;
  JMediationNativeAdCallback = interface;
  JMediationNativeAdConfiguration = interface;
  JMediationNativeAdapter = interface;
  JMediationNativeListener = interface;
  JMediationRewardedAd = interface;
  JMediationRewardedAdCallback = interface;
  JMediationRewardedAdConfiguration = interface;
  JMediationUtils = interface;
  JMobileAds = interface;
  JMobileAdsInitProvider = interface;
  JMuteThisAdListener = interface;
  JMuteThisAdReason = interface;
  JNativeAd = interface;
  JNativeAdMapper = interface;
  JNativeAdOptions = interface;
  JNativeAdOptions_AdChoicesPlacement = interface;
  JNativeAdOptions_Builder = interface;
  JNativeAdOptions_NativeMediaAspectRatio = interface;
  JNativeAd_AdChoicesInfo = interface;
  JNativeAd_Image = interface;
  JNativeMediationAdRequest = interface;
  JNetworkExtras = interface;
  JNotificationHandlerActivity = interface;
  JOfflineNotificationPoster = interface;
  JOfflinePingSender = interface;
  JOnAdInspectorClosedListener = interface;
  JOnAdManagerAdViewLoadedListener = interface;
  JOnAdMetadataChangedListener = interface;
  JOnContextChangedListener = interface;
  JOnImmersiveModeUpdatedListener = interface;
  JOnInitializationCompleteListener = interface;
  JOnPaidEventListener = interface;
  JOnUserEarnedRewardListener = interface;
  JOutOfContextTestingActivity = interface;
  JPreloadCallback = interface;
  JPreloadCallbackV2 = interface;
  JPreloadConfiguration = interface;
  JPreloadConfiguration_Builder = interface;
  JProgressUpdater = interface;
  JPublisherAdViewOptions = interface;
  JPublisherAdViewOptions_Builder = interface;
  JQueryInfo = interface;
  JQueryInfoGenerationCallback = interface;
  JRequestConfiguration = interface;
  JRequestConfiguration_Builder = interface;
  JRequestConfiguration_MaxAdContentRating = interface;
  JRequestConfiguration_PublisherPrivacyPersonalizationState = interface;
  JRequestConfiguration_TagForChildDirectedTreatment = interface;
  JRequestConfiguration_TagForUnderAgeOfConsent = interface;
  JResponseInfo = interface;
  JRewardItem = interface;
  JRewardedAd = interface;
  JRewardedAdLoadCallback = interface;
  JRewardedAdPreloader = interface;
  JRewardedInterstitialAd = interface;
  JRewardedInterstitialAdLoadCallback = interface;
  JRtbAdapter = interface;
  JRtbSignalData = interface;
  JSerialExecutor = interface;
  JServerSideVerificationOptions = interface;
  JServerSideVerificationOptions_Builder = interface;
  JShouldDelayBannerRenderingListener = interface;
  JSignalCallbacks = interface;
  JTaskExecutor = interface;
  JUnifiedNativeAd = interface;
  JUnifiedNativeAdAssetNames = interface;
  JUnifiedNativeAdMapper = interface;
  JVersionInfo = interface;
  JVersionInfoParcel = interface;
  JVideoController = interface;
  JVideoController_VideoLifecycleCallbacks = interface;
  JVideoOptions = interface;
  JVideoOptions_Builder = interface;
  JWorker = interface;
  JWorkerFactory = interface;
  JWorkerParameters = interface;
  JWorkerParameters_RuntimeExtras = interface;

  JNativeAdMapperClass = interface(JObjectClass)
    ['{B8F7A1A8-5778-434B-BF9E-40A53999B49F}']
    {class} function init: JNativeAdMapper; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/NativeAdMapper')]
  JNativeAdMapper = interface(JObject)
    ['{0430716B-3E06-49EC-854E-DB6F56AC045D}']
    procedure destroy; cdecl;
    function getAdChoicesContent: JView; cdecl;
    function getAdvertiser: JString; cdecl;
    function getBody: JString; cdecl;
    function getCallToAction: JString; cdecl;
    function getCurrentTime: Single; cdecl;
    function getDuration: Single; cdecl;
    function getExtras: JBundle; cdecl;
    function getHeadline: JString; cdecl;
    function getIcon: Jnativead_NativeAd_Image; cdecl;
    function getImages: JList; cdecl;
    function getMediaContentAspectRatio: Single; cdecl;
    function getOverrideClickHandling: Boolean; cdecl;
    function getOverrideImpressionRecording: Boolean; cdecl;
    function getPrice: JString; cdecl;
    function getStarRating: JDouble; cdecl;
    function getStore: JString; cdecl;
    procedure handleClick(view: JView); cdecl;
    function hasVideoContent: Boolean; cdecl;
    procedure recordImpression; cdecl;
    procedure setAdChoicesContent(view: JView); cdecl;
    procedure setAdvertiser(string_1: JString); cdecl;
    procedure setBody(string_1: JString); cdecl;
    procedure setCallToAction(string_1: JString); cdecl;
    procedure setExtras(bundle: JBundle); cdecl;
    procedure setHasVideoContent(boolean: Boolean); cdecl;
    procedure setHeadline(string_1: JString); cdecl;
    procedure setIcon(image: Jnativead_NativeAd_Image); cdecl;
    procedure setImages(list: JList); cdecl;
    procedure setMediaContentAspectRatio(float: Single); cdecl;
    procedure setMediaView(view: JView); cdecl;
    procedure setOverrideClickHandling(boolean: Boolean); cdecl;
    procedure setOverrideImpressionRecording(boolean: Boolean); cdecl;
    procedure setPrice(string_1: JString); cdecl;
    procedure setStarRating(double: JDouble); cdecl;
    procedure setStore(string_1: JString); cdecl;
    procedure trackViews(view: JView; map: JMap; map_1: JMap); cdecl;
    procedure untrackView(view: JView); cdecl;
  end;
  TJNativeAdMapper = class(TJavaGenericImport<JNativeAdMapperClass, JNativeAdMapper>) end;

  JCustomTabsServiceConnectionClass = interface(JObjectClass)
    ['{F3F8F552-A1AF-4EA3-ADCC-CF2495A63161}']
    {class} function init: JCustomTabsServiceConnection; cdecl;
  end;

  [JavaSignature('androidx/browser/customtabs/CustomTabsServiceConnection')]
  JCustomTabsServiceConnection = interface(JObject)
    ['{66AF5F67-F213-4626-9871-8FD233DE5406}']
    procedure onCustomTabsServiceConnected(componentName: JComponentName; customTabsClient: JCustomTabsClient); cdecl;
    procedure onServiceConnected(componentName: JComponentName; iBinder: JIBinder); cdecl;
  end;
  TJCustomTabsServiceConnection = class(TJavaGenericImport<JCustomTabsServiceConnectionClass, JCustomTabsServiceConnection>) end;

  JCustomTabsSession_PendingSessionClass = interface(JObjectClass)
    ['{13BA8CE1-B95F-45FE-AF54-72D33060C15B}']
  end;

  [JavaSignature('androidx/browser/customtabs/CustomTabsSession$PendingSession')]
  JCustomTabsSession_PendingSession = interface(JObject)
    ['{DD6E4224-A344-40E6-857A-27472F0D0971}']
  end;
  TJCustomTabsSession_PendingSession = class(TJavaGenericImport<JCustomTabsSession_PendingSessionClass, JCustomTabsSession_PendingSession>) end;

  JEngagementSignalsCallbackClass = interface(IJavaClass)
    ['{7D6CA26F-8D40-45B3-9FBE-8B1F640DE048}']
  end;

  [JavaSignature('androidx/browser/customtabs/EngagementSignalsCallback')]
  JEngagementSignalsCallback = interface(IJavaInstance)
    ['{EFFA9280-FE20-4890-B5D5-92060B7B6707}']
    procedure onGreatestScrollPercentageIncreased(int: Integer; bundle: JBundle); cdecl;
    procedure onSessionEnded(boolean: Boolean; bundle_1: JBundle); cdecl;
    procedure onVerticalScrollEvent(boolean: Boolean; bundle_1: JBundle); cdecl;
  end;
  TJEngagementSignalsCallback = class(TJavaGenericImport<JEngagementSignalsCallbackClass, JEngagementSignalsCallback>) end;

  JCustomTabsSessionClass = interface(JObjectClass)
    ['{12316444-64F5-4704-BEAA-136F18E45FAF}']
    {class} function createMockSessionForTesting(componentName: JComponentName): JCustomTabsSession; cdecl;
  end;

  [JavaSignature('androidx/browser/customtabs/CustomTabsSession')]
  JCustomTabsSession = interface(JObject)
    ['{15C3D1A4-9603-4456-8E81-818E046E64F2}']
    function isEngagementSignalsApiAvailable(bundle: JBundle): Boolean; cdecl;
    function mayLaunchUrl(uri: Jnet_Uri; bundle: JBundle; list: JList): Boolean; cdecl;
    function postMessage(string_1: JString; bundle: JBundle): Integer; cdecl;
    function receiveFile(uri: Jnet_Uri; int: Integer; bundle: JBundle): Boolean; cdecl;
    function requestPostMessageChannel(uri: Jnet_Uri; uri_1: Jnet_Uri; bundle: JBundle): Boolean; overload; cdecl;
    function requestPostMessageChannel(uri: Jnet_Uri): Boolean; overload; cdecl;
    function setActionButton(bitmap: JBitmap; string_1: JString): Boolean; cdecl;
    function setEngagementSignalsCallback(executor: JExecutor; engagementSignalsCallback: JEngagementSignalsCallback; bundle: JBundle): Boolean; overload; cdecl;
    function setEngagementSignalsCallback(engagementSignalsCallback: JEngagementSignalsCallback; bundle: JBundle): Boolean; overload; cdecl;
    function setSecondaryToolbarSwipeUpGesture(pendingIntent: JPendingIntent): Boolean; cdecl;
    function setSecondaryToolbarViews(remoteViews: JRemoteViews; ints: TJavaArray<Integer>; pendingIntent: JPendingIntent): Boolean; cdecl;
    function setToolbarItem(int: Integer; bitmap: JBitmap; string_1: JString): Boolean; cdecl;
    function validateRelationship(int: Integer; uri: Jnet_Uri; bundle: JBundle): Boolean; cdecl;
  end;
  TJCustomTabsSession = class(TJavaGenericImport<JCustomTabsSessionClass, JCustomTabsSession>) end;

  JCustomTabsClientClass = interface(JObjectClass)
    ['{BF4A9D09-9CFF-4F3E-9F23-DBD00B1BB450}']
    {class} function bindCustomTabsService(context: JContext; string_1: JString; customTabsServiceConnection: JCustomTabsServiceConnection): Boolean; cdecl;
    {class} function bindCustomTabsServicePreservePriority(context: JContext; string_1: JString; customTabsServiceConnection: JCustomTabsServiceConnection): Boolean; cdecl;
    {class} function connectAndInitialize(context: JContext; string_1: JString): Boolean; cdecl;
    {class} function getPackageName(context: JContext; list: JList): JString; overload; cdecl;
    {class} function getPackageName(context: JContext; list: JList; boolean: Boolean): JString; overload; cdecl;
    {class} function newPendingSession(context: JContext; customTabsCallback: JCustomTabsCallback; int: Integer): JCustomTabsSession_PendingSession; cdecl;
  end;

  [JavaSignature('androidx/browser/customtabs/CustomTabsClient')]
  JCustomTabsClient = interface(JObject)
    ['{696E780C-84E1-49B6-B2D4-526C8AEC7209}']
    function attachSession(pendingSession: JCustomTabsSession_PendingSession): JCustomTabsSession; cdecl;
    function extraCommand(string_1: JString; bundle: JBundle): JBundle; cdecl;
    function newSession(customTabsCallback: JCustomTabsCallback; int: Integer): JCustomTabsSession; overload; cdecl;
    function newSession(customTabsCallback: JCustomTabsCallback): JCustomTabsSession; overload; cdecl;
    function warmup(long: Int64): Boolean; cdecl;
  end;
  TJCustomTabsClient = class(TJavaGenericImport<JCustomTabsClientClass, JCustomTabsClient>) end;

  JCustomTabsCallbackClass = interface(JObjectClass)
    ['{98BD1606-C75B-4AFB-8438-2ECC164DDDA2}']
    {class} function _GetACTIVITY_LAYOUT_STATE_BOTTOM_SHEET: Integer; cdecl;
    {class} function _GetACTIVITY_LAYOUT_STATE_BOTTOM_SHEET_MAXIMIZED: Integer; cdecl;
    {class} function _GetACTIVITY_LAYOUT_STATE_FULL_SCREEN: Integer; cdecl;
    {class} function _GetACTIVITY_LAYOUT_STATE_SIDE_SHEET: Integer; cdecl;
    {class} function _GetACTIVITY_LAYOUT_STATE_SIDE_SHEET_MAXIMIZED: Integer; cdecl;
    {class} function _GetACTIVITY_LAYOUT_STATE_UNKNOWN: Integer; cdecl;
    {class} function _GetNAVIGATION_ABORTED: Integer; cdecl;
    {class} function _GetNAVIGATION_FAILED: Integer; cdecl;
    {class} function _GetNAVIGATION_FINISHED: Integer; cdecl;
    {class} function _GetNAVIGATION_STARTED: Integer; cdecl;
    {class} function _GetONLINE_EXTRAS_KEY: JString; cdecl;
    {class} function _GetTAB_HIDDEN: Integer; cdecl;
    {class} function _GetTAB_SHOWN: Integer; cdecl;
    {class} function init: JCustomTabsCallback; cdecl;
    {class} property ACTIVITY_LAYOUT_STATE_BOTTOM_SHEET: Integer read _GetACTIVITY_LAYOUT_STATE_BOTTOM_SHEET;
    {class} property ACTIVITY_LAYOUT_STATE_BOTTOM_SHEET_MAXIMIZED: Integer read _GetACTIVITY_LAYOUT_STATE_BOTTOM_SHEET_MAXIMIZED;
    {class} property ACTIVITY_LAYOUT_STATE_FULL_SCREEN: Integer read _GetACTIVITY_LAYOUT_STATE_FULL_SCREEN;
    {class} property ACTIVITY_LAYOUT_STATE_SIDE_SHEET: Integer read _GetACTIVITY_LAYOUT_STATE_SIDE_SHEET;
    {class} property ACTIVITY_LAYOUT_STATE_SIDE_SHEET_MAXIMIZED: Integer read _GetACTIVITY_LAYOUT_STATE_SIDE_SHEET_MAXIMIZED;
    {class} property ACTIVITY_LAYOUT_STATE_UNKNOWN: Integer read _GetACTIVITY_LAYOUT_STATE_UNKNOWN;
    {class} property NAVIGATION_ABORTED: Integer read _GetNAVIGATION_ABORTED;
    {class} property NAVIGATION_FAILED: Integer read _GetNAVIGATION_FAILED;
    {class} property NAVIGATION_FINISHED: Integer read _GetNAVIGATION_FINISHED;
    {class} property NAVIGATION_STARTED: Integer read _GetNAVIGATION_STARTED;
    {class} property ONLINE_EXTRAS_KEY: JString read _GetONLINE_EXTRAS_KEY;
    {class} property TAB_HIDDEN: Integer read _GetTAB_HIDDEN;
    {class} property TAB_SHOWN: Integer read _GetTAB_SHOWN;
  end;

  [JavaSignature('androidx/browser/customtabs/CustomTabsCallback')]
  JCustomTabsCallback = interface(JObject)
    ['{764D002D-C07E-4BB3-89F6-F010F1289F56}']
    procedure extraCallback(string_1: JString; bundle: JBundle); cdecl;
    function extraCallbackWithResult(string_1: JString; bundle: JBundle): JBundle; cdecl;
    procedure onActivityLayout(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; bundle: JBundle); cdecl;
    procedure onActivityResized(int: Integer; int_1: Integer; bundle: JBundle); cdecl;
    procedure onMessageChannelReady(bundle: JBundle); cdecl;
    procedure onMinimized(bundle: JBundle); cdecl;
    procedure onNavigationEvent(int: Integer; bundle: JBundle); cdecl;
    procedure onPostMessage(string_1: JString; bundle: JBundle); cdecl;
    procedure onRelationshipValidationResult(int: Integer; uri: Jnet_Uri; boolean: Boolean; bundle_1: JBundle); cdecl;
    procedure onUnminimized(bundle: JBundle); cdecl;
    procedure onWarmupCompleted(bundle: JBundle); cdecl;
  end;
  TJCustomTabsCallback = class(TJavaGenericImport<JCustomTabsCallbackClass, JCustomTabsCallback>) end;

  JIObjectWrapperClass = interface(JIInterfaceClass)
    ['{C2BB237F-4E5B-48FF-AA10-8D207A73B8BD}']
  end;

  [JavaSignature('com/google/android/gms/dynamic/IObjectWrapper')]
  JIObjectWrapper = interface(JIInterface)
    ['{1036DE43-9CCD-44C9-99C2-6DB1F67DEC6C}']
  end;
  TJIObjectWrapper = class(TJavaGenericImport<JIObjectWrapperClass, JIObjectWrapper>) end;

  JForegroundInfoClass = interface(JObjectClass)
    ['{7F9E60FB-B27F-4ED5-8392-D95AFA05C36A}']
    {class} function init(int: Integer; notification: JNotification): JForegroundInfo; overload; cdecl;
    {class} function init(int: Integer; notification: JNotification; int_1: Integer): JForegroundInfo; overload; cdecl;
  end;

  [JavaSignature('androidx/work/ForegroundInfo')]
  JForegroundInfo = interface(JObject)
    ['{C537D278-BACA-48EC-B346-7290793A2DA1}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getForegroundServiceType: Integer; cdecl;
    function getNotification: JNotification; cdecl;
    function getNotificationId: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJForegroundInfo = class(TJavaGenericImport<JForegroundInfoClass, JForegroundInfo>) end;

  JSerialExecutorClass = interface(JObjectClass)
    ['{8AF6D21A-A51F-4333-8E35-182905CBC784}']
    {class} function init(executor: JExecutor): JSerialExecutor; cdecl;
  end;

  [JavaSignature('androidx/work/impl/utils/SerialExecutor')]
  JSerialExecutor = interface(JObject)
    ['{BC50DB99-7739-47D1-A3C4-9DDA83CC4594}']
    procedure execute(runnable: JRunnable); cdecl;
    function getDelegatedExecutor: JExecutor; cdecl;
    function hasPendingTasks: Boolean; cdecl;
  end;
  TJSerialExecutor = class(TJavaGenericImport<JSerialExecutorClass, JSerialExecutor>) end;

  JDataClass = interface(JObjectClass)
    ['{467DA7EB-2578-4A1B-B494-5146938260A8}']
    {class} function _GetEMPTY: JData; cdecl;
    {class} function _GetMAX_DATA_BYTES: Integer; cdecl;
    {class} function convertPrimitiveBooleanArray(booleans: TJavaArray<Boolean>): TJavaObjectArray<JBoolean>; cdecl;
    {class} function convertPrimitiveByteArray(bytes: TJavaArray<Byte>): TJavaObjectArray<JByte>; cdecl;
    {class} function convertPrimitiveDoubleArray(doubles: TJavaArray<Double>): TJavaObjectArray<JDouble>; cdecl;
    {class} function convertPrimitiveFloatArray(floats: TJavaArray<Single>): TJavaObjectArray<JFloat>; cdecl;
    {class} function convertPrimitiveIntArray(ints: TJavaArray<Integer>): TJavaObjectArray<JInteger>; cdecl;
    {class} function convertPrimitiveLongArray(longs: TJavaArray<Int64>): TJavaObjectArray<JLong>; cdecl;
    {class} function convertToPrimitiveArray(floats: TJavaObjectArray<JFloat>): TJavaArray<Single>; overload; cdecl;
    {class} function convertToPrimitiveArray(booleans: TJavaObjectArray<JBoolean>): TJavaArray<Boolean>; overload; cdecl;
    {class} function convertToPrimitiveArray(doubles: TJavaObjectArray<JDouble>): TJavaArray<Double>; overload; cdecl;
    {class} function convertToPrimitiveArray(longs: TJavaObjectArray<JLong>): TJavaArray<Int64>; overload; cdecl;
    {class} function convertToPrimitiveArray(bytes: TJavaObjectArray<JByte>): TJavaArray<Byte>; overload; cdecl;
    {class} function convertToPrimitiveArray(integers: TJavaObjectArray<JInteger>): TJavaArray<Integer>; overload; cdecl;
    {class} function fromByteArray(bytes: TJavaArray<Byte>): JData; cdecl;
    {class} function init(data: JData): JData; overload; cdecl;
    {class} function init(map: JMap): JData; overload; cdecl;
    {class} function toByteArrayInternal(data: JData): TJavaArray<Byte>; cdecl;
    {class} property EMPTY: JData read _GetEMPTY;
    {class} property MAX_DATA_BYTES: Integer read _GetMAX_DATA_BYTES;
  end;

  [JavaSignature('androidx/work/Data')]
  JData = interface(JObject)
    ['{6C15FEF2-44D7-40A4-AB25-A5428729C5EE}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getBoolean(string_1: JString; boolean: Boolean): Boolean; cdecl;
    function getBooleanArray(string_1: JString): TJavaArray<Boolean>; cdecl;
    function getByte(string_1: JString; byte: Byte): Byte; cdecl;
    function getByteArray(string_1: JString): TJavaArray<Byte>; cdecl;
    function getDouble(string_1: JString; double: Double): Double; cdecl;
    function getDoubleArray(string_1: JString): TJavaArray<Double>; cdecl;
    function getFloat(string_1: JString; float: Single): Single; cdecl;
    function getFloatArray(string_1: JString): TJavaArray<Single>; cdecl;
    function getInt(string_1: JString; int: Integer): Integer; cdecl;
    function getIntArray(string_1: JString): TJavaArray<Integer>; cdecl;
    function getKeyValueMap: JMap; cdecl;
    function getLong(string_1: JString; long: Int64): Int64; cdecl;
    function getLongArray(string_1: JString): TJavaArray<Int64>; cdecl;
    function getString(string_1: JString): JString; cdecl;
    function getStringArray(string_1: JString): TJavaObjectArray<JString>; cdecl;
    function hasKeyWithValueOfType(string_1: JString; class_1: Jlang_Class): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function size: Integer; cdecl;
    function toByteArray: TJavaArray<Byte>; cdecl;
    function toString: JString; cdecl;
  end;
  TJData = class(TJavaGenericImport<JDataClass, JData>) end;

  JWorkerParameters_RuntimeExtrasClass = interface(JObjectClass)
    ['{58F31196-88A5-4A82-941B-AF11C3989B1D}']
    {class} function init: JWorkerParameters_RuntimeExtras; cdecl;
  end;

  [JavaSignature('androidx/work/WorkerParameters$RuntimeExtras')]
  JWorkerParameters_RuntimeExtras = interface(JObject)
    ['{FC7F89A5-5C2B-417E-A756-8DCCC8B323F1}']
    function _Getnetwork: JNetwork; cdecl;
    function _GettriggeredContentAuthorities: JList; cdecl;
    function _GettriggeredContentUris: JList; cdecl;
    property network: JNetwork read _Getnetwork;
    property triggeredContentAuthorities: JList read _GettriggeredContentAuthorities;
    property triggeredContentUris: JList read _GettriggeredContentUris;
  end;
  TJWorkerParameters_RuntimeExtras = class(TJavaGenericImport<JWorkerParameters_RuntimeExtrasClass, JWorkerParameters_RuntimeExtras>) end;

  JTaskExecutorClass = interface(IJavaClass)
    ['{A58A7D85-3DC2-49B1-B317-C629DFD2AEC1}']
  end;

  [JavaSignature('androidx/work/impl/utils/taskexecutor/TaskExecutor')]
  JTaskExecutor = interface(IJavaInstance)
    ['{F90F4E82-693C-4359-805F-CA0C725F3500}']
    procedure executeOnBackgroundThread(runnable: JRunnable); cdecl;
    function getBackgroundExecutor: JSerialExecutor; cdecl;
    function getMainThreadExecutor: JExecutor; cdecl;
    procedure postToMainThread(runnable: JRunnable); cdecl;
  end;
  TJTaskExecutor = class(TJavaGenericImport<JTaskExecutorClass, JTaskExecutor>) end;

  JWorkerFactoryClass = interface(JObjectClass)
    ['{F042B1C1-A1F8-4AD5-A26F-E775694AC658}']
    {class} function getDefaultWorkerFactory: JWorkerFactory; cdecl;
    {class} function init: JWorkerFactory; cdecl;
  end;

  [JavaSignature('androidx/work/WorkerFactory')]
  JWorkerFactory = interface(JObject)
    ['{E09E6A28-44BD-47BB-AB6F-7C3E8052A8D9}']
    function createWorker(context: JContext; string_1: JString; workerParameters: JWorkerParameters): JListenableWorker; cdecl;
    function createWorkerWithDefaultFallback(context: JContext; string_1: JString; workerParameters: JWorkerParameters): JListenableWorker; cdecl;
  end;
  TJWorkerFactory = class(TJavaGenericImport<JWorkerFactoryClass, JWorkerFactory>) end;

  JProgressUpdaterClass = interface(IJavaClass)
    ['{BB046B03-AD70-41E1-BAE2-93CD67C9539F}']
  end;

  [JavaSignature('androidx/work/ProgressUpdater')]
  JProgressUpdater = interface(IJavaInstance)
    ['{1E4D181D-C760-409D-BA37-846E9E07EAEE}']
    function updateProgress(context: JContext; UUID: JUUID; data: JData): JListenableFuture; cdecl;
  end;
  TJProgressUpdater = class(TJavaGenericImport<JProgressUpdaterClass, JProgressUpdater>) end;

  JForegroundUpdaterClass = interface(IJavaClass)
    ['{9770FA34-E2BD-4BBE-AA23-60B1884728E5}']
  end;

  [JavaSignature('androidx/work/ForegroundUpdater')]
  JForegroundUpdater = interface(IJavaInstance)
    ['{E4697BB4-6BC9-41F5-AEA8-BCB170170185}']
    function setForegroundAsync(context: JContext; UUID: JUUID; foregroundInfo: JForegroundInfo): JListenableFuture; cdecl;
  end;
  TJForegroundUpdater = class(TJavaGenericImport<JForegroundUpdaterClass, JForegroundUpdater>) end;

  JListenableWorkerClass = interface(JObjectClass)
    ['{AE4C6893-9364-4F60-9411-6D86FAF0A3C7}']
    {class} function init(context: JContext; workerParameters: JWorkerParameters): JListenableWorker; cdecl;
  end;

  [JavaSignature('androidx/work/ListenableWorker')]
  JListenableWorker = interface(JObject)
    ['{E4CEF218-789E-4647-9395-0975FAEF87C6}']
    function getApplicationContext: JContext; cdecl;
    function getBackgroundExecutor: JExecutor; cdecl;
    function getForegroundInfoAsync: JListenableFuture; cdecl;
    function getId: JUUID; cdecl;
    function getInputData: JData; cdecl;
    function getNetwork: JNetwork; cdecl;
    function getRunAttemptCount: Integer; cdecl;
    function getTags: JSet; cdecl;
    function getTaskExecutor: JTaskExecutor; cdecl;
    function getTriggeredContentAuthorities: JList; cdecl;
    function getTriggeredContentUris: JList; cdecl;
    function getWorkerFactory: JWorkerFactory; cdecl;
    function isRunInForeground: Boolean; cdecl;
    function isStopped: Boolean; cdecl;
    function isUsed: Boolean; cdecl;
    procedure onStopped; cdecl;
    function setForegroundAsync(foregroundInfo: JForegroundInfo): JListenableFuture; cdecl;
    function setProgressAsync(data: JData): JListenableFuture; cdecl;
    procedure setRunInForeground(boolean: Boolean); cdecl;
    procedure setUsed; cdecl;
    function startWork: JListenableFuture; cdecl;
    procedure stop; cdecl;
  end;
  TJListenableWorker = class(TJavaGenericImport<JListenableWorkerClass, JListenableWorker>) end;

  JListenableFutureClass = interface(JFutureClass)
    ['{E1BDC49D-230C-409E-9B8A-80E61FDBA0ED}']
  end;

  [JavaSignature('com/google/common/util/concurrent/ListenableFuture')]
  JListenableFuture = interface(JFuture)
    ['{C6F4EAE3-4E6F-439B-A672-86D3D9810284}']
    procedure addListener(runnable: JRunnable; executor: JExecutor); cdecl;
  end;
  TJListenableFuture = class(TJavaGenericImport<JListenableFutureClass, JListenableFuture>) end;

  JWorkerClass = interface(JListenableWorkerClass)
    ['{D0914696-937E-4239-93DC-07D76E955B5C}']
    {class} function init(context: JContext; workerParameters: JWorkerParameters): JWorker; cdecl;
  end;

  [JavaSignature('androidx/work/Worker')]
  JWorker = interface(JListenableWorker)
    ['{89CE10CF-7193-4C9B-896A-1337188C4BE4}']
    function doWork: JListenableWorker_Result; cdecl;
    function startWork: JListenableFuture; cdecl;
  end;
  TJWorker = class(TJavaGenericImport<JWorkerClass, JWorker>) end;

  JWorkerParametersClass = interface(JObjectClass)
    ['{66CF0132-0313-428F-AC0F-647A56CD5795}']
    {class} function init(UUID: JUUID; data: JData; collection: JCollection; runtimeExtras: JWorkerParameters_RuntimeExtras; int: Integer; executor: JExecutor; taskExecutor: JTaskExecutor; workerFactory: JWorkerFactory; progressUpdater: JProgressUpdater; foregroundUpdater: JForegroundUpdater): JWorkerParameters; cdecl;
  end;

  [JavaSignature('androidx/work/WorkerParameters')]
  JWorkerParameters = interface(JObject)
    ['{D2ABBE58-5E8B-4B35-91CF-28615B1723DF}']
    function getBackgroundExecutor: JExecutor; cdecl;
    function getForegroundUpdater: JForegroundUpdater; cdecl;
    function getId: JUUID; cdecl;
    function getInputData: JData; cdecl;
    function getNetwork: JNetwork; cdecl;
    function getProgressUpdater: JProgressUpdater; cdecl;
    function getRunAttemptCount: Integer; cdecl;
    function getRuntimeExtras: JWorkerParameters_RuntimeExtras; cdecl;
    function getTags: JSet; cdecl;
    function getTaskExecutor: JTaskExecutor; cdecl;
    function getTriggeredContentAuthorities: JList; cdecl;
    function getTriggeredContentUris: JList; cdecl;
    function getWorkerFactory: JWorkerFactory; cdecl;
  end;
  TJWorkerParameters = class(TJavaGenericImport<JWorkerParametersClass, JWorkerParameters>) end;

  JListenableWorker_ResultClass = interface(JObjectClass)
    ['{13F51E66-276B-461B-B3E6-10A8FCE70AEE}']
    {class} function failure: JListenableWorker_Result; overload; cdecl;
    {class} function failure(data: JData): JListenableWorker_Result; overload; cdecl;
    {class} function retry: JListenableWorker_Result; cdecl;
    {class} function success: JListenableWorker_Result; overload; cdecl;
    {class} function success(data: JData): JListenableWorker_Result; overload; cdecl;
  end;

  [JavaSignature('androidx/work/ListenableWorker$Result')]
  JListenableWorker_Result = interface(JObject)
    ['{7CAFF6A3-57BC-4A38-B1DF-9198D03567B9}']
    function getOutputData: JData; cdecl;
  end;
  TJListenableWorker_Result = class(TJavaGenericImport<JListenableWorker_ResultClass, JListenableWorker_Result>) end;

  JRewardedInterstitialAdClass = interface(JObjectClass)
    ['{54C75AEE-B564-4B83-9F1B-EC525BC6D4CB}']
    {class} function init: JRewardedInterstitialAd; cdecl;
    {class} procedure load(context: JContext; string_1: JString; adRequest: JAdRequest; rewardedInterstitialAdLoadCallback: JRewardedInterstitialAdLoadCallback); overload; cdecl;
    {class} procedure load(context: JContext; string_1: JString; adManagerAdRequest: JAdManagerAdRequest; rewardedInterstitialAdLoadCallback: JRewardedInterstitialAdLoadCallback); overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewardedinterstitial/RewardedInterstitialAd')]
  JRewardedInterstitialAd = interface(JObject)
    ['{800CACB2-BEE2-48F1-AC28-5BA085F9C69E}']
    function getAdMetadata: JBundle; cdecl;
    function getAdUnitId: JString; cdecl;
    function getFullScreenContentCallback: JFullScreenContentCallback; cdecl;
    function getOnAdMetadataChangedListener: JOnAdMetadataChangedListener; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getPlacementId: Int64; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    function getRewardItem: JRewardItem; cdecl;
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure setImmersiveMode(boolean: Boolean); cdecl;
    procedure setOnAdMetadataChangedListener(onAdMetadataChangedListener: JOnAdMetadataChangedListener); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setPlacementId(long: Int64); cdecl;
    procedure setServerSideVerificationOptions(serverSideVerificationOptions: JServerSideVerificationOptions); cdecl;
    procedure show(activity: JActivity; onUserEarnedRewardListener: JOnUserEarnedRewardListener); cdecl;
  end;
  TJRewardedInterstitialAd = class(TJavaGenericImport<JRewardedInterstitialAdClass, JRewardedInterstitialAd>) end;

  JServerSideVerificationOptionsClass = interface(JObjectClass)
    ['{63499628-9FFB-452D-9978-568B8ABFD1A2}']
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/ServerSideVerificationOptions')]
  JServerSideVerificationOptions = interface(JObject)
    ['{3A1BA98B-65A3-4D9B-B1A4-EEB5F51DF07B}']
    function getCustomData: JString; cdecl;
    function getUserId: JString; cdecl;
  end;
  TJServerSideVerificationOptions = class(TJavaGenericImport<JServerSideVerificationOptionsClass, JServerSideVerificationOptions>) end;

  JServerSideVerificationOptions_BuilderClass = interface(JObjectClass)
    ['{38AD881E-C574-4592-9905-9CEC15F3A3BD}']
    {class} function init: JServerSideVerificationOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/ServerSideVerificationOptions$Builder')]
  JServerSideVerificationOptions_Builder = interface(JObject)
    ['{B1ADA9E2-1AC0-4D0F-9019-CDC88C5BF705}']
    function build: JServerSideVerificationOptions; cdecl;
    function setCustomData(string_1: JString): JServerSideVerificationOptions_Builder; cdecl;
    function setUserId(string_1: JString): JServerSideVerificationOptions_Builder; cdecl;
  end;
  TJServerSideVerificationOptions_Builder = class(TJavaGenericImport<JServerSideVerificationOptions_BuilderClass, JServerSideVerificationOptions_Builder>) end;

  JRewardedAdPreloaderClass = interface(JObjectClass)
    ['{4AEFF902-863D-48C2-91AD-4E616A3495F4}']
    {class} function destroy(string_1: JString): Boolean; cdecl;
    {class} procedure destroyAll; cdecl;
    {class} function getConfiguration(string_1: JString): JPreloadConfiguration; cdecl;
    {class} function getConfigurations: JMap; cdecl;
    {class} function getNumAdsAvailable(string_1: JString): Integer; cdecl;
    {class} function isAdAvailable(string_1: JString): Boolean; cdecl;
    {class} function pollAd(string_1: JString): JRewardedAd; cdecl;
    {class} function start(string_1: JString; preloadConfiguration: JPreloadConfiguration; preloadCallbackV2: JPreloadCallbackV2): Boolean; overload; cdecl;
    {class} function start(string_1: JString; preloadConfiguration: JPreloadConfiguration): Boolean; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/RewardedAdPreloader')]
  JRewardedAdPreloader = interface(JObject)
    ['{92187564-DDC3-4BDC-8405-81AF6DED3DCA}']
  end;
  TJRewardedAdPreloader = class(TJavaGenericImport<JRewardedAdPreloaderClass, JRewardedAdPreloader>) end;

  JRewardedAdClass = interface(JObjectClass)
    ['{E38D464B-BD19-47B5-A77A-637EF399745A}']
    {class} function init: JRewardedAd; cdecl;
    {class} function isAdAvailable(context: JContext; string_1: JString): Boolean; cdecl;
    {class} procedure load(context: JContext; string_1: JString; adManagerAdRequest: JAdManagerAdRequest; rewardedAdLoadCallback: JRewardedAdLoadCallback); overload; cdecl;
    {class} procedure load(context: JContext; string_1: JString; adRequest: JAdRequest; rewardedAdLoadCallback: JRewardedAdLoadCallback); overload; cdecl;
    {class} function pollAd(context: JContext; string_1: JString): JRewardedAd; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/RewardedAd')]
  JRewardedAd = interface(JObject)
    ['{4F367C29-EBF3-41C2-AA50-F7FBCCF7203D}']
    function getAdMetadata: JBundle; cdecl;
    function getAdUnitId: JString; cdecl;
    function getFullScreenContentCallback: JFullScreenContentCallback; cdecl;
    function getOnAdMetadataChangedListener: JOnAdMetadataChangedListener; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getPlacementId: Int64; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    function getRewardItem: JRewardItem; cdecl;
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure setImmersiveMode(boolean: Boolean); cdecl;
    procedure setOnAdMetadataChangedListener(onAdMetadataChangedListener: JOnAdMetadataChangedListener); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setPlacementId(long: Int64); cdecl;
    procedure setServerSideVerificationOptions(serverSideVerificationOptions: JServerSideVerificationOptions); cdecl;
    procedure show(activity: JActivity; onUserEarnedRewardListener: JOnUserEarnedRewardListener); cdecl;
  end;
  TJRewardedAd = class(TJavaGenericImport<JRewardedAdClass, JRewardedAd>) end;

  JRewardItemClass = interface(IJavaClass)
    ['{266B65CF-BB76-4C16-826C-4F99EE3D857D}']
    {class} function _GetDEFAULT_REWARD: JRewardItem; cdecl;
    {class} property DEFAULT_REWARD: JRewardItem read _GetDEFAULT_REWARD;
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/RewardItem')]
  JRewardItem = interface(IJavaInstance)
    ['{8748784D-2F3E-40D9-A045-CA83EFC6DDD9}']
    function getAmount: Integer; cdecl;
    function getType: JString; cdecl;
  end;
  TJRewardItem = class(TJavaGenericImport<JRewardItemClass, JRewardItem>) end;

  JOnAdMetadataChangedListenerClass = interface(IJavaClass)
    ['{52F50C8F-C595-4224-8C55-62A9DB9DAE28}']
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/OnAdMetadataChangedListener')]
  JOnAdMetadataChangedListener = interface(IJavaInstance)
    ['{0DD0007B-FF5F-497E-AA6F-56DF12EF8A3A}']
    procedure onAdMetadataChanged; cdecl;
  end;
  TJOnAdMetadataChangedListener = class(TJavaGenericImport<JOnAdMetadataChangedListenerClass, JOnAdMetadataChangedListener>) end;

  JQueryInfoGenerationCallbackClass = interface(JObjectClass)
    ['{86C2B01B-E6A5-420E-B8FE-749D5C4197C8}']
    {class} function init: JQueryInfoGenerationCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/query/QueryInfoGenerationCallback')]
  JQueryInfoGenerationCallback = interface(JObject)
    ['{5E696E13-7C38-4D1B-B394-8CF651A4A5E6}']
    procedure onFailure(string_1: JString); cdecl;
    procedure onSuccess(queryInfo: JQueryInfo); cdecl;
  end;
  TJQueryInfoGenerationCallback = class(TJavaGenericImport<JQueryInfoGenerationCallbackClass, JQueryInfoGenerationCallback>) end;

  JQueryInfoClass = interface(JObjectClass)
    ['{45F55F09-E3A1-432E-AAFE-50AC0904E557}']
    {class} procedure generate(context: JContext; adFormat: JAdFormat; adRequest: JAdRequest; string_1: JString; queryInfoGenerationCallback: JQueryInfoGenerationCallback); overload; cdecl;
    {class} procedure generate(context: JContext; adFormat: JAdFormat; adRequest: JAdRequest; queryInfoGenerationCallback: JQueryInfoGenerationCallback); overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/query/QueryInfo')]
  JQueryInfo = interface(JObject)
    ['{27D67EFD-D41F-4FE6-9B55-D911D8511F4C}']
    function getQuery: JString; cdecl;
    function getQueryBundle: JBundle; cdecl;
    function getRequestId: JString; cdecl;
  end;
  TJQueryInfo = class(TJavaGenericImport<JQueryInfoClass, JQueryInfo>) end;

  JPreloadConfigurationClass = interface(JObjectClass)
    ['{0BE60ABC-9AC7-4E37-8EB3-692383599BC6}']
  end;

  [JavaSignature('com/google/android/gms/ads/preload/PreloadConfiguration')]
  JPreloadConfiguration = interface(JObject)
    ['{13BDB441-241E-434D-92C1-EC0C3E55E37E}']
    function getAdFormat: JAdFormat; cdecl;
    function getAdRequest: JAdRequest; cdecl;
    function getAdUnitId: JString; cdecl;
    function getBufferSize: Integer; cdecl;
  end;
  TJPreloadConfiguration = class(TJavaGenericImport<JPreloadConfigurationClass, JPreloadConfiguration>) end;

  JPreloadConfiguration_BuilderClass = interface(JObjectClass)
    ['{0E1754E1-8927-490B-9146-7D29023AAD14}']
    {class} function init(string_1: JString): JPreloadConfiguration_Builder; overload; cdecl;
    {class} function init(string_1: JString; adFormat: JAdFormat): JPreloadConfiguration_Builder; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/preload/PreloadConfiguration$Builder')]
  JPreloadConfiguration_Builder = interface(JObject)
    ['{033660BF-1DCD-405F-B54D-3381B9DDAAC0}']
    function build: JPreloadConfiguration; cdecl;
    function setAdRequest(adRequest: JAdRequest): JPreloadConfiguration_Builder; cdecl;
    function setBufferSize(int: Integer): JPreloadConfiguration_Builder; cdecl;
  end;
  TJPreloadConfiguration_Builder = class(TJavaGenericImport<JPreloadConfiguration_BuilderClass, JPreloadConfiguration_Builder>) end;

  JPreloadCallbackV2Class = interface(JObjectClass)
    ['{D74E366E-EF17-4499-B72F-B7CE646E708D}']
    {class} function init: JPreloadCallbackV2; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/preload/PreloadCallbackV2')]
  JPreloadCallbackV2 = interface(JObject)
    ['{21E9FCC9-37F5-4EDD-A54B-A32D8FD17D5F}']
    procedure onAdFailedToPreload(string_1: JString; adError: JAdError); cdecl;
    procedure onAdPreloaded(string_1: JString; responseInfo: JResponseInfo); cdecl;
    procedure onAdsExhausted(string_1: JString); cdecl;
  end;
  TJPreloadCallbackV2 = class(TJavaGenericImport<JPreloadCallbackV2Class, JPreloadCallbackV2>) end;

  JPreloadCallbackClass = interface(IJavaClass)
    ['{3CA8F603-1A44-4238-807D-216A5F3B3720}']
  end;

  [JavaSignature('com/google/android/gms/ads/preload/PreloadCallback')]
  JPreloadCallback = interface(IJavaInstance)
    ['{3AEC7ADE-1DA1-41C8-B653-48E7A92AF45C}']
    procedure onAdsAvailable(preloadConfiguration: JPreloadConfiguration); cdecl;
    procedure onAdsExhausted(preloadConfiguration: JPreloadConfiguration); cdecl;
  end;
  TJPreloadCallback = class(TJavaGenericImport<JPreloadCallbackClass, JPreloadCallback>) end;

  JSignalCallbacksClass = interface(IJavaClass)
    ['{9EF4960D-12FE-4C94-8836-4A3125E0ADB1}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/rtb/SignalCallbacks')]
  JSignalCallbacks = interface(IJavaInstance)
    ['{0897FA60-F93E-4F7D-A3A2-7E03EDF5F021}']
    procedure onFailure(adError: JAdError); cdecl;
    procedure onSuccess(string_1: JString); cdecl;
  end;
  TJSignalCallbacks = class(TJavaGenericImport<JSignalCallbacksClass, JSignalCallbacks>) end;

  JRtbSignalDataClass = interface(JObjectClass)
    ['{201191B1-7118-407B-B8AA-C6F8B03366AE}']
    {class} function init(context: JContext; list: JList; bundle: JBundle; adSize: JAdSize): JRtbSignalData; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/rtb/RtbSignalData')]
  JRtbSignalData = interface(JObject)
    ['{57B47DA9-53ED-4229-B51F-174ABBA4B7EA}']
    function getAdSize: JAdSize; cdecl;
    function getConfigurations: JList; cdecl;
    function getContext: JContext; cdecl;
    function getNetworkExtras: JBundle; cdecl;
  end;
  TJRtbSignalData = class(TJavaGenericImport<JRtbSignalDataClass, JRtbSignalData>) end;

  JCustomEventListenerClass = interface(IJavaClass)
    ['{A82682AF-CF1F-4E2A-A10D-847B078F7CA2}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventListener')]
  JCustomEventListener = interface(IJavaInstance)
    ['{62B3F95F-E262-4B8D-9F08-803ED0B8476D}']
    procedure onAdClicked; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(adError: JAdError); overload; cdecl;
    procedure onAdFailedToLoad(int: Integer); overload; cdecl;
    procedure onAdLeftApplication; cdecl;
    procedure onAdOpened; cdecl;
  end;
  TJCustomEventListener = class(TJavaGenericImport<JCustomEventListenerClass, JCustomEventListener>) end;

  JCustomEventInterstitialListenerClass = interface(JCustomEventListenerClass)
    ['{79492325-DBC1-4011-B4AE-3D524EBA9EF6}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventInterstitialListener')]
  JCustomEventInterstitialListener = interface(JCustomEventListener)
    ['{7570531C-1A41-46D0-A241-7CB647DBBE5F}']
    procedure onAdLoaded; cdecl;
  end;
  TJCustomEventInterstitialListener = class(TJavaGenericImport<JCustomEventInterstitialListenerClass, JCustomEventInterstitialListener>) end;

  JCustomEventExtrasClass = interface(JObjectClass)
    ['{01306E88-19F3-4DFC-B426-F2937036E16A}']
    {class} function init: JCustomEventExtras; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventExtras')]
  JCustomEventExtras = interface(JObject)
    ['{3E708910-03E9-44BC-9748-818278F5E9B1}']
    function getExtra(string_1: JString): JObject; cdecl;
    procedure setExtra(string_1: JString; object_1: JObject); cdecl;
  end;
  TJCustomEventExtras = class(TJavaGenericImport<JCustomEventExtrasClass, JCustomEventExtras>) end;

  JCustomEventBannerListenerClass = interface(JCustomEventListenerClass)
    ['{DEE785E6-393A-4CE7-9B33-3EDC358E6A28}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventBannerListener')]
  JCustomEventBannerListener = interface(JCustomEventListener)
    ['{1E6B8938-CF42-4C7C-AAC0-8F9D8A273BC1}']
    procedure onAdLoaded(view: JView); cdecl;
  end;
  TJCustomEventBannerListener = class(TJavaGenericImport<JCustomEventBannerListenerClass, JCustomEventBannerListener>) end;

  JCustomEventAdapterClass = interface(JObjectClass)
    ['{A6D5F9BD-B7B7-4BC0-8E20-4022A550F25A}']
    {class} function init: JCustomEventAdapter; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventAdapter')]
  JCustomEventAdapter = interface(JObject)
    ['{FCCD89A1-5787-4913-8AAB-BC1A7F310C1A}']
    function getBannerView: JView; cdecl;
    procedure onDestroy; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
    procedure requestBannerAd(context: JContext; mediationBannerListener: Jmediation_MediationBannerListener; bundle: JBundle; adSize: JAdSize; mediationAdRequest: Jmediation_MediationAdRequest; bundle_1: JBundle); cdecl;
    procedure requestInterstitialAd(context: JContext; mediationInterstitialListener: Jmediation_MediationInterstitialListener; bundle: JBundle; mediationAdRequest: Jmediation_MediationAdRequest; bundle_1: JBundle); cdecl;
    procedure requestNativeAd(context: JContext; mediationNativeListener: JMediationNativeListener; bundle: JBundle; nativeMediationAdRequest: JNativeMediationAdRequest; bundle_1: JBundle); cdecl;
    procedure showInterstitial; cdecl;
  end;
  TJCustomEventAdapter = class(TJavaGenericImport<JCustomEventAdapterClass, JCustomEventAdapter>) end;

  JCustomEventClass = interface(IJavaClass)
    ['{CC0F6065-938E-418A-8FDF-9CB17D9E54BE}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEvent')]
  JCustomEvent = interface(IJavaInstance)
    ['{D76B067E-0118-41AE-AB3E-5C4FB491D149}']
    procedure onDestroy; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
  end;
  TJCustomEvent = class(TJavaGenericImport<JCustomEventClass, JCustomEvent>) end;

  JUnifiedNativeAdMapperClass = interface(JObjectClass)
    ['{F3557C91-51D4-44B8-B379-AAD148A74F06}']
    {class} function init: JUnifiedNativeAdMapper; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/UnifiedNativeAdMapper')]
  JUnifiedNativeAdMapper = interface(JObject)
    ['{DBCD2941-2ABE-48FD-89E4-AA453D308A63}']
    procedure destroy; cdecl;
    function getAdChoicesContent: JView; cdecl;
    function getAdvertiser: JString; cdecl;
    function getBody: JString; cdecl;
    function getCallToAction: JString; cdecl;
    function getCurrentTime: Single; cdecl;
    function getDuration: Single; cdecl;
    function getExtras: JBundle; cdecl;
    function getHeadline: JString; cdecl;
    function getIcon: JNativeAd_Image; cdecl;
    function getImages: JList; cdecl;
    function getMediaContentAspectRatio: Single; cdecl;
    function getOverrideClickHandling: Boolean; cdecl;
    function getOverrideImpressionRecording: Boolean; cdecl;
    function getPrice: JString; cdecl;
    function getStarRating: JDouble; cdecl;
    function getStore: JString; cdecl;
    procedure handleClick(view: JView); cdecl;
    function hasVideoContent: Boolean; cdecl;
    procedure recordImpression; cdecl;
    procedure setAdChoicesContent(view: JView); cdecl;
    procedure setAdvertiser(string_1: JString); cdecl;
    procedure setBody(string_1: JString); cdecl;
    procedure setCallToAction(string_1: JString); cdecl;
    procedure setExtras(bundle: JBundle); cdecl;
    procedure setHasVideoContent(boolean: Boolean); cdecl;
    procedure setHeadline(string_1: JString); cdecl;
    procedure setIcon(image: JNativeAd_Image); cdecl;
    procedure setImages(list: JList); cdecl;
    procedure setMediaContentAspectRatio(float: Single); cdecl;
    procedure setMediaView(view: JView); cdecl;
    procedure setOverrideClickHandling(boolean: Boolean); cdecl;
    procedure setOverrideImpressionRecording(boolean: Boolean); cdecl;
    procedure setPrice(string_1: JString); cdecl;
    procedure setStarRating(double: JDouble); cdecl;
    procedure setStore(string_1: JString); cdecl;
    procedure trackViews(view: JView; map: JMap; map_1: JMap); cdecl;
    procedure untrackView(view: JView); cdecl;
  end;
  TJUnifiedNativeAdMapper = class(TJavaGenericImport<JUnifiedNativeAdMapperClass, JUnifiedNativeAdMapper>) end;

  JOnImmersiveModeUpdatedListenerClass = interface(IJavaClass)
    ['{00E893E6-588C-4A6F-9CF6-B918871BB64F}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/OnImmersiveModeUpdatedListener')]
  JOnImmersiveModeUpdatedListener = interface(IJavaInstance)
    ['{C1812260-F87F-4C37-A8FE-6CE8CF1B56E6}']
    procedure onImmersiveModeUpdated(boolean: Boolean); cdecl;
  end;
  TJOnImmersiveModeUpdatedListener = class(TJavaGenericImport<JOnImmersiveModeUpdatedListenerClass, JOnImmersiveModeUpdatedListener>) end;

  JOnContextChangedListenerClass = interface(IJavaClass)
    ['{1A6E0205-0998-4D72-AD32-57BE24FB45D4}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/OnContextChangedListener')]
  JOnContextChangedListener = interface(IJavaInstance)
    ['{3CA61196-9F31-4261-A4A9-62E4E8CE6766}']
    procedure onContextChanged(context: JContext); cdecl;
  end;
  TJOnContextChangedListener = class(TJavaGenericImport<JOnContextChangedListenerClass, JOnContextChangedListener>) end;

  JNetworkExtrasClass = interface(IJavaClass)
    ['{96E61FD9-9D77-4446-AE3C-4ED919785CEB}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/NetworkExtras')]
  JNetworkExtras = interface(IJavaInstance)
    ['{6A9D11C6-6904-4801-9E33-18AD53ECAFED}']
  end;
  TJNetworkExtras = class(TJavaGenericImport<JNetworkExtrasClass, JNetworkExtras>) end;

  JMediationRewardedAdClass = interface(IJavaClass)
    ['{3A9AA417-1631-493D-8DCE-166E7908BCBB}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationRewardedAd')]
  JMediationRewardedAd = interface(IJavaInstance)
    ['{E08141A2-67FB-4EA7-AECD-2E1E3C1616F8}']
    procedure showAd(context: JContext); cdecl;
  end;
  TJMediationRewardedAd = class(TJavaGenericImport<JMediationRewardedAdClass, JMediationRewardedAd>) end;

  JMediationNativeListenerClass = interface(IJavaClass)
    ['{3B0E0B6B-9964-4B8B-B424-0F2A6D28B666}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationNativeListener')]
  JMediationNativeListener = interface(IJavaInstance)
    ['{2117826A-5096-4F53-BFE4-98BFB691D4D5}']
    procedure onAdClicked(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onAdClosed(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onAdFailedToLoad(mediationNativeAdapter: JMediationNativeAdapter; adError: JAdError); overload; cdecl;
    procedure onAdFailedToLoad(mediationNativeAdapter: JMediationNativeAdapter; int: Integer); overload; cdecl;
    procedure onAdImpression(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onAdLeftApplication(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onAdLoaded(mediationNativeAdapter: JMediationNativeAdapter; unifiedNativeAdMapper: JUnifiedNativeAdMapper); cdecl;
    procedure onAdOpened(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
    procedure onVideoEnd(mediationNativeAdapter: JMediationNativeAdapter); cdecl;
  end;
  TJMediationNativeListener = class(TJavaGenericImport<JMediationNativeListenerClass, JMediationNativeListener>) end;

  JMediationInterstitialListenerClass = interface(IJavaClass)
    ['{E21DDFA3-9EB0-4781-A9DA-518DCA7FEDDD}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationInterstitialListener')]
  JMediationInterstitialListener = interface(IJavaInstance)
    ['{1D8548BD-6DCC-4036-9021-F7D12D967CCB}']
    procedure onAdClicked(mediationInterstitialAdapter: JMediationInterstitialAdapter); cdecl;
    procedure onAdClosed(mediationInterstitialAdapter: JMediationInterstitialAdapter); cdecl;
    procedure onAdFailedToLoad(mediationInterstitialAdapter: JMediationInterstitialAdapter; adError: JAdError); overload; cdecl;
    procedure onAdFailedToLoad(mediationInterstitialAdapter: JMediationInterstitialAdapter; int: Integer); overload; cdecl;
    procedure onAdLeftApplication(mediationInterstitialAdapter: JMediationInterstitialAdapter); cdecl;
    procedure onAdLoaded(mediationInterstitialAdapter: JMediationInterstitialAdapter); cdecl;
    procedure onAdOpened(mediationInterstitialAdapter: JMediationInterstitialAdapter); cdecl;
  end;
  TJMediationInterstitialListener = class(TJavaGenericImport<JMediationInterstitialListenerClass, JMediationInterstitialListener>) end;

  JMediationInterstitialAdClass = interface(IJavaClass)
    ['{FA841E9C-0CA5-4D6F-92A9-08E517CD4802}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationInterstitialAd')]
  JMediationInterstitialAd = interface(IJavaInstance)
    ['{D151D4D8-5D46-4482-89ED-BB6AFC9EDF47}']
    procedure showAd(context: JContext); cdecl;
  end;
  TJMediationInterstitialAd = class(TJavaGenericImport<JMediationInterstitialAdClass, JMediationInterstitialAd>) end;

  JMediationExtrasReceiverClass = interface(IJavaClass)
    ['{CB30F30A-031D-41D1-857A-DD1A0BC82C1B}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationExtrasReceiver')]
  JMediationExtrasReceiver = interface(IJavaInstance)
    ['{791B52C8-B125-4918-BA4B-B4C4FCC36391}']
  end;
  TJMediationExtrasReceiver = class(TJavaGenericImport<JMediationExtrasReceiverClass, JMediationExtrasReceiver>) end;

  JMediationConfigurationClass = interface(JObjectClass)
    ['{19C59398-9180-404A-BD2F-3E0CCC66A935}']
    {class} function _GetCUSTOM_EVENT_SERVER_PARAMETER_FIELD: JString; cdecl;
    {class} function init(adFormat: JAdFormat; bundle: JBundle): JMediationConfiguration; cdecl;
    {class} property CUSTOM_EVENT_SERVER_PARAMETER_FIELD: JString read _GetCUSTOM_EVENT_SERVER_PARAMETER_FIELD;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationConfiguration')]
  JMediationConfiguration = interface(JObject)
    ['{1D613B55-3C5C-48AE-9035-6D23600E934A}']
    function getFormat: JAdFormat; cdecl;
    function getServerParameters: JBundle; cdecl;
  end;
  TJMediationConfiguration = class(TJavaGenericImport<JMediationConfigurationClass, JMediationConfiguration>) end;

  JMediationBannerListenerClass = interface(IJavaClass)
    ['{242BBBE4-1518-483F-9716-803BE6866A91}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationBannerListener')]
  JMediationBannerListener = interface(IJavaInstance)
    ['{A1DB4DA6-23C6-4FAB-92DD-39380ACF9A69}']
    procedure onAdClicked(mediationBannerAdapter: JMediationBannerAdapter); cdecl;
    procedure onAdClosed(mediationBannerAdapter: JMediationBannerAdapter); cdecl;
    procedure onAdFailedToLoad(mediationBannerAdapter: JMediationBannerAdapter; adError: JAdError); overload; cdecl;
    procedure onAdFailedToLoad(mediationBannerAdapter: JMediationBannerAdapter; int: Integer); overload; cdecl;
    procedure onAdLeftApplication(mediationBannerAdapter: JMediationBannerAdapter); cdecl;
    procedure onAdLoaded(mediationBannerAdapter: JMediationBannerAdapter); cdecl;
    procedure onAdOpened(mediationBannerAdapter: JMediationBannerAdapter); cdecl;
  end;
  TJMediationBannerListener = class(TJavaGenericImport<JMediationBannerListenerClass, JMediationBannerListener>) end;

  JMediationBannerAdClass = interface(IJavaClass)
    ['{28EBE2A6-5402-48F9-B5CE-D76D91678B4B}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationBannerAd')]
  JMediationBannerAd = interface(IJavaInstance)
    ['{81D3D7B9-AE58-4A06-856D-0D17754EB994}']
    function getView: JView; cdecl;
  end;
  TJMediationBannerAd = class(TJavaGenericImport<JMediationBannerAdClass, JMediationBannerAd>) end;

  JMediationAppOpenAdClass = interface(IJavaClass)
    ['{DBC6ADB4-913A-4622-9929-AC4A69B6F134}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAppOpenAd')]
  JMediationAppOpenAd = interface(IJavaInstance)
    ['{9DEB52A7-3584-4E75-9348-1C4FA8BB3B94}']
    procedure showAd(context: JContext); cdecl;
  end;
  TJMediationAppOpenAd = class(TJavaGenericImport<JMediationAppOpenAdClass, JMediationAppOpenAd>) end;

  JMediationAdapterClass = interface(JMediationExtrasReceiverClass)
    ['{2AA72147-E000-4C59-9B07-D34E385ACF36}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAdapter')]
  JMediationAdapter = interface(JMediationExtrasReceiver)
    ['{CEF46AFB-9CE7-45EF-B5B8-99DC4C5A7003}']
    procedure onDestroy; cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
  end;
  TJMediationAdapter = class(TJavaGenericImport<JMediationAdapterClass, JMediationAdapter>) end;

  JMediationAdRequestClass = interface(IJavaClass)
    ['{01E06282-1869-4417-BC49-DF087AED4A80}']
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer; cdecl;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAdRequest')]
  JMediationAdRequest = interface(IJavaInstance)
    ['{9809DF69-2FB7-4E05-9C47-1C9844EF6C63}']
    function getBirthday: JDate; cdecl;
    function getGender: Integer; cdecl;
    function getKeywords: JSet; cdecl;
    function getLocation: JLocation; cdecl;
    function isDesignedForFamilies: Boolean; cdecl;
    function isTesting: Boolean; cdecl;
    function taggedForChildDirectedTreatment: Integer; cdecl;
  end;
  TJMediationAdRequest = class(TJavaGenericImport<JMediationAdRequestClass, JMediationAdRequest>) end;

  JMediationAdLoadCallbackClass = interface(IJavaClass)
    ['{6407E758-4B9A-4A29-8DF5-79A0873CEB51}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAdLoadCallback')]
  JMediationAdLoadCallback = interface(IJavaInstance)
    ['{DDAB91BA-BB10-43F2-A599-7679082D59F9}']
    procedure onFailure(adError: JAdError); cdecl;
    function onSuccess(mediationAdT: JObject): JObject; cdecl;
  end;
  TJMediationAdLoadCallback = class(TJavaGenericImport<JMediationAdLoadCallbackClass, JMediationAdLoadCallback>) end;

  JMediationAdConfigurationClass = interface(JObjectClass)
    ['{EF6D0CA9-CA47-4BE8-ADEF-D2955CC36D23}']
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer; cdecl;
    {class} function init(context: JContext; string_1: JString; bundle: JBundle; bundle_1: JBundle; boolean: Boolean; location_1: JLocation; int: Integer; int_1: Integer; string_2: JString; string_3: JString): JMediationAdConfiguration; cdecl;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAdConfiguration')]
  JMediationAdConfiguration = interface(JObject)
    ['{D9F1A9AB-0DB3-4FF6-8B47-5251BC780B9A}']
    function getBidResponse: JString; cdecl;
    function getContext: JContext; cdecl;
    function getMaxAdContentRating: JString; cdecl;
    function getMediationExtras: JBundle; cdecl;
    function getServerParameters: JBundle; cdecl;
    function getWatermark: JString; cdecl;
    function isTestRequest: Boolean; cdecl;
    function taggedForChildDirectedTreatment: Integer; cdecl;
    function taggedForUnderAgeTreatment: Integer; cdecl;
  end;
  TJMediationAdConfiguration = class(TJavaGenericImport<JMediationAdConfigurationClass, JMediationAdConfiguration>) end;

  JMediationAdConfiguration_TagForChildDirectedTreatmentClass = interface(JAnnotationClass)
    ['{2ED8423E-4177-42C0-A696-922BD5F096AD}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAdConfiguration$TagForChildDirectedTreatment')]
  JMediationAdConfiguration_TagForChildDirectedTreatment = interface(JAnnotation)
    ['{BA405E17-EA25-4CDF-94A3-51F8BB2B3711}']
  end;
  TJMediationAdConfiguration_TagForChildDirectedTreatment = class(TJavaGenericImport<JMediationAdConfiguration_TagForChildDirectedTreatmentClass, JMediationAdConfiguration_TagForChildDirectedTreatment>) end;

  JMediationAdCallbackClass = interface(IJavaClass)
    ['{B4192717-BCC2-4536-9567-F6434446750C}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAdCallback')]
  JMediationAdCallback = interface(IJavaInstance)
    ['{0066A49D-4E32-4168-9155-4F61AED14F2B}']
    procedure onAdClosed; cdecl;
    procedure onAdOpened; cdecl;
    procedure reportAdClicked; cdecl;
    procedure reportAdImpression; cdecl;
  end;
  TJMediationAdCallback = class(TJavaGenericImport<JMediationAdCallbackClass, JMediationAdCallback>) end;

  JInitializationCompleteCallbackClass = interface(IJavaClass)
    ['{51913840-123B-4003-B50F-8E63B7D4BAC0}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/InitializationCompleteCallback')]
  JInitializationCompleteCallback = interface(IJavaInstance)
    ['{E80AB603-1DE8-4E05-B4DB-5919CD5EC7CD}']
    procedure onInitializationFailed(string_1: JString); cdecl;
    procedure onInitializationSucceeded; cdecl;
  end;
  TJInitializationCompleteCallback = class(TJavaGenericImport<JInitializationCompleteCallbackClass, JInitializationCompleteCallback>) end;

  JAdapterClass = interface(JObjectClass)
    ['{B5067C4F-634E-46D9-9BA5-BDE6A2E89C7D}']
    {class} function init: JAdapter; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/Adapter')]
  JAdapter = interface(JObject)
    ['{D668EC40-4810-497B-B643-158267E8DC23}']
    function getSDKVersionInfo: Jads_VersionInfo; cdecl;
    function getVersionInfo: Jads_VersionInfo; cdecl;
    procedure initialize(context: JContext; initializationCompleteCallback: JInitializationCompleteCallback; list: JList); cdecl;
    procedure loadAppOpenAd(mediationAppOpenAdConfiguration: JMediationAppOpenAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadBannerAd(mediationBannerAdConfiguration: JMediationBannerAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadInterstitialAd(mediationInterstitialAdConfiguration: JMediationInterstitialAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadNativeAd(mediationNativeAdConfiguration: JMediationNativeAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadNativeAdMapper(mediationNativeAdConfiguration: JMediationNativeAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadRewardedAd(mediationRewardedAdConfiguration: JMediationRewardedAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadRewardedInterstitialAd(mediationRewardedAdConfiguration: JMediationRewardedAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
  end;
  TJAdapter = class(TJavaGenericImport<JAdapterClass, JAdapter>) end;

  JInterstitialAdPreloaderClass = interface(JObjectClass)
    ['{4CFE8AC7-36B5-4EE1-BE70-FA58348208CD}']
    {class} function destroy(string_1: JString): Boolean; cdecl;
    {class} procedure destroyAll; cdecl;
    {class} function getConfiguration(string_1: JString): JPreloadConfiguration; cdecl;
    {class} function getConfigurations: JMap; cdecl;
    {class} function getNumAdsAvailable(string_1: JString): Integer; cdecl;
    {class} function isAdAvailable(string_1: JString): Boolean; cdecl;
    {class} function pollAd(string_1: JString): Jinterstitial_InterstitialAd; cdecl;
    {class} function start(string_1: JString; preloadConfiguration: JPreloadConfiguration; preloadCallbackV2: JPreloadCallbackV2): Boolean; overload; cdecl;
    {class} function start(string_1: JString; preloadConfiguration: JPreloadConfiguration): Boolean; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/interstitial/InterstitialAdPreloader')]
  JInterstitialAdPreloader = interface(JObject)
    ['{49DF99E7-C763-4E27-9410-B106EDA463A3}']
  end;
  TJInterstitialAdPreloader = class(TJavaGenericImport<JInterstitialAdPreloaderClass, JInterstitialAdPreloader>) end;

  JInterstitialAdClass = interface(JObjectClass)
    ['{FC9BB4CC-784A-4D0A-B82E-DD8ADDB77EDC}']
    {class} function init: JInterstitialAd; cdecl;
    {class} function isAdAvailable(context: JContext; string_1: JString): Boolean; cdecl;
    {class} procedure load(context: JContext; string_1: JString; adRequest: JAdRequest; interstitialAdLoadCallback: JInterstitialAdLoadCallback); cdecl;
    {class} function pollAd(context: JContext; string_1: JString): JInterstitialAd; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/interstitial/InterstitialAd')]
  JInterstitialAd = interface(JObject)
    ['{42CB0BCF-E4C2-45A8-86BD-3954C1FA4C8D}']
    function getAdUnitId: JString; cdecl;
    function getFullScreenContentCallback: JFullScreenContentCallback; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getPlacementId: Int64; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure setImmersiveMode(boolean: Boolean); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setPlacementId(long: Int64); cdecl;
    procedure show(activity: JActivity); cdecl;
  end;
  TJInterstitialAd = class(TJavaGenericImport<JInterstitialAdClass, JInterstitialAd>) end;

  JVersionInfoParcelClass = interface(JAbstractSafeParcelableClass)
    ['{49890E20-9D64-432C-A6E7-EE0A20AEB18F}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function forPackage: JVersionInfoParcel; cdecl;
    {class} function init(int: Integer; int_1: Integer; boolean: Boolean; boolean_1: Boolean): JVersionInfoParcel; overload; cdecl;
    {class} function init(int: Integer; int_1: Integer; boolean: Boolean): JVersionInfoParcel; overload; cdecl;
    {class} function init(int: Integer; int_1: Integer; boolean: Boolean; boolean_1: Boolean; boolean_2: Boolean): JVersionInfoParcel; overload; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/ads/internal/util/client/VersionInfoParcel')]
  JVersionInfoParcel = interface(JAbstractSafeParcelable)
    ['{426DC128-72FB-47BB-A206-E3138F0AC346}']
    function _GetafmaVersion: JString; cdecl;
    function _GetbuddyApkVersion: Integer; cdecl;
    function _GetclientJarVersion: Integer; cdecl;
    function _GetisClientJar: Boolean; cdecl;
    function _GetisLiteSdk: Boolean; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
    property afmaVersion: JString read _GetafmaVersion;
    property buddyApkVersion: Integer read _GetbuddyApkVersion;
    property clientJarVersion: Integer read _GetclientJarVersion;
    property isClientJar: Boolean read _GetisClientJar;
    property isLiteSdk: Boolean read _GetisLiteSdk;
  end;
  TJVersionInfoParcel = class(TJavaGenericImport<JVersionInfoParcelClass, JVersionInfoParcel>) end;

  JOfflinePingSenderClass = interface(JWorkerClass)
    ['{CF72D107-A4F4-4B0D-B851-E72E665D82D5}']
    {class} function init(context: JContext; workerParameters: JWorkerParameters): JOfflinePingSender; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/internal/offline/buffering/OfflinePingSender')]
  JOfflinePingSender = interface(JWorker)
    ['{0E2BD366-A45F-4324-B099-8F34CBFC54B4}']
    function doWork: JListenableWorker_Result; cdecl;
  end;
  TJOfflinePingSender = class(TJavaGenericImport<JOfflinePingSenderClass, JOfflinePingSender>) end;

  JOfflineNotificationPosterClass = interface(JWorkerClass)
    ['{F053200A-9010-45E9-8B13-F10012BAA173}']
    {class} function init(context: JContext; workerParameters: JWorkerParameters): JOfflineNotificationPoster; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/internal/offline/buffering/OfflineNotificationPoster')]
  JOfflineNotificationPoster = interface(JWorker)
    ['{E6EF5919-2647-4BC3-A836-6CE55C30FCFB}']
    function doWork: JListenableWorker_Result; cdecl;
  end;
  TJOfflineNotificationPoster = class(TJavaGenericImport<JOfflineNotificationPosterClass, JOfflineNotificationPoster>) end;

  JOnInitializationCompleteListenerClass = interface(IJavaClass)
    ['{3E6BD1EE-BD63-4B48-9DB0-74FA31B4A1F6}']
  end;

  [JavaSignature('com/google/android/gms/ads/initialization/OnInitializationCompleteListener')]
  JOnInitializationCompleteListener = interface(IJavaInstance)
    ['{8AA50D6F-B57F-474F-848B-EDF2E04449C8}']
    procedure onInitializationComplete(initializationStatus: JInitializationStatus); cdecl;
  end;
  TJOnInitializationCompleteListener = class(TJavaGenericImport<JOnInitializationCompleteListenerClass, JOnInitializationCompleteListener>) end;

  JInitializationStatusClass = interface(IJavaClass)
    ['{966C9539-ECC5-45EF-AF7A-0A8450C28A0E}']
  end;

  [JavaSignature('com/google/android/gms/ads/initialization/InitializationStatus')]
  JInitializationStatus = interface(IJavaInstance)
    ['{611FCAAA-C03B-4D20-916F-CA7EF269AD50}']
    function getAdapterStatusMap: JMap; cdecl;
  end;
  TJInitializationStatus = class(TJavaGenericImport<JInitializationStatusClass, JInitializationStatus>) end;

  JAdapterStatusClass = interface(IJavaClass)
    ['{201254FA-DAFE-44AB-B4A1-54AB90BADC66}']
  end;

  [JavaSignature('com/google/android/gms/ads/initialization/AdapterStatus')]
  JAdapterStatus = interface(IJavaInstance)
    ['{A0CFE553-779D-4537-B8E1-EF10065204A1}']
    function getDescription: JString; cdecl;
    function getInitializationState: JAdapterStatus_State; cdecl;
    function getLatency: Integer; cdecl;
  end;
  TJAdapterStatus = class(TJavaGenericImport<JAdapterStatusClass, JAdapterStatus>) end;

  JAdapterStatus_StateClass = interface(JEnumClass)
    ['{381ECCB7-5713-4457-AB96-BAA4B2CE598C}']
    {class} function _GetNOT_READY: JAdapterStatus_State; cdecl;
    {class} function _GetREADY: JAdapterStatus_State; cdecl;
    {class} function valueOf(string_1: JString): JAdapterStatus_State; cdecl;
    {class} function values: TJavaObjectArray<JAdapterStatus_State>; cdecl;
    {class} property NOT_READY: JAdapterStatus_State read _GetNOT_READY;
    {class} property READY: JAdapterStatus_State read _GetREADY;
  end;

  [JavaSignature('com/google/android/gms/ads/initialization/AdapterStatus$State')]
  JAdapterStatus_State = interface(JEnum)
    ['{B1E89709-3648-4091-9D9F-3A1277749429}']
  end;
  TJAdapterStatus_State = class(TJavaGenericImport<JAdapterStatus_StateClass, JAdapterStatus_State>) end;

  JUnifiedNativeAdAssetNamesClass = interface(JObjectClass)
    ['{47BDBB4C-3A29-4FE3-8F85-78219F5067FC}']
    {class} function _GetASSET_ADCHOICES_CONTAINER_VIEW: JString; cdecl;
    {class} function _GetASSET_ADVERTISER: JString; cdecl;
    {class} function _GetASSET_BODY: JString; cdecl;
    {class} function _GetASSET_CALL_TO_ACTION: JString; cdecl;
    {class} function _GetASSET_HEADLINE: JString; cdecl;
    {class} function _GetASSET_ICON: JString; cdecl;
    {class} function _GetASSET_IMAGE: JString; cdecl;
    {class} function _GetASSET_MEDIA_VIDEO: JString; cdecl;
    {class} function _GetASSET_PRICE: JString; cdecl;
    {class} function _GetASSET_STAR_RATING: JString; cdecl;
    {class} function _GetASSET_STORE: JString; cdecl;
    {class} function init: JUnifiedNativeAdAssetNames; cdecl;
    {class} property ASSET_ADCHOICES_CONTAINER_VIEW: JString read _GetASSET_ADCHOICES_CONTAINER_VIEW;
    {class} property ASSET_ADVERTISER: JString read _GetASSET_ADVERTISER;
    {class} property ASSET_BODY: JString read _GetASSET_BODY;
    {class} property ASSET_CALL_TO_ACTION: JString read _GetASSET_CALL_TO_ACTION;
    {class} property ASSET_HEADLINE: JString read _GetASSET_HEADLINE;
    {class} property ASSET_ICON: JString read _GetASSET_ICON;
    {class} property ASSET_IMAGE: JString read _GetASSET_IMAGE;
    {class} property ASSET_MEDIA_VIDEO: JString read _GetASSET_MEDIA_VIDEO;
    {class} property ASSET_PRICE: JString read _GetASSET_PRICE;
    {class} property ASSET_STAR_RATING: JString read _GetASSET_STAR_RATING;
    {class} property ASSET_STORE: JString read _GetASSET_STORE;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/UnifiedNativeAdAssetNames')]
  JUnifiedNativeAdAssetNames = interface(JObject)
    ['{2BD58A1C-71D6-4222-B735-8684E6C061A6}']
  end;
  TJUnifiedNativeAdAssetNames = class(TJavaGenericImport<JUnifiedNativeAdAssetNamesClass, JUnifiedNativeAdAssetNames>) end;

  JUnifiedNativeAdClass = interface(JObjectClass)
    ['{2513B5F5-172F-41E8-B8A9-C516BABF8128}']
    {class} function init: JUnifiedNativeAd; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/UnifiedNativeAd')]
  JUnifiedNativeAd = interface(JObject)
    ['{222C1F9F-A1E7-4EB8-9296-73B00A8FF5A7}']
    procedure performClick(bundle: JBundle); cdecl;
    function recordImpression(bundle: JBundle): Boolean; cdecl;
    procedure reportTouchEvent(bundle: JBundle); cdecl;
  end;
  TJUnifiedNativeAd = class(TJavaGenericImport<JUnifiedNativeAdClass, JUnifiedNativeAd>) end;

  JShouldDelayBannerRenderingListenerClass = interface(IJavaClass)
    ['{0726089E-CA44-4C25-8E15-B3CD71C44394}']
  end;

  [JavaSignature('com/google/android/gms/ads/formats/ShouldDelayBannerRenderingListener')]
  JShouldDelayBannerRenderingListener = interface(IJavaInstance)
    ['{3F4F6F55-356E-4CA0-A649-85E02EB65C55}']
  end;
  TJShouldDelayBannerRenderingListener = class(TJavaGenericImport<JShouldDelayBannerRenderingListenerClass, JShouldDelayBannerRenderingListener>) end;

  JPublisherAdViewOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{FD3F7DC0-7170-4DA4-8610-4D9AB6102729}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/PublisherAdViewOptions')]
  JPublisherAdViewOptions = interface(JAbstractSafeParcelable)
    ['{7C32C2CC-69AA-4B4D-AA34-D009DA16A11E}']
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJPublisherAdViewOptions = class(TJavaGenericImport<JPublisherAdViewOptionsClass, JPublisherAdViewOptions>) end;

  JPublisherAdViewOptions_BuilderClass = interface(JObjectClass)
    ['{550DA44A-BE78-4BEB-97CC-EBBC19B1DD4C}']
    {class} function init: JPublisherAdViewOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/PublisherAdViewOptions$Builder')]
  JPublisherAdViewOptions_Builder = interface(JObject)
    ['{352AD8E8-2CBF-48BD-914E-8DFA85E5C002}']
    function setShouldDelayBannerRenderingListener(shouldDelayBannerRenderingListener: JShouldDelayBannerRenderingListener): JPublisherAdViewOptions_Builder; cdecl;
  end;
  TJPublisherAdViewOptions_Builder = class(TJavaGenericImport<JPublisherAdViewOptions_BuilderClass, JPublisherAdViewOptions_Builder>) end;

  JOnAdManagerAdViewLoadedListenerClass = interface(IJavaClass)
    ['{5330A885-40BF-4A25-9ABC-32023A38F505}']
  end;

  [JavaSignature('com/google/android/gms/ads/formats/OnAdManagerAdViewLoadedListener')]
  JOnAdManagerAdViewLoadedListener = interface(IJavaInstance)
    ['{413342C0-EA55-4FE0-A6FF-4EAF892B6EF4}']
    procedure onAdManagerAdViewLoaded(adManagerAdView: JAdManagerAdView); cdecl;
  end;
  TJOnAdManagerAdViewLoadedListener = class(TJavaGenericImport<JOnAdManagerAdViewLoadedListenerClass, JOnAdManagerAdViewLoadedListener>) end;

  JNativeAdOptionsClass = interface(JObjectClass)
    ['{E913F312-3D92-4E88-B0A8-CE410090DDA5}']
    {class} function _GetADCHOICES_BOTTOM_LEFT: Integer; cdecl;
    {class} function _GetADCHOICES_BOTTOM_RIGHT: Integer; cdecl;
    {class} function _GetADCHOICES_TOP_LEFT: Integer; cdecl;
    {class} function _GetADCHOICES_TOP_RIGHT: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_ANY: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_PORTRAIT: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_SQUARE: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_UNKNOWN: Integer; cdecl;
    {class} function _GetORIENTATION_ANY: Integer; cdecl;
    {class} function _GetORIENTATION_LANDSCAPE: Integer; cdecl;
    {class} function _GetORIENTATION_PORTRAIT: Integer; cdecl;
    {class} property ADCHOICES_BOTTOM_LEFT: Integer read _GetADCHOICES_BOTTOM_LEFT;
    {class} property ADCHOICES_BOTTOM_RIGHT: Integer read _GetADCHOICES_BOTTOM_RIGHT;
    {class} property ADCHOICES_TOP_LEFT: Integer read _GetADCHOICES_TOP_LEFT;
    {class} property ADCHOICES_TOP_RIGHT: Integer read _GetADCHOICES_TOP_RIGHT;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_ANY: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_ANY;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_PORTRAIT: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_PORTRAIT;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_SQUARE: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_SQUARE;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_UNKNOWN: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_UNKNOWN;
    {class} property ORIENTATION_ANY: Integer read _GetORIENTATION_ANY;
    {class} property ORIENTATION_LANDSCAPE: Integer read _GetORIENTATION_LANDSCAPE;
    {class} property ORIENTATION_PORTRAIT: Integer read _GetORIENTATION_PORTRAIT;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAdOptions')]
  JNativeAdOptions = interface(JObject)
    ['{8D452510-8E3B-40A6-BAEF-B486B0FEC541}']
    function getAdChoicesPlacement: Integer; cdecl;
    function getImageOrientation: Integer; cdecl;
    function getMediaAspectRatio: Integer; cdecl;
    function getVideoOptions: JVideoOptions; cdecl;
    function shouldRequestMultipleImages: Boolean; cdecl;
    function shouldReturnUrlsForImageAssets: Boolean; cdecl;
  end;
  TJNativeAdOptions = class(TJavaGenericImport<JNativeAdOptionsClass, JNativeAdOptions>) end;

  JNativeAdOptions_NativeMediaAspectRatioClass = interface(JAnnotationClass)
    ['{A3D7E2DE-BAF6-4219-A63F-6C8E51A535DB}']
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAdOptions$NativeMediaAspectRatio')]
  JNativeAdOptions_NativeMediaAspectRatio = interface(JAnnotation)
    ['{77360958-9A9E-4CD3-B072-BE11405BBF51}']
  end;
  TJNativeAdOptions_NativeMediaAspectRatio = class(TJavaGenericImport<JNativeAdOptions_NativeMediaAspectRatioClass, JNativeAdOptions_NativeMediaAspectRatio>) end;

  JNativeAdOptions_BuilderClass = interface(JObjectClass)
    ['{D800CEDA-A5DD-444B-8E66-75B0F374B6DA}']
    {class} function init: JNativeAdOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAdOptions$Builder')]
  JNativeAdOptions_Builder = interface(JObject)
    ['{6DB0785A-B312-40F6-8D19-56589F7F8028}']
    function build: JNativeAdOptions; cdecl;
    function setAdChoicesPlacement(int: Integer): JNativeAdOptions_Builder; cdecl;
    function setImageOrientation(int: Integer): JNativeAdOptions_Builder; cdecl;
    function setMediaAspectRatio(int: Integer): JNativeAdOptions_Builder; cdecl;
    function setRequestCustomMuteThisAd(boolean: Boolean): JNativeAdOptions_Builder; cdecl;
    function setRequestMultipleImages(boolean: Boolean): JNativeAdOptions_Builder; cdecl;
    function setReturnUrlsForImageAssets(boolean: Boolean): JNativeAdOptions_Builder; cdecl;
    function setVideoOptions(videoOptions: JVideoOptions): JNativeAdOptions_Builder; cdecl;
  end;
  TJNativeAdOptions_Builder = class(TJavaGenericImport<JNativeAdOptions_BuilderClass, JNativeAdOptions_Builder>) end;

  JNativeAdOptions_AdChoicesPlacementClass = interface(JAnnotationClass)
    ['{17A0586A-3113-480C-AF78-4BE84A90CE32}']
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAdOptions$AdChoicesPlacement')]
  JNativeAdOptions_AdChoicesPlacement = interface(JAnnotation)
    ['{3F46D1FC-7DAA-4833-B408-E40030EFC936}']
  end;
  TJNativeAdOptions_AdChoicesPlacement = class(TJavaGenericImport<JNativeAdOptions_AdChoicesPlacementClass, JNativeAdOptions_AdChoicesPlacement>) end;

  JNativeAdClass = interface(JObjectClass)
    ['{64820319-A816-4670-9E84-1B1D86D52352}']
    {class} function _GetASSET_ADCHOICES_CONTAINER_VIEW: JString; cdecl;
    {class} function init: JNativeAd; cdecl;
    {class} property ASSET_ADCHOICES_CONTAINER_VIEW: JString read _GetASSET_ADCHOICES_CONTAINER_VIEW;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAd')]
  JNativeAd = interface(JObject)
    ['{C283FDD8-B7DE-4556-8F83-E9C2E52D105A}']
    procedure performClick(bundle: JBundle); cdecl;
    function recordImpression(bundle: JBundle): Boolean; cdecl;
    procedure reportTouchEvent(bundle: JBundle); cdecl;
  end;
  TJNativeAd = class(TJavaGenericImport<JNativeAdClass, JNativeAd>) end;

  JNativeAd_ImageClass = interface(JObjectClass)
    ['{B251ECF2-9975-464B-A02D-305C6D054CF7}']
    {class} function init: JNativeAd_Image; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAd$Image')]
  JNativeAd_Image = interface(JObject)
    ['{C57BE1AB-1F0A-4F92-8325-30F8D0365634}']
    function getDrawable: JDrawable; cdecl;
    function getScale: Double; cdecl;
    function getUri: Jnet_Uri; cdecl;
  end;
  TJNativeAd_Image = class(TJavaGenericImport<JNativeAd_ImageClass, JNativeAd_Image>) end;

  JNativeAd_AdChoicesInfoClass = interface(JObjectClass)
    ['{06C2B489-3D69-4D87-B078-EE566FF56733}']
    {class} function init: JNativeAd_AdChoicesInfo; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/NativeAd$AdChoicesInfo')]
  JNativeAd_AdChoicesInfo = interface(JObject)
    ['{3BAC08BD-0A03-4E22-A20A-BD907205FCD9}']
    function getImages: JList; cdecl;
    function getText: JCharSequence; cdecl;
  end;
  TJNativeAd_AdChoicesInfo = class(TJavaGenericImport<JNativeAd_AdChoicesInfoClass, JNativeAd_AdChoicesInfo>) end;

  JMediaViewClass = interface(JFrameLayoutClass)
    ['{4A31CC42-1BCB-4D0A-AA7A-2B2BE8C07464}']
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer; int_1: Integer): JMediaView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer): JMediaView; overload; cdecl;
    {class} function init(context: JContext): JMediaView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JMediaView; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/MediaView')]
  JMediaView = interface(JFrameLayout)
    ['{CB782166-A3C6-4DCE-B8B1-CB3BDF0275D5}']
    procedure setImageScaleType(scaleType: JImageView_ScaleType); cdecl;
    procedure setMediaContent(mediaContent: JMediaContent); cdecl;
  end;
  TJMediaView = class(TJavaGenericImport<JMediaViewClass, JMediaView>) end;

  JAdManagerAdViewOptionsClass = interface(JAbstractSafeParcelableClass)
    ['{03796671-5789-4E95-9642-7A673A9C4673}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/AdManagerAdViewOptions')]
  JAdManagerAdViewOptions = interface(JAbstractSafeParcelable)
    ['{658FF16F-B891-4FE3-B9D9-F6A805981939}']
    function getManualImpressionsEnabled: Boolean; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
  end;
  TJAdManagerAdViewOptions = class(TJavaGenericImport<JAdManagerAdViewOptionsClass, JAdManagerAdViewOptions>) end;

  JAdManagerAdViewOptions_BuilderClass = interface(JObjectClass)
    ['{3F2E5449-999F-4F3C-9467-E91E9487D85A}']
    {class} function init: JAdManagerAdViewOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/formats/AdManagerAdViewOptions$Builder')]
  JAdManagerAdViewOptions_Builder = interface(JObject)
    ['{5F3DBF46-AA79-4211-B32B-42E193791E0C}']
    function build: JAdManagerAdViewOptions; cdecl;
    function setManualImpressionsEnabled(boolean: Boolean): JAdManagerAdViewOptions_Builder; cdecl;
  end;
  TJAdManagerAdViewOptions_Builder = class(TJavaGenericImport<JAdManagerAdViewOptions_BuilderClass, JAdManagerAdViewOptions_Builder>) end;

  JAppOpenAdPreloaderClass = interface(JObjectClass)
    ['{02134EB9-6DF2-4EBB-B1AE-9F58EB043B88}']
    {class} function destroy(string_1: JString): Boolean; cdecl;
    {class} procedure destroyAll; cdecl;
    {class} function getConfiguration(string_1: JString): JPreloadConfiguration; cdecl;
    {class} function getConfigurations: JMap; cdecl;
    {class} function getNumAdsAvailable(string_1: JString): Integer; cdecl;
    {class} function isAdAvailable(string_1: JString): Boolean; cdecl;
    {class} function pollAd(string_1: JString): JAppOpenAd; cdecl;
    {class} function start(string_1: JString; preloadConfiguration: JPreloadConfiguration; preloadCallbackV2: JPreloadCallbackV2): Boolean; overload; cdecl;
    {class} function start(string_1: JString; preloadConfiguration: JPreloadConfiguration): Boolean; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/appopen/AppOpenAdPreloader')]
  JAppOpenAdPreloader = interface(JObject)
    ['{963F67B8-51BC-4212-9A13-E10C1B9734B8}']
  end;
  TJAppOpenAdPreloader = class(TJavaGenericImport<JAppOpenAdPreloaderClass, JAppOpenAdPreloader>) end;

  JAppOpenAdClass = interface(JObjectClass)
    ['{479C1FEF-AEFF-47F8-8E38-E8460EE96135}']
    {class} function init: JAppOpenAd; cdecl;
    {class} function isAdAvailable(context: JContext; string_1: JString): Boolean; cdecl;
    {class} procedure load(context: JContext; string_1: JString; adRequest: JAdRequest; appOpenAdLoadCallback: JAppOpenAd_AppOpenAdLoadCallback); cdecl;
    {class} function pollAd(context: JContext; string_1: JString): JAppOpenAd; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/appopen/AppOpenAd')]
  JAppOpenAd = interface(JObject)
    ['{50C88C5D-139F-4997-8A62-531EB3D2CD53}']
    function getAdUnitId: JString; cdecl;
    function getFullScreenContentCallback: JFullScreenContentCallback; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getPlacementId: Int64; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure setImmersiveMode(boolean: Boolean); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setPlacementId(long: Int64); cdecl;
    procedure show(activity: JActivity); cdecl;
  end;
  TJAppOpenAd = class(TJavaGenericImport<JAppOpenAdClass, JAppOpenAd>) end;

  JAppEventListenerClass = interface(IJavaClass)
    ['{74B0D4E3-D2C9-43CD-A865-C0C90361C7D0}']
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AppEventListener')]
  JAppEventListener = interface(IJavaInstance)
    ['{764AF780-DD69-4D39-A9B5-CD1D388D4551}']
    procedure onAppEvent(string_1: JString; string_2: JString); cdecl;
  end;
  TJAppEventListener = class(TJavaGenericImport<JAppEventListenerClass, JAppEventListener>) end;

  JAdManagerInterstitialAdClass = interface(Jinterstitial_InterstitialAdClass)
    ['{7839DA36-4876-40A3-BFA8-16C43699D6E2}']
    {class} function init: JAdManagerInterstitialAd; cdecl;
    {class} procedure load(context: JContext; string_1: JString; adManagerAdRequest: JAdManagerAdRequest; adManagerInterstitialAdLoadCallback: JAdManagerInterstitialAdLoadCallback); cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AdManagerInterstitialAd')]
  JAdManagerInterstitialAd = interface(Jinterstitial_InterstitialAd)
    ['{BA90E914-BE6C-420C-A2CD-B9723FC6AEE2}']
    function getAppEventListener: Jadmanager_AppEventListener; cdecl;
    procedure setAppEventListener(appEventListener: Jadmanager_AppEventListener); cdecl;
  end;
  TJAdManagerInterstitialAd = class(TJavaGenericImport<JAdManagerInterstitialAdClass, JAdManagerInterstitialAd>) end;

  JVideoOptionsClass = interface(JObjectClass)
    ['{F8AAB2B5-DB4D-4F80-923D-C9F5381B56C9}']
  end;

  [JavaSignature('com/google/android/gms/ads/VideoOptions')]
  JVideoOptions = interface(JObject)
    ['{4F02DD62-34C5-44C2-8255-6EC4EB23C89C}']
    function getClickToExpandRequested: Boolean; cdecl;
    function getCustomControlsRequested: Boolean; cdecl;
    function getStartMuted: Boolean; cdecl;
  end;
  TJVideoOptions = class(TJavaGenericImport<JVideoOptionsClass, JVideoOptions>) end;

  JVideoOptions_BuilderClass = interface(JObjectClass)
    ['{20B5B008-EAA0-4F07-89AC-000F140ED3F0}']
    {class} function init: JVideoOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/VideoOptions$Builder')]
  JVideoOptions_Builder = interface(JObject)
    ['{D9AB0A4D-FFE6-4C55-BAB4-36ADA5B65DEE}']
    function build: JVideoOptions; cdecl;
    function setClickToExpandRequested(boolean: Boolean): JVideoOptions_Builder; cdecl;
    function setCustomControlsRequested(boolean: Boolean): JVideoOptions_Builder; cdecl;
    function setStartMuted(boolean: Boolean): JVideoOptions_Builder; cdecl;
  end;
  TJVideoOptions_Builder = class(TJavaGenericImport<JVideoOptions_BuilderClass, JVideoOptions_Builder>) end;

  JVideoControllerClass = interface(JObjectClass)
    ['{6D8B52CB-9B66-47A7-B831-62EC792F7CB2}']
    {class} function _GetPLAYBACK_STATE_ENDED: Integer; cdecl;
    {class} function _GetPLAYBACK_STATE_PAUSED: Integer; cdecl;
    {class} function _GetPLAYBACK_STATE_PLAYING: Integer; cdecl;
    {class} function _GetPLAYBACK_STATE_READY: Integer; cdecl;
    {class} function _GetPLAYBACK_STATE_UNKNOWN: Integer; cdecl;
    {class} function init: JVideoController; cdecl;
    {class} property PLAYBACK_STATE_ENDED: Integer read _GetPLAYBACK_STATE_ENDED;
    {class} property PLAYBACK_STATE_PAUSED: Integer read _GetPLAYBACK_STATE_PAUSED;
    {class} property PLAYBACK_STATE_PLAYING: Integer read _GetPLAYBACK_STATE_PLAYING;
    {class} property PLAYBACK_STATE_READY: Integer read _GetPLAYBACK_STATE_READY;
    {class} property PLAYBACK_STATE_UNKNOWN: Integer read _GetPLAYBACK_STATE_UNKNOWN;
  end;

  [JavaSignature('com/google/android/gms/ads/VideoController')]
  JVideoController = interface(JObject)
    ['{13A2BC1E-3D58-4709-9726-8B6FF76CE515}']
    function getPlaybackState: Integer; cdecl;
    function getVideoLifecycleCallbacks: JVideoController_VideoLifecycleCallbacks; cdecl;
    function hasVideoContent: Boolean; cdecl;
    function isClickToExpandEnabled: Boolean; cdecl;
    function isCustomControlsEnabled: Boolean; cdecl;
    function isMuted: Boolean; cdecl;
    procedure mute(boolean: Boolean); cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure setVideoLifecycleCallbacks(videoLifecycleCallbacks: JVideoController_VideoLifecycleCallbacks); cdecl;
    procedure stop; cdecl;
  end;
  TJVideoController = class(TJavaGenericImport<JVideoControllerClass, JVideoController>) end;

  JVideoController_VideoLifecycleCallbacksClass = interface(JObjectClass)
    ['{18E05678-0D4D-40B9-AEAE-077B7E21EAF3}']
    {class} function init: JVideoController_VideoLifecycleCallbacks; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/VideoController$VideoLifecycleCallbacks')]
  JVideoController_VideoLifecycleCallbacks = interface(JObject)
    ['{C31C337C-2723-44FE-A760-BAA5877BDAAA}']
    procedure onVideoEnd; cdecl;
    procedure onVideoMute(boolean: Boolean); cdecl;
    procedure onVideoPause; cdecl;
    procedure onVideoPlay; cdecl;
    procedure onVideoStart; cdecl;
  end;
  TJVideoController_VideoLifecycleCallbacks = class(TJavaGenericImport<JVideoController_VideoLifecycleCallbacksClass, JVideoController_VideoLifecycleCallbacks>) end;

  JVersionInfoClass = interface(JObjectClass)
    ['{EC9B47C2-55A1-4FBE-B74F-3D4671B0AD57}']
    {class} function init(int: Integer; int_1: Integer; int_2: Integer): JVersionInfo; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/VersionInfo')]
  JVersionInfo = interface(JObject)
    ['{1787A39F-D351-4AE1-BDB8-90E3CCDD21C2}']
    function getMajorVersion: Integer; cdecl;
    function getMicroVersion: Integer; cdecl;
    function getMinorVersion: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJVersionInfo = class(TJavaGenericImport<JVersionInfoClass, JVersionInfo>) end;

  JResponseInfoClass = interface(JObjectClass)
    ['{DFD3167F-A1D2-4D2F-948E-5CF0F279DCE1}']
  end;

  [JavaSignature('com/google/android/gms/ads/ResponseInfo')]
  JResponseInfo = interface(JObject)
    ['{082D5EF2-E441-4492-B071-092818D26B67}']
    function getAdapterResponses: JList; cdecl;
    function getLoadedAdapterResponseInfo: JAdapterResponseInfo; cdecl;
    function getMediationAdapterClassName: JString; cdecl;
    function getResponseExtras: JBundle; cdecl;
    function getResponseId: JString; cdecl;
    function toString: JString; cdecl;
  end;
  TJResponseInfo = class(TJavaGenericImport<JResponseInfoClass, JResponseInfo>) end;

  JRequestConfigurationClass = interface(JObjectClass)
    ['{21EB3243-F2F9-4E78-812B-DCD645521CF9}']
    {class} function _GetMAX_AD_CONTENT_RATING_G: JString; cdecl;
    {class} function _GetMAX_AD_CONTENT_RATING_MA: JString; cdecl;
    {class} function _GetMAX_AD_CONTENT_RATING_PG: JString; cdecl;
    {class} function _GetMAX_AD_CONTENT_RATING_T: JString; cdecl;
    {class} function _GetMAX_AD_CONTENT_RATING_UNSPECIFIED: JString; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer; cdecl;
    {class} function _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer; cdecl;
    {class} function _GetTAG_FOR_UNDER_AGE_OF_CONSENT_FALSE: Integer; cdecl;
    {class} function _GetTAG_FOR_UNDER_AGE_OF_CONSENT_TRUE: Integer; cdecl;
    {class} function _GetTAG_FOR_UNDER_AGE_OF_CONSENT_UNSPECIFIED: Integer; cdecl;
    {class} property MAX_AD_CONTENT_RATING_G: JString read _GetMAX_AD_CONTENT_RATING_G;
    {class} property MAX_AD_CONTENT_RATING_MA: JString read _GetMAX_AD_CONTENT_RATING_MA;
    {class} property MAX_AD_CONTENT_RATING_PG: JString read _GetMAX_AD_CONTENT_RATING_PG;
    {class} property MAX_AD_CONTENT_RATING_T: JString read _GetMAX_AD_CONTENT_RATING_T;
    {class} property MAX_AD_CONTENT_RATING_UNSPECIFIED: JString read _GetMAX_AD_CONTENT_RATING_UNSPECIFIED;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_FALSE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE;
    {class} property TAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED: Integer read _GetTAG_FOR_CHILD_DIRECTED_TREATMENT_UNSPECIFIED;
    {class} property TAG_FOR_UNDER_AGE_OF_CONSENT_FALSE: Integer read _GetTAG_FOR_UNDER_AGE_OF_CONSENT_FALSE;
    {class} property TAG_FOR_UNDER_AGE_OF_CONSENT_TRUE: Integer read _GetTAG_FOR_UNDER_AGE_OF_CONSENT_TRUE;
    {class} property TAG_FOR_UNDER_AGE_OF_CONSENT_UNSPECIFIED: Integer read _GetTAG_FOR_UNDER_AGE_OF_CONSENT_UNSPECIFIED;
  end;

  [JavaSignature('com/google/android/gms/ads/RequestConfiguration')]
  JRequestConfiguration = interface(JObject)
    ['{FF390D05-0461-4A34-9312-A62370BAD21E}']
    function getAgeRestrictedTreatment: JAgeRestrictedTreatment; cdecl;
    function getMaxAdContentRating: JString; cdecl;
    function getPublisherPrivacyPersonalizationState: JRequestConfiguration_PublisherPrivacyPersonalizationState; cdecl;
    function getTagForChildDirectedTreatment: Integer; cdecl;
    function getTagForUnderAgeOfConsent: Integer; cdecl;
    function getTestDeviceIds: JList; cdecl;
    function toBuilder: JRequestConfiguration_Builder; cdecl;
  end;
  TJRequestConfiguration = class(TJavaGenericImport<JRequestConfigurationClass, JRequestConfiguration>) end;

  JRequestConfiguration_TagForUnderAgeOfConsentClass = interface(JAnnotationClass)
    ['{BF53C1CF-A87A-4125-9049-BC45099D9B8B}']
  end;

  [JavaSignature('com/google/android/gms/ads/RequestConfiguration$TagForUnderAgeOfConsent')]
  JRequestConfiguration_TagForUnderAgeOfConsent = interface(JAnnotation)
    ['{5F8EE053-F0DD-4F00-A439-9020CCFBDFE7}']
  end;
  TJRequestConfiguration_TagForUnderAgeOfConsent = class(TJavaGenericImport<JRequestConfiguration_TagForUnderAgeOfConsentClass, JRequestConfiguration_TagForUnderAgeOfConsent>) end;

  JRequestConfiguration_TagForChildDirectedTreatmentClass = interface(JAnnotationClass)
    ['{D22DEDEB-16A7-4BDD-8254-B979B1DA5476}']
  end;

  [JavaSignature('com/google/android/gms/ads/RequestConfiguration$TagForChildDirectedTreatment')]
  JRequestConfiguration_TagForChildDirectedTreatment = interface(JAnnotation)
    ['{B4C6338E-F584-45EE-B3D3-42C9C60E174A}']
  end;
  TJRequestConfiguration_TagForChildDirectedTreatment = class(TJavaGenericImport<JRequestConfiguration_TagForChildDirectedTreatmentClass, JRequestConfiguration_TagForChildDirectedTreatment>) end;

  JRequestConfiguration_PublisherPrivacyPersonalizationStateClass = interface(JEnumClass)
    ['{B526DAC8-F135-4F68-A257-C49D60653DA7}']
    {class} function _GetDEFAULT: JRequestConfiguration_PublisherPrivacyPersonalizationState; cdecl;
    {class} function _GetDISABLED: JRequestConfiguration_PublisherPrivacyPersonalizationState; cdecl;
    {class} function _GetENABLED: JRequestConfiguration_PublisherPrivacyPersonalizationState; cdecl;
    {class} function valueOf(string_1: JString): JRequestConfiguration_PublisherPrivacyPersonalizationState; cdecl;
    {class} function values: TJavaObjectArray<JRequestConfiguration_PublisherPrivacyPersonalizationState>; cdecl;
    {class} property &DEFAULT: JRequestConfiguration_PublisherPrivacyPersonalizationState read _GetDEFAULT;
    {class} property DISABLED: JRequestConfiguration_PublisherPrivacyPersonalizationState read _GetDISABLED;
    {class} property ENABLED: JRequestConfiguration_PublisherPrivacyPersonalizationState read _GetENABLED;
  end;

  [JavaSignature('com/google/android/gms/ads/RequestConfiguration$PublisherPrivacyPersonalizationState')]
  JRequestConfiguration_PublisherPrivacyPersonalizationState = interface(JEnum)
    ['{35D53885-E8E5-44B4-A1E7-1ADBC91B1849}']
    function getValue: Integer; cdecl;
  end;
  TJRequestConfiguration_PublisherPrivacyPersonalizationState = class(TJavaGenericImport<JRequestConfiguration_PublisherPrivacyPersonalizationStateClass, JRequestConfiguration_PublisherPrivacyPersonalizationState>) end;

  JRequestConfiguration_MaxAdContentRatingClass = interface(JAnnotationClass)
    ['{B9BDB9F2-EABF-4902-BDAB-B1F259B04A98}']
  end;

  [JavaSignature('com/google/android/gms/ads/RequestConfiguration$MaxAdContentRating')]
  JRequestConfiguration_MaxAdContentRating = interface(JAnnotation)
    ['{58463FEE-E250-453F-9153-151AA8CF106B}']
  end;
  TJRequestConfiguration_MaxAdContentRating = class(TJavaGenericImport<JRequestConfiguration_MaxAdContentRatingClass, JRequestConfiguration_MaxAdContentRating>) end;

  JRequestConfiguration_BuilderClass = interface(JObjectClass)
    ['{61571D64-A1FE-4199-9BFD-0BAA7C4A2532}']
    {class} function init: JRequestConfiguration_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/RequestConfiguration$Builder')]
  JRequestConfiguration_Builder = interface(JObject)
    ['{910DB83D-428C-4D6E-9FB2-6A87EB085E52}']
    function build: JRequestConfiguration; cdecl;
    function setAgeRestrictedTreatment(ageRestrictedTreatment: JAgeRestrictedTreatment): JRequestConfiguration_Builder; cdecl;
    function setMaxAdContentRating(string_1: JString): JRequestConfiguration_Builder; cdecl;
    function setPublisherPrivacyPersonalizationState(publisherPrivacyPersonalizationState: JRequestConfiguration_PublisherPrivacyPersonalizationState): JRequestConfiguration_Builder; cdecl;
    function setTagForChildDirectedTreatment(int: Integer): JRequestConfiguration_Builder; cdecl;
    function setTagForUnderAgeOfConsent(int: Integer): JRequestConfiguration_Builder; cdecl;
    function setTestDeviceIds(list: JList): JRequestConfiguration_Builder; cdecl;
  end;
  TJRequestConfiguration_Builder = class(TJavaGenericImport<JRequestConfiguration_BuilderClass, JRequestConfiguration_Builder>) end;

  JOutOfContextTestingActivityClass = interface(JActivityClass)
    ['{3A6A8B6E-7973-4927-B9AE-EB4F27EC52FF}']
    {class} function _GetAD_UNIT_KEY: JString; cdecl;
    {class} function _GetCLASS_NAME: JString; cdecl;
    {class} function init: JOutOfContextTestingActivity; cdecl;
    {class} property AD_UNIT_KEY: JString read _GetAD_UNIT_KEY;
    {class} property CLASS_NAME: JString read _GetCLASS_NAME;
  end;

  [JavaSignature('com/google/android/gms/ads/OutOfContextTestingActivity')]
  JOutOfContextTestingActivity = interface(JActivity)
    ['{8E2BE612-8D56-475F-ABE8-B1B8005830F4}']
  end;
  TJOutOfContextTestingActivity = class(TJavaGenericImport<JOutOfContextTestingActivityClass, JOutOfContextTestingActivity>) end;

  JOnUserEarnedRewardListenerClass = interface(IJavaClass)
    ['{F2321D62-C796-4A52-91DA-4E3195F305A6}']
  end;

  [JavaSignature('com/google/android/gms/ads/OnUserEarnedRewardListener')]
  JOnUserEarnedRewardListener = interface(IJavaInstance)
    ['{4F53EA0C-4167-49A5-A324-CEA7DAAF9F82}']
    procedure onUserEarnedReward(rewardItem: JRewardItem); cdecl;
  end;
  TJOnUserEarnedRewardListener = class(TJavaGenericImport<JOnUserEarnedRewardListenerClass, JOnUserEarnedRewardListener>) end;

  JOnPaidEventListenerClass = interface(IJavaClass)
    ['{5FF38291-7B31-4C94-B7B8-A2F51F137169}']
  end;

  [JavaSignature('com/google/android/gms/ads/OnPaidEventListener')]
  JOnPaidEventListener = interface(IJavaInstance)
    ['{AECC0ED3-FEE7-4FD2-A623-0F7AD87D61FB}']
    procedure onPaidEvent(adValue: JAdValue); cdecl;
  end;
  TJOnPaidEventListener = class(TJavaGenericImport<JOnPaidEventListenerClass, JOnPaidEventListener>) end;

  JOnAdInspectorClosedListenerClass = interface(IJavaClass)
    ['{F7480E1D-16D2-4B35-BF87-CBD5D73242F8}']
  end;

  [JavaSignature('com/google/android/gms/ads/OnAdInspectorClosedListener')]
  JOnAdInspectorClosedListener = interface(IJavaInstance)
    ['{87D893DE-AA86-4C7E-A6B7-22429FFD0402}']
    procedure onAdInspectorClosed(adInspectorError: JAdInspectorError); cdecl;
  end;
  TJOnAdInspectorClosedListener = class(TJavaGenericImport<JOnAdInspectorClosedListenerClass, JOnAdInspectorClosedListener>) end;

  JNotificationHandlerActivityClass = interface(JActivityClass)
    ['{5A1010A5-6E55-4F2F-B82C-16316BD99B2E}']
    {class} function _GetCLASS_NAME: JString; cdecl;
    {class} function init: JNotificationHandlerActivity; cdecl;
    {class} property CLASS_NAME: JString read _GetCLASS_NAME;
  end;

  [JavaSignature('com/google/android/gms/ads/NotificationHandlerActivity')]
  JNotificationHandlerActivity = interface(JActivity)
    ['{19FCFFE3-6BDC-4037-8684-4A9611248543}']
  end;
  TJNotificationHandlerActivity = class(TJavaGenericImport<JNotificationHandlerActivityClass, JNotificationHandlerActivity>) end;

  JMuteThisAdReasonClass = interface(IJavaClass)
    ['{3AD25555-C9F4-44D0-8EEE-7199812E4334}']
  end;

  [JavaSignature('com/google/android/gms/ads/MuteThisAdReason')]
  JMuteThisAdReason = interface(IJavaInstance)
    ['{88CACC4B-F456-40A8-B201-FF37EE005EB2}']
    function getDescription: JString; cdecl;
  end;
  TJMuteThisAdReason = class(TJavaGenericImport<JMuteThisAdReasonClass, JMuteThisAdReason>) end;

  JMuteThisAdListenerClass = interface(IJavaClass)
    ['{8308A1D6-89D3-484B-B4B0-8F6A4F1CBCAF}']
  end;

  [JavaSignature('com/google/android/gms/ads/MuteThisAdListener')]
  JMuteThisAdListener = interface(IJavaInstance)
    ['{90B3144A-955D-4B3C-AC25-7B6D68ED8B70}']
    procedure onAdMuted; cdecl;
  end;
  TJMuteThisAdListener = class(TJavaGenericImport<JMuteThisAdListenerClass, JMuteThisAdListener>) end;

  JMobileAdsInitProviderClass = interface(JContentProviderClass)
    ['{4CA71C09-32CC-4A6A-9202-F440469A65E7}']
    {class} function init: JMobileAdsInitProvider; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/MobileAdsInitProvider')]
  JMobileAdsInitProvider = interface(JContentProvider)
    ['{C6C4E2AB-1F3F-4104-A815-83214A5B0026}']
    procedure attachInfo(context: JContext; providerInfo: JProviderInfo); cdecl;
    function delete(uri: Jnet_Uri; string_1: JString; strings: TJavaObjectArray<JString>): Integer; cdecl;
    function getType(uri: Jnet_Uri): JString; cdecl;
    function insert(uri: Jnet_Uri; contentValues: JContentValues): Jnet_Uri; cdecl;
    function onCreate: Boolean; cdecl;
    function query(uri: Jnet_Uri; strings: TJavaObjectArray<JString>; string_1: JString; strings_1: TJavaObjectArray<JString>; string_2: JString): JCursor; cdecl;
    function update(uri: Jnet_Uri; contentValues: JContentValues; string_1: JString; strings: TJavaObjectArray<JString>): Integer; cdecl;
  end;
  TJMobileAdsInitProvider = class(TJavaGenericImport<JMobileAdsInitProviderClass, JMobileAdsInitProvider>) end;

  JMobileAdsClass = interface(JObjectClass)
    ['{1FF419A1-0429-4DF1-BC40-817C10CDA75A}']
    {class} function _GetERROR_DOMAIN: JString; cdecl;
    {class} procedure disableMediationAdapterInitialization(context: JContext); cdecl;
    {class} function getInitializationStatus: JInitializationStatus; cdecl;
    {class} function getRequestConfiguration: JRequestConfiguration; cdecl;
    {class} function getVersion: JVersionInfo; cdecl;
    {class} procedure initialize(context: JContext; onInitializationCompleteListener: JOnInitializationCompleteListener); overload; cdecl;
    {class} procedure initialize(context: JContext); overload; cdecl;
    {class} procedure openAdInspector(context: JContext; onAdInspectorClosedListener: JOnAdInspectorClosedListener); cdecl;
    {class} procedure openDebugMenu(context: JContext; string_1: JString); cdecl;
    {class} function putPublisherFirstPartyIdEnabled(boolean: Boolean): Boolean; cdecl;
    {class} function registerCustomTabsSession(context: JContext; customTabsClient: JCustomTabsClient; string_1: JString; customTabsCallback: JCustomTabsCallback): JCustomTabsSession; cdecl;
//    {class} procedure registerRtbAdapter(class_1: JClass); cdecl;
    {class} procedure registerWebView(webView: JWebView); cdecl;
    {class} procedure setAppMuted(boolean: Boolean); cdecl;
    {class} procedure setAppVolume(float: Single); cdecl;
    {class} procedure setRequestConfiguration(requestConfiguration: JRequestConfiguration); cdecl;
    {class} procedure startPreload(context: JContext; list: JList; preloadCallback: JPreloadCallback); cdecl;
    {class} property ERROR_DOMAIN: JString read _GetERROR_DOMAIN;
  end;

  [JavaSignature('com/google/android/gms/ads/MobileAds')]
  JMobileAds = interface(JObject)
    ['{1D12BBB1-205E-4D26-A2DF-3673C36D35DF}']
  end;
  TJMobileAds = class(TJavaGenericImport<JMobileAdsClass, JMobileAds>) end;

  JMediationUtilsClass = interface(JObjectClass)
    ['{9713A185-B7EF-431D-BA3F-DADD3947C64B}']
    {class} function findClosestSize(context: JContext; adSize: JAdSize; list: JList): JAdSize; cdecl;
    {class} function init: JMediationUtils; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/MediationUtils')]
  JMediationUtils = interface(JObject)
    ['{AB788997-5FB6-46C9-87FE-7C11BB7A7675}']
  end;
  TJMediationUtils = class(TJavaGenericImport<JMediationUtilsClass, JMediationUtils>) end;

  JMediaContentClass = interface(IJavaClass)
    ['{E6E5DB43-BDCD-4862-B17F-BF435319123B}']
  end;

  [JavaSignature('com/google/android/gms/ads/MediaContent')]
  JMediaContent = interface(IJavaInstance)
    ['{C1B8F500-FF85-4793-9A74-DF9BD6C8B0D7}']
    function getAspectRatio: Single; cdecl;
    function getCurrentTime: Single; cdecl;
    function getDuration: Single; cdecl;
    function getMainImage: JDrawable; cdecl;
    function getVideoController: JVideoController; cdecl;
    function hasVideoContent: Boolean; cdecl;
    procedure setMainImage(drawable: JDrawable); cdecl;
  end;
  TJMediaContent = class(TJavaGenericImport<JMediaContentClass, JMediaContent>) end;

  JMediaAspectRatioClass = interface(JAnnotationClass)
    ['{F12DE99A-A978-4261-8E3A-A778F3528B53}']
    {class} function _GetANY: Integer; cdecl;
    {class} function _GetLANDSCAPE: Integer; cdecl;
    {class} function _GetPORTRAIT: Integer; cdecl;
    {class} function _GetSQUARE: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} property ANY: Integer read _GetANY;
    {class} property LANDSCAPE: Integer read _GetLANDSCAPE;
    {class} property PORTRAIT: Integer read _GetPORTRAIT;
    {class} property SQUARE: Integer read _GetSQUARE;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
  end;

  [JavaSignature('com/google/android/gms/ads/MediaAspectRatio')]
  JMediaAspectRatio = interface(JAnnotation)
    ['{645EE788-3981-49D1-9318-F923D18AECC2}']
  end;
  TJMediaAspectRatio = class(TJavaGenericImport<JMediaAspectRatioClass, JMediaAspectRatio>) end;

  JFullScreenContentCallbackClass = interface(JObjectClass)
    ['{F6FE0B6E-DCBA-4B85-87AE-D3D4532F1530}']
    {class} function _GetERROR_CODE_AD_REUSED: Integer; cdecl;
    {class} function _GetERROR_CODE_APP_NOT_FOREGROUND: Integer; cdecl;
    {class} function _GetERROR_CODE_INTERNAL_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_MEDIATION_SHOW_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_NOT_READY: Integer; cdecl;
    {class} function init: JFullScreenContentCallback; cdecl;
    {class} property ERROR_CODE_AD_REUSED: Integer read _GetERROR_CODE_AD_REUSED;
    {class} property ERROR_CODE_APP_NOT_FOREGROUND: Integer read _GetERROR_CODE_APP_NOT_FOREGROUND;
    {class} property ERROR_CODE_INTERNAL_ERROR: Integer read _GetERROR_CODE_INTERNAL_ERROR;
    {class} property ERROR_CODE_MEDIATION_SHOW_ERROR: Integer read _GetERROR_CODE_MEDIATION_SHOW_ERROR;
    {class} property ERROR_CODE_NOT_READY: Integer read _GetERROR_CODE_NOT_READY;
  end;

  [JavaSignature('com/google/android/gms/ads/FullScreenContentCallback')]
  JFullScreenContentCallback = interface(JObject)
    ['{B068831C-603A-4540-9DC4-080095D72263}']
    procedure onAdClicked; cdecl;
    procedure onAdDismissedFullScreenContent; cdecl;
    procedure onAdFailedToShowFullScreenContent(adError: JAdError); cdecl;
    procedure onAdImpression; cdecl;
    procedure onAdShowedFullScreenContent; cdecl;
  end;
  TJFullScreenContentCallback = class(TJavaGenericImport<JFullScreenContentCallbackClass, JFullScreenContentCallback>) end;

  JContextualSignalsClass = interface(JObjectClass)
    ['{E6B015DB-887E-46DB-AE2A-5EAF781C9B22}']
  end;

  [JavaSignature('com/google/android/gms/ads/ContextualSignals')]
  JContextualSignals = interface(JObject)
    ['{73334742-0B7D-4F62-ABFC-A5FAF46E6754}']
  end;
  TJContextualSignals = class(TJavaGenericImport<JContextualSignalsClass, JContextualSignals>) end;

  JBaseAdViewClass = interface(JViewGroupClass)
    ['{49EDCDB0-BA99-44E5-914A-398CD64723F8}']
  end;

  [JavaSignature('com/google/android/gms/ads/BaseAdView')]
  JBaseAdView = interface(JViewGroup)
    ['{079103C0-3282-49EE-8884-5BA996B205EE}']
    procedure destroy; cdecl;
    function getAdListener: JAdListener; cdecl;
    function getAdSize: JAdSize; cdecl;
    function getAdUnitId: JString; cdecl;
    function getOnPaidEventListener: JOnPaidEventListener; cdecl;
    function getPlacementId: Int64; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    function isCollapsible: Boolean; cdecl;
    function isLoading: Boolean; cdecl;
    procedure loadAd(adRequest: JAdRequest); cdecl;
    procedure pause; cdecl;
    procedure resume; cdecl;
    procedure setAdListener(adListener: JAdListener); cdecl;
    procedure setAdSize(adSize: JAdSize); cdecl;
    procedure setAdUnitId(string_1: JString); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setPlacementId(long: Int64); cdecl;
  end;
  TJBaseAdView = class(TJavaGenericImport<JBaseAdViewClass, JBaseAdView>) end;

  JAgeRestrictedTreatmentClass = interface(JEnumClass)
    ['{EBBA4046-A7B7-481D-81CA-1C5BFF23B1EC}']
    {class} function _GetCHILD: JAgeRestrictedTreatment; cdecl;
    {class} function _GetTEEN: JAgeRestrictedTreatment; cdecl;
    {class} function _GetUNSPECIFIED: JAgeRestrictedTreatment; cdecl;
    {class} function valueOf(string_1: JString): JAgeRestrictedTreatment; cdecl;
    {class} function values: TJavaObjectArray<JAgeRestrictedTreatment>; cdecl;
    {class} property CHILD: JAgeRestrictedTreatment read _GetCHILD;
    {class} property TEEN: JAgeRestrictedTreatment read _GetTEEN;
    {class} property UNSPECIFIED: JAgeRestrictedTreatment read _GetUNSPECIFIED;
  end;

  [JavaSignature('com/google/android/gms/ads/AgeRestrictedTreatment')]
  JAgeRestrictedTreatment = interface(JEnum)
    ['{19BF3E88-40D6-4465-849F-3CE3486FA493}']
    function getValue: Integer; cdecl;
  end;
  TJAgeRestrictedTreatment = class(TJavaGenericImport<JAgeRestrictedTreatmentClass, JAgeRestrictedTreatment>) end;

  JAdapterResponseInfoClass = interface(JObjectClass)
    ['{FBACADBC-367C-4042-BA22-479283E0FEDD}']
  end;

  [JavaSignature('com/google/android/gms/ads/AdapterResponseInfo')]
  JAdapterResponseInfo = interface(JObject)
    ['{C45CD324-7447-483C-AD49-538156724AB8}']
    function getAdError: JAdError; cdecl;
    function getAdSourceId: JString; cdecl;
    function getAdSourceInstanceId: JString; cdecl;
    function getAdSourceInstanceName: JString; cdecl;
    function getAdSourceName: JString; cdecl;
    function getAdapterClassName: JString; cdecl;
    function getCredentials: JBundle; cdecl;
    function getLatencyMillis: Int64; cdecl;
    function toString: JString; cdecl;
  end;
  TJAdapterResponseInfo = class(TJavaGenericImport<JAdapterResponseInfoClass, JAdapterResponseInfo>) end;

  JAdViewClass = interface(JBaseAdViewClass)
    ['{1B6D3BA0-CD11-4EF7-A0E7-59E2091ED115}']
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer): JAdView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JAdView; overload; cdecl;
    {class} function init(context: JContext): JAdView; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdView')]
  JAdView = interface(JBaseAdView)
    ['{5261DE18-6643-4BC6-9E2F-C512CA8E6FE7}']
  end;
  TJAdView = class(TJavaGenericImport<JAdViewClass, JAdView>) end;

  JAdValueClass = interface(JObjectClass)
    ['{3A2AADD5-346C-41B3-8FE6-F2064908D052}']
  end;

  [JavaSignature('com/google/android/gms/ads/AdValue')]
  JAdValue = interface(JObject)
    ['{BBF56993-5ACC-48D4-B575-FE7FA3DC198C}']
    function getCurrencyCode: JString; cdecl;
    function getPrecisionType: Integer; cdecl;
    function getValueMicros: Int64; cdecl;
  end;
  TJAdValue = class(TJavaGenericImport<JAdValueClass, JAdValue>) end;

  JAdValue_PrecisionTypeClass = interface(JAnnotationClass)
    ['{7C3C7C21-3EE3-4B30-83EB-AE148149A3DB}']
    {class} function _GetESTIMATED: Integer; cdecl;
    {class} function _GetPRECISE: Integer; cdecl;
    {class} function _GetPUBLISHER_PROVIDED: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} property ESTIMATED: Integer read _GetESTIMATED;
    {class} property PRECISE: Integer read _GetPRECISE;
    {class} property PUBLISHER_PROVIDED: Integer read _GetPUBLISHER_PROVIDED;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
  end;

  [JavaSignature('com/google/android/gms/ads/AdValue$PrecisionType')]
  JAdValue_PrecisionType = interface(JAnnotation)
    ['{EBCD4278-EDC6-4474-A860-3E4FBF1D4106}']
  end;
  TJAdValue_PrecisionType = class(TJavaGenericImport<JAdValue_PrecisionTypeClass, JAdValue_PrecisionType>) end;

  JAdSizeClass = interface(JObjectClass)
    ['{6B77F7C2-4F94-4151-8F59-550931494F1A}']
    {class} function _GetAUTO_HEIGHT: Integer; cdecl;
    {class} function _GetBANNER: JAdSize; cdecl;
    {class} function _GetFLUID: JAdSize; cdecl;
    {class} function _GetFULL_BANNER: JAdSize; cdecl;
    {class} function _GetFULL_WIDTH: Integer; cdecl;
    {class} function _GetINVALID: JAdSize; cdecl;
    {class} function _GetLARGE_BANNER: JAdSize; cdecl;
    {class} function _GetLEADERBOARD: JAdSize; cdecl;
    {class} function _GetMEDIUM_RECTANGLE: JAdSize; cdecl;
    {class} function _GetSMART_BANNER: JAdSize; cdecl;
    {class} function _GetWIDE_SKYSCRAPER: JAdSize; cdecl;
    {class} function getCurrentOrientationAnchoredAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function getCurrentOrientationInlineAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function getInlineAdaptiveBannerAdSize(int: Integer; int_1: Integer): JAdSize; cdecl;
    {class} function getLandscapeAnchoredAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function getLandscapeInlineAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function getLargeAnchoredAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function getLargeLandscapeAnchoredAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function getLargePortraitAnchoredAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function getPortraitAnchoredAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function getPortraitInlineAdaptiveBannerAdSize(context: JContext; int: Integer): JAdSize; cdecl;
    {class} function init(int: Integer; int_1: Integer): JAdSize; cdecl;
    {class} property AUTO_HEIGHT: Integer read _GetAUTO_HEIGHT;
    {class} property BANNER: JAdSize read _GetBANNER;
    {class} property FLUID: JAdSize read _GetFLUID;
    {class} property FULL_BANNER: JAdSize read _GetFULL_BANNER;
    {class} property FULL_WIDTH: Integer read _GetFULL_WIDTH;
    {class} property INVALID: JAdSize read _GetINVALID;
    {class} property LARGE_BANNER: JAdSize read _GetLARGE_BANNER;
    {class} property LEADERBOARD: JAdSize read _GetLEADERBOARD;
    {class} property MEDIUM_RECTANGLE: JAdSize read _GetMEDIUM_RECTANGLE;
    {class} property SMART_BANNER: JAdSize read _GetSMART_BANNER;
    {class} property WIDE_SKYSCRAPER: JAdSize read _GetWIDE_SKYSCRAPER;
  end;

  [JavaSignature('com/google/android/gms/ads/AdSize')]
  JAdSize = interface(JObject)
    ['{0941660C-1CC2-48ED-8CBC-EA3A4E988B7A}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getHeight: Integer; cdecl;
    function getHeightInPixels(context: JContext): Integer; cdecl;
    function getWidth: Integer; cdecl;
    function getWidthInPixels(context: JContext): Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isAutoHeight: Boolean; cdecl;
    function isFluid: Boolean; cdecl;
    function isFullWidth: Boolean; cdecl;
    function toString: JString; cdecl;
  end;
  TJAdSize = class(TJavaGenericImport<JAdSizeClass, JAdSize>) end;

  JAdServiceClass = interface(JIntentServiceClass)
    ['{E8F75085-0E67-41C3-B435-65CE5951C161}']
    {class} function _GetCLASS_NAME: JString; cdecl;
    {class} function init: JAdService; cdecl;
    {class} property CLASS_NAME: JString read _GetCLASS_NAME;
  end;

  [JavaSignature('com/google/android/gms/ads/AdService')]
  JAdService = interface(JIntentService)
    ['{521897AB-61BD-423F-842A-F8696B7D4966}']
  end;
  TJAdService = class(TJavaGenericImport<JAdServiceClass, JAdService>) end;

  JAdRequestClass = interface(JObjectClass)
    ['{2FB23223-E6F6-41AA-8DA5-4257F7255549}']
    {class} function _GetDEVICE_ID_EMULATOR: JString; cdecl;
    {class} function _GetERROR_CODE_APP_ID_MISSING: Integer; cdecl;
    {class} function _GetERROR_CODE_INTERNAL_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_INVALID_AD_STRING: Integer; cdecl;
    {class} function _GetERROR_CODE_INVALID_REQUEST: Integer; cdecl;
    {class} function _GetERROR_CODE_MEDIATION_NO_FILL: Integer; cdecl;
    {class} function _GetERROR_CODE_NETWORK_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_NO_FILL: Integer; cdecl;
    {class} function _GetERROR_CODE_REQUEST_ID_MISMATCH: Integer; cdecl;
    {class} function _GetMAX_CONTENT_URL_LENGTH: Integer; cdecl;
    {class} property DEVICE_ID_EMULATOR: JString read _GetDEVICE_ID_EMULATOR;
    {class} property ERROR_CODE_APP_ID_MISSING: Integer read _GetERROR_CODE_APP_ID_MISSING;
    {class} property ERROR_CODE_INTERNAL_ERROR: Integer read _GetERROR_CODE_INTERNAL_ERROR;
    {class} property ERROR_CODE_INVALID_AD_STRING: Integer read _GetERROR_CODE_INVALID_AD_STRING;
    {class} property ERROR_CODE_INVALID_REQUEST: Integer read _GetERROR_CODE_INVALID_REQUEST;
    {class} property ERROR_CODE_MEDIATION_NO_FILL: Integer read _GetERROR_CODE_MEDIATION_NO_FILL;
    {class} property ERROR_CODE_NETWORK_ERROR: Integer read _GetERROR_CODE_NETWORK_ERROR;
    {class} property ERROR_CODE_NO_FILL: Integer read _GetERROR_CODE_NO_FILL;
    {class} property ERROR_CODE_REQUEST_ID_MISMATCH: Integer read _GetERROR_CODE_REQUEST_ID_MISMATCH;
    {class} property MAX_CONTENT_URL_LENGTH: Integer read _GetMAX_CONTENT_URL_LENGTH;
  end;

  [JavaSignature('com/google/android/gms/ads/AdRequest')]
  JAdRequest = interface(JObject)
    ['{FC8FFCCA-D18A-4662-AD60-22D0A1C99AF8}']
    function getAdString: JString; cdecl;
    function getContentUrl: JString; cdecl;
    function getCustomEventExtrasBundle(class_1: Jlang_Class): JBundle; cdecl;
    function getCustomTargeting: JBundle; cdecl;
    function getKeywords: JSet; cdecl;
    function getNeighboringContentUrls: JList; cdecl;
    function getNetworkExtrasBundle(class_1: Jlang_Class): JBundle; cdecl;
    function getPlacementId: Int64; cdecl;
    function getRequestAgent: JString; cdecl;
    function isTestDevice(context: JContext): Boolean; cdecl;
  end;
  TJAdRequest = class(TJavaGenericImport<JAdRequestClass, JAdRequest>) end;

  JAdLoaderClass = interface(JObjectClass)
    ['{3EB7BD10-FD98-4043-82AD-AACC37B20E6A}']
  end;

  [JavaSignature('com/google/android/gms/ads/AdLoader')]
  JAdLoader = interface(JObject)
    ['{EA7DBA0E-807E-4859-8448-09F2E432F749}']
    function isLoading: Boolean; cdecl;
    procedure loadAd(adManagerAdRequest: JAdManagerAdRequest); overload; cdecl;
    procedure loadAd(adRequest: JAdRequest); overload; cdecl;
    procedure loadAds(adRequest: JAdRequest; int: Integer); cdecl;
  end;
  TJAdLoader = class(TJavaGenericImport<JAdLoaderClass, JAdLoader>) end;

  JAdLoader_BuilderClass = interface(JObjectClass)
    ['{A3978405-CAE1-4ADA-B6B7-4D148B154E9A}']
    {class} function init(context: JContext; string_1: JString): JAdLoader_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdLoader$Builder')]
  JAdLoader_Builder = interface(JObject)
    ['{D47DAA42-2D57-4585-93DF-D31FC3188A58}']
    function build: JAdLoader; cdecl;
    function forAdManagerAdView(onAdManagerAdViewLoadedListener: JOnAdManagerAdViewLoadedListener; adSizes: TJavaObjectArray<JAdSize>): JAdLoader_Builder; cdecl;
    function forCustomFormatAd(string_1: JString; onCustomFormatAdLoadedListener: JNativeCustomFormatAd_OnCustomFormatAdLoadedListener; onCustomClickListener: JNativeCustomFormatAd_OnCustomClickListener): JAdLoader_Builder; cdecl;
    function forNativeAd(onNativeAdLoadedListener: JNativeAd_OnNativeAdLoadedListener): JAdLoader_Builder; cdecl;
    function withAdListener(adListener: JAdListener): JAdLoader_Builder; cdecl;
    function withAdManagerAdViewOptions(adManagerAdViewOptions: JAdManagerAdViewOptions): JAdLoader_Builder; cdecl;
    function withNativeAdOptions(nativeAdOptions: JNativeAdOptions): JAdLoader_Builder; cdecl;
  end;
  TJAdLoader_Builder = class(TJavaGenericImport<JAdLoader_BuilderClass, JAdLoader_Builder>) end;

  JAdLoadCallbackClass = interface(JObjectClass)
    ['{478C8B93-A459-4D54-BD3C-A5C33BB772D9}']
    {class} function init: JAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdLoadCallback')]
  JAdLoadCallback = interface(JObject)
    ['{967660F6-F457-4082-B254-23F7A7F3B301}']
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdLoaded(adT: JObject); cdecl;
  end;
  TJAdLoadCallback = class(TJavaGenericImport<JAdLoadCallbackClass, JAdLoadCallback>) end;

  JAdListenerClass = interface(JObjectClass)
    ['{A936E07B-AF13-46D5-900C-6630B44947D2}']
    {class} function init: JAdListener; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdListener')]
  JAdListener = interface(JObject)
    ['{F08C6B6D-14F6-4AB3-A9C5-9A71C55EABDE}']
    procedure onAdClicked; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(loadAdError: JLoadAdError); cdecl;
    procedure onAdImpression; cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdOpened; cdecl;
    procedure onAdSwipeGestureClicked; cdecl;
  end;
  TJAdListener = class(TJavaGenericImport<JAdListenerClass, JAdListener>) end;

  JAdInspectorError_AdInspectorErrorCodeClass = interface(JAnnotationClass)
    ['{9600E401-FD14-4A0F-94C0-8703BE31AABF}']
  end;

  [JavaSignature('com/google/android/gms/ads/AdInspectorError$AdInspectorErrorCode')]
  JAdInspectorError_AdInspectorErrorCode = interface(JAnnotation)
    ['{7D998440-40EC-4910-90B1-B508407ACFC3}']
  end;
  TJAdInspectorError_AdInspectorErrorCode = class(TJavaGenericImport<JAdInspectorError_AdInspectorErrorCodeClass, JAdInspectorError_AdInspectorErrorCode>) end;

  JAdFormatClass = interface(JEnumClass)
    ['{9752BB1D-DDC4-48BC-B5C4-8FFE0DB6700F}']
    {class} function _GetAPP_OPEN_AD: JAdFormat; cdecl;
    {class} function _GetBANNER: JAdFormat; cdecl;
    {class} function _GetINTERSTITIAL: JAdFormat; cdecl;
    {class} function _GetNATIVE: JAdFormat; cdecl;
    {class} function _GetREWARDED: JAdFormat; cdecl;
    {class} function _GetREWARDED_INTERSTITIAL: JAdFormat; cdecl;
    {class} function getAdFormat(int: Integer): JAdFormat; cdecl;
    {class} function valueOf(string_1: JString): JAdFormat; cdecl;
    {class} function values: TJavaObjectArray<JAdFormat>; cdecl;
    {class} property APP_OPEN_AD: JAdFormat read _GetAPP_OPEN_AD;
    {class} property BANNER: JAdFormat read _GetBANNER;
    {class} property INTERSTITIAL: JAdFormat read _GetINTERSTITIAL;
    {class} property NATIVE: JAdFormat read _GetNATIVE;
    {class} property REWARDED: JAdFormat read _GetREWARDED;
    {class} property REWARDED_INTERSTITIAL: JAdFormat read _GetREWARDED_INTERSTITIAL;
  end;

  [JavaSignature('com/google/android/gms/ads/AdFormat')]
  JAdFormat = interface(JEnum)
    ['{35330211-9361-432D-BC42-392D34BD0F9A}']
    function getValue: Integer; cdecl;
  end;
  TJAdFormat = class(TJavaGenericImport<JAdFormatClass, JAdFormat>) end;

  JAdErrorClass = interface(JObjectClass)
    ['{61F66047-3C49-4F7A-A151-9D755944C528}']
    {class} function _GetUNDEFINED_DOMAIN: JString; cdecl;
    {class} function init(int: Integer; string_1: JString; string_2: JString; adError: JAdError): JAdError; overload; cdecl;
    {class} function init(int: Integer; string_1: JString; string_2: JString): JAdError; overload; cdecl;
    {class} property UNDEFINED_DOMAIN: JString read _GetUNDEFINED_DOMAIN;
  end;

  [JavaSignature('com/google/android/gms/ads/AdError')]
  JAdError = interface(JObject)
    ['{9D789CF6-701A-4A5D-9E57-494CEFD28557}']
    function getCause: JAdError; cdecl;
    function getCode: Integer; cdecl;
    function getDomain: JString; cdecl;
    function getMessage: JString; cdecl;
    function toString: JString; cdecl;
  end;
  TJAdError = class(TJavaGenericImport<JAdErrorClass, JAdError>) end;

  JAdActivityClass = interface(JActivityClass)
    ['{534A0FFC-F68D-4133-8CFB-175C3DFDFE60}']
    {class} function _GetCLASS_NAME: JString; cdecl;
    {class} function init: JAdActivity; cdecl;
    {class} property CLASS_NAME: JString read _GetCLASS_NAME;
  end;

  [JavaSignature('com/google/android/gms/ads/AdActivity')]
  JAdActivity = interface(JActivity)
    ['{1D8398B7-9F0C-4014-93B3-2E292933F40F}']
    procedure onBackPressed; cdecl;
    procedure onConfigurationChanged(configuration: JConfiguration); cdecl;
    procedure onRequestPermissionsResult(int: Integer; strings: TJavaObjectArray<JString>; ints: TJavaArray<Integer>); cdecl;
    procedure setContentView(int: Integer); overload; cdecl;
    procedure setContentView(view: JView); overload; cdecl;
    procedure setContentView(view: JView; layoutParams: JViewGroup_LayoutParams); overload; cdecl;
  end;
  TJAdActivity = class(TJavaGenericImport<JAdActivityClass, JAdActivity>) end;

  JAbstractAdRequestBuilderClass = interface(JObjectClass)
    ['{07C1EEC6-762C-4C75-BBE2-A8961350F072}']
  end;

  [JavaSignature('com/google/android/gms/ads/AbstractAdRequestBuilder')]
  JAbstractAdRequestBuilder = interface(JObject)
    ['{0B807677-D299-486F-9FD9-CD9E50BC899E}']
    function addCustomEventExtrasBundle(class_1: Jlang_Class; bundle: JBundle): JObject; cdecl;
    function addCustomTargeting(string_1: JString; string_2: JString): JObject; overload; cdecl;
    function addCustomTargeting(string_1: JString; list: JList): JObject; overload; cdecl;
    function addKeyword(string_1: JString): JObject; cdecl;
    function addNetworkExtrasBundle(class_1: Jlang_Class; bundle: JBundle): JObject; cdecl;
    function setAdString(string_1: JString): JObject; cdecl;
    function setContentUrl(string_1: JString): JObject; cdecl;
    function setHttpTimeoutMillis(int: Integer): JObject; cdecl;
    function setNeighboringContentUrls(list: JList): JObject; cdecl;
    function setPlacementId(long: Int64): JObject; cdecl;
    function setRequestAgent(string_1: JString): JObject; cdecl;
  end;
  TJAbstractAdRequestBuilder = class(TJavaGenericImport<JAbstractAdRequestBuilderClass, JAbstractAdRequestBuilder>) end;

  JAbstractAdViewAdapterClass = interface(JObjectClass)
    ['{EC7BE558-EDAC-49F8-83D8-723DB7AD2F46}']
    {class} function _GetAD_UNIT_ID_PARAMETER: JString; cdecl;
    {class} function init: JAbstractAdViewAdapter; cdecl;
    {class} property AD_UNIT_ID_PARAMETER: JString read _GetAD_UNIT_ID_PARAMETER;
  end;

  [JavaSignature('com/google/ads/mediation/AbstractAdViewAdapter')]
  JAbstractAdViewAdapter = interface(JObject)
    ['{4D29BC96-3A62-4BEC-9B1C-43E62BBC5C73}']
    function getAdUnitId(bundle: JBundle): JString; cdecl;
    function getBannerView: JView; cdecl;
    procedure onDestroy; cdecl;
    procedure onImmersiveModeUpdated(boolean: Boolean); cdecl;
    procedure onPause; cdecl;
    procedure onResume; cdecl;
    procedure requestBannerAd(context: JContext; mediationBannerListener: Jmediation_MediationBannerListener; bundle: JBundle; adSize: JAdSize; mediationAdRequest: Jmediation_MediationAdRequest; bundle_1: JBundle); cdecl;
    procedure requestInterstitialAd(context: JContext; mediationInterstitialListener: Jmediation_MediationInterstitialListener; bundle: JBundle; mediationAdRequest: Jmediation_MediationAdRequest; bundle_1: JBundle); cdecl;
    procedure requestNativeAd(context: JContext; mediationNativeListener: JMediationNativeListener; bundle: JBundle; nativeMediationAdRequest: JNativeMediationAdRequest; bundle_1: JBundle); cdecl;
    procedure showInterstitial; cdecl;
  end;
  TJAbstractAdViewAdapter = class(TJavaGenericImport<JAbstractAdViewAdapterClass, JAbstractAdViewAdapter>) end;

  JRewardedInterstitialAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{17DC665B-93BE-4465-98E7-E67F5A8712CE}']
    {class} function init: JRewardedInterstitialAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewardedinterstitial/RewardedInterstitialAdLoadCallback')]
  JRewardedInterstitialAdLoadCallback = interface(JAdLoadCallback)
    ['{609D41E6-2D10-4AC4-8CD5-807A9DB5E33C}']
  end;
  TJRewardedInterstitialAdLoadCallback = class(TJavaGenericImport<JRewardedInterstitialAdLoadCallbackClass, JRewardedInterstitialAdLoadCallback>) end;

  JRewardedAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{C946370E-A5DA-4463-A001-6D9C475B3AB0}']
    {class} function init: JRewardedAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/rewarded/RewardedAdLoadCallback')]
  JRewardedAdLoadCallback = interface(JAdLoadCallback)
    ['{655B1F0F-D0BA-4429-8D38-43D3FA08E1BE}']
  end;
  TJRewardedAdLoadCallback = class(TJavaGenericImport<JRewardedAdLoadCallbackClass, JRewardedAdLoadCallback>) end;

  JRtbAdapterClass = interface(JAdapterClass)
    ['{738AD6BC-70E2-41D9-8508-047E89D98B20}']
    {class} function init: JRtbAdapter; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/rtb/RtbAdapter')]
  JRtbAdapter = interface(JAdapter)
    ['{5919FDFA-FDB5-4719-AAD2-94A4F176B123}']
    procedure collectSignals(rtbSignalData: JRtbSignalData; signalCallbacks: JSignalCallbacks); cdecl;
    procedure loadRtbAppOpenAd(mediationAppOpenAdConfiguration: JMediationAppOpenAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadRtbBannerAd(mediationBannerAdConfiguration: JMediationBannerAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadRtbInterstitialAd(mediationInterstitialAdConfiguration: JMediationInterstitialAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadRtbNativeAd(mediationNativeAdConfiguration: JMediationNativeAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadRtbNativeAdMapper(mediationNativeAdConfiguration: JMediationNativeAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadRtbRewardedAd(mediationRewardedAdConfiguration: JMediationRewardedAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
    procedure loadRtbRewardedInterstitialAd(mediationRewardedAdConfiguration: JMediationRewardedAdConfiguration; mediationAdLoadCallback: JMediationAdLoadCallback); cdecl;
  end;
  TJRtbAdapter = class(TJavaGenericImport<JRtbAdapterClass, JRtbAdapter>) end;

  JCustomEventNativeListenerClass = interface(JCustomEventListenerClass)
    ['{3A6C7E11-CC48-493E-AC1C-B8403CCD7925}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventNativeListener')]
  JCustomEventNativeListener = interface(JCustomEventListener)
    ['{3A1A459E-20B4-419A-9403-1B32D2399511}']
    procedure onAdImpression; cdecl;
    procedure onAdLoaded(unifiedNativeAdMapper: JUnifiedNativeAdMapper); cdecl;
  end;
  TJCustomEventNativeListener = class(TJavaGenericImport<JCustomEventNativeListenerClass, JCustomEventNativeListener>) end;

  JCustomEventNativeClass = interface(JCustomEventClass)
    ['{11314B63-FA51-4572-98B0-A5F1862ED04C}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventNative')]
  JCustomEventNative = interface(JCustomEvent)
    ['{A17F80DF-D0E7-4B4D-BF03-6188FC2C7635}']
    procedure requestNativeAd(context: JContext; customEventNativeListener: JCustomEventNativeListener; string_1: JString; nativeMediationAdRequest: JNativeMediationAdRequest; bundle: JBundle); cdecl;
  end;
  TJCustomEventNative = class(TJavaGenericImport<JCustomEventNativeClass, JCustomEventNative>) end;

  JCustomEventInterstitialClass = interface(JCustomEventClass)
    ['{1BFFC514-4132-4936-B435-5D32D937C17B}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventInterstitial')]
  JCustomEventInterstitial = interface(JCustomEvent)
    ['{06A0DD14-232A-43D2-88D6-30929A7B596A}']
    procedure requestInterstitialAd(context: JContext; customEventInterstitialListener: JCustomEventInterstitialListener; string_1: JString; mediationAdRequest: Jmediation_MediationAdRequest; bundle: JBundle); cdecl;
    procedure showInterstitial; cdecl;
  end;
  TJCustomEventInterstitial = class(TJavaGenericImport<JCustomEventInterstitialClass, JCustomEventInterstitial>) end;

  JCustomEventBannerClass = interface(JCustomEventClass)
    ['{59E817DE-54BA-4E9B-8DC2-7742B89EFF52}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/customevent/CustomEventBanner')]
  JCustomEventBanner = interface(JCustomEvent)
    ['{F58F3CB7-C023-4D06-B79A-FC506335B327}']
    procedure requestBannerAd(context: JContext; customEventBannerListener: JCustomEventBannerListener; string_1: JString; adSize: JAdSize; mediationAdRequest: Jmediation_MediationAdRequest; bundle: JBundle); cdecl;
  end;
  TJCustomEventBanner = class(TJavaGenericImport<JCustomEventBannerClass, JCustomEventBanner>) end;

  JNativeMediationAdRequestClass = interface(Jmediation_MediationAdRequestClass)
    ['{5D6550A8-6441-47A2-98B7-2F193CE7797C}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/NativeMediationAdRequest')]
  JNativeMediationAdRequest = interface(Jmediation_MediationAdRequest)
    ['{9D314B7D-67D7-4986-A032-672E9DA5E03B}']
    function getAdVolume: Single; cdecl;
    function getNativeAdOptions: JNativeAdOptions; cdecl;
    function getNativeAdRequestOptions: Jnativead_NativeAdOptions; cdecl;
    function isAdMuted: Boolean; cdecl;
    function isUnifiedNativeAdRequested: Boolean; cdecl;
  end;
  TJNativeMediationAdRequest = class(TJavaGenericImport<JNativeMediationAdRequestClass, JNativeMediationAdRequest>) end;

  JMediationRewardedAdConfigurationClass = interface(JMediationAdConfigurationClass)
    ['{96C6E6F3-878B-4F44-A460-5290298AC727}']
    {class} function init(context: JContext; string_1: JString; bundle: JBundle; bundle_1: JBundle; boolean: Boolean; location_1: JLocation; int: Integer; int_1: Integer; string_2: JString; string_3: JString): JMediationRewardedAdConfiguration; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationRewardedAdConfiguration')]
  JMediationRewardedAdConfiguration = interface(JMediationAdConfiguration)
    ['{F0821546-3B61-4E31-9653-A07F1F4067A0}']
  end;
  TJMediationRewardedAdConfiguration = class(TJavaGenericImport<JMediationRewardedAdConfigurationClass, JMediationRewardedAdConfiguration>) end;

  JMediationRewardedAdCallbackClass = interface(JMediationAdCallbackClass)
    ['{AFB9D4E0-47E9-426A-B719-BE3E67A08619}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationRewardedAdCallback')]
  JMediationRewardedAdCallback = interface(JMediationAdCallback)
    ['{67CE3C9B-897D-4028-B154-A7EAF7D07FAA}']
    procedure onAdFailedToShow(adError: JAdError); cdecl;
    procedure onUserEarnedReward; overload; cdecl;
    procedure onUserEarnedReward(rewardItem: JRewardItem); overload; cdecl;
    procedure onVideoComplete; cdecl;
    procedure onVideoStart; cdecl;
  end;
  TJMediationRewardedAdCallback = class(TJavaGenericImport<JMediationRewardedAdCallbackClass, JMediationRewardedAdCallback>) end;

  JMediationNativeAdapterClass = interface(Jmediation_MediationAdapterClass)
    ['{A6EE6703-FF0D-42A5-9CD8-D25619DCFAA3}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationNativeAdapter')]
  JMediationNativeAdapter = interface(Jmediation_MediationAdapter)
    ['{6CDA4929-3D38-4A63-ADCD-8B7DECA63977}']
    procedure requestNativeAd(context: JContext; mediationNativeListener: JMediationNativeListener; bundle: JBundle; nativeMediationAdRequest: JNativeMediationAdRequest; bundle_1: JBundle); cdecl;
  end;
  TJMediationNativeAdapter = class(TJavaGenericImport<JMediationNativeAdapterClass, JMediationNativeAdapter>) end;

  JMediationNativeAdConfigurationClass = interface(JMediationAdConfigurationClass)
    ['{4B1C8239-35FB-480F-8817-C37BD6DA7ED7}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationNativeAdConfiguration')]
  JMediationNativeAdConfiguration = interface(JMediationAdConfiguration)
    ['{1D53566E-206C-415F-818B-45451378143E}']
    function getNativeAdOptions: Jnativead_NativeAdOptions; cdecl;
  end;
  TJMediationNativeAdConfiguration = class(TJavaGenericImport<JMediationNativeAdConfigurationClass, JMediationNativeAdConfiguration>) end;

  JMediationNativeAdCallbackClass = interface(JMediationAdCallbackClass)
    ['{02BAB194-337F-4260-AC2E-C3E46B3A226E}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationNativeAdCallback')]
  JMediationNativeAdCallback = interface(JMediationAdCallback)
    ['{98B40334-0A60-4544-B4BF-77B8DF4B6D40}']
    procedure onAdLeftApplication; cdecl;
    procedure onVideoComplete; cdecl;
    procedure onVideoMute; cdecl;
    procedure onVideoPause; cdecl;
    procedure onVideoPlay; cdecl;
    procedure onVideoUnmute; cdecl;
  end;
  TJMediationNativeAdCallback = class(TJavaGenericImport<JMediationNativeAdCallbackClass, JMediationNativeAdCallback>) end;

  JMediationInterstitialAdapterClass = interface(Jmediation_MediationAdapterClass)
    ['{F2AF30A8-AFEE-4EA5-9508-7C971A64D632}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationInterstitialAdapter')]
  JMediationInterstitialAdapter = interface(Jmediation_MediationAdapter)
    ['{7F5FA657-ED47-4F52-84F6-E2386C3ED74B}']
    procedure requestInterstitialAd(context: JContext; mediationInterstitialListener: JMediationInterstitialListener; bundle: JBundle; mediationAdRequest: JMediationAdRequest; bundle_1: JBundle); cdecl;
    procedure showInterstitial; cdecl;
  end;
  TJMediationInterstitialAdapter = class(TJavaGenericImport<JMediationInterstitialAdapterClass, JMediationInterstitialAdapter>) end;

  JMediationInterstitialAdConfigurationClass = interface(JMediationAdConfigurationClass)
    ['{F345E642-0546-455E-BA61-B63F38BD201E}']
    {class} function init(context: JContext; string_1: JString; bundle: JBundle; bundle_1: JBundle; boolean: Boolean; location_1: JLocation; int: Integer; int_1: Integer; string_2: JString; string_3: JString): JMediationInterstitialAdConfiguration; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationInterstitialAdConfiguration')]
  JMediationInterstitialAdConfiguration = interface(JMediationAdConfiguration)
    ['{C9BF9BFA-B79C-45E5-B119-3788A8C6364E}']
  end;
  TJMediationInterstitialAdConfiguration = class(TJavaGenericImport<JMediationInterstitialAdConfigurationClass, JMediationInterstitialAdConfiguration>) end;

  JMediationInterstitialAdCallbackClass = interface(JMediationAdCallbackClass)
    ['{0329C081-728E-47FB-A9C9-817448A224A5}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationInterstitialAdCallback')]
  JMediationInterstitialAdCallback = interface(JMediationAdCallback)
    ['{33B04D16-5A0D-4A2A-A283-D1C56391EA0B}']
    procedure onAdFailedToShow(adError: JAdError); cdecl;
    procedure onAdLeftApplication; cdecl;
  end;
  TJMediationInterstitialAdCallback = class(TJavaGenericImport<JMediationInterstitialAdCallbackClass, JMediationInterstitialAdCallback>) end;

  JMediationInterscrollerAdClass = interface(JMediationBannerAdClass)
    ['{2624EBF5-AB82-4BEE-AE95-62F22FE15AF6}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationInterscrollerAd')]
  JMediationInterscrollerAd = interface(JMediationBannerAd)
    ['{73D4FC42-1CE5-4DC9-8F65-E3CD5E6DEAAF}']
    function shouldDelegateInterscrollerEffect: Boolean; cdecl;
  end;
  TJMediationInterscrollerAd = class(TJavaGenericImport<JMediationInterscrollerAdClass, JMediationInterscrollerAd>) end;

  JMediationBannerAdapterClass = interface(Jmediation_MediationAdapterClass)
    ['{897F944A-2524-4DF3-AA3C-A991A23F5060}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationBannerAdapter')]
  JMediationBannerAdapter = interface(Jmediation_MediationAdapter)
    ['{227CE301-B392-4DE2-B63A-A0D9B66D88F7}']
    function getBannerView: JView; cdecl;
    procedure requestBannerAd(context: JContext; mediationBannerListener: JMediationBannerListener; bundle: JBundle; adSize: JAdSize; mediationAdRequest: JMediationAdRequest; bundle_1: JBundle); cdecl;
  end;
  TJMediationBannerAdapter = class(TJavaGenericImport<JMediationBannerAdapterClass, JMediationBannerAdapter>) end;

  JMediationBannerAdConfigurationClass = interface(JMediationAdConfigurationClass)
    ['{91AAB198-3B3F-447F-8EE7-EE54224CA077}']
    {class} function init(context: JContext; string_1: JString; bundle: JBundle; bundle_1: JBundle; boolean: Boolean; location_1: JLocation; int: Integer; int_1: Integer; string_2: JString; adSize: JAdSize; string_3: JString): JMediationBannerAdConfiguration; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationBannerAdConfiguration')]
  JMediationBannerAdConfiguration = interface(JMediationAdConfiguration)
    ['{FCCC84F1-274E-4576-9F44-39E5E13B55F7}']
    function getAdSize: JAdSize; cdecl;
  end;
  TJMediationBannerAdConfiguration = class(TJavaGenericImport<JMediationBannerAdConfigurationClass, JMediationBannerAdConfiguration>) end;

  JMediationBannerAdCallbackClass = interface(JMediationAdCallbackClass)
    ['{9CB9228B-C101-46DF-B361-143E043A49AD}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationBannerAdCallback')]
  JMediationBannerAdCallback = interface(JMediationAdCallback)
    ['{3206EBF7-1F24-4AB2-AD70-AE1235487522}']
    procedure onAdLeftApplication; cdecl;
  end;
  TJMediationBannerAdCallback = class(TJavaGenericImport<JMediationBannerAdCallbackClass, JMediationBannerAdCallback>) end;

  JMediationAppOpenAdConfigurationClass = interface(JMediationAdConfigurationClass)
    ['{68FB9399-E8A9-45A2-8E06-2C7A5E9E28F8}']
    {class} function init(context: JContext; string_1: JString; bundle: JBundle; bundle_1: JBundle; boolean: Boolean; location_1: JLocation; int: Integer; int_1: Integer; string_2: JString; string_3: JString): JMediationAppOpenAdConfiguration; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAppOpenAdConfiguration')]
  JMediationAppOpenAdConfiguration = interface(JMediationAdConfiguration)
    ['{6A138D76-7C37-491E-A8F9-0D55462A9AE2}']
  end;
  TJMediationAppOpenAdConfiguration = class(TJavaGenericImport<JMediationAppOpenAdConfigurationClass, JMediationAppOpenAdConfiguration>) end;

  JMediationAppOpenAdCallbackClass = interface(JMediationAdCallbackClass)
    ['{D28D60E6-368E-43AD-944C-B111394B240D}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/MediationAppOpenAdCallback')]
  JMediationAppOpenAdCallback = interface(JMediationAdCallback)
    ['{C37AF2D8-D046-4483-AD50-33A4E18E34CD}']
    procedure onAdFailedToShow(adError: JAdError); cdecl;
  end;
  TJMediationAppOpenAdCallback = class(TJavaGenericImport<JMediationAppOpenAdCallbackClass, JMediationAppOpenAdCallback>) end;

  JInterstitialAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{19FAAE45-F52A-48BA-8719-FA503EF83752}']
    {class} function init: JInterstitialAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/interstitial/InterstitialAdLoadCallback')]
  JInterstitialAdLoadCallback = interface(JAdLoadCallback)
    ['{80616D56-BFF6-48F2-A7E6-AABC957B6E59}']
  end;
  TJInterstitialAdLoadCallback = class(TJavaGenericImport<JInterstitialAdLoadCallbackClass, JInterstitialAdLoadCallback>) end;

  JAppOpenAd_AppOpenAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{0C8EDA57-27CA-41F9-9EA7-CE07847F05B0}']
    {class} function init: JAppOpenAd_AppOpenAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/appopen/AppOpenAd$AppOpenAdLoadCallback')]
  JAppOpenAd_AppOpenAdLoadCallback = interface(JAdLoadCallback)
    ['{73A62ED9-0074-4ED4-8C97-76DD75C1DBDC}']
  end;
  TJAppOpenAd_AppOpenAdLoadCallback = class(TJavaGenericImport<JAppOpenAd_AppOpenAdLoadCallbackClass, JAppOpenAd_AppOpenAdLoadCallback>) end;

  JAdManagerInterstitialAdLoadCallbackClass = interface(JAdLoadCallbackClass)
    ['{988CB396-4A42-41CE-A683-6655343CB8B8}']
    {class} function init: JAdManagerInterstitialAdLoadCallback; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AdManagerInterstitialAdLoadCallback')]
  JAdManagerInterstitialAdLoadCallback = interface(JAdLoadCallback)
    ['{305E5ED5-5E10-4485-A72D-C9D7F21D9A08}']
  end;
  TJAdManagerInterstitialAdLoadCallback = class(TJavaGenericImport<JAdManagerInterstitialAdLoadCallbackClass, JAdManagerInterstitialAdLoadCallback>) end;

  JAdManagerAdViewClass = interface(JBaseAdViewClass)
    ['{6F89E2DD-6035-4BFE-B1E0-EECFB9EAFB94}']
    {class} function init(context: JContext): JAdManagerAdView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JAdManagerAdView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer): JAdManagerAdView; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AdManagerAdView')]
  JAdManagerAdView = interface(JBaseAdView)
    ['{D8FFCDEC-9341-4D2D-8968-E9E246740B6D}']
    function getAdSizes: TJavaObjectArray<JAdSize>; cdecl;
    function getAppEventListener: JAppEventListener; cdecl;
    function getVideoController: JVideoController; cdecl;
    function getVideoOptions: JVideoOptions; cdecl;
    procedure loadAd(adManagerAdRequest: JAdManagerAdRequest); cdecl;
    procedure recordManualImpression; cdecl;
    procedure setAdSizes(adSizes: TJavaObjectArray<JAdSize>); cdecl;
    procedure setAppEventListener(appEventListener: JAppEventListener); cdecl;
    procedure setManualImpressionsEnabled(boolean: Boolean); cdecl;
    procedure setVideoOptions(videoOptions: JVideoOptions); cdecl;
  end;
  TJAdManagerAdView = class(TJavaGenericImport<JAdManagerAdViewClass, JAdManagerAdView>) end;

  JAdManagerAdRequestClass = interface(JAdRequestClass)
    ['{60D71772-84B2-419C-8215-D7A9306F3881}']
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AdManagerAdRequest')]
  JAdManagerAdRequest = interface(JAdRequest)
    ['{D0A72CD5-5BE5-4893-952D-F8BF0AD637B4}']
    function getCustomTargeting: JBundle; cdecl;
    function getPublisherProvidedId: JString; cdecl;
  end;
  TJAdManagerAdRequest = class(TJavaGenericImport<JAdManagerAdRequestClass, JAdManagerAdRequest>) end;

  JAdManagerAdRequest_BuilderClass = interface(JAbstractAdRequestBuilderClass)
    ['{271A30DA-AB25-41C7-9858-E5217B11AA06}']
    {class} function init: JAdManagerAdRequest_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/admanager/AdManagerAdRequest$Builder')]
  JAdManagerAdRequest_Builder = interface(JAbstractAdRequestBuilder)
    ['{147128C7-CD1E-4186-A5B8-56FA245E765B}']
    function addCategoryExclusion(string_1: JString): JAdManagerAdRequest_Builder; cdecl;
    function build: JAdManagerAdRequest; cdecl;
    function self: JAdManagerAdRequest_Builder; overload; cdecl;
    function setPublisherProvidedId(string_1: JString): JAdManagerAdRequest_Builder; cdecl;
  end;
  TJAdManagerAdRequest_Builder = class(TJavaGenericImport<JAdManagerAdRequest_BuilderClass, JAdManagerAdRequest_Builder>) end;

  JLoadAdErrorClass = interface(JAdErrorClass)
    ['{8D7B096C-BDD6-40D1-BF62-C6F159205763}']
    {class} function init(int: Integer; string_1: JString; string_2: JString; adError: JAdError; responseInfo: JResponseInfo): JLoadAdError; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/LoadAdError')]
  JLoadAdError = interface(JAdError)
    ['{6E1DDF04-7952-44AF-B5EE-9D405E1A58AC}']
    function getResponseInfo: JResponseInfo; cdecl;
    function toString: JString; cdecl;
  end;
  TJLoadAdError = class(TJavaGenericImport<JLoadAdErrorClass, JLoadAdError>) end;

  JAdRequest_BuilderClass = interface(JAbstractAdRequestBuilderClass)
    ['{70A37781-2DB4-466F-A280-1F82001E1521}']
    {class} function init: JAdRequest_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdRequest$Builder')]
  JAdRequest_Builder = interface(JAbstractAdRequestBuilder)
    ['{B847EEEC-90E2-4EF7-86A2-F1EE83F5BFE1}']
    function build: JAdRequest; cdecl;
    function self: JAdRequest_Builder; overload; cdecl;
  end;
  TJAdRequest_Builder = class(TJavaGenericImport<JAdRequest_BuilderClass, JAdRequest_Builder>) end;

  JAdInspectorErrorClass = interface(JAdErrorClass)
    ['{D8AF3812-9BAE-405A-AC80-DBD7EEA105A5}']
    {class} function _GetERROR_CODE_ALREADY_OPEN: Integer; cdecl;
    {class} function _GetERROR_CODE_FAILED_TO_LOAD: Integer; cdecl;
    {class} function _GetERROR_CODE_INTERNAL_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_NOT_IN_TEST_MODE: Integer; cdecl;
    {class} function init(int: Integer; string_1: JString; string_2: JString): JAdInspectorError; cdecl;
    {class} property ERROR_CODE_ALREADY_OPEN: Integer read _GetERROR_CODE_ALREADY_OPEN;
    {class} property ERROR_CODE_FAILED_TO_LOAD: Integer read _GetERROR_CODE_FAILED_TO_LOAD;
    {class} property ERROR_CODE_INTERNAL_ERROR: Integer read _GetERROR_CODE_INTERNAL_ERROR;
    {class} property ERROR_CODE_NOT_IN_TEST_MODE: Integer read _GetERROR_CODE_NOT_IN_TEST_MODE;
  end;

  [JavaSignature('com/google/android/gms/ads/AdInspectorError')]
  JAdInspectorError = interface(JAdError)
    ['{2070D4CC-246D-45DB-8D47-D56379CD87C3}']
    function getCode: Integer; cdecl;
  end;
  TJAdInspectorError = class(TJavaGenericImport<JAdInspectorErrorClass, JAdInspectorError>) end;

  JAdMobAdapterClass = interface(JAbstractAdViewAdapterClass)
    ['{E01015C6-DAD9-4BE9-AF17-F56E8FCAFECF}']
    {class} function _GetNEW_BUNDLE: JString; cdecl;
    {class} function init: JAdMobAdapter; cdecl;
    {class} property NEW_BUNDLE: JString read _GetNEW_BUNDLE;
  end;

  [JavaSignature('com/google/ads/mediation/admob/AdMobAdapter')]
  JAdMobAdapter = interface(JAbstractAdViewAdapter)
    ['{FA20A2C5-9BBA-4AC1-B00F-D1106D2B3413}']
  end;
  TJAdMobAdapter = class(TJavaGenericImport<JAdMobAdapterClass, JAdMobAdapter>) end;

implementation

end.
