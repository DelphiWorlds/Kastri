unit DW.Androidapi.JNI.AdMob.NativeAd;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net, Androidapi.JNI.Widget,
  Androidapi.JNI.Util, Androidapi.JNI.Os,
  // DW
  DW.Androidapi.JNI.AdMob;

type
  JAdChoicesInfo = interface;
  JAdChoicesView = interface;
  JMediaView = interface;
  JNativeAd = interface;
  JNativeAdAssetNames = interface;
  JNativeAdOptions = interface;
  JNativeAdOptions_AdChoicesPlacement = interface;
  JNativeAdOptions_Builder = interface;
  JNativeAdOptions_NativeMediaAspectRatio = interface;
  JNativeAdOptions_SwipeGestureDirection = interface;
  JNativeAdView = interface;
  JNativeAd_Image = interface;
  JNativeAd_OnNativeAdLoadedListener = interface;
  JNativeAd_UnconfirmedClickListener = interface;
  JNativeCustomFormatAd = interface;
  JNativeCustomFormatAd_DisplayOpenMeasurement = interface;
  JNativeCustomFormatAd_OnCustomClickListener = interface;
  JNativeCustomFormatAd_OnCustomFormatAdLoadedListener = interface;

  JNativeAd_OnNativeAdLoadedListenerClass = interface(IJavaClass)
    ['{5C0ADF98-A32E-49B2-93EA-1ACBA82BCBBE}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAd$OnNativeAdLoadedListener')]
  JNativeAd_OnNativeAdLoadedListener = interface(IJavaInstance)
    ['{1593787F-3171-44EC-BF8B-3A186896CBC0}']
    procedure onNativeAdLoaded(nativeAd: JNativeAd); cdecl;
  end;
  TJNativeAd_OnNativeAdLoadedListener = class(TJavaGenericImport<JNativeAd_OnNativeAdLoadedListenerClass, JNativeAd_OnNativeAdLoadedListener>) end;

  JNativeAdViewClass = interface(JFrameLayoutClass)
    ['{3DBECC7A-303F-448F-BD97-54686188EB00}']
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer; int_1: Integer): JNativeAdView; overload; cdecl;
    {class} function init(context: JContext): JNativeAdView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer): JNativeAdView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JNativeAdView; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdView')]
  JNativeAdView = interface(JFrameLayout)
    ['{85F0FE27-90EA-42F6-B0AE-5D2018FE69D2}']
    procedure addView(view: JView; int: Integer; layoutParams: JViewGroup_LayoutParams); cdecl;
    procedure bringChildToFront(view: JView); cdecl;
    procedure destroy; cdecl;
    function dispatchTouchEvent(motionEvent: JMotionEvent): Boolean; cdecl;
    function getAdChoicesView: JAdChoicesView; cdecl;
    function getAdvertiserView: JView; cdecl;
    function getBodyView: JView; cdecl;
    function getCallToActionView: JView; cdecl;
    function getHeadlineView: JView; cdecl;
    function getIconView: JView; cdecl;
    function getImageView: JView; cdecl;
    function getMediaView: JMediaView; cdecl;
    function getPriceView: JView; cdecl;
    function getStarRatingView: JView; cdecl;
    function getStoreView: JView; cdecl;
    procedure onVisibilityChanged(view: JView; int: Integer); cdecl;
    procedure removeAllViews; cdecl;
    procedure removeView(view: JView); cdecl;
    procedure setAdChoicesView(adChoicesView: JAdChoicesView); cdecl;
    procedure setAdvertiserView(view: JView); cdecl;
    procedure setBodyView(view: JView); cdecl;
    procedure setCallToActionView(view: JView); cdecl;
    procedure setClickConfirmingView(view: JView); cdecl;
    procedure setHeadlineView(view: JView); cdecl;
    procedure setIconView(view: JView); cdecl;
    procedure setImageView(view: JView); cdecl;
    procedure setMediaView(mediaView: JMediaView); cdecl;
    procedure setNativeAd(nativeAd: JNativeAd); cdecl;
    procedure setPriceView(view: JView); cdecl;
    procedure setStarRatingView(view: JView); cdecl;
    procedure setStoreView(view: JView); cdecl;
  end;
  TJNativeAdView = class(TJavaGenericImport<JNativeAdViewClass, JNativeAdView>) end;

  JNativeAdOptions_SwipeGestureDirectionClass = interface(JAnnotationClass)
    ['{94E20C9E-8132-4170-9387-1C5D638E5B31}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdOptions$SwipeGestureDirection')]
  JNativeAdOptions_SwipeGestureDirection = interface(JAnnotation)
    ['{55EA97CC-2177-49E7-B3BA-411B8102A8D4}']
  end;
  TJNativeAdOptions_SwipeGestureDirection = class(TJavaGenericImport<JNativeAdOptions_SwipeGestureDirectionClass, JNativeAdOptions_SwipeGestureDirection>) end;

  JNativeAdAssetNamesClass = interface(JObjectClass)
    ['{04103780-1358-4AEB-83F3-F59E9D236290}']
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

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdAssetNames')]
  JNativeAdAssetNames = interface(JObject)
    ['{DB2A75FD-6F08-45DD-8E11-23EC56E37127}']
  end;
  TJNativeAdAssetNames = class(TJavaGenericImport<JNativeAdAssetNamesClass, JNativeAdAssetNames>) end;

  JNativeCustomFormatAdClass = interface(IJavaClass)
    ['{EC66C61F-CD3A-4A5F-A4E7-8463757A60C1}']
    {class} function _GetASSET_NAME_VIDEO: JString; cdecl;
    {class} property ASSET_NAME_VIDEO: JString read _GetASSET_NAME_VIDEO;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeCustomFormatAd')]
  JNativeCustomFormatAd = interface(IJavaInstance)
    ['{BB1F666F-495E-41E2-946A-B44BC2D7A0CF}']
    procedure destroy; cdecl;
    function getAvailableAssetNames: JList; cdecl;
    function getCustomFormatId: JString; cdecl;
    function getDisplayOpenMeasurement: JNativeCustomFormatAd_DisplayOpenMeasurement; cdecl;
    function getImage(string_1: JString): JNativeAd_Image; cdecl;
    function getMediaContent: JMediaContent; cdecl;
    function getText(string_1: JString): JCharSequence; cdecl;
    procedure performClick(string_1: JString); cdecl;
    procedure recordImpression; cdecl;
  end;
  TJNativeCustomFormatAd = class(TJavaGenericImport<JNativeCustomFormatAdClass, JNativeCustomFormatAd>) end;

  JNativeCustomFormatAd_OnCustomFormatAdLoadedListenerClass = interface(IJavaClass)
    ['{741ED7BD-111B-4B0D-A1C9-772C3ED992A2}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeCustomFormatAd$OnCustomFormatAdLoadedListener')]
  JNativeCustomFormatAd_OnCustomFormatAdLoadedListener = interface(IJavaInstance)
    ['{ADD30CA4-ECBC-4724-BEF3-FD561652A643}']
    procedure onCustomFormatAdLoaded(nativeCustomFormatAd: JNativeCustomFormatAd); cdecl;
  end;
  TJNativeCustomFormatAd_OnCustomFormatAdLoadedListener = class(TJavaGenericImport<JNativeCustomFormatAd_OnCustomFormatAdLoadedListenerClass, JNativeCustomFormatAd_OnCustomFormatAdLoadedListener>) end;

  JNativeCustomFormatAd_OnCustomClickListenerClass = interface(IJavaClass)
    ['{4EA272EF-A951-4F3A-A6A7-10C1C697F41D}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeCustomFormatAd$OnCustomClickListener')]
  JNativeCustomFormatAd_OnCustomClickListener = interface(IJavaInstance)
    ['{56B8D5CE-0552-4760-94A9-56EA8961E83D}']
    procedure onCustomClick(nativeCustomFormatAd: JNativeCustomFormatAd; string_1: JString); cdecl;
  end;
  TJNativeCustomFormatAd_OnCustomClickListener = class(TJavaGenericImport<JNativeCustomFormatAd_OnCustomClickListenerClass, JNativeCustomFormatAd_OnCustomClickListener>) end;

  JNativeCustomFormatAd_DisplayOpenMeasurementClass = interface(IJavaClass)
    ['{A95452B8-F3CD-465E-96FE-DFEFAC2F44DC}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeCustomFormatAd$DisplayOpenMeasurement')]
  JNativeCustomFormatAd_DisplayOpenMeasurement = interface(IJavaInstance)
    ['{93D0CFE9-ABDB-4C46-8679-DE7D6BF062B4}']
    procedure setView(view: JView); cdecl;
    function start: Boolean; cdecl;
  end;
  TJNativeCustomFormatAd_DisplayOpenMeasurement = class(TJavaGenericImport<JNativeCustomFormatAd_DisplayOpenMeasurementClass, JNativeCustomFormatAd_DisplayOpenMeasurement>) end;

  JNativeAdOptionsClass = interface(JObjectClass)
    ['{490EDBF9-2FEE-440C-ADB2-B3BDFCF62FCE}']
    {class} function _GetADCHOICES_BOTTOM_LEFT: Integer; cdecl;
    {class} function _GetADCHOICES_BOTTOM_RIGHT: Integer; cdecl;
    {class} function _GetADCHOICES_TOP_LEFT: Integer; cdecl;
    {class} function _GetADCHOICES_TOP_RIGHT: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_ANY: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_PORTRAIT: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_SQUARE: Integer; cdecl;
    {class} function _GetNATIVE_MEDIA_ASPECT_RATIO_UNKNOWN: Integer; cdecl;
    {class} function _GetSWIPE_GESTURE_DIRECTION_DOWN: Integer; cdecl;
    {class} function _GetSWIPE_GESTURE_DIRECTION_LEFT: Integer; cdecl;
    {class} function _GetSWIPE_GESTURE_DIRECTION_RIGHT: Integer; cdecl;
    {class} function _GetSWIPE_GESTURE_DIRECTION_UP: Integer; cdecl;
    {class} property ADCHOICES_BOTTOM_LEFT: Integer read _GetADCHOICES_BOTTOM_LEFT;
    {class} property ADCHOICES_BOTTOM_RIGHT: Integer read _GetADCHOICES_BOTTOM_RIGHT;
    {class} property ADCHOICES_TOP_LEFT: Integer read _GetADCHOICES_TOP_LEFT;
    {class} property ADCHOICES_TOP_RIGHT: Integer read _GetADCHOICES_TOP_RIGHT;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_ANY: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_ANY;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_LANDSCAPE;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_PORTRAIT: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_PORTRAIT;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_SQUARE: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_SQUARE;
    {class} property NATIVE_MEDIA_ASPECT_RATIO_UNKNOWN: Integer read _GetNATIVE_MEDIA_ASPECT_RATIO_UNKNOWN;
    {class} property SWIPE_GESTURE_DIRECTION_DOWN: Integer read _GetSWIPE_GESTURE_DIRECTION_DOWN;
    {class} property SWIPE_GESTURE_DIRECTION_LEFT: Integer read _GetSWIPE_GESTURE_DIRECTION_LEFT;
    {class} property SWIPE_GESTURE_DIRECTION_RIGHT: Integer read _GetSWIPE_GESTURE_DIRECTION_RIGHT;
    {class} property SWIPE_GESTURE_DIRECTION_UP: Integer read _GetSWIPE_GESTURE_DIRECTION_UP;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdOptions')]
  JNativeAdOptions = interface(JObject)
    ['{6CACAF97-D26E-4EDB-8AE3-0F7FBAAE485D}']
    function getAdChoicesPlacement: Integer; cdecl;
    function getMediaAspectRatio: Integer; cdecl;
    function getVideoOptions: JVideoOptions; cdecl;
    function shouldRequestMultipleImages: Boolean; cdecl;
    function shouldReturnUrlsForImageAssets: Boolean; cdecl;
  end;
  TJNativeAdOptions = class(TJavaGenericImport<JNativeAdOptionsClass, JNativeAdOptions>) end;

  JNativeAdOptions_NativeMediaAspectRatioClass = interface(JAnnotationClass)
    ['{95026718-DB9D-4036-9EFE-A05597FE4421}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdOptions$NativeMediaAspectRatio')]
  JNativeAdOptions_NativeMediaAspectRatio = interface(JAnnotation)
    ['{BB27A3BE-4354-4ECF-B0AE-6CB07C722621}']
  end;
  TJNativeAdOptions_NativeMediaAspectRatio = class(TJavaGenericImport<JNativeAdOptions_NativeMediaAspectRatioClass, JNativeAdOptions_NativeMediaAspectRatio>) end;

  JNativeAdOptions_BuilderClass = interface(JObjectClass)
    ['{B514749F-A9EB-4CAF-AFB5-90480738BDE7}']
    {class} function init: JNativeAdOptions_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdOptions$Builder')]
  JNativeAdOptions_Builder = interface(JObject)
    ['{96FA6361-5C44-4A0C-9105-A724B5054EFB}']
    function build: JNativeAdOptions; cdecl;
    function enableCustomClickGestureDirection(int: Integer; boolean: Boolean): JNativeAdOptions_Builder; cdecl;
    function setAdChoicesPlacement(int: Integer): JNativeAdOptions_Builder; cdecl;
    function setMediaAspectRatio(int: Integer): JNativeAdOptions_Builder; cdecl;
    function setRequestCustomMuteThisAd(boolean: Boolean): JNativeAdOptions_Builder; cdecl;
    function setRequestMultipleImages(boolean: Boolean): JNativeAdOptions_Builder; cdecl;
    function setReturnUrlsForImageAssets(boolean: Boolean): JNativeAdOptions_Builder; cdecl;
    function setVideoOptions(videoOptions: JVideoOptions): JNativeAdOptions_Builder; cdecl;
  end;
  TJNativeAdOptions_Builder = class(TJavaGenericImport<JNativeAdOptions_BuilderClass, JNativeAdOptions_Builder>) end;

  JNativeAdOptions_AdChoicesPlacementClass = interface(JAnnotationClass)
    ['{B4C2F619-22E8-44AA-B97B-DDAC326FA4F6}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAdOptions$AdChoicesPlacement')]
  JNativeAdOptions_AdChoicesPlacement = interface(JAnnotation)
    ['{5B477161-8F8E-41E8-B2E5-2E57C5D476F5}']
  end;
  TJNativeAdOptions_AdChoicesPlacement = class(TJavaGenericImport<JNativeAdOptions_AdChoicesPlacementClass, JNativeAdOptions_AdChoicesPlacement>) end;

  JNativeAdClass = interface(JObjectClass)
    ['{20D317FB-3659-432F-8CA4-CEDC327688A9}']
    {class} function init: JNativeAd; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAd')]
  JNativeAd = interface(JObject)
    ['{841AF887-8FED-479D-B3F1-8250ACCEDB92}']
    procedure cancelUnconfirmedClick; cdecl;
    procedure destroy; cdecl;
    procedure enableCustomClickGesture; cdecl;
    function getAdChoicesInfo: JAdChoicesInfo; cdecl;
    function getAdvertiser: JString; cdecl;
    function getBody: JString; cdecl;
    function getCallToAction: JString; cdecl;
    function getExtras: JBundle; cdecl;
    function getHeadline: JString; cdecl;
    function getIcon: JNativeAd_Image; cdecl;
    function getImages: JList; cdecl;
    function getMediaContent: JMediaContent; cdecl;
    function getMuteThisAdReasons: JList; cdecl;
    function getPlacementId: Int64; cdecl;
    function getPrice: JString; cdecl;
    function getResponseInfo: JResponseInfo; cdecl;
    function getStarRating: JDouble; cdecl;
    function getStore: JString; cdecl;
    function isCustomClickGestureEnabled: Boolean; cdecl;
    function isCustomMuteThisAdEnabled: Boolean; cdecl;
    procedure muteThisAd(muteThisAdReason: JMuteThisAdReason); cdecl;
    procedure performClick(bundle: JBundle); cdecl;
    procedure recordCustomClickGesture; cdecl;
    function recordImpression(bundle: JBundle): Boolean; cdecl;
    procedure reportTouchEvent(bundle: JBundle); cdecl;
    procedure setMuteThisAdListener(muteThisAdListener: JMuteThisAdListener); cdecl;
    procedure setOnPaidEventListener(onPaidEventListener: JOnPaidEventListener); cdecl;
    procedure setPlacementId(long: Int64); cdecl;
    procedure setUnconfirmedClickListener(unconfirmedClickListener: JNativeAd_UnconfirmedClickListener); cdecl;
  end;
  TJNativeAd = class(TJavaGenericImport<JNativeAdClass, JNativeAd>) end;

  JNativeAd_UnconfirmedClickListenerClass = interface(IJavaClass)
    ['{3EE309E2-A2F4-4965-8935-B178B1BA6D35}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAd$UnconfirmedClickListener')]
  JNativeAd_UnconfirmedClickListener = interface(IJavaInstance)
    ['{FFB0670F-F5AF-46A1-99CA-3CA199F90445}']
    procedure onUnconfirmedClickCancelled; cdecl;
    procedure onUnconfirmedClickReceived(string_1: JString); cdecl;
  end;
  TJUnconfirmedClickListener = class(TJavaGenericImport<JNativeAd_UnconfirmedClickListenerClass, JNativeAd_UnconfirmedClickListener>) end;

  JOnNativeAdLoadedListenerClass = interface(IJavaClass)
    ['{5C0ADF98-A32E-49B2-93EA-1ACBA82BCBBE}']
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAd$OnNativeAdLoadedListener')]
  JOnNativeAdLoadedListener = interface(IJavaInstance)
    ['{1593787F-3171-44EC-BF8B-3A186896CBC0}']
    procedure onNativeAdLoaded(nativeAd: JNativeAd); cdecl;
  end;
  TJOnNativeAdLoadedListener = class(TJavaGenericImport<JOnNativeAdLoadedListenerClass, JOnNativeAdLoadedListener>) end;

  JNativeAd_ImageClass = interface(JObjectClass)
    ['{B334ED65-EF61-4A37-938C-BB75E8A4B00C}']
    {class} function init: JNativeAd_Image; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAd$Image')]
  JNativeAd_Image = interface(JObject)
    ['{E3BF005E-EDDD-4523-A62B-701E805A5142}']
    function getDrawable: JDrawable; cdecl;
    function getScale: Double; cdecl;
    function getUri: Jnet_Uri; cdecl;
  end;
  TJImage = class(TJavaGenericImport<JNativeAd_ImageClass, JNativeAd_Image>) end;

  JAdChoicesInfoClass = interface(JObjectClass)
    ['{F08B1B2A-50B7-4AC4-81D5-CA7EE3EEA382}']
    {class} function init: JAdChoicesInfo; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/NativeAd$AdChoicesInfo')]
  JAdChoicesInfo = interface(JObject)
    ['{6E479363-AB8D-49A4-92B7-144F24CEE95D}']
    function getImages: JList; cdecl;
    function getText: JCharSequence; cdecl;
  end;
  TJAdChoicesInfo = class(TJavaGenericImport<JAdChoicesInfoClass, JAdChoicesInfo>) end;

  JMediaViewClass = interface(JFrameLayoutClass)
    ['{44EC77BA-C6AB-43A3-9A7D-C2510D0EBE9E}']
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer; int_1: Integer): JMediaView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer): JMediaView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JMediaView; overload; cdecl;
    {class} function init(context: JContext): JMediaView; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/MediaView')]
  JMediaView = interface(JFrameLayout)
    ['{0DF3424B-29B8-46CC-825D-B35F21AED6D3}']
    function getMediaContent: JMediaContent; cdecl;
    procedure setImageScaleType(scaleType: JImageView_ScaleType); cdecl;
    procedure setMediaContent(mediaContent: JMediaContent); cdecl;
  end;
  TJMediaView = class(TJavaGenericImport<JMediaViewClass, JMediaView>) end;

  JAdChoicesViewClass = interface(JRelativeLayoutClass)
    ['{56C5C821-E2CF-427D-9121-E0CAD86DE2CF}']
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer; int_1: Integer): JAdChoicesView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet; int: Integer): JAdChoicesView; overload; cdecl;
    {class} function init(context: JContext; attributeSet: JAttributeSet): JAdChoicesView; overload; cdecl;
    {class} function init(context: JContext): JAdChoicesView; overload; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/nativead/AdChoicesView')]
  JAdChoicesView = interface(JRelativeLayout)
    ['{22BA8AE7-E8A5-4E3C-855F-079315FC630E}']
  end;
  TJAdChoicesView = class(TJavaGenericImport<JAdChoicesViewClass, JAdChoicesView>) end;

implementation

end.
