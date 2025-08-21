unit DW.Androidapi.JNI.AndroidX.Media3.Common;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.Os, Androidapi.JNI.Media,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Util,
  // DW
  DW.Androidapi.JNI.Guava;

type
  JAdPlaybackState = interface;
  JAdPlaybackState_AdGroup = interface;
  JAudioAttributes = interface;
  JAudioAttributes_AudioAttributesV21 = interface;
  JAuxEffectInfo = interface;
  JBitmapLoader = interface;
  JBundleable_Creator = interface;
  JClock = interface;
  JColorInfo = interface;
  JColorInfo_Builder = interface;
  JCueGroup = interface;
  JDataReader = interface;
  JDeviceInfo = interface;
  JDrmInitData = interface;
  JDrmInitData_SchemeData = interface;
  JErrorMessageProvider = interface;
  JFlagSet = interface;
  JFormat = interface;
  JFormat_Builder = interface;
  JHandlerWrapper = interface;
  JHandlerWrapper_Message = interface;
  JMediaItem = interface;
  JMediaItem_AdsConfiguration = interface;
  JMediaItem_AdsConfiguration_Builder = interface;
  JMediaItem_Builder = interface;
  JMediaItem_ClippingConfiguration = interface;
  JMediaItem_ClippingConfiguration_Builder = interface;
  JMediaItem_ClippingProperties = interface;
  JMediaItem_DrmConfiguration = interface;
  JMediaItem_DrmConfiguration_Builder = interface;
  JMediaItem_LiveConfiguration = interface;
  JMediaItem_LiveConfiguration_Builder = interface;
  JMediaItem_LocalConfiguration = interface;
  JMediaItem_RequestMetadata = interface;
  JMediaItem_RequestMetadata_Builder = interface;
  JMediaMetadata = interface;
  JMediaMetadata_Builder = interface;
  JMetadata = interface;
  JMetadata_Entry = interface;
  JPlaybackException = interface;
  JPlaybackParameters = interface;
  JPlayer = interface;
  JPlayer_Commands = interface;
  JPlayer_Commands_Builder = interface;
  JPlayer_Events = interface;
  JPlayer_Listener = interface;
  JPlayer_PositionInfo = interface;
  JPriorityTaskManager = interface;
  JRating = interface;
  JSize = interface;
  JTimeline = interface;
  JTimeline_Period = interface;
  JTimeline_Window = interface;
  JTrackGroup = interface;
  JTrackSelectionOverride = interface;
  JTrackSelectionParameters = interface;
  JTrackSelectionParameters_AudioOffloadPreferences = interface;
  JTrackSelectionParameters_AudioOffloadPreferences_Builder = interface;
  JTrackSelectionParameters_Builder = interface;
  JTracks = interface;
  JVideoSize = interface;

  JBitmapLoaderClass = interface(IJavaClass)
    ['{86BDB629-C94C-4845-B853-58D37AA024FE}']
  end;

  [JavaSignature('androidx/media3/common/util/BitmapLoader')]
  JBitmapLoader = interface(IJavaInstance)
    ['{516D03C5-E92B-48C0-A8CE-34D675B5E59E}']
    function decodeBitmap(bytes: TJavaArray<Byte>): JListenableFuture; cdecl;
    function loadBitmap(uri: Jnet_Uri): JListenableFuture; cdecl;
    function loadBitmapFromMetadata(mediametadata: JMediaMetadata): JListenableFuture; cdecl;
    function supportsMimeType(string_1: JString): Boolean; cdecl;
  end;
  TJBitmapLoader = class(TJavaGenericImport<JBitmapLoaderClass, JBitmapLoader>) end;

  JBasePlayerClass = interface(JObjectClass)
    ['{08D14E5F-172C-4189-8A90-2441B0C2E7B2}']
  end;

  [JavaSignature('androidx/media3/common/BasePlayer')]
  JBasePlayer = interface(JObject)
    ['{4074E0EF-CA91-4699-826E-280AF87E7C0B}']
    procedure addMediaItem(mediaitem: JMediaItem); overload; cdecl;
    procedure addMediaItem(int: Integer; mediaitem: JMediaItem); overload; cdecl;
    procedure addMediaItems(list: JList); cdecl;
    function canAdvertiseSession: Boolean; cdecl;
    procedure clearMediaItems; cdecl;
    function getBufferedPercentage: Integer; cdecl;
    function getContentDuration: Int64; cdecl;
    function getCurrentLiveOffset: Int64; cdecl;
    function getCurrentManifest: JObject; cdecl;
    function getCurrentMediaItem: JMediaItem; cdecl;
    function getCurrentWindowIndex: Integer; cdecl;
    function getMediaItemAt(int: Integer): JMediaItem; cdecl;
    function getMediaItemCount: Integer; cdecl;
    function getNextMediaItemIndex: Integer; cdecl;
    function getNextWindowIndex: Integer; cdecl;
    function getPreviousMediaItemIndex: Integer; cdecl;
    function getPreviousWindowIndex: Integer; cdecl;
    function hasNext: Boolean; cdecl;
    function hasNextMediaItem: Boolean; cdecl;
    function hasNextWindow: Boolean; cdecl;
    function hasPrevious: Boolean; cdecl;
    function hasPreviousMediaItem: Boolean; cdecl;
    function hasPreviousWindow: Boolean; cdecl;
    function isCommandAvailable(int: Integer): Boolean; cdecl;
    function isCurrentMediaItemDynamic: Boolean; cdecl;
    function isCurrentMediaItemLive: Boolean; cdecl;
    function isCurrentMediaItemSeekable: Boolean; cdecl;
    function isCurrentWindowDynamic: Boolean; cdecl;
    function isCurrentWindowLive: Boolean; cdecl;
    function isCurrentWindowSeekable: Boolean; cdecl;
    function isPlaying: Boolean; cdecl;
    procedure moveMediaItem(int: Integer; int_1: Integer); cdecl;
    procedure next; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure previous; cdecl;
    procedure removeMediaItem(int: Integer); cdecl;
    procedure replaceMediaItem(int: Integer; mediaitem: JMediaItem); cdecl;
    procedure seekBack; cdecl;
    procedure seekForward; cdecl;
    procedure seekTo(long: Int64); overload; cdecl;
    procedure seekTo(int: Integer; long: Int64); overload; cdecl;
    procedure seekTo(int: Integer; long: Int64; int_1: Integer; boolean: Boolean); overload; cdecl;
    procedure seekToDefaultPosition(int: Integer); overload; cdecl;
    procedure seekToDefaultPosition; overload; cdecl;
    procedure seekToNext; cdecl;
    procedure seekToNextMediaItem; cdecl;
    procedure seekToNextWindow; cdecl;
    procedure seekToPrevious; cdecl;
    procedure seekToPreviousMediaItem; cdecl;
    procedure seekToPreviousWindow; cdecl;
    procedure setMediaItem(mediaitem: JMediaItem); overload; cdecl;
    procedure setMediaItem(mediaitem: JMediaItem; long: Int64); overload; cdecl;
    procedure setMediaItem(mediaitem: JMediaItem; boolean: Boolean); overload; cdecl;
    procedure setMediaItems(list: JList); cdecl;
    procedure setPlaybackSpeed(float: Single); cdecl;
  end;
  TJBasePlayer = class(TJavaGenericImport<JBasePlayerClass, JBasePlayer>) end;

  JErrorMessageProviderClass = interface(IJavaClass)
    ['{6234D9BA-CA32-460E-883B-EAE4AC74DF34}']
  end;

  [JavaSignature('androidx/media3/common/ErrorMessageProvider')]
  JErrorMessageProvider = interface(IJavaInstance)
    ['{F9EAB7CB-1291-4C5A-B6B2-172BC95F6A09}']
    function getErrorMessage(t: JObject): JPair; cdecl;
  end;
  TJErrorMessageProvider = class(TJavaGenericImport<JErrorMessageProviderClass, JErrorMessageProvider>) end;

  JDataReaderClass = interface(IJavaClass)
    ['{BDF2989D-E068-47EE-8328-5D0C2711259A}']
  end;

  [JavaSignature('androidx/media3/common/DataReader')]
  JDataReader = interface(IJavaInstance)
    ['{0CC5A41C-D65C-4809-859D-8B2AAB9156BC}']
    function read(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer): Integer; cdecl;
  end;
  TJDataReader = class(TJavaGenericImport<JDataReaderClass, JDataReader>) end;

  JSizeClass = interface(JObjectClass)
    ['{2C82A571-5124-4097-BC3D-214BAACE46CB}']
    {class} function _GetUNKNOWN: JSize; cdecl;
    {class} function _GetZERO: JSize; cdecl;
    {class} function init(int: Integer; int_1: Integer): JSize; cdecl;
    {class} property UNKNOWN: JSize read _GetUNKNOWN;
    {class} property ZERO: JSize read _GetZERO;
  end;

  [JavaSignature('androidx/media3/common/util/Size')]
  JSize = interface(JObject)
    ['{2C29E78B-C6E7-45D9-B09E-2D5CB76F51D7}']
    function equals(object_1: JObject): Boolean; cdecl;
    function getHeight: Integer; cdecl;
    function getWidth: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJSize = class(TJavaGenericImport<JSizeClass, JSize>) end;

  JHandlerWrapperClass = interface(IJavaClass)
    ['{C9CCDFE2-D82D-42E2-937E-E90600C8D882}']
  end;

  [JavaSignature('androidx/media3/common/util/HandlerWrapper')]
  JHandlerWrapper = interface(IJavaInstance)
    ['{91356AC9-C7B1-4C31-A2DA-BAB751DDC39F}']
    function getLooper: JLooper; cdecl;
    function hasMessages(int: Integer): Boolean; cdecl;
    function obtainMessage(int: Integer; int_1: Integer; int_2: Integer; object_1: JObject): JHandlerWrapper_Message; cdecl; overload;
    function obtainMessage(int: Integer; int_1: Integer; int_2: Integer): JHandlerWrapper_Message; cdecl; overload;
    function obtainMessage(int: Integer): JHandlerWrapper_Message; cdecl; overload;
    function obtainMessage(int: Integer; object_1: JObject): JHandlerWrapper_Message; cdecl; overload;
    function post(runnable: JRunnable): Boolean; cdecl;
    function postAtFrontOfQueue(runnable: JRunnable): Boolean; cdecl;
    function postDelayed(runnable: JRunnable; long: Int64): Boolean; cdecl;
    procedure removeCallbacksAndMessages(object_1: JObject); cdecl;
    procedure removeMessages(int: Integer); cdecl;
    function sendEmptyMessage(int: Integer): Boolean; cdecl;
    function sendEmptyMessageAtTime(int: Integer; long: Int64): Boolean; cdecl;
    function sendEmptyMessageDelayed(int: Integer; int_1: Integer): Boolean; cdecl;
    function sendMessageAtFrontOfQueue(message: JHandlerWrapper_Message): Boolean; cdecl;
  end;
  TJHandlerWrapper = class(TJavaGenericImport<JHandlerWrapperClass, JHandlerWrapper>) end;

  JHandlerWrapper_MessageClass = interface(IJavaClass)
    ['{4309E359-8169-46FA-BDD2-5A93C4256E01}']
  end;

  [JavaSignature('androidx/media3/common/util/HandlerWrapper$Message')]
  JHandlerWrapper_Message = interface(IJavaInstance)
    ['{E44BA275-2756-412F-A01D-FE9AAF0A31E9}']
    function getTarget: JHandlerWrapper; cdecl;
    procedure sendToTarget; cdecl;
  end;
  TJHandlerWrapper_Message = class(TJavaGenericImport<JHandlerWrapper_MessageClass, JHandlerWrapper_Message>) end;

  JClockClass = interface(IJavaClass)
    ['{74B5C123-2FA0-4E1B-94FB-627FFCB8DAA5}']
    {class} function _GetDEFAULT: JClock; cdecl;
    {class} property &DEFAULT: JClock read _GetDEFAULT;
  end;

  [JavaSignature('androidx/media3/common/util/Clock')]
  JClock = interface(IJavaInstance)
    ['{E83FCFCA-8CF4-4B30-884F-63423EA66272}']
    function createHandler(looper: JLooper; callback: JHandler_Callback): JHandlerWrapper; cdecl;
    function currentTimeMillis: Int64; cdecl;
    function elapsedRealtime: Int64; cdecl;
    function nanoTime: Int64; cdecl;
    procedure onThreadBlocked; cdecl;
    function uptimeMillis: Int64; cdecl;
  end;
  TJClock = class(TJavaGenericImport<JClockClass, JClock>) end;

  JCueGroupClass = interface(JObjectClass)
    ['{7FF1DF2F-4B74-445F-82FC-DD2FAC90504E}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetEMPTY_TIME_ZERO: JCueGroup; cdecl;
    {class} function fromBundle(bundle: JBundle): JCueGroup; cdecl;
    {class} function init(list: JList; long: Int64): JCueGroup; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property EMPTY_TIME_ZERO: JCueGroup read _GetEMPTY_TIME_ZERO;
  end;

  [JavaSignature('androidx/media3/common/text/CueGroup')]
  JCueGroup = interface(JObject)
    ['{C0CCB69A-D643-4FCA-B530-063A82C5CFC9}']
    function _Getcues: JImmutableList; cdecl;
    function _GetpresentationTimeUs: Int64; cdecl;
    function toBundle: JBundle; cdecl;
    property cues: JImmutableList read _Getcues;
    property presentationTimeUs: Int64 read _GetpresentationTimeUs;
  end;
  TJCueGroup = class(TJavaGenericImport<JCueGroupClass, JCueGroup>) end;

  JVideoSizeClass = interface(JObjectClass)
    ['{7B40D6C6-27DF-4DA6-AECB-E13D62737D63}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetUNKNOWN: JVideoSize; cdecl;
    {class} function fromBundle(bundle: JBundle): JVideoSize; cdecl;
    {class} function init(int: Integer; int_1: Integer; int_2: Integer; float: Single): JVideoSize; cdecl; overload;
    {class} function init(int: Integer; int_1: Integer): JVideoSize; cdecl; overload;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property UNKNOWN: JVideoSize read _GetUNKNOWN;
  end;

  [JavaSignature('androidx/media3/common/VideoSize')]
  JVideoSize = interface(JObject)
    ['{9E16D825-FAE3-4144-A3EC-8AA3607C6E49}']
    function _Getheight: Integer; cdecl;
    function _GetpixelWidthHeightRatio: Single; cdecl;
    function _GetunappliedRotationDegrees: Integer; cdecl;
    function _Getwidth: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property height: Integer read _Getheight;
    property pixelWidthHeightRatio: Single read _GetpixelWidthHeightRatio;
    property unappliedRotationDegrees: Integer read _GetunappliedRotationDegrees;
    property width: Integer read _Getwidth;
  end;
  TJVideoSize = class(TJavaGenericImport<JVideoSizeClass, JVideoSize>) end;

  JTracksClass = interface(JObjectClass)
    ['{8607E7E2-9B5D-4BA3-9B2E-E8B53118D01B}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetEMPTY: JTracks; cdecl;
    {class} function fromBundle(bundle: JBundle): JTracks; cdecl;
    {class} function init(list: JList): JTracks; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property EMPTY: JTracks read _GetEMPTY;
  end;

  [JavaSignature('androidx/media3/common/Tracks')]
  JTracks = interface(JObject)
    ['{F05E8333-18E5-4FD7-8612-FF3CC1601B49}']
    function containsType(int: Integer): Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getGroups: JImmutableList; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function isTypeSelected(int: Integer): Boolean; cdecl;
    function isTypeSupported(int: Integer): Boolean; cdecl; overload;
    function isTypeSupported(int: Integer; boolean: Boolean): Boolean; cdecl; overload;
    function isTypeSupportedOrEmpty(int: Integer): Boolean; cdecl; overload;
    function isTypeSupportedOrEmpty(int: Integer; boolean: Boolean): Boolean; cdecl; overload;
    function toBundle: JBundle; cdecl;
  end;
  TJTracks = class(TJavaGenericImport<JTracksClass, JTracks>) end;

  JTrackSelectionParametersClass = interface(JObjectClass)
    ['{84045E5C-0034-400F-8D8B-1DF0D9EBAC13}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetDEFAULT: JTrackSelectionParameters; cdecl;
    {class} function _GetDEFAULT_WITHOUT_CONTEXT: JTrackSelectionParameters; cdecl;
    {class} function fromBundle(bundle: JBundle): JTrackSelectionParameters; cdecl;
    {class} function getDefaults(context: JContext): JTrackSelectionParameters; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property &DEFAULT: JTrackSelectionParameters read _GetDEFAULT;
    {class} property DEFAULT_WITHOUT_CONTEXT: JTrackSelectionParameters read _GetDEFAULT_WITHOUT_CONTEXT;
  end;

  [JavaSignature('androidx/media3/common/TrackSelectionParameters')]
  JTrackSelectionParameters = interface(JObject)
    ['{C1C7D42D-D324-4445-9BD3-F59F17AA91CE}']
    function _GetaudioOffloadPreferences: JTrackSelectionParameters_AudioOffloadPreferences; cdecl;
    function _GetdisabledTrackTypes: JImmutableSet; cdecl;
    function _GetforceHighestSupportedBitrate: Boolean; cdecl;
    function _GetforceLowestBitrate: Boolean; cdecl;
    function _GetignoredTextSelectionFlags: Integer; cdecl;
    function _GetisPrioritizeImageOverVideoEnabled: Boolean; cdecl;
    function _GetmaxAudioBitrate: Integer; cdecl;
    function _GetmaxAudioChannelCount: Integer; cdecl;
    function _GetmaxVideoBitrate: Integer; cdecl;
    function _GetmaxVideoFrameRate: Integer; cdecl;
    function _GetmaxVideoHeight: Integer; cdecl;
    function _GetmaxVideoWidth: Integer; cdecl;
    function _GetminVideoBitrate: Integer; cdecl;
    function _GetminVideoFrameRate: Integer; cdecl;
    function _GetminVideoHeight: Integer; cdecl;
    function _GetminVideoWidth: Integer; cdecl;
    function _Getoverrides: JImmutableMap; cdecl;
    function _GetpreferredAudioLanguages: JImmutableList; cdecl;
    function _GetpreferredAudioMimeTypes: JImmutableList; cdecl;
    function _GetpreferredAudioRoleFlags: Integer; cdecl;
    function _GetpreferredTextLanguages: JImmutableList; cdecl;
    function _GetpreferredTextRoleFlags: Integer; cdecl;
    function _GetpreferredVideoMimeTypes: JImmutableList; cdecl;
    function _GetpreferredVideoRoleFlags: Integer; cdecl;
    function _GetselectUndeterminedTextLanguage: Boolean; cdecl;
    function _GetviewportHeight: Integer; cdecl;
    function _GetviewportOrientationMayChange: Boolean; cdecl;
    function _GetviewportWidth: Integer; cdecl;
    function buildUpon: JTrackSelectionParameters_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property audioOffloadPreferences: JTrackSelectionParameters_AudioOffloadPreferences read _GetaudioOffloadPreferences;
    property disabledTrackTypes: JImmutableSet read _GetdisabledTrackTypes;
    property forceHighestSupportedBitrate: Boolean read _GetforceHighestSupportedBitrate;
    property forceLowestBitrate: Boolean read _GetforceLowestBitrate;
    property ignoredTextSelectionFlags: Integer read _GetignoredTextSelectionFlags;
    property isPrioritizeImageOverVideoEnabled: Boolean read _GetisPrioritizeImageOverVideoEnabled;
    property maxAudioBitrate: Integer read _GetmaxAudioBitrate;
    property maxAudioChannelCount: Integer read _GetmaxAudioChannelCount;
    property maxVideoBitrate: Integer read _GetmaxVideoBitrate;
    property maxVideoFrameRate: Integer read _GetmaxVideoFrameRate;
    property maxVideoHeight: Integer read _GetmaxVideoHeight;
    property maxVideoWidth: Integer read _GetmaxVideoWidth;
    property minVideoBitrate: Integer read _GetminVideoBitrate;
    property minVideoFrameRate: Integer read _GetminVideoFrameRate;
    property minVideoHeight: Integer read _GetminVideoHeight;
    property minVideoWidth: Integer read _GetminVideoWidth;
    property overrides: JImmutableMap read _Getoverrides;
    property preferredAudioLanguages: JImmutableList read _GetpreferredAudioLanguages;
    property preferredAudioMimeTypes: JImmutableList read _GetpreferredAudioMimeTypes;
    property preferredAudioRoleFlags: Integer read _GetpreferredAudioRoleFlags;
    property preferredTextLanguages: JImmutableList read _GetpreferredTextLanguages;
    property preferredTextRoleFlags: Integer read _GetpreferredTextRoleFlags;
    property preferredVideoMimeTypes: JImmutableList read _GetpreferredVideoMimeTypes;
    property preferredVideoRoleFlags: Integer read _GetpreferredVideoRoleFlags;
    property selectUndeterminedTextLanguage: Boolean read _GetselectUndeterminedTextLanguage;
    property viewportHeight: Integer read _GetviewportHeight;
    property viewportOrientationMayChange: Boolean read _GetviewportOrientationMayChange;
    property viewportWidth: Integer read _GetviewportWidth;
  end;
  TJTrackSelectionParameters = class(TJavaGenericImport<JTrackSelectionParametersClass, JTrackSelectionParameters>) end;

  JTrackSelectionParameters_BuilderClass = interface(JObjectClass)
    ['{E5E24220-C7BE-425A-A223-D2D8B6E36ABD}']
    {class} function init: JTrackSelectionParameters_Builder; cdecl; overload;
    {class} function init(context: JContext): JTrackSelectionParameters_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/media3/common/TrackSelectionParameters$Builder')]
  JTrackSelectionParameters_Builder = interface(JObject)
    ['{7A81135A-B5AD-4A4B-8B28-2B0386B6EC85}']
    function addOverride(trackselectionoverride: JTrackSelectionOverride): JTrackSelectionParameters_Builder; cdecl;
    function build: JTrackSelectionParameters; cdecl;
    function clearOverride(trackgroup: JTrackGroup): JTrackSelectionParameters_Builder; cdecl;
    function clearOverrides: JTrackSelectionParameters_Builder; cdecl;
    function clearOverridesOfType(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function clearVideoSizeConstraints: JTrackSelectionParameters_Builder; cdecl;
    function clearViewportSizeConstraints: JTrackSelectionParameters_Builder; cdecl;
    function setAudioOffloadPreferences(audiooffloadpreferences: JTrackSelectionParameters_AudioOffloadPreferences): JTrackSelectionParameters_Builder; cdecl;
    function setDisabledTrackTypes(set_1: JSet): JTrackSelectionParameters_Builder; cdecl;
    function setForceHighestSupportedBitrate(boolean: Boolean): JTrackSelectionParameters_Builder; cdecl;
    function setForceLowestBitrate(boolean: Boolean): JTrackSelectionParameters_Builder; cdecl;
    function setIgnoredTextSelectionFlags(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setMaxAudioBitrate(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setMaxAudioChannelCount(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setMaxVideoBitrate(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setMaxVideoFrameRate(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setMaxVideoSize(int: Integer; int_1: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setMaxVideoSizeSd: JTrackSelectionParameters_Builder; cdecl;
    function setMinVideoBitrate(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setMinVideoFrameRate(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setMinVideoSize(int: Integer; int_1: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setOverrideForType(trackselectionoverride: JTrackSelectionOverride): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredAudioLanguage(string_1: JString): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredAudioLanguages(string_1: JString): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredAudioMimeType(string_1: JString): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredAudioMimeTypes(string_1: JString): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredAudioRoleFlags(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredTextLanguage(string_1: JString): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredTextLanguageAndRoleFlagsToCaptioningManagerSettings(context: JContext): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredTextLanguages(string_1: JString): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredTextRoleFlags(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredVideoMimeType(string_1: JString): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredVideoMimeTypes(string_1: JString): JTrackSelectionParameters_Builder; cdecl;
    function setPreferredVideoRoleFlags(int: Integer): JTrackSelectionParameters_Builder; cdecl;
    function setPrioritizeImageOverVideoEnabled(boolean: Boolean): JTrackSelectionParameters_Builder; cdecl;
    function setSelectUndeterminedTextLanguage(boolean: Boolean): JTrackSelectionParameters_Builder; cdecl;
    function setTrackTypeDisabled(int: Integer; boolean: Boolean): JTrackSelectionParameters_Builder; cdecl;
    function setViewportSize(int: Integer; int_1: Integer; boolean: Boolean): JTrackSelectionParameters_Builder; cdecl;
    function setViewportSizeToPhysicalDisplaySize(context: JContext; boolean: Boolean): JTrackSelectionParameters_Builder; cdecl;
  end;
  TJTrackSelectionParameters_Builder = class(TJavaGenericImport<JTrackSelectionParameters_BuilderClass, JTrackSelectionParameters_Builder>) end;

  JTrackSelectionParameters_AudioOffloadPreferencesClass = interface(JObjectClass)
    ['{16B1BA5A-79BF-44C1-8EE3-03FE12410858}']
    {class} function _GetAUDIO_OFFLOAD_MODE_DISABLED: Integer; cdecl;
    {class} function _GetAUDIO_OFFLOAD_MODE_ENABLED: Integer; cdecl;
    {class} function _GetAUDIO_OFFLOAD_MODE_REQUIRED: Integer; cdecl;
    {class} function _GetDEFAULT: JTrackSelectionParameters_AudioOffloadPreferences; cdecl;
    {class} function fromBundle(bundle: JBundle): JTrackSelectionParameters_AudioOffloadPreferences; cdecl;
    {class} property AUDIO_OFFLOAD_MODE_DISABLED: Integer read _GetAUDIO_OFFLOAD_MODE_DISABLED;
    {class} property AUDIO_OFFLOAD_MODE_ENABLED: Integer read _GetAUDIO_OFFLOAD_MODE_ENABLED;
    {class} property AUDIO_OFFLOAD_MODE_REQUIRED: Integer read _GetAUDIO_OFFLOAD_MODE_REQUIRED;
    {class} property &DEFAULT: JTrackSelectionParameters_AudioOffloadPreferences read _GetDEFAULT;
  end;

  [JavaSignature('androidx/media3/common/TrackSelectionParameters$AudioOffloadPreferences')]
  JTrackSelectionParameters_AudioOffloadPreferences = interface(JObject)
    ['{071AB42B-C7F1-42BC-B6A4-BF842B7EF85A}']
    function _GetaudioOffloadMode: Integer; cdecl;
    function _GetisGaplessSupportRequired: Boolean; cdecl;
    function _GetisSpeedChangeSupportRequired: Boolean; cdecl;
    function buildUpon: JTrackSelectionParameters_AudioOffloadPreferences_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property audioOffloadMode: Integer read _GetaudioOffloadMode;
    property isGaplessSupportRequired: Boolean read _GetisGaplessSupportRequired;
    property isSpeedChangeSupportRequired: Boolean read _GetisSpeedChangeSupportRequired;
  end;
  TJTrackSelectionParameters_AudioOffloadPreferences = class(TJavaGenericImport<JTrackSelectionParameters_AudioOffloadPreferencesClass, JTrackSelectionParameters_AudioOffloadPreferences>) end;

  JTrackSelectionParameters_AudioOffloadPreferences_BuilderClass = interface(JObjectClass)
    ['{CFED8384-4880-4032-86A6-7DCF5AEF94E5}']
    {class} function init: JTrackSelectionParameters_AudioOffloadPreferences_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/TrackSelectionParameters$AudioOffloadPreferences$Builder')]
  JTrackSelectionParameters_AudioOffloadPreferences_Builder = interface(JObject)
    ['{1817E5F5-6EF0-4ACB-A994-96E148534113}']
    function build: JTrackSelectionParameters_AudioOffloadPreferences; cdecl;
    function setAudioOffloadMode(int: Integer): JTrackSelectionParameters_AudioOffloadPreferences_Builder; cdecl;
    function setIsGaplessSupportRequired(boolean: Boolean): JTrackSelectionParameters_AudioOffloadPreferences_Builder; cdecl;
    function setIsSpeedChangeSupportRequired(boolean: Boolean): JTrackSelectionParameters_AudioOffloadPreferences_Builder; cdecl;
  end;
  TJTrackSelectionParameters_AudioOffloadPreferences_Builder = class(TJavaGenericImport<JTrackSelectionParameters_AudioOffloadPreferences_BuilderClass, JTrackSelectionParameters_AudioOffloadPreferences_Builder>) end;

  JTrackSelectionOverrideClass = interface(JObjectClass)
    ['{BE0EB4BE-4B9E-4CA3-BE69-F4AFC547E34B}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JTrackSelectionOverride; cdecl;
    {class} function init(trackgroup: JTrackGroup; int: Integer): JTrackSelectionOverride; cdecl; overload;
    {class} function init(trackgroup: JTrackGroup; list: JList): JTrackSelectionOverride; cdecl; overload;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/TrackSelectionOverride')]
  JTrackSelectionOverride = interface(JObject)
    ['{D3DB01A3-C924-405E-BE79-BDDB1D007B17}']
    function _GetmediaTrackGroup: JTrackGroup; cdecl;
    function _GettrackIndices: JImmutableList; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getType: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property mediaTrackGroup: JTrackGroup read _GetmediaTrackGroup;
    property trackIndices: JImmutableList read _GettrackIndices;
  end;
  TJTrackSelectionOverride = class(TJavaGenericImport<JTrackSelectionOverrideClass, JTrackSelectionOverride>) end;

  JTrackGroupClass = interface(JObjectClass)
    ['{D80A9FED-2472-4543-9A9B-15B12F9A43AA}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JTrackGroup; cdecl;
    {class} function init(string_1: JString; format: JFormat): JTrackGroup; cdecl; overload;
    {class} function init(format: JFormat): JTrackGroup; cdecl; overload;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/TrackGroup')]
  JTrackGroup = interface(JObject)
    ['{6208BC36-5990-45EC-85B8-595B308F35D5}']
    function _Getid: JString; cdecl;
    function _Getlength: Integer; cdecl;
    function _Gettype: Integer; cdecl;
    function copyWithId(string_1: JString): JTrackGroup; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getFormat(int: Integer): JFormat; cdecl;
    function hashCode: Integer; cdecl;
    function indexOf(format: JFormat): Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property id: JString read _Getid;
    property length: Integer read _Getlength;
    property &type: Integer read _Gettype;
  end;
  TJTrackGroup = class(TJavaGenericImport<JTrackGroupClass, JTrackGroup>) end;

  JTimelineClass = interface(JObjectClass)
    ['{F16B839B-89BF-4CCC-9D34-E7E0197F79DB}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetEMPTY: JTimeline; cdecl;
    {class} function fromBundle(bundle: JBundle): JTimeline; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property EMPTY: JTimeline read _GetEMPTY;
  end;

  [JavaSignature('androidx/media3/common/Timeline')]
  JTimeline = interface(JObject)
    ['{68F78C8A-332E-44FE-915B-FFE34E5C928B}']
    function copyWithSingleWindow(int: Integer): JTimeline; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getFirstWindowIndex(boolean: Boolean): Integer; cdecl;
    function getIndexOfPeriod(object_1: JObject): Integer; cdecl;
    function getLastWindowIndex(boolean: Boolean): Integer; cdecl;
    function getNextPeriodIndex(int: Integer; period: JTimeline_Period; window: JTimeline_Window; int_1: Integer; boolean: Boolean): Integer; cdecl;
    function getNextWindowIndex(int: Integer; int_1: Integer; boolean: Boolean): Integer; cdecl;
    function getPeriod(int: Integer; period: JTimeline_Period): JTimeline_Period; cdecl; overload;
    function getPeriod(int: Integer; period: JTimeline_Period; boolean: Boolean): JTimeline_Period; cdecl; overload;
    function getPeriodByUid(object_1: JObject; period: JTimeline_Period): JTimeline_Period; cdecl;
    function getPeriodCount: Integer; cdecl;
    function getPeriodPosition(window: JTimeline_Window; period: JTimeline_Period; int: Integer; long: Int64; long_1: Int64): JPair; cdecl; overload;
    function getPeriodPosition(window: JTimeline_Window; period: JTimeline_Period; int: Integer; long: Int64): JPair; cdecl; overload;
    function getPeriodPositionUs(window: JTimeline_Window; period: JTimeline_Period; int: Integer; long: Int64): JPair; cdecl; overload;
    function getPeriodPositionUs(window: JTimeline_Window; period: JTimeline_Period; int: Integer; long: Int64; long_1: Int64): JPair; cdecl; overload;
    function getPreviousWindowIndex(int: Integer; int_1: Integer; boolean: Boolean): Integer; cdecl;
    function getUidOfPeriod(int: Integer): JObject; cdecl;
    function getWindow(int: Integer; window: JTimeline_Window): JTimeline_Window; cdecl; overload;
    function getWindow(int: Integer; window: JTimeline_Window; long: Int64): JTimeline_Window; cdecl; overload;
    function getWindowCount: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function isLastPeriod(int: Integer; period: JTimeline_Period; window: JTimeline_Window; int_1: Integer; boolean: Boolean): Boolean; cdecl;
    function toBundle: JBundle; cdecl;
  end;
  TJTimeline = class(TJavaGenericImport<JTimelineClass, JTimeline>) end;

  JTimeline_WindowClass = interface(JObjectClass)
    ['{04A9517D-F791-48EE-AAF0-A90C0B0318C9}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetSINGLE_WINDOW_UID: JObject; cdecl;
    {class} function fromBundle(bundle: JBundle): JTimeline_Window; cdecl;
    {class} function init: JTimeline_Window; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property SINGLE_WINDOW_UID: JObject read _GetSINGLE_WINDOW_UID;
  end;

  [JavaSignature('androidx/media3/common/Timeline$Window')]
  JTimeline_Window = interface(JObject)
    ['{553FBFAE-7029-424A-864A-5391DED50E78}']
    function _GetdefaultPositionUs: Int64; cdecl;
    function _GetdurationUs: Int64; cdecl;
    function _GetelapsedRealtimeEpochOffsetMs: Int64; cdecl;
    function _GetfirstPeriodIndex: Integer; cdecl;
    function _GetisDynamic: Boolean; cdecl;
    function _GetisLive: Boolean; cdecl;
    function _GetisPlaceholder: Boolean; cdecl;
    function _GetisSeekable: Boolean; cdecl;
    function _GetlastPeriodIndex: Integer; cdecl;
    function _GetliveConfiguration: JMediaItem_LiveConfiguration; cdecl;
    function _Getmanifest: JObject; cdecl;
    function _GetmediaItem: JMediaItem; cdecl;
    function _GetpositionInFirstPeriodUs: Int64; cdecl;
    function _GetpresentationStartTimeMs: Int64; cdecl;
    function _Gettag: JObject; cdecl;
    function _Getuid: JObject; cdecl;
    function _GetwindowStartTimeMs: Int64; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getCurrentUnixTimeMs: Int64; cdecl;
    function getDefaultPositionMs: Int64; cdecl;
    function getDefaultPositionUs: Int64; cdecl;
    function getDurationMs: Int64; cdecl;
    function getDurationUs: Int64; cdecl;
    function getPositionInFirstPeriodMs: Int64; cdecl;
    function getPositionInFirstPeriodUs: Int64; cdecl;
    function hashCode: Integer; cdecl;
    function isLive: Boolean; cdecl;
    function &set(object_1: JObject; mediaitem: JMediaItem; object_2: JObject; long: Int64; long_1: Int64; long_2: Int64; boolean: Boolean; boolean_1: Boolean; liveconfiguration: JMediaItem_LiveConfiguration; long_3: Int64; long_4: Int64; int: Integer; int_1: Integer; long_5: Int64): JTimeline_Window; cdecl;
    function toBundle: JBundle; cdecl;
    property defaultPositionUs: Int64 read _GetdefaultPositionUs;
    property durationUs: Int64 read _GetdurationUs;
    property elapsedRealtimeEpochOffsetMs: Int64 read _GetelapsedRealtimeEpochOffsetMs;
    property firstPeriodIndex: Integer read _GetfirstPeriodIndex;
    property isDynamic: Boolean read _GetisDynamic;
    property isLive: Boolean read _GetisLive;
    property isPlaceholder: Boolean read _GetisPlaceholder;
    property isSeekable: Boolean read _GetisSeekable;
    property lastPeriodIndex: Integer read _GetlastPeriodIndex;
    property liveConfiguration: JMediaItem_LiveConfiguration read _GetliveConfiguration;
    property manifest: JObject read _Getmanifest;
    property mediaItem: JMediaItem read _GetmediaItem;
    property positionInFirstPeriodUs: Int64 read _GetpositionInFirstPeriodUs;
    property presentationStartTimeMs: Int64 read _GetpresentationStartTimeMs;
    property tag: JObject read _Gettag;
    property uid: JObject read _Getuid;
    property windowStartTimeMs: Int64 read _GetwindowStartTimeMs;
  end;
  TJTimeline_Window = class(TJavaGenericImport<JTimeline_WindowClass, JTimeline_Window>) end;

  JTimeline_PeriodClass = interface(JObjectClass)
    ['{42D8A6FF-A4B2-486F-95F8-7917A5783621}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JTimeline_Period; cdecl;
    {class} function init: JTimeline_Period; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/Timeline$Period')]
  JTimeline_Period = interface(JObject)
    ['{FB1A3F72-C804-4881-A429-951AF36EFB61}']
    function _GetdurationUs: Int64; cdecl;
    function _Getid: JObject; cdecl;
    function _GetisPlaceholder: Boolean; cdecl;
    function _GetpositionInWindowUs: Int64; cdecl;
    function _Getuid: JObject; cdecl;
    function _GetwindowIndex: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getAdCountInAdGroup(int: Integer): Integer; cdecl;
    function getAdDurationUs(int: Integer; int_1: Integer): Int64; cdecl;
    function getAdGroupCount: Integer; cdecl;
    function getAdGroupIndexAfterPositionUs(long: Int64): Integer; cdecl;
    function getAdGroupIndexForPositionUs(long: Int64): Integer; cdecl;
    function getAdGroupTimeUs(int: Integer): Int64; cdecl;
    function getAdResumePositionUs: Int64; cdecl;
    function getAdState(int: Integer; int_1: Integer): Integer; cdecl;
    function getAdsId: JObject; cdecl;
    function getContentResumeOffsetUs(int: Integer): Int64; cdecl;
    function getDurationMs: Int64; cdecl;
    function getDurationUs: Int64; cdecl;
    function getFirstAdIndexToPlay(int: Integer): Integer; cdecl;
    function getNextAdIndexToPlay(int: Integer; int_1: Integer): Integer; cdecl;
    function getPositionInWindowMs: Int64; cdecl;
    function getPositionInWindowUs: Int64; cdecl;
    function getRemovedAdGroupCount: Integer; cdecl;
    function hasPlayedAdGroup(int: Integer): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isLivePostrollPlaceholder(int: Integer): Boolean; cdecl;
    function isServerSideInsertedAdGroup(int: Integer): Boolean; cdecl;
    function &set(object_1: JObject; object_2: JObject; int: Integer; long: Int64; long_1: Int64): JTimeline_Period; cdecl; overload;
    function &set(object_1: JObject; object_2: JObject; int: Integer; long: Int64; long_1: Int64; adplaybackstate: JAdPlaybackState; boolean: Boolean): JTimeline_Period; cdecl; overload;
    function toBundle: JBundle; cdecl;
    property durationUs: Int64 read _GetdurationUs;
    property id: JObject read _Getid;
    property isPlaceholder: Boolean read _GetisPlaceholder;
    property positionInWindowUs: Int64 read _GetpositionInWindowUs;
    property uid: JObject read _Getuid;
    property windowIndex: Integer read _GetwindowIndex;
  end;
  TJTimeline_Period = class(TJavaGenericImport<JTimeline_PeriodClass, JTimeline_Period>) end;

  JRatingClass = interface(JObjectClass)
    ['{E7561EF4-EACF-484D-9B33-317BA1E14D7B}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JRating; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/Rating')]
  JRating = interface(JObject)
    ['{DED6FA94-AD32-491B-B87D-8E8D38537413}']
    function isRated: Boolean; cdecl;
  end;
  TJRating = class(TJavaGenericImport<JRatingClass, JRating>) end;

  JPriorityTaskManagerClass = interface(JObjectClass)
    ['{1F4C3C0D-BFC0-48EA-8AA4-D900C2960551}']
    {class} function init: JPriorityTaskManager; cdecl;
  end;

  [JavaSignature('androidx/media3/common/PriorityTaskManager')]
  JPriorityTaskManager = interface(JObject)
    ['{54155149-5936-4BFA-A1BA-7017DA5A2BDB}']
    procedure add(int: Integer); cdecl;
    procedure proceed(int: Integer); cdecl;
    function proceedNonBlocking(int: Integer): Boolean; cdecl;
    procedure proceedOrThrow(int: Integer); cdecl;
    procedure remove(int: Integer); cdecl;
  end;
  TJPriorityTaskManager = class(TJavaGenericImport<JPriorityTaskManagerClass, JPriorityTaskManager>) end;

  JPlayerClass = interface(IJavaClass)
    ['{457B6CB3-8BCC-4006-B68E-A8FEC92ECCE8}']
    {class} function _GetCOMMAND_ADJUST_DEVICE_VOLUME: Integer; cdecl;
    {class} function _GetCOMMAND_ADJUST_DEVICE_VOLUME_WITH_FLAGS: Integer; cdecl;
    {class} function _GetCOMMAND_CHANGE_MEDIA_ITEMS: Integer; cdecl;
    {class} function _GetCOMMAND_GET_AUDIO_ATTRIBUTES: Integer; cdecl;
    {class} function _GetCOMMAND_GET_CURRENT_MEDIA_ITEM: Integer; cdecl;
    {class} function _GetCOMMAND_GET_DEVICE_VOLUME: Integer; cdecl;
    {class} function _GetCOMMAND_GET_MEDIA_ITEMS_METADATA: Integer; cdecl;
    {class} function _GetCOMMAND_GET_METADATA: Integer; cdecl;
    {class} function _GetCOMMAND_GET_TEXT: Integer; cdecl;
    {class} function _GetCOMMAND_GET_TIMELINE: Integer; cdecl;
    {class} function _GetCOMMAND_GET_TRACKS: Integer; cdecl;
    {class} function _GetCOMMAND_GET_VOLUME: Integer; cdecl;
    {class} function _GetCOMMAND_INVALID: Integer; cdecl;
    {class} function _GetCOMMAND_PLAY_PAUSE: Integer; cdecl;
    {class} function _GetCOMMAND_PREPARE: Integer; cdecl;
    {class} function _GetCOMMAND_RELEASE: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_BACK: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_FORWARD: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_IN_CURRENT_MEDIA_ITEM: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_IN_CURRENT_WINDOW: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_DEFAULT_POSITION: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_MEDIA_ITEM: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_NEXT: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_NEXT_MEDIA_ITEM: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_NEXT_WINDOW: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_PREVIOUS: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_PREVIOUS_MEDIA_ITEM: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_PREVIOUS_WINDOW: Integer; cdecl;
    {class} function _GetCOMMAND_SEEK_TO_WINDOW: Integer; cdecl;
    {class} function _GetCOMMAND_SET_AUDIO_ATTRIBUTES: Integer; cdecl;
    {class} function _GetCOMMAND_SET_DEVICE_VOLUME: Integer; cdecl;
    {class} function _GetCOMMAND_SET_DEVICE_VOLUME_WITH_FLAGS: Integer; cdecl;
    {class} function _GetCOMMAND_SET_MEDIA_ITEM: Integer; cdecl;
    {class} function _GetCOMMAND_SET_MEDIA_ITEMS_METADATA: Integer; cdecl;
    {class} function _GetCOMMAND_SET_PLAYLIST_METADATA: Integer; cdecl;
    {class} function _GetCOMMAND_SET_REPEAT_MODE: Integer; cdecl;
    {class} function _GetCOMMAND_SET_SHUFFLE_MODE: Integer; cdecl;
    {class} function _GetCOMMAND_SET_SPEED_AND_PITCH: Integer; cdecl;
    {class} function _GetCOMMAND_SET_TRACK_SELECTION_PARAMETERS: Integer; cdecl;
    {class} function _GetCOMMAND_SET_VIDEO_SURFACE: Integer; cdecl;
    {class} function _GetCOMMAND_SET_VOLUME: Integer; cdecl;
    {class} function _GetCOMMAND_STOP: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_AUTO_TRANSITION: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_INTERNAL: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_REMOVE: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_SEEK: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_SEEK_ADJUSTMENT: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_SILENCE_SKIP: Integer; cdecl;
    {class} function _GetDISCONTINUITY_REASON_SKIP: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_ATTRIBUTES_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_SESSION_ID: Integer; cdecl;
    {class} function _GetEVENT_AVAILABLE_COMMANDS_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_CUES: Integer; cdecl;
    {class} function _GetEVENT_DEVICE_INFO_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_DEVICE_VOLUME_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_IS_LOADING_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_IS_PLAYING_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_MAX_SEEK_TO_PREVIOUS_POSITION_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_MEDIA_ITEM_TRANSITION: Integer; cdecl;
    {class} function _GetEVENT_MEDIA_METADATA_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_METADATA: Integer; cdecl;
    {class} function _GetEVENT_PLAYBACK_PARAMETERS_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_PLAYBACK_STATE_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_PLAYBACK_SUPPRESSION_REASON_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_PLAYER_ERROR: Integer; cdecl;
    {class} function _GetEVENT_PLAYLIST_METADATA_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_PLAY_WHEN_READY_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_POSITION_DISCONTINUITY: Integer; cdecl;
    {class} function _GetEVENT_RENDERED_FIRST_FRAME: Integer; cdecl;
    {class} function _GetEVENT_REPEAT_MODE_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_SEEK_BACK_INCREMENT_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_SEEK_FORWARD_INCREMENT_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_SHUFFLE_MODE_ENABLED_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_SKIP_SILENCE_ENABLED_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_SURFACE_SIZE_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_TIMELINE_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_TRACKS_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_TRACK_SELECTION_PARAMETERS_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_SIZE_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_VOLUME_CHANGED: Integer; cdecl;
    {class} function _GetMEDIA_ITEM_TRANSITION_REASON_AUTO: Integer; cdecl;
    {class} function _GetMEDIA_ITEM_TRANSITION_REASON_PLAYLIST_CHANGED: Integer; cdecl;
    {class} function _GetMEDIA_ITEM_TRANSITION_REASON_REPEAT: Integer; cdecl;
    {class} function _GetMEDIA_ITEM_TRANSITION_REASON_SEEK: Integer; cdecl;
    {class} function _GetPLAYBACK_SUPPRESSION_REASON_NONE: Integer; cdecl;
    {class} function _GetPLAYBACK_SUPPRESSION_REASON_TRANSIENT_AUDIO_FOCUS_LOSS: Integer; cdecl;
    {class} function _GetPLAYBACK_SUPPRESSION_REASON_UNSUITABLE_AUDIO_OUTPUT: Integer; cdecl;
    {class} function _GetPLAYBACK_SUPPRESSION_REASON_UNSUITABLE_AUDIO_ROUTE: Integer; cdecl;
    {class} function _GetPLAY_WHEN_READY_CHANGE_REASON_AUDIO_BECOMING_NOISY: Integer; cdecl;
    {class} function _GetPLAY_WHEN_READY_CHANGE_REASON_AUDIO_FOCUS_LOSS: Integer; cdecl;
    {class} function _GetPLAY_WHEN_READY_CHANGE_REASON_END_OF_MEDIA_ITEM: Integer; cdecl;
    {class} function _GetPLAY_WHEN_READY_CHANGE_REASON_REMOTE: Integer; cdecl;
    {class} function _GetPLAY_WHEN_READY_CHANGE_REASON_SUPPRESSED_TOO_LONG: Integer; cdecl;
    {class} function _GetPLAY_WHEN_READY_CHANGE_REASON_USER_REQUEST: Integer; cdecl;
    {class} function _GetREPEAT_MODE_ALL: Integer; cdecl;
    {class} function _GetREPEAT_MODE_OFF: Integer; cdecl;
    {class} function _GetREPEAT_MODE_ONE: Integer; cdecl;
    {class} function _GetSTATE_BUFFERING: Integer; cdecl;
    {class} function _GetSTATE_ENDED: Integer; cdecl;
    {class} function _GetSTATE_IDLE: Integer; cdecl;
    {class} function _GetSTATE_READY: Integer; cdecl;
    {class} function _GetTIMELINE_CHANGE_REASON_PLAYLIST_CHANGED: Integer; cdecl;
    {class} function _GetTIMELINE_CHANGE_REASON_SOURCE_UPDATE: Integer; cdecl;
    {class} property COMMAND_ADJUST_DEVICE_VOLUME: Integer read _GetCOMMAND_ADJUST_DEVICE_VOLUME;
    {class} property COMMAND_ADJUST_DEVICE_VOLUME_WITH_FLAGS: Integer read _GetCOMMAND_ADJUST_DEVICE_VOLUME_WITH_FLAGS;
    {class} property COMMAND_CHANGE_MEDIA_ITEMS: Integer read _GetCOMMAND_CHANGE_MEDIA_ITEMS;
    {class} property COMMAND_GET_AUDIO_ATTRIBUTES: Integer read _GetCOMMAND_GET_AUDIO_ATTRIBUTES;
    {class} property COMMAND_GET_CURRENT_MEDIA_ITEM: Integer read _GetCOMMAND_GET_CURRENT_MEDIA_ITEM;
    {class} property COMMAND_GET_DEVICE_VOLUME: Integer read _GetCOMMAND_GET_DEVICE_VOLUME;
    {class} property COMMAND_GET_MEDIA_ITEMS_METADATA: Integer read _GetCOMMAND_GET_MEDIA_ITEMS_METADATA;
    {class} property COMMAND_GET_METADATA: Integer read _GetCOMMAND_GET_METADATA;
    {class} property COMMAND_GET_TEXT: Integer read _GetCOMMAND_GET_TEXT;
    {class} property COMMAND_GET_TIMELINE: Integer read _GetCOMMAND_GET_TIMELINE;
    {class} property COMMAND_GET_TRACKS: Integer read _GetCOMMAND_GET_TRACKS;
    {class} property COMMAND_GET_VOLUME: Integer read _GetCOMMAND_GET_VOLUME;
    {class} property COMMAND_INVALID: Integer read _GetCOMMAND_INVALID;
    {class} property COMMAND_PLAY_PAUSE: Integer read _GetCOMMAND_PLAY_PAUSE;
    {class} property COMMAND_PREPARE: Integer read _GetCOMMAND_PREPARE;
    {class} property COMMAND_RELEASE: Integer read _GetCOMMAND_RELEASE;
    {class} property COMMAND_SEEK_BACK: Integer read _GetCOMMAND_SEEK_BACK;
    {class} property COMMAND_SEEK_FORWARD: Integer read _GetCOMMAND_SEEK_FORWARD;
    {class} property COMMAND_SEEK_IN_CURRENT_MEDIA_ITEM: Integer read _GetCOMMAND_SEEK_IN_CURRENT_MEDIA_ITEM;
    {class} property COMMAND_SEEK_IN_CURRENT_WINDOW: Integer read _GetCOMMAND_SEEK_IN_CURRENT_WINDOW;
    {class} property COMMAND_SEEK_TO_DEFAULT_POSITION: Integer read _GetCOMMAND_SEEK_TO_DEFAULT_POSITION;
    {class} property COMMAND_SEEK_TO_MEDIA_ITEM: Integer read _GetCOMMAND_SEEK_TO_MEDIA_ITEM;
    {class} property COMMAND_SEEK_TO_NEXT: Integer read _GetCOMMAND_SEEK_TO_NEXT;
    {class} property COMMAND_SEEK_TO_NEXT_MEDIA_ITEM: Integer read _GetCOMMAND_SEEK_TO_NEXT_MEDIA_ITEM;
    {class} property COMMAND_SEEK_TO_NEXT_WINDOW: Integer read _GetCOMMAND_SEEK_TO_NEXT_WINDOW;
    {class} property COMMAND_SEEK_TO_PREVIOUS: Integer read _GetCOMMAND_SEEK_TO_PREVIOUS;
    {class} property COMMAND_SEEK_TO_PREVIOUS_MEDIA_ITEM: Integer read _GetCOMMAND_SEEK_TO_PREVIOUS_MEDIA_ITEM;
    {class} property COMMAND_SEEK_TO_PREVIOUS_WINDOW: Integer read _GetCOMMAND_SEEK_TO_PREVIOUS_WINDOW;
    {class} property COMMAND_SEEK_TO_WINDOW: Integer read _GetCOMMAND_SEEK_TO_WINDOW;
    {class} property COMMAND_SET_AUDIO_ATTRIBUTES: Integer read _GetCOMMAND_SET_AUDIO_ATTRIBUTES;
    {class} property COMMAND_SET_DEVICE_VOLUME: Integer read _GetCOMMAND_SET_DEVICE_VOLUME;
    {class} property COMMAND_SET_DEVICE_VOLUME_WITH_FLAGS: Integer read _GetCOMMAND_SET_DEVICE_VOLUME_WITH_FLAGS;
    {class} property COMMAND_SET_MEDIA_ITEM: Integer read _GetCOMMAND_SET_MEDIA_ITEM;
    {class} property COMMAND_SET_MEDIA_ITEMS_METADATA: Integer read _GetCOMMAND_SET_MEDIA_ITEMS_METADATA;
    {class} property COMMAND_SET_PLAYLIST_METADATA: Integer read _GetCOMMAND_SET_PLAYLIST_METADATA;
    {class} property COMMAND_SET_REPEAT_MODE: Integer read _GetCOMMAND_SET_REPEAT_MODE;
    {class} property COMMAND_SET_SHUFFLE_MODE: Integer read _GetCOMMAND_SET_SHUFFLE_MODE;
    {class} property COMMAND_SET_SPEED_AND_PITCH: Integer read _GetCOMMAND_SET_SPEED_AND_PITCH;
    {class} property COMMAND_SET_TRACK_SELECTION_PARAMETERS: Integer read _GetCOMMAND_SET_TRACK_SELECTION_PARAMETERS;
    {class} property COMMAND_SET_VIDEO_SURFACE: Integer read _GetCOMMAND_SET_VIDEO_SURFACE;
    {class} property COMMAND_SET_VOLUME: Integer read _GetCOMMAND_SET_VOLUME;
    {class} property COMMAND_STOP: Integer read _GetCOMMAND_STOP;
    {class} property DISCONTINUITY_REASON_AUTO_TRANSITION: Integer read _GetDISCONTINUITY_REASON_AUTO_TRANSITION;
    {class} property DISCONTINUITY_REASON_INTERNAL: Integer read _GetDISCONTINUITY_REASON_INTERNAL;
    {class} property DISCONTINUITY_REASON_REMOVE: Integer read _GetDISCONTINUITY_REASON_REMOVE;
    {class} property DISCONTINUITY_REASON_SEEK: Integer read _GetDISCONTINUITY_REASON_SEEK;
    {class} property DISCONTINUITY_REASON_SEEK_ADJUSTMENT: Integer read _GetDISCONTINUITY_REASON_SEEK_ADJUSTMENT;
    {class} property DISCONTINUITY_REASON_SILENCE_SKIP: Integer read _GetDISCONTINUITY_REASON_SILENCE_SKIP;
    {class} property DISCONTINUITY_REASON_SKIP: Integer read _GetDISCONTINUITY_REASON_SKIP;
    {class} property EVENT_AUDIO_ATTRIBUTES_CHANGED: Integer read _GetEVENT_AUDIO_ATTRIBUTES_CHANGED;
    {class} property EVENT_AUDIO_SESSION_ID: Integer read _GetEVENT_AUDIO_SESSION_ID;
    {class} property EVENT_AVAILABLE_COMMANDS_CHANGED: Integer read _GetEVENT_AVAILABLE_COMMANDS_CHANGED;
    {class} property EVENT_CUES: Integer read _GetEVENT_CUES;
    {class} property EVENT_DEVICE_INFO_CHANGED: Integer read _GetEVENT_DEVICE_INFO_CHANGED;
    {class} property EVENT_DEVICE_VOLUME_CHANGED: Integer read _GetEVENT_DEVICE_VOLUME_CHANGED;
    {class} property EVENT_IS_LOADING_CHANGED: Integer read _GetEVENT_IS_LOADING_CHANGED;
    {class} property EVENT_IS_PLAYING_CHANGED: Integer read _GetEVENT_IS_PLAYING_CHANGED;
    {class} property EVENT_MAX_SEEK_TO_PREVIOUS_POSITION_CHANGED: Integer read _GetEVENT_MAX_SEEK_TO_PREVIOUS_POSITION_CHANGED;
    {class} property EVENT_MEDIA_ITEM_TRANSITION: Integer read _GetEVENT_MEDIA_ITEM_TRANSITION;
    {class} property EVENT_MEDIA_METADATA_CHANGED: Integer read _GetEVENT_MEDIA_METADATA_CHANGED;
    {class} property EVENT_METADATA: Integer read _GetEVENT_METADATA;
    {class} property EVENT_PLAYBACK_PARAMETERS_CHANGED: Integer read _GetEVENT_PLAYBACK_PARAMETERS_CHANGED;
    {class} property EVENT_PLAYBACK_STATE_CHANGED: Integer read _GetEVENT_PLAYBACK_STATE_CHANGED;
    {class} property EVENT_PLAYBACK_SUPPRESSION_REASON_CHANGED: Integer read _GetEVENT_PLAYBACK_SUPPRESSION_REASON_CHANGED;
    {class} property EVENT_PLAYER_ERROR: Integer read _GetEVENT_PLAYER_ERROR;
    {class} property EVENT_PLAYLIST_METADATA_CHANGED: Integer read _GetEVENT_PLAYLIST_METADATA_CHANGED;
    {class} property EVENT_PLAY_WHEN_READY_CHANGED: Integer read _GetEVENT_PLAY_WHEN_READY_CHANGED;
    {class} property EVENT_POSITION_DISCONTINUITY: Integer read _GetEVENT_POSITION_DISCONTINUITY;
    {class} property EVENT_RENDERED_FIRST_FRAME: Integer read _GetEVENT_RENDERED_FIRST_FRAME;
    {class} property EVENT_REPEAT_MODE_CHANGED: Integer read _GetEVENT_REPEAT_MODE_CHANGED;
    {class} property EVENT_SEEK_BACK_INCREMENT_CHANGED: Integer read _GetEVENT_SEEK_BACK_INCREMENT_CHANGED;
    {class} property EVENT_SEEK_FORWARD_INCREMENT_CHANGED: Integer read _GetEVENT_SEEK_FORWARD_INCREMENT_CHANGED;
    {class} property EVENT_SHUFFLE_MODE_ENABLED_CHANGED: Integer read _GetEVENT_SHUFFLE_MODE_ENABLED_CHANGED;
    {class} property EVENT_SKIP_SILENCE_ENABLED_CHANGED: Integer read _GetEVENT_SKIP_SILENCE_ENABLED_CHANGED;
    {class} property EVENT_SURFACE_SIZE_CHANGED: Integer read _GetEVENT_SURFACE_SIZE_CHANGED;
    {class} property EVENT_TIMELINE_CHANGED: Integer read _GetEVENT_TIMELINE_CHANGED;
    {class} property EVENT_TRACKS_CHANGED: Integer read _GetEVENT_TRACKS_CHANGED;
    {class} property EVENT_TRACK_SELECTION_PARAMETERS_CHANGED: Integer read _GetEVENT_TRACK_SELECTION_PARAMETERS_CHANGED;
    {class} property EVENT_VIDEO_SIZE_CHANGED: Integer read _GetEVENT_VIDEO_SIZE_CHANGED;
    {class} property EVENT_VOLUME_CHANGED: Integer read _GetEVENT_VOLUME_CHANGED;
    {class} property MEDIA_ITEM_TRANSITION_REASON_AUTO: Integer read _GetMEDIA_ITEM_TRANSITION_REASON_AUTO;
    {class} property MEDIA_ITEM_TRANSITION_REASON_PLAYLIST_CHANGED: Integer read _GetMEDIA_ITEM_TRANSITION_REASON_PLAYLIST_CHANGED;
    {class} property MEDIA_ITEM_TRANSITION_REASON_REPEAT: Integer read _GetMEDIA_ITEM_TRANSITION_REASON_REPEAT;
    {class} property MEDIA_ITEM_TRANSITION_REASON_SEEK: Integer read _GetMEDIA_ITEM_TRANSITION_REASON_SEEK;
    {class} property PLAYBACK_SUPPRESSION_REASON_NONE: Integer read _GetPLAYBACK_SUPPRESSION_REASON_NONE;
    {class} property PLAYBACK_SUPPRESSION_REASON_TRANSIENT_AUDIO_FOCUS_LOSS: Integer read _GetPLAYBACK_SUPPRESSION_REASON_TRANSIENT_AUDIO_FOCUS_LOSS;
    {class} property PLAYBACK_SUPPRESSION_REASON_UNSUITABLE_AUDIO_OUTPUT: Integer read _GetPLAYBACK_SUPPRESSION_REASON_UNSUITABLE_AUDIO_OUTPUT;
    {class} property PLAYBACK_SUPPRESSION_REASON_UNSUITABLE_AUDIO_ROUTE: Integer read _GetPLAYBACK_SUPPRESSION_REASON_UNSUITABLE_AUDIO_ROUTE;
    {class} property PLAY_WHEN_READY_CHANGE_REASON_AUDIO_BECOMING_NOISY: Integer read _GetPLAY_WHEN_READY_CHANGE_REASON_AUDIO_BECOMING_NOISY;
    {class} property PLAY_WHEN_READY_CHANGE_REASON_AUDIO_FOCUS_LOSS: Integer read _GetPLAY_WHEN_READY_CHANGE_REASON_AUDIO_FOCUS_LOSS;
    {class} property PLAY_WHEN_READY_CHANGE_REASON_END_OF_MEDIA_ITEM: Integer read _GetPLAY_WHEN_READY_CHANGE_REASON_END_OF_MEDIA_ITEM;
    {class} property PLAY_WHEN_READY_CHANGE_REASON_REMOTE: Integer read _GetPLAY_WHEN_READY_CHANGE_REASON_REMOTE;
    {class} property PLAY_WHEN_READY_CHANGE_REASON_SUPPRESSED_TOO_LONG: Integer read _GetPLAY_WHEN_READY_CHANGE_REASON_SUPPRESSED_TOO_LONG;
    {class} property PLAY_WHEN_READY_CHANGE_REASON_USER_REQUEST: Integer read _GetPLAY_WHEN_READY_CHANGE_REASON_USER_REQUEST;
    {class} property REPEAT_MODE_ALL: Integer read _GetREPEAT_MODE_ALL;
    {class} property REPEAT_MODE_OFF: Integer read _GetREPEAT_MODE_OFF;
    {class} property REPEAT_MODE_ONE: Integer read _GetREPEAT_MODE_ONE;
    {class} property STATE_BUFFERING: Integer read _GetSTATE_BUFFERING;
    {class} property STATE_ENDED: Integer read _GetSTATE_ENDED;
    {class} property STATE_IDLE: Integer read _GetSTATE_IDLE;
    {class} property STATE_READY: Integer read _GetSTATE_READY;
    {class} property TIMELINE_CHANGE_REASON_PLAYLIST_CHANGED: Integer read _GetTIMELINE_CHANGE_REASON_PLAYLIST_CHANGED;
    {class} property TIMELINE_CHANGE_REASON_SOURCE_UPDATE: Integer read _GetTIMELINE_CHANGE_REASON_SOURCE_UPDATE;
  end;

  [JavaSignature('androidx/media3/common/Player')]
  JPlayer = interface(IJavaInstance)
    ['{7C64262A-9F0C-46D4-81B2-E084BD8A7615}']
    procedure addListener(listener: JPlayer_Listener); cdecl;
    procedure addMediaItem(mediaitem: JMediaItem); cdecl; overload;
    procedure addMediaItem(int: Integer; mediaitem: JMediaItem); cdecl; overload;
    procedure addMediaItems(list: JList); cdecl; overload;
    procedure addMediaItems(int: Integer; list: JList); cdecl; overload;
    function canAdvertiseSession: Boolean; cdecl;
    procedure clearMediaItems; cdecl;
    procedure clearVideoSurface; cdecl; overload;
    procedure clearVideoSurface(surface: JSurface); cdecl; overload;
    procedure clearVideoSurfaceHolder(surfaceholder: JSurfaceHolder); cdecl;
    procedure clearVideoSurfaceView(surfaceview: JSurfaceView); cdecl;
    procedure clearVideoTextureView(textureview: JTextureView); cdecl;
    procedure decreaseDeviceVolume(int: Integer); cdecl; overload;
    procedure decreaseDeviceVolume; cdecl; overload;
    function getApplicationLooper: JLooper; cdecl;
    function getAudioAttributes: JAudioAttributes; cdecl;
    function getAvailableCommands: JPlayer_Commands; cdecl;
    function getBufferedPercentage: Integer; cdecl;
    function getBufferedPosition: Int64; cdecl;
    function getContentBufferedPosition: Int64; cdecl;
    function getContentDuration: Int64; cdecl;
    function getContentPosition: Int64; cdecl;
    function getCurrentAdGroupIndex: Integer; cdecl;
    function getCurrentAdIndexInAdGroup: Integer; cdecl;
    function getCurrentCues: JCueGroup; cdecl;
    function getCurrentLiveOffset: Int64; cdecl;
    function getCurrentManifest: JObject; cdecl;
    function getCurrentMediaItem: JMediaItem; cdecl;
    function getCurrentMediaItemIndex: Integer; cdecl;
    function getCurrentPeriodIndex: Integer; cdecl;
    function getCurrentPosition: Int64; cdecl;
    function getCurrentTimeline: JTimeline; cdecl;
    function getCurrentTracks: JTracks; cdecl;
    function getCurrentWindowIndex: Integer; cdecl;
    function getDeviceInfo: JDeviceInfo; cdecl;
    function getDeviceVolume: Integer; cdecl;
    function getDuration: Int64; cdecl;
    function getMaxSeekToPreviousPosition: Int64; cdecl;
    function getMediaItemAt(int: Integer): JMediaItem; cdecl;
    function getMediaItemCount: Integer; cdecl;
    function getMediaMetadata: JMediaMetadata; cdecl;
    function getNextMediaItemIndex: Integer; cdecl;
    function getNextWindowIndex: Integer; cdecl;
    function getPlayWhenReady: Boolean; cdecl;
    function getPlaybackParameters: JPlaybackParameters; cdecl;
    function getPlaybackState: Integer; cdecl;
    function getPlaybackSuppressionReason: Integer; cdecl;
    function getPlayerError: JPlaybackException; cdecl;
    function getPlaylistMetadata: JMediaMetadata; cdecl;
    function getPreviousMediaItemIndex: Integer; cdecl;
    function getPreviousWindowIndex: Integer; cdecl;
    function getRepeatMode: Integer; cdecl;
    function getSeekBackIncrement: Int64; cdecl;
    function getSeekForwardIncrement: Int64; cdecl;
    function getShuffleModeEnabled: Boolean; cdecl;
    function getSurfaceSize: JSize; cdecl;
    function getTotalBufferedDuration: Int64; cdecl;
    function getTrackSelectionParameters: JTrackSelectionParameters; cdecl;
    function getVideoSize: JVideoSize; cdecl;
    function getVolume: Single; cdecl;
    function hasNext: Boolean; cdecl;
    function hasNextMediaItem: Boolean; cdecl;
    function hasNextWindow: Boolean; cdecl;
    function hasPrevious: Boolean; cdecl;
    function hasPreviousMediaItem: Boolean; cdecl;
    function hasPreviousWindow: Boolean; cdecl;
    procedure increaseDeviceVolume; cdecl; overload;
    procedure increaseDeviceVolume(int: Integer); cdecl; overload;
    function isCommandAvailable(int: Integer): Boolean; cdecl;
    function isCurrentMediaItemDynamic: Boolean; cdecl;
    function isCurrentMediaItemLive: Boolean; cdecl;
    function isCurrentMediaItemSeekable: Boolean; cdecl;
    function isCurrentWindowDynamic: Boolean; cdecl;
    function isCurrentWindowLive: Boolean; cdecl;
    function isCurrentWindowSeekable: Boolean; cdecl;
    function isDeviceMuted: Boolean; cdecl;
    function isLoading: Boolean; cdecl;
    function isPlaying: Boolean; cdecl;
    function isPlayingAd: Boolean; cdecl;
    procedure moveMediaItem(int: Integer; int_1: Integer); cdecl;
    procedure moveMediaItems(int: Integer; int_1: Integer; int_2: Integer); cdecl;
    procedure next; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure prepare; cdecl;
    procedure previous; cdecl;
    procedure release; cdecl;
    procedure removeListener(listener: JPlayer_Listener); cdecl;
    procedure removeMediaItem(int: Integer); cdecl;
    procedure removeMediaItems(int: Integer; int_1: Integer); cdecl;
    procedure replaceMediaItem(int: Integer; mediaitem: JMediaItem); cdecl;
    procedure replaceMediaItems(int: Integer; int_1: Integer; list: JList); cdecl;
    procedure seekBack; cdecl;
    procedure seekForward; cdecl;
    procedure seekTo(int: Integer; long: Int64); cdecl; overload;
    procedure seekTo(long: Int64); cdecl; overload;
    procedure seekToDefaultPosition(int: Integer); cdecl; overload;
    procedure seekToDefaultPosition; cdecl; overload;
    procedure seekToNext; cdecl;
    procedure seekToNextMediaItem; cdecl;
    procedure seekToNextWindow; cdecl;
    procedure seekToPrevious; cdecl;
    procedure seekToPreviousMediaItem; cdecl;
    procedure seekToPreviousWindow; cdecl;
    procedure setAudioAttributes(audioattributes: JAudioAttributes; boolean: Boolean); cdecl;
    procedure setDeviceMuted(boolean: Boolean); cdecl; overload;
    procedure setDeviceMuted(boolean: Boolean; int_1: Integer); cdecl; overload;
    procedure setDeviceVolume(int: Integer); cdecl; overload;
    procedure setDeviceVolume(int: Integer; int_1: Integer); cdecl; overload;
    procedure setMediaItem(mediaitem: JMediaItem; boolean: Boolean); cdecl; overload;
    procedure setMediaItem(mediaitem: JMediaItem); cdecl; overload;
    procedure setMediaItem(mediaitem: JMediaItem; long: Int64); cdecl; overload;
    procedure setMediaItems(list: JList); cdecl; overload;
    procedure setMediaItems(list: JList; boolean: Boolean); cdecl; overload;
    procedure setMediaItems(list: JList; int: Integer; long: Int64); cdecl; overload;
    procedure setPlayWhenReady(boolean: Boolean); cdecl;
    procedure setPlaybackParameters(playbackparameters: JPlaybackParameters); cdecl;
    procedure setPlaybackSpeed(float: Single); cdecl;
    procedure setPlaylistMetadata(mediametadata: JMediaMetadata); cdecl;
    procedure setRepeatMode(int: Integer); cdecl;
    procedure setShuffleModeEnabled(boolean: Boolean); cdecl;
    procedure setTrackSelectionParameters(trackselectionparameters: JTrackSelectionParameters); cdecl;
    procedure setVideoSurface(surface: JSurface); cdecl;
    procedure setVideoSurfaceHolder(surfaceholder: JSurfaceHolder); cdecl;
    procedure setVideoSurfaceView(surfaceview: JSurfaceView); cdecl;
    procedure setVideoTextureView(textureview: JTextureView); cdecl;
    procedure setVolume(float: Single); cdecl;
    procedure stop; cdecl;
  end;
  TJPlayer = class(TJavaGenericImport<JPlayerClass, JPlayer>) end;

  JPlayer_PositionInfoClass = interface(JObjectClass)
    ['{91937906-A212-4E28-A23D-58C56624E641}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JPlayer_PositionInfo; cdecl;
    {class} function init(object_1: JObject; int: Integer; object_2: JObject; int_1: Integer; long: Int64; long_1: Int64; int_2: Integer; int_3: Integer): JPlayer_PositionInfo; cdecl; overload;
    {class} function init(object_1: JObject; int: Integer; mediaitem: JMediaItem; object_2: JObject; int_1: Integer; long: Int64; long_1: Int64; int_2: Integer; int_3: Integer): JPlayer_PositionInfo; cdecl; overload;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/Player$PositionInfo')]
  JPlayer_PositionInfo = interface(JObject)
    ['{4CB14E20-0798-4FBB-94C9-096EB1218179}']
    function _GetadGroupIndex: Integer; cdecl;
    function _GetadIndexInAdGroup: Integer; cdecl;
    function _GetcontentPositionMs: Int64; cdecl;
    function _GetmediaItem: JMediaItem; cdecl;
    function _GetmediaItemIndex: Integer; cdecl;
    function _GetperiodIndex: Integer; cdecl;
    function _GetperiodUid: JObject; cdecl;
    function _GetpositionMs: Int64; cdecl;
    function _GetwindowIndex: Integer; cdecl;
    function _GetwindowUid: JObject; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function equalsForBundling(positioninfo: JPlayer_PositionInfo): Boolean; cdecl;
    function filterByAvailableCommands(boolean: Boolean; boolean_1: Boolean): JPlayer_PositionInfo; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl; overload;
    function toBundle(int: Integer): JBundle; cdecl; overload;
    property adGroupIndex: Integer read _GetadGroupIndex;
    property adIndexInAdGroup: Integer read _GetadIndexInAdGroup;
    property contentPositionMs: Int64 read _GetcontentPositionMs;
    property mediaItem: JMediaItem read _GetmediaItem;
    property mediaItemIndex: Integer read _GetmediaItemIndex;
    property periodIndex: Integer read _GetperiodIndex;
    property periodUid: JObject read _GetperiodUid;
    property positionMs: Int64 read _GetpositionMs;
    property windowIndex: Integer read _GetwindowIndex;
    property windowUid: JObject read _GetwindowUid;
  end;
  TJPlayer_PositionInfo = class(TJavaGenericImport<JPlayer_PositionInfoClass, JPlayer_PositionInfo>) end;

  JPlayer_ListenerClass = interface(IJavaClass)
    ['{E92C5DD2-C2B2-40A7-8B16-4E12E7DF6FEF}']
  end;

  [JavaSignature('androidx/media3/common/Player$Listener')]
  JPlayer_Listener = interface(IJavaInstance)
    ['{FBFDA90D-6B85-4184-AF28-0D43CE455F35}']
    procedure onAudioAttributesChanged(audioattributes: JAudioAttributes); cdecl;
    procedure onAudioSessionIdChanged(int: Integer); cdecl;
    procedure onAvailableCommandsChanged(commands: JPlayer_Commands); cdecl;
    procedure onCues(list: JList); cdecl; overload;
    procedure onCues(cuegroup: JCueGroup); cdecl; overload;
    procedure onDeviceInfoChanged(deviceinfo: JDeviceInfo); cdecl;
    procedure onDeviceVolumeChanged(int: Integer; boolean: Boolean); cdecl;
    procedure onEvents(player: JPlayer; events: JPlayer_Events); cdecl;
    procedure onIsLoadingChanged(boolean: Boolean); cdecl;
    procedure onIsPlayingChanged(boolean: Boolean); cdecl;
    procedure onLoadingChanged(boolean: Boolean); cdecl;
    procedure onMaxSeekToPreviousPositionChanged(long: Int64); cdecl;
    procedure onMediaItemTransition(mediaitem: JMediaItem; int: Integer); cdecl;
    procedure onMediaMetadataChanged(mediametadata: JMediaMetadata); cdecl;
    procedure onMetadata(metadata: JMetadata); cdecl;
    procedure onPlayWhenReadyChanged(boolean: Boolean; int_1: Integer); cdecl;
    procedure onPlaybackParametersChanged(playbackparameters: JPlaybackParameters); cdecl;
    procedure onPlaybackStateChanged(int: Integer); cdecl;
    procedure onPlaybackSuppressionReasonChanged(int: Integer); cdecl;
    procedure onPlayerError(playbackexception: JPlaybackException); cdecl;
    procedure onPlayerErrorChanged(playbackexception: JPlaybackException); cdecl;
    procedure onPlayerStateChanged(boolean: Boolean; int_1: Integer); cdecl;
    procedure onPlaylistMetadataChanged(mediametadata: JMediaMetadata); cdecl;
    procedure onPositionDiscontinuity(positioninfo: JPlayer_PositionInfo; positioninfo_1: JPlayer_PositionInfo; int: Integer); cdecl; overload;
    procedure onPositionDiscontinuity(int: Integer); cdecl; overload;
    procedure onRenderedFirstFrame; cdecl;
    procedure onRepeatModeChanged(int: Integer); cdecl;
    procedure onSeekBackIncrementChanged(long: Int64); cdecl;
    procedure onSeekForwardIncrementChanged(long: Int64); cdecl;
    procedure onShuffleModeEnabledChanged(boolean: Boolean); cdecl;
    procedure onSkipSilenceEnabledChanged(boolean: Boolean); cdecl;
    procedure onSurfaceSizeChanged(int: Integer; int_1: Integer); cdecl;
    procedure onTimelineChanged(timeline: JTimeline; int: Integer); cdecl;
    procedure onTrackSelectionParametersChanged(trackselectionparameters: JTrackSelectionParameters); cdecl;
    procedure onTracksChanged(tracks: JTracks); cdecl;
    procedure onVideoSizeChanged(videosize: JVideoSize); cdecl;
    procedure onVolumeChanged(float: Single); cdecl;
  end;
  TJPlayer_Listener = class(TJavaGenericImport<JPlayer_ListenerClass, JPlayer_Listener>) end;

  JPlayer_EventsClass = interface(JObjectClass)
    ['{5A994FA3-DE5C-460D-B280-2576C72CCDFD}']
    {class} function init(flagset: JFlagSet): JPlayer_Events; cdecl;
  end;

  [JavaSignature('androidx/media3/common/Player$Events')]
  JPlayer_Events = interface(JObject)
    ['{D88DCC2A-80EB-4B8E-84D0-B0D476461349}']
    function contains(int: Integer): Boolean; cdecl;
    function containsAny(int: Integer): Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(int: Integer): Integer; cdecl;
    function hashCode: Integer; cdecl;
    function size: Integer; cdecl;
  end;
  TJPlayer_Events = class(TJavaGenericImport<JPlayer_EventsClass, JPlayer_Events>) end;

  JPlayer_CommandsClass = interface(JObjectClass)
    ['{55D16CAA-C8E1-408B-B38E-40AAC1F1891F}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetEMPTY: JPlayer_Commands; cdecl;
    {class} function fromBundle(bundle: JBundle): JPlayer_Commands; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property EMPTY: JPlayer_Commands read _GetEMPTY;
  end;

  [JavaSignature('androidx/media3/common/Player$Commands')]
  JPlayer_Commands = interface(JObject)
    ['{40C6EFEB-10D8-4203-B84E-0A331F18A9BC}']
    function buildUpon: JPlayer_Commands_Builder; cdecl;
    function contains(int: Integer): Boolean; cdecl;
    function containsAny(int: Integer): Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(int: Integer): Integer; cdecl;
    function hashCode: Integer; cdecl;
    function size: Integer; cdecl;
    function toBundle: JBundle; cdecl;
  end;
  TJPlayer_Commands = class(TJavaGenericImport<JPlayer_CommandsClass, JPlayer_Commands>) end;

  JPlayer_Commands_BuilderClass = interface(JObjectClass)
    ['{8CFEB8EF-0A78-4A00-9177-BE08937A56AC}']
    {class} function init: JPlayer_Commands_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/Player$Commands$Builder')]
  JPlayer_Commands_Builder = interface(JObject)
    ['{83A9466A-EBDD-4687-A942-B35A92A7B449}']
    function add(int: Integer): JPlayer_Commands_Builder; cdecl;
    function addAll(commands: JPlayer_Commands): JPlayer_Commands_Builder; cdecl; overload;
    function addAll(int: Integer): JPlayer_Commands_Builder; cdecl; overload;
    function addAllCommands: JPlayer_Commands_Builder; cdecl;
    function addIf(int: Integer; boolean: Boolean): JPlayer_Commands_Builder; cdecl;
    function build: JPlayer_Commands; cdecl;
    function remove(int: Integer): JPlayer_Commands_Builder; cdecl;
    function removeAll(int: Integer): JPlayer_Commands_Builder; cdecl;
    function removeIf(int: Integer; boolean: Boolean): JPlayer_Commands_Builder; cdecl;
  end;
  TJPlayer_Commands_Builder = class(TJavaGenericImport<JPlayer_Commands_BuilderClass, JPlayer_Commands_Builder>) end;

  JPlaybackParametersClass = interface(JObjectClass)
    ['{E72C3FC4-FAF3-446B-B619-345E6D5550AF}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetDEFAULT: JPlaybackParameters; cdecl;
    {class} function fromBundle(bundle: JBundle): JPlaybackParameters; cdecl;
    {class} function init(float: Single): JPlaybackParameters; cdecl; overload;
    {class} function init(float: Single; float_1: Single): JPlaybackParameters; cdecl; overload;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property &DEFAULT: JPlaybackParameters read _GetDEFAULT;
  end;

  [JavaSignature('androidx/media3/common/PlaybackParameters')]
  JPlaybackParameters = interface(JObject)
    ['{B1E69525-FB99-4548-9AE6-A9A9B4370B8E}']
    function _Getpitch: Single; cdecl;
    function _Getspeed: Single; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getMediaTimeUsForPlayoutTimeMs(long: Int64): Int64; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    function toString: JString; cdecl;
    function withSpeed(float: Single): JPlaybackParameters; cdecl;
    property pitch: Single read _Getpitch;
    property speed: Single read _Getspeed;
  end;
  TJPlaybackParameters = class(TJavaGenericImport<JPlaybackParametersClass, JPlaybackParameters>) end;

  JPlaybackExceptionClass = interface(JExceptionClass)
    ['{484DD65F-96E2-47EF-B1D4-36C365E4D245}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetCUSTOM_ERROR_CODE_BASE: Integer; cdecl;
    {class} function _GetERROR_CODE_AUDIO_TRACK_INIT_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_AUDIO_TRACK_OFFLOAD_WRITE_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_AUDIO_TRACK_WRITE_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_BEHIND_LIVE_WINDOW: Integer; cdecl;
    {class} function _GetERROR_CODE_DECODER_INIT_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_DECODER_QUERY_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_DECODING_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_DECODING_FORMAT_EXCEEDS_CAPABILITIES: Integer; cdecl;
    {class} function _GetERROR_CODE_DECODING_FORMAT_UNSUPPORTED: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_CONTENT_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_DEVICE_REVOKED: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_DISALLOWED_OPERATION: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_LICENSE_ACQUISITION_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_LICENSE_EXPIRED: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_PROVISIONING_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_SCHEME_UNSUPPORTED: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_SYSTEM_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_DRM_UNSPECIFIED: Integer; cdecl;
    {class} function _GetERROR_CODE_FAILED_RUNTIME_CHECK: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_BAD_HTTP_STATUS: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_CLEARTEXT_NOT_PERMITTED: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_FILE_NOT_FOUND: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_INVALID_HTTP_CONTENT_TYPE: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_NETWORK_CONNECTION_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_NETWORK_CONNECTION_TIMEOUT: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_NO_PERMISSION: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_READ_POSITION_OUT_OF_RANGE: Integer; cdecl;
    {class} function _GetERROR_CODE_IO_UNSPECIFIED: Integer; cdecl;
    {class} function _GetERROR_CODE_PARSING_CONTAINER_MALFORMED: Integer; cdecl;
    {class} function _GetERROR_CODE_PARSING_CONTAINER_UNSUPPORTED: Integer; cdecl;
    {class} function _GetERROR_CODE_PARSING_MANIFEST_MALFORMED: Integer; cdecl;
    {class} function _GetERROR_CODE_PARSING_MANIFEST_UNSUPPORTED: Integer; cdecl;
    {class} function _GetERROR_CODE_REMOTE_ERROR: Integer; cdecl;
    {class} function _GetERROR_CODE_TIMEOUT: Integer; cdecl;
    {class} function _GetERROR_CODE_UNSPECIFIED: Integer; cdecl;
    {class} function _GetERROR_CODE_VIDEO_FRAME_PROCESSING_FAILED: Integer; cdecl;
    {class} function _GetERROR_CODE_VIDEO_FRAME_PROCESSOR_INIT_FAILED: Integer; cdecl;
    {class} function fromBundle(bundle: JBundle): JPlaybackException; cdecl;
    {class} function getErrorCodeName(int: Integer): JString; cdecl; overload;
    {class} function init(string_1: JString; throwable: JThrowable; int: Integer): JPlaybackException; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property CUSTOM_ERROR_CODE_BASE: Integer read _GetCUSTOM_ERROR_CODE_BASE;
    {class} property ERROR_CODE_AUDIO_TRACK_INIT_FAILED: Integer read _GetERROR_CODE_AUDIO_TRACK_INIT_FAILED;
    {class} property ERROR_CODE_AUDIO_TRACK_OFFLOAD_WRITE_FAILED: Integer read _GetERROR_CODE_AUDIO_TRACK_OFFLOAD_WRITE_FAILED;
    {class} property ERROR_CODE_AUDIO_TRACK_WRITE_FAILED: Integer read _GetERROR_CODE_AUDIO_TRACK_WRITE_FAILED;
    {class} property ERROR_CODE_BEHIND_LIVE_WINDOW: Integer read _GetERROR_CODE_BEHIND_LIVE_WINDOW;
    {class} property ERROR_CODE_DECODER_INIT_FAILED: Integer read _GetERROR_CODE_DECODER_INIT_FAILED;
    {class} property ERROR_CODE_DECODER_QUERY_FAILED: Integer read _GetERROR_CODE_DECODER_QUERY_FAILED;
    {class} property ERROR_CODE_DECODING_FAILED: Integer read _GetERROR_CODE_DECODING_FAILED;
    {class} property ERROR_CODE_DECODING_FORMAT_EXCEEDS_CAPABILITIES: Integer read _GetERROR_CODE_DECODING_FORMAT_EXCEEDS_CAPABILITIES;
    {class} property ERROR_CODE_DECODING_FORMAT_UNSUPPORTED: Integer read _GetERROR_CODE_DECODING_FORMAT_UNSUPPORTED;
    {class} property ERROR_CODE_DRM_CONTENT_ERROR: Integer read _GetERROR_CODE_DRM_CONTENT_ERROR;
    {class} property ERROR_CODE_DRM_DEVICE_REVOKED: Integer read _GetERROR_CODE_DRM_DEVICE_REVOKED;
    {class} property ERROR_CODE_DRM_DISALLOWED_OPERATION: Integer read _GetERROR_CODE_DRM_DISALLOWED_OPERATION;
    {class} property ERROR_CODE_DRM_LICENSE_ACQUISITION_FAILED: Integer read _GetERROR_CODE_DRM_LICENSE_ACQUISITION_FAILED;
    {class} property ERROR_CODE_DRM_LICENSE_EXPIRED: Integer read _GetERROR_CODE_DRM_LICENSE_EXPIRED;
    {class} property ERROR_CODE_DRM_PROVISIONING_FAILED: Integer read _GetERROR_CODE_DRM_PROVISIONING_FAILED;
    {class} property ERROR_CODE_DRM_SCHEME_UNSUPPORTED: Integer read _GetERROR_CODE_DRM_SCHEME_UNSUPPORTED;
    {class} property ERROR_CODE_DRM_SYSTEM_ERROR: Integer read _GetERROR_CODE_DRM_SYSTEM_ERROR;
    {class} property ERROR_CODE_DRM_UNSPECIFIED: Integer read _GetERROR_CODE_DRM_UNSPECIFIED;
    {class} property ERROR_CODE_FAILED_RUNTIME_CHECK: Integer read _GetERROR_CODE_FAILED_RUNTIME_CHECK;
    {class} property ERROR_CODE_IO_BAD_HTTP_STATUS: Integer read _GetERROR_CODE_IO_BAD_HTTP_STATUS;
    {class} property ERROR_CODE_IO_CLEARTEXT_NOT_PERMITTED: Integer read _GetERROR_CODE_IO_CLEARTEXT_NOT_PERMITTED;
    {class} property ERROR_CODE_IO_FILE_NOT_FOUND: Integer read _GetERROR_CODE_IO_FILE_NOT_FOUND;
    {class} property ERROR_CODE_IO_INVALID_HTTP_CONTENT_TYPE: Integer read _GetERROR_CODE_IO_INVALID_HTTP_CONTENT_TYPE;
    {class} property ERROR_CODE_IO_NETWORK_CONNECTION_FAILED: Integer read _GetERROR_CODE_IO_NETWORK_CONNECTION_FAILED;
    {class} property ERROR_CODE_IO_NETWORK_CONNECTION_TIMEOUT: Integer read _GetERROR_CODE_IO_NETWORK_CONNECTION_TIMEOUT;
    {class} property ERROR_CODE_IO_NO_PERMISSION: Integer read _GetERROR_CODE_IO_NO_PERMISSION;
    {class} property ERROR_CODE_IO_READ_POSITION_OUT_OF_RANGE: Integer read _GetERROR_CODE_IO_READ_POSITION_OUT_OF_RANGE;
    {class} property ERROR_CODE_IO_UNSPECIFIED: Integer read _GetERROR_CODE_IO_UNSPECIFIED;
    {class} property ERROR_CODE_PARSING_CONTAINER_MALFORMED: Integer read _GetERROR_CODE_PARSING_CONTAINER_MALFORMED;
    {class} property ERROR_CODE_PARSING_CONTAINER_UNSUPPORTED: Integer read _GetERROR_CODE_PARSING_CONTAINER_UNSUPPORTED;
    {class} property ERROR_CODE_PARSING_MANIFEST_MALFORMED: Integer read _GetERROR_CODE_PARSING_MANIFEST_MALFORMED;
    {class} property ERROR_CODE_PARSING_MANIFEST_UNSUPPORTED: Integer read _GetERROR_CODE_PARSING_MANIFEST_UNSUPPORTED;
    {class} property ERROR_CODE_REMOTE_ERROR: Integer read _GetERROR_CODE_REMOTE_ERROR;
    {class} property ERROR_CODE_TIMEOUT: Integer read _GetERROR_CODE_TIMEOUT;
    {class} property ERROR_CODE_UNSPECIFIED: Integer read _GetERROR_CODE_UNSPECIFIED;
    {class} property ERROR_CODE_VIDEO_FRAME_PROCESSING_FAILED: Integer read _GetERROR_CODE_VIDEO_FRAME_PROCESSING_FAILED;
    {class} property ERROR_CODE_VIDEO_FRAME_PROCESSOR_INIT_FAILED: Integer read _GetERROR_CODE_VIDEO_FRAME_PROCESSOR_INIT_FAILED;
  end;

  [JavaSignature('androidx/media3/common/PlaybackException')]
  JPlaybackException = interface(JException)
    ['{C104A27A-154B-4A96-B77D-225D42571E7B}']
    function _GeterrorCode: Integer; cdecl;
    function _GettimestampMs: Int64; cdecl;
    function errorInfoEquals(playbackexception: JPlaybackException): Boolean; cdecl;
    function getErrorCodeName: JString; cdecl; overload;
    function toBundle: JBundle; cdecl;
    property errorCode: Integer read _GeterrorCode;
    property timestampMs: Int64 read _GettimestampMs;
  end;
  TJPlaybackException = class(TJavaGenericImport<JPlaybackExceptionClass, JPlaybackException>) end;

  JMetadataClass = interface(JObjectClass)
    ['{2105EEA4-BD8C-494A-8D99-9B1BC6762BFC}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(long: Int64; entry: JMetadata_Entry): JMetadata; cdecl; overload;
    {class} function init(entry: JMetadata_Entry): JMetadata; cdecl; overload;
    {class} function init(list: JList): JMetadata; cdecl; overload;
    {class} function init(long: Int64; list: JList): JMetadata; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/Metadata')]
  JMetadata = interface(JObject)
    ['{054FF7EE-5BDE-4FE0-875F-A88FE2E9AEC3}']
    function _GetpresentationTimeUs: Int64; cdecl;
    function copyWithAppendedEntries(entry: JMetadata_Entry): JMetadata; cdecl;
    function copyWithAppendedEntriesFrom(metadata: JMetadata): JMetadata; cdecl;
    function copyWithPresentationTimeUs(long: Int64): JMetadata; cdecl;
    function describeContents: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(int: Integer): JMetadata_Entry; cdecl;
    function hashCode: Integer; cdecl;
    function length: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
    property presentationTimeUs: Int64 read _GetpresentationTimeUs;
  end;
  TJMetadata = class(TJavaGenericImport<JMetadataClass, JMetadata>) end;

  JMetadata_EntryClass = interface(JParcelableClass)
    ['{C29F14DC-CA41-423B-8DF5-38FD83B04C8A}']
  end;

  [JavaSignature('androidx/media3/common/Metadata$Entry')]
  JMetadata_Entry = interface(JParcelable)
    ['{EEA1B275-199F-4AFF-9E43-825EC31F53F0}']
    function getWrappedMetadataBytes: TJavaArray<Byte>; cdecl;
    function getWrappedMetadataFormat: JFormat; cdecl;
    procedure populateMediaMetadata(builder: JMediaMetadata_Builder); cdecl;
  end;
  TJMetadata_Entry = class(TJavaGenericImport<JMetadata_EntryClass, JMetadata_Entry>) end;

  JMediaMetadataClass = interface(JObjectClass)
    ['{DD2CFE25-2343-43ED-96D1-913E6B193645}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetEMPTY: JMediaMetadata; cdecl;
    {class} function _GetFOLDER_TYPE_ALBUMS: Integer; cdecl;
    {class} function _GetFOLDER_TYPE_ARTISTS: Integer; cdecl;
    {class} function _GetFOLDER_TYPE_GENRES: Integer; cdecl;
    {class} function _GetFOLDER_TYPE_MIXED: Integer; cdecl;
    {class} function _GetFOLDER_TYPE_NONE: Integer; cdecl;
    {class} function _GetFOLDER_TYPE_PLAYLISTS: Integer; cdecl;
    {class} function _GetFOLDER_TYPE_TITLES: Integer; cdecl;
    {class} function _GetFOLDER_TYPE_YEARS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_ALBUM: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_ARTIST: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_AUDIO_BOOK: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_AUDIO_BOOK_CHAPTER: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_ALBUMS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_ARTISTS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_AUDIO_BOOKS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_GENRES: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_MIXED: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_MOVIES: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_NEWS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_PLAYLISTS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_PODCASTS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_RADIO_STATIONS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_TRAILERS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_TV_CHANNELS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_TV_SERIES: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_TV_SHOWS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_VIDEOS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_FOLDER_YEARS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_GENRE: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_MIXED: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_MOVIE: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_MUSIC: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_NEWS: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_PLAYLIST: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_PODCAST: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_PODCAST_EPISODE: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_RADIO_STATION: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_TRAILER: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_TV_CHANNEL: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_TV_SEASON: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_TV_SERIES: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_TV_SHOW: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_VIDEO: Integer; cdecl;
    {class} function _GetMEDIA_TYPE_YEAR: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_ARTIST_PERFORMER: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_A_BRIGHT_COLORED_FISH: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_BACK_COVER: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_BAND_ARTIST_LOGO: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_BAND_ORCHESTRA: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_COMPOSER: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_CONDUCTOR: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_DURING_PERFORMANCE: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_DURING_RECORDING: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_FILE_ICON: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_FILE_ICON_OTHER: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_FRONT_COVER: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_ILLUSTRATION: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_LEAD_ARTIST_PERFORMER: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_LEAFLET_PAGE: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_LYRICIST: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_MEDIA: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_MOVIE_VIDEO_SCREEN_CAPTURE: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_OTHER: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_PUBLISHER_STUDIO_LOGO: Integer; cdecl;
    {class} function _GetPICTURE_TYPE_RECORDING_LOCATION: Integer; cdecl;
    {class} function fromBundle(bundle: JBundle): JMediaMetadata; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property EMPTY: JMediaMetadata read _GetEMPTY;
    {class} property FOLDER_TYPE_ALBUMS: Integer read _GetFOLDER_TYPE_ALBUMS;
    {class} property FOLDER_TYPE_ARTISTS: Integer read _GetFOLDER_TYPE_ARTISTS;
    {class} property FOLDER_TYPE_GENRES: Integer read _GetFOLDER_TYPE_GENRES;
    {class} property FOLDER_TYPE_MIXED: Integer read _GetFOLDER_TYPE_MIXED;
    {class} property FOLDER_TYPE_NONE: Integer read _GetFOLDER_TYPE_NONE;
    {class} property FOLDER_TYPE_PLAYLISTS: Integer read _GetFOLDER_TYPE_PLAYLISTS;
    {class} property FOLDER_TYPE_TITLES: Integer read _GetFOLDER_TYPE_TITLES;
    {class} property FOLDER_TYPE_YEARS: Integer read _GetFOLDER_TYPE_YEARS;
    {class} property MEDIA_TYPE_ALBUM: Integer read _GetMEDIA_TYPE_ALBUM;
    {class} property MEDIA_TYPE_ARTIST: Integer read _GetMEDIA_TYPE_ARTIST;
    {class} property MEDIA_TYPE_AUDIO_BOOK: Integer read _GetMEDIA_TYPE_AUDIO_BOOK;
    {class} property MEDIA_TYPE_AUDIO_BOOK_CHAPTER: Integer read _GetMEDIA_TYPE_AUDIO_BOOK_CHAPTER;
    {class} property MEDIA_TYPE_FOLDER_ALBUMS: Integer read _GetMEDIA_TYPE_FOLDER_ALBUMS;
    {class} property MEDIA_TYPE_FOLDER_ARTISTS: Integer read _GetMEDIA_TYPE_FOLDER_ARTISTS;
    {class} property MEDIA_TYPE_FOLDER_AUDIO_BOOKS: Integer read _GetMEDIA_TYPE_FOLDER_AUDIO_BOOKS;
    {class} property MEDIA_TYPE_FOLDER_GENRES: Integer read _GetMEDIA_TYPE_FOLDER_GENRES;
    {class} property MEDIA_TYPE_FOLDER_MIXED: Integer read _GetMEDIA_TYPE_FOLDER_MIXED;
    {class} property MEDIA_TYPE_FOLDER_MOVIES: Integer read _GetMEDIA_TYPE_FOLDER_MOVIES;
    {class} property MEDIA_TYPE_FOLDER_NEWS: Integer read _GetMEDIA_TYPE_FOLDER_NEWS;
    {class} property MEDIA_TYPE_FOLDER_PLAYLISTS: Integer read _GetMEDIA_TYPE_FOLDER_PLAYLISTS;
    {class} property MEDIA_TYPE_FOLDER_PODCASTS: Integer read _GetMEDIA_TYPE_FOLDER_PODCASTS;
    {class} property MEDIA_TYPE_FOLDER_RADIO_STATIONS: Integer read _GetMEDIA_TYPE_FOLDER_RADIO_STATIONS;
    {class} property MEDIA_TYPE_FOLDER_TRAILERS: Integer read _GetMEDIA_TYPE_FOLDER_TRAILERS;
    {class} property MEDIA_TYPE_FOLDER_TV_CHANNELS: Integer read _GetMEDIA_TYPE_FOLDER_TV_CHANNELS;
    {class} property MEDIA_TYPE_FOLDER_TV_SERIES: Integer read _GetMEDIA_TYPE_FOLDER_TV_SERIES;
    {class} property MEDIA_TYPE_FOLDER_TV_SHOWS: Integer read _GetMEDIA_TYPE_FOLDER_TV_SHOWS;
    {class} property MEDIA_TYPE_FOLDER_VIDEOS: Integer read _GetMEDIA_TYPE_FOLDER_VIDEOS;
    {class} property MEDIA_TYPE_FOLDER_YEARS: Integer read _GetMEDIA_TYPE_FOLDER_YEARS;
    {class} property MEDIA_TYPE_GENRE: Integer read _GetMEDIA_TYPE_GENRE;
    {class} property MEDIA_TYPE_MIXED: Integer read _GetMEDIA_TYPE_MIXED;
    {class} property MEDIA_TYPE_MOVIE: Integer read _GetMEDIA_TYPE_MOVIE;
    {class} property MEDIA_TYPE_MUSIC: Integer read _GetMEDIA_TYPE_MUSIC;
    {class} property MEDIA_TYPE_NEWS: Integer read _GetMEDIA_TYPE_NEWS;
    {class} property MEDIA_TYPE_PLAYLIST: Integer read _GetMEDIA_TYPE_PLAYLIST;
    {class} property MEDIA_TYPE_PODCAST: Integer read _GetMEDIA_TYPE_PODCAST;
    {class} property MEDIA_TYPE_PODCAST_EPISODE: Integer read _GetMEDIA_TYPE_PODCAST_EPISODE;
    {class} property MEDIA_TYPE_RADIO_STATION: Integer read _GetMEDIA_TYPE_RADIO_STATION;
    {class} property MEDIA_TYPE_TRAILER: Integer read _GetMEDIA_TYPE_TRAILER;
    {class} property MEDIA_TYPE_TV_CHANNEL: Integer read _GetMEDIA_TYPE_TV_CHANNEL;
    {class} property MEDIA_TYPE_TV_SEASON: Integer read _GetMEDIA_TYPE_TV_SEASON;
    {class} property MEDIA_TYPE_TV_SERIES: Integer read _GetMEDIA_TYPE_TV_SERIES;
    {class} property MEDIA_TYPE_TV_SHOW: Integer read _GetMEDIA_TYPE_TV_SHOW;
    {class} property MEDIA_TYPE_VIDEO: Integer read _GetMEDIA_TYPE_VIDEO;
    {class} property MEDIA_TYPE_YEAR: Integer read _GetMEDIA_TYPE_YEAR;
    {class} property PICTURE_TYPE_ARTIST_PERFORMER: Integer read _GetPICTURE_TYPE_ARTIST_PERFORMER;
    {class} property PICTURE_TYPE_A_BRIGHT_COLORED_FISH: Integer read _GetPICTURE_TYPE_A_BRIGHT_COLORED_FISH;
    {class} property PICTURE_TYPE_BACK_COVER: Integer read _GetPICTURE_TYPE_BACK_COVER;
    {class} property PICTURE_TYPE_BAND_ARTIST_LOGO: Integer read _GetPICTURE_TYPE_BAND_ARTIST_LOGO;
    {class} property PICTURE_TYPE_BAND_ORCHESTRA: Integer read _GetPICTURE_TYPE_BAND_ORCHESTRA;
    {class} property PICTURE_TYPE_COMPOSER: Integer read _GetPICTURE_TYPE_COMPOSER;
    {class} property PICTURE_TYPE_CONDUCTOR: Integer read _GetPICTURE_TYPE_CONDUCTOR;
    {class} property PICTURE_TYPE_DURING_PERFORMANCE: Integer read _GetPICTURE_TYPE_DURING_PERFORMANCE;
    {class} property PICTURE_TYPE_DURING_RECORDING: Integer read _GetPICTURE_TYPE_DURING_RECORDING;
    {class} property PICTURE_TYPE_FILE_ICON: Integer read _GetPICTURE_TYPE_FILE_ICON;
    {class} property PICTURE_TYPE_FILE_ICON_OTHER: Integer read _GetPICTURE_TYPE_FILE_ICON_OTHER;
    {class} property PICTURE_TYPE_FRONT_COVER: Integer read _GetPICTURE_TYPE_FRONT_COVER;
    {class} property PICTURE_TYPE_ILLUSTRATION: Integer read _GetPICTURE_TYPE_ILLUSTRATION;
    {class} property PICTURE_TYPE_LEAD_ARTIST_PERFORMER: Integer read _GetPICTURE_TYPE_LEAD_ARTIST_PERFORMER;
    {class} property PICTURE_TYPE_LEAFLET_PAGE: Integer read _GetPICTURE_TYPE_LEAFLET_PAGE;
    {class} property PICTURE_TYPE_LYRICIST: Integer read _GetPICTURE_TYPE_LYRICIST;
    {class} property PICTURE_TYPE_MEDIA: Integer read _GetPICTURE_TYPE_MEDIA;
    {class} property PICTURE_TYPE_MOVIE_VIDEO_SCREEN_CAPTURE: Integer read _GetPICTURE_TYPE_MOVIE_VIDEO_SCREEN_CAPTURE;
    {class} property PICTURE_TYPE_OTHER: Integer read _GetPICTURE_TYPE_OTHER;
    {class} property PICTURE_TYPE_PUBLISHER_STUDIO_LOGO: Integer read _GetPICTURE_TYPE_PUBLISHER_STUDIO_LOGO;
    {class} property PICTURE_TYPE_RECORDING_LOCATION: Integer read _GetPICTURE_TYPE_RECORDING_LOCATION;
  end;

  [JavaSignature('androidx/media3/common/MediaMetadata')]
  JMediaMetadata = interface(JObject)
    ['{865C3761-CD76-49A2-AA77-AD293F5B691B}']
    function _GetalbumArtist: JCharSequence; cdecl;
    function _GetalbumTitle: JCharSequence; cdecl;
    function _Getartist: JCharSequence; cdecl;
    function _GetartworkData: TJavaArray<Byte>; cdecl;
    function _GetartworkDataType: JInteger; cdecl;
    function _GetartworkUri: Jnet_Uri; cdecl;
    function _Getcompilation: JCharSequence; cdecl;
    function _Getcomposer: JCharSequence; cdecl;
    function _Getconductor: JCharSequence; cdecl;
    function _Getdescription: JCharSequence; cdecl;
    function _GetdiscNumber: JInteger; cdecl;
    function _GetdisplayTitle: JCharSequence; cdecl;
    function _Getextras: JBundle; cdecl;
    function _GetfolderType: JInteger; cdecl;
    function _Getgenre: JCharSequence; cdecl;
    function _GetisBrowsable: JBoolean; cdecl;
    function _GetisPlayable: JBoolean; cdecl;
    function _GetmediaType: JInteger; cdecl;
    function _GetoverallRating: JRating; cdecl;
    function _GetrecordingDay: JInteger; cdecl;
    function _GetrecordingMonth: JInteger; cdecl;
    function _GetrecordingYear: JInteger; cdecl;
    function _GetreleaseDay: JInteger; cdecl;
    function _GetreleaseMonth: JInteger; cdecl;
    function _GetreleaseYear: JInteger; cdecl;
    function _Getstation: JCharSequence; cdecl;
    function _Getsubtitle: JCharSequence; cdecl;
    function _Gettitle: JCharSequence; cdecl;
    function _GettotalDiscCount: JInteger; cdecl;
    function _GettotalTrackCount: JInteger; cdecl;
    function _GettrackNumber: JInteger; cdecl;
    function _GetuserRating: JRating; cdecl;
    function _Getwriter: JCharSequence; cdecl;
    function _Getyear: JInteger; cdecl;
    function buildUpon: JMediaMetadata_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property albumArtist: JCharSequence read _GetalbumArtist;
    property albumTitle: JCharSequence read _GetalbumTitle;
    property artist: JCharSequence read _Getartist;
    property artworkData: TJavaArray<Byte> read _GetartworkData;
    property artworkDataType: JInteger read _GetartworkDataType;
    property artworkUri: Jnet_Uri read _GetartworkUri;
    property compilation: JCharSequence read _Getcompilation;
    property composer: JCharSequence read _Getcomposer;
    property conductor: JCharSequence read _Getconductor;
    property description: JCharSequence read _Getdescription;
    property discNumber: JInteger read _GetdiscNumber;
    property displayTitle: JCharSequence read _GetdisplayTitle;
    property extras: JBundle read _Getextras;
    property folderType: JInteger read _GetfolderType;
    property genre: JCharSequence read _Getgenre;
    property isBrowsable: JBoolean read _GetisBrowsable;
    property isPlayable: JBoolean read _GetisPlayable;
    property mediaType: JInteger read _GetmediaType;
    property overallRating: JRating read _GetoverallRating;
    property recordingDay: JInteger read _GetrecordingDay;
    property recordingMonth: JInteger read _GetrecordingMonth;
    property recordingYear: JInteger read _GetrecordingYear;
    property releaseDay: JInteger read _GetreleaseDay;
    property releaseMonth: JInteger read _GetreleaseMonth;
    property releaseYear: JInteger read _GetreleaseYear;
    property station: JCharSequence read _Getstation;
    property subtitle: JCharSequence read _Getsubtitle;
    property title: JCharSequence read _Gettitle;
    property totalDiscCount: JInteger read _GettotalDiscCount;
    property totalTrackCount: JInteger read _GettotalTrackCount;
    property trackNumber: JInteger read _GettrackNumber;
    property userRating: JRating read _GetuserRating;
    property writer: JCharSequence read _Getwriter;
    property year: JInteger read _Getyear;
  end;
  TJMediaMetadata = class(TJavaGenericImport<JMediaMetadataClass, JMediaMetadata>) end;

  JMediaMetadata_BuilderClass = interface(JObjectClass)
    ['{428151AD-DE9E-4186-B321-4DCA9C315664}']
    {class} function init: JMediaMetadata_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/MediaMetadata$Builder')]
  JMediaMetadata_Builder = interface(JObject)
    ['{25275C81-FFB4-4C54-9C62-29D2E063DC75}']
    function build: JMediaMetadata; cdecl;
    function maybeSetArtworkData(bytes: TJavaArray<Byte>; int: Integer): JMediaMetadata_Builder; cdecl;
    function populate(mediametadata: JMediaMetadata): JMediaMetadata_Builder; cdecl;
    function populateFromMetadata(list: JList): JMediaMetadata_Builder; cdecl; overload;
    function populateFromMetadata(metadata: JMetadata): JMediaMetadata_Builder; cdecl; overload;
    function setAlbumArtist(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setAlbumTitle(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setArtist(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setArtworkData(bytes: TJavaArray<Byte>): JMediaMetadata_Builder; cdecl; overload;
    function setArtworkData(bytes: TJavaArray<Byte>; integer: JInteger): JMediaMetadata_Builder; cdecl; overload;
    function setArtworkUri(uri: Jnet_Uri): JMediaMetadata_Builder; cdecl;
    function setCompilation(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setComposer(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setConductor(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setDescription(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setDiscNumber(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setDisplayTitle(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setExtras(bundle: JBundle): JMediaMetadata_Builder; cdecl;
    function setFolderType(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setGenre(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setIsBrowsable(boolean: JBoolean): JMediaMetadata_Builder; cdecl;
    function setIsPlayable(boolean: JBoolean): JMediaMetadata_Builder; cdecl;
    function setMediaType(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setOverallRating(rating: JRating): JMediaMetadata_Builder; cdecl;
    function setRecordingDay(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setRecordingMonth(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setRecordingYear(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setReleaseDay(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setReleaseMonth(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setReleaseYear(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setStation(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setSubtitle(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setTitle(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setTotalDiscCount(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setTotalTrackCount(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setTrackNumber(integer: JInteger): JMediaMetadata_Builder; cdecl;
    function setUserRating(rating: JRating): JMediaMetadata_Builder; cdecl;
    function setWriter(charsequence: JCharSequence): JMediaMetadata_Builder; cdecl;
    function setYear(integer: JInteger): JMediaMetadata_Builder; cdecl;
  end;
  TJMediaMetadata_Builder = class(TJavaGenericImport<JMediaMetadata_BuilderClass, JMediaMetadata_Builder>) end;

  JMediaItemClass = interface(JObjectClass)
    ['{A3FCFB33-97FA-497B-B2E4-DCCBD0CA6F64}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetDEFAULT_MEDIA_ID: JString; cdecl;
    {class} function _GetEMPTY: JMediaItem; cdecl;
    {class} function fromBundle(bundle: JBundle): JMediaItem; cdecl;
    {class} function fromUri(uri: Jnet_Uri): JMediaItem; cdecl; overload;
    {class} function fromUri(string_1: JString): JMediaItem; cdecl; overload;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property DEFAULT_MEDIA_ID: JString read _GetDEFAULT_MEDIA_ID;
    {class} property EMPTY: JMediaItem read _GetEMPTY;
  end;

  [JavaSignature('androidx/media3/common/MediaItem')]
  JMediaItem = interface(JObject)
    ['{79B115CC-0793-47CD-BA63-258477766499}']
    function _GetclippingConfiguration: JMediaItem_ClippingConfiguration; cdecl;
    function _GetclippingProperties: JMediaItem_ClippingProperties; cdecl;
    function _GetliveConfiguration: JMediaItem_LiveConfiguration; cdecl;
    function _GetlocalConfiguration: JMediaItem_LocalConfiguration; cdecl;
    function _GetmediaId: JString; cdecl;
    function _GetmediaMetadata: JMediaMetadata; cdecl;
    function _GetplaybackProperties: JMediaItem_LocalConfiguration; cdecl;
    function _GetrequestMetadata: JMediaItem_RequestMetadata; cdecl;
    function buildUpon: JMediaItem_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    function toBundleIncludeLocalConfiguration: JBundle; cdecl;
    property clippingConfiguration: JMediaItem_ClippingConfiguration read _GetclippingConfiguration;
    property clippingProperties: JMediaItem_ClippingProperties read _GetclippingProperties;
    property liveConfiguration: JMediaItem_LiveConfiguration read _GetliveConfiguration;
    property localConfiguration: JMediaItem_LocalConfiguration read _GetlocalConfiguration;
    property mediaId: JString read _GetmediaId;
    property mediaMetadata: JMediaMetadata read _GetmediaMetadata;
    property playbackProperties: JMediaItem_LocalConfiguration read _GetplaybackProperties;
    property requestMetadata: JMediaItem_RequestMetadata read _GetrequestMetadata;
  end;
  TJMediaItem = class(TJavaGenericImport<JMediaItemClass, JMediaItem>) end;

  JMediaItem_RequestMetadataClass = interface(JObjectClass)
    ['{3D22F22B-19D9-4CF9-BAFA-C051E6CE3D4C}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetEMPTY: JMediaItem_RequestMetadata; cdecl;
    {class} function fromBundle(bundle: JBundle): JMediaItem_RequestMetadata; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property EMPTY: JMediaItem_RequestMetadata read _GetEMPTY;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$RequestMetadata')]
  JMediaItem_RequestMetadata = interface(JObject)
    ['{52C802DC-3D66-49AC-B33C-D0385262CFE7}']
    function _Getextras: JBundle; cdecl;
    function _GetmediaUri: Jnet_Uri; cdecl;
    function _GetsearchQuery: JString; cdecl;
    function buildUpon: JMediaItem_RequestMetadata_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property extras: JBundle read _Getextras;
    property mediaUri: Jnet_Uri read _GetmediaUri;
    property searchQuery: JString read _GetsearchQuery;
  end;
  TJMediaItem_RequestMetadata = class(TJavaGenericImport<JMediaItem_RequestMetadataClass, JMediaItem_RequestMetadata>) end;

  JMediaItem_RequestMetadata_BuilderClass = interface(JObjectClass)
    ['{5E0EF6EA-EE69-4096-92B5-D03A97D5527A}']
    {class} function init: JMediaItem_RequestMetadata_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$RequestMetadata$Builder')]
  JMediaItem_RequestMetadata_Builder = interface(JObject)
    ['{2786DD81-94FC-48C4-84D3-1B815071F34A}']
    function build: JMediaItem_RequestMetadata; cdecl;
    function setExtras(bundle: JBundle): JMediaItem_RequestMetadata_Builder; cdecl;
    function setMediaUri(uri: Jnet_Uri): JMediaItem_RequestMetadata_Builder; cdecl;
    function setSearchQuery(string_1: JString): JMediaItem_RequestMetadata_Builder; cdecl;
  end;
  TJMediaItem_RequestMetadata_Builder = class(TJavaGenericImport<JMediaItem_RequestMetadata_BuilderClass, JMediaItem_RequestMetadata_Builder>) end;

  JMediaItem_LocalConfigurationClass = interface(JObjectClass)
    ['{AFDF0382-723B-408F-BA79-CEA4A61E5799}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JMediaItem_LocalConfiguration; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$LocalConfiguration')]
  JMediaItem_LocalConfiguration = interface(JObject)
    ['{F7831D6D-B785-4E13-916E-17B3EC6F8AD7}']
    function _GetadsConfiguration: JMediaItem_AdsConfiguration; cdecl;
    function _GetcustomCacheKey: JString; cdecl;
    function _GetdrmConfiguration: JMediaItem_DrmConfiguration; cdecl;
    function _GetimageDurationMs: Int64; cdecl;
    function _GetmimeType: JString; cdecl;
    function _GetstreamKeys: JList; cdecl;
    function _GetsubtitleConfigurations: JImmutableList; cdecl;
    function _Getsubtitles: JList; cdecl;
    function _Gettag: JObject; cdecl;
    function _Geturi: Jnet_Uri; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property adsConfiguration: JMediaItem_AdsConfiguration read _GetadsConfiguration;
    property customCacheKey: JString read _GetcustomCacheKey;
    property drmConfiguration: JMediaItem_DrmConfiguration read _GetdrmConfiguration;
    property imageDurationMs: Int64 read _GetimageDurationMs;
    property mimeType: JString read _GetmimeType;
    property streamKeys: JList read _GetstreamKeys;
    property subtitleConfigurations: JImmutableList read _GetsubtitleConfigurations;
    property subtitles: JList read _Getsubtitles;
    property tag: JObject read _Gettag;
    property uri: Jnet_Uri read _Geturi;
  end;
  TJMediaItem_LocalConfiguration = class(TJavaGenericImport<JMediaItem_LocalConfigurationClass, JMediaItem_LocalConfiguration>) end;

  JMediaItem_LiveConfigurationClass = interface(JObjectClass)
    ['{5C3B562F-CC35-456E-B394-710840C7A979}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetUNSET: JMediaItem_LiveConfiguration; cdecl;
    {class} function fromBundle(bundle: JBundle): JMediaItem_LiveConfiguration; cdecl;
    {class} function init(long: Int64; long_1: Int64; long_2: Int64; float: Single; float_1: Single): JMediaItem_LiveConfiguration; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property UNSET: JMediaItem_LiveConfiguration read _GetUNSET;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$LiveConfiguration')]
  JMediaItem_LiveConfiguration = interface(JObject)
    ['{FC295A39-6ABA-49AB-805A-9E1829A9755D}']
    function _GetmaxOffsetMs: Int64; cdecl;
    function _GetmaxPlaybackSpeed: Single; cdecl;
    function _GetminOffsetMs: Int64; cdecl;
    function _GetminPlaybackSpeed: Single; cdecl;
    function _GettargetOffsetMs: Int64; cdecl;
    function buildUpon: JMediaItem_LiveConfiguration_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property maxOffsetMs: Int64 read _GetmaxOffsetMs;
    property maxPlaybackSpeed: Single read _GetmaxPlaybackSpeed;
    property minOffsetMs: Int64 read _GetminOffsetMs;
    property minPlaybackSpeed: Single read _GetminPlaybackSpeed;
    property targetOffsetMs: Int64 read _GettargetOffsetMs;
  end;
  TJMediaItem_LiveConfiguration = class(TJavaGenericImport<JMediaItem_LiveConfigurationClass, JMediaItem_LiveConfiguration>) end;

  JMediaItem_LiveConfiguration_BuilderClass = interface(JObjectClass)
    ['{12A31C3B-42A9-4389-9B96-97678766FF14}']
    {class} function init: JMediaItem_LiveConfiguration_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$LiveConfiguration$Builder')]
  JMediaItem_LiveConfiguration_Builder = interface(JObject)
    ['{D3887186-3C22-4A37-A725-5ECA4285AFD7}']
    function build: JMediaItem_LiveConfiguration; cdecl;
    function setMaxOffsetMs(long: Int64): JMediaItem_LiveConfiguration_Builder; cdecl;
    function setMaxPlaybackSpeed(float: Single): JMediaItem_LiveConfiguration_Builder; cdecl;
    function setMinOffsetMs(long: Int64): JMediaItem_LiveConfiguration_Builder; cdecl;
    function setMinPlaybackSpeed(float: Single): JMediaItem_LiveConfiguration_Builder; cdecl;
    function setTargetOffsetMs(long: Int64): JMediaItem_LiveConfiguration_Builder; cdecl;
  end;
  TJMediaItem_LiveConfiguration_Builder = class(TJavaGenericImport<JMediaItem_LiveConfiguration_BuilderClass, JMediaItem_LiveConfiguration_Builder>) end;

  JMediaItem_DrmConfigurationClass = interface(JObjectClass)
    ['{3B8E34F1-B258-4C2A-ADF0-01CB66AA1825}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JMediaItem_DrmConfiguration; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$DrmConfiguration')]
  JMediaItem_DrmConfiguration = interface(JObject)
    ['{F06A77D9-BF9E-4DC3-B411-1CA6CA0F8364}']
    function _GetforceDefaultLicenseUri: Boolean; cdecl;
    function _GetforcedSessionTrackTypes: JImmutableList; cdecl;
    function _GetlicenseRequestHeaders: JImmutableMap; cdecl;
    function _GetlicenseUri: Jnet_Uri; cdecl;
    function _GetmultiSession: Boolean; cdecl;
    function _GetplayClearContentWithoutKey: Boolean; cdecl;
    function _GetrequestHeaders: JImmutableMap; cdecl;
    function _Getscheme: JUUID; cdecl;
    function _GetsessionForClearTypes: JImmutableList; cdecl;
    function _Getuuid: JUUID; cdecl;
    function buildUpon: JMediaItem_DrmConfiguration_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getKeySetId: TJavaArray<Byte>; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property forceDefaultLicenseUri: Boolean read _GetforceDefaultLicenseUri;
    property forcedSessionTrackTypes: JImmutableList read _GetforcedSessionTrackTypes;
    property licenseRequestHeaders: JImmutableMap read _GetlicenseRequestHeaders;
    property licenseUri: Jnet_Uri read _GetlicenseUri;
    property multiSession: Boolean read _GetmultiSession;
    property playClearContentWithoutKey: Boolean read _GetplayClearContentWithoutKey;
    property requestHeaders: JImmutableMap read _GetrequestHeaders;
    property scheme: JUUID read _Getscheme;
    property sessionForClearTypes: JImmutableList read _GetsessionForClearTypes;
    property uuid: JUUID read _Getuuid;
  end;
  TJMediaItem_DrmConfiguration = class(TJavaGenericImport<JMediaItem_DrmConfigurationClass, JMediaItem_DrmConfiguration>) end;

  JMediaItem_DrmConfiguration_BuilderClass = interface(JObjectClass)
    ['{6FB2C51C-304D-45EA-8537-1FC50D045148}']
    {class} function init(uuid: JUUID): JMediaItem_DrmConfiguration_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$DrmConfiguration$Builder')]
  JMediaItem_DrmConfiguration_Builder = interface(JObject)
    ['{09C6667B-692B-4D4F-82AA-95BD85D9CEB9}']
    function build: JMediaItem_DrmConfiguration; cdecl;
    function forceSessionsForAudioAndVideoTracks(boolean: Boolean): JMediaItem_DrmConfiguration_Builder; cdecl;
    function setForceDefaultLicenseUri(boolean: Boolean): JMediaItem_DrmConfiguration_Builder; cdecl;
    function setForceSessionsForAudioAndVideoTracks(boolean: Boolean): JMediaItem_DrmConfiguration_Builder; cdecl;
    function setForcedSessionTrackTypes(list: JList): JMediaItem_DrmConfiguration_Builder; cdecl;
    function setKeySetId(bytes: TJavaArray<Byte>): JMediaItem_DrmConfiguration_Builder; cdecl;
    function setLicenseRequestHeaders(map: JMap): JMediaItem_DrmConfiguration_Builder; cdecl;
    function setLicenseUri(uri: Jnet_Uri): JMediaItem_DrmConfiguration_Builder; cdecl; overload;
    function setLicenseUri(string_1: JString): JMediaItem_DrmConfiguration_Builder; cdecl; overload;
    function setMultiSession(boolean: Boolean): JMediaItem_DrmConfiguration_Builder; cdecl;
    function setPlayClearContentWithoutKey(boolean: Boolean): JMediaItem_DrmConfiguration_Builder; cdecl;
    function setScheme(uuid: JUUID): JMediaItem_DrmConfiguration_Builder; cdecl;
  end;
  TJMediaItem_DrmConfiguration_Builder = class(TJavaGenericImport<JMediaItem_DrmConfiguration_BuilderClass, JMediaItem_DrmConfiguration_Builder>) end;

  JMediaItem_ClippingConfigurationClass = interface(JObjectClass)
    ['{35BA6B1F-05F2-47C4-AA8E-4B259269980C}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetUNSET: JMediaItem_ClippingConfiguration; cdecl;
    {class} function fromBundle(bundle: JBundle): JMediaItem_ClippingProperties; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property UNSET: JMediaItem_ClippingConfiguration read _GetUNSET;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$ClippingConfiguration')]
  JMediaItem_ClippingConfiguration = interface(JObject)
    ['{B127FCEC-1113-429C-92B0-5E79D8A23538}']
    function _GetendPositionMs: Int64; cdecl;
    function _GetendPositionUs: Int64; cdecl;
    function _GetrelativeToDefaultPosition: Boolean; cdecl;
    function _GetrelativeToLiveWindow: Boolean; cdecl;
    function _GetstartPositionMs: Int64; cdecl;
    function _GetstartPositionUs: Int64; cdecl;
    function _GetstartsAtKeyFrame: Boolean; cdecl;
    function buildUpon: JMediaItem_ClippingConfiguration_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property endPositionMs: Int64 read _GetendPositionMs;
    property endPositionUs: Int64 read _GetendPositionUs;
    property relativeToDefaultPosition: Boolean read _GetrelativeToDefaultPosition;
    property relativeToLiveWindow: Boolean read _GetrelativeToLiveWindow;
    property startPositionMs: Int64 read _GetstartPositionMs;
    property startPositionUs: Int64 read _GetstartPositionUs;
    property startsAtKeyFrame: Boolean read _GetstartsAtKeyFrame;
  end;
  TJMediaItem_ClippingConfiguration = class(TJavaGenericImport<JMediaItem_ClippingConfigurationClass, JMediaItem_ClippingConfiguration>) end;

  JMediaItem_ClippingConfiguration_BuilderClass = interface(JObjectClass)
    ['{8D82C770-3AF7-4CAA-B9AD-512E6A80651F}']
    {class} function init: JMediaItem_ClippingConfiguration_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$ClippingConfiguration$Builder')]
  JMediaItem_ClippingConfiguration_Builder = interface(JObject)
    ['{02D068AE-66A6-4F67-8E9E-FE50BC6DB1C0}']
    function build: JMediaItem_ClippingConfiguration; cdecl;
    function buildClippingProperties: JMediaItem_ClippingProperties; cdecl;
    function setEndPositionMs(long: Int64): JMediaItem_ClippingConfiguration_Builder; cdecl;
    function setEndPositionUs(long: Int64): JMediaItem_ClippingConfiguration_Builder; cdecl;
    function setRelativeToDefaultPosition(boolean: Boolean): JMediaItem_ClippingConfiguration_Builder; cdecl;
    function setRelativeToLiveWindow(boolean: Boolean): JMediaItem_ClippingConfiguration_Builder; cdecl;
    function setStartPositionMs(long: Int64): JMediaItem_ClippingConfiguration_Builder; cdecl;
    function setStartPositionUs(long: Int64): JMediaItem_ClippingConfiguration_Builder; cdecl;
    function setStartsAtKeyFrame(boolean: Boolean): JMediaItem_ClippingConfiguration_Builder; cdecl;
  end;
  TJMediaItem_ClippingConfiguration_Builder = class(TJavaGenericImport<JMediaItem_ClippingConfiguration_BuilderClass, JMediaItem_ClippingConfiguration_Builder>) end;

  JMediaItem_BuilderClass = interface(JObjectClass)
    ['{7FCDD1DC-0742-459E-AC66-65E5B9246E19}']
    {class} function init: JMediaItem_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$Builder')]
  JMediaItem_Builder = interface(JObject)
    ['{776906C6-ADE0-40D6-8425-BD7E2FC9F83C}']
    function build: JMediaItem; cdecl;
    function setAdTagUri(string_1: JString): JMediaItem_Builder; cdecl; overload;
    function setAdTagUri(uri: Jnet_Uri): JMediaItem_Builder; cdecl; overload;
    function setAdTagUri(uri: Jnet_Uri; object_1: JObject): JMediaItem_Builder; cdecl; overload;
    function setAdsConfiguration(adsconfiguration: JMediaItem_AdsConfiguration): JMediaItem_Builder; cdecl;
    function setClipEndPositionMs(long: Int64): JMediaItem_Builder; cdecl;
    function setClipRelativeToDefaultPosition(boolean: Boolean): JMediaItem_Builder; cdecl;
    function setClipRelativeToLiveWindow(boolean: Boolean): JMediaItem_Builder; cdecl;
    function setClipStartPositionMs(long: Int64): JMediaItem_Builder; cdecl;
    function setClipStartsAtKeyFrame(boolean: Boolean): JMediaItem_Builder; cdecl;
    function setClippingConfiguration(clippingconfiguration: JMediaItem_ClippingConfiguration): JMediaItem_Builder; cdecl;
    function setCustomCacheKey(string_1: JString): JMediaItem_Builder; cdecl;
    function setDrmConfiguration(drmconfiguration: JMediaItem_DrmConfiguration): JMediaItem_Builder; cdecl;
    function setDrmForceDefaultLicenseUri(boolean: Boolean): JMediaItem_Builder; cdecl;
    function setDrmKeySetId(bytes: TJavaArray<Byte>): JMediaItem_Builder; cdecl;
    function setDrmLicenseRequestHeaders(map: JMap): JMediaItem_Builder; cdecl;
    function setDrmLicenseUri(uri: Jnet_Uri): JMediaItem_Builder; cdecl; overload;
    function setDrmLicenseUri(string_1: JString): JMediaItem_Builder; cdecl; overload;
    function setDrmMultiSession(boolean: Boolean): JMediaItem_Builder; cdecl;
    function setDrmPlayClearContentWithoutKey(boolean: Boolean): JMediaItem_Builder; cdecl;
    function setDrmSessionForClearPeriods(boolean: Boolean): JMediaItem_Builder; cdecl;
    function setDrmSessionForClearTypes(list: JList): JMediaItem_Builder; cdecl;
    function setDrmUuid(uuid: JUUID): JMediaItem_Builder; cdecl;
    function setImageDurationMs(long: Int64): JMediaItem_Builder; cdecl;
    function setLiveConfiguration(liveconfiguration: JMediaItem_LiveConfiguration): JMediaItem_Builder; cdecl;
    function setLiveMaxOffsetMs(long: Int64): JMediaItem_Builder; cdecl;
    function setLiveMaxPlaybackSpeed(float: Single): JMediaItem_Builder; cdecl;
    function setLiveMinOffsetMs(long: Int64): JMediaItem_Builder; cdecl;
    function setLiveMinPlaybackSpeed(float: Single): JMediaItem_Builder; cdecl;
    function setLiveTargetOffsetMs(long: Int64): JMediaItem_Builder; cdecl;
    function setMediaId(string_1: JString): JMediaItem_Builder; cdecl;
    function setMediaMetadata(mediametadata: JMediaMetadata): JMediaItem_Builder; cdecl;
    function setMimeType(string_1: JString): JMediaItem_Builder; cdecl;
    function setRequestMetadata(requestmetadata: JMediaItem_RequestMetadata): JMediaItem_Builder; cdecl;
    function setStreamKeys(list: JList): JMediaItem_Builder; cdecl;
    function setSubtitleConfigurations(list: JList): JMediaItem_Builder; cdecl;
    function setSubtitles(list: JList): JMediaItem_Builder; cdecl;
    function setTag(object_1: JObject): JMediaItem_Builder; cdecl;
    function setUri(string_1: JString): JMediaItem_Builder; cdecl; overload;
    function setUri(uri: Jnet_Uri): JMediaItem_Builder; cdecl; overload;
  end;
  TJMediaItem_Builder = class(TJavaGenericImport<JMediaItem_BuilderClass, JMediaItem_Builder>) end;

  JMediaItem_AdsConfigurationClass = interface(JObjectClass)
    ['{C58C0145-0319-498F-A14E-7AE579D0512E}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JMediaItem_AdsConfiguration; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$AdsConfiguration')]
  JMediaItem_AdsConfiguration = interface(JObject)
    ['{CB2CF8A2-657E-4BA4-ADE6-9D0E7E45B6B5}']
    function _GetadTagUri: Jnet_Uri; cdecl;
    function _GetadsId: JObject; cdecl;
    function buildUpon: JMediaItem_AdsConfiguration_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property adTagUri: Jnet_Uri read _GetadTagUri;
    property adsId: JObject read _GetadsId;
  end;
  TJMediaItem_AdsConfiguration = class(TJavaGenericImport<JMediaItem_AdsConfigurationClass, JMediaItem_AdsConfiguration>) end;

  JMediaItem_AdsConfiguration_BuilderClass = interface(JObjectClass)
    ['{15ED07D9-A5DB-4E53-B4FA-0500F2FB2DF1}']
    {class} function init(uri: Jnet_Uri): JMediaItem_AdsConfiguration_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$AdsConfiguration$Builder')]
  JMediaItem_AdsConfiguration_Builder = interface(JObject)
    ['{FA0473CD-2784-4E66-B3B9-CF448F2B3993}']
    function build: JMediaItem_AdsConfiguration; cdecl;
    function setAdTagUri(uri: Jnet_Uri): JMediaItem_AdsConfiguration_Builder; cdecl;
    function setAdsId(object_1: JObject): JMediaItem_AdsConfiguration_Builder; cdecl;
  end;
  TJMediaItem_AdsConfiguration_Builder = class(TJavaGenericImport<JMediaItem_AdsConfiguration_BuilderClass, JMediaItem_AdsConfiguration_Builder>) end;

  JFormatClass = interface(JObjectClass)
    ['{9702EE79-F34B-4474-B726-9CF44FCDF601}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetCUE_REPLACEMENT_BEHAVIOR_MERGE: Integer; cdecl;
    {class} function _GetCUE_REPLACEMENT_BEHAVIOR_REPLACE: Integer; cdecl;
    {class} function _GetNO_VALUE: Integer; cdecl;
    {class} function _GetOFFSET_SAMPLE_RELATIVE: Int64; cdecl;
    {class} function fromBundle(bundle: JBundle): JFormat; cdecl;
    {class} function toLogString(format: JFormat): JString; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property CUE_REPLACEMENT_BEHAVIOR_MERGE: Integer read _GetCUE_REPLACEMENT_BEHAVIOR_MERGE;
    {class} property CUE_REPLACEMENT_BEHAVIOR_REPLACE: Integer read _GetCUE_REPLACEMENT_BEHAVIOR_REPLACE;
    {class} property NO_VALUE: Integer read _GetNO_VALUE;
    {class} property OFFSET_SAMPLE_RELATIVE: Int64 read _GetOFFSET_SAMPLE_RELATIVE;
  end;

  [JavaSignature('androidx/media3/common/Format')]
  JFormat = interface(JObject)
    ['{C462F6CD-15F8-40F3-BED0-10A3960359FA}']
    function _GetaccessibilityChannel: Integer; cdecl;
    function _GetaverageBitrate: Integer; cdecl;
    function _Getbitrate: Integer; cdecl;
    function _GetchannelCount: Integer; cdecl;
    function _Getcodecs: JString; cdecl;
    function _GetcolorInfo: JColorInfo; cdecl;
    function _GetcontainerMimeType: JString; cdecl;
    function _GetcryptoType: Integer; cdecl;
    function _GetcueReplacementBehavior: Integer; cdecl;
    function _GetdrmInitData: JDrmInitData; cdecl;
    function _GetencoderDelay: Integer; cdecl;
    function _GetencoderPadding: Integer; cdecl;
    function _GetframeRate: Single; cdecl;
    function _Getheight: Integer; cdecl;
    function _Getid: JString; cdecl;
    function _GetinitializationData: JList; cdecl;
    function _Getlabel: JString; cdecl;
    function _Getlanguage: JString; cdecl;
    function _GetmaxInputSize: Integer; cdecl;
    function _Getmetadata: JMetadata; cdecl;
    function _GetpcmEncoding: Integer; cdecl;
    function _GetpeakBitrate: Integer; cdecl;
    function _GetpixelWidthHeightRatio: Single; cdecl;
    function _GetprojectionData: TJavaArray<Byte>; cdecl;
    function _GetroleFlags: Integer; cdecl;
    function _GetrotationDegrees: Integer; cdecl;
    function _GetsampleMimeType: JString; cdecl;
    function _GetsampleRate: Integer; cdecl;
    function _GetselectionFlags: Integer; cdecl;
    function _GetstereoMode: Integer; cdecl;
    function _GetsubsampleOffsetUs: Int64; cdecl;
    function _GettileCountHorizontal: Integer; cdecl;
    function _GettileCountVertical: Integer; cdecl;
    function _Getwidth: Integer; cdecl;
    function buildUpon: JFormat_Builder; cdecl;
    function copyWithCryptoType(int: Integer): JFormat; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getPixelCount: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function initializationDataEquals(format: JFormat): Boolean; cdecl;
    function toBundle(boolean: Boolean): JBundle; cdecl; overload;
    function toBundle: JBundle; cdecl; overload;
    function toString: JString; cdecl;
    function withManifestFormatInfo(format: JFormat): JFormat; cdecl;
    property accessibilityChannel: Integer read _GetaccessibilityChannel;
    property averageBitrate: Integer read _GetaverageBitrate;
    property bitrate: Integer read _Getbitrate;
    property channelCount: Integer read _GetchannelCount;
    property codecs: JString read _Getcodecs;
    property colorInfo: JColorInfo read _GetcolorInfo;
    property containerMimeType: JString read _GetcontainerMimeType;
    property cryptoType: Integer read _GetcryptoType;
    property cueReplacementBehavior: Integer read _GetcueReplacementBehavior;
    property drmInitData: JDrmInitData read _GetdrmInitData;
    property encoderDelay: Integer read _GetencoderDelay;
    property encoderPadding: Integer read _GetencoderPadding;
    property frameRate: Single read _GetframeRate;
    property height: Integer read _Getheight;
    property id: JString read _Getid;
    property initializationData: JList read _GetinitializationData;
    property &label: JString read _Getlabel;
    property language: JString read _Getlanguage;
    property maxInputSize: Integer read _GetmaxInputSize;
    property metadata: JMetadata read _Getmetadata;
    property pcmEncoding: Integer read _GetpcmEncoding;
    property peakBitrate: Integer read _GetpeakBitrate;
    property pixelWidthHeightRatio: Single read _GetpixelWidthHeightRatio;
    property projectionData: TJavaArray<Byte> read _GetprojectionData;
    property roleFlags: Integer read _GetroleFlags;
    property rotationDegrees: Integer read _GetrotationDegrees;
    property sampleMimeType: JString read _GetsampleMimeType;
    property sampleRate: Integer read _GetsampleRate;
    property selectionFlags: Integer read _GetselectionFlags;
    property stereoMode: Integer read _GetstereoMode;
    property subsampleOffsetUs: Int64 read _GetsubsampleOffsetUs;
    property tileCountHorizontal: Integer read _GettileCountHorizontal;
    property tileCountVertical: Integer read _GettileCountVertical;
    property width: Integer read _Getwidth;
  end;
  TJFormat = class(TJavaGenericImport<JFormatClass, JFormat>) end;

  JFormat_BuilderClass = interface(JObjectClass)
    ['{AA11F249-D077-4780-89B3-36FCB619C2CE}']
    {class} function init: JFormat_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/Format$Builder')]
  JFormat_Builder = interface(JObject)
    ['{0C217EF0-E507-4470-A66E-2ED5C7F23657}']
    function build: JFormat; cdecl;
    function setAccessibilityChannel(int: Integer): JFormat_Builder; cdecl;
    function setAverageBitrate(int: Integer): JFormat_Builder; cdecl;
    function setChannelCount(int: Integer): JFormat_Builder; cdecl;
    function setCodecs(string_1: JString): JFormat_Builder; cdecl;
    function setColorInfo(colorinfo: JColorInfo): JFormat_Builder; cdecl;
    function setContainerMimeType(string_1: JString): JFormat_Builder; cdecl;
    function setCryptoType(int: Integer): JFormat_Builder; cdecl;
    function setCueReplacementBehavior(int: Integer): JFormat_Builder; cdecl;
    function setDrmInitData(drminitdata: JDrmInitData): JFormat_Builder; cdecl;
    function setEncoderDelay(int: Integer): JFormat_Builder; cdecl;
    function setEncoderPadding(int: Integer): JFormat_Builder; cdecl;
    function setFrameRate(float: Single): JFormat_Builder; cdecl;
    function setHeight(int: Integer): JFormat_Builder; cdecl;
    function setId(string_1: JString): JFormat_Builder; cdecl; overload;
    function setId(int: Integer): JFormat_Builder; cdecl; overload;
    function setInitializationData(list: JList): JFormat_Builder; cdecl;
    function setLabel(string_1: JString): JFormat_Builder; cdecl;
    function setLanguage(string_1: JString): JFormat_Builder; cdecl;
    function setMaxInputSize(int: Integer): JFormat_Builder; cdecl;
    function setMetadata(metadata: JMetadata): JFormat_Builder; cdecl;
    function setPcmEncoding(int: Integer): JFormat_Builder; cdecl;
    function setPeakBitrate(int: Integer): JFormat_Builder; cdecl;
    function setPixelWidthHeightRatio(float: Single): JFormat_Builder; cdecl;
    function setProjectionData(bytes: TJavaArray<Byte>): JFormat_Builder; cdecl;
    function setRoleFlags(int: Integer): JFormat_Builder; cdecl;
    function setRotationDegrees(int: Integer): JFormat_Builder; cdecl;
    function setSampleMimeType(string_1: JString): JFormat_Builder; cdecl;
    function setSampleRate(int: Integer): JFormat_Builder; cdecl;
    function setSelectionFlags(int: Integer): JFormat_Builder; cdecl;
    function setStereoMode(int: Integer): JFormat_Builder; cdecl;
    function setSubsampleOffsetUs(long: Int64): JFormat_Builder; cdecl;
    function setTileCountHorizontal(int: Integer): JFormat_Builder; cdecl;
    function setTileCountVertical(int: Integer): JFormat_Builder; cdecl;
    function setWidth(int: Integer): JFormat_Builder; cdecl;
  end;
  TJFormat_Builder = class(TJavaGenericImport<JFormat_BuilderClass, JFormat_Builder>) end;

  JFlagSetClass = interface(JObjectClass)
    ['{D04A9DE0-8673-4E47-B56C-451B96F1905B}']
  end;

  [JavaSignature('androidx/media3/common/FlagSet')]
  JFlagSet = interface(JObject)
    ['{0FC9F55E-78DE-400B-9360-D065465F3838}']
    function contains(int: Integer): Boolean; cdecl;
    function containsAny(int: Integer): Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(int: Integer): Integer; cdecl;
    function hashCode: Integer; cdecl;
    function size: Integer; cdecl;
  end;
  TJFlagSet = class(TJavaGenericImport<JFlagSetClass, JFlagSet>) end;

  JDrmInitDataClass = interface(JObjectClass)
    ['{F9210A59-A4D1-4217-AE60-14C5C8EBFE78}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function createSessionCreationData(drminitdata: JDrmInitData; drminitdata_1: JDrmInitData): JDrmInitData; cdecl;
    {class} function init(string_1: JString; list: JList): JDrmInitData; cdecl; overload;
    {class} function init(list: JList): JDrmInitData; cdecl; overload;
    {class} function init(string_1: JString; schemedata: JDrmInitData_SchemeData): JDrmInitData; cdecl; overload;
    {class} function init(schemedata: JDrmInitData_SchemeData): JDrmInitData; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/DrmInitData')]
  JDrmInitData = interface(JObject)
    ['{4303AABF-E1EF-4723-B7C8-42E581F035B2}']
    function _GetschemeDataCount: Integer; cdecl;
    function _GetschemeType: JString; cdecl;
    function compare(schemedata: JDrmInitData_SchemeData; schemedata_1: JDrmInitData_SchemeData): Integer; cdecl; overload;
    function compare(object_1: JObject; object_2: JObject): Integer; cdecl; overload;
    function copyWithSchemeType(string_1: JString): JDrmInitData; cdecl;
    function describeContents: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(int: Integer): JDrmInitData_SchemeData; cdecl;
    function hashCode: Integer; cdecl;
    function merge(drminitdata: JDrmInitData): JDrmInitData; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
    property schemeDataCount: Integer read _GetschemeDataCount;
    property schemeType: JString read _GetschemeType;
  end;
  TJDrmInitData = class(TJavaGenericImport<JDrmInitDataClass, JDrmInitData>) end;

  JDrmInitData_SchemeDataClass = interface(JObjectClass)
    ['{C4FAFBAA-456D-4E0D-8F2E-7628614930CA}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init(uuid: JUUID; string_1: JString; bytes: TJavaArray<Byte>): JDrmInitData_SchemeData; cdecl; overload;
    {class} function init(uuid: JUUID; string_1: JString; string_2: JString; bytes: TJavaArray<Byte>): JDrmInitData_SchemeData; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/DrmInitData$SchemeData')]
  JDrmInitData_SchemeData = interface(JObject)
    ['{C9A64B89-2753-49D4-88B7-674BD3553054}']
    function _Getdata: TJavaArray<Byte>; cdecl;
    function _GetlicenseServerUrl: JString; cdecl;
    function _GetmimeType: JString; cdecl;
    function _Getuuid: JUUID; cdecl;
    function canReplace(schemedata: JDrmInitData_SchemeData): Boolean; cdecl;
    function copyWithData(bytes: TJavaArray<Byte>): JDrmInitData_SchemeData; cdecl;
    function describeContents: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hasData: Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function matches(uuid: JUUID): Boolean; cdecl;
    procedure writeToParcel(parcel: JParcel; int: Integer); cdecl;
    property data: TJavaArray<Byte> read _Getdata;
    property licenseServerUrl: JString read _GetlicenseServerUrl;
    property mimeType: JString read _GetmimeType;
    property uuid: JUUID read _Getuuid;
  end;
  TJDrmInitData_SchemeData = class(TJavaGenericImport<JDrmInitData_SchemeDataClass, JDrmInitData_SchemeData>) end;

  JDeviceInfoClass = interface(JObjectClass)
    ['{2127D2C8-85E1-4899-8AE9-D1E207B5FEF8}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetPLAYBACK_TYPE_LOCAL: Integer; cdecl;
    {class} function _GetPLAYBACK_TYPE_REMOTE: Integer; cdecl;
    {class} function _GetUNKNOWN: JDeviceInfo; cdecl;
    {class} function fromBundle(bundle: JBundle): JDeviceInfo; cdecl;
    {class} function init(int: Integer; int_1: Integer; int_2: Integer): JDeviceInfo; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property PLAYBACK_TYPE_LOCAL: Integer read _GetPLAYBACK_TYPE_LOCAL;
    {class} property PLAYBACK_TYPE_REMOTE: Integer read _GetPLAYBACK_TYPE_REMOTE;
    {class} property UNKNOWN: JDeviceInfo read _GetUNKNOWN;
  end;

  [JavaSignature('androidx/media3/common/DeviceInfo')]
  JDeviceInfo = interface(JObject)
    ['{61285719-88B5-41AE-B8AE-91CFCB2ECF43}']
    function _GetmaxVolume: Integer; cdecl;
    function _GetminVolume: Integer; cdecl;
    function _GetplaybackType: Integer; cdecl;
    function _GetroutingControllerId: JString; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property maxVolume: Integer read _GetmaxVolume;
    property minVolume: Integer read _GetminVolume;
    property playbackType: Integer read _GetplaybackType;
    property routingControllerId: JString read _GetroutingControllerId;
  end;
  TJDeviceInfo = class(TJavaGenericImport<JDeviceInfoClass, JDeviceInfo>) end;

  JColorInfoClass = interface(JObjectClass)
    ['{7A370C4A-3DB1-4E32-A79F-3D8E4F603273}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetSDR_BT709_LIMITED: JColorInfo; cdecl;
    {class} function _GetSRGB_BT709_FULL: JColorInfo; cdecl;
    {class} function fromBundle(bundle: JBundle): JColorInfo; cdecl;
    {class} function isTransferHdr(colorinfo: JColorInfo): Boolean; cdecl;
    {class} function isoColorPrimariesToColorSpace(int: Integer): Integer; cdecl;
    {class} function isoTransferCharacteristicsToColorTransfer(int: Integer): Integer; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property SDR_BT709_LIMITED: JColorInfo read _GetSDR_BT709_LIMITED;
    {class} property SRGB_BT709_FULL: JColorInfo read _GetSRGB_BT709_FULL;
  end;

  [JavaSignature('androidx/media3/common/ColorInfo')]
  JColorInfo = interface(JObject)
    ['{47E28936-AEE4-4845-B570-AF111B2ED83E}']
    function _GetchromaBitdepth: Integer; cdecl;
    function _GetcolorRange: Integer; cdecl;
    function _GetcolorSpace: Integer; cdecl;
    function _GetcolorTransfer: Integer; cdecl;
    function _GethdrStaticInfo: TJavaArray<Byte>; cdecl;
    function _GetlumaBitdepth: Integer; cdecl;
    function buildUpon: JColorInfo_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isBitdepthValid: Boolean; cdecl;
    function isDataSpaceValid: Boolean; cdecl;
    function isValid: Boolean; cdecl;
    function toBundle: JBundle; cdecl;
    function toLogString: JString; cdecl;
    function toString: JString; cdecl;
    property chromaBitdepth: Integer read _GetchromaBitdepth;
    property colorRange: Integer read _GetcolorRange;
    property colorSpace: Integer read _GetcolorSpace;
    property colorTransfer: Integer read _GetcolorTransfer;
    property hdrStaticInfo: TJavaArray<Byte> read _GethdrStaticInfo;
    property lumaBitdepth: Integer read _GetlumaBitdepth;
  end;
  TJColorInfo = class(TJavaGenericImport<JColorInfoClass, JColorInfo>) end;

  JColorInfo_BuilderClass = interface(JObjectClass)
    ['{197CD453-9985-43E5-AF58-376BE1DFA1D9}']
    {class} function init: JColorInfo_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/common/ColorInfo$Builder')]
  JColorInfo_Builder = interface(JObject)
    ['{D4E43887-FD90-4676-9F87-44921CC3C78A}']
    function build: JColorInfo; cdecl;
    function setChromaBitdepth(int: Integer): JColorInfo_Builder; cdecl;
    function setColorRange(int: Integer): JColorInfo_Builder; cdecl;
    function setColorSpace(int: Integer): JColorInfo_Builder; cdecl;
    function setColorTransfer(int: Integer): JColorInfo_Builder; cdecl;
    function setHdrStaticInfo(bytes: TJavaArray<Byte>): JColorInfo_Builder; cdecl;
    function setLumaBitdepth(int: Integer): JColorInfo_Builder; cdecl;
  end;
  TJColorInfo_Builder = class(TJavaGenericImport<JColorInfo_BuilderClass, JColorInfo_Builder>) end;

  JBundleable_CreatorClass = interface(IJavaClass)
    ['{AB2772E3-06B4-4E1A-A65E-39444F303870}']
  end;

  [JavaSignature('androidx/media3/common/Bundleable$Creator')]
  JBundleable_Creator = interface(IJavaInstance)
    ['{EEF3F7BA-3B54-4FF0-97D6-E5B677F49DE8}']
    function fromBundle(bundle: JBundle): JObject; cdecl;
  end;
  TJBundleable_Creator = class(TJavaGenericImport<JBundleable_CreatorClass, JBundleable_Creator>) end;

  JAuxEffectInfoClass = interface(JObjectClass)
    ['{39B1AE7C-9350-46AE-B91A-491CBA75B1D0}']
    {class} function _GetNO_AUX_EFFECT_ID: Integer; cdecl;
    {class} function init(int: Integer; float: Single): JAuxEffectInfo; cdecl;
    {class} property NO_AUX_EFFECT_ID: Integer read _GetNO_AUX_EFFECT_ID;
  end;

  [JavaSignature('androidx/media3/common/AuxEffectInfo')]
  JAuxEffectInfo = interface(JObject)
    ['{1E6D2FC9-696A-4E16-9338-0D2AB9384296}']
    function _GeteffectId: Integer; cdecl;
    function _GetsendLevel: Single; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    property effectId: Integer read _GeteffectId;
    property sendLevel: Single read _GetsendLevel;
  end;
  TJAuxEffectInfo = class(TJavaGenericImport<JAuxEffectInfoClass, JAuxEffectInfo>) end;

  JAudioAttributesClass = interface(JObjectClass)
    ['{D24427CB-8564-416E-97E1-549EF30B85F6}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetDEFAULT: JAudioAttributes; cdecl;
    {class} function fromBundle(bundle: JBundle): JAudioAttributes; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property &DEFAULT: JAudioAttributes read _GetDEFAULT;
  end;

  [JavaSignature('androidx/media3/common/AudioAttributes')]
  JAudioAttributes = interface(JObject)
    ['{6A7290D6-8E65-4EBF-BF27-357F8F19FE8D}']
    function _GetallowedCapturePolicy: Integer; cdecl;
    function _GetcontentType: Integer; cdecl;
    function _Getflags: Integer; cdecl;
    function _GetspatializationBehavior: Integer; cdecl;
    function _Getusage: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getAudioAttributesV21: JAudioAttributes_AudioAttributesV21; cdecl;
    function hashCode: Integer; cdecl;
    function toBundle: JBundle; cdecl;
    property allowedCapturePolicy: Integer read _GetallowedCapturePolicy;
    property contentType: Integer read _GetcontentType;
    property flags: Integer read _Getflags;
    property spatializationBehavior: Integer read _GetspatializationBehavior;
    property usage: Integer read _Getusage;
  end;
  TJAudioAttributes = class(TJavaGenericImport<JAudioAttributesClass, JAudioAttributes>) end;

  JAudioAttributes_AudioAttributesV21Class = interface(JObjectClass)
    ['{F2D49430-D3DC-4F0E-8751-08745D4FBA8C}']
  end;

  [JavaSignature('androidx/media3/common/AudioAttributes$AudioAttributesV21')]
  JAudioAttributes_AudioAttributesV21 = interface(JObject)
    ['{CB41EC99-AF70-46D2-9B26-72B4AD5CB85F}']
    function _GetaudioAttributes: JAudioAttributes; cdecl;
    property audioAttributes: JAudioAttributes read _GetaudioAttributes;
  end;
  TJAudioAttributes_AudioAttributesV21 = class(TJavaGenericImport<JAudioAttributes_AudioAttributesV21Class, JAudioAttributes_AudioAttributesV21>) end;

  JAdPlaybackStateClass = interface(JObjectClass)
    ['{6FC8D9A8-AFE2-4A42-930B-AE18677D2CE0}']
    {class} function _GetAD_STATE_AVAILABLE: Integer; cdecl;
    {class} function _GetAD_STATE_ERROR: Integer; cdecl;
    {class} function _GetAD_STATE_PLAYED: Integer; cdecl;
    {class} function _GetAD_STATE_SKIPPED: Integer; cdecl;
    {class} function _GetAD_STATE_UNAVAILABLE: Integer; cdecl;
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetNONE: JAdPlaybackState; cdecl;
    {class} function fromAdPlaybackState(object_1: JObject; adplaybackstate: JAdPlaybackState): JAdPlaybackState; cdecl;
    {class} function fromBundle(bundle: JBundle): JAdPlaybackState; cdecl;
    {class} function init(object_1: JObject; long: Int64): JAdPlaybackState; cdecl;
    {class} property AD_STATE_AVAILABLE: Integer read _GetAD_STATE_AVAILABLE;
    {class} property AD_STATE_ERROR: Integer read _GetAD_STATE_ERROR;
    {class} property AD_STATE_PLAYED: Integer read _GetAD_STATE_PLAYED;
    {class} property AD_STATE_SKIPPED: Integer read _GetAD_STATE_SKIPPED;
    {class} property AD_STATE_UNAVAILABLE: Integer read _GetAD_STATE_UNAVAILABLE;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property NONE: JAdPlaybackState read _GetNONE;
  end;

  [JavaSignature('androidx/media3/common/AdPlaybackState')]
  JAdPlaybackState = interface(JObject)
    ['{5CCAFFB2-F2D8-48CF-9BAF-D17DFEA043CA}']
    function _GetadGroupCount: Integer; cdecl;
    function _GetadResumePositionUs: Int64; cdecl;
    function _GetadsId: JObject; cdecl;
    function _GetcontentDurationUs: Int64; cdecl;
    function _GetremovedAdGroupCount: Integer; cdecl;
    function endsWithLivePostrollPlaceHolder: Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getAdGroup(int: Integer): JAdPlaybackState_AdGroup; cdecl;
    function getAdGroupIndexAfterPositionUs(long: Int64; long_1: Int64): Integer; cdecl;
    function getAdGroupIndexForPositionUs(long: Int64; long_1: Int64): Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isAdInErrorState(int: Integer; int_1: Integer): Boolean; cdecl;
    function isLivePostrollPlaceholder(int: Integer): Boolean; cdecl;
    function toBundle: JBundle; cdecl;
    function toString: JString; cdecl;
    function withAdCount(int: Integer; int_1: Integer): JAdPlaybackState; cdecl;
    function withAdDurationsUs(longss: TJavaArray<Int64>): JAdPlaybackState; cdecl; overload;
    function withAdDurationsUs(int: Integer; long: Int64): JAdPlaybackState; cdecl; overload;
    function withAdGroupTimeUs(int: Integer; long: Int64): JAdPlaybackState; cdecl;
    function withAdLoadError(int: Integer; int_1: Integer): JAdPlaybackState; cdecl;
    function withAdResumePositionUs(long: Int64): JAdPlaybackState; cdecl;
    function withAvailableAd(int: Integer; int_1: Integer): JAdPlaybackState; cdecl;
    function withAvailableAdMediaItem(int: Integer; int_1: Integer; mediaitem: JMediaItem): JAdPlaybackState; cdecl;
    function withAvailableAdUri(int: Integer; int_1: Integer; uri: Jnet_Uri): JAdPlaybackState; cdecl;
    function withContentDurationUs(long: Int64): JAdPlaybackState; cdecl;
    function withContentResumeOffsetUs(int: Integer; long: Int64): JAdPlaybackState; cdecl;
    function withIsServerSideInserted(int: Integer; boolean: Boolean): JAdPlaybackState; cdecl;
    function withLastAdRemoved(int: Integer): JAdPlaybackState; cdecl;
    function withLivePostrollPlaceholderAppended: JAdPlaybackState; cdecl;
    function withNewAdGroup(int: Integer; long: Int64): JAdPlaybackState; cdecl;
    function withOriginalAdCount(int: Integer; int_1: Integer): JAdPlaybackState; cdecl;
    function withPlayedAd(int: Integer; int_1: Integer): JAdPlaybackState; cdecl;
    function withRemovedAdGroupCount(int: Integer): JAdPlaybackState; cdecl;
    function withResetAdGroup(int: Integer): JAdPlaybackState; cdecl;
    function withSkippedAd(int: Integer; int_1: Integer): JAdPlaybackState; cdecl;
    function withSkippedAdGroup(int: Integer): JAdPlaybackState; cdecl;
    property adGroupCount: Integer read _GetadGroupCount;
    property adResumePositionUs: Int64 read _GetadResumePositionUs;
    property adsId: JObject read _GetadsId;
    property contentDurationUs: Int64 read _GetcontentDurationUs;
    property removedAdGroupCount: Integer read _GetremovedAdGroupCount;
  end;
  TJAdPlaybackState = class(TJavaGenericImport<JAdPlaybackStateClass, JAdPlaybackState>) end;

  JAdPlaybackState_AdGroupClass = interface(JObjectClass)
    ['{3DC34FA8-197E-4B9B-B44C-5A0550C47EF2}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function fromBundle(bundle: JBundle): JAdPlaybackState_AdGroup; cdecl;
    {class} function init(long: Int64): JAdPlaybackState_AdGroup; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
  end;

  [JavaSignature('androidx/media3/common/AdPlaybackState$AdGroup')]
  JAdPlaybackState_AdGroup = interface(JObject)
    ['{80EC3418-7793-43B7-B6AD-99303884AD60}']
    function _GetcontentResumeOffsetUs: Int64; cdecl;
    function _Getcount: Integer; cdecl;
    function _GetdurationsUs: TJavaArray<Int64>; cdecl;
    function _GetisServerSideInserted: Boolean; cdecl;
    function _GetmediaItems: TJavaObjectArray<JMediaItem>; cdecl;
    function _GetoriginalCount: Integer; cdecl;
    function _Getstates: TJavaArray<Integer>; cdecl;
    function _GettimeUs: Int64; cdecl;
    function _Geturis: TJavaObjectArray<Jnet_Uri>; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function getFirstAdIndexToPlay: Integer; cdecl;
    function getNextAdIndexToPlay(int: Integer): Integer; cdecl;
    function hasUnplayedAds: Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function shouldPlayAdGroup: Boolean; cdecl;
    function toBundle: JBundle; cdecl;
    function withAdCount(int: Integer): JAdPlaybackState_AdGroup; cdecl;
    function withAdDurationsUs(longs: TJavaArray<Int64>): JAdPlaybackState_AdGroup; cdecl;
    function withAdMediaItem(mediaitem: JMediaItem; int: Integer): JAdPlaybackState_AdGroup; cdecl;
    function withAdState(int: Integer; int_1: Integer): JAdPlaybackState_AdGroup; cdecl;
    function withAdUri(uri: Jnet_Uri; int: Integer): JAdPlaybackState_AdGroup; cdecl;
    function withAllAdsReset: JAdPlaybackState_AdGroup; cdecl;
    function withAllAdsSkipped: JAdPlaybackState_AdGroup; cdecl;
    function withContentResumeOffsetUs(long: Int64): JAdPlaybackState_AdGroup; cdecl;
    function withIsServerSideInserted(boolean: Boolean): JAdPlaybackState_AdGroup; cdecl;
    function withLastAdRemoved: JAdPlaybackState_AdGroup; cdecl;
    function withOriginalAdCount(int: Integer): JAdPlaybackState_AdGroup; cdecl;
    function withTimeUs(long: Int64): JAdPlaybackState_AdGroup; cdecl;
    property contentResumeOffsetUs: Int64 read _GetcontentResumeOffsetUs;
    property count: Integer read _Getcount;
    property durationsUs: TJavaArray<Int64> read _GetdurationsUs;
    property isServerSideInserted: Boolean read _GetisServerSideInserted;
    property mediaItems: TJavaObjectArray<JMediaItem> read _GetmediaItems;
    property originalCount: Integer read _GetoriginalCount;
    property states: TJavaArray<Integer> read _Getstates;
    property timeUs: Int64 read _GettimeUs;
    property uris: TJavaObjectArray<Jnet_Uri> read _Geturis;
  end;
  TJAdPlaybackState_AdGroup = class(TJavaGenericImport<JAdPlaybackState_AdGroupClass, JAdPlaybackState_AdGroup>) end;

  JMediaItem_ClippingPropertiesClass = interface(JMediaItem_ClippingConfigurationClass)
    ['{EDD71339-0C79-4CCC-A9C0-86405BC8DA8B}']
    {class} function _GetUNSET: JMediaItem_ClippingProperties; cdecl;
    {class} property UNSET: JMediaItem_ClippingProperties read _GetUNSET;
  end;

  [JavaSignature('androidx/media3/common/MediaItem$ClippingProperties')]
  JMediaItem_ClippingProperties = interface(JMediaItem_ClippingConfiguration)
    ['{9855DEC2-80B8-47CA-B12C-4A6E3FAB0508}']
  end;
  TJMediaItem_ClippingProperties = class(TJavaGenericImport<JMediaItem_ClippingPropertiesClass, JMediaItem_ClippingProperties>) end;

implementation

end.
