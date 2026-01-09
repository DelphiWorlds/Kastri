unit DW.Androidapi.JNI.AndroidX.Media3.Exoplayer;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Media,
  Androidapi.JNI.Util, Androidapi.JNI.Net,
  // DW
  DW.Androidapi.JNI.AndroidX.Media3.DataSource, DW.Androidapi.JNI.AndroidX.Media3.Common, DW.Androidapi.JNI.AndroidX.Media3.Decoder,
  DW.Androidapi.JNI.Guava;

type
  JAllocation = interface;
  JAllocator = interface;
  JAllocator_AllocationNode = interface;
  JAnalyticsCollector = interface;
  JAnalyticsListener = interface;
  JAnalyticsListener_EventTime = interface;
  JAnalyticsListener_Events = interface;
  JAudioSink_AudioTrackConfig = interface;
  JBandwidthMeter = interface;
  JBandwidthMeter_EventListener = interface;
  JCameraMotionListener = interface;
  JChunk = interface;
  JDecoderCounters = interface;
  JDecoderReuseEvaluation = interface;
  JDrmSession = interface;
  JDrmSessionEventListener = interface;
  JDrmSessionEventListener_EventDispatcher = interface;
  JDrmSession_DrmSessionException = interface;
  JExoPlaybackException = interface;
  JExoPlayer = interface;
  JExoPlayer_AudioComponent = interface;
  JExoPlayer_AudioOffloadListener = interface;
  JExoPlayer_Builder = interface;
  JExoPlayer_DeviceComponent = interface;
  JExoPlayer_TextComponent = interface;
  JExoPlayer_VideoComponent = interface;
  JExoTrackSelection = interface;
  JFormatHolder = interface;
  JImageOutput = interface;
  JLoadEventInfo = interface;
  JLoadingInfo = interface;
  JLoadingInfo_Builder = interface;
  {$IF CompilerVersion < 36}
  JLogSessionId = interface;
  {$ENDIF}
  JMediaChunkIterator = interface;
  JMediaClock = interface;
  JMediaLoadData = interface;
  JMediaPeriod = interface;
  JMediaPeriod_Callback = interface;
  JMediaSource = interface;
  JMediaSourceEventListener = interface;
  JMediaSource_MediaPeriodId = interface;
  JMediaSource_MediaSourceCaller = interface;
  JPlayerId = interface;
  JPlayerMessage = interface;
  JPlayerMessage_Sender = interface;
  JPlayerMessage_Target = interface;
  JRenderer = interface;
  JRendererCapabilities = interface;
  JRendererCapabilities_Listener = interface;
  JRendererConfiguration = interface;
  JSampleStream = interface;
  JSeekParameters = interface;
  JSequenceableLoader = interface;
  JSequenceableLoader_Callback = interface;
  JShuffleOrder = interface;
  JTrackGroupArray = interface;
  JTrackSelection = interface;
  JTrackSelectionArray = interface;
  JTrackSelector = interface;
  JTrackSelectorResult = interface;
  JTrackSelector_InvalidationListener = interface;
  JVideoFrameMetadataListener = interface;

  JCameraMotionListenerClass = interface(IJavaClass)
    ['{3CCB69D9-D0AA-4BEC-9921-C8B964DFD69A}']
  end;

  [JavaSignature('androidx/media3/exoplayer/video/spherical/CameraMotionListener')]
  JCameraMotionListener = interface(IJavaInstance)
    ['{FF1CF438-4D15-4931-8284-168109E9EC73}']
    procedure onCameraMotion(long: Int64; floats: TJavaArray<Single>); cdecl;
    procedure onCameraMotionReset; cdecl;
  end;
  TJCameraMotionListener = class(TJavaGenericImport<JCameraMotionListenerClass, JCameraMotionListener>) end;

  JVideoFrameMetadataListenerClass = interface(IJavaClass)
    ['{61D90FE4-B89D-412D-AD69-AB64B4539E55}']
  end;

  [JavaSignature('androidx/media3/exoplayer/video/VideoFrameMetadataListener')]
  JVideoFrameMetadataListener = interface(IJavaInstance)
    ['{7E908D50-4928-40DD-BDA3-EADB76DC3E28}']
    procedure onVideoFrameAboutToBeRendered(long: Int64; long_1: Int64; format: JFormat; mediaformat: JMediaFormat); cdecl;
  end;
  TJVideoFrameMetadataListener = class(TJavaGenericImport<JVideoFrameMetadataListenerClass, JVideoFrameMetadataListener>) end;

  JBandwidthMeterClass = interface(IJavaClass)
    ['{E2F348B3-ADFA-47D6-8BC8-E2F9417BE855}']
  end;

  [JavaSignature('androidx/media3/exoplayer/upstream/BandwidthMeter')]
  JBandwidthMeter = interface(IJavaInstance)
    ['{67E06232-D84B-4558-AB31-8A5AC1E6907F}']
    procedure addEventListener(handler: JHandler; eventlistener: JBandwidthMeter_EventListener); cdecl;
    function getBitrateEstimate: Int64; cdecl;
    function getTimeToFirstByteEstimateUs: Int64; cdecl;
    function getTransferListener: JTransferListener; cdecl;
    procedure removeEventListener(eventlistener: JBandwidthMeter_EventListener); cdecl;
  end;
  TJBandwidthMeter = class(TJavaGenericImport<JBandwidthMeterClass, JBandwidthMeter>) end;

  JBandwidthMeter_EventListenerClass = interface(IJavaClass)
    ['{0646CBA2-9C7A-49A5-9127-FD7F07C4B980}']
  end;

  [JavaSignature('androidx/media3/exoplayer/upstream/BandwidthMeter$EventListener')]
  JBandwidthMeter_EventListener = interface(IJavaInstance)
    ['{955E1348-057D-4300-B310-EED3C9CD1C7C}']
    procedure onBandwidthSample(int: Integer; long: Int64; long_1: Int64); cdecl;
  end;
  TJBandwidthMeter_EventListener = class(TJavaGenericImport<JBandwidthMeter_EventListenerClass, JBandwidthMeter_EventListener>) end;

  JAllocatorClass = interface(IJavaClass)
    ['{BA164F1D-ACDC-4E99-96CA-F144995CA166}']
  end;

  [JavaSignature('androidx/media3/exoplayer/upstream/Allocator')]
  JAllocator = interface(IJavaInstance)
    ['{83D59789-D732-462D-B1D2-737FB29AB143}']
    function allocate: JAllocation; cdecl;
    function getIndividualAllocationLength: Integer; cdecl;
    function getTotalBytesAllocated: Integer; cdecl;
    procedure release(allocation: JAllocation); cdecl; overload;
    procedure release(allocationnode: JAllocator_AllocationNode); cdecl; overload;
    procedure trim; cdecl;
  end;
  TJAllocator = class(TJavaGenericImport<JAllocatorClass, JAllocator>) end;

  JAllocator_AllocationNodeClass = interface(IJavaClass)
    ['{5ABDC093-5C43-4D0F-B1EE-C74E802C9E10}']
  end;

  [JavaSignature('androidx/media3/exoplayer/upstream/Allocator$AllocationNode')]
  JAllocator_AllocationNode = interface(IJavaInstance)
    ['{FE31CCCE-C7B5-466E-B8D2-F3C10F266602}']
    function getAllocation: JAllocation; cdecl;
    function next: JAllocator_AllocationNode; cdecl;
  end;
  TJAllocator_AllocationNode = class(TJavaGenericImport<JAllocator_AllocationNodeClass, JAllocator_AllocationNode>) end;

  JAllocationClass = interface(JObjectClass)
    ['{633EB4CB-48EC-445D-B921-776C41F8DEAE}']
    {class} function init(bytes: TJavaArray<Byte>; int: Integer): JAllocation; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/upstream/Allocation')]
  JAllocation = interface(JObject)
    ['{08BDDAE6-75F0-41B8-84DB-C5303CABA831}']
    function _Getdata: TJavaArray<Byte>; cdecl;
    function _Getoffset: Integer; cdecl;
    property data: TJavaArray<Byte> read _Getdata;
    property offset: Integer read _Getoffset;
  end;
  TJAllocation = class(TJavaGenericImport<JAllocationClass, JAllocation>) end;

  JTrackSelectorResultClass = interface(JObjectClass)
    ['{70854EB3-3BF6-4090-8CD8-EA0ABCC5E26E}']
    {class} function init(rendererconfigurations: TJavaObjectArray<JRendererConfiguration>; exotrackselections: TJavaObjectArray<JExoTrackSelection>;
      tracks: JTracks; object_1: JObject): JTrackSelectorResult; cdecl; overload;
    {class} function init(rendererconfigurations: TJavaObjectArray<JRendererConfiguration>; exotrackselections: TJavaObjectArray<JExoTrackSelection>;
      object_1: JObject): JTrackSelectorResult; cdecl; overload;
  end;

  [JavaSignature('androidx/media3/exoplayer/trackselection/TrackSelectorResult')]
  JTrackSelectorResult = interface(JObject)
    ['{B826B1B2-2EB8-4F75-B320-37698DA5DCD7}']
    function _Getinfo: JObject; cdecl;
    function _Getlength: Integer; cdecl;
    function _GetrendererConfigurations: TJavaObjectArray<JRendererConfiguration>; cdecl;
    function _Getselections: TJavaObjectArray<JExoTrackSelection>; cdecl;
    function _Gettracks: JTracks; cdecl;
    function isEquivalent(trackselectorresult: JTrackSelectorResult; int: Integer): Boolean; cdecl; overload;
    function isEquivalent(trackselectorresult: JTrackSelectorResult): Boolean; cdecl; overload;
    function isRendererEnabled(int: Integer): Boolean; cdecl;
    property info: JObject read _Getinfo;
    property length: Integer read _Getlength;
    property rendererConfigurations: TJavaObjectArray<JRendererConfiguration> read _GetrendererConfigurations;
    property selections: TJavaObjectArray<JExoTrackSelection> read _Getselections;
    property tracks: JTracks read _Gettracks;
  end;
  TJTrackSelectorResult = class(TJavaGenericImport<JTrackSelectorResultClass, JTrackSelectorResult>) end;

  JTrackSelectorClass = interface(JObjectClass)
    ['{F7B82BA0-C6BC-43DE-99E7-8B3C6716EFB3}']
    {class} function init: JTrackSelector; cdecl; overload;
  end;

  [JavaSignature('androidx/media3/exoplayer/trackselection/TrackSelector')]
  JTrackSelector = interface(JObject)
    ['{4C97F76A-D1C0-4D7E-81A8-D6E06FFF55C8}']
    function getParameters: JTrackSelectionParameters; cdecl;
    function getRendererCapabilitiesListener: JRendererCapabilities_Listener; cdecl;
    procedure init(invalidationlistener: JTrackSelector_InvalidationListener; bandwidthmeter: JBandwidthMeter); cdecl; overload;
    function isSetParametersSupported: Boolean; cdecl;
    procedure onSelectionActivated(object_1: JObject); cdecl;
    procedure release; cdecl;
    function selectTracks(renderercapabilitiess: TJavaObjectArray<JRendererCapabilities>; trackgrouparray: JTrackGroupArray; mediaperiodid: JMediaSource_MediaPeriodId; timeline: JTimeline): JTrackSelectorResult; cdecl;
    procedure setAudioAttributes(audioattributes: JAudioAttributes); cdecl;
    procedure setParameters(trackselectionparameters: JTrackSelectionParameters); cdecl;
  end;
  TJTrackSelector = class(TJavaGenericImport<JTrackSelectorClass, JTrackSelector>) end;

  JTrackSelector_InvalidationListenerClass = interface(IJavaClass)
    ['{C8B07092-BFAB-4AB5-B400-0C0E80CB3A74}']
  end;

  [JavaSignature('androidx/media3/exoplayer/trackselection/TrackSelector$InvalidationListener')]
  JTrackSelector_InvalidationListener = interface(IJavaInstance)
    ['{FDDBF90D-9562-4A39-94D8-69B07A8966F5}']
    procedure onRendererCapabilitiesChanged(renderer: JRenderer); cdecl;
    procedure onTrackSelectionsInvalidated; cdecl;
  end;
  TJTrackSelector_InvalidationListener = class(TJavaGenericImport<JTrackSelector_InvalidationListenerClass, JTrackSelector_InvalidationListener>) end;

  JTrackSelectionArrayClass = interface(JObjectClass)
    ['{AF020F56-671C-4EF6-9F31-8B089DC555F7}']
    {class} function init(trackselection: JTrackSelection): JTrackSelectionArray; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/trackselection/TrackSelectionArray')]
  JTrackSelectionArray = interface(JObject)
    ['{80059C2E-8BA4-404F-895B-B7019D3E1822}']
    function _Getlength: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(int: Integer): JTrackSelection; cdecl;
    function getAll: TJavaObjectArray<JTrackSelection>; cdecl;
    function hashCode: Integer; cdecl;
    property length: Integer read _Getlength;
  end;
  TJTrackSelectionArray = class(TJavaGenericImport<JTrackSelectionArrayClass, JTrackSelectionArray>) end;

  JTrackSelectionClass = interface(IJavaClass)
    ['{E7EAC261-E67C-4DEF-AC62-03EC3FC5AD73}']
    {class} function _GetTYPE_CUSTOM_BASE: Integer; cdecl;
    {class} function _GetTYPE_UNSET: Integer; cdecl;
    {class} property TYPE_CUSTOM_BASE: Integer read _GetTYPE_CUSTOM_BASE;
    {class} property TYPE_UNSET: Integer read _GetTYPE_UNSET;
  end;

  [JavaSignature('androidx/media3/exoplayer/trackselection/TrackSelection')]
  JTrackSelection = interface(IJavaInstance)
    ['{FA224F34-AF31-4D80-95EF-35C6727A9C38}']
    function getFormat(int: Integer): JFormat; cdecl;
    function getIndexInTrackGroup(int: Integer): Integer; cdecl;
    function getTrackGroup: JTrackGroup; cdecl;
    function getType: Integer; cdecl;
    function indexOf(int: Integer): Integer; cdecl; overload;
    function indexOf(format: JFormat): Integer; cdecl; overload;
    function length: Integer; cdecl;
  end;
  TJTrackSelection = class(TJavaGenericImport<JTrackSelectionClass, JTrackSelection>) end;

  JExoTrackSelectionClass = interface(JTrackSelectionClass)
    ['{2DD2444D-1689-4481-98B8-FF5F608A627A}']
  end;

  [JavaSignature('androidx/media3/exoplayer/trackselection/ExoTrackSelection')]
  JExoTrackSelection = interface(JTrackSelection)
    ['{3E09B1EE-312C-4EF8-A5C1-C8ED9F0D93DE}']
    procedure disable; cdecl;
    procedure enable; cdecl;
    function evaluateQueueSize(long: Int64; list: JList): Integer; cdecl;
    function excludeTrack(int: Integer; long: Int64): Boolean; cdecl;
    function getLatestBitrateEstimate: Int64; cdecl;
    function getSelectedFormat: JFormat; cdecl;
    function getSelectedIndex: Integer; cdecl;
    function getSelectedIndexInTrackGroup: Integer; cdecl;
    function getSelectionData: JObject; cdecl;
    function getSelectionReason: Integer; cdecl;
    function isTrackExcluded(int: Integer; long: Int64): Boolean; cdecl;
    procedure onDiscontinuity; cdecl;
    procedure onPlayWhenReadyChanged(boolean: Boolean); cdecl;
    procedure onPlaybackSpeed(float: Single); cdecl;
    procedure onRebuffer; cdecl;
    function shouldCancelChunkLoad(long: Int64; chunk: JChunk; list: JList): Boolean; cdecl;
    procedure updateSelectedTrack(long: Int64; long_1: Int64; long_2: Int64; list: JList; mediachunkiterators: TJavaObjectArray<JMediaChunkIterator>); cdecl;
  end;
  TJExoTrackSelection = class(TJavaGenericImport<JExoTrackSelectionClass, JExoTrackSelection>) end;

  JMediaChunkIteratorClass = interface(IJavaClass)
    ['{C2A94E74-7967-465B-8DC7-A591C95B6A21}']
    {class} function _GetEMPTY: JMediaChunkIterator; cdecl;
    {class} property EMPTY: JMediaChunkIterator read _GetEMPTY;
  end;

  [JavaSignature('androidx/media3/exoplayer/source/chunk/MediaChunkIterator')]
  JMediaChunkIterator = interface(IJavaInstance)
    ['{6CB7F17E-8336-4C0C-95A9-118F0EE3AFFF}']
    function getChunkEndTimeUs: Int64; cdecl;
    function getChunkStartTimeUs: Int64; cdecl;
    function getDataSpec: JDataSpec; cdecl;
    function isEnded: Boolean; cdecl;
    function next: Boolean; cdecl;
    procedure reset; cdecl;
  end;
  TJMediaChunkIterator = class(TJavaGenericImport<JMediaChunkIteratorClass, JMediaChunkIterator>) end;

  JChunkClass = interface(JObjectClass)
    ['{4E077D68-59DB-472B-A229-78CF50A56E0D}']
    {class} function init(datasource: JDataSource; dataspec: JDataSpec; int: Integer; format: JFormat; int_1: Integer; object_1: JObject; long: Int64; long_1: Int64): JChunk; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/source/chunk/Chunk')]
  JChunk = interface(JObject)
    ['{F1648BB7-7928-4E7F-A3AC-FB2DDC03F32B}']
    function _GetdataSpec: JDataSpec; cdecl;
    function _GetendTimeUs: Int64; cdecl;
    function _GetloadTaskId: Int64; cdecl;
    function _GetstartTimeUs: Int64; cdecl;
    function _GettrackFormat: JFormat; cdecl;
    function _GettrackSelectionData: JObject; cdecl;
    function _GettrackSelectionReason: Integer; cdecl;
    function _Gettype: Integer; cdecl;
    function bytesLoaded: Int64; cdecl;
    function getDurationUs: Int64; cdecl;
    function getResponseHeaders: JMap; cdecl;
    function getUri: Jnet_Uri; cdecl;
    property dataSpec: JDataSpec read _GetdataSpec;
    property endTimeUs: Int64 read _GetendTimeUs;
    property loadTaskId: Int64 read _GetloadTaskId;
    property startTimeUs: Int64 read _GetstartTimeUs;
    property trackFormat: JFormat read _GettrackFormat;
    property trackSelectionData: JObject read _GettrackSelectionData;
    property trackSelectionReason: Integer read _GettrackSelectionReason;
    property &type: Integer read _Gettype;
  end;
  TJChunk = class(TJavaGenericImport<JChunkClass, JChunk>) end;

  JTrackGroupArrayClass = interface(JObjectClass)
    ['{3675FB92-DA5E-46C1-AB43-16BF3C48254C}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetEMPTY: JTrackGroupArray; cdecl;
    {class} function fromBundle(bundle: JBundle): JTrackGroupArray; cdecl;
    {class} function init(trackgroup: JTrackGroup): JTrackGroupArray; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property EMPTY: JTrackGroupArray read _GetEMPTY;
  end;

  [JavaSignature('androidx/media3/exoplayer/source/TrackGroupArray')]
  JTrackGroupArray = interface(JObject)
    ['{0C62A60B-FD1D-4C17-B5D0-831ED3B0CC8A}']
    function _Getlength: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function get(int: Integer): JTrackGroup; cdecl;
    function getTrackTypes: JImmutableList; cdecl;
    function hashCode: Integer; cdecl;
    function indexOf(trackgroup: JTrackGroup): Integer; cdecl;
    function isEmpty: Boolean; cdecl;
    function toBundle: JBundle; cdecl;
    property length: Integer read _Getlength;
  end;
  TJTrackGroupArray = class(TJavaGenericImport<JTrackGroupArrayClass, JTrackGroupArray>) end;

  JShuffleOrderClass = interface(IJavaClass)
    ['{A16482B4-FDBC-44B4-A807-A15940F95985}']
  end;

  [JavaSignature('androidx/media3/exoplayer/source/ShuffleOrder')]
  JShuffleOrder = interface(IJavaInstance)
    ['{0EBFEBDF-1C58-48AF-845B-561C9EE6FB59}']
    function cloneAndClear: JShuffleOrder; cdecl;
    function cloneAndInsert(int: Integer; int_1: Integer): JShuffleOrder; cdecl;
    function cloneAndRemove(int: Integer; int_1: Integer): JShuffleOrder; cdecl;
    function getFirstIndex: Integer; cdecl;
    function getLastIndex: Integer; cdecl;
    function getLength: Integer; cdecl;
    function getNextIndex(int: Integer): Integer; cdecl;
    function getPreviousIndex(int: Integer): Integer; cdecl;
  end;
  TJShuffleOrder = class(TJavaGenericImport<JShuffleOrderClass, JShuffleOrder>) end;

  JSequenceableLoaderClass = interface(IJavaClass)
    ['{F82B8858-8D9C-4DA3-8F50-9F3F45A3A60B}']
  end;

  [JavaSignature('androidx/media3/exoplayer/source/SequenceableLoader')]
  JSequenceableLoader = interface(IJavaInstance)
    ['{5018DB19-032F-4141-BBA9-909933460230}']
    function continueLoading(loadinginfo: JLoadingInfo): Boolean; cdecl;
    function getBufferedPositionUs: Int64; cdecl;
    function getNextLoadPositionUs: Int64; cdecl;
    function isLoading: Boolean; cdecl;
    procedure reevaluateBuffer(long: Int64); cdecl;
  end;
  TJSequenceableLoader = class(TJavaGenericImport<JSequenceableLoaderClass, JSequenceableLoader>) end;

  JSequenceableLoader_CallbackClass = interface(IJavaClass)
    ['{5934FE48-7999-4682-A6C7-D5A167A8D490}']
  end;

  [JavaSignature('androidx/media3/exoplayer/source/SequenceableLoader$Callback')]
  JSequenceableLoader_Callback = interface(IJavaInstance)
    ['{ACDBF59E-A2B6-41F9-A272-A874E0BE29D5}']
    procedure onContinueLoadingRequested(t: JObject); cdecl;
  end;
  TJSequenceableLoader_Callback = class(TJavaGenericImport<JSequenceableLoader_CallbackClass, JSequenceableLoader_Callback>) end;

  JSampleStreamClass = interface(IJavaClass)
    ['{8F2AC8F1-A5BD-46DF-8D71-005A7F569C28}']
    {class} function _GetFLAG_OMIT_SAMPLE_DATA: Integer; cdecl;
    {class} function _GetFLAG_PEEK: Integer; cdecl;
    {class} function _GetFLAG_REQUIRE_FORMAT: Integer; cdecl;
    {class} property FLAG_OMIT_SAMPLE_DATA: Integer read _GetFLAG_OMIT_SAMPLE_DATA;
    {class} property FLAG_PEEK: Integer read _GetFLAG_PEEK;
    {class} property FLAG_REQUIRE_FORMAT: Integer read _GetFLAG_REQUIRE_FORMAT;
  end;

  [JavaSignature('androidx/media3/exoplayer/source/SampleStream')]
  JSampleStream = interface(IJavaInstance)
    ['{F17A3606-1B0C-496F-BA0B-504D8AA8CCB6}']
    function isReady: Boolean; cdecl;
    procedure maybeThrowError; cdecl;
    function readData(formatholder: JFormatHolder; decoderinputbuffer: JDecoderInputBuffer; int: Integer): Integer; cdecl;
    function skipData(long: Int64): Integer; cdecl;
  end;
  TJSampleStream = class(TJavaGenericImport<JSampleStreamClass, JSampleStream>) end;

  JMediaSourceEventListenerClass = interface(IJavaClass)
    ['{B885BE7C-11B6-424B-89AD-2B758422B067}']
  end;

  [JavaSignature('androidx/media3/exoplayer/source/MediaSourceEventListener')]
  JMediaSourceEventListener = interface(IJavaInstance)
    ['{74ACE07F-4F0D-40DE-AF72-D72650A80660}']
    procedure onDownstreamFormatChanged(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; medialoaddata: JMediaLoadData); cdecl;
    procedure onLoadCanceled(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; loadeventinfo: JLoadEventInfo; medialoaddata: JMediaLoadData); cdecl;
    procedure onLoadCompleted(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; loadeventinfo: JLoadEventInfo; medialoaddata: JMediaLoadData); cdecl;
    procedure onLoadError(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; loadeventinfo: JLoadEventInfo; medialoaddata: JMediaLoadData; ioexception: JIOException; boolean: Boolean); cdecl;
    procedure onLoadStarted(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; loadeventinfo: JLoadEventInfo; medialoaddata: JMediaLoadData); cdecl;
    procedure onUpstreamDiscarded(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; medialoaddata: JMediaLoadData); cdecl;
  end;
  TJMediaSourceEventListener = class(TJavaGenericImport<JMediaSourceEventListenerClass, JMediaSourceEventListener>) end;

  JMediaSourceClass = interface(IJavaClass)
    ['{375A4DC9-5109-4A40-9F76-AE52F62AF93A}']
  end;

  [JavaSignature('androidx/media3/exoplayer/source/MediaSource')]
  JMediaSource = interface(IJavaInstance)
    ['{BC8A722A-98BB-480B-97A5-3FEC9B6043BF}']
    procedure addDrmEventListener(handler: JHandler; drmsessioneventlistener: JDrmSessionEventListener); cdecl;
    procedure addEventListener(handler: JHandler; mediasourceeventlistener: JMediaSourceEventListener); cdecl;
    function canUpdateMediaItem(mediaitem: JMediaItem): Boolean; cdecl;
    function createPeriod(mediaperiodid: JMediaSource_MediaPeriodId; allocator: JAllocator; long: Int64): JMediaPeriod; cdecl;
    procedure disable(mediasourcecaller: JMediaSource_MediaSourceCaller); cdecl;
    procedure enable(mediasourcecaller: JMediaSource_MediaSourceCaller); cdecl;
    function getInitialTimeline: JTimeline; cdecl;
    function getMediaItem: JMediaItem; cdecl;
    function isSingleWindow: Boolean; cdecl;
    procedure maybeThrowSourceInfoRefreshError; cdecl;
    procedure prepareSource(mediasourcecaller: JMediaSource_MediaSourceCaller; transferlistener: JTransferListener; playerid: JPlayerId); cdecl; overload;
    procedure prepareSource(mediasourcecaller: JMediaSource_MediaSourceCaller; transferlistener: JTransferListener); cdecl; overload;
    procedure releasePeriod(mediaperiod: JMediaPeriod); cdecl;
    procedure releaseSource(mediasourcecaller: JMediaSource_MediaSourceCaller); cdecl;
    procedure removeDrmEventListener(drmsessioneventlistener: JDrmSessionEventListener); cdecl;
    procedure removeEventListener(mediasourceeventlistener: JMediaSourceEventListener); cdecl;
    procedure updateMediaItem(mediaitem: JMediaItem); cdecl;
  end;
  TJMediaSource = class(TJavaGenericImport<JMediaSourceClass, JMediaSource>) end;

  JMediaSource_MediaSourceCallerClass = interface(IJavaClass)
    ['{432BDB06-C10D-4CCA-A5DF-FC6F21AD72D8}']
  end;

  [JavaSignature('androidx/media3/exoplayer/source/MediaSource$MediaSourceCaller')]
  JMediaSource_MediaSourceCaller = interface(IJavaInstance)
    ['{867F6A24-0D35-441A-AB27-EBC2838A0C6F}']
    procedure onSourceInfoRefreshed(mediasource: JMediaSource; timeline: JTimeline); cdecl;
  end;
  TJMediaSource_MediaSourceCaller = class(TJavaGenericImport<JMediaSource_MediaSourceCallerClass, JMediaSource_MediaSourceCaller>) end;

  JMediaSource_MediaPeriodIdClass = interface(JObjectClass)
    ['{5816205F-3548-440D-A778-5A7AFFEACFC3}']
    {class} function init(object_1: JObject; int: Integer; int_1: Integer; long: Int64): JMediaSource_MediaPeriodId; cdecl; overload;
    {class} function init(object_1: JObject; long: Int64; int: Integer): JMediaSource_MediaPeriodId; cdecl; overload;
    {class} function init(object_1: JObject): JMediaSource_MediaPeriodId; cdecl; overload;
    {class} function init(object_1: JObject; long: Int64): JMediaSource_MediaPeriodId; cdecl; overload;
  end;

  [JavaSignature('androidx/media3/exoplayer/source/MediaSource$MediaPeriodId')]
  JMediaSource_MediaPeriodId = interface(JObject)
    ['{8A30D1EE-0267-446A-8B28-6D3996686151}']
    function _GetadGroupIndex: Integer; cdecl;
    function _GetadIndexInAdGroup: Integer; cdecl;
    function _GetnextAdGroupIndex: Integer; cdecl;
    function _GetperiodUid: JObject; cdecl;
    function _GetwindowSequenceNumber: Int64; cdecl;
    function copyWithPeriodUid(object_1: JObject): JMediaSource_MediaPeriodId; cdecl;
    function copyWithWindowSequenceNumber(long: Int64): JMediaSource_MediaPeriodId; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function isAd: Boolean; cdecl;
    property adGroupIndex: Integer read _GetadGroupIndex;
    property adIndexInAdGroup: Integer read _GetadIndexInAdGroup;
    property nextAdGroupIndex: Integer read _GetnextAdGroupIndex;
    property periodUid: JObject read _GetperiodUid;
    property windowSequenceNumber: Int64 read _GetwindowSequenceNumber;
  end;
  TJMediaSource_MediaPeriodId = class(TJavaGenericImport<JMediaSource_MediaPeriodIdClass, JMediaSource_MediaPeriodId>) end;

  JMediaPeriodClass = interface(JSequenceableLoaderClass)
    ['{69938110-AF44-4964-B8F4-5AAD3E5D445C}']
  end;

  [JavaSignature('androidx/media3/exoplayer/source/MediaPeriod')]
  JMediaPeriod = interface(JSequenceableLoader)
    ['{02079A08-9256-47E0-895E-2EFFEAA76295}']
    function continueLoading(loadinginfo: JLoadingInfo): Boolean; cdecl;
    procedure discardBuffer(long: Int64; boolean: Boolean); cdecl;
    function getAdjustedSeekPositionUs(long: Int64; seekparameters: JSeekParameters): Int64; cdecl;
    function getBufferedPositionUs: Int64; cdecl;
    function getNextLoadPositionUs: Int64; cdecl;
    function getStreamKeys(list: JList): JList; cdecl;
    function getTrackGroups: JTrackGroupArray; cdecl;
    function isLoading: Boolean; cdecl;
    procedure maybeThrowPrepareError; cdecl;
    procedure prepare(callback: JMediaPeriod_Callback; long: Int64); cdecl;
    function readDiscontinuity: Int64; cdecl;
    procedure reevaluateBuffer(long: Int64); cdecl;
    function seekToUs(long: Int64): Int64; cdecl;
    function selectTracks(exotrackselections: TJavaObjectArray<JExoTrackSelection>; booleans: TJavaArray<Boolean>; samplestreams: TJavaObjectArray<JSampleStream>; booleans_1: TJavaArray<Boolean>; long: Int64): Int64; cdecl;
  end;
  TJMediaPeriod = class(TJavaGenericImport<JMediaPeriodClass, JMediaPeriod>) end;

  JMediaPeriod_CallbackClass = interface(JSequenceableLoader_CallbackClass)
    ['{A0203E15-4E5F-49D9-8503-BF426FCBD941}']
  end;

  [JavaSignature('androidx/media3/exoplayer/source/MediaPeriod$Callback')]
  JMediaPeriod_Callback = interface(JSequenceableLoader_Callback)
    ['{4C7BE14F-0A91-4D3B-897B-CFCAEF32F65A}']
    procedure onPrepared(mediaperiod: JMediaPeriod); cdecl;
  end;
  TJMediaPeriod_Callback = class(TJavaGenericImport<JMediaPeriod_CallbackClass, JMediaPeriod_Callback>) end;

  JMediaLoadDataClass = interface(JObjectClass)
    ['{893456D6-DE02-4842-A3CF-8E56BFEF8851}']
    {class} function init(int: Integer): JMediaLoadData; cdecl; overload;
    {class} function init(int: Integer; int_1: Integer; format: JFormat; int_2: Integer; object_1: JObject; long: Int64; long_1: Int64): JMediaLoadData; cdecl; overload;
  end;

  [JavaSignature('androidx/media3/exoplayer/source/MediaLoadData')]
  JMediaLoadData = interface(JObject)
    ['{7F188495-BB2B-4419-9CE2-BA2C7D3BB1EE}']
    function _GetdataType: Integer; cdecl;
    function _GetmediaEndTimeMs: Int64; cdecl;
    function _GetmediaStartTimeMs: Int64; cdecl;
    function _GettrackFormat: JFormat; cdecl;
    function _GettrackSelectionData: JObject; cdecl;
    function _GettrackSelectionReason: Integer; cdecl;
    function _GettrackType: Integer; cdecl;
    property dataType: Integer read _GetdataType;
    property mediaEndTimeMs: Int64 read _GetmediaEndTimeMs;
    property mediaStartTimeMs: Int64 read _GetmediaStartTimeMs;
    property trackFormat: JFormat read _GettrackFormat;
    property trackSelectionData: JObject read _GettrackSelectionData;
    property trackSelectionReason: Integer read _GettrackSelectionReason;
    property trackType: Integer read _GettrackType;
  end;
  TJMediaLoadData = class(TJavaGenericImport<JMediaLoadDataClass, JMediaLoadData>) end;

  JLoadEventInfoClass = interface(JObjectClass)
    ['{7A6EBF7C-E75F-47F1-8375-DD721CC89AAB}']
    {class} function getNewId: Int64; cdecl;
    {class} function init(long: Int64; dataspec: JDataSpec; long_1: Int64): JLoadEventInfo; cdecl; overload;
    {class} function init(long: Int64; dataspec: JDataSpec; uri: Jnet_Uri; map: JMap; long_1: Int64; long_2: Int64; long_3: Int64): JLoadEventInfo; cdecl; overload;
  end;

  [JavaSignature('androidx/media3/exoplayer/source/LoadEventInfo')]
  JLoadEventInfo = interface(JObject)
    ['{4806A816-74B9-4ADB-A181-04775BE37381}']
    function _GetbytesLoaded: Int64; cdecl;
    function _GetdataSpec: JDataSpec; cdecl;
    function _GetelapsedRealtimeMs: Int64; cdecl;
    function _GetloadDurationMs: Int64; cdecl;
    function _GetloadTaskId: Int64; cdecl;
    function _GetresponseHeaders: JMap; cdecl;
    function _Geturi: Jnet_Uri; cdecl;
    property bytesLoaded: Int64 read _GetbytesLoaded;
    property dataSpec: JDataSpec read _GetdataSpec;
    property elapsedRealtimeMs: Int64 read _GetelapsedRealtimeMs;
    property loadDurationMs: Int64 read _GetloadDurationMs;
    property loadTaskId: Int64 read _GetloadTaskId;
    property responseHeaders: JMap read _GetresponseHeaders;
    property uri: Jnet_Uri read _Geturi;
  end;
  TJLoadEventInfo = class(TJavaGenericImport<JLoadEventInfoClass, JLoadEventInfo>) end;

  JImageOutputClass = interface(IJavaClass)
    ['{F46918FD-08A2-4967-8A1C-B7501386F19F}']
    {class} function _GetNO_OP: JImageOutput; cdecl;
    {class} property NO_OP: JImageOutput read _GetNO_OP;
  end;

  [JavaSignature('androidx/media3/exoplayer/image/ImageOutput')]
  JImageOutput = interface(IJavaInstance)
    ['{7F2C1497-D969-4256-8A09-668586AF0F26}']
    procedure onDisabled; cdecl;
    procedure onImageAvailable(long: Int64; bitmap: JBitmap); cdecl;
  end;
  TJImageOutput = class(TJavaGenericImport<JImageOutputClass, JImageOutput>) end;

  JDrmSessionEventListenerClass = interface(IJavaClass)
    ['{D653F42C-6B48-4418-8EF6-D9BB954D4AAB}']
  end;

  [JavaSignature('androidx/media3/exoplayer/drm/DrmSessionEventListener')]
  JDrmSessionEventListener = interface(IJavaInstance)
    ['{0595AB6C-8AB4-458C-8194-9B3670C5807C}']
    procedure onDrmKeysLoaded(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId); cdecl;
    procedure onDrmKeysRemoved(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId); cdecl;
    procedure onDrmKeysRestored(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId); cdecl;
    procedure onDrmSessionAcquired(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId); cdecl; overload;
    procedure onDrmSessionAcquired(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; int_1: Integer); cdecl; overload;
    procedure onDrmSessionManagerError(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; exception: JException); cdecl;
    procedure onDrmSessionReleased(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId); cdecl;
  end;
  TJDrmSessionEventListener = class(TJavaGenericImport<JDrmSessionEventListenerClass, JDrmSessionEventListener>) end;

  JDrmSessionEventListener_EventDispatcherClass = interface(JObjectClass)
    ['{F0DB06C9-90E4-4486-8AD4-03A70E463960}']
    {class} function init: JDrmSessionEventListener_EventDispatcher; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/drm/DrmSessionEventListener$EventDispatcher')]
  JDrmSessionEventListener_EventDispatcher = interface(JObject)
    ['{D803978D-2F5B-4FD1-B102-E9C7884AA8F7}']
    function _GetmediaPeriodId: JMediaSource_MediaPeriodId; cdecl;
    function _GetwindowIndex: Integer; cdecl;
    procedure addEventListener(handler: JHandler; drmsessioneventlistener: JDrmSessionEventListener); cdecl;
    procedure drmKeysLoaded; cdecl;
    procedure drmKeysRemoved; cdecl;
    procedure drmKeysRestored; cdecl;
    procedure drmSessionAcquired(int: Integer); cdecl;
    procedure drmSessionManagerError(exception: JException); cdecl;
    procedure drmSessionReleased; cdecl;
    procedure removeEventListener(drmsessioneventlistener: JDrmSessionEventListener); cdecl;
    function withParameters(int: Integer; mediaperiodid: JMediaSource_MediaPeriodId): JDrmSessionEventListener_EventDispatcher; cdecl;
    property mediaPeriodId: JMediaSource_MediaPeriodId read _GetmediaPeriodId;
    property windowIndex: Integer read _GetwindowIndex;
  end;
  TJDrmSessionEventListener_EventDispatcher = class(TJavaGenericImport<JDrmSessionEventListener_EventDispatcherClass, JDrmSessionEventListener_EventDispatcher>) end;

  JDrmSessionClass = interface(IJavaClass)
    ['{BA531D76-FFAE-4068-9CF6-85A1361979BB}']
    {class} function _GetSTATE_ERROR: Integer; cdecl;
    {class} function _GetSTATE_OPENED: Integer; cdecl;
    {class} function _GetSTATE_OPENED_WITH_KEYS: Integer; cdecl;
    {class} function _GetSTATE_OPENING: Integer; cdecl;
    {class} function _GetSTATE_RELEASED: Integer; cdecl;
    {class} procedure replaceSession(drmsession: JDrmSession; drmsession_1: JDrmSession); cdecl;
    {class} property STATE_ERROR: Integer read _GetSTATE_ERROR;
    {class} property STATE_OPENED: Integer read _GetSTATE_OPENED;
    {class} property STATE_OPENED_WITH_KEYS: Integer read _GetSTATE_OPENED_WITH_KEYS;
    {class} property STATE_OPENING: Integer read _GetSTATE_OPENING;
    {class} property STATE_RELEASED: Integer read _GetSTATE_RELEASED;
  end;

  [JavaSignature('androidx/media3/exoplayer/drm/DrmSession')]
  JDrmSession = interface(IJavaInstance)
    ['{06E96777-0F64-4AD0-B63A-57475AFC2E37}']
    procedure acquire(eventdispatcher: JDrmSessionEventListener_EventDispatcher); cdecl;
    function getCryptoConfig: JCryptoConfig; cdecl;
    function getError: JDrmSession_DrmSessionException; cdecl;
    function getOfflineLicenseKeySetId: TJavaArray<Byte>; cdecl;
    function getSchemeUuid: JUUID; cdecl;
    function getState: Integer; cdecl;
    function playClearSamplesWithoutKeys: Boolean; cdecl;
    function queryKeyStatus: JMap; cdecl;
    procedure release(eventdispatcher: JDrmSessionEventListener_EventDispatcher); cdecl;
    function requiresSecureDecoder(string_1: JString): Boolean; cdecl;
  end;
  TJDrmSession = class(TJavaGenericImport<JDrmSessionClass, JDrmSession>) end;

  JDrmSession_DrmSessionExceptionClass = interface(JIOExceptionClass)
    ['{262CE970-1F7F-4621-B9A6-B749FAB1FBC7}']
    {class} function init(throwable: JThrowable; int: Integer): JDrmSession_DrmSessionException; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/drm/DrmSession$DrmSessionException')]
  JDrmSession_DrmSessionException = interface(JIOException)
    ['{7B8EBF04-D534-46BE-B03A-294E4763CD04}']
    function _GeterrorCode: Integer; cdecl;
    property errorCode: Integer read _GeterrorCode;
  end;
  TJDrmSession_DrmSessionException = class(TJavaGenericImport<JDrmSession_DrmSessionExceptionClass, JDrmSession_DrmSessionException>) end;

  JAudioSink_AudioTrackConfigClass = interface(JObjectClass)
    ['{08EE28D1-DD50-47E3-AE94-4C4704FE0BAD}']
    {class} function init(int: Integer; int_1: Integer; int_2: Integer; boolean: Boolean; boolean_1: Boolean; int_3: Integer): JAudioSink_AudioTrackConfig; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/audio/AudioSink$AudioTrackConfig')]
  JAudioSink_AudioTrackConfig = interface(JObject)
    ['{9EACBA90-A70A-4D66-9585-2B67C7EEAE46}']
    function _GetbufferSize: Integer; cdecl;
    function _GetchannelConfig: Integer; cdecl;
    function _Getencoding: Integer; cdecl;
    function _Getoffload: Boolean; cdecl;
    function _GetsampleRate: Integer; cdecl;
    function _Gettunneling: Boolean; cdecl;
    property bufferSize: Integer read _GetbufferSize;
    property channelConfig: Integer read _GetchannelConfig;
    property encoding: Integer read _Getencoding;
    property offload: Boolean read _Getoffload;
    property sampleRate: Integer read _GetsampleRate;
    property tunneling: Boolean read _Gettunneling;
  end;
  TJAudioSink_AudioTrackConfig = class(TJavaGenericImport<JAudioSink_AudioTrackConfigClass, JAudioSink_AudioTrackConfig>) end;

  {$IF CompilerVersion < 36}
  JLogSessionIdClass = interface(JObjectClass)
    ['{D8C1D067-1565-4FFA-8AE8-E98DC75D0564}']
    {class} function _GetLOG_SESSION_ID_NONE: JLogSessionId; cdecl;
    {class} property LOG_SESSION_ID_NONE: JLogSessionId read _GetLOG_SESSION_ID_NONE;
  end;

  [JavaSignature('android/media/metrics/LogSessionId')]
  JLogSessionId = interface(JObject)
    ['{F33CC65A-A4BE-4AFC-8A0A-720B72959395}']
    function equals(o: JObject): Boolean; cdecl;
    function getStringId: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
  end;
  TJLogSessionId = class(TJavaGenericImport<JLogSessionIdClass, JLogSessionId>) end;
  {$ENDIF}

  JPlayerIdClass = interface(JObjectClass)
    ['{C59D0179-3A88-4C29-A34D-579A468C2624}']
    {class} function _GetUNSET: JPlayerId; cdecl;
    {class} function init(logsessionid: JLogSessionId): JPlayerId; cdecl; overload;
    {class} function init: JPlayerId; cdecl; overload;
    {class} property UNSET: JPlayerId read _GetUNSET;
  end;

  [JavaSignature('androidx/media3/exoplayer/analytics/PlayerId')]
  JPlayerId = interface(JObject)
    ['{64A4A47A-BECD-496D-9D86-639443102E73}']
    function getLogSessionId: JLogSessionId; cdecl;
  end;
  TJPlayerId = class(TJavaGenericImport<JPlayerIdClass, JPlayerId>) end;

  JAnalyticsListenerClass = interface(IJavaClass)
    ['{61D9C9B9-84A1-48DE-999C-2D1DFBBDE84D}']
    {class} function _GetEVENT_AUDIO_ATTRIBUTES_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_CODEC_ERROR: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_DECODER_INITIALIZED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_DECODER_RELEASED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_DISABLED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_ENABLED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_INPUT_FORMAT_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_POSITION_ADVANCING: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_SESSION_ID: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_SINK_ERROR: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_TRACK_INITIALIZED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_TRACK_RELEASED: Integer; cdecl;
    {class} function _GetEVENT_AUDIO_UNDERRUN: Integer; cdecl;
    {class} function _GetEVENT_AVAILABLE_COMMANDS_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_BANDWIDTH_ESTIMATE: Integer; cdecl;
    {class} function _GetEVENT_CUES: Integer; cdecl;
    {class} function _GetEVENT_DEVICE_INFO_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_DEVICE_VOLUME_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_DOWNSTREAM_FORMAT_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_DRM_KEYS_LOADED: Integer; cdecl;
    {class} function _GetEVENT_DRM_KEYS_REMOVED: Integer; cdecl;
    {class} function _GetEVENT_DRM_KEYS_RESTORED: Integer; cdecl;
    {class} function _GetEVENT_DRM_SESSION_ACQUIRED: Integer; cdecl;
    {class} function _GetEVENT_DRM_SESSION_MANAGER_ERROR: Integer; cdecl;
    {class} function _GetEVENT_DRM_SESSION_RELEASED: Integer; cdecl;
    {class} function _GetEVENT_DROPPED_VIDEO_FRAMES: Integer; cdecl;
    {class} function _GetEVENT_IS_LOADING_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_IS_PLAYING_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_LOAD_CANCELED: Integer; cdecl;
    {class} function _GetEVENT_LOAD_COMPLETED: Integer; cdecl;
    {class} function _GetEVENT_LOAD_ERROR: Integer; cdecl;
    {class} function _GetEVENT_LOAD_STARTED: Integer; cdecl;
    {class} function _GetEVENT_MAX_SEEK_TO_PREVIOUS_POSITION_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_MEDIA_ITEM_TRANSITION: Integer; cdecl;
    {class} function _GetEVENT_MEDIA_METADATA_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_METADATA: Integer; cdecl;
    {class} function _GetEVENT_PLAYBACK_PARAMETERS_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_PLAYBACK_STATE_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_PLAYBACK_SUPPRESSION_REASON_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_PLAYER_ERROR: Integer; cdecl;
    {class} function _GetEVENT_PLAYER_RELEASED: Integer; cdecl;
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
    {class} function _GetEVENT_UPSTREAM_DISCARDED: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_CODEC_ERROR: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_DECODER_INITIALIZED: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_DECODER_RELEASED: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_DISABLED: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_ENABLED: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_FRAME_PROCESSING_OFFSET: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_INPUT_FORMAT_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_VIDEO_SIZE_CHANGED: Integer; cdecl;
    {class} function _GetEVENT_VOLUME_CHANGED: Integer; cdecl;
    {class} property EVENT_AUDIO_ATTRIBUTES_CHANGED: Integer read _GetEVENT_AUDIO_ATTRIBUTES_CHANGED;
    {class} property EVENT_AUDIO_CODEC_ERROR: Integer read _GetEVENT_AUDIO_CODEC_ERROR;
    {class} property EVENT_AUDIO_DECODER_INITIALIZED: Integer read _GetEVENT_AUDIO_DECODER_INITIALIZED;
    {class} property EVENT_AUDIO_DECODER_RELEASED: Integer read _GetEVENT_AUDIO_DECODER_RELEASED;
    {class} property EVENT_AUDIO_DISABLED: Integer read _GetEVENT_AUDIO_DISABLED;
    {class} property EVENT_AUDIO_ENABLED: Integer read _GetEVENT_AUDIO_ENABLED;
    {class} property EVENT_AUDIO_INPUT_FORMAT_CHANGED: Integer read _GetEVENT_AUDIO_INPUT_FORMAT_CHANGED;
    {class} property EVENT_AUDIO_POSITION_ADVANCING: Integer read _GetEVENT_AUDIO_POSITION_ADVANCING;
    {class} property EVENT_AUDIO_SESSION_ID: Integer read _GetEVENT_AUDIO_SESSION_ID;
    {class} property EVENT_AUDIO_SINK_ERROR: Integer read _GetEVENT_AUDIO_SINK_ERROR;
    {class} property EVENT_AUDIO_TRACK_INITIALIZED: Integer read _GetEVENT_AUDIO_TRACK_INITIALIZED;
    {class} property EVENT_AUDIO_TRACK_RELEASED: Integer read _GetEVENT_AUDIO_TRACK_RELEASED;
    {class} property EVENT_AUDIO_UNDERRUN: Integer read _GetEVENT_AUDIO_UNDERRUN;
    {class} property EVENT_AVAILABLE_COMMANDS_CHANGED: Integer read _GetEVENT_AVAILABLE_COMMANDS_CHANGED;
    {class} property EVENT_BANDWIDTH_ESTIMATE: Integer read _GetEVENT_BANDWIDTH_ESTIMATE;
    {class} property EVENT_CUES: Integer read _GetEVENT_CUES;
    {class} property EVENT_DEVICE_INFO_CHANGED: Integer read _GetEVENT_DEVICE_INFO_CHANGED;
    {class} property EVENT_DEVICE_VOLUME_CHANGED: Integer read _GetEVENT_DEVICE_VOLUME_CHANGED;
    {class} property EVENT_DOWNSTREAM_FORMAT_CHANGED: Integer read _GetEVENT_DOWNSTREAM_FORMAT_CHANGED;
    {class} property EVENT_DRM_KEYS_LOADED: Integer read _GetEVENT_DRM_KEYS_LOADED;
    {class} property EVENT_DRM_KEYS_REMOVED: Integer read _GetEVENT_DRM_KEYS_REMOVED;
    {class} property EVENT_DRM_KEYS_RESTORED: Integer read _GetEVENT_DRM_KEYS_RESTORED;
    {class} property EVENT_DRM_SESSION_ACQUIRED: Integer read _GetEVENT_DRM_SESSION_ACQUIRED;
    {class} property EVENT_DRM_SESSION_MANAGER_ERROR: Integer read _GetEVENT_DRM_SESSION_MANAGER_ERROR;
    {class} property EVENT_DRM_SESSION_RELEASED: Integer read _GetEVENT_DRM_SESSION_RELEASED;
    {class} property EVENT_DROPPED_VIDEO_FRAMES: Integer read _GetEVENT_DROPPED_VIDEO_FRAMES;
    {class} property EVENT_IS_LOADING_CHANGED: Integer read _GetEVENT_IS_LOADING_CHANGED;
    {class} property EVENT_IS_PLAYING_CHANGED: Integer read _GetEVENT_IS_PLAYING_CHANGED;
    {class} property EVENT_LOAD_CANCELED: Integer read _GetEVENT_LOAD_CANCELED;
    {class} property EVENT_LOAD_COMPLETED: Integer read _GetEVENT_LOAD_COMPLETED;
    {class} property EVENT_LOAD_ERROR: Integer read _GetEVENT_LOAD_ERROR;
    {class} property EVENT_LOAD_STARTED: Integer read _GetEVENT_LOAD_STARTED;
    {class} property EVENT_MAX_SEEK_TO_PREVIOUS_POSITION_CHANGED: Integer read _GetEVENT_MAX_SEEK_TO_PREVIOUS_POSITION_CHANGED;
    {class} property EVENT_MEDIA_ITEM_TRANSITION: Integer read _GetEVENT_MEDIA_ITEM_TRANSITION;
    {class} property EVENT_MEDIA_METADATA_CHANGED: Integer read _GetEVENT_MEDIA_METADATA_CHANGED;
    {class} property EVENT_METADATA: Integer read _GetEVENT_METADATA;
    {class} property EVENT_PLAYBACK_PARAMETERS_CHANGED: Integer read _GetEVENT_PLAYBACK_PARAMETERS_CHANGED;
    {class} property EVENT_PLAYBACK_STATE_CHANGED: Integer read _GetEVENT_PLAYBACK_STATE_CHANGED;
    {class} property EVENT_PLAYBACK_SUPPRESSION_REASON_CHANGED: Integer read _GetEVENT_PLAYBACK_SUPPRESSION_REASON_CHANGED;
    {class} property EVENT_PLAYER_ERROR: Integer read _GetEVENT_PLAYER_ERROR;
    {class} property EVENT_PLAYER_RELEASED: Integer read _GetEVENT_PLAYER_RELEASED;
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
    {class} property EVENT_UPSTREAM_DISCARDED: Integer read _GetEVENT_UPSTREAM_DISCARDED;
    {class} property EVENT_VIDEO_CODEC_ERROR: Integer read _GetEVENT_VIDEO_CODEC_ERROR;
    {class} property EVENT_VIDEO_DECODER_INITIALIZED: Integer read _GetEVENT_VIDEO_DECODER_INITIALIZED;
    {class} property EVENT_VIDEO_DECODER_RELEASED: Integer read _GetEVENT_VIDEO_DECODER_RELEASED;
    {class} property EVENT_VIDEO_DISABLED: Integer read _GetEVENT_VIDEO_DISABLED;
    {class} property EVENT_VIDEO_ENABLED: Integer read _GetEVENT_VIDEO_ENABLED;
    {class} property EVENT_VIDEO_FRAME_PROCESSING_OFFSET: Integer read _GetEVENT_VIDEO_FRAME_PROCESSING_OFFSET;
    {class} property EVENT_VIDEO_INPUT_FORMAT_CHANGED: Integer read _GetEVENT_VIDEO_INPUT_FORMAT_CHANGED;
    {class} property EVENT_VIDEO_SIZE_CHANGED: Integer read _GetEVENT_VIDEO_SIZE_CHANGED;
    {class} property EVENT_VOLUME_CHANGED: Integer read _GetEVENT_VOLUME_CHANGED;
  end;

  [JavaSignature('androidx/media3/exoplayer/analytics/AnalyticsListener')]
  JAnalyticsListener = interface(IJavaInstance)
    ['{A41CCF97-F3EB-4219-9CE2-304730836EA7}']
    procedure onAudioAttributesChanged(eventtime: JAnalyticsListener_EventTime; audioattributes: JAudioAttributes); cdecl;
    procedure onAudioCodecError(eventtime: JAnalyticsListener_EventTime; exception: JException); cdecl;
    procedure onAudioDecoderInitialized(eventtime: JAnalyticsListener_EventTime; string_1: JString; long: Int64); cdecl; overload;
    procedure onAudioDecoderInitialized(eventtime: JAnalyticsListener_EventTime; string_1: JString; long: Int64; long_1: Int64); cdecl; overload;
    procedure onAudioDecoderReleased(eventtime: JAnalyticsListener_EventTime; string_1: JString); cdecl;
    procedure onAudioDisabled(eventtime: JAnalyticsListener_EventTime; decodercounters: JDecoderCounters); cdecl;
    procedure onAudioEnabled(eventtime: JAnalyticsListener_EventTime; decodercounters: JDecoderCounters); cdecl;
    procedure onAudioInputFormatChanged(eventtime: JAnalyticsListener_EventTime; format: JFormat; decoderreuseevaluation: JDecoderReuseEvaluation); cdecl; overload;
    procedure onAudioInputFormatChanged(eventtime: JAnalyticsListener_EventTime; format: JFormat); cdecl; overload;
    procedure onAudioPositionAdvancing(eventtime: JAnalyticsListener_EventTime; long: Int64); cdecl;
    procedure onAudioSessionIdChanged(eventtime: JAnalyticsListener_EventTime; int: Integer); cdecl;
    procedure onAudioSinkError(eventtime: JAnalyticsListener_EventTime; exception: JException); cdecl;
    procedure onAudioTrackInitialized(eventtime: JAnalyticsListener_EventTime; audiotrackconfig: JAudioSink_AudioTrackConfig); cdecl;
    procedure onAudioTrackReleased(eventtime: JAnalyticsListener_EventTime; audiotrackconfig: JAudioSink_AudioTrackConfig); cdecl;
    procedure onAudioUnderrun(eventtime: JAnalyticsListener_EventTime; int: Integer; long: Int64; long_1: Int64); cdecl;
    procedure onAvailableCommandsChanged(eventtime: JAnalyticsListener_EventTime; commands: JPlayer_Commands); cdecl;
    procedure onBandwidthEstimate(eventtime: JAnalyticsListener_EventTime; int: Integer; long: Int64; long_1: Int64); cdecl;
    procedure onCues(eventtime: JAnalyticsListener_EventTime; cuegroup: JCueGroup); cdecl; overload;
    procedure onCues(eventtime: JAnalyticsListener_EventTime; list: JList); cdecl; overload;
    procedure onDeviceInfoChanged(eventtime: JAnalyticsListener_EventTime; deviceinfo: JDeviceInfo); cdecl;
    procedure onDeviceVolumeChanged(eventtime: JAnalyticsListener_EventTime; int: Integer; boolean: Boolean); cdecl;
    procedure onDownstreamFormatChanged(eventtime: JAnalyticsListener_EventTime; medialoaddata: JMediaLoadData); cdecl;
    procedure onDrmKeysLoaded(eventtime: JAnalyticsListener_EventTime); cdecl;
    procedure onDrmKeysRemoved(eventtime: JAnalyticsListener_EventTime); cdecl;
    procedure onDrmKeysRestored(eventtime: JAnalyticsListener_EventTime); cdecl;
    procedure onDrmSessionAcquired(eventtime: JAnalyticsListener_EventTime; int: Integer); cdecl; overload;
    procedure onDrmSessionAcquired(eventtime: JAnalyticsListener_EventTime); cdecl; overload;
    procedure onDrmSessionManagerError(eventtime: JAnalyticsListener_EventTime; exception: JException); cdecl;
    procedure onDrmSessionReleased(eventtime: JAnalyticsListener_EventTime); cdecl;
    procedure onDroppedVideoFrames(eventtime: JAnalyticsListener_EventTime; int: Integer; long: Int64); cdecl;
    procedure onEvents(player: JPlayer; events: JAnalyticsListener_Events); cdecl;
    procedure onIsLoadingChanged(eventtime: JAnalyticsListener_EventTime; boolean: Boolean); cdecl;
    procedure onIsPlayingChanged(eventtime: JAnalyticsListener_EventTime; boolean: Boolean); cdecl;
    procedure onLoadCanceled(eventtime: JAnalyticsListener_EventTime; loadeventinfo: JLoadEventInfo; medialoaddata: JMediaLoadData); cdecl;
    procedure onLoadCompleted(eventtime: JAnalyticsListener_EventTime; loadeventinfo: JLoadEventInfo; medialoaddata: JMediaLoadData); cdecl;
    procedure onLoadError(eventtime: JAnalyticsListener_EventTime; loadeventinfo: JLoadEventInfo; medialoaddata: JMediaLoadData; ioexception: JIOException; boolean: Boolean); cdecl;
    procedure onLoadStarted(eventtime: JAnalyticsListener_EventTime; loadeventinfo: JLoadEventInfo; medialoaddata: JMediaLoadData); cdecl;
    procedure onLoadingChanged(eventtime: JAnalyticsListener_EventTime; boolean: Boolean); cdecl;
    procedure onMaxSeekToPreviousPositionChanged(eventtime: JAnalyticsListener_EventTime; long: Int64); cdecl;
    procedure onMediaItemTransition(eventtime: JAnalyticsListener_EventTime; mediaitem: JMediaItem; int: Integer); cdecl;
    procedure onMediaMetadataChanged(eventtime: JAnalyticsListener_EventTime; mediametadata: JMediaMetadata); cdecl;
    procedure onMetadata(eventtime: JAnalyticsListener_EventTime; metadata: JMetadata); cdecl;
    procedure onPlayWhenReadyChanged(eventtime: JAnalyticsListener_EventTime; boolean: Boolean; int_1: Integer); cdecl;
    procedure onPlaybackParametersChanged(eventtime: JAnalyticsListener_EventTime; playbackparameters: JPlaybackParameters); cdecl;
    procedure onPlaybackStateChanged(eventtime: JAnalyticsListener_EventTime; int: Integer); cdecl;
    procedure onPlaybackSuppressionReasonChanged(eventtime: JAnalyticsListener_EventTime; int: Integer); cdecl;
    procedure onPlayerError(eventtime: JAnalyticsListener_EventTime; playbackexception: JPlaybackException); cdecl;
    procedure onPlayerErrorChanged(eventtime: JAnalyticsListener_EventTime; playbackexception: JPlaybackException); cdecl;
    procedure onPlayerReleased(eventtime: JAnalyticsListener_EventTime); cdecl;
    procedure onPlayerStateChanged(eventtime: JAnalyticsListener_EventTime; boolean: Boolean; int_1: Integer); cdecl;
    procedure onPlaylistMetadataChanged(eventtime: JAnalyticsListener_EventTime; mediametadata: JMediaMetadata); cdecl;
    procedure onPositionDiscontinuity(eventtime: JAnalyticsListener_EventTime; int: Integer); cdecl; overload;
    procedure onPositionDiscontinuity(eventtime: JAnalyticsListener_EventTime; positioninfo: JPlayer_PositionInfo; positioninfo_1: JPlayer_PositionInfo; int: Integer); cdecl; overload;
    procedure onRenderedFirstFrame(eventtime: JAnalyticsListener_EventTime; object_1: JObject; long: Int64); cdecl;
    procedure onRepeatModeChanged(eventtime: JAnalyticsListener_EventTime; int: Integer); cdecl;
    procedure onSeekBackIncrementChanged(eventtime: JAnalyticsListener_EventTime; long: Int64); cdecl;
    procedure onSeekForwardIncrementChanged(eventtime: JAnalyticsListener_EventTime; long: Int64); cdecl;
    procedure onSeekStarted(eventtime: JAnalyticsListener_EventTime); cdecl;
    procedure onShuffleModeChanged(eventtime: JAnalyticsListener_EventTime; boolean: Boolean); cdecl;
    procedure onSkipSilenceEnabledChanged(eventtime: JAnalyticsListener_EventTime; boolean: Boolean); cdecl;
    procedure onSurfaceSizeChanged(eventtime: JAnalyticsListener_EventTime; int: Integer; int_1: Integer); cdecl;
    procedure onTimelineChanged(eventtime: JAnalyticsListener_EventTime; int: Integer); cdecl;
    procedure onTrackSelectionParametersChanged(eventtime: JAnalyticsListener_EventTime; trackselectionparameters: JTrackSelectionParameters); cdecl;
    procedure onTracksChanged(eventtime: JAnalyticsListener_EventTime; tracks: JTracks); cdecl;
    procedure onUpstreamDiscarded(eventtime: JAnalyticsListener_EventTime; medialoaddata: JMediaLoadData); cdecl;
    procedure onVideoCodecError(eventtime: JAnalyticsListener_EventTime; exception: JException); cdecl;
    procedure onVideoDecoderInitialized(eventtime: JAnalyticsListener_EventTime; string_1: JString; long: Int64; long_1: Int64); cdecl; overload;
    procedure onVideoDecoderInitialized(eventtime: JAnalyticsListener_EventTime; string_1: JString; long: Int64); cdecl; overload;
    procedure onVideoDecoderReleased(eventtime: JAnalyticsListener_EventTime; string_1: JString); cdecl;
    procedure onVideoDisabled(eventtime: JAnalyticsListener_EventTime; decodercounters: JDecoderCounters); cdecl;
    procedure onVideoEnabled(eventtime: JAnalyticsListener_EventTime; decodercounters: JDecoderCounters); cdecl;
    procedure onVideoFrameProcessingOffset(eventtime: JAnalyticsListener_EventTime; long: Int64; int: Integer); cdecl;
    procedure onVideoInputFormatChanged(eventtime: JAnalyticsListener_EventTime; format: JFormat); cdecl; overload;
    procedure onVideoInputFormatChanged(eventtime: JAnalyticsListener_EventTime; format: JFormat; decoderreuseevaluation: JDecoderReuseEvaluation); cdecl; overload;
    procedure onVideoSizeChanged(eventtime: JAnalyticsListener_EventTime; int: Integer; int_1: Integer; int_2: Integer; float: Single); cdecl; overload;
    procedure onVideoSizeChanged(eventtime: JAnalyticsListener_EventTime; videosize: JVideoSize); cdecl; overload;
    procedure onVolumeChanged(eventtime: JAnalyticsListener_EventTime; float: Single); cdecl;
  end;
  TJAnalyticsListener = class(TJavaGenericImport<JAnalyticsListenerClass, JAnalyticsListener>) end;

  JAnalyticsListener_EventsClass = interface(JObjectClass)
    ['{38665A40-3D01-4EF3-93F6-0EA80375714A}']
    {class} function init(flagset: JFlagSet; sparsearray: JSparseArray): JAnalyticsListener_Events; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/analytics/AnalyticsListener$Events')]
  JAnalyticsListener_Events = interface(JObject)
    ['{DB854921-FE1C-40F3-9E06-2BEF6E21B443}']
    function contains(int: Integer): Boolean; cdecl;
    function containsAny(int: Integer): Boolean; cdecl;
    function get(int: Integer): Integer; cdecl;
    function getEventTime(int: Integer): JAnalyticsListener_EventTime; cdecl;
    function size: Integer; cdecl;
  end;
  TJAnalyticsListener_Events = class(TJavaGenericImport<JAnalyticsListener_EventsClass, JAnalyticsListener_Events>) end;

  JAnalyticsListener_EventTimeClass = interface(JObjectClass)
    ['{0E137D45-CB7B-4672-9A1D-FD5FFD799EEE}']
    {class} function init(long: Int64; timeline: JTimeline; int: Integer; mediaperiodid: JMediaSource_MediaPeriodId; long_1: Int64; timeline_1: JTimeline; int_1: Integer; mediaperiodid_1: JMediaSource_MediaPeriodId; long_2: Int64; long_3: Int64): JAnalyticsListener_EventTime; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/analytics/AnalyticsListener$EventTime')]
  JAnalyticsListener_EventTime = interface(JObject)
    ['{5B443532-CE4F-49CE-B126-42E7D2B2540C}']
    function _GetcurrentMediaPeriodId: JMediaSource_MediaPeriodId; cdecl;
    function _GetcurrentPlaybackPositionMs: Int64; cdecl;
    function _GetcurrentTimeline: JTimeline; cdecl;
    function _GetcurrentWindowIndex: Integer; cdecl;
    function _GeteventPlaybackPositionMs: Int64; cdecl;
    function _GetmediaPeriodId: JMediaSource_MediaPeriodId; cdecl;
    function _GetrealtimeMs: Int64; cdecl;
    function _Gettimeline: JTimeline; cdecl;
    function _GettotalBufferedDurationMs: Int64; cdecl;
    function _GetwindowIndex: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    property currentMediaPeriodId: JMediaSource_MediaPeriodId read _GetcurrentMediaPeriodId;
    property currentPlaybackPositionMs: Int64 read _GetcurrentPlaybackPositionMs;
    property currentTimeline: JTimeline read _GetcurrentTimeline;
    property currentWindowIndex: Integer read _GetcurrentWindowIndex;
    property eventPlaybackPositionMs: Int64 read _GeteventPlaybackPositionMs;
    property mediaPeriodId: JMediaSource_MediaPeriodId read _GetmediaPeriodId;
    property realtimeMs: Int64 read _GetrealtimeMs;
    property timeline: JTimeline read _Gettimeline;
    property totalBufferedDurationMs: Int64 read _GettotalBufferedDurationMs;
    property windowIndex: Integer read _GetwindowIndex;
  end;
  TJAnalyticsListener_EventTime = class(TJavaGenericImport<JAnalyticsListener_EventTimeClass, JAnalyticsListener_EventTime>) end;

  JAnalyticsCollectorClass = interface(JPlayer_ListenerClass)
    ['{D89644F3-C7AB-4A13-AE9B-3996F54764EC}']
  end;

  [JavaSignature('androidx/media3/exoplayer/analytics/AnalyticsCollector')]
  JAnalyticsCollector = interface(JPlayer_Listener)
    ['{05B690D9-C65E-4A5F-BA9D-FE2DDDC43EF5}']
    procedure addListener(analyticslistener: JAnalyticsListener); cdecl;
    procedure notifySeekStarted; cdecl;
    procedure onAudioCodecError(exception: JException); cdecl;
    procedure onAudioDecoderInitialized(string_1: JString; long: Int64; long_1: Int64); cdecl;
    procedure onAudioDecoderReleased(string_1: JString); cdecl;
    procedure onAudioDisabled(decodercounters: JDecoderCounters); cdecl;
    procedure onAudioEnabled(decodercounters: JDecoderCounters); cdecl;
    procedure onAudioInputFormatChanged(format: JFormat; decoderreuseevaluation: JDecoderReuseEvaluation); cdecl;
    procedure onAudioPositionAdvancing(long: Int64); cdecl;
    procedure onAudioSinkError(exception: JException); cdecl;
    procedure onAudioTrackInitialized(audiotrackconfig: JAudioSink_AudioTrackConfig); cdecl;
    procedure onAudioTrackReleased(audiotrackconfig: JAudioSink_AudioTrackConfig); cdecl;
    procedure onAudioUnderrun(int: Integer; long: Int64; long_1: Int64); cdecl;
    procedure onDroppedFrames(int: Integer; long: Int64); cdecl;
    procedure onRenderedFirstFrame(object_1: JObject; long: Int64); cdecl;
    procedure onVideoCodecError(exception: JException); cdecl;
    procedure onVideoDecoderInitialized(string_1: JString; long: Int64; long_1: Int64); cdecl;
    procedure onVideoDecoderReleased(string_1: JString); cdecl;
    procedure onVideoDisabled(decodercounters: JDecoderCounters); cdecl;
    procedure onVideoEnabled(decodercounters: JDecoderCounters); cdecl;
    procedure onVideoFrameProcessingOffset(long: Int64; int: Integer); cdecl;
    procedure onVideoInputFormatChanged(format: JFormat; decoderreuseevaluation: JDecoderReuseEvaluation); cdecl;
    procedure release; cdecl;
    procedure removeListener(analyticslistener: JAnalyticsListener); cdecl;
    procedure setPlayer(player: JPlayer; looper: JLooper); cdecl;
    procedure updateMediaPeriodQueueInfo(list: JList; mediaperiodid: JMediaSource_MediaPeriodId); cdecl;
  end;
  TJAnalyticsCollector = class(TJavaGenericImport<JAnalyticsCollectorClass, JAnalyticsCollector>) end;

  JSeekParametersClass = interface(JObjectClass)
    ['{AB41820C-FAC7-4234-92B8-CB85FB1C1E65}']
    {class} function _GetCLOSEST_SYNC: JSeekParameters; cdecl;
    {class} function _GetDEFAULT: JSeekParameters; cdecl;
    {class} function _GetEXACT: JSeekParameters; cdecl;
    {class} function _GetNEXT_SYNC: JSeekParameters; cdecl;
    {class} function _GetPREVIOUS_SYNC: JSeekParameters; cdecl;
    {class} function init(long: Int64; long_1: Int64): JSeekParameters; cdecl;
    {class} property CLOSEST_SYNC: JSeekParameters read _GetCLOSEST_SYNC;
    {class} property &DEFAULT: JSeekParameters read _GetDEFAULT;
    {class} property EXACT: JSeekParameters read _GetEXACT;
    {class} property NEXT_SYNC: JSeekParameters read _GetNEXT_SYNC;
    {class} property PREVIOUS_SYNC: JSeekParameters read _GetPREVIOUS_SYNC;
  end;

  [JavaSignature('androidx/media3/exoplayer/SeekParameters')]
  JSeekParameters = interface(JObject)
    ['{B2E07C52-B4B4-4F25-9BA3-F4ADAC65E24F}']
    function _GettoleranceAfterUs: Int64; cdecl;
    function _GettoleranceBeforeUs: Int64; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function resolveSeekPositionUs(long: Int64; long_1: Int64; long_2: Int64): Int64; cdecl;
    property toleranceAfterUs: Int64 read _GettoleranceAfterUs;
    property toleranceBeforeUs: Int64 read _GettoleranceBeforeUs;
  end;
  TJSeekParameters = class(TJavaGenericImport<JSeekParametersClass, JSeekParameters>) end;

  JRendererConfigurationClass = interface(JObjectClass)
    ['{64566A76-A1F1-485A-BDFF-BE63A97C409A}']
    {class} function _GetDEFAULT: JRendererConfiguration; cdecl;
    {class} function init(int: Integer; boolean: Boolean): JRendererConfiguration; cdecl; overload;
    {class} function init(boolean: Boolean): JRendererConfiguration; cdecl; overload;
    {class} property &DEFAULT: JRendererConfiguration read _GetDEFAULT;
  end;

  [JavaSignature('androidx/media3/exoplayer/RendererConfiguration')]
  JRendererConfiguration = interface(JObject)
    ['{8FDE5822-6FD0-484D-AC3C-1BA906DD58E2}']
    function _GetoffloadModePreferred: Integer; cdecl;
    function _Gettunneling: Boolean; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    property offloadModePreferred: Integer read _GetoffloadModePreferred;
    property tunneling: Boolean read _Gettunneling;
  end;
  TJRendererConfiguration = class(TJavaGenericImport<JRendererConfigurationClass, JRendererConfiguration>) end;

  JRendererCapabilitiesClass = interface(IJavaClass)
    ['{3D1F629A-F9F3-48BC-ABE5-E030B31086F6}']
    {class} function _GetADAPTIVE_NOT_SEAMLESS: Integer; cdecl;
    {class} function _GetADAPTIVE_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetADAPTIVE_SEAMLESS: Integer; cdecl;
    {class} function _GetADAPTIVE_SUPPORT_MASK: Integer; cdecl;
    {class} function _GetAUDIO_OFFLOAD_GAPLESS_SUPPORTED: Integer; cdecl;
    {class} function _GetAUDIO_OFFLOAD_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetAUDIO_OFFLOAD_SPEED_CHANGE_SUPPORTED: Integer; cdecl;
    {class} function _GetAUDIO_OFFLOAD_SUPPORTED: Integer; cdecl;
    {class} function _GetAUDIO_OFFLOAD_SUPPORT_MASK: Integer; cdecl;
    {class} function _GetDECODER_SUPPORT_FALLBACK: Integer; cdecl;
    {class} function _GetDECODER_SUPPORT_FALLBACK_MIMETYPE: Integer; cdecl;
    {class} function _GetDECODER_SUPPORT_MASK: Integer; cdecl;
    {class} function _GetDECODER_SUPPORT_PRIMARY: Integer; cdecl;
    {class} function _GetFORMAT_EXCEEDS_CAPABILITIES: Integer; cdecl;
    {class} function _GetFORMAT_HANDLED: Integer; cdecl;
    {class} function _GetFORMAT_SUPPORT_MASK: Integer; cdecl;
    {class} function _GetFORMAT_UNSUPPORTED_DRM: Integer; cdecl;
    {class} function _GetFORMAT_UNSUPPORTED_SUBTYPE: Integer; cdecl;
    {class} function _GetFORMAT_UNSUPPORTED_TYPE: Integer; cdecl;
    {class} function _GetHARDWARE_ACCELERATION_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetHARDWARE_ACCELERATION_SUPPORTED: Integer; cdecl;
    {class} function _GetHARDWARE_ACCELERATION_SUPPORT_MASK: Integer; cdecl;
    {class} function _GetTUNNELING_NOT_SUPPORTED: Integer; cdecl;
    {class} function _GetTUNNELING_SUPPORTED: Integer; cdecl;
    {class} function _GetTUNNELING_SUPPORT_MASK: Integer; cdecl;
    {class} function create(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer; int_5: Integer): Integer; cdecl; overload;
    {class} function create(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer; int_4: Integer): Integer; cdecl; overload;
    {class} function create(int: Integer): Integer; cdecl; overload;
    {class} function create(int: Integer; int_1: Integer; int_2: Integer): Integer; cdecl; overload;
    {class} function create(int: Integer; int_1: Integer; int_2: Integer; int_3: Integer): Integer; cdecl; overload;
    {class} function getAdaptiveSupport(int: Integer): Integer; cdecl;
    {class} function getAudioOffloadSupport(int: Integer): Integer; cdecl;
    {class} function getDecoderSupport(int: Integer): Integer; cdecl;
    {class} function getFormatSupport(int: Integer): Integer; cdecl;
    {class} function getHardwareAccelerationSupport(int: Integer): Integer; cdecl;
    {class} function getTunnelingSupport(int: Integer): Integer; cdecl;
    {class} property ADAPTIVE_NOT_SEAMLESS: Integer read _GetADAPTIVE_NOT_SEAMLESS;
    {class} property ADAPTIVE_NOT_SUPPORTED: Integer read _GetADAPTIVE_NOT_SUPPORTED;
    {class} property ADAPTIVE_SEAMLESS: Integer read _GetADAPTIVE_SEAMLESS;
    {class} property ADAPTIVE_SUPPORT_MASK: Integer read _GetADAPTIVE_SUPPORT_MASK;
    {class} property AUDIO_OFFLOAD_GAPLESS_SUPPORTED: Integer read _GetAUDIO_OFFLOAD_GAPLESS_SUPPORTED;
    {class} property AUDIO_OFFLOAD_NOT_SUPPORTED: Integer read _GetAUDIO_OFFLOAD_NOT_SUPPORTED;
    {class} property AUDIO_OFFLOAD_SPEED_CHANGE_SUPPORTED: Integer read _GetAUDIO_OFFLOAD_SPEED_CHANGE_SUPPORTED;
    {class} property AUDIO_OFFLOAD_SUPPORTED: Integer read _GetAUDIO_OFFLOAD_SUPPORTED;
    {class} property AUDIO_OFFLOAD_SUPPORT_MASK: Integer read _GetAUDIO_OFFLOAD_SUPPORT_MASK;
    {class} property DECODER_SUPPORT_FALLBACK: Integer read _GetDECODER_SUPPORT_FALLBACK;
    {class} property DECODER_SUPPORT_FALLBACK_MIMETYPE: Integer read _GetDECODER_SUPPORT_FALLBACK_MIMETYPE;
    {class} property DECODER_SUPPORT_MASK: Integer read _GetDECODER_SUPPORT_MASK;
    {class} property DECODER_SUPPORT_PRIMARY: Integer read _GetDECODER_SUPPORT_PRIMARY;
    {class} property FORMAT_EXCEEDS_CAPABILITIES: Integer read _GetFORMAT_EXCEEDS_CAPABILITIES;
    {class} property FORMAT_HANDLED: Integer read _GetFORMAT_HANDLED;
    {class} property FORMAT_SUPPORT_MASK: Integer read _GetFORMAT_SUPPORT_MASK;
    {class} property FORMAT_UNSUPPORTED_DRM: Integer read _GetFORMAT_UNSUPPORTED_DRM;
    {class} property FORMAT_UNSUPPORTED_SUBTYPE: Integer read _GetFORMAT_UNSUPPORTED_SUBTYPE;
    {class} property FORMAT_UNSUPPORTED_TYPE: Integer read _GetFORMAT_UNSUPPORTED_TYPE;
    {class} property HARDWARE_ACCELERATION_NOT_SUPPORTED: Integer read _GetHARDWARE_ACCELERATION_NOT_SUPPORTED;
    {class} property HARDWARE_ACCELERATION_SUPPORTED: Integer read _GetHARDWARE_ACCELERATION_SUPPORTED;
    {class} property HARDWARE_ACCELERATION_SUPPORT_MASK: Integer read _GetHARDWARE_ACCELERATION_SUPPORT_MASK;
    {class} property TUNNELING_NOT_SUPPORTED: Integer read _GetTUNNELING_NOT_SUPPORTED;
    {class} property TUNNELING_SUPPORTED: Integer read _GetTUNNELING_SUPPORTED;
    {class} property TUNNELING_SUPPORT_MASK: Integer read _GetTUNNELING_SUPPORT_MASK;
  end;

  [JavaSignature('androidx/media3/exoplayer/RendererCapabilities')]
  JRendererCapabilities = interface(IJavaInstance)
    ['{899BC072-F51A-42F4-8168-38C5AC86396C}']
    procedure clearListener; cdecl;
    function getName: JString; cdecl;
    function getTrackType: Integer; cdecl;
    procedure setListener(listener: JRendererCapabilities_Listener); cdecl;
    function supportsFormat(format: JFormat): Integer; cdecl;
    function supportsMixedMimeTypeAdaptation: Integer; cdecl;
  end;
  TJRendererCapabilities = class(TJavaGenericImport<JRendererCapabilitiesClass, JRendererCapabilities>) end;

  JRendererCapabilities_ListenerClass = interface(IJavaClass)
    ['{787F4889-764B-4E61-9E11-EF58FCE548CC}']
  end;

  [JavaSignature('androidx/media3/exoplayer/RendererCapabilities$Listener')]
  JRendererCapabilities_Listener = interface(IJavaInstance)
    ['{2E92E5C7-2CF0-456D-93C5-982D25871C02}']
    procedure onRendererCapabilitiesChanged(renderer: JRenderer); cdecl;
  end;
  TJRendererCapabilities_Listener = class(TJavaGenericImport<JRendererCapabilities_ListenerClass, JRendererCapabilities_Listener>) end;

  JPlayerMessageClass = interface(JObjectClass)
    ['{6D7B6561-F25A-4444-A3AE-0F123FBA8C50}']
    {class} function init(sender: JPlayerMessage_Sender; target: JPlayerMessage_Target; timeline: JTimeline; int: Integer; clock: JClock; looper: JLooper): JPlayerMessage; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/PlayerMessage')]
  JPlayerMessage = interface(JObject)
    ['{C4FB4679-B7F5-4669-9E02-620AC2924B72}']
    function blockUntilDelivered: Boolean; cdecl; overload;
    function blockUntilDelivered(long: Int64): Boolean; cdecl; overload;
    function cancel: JPlayerMessage; cdecl;
    function getDeleteAfterDelivery: Boolean; cdecl;
    function getLooper: JLooper; cdecl;
    function getMediaItemIndex: Integer; cdecl;
    function getPayload: JObject; cdecl;
    function getPositionMs: Int64; cdecl;
    function getTarget: JPlayerMessage_Target; cdecl;
    function getTimeline: JTimeline; cdecl;
    function getType: Integer; cdecl;
    function isCanceled: Boolean; cdecl;
    procedure markAsProcessed(boolean: Boolean); cdecl;
    function send: JPlayerMessage; cdecl;
    function setDeleteAfterDelivery(boolean: Boolean): JPlayerMessage; cdecl;
    function setHandler(handler: JHandler): JPlayerMessage; cdecl;
    function setLooper(looper: JLooper): JPlayerMessage; cdecl;
    function setPayload(object_1: JObject): JPlayerMessage; cdecl;
    function setPosition(int: Integer; long: Int64): JPlayerMessage; cdecl; overload;
    function setPosition(long: Int64): JPlayerMessage; cdecl; overload;
    function setType(int: Integer): JPlayerMessage; cdecl;
  end;
  TJPlayerMessage = class(TJavaGenericImport<JPlayerMessageClass, JPlayerMessage>) end;

  JPlayerMessage_TargetClass = interface(IJavaClass)
    ['{1E59CB23-1062-4ECF-803C-7A6858CD721C}']
  end;

  [JavaSignature('androidx/media3/exoplayer/PlayerMessage$Target')]
  JPlayerMessage_Target = interface(IJavaInstance)
    ['{FC58169F-5D29-412A-B35F-B1EFC265D448}']
    procedure handleMessage(int: Integer; object_1: JObject); cdecl;
  end;
  TJPlayerMessage_Target = class(TJavaGenericImport<JPlayerMessage_TargetClass, JPlayerMessage_Target>) end;

  JPlayerMessage_SenderClass = interface(IJavaClass)
    ['{12EEE3AE-E3EA-4C4F-8277-E6021615E792}']
  end;

  [JavaSignature('androidx/media3/exoplayer/PlayerMessage$Sender')]
  JPlayerMessage_Sender = interface(IJavaInstance)
    ['{1E5EA909-80D6-47CB-9D03-75A88C79C8CE}']
    procedure sendMessage(playermessage: JPlayerMessage); cdecl;
  end;
  TJPlayerMessage_Sender = class(TJavaGenericImport<JPlayerMessage_SenderClass, JPlayerMessage_Sender>) end;

  JMediaClockClass = interface(IJavaClass)
    ['{7F42059A-AA5E-4926-959E-E08288C1E784}']
  end;

  [JavaSignature('androidx/media3/exoplayer/MediaClock')]
  JMediaClock = interface(IJavaInstance)
    ['{BCF22858-5912-4325-B4E0-8A2D51694D91}']
    function getPlaybackParameters: JPlaybackParameters; cdecl;
    function getPositionUs: Int64; cdecl;
    function hasSkippedSilenceSinceLastCall: Boolean; cdecl;
    procedure setPlaybackParameters(playbackparameters: JPlaybackParameters); cdecl;
  end;
  TJMediaClock = class(TJavaGenericImport<JMediaClockClass, JMediaClock>) end;

  JLoadingInfoClass = interface(JObjectClass)
    ['{159EF022-F4B3-48F8-8A4F-83132061D61F}']
  end;

  [JavaSignature('androidx/media3/exoplayer/LoadingInfo')]
  JLoadingInfo = interface(JObject)
    ['{74108781-EE79-452C-8625-FA93D8551D9B}']
    function _GetlastRebufferRealtimeMs: Int64; cdecl;
    function _GetplaybackPositionUs: Int64; cdecl;
    function _GetplaybackSpeed: Single; cdecl;
    function buildUpon: JLoadingInfo_Builder; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    function rebufferedSince(long: Int64): Boolean; cdecl;
    property lastRebufferRealtimeMs: Int64 read _GetlastRebufferRealtimeMs;
    property playbackPositionUs: Int64 read _GetplaybackPositionUs;
    property playbackSpeed: Single read _GetplaybackSpeed;
  end;
  TJLoadingInfo = class(TJavaGenericImport<JLoadingInfoClass, JLoadingInfo>) end;

  JLoadingInfo_BuilderClass = interface(JObjectClass)
    ['{EF7A64F4-58F8-4575-A06F-68CF7E0496A4}']
    {class} function init: JLoadingInfo_Builder; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/LoadingInfo$Builder')]
  JLoadingInfo_Builder = interface(JObject)
    ['{F2515925-232B-4B9C-93C2-12A0B1D6C22E}']
    function build: JLoadingInfo; cdecl;
    function setLastRebufferRealtimeMs(long: Int64): JLoadingInfo_Builder; cdecl;
    function setPlaybackPositionUs(long: Int64): JLoadingInfo_Builder; cdecl;
    function setPlaybackSpeed(float: Single): JLoadingInfo_Builder; cdecl;
  end;
  TJLoadingInfo_Builder = class(TJavaGenericImport<JLoadingInfo_BuilderClass, JLoadingInfo_Builder>) end;

  JFormatHolderClass = interface(JObjectClass)
    ['{1F9ED697-2369-4F43-AA9D-78DE10648AA0}']
    {class} function init: JFormatHolder; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/FormatHolder')]
  JFormatHolder = interface(JObject)
    ['{D4FCDEE0-C4A6-4F06-871C-D649C51DE2DF}']
    function _GetdrmSession: JDrmSession; cdecl;
    function _Getformat: JFormat; cdecl;
    procedure clear; cdecl;
    property drmSession: JDrmSession read _GetdrmSession;
    property format: JFormat read _Getformat;
  end;
  TJFormatHolder = class(TJavaGenericImport<JFormatHolderClass, JFormatHolder>) end;

  JExoPlayer_BuilderClass = interface(JObjectClass)
    ['{42E12DDD-2FF2-4D50-9378-758AE4E96431}']
    {class} function init(context: JContext): JExoPlayer_Builder; cdecl; overload;
//    {class} function init(context: JContext; renderersfactory: JRenderersFactory; factory: JMediaSource_Factory; trackselector: JTrackSelector; loadcontrol: JLoadControl; bandwidthmeter: JBandwidthMeter; analyticscollector: JAnalyticsCollector): JExoPlayer_Builder; cdecl; overload;
//    {class} function init(context: JContext; renderersfactory: JRenderersFactory): JExoPlayer_Builder; cdecl; overload;
//    {class} function init(context: JContext; factory: JMediaSource_Factory): JExoPlayer_Builder; cdecl; overload;
//    {class} function init(context: JContext; renderersfactory: JRenderersFactory; factory: JMediaSource_Factory): JExoPlayer_Builder; cdecl; overload;
  end;

  [JavaSignature('androidx/media3/exoplayer/ExoPlayer$Builder')]
  JExoPlayer_Builder = interface(JObject)
    ['{20EE6A34-846F-46B4-A495-883E8DCDC189}']
    function build: JExoPlayer; cdecl;
    function experimentalSetForegroundModeTimeoutMs(long: Int64): JExoPlayer_Builder; cdecl;
    function setAnalyticsCollector(analyticscollector: JAnalyticsCollector): JExoPlayer_Builder; cdecl;
    function setAudioAttributes(audioattributes: JAudioAttributes; boolean: Boolean): JExoPlayer_Builder; cdecl;
    function setBandwidthMeter(bandwidthmeter: JBandwidthMeter): JExoPlayer_Builder; cdecl;
    function setClock(clock: JClock): JExoPlayer_Builder; cdecl;
    function setDetachSurfaceTimeoutMs(long: Int64): JExoPlayer_Builder; cdecl;
    function setDeviceVolumeControlEnabled(boolean: Boolean): JExoPlayer_Builder; cdecl;
    function setHandleAudioBecomingNoisy(boolean: Boolean): JExoPlayer_Builder; cdecl;
//    function setLivePlaybackSpeedControl(liveplaybackspeedcontrol: JLivePlaybackSpeedControl): JExoPlayer_Builder; cdecl;
//    function setLoadControl(loadcontrol: JLoadControl): JExoPlayer_Builder; cdecl;
    function setLooper(looper: JLooper): JExoPlayer_Builder; cdecl;
//    function setMediaSourceFactory(factory: JMediaSource_Factory): JExoPlayer_Builder; cdecl;
    function setPauseAtEndOfMediaItems(boolean: Boolean): JExoPlayer_Builder; cdecl;
    function setPlaybackLooper(looper: JLooper): JExoPlayer_Builder; cdecl;
    function setPriorityTaskManager(prioritytaskmanager: JPriorityTaskManager): JExoPlayer_Builder; cdecl;
    function setReleaseTimeoutMs(long: Int64): JExoPlayer_Builder; cdecl;
//    function setRenderersFactory(renderersfactory: JRenderersFactory): JExoPlayer_Builder; cdecl;
    function setSeekBackIncrementMs(long: Int64): JExoPlayer_Builder; cdecl;
    function setSeekForwardIncrementMs(long: Int64): JExoPlayer_Builder; cdecl;
    function setSeekParameters(seekparameters: JSeekParameters): JExoPlayer_Builder; cdecl;
    function setSkipSilenceEnabled(boolean: Boolean): JExoPlayer_Builder; cdecl;
    function setSuppressPlaybackOnUnsuitableOutput(boolean: Boolean): JExoPlayer_Builder; cdecl;
    function setTrackSelector(trackselector: JTrackSelector): JExoPlayer_Builder; cdecl;
    function setUseLazyPreparation(boolean: Boolean): JExoPlayer_Builder; cdecl;
    function setUsePlatformDiagnostics(boolean: Boolean): JExoPlayer_Builder; cdecl;
    function setVideoChangeFrameRateStrategy(int: Integer): JExoPlayer_Builder; cdecl;
    function setVideoScalingMode(int: Integer): JExoPlayer_Builder; cdecl;
    function setWakeMode(int: Integer): JExoPlayer_Builder; cdecl;
  end;
  TJExoPlayer_Builder = class(TJavaGenericImport<JExoPlayer_BuilderClass, JExoPlayer_Builder>) end;

  JExoPlayerClass = interface(JPlayerClass)
    ['{147B9290-9233-4AAE-B351-5CF9213BF28C}']
    {class} function _GetDEFAULT_DETACH_SURFACE_TIMEOUT_MS: Int64; cdecl;
    {class} function _GetDEFAULT_RELEASE_TIMEOUT_MS: Int64; cdecl;
    {class} property DEFAULT_DETACH_SURFACE_TIMEOUT_MS: Int64 read _GetDEFAULT_DETACH_SURFACE_TIMEOUT_MS;
    {class} property DEFAULT_RELEASE_TIMEOUT_MS: Int64 read _GetDEFAULT_RELEASE_TIMEOUT_MS;
  end;

  [JavaSignature('androidx/media3/exoplayer/ExoPlayer')]
  JExoPlayer = interface(JPlayer)
    ['{3946EF3A-579E-41D4-9833-5623626DB8DE}']
    procedure addAnalyticsListener(analyticslistener: JAnalyticsListener); cdecl;
    procedure addAudioOffloadListener(audiooffloadlistener: JExoPlayer_AudioOffloadListener); cdecl;
    procedure addMediaSource(int: Integer; mediasource: JMediaSource); cdecl; overload;
    procedure addMediaSource(mediasource: JMediaSource); cdecl; overload;
    procedure addMediaSources(int: Integer; list: JList); cdecl; overload;
    procedure addMediaSources(list: JList); cdecl; overload;
    procedure clearAuxEffectInfo; cdecl;
    procedure clearCameraMotionListener(cameramotionlistener: JCameraMotionListener); cdecl;
    procedure clearVideoFrameMetadataListener(videoframemetadatalistener: JVideoFrameMetadataListener); cdecl;
    function createMessage(target: JPlayerMessage_Target): JPlayerMessage; cdecl;
    function getAnalyticsCollector: JAnalyticsCollector; cdecl;
    function getAudioComponent: JExoPlayer_AudioComponent; cdecl;
    function getAudioDecoderCounters: JDecoderCounters; cdecl;
    function getAudioFormat: JFormat; cdecl;
    function getAudioSessionId: Integer; cdecl;
    function getClock: JClock; cdecl;
    function getCurrentTrackGroups: JTrackGroupArray; cdecl;
    function getCurrentTrackSelections: JTrackSelectionArray; cdecl;
    function getDeviceComponent: JExoPlayer_DeviceComponent; cdecl;
    function getPauseAtEndOfMediaItems: Boolean; cdecl;
    function getPlaybackLooper: JLooper; cdecl;
//    function getPlayerError: JExoPlaybackException; cdecl; overload;
//    function getPlayerError: JPlaybackException; cdecl; overload;
    function getRenderer(int: Integer): JRenderer; cdecl;
    function getRendererCount: Integer; cdecl;
    function getRendererType(int: Integer): Integer; cdecl;
    function getSeekParameters: JSeekParameters; cdecl;
    function getSkipSilenceEnabled: Boolean; cdecl;
    function getTextComponent: JExoPlayer_TextComponent; cdecl;
    function getTrackSelector: JTrackSelector; cdecl;
    function getVideoChangeFrameRateStrategy: Integer; cdecl;
    function getVideoComponent: JExoPlayer_VideoComponent; cdecl;
    function getVideoDecoderCounters: JDecoderCounters; cdecl;
    function getVideoFormat: JFormat; cdecl;
    function getVideoScalingMode: Integer; cdecl;
    function isSleepingForOffload: Boolean; cdecl;
    function isTunnelingEnabled: Boolean; cdecl;
    procedure prepare(mediasource: JMediaSource); cdecl; overload;
    procedure prepare(mediasource: JMediaSource; boolean: Boolean; boolean_1: Boolean); cdecl; overload;
    procedure removeAnalyticsListener(analyticslistener: JAnalyticsListener); cdecl;
    procedure removeAudioOffloadListener(audiooffloadlistener: JExoPlayer_AudioOffloadListener); cdecl;
    procedure replaceMediaItem(int: Integer; mediaitem: JMediaItem); cdecl;
    procedure replaceMediaItems(int: Integer; int_1: Integer; list: JList); cdecl;
    procedure setAudioSessionId(int: Integer); cdecl;
    procedure setAuxEffectInfo(auxeffectinfo: JAuxEffectInfo); cdecl;
    procedure setCameraMotionListener(cameramotionlistener: JCameraMotionListener); cdecl;
    procedure setForegroundMode(boolean: Boolean); cdecl;
    procedure setHandleAudioBecomingNoisy(boolean: Boolean); cdecl;
    procedure setImageOutput(imageoutput: JImageOutput); cdecl;
    procedure setMediaSource(mediasource: JMediaSource; boolean: Boolean); cdecl; overload;
    procedure setMediaSource(mediasource: JMediaSource; long: Int64); cdecl; overload;
    procedure setMediaSource(mediasource: JMediaSource); cdecl; overload;
    procedure setMediaSources(list: JList); cdecl; overload;
    procedure setMediaSources(list: JList; int: Integer; long: Int64); cdecl; overload;
    procedure setMediaSources(list: JList; boolean: Boolean); cdecl; overload;
    procedure setPauseAtEndOfMediaItems(boolean: Boolean); cdecl;
    procedure setPreferredAudioDevice(audiodeviceinfo: JAudioDeviceInfo); cdecl;
    procedure setPriorityTaskManager(prioritytaskmanager: JPriorityTaskManager); cdecl;
    procedure setSeekParameters(seekparameters: JSeekParameters); cdecl;
    procedure setShuffleOrder(shuffleorder: JShuffleOrder); cdecl;
    procedure setSkipSilenceEnabled(boolean: Boolean); cdecl;
    procedure setVideoChangeFrameRateStrategy(int: Integer); cdecl;
    procedure setVideoEffects(list: JList); cdecl;
    procedure setVideoFrameMetadataListener(videoframemetadatalistener: JVideoFrameMetadataListener); cdecl;
    procedure setVideoScalingMode(int: Integer); cdecl;
    procedure setWakeMode(int: Integer); cdecl;
  end;
  TJExoPlayer = class(TJavaGenericImport<JExoPlayerClass, JExoPlayer>) end;

  JExoPlayer_VideoComponentClass = interface(IJavaClass)
    ['{7E68DACB-1E0F-4118-AD17-FC39C5FBCDEB}']
  end;

  [JavaSignature('androidx/media3/exoplayer/ExoPlayer$VideoComponent')]
  JExoPlayer_VideoComponent = interface(IJavaInstance)
    ['{AB1ACB9C-C006-4244-8766-8592C1AC1AA5}']
    procedure clearCameraMotionListener(cameramotionlistener: JCameraMotionListener); cdecl;
    procedure clearVideoFrameMetadataListener(videoframemetadatalistener: JVideoFrameMetadataListener); cdecl;
    procedure clearVideoSurface; cdecl; overload;
    procedure clearVideoSurface(surface: JSurface); cdecl; overload;
    procedure clearVideoSurfaceHolder(surfaceholder: JSurfaceHolder); cdecl;
    procedure clearVideoSurfaceView(surfaceview: JSurfaceView); cdecl;
    procedure clearVideoTextureView(textureview: JTextureView); cdecl;
    function getVideoChangeFrameRateStrategy: Integer; cdecl;
    function getVideoScalingMode: Integer; cdecl;
    function getVideoSize: JVideoSize; cdecl;
    procedure setCameraMotionListener(cameramotionlistener: JCameraMotionListener); cdecl;
    procedure setVideoChangeFrameRateStrategy(int: Integer); cdecl;
    procedure setVideoFrameMetadataListener(videoframemetadatalistener: JVideoFrameMetadataListener); cdecl;
    procedure setVideoScalingMode(int: Integer); cdecl;
    procedure setVideoSurface(surface: JSurface); cdecl;
    procedure setVideoSurfaceHolder(surfaceholder: JSurfaceHolder); cdecl;
    procedure setVideoSurfaceView(surfaceview: JSurfaceView); cdecl;
    procedure setVideoTextureView(textureview: JTextureView); cdecl;
  end;
  TJExoPlayer_VideoComponent = class(TJavaGenericImport<JExoPlayer_VideoComponentClass, JExoPlayer_VideoComponent>) end;

  JExoPlayer_TextComponentClass = interface(IJavaClass)
    ['{2B5B8EE6-99EF-4794-836E-1FE3A81A2898}']
  end;

  [JavaSignature('androidx/media3/exoplayer/ExoPlayer$TextComponent')]
  JExoPlayer_TextComponent = interface(IJavaInstance)
    ['{80D8B963-5936-4233-8578-3059A7253200}']
    function getCurrentCues: JCueGroup; cdecl;
  end;
  TJExoPlayer_TextComponent = class(TJavaGenericImport<JExoPlayer_TextComponentClass, JExoPlayer_TextComponent>) end;

  JExoPlayer_DeviceComponentClass = interface(IJavaClass)
    ['{4BCA0906-F141-4C16-8764-678493972005}']
  end;

  [JavaSignature('androidx/media3/exoplayer/ExoPlayer$DeviceComponent')]
  JExoPlayer_DeviceComponent = interface(IJavaInstance)
    ['{7095E9E9-1EF4-4524-9A3B-608F41233A11}']
    procedure decreaseDeviceVolume; cdecl;
    function getDeviceInfo: JDeviceInfo; cdecl;
    function getDeviceVolume: Integer; cdecl;
    procedure increaseDeviceVolume; cdecl;
    function isDeviceMuted: Boolean; cdecl;
    procedure setDeviceMuted(boolean: Boolean); cdecl;
    procedure setDeviceVolume(int: Integer); cdecl;
  end;
  TJExoPlayer_DeviceComponent = class(TJavaGenericImport<JExoPlayer_DeviceComponentClass, JExoPlayer_DeviceComponent>) end;

  JExoPlayer_AudioOffloadListenerClass = interface(IJavaClass)
    ['{E7AF28CA-6FD4-4975-9425-A49040017D32}']
  end;

  [JavaSignature('androidx/media3/exoplayer/ExoPlayer$AudioOffloadListener')]
  JExoPlayer_AudioOffloadListener = interface(IJavaInstance)
    ['{4DEB4E4C-7854-4A2F-BFD5-0E2E6F095FE1}']
    procedure onOffloadedPlayback(boolean: Boolean); cdecl;
    procedure onSleepingForOffloadChanged(boolean: Boolean); cdecl;
  end;
  TJExoPlayer_AudioOffloadListener = class(TJavaGenericImport<JExoPlayer_AudioOffloadListenerClass, JExoPlayer_AudioOffloadListener>) end;

  JExoPlayer_AudioComponentClass = interface(IJavaClass)
    ['{B1FB8FAD-5317-47E9-9B0E-2B9DE8B6F8EB}']
  end;

  [JavaSignature('androidx/media3/exoplayer/ExoPlayer$AudioComponent')]
  JExoPlayer_AudioComponent = interface(IJavaInstance)
    ['{131108BF-C5CB-4F6F-B680-667B8EFE4502}']
    procedure clearAuxEffectInfo; cdecl;
    function getAudioAttributes: JAudioAttributes; cdecl;
    function getAudioSessionId: Integer; cdecl;
    function getSkipSilenceEnabled: Boolean; cdecl;
    function getVolume: Single; cdecl;
    procedure setAudioAttributes(audioattributes: JAudioAttributes; boolean: Boolean); cdecl;
    procedure setAudioSessionId(int: Integer); cdecl;
    procedure setAuxEffectInfo(auxeffectinfo: JAuxEffectInfo); cdecl;
    procedure setSkipSilenceEnabled(boolean: Boolean); cdecl;
    procedure setVolume(float: Single); cdecl;
  end;
  TJExoPlayer_AudioComponent = class(TJavaGenericImport<JExoPlayer_AudioComponentClass, JExoPlayer_AudioComponent>) end;

  JExoPlaybackExceptionClass = interface(JPlaybackExceptionClass)
    ['{B11D827F-DC11-479F-8087-32569F191880}']
    {class} function _GetCREATOR: JBundleable_Creator; cdecl;
    {class} function _GetTYPE_REMOTE: Integer; cdecl;
    {class} function _GetTYPE_RENDERER: Integer; cdecl;
    {class} function _GetTYPE_SOURCE: Integer; cdecl;
    {class} function _GetTYPE_UNEXPECTED: Integer; cdecl;
    {class} function createForRemote(string_1: JString): JExoPlaybackException; cdecl;
    {class} function createForRenderer(throwable: JThrowable; string_1: JString; int: Integer; format: JFormat; int_1: Integer; boolean: Boolean; int_2: Integer): JExoPlaybackException; cdecl;
    {class} function createForSource(ioexception: JIOException; int: Integer): JExoPlaybackException; cdecl;
    {class} function createForUnexpected(runtimeexception: JRuntimeException; int: Integer): JExoPlaybackException; cdecl; overload;
    {class} function createForUnexpected(runtimeexception: JRuntimeException): JExoPlaybackException; cdecl; overload;
    {class} function fromBundle(bundle: JBundle): JExoPlaybackException; cdecl;
    {class} property CREATOR: JBundleable_Creator read _GetCREATOR;
    {class} property TYPE_REMOTE: Integer read _GetTYPE_REMOTE;
    {class} property TYPE_RENDERER: Integer read _GetTYPE_RENDERER;
    {class} property TYPE_SOURCE: Integer read _GetTYPE_SOURCE;
    {class} property TYPE_UNEXPECTED: Integer read _GetTYPE_UNEXPECTED;
  end;

  [JavaSignature('androidx/media3/exoplayer/ExoPlaybackException')]
  JExoPlaybackException = interface(JPlaybackException)
    ['{C9DA5955-8995-448C-9AA9-EF0914B719B9}']
    function _GetmediaPeriodId: JMediaSource_MediaPeriodId; cdecl;
    function _GetrendererFormat: JFormat; cdecl;
    function _GetrendererFormatSupport: Integer; cdecl;
    function _GetrendererIndex: Integer; cdecl;
    function _GetrendererName: JString; cdecl;
    function _Gettype: Integer; cdecl;
    function errorInfoEquals(playbackexception: JPlaybackException): Boolean; cdecl;
    function getRendererException: JException; cdecl;
    function getSourceException: JIOException; cdecl;
    function getUnexpectedException: JRuntimeException; cdecl;
    function toBundle: JBundle; cdecl;
    property mediaPeriodId: JMediaSource_MediaPeriodId read _GetmediaPeriodId;
    property rendererFormat: JFormat read _GetrendererFormat;
    property rendererFormatSupport: Integer read _GetrendererFormatSupport;
    property rendererIndex: Integer read _GetrendererIndex;
    property rendererName: JString read _GetrendererName;
    property &type: Integer read _Gettype;
  end;
  TJExoPlaybackException = class(TJavaGenericImport<JExoPlaybackExceptionClass, JExoPlaybackException>) end;

  JDecoderReuseEvaluationClass = interface(JObjectClass)
    ['{7FF8262F-D403-4407-9A59-11518363D08D}']
    {class} function _GetDISCARD_REASON_APP_OVERRIDE: Integer; cdecl;
    {class} function _GetDISCARD_REASON_AUDIO_BYPASS_POSSIBLE: Integer; cdecl;
    {class} function _GetDISCARD_REASON_AUDIO_CHANNEL_COUNT_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_AUDIO_ENCODING_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_AUDIO_SAMPLE_RATE_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_DRM_SESSION_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_INITIALIZATION_DATA_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_MAX_INPUT_SIZE_EXCEEDED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_MIME_TYPE_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_OPERATING_RATE_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_REUSE_NOT_IMPLEMENTED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_VIDEO_COLOR_INFO_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_VIDEO_MAX_RESOLUTION_EXCEEDED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_VIDEO_RESOLUTION_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_VIDEO_ROTATION_CHANGED: Integer; cdecl;
    {class} function _GetDISCARD_REASON_WORKAROUND: Integer; cdecl;
    {class} function _GetREUSE_RESULT_NO: Integer; cdecl;
    {class} function _GetREUSE_RESULT_YES_WITHOUT_RECONFIGURATION: Integer; cdecl;
    {class} function _GetREUSE_RESULT_YES_WITH_FLUSH: Integer; cdecl;
    {class} function _GetREUSE_RESULT_YES_WITH_RECONFIGURATION: Integer; cdecl;
    {class} function init(string_1: JString; format: JFormat; format_1: JFormat; int: Integer; int_1: Integer): JDecoderReuseEvaluation; cdecl;
    {class} property DISCARD_REASON_APP_OVERRIDE: Integer read _GetDISCARD_REASON_APP_OVERRIDE;
    {class} property DISCARD_REASON_AUDIO_BYPASS_POSSIBLE: Integer read _GetDISCARD_REASON_AUDIO_BYPASS_POSSIBLE;
    {class} property DISCARD_REASON_AUDIO_CHANNEL_COUNT_CHANGED: Integer read _GetDISCARD_REASON_AUDIO_CHANNEL_COUNT_CHANGED;
    {class} property DISCARD_REASON_AUDIO_ENCODING_CHANGED: Integer read _GetDISCARD_REASON_AUDIO_ENCODING_CHANGED;
    {class} property DISCARD_REASON_AUDIO_SAMPLE_RATE_CHANGED: Integer read _GetDISCARD_REASON_AUDIO_SAMPLE_RATE_CHANGED;
    {class} property DISCARD_REASON_DRM_SESSION_CHANGED: Integer read _GetDISCARD_REASON_DRM_SESSION_CHANGED;
    {class} property DISCARD_REASON_INITIALIZATION_DATA_CHANGED: Integer read _GetDISCARD_REASON_INITIALIZATION_DATA_CHANGED;
    {class} property DISCARD_REASON_MAX_INPUT_SIZE_EXCEEDED: Integer read _GetDISCARD_REASON_MAX_INPUT_SIZE_EXCEEDED;
    {class} property DISCARD_REASON_MIME_TYPE_CHANGED: Integer read _GetDISCARD_REASON_MIME_TYPE_CHANGED;
    {class} property DISCARD_REASON_OPERATING_RATE_CHANGED: Integer read _GetDISCARD_REASON_OPERATING_RATE_CHANGED;
    {class} property DISCARD_REASON_REUSE_NOT_IMPLEMENTED: Integer read _GetDISCARD_REASON_REUSE_NOT_IMPLEMENTED;
    {class} property DISCARD_REASON_VIDEO_COLOR_INFO_CHANGED: Integer read _GetDISCARD_REASON_VIDEO_COLOR_INFO_CHANGED;
    {class} property DISCARD_REASON_VIDEO_MAX_RESOLUTION_EXCEEDED: Integer read _GetDISCARD_REASON_VIDEO_MAX_RESOLUTION_EXCEEDED;
    {class} property DISCARD_REASON_VIDEO_RESOLUTION_CHANGED: Integer read _GetDISCARD_REASON_VIDEO_RESOLUTION_CHANGED;
    {class} property DISCARD_REASON_VIDEO_ROTATION_CHANGED: Integer read _GetDISCARD_REASON_VIDEO_ROTATION_CHANGED;
    {class} property DISCARD_REASON_WORKAROUND: Integer read _GetDISCARD_REASON_WORKAROUND;
    {class} property REUSE_RESULT_NO: Integer read _GetREUSE_RESULT_NO;
    {class} property REUSE_RESULT_YES_WITHOUT_RECONFIGURATION: Integer read _GetREUSE_RESULT_YES_WITHOUT_RECONFIGURATION;
    {class} property REUSE_RESULT_YES_WITH_FLUSH: Integer read _GetREUSE_RESULT_YES_WITH_FLUSH;
    {class} property REUSE_RESULT_YES_WITH_RECONFIGURATION: Integer read _GetREUSE_RESULT_YES_WITH_RECONFIGURATION;
  end;

  [JavaSignature('androidx/media3/exoplayer/DecoderReuseEvaluation')]
  JDecoderReuseEvaluation = interface(JObject)
    ['{827C829E-7CCE-45CD-AA30-7BC7AD32D132}']
    function _GetdecoderName: JString; cdecl;
    function _GetdiscardReasons: Integer; cdecl;
    function _GetnewFormat: JFormat; cdecl;
    function _GetoldFormat: JFormat; cdecl;
    function _Getresult: Integer; cdecl;
    function equals(object_1: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    property decoderName: JString read _GetdecoderName;
    property discardReasons: Integer read _GetdiscardReasons;
    property newFormat: JFormat read _GetnewFormat;
    property oldFormat: JFormat read _GetoldFormat;
    property result: Integer read _Getresult;
  end;
  TJDecoderReuseEvaluation = class(TJavaGenericImport<JDecoderReuseEvaluationClass, JDecoderReuseEvaluation>) end;

  JDecoderCountersClass = interface(JObjectClass)
    ['{D7E6C87C-F79A-4079-9392-136BEB28331D}']
    {class} function init: JDecoderCounters; cdecl;
  end;

  [JavaSignature('androidx/media3/exoplayer/DecoderCounters')]
  JDecoderCounters = interface(JObject)
    ['{A8B0D1A2-49D0-4A08-BE1E-3380CFB25F93}']
    function _GetdecoderInitCount: Integer; cdecl;
    function _GetdecoderReleaseCount: Integer; cdecl;
    function _GetdroppedBufferCount: Integer; cdecl;
    function _GetdroppedInputBufferCount: Integer; cdecl;
    function _GetdroppedToKeyframeCount: Integer; cdecl;
    function _GetmaxConsecutiveDroppedBufferCount: Integer; cdecl;
    function _GetqueuedInputBufferCount: Integer; cdecl;
    function _GetrenderedOutputBufferCount: Integer; cdecl;
    function _GetskippedInputBufferCount: Integer; cdecl;
    function _GetskippedOutputBufferCount: Integer; cdecl;
    function _GettotalVideoFrameProcessingOffsetUs: Int64; cdecl;
    function _GetvideoFrameProcessingOffsetCount: Integer; cdecl;
    procedure addVideoFrameProcessingOffset(long: Int64); cdecl;
    procedure ensureUpdated; cdecl;
    procedure merge(decodercounters: JDecoderCounters); cdecl;
    function toString: JString; cdecl;
    property decoderInitCount: Integer read _GetdecoderInitCount;
    property decoderReleaseCount: Integer read _GetdecoderReleaseCount;
    property droppedBufferCount: Integer read _GetdroppedBufferCount;
    property droppedInputBufferCount: Integer read _GetdroppedInputBufferCount;
    property droppedToKeyframeCount: Integer read _GetdroppedToKeyframeCount;
    property maxConsecutiveDroppedBufferCount: Integer read _GetmaxConsecutiveDroppedBufferCount;
    property queuedInputBufferCount: Integer read _GetqueuedInputBufferCount;
    property renderedOutputBufferCount: Integer read _GetrenderedOutputBufferCount;
    property skippedInputBufferCount: Integer read _GetskippedInputBufferCount;
    property skippedOutputBufferCount: Integer read _GetskippedOutputBufferCount;
    property totalVideoFrameProcessingOffsetUs: Int64 read _GettotalVideoFrameProcessingOffsetUs;
    property videoFrameProcessingOffsetCount: Integer read _GetvideoFrameProcessingOffsetCount;
  end;
  TJDecoderCounters = class(TJavaGenericImport<JDecoderCountersClass, JDecoderCounters>) end;

  JRendererClass = interface(JPlayerMessage_TargetClass)
    ['{A54F4B98-97F8-4627-A640-9FDE426D94D2}']
    {class} function _GetMSG_CUSTOM_BASE: Integer; cdecl;
    {class} function _GetMSG_SET_AUDIO_ATTRIBUTES: Integer; cdecl;
    {class} function _GetMSG_SET_AUDIO_SESSION_ID: Integer; cdecl;
    {class} function _GetMSG_SET_AUX_EFFECT_INFO: Integer; cdecl;
    {class} function _GetMSG_SET_CAMERA_MOTION_LISTENER: Integer; cdecl;
    {class} function _GetMSG_SET_CHANGE_FRAME_RATE_STRATEGY: Integer; cdecl;
    {class} function _GetMSG_SET_IMAGE_OUTPUT: Integer; cdecl;
    {class} function _GetMSG_SET_PREFERRED_AUDIO_DEVICE: Integer; cdecl;
    {class} function _GetMSG_SET_SCALING_MODE: Integer; cdecl;
    {class} function _GetMSG_SET_SKIP_SILENCE_ENABLED: Integer; cdecl;
    {class} function _GetMSG_SET_VIDEO_EFFECTS: Integer; cdecl;
    {class} function _GetMSG_SET_VIDEO_FRAME_METADATA_LISTENER: Integer; cdecl;
    {class} function _GetMSG_SET_VIDEO_OUTPUT: Integer; cdecl;
    {class} function _GetMSG_SET_VIDEO_OUTPUT_RESOLUTION: Integer; cdecl;
    {class} function _GetMSG_SET_VOLUME: Integer; cdecl;
    {class} function _GetMSG_SET_WAKEUP_LISTENER: Integer; cdecl;
    {class} function _GetSTATE_DISABLED: Integer; cdecl;
    {class} function _GetSTATE_ENABLED: Integer; cdecl;
    {class} function _GetSTATE_STARTED: Integer; cdecl;
    {class} property MSG_CUSTOM_BASE: Integer read _GetMSG_CUSTOM_BASE;
    {class} property MSG_SET_AUDIO_ATTRIBUTES: Integer read _GetMSG_SET_AUDIO_ATTRIBUTES;
    {class} property MSG_SET_AUDIO_SESSION_ID: Integer read _GetMSG_SET_AUDIO_SESSION_ID;
    {class} property MSG_SET_AUX_EFFECT_INFO: Integer read _GetMSG_SET_AUX_EFFECT_INFO;
    {class} property MSG_SET_CAMERA_MOTION_LISTENER: Integer read _GetMSG_SET_CAMERA_MOTION_LISTENER;
    {class} property MSG_SET_CHANGE_FRAME_RATE_STRATEGY: Integer read _GetMSG_SET_CHANGE_FRAME_RATE_STRATEGY;
    {class} property MSG_SET_IMAGE_OUTPUT: Integer read _GetMSG_SET_IMAGE_OUTPUT;
    {class} property MSG_SET_PREFERRED_AUDIO_DEVICE: Integer read _GetMSG_SET_PREFERRED_AUDIO_DEVICE;
    {class} property MSG_SET_SCALING_MODE: Integer read _GetMSG_SET_SCALING_MODE;
    {class} property MSG_SET_SKIP_SILENCE_ENABLED: Integer read _GetMSG_SET_SKIP_SILENCE_ENABLED;
    {class} property MSG_SET_VIDEO_EFFECTS: Integer read _GetMSG_SET_VIDEO_EFFECTS;
    {class} property MSG_SET_VIDEO_FRAME_METADATA_LISTENER: Integer read _GetMSG_SET_VIDEO_FRAME_METADATA_LISTENER;
    {class} property MSG_SET_VIDEO_OUTPUT: Integer read _GetMSG_SET_VIDEO_OUTPUT;
    {class} property MSG_SET_VIDEO_OUTPUT_RESOLUTION: Integer read _GetMSG_SET_VIDEO_OUTPUT_RESOLUTION;
    {class} property MSG_SET_VOLUME: Integer read _GetMSG_SET_VOLUME;
    {class} property MSG_SET_WAKEUP_LISTENER: Integer read _GetMSG_SET_WAKEUP_LISTENER;
    {class} property STATE_DISABLED: Integer read _GetSTATE_DISABLED;
    {class} property STATE_ENABLED: Integer read _GetSTATE_ENABLED;
    {class} property STATE_STARTED: Integer read _GetSTATE_STARTED;
  end;

  [JavaSignature('androidx/media3/exoplayer/Renderer')]
  JRenderer = interface(JPlayerMessage_Target)
    ['{4B890321-C28B-4495-85BC-FBA830B8B8B3}']
    procedure disable; cdecl;
    procedure enable(rendererconfiguration: JRendererConfiguration; formats: TJavaObjectArray<JFormat>; samplestream: JSampleStream; long: Int64; boolean: Boolean; boolean_1: Boolean; long_1: Int64; long_2: Int64; mediaperiodid: JMediaSource_MediaPeriodId); cdecl;
    procedure enableMayRenderStartOfStream; cdecl;
    function getCapabilities: JRendererCapabilities; cdecl;
    function getMediaClock: JMediaClock; cdecl;
    function getName: JString; cdecl;
    function getReadingPositionUs: Int64; cdecl;
    function getState: Integer; cdecl;
    function getStream: JSampleStream; cdecl;
    function getTrackType: Integer; cdecl;
    function hasReadStreamToEnd: Boolean; cdecl;
    procedure init(int: Integer; playerid: JPlayerId; clock: JClock); cdecl;
    function isCurrentStreamFinal: Boolean; cdecl;
    function isEnded: Boolean; cdecl;
    function isReady: Boolean; cdecl;
    procedure maybeThrowStreamError; cdecl;
    procedure release; cdecl;
    procedure render(long: Int64; long_1: Int64); cdecl;
    procedure replaceStream(formats: TJavaObjectArray<JFormat>; samplestream: JSampleStream; long: Int64; long_1: Int64; mediaperiodid: JMediaSource_MediaPeriodId); cdecl;
    procedure reset; cdecl;
    procedure resetPosition(long: Int64); cdecl;
    procedure setCurrentStreamFinal; cdecl;
    procedure setPlaybackSpeed(float: Single; float_1: Single); cdecl;
    procedure setTimeline(timeline: JTimeline); cdecl;
    procedure start; cdecl;
    procedure stop; cdecl;
  end;
  TJRenderer = class(TJavaGenericImport<JRendererClass, JRenderer>) end;

implementation

end.
