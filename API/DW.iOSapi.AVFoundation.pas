unit DW.iOSapi.AVFoundation;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.CoreFoundation, Macapi.ObjCRuntime, Macapi.ObjectiveC, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.AVFoundation, iOSapi.CoreMedia, iOSapi.CoreGraphics, iOSapi.CoreVideo,
  // DW
  DW.Macapi.Simd;

const
  AVAudioSessionSetActiveOptionNotifyOthersOnDeactivation = 1;
  AVAuthorizationStatusNotDetermined = 0;
  AVAuthorizationStatusRestricted = 1;
  AVAuthorizationStatusDenied = 2;
  AVAuthorizationStatusAuthorized = 3;
  AVAudioSessionRouteChangeReasonUnknown = 0;
  AVAudioSessionRouteChangeReasonNewDeviceAvailable = 1;
  AVAudioSessionRouteChangeReasonOldDeviceUnavailable = 2;
  AVAudioSessionRouteChangeReasonCategoryChange = 3;
  AVAudioSessionRouteChangeReasonOverride = 4;
  AVAudioSessionRouteChangeReasonWakeFromSleep = 6;
  AVAudioSessionRouteChangeReasonNoSuitableRouteForCategory = 7;
  AVAudioSessionRouteChangeReasonRouteConfigurationChange = 8;
  AVAudioEnvironmentDistanceAttenuationModelExponential = 1;
  AVAudioEnvironmentDistanceAttenuationModelInverse = 2;
  AVAudioEnvironmentDistanceAttenuationModelLinear = 3;
  AVAudioEnvironmentOutputTypeAuto = 0;
  AVAudioEnvironmentOutputTypeHeadphones = 1;
  AVAudioEnvironmentOutputTypeBuiltInSpeakers = 2;
  AVAudioEnvironmentOutputTypeExternalSpeakers = 3;
  AVAudioUnitEQFilterTypeParametric = 0;
  AVAudioUnitEQFilterTypeLowPass = 1;
  AVAudioUnitEQFilterTypeHighPass = 2;
  AVAudioUnitEQFilterTypeResonantLowPass = 3;
  AVAudioUnitEQFilterTypeResonantHighPass = 4;
  AVAudioUnitEQFilterTypeBandPass = 5;
  AVAudioUnitEQFilterTypeBandStop = 6;
  AVAudioUnitEQFilterTypeLowShelf = 7;
  AVAudioUnitEQFilterTypeHighShelf = 8;
  AVAudioUnitEQFilterTypeResonantLowShelf = 9;
  AVAudioUnitEQFilterTypeResonantHighShelf = 10;
  AVAudioUnitReverbPresetSmallRoom = 0;
  AVAudioUnitReverbPresetMediumRoom = 1;
  AVAudioUnitReverbPresetLargeRoom = 2;
  AVAudioUnitReverbPresetMediumHall = 3;
  AVAudioUnitReverbPresetLargeHall = 4;
  AVAudioUnitReverbPresetPlate = 5;
  AVAudioUnitReverbPresetMediumChamber = 6;
  AVAudioUnitReverbPresetLargeChamber = 7;
  AVAudioUnitReverbPresetCathedral = 8;
  AVAudioUnitReverbPresetLargeRoom2 = 9;
  AVAudioUnitReverbPresetMediumHall2 = 10;
  AVAudioUnitReverbPresetMediumHall3 = 11;
  AVAudioUnitReverbPresetLargeHall2 = 12;
  AVPlayerHDRModeHLG = 1;
  AVPlayerHDRModeHDR10 = 2;
  AVPlayerHDRModeDolbyVision = 4;
  AVPlayerTimeControlStatusPaused = 0;
  AVPlayerTimeControlStatusWaitingToPlayAtSpecifiedRate = 1;
  AVPlayerTimeControlStatusPlaying = 2;

type
  AVAudioBuffer = interface;
  AVAudioChannelLayout = interface;
  AVAudioConnectionPoint = interface;
  AVAudioEngine = interface;
  AVAudioFormat = interface;
  AVAudioInputNode = interface;
  AVAudioIONode = interface;
  AVAudioMixerNode = interface;
  AVAudioNode = interface;
  AVAudioOutputNode = interface;
  AVAudioPCMBuffer = interface;
  AVAudioSession = interface;
  AVAudioSessionDataSourceDescription = interface;
  AVAudioSessionPortDescription = interface;
  AVAudioSessionRouteDescription = interface;
  AVAudioTime = interface;
  AVAudioEnvironmentNode = interface;
  AVAudioEnvironmentDistanceAttenuationParameters = interface;
  AVAudioUnitEQFilterParameters = interface;
  AVAudioEnvironmentReverbParameters = interface;

  PInt32 = ^Int32;
  AVAuthorizationStatus = NSInteger;
  AVAudioNodeBus = NSUInteger;
  AVAudioCommonFormat = NSUInteger;
  AVAudioSessionSetActiveOptions = NSUInteger;
  AVAudioSessionCategoryOptions = NSUInteger;
  AVAudioSessionRecordPermission = NSUInteger;
  AVAudioSessionPortOverride = NSUInteger;
  AVAudioSessionIOType = NSUInteger;
  AVAudioChannelCount = LongWord;
  AVAudioFrameCount = LongWord;
  AVAudioFramePosition = Int64;
  AVAudioSessionRouteChangeReason = NSInteger;
  CMFormatDescriptionRef = Pointer;
  CMAudioFormatDescriptionRef = CMFormatDescriptionRef;
  AVAudioEnvironmentDistanceAttenuationModel = NSInteger;
  AVAudioEnvironmentOutputType = NSInteger;
  AVAudioUnitEQFilterType = NSInteger;
  AVAudioUnitReverbPreset = NSInteger;
  AVPlayerHDRMode = NSInteger;
  AVLayerVideoGravity = NSString;
  AVMediaCharacteristic = NSString;
  AVPlayerWaitingReason = NSString;
  AVPlayerTimeControlStatus = NSInteger;
  AVDepthDataAccuracy = NSInteger;
  AVDepthDataQuality = NSInteger;

  CGImagePropertyOrientation = NSUInteger;

  AVAudio3DAngularOrientation = record
    yaw: Single;
    pitch: Single;
    roll: Single;
  end;

  AVAudio3DPoint = record
    x: Single;
    y: Single;
    z: Single;
  end;

  AVAudio3DVector = AVAudio3DPoint;

  AVAudio3DVectorOrientation = record
    forward: AVAudio3DVector;
    up: AVAudio3DVector;
  end;

  AVAudioNodeTapBlock = procedure(buffer: AVAudioPCMBuffer; when: AVAudioTime) of object;
  PermissionBlock = procedure(granted: Boolean) of object;

  TAVPlayerBlockMethod1 = procedure(finished: Boolean) of object;
  TAVPlayerBlockMethod2 = procedure(time: CMTime) of object;
  TAVPlayerBlockMethod3 = procedure of object;

  AVAudioSessionRouteDescriptionClass = interface(NSObjectClass)
    ['{A7524349-5447-431C-A1A9-D09F2C1F6588}']
  end;

  AVAudioSessionRouteDescription = interface(NSObject)
    ['{44C1B098-0235-42D7-98B2-36355BC78080}']
    function inputs: NSArray; cdecl;
    function outputs: NSArray; cdecl;
  end;
  TAVAudioSessionRouteDescription = class(TOCGenericImport<AVAudioSessionRouteDescriptionClass, AVAudioSessionRouteDescription>) end;

  AVAudioChannelLayoutClass = interface(NSObjectClass)
    ['{A4F56561-9CC2-46F9-B273-949A566D2FD7}']
    { class } function layoutWithLayoutTag(layoutTag: Pointer): Pointer; cdecl;
    { class } function layoutWithLayout(layout: Pointer): Pointer; cdecl;
  end;

  AVAudioChannelLayout = interface(NSObject)
    ['{67D22534-0D14-4BCE-AD5B-4383B19D6906}']
    function initWithLayoutTag(layoutTag: Pointer): Pointer; cdecl;
    function initWithLayout(layout: Pointer): Pointer; cdecl;
    function isEqual(&object: Pointer): Boolean; cdecl;
  end;
  TAVAudioChannelLayout = class(TOCGenericImport<AVAudioChannelLayoutClass, AVAudioChannelLayout>)
  end;

  AVAudioFormatClass = interface(NSObjectClass)
    ['{A62C7806-23D2-4FAA-95B0-B69F6507B249}']
  end;

  AVAudioFormat = interface(NSObject)
    ['{E00DB4D9-D62C-4FE3-B461-9F949C440EDE}']
    [MethodName('initStandardFormatWithSampleRate:channels:')]
    function initStandardFormatWithSampleRateChannels(sampleRate: Double; channels: AVAudioChannelCount): Pointer; cdecl;
    [MethodName('initStandardFormatWithSampleRate:channelLayout:')]
    function initStandardFormatWithSampleRateChannelLayout(sampleRate: Double; channelLayout: AVAudioChannelLayout): Pointer; cdecl;
    [MethodName('initWithStreamDescription:')]
    function initWithStreamDescription(asbd: Pointer): Pointer; cdecl;
    [MethodName('initWithStreamDescription:channelLayout:')]
    function initWithStreamDescriptionChannelLayout(asbd: Pointer; channelLayout: AVAudioChannelLayout): Pointer; cdecl;
    [MethodName('initWithCommonFormat:sampleRate:channels:interleaved:')]
    function initWithCommonFormatSampleRateChannelsInterleaved(format: AVAudioCommonFormat; sampleRate: Double; channels: AVAudioChannelCount;
      interleaved: Boolean): Pointer; cdecl;
    [MethodName('initWithCommonFormat:sampleRate:interleaved:channelLayout:')]
    function initWithCommonFormatSampleRateInterleavedChannelLayout(format: AVAudioCommonFormat; sampleRate: Double; interleaved: Boolean;
      channelLayout: AVAudioChannelLayout): Pointer; cdecl;
    function initWithSettings(settings: NSDictionary): Pointer; cdecl;
    function initWithCMAudioFormatDescription(formatDescription: CMAudioFormatDescriptionRef): Pointer; cdecl;
    function isEqual(&object: Pointer): Boolean; cdecl;
  end;
  TAVAudioFormat = class(TOCGenericImport<AVAudioFormatClass, AVAudioFormat>)
  end;

  AVAudioBufferClass = interface(NSObjectClass)
    ['{5E65EB49-D7C4-446D-A9DE-C58B38CE9FF0}']
  end;

  AVAudioBuffer = interface(NSObject)
    ['{0ADF04AA-820E-484D-980B-AE9908C6645B}']
    function format: AVAudioFormat; cdecl;
    function audioBufferList: PInteger; cdecl;
    function mutableAudioBufferList: PInteger; cdecl;
  end;
  TAVAudioBuffer = class(TOCGenericImport<AVAudioBufferClass, AVAudioBuffer>)
  end;

  AVAudioPCMBufferClass = interface(AVAudioBufferClass)
    ['{5E0E7B5A-0FED-4171-97F1-6EEB756883D5}']
  end;

  AVAudioPCMBuffer = interface(AVAudioBuffer)
    ['{44654F6B-2828-4270-B7C8-455F4E2655AF}']
    function initWithPCMFormat(format: AVAudioFormat; frameCapacity: AVAudioFrameCount): Pointer; cdecl;
    function frameCapacity: AVAudioFrameCount; cdecl;
    procedure setFrameLength(frameLength: AVAudioFrameCount); cdecl;
    function frameLength: AVAudioFrameCount; cdecl;
    function stride: NSUInteger; cdecl;
    function floatChannelData: PSingle; cdecl;
    function int16ChannelData: PSmallInt; cdecl;
    function int32ChannelData: PInt32; cdecl;
  end;
  TAVAudioPCMBuffer = class(TOCGenericImport<AVAudioPCMBufferClass, AVAudioPCMBuffer>)
  end;

  AVAudioTimeClass = interface(NSObjectClass)
    ['{D4E13B15-54DE-43E3-A1A1-9C7B2D71C5A6}']
    { class } function timeWithAudioTimeStamp(ts: Pointer; sampleRate: Double): Pointer; cdecl;
    [MethodName('timeWithHostTime:')]
    { class } function timeWithHostTime(hostTime: UInt64): Pointer; cdecl;
    { class } function timeWithSampleTime(sampleTime: AVAudioFramePosition; atRate: Double): Pointer; cdecl;
    [MethodName('timeWithHostTime:sampleTime:atRate:')]
    { class } function timeWithHostTimeSampleTimeAtRate(hostTime: UInt64; sampleTime: AVAudioFramePosition;
      atRate: Double): Pointer; cdecl;
    { class } function hostTimeForSeconds(seconds: NSTimeInterval): UInt64; cdecl;
    { class } function secondsForHostTime(hostTime: UInt64): NSTimeInterval; cdecl;
  end;

  AVAudioTime = interface(NSObject)
    ['{CC5015D1-FFB9-4E48-BDCF-B4F6DC827712}']
    function initWithAudioTimeStamp(ts: Pointer; sampleRate: Double): Pointer; cdecl;
    [MethodName('initWithHostTime:')]
    function initWithHostTime(hostTime: UInt64): Pointer; cdecl;
    function initWithSampleTime(sampleTime: AVAudioFramePosition; atRate: Double): Pointer; cdecl;
    [MethodName('initWithHostTime:sampleTime:atRate:')]
    function initWithHostTimeSampleTimeAtRate(hostTime: UInt64; sampleTime: AVAudioFramePosition; atRate: Double): Pointer; cdecl;
    function extrapolateTimeFromAnchor(anchorTime: AVAudioTime): AVAudioTime; cdecl;
  end;
  TAVAudioTime = class(TOCGenericImport<AVAudioTimeClass, AVAudioTime>)
  end;

  AVAudioNodeClass = interface(NSObjectClass)
    ['{3F72F427-4B5E-4124-BC37-5230C23DEDAC}']
  end;

  AVAudioNode = interface(NSObject)
    ['{3837A137-8FC4-4DBD-B610-9A5718AA282C}']
    procedure reset; cdecl;
    function inputFormatForBus(bus: AVAudioNodeBus): AVAudioFormat; cdecl;
    function outputFormatForBus(bus: AVAudioNodeBus): AVAudioFormat; cdecl;
    function nameForInputBus(bus: AVAudioNodeBus): NSString; cdecl;
    function nameForOutputBus(bus: AVAudioNodeBus): NSString; cdecl;
    procedure installTapOnBus(bus: AVAudioNodeBus; bufferSize: AVAudioFrameCount; format: AVAudioFormat; block: AVAudioNodeTapBlock); cdecl;
    procedure removeTapOnBus(bus: AVAudioNodeBus); cdecl;
    function engine: AVAudioEngine; cdecl;
    function numberOfInputs: NSUInteger; cdecl;
    function numberOfOutputs: NSUInteger; cdecl;
    function lastRenderTime: AVAudioTime; cdecl;
  end;
  TAVAudioNode = class(TOCGenericImport<AVAudioNodeClass, AVAudioNode>)
  end;

  AVAudioConnectionPointClass = interface(NSObjectClass)
    ['{66C0AF52-B44F-44E8-802E-A57B7A15E03B}']
  end;

  AVAudioConnectionPoint = interface(NSObject)
    ['{A49EFD83-E6E5-4302-8AAB-D3A42B48B793}']
    function initWithNode(node: AVAudioNode; bus: AVAudioNodeBus): Pointer; cdecl;
    function node: AVAudioNode; cdecl;
    function bus: AVAudioNodeBus; cdecl;
  end;
  TAVAudioConnectionPoint = class(TOCGenericImport<AVAudioConnectionPointClass, AVAudioConnectionPoint>)
  end;

  AVAudioIONodeClass = interface(AVAudioNodeClass)
    ['{4157938D-B648-4071-B7FE-3DBF33A6F264}']
  end;

  AVAudioIONode = interface(AVAudioNode)
    ['{32D9F0C8-BE08-4148-8642-F93036994696}']
    function presentationLatency: NSTimeInterval; cdecl;
  end;
  TAVAudioIONode = class(TOCGenericImport<AVAudioIONodeClass, AVAudioIONode>)
  end;

  AVAudioInputNodeClass = interface(AVAudioIONodeClass)
    ['{2809A0C7-7D59-4243-B519-B73BD18CCA4A}']
  end;

  AVAudioInputNode = interface(AVAudioIONode)
    ['{F06F0968-29FB-42B0-B501-689F11F1936B}']
  end;
  TAVAudioInputNode = class(TOCGenericImport<AVAudioInputNodeClass, AVAudioInputNode>)
  end;

  AVAudioOutputNodeClass = interface(AVAudioIONodeClass)
    ['{8AABDEF0-BE0F-48B5-8684-16432300ADD1}']
  end;

  AVAudioOutputNode = interface(AVAudioIONode)
    ['{24C72C5E-11F1-462C-B538-FDFE912BF0AD}']
  end;
  TAVAudioOutputNode = class(TOCGenericImport<AVAudioOutputNodeClass, AVAudioOutputNode>)
  end;

  AVAudioMixerNodeClass = interface(AVAudioNodeClass)
    ['{BBA5EAB3-F79E-4BF4-A986-8A3288276D3F}']
  end;

  AVAudioMixerNode = interface(AVAudioNode)
    ['{37B7FDCC-8C21-4578-A8E3-96C4F78112CC}']
    function init: Pointer; cdecl;
    procedure setOutputVolume(outputVolume: Single); cdecl;
    function outputVolume: Single; cdecl;
    function nextAvailableInputBus: AVAudioNodeBus; cdecl;
  end;
  TAVAudioMixerNode = class(TOCGenericImport<AVAudioMixerNodeClass, AVAudioMixerNode>)
  end;

  AVAudioEngineClass = interface(NSObjectClass)
    ['{DC42DAC2-C474-4747-8E16-7B4B30784EEC}']
  end;

  AVAudioEngine = interface(NSObject)
    ['{E8474587-E473-44F1-9BB3-0D035238EBA3}']
    function init: Pointer; cdecl;
    procedure attachNode(node: AVAudioNode); cdecl;
    procedure detachNode(node: AVAudioNode); cdecl;
    [MethodName('connect:to:fromBus:toBus:format:')]
    procedure connectToFromBusToBusFormat(node1: AVAudioNode; &to: AVAudioNode; fromBus: AVAudioNodeBus; toBus: AVAudioNodeBus; format: AVAudioFormat);
      cdecl;
    [MethodName('connect:to:format:')]
    procedure connectToFormat(node1: AVAudioNode; &to: AVAudioNode; format: AVAudioFormat); cdecl;
    [MethodName('connect:toConnectionPoints:fromBus:format:')]
    procedure connectToConnectionPointsFromBusFormat(sourceNode: AVAudioNode; toConnectionPoints: NSArray; fromBus: AVAudioNodeBus; format:
      AVAudioFormat); cdecl;
    [MethodName('disconnectNodeInput:bus:')]
    procedure disconnectNodeInputBus(node: AVAudioNode; bus: AVAudioNodeBus); cdecl;
    [MethodName('disconnectNodeInput:')]
    procedure disconnectNodeInput(node: AVAudioNode); cdecl;
    [MethodName('disconnectNodeOutput:bus:')]
    procedure disconnectNodeOutputBus(node: AVAudioNode; bus: AVAudioNodeBus); cdecl;
    [MethodName('disconnectNodeOutput:')]
    procedure disconnectNodeOutput(node: AVAudioNode); cdecl;
    procedure prepare; cdecl;
    function startAndReturnError(outError: PPointer): Boolean; cdecl;
    procedure pause; cdecl;
    procedure reset; cdecl;
    procedure stop; cdecl;
    function inputConnectionPointForNode(node: AVAudioNode; inputBus: AVAudioNodeBus): AVAudioConnectionPoint; cdecl;
    function outputConnectionPointsForNode(node: AVAudioNode; outputBus: AVAudioNodeBus): NSArray; cdecl;
    function outputNode: AVAudioOutputNode; cdecl;
    function inputNode: AVAudioInputNode; cdecl;
    function mainMixerNode: AVAudioMixerNode; cdecl;
    function isRunning: Boolean; cdecl;
  end;
  TAVAudioEngine = class(TOCGenericImport<AVAudioEngineClass, AVAudioEngine>)
  end;

  AVAudioSessionDataSourceDescriptionClass = interface(NSObjectClass)
    ['{9B3B13AA-74E6-4623-8CE5-68B77CDCC47F}']
  end;

  AVAudioSessionDataSourceDescription = interface(NSObject)
    ['{B7C38FE6-CE63-4FFB-BAC5-AD798ACE23C1}']
    function dataSourceID: NSNumber; cdecl;
    function dataSourceName: NSString; cdecl;
    function location: NSString; cdecl;
    function orientation: NSString; cdecl;
    function supportedPolarPatterns: NSArray; cdecl;
    function selectedPolarPattern: NSString; cdecl;
    function preferredPolarPattern: NSString; cdecl;
    function setPreferredPolarPattern(pattern: NSString; error: NSError): Boolean; cdecl;
  end;
  TAVAudioSessionDataSourceDescription = class(TOCGenericImport<AVAudioSessionDataSourceDescriptionClass, AVAudioSessionDataSourceDescription>)
  end;

  AVAudioSessionPortDescriptionClass = interface(NSObjectClass)
    ['{27B5BAB7-97FA-468D-B8D6-98A3547E0E92}']
  end;

  AVAudioSessionPortDescription = interface(NSObject)
    ['{476A3389-D468-49B8-A71C-176F85A2A8F1}']
    function portType: NSString; cdecl;
    function portName: NSString; cdecl;
    function UID: NSString; cdecl;
    function hasHardwareVoiceCallProcessing: Boolean; cdecl;
    function channels: NSArray; cdecl;
    function dataSources: NSArray; cdecl;
    function selectedDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function preferredDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function setPreferredDataSource(dataSource: AVAudioSessionDataSourceDescription; error: NSError): Boolean; cdecl;
  end;
  TAVAudioSessionPortDescription = class(TOCGenericImport<AVAudioSessionPortDescriptionClass, AVAudioSessionPortDescription>)
  end;

  AVAudioSessionClass = interface(NSObjectClass)
    ['{F610D17D-E53E-4D85-976B-21D68784880A}']
    { class } function sharedInstance: Pointer; cdecl;
  end;

  AVAudioSession = interface(NSObject)
    ['{4C5310C3-D516-4457-BBF9-4087E58B3C7F}']
    [MethodName('setActive:error:')]
    function setActiveError(active: Boolean; error: NSError): Boolean; cdecl;
    [MethodName('setActive:withOptions:error:')]
    function setActiveWithOptionsError(active: Boolean; withOptions: AVAudioSessionSetActiveOptions; error: NSError): Boolean; cdecl;
    function availableCategories: NSArray; cdecl;
    [MethodName('setCategory:error:')]
    function setCategoryError(category: NSString; error: PPointer): Boolean; cdecl;
    [MethodName('setCategory:withOptions:error:')]
    function setCategoryWithOptionsError(category: NSString; withOptions: AVAudioSessionCategoryOptions; error: PPointer): Boolean; cdecl;
    [MethodName('setCategory:mode:options:error:')]
    function setCategoryModeOptionsError(category: NSString; mode: NSString; options: AVAudioSessionCategoryOptions; error: PPointer): Boolean; cdecl;
    function category: NSString; cdecl;
    function recordPermission: AVAudioSessionRecordPermission; cdecl;
    procedure requestRecordPermission(response: PermissionBlock); cdecl;
    function categoryOptions: AVAudioSessionCategoryOptions; cdecl;
    function availableModes: NSArray; cdecl;
    function setMode(mode: NSString; error: NSError): Boolean; cdecl;
    function mode: NSString; cdecl;
    function overrideOutputAudioPort(portOverride: AVAudioSessionPortOverride; error: NSError): Boolean; cdecl;
    function isOtherAudioPlaying: Boolean; cdecl;
    function secondaryAudioShouldBeSilencedHint: Boolean; cdecl;
    function currentRoute: AVAudioSessionRouteDescription; cdecl;
    function setPreferredInput(inPort: AVAudioSessionPortDescription; error: NSError): Boolean; cdecl;
    function preferredInput: AVAudioSessionPortDescription; cdecl;
    function availableInputs: NSArray; cdecl;
    function setPreferredSampleRate(sampleRate: Double; error: NSError): Boolean; cdecl;
    function preferredSampleRate: Double; cdecl;
    function setPreferredIOBufferDuration(duration: NSTimeInterval; error: NSError): Boolean; cdecl;
    function preferredIOBufferDuration: NSTimeInterval; cdecl;
    function setPreferredInputNumberOfChannels(count: NSInteger; error: NSError): Boolean; cdecl;
    function preferredInputNumberOfChannels: NSInteger; cdecl;
    function setPreferredOutputNumberOfChannels(count: NSInteger; error: NSError): Boolean; cdecl;
    function preferredOutputNumberOfChannels: NSInteger; cdecl;
    function maximumInputNumberOfChannels: NSInteger; cdecl;
    function maximumOutputNumberOfChannels: NSInteger; cdecl;
    function setInputGain(gain: Single; error: NSError): Boolean; cdecl;
    function inputGain: Single; cdecl;
    function isInputGainSettable: Boolean; cdecl;
    function isInputAvailable: Boolean; cdecl;
    function inputDataSources: NSArray; cdecl;
    function inputDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function setInputDataSource(dataSource: AVAudioSessionDataSourceDescription; error: NSError): Boolean; cdecl;
    function outputDataSources: NSArray; cdecl;
    function outputDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function setOutputDataSource(dataSource: AVAudioSessionDataSourceDescription; error: NSError): Boolean; cdecl;
    function sampleRate: Double; cdecl;
    function inputNumberOfChannels: NSInteger; cdecl;
    function outputNumberOfChannels: NSInteger; cdecl;
    function outputVolume: Single; cdecl;
    function inputLatency: NSTimeInterval; cdecl;
    function outputLatency: NSTimeInterval; cdecl;
    function IOBufferDuration: NSTimeInterval; cdecl;
    function setAggregatedIOPreference(inIOType: AVAudioSessionIOType; error: NSError): Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;
    function init: Pointer; cdecl;
    [MethodName('setActive:withFlags:error:')]
    function setActiveWithFlagsError(active: Boolean; withFlags: NSInteger; error: NSError): Boolean; cdecl;
    function inputIsAvailable: Boolean; cdecl;
    function currentHardwareSampleRate: Double; cdecl;
    function currentHardwareInputNumberOfChannels: NSInteger; cdecl;
    function currentHardwareOutputNumberOfChannels: NSInteger; cdecl;
    function setPreferredHardwareSampleRate(sampleRate: Double; error: NSError): Boolean; cdecl;
    function preferredHardwareSampleRate: Double; cdecl;
  end;
  TAVAudioSession = class(TOCGenericImport<AVAudioSessionClass, AVAudioSession>)
  end;

  TRequestAccessForMediaTypeCompletionHandler = procedure(granted: Boolean) of object;

  AVCaptureDeviceClass = interface(iOSapi.AVFoundation.AVCaptureDeviceClass)
    ['{9B4047B4-2565-46E8-A4F3-325300B1968D}']
    { class } function authorizationStatusForMediaType(mediaType: NSString): AVAuthorizationStatus; cdecl;
    { class } procedure requestAccessForMediaType(mediaType: NSString; completionHandler: TRequestAccessForMediaTypeCompletionHandler); cdecl;
  end;
  TAVCaptureDevice = class(TOCGenericImport<AVCaptureDeviceClass, AVCaptureDevice>) end;

  AVAudioEnvironmentDistanceAttenuationParametersClass = interface(NSObjectClass)
    ['{D889B82A-CF52-4EB4-8A56-23BCB07E61BF}']
  end;

  AVAudioEnvironmentDistanceAttenuationParameters = interface(NSObject)
    ['{B2F3F993-FA7B-4360-A2B0-6AAFEEA4D021}']
    function distanceAttenuationModel: AVAudioEnvironmentDistanceAttenuationModel; cdecl;
    function maximumDistance: Single; cdecl;
    function referenceDistance: Single; cdecl;
    function rolloffFactor: Single; cdecl;
    procedure setDistanceAttenuationModel(distanceAttenuationModel: AVAudioEnvironmentDistanceAttenuationModel); cdecl;
    procedure setMaximumDistance(maximumDistance: Single); cdecl;
    procedure setReferenceDistance(referenceDistance: Single); cdecl;
    procedure setRolloffFactor(rolloffFactor: Single); cdecl;
  end;
  TAVAudioEnvironmentDistanceAttenuationParameters = class(TOCGenericImport<AVAudioEnvironmentDistanceAttenuationParametersClass,
    AVAudioEnvironmentDistanceAttenuationParameters>) end;

  AVAudioUnitEQFilterParametersClass = interface(NSObjectClass)
    ['{AC93CB36-1660-4A47-8EBE-327F2A0E443A}']
  end;

  AVAudioUnitEQFilterParameters = interface(NSObject)
    ['{78DBAF70-C134-40E3-8BC4-27462A36264E}']
    function bandwidth: Single; cdecl;
    function bypass: Boolean; cdecl;
    function filterType: AVAudioUnitEQFilterType; cdecl;
    function frequency: Single; cdecl;
    function gain: Single; cdecl;
    procedure setBandwidth(bandwidth: Single); cdecl;
    procedure setBypass(bypass: Boolean); cdecl;
    procedure setFilterType(filterType: AVAudioUnitEQFilterType); cdecl;
    procedure setFrequency(frequency: Single); cdecl;
    procedure setGain(gain: Single); cdecl;
  end;
  TAVAudioUnitEQFilterParameters = class(TOCGenericImport<AVAudioUnitEQFilterParametersClass, AVAudioUnitEQFilterParameters>) end;

  AVAudioEnvironmentReverbParametersClass = interface(NSObjectClass)
    ['{3441D228-B4B1-4CEE-BC66-FCC20213A63A}']
  end;

  AVAudioEnvironmentReverbParameters = interface(NSObject)
    ['{84781FC7-2997-4D8D-AC81-937E36070959}']
    function enable: Boolean; cdecl;
    function filterParameters: AVAudioUnitEQFilterParameters; cdecl;
    function level: Single; cdecl;
    procedure loadFactoryReverbPreset(preset: AVAudioUnitReverbPreset); cdecl;
    procedure setEnable(enable: Boolean); cdecl;
    procedure setLevel(level: Single); cdecl;
  end;
  TAVAudioEnvironmentReverbParameters = class(TOCGenericImport<AVAudioEnvironmentReverbParametersClass, AVAudioEnvironmentReverbParameters>) end;

  AVAudioEnvironmentNodeClass = interface(AVAudioNodeClass)
    ['{DB31D7ED-0CD4-44DB-B378-AD391410A687}']
  end;

  AVAudioEnvironmentNode = interface(AVAudioNode)
    ['{F69CD57E-A3CD-44D4-A51B-E0048575A350}']
    function applicableRenderingAlgorithms: NSArray; cdecl;
    function distanceAttenuationParameters: AVAudioEnvironmentDistanceAttenuationParameters; cdecl;
    function listenerAngularOrientation: AVAudio3DAngularOrientation; cdecl;
    function listenerPosition: AVAudio3DPoint; cdecl;
    function listenerVectorOrientation: AVAudio3DVectorOrientation; cdecl;
    function nextAvailableInputBus: AVAudioNodeBus; cdecl;
    function outputType: AVAudioEnvironmentOutputType; cdecl;
    function outputVolume: Single; cdecl;
    function reverbParameters: AVAudioEnvironmentReverbParameters; cdecl;
    procedure setListenerAngularOrientation(listenerAngularOrientation: AVAudio3DAngularOrientation); cdecl;
    procedure setListenerPosition(listenerPosition: AVAudio3DPoint); cdecl;
    procedure setListenerVectorOrientation(listenerVectorOrientation: AVAudio3DVectorOrientation); cdecl;
    procedure setOutputType(outputType: AVAudioEnvironmentOutputType); cdecl;
    procedure setOutputVolume(outputVolume: Single); cdecl;
  end;
  TAVAudioEnvironmentNode = class(TOCGenericImport<AVAudioEnvironmentNodeClass, AVAudioEnvironmentNode>) end;

  AVCameraCalibrationDataClass = interface(NSObjectClass)
    ['{03A8465E-E5BC-4333-B290-0D8450CA6AE1}']
    {class} function new: Pointer; cdecl;
  end;

  AVCameraCalibrationData = interface(NSObject)
    ['{376535E1-EFF2-4542-81B1-B38AB46FEC94}']
    function extrinsicMatrix: matrix_float4x3; cdecl;
    function intrinsicMatrix: matrix_float3x3; cdecl;
    function intrinsicMatrixReferenceDimensions: CGSize; cdecl;
    function inverseLensDistortionLookupTable: NSData; cdecl;
    function lensDistortionCenter: CGPoint; cdecl;
    function lensDistortionLookupTable: NSData; cdecl;
    function pixelSize: Single; cdecl;
  end;
  TAVCameraCalibrationData = class(TOCGenericImport<AVCameraCalibrationDataClass, AVCameraCalibrationData>) end;

  AVDepthDataClass = interface(NSObjectClass)
    ['{532EB378-577E-425C-95A9-22507AC64804}']
    [MethodName('depthDataFromDictionaryRepresentation:error:')]
    {class} function depthDataFromDictionaryRepresentation(imageSourceAuxDataInfoDictionary: NSDictionary; outError: PNSError): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  AVDepthData = interface(NSObject)
    ['{3049DEC1-E604-4510-B028-51407F54AC61}']
    function availableDepthDataTypes: NSArray; cdecl;
    function cameraCalibrationData: AVCameraCalibrationData; cdecl;
    function depthDataAccuracy: AVDepthDataAccuracy; cdecl;
    function depthDataByApplyingExifOrientation(exifOrientation: CGImagePropertyOrientation): Pointer; cdecl;
    function depthDataByConvertingToDepthDataType(depthDataType: OSType): Pointer; cdecl;
    [MethodName('depthDataByReplacingDepthDataMapWithPixelBuffer:error:')]
    function depthDataByReplacingDepthDataMapWithPixelBuffer(pixelBuffer: CVPixelBufferRef; outError: PNSError): Pointer; cdecl;
    function depthDataMap: CVPixelBufferRef; cdecl;
    function depthDataQuality: AVDepthDataQuality; cdecl;
    function depthDataType: OSType; cdecl;
    function dictionaryRepresentationForAuxiliaryDataType(outAuxDataType: PNSString): NSDictionary; cdecl;
    function isDepthDataFiltered: Boolean; cdecl;
  end;
  TAVDepthData = class(TOCGenericImport<AVDepthDataClass, AVDepthData>) end;

  AVPlayerClass = interface(NSObjectClass)
    ['{8B1775DB-D230-4202-9D43-8ED8BACF1D48}']
    {class} function availableHDRModes: AVPlayerHDRMode; cdecl;
    {class} function eligibleForHDRPlayback: Boolean; cdecl;
    {class} function playerWithPlayerItem(item: AVPlayerItem): Pointer; cdecl;
    {class} function playerWithURL(URL: NSURL): Pointer; cdecl;
  end;

  AVPlayer = interface(NSObject)
    ['{ECF23C67-9F21-4166-8328-66F0B5413F74}']
    function actionAtItemEnd: AVPlayerActionAtItemEnd; cdecl;
    [MethodName('addBoundaryTimeObserverForTimes:queue:usingBlock:')]
    function addBoundaryTimeObserverForTimes(times: NSArray; queue: dispatch_queue_t; block: TAVPlayerBlockMethod3): Pointer; cdecl;
    [MethodName('addPeriodicTimeObserverForInterval:queue:usingBlock:')]
    function addPeriodicTimeObserverForInterval(interval: CMTime; queue: dispatch_queue_t; block: TAVPlayerBlockMethod2): Pointer; cdecl;
    function allowsAirPlayVideo: Boolean; cdecl; // API_DEPRECATED("No longer supported", ios(5.0, 6.0), tvos(9.0, 9.0))
    function allowsExternalPlayback: Boolean; cdecl;
    function appliesMediaSelectionCriteriaAutomatically: Boolean; cdecl;
    function audioOutputDeviceUniqueID: NSString; cdecl;
    function automaticallyWaitsToMinimizeStalling: Boolean; cdecl;
    procedure cancelPendingPrerolls; cdecl;
    function currentItem: AVPlayerItem; cdecl;
    function currentTime: CMTime; cdecl;
    function error: NSError; cdecl;
    function externalPlaybackVideoGravity: AVLayerVideoGravity; cdecl;
    function initWithPlayerItem(item: AVPlayerItem): Pointer; cdecl;
    function initWithURL(URL: NSURL): Pointer; cdecl;
    function isAirPlayVideoActive: Boolean; cdecl; // API_DEPRECATED("No longer supported", ios(5.0, 6.0), tvos(9.0, 9.0))
    function isClosedCaptionDisplayEnabled: Boolean; cdecl; // API_DEPRECATED("Allow AVPlayer to enable closed captions automatically according to user preferences by ensuring that the value of appliesMediaSelectionCriteriaAutomatically is YES.", macos(10.7, 10.13), ios(4.0, 11.0), tvos(9.0, 11.0))
    function isExternalPlaybackActive: Boolean; cdecl;
    function isMuted: Boolean; cdecl;
    function masterClock: CMClockRef; cdecl;
    function mediaSelectionCriteriaForMediaCharacteristic(mediaCharacteristic: AVMediaCharacteristic): AVPlayerMediaSelectionCriteria; cdecl;
    function outputObscuredDueToInsufficientExternalProtection: Boolean; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure playImmediatelyAtRate(rate: Single); cdecl;
    function preferredVideoDecoderGPURegistryID: UInt64; cdecl;
    [MethodName('prerollAtRate:completionHandler:')]
    procedure prerollAtRate(rate: Single; completionHandler: TAVPlayerBlockMethod1); cdecl;
    function preventsDisplaySleepDuringVideoPlayback: Boolean; cdecl;
    function rate: Single; cdecl;
    function reasonForWaitingToPlay: AVPlayerWaitingReason; cdecl;
    procedure removeTimeObserver(observer: Pointer); cdecl;
    procedure replaceCurrentItemWithPlayerItem(item: AVPlayerItem); cdecl;
    [MethodName('seekToDate:completionHandler:')]
    procedure seekToDate(date: NSDate; completionHandler: TAVPlayerBlockMethod1); overload; cdecl;
    procedure seekToDate(date: NSDate); overload; cdecl;
    [MethodName('seekToTime:completionHandler:')]
    procedure seekToTime(time: CMTime; completionHandler: TAVPlayerBlockMethod1); overload; cdecl;
    [MethodName('seekToTime:toleranceBefore:toleranceAfter:completionHandler:')]
    procedure seekToTime(time: CMTime; toleranceBefore: CMTime; toleranceAfter: CMTime; completionHandler: TAVPlayerBlockMethod1); overload; cdecl;
    [MethodName('seekToTime:toleranceBefore:toleranceAfter:')]
    procedure seekToTime(time: CMTime; toleranceBefore: CMTime; toleranceAfter: CMTime); overload; cdecl;
    procedure seekToTime(time: CMTime); overload; cdecl;
    procedure setActionAtItemEnd(actionAtItemEnd: AVPlayerActionAtItemEnd); cdecl;
    procedure setAllowsAirPlayVideo(allowsAirPlayVideo: Boolean); cdecl; // API_DEPRECATED("No longer supported", ios(5.0, 6.0), tvos(9.0, 9.0))
    procedure setAllowsExternalPlayback(allowsExternalPlayback: Boolean); cdecl;
    procedure setAppliesMediaSelectionCriteriaAutomatically(appliesMediaSelectionCriteriaAutomatically: Boolean); cdecl;
    procedure setAudioOutputDeviceUniqueID(audioOutputDeviceUniqueID: NSString); cdecl;
    procedure setAutomaticallyWaitsToMinimizeStalling(automaticallyWaitsToMinimizeStalling: Boolean); cdecl;
    procedure setClosedCaptionDisplayEnabled(closedCaptionDisplayEnabled: Boolean); cdecl; // API_DEPRECATED("Allow AVPlayer to enable closed captions automatically according to user preferences by ensuring that the value of appliesMediaSelectionCriteriaAutomatically is YES.", macos(10.7, 10.13), ios(4.0, 11.0), tvos(9.0, 11.0))
    procedure setExternalPlaybackVideoGravity(externalPlaybackVideoGravity: AVLayerVideoGravity); cdecl;
    procedure setMasterClock(masterClock: CMClockRef); cdecl;
    [MethodName('setMediaSelectionCriteria:forMediaCharacteristic:')]
    procedure setMediaSelectionCriteria(criteria: AVPlayerMediaSelectionCriteria; mediaCharacteristic: AVMediaCharacteristic); cdecl;
    procedure setMuted(muted: Boolean); cdecl;
    procedure setPreferredVideoDecoderGPURegistryID(preferredVideoDecoderGPURegistryID: UInt64); cdecl;
    procedure setPreventsDisplaySleepDuringVideoPlayback(preventsDisplaySleepDuringVideoPlayback: Boolean); cdecl;
    procedure setRate(rate: Single); overload; cdecl;
    [MethodName('setRate:time:atHostTime:')]
    procedure setRate(rate: Single; itemTime: CMTime; hostClockTime: CMTime); overload; cdecl;
    procedure setUsesAirPlayVideoWhileAirPlayScreenIsActive(usesAirPlayVideoWhileAirPlayScreenIsActive: Boolean); cdecl; // API_DEPRECATED("No longer supported", ios(5.0, 6.0), tvos(9.0, 9.0))
    procedure setUsesExternalPlaybackWhileExternalScreenIsActive(usesExternalPlaybackWhileExternalScreenIsActive: Boolean); cdecl;
    procedure setVolume(volume: Single); cdecl;
    function status: AVPlayerStatus; cdecl;
    function timeControlStatus: AVPlayerTimeControlStatus; cdecl;
    function usesAirPlayVideoWhileAirPlayScreenIsActive: Boolean; cdecl; // API_DEPRECATED("No longer supported", ios(5.0, 6.0), tvos(9.0, 9.0))
    function usesExternalPlaybackWhileExternalScreenIsActive: Boolean; cdecl;
    function volume: Single; cdecl;
  end;
  TAVPlayer = class(TOCGenericImport<AVPlayerClass, AVPlayer>) end;

function AVAudioSessionCategoryRecord: NSString;
function AVAudioSessionModeMeasurement: NSString;

implementation

function AVAudioSessionCategoryRecord: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVAudioSessionCategoryRecord');
end;

function AVAudioSessionModeMeasurement: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVAudioSessionModeMeasurement');
end;

end.


