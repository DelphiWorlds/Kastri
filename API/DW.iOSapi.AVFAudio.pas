unit DW.iOSapi.AVFAudio;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

// *** NOTE: Some properties and methods have been commented out until imports are done for other frameworks

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreAudio, iOSapi.CoreMedia;

const
  AVAUDIOFORMAT_HAVE_CMFORMATDESCRIPTION = 1;
  AVAUDIONODE_HAVE_AUAUDIOUNIT = 1;
  AVAUDIOIONODE_HAVE_AUDIOUNIT = 1;
  AVAUDIOENGINE_HAVE_MUSICPLAYER = 1;
  AVAUDIOENGINE_HAVE_AUAUDIOUNIT = 1;
  AVAUDIOUNIT_HAVE_AUDIOUNIT = 1;
  AVAUDIOUNITCOMPONENT_HAVE_AUDIOCOMPONENT = 1;
  AVAudioOtherFormat = 0;
  AVAudioPCMFormatFloat32 = 1;
  AVAudioPCMFormatFloat64 = 2;
  AVAudioPCMFormatInt16 = 3;
  AVAudioPCMFormatInt32 = 4;
  AVAudioConverterPrimeMethod_Pre = 0;
  AVAudioConverterPrimeMethod_Normal = 1;
  AVAudioConverterPrimeMethod_None = 2;
  AVAudioConverterInputStatus_HaveData = 0;
  AVAudioConverterInputStatus_NoDataNow = 1;
  AVAudioConverterInputStatus_EndOfStream = 2;
  AVAudioConverterOutputStatus_HaveData = 0;
  AVAudioConverterOutputStatus_InputRanDry = 1;
  AVAudioConverterOutputStatus_EndOfStream = 2;
  AVAudioConverterOutputStatus_Error = 3;
  AVAudio3DMixingRenderingAlgorithmEqualPowerPanning = 0;
  AVAudio3DMixingRenderingAlgorithmSphericalHead = 1;
  AVAudio3DMixingRenderingAlgorithmHRTF = 2;
  AVAudio3DMixingRenderingAlgorithmSoundField = 3;
  AVAudio3DMixingRenderingAlgorithmStereoPassThrough = 5;
  AVAudio3DMixingRenderingAlgorithmHRTFHQ = 6;
  AVAudio3DMixingRenderingAlgorithmAuto = 7;
  AVAudio3DMixingSourceModeSpatializeIfMono = 0;
  AVAudio3DMixingSourceModeBypass = 1;
  AVAudio3DMixingSourceModePointSource = 2;
  AVAudio3DMixingSourceModeAmbienceBed = 3;
  AVAudio3DMixingPointSourceInHeadModeMono = 0;
  AVAudio3DMixingPointSourceInHeadModeBypass = 1;
  AVAudioEngineManualRenderingErrorInvalidMode = -80800;
  AVAudioEngineManualRenderingErrorInitialized = -80801;
  AVAudioEngineManualRenderingErrorNotRunning = -80802;
  AVAudioEngineManualRenderingStatusError = -1;
  AVAudioEngineManualRenderingStatusSuccess = 0;
  AVAudioEngineManualRenderingStatusInsufficientDataFromInputNode = 1;
  AVAudioEngineManualRenderingStatusCannotDoInCurrentContext = 2;
  AVAudioEngineManualRenderingModeOffline = 0;
  AVAudioEngineManualRenderingModeRealtime = 1;
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
  AVAudioEnvironmentDistanceAttenuationModelExponential = 1;
  AVAudioEnvironmentDistanceAttenuationModelInverse = 2;
  AVAudioEnvironmentDistanceAttenuationModelLinear = 3;
  AVAudioEnvironmentOutputTypeAuto = 0;
  AVAudioEnvironmentOutputTypeHeadphones = 1;
  AVAudioEnvironmentOutputTypeBuiltInSpeakers = 2;
  AVAudioEnvironmentOutputTypeExternalSpeakers = 3;
  AVAudioQualityMin = 0;
  AVAudioQualityLow = 32;
  AVAudioQualityMedium = 64;
  AVAudioQualityHigh = 96;
  AVAudioQualityMax = 127;
  AVAudioSessionActivationOptionNone = 0;
  AVAudioSessionPortOverrideNone = 0;
  AVAudioSessionPortOverrideSpeaker = 1936747378;
  AVAudioSessionRouteChangeReasonUnknown = 0;
  AVAudioSessionRouteChangeReasonNewDeviceAvailable = 1;
  AVAudioSessionRouteChangeReasonOldDeviceUnavailable = 2;
  AVAudioSessionRouteChangeReasonCategoryChange = 3;
  AVAudioSessionRouteChangeReasonOverride = 4;
  AVAudioSessionRouteChangeReasonWakeFromSleep = 6;
  AVAudioSessionRouteChangeReasonNoSuitableRouteForCategory = 7;
  AVAudioSessionRouteChangeReasonRouteConfigurationChange = 8;
  AVAudioSessionCategoryOptionMixWithOthers = 1;
  AVAudioSessionCategoryOptionDuckOthers = 2;
  AVAudioSessionCategoryOptionAllowBluetooth = 4;
  AVAudioSessionCategoryOptionDefaultToSpeaker = 8;
  AVAudioSessionCategoryOptionInterruptSpokenAudioAndMixWithOthers = 17;
  AVAudioSessionCategoryOptionAllowBluetoothA2DP = 32;
  AVAudioSessionCategoryOptionAllowAirPlay = 64;
  AVAudioSessionCategoryOptionOverrideMutedMicrophoneInterruption = 128;
  AVAudioSessionInterruptionTypeBegan = 1;
  AVAudioSessionInterruptionTypeEnded = 0;
  AVAudioSessionInterruptionOptionShouldResume = 1;
  AVAudioSessionInterruptionReasonDefault = 0;
  AVAudioSessionInterruptionReasonAppWasSuspended = 1;
  AVAudioSessionInterruptionReasonBuiltInMicMuted = 2;
  AVAudioSessionSetActiveOptionNotifyOthersOnDeactivation = 1;
  AVAudioSessionSilenceSecondaryAudioHintTypeBegin = 1;
  AVAudioSessionSilenceSecondaryAudioHintTypeEnd = 0;
  AVAudioSessionIOTypeNotSpecified = 0;
  AVAudioSessionIOTypeAggregated = 1;
  AVAudioSessionRouteSharingPolicyDefault = 0;
  AVAudioSessionRouteSharingPolicyLongFormAudio = 1;
  AVAudioSessionRouteSharingPolicyLongForm = AVAudioSessionRouteSharingPolicyLongFormAudio;
  AVAudioSessionRouteSharingPolicyIndependent = 2;
  AVAudioSessionRouteSharingPolicyLongFormVideo = 3;
  AVAudioSessionPromptStyleNone = 1852796517;
  AVAudioSessionPromptStyleShort = 1936224884;
  AVAudioSessionPromptStyleNormal = 1852992876;
  AVAudioStereoOrientationNone = 0;
  AVAudioStereoOrientationPortrait = 1;
  AVAudioStereoOrientationPortraitUpsideDown = 2;
  AVAudioStereoOrientationLandscapeRight = 3;
  AVAudioStereoOrientationLandscapeLeft = 4;
  AVAudioSessionRecordPermissionUndetermined = 1970168948;
  AVAudioSessionRecordPermissionDenied = 1684369017;
  AVAudioSessionRecordPermissionGranted = 1735552628;
  AVAudioSessionInterruptionFlags_ShouldResume = 1;
  AVAudioSessionSetActiveFlags_NotifyOthersOnDeactivation = 1;
  AVAudioPlayerNodeBufferLoops = 1;
  AVAudioPlayerNodeBufferInterrupts = 2;
  AVAudioPlayerNodeBufferInterruptsAtLoop = 4;
  AVAudioPlayerNodeCompletionDataConsumed = 0;
  AVAudioPlayerNodeCompletionDataRendered = 1;
  AVAudioPlayerNodeCompletionDataPlayedBack = 2;
  AVAudioRoutingArbitrationCategoryPlayback = 0;
  AVAudioRoutingArbitrationCategoryPlayAndRecord = 1;
  AVAudioRoutingArbitrationCategoryPlayAndRecordVoice = 2;
  AVMusicSequenceLoadSMF_PreserveTracks = 0;
  AVMusicSequenceLoadSMF_ChannelsToTracks = 1;
  AVMusicTrackLoopCountForever = -1;
  AVAudioUnitDistortionPresetDrumsBitBrush = 0;
  AVAudioUnitDistortionPresetDrumsBufferBeats = 1;
  AVAudioUnitDistortionPresetDrumsLoFi = 2;
  AVAudioUnitDistortionPresetMultiBrokenSpeaker = 3;
  AVAudioUnitDistortionPresetMultiCellphoneConcert = 4;
  AVAudioUnitDistortionPresetMultiDecimated1 = 5;
  AVAudioUnitDistortionPresetMultiDecimated2 = 6;
  AVAudioUnitDistortionPresetMultiDecimated3 = 7;
  AVAudioUnitDistortionPresetMultiDecimated4 = 8;
  AVAudioUnitDistortionPresetMultiDistortedFunk = 9;
  AVAudioUnitDistortionPresetMultiDistortedCubed = 10;
  AVAudioUnitDistortionPresetMultiDistortedSquared = 11;
  AVAudioUnitDistortionPresetMultiEcho1 = 12;
  AVAudioUnitDistortionPresetMultiEcho2 = 13;
  AVAudioUnitDistortionPresetMultiEchoTight1 = 14;
  AVAudioUnitDistortionPresetMultiEchoTight2 = 15;
  AVAudioUnitDistortionPresetMultiEverythingIsBroken = 16;
  AVAudioUnitDistortionPresetSpeechAlienChatter = 17;
  AVAudioUnitDistortionPresetSpeechCosmicInterference = 18;
  AVAudioUnitDistortionPresetSpeechGoldenPi = 19;
  AVAudioUnitDistortionPresetSpeechRadioTower = 20;
  AVAudioUnitDistortionPresetSpeechWaves = 21;
  AVSpeechBoundaryImmediate = 0;
  AVSpeechBoundaryWord = 1;
  AVSpeechSynthesisVoiceQualityDefault = 1;
  AVSpeechSynthesisVoiceQualityEnhanced = 2;
  AVSpeechSynthesisVoiceGenderUnspecified = 0;
  AVSpeechSynthesisVoiceGenderMale = 1;
  AVSpeechSynthesisVoiceGenderFemale = 2;

type
  AVAudioBuffer = interface;
  AVAudioPCMBuffer = interface;
  AVAudioCompressedBuffer = interface;
  AVAudioChannelLayout = interface;
  AVAudioConnectionPoint = interface;
  AVAudioFormat = interface;
  AVAudioConverter = interface;
  AVAudioNode = interface;
  AVAudioMixing = interface;
  AVAudioStereoMixing = interface;
  AVAudio3DMixing = interface;
  AVAudioMixingDestination = interface;
  AVAudioIONode = interface;
  AVAudioInputNode = interface;
  AVAudioOutputNode = interface;
  AVAudioTime = interface;
  AVAudioEngine = interface;
  AVAudioUnit = interface;
  AVAudioUnitEffect = interface;
  AVAudioUnitReverb = interface;
  AVAudioUnitEQFilterParameters = interface;
  AVAudioUnitEQ = interface;
  AVAudioEnvironmentDistanceAttenuationParameters = interface;
  AVAudioEnvironmentReverbParameters = interface;
  AVAudioEnvironmentNode = interface;
  AVAudioFile = interface;
  AVAudioMixerNode = interface;
  AVAudioSessionChannelDescription = interface;
  AVAudioSessionDataSourceDescription = interface;
  AVAudioSessionPortDescription = interface;
  AVAudioSessionRouteDescription = interface;
  AVAudioSession = interface;
  AVAudioSessionDelegate = interface;
  AVAudioPlayer = interface;
  AVAudioPlayerDelegate = interface;
  AVAudioPlayerNode = interface;
  AVAudioRecorder = interface;
  AVAudioRecorderDelegate = interface;
  AVAudioRoutingArbiter = interface;
  AVAudioSequencer = interface;
  AVMusicTrack = interface;
  AVAudioSinkNode = interface;
  AVAudioSourceNode = interface;
  AVAudioUnitComponent = interface;
  AVAudioUnitComponentManager = interface;
  AVAudioUnitDelay = interface;
  AVAudioUnitDistortion = interface;
  AVAudioUnitGenerator = interface;
  AVAudioUnitMIDIInstrument = interface;
  AVAudioUnitSampler = interface;
  AVAudioUnitTimeEffect = interface;
  AVAudioUnitTimePitch = interface;
  AVAudioUnitVarispeed = interface;
  AVMIDIPlayer = interface;
  AVSpeechSynthesisVoice = interface;
  AVSpeechUtterance = interface;
  AVSpeechSynthesizer = interface;
  AVSpeechSynthesizerDelegate = interface;

  OSStatus = Integer;
  POSStatus = PInteger;
  PSingle = ^Single;
  PPSingle = ^PSingle;

  PBoolean = ^Boolean;
  PInt16 = ^Int16;
  PPInt16 = ^PInt16;
  PInt32 = ^Int32;
  PPInt32 = ^PInt32;
  PAVAudio3DPoint = ^AVAudio3DPoint;
  PAVAudio3DVectorOrientation = ^AVAudio3DVectorOrientation;
  PAVAudio3DAngularOrientation = ^AVAudio3DAngularOrientation;
  PAVAudioConverterPrimeInfo = ^AVAudioConverterPrimeInfo;
  P_AVBeatRange = ^_AVBeatRange;

  AVAudioFramePosition = Int64;
  AVAudioFrameCount = UInt32;
  AVAudioPacketCount = UInt32;
  AVAudioChannelCount = UInt32;

  AVAudioNodeCompletionHandler = procedure of object;
  AVAudioNodeBus = NSUInteger;

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

  AVAudio3DAngularOrientation = record
    yaw: Single;
    pitch: Single;
    roll: Single;
  end;

  AVAudioCommonFormat = NSInteger;
  AVAudioConverterPrimeMethod = NSInteger;

  AVAudioConverterPrimeInfo = record
    leadingFrames: AVAudioFrameCount;
    trailingFrames: AVAudioFrameCount;
  end;

  AVAudioConverterInputStatus = NSInteger;
  AVAudioConverterOutputStatus = NSInteger;
  PAVAudioConverterInputStatus = ^AVAudioConverterInputStatus;

  AVAudioConverterInputBlock = function(inNumberOfPackets: AVAudioPacketCount; outStatus: PAVAudioConverterInputStatus): AVAudioBuffer of object;

  AVAudioNodeTapBlock = procedure(buffer: AVAudioPCMBuffer; when: AVAudioTime) of object;
  AVAudio3DMixingRenderingAlgorithm = NSInteger;
  AVAudio3DMixingSourceMode = NSInteger;
  AVAudio3DMixingPointSourceInHeadMode = NSInteger;

  AVAudioIONodeInputBlock = function(inNumberOfFrames: AVAudioFrameCount): PAudioBufferList of object;
  AVAudioEngineManualRenderingError = NSInteger;
  AVAudioEngineManualRenderingStatus = NSInteger;
  AVAudioEngineManualRenderingMode = NSInteger;

  AVAudioEngineManualRenderingBlock = function(numberOfFrames: AVAudioFrameCount; outBuffer: PAudioBufferList;
    outError: POSStatus): AVAudioEngineManualRenderingStatus of object;
  AVAudioUnitReverbPreset = NSInteger;
  AVAudioUnitEQFilterType = NSInteger;
  AVAudioEnvironmentDistanceAttenuationModel = NSInteger;
  AVAudioEnvironmentOutputType = NSInteger;
  AVAudioQuality = NSInteger;
  AVAudioSessionPort = NSString;
  AVAudioSessionCategory = NSString;
  AVAudioSessionMode = NSString;
  AVAudioSessionActivationOptions = NSInteger;
  AVAudioSessionPortOverride = NSInteger;
  AVAudioSessionRouteChangeReason = NSInteger;
  AVAudioSessionCategoryOptions = NSInteger;
  AVAudioSessionInterruptionType = NSInteger;
  AVAudioSessionInterruptionOptions = NSInteger;
  AVAudioSessionInterruptionReason = NSInteger;
  AVAudioSessionSetActiveOptions = NSInteger;
  AVAudioSessionSilenceSecondaryAudioHintType = NSInteger;
  AVAudioSessionIOType = NSInteger;
  AVAudioSessionRouteSharingPolicy = NSInteger;
  AVAudioSessionPromptStyle = NSInteger;
  AVAudioStereoOrientation = NSInteger;
  AVAudioSessionRecordPermission = NSInteger;
  AVAudioSessionLocation = NSString;
  AVAudioSessionOrientation = NSString;
  AVAudioSessionPolarPattern = NSString;
  AVAudioPlayerNodeBufferOptions = NSInteger;
  AVAudioPlayerNodeCompletionCallbackType = NSInteger;

  AVAudioPlayerNodeCompletionHandler = procedure(callbackType: AVAudioPlayerNodeCompletionCallbackType) of object;
  AVAudioRoutingArbitrationCategory = NSInteger;
  AVMusicTimeStamp = Float64;
  AVMusicSequenceLoadOptions = NSInteger;

  _AVBeatRange = record
    start: AVMusicTimeStamp;
    length: AVMusicTimeStamp;
  end;

  AVBeatRange = _AVBeatRange;
  AVMusicTrackLoopCount = NSInteger;

  AVAudioSinkNodeReceiverBlock = function(timestamp: PAudioTimeStamp; frameCount: AVAudioFrameCount;
    inputData: PAudioBufferList): OSStatus of object;

  AVAudioSourceNodeRenderBlock = function(isSilence: PBoolean; timestamp: PAudioTimeStamp; frameCount: AVAudioFrameCount;
    outputData: PAudioBufferList): OSStatus of object;
  AVAudioUnitDistortionPreset = NSInteger;

  AVMIDIPlayerCompletionHandler = procedure of object;
  AVSpeechBoundary = NSInteger;
  AVSpeechSynthesisVoiceQuality = NSInteger;
  AVSpeechSynthesisVoiceGender = NSInteger;

  AVSpeechSynthesizerBufferCallback = procedure(buffer: AVAudioBuffer) of object;
  TAVAudioPCMBufferBlockMethod1 = procedure(param1: PAudioBufferList) of object;
  TAVAudioUnitBlockMethod1 = procedure(audioUnit: AVAudioUnit; error: NSError) of object;
  TAVAudioSessionBlockMethod1 = procedure(granted: Boolean) of object;
  TAVAudioSessionBlockMethod2 = procedure(activated: Boolean; error: NSError) of object;
  TAVAudioRoutingArbiterBlockMethod1 = procedure(defaultDeviceChanged: Boolean; error: NSError) of object;
  TAVAudioUnitComponentManagerBlockMethod1 = procedure(comp: AVAudioUnitComponent; stop: PBoolean) of object;

  AVAudioBufferClass = interface(NSObjectClass)
    ['{8A771200-966D-45A8-B913-718C56511E2B}']
  end;

  AVAudioBuffer = interface(NSObject)
    ['{9CD1CD47-76D4-46C7-8E01-5D9691EBEE21}']
    function audioBufferList: PAudioBufferList; cdecl;
    function format: AVAudioFormat; cdecl;
    function mutableAudioBufferList: PAudioBufferList; cdecl;
  end;
  TAVAudioBuffer = class(TOCGenericImport<AVAudioBufferClass, AVAudioBuffer>) end;

  AVAudioPCMBufferClass = interface(AVAudioBufferClass)
    ['{134E9329-8123-4218-945A-779FC51E5E0E}']
  end;

  AVAudioPCMBuffer = interface(AVAudioBuffer)
    ['{DF7FC7E9-C7BB-453C-9A64-C1D86E5741A3}']
    function floatChannelData: PPSingle; cdecl;
    function frameCapacity: AVAudioFrameCount; cdecl;
    function frameLength: AVAudioFrameCount; cdecl;
    function initWithPCMFormat(format: AVAudioFormat; frameCapacity: AVAudioFrameCount): Pointer; overload; cdecl;
    function initWithPCMFormat(format: AVAudioFormat; bufferListNoCopy: PAudioBufferList;
      deallocator: TAVAudioPCMBufferBlockMethod1): Pointer; overload; cdecl;
    function int16ChannelData: PPInt16; cdecl;
    function int32ChannelData: PPInt32; cdecl;
    procedure setFrameLength(frameLength: AVAudioFrameCount); cdecl;
    function stride: NSUInteger; cdecl;
  end;
  TAVAudioPCMBuffer = class(TOCGenericImport<AVAudioPCMBufferClass, AVAudioPCMBuffer>) end;

  AVAudioCompressedBufferClass = interface(AVAudioBufferClass)
    ['{AC9F97AE-6792-4976-BAEB-D2D3120BFD45}']
  end;

  AVAudioCompressedBuffer = interface(AVAudioBuffer)
    ['{4EAD4C44-A669-47F3-999C-D73AD1298DAE}']
    function byteCapacity: UInt32; cdecl;
    function byteLength: UInt32; cdecl;
    function data: Pointer; cdecl;
    function initWithFormat(format: AVAudioFormat; packetCapacity: AVAudioPacketCount; maximumPacketSize: NSInteger): Pointer; overload; cdecl;
    function initWithFormat(format: AVAudioFormat; packetCapacity: AVAudioPacketCount): Pointer; overload; cdecl;
    function maximumPacketSize: NSInteger; cdecl;
    function packetCapacity: AVAudioPacketCount; cdecl;
    function packetCount: AVAudioPacketCount; cdecl;
    function packetDescriptions: PAudioStreamPacketDescription; cdecl;
    procedure setByteLength(byteLength: UInt32); cdecl;
    procedure setPacketCount(packetCount: AVAudioPacketCount); cdecl;
  end;
  TAVAudioCompressedBuffer = class(TOCGenericImport<AVAudioCompressedBufferClass, AVAudioCompressedBuffer>) end;

  AVAudioChannelLayoutClass = interface(NSObjectClass)
    ['{5D3DAF50-BC94-4D7C-AA74-D37118E3A590}']
    {class} function layoutWithLayout(layout: PAudioChannelLayout): Pointer; cdecl;
    {class} function layoutWithLayoutTag(layoutTag: AudioChannelLayoutTag): Pointer; cdecl;
  end;

  AVAudioChannelLayout = interface(NSObject)
    ['{E43957F5-9C3B-4FD7-91EC-18D9ACB6F1BF}']
    function channelCount: AVAudioChannelCount; cdecl;
    function initWithLayout(layout: PAudioChannelLayout): Pointer; cdecl;
    function initWithLayoutTag(layoutTag: AudioChannelLayoutTag): Pointer; cdecl;
    function isEqual(&object: Pointer): Boolean; cdecl;
    function layout: PAudioChannelLayout; cdecl;
    function layoutTag: AudioChannelLayoutTag; cdecl;
  end;
  TAVAudioChannelLayout = class(TOCGenericImport<AVAudioChannelLayoutClass, AVAudioChannelLayout>) end;

  AVAudioConnectionPointClass = interface(NSObjectClass)
    ['{8CB882E7-9424-466E-AADC-3B395865F47A}']
  end;

  AVAudioConnectionPoint = interface(NSObject)
    ['{B6101E00-D506-4B4C-B414-F7F73D74BD73}']
    function bus: AVAudioNodeBus; cdecl;
    function initWithNode(node: AVAudioNode; bus: AVAudioNodeBus): Pointer; cdecl;
    function node: AVAudioNode; cdecl;
  end;
  TAVAudioConnectionPoint = class(TOCGenericImport<AVAudioConnectionPointClass, AVAudioConnectionPoint>) end;

  AVAudioFormatClass = interface(NSObjectClass)
    ['{0CCAF11C-F50A-4B25-B218-AE68616960C0}']
  end;

  AVAudioFormat = interface(NSObject)
    ['{83869621-B1A7-4755-A28B-C49F2F807D5F}']
    function channelCount: AVAudioChannelCount; cdecl;
    function channelLayout: AVAudioChannelLayout; cdecl;
    function commonFormat: AVAudioCommonFormat; cdecl;
    function formatDescription: CMAudioFormatDescriptionRef; cdecl;
    function initStandardFormatWithSampleRate(sampleRate: Double; channelLayout: AVAudioChannelLayout): Pointer; overload; cdecl;
    function initStandardFormatWithSampleRate(sampleRate: Double; channels: AVAudioChannelCount): Pointer; overload; cdecl;
    function initWithCMAudioFormatDescription(formatDescription: CMAudioFormatDescriptionRef): Pointer; cdecl;
    function initWithCommonFormat(format: AVAudioCommonFormat; sampleRate: Double; interleaved: Boolean;
      channelLayout: AVAudioChannelLayout): Pointer; overload; cdecl;
    function initWithCommonFormat(format: AVAudioCommonFormat; sampleRate: Double; channels: AVAudioChannelCount;
      interleaved: Boolean): Pointer; overload; cdecl;
    function initWithSettings(settings: NSDictionary): Pointer; cdecl;
    function initWithStreamDescription(asbd: PAudioStreamBasicDescription): Pointer; overload; cdecl;
    function initWithStreamDescription(asbd: PAudioStreamBasicDescription; channelLayout: AVAudioChannelLayout): Pointer; overload; cdecl;
    function isEqual(&object: Pointer): Boolean; cdecl;
    function isInterleaved: Boolean; cdecl;
    function isStandard: Boolean; cdecl;
    function magicCookie: NSData; cdecl;
    function sampleRate: Double; cdecl;
    procedure setMagicCookie(magicCookie: NSData); cdecl;
    function settings: NSDictionary; cdecl;
    function streamDescription: PAudioStreamBasicDescription; cdecl;
  end;
  TAVAudioFormat = class(TOCGenericImport<AVAudioFormatClass, AVAudioFormat>) end;

  AVAudioConverterClass = interface(NSObjectClass)
    ['{E9CFD611-2B80-4226-A63D-9A552D407AFB}']
  end;

  AVAudioConverter = interface(NSObject)
    ['{F8601F75-B3E2-43F5-8E8D-9A908E1B838B}']
    function applicableEncodeBitRates: NSArray; cdecl;
    function applicableEncodeSampleRates: NSArray; cdecl;
    function availableEncodeBitRates: NSArray; cdecl;
    function availableEncodeChannelLayoutTags: NSArray; cdecl;
    function availableEncodeSampleRates: NSArray; cdecl;
    function bitRate: NSInteger; cdecl;
    function bitRateStrategy: NSString; cdecl;
    function channelMap: NSArray; cdecl;
    function convertToBuffer(outputBuffer: AVAudioPCMBuffer; fromBuffer: AVAudioPCMBuffer; error: PPointer): Boolean; overload; cdecl;
    function convertToBuffer(outputBuffer: AVAudioBuffer; error: PPointer;
      withInputFromBlock: AVAudioConverterInputBlock): AVAudioConverterOutputStatus; overload; cdecl;
    function dither: Boolean; cdecl;
    function downmix: Boolean; cdecl;
    function initFromFormat(fromFormat: AVAudioFormat; toFormat: AVAudioFormat): Pointer; cdecl;
    function inputFormat: AVAudioFormat; cdecl;
    function magicCookie: NSData; cdecl;
    function maximumOutputPacketSize: NSInteger; cdecl;
    function outputFormat: AVAudioFormat; cdecl;
    function primeInfo: AVAudioConverterPrimeInfo; cdecl;
    function primeMethod: AVAudioConverterPrimeMethod; cdecl;
    procedure reset; cdecl;
    function sampleRateConverterAlgorithm: NSString; cdecl;
    function sampleRateConverterQuality: NSInteger; cdecl;
    procedure setBitRate(bitRate: NSInteger); cdecl;
    procedure setBitRateStrategy(bitRateStrategy: NSString); cdecl;
    procedure setChannelMap(channelMap: NSArray); cdecl;
    procedure setDither(dither: Boolean); cdecl;
    procedure setDownmix(downmix: Boolean); cdecl;
    procedure setMagicCookie(magicCookie: NSData); cdecl;
    procedure setPrimeInfo(primeInfo: AVAudioConverterPrimeInfo); cdecl;
    procedure setPrimeMethod(primeMethod: AVAudioConverterPrimeMethod); cdecl;
    procedure setSampleRateConverterAlgorithm(sampleRateConverterAlgorithm: NSString); cdecl;
    procedure setSampleRateConverterQuality(sampleRateConverterQuality: NSInteger); cdecl;
  end;
  TAVAudioConverter = class(TOCGenericImport<AVAudioConverterClass, AVAudioConverter>) end;

  AVAudioNodeClass = interface(NSObjectClass)
    ['{82B2AD0E-BA29-46A4-82FA-34827059AE58}']
  end;

  AVAudioNode = interface(NSObject)
    ['{73E0852D-5D4C-4830-A3DD-0254F7C3794E}']
    // function AUAudioUnit: AUAudioUnit; cdecl; // AudioToolbox
    function engine: AVAudioEngine; cdecl;
    function inputFormatForBus(bus: AVAudioNodeBus): AVAudioFormat; cdecl;
    procedure installTapOnBus(bus: AVAudioNodeBus; bufferSize: AVAudioFrameCount; format: AVAudioFormat; block: AVAudioNodeTapBlock); cdecl;
    function lastRenderTime: AVAudioTime; cdecl;
    function latency: NSTimeInterval; cdecl;
    function nameForInputBus(bus: AVAudioNodeBus): NSString; cdecl;
    function nameForOutputBus(bus: AVAudioNodeBus): NSString; cdecl;
    function numberOfInputs: NSUInteger; cdecl;
    function numberOfOutputs: NSUInteger; cdecl;
    function outputFormatForBus(bus: AVAudioNodeBus): AVAudioFormat; cdecl;
    function outputPresentationLatency: NSTimeInterval; cdecl;
    procedure removeTapOnBus(bus: AVAudioNodeBus); cdecl;
    procedure reset; cdecl;
  end;
  TAVAudioNode = class(TOCGenericImport<AVAudioNodeClass, AVAudioNode>) end;

  AVAudioMixing = interface(IObjectiveC)
    ['{2F4E7C78-1BDB-4931-809D-36203091784F}']
    function destinationForMixer(mixer: AVAudioNode; bus: AVAudioNodeBus): AVAudioMixingDestination; cdecl;
    procedure setVolume(volume: Single); cdecl;
    function volume: Single; cdecl;
  end;

  AVAudioStereoMixing = interface(IObjectiveC)
    ['{68601749-D5E2-423B-B293-32861AEC49C2}']
    function pan: Single; cdecl;
    procedure setPan(pan: Single); cdecl;
  end;

  AVAudio3DMixing = interface(IObjectiveC)
    ['{9E291578-C3B8-40C2-929D-96B281CE40CB}']
    function obstruction: Single; cdecl;
    function occlusion: Single; cdecl;
    function pointSourceInHeadMode: AVAudio3DMixingPointSourceInHeadMode; cdecl;
    function position: AVAudio3DPoint; cdecl;
    function rate: Single; cdecl;
    function renderingAlgorithm: AVAudio3DMixingRenderingAlgorithm; cdecl;
    function reverbBlend: Single; cdecl;
    procedure setObstruction(obstruction: Single); cdecl;
    procedure setOcclusion(occlusion: Single); cdecl;
    procedure setPointSourceInHeadMode(pointSourceInHeadMode: AVAudio3DMixingPointSourceInHeadMode); cdecl;
    procedure setPosition(position: AVAudio3DPoint); cdecl;
    procedure setRate(rate: Single); cdecl;
    procedure setRenderingAlgorithm(renderingAlgorithm: AVAudio3DMixingRenderingAlgorithm); cdecl;
    procedure setReverbBlend(reverbBlend: Single); cdecl;
    procedure setSourceMode(sourceMode: AVAudio3DMixingSourceMode); cdecl;
    function sourceMode: AVAudio3DMixingSourceMode; cdecl;
  end;

  AVAudioMixingDestinationClass = interface(NSObjectClass)
    ['{2473EB98-E318-48F8-B38C-8CBF82AC6F04}']
  end;

  AVAudioMixingDestination = interface(NSObject)
    ['{2BAF3288-CC0F-4A26-B1B4-21116E9DF547}']
    function connectionPoint: AVAudioConnectionPoint; cdecl;
  end;
  TAVAudioMixingDestination = class(TOCGenericImport<AVAudioMixingDestinationClass, AVAudioMixingDestination>) end;

  AVAudioIONodeClass = interface(AVAudioNodeClass)
    ['{5017B19F-8F74-4F92-8FEA-4099EA3C6F76}']
  end;

  AVAudioIONode = interface(AVAudioNode)
    ['{6FB4FA75-9638-4285-B195-01BB1D43630C}']
    // function audioUnit: AudioUnit; cdecl;  // AudioToolbox
    function isVoiceProcessingEnabled: Boolean; cdecl;
    function presentationLatency: NSTimeInterval; cdecl;
    function setVoiceProcessingEnabled(enabled: Boolean; error: PPointer): Boolean; cdecl;
  end;
  TAVAudioIONode = class(TOCGenericImport<AVAudioIONodeClass, AVAudioIONode>) end;

  AVAudioInputNodeClass = interface(AVAudioIONodeClass)
    ['{6FE12EF0-E85B-4E2B-9FC6-145BC5B37C9B}']
  end;

  AVAudioInputNode = interface(AVAudioIONode)
    ['{E5FE48C9-DF9E-4842-8B25-AC891B245DFA}']
    function isVoiceProcessingAGCEnabled: Boolean; cdecl;
    function isVoiceProcessingBypassed: Boolean; cdecl;
    function isVoiceProcessingInputMuted: Boolean; cdecl;
    function setManualRenderingInputPCMFormat(format: AVAudioFormat; inputBlock: AVAudioIONodeInputBlock): Boolean; cdecl;
    procedure setVoiceProcessingAGCEnabled(voiceProcessingAGCEnabled: Boolean); cdecl;
    procedure setVoiceProcessingBypassed(voiceProcessingBypassed: Boolean); cdecl;
    procedure setVoiceProcessingInputMuted(voiceProcessingInputMuted: Boolean); cdecl;
  end;
  TAVAudioInputNode = class(TOCGenericImport<AVAudioInputNodeClass, AVAudioInputNode>) end;

  AVAudioOutputNodeClass = interface(AVAudioIONodeClass)
    ['{A6B49F74-1998-4A3F-AFAB-05363D2AE9F3}']
  end;

  AVAudioOutputNode = interface(AVAudioIONode)
    ['{FD0F9747-E8CB-4453-B077-972AE392FAA6}']
  end;
  TAVAudioOutputNode = class(TOCGenericImport<AVAudioOutputNodeClass, AVAudioOutputNode>) end;

  AVAudioTimeClass = interface(NSObjectClass)
    ['{93E12D6C-958A-463F-834F-B756188F672E}']
    {class} function hostTimeForSeconds(seconds: NSTimeInterval): UInt64; cdecl;
    {class} function secondsForHostTime(hostTime: UInt64): NSTimeInterval; cdecl;
    {class} function timeWithAudioTimeStamp(ts: PAudioTimeStamp; sampleRate: Double): Pointer; cdecl;
    {class} function timeWithHostTime(hostTime: UInt64; sampleTime: AVAudioFramePosition; atRate: Double): Pointer; overload; cdecl;
    {class} function timeWithHostTime(hostTime: UInt64): Pointer; overload; cdecl;
    {class} function timeWithSampleTime(sampleTime: AVAudioFramePosition; atRate: Double): Pointer; cdecl;
  end;

  AVAudioTime = interface(NSObject)
    ['{1A517FED-031A-4FB0-B7D6-BE16A208BA4F}']
    function audioTimeStamp: AudioTimeStamp; cdecl;
    function extrapolateTimeFromAnchor(anchorTime: AVAudioTime): AVAudioTime; cdecl;
    function hostTime: UInt64; cdecl;
    function initWithAudioTimeStamp(ts: PAudioTimeStamp; sampleRate: Double): Pointer; cdecl;
    function initWithHostTime(hostTime: UInt64; sampleTime: AVAudioFramePosition; atRate: Double): Pointer; overload; cdecl;
    function initWithHostTime(hostTime: UInt64): Pointer; overload; cdecl;
    function initWithSampleTime(sampleTime: AVAudioFramePosition; atRate: Double): Pointer; cdecl;
    function isHostTimeValid: Boolean; cdecl;
    function isSampleTimeValid: Boolean; cdecl;
    function sampleRate: Double; cdecl;
    function sampleTime: AVAudioFramePosition; cdecl;
  end;
  TAVAudioTime = class(TOCGenericImport<AVAudioTimeClass, AVAudioTime>) end;

  AVAudioEngineClass = interface(NSObjectClass)
    ['{D4C82DBA-F584-4EDF-8A2A-722B19D3A4BB}']
  end;

  AVAudioEngine = interface(NSObject)
    ['{C41A7A53-89C2-40F0-8261-4F986C591AE0}']
    function attachedNodes: NSSet; cdecl;
    procedure attachNode(node: AVAudioNode); cdecl;
    procedure connect(sourceNode: AVAudioNode; toConnectionPoints: NSArray; fromBus: AVAudioNodeBus; format: AVAudioFormat); overload; cdecl;
    procedure connect(node1: AVAudioNode; &to: AVAudioNode; format: AVAudioFormat); overload; cdecl;
    procedure connect(node1: AVAudioNode; &to: AVAudioNode; fromBus: AVAudioNodeBus; toBus: AVAudioNodeBus; format: AVAudioFormat); overload; cdecl;
    // AudioToolbox
    // procedure connectMIDI(sourceNode: AVAudioNode; &to: AVAudioNode; format: AVAudioFormat; block: AUMIDIOutputEventBlock); overload; cdecl;
    // procedure connectMIDI(sourceNode: AVAudioNode; toNodes: NSArray; format: AVAudioFormat; block: AUMIDIOutputEventBlock); overload; cdecl;
    procedure detachNode(node: AVAudioNode); cdecl;
    procedure disableManualRenderingMode; cdecl;
    procedure disconnectMIDI(sourceNode: AVAudioNode; from: AVAudioNode); overload; cdecl;
    procedure disconnectMIDI(sourceNode: AVAudioNode; fromNodes: NSArray); overload; cdecl;
    procedure disconnectMIDIInput(node: AVAudioNode); cdecl;
    procedure disconnectMIDIOutput(node: AVAudioNode); cdecl;
    procedure disconnectNodeInput(node: AVAudioNode); overload; cdecl;
    procedure disconnectNodeInput(node: AVAudioNode; bus: AVAudioNodeBus); overload; cdecl;
    procedure disconnectNodeOutput(node: AVAudioNode); overload; cdecl;
    procedure disconnectNodeOutput(node: AVAudioNode; bus: AVAudioNodeBus); overload; cdecl;
    function enableManualRenderingMode(mode: AVAudioEngineManualRenderingMode; format: AVAudioFormat; maximumFrameCount: AVAudioFrameCount;
      error: PPointer): Boolean; cdecl;
    function inputConnectionPointForNode(node: AVAudioNode; inputBus: AVAudioNodeBus): AVAudioConnectionPoint; cdecl;
    function inputNode: AVAudioInputNode; cdecl;
    function isAutoShutdownEnabled: Boolean; cdecl;
    function isInManualRenderingMode: Boolean; cdecl;
    function isRunning: Boolean; cdecl;
    function mainMixerNode: AVAudioMixerNode; cdecl;
    function manualRenderingBlock: AVAudioEngineManualRenderingBlock; cdecl;
    function manualRenderingFormat: AVAudioFormat; cdecl;
    function manualRenderingMaximumFrameCount: AVAudioFrameCount; cdecl;
    function manualRenderingMode: AVAudioEngineManualRenderingMode; cdecl;
    function manualRenderingSampleTime: AVAudioFramePosition; cdecl;
    // AudioToolbox
    // function musicSequence: MusicSequence; cdecl;
    function outputConnectionPointsForNode(node: AVAudioNode; outputBus: AVAudioNodeBus): NSArray; cdecl;
    function outputNode: AVAudioOutputNode; cdecl;
    procedure pause; cdecl;
    procedure prepare; cdecl;
    function renderOffline(numberOfFrames: AVAudioFrameCount; toBuffer: AVAudioPCMBuffer; error: PPointer): AVAudioEngineManualRenderingStatus; cdecl;
    procedure reset; cdecl;
    procedure setAutoShutdownEnabled(autoShutdownEnabled: Boolean); cdecl;
    // AudioToolbox
    // procedure setMusicSequence(musicSequence: MusicSequence); cdecl;
    function startAndReturnError(outError: PPointer): Boolean; cdecl;
    procedure stop; cdecl;
  end;
  TAVAudioEngine = class(TOCGenericImport<AVAudioEngineClass, AVAudioEngine>) end;

  AVAudioUnitClass = interface(AVAudioNodeClass)
    ['{C0E6DD95-0B57-4953-9D24-85BDF65CD7AA}']
    // AudioToolbox
    // {class} procedure instantiateWithComponentDescription(audioComponentDescription: AudioComponentDescription;
    //   options: AudioComponentInstantiationOptions; completionHandler: TAVAudioUnitBlockMethod1); cdecl;
  end;

  AVAudioUnit = interface(AVAudioNode)
    ['{F77B8B74-7A5A-4EFF-A0C4-3683733552CC}']
    // AudioToolbox
    // function AUAudioUnit: AUAudioUnit; cdecl;
    // function audioComponentDescription: AudioComponentDescription; cdecl;
    // function audioUnit: AudioUnit; cdecl;
    function loadAudioUnitPresetAtURL(url: NSURL; error: PPointer): Boolean; cdecl;
    function manufacturerName: NSString; cdecl;
    function name: NSString; cdecl;
    function version: NSUInteger; cdecl;
  end;
  TAVAudioUnit = class(TOCGenericImport<AVAudioUnitClass, AVAudioUnit>) end;

  AVAudioUnitEffectClass = interface(AVAudioUnitClass)
    ['{9DFD9F87-CC92-482E-AD1F-6DD888F44702}']
  end;

  AVAudioUnitEffect = interface(AVAudioUnit)
    ['{ECA36D2F-565B-4007-B48E-9CBF158FD8FB}']
    function bypass: Boolean; cdecl;
    // AudioToolbox
    // function initWithAudioComponentDescription(audioComponentDescription: AudioComponentDescription): Pointer; cdecl;
    procedure setBypass(bypass: Boolean); cdecl;
  end;
  TAVAudioUnitEffect = class(TOCGenericImport<AVAudioUnitEffectClass, AVAudioUnitEffect>) end;

  AVAudioUnitReverbClass = interface(AVAudioUnitEffectClass)
    ['{F98C9D9C-DEF1-4E7A-9DEB-55E5A07AF90D}']
  end;

  AVAudioUnitReverb = interface(AVAudioUnitEffect)
    ['{CBA4F1E8-BC51-4C8F-9578-AF379E5BB8F0}']
    procedure loadFactoryPreset(preset: AVAudioUnitReverbPreset); cdecl;
    procedure setWetDryMix(wetDryMix: Single); cdecl;
    function wetDryMix: Single; cdecl;
  end;
  TAVAudioUnitReverb = class(TOCGenericImport<AVAudioUnitReverbClass, AVAudioUnitReverb>) end;

  AVAudioUnitEQFilterParametersClass = interface(NSObjectClass)
    ['{8167CDE4-47C0-458A-9F62-6392343F0F95}']
  end;

  AVAudioUnitEQFilterParameters = interface(NSObject)
    ['{F0442980-DC89-4213-B6F1-BCE1930BBFC0}']
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

  AVAudioUnitEQClass = interface(AVAudioUnitEffectClass)
    ['{A11D63B2-7DE2-4313-A0FE-7A513039D16E}']
  end;

  AVAudioUnitEQ = interface(AVAudioUnitEffect)
    ['{38BD6350-270E-451F-883D-9127A030E130}']
    function bands: NSArray; cdecl;
    function globalGain: Single; cdecl;
    function initWithNumberOfBands(numberOfBands: NSUInteger): Pointer; cdecl;
    procedure setGlobalGain(globalGain: Single); cdecl;
  end;
  TAVAudioUnitEQ = class(TOCGenericImport<AVAudioUnitEQClass, AVAudioUnitEQ>) end;

  AVAudioEnvironmentDistanceAttenuationParametersClass = interface(NSObjectClass)
    ['{8631B65D-D555-4378-9310-C6D8CC9449CE}']
  end;

  AVAudioEnvironmentDistanceAttenuationParameters = interface(NSObject)
    ['{A987B5A3-68FA-42F6-933F-D0B81D7044FE}']
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

  AVAudioEnvironmentReverbParametersClass = interface(NSObjectClass)
    ['{68F5488F-283D-4143-A418-62609EB7A4F0}']
  end;

  AVAudioEnvironmentReverbParameters = interface(NSObject)
    ['{FCB25601-39C8-4E99-BAC0-AB0AE4E8B0E5}']
    function enable: Boolean; cdecl;
    function filterParameters: AVAudioUnitEQFilterParameters; cdecl;
    function level: Single; cdecl;
    procedure loadFactoryReverbPreset(preset: AVAudioUnitReverbPreset); cdecl;
    procedure setEnable(enable: Boolean); cdecl;
    procedure setLevel(level: Single); cdecl;
  end;
  TAVAudioEnvironmentReverbParameters = class(TOCGenericImport<AVAudioEnvironmentReverbParametersClass, AVAudioEnvironmentReverbParameters>) end;

  AVAudioEnvironmentNodeClass = interface(AVAudioNodeClass)
    ['{D9A8A6BB-36FF-4CDC-9878-F0B847AEF0F8}']
  end;

  AVAudioEnvironmentNode = interface(AVAudioNode)
    ['{0A32D3D3-BB0F-4D05-8308-A277D5F035DE}']
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

  AVAudioFileClass = interface(NSObjectClass)
    ['{DBE83592-629E-45BA-8391-B57E0737FBA9}']
  end;

  AVAudioFile = interface(NSObject)
    ['{91CBCE2A-5F2C-4F5A-8F1E-6D38D29C4F8E}']
    function fileFormat: AVAudioFormat; cdecl;
    function framePosition: AVAudioFramePosition; cdecl;
    function initForReading(fileURL: NSURL; commonFormat: AVAudioCommonFormat; interleaved: Boolean; error: PPointer): Pointer; overload; cdecl;
    function initForReading(fileURL: NSURL; error: PPointer): Pointer; overload; cdecl;
    function initForWriting(fileURL: NSURL; settings: NSDictionary; commonFormat: AVAudioCommonFormat; interleaved: Boolean;
      error: PPointer): Pointer; overload; cdecl;
    function initForWriting(fileURL: NSURL; settings: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    function length: AVAudioFramePosition; cdecl;
    function processingFormat: AVAudioFormat; cdecl;
    function readIntoBuffer(buffer: AVAudioPCMBuffer; error: PPointer): Boolean; overload; cdecl;
    function readIntoBuffer(buffer: AVAudioPCMBuffer; frameCount: AVAudioFrameCount; error: PPointer): Boolean; overload; cdecl;
    procedure setFramePosition(framePosition: AVAudioFramePosition); cdecl;
    function url: NSURL; cdecl;
    function writeFromBuffer(buffer: AVAudioPCMBuffer; error: PPointer): Boolean; cdecl;
  end;
  TAVAudioFile = class(TOCGenericImport<AVAudioFileClass, AVAudioFile>) end;

  AVAudioMixerNodeClass = interface(AVAudioNodeClass)
    ['{94ED5BD9-5200-48C8-A9AD-506D11B473D1}']
  end;

  AVAudioMixerNode = interface(AVAudioNode)
    ['{548B15F6-D301-4035-A966-0BB79C11B32F}']
    function nextAvailableInputBus: AVAudioNodeBus; cdecl;
    function outputVolume: Single; cdecl;
    procedure setOutputVolume(outputVolume: Single); cdecl;
  end;
  TAVAudioMixerNode = class(TOCGenericImport<AVAudioMixerNodeClass, AVAudioMixerNode>) end;

  AVAudioSessionChannelDescriptionClass = interface(NSObjectClass)
    ['{0566C464-BE12-48DE-915D-811DA228C097}']
  end;

  AVAudioSessionChannelDescription = interface(NSObject)
    ['{9FDD1B6A-497A-40C2-A1ED-9F20BD6398C1}']
    function channelLabel: AudioChannelLabel; cdecl;
    function channelName: NSString; cdecl;
    function channelNumber: NSUInteger; cdecl;
    function owningPortUID: NSString; cdecl;
  end;
  TAVAudioSessionChannelDescription = class(TOCGenericImport<AVAudioSessionChannelDescriptionClass, AVAudioSessionChannelDescription>) end;

  AVAudioSessionDataSourceDescriptionClass = interface(NSObjectClass)
    ['{A8CA519E-F529-4412-B20F-145FC2C7BA95}']
  end;

  AVAudioSessionDataSourceDescription = interface(NSObject)
    ['{7B1AEE25-C90E-48A0-9FA4-073B1448AC62}']
    function dataSourceID: NSNumber; cdecl;
    function dataSourceName: NSString; cdecl;
    function location: AVAudioSessionLocation; cdecl;
    function orientation: AVAudioSessionOrientation; cdecl;
    function preferredPolarPattern: AVAudioSessionPolarPattern; cdecl;
    function selectedPolarPattern: AVAudioSessionPolarPattern; cdecl;
    function setPreferredPolarPattern(pattern: AVAudioSessionPolarPattern; error: PPointer): Boolean; cdecl;
    function supportedPolarPatterns: NSArray; cdecl;
  end;
  TAVAudioSessionDataSourceDescription = class(TOCGenericImport<AVAudioSessionDataSourceDescriptionClass, AVAudioSessionDataSourceDescription>) end;

  AVAudioSessionPortDescriptionClass = interface(NSObjectClass)
    ['{01D2065E-B140-4296-BC8C-99BE54EA84F0}']
  end;

  AVAudioSessionPortDescription = interface(NSObject)
    ['{0DA11354-75F5-47B6-A98F-1B896571096B}']
    function channels: NSArray; cdecl;
    function dataSources: NSArray; cdecl;
    function hasHardwareVoiceCallProcessing: Boolean; cdecl;
    function isSpatialAudioEnabled: Boolean; cdecl;
    function portName: NSString; cdecl;
    function portType: AVAudioSessionPort; cdecl;
    function preferredDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function selectedDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function setPreferredDataSource(dataSource: AVAudioSessionDataSourceDescription; error: PPointer): Boolean; cdecl;
    function UID: NSString; cdecl;
  end;
  TAVAudioSessionPortDescription = class(TOCGenericImport<AVAudioSessionPortDescriptionClass, AVAudioSessionPortDescription>) end;

  AVAudioSessionRouteDescriptionClass = interface(NSObjectClass)
    ['{73D7D94C-C425-407B-BD3B-96D92E07A323}']
  end;

  AVAudioSessionRouteDescription = interface(NSObject)
    ['{F7040979-D5DB-4699-AC43-43CBB38BB08B}']
    function inputs: NSArray; cdecl;
    function outputs: NSArray; cdecl;
  end;
  TAVAudioSessionRouteDescription = class(TOCGenericImport<AVAudioSessionRouteDescriptionClass, AVAudioSessionRouteDescription>) end;

  AVAudioSessionClass = interface(NSObjectClass)
    ['{B2552E09-F415-4CFF-BD76-8FB5F6426CE6}']
    {class} function sharedInstance: AVAudioSession; cdecl;
  end;

  AVAudioSession = interface(NSObject)
    ['{A2F4EF18-413F-4AC6-BCF6-F84F6727075F}']
    procedure activateWithOptions(options: AVAudioSessionActivationOptions; completionHandler: TAVAudioSessionBlockMethod2); cdecl;
    function allowHapticsAndSystemSoundsDuringRecording: Boolean; cdecl;
    function availableCategories: NSArray; cdecl;
    function availableInputs: NSArray; cdecl;
    function availableModes: NSArray; cdecl;
    function category: AVAudioSessionCategory; cdecl;
    function categoryOptions: AVAudioSessionCategoryOptions; cdecl;
    function currentHardwareInputNumberOfChannels: NSInteger; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("inputNumberOfChannels", ios(3.0, 6.0), macCatalyst(14.0, 14.0))
    function currentHardwareOutputNumberOfChannels: NSInteger; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("outputNumberOfChannels", ios(3.0, 6.0), macCatalyst(14.0, 14.0))
    function currentHardwareSampleRate: Double; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("sampleRate", ios(3.0, 6.0), macCatalyst(14.0, 14.0))
    function currentRoute: AVAudioSessionRouteDescription; cdecl;
    function delegate: Pointer; cdecl; // API_DEPRECATED("No longer supported", ios(4.0, 6.0), macCatalyst(14.0, 14.0))
    function init: Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+sharedInstance", ios(3.0, 10.0), macCatalyst(14.0, 14.0))
    function inputDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function inputDataSources: NSArray; cdecl;
    function inputGain: Single; cdecl;
    function inputIsAvailable: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("isInputAvailable", ios(3.0, 6.0), macCatalyst(14.0, 14.0))
    function inputLatency: NSTimeInterval; cdecl;
    function inputNumberOfChannels: NSInteger; cdecl;
    function inputOrientation: AVAudioStereoOrientation; cdecl;
    function IOBufferDuration: NSTimeInterval; cdecl;
    function isInputAvailable: Boolean; cdecl;
    function isInputGainSettable: Boolean; cdecl;
    function isOtherAudioPlaying: Boolean; cdecl;
    function maximumInputNumberOfChannels: NSInteger; cdecl;
    function maximumOutputNumberOfChannels: NSInteger; cdecl;
    function mode: AVAudioSessionMode; cdecl;
    function outputDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function outputDataSources: NSArray; cdecl;
    function outputLatency: NSTimeInterval; cdecl;
    function outputNumberOfChannels: NSInteger; cdecl;
    function outputVolume: Single; cdecl;
    function overrideOutputAudioPort(portOverride: AVAudioSessionPortOverride; error: PPointer): Boolean; cdecl;
    function preferredHardwareSampleRate: Double; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preferredSampleRate", ios(3.0, 6.0), macCatalyst(14.0, 14.0))
    function preferredInput: AVAudioSessionPortDescription; cdecl;
    function preferredInputNumberOfChannels: NSInteger; cdecl;
    function preferredInputOrientation: AVAudioStereoOrientation; cdecl;
    function preferredIOBufferDuration: NSTimeInterval; cdecl;
    function preferredOutputNumberOfChannels: NSInteger; cdecl;
    function preferredSampleRate: Double; cdecl;
    function prefersNoInterruptionsFromSystemAlerts: Boolean; cdecl;
    function promptStyle: AVAudioSessionPromptStyle; cdecl;
    function recordPermission: AVAudioSessionRecordPermission; cdecl;
    procedure requestRecordPermission(response: TAVAudioSessionBlockMethod1); cdecl;
    function routeSharingPolicy: AVAudioSessionRouteSharingPolicy; cdecl;
    function sampleRate: Double; cdecl;
    function secondaryAudioShouldBeSilencedHint: Boolean; cdecl;
    // function setActive(active: Boolean; withFlags: NSInteger; error: PPointer): Boolean; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-setActive:withOptions:error:", ios(4.0, 6.0), macCatalyst(14.0, 14.0))
    function setActive(active: Boolean; error: PPointer): Boolean; overload; cdecl;
    function setActive(active: Boolean; withOptions: AVAudioSessionSetActiveOptions; error: PPointer): Boolean; overload; cdecl;
    function setAggregatedIOPreference(inIOType: AVAudioSessionIOType; error: PPointer): Boolean; cdecl;
    function setAllowHapticsAndSystemSoundsDuringRecording(inValue: Boolean; error: PPointer): Boolean; cdecl;
    function setCategory(category: AVAudioSessionCategory; error: PPointer): Boolean; overload; cdecl;
    function setCategory(category: AVAudioSessionCategory; withOptions: AVAudioSessionCategoryOptions;
      error: PPointer): Boolean; overload; cdecl;
    function setCategory(category: AVAudioSessionCategory; mode: AVAudioSessionMode; options: AVAudioSessionCategoryOptions;
      error: PPointer): Boolean; overload; cdecl;
    function setCategory(category: AVAudioSessionCategory; mode: AVAudioSessionMode; routeSharingPolicy: AVAudioSessionRouteSharingPolicy;
      options: AVAudioSessionCategoryOptions; error: PPointer): Boolean; overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl; // API_DEPRECATED("No longer supported", ios(4.0, 6.0), macCatalyst(14.0, 14.0))
    function setInputDataSource(dataSource: AVAudioSessionDataSourceDescription; error: PPointer): Boolean; cdecl;
    function setInputGain(gain: Single; error: PPointer): Boolean; cdecl;
    function setMode(mode: AVAudioSessionMode; error: PPointer): Boolean; cdecl;
    function setOutputDataSource(dataSource: AVAudioSessionDataSourceDescription; error: PPointer): Boolean; cdecl;
    function setPreferredHardwareSampleRate(sampleRate: Double; error: PPointer): Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("setPreferredSampleRate:error:", ios(3.0, 6.0), macCatalyst(14.0, 14.0))
    function setPreferredInput(inPort: AVAudioSessionPortDescription; error: PPointer): Boolean; cdecl;
    function setPreferredInputNumberOfChannels(count: NSInteger; error: PPointer): Boolean; cdecl;
    function setPreferredInputOrientation(orientation: AVAudioStereoOrientation; error: PPointer): Boolean; cdecl;
    function setPreferredIOBufferDuration(duration: NSTimeInterval; error: PPointer): Boolean; cdecl;
    function setPreferredOutputNumberOfChannels(count: NSInteger; error: PPointer): Boolean; cdecl;
    function setPreferredSampleRate(sampleRate: Double; error: PPointer): Boolean; cdecl;
    function setPrefersNoInterruptionsFromSystemAlerts(inValue: Boolean; error: PPointer): Boolean; cdecl;
    function setSupportsMultichannelContent(inValue: Boolean; error: PPointer): Boolean; cdecl;
    function supportsMultichannelContent: Boolean; cdecl;
  end;
  TAVAudioSession = class(TOCGenericImport<AVAudioSessionClass, AVAudioSession>) end;

  AVAudioSessionDelegate = interface(IObjectiveC)
    ['{113ADF77-9C82-4C7A-B76E-C01C8AC885C1}']
    procedure beginInterruption; cdecl;
    procedure endInterruption; cdecl;
    procedure endInterruptionWithFlags(flags: NSUInteger); cdecl;
    procedure inputIsAvailableChanged(isInputAvailable: Boolean); cdecl;
  end;

  AVAudioPlayerClass = interface(NSObjectClass)
    ['{21DF239C-F7AF-483D-B870-9981C4441F76}']
  end;

  AVAudioPlayer = interface(NSObject)
    ['{47C2285A-D870-4E1D-9A2C-155AA843F88C}']
    function averagePowerForChannel(channelNumber: NSUInteger): Single; cdecl;
    function channelAssignments: NSArray; cdecl;
    function currentDevice: NSString; cdecl;
    function currentTime: NSTimeInterval; cdecl;
    function data: NSData; cdecl;
    function delegate: Pointer; cdecl;
    function deviceCurrentTime: NSTimeInterval; cdecl;
    function duration: NSTimeInterval; cdecl;
    function enableRate: Boolean; cdecl;
    function format: AVAudioFormat; cdecl;
    function initWithContentsOfURL(url: NSURL; error: PPointer): Pointer; overload; cdecl;
    function initWithContentsOfURL(url: NSURL; fileTypeHint: NSString; error: PPointer): Pointer; overload; cdecl;
    function initWithData(data: NSData; fileTypeHint: NSString; error: PPointer): Pointer; overload; cdecl;
    function initWithData(data: NSData; error: PPointer): Pointer; overload; cdecl;
    function isMeteringEnabled: Boolean; cdecl;
    function isPlaying: Boolean; cdecl;
    function numberOfChannels: NSUInteger; cdecl;
    function numberOfLoops: NSInteger; cdecl;
    function pan: Single; cdecl;
    procedure pause; cdecl;
    function peakPowerForChannel(channelNumber: NSUInteger): Single; cdecl;
    function play: Boolean; cdecl;
    function playAtTime(time: NSTimeInterval): Boolean; cdecl;
    function prepareToPlay: Boolean; cdecl;
    function rate: Single; cdecl;
    procedure setChannelAssignments(channelAssignments: NSArray); cdecl;
    procedure setCurrentDevice(currentDevice: NSString); cdecl;
    procedure setCurrentTime(currentTime: NSTimeInterval); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setEnableRate(enableRate: Boolean); cdecl;
    procedure setMeteringEnabled(meteringEnabled: Boolean); cdecl;
    procedure setNumberOfLoops(numberOfLoops: NSInteger); cdecl;
    procedure setPan(pan: Single); cdecl;
    procedure setRate(rate: Single); cdecl;
    function settings: NSDictionary; cdecl;
    procedure setVolume(volume: Single; fadeDuration: NSTimeInterval); overload; cdecl;
    procedure setVolume(volume: Single); overload; cdecl;
    procedure stop; cdecl;
    procedure updateMeters; cdecl;
    function url: NSURL; cdecl;
    function volume: Single; cdecl;
  end;
  TAVAudioPlayer = class(TOCGenericImport<AVAudioPlayerClass, AVAudioPlayer>) end;

  AVAudioPlayerDelegate = interface(IObjectiveC)
    ['{DDBA33FB-DDD4-4B5E-A300-92510F5AC30D}']
    procedure audioPlayerBeginInterruption(player: AVAudioPlayer); cdecl;
    procedure audioPlayerDecodeErrorDidOccur(player: AVAudioPlayer; error: NSError); cdecl;
    procedure audioPlayerDidFinishPlaying(player: AVAudioPlayer; successfully: Boolean); cdecl;
    procedure audioPlayerEndInterruption(player: AVAudioPlayer); cdecl;
    [MethodName('audioPlayerEndInterruption:withFlags:')]
    procedure audioPlayerEndInterruptionWithFlags(player: AVAudioPlayer; withFlags: NSUInteger); cdecl;
    [MethodName('audioPlayerEndInterruption:withOptions:')]
    procedure audioPlayerEndInterruptionWithOptions(player: AVAudioPlayer; withOptions: NSUInteger); cdecl;
  end;

  AVAudioPlayerNodeClass = interface(AVAudioNodeClass)
    ['{3A3B30DC-BF3F-4DDC-9234-88BF1C84348B}']
  end;

  AVAudioPlayerNode = interface(AVAudioNode)
    ['{E77F0FE8-918D-4837-A36F-7F4796848C28}']
    function isPlaying: Boolean; cdecl;
    function nodeTimeForPlayerTime(playerTime: AVAudioTime): AVAudioTime; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure playAtTime(when: AVAudioTime); cdecl;
    function playerTimeForNodeTime(nodeTime: AVAudioTime): AVAudioTime; cdecl;
    procedure prepareWithFrameCount(frameCount: AVAudioFrameCount); cdecl;
    procedure scheduleBuffer(buffer: AVAudioPCMBuffer; atTime: AVAudioTime; options: AVAudioPlayerNodeBufferOptions;
      completionCallbackType: AVAudioPlayerNodeCompletionCallbackType; completionHandler: AVAudioPlayerNodeCompletionHandler); overload; cdecl;
    procedure scheduleBuffer(buffer: AVAudioPCMBuffer; atTime: AVAudioTime; options: AVAudioPlayerNodeBufferOptions;
      completionHandler: AVAudioNodeCompletionHandler); overload; cdecl;
    procedure scheduleBuffer(buffer: AVAudioPCMBuffer; completionCallbackType: AVAudioPlayerNodeCompletionCallbackType;
      completionHandler: AVAudioPlayerNodeCompletionHandler); overload; cdecl;
    procedure scheduleBuffer(buffer: AVAudioPCMBuffer; completionHandler: AVAudioNodeCompletionHandler); overload; cdecl;
    procedure scheduleFile(&file: AVAudioFile; atTime: AVAudioTime; completionHandler: AVAudioNodeCompletionHandler); overload; cdecl;
    procedure scheduleFile(&file: AVAudioFile; atTime: AVAudioTime; completionCallbackType: AVAudioPlayerNodeCompletionCallbackType;
      completionHandler: AVAudioPlayerNodeCompletionHandler); overload; cdecl;
    procedure scheduleSegment(&file: AVAudioFile; startingFrame: AVAudioFramePosition; frameCount: AVAudioFrameCount; atTime: AVAudioTime;
      completionHandler: AVAudioNodeCompletionHandler); overload; cdecl;
    procedure scheduleSegment(&file: AVAudioFile; startingFrame: AVAudioFramePosition; frameCount: AVAudioFrameCount; atTime: AVAudioTime;
      completionCallbackType: AVAudioPlayerNodeCompletionCallbackType; completionHandler: AVAudioPlayerNodeCompletionHandler); overload; cdecl;
    procedure stop; cdecl;
  end;
  TAVAudioPlayerNode = class(TOCGenericImport<AVAudioPlayerNodeClass, AVAudioPlayerNode>) end;

  AVAudioRecorderClass = interface(NSObjectClass)
    ['{FBE1D17C-DB7E-406F-9995-D313106BD0DB}']
  end;

  AVAudioRecorder = interface(NSObject)
    ['{86C808F9-FF59-4F8D-8684-C1CA7DBBE398}']
    [MethodName('record')]
    function &record: Boolean; cdecl;
    function averagePowerForChannel(channelNumber: NSUInteger): Single; cdecl;
    function channelAssignments: NSArray; cdecl;
    function currentTime: NSTimeInterval; cdecl;
    function delegate: Pointer; cdecl;
    function deleteRecording: Boolean; cdecl;
    function deviceCurrentTime: NSTimeInterval; cdecl;
    function format: AVAudioFormat; cdecl;
    function initWithURL(url: NSURL; settings: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    function initWithURL(url: NSURL; format: AVAudioFormat; error: PPointer): Pointer; overload; cdecl;
    function isMeteringEnabled: Boolean; cdecl;
    function isRecording: Boolean; cdecl;
    procedure pause; cdecl;
    function peakPowerForChannel(channelNumber: NSUInteger): Single; cdecl;
    function prepareToRecord: Boolean; cdecl;
    function recordAtTime(time: NSTimeInterval; forDuration: NSTimeInterval): Boolean; overload; cdecl;
    function recordAtTime(time: NSTimeInterval): Boolean; overload; cdecl;
    function recordForDuration(duration: NSTimeInterval): Boolean; cdecl;
    procedure setChannelAssignments(channelAssignments: NSArray); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMeteringEnabled(meteringEnabled: Boolean); cdecl;
    function settings: NSDictionary; cdecl;
    procedure stop; cdecl;
    procedure updateMeters; cdecl;
    function url: NSURL; cdecl;
  end;
  TAVAudioRecorder = class(TOCGenericImport<AVAudioRecorderClass, AVAudioRecorder>) end;

  AVAudioRecorderDelegate = interface(IObjectiveC)
    ['{BDC50FA1-A8FF-4092-83A9-56FDC2C05B4F}']
    procedure audioRecorderBeginInterruption(recorder: AVAudioRecorder); cdecl;
    procedure audioRecorderDidFinishRecording(recorder: AVAudioRecorder; successfully: Boolean); cdecl;
    procedure audioRecorderEncodeErrorDidOccur(recorder: AVAudioRecorder; error: NSError); cdecl;
    procedure audioRecorderEndInterruption(recorder: AVAudioRecorder); cdecl;
    [MethodName('audioRecorderEndInterruption:withFlags:')]
    procedure audioRecorderEndInterruptionWithFlags(recorder: AVAudioRecorder; withFlags: NSUInteger); cdecl;
    [MethodName('audioRecorderEndInterruption:withOptions:')]
    procedure audioRecorderEndInterruptionWithOptions(recorder: AVAudioRecorder; withOptions: NSUInteger); cdecl;
  end;

  AVAudioRoutingArbiterClass = interface(NSObjectClass)
    ['{6BDC3509-4FAC-400E-8456-FFF6A5F1FE80}']
    {class} function new: Pointer; cdecl;
    {class} function sharedRoutingArbiter: AVAudioRoutingArbiter; cdecl;
  end;

  AVAudioRoutingArbiter = interface(NSObject)
    ['{B65BEA0B-8ED6-41FB-9E8E-C6C77B3AD2EC}']
    procedure beginArbitrationWithCategory(category: AVAudioRoutingArbitrationCategory; completionHandler: TAVAudioRoutingArbiterBlockMethod1); cdecl;
    procedure leaveArbitration; cdecl;
  end;
  TAVAudioRoutingArbiter = class(TOCGenericImport<AVAudioRoutingArbiterClass, AVAudioRoutingArbiter>) end;

  AVAudioSequencerClass = interface(NSObjectClass)
    ['{217C159B-58E6-415D-A9CA-A53E859525A2}']
  end;

  AVAudioSequencer = interface(NSObject)
    ['{16574BCF-6E87-4774-A500-7D3FCC32E47C}']
    function beatsForHostTime(inHostTime: UInt64; error: PPointer): AVMusicTimeStamp; cdecl;
    function beatsForSeconds(seconds: NSTimeInterval): AVMusicTimeStamp; cdecl;
    function currentPositionInBeats: NSTimeInterval; cdecl;
    function currentPositionInSeconds: NSTimeInterval; cdecl;
    function dataWithSMPTEResolution(SMPTEResolution: NSInteger; error: PPointer): NSData; cdecl;
    function hostTimeForBeats(inBeats: AVMusicTimeStamp; error: PPointer): UInt64; cdecl;
    function initWithAudioEngine(engine: AVAudioEngine): Pointer; cdecl;
    function isPlaying: Boolean; cdecl;
    function loadFromData(data: NSData; options: AVMusicSequenceLoadOptions; error: PPointer): Boolean; cdecl;
    function loadFromURL(fileURL: NSURL; options: AVMusicSequenceLoadOptions; error: PPointer): Boolean; cdecl;
    procedure prepareToPlay; cdecl;
    function rate: Single; cdecl;
    function secondsForBeats(beats: AVMusicTimeStamp): NSTimeInterval; cdecl;
    procedure setCurrentPositionInBeats(currentPositionInBeats: NSTimeInterval); cdecl;
    procedure setCurrentPositionInSeconds(currentPositionInSeconds: NSTimeInterval); cdecl;
    procedure setRate(rate: Single); cdecl;
    function startAndReturnError(outError: PPointer): Boolean; cdecl;
    procedure stop; cdecl;
    function tempoTrack: AVMusicTrack; cdecl;
    function tracks: NSArray; cdecl;
    function userInfo: NSDictionary; cdecl;
    function writeToURL(fileURL: NSURL; SMPTEResolution: NSInteger; replaceExisting: Boolean; error: PPointer): Boolean; cdecl;
  end;
  TAVAudioSequencer = class(TOCGenericImport<AVAudioSequencerClass, AVAudioSequencer>) end;

  AVMusicTrackClass = interface(NSObjectClass)
    ['{8D36BE31-D29B-48F2-B61B-C06EB9CFCFAE}']
  end;

  AVMusicTrack = interface(NSObject)
    ['{60A8F66C-DBC4-4669-BE7A-58E50A2F5045}']
    function destinationAudioUnit: AVAudioUnit; cdecl;
    // CoreMIDI
    // function destinationMIDIEndpoint: MIDIEndpointRef; cdecl;
    function isLoopingEnabled: Boolean; cdecl;
    function isMuted: Boolean; cdecl;
    function isSoloed: Boolean; cdecl;
    function lengthInBeats: AVMusicTimeStamp; cdecl;
    function lengthInSeconds: NSTimeInterval; cdecl;
    function loopRange: AVBeatRange; cdecl;
    function numberOfLoops: NSInteger; cdecl;
    function offsetTime: AVMusicTimeStamp; cdecl;
    procedure setDestinationAudioUnit(destinationAudioUnit: AVAudioUnit); cdecl;
    // CoreMIDI
    // procedure setDestinationMIDIEndpoint(destinationMIDIEndpoint: MIDIEndpointRef); cdecl;
    procedure setLengthInBeats(lengthInBeats: AVMusicTimeStamp); cdecl;
    procedure setLengthInSeconds(lengthInSeconds: NSTimeInterval); cdecl;
    procedure setLoopingEnabled(loopingEnabled: Boolean); cdecl;
    procedure setLoopRange(loopRange: AVBeatRange); cdecl;
    procedure setMuted(muted: Boolean); cdecl;
    procedure setNumberOfLoops(numberOfLoops: NSInteger); cdecl;
    procedure setOffsetTime(offsetTime: AVMusicTimeStamp); cdecl;
    procedure setSoloed(soloed: Boolean); cdecl;
    function timeResolution: NSUInteger; cdecl;
  end;
  TAVMusicTrack = class(TOCGenericImport<AVMusicTrackClass, AVMusicTrack>) end;

  AVAudioSinkNodeClass = interface(AVAudioNodeClass)
    ['{3112C2FE-63C8-4320-A3E1-2C0EBF8DBB39}']
  end;

  AVAudioSinkNode = interface(AVAudioNode)
    ['{15569A97-0858-4292-9151-51D0F171E9ED}']
    function initWithReceiverBlock(block: AVAudioSinkNodeReceiverBlock): Pointer; cdecl;
  end;
  TAVAudioSinkNode = class(TOCGenericImport<AVAudioSinkNodeClass, AVAudioSinkNode>) end;

  AVAudioSourceNodeClass = interface(AVAudioNodeClass)
    ['{AFC71BBB-EE29-45A2-8B3F-A81E85C9DB9C}']
  end;

  AVAudioSourceNode = interface(AVAudioNode)
    ['{197C97D0-72B6-493D-BFC7-04D6E3A301C7}']
    function initWithFormat(format: AVAudioFormat; renderBlock: AVAudioSourceNodeRenderBlock): Pointer; cdecl;
    function initWithRenderBlock(block: AVAudioSourceNodeRenderBlock): Pointer; cdecl;
  end;
  TAVAudioSourceNode = class(TOCGenericImport<AVAudioSourceNodeClass, AVAudioSourceNode>) end;

  AVAudioUnitComponentClass = interface(NSObjectClass)
    ['{8C482F6B-C64B-4BC2-A2CB-E5C4AF0BC7B7}']
  end;

  AVAudioUnitComponent = interface(NSObject)
    ['{C289C586-4345-4650-8156-15EDA324BD64}']
    function allTagNames: NSArray; cdecl;
    // AudioToolbox
    // function audioComponent: AudioComponent; cdecl;
    // function audioComponentDescription: AudioComponentDescription; cdecl;
    function availableArchitectures: NSArray; cdecl;
    function componentURL: NSURL; cdecl;
    function configurationDictionary: NSDictionary; cdecl;
    function hasCustomView: Boolean; cdecl;
    function hasMIDIInput: Boolean; cdecl;
    function hasMIDIOutput: Boolean; cdecl;
    function iconURL: NSURL; cdecl;
    function isSandboxSafe: Boolean; cdecl;
    function localizedTypeName: NSString; cdecl;
    function manufacturerName: NSString; cdecl;
    function name: NSString; cdecl;
    function passesAUVal: Boolean; cdecl;
    procedure setUserTagNames(userTagNames: NSArray); cdecl;
    function supportsNumberInputChannels(numInputChannels: NSInteger; outputChannels: NSInteger): Boolean; cdecl;
    function typeName: NSString; cdecl;
    function userTagNames: NSArray; cdecl;
    function version: NSUInteger; cdecl;
    function versionString: NSString; cdecl;
  end;
  TAVAudioUnitComponent = class(TOCGenericImport<AVAudioUnitComponentClass, AVAudioUnitComponent>) end;

  AVAudioUnitComponentManagerClass = interface(NSObjectClass)
    ['{E7B33CE0-6EC4-46D6-A8CB-3FAC91F100CF}']
    {class} function sharedAudioUnitComponentManager: Pointer; cdecl;
  end;

  AVAudioUnitComponentManager = interface(NSObject)
    ['{C04F91D6-958E-44B1-B928-01DEF94EEC60}']
    // AudioToolbox
    // function componentsMatchingDescription(desc: AudioComponentDescription): NSArray; cdecl;
    function componentsMatchingPredicate(predicate: NSPredicate): NSArray; cdecl;
    function componentsPassingTest(testHandler: TAVAudioUnitComponentManagerBlockMethod1): NSArray; cdecl;
    function standardLocalizedTagNames: NSArray; cdecl;
    function tagNames: NSArray; cdecl;
  end;
  TAVAudioUnitComponentManager = class(TOCGenericImport<AVAudioUnitComponentManagerClass, AVAudioUnitComponentManager>) end;

  AVAudioUnitDelayClass = interface(AVAudioUnitEffectClass)
    ['{C865540A-63B4-4CF0-81E4-8B878FD527BB}']
  end;

  AVAudioUnitDelay = interface(AVAudioUnitEffect)
    ['{40EFD94F-7EDD-454D-8916-FA711AA5CADA}']
    function delayTime: NSTimeInterval; cdecl;
    function feedback: Single; cdecl;
    function lowPassCutoff: Single; cdecl;
    procedure setDelayTime(delayTime: NSTimeInterval); cdecl;
    procedure setFeedback(feedback: Single); cdecl;
    procedure setLowPassCutoff(lowPassCutoff: Single); cdecl;
    procedure setWetDryMix(wetDryMix: Single); cdecl;
    function wetDryMix: Single; cdecl;
  end;
  TAVAudioUnitDelay = class(TOCGenericImport<AVAudioUnitDelayClass, AVAudioUnitDelay>) end;

  AVAudioUnitDistortionClass = interface(AVAudioUnitEffectClass)
    ['{EEA14985-9107-42FA-8A7B-A31170F81F56}']
  end;

  AVAudioUnitDistortion = interface(AVAudioUnitEffect)
    ['{2D66EA51-1413-4232-854D-A111F039A574}']
    procedure loadFactoryPreset(preset: AVAudioUnitDistortionPreset); cdecl;
    function preGain: Single; cdecl;
    procedure setPreGain(preGain: Single); cdecl;
    procedure setWetDryMix(wetDryMix: Single); cdecl;
    function wetDryMix: Single; cdecl;
  end;
  TAVAudioUnitDistortion = class(TOCGenericImport<AVAudioUnitDistortionClass, AVAudioUnitDistortion>) end;

  AVAudioUnitGeneratorClass = interface(AVAudioUnitClass)
    ['{85683A26-0041-41D1-A390-6817D9657E24}']
  end;

  AVAudioUnitGenerator = interface(AVAudioUnit)
    ['{B359B583-5F23-4420-87A5-37200FC018BB}']
    function bypass: Boolean; cdecl;
    // AudioToolbox
    // function initWithAudioComponentDescription(audioComponentDescription: AudioComponentDescription): Pointer; cdecl;
    procedure setBypass(bypass: Boolean); cdecl;
  end;
  TAVAudioUnitGenerator = class(TOCGenericImport<AVAudioUnitGeneratorClass, AVAudioUnitGenerator>) end;

  AVAudioUnitMIDIInstrumentClass = interface(AVAudioUnitClass)
    ['{BA60E0AC-F97A-4E92-8E3A-E34DAAC02103}']
  end;

  AVAudioUnitMIDIInstrument = interface(AVAudioUnit)
    ['{6258FA59-17B2-4091-90A0-5BADD2588D99}']
    // AudioToolbox
    // function initWithAudioComponentDescription(description: AudioComponentDescription): Pointer; cdecl;
    procedure sendController(controller: UInt8; withValue: UInt8; onChannel: UInt8); cdecl;
    procedure sendMIDIEvent(midiStatus: UInt8; data1: UInt8; data2: UInt8); overload; cdecl;
    procedure sendMIDIEvent(midiStatus: UInt8; data1: UInt8); overload; cdecl;
    procedure sendMIDISysExEvent(midiData: NSData); cdecl;
    procedure sendPitchBend(pitchbend: UInt16; onChannel: UInt8); cdecl;
    procedure sendPressure(pressure: UInt8; onChannel: UInt8); cdecl;
    procedure sendPressureForKey(key: UInt8; withValue: UInt8; onChannel: UInt8); cdecl;
    procedure sendProgramChange(&program: UInt8; bankMSB: UInt8; bankLSB: UInt8; onChannel: UInt8); overload; cdecl;
    procedure sendProgramChange(&program: UInt8; onChannel: UInt8); overload; cdecl;
    procedure startNote(note: UInt8; withVelocity: UInt8; onChannel: UInt8); cdecl;
    procedure stopNote(note: UInt8; onChannel: UInt8); cdecl;
  end;
  TAVAudioUnitMIDIInstrument = class(TOCGenericImport<AVAudioUnitMIDIInstrumentClass, AVAudioUnitMIDIInstrument>) end;

  AVAudioUnitSamplerClass = interface(AVAudioUnitMIDIInstrumentClass)
    ['{B6E6BACF-6581-447B-9D2A-8831C552969D}']
  end;

  AVAudioUnitSampler = interface(AVAudioUnitMIDIInstrument)
    ['{58D5602F-FB2E-49FB-8F13-DCC445689DC9}']
    function globalTuning: Single; cdecl;
    function loadAudioFilesAtURLs(audioFiles: NSArray; error: PPointer): Boolean; cdecl;
    function loadInstrumentAtURL(instrumentURL: NSURL; error: PPointer): Boolean; cdecl;
    function loadSoundBankInstrumentAtURL(bankURL: NSURL; &program: UInt8; bankMSB: UInt8; bankLSB: UInt8; error: PPointer): Boolean; cdecl;
    function masterGain: Single; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("overallGain", ios(8.0, 15.0), macos(10.10, 12.0), tvos(9.0, 15.0))
    function overallGain: Single; cdecl;
    procedure setGlobalTuning(globalTuning: Single); cdecl;
    procedure setMasterGain(masterGain: Single); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("overallGain", ios(8.0, 15.0), macos(10.10, 12.0), tvos(9.0, 15.0))
    procedure setOverallGain(overallGain: Single); cdecl;
    procedure setStereoPan(stereoPan: Single); cdecl;
    function stereoPan: Single; cdecl;
  end;
  TAVAudioUnitSampler = class(TOCGenericImport<AVAudioUnitSamplerClass, AVAudioUnitSampler>) end;

  AVAudioUnitTimeEffectClass = interface(AVAudioUnitClass)
    ['{9A0CB7DD-777C-498C-9D7C-27AD5D215E82}']
  end;

  AVAudioUnitTimeEffect = interface(AVAudioUnit)
    ['{A953D866-A985-4C01-844F-058DD1F08262}']
    function bypass: Boolean; cdecl;
    // AudioToolbox
    // function initWithAudioComponentDescription(audioComponentDescription: AudioComponentDescription): Pointer; cdecl;
    procedure setBypass(bypass: Boolean); cdecl;
  end;
  TAVAudioUnitTimeEffect = class(TOCGenericImport<AVAudioUnitTimeEffectClass, AVAudioUnitTimeEffect>) end;

  AVAudioUnitTimePitchClass = interface(AVAudioUnitTimeEffectClass)
    ['{D2DD697D-B1B8-4D14-BB65-860CA0C633E9}']
  end;

  AVAudioUnitTimePitch = interface(AVAudioUnitTimeEffect)
    ['{48EA2109-6CE7-4612-99F0-6698088484F9}']
    function overlap: Single; cdecl;
    function pitch: Single; cdecl;
    function rate: Single; cdecl;
    procedure setOverlap(overlap: Single); cdecl;
    procedure setPitch(pitch: Single); cdecl;
    procedure setRate(rate: Single); cdecl;
  end;
  TAVAudioUnitTimePitch = class(TOCGenericImport<AVAudioUnitTimePitchClass, AVAudioUnitTimePitch>) end;

  AVAudioUnitVarispeedClass = interface(AVAudioUnitTimeEffectClass)
    ['{A509EA7D-1745-4F48-9CF7-563C2E57A3BC}']
  end;

  AVAudioUnitVarispeed = interface(AVAudioUnitTimeEffect)
    ['{B1859546-492A-4260-A021-61C2302B95F0}']
    function rate: Single; cdecl;
    procedure setRate(rate: Single); cdecl;
  end;
  TAVAudioUnitVarispeed = class(TOCGenericImport<AVAudioUnitVarispeedClass, AVAudioUnitVarispeed>) end;

  AVMIDIPlayerClass = interface(NSObjectClass)
    ['{D7AD2098-A31E-467A-B741-621635A67FD8}']
  end;

  AVMIDIPlayer = interface(NSObject)
    ['{6BB01E74-D3C0-40DF-A9D7-E72BD7ECA6B4}']
    function currentPosition: NSTimeInterval; cdecl;
    function duration: NSTimeInterval; cdecl;
    function initWithContentsOfURL(inURL: NSURL; soundBankURL: NSURL; error: PPointer): Pointer; cdecl;
    function initWithData(data: NSData; soundBankURL: NSURL; error: PPointer): Pointer; cdecl;
    function isPlaying: Boolean; cdecl;
    procedure play(completionHandler: AVMIDIPlayerCompletionHandler); cdecl;
    procedure prepareToPlay; cdecl;
    function rate: Single; cdecl;
    procedure setCurrentPosition(currentPosition: NSTimeInterval); cdecl;
    procedure setRate(rate: Single); cdecl;
    procedure stop; cdecl;
  end;
  TAVMIDIPlayer = class(TOCGenericImport<AVMIDIPlayerClass, AVMIDIPlayer>) end;

  AVSpeechSynthesisVoiceClass = interface(NSObjectClass)
    ['{40460095-5DE9-47E6-AF67-35020979A8D8}']
    {class} function currentLanguageCode: NSString; cdecl;
    {class} function speechVoices: NSArray; cdecl;
    {class} function voiceWithIdentifier(identifier: NSString): AVSpeechSynthesisVoice; cdecl;
    {class} function voiceWithLanguage(languageCode: NSString): AVSpeechSynthesisVoice; cdecl;
  end;

  AVSpeechSynthesisVoice = interface(NSObject)
    ['{D229D11F-8B98-4F48-8135-161A0DA2A2BA}']
    function audioFileSettings: NSDictionary; cdecl;
    function gender: AVSpeechSynthesisVoiceGender; cdecl;
    function identifier: NSString; cdecl;
    function language: NSString; cdecl;
    function name: NSString; cdecl;
    function quality: AVSpeechSynthesisVoiceQuality; cdecl;
  end;
  TAVSpeechSynthesisVoice = class(TOCGenericImport<AVSpeechSynthesisVoiceClass, AVSpeechSynthesisVoice>) end;

  AVSpeechUtteranceClass = interface(NSObjectClass)
    ['{54508799-D178-4EEB-AB5D-B2F07FA45DC2}']
    {class} function speechUtteranceWithAttributedString(&string: NSAttributedString): Pointer; cdecl;
    {class} function speechUtteranceWithString(&string: NSString): Pointer; cdecl;
  end;

  AVSpeechUtterance = interface(NSObject)
    ['{51C688C2-F872-4DBD-9904-967CE56F8C4F}']
    function attributedSpeechString: NSAttributedString; cdecl;
    function initWithAttributedString(&string: NSAttributedString): Pointer; cdecl;
    function initWithString(&string: NSString): Pointer; cdecl;
    function pitchMultiplier: Single; cdecl;
    function postUtteranceDelay: NSTimeInterval; cdecl;
    function prefersAssistiveTechnologySettings: Boolean; cdecl;
    function preUtteranceDelay: NSTimeInterval; cdecl;
    function rate: Single; cdecl;
    procedure setPitchMultiplier(pitchMultiplier: Single); cdecl;
    procedure setPostUtteranceDelay(postUtteranceDelay: NSTimeInterval); cdecl;
    procedure setPrefersAssistiveTechnologySettings(prefersAssistiveTechnologySettings: Boolean); cdecl;
    procedure setPreUtteranceDelay(preUtteranceDelay: NSTimeInterval); cdecl;
    procedure setRate(rate: Single); cdecl;
    procedure setVoice(voice: AVSpeechSynthesisVoice); cdecl;
    procedure setVolume(volume: Single); cdecl;
    function speechString: NSString; cdecl;
    function voice: AVSpeechSynthesisVoice; cdecl;
    function volume: Single; cdecl;
  end;
  TAVSpeechUtterance = class(TOCGenericImport<AVSpeechUtteranceClass, AVSpeechUtterance>) end;

  AVSpeechSynthesizerClass = interface(NSObjectClass)
    ['{3E06281B-7AA9-43EF-B1A1-D112AB137193}']
  end;

  AVSpeechSynthesizer = interface(NSObject)
    ['{154C1160-52CA-4ED9-958D-2AC3D2B7566F}']
    function continueSpeaking: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function isPaused: Boolean; cdecl;
    function isSpeaking: Boolean; cdecl;
    function mixToTelephonyUplink: Boolean; cdecl;
    function outputChannels: NSArray; cdecl;
    function pauseSpeakingAtBoundary(boundary: AVSpeechBoundary): Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMixToTelephonyUplink(mixToTelephonyUplink: Boolean); cdecl;
    procedure setOutputChannels(outputChannels: NSArray); cdecl;
    procedure setUsesApplicationAudioSession(usesApplicationAudioSession: Boolean); cdecl;
    procedure speakUtterance(utterance: AVSpeechUtterance); cdecl;
    function stopSpeakingAtBoundary(boundary: AVSpeechBoundary): Boolean; cdecl;
    function usesApplicationAudioSession: Boolean; cdecl;
    procedure writeUtterance(utterance: AVSpeechUtterance; toBufferCallback: AVSpeechSynthesizerBufferCallback); cdecl;
  end;
  TAVSpeechSynthesizer = class(TOCGenericImport<AVSpeechSynthesizerClass, AVSpeechSynthesizer>) end;

  AVSpeechSynthesizerDelegate = interface(IObjectiveC)
    ['{B0A1930E-DF60-4247-9D4A-A68622D4618C}']
    [MethodName('speechSynthesizer:didCancelSpeechUtterance:')]
    procedure speechSynthesizerDidCancelSpeechUtterance(synthesizer: AVSpeechSynthesizer; didCancelSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didContinueSpeechUtterance:')]
    procedure speechSynthesizerDidContinueSpeechUtterance(synthesizer: AVSpeechSynthesizer; didContinueSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didFinishSpeechUtterance:')]
    procedure speechSynthesizerDidFinishSpeechUtterance(synthesizer: AVSpeechSynthesizer; didFinishSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didPauseSpeechUtterance:')]
    procedure speechSynthesizerDidPauseSpeechUtterance(synthesizer: AVSpeechSynthesizer; didPauseSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didStartSpeechUtterance:')]
    procedure speechSynthesizerDidStartSpeechUtterance(synthesizer: AVSpeechSynthesizer; didStartSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:willSpeakRangeOfSpeechString:utterance:')]
    procedure speechSynthesizerWillSpeakRangeOfSpeechString(synthesizer: AVSpeechSynthesizer; willSpeakRangeOfSpeechString: NSRange;
      utterance: AVSpeechUtterance); cdecl;
  end;

function AVAudioEngineConfigurationChangeNotification: NSString;
function AVFormatIDKey: NSString;
function AVSampleRateKey: NSString;
function AVNumberOfChannelsKey: NSString;
function AVLinearPCMBitDepthKey: NSString;
function AVLinearPCMIsBigEndianKey: NSString;
function AVLinearPCMIsFloatKey: NSString;
function AVLinearPCMIsNonInterleaved: NSString;
function AVAudioFileTypeKey: NSString;
function AVEncoderAudioQualityKey: NSString;
function AVEncoderAudioQualityForVBRKey: NSString;
function AVEncoderBitRateKey: NSString;
function AVEncoderBitRatePerChannelKey: NSString;
function AVEncoderBitRateStrategyKey: NSString;
function AVEncoderBitDepthHintKey: NSString;
function AVSampleRateConverterAlgorithmKey: NSString;
function AVSampleRateConverterAudioQualityKey: NSString;
function AVChannelLayoutKey: NSString;
function AVAudioBitRateStrategy_Constant: NSString;
function AVAudioBitRateStrategy_LongTermAverage: NSString;
function AVAudioBitRateStrategy_VariableConstrained: NSString;
function AVAudioBitRateStrategy_Variable: NSString;
function AVSampleRateConverterAlgorithm_Normal: NSString;
function AVSampleRateConverterAlgorithm_Mastering: NSString;
function AVSampleRateConverterAlgorithm_MinimumPhase: NSString;
function AVAudioSessionPortLineIn: AVAudioSessionPort;
function AVAudioSessionPortBuiltInMic: AVAudioSessionPort;
function AVAudioSessionPortHeadsetMic: AVAudioSessionPort;
function AVAudioSessionPortLineOut: AVAudioSessionPort;
function AVAudioSessionPortHeadphones: AVAudioSessionPort;
function AVAudioSessionPortBluetoothA2DP: AVAudioSessionPort;
function AVAudioSessionPortBuiltInReceiver: AVAudioSessionPort;
function AVAudioSessionPortBuiltInSpeaker: AVAudioSessionPort;
function AVAudioSessionPortHDMI: AVAudioSessionPort;
function AVAudioSessionPortAirPlay: AVAudioSessionPort;
function AVAudioSessionPortBluetoothLE: AVAudioSessionPort;
function AVAudioSessionPortBluetoothHFP: AVAudioSessionPort;
function AVAudioSessionPortUSBAudio: AVAudioSessionPort;
function AVAudioSessionPortCarAudio: AVAudioSessionPort;
function AVAudioSessionPortVirtual: AVAudioSessionPort;
function AVAudioSessionPortPCI: AVAudioSessionPort;
function AVAudioSessionPortFireWire: AVAudioSessionPort;
function AVAudioSessionPortDisplayPort: AVAudioSessionPort;
function AVAudioSessionPortAVB: AVAudioSessionPort;
function AVAudioSessionPortThunderbolt: AVAudioSessionPort;
function AVAudioSessionCategoryAmbient: AVAudioSessionCategory;
function AVAudioSessionCategorySoloAmbient: AVAudioSessionCategory;
function AVAudioSessionCategoryPlayback: AVAudioSessionCategory;
function AVAudioSessionCategoryRecord: AVAudioSessionCategory;
function AVAudioSessionCategoryPlayAndRecord: AVAudioSessionCategory;
function AVAudioSessionCategoryAudioProcessing: AVAudioSessionCategory;
function AVAudioSessionCategoryMultiRoute: AVAudioSessionCategory;
function AVAudioSessionModeDefault: AVAudioSessionMode;
function AVAudioSessionModeVoiceChat: AVAudioSessionMode;
function AVAudioSessionModeGameChat: AVAudioSessionMode;
function AVAudioSessionModeVideoRecording: AVAudioSessionMode;
function AVAudioSessionModeMeasurement: AVAudioSessionMode;
function AVAudioSessionModeMoviePlayback: AVAudioSessionMode;
function AVAudioSessionModeVideoChat: AVAudioSessionMode;
function AVAudioSessionModeSpokenAudio: AVAudioSessionMode;
function AVAudioSessionModeVoicePrompt: AVAudioSessionMode;
function AVAudioSessionLocationUpper: AVAudioSessionLocation;
function AVAudioSessionLocationLower: AVAudioSessionLocation;
function AVAudioSessionOrientationTop: AVAudioSessionOrientation;
function AVAudioSessionOrientationBottom: AVAudioSessionOrientation;
function AVAudioSessionOrientationFront: AVAudioSessionOrientation;
function AVAudioSessionOrientationBack: AVAudioSessionOrientation;
function AVAudioSessionOrientationLeft: AVAudioSessionOrientation;
function AVAudioSessionOrientationRight: AVAudioSessionOrientation;
function AVAudioSessionPolarPatternOmnidirectional: AVAudioSessionPolarPattern;
function AVAudioSessionPolarPatternCardioid: AVAudioSessionPolarPattern;
function AVAudioSessionPolarPatternSubcardioid: AVAudioSessionPolarPattern;
function AVAudioSessionPolarPatternStereo: AVAudioSessionPolarPattern;
function AVAudioSessionInterruptionNotification: NSNotificationName;
function AVAudioSessionRouteChangeNotification: NSNotificationName;
function AVAudioSessionMediaServicesWereLostNotification: NSNotificationName;
function AVAudioSessionMediaServicesWereResetNotification: NSNotificationName;
function AVAudioSessionSilenceSecondaryAudioHintNotification: NSNotificationName;
function AVAudioSessionSpatialPlaybackCapabilitiesChangedNotification: NSNotificationName;
function AVAudioSessionSpatialAudioEnabledKey: NSString;
function AVAudioSessionInterruptionTypeKey: NSString;
function AVAudioSessionInterruptionOptionKey: NSString;
function AVAudioSessionInterruptionReasonKey: NSString;
function AVAudioSessionInterruptionWasSuspendedKey: NSString;
function AVAudioSessionRouteChangeReasonKey: NSString;
function AVAudioSessionRouteChangePreviousRouteKey: NSString;
function AVAudioSessionSilenceSecondaryAudioHintTypeKey: NSString;
function AVAudioUnitTypeOutput: NSString;
function AVAudioUnitTypeMusicDevice: NSString;
function AVAudioUnitTypeMusicEffect: NSString;
function AVAudioUnitTypeFormatConverter: NSString;
function AVAudioUnitTypeEffect: NSString;
function AVAudioUnitTypeMixer: NSString;
function AVAudioUnitTypePanner: NSString;
function AVAudioUnitTypeGenerator: NSString;
function AVAudioUnitTypeOfflineEffect: NSString;
function AVAudioUnitTypeMIDIProcessor: NSString;
function AVAudioUnitManufacturerNameApple: NSString;
function AVAudioUnitComponentTagsDidChangeNotification: NSString;
function AVAudioUnitComponentManagerRegistrationsChangedNotification: NSNotificationName;
// Exported const AVSpeechUtteranceMinimumSpeechRate has an unsupported type: const float
// Exported const AVSpeechUtteranceMaximumSpeechRate has an unsupported type: const float
// Exported const AVSpeechUtteranceDefaultSpeechRate has an unsupported type: const float
function AVSpeechSynthesisVoiceIdentifierAlex: NSString;
function AVSpeechSynthesisIPANotationAttribute: NSString;

const
  libAVFAudio = '/System/Library/Frameworks/AVFAudio.framework/AVFAudio';

implementation

uses
  Posix.Dlfcn;

var
  AVFAudioModule: THandle;

function AVAudioEngineConfigurationChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioEngineConfigurationChangeNotification');
end;

function AVFormatIDKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVFormatIDKey');
end;

function AVSampleRateKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSampleRateKey');
end;

function AVNumberOfChannelsKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVNumberOfChannelsKey');
end;

function AVLinearPCMBitDepthKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVLinearPCMBitDepthKey');
end;

function AVLinearPCMIsBigEndianKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVLinearPCMIsBigEndianKey');
end;

function AVLinearPCMIsFloatKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVLinearPCMIsFloatKey');
end;

function AVLinearPCMIsNonInterleaved: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVLinearPCMIsNonInterleaved');
end;

function AVAudioFileTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioFileTypeKey');
end;

function AVEncoderAudioQualityKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVEncoderAudioQualityKey');
end;

function AVEncoderAudioQualityForVBRKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVEncoderAudioQualityForVBRKey');
end;

function AVEncoderBitRateKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVEncoderBitRateKey');
end;

function AVEncoderBitRatePerChannelKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVEncoderBitRatePerChannelKey');
end;

function AVEncoderBitRateStrategyKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVEncoderBitRateStrategyKey');
end;

function AVEncoderBitDepthHintKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVEncoderBitDepthHintKey');
end;

function AVSampleRateConverterAlgorithmKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSampleRateConverterAlgorithmKey');
end;

function AVSampleRateConverterAudioQualityKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSampleRateConverterAudioQualityKey');
end;

function AVChannelLayoutKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVChannelLayoutKey');
end;

function AVAudioBitRateStrategy_Constant: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioBitRateStrategy_Constant');
end;

function AVAudioBitRateStrategy_LongTermAverage: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioBitRateStrategy_LongTermAverage');
end;

function AVAudioBitRateStrategy_VariableConstrained: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioBitRateStrategy_VariableConstrained');
end;

function AVAudioBitRateStrategy_Variable: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioBitRateStrategy_Variable');
end;

function AVSampleRateConverterAlgorithm_Normal: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSampleRateConverterAlgorithm_Normal');
end;

function AVSampleRateConverterAlgorithm_Mastering: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSampleRateConverterAlgorithm_Mastering');
end;

function AVSampleRateConverterAlgorithm_MinimumPhase: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSampleRateConverterAlgorithm_MinimumPhase');
end;

function AVAudioSessionPortLineIn: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortLineIn');
end;

function AVAudioSessionPortBuiltInMic: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBuiltInMic');
end;

function AVAudioSessionPortHeadsetMic: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortHeadsetMic');
end;

function AVAudioSessionPortLineOut: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortLineOut');
end;

function AVAudioSessionPortHeadphones: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortHeadphones');
end;

function AVAudioSessionPortBluetoothA2DP: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBluetoothA2DP');
end;

function AVAudioSessionPortBuiltInReceiver: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBuiltInReceiver');
end;

function AVAudioSessionPortBuiltInSpeaker: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBuiltInSpeaker');
end;

function AVAudioSessionPortHDMI: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortHDMI');
end;

function AVAudioSessionPortAirPlay: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortAirPlay');
end;

function AVAudioSessionPortBluetoothLE: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBluetoothLE');
end;

function AVAudioSessionPortBluetoothHFP: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBluetoothHFP');
end;

function AVAudioSessionPortUSBAudio: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortUSBAudio');
end;

function AVAudioSessionPortCarAudio: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortCarAudio');
end;

function AVAudioSessionPortVirtual: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortVirtual');
end;

function AVAudioSessionPortPCI: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortPCI');
end;

function AVAudioSessionPortFireWire: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortFireWire');
end;

function AVAudioSessionPortDisplayPort: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortDisplayPort');
end;

function AVAudioSessionPortAVB: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortAVB');
end;

function AVAudioSessionPortThunderbolt: AVAudioSessionPort;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortThunderbolt');
end;

function AVAudioSessionCategoryAmbient: AVAudioSessionCategory;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryAmbient');
end;

function AVAudioSessionCategorySoloAmbient: AVAudioSessionCategory;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategorySoloAmbient');
end;

function AVAudioSessionCategoryPlayback: AVAudioSessionCategory;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryPlayback');
end;

function AVAudioSessionCategoryRecord: AVAudioSessionCategory;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryRecord');
end;

function AVAudioSessionCategoryPlayAndRecord: AVAudioSessionCategory;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryPlayAndRecord');
end;

function AVAudioSessionCategoryAudioProcessing: AVAudioSessionCategory;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryAudioProcessing');
end;

function AVAudioSessionCategoryMultiRoute: AVAudioSessionCategory;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryMultiRoute');
end;

function AVAudioSessionModeDefault: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeDefault');
end;

function AVAudioSessionModeVoiceChat: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeVoiceChat');
end;

function AVAudioSessionModeGameChat: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeGameChat');
end;

function AVAudioSessionModeVideoRecording: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeVideoRecording');
end;

function AVAudioSessionModeMeasurement: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeMeasurement');
end;

function AVAudioSessionModeMoviePlayback: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeMoviePlayback');
end;

function AVAudioSessionModeVideoChat: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeVideoChat');
end;

function AVAudioSessionModeSpokenAudio: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeSpokenAudio');
end;

function AVAudioSessionModeVoicePrompt: AVAudioSessionMode;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeVoicePrompt');
end;

function AVAudioSessionLocationUpper: AVAudioSessionLocation;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionLocationUpper');
end;

function AVAudioSessionLocationLower: AVAudioSessionLocation;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionLocationLower');
end;

function AVAudioSessionOrientationTop: AVAudioSessionOrientation;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationTop');
end;

function AVAudioSessionOrientationBottom: AVAudioSessionOrientation;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationBottom');
end;

function AVAudioSessionOrientationFront: AVAudioSessionOrientation;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationFront');
end;

function AVAudioSessionOrientationBack: AVAudioSessionOrientation;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationBack');
end;

function AVAudioSessionOrientationLeft: AVAudioSessionOrientation;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationLeft');
end;

function AVAudioSessionOrientationRight: AVAudioSessionOrientation;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationRight');
end;

function AVAudioSessionPolarPatternOmnidirectional: AVAudioSessionPolarPattern;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPolarPatternOmnidirectional');
end;

function AVAudioSessionPolarPatternCardioid: AVAudioSessionPolarPattern;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPolarPatternCardioid');
end;

function AVAudioSessionPolarPatternSubcardioid: AVAudioSessionPolarPattern;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPolarPatternSubcardioid');
end;

function AVAudioSessionPolarPatternStereo: AVAudioSessionPolarPattern;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPolarPatternStereo');
end;

function AVAudioSessionInterruptionNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionInterruptionNotification');
end;

function AVAudioSessionRouteChangeNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionRouteChangeNotification');
end;

function AVAudioSessionMediaServicesWereLostNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionMediaServicesWereLostNotification');
end;

function AVAudioSessionMediaServicesWereResetNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionMediaServicesWereResetNotification');
end;

function AVAudioSessionSilenceSecondaryAudioHintNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionSilenceSecondaryAudioHintNotification');
end;

function AVAudioSessionSpatialPlaybackCapabilitiesChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionSpatialPlaybackCapabilitiesChangedNotification');
end;

function AVAudioSessionSpatialAudioEnabledKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionSpatialAudioEnabledKey');
end;

function AVAudioSessionInterruptionTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionInterruptionTypeKey');
end;

function AVAudioSessionInterruptionOptionKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionInterruptionOptionKey');
end;

function AVAudioSessionInterruptionReasonKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionInterruptionReasonKey');
end;

function AVAudioSessionInterruptionWasSuspendedKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionInterruptionWasSuspendedKey');
end;

function AVAudioSessionRouteChangeReasonKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionRouteChangeReasonKey');
end;

function AVAudioSessionRouteChangePreviousRouteKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionRouteChangePreviousRouteKey');
end;

function AVAudioSessionSilenceSecondaryAudioHintTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionSilenceSecondaryAudioHintTypeKey');
end;

function AVAudioUnitTypeOutput: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeOutput');
end;

function AVAudioUnitTypeMusicDevice: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeMusicDevice');
end;

function AVAudioUnitTypeMusicEffect: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeMusicEffect');
end;

function AVAudioUnitTypeFormatConverter: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeFormatConverter');
end;

function AVAudioUnitTypeEffect: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeEffect');
end;

function AVAudioUnitTypeMixer: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeMixer');
end;

function AVAudioUnitTypePanner: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypePanner');
end;

function AVAudioUnitTypeGenerator: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeGenerator');
end;

function AVAudioUnitTypeOfflineEffect: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeOfflineEffect');
end;

function AVAudioUnitTypeMIDIProcessor: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitTypeMIDIProcessor');
end;

function AVAudioUnitManufacturerNameApple: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitManufacturerNameApple');
end;

function AVAudioUnitComponentTagsDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitComponentTagsDidChangeNotification');
end;

function AVAudioUnitComponentManagerRegistrationsChangedNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioUnitComponentManagerRegistrationsChangedNotification');
end;

function AVSpeechSynthesisVoiceIdentifierAlex: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSpeechSynthesisVoiceIdentifierAlex');
end;

function AVSpeechSynthesisIPANotationAttribute: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSpeechSynthesisIPANotationAttribute');
end;

initialization
  AVFAudioModule := dlopen(MarshaledAString(libAVFAudio), RTLD_LAZY);

finalization
  dlclose(AVFAudioModule);

end.