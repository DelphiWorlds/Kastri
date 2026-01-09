unit DW.Macapi.AVFAudio;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.CoreMedia, Macapi.CoreMIDI, Macapi.AppKit,
  // DW
  DW.Macapi.CoreAudioTypes;

const
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
  AVAudioVoiceProcessingSpeechActivityStarted = 0;
  AVAudioVoiceProcessingSpeechActivityEnded = 1;
  AVAudioVoiceProcessingOtherAudioDuckingLevelDefault = 0;
  AVAudioVoiceProcessingOtherAudioDuckingLevelMin = 10;
  AVAudioVoiceProcessingOtherAudioDuckingLevelMid = 20;
  AVAudioVoiceProcessingOtherAudioDuckingLevelMax = 30;
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
  AVAudioSessionInterruptionReasonRouteDisconnected = 4;
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
  AVAudioSessionRenderingModeNotApplicable = 0;
  AVAudioSessionRenderingModeMonoStereo = 1;
  AVAudioSessionRenderingModeSurround = 2;
  AVAudioSessionRenderingModeSpatialAudio = 3;
  AVAudioSessionRenderingModeDolbyAudio = 4;
  AVAudioSessionRenderingModeDolbyAtmos = 5;
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
  AVMusicSequenceLoadSMF_ChannelsToTracks = 1 shl 0;
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
  AVMIDIControlChangeMessageTypeBankSelect = 0;
  AVMIDIControlChangeMessageTypeModWheel = 1;
  AVMIDIControlChangeMessageTypeBreath = 2;
  AVMIDIControlChangeMessageTypeFoot = 4;
  AVMIDIControlChangeMessageTypePortamentoTime = 5;
  AVMIDIControlChangeMessageTypeDataEntry = 6;
  AVMIDIControlChangeMessageTypeVolume = 7;
  AVMIDIControlChangeMessageTypeBalance = 8;
  AVMIDIControlChangeMessageTypePan = 10;
  AVMIDIControlChangeMessageTypeExpression = 11;
  AVMIDIControlChangeMessageTypeSustain = 64;
  AVMIDIControlChangeMessageTypePortamento = 65;
  AVMIDIControlChangeMessageTypeSostenuto = 66;
  AVMIDIControlChangeMessageTypeSoft = 67;
  AVMIDIControlChangeMessageTypeLegatoPedal = 68;
  AVMIDIControlChangeMessageTypeHold2Pedal = 69;
  AVMIDIControlChangeMessageTypeFilterResonance = 71;
  AVMIDIControlChangeMessageTypeReleaseTime = 72;
  AVMIDIControlChangeMessageTypeAttackTime = 73;
  AVMIDIControlChangeMessageTypeBrightness = 74;
  AVMIDIControlChangeMessageTypeDecayTime = 75;
  AVMIDIControlChangeMessageTypeVibratoRate = 76;
  AVMIDIControlChangeMessageTypeVibratoDepth = 77;
  AVMIDIControlChangeMessageTypeVibratoDelay = 78;
  AVMIDIControlChangeMessageTypeReverbLevel = 91;
  AVMIDIControlChangeMessageTypeChorusLevel = 93;
  AVMIDIControlChangeMessageTypeRPN_LSB = 100;
  AVMIDIControlChangeMessageTypeRPN_MSB = 101;
  AVMIDIControlChangeMessageTypeAllSoundOff = 120;
  AVMIDIControlChangeMessageTypeResetAllControllers = 121;
  AVMIDIControlChangeMessageTypeAllNotesOff = 123;
  AVMIDIControlChangeMessageTypeOmniModeOff = 124;
  AVMIDIControlChangeMessageTypeOmniModeOn = 125;
  AVMIDIControlChangeMessageTypeMonoModeOn = 126;
  AVMIDIControlChangeMessageTypeMonoModeOff = 127;
  AVMIDIMetaEventTypeSequenceNumber = 0;
  AVMIDIMetaEventTypeText = 1;
  AVMIDIMetaEventTypeCopyright = 2;
  AVMIDIMetaEventTypeTrackName = 3;
  AVMIDIMetaEventTypeInstrument = 4;
  AVMIDIMetaEventTypeLyric = 5;
  AVMIDIMetaEventTypeMarker = 6;
  AVMIDIMetaEventTypeCuePoint = 7;
  AVMIDIMetaEventTypeMidiChannel = 32;
  AVMIDIMetaEventTypeMidiPort = 33;
  AVMIDIMetaEventTypeEndOfTrack = 47;
  AVMIDIMetaEventTypeTempo = 81;
  AVMIDIMetaEventTypeSmpteOffset = 84;
  AVMIDIMetaEventTypeTimeSignature = 88;
  AVMIDIMetaEventTypeKeySignature = 89;
  AVMIDIMetaEventTypeProprietaryEvent = 127;
  AVSpeechBoundaryImmediate = 0;
  AVSpeechBoundaryWord = 1;
  AVSpeechSynthesisVoiceQualityDefault = 1;
  AVSpeechSynthesisVoiceQualityEnhanced = 2;
  AVSpeechSynthesisVoiceQualityPremium = 3;
  AVSpeechSynthesisVoiceGenderUnspecified = 0;
  AVSpeechSynthesisVoiceGenderMale = 1;
  AVSpeechSynthesisVoiceGenderFemale = 2;
  AVSpeechSynthesisMarkerMarkPhoneme = 0;
  AVSpeechSynthesisMarkerMarkWord = 1;
  AVSpeechSynthesisMarkerMarkSentence = 2;
  AVSpeechSynthesisMarkerMarkParagraph = 3;
  AVSpeechSynthesisMarkerMarkBookmark = 4;
  AVSpeechSynthesisPersonalVoiceAuthorizationStatusNotDetermined = 0;
  AVSpeechSynthesisPersonalVoiceAuthorizationStatusDenied = 1;
  AVSpeechSynthesisPersonalVoiceAuthorizationStatusUnsupported = 2;
  AVSpeechSynthesisPersonalVoiceAuthorizationStatusAuthorized = 3;
  AVSpeechSynthesisVoiceTraitNone = 0;
  AVSpeechSynthesisVoiceTraitIsNoveltyVoice = 1;
  AVSpeechSynthesisVoiceTraitIsPersonalVoice = 2;
  AVAudioApplicationRecordPermissionUndetermined = 1970168948;
  AVAudioApplicationRecordPermissionDenied = 1684369017;
  AVAudioApplicationRecordPermissionGranted = 1735552628;

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
  AVMusicEvent = interface;
  AVMIDINoteEvent = interface;
  AVMIDIChannelEvent = interface;
  AVMIDIControlChangeEvent = interface;
  AVMIDIPolyPressureEvent = interface;
  AVMIDIProgramChangeEvent = interface;
  AVMIDIChannelPressureEvent = interface;
  AVMIDIPitchBendEvent = interface;
  AVMIDISysexEvent = interface;
  AVMIDIMetaEvent = interface;
  AVMusicUserEvent = interface;
  AVExtendedNoteOnEvent = interface;
  AVParameterEvent = interface;
  AVAUPresetEvent = interface;
  AVExtendedTempoEvent = interface;
  AVSpeechSynthesisVoice = interface;
  AVSpeechUtterance = interface;
  AVSpeechSynthesizer = interface;
  AVSpeechSynthesizerDelegate = interface;
  AVSpeechSynthesisMarker = interface;
  AVSpeechSynthesisProviderVoice = interface;
  AVSpeechSynthesisProviderRequest = interface;
  AVAudioApplication = interface;

  PInt16 = ^Int16;
  PPInt16 = ^PInt16;
  PInt32 = ^Int32;
  PPInt32 = ^PInt32;
  PBoolean = ^Boolean;
  PPSingle = ^PSingle;
  PAVAudio3DPoint = ^AVAudio3DPoint;
  PAVAudio3DVectorOrientation = ^AVAudio3DVectorOrientation;
  PAVAudio3DAngularOrientation = ^AVAudio3DAngularOrientation;
  PAVAudioConverterPrimeInfo = ^AVAudioConverterPrimeInfo;
  PAVAudioVoiceProcessingOtherAudioDuckingConfiguration = ^AVAudioVoiceProcessingOtherAudioDuckingConfiguration;
  P_AVBeatRange = ^_AVBeatRange;

  AVAudioFramePosition = Int64;
  AVAudioFrameCount = UInt32;
  AVAudioPacketCount = UInt32;
  AVAudioChannelCount = UInt32;

  AVAudioNodeCompletionHandler = procedure of object;
  AVAudioNodeBus = NSUInteger;
  AVMusicTimeStamp = Double;
  PAVMusicTimeStamp = ^AVMusicTimeStamp;

  OSStatus = Integer;
  POSStatus = PInteger;

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
  PAVAudioConverterInputStatus = ^AVAudioConverterInputStatus;
  AVAudioConverterOutputStatus = NSInteger;

  AVAudioConverterInputBlock = function(inNumberOfPackets: AVAudioPacketCount; outStatus: PAVAudioConverterInputStatus): AVAudioBuffer of object;

  AVAudioNodeTapBlock = procedure(buffer: AVAudioPCMBuffer; when: AVAudioTime) of object;
  AVAudio3DMixingRenderingAlgorithm = NSInteger;
  AVAudio3DMixingSourceMode = NSInteger;
  AVAudio3DMixingPointSourceInHeadMode = NSInteger;

  AVAudioIONodeInputBlock = function(inNumberOfFrames: AVAudioFrameCount): PAudioBufferList of object;
  AVAudioVoiceProcessingSpeechActivityEvent = NSInteger;
  AVAudioVoiceProcessingOtherAudioDuckingLevel = NSInteger;

  AVAudioVoiceProcessingOtherAudioDuckingConfiguration = record
    enableAdvancedDucking: Boolean;
    duckingLevel: AVAudioVoiceProcessingOtherAudioDuckingLevel;
  end;

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
  AVAudioSessionRenderingMode = NSInteger;
  AVAudioSessionLocation = NSString;
  AVAudioSessionOrientation = NSString;
  AVAudioSessionPolarPattern = NSString;
  AVAudioPlayerNodeBufferOptions = NSInteger;
  AVAudioPlayerNodeCompletionCallbackType = NSInteger;

  AVAudioPlayerNodeCompletionHandler = procedure(callbackType: AVAudioPlayerNodeCompletionCallbackType) of object;
  AVAudioRoutingArbitrationCategory = NSInteger;
  AVMusicSequenceLoadOptions = NSInteger;

  _AVBeatRange = record
    start: AVMusicTimeStamp;
    length: AVMusicTimeStamp;
  end;

  AVBeatRange = _AVBeatRange;
  AVAudioSequencerInfoDictionaryKey = NSString;

  AVAudioSequencerUserCallback = procedure(track: AVMusicTrack; userData: NSData; timeStamp: AVMusicTimeStamp) of object;
  AVMusicTrackLoopCount = NSInteger;

  AVMusicEventEnumerationBlock = procedure(event: AVMusicEvent; timeStamp: PAVMusicTimeStamp; removeEvent: PBoolean) of object;

  AVAudioSinkNodeReceiverBlock = function(timestamp: PAudioTimeStamp; frameCount: AVAudioFrameCount; inputData: PAudioBufferList): OSStatus of object;

  AVAudioSourceNodeRenderBlock = function(isSilence: PBoolean; timestamp: PAudioTimeStamp; frameCount: AVAudioFrameCount;
    outputData: PAudioBufferList): OSStatus of object;
  AVAudioUnitDistortionPreset = NSInteger;

  AVMIDIPlayerCompletionHandler = procedure of object;
  AVMIDIControlChangeMessageType = NSInteger;
  AVMIDIMetaEventType = NSInteger;
  AVSpeechBoundary = NSInteger;
  AVSpeechSynthesisVoiceQuality = NSInteger;
  AVSpeechSynthesisVoiceGender = NSInteger;
  AVSpeechSynthesisMarkerMark = NSInteger;

  AVSpeechSynthesizerBufferCallback = procedure(buffer: AVAudioBuffer) of object;

  AVSpeechSynthesizerMarkerCallback = procedure(markers: NSArray) of object;
  AVSpeechSynthesisPersonalVoiceAuthorizationStatus = NSInteger;
  AVSpeechSynthesisVoiceTraits = NSInteger;
  AVAudioApplicationRecordPermission = NSInteger;
  TAVAudioPCMBufferBlockMethod1 = procedure(param1: PAudioBufferList) of object;
  TAVAudioInputNodeBlockMethod1 = procedure(event: AVAudioVoiceProcessingSpeechActivityEvent) of object;
  TAVAudioSessionBlockMethod1 = procedure(granted: Boolean) of object;
  TAVAudioSessionBlockMethod2 = procedure(activated: Boolean; error: NSError) of object;
  TAVAudioRoutingArbiterBlockMethod1 = procedure(defaultDeviceChanged: Boolean; error: NSError) of object;
  TAVAudioUnitComponentManagerBlockMethod1 = procedure(comp: AVAudioUnitComponent; stop: PBoolean) of object;
  TAVSpeechSynthesizerBlockMethod1 = procedure(status: AVSpeechSynthesisPersonalVoiceAuthorizationStatus) of object;
  TAVAudioApplicationBlockMethod1 = procedure(inputShouldBeMuted: Boolean) of object;
  TAVAudioApplicationBlockMethod2 = procedure(granted: Boolean) of object;

  AVAudioBufferClass = interface(NSObjectClass)
    ['{6D04EB89-D9C8-482E-9227-E297AC7DF64E}']
  end;

  AVAudioBuffer = interface(NSObject)
    ['{BF9E4C36-6B5C-41D3-869E-C088D279788F}']
    function audioBufferList: PAudioBufferList; cdecl;
    function format: AVAudioFormat; cdecl;
    function mutableAudioBufferList: PAudioBufferList; cdecl;
  end;
  TAVAudioBuffer = class(TOCGenericImport<AVAudioBufferClass, AVAudioBuffer>) end;

  AVAudioPCMBufferClass = interface(AVAudioBufferClass)
    ['{9A7B4E9D-76EC-41BA-854D-A9E4D88B4EB4}']
  end;

  AVAudioPCMBuffer = interface(AVAudioBuffer)
    ['{567DA4F5-A65D-49FA-8240-3B6BBDEBA75E}']
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
    ['{B8C16846-0BB9-4F40-B758-6C92106843C6}']
  end;

  AVAudioCompressedBuffer = interface(AVAudioBuffer)
    ['{E20940B1-A6F9-42FA-83EB-F156814D6F66}']
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
    ['{29B5A674-80B2-40A7-AD07-254209C38F93}']
    {class} function layoutWithLayout(layout: PAudioChannelLayout): Pointer; cdecl;
    {class} function layoutWithLayoutTag(layoutTag: AudioChannelLayoutTag): Pointer; cdecl;
  end;

  AVAudioChannelLayout = interface(NSObject)
    ['{94DD6023-D04F-41C4-A12C-DA2B2B0A6C9D}']
    function channelCount: AVAudioChannelCount; cdecl;
    function initWithLayout(layout: PAudioChannelLayout): Pointer; cdecl;
    function initWithLayoutTag(layoutTag: AudioChannelLayoutTag): Pointer; cdecl;
    function isEqual(&object: Pointer): Boolean; cdecl;
    function layout: PAudioChannelLayout; cdecl;
    function layoutTag: AudioChannelLayoutTag; cdecl;
  end;
  TAVAudioChannelLayout = class(TOCGenericImport<AVAudioChannelLayoutClass, AVAudioChannelLayout>) end;

  AVAudioConnectionPointClass = interface(NSObjectClass)
    ['{C82C9585-CDB2-4DAD-8B76-3EB4C128D6D3}']
  end;

  AVAudioConnectionPoint = interface(NSObject)
    ['{9BA0E864-663F-4CE8-BF72-57ABB8F91CA1}']
    function bus: AVAudioNodeBus; cdecl;
    function initWithNode(node: AVAudioNode; bus: AVAudioNodeBus): Pointer; cdecl;
    function node: AVAudioNode; cdecl;
  end;
  TAVAudioConnectionPoint = class(TOCGenericImport<AVAudioConnectionPointClass, AVAudioConnectionPoint>) end;

  AVAudioFormatClass = interface(NSObjectClass)
    ['{4D601F54-B15E-47B3-95DA-787F5854FF0F}']
  end;

  AVAudioFormat = interface(NSObject)
    ['{40E7BB3F-6339-4390-94E4-4039F83C3854}']
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
    ['{0C74CB70-A0A2-4610-9CD3-A96AC0910360}']
  end;

  AVAudioConverter = interface(NSObject)
    ['{2716AD48-36E8-42D6-BCCF-096B387F90EC}']
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
    ['{07EA192A-5DE9-4EF5-97AA-618634D20449}']
  end;

  AVAudioNode = interface(NSObject)
    ['{D280DF6C-CC53-4D56-8727-FA1AE859E766}']
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
    ['{23591DF3-FEA2-4489-8734-2A0E63D50F23}']
    function destinationForMixer(mixer: AVAudioNode; bus: AVAudioNodeBus): AVAudioMixingDestination; cdecl;
    procedure setVolume(volume: Single); cdecl;
    function volume: Single; cdecl;
  end;

  AVAudioStereoMixing = interface(IObjectiveC)
    ['{40A77FD5-8365-48C4-9127-F142176BBB4C}']
    function pan: Single; cdecl;
    procedure setPan(pan: Single); cdecl;
  end;

  AVAudio3DMixing = interface(IObjectiveC)
    ['{116CAFEC-7D8B-40E9-BDD5-89A2EB11F33F}']
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
    ['{E0E4B6BE-03F8-4E27-AD25-3BDC9E174612}']
  end;

  AVAudioMixingDestination = interface(NSObject)
    ['{0B826129-985D-4C48-B2CB-B5F243865AFA}']
    function connectionPoint: AVAudioConnectionPoint; cdecl;
  end;
  TAVAudioMixingDestination = class(TOCGenericImport<AVAudioMixingDestinationClass, AVAudioMixingDestination>) end;

  AVAudioIONodeClass = interface(AVAudioNodeClass)
    ['{085E75DE-A270-406E-86B5-45B4544FCE0A}']
  end;

  AVAudioIONode = interface(AVAudioNode)
    ['{3DD21D29-4BA4-409B-B8F7-D50324D89C77}']
    function isVoiceProcessingEnabled: Boolean; cdecl;
    function presentationLatency: NSTimeInterval; cdecl;
    function setVoiceProcessingEnabled(enabled: Boolean; error: PPointer): Boolean; cdecl;
  end;
  TAVAudioIONode = class(TOCGenericImport<AVAudioIONodeClass, AVAudioIONode>) end;

  AVAudioInputNodeClass = interface(AVAudioIONodeClass)
    ['{05283AFE-18B1-4462-AB54-02E78C7EF476}']
  end;

  AVAudioInputNode = interface(AVAudioIONode)
    ['{7C09DE18-497A-444E-9AE1-847960B7A92F}']
    function isVoiceProcessingAGCEnabled: Boolean; cdecl;
    function isVoiceProcessingBypassed: Boolean; cdecl;
    function isVoiceProcessingInputMuted: Boolean; cdecl;
    function setManualRenderingInputPCMFormat(format: AVAudioFormat; inputBlock: AVAudioIONodeInputBlock): Boolean; cdecl;
    function setMutedSpeechActivityEventListener(listenerBlock: TAVAudioInputNodeBlockMethod1): Boolean; cdecl;
    procedure setVoiceProcessingAGCEnabled(voiceProcessingAGCEnabled: Boolean); cdecl;
    procedure setVoiceProcessingBypassed(voiceProcessingBypassed: Boolean); cdecl;
    procedure setVoiceProcessingInputMuted(voiceProcessingInputMuted: Boolean); cdecl;
    procedure setVoiceProcessingOtherAudioDuckingConfiguration(voiceProcessingOtherAudioDuckingConfiguration: AVAudioVoiceProcessingOtherAudioDuckingConfiguration); cdecl;
    function voiceProcessingOtherAudioDuckingConfiguration: AVAudioVoiceProcessingOtherAudioDuckingConfiguration; cdecl;
  end;
  TAVAudioInputNode = class(TOCGenericImport<AVAudioInputNodeClass, AVAudioInputNode>) end;

  AVAudioOutputNodeClass = interface(AVAudioIONodeClass)
    ['{FA6A82C9-1585-42A6-97CB-8B82E9A50573}']
  end;

  AVAudioOutputNode = interface(AVAudioIONode)
    ['{54FB3B4F-A8B1-4034-A855-D51982BB0B37}']
  end;
  TAVAudioOutputNode = class(TOCGenericImport<AVAudioOutputNodeClass, AVAudioOutputNode>) end;

  AVAudioTimeClass = interface(NSObjectClass)
    ['{3A0CF05E-C842-4941-81C5-F182B8EE1D70}']
    {class} function hostTimeForSeconds(seconds: NSTimeInterval): UInt64; cdecl;
    {class} function secondsForHostTime(hostTime: UInt64): NSTimeInterval; cdecl;
    {class} function timeWithAudioTimeStamp(ts: PAudioTimeStamp; sampleRate: Double): Pointer; cdecl;
    {class} function timeWithHostTime(hostTime: UInt64; sampleTime: AVAudioFramePosition; atRate: Double): Pointer; overload; cdecl;
    {class} function timeWithHostTime(hostTime: UInt64): Pointer; overload; cdecl;
    {class} function timeWithSampleTime(sampleTime: AVAudioFramePosition; atRate: Double): Pointer; cdecl;
  end;

  AVAudioTime = interface(NSObject)
    ['{65DCC304-7E5D-447C-8A1E-73EB8196F6FB}']
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
    ['{40243F98-00AA-4DF1-9DB0-027A6F64B39B}']
  end;

  AVAudioEngine = interface(NSObject)
    ['{58F79F54-E03D-42E7-97E0-3A2D39D605EF}']
    function attachedNodes: NSSet; cdecl;
    procedure attachNode(node: AVAudioNode); cdecl;
    procedure connect(sourceNode: AVAudioNode; toConnectionPoints: NSArray; fromBus: AVAudioNodeBus; format: AVAudioFormat); overload; cdecl;
    procedure connect(node1: AVAudioNode; &to: AVAudioNode; format: AVAudioFormat); overload; cdecl;
    procedure connect(node1: AVAudioNode; &to: AVAudioNode; fromBus: AVAudioNodeBus; toBus: AVAudioNodeBus; format: AVAudioFormat); overload; cdecl;
    procedure detachNode(node: AVAudioNode); cdecl;
    procedure disableManualRenderingMode; cdecl;
    procedure disconnectNodeInput(node: AVAudioNode); overload; cdecl;
    procedure disconnectNodeInput(node: AVAudioNode; bus: AVAudioNodeBus); overload; cdecl;
    procedure disconnectNodeOutput(node: AVAudioNode; bus: AVAudioNodeBus); overload; cdecl;
    procedure disconnectNodeOutput(node: AVAudioNode); overload; cdecl;
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
    function outputConnectionPointsForNode(node: AVAudioNode; outputBus: AVAudioNodeBus): NSArray; cdecl;
    function outputNode: AVAudioOutputNode; cdecl;
    procedure pause; cdecl;
    procedure prepare; cdecl;
    function renderOffline(numberOfFrames: AVAudioFrameCount; toBuffer: AVAudioPCMBuffer; error: PPointer): AVAudioEngineManualRenderingStatus; cdecl;
    procedure reset; cdecl;
    procedure setAutoShutdownEnabled(autoShutdownEnabled: Boolean); cdecl;
    function startAndReturnError(outError: PPointer): Boolean; cdecl;
    procedure stop; cdecl;
  end;
  TAVAudioEngine = class(TOCGenericImport<AVAudioEngineClass, AVAudioEngine>) end;

  AVAudioUnitClass = interface(AVAudioNodeClass)
    ['{EEE60B78-4BF7-4C20-9F9B-96B5766F145C}']
  end;

  AVAudioUnit = interface(AVAudioNode)
    ['{43D67219-541D-4C89-981A-D04965C97E83}']
  end;
  TAVAudioUnit = class(TOCGenericImport<AVAudioUnitClass, AVAudioUnit>) end;

  AVAudioUnitEffectClass = interface(AVAudioUnitClass)
    ['{7CD5242B-6956-40F0-9519-E1C74D71BDB2}']
  end;

  AVAudioUnitEffect = interface(AVAudioUnit)
    ['{B33A0FB1-F82D-433E-A438-20A6747E053E}']
    function bypass: Boolean; cdecl;
    procedure setBypass(bypass: Boolean); cdecl;
  end;
  TAVAudioUnitEffect = class(TOCGenericImport<AVAudioUnitEffectClass, AVAudioUnitEffect>) end;

  AVAudioUnitReverbClass = interface(AVAudioUnitEffectClass)
    ['{DB749B35-C43F-4FCC-9448-B26A8436CC81}']
  end;

  AVAudioUnitReverb = interface(AVAudioUnitEffect)
    ['{CAC72551-0F75-488E-BE84-AE947F3FC56B}']
    procedure loadFactoryPreset(preset: AVAudioUnitReverbPreset); cdecl;
    procedure setWetDryMix(wetDryMix: Single); cdecl;
    function wetDryMix: Single; cdecl;
  end;
  TAVAudioUnitReverb = class(TOCGenericImport<AVAudioUnitReverbClass, AVAudioUnitReverb>) end;

  AVAudioUnitEQFilterParametersClass = interface(NSObjectClass)
    ['{CB12ABFE-ADF9-4E82-A26B-DA06E2EB5779}']
  end;

  AVAudioUnitEQFilterParameters = interface(NSObject)
    ['{CA453BBF-D175-4168-9F1F-F9793F714FB4}']
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
    ['{F967322C-7EF6-4473-BD43-5A97E4E1E27E}']
  end;

  AVAudioUnitEQ = interface(AVAudioUnitEffect)
    ['{11CF6308-DFF0-4BF7-BD46-930438AB414F}']
    function bands: NSArray; cdecl;
    function globalGain: Single; cdecl;
    function initWithNumberOfBands(numberOfBands: NSUInteger): Pointer; cdecl;
    procedure setGlobalGain(globalGain: Single); cdecl;
  end;
  TAVAudioUnitEQ = class(TOCGenericImport<AVAudioUnitEQClass, AVAudioUnitEQ>) end;

  AVAudioEnvironmentDistanceAttenuationParametersClass = interface(NSObjectClass)
    ['{ED64F98C-4D52-4BCB-8F27-B49EBA27708F}']
  end;

  AVAudioEnvironmentDistanceAttenuationParameters = interface(NSObject)
    ['{DBCF83F2-0C25-417E-A15A-25D1AF06E210}']
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
    ['{3CD57AAF-74A8-40E9-820B-0EDAAF97E46E}']
  end;

  AVAudioEnvironmentReverbParameters = interface(NSObject)
    ['{36CBC040-545B-40DE-A632-0AB1166D3A8B}']
    function enable: Boolean; cdecl;
    function filterParameters: AVAudioUnitEQFilterParameters; cdecl;
    function level: Single; cdecl;
    procedure loadFactoryReverbPreset(preset: AVAudioUnitReverbPreset); cdecl;
    procedure setEnable(enable: Boolean); cdecl;
    procedure setLevel(level: Single); cdecl;
  end;
  TAVAudioEnvironmentReverbParameters = class(TOCGenericImport<AVAudioEnvironmentReverbParametersClass,
    AVAudioEnvironmentReverbParameters>) end;

  AVAudioEnvironmentNodeClass = interface(AVAudioNodeClass)
    ['{CB6FDADA-0229-44BE-A689-1AA1215FDAE6}']
  end;

  AVAudioEnvironmentNode = interface(AVAudioNode)
    ['{CAD088AB-3161-4BE7-A06B-540F128DFCC0}']
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
    ['{BB449003-9CF5-4D76-9B72-B7B4BE090617}']
  end;

  AVAudioFile = interface(NSObject)
    ['{38A88838-66B4-4C51-8F6F-72AAEDEDF328}']
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
    ['{BE989F69-0D32-495B-A76B-5D4F39C0AC1F}']
  end;

  AVAudioMixerNode = interface(AVAudioNode)
    ['{904284FE-59F1-4053-85D3-0BD27DA09CE5}']
    function nextAvailableInputBus: AVAudioNodeBus; cdecl;
    function outputVolume: Single; cdecl;
    procedure setOutputVolume(outputVolume: Single); cdecl;
  end;
  TAVAudioMixerNode = class(TOCGenericImport<AVAudioMixerNodeClass, AVAudioMixerNode>) end;

  AVAudioSessionChannelDescriptionClass = interface(NSObjectClass)
    ['{BC2A6847-E3EF-46FB-B07F-645B05A71EB7}']
  end;

  AVAudioSessionChannelDescription = interface(NSObject)
    ['{AD2A91ED-4640-4942-A2E0-570FFAA18AD6}']
    function channelLabel: AudioChannelLabel; cdecl;
    function channelName: NSString; cdecl;
    function channelNumber: NSUInteger; cdecl;
    function owningPortUID: NSString; cdecl;
  end;
  TAVAudioSessionChannelDescription = class(TOCGenericImport<AVAudioSessionChannelDescriptionClass, AVAudioSessionChannelDescription>) end;

  AVAudioSessionDataSourceDescriptionClass = interface(NSObjectClass)
    ['{39B18BE2-76CE-4BC2-BB7C-FB0AEB4939EC}']
  end;

  AVAudioSessionDataSourceDescription = interface(NSObject)
    ['{12932FB5-9C9E-4F6A-A777-362B922668B7}']
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
    ['{8CD6F645-3120-4968-9FCA-29D1C99F7A6F}']
  end;

  AVAudioSessionPortDescription = interface(NSObject)
    ['{01621110-096D-484C-AAD3-41D7FD933652}']
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
    ['{564665B3-567E-4417-937F-39CD61ABFE30}']
  end;

  AVAudioSessionRouteDescription = interface(NSObject)
    ['{5A1BFF74-6520-4AF0-9652-019BA8024CDC}']
    function inputs: NSArray; cdecl;
    function outputs: NSArray; cdecl;
  end;
  TAVAudioSessionRouteDescription = class(TOCGenericImport<AVAudioSessionRouteDescriptionClass, AVAudioSessionRouteDescription>) end;

  AVAudioSessionClass = interface(NSObjectClass)
    ['{378CD164-91B5-4D59-987C-B91EF19745BD}']
    {class} function sharedInstance: AVAudioSession; cdecl;
  end;

  AVAudioSession = interface(NSObject)
    ['{B7339F5A-5F4D-4881-AD5E-767E13B3E4D1}']
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
    function prefersInterruptionOnRouteDisconnect: Boolean; cdecl;
    function prefersNoInterruptionsFromSystemAlerts: Boolean; cdecl;
    function promptStyle: AVAudioSessionPromptStyle; cdecl;
    function recordPermission: AVAudioSessionRecordPermission; cdecl; // API_DEPRECATED("Please use AVAudioApplication recordPermission", ios(8.0, 17.0), watchos(4.0, 10.0))
    function renderingMode: AVAudioSessionRenderingMode; cdecl;
    procedure requestRecordPermission(response: TAVAudioSessionBlockMethod1); cdecl; // API_DEPRECATED("Please use AVAudioApplication requestRecordPermissionWithCompletionHandler", ios(7.0, 17.0), watchos(4.0, 10.0))
    function routeSharingPolicy: AVAudioSessionRouteSharingPolicy; cdecl;
    function sampleRate: Double; cdecl;
    function secondaryAudioShouldBeSilencedHint: Boolean; cdecl;
    function setActive(active: Boolean; withFlags: NSInteger; error: PPointer): Boolean; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-setActive:withOptions:error:", ios(4.0, 6.0), macCatalyst(14.0, 14.0))
    function setActive(active: Boolean; error: PPointer): Boolean; overload; cdecl;
    [MethodName('setActive:withOptions:error:')]
    function setActiveWithOptions(active: Boolean; withOptions: AVAudioSessionSetActiveOptions; error: PPointer): Boolean; cdecl;
    function setAggregatedIOPreference(inIOType: AVAudioSessionIOType; error: PPointer): Boolean; cdecl;
    function setAllowHapticsAndSystemSoundsDuringRecording(inValue: Boolean; error: PPointer): Boolean; cdecl;
    function setCategory(category: AVAudioSessionCategory; mode: AVAudioSessionMode; options: AVAudioSessionCategoryOptions;
      error: PPointer): Boolean; overload; cdecl;
    function setCategory(category: AVAudioSessionCategory; mode: AVAudioSessionMode; routeSharingPolicy: AVAudioSessionRouteSharingPolicy;
      options: AVAudioSessionCategoryOptions; error: PPointer): Boolean; overload; cdecl;
    function setCategory(category: AVAudioSessionCategory; withOptions: AVAudioSessionCategoryOptions; error: PPointer): Boolean; overload; cdecl;
    function setCategory(category: AVAudioSessionCategory; error: PPointer): Boolean; overload; cdecl;
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
    function setPrefersInterruptionOnRouteDisconnect(inValue: Boolean; error: PPointer): Boolean; cdecl;
    function setPrefersNoInterruptionsFromSystemAlerts(inValue: Boolean; error: PPointer): Boolean; cdecl;
    function setSupportsMultichannelContent(inValue: Boolean; error: PPointer): Boolean; cdecl;
    function supportedOutputChannelLayouts: NSArray; cdecl;
    function supportsMultichannelContent: Boolean; cdecl;
  end;
  TAVAudioSession = class(TOCGenericImport<AVAudioSessionClass, AVAudioSession>) end;

  AVAudioSessionDelegate = interface(IObjectiveC)
    ['{688B3938-E9B4-4FA6-BDA7-01A0CE835AAB}']
    procedure beginInterruption; cdecl;
    procedure endInterruption; cdecl;
    procedure endInterruptionWithFlags(flags: NSUInteger); cdecl;
    procedure inputIsAvailableChanged(isInputAvailable: Boolean); cdecl;
  end;

  AVAudioPlayerClass = interface(NSObjectClass)
    ['{1615AD29-36DC-42D7-AC1D-523E9F5E9D3E}']
  end;

  AVAudioPlayer = interface(NSObject)
    ['{C5154118-CBB4-47A1-811C-50CFEE24ACCE}']
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
    ['{B60F7347-2561-4CFD-92D5-6C4FDFA1EE70}']
    procedure audioPlayerDecodeErrorDidOccur(player: AVAudioPlayer; error: NSError); cdecl;
    procedure audioPlayerDidFinishPlaying(player: AVAudioPlayer; successfully: Boolean); cdecl;
  end;

  AVAudioPlayerNodeClass = interface(AVAudioNodeClass)
    ['{F00F123D-2348-4C6F-9DE4-112A4B7EFB57}']
  end;

  AVAudioPlayerNode = interface(AVAudioNode)
    ['{ADC96595-71B2-4F94-97AA-FAD7F0902887}']
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
    ['{EBEC62CD-C4A5-4A22-95A3-D0DAFB12C8F8}']
  end;

  AVAudioRecorder = interface(NSObject)
    ['{42828030-8B97-4A77-AD43-A385FA21E91C}']
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
    ['{549B0B38-10D6-498B-B85D-1663839528EB}']
    procedure audioRecorderDidFinishRecording(recorder: AVAudioRecorder; successfully: Boolean); cdecl;
    procedure audioRecorderEncodeErrorDidOccur(recorder: AVAudioRecorder; error: NSError); cdecl;
  end;

  AVAudioRoutingArbiterClass = interface(NSObjectClass)
    ['{D938C13D-2887-4667-8532-DE1702A594D1}']
    {class} function new: Pointer; cdecl;
    {class} function sharedRoutingArbiter: AVAudioRoutingArbiter; cdecl;
  end;

  AVAudioRoutingArbiter = interface(NSObject)
    ['{5E5AD74F-7280-48EC-B04B-25148486AAAA}']
    procedure beginArbitrationWithCategory(category: AVAudioRoutingArbitrationCategory; completionHandler: TAVAudioRoutingArbiterBlockMethod1); cdecl;
    procedure leaveArbitration; cdecl;
  end;
  TAVAudioRoutingArbiter = class(TOCGenericImport<AVAudioRoutingArbiterClass, AVAudioRoutingArbiter>) end;

  AVAudioSequencerClass = interface(NSObjectClass)
    ['{800B5106-6E41-4220-8DD3-439320E8C1C2}']
  end;

  AVAudioSequencer = interface(NSObject)
    ['{130C841E-C333-42B8-9AFA-0C8F9920D2BD}']
    function beatsForHostTime(inHostTime: UInt64; error: PPointer): AVMusicTimeStamp; cdecl;
    function beatsForSeconds(seconds: NSTimeInterval): AVMusicTimeStamp; cdecl;
    function createAndAppendTrack: AVMusicTrack; cdecl;
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
    function removeTrack(track: AVMusicTrack): Boolean; cdecl;
    procedure reverseEvents; cdecl;
    function secondsForBeats(beats: AVMusicTimeStamp): NSTimeInterval; cdecl;
    procedure setCurrentPositionInBeats(currentPositionInBeats: NSTimeInterval); cdecl;
    procedure setCurrentPositionInSeconds(currentPositionInSeconds: NSTimeInterval); cdecl;
    procedure setRate(rate: Single); cdecl;
    procedure setUserCallback(userCallback: AVAudioSequencerUserCallback); cdecl;
    function startAndReturnError(outError: PPointer): Boolean; cdecl;
    procedure stop; cdecl;
    function tempoTrack: AVMusicTrack; cdecl;
    function tracks: NSArray; cdecl;
    function userInfo: NSDictionary; cdecl;
    function writeToURL(fileURL: NSURL; SMPTEResolution: NSInteger; replaceExisting: Boolean; error: PPointer): Boolean; cdecl;
  end;
  TAVAudioSequencer = class(TOCGenericImport<AVAudioSequencerClass, AVAudioSequencer>) end;

  AVMusicTrackClass = interface(NSObjectClass)
    ['{378D8C2C-28CA-445A-828D-D1573FBBD66E}']
  end;

  AVMusicTrack = interface(NSObject)
    ['{AACD6AB8-1A6A-405F-8920-B33C88CC7FA0}']
    procedure addEvent(event: AVMusicEvent; atBeat: AVMusicTimeStamp); cdecl;
    procedure clearEventsInRange(range: AVBeatRange); cdecl;
    procedure copyAndMergeEventsInRange(range: AVBeatRange; fromTrack: AVMusicTrack; mergeAtBeat: AVMusicTimeStamp); cdecl;
    procedure copyEventsInRange(range: AVBeatRange; fromTrack: AVMusicTrack; insertAtBeat: AVMusicTimeStamp); cdecl;
    procedure cutEventsInRange(range: AVBeatRange); cdecl;
    function destinationAudioUnit: AVAudioUnit; cdecl;
    procedure enumerateEventsInRange(range: AVBeatRange; usingBlock: AVMusicEventEnumerationBlock); cdecl;
    function isLoopingEnabled: Boolean; cdecl;
    function isMuted: Boolean; cdecl;
    function isSoloed: Boolean; cdecl;
    function lengthInBeats: AVMusicTimeStamp; cdecl;
    function lengthInSeconds: NSTimeInterval; cdecl;
    function loopRange: AVBeatRange; cdecl;
    procedure moveEventsInRange(range: AVBeatRange; byAmount: AVMusicTimeStamp); cdecl;
    function numberOfLoops: NSInteger; cdecl;
    function offsetTime: AVMusicTimeStamp; cdecl;
    procedure setDestinationAudioUnit(destinationAudioUnit: AVAudioUnit); cdecl;
    procedure setLengthInBeats(lengthInBeats: AVMusicTimeStamp); cdecl;
    procedure setLengthInSeconds(lengthInSeconds: NSTimeInterval); cdecl;
    procedure setLoopingEnabled(loopingEnabled: Boolean); cdecl;
    procedure setLoopRange(loopRange: AVBeatRange); cdecl;
    procedure setMuted(muted: Boolean); cdecl;
    procedure setNumberOfLoops(numberOfLoops: NSInteger); cdecl;
    procedure setOffsetTime(offsetTime: AVMusicTimeStamp); cdecl;
    procedure setSoloed(soloed: Boolean); cdecl;
    procedure setUsesAutomatedParameters(usesAutomatedParameters: Boolean); cdecl;
    function timeResolution: NSUInteger; cdecl;
    function usesAutomatedParameters: Boolean; cdecl;
  end;
  TAVMusicTrack = class(TOCGenericImport<AVMusicTrackClass, AVMusicTrack>) end;

  AVAudioSinkNodeClass = interface(AVAudioNodeClass)
    ['{5899AB86-5E19-43DE-9AFC-42CF07F02901}']
  end;

  AVAudioSinkNode = interface(AVAudioNode)
    ['{5A78A3B7-8E5E-41CF-AF99-B7A5EC774B82}']
    function initWithReceiverBlock(block: AVAudioSinkNodeReceiverBlock): Pointer; cdecl;
  end;
  TAVAudioSinkNode = class(TOCGenericImport<AVAudioSinkNodeClass, AVAudioSinkNode>) end;

  AVAudioSourceNodeClass = interface(AVAudioNodeClass)
    ['{ECD0DD89-B095-4CC7-9749-E4353BE3CA77}']
  end;

  AVAudioSourceNode = interface(AVAudioNode)
    ['{522AE60C-9501-488A-BCED-0F205278F326}']
    function initWithFormat(format: AVAudioFormat; renderBlock: AVAudioSourceNodeRenderBlock): Pointer; cdecl;
    function initWithRenderBlock(block: AVAudioSourceNodeRenderBlock): Pointer; cdecl;
  end;
  TAVAudioSourceNode = class(TOCGenericImport<AVAudioSourceNodeClass, AVAudioSourceNode>) end;

  AVAudioUnitComponentClass = interface(NSObjectClass)
    ['{92E45DE8-63F1-4FEB-BE8E-34F72A83C9B0}']
  end;

  AVAudioUnitComponent = interface(NSObject)
    ['{93DC972A-8863-446F-BB6F-D4D4FEBA4859}']
    function allTagNames: NSArray; cdecl;
    function availableArchitectures: NSArray; cdecl;
    function componentURL: NSURL; cdecl;
    function configurationDictionary: NSDictionary; cdecl;
    function hasCustomView: Boolean; cdecl;
    function hasMIDIInput: Boolean; cdecl;
    function hasMIDIOutput: Boolean; cdecl;
    function icon: NSImage; cdecl;
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
    ['{0E0C3D94-CCCC-404D-9D10-6B1305993329}']
    {class} function sharedAudioUnitComponentManager: Pointer; cdecl;
  end;

  AVAudioUnitComponentManager = interface(NSObject)
    ['{DF433901-072D-4523-BF53-FDC716AEB354}']
    function componentsMatchingPredicate(predicate: Pointer): NSArray; cdecl;
    function componentsPassingTest(testHandler: TAVAudioUnitComponentManagerBlockMethod1): NSArray; cdecl;
    function standardLocalizedTagNames: NSArray; cdecl;
    function tagNames: NSArray; cdecl;
  end;
  TAVAudioUnitComponentManager = class(TOCGenericImport<AVAudioUnitComponentManagerClass, AVAudioUnitComponentManager>) end;

  AVAudioUnitDelayClass = interface(AVAudioUnitEffectClass)
    ['{6451C820-145E-4A2E-A0C5-D22FA85F3013}']
  end;

  AVAudioUnitDelay = interface(AVAudioUnitEffect)
    ['{EFB6DCC5-EA5A-46B5-8743-940F5F5185B3}']
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
    ['{95C88B5A-FEFA-426E-86EE-74123A47EE27}']
  end;

  AVAudioUnitDistortion = interface(AVAudioUnitEffect)
    ['{FAB2CB10-F904-4742-8A7B-78EB58FBC8C2}']
    procedure loadFactoryPreset(preset: AVAudioUnitDistortionPreset); cdecl;
    function preGain: Single; cdecl;
    procedure setPreGain(preGain: Single); cdecl;
    procedure setWetDryMix(wetDryMix: Single); cdecl;
    function wetDryMix: Single; cdecl;
  end;
  TAVAudioUnitDistortion = class(TOCGenericImport<AVAudioUnitDistortionClass, AVAudioUnitDistortion>) end;

  AVAudioUnitGeneratorClass = interface(AVAudioUnitClass)
    ['{A6D2F28E-ACD4-4486-8546-84CC2D49D68E}']
  end;

  AVAudioUnitGenerator = interface(AVAudioUnit)
    ['{999E34BB-618C-4ECE-9D55-0E493B5A719A}']
    function bypass: Boolean; cdecl;
    procedure setBypass(bypass: Boolean); cdecl;
  end;
  TAVAudioUnitGenerator = class(TOCGenericImport<AVAudioUnitGeneratorClass, AVAudioUnitGenerator>) end;

  AVAudioUnitMIDIInstrumentClass = interface(AVAudioUnitClass)
    ['{390E94AE-7B32-4816-874F-612AE707DCCF}']
  end;

  AVAudioUnitMIDIInstrument = interface(AVAudioUnit)
    ['{03C03DCC-EBCE-433F-94F2-262DCFEE3792}']
    procedure sendController(controller: UInt8; withValue: UInt8; onChannel: UInt8); cdecl;
    procedure sendMIDIEvent(midiStatus: UInt8; data1: UInt8); overload; cdecl;
    procedure sendMIDIEvent(midiStatus: UInt8; data1: UInt8; data2: UInt8); overload; cdecl;
    procedure sendMIDISysExEvent(midiData: NSData); cdecl;
    procedure sendPitchBend(pitchbend: UInt16; onChannel: UInt8); cdecl;
    procedure sendPressure(pressure: UInt8; onChannel: UInt8); cdecl;
    procedure sendPressureForKey(key: UInt8; withValue: UInt8; onChannel: UInt8); cdecl;
    procedure sendProgramChange(&program: UInt8; onChannel: UInt8); overload; cdecl;
    procedure sendProgramChange(&program: UInt8; bankMSB: UInt8; bankLSB: UInt8; onChannel: UInt8); overload; cdecl;
    procedure startNote(note: UInt8; withVelocity: UInt8; onChannel: UInt8); cdecl;
    procedure stopNote(note: UInt8; onChannel: UInt8); cdecl;
  end;
  TAVAudioUnitMIDIInstrument = class(TOCGenericImport<AVAudioUnitMIDIInstrumentClass, AVAudioUnitMIDIInstrument>) end;

  AVAudioUnitSamplerClass = interface(AVAudioUnitMIDIInstrumentClass)
    ['{217A3532-F2B4-430F-94A4-1F842563D671}']
  end;

  AVAudioUnitSampler = interface(AVAudioUnitMIDIInstrument)
    ['{C4D46808-1483-4FC5-B809-F5A3FF8E0793}']
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
    ['{A62A6BB0-71FC-4B6C-8895-BD9352A90855}']
  end;

  AVAudioUnitTimeEffect = interface(AVAudioUnit)
    ['{9F0DEE47-9789-409D-ABC2-9764F516161A}']
    function bypass: Boolean; cdecl;
    procedure setBypass(bypass: Boolean); cdecl;
  end;
  TAVAudioUnitTimeEffect = class(TOCGenericImport<AVAudioUnitTimeEffectClass, AVAudioUnitTimeEffect>) end;

  AVAudioUnitTimePitchClass = interface(AVAudioUnitTimeEffectClass)
    ['{4CEBB2DB-4641-4E45-A155-E10DC99B0319}']
  end;

  AVAudioUnitTimePitch = interface(AVAudioUnitTimeEffect)
    ['{9C44FC65-4FB1-44BD-8E15-7C68BD0D2B3B}']
    function overlap: Single; cdecl;
    function pitch: Single; cdecl;
    function rate: Single; cdecl;
    procedure setOverlap(overlap: Single); cdecl;
    procedure setPitch(pitch: Single); cdecl;
    procedure setRate(rate: Single); cdecl;
  end;
  TAVAudioUnitTimePitch = class(TOCGenericImport<AVAudioUnitTimePitchClass, AVAudioUnitTimePitch>) end;

  AVAudioUnitVarispeedClass = interface(AVAudioUnitTimeEffectClass)
    ['{0A619DCA-0347-4918-B988-366BFC624D5C}']
  end;

  AVAudioUnitVarispeed = interface(AVAudioUnitTimeEffect)
    ['{F7FFEF8D-E21D-4ACD-9E5E-1534450B500A}']
    function rate: Single; cdecl;
    procedure setRate(rate: Single); cdecl;
  end;
  TAVAudioUnitVarispeed = class(TOCGenericImport<AVAudioUnitVarispeedClass, AVAudioUnitVarispeed>) end;

  AVMIDIPlayerClass = interface(NSObjectClass)
    ['{4A72FAB9-B249-4FFB-8769-964F3DB05E16}']
  end;

  AVMIDIPlayer = interface(NSObject)
    ['{851704B9-41D6-49D3-82B0-3F76C084910B}']
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

  AVMusicEventClass = interface(NSObjectClass)
    ['{EBCE25A3-355F-482D-9791-2015B192A6C8}']
  end;

  AVMusicEvent = interface(NSObject)
    ['{53397100-A61F-4552-8EAD-0ED24166C9A6}']
  end;
  TAVMusicEvent = class(TOCGenericImport<AVMusicEventClass, AVMusicEvent>) end;

  AVMIDINoteEventClass = interface(AVMusicEventClass)
    ['{CFA36DF5-E175-4585-9574-30D969C317F5}']
  end;

  AVMIDINoteEvent = interface(AVMusicEvent)
    ['{D3E1504F-5666-40ED-A6B9-21F3F3E7841F}']
    function channel: UInt32; cdecl;
    function duration: AVMusicTimeStamp; cdecl;
    function initWithChannel(channel: UInt32; key: UInt32; velocity: UInt32; duration: AVMusicTimeStamp): Pointer; cdecl;
    function key: UInt32; cdecl;
    procedure setChannel(channel: UInt32); cdecl;
    procedure setDuration(duration: AVMusicTimeStamp); cdecl;
    procedure setKey(key: UInt32); cdecl;
    procedure setVelocity(velocity: UInt32); cdecl;
    function velocity: UInt32; cdecl;
  end;
  TAVMIDINoteEvent = class(TOCGenericImport<AVMIDINoteEventClass, AVMIDINoteEvent>) end;

  AVMIDIChannelEventClass = interface(AVMusicEventClass)
    ['{601795AF-BA74-4904-AE59-A3D6DD69BAA1}']
  end;

  AVMIDIChannelEvent = interface(AVMusicEvent)
    ['{AB34D4E6-A21C-49DB-8790-2DC1C814A5FA}']
    function channel: UInt32; cdecl;
    procedure setChannel(channel: UInt32); cdecl;
  end;
  TAVMIDIChannelEvent = class(TOCGenericImport<AVMIDIChannelEventClass, AVMIDIChannelEvent>) end;

  AVMIDIControlChangeEventClass = interface(AVMIDIChannelEventClass)
    ['{3DFCA979-454B-40DD-AEF9-4935C88F8318}']
  end;

  AVMIDIControlChangeEvent = interface(AVMIDIChannelEvent)
    ['{7409E1F8-B2C0-4B08-9D44-9F087FC71F6C}']
    function initWithChannel(channel: UInt32; messageType: AVMIDIControlChangeMessageType; value: UInt32): Pointer; cdecl;
    function messageType: AVMIDIControlChangeMessageType; cdecl;
    function value: UInt32; cdecl;
  end;
  TAVMIDIControlChangeEvent = class(TOCGenericImport<AVMIDIControlChangeEventClass, AVMIDIControlChangeEvent>) end;

  AVMIDIPolyPressureEventClass = interface(AVMIDIChannelEventClass)
    ['{B5315140-C286-49F7-A18A-84AB8A604F94}']
  end;

  AVMIDIPolyPressureEvent = interface(AVMIDIChannelEvent)
    ['{1684C4BF-F93D-4648-97A7-C62BC4FEE700}']
    function initWithChannel(channel: UInt32; key: UInt32; pressure: UInt32): Pointer; cdecl;
    function key: UInt32; cdecl;
    function pressure: UInt32; cdecl;
    procedure setKey(key: UInt32); cdecl;
    procedure setPressure(pressure: UInt32); cdecl;
  end;
  TAVMIDIPolyPressureEvent = class(TOCGenericImport<AVMIDIPolyPressureEventClass, AVMIDIPolyPressureEvent>) end;

  AVMIDIProgramChangeEventClass = interface(AVMIDIChannelEventClass)
    ['{D0FA2B04-47F1-4925-85A5-A147D048CEB7}']
  end;

  AVMIDIProgramChangeEvent = interface(AVMIDIChannelEvent)
    ['{7AD0BFBD-98D0-483F-8081-48253211FC1A}']
    function initWithChannel(channel: UInt32; programNumber: UInt32): Pointer; cdecl;
    function programNumber: UInt32; cdecl;
    procedure setProgramNumber(programNumber: UInt32); cdecl;
  end;
  TAVMIDIProgramChangeEvent = class(TOCGenericImport<AVMIDIProgramChangeEventClass, AVMIDIProgramChangeEvent>) end;

  AVMIDIChannelPressureEventClass = interface(AVMIDIChannelEventClass)
    ['{7702F622-2592-4203-A91C-D884A2922025}']
  end;

  AVMIDIChannelPressureEvent = interface(AVMIDIChannelEvent)
    ['{37CC1D38-64E8-4570-9016-76F560DF9017}']
    function initWithChannel(channel: UInt32; pressure: UInt32): Pointer; cdecl;
    function pressure: UInt32; cdecl;
    procedure setPressure(pressure: UInt32); cdecl;
  end;
  TAVMIDIChannelPressureEvent = class(TOCGenericImport<AVMIDIChannelPressureEventClass, AVMIDIChannelPressureEvent>) end;

  AVMIDIPitchBendEventClass = interface(AVMIDIChannelEventClass)
    ['{79B26EA0-4B31-433A-81C9-E18005AD723A}']
  end;

  AVMIDIPitchBendEvent = interface(AVMIDIChannelEvent)
    ['{60BD7210-3927-4E82-A8EF-79B713074A57}']
    function initWithChannel(channel: UInt32; value: UInt32): Pointer; cdecl;
    procedure setValue(value: UInt32); cdecl;
    function value: UInt32; cdecl;
  end;
  TAVMIDIPitchBendEvent = class(TOCGenericImport<AVMIDIPitchBendEventClass, AVMIDIPitchBendEvent>) end;

  AVMIDISysexEventClass = interface(AVMusicEventClass)
    ['{25DF4EA9-F109-4921-8AE4-8ACB9DECB71E}']
  end;

  AVMIDISysexEvent = interface(AVMusicEvent)
    ['{C746601C-6563-4D7A-891F-36658D5483F1}']
    function initWithData(data: NSData): Pointer; cdecl;
    function sizeInBytes: UInt32; cdecl;
  end;
  TAVMIDISysexEvent = class(TOCGenericImport<AVMIDISysexEventClass, AVMIDISysexEvent>) end;

  AVMIDIMetaEventClass = interface(AVMusicEventClass)
    ['{11042EBF-A57A-471A-8888-538283B73E0E}']
  end;

  AVMIDIMetaEvent = interface(AVMusicEvent)
    ['{B0EF0A81-2CF4-46DF-8F5E-25F69771C0DE}']
    function &type: AVMIDIMetaEventType; cdecl;
    function initWithType(&type: AVMIDIMetaEventType; data: NSData): Pointer; cdecl;
  end;
  TAVMIDIMetaEvent = class(TOCGenericImport<AVMIDIMetaEventClass, AVMIDIMetaEvent>) end;

  AVMusicUserEventClass = interface(AVMusicEventClass)
    ['{8F1303A3-18C7-414B-91FA-80996857ECFC}']
  end;

  AVMusicUserEvent = interface(AVMusicEvent)
    ['{8E8AFA80-BED8-4E98-9DE9-7D48643F413F}']
    function initWithData(data: NSData): Pointer; cdecl;
    function sizeInBytes: UInt32; cdecl;
  end;
  TAVMusicUserEvent = class(TOCGenericImport<AVMusicUserEventClass, AVMusicUserEvent>) end;

  AVExtendedNoteOnEventClass = interface(AVMusicEventClass)
    ['{7AFF9316-FC9C-4D08-982C-F718AF698927}']
  end;

  AVExtendedNoteOnEvent = interface(AVMusicEvent)
    ['{5D428F18-492B-4125-8955-74304C913D0A}']
    function duration: AVMusicTimeStamp; cdecl;
    function groupID: UInt32; cdecl;
    function initWithMIDINote(midiNote: Single; velocity: Single; groupID: UInt32; duration: AVMusicTimeStamp): Pointer; overload; cdecl;
    function initWithMIDINote(midiNote: Single; velocity: Single; instrumentID: UInt32; groupID: UInt32;
      duration: AVMusicTimeStamp): Pointer; overload; cdecl;
    function instrumentID: UInt32; cdecl;
    function midiNote: Single; cdecl;
    procedure setDuration(duration: AVMusicTimeStamp); cdecl;
    procedure setGroupID(groupID: UInt32); cdecl;
    procedure setInstrumentID(instrumentID: UInt32); cdecl;
    procedure setMidiNote(midiNote: Single); cdecl;
    procedure setVelocity(velocity: Single); cdecl;
    function velocity: Single; cdecl;
  end;
  TAVExtendedNoteOnEvent = class(TOCGenericImport<AVExtendedNoteOnEventClass, AVExtendedNoteOnEvent>) end;

  AVParameterEventClass = interface(AVMusicEventClass)
    ['{B963F56C-A2E5-4AA5-AD1B-9E1727A7EDDD}']
  end;

  AVParameterEvent = interface(AVMusicEvent)
    ['{0672AC10-7ADB-4572-B9DB-F9EBA396AAEA}']
    function element: UInt32; cdecl;
    function initWithParameterID(parameterID: UInt32; scope: UInt32; element: UInt32; value: Single): Pointer; cdecl;
    function parameterID: UInt32; cdecl;
    function scope: UInt32; cdecl;
    procedure setElement(element: UInt32); cdecl;
    procedure setParameterID(parameterID: UInt32); cdecl;
    procedure setScope(scope: UInt32); cdecl;
    procedure setValue(value: Single); cdecl;
    function value: Single; cdecl;
  end;
  TAVParameterEvent = class(TOCGenericImport<AVParameterEventClass, AVParameterEvent>) end;

  AVAUPresetEventClass = interface(AVMusicEventClass)
    ['{22F48F49-2E39-4FE3-AA16-B68F8DC64ED4}']
  end;

  AVAUPresetEvent = interface(AVMusicEvent)
    ['{6CA7D15B-E268-45CB-A1C8-A32E2BACA22D}']
    function element: UInt32; cdecl;
    function initWithScope(scope: UInt32; element: UInt32; dictionary: NSDictionary): Pointer; cdecl;
    function presetDictionary: NSDictionary; cdecl;
    function scope: UInt32; cdecl;
    procedure setElement(element: UInt32); cdecl;
    procedure setScope(scope: UInt32); cdecl;
  end;
  TAVAUPresetEvent = class(TOCGenericImport<AVAUPresetEventClass, AVAUPresetEvent>) end;

  AVExtendedTempoEventClass = interface(AVMusicEventClass)
    ['{D0466DD4-F675-4C64-94DF-D60DFD832EE6}']
  end;

  AVExtendedTempoEvent = interface(AVMusicEvent)
    ['{2CBCA744-3A17-4B65-AE6D-67D2041022E4}']
    function initWithTempo(tempo: Double): Pointer; cdecl;
    procedure setTempo(tempo: Double); cdecl;
    function tempo: Double; cdecl;
  end;
  TAVExtendedTempoEvent = class(TOCGenericImport<AVExtendedTempoEventClass, AVExtendedTempoEvent>) end;

  AVSpeechSynthesisVoiceClass = interface(NSObjectClass)
    ['{51718BD8-02BC-40A7-AF5A-F372D3F323CC}']
    {class} function currentLanguageCode: NSString; cdecl;
    {class} function speechVoices: NSArray; cdecl;
    {class} function voiceWithIdentifier(identifier: NSString): AVSpeechSynthesisVoice; cdecl;
    {class} function voiceWithLanguage(languageCode: NSString): AVSpeechSynthesisVoice; cdecl;
  end;

  AVSpeechSynthesisVoice = interface(NSObject)
    ['{0DA449F5-5FC8-4DC2-9A79-FC2949D4CBE1}']
    function audioFileSettings: NSDictionary; cdecl;
    function gender: AVSpeechSynthesisVoiceGender; cdecl;
    function identifier: NSString; cdecl;
    function language: NSString; cdecl;
    function name: NSString; cdecl;
    function quality: AVSpeechSynthesisVoiceQuality; cdecl;
    function voiceTraits: AVSpeechSynthesisVoiceTraits; cdecl;
  end;
  TAVSpeechSynthesisVoice = class(TOCGenericImport<AVSpeechSynthesisVoiceClass, AVSpeechSynthesisVoice>) end;

  AVSpeechUtteranceClass = interface(NSObjectClass)
    ['{49479EC7-BFD6-428D-8B58-DF1B710AC5F3}']
    {class} function speechUtteranceWithAttributedString(&string: NSAttributedString): Pointer; cdecl;
    {class} function speechUtteranceWithSSMLRepresentation(&string: NSString): Pointer; cdecl;
    {class} function speechUtteranceWithString(&string: NSString): Pointer; cdecl;
  end;

  AVSpeechUtterance = interface(NSObject)
    ['{2B75137B-1904-48B2-83BE-9826C7FFD04E}']
    function attributedSpeechString: NSAttributedString; cdecl;
    function initWithAttributedString(&string: NSAttributedString): Pointer; cdecl;
    function initWithSSMLRepresentation(&string: NSString): Pointer; cdecl;
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
    ['{431911C4-8CB6-467E-A0FB-7120587C7D4A}']
    {class} function personalVoiceAuthorizationStatus: AVSpeechSynthesisPersonalVoiceAuthorizationStatus; cdecl;
    {class} procedure requestPersonalVoiceAuthorizationWithCompletionHandler(handler: TAVSpeechSynthesizerBlockMethod1); cdecl;
  end;

  AVSpeechSynthesizer = interface(NSObject)
    ['{7E883743-CF3B-4821-9727-4351972EDFFB}']
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
    procedure writeUtterance(utterance: AVSpeechUtterance; toBufferCallback: AVSpeechSynthesizerBufferCallback); overload; cdecl;
    procedure writeUtterance(utterance: AVSpeechUtterance; toBufferCallback: AVSpeechSynthesizerBufferCallback;
      toMarkerCallback: AVSpeechSynthesizerMarkerCallback); overload; cdecl;
  end;
  TAVSpeechSynthesizer = class(TOCGenericImport<AVSpeechSynthesizerClass, AVSpeechSynthesizer>) end;

  AVSpeechSynthesizerDelegate = interface(IObjectiveC)
    ['{FC2F121C-9EB3-4B29-AA5E-B13A110C120C}']
    procedure speechSynthesizer(synthesizer: AVSpeechSynthesizer; willSpeakRangeOfSpeechString: NSRange;
      utterance: AVSpeechUtterance); overload; cdecl;
    procedure speechSynthesizer(synthesizer: AVSpeechSynthesizer; willSpeakMarker: AVSpeechSynthesisMarker;
      utterance: AVSpeechUtterance); overload; cdecl;
    procedure speechSynthesizer(synthesizer: AVSpeechSynthesizer; didStartSpeechUtterance: AVSpeechUtterance); overload; cdecl;
    [MethodName('speechSynthesizer:didCancelSpeechUtterance:')]
    procedure speechSynthesizerDidCancelSpeechUtterance(synthesizer: AVSpeechSynthesizer; didCancelSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didContinueSpeechUtterance:')]
    procedure speechSynthesizerDidContinueSpeechUtterance(synthesizer: AVSpeechSynthesizer; didContinueSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didFinishSpeechUtterance:')]
    procedure speechSynthesizerDidFinishSpeechUtterance(synthesizer: AVSpeechSynthesizer; didFinishSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didPauseSpeechUtterance:')]
    procedure speechSynthesizerDidPauseSpeechUtterance(synthesizer: AVSpeechSynthesizer; didPauseSpeechUtterance: AVSpeechUtterance); cdecl;
  end;

  AVSpeechSynthesisMarkerClass = interface(NSObjectClass)
    ['{70D40067-F690-49EA-890E-D1A589BAC6F4}']
  end;

  AVSpeechSynthesisMarker = interface(NSObject)
    ['{72D58657-C9A2-4A8F-B435-A71F616114D1}']
    function bookmarkName: NSString; cdecl;
    function byteSampleOffset: NSUInteger; cdecl;
    function initWithBookmarkName(mark: NSString; atByteSampleOffset: NSInteger): Pointer; cdecl;
    function initWithMarkerType(&type: AVSpeechSynthesisMarkerMark; forTextRange: NSRange; atByteSampleOffset: NSUInteger): Pointer; cdecl;
    function initWithParagraphRange(range: NSRange; atByteSampleOffset: NSInteger): Pointer; cdecl;
    function initWithPhonemeString(phoneme: NSString; atByteSampleOffset: NSInteger): Pointer; cdecl;
    function initWithSentenceRange(range: NSRange; atByteSampleOffset: NSInteger): Pointer; cdecl;
    function initWithWordRange(range: NSRange; atByteSampleOffset: NSInteger): Pointer; cdecl;
    function mark: AVSpeechSynthesisMarkerMark; cdecl;
    function phoneme: NSString; cdecl;
    procedure setBookmarkName(bookmarkName: NSString); cdecl;
    procedure setByteSampleOffset(byteSampleOffset: NSUInteger); cdecl;
    procedure setMark(mark: AVSpeechSynthesisMarkerMark); cdecl;
    procedure setPhoneme(phoneme: NSString); cdecl;
    procedure setTextRange(textRange: NSRange); cdecl;
    function textRange: NSRange; cdecl;
  end;
  TAVSpeechSynthesisMarker = class(TOCGenericImport<AVSpeechSynthesisMarkerClass, AVSpeechSynthesisMarker>) end;

  AVSpeechSynthesisProviderVoiceClass = interface(NSObjectClass)
    ['{B551E30E-F062-4907-AE4C-00B1231126E2}']
    {class} function new: Pointer; cdecl;
    {class} procedure updateSpeechVoices; cdecl;
  end;

  AVSpeechSynthesisProviderVoice = interface(NSObject)
    ['{6E6DDE26-CD95-47C6-B8C5-03D4D607507A}']
    function age: NSInteger; cdecl;
    function gender: AVSpeechSynthesisVoiceGender; cdecl;
    function identifier: NSString; cdecl;
    function initWithName(name: NSString; identifier: NSString; primaryLanguages: NSArray; supportedLanguages: NSArray): Pointer; cdecl;
    function name: NSString; cdecl;
    function primaryLanguages: NSArray; cdecl;
    procedure setAge(age: NSInteger); cdecl;
    procedure setGender(gender: AVSpeechSynthesisVoiceGender); cdecl;
    procedure setVersion(version: NSString); cdecl;
    procedure setVoiceSize(voiceSize: Int64); cdecl;
    function supportedLanguages: NSArray; cdecl;
    function version: NSString; cdecl;
    function voiceSize: Int64; cdecl;
  end;
  TAVSpeechSynthesisProviderVoice = class(TOCGenericImport<AVSpeechSynthesisProviderVoiceClass, AVSpeechSynthesisProviderVoice>) end;

  AVSpeechSynthesisProviderRequestClass = interface(NSObjectClass)
    ['{B8855126-5615-402B-A8D6-8372B375BF07}']
    {class} function new: Pointer; cdecl;
  end;

  AVSpeechSynthesisProviderRequest = interface(NSObject)
    ['{CA548AF1-5596-4C70-B47D-947F852C52CA}']
    function initWithSSMLRepresentation(text: NSString; voice: AVSpeechSynthesisProviderVoice): Pointer; cdecl;
    function ssmlRepresentation: NSString; cdecl;
    function voice: AVSpeechSynthesisProviderVoice; cdecl;
  end;
  TAVSpeechSynthesisProviderRequest = class(TOCGenericImport<AVSpeechSynthesisProviderRequestClass, AVSpeechSynthesisProviderRequest>) end;

  AVAudioApplicationClass = interface(NSObjectClass)
    ['{62A8447F-F425-4683-840C-7D96AB781393}']
    {class} procedure requestRecordPermissionWithCompletionHandler(response: TAVAudioApplicationBlockMethod2); cdecl;
    {class} function sharedInstance: AVAudioApplication; cdecl;
  end;

  AVAudioApplication = interface(NSObject)
    ['{F6F10132-D7F4-46AB-9ABF-D25A2D034238}']
    function isInputMuted: Boolean; cdecl;
    function recordPermission: AVAudioApplicationRecordPermission; cdecl;
    function setInputMuted(muted: Boolean; error: PPointer): Boolean; cdecl;
    function setInputMuteStateChangeHandler(inputMuteHandler: TAVAudioApplicationBlockMethod1; error: PPointer): Boolean; cdecl;
  end;
  TAVAudioApplication = class(TOCGenericImport<AVAudioApplicationClass, AVAudioApplication>) end;

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
function AVAudioSessionPortContinuityMicrophone: AVAudioSessionPort;
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
function AVAudioSessionRenderingModeChangeNotification: NSNotificationName;
function AVAudioSessionRenderingCapabilitiesChangeNotification: NSNotificationName;
function AVAudioSessionSpatialAudioEnabledKey: NSString;
function AVAudioSessionInterruptionTypeKey: NSString;
function AVAudioSessionInterruptionOptionKey: NSString;
function AVAudioSessionInterruptionReasonKey: NSString;
function AVAudioSessionInterruptionWasSuspendedKey: NSString;
function AVAudioSessionRouteChangeReasonKey: NSString;
function AVAudioSessionRouteChangePreviousRouteKey: NSString;
function AVAudioSessionSilenceSecondaryAudioHintTypeKey: NSString;
function AVAudioSessionRenderingModeNewRenderingModeKey: NSString;
function AVAudioSequencerInfoDictionaryKeyAlbum: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyApproximateDurationInSeconds: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyArtist: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyChannelLayout: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyComments: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyComposer: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyCopyright: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyEncodingApplication: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyGenre: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyISRC: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyKeySignature: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyLyricist: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyNominalBitRate: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyRecordedDate: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeySourceBitDepth: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeySourceEncoder: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeySubTitle: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyTempo: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyTimeSignature: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyTitle: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyTrackNumber: AVAudioSequencerInfoDictionaryKey;
function AVAudioSequencerInfoDictionaryKeyYear: AVAudioSequencerInfoDictionaryKey;
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
function AVSpeechSynthesisVoiceIdentifierAlex: NSString;
function AVSpeechSynthesisIPANotationAttribute: NSString;
function AVSpeechSynthesisAvailableVoicesDidChangeNotification: NSNotificationName;
function AVAudioApplicationInputMuteStateChangeNotification: NSNotificationName;
function AVAudioApplicationMuteStateKey: NSString;

const
  libAVFAudio = '/System/Library/Frameworks/AVFAudio.framework/AVFAudio';

implementation

uses
  System.SysUtils;

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

function AVAudioSessionPortContinuityMicrophone: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortContinuityMicrophone');
end;

function AVAudioSessionPortLineIn: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortLineIn');
end;

function AVAudioSessionPortBuiltInMic: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBuiltInMic');
end;

function AVAudioSessionPortHeadsetMic: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortHeadsetMic');
end;

function AVAudioSessionPortLineOut: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortLineOut');
end;

function AVAudioSessionPortHeadphones: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortHeadphones');
end;

function AVAudioSessionPortBluetoothA2DP: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBluetoothA2DP');
end;

function AVAudioSessionPortBuiltInReceiver: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBuiltInReceiver');
end;

function AVAudioSessionPortBuiltInSpeaker: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBuiltInSpeaker');
end;

function AVAudioSessionPortHDMI: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortHDMI');
end;

function AVAudioSessionPortAirPlay: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortAirPlay');
end;

function AVAudioSessionPortBluetoothLE: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBluetoothLE');
end;

function AVAudioSessionPortBluetoothHFP: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortBluetoothHFP');
end;

function AVAudioSessionPortUSBAudio: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortUSBAudio');
end;

function AVAudioSessionPortCarAudio: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortCarAudio');
end;

function AVAudioSessionPortVirtual: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortVirtual');
end;

function AVAudioSessionPortPCI: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortPCI');
end;

function AVAudioSessionPortFireWire: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortFireWire');
end;

function AVAudioSessionPortDisplayPort: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortDisplayPort');
end;

function AVAudioSessionPortAVB: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortAVB');
end;

function AVAudioSessionPortThunderbolt: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPortThunderbolt');
end;

function AVAudioSessionCategoryAmbient: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryAmbient');
end;

function AVAudioSessionCategorySoloAmbient: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategorySoloAmbient');
end;

function AVAudioSessionCategoryPlayback: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryPlayback');
end;

function AVAudioSessionCategoryRecord: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryRecord');
end;

function AVAudioSessionCategoryPlayAndRecord: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryPlayAndRecord');
end;

function AVAudioSessionCategoryAudioProcessing: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryAudioProcessing');
end;

function AVAudioSessionCategoryMultiRoute: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionCategoryMultiRoute');
end;

function AVAudioSessionModeDefault: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeDefault');
end;

function AVAudioSessionModeVoiceChat: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeVoiceChat');
end;

function AVAudioSessionModeGameChat: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeGameChat');
end;

function AVAudioSessionModeVideoRecording: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeVideoRecording');
end;

function AVAudioSessionModeMeasurement: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeMeasurement');
end;

function AVAudioSessionModeMoviePlayback: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeMoviePlayback');
end;

function AVAudioSessionModeVideoChat: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeVideoChat');
end;

function AVAudioSessionModeSpokenAudio: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeSpokenAudio');
end;

function AVAudioSessionModeVoicePrompt: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionModeVoicePrompt');
end;

function AVAudioSessionLocationUpper: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionLocationUpper');
end;

function AVAudioSessionLocationLower: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionLocationLower');
end;

function AVAudioSessionOrientationTop: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationTop');
end;

function AVAudioSessionOrientationBottom: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationBottom');
end;

function AVAudioSessionOrientationFront: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationFront');
end;

function AVAudioSessionOrientationBack: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationBack');
end;

function AVAudioSessionOrientationLeft: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationLeft');
end;

function AVAudioSessionOrientationRight: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionOrientationRight');
end;

function AVAudioSessionPolarPatternOmnidirectional: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPolarPatternOmnidirectional');
end;

function AVAudioSessionPolarPatternCardioid: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPolarPatternCardioid');
end;

function AVAudioSessionPolarPatternSubcardioid: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPolarPatternSubcardioid');
end;

function AVAudioSessionPolarPatternStereo: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionPolarPatternStereo');
end;

function AVAudioSessionInterruptionNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionInterruptionNotification');
end;

function AVAudioSessionRouteChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionRouteChangeNotification');
end;

function AVAudioSessionMediaServicesWereLostNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionMediaServicesWereLostNotification');
end;

function AVAudioSessionMediaServicesWereResetNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionMediaServicesWereResetNotification');
end;

function AVAudioSessionSilenceSecondaryAudioHintNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionSilenceSecondaryAudioHintNotification');
end;

function AVAudioSessionSpatialPlaybackCapabilitiesChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionSpatialPlaybackCapabilitiesChangedNotification');
end;

function AVAudioSessionRenderingModeChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionRenderingModeChangeNotification');
end;

function AVAudioSessionRenderingCapabilitiesChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionRenderingCapabilitiesChangeNotification');
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

function AVAudioSessionRenderingModeNewRenderingModeKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSessionRenderingModeNewRenderingModeKey');
end;

function AVAudioSequencerInfoDictionaryKeyAlbum: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyAlbum');
end;

function AVAudioSequencerInfoDictionaryKeyApproximateDurationInSeconds: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyApproximateDurationInSeconds');
end;

function AVAudioSequencerInfoDictionaryKeyArtist: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyArtist');
end;

function AVAudioSequencerInfoDictionaryKeyChannelLayout: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyChannelLayout');
end;

function AVAudioSequencerInfoDictionaryKeyComments: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyComments');
end;

function AVAudioSequencerInfoDictionaryKeyComposer: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyComposer');
end;

function AVAudioSequencerInfoDictionaryKeyCopyright: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyCopyright');
end;

function AVAudioSequencerInfoDictionaryKeyEncodingApplication: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyEncodingApplication');
end;

function AVAudioSequencerInfoDictionaryKeyGenre: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyGenre');
end;

function AVAudioSequencerInfoDictionaryKeyISRC: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyISRC');
end;

function AVAudioSequencerInfoDictionaryKeyKeySignature: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyKeySignature');
end;

function AVAudioSequencerInfoDictionaryKeyLyricist: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyLyricist');
end;

function AVAudioSequencerInfoDictionaryKeyNominalBitRate: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyNominalBitRate');
end;

function AVAudioSequencerInfoDictionaryKeyRecordedDate: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyRecordedDate');
end;

function AVAudioSequencerInfoDictionaryKeySourceBitDepth: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeySourceBitDepth');
end;

function AVAudioSequencerInfoDictionaryKeySourceEncoder: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeySourceEncoder');
end;

function AVAudioSequencerInfoDictionaryKeySubTitle: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeySubTitle');
end;

function AVAudioSequencerInfoDictionaryKeyTempo: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyTempo');
end;

function AVAudioSequencerInfoDictionaryKeyTimeSignature: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyTimeSignature');
end;

function AVAudioSequencerInfoDictionaryKeyTitle: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyTitle');
end;

function AVAudioSequencerInfoDictionaryKeyTrackNumber: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyTrackNumber');
end;

function AVAudioSequencerInfoDictionaryKeyYear: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioSequencerInfoDictionaryKeyYear');
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

function AVAudioUnitComponentManagerRegistrationsChangedNotification: NSString;
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

function AVSpeechSynthesisAvailableVoicesDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVSpeechSynthesisAvailableVoicesDidChangeNotification');
end;

function AVAudioApplicationInputMuteStateChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioApplicationInputMuteStateChangeNotification');
end;

function AVAudioApplicationMuteStateKey: NSString;
begin
  Result := CocoaNSStringConst(libAVFAudio, 'AVAudioApplicationMuteStateKey');
end;

initialization
  AVFAudioModule := LoadLibrary(libAVFAudio);

finalization
  if AVFAudioModule <> 0 then
    FreeLibrary(AVFAudioModule);

end.