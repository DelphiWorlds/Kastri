unit DW.Macapi.AVFoundation;

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

interface

uses
  // macOS
  Macapi.Foundation, Macapi.CoreFoundation, Macapi.AVFoundation, Macapi.ObjectiveC, Macapi.CoreMedia, Macapi.CocoaTypes, Macapi.Dispatch;

const
  AVAudioSessionActivationOptionNone = 0;
  AVAudioSessionCategoryOptionMixWithOthers = 1;
  AVAudioSessionCategoryOptionDuckOthers = 2;
  AVAudioSessionCategoryOptionAllowBluetooth = 4;
  AVAudioSessionCategoryOptionDefaultToSpeaker = 8;
  AVAudioSessionCategoryOptionInterruptSpokenAudioAndMixWithOthers = 17;
  AVAudioSessionCategoryOptionAllowBluetoothA2DP = 32;
  AVAudioSessionCategoryOptionAllowAirPlay = 64;
  AVAudioSessionIOTypeNotSpecified = 0;
  AVAudioSessionIOTypeAggregated = 1;
  AVAudioSessionPortOverrideNone = 0;
  AVAudioSessionPortOverrideSpeaker = 1936747378;
  AVAudioSessionPromptStyleNone = 1852796517;
  AVAudioSessionPromptStyleShort = 1936224884;
  AVAudioSessionPromptStyleNormal = 1852992876;
  AVAudioSessionRecordPermissionUndetermined = 1970168948;
  AVAudioSessionRecordPermissionDenied = 1684369017;
  AVAudioSessionRecordPermissionGranted = 1735552628;
  AVAudioSessionRouteSharingPolicyDefault = 0;
  AVAudioSessionRouteSharingPolicyLongFormAudio = 1;
  AVAudioSessionRouteSharingPolicyLongForm = AVAudioSessionRouteSharingPolicyLongFormAudio;
  AVAudioSessionRouteSharingPolicyIndependent = 2;
  AVAudioSessionRouteSharingPolicyLongFormVideo = 3;

type
  AVAudioSession = interface;
  AVAudioSessionDataSourceDescription = interface;
  AVAudioSessionPortDescription = interface;
  AVAudioSessionRouteDescription = interface;
  AVPlayerItemOutput = interface;
  AVPlayerItemVideoOutput = interface;

  AVAudioFormat = NSString;
  AVAudioSessionActivationOptions = NSInteger;
  AVAudioSessionCategory = NSString;
  AVAudioSessionCategoryOptions = NSInteger;
  AVAudioSessionIOType = NSInteger;
  AVAudioSessionLocation = NSString;
  AVAudioSessionOrientation = NSString;
  AVAudioSessionMode = NSString;
  AVAudioSessionPolarPattern = NSString;
  AVAudioSessionPort = NSString;
  AVAudioSessionPortOverride = NSInteger;
  AVAudioSessionPromptStyle = NSInteger;
  AVAudioSessionRecordPermission = NSInteger;
  AVAudioSessionRouteSharingPolicy = NSInteger;
  AVAudioSessionSetActiveOptions = NSInteger;

  PNSError = ^NSError;

  PermissionBlock = procedure(granted: Boolean) of object;
  TAVAudioSessionBlockMethod1 = procedure(activated: Boolean; error: NSError) of object;

  AVAudioPCMBufferClass = interface(AVAudioBufferClass)
    ['{C7781F91-4332-4117-A543-D906AA271F01}']
  end;

  AVPlayerItemOutputClass = interface(NSObjectClass)
    ['{3D89BA95-B673-402D-8046-BEE1763D9FF3}']
  end;

  AVPlayerItemOutput = interface(NSObject)
    ['{D2175E46-20A7-4F62-9D52-714CB52DE422}']
    function itemTimeForCVTimestamp(timestamp: CVTimeStamp): CMTime; cdecl;
    function itemTimeForHostTime(hostTimeInSeconds: CFTimeInterval): CMTime; cdecl;
    function itemTimeForMachAbsoluteTime(machAbsoluteTime: Int64): CMTime; cdecl;
    procedure setSuppressesPlayerRendering(suppressesPlayerRendering: Boolean); cdecl;
    function suppressesPlayerRendering: Boolean; cdecl;
  end;
  TAVPlayerItemOutput = class(TOCGenericImport<AVPlayerItemOutputClass, AVPlayerItemOutput>)
  end;

  AVPlayerItemVideoOutputClass = interface(AVPlayerItemOutputClass)
    ['{99EF1ABB-428A-44FE-B732-3A5CFF735C16}']
  end;

  AVPlayerItemVideoOutput = interface(AVPlayerItemOutput)
    ['{C25F9FB1-E515-4490-88E4-9D734088EF4D}']
    function initWithPixelBufferAttributes(pixelBufferAttributes: NSDictionary): Pointer { instancetype }; cdecl;
    function initWithOutputSettings(outputSettings: NSDictionary): Pointer { instancetype }; cdecl;
    function hasNewPixelBufferForItemTime(itemTime: CMTime): Pointer; cdecl;
    function copyPixelBufferForItemTime(itemTime: CMTime; itemTimeForDisplay: Pointer): CVPixelBufferRef; cdecl;
    procedure setDelegate(delegate: Pointer; queue: dispatch_queue_t); cdecl;
    procedure requestNotificationOfMediaDataChangeWithAdvanceInterval(interval: NSTimeInterval); cdecl;
    function delegate: Pointer; cdecl;
    function delegateQueue: dispatch_queue_t; cdecl;
  end;
  TAVPlayerItemVideoOutput = class(TOCGenericImport<AVPlayerItemVideoOutputClass, AVPlayerItemVideoOutput>)
  end;

  AVAudioSessionPortDescriptionClass = interface(NSObjectClass)
    ['{55543903-07DF-4E7E-8F46-299FB0525EDF}']
  end;

  AVAudioSessionPortDescription = interface(NSObject)
    ['{A32A505D-379D-480D-A5DF-D5EB8158E1A3}']
    function channels: NSArray; cdecl;
    function dataSources: NSArray; cdecl;
    function hasHardwareVoiceCallProcessing: Boolean; cdecl;
    function portName: NSString; cdecl;
    function portType: AVAudioSessionPort; cdecl;
    function preferredDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function selectedDataSource: AVAudioSessionDataSourceDescription; cdecl;
    [MethodName('setPreferredDataSource:error:')]
    function setPreferredDataSource(dataSource: AVAudioSessionDataSourceDescription; outError: PNSError): Boolean; cdecl;
    function UID: NSString; cdecl;
  end;
  TAVAudioSessionPortDescription = class(TOCGenericImport<AVAudioSessionPortDescriptionClass, AVAudioSessionPortDescription>) end;

  AVAudioSessionRouteDescriptionClass = interface(NSObjectClass)
    ['{2053C078-CC59-4632-AA57-C181AFCEDB18}']
  end;

  AVAudioSessionRouteDescription = interface(NSObject)
    ['{21EABAEF-0715-467E-8220-D8DC90A04E47}']
    function inputs: NSArray; cdecl;
    function outputs: NSArray; cdecl;
  end;
  TAVAudioSessionRouteDescription = class(TOCGenericImport<AVAudioSessionRouteDescriptionClass, AVAudioSessionRouteDescription>) end;

  AVAudioSessionDataSourceDescriptionClass = interface(NSObjectClass)
    ['{DF972BAE-60DB-4B4C-8D55-8DCFFB66E622}']
  end;

  AVAudioSessionDataSourceDescription = interface(NSObject)
    ['{820F6DCE-5800-42A2-8556-6D74BDD395E8}']
    function dataSourceID: NSNumber; cdecl;
    function dataSourceName: NSString; cdecl;
    function location: AVAudioSessionLocation; cdecl;
    function orientation: AVAudioSessionOrientation; cdecl;
    function preferredPolarPattern: AVAudioSessionPolarPattern; cdecl;
    function selectedPolarPattern: AVAudioSessionPolarPattern; cdecl;
    [MethodName('setPreferredPolarPattern:error:')]
    function setPreferredPolarPattern(pattern: AVAudioSessionPolarPattern; outError: PNSError): Boolean; cdecl;
    function supportedPolarPatterns: NSArray; cdecl;
  end;
  TAVAudioSessionDataSourceDescription = class(TOCGenericImport<AVAudioSessionDataSourceDescriptionClass, AVAudioSessionDataSourceDescription>) end;

  AVAudioSessionClass = interface(NSObjectClass)
    ['{A09C4958-0DFF-4F06-A312-EC90CCA30E52}']
    {class} function sharedInstance: AVAudioSession; cdecl;
  end;

  AVAudioSession = interface(NSObject)
    ['{22D61823-9B35-4834-A6B4-C70256017E88}']
    [MethodName('activateWithOptions:completionHandler:')]
    procedure activateWithOptions(options: AVAudioSessionActivationOptions; handler: TAVAudioSessionBlockMethod1); cdecl;
    function allowHapticsAndSystemSoundsDuringRecording: Boolean; cdecl;
    function availableCategories: NSArray; cdecl;
    function availableInputs: NSArray; cdecl;
    function availableModes: NSArray; cdecl;
    function category: AVAudioSessionCategory; cdecl;
    function categoryOptions: AVAudioSessionCategoryOptions; cdecl;
    function currentHardwareInputNumberOfChannels: NSInteger; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("inputNumberOfChannels", ios(3.0, 6.0))
    function currentHardwareOutputNumberOfChannels: NSInteger; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("outputNumberOfChannels", ios(3.0, 6.0))
    function currentHardwareSampleRate: Double; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("sampleRate", ios(3.0, 6.0))
    function currentRoute: AVAudioSessionRouteDescription; cdecl;
    function delegate: Pointer; cdecl; // API_DEPRECATED("No longer supported", ios(4.0, 6.0))
    function init: Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+sharedInstance", ios(3.0, 10.0))
    function inputDataSource: AVAudioSessionDataSourceDescription; cdecl;
    function inputDataSources: NSArray; cdecl;
    function inputGain: Single; cdecl;
    function inputIsAvailable: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("isInputAvailable", ios(3.0, 6.0))
    function inputLatency: NSTimeInterval; cdecl;
    function inputNumberOfChannels: NSInteger; cdecl;
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
    [MethodName('overrideOutputAudioPort:error:')]
    function overrideOutputAudioPort(portOverride: AVAudioSessionPortOverride; outError: PNSError): Boolean; cdecl;
    function preferredHardwareSampleRate: Double; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("preferredSampleRate", ios(3.0, 6.0))
    function preferredInput: AVAudioSessionPortDescription; cdecl;
    function preferredInputNumberOfChannels: NSInteger; cdecl;
    function preferredIOBufferDuration: NSTimeInterval; cdecl;
    function preferredOutputNumberOfChannels: NSInteger; cdecl;
    function preferredSampleRate: Double; cdecl;
    function promptStyle: AVAudioSessionPromptStyle; cdecl;
    function recordPermission: AVAudioSessionRecordPermission; cdecl;
    procedure requestRecordPermission(response: PermissionBlock); cdecl;
    function routeSharingPolicy: AVAudioSessionRouteSharingPolicy; cdecl;
    function sampleRate: Double; cdecl;
    function secondaryAudioShouldBeSilencedHint: Boolean; cdecl;
    [MethodName('setActive:withFlags:error:')]
    function setActive(active: Boolean; flags: NSInteger; outError: PNSError): Boolean; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-setActive:withOptions:error:", ios(4.0, 6.0))
    [MethodName('setActive:withOptions:error:')]
    function setActiveWithOptions(active: Boolean; options: AVAudioSessionSetActiveOptions; outError: PNSError): Boolean; cdecl;
    [MethodName('setActive:error:')]
    function setActive(active: Boolean; outError: PNSError): Boolean; overload; cdecl;
    [MethodName('setAggregatedIOPreference:error:')]
    function setAggregatedIOPreference(inIOType: AVAudioSessionIOType; outError: PNSError): Boolean; cdecl;
    [MethodName('setAllowHapticsAndSystemSoundsDuringRecording:error:')]
    function setAllowHapticsAndSystemSoundsDuringRecording(inValue: Boolean; outError: PNSError): Boolean; cdecl;
    [MethodName('setCategory:mode:routeSharingPolicy:options:error:')]
    function setCategory(category: AVAudioSessionCategory; mode: AVAudioSessionMode; policy: AVAudioSessionRouteSharingPolicy;
      options: AVAudioSessionCategoryOptions; outError: PNSError): Boolean; overload; cdecl;
    [MethodName('setCategory:mode:options:error:')]
    function setCategory(category: AVAudioSessionCategory; mode: AVAudioSessionMode; options: AVAudioSessionCategoryOptions;
      outError: PNSError): Boolean; overload; cdecl;
    [MethodName('setCategory:withOptions:error:')]
    function setCategory(category: AVAudioSessionCategory; options: AVAudioSessionCategoryOptions; outError: PNSError): Boolean; overload; cdecl;
    [MethodName('setCategory:error:')]
    function setCategoryError(category: AVAudioSessionCategory; outError: PNSError): Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl; // API_DEPRECATED("No longer supported", ios(4.0, 6.0))
    [MethodName('setInputDataSource:error:')]
    function setInputDataSource(dataSource: AVAudioSessionDataSourceDescription; outError: PNSError): Boolean; cdecl;
    [MethodName('setInputGain:error:')]
    function setInputGain(gain: Single; outError: PNSError): Boolean; cdecl;
    [MethodName('setMode:error:')]
    function setMode(mode: AVAudioSessionMode; outError: PNSError): Boolean; cdecl;
    [MethodName('setOutputDataSource:error:')]
    function setOutputDataSource(dataSource: AVAudioSessionDataSourceDescription; outError: PNSError): Boolean; cdecl;
    [MethodName('setPreferredHardwareSampleRate:error:')]
    function setPreferredHardwareSampleRate(sampleRate: Double; outError: PNSError): Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("setPreferredSampleRate:error:", ios(3.0, 6.0))
    [MethodName('setPreferredInput:error:')]
    function setPreferredInput(inPort: AVAudioSessionPortDescription; outError: PNSError): Boolean; cdecl;
    [MethodName('setPreferredInputNumberOfChannels:error:')]
    function setPreferredInputNumberOfChannels(count: NSInteger; outError: PNSError): Boolean; cdecl;
    [MethodName('setPreferredIOBufferDuration:error:')]
    function setPreferredIOBufferDuration(duration: NSTimeInterval; outError: PNSError): Boolean; cdecl;
    [MethodName('setPreferredOutputNumberOfChannels:error:')]
    function setPreferredOutputNumberOfChannels(count: NSInteger; outError: PNSError): Boolean; cdecl;
    [MethodName('setPreferredSampleRate:error:')]
    function setPreferredSampleRate(sampleRate: Double; outError: PNSError): Boolean; cdecl;
  end;
  TAVAudioSession = class(TOCGenericImport<AVAudioSessionClass, AVAudioSession>) end;

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
