unit DW.iOSapi.Speech;

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
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.AVFoundation, iOSapi.CoreMedia,
  // DW
  DW.iOSapi.AVFoundation;

const
  SFSpeechRecognitionTaskHintUnspecified = 0;
  SFSpeechRecognitionTaskHintDictation = 1;
  SFSpeechRecognitionTaskHintSearch = 2;
  SFSpeechRecognitionTaskHintConfirmation = 3;
  SFSpeechRecognitionTaskStateStarting = 0;
  SFSpeechRecognitionTaskStateRunning = 1;
  SFSpeechRecognitionTaskStateFinishing = 2;
  SFSpeechRecognitionTaskStateCanceling = 3;
  SFSpeechRecognitionTaskStateCompleted = 4;
  SFSpeechRecognizerAuthorizationStatusNotDetermined = 0;
  SFSpeechRecognizerAuthorizationStatusDenied = 1;
  SFSpeechRecognizerAuthorizationStatusRestricted = 2;
  SFSpeechRecognizerAuthorizationStatusAuthorized = 3;

type
  SFAcousticFeature = interface;
  SFVoiceAnalytics = interface;
  SFSpeechRecognitionMetadata = interface;
  SFSpeechRecognitionResult = interface;
  SFSpeechRecognitionRequest = interface;
  SFSpeechURLRecognitionRequest = interface;
  SFSpeechAudioBufferRecognitionRequest = interface;
  SFSpeechRecognitionTask = interface;
  SFSpeechRecognitionTaskDelegate = interface;
  SFSpeechRecognizer = interface;
  SFSpeechRecognizerDelegate = interface;
  SFTranscriptionSegment = interface;
  SFTranscription = interface;

  SFSpeechRecognitionTaskHint = NSInteger;
  SFSpeechRecognitionTaskState = NSInteger;
  SFSpeechRecognizerAuthorizationStatus = NSInteger;
  TSFSpeechRecognizerBlockMethod1 = procedure(status: SFSpeechRecognizerAuthorizationStatus) of object;
  TSFSpeechRecognizerBlockMethod2 = procedure(result: SFSpeechRecognitionResult; error: NSError) of object;

  SFAcousticFeatureClass = interface(NSObjectClass)
    ['{8079746C-3E7E-4C0A-9640-87F675A553E4}']
  end;

  SFAcousticFeature = interface(NSObject)
    ['{8AD4DA6E-401E-4F66-B961-CB4B1D1FF3F3}']
    function acousticFeatureValuePerFrame: NSArray; cdecl;
    function frameDuration: NSTimeInterval; cdecl;
  end;
  TSFAcousticFeature = class(TOCGenericImport<SFAcousticFeatureClass, SFAcousticFeature>) end;

  SFVoiceAnalyticsClass = interface(NSObjectClass)
    ['{B57FC22D-2BFD-4069-902A-A9EC61B6ACA2}']
  end;

  SFVoiceAnalytics = interface(NSObject)
    ['{FE0D7AD4-8788-4CE5-B197-2E52930CB0AC}']
    function jitter: SFAcousticFeature; cdecl;
    function pitch: SFAcousticFeature; cdecl;
    function shimmer: SFAcousticFeature; cdecl;
    function voicing: SFAcousticFeature; cdecl;
  end;
  TSFVoiceAnalytics = class(TOCGenericImport<SFVoiceAnalyticsClass, SFVoiceAnalytics>) end;

  SFSpeechRecognitionMetadataClass = interface(NSObjectClass)
    ['{32933790-45D2-457D-85E0-96CA8DB28C68}']
  end;

  SFSpeechRecognitionMetadata = interface(NSObject)
    ['{02DFB0B2-2FFC-4BC5-BA6C-5175248241CD}']
    function averagePauseDuration: NSTimeInterval; cdecl;
    function speakingRate: Double; cdecl;
    function speechDuration: NSTimeInterval; cdecl;
    function speechStartTimestamp: NSTimeInterval; cdecl;
    function voiceAnalytics: SFVoiceAnalytics; cdecl;
  end;
  TSFSpeechRecognitionMetadata = class(TOCGenericImport<SFSpeechRecognitionMetadataClass, SFSpeechRecognitionMetadata>) end;

  SFSpeechRecognitionResultClass = interface(NSObjectClass)
    ['{7A07B162-D051-4B8D-9F81-56706B101D88}']
  end;

  SFSpeechRecognitionResult = interface(NSObject)
    ['{3A7E9CAC-6A2C-4274-8F2C-DB7B9FB26BE7}']
    function bestTranscription: SFTranscription; cdecl;
    function isFinal: Boolean; cdecl;
    function speechRecognitionMetadata: SFSpeechRecognitionMetadata; cdecl;
    function transcriptions: NSArray; cdecl;
  end;
  TSFSpeechRecognitionResult = class(TOCGenericImport<SFSpeechRecognitionResultClass, SFSpeechRecognitionResult>) end;

  SFSpeechRecognitionRequestClass = interface(NSObjectClass)
    ['{C209DB6B-1635-4BC2-A149-358D9B57ECA4}']
  end;

  SFSpeechRecognitionRequest = interface(NSObject)
    ['{790A7056-EAB7-4C4F-88FB-4CAC9DC176C0}']
    function contextualStrings: NSArray; cdecl;
    function interactionIdentifier: NSString; cdecl;
    function requiresOnDeviceRecognition: Boolean; cdecl;
    procedure setContextualStrings(contextualStrings: NSArray); cdecl;
    procedure setInteractionIdentifier(interactionIdentifier: NSString); cdecl;
    procedure setRequiresOnDeviceRecognition(requiresOnDeviceRecognition: Boolean); cdecl;
    procedure setShouldReportPartialResults(shouldReportPartialResults: Boolean); cdecl;
    procedure setTaskHint(taskHint: SFSpeechRecognitionTaskHint); cdecl;
    function shouldReportPartialResults: Boolean; cdecl;
    function taskHint: SFSpeechRecognitionTaskHint; cdecl;
  end;
  TSFSpeechRecognitionRequest = class(TOCGenericImport<SFSpeechRecognitionRequestClass, SFSpeechRecognitionRequest>) end;

  SFSpeechURLRecognitionRequestClass = interface(SFSpeechRecognitionRequestClass)
    ['{D50082D7-0ADC-4ABB-9213-6BF24571D578}']
  end;

  SFSpeechURLRecognitionRequest = interface(SFSpeechRecognitionRequest)
    ['{122F1A9E-9AE3-4F1E-B460-D7EC3E9CD126}']
    function initWithURL(URL: NSURL): Pointer; cdecl;
    function URL: NSURL; cdecl;
  end;
  TSFSpeechURLRecognitionRequest = class(TOCGenericImport<SFSpeechURLRecognitionRequestClass, SFSpeechURLRecognitionRequest>) end;

  SFSpeechAudioBufferRecognitionRequestClass = interface(SFSpeechRecognitionRequestClass)
    ['{32D70BA1-C609-4DF3-80D5-76C1CBBC1CEF}']
  end;

  SFSpeechAudioBufferRecognitionRequest = interface(SFSpeechRecognitionRequest)
    ['{A869E100-9E11-4421-99B4-6B7D175831E9}']
    procedure appendAudioPCMBuffer(audioPCMBuffer: AVAudioPCMBuffer); cdecl;
    procedure appendAudioSampleBuffer(sampleBuffer: CMSampleBufferRef); cdecl;
    procedure endAudio; cdecl;
    function nativeAudioFormat: AVAudioFormat; cdecl;
  end;
  TSFSpeechAudioBufferRecognitionRequest = class(TOCGenericImport<SFSpeechAudioBufferRecognitionRequestClass, SFSpeechAudioBufferRecognitionRequest>) end;

  SFSpeechRecognitionTaskClass = interface(NSObjectClass)
    ['{60774E0D-0332-4682-A885-AE48550E407E}']
  end;

  SFSpeechRecognitionTask = interface(NSObject)
    ['{BCB2FD7C-B7EE-43EE-ACAE-797A7B82EAA4}']
    procedure cancel; cdecl;
    function error: NSError; cdecl;
    procedure finish; cdecl;
    function isCancelled: Boolean; cdecl;
    function isFinishing: Boolean; cdecl;
    function state: SFSpeechRecognitionTaskState; cdecl;
  end;
  TSFSpeechRecognitionTask = class(TOCGenericImport<SFSpeechRecognitionTaskClass, SFSpeechRecognitionTask>) end;

  SFSpeechRecognitionTaskDelegate = interface(IObjectiveC)
    ['{92718CD6-A513-459C-9D36-6EF6AA562C04}']
    procedure speechRecognitionDidDetectSpeech(task: SFSpeechRecognitionTask); cdecl;
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; didFinishSuccessfully: Boolean); overload; cdecl;
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; didFinishRecognition: SFSpeechRecognitionResult); overload; cdecl;
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; didHypothesizeTranscription: SFTranscription); overload; cdecl;
    procedure speechRecognitionTaskFinishedReadingAudio(task: SFSpeechRecognitionTask); cdecl;
    procedure speechRecognitionTaskWasCancelled(task: SFSpeechRecognitionTask); cdecl;
  end;

  SFSpeechRecognizerClass = interface(NSObjectClass)
    ['{A71F83A9-2D4F-4780-9D40-2232DED79BF1}']
    {class} function authorizationStatus: SFSpeechRecognizerAuthorizationStatus; cdecl;
    {class} procedure requestAuthorization(handler: TSFSpeechRecognizerBlockMethod1); cdecl;
    {class} function supportedLocales: NSSet; cdecl;
  end;

  SFSpeechRecognizer = interface(NSObject)
    ['{29181896-1685-44D5-894E-26A9AD92FFB6}']
    function defaultTaskHint: SFSpeechRecognitionTaskHint; cdecl;
    function delegate: Pointer; cdecl;
    function initWithLocale(locale: NSLocale): Pointer; cdecl;
    function isAvailable: Boolean; cdecl;
    function locale: NSLocale; cdecl;
    function queue: NSOperationQueue; cdecl;
    function recognitionTaskWithRequest(request: SFSpeechRecognitionRequest;
      resultHandler: TSFSpeechRecognizerBlockMethod2): SFSpeechRecognitionTask; overload; cdecl;
    function recognitionTaskWithRequest(request: SFSpeechRecognitionRequest; delegate: Pointer): SFSpeechRecognitionTask; overload; cdecl;
    procedure setDefaultTaskHint(defaultTaskHint: SFSpeechRecognitionTaskHint); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setQueue(queue: NSOperationQueue); cdecl;
    procedure setSupportsOnDeviceRecognition(supportsOnDeviceRecognition: Boolean); cdecl;
    function supportsOnDeviceRecognition: Boolean; cdecl;
  end;
  TSFSpeechRecognizer = class(TOCGenericImport<SFSpeechRecognizerClass, SFSpeechRecognizer>) end;

  SFSpeechRecognizerDelegate = interface(IObjectiveC)
    ['{6F8F57CA-346B-4E1E-BE24-DF8FAC4FA6E3}']
    procedure speechRecognizer(speechRecognizer: SFSpeechRecognizer; availabilityDidChange: Boolean); cdecl;
  end;

  SFTranscriptionSegmentClass = interface(NSObjectClass)
    ['{73A14B9C-7E68-4D57-ABD5-9E3A12BE3E5A}']
  end;

  SFTranscriptionSegment = interface(NSObject)
    ['{3BB20070-5C98-4D43-9261-B2CD696316E8}']
    function alternativeSubstrings: NSArray; cdecl;
    function confidence: Single; cdecl;
    function duration: NSTimeInterval; cdecl;
    function substring: NSString; cdecl;
    function substringRange: NSRange; cdecl;
    function timestamp: NSTimeInterval; cdecl;
    function voiceAnalytics: SFVoiceAnalytics; cdecl;
  end;
  TSFTranscriptionSegment = class(TOCGenericImport<SFTranscriptionSegmentClass, SFTranscriptionSegment>) end;

  SFTranscriptionClass = interface(NSObjectClass)
    ['{103C53EA-79F5-48DE-8EFD-FC71936E300C}']
  end;

  SFTranscription = interface(NSObject)
    ['{236774EE-488C-4BFC-941A-FDCC6BB7EC36}']
    function averagePauseDuration: NSTimeInterval; cdecl;
    function formattedString: NSString; cdecl;
    function segments: NSArray; cdecl;
    function speakingRate: Double; cdecl;
  end;
  TSFTranscription = class(TOCGenericImport<SFTranscriptionClass, SFTranscription>) end;

const
  libSpeech = '/System/Library/Frameworks/Speech.framework/Speech';

implementation

uses
  Posix.Dlfcn;

var
  SpeechModule: THandle;

initialization
  SpeechModule := dlopen(MarshaledAString(libSpeech), RTLD_LAZY);

finalization
  dlclose(SpeechModule);

end.