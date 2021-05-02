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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.AVFoundation,
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
  SFSpeechRecognitionRequest = interface;
  SFSpeechURLRecognitionRequest = interface;
  SFSpeechAudioBufferRecognitionRequest = interface;
  SFSpeechRecognitionResult = interface;
  SFSpeechRecognitionTask = interface;
  SFSpeechRecognitionTaskDelegate = interface;
  SFSpeechRecognizer = interface;
  SFSpeechRecognizerDelegate = interface;
  SFTranscription = interface;
  SFTranscriptionSegment = interface;
  SFAcousticFeature = interface;
  SFVoiceAnalytics = interface;

  CMSampleBufferRef = Pointer;
  SFSpeechRecognitionTaskHint = NSInteger;
  SFSpeechRecognitionTaskState = NSInteger;
  SFSpeechRecognizerAuthorizationStatus = NSInteger;
  TSFSpeechRecognizerBlockMethod1 = procedure(status: SFSpeechRecognizerAuthorizationStatus) of object;
  TSFSpeechRecognizerBlockMethod2 = procedure(result: SFSpeechRecognitionResult; error: NSError) of object;

  SFSpeechRecognitionRequestClass = interface(NSObjectClass)
    ['{4566BEE9-FD82-4AAE-A886-35B908647C54}']
  end;

  SFSpeechRecognitionRequest = interface(NSObject)
    ['{0B30EEA4-4CC0-4269-B776-4A8265163ACC}']
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
    ['{6A942D5B-31A9-452E-86C0-8B6791433BD3}']
  end;

  SFSpeechURLRecognitionRequest = interface(SFSpeechRecognitionRequest)
    ['{7BA4B974-93E7-40FC-B65A-9B9F9C2C56A2}']
    function initWithURL(URL: NSURL): Pointer; cdecl;
    function URL: NSURL; cdecl;
  end;
  TSFSpeechURLRecognitionRequest = class(TOCGenericImport<SFSpeechURLRecognitionRequestClass, SFSpeechURLRecognitionRequest>) end;

  SFSpeechAudioBufferRecognitionRequestClass = interface(SFSpeechRecognitionRequestClass)
    ['{B2814305-C1AF-4238-8CB6-4FDDA8046DE7}']
  end;

  SFSpeechAudioBufferRecognitionRequest = interface(SFSpeechRecognitionRequest)
    ['{8B030017-D7FD-4961-A7D6-2B7301DC3C28}']
    procedure appendAudioPCMBuffer(audioPCMBuffer: AVAudioPCMBuffer); cdecl;
    procedure appendAudioSampleBuffer(sampleBuffer: CMSampleBufferRef); cdecl;
    procedure endAudio; cdecl;
    function nativeAudioFormat: AVAudioFormat; cdecl;
  end;
  TSFSpeechAudioBufferRecognitionRequest = class(TOCGenericImport<SFSpeechAudioBufferRecognitionRequestClass, SFSpeechAudioBufferRecognitionRequest>)
  end;

  SFSpeechRecognitionResultClass = interface(NSObjectClass)
    ['{AF09448D-A044-4442-A089-9F3F2B917C05}']
  end;

  SFSpeechRecognitionResult = interface(NSObject)
    ['{21D9172B-CACE-4EF6-AFC9-259A21F3731F}']
    function bestTranscription: SFTranscription; cdecl;
    function isFinal: Boolean; cdecl;
    function transcriptions: NSArray; cdecl;
  end;
  TSFSpeechRecognitionResult = class(TOCGenericImport<SFSpeechRecognitionResultClass, SFSpeechRecognitionResult>) end;

  SFSpeechRecognitionTaskClass = interface(NSObjectClass)
    ['{2B76F35E-9250-456C-A6A7-42547F8CBFE2}']
  end;

  SFSpeechRecognitionTask = interface(NSObject)
    ['{35CC04FF-E3A9-40BE-A615-C555B02C3578}']
    procedure cancel; cdecl;
    function error: NSError; cdecl;
    procedure finish; cdecl;
    function isCancelled: Boolean; cdecl;
    function isFinishing: Boolean; cdecl;
    function state: SFSpeechRecognitionTaskState; cdecl;
  end;
  TSFSpeechRecognitionTask = class(TOCGenericImport<SFSpeechRecognitionTaskClass, SFSpeechRecognitionTask>) end;

  SFSpeechRecognitionTaskDelegate = interface(IObjectiveC)
    ['{AC2753AE-CF5A-4971-B2EE-FE537F6F547B}']
    procedure speechRecognitionDidDetectSpeech(task: SFSpeechRecognitionTask); cdecl;
    [MethodName('speechRecognitionTask:didFinishSuccessfully:')]
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; successfully: Boolean); overload; cdecl;
    [MethodName('speechRecognitionTask:didFinishRecognition:')]
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; recognitionResult: SFSpeechRecognitionResult); overload; cdecl;
    [MethodName('speechRecognitionTask:didHypothesizeTranscription:')]
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; transcription: SFTranscription); overload; cdecl;
    procedure speechRecognitionTaskFinishedReadingAudio(task: SFSpeechRecognitionTask); cdecl;
    procedure speechRecognitionTaskWasCancelled(task: SFSpeechRecognitionTask); cdecl;
  end;

  SFSpeechRecognizerClass = interface(NSObjectClass)
    ['{F09E365A-0445-4EE1-83DF-106F06ACC370}']
    {class} function authorizationStatus: SFSpeechRecognizerAuthorizationStatus; cdecl;
    {class} procedure requestAuthorization(handler: TSFSpeechRecognizerBlockMethod1); cdecl;
    {class} function supportedLocales: NSSet; cdecl;
  end;

  SFSpeechRecognizer = interface(NSObject)
    ['{7DC66BE6-338B-453D-ACD0-84D7C6F65AD2}']
    function defaultTaskHint: SFSpeechRecognitionTaskHint; cdecl;
    function delegate: Pointer; cdecl;
    function initWithLocale(locale: NSLocale): Pointer; cdecl;
    function isAvailable: Boolean; cdecl;
    function locale: NSLocale; cdecl;
    function queue: NSOperationQueue; cdecl;
    [MethodName('recognitionTaskWithRequest:resultHandler:')]
    function recognitionTaskWithRequest(request: SFSpeechRecognitionRequest;
      resultHandler: TSFSpeechRecognizerBlockMethod2): SFSpeechRecognitionTask; overload; cdecl;
    [MethodName('recognitionTaskWithRequest:delegate:')]
    function recognitionTaskWithRequest(request: SFSpeechRecognitionRequest; delegate: Pointer): SFSpeechRecognitionTask; overload; cdecl;
    procedure setDefaultTaskHint(defaultTaskHint: SFSpeechRecognitionTaskHint); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setQueue(queue: NSOperationQueue); cdecl;
    procedure setSupportsOnDeviceRecognition(supportsOnDeviceRecognition: Boolean); cdecl;
    function supportsOnDeviceRecognition: Boolean; cdecl;
  end;
  TSFSpeechRecognizer = class(TOCGenericImport<SFSpeechRecognizerClass, SFSpeechRecognizer>) end;

  SFSpeechRecognizerDelegate = interface(IObjectiveC)
    ['{796B2D4F-95DD-48BE-A6F7-0A8F261748E8}']
    [MethodName('speechRecognizer:availabilityDidChange:')]
    procedure speechRecognizer(speechRecognizer: SFSpeechRecognizer; available: Boolean); cdecl;
  end;

  SFTranscriptionClass = interface(NSObjectClass)
    ['{3138A6C8-71A1-4E3B-A571-6D216B31F1FF}']
  end;

  SFTranscription = interface(NSObject)
    ['{08D6927D-48CD-47BF-A8E7-240F0FF0D65A}']
    function averagePauseDuration: NSTimeInterval; cdecl;
    function formattedString: NSString; cdecl;
    function segments: NSArray; cdecl;
    function speakingRate: Double; cdecl;
  end;
  TSFTranscription = class(TOCGenericImport<SFTranscriptionClass, SFTranscription>) end;

  SFTranscriptionSegmentClass = interface(NSObjectClass)
    ['{F27BD056-3E63-4653-B494-32B3A97BDE16}']
  end;

  SFTranscriptionSegment = interface(NSObject)
    ['{5B3F0F33-BD89-4414-9901-97A3695755DD}']
    function alternativeSubstrings: NSArray; cdecl;
    function confidence: Single; cdecl;
    function duration: NSTimeInterval; cdecl;
    function substring: NSString; cdecl;
    function substringRange: NSRange; cdecl;
    function timestamp: NSTimeInterval; cdecl;
    function voiceAnalytics: SFVoiceAnalytics; cdecl;
  end;
  TSFTranscriptionSegment = class(TOCGenericImport<SFTranscriptionSegmentClass, SFTranscriptionSegment>) end;

  SFAcousticFeatureClass = interface(NSObjectClass)
    ['{4E8E35CE-015E-4269-A69A-A05CB8997AC3}']
  end;

  SFAcousticFeature = interface(NSObject)
    ['{5D689C6E-1F8B-4CFE-ADEC-4E1C1B8FC840}']
    function acousticFeatureValuePerFrame: NSArray; cdecl;
    function frameDuration: NSTimeInterval; cdecl;
  end;
  TSFAcousticFeature = class(TOCGenericImport<SFAcousticFeatureClass, SFAcousticFeature>) end;

  SFVoiceAnalyticsClass = interface(NSObjectClass)
    ['{B679CEA9-FA51-47F5-B334-66081F6773E4}']
  end;

  SFVoiceAnalytics = interface(NSObject)
    ['{A9E7A5E7-89F3-4A0A-B1FA-B3BBDAE0373C}']
    function jitter: SFAcousticFeature; cdecl;
    function pitch: SFAcousticFeature; cdecl;
    function shimmer: SFAcousticFeature; cdecl;
    function voicing: SFAcousticFeature; cdecl;
  end;
  TSFVoiceAnalytics = class(TOCGenericImport<SFVoiceAnalyticsClass, SFVoiceAnalytics>) end;

const
  libSpeech = '/System/Library/Frameworks/Speech.framework/Speech';

procedure libSpeechLoader; cdecl; external libSpeech;

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  SpeechModule: THandle;
{$ENDIF}

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  SpeechModule := dlopen(MarshaledAString(libSpeech), RTLD_LAZY);

finalization
  dlclose(SpeechModule)
{$ENDIF}

end.