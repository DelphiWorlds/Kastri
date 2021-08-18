unit DW.Macapi.Speech;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation;

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
    ['{4C49D906-1F67-4E5F-A968-3A3BC2A011E1}']
  end;

  SFAcousticFeature = interface(NSObject)
    ['{0CFA006F-2E13-4D56-B3C9-3C45DDA58E73}']
    function acousticFeatureValuePerFrame: NSArray; cdecl;
    function frameDuration: NSTimeInterval; cdecl;
  end;
  TSFAcousticFeature = class(TOCGenericImport<SFAcousticFeatureClass, SFAcousticFeature>) end;

  SFVoiceAnalyticsClass = interface(NSObjectClass)
    ['{9FAE6C43-5B34-4E5C-99BF-F568B018C99E}']
  end;

  SFVoiceAnalytics = interface(NSObject)
    ['{AC630EA0-8DD6-4A7A-87A7-D9174A5726D4}']
    function jitter: SFAcousticFeature; cdecl;
    function pitch: SFAcousticFeature; cdecl;
    function shimmer: SFAcousticFeature; cdecl;
    function voicing: SFAcousticFeature; cdecl;
  end;
  TSFVoiceAnalytics = class(TOCGenericImport<SFVoiceAnalyticsClass, SFVoiceAnalytics>) end;

  SFSpeechRecognitionMetadataClass = interface(NSObjectClass)
    ['{A8096988-E36D-4C88-857C-0FC68283F1F7}']
  end;

  SFSpeechRecognitionMetadata = interface(NSObject)
    ['{4AF01280-3DC9-4DB3-959A-04FD5BF8F048}']
    function averagePauseDuration: NSTimeInterval; cdecl;
    function speakingRate: Double; cdecl;
    function speechDuration: NSTimeInterval; cdecl;
    function speechStartTimestamp: NSTimeInterval; cdecl;
    function voiceAnalytics: SFVoiceAnalytics; cdecl;
  end;
  TSFSpeechRecognitionMetadata = class(TOCGenericImport<SFSpeechRecognitionMetadataClass, SFSpeechRecognitionMetadata>) end;

  SFSpeechRecognitionResultClass = interface(NSObjectClass)
    ['{8422338C-D41C-4498-B69C-33A5DE49BAEC}']
  end;

  SFSpeechRecognitionResult = interface(NSObject)
    ['{58921884-8B10-40CD-AD89-097B25E0440C}']
    function bestTranscription: SFTranscription; cdecl;
    function isFinal: Boolean; cdecl;
    function speechRecognitionMetadata: SFSpeechRecognitionMetadata; cdecl;
    function transcriptions: NSArray; cdecl;
  end;
  TSFSpeechRecognitionResult = class(TOCGenericImport<SFSpeechRecognitionResultClass, SFSpeechRecognitionResult>) end;

  SFSpeechRecognitionRequestClass = interface(NSObjectClass)
    ['{E1D2FFAE-FB62-4028-B549-5E1FE07D7615}']
  end;

  SFSpeechRecognitionRequest = interface(NSObject)
    ['{F5B89A17-A923-424E-98A5-4C5C62A02712}']
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
    ['{34658A81-099D-4A46-A474-1C5DC4D7E2FC}']
  end;

  SFSpeechURLRecognitionRequest = interface(SFSpeechRecognitionRequest)
    ['{13544912-1012-46A7-B74C-A49C60AF5B1F}']
    function initWithURL(URL: NSURL): Pointer; cdecl;
    function URL: NSURL; cdecl;
  end;
  TSFSpeechURLRecognitionRequest = class(TOCGenericImport<SFSpeechURLRecognitionRequestClass, SFSpeechURLRecognitionRequest>) end;

  SFSpeechAudioBufferRecognitionRequestClass = interface(SFSpeechRecognitionRequestClass)
    ['{686BA6FC-E88C-46F6-B8B1-550A0D3CAC53}']
  end;

  SFSpeechAudioBufferRecognitionRequest = interface(SFSpeechRecognitionRequest)
    ['{1B08499B-6E30-4474-B57D-F85B0144C1ED}']
    procedure appendAudioPCMBuffer(audioPCMBuffer: AVAudioPCMBuffer); cdecl;
    procedure appendAudioSampleBuffer(sampleBuffer: CMSampleBufferRef); cdecl;
    procedure endAudio; cdecl;
    function nativeAudioFormat: AVAudioFormat; cdecl;
  end;
  TSFSpeechAudioBufferRecognitionRequest = class(TOCGenericImport<SFSpeechAudioBufferRecognitionRequestClass, SFSpeechAudioBufferRecognitionRequest>) end;

  SFSpeechRecognitionTaskClass = interface(NSObjectClass)
    ['{68284685-C6E9-46CE-8E21-BA5283267388}']
  end;

  SFSpeechRecognitionTask = interface(NSObject)
    ['{15399EB1-7984-4D91-89FC-00E5E22CD8BA}']
    procedure cancel; cdecl;
    function error: NSError; cdecl;
    procedure finish; cdecl;
    function isCancelled: Boolean; cdecl;
    function isFinishing: Boolean; cdecl;
    function state: SFSpeechRecognitionTaskState; cdecl;
  end;
  TSFSpeechRecognitionTask = class(TOCGenericImport<SFSpeechRecognitionTaskClass, SFSpeechRecognitionTask>) end;

  SFSpeechRecognitionTaskDelegate = interface(IObjectiveC)
    ['{D999131C-C4AA-4746-ACE4-3C7CFC4E14B8}']
    procedure speechRecognitionDidDetectSpeech(task: SFSpeechRecognitionTask); cdecl;
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; didFinishSuccessfully: Boolean); overload; cdecl;
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; didFinishRecognition: SFSpeechRecognitionResult); overload; cdecl;
    procedure speechRecognitionTask(task: SFSpeechRecognitionTask; didHypothesizeTranscription: SFTranscription); overload; cdecl;
    procedure speechRecognitionTaskFinishedReadingAudio(task: SFSpeechRecognitionTask); cdecl;
    procedure speechRecognitionTaskWasCancelled(task: SFSpeechRecognitionTask); cdecl;
  end;

  SFSpeechRecognizerClass = interface(NSObjectClass)
    ['{F5252F37-9732-4B22-80D3-8E261FF7212E}']
    {class} function authorizationStatus: SFSpeechRecognizerAuthorizationStatus; cdecl;
    {class} procedure requestAuthorization(handler: TSFSpeechRecognizerBlockMethod1); cdecl;
    {class} function supportedLocales: NSSet; cdecl;
  end;

  SFSpeechRecognizer = interface(NSObject)
    ['{A2AD5BAF-781C-4DDB-AA9C-9792F35B28E8}']
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
    ['{BB7C8EA3-F52B-41F1-A22A-5BD0D1CC5336}']
    procedure speechRecognizer(speechRecognizer: SFSpeechRecognizer; availabilityDidChange: Boolean); cdecl;
  end;

  SFTranscriptionSegmentClass = interface(NSObjectClass)
    ['{37AD3C9B-E34C-4905-B5A5-B32F6AD033CF}']
  end;

  SFTranscriptionSegment = interface(NSObject)
    ['{A05F9D38-4EB3-4D72-B14C-2AD82D841C80}']
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
    ['{48974585-EA66-457E-871E-1653C58F4020}']
  end;

  SFTranscription = interface(NSObject)
    ['{9A39937F-9BA9-4CFE-9D0A-CF4B9A599155}']
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
  System.SysUtils;

var
  SpeechModule: THandle;

initialization
  SpeechModule := LoadLibrary(libSpeech);

finalization
  if SpeechModule <> 0 then
    FreeLibrary(SpeechModule);

end.