unit DW.iOSapi.SoundAnalysis;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreMedia,
  // DW
  DW.iOSapi.AVFAudio, DW.iOSapi.CoreML;

const
  SNErrorCodeUnknownError = 1;
  SNErrorCodeOperationFailed = 2;
  SNErrorCodeInvalidFormat = 3;
  SNErrorCodeInvalidModel = 4;
  SNErrorCodeInvalidFile = 5;
  SNTimeDurationConstraintTypeEnumerated = 1;
  SNTimeDurationConstraintTypeRange = 2;

type
  SNRequest = interface;
  SNResult = interface;
  SNResultsObserving = interface;
  SNAudioStreamAnalyzer = interface;
  SNAudioFileAnalyzer = interface;
  SNTimeDurationConstraint = interface;
  SNClassifySoundRequest = interface;
  SNClassification = interface;
  SNClassificationResult = interface;

  SNClassifierIdentifier = NSString;
  SNErrorCode = NSInteger;
  SNTimeDurationConstraintType = NSInteger;
  TSNAudioFileAnalyzerBlockMethod1 = procedure(didReachEndOfFile: Boolean) of object;

  SNRequest = interface(IObjectiveC)
    ['{53494FD6-2D4E-45A1-B638-89EA3CB0166F}']
  end;

  SNResult = interface(IObjectiveC)
    ['{4CAE4A5C-8398-4770-A026-BA463687B23F}']
  end;

  SNResultsObserving = interface(IObjectiveC)
    ['{0571110F-3056-4437-ABEE-BB4CF61A91AF}']
    procedure request(request: Pointer; didFailWithError: NSError); overload; cdecl;
    procedure request(request: Pointer; didProduceResult: Pointer); overload; cdecl;
    procedure requestDidComplete(request: Pointer); cdecl;
  end;

  SNAudioStreamAnalyzerClass = interface(NSObjectClass)
    ['{373CA771-C265-4D94-AE1F-72C18302A877}']
  end;

  SNAudioStreamAnalyzer = interface(NSObject)
    ['{840D7CD5-2AFF-4031-8D84-40E4FE126CA7}']
    function addRequest(request: Pointer; withObserver: Pointer; error: PPointer): Boolean; cdecl;
    procedure analyzeAudioBuffer(audioBuffer: AVAudioBuffer; atAudioFramePosition: AVAudioFramePosition); cdecl;
    procedure completeAnalysis; cdecl;
    function initWithFormat(format: AVAudioFormat): Pointer; cdecl;
    procedure removeAllRequests; cdecl;
    procedure removeRequest(request: Pointer); cdecl;
  end;
  TSNAudioStreamAnalyzer = class(TOCGenericImport<SNAudioStreamAnalyzerClass, SNAudioStreamAnalyzer>) end;

  SNAudioFileAnalyzerClass = interface(NSObjectClass)
    ['{7B0D6415-FE4A-46FB-A1DC-0A52239E2645}']
  end;

  SNAudioFileAnalyzer = interface(NSObject)
    ['{F92462DA-045B-468C-A5C2-E77D6213D37E}']
    function addRequest(request: Pointer; withObserver: Pointer; error: PPointer): Boolean; cdecl;
    procedure analyze; cdecl;
    procedure analyzeWithCompletionHandler(completionHandler: TSNAudioFileAnalyzerBlockMethod1); cdecl;
    procedure cancelAnalysis; cdecl;
    function initWithURL(url: NSURL; error: PPointer): Pointer; cdecl;
    procedure removeAllRequests; cdecl;
    procedure removeRequest(request: Pointer); cdecl;
  end;
  TSNAudioFileAnalyzer = class(TOCGenericImport<SNAudioFileAnalyzerClass, SNAudioFileAnalyzer>) end;

  SNTimeDurationConstraintClass = interface(NSObjectClass)
    ['{540FFC1E-88D8-4B77-BC6C-3D78BC6EAAA5}']
    {class} function new: Pointer; cdecl;
  end;

  SNTimeDurationConstraint = interface(NSObject)
    ['{FFF0EC71-D167-43D4-A8EA-7DB550B0B5F5}']
    function &type: SNTimeDurationConstraintType; cdecl;
    function durationRange: CMTimeRange; cdecl;
    function enumeratedDurations: NSArray; cdecl;
    function initWithDurationRange(durationRange: CMTimeRange): Pointer; cdecl;
    function initWithEnumeratedDurations(enumeratedDurations: NSArray): Pointer; cdecl;
  end;
  TSNTimeDurationConstraint = class(TOCGenericImport<SNTimeDurationConstraintClass, SNTimeDurationConstraint>) end;

  SNClassifySoundRequestClass = interface(NSObjectClass)
    ['{E0BFBBEA-6338-4332-8DD2-858314E35B4B}']
    {class} function new: Pointer; cdecl;
  end;

  SNClassifySoundRequest = interface(NSObject)
    ['{5060F05A-BFC5-4408-A7D6-C11770B6F8E2}']
    function initWithClassifierIdentifier(classifierIdentifier: SNClassifierIdentifier; error: PPointer): Pointer; cdecl;
    function initWithMLModel(mlModel: MLModel; error: PPointer): Pointer; cdecl;
    function knownClassifications: NSArray; cdecl;
    function overlapFactor: Double; cdecl;
    procedure setOverlapFactor(overlapFactor: Double); cdecl;
    procedure setWindowDuration(windowDuration: CMTime); cdecl;
    function windowDuration: CMTime; cdecl;
    function windowDurationConstraint: SNTimeDurationConstraint; cdecl;
  end;
  TSNClassifySoundRequest = class(TOCGenericImport<SNClassifySoundRequestClass, SNClassifySoundRequest>) end;

  SNClassificationClass = interface(NSObjectClass)
    ['{2C46474F-BADB-4EE0-AB3F-F2735372B3AF}']
    {class} function new: Pointer; cdecl;
  end;

  SNClassification = interface(NSObject)
    ['{C331ECD5-47F0-4001-8536-0ABD9B30DF5D}']
    function confidence: Double; cdecl;
    function identifier: NSString; cdecl;
  end;
  TSNClassification = class(TOCGenericImport<SNClassificationClass, SNClassification>) end;

  SNClassificationResultClass = interface(NSObjectClass)
    ['{38ECC7DC-A58E-4938-9FEE-01B679093115}']
    {class} function new: Pointer; cdecl;
  end;

  SNClassificationResult = interface(NSObject)
    ['{55347141-472B-46BD-BF9D-94ED9A6CB8A6}']
    function classificationForIdentifier(identifier: NSString): SNClassification; cdecl;
    function classifications: NSArray; cdecl;
    function timeRange: CMTimeRange; cdecl;
  end;
  TSNClassificationResult = class(TOCGenericImport<SNClassificationResultClass, SNClassificationResult>) end;

function SNClassifierIdentifierVersion1: SNClassifierIdentifier;
function SNErrorDomain: NSString;

const
  libSoundAnalysis = '/System/Library/Frameworks/SoundAnalysis.framework/SoundAnalysis';

implementation

uses
  Posix.Dlfcn;

var
  SoundAnalysisModule: THandle;

function SNClassifierIdentifierVersion1: NSString;
begin
  Result := CocoaNSStringConst(libSoundAnalysis, 'SNClassifierIdentifierVersion1');
end;

function SNErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libSoundAnalysis, 'SNErrorDomain');
end;

initialization
  SoundAnalysisModule := dlopen(MarshaledAString(libSoundAnalysis), RTLD_LAZY);

finalization
  dlclose(SoundAnalysisModule);

end.