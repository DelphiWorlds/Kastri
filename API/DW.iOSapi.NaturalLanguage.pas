unit DW.iOSapi.NaturalLanguage;

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
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.CoreML;

const
  NLDistanceTypeCosine = 0;
  NLTokenUnitWord = 0;
  NLTokenUnitSentence = 1;
  NLTokenUnitParagraph = 2;
  NLTokenUnitDocument = 3;
  NLTokenizerAttributeNumeric = 1;
  NLTokenizerAttributeSymbolic = 2;
  NLTokenizerAttributeEmoji = 4;
  NLContextualEmbeddingAssetsResultAvailable = 0;
  NLContextualEmbeddingAssetsResultNotAvailable = 1;
  NLContextualEmbeddingAssetsResultError = 2;
  NLModelTypeClassifier = 0;
  NLModelTypeSequence = 1;
  NLTaggerOmitWords = 1;
  NLTaggerOmitPunctuation = 2;
  NLTaggerOmitWhitespace = 4;
  NLTaggerOmitOther = 8;
  NLTaggerJoinNames = 16;
  NLTaggerJoinContractions = 32;
  NLTaggerAssetsResultAvailable = 0;
  NLTaggerAssetsResultNotAvailable = 1;
  NLTaggerAssetsResultError = 2;

type
  NLEmbedding = interface;
  NLTokenizer = interface;
  NLContextualEmbedding = interface;
  NLContextualEmbeddingResult = interface;
  NLGazetteer = interface;
  NLLanguageRecognizer = interface;
  NLModel = interface;
  NLModelConfiguration = interface;
  NLTagger = interface;

  PBoolean = ^Boolean;
  NLLanguage = NSString;
  NLDistanceType = NSInteger;
  NLDistance = Double;
  NLScript = NSString;
  NLTokenUnit = NSInteger;
  NLTokenizerAttributes = NSInteger;
  NLContextualEmbeddingKey = NSString;
  NLContextualEmbeddingAssetsResult = NSInteger;
  NLModelType = NSInteger;
  NLTagScheme = NSString;
  NLTag = NSString;
  NLTaggerOptions = NSInteger;
  NLTaggerAssetsResult = NSInteger;
  TNLEmbeddingBlockMethod1 = procedure(neighbor: NSString; distance: NLDistance; stop: PBoolean) of object;
  TNLTokenizerBlockMethod1 = procedure(tokenRange: NSRange; flags: NLTokenizerAttributes; stop: PBoolean) of object;
  TNLContextualEmbeddingBlockMethod1 = procedure(result: NLContextualEmbeddingAssetsResult; error: NSError) of object;
  TNLContextualEmbeddingResultBlockMethod1 = procedure(tokenVector: NSArray; tokenRange: NSRange; stop: PBoolean) of object;
  TNLTaggerBlockMethod1 = procedure(tag: NLTag; tokenRange: NSRange; stop: PBoolean) of object;
  TNLTaggerBlockMethod2 = procedure(result: NLTaggerAssetsResult; error: NSError) of object;

  NLEmbeddingClass = interface(NSObjectClass)
    ['{7B0B1EA6-8C9E-4100-9635-A33B34D54B3D}']
    {class} function currentRevisionForLanguage(language: NLLanguage): NSUInteger; cdecl;
    {class} function currentSentenceEmbeddingRevisionForLanguage(language: NLLanguage): NSUInteger; cdecl;
    {class} function embeddingWithContentsOfURL(url: NSURL; error: PPointer): Pointer; cdecl;
    {class} function sentenceEmbeddingForLanguage(language: NLLanguage; revision: NSUInteger): NLEmbedding; overload; cdecl;
    {class} function sentenceEmbeddingForLanguage(language: NLLanguage): NLEmbedding; overload; cdecl;
    {class} function supportedRevisionsForLanguage(language: NLLanguage): NSIndexSet; cdecl;
    {class} function supportedSentenceEmbeddingRevisionsForLanguage(language: NLLanguage): NSIndexSet; cdecl;
    {class} function wordEmbeddingForLanguage(language: NLLanguage): NLEmbedding; overload; cdecl;
    {class} function wordEmbeddingForLanguage(language: NLLanguage; revision: NSUInteger): NLEmbedding; overload; cdecl;
    {class} function writeEmbeddingForDictionary(dictionary: NSDictionary; language: NLLanguage; revision: NSUInteger; toURL: NSURL;
      error: PPointer): Boolean; cdecl;
  end;

  NLEmbedding = interface(NSObject)
    ['{D4092468-0CA6-4F8D-929E-2A130DB24301}']
    function containsString(&string: NSString): Boolean; cdecl;
    function dimension: NSUInteger; cdecl;
    function distanceBetweenString(firstString: NSString; andString: NSString; distanceType: NLDistanceType): NLDistance; cdecl;
    procedure enumerateNeighborsForString(&string: NSString; maximumCount: NSUInteger; maximumDistance: NLDistance;
      distanceType: NLDistanceType; usingBlock: TNLEmbeddingBlockMethod1); overload; cdecl;
    procedure enumerateNeighborsForString(&string: NSString; maximumCount: NSUInteger; distanceType: NLDistanceType;
      usingBlock: TNLEmbeddingBlockMethod1); overload; cdecl;
    procedure enumerateNeighborsForVector(vector: NSArray; maximumCount: NSUInteger; distanceType: NLDistanceType;
      usingBlock: TNLEmbeddingBlockMethod1); overload; cdecl;
    procedure enumerateNeighborsForVector(vector: NSArray; maximumCount: NSUInteger; maximumDistance: NLDistance; distanceType: NLDistanceType;
      usingBlock: TNLEmbeddingBlockMethod1); overload; cdecl;
    function getVector(vector: PSingle; forString: NSString): Boolean; cdecl;
    function language: NLLanguage; cdecl;
    function neighborsForString(&string: NSString; maximumCount: NSUInteger; distanceType: NLDistanceType): NSArray; overload; cdecl;
    function neighborsForString(&string: NSString; maximumCount: NSUInteger; maximumDistance: NLDistance;
      distanceType: NLDistanceType): NSArray; overload; cdecl;
    function neighborsForVector(vector: NSArray; maximumCount: NSUInteger; maximumDistance: NLDistance;
      distanceType: NLDistanceType): NSArray; overload; cdecl;
    function neighborsForVector(vector: NSArray; maximumCount: NSUInteger; distanceType: NLDistanceType): NSArray; overload; cdecl;
    function revision: NSUInteger; cdecl;
    function vectorForString(&string: NSString): NSArray; cdecl;
    function vocabularySize: NSUInteger; cdecl;
  end;
  TNLEmbedding = class(TOCGenericImport<NLEmbeddingClass, NLEmbedding>) end;

  NLTokenizerClass = interface(NSObjectClass)
    ['{BF4DA6EC-4893-494B-89FC-13FC056A0394}']
  end;

  NLTokenizer = interface(NSObject)
    ['{2447516B-9241-4BC2-9BF6-FECC2616069E}']
    function &string: NSString; cdecl;
    function &unit: NLTokenUnit; cdecl;
    procedure enumerateTokensInRange(range: NSRange; usingBlock: TNLTokenizerBlockMethod1); cdecl;
    function initWithUnit(&unit: NLTokenUnit): Pointer; cdecl;
    procedure setLanguage(language: NLLanguage); cdecl;
    procedure setString(&string: NSString); cdecl;
    function tokenRangeAtIndex(characterIndex: NSUInteger): NSRange; cdecl;
    function tokenRangeForRange(range: NSRange): NSRange; cdecl;
    function tokensForRange(range: NSRange): NSArray; cdecl;
  end;
  TNLTokenizer = class(TOCGenericImport<NLTokenizerClass, NLTokenizer>) end;

  NLContextualEmbeddingClass = interface(NSObjectClass)
    ['{0855454B-0583-45B8-86AA-000C6169A69B}']
    {class} function contextualEmbeddingsForValues(valuesDictionary: NSDictionary): NSArray; cdecl;
    {class} function contextualEmbeddingWithLanguage(language: NLLanguage): NLContextualEmbedding; cdecl;
    {class} function contextualEmbeddingWithModelIdentifier(modelIdentifier: NSString): Pointer; cdecl;
    {class} function contextualEmbeddingWithScript(script: NLScript): NLContextualEmbedding; cdecl;
  end;

  NLContextualEmbedding = interface(NSObject)
    ['{4BC9F694-9A32-4252-A45B-0DD204C05B47}']
    function dimension: NSUInteger; cdecl;
    function embeddingResultForString(&string: NSString; language: NLLanguage; error: PPointer): NLContextualEmbeddingResult; cdecl;
    function hasAvailableAssets: Boolean; cdecl;
    function languages: NSArray; cdecl;
    function loadWithError(error: PPointer): Boolean; cdecl;
    function maximumSequenceLength: NSUInteger; cdecl;
    function modelIdentifier: NSString; cdecl;
    procedure requestEmbeddingAssetsWithCompletionHandler(completionHandler: TNLContextualEmbeddingBlockMethod1); cdecl;
    function revision: NSUInteger; cdecl;
    function scripts: NSArray; cdecl;
    procedure unload; cdecl;
  end;
  TNLContextualEmbedding = class(TOCGenericImport<NLContextualEmbeddingClass, NLContextualEmbedding>) end;

  NLContextualEmbeddingResultClass = interface(NSObjectClass)
    ['{47B730B1-8912-4BEF-AED3-81F690616E49}']
  end;

  NLContextualEmbeddingResult = interface(NSObject)
    ['{E5C52CBE-CF58-4DA4-9A6F-C69EF0FF3971}']
    function &string: NSString; cdecl;
    procedure enumerateTokenVectorsInRange(range: NSRange; usingBlock: TNLContextualEmbeddingResultBlockMethod1); cdecl;
    function language: NLLanguage; cdecl;
    function sequenceLength: NSUInteger; cdecl;
    function tokenVectorAtIndex(characterIndex: NSUInteger; tokenRange: NSRangePointer): NSArray; cdecl;
  end;
  TNLContextualEmbeddingResult = class(TOCGenericImport<NLContextualEmbeddingResultClass, NLContextualEmbeddingResult>) end;

  NLGazetteerClass = interface(NSObjectClass)
    ['{CD39CED0-9CEF-4FC6-9D1B-95DBC7DFDF58}']
    {class} function gazetteerWithContentsOfURL(url: NSURL; error: PPointer): Pointer; cdecl;
    {class} function writeGazetteerForDictionary(dictionary: NSDictionary; language: NLLanguage; toURL: NSURL; error: PPointer): Boolean; cdecl;
  end;

  NLGazetteer = interface(NSObject)
    ['{02A865E5-B9D4-421C-9781-C06A1E2D2200}']
    function data: NSData; cdecl;
    function initWithContentsOfURL(url: NSURL; error: PPointer): Pointer; cdecl;
    function initWithData(data: NSData; error: PPointer): Pointer; cdecl;
    function initWithDictionary(dictionary: NSDictionary; language: NLLanguage; error: PPointer): Pointer; cdecl;
    function labelForString(&string: NSString): NSString; cdecl;
    function language: NLLanguage; cdecl;
  end;
  TNLGazetteer = class(TOCGenericImport<NLGazetteerClass, NLGazetteer>) end;

  NLLanguageRecognizerClass = interface(NSObjectClass)
    ['{A677239D-0869-4CB1-AB75-66B4A2D13B9F}']
    {class} function dominantLanguageForString(&string: NSString): NLLanguage; cdecl;
  end;

  NLLanguageRecognizer = interface(NSObject)
    ['{7FDE36C1-048D-459A-9E6B-1C92E1D81C2B}']
    function dominantLanguage: NLLanguage; cdecl;
    function languageConstraints: NSArray; cdecl;
    function languageHints: NSDictionary; cdecl;
    function languageHypothesesWithMaximum(maxHypotheses: NSUInteger): NSDictionary; cdecl;
    procedure processString(&string: NSString); cdecl;
    procedure reset; cdecl;
    procedure setLanguageConstraints(languageConstraints: NSArray); cdecl;
    procedure setLanguageHints(languageHints: NSDictionary); cdecl;
  end;
  TNLLanguageRecognizer = class(TOCGenericImport<NLLanguageRecognizerClass, NLLanguageRecognizer>) end;

  NLModelClass = interface(NSObjectClass)
    ['{77BC25FC-06C7-463E-9974-383823684170}']
    {class} function modelWithContentsOfURL(url: NSURL; error: PPointer): Pointer; cdecl;
    {class} function modelWithMLModel(mlModel: MLModel; error: PPointer): Pointer; cdecl;
  end;

  NLModel = interface(NSObject)
    ['{E4D2080D-A480-4DAF-A48D-870D8DFE4018}']
    function configuration: NLModelConfiguration; cdecl;
    function predictedLabelForString(&string: NSString): NSString; cdecl;
    function predictedLabelHypothesesForString(&string: NSString; maximumCount: NSUInteger): NSDictionary; cdecl;
    function predictedLabelHypothesesForTokens(tokens: NSArray; maximumCount: NSUInteger): NSArray; cdecl;
    function predictedLabelsForTokens(tokens: NSArray): NSArray; cdecl;
  end;
  TNLModel = class(TOCGenericImport<NLModelClass, NLModel>) end;

  NLModelConfigurationClass = interface(NSObjectClass)
    ['{4E1D1D77-BFB5-4C90-89DD-054524F9D8E3}']
    {class} function currentRevisionForType(&type: NLModelType): NSUInteger; cdecl;
    {class} function supportedRevisionsForType(&type: NLModelType): NSIndexSet; cdecl;
  end;

  NLModelConfiguration = interface(NSObject)
    ['{767C1890-EF7D-431C-8685-83A0662A4E4B}']
    function &type: NLModelType; cdecl;
    function language: NLLanguage; cdecl;
    function revision: NSUInteger; cdecl;
  end;
  TNLModelConfiguration = class(TOCGenericImport<NLModelConfigurationClass, NLModelConfiguration>) end;

  NLTaggerClass = interface(NSObjectClass)
    ['{084353A7-0420-4ADE-A40D-3805A9A4D135}']
    {class} function availableTagSchemesForUnit(&unit: NLTokenUnit; language: NLLanguage): NSArray; cdecl;
    {class} procedure requestAssetsForLanguage(language: NLLanguage; tagScheme: NLTagScheme; completionHandler: TNLTaggerBlockMethod2); cdecl;
  end;

  NLTagger = interface(NSObject)
    ['{691043AD-AB7A-46DE-89E5-A0E014D5898B}']
    function &string: NSString; cdecl;
    function dominantLanguage: NLLanguage; cdecl;
    procedure enumerateTagsInRange(range: NSRange; &unit: NLTokenUnit; scheme: NLTagScheme; options: NLTaggerOptions;
      usingBlock: TNLTaggerBlockMethod1); cdecl;
    function gazetteersForTagScheme(tagScheme: NLTagScheme): NSArray; cdecl;
    function initWithTagSchemes(tagSchemes: NSArray): Pointer; cdecl;
    function modelsForTagScheme(tagScheme: NLTagScheme): NSArray; cdecl;
    procedure setGazetteers(gazetteers: NSArray; forTagScheme: NLTagScheme); cdecl;
    procedure setLanguage(language: NLLanguage; range: NSRange); cdecl;
    procedure setModels(models: NSArray; forTagScheme: NLTagScheme); cdecl;
    procedure setOrthography(orthography: NSOrthography; range: NSRange); cdecl;
    procedure setString(&string: NSString); cdecl;
    function tagAtIndex(characterIndex: NSUInteger; &unit: NLTokenUnit; scheme: NLTagScheme; tokenRange: NSRangePointer): NLTag; cdecl;
    function tagHypothesesAtIndex(characterIndex: NSUInteger; &unit: NLTokenUnit; scheme: NLTagScheme; maximumCount: NSUInteger;
      tokenRange: NSRangePointer): NSDictionary; cdecl;
    function tagSchemes: NSArray; cdecl;
    function tagsInRange(range: NSRange; &unit: NLTokenUnit; scheme: NLTagScheme; options: NLTaggerOptions; tokenRanges: PNSArray): NSArray; cdecl;
    function tokenRangeAtIndex(characterIndex: NSUInteger; &unit: NLTokenUnit): NSRange; cdecl;
    function tokenRangeForRange(range: NSRange; &unit: NLTokenUnit): NSRange; cdecl;
  end;
  TNLTagger = class(TOCGenericImport<NLTaggerClass, NLTagger>) end;

function NLLanguageUndetermined: NLLanguage;
function NLLanguageAmharic: NLLanguage;
function NLLanguageArabic: NLLanguage;
function NLLanguageArmenian: NLLanguage;
function NLLanguageBengali: NLLanguage;
function NLLanguageBulgarian: NLLanguage;
function NLLanguageBurmese: NLLanguage;
function NLLanguageCatalan: NLLanguage;
function NLLanguageCherokee: NLLanguage;
function NLLanguageCroatian: NLLanguage;
function NLLanguageCzech: NLLanguage;
function NLLanguageDanish: NLLanguage;
function NLLanguageDutch: NLLanguage;
function NLLanguageEnglish: NLLanguage;
function NLLanguageFinnish: NLLanguage;
function NLLanguageFrench: NLLanguage;
function NLLanguageGeorgian: NLLanguage;
function NLLanguageGerman: NLLanguage;
function NLLanguageGreek: NLLanguage;
function NLLanguageGujarati: NLLanguage;
function NLLanguageHebrew: NLLanguage;
function NLLanguageHindi: NLLanguage;
function NLLanguageHungarian: NLLanguage;
function NLLanguageIcelandic: NLLanguage;
function NLLanguageIndonesian: NLLanguage;
function NLLanguageItalian: NLLanguage;
function NLLanguageJapanese: NLLanguage;
function NLLanguageKannada: NLLanguage;
function NLLanguageKhmer: NLLanguage;
function NLLanguageKorean: NLLanguage;
function NLLanguageLao: NLLanguage;
function NLLanguageMalay: NLLanguage;
function NLLanguageMalayalam: NLLanguage;
function NLLanguageMarathi: NLLanguage;
function NLLanguageMongolian: NLLanguage;
function NLLanguageNorwegian: NLLanguage;
function NLLanguageOriya: NLLanguage;
function NLLanguagePersian: NLLanguage;
function NLLanguagePolish: NLLanguage;
function NLLanguagePortuguese: NLLanguage;
function NLLanguagePunjabi: NLLanguage;
function NLLanguageRomanian: NLLanguage;
function NLLanguageRussian: NLLanguage;
function NLLanguageSimplifiedChinese: NLLanguage;
function NLLanguageSinhalese: NLLanguage;
function NLLanguageSlovak: NLLanguage;
function NLLanguageSpanish: NLLanguage;
function NLLanguageSwedish: NLLanguage;
function NLLanguageTamil: NLLanguage;
function NLLanguageTelugu: NLLanguage;
function NLLanguageThai: NLLanguage;
function NLLanguageTibetan: NLLanguage;
function NLLanguageTraditionalChinese: NLLanguage;
function NLLanguageTurkish: NLLanguage;
function NLLanguageUkrainian: NLLanguage;
function NLLanguageUrdu: NLLanguage;
function NLLanguageVietnamese: NLLanguage;
function NLLanguageKazakh: NLLanguage;
function NLScriptUndetermined: NLScript;
function NLScriptArabic: NLScript;
function NLScriptArmenian: NLScript;
function NLScriptBengali: NLScript;
function NLScriptCanadianAboriginalSyllabics: NLScript;
function NLScriptCherokee: NLScript;
function NLScriptCyrillic: NLScript;
function NLScriptDevanagari: NLScript;
function NLScriptEthiopic: NLScript;
function NLScriptGeorgian: NLScript;
function NLScriptGreek: NLScript;
function NLScriptGujarati: NLScript;
function NLScriptGurmukhi: NLScript;
function NLScriptHebrew: NLScript;
function NLScriptJapanese: NLScript;
function NLScriptKannada: NLScript;
function NLScriptKhmer: NLScript;
function NLScriptKorean: NLScript;
function NLScriptLao: NLScript;
function NLScriptLatin: NLScript;
function NLScriptMalayalam: NLScript;
function NLScriptMongolian: NLScript;
function NLScriptMyanmar: NLScript;
function NLScriptOriya: NLScript;
function NLScriptSimplifiedChinese: NLScript;
function NLScriptSinhala: NLScript;
function NLScriptTamil: NLScript;
function NLScriptTelugu: NLScript;
function NLScriptThai: NLScript;
function NLScriptTibetan: NLScript;
function NLScriptTraditionalChinese: NLScript;
function NLContextualEmbeddingKeyLanguages: NLContextualEmbeddingKey;
function NLContextualEmbeddingKeyScripts: NLContextualEmbeddingKey;
function NLContextualEmbeddingKeyRevision: NLContextualEmbeddingKey;
function NLTagSchemeTokenType: NLTagScheme;
function NLTagSchemeLexicalClass: NLTagScheme;
function NLTagSchemeNameType: NLTagScheme;
function NLTagSchemeNameTypeOrLexicalClass: NLTagScheme;
function NLTagSchemeLemma: NLTagScheme;
function NLTagSchemeLanguage: NLTagScheme;
function NLTagSchemeScript: NLTagScheme;
function NLTagSchemeSentimentScore: NLTagScheme;
function NLTagWord: NLTag;
function NLTagPunctuation: NLTag;
function NLTagWhitespace: NLTag;
function NLTagOther: NLTag;
function NLTagNoun: NLTag;
function NLTagVerb: NLTag;
function NLTagAdjective: NLTag;
function NLTagAdverb: NLTag;
function NLTagPronoun: NLTag;
function NLTagDeterminer: NLTag;
function NLTagParticle: NLTag;
function NLTagPreposition: NLTag;
function NLTagNumber: NLTag;
function NLTagConjunction: NLTag;
function NLTagInterjection: NLTag;
function NLTagClassifier: NLTag;
function NLTagIdiom: NLTag;
function NLTagOtherWord: NLTag;
function NLTagSentenceTerminator: NLTag;
function NLTagOpenQuote: NLTag;
function NLTagCloseQuote: NLTag;
function NLTagOpenParenthesis: NLTag;
function NLTagCloseParenthesis: NLTag;
function NLTagWordJoiner: NLTag;
function NLTagDash: NLTag;
function NLTagOtherPunctuation: NLTag;
function NLTagParagraphBreak: NLTag;
function NLTagOtherWhitespace: NLTag;
function NLTagPersonalName: NLTag;
function NLTagPlaceName: NLTag;
function NLTagOrganizationName: NLTag;

const
  libNaturalLanguage = '/System/Library/Frameworks/NaturalLanguage.framework/NaturalLanguage';

implementation

uses
  Posix.Dlfcn;

var
  NaturalLanguageModule: THandle;

function NLLanguageUndetermined: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageUndetermined');
end;

function NLLanguageAmharic: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageAmharic');
end;

function NLLanguageArabic: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageArabic');
end;

function NLLanguageArmenian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageArmenian');
end;

function NLLanguageBengali: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageBengali');
end;

function NLLanguageBulgarian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageBulgarian');
end;

function NLLanguageBurmese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageBurmese');
end;

function NLLanguageCatalan: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageCatalan');
end;

function NLLanguageCherokee: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageCherokee');
end;

function NLLanguageCroatian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageCroatian');
end;

function NLLanguageCzech: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageCzech');
end;

function NLLanguageDanish: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageDanish');
end;

function NLLanguageDutch: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageDutch');
end;

function NLLanguageEnglish: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageEnglish');
end;

function NLLanguageFinnish: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageFinnish');
end;

function NLLanguageFrench: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageFrench');
end;

function NLLanguageGeorgian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageGeorgian');
end;

function NLLanguageGerman: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageGerman');
end;

function NLLanguageGreek: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageGreek');
end;

function NLLanguageGujarati: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageGujarati');
end;

function NLLanguageHebrew: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageHebrew');
end;

function NLLanguageHindi: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageHindi');
end;

function NLLanguageHungarian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageHungarian');
end;

function NLLanguageIcelandic: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageIcelandic');
end;

function NLLanguageIndonesian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageIndonesian');
end;

function NLLanguageItalian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageItalian');
end;

function NLLanguageJapanese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageJapanese');
end;

function NLLanguageKannada: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageKannada');
end;

function NLLanguageKhmer: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageKhmer');
end;

function NLLanguageKorean: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageKorean');
end;

function NLLanguageLao: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageLao');
end;

function NLLanguageMalay: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageMalay');
end;

function NLLanguageMalayalam: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageMalayalam');
end;

function NLLanguageMarathi: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageMarathi');
end;

function NLLanguageMongolian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageMongolian');
end;

function NLLanguageNorwegian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageNorwegian');
end;

function NLLanguageOriya: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageOriya');
end;

function NLLanguagePersian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguagePersian');
end;

function NLLanguagePolish: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguagePolish');
end;

function NLLanguagePortuguese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguagePortuguese');
end;

function NLLanguagePunjabi: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguagePunjabi');
end;

function NLLanguageRomanian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageRomanian');
end;

function NLLanguageRussian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageRussian');
end;

function NLLanguageSimplifiedChinese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageSimplifiedChinese');
end;

function NLLanguageSinhalese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageSinhalese');
end;

function NLLanguageSlovak: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageSlovak');
end;

function NLLanguageSpanish: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageSpanish');
end;

function NLLanguageSwedish: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageSwedish');
end;

function NLLanguageTamil: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageTamil');
end;

function NLLanguageTelugu: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageTelugu');
end;

function NLLanguageThai: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageThai');
end;

function NLLanguageTibetan: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageTibetan');
end;

function NLLanguageTraditionalChinese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageTraditionalChinese');
end;

function NLLanguageTurkish: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageTurkish');
end;

function NLLanguageUkrainian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageUkrainian');
end;

function NLLanguageUrdu: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageUrdu');
end;

function NLLanguageVietnamese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageVietnamese');
end;

function NLLanguageKazakh: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLLanguageKazakh');
end;

function NLScriptUndetermined: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptUndetermined');
end;

function NLScriptArabic: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptArabic');
end;

function NLScriptArmenian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptArmenian');
end;

function NLScriptBengali: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptBengali');
end;

function NLScriptCanadianAboriginalSyllabics: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptCanadianAboriginalSyllabics');
end;

function NLScriptCherokee: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptCherokee');
end;

function NLScriptCyrillic: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptCyrillic');
end;

function NLScriptDevanagari: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptDevanagari');
end;

function NLScriptEthiopic: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptEthiopic');
end;

function NLScriptGeorgian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptGeorgian');
end;

function NLScriptGreek: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptGreek');
end;

function NLScriptGujarati: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptGujarati');
end;

function NLScriptGurmukhi: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptGurmukhi');
end;

function NLScriptHebrew: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptHebrew');
end;

function NLScriptJapanese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptJapanese');
end;

function NLScriptKannada: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptKannada');
end;

function NLScriptKhmer: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptKhmer');
end;

function NLScriptKorean: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptKorean');
end;

function NLScriptLao: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptLao');
end;

function NLScriptLatin: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptLatin');
end;

function NLScriptMalayalam: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptMalayalam');
end;

function NLScriptMongolian: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptMongolian');
end;

function NLScriptMyanmar: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptMyanmar');
end;

function NLScriptOriya: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptOriya');
end;

function NLScriptSimplifiedChinese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptSimplifiedChinese');
end;

function NLScriptSinhala: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptSinhala');
end;

function NLScriptTamil: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptTamil');
end;

function NLScriptTelugu: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptTelugu');
end;

function NLScriptThai: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptThai');
end;

function NLScriptTibetan: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptTibetan');
end;

function NLScriptTraditionalChinese: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLScriptTraditionalChinese');
end;

function NLContextualEmbeddingKeyLanguages: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLContextualEmbeddingKeyLanguages');
end;

function NLContextualEmbeddingKeyScripts: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLContextualEmbeddingKeyScripts');
end;

function NLContextualEmbeddingKeyRevision: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLContextualEmbeddingKeyRevision');
end;

function NLTagSchemeTokenType: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSchemeTokenType');
end;

function NLTagSchemeLexicalClass: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSchemeLexicalClass');
end;

function NLTagSchemeNameType: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSchemeNameType');
end;

function NLTagSchemeNameTypeOrLexicalClass: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSchemeNameTypeOrLexicalClass');
end;

function NLTagSchemeLemma: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSchemeLemma');
end;

function NLTagSchemeLanguage: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSchemeLanguage');
end;

function NLTagSchemeScript: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSchemeScript');
end;

function NLTagSchemeSentimentScore: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSchemeSentimentScore');
end;

function NLTagWord: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagWord');
end;

function NLTagPunctuation: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagPunctuation');
end;

function NLTagWhitespace: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagWhitespace');
end;

function NLTagOther: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagOther');
end;

function NLTagNoun: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagNoun');
end;

function NLTagVerb: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagVerb');
end;

function NLTagAdjective: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagAdjective');
end;

function NLTagAdverb: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagAdverb');
end;

function NLTagPronoun: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagPronoun');
end;

function NLTagDeterminer: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagDeterminer');
end;

function NLTagParticle: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagParticle');
end;

function NLTagPreposition: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagPreposition');
end;

function NLTagNumber: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagNumber');
end;

function NLTagConjunction: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagConjunction');
end;

function NLTagInterjection: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagInterjection');
end;

function NLTagClassifier: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagClassifier');
end;

function NLTagIdiom: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagIdiom');
end;

function NLTagOtherWord: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagOtherWord');
end;

function NLTagSentenceTerminator: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagSentenceTerminator');
end;

function NLTagOpenQuote: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagOpenQuote');
end;

function NLTagCloseQuote: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagCloseQuote');
end;

function NLTagOpenParenthesis: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagOpenParenthesis');
end;

function NLTagCloseParenthesis: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagCloseParenthesis');
end;

function NLTagWordJoiner: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagWordJoiner');
end;

function NLTagDash: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagDash');
end;

function NLTagOtherPunctuation: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagOtherPunctuation');
end;

function NLTagParagraphBreak: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagParagraphBreak');
end;

function NLTagOtherWhitespace: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagOtherWhitespace');
end;

function NLTagPersonalName: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagPersonalName');
end;

function NLTagPlaceName: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagPlaceName');
end;

function NLTagOrganizationName: NSString;
begin
  Result := CocoaNSStringConst(libNaturalLanguage, 'NLTagOrganizationName');
end;

initialization
  NaturalLanguageModule := dlopen(MarshaledAString(libNaturalLanguage), RTLD_LAZY);

finalization
  dlclose(NaturalLanguageModule);

end.