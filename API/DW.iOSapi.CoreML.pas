unit DW.iOSapi.CoreML;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.CoreVideo,
  // DW;
  DW.iOSapi.Foundation;

const
  MLFeatureTypeInvalid = 0;
  MLFeatureTypeInt64 = 1;
  MLFeatureTypeDouble = 2;
  MLFeatureTypeString = 3;
  MLFeatureTypeImage = 4;
  MLFeatureTypeMultiArray = 5;
  MLFeatureTypeDictionary = 6;
  MLFeatureTypeSequence = 7;
  MLMultiArrayDataTypeDouble = 65600;
  MLMultiArrayDataTypeFloat64 = 65600;
  MLMultiArrayDataTypeFloat32 = 65568;
  MLMultiArrayDataTypeFloat = 65568;
  MLMultiArrayDataTypeInt32 = 131104;
  MLImageSizeConstraintTypeUnspecified = 0;
  MLImageSizeConstraintTypeEnumerated = 2;
  MLImageSizeConstraintTypeRange = 3;
  MLMultiArrayShapeConstraintTypeUnspecified = 1;
  MLMultiArrayShapeConstraintTypeEnumerated = 2;
  MLMultiArrayShapeConstraintTypeRange = 3;
  MLComputeUnitsCPUOnly = 0;
  MLComputeUnitsCPUAndGPU = 1;
  MLComputeUnitsAll = 2;
  MLModelErrorGeneric = 0;
  MLModelErrorFeatureType = 1;
  MLModelErrorIO = 3;
  MLModelErrorCustomLayer = 4;
  MLModelErrorCustomModel = 5;
  MLModelErrorUpdate = 6;
  MLModelErrorParameters = 7;
  MLModelErrorModelDecryptionKeyFetch = 8;
  MLModelErrorModelDecryption = 9;
  MLModelErrorModelCollection = 10;
  MLTaskStateSuspended = 1;
  MLTaskStateRunning = 2;
  MLTaskStateCancelling = 3;
  MLTaskStateCompleted = 4;
  MLTaskStateFailed = 5;
  MLUpdateProgressEventTrainingBegin = 1;
  MLUpdateProgressEventEpochEnd = 2;
  MLUpdateProgressEventMiniBatchEnd = 4;

type
  MLMultiArray = interface;
  MLSequence = interface;
  MLFeatureValue = interface;
  MLImageSize = interface;
  MLImageSizeConstraint = interface;
  MLImageConstraint = interface;
  MLDictionaryConstraint = interface;
  MLMultiArrayShapeConstraint = interface;
  MLMultiArrayConstraint = interface;
  MLSequenceConstraint = interface;
  MLFeatureDescription = interface;
  MLFeatureProvider = interface;
  MLDictionaryFeatureProvider = interface;
  MLBatchProvider = interface;
  MLArrayBatchProvider = interface;
  MLModelDescription = interface;
  MLPredictionOptions = interface;
  MLModelConfiguration = interface;
  MLModel = interface;
  MLCustomLayer = interface;
  MLCustomModel = interface;
  MLKey = interface;
  MLTask = interface;
  MLUpdateTask = interface;
  MLWritable = interface;
  MLUpdateContext = interface;
  MLUpdateProgressHandlers = interface;
  MLMetricKey = interface;
  MLNumericConstraint = interface;
  MLParameterDescription = interface;
  MLParameterKey = interface;
  MLModelCollectionEntry = interface;
  MLModelCollection = interface;

  MLFeatureType = NSInteger;
  MLMultiArrayDataType = NSInteger;
  MLImageSizeConstraintType = NSInteger;
  MLFeatureValueImageOption = NSString;
  MLMultiArrayShapeConstraintType = NSInteger;
  MLModelMetadataKey = NSString;
  MLComputeUnits = NSInteger;
  MLModelError = NSInteger;
  MLTaskState = NSInteger;
  MLUpdateProgressEvent = NSInteger;
  CGImagePropertyOrientation = NSUInteger;

  TMLMultiArrayBlockMethod1 = procedure(bytes: Pointer) of object;
  TMLModelBlockMethod1 = procedure(model: MLModel; error: NSError) of object;
  TMLUpdateTaskBlockMethod1 = procedure(context: MLUpdateContext) of object;
  TMLUpdateTaskBlockMethod2 = procedure(param1: MLUpdateContext) of object;
  TMLUpdateProgressHandlersBlockMethod1 = procedure(context: MLUpdateContext) of object;
  TMLModelCollectionBlockMethod1 = procedure(modelCollection: MLModelCollection; error: NSError) of object;
  TMLModelCollectionBlockMethod2 = procedure(success: Boolean; error: NSError) of object;

  MLMultiArrayClass = interface(NSObjectClass)
    ['{5983F165-599C-4082-B547-427753C625F5}']
    {class} function multiArrayByConcatenatingMultiArrays(multiArrays: NSArray; alongAxis: NSInteger; dataType: MLMultiArrayDataType): Pointer; cdecl;
  end;

  MLMultiArray = interface(NSObject)
    ['{BA2EF58A-115B-4A04-8D5C-A397E6B09FF3}']
    function count: NSInteger; cdecl;
    function dataPointer: Pointer; cdecl;
    function dataType: MLMultiArrayDataType; cdecl;
    function initWithDataPointer(dataPointer: Pointer; shape: NSArray; dataType: MLMultiArrayDataType; strides: NSArray;
      deallocator: TMLMultiArrayBlockMethod1; error: PPointer): Pointer; cdecl;
    function initWithShape(shape: NSArray; dataType: MLMultiArrayDataType; error: PPointer): Pointer; cdecl;
    function objectAtIndexedSubscript(idx: NSInteger): NSNumber; cdecl;
    function objectForKeyedSubscript(key: NSArray): NSNumber; cdecl;
    procedure setObject(obj: NSNumber; forKeyedSubscript: NSArray); overload; cdecl;
    procedure setObject(obj: NSNumber; atIndexedSubscript: NSInteger); overload; cdecl;
    function shape: NSArray; cdecl;
    function strides: NSArray; cdecl;
  end;
  TMLMultiArray = class(TOCGenericImport<MLMultiArrayClass, MLMultiArray>) end;

  MLSequenceClass = interface(NSObjectClass)
    ['{68B42CDF-5ADC-4850-BD4F-2AD4BF97B4A1}']
    {class} function emptySequenceWithType(&type: MLFeatureType): Pointer; cdecl;
    {class} function sequenceWithInt64Array(int64Values: NSArray): Pointer; cdecl;
    {class} function sequenceWithStringArray(stringValues: NSArray): Pointer; cdecl;
  end;

  MLSequence = interface(NSObject)
    ['{8A7ADC86-D29F-48CD-B990-49F286CD079D}']
    [MethodName('type')]
    function &type: MLFeatureType; cdecl;
    function int64Values: NSArray; cdecl;
    function stringValues: NSArray; cdecl;
  end;
  TMLSequence = class(TOCGenericImport<MLSequenceClass, MLSequence>) end;

  MLFeatureValueClass = interface(NSObjectClass)
    ['{7DA14AA8-9E31-4368-9DD2-7DA9FEBA0A5F}']
    {class} function featureValueWithCGImage(cgImage: CGImageRef; pixelsWide: NSInteger; pixelsHigh: NSInteger; pixelFormatType: OSType;
      options: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    {class} function featureValueWithCGImage(cgImage: CGImageRef; constraint: MLImageConstraint; options: NSDictionary;
      error: PPointer): Pointer; overload; cdecl;
    {class} function featureValueWithCGImage(cgImage: CGImageRef; orientation: CGImagePropertyOrientation; constraint: MLImageConstraint;
      options: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    {class} function featureValueWithCGImage(cgImage: CGImageRef; orientation: CGImagePropertyOrientation; pixelsWide: NSInteger;
      pixelsHigh: NSInteger; pixelFormatType: OSType; options: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    {class} function featureValueWithDictionary(value: NSDictionary; error: PPointer): Pointer; cdecl;
    {class} function featureValueWithDouble(value: Double): Pointer; cdecl;
    {class} function featureValueWithImageAtURL(url: NSURL; orientation: CGImagePropertyOrientation; pixelsWide: NSInteger; pixelsHigh: NSInteger;
      pixelFormatType: OSType; options: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    {class} function featureValueWithImageAtURL(url: NSURL; constraint: MLImageConstraint; options: NSDictionary;
      error: PPointer): Pointer; overload; cdecl;
    {class} function featureValueWithImageAtURL(url: NSURL; pixelsWide: NSInteger; pixelsHigh: NSInteger; pixelFormatType: OSType;
      options: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    {class} function featureValueWithImageAtURL(url: NSURL; orientation: CGImagePropertyOrientation; constraint: MLImageConstraint;
      options: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    {class} function featureValueWithInt64(value: Int64): Pointer; cdecl;
    {class} function featureValueWithMultiArray(value: MLMultiArray): Pointer; cdecl;
    {class} function featureValueWithPixelBuffer(value: CVPixelBufferRef): Pointer; cdecl;
    {class} function featureValueWithSequence(sequence: MLSequence): Pointer; cdecl;
    {class} function featureValueWithString(value: NSString): Pointer; cdecl;
    {class} function undefinedFeatureValueWithType(&type: MLFeatureType): Pointer; cdecl;
  end;

  MLFeatureValue = interface(NSObject)
    ['{CFCCDC67-73B6-4CC4-8F48-FBB6084F1D3A}']
    [MethodName('type')]
    function &type: MLFeatureType; cdecl;
    function dictionaryValue: NSDictionary; cdecl;
    function doubleValue: Double; cdecl;
    function imageBufferValue: CVPixelBufferRef; cdecl;
    function int64Value: Int64; cdecl;
    function isEqualToFeatureValue(value: MLFeatureValue): Boolean; cdecl;
    function isUndefined: Boolean; cdecl;
    function multiArrayValue: MLMultiArray; cdecl;
    function sequenceValue: MLSequence; cdecl;
    function stringValue: NSString; cdecl;
  end;
  TMLFeatureValue = class(TOCGenericImport<MLFeatureValueClass, MLFeatureValue>) end;

  MLImageSizeClass = interface(NSObjectClass)
    ['{CE66B6B0-AAE9-4C6B-A870-EA2F9F2859C5}']
  end;

  MLImageSize = interface(NSObject)
    ['{02F3FC68-1944-4927-AF82-7BA650C010E6}']
    function pixelsHigh: NSInteger; cdecl;
    function pixelsWide: NSInteger; cdecl;
  end;
  TMLImageSize = class(TOCGenericImport<MLImageSizeClass, MLImageSize>) end;

  MLImageSizeConstraintClass = interface(NSObjectClass)
    ['{45DFEF0A-16FD-4E2A-8625-A8D890261BC6}']
  end;

  MLImageSizeConstraint = interface(NSObject)
    ['{F6EACD78-528B-460A-9E84-8288FEC89D1F}']
    [MethodName('type')]
    function &type: MLImageSizeConstraintType; cdecl;
    function enumeratedImageSizes: NSArray; cdecl;
    function pixelsHighRange: NSRange; cdecl;
    function pixelsWideRange: NSRange; cdecl;
  end;
  TMLImageSizeConstraint = class(TOCGenericImport<MLImageSizeConstraintClass, MLImageSizeConstraint>) end;

  MLImageConstraintClass = interface(NSObjectClass)
    ['{D93631D7-75EE-4811-AD6B-C193E6C3D109}']
  end;

  MLImageConstraint = interface(NSObject)
    ['{88A68F86-AFD1-4913-8B30-4AD89CAB60B5}']
    function pixelFormatType: OSType; cdecl;
    function pixelsHigh: NSInteger; cdecl;
    function pixelsWide: NSInteger; cdecl;
    function sizeConstraint: MLImageSizeConstraint; cdecl;
  end;
  TMLImageConstraint = class(TOCGenericImport<MLImageConstraintClass, MLImageConstraint>) end;

  MLDictionaryConstraintClass = interface(NSObjectClass)
    ['{DE77501D-D2B9-4A4F-9F7F-F2947EF514C1}']
  end;

  MLDictionaryConstraint = interface(NSObject)
    ['{9DE55ECF-100F-4A11-9C68-66E200AFA10F}']
    function keyType: MLFeatureType; cdecl;
  end;
  TMLDictionaryConstraint = class(TOCGenericImport<MLDictionaryConstraintClass, MLDictionaryConstraint>) end;

  MLMultiArrayShapeConstraintClass = interface(NSObjectClass)
    ['{F86D0762-84AC-462C-B03D-467F504F2A62}']
  end;

  MLMultiArrayShapeConstraint = interface(NSObject)
    ['{AD14E81F-4CC0-4874-8582-4E349E5A017D}']
    [MethodName('type')]
    function &type: MLMultiArrayShapeConstraintType; cdecl;
    function enumeratedShapes: NSArray; cdecl;
    function sizeRangeForDimension: NSArray; cdecl;
  end;
  TMLMultiArrayShapeConstraint = class(TOCGenericImport<MLMultiArrayShapeConstraintClass, MLMultiArrayShapeConstraint>) end;

  MLMultiArrayConstraintClass = interface(NSObjectClass)
    ['{16D0A721-0C1D-49D7-9752-775530FC70F2}']
  end;

  MLMultiArrayConstraint = interface(NSObject)
    ['{C7F157C4-5542-4A76-ADB4-F2299674F3F4}']
    function dataType: MLMultiArrayDataType; cdecl;
    function shape: NSArray; cdecl;
    function shapeConstraint: MLMultiArrayShapeConstraint; cdecl;
  end;
  TMLMultiArrayConstraint = class(TOCGenericImport<MLMultiArrayConstraintClass, MLMultiArrayConstraint>) end;

  MLSequenceConstraintClass = interface(NSObjectClass)
    ['{515593D2-264E-440D-A94F-1B451456729C}']
  end;

  MLSequenceConstraint = interface(NSObject)
    ['{84E20EB0-023D-4903-9462-C21467B522D2}']
    function countRange: NSRange; cdecl;
    function valueDescription: MLFeatureDescription; cdecl;
  end;
  TMLSequenceConstraint = class(TOCGenericImport<MLSequenceConstraintClass, MLSequenceConstraint>) end;

  MLFeatureDescriptionClass = interface(NSObjectClass)
    ['{28818D57-B894-4752-B13E-50BB5B6F8E8A}']
  end;

  MLFeatureDescription = interface(NSObject)
    ['{685A7177-B8D6-4757-81DC-47D5A9410BC3}']
    [MethodName('type')]
    function &type: MLFeatureType; cdecl;
    function dictionaryConstraint: MLDictionaryConstraint; cdecl;
    function imageConstraint: MLImageConstraint; cdecl;
    function isAllowedValue(value: MLFeatureValue): Boolean; cdecl;
    function isOptional: Boolean; cdecl;
    function multiArrayConstraint: MLMultiArrayConstraint; cdecl;
    function name: NSString; cdecl;
    function sequenceConstraint: MLSequenceConstraint; cdecl;
  end;
  TMLFeatureDescription = class(TOCGenericImport<MLFeatureDescriptionClass, MLFeatureDescription>) end;

  MLFeatureProvider = interface(IObjectiveC)
    ['{DA93F9C7-723F-4F7A-ADB8-A2A629B5B5C5}']
    function featureNames: NSSet; cdecl;
    function featureValueForName(featureName: NSString): MLFeatureValue; cdecl;
  end;

  MLDictionaryFeatureProviderClass = interface(NSObjectClass)
    ['{CF766FA4-2B89-4FDF-ACA9-32779BF79116}']
  end;

  MLDictionaryFeatureProvider = interface(NSObject)
    ['{C3EE0436-58EB-48A2-A571-2A7011EC04FB}']
    function dictionary: NSDictionary; cdecl;
    function initWithDictionary(dictionary: NSDictionary; error: PPointer): Pointer; cdecl;
    function objectForKeyedSubscript(featureName: NSString): MLFeatureValue; cdecl;
  end;
  TMLDictionaryFeatureProvider = class(TOCGenericImport<MLDictionaryFeatureProviderClass, MLDictionaryFeatureProvider>) end;

  MLBatchProvider = interface(IObjectiveC)
    ['{EA14EF9D-DB15-4A46-B124-C9996665C94B}']
    function count: NSInteger; cdecl;
    function featuresAtIndex(index: NSInteger): Pointer; cdecl;
  end;

  MLArrayBatchProviderClass = interface(NSObjectClass)
    ['{FEBFF9F9-A0E8-48F7-91A5-B0C54D14F31C}']
  end;

  MLArrayBatchProvider = interface(NSObject)
    ['{3FF803CA-1721-4124-A085-C8AC9F518B81}']
    [MethodName('array')]
    function &array: NSArray; cdecl;
    function initWithDictionary(dictionary: NSDictionary; error: PPointer): Pointer; cdecl;
    function initWithFeatureProviderArray(&array: NSArray): Pointer; cdecl;
  end;
  TMLArrayBatchProvider = class(TOCGenericImport<MLArrayBatchProviderClass, MLArrayBatchProvider>) end;

  MLModelDescriptionClass = interface(NSObjectClass)
    ['{2F942DD1-E3A8-4037-8693-31749F2EE006}']
  end;

  MLModelDescription = interface(NSObject)
    ['{8CE907D2-5785-4F71-AB3E-38905CC777CD}']
    function classLabels: NSArray; cdecl;
    function inputDescriptionsByName: NSDictionary; cdecl;
    function isUpdatable: Boolean; cdecl;
    function metadata: NSDictionary; cdecl;
    function outputDescriptionsByName: NSDictionary; cdecl;
    function parameterDescriptionsByKey: NSDictionary; cdecl;
    function predictedFeatureName: NSString; cdecl;
    function predictedProbabilitiesName: NSString; cdecl;
    function trainingInputDescriptionsByName: NSDictionary; cdecl;
  end;
  TMLModelDescription = class(TOCGenericImport<MLModelDescriptionClass, MLModelDescription>) end;

  MLPredictionOptionsClass = interface(NSObjectClass)
    ['{61D079A4-2DD2-4E64-9728-3EBE93E96CA7}']
  end;

  MLPredictionOptions = interface(NSObject)
    ['{B60D84F1-8A13-49EB-A617-81991A8E0184}']
    procedure setUsesCPUOnly(usesCPUOnly: Boolean); cdecl;
    function usesCPUOnly: Boolean; cdecl;
  end;
  TMLPredictionOptions = class(TOCGenericImport<MLPredictionOptionsClass, MLPredictionOptions>) end;

  MLModelConfigurationClass = interface(NSObjectClass)
    ['{09FBC8A7-E1A1-420A-B593-7429074855B9}']
  end;

  MLModelConfiguration = interface(NSObject)
    ['{EABC14E9-0737-4330-A6C2-3F175D4061DE}']
    function allowLowPrecisionAccumulationOnGPU: Boolean; cdecl;
    function computeUnits: MLComputeUnits; cdecl;
    function parameters: NSDictionary; cdecl;
    function preferredMetalDevice: Pointer; cdecl;
    procedure setAllowLowPrecisionAccumulationOnGPU(allowLowPrecisionAccumulationOnGPU: Boolean); cdecl;
    procedure setComputeUnits(computeUnits: MLComputeUnits); cdecl;
    procedure setParameters(parameters: NSDictionary); cdecl;
    procedure setPreferredMetalDevice(preferredMetalDevice: Pointer); cdecl;
  end;
  TMLModelConfiguration = class(TOCGenericImport<MLModelConfigurationClass, MLModelConfiguration>) end;

  MLModelClass = interface(NSObjectClass)
    ['{F641CDD7-8BF8-4234-A238-510D7896D723}']
    {class} function compileModelAtURL(modelURL: NSURL; error: PPointer): NSURL; cdecl;
    {class} procedure loadContentsOfURL(url: NSURL; configuration: MLModelConfiguration; completionHandler: TMLModelBlockMethod1); cdecl;
    {class} function modelWithContentsOfURL(url: NSURL; configuration: MLModelConfiguration; error: PPointer): Pointer; overload; cdecl;
    {class} function modelWithContentsOfURL(url: NSURL; error: PPointer): Pointer; overload; cdecl;
  end;

  MLModel = interface(NSObject)
    ['{64867500-CAE3-4950-8241-B32045A8C3CB}']
    function configuration: MLModelConfiguration; cdecl;
    function modelDescription: MLModelDescription; cdecl;
    function parameterValueForKey(key: MLParameterKey; error: PPointer): Pointer; cdecl;
    function predictionFromFeatures(input: Pointer; error: PPointer): Pointer; overload; cdecl;
    function predictionFromFeatures(input: Pointer; options: MLPredictionOptions; error: PPointer): Pointer; overload; cdecl;
    function predictionsFromBatch(inputBatch: Pointer; options: MLPredictionOptions; error: PPointer): Pointer; overload; cdecl;
    function predictionsFromBatch(inputBatch: Pointer; error: PPointer): Pointer; overload; cdecl;
  end;
  TMLModel = class(TOCGenericImport<MLModelClass, MLModel>) end;

  MLCustomLayer = interface(IObjectiveC)
    ['{50F3340C-2096-4A88-8972-C786FAF208AD}']
    function encodeToCommandBuffer(commandBuffer: Pointer; inputs: NSArray; outputs: NSArray; error: PPointer): Boolean; cdecl;
    function evaluateOnCPUWithInputs(inputs: NSArray; outputs: NSArray; error: PPointer): Boolean; cdecl;
    function initWithParameterDictionary(parameters: NSDictionary; error: PPointer): Pointer; cdecl;
    function outputShapesForInputShapes(inputShapes: NSArray; error: PPointer): NSArray; cdecl;
    function setWeightData(weights: NSArray; error: PPointer): Boolean; cdecl;
  end;

  MLCustomModel = interface(IObjectiveC)
    ['{D5C8EF93-DE2C-427A-8EC1-B599A896168D}']
    function initWithModelDescription(modelDescription: MLModelDescription; parameterDictionary: NSDictionary; error: PPointer): Pointer; cdecl;
    function predictionFromFeatures(input: Pointer; options: MLPredictionOptions; error: PPointer): Pointer; cdecl;
    function predictionsFromBatch(inputBatch: Pointer; options: MLPredictionOptions; error: PPointer): Pointer; cdecl;
  end;

  MLKeyClass = interface(NSObjectClass)
    ['{6E5FE12D-902C-4070-9B9D-1D64924A01E3}']
    {class} function new: Pointer; cdecl;
  end;

  MLKey = interface(NSObject)
    ['{D95461D6-BEED-476C-8808-144D465DA536}']
    function name: NSString; cdecl;
    function scope: NSString; cdecl;
  end;
  TMLKey = class(TOCGenericImport<MLKeyClass, MLKey>) end;

  MLTaskClass = interface(NSObjectClass)
    ['{A244070E-8794-4B40-9734-8C70F68D0EF9}']
    {class} function new: Pointer; cdecl;
  end;

  MLTask = interface(NSObject)
    ['{FB4DCD6F-9479-4A17-91B2-7E5766C48C01}']
    procedure cancel; cdecl;
    function error: NSError; cdecl;
    procedure resume; cdecl;
    function state: MLTaskState; cdecl;
    function taskIdentifier: NSString; cdecl;
  end;
  TMLTask = class(TOCGenericImport<MLTaskClass, MLTask>) end;

  MLUpdateTaskClass = interface(MLTaskClass)
    ['{5110C8B4-A22F-44F1-AE8F-D1829DD0A590}']
    {class} function new: Pointer; cdecl;
    {class} function updateTaskForModelAtURL(modelURL: NSURL; trainingData: Pointer; progressHandlers: MLUpdateProgressHandlers;
      error: PPointer): Pointer; overload; cdecl;
    {class} function updateTaskForModelAtURL(modelURL: NSURL; trainingData: Pointer; configuration: MLModelConfiguration;
      progressHandlers: MLUpdateProgressHandlers; error: PPointer): Pointer; overload; cdecl;
    {class} function updateTaskForModelAtURL(modelURL: NSURL; trainingData: Pointer; completionHandler: TMLUpdateTaskBlockMethod2;
      error: PPointer): Pointer; overload; cdecl;
    {class} function updateTaskForModelAtURL(modelURL: NSURL; trainingData: Pointer; configuration: MLModelConfiguration;
      completionHandler: TMLUpdateTaskBlockMethod1; error: PPointer): Pointer; overload; cdecl;
  end;

  MLUpdateTask = interface(MLTask)
    ['{DE320707-7C68-42AE-90BF-9A5E6D1F761F}']
    procedure resumeWithParameters(updateParameters: NSDictionary); cdecl;
  end;
  TMLUpdateTask = class(TOCGenericImport<MLUpdateTaskClass, MLUpdateTask>) end;

  MLWritable = interface(IObjectiveC)
    ['{558AFBB7-9758-440A-92CD-31A011793F67}']
    function writeToURL(url: NSURL; error: PPointer): Boolean; cdecl;
  end;

  MLUpdateContextClass = interface(NSObjectClass)
    ['{67418965-6359-4866-96F8-3C1F3D01C074}']
  end;

  MLUpdateContext = interface(NSObject)
    ['{48382212-A4B5-4602-B192-525BE873FEC9}']
    function event: MLUpdateProgressEvent; cdecl;
    function metrics: NSDictionary; cdecl;
    function model: MLModel; cdecl;
    function parameters: NSDictionary; cdecl;
    function task: MLUpdateTask; cdecl;
  end;
  TMLUpdateContext = class(TOCGenericImport<MLUpdateContextClass, MLUpdateContext>) end;

  MLUpdateProgressHandlersClass = interface(NSObjectClass)
    ['{08BC2509-64A0-4CF1-904E-58423488C804}']
    {class} function new: Pointer; cdecl;
  end;

  MLUpdateProgressHandlers = interface(NSObject)
    ['{193DF1EE-A06C-48D9-A16B-AAAACF56EA52}']
    function initForEvents(interestedEvents: MLUpdateProgressEvent; progressHandler: TMLUpdateProgressHandlersBlockMethod1;
      completionHandler: TMLUpdateProgressHandlersBlockMethod1): Pointer; cdecl;
  end;
  TMLUpdateProgressHandlers = class(TOCGenericImport<MLUpdateProgressHandlersClass, MLUpdateProgressHandlers>) end;

  MLMetricKeyClass = interface(MLKeyClass)
    ['{6371FCC3-7135-4DD5-9F73-D14287D9FBFD}']
    {class} function epochIndex: MLMetricKey; cdecl;
    {class} function lossValue: MLMetricKey; cdecl;
    {class} function miniBatchIndex: MLMetricKey; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  MLMetricKey = interface(MLKey)
    ['{18EFCE77-4762-4F41-88F3-0E6967287175}']
  end;
  TMLMetricKey = class(TOCGenericImport<MLMetricKeyClass, MLMetricKey>) end;

  MLNumericConstraintClass = interface(NSObjectClass)
    ['{D6BCEF95-D810-4BC2-B3F7-80EB175E4704}']
  end;

  MLNumericConstraint = interface(NSObject)
    ['{419627C1-29E8-4BF3-8E6C-31DA52625336}']
    function enumeratedNumbers: NSSet; cdecl;
    function maxNumber: NSNumber; cdecl;
    function minNumber: NSNumber; cdecl;
  end;
  TMLNumericConstraint = class(TOCGenericImport<MLNumericConstraintClass, MLNumericConstraint>) end;

  MLParameterDescriptionClass = interface(NSObjectClass)
    ['{EA1A308A-9C33-4A5D-B6F6-3DD812F2054E}']
  end;

  MLParameterDescription = interface(NSObject)
    ['{29847B85-4615-4841-A35F-223F0C240CFA}']
    function defaultValue: Pointer; cdecl;
    function key: MLParameterKey; cdecl;
    function numericConstraint: MLNumericConstraint; cdecl;
  end;
  TMLParameterDescription = class(TOCGenericImport<MLParameterDescriptionClass, MLParameterDescription>) end;

  MLParameterKeyClass = interface(MLKeyClass)
    ['{2005A5B2-F695-4291-814F-759B275570FF}']
    {class} function beta1: MLParameterKey; cdecl;
    {class} function beta2: MLParameterKey; cdecl;
    {class} function biases: MLParameterKey; cdecl;
    {class} function epochs: MLParameterKey; cdecl;
    {class} function eps: MLParameterKey; cdecl;
    {class} function learningRate: MLParameterKey; cdecl;
    {class} function linkedModelFileName: MLParameterKey; cdecl;
    {class} function linkedModelSearchPath: MLParameterKey; cdecl;
    {class} function miniBatchSize: MLParameterKey; cdecl;
    {class} function momentum: MLParameterKey; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function numberOfNeighbors: MLParameterKey; cdecl;
    {class} function seed: MLParameterKey; cdecl;
    {class} function shuffle: MLParameterKey; cdecl;
    {class} function weights: MLParameterKey; cdecl;
  end;

  MLParameterKey = interface(MLKey)
    ['{607D21C8-EF15-450B-849C-60D30E52E09A}']
    function scopedTo(scope: NSString): MLParameterKey; cdecl;
  end;
  TMLParameterKey = class(TOCGenericImport<MLParameterKeyClass, MLParameterKey>) end;

  MLModelCollectionEntryClass = interface(NSObjectClass)
    ['{BC7AEA5C-D334-47E9-B7B9-7FB3DA524838}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelCollectionEntry = interface(NSObject)
    ['{C6A77573-DC07-4158-8123-53D0DA861E1B}']
    function isEqualToModelCollectionEntry(entry: MLModelCollectionEntry): Boolean; cdecl;
    function modelIdentifier: NSString; cdecl;
    function modelURL: NSURL; cdecl;
  end;
  TMLModelCollectionEntry = class(TOCGenericImport<MLModelCollectionEntryClass, MLModelCollectionEntry>) end;

  MLModelCollectionClass = interface(NSObjectClass)
    ['{14363D9E-76B1-48F1-83B7-A280014AA3F4}']
    {class} function beginAccessingModelCollectionWithIdentifier(identifier: NSString;
      completionHandler: TMLModelCollectionBlockMethod1): NSProgress; cdecl;
    {class} procedure endAccessingModelCollectionWithIdentifier(identifier: NSString; completionHandler: TMLModelCollectionBlockMethod2); cdecl;
    {class} function new: Pointer; cdecl;
  end;

  MLModelCollection = interface(NSObject)
    ['{69A9CCF0-1482-4137-A786-9FB165233EDB}']
    function deploymentID: NSString; cdecl;
    function entries: NSDictionary; cdecl;
    function identifier: NSString; cdecl;
  end;
  TMLModelCollection = class(TOCGenericImport<MLModelCollectionClass, MLModelCollection>) end;

function MLFeatureValueImageOptionCropRect: MLFeatureValueImageOption;
function MLFeatureValueImageOptionCropAndScale: MLFeatureValueImageOption;
function MLModelDescriptionKey: MLModelMetadataKey;
function MLModelVersionStringKey: MLModelMetadataKey;
function MLModelAuthorKey: MLModelMetadataKey;
function MLModelLicenseKey: MLModelMetadataKey;
function MLModelCreatorDefinedKey: MLModelMetadataKey;
function MLModelErrorDomain: NSString;
function MLModelCollectionDidChangeNotification: NSNotificationName;

const
  libCoreML = '/System/Library/Frameworks/CoreML.framework/CoreML';

implementation

uses
  Posix.Dlfcn;

var
  CoreMLModule: THandle;

function MLFeatureValueImageOptionCropRect: MLFeatureValueImageOption;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLFeatureValueImageOptionCropRect');
end;

function MLFeatureValueImageOptionCropAndScale: MLFeatureValueImageOption;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLFeatureValueImageOptionCropAndScale');
end;

function MLModelDescriptionKey: MLModelMetadataKey;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelDescriptionKey');
end;

function MLModelVersionStringKey: MLModelMetadataKey;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelVersionStringKey');
end;

function MLModelAuthorKey: MLModelMetadataKey;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelAuthorKey');
end;

function MLModelLicenseKey: MLModelMetadataKey;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelLicenseKey');
end;

function MLModelCreatorDefinedKey: MLModelMetadataKey;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelCreatorDefinedKey');
end;

function MLModelErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelErrorDomain');
end;

function MLModelCollectionDidChangeNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelCollectionDidChangeNotification');
end;

initialization
  CoreMLModule := dlopen(MarshaledAString(libCoreML), RTLD_LAZY);

finalization
  dlclose(CoreMLModule);

end.