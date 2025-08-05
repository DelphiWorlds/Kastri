unit DW.iOSapi.CoreML;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
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
  MLFeatureTypeState = 8;
  MLMultiArrayDataTypeDouble = 65600;
  MLMultiArrayDataTypeFloat64 = 65600;
  MLMultiArrayDataTypeFloat32 = 65568;
  MLMultiArrayDataTypeFloat16 = 65552;
  MLMultiArrayDataTypeFloat = 65568;
  MLMultiArrayDataTypeInt32 = 131104;
  MLImageSizeConstraintTypeUnspecified = 0;
  MLImageSizeConstraintTypeEnumerated = 2;
  MLImageSizeConstraintTypeRange = 3;
  MLMultiArrayShapeConstraintTypeUnspecified = 1;
  MLMultiArrayShapeConstraintTypeEnumerated = 2;
  MLMultiArrayShapeConstraintTypeRange = 3;
  MLReshapeFrequencyHintFrequent = 0;
  MLReshapeFrequencyHintInfrequent = 1;
  MLSpecializationStrategyDefault = 0;
  MLSpecializationStrategyFastPrediction = 1;
  MLComputeUnitsCPUOnly = 0;
  MLComputeUnitsCPUAndGPU = 1;
  MLComputeUnitsAll = 2;
  MLComputeUnitsCPUAndNeuralEngine = 3;
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
  MLModelErrorPredictionCancelled = 11;
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
  MLState = interface;
  MLFeatureValue = interface;
  MLImageSize = interface;
  MLImageSizeConstraint = interface;
  MLImageConstraint = interface;
  MLDictionaryConstraint = interface;
  MLMultiArrayShapeConstraint = interface;
  MLMultiArrayConstraint = interface;
  MLSequenceConstraint = interface;
  MLStateConstraint = interface;
  MLFeatureDescription = interface;
  MLFeatureProvider = interface;
  MLDictionaryFeatureProvider = interface;
  MLBatchProvider = interface;
  MLArrayBatchProvider = interface;
  MLModelDescription = interface;
  MLPredictionOptions = interface;
  MLOptimizationHints = interface;
  MLModelConfiguration = interface;
  MLModelAsset = interface;
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
  MLComputeDeviceProtocol = interface;
  MLCPUComputeDevice = interface;
  MLGPUComputeDevice = interface;
  MLNeuralEngineComputeDevice = interface;
  MLModelStructure = interface;
  MLModelStructureNeuralNetwork = interface;
  MLModelStructureNeuralNetworkLayer = interface;
  MLModelStructurePipeline = interface;
  MLModelStructureProgram = interface;
  MLModelStructureProgramArgument = interface;
  MLModelStructureProgramBinding = interface;
  MLModelStructureProgramBlock = interface;
  MLModelStructureProgramFunction = interface;
  MLModelStructureProgramNamedValueType = interface;
  MLModelStructureProgramOperation = interface;
  MLModelStructureProgramValue = interface;
  MLModelStructureProgramValueType = interface;
  MLComputePlan = interface;
  MLComputePlanCost = interface;
  MLComputePlanDeviceUsage = interface;

  MLFeatureType = NSInteger;
  MLMultiArrayDataType = NSInteger;
  MLImageSizeConstraintType = NSInteger;
  MLFeatureValueImageOption = NSString;
  MLMultiArrayShapeConstraintType = NSInteger;
  MLModelMetadataKey = NSString;
  MLReshapeFrequencyHint = NSInteger;
  MLSpecializationStrategy = NSInteger;
  MLComputeUnits = NSInteger;
  MLModelError = NSInteger;
  MLTaskState = NSInteger;
  MLUpdateProgressEvent = NSInteger;
  CGImagePropertyOrientation = NSUInteger;

  TMLMultiArrayBlockMethod1 = procedure(bytes: Pointer) of object;
  TMLMultiArrayBlockMethod2 = procedure(bytes: Pointer; size: NSInteger) of object;
  TMLMultiArrayBlockMethod3 = procedure(mutableBytes: Pointer; size: NSInteger; strides: NSArray) of object;
  TMLStateBlockMethod1 = procedure(buffer: MLMultiArray) of object;
  TMLModelAssetBlockMethod1 = procedure(modelDescription: MLModelDescription; error: NSError) of object;
  TMLModelAssetBlockMethod2 = procedure(functionNames: NSArray; error: NSError) of object;
  TMLModelBlockMethod1 = procedure(model: MLModel; error: NSError) of object;
  TMLModelBlockMethod2 = procedure(output: Pointer; error: NSError) of object;
  TMLModelBlockMethod3 = procedure(compiledModelURL: NSURL; error: NSError) of object;
  TMLUpdateTaskBlockMethod1 = procedure(context: MLUpdateContext) of object;
  TMLUpdateTaskBlockMethod2 = procedure(param1: MLUpdateContext) of object;
  TMLUpdateProgressHandlersBlockMethod1 = procedure(context: MLUpdateContext) of object;
  TMLModelCollectionBlockMethod1 = procedure(modelCollection: MLModelCollection; error: NSError) of object;
  TMLModelCollectionBlockMethod2 = procedure(success: Boolean; error: NSError) of object;
  TMLModelStructureBlockMethod1 = procedure(modelStructure: MLModelStructure; error: NSError) of object;
  TMLComputePlanBlockMethod1 = procedure(computePlan: MLComputePlan; error: NSError) of object;

  MLMultiArrayClass = interface(NSObjectClass)
    ['{5A467C49-0000-47FC-B3FB-8723FF79943C}']
    {class} function multiArrayByConcatenatingMultiArrays(multiArrays: NSArray; alongAxis: NSInteger; dataType: MLMultiArrayDataType): Pointer; cdecl;
  end;

  MLMultiArray = interface(NSObject)
    ['{C84062EA-92A4-4759-B414-27D645545B71}']
    function count: NSInteger; cdecl;
    function dataPointer: Pointer; cdecl; // API_DEPRECATED("Use getBytesWithHandler or getMutableBytesWithHandler instead. For Swift, use withUnsafeBytes or withUnsafeMutableBytes.", macos(10.13, API_TO_BE_DEPRECATED), ios(11.0, API_TO_BE_DEPRECATED), tvos(11.0, API_TO_BE_DEPRECATED), watchos(4.0, API_TO_BE_DEPRECATED))
    function dataType: MLMultiArrayDataType; cdecl;
    procedure getBytesWithHandler(handler: TMLMultiArrayBlockMethod2); cdecl;
    procedure getMutableBytesWithHandler(handler: TMLMultiArrayBlockMethod3); cdecl;
    function initWithDataPointer(dataPointer: Pointer; shape: NSArray; dataType: MLMultiArrayDataType; strides: NSArray;
      deallocator: TMLMultiArrayBlockMethod1; error: PPointer): Pointer; cdecl;
    function initWithPixelBuffer(pixelBuffer: CVPixelBufferRef; shape: NSArray): Pointer; cdecl;
    function initWithShape(shape: NSArray; dataType: MLMultiArrayDataType; error: PPointer): Pointer; overload; cdecl;
    function initWithShape(shape: NSArray; dataType: MLMultiArrayDataType; strides: NSArray): Pointer; overload; cdecl;
    function objectAtIndexedSubscript(idx: NSInteger): NSNumber; cdecl;
    function objectForKeyedSubscript(key: NSArray): NSNumber; cdecl;
    function pixelBuffer: CVPixelBufferRef; cdecl;
    procedure setObject(obj: NSNumber; atIndexedSubscript: NSInteger); overload; cdecl;
    procedure setObject(obj: NSNumber; forKeyedSubscript: NSArray); overload; cdecl;
    function shape: NSArray; cdecl;
    function strides: NSArray; cdecl;
    procedure transferToMultiArray(destinationMultiArray: MLMultiArray); cdecl;
  end;
  TMLMultiArray = class(TOCGenericImport<MLMultiArrayClass, MLMultiArray>) end;

  MLSequenceClass = interface(NSObjectClass)
    ['{40B7E444-B325-4D7D-909C-68D40815B730}']
    {class} function emptySequenceWithType(&type: MLFeatureType): Pointer; cdecl;
    {class} function sequenceWithInt64Array(int64Values: NSArray): Pointer; cdecl;
    {class} function sequenceWithStringArray(stringValues: NSArray): Pointer; cdecl;
  end;

  MLSequence = interface(NSObject)
    ['{2B77524F-1BA7-43FE-B468-C6B00D7BD74F}']
    function &type: MLFeatureType; cdecl;
    function int64Values: NSArray; cdecl;
    function stringValues: NSArray; cdecl;
  end;
  TMLSequence = class(TOCGenericImport<MLSequenceClass, MLSequence>) end;

  MLStateClass = interface(NSObjectClass)
    ['{BE7DABAD-9AD7-4AFC-8A28-090BA7A22991}']
    {class} function new: Pointer; cdecl;
  end;

  MLState = interface(NSObject)
    ['{E27A69C9-FA79-4AE9-AEC7-4B1CF45E4322}']
    procedure getMultiArrayForStateNamed(stateName: NSString; handler: TMLStateBlockMethod1); cdecl;
  end;
  TMLState = class(TOCGenericImport<MLStateClass, MLState>) end;

  MLFeatureValueClass = interface(NSObjectClass)
    ['{929B03ED-0FF7-40BB-AB9C-866E7EE99F65}']
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
    ['{1573105C-7320-4F0F-8E0F-31BEEB44A243}']
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
    ['{3EB93BCD-257C-470B-9160-6A24A71EBDB1}']
  end;

  MLImageSize = interface(NSObject)
    ['{7B27080C-FF4E-46E1-BF96-4409F09AB25B}']
    function pixelsHigh: NSInteger; cdecl;
    function pixelsWide: NSInteger; cdecl;
  end;
  TMLImageSize = class(TOCGenericImport<MLImageSizeClass, MLImageSize>) end;

  MLImageSizeConstraintClass = interface(NSObjectClass)
    ['{F847434A-1DAA-4E18-8607-1B207DF29B66}']
  end;

  MLImageSizeConstraint = interface(NSObject)
    ['{821F5DAD-4B69-4D31-BF3F-FD0EA2F000BD}']
    function &type: MLImageSizeConstraintType; cdecl;
    function enumeratedImageSizes: NSArray; cdecl;
    function pixelsHighRange: NSRange; cdecl;
    function pixelsWideRange: NSRange; cdecl;
  end;
  TMLImageSizeConstraint = class(TOCGenericImport<MLImageSizeConstraintClass, MLImageSizeConstraint>) end;

  MLImageConstraintClass = interface(NSObjectClass)
    ['{06F215D7-C668-4ADE-B048-5A500889CA08}']
  end;

  MLImageConstraint = interface(NSObject)
    ['{E54BA061-901D-4553-B126-1A2596BF05FD}']
    function pixelFormatType: OSType; cdecl;
    function pixelsHigh: NSInteger; cdecl;
    function pixelsWide: NSInteger; cdecl;
    function sizeConstraint: MLImageSizeConstraint; cdecl;
  end;
  TMLImageConstraint = class(TOCGenericImport<MLImageConstraintClass, MLImageConstraint>) end;

  MLDictionaryConstraintClass = interface(NSObjectClass)
    ['{B9351812-E5F2-4D08-BBC4-44729B49ABC5}']
  end;

  MLDictionaryConstraint = interface(NSObject)
    ['{F255F9D5-EF8E-4F05-A8BF-BB4EF6A5941D}']
    function keyType: MLFeatureType; cdecl;
  end;
  TMLDictionaryConstraint = class(TOCGenericImport<MLDictionaryConstraintClass, MLDictionaryConstraint>) end;

  MLMultiArrayShapeConstraintClass = interface(NSObjectClass)
    ['{BE2CC643-1354-461E-AE4B-EA44601D28FB}']
  end;

  MLMultiArrayShapeConstraint = interface(NSObject)
    ['{67F8DF3A-0BC4-4D43-8CB2-BE29408C761C}']
    function &type: MLMultiArrayShapeConstraintType; cdecl;
    function enumeratedShapes: NSArray; cdecl;
    function sizeRangeForDimension: NSArray; cdecl;
  end;
  TMLMultiArrayShapeConstraint = class(TOCGenericImport<MLMultiArrayShapeConstraintClass, MLMultiArrayShapeConstraint>) end;

  MLMultiArrayConstraintClass = interface(NSObjectClass)
    ['{8DA4180C-5F15-44A5-9EFB-CBB1CDF89187}']
  end;

  MLMultiArrayConstraint = interface(NSObject)
    ['{C2C2E244-F303-4496-A632-629AA38A1720}']
    function dataType: MLMultiArrayDataType; cdecl;
    function shape: NSArray; cdecl;
    function shapeConstraint: MLMultiArrayShapeConstraint; cdecl;
  end;
  TMLMultiArrayConstraint = class(TOCGenericImport<MLMultiArrayConstraintClass, MLMultiArrayConstraint>) end;

  MLSequenceConstraintClass = interface(NSObjectClass)
    ['{7099D6DA-9919-4F9C-861F-D089DFD60688}']
  end;

  MLSequenceConstraint = interface(NSObject)
    ['{0C2B6AF9-B4FF-4DF8-AC77-9A4122D3D713}']
    function countRange: NSRange; cdecl;
    function valueDescription: MLFeatureDescription; cdecl;
  end;
  TMLSequenceConstraint = class(TOCGenericImport<MLSequenceConstraintClass, MLSequenceConstraint>) end;

  MLStateConstraintClass = interface(NSObjectClass)
    ['{4DD39631-FDF5-4E0F-9022-9167D1F13A5F}']
  end;

  MLStateConstraint = interface(NSObject)
    ['{A292E2A2-DF87-45D8-8E50-D73ACB2B8BDD}']
    function bufferShape: NSArray; cdecl;
    function dataType: MLMultiArrayDataType; cdecl;
  end;
  TMLStateConstraint = class(TOCGenericImport<MLStateConstraintClass, MLStateConstraint>) end;

  MLFeatureDescriptionClass = interface(NSObjectClass)
    ['{687DD074-AB1C-4BEB-9E18-AAE18DCD4F14}']
  end;

  MLFeatureDescription = interface(NSObject)
    ['{86653559-5342-4EBD-BA06-0C5B37C8C7CC}']
    function &type: MLFeatureType; cdecl;
    function dictionaryConstraint: MLDictionaryConstraint; cdecl;
    function imageConstraint: MLImageConstraint; cdecl;
    function isAllowedValue(value: MLFeatureValue): Boolean; cdecl;
    function isOptional: Boolean; cdecl;
    function multiArrayConstraint: MLMultiArrayConstraint; cdecl;
    function name: NSString; cdecl;
    function sequenceConstraint: MLSequenceConstraint; cdecl;
    function stateConstraint: MLStateConstraint; cdecl;
  end;
  TMLFeatureDescription = class(TOCGenericImport<MLFeatureDescriptionClass, MLFeatureDescription>) end;

  MLFeatureProvider = interface(IObjectiveC)
    ['{24B92C49-8B80-45B8-9D24-D271D9590673}']
    function featureNames: NSSet; cdecl;
    function featureValueForName(featureName: NSString): MLFeatureValue; cdecl;
  end;

  MLDictionaryFeatureProviderClass = interface(NSObjectClass)
    ['{F0038026-C19B-49F9-9D6B-ED70560FF36A}']
  end;

  MLDictionaryFeatureProvider = interface(NSObject)
    ['{686AFBC7-B916-4F3E-B434-6A5366E427FA}']
    function dictionary: NSDictionary; cdecl;
    function initWithDictionary(dictionary: NSDictionary; error: PPointer): Pointer; cdecl;
    function objectForKeyedSubscript(featureName: NSString): MLFeatureValue; cdecl;
  end;
  TMLDictionaryFeatureProvider = class(TOCGenericImport<MLDictionaryFeatureProviderClass, MLDictionaryFeatureProvider>) end;

  MLBatchProvider = interface(IObjectiveC)
    ['{3561A47D-C867-44C8-BA58-5C95CC6929D4}']
    function count: NSInteger; cdecl;
    function featuresAtIndex(index: NSInteger): Pointer; cdecl;
  end;

  MLArrayBatchProviderClass = interface(NSObjectClass)
    ['{10FB8D91-BEE3-4CB8-9E9C-EDDE917872C1}']
  end;

  MLArrayBatchProvider = interface(NSObject)
    ['{478496AF-2A53-40B4-A25B-5D7F4C250FF1}']
    function &array: NSArray; cdecl;
    function initWithDictionary(dictionary: NSDictionary; error: PPointer): Pointer; cdecl;
    function initWithFeatureProviderArray(&array: NSArray): Pointer; cdecl;
  end;
  TMLArrayBatchProvider = class(TOCGenericImport<MLArrayBatchProviderClass, MLArrayBatchProvider>) end;

  MLModelDescriptionClass = interface(NSObjectClass)
    ['{DB920F0B-915E-4B35-93D3-3B6A9794E42D}']
  end;

  MLModelDescription = interface(NSObject)
    ['{FF31F3A2-2994-415A-B4E2-73A73E17C444}']
    function classLabels: NSArray; cdecl;
    function inputDescriptionsByName: NSDictionary; cdecl;
    function isUpdatable: Boolean; cdecl;
    function metadata: NSDictionary; cdecl;
    function outputDescriptionsByName: NSDictionary; cdecl;
    function parameterDescriptionsByKey: NSDictionary; cdecl;
    function predictedFeatureName: NSString; cdecl;
    function predictedProbabilitiesName: NSString; cdecl;
    function stateDescriptionsByName: NSDictionary; cdecl;
    function trainingInputDescriptionsByName: NSDictionary; cdecl;
  end;
  TMLModelDescription = class(TOCGenericImport<MLModelDescriptionClass, MLModelDescription>) end;

  MLPredictionOptionsClass = interface(NSObjectClass)
    ['{0C1AEE91-C286-4FC0-92AC-5025111A9D43}']
  end;

  MLPredictionOptions = interface(NSObject)
    ['{21767851-B869-4271-9DD9-1C66E68314B4}']
    function outputBackings: NSDictionary; cdecl;
    procedure setOutputBackings(outputBackings: NSDictionary); cdecl;
    procedure setUsesCPUOnly(usesCPUOnly: Boolean); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use -[MLModelConfiguration computeUnits] instead.", macos(10.13, 12.0), ios(11.0, 15.0), tvos(11.0, 15.0), watchos(4.0, 8.0))
    function usesCPUOnly: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use -[MLModelConfiguration computeUnits] instead.", macos(10.13, 12.0), ios(11.0, 15.0), tvos(11.0, 15.0), watchos(4.0, 8.0))
  end;
  TMLPredictionOptions = class(TOCGenericImport<MLPredictionOptionsClass, MLPredictionOptions>) end;

  MLOptimizationHintsClass = interface(NSObjectClass)
    ['{5A9B7BB9-0CF2-472F-88F3-EA018EA0FCB9}']
  end;

  MLOptimizationHints = interface(NSObject)
    ['{1BEB3BC7-0735-4F9F-98B4-F2B57B554F1C}']
    function reshapeFrequency: MLReshapeFrequencyHint; cdecl;
    procedure setReshapeFrequency(reshapeFrequency: MLReshapeFrequencyHint); cdecl;
    procedure setSpecializationStrategy(specializationStrategy: MLSpecializationStrategy); cdecl;
    function specializationStrategy: MLSpecializationStrategy; cdecl;
  end;
  TMLOptimizationHints = class(TOCGenericImport<MLOptimizationHintsClass, MLOptimizationHints>) end;

  MLModelConfigurationClass = interface(NSObjectClass)
    ['{4B63817E-0FC7-49B3-A836-D462B7F9E999}']
  end;

  MLModelConfiguration = interface(NSObject)
    ['{88CDC2B5-F442-459E-8446-6C6A767F0338}']
    function allowLowPrecisionAccumulationOnGPU: Boolean; cdecl;
    function computeUnits: MLComputeUnits; cdecl;
    function functionName: NSString; cdecl;
    function modelDisplayName: NSString; cdecl;
    function optimizationHints: MLOptimizationHints; cdecl;
    function parameters: NSDictionary; cdecl;
    function preferredMetalDevice: Pointer; cdecl;
    procedure setAllowLowPrecisionAccumulationOnGPU(allowLowPrecisionAccumulationOnGPU: Boolean); cdecl;
    procedure setComputeUnits(computeUnits: MLComputeUnits); cdecl;
    procedure setFunctionName(functionName: NSString); cdecl;
    procedure setModelDisplayName(modelDisplayName: NSString); cdecl;
    procedure setOptimizationHints(optimizationHints: MLOptimizationHints); cdecl;
    procedure setParameters(parameters: NSDictionary); cdecl;
    procedure setPreferredMetalDevice(preferredMetalDevice: Pointer); cdecl;
  end;
  TMLModelConfiguration = class(TOCGenericImport<MLModelConfigurationClass, MLModelConfiguration>) end;

  MLModelAssetClass = interface(NSObjectClass)
    ['{B92863FC-A707-4EF3-8BBB-78D1D6366B53}']
    {class} function modelAssetWithSpecificationData(specificationData: NSData; blobMapping: NSDictionary; error: PPointer): Pointer; overload; cdecl;
    {class} function modelAssetWithSpecificationData(specificationData: NSData; error: PPointer): Pointer; overload; cdecl;
    {class} function modelAssetWithURL(compiledModelURL: NSURL; error: PPointer): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  MLModelAsset = interface(NSObject)
    ['{563B9371-7D46-421D-9849-8EB952AFE404}']
    procedure functionNamesWithCompletionHandler(handler: TMLModelAssetBlockMethod2); cdecl;
    procedure modelDescriptionOfFunctionNamed(functionName: NSString; completionHandler: TMLModelAssetBlockMethod1); cdecl;
    procedure modelDescriptionWithCompletionHandler(handler: TMLModelAssetBlockMethod1); cdecl;
  end;
  TMLModelAsset = class(TOCGenericImport<MLModelAssetClass, MLModelAsset>) end;

  MLModelClass = interface(NSObjectClass)
    ['{C39C3F66-ACEA-45EB-A131-AFBA84077B8A}']
    {class} function availableComputeDevices: NSArray; cdecl;
    {class} function compileModelAtURL(modelURL: NSURL; error: PPointer): NSURL; overload; cdecl; // API_DEPRECATED("Use the asynchronous interface compileModelAtURL:completionHandler:error: instead.", macos(10.13, API_TO_BE_DEPRECATED), ios(11.0, API_TO_BE_DEPRECATED), tvos(11.0, API_TO_BE_DEPRECATED))
    {class} procedure compileModelAtURL(modelURL: NSURL; completionHandler: TMLModelBlockMethod3); overload; cdecl;
    {class} procedure loadContentsOfURL(url: NSURL; configuration: MLModelConfiguration; completionHandler: TMLModelBlockMethod1); cdecl;
    {class} procedure loadModelAsset(asset: MLModelAsset; configuration: MLModelConfiguration; completionHandler: TMLModelBlockMethod1); cdecl;
    {class} function modelWithContentsOfURL(url: NSURL; error: PPointer): Pointer; overload; cdecl;
    {class} function modelWithContentsOfURL(url: NSURL; configuration: MLModelConfiguration; error: PPointer): Pointer; overload; cdecl;
  end;

  MLModel = interface(NSObject)
    ['{A3C0D7B2-96A8-489D-BB52-C765C05EDE6D}']
    function configuration: MLModelConfiguration; cdecl;
    function modelDescription: MLModelDescription; cdecl;
    function newState: MLState; cdecl;
    function parameterValueForKey(key: MLParameterKey; error: PPointer): Pointer; cdecl;
    function predictionFromFeatures(input: Pointer; error: PPointer): Pointer; overload; cdecl;
    function predictionFromFeatures(inputFeatures: Pointer; usingState: MLState; error: PPointer): Pointer; overload; cdecl;
    function predictionFromFeatures(inputFeatures: Pointer; usingState: MLState; options: MLPredictionOptions;
      error: PPointer): Pointer; overload; cdecl;
    procedure predictionFromFeatures(inputFeatures: Pointer; usingState: MLState; options: MLPredictionOptions;
      completionHandler: TMLModelBlockMethod2); overload; cdecl;
    function predictionFromFeatures(input: Pointer; options: MLPredictionOptions; error: PPointer): Pointer; overload; cdecl;
    procedure predictionFromFeatures(input: Pointer; options: MLPredictionOptions; completionHandler: TMLModelBlockMethod2); overload; cdecl;
    procedure predictionFromFeatures(input: Pointer; completionHandler: TMLModelBlockMethod2); overload; cdecl;
    function predictionsFromBatch(inputBatch: Pointer; error: PPointer): Pointer; overload; cdecl;
    function predictionsFromBatch(inputBatch: Pointer; options: MLPredictionOptions; error: PPointer): Pointer; overload; cdecl;
  end;
  TMLModel = class(TOCGenericImport<MLModelClass, MLModel>) end;

  MLCustomLayer = interface(IObjectiveC)
    ['{4DA6D6B4-2DF3-4940-9F8D-ECC752679F08}']
    function encodeToCommandBuffer(commandBuffer: Pointer; inputs: NSArray; outputs: NSArray; error: PPointer): Boolean; cdecl;
    function evaluateOnCPUWithInputs(inputs: NSArray; outputs: NSArray; error: PPointer): Boolean; cdecl;
    function initWithParameterDictionary(parameters: NSDictionary; error: PPointer): Pointer; cdecl;
    function outputShapesForInputShapes(inputShapes: NSArray; error: PPointer): NSArray; cdecl;
    function setWeightData(weights: NSArray; error: PPointer): Boolean; cdecl;
  end;

  MLCustomModel = interface(IObjectiveC)
    ['{C0F2B178-82BA-4DB8-91D8-29741E4EBB13}']
    function initWithModelDescription(modelDescription: MLModelDescription; parameterDictionary: NSDictionary; error: PPointer): Pointer; cdecl;
    function predictionFromFeatures(input: Pointer; options: MLPredictionOptions; error: PPointer): Pointer; cdecl;
    function predictionsFromBatch(inputBatch: Pointer; options: MLPredictionOptions; error: PPointer): Pointer; cdecl;
  end;

  MLKeyClass = interface(NSObjectClass)
    ['{1559DF30-29F3-4416-99A7-32EFC5141EB0}']
    {class} function new: Pointer; cdecl;
  end;

  MLKey = interface(NSObject)
    ['{5ED62692-33C5-4AD8-9A8E-2C3D94EDC547}']
    function name: NSString; cdecl;
    function scope: NSString; cdecl;
  end;
  TMLKey = class(TOCGenericImport<MLKeyClass, MLKey>) end;

  MLTaskClass = interface(NSObjectClass)
    ['{D33D3581-B6D2-4D12-87AD-660A97972B55}']
    {class} function new: Pointer; cdecl;
  end;

  MLTask = interface(NSObject)
    ['{D14805F7-53EE-4141-9CBE-081B8DED924A}']
    procedure cancel; cdecl;
    function error: NSError; cdecl;
    procedure resume; cdecl;
    function state: MLTaskState; cdecl;
    function taskIdentifier: NSString; cdecl;
  end;
  TMLTask = class(TOCGenericImport<MLTaskClass, MLTask>) end;

  MLUpdateTaskClass = interface(MLTaskClass)
    ['{5FA7AA20-02DE-4A24-BD6F-038C7B6A3E2F}']
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
    ['{53C94FF9-20E6-4FC2-8FA5-19BFECC683EA}']
    procedure resumeWithParameters(updateParameters: NSDictionary); cdecl;
  end;
  TMLUpdateTask = class(TOCGenericImport<MLUpdateTaskClass, MLUpdateTask>) end;

  MLWritable = interface(IObjectiveC)
    ['{0C227922-411F-407D-90D6-6C99EB956ED8}']
    function writeToURL(url: NSURL; error: PPointer): Boolean; cdecl;
  end;

  MLUpdateContextClass = interface(NSObjectClass)
    ['{C9EBD0AD-29AA-4C2F-958D-488EBED789D6}']
  end;

  MLUpdateContext = interface(NSObject)
    ['{916F016A-0F25-4CCB-910E-663A5316D682}']
    function event: MLUpdateProgressEvent; cdecl;
    function metrics: NSDictionary; cdecl;
    function model: MLModel; cdecl;
    function parameters: NSDictionary; cdecl;
    function task: MLUpdateTask; cdecl;
  end;
  TMLUpdateContext = class(TOCGenericImport<MLUpdateContextClass, MLUpdateContext>) end;

  MLUpdateProgressHandlersClass = interface(NSObjectClass)
    ['{4A2EFE3E-6F3B-49A3-A0A1-8EFFAACD655A}']
    {class} function new: Pointer; cdecl;
  end;

  MLUpdateProgressHandlers = interface(NSObject)
    ['{5BBB59F7-C461-4A91-8CEF-DCD80848CD8C}']
    function initForEvents(interestedEvents: MLUpdateProgressEvent; progressHandler: TMLUpdateProgressHandlersBlockMethod1;
      completionHandler: TMLUpdateProgressHandlersBlockMethod1): Pointer; cdecl;
  end;
  TMLUpdateProgressHandlers = class(TOCGenericImport<MLUpdateProgressHandlersClass, MLUpdateProgressHandlers>) end;

  MLMetricKeyClass = interface(MLKeyClass)
    ['{5602B6A5-BB03-46BA-BFB5-C87DF084F8BA}']
    {class} function epochIndex: MLMetricKey; cdecl;
    {class} function lossValue: MLMetricKey; cdecl;
    {class} function miniBatchIndex: MLMetricKey; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  MLMetricKey = interface(MLKey)
    ['{127ED421-0B27-4289-AE5A-7FD36DAA31BF}']
  end;
  TMLMetricKey = class(TOCGenericImport<MLMetricKeyClass, MLMetricKey>) end;

  MLNumericConstraintClass = interface(NSObjectClass)
    ['{319AF916-A953-4E1E-94ED-01873EFD4C5A}']
  end;

  MLNumericConstraint = interface(NSObject)
    ['{D0A56D0A-9A32-490B-BAC0-C6DC6057CD3D}']
    function enumeratedNumbers: NSSet; cdecl;
    function maxNumber: NSNumber; cdecl;
    function minNumber: NSNumber; cdecl;
  end;
  TMLNumericConstraint = class(TOCGenericImport<MLNumericConstraintClass, MLNumericConstraint>) end;

  MLParameterDescriptionClass = interface(NSObjectClass)
    ['{44C4AFAF-A866-4006-B2AA-DBD9A45421AB}']
  end;

  MLParameterDescription = interface(NSObject)
    ['{F13041DC-A900-4E7E-926E-FD1E0C3F5D3C}']
    function defaultValue: Pointer; cdecl;
    function key: MLParameterKey; cdecl;
    function numericConstraint: MLNumericConstraint; cdecl;
  end;
  TMLParameterDescription = class(TOCGenericImport<MLParameterDescriptionClass, MLParameterDescription>) end;

  MLParameterKeyClass = interface(MLKeyClass)
    ['{B2C57E5C-1A0C-47F8-881F-646D3B1F5AE4}']
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
    ['{023E351C-AE29-42BD-8A45-62A88F8AC5FF}']
    function scopedTo(scope: NSString): MLParameterKey; cdecl;
  end;
  TMLParameterKey = class(TOCGenericImport<MLParameterKeyClass, MLParameterKey>) end;

  MLModelCollectionEntryClass = interface(NSObjectClass)
    ['{60178960-8DB8-474B-8258-BB8578F16328}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelCollectionEntry = interface(NSObject)
    ['{CE874623-B2F9-41A4-B5CC-659F388BFDBE}']
    function isEqualToModelCollectionEntry(entry: MLModelCollectionEntry): Boolean; cdecl;
    function modelIdentifier: NSString; cdecl;
    function modelURL: NSURL; cdecl;
  end;
  TMLModelCollectionEntry = class(TOCGenericImport<MLModelCollectionEntryClass, MLModelCollectionEntry>) end;

  MLModelCollectionClass = interface(NSObjectClass)
    ['{EE444C86-746F-4F9B-A0C3-3921DC5446EE}']
    {class} function beginAccessingModelCollectionWithIdentifier(identifier: NSString;
      completionHandler: TMLModelCollectionBlockMethod1): NSProgress; cdecl;
    {class} procedure endAccessingModelCollectionWithIdentifier(identifier: NSString; completionHandler: TMLModelCollectionBlockMethod2); cdecl;
    {class} function new: Pointer; cdecl;
  end;

  MLModelCollection = interface(NSObject)
    ['{683BBE3D-2CF6-48EF-9710-E65AFBDDBC53}']
    function deploymentID: NSString; cdecl;
    function entries: NSDictionary; cdecl;
    function identifier: NSString; cdecl;
  end;
  TMLModelCollection = class(TOCGenericImport<MLModelCollectionClass, MLModelCollection>) end;

  MLComputeDeviceProtocol = interface(IObjectiveC)
    ['{B51DD4D1-2E11-4086-9279-7900A3D0A076}']
  end;

  MLCPUComputeDeviceClass = interface(NSObjectClass)
    ['{A583C9BC-2391-44D4-B5E0-DD407094B218}']
    {class} function new: Pointer; cdecl;
  end;

  MLCPUComputeDevice = interface(NSObject)
    ['{8D16342B-6FE4-470B-948E-7D31B57FE970}']
  end;
  TMLCPUComputeDevice = class(TOCGenericImport<MLCPUComputeDeviceClass, MLCPUComputeDevice>) end;

  MLGPUComputeDeviceClass = interface(NSObjectClass)
    ['{94094AE8-1FBD-4827-913F-07B499BB140F}']
    {class} function new: Pointer; cdecl;
  end;

  MLGPUComputeDevice = interface(NSObject)
    ['{E32FC914-C3A9-4578-AE42-60F2917B3977}']
    function metalDevice: Pointer; cdecl;
  end;
  TMLGPUComputeDevice = class(TOCGenericImport<MLGPUComputeDeviceClass, MLGPUComputeDevice>) end;

  MLNeuralEngineComputeDeviceClass = interface(NSObjectClass)
    ['{6C40D772-67F9-45D8-92A5-5E1251692265}']
    {class} function new: Pointer; cdecl;
  end;

  MLNeuralEngineComputeDevice = interface(NSObject)
    ['{683BE0DC-5794-43ED-9C9C-171A9B0FA555}']
    function totalCoreCount: NSInteger; cdecl;
  end;
  TMLNeuralEngineComputeDevice = class(TOCGenericImport<MLNeuralEngineComputeDeviceClass, MLNeuralEngineComputeDevice>) end;

  MLModelStructureClass = interface(NSObjectClass)
    ['{D9727303-CB6E-4DE9-8E48-972BD527A19D}']
    {class} procedure loadContentsOfURL(url: NSURL; completionHandler: TMLModelStructureBlockMethod1); cdecl;
    {class} procedure loadModelAsset(asset: MLModelAsset; completionHandler: TMLModelStructureBlockMethod1); cdecl;
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructure = interface(NSObject)
    ['{275A9E61-BCB9-4777-8EB1-0AD95B01BB65}']
    function &program: MLModelStructureProgram; cdecl;
    function neuralNetwork: MLModelStructureNeuralNetwork; cdecl;
    function pipeline: MLModelStructurePipeline; cdecl;
  end;
  TMLModelStructure = class(TOCGenericImport<MLModelStructureClass, MLModelStructure>) end;

  MLModelStructureNeuralNetworkClass = interface(NSObjectClass)
    ['{004DD963-438D-4DC2-994C-82C3DC3CEEC4}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureNeuralNetwork = interface(NSObject)
    ['{F22384DB-F15F-488F-B14F-0B5FC027FD77}']
    function layers: NSArray; cdecl;
  end;
  TMLModelStructureNeuralNetwork = class(TOCGenericImport<MLModelStructureNeuralNetworkClass, MLModelStructureNeuralNetwork>) end;

  MLModelStructureNeuralNetworkLayerClass = interface(NSObjectClass)
    ['{AD72E8B0-B4E3-4390-9FD4-C1AF3F56E2A0}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureNeuralNetworkLayer = interface(NSObject)
    ['{CBC98D59-2B0B-4832-BEE4-E7F218EF5A4D}']
    function &type: NSString; cdecl;
    function inputNames: NSArray; cdecl;
    function name: NSString; cdecl;
    function outputNames: NSArray; cdecl;
  end;
  TMLModelStructureNeuralNetworkLayer = class(TOCGenericImport<MLModelStructureNeuralNetworkLayerClass, MLModelStructureNeuralNetworkLayer>) end;

  MLModelStructurePipelineClass = interface(NSObjectClass)
    ['{398A65F9-CADD-4D70-9A75-7EDAE0FC2FC7}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructurePipeline = interface(NSObject)
    ['{0BD8730E-A3EE-4CF2-8446-1BE465221ADE}']
    function subModelNames: NSArray; cdecl;
    function subModels: NSArray; cdecl;
  end;
  TMLModelStructurePipeline = class(TOCGenericImport<MLModelStructurePipelineClass, MLModelStructurePipeline>) end;

  MLModelStructureProgramClass = interface(NSObjectClass)
    ['{B9992C7B-094F-4413-8A5B-3F2A549CE04C}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgram = interface(NSObject)
    ['{BBDD2C6A-5CD2-4692-A7CB-2C87D07AFFF9}']
    function functions: NSDictionary; cdecl;
  end;
  TMLModelStructureProgram = class(TOCGenericImport<MLModelStructureProgramClass, MLModelStructureProgram>) end;

  MLModelStructureProgramArgumentClass = interface(NSObjectClass)
    ['{EDD947E1-9E83-486C-8B29-320AB486304E}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgramArgument = interface(NSObject)
    ['{DD1C156B-7E7F-416F-8470-5017B2BDF8EC}']
    function bindings: NSArray; cdecl;
  end;
  TMLModelStructureProgramArgument = class(TOCGenericImport<MLModelStructureProgramArgumentClass, MLModelStructureProgramArgument>) end;

  MLModelStructureProgramBindingClass = interface(NSObjectClass)
    ['{C601D245-6A39-4E2D-9B48-18AEC2C39135}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgramBinding = interface(NSObject)
    ['{FC2E6C13-1089-4F64-9BBB-A87DDA1B2C02}']
    function name: NSString; cdecl;
    function value: MLModelStructureProgramValue; cdecl;
  end;
  TMLModelStructureProgramBinding = class(TOCGenericImport<MLModelStructureProgramBindingClass, MLModelStructureProgramBinding>) end;

  MLModelStructureProgramBlockClass = interface(NSObjectClass)
    ['{D9B1523F-8F74-41DC-A4D5-D7885FA3FD19}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgramBlock = interface(NSObject)
    ['{ACF48D78-CC4F-4530-A5E9-3E94C42013CA}']
    function inputs: NSArray; cdecl;
    function operations: NSArray; cdecl;
    function outputNames: NSArray; cdecl;
  end;
  TMLModelStructureProgramBlock = class(TOCGenericImport<MLModelStructureProgramBlockClass, MLModelStructureProgramBlock>) end;

  MLModelStructureProgramFunctionClass = interface(NSObjectClass)
    ['{9C291A11-E0F2-4311-83E7-9001D310930A}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgramFunction = interface(NSObject)
    ['{CD71E9CD-7278-4EF5-A07C-27708C7B7A91}']
    function block: MLModelStructureProgramBlock; cdecl;
    function inputs: NSArray; cdecl;
  end;
  TMLModelStructureProgramFunction = class(TOCGenericImport<MLModelStructureProgramFunctionClass, MLModelStructureProgramFunction>) end;

  MLModelStructureProgramNamedValueTypeClass = interface(NSObjectClass)
    ['{4AD1D4AC-F158-4F31-9999-8819E320A17E}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgramNamedValueType = interface(NSObject)
    ['{B911DD10-C2A7-4E1B-A443-61C7290A295B}']
    function &type: MLModelStructureProgramValueType; cdecl;
    function name: NSString; cdecl;
  end;
  TMLModelStructureProgramNamedValueType = class(TOCGenericImport<MLModelStructureProgramNamedValueTypeClass, MLModelStructureProgramNamedValueType>) end;

  MLModelStructureProgramOperationClass = interface(NSObjectClass)
    ['{8C7DAD99-5589-4202-A636-B94B4B91275B}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgramOperation = interface(NSObject)
    ['{EF2F3663-9900-4697-BA52-7CEDD542F480}']
    function blocks: NSArray; cdecl;
    function inputs: NSDictionary; cdecl;
    function operatorName: NSString; cdecl;
    function outputs: NSArray; cdecl;
  end;
  TMLModelStructureProgramOperation = class(TOCGenericImport<MLModelStructureProgramOperationClass, MLModelStructureProgramOperation>) end;

  MLModelStructureProgramValueClass = interface(NSObjectClass)
    ['{435C5C69-2C95-4D81-8CEB-7D47CF5A1984}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgramValue = interface(NSObject)
    ['{09FADAAE-D480-4CF3-9DD5-9B17F28C0B87}']
  end;
  TMLModelStructureProgramValue = class(TOCGenericImport<MLModelStructureProgramValueClass, MLModelStructureProgramValue>) end;

  MLModelStructureProgramValueTypeClass = interface(NSObjectClass)
    ['{11E8CA75-DAC6-4E89-9095-692393AD2F85}']
    {class} function new: Pointer; cdecl;
  end;

  MLModelStructureProgramValueType = interface(NSObject)
    ['{02AF66CC-860A-45D5-A781-B07EB3C2CF03}']
  end;
  TMLModelStructureProgramValueType = class(TOCGenericImport<MLModelStructureProgramValueTypeClass, MLModelStructureProgramValueType>) end;

  MLComputePlanClass = interface(NSObjectClass)
    ['{8196C3CB-4333-4447-9EFA-E1A0C70833AB}']
    {class} procedure loadContentsOfURL(url: NSURL; configuration: MLModelConfiguration; completionHandler: TMLComputePlanBlockMethod1); cdecl;
    {class} procedure loadModelAsset(asset: MLModelAsset; configuration: MLModelConfiguration; completionHandler: TMLComputePlanBlockMethod1); cdecl;
    {class} function new: Pointer; cdecl;
  end;

  MLComputePlan = interface(NSObject)
    ['{760F5A68-B956-4819-9946-733B1F15AAA3}']
    function computeDeviceUsageForMLProgramOperation(operation: MLModelStructureProgramOperation): MLComputePlanDeviceUsage; cdecl;
    function computeDeviceUsageForNeuralNetworkLayer(layer: MLModelStructureNeuralNetworkLayer): MLComputePlanDeviceUsage; cdecl;
    function estimatedCostOfMLProgramOperation(operation: MLModelStructureProgramOperation): MLComputePlanCost; cdecl;
    function modelStructure: MLModelStructure; cdecl;
  end;
  TMLComputePlan = class(TOCGenericImport<MLComputePlanClass, MLComputePlan>) end;

  MLComputePlanCostClass = interface(NSObjectClass)
    ['{666FD7EC-0A01-49F2-B8E3-7071D0C4F3F6}']
    {class} function new: Pointer; cdecl;
  end;

  MLComputePlanCost = interface(NSObject)
    ['{065BB643-D611-4AF5-9D9C-072CD7C1CEA7}']
    function weight: Double; cdecl;
  end;
  TMLComputePlanCost = class(TOCGenericImport<MLComputePlanCostClass, MLComputePlanCost>) end;

  MLComputePlanDeviceUsageClass = interface(NSObjectClass)
    ['{05104F65-F7DC-41DD-B2A1-8FFB9482DEA0}']
    {class} function new: Pointer; cdecl;
  end;

  MLComputePlanDeviceUsage = interface(NSObject)
    ['{1D8CF972-DA2C-422E-9537-79410BAC3CA1}']
    function preferredComputeDevice: Pointer; cdecl;
    function supportedComputeDevices: NSArray; cdecl;
  end;
  TMLComputePlanDeviceUsage = class(TOCGenericImport<MLComputePlanDeviceUsageClass, MLComputePlanDeviceUsage>) end;

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

function MLAllComputeDevices: NSArray; cdecl;
  external libCoreML name _PU + 'MLAllComputeDevices';

implementation

uses
  Posix.Dlfcn;

var
  CoreMLModule: THandle;

function MLFeatureValueImageOptionCropRect: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLFeatureValueImageOptionCropRect');
end;

function MLFeatureValueImageOptionCropAndScale: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLFeatureValueImageOptionCropAndScale');
end;

function MLModelDescriptionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelDescriptionKey');
end;

function MLModelVersionStringKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelVersionStringKey');
end;

function MLModelAuthorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelAuthorKey');
end;

function MLModelLicenseKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelLicenseKey');
end;

function MLModelCreatorDefinedKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelCreatorDefinedKey');
end;

function MLModelErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelErrorDomain');
end;

function MLModelCollectionDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreML, 'MLModelCollectionDidChangeNotification');
end;

initialization
  CoreMLModule := dlopen(MarshaledAString(libCoreML), RTLD_LAZY);

finalization
  dlclose(CoreMLModule);

end.