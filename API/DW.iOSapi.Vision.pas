unit DW.iOSapi.Vision;

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
  Macapi.ObjectiveC, Macapi.ObjCRuntime,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.CoreImage,
  // DW
  DW.iOSapi.CoreImage;

const
  VNImageCropAndScaleOptionCenterCrop = 0;
  VNImageCropAndScaleOptionScaleFit = 1;
  VNImageCropAndScaleOptionScaleFill = 2;
  VNElementTypeUnknown = 0;
  VNElementTypeFloat = 1;
  VNElementTypeDouble = 2;
  VNErrorOK = 0;
  VNErrorRequestCancelled = 1;
  VNErrorInvalidFormat = 2;
  VNErrorOperationFailed = 3;
  VNErrorOutOfBoundsError = 4;
  VNErrorInvalidOption = 5;
  VNErrorIOError = 6;
  VNErrorMissingOption = 7;
  VNErrorNotImplemented = 8;
  VNErrorInternalError = 9;
  VNErrorOutOfMemory = 10;
  VNErrorUnknownError = 11;
  VNErrorInvalidOperation = 12;
  VNErrorInvalidImage = 13;
  VNErrorInvalidArgument = 14;
  VNErrorInvalidModel = 15;
  VNErrorUnsupportedRevision = 16;
  VNErrorDataUnavailable = 17;
  VNRequestFaceLandmarksConstellationNotDefined = 0;
  VNRequestFaceLandmarksConstellation65Points = 1;
  VNRequestFaceLandmarksConstellation76Points = 2;
  VNRequestTextRecognitionLevelAccurate = 0;
  VNRequestTextRecognitionLevelFast = 1;
  VNRequestTrackingLevelAccurate = 0;
  VNRequestTrackingLevelFast = 1;

type
  VNRequestRevisionProviding = interface;
  VNFaceLandmarkRegion = interface;
  VNFaceLandmarkRegion2D = interface;
  VNFaceLandmarks = interface;
  VNFaceLandmarks2D = interface;
  VNRequest = interface;
  VNImageBasedRequest = interface;
  VNRequestProgressProviding = interface;
  VNFaceObservationAccepting = interface;
  VNClassifyImageRequest = interface;
  VNDetectBarcodesRequest = interface;
  VNDetectFaceRectanglesRequest = interface;
  VNDetectFaceLandmarksRequest = interface;
  VNDetectFaceCaptureQualityRequest = interface;
  VNDetectHorizonRequest = interface;
  VNDetectRectanglesRequest = interface;
  VNDetectTextRectanglesRequest = interface;
  VNRecognizeTextRequest = interface;
  VNGenerateAttentionBasedSaliencyImageRequest = interface;
  VNGenerateObjectnessBasedSaliencyImageRequest = interface;
  VNGenerateImageFeaturePrintRequest = interface;
  VNCoreMLModel = interface;
  VNCoreMLRequest = interface;
  VNImageRequestHandler = interface;
  VNSequenceRequestHandler = interface;
  VNTargetedImageRequest = interface;
  VNObservation = interface;
  VNDetectedObjectObservation = interface;
  VNFaceObservation = interface;
  VNClassificationObservation = interface;
  VNRecognizedObjectObservation = interface;
  VNCoreMLFeatureValueObservation = interface;
  VNPixelBufferObservation = interface;
  VNRectangleObservation = interface;
  VNTextObservation = interface;
  VNRecognizedText = interface;
  VNRecognizedTextObservation = interface;
  VNBarcodeObservation = interface;
  VNHorizonObservation = interface;
  VNImageAlignmentObservation = interface;
  VNImageTranslationAlignmentObservation = interface;
  VNImageHomographicAlignmentObservation = interface;
  VNSaliencyImageObservation = interface;
  VNFeaturePrintObservation = interface;
  VNImageRegistrationRequest = interface;
  VNTranslationalImageRegistrationRequest = interface;
  VNHomographicImageRegistrationRequest = interface;
  VNTrackingRequest = interface;
  VNTrackObjectRequest = interface;
  VNTrackRectangleRequest = interface;
  VNDetectHumanRectanglesRequest = interface;
  VNRecognizeAnimalsRequest = interface;

  CGImagePropertyOrientation = NSUInteger;
  VNConfidence = Single;
  VNAspectRatio = Single;
  VNDegrees = Single;
  VNImageCropAndScaleOption = NSInteger;
  VNBarcodeSymbology = NSString;
  VNElementType = NSInteger;
  VNErrorCode = NSInteger;

  VNRequestCompletionHandler = procedure(request: VNRequest; error: NSError) of object;

  VNRequestProgressHandler = procedure(request: VNRequest; fractionCompleted: Double; error: NSError) of object;
  VNRequestFaceLandmarksConstellation = NSInteger;
  VNRequestTextRecognitionLevel = NSInteger;
  VNImageOption = NSString;
  VNRequestTrackingLevel = NSInteger;
  VNAnimalIdentifier = NSString;
  VNAnimalDetector = NSString;

  VNRequestRevisionProviding = interface(IObjectiveC)
    ['{F3EFABFA-32B8-4AE0-9CCE-75E22A0E5DA7}']
    function requestRevision: NSUInteger; cdecl;
  end;

  VNFaceLandmarkRegionClass = interface(NSObjectClass)
    ['{679FBFED-ECEC-44F5-B95F-90AA02A1CA21}']
  end;

  VNFaceLandmarkRegion = interface(NSObject)
    ['{75286406-576F-412A-93C3-FC4A8665325D}']
    function pointCount: NSUInteger; cdecl;
  end;
  TVNFaceLandmarkRegion = class(TOCGenericImport<VNFaceLandmarkRegionClass, VNFaceLandmarkRegion>) end;

  VNFaceLandmarkRegion2DClass = interface(VNFaceLandmarkRegionClass)
    ['{9BF56F8A-CAF0-43D4-B0A6-A03A20A2EA17}']
  end;

  VNFaceLandmarkRegion2D = interface(VNFaceLandmarkRegion)
    ['{CB0CACB1-6F10-402E-A284-A7E2872FFB64}']
    function normalizedPoints: PCGPoint; cdecl;
    function pointsInImageOfSize(imageSize: CGSize): PCGPoint; cdecl;
    function precisionEstimatesPerPoint: NSArray; cdecl;
  end;
  TVNFaceLandmarkRegion2D = class(TOCGenericImport<VNFaceLandmarkRegion2DClass, VNFaceLandmarkRegion2D>) end;

  VNFaceLandmarksClass = interface(NSObjectClass)
    ['{0535411F-859B-4541-9055-F4B14198DF95}']
  end;

  VNFaceLandmarks = interface(NSObject)
    ['{8561CA6A-ABEF-4628-BC13-2437194AF32C}']
    function confidence: VNConfidence; cdecl;
  end;
  TVNFaceLandmarks = class(TOCGenericImport<VNFaceLandmarksClass, VNFaceLandmarks>) end;

  VNFaceLandmarks2DClass = interface(VNFaceLandmarksClass)
    ['{4D08C4C4-2F48-4276-A7B5-734E44E10494}']
  end;

  VNFaceLandmarks2D = interface(VNFaceLandmarks)
    ['{C69C3FE9-5E5B-4E34-9635-9822A2FCFF8C}']
    function allPoints: VNFaceLandmarkRegion2D; cdecl;
    function faceContour: VNFaceLandmarkRegion2D; cdecl;
    function innerLips: VNFaceLandmarkRegion2D; cdecl;
    function leftEye: VNFaceLandmarkRegion2D; cdecl;
    function leftEyebrow: VNFaceLandmarkRegion2D; cdecl;
    function leftPupil: VNFaceLandmarkRegion2D; cdecl;
    function medianLine: VNFaceLandmarkRegion2D; cdecl;
    function nose: VNFaceLandmarkRegion2D; cdecl;
    function noseCrest: VNFaceLandmarkRegion2D; cdecl;
    function outerLips: VNFaceLandmarkRegion2D; cdecl;
    function rightEye: VNFaceLandmarkRegion2D; cdecl;
    function rightEyebrow: VNFaceLandmarkRegion2D; cdecl;
    function rightPupil: VNFaceLandmarkRegion2D; cdecl;
  end;
  TVNFaceLandmarks2D = class(TOCGenericImport<VNFaceLandmarks2DClass, VNFaceLandmarks2D>) end;

  VNRequestClass = interface(NSObjectClass)
    ['{7FD313CB-6227-4074-BA41-423284689BBE}']
    {class} function currentRevision: NSUInteger; cdecl;
    {class} function defaultRevision: NSUInteger; cdecl;
    {class} function supportedRevisions: NSIndexSet; cdecl;
  end;

  VNRequest = interface(NSObject)
    ['{DE3DD0A7-E4B8-43D6-944D-0C8633C569B0}']
    procedure cancel; cdecl;
    function completionHandler: VNRequestCompletionHandler; cdecl;
    function initWithCompletionHandler(completionHandler: VNRequestCompletionHandler): Pointer; cdecl;
    function preferBackgroundProcessing: Boolean; cdecl;
    function results: NSArray; cdecl;
    function revision: NSUInteger; cdecl;
    procedure setPreferBackgroundProcessing(preferBackgroundProcessing: Boolean); cdecl;
    procedure setRevision(revision: NSUInteger); cdecl;
    procedure setUsesCPUOnly(usesCPUOnly: Boolean); cdecl;
    function usesCPUOnly: Boolean; cdecl;
  end;
  TVNRequest = class(TOCGenericImport<VNRequestClass, VNRequest>) end;

  VNImageBasedRequestClass = interface(VNRequestClass)
    ['{10BB566F-39FD-44CC-8EBD-28FA6669E03C}']
  end;

  VNImageBasedRequest = interface(VNRequest)
    ['{9EA37C4C-8429-4A30-AFDB-F39BC0DDEDB8}']
    function regionOfInterest: CGRect; cdecl;
    procedure setRegionOfInterest(regionOfInterest: CGRect); cdecl;
  end;
  TVNImageBasedRequest = class(TOCGenericImport<VNImageBasedRequestClass, VNImageBasedRequest>) end;

  VNRequestProgressProviding = interface(IObjectiveC)
    ['{3EB026E0-6027-4F3D-BA13-1F6905B6155B}']
    function indeterminate: Boolean; cdecl;
    function progressHandler: VNRequestProgressHandler; cdecl;
    procedure setProgressHandler(progressHandler: VNRequestProgressHandler); cdecl;
  end;

  VNFaceObservationAccepting = interface(IObjectiveC)
    ['{57A474EC-FA16-49FA-BC09-9C67AB15DADC}']
    function inputFaceObservations: NSArray; cdecl;
    procedure setInputFaceObservations(inputFaceObservations: NSArray); cdecl;
  end;

  VNClassifyImageRequestClass = interface(VNImageBasedRequestClass)
    ['{F3141519-6B93-4218-B47B-13BA08EE43BA}']
    [MethodName('knownClassificationsForRevision:error:')]
    {class} function knownClassificationsForRevision(requestRevision: NSUInteger; error: PPointer): NSArray; cdecl;
  end;

  VNClassifyImageRequest = interface(VNImageBasedRequest)
    ['{5EC964B7-8294-4302-955F-AD4189E686E1}']
  end;
  TVNClassifyImageRequest = class(TOCGenericImport<VNClassifyImageRequestClass, VNClassifyImageRequest>) end;

  VNDetectBarcodesRequestClass = interface(VNImageBasedRequestClass)
    ['{D887844A-A099-4952-BDFE-DD505CD1703B}']
    {class} function supportedSymbologies: NSArray; cdecl;
  end;

  VNDetectBarcodesRequest = interface(VNImageBasedRequest)
    ['{61A74538-698F-40FF-AE49-A795220FDB31}']
    procedure setSymbologies(symbologies: NSArray); cdecl;
    function symbologies: NSArray; cdecl;
  end;
  TVNDetectBarcodesRequest = class(TOCGenericImport<VNDetectBarcodesRequestClass, VNDetectBarcodesRequest>) end;

  VNDetectFaceRectanglesRequestClass = interface(VNImageBasedRequestClass)
    ['{5D661869-AA5B-455C-920F-1BCA48B60F34}']
  end;

  VNDetectFaceRectanglesRequest = interface(VNImageBasedRequest)
    ['{88F4B644-E34E-4817-9180-710B4B0BE4CB}']
  end;
  TVNDetectFaceRectanglesRequest = class(TOCGenericImport<VNDetectFaceRectanglesRequestClass, VNDetectFaceRectanglesRequest>) end;

  VNDetectFaceLandmarksRequestClass = interface(VNImageBasedRequestClass)
    ['{78EEE0B5-4323-47C2-B2D2-4961D34CC566}']
    [MethodName('revision:supportsConstellation:')]
    {class} function revision(requestRevision: NSUInteger; constellation: VNRequestFaceLandmarksConstellation): Boolean; cdecl;
  end;

  VNDetectFaceLandmarksRequest = interface(VNImageBasedRequest)
    ['{68F05F21-3373-44EF-95D9-F462D095CF3C}']
    function constellation: VNRequestFaceLandmarksConstellation; cdecl;
    procedure setConstellation(constellation: VNRequestFaceLandmarksConstellation); cdecl;
  end;
  TVNDetectFaceLandmarksRequest = class(TOCGenericImport<VNDetectFaceLandmarksRequestClass, VNDetectFaceLandmarksRequest>) end;

  VNDetectFaceCaptureQualityRequestClass = interface(VNImageBasedRequestClass)
    ['{41CB5C91-7786-40EE-9EAF-FB2E1E08C9C2}']
  end;

  VNDetectFaceCaptureQualityRequest = interface(VNImageBasedRequest)
    ['{821448A9-3F2F-4F09-9C4B-6FC5E690EBBA}']
  end;
  TVNDetectFaceCaptureQualityRequest = class(TOCGenericImport<VNDetectFaceCaptureQualityRequestClass, VNDetectFaceCaptureQualityRequest>) end;

  VNDetectHorizonRequestClass = interface(VNImageBasedRequestClass)
    ['{9FAA17F7-CFEC-4CEA-8503-1A9E2B76BFC9}']
  end;

  VNDetectHorizonRequest = interface(VNImageBasedRequest)
    ['{CAFCAEEA-3EFC-466E-BCE2-CF19F87CE15A}']
  end;
  TVNDetectHorizonRequest = class(TOCGenericImport<VNDetectHorizonRequestClass, VNDetectHorizonRequest>) end;

  VNDetectRectanglesRequestClass = interface(VNImageBasedRequestClass)
    ['{1C0FBCC9-8C5A-4F8C-B76F-A39C60E26ABF}']
  end;

  VNDetectRectanglesRequest = interface(VNImageBasedRequest)
    ['{952169B5-3651-4467-85E0-6413AC30B964}']
    function maximumAspectRatio: VNAspectRatio; cdecl;
    function maximumObservations: NSUInteger; cdecl;
    function minimumAspectRatio: VNAspectRatio; cdecl;
    function minimumConfidence: VNConfidence; cdecl;
    function minimumSize: Single; cdecl;
    function quadratureTolerance: VNDegrees; cdecl;
    procedure setMaximumAspectRatio(maximumAspectRatio: VNAspectRatio); cdecl;
    procedure setMaximumObservations(maximumObservations: NSUInteger); cdecl;
    procedure setMinimumAspectRatio(minimumAspectRatio: VNAspectRatio); cdecl;
    procedure setMinimumConfidence(minimumConfidence: VNConfidence); cdecl;
    procedure setMinimumSize(minimumSize: Single); cdecl;
    procedure setQuadratureTolerance(quadratureTolerance: VNDegrees); cdecl;
  end;
  TVNDetectRectanglesRequest = class(TOCGenericImport<VNDetectRectanglesRequestClass, VNDetectRectanglesRequest>) end;

  VNDetectTextRectanglesRequestClass = interface(VNImageBasedRequestClass)
    ['{EA2EEB73-0F61-4507-864F-963B2B27EAA1}']
  end;

  VNDetectTextRectanglesRequest = interface(VNImageBasedRequest)
    ['{7A1D8F4A-576B-46F6-8E04-62972293A279}']
    function reportCharacterBoxes: Boolean; cdecl;
    procedure setReportCharacterBoxes(reportCharacterBoxes: Boolean); cdecl;
  end;
  TVNDetectTextRectanglesRequest = class(TOCGenericImport<VNDetectTextRectanglesRequestClass, VNDetectTextRectanglesRequest>) end;

  VNRecognizeTextRequestClass = interface(VNImageBasedRequestClass)
    ['{569D531D-ADA6-4444-8FB6-09033A7D48B6}']
    [MethodName('supportedRecognitionLanguagesForTextRecognitionLevel:revision:error:')]
    {class} function supportedRecognitionLanguagesForTextRecognitionLevel(recognitionLevel: VNRequestTextRecognitionLevel;
      requestRevision: NSUInteger; error: PPointer): NSArray; cdecl;
  end;

  VNRecognizeTextRequest = interface(VNImageBasedRequest)
    ['{BD11292E-D522-4562-9E3C-4EA81226C34D}']
    function customWords: NSArray; cdecl;
    function minimumTextHeight: Single; cdecl;
    function recognitionLanguages: NSArray; cdecl;
    function recognitionLevel: VNRequestTextRecognitionLevel; cdecl;
    procedure setCustomWords(customWords: NSArray); cdecl;
    procedure setMinimumTextHeight(minimumTextHeight: Single); cdecl;
    procedure setRecognitionLanguages(recognitionLanguages: NSArray); cdecl;
    procedure setRecognitionLevel(recognitionLevel: VNRequestTextRecognitionLevel); cdecl;
    procedure setUsesLanguageCorrection(usesLanguageCorrection: Boolean); cdecl;
    function usesLanguageCorrection: Boolean; cdecl;
  end;
  TVNRecognizeTextRequest = class(TOCGenericImport<VNRecognizeTextRequestClass, VNRecognizeTextRequest>) end;

  VNGenerateAttentionBasedSaliencyImageRequestClass = interface(VNImageBasedRequestClass)
    ['{B191C2FF-915A-45F4-9892-66466E218554}']
  end;

  VNGenerateAttentionBasedSaliencyImageRequest = interface(VNImageBasedRequest)
    ['{490546B9-1AAD-4D0D-B2F1-87EBC426D0EB}']
  end;
  TVNGenerateAttentionBasedSaliencyImageRequest = class(TOCGenericImport<VNGenerateAttentionBasedSaliencyImageRequestClass,
    VNGenerateAttentionBasedSaliencyImageRequest>) end;

  VNGenerateObjectnessBasedSaliencyImageRequestClass = interface(VNImageBasedRequestClass)
    ['{08919C1C-3815-4B09-A5A3-FD1464B32C60}']
  end;

  VNGenerateObjectnessBasedSaliencyImageRequest = interface(VNImageBasedRequest)
    ['{ECBFB97C-77DA-41E9-94CD-0AEB087CEFF4}']
  end;
  TVNGenerateObjectnessBasedSaliencyImageRequest = class(TOCGenericImport<VNGenerateObjectnessBasedSaliencyImageRequestClass,
    VNGenerateObjectnessBasedSaliencyImageRequest>) end;

  VNGenerateImageFeaturePrintRequestClass = interface(VNImageBasedRequestClass)
    ['{D19BD30F-31AF-400B-87BC-FA7F54DA79EE}']
  end;

  VNGenerateImageFeaturePrintRequest = interface(VNImageBasedRequest)
    ['{BEC30D93-651A-48B3-B90B-69240DB0A63D}']
    function imageCropAndScaleOption: VNImageCropAndScaleOption; cdecl;
    procedure setImageCropAndScaleOption(imageCropAndScaleOption: VNImageCropAndScaleOption); cdecl;
  end;
  TVNGenerateImageFeaturePrintRequest = class(TOCGenericImport<VNGenerateImageFeaturePrintRequestClass, VNGenerateImageFeaturePrintRequest>) end;

  VNCoreMLModelClass = interface(NSObjectClass)
    ['{29DFE401-55B6-40C7-9E76-D6F24DBB20EB}']
    [MethodName('modelForMLModel:error:')]
    {class} function modelForMLModel(model: Pointer; error: PPointer): Pointer; cdecl;
  end;

  VNCoreMLModel = interface(NSObject)
    ['{F17B5E61-8865-4837-AD35-FA7796DF6391}']
    function featureProvider: Pointer; cdecl;
    function inputImageFeatureName: NSString; cdecl;
    procedure setFeatureProvider(featureProvider: Pointer); cdecl;
    procedure setInputImageFeatureName(inputImageFeatureName: NSString); cdecl;
  end;
  TVNCoreMLModel = class(TOCGenericImport<VNCoreMLModelClass, VNCoreMLModel>) end;

  VNCoreMLRequestClass = interface(VNImageBasedRequestClass)
    ['{A03FC4F5-3EA2-4149-B696-3E1984F19726}']
  end;

  VNCoreMLRequest = interface(VNImageBasedRequest)
    ['{4FB4826A-9A5D-48E6-B960-FA27EEA52416}']
    function imageCropAndScaleOption: VNImageCropAndScaleOption; cdecl;
    function initWithCompletionHandler(completionHandler: VNRequestCompletionHandler): Pointer; cdecl;
    [MethodName('initWithModel:completionHandler:')]
    function initWithModel(model: VNCoreMLModel; completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    function initWithModel(model: VNCoreMLModel): Pointer; overload; cdecl;
    function model: VNCoreMLModel; cdecl;
    procedure setImageCropAndScaleOption(imageCropAndScaleOption: VNImageCropAndScaleOption); cdecl;
  end;
  TVNCoreMLRequest = class(TOCGenericImport<VNCoreMLRequestClass, VNCoreMLRequest>) end;

  VNImageRequestHandlerClass = interface(NSObjectClass)
    ['{88F6B01E-D7DA-464E-A67D-95C72C69615A}']
  end;

  VNImageRequestHandler = interface(NSObject)
    ['{B3FC51F2-0248-41D9-AB8D-B0DB437F8520}']
    [MethodName('initWithCGImage:orientation:options:')]
    function initWithCGImage(image: CGImageRef; orientation: CGImagePropertyOrientation; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithCGImage:options:')]
    function initWithCGImage(image: CGImageRef; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithCIImage:options:')]
    function initWithCIImage(image: CIImage; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithCIImage:orientation:options:')]
    function initWithCIImage(image: CIImage; orientation: CGImagePropertyOrientation; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithCVPixelBuffer:options:')]
    function initWithCVPixelBuffer(pixelBuffer: CVPixelBufferRef; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithCVPixelBuffer:orientation:options:')]
    function initWithCVPixelBuffer(pixelBuffer: CVPixelBufferRef; orientation: CGImagePropertyOrientation;
      options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithData:options:')]
    function initWithData(imageData: NSData; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithData:orientation:options:')]
    function initWithData(imageData: NSData; orientation: CGImagePropertyOrientation; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithURL:options:')]
    function initWithURL(imageURL: NSURL; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithURL:orientation:options:')]
    function initWithURL(imageURL: NSURL; orientation: CGImagePropertyOrientation; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('performRequests:error:')]
    function performRequests(requests: NSArray; error: PPointer): Boolean; cdecl;
  end;
  TVNImageRequestHandler = class(TOCGenericImport<VNImageRequestHandlerClass, VNImageRequestHandler>) end;

  VNSequenceRequestHandlerClass = interface(NSObjectClass)
    ['{014BAC33-A415-4CE0-BD8B-50DE4D079777}']
  end;

  VNSequenceRequestHandler = interface(NSObject)
    ['{F483AC1D-2E59-4B31-8F58-B76743DF470C}']
    [MethodName('performRequests:onCIImage:orientation:error:')]
    function performRequests(requests: NSArray; image: CIImage; orientation: CGImagePropertyOrientation; error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onImageURL:error:')]
    function performRequests(requests: NSArray; imageURL: NSURL; error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onImageURL:orientation:error:')]
    function performRequests(requests: NSArray; imageURL: NSURL; orientation: CGImagePropertyOrientation; error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onImageData:error:')]
    function performRequests(requests: NSArray; imageData: NSData; error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onImageData:orientation:error:')]
    function performRequests(requests: NSArray; imageData: NSData; orientation: CGImagePropertyOrientation; error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onCVPixelBuffer:error:')]
    function performRequests(requests: NSArray; pixelBuffer: CVPixelBufferRef; error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onCVPixelBuffer:orientation:error:')]
    function performRequests(requests: NSArray; pixelBuffer: CVPixelBufferRef; orientation: CGImagePropertyOrientation;
      error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onCGImage:error:')]
    function performRequests(requests: NSArray; image: CGImageRef; error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onCGImage:orientation:error:')]
    function performRequests(requests: NSArray; image: CGImageRef; orientation: CGImagePropertyOrientation; error: PPointer): Boolean; overload; cdecl;
    [MethodName('performRequests:onCIImage:error:')]
    function performRequests(requests: NSArray; image: CIImage; error: PPointer): Boolean; overload; cdecl;
  end;
  TVNSequenceRequestHandler = class(TOCGenericImport<VNSequenceRequestHandlerClass, VNSequenceRequestHandler>) end;

  VNTargetedImageRequestClass = interface(VNImageBasedRequestClass)
    ['{13343F80-9B41-4B25-8D17-E8713801C3C0}']
  end;

  VNTargetedImageRequest = interface(VNImageBasedRequest)
    ['{1F613586-30DC-4841-91E6-E5049F747D7C}']
    function initWithCompletionHandler(completionHandler: VNRequestCompletionHandler): Pointer; cdecl;
    [MethodName('initWithTargetedCGImage:options:completionHandler:')]
    function initWithTargetedCGImage(cgImage: CGImageRef; options: NSDictionary;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCGImage:options:')]
    function initWithTargetedCGImage(cgImage: CGImageRef; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCGImage:orientation:options:')]
    function initWithTargetedCGImage(cgImage: CGImageRef; orientation: CGImagePropertyOrientation; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCGImage:orientation:options:completionHandler:')]
    function initWithTargetedCGImage(cgImage: CGImageRef; orientation: CGImagePropertyOrientation; options: NSDictionary;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCIImage:orientation:options:completionHandler:')]
    function initWithTargetedCIImage(ciImage: CIImage; orientation: CGImagePropertyOrientation; options: NSDictionary;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCIImage:orientation:options:')]
    function initWithTargetedCIImage(ciImage: CIImage; orientation: CGImagePropertyOrientation; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCIImage:options:completionHandler:')]
    function initWithTargetedCIImage(ciImage: CIImage; options: NSDictionary; completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCIImage:options:')]
    function initWithTargetedCIImage(ciImage: CIImage; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCVPixelBuffer:options:')]
    function initWithTargetedCVPixelBuffer(pixelBuffer: CVPixelBufferRef; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCVPixelBuffer:options:completionHandler:')]
    function initWithTargetedCVPixelBuffer(pixelBuffer: CVPixelBufferRef; options: NSDictionary;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCVPixelBuffer:orientation:options:completionHandler:')]
    function initWithTargetedCVPixelBuffer(pixelBuffer: CVPixelBufferRef; orientation: CGImagePropertyOrientation; options: NSDictionary;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedCVPixelBuffer:orientation:options:')]
    function initWithTargetedCVPixelBuffer(pixelBuffer: CVPixelBufferRef; orientation: CGImagePropertyOrientation;
      options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedImageData:options:completionHandler:')]
    function initWithTargetedImageData(imageData: NSData; options: NSDictionary;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedImageData:options:')]
    function initWithTargetedImageData(imageData: NSData; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedImageData:orientation:options:')]
    function initWithTargetedImageData(imageData: NSData; orientation: CGImagePropertyOrientation; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedImageData:orientation:options:completionHandler:')]
    function initWithTargetedImageData(imageData: NSData; orientation: CGImagePropertyOrientation; options: NSDictionary;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedImageURL:options:')]
    function initWithTargetedImageURL(imageURL: NSURL; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedImageURL:options:completionHandler:')]
    function initWithTargetedImageURL(imageURL: NSURL; options: NSDictionary; completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    [MethodName('initWithTargetedImageURL:orientation:options:')]
    function initWithTargetedImageURL(imageURL: NSURL; orientation: CGImagePropertyOrientation; options: NSDictionary): Pointer; overload; cdecl;
    [MethodName('initWithTargetedImageURL:orientation:options:completionHandler:')]
    function initWithTargetedImageURL(imageURL: NSURL; orientation: CGImagePropertyOrientation; options: NSDictionary;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
  end;
  TVNTargetedImageRequest = class(TOCGenericImport<VNTargetedImageRequestClass, VNTargetedImageRequest>) end;

  VNObservationClass = interface(NSObjectClass)
    ['{FAA2BF58-C06A-48D0-993A-6652DAAA78A4}']
  end;

  VNObservation = interface(NSObject)
    ['{CD667FD4-F0FD-46F3-8596-38130979C180}']
    function confidence: VNConfidence; cdecl;
    function uuid: NSUUID; cdecl;
  end;
  TVNObservation = class(TOCGenericImport<VNObservationClass, VNObservation>) end;

  VNDetectedObjectObservationClass = interface(VNObservationClass)
    ['{88631C06-897D-411A-8B4A-700CA62B0B49}']
    {class} function observationWithBoundingBox(boundingBox: CGRect): Pointer; cdecl;
    [MethodName('observationWithRequestRevision:boundingBox:')]
    {class} function observationWithRequestRevision(requestRevision: NSUInteger; boundingBox: CGRect): Pointer; cdecl;
  end;

  VNDetectedObjectObservation = interface(VNObservation)
    ['{C64887FD-72FB-4FDD-BD7D-A2554E799169}']
    function boundingBox: CGRect; cdecl;
  end;
  TVNDetectedObjectObservation = class(TOCGenericImport<VNDetectedObjectObservationClass, VNDetectedObjectObservation>) end;

  VNFaceObservationClass = interface(VNDetectedObjectObservationClass)
    ['{2378CFEA-7625-488B-A278-781DBA8B07E1}']
    [MethodName('faceObservationWithRequestRevision:boundingBox:roll:yaw:')]
    {class} function faceObservationWithRequestRevision(requestRevision: NSUInteger; boundingBox: CGRect; roll: NSNumber;
      yaw: NSNumber): Pointer; cdecl;
  end;

  VNFaceObservation = interface(VNDetectedObjectObservation)
    ['{50CF6A97-2DA9-42FA-B3E1-A6D18397BE10}']
    function faceCaptureQuality: NSNumber; cdecl;
    function landmarks: VNFaceLandmarks2D; cdecl;
    function roll: NSNumber; cdecl;
    function yaw: NSNumber; cdecl;
  end;
  TVNFaceObservation = class(TOCGenericImport<VNFaceObservationClass, VNFaceObservation>) end;

  VNClassificationObservationClass = interface(VNObservationClass)
    ['{8998AD9B-84FA-4AD0-9ABB-678F67913A63}']
  end;

  VNClassificationObservation = interface(VNObservation)
    ['{68A5EB25-AE13-40CB-B8BA-D20B07BD8F3D}']
    [MethodName('hasMinimumPrecision:forRecall:')]
    function hasMinimumPrecision(minimumPrecision: Single; recall: Single): Boolean; cdecl;
    [MethodName('hasMinimumRecall:forPrecision:')]
    function hasMinimumRecall(minimumRecall: Single; precision: Single): Boolean; cdecl;
    function hasPrecisionRecallCurve: Boolean; cdecl;
    function identifier: NSString; cdecl;
  end;
  TVNClassificationObservation = class(TOCGenericImport<VNClassificationObservationClass, VNClassificationObservation>) end;

  VNRecognizedObjectObservationClass = interface(VNDetectedObjectObservationClass)
    ['{1955E89D-CC41-4F4A-854B-1DDDBAB92786}']
  end;

  VNRecognizedObjectObservation = interface(VNDetectedObjectObservation)
    ['{E8D984AE-92A8-431F-9BF7-DD513E8FFD53}']
    function labels: NSArray; cdecl;
  end;
  TVNRecognizedObjectObservation = class(TOCGenericImport<VNRecognizedObjectObservationClass, VNRecognizedObjectObservation>) end;

  VNCoreMLFeatureValueObservationClass = interface(VNObservationClass)
    ['{071211EA-0EF5-4E3F-AFAA-AE77F1BF6E18}']
  end;

  VNCoreMLFeatureValueObservation = interface(VNObservation)
    ['{B7530F8E-5BEB-4127-B038-AC23B8B37EEA}']
    function featureName: NSString; cdecl;
    // function featureValue: MLFeatureValue; cdecl;
  end;
  TVNCoreMLFeatureValueObservation = class(TOCGenericImport<VNCoreMLFeatureValueObservationClass, VNCoreMLFeatureValueObservation>) end;

  VNPixelBufferObservationClass = interface(VNObservationClass)
    ['{B34C00BD-5255-4FBF-9915-E39F6F469792}']
  end;

  VNPixelBufferObservation = interface(VNObservation)
    ['{7C82E69B-3D60-46E6-89FF-12C730071CAA}']
    function featureName: NSString; cdecl;
    function pixelBuffer: CVPixelBufferRef; cdecl;
  end;
  TVNPixelBufferObservation = class(TOCGenericImport<VNPixelBufferObservationClass, VNPixelBufferObservation>) end;

  VNRectangleObservationClass = interface(VNDetectedObjectObservationClass)
    ['{097D54BD-F30A-4FE5-BCB2-4FF86F38DE60}']
    [MethodName('rectangleObservationWithRequestRevision:topLeft:bottomLeft:bottomRight:topRight:')]
    {class} function rectangleObservationWithRequestRevision(requestRevision: NSUInteger; topLeft: CGPoint; bottomLeft: CGPoint;
      bottomRight: CGPoint; topRight: CGPoint): Pointer; cdecl;
  end;

  VNRectangleObservation = interface(VNDetectedObjectObservation)
    ['{E27B4615-CB07-42DA-A1A2-13FD4E5E4A52}']
    function bottomLeft: CGPoint; cdecl;
    function bottomRight: CGPoint; cdecl;
    function topLeft: CGPoint; cdecl;
    function topRight: CGPoint; cdecl;
  end;
  TVNRectangleObservation = class(TOCGenericImport<VNRectangleObservationClass, VNRectangleObservation>) end;

  VNTextObservationClass = interface(VNRectangleObservationClass)
    ['{33F53088-CC0C-4B81-BF58-90F1C87C7EC0}']
  end;

  VNTextObservation = interface(VNRectangleObservation)
    ['{244B9DDA-CB2E-4B72-BCE2-CFF73CAE43A4}']
    function characterBoxes: NSArray; cdecl;
  end;
  TVNTextObservation = class(TOCGenericImport<VNTextObservationClass, VNTextObservation>) end;

  VNRecognizedTextClass = interface(NSObjectClass)
    ['{15CB239B-8421-4CB5-A9D1-82B7CC5AE888}']
  end;

  VNRecognizedText = interface(NSObject)
    ['{1D4BC739-B9AD-4005-971F-4CA5EC05EBDB}']
    function &string: NSString; cdecl;
    [MethodName('boundingBoxForRange:error:')]
    function boundingBoxForRange(range: NSRange; error: PPointer): VNRectangleObservation; cdecl;
    function confidence: VNConfidence; cdecl;
  end;
  TVNRecognizedText = class(TOCGenericImport<VNRecognizedTextClass, VNRecognizedText>) end;

  VNRecognizedTextObservationClass = interface(VNRectangleObservationClass)
    ['{B6245C18-94B1-4D4F-A516-781ADAE9D133}']
  end;

  VNRecognizedTextObservation = interface(VNRectangleObservation)
    ['{A01EC03B-1E9A-4C69-A050-FAB61D471B56}']
    function topCandidates(maxCandidateCount: NSUInteger): NSArray; cdecl;
  end;
  TVNRecognizedTextObservation = class(TOCGenericImport<VNRecognizedTextObservationClass, VNRecognizedTextObservation>) end;

  VNBarcodeObservationClass = interface(VNRectangleObservationClass)
    ['{B2F31381-3753-4F6C-A6DE-A264BBBDCBA6}']
  end;

  VNBarcodeObservation = interface(VNRectangleObservation)
    ['{9DC1704E-28C1-4D84-8C87-55C4DBE90002}']
    function barcodeDescriptor: CIBarcodeDescriptor; cdecl;
    function payloadStringValue: NSString; cdecl;
    function symbology: VNBarcodeSymbology; cdecl;
  end;
  TVNBarcodeObservation = class(TOCGenericImport<VNBarcodeObservationClass, VNBarcodeObservation>) end;

  VNHorizonObservationClass = interface(VNObservationClass)
    ['{DE078084-CED3-4E67-A3CC-95477593B559}']
  end;

  VNHorizonObservation = interface(VNObservation)
    ['{A0F88DF4-F689-4490-A1BF-1E1B2B352AD4}']
    function angle: CGFloat; cdecl;
    function transform: CGAffineTransform; cdecl;
  end;
  TVNHorizonObservation = class(TOCGenericImport<VNHorizonObservationClass, VNHorizonObservation>) end;

  VNImageAlignmentObservationClass = interface(VNObservationClass)
    ['{AD72AF3F-02F0-48CB-93CE-405502A3AB63}']
  end;

  VNImageAlignmentObservation = interface(VNObservation)
    ['{B3D5188F-8525-4A3D-87A9-E892246E5E3D}']
  end;
  TVNImageAlignmentObservation = class(TOCGenericImport<VNImageAlignmentObservationClass, VNImageAlignmentObservation>) end;

  VNImageTranslationAlignmentObservationClass = interface(VNImageAlignmentObservationClass)
    ['{51339B71-B08A-4918-A0C7-D0A0C9F11E65}']
  end;

  VNImageTranslationAlignmentObservation = interface(VNImageAlignmentObservation)
    ['{55EA811D-23B1-49C2-A396-BE2B7E5904ED}']
    function alignmentTransform: CGAffineTransform; cdecl;
    procedure setAlignmentTransform(alignmentTransform: CGAffineTransform); cdecl;
  end;
  TVNImageTranslationAlignmentObservation = class(TOCGenericImport<VNImageTranslationAlignmentObservationClass,
    VNImageTranslationAlignmentObservation>) end;

  VNImageHomographicAlignmentObservationClass = interface(VNImageAlignmentObservationClass)
    ['{97450AAD-114C-47BF-B626-085133DAB934}']
  end;

  VNImageHomographicAlignmentObservation = interface(VNImageAlignmentObservation)
    ['{17A0EC10-37C6-4D22-BCA7-11DFB009A0A8}']
    // procedure setWarpTransform(warpTransform: matrix_float3x3); cdecl;
    // function warpTransform: matrix_float3x3; cdecl;
  end;
  TVNImageHomographicAlignmentObservation = class(TOCGenericImport<VNImageHomographicAlignmentObservationClass,
    VNImageHomographicAlignmentObservation>) end;

  VNSaliencyImageObservationClass = interface(VNPixelBufferObservationClass)
    ['{DA23CCAA-C14E-45B7-84E3-926DD55E4BB0}']
  end;

  VNSaliencyImageObservation = interface(VNPixelBufferObservation)
    ['{5414E7A3-2D6E-4567-B43F-F0071581E0CF}']
    function salientObjects: NSArray; cdecl;
  end;
  TVNSaliencyImageObservation = class(TOCGenericImport<VNSaliencyImageObservationClass, VNSaliencyImageObservation>) end;

  VNFeaturePrintObservationClass = interface(VNObservationClass)
    ['{8A02DD75-DE8C-4D8D-93FE-74F85B25E069}']
  end;

  VNFeaturePrintObservation = interface(VNObservation)
    ['{8C019283-086B-4663-B84D-1FFE17537B03}']
    [MethodName('computeDistance:toFeaturePrintObservation:error:')]
    function computeDistance(outDistance: PSingle; featurePrint: VNFeaturePrintObservation; error: PPointer): Boolean; cdecl;
    function data: NSData; cdecl;
    function elementCount: NSUInteger; cdecl;
    function elementType: VNElementType; cdecl;
  end;
  TVNFeaturePrintObservation = class(TOCGenericImport<VNFeaturePrintObservationClass, VNFeaturePrintObservation>) end;

  VNImageRegistrationRequestClass = interface(VNTargetedImageRequestClass)
    ['{C0663DAB-E75E-4E05-94D4-12CAB6FC5CA1}']
  end;

  VNImageRegistrationRequest = interface(VNTargetedImageRequest)
    ['{A31A53AE-F8E2-491B-9C95-BE625C2A3766}']
  end;
  TVNImageRegistrationRequest = class(TOCGenericImport<VNImageRegistrationRequestClass, VNImageRegistrationRequest>) end;

  VNTranslationalImageRegistrationRequestClass = interface(VNImageRegistrationRequestClass)
    ['{40017E25-42AE-4900-B841-078117FF50D5}']
  end;

  VNTranslationalImageRegistrationRequest = interface(VNImageRegistrationRequest)
    ['{F2B2FFBD-AD51-43D7-9467-0DF4C88E084F}']
  end;
  TVNTranslationalImageRegistrationRequest = class(TOCGenericImport<VNTranslationalImageRegistrationRequestClass,
    VNTranslationalImageRegistrationRequest>) end;

  VNHomographicImageRegistrationRequestClass = interface(VNImageRegistrationRequestClass)
    ['{DCE0F540-2701-44B3-AB8B-9D72F71876A4}']
  end;

  VNHomographicImageRegistrationRequest = interface(VNImageRegistrationRequest)
    ['{53F304C6-C88E-4ECD-B3A8-25B87C22C7A2}']
  end;
  TVNHomographicImageRegistrationRequest = class(TOCGenericImport<VNHomographicImageRegistrationRequestClass,
    VNHomographicImageRegistrationRequest>) end;

  VNTrackingRequestClass = interface(VNImageBasedRequestClass)
    ['{3DFE0C16-58D3-434A-BEE4-3E79B70DF595}']
  end;

  VNTrackingRequest = interface(VNImageBasedRequest)
    ['{1292BF4E-FA8C-4380-98F0-C0F0A5BD536B}']
    function initWithCompletionHandler(completionHandler: VNRequestCompletionHandler): Pointer; cdecl;
    function inputObservation: VNDetectedObjectObservation; cdecl;
    function isLastFrame: Boolean; cdecl;
    procedure setInputObservation(inputObservation: VNDetectedObjectObservation); cdecl;
    procedure setLastFrame(lastFrame: Boolean); cdecl;
    procedure setTrackingLevel(trackingLevel: VNRequestTrackingLevel); cdecl;
    function trackingLevel: VNRequestTrackingLevel; cdecl;
  end;
  TVNTrackingRequest = class(TOCGenericImport<VNTrackingRequestClass, VNTrackingRequest>) end;

  VNTrackObjectRequestClass = interface(VNTrackingRequestClass)
    ['{66356F91-365D-43A9-ADC4-7D9C5DAECBD0}']
  end;

  VNTrackObjectRequest = interface(VNTrackingRequest)
    ['{B516910D-65E1-439E-9A81-66CAD785BACF}']
    function initWithCompletionHandler(completionHandler: VNRequestCompletionHandler): Pointer; cdecl;
    [MethodName('initWithDetectedObjectObservation:completionHandler:')]
    function initWithDetectedObjectObservation(observation: VNDetectedObjectObservation;
      completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    function initWithDetectedObjectObservation(observation: VNDetectedObjectObservation): Pointer; overload; cdecl;
  end;
  TVNTrackObjectRequest = class(TOCGenericImport<VNTrackObjectRequestClass, VNTrackObjectRequest>) end;

  VNTrackRectangleRequestClass = interface(VNTrackingRequestClass)
    ['{F8131403-6CAF-42AA-A2D4-4D313A815C99}']
  end;

  VNTrackRectangleRequest = interface(VNTrackingRequest)
    ['{A7ED81EE-C019-47E0-8F0B-FD3468CEBE64}']
    function initWithCompletionHandler(completionHandler: VNRequestCompletionHandler): Pointer; cdecl;
    [MethodName('initWithRectangleObservation:completionHandler:')]
    function initWithRectangleObservation(observation: VNRectangleObservation; completionHandler: VNRequestCompletionHandler): Pointer; overload; cdecl;
    function initWithRectangleObservation(observation: VNRectangleObservation): Pointer; overload; cdecl;
  end;
  TVNTrackRectangleRequest = class(TOCGenericImport<VNTrackRectangleRequestClass, VNTrackRectangleRequest>) end;

  VNDetectHumanRectanglesRequestClass = interface(VNImageBasedRequestClass)
    ['{9CFB69B3-1BDD-455A-8D55-F010E0826ED2}']
  end;

  VNDetectHumanRectanglesRequest = interface(VNImageBasedRequest)
    ['{99EDA82F-EE5F-44F4-AF89-103F6EC3A3DE}']
  end;
  TVNDetectHumanRectanglesRequest = class(TOCGenericImport<VNDetectHumanRectanglesRequestClass, VNDetectHumanRectanglesRequest>) end;

  VNRecognizeAnimalsRequestClass = interface(VNImageBasedRequestClass)
    ['{DDDD5DA5-52B0-4B6A-8E86-C4849897B2E1}']
    [MethodName('knownAnimalIdentifiersForRevision:error:')]
    {class} function knownAnimalIdentifiersForRevision(requestRevision: NSUInteger; error: PPointer): NSArray; cdecl;
  end;

  VNRecognizeAnimalsRequest = interface(VNImageBasedRequest)
    ['{544F46D8-2436-424F-848D-0DA59E3200F0}']
  end;
  TVNRecognizeAnimalsRequest = class(TOCGenericImport<VNRecognizeAnimalsRequestClass, VNRecognizeAnimalsRequest>) end;

function VNBarcodeSymbologyAztec: VNBarcodeSymbology;
function VNBarcodeSymbologyCode39: VNBarcodeSymbology;
function VNBarcodeSymbologyCode39Checksum: VNBarcodeSymbology;
function VNBarcodeSymbologyCode39FullASCII: VNBarcodeSymbology;
function VNBarcodeSymbologyCode39FullASCIIChecksum: VNBarcodeSymbology;
function VNBarcodeSymbologyCode93: VNBarcodeSymbology;
function VNBarcodeSymbologyCode93i: VNBarcodeSymbology;
function VNBarcodeSymbologyCode128: VNBarcodeSymbology;
function VNBarcodeSymbologyDataMatrix: VNBarcodeSymbology;
function VNBarcodeSymbologyEAN8: VNBarcodeSymbology;
function VNBarcodeSymbologyEAN13: VNBarcodeSymbology;
function VNBarcodeSymbologyI2of5: VNBarcodeSymbology;
function VNBarcodeSymbologyI2of5Checksum: VNBarcodeSymbology;
function VNBarcodeSymbologyITF14: VNBarcodeSymbology;
function VNBarcodeSymbologyPDF417: VNBarcodeSymbology;
function VNBarcodeSymbologyQR: VNBarcodeSymbology;
function VNBarcodeSymbologyUPCE: VNBarcodeSymbology;
function VNErrorDomain: NSString;
// function VNNormalizedIdentityRect: CGRect;
function VNImageOptionProperties: VNImageOption;
function VNImageOptionCameraIntrinsics: VNImageOption;
function VNImageOptionCIContext: VNImageOption;
function VNAnimalIdentifierDog: VNAnimalIdentifier;
function VNAnimalIdentifierCat: VNAnimalIdentifier;
function VNAnimalDetectorDog: VNAnimalDetector;
function VNAnimalDetectorCat: VNAnimalDetector;
function VNVisionVersionNumber: Double;

const
  libVision = '/System/Library/Frameworks/Vision.framework/Vision';

function VNNormalizedRectIsIdentityRect(normalizedRect: CGRect): Boolean; cdecl;
  external libVision name _PU + 'VNNormalizedRectIsIdentityRect';

function VNImagePointForNormalizedPoint(normalizedPoint: CGPoint; imageWidth: NativeUInt; imageHeight: NativeUInt): CGPoint; cdecl;
  external libVision name _PU + 'VNImagePointForNormalizedPoint';

function VNImageRectForNormalizedRect(normalizedRect: CGRect; imageWidth: NativeUInt; imageHeight: NativeUInt): CGRect; cdecl;
  external libVision name _PU + 'VNImageRectForNormalizedRect';

function VNNormalizedRectForImageRect(imageRect: CGRect; imageWidth: NativeUInt; imageHeight: NativeUInt): CGRect; cdecl;
  external libVision name _PU + 'VNNormalizedRectForImageRect';

//function VNNormalizedFaceBoundingBoxPointForLandmarkPoint(faceLandmarkPoint: vector_float2; faceBoundingBox: CGRect; imageWidth: NativeUInt; imageHeight: NativeUInt): CGPoint; cdecl;
//  external libVision name _PU + 'VNNormalizedFaceBoundingBoxPointForLandmarkPoint';

//function VNImagePointForFaceLandmarkPoint(faceLandmarkPoint: vector_float2; faceBoundingBox: CGRect; imageWidth: NativeUInt; imageHeight: NativeUInt): CGPoint; cdecl;
//  external libVision name _PU + 'VNImagePointForFaceLandmarkPoint';

function VNElementTypeSize(elementType: VNElementType): NSUInteger; cdecl;
  external libVision name _PU + 'VNElementTypeSize';

implementation

uses
{$IF Defined(IOS) and not Defined(CPUARM)}
  Posix.Dlfcn,
{$ENDIF}
  DW.Macapi.Helpers;


{$IF Defined(IOS) and not Defined(CPUARM)}
var
  VisionModule: THandle;
{$ENDIF}

function VNBarcodeSymbologyAztec: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyAztec');
end;

function VNBarcodeSymbologyCode39: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyCode39');
end;

function VNBarcodeSymbologyCode39Checksum: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyCode39Checksum');
end;

function VNBarcodeSymbologyCode39FullASCII: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyCode39FullASCII');
end;

function VNBarcodeSymbologyCode39FullASCIIChecksum: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyCode39FullASCIIChecksum');
end;

function VNBarcodeSymbologyCode93: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyCode93');
end;

function VNBarcodeSymbologyCode93i: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyCode93i');
end;

function VNBarcodeSymbologyCode128: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyCode128');
end;

function VNBarcodeSymbologyDataMatrix: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyDataMatrix');
end;

function VNBarcodeSymbologyEAN8: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyEAN8');
end;

function VNBarcodeSymbologyEAN13: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyEAN13');
end;

function VNBarcodeSymbologyI2of5: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyI2of5');
end;

function VNBarcodeSymbologyI2of5Checksum: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyI2of5Checksum');
end;

function VNBarcodeSymbologyITF14: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyITF14');
end;

function VNBarcodeSymbologyPDF417: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyPDF417');
end;

function VNBarcodeSymbologyQR: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyQR');
end;

function VNBarcodeSymbologyUPCE: VNBarcodeSymbology;
begin
  Result := CocoaNSStringConst(libVision, 'VNBarcodeSymbologyUPCE');
end;

function VNErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libVision, 'VNErrorDomain');
end;

function VNImageOptionProperties: VNImageOption;
begin
  Result := CocoaNSStringConst(libVision, 'VNImageOptionProperties');
end;

function VNImageOptionCameraIntrinsics: VNImageOption;
begin
  Result := CocoaNSStringConst(libVision, 'VNImageOptionCameraIntrinsics');
end;

function VNImageOptionCIContext: VNImageOption;
begin
  Result := CocoaNSStringConst(libVision, 'VNImageOptionCIContext');
end;

function VNAnimalIdentifierDog: VNAnimalIdentifier;
begin
  Result := CocoaNSStringConst(libVision, 'VNAnimalIdentifierDog');
end;

function VNAnimalIdentifierCat: VNAnimalIdentifier;
begin
  Result := CocoaNSStringConst(libVision, 'VNAnimalIdentifierCat');
end;

function VNAnimalDetectorDog: VNAnimalDetector;
begin
  Result := CocoaNSStringConst(libVision, 'VNAnimalDetectorDog');
end;

function VNAnimalDetectorCat: VNAnimalDetector;
begin
  Result := CocoaNSStringConst(libVision, 'VNAnimalDetectorCat');
end;

function VNVisionVersionNumber: Double;
begin
  Result := CocoaDoubleConst(libVision, 'VNVisionVersionNumber');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  VisionModule := dlopen(MarshaledAString(libVision), RTLD_LAZY);

finalization
  dlclose(VisionModule)
{$ENDIF}

end.