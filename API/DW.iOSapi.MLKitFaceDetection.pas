unit DW.iOSapi.MLKitFaceDetection;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.MLKitVision;

type
  MLKFaceContour = interface;
  MLKFaceLandmark = interface;
  MLKFace = interface;
  MLKFaceDetector = interface;
  MLKFaceDetectorOptions = interface;

  MLKFaceContourType = NSString;
  MLKFaceLandmarkType = NSString;

  MLKFaceDetectionCallback = procedure(faces: NSArray; error: NSError) of object;
  MLKFaceDetectorClassificationMode = NSInteger;
  MLKFaceDetectorPerformanceMode = NSInteger;
  MLKFaceDetectorLandmarkMode = NSInteger;
  MLKFaceDetectorContourMode = NSInteger;

  MLKFaceContourClass = interface(NSObjectClass)
    ['{36775618-7BF6-44B4-AF9D-7DB5AA5FC1C9}']
  end;

  MLKFaceContour = interface(NSObject)
    ['{013C495D-048F-4FA4-8C91-5B535F37DC3E}']
    [MethodName('type')]
    function &type: MLKFaceContourType; cdecl;
    function points: NSArray; cdecl;
  end;
  TMLKFaceContour = class(TOCGenericImport<MLKFaceContourClass, MLKFaceContour>) end;

  MLKFaceLandmarkClass = interface(NSObjectClass)
    ['{33E8CB06-4A26-4DFC-9C1D-067EAF26E617}']
  end;

  MLKFaceLandmark = interface(NSObject)
    ['{EB79B3E6-1BEC-4645-9088-C77FFB644D90}']
    [MethodName('type')]
    function &type: MLKFaceLandmarkType; cdecl;
    function position: MLKVisionPoint; cdecl;
  end;
  TMLKFaceLandmark = class(TOCGenericImport<MLKFaceLandmarkClass, MLKFaceLandmark>) end;

  MLKFaceClass = interface(NSObjectClass)
    ['{25C5091E-051A-4862-BA67-6EC04EDACC85}']
  end;

  MLKFace = interface(NSObject)
    ['{F5AA35D3-65FC-4CB0-91F8-412BD8E5AD61}']
    function contourOfType(&type: MLKFaceContourType): MLKFaceContour; cdecl;
    function contours: NSArray; cdecl;
    function frame: CGRect; cdecl;
    function hasHeadEulerAngleX: Boolean; cdecl;
    function hasHeadEulerAngleY: Boolean; cdecl;
    function hasHeadEulerAngleZ: Boolean; cdecl;
    function hasLeftEyeOpenProbability: Boolean; cdecl;
    function hasRightEyeOpenProbability: Boolean; cdecl;
    function hasSmilingProbability: Boolean; cdecl;
    function hasTrackingID: Boolean; cdecl;
    function headEulerAngleX: CGFloat; cdecl;
    function headEulerAngleY: CGFloat; cdecl;
    function headEulerAngleZ: CGFloat; cdecl;
    function landmarkOfType(&type: MLKFaceLandmarkType): MLKFaceLandmark; cdecl;
    function landmarks: NSArray; cdecl;
    function leftEyeOpenProbability: CGFloat; cdecl;
    function rightEyeOpenProbability: CGFloat; cdecl;
    function smilingProbability: CGFloat; cdecl;
    function trackingID: NSInteger; cdecl;
  end;
  TMLKFace = class(TOCGenericImport<MLKFaceClass, MLKFace>) end;

  MLKFaceDetectorClass = interface(NSObjectClass)
    ['{6271C3CD-29BB-49FC-BA4F-0F62E195C709}']
    {class} function faceDetector: Pointer; cdecl;
    {class} function faceDetectorWithOptions(options: MLKFaceDetectorOptions): Pointer; cdecl;
  end;

  MLKFaceDetector = interface(NSObject)
    ['{BDED7AE7-A2E1-4D43-B3D7-CCC2C05F4673}']
    procedure processImage(image: MLKVisionImage; completion: MLKFaceDetectionCallback); cdecl;
    function resultsInImage(image: MLKVisionImage; error: PPointer): NSArray; cdecl;
  end;
  TMLKFaceDetector = class(TOCGenericImport<MLKFaceDetectorClass, MLKFaceDetector>) end;

  MLKFaceDetectorOptionsClass = interface(NSObjectClass)
    ['{00935DC7-DCE7-470C-A547-AEB091BB73BE}']
  end;

  MLKFaceDetectorOptions = interface(NSObject)
    ['{53B28776-25FB-4AF8-9FB8-308691E2C377}']
    function classificationMode: MLKFaceDetectorClassificationMode; cdecl;
    function contourMode: MLKFaceDetectorContourMode; cdecl;
    function isTrackingEnabled: Boolean; cdecl;
    function landmarkMode: MLKFaceDetectorLandmarkMode; cdecl;
    function minFaceSize: CGFloat; cdecl;
    function performanceMode: MLKFaceDetectorPerformanceMode; cdecl;
    procedure setClassificationMode(classificationMode: MLKFaceDetectorClassificationMode); cdecl;
    procedure setContourMode(contourMode: MLKFaceDetectorContourMode); cdecl;
    procedure setLandmarkMode(landmarkMode: MLKFaceDetectorLandmarkMode); cdecl;
    procedure setMinFaceSize(minFaceSize: CGFloat); cdecl;
    procedure setPerformanceMode(performanceMode: MLKFaceDetectorPerformanceMode); cdecl;
    procedure setTrackingEnabled(trackingEnabled: Boolean); cdecl;
  end;
  TMLKFaceDetectorOptions = class(TOCGenericImport<MLKFaceDetectorOptionsClass, MLKFaceDetectorOptions>) end;

const
  libMLKitBarcodeFaceDetection = 'MLKitFaceDetection';

implementation

uses
  // DW
  DW.iOSapi.MLKitCommon;

procedure MLKitFaceDetectionLoader; cdecl; external framework libMLKitBarcodeFaceDetection; // dependency 'c++';

end.