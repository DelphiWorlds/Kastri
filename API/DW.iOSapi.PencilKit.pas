unit DW.iOSapi.PencilKit;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics;

const
  PKContentVersion1 = 1;
  PKContentVersion2 = 2;
  PKContentVersionLatest = PKContentVersion2;
  PKCanvasViewDrawingPolicyDefault = 0;
  PKCanvasViewDrawingPolicyAnyInput = 1;
  PKCanvasViewDrawingPolicyPencilOnly = 2;
  PKEraserTypeVector = 0;
  PKEraserTypeBitmap = 1;
  PKEraserTypeFixedWidthBitmap = 2;

type
  PKToolPickerObserver = interface;
  PKToolPicker = interface;
  PKCanvasViewDelegate = interface;
  PKCanvasView = interface;
  PKTool = interface;
  PKLassoTool = interface;
  PKEraserTool = interface;
  PKInkingTool = interface;
  PKDrawing = interface;
  PKStroke = interface;
  PKFloatRange = interface;
  PKStrokePath = interface;
  PKStrokePoint = interface;
  PKInk = interface;

  PBoolean = ^Boolean;
  PKContentVersion = NSInteger;
  PKCanvasViewDrawingPolicy = NSInteger;
  PKEraserType = NSInteger;
  PKInkType = NSString;
  TPKStrokePathBlockMethod1 = procedure(point: PKStrokePoint; stop: PBoolean) of object;

  PKToolPickerObserver = interface(IObjectiveC)
    ['{F80C5EB7-EAB4-4244-8E19-B81ADE389448}']
    procedure toolPickerFramesObscuredDidChange(toolPicker: PKToolPicker); cdecl;
    procedure toolPickerIsRulerActiveDidChange(toolPicker: PKToolPicker); cdecl;
    procedure toolPickerSelectedToolDidChange(toolPicker: PKToolPicker); cdecl;
    procedure toolPickerVisibilityDidChange(toolPicker: PKToolPicker); cdecl;
  end;

  PKToolPickerClass = interface(NSObjectClass)
    ['{5527FDF6-DFFA-4106-BC04-1F4999A0ABEA}']
    {class} function sharedToolPickerForWindow(window: UIWindow): PKToolPicker; cdecl; // API_DEPRECATED("Create individual instances instead.", ios(13_0, 14_0))
  end;

  PKToolPicker = interface(NSObject)
    ['{961F6B2A-DE3B-4DD9-B23B-051536BCD31E}']
    procedure addObserver(observer: Pointer); cdecl;
    function colorUserInterfaceStyle: UIUserInterfaceStyle; cdecl;
    function frameObscuredInView(view: UIView): CGRect; cdecl;
    function isRulerActive: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    function maximumSupportedContentVersion: PKContentVersion; cdecl;
    function overrideUserInterfaceStyle: UIUserInterfaceStyle; cdecl;
    procedure removeObserver(observer: Pointer); cdecl;
    function selectedTool: PKTool; cdecl;
    procedure setColorUserInterfaceStyle(colorUserInterfaceStyle: UIUserInterfaceStyle); cdecl;
    procedure setMaximumSupportedContentVersion(maximumSupportedContentVersion: PKContentVersion); cdecl;
    procedure setOverrideUserInterfaceStyle(overrideUserInterfaceStyle: UIUserInterfaceStyle); cdecl;
    procedure setRulerActive(rulerActive: Boolean); cdecl;
    procedure setSelectedTool(selectedTool: PKTool); cdecl;
    procedure setShowsDrawingPolicyControls(showsDrawingPolicyControls: Boolean); cdecl;
    procedure setStateAutosaveName(stateAutosaveName: NSString); cdecl;
    procedure setVisible(visible: Boolean; forFirstResponder: UIResponder); cdecl;
    function showsDrawingPolicyControls: Boolean; cdecl;
    function stateAutosaveName: NSString; cdecl;
  end;
  TPKToolPicker = class(TOCGenericImport<PKToolPickerClass, PKToolPicker>) end;

  PKCanvasViewDelegate = interface(IObjectiveC)
    ['{4EDA63A2-B1A2-41C5-A2B0-FC3C29963B4C}']
    procedure canvasViewDidBeginUsingTool(canvasView: PKCanvasView); cdecl;
    procedure canvasViewDidEndUsingTool(canvasView: PKCanvasView); cdecl;
    procedure canvasViewDidFinishRendering(canvasView: PKCanvasView); cdecl;
    procedure canvasViewDrawingDidChange(canvasView: PKCanvasView); cdecl;
  end;

  PKCanvasViewClass = interface(UIScrollViewClass)
    ['{1E3B7187-9811-4454-ABE6-17C5F21DA774}']
  end;

  PKCanvasView = interface(UIScrollView)
    ['{714935E6-C65B-4129-BEE5-AF289F05A47B}']
    function allowsFingerDrawing: Boolean; cdecl; // API_DEPRECATED("Use 'drawingPolicy' property.", ios(13_0, 14_0))
    function delegate: Pointer; cdecl;
    function drawing: PKDrawing; cdecl;
    function drawingGestureRecognizer: UIGestureRecognizer; cdecl;
    function drawingPolicy: PKCanvasViewDrawingPolicy; cdecl;
    function isRulerActive: Boolean; cdecl;
    function maximumSupportedContentVersion: PKContentVersion; cdecl;
    procedure setAllowsFingerDrawing(allowsFingerDrawing: Boolean); cdecl; // API_DEPRECATED("Use 'drawingPolicy' property.", ios(13_0, 14_0))
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDrawing(drawing: PKDrawing); cdecl;
    procedure setDrawingPolicy(drawingPolicy: PKCanvasViewDrawingPolicy); cdecl;
    procedure setMaximumSupportedContentVersion(maximumSupportedContentVersion: PKContentVersion); cdecl;
    procedure setRulerActive(rulerActive: Boolean); cdecl;
    procedure setTool(tool: PKTool); cdecl;
    function tool: PKTool; cdecl;
  end;
  TPKCanvasView = class(TOCGenericImport<PKCanvasViewClass, PKCanvasView>) end;

  PKToolClass = interface(NSObjectClass)
    ['{BEDDD7F4-F4A9-4CE6-9F34-1355A4930483}']
  end;

  PKTool = interface(NSObject)
    ['{1EE5D8D0-04E9-406E-AAAC-B636AB15DF50}']
  end;
  TPKTool = class(TOCGenericImport<PKToolClass, PKTool>) end;

  PKLassoToolClass = interface(PKToolClass)
    ['{539454DE-A5A7-48CC-944E-58E05BA333D5}']
  end;

  PKLassoTool = interface(PKTool)
    ['{4ADF6B78-A4E9-4F85-8596-50C8030B5D1C}']
  end;
  TPKLassoTool = class(TOCGenericImport<PKLassoToolClass, PKLassoTool>) end;

  PKEraserToolClass = interface(PKToolClass)
    ['{9351FE77-D1ED-4A8B-96A6-BCFCF300D08A}']
    {class} function defaultWidthForEraserType(eraserType: PKEraserType): CGFloat; cdecl;
    {class} function maximumWidthForEraserType(eraserType: PKEraserType): CGFloat; cdecl;
    {class} function minimumWidthForEraserType(eraserType: PKEraserType): CGFloat; cdecl;
  end;

  PKEraserTool = interface(PKTool)
    ['{18C54BAB-2872-4B2A-B4FB-97D23798A1D0}']
    function eraserType: PKEraserType; cdecl;
    function initWithEraserType(eraserType: PKEraserType; width: CGFloat): Pointer; overload; cdecl;
    function initWithEraserType(eraserType: PKEraserType): Pointer; overload; cdecl;
    function width: CGFloat; cdecl;
  end;
  TPKEraserTool = class(TOCGenericImport<PKEraserToolClass, PKEraserTool>) end;

  PKInkingToolClass = interface(PKToolClass)
    ['{0E3C4BFD-4154-44B6-9DA7-541508320384}']
    {class} function convertColor(color: UIColor; fromUserInterfaceStyle: UIUserInterfaceStyle; &to: UIUserInterfaceStyle): UIColor; cdecl;
    {class} function defaultWidthForInkType(inkType: PKInkType): CGFloat; cdecl;
    {class} function maximumWidthForInkType(inkType: PKInkType): CGFloat; cdecl;
    {class} function minimumWidthForInkType(inkType: PKInkType): CGFloat; cdecl;
  end;

  PKInkingTool = interface(PKTool)
    ['{69A1C0C4-6175-42DB-9475-E7263120A2D5}']
    function color: UIColor; cdecl;
    function initWithInk(ink: PKInk; width: CGFloat): Pointer; cdecl;
    function initWithInkType(&type: PKInkType; color: UIColor): Pointer; overload; cdecl;
    function initWithInkType(&type: PKInkType; color: UIColor; width: CGFloat): Pointer; overload; cdecl;
    function ink: PKInk; cdecl;
    function inkType: PKInkType; cdecl;
    function requiredContentVersion: PKContentVersion; cdecl;
    function width: CGFloat; cdecl;
  end;
  TPKInkingTool = class(TOCGenericImport<PKInkingToolClass, PKInkingTool>) end;

  PKDrawingClass = interface(NSObjectClass)
    ['{A59AB8A9-3DE1-4700-B1D1-A328B30B588D}']
  end;

  PKDrawing = interface(NSObject)
    ['{4B84F160-7B56-424C-B979-8092A92AA1AA}']
    function bounds: CGRect; cdecl;
    function dataRepresentation: NSData; cdecl;
    function drawingByAppendingDrawing(drawing: PKDrawing): PKDrawing; cdecl;
    function drawingByAppendingStrokes(strokes: NSArray): PKDrawing; cdecl;
    function drawingByApplyingTransform(transform: CGAffineTransform): PKDrawing; cdecl;
    function imageFromRect(rect: CGRect; scale: CGFloat): UIImage; cdecl;
    function initWithData(data: NSData; error: PPointer): Pointer; cdecl;
    function initWithStrokes(strokes: NSArray): Pointer; cdecl;
    function requiredContentVersion: PKContentVersion; cdecl;
    function strokes: NSArray; cdecl;
  end;
  TPKDrawing = class(TOCGenericImport<PKDrawingClass, PKDrawing>) end;

  PKStrokeClass = interface(NSObjectClass)
    ['{A2591784-7930-4551-80DB-1A98DF6398DC}']
  end;

  PKStroke = interface(NSObject)
    ['{0B26287E-94CC-4C2E-A3BF-DC36A6E7DD2D}']
    function initWithInk(ink: PKInk; strokePath: PKStrokePath; transform: CGAffineTransform; mask: UIBezierPath): Pointer; overload; cdecl;
    function initWithInk(ink: PKInk; strokePath: PKStrokePath; transform: CGAffineTransform; mask: UIBezierPath;
      randomSeed: UInt32): Pointer; overload; cdecl;
    function ink: PKInk; cdecl;
    function mask: UIBezierPath; cdecl;
    function maskedPathRanges: NSArray; cdecl;
    function path: PKStrokePath; cdecl;
    function randomSeed: UInt32; cdecl;
    function renderBounds: CGRect; cdecl;
    function requiredContentVersion: PKContentVersion; cdecl;
    function transform: CGAffineTransform; cdecl;
  end;
  TPKStroke = class(TOCGenericImport<PKStrokeClass, PKStroke>) end;

  PKFloatRangeClass = interface(NSObjectClass)
    ['{8ED4CC72-F55A-4E07-992F-43E70F50DA79}']
  end;

  PKFloatRange = interface(NSObject)
    ['{94445F58-81F8-4A54-91CA-207615A0BDE0}']
    function initWithLowerBound(lowerBound: CGFloat; upperBound: CGFloat): Pointer; cdecl;
    function lowerBound: CGFloat; cdecl;
    function upperBound: CGFloat; cdecl;
  end;
  TPKFloatRange = class(TOCGenericImport<PKFloatRangeClass, PKFloatRange>) end;

  PKStrokePathClass = interface(NSObjectClass)
    ['{25278CA8-EFE3-4C73-BC22-A47315A9D7F1}']
  end;

  PKStrokePath = interface(NSObject)
    ['{31FFF0B8-0A1D-4D67-99D0-B03D1A366BE4}']
    function count: NSUInteger; cdecl;
    function creationDate: NSDate; cdecl;
    [MethodName('enumerateInterpolatedPointsInRange:strideByDistance:usingBlock:')]
    procedure enumerateInterpolatedPointsInRangeStrideByDistance(range: PKFloatRange; strideByDistance: CGFloat;
      usingBlock: TPKStrokePathBlockMethod1); cdecl;
    [MethodName('enumerateInterpolatedPointsInRange:strideByParametricStep:usingBlock:')]
    procedure enumerateInterpolatedPointsInRangeStrideByParametricStep(range: PKFloatRange; strideByParametricStep: CGFloat;
      usingBlock: TPKStrokePathBlockMethod1); cdecl;
    [MethodName('enumerateInterpolatedPointsInRange:strideByTime:usingBlock:')]
    procedure enumerateInterpolatedPointsInRangeStrideByTime(range: PKFloatRange; strideByTime: NSTimeInterval;
      usingBlock: TPKStrokePathBlockMethod1); cdecl;
    function initWithControlPoints(controlPoints: NSArray; creationDate: NSDate): Pointer; cdecl;
    function interpolatedLocationAt(parametricValue: CGFloat): CGPoint; cdecl;
    function interpolatedPointAt(parametricValue: CGFloat): PKStrokePoint; cdecl;
    function objectAtIndexedSubscript(i: NSUInteger): PKStrokePoint; cdecl;
    function parametricValue(parametricValue: CGFloat; offsetByTime: NSTimeInterval): CGFloat; cdecl;
    [MethodName('parametricValue:offsetByDistance:')]
    function parametricValueOffsetByDistance(parametricValue: CGFloat; offsetByDistance: CGFloat): CGFloat; cdecl;
    function pointAtIndex(i: NSUInteger): PKStrokePoint; cdecl;
  end;
  TPKStrokePath = class(TOCGenericImport<PKStrokePathClass, PKStrokePath>) end;

  PKStrokePointClass = interface(NSObjectClass)
    ['{F6259B59-3097-4DCB-8AAE-94EF61AE3E5D}']
  end;

  PKStrokePoint = interface(NSObject)
    ['{32752F3F-E702-47D1-AD0E-B30C8157A3AF}']
    function altitude: CGFloat; cdecl;
    function azimuth: CGFloat; cdecl;
    function force: CGFloat; cdecl;
    function initWithLocation(location: CGPoint; timeOffset: NSTimeInterval; size: CGSize; opacity: CGFloat; force: CGFloat; azimuth: CGFloat;
      altitude: CGFloat): Pointer; overload; cdecl;
    function initWithLocation(location: CGPoint; timeOffset: NSTimeInterval; size: CGSize; opacity: CGFloat; force: CGFloat; azimuth: CGFloat;
      altitude: CGFloat; secondaryScale: CGFloat): Pointer; overload; cdecl;
    function location: CGPoint; cdecl;
    function opacity: CGFloat; cdecl;
    function secondaryScale: CGFloat; cdecl;
    function size: CGSize; cdecl;
    function timeOffset: NSTimeInterval; cdecl;
  end;
  TPKStrokePoint = class(TOCGenericImport<PKStrokePointClass, PKStrokePoint>) end;

  PKInkClass = interface(NSObjectClass)
    ['{23375FB7-20F6-4727-973D-0702ADAD1E1A}']
  end;

  PKInk = interface(NSObject)
    ['{1CD7F75F-197A-49EF-8CF2-AEBD62028876}']
    function color: UIColor; cdecl;
    function initWithInkType(&type: PKInkType; color: UIColor): Pointer; cdecl;
    function inkType: PKInkType; cdecl;
    function requiredContentVersion: PKContentVersion; cdecl;
  end;
  TPKInk = class(TOCGenericImport<PKInkClass, PKInk>) end;

function PKInkTypePen: PKInkType;
function PKInkTypePencil: PKInkType;
function PKInkTypeMarker: PKInkType;
function PKInkTypeMonoline: PKInkType;
function PKInkTypeFountainPen: PKInkType;
function PKInkTypeWatercolor: PKInkType;
function PKInkTypeCrayon: PKInkType;
function PKAppleDrawingTypeIdentifier: CFStringRef;

const
  libPencilKit = '/System/Library/Frameworks/PencilKit.framework/PencilKit';

implementation

uses
  // Posix
  Posix.Dlfcn;

var
  PencilKitModule: THandle;

function PKInkTypePen: PKInkType;
begin
  Result := CocoaNSStringConst(libPencilKit, 'PKInkTypePen');
end;

function PKInkTypePencil: PKInkType;
begin
  Result := CocoaNSStringConst(libPencilKit, 'PKInkTypePencil');
end;

function PKInkTypeMarker: PKInkType;
begin
  Result := CocoaNSStringConst(libPencilKit, 'PKInkTypeMarker');
end;

function PKInkTypeMonoline: PKInkType;
begin
  Result := CocoaNSStringConst(libPencilKit, 'PKInkTypeMonoline');
end;

function PKInkTypeFountainPen: PKInkType;
begin
  Result := CocoaNSStringConst(libPencilKit, 'PKInkTypeFountainPen');
end;

function PKInkTypeWatercolor: PKInkType;
begin
  Result := CocoaNSStringConst(libPencilKit, 'PKInkTypeWatercolor');
end;

function PKInkTypeCrayon: PKInkType;
begin
  Result := CocoaNSStringConst(libPencilKit, 'PKInkTypeCrayon');
end;

function PKAppleDrawingTypeIdentifier: CFStringRef;
begin
  Result := CFStringRef(CocoaPointerConst(libPencilKit, 'PKAppleDrawingTypeIdentifier'));
end;

initialization
  PencilKitModule := dlopen(MarshaledAString(libPencilKit), RTLD_LAZY);

finalization
  dlclose(PencilKitModule);

end.