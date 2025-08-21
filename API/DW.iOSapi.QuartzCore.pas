unit DW.iOSapi.QuartzCore;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.ObjCRuntime,
  // iOS
  iOSapi.CocoaTypes, iOSapi.CoreGraphics, iOSapi.Foundation, iOSapi.OpenGLES, iOSapi.CoreVideo,
  // DW
  DW.iOSapi.Foundation, DW.iOSapi.Metal;

const
  kCALayerLeftEdge = 1;
  kCALayerRightEdge = 2;
  kCALayerBottomEdge = 4;
  kCALayerTopEdge = 8;
  kCALayerMinXMinYCorner = 1;
  kCALayerMaxXMinYCorner = 2;
  kCALayerMinXMaxYCorner = 4;
  kCALayerMaxXMaxYCorner = 8;

type
  CAMediaTiming = interface;
  CALayer = interface;
  CAAction = interface;
  CALayerDelegate = interface;
  CAAnimation = interface;
  CAAnimationDelegate = interface;
  CAPropertyAnimation = interface;
  CABasicAnimation = interface;
  CAKeyframeAnimation = interface;
  CASpringAnimation = interface;
  CATransition = interface;
  CAAnimationGroup = interface;
  CADisplayLink = interface;
  CAEAGLLayer = interface;
  CAEDRMetadata = interface;
  CAMetalDrawable = interface;
  CAMetalLayer = interface;
  CAMetalDisplayLinkUpdate = interface;
  CAMetalDisplayLinkDelegate = interface;
  CAMetalDisplayLink = interface;
  CAEmitterCell = interface;
  CAEmitterLayer = interface;
  CAMediaTimingFunction = interface;
  CAGradientLayer = interface;
  CARenderer = interface;
  CAReplicatorLayer = interface;
  CAScrollLayer = interface;
  CAShapeLayer = interface;
  CATextLayer = interface;
  CATiledLayer = interface;
  CATransaction = interface;
  CATransformLayer = interface;
  CAValueFunction = interface;

  P_CAEAGLNativeWindow = Pointer;
  PP_CAEAGLNativeWindow = ^P_CAEAGLNativeWindow;
  P_CAEDRMetadataPrivate = Pointer;
  PP_CAEDRMetadataPrivate = ^P_CAEDRMetadataPrivate;
  P_CAMetalLayerPrivate = Pointer;
  PP_CAMetalLayerPrivate = ^P_CAMetalLayerPrivate;
  PCAMediaTimingFunctionPrivate = Pointer;
  PPCAMediaTimingFunctionPrivate = ^PCAMediaTimingFunctionPrivate;
  PCARendererPriv = Pointer;
  PPCARendererPriv = ^PCARendererPriv;
  PCATextLayerPrivate = Pointer;
  PPCATextLayerPrivate = ^PCATextLayerPrivate;
  PCATransform3D = ^CATransform3D;
  PCAFrameRateRange = ^CAFrameRateRange;

  CVSMPTETime = record
    subframes: SInt16;
    subframeDivisor: SInt16;
    counter: UInt32;
    type_: UInt32;
    flags: UInt32;
    hours: SInt16;
    minutes: SInt16;
    seconds: SInt16;
    frames: SInt16;
  end;
  SMPTETime = CVSMPTETime;

  CVTimeStamp = record
    version: UInt32;
    videoTimeScale: Integer;
    videoTime: Int64;
    hostTime: UInt64;
    rateScalar: Double;
    videoRefreshPeriod: Int64;
    smpteTime: CVSMPTETime;
    flags: UInt64;
    reserved: UInt64;
  end;
  PCVTimeStamp = ^CVTimestamp;

  CATransform3D = record
    m11: CGFloat;
    m12: CGFloat;
    m13: CGFloat;
    m14: CGFloat;
    m21: CGFloat;
    m22: CGFloat;
    m23: CGFloat;
    m24: CGFloat;
    m31: CGFloat;
    m32: CGFloat;
    m33: CGFloat;
    m34: CGFloat;
    m41: CGFloat;
    m42: CGFloat;
    m43: CGFloat;
    m44: CGFloat;
  end;

  CAMediaTimingFillMode = NSString;
  CALayerContentsGravity = NSString;
  CALayerContentsFormat = NSString;
  CALayerContentsFilter = NSString;
  CALayerCornerCurve = NSString;
  CAEdgeAntialiasingMask = NSInteger;
  CACornerMask = NSInteger;

  CAFrameRateRange = record
    minimum: Single;
    maximum: Single;
    preferred: Single;
  end;

  CAAnimationCalculationMode = NSString;
  CAAnimationRotationMode = NSString;
  CATransitionType = NSString;
  CATransitionSubtype = NSString;
  CAEmitterLayerEmitterShape = NSString;
  CAEmitterLayerEmitterMode = NSString;
  CAEmitterLayerRenderMode = NSString;
  CAMediaTimingFunctionName = NSString;
  CAGradientLayerType = NSString;
  CAScrollLayerScrollMode = NSString;
  CAShapeLayerFillRule = NSString;
  CAShapeLayerLineJoin = NSString;
  CAShapeLayerLineCap = NSString;
  CATextLayerTruncationMode = NSString;
  CATextLayerAlignmentMode = NSString;
  CAValueFunctionName = NSString;
  TCATransactionBlockMethod1 = procedure of object;

  CAMediaTiming = interface(IObjectiveC)
    ['{48F6AF02-A682-46DA-A800-3CB5AACF0FE1}']
    function autoreverses: Boolean; cdecl;
    function beginTime: CFTimeInterval; cdecl;
    function duration: CFTimeInterval; cdecl;
    function fillMode: CAMediaTimingFillMode; cdecl;
    function repeatCount: Single; cdecl;
    function repeatDuration: CFTimeInterval; cdecl;
    procedure setAutoreverses(autoreverses: Boolean); cdecl;
    procedure setBeginTime(beginTime: CFTimeInterval); cdecl;
    procedure setDuration(duration: CFTimeInterval); cdecl;
    procedure setFillMode(fillMode: CAMediaTimingFillMode); cdecl;
    procedure setRepeatCount(repeatCount: Single); cdecl;
    procedure setRepeatDuration(repeatDuration: CFTimeInterval); cdecl;
    procedure setSpeed(speed: Single); cdecl;
    procedure setTimeOffset(timeOffset: CFTimeInterval); cdecl;
    function speed: Single; cdecl;
    function timeOffset: CFTimeInterval; cdecl;
  end;

  CALayerClass = interface(NSObjectClass)
    ['{4E0AF6CB-012E-4A1B-BCAA-62B1333462E5}']
    {class} function cornerCurveExpansionFactor(curve: CALayerCornerCurve): CGFloat; cdecl;
    {class} function defaultActionForKey(event: NSString): Pointer; cdecl;
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
    {class} function layer: Pointer; cdecl;
    {class} function needsDisplayForKey(key: NSString): Boolean; cdecl;
  end;

  CALayer = interface(NSObject)
    ['{27226D9A-9519-4310-B3E0-40E2DA0306D7}']
    function actionForKey(event: NSString): Pointer; cdecl;
    function actions: NSDictionary; cdecl;
    procedure addAnimation(anim: CAAnimation; forKey: NSString); cdecl;
    procedure addSublayer(layer: CALayer); cdecl;
    function affineTransform: CGAffineTransform; cdecl;
    function allowsEdgeAntialiasing: Boolean; cdecl;
    function allowsGroupOpacity: Boolean; cdecl;
    function anchorPoint: CGPoint; cdecl;
    function anchorPointZ: CGFloat; cdecl;
    function animationForKey(key: NSString): CAAnimation; cdecl;
    function animationKeys: NSArray; cdecl;
    function backgroundColor: CGColorRef; cdecl;
    function backgroundFilters: NSArray; cdecl;
    function borderColor: CGColorRef; cdecl;
    function borderWidth: CGFloat; cdecl;
    function bounds: CGRect; cdecl;
    function compositingFilter: Pointer; cdecl;
    function containsPoint(p: CGPoint): Boolean; cdecl;
    function contents: Pointer; cdecl;
    function contentsAreFlipped: Boolean; cdecl;
    function contentsCenter: CGRect; cdecl;
    function contentsFormat: CALayerContentsFormat; cdecl;
    function contentsGravity: CALayerContentsGravity; cdecl;
    function contentsRect: CGRect; cdecl;
    function contentsScale: CGFloat; cdecl;
    function convertPoint(p: CGPoint; fromLayer: CALayer): CGPoint; cdecl;
    [MethodName('convertPoint:toLayer:')]
    function convertPointToLayer(p: CGPoint; toLayer: CALayer): CGPoint; cdecl;
    function convertRect(r: CGRect; fromLayer: CALayer): CGRect; cdecl;
    [MethodName('convertRect:toLayer:')]
    function convertRectToLayer(r: CGRect; toLayer: CALayer): CGRect; cdecl;
    function convertTime(t: CFTimeInterval; fromLayer: CALayer): CFTimeInterval; cdecl;
    [MethodName('convertTime:toLayer:')]
    function convertTimeToLayer(t: CFTimeInterval; toLayer: CALayer): CFTimeInterval; cdecl;
    function cornerCurve: CALayerCornerCurve; cdecl;
    function cornerRadius: CGFloat; cdecl;
    function delegate: Pointer; cdecl;
    procedure display; cdecl;
    procedure displayIfNeeded; cdecl;
    procedure drawInContext(ctx: CGContextRef); cdecl;
    function drawsAsynchronously: Boolean; cdecl;
    function edgeAntialiasingMask: CAEdgeAntialiasingMask; cdecl;
    function filters: NSArray; cdecl;
    function frame: CGRect; cdecl;
    function hitTest(p: CGPoint): CALayer; cdecl;
    function initWithLayer(layer: Pointer): Pointer; cdecl;
    procedure insertSublayer(layer: CALayer; atIndex: Cardinal); overload; cdecl;
    procedure insertSublayer(layer: CALayer; below: CALayer); overload; cdecl;
    [MethodName('insertSublayer:above:')]
    procedure insertSublayerAbove(layer: CALayer; above: CALayer); cdecl;
    function isDoubleSided: Boolean; cdecl;
    function isGeometryFlipped: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isOpaque: Boolean; cdecl;
    procedure layoutIfNeeded; cdecl;
    procedure layoutSublayers; cdecl;
    function magnificationFilter: CALayerContentsFilter; cdecl;
    function mask: CALayer; cdecl;
    function maskedCorners: CACornerMask; cdecl;
    function masksToBounds: Boolean; cdecl;
    function minificationFilter: CALayerContentsFilter; cdecl;
    function minificationFilterBias: Single; cdecl;
    function modelLayer: Pointer; cdecl;
    function name: NSString; cdecl;
    function needsDisplay: Boolean; cdecl;
    function needsDisplayOnBoundsChange: Boolean; cdecl;
    function needsLayout: Boolean; cdecl;
    function opacity: Single; cdecl;
    function position: CGPoint; cdecl;
    function preferredFrameSize: CGSize; cdecl;
    function presentationLayer: Pointer; cdecl;
    function rasterizationScale: CGFloat; cdecl;
    procedure removeAllAnimations; cdecl;
    procedure removeAnimationForKey(key: NSString); cdecl;
    procedure removeFromSuperlayer; cdecl;
    procedure renderInContext(ctx: CGContextRef); cdecl;
    procedure replaceSublayer(oldLayer: CALayer; &with: CALayer); cdecl;
    procedure scrollPoint(p: CGPoint); cdecl;
    procedure scrollRectToVisible(r: CGRect); cdecl;
    procedure setActions(actions: NSDictionary); cdecl;
    procedure setAffineTransform(m: CGAffineTransform); cdecl;
    procedure setAllowsEdgeAntialiasing(allowsEdgeAntialiasing: Boolean); cdecl;
    procedure setAllowsGroupOpacity(allowsGroupOpacity: Boolean); cdecl;
    procedure setAnchorPoint(anchorPoint: CGPoint); cdecl;
    procedure setAnchorPointZ(anchorPointZ: CGFloat); cdecl;
    procedure setBackgroundColor(backgroundColor: CGColorRef); cdecl;
    procedure setBackgroundFilters(backgroundFilters: NSArray); cdecl;
    procedure setBorderColor(borderColor: CGColorRef); cdecl;
    procedure setBorderWidth(borderWidth: CGFloat); cdecl;
    procedure setBounds(bounds: CGRect); cdecl;
    procedure setCompositingFilter(compositingFilter: Pointer); cdecl;
    procedure setContents(contents: Pointer); cdecl;
    procedure setContentsCenter(contentsCenter: CGRect); cdecl;
    procedure setContentsFormat(contentsFormat: CALayerContentsFormat); cdecl;
    procedure setContentsGravity(contentsGravity: CALayerContentsGravity); cdecl;
    procedure setContentsRect(contentsRect: CGRect); cdecl;
    procedure setContentsScale(contentsScale: CGFloat); cdecl;
    procedure setCornerCurve(cornerCurve: CALayerCornerCurve); cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDoubleSided(doubleSided: Boolean); cdecl;
    procedure setDrawsAsynchronously(drawsAsynchronously: Boolean); cdecl;
    procedure setEdgeAntialiasingMask(edgeAntialiasingMask: CAEdgeAntialiasingMask); cdecl;
    procedure setFilters(filters: NSArray); cdecl;
    procedure setFrame(frame: CGRect); cdecl;
    procedure setGeometryFlipped(geometryFlipped: Boolean); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setMagnificationFilter(magnificationFilter: CALayerContentsFilter); cdecl;
    procedure setMask(mask: CALayer); cdecl;
    procedure setMaskedCorners(maskedCorners: CACornerMask); cdecl;
    procedure setMasksToBounds(masksToBounds: Boolean); cdecl;
    procedure setMinificationFilter(minificationFilter: CALayerContentsFilter); cdecl;
    procedure setMinificationFilterBias(minificationFilterBias: Single); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setNeedsDisplay; cdecl;
    procedure setNeedsDisplayInRect(r: CGRect); cdecl;
    procedure setNeedsDisplayOnBoundsChange(needsDisplayOnBoundsChange: Boolean); cdecl;
    procedure setNeedsLayout; cdecl;
    procedure setOpacity(opacity: Single); cdecl;
    procedure setOpaque(opaque: Boolean); cdecl;
    procedure setPosition(position: CGPoint); cdecl;
    procedure setRasterizationScale(rasterizationScale: CGFloat); cdecl;
    procedure setShadowColor(shadowColor: CGColorRef); cdecl;
    procedure setShadowOffset(shadowOffset: CGSize); cdecl;
    procedure setShadowOpacity(shadowOpacity: Single); cdecl;
    procedure setShadowPath(shadowPath: CGPathRef); cdecl;
    procedure setShadowRadius(shadowRadius: CGFloat); cdecl;
    procedure setShouldRasterize(shouldRasterize: Boolean); cdecl;
    procedure setStyle(style: NSDictionary); cdecl;
    procedure setSublayers(sublayers: NSArray); cdecl;
    procedure setSublayerTransform(sublayerTransform: CATransform3D); cdecl;
    procedure setTransform(transform: CATransform3D); cdecl;
    procedure setWantsExtendedDynamicRangeContent(wantsExtendedDynamicRangeContent: Boolean); cdecl;
    procedure setZPosition(zPosition: CGFloat); cdecl;
    function shadowColor: CGColorRef; cdecl;
    function shadowOffset: CGSize; cdecl;
    function shadowOpacity: Single; cdecl;
    function shadowPath: CGPathRef; cdecl;
    function shadowRadius: CGFloat; cdecl;
    function shouldArchiveValueForKey(key: NSString): Boolean; cdecl;
    function shouldRasterize: Boolean; cdecl;
    function style: NSDictionary; cdecl;
    function sublayers: NSArray; cdecl;
    function sublayerTransform: CATransform3D; cdecl;
    function superlayer: CALayer; cdecl;
    function transform: CATransform3D; cdecl;
    function visibleRect: CGRect; cdecl;
    function wantsExtendedDynamicRangeContent: Boolean; cdecl;
    function zPosition: CGFloat; cdecl;
  end;
  TCALayer = class(TOCGenericImport<CALayerClass, CALayer>) end;

  CAAction = interface(IObjectiveC)
    ['{F50CE0ED-C4DC-479E-9B75-A35889B745C0}']
    procedure runActionForKey(event: NSString; &object: Pointer; arguments: NSDictionary); cdecl;
  end;

  CALayerDelegate = interface(IObjectiveC)
    ['{CDE6E602-8714-4AF9-A61F-D9AE6C61FBDF}']
    function actionForLayer(layer: CALayer; forKey: NSString): Pointer; cdecl;
    procedure displayLayer(layer: CALayer); cdecl;
    procedure drawLayer(layer: CALayer; inContext: CGContextRef); cdecl;
    procedure layerWillDraw(layer: CALayer); cdecl;
    procedure layoutSublayersOfLayer(layer: CALayer); cdecl;
  end;

  CAAnimationClass = interface(NSObjectClass)
    ['{6253A335-83D4-4A6A-AD35-EEB604CD1035}']
    {class} function animation: Pointer; cdecl;
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
  end;

  CAAnimation = interface(NSObject)
    ['{DD9C972C-B6AD-4141-A66B-A8AB284303DF}']
    function delegate: Pointer; cdecl;
    function isRemovedOnCompletion: Boolean; cdecl;
    function preferredFrameRateRange: CAFrameRateRange; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPreferredFrameRateRange(preferredFrameRateRange: CAFrameRateRange); cdecl;
    procedure setRemovedOnCompletion(removedOnCompletion: Boolean); cdecl;
    procedure setTimingFunction(timingFunction: CAMediaTimingFunction); cdecl;
    function shouldArchiveValueForKey(key: NSString): Boolean; cdecl;
    function timingFunction: CAMediaTimingFunction; cdecl;
  end;
  TCAAnimation = class(TOCGenericImport<CAAnimationClass, CAAnimation>) end;

  CAAnimationDelegate = interface(IObjectiveC)
    ['{9E4FD4A0-B3F4-41AB-BF8A-856011E241AB}']
    procedure animationDidStart(anim: CAAnimation); cdecl;
    procedure animationDidStop(anim: CAAnimation; finished: Boolean); cdecl;
  end;

  CAPropertyAnimationClass = interface(CAAnimationClass)
    ['{A58A5FF5-6A57-4402-9E9D-67F026475C48}']
    {class} function animationWithKeyPath(path: NSString): Pointer; cdecl;
  end;

  CAPropertyAnimation = interface(CAAnimation)
    ['{898BB882-E886-462F-BBC8-BE4999498EF2}']
    function isAdditive: Boolean; cdecl;
    function isCumulative: Boolean; cdecl;
    function keyPath: NSString; cdecl;
    procedure setAdditive(additive: Boolean); cdecl;
    procedure setCumulative(cumulative: Boolean); cdecl;
    procedure setKeyPath(keyPath: NSString); cdecl;
    procedure setValueFunction(valueFunction: CAValueFunction); cdecl;
    function valueFunction: CAValueFunction; cdecl;
  end;
  TCAPropertyAnimation = class(TOCGenericImport<CAPropertyAnimationClass, CAPropertyAnimation>) end;

  CABasicAnimationClass = interface(CAPropertyAnimationClass)
    ['{616F2114-EF52-4C10-AB95-A66C920F1C50}']
  end;

  CABasicAnimation = interface(CAPropertyAnimation)
    ['{889EDF1B-8BD8-485E-89C5-9134F52A9747}']
    function byValue: Pointer; cdecl;
    function fromValue: Pointer; cdecl;
    procedure setByValue(byValue: Pointer); cdecl;
    procedure setFromValue(fromValue: Pointer); cdecl;
    procedure setToValue(toValue: Pointer); cdecl;
    function toValue: Pointer; cdecl;
  end;
  TCABasicAnimation = class(TOCGenericImport<CABasicAnimationClass, CABasicAnimation>) end;

  CAKeyframeAnimationClass = interface(CAPropertyAnimationClass)
    ['{BE44946B-194B-4313-840E-61316B018510}']
  end;

  CAKeyframeAnimation = interface(CAPropertyAnimation)
    ['{80C1287B-3EEA-4E63-BF48-167D2E764F57}']
    function biasValues: NSArray; cdecl;
    function calculationMode: CAAnimationCalculationMode; cdecl;
    function continuityValues: NSArray; cdecl;
    function keyTimes: NSArray; cdecl;
    function path: CGPathRef; cdecl;
    function rotationMode: CAAnimationRotationMode; cdecl;
    procedure setBiasValues(biasValues: NSArray); cdecl;
    procedure setCalculationMode(calculationMode: CAAnimationCalculationMode); cdecl;
    procedure setContinuityValues(continuityValues: NSArray); cdecl;
    procedure setKeyTimes(keyTimes: NSArray); cdecl;
    procedure setPath(path: CGPathRef); cdecl;
    procedure setRotationMode(rotationMode: CAAnimationRotationMode); cdecl;
    procedure setTensionValues(tensionValues: NSArray); cdecl;
    procedure setTimingFunctions(timingFunctions: NSArray); cdecl;
    procedure setValues(values: NSArray); cdecl;
    function tensionValues: NSArray; cdecl;
    function timingFunctions: NSArray; cdecl;
    function values: NSArray; cdecl;
  end;
  TCAKeyframeAnimation = class(TOCGenericImport<CAKeyframeAnimationClass, CAKeyframeAnimation>) end;

  CASpringAnimationClass = interface(CABasicAnimationClass)
    ['{7E6FB8DD-0804-4E4C-B166-45470D1ACEAC}']
  end;

  CASpringAnimation = interface(CABasicAnimation)
    ['{1FF12579-8972-4C15-9EF0-53F135B356C0}']
    function allowsOverdamping: Boolean; cdecl;
    function bounce: CGFloat; cdecl;
    function damping: CGFloat; cdecl;
    function initialVelocity: CGFloat; cdecl;
    function initWithPerceptualDuration(perceptualDuration: CFTimeInterval; bounce: CGFloat): Pointer; cdecl;
    function mass: CGFloat; cdecl;
    function perceptualDuration: CFTimeInterval; cdecl;
    procedure setAllowsOverdamping(allowsOverdamping: Boolean); cdecl;
    procedure setDamping(damping: CGFloat); cdecl;
    procedure setInitialVelocity(initialVelocity: CGFloat); cdecl;
    procedure setMass(mass: CGFloat); cdecl;
    procedure setStiffness(stiffness: CGFloat); cdecl;
    function settlingDuration: CFTimeInterval; cdecl;
    function stiffness: CGFloat; cdecl;
  end;
  TCASpringAnimation = class(TOCGenericImport<CASpringAnimationClass, CASpringAnimation>) end;

  CATransitionClass = interface(CAAnimationClass)
    ['{010011FF-158F-48E7-949D-0AD4FFFF9730}']
  end;

  CATransition = interface(CAAnimation)
    ['{B07EAA50-5E99-47D1-8F5C-BA80E6642B7C}']
    function &type: CATransitionType; cdecl;
    function endProgress: Single; cdecl;
    procedure setEndProgress(endProgress: Single); cdecl;
    procedure setStartProgress(startProgress: Single); cdecl;
    procedure setSubtype(subtype: CATransitionSubtype); cdecl;
    procedure setType(&type: CATransitionType); cdecl;
    function startProgress: Single; cdecl;
    function subtype: CATransitionSubtype; cdecl;
  end;
  TCATransition = class(TOCGenericImport<CATransitionClass, CATransition>) end;

  CAAnimationGroupClass = interface(CAAnimationClass)
    ['{D5BAA29C-5EDD-4252-B4AC-22909A1A5FF7}']
  end;

  CAAnimationGroup = interface(CAAnimation)
    ['{C3A59A35-A93A-4787-93AD-E32360058221}']
    function animations: NSArray; cdecl;
    procedure setAnimations(animations: NSArray); cdecl;
  end;
  TCAAnimationGroup = class(TOCGenericImport<CAAnimationGroupClass, CAAnimationGroup>) end;

  CADisplayLinkClass = interface(NSObjectClass)
    ['{4665E2ED-8641-449D-BBDA-141A4B5F6093}']
    {class} function displayLinkWithTarget(target: Pointer; selector: SEL): CADisplayLink; cdecl;
  end;

  CADisplayLink = interface(NSObject)
    ['{1B6E28E3-CF96-4AC3-9972-8A4A6A869BC8}']
    procedure addToRunLoop(runloop: NSRunLoop; forMode: NSRunLoopMode); cdecl;
    function duration: CFTimeInterval; cdecl;
    function frameInterval: NSInteger; cdecl; // API_DEPRECATED("preferredFramesPerSecond", ios(3.1, 10.0), watchos(2.0, 3.0), tvos(9.0, 10.0))
    procedure invalidate; cdecl;
    function isPaused: Boolean; cdecl;
    function preferredFrameRateRange: CAFrameRateRange; cdecl;
    function preferredFramesPerSecond: NSInteger; cdecl; // API_DEPRECATED_WITH_REPLACEMENT ("preferredFrameRateRange", ios(10.0, API_TO_BE_DEPRECATED), watchos(3.0, API_TO_BE_DEPRECATED), tvos(10.0, API_TO_BE_DEPRECATED))
    procedure removeFromRunLoop(runloop: NSRunLoop; forMode: NSRunLoopMode); cdecl;
    procedure setFrameInterval(frameInterval: NSInteger); cdecl; // API_DEPRECATED("preferredFramesPerSecond", ios(3.1, 10.0), watchos(2.0, 3.0), tvos(9.0, 10.0))
    procedure setPaused(paused: Boolean); cdecl;
    procedure setPreferredFrameRateRange(preferredFrameRateRange: CAFrameRateRange); cdecl;
    procedure setPreferredFramesPerSecond(preferredFramesPerSecond: NSInteger); cdecl; // API_DEPRECATED_WITH_REPLACEMENT ("preferredFrameRateRange", ios(10.0, API_TO_BE_DEPRECATED), watchos(3.0, API_TO_BE_DEPRECATED), tvos(10.0, API_TO_BE_DEPRECATED))
    function targetTimestamp: CFTimeInterval; cdecl;
    function timestamp: CFTimeInterval; cdecl;
  end;
  TCADisplayLink = class(TOCGenericImport<CADisplayLinkClass, CADisplayLink>) end;

  CAEAGLLayerClass = interface(CALayerClass)
    ['{3ADEF51F-FE96-4A69-8206-7DCAAFAB94C2}']
  end;

  CAEAGLLayer = interface(CALayer)
    ['{E75ACF7A-E1EF-4DC6-8053-6B256192C082}']
    function presentsWithTransaction: Boolean; cdecl;
    procedure setPresentsWithTransaction(presentsWithTransaction: Boolean); cdecl;
  end;
  TCAEAGLLayer = class(TOCGenericImport<CAEAGLLayerClass, CAEAGLLayer>) end;

  CAEDRMetadataClass = interface(NSObjectClass)
    ['{9C4E612D-E950-4B3F-A47B-A49BA20B57C9}']
    {class} function HDR10MetadataWithDisplayInfo(displayData: NSData; contentInfo: NSData; opticalOutputScale: Single): CAEDRMetadata; cdecl;
    {class} function HDR10MetadataWithMinLuminance(minNits: Single; maxLuminance: Single; opticalOutputScale: Single): CAEDRMetadata; cdecl;
    {class} function HLGMetadata: CAEDRMetadata; cdecl;
    {class} function HLGMetadataWithAmbientViewingEnvironment(data: NSData): CAEDRMetadata; cdecl;
    {class} function isAvailable: Boolean; cdecl;
  end;

  CAEDRMetadata = interface(NSObject)
    ['{4C255028-0B05-4042-B935-A3CA1A7C530F}']
  end;
  TCAEDRMetadata = class(TOCGenericImport<CAEDRMetadataClass, CAEDRMetadata>) end;

  CAMetalDrawable = interface(IObjectiveC)
    ['{FB51F0CA-3BB8-4B93-A087-4D85BDB10C00}']
    function layer: CAMetalLayer; cdecl;
    function texture: Pointer; cdecl;
  end;

  CAMetalLayerClass = interface(CALayerClass)
    ['{69795D35-D1C9-4C17-BB49-71BF02861BB5}']
  end;

  CAMetalLayer = interface(CALayer)
    ['{237981ED-9339-4EA7-8939-1BAD40DDD997}']
    function allowsNextDrawableTimeout: Boolean; cdecl;
    function colorspace: CGColorSpaceRef; cdecl;
    function developerHUDProperties: NSDictionary; cdecl;
    function device: Pointer; cdecl;
    function drawableSize: CGSize; cdecl;
    function EDRMetadata: CAEDRMetadata; cdecl;
    function framebufferOnly: Boolean; cdecl;
    function maximumDrawableCount: NSUInteger; cdecl;
    function nextDrawable: Pointer; cdecl;
    function pixelFormat: MTLPixelFormat; cdecl;
    function preferredDevice: Pointer; cdecl;
    function presentsWithTransaction: Boolean; cdecl;
    procedure setAllowsNextDrawableTimeout(allowsNextDrawableTimeout: Boolean); cdecl;
    procedure setColorspace(colorspace: CGColorSpaceRef); cdecl;
    procedure setDeveloperHUDProperties(developerHUDProperties: NSDictionary); cdecl;
    procedure setDevice(device: Pointer); cdecl;
    procedure setDrawableSize(drawableSize: CGSize); cdecl;
    procedure setEDRMetadata(EDRMetadata: CAEDRMetadata); cdecl;
    procedure setFramebufferOnly(framebufferOnly: Boolean); cdecl;
    procedure setMaximumDrawableCount(maximumDrawableCount: NSUInteger); cdecl;
    procedure setPixelFormat(pixelFormat: MTLPixelFormat); cdecl;
    procedure setPresentsWithTransaction(presentsWithTransaction: Boolean); cdecl;
    procedure setWantsExtendedDynamicRangeContent(wantsExtendedDynamicRangeContent: Boolean); cdecl;
    function wantsExtendedDynamicRangeContent: Boolean; cdecl;
  end;
  TCAMetalLayer = class(TOCGenericImport<CAMetalLayerClass, CAMetalLayer>) end;

  CAMetalDisplayLinkUpdateClass = interface(NSObjectClass)
    ['{1F2CD764-5A86-44B4-808C-DFB0B837DE1D}']
  end;

  CAMetalDisplayLinkUpdate = interface(NSObject)
    ['{13621073-0F17-44EC-9127-427621800B26}']
    function drawable: Pointer; cdecl;
    function targetPresentationTimestamp: CFTimeInterval; cdecl;
    function targetTimestamp: CFTimeInterval; cdecl;
  end;
  TCAMetalDisplayLinkUpdate = class(TOCGenericImport<CAMetalDisplayLinkUpdateClass, CAMetalDisplayLinkUpdate>) end;

  CAMetalDisplayLinkDelegate = interface(IObjectiveC)
    ['{13F19530-A379-4043-ABF2-1F5D42876235}']
    procedure metalDisplayLink(link: CAMetalDisplayLink; needsUpdate: CAMetalDisplayLinkUpdate); cdecl;
  end;

  CAMetalDisplayLinkClass = interface(NSObjectClass)
    ['{6D5BBD67-FA79-4619-80CF-A573C7D2E41A}']
  end;

  CAMetalDisplayLink = interface(NSObject)
    ['{529FD391-56CB-4495-9A98-DF9BF24FAD52}']
    procedure addToRunLoop(runloop: NSRunLoop; forMode: NSRunLoopMode); cdecl;
    function delegate: Pointer; cdecl;
    function initWithMetalLayer(layer: CAMetalLayer): Pointer; cdecl;
    procedure invalidate; cdecl;
    function isPaused: Boolean; cdecl;
    function preferredFrameLatency: Single; cdecl;
    function preferredFrameRateRange: CAFrameRateRange; cdecl;
    procedure removeFromRunLoop(runloop: NSRunLoop; forMode: NSRunLoopMode); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    procedure setPreferredFrameLatency(preferredFrameLatency: Single); cdecl;
    procedure setPreferredFrameRateRange(preferredFrameRateRange: CAFrameRateRange); cdecl;
  end;
  TCAMetalDisplayLink = class(TOCGenericImport<CAMetalDisplayLinkClass, CAMetalDisplayLink>) end;

  CAEmitterCellClass = interface(NSObjectClass)
    ['{38186571-5E27-45C7-8C14-01F6586D4B52}']
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
    {class} function emitterCell: Pointer; cdecl;
  end;

  CAEmitterCell = interface(NSObject)
    ['{703856DC-2BAD-410B-9857-E0FB4E331FF3}']
    function alphaRange: Single; cdecl;
    function alphaSpeed: Single; cdecl;
    function birthRate: Single; cdecl;
    function blueRange: Single; cdecl;
    function blueSpeed: Single; cdecl;
    function color: CGColorRef; cdecl;
    function contents: Pointer; cdecl;
    function contentsRect: CGRect; cdecl;
    function contentsScale: CGFloat; cdecl;
    function emissionLatitude: CGFloat; cdecl;
    function emissionLongitude: CGFloat; cdecl;
    function emissionRange: CGFloat; cdecl;
    function emitterCells: NSArray; cdecl;
    function greenRange: Single; cdecl;
    function greenSpeed: Single; cdecl;
    function isEnabled: Boolean; cdecl;
    function lifetime: Single; cdecl;
    function lifetimeRange: Single; cdecl;
    function magnificationFilter: NSString; cdecl;
    function minificationFilter: NSString; cdecl;
    function minificationFilterBias: Single; cdecl;
    function name: NSString; cdecl;
    function redRange: Single; cdecl;
    function redSpeed: Single; cdecl;
    function scale: CGFloat; cdecl;
    function scaleRange: CGFloat; cdecl;
    function scaleSpeed: CGFloat; cdecl;
    procedure setAlphaRange(alphaRange: Single); cdecl;
    procedure setAlphaSpeed(alphaSpeed: Single); cdecl;
    procedure setBirthRate(birthRate: Single); cdecl;
    procedure setBlueRange(blueRange: Single); cdecl;
    procedure setBlueSpeed(blueSpeed: Single); cdecl;
    procedure setColor(color: CGColorRef); cdecl;
    procedure setContents(contents: Pointer); cdecl;
    procedure setContentsRect(contentsRect: CGRect); cdecl;
    procedure setContentsScale(contentsScale: CGFloat); cdecl;
    procedure setEmissionLatitude(emissionLatitude: CGFloat); cdecl;
    procedure setEmissionLongitude(emissionLongitude: CGFloat); cdecl;
    procedure setEmissionRange(emissionRange: CGFloat); cdecl;
    procedure setEmitterCells(emitterCells: NSArray); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setGreenRange(greenRange: Single); cdecl;
    procedure setGreenSpeed(greenSpeed: Single); cdecl;
    procedure setLifetime(lifetime: Single); cdecl;
    procedure setLifetimeRange(lifetimeRange: Single); cdecl;
    procedure setMagnificationFilter(magnificationFilter: NSString); cdecl;
    procedure setMinificationFilter(minificationFilter: NSString); cdecl;
    procedure setMinificationFilterBias(minificationFilterBias: Single); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setRedRange(redRange: Single); cdecl;
    procedure setRedSpeed(redSpeed: Single); cdecl;
    procedure setScale(scale: CGFloat); cdecl;
    procedure setScaleRange(scaleRange: CGFloat); cdecl;
    procedure setScaleSpeed(scaleSpeed: CGFloat); cdecl;
    procedure setSpin(spin: CGFloat); cdecl;
    procedure setSpinRange(spinRange: CGFloat); cdecl;
    procedure setStyle(style: NSDictionary); cdecl;
    procedure setVelocity(velocity: CGFloat); cdecl;
    procedure setVelocityRange(velocityRange: CGFloat); cdecl;
    procedure setXAcceleration(xAcceleration: CGFloat); cdecl;
    procedure setYAcceleration(yAcceleration: CGFloat); cdecl;
    procedure setZAcceleration(zAcceleration: CGFloat); cdecl;
    function shouldArchiveValueForKey(key: NSString): Boolean; cdecl;
    function spin: CGFloat; cdecl;
    function spinRange: CGFloat; cdecl;
    function style: NSDictionary; cdecl;
    function velocity: CGFloat; cdecl;
    function velocityRange: CGFloat; cdecl;
    function xAcceleration: CGFloat; cdecl;
    function yAcceleration: CGFloat; cdecl;
    function zAcceleration: CGFloat; cdecl;
  end;
  TCAEmitterCell = class(TOCGenericImport<CAEmitterCellClass, CAEmitterCell>) end;

  CAEmitterLayerClass = interface(CALayerClass)
    ['{0779901A-154D-4D4A-897D-7A648577204A}']
  end;

  CAEmitterLayer = interface(CALayer)
    ['{879BD2BC-4CFC-4D94-A7C9-3D8C44E06616}']
    function birthRate: Single; cdecl;
    function emitterCells: NSArray; cdecl;
    function emitterDepth: CGFloat; cdecl;
    function emitterMode: CAEmitterLayerEmitterMode; cdecl;
    function emitterPosition: CGPoint; cdecl;
    function emitterShape: CAEmitterLayerEmitterShape; cdecl;
    function emitterSize: CGSize; cdecl;
    function emitterZPosition: CGFloat; cdecl;
    function lifetime: Single; cdecl;
    function preservesDepth: Boolean; cdecl;
    function renderMode: CAEmitterLayerRenderMode; cdecl;
    function scale: Single; cdecl;
    function seed: Cardinal; cdecl;
    procedure setBirthRate(birthRate: Single); cdecl;
    procedure setEmitterCells(emitterCells: NSArray); cdecl;
    procedure setEmitterDepth(emitterDepth: CGFloat); cdecl;
    procedure setEmitterMode(emitterMode: CAEmitterLayerEmitterMode); cdecl;
    procedure setEmitterPosition(emitterPosition: CGPoint); cdecl;
    procedure setEmitterShape(emitterShape: CAEmitterLayerEmitterShape); cdecl;
    procedure setEmitterSize(emitterSize: CGSize); cdecl;
    procedure setEmitterZPosition(emitterZPosition: CGFloat); cdecl;
    procedure setLifetime(lifetime: Single); cdecl;
    procedure setPreservesDepth(preservesDepth: Boolean); cdecl;
    procedure setRenderMode(renderMode: CAEmitterLayerRenderMode); cdecl;
    procedure setScale(scale: Single); cdecl;
    procedure setSeed(seed: Cardinal); cdecl;
    procedure setSpin(spin: Single); cdecl;
    procedure setVelocity(velocity: Single); cdecl;
    function spin: Single; cdecl;
    function velocity: Single; cdecl;
  end;
  TCAEmitterLayer = class(TOCGenericImport<CAEmitterLayerClass, CAEmitterLayer>) end;

  CAMediaTimingFunctionClass = interface(NSObjectClass)
    ['{9C76B637-6BBA-40EC-A3A7-E2509520D3FE}']
    {class} function functionWithControlPoints(c1x: Single; c1y: Single; c2x: Single; c2y: Single): Pointer; cdecl;
    {class} function functionWithName(name: CAMediaTimingFunctionName): Pointer; cdecl;
  end;

  CAMediaTimingFunction = interface(NSObject)
    ['{99DCEF66-5449-469F-962A-9EBED988F436}']
    procedure getControlPointAtIndex(idx: NativeUInt; values: PSingle); cdecl;
    function initWithControlPoints(c1x: Single; c1y: Single; c2x: Single; c2y: Single): Pointer; cdecl;
  end;
  TCAMediaTimingFunction = class(TOCGenericImport<CAMediaTimingFunctionClass, CAMediaTimingFunction>) end;

  CAGradientLayerClass = interface(CALayerClass)
    ['{6C60E152-C92F-4383-9FBF-992B6AC0CB0F}']
  end;

  CAGradientLayer = interface(CALayer)
    ['{7B7707F1-87E6-473D-BBC4-30233687B2AC}']
    function &type: CAGradientLayerType; cdecl;
    function colors: NSArray; cdecl;
    function endPoint: CGPoint; cdecl;
    function locations: NSArray; cdecl;
    procedure setColors(colors: NSArray); cdecl;
    procedure setEndPoint(endPoint: CGPoint); cdecl;
    procedure setLocations(locations: NSArray); cdecl;
    procedure setStartPoint(startPoint: CGPoint); cdecl;
    procedure setType(&type: CAGradientLayerType); cdecl;
    function startPoint: CGPoint; cdecl;
  end;
  TCAGradientLayer = class(TOCGenericImport<CAGradientLayerClass, CAGradientLayer>) end;

  CARendererClass = interface(NSObjectClass)
    ['{889F1DBE-2BDD-486A-896B-AD5A5DF631AD}']
    {class} function rendererWithMTLTexture(tex: Pointer; options: NSDictionary): CARenderer; cdecl;
  end;

  CARenderer = interface(NSObject)
    ['{E2CD390F-C57E-47CC-B7DE-D1304A2F76B2}']
    procedure addUpdateRect(r: CGRect); cdecl;
    procedure beginFrameAtTime(t: CFTimeInterval; timeStamp: PCVTimeStamp); cdecl;
    function bounds: CGRect; cdecl;
    procedure endFrame; cdecl;
    function layer: CALayer; cdecl;
    function nextFrameTime: CFTimeInterval; cdecl;
    procedure render; cdecl;
    procedure setBounds(bounds: CGRect); cdecl;
    procedure setDestination(tex: Pointer); cdecl;
    procedure setLayer(layer: CALayer); cdecl;
    function updateBounds: CGRect; cdecl;
  end;
  TCARenderer = class(TOCGenericImport<CARendererClass, CARenderer>) end;

  CAReplicatorLayerClass = interface(CALayerClass)
    ['{4D1EEE72-A91B-4D6D-814C-4BF3448E98EF}']
  end;

  CAReplicatorLayer = interface(CALayer)
    ['{30DBAE72-2F59-4BB6-94A9-EA6FAB3423C7}']
    function instanceAlphaOffset: Single; cdecl;
    function instanceBlueOffset: Single; cdecl;
    function instanceColor: CGColorRef; cdecl;
    function instanceCount: NSInteger; cdecl;
    function instanceDelay: CFTimeInterval; cdecl;
    function instanceGreenOffset: Single; cdecl;
    function instanceRedOffset: Single; cdecl;
    function instanceTransform: CATransform3D; cdecl;
    function preservesDepth: Boolean; cdecl;
    procedure setInstanceAlphaOffset(instanceAlphaOffset: Single); cdecl;
    procedure setInstanceBlueOffset(instanceBlueOffset: Single); cdecl;
    procedure setInstanceColor(instanceColor: CGColorRef); cdecl;
    procedure setInstanceCount(instanceCount: NSInteger); cdecl;
    procedure setInstanceDelay(instanceDelay: CFTimeInterval); cdecl;
    procedure setInstanceGreenOffset(instanceGreenOffset: Single); cdecl;
    procedure setInstanceRedOffset(instanceRedOffset: Single); cdecl;
    procedure setInstanceTransform(instanceTransform: CATransform3D); cdecl;
    procedure setPreservesDepth(preservesDepth: Boolean); cdecl;
  end;
  TCAReplicatorLayer = class(TOCGenericImport<CAReplicatorLayerClass, CAReplicatorLayer>) end;

  CAScrollLayerClass = interface(CALayerClass)
    ['{F88C4615-6B78-4CD8-AB82-B9C514B24853}']
  end;

  CAScrollLayer = interface(CALayer)
    ['{0F5B7DCB-4874-46AF-9BF6-A9A854775A6E}']
    function scrollMode: CAScrollLayerScrollMode; cdecl;
    procedure scrollToPoint(p: CGPoint); cdecl;
    procedure scrollToRect(r: CGRect); cdecl;
    procedure setScrollMode(scrollMode: CAScrollLayerScrollMode); cdecl;
  end;
  TCAScrollLayer = class(TOCGenericImport<CAScrollLayerClass, CAScrollLayer>) end;

  CAShapeLayerClass = interface(CALayerClass)
    ['{6A206463-4DD4-4032-AF0C-00F1B1EEA1DC}']
  end;

  CAShapeLayer = interface(CALayer)
    ['{8A23BF51-5725-49E6-A28D-1ECEF0AB1BCA}']
    function fillColor: CGColorRef; cdecl;
    function fillRule: CAShapeLayerFillRule; cdecl;
    function lineCap: CAShapeLayerLineCap; cdecl;
    function lineDashPattern: NSArray; cdecl;
    function lineDashPhase: CGFloat; cdecl;
    function lineJoin: CAShapeLayerLineJoin; cdecl;
    function lineWidth: CGFloat; cdecl;
    function miterLimit: CGFloat; cdecl;
    function path: CGPathRef; cdecl;
    procedure setFillColor(fillColor: CGColorRef); cdecl;
    procedure setFillRule(fillRule: CAShapeLayerFillRule); cdecl;
    procedure setLineCap(lineCap: CAShapeLayerLineCap); cdecl;
    procedure setLineDashPattern(lineDashPattern: NSArray); cdecl;
    procedure setLineDashPhase(lineDashPhase: CGFloat); cdecl;
    procedure setLineJoin(lineJoin: CAShapeLayerLineJoin); cdecl;
    procedure setLineWidth(lineWidth: CGFloat); cdecl;
    procedure setMiterLimit(miterLimit: CGFloat); cdecl;
    procedure setPath(path: CGPathRef); cdecl;
    procedure setStrokeColor(strokeColor: CGColorRef); cdecl;
    procedure setStrokeEnd(strokeEnd: CGFloat); cdecl;
    procedure setStrokeStart(strokeStart: CGFloat); cdecl;
    function strokeColor: CGColorRef; cdecl;
    function strokeEnd: CGFloat; cdecl;
    function strokeStart: CGFloat; cdecl;
  end;
  TCAShapeLayer = class(TOCGenericImport<CAShapeLayerClass, CAShapeLayer>) end;

  CATextLayerClass = interface(CALayerClass)
    ['{B88AC8ED-268A-4271-AFEB-FDD142B982D7}']
  end;

  CATextLayer = interface(CALayer)
    ['{F92038A0-8CBB-4AFB-9699-B219D386E904}']
    function &string: Pointer; cdecl;
    function alignmentMode: CATextLayerAlignmentMode; cdecl;
    function allowsFontSubpixelQuantization: Boolean; cdecl;
    function font: CFTypeRef; cdecl;
    function fontSize: CGFloat; cdecl;
    function foregroundColor: CGColorRef; cdecl;
    function isWrapped: Boolean; cdecl;
    procedure setAlignmentMode(alignmentMode: CATextLayerAlignmentMode); cdecl;
    procedure setAllowsFontSubpixelQuantization(allowsFontSubpixelQuantization: Boolean); cdecl;
    procedure setFont(font: CFTypeRef); cdecl;
    procedure setFontSize(fontSize: CGFloat); cdecl;
    procedure setForegroundColor(foregroundColor: CGColorRef); cdecl;
    procedure setString(&string: Pointer); cdecl;
    procedure setTruncationMode(truncationMode: CATextLayerTruncationMode); cdecl;
    procedure setWrapped(wrapped: Boolean); cdecl;
    function truncationMode: CATextLayerTruncationMode; cdecl;
  end;
  TCATextLayer = class(TOCGenericImport<CATextLayerClass, CATextLayer>) end;

  CATiledLayerClass = interface(CALayerClass)
    ['{F04EA732-9B67-479D-9365-79A27B82E4BB}']
    {class} function fadeDuration: CFTimeInterval; cdecl;
  end;

  CATiledLayer = interface(CALayer)
    ['{D8CF8FFA-3451-4314-B9DF-BB7367C3ACD4}']
    function levelsOfDetail: NativeUInt; cdecl;
    function levelsOfDetailBias: NativeUInt; cdecl;
    procedure setLevelsOfDetail(levelsOfDetail: NativeUInt); cdecl;
    procedure setLevelsOfDetailBias(levelsOfDetailBias: NativeUInt); cdecl;
    procedure setTileSize(tileSize: CGSize); cdecl;
    function tileSize: CGSize; cdecl;
  end;
  TCATiledLayer = class(TOCGenericImport<CATiledLayerClass, CATiledLayer>) end;

  CATransactionClass = interface(NSObjectClass)
    ['{404CE1FF-A496-4EB6-B617-199B4B7C2C1D}']
    {class} procedure &begin; cdecl;
    {class} function animationDuration: CFTimeInterval; cdecl;
    {class} function animationTimingFunction: CAMediaTimingFunction; cdecl;
    {class} procedure commit; cdecl;
    {class} function completionBlock: TCATransactionBlockMethod1; cdecl;
    {class} function disableActions: Boolean; cdecl;
    {class} procedure flush; cdecl;
    {class} procedure lock; cdecl;
    {class} procedure setAnimationDuration(dur: CFTimeInterval); cdecl;
    {class} procedure setAnimationTimingFunction(&function: CAMediaTimingFunction); cdecl;
    {class} procedure setCompletionBlock(block: TCATransactionBlockMethod1); cdecl;
    {class} procedure setDisableActions(flag: Boolean); cdecl;
    {class} procedure setValue(anObject: Pointer; forKey: NSString); cdecl;
    {class} procedure unlock; cdecl;
    {class} function valueForKey(key: NSString): Pointer; cdecl;
  end;

  CATransaction = interface(NSObject)
    ['{B94FEF34-72AB-4EDF-A5E1-5883F0B36981}']
  end;
  TCATransaction = class(TOCGenericImport<CATransactionClass, CATransaction>) end;

  CATransformLayerClass = interface(CALayerClass)
    ['{ED8AF215-F3C7-47AB-B3E0-1FC6EC531FAE}']
  end;

  CATransformLayer = interface(CALayer)
    ['{9EC2BCCE-5805-4413-8BA8-FFF0DA5B8185}']
  end;
  TCATransformLayer = class(TOCGenericImport<CATransformLayerClass, CATransformLayer>) end;

  CAValueFunctionClass = interface(NSObjectClass)
    ['{3C572A3B-8A02-4043-A490-DB7EFA20709A}']
    {class} function functionWithName(name: CAValueFunctionName): Pointer; cdecl;
  end;

  CAValueFunction = interface(NSObject)
    ['{60002CE7-B885-4830-B58B-9ED1A506C8F5}']
    function name: CAValueFunctionName; cdecl;
  end;
  TCAValueFunction = class(TOCGenericImport<CAValueFunctionClass, CAValueFunction>) end;

function kCAFillModeForwards: CAMediaTimingFillMode;
function kCAFillModeBackwards: CAMediaTimingFillMode;
function kCAFillModeBoth: CAMediaTimingFillMode;
function kCAFillModeRemoved: CAMediaTimingFillMode;
function kCAGravityCenter: CALayerContentsGravity;
function kCAGravityTop: CALayerContentsGravity;
function kCAGravityBottom: CALayerContentsGravity;
function kCAGravityLeft: CALayerContentsGravity;
function kCAGravityRight: CALayerContentsGravity;
function kCAGravityTopLeft: CALayerContentsGravity;
function kCAGravityTopRight: CALayerContentsGravity;
function kCAGravityBottomLeft: CALayerContentsGravity;
function kCAGravityBottomRight: CALayerContentsGravity;
function kCAGravityResize: CALayerContentsGravity;
function kCAGravityResizeAspect: CALayerContentsGravity;
function kCAGravityResizeAspectFill: CALayerContentsGravity;
function kCAContentsFormatRGBA8Uint: CALayerContentsFormat;
function kCAContentsFormatRGBA16Float: CALayerContentsFormat;
function kCAContentsFormatGray8Uint: CALayerContentsFormat;
function kCAFilterNearest: CALayerContentsFilter;
function kCAFilterLinear: CALayerContentsFilter;
function kCAFilterTrilinear: CALayerContentsFilter;
function kCACornerCurveCircular: CALayerCornerCurve;
function kCACornerCurveContinuous: CALayerCornerCurve;
function kCAOnOrderIn: NSString;
function kCAOnOrderOut: NSString;
function kCATransition: NSString;
function kCAAnimationLinear: CAAnimationCalculationMode;
function kCAAnimationDiscrete: CAAnimationCalculationMode;
function kCAAnimationPaced: CAAnimationCalculationMode;
function kCAAnimationCubic: CAAnimationCalculationMode;
function kCAAnimationCubicPaced: CAAnimationCalculationMode;
function kCAAnimationRotateAuto: CAAnimationRotationMode;
function kCAAnimationRotateAutoReverse: CAAnimationRotationMode;
function kCATransitionFade: CATransitionType;
function kCATransitionMoveIn: CATransitionType;
function kCATransitionPush: CATransitionType;
function kCATransitionReveal: CATransitionType;
function kCATransitionFromRight: CATransitionSubtype;
function kCATransitionFromLeft: CATransitionSubtype;
function kCATransitionFromTop: CATransitionSubtype;
function kCATransitionFromBottom: CATransitionSubtype;
function kCAEmitterLayerPoint: CAEmitterLayerEmitterShape;
function kCAEmitterLayerLine: CAEmitterLayerEmitterShape;
function kCAEmitterLayerRectangle: CAEmitterLayerEmitterShape;
function kCAEmitterLayerCuboid: CAEmitterLayerEmitterShape;
function kCAEmitterLayerCircle: CAEmitterLayerEmitterShape;
function kCAEmitterLayerSphere: CAEmitterLayerEmitterShape;
function kCAEmitterLayerPoints: CAEmitterLayerEmitterMode;
function kCAEmitterLayerOutline: CAEmitterLayerEmitterMode;
function kCAEmitterLayerSurface: CAEmitterLayerEmitterMode;
function kCAEmitterLayerVolume: CAEmitterLayerEmitterMode;
function kCAEmitterLayerUnordered: CAEmitterLayerRenderMode;
function kCAEmitterLayerOldestFirst: CAEmitterLayerRenderMode;
function kCAEmitterLayerOldestLast: CAEmitterLayerRenderMode;
function kCAEmitterLayerBackToFront: CAEmitterLayerRenderMode;
function kCAEmitterLayerAdditive: CAEmitterLayerRenderMode;
function kCAMediaTimingFunctionLinear: CAMediaTimingFunctionName;
function kCAMediaTimingFunctionEaseIn: CAMediaTimingFunctionName;
function kCAMediaTimingFunctionEaseOut: CAMediaTimingFunctionName;
function kCAMediaTimingFunctionEaseInEaseOut: CAMediaTimingFunctionName;
function kCAMediaTimingFunctionDefault: CAMediaTimingFunctionName;
function kCAGradientLayerAxial: CAGradientLayerType;
function kCAGradientLayerRadial: CAGradientLayerType;
function kCAGradientLayerConic: CAGradientLayerType;
function kCARendererColorSpace: NSString;
function kCARendererMetalCommandQueue: NSString;
function kCAScrollNone: CAScrollLayerScrollMode;
function kCAScrollVertically: CAScrollLayerScrollMode;
function kCAScrollHorizontally: CAScrollLayerScrollMode;
function kCAScrollBoth: CAScrollLayerScrollMode;
function kCAFillRuleNonZero: CAShapeLayerFillRule;
function kCAFillRuleEvenOdd: CAShapeLayerFillRule;
function kCALineJoinMiter: CAShapeLayerLineJoin;
function kCALineJoinRound: CAShapeLayerLineJoin;
function kCALineJoinBevel: CAShapeLayerLineJoin;
function kCALineCapButt: CAShapeLayerLineCap;
function kCALineCapRound: CAShapeLayerLineCap;
function kCALineCapSquare: CAShapeLayerLineCap;
function kCATruncationNone: CATextLayerTruncationMode;
function kCATruncationStart: CATextLayerTruncationMode;
function kCATruncationEnd: CATextLayerTruncationMode;
function kCATruncationMiddle: CATextLayerTruncationMode;
function kCAAlignmentNatural: CATextLayerAlignmentMode;
function kCAAlignmentLeft: CATextLayerAlignmentMode;
function kCAAlignmentRight: CATextLayerAlignmentMode;
function kCAAlignmentCenter: CATextLayerAlignmentMode;
function kCAAlignmentJustified: CATextLayerAlignmentMode;
function kCATransactionAnimationDuration: NSString;
function kCATransactionDisableActions: NSString;
function kCATransactionAnimationTimingFunction: NSString;
function kCATransactionCompletionBlock: NSString;
function kCAValueFunctionRotateX: CAValueFunctionName;
function kCAValueFunctionRotateY: CAValueFunctionName;
function kCAValueFunctionRotateZ: CAValueFunctionName;
function kCAValueFunctionScale: CAValueFunctionName;
function kCAValueFunctionScaleX: CAValueFunctionName;
function kCAValueFunctionScaleY: CAValueFunctionName;
function kCAValueFunctionScaleZ: CAValueFunctionName;
function kCAValueFunctionTranslate: CAValueFunctionName;
function kCAValueFunctionTranslateX: CAValueFunctionName;
function kCAValueFunctionTranslateY: CAValueFunctionName;
function kCAValueFunctionTranslateZ: CAValueFunctionName;

const
  libQuartzCore = '/System/Library/Frameworks/QuartzCore.framework/QuartzCore';

function CACurrentMediaTime: CFTimeInterval; cdecl;
  external libQuartzCore name _PU + 'CACurrentMediaTime';

function CATransform3DIsIdentity(t: CATransform3D): Boolean; cdecl;
  external libQuartzCore name _PU + 'CATransform3DIsIdentity';

function CATransform3DEqualToTransform(a: CATransform3D; b: CATransform3D): Boolean; cdecl;
  external libQuartzCore name _PU + 'CATransform3DEqualToTransform';

function CATransform3DMakeTranslation(tx: CGFloat; ty: CGFloat; tz: CGFloat): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DMakeTranslation';

function CATransform3DMakeScale(sx: CGFloat; sy: CGFloat; sz: CGFloat): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DMakeScale';

function CATransform3DMakeRotation(angle: CGFloat; x: CGFloat; y: CGFloat; z: CGFloat): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DMakeRotation';

function CATransform3DTranslate(t: CATransform3D; tx: CGFloat; ty: CGFloat; tz: CGFloat): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DTranslate';

function CATransform3DScale(t: CATransform3D; sx: CGFloat; sy: CGFloat; sz: CGFloat): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DScale';

function CATransform3DRotate(t: CATransform3D; angle: CGFloat; x: CGFloat; y: CGFloat; z: CGFloat): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DRotate';

function CATransform3DConcat(a: CATransform3D; b: CATransform3D): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DConcat';

function CATransform3DInvert(t: CATransform3D): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DInvert';

function CATransform3DMakeAffineTransform(m: CGAffineTransform): CATransform3D; cdecl;
  external libQuartzCore name _PU + 'CATransform3DMakeAffineTransform';

function CATransform3DIsAffine(t: CATransform3D): Boolean; cdecl;
  external libQuartzCore name _PU + 'CATransform3DIsAffine';

function CATransform3DGetAffineTransform(t: CATransform3D): CGAffineTransform; cdecl;
  external libQuartzCore name _PU + 'CATransform3DGetAffineTransform';

function CAFrameRateRangeMake(minimum: Single; maximum: Single; preferred: Single): CAFrameRateRange; cdecl;
  external libQuartzCore name _PU + 'CAFrameRateRangeMake';

function CAFrameRateRangeIsEqualToRange(range: CAFrameRateRange; other: CAFrameRateRange): Boolean; cdecl;
  external libQuartzCore name _PU + 'CAFrameRateRangeIsEqualToRange';

implementation

uses
  Posix.Dlfcn;

var
  QuartzCoreModule: THandle;

function kCAFillModeForwards: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFillModeForwards');
end;

function kCAFillModeBackwards: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFillModeBackwards');
end;

function kCAFillModeBoth: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFillModeBoth');
end;

function kCAFillModeRemoved: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFillModeRemoved');
end;

function kCAGravityCenter: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityCenter');
end;

function kCAGravityTop: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityTop');
end;

function kCAGravityBottom: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityBottom');
end;

function kCAGravityLeft: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityLeft');
end;

function kCAGravityRight: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityRight');
end;

function kCAGravityTopLeft: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityTopLeft');
end;

function kCAGravityTopRight: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityTopRight');
end;

function kCAGravityBottomLeft: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityBottomLeft');
end;

function kCAGravityBottomRight: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityBottomRight');
end;

function kCAGravityResize: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityResize');
end;

function kCAGravityResizeAspect: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityResizeAspect');
end;

function kCAGravityResizeAspectFill: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGravityResizeAspectFill');
end;

function kCAContentsFormatRGBA8Uint: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAContentsFormatRGBA8Uint');
end;

function kCAContentsFormatRGBA16Float: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAContentsFormatRGBA16Float');
end;

function kCAContentsFormatGray8Uint: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAContentsFormatGray8Uint');
end;

function kCAFilterNearest: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFilterNearest');
end;

function kCAFilterLinear: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFilterLinear');
end;

function kCAFilterTrilinear: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFilterTrilinear');
end;

function kCACornerCurveCircular: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCACornerCurveCircular');
end;

function kCACornerCurveContinuous: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCACornerCurveContinuous');
end;

function kCAOnOrderIn: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAOnOrderIn');
end;

function kCAOnOrderOut: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAOnOrderOut');
end;

function kCATransition: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransition');
end;

function kCAAnimationLinear: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAnimationLinear');
end;

function kCAAnimationDiscrete: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAnimationDiscrete');
end;

function kCAAnimationPaced: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAnimationPaced');
end;

function kCAAnimationCubic: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAnimationCubic');
end;

function kCAAnimationCubicPaced: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAnimationCubicPaced');
end;

function kCAAnimationRotateAuto: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAnimationRotateAuto');
end;

function kCAAnimationRotateAutoReverse: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAnimationRotateAutoReverse');
end;

function kCATransitionFade: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransitionFade');
end;

function kCATransitionMoveIn: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransitionMoveIn');
end;

function kCATransitionPush: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransitionPush');
end;

function kCATransitionReveal: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransitionReveal');
end;

function kCATransitionFromRight: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransitionFromRight');
end;

function kCATransitionFromLeft: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransitionFromLeft');
end;

function kCATransitionFromTop: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransitionFromTop');
end;

function kCATransitionFromBottom: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransitionFromBottom');
end;

function kCAEmitterLayerPoint: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerPoint');
end;

function kCAEmitterLayerLine: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerLine');
end;

function kCAEmitterLayerRectangle: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerRectangle');
end;

function kCAEmitterLayerCuboid: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerCuboid');
end;

function kCAEmitterLayerCircle: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerCircle');
end;

function kCAEmitterLayerSphere: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerSphere');
end;

function kCAEmitterLayerPoints: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerPoints');
end;

function kCAEmitterLayerOutline: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerOutline');
end;

function kCAEmitterLayerSurface: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerSurface');
end;

function kCAEmitterLayerVolume: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerVolume');
end;

function kCAEmitterLayerUnordered: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerUnordered');
end;

function kCAEmitterLayerOldestFirst: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerOldestFirst');
end;

function kCAEmitterLayerOldestLast: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerOldestLast');
end;

function kCAEmitterLayerBackToFront: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerBackToFront');
end;

function kCAEmitterLayerAdditive: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAEmitterLayerAdditive');
end;

function kCAMediaTimingFunctionLinear: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAMediaTimingFunctionLinear');
end;

function kCAMediaTimingFunctionEaseIn: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAMediaTimingFunctionEaseIn');
end;

function kCAMediaTimingFunctionEaseOut: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAMediaTimingFunctionEaseOut');
end;

function kCAMediaTimingFunctionEaseInEaseOut: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAMediaTimingFunctionEaseInEaseOut');
end;

function kCAMediaTimingFunctionDefault: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAMediaTimingFunctionDefault');
end;

function kCAGradientLayerAxial: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGradientLayerAxial');
end;

function kCAGradientLayerRadial: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGradientLayerRadial');
end;

function kCAGradientLayerConic: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAGradientLayerConic');
end;

function kCARendererColorSpace: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCARendererColorSpace');
end;

function kCARendererMetalCommandQueue: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCARendererMetalCommandQueue');
end;

function kCAScrollNone: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAScrollNone');
end;

function kCAScrollVertically: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAScrollVertically');
end;

function kCAScrollHorizontally: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAScrollHorizontally');
end;

function kCAScrollBoth: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAScrollBoth');
end;

function kCAFillRuleNonZero: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFillRuleNonZero');
end;

function kCAFillRuleEvenOdd: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAFillRuleEvenOdd');
end;

function kCALineJoinMiter: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCALineJoinMiter');
end;

function kCALineJoinRound: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCALineJoinRound');
end;

function kCALineJoinBevel: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCALineJoinBevel');
end;

function kCALineCapButt: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCALineCapButt');
end;

function kCALineCapRound: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCALineCapRound');
end;

function kCALineCapSquare: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCALineCapSquare');
end;

function kCATruncationNone: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATruncationNone');
end;

function kCATruncationStart: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATruncationStart');
end;

function kCATruncationEnd: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATruncationEnd');
end;

function kCATruncationMiddle: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATruncationMiddle');
end;

function kCAAlignmentNatural: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAlignmentNatural');
end;

function kCAAlignmentLeft: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAlignmentLeft');
end;

function kCAAlignmentRight: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAlignmentRight');
end;

function kCAAlignmentCenter: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAlignmentCenter');
end;

function kCAAlignmentJustified: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAAlignmentJustified');
end;

function kCATransactionAnimationDuration: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransactionAnimationDuration');
end;

function kCATransactionDisableActions: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransactionDisableActions');
end;

function kCATransactionAnimationTimingFunction: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransactionAnimationTimingFunction');
end;

function kCATransactionCompletionBlock: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCATransactionCompletionBlock');
end;

function kCAValueFunctionRotateX: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionRotateX');
end;

function kCAValueFunctionRotateY: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionRotateY');
end;

function kCAValueFunctionRotateZ: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionRotateZ');
end;

function kCAValueFunctionScale: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionScale');
end;

function kCAValueFunctionScaleX: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionScaleX');
end;

function kCAValueFunctionScaleY: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionScaleY');
end;

function kCAValueFunctionScaleZ: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionScaleZ');
end;

function kCAValueFunctionTranslate: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionTranslate');
end;

function kCAValueFunctionTranslateX: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionTranslateX');
end;

function kCAValueFunctionTranslateY: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionTranslateY');
end;

function kCAValueFunctionTranslateZ: NSString;
begin
  Result := CocoaNSStringConst(libQuartzCore, 'kCAValueFunctionTranslateZ');
end;

initialization
  QuartzCoreModule := dlopen(MarshaledAString(libQuartzCore), RTLD_LAZY);

finalization
  dlclose(QuartzCoreModule);

end.