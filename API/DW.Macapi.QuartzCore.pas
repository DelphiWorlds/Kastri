unit DW.Macapi.QuartzCore;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.CoreVideo, Macapi.CoreGraphics, Macapi.Foundation, Macapi.Metal, Macapi.OpenGL,
  Macapi.Mach,
  // DW
  DW.Macapi.Foundation;

const
  kCALayerNotSizable = 0;
  kCALayerMinXMargin = 1;
  kCALayerWidthSizable = 2;
  kCALayerMaxXMargin = 4;
  kCALayerMinYMargin = 8;
  kCALayerHeightSizable = 16;
  kCALayerMaxYMargin = 32;
  kCALayerLeftEdge = 1;
  kCALayerRightEdge = 2;
  kCALayerBottomEdge = 4;
  kCALayerTopEdge = 8;
  kCALayerMinXMinYCorner = 1;
  kCALayerMaxXMinYCorner = 2;
  kCALayerMinXMaxYCorner = 4;
  kCALayerMaxXMaxYCorner = 8;
  kCAConstraintMinX = 0;
  kCAConstraintMidX = 1;
  kCAConstraintMaxX = 2;
  kCAConstraintWidth = 3;
  kCAConstraintMinY = 4;
  kCAConstraintMidY = 5;
  kCAConstraintMaxY = 6;
  kCAConstraintHeight = 7;

type
  CAMediaTiming = interface;
  CALayer = interface;
  CALayoutManager = interface;
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
  CAConstraintLayoutManager = interface;
  CAConstraint = interface;
  CADisplayLink = interface;
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
  CAOpenGLLayer = interface;
  CARemoteLayerClient = interface;
  CARemoteLayerServer = interface;
  CARenderer = interface;
  CAReplicatorLayer = interface;
  CAScrollLayer = interface;
  CAShapeLayer = interface;
  CATextLayer = interface;
  CATiledLayer = interface;
  CATransaction = interface;
  CATransformLayer = interface;
  CAValueFunction = interface;

  P_CAEDRMetadataPrivate = Pointer;
  PP_CAEDRMetadataPrivate = ^P_CAEDRMetadataPrivate;
  P_CAMetalLayerPrivate = Pointer;
  PP_CAMetalLayerPrivate = ^P_CAMetalLayerPrivate;
  PCAMediaTimingFunctionPrivate = Pointer;
  PPCAMediaTimingFunctionPrivate = ^PCAMediaTimingFunctionPrivate;
  PCAOpenGLLayerPrivate = Pointer;
  PPCAOpenGLLayerPrivate = ^PCAOpenGLLayerPrivate;
  PCARendererPriv = Pointer;
  PPCARendererPriv = ^PCARendererPriv;
  PCATextLayerPrivate = Pointer;
  PPCATextLayerPrivate = ^PCATextLayerPrivate;
  PCATransform3D = ^CATransform3D;
  PCAFrameRateRange = ^CAFrameRateRange;

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
  CAAutoresizingMask = NSInteger;
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
  CAConstraintAttribute = NSInteger;
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
    ['{6FFB2E78-804D-4948-ABB9-F7BE6203659A}']
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
    ['{05228032-04AD-49F8-837B-BEF6E436E54F}']
    {class} function cornerCurveExpansionFactor(curve: CALayerCornerCurve): CGFloat; cdecl;
    {class} function defaultActionForKey(event: NSString): Pointer; cdecl;
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
    {class} function layer: Pointer; cdecl;
    {class} function layerWithRemoteClientId(client_id: UInt32): CALayer; cdecl;
    {class} function needsDisplayForKey(key: NSString): Boolean; cdecl;
  end;

  CALayer = interface(NSObject)
    ['{0E888E4C-4959-4476-9CFB-9156918376E0}']
    function actionForKey(event: NSString): Pointer; cdecl;
    function actions: NSDictionary; cdecl;
    procedure addAnimation(anim: CAAnimation; forKey: NSString); cdecl;
    procedure addConstraint(c: CAConstraint); cdecl;
    procedure addSublayer(layer: CALayer); cdecl;
    function affineTransform: CGAffineTransform; cdecl;
    function allowsEdgeAntialiasing: Boolean; cdecl;
    function allowsGroupOpacity: Boolean; cdecl;
    function anchorPoint: CGPoint; cdecl;
    function anchorPointZ: CGFloat; cdecl;
    function animationForKey(key: NSString): CAAnimation; cdecl;
    function animationKeys: NSArray; cdecl;
    function autoresizingMask: CAAutoresizingMask; cdecl;
    function backgroundColor: CGColorRef; cdecl;
    function backgroundFilters: NSArray; cdecl;
    function borderColor: CGColorRef; cdecl;
    function borderWidth: CGFloat; cdecl;
    function bounds: CGRect; cdecl;
    function compositingFilter: Pointer; cdecl;
    function constraints: NSArray; cdecl;
    function containsPoint(p: CGPoint): Boolean; cdecl;
    function contents: Pointer; cdecl;
    function contentsAreFlipped: Boolean; cdecl;
    function contentsCenter: CGRect; cdecl;
    function contentsFormat: CALayerContentsFormat; cdecl;
    function contentsGravity: CALayerContentsGravity; cdecl;
    function contentsRect: CGRect; cdecl;
    function contentsScale: CGFloat; cdecl;
    [MethodName('convertPoint:fromLayer:')]
    function convertPointFromLayer(p: CGPoint; fromLayer: CALayer): CGPoint; cdecl;
    [MethodName('convertPoint:toLayer:')]
    function convertPointToLayer(p: CGPoint; toLayer: CALayer): CGPoint; cdecl;
    [MethodName('convertRect:fromLayer:')]
    function convertRectFromLayer(r: CGRect; fromLayer: CALayer): CGRect; cdecl;
    [MethodName('convertRect:toLayer:')]
    function convertRectToLayer(r: CGRect; toLayer: CALayer): CGRect; cdecl;
    [MethodName('convertTime:fromLayer:')]
    function convertTimeFromLayer(t: CFTimeInterval; fromLayer: CALayer): CFTimeInterval; cdecl;
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
    [MethodName('insertSublayer:above:')]
    procedure insertSublayerAbove(layer: CALayer; above: CALayer); cdecl;
    [MethodName('insertSublayer:atIndex:')]
    procedure insertSublayerAtIndex(layer: CALayer; atIndex: Cardinal); cdecl;
    [MethodName('insertSublayer:below:')]
    procedure insertSublayerBelow(layer: CALayer; below: CALayer); cdecl;
    function isDoubleSided: Boolean; cdecl;
    function isGeometryFlipped: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isOpaque: Boolean; cdecl;
    procedure layoutIfNeeded; cdecl;
    function layoutManager: Pointer; cdecl;
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
    procedure resizeSublayersWithOldSize(size: CGSize); cdecl;
    procedure resizeWithOldSuperlayerSize(size: CGSize); cdecl;
    procedure scrollPoint(p: CGPoint); cdecl;
    procedure scrollRectToVisible(r: CGRect); cdecl;
    procedure setActions(actions: NSDictionary); cdecl;
    procedure setAffineTransform(m: CGAffineTransform); cdecl;
    procedure setAllowsEdgeAntialiasing(allowsEdgeAntialiasing: Boolean); cdecl;
    procedure setAllowsGroupOpacity(allowsGroupOpacity: Boolean); cdecl;
    procedure setAnchorPoint(anchorPoint: CGPoint); cdecl;
    procedure setAnchorPointZ(anchorPointZ: CGFloat); cdecl;
    procedure setAutoresizingMask(autoresizingMask: CAAutoresizingMask); cdecl;
    procedure setBackgroundColor(backgroundColor: CGColorRef); cdecl;
    procedure setBackgroundFilters(backgroundFilters: NSArray); cdecl;
    procedure setBorderColor(borderColor: CGColorRef); cdecl;
    procedure setBorderWidth(borderWidth: CGFloat); cdecl;
    procedure setBounds(bounds: CGRect); cdecl;
    procedure setCompositingFilter(compositingFilter: Pointer); cdecl;
    procedure setConstraints(constraints: NSArray); cdecl;
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
    procedure setLayoutManager(layoutManager: Pointer); cdecl;
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

  CALayoutManager = interface(IObjectiveC)
    ['{0224198D-5CAC-4207-A7D7-E0E593DD99C5}']
    procedure invalidateLayoutOfLayer(layer: CALayer); cdecl;
    procedure layoutSublayersOfLayer(layer: CALayer); cdecl;
    function preferredSizeOfLayer(layer: CALayer): CGSize; cdecl;
  end;

  CAAction = interface(IObjectiveC)
    ['{268CA170-6337-4DEA-96BC-6E82F6799106}']
    procedure runActionForKey(event: NSString; &object: Pointer; arguments: NSDictionary); cdecl;
  end;

  CALayerDelegate = interface(IObjectiveC)
    ['{D299B3A1-F606-4E84-B190-E1BCD0713987}']
    function actionForLayer(layer: CALayer; forKey: NSString): Pointer; cdecl;
    procedure displayLayer(layer: CALayer); cdecl;
    procedure drawLayer(layer: CALayer; inContext: CGContextRef); cdecl;
    procedure layerWillDraw(layer: CALayer); cdecl;
    procedure layoutSublayersOfLayer(layer: CALayer); cdecl;
  end;

  CAAnimationClass = interface(NSObjectClass)
    ['{1F859F1E-790B-4C45-A3CD-6BEF5A18C111}']
    {class} function animation: Pointer; cdecl;
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
  end;

  CAAnimation = interface(NSObject)
    ['{DDD34480-085D-4E15-8E65-45479BAA9E50}']
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
    ['{9DC20372-DE62-4FC2-9305-3E739AF1DDCC}']
    procedure animationDidStart(anim: CAAnimation); cdecl;
    procedure animationDidStop(anim: CAAnimation; finished: Boolean); cdecl;
  end;

  CAPropertyAnimationClass = interface(CAAnimationClass)
    ['{5877277D-DC90-47AF-8409-D4D134473E20}']
    {class} function animationWithKeyPath(path: NSString): Pointer; cdecl;
  end;

  CAPropertyAnimation = interface(CAAnimation)
    ['{06E50074-7A92-491E-B9CC-AB4DFFEF31C9}']
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
    ['{712661B2-E2D7-4052-8709-E6564D386CAE}']
  end;

  CABasicAnimation = interface(CAPropertyAnimation)
    ['{D7DC317D-5B04-40DB-8C3B-156669522C72}']
    function byValue: Pointer; cdecl;
    function fromValue: Pointer; cdecl;
    procedure setByValue(byValue: Pointer); cdecl;
    procedure setFromValue(fromValue: Pointer); cdecl;
    procedure setToValue(toValue: Pointer); cdecl;
    function toValue: Pointer; cdecl;
  end;
  TCABasicAnimation = class(TOCGenericImport<CABasicAnimationClass, CABasicAnimation>) end;

  CAKeyframeAnimationClass = interface(CAPropertyAnimationClass)
    ['{2BA4036E-E9E0-4781-85C6-B8B815BCAF34}']
  end;

  CAKeyframeAnimation = interface(CAPropertyAnimation)
    ['{840D3D92-246D-4495-B1C4-A7263B89D66A}']
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
    ['{B91BCF84-E7D4-4180-BC34-757DCB899E06}']
  end;

  CASpringAnimation = interface(CABasicAnimation)
    ['{0823123A-069D-4E47-9E6D-253472C456D2}']
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
    ['{22E62320-5384-4171-BFB6-55BCB838C519}']
  end;

  CATransition = interface(CAAnimation)
    ['{ABFF6DE4-4507-4CEA-B01A-B415C2B10526}']
    function &type: CATransitionType; cdecl;
    function endProgress: Single; cdecl;
    function filter: Pointer; cdecl;
    procedure setEndProgress(endProgress: Single); cdecl;
    procedure setFilter(filter: Pointer); cdecl;
    procedure setStartProgress(startProgress: Single); cdecl;
    procedure setSubtype(subtype: CATransitionSubtype); cdecl;
    procedure setType(&type: CATransitionType); cdecl;
    function startProgress: Single; cdecl;
    function subtype: CATransitionSubtype; cdecl;
  end;
  TCATransition = class(TOCGenericImport<CATransitionClass, CATransition>) end;

  CAAnimationGroupClass = interface(CAAnimationClass)
    ['{94F00091-32C1-4A99-96B4-545B414F9438}']
  end;

  CAAnimationGroup = interface(CAAnimation)
    ['{D61E3D86-16C2-446D-B033-6CB1B0A64B9A}']
    function animations: NSArray; cdecl;
    procedure setAnimations(animations: NSArray); cdecl;
  end;
  TCAAnimationGroup = class(TOCGenericImport<CAAnimationGroupClass, CAAnimationGroup>) end;

  CAConstraintLayoutManagerClass = interface(NSObjectClass)
    ['{4C667316-6832-4831-B794-262977E75767}']
    {class} function layoutManager: Pointer; cdecl;
  end;

  CAConstraintLayoutManager = interface(NSObject)
    ['{FEB281CF-9754-4E6D-BFC5-6A5EACE01439}']
  end;
  TCAConstraintLayoutManager = class(TOCGenericImport<CAConstraintLayoutManagerClass, CAConstraintLayoutManager>) end;

  CAConstraintClass = interface(NSObjectClass)
    ['{ACF4ACAC-0E9C-4FEE-AF47-4C7DC8460C1D}']
    {class} function constraintWithAttribute(attr: CAConstraintAttribute; relativeTo: NSString; attribute: CAConstraintAttribute): Pointer; overload; cdecl;
    {class} function constraintWithAttribute(attr: CAConstraintAttribute; relativeTo: NSString; attribute: CAConstraintAttribute; offset: CGFloat): Pointer; overload; cdecl;
    {class} function constraintWithAttribute(attr: CAConstraintAttribute; relativeTo: NSString; attribute: CAConstraintAttribute; scale: CGFloat; offset: CGFloat): Pointer; overload; cdecl;
  end;

  CAConstraint = interface(NSObject)
    ['{A1376527-1E8B-44B1-AB8C-7B60D8647BA5}']
    function attribute: CAConstraintAttribute; cdecl;
    function initWithAttribute(attr: CAConstraintAttribute; relativeTo: NSString; attribute: CAConstraintAttribute; scale: CGFloat; offset: CGFloat): Pointer; cdecl;
    function offset: CGFloat; cdecl;
    function scale: CGFloat; cdecl;
    function sourceAttribute: CAConstraintAttribute; cdecl;
    function sourceName: NSString; cdecl;
  end;
  TCAConstraint = class(TOCGenericImport<CAConstraintClass, CAConstraint>) end;

  CADisplayLinkClass = interface(NSObjectClass)
    ['{C838827B-C2C6-4D4C-86E2-13FDFFC20153}']
    {class} function displayLinkWithTarget(target: Pointer; selector: SEL): CADisplayLink; cdecl;
  end;

  CADisplayLink = interface(NSObject)
    ['{F539DD5E-0A0D-42BE-9FA3-DFBB62AFC4BF}']
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

  CAEDRMetadataClass = interface(NSObjectClass)
    ['{E05E38EC-A8E8-4C35-A545-C8E189CFEEAB}']
    {class} function HDR10MetadataWithDisplayInfo(displayData: NSData; contentInfo: NSData; opticalOutputScale: Single): CAEDRMetadata; cdecl;
    {class} function HDR10MetadataWithMinLuminance(minNits: Single; maxLuminance: Single; opticalOutputScale: Single): CAEDRMetadata; cdecl;
    {class} function HLGMetadata: CAEDRMetadata; cdecl;
    {class} function HLGMetadataWithAmbientViewingEnvironment(data: NSData): CAEDRMetadata; cdecl;
    {class} function isAvailable: Boolean; cdecl;
  end;

  CAEDRMetadata = interface(NSObject)
    ['{5F7E38C3-5C50-4DB0-82A0-87D43F459DAA}']
  end;
  TCAEDRMetadata = class(TOCGenericImport<CAEDRMetadataClass, CAEDRMetadata>) end;

  CAMetalDrawable = interface(IObjectiveC)
    ['{B0E8C6BE-01E4-4662-AFF5-70E54E376CE4}']
    function layer: CAMetalLayer; cdecl;
    function texture: Pointer; cdecl;
  end;

  CAMetalLayerClass = interface(CALayerClass)
    ['{58A49636-0CFE-4F98-94D1-AA73D9F0DEE9}']
  end;

  CAMetalLayer = interface(CALayer)
    ['{8493CC6E-4239-4864-AEA9-14E4C74D8E1A}']
    function allowsNextDrawableTimeout: Boolean; cdecl;
    function colorspace: CGColorSpaceRef; cdecl;
    function developerHUDProperties: NSDictionary; cdecl;
    function device: Pointer; cdecl;
    function displaySyncEnabled: Boolean; cdecl;
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
    procedure setDisplaySyncEnabled(displaySyncEnabled: Boolean); cdecl;
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
    ['{0A6CB819-7840-4192-B9B3-28535A37C7FF}']
  end;

  CAMetalDisplayLinkUpdate = interface(NSObject)
    ['{21860B38-B50B-42B8-940C-CEC3DCE492FD}']
    function drawable: Pointer; cdecl;
    function targetPresentationTimestamp: CFTimeInterval; cdecl;
    function targetTimestamp: CFTimeInterval; cdecl;
  end;
  TCAMetalDisplayLinkUpdate = class(TOCGenericImport<CAMetalDisplayLinkUpdateClass, CAMetalDisplayLinkUpdate>) end;

  CAMetalDisplayLinkDelegate = interface(IObjectiveC)
    ['{3B482998-E423-4625-BDCC-5AF04A552466}']
    procedure metalDisplayLink(link: CAMetalDisplayLink; needsUpdate: CAMetalDisplayLinkUpdate); cdecl;
  end;

  CAMetalDisplayLinkClass = interface(NSObjectClass)
    ['{07FC216E-76F0-4ED8-968D-BF4627371256}']
  end;

  CAMetalDisplayLink = interface(NSObject)
    ['{E5548B71-2753-4C56-B320-A6BF48AF43E9}']
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
    ['{790B250D-B7F3-4497-B7F6-957B676BF174}']
    {class} function defaultValueForKey(key: NSString): Pointer; cdecl;
    {class} function emitterCell: Pointer; cdecl;
  end;

  CAEmitterCell = interface(NSObject)
    ['{AA2D36FE-1D19-4FD8-BD29-CD4CE958A659}']
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
    ['{6ACE598C-AD67-4A40-AAA1-9CE0F2141561}']
  end;

  CAEmitterLayer = interface(CALayer)
    ['{C9E1A63C-A328-47DD-9F3E-80B9E060503B}']
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
    ['{4BC31588-0490-4329-ACF7-FD6FD4360B06}']
    {class} function functionWithControlPoints(c1x: Single; c1y: Single; c2x: Single; c2y: Single): Pointer; cdecl;
    {class} function functionWithName(name: CAMediaTimingFunctionName): Pointer; cdecl;
  end;

  CAMediaTimingFunction = interface(NSObject)
    ['{27715A7D-A958-47F1-A380-FE454BE55858}']
    procedure getControlPointAtIndex(idx: NativeUInt; values: PSingle); cdecl;
    function initWithControlPoints(c1x: Single; c1y: Single; c2x: Single; c2y: Single): Pointer; cdecl;
  end;
  TCAMediaTimingFunction = class(TOCGenericImport<CAMediaTimingFunctionClass, CAMediaTimingFunction>) end;

  CAGradientLayerClass = interface(CALayerClass)
    ['{6309FDD1-6C6C-4FE0-9C7B-3DC20EF57AF0}']
  end;

  CAGradientLayer = interface(CALayer)
    ['{627A9020-54C9-4552-B9B5-F1535E092479}']
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

  CAOpenGLLayerClass = interface(CALayerClass)
    ['{20559BA9-964D-4761-9A9C-6508F6615ABC}']
  end;

  CAOpenGLLayer = interface(CALayer)
    ['{5FB5E005-243F-434E-9925-53069EAF5045}']
    function canDrawInCGLContext(ctx: CGLContextObj; pixelFormat: CGLPixelFormatObj; forLayerTime: CFTimeInterval; displayTime: PCVTimeStamp): Boolean; cdecl;
    function colorspace: CGColorSpaceRef; cdecl;
    function copyCGLContextForPixelFormat(pf: CGLPixelFormatObj): CGLContextObj; cdecl;
    function copyCGLPixelFormatForDisplayMask(mask: UInt32): CGLPixelFormatObj; cdecl;
    procedure drawInCGLContext(ctx: CGLContextObj; pixelFormat: CGLPixelFormatObj; forLayerTime: CFTimeInterval; displayTime: PCVTimeStamp); cdecl;
    function isAsynchronous: Boolean; cdecl;
    procedure releaseCGLContext(ctx: CGLContextObj); cdecl;
    procedure releaseCGLPixelFormat(pf: CGLPixelFormatObj); cdecl;
    procedure setAsynchronous(asynchronous: Boolean); cdecl;
    procedure setColorspace(colorspace: CGColorSpaceRef); cdecl;
    procedure setWantsExtendedDynamicRangeContent(wantsExtendedDynamicRangeContent: Boolean); cdecl;
    function wantsExtendedDynamicRangeContent: Boolean; cdecl;
  end;
  TCAOpenGLLayer = class(TOCGenericImport<CAOpenGLLayerClass, CAOpenGLLayer>) end;

  CARemoteLayerClientClass = interface(NSObjectClass)
    ['{B2BDA184-1BDE-47DF-88FB-E2E60B6C4756}']
  end;

  CARemoteLayerClient = interface(NSObject)
    ['{B3CA6AF2-EC94-49D1-8D53-7D037747ACA4}']
    function clientId: UInt32; cdecl;
    function initWithServerPort(port: mach_port_t): Pointer; cdecl;
    procedure invalidate; cdecl;
    function layer: CALayer; cdecl;
    procedure setLayer(layer: CALayer); cdecl;
  end;
  TCARemoteLayerClient = class(TOCGenericImport<CARemoteLayerClientClass, CARemoteLayerClient>) end;

  CARemoteLayerServerClass = interface(NSObjectClass)
    ['{9F1F343A-2FB8-4FC0-A545-EFA6F9D89260}']
    {class} function sharedServer: CARemoteLayerServer; cdecl;
  end;

  CARemoteLayerServer = interface(NSObject)
    ['{4FAC870F-F950-48C7-A2D2-7F8CDC780089}']
    function serverPort: mach_port_t; cdecl;
  end;
  TCARemoteLayerServer = class(TOCGenericImport<CARemoteLayerServerClass, CARemoteLayerServer>) end;

  CARendererClass = interface(NSObjectClass)
    ['{D567DFD0-286A-4E19-BBA2-D70B2D384F0D}']
    {class} function rendererWithCGLContext(ctx: Pointer; options: NSDictionary): CARenderer; cdecl; // API_DEPRECATED("+rendererWithMTLTexture", macos(10.5, 10.14))
    {class} function rendererWithMTLTexture(tex: Pointer; options: NSDictionary): CARenderer; cdecl;
  end;

  CARenderer = interface(NSObject)
    ['{A9453D74-C613-4F89-BF96-834F231A6A73}']
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
    ['{A9D7B0D1-12FC-40A1-90D1-9DDF71ACF9C6}']
  end;

  CAReplicatorLayer = interface(CALayer)
    ['{606A3844-045C-4413-9E4B-65DCDC991CA4}']
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
    ['{C15B4E3D-79BE-4AAB-AFB5-5AEA3F97B766}']
  end;

  CAScrollLayer = interface(CALayer)
    ['{D9D75ECF-853F-4432-9555-680132174159}']
    function scrollMode: CAScrollLayerScrollMode; cdecl;
    procedure scrollToPoint(p: CGPoint); cdecl;
    procedure scrollToRect(r: CGRect); cdecl;
    procedure setScrollMode(scrollMode: CAScrollLayerScrollMode); cdecl;
  end;
  TCAScrollLayer = class(TOCGenericImport<CAScrollLayerClass, CAScrollLayer>) end;

  CAShapeLayerClass = interface(CALayerClass)
    ['{DEA000FC-814C-4C97-BC24-86D306264FFF}']
  end;

  CAShapeLayer = interface(CALayer)
    ['{E27CB56C-59FB-4F9F-A1B7-432786C01676}']
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
    ['{822F94D6-D29C-4C4A-9A7E-939E0BC75487}']
  end;

  CATextLayer = interface(CALayer)
    ['{CD469DEF-8599-4B26-BB10-78E20FC7F9F5}']
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
    ['{468CF5B5-8BE6-4CF4-A8ED-4ADD561DDEBA}']
    {class} function fadeDuration: CFTimeInterval; cdecl;
  end;

  CATiledLayer = interface(CALayer)
    ['{6D9699B7-FFA7-4E0B-940F-D8A85EF5C26F}']
    function levelsOfDetail: NativeUInt; cdecl;
    function levelsOfDetailBias: NativeUInt; cdecl;
    procedure setLevelsOfDetail(levelsOfDetail: NativeUInt); cdecl;
    procedure setLevelsOfDetailBias(levelsOfDetailBias: NativeUInt); cdecl;
    procedure setTileSize(tileSize: CGSize); cdecl;
    function tileSize: CGSize; cdecl;
  end;
  TCATiledLayer = class(TOCGenericImport<CATiledLayerClass, CATiledLayer>) end;

  CATransactionClass = interface(NSObjectClass)
    ['{C147A822-8EF6-4756-A27F-B3F892597287}']
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
    ['{7F303520-CD0A-454B-AD2F-873B211BCF48}']
  end;
  TCATransaction = class(TOCGenericImport<CATransactionClass, CATransaction>) end;

  CATransformLayerClass = interface(CALayerClass)
    ['{7AB20677-F86D-4EC8-887D-3D578419DDF8}']
  end;

  CATransformLayer = interface(CALayer)
    ['{DBAE8800-4A46-4279-8900-F39CDF30DDFB}']
  end;
  TCATransformLayer = class(TOCGenericImport<CATransformLayerClass, CATransformLayer>) end;

  CAValueFunctionClass = interface(NSObjectClass)
    ['{F15308AB-48C7-4D45-ABDC-0E49CDBFCE17}']
    {class} function functionWithName(name: CAValueFunctionName): Pointer; cdecl;
  end;

  CAValueFunction = interface(NSObject)
    ['{AE75B2D6-F232-4944-88CD-45F146ACF802}']
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
  System.SysUtils;

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
  QuartzCoreModule := LoadLibrary(libQuartzCore);

finalization
  if QuartzCoreModule <> 0 then
    FreeLibrary(QuartzCoreModule);

end.