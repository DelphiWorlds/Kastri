unit DW.iOSapi.SceneKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.QuartzCore, iOSapi.CoreGraphics, iOSapi.AVFoundation, iOSapi.OpenGLES, iOSapi.GLKit,
  // DW
  DW.Macapi.Simd, DW.iOSapi.Metal, DW.iOSapi.AVFoundation, DW.iOSapi.SpriteKit, DW.iOSapi.JavaScriptCore;

const
  SCN_ENABLE_METAL = 1;
  SCN_ENABLE_OPENGL = 1;
  SCNColor = UIColor;
  SCNActionTimingModeLinear = 0;
  SCNActionTimingModeEaseIn = 1;
  SCNActionTimingModeEaseOut = 2;
  SCNActionTimingModeEaseInEaseOut = 3;
  SCNColorMaskNone = 0;
  SCNColorMaskRed = 8;
  SCNColorMaskGreen = 4;
  SCNColorMaskBlue = 2;
  SCNColorMaskAlpha = 1;
  SCNColorMaskAll = 15;
  SCNProgramCompilationError = 1;
  SCNMovabilityHintFixed = 0;
  SCNMovabilityHintMovable = 1;
  SCNNodeFocusBehaviorNone = 0;
  SCNNodeFocusBehaviorOccluding = 1;
  SCNNodeFocusBehaviorFocusable = 2;
  SCNConsistencyInvalidURIError = 1000;
  SCNConsistencyInvalidCountError = 1001;
  SCNConsistencyInvalidArgumentError = 1002;
  SCNConsistencyMissingElementError = 1003;
  SCNConsistencyMissingAttributeError = 1004;
  SCNConsistencyXMLSchemaValidationError = 1005;
  SCNSceneSourceStatusError = -1;
  SCNSceneSourceStatusParsing = 4;
  SCNSceneSourceStatusValidating = 8;
  SCNSceneSourceStatusProcessing = 12;
  SCNSceneSourceStatusComplete = 16;
  SCNFilterModeNone = 0;
  SCNFilterModeNearest = 1;
  SCNFilterModeLinear = 2;
  SCNWrapModeClamp = 1;
  SCNWrapModeRepeat = 2;
  SCNWrapModeClampToBorder = 3;
  SCNWrapModeMirror = 4;
  SCNBufferFrequencyPerFrame = 0;
  SCNBufferFrequencyPerNode = 1;
  SCNBufferFrequencyPerShadable = 2;
  SCNShadowModeForward = 0;
  SCNShadowModeDeferred = 1;
  SCNShadowModeModulated = 2;
  SCNLightProbeTypeIrradiance = 0;
  SCNLightProbeTypeRadiance = 1;
  SCNLightProbeUpdateTypeNever = 0;
  SCNLightProbeUpdateTypeRealtime = 1;
  SCNLightAreaTypeRectangle = 1;
  SCNLightAreaTypePolygon = 4;
  SCNCameraProjectionDirectionVertical = 0;
  SCNCameraProjectionDirectionHorizontal = 1;
  SCNGeometryPrimitiveTypeTriangles = 0;
  SCNGeometryPrimitiveTypeTriangleStrip = 1;
  SCNGeometryPrimitiveTypeLine = 2;
  SCNGeometryPrimitiveTypePoint = 3;
  SCNGeometryPrimitiveTypePolygon = 4;
  SCNTessellationSmoothingModeNone = 0;
  SCNTessellationSmoothingModePNTriangles = 1;
  SCNTessellationSmoothingModePhong = 2;
  SCNFillModeFill = 0;
  SCNFillModeLines = 1;
  SCNCullModeBack = 0;
  SCNCullModeFront = 1;
  SCNCullBack = SCNCullModeBack;
  SCNCullFront = SCNCullModeFront;
  SCNTransparencyModeAOne = 0;
  SCNTransparencyModeRGBZero = 1;
  SCNTransparencyModeSingleLayer = 2;
  SCNTransparencyModeDualLayer = 3;
  SCNTransparencyModeDefault = SCNTransparencyModeAOne;
  SCNBlendModeAlpha = 0;
  SCNBlendModeAdd = 1;
  SCNBlendModeSubtract = 2;
  SCNBlendModeMultiply = 3;
  SCNBlendModeScreen = 4;
  SCNBlendModeReplace = 5;
  SCNBlendModeMax = 6;
  SCNHitTestSearchModeClosest = 0;
  SCNHitTestSearchModeAll = 1;
  SCNHitTestSearchModeAny = 2;
  SCNAntialiasingModeNone = 0;
  SCNAntialiasingModeMultisampling2X = 1;
  SCNAntialiasingModeMultisampling4X = 2;
  SCNRenderingAPIMetal = 0;
  SCNRenderingAPIOpenGLES2 = 1;
  SCNDebugOptionNone = 0;
  SCNDebugOptionShowPhysicsShapes = 1;
  SCNDebugOptionShowBoundingBoxes = 2;
  SCNDebugOptionShowLightInfluences = 4;
  SCNDebugOptionShowLightExtents = 8;
  SCNDebugOptionShowPhysicsFields = 16;
  SCNDebugOptionShowWireframe = 32;
  SCNDebugOptionRenderAsWireframe = 64;
  SCNDebugOptionShowSkeletons = 128;
  SCNDebugOptionShowCreases = 256;
  SCNDebugOptionShowConstraints = 512;
  SCNDebugOptionShowCameras = 1024;
  SCNChamferModeBoth = 0;
  SCNChamferModeFront = 1;
  SCNChamferModeBack = 2;
  SCNMorpherCalculationModeNormalized = 0;
  SCNMorpherCalculationModeAdditive = 1;
  SCNBillboardAxisX = 1;
  SCNBillboardAxisY = 2;
  SCNBillboardAxisZ = 4;
  SCNBillboardAxisAll = 7;
  SCNParticleSortingModeNone = 0;
  SCNParticleSortingModeProjectedDepth = 1;
  SCNParticleSortingModeDistance = 2;
  SCNParticleSortingModeOldestFirst = 3;
  SCNParticleSortingModeYoungestFirst = 4;
  SCNParticleBlendModeAdditive = 0;
  SCNParticleBlendModeSubtract = 1;
  SCNParticleBlendModeMultiply = 2;
  SCNParticleBlendModeScreen = 3;
  SCNParticleBlendModeAlpha = 4;
  SCNParticleBlendModeReplace = 5;
  SCNParticleOrientationModeBillboardScreenAligned = 0;
  SCNParticleOrientationModeBillboardViewAligned = 1;
  SCNParticleOrientationModeFree = 2;
  SCNParticleOrientationModeBillboardYAligned = 3;
  SCNParticleBirthLocationSurface = 0;
  SCNParticleBirthLocationVolume = 1;
  SCNParticleBirthLocationVertex = 2;
  SCNParticleBirthDirectionConstant = 0;
  SCNParticleBirthDirectionSurfaceNormal = 1;
  SCNParticleBirthDirectionRandom = 2;
  SCNParticleImageSequenceAnimationModeRepeat = 0;
  SCNParticleImageSequenceAnimationModeClamp = 1;
  SCNParticleImageSequenceAnimationModeAutoReverse = 2;
  SCNParticleInputModeOverLife = 0;
  SCNParticleInputModeOverDistance = 1;
  SCNParticleInputModeOverOtherProperty = 2;
  SCNParticleModifierStagePreDynamics = 0;
  SCNParticleModifierStagePostDynamics = 1;
  SCNParticleModifierStagePreCollision = 2;
  SCNParticleModifierStagePostCollision = 3;
  SCNParticleEventBirth = 0;
  SCNParticleEventDeath = 1;
  SCNParticleEventCollision = 2;
  SCNPhysicsBodyTypeStatic = 0;
  SCNPhysicsBodyTypeDynamic = 1;
  SCNPhysicsBodyTypeKinematic = 2;
  SCNPhysicsCollisionCategoryDefault = 1;
  SCNPhysicsCollisionCategoryStatic = 2;
  SCNPhysicsCollisionCategoryAll = -1;
  SCNPhysicsFieldScopeInsideExtent = 0;
  SCNPhysicsFieldScopeOutsideExtent = 1;
  SCNReferenceLoadingPolicyImmediate = 0;
  SCNReferenceLoadingPolicyOnDemand = 1;
  SCNInteractionModeFly = 0;
  SCNInteractionModeOrbitTurntable = 1;
  SCNInteractionModeOrbitAngleMapping = 2;
  SCNInteractionModeOrbitCenteredArcball = 3;
  SCNInteractionModeOrbitArcball = 4;
  SCNInteractionModePan = 5;
  SCNInteractionModeTruck = 6;

type
  SCNTimingFunction = interface;
  SCNAnimatable = interface;
  SCNAnimation = interface;
  SCNAnimationPlayer = interface;
  SCNAnimationEvent = interface;
  SCNBoundingVolume = interface;
  SCNActionable = interface;
  SCNAction = interface;
  SCNNode = interface;
  SCNNodeRendererDelegate = interface;
  SCNSceneSource = interface;
  SCNMaterialProperty = interface;
  SCNScene = interface;
  SCNSceneExportDelegate = interface;
  SCNBufferStream = interface;
  SCNShadable = interface;
  SCNProgram = interface;
  SCNProgramDelegate = interface;
  SCNTechnique = interface;
  SCNTechniqueSupport = interface;
  SCNLight = interface;
  SCNCamera = interface;
  SCNGeometry = interface;
  SCNGeometrySource = interface;
  SCNGeometryElement = interface;
  SCNGeometryTessellator = interface;
  SCNMaterial = interface;
  SCNHitTestResult = interface;
  SCNSceneRenderer = interface;
  SCNSceneRendererDelegate = interface;
  SCNCameraControlConfiguration = interface;
  SCNView = interface;
  SCNRenderer = interface;
  SCNPlane = interface;
  SCNBox = interface;
  SCNPyramid = interface;
  SCNSphere = interface;
  SCNCylinder = interface;
  SCNCone = interface;
  SCNTube = interface;
  SCNCapsule = interface;
  SCNTorus = interface;
  SCNFloor = interface;
  SCNText = interface;
  SCNShape = interface;
  SCNTransaction = interface;
  SCNMorpher = interface;
  SCNSkinner = interface;
  SCNConstraint = interface;
  SCNLookAtConstraint = interface;
  SCNBillboardConstraint = interface;
  SCNTransformConstraint = interface;
  SCNIKConstraint = interface;
  SCNDistanceConstraint = interface;
  SCNReplicatorConstraint = interface;
  SCNAccelerationConstraint = interface;
  SCNSliderConstraint = interface;
  SCNAvoidOccluderConstraintDelegate = interface;
  SCNAvoidOccluderConstraint = interface;
  SCNLevelOfDetail = interface;
  SCNParticlePropertyController = interface;
  SCNParticleSystem = interface;
  SCNPhysicsBody = interface;
  SCNPhysicsField = interface;
  SCNPhysicsShape = interface;
  SCNPhysicsContactDelegate = interface;
  SCNPhysicsWorld = interface;
  SCNPhysicsContact = interface;
  SCNPhysicsBehavior = interface;
  SCNPhysicsHingeJoint = interface;
  SCNPhysicsBallSocketJoint = interface;
  SCNPhysicsSliderJoint = interface;
  SCNPhysicsConeTwistJoint = interface;
  SCNPhysicsVehicleWheel = interface;
  SCNPhysicsVehicle = interface;
  SCNReferenceNode = interface;
  SCNAudioSource = interface;
  SCNAudioPlayer = interface;
  SCNCameraControllerDelegate = interface;
  SCNCameraController = interface;

  PNativeUInt = ^NativeUInt;
  PBoolean = ^Boolean;
  PUInt32 = ^UInt32;
  PSCNVector3 = ^SCNVector3;
  PSCNVector4 = ^SCNVector4;
  PSCNMatrix4 = ^SCNMatrix4;

  SCNActionTimingMode = NSInteger;
  SCNColorMask = NSInteger;

  SCNVector3 = record
    x: Single;
    y: Single;
    z: Single;
  end;

  SCNVector4 = record
    x: Single;
    y: Single;
    z: Single;
    w: Single;
  end;

  SCNQuaternion = SCNVector4;

  SCNMatrix4 = record
    m11: Single;
    m12: Single;
    m13: Single;
    m14: Single;
    m21: Single;
    m22: Single;
    m23: Single;
    m24: Single;
    m31: Single;
    m32: Single;
    m33: Single;
    m34: Single;
    m41: Single;
    m42: Single;
    m43: Single;
    m44: Single;
  end;

  SCNAnimationDidStartBlock = procedure(animation: SCNAnimation; receiver: Pointer) of object;

  SCNAnimationDidStopBlock = procedure(animation: SCNAnimation; receiver: Pointer; completed: Boolean) of object;

  SCNAnimationEventBlock = procedure(animation: Pointer; animatedObject: Pointer; playingBackward: Boolean) of object;

  SCNActionTimingFunction = function(time: Single): Single of object;
  SCNMovabilityHint = NSInteger;
  SCNNodeFocusBehavior = NSInteger;
  SCNSceneSourceLoadingOption = NSString;
  SCNSceneSourceAnimationImportPolicy = NSString;
  SCNSceneSourceStatus = NSInteger;

  SCNSceneSourceStatusHandler = procedure(totalProgress: Single; status: SCNSceneSourceStatus; error: NSError; stop: PBoolean) of object;
  SCNFilterMode = NSInteger;
  SCNWrapMode = NSInteger;

  SCNSceneExportProgressHandler = procedure(totalProgress: Single; error: NSError; stop: PBoolean) of object;
  SCNSceneAttribute = NSString;
  SCNShaderModifierEntryPoint = NSString;
  SCNBufferFrequency = NSInteger;

  SCNBufferBindingBlock = procedure(buffer: Pointer; node: SCNNode; shadable: Pointer; renderer: SCNRenderer) of object;

  SCNBindingBlock = procedure(programID: Cardinal; location: Cardinal; renderedNode: SCNNode; renderer: SCNRenderer) of object;
  SCNLightType = NSString;
  SCNShadowMode = NSInteger;
  SCNLightProbeType = NSInteger;
  SCNLightProbeUpdateType = NSInteger;
  SCNLightAreaType = NSInteger;
  SCNCameraProjectionDirection = NSInteger;
  SCNGeometryPrimitiveType = NSInteger;
  SCNGeometrySourceSemantic = NSString;
  SCNTessellationSmoothingMode = NSInteger;
  SCNLightingModel = NSString;
  SCNFillMode = NSInteger;
  SCNCullMode = NSInteger;
  SCNTransparencyMode = NSInteger;
  SCNBlendMode = NSInteger;
  SCNHitTestSearchMode = NSInteger;
  SCNHitTestOption = NSString;
  SCNAntialiasingMode = NSInteger;
  SCNRenderingAPI = NSInteger;
  SCNDebugOptions = NSInteger;
  SCNViewOption = NSString;
  SCNChamferMode = NSInteger;
  SCNMorpherCalculationMode = NSInteger;
  SCNBillboardAxis = NSInteger;
  SCNParticleProperty = NSString;

  SCNParticleEventBlock = procedure(data: PPointer; dataStride: PNativeUInt; indices: PUInt32; count: NSInteger) of object;

  SCNParticleModifierBlock = procedure(data: PPointer; dataStride: PNativeUInt; start: NSInteger; &end: NSInteger; deltaTime: Single) of object;
  SCNParticleSortingMode = NSInteger;
  SCNParticleBlendMode = NSInteger;
  SCNParticleOrientationMode = NSInteger;
  SCNParticleBirthLocation = NSInteger;
  SCNParticleBirthDirection = NSInteger;
  SCNParticleImageSequenceAnimationMode = NSInteger;
  SCNParticleInputMode = NSInteger;
  SCNParticleModifierStage = NSInteger;
  SCNParticleEvent = NSInteger;
  SCNPhysicsBodyType = NSInteger;
  SCNPhysicsCollisionCategory = NSInteger;
  SCNPhysicsFieldScope = NSInteger;

  SCNFieldForceEvaluator = function(position: SCNVector3; velocity: SCNVector3; mass: Single; charge: Single; time: NSTimeInterval): SCNVector3 of object;
  SCNPhysicsShapeOption = NSString;
  SCNPhysicsShapeType = NSString;
  SCNPhysicsTestOption = NSString;
  SCNPhysicsTestSearchMode = NSString;
  SCNReferenceLoadingPolicy = NSInteger;
  SCNInteractionMode = NSInteger;
  TSCNActionableBlockMethod1 = procedure of object;
  TSCNActionBlockMethod1 = procedure(node: SCNNode) of object;
  TSCNActionBlockMethod2 = procedure(node: SCNNode; elapsedTime: CGFloat) of object;
  TSCNNodeBlockMethod1 = procedure(child: SCNNode; stop: PBoolean) of object;
  TSCNNodeBlockMethod2 = procedure(node: SCNNode; stop: PBoolean) of object;
  TSCNSceneSourceBlockMethod1 = procedure(entry: Pointer; identifier: NSString; stop: PBoolean) of object;
  TSCNSceneRendererBlockMethod1 = procedure of object;
  TSCNSceneRendererBlockMethod2 = procedure(success: Boolean) of object;
  TSCNTransactionBlockMethod1 = procedure of object;
  TSCNTransformConstraintBlockMethod1 = procedure(node: SCNNode; transform: SCNMatrix4) of object;
  TSCNTransformConstraintBlockMethod2 = procedure(node: SCNNode; position: SCNVector3) of object;
  TSCNTransformConstraintBlockMethod3 = procedure(node: SCNNode; quaternion: SCNQuaternion) of object;
  TSCNAudioPlayerBlockMethod1 = procedure of object;
  TSCNAudioPlayerBlockMethod2 = procedure of object;

  SCNTimingFunctionClass = interface(NSObjectClass)
    ['{23DB5DA0-97B7-4796-B431-21EB102F3118}']
    {class} function functionWithCAMediaTimingFunction(caTimingFunction: CAMediaTimingFunction): SCNTimingFunction; cdecl;
    {class} function functionWithTimingMode(timingMode: SCNActionTimingMode): SCNTimingFunction; cdecl;
  end;

  SCNTimingFunction = interface(NSObject)
    ['{C5D45B52-174E-459E-B6D8-DF88B290F65D}']
  end;
  TSCNTimingFunction = class(TOCGenericImport<SCNTimingFunctionClass, SCNTimingFunction>) end;

  SCNAnimatable = interface(IObjectiveC)
    ['{16D01AA2-D75A-4841-8BD3-21F154C052D8}']
    procedure addAnimation(animation: Pointer; forKey: NSString); cdecl;
    procedure addAnimationPlayer(player: SCNAnimationPlayer; forKey: NSString); cdecl;
    function animationForKey(key: NSString): CAAnimation; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-animationPlayerForKey:", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0))
    function animationKeys: NSArray; cdecl;
    function animationPlayerForKey(key: NSString): SCNAnimationPlayer; cdecl;
    function isAnimationForKeyPaused(key: NSString): Boolean; cdecl; // API_DEPRECATED("Use -[SCNAnimationPlayer paused] instead", macos(10.9, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure pauseAnimationForKey(key: NSString); cdecl; // API_DEPRECATED("Use -[SCNAnimationPlayer setPaused:] instead", macos(10.9, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure removeAllAnimations; cdecl;
    procedure removeAnimationForKey(key: NSString); cdecl;
    [MethodName('removeAnimationForKey:blendOutDuration:')]
    procedure removeAnimationForKeyBlendOutDuration(key: NSString; blendOutDuration: CGFloat); cdecl;
    [MethodName('removeAnimationForKey:fadeOutDuration:')]
    procedure removeAnimationForKeyFadeOutDuration(key: NSString; fadeOutDuration: CGFloat); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-removeAnimationForKey:blendOutDuration:", macos(10.10, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure resumeAnimationForKey(key: NSString); cdecl; // API_DEPRECATED("Use -[SCNAnimationPlayer setPaused:] instead", macos(10.9, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setSpeed(speed: CGFloat; forAnimationKey: NSString); cdecl; // API_DEPRECATED("Use -[SCNAnimationPlayer setSpeed:] instead", macos(10.12, 10.13), ios(10.0, 11.0), tvos(10.0, 11.0), watchos(3.0, 4.0))
  end;

  SCNAnimationClass = interface(NSObjectClass)
    ['{46E08FD3-012C-4961-94BE-6CB9D13E9BF3}']
    {class} function animationNamed(animationName: NSString): SCNAnimation; cdecl;
    {class} function animationWithCAAnimation(caAnimation: CAAnimation): SCNAnimation; cdecl;
    {class} function animationWithContentsOfURL(animationUrl: NSURL): SCNAnimation; cdecl;
  end;

  SCNAnimation = interface(NSObject)
    ['{F3F7E56E-2D6A-48DB-A57B-E0237D1268D3}']
    function animationDidStart: SCNAnimationDidStartBlock; cdecl;
    function animationDidStop: SCNAnimationDidStopBlock; cdecl;
    function animationEvents: NSArray; cdecl;
    function autoreverses: Boolean; cdecl;
    function blendInDuration: NSTimeInterval; cdecl;
    function blendOutDuration: NSTimeInterval; cdecl;
    function duration: NSTimeInterval; cdecl;
    function fillsBackward: Boolean; cdecl;
    function fillsForward: Boolean; cdecl;
    function isAdditive: Boolean; cdecl;
    function isAppliedOnCompletion: Boolean; cdecl;
    function isCumulative: Boolean; cdecl;
    function isRemovedOnCompletion: Boolean; cdecl;
    function keyPath: NSString; cdecl;
    function repeatCount: CGFloat; cdecl;
    procedure setAdditive(additive: Boolean); cdecl;
    procedure setAnimationDidStart(animationDidStart: SCNAnimationDidStartBlock); cdecl;
    procedure setAnimationDidStop(animationDidStop: SCNAnimationDidStopBlock); cdecl;
    procedure setAnimationEvents(animationEvents: NSArray); cdecl;
    procedure setAppliedOnCompletion(appliedOnCompletion: Boolean); cdecl;
    procedure setAutoreverses(autoreverses: Boolean); cdecl;
    procedure setBlendInDuration(blendInDuration: NSTimeInterval); cdecl;
    procedure setBlendOutDuration(blendOutDuration: NSTimeInterval); cdecl;
    procedure setCumulative(cumulative: Boolean); cdecl;
    procedure setDuration(duration: NSTimeInterval); cdecl;
    procedure setFillsBackward(fillsBackward: Boolean); cdecl;
    procedure setFillsForward(fillsForward: Boolean); cdecl;
    procedure setKeyPath(keyPath: NSString); cdecl;
    procedure setRemovedOnCompletion(removedOnCompletion: Boolean); cdecl;
    procedure setRepeatCount(repeatCount: CGFloat); cdecl;
    procedure setStartDelay(startDelay: NSTimeInterval); cdecl;
    procedure setTimeOffset(timeOffset: NSTimeInterval); cdecl;
    procedure setTimingFunction(timingFunction: SCNTimingFunction); cdecl;
    procedure setUsesSceneTimeBase(usesSceneTimeBase: Boolean); cdecl;
    function startDelay: NSTimeInterval; cdecl;
    function timeOffset: NSTimeInterval; cdecl;
    function timingFunction: SCNTimingFunction; cdecl;
    function usesSceneTimeBase: Boolean; cdecl;
  end;
  TSCNAnimation = class(TOCGenericImport<SCNAnimationClass, SCNAnimation>) end;

  SCNAnimationPlayerClass = interface(NSObjectClass)
    ['{4533D55B-1FE7-4B88-8DFD-1D93526D654A}']
    {class} function animationPlayerWithAnimation(animation: SCNAnimation): SCNAnimationPlayer; cdecl;
  end;

  SCNAnimationPlayer = interface(NSObject)
    ['{B55BDE6E-C071-406E-9A34-FAAF2D258A89}']
    function animation: SCNAnimation; cdecl;
    function blendFactor: CGFloat; cdecl;
    function paused: Boolean; cdecl;
    procedure play; cdecl;
    procedure setBlendFactor(blendFactor: CGFloat); cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    procedure setSpeed(speed: CGFloat); cdecl;
    function speed: CGFloat; cdecl;
    procedure stop; cdecl;
    procedure stopWithBlendOutDuration(duration: NSTimeInterval); cdecl;
  end;
  TSCNAnimationPlayer = class(TOCGenericImport<SCNAnimationPlayerClass, SCNAnimationPlayer>) end;

  SCNAnimationEventClass = interface(NSObjectClass)
    ['{CD925E34-6E51-42D5-AF9F-7795146123F7}']
    {class} function animationEventWithKeyTime(time: CGFloat; block: SCNAnimationEventBlock): Pointer; cdecl;
  end;

  SCNAnimationEvent = interface(NSObject)
    ['{A0B015AE-A8F2-459D-AD48-ACAD46BBAF96}']
  end;
  TSCNAnimationEvent = class(TOCGenericImport<SCNAnimationEventClass, SCNAnimationEvent>) end;

  SCNBoundingVolume = interface(IObjectiveC)
    ['{25FE5B04-B52F-42D5-ACB8-8AE1A9BE459F}']
    function getBoundingBoxMin(min: PSCNVector3; max: PSCNVector3): Boolean; cdecl;
    function getBoundingSphereCenter(center: PSCNVector3; radius: PCGFloat): Boolean; cdecl;
    procedure setBoundingBoxMin(min: PSCNVector3; max: PSCNVector3); cdecl;
  end;

  SCNActionable = interface(IObjectiveC)
    ['{FA7D44D7-72C8-46C0-9A2C-621FAD81FE8B}']
    function actionForKey(key: NSString): SCNAction; cdecl;
    function actionKeys: NSArray; cdecl;
    function hasActions: Boolean; cdecl;
    procedure removeActionForKey(key: NSString); cdecl;
    procedure removeAllActions; cdecl;
    procedure runAction(action: SCNAction; completionHandler: TSCNActionableBlockMethod1); overload; cdecl;
    procedure runAction(action: SCNAction; forKey: NSString); overload; cdecl;
    procedure runAction(action: SCNAction; forKey: NSString; completionHandler: TSCNActionableBlockMethod1); overload; cdecl;
    procedure runAction(action: SCNAction); overload; cdecl;
  end;

  SCNActionClass = interface(NSObjectClass)
    ['{1DA5FE6B-B08F-48F0-BF39-61A67502B9D7}']
    {class} function customActionWithDuration(seconds: NSTimeInterval; actionBlock: TSCNActionBlockMethod2): SCNAction; cdecl;
    {class} function fadeInWithDuration(sec: NSTimeInterval): SCNAction; cdecl;
    {class} function fadeOpacityBy(factor: CGFloat; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function fadeOpacityTo(opacity: CGFloat; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function fadeOutWithDuration(sec: NSTimeInterval): SCNAction; cdecl;
    {class} function group(actions: NSArray): SCNAction; cdecl;
    {class} function hide: SCNAction; cdecl;
    {class} function javaScriptActionWithScript(script: NSString; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function moveBy(delta: SCNVector3; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function moveByX(deltaX: CGFloat; y: CGFloat; z: CGFloat; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function moveTo(location: SCNVector3; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function playAudioSource(source: SCNAudioSource; waitForCompletion: Boolean): SCNAction; cdecl;
    {class} function removeFromParentNode: SCNAction; cdecl;
    {class} function repeatAction(action: SCNAction; count: NSUInteger): SCNAction; cdecl;
    {class} function repeatActionForever(action: SCNAction): SCNAction; cdecl;
    {class} function rotateByAngle(angle: CGFloat; aroundAxis: SCNVector3; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function rotateByX(xAngle: CGFloat; y: CGFloat; z: CGFloat; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function rotateToAxisAngle(axisAngle: SCNVector4; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function rotateToX(xAngle: CGFloat; y: CGFloat; z: CGFloat; duration: NSTimeInterval): SCNAction; overload; cdecl;
    {class} function rotateToX(xAngle: CGFloat; y: CGFloat; z: CGFloat; duration: NSTimeInterval;
      shortestUnitArc: Boolean): SCNAction; overload; cdecl;
    {class} function runBlock(block: TSCNActionBlockMethod1; queue: dispatch_queue_t): SCNAction; overload; cdecl;
    {class} function runBlock(block: TSCNActionBlockMethod1): SCNAction; overload; cdecl;
    {class} function scaleBy(scale: CGFloat; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function scaleTo(scale: CGFloat; duration: NSTimeInterval): SCNAction; cdecl;
    {class} function sequence(actions: NSArray): SCNAction; cdecl;
    {class} function unhide: SCNAction; cdecl;
    {class} function waitForDuration(sec: NSTimeInterval; withRange: NSTimeInterval): SCNAction; overload; cdecl;
    {class} function waitForDuration(sec: NSTimeInterval): SCNAction; overload; cdecl;
  end;

  SCNAction = interface(NSObject)
    ['{55F249AD-65E2-45BA-9795-94EA9F6F61DC}']
    function duration: NSTimeInterval; cdecl;
    function reversedAction: SCNAction; cdecl;
    procedure setDuration(duration: NSTimeInterval); cdecl;
    procedure setSpeed(speed: CGFloat); cdecl;
    procedure setTimingFunction(timingFunction: SCNActionTimingFunction); cdecl;
    procedure setTimingMode(timingMode: SCNActionTimingMode); cdecl;
    function speed: CGFloat; cdecl;
    function timingFunction: SCNActionTimingFunction; cdecl;
    function timingMode: SCNActionTimingMode; cdecl;
  end;
  TSCNAction = class(TOCGenericImport<SCNActionClass, SCNAction>) end;

  SCNNodeClass = interface(NSObjectClass)
    ['{B15C1158-20BA-476A-BC7A-D2FC78D5161A}']
    {class} function localFront: SCNVector3; cdecl;
    {class} function localRight: SCNVector3; cdecl;
    {class} function localUp: SCNVector3; cdecl;
    {class} function node: Pointer; cdecl;
    {class} function nodeWithGeometry(geometry: SCNGeometry): SCNNode; cdecl;
    {class} function nodeWithMDLObject(mdlObject: Pointer): Pointer; cdecl;
    {class} function simdLocalFront: simd_float3; cdecl;
    {class} function simdLocalRight: simd_float3; cdecl;
    {class} function simdLocalUp: simd_float3; cdecl;
  end;

  SCNNode = interface(NSObject)
    ['{F1F91C69-BA27-44DB-98FC-4B25585513ED}']
    procedure addAudioPlayer(player: SCNAudioPlayer); cdecl;
    procedure addChildNode(child: SCNNode); cdecl;
    procedure addParticleSystem(system: SCNParticleSystem); cdecl;
    function audioPlayers: NSArray; cdecl;
    function camera: SCNCamera; cdecl;
    function castsShadow: Boolean; cdecl;
    function categoryBitMask: NSUInteger; cdecl;
    function childNodes: NSArray; cdecl;
    function childNodesPassingTest(predicate: TSCNNodeBlockMethod1): NSArray; cdecl;
    function childNodeWithName(name: NSString; recursively: Boolean): SCNNode; cdecl;
    function clone: Pointer; cdecl;
    function constraints: NSArray; cdecl;
    [MethodName('convertPosition:fromNode:')]
    function convertPositionFromNode(position: SCNVector3; fromNode: SCNNode): SCNVector3; cdecl;
    [MethodName('convertPosition:toNode:')]
    function convertPositionToNode(position: SCNVector3; toNode: SCNNode): SCNVector3; cdecl;
    [MethodName('convertTransform:fromNode:')]
    function convertTransformFromNode(transform: SCNMatrix4; fromNode: SCNNode): SCNMatrix4; cdecl;
    [MethodName('convertTransform:toNode:')]
    function convertTransformToNode(transform: SCNMatrix4; toNode: SCNNode): SCNMatrix4; cdecl;
    [MethodName('convertVector:fromNode:')]
    function convertVectorFromNode(vector: SCNVector3; fromNode: SCNNode): SCNVector3; cdecl;
    [MethodName('convertVector:toNode:')]
    function convertVectorToNode(vector: SCNVector3; toNode: SCNNode): SCNVector3; cdecl;
    procedure enumerateChildNodesUsingBlock(block: TSCNNodeBlockMethod1); cdecl;
    procedure enumerateHierarchyUsingBlock(block: TSCNNodeBlockMethod2); cdecl;
    function eulerAngles: SCNVector3; cdecl;
    function filters: NSArray; cdecl;
    function flattenedClone: Pointer; cdecl;
    function focusBehavior: SCNNodeFocusBehavior; cdecl;
    function geometry: SCNGeometry; cdecl;
    function hitTestWithSegmentFromPoint(pointA: SCNVector3; toPoint: SCNVector3; options: NSDictionary): NSArray; cdecl;
    procedure insertChildNode(child: SCNNode; atIndex: NSUInteger); cdecl;
    function isHidden: Boolean; cdecl;
    function isPaused: Boolean; cdecl;
    function light: SCNLight; cdecl;
    procedure localRotateBy(rotation: SCNQuaternion); cdecl;
    procedure localTranslateBy(translation: SCNVector3); cdecl;
    procedure lookAt(worldTarget: SCNVector3; up: SCNVector3; localFront: SCNVector3); overload; cdecl;
    procedure lookAt(worldTarget: SCNVector3); overload; cdecl;
    function morpher: SCNMorpher; cdecl;
    function movabilityHint: SCNMovabilityHint; cdecl;
    function name: NSString; cdecl;
    function opacity: CGFloat; cdecl;
    function orientation: SCNQuaternion; cdecl;
    function parentNode: SCNNode; cdecl;
    function particleSystems: NSArray; cdecl;
    function physicsBody: SCNPhysicsBody; cdecl;
    function physicsField: SCNPhysicsField; cdecl;
    function pivot: SCNMatrix4; cdecl;
    function position: SCNVector3; cdecl;
    function presentationNode: SCNNode; cdecl;
    procedure removeAllAudioPlayers; cdecl;
    procedure removeAllParticleSystems; cdecl;
    procedure removeAudioPlayer(player: SCNAudioPlayer); cdecl;
    procedure removeFromParentNode; cdecl;
    procedure removeParticleSystem(system: SCNParticleSystem); cdecl;
    function rendererDelegate: Pointer; cdecl;
    function renderingOrder: NSInteger; cdecl;
    procedure replaceChildNode(oldChild: SCNNode; &with: SCNNode); cdecl;
    procedure rotateBy(worldRotation: SCNQuaternion; aroundTarget: SCNVector3); cdecl;
    function rotation: SCNVector4; cdecl;
    function scale: SCNVector3; cdecl;
    procedure setCamera(camera: SCNCamera); cdecl;
    procedure setCastsShadow(castsShadow: Boolean); cdecl;
    procedure setCategoryBitMask(categoryBitMask: NSUInteger); cdecl;
    procedure setConstraints(constraints: NSArray); cdecl;
    procedure setEulerAngles(eulerAngles: SCNVector3); cdecl;
    procedure setFilters(filters: NSArray); cdecl;
    procedure setFocusBehavior(focusBehavior: SCNNodeFocusBehavior); cdecl;
    procedure setGeometry(geometry: SCNGeometry); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setLight(light: SCNLight); cdecl;
    procedure setMorpher(morpher: SCNMorpher); cdecl;
    procedure setMovabilityHint(movabilityHint: SCNMovabilityHint); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setOpacity(opacity: CGFloat); cdecl;
    procedure setOrientation(orientation: SCNQuaternion); cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    procedure setPhysicsBody(physicsBody: SCNPhysicsBody); cdecl;
    procedure setPhysicsField(physicsField: SCNPhysicsField); cdecl;
    procedure setPivot(pivot: SCNMatrix4); cdecl;
    procedure setPosition(position: SCNVector3); cdecl;
    procedure setRendererDelegate(rendererDelegate: Pointer); cdecl;
    procedure setRenderingOrder(renderingOrder: NSInteger); cdecl;
    procedure setRotation(rotation: SCNVector4); cdecl;
    procedure setScale(scale: SCNVector3); cdecl;
    procedure setSimdEulerAngles(simdEulerAngles: simd_float3); cdecl;
    procedure setSimdOrientation(simdOrientation: simd_quatf); cdecl;
    procedure setSimdPivot(simdPivot: simd_float4x4); cdecl;
    procedure setSimdPosition(simdPosition: simd_float3); cdecl;
    procedure setSimdRotation(simdRotation: simd_float4); cdecl;
    procedure setSimdScale(simdScale: simd_float3); cdecl;
    procedure setSimdTransform(simdTransform: simd_float4x4); cdecl;
    procedure setSimdWorldOrientation(simdWorldOrientation: simd_quatf); cdecl;
    procedure setSimdWorldPosition(simdWorldPosition: simd_float3); cdecl;
    procedure setSimdWorldTransform(simdWorldTransform: simd_float4x4); cdecl;
    procedure setSkinner(skinner: SCNSkinner); cdecl;
    procedure setTransform(transform: SCNMatrix4); cdecl;
    procedure setWorldOrientation(worldOrientation: SCNQuaternion); cdecl;
    procedure setWorldPosition(worldPosition: SCNVector3); cdecl;
    procedure setWorldTransform(worldTransform: SCNMatrix4); cdecl;
    [MethodName('simdConvertPosition:fromNode:')]
    function simdConvertPositionFromNode(position: simd_float3; fromNode: SCNNode): simd_float3; cdecl;
    [MethodName('simdConvertPosition:toNode:')]
    function simdConvertPositionToNode(position: simd_float3; toNode: SCNNode): simd_float3; cdecl;
    [MethodName('simdConvertTransform:fromNode:')]
    function simdConvertTransformFromNode(transform: simd_float4x4; fromNode: SCNNode): simd_float4x4; cdecl;
    [MethodName('simdConvertTransform:toNode:')]
    function simdConvertTransformToNode(transform: simd_float4x4; toNode: SCNNode): simd_float4x4; cdecl;
    [MethodName('simdConvertVector:fromNode:')]
    function simdConvertVectorFromNode(vector: simd_float3; fromNode: SCNNode): simd_float3; cdecl;
    [MethodName('simdConvertVector:toNode:')]
    function simdConvertVectorToNode(vector: simd_float3; toNode: SCNNode): simd_float3; cdecl;
    function simdEulerAngles: simd_float3; cdecl;
    procedure simdLocalRotateBy(rotation: simd_quatf); cdecl;
    procedure simdLocalTranslateBy(translation: simd_float3); cdecl;
    procedure simdLookAt(worldTarget: simd_float3; up: simd_float3; localFront: simd_float3); overload; cdecl;
    procedure simdLookAt(worldTarget: simd_float3); overload; cdecl;
    function simdOrientation: simd_quatf; cdecl;
    function simdPivot: simd_float4x4; cdecl;
    function simdPosition: simd_float3; cdecl;
    procedure simdRotateBy(worldRotation: simd_quatf; aroundTarget: simd_float3); cdecl;
    function simdRotation: simd_float4; cdecl;
    function simdScale: simd_float3; cdecl;
    function simdTransform: simd_float4x4; cdecl;
    function simdWorldFront: simd_float3; cdecl;
    function simdWorldOrientation: simd_quatf; cdecl;
    function simdWorldPosition: simd_float3; cdecl;
    function simdWorldRight: simd_float3; cdecl;
    function simdWorldTransform: simd_float4x4; cdecl;
    function simdWorldUp: simd_float3; cdecl;
    function skinner: SCNSkinner; cdecl;
    function transform: SCNMatrix4; cdecl;
    function worldFront: SCNVector3; cdecl;
    function worldOrientation: SCNQuaternion; cdecl;
    function worldPosition: SCNVector3; cdecl;
    function worldRight: SCNVector3; cdecl;
    function worldTransform: SCNMatrix4; cdecl;
    function worldUp: SCNVector3; cdecl;
  end;
  TSCNNode = class(TOCGenericImport<SCNNodeClass, SCNNode>) end;

  SCNNodeRendererDelegate = interface(IObjectiveC)
    ['{2A775A06-F3B0-4CB3-BDDA-9C6835921514}']
    procedure renderNode(node: SCNNode; renderer: SCNRenderer; arguments: NSDictionary); cdecl;
  end;

  SCNSceneSourceClass = interface(NSObjectClass)
    ['{E4371980-5E30-413B-9974-5D91BD41D058}']
    {class} function sceneSourceWithData(data: NSData; options: NSDictionary): Pointer; cdecl;
    {class} function sceneSourceWithURL(url: NSURL; options: NSDictionary): Pointer; cdecl;
  end;

  SCNSceneSource = interface(NSObject)
    ['{5C249BC6-51E8-4239-B051-A2327A2F8F1A}']
    function data: NSData; cdecl;
    function entriesPassingTest(predicate: TSCNSceneSourceBlockMethod1): NSArray; cdecl;
    function entryWithIdentifier(uid: NSString; withClass: Pointer): Pointer; cdecl;
    function identifiersOfEntriesWithClass(entryClass: Pointer): NSArray; cdecl;
    function initWithData(data: NSData; options: NSDictionary): Pointer; cdecl;
    function initWithURL(url: NSURL; options: NSDictionary): Pointer; cdecl;
    function propertyForKey(key: NSString): Pointer; cdecl;
    function sceneWithOptions(options: NSDictionary; statusHandler: SCNSceneSourceStatusHandler): SCNScene; overload; cdecl;
    function sceneWithOptions(options: NSDictionary; error: PPointer): SCNScene; overload; cdecl;
    function url: NSURL; cdecl;
  end;
  TSCNSceneSource = class(TOCGenericImport<SCNSceneSourceClass, SCNSceneSource>) end;

  SCNMaterialPropertyClass = interface(NSObjectClass)
    ['{C7EDDEF2-308A-4732-91ED-0D10F4998D92}']
    {class} function materialPropertyWithContents(contents: Pointer): Pointer; cdecl;
  end;

  SCNMaterialProperty = interface(NSObject)
    ['{B7437F57-18A7-4270-8EEA-894833FC52D8}']
    function borderColor: Pointer; cdecl; // API_DEPRECATED("Deprecated", macos(10.8, 10.12), ios(8.0, 10.0))
    function contents: Pointer; cdecl;
    function contentsTransform: SCNMatrix4; cdecl;
    function intensity: CGFloat; cdecl;
    function magnificationFilter: SCNFilterMode; cdecl;
    function mappingChannel: NSInteger; cdecl;
    function maxAnisotropy: CGFloat; cdecl;
    function minificationFilter: SCNFilterMode; cdecl;
    function mipFilter: SCNFilterMode; cdecl;
    procedure setBorderColor(borderColor: Pointer); cdecl; // API_DEPRECATED("Deprecated", macos(10.8, 10.12), ios(8.0, 10.0))
    procedure setContents(contents: Pointer); cdecl;
    procedure setContentsTransform(contentsTransform: SCNMatrix4); cdecl;
    procedure setIntensity(intensity: CGFloat); cdecl;
    procedure setMagnificationFilter(magnificationFilter: SCNFilterMode); cdecl;
    procedure setMappingChannel(mappingChannel: NSInteger); cdecl;
    procedure setMaxAnisotropy(maxAnisotropy: CGFloat); cdecl;
    procedure setMinificationFilter(minificationFilter: SCNFilterMode); cdecl;
    procedure setMipFilter(mipFilter: SCNFilterMode); cdecl;
    procedure setTextureComponents(textureComponents: SCNColorMask); cdecl;
    procedure setWrapS(wrapS: SCNWrapMode); cdecl;
    procedure setWrapT(wrapT: SCNWrapMode); cdecl;
    function textureComponents: SCNColorMask; cdecl;
    function wrapS: SCNWrapMode; cdecl;
    function wrapT: SCNWrapMode; cdecl;
  end;
  TSCNMaterialProperty = class(TOCGenericImport<SCNMaterialPropertyClass, SCNMaterialProperty>) end;

  SCNSceneClass = interface(NSObjectClass)
    ['{C8DFA78E-8317-4AF1-8C50-43FC91319946}']
    {class} function scene: Pointer; cdecl;
    {class} function sceneNamed(name: NSString; inDirectory: NSString; options: NSDictionary): Pointer; overload; cdecl;
    {class} function sceneNamed(name: NSString): Pointer; overload; cdecl;
    {class} function sceneWithMDLAsset(mdlAsset: Pointer): Pointer; cdecl;
    {class} function sceneWithURL(url: NSURL; options: NSDictionary; error: PPointer): Pointer; cdecl;
  end;

  SCNScene = interface(NSObject)
    ['{6C36DD67-5681-4431-8D75-DA82954FB96B}']
    procedure addParticleSystem(system: SCNParticleSystem; withTransform: SCNMatrix4); cdecl;
    function attributeForKey(key: NSString): Pointer; cdecl;
    function background: SCNMaterialProperty; cdecl;
    function fogColor: Pointer; cdecl;
    function fogDensityExponent: CGFloat; cdecl;
    function fogEndDistance: CGFloat; cdecl;
    function fogStartDistance: CGFloat; cdecl;
    function isPaused: Boolean; cdecl;
    function lightingEnvironment: SCNMaterialProperty; cdecl;
    function particleSystems: NSArray; cdecl;
    function physicsWorld: SCNPhysicsWorld; cdecl;
    procedure removeAllParticleSystems; cdecl;
    procedure removeParticleSystem(system: SCNParticleSystem); cdecl;
    function rootNode: SCNNode; cdecl;
    function screenSpaceReflectionMaximumDistance: CGFloat; cdecl;
    function screenSpaceReflectionSampleCount: NSInteger; cdecl;
    function screenSpaceReflectionStride: CGFloat; cdecl;
    procedure setAttribute(attribute: Pointer; forKey: NSString); cdecl;
    procedure setFogColor(fogColor: Pointer); cdecl;
    procedure setFogDensityExponent(fogDensityExponent: CGFloat); cdecl;
    procedure setFogEndDistance(fogEndDistance: CGFloat); cdecl;
    procedure setFogStartDistance(fogStartDistance: CGFloat); cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    procedure setScreenSpaceReflectionMaximumDistance(screenSpaceReflectionMaximumDistance: CGFloat); cdecl;
    procedure setScreenSpaceReflectionSampleCount(screenSpaceReflectionSampleCount: NSInteger); cdecl;
    procedure setScreenSpaceReflectionStride(screenSpaceReflectionStride: CGFloat); cdecl;
    procedure setWantsScreenSpaceReflection(wantsScreenSpaceReflection: Boolean); cdecl;
    function wantsScreenSpaceReflection: Boolean; cdecl;
    function writeToURL(url: NSURL; options: NSDictionary; delegate: Pointer; progressHandler: SCNSceneExportProgressHandler): Boolean; cdecl;
  end;
  TSCNScene = class(TOCGenericImport<SCNSceneClass, SCNScene>) end;

  SCNSceneExportDelegate = interface(IObjectiveC)
    ['{C7676498-7FDF-49FF-A06A-F0E176E00D3C}']
    function writeImage(image: UIImage; withSceneDocumentURL: NSURL; originalImageURL: NSURL): NSURL; cdecl;
  end;

  SCNBufferStream = interface(IObjectiveC)
    ['{0675854C-62C6-4057-85A4-503793C04CA3}']
    procedure writeBytes(bytes: Pointer; length: NSUInteger); cdecl;
  end;

  SCNShadable = interface(IObjectiveC)
    ['{442B8377-49F8-4A33-B852-AFBC7A1DEB55}']
    [MethodName('program')]
    function &program: SCNProgram; cdecl;
    procedure handleBindingOfSymbol(symbol: NSString; usingBlock: SCNBindingBlock); cdecl;
    procedure handleUnbindingOfSymbol(symbol: NSString; usingBlock: SCNBindingBlock); cdecl;
    procedure setProgram(&program: SCNProgram); cdecl;
    procedure setShaderModifiers(shaderModifiers: NSDictionary); cdecl;
    function shaderModifiers: NSDictionary; cdecl;
  end;

  SCNProgramClass = interface(NSObjectClass)
    ['{36CC69E5-5D9E-4235-8331-CBEB3F10F5E6}']
    [MethodName('program')]
    {class} function &program: Pointer; cdecl;
  end;

  SCNProgram = interface(NSObject)
    ['{6DE3DC74-8806-4C16-B497-442472316A34}']
    [MethodName('library')]
    function &library: Pointer; cdecl;
    function delegate: Pointer; cdecl;
    function fragmentFunctionName: NSString; cdecl;
    function fragmentShader: NSString; cdecl;
    procedure handleBindingOfBufferNamed(name: NSString; frequency: SCNBufferFrequency; usingBlock: SCNBufferBindingBlock); cdecl;
    function isOpaque: Boolean; cdecl;
    function semanticForSymbol(symbol: NSString): NSString; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setFragmentFunctionName(fragmentFunctionName: NSString); cdecl;
    procedure setFragmentShader(fragmentShader: NSString); cdecl;
    procedure setLibrary(&library: Pointer); cdecl;
    procedure setOpaque(opaque: Boolean); cdecl;
    procedure setSemantic(semantic: NSString; forSymbol: NSString; options: NSDictionary); cdecl;
    procedure setVertexFunctionName(vertexFunctionName: NSString); cdecl;
    procedure setVertexShader(vertexShader: NSString); cdecl;
    function vertexFunctionName: NSString; cdecl;
    function vertexShader: NSString; cdecl;
  end;
  TSCNProgram = class(TOCGenericImport<SCNProgramClass, SCNProgram>) end;

  SCNProgramDelegate = interface(IObjectiveC)
    ['{A2E7BDB7-20D5-4283-A6D0-0243BA466DC7}']
    [MethodName('program:handleError:')]
    procedure &program(&program: SCNProgram; handleError: NSError); cdecl;
    function programIsOpaque(&program: SCNProgram): Boolean; cdecl; // API_DEPRECATED("Use SCNProgram.opaque instead", macos(10.8, 10.10))
  end;

  SCNTechniqueClass = interface(NSObjectClass)
    ['{C00D8B31-A7B0-4EF4-9092-5934ACB8C5A4}']
    {class} function techniqueBySequencingTechniques(techniques: NSArray): SCNTechnique; cdecl;
    {class} function techniqueWithDictionary(dictionary: NSDictionary): SCNTechnique; cdecl;
  end;

  SCNTechnique = interface(NSObject)
    ['{C4ECD3AF-3857-4E5E-BCF9-5E536C32826D}']
    [MethodName('library')]
    function &library: Pointer; cdecl;
    function dictionaryRepresentation: NSDictionary; cdecl;
    procedure handleBindingOfSymbol(symbol: NSString; usingBlock: SCNBindingBlock); cdecl;
    function objectForKeyedSubscript(key: Pointer): Pointer; cdecl;
    procedure setLibrary(&library: Pointer); cdecl;
    procedure setObject(obj: Pointer; forKeyedSubscript: Pointer); cdecl;
  end;
  TSCNTechnique = class(TOCGenericImport<SCNTechniqueClass, SCNTechnique>) end;

  SCNTechniqueSupport = interface(IObjectiveC)
    ['{F92304EF-A636-4800-933A-127247CE3203}']
    procedure setTechnique(technique: SCNTechnique); cdecl;
    function technique: SCNTechnique; cdecl;
  end;

  SCNLightClass = interface(NSObjectClass)
    ['{A43E8DA8-484F-4F21-81F8-E07B930B6753}']
    {class} function light: Pointer; cdecl;
    {class} function lightWithMDLLight(mdlLight: Pointer): Pointer; cdecl;
  end;

  SCNLight = interface(NSObject)
    ['{3B0D2689-1B55-4AFB-9C6A-16CC9D9D5671}']
    [MethodName('type')]
    function &type: SCNLightType; cdecl;
    function areaExtents: simd_float3; cdecl;
    function areaPolygonVertices: NSArray; cdecl;
    function areaType: SCNLightAreaType; cdecl;
    function attenuationEndDistance: CGFloat; cdecl;
    function attenuationFalloffExponent: CGFloat; cdecl;
    function attenuationStartDistance: CGFloat; cdecl;
    function automaticallyAdjustsShadowProjection: Boolean; cdecl;
    function castsShadow: Boolean; cdecl;
    function categoryBitMask: NSUInteger; cdecl;
    function color: Pointer; cdecl;
    function doubleSided: Boolean; cdecl;
    function drawsArea: Boolean; cdecl;
    function forcesBackFaceCasters: Boolean; cdecl;
    function gobo: SCNMaterialProperty; cdecl;
    function IESProfileURL: NSURL; cdecl;
    function intensity: CGFloat; cdecl;
    function maximumShadowDistance: CGFloat; cdecl;
    function name: NSString; cdecl;
    function orthographicScale: CGFloat; cdecl;
    function parallaxCenterOffset: simd_float3; cdecl;
    function parallaxCorrectionEnabled: Boolean; cdecl;
    function parallaxExtentsFactor: simd_float3; cdecl;
    function probeEnvironment: SCNMaterialProperty; cdecl;
    function probeExtents: simd_float3; cdecl;
    function probeOffset: simd_float3; cdecl;
    function probeType: SCNLightProbeType; cdecl;
    function probeUpdateType: SCNLightProbeUpdateType; cdecl;
    function sampleDistributedShadowMaps: Boolean; cdecl;
    procedure setAreaExtents(areaExtents: simd_float3); cdecl;
    procedure setAreaPolygonVertices(areaPolygonVertices: NSArray); cdecl;
    procedure setAreaType(areaType: SCNLightAreaType); cdecl;
    procedure setAttenuationEndDistance(attenuationEndDistance: CGFloat); cdecl;
    procedure setAttenuationFalloffExponent(attenuationFalloffExponent: CGFloat); cdecl;
    procedure setAttenuationStartDistance(attenuationStartDistance: CGFloat); cdecl;
    procedure setAutomaticallyAdjustsShadowProjection(automaticallyAdjustsShadowProjection: Boolean); cdecl;
    procedure setCastsShadow(castsShadow: Boolean); cdecl;
    procedure setCategoryBitMask(categoryBitMask: NSUInteger); cdecl;
    procedure setColor(color: Pointer); cdecl;
    procedure setDoubleSided(doubleSided: Boolean); cdecl;
    procedure setDrawsArea(drawsArea: Boolean); cdecl;
    procedure setForcesBackFaceCasters(forcesBackFaceCasters: Boolean); cdecl;
    procedure setIESProfileURL(IESProfileURL: NSURL); cdecl;
    procedure setIntensity(intensity: CGFloat); cdecl;
    procedure setMaximumShadowDistance(maximumShadowDistance: CGFloat); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setOrthographicScale(orthographicScale: CGFloat); cdecl;
    procedure setParallaxCenterOffset(parallaxCenterOffset: simd_float3); cdecl;
    procedure setParallaxCorrectionEnabled(parallaxCorrectionEnabled: Boolean); cdecl;
    procedure setParallaxExtentsFactor(parallaxExtentsFactor: simd_float3); cdecl;
    procedure setProbeExtents(probeExtents: simd_float3); cdecl;
    procedure setProbeOffset(probeOffset: simd_float3); cdecl;
    procedure setProbeType(probeType: SCNLightProbeType); cdecl;
    procedure setProbeUpdateType(probeUpdateType: SCNLightProbeUpdateType); cdecl;
    procedure setSampleDistributedShadowMaps(sampleDistributedShadowMaps: Boolean); cdecl;
    procedure setShadowBias(shadowBias: CGFloat); cdecl;
    procedure setShadowCascadeCount(shadowCascadeCount: NSUInteger); cdecl;
    procedure setShadowCascadeSplittingFactor(shadowCascadeSplittingFactor: CGFloat); cdecl;
    procedure setShadowColor(shadowColor: Pointer); cdecl;
    procedure setShadowMapSize(shadowMapSize: CGSize); cdecl;
    procedure setShadowMode(shadowMode: SCNShadowMode); cdecl;
    procedure setShadowRadius(shadowRadius: CGFloat); cdecl;
    procedure setShadowSampleCount(shadowSampleCount: NSUInteger); cdecl;
    procedure setSpotInnerAngle(spotInnerAngle: CGFloat); cdecl;
    procedure setSpotOuterAngle(spotOuterAngle: CGFloat); cdecl;
    procedure setTemperature(temperature: CGFloat); cdecl;
    procedure setType(&type: SCNLightType); cdecl;
    procedure setZFar(zFar: CGFloat); cdecl;
    procedure setZNear(zNear: CGFloat); cdecl;
    function shadowBias: CGFloat; cdecl;
    function shadowCascadeCount: NSUInteger; cdecl;
    function shadowCascadeSplittingFactor: CGFloat; cdecl;
    function shadowColor: Pointer; cdecl;
    function shadowMapSize: CGSize; cdecl;
    function shadowMode: SCNShadowMode; cdecl;
    function shadowRadius: CGFloat; cdecl;
    function shadowSampleCount: NSUInteger; cdecl;
    function sphericalHarmonicsCoefficients: NSData; cdecl;
    function spotInnerAngle: CGFloat; cdecl;
    function spotOuterAngle: CGFloat; cdecl;
    function temperature: CGFloat; cdecl;
    function zFar: CGFloat; cdecl;
    function zNear: CGFloat; cdecl;
  end;
  TSCNLight = class(TOCGenericImport<SCNLightClass, SCNLight>) end;

  SCNCameraClass = interface(NSObjectClass)
    ['{C48E02A7-5A71-46CD-9553-F5203F88C4A6}']
    {class} function camera: Pointer; cdecl;
    {class} function cameraWithMDLCamera(mdlCamera: Pointer): Pointer; cdecl;
  end;

  SCNCamera = interface(NSObject)
    ['{1A2F7686-FE48-4A2F-927C-F1194E4D6CE1}']
    function aperture: CGFloat; cdecl; // API_DEPRECATED("Use -[SCNCamera fStop] instead with fStop = sensorHeight / aperture.", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    function apertureBladeCount: NSInteger; cdecl;
    function automaticallyAdjustsZRange: Boolean; cdecl;
    function averageGray: CGFloat; cdecl;
    function bloomBlurRadius: CGFloat; cdecl;
    function bloomIntensity: CGFloat; cdecl;
    function bloomIterationCount: NSInteger; cdecl;
    function bloomIterationSpread: CGFloat; cdecl;
    function bloomThreshold: CGFloat; cdecl;
    function categoryBitMask: NSUInteger; cdecl;
    function colorFringeIntensity: CGFloat; cdecl;
    function colorFringeStrength: CGFloat; cdecl;
    function colorGrading: SCNMaterialProperty; cdecl;
    function contrast: CGFloat; cdecl;
    function exposureAdaptationBrighteningSpeedFactor: CGFloat; cdecl;
    function exposureAdaptationDarkeningSpeedFactor: CGFloat; cdecl;
    function exposureOffset: CGFloat; cdecl;
    function fieldOfView: CGFloat; cdecl;
    function focalBlurRadius: CGFloat; cdecl; // API_DEPRECATED("Use fStop instead", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    function focalBlurSampleCount: NSInteger; cdecl;
    function focalDistance: CGFloat; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-focusDistance", macos(10.9, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    function focalLength: CGFloat; cdecl;
    function focalSize: CGFloat; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-focusDistance", macos(10.9, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    function focusDistance: CGFloat; cdecl;
    function fStop: CGFloat; cdecl;
    function grainIntensity: CGFloat; cdecl;
    function grainIsColored: Boolean; cdecl;
    function grainScale: CGFloat; cdecl;
    function maximumExposure: CGFloat; cdecl;
    function minimumExposure: CGFloat; cdecl;
    function motionBlurIntensity: CGFloat; cdecl;
    function name: NSString; cdecl;
    function orthographicScale: Double; cdecl;
    function projectionDirection: SCNCameraProjectionDirection; cdecl;
    function projectionTransform: SCNMatrix4; cdecl;
    function projectionTransformWithViewportSize(viewportSize: CGSize): SCNMatrix4; cdecl;
    function saturation: CGFloat; cdecl;
    function screenSpaceAmbientOcclusionBias: CGFloat; cdecl;
    function screenSpaceAmbientOcclusionDepthThreshold: CGFloat; cdecl;
    function screenSpaceAmbientOcclusionIntensity: CGFloat; cdecl;
    function screenSpaceAmbientOcclusionNormalThreshold: CGFloat; cdecl;
    function screenSpaceAmbientOcclusionRadius: CGFloat; cdecl;
    function sensorHeight: CGFloat; cdecl;
    procedure setAperture(aperture: CGFloat); cdecl; // API_DEPRECATED("Use -[SCNCamera fStop] instead with fStop = sensorHeight / aperture.", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setApertureBladeCount(apertureBladeCount: NSInteger); cdecl;
    procedure setAutomaticallyAdjustsZRange(automaticallyAdjustsZRange: Boolean); cdecl;
    procedure setAverageGray(averageGray: CGFloat); cdecl;
    procedure setBloomBlurRadius(bloomBlurRadius: CGFloat); cdecl;
    procedure setBloomIntensity(bloomIntensity: CGFloat); cdecl;
    procedure setBloomIterationCount(bloomIterationCount: NSInteger); cdecl;
    procedure setBloomIterationSpread(bloomIterationSpread: CGFloat); cdecl;
    procedure setBloomThreshold(bloomThreshold: CGFloat); cdecl;
    procedure setCategoryBitMask(categoryBitMask: NSUInteger); cdecl;
    procedure setColorFringeIntensity(colorFringeIntensity: CGFloat); cdecl;
    procedure setColorFringeStrength(colorFringeStrength: CGFloat); cdecl;
    procedure setContrast(contrast: CGFloat); cdecl;
    procedure setExposureAdaptationBrighteningSpeedFactor(exposureAdaptationBrighteningSpeedFactor: CGFloat); cdecl;
    procedure setExposureAdaptationDarkeningSpeedFactor(exposureAdaptationDarkeningSpeedFactor: CGFloat); cdecl;
    procedure setExposureOffset(exposureOffset: CGFloat); cdecl;
    procedure setFieldOfView(fieldOfView: CGFloat); cdecl;
    procedure setFocalBlurRadius(focalBlurRadius: CGFloat); cdecl; // API_DEPRECATED("Use fStop instead", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setFocalBlurSampleCount(focalBlurSampleCount: NSInteger); cdecl;
    procedure setFocalDistance(focalDistance: CGFloat); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-focusDistance", macos(10.9, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setFocalLength(focalLength: CGFloat); cdecl;
    procedure setFocalSize(focalSize: CGFloat); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-focusDistance", macos(10.9, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setFocusDistance(focusDistance: CGFloat); cdecl;
    procedure setFStop(fStop: CGFloat); cdecl;
    procedure setGrainIntensity(grainIntensity: CGFloat); cdecl;
    procedure setGrainIsColored(grainIsColored: Boolean); cdecl;
    procedure setGrainScale(grainScale: CGFloat); cdecl;
    procedure setMaximumExposure(maximumExposure: CGFloat); cdecl;
    procedure setMinimumExposure(minimumExposure: CGFloat); cdecl;
    procedure setMotionBlurIntensity(motionBlurIntensity: CGFloat); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setOrthographicScale(orthographicScale: Double); cdecl;
    procedure setProjectionDirection(projectionDirection: SCNCameraProjectionDirection); cdecl;
    procedure setProjectionTransform(projectionTransform: SCNMatrix4); cdecl;
    procedure setSaturation(saturation: CGFloat); cdecl;
    procedure setScreenSpaceAmbientOcclusionBias(screenSpaceAmbientOcclusionBias: CGFloat); cdecl;
    procedure setScreenSpaceAmbientOcclusionDepthThreshold(screenSpaceAmbientOcclusionDepthThreshold: CGFloat); cdecl;
    procedure setScreenSpaceAmbientOcclusionIntensity(screenSpaceAmbientOcclusionIntensity: CGFloat); cdecl;
    procedure setScreenSpaceAmbientOcclusionNormalThreshold(screenSpaceAmbientOcclusionNormalThreshold: CGFloat); cdecl;
    procedure setScreenSpaceAmbientOcclusionRadius(screenSpaceAmbientOcclusionRadius: CGFloat); cdecl;
    procedure setSensorHeight(sensorHeight: CGFloat); cdecl;
    procedure setUsesOrthographicProjection(usesOrthographicProjection: Boolean); cdecl;
    procedure setVignettingIntensity(vignettingIntensity: CGFloat); cdecl;
    procedure setVignettingPower(vignettingPower: CGFloat); cdecl;
    procedure setWantsDepthOfField(wantsDepthOfField: Boolean); cdecl;
    procedure setWantsExposureAdaptation(wantsExposureAdaptation: Boolean); cdecl;
    procedure setWantsHDR(wantsHDR: Boolean); cdecl;
    procedure setWhiteBalanceTemperature(whiteBalanceTemperature: CGFloat); cdecl;
    procedure setWhiteBalanceTint(whiteBalanceTint: CGFloat); cdecl;
    procedure setWhitePoint(whitePoint: CGFloat); cdecl;
    procedure setXFov(xFov: Double); cdecl; // API_DEPRECATED("Use -[SCNCamera fieldOfView] or -[SCNCamera focalLength] instead", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setYFov(yFov: Double); cdecl; // API_DEPRECATED("Use -[SCNCamera fieldOfView] or -[SCNCamera focalLength] instead", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setZFar(zFar: Double); cdecl;
    procedure setZNear(zNear: Double); cdecl;
    function usesOrthographicProjection: Boolean; cdecl;
    function vignettingIntensity: CGFloat; cdecl;
    function vignettingPower: CGFloat; cdecl;
    function wantsDepthOfField: Boolean; cdecl;
    function wantsExposureAdaptation: Boolean; cdecl;
    function wantsHDR: Boolean; cdecl;
    function whiteBalanceTemperature: CGFloat; cdecl;
    function whiteBalanceTint: CGFloat; cdecl;
    function whitePoint: CGFloat; cdecl;
    function xFov: Double; cdecl; // API_DEPRECATED("Use -[SCNCamera fieldOfView] or -[SCNCamera focalLength] instead", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    function yFov: Double; cdecl; // API_DEPRECATED("Use -[SCNCamera fieldOfView] or -[SCNCamera focalLength] instead", macos(10.8, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    function zFar: Double; cdecl;
    function zNear: Double; cdecl;
  end;
  TSCNCamera = class(TOCGenericImport<SCNCameraClass, SCNCamera>) end;

  SCNGeometryClass = interface(NSObjectClass)
    ['{55D9D544-B480-453B-AE35-390F40BB05D1}']
    {class} function geometry: Pointer; cdecl;
    {class} function geometryWithMDLMesh(mdlMesh: Pointer): Pointer; cdecl;
    {class} function geometryWithSources(sources: NSArray; elements: NSArray): Pointer; cdecl;
  end;

  SCNGeometry = interface(NSObject)
    ['{0056EDAC-0D6D-4899-8B4D-B34CC3C493FF}']
    function edgeCreasesElement: SCNGeometryElement; cdecl;
    function edgeCreasesSource: SCNGeometrySource; cdecl;
    function firstMaterial: SCNMaterial; cdecl;
    function geometryElementAtIndex(elementIndex: NSInteger): SCNGeometryElement; cdecl;
    function geometryElementCount: NSInteger; cdecl;
    function geometryElements: NSArray; cdecl;
    function geometrySources: NSArray; cdecl;
    function geometrySourcesForSemantic(semantic: SCNGeometrySourceSemantic): NSArray; cdecl;
    procedure insertMaterial(material: SCNMaterial; atIndex: NSUInteger); cdecl;
    function levelsOfDetail: NSArray; cdecl;
    function materials: NSArray; cdecl;
    function materialWithName(name: NSString): SCNMaterial; cdecl;
    function name: NSString; cdecl;
    procedure removeMaterialAtIndex(index: NSUInteger); cdecl;
    procedure replaceMaterialAtIndex(index: NSUInteger; withMaterial: SCNMaterial); cdecl;
    procedure setEdgeCreasesElement(edgeCreasesElement: SCNGeometryElement); cdecl;
    procedure setEdgeCreasesSource(edgeCreasesSource: SCNGeometrySource); cdecl;
    procedure setFirstMaterial(firstMaterial: SCNMaterial); cdecl;
    procedure setLevelsOfDetail(levelsOfDetail: NSArray); cdecl;
    procedure setMaterials(materials: NSArray); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setSubdivisionLevel(subdivisionLevel: NSUInteger); cdecl;
    procedure setTessellator(tessellator: SCNGeometryTessellator); cdecl;
    procedure setWantsAdaptiveSubdivision(wantsAdaptiveSubdivision: Boolean); cdecl;
    function subdivisionLevel: NSUInteger; cdecl;
    function tessellator: SCNGeometryTessellator; cdecl;
    function wantsAdaptiveSubdivision: Boolean; cdecl;
  end;
  TSCNGeometry = class(TOCGenericImport<SCNGeometryClass, SCNGeometry>) end;

  SCNGeometrySourceClass = interface(NSObjectClass)
    ['{45E59684-5399-43CF-946B-D1443EC86940}']
    {class} function geometrySourceWithBuffer(buffer: Pointer; vertexFormat: MTLVertexFormat; semantic: SCNGeometrySourceSemantic; vertexCount: NSInteger; dataOffset: NSInteger; dataStride: NSInteger): Pointer; cdecl;
    {class} function geometrySourceWithData(data: NSData; semantic: SCNGeometrySourceSemantic; vectorCount: NSInteger; floatComponents: Boolean; componentsPerVector: NSInteger; bytesPerComponent: NSInteger; dataOffset: NSInteger; dataStride: NSInteger): Pointer; cdecl;
    {class} function geometrySourceWithNormals(normals: PSCNVector3; count: NSInteger): Pointer; cdecl;
    {class} function geometrySourceWithTextureCoordinates(texcoord: PCGPoint; count: NSInteger): Pointer; cdecl;
    {class} function geometrySourceWithVertices(vertices: PSCNVector3; count: NSInteger): Pointer; cdecl;
  end;

  SCNGeometrySource = interface(NSObject)
    ['{2A942E88-2E63-44A6-ABE9-66E42DD22F8B}']
    function bytesPerComponent: NSInteger; cdecl;
    function componentsPerVector: NSInteger; cdecl;
    function data: NSData; cdecl;
    function dataOffset: NSInteger; cdecl;
    function dataStride: NSInteger; cdecl;
    function floatComponents: Boolean; cdecl;
    function semantic: SCNGeometrySourceSemantic; cdecl;
    function vectorCount: NSInteger; cdecl;
  end;
  TSCNGeometrySource = class(TOCGenericImport<SCNGeometrySourceClass, SCNGeometrySource>) end;

  SCNGeometryElementClass = interface(NSObjectClass)
    ['{C5D987AD-2947-400F-98A4-63516FA01F7D}']
    {class} function geometryElementWithBuffer(buffer: Pointer; primitiveType: SCNGeometryPrimitiveType; primitiveCount: NSInteger; bytesPerIndex: NSInteger): Pointer; cdecl;
    {class} function geometryElementWithData(data: NSData; primitiveType: SCNGeometryPrimitiveType; primitiveCount: NSInteger; bytesPerIndex: NSInteger): Pointer; cdecl;
    {class} function geometryElementWithMDLSubmesh(mdlSubMesh: Pointer): Pointer; cdecl;
  end;

  SCNGeometryElement = interface(NSObject)
    ['{BEBF6E0C-4A42-4C3A-8DE0-1B09B823D797}']
    function bytesPerIndex: NSInteger; cdecl;
    function data: NSData; cdecl;
    function maximumPointScreenSpaceRadius: CGFloat; cdecl;
    function minimumPointScreenSpaceRadius: CGFloat; cdecl;
    function pointSize: CGFloat; cdecl;
    function primitiveCount: NSInteger; cdecl;
    function primitiveRange: NSRange; cdecl;
    function primitiveType: SCNGeometryPrimitiveType; cdecl;
    procedure setMaximumPointScreenSpaceRadius(maximumPointScreenSpaceRadius: CGFloat); cdecl;
    procedure setMinimumPointScreenSpaceRadius(minimumPointScreenSpaceRadius: CGFloat); cdecl;
    procedure setPointSize(pointSize: CGFloat); cdecl;
    procedure setPrimitiveRange(primitiveRange: NSRange); cdecl;
  end;
  TSCNGeometryElement = class(TOCGenericImport<SCNGeometryElementClass, SCNGeometryElement>) end;

  SCNGeometryTessellatorClass = interface(NSObjectClass)
    ['{C6F2D84D-3FEF-4C97-AD49-44328EBD9D26}']
  end;

  SCNGeometryTessellator = interface(NSObject)
    ['{A60B3472-00EA-4D94-8E80-749489F4FB8E}']
    function edgeTessellationFactor: CGFloat; cdecl;
    function insideTessellationFactor: CGFloat; cdecl;
    function isAdaptive: Boolean; cdecl;
    function isScreenSpace: Boolean; cdecl;
    function maximumEdgeLength: CGFloat; cdecl;
    procedure setAdaptive(adaptive: Boolean); cdecl;
    procedure setEdgeTessellationFactor(edgeTessellationFactor: CGFloat); cdecl;
    procedure setInsideTessellationFactor(insideTessellationFactor: CGFloat); cdecl;
    procedure setMaximumEdgeLength(maximumEdgeLength: CGFloat); cdecl;
    procedure setScreenSpace(screenSpace: Boolean); cdecl;
    procedure setSmoothingMode(smoothingMode: SCNTessellationSmoothingMode); cdecl;
    procedure setTessellationFactorScale(tessellationFactorScale: CGFloat); cdecl;
    procedure setTessellationPartitionMode(tessellationPartitionMode: MTLTessellationPartitionMode); cdecl;
    function smoothingMode: SCNTessellationSmoothingMode; cdecl;
    function tessellationFactorScale: CGFloat; cdecl;
    function tessellationPartitionMode: MTLTessellationPartitionMode; cdecl;
  end;
  TSCNGeometryTessellator = class(TOCGenericImport<SCNGeometryTessellatorClass, SCNGeometryTessellator>) end;

  SCNMaterialClass = interface(NSObjectClass)
    ['{4DE542BF-9B92-4F93-A1DC-4ED9D75944FB}']
    {class} function material: Pointer; cdecl;
    {class} function materialWithMDLMaterial(mdlMaterial: Pointer): Pointer; cdecl;
  end;

  SCNMaterial = interface(NSObject)
    ['{950AB0AC-6E5F-4F36-B0C4-12A5ECFBBFE6}']
    function ambient: SCNMaterialProperty; cdecl;
    function ambientOcclusion: SCNMaterialProperty; cdecl;
    function blendMode: SCNBlendMode; cdecl;
    function clearCoat: SCNMaterialProperty; cdecl;
    function clearCoatNormal: SCNMaterialProperty; cdecl;
    function clearCoatRoughness: SCNMaterialProperty; cdecl;
    function colorBufferWriteMask: SCNColorMask; cdecl;
    function cullMode: SCNCullMode; cdecl;
    function diffuse: SCNMaterialProperty; cdecl;
    function displacement: SCNMaterialProperty; cdecl;
    function emission: SCNMaterialProperty; cdecl;
    function fillMode: SCNFillMode; cdecl;
    function fresnelExponent: CGFloat; cdecl;
    function isDoubleSided: Boolean; cdecl;
    function isLitPerPixel: Boolean; cdecl;
    function lightingModelName: SCNLightingModel; cdecl;
    function locksAmbientWithDiffuse: Boolean; cdecl;
    function metalness: SCNMaterialProperty; cdecl;
    function multiply: SCNMaterialProperty; cdecl;
    function name: NSString; cdecl;
    function normal: SCNMaterialProperty; cdecl;
    function readsFromDepthBuffer: Boolean; cdecl;
    function reflective: SCNMaterialProperty; cdecl;
    function roughness: SCNMaterialProperty; cdecl;
    function selfIllumination: SCNMaterialProperty; cdecl;
    procedure setBlendMode(blendMode: SCNBlendMode); cdecl;
    procedure setColorBufferWriteMask(colorBufferWriteMask: SCNColorMask); cdecl;
    procedure setCullMode(cullMode: SCNCullMode); cdecl;
    procedure setDoubleSided(doubleSided: Boolean); cdecl;
    procedure setFillMode(fillMode: SCNFillMode); cdecl;
    procedure setFresnelExponent(fresnelExponent: CGFloat); cdecl;
    procedure setLightingModelName(lightingModelName: SCNLightingModel); cdecl;
    procedure setLitPerPixel(litPerPixel: Boolean); cdecl;
    procedure setLocksAmbientWithDiffuse(locksAmbientWithDiffuse: Boolean); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setReadsFromDepthBuffer(readsFromDepthBuffer: Boolean); cdecl;
    procedure setShininess(shininess: CGFloat); cdecl;
    procedure setTransparency(transparency: CGFloat); cdecl;
    procedure setTransparencyMode(transparencyMode: SCNTransparencyMode); cdecl;
    procedure setWritesToDepthBuffer(writesToDepthBuffer: Boolean); cdecl;
    function shininess: CGFloat; cdecl;
    function specular: SCNMaterialProperty; cdecl;
    function transparency: CGFloat; cdecl;
    function transparencyMode: SCNTransparencyMode; cdecl;
    function transparent: SCNMaterialProperty; cdecl;
    function writesToDepthBuffer: Boolean; cdecl;
  end;
  TSCNMaterial = class(TOCGenericImport<SCNMaterialClass, SCNMaterial>) end;

  SCNHitTestResultClass = interface(NSObjectClass)
    ['{DC2E44BD-CA2F-4EF1-A138-3D55B3A0126D}']
  end;

  SCNHitTestResult = interface(NSObject)
    ['{E991F1EB-546F-4589-8864-84A6FF645763}']
    function boneNode: SCNNode; cdecl;
    function faceIndex: NSInteger; cdecl;
    function geometryIndex: NSInteger; cdecl;
    function localCoordinates: SCNVector3; cdecl;
    function localNormal: SCNVector3; cdecl;
    function modelTransform: SCNMatrix4; cdecl;
    function node: SCNNode; cdecl;
    function simdLocalCoordinates: simd_float3; cdecl;
    function simdLocalNormal: simd_float3; cdecl;
    function simdModelTransform: simd_float4x4; cdecl;
    function simdWorldCoordinates: simd_float3; cdecl;
    function simdWorldNormal: simd_float3; cdecl;
    function textureCoordinatesWithMappingChannel(channel: NSInteger): CGPoint; cdecl;
    function worldCoordinates: SCNVector3; cdecl;
    function worldNormal: SCNVector3; cdecl;
  end;
  TSCNHitTestResult = class(TOCGenericImport<SCNHitTestResultClass, SCNHitTestResult>) end;

  SCNSceneRenderer = interface(IObjectiveC)
    ['{5E3A719E-583C-4981-94CD-1F5E53D87C33}']
    function audioEngine: AVAudioEngine; cdecl;
    function audioEnvironmentNode: AVAudioEnvironmentNode; cdecl;
    function audioListener: SCNNode; cdecl;
    function autoenablesDefaultLighting: Boolean; cdecl;
    function colorPixelFormat: MTLPixelFormat; cdecl;
    function commandQueue: Pointer; cdecl;
    function context: Pointer; cdecl;
    function currentRenderCommandEncoder: Pointer; cdecl;
    function currentRenderPassDescriptor: MTLRenderPassDescriptor; cdecl;
    function currentViewport: CGRect; cdecl;
    function debugOptions: SCNDebugOptions; cdecl;
    function delegate: Pointer; cdecl;
    function depthPixelFormat: MTLPixelFormat; cdecl;
    function device: Pointer; cdecl;
    function hitTest(point: CGPoint; options: NSDictionary): NSArray; cdecl;
    function isJitteringEnabled: Boolean; cdecl;
    function isNodeInsideFrustum(node: SCNNode; withPointOfView: SCNNode): Boolean; cdecl;
    function isPlaying: Boolean; cdecl;
    function isTemporalAntialiasingEnabled: Boolean; cdecl;
    function loops: Boolean; cdecl;
    function nodesInsideFrustumWithPointOfView(pointOfView: SCNNode): NSArray; cdecl;
    function overlaySKScene: SKScene; cdecl;
    function pointOfView: SCNNode; cdecl;
    function prepareObject(&object: Pointer; shouldAbortBlock: TSCNSceneRendererBlockMethod1): Boolean; cdecl;
    procedure prepareObjects(objects: NSArray; withCompletionHandler: TSCNSceneRendererBlockMethod2); cdecl;
    procedure presentScene(scene: SCNScene; withTransition: SKTransition; incomingPointOfView: SCNNode; completionHandler: TSCNSceneRendererBlockMethod1); cdecl;
    function projectPoint(point: SCNVector3): SCNVector3; cdecl;
    function renderingAPI: SCNRenderingAPI; cdecl;
    function scene: SCNScene; cdecl;
    function sceneTime: NSTimeInterval; cdecl;
    procedure setAudioListener(audioListener: SCNNode); cdecl;
    procedure setAutoenablesDefaultLighting(autoenablesDefaultLighting: Boolean); cdecl;
    procedure setDebugOptions(debugOptions: SCNDebugOptions); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setJitteringEnabled(jitteringEnabled: Boolean); cdecl;
    procedure setLoops(loops: Boolean); cdecl;
    procedure setOverlaySKScene(overlaySKScene: SKScene); cdecl;
    procedure setPlaying(playing: Boolean); cdecl;
    procedure setPointOfView(pointOfView: SCNNode); cdecl;
    procedure setScene(scene: SCNScene); cdecl;
    procedure setSceneTime(sceneTime: NSTimeInterval); cdecl;
    procedure setShowsStatistics(showsStatistics: Boolean); cdecl;
    procedure setTemporalAntialiasingEnabled(temporalAntialiasingEnabled: Boolean); cdecl;
    procedure setUsesReverseZ(usesReverseZ: Boolean); cdecl;
    function showsStatistics: Boolean; cdecl;
    function stencilPixelFormat: MTLPixelFormat; cdecl;
    function unprojectPoint(point: SCNVector3): SCNVector3; cdecl;
    function usesReverseZ: Boolean; cdecl;
  end;

  SCNSceneRendererDelegate = interface(IObjectiveC)
    ['{341215ED-0A73-4693-A878-6A353240F4F2}']
    [MethodName('renderer:didApplyAnimationsAtTime:')]
    procedure rendererDidApplyAnimationsAtTime(renderer: Pointer; didApplyAnimationsAtTime: NSTimeInterval); cdecl;
    [MethodName('renderer:didApplyConstraintsAtTime:')]
    procedure rendererDidApplyConstraintsAtTime(renderer: Pointer; didApplyConstraintsAtTime: NSTimeInterval); cdecl;
    [MethodName('renderer:didRenderScene:atTime:')]
    procedure rendererDidRenderScene(renderer: Pointer; didRenderScene: SCNScene; atTime: NSTimeInterval); cdecl;
    [MethodName('renderer:didSimulatePhysicsAtTime:')]
    procedure rendererDidSimulatePhysicsAtTime(renderer: Pointer; didSimulatePhysicsAtTime: NSTimeInterval); cdecl;
    [MethodName('renderer:updateAtTime:')]
    procedure rendererUpdateAtTime(renderer: Pointer; updateAtTime: NSTimeInterval); cdecl;
    [MethodName('renderer:willRenderScene:atTime:')]
    procedure rendererWillRenderScene(renderer: Pointer; willRenderScene: SCNScene; atTime: NSTimeInterval); cdecl;
  end;

  SCNCameraControlConfiguration = interface(IObjectiveC)
    ['{23F45FD8-DE49-4174-9576-2B96A8EAA1A6}']
    function allowsTranslation: Boolean; cdecl;
    function autoSwitchToFreeCamera: Boolean; cdecl;
    function flyModeVelocity: CGFloat; cdecl;
    function panSensitivity: CGFloat; cdecl;
    function rotationSensitivity: CGFloat; cdecl;
    procedure setAllowsTranslation(allowsTranslation: Boolean); cdecl;
    procedure setAutoSwitchToFreeCamera(autoSwitchToFreeCamera: Boolean); cdecl;
    procedure setFlyModeVelocity(flyModeVelocity: CGFloat); cdecl;
    procedure setPanSensitivity(panSensitivity: CGFloat); cdecl;
    procedure setRotationSensitivity(rotationSensitivity: CGFloat); cdecl;
    procedure setTruckSensitivity(truckSensitivity: CGFloat); cdecl;
    function truckSensitivity: CGFloat; cdecl;
  end;

  SCNViewClass = interface(UIViewClass)
    ['{7D56409B-1FF9-4BA8-BA3E-3D1EDECE8DBD}']
  end;

  SCNView = interface(UIView)
    ['{8925A273-5F61-4233-84BF-55E97ABEAE50}']
    function allowsCameraControl: Boolean; cdecl;
    function antialiasingMode: SCNAntialiasingMode; cdecl;
    function cameraControlConfiguration: Pointer; cdecl;
    function defaultCameraController: SCNCameraController; cdecl;
    function eaglContext: EAGLContext; cdecl;
    function initWithFrame(frame: CGRect; options: NSDictionary): Pointer; cdecl;
    procedure pause(sender: Pointer); cdecl;
    procedure play(sender: Pointer); cdecl;
    function preferredFramesPerSecond: NSInteger; cdecl;
    function rendersContinuously: Boolean; cdecl;
    function scene: SCNScene; cdecl;
    procedure setAllowsCameraControl(allowsCameraControl: Boolean); cdecl;
    procedure setAntialiasingMode(antialiasingMode: SCNAntialiasingMode); cdecl;
    procedure setEaglContext(eaglContext: EAGLContext); cdecl;
    procedure setPreferredFramesPerSecond(preferredFramesPerSecond: NSInteger); cdecl;
    procedure setRendersContinuously(rendersContinuously: Boolean); cdecl;
    procedure setScene(scene: SCNScene); cdecl;
    function snapshot: UIImage; cdecl;
    procedure stop(sender: Pointer); cdecl;
  end;
  TSCNView = class(TOCGenericImport<SCNViewClass, SCNView>) end;

  SCNRendererClass = interface(NSObjectClass)
    ['{D92DC396-4903-4955-8760-F0B451468563}']
    {class} function rendererWithContext(context: EAGLContext; options: NSDictionary): Pointer; cdecl;
    {class} function rendererWithDevice(device: Pointer; options: NSDictionary): Pointer; cdecl;
  end;

  SCNRenderer = interface(NSObject)
    ['{0AF00943-2530-403F-BB71-0990C8C3EA06}']
    function nextFrameTime: CFTimeInterval; cdecl;
    procedure render; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-renderAtTime:withEncoder:pass:commandQueue:", macos(10.8, 10.11), ios(8.0, 9.0))
    procedure renderAtTime(time: CFTimeInterval); overload; cdecl;
    procedure renderAtTime(time: CFTimeInterval; viewport: CGRect; commandBuffer: Pointer; passDescriptor: MTLRenderPassDescriptor); overload; cdecl;
    procedure renderWithViewport(viewport: CGRect; commandBuffer: Pointer; passDescriptor: MTLRenderPassDescriptor); cdecl;
    function scene: SCNScene; cdecl;
    procedure setScene(scene: SCNScene); cdecl;
    function snapshotAtTime(time: CFTimeInterval; withSize: CGSize; antialiasingMode: SCNAntialiasingMode): UIImage; cdecl;
    procedure updateAtTime(time: CFTimeInterval); cdecl;
    procedure updateProbes(lightProbes: NSArray; atTime: CFTimeInterval); cdecl;
  end;
  TSCNRenderer = class(TOCGenericImport<SCNRendererClass, SCNRenderer>) end;

  SCNPlaneClass = interface(SCNGeometryClass)
    ['{2E521332-F7AB-4AFC-B61F-BE215B7DEE14}']
    {class} function planeWithWidth(width: CGFloat; height: CGFloat): Pointer; cdecl;
  end;

  SCNPlane = interface(SCNGeometry)
    ['{88442B6B-0566-4D9B-9D2C-6E09D6FB9866}']
    function cornerRadius: CGFloat; cdecl;
    function cornerSegmentCount: NSInteger; cdecl;
    function height: CGFloat; cdecl;
    function heightSegmentCount: NSInteger; cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
    procedure setCornerSegmentCount(cornerSegmentCount: NSInteger); cdecl;
    procedure setHeight(height: CGFloat); cdecl;
    procedure setHeightSegmentCount(heightSegmentCount: NSInteger); cdecl;
    procedure setWidth(width: CGFloat); cdecl;
    procedure setWidthSegmentCount(widthSegmentCount: NSInteger); cdecl;
    function width: CGFloat; cdecl;
    function widthSegmentCount: NSInteger; cdecl;
  end;
  TSCNPlane = class(TOCGenericImport<SCNPlaneClass, SCNPlane>) end;

  SCNBoxClass = interface(SCNGeometryClass)
    ['{3670C651-386A-4D76-BD90-F478AAADBB29}']
    {class} function boxWithWidth(width: CGFloat; height: CGFloat; length: CGFloat; chamferRadius: CGFloat): Pointer; cdecl;
  end;

  SCNBox = interface(SCNGeometry)
    ['{1DBB8523-93E6-4B30-B69F-FD215C7B2BB0}']
    function chamferRadius: CGFloat; cdecl;
    function chamferSegmentCount: NSInteger; cdecl;
    function height: CGFloat; cdecl;
    function heightSegmentCount: NSInteger; cdecl;
    function length: CGFloat; cdecl;
    function lengthSegmentCount: NSInteger; cdecl;
    procedure setChamferRadius(chamferRadius: CGFloat); cdecl;
    procedure setChamferSegmentCount(chamferSegmentCount: NSInteger); cdecl;
    procedure setHeight(height: CGFloat); cdecl;
    procedure setHeightSegmentCount(heightSegmentCount: NSInteger); cdecl;
    procedure setLength(length: CGFloat); cdecl;
    procedure setLengthSegmentCount(lengthSegmentCount: NSInteger); cdecl;
    procedure setWidth(width: CGFloat); cdecl;
    procedure setWidthSegmentCount(widthSegmentCount: NSInteger); cdecl;
    function width: CGFloat; cdecl;
    function widthSegmentCount: NSInteger; cdecl;
  end;
  TSCNBox = class(TOCGenericImport<SCNBoxClass, SCNBox>) end;

  SCNPyramidClass = interface(SCNGeometryClass)
    ['{B0205370-360A-4ED5-90E9-3C1A6E6CAE21}']
    {class} function pyramidWithWidth(width: CGFloat; height: CGFloat; length: CGFloat): Pointer; cdecl;
  end;

  SCNPyramid = interface(SCNGeometry)
    ['{8079D9D9-EA5D-49DC-B739-F3A9A5FD494C}']
    function height: CGFloat; cdecl;
    function heightSegmentCount: NSInteger; cdecl;
    function length: CGFloat; cdecl;
    function lengthSegmentCount: NSInteger; cdecl;
    procedure setHeight(height: CGFloat); cdecl;
    procedure setHeightSegmentCount(heightSegmentCount: NSInteger); cdecl;
    procedure setLength(length: CGFloat); cdecl;
    procedure setLengthSegmentCount(lengthSegmentCount: NSInteger); cdecl;
    procedure setWidth(width: CGFloat); cdecl;
    procedure setWidthSegmentCount(widthSegmentCount: NSInteger); cdecl;
    function width: CGFloat; cdecl;
    function widthSegmentCount: NSInteger; cdecl;
  end;
  TSCNPyramid = class(TOCGenericImport<SCNPyramidClass, SCNPyramid>) end;

  SCNSphereClass = interface(SCNGeometryClass)
    ['{C37E91BF-CE8A-4A61-A1B8-B01B765F02C3}']
    {class} function sphereWithRadius(radius: CGFloat): Pointer; cdecl;
  end;

  SCNSphere = interface(SCNGeometry)
    ['{4ACB69FB-3C1B-4C21-A4E2-146652CE4FC5}']
    function isGeodesic: Boolean; cdecl;
    function radius: CGFloat; cdecl;
    function segmentCount: NSInteger; cdecl;
    procedure setGeodesic(geodesic: Boolean); cdecl;
    procedure setRadius(radius: CGFloat); cdecl;
    procedure setSegmentCount(segmentCount: NSInteger); cdecl;
  end;
  TSCNSphere = class(TOCGenericImport<SCNSphereClass, SCNSphere>) end;

  SCNCylinderClass = interface(SCNGeometryClass)
    ['{91D94D8B-2644-432B-9ECB-0795630FB37E}']
    {class} function cylinderWithRadius(radius: CGFloat; height: CGFloat): Pointer; cdecl;
  end;

  SCNCylinder = interface(SCNGeometry)
    ['{E1EAB203-350E-4B78-8CAC-14656A817E7D}']
    function height: CGFloat; cdecl;
    function heightSegmentCount: NSInteger; cdecl;
    function radialSegmentCount: NSInteger; cdecl;
    function radius: CGFloat; cdecl;
    procedure setHeight(height: CGFloat); cdecl;
    procedure setHeightSegmentCount(heightSegmentCount: NSInteger); cdecl;
    procedure setRadialSegmentCount(radialSegmentCount: NSInteger); cdecl;
    procedure setRadius(radius: CGFloat); cdecl;
  end;
  TSCNCylinder = class(TOCGenericImport<SCNCylinderClass, SCNCylinder>) end;

  SCNConeClass = interface(SCNGeometryClass)
    ['{B6AB4F59-F862-4754-B195-90FF3FBE807A}']
    {class} function coneWithTopRadius(topRadius: CGFloat; bottomRadius: CGFloat; height: CGFloat): Pointer; cdecl;
  end;

  SCNCone = interface(SCNGeometry)
    ['{461CAD3D-735D-41F0-BECE-7CB5E367EC38}']
    function bottomRadius: CGFloat; cdecl;
    function height: CGFloat; cdecl;
    function heightSegmentCount: NSInteger; cdecl;
    function radialSegmentCount: NSInteger; cdecl;
    procedure setBottomRadius(bottomRadius: CGFloat); cdecl;
    procedure setHeight(height: CGFloat); cdecl;
    procedure setHeightSegmentCount(heightSegmentCount: NSInteger); cdecl;
    procedure setRadialSegmentCount(radialSegmentCount: NSInteger); cdecl;
    procedure setTopRadius(topRadius: CGFloat); cdecl;
    function topRadius: CGFloat; cdecl;
  end;
  TSCNCone = class(TOCGenericImport<SCNConeClass, SCNCone>) end;

  SCNTubeClass = interface(SCNGeometryClass)
    ['{339818A6-CA90-48CF-B38F-DEA03B1EA760}']
    {class} function tubeWithInnerRadius(innerRadius: CGFloat; outerRadius: CGFloat; height: CGFloat): Pointer; cdecl;
  end;

  SCNTube = interface(SCNGeometry)
    ['{FF289B32-06E2-4C81-A358-EDEF40AE5318}']
    function height: CGFloat; cdecl;
    function heightSegmentCount: NSInteger; cdecl;
    function innerRadius: CGFloat; cdecl;
    function outerRadius: CGFloat; cdecl;
    function radialSegmentCount: NSInteger; cdecl;
    procedure setHeight(height: CGFloat); cdecl;
    procedure setHeightSegmentCount(heightSegmentCount: NSInteger); cdecl;
    procedure setInnerRadius(innerRadius: CGFloat); cdecl;
    procedure setOuterRadius(outerRadius: CGFloat); cdecl;
    procedure setRadialSegmentCount(radialSegmentCount: NSInteger); cdecl;
  end;
  TSCNTube = class(TOCGenericImport<SCNTubeClass, SCNTube>) end;

  SCNCapsuleClass = interface(SCNGeometryClass)
    ['{391A57D8-6DC0-453B-B011-2EFE6A2B53DE}']
    {class} function capsuleWithCapRadius(capRadius: CGFloat; height: CGFloat): Pointer; cdecl;
  end;

  SCNCapsule = interface(SCNGeometry)
    ['{ABD6848D-6F04-4CAF-B752-900CBA24A6EC}']
    function capRadius: CGFloat; cdecl;
    function capSegmentCount: NSInteger; cdecl;
    function height: CGFloat; cdecl;
    function heightSegmentCount: NSInteger; cdecl;
    function radialSegmentCount: NSInteger; cdecl;
    procedure setCapRadius(capRadius: CGFloat); cdecl;
    procedure setCapSegmentCount(capSegmentCount: NSInteger); cdecl;
    procedure setHeight(height: CGFloat); cdecl;
    procedure setHeightSegmentCount(heightSegmentCount: NSInteger); cdecl;
    procedure setRadialSegmentCount(radialSegmentCount: NSInteger); cdecl;
  end;
  TSCNCapsule = class(TOCGenericImport<SCNCapsuleClass, SCNCapsule>) end;

  SCNTorusClass = interface(SCNGeometryClass)
    ['{6E0B2D61-6D51-49F9-BF6B-8E3CFBF3E071}']
    {class} function torusWithRingRadius(ringRadius: CGFloat; pipeRadius: CGFloat): Pointer; cdecl;
  end;

  SCNTorus = interface(SCNGeometry)
    ['{6A895FCA-F2B7-4577-A376-FE1B2BC3E75B}']
    function pipeRadius: CGFloat; cdecl;
    function pipeSegmentCount: NSInteger; cdecl;
    function ringRadius: CGFloat; cdecl;
    function ringSegmentCount: NSInteger; cdecl;
    procedure setPipeRadius(pipeRadius: CGFloat); cdecl;
    procedure setPipeSegmentCount(pipeSegmentCount: NSInteger); cdecl;
    procedure setRingRadius(ringRadius: CGFloat); cdecl;
    procedure setRingSegmentCount(ringSegmentCount: NSInteger); cdecl;
  end;
  TSCNTorus = class(TOCGenericImport<SCNTorusClass, SCNTorus>) end;

  SCNFloorClass = interface(SCNGeometryClass)
    ['{4A570092-2FC9-43D9-8C9E-92EDF4017C5A}']
    {class} function floor: Pointer; cdecl;
  end;

  SCNFloor = interface(SCNGeometry)
    ['{6640646F-4F09-4E04-80BE-3198D772FFC7}']
    function length: CGFloat; cdecl;
    function reflectionCategoryBitMask: NSUInteger; cdecl;
    function reflectionFalloffEnd: CGFloat; cdecl;
    function reflectionFalloffStart: CGFloat; cdecl;
    function reflectionResolutionScaleFactor: CGFloat; cdecl;
    function reflectivity: CGFloat; cdecl;
    procedure setLength(length: CGFloat); cdecl;
    procedure setReflectionCategoryBitMask(reflectionCategoryBitMask: NSUInteger); cdecl;
    procedure setReflectionFalloffEnd(reflectionFalloffEnd: CGFloat); cdecl;
    procedure setReflectionFalloffStart(reflectionFalloffStart: CGFloat); cdecl;
    procedure setReflectionResolutionScaleFactor(reflectionResolutionScaleFactor: CGFloat); cdecl;
    procedure setReflectivity(reflectivity: CGFloat); cdecl;
    procedure setWidth(width: CGFloat); cdecl;
    function width: CGFloat; cdecl;
  end;
  TSCNFloor = class(TOCGenericImport<SCNFloorClass, SCNFloor>) end;

  SCNTextClass = interface(SCNGeometryClass)
    ['{02275452-5705-4363-A9E4-24E885E12EC3}']
    {class} function textWithString(&string: Pointer; extrusionDepth: CGFloat): Pointer; cdecl;
  end;

  SCNText = interface(SCNGeometry)
    ['{394F953B-16A9-486E-ABB8-F9D0F65C0A39}']
    [MethodName('string')]
    function &string: Pointer; cdecl;
    function alignmentMode: NSString; cdecl;
    function chamferProfile: UIBezierPath; cdecl;
    function chamferRadius: CGFloat; cdecl;
    function containerFrame: CGRect; cdecl;
    function extrusionDepth: CGFloat; cdecl;
    function flatness: CGFloat; cdecl;
    function font: UIFont; cdecl;
    function isWrapped: Boolean; cdecl;
    procedure setAlignmentMode(alignmentMode: NSString); cdecl;
    procedure setChamferProfile(chamferProfile: UIBezierPath); cdecl;
    procedure setChamferRadius(chamferRadius: CGFloat); cdecl;
    procedure setContainerFrame(containerFrame: CGRect); cdecl;
    procedure setExtrusionDepth(extrusionDepth: CGFloat); cdecl;
    procedure setFlatness(flatness: CGFloat); cdecl;
    procedure setFont(font: UIFont); cdecl;
    procedure setString(&string: Pointer); cdecl;
    procedure setTruncationMode(truncationMode: NSString); cdecl;
    procedure setWrapped(wrapped: Boolean); cdecl;
    function truncationMode: NSString; cdecl;
  end;
  TSCNText = class(TOCGenericImport<SCNTextClass, SCNText>) end;

  SCNShapeClass = interface(SCNGeometryClass)
    ['{CA39FD1E-9AE1-4B49-A793-B031C33E8583}']
    {class} function shapeWithPath(path: UIBezierPath; extrusionDepth: CGFloat): Pointer; cdecl;
  end;

  SCNShape = interface(SCNGeometry)
    ['{4D8562E9-E11D-43B6-9C95-A2C4310B7FAC}']
    function chamferMode: SCNChamferMode; cdecl;
    function chamferProfile: UIBezierPath; cdecl;
    function chamferRadius: CGFloat; cdecl;
    function extrusionDepth: CGFloat; cdecl;
    function path: UIBezierPath; cdecl;
    procedure setChamferMode(chamferMode: SCNChamferMode); cdecl;
    procedure setChamferProfile(chamferProfile: UIBezierPath); cdecl;
    procedure setChamferRadius(chamferRadius: CGFloat); cdecl;
    procedure setExtrusionDepth(extrusionDepth: CGFloat); cdecl;
    procedure setPath(path: UIBezierPath); cdecl;
  end;
  TSCNShape = class(TOCGenericImport<SCNShapeClass, SCNShape>) end;

  SCNTransactionClass = interface(NSObjectClass)
    ['{1CDE1472-36B3-46BD-B2D9-3D9F1C0C225C}']
    [MethodName('begin')]
    {class} procedure &begin; cdecl;
    {class} function animationDuration: CFTimeInterval; cdecl;
    {class} function animationTimingFunction: CAMediaTimingFunction; cdecl;
    {class} procedure commit; cdecl;
    {class} function completionBlock: TSCNTransactionBlockMethod1; cdecl;
    {class} function disableActions: Boolean; cdecl;
    {class} procedure flush; cdecl;
    {class} procedure lock; cdecl;
    {class} procedure setAnimationDuration(animationDuration: CFTimeInterval); cdecl;
    {class} procedure setAnimationTimingFunction(animationTimingFunction: CAMediaTimingFunction); cdecl;
    {class} procedure setCompletionBlock(completionBlock: TSCNTransactionBlockMethod1); cdecl;
    {class} procedure setDisableActions(disableActions: Boolean); cdecl;
    {class} procedure setValue(value: Pointer; forKey: NSString); cdecl;
    {class} procedure unlock; cdecl;
    {class} function valueForKey(key: NSString): Pointer; cdecl;
  end;

  SCNTransaction = interface(NSObject)
    ['{E466D782-0BC2-4D07-814E-6E2C82BD76C5}']
  end;
  TSCNTransaction = class(TOCGenericImport<SCNTransactionClass, SCNTransaction>) end;

  SCNMorpherClass = interface(NSObjectClass)
    ['{187359E3-1ECD-4083-9C93-34AFD28DDAF0}']
  end;

  SCNMorpher = interface(NSObject)
    ['{62670850-6660-4193-B88A-B4E4D870B354}']
    function calculationMode: SCNMorpherCalculationMode; cdecl;
    procedure setCalculationMode(calculationMode: SCNMorpherCalculationMode); cdecl;
    procedure setTargets(targets: NSArray); cdecl;
    procedure setUnifiesNormals(unifiesNormals: Boolean); cdecl;
    procedure setWeight(weight: CGFloat; forTargetNamed: NSString); overload; cdecl;
    procedure setWeight(weight: CGFloat; forTargetAtIndex: NSUInteger); overload; cdecl;
    procedure setWeights(weights: NSArray); cdecl;
    function targets: NSArray; cdecl;
    function unifiesNormals: Boolean; cdecl;
    function weightForTargetAtIndex(targetIndex: NSUInteger): CGFloat; cdecl;
    function weightForTargetNamed(targetName: NSString): CGFloat; cdecl;
    function weights: NSArray; cdecl;
  end;
  TSCNMorpher = class(TOCGenericImport<SCNMorpherClass, SCNMorpher>) end;

  SCNSkinnerClass = interface(NSObjectClass)
    ['{FAC5ADC1-4FC6-42A4-AE0B-51EDFA6B4C10}']
    {class} function skinnerWithBaseGeometry(baseGeometry: SCNGeometry; bones: NSArray; boneInverseBindTransforms: NSArray; boneWeights: SCNGeometrySource; boneIndices: SCNGeometrySource): Pointer; cdecl;
  end;

  SCNSkinner = interface(NSObject)
    ['{0BCD5791-EAE2-4CE2-9290-154E921D2F75}']
    function baseGeometry: SCNGeometry; cdecl;
    function baseGeometryBindTransform: SCNMatrix4; cdecl;
    function boneIndices: SCNGeometrySource; cdecl;
    function boneInverseBindTransforms: NSArray; cdecl;
    function bones: NSArray; cdecl;
    function boneWeights: SCNGeometrySource; cdecl;
    procedure setBaseGeometry(baseGeometry: SCNGeometry); cdecl;
    procedure setBaseGeometryBindTransform(baseGeometryBindTransform: SCNMatrix4); cdecl;
    procedure setSkeleton(skeleton: SCNNode); cdecl;
    function skeleton: SCNNode; cdecl;
  end;
  TSCNSkinner = class(TOCGenericImport<SCNSkinnerClass, SCNSkinner>) end;

  SCNConstraintClass = interface(NSObjectClass)
    ['{D98AE5D4-F6FB-4DDA-B479-D2E50CE85FBA}']
  end;

  SCNConstraint = interface(NSObject)
    ['{E67F2624-F87C-46D5-9E80-FA1C12CFDF4A}']
    function influenceFactor: CGFloat; cdecl;
    function isEnabled: Boolean; cdecl;
    function isIncremental: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setIncremental(incremental: Boolean); cdecl;
    procedure setInfluenceFactor(influenceFactor: CGFloat); cdecl;
  end;
  TSCNConstraint = class(TOCGenericImport<SCNConstraintClass, SCNConstraint>) end;

  SCNLookAtConstraintClass = interface(SCNConstraintClass)
    ['{C81ACEB8-EDFC-43AF-B536-9DEA7199E894}']
    {class} function lookAtConstraintWithTarget(target: SCNNode): Pointer; cdecl;
  end;

  SCNLookAtConstraint = interface(SCNConstraint)
    ['{245E6928-916C-4F60-AB42-048A37959741}']
    function gimbalLockEnabled: Boolean; cdecl;
    function localFront: SCNVector3; cdecl;
    procedure setGimbalLockEnabled(gimbalLockEnabled: Boolean); cdecl;
    procedure setLocalFront(localFront: SCNVector3); cdecl;
    procedure setTarget(target: SCNNode); cdecl;
    procedure setTargetOffset(targetOffset: SCNVector3); cdecl;
    procedure setWorldUp(worldUp: SCNVector3); cdecl;
    function target: SCNNode; cdecl;
    function targetOffset: SCNVector3; cdecl;
    function worldUp: SCNVector3; cdecl;
  end;
  TSCNLookAtConstraint = class(TOCGenericImport<SCNLookAtConstraintClass, SCNLookAtConstraint>) end;

  SCNBillboardConstraintClass = interface(SCNConstraintClass)
    ['{16374279-592A-4616-8A39-A3F0CAC2645E}']
    {class} function billboardConstraint: Pointer; cdecl;
  end;

  SCNBillboardConstraint = interface(SCNConstraint)
    ['{7858198E-2598-4D55-A0DF-352E2CF6EDE1}']
    function freeAxes: SCNBillboardAxis; cdecl;
    procedure setFreeAxes(freeAxes: SCNBillboardAxis); cdecl;
  end;
  TSCNBillboardConstraint = class(TOCGenericImport<SCNBillboardConstraintClass, SCNBillboardConstraint>) end;

  SCNTransformConstraintClass = interface(SCNConstraintClass)
    ['{7EBF642B-029E-450B-90E9-C2FF89109890}']
    {class} function orientationConstraintInWorldSpace(world: Boolean; withBlock: TSCNTransformConstraintBlockMethod3): Pointer; cdecl;
    {class} function positionConstraintInWorldSpace(world: Boolean; withBlock: TSCNTransformConstraintBlockMethod2): Pointer; cdecl;
    {class} function transformConstraintInWorldSpace(world: Boolean; withBlock: TSCNTransformConstraintBlockMethod1): Pointer; cdecl;
  end;

  SCNTransformConstraint = interface(SCNConstraint)
    ['{4C556C3E-1AEC-4313-88E5-F3D2CE9391B5}']
  end;
  TSCNTransformConstraint = class(TOCGenericImport<SCNTransformConstraintClass, SCNTransformConstraint>) end;

  SCNIKConstraintClass = interface(SCNConstraintClass)
    ['{EFF6DDB3-C9EB-4782-83FB-F7A486B94672}']
    {class} function inverseKinematicsConstraintWithChainRootNode(chainRootNode: SCNNode): Pointer; cdecl;
  end;

  SCNIKConstraint = interface(SCNConstraint)
    ['{634F54E2-3356-4B98-9FE3-FE1D5E036E5D}']
    function chainRootNode: SCNNode; cdecl;
    function initWithChainRootNode(chainRootNode: SCNNode): Pointer; cdecl;
    function maxAllowedRotationAngleForJoint(node: SCNNode): CGFloat; cdecl;
    procedure setMaxAllowedRotationAngle(angle: CGFloat; forJoint: SCNNode); cdecl;
    procedure setTargetPosition(targetPosition: SCNVector3); cdecl;
    function targetPosition: SCNVector3; cdecl;
  end;
  TSCNIKConstraint = class(TOCGenericImport<SCNIKConstraintClass, SCNIKConstraint>) end;

  SCNDistanceConstraintClass = interface(SCNConstraintClass)
    ['{60D54B52-F4CF-47F0-A4EC-B268459D3ECA}']
    {class} function distanceConstraintWithTarget(target: SCNNode): Pointer; cdecl;
  end;

  SCNDistanceConstraint = interface(SCNConstraint)
    ['{AB5AC96C-322A-4512-87FF-886B0EA6C518}']
    function maximumDistance: CGFloat; cdecl;
    function minimumDistance: CGFloat; cdecl;
    procedure setMaximumDistance(maximumDistance: CGFloat); cdecl;
    procedure setMinimumDistance(minimumDistance: CGFloat); cdecl;
    procedure setTarget(target: SCNNode); cdecl;
    function target: SCNNode; cdecl;
  end;
  TSCNDistanceConstraint = class(TOCGenericImport<SCNDistanceConstraintClass, SCNDistanceConstraint>) end;

  SCNReplicatorConstraintClass = interface(SCNConstraintClass)
    ['{239A5B4B-E957-4AB4-9477-3F668429A536}']
    {class} function replicatorConstraintWithTarget(target: SCNNode): Pointer; cdecl;
  end;

  SCNReplicatorConstraint = interface(SCNConstraint)
    ['{42E945E2-9C3E-4546-8201-79BBAEC2505D}']
    function orientationOffset: SCNQuaternion; cdecl;
    function positionOffset: SCNVector3; cdecl;
    function replicatesOrientation: Boolean; cdecl;
    function replicatesPosition: Boolean; cdecl;
    function replicatesScale: Boolean; cdecl;
    function scaleOffset: SCNVector3; cdecl;
    procedure setOrientationOffset(orientationOffset: SCNQuaternion); cdecl;
    procedure setPositionOffset(positionOffset: SCNVector3); cdecl;
    procedure setReplicatesOrientation(replicatesOrientation: Boolean); cdecl;
    procedure setReplicatesPosition(replicatesPosition: Boolean); cdecl;
    procedure setReplicatesScale(replicatesScale: Boolean); cdecl;
    procedure setScaleOffset(scaleOffset: SCNVector3); cdecl;
    procedure setTarget(target: SCNNode); cdecl;
    function target: SCNNode; cdecl;
  end;
  TSCNReplicatorConstraint = class(TOCGenericImport<SCNReplicatorConstraintClass, SCNReplicatorConstraint>) end;

  SCNAccelerationConstraintClass = interface(SCNConstraintClass)
    ['{3876BA66-D8DD-498D-8554-D07A2AE78F38}']
    {class} function accelerationConstraint: Pointer; cdecl;
  end;

  SCNAccelerationConstraint = interface(SCNConstraint)
    ['{9A7F25F4-B71B-4BC8-AF8D-535DA4DC7FA1}']
    function damping: CGFloat; cdecl;
    function decelerationDistance: CGFloat; cdecl;
    function maximumLinearAcceleration: CGFloat; cdecl;
    function maximumLinearVelocity: CGFloat; cdecl;
    procedure setDamping(damping: CGFloat); cdecl;
    procedure setDecelerationDistance(decelerationDistance: CGFloat); cdecl;
    procedure setMaximumLinearAcceleration(maximumLinearAcceleration: CGFloat); cdecl;
    procedure setMaximumLinearVelocity(maximumLinearVelocity: CGFloat); cdecl;
  end;
  TSCNAccelerationConstraint = class(TOCGenericImport<SCNAccelerationConstraintClass, SCNAccelerationConstraint>) end;

  SCNSliderConstraintClass = interface(SCNConstraintClass)
    ['{4750E3B9-D9B7-4283-BF7D-2B355D0049B7}']
    {class} function sliderConstraint: Pointer; cdecl;
  end;

  SCNSliderConstraint = interface(SCNConstraint)
    ['{F557738A-F06D-45BA-A6A5-DC1DB311EAC9}']
    function collisionCategoryBitMask: NSUInteger; cdecl;
    function offset: SCNVector3; cdecl;
    function radius: CGFloat; cdecl;
    procedure setCollisionCategoryBitMask(collisionCategoryBitMask: NSUInteger); cdecl;
    procedure setOffset(offset: SCNVector3); cdecl;
    procedure setRadius(radius: CGFloat); cdecl;
  end;
  TSCNSliderConstraint = class(TOCGenericImport<SCNSliderConstraintClass, SCNSliderConstraint>) end;

  SCNAvoidOccluderConstraintDelegate = interface(IObjectiveC)
    ['{D6857A63-B56C-4C78-A66B-76D85041A7AE}']
    [MethodName('avoidOccluderConstraint:didAvoidOccluder:forNode:')]
    procedure avoidOccluderConstraintDidAvoidOccluder(constraint: SCNAvoidOccluderConstraint; didAvoidOccluder: SCNNode; forNode: SCNNode); cdecl;
    [MethodName('avoidOccluderConstraint:shouldAvoidOccluder:forNode:')]
    function avoidOccluderConstraintShouldAvoidOccluder(constraint: SCNAvoidOccluderConstraint; shouldAvoidOccluder: SCNNode; forNode: SCNNode): Boolean; cdecl;
  end;

  SCNAvoidOccluderConstraintClass = interface(SCNConstraintClass)
    ['{F9AED7BA-57DF-4DE3-ADD3-47BDE2C511E9}']
    {class} function avoidOccluderConstraintWithTarget(target: SCNNode): Pointer; cdecl;
  end;

  SCNAvoidOccluderConstraint = interface(SCNConstraint)
    ['{CA631110-C473-457A-8DD6-A47D2D59995C}']
    function bias: CGFloat; cdecl;
    function delegate: Pointer; cdecl;
    function occluderCategoryBitMask: NSUInteger; cdecl;
    procedure setBias(bias: CGFloat); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setOccluderCategoryBitMask(occluderCategoryBitMask: NSUInteger); cdecl;
    procedure setTarget(target: SCNNode); cdecl;
    function target: SCNNode; cdecl;
  end;
  TSCNAvoidOccluderConstraint = class(TOCGenericImport<SCNAvoidOccluderConstraintClass, SCNAvoidOccluderConstraint>) end;

  SCNLevelOfDetailClass = interface(NSObjectClass)
    ['{9B4F2886-7639-4084-91F5-D622F9A11A39}']
    [MethodName('levelOfDetailWithGeometry:screenSpaceRadius:')]
    {class} function levelOfDetailWithGeometryScreenSpaceRadius(geometry: SCNGeometry; screenSpaceRadius: CGFloat): Pointer; cdecl;
    [MethodName('levelOfDetailWithGeometry:worldSpaceDistance:')]
    {class} function levelOfDetailWithGeometryWorldSpaceDistance(geometry: SCNGeometry; worldSpaceDistance: CGFloat): Pointer; cdecl;
  end;

  SCNLevelOfDetail = interface(NSObject)
    ['{F3B97421-CA02-4F08-B740-6560546B0461}']
    function geometry: SCNGeometry; cdecl;
    function screenSpaceRadius: CGFloat; cdecl;
    function worldSpaceDistance: CGFloat; cdecl;
  end;
  TSCNLevelOfDetail = class(TOCGenericImport<SCNLevelOfDetailClass, SCNLevelOfDetail>) end;

  SCNParticlePropertyControllerClass = interface(NSObjectClass)
    ['{1877ADD3-0DCC-4EC2-9EE4-227B4C77ED81}']
    {class} function controllerWithAnimation(animation: CAAnimation): Pointer; cdecl;
  end;

  SCNParticlePropertyController = interface(NSObject)
    ['{146711BB-8601-43B6-ADAE-050AB5705DAB}']
    function animation: CAAnimation; cdecl;
    function inputBias: CGFloat; cdecl;
    function inputMode: SCNParticleInputMode; cdecl;
    function inputOrigin: SCNNode; cdecl;
    function inputProperty: SCNParticleProperty; cdecl;
    function inputScale: CGFloat; cdecl;
    procedure setAnimation(animation: CAAnimation); cdecl;
    procedure setInputBias(inputBias: CGFloat); cdecl;
    procedure setInputMode(inputMode: SCNParticleInputMode); cdecl;
    procedure setInputOrigin(inputOrigin: SCNNode); cdecl;
    procedure setInputProperty(inputProperty: SCNParticleProperty); cdecl;
    procedure setInputScale(inputScale: CGFloat); cdecl;
  end;
  TSCNParticlePropertyController = class(TOCGenericImport<SCNParticlePropertyControllerClass, SCNParticlePropertyController>) end;

  SCNParticleSystemClass = interface(NSObjectClass)
    ['{B3E0F180-D560-49FE-9F9E-B9E9E68ABA27}']
    {class} function particleSystem: Pointer; cdecl;
    {class} function particleSystemNamed(name: NSString; inDirectory: NSString): Pointer; cdecl;
  end;

  SCNParticleSystem = interface(NSObject)
    ['{A52ADA03-8309-41F4-8331-4603209639BD}']
    function acceleration: SCNVector3; cdecl;
    procedure addModifierForProperties(properties: NSArray; atStage: SCNParticleModifierStage; withBlock: SCNParticleModifierBlock); cdecl;
    function affectedByGravity: Boolean; cdecl;
    function affectedByPhysicsFields: Boolean; cdecl;
    function birthDirection: SCNParticleBirthDirection; cdecl;
    function birthLocation: SCNParticleBirthLocation; cdecl;
    function birthRate: CGFloat; cdecl;
    function birthRateVariation: CGFloat; cdecl;
    function blendMode: SCNParticleBlendMode; cdecl;
    function colliderNodes: NSArray; cdecl;
    function dampingFactor: CGFloat; cdecl;
    function emissionDuration: CGFloat; cdecl;
    function emissionDurationVariation: CGFloat; cdecl;
    function emitterShape: SCNGeometry; cdecl;
    function emittingDirection: SCNVector3; cdecl;
    function fresnelExponent: CGFloat; cdecl;
    procedure handleEvent(event: SCNParticleEvent; forProperties: NSArray; withBlock: SCNParticleEventBlock); cdecl;
    function idleDuration: CGFloat; cdecl;
    function idleDurationVariation: CGFloat; cdecl;
    function imageSequenceAnimationMode: SCNParticleImageSequenceAnimationMode; cdecl;
    function imageSequenceColumnCount: NSUInteger; cdecl;
    function imageSequenceFrameRate: CGFloat; cdecl;
    function imageSequenceFrameRateVariation: CGFloat; cdecl;
    function imageSequenceInitialFrame: CGFloat; cdecl;
    function imageSequenceInitialFrameVariation: CGFloat; cdecl;
    function imageSequenceRowCount: NSUInteger; cdecl;
    function isBlackPassEnabled: Boolean; cdecl;
    function isLightingEnabled: Boolean; cdecl;
    function isLocal: Boolean; cdecl;
    function loops: Boolean; cdecl;
    function orientationDirection: SCNVector3; cdecl;
    function orientationMode: SCNParticleOrientationMode; cdecl;
    function particleAngle: CGFloat; cdecl;
    function particleAngleVariation: CGFloat; cdecl;
    function particleAngularVelocity: CGFloat; cdecl;
    function particleAngularVelocityVariation: CGFloat; cdecl;
    function particleBounce: CGFloat; cdecl;
    function particleBounceVariation: CGFloat; cdecl;
    function particleCharge: CGFloat; cdecl;
    function particleChargeVariation: CGFloat; cdecl;
    function particleColor: UIColor; cdecl;
    function particleColorVariation: SCNVector4; cdecl;
    function particleDiesOnCollision: Boolean; cdecl;
    function particleFriction: CGFloat; cdecl;
    function particleFrictionVariation: CGFloat; cdecl;
    function particleImage: Pointer; cdecl;
    function particleIntensity: CGFloat; cdecl;
    function particleIntensityVariation: CGFloat; cdecl;
    function particleLifeSpan: CGFloat; cdecl;
    function particleLifeSpanVariation: CGFloat; cdecl;
    function particleMass: CGFloat; cdecl;
    function particleMassVariation: CGFloat; cdecl;
    function particleSize: CGFloat; cdecl;
    function particleSizeVariation: CGFloat; cdecl;
    function particleVelocity: CGFloat; cdecl;
    function particleVelocityVariation: CGFloat; cdecl;
    function propertyControllers: NSDictionary; cdecl;
    procedure removeAllModifiers; cdecl;
    procedure removeModifiersOfStage(stage: SCNParticleModifierStage); cdecl;
    procedure reset; cdecl;
    procedure setAcceleration(acceleration: SCNVector3); cdecl;
    procedure setAffectedByGravity(affectedByGravity: Boolean); cdecl;
    procedure setAffectedByPhysicsFields(affectedByPhysicsFields: Boolean); cdecl;
    procedure setBirthDirection(birthDirection: SCNParticleBirthDirection); cdecl;
    procedure setBirthLocation(birthLocation: SCNParticleBirthLocation); cdecl;
    procedure setBirthRate(birthRate: CGFloat); cdecl;
    procedure setBirthRateVariation(birthRateVariation: CGFloat); cdecl;
    procedure setBlackPassEnabled(blackPassEnabled: Boolean); cdecl;
    procedure setBlendMode(blendMode: SCNParticleBlendMode); cdecl;
    procedure setColliderNodes(colliderNodes: NSArray); cdecl;
    procedure setDampingFactor(dampingFactor: CGFloat); cdecl;
    procedure setEmissionDuration(emissionDuration: CGFloat); cdecl;
    procedure setEmissionDurationVariation(emissionDurationVariation: CGFloat); cdecl;
    procedure setEmitterShape(emitterShape: SCNGeometry); cdecl;
    procedure setEmittingDirection(emittingDirection: SCNVector3); cdecl;
    procedure setFresnelExponent(fresnelExponent: CGFloat); cdecl;
    procedure setIdleDuration(idleDuration: CGFloat); cdecl;
    procedure setIdleDurationVariation(idleDurationVariation: CGFloat); cdecl;
    procedure setImageSequenceAnimationMode(imageSequenceAnimationMode: SCNParticleImageSequenceAnimationMode); cdecl;
    procedure setImageSequenceColumnCount(imageSequenceColumnCount: NSUInteger); cdecl;
    procedure setImageSequenceFrameRate(imageSequenceFrameRate: CGFloat); cdecl;
    procedure setImageSequenceFrameRateVariation(imageSequenceFrameRateVariation: CGFloat); cdecl;
    procedure setImageSequenceInitialFrame(imageSequenceInitialFrame: CGFloat); cdecl;
    procedure setImageSequenceInitialFrameVariation(imageSequenceInitialFrameVariation: CGFloat); cdecl;
    procedure setImageSequenceRowCount(imageSequenceRowCount: NSUInteger); cdecl;
    procedure setLightingEnabled(lightingEnabled: Boolean); cdecl;
    procedure setLocal(local: Boolean); cdecl;
    procedure setLoops(loops: Boolean); cdecl;
    procedure setOrientationDirection(orientationDirection: SCNVector3); cdecl;
    procedure setOrientationMode(orientationMode: SCNParticleOrientationMode); cdecl;
    procedure setParticleAngle(particleAngle: CGFloat); cdecl;
    procedure setParticleAngleVariation(particleAngleVariation: CGFloat); cdecl;
    procedure setParticleAngularVelocity(particleAngularVelocity: CGFloat); cdecl;
    procedure setParticleAngularVelocityVariation(particleAngularVelocityVariation: CGFloat); cdecl;
    procedure setParticleBounce(particleBounce: CGFloat); cdecl;
    procedure setParticleBounceVariation(particleBounceVariation: CGFloat); cdecl;
    procedure setParticleCharge(particleCharge: CGFloat); cdecl;
    procedure setParticleChargeVariation(particleChargeVariation: CGFloat); cdecl;
    procedure setParticleColor(particleColor: UIColor); cdecl;
    procedure setParticleColorVariation(particleColorVariation: SCNVector4); cdecl;
    procedure setParticleDiesOnCollision(particleDiesOnCollision: Boolean); cdecl;
    procedure setParticleFriction(particleFriction: CGFloat); cdecl;
    procedure setParticleFrictionVariation(particleFrictionVariation: CGFloat); cdecl;
    procedure setParticleImage(particleImage: Pointer); cdecl;
    procedure setParticleIntensity(particleIntensity: CGFloat); cdecl;
    procedure setParticleIntensityVariation(particleIntensityVariation: CGFloat); cdecl;
    procedure setParticleLifeSpan(particleLifeSpan: CGFloat); cdecl;
    procedure setParticleLifeSpanVariation(particleLifeSpanVariation: CGFloat); cdecl;
    procedure setParticleMass(particleMass: CGFloat); cdecl;
    procedure setParticleMassVariation(particleMassVariation: CGFloat); cdecl;
    procedure setParticleSize(particleSize: CGFloat); cdecl;
    procedure setParticleSizeVariation(particleSizeVariation: CGFloat); cdecl;
    procedure setParticleVelocity(particleVelocity: CGFloat); cdecl;
    procedure setParticleVelocityVariation(particleVelocityVariation: CGFloat); cdecl;
    procedure setPropertyControllers(propertyControllers: NSDictionary); cdecl;
    procedure setSortingMode(sortingMode: SCNParticleSortingMode); cdecl;
    procedure setSpeedFactor(speedFactor: CGFloat); cdecl;
    procedure setSpreadingAngle(spreadingAngle: CGFloat); cdecl;
    procedure setStretchFactor(stretchFactor: CGFloat); cdecl;
    procedure setSystemSpawnedOnCollision(systemSpawnedOnCollision: SCNParticleSystem); cdecl;
    procedure setSystemSpawnedOnDying(systemSpawnedOnDying: SCNParticleSystem); cdecl;
    procedure setSystemSpawnedOnLiving(systemSpawnedOnLiving: SCNParticleSystem); cdecl;
    procedure setWarmupDuration(warmupDuration: CGFloat); cdecl;
    function sortingMode: SCNParticleSortingMode; cdecl;
    function speedFactor: CGFloat; cdecl;
    function spreadingAngle: CGFloat; cdecl;
    function stretchFactor: CGFloat; cdecl;
    function systemSpawnedOnCollision: SCNParticleSystem; cdecl;
    function systemSpawnedOnDying: SCNParticleSystem; cdecl;
    function systemSpawnedOnLiving: SCNParticleSystem; cdecl;
    function warmupDuration: CGFloat; cdecl;
  end;
  TSCNParticleSystem = class(TOCGenericImport<SCNParticleSystemClass, SCNParticleSystem>) end;

  SCNPhysicsBodyClass = interface(NSObjectClass)
    ['{AFDB91DB-736F-4F8A-8222-B55D539F7220}']
    {class} function bodyWithType(&type: SCNPhysicsBodyType; shape: SCNPhysicsShape): Pointer; cdecl;
    {class} function dynamicBody: Pointer; cdecl;
    {class} function kinematicBody: Pointer; cdecl;
    {class} function staticBody: Pointer; cdecl;
  end;

  SCNPhysicsBody = interface(NSObject)
    ['{D14E097E-1C6A-4ACB-A1F1-2BD032832B5D}']
    [MethodName('type')]
    function &type: SCNPhysicsBodyType; cdecl;
    function allowsResting: Boolean; cdecl;
    function angularDamping: CGFloat; cdecl;
    function angularRestingThreshold: CGFloat; cdecl;
    function angularVelocity: SCNVector4; cdecl;
    function angularVelocityFactor: SCNVector3; cdecl;
    procedure applyForce(direction: SCNVector3; impulse: Boolean); overload; cdecl;
    procedure applyForce(direction: SCNVector3; atPosition: SCNVector3; impulse: Boolean); overload; cdecl;
    procedure applyTorque(torque: SCNVector4; impulse: Boolean); cdecl;
    function categoryBitMask: NSUInteger; cdecl;
    function centerOfMassOffset: SCNVector3; cdecl;
    function charge: CGFloat; cdecl;
    procedure clearAllForces; cdecl;
    function collisionBitMask: NSUInteger; cdecl;
    function contactTestBitMask: NSUInteger; cdecl;
    function continuousCollisionDetectionThreshold: CGFloat; cdecl;
    function damping: CGFloat; cdecl;
    function friction: CGFloat; cdecl;
    function isAffectedByGravity: Boolean; cdecl;
    function isResting: Boolean; cdecl;
    function linearRestingThreshold: CGFloat; cdecl;
    function mass: CGFloat; cdecl;
    function momentOfInertia: SCNVector3; cdecl;
    function physicsShape: SCNPhysicsShape; cdecl;
    procedure resetTransform; cdecl;
    function restitution: CGFloat; cdecl;
    function rollingFriction: CGFloat; cdecl;
    procedure setAffectedByGravity(affectedByGravity: Boolean); cdecl;
    procedure setAllowsResting(allowsResting: Boolean); cdecl;
    procedure setAngularDamping(angularDamping: CGFloat); cdecl;
    procedure setAngularRestingThreshold(angularRestingThreshold: CGFloat); cdecl;
    procedure setAngularVelocity(angularVelocity: SCNVector4); cdecl;
    procedure setAngularVelocityFactor(angularVelocityFactor: SCNVector3); cdecl;
    procedure setCategoryBitMask(categoryBitMask: NSUInteger); cdecl;
    procedure setCenterOfMassOffset(centerOfMassOffset: SCNVector3); cdecl;
    procedure setCharge(charge: CGFloat); cdecl;
    procedure setCollisionBitMask(collisionBitMask: NSUInteger); cdecl;
    procedure setContactTestBitMask(contactTestBitMask: NSUInteger); cdecl;
    procedure setContinuousCollisionDetectionThreshold(continuousCollisionDetectionThreshold: CGFloat); cdecl;
    procedure setDamping(damping: CGFloat); cdecl;
    procedure setFriction(friction: CGFloat); cdecl;
    procedure setLinearRestingThreshold(linearRestingThreshold: CGFloat); cdecl;
    procedure setMass(mass: CGFloat); cdecl;
    procedure setMomentOfInertia(momentOfInertia: SCNVector3); cdecl;
    procedure setPhysicsShape(physicsShape: SCNPhysicsShape); cdecl;
    procedure setResting(resting: Boolean); cdecl;
    procedure setRestitution(restitution: CGFloat); cdecl;
    procedure setRollingFriction(rollingFriction: CGFloat); cdecl;
    procedure setType(&type: SCNPhysicsBodyType); cdecl;
    procedure setUsesDefaultMomentOfInertia(usesDefaultMomentOfInertia: Boolean); cdecl;
    procedure setVelocity(velocity: SCNVector3); cdecl;
    procedure setVelocityFactor(velocityFactor: SCNVector3); cdecl;
    function usesDefaultMomentOfInertia: Boolean; cdecl;
    function velocity: SCNVector3; cdecl;
    function velocityFactor: SCNVector3; cdecl;
  end;
  TSCNPhysicsBody = class(TOCGenericImport<SCNPhysicsBodyClass, SCNPhysicsBody>) end;

  SCNPhysicsFieldClass = interface(NSObjectClass)
    ['{083BE30C-3B6F-47F7-8327-0617C0BC3D05}']
    {class} function customFieldWithEvaluationBlock(block: SCNFieldForceEvaluator): SCNPhysicsField; cdecl;
    {class} function dragField: SCNPhysicsField; cdecl;
    {class} function electricField: SCNPhysicsField; cdecl;
    {class} function linearGravityField: SCNPhysicsField; cdecl;
    {class} function magneticField: SCNPhysicsField; cdecl;
    {class} function noiseFieldWithSmoothness(smoothness: CGFloat; animationSpeed: CGFloat): SCNPhysicsField; cdecl;
    {class} function radialGravityField: SCNPhysicsField; cdecl;
    {class} function springField: SCNPhysicsField; cdecl;
    {class} function turbulenceFieldWithSmoothness(smoothness: CGFloat; animationSpeed: CGFloat): SCNPhysicsField; cdecl;
    {class} function vortexField: SCNPhysicsField; cdecl;
  end;

  SCNPhysicsField = interface(NSObject)
    ['{A833DECA-6D44-4923-92D1-4753F3D4A67D}']
    function categoryBitMask: NSUInteger; cdecl;
    function direction: SCNVector3; cdecl;
    function falloffExponent: CGFloat; cdecl;
    function halfExtent: SCNVector3; cdecl;
    function isActive: Boolean; cdecl;
    function isExclusive: Boolean; cdecl;
    function minimumDistance: CGFloat; cdecl;
    function offset: SCNVector3; cdecl;
    function scope: SCNPhysicsFieldScope; cdecl;
    procedure setActive(active: Boolean); cdecl;
    procedure setCategoryBitMask(categoryBitMask: NSUInteger); cdecl;
    procedure setDirection(direction: SCNVector3); cdecl;
    procedure setExclusive(exclusive: Boolean); cdecl;
    procedure setFalloffExponent(falloffExponent: CGFloat); cdecl;
    procedure setHalfExtent(halfExtent: SCNVector3); cdecl;
    procedure setMinimumDistance(minimumDistance: CGFloat); cdecl;
    procedure setOffset(offset: SCNVector3); cdecl;
    procedure setScope(scope: SCNPhysicsFieldScope); cdecl;
    procedure setStrength(strength: CGFloat); cdecl;
    procedure setUsesEllipsoidalExtent(usesEllipsoidalExtent: Boolean); cdecl;
    function strength: CGFloat; cdecl;
    function usesEllipsoidalExtent: Boolean; cdecl;
  end;
  TSCNPhysicsField = class(TOCGenericImport<SCNPhysicsFieldClass, SCNPhysicsField>) end;

  SCNPhysicsShapeClass = interface(NSObjectClass)
    ['{2DED5592-094C-46F5-B0DB-DB1350609B02}']
    {class} function shapeWithGeometry(geometry: SCNGeometry; options: NSDictionary): Pointer; cdecl;
    {class} function shapeWithNode(node: SCNNode; options: NSDictionary): Pointer; cdecl;
    {class} function shapeWithShapes(shapes: NSArray; transforms: NSArray): Pointer; cdecl;
  end;

  SCNPhysicsShape = interface(NSObject)
    ['{962AAE8F-D08F-479B-9174-9689B509388B}']
    function options: NSDictionary; cdecl;
    function sourceObject: Pointer; cdecl;
    function transforms: NSArray; cdecl;
  end;
  TSCNPhysicsShape = class(TOCGenericImport<SCNPhysicsShapeClass, SCNPhysicsShape>) end;

  SCNPhysicsContactDelegate = interface(IObjectiveC)
    ['{637798E6-FEB5-461E-99F3-64C5BD534D4E}']
    [MethodName('physicsWorld:didBeginContact:')]
    procedure physicsWorldDidBeginContact(world: SCNPhysicsWorld; didBeginContact: SCNPhysicsContact); cdecl;
    [MethodName('physicsWorld:didEndContact:')]
    procedure physicsWorldDidEndContact(world: SCNPhysicsWorld; didEndContact: SCNPhysicsContact); cdecl;
    [MethodName('physicsWorld:didUpdateContact:')]
    procedure physicsWorldDidUpdateContact(world: SCNPhysicsWorld; didUpdateContact: SCNPhysicsContact); cdecl;
  end;

  SCNPhysicsWorldClass = interface(NSObjectClass)
    ['{057DFB83-339E-4E1A-BCA4-5F4960CE6A9C}']
  end;

  SCNPhysicsWorld = interface(NSObject)
    ['{DF494D2C-D1AA-4E4F-B162-A2D2D041E91A}']
    procedure addBehavior(behavior: SCNPhysicsBehavior); cdecl;
    function allBehaviors: NSArray; cdecl;
    function contactDelegate: Pointer; cdecl;
    function contactTestBetweenBody(bodyA: SCNPhysicsBody; andBody: SCNPhysicsBody; options: NSDictionary): NSArray; cdecl;
    function contactTestWithBody(body: SCNPhysicsBody; options: NSDictionary): NSArray; cdecl;
    function convexSweepTestWithShape(shape: SCNPhysicsShape; fromTransform: SCNMatrix4; toTransform: SCNMatrix4; options: NSDictionary): NSArray; cdecl;
    function gravity: SCNVector3; cdecl;
    function rayTestWithSegmentFromPoint(origin: SCNVector3; toPoint: SCNVector3; options: NSDictionary): NSArray; cdecl;
    procedure removeAllBehaviors; cdecl;
    procedure removeBehavior(behavior: SCNPhysicsBehavior); cdecl;
    procedure setContactDelegate(contactDelegate: Pointer); cdecl;
    procedure setGravity(gravity: SCNVector3); cdecl;
    procedure setSpeed(speed: CGFloat); cdecl;
    procedure setTimeStep(timeStep: NSTimeInterval); cdecl;
    function speed: CGFloat; cdecl;
    function timeStep: NSTimeInterval; cdecl;
    procedure updateCollisionPairs; cdecl;
  end;
  TSCNPhysicsWorld = class(TOCGenericImport<SCNPhysicsWorldClass, SCNPhysicsWorld>) end;

  SCNPhysicsContactClass = interface(NSObjectClass)
    ['{A8045D9C-0A90-4520-85AD-ABCE2410A800}']
  end;

  SCNPhysicsContact = interface(NSObject)
    ['{59605E53-7A51-44DE-833D-67061E0E0DF9}']
    function collisionImpulse: CGFloat; cdecl;
    function contactNormal: SCNVector3; cdecl;
    function contactPoint: SCNVector3; cdecl;
    function nodeA: SCNNode; cdecl;
    function nodeB: SCNNode; cdecl;
    function penetrationDistance: CGFloat; cdecl;
    function sweepTestFraction: CGFloat; cdecl;
  end;
  TSCNPhysicsContact = class(TOCGenericImport<SCNPhysicsContactClass, SCNPhysicsContact>) end;

  SCNPhysicsBehaviorClass = interface(NSObjectClass)
    ['{1E34704E-F6AF-4C89-A2A1-A55DDD9C08E6}']
  end;

  SCNPhysicsBehavior = interface(NSObject)
    ['{FE1D72EB-2847-4C3B-B3F1-97C8EC6F8B69}']
  end;
  TSCNPhysicsBehavior = class(TOCGenericImport<SCNPhysicsBehaviorClass, SCNPhysicsBehavior>) end;

  SCNPhysicsHingeJointClass = interface(SCNPhysicsBehaviorClass)
    ['{50DCF88B-59EE-4DBB-A7D2-3DED6E072571}']
    {class} function jointWithBody(body: SCNPhysicsBody; axis: SCNVector3; anchor: SCNVector3): Pointer; cdecl;
    {class} function jointWithBodyA(bodyA: SCNPhysicsBody; axisA: SCNVector3; anchorA: SCNVector3; bodyB: SCNPhysicsBody; axisB: SCNVector3; anchorB: SCNVector3): Pointer; cdecl;
  end;

  SCNPhysicsHingeJoint = interface(SCNPhysicsBehavior)
    ['{88E3C1EF-010C-4B86-9EE9-FB66538688C0}']
    function anchorA: SCNVector3; cdecl;
    function anchorB: SCNVector3; cdecl;
    function axisA: SCNVector3; cdecl;
    function axisB: SCNVector3; cdecl;
    function bodyA: SCNPhysicsBody; cdecl;
    function bodyB: SCNPhysicsBody; cdecl;
    procedure setAnchorA(anchorA: SCNVector3); cdecl;
    procedure setAnchorB(anchorB: SCNVector3); cdecl;
    procedure setAxisA(axisA: SCNVector3); cdecl;
    procedure setAxisB(axisB: SCNVector3); cdecl;
  end;
  TSCNPhysicsHingeJoint = class(TOCGenericImport<SCNPhysicsHingeJointClass, SCNPhysicsHingeJoint>) end;

  SCNPhysicsBallSocketJointClass = interface(SCNPhysicsBehaviorClass)
    ['{1DB3EDFA-9BF4-44BE-A240-88AA679527AA}']
    {class} function jointWithBody(body: SCNPhysicsBody; anchor: SCNVector3): Pointer; cdecl;
    {class} function jointWithBodyA(bodyA: SCNPhysicsBody; anchorA: SCNVector3; bodyB: SCNPhysicsBody; anchorB: SCNVector3): Pointer; cdecl;
  end;

  SCNPhysicsBallSocketJoint = interface(SCNPhysicsBehavior)
    ['{5A949BC0-1735-4205-8A14-48FAFC00924A}']
    function anchorA: SCNVector3; cdecl;
    function anchorB: SCNVector3; cdecl;
    function bodyA: SCNPhysicsBody; cdecl;
    function bodyB: SCNPhysicsBody; cdecl;
    procedure setAnchorA(anchorA: SCNVector3); cdecl;
    procedure setAnchorB(anchorB: SCNVector3); cdecl;
  end;
  TSCNPhysicsBallSocketJoint = class(TOCGenericImport<SCNPhysicsBallSocketJointClass, SCNPhysicsBallSocketJoint>) end;

  SCNPhysicsSliderJointClass = interface(SCNPhysicsBehaviorClass)
    ['{95B4A047-F3E3-4694-A595-7E58BA8A7FB2}']
    {class} function jointWithBody(body: SCNPhysicsBody; axis: SCNVector3; anchor: SCNVector3): Pointer; cdecl;
    {class} function jointWithBodyA(bodyA: SCNPhysicsBody; axisA: SCNVector3; anchorA: SCNVector3; bodyB: SCNPhysicsBody; axisB: SCNVector3; anchorB: SCNVector3): Pointer; cdecl;
  end;

  SCNPhysicsSliderJoint = interface(SCNPhysicsBehavior)
    ['{E0DF087D-17A4-4E49-AEA6-767BED08D9EE}']
    function anchorA: SCNVector3; cdecl;
    function anchorB: SCNVector3; cdecl;
    function axisA: SCNVector3; cdecl;
    function axisB: SCNVector3; cdecl;
    function bodyA: SCNPhysicsBody; cdecl;
    function bodyB: SCNPhysicsBody; cdecl;
    function maximumAngularLimit: CGFloat; cdecl;
    function maximumLinearLimit: CGFloat; cdecl;
    function minimumAngularLimit: CGFloat; cdecl;
    function minimumLinearLimit: CGFloat; cdecl;
    function motorMaximumForce: CGFloat; cdecl;
    function motorMaximumTorque: CGFloat; cdecl;
    function motorTargetAngularVelocity: CGFloat; cdecl;
    function motorTargetLinearVelocity: CGFloat; cdecl;
    procedure setAnchorA(anchorA: SCNVector3); cdecl;
    procedure setAnchorB(anchorB: SCNVector3); cdecl;
    procedure setAxisA(axisA: SCNVector3); cdecl;
    procedure setAxisB(axisB: SCNVector3); cdecl;
    procedure setMaximumAngularLimit(maximumAngularLimit: CGFloat); cdecl;
    procedure setMaximumLinearLimit(maximumLinearLimit: CGFloat); cdecl;
    procedure setMinimumAngularLimit(minimumAngularLimit: CGFloat); cdecl;
    procedure setMinimumLinearLimit(minimumLinearLimit: CGFloat); cdecl;
    procedure setMotorMaximumForce(motorMaximumForce: CGFloat); cdecl;
    procedure setMotorMaximumTorque(motorMaximumTorque: CGFloat); cdecl;
    procedure setMotorTargetAngularVelocity(motorTargetAngularVelocity: CGFloat); cdecl;
    procedure setMotorTargetLinearVelocity(motorTargetLinearVelocity: CGFloat); cdecl;
  end;
  TSCNPhysicsSliderJoint = class(TOCGenericImport<SCNPhysicsSliderJointClass, SCNPhysicsSliderJoint>) end;

  SCNPhysicsConeTwistJointClass = interface(SCNPhysicsBehaviorClass)
    ['{989DA3A9-C3CB-4F6B-9CD1-5CA8736EE107}']
    {class} function jointWithBody(body: SCNPhysicsBody; frame: SCNMatrix4): Pointer; cdecl;
    {class} function jointWithBodyA(bodyA: SCNPhysicsBody; frameA: SCNMatrix4; bodyB: SCNPhysicsBody; frameB: SCNMatrix4): Pointer; cdecl;
  end;

  SCNPhysicsConeTwistJoint = interface(SCNPhysicsBehavior)
    ['{8B224DA6-C40C-411A-B210-0B1B574601D3}']
    function bodyA: SCNPhysicsBody; cdecl;
    function bodyB: SCNPhysicsBody; cdecl;
    function frameA: SCNMatrix4; cdecl;
    function frameB: SCNMatrix4; cdecl;
    function maximumAngularLimit1: CGFloat; cdecl;
    function maximumAngularLimit2: CGFloat; cdecl;
    function maximumTwistAngle: CGFloat; cdecl;
    procedure setFrameA(frameA: SCNMatrix4); cdecl;
    procedure setFrameB(frameB: SCNMatrix4); cdecl;
    procedure setMaximumAngularLimit1(maximumAngularLimit1: CGFloat); cdecl;
    procedure setMaximumAngularLimit2(maximumAngularLimit2: CGFloat); cdecl;
    procedure setMaximumTwistAngle(maximumTwistAngle: CGFloat); cdecl;
  end;
  TSCNPhysicsConeTwistJoint = class(TOCGenericImport<SCNPhysicsConeTwistJointClass, SCNPhysicsConeTwistJoint>) end;

  SCNPhysicsVehicleWheelClass = interface(NSObjectClass)
    ['{74F6CA13-1554-4A7B-86CF-A91EC207C7DD}']
    {class} function wheelWithNode(node: SCNNode): Pointer; cdecl;
  end;

  SCNPhysicsVehicleWheel = interface(NSObject)
    ['{52DCE897-A51F-4A29-9A18-EC0F32DFD2DF}']
    function axle: SCNVector3; cdecl;
    function connectionPosition: SCNVector3; cdecl;
    function frictionSlip: CGFloat; cdecl;
    function maximumSuspensionForce: CGFloat; cdecl;
    function maximumSuspensionTravel: CGFloat; cdecl;
    function node: SCNNode; cdecl;
    function radius: CGFloat; cdecl;
    procedure setAxle(axle: SCNVector3); cdecl;
    procedure setConnectionPosition(connectionPosition: SCNVector3); cdecl;
    procedure setFrictionSlip(frictionSlip: CGFloat); cdecl;
    procedure setMaximumSuspensionForce(maximumSuspensionForce: CGFloat); cdecl;
    procedure setMaximumSuspensionTravel(maximumSuspensionTravel: CGFloat); cdecl;
    procedure setRadius(radius: CGFloat); cdecl;
    procedure setSteeringAxis(steeringAxis: SCNVector3); cdecl;
    procedure setSuspensionCompression(suspensionCompression: CGFloat); cdecl;
    procedure setSuspensionDamping(suspensionDamping: CGFloat); cdecl;
    procedure setSuspensionRestLength(suspensionRestLength: CGFloat); cdecl;
    procedure setSuspensionStiffness(suspensionStiffness: CGFloat); cdecl;
    function steeringAxis: SCNVector3; cdecl;
    function suspensionCompression: CGFloat; cdecl;
    function suspensionDamping: CGFloat; cdecl;
    function suspensionRestLength: CGFloat; cdecl;
    function suspensionStiffness: CGFloat; cdecl;
  end;
  TSCNPhysicsVehicleWheel = class(TOCGenericImport<SCNPhysicsVehicleWheelClass, SCNPhysicsVehicleWheel>) end;

  SCNPhysicsVehicleClass = interface(SCNPhysicsBehaviorClass)
    ['{B7A3AA7D-19FD-4791-AE59-C5C9189E0C20}']
    {class} function vehicleWithChassisBody(chassisBody: SCNPhysicsBody; wheels: NSArray): Pointer; cdecl;
  end;

  SCNPhysicsVehicle = interface(SCNPhysicsBehavior)
    ['{FB9C31A5-E297-4F1C-AB25-7D8D90C9EA7B}']
    procedure applyBrakingForce(value: CGFloat; forWheelAtIndex: NSInteger); cdecl;
    procedure applyEngineForce(value: CGFloat; forWheelAtIndex: NSInteger); cdecl;
    function chassisBody: SCNPhysicsBody; cdecl;
    procedure setSteeringAngle(value: CGFloat; forWheelAtIndex: NSInteger); cdecl;
    function speedInKilometersPerHour: CGFloat; cdecl;
    function wheels: NSArray; cdecl;
  end;
  TSCNPhysicsVehicle = class(TOCGenericImport<SCNPhysicsVehicleClass, SCNPhysicsVehicle>) end;

  SCNReferenceNodeClass = interface(SCNNodeClass)
    ['{594821D4-1B75-4646-B029-B760FE47AFF5}']
    {class} function referenceNodeWithURL(referenceURL: NSURL): Pointer; cdecl;
  end;

  SCNReferenceNode = interface(SCNNode)
    ['{50D6B857-36B4-4B01-AA2F-D30CC67318C2}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithURL(referenceURL: NSURL): Pointer; cdecl;
    function isLoaded: Boolean; cdecl;
    procedure load; cdecl;
    function loadingPolicy: SCNReferenceLoadingPolicy; cdecl;
    function referenceURL: NSURL; cdecl;
    procedure setLoadingPolicy(loadingPolicy: SCNReferenceLoadingPolicy); cdecl;
    procedure setReferenceURL(referenceURL: NSURL); cdecl;
    procedure unload; cdecl;
  end;
  TSCNReferenceNode = class(TOCGenericImport<SCNReferenceNodeClass, SCNReferenceNode>) end;

  SCNAudioSourceClass = interface(NSObjectClass)
    ['{8EF70C6F-9C68-425E-99F8-603D52AD030F}']
    {class} function audioSourceNamed(fileName: NSString): Pointer; cdecl;
  end;

  SCNAudioSource = interface(NSObject)
    ['{328E66D8-BCFE-4EC7-98B1-6EAEC072EB68}']
    function initWithFileNamed(name: NSString): Pointer; cdecl;
    function initWithURL(url: NSURL): Pointer; cdecl;
    function isPositional: Boolean; cdecl;
    procedure load; cdecl;
    function loops: Boolean; cdecl;
    function rate: Single; cdecl;
    function reverbBlend: Single; cdecl;
    procedure setLoops(loops: Boolean); cdecl;
    procedure setPositional(positional: Boolean); cdecl;
    procedure setRate(rate: Single); cdecl;
    procedure setReverbBlend(reverbBlend: Single); cdecl;
    procedure setShouldStream(shouldStream: Boolean); cdecl;
    procedure setVolume(volume: Single); cdecl;
    function shouldStream: Boolean; cdecl;
    function volume: Single; cdecl;
  end;
  TSCNAudioSource = class(TOCGenericImport<SCNAudioSourceClass, SCNAudioSource>) end;

  SCNAudioPlayerClass = interface(NSObjectClass)
    ['{A760ECB9-664C-47EB-B321-25ADBAFCB3A1}']
    {class} function audioPlayerWithAVAudioNode(audioNode: AVAudioNode): Pointer; cdecl;
    {class} function audioPlayerWithSource(source: SCNAudioSource): Pointer; cdecl;
  end;

  SCNAudioPlayer = interface(NSObject)
    ['{B322DB41-3EDC-476C-8E39-45640ADDF43A}']
    function audioNode: AVAudioNode; cdecl;
    function audioSource: SCNAudioSource; cdecl;
    function didFinishPlayback: TSCNAudioPlayerBlockMethod1; cdecl;
    function initWithAVAudioNode(audioNode: AVAudioNode): Pointer; cdecl;
    function initWithSource(source: SCNAudioSource): Pointer; cdecl;
    procedure setDidFinishPlayback(didFinishPlayback: TSCNAudioPlayerBlockMethod1); cdecl;
    procedure setWillStartPlayback(willStartPlayback: TSCNAudioPlayerBlockMethod1); cdecl;
    function willStartPlayback: TSCNAudioPlayerBlockMethod1; cdecl;
  end;
  TSCNAudioPlayer = class(TOCGenericImport<SCNAudioPlayerClass, SCNAudioPlayer>) end;

  SCNCameraControllerDelegate = interface(IObjectiveC)
    ['{98BF1425-96A5-4412-9487-748D7921C8A7}']
    procedure cameraInertiaDidEndForController(cameraController: SCNCameraController); cdecl;
    procedure cameraInertiaWillStartForController(cameraController: SCNCameraController); cdecl;
  end;

  SCNCameraControllerClass = interface(NSObjectClass)
    ['{30FD0D4C-3116-4492-85B5-C5DAB3DA3180}']
  end;

  SCNCameraController = interface(NSObject)
    ['{FA9E2DF1-BE4E-4CDC-91E1-6CF01140E4A8}']
    function automaticTarget: Boolean; cdecl;
    procedure beginInteraction(location: CGPoint; withViewport: CGSize); cdecl;
    procedure clearRoll; cdecl;
    procedure continueInteraction(location: CGPoint; withViewport: CGSize; sensitivity: CGFloat); cdecl;
    function delegate: Pointer; cdecl;
    procedure dollyBy(delta: Single; onScreenPoint: CGPoint; viewport: CGSize); cdecl;
    procedure dollyToTarget(delta: Single); cdecl;
    procedure endInteraction(location: CGPoint; withViewport: CGSize; velocity: CGPoint); cdecl;
    procedure frameNodes(nodes: NSArray); cdecl;
    function inertiaEnabled: Boolean; cdecl;
    function inertiaFriction: Single; cdecl;
    function interactionMode: SCNInteractionMode; cdecl;
    function isInertiaRunning: Boolean; cdecl;
    function maximumHorizontalAngle: Single; cdecl;
    function maximumVerticalAngle: Single; cdecl;
    function minimumHorizontalAngle: Single; cdecl;
    function minimumVerticalAngle: Single; cdecl;
    function pointOfView: SCNNode; cdecl;
    procedure rollAroundTarget(delta: Single); cdecl;
    procedure rollBy(delta: Single; aroundScreenPoint: CGPoint; viewport: CGSize); cdecl;
    procedure rotateByX(deltaX: Single; Y: Single); cdecl;
    procedure setAutomaticTarget(automaticTarget: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setInertiaEnabled(inertiaEnabled: Boolean); cdecl;
    procedure setInertiaFriction(inertiaFriction: Single); cdecl;
    procedure setInteractionMode(interactionMode: SCNInteractionMode); cdecl;
    procedure setMaximumHorizontalAngle(maximumHorizontalAngle: Single); cdecl;
    procedure setMaximumVerticalAngle(maximumVerticalAngle: Single); cdecl;
    procedure setMinimumHorizontalAngle(minimumHorizontalAngle: Single); cdecl;
    procedure setMinimumVerticalAngle(minimumVerticalAngle: Single); cdecl;
    procedure setPointOfView(pointOfView: SCNNode); cdecl;
    procedure setTarget(target: SCNVector3); cdecl;
    procedure setWorldUp(worldUp: SCNVector3); cdecl;
    procedure stopInertia; cdecl;
    function target: SCNVector3; cdecl;
    procedure translateInCameraSpaceByX(deltaX: Single; Y: Single; Z: Single); cdecl;
    function worldUp: SCNVector3; cdecl;
  end;
  TSCNCameraController = class(TOCGenericImport<SCNCameraControllerClass, SCNCameraController>) end;

function SCNErrorDomain: NSString;
function SCNModelTransform: NSString;
function SCNViewTransform: NSString;
function SCNProjectionTransform: NSString;
function SCNNormalTransform: NSString;
function SCNModelViewTransform: NSString;
function SCNModelViewProjectionTransform: NSString;
function SCNSceneSourceAssetContributorsKey: NSString;
function SCNSceneSourceAssetCreatedDateKey: NSString;
function SCNSceneSourceAssetModifiedDateKey: NSString;
function SCNSceneSourceAssetUpAxisKey: NSString;
function SCNSceneSourceAssetUnitKey: NSString;
function SCNSceneSourceAssetAuthoringToolKey: NSString;
function SCNSceneSourceAssetAuthorKey: NSString;
function SCNSceneSourceAssetUnitNameKey: NSString;
function SCNSceneSourceAssetUnitMeterKey: NSString;
function SCNSceneSourceCreateNormalsIfAbsentKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionCreateNormalsIfAbsent
function SCNSceneSourceCheckConsistencyKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionCheckConsistency
function SCNSceneSourceFlattenSceneKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionFlattenScene
function SCNSceneSourceUseSafeModeKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionUseSafeMode
function SCNSceneSourceAssetDirectoryURLsKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionAssetDirectoryURLs
function SCNSceneSourceOverrideAssetURLsKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionOverrideAssetURLs
function SCNSceneSourceStrictConformanceKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionStrictConformance
function SCNSceneSourceConvertUnitsToMetersKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionConvertUnitsToMeters
function SCNSceneSourceConvertToYUpKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionConvertToYUp
function SCNSceneSourceAnimationImportPolicyKey: SCNSceneSourceLoadingOption; // SCNSceneSourceLoadingOptionAnimationImportPolicy
function SCNSceneSourceLoadingOptionPreserveOriginalTopology: SCNSceneSourceLoadingOption;
function SCNSceneSourceAnimationImportPolicyPlay: SCNSceneSourceAnimationImportPolicy;
function SCNSceneSourceAnimationImportPolicyPlayRepeatedly: SCNSceneSourceAnimationImportPolicy;
function SCNSceneSourceAnimationImportPolicyDoNotPlay: SCNSceneSourceAnimationImportPolicy;
function SCNSceneSourceAnimationImportPolicyPlayUsingSceneTimeBase: SCNSceneSourceAnimationImportPolicy;
function SCNDetailedErrorsKey: NSString;
function SCNConsistencyElementIDErrorKey: NSString;
function SCNConsistencyElementTypeErrorKey: NSString;
function SCNConsistencyLineNumberErrorKey: NSString;
function SCNSceneExportDestinationURL: NSString;
function SCNSceneStartTimeAttributeKey: SCNSceneAttribute; // SCNSceneAttributeStartTime
function SCNSceneEndTimeAttributeKey: SCNSceneAttribute; // SCNSceneAttributeEndTime
function SCNSceneFrameRateAttributeKey: SCNSceneAttribute; // SCNSceneAttributeFrameRate
function SCNSceneUpAxisAttributeKey: SCNSceneAttribute; // SCNSceneUpAxisAttribute
function SCNProgramMappingChannelKey: NSString;
function SCNShaderModifierEntryPointGeometry: SCNShaderModifierEntryPoint;
function SCNShaderModifierEntryPointSurface: SCNShaderModifierEntryPoint;
function SCNShaderModifierEntryPointLightingModel: SCNShaderModifierEntryPoint;
function SCNShaderModifierEntryPointFragment: SCNShaderModifierEntryPoint;
function SCNLightTypeAmbient: SCNLightType;
function SCNLightTypeOmni: SCNLightType;
function SCNLightTypeDirectional: SCNLightType;
function SCNLightTypeSpot: SCNLightType;
function SCNLightTypeIES: SCNLightType;
function SCNLightTypeProbe: SCNLightType;
function SCNLightTypeArea: SCNLightType;
function SCNGeometrySourceSemanticVertex: SCNGeometrySourceSemantic;
function SCNGeometrySourceSemanticNormal: SCNGeometrySourceSemantic;
function SCNGeometrySourceSemanticColor: SCNGeometrySourceSemantic;
function SCNGeometrySourceSemanticTexcoord: SCNGeometrySourceSemantic;
function SCNGeometrySourceSemanticTangent: SCNGeometrySourceSemantic;
function SCNGeometrySourceSemanticVertexCrease: SCNGeometrySourceSemantic;
function SCNGeometrySourceSemanticEdgeCrease: SCNGeometrySourceSemantic;
function SCNGeometrySourceSemanticBoneWeights: SCNGeometrySourceSemantic;
function SCNGeometrySourceSemanticBoneIndices: SCNGeometrySourceSemantic;
function SCNLightingModelPhong: SCNLightingModel;
function SCNLightingModelBlinn: SCNLightingModel;
function SCNLightingModelLambert: SCNLightingModel;
function SCNLightingModelConstant: SCNLightingModel;
function SCNLightingModelPhysicallyBased: SCNLightingModel;
function SCNLightingModelShadowOnly: SCNLightingModel;
function SCNHitTestClipToZRangeKey: SCNHitTestOption; // SCNHitTestOptionClipToZRange
function SCNHitTestBackFaceCullingKey: SCNHitTestOption; // SCNHitTestOptionBackFaceCulling
function SCNHitTestBoundingBoxOnlyKey: SCNHitTestOption; // SCNHitTestOptionBoundingBoxOnly
function SCNHitTestIgnoreChildNodesKey: SCNHitTestOption; // SCNHitTestOptionIgnoreChildNodes
function SCNHitTestRootNodeKey: SCNHitTestOption; // SCNHitTestOptionRootNode
function SCNHitTestIgnoreHiddenNodesKey: SCNHitTestOption; // SCNHitTestOptionIgnoreHiddenNodes
function SCNHitTestOptionCategoryBitMask: SCNHitTestOption;
function SCNHitTestOptionSearchMode: SCNHitTestOption;
function SCNHitTestOptionIgnoreLightArea: SCNHitTestOption;
function SCNHitTestFirstFoundOnlyKey: SCNHitTestOption; // SCNHitTestOptionFirstFoundOnly
function SCNHitTestSortResultsKey: SCNHitTestOption; // SCNHitTestOptionSortResults
function SCNPreferredRenderingAPIKey: SCNViewOption; // SCNViewOptionPreferredRenderingAPI
function SCNPreferredDeviceKey: SCNViewOption; // SCNViewOptionPreferredDevice
function SCNPreferLowPowerDeviceKey: SCNViewOption; // SCNViewOptionPreferLowPowerDevice
function SCNParticlePropertyPosition: SCNParticleProperty;
function SCNParticlePropertyAngle: SCNParticleProperty;
function SCNParticlePropertyRotationAxis: SCNParticleProperty;
function SCNParticlePropertyVelocity: SCNParticleProperty;
function SCNParticlePropertyAngularVelocity: SCNParticleProperty;
function SCNParticlePropertyLife: SCNParticleProperty;
function SCNParticlePropertyColor: SCNParticleProperty;
function SCNParticlePropertyOpacity: SCNParticleProperty;
function SCNParticlePropertySize: SCNParticleProperty;
function SCNParticlePropertyFrame: SCNParticleProperty;
function SCNParticlePropertyFrameRate: SCNParticleProperty;
function SCNParticlePropertyBounce: SCNParticleProperty;
function SCNParticlePropertyCharge: SCNParticleProperty;
function SCNParticlePropertyFriction: SCNParticleProperty;
function SCNParticlePropertyContactPoint: SCNParticleProperty;
function SCNParticlePropertyContactNormal: SCNParticleProperty;
function SCNPhysicsShapeTypeKey: SCNPhysicsShapeOption; // SCNPhysicsShapeOptionType
function SCNPhysicsShapeKeepAsCompoundKey: SCNPhysicsShapeOption; // SCNPhysicsShapeOptionKeepAsCompound
function SCNPhysicsShapeScaleKey: SCNPhysicsShapeOption; // SCNPhysicsShapeOptionScale
function SCNPhysicsShapeOptionCollisionMargin: SCNPhysicsShapeOption;
function SCNPhysicsShapeTypeBoundingBox: SCNPhysicsShapeType;
function SCNPhysicsShapeTypeConvexHull: SCNPhysicsShapeType;
function SCNPhysicsShapeTypeConcavePolyhedron: SCNPhysicsShapeType;
function SCNPhysicsTestCollisionBitMaskKey: SCNPhysicsTestOption; // SCNPhysicsTestOptionCollisionBitMask
function SCNPhysicsTestSearchModeKey: SCNPhysicsTestOption; // SCNPhysicsTestOptionSearchMode
function SCNPhysicsTestBackfaceCullingKey: SCNPhysicsTestOption; // SCNPhysicsTestOptionBackfaceCulling
function SCNPhysicsTestSearchModeAny: SCNPhysicsTestSearchMode;
function SCNPhysicsTestSearchModeClosest: SCNPhysicsTestSearchMode;
function SCNPhysicsTestSearchModeAll: SCNPhysicsTestSearchMode;

const
  libSceneKit = '/System/Library/Frameworks/SceneKit.framework/SceneKit';

function SCNVector3EqualToVector3(): Integer; cdecl;
  external libSceneKit name _PU + 'SCNVector3EqualToVector3';

function SCNVector4EqualToVector4(): Integer; cdecl;
  external libSceneKit name _PU + 'SCNVector4EqualToVector4';

function SCNMatrix4IsIdentity(): Integer; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4IsIdentity';

function SCNMatrix4EqualToMatrix4(): Integer; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4EqualToMatrix4';

function SCNMatrix4MakeRotation(angle: Single; x: Single; y: Single; z: Single): SCNMatrix4; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4MakeRotation';

function SCNMatrix4Scale(m: SCNMatrix4; sx: Single; sy: Single; sz: Single): SCNMatrix4; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4Scale';

function SCNMatrix4Rotate(m: SCNMatrix4; angle: Single; x: Single; y: Single; z: Single): SCNMatrix4; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4Rotate';

function SCNMatrix4Invert(m: SCNMatrix4): SCNMatrix4; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4Invert';

function SCNMatrix4Mult(a: SCNMatrix4; b: SCNMatrix4): SCNMatrix4; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4Mult';

function SCNMatrix4ToGLKMatrix4(mat: SCNMatrix4): GLKMatrix4; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4ToGLKMatrix4';

function SCNMatrix4FromGLKMatrix4(mat: GLKMatrix4): SCNMatrix4; cdecl;
  external libSceneKit name _PU + 'SCNMatrix4FromGLKMatrix4';

procedure SCNExportJavaScriptModule(context: JSContext); cdecl;
  external libSceneKit name _PU + 'SCNExportJavaScriptModule';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  SceneKitModule: THandle;
{$ENDIF}

function SCNErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNErrorDomain');
end;

function SCNModelTransform: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNModelTransform');
end;

function SCNViewTransform: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNViewTransform');
end;

function SCNProjectionTransform: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNProjectionTransform');
end;

function SCNNormalTransform: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNNormalTransform');
end;

function SCNModelViewTransform: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNModelViewTransform');
end;

function SCNModelViewProjectionTransform: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNModelViewProjectionTransform');
end;

function SCNSceneSourceAssetContributorsKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetContributorsKey');
end;

function SCNSceneSourceAssetCreatedDateKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetCreatedDateKey');
end;

function SCNSceneSourceAssetModifiedDateKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetModifiedDateKey');
end;

function SCNSceneSourceAssetUpAxisKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetUpAxisKey');
end;

function SCNSceneSourceAssetUnitKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetUnitKey');
end;

function SCNSceneSourceAssetAuthoringToolKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetAuthoringToolKey');
end;

function SCNSceneSourceAssetAuthorKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetAuthorKey');
end;

function SCNSceneSourceAssetUnitNameKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetUnitNameKey');
end;

function SCNSceneSourceAssetUnitMeterKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetUnitMeterKey');
end;

function SCNSceneSourceCreateNormalsIfAbsentKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceCreateNormalsIfAbsentKey');
end;

function SCNSceneSourceCheckConsistencyKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceCheckConsistencyKey');
end;

function SCNSceneSourceFlattenSceneKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceFlattenSceneKey');
end;

function SCNSceneSourceUseSafeModeKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceUseSafeModeKey');
end;

function SCNSceneSourceAssetDirectoryURLsKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAssetDirectoryURLsKey');
end;

function SCNSceneSourceOverrideAssetURLsKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceOverrideAssetURLsKey');
end;

function SCNSceneSourceStrictConformanceKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceStrictConformanceKey');
end;

function SCNSceneSourceConvertUnitsToMetersKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceConvertUnitsToMetersKey');
end;

function SCNSceneSourceConvertToYUpKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceConvertToYUpKey');
end;

function SCNSceneSourceAnimationImportPolicyKey: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAnimationImportPolicyKey');
end;

function SCNSceneSourceLoadingOptionPreserveOriginalTopology: SCNSceneSourceLoadingOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceLoadingOptionPreserveOriginalTopology');
end;

function SCNSceneSourceAnimationImportPolicyPlay: SCNSceneSourceAnimationImportPolicy;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAnimationImportPolicyPlay');
end;

function SCNSceneSourceAnimationImportPolicyPlayRepeatedly: SCNSceneSourceAnimationImportPolicy;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAnimationImportPolicyPlayRepeatedly');
end;

function SCNSceneSourceAnimationImportPolicyDoNotPlay: SCNSceneSourceAnimationImportPolicy;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAnimationImportPolicyDoNotPlay');
end;

function SCNSceneSourceAnimationImportPolicyPlayUsingSceneTimeBase: SCNSceneSourceAnimationImportPolicy;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneSourceAnimationImportPolicyPlayUsingSceneTimeBase');
end;

function SCNDetailedErrorsKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNDetailedErrorsKey');
end;

function SCNConsistencyElementIDErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNConsistencyElementIDErrorKey');
end;

function SCNConsistencyElementTypeErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNConsistencyElementTypeErrorKey');
end;

function SCNConsistencyLineNumberErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNConsistencyLineNumberErrorKey');
end;

function SCNSceneExportDestinationURL: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneExportDestinationURL');
end;

function SCNSceneStartTimeAttributeKey: SCNSceneAttribute;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneStartTimeAttributeKey');
end;

function SCNSceneEndTimeAttributeKey: SCNSceneAttribute;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneEndTimeAttributeKey');
end;

function SCNSceneFrameRateAttributeKey: SCNSceneAttribute;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneFrameRateAttributeKey');
end;

function SCNSceneUpAxisAttributeKey: SCNSceneAttribute;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNSceneUpAxisAttributeKey');
end;

function SCNProgramMappingChannelKey: NSString;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNProgramMappingChannelKey');
end;

function SCNShaderModifierEntryPointGeometry: SCNShaderModifierEntryPoint;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNShaderModifierEntryPointGeometry');
end;

function SCNShaderModifierEntryPointSurface: SCNShaderModifierEntryPoint;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNShaderModifierEntryPointSurface');
end;

function SCNShaderModifierEntryPointLightingModel: SCNShaderModifierEntryPoint;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNShaderModifierEntryPointLightingModel');
end;

function SCNShaderModifierEntryPointFragment: SCNShaderModifierEntryPoint;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNShaderModifierEntryPointFragment');
end;

function SCNLightTypeAmbient: SCNLightType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightTypeAmbient');
end;

function SCNLightTypeOmni: SCNLightType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightTypeOmni');
end;

function SCNLightTypeDirectional: SCNLightType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightTypeDirectional');
end;

function SCNLightTypeSpot: SCNLightType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightTypeSpot');
end;

function SCNLightTypeIES: SCNLightType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightTypeIES');
end;

function SCNLightTypeProbe: SCNLightType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightTypeProbe');
end;

function SCNLightTypeArea: SCNLightType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightTypeArea');
end;

function SCNGeometrySourceSemanticVertex: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticVertex');
end;

function SCNGeometrySourceSemanticNormal: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticNormal');
end;

function SCNGeometrySourceSemanticColor: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticColor');
end;

function SCNGeometrySourceSemanticTexcoord: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticTexcoord');
end;

function SCNGeometrySourceSemanticTangent: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticTangent');
end;

function SCNGeometrySourceSemanticVertexCrease: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticVertexCrease');
end;

function SCNGeometrySourceSemanticEdgeCrease: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticEdgeCrease');
end;

function SCNGeometrySourceSemanticBoneWeights: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticBoneWeights');
end;

function SCNGeometrySourceSemanticBoneIndices: SCNGeometrySourceSemantic;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNGeometrySourceSemanticBoneIndices');
end;

function SCNLightingModelPhong: SCNLightingModel;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightingModelPhong');
end;

function SCNLightingModelBlinn: SCNLightingModel;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightingModelBlinn');
end;

function SCNLightingModelLambert: SCNLightingModel;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightingModelLambert');
end;

function SCNLightingModelConstant: SCNLightingModel;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightingModelConstant');
end;

function SCNLightingModelPhysicallyBased: SCNLightingModel;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightingModelPhysicallyBased');
end;

function SCNLightingModelShadowOnly: SCNLightingModel;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNLightingModelShadowOnly');
end;

function SCNHitTestClipToZRangeKey: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestClipToZRangeKey');
end;

function SCNHitTestBackFaceCullingKey: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestBackFaceCullingKey');
end;

function SCNHitTestBoundingBoxOnlyKey: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestBoundingBoxOnlyKey');
end;

function SCNHitTestIgnoreChildNodesKey: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestIgnoreChildNodesKey');
end;

function SCNHitTestRootNodeKey: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestRootNodeKey');
end;

function SCNHitTestIgnoreHiddenNodesKey: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestIgnoreHiddenNodesKey');
end;

function SCNHitTestOptionCategoryBitMask: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestOptionCategoryBitMask');
end;

function SCNHitTestOptionSearchMode: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestOptionSearchMode');
end;

function SCNHitTestOptionIgnoreLightArea: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestOptionIgnoreLightArea');
end;

function SCNHitTestFirstFoundOnlyKey: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestFirstFoundOnlyKey');
end;

function SCNHitTestSortResultsKey: SCNHitTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNHitTestSortResultsKey');
end;

function SCNPreferredRenderingAPIKey: SCNViewOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPreferredRenderingAPIKey');
end;

function SCNPreferredDeviceKey: SCNViewOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPreferredDeviceKey');
end;

function SCNPreferLowPowerDeviceKey: SCNViewOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPreferLowPowerDeviceKey');
end;

function SCNParticlePropertyPosition: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyPosition');
end;

function SCNParticlePropertyAngle: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyAngle');
end;

function SCNParticlePropertyRotationAxis: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyRotationAxis');
end;

function SCNParticlePropertyVelocity: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyVelocity');
end;

function SCNParticlePropertyAngularVelocity: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyAngularVelocity');
end;

function SCNParticlePropertyLife: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyLife');
end;

function SCNParticlePropertyColor: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyColor');
end;

function SCNParticlePropertyOpacity: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyOpacity');
end;

function SCNParticlePropertySize: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertySize');
end;

function SCNParticlePropertyFrame: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyFrame');
end;

function SCNParticlePropertyFrameRate: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyFrameRate');
end;

function SCNParticlePropertyBounce: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyBounce');
end;

function SCNParticlePropertyCharge: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyCharge');
end;

function SCNParticlePropertyFriction: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyFriction');
end;

function SCNParticlePropertyContactPoint: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyContactPoint');
end;

function SCNParticlePropertyContactNormal: SCNParticleProperty;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNParticlePropertyContactNormal');
end;

function SCNPhysicsShapeTypeKey: SCNPhysicsShapeOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsShapeTypeKey');
end;

function SCNPhysicsShapeKeepAsCompoundKey: SCNPhysicsShapeOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsShapeKeepAsCompoundKey');
end;

function SCNPhysicsShapeScaleKey: SCNPhysicsShapeOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsShapeScaleKey');
end;

function SCNPhysicsShapeOptionCollisionMargin: SCNPhysicsShapeOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsShapeOptionCollisionMargin');
end;

function SCNPhysicsShapeTypeBoundingBox: SCNPhysicsShapeType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsShapeTypeBoundingBox');
end;

function SCNPhysicsShapeTypeConvexHull: SCNPhysicsShapeType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsShapeTypeConvexHull');
end;

function SCNPhysicsShapeTypeConcavePolyhedron: SCNPhysicsShapeType;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsShapeTypeConcavePolyhedron');
end;

function SCNPhysicsTestCollisionBitMaskKey: SCNPhysicsTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsTestCollisionBitMaskKey');
end;

function SCNPhysicsTestSearchModeKey: SCNPhysicsTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsTestSearchModeKey');
end;

function SCNPhysicsTestBackfaceCullingKey: SCNPhysicsTestOption;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsTestBackfaceCullingKey');
end;

function SCNPhysicsTestSearchModeAny: SCNPhysicsTestSearchMode;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsTestSearchModeAny');
end;

function SCNPhysicsTestSearchModeClosest: SCNPhysicsTestSearchMode;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsTestSearchModeClosest');
end;

function SCNPhysicsTestSearchModeAll: SCNPhysicsTestSearchMode;
begin
  Result := CocoaNSStringConst(libSceneKit, 'SCNPhysicsTestSearchModeAll');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  SceneKitModule := dlopen(MarshaledAString(libSceneKit), RTLD_LAZY);

finalization
  dlclose(SceneKitModule)
{$ENDIF}

end.