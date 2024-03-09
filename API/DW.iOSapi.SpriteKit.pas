unit DW.iOSapi.SpriteKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.ObjCRuntime, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.CoreImage, iOSapi.GLKit,
  // DW
  DW.Macapi.Simd, DW.Macapi.Dispatch, DW.iOSapi.AVFoundation, DW.iOSapi.Metal;

const
  SKColor = UIColor;
  SKBlendModeAlpha = 0;
  SKBlendModeAdd = 1;
  SKBlendModeSubtract = 2;
  SKBlendModeMultiply = 3;
  SKBlendModeMultiplyX2 = 4;
  SKBlendModeScreen = 5;
  SKBlendModeReplace = 6;
  SKBlendModeMultiplyAlpha = 7;
  SKNodeFocusBehaviorNone = 0;
  SKNodeFocusBehaviorOccluding = 1;
  SKNodeFocusBehaviorFocusable = 2;
  SKActionTimingLinear = 0;
  SKActionTimingEaseIn = 1;
  SKActionTimingEaseOut = 2;
  SKActionTimingEaseInEaseOut = 3;
  SKAttributeTypeNone = 0;
  SKAttributeTypeFloat = 1;
  SKAttributeTypeVectorFloat2 = 2;
  SKAttributeTypeVectorFloat3 = 3;
  SKAttributeTypeVectorFloat4 = 4;
  SKAttributeTypeHalfFloat = 5;
  SKAttributeTypeVectorHalfFloat2 = 6;
  SKAttributeTypeVectorHalfFloat3 = 7;
  SKAttributeTypeVectorHalfFloat4 = 8;
  SKSceneScaleModeFill = 0;
  SKSceneScaleModeAspectFill = 1;
  SKSceneScaleModeAspectFit = 2;
  SKSceneScaleModeResizeFill = 3;
  SKInterpolationModeLinear = 1;
  SKInterpolationModeSpline = 2;
  SKInterpolationModeStep = 3;
  SKRepeatModeClamp = 1;
  SKRepeatModeLoop = 2;
  SKParticleRenderOrderOldestLast = 0;
  SKParticleRenderOrderOldestFirst = 1;
  SKParticleRenderOrderDontCare = 2;
  SKLabelVerticalAlignmentModeBaseline = 0;
  SKLabelVerticalAlignmentModeCenter = 1;
  SKLabelVerticalAlignmentModeTop = 2;
  SKLabelVerticalAlignmentModeBottom = 3;
  SKLabelHorizontalAlignmentModeCenter = 0;
  SKLabelHorizontalAlignmentModeLeft = 1;
  SKLabelHorizontalAlignmentModeRight = 2;
  SKTransitionDirectionUp = 0;
  SKTransitionDirectionDown = 1;
  SKTransitionDirectionRight = 2;
  SKTransitionDirectionLeft = 3;
  SKTextureFilteringNearest = 0;
  SKTextureFilteringLinear = 1;
  SKUniformTypeNone = 0;
  SKUniformTypeFloat = 1;
  SKUniformTypeFloatVector2 = 2;
  SKUniformTypeFloatVector3 = 3;
  SKUniformTypeFloatVector4 = 4;
  SKUniformTypeFloatMatrix2 = 5;
  SKUniformTypeFloatMatrix3 = 6;
  SKUniformTypeFloatMatrix4 = 7;
  SKUniformTypeTexture = 8;
  SKTileDefinitionRotation0 = 0;
  SKTileDefinitionRotation90 = 1;
  SKTileDefinitionRotation180 = 2;
  SKTileDefinitionRotation270 = 3;
  SKTileSetTypeGrid = 0;
  SKTileSetTypeIsometric = 1;
  SKTileSetTypeHexagonalFlat = 2;
  SKTileSetTypeHexagonalPointy = 3;
  SKTileAdjacencyUp = 1;
  SKTileAdjacencyUpperRight = 2;
  SKTileAdjacencyRight = 4;
  SKTileAdjacencyLowerRight = 8;
  SKTileAdjacencyDown = 16;
  SKTileAdjacencyLowerLeft = 32;
  SKTileAdjacencyLeft = 64;
  SKTileAdjacencyUpperLeft = 128;
  SKTileAdjacencyAll = 255;
  SKTileHexFlatAdjacencyUp = 1;
  SKTileHexFlatAdjacencyUpperRight = 2;
  SKTileHexFlatAdjacencyLowerRight = 4;
  SKTileHexFlatAdjacencyDown = 8;
  SKTileHexFlatAdjacencyLowerLeft = 16;
  SKTileHexFlatAdjacencyUpperLeft = 32;
  SKTileHexFlatAdjacencyAll = 63;
  SKTileHexPointyAdjacencyUpperLeft = 1;
  SKTileHexPointyAdjacencyUpperRight = 2;
  SKTileHexPointyAdjacencyRight = 4;
  SKTileHexPointyAdjacencyLowerRight = 8;
  SKTileHexPointyAdjacencyLowerLeft = 16;
  SKTileHexPointyAdjacencyLeft = 32;
  SKTileHexPointyAdjacencyAdd = 63;
  SKTileAdjacencyUpEdge = 124;
  SKTileAdjacencyUpperRightEdge = 112;
  SKTileAdjacencyRightEdge = 241;
  SKTileAdjacencyLowerRightEdge = 193;
  SKTileAdjacencyDownEdge = 199;
  SKTileAdjacencyLowerLeftEdge = 7;
  SKTileAdjacencyLeftEdge = 31;
  SKTileAdjacencyUpperLeftEdge = 28;
  SKTileAdjacencyUpperRightCorner = 223;
  SKTileAdjacencyLowerRightCorner = 127;
  SKTileAdjacencyLowerLeftCorner = 253;
  SKTileAdjacencyUpperLeftCorner = 247;

type
  SKNode = interface;
  SKShader = interface;
  SKAction = interface;
  SKWarpable = interface;
  SKWarpGeometry = interface;
  SKWarpGeometryGrid = interface;
  SKSpriteNode = interface;
  SKAttribute = interface;
  SKAttributeValue = interface;
  SKCameraNode = interface;
  SKEffectNode = interface;
  SKSceneDelegate = interface;
  SKScene = interface;
  SKKeyframeSequence = interface;
  SKEmitterNode = interface;
  SKShapeNode = interface;
  SKFieldNode = interface;
  SKLabelNode = interface;
  SKVideoNode = interface;
  SKCropNode = interface;
  SKLightNode = interface;
  SKReferenceNode = interface;
  SKTransformNode = interface;
  SKRegion = interface;
  SKView = interface;
  SKViewDelegate = interface;
  SKTransition = interface;
  SKTexture = interface;
  SKUniform = interface;
  SKRenderer = interface;
  SKTileDefinition = interface;
  SKTileSet = interface;
  SKTileGroup = interface;
  SKTileGroupRule = interface;
  SKTileMapNode = interface;
  SKMutableTexture = interface;
  SKTextureAtlas = interface;
  SKRange = interface;
  SKConstraint = interface;
  SKReachConstraints = interface;
  SKPhysicsBody = interface;
  SKPhysicsJoint = interface;
  SKPhysicsJointPin = interface;
  SKPhysicsJointSpring = interface;
  SKPhysicsJointFixed = interface;
  SKPhysicsJointSliding = interface;
  SKPhysicsJointLimit = interface;
  SKPhysicsContact = interface;
  SKPhysicsContactDelegate = interface;
  SKPhysicsWorld = interface;
  SKAudioNode = interface;

  SKBlendMode = NSInteger;
  SKNodeFocusBehavior = NSInteger;
  SKActionTimingMode = NSInteger;

  SKActionTimingFunction = function(time: Single): Single of object;
  SKAttributeType = NSInteger;
  SKSceneScaleMode = NSInteger;
  SKInterpolationMode = NSInteger;
  SKRepeatMode = NSInteger;
  SKParticleRenderOrder = NSInteger;

  SKFieldForceEvaluator = function(position: vector_float3; velocity: vector_float3; mass: Single; charge: Single;
    deltaTime: NSTimeInterval): vector_float3 of object;
  SKLabelVerticalAlignmentMode = NSInteger;
  SKLabelHorizontalAlignmentMode = NSInteger;
  SKTransitionDirection = NSInteger;
  SKTextureFilteringMode = NSInteger;
  SKUniformType = NSInteger;
  SKTileDefinitionRotation = NSInteger;
  SKTileSetType = NSInteger;
  SKTileAdjacencyMask = NSInteger;
  TSKNodeBlockMethod1 = procedure(node: SKNode; stop: PBoolean) of object;
  TSKNodeBlockMethod2 = procedure of object;
  TSKActionBlockMethod1 = procedure(node: SKNode; elapsedTime: CGFloat) of object;
  TSKTextureBlockMethod1 = procedure of object;
  TSKMutableTextureBlockMethod1 = procedure(pixelData: Pointer; lengthInBytes: NativeUInt) of object;
  TSKTextureAtlasBlockMethod1 = procedure of object;
  TSKTextureAtlasBlockMethod2 = procedure(error: NSError; foundAtlases: NSArray) of object;
  TSKPhysicsWorldBlockMethod1 = procedure(body: SKPhysicsBody; stop: PBoolean) of object;
  TSKPhysicsWorldBlockMethod2 = procedure(body: SKPhysicsBody; point: CGPoint; normal: CGVector; stop: PBoolean) of object;

  SKNodeClass = interface(UIResponderClass)
    ['{6AE7015B-53B1-4F10-B425-570D51701B77}']
    {class} function node: Pointer; cdecl;
    {class} function nodeWithFileNamed(filename: NSString; securelyWithClasses: NSSet; andError: PPointer): Pointer; overload; cdecl;
    {class} function nodeWithFileNamed(filename: NSString): Pointer; overload; cdecl;
  end;

  SKNode = interface(UIResponder)
    ['{A218481C-C23B-4E18-BBFC-73767A9751D5}']
    function actionForKey(key: NSString): SKAction; cdecl;
    procedure addChild(node: SKNode); cdecl;
    function alpha: CGFloat; cdecl;
    function attributeValues: NSDictionary; cdecl;
    function calculateAccumulatedFrame: CGRect; cdecl;
    function childNodeWithName(name: NSString): SKNode; cdecl;
    function children: NSArray; cdecl;
    function constraints: NSArray; cdecl;
    function containsPoint(p: CGPoint): Boolean; cdecl;
    [MethodName('convertPoint:fromNode:')]
    function convertPointFromNode(point: CGPoint; fromNode: SKNode): CGPoint; cdecl;
    [MethodName('convertPoint:toNode:')]
    function convertPointToNode(point: CGPoint; toNode: SKNode): CGPoint; cdecl;
    procedure enumerateChildNodesWithName(name: NSString; usingBlock: TSKNodeBlockMethod1); cdecl;
    function focusBehavior: SKNodeFocusBehavior; cdecl;
    function frame: CGRect; cdecl;
    function hasActions: Boolean; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function inParentHierarchy(parent: SKNode): Boolean; cdecl;
    procedure insertChild(node: SKNode; atIndex: NSInteger); cdecl;
    function intersectsNode(node: SKNode): Boolean; cdecl;
    function isEqualToNode(node: SKNode): Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isPaused: Boolean; cdecl;
    function isUserInteractionEnabled: Boolean; cdecl;
    procedure moveToParent(parent: SKNode); cdecl;
    function name: NSString; cdecl;
    function nodeAtPoint(p: CGPoint): SKNode; cdecl;
    function nodesAtPoint(p: CGPoint): NSArray; cdecl;
    function objectForKeyedSubscript(name: NSString): NSArray; cdecl;
    function parent: SKNode; cdecl;
    function physicsBody: SKPhysicsBody; cdecl;
    function position: CGPoint; cdecl;
    function reachConstraints: SKReachConstraints; cdecl;
    procedure removeActionForKey(key: NSString); cdecl;
    procedure removeAllActions; cdecl;
    procedure removeAllChildren; cdecl;
    procedure removeChildrenInArray(nodes: NSArray); cdecl;
    procedure removeFromParent; cdecl;
    procedure runAction(action: SKAction); overload; cdecl;
    procedure runAction(action: SKAction; completion: TSKNodeBlockMethod2); overload; cdecl;
    procedure runAction(action: SKAction; withKey: NSString); overload; cdecl;
    function scene: SKScene; cdecl;
    procedure setAlpha(alpha: CGFloat); cdecl;
    procedure setAttributeValues(attributeValues: NSDictionary); cdecl;
    procedure setConstraints(constraints: NSArray); cdecl;
    procedure setFocusBehavior(focusBehavior: SKNodeFocusBehavior); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    procedure setPhysicsBody(physicsBody: SKPhysicsBody); cdecl;
    procedure setPosition(position: CGPoint); cdecl;
    procedure setReachConstraints(reachConstraints: SKReachConstraints); cdecl;
    procedure setScale(scale: CGFloat); cdecl;
    procedure setSpeed(speed: CGFloat); cdecl;
    procedure setUserData(userData: NSMutableDictionary); cdecl;
    procedure setUserInteractionEnabled(userInteractionEnabled: Boolean); cdecl;
    procedure setValue(value: SKAttributeValue; forAttributeNamed: NSString); cdecl;
    procedure setXScale(xScale: CGFloat); cdecl;
    procedure setYScale(yScale: CGFloat); cdecl;
    procedure setZPosition(zPosition: CGFloat); cdecl;
    procedure setZRotation(zRotation: CGFloat); cdecl;
    function speed: CGFloat; cdecl;
    function userData: NSMutableDictionary; cdecl;
    function valueForAttributeNamed(key: NSString): SKAttributeValue; cdecl;
    function xScale: CGFloat; cdecl;
    function yScale: CGFloat; cdecl;
    function zPosition: CGFloat; cdecl;
    function zRotation: CGFloat; cdecl;
  end;
  TSKNode = class(TOCGenericImport<SKNodeClass, SKNode>) end;

  SKShaderClass = interface(NSObjectClass)
    ['{9A5BBCFE-82DE-418A-995B-A27234433A83}']
    {class} function shader: Pointer; cdecl;
    {class} function shaderWithFileNamed(name: NSString): Pointer; cdecl;
    {class} function shaderWithSource(source: NSString; uniforms: NSArray): Pointer; overload; cdecl;
    {class} function shaderWithSource(source: NSString): Pointer; overload; cdecl;
  end;

  SKShader = interface(NSObject)
    ['{1895FB3E-3226-4A93-B59B-4EBDDC77BA66}']
    procedure addUniform(uniform: SKUniform); cdecl;
    function attributes: NSArray; cdecl;
    function initWithSource(source: NSString): Pointer; overload; cdecl;
    function initWithSource(source: NSString; uniforms: NSArray): Pointer; overload; cdecl;
    procedure removeUniformNamed(name: NSString); cdecl;
    procedure setAttributes(attributes: NSArray); cdecl;
    procedure setSource(source: NSString); cdecl;
    procedure setUniforms(uniforms: NSArray); cdecl;
    function source: NSString; cdecl;
    function uniformNamed(name: NSString): SKUniform; cdecl;
    function uniforms: NSArray; cdecl;
  end;
  TSKShader = class(TOCGenericImport<SKShaderClass, SKShader>) end;

  SKActionClass = interface(NSObjectClass)
    ['{E5059A74-9144-41D2-9405-33BCC0A31E5F}']
    {class} function actionNamed(name: NSString; fromURL: NSURL; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function actionNamed(name: NSString; fromURL: NSURL): SKAction; overload; cdecl;
    {class} function actionNamed(name: NSString; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function actionNamed(name: NSString): SKAction; overload; cdecl;
    {class} function animateWithNormalTextures(textures: NSArray; timePerFrame: NSTimeInterval): SKAction; overload; cdecl;
    {class} function animateWithNormalTextures(textures: NSArray; timePerFrame: NSTimeInterval; resize: Boolean;
      restore: Boolean): SKAction; overload; cdecl;
    {class} function animateWithTextures(textures: NSArray; timePerFrame: NSTimeInterval): SKAction; overload; cdecl;
    {class} function animateWithTextures(textures: NSArray; timePerFrame: NSTimeInterval; resize: Boolean;
      restore: Boolean): SKAction; overload; cdecl;
    {class} function animateWithWarps(warps: NSArray; times: NSArray; restore: Boolean): SKAction; overload; cdecl;
    {class} function animateWithWarps(warps: NSArray; times: NSArray): SKAction; overload; cdecl;
    {class} function applyAngularImpulse(impulse: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function applyForce(force: CGVector; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function applyForce(force: CGVector; atPoint: CGPoint; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function applyImpulse(impulse: CGVector; atPoint: CGPoint; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function applyImpulse(impulse: CGVector; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function applyTorque(torque: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeChargeBy(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeChargeTo(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeMassBy(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeMassTo(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeObstructionBy(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeObstructionTo(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeOcclusionBy(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeOcclusionTo(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changePlaybackRateBy(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changePlaybackRateTo(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeReverbBy(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeReverbTo(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeVolumeBy(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function changeVolumeTo(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function colorizeWithColor(color: UIColor; colorBlendFactor: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function colorizeWithColorBlendFactor(colorBlendFactor: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function customActionWithDuration(duration: NSTimeInterval; actionBlock: TSKActionBlockMethod1): SKAction; cdecl;
    {class} function fadeAlphaBy(factor: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function fadeAlphaTo(alpha: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function fadeInWithDuration(duration: NSTimeInterval): SKAction; cdecl;
    {class} function fadeOutWithDuration(duration: NSTimeInterval): SKAction; cdecl;
    {class} function falloffBy(falloff: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function falloffTo(falloff: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function followPath(path: CGPathRef; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function followPath(path: CGPathRef; asOffset: Boolean; orientToPath: Boolean; duration: NSTimeInterval): SKAction; overload; cdecl;
    [MethodName('followPath:speed:')]
    {class} function followPathSpeed(path: CGPathRef; speed: CGFloat): SKAction; overload; cdecl;
    [MethodName('followPath:asOffset:orientToPath:speed')]
    {class} function followPathAsOffsetOrientToPathSpeed(path: CGPathRef; asOffset: Boolean; orientToPath: Boolean;
      speed: CGFloat): SKAction; overload; cdecl;
    {class} function group(actions: NSArray): SKAction; cdecl;
    {class} function hide: SKAction; cdecl;
    {class} function moveBy(delta: CGVector; duration: NSTimeInterval): SKAction; cdecl;
    {class} function moveByX(deltaX: CGFloat; y: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function moveTo(location: CGPoint; duration: NSTimeInterval): SKAction; cdecl;
    {class} function moveToX(x: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function moveToY(y: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function pause: SKAction; cdecl;
    {class} function performSelector(selector: SEL; onTarget: Pointer): SKAction; cdecl;
    {class} function play: SKAction; cdecl;
    {class} function playSoundFileNamed(soundFile: NSString; waitForCompletion: Boolean): SKAction; cdecl;
    {class} function reachTo(position: CGPoint; rootNode: SKNode; duration: NSTimeInterval): SKAction; overload; cdecl;
    [MethodName('reachTo:rootNode:velocity')]
    {class} function reachToRootNodeVelocity(position: CGPoint; rootNode: SKNode; velocity: CGFloat): SKAction; overload; cdecl;
    {class} function reachToNode(node: SKNode; rootNode: SKNode; velocity: CGFloat): SKAction; overload; cdecl;
    [MethodName('reachTo:rootNode:duration')]
    {class} function reachToNodeRootNodeDuration(node: SKNode; rootNode: SKNode; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function removeFromParent: SKAction; cdecl;
    {class} function repeatAction(action: SKAction; count: NSUInteger): SKAction; cdecl;
    {class} function repeatActionForever(action: SKAction): SKAction; cdecl;
    {class} function resizeByWidth(width: CGFloat; height: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function resizeToHeight(height: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function resizeToWidth(width: CGFloat; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function resizeToWidth(width: CGFloat; height: CGFloat; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function rotateByAngle(radians: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function rotateToAngle(radians: CGFloat; duration: NSTimeInterval; shortestUnitArc: Boolean): SKAction; overload; cdecl;
    {class} function rotateToAngle(radians: CGFloat; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function runAction(action: SKAction; onChildWithName: NSString): SKAction; cdecl;
    {class} function runBlock(block: dispatch_block_t): SKAction; overload; cdecl;
    {class} function runBlock(block: dispatch_block_t; queue: dispatch_queue_t): SKAction; overload; cdecl;
    {class} function scaleBy(scale: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function scaleTo(scale: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function scaleToSize(size: CGSize; duration: NSTimeInterval): SKAction; cdecl;
    {class} function scaleXBy(xScale: CGFloat; y: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function scaleXTo(xScale: CGFloat; y: CGFloat; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function scaleXTo(scale: CGFloat; duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function scaleYTo(scale: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function sequence(actions: NSArray): SKAction; cdecl;
    {class} function setNormalTexture(texture: SKTexture; resize: Boolean): SKAction; overload; cdecl;
    {class} function setNormalTexture(texture: SKTexture): SKAction; overload; cdecl;
    {class} function setTexture(texture: SKTexture; resize: Boolean): SKAction; overload; cdecl;
    {class} function setTexture(texture: SKTexture): SKAction; overload; cdecl;
    {class} function speedBy(speed: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function speedTo(speed: CGFloat; duration: NSTimeInterval): SKAction; cdecl;
    {class} function stereoPanBy(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function stereoPanTo(v: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function stop: SKAction; cdecl;
    {class} function strengthBy(strength: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function strengthTo(strength: Single; duration: NSTimeInterval): SKAction; cdecl;
    {class} function unhide: SKAction; cdecl;
    {class} function waitForDuration(duration: NSTimeInterval; withRange: NSTimeInterval): SKAction; overload; cdecl;
    {class} function waitForDuration(duration: NSTimeInterval): SKAction; overload; cdecl;
    {class} function warpTo(warp: SKWarpGeometry; duration: NSTimeInterval): SKAction; cdecl;
  end;

  SKAction = interface(NSObject)
    ['{53ED2CD4-E999-4E72-A2CD-DF45D53E4943}']
    function duration: NSTimeInterval; cdecl;
    function reversedAction: SKAction; cdecl;
    procedure setDuration(duration: NSTimeInterval); cdecl;
    procedure setSpeed(speed: CGFloat); cdecl;
    procedure setTimingFunction(timingFunction: SKActionTimingFunction); cdecl;
    procedure setTimingMode(timingMode: SKActionTimingMode); cdecl;
    function speed: CGFloat; cdecl;
    function timingFunction: SKActionTimingFunction; cdecl;
    function timingMode: SKActionTimingMode; cdecl;
  end;
  TSKAction = class(TOCGenericImport<SKActionClass, SKAction>) end;

  SKWarpable = interface(IObjectiveC)
    ['{F4436EDC-5F3F-485A-8DF3-3E9BD1DF3E93}']
    procedure setSubdivisionLevels(subdivisionLevels: NSInteger); cdecl;
    procedure setWarpGeometry(warpGeometry: SKWarpGeometry); cdecl;
    function subdivisionLevels: NSInteger; cdecl;
    function warpGeometry: SKWarpGeometry; cdecl;
  end;

  SKWarpGeometryClass = interface(NSObjectClass)
    ['{910EA810-40FB-475C-ABDF-3EC6A4CCC920}']
  end;

  SKWarpGeometry = interface(NSObject)
    ['{0901B5E1-CF4E-4360-B1B5-86F7C348755A}']
  end;
  TSKWarpGeometry = class(TOCGenericImport<SKWarpGeometryClass, SKWarpGeometry>) end;

  SKWarpGeometryGridClass = interface(SKWarpGeometryClass)
    ['{40E83BD6-6C42-4916-98C4-506BE780E95E}']
    {class} function grid: Pointer; cdecl;
    {class} function gridWithColumns(cols: NSInteger; rows: NSInteger; sourcePositions: Pvector_float2;
      destPositions: Pvector_float2): Pointer; overload; cdecl;
    {class} function gridWithColumns(cols: NSInteger; rows: NSInteger): Pointer; overload; cdecl;
  end;

  SKWarpGeometryGrid = interface(SKWarpGeometry)
    ['{D26B19A0-4F83-4A88-9C96-D66A739F6911}']
    function destPositionAtIndex(index: NSInteger): vector_float2; cdecl;
    function gridByReplacingDestPositions(destPositions: Pvector_float2): Pointer; cdecl;
    function gridByReplacingSourcePositions(sourcePositions: Pvector_float2): Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithColumns(cols: NSInteger; rows: NSInteger; sourcePositions: Pvector_float2; destPositions: Pvector_float2): Pointer; cdecl;
    function numberOfColumns: NSInteger; cdecl;
    function numberOfRows: NSInteger; cdecl;
    function sourcePositionAtIndex(index: NSInteger): vector_float2; cdecl;
    function vertexCount: NSInteger; cdecl;
  end;
  TSKWarpGeometryGrid = class(TOCGenericImport<SKWarpGeometryGridClass, SKWarpGeometryGrid>) end;

  SKSpriteNodeClass = interface(SKNodeClass)
    ['{CBF51D71-FA73-46D8-8BF2-DC04E5E855D3}']
    {class} function spriteNodeWithColor(color: UIColor; size: CGSize): Pointer; cdecl;
    {class} function spriteNodeWithImageNamed(name: NSString): Pointer; overload; cdecl;
    {class} function spriteNodeWithImageNamed(name: NSString; normalMapped: Boolean): Pointer; overload; cdecl;
    {class} function spriteNodeWithTexture(texture: SKTexture; size: CGSize): Pointer; overload; cdecl;
    {class} function spriteNodeWithTexture(texture: SKTexture): Pointer; overload; cdecl;
    {class} function spriteNodeWithTexture(texture: SKTexture; normalMap: SKTexture): Pointer; overload; cdecl;
  end;

  SKSpriteNode = interface(SKNode)
    ['{EA3F6B2C-94DA-4E72-ACAF-A84C56BC47BB}']
    function anchorPoint: CGPoint; cdecl;
    function attributeValues: NSDictionary; cdecl;
    function blendMode: SKBlendMode; cdecl;
    function centerRect: CGRect; cdecl;
    function color: UIColor; cdecl;
    function colorBlendFactor: CGFloat; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithColor(color: UIColor; size: CGSize): Pointer; cdecl;
    function initWithImageNamed(name: NSString): Pointer; cdecl;
    function initWithTexture(texture: SKTexture; color: UIColor; size: CGSize): Pointer; overload; cdecl;
    function initWithTexture(texture: SKTexture): Pointer; overload; cdecl;
    function lightingBitMask: UInt32; cdecl;
    function normalTexture: SKTexture; cdecl;
    procedure scaleToSize(size: CGSize); cdecl;
    procedure setAnchorPoint(anchorPoint: CGPoint); cdecl;
    procedure setAttributeValues(attributeValues: NSDictionary); cdecl;
    procedure setBlendMode(blendMode: SKBlendMode); cdecl;
    procedure setCenterRect(centerRect: CGRect); cdecl;
    procedure setColor(color: UIColor); cdecl;
    procedure setColorBlendFactor(colorBlendFactor: CGFloat); cdecl;
    procedure setLightingBitMask(lightingBitMask: UInt32); cdecl;
    procedure setNormalTexture(normalTexture: SKTexture); cdecl;
    procedure setShader(shader: SKShader); cdecl;
    procedure setShadowCastBitMask(shadowCastBitMask: UInt32); cdecl;
    procedure setShadowedBitMask(shadowedBitMask: UInt32); cdecl;
    procedure setSize(size: CGSize); cdecl;
    procedure setTexture(texture: SKTexture); cdecl;
    procedure setValue(value: SKAttributeValue; forAttributeNamed: NSString); cdecl;
    function shader: SKShader; cdecl;
    function shadowCastBitMask: UInt32; cdecl;
    function shadowedBitMask: UInt32; cdecl;
    function size: CGSize; cdecl;
    function texture: SKTexture; cdecl;
    function valueForAttributeNamed(key: NSString): SKAttributeValue; cdecl;
  end;
  TSKSpriteNode = class(TOCGenericImport<SKSpriteNodeClass, SKSpriteNode>) end;

  SKAttributeClass = interface(NSObjectClass)
    ['{619B5E94-99C6-4563-B3C7-DAA7E2A450F1}']
    {class} function attributeWithName(name: NSString; &type: SKAttributeType): Pointer; cdecl;
  end;

  SKAttribute = interface(NSObject)
    ['{8AAB3C34-21BD-4C3C-8C24-CCA6B5FF7B25}']
    [MethodName('type')]
    function &type: SKAttributeType; cdecl;
    function initWithName(name: NSString; &type: SKAttributeType): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TSKAttribute = class(TOCGenericImport<SKAttributeClass, SKAttribute>) end;

  SKAttributeValueClass = interface(NSObjectClass)
    ['{4EFEBDD2-F4C1-4E3D-91F2-B62A26B953BA}']
    {class} function valueWithFloat(value: Single): Pointer; cdecl;
    {class} function valueWithVectorFloat2(value: vector_float2): Pointer; cdecl;
    {class} function valueWithVectorFloat3(value: vector_float3): Pointer; cdecl;
    {class} function valueWithVectorFloat4(value: vector_float4): Pointer; cdecl;
  end;

  SKAttributeValue = interface(NSObject)
    ['{D88E5DBC-BFA9-436F-A155-684DAD669896}']
    function floatValue: Single; cdecl;
    procedure setFloatValue(floatValue: Single); cdecl;
    procedure setVectorFloat2Value(vectorFloat2Value: vector_float2); cdecl;
    procedure setVectorFloat3Value(vectorFloat3Value: vector_float3); cdecl;
    procedure setVectorFloat4Value(vectorFloat4Value: vector_float4); cdecl;
    function vectorFloat2Value: vector_float2; cdecl;
    function vectorFloat3Value: vector_float3; cdecl;
    function vectorFloat4Value: vector_float4; cdecl;
  end;
  TSKAttributeValue = class(TOCGenericImport<SKAttributeValueClass, SKAttributeValue>) end;

  SKCameraNodeClass = interface(SKNodeClass)
    ['{9502BA7B-E23C-4261-AD78-EFC3F53EF9D4}']
  end;

  SKCameraNode = interface(SKNode)
    ['{73087342-57FD-42BE-9596-91902B2C44BE}']
    function containedNodeSet: NSSet; cdecl;
    function containsNode(node: SKNode): Boolean; cdecl;
  end;
  TSKCameraNode = class(TOCGenericImport<SKCameraNodeClass, SKCameraNode>) end;

  SKEffectNodeClass = interface(SKNodeClass)
    ['{FDBB97B8-531D-4F8F-A5EC-EA05B40CB450}']
  end;

  SKEffectNode = interface(SKNode)
    ['{47FC88DC-8B7E-414A-9B36-6BD815CF3A0A}']
    function attributeValues: NSDictionary; cdecl;
    function blendMode: SKBlendMode; cdecl;
    function filter: CIFilter; cdecl;
    procedure setAttributeValues(attributeValues: NSDictionary); cdecl;
    procedure setBlendMode(blendMode: SKBlendMode); cdecl;
    procedure setFilter(filter: CIFilter); cdecl;
    procedure setShader(shader: SKShader); cdecl;
    procedure setShouldCenterFilter(shouldCenterFilter: Boolean); cdecl;
    procedure setShouldEnableEffects(shouldEnableEffects: Boolean); cdecl;
    procedure setShouldRasterize(shouldRasterize: Boolean); cdecl;
    procedure setValue(value: SKAttributeValue; forAttributeNamed: NSString); cdecl;
    function shader: SKShader; cdecl;
    function shouldCenterFilter: Boolean; cdecl;
    function shouldEnableEffects: Boolean; cdecl;
    function shouldRasterize: Boolean; cdecl;
    function valueForAttributeNamed(key: NSString): SKAttributeValue; cdecl;
  end;
  TSKEffectNode = class(TOCGenericImport<SKEffectNodeClass, SKEffectNode>) end;

  SKSceneDelegate = interface(IObjectiveC)
    ['{9A4EC22B-1F03-4F34-8526-2444E795B7C4}']
    procedure didApplyConstraintsForScene(scene: SKScene); cdecl;
    procedure didEvaluateActionsForScene(scene: SKScene); cdecl;
    procedure didFinishUpdateForScene(scene: SKScene); cdecl;
    procedure didSimulatePhysicsForScene(scene: SKScene); cdecl;
    procedure update(currentTime: NSTimeInterval; forScene: SKScene); cdecl;
  end;

  SKSceneClass = interface(SKEffectNodeClass)
    ['{6D102EB7-E2C7-407B-8998-0CE6B0401BCF}']
    {class} function sceneWithSize(size: CGSize): Pointer; cdecl;
  end;

  SKScene = interface(SKEffectNode)
    ['{78A36B89-FA21-4537-99EC-87271A1873D1}']
    function anchorPoint: CGPoint; cdecl;
    function audioEngine: AVAudioEngine; cdecl;
    function backgroundColor: UIColor; cdecl;
    function camera: SKCameraNode; cdecl;
    function convertPointFromView(point: CGPoint): CGPoint; cdecl;
    function convertPointToView(point: CGPoint): CGPoint; cdecl;
    function delegate: Pointer; cdecl;
    procedure didApplyConstraints; cdecl;
    procedure didChangeSize(oldSize: CGSize); cdecl;
    procedure didEvaluateActions; cdecl;
    procedure didFinishUpdate; cdecl;
    procedure didMoveToView(view: SKView); cdecl;
    procedure didSimulatePhysics; cdecl;
    function initWithSize(size: CGSize): Pointer; cdecl;
    function listener: SKNode; cdecl;
    function physicsWorld: SKPhysicsWorld; cdecl;
    function scaleMode: SKSceneScaleMode; cdecl;
    procedure sceneDidLoad; cdecl;
    procedure setAnchorPoint(anchorPoint: CGPoint); cdecl;
    procedure setBackgroundColor(backgroundColor: UIColor); cdecl;
    procedure setCamera(camera: SKCameraNode); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setListener(listener: SKNode); cdecl;
    procedure setScaleMode(scaleMode: SKSceneScaleMode); cdecl;
    procedure setSize(size: CGSize); cdecl;
    function size: CGSize; cdecl;
    procedure update(currentTime: NSTimeInterval); cdecl;
    function view: SKView; cdecl;
    procedure willMoveFromView(view: SKView); cdecl;
  end;
  TSKScene = class(TOCGenericImport<SKSceneClass, SKScene>) end;

  SKKeyframeSequenceClass = interface(NSObjectClass)
    ['{26A904F7-0D26-4601-88AD-9667D4090B37}']
  end;

  SKKeyframeSequence = interface(NSObject)
    ['{99C92AA1-4DA0-4F95-BF89-A52C33B8CFA6}']
    procedure addKeyframeValue(value: Pointer; time: CGFloat); cdecl;
    function count: NSUInteger; cdecl;
    function getKeyframeTimeForIndex(index: NSUInteger): CGFloat; cdecl;
    function getKeyframeValueForIndex(index: NSUInteger): Pointer; cdecl;
    function initWithCapacity(numItems: NSUInteger): Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithKeyframeValues(values: NSArray; times: NSArray): Pointer; cdecl;
    function interpolationMode: SKInterpolationMode; cdecl;
    procedure removeKeyframeAtIndex(index: NSUInteger); cdecl;
    procedure removeLastKeyframe; cdecl;
    function repeatMode: SKRepeatMode; cdecl;
    function sampleAtTime(time: CGFloat): Pointer; cdecl;
    procedure setInterpolationMode(interpolationMode: SKInterpolationMode); cdecl;
    procedure setKeyframeTime(time: CGFloat; forIndex: NSUInteger); cdecl;
    procedure setKeyframeValue(value: Pointer; forIndex: NSUInteger); overload; cdecl;
    procedure setKeyframeValue(value: Pointer; time: CGFloat; forIndex: NSUInteger); overload; cdecl;
    procedure setRepeatMode(repeatMode: SKRepeatMode); cdecl;
  end;
  TSKKeyframeSequence = class(TOCGenericImport<SKKeyframeSequenceClass, SKKeyframeSequence>) end;

  SKEmitterNodeClass = interface(SKNodeClass)
    ['{DE35BC2C-51D9-4637-A319-2167D7BEC429}']
  end;

  SKEmitterNode = interface(SKNode)
    ['{4F177C36-6C64-41EC-B998-F5B9B5F7A244}']
    procedure advanceSimulationTime(sec: NSTimeInterval); cdecl;
    function attributeValues: NSDictionary; cdecl;
    function emissionAngle: CGFloat; cdecl;
    function emissionAngleRange: CGFloat; cdecl;
    function fieldBitMask: UInt32; cdecl;
    function numParticlesToEmit: NSUInteger; cdecl;
    function particleAction: SKAction; cdecl;
    function particleAlpha: CGFloat; cdecl;
    function particleAlphaRange: CGFloat; cdecl;
    function particleAlphaSequence: SKKeyframeSequence; cdecl;
    function particleAlphaSpeed: CGFloat; cdecl;
    function particleBirthRate: CGFloat; cdecl;
    function particleBlendMode: SKBlendMode; cdecl;
    function particleColor: UIColor; cdecl;
    function particleColorAlphaRange: CGFloat; cdecl;
    function particleColorAlphaSpeed: CGFloat; cdecl;
    function particleColorBlendFactor: CGFloat; cdecl;
    function particleColorBlendFactorRange: CGFloat; cdecl;
    function particleColorBlendFactorSequence: SKKeyframeSequence; cdecl;
    function particleColorBlendFactorSpeed: CGFloat; cdecl;
    function particleColorBlueRange: CGFloat; cdecl;
    function particleColorBlueSpeed: CGFloat; cdecl;
    function particleColorGreenRange: CGFloat; cdecl;
    function particleColorGreenSpeed: CGFloat; cdecl;
    function particleColorRedRange: CGFloat; cdecl;
    function particleColorRedSpeed: CGFloat; cdecl;
    function particleColorSequence: SKKeyframeSequence; cdecl;
    function particleLifetime: CGFloat; cdecl;
    function particleLifetimeRange: CGFloat; cdecl;
    function particlePosition: CGPoint; cdecl;
    function particlePositionRange: CGVector; cdecl;
    function particleRenderOrder: SKParticleRenderOrder; cdecl;
    function particleRotation: CGFloat; cdecl;
    function particleRotationRange: CGFloat; cdecl;
    function particleRotationSpeed: CGFloat; cdecl;
    function particleScale: CGFloat; cdecl;
    function particleScaleRange: CGFloat; cdecl;
    function particleScaleSequence: SKKeyframeSequence; cdecl;
    function particleScaleSpeed: CGFloat; cdecl;
    function particleSize: CGSize; cdecl;
    function particleSpeed: CGFloat; cdecl;
    function particleSpeedRange: CGFloat; cdecl;
    function particleTexture: SKTexture; cdecl;
    function particleZPosition: CGFloat; cdecl;
    function particleZPositionRange: CGFloat; cdecl;
    function particleZPositionSpeed: CGFloat; cdecl;
    procedure resetSimulation; cdecl;
    procedure setAttributeValues(attributeValues: NSDictionary); cdecl;
    procedure setEmissionAngle(emissionAngle: CGFloat); cdecl;
    procedure setEmissionAngleRange(emissionAngleRange: CGFloat); cdecl;
    procedure setFieldBitMask(fieldBitMask: UInt32); cdecl;
    procedure setNumParticlesToEmit(numParticlesToEmit: NSUInteger); cdecl;
    procedure setParticleAction(particleAction: SKAction); cdecl;
    procedure setParticleAlpha(particleAlpha: CGFloat); cdecl;
    procedure setParticleAlphaRange(particleAlphaRange: CGFloat); cdecl;
    procedure setParticleAlphaSequence(particleAlphaSequence: SKKeyframeSequence); cdecl;
    procedure setParticleAlphaSpeed(particleAlphaSpeed: CGFloat); cdecl;
    procedure setParticleBirthRate(particleBirthRate: CGFloat); cdecl;
    procedure setParticleBlendMode(particleBlendMode: SKBlendMode); cdecl;
    procedure setParticleColor(particleColor: UIColor); cdecl;
    procedure setParticleColorAlphaRange(particleColorAlphaRange: CGFloat); cdecl;
    procedure setParticleColorAlphaSpeed(particleColorAlphaSpeed: CGFloat); cdecl;
    procedure setParticleColorBlendFactor(particleColorBlendFactor: CGFloat); cdecl;
    procedure setParticleColorBlendFactorRange(particleColorBlendFactorRange: CGFloat); cdecl;
    procedure setParticleColorBlendFactorSequence(particleColorBlendFactorSequence: SKKeyframeSequence); cdecl;
    procedure setParticleColorBlendFactorSpeed(particleColorBlendFactorSpeed: CGFloat); cdecl;
    procedure setParticleColorBlueRange(particleColorBlueRange: CGFloat); cdecl;
    procedure setParticleColorBlueSpeed(particleColorBlueSpeed: CGFloat); cdecl;
    procedure setParticleColorGreenRange(particleColorGreenRange: CGFloat); cdecl;
    procedure setParticleColorGreenSpeed(particleColorGreenSpeed: CGFloat); cdecl;
    procedure setParticleColorRedRange(particleColorRedRange: CGFloat); cdecl;
    procedure setParticleColorRedSpeed(particleColorRedSpeed: CGFloat); cdecl;
    procedure setParticleColorSequence(particleColorSequence: SKKeyframeSequence); cdecl;
    procedure setParticleLifetime(particleLifetime: CGFloat); cdecl;
    procedure setParticleLifetimeRange(particleLifetimeRange: CGFloat); cdecl;
    procedure setParticlePosition(particlePosition: CGPoint); cdecl;
    procedure setParticlePositionRange(particlePositionRange: CGVector); cdecl;
    procedure setParticleRenderOrder(particleRenderOrder: SKParticleRenderOrder); cdecl;
    procedure setParticleRotation(particleRotation: CGFloat); cdecl;
    procedure setParticleRotationRange(particleRotationRange: CGFloat); cdecl;
    procedure setParticleRotationSpeed(particleRotationSpeed: CGFloat); cdecl;
    procedure setParticleScale(particleScale: CGFloat); cdecl;
    procedure setParticleScaleRange(particleScaleRange: CGFloat); cdecl;
    procedure setParticleScaleSequence(particleScaleSequence: SKKeyframeSequence); cdecl;
    procedure setParticleScaleSpeed(particleScaleSpeed: CGFloat); cdecl;
    procedure setParticleSize(particleSize: CGSize); cdecl;
    procedure setParticleSpeed(particleSpeed: CGFloat); cdecl;
    procedure setParticleSpeedRange(particleSpeedRange: CGFloat); cdecl;
    procedure setParticleTexture(particleTexture: SKTexture); cdecl;
    procedure setParticleZPosition(particleZPosition: CGFloat); cdecl;
    procedure setParticleZPositionRange(particleZPositionRange: CGFloat); cdecl;
    procedure setParticleZPositionSpeed(particleZPositionSpeed: CGFloat); cdecl;
    procedure setShader(shader: SKShader); cdecl;
    procedure setTargetNode(targetNode: SKNode); cdecl;
    procedure setValue(value: SKAttributeValue; forAttributeNamed: NSString); cdecl;
    procedure setXAcceleration(xAcceleration: CGFloat); cdecl;
    procedure setYAcceleration(yAcceleration: CGFloat); cdecl;
    function shader: SKShader; cdecl;
    function targetNode: SKNode; cdecl;
    function valueForAttributeNamed(key: NSString): SKAttributeValue; cdecl;
    function xAcceleration: CGFloat; cdecl;
    function yAcceleration: CGFloat; cdecl;
  end;
  TSKEmitterNode = class(TOCGenericImport<SKEmitterNodeClass, SKEmitterNode>) end;

  SKShapeNodeClass = interface(SKNodeClass)
    ['{767B8471-1ACE-432F-B17C-6DBE9BDA5736}']
    {class} function shapeNodeWithCircleOfRadius(radius: CGFloat): Pointer; cdecl;
    {class} function shapeNodeWithEllipseInRect(rect: CGRect): Pointer; cdecl;
    {class} function shapeNodeWithEllipseOfSize(size: CGSize): Pointer; cdecl;
    {class} function shapeNodeWithPath(path: CGPathRef): Pointer; overload; cdecl;
    {class} function shapeNodeWithPath(path: CGPathRef; centered: Boolean): Pointer; overload; cdecl;
    {class} function shapeNodeWithPoints(points: PCGPoint; count: NativeUInt): Pointer; cdecl;
    {class} function shapeNodeWithRect(rect: CGRect; cornerRadius: CGFloat): Pointer; overload; cdecl;
    {class} function shapeNodeWithRect(rect: CGRect): Pointer; overload; cdecl;
    {class} function shapeNodeWithRectOfSize(size: CGSize): Pointer; overload; cdecl;
    {class} function shapeNodeWithRectOfSize(size: CGSize; cornerRadius: CGFloat): Pointer; overload; cdecl;
    {class} function shapeNodeWithSplinePoints(points: PCGPoint; count: NativeUInt): Pointer; cdecl;
  end;

  SKShapeNode = interface(SKNode)
    ['{D650A472-AAC7-4CD8-BD8B-006D2B129662}']
    function attributeValues: NSDictionary; cdecl;
    function blendMode: SKBlendMode; cdecl;
    function fillColor: UIColor; cdecl;
    function fillShader: SKShader; cdecl;
    function fillTexture: SKTexture; cdecl;
    function glowWidth: CGFloat; cdecl;
    function isAntialiased: Boolean; cdecl;
    function lineCap: CGLineCap; cdecl;
    function lineJoin: CGLineJoin; cdecl;
    function lineLength: CGFloat; cdecl;
    function lineWidth: CGFloat; cdecl;
    function miterLimit: CGFloat; cdecl;
    function path: CGPathRef; cdecl;
    procedure setAntialiased(antialiased: Boolean); cdecl;
    procedure setAttributeValues(attributeValues: NSDictionary); cdecl;
    procedure setBlendMode(blendMode: SKBlendMode); cdecl;
    procedure setFillColor(fillColor: UIColor); cdecl;
    procedure setFillShader(fillShader: SKShader); cdecl;
    procedure setFillTexture(fillTexture: SKTexture); cdecl;
    procedure setGlowWidth(glowWidth: CGFloat); cdecl;
    procedure setLineCap(lineCap: CGLineCap); cdecl;
    procedure setLineJoin(lineJoin: CGLineJoin); cdecl;
    procedure setLineWidth(lineWidth: CGFloat); cdecl;
    procedure setMiterLimit(miterLimit: CGFloat); cdecl;
    procedure setPath(path: CGPathRef); cdecl;
    procedure setStrokeColor(strokeColor: UIColor); cdecl;
    procedure setStrokeShader(strokeShader: SKShader); cdecl;
    procedure setStrokeTexture(strokeTexture: SKTexture); cdecl;
    procedure setValue(value: SKAttributeValue; forAttributeNamed: NSString); cdecl;
    function strokeColor: UIColor; cdecl;
    function strokeShader: SKShader; cdecl;
    function strokeTexture: SKTexture; cdecl;
    function valueForAttributeNamed(key: NSString): SKAttributeValue; cdecl;
  end;
  TSKShapeNode = class(TOCGenericImport<SKShapeNodeClass, SKShapeNode>) end;

  SKFieldNodeClass = interface(SKNodeClass)
    ['{E0ADC139-5560-47BD-92B3-4A017309D43E}']
    {class} function customFieldWithEvaluationBlock(block: SKFieldForceEvaluator): SKFieldNode; cdecl;
    {class} function dragField: SKFieldNode; cdecl;
    {class} function electricField: SKFieldNode; cdecl;
    {class} function linearGravityFieldWithVector(direction: vector_float3): SKFieldNode; cdecl;
    {class} function magneticField: SKFieldNode; cdecl;
    {class} function noiseFieldWithSmoothness(smoothness: CGFloat; animationSpeed: CGFloat): SKFieldNode; cdecl;
    {class} function radialGravityField: SKFieldNode; cdecl;
    {class} function springField: SKFieldNode; cdecl;
    {class} function turbulenceFieldWithSmoothness(smoothness: CGFloat; animationSpeed: CGFloat): SKFieldNode; cdecl;
    {class} function velocityFieldWithTexture(velocityTexture: SKTexture): SKFieldNode; cdecl;
    {class} function velocityFieldWithVector(direction: vector_float3): SKFieldNode; cdecl;
    {class} function vortexField: SKFieldNode; cdecl;
  end;

  SKFieldNode = interface(SKNode)
    ['{4B72FA5C-C898-4787-AE41-68DE81A1389B}']
    function animationSpeed: Single; cdecl;
    function categoryBitMask: UInt32; cdecl;
    function direction: vector_float3; cdecl;
    function falloff: Single; cdecl;
    function isEnabled: Boolean; cdecl;
    function isExclusive: Boolean; cdecl;
    function minimumRadius: Single; cdecl;
    function region: SKRegion; cdecl;
    procedure setAnimationSpeed(animationSpeed: Single); cdecl;
    procedure setCategoryBitMask(categoryBitMask: UInt32); cdecl;
    procedure setDirection(direction: vector_float3); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setExclusive(exclusive: Boolean); cdecl;
    procedure setFalloff(falloff: Single); cdecl;
    procedure setMinimumRadius(minimumRadius: Single); cdecl;
    procedure setRegion(region: SKRegion); cdecl;
    procedure setSmoothness(smoothness: Single); cdecl;
    procedure setStrength(strength: Single); cdecl;
    procedure setTexture(texture: SKTexture); cdecl;
    function smoothness: Single; cdecl;
    function strength: Single; cdecl;
    function texture: SKTexture; cdecl;
  end;
  TSKFieldNode = class(TOCGenericImport<SKFieldNodeClass, SKFieldNode>) end;

  SKLabelNodeClass = interface(SKNodeClass)
    ['{D5F21F22-9D9A-4522-A9E4-601BBF28D403}']
    {class} function labelNodeWithAttributedText(attributedText: NSAttributedString): Pointer; cdecl;
    {class} function labelNodeWithFontNamed(fontName: NSString): Pointer; cdecl;
    {class} function labelNodeWithText(text: NSString): Pointer; cdecl;
  end;

  SKLabelNode = interface(SKNode)
    ['{D4726AC1-537E-4FFB-9266-EDC34A899AE4}']
    function attributedText: NSAttributedString; cdecl;
    function blendMode: SKBlendMode; cdecl;
    function color: UIColor; cdecl;
    function colorBlendFactor: CGFloat; cdecl;
    function fontColor: UIColor; cdecl;
    function fontName: NSString; cdecl;
    function fontSize: CGFloat; cdecl;
    function horizontalAlignmentMode: SKLabelHorizontalAlignmentMode; cdecl;
    function initWithFontNamed(fontName: NSString): Pointer; cdecl;
    function lineBreakMode: NSLineBreakMode; cdecl;
    function numberOfLines: NSInteger; cdecl;
    function preferredMaxLayoutWidth: CGFloat; cdecl;
    procedure setAttributedText(attributedText: NSAttributedString); cdecl;
    procedure setBlendMode(blendMode: SKBlendMode); cdecl;
    procedure setColor(color: UIColor); cdecl;
    procedure setColorBlendFactor(colorBlendFactor: CGFloat); cdecl;
    procedure setFontColor(fontColor: UIColor); cdecl;
    procedure setFontName(fontName: NSString); cdecl;
    procedure setFontSize(fontSize: CGFloat); cdecl;
    procedure setHorizontalAlignmentMode(horizontalAlignmentMode: SKLabelHorizontalAlignmentMode); cdecl;
    procedure setLineBreakMode(lineBreakMode: NSLineBreakMode); cdecl;
    procedure setNumberOfLines(numberOfLines: NSInteger); cdecl;
    procedure setPreferredMaxLayoutWidth(preferredMaxLayoutWidth: CGFloat); cdecl;
    procedure setText(text: NSString); cdecl;
    procedure setVerticalAlignmentMode(verticalAlignmentMode: SKLabelVerticalAlignmentMode); cdecl;
    function text: NSString; cdecl;
    function verticalAlignmentMode: SKLabelVerticalAlignmentMode; cdecl;
  end;
  TSKLabelNode = class(TOCGenericImport<SKLabelNodeClass, SKLabelNode>) end;

  SKVideoNodeClass = interface(SKNodeClass)
    ['{61A5E18F-43A9-40EC-B888-890451892D3F}']
    {class} function videoNodeWithAVPlayer(player: AVPlayer): SKVideoNode; cdecl;
    {class} function videoNodeWithFileNamed(videoFile: NSString): SKVideoNode; cdecl;
    {class} function videoNodeWithURL(videoURL: NSURL): SKVideoNode; cdecl;
    {class} function videoNodeWithVideoFileNamed(videoFile: NSString): SKVideoNode; cdecl;
    {class} function videoNodeWithVideoURL(videoURL: NSURL): SKVideoNode; cdecl;
  end;

  SKVideoNode = interface(SKNode)
    ['{C4B30FD7-8CC3-4B8F-A8C1-52C257CDE95C}']
    function anchorPoint: CGPoint; cdecl;
    function initWithAVPlayer(player: AVPlayer): Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithFileNamed(videoFile: NSString): Pointer; cdecl;
    function initWithURL(url: NSURL): Pointer; cdecl;
    function initWithVideoFileNamed(videoFile: NSString): Pointer; cdecl;
    function initWithVideoURL(url: NSURL): Pointer; cdecl;
    procedure pause; cdecl;
    procedure play; cdecl;
    procedure setAnchorPoint(anchorPoint: CGPoint); cdecl;
    procedure setSize(size: CGSize); cdecl;
    function size: CGSize; cdecl;
  end;
  TSKVideoNode = class(TOCGenericImport<SKVideoNodeClass, SKVideoNode>) end;

  SKCropNodeClass = interface(SKNodeClass)
    ['{089ED339-7159-4283-86C2-143933E8A7E6}']
  end;

  SKCropNode = interface(SKNode)
    ['{EDC480D7-8A05-47A4-8359-D8F8C2E0977B}']
    function maskNode: SKNode; cdecl;
    procedure setMaskNode(maskNode: SKNode); cdecl;
  end;
  TSKCropNode = class(TOCGenericImport<SKCropNodeClass, SKCropNode>) end;

  SKLightNodeClass = interface(SKNodeClass)
    ['{8A4CD65B-7412-45B6-8794-6FC5EC92B3D7}']
  end;

  SKLightNode = interface(SKNode)
    ['{8FBB54E9-E40E-4B4B-BA99-896C7BEC8584}']
    function ambientColor: UIColor; cdecl;
    function categoryBitMask: UInt32; cdecl;
    function falloff: CGFloat; cdecl;
    function isEnabled: Boolean; cdecl;
    function lightColor: UIColor; cdecl;
    procedure setAmbientColor(ambientColor: UIColor); cdecl;
    procedure setCategoryBitMask(categoryBitMask: UInt32); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setFalloff(falloff: CGFloat); cdecl;
    procedure setLightColor(lightColor: UIColor); cdecl;
    procedure setShadowColor(shadowColor: UIColor); cdecl;
    function shadowColor: UIColor; cdecl;
  end;
  TSKLightNode = class(TOCGenericImport<SKLightNodeClass, SKLightNode>) end;

  SKReferenceNodeClass = interface(SKNodeClass)
    ['{B12C894D-5A37-4645-8A4D-6929CC36626A}']
    {class} function referenceNodeWithFileNamed(fileName: NSString): Pointer; cdecl;
    {class} function referenceNodeWithURL(referenceURL: NSURL): Pointer; cdecl;
  end;

  SKReferenceNode = interface(SKNode)
    ['{9D71EC4C-1496-4983-A3DD-CAB41A7252A5}']
    procedure didLoadReferenceNode(node: SKNode); cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithFileNamed(fileName: NSString): Pointer; cdecl;
    function initWithURL(url: NSURL): Pointer; cdecl;
    procedure resolveReferenceNode; cdecl;
  end;
  TSKReferenceNode = class(TOCGenericImport<SKReferenceNodeClass, SKReferenceNode>) end;

  SKTransformNodeClass = interface(SKNodeClass)
    ['{5D6422D5-049D-42D1-AC1D-5013D9685640}']
  end;

  SKTransformNode = interface(SKNode)
    ['{E513109F-0558-47F3-9D4D-A7A2A59E560B}']
    function eulerAngles: vector_float3; cdecl;
    function quaternion: simd_quatf; cdecl;
    function rotationMatrix: matrix_float3x3; cdecl;
    procedure setEulerAngles(euler: vector_float3); cdecl;
    procedure setQuaternion(quaternion: simd_quatf); cdecl;
    procedure setRotationMatrix(rotationMatrix: matrix_float3x3); cdecl;
    procedure setXRotation(xRotation: CGFloat); cdecl;
    procedure setYRotation(yRotation: CGFloat); cdecl;
    function xRotation: CGFloat; cdecl;
    function yRotation: CGFloat; cdecl;
  end;
  TSKTransformNode = class(TOCGenericImport<SKTransformNodeClass, SKTransformNode>) end;

  SKRegionClass = interface(NSObjectClass)
    ['{EDFAA987-FCDC-4FED-B429-5CF47071D77A}']
    {class} function infiniteRegion: Pointer; cdecl;
  end;

  SKRegion = interface(NSObject)
    ['{AA3DB7AE-9FD3-42B1-AEF0-E1021C2089ED}']
    function containsPoint(point: CGPoint): Boolean; cdecl;
    function initWithPath(path: CGPathRef): Pointer; cdecl;
    function initWithRadius(radius: Single): Pointer; cdecl;
    function initWithSize(size: CGSize): Pointer; cdecl;
    function inverseRegion: Pointer; cdecl;
    function path: CGPathRef; cdecl;
    function regionByDifferenceFromRegion(region: SKRegion): Pointer; cdecl;
    function regionByIntersectionWithRegion(region: SKRegion): Pointer; cdecl;
    function regionByUnionWithRegion(region: SKRegion): Pointer; cdecl;
  end;
  TSKRegion = class(TOCGenericImport<SKRegionClass, SKRegion>) end;

  SKViewClass = interface(UIViewClass)
    ['{BFD5E7DE-1F08-4FF9-B7CF-70933F1B7B90}']
  end;

  SKView = interface(UIView)
    ['{380F19FF-1CCB-4720-913C-1314ED1918C2}']
    function allowsTransparency: Boolean; cdecl;
    [MethodName('convertPoint:fromScene:')]
    function convertPointFromScene(point: CGPoint; fromScene: SKScene): CGPoint; cdecl;
    [MethodName('convertPoint:toScene:')]
    function convertPointToScene(point: CGPoint; toScene: SKScene): CGPoint; cdecl;
    function delegate: NSObject; cdecl;
    function disableDepthStencilBuffer: Boolean; cdecl;
    function frameInterval: NSInteger; cdecl;
    function ignoresSiblingOrder: Boolean; cdecl;
    function isAsynchronous: Boolean; cdecl;
    function isPaused: Boolean; cdecl;
    function preferredFrameRate: Single; cdecl;
    function preferredFramesPerSecond: NSInteger; cdecl;
    procedure presentScene(scene: SKScene; transition: SKTransition); overload; cdecl;
    procedure presentScene(scene: SKScene); overload; cdecl;
    function scene: SKScene; cdecl;
    procedure setAllowsTransparency(allowsTransparency: Boolean); cdecl;
    procedure setAsynchronous(asynchronous: Boolean); cdecl;
    procedure setDelegate(delegate: NSObject); cdecl;
    procedure setDisableDepthStencilBuffer(disableDepthStencilBuffer: Boolean); cdecl;
    procedure setFrameInterval(frameInterval: NSInteger); cdecl;
    procedure setIgnoresSiblingOrder(ignoresSiblingOrder: Boolean); cdecl;
    procedure setPaused(paused: Boolean); cdecl;
    procedure setPreferredFrameRate(preferredFrameRate: Single); cdecl;
    procedure setPreferredFramesPerSecond(preferredFramesPerSecond: NSInteger); cdecl;
    procedure setShouldCullNonVisibleNodes(shouldCullNonVisibleNodes: Boolean); cdecl;
    procedure setShowsDrawCount(showsDrawCount: Boolean); cdecl;
    procedure setShowsFields(showsFields: Boolean); cdecl;
    procedure setShowsFPS(showsFPS: Boolean); cdecl;
    procedure setShowsNodeCount(showsNodeCount: Boolean); cdecl;
    procedure setShowsPhysics(showsPhysics: Boolean); cdecl;
    procedure setShowsQuadCount(showsQuadCount: Boolean); cdecl;
    function shouldCullNonVisibleNodes: Boolean; cdecl;
    function showsDrawCount: Boolean; cdecl;
    function showsFields: Boolean; cdecl;
    function showsFPS: Boolean; cdecl;
    function showsNodeCount: Boolean; cdecl;
    function showsPhysics: Boolean; cdecl;
    function showsQuadCount: Boolean; cdecl;
    function textureFromNode(node: SKNode): SKTexture; overload; cdecl;
    function textureFromNode(node: SKNode; crop: CGRect): SKTexture; overload; cdecl;
  end;
  TSKView = class(TOCGenericImport<SKViewClass, SKView>) end;

  SKViewDelegate = interface(IObjectiveC)
    ['{B81B7EDD-CC2C-4374-9D40-9EF2ED79C718}']
    function view(view: SKView; shouldRenderAtTime: NSTimeInterval): Boolean; cdecl;
  end;

  SKTransitionClass = interface(NSObjectClass)
    ['{D718E0A8-F280-4D6F-A911-20608E14AD94}']
    {class} function crossFadeWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function doorsCloseHorizontalWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function doorsCloseVerticalWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function doorsOpenHorizontalWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function doorsOpenVerticalWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function doorwayWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function fadeWithColor(color: UIColor; duration: NSTimeInterval): SKTransition; cdecl;
    {class} function fadeWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function flipHorizontalWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function flipVerticalWithDuration(sec: NSTimeInterval): SKTransition; cdecl;
    {class} function moveInWithDirection(direction: SKTransitionDirection; duration: NSTimeInterval): SKTransition; cdecl;
    {class} function pushWithDirection(direction: SKTransitionDirection; duration: NSTimeInterval): SKTransition; cdecl;
    {class} function revealWithDirection(direction: SKTransitionDirection; duration: NSTimeInterval): SKTransition; cdecl;
    {class} function transitionWithCIFilter(filter: CIFilter; duration: NSTimeInterval): SKTransition; cdecl;
  end;

  SKTransition = interface(NSObject)
    ['{B5662C10-E7D9-4071-8547-63FC832F4508}']
    function pausesIncomingScene: Boolean; cdecl;
    function pausesOutgoingScene: Boolean; cdecl;
    procedure setPausesIncomingScene(pausesIncomingScene: Boolean); cdecl;
    procedure setPausesOutgoingScene(pausesOutgoingScene: Boolean); cdecl;
  end;
  TSKTransition = class(TOCGenericImport<SKTransitionClass, SKTransition>) end;

  SKTextureClass = interface(NSObjectClass)
    ['{14ADFB10-1289-46CA-8CD5-B626907D9627}']
    {class} procedure preloadTextures(textures: NSArray; withCompletionHandler: TSKTextureBlockMethod1); cdecl;
    {class} function textureNoiseWithSmoothness(smoothness: CGFloat; size: CGSize; grayscale: Boolean): Pointer; cdecl;
    {class} function textureVectorNoiseWithSmoothness(smoothness: CGFloat; size: CGSize): Pointer; cdecl;
    {class} function textureWithCGImage(image: CGImageRef): Pointer; cdecl;
    {class} function textureWithData(pixelData: NSData; size: CGSize; rowLength: Cardinal; alignment: Cardinal): Pointer; overload; cdecl;
    {class} function textureWithData(pixelData: NSData; size: CGSize; flipped: Boolean): Pointer; overload; cdecl;
    {class} function textureWithData(pixelData: NSData; size: CGSize): Pointer; overload; cdecl;
    {class} function textureWithImage(image: UIImage): Pointer; cdecl;
    {class} function textureWithImageNamed(name: NSString): Pointer; cdecl;
    {class} function textureWithRect(rect: CGRect; inTexture: SKTexture): Pointer; cdecl;
  end;

  SKTexture = interface(NSObject)
    ['{5916853B-DD3D-4E4C-AD07-F544D38D04E6}']
    function CGImage: CGImageRef; cdecl;
    function filteringMode: SKTextureFilteringMode; cdecl;
    procedure preloadWithCompletionHandler(completionHandler: TSKTextureBlockMethod1); cdecl;
    procedure setFilteringMode(filteringMode: SKTextureFilteringMode); cdecl;
    procedure setUsesMipmaps(usesMipmaps: Boolean); cdecl;
    function size: CGSize; cdecl;
    function textureByApplyingCIFilter(filter: CIFilter): Pointer; cdecl;
    function textureByGeneratingNormalMap: Pointer; cdecl;
    function textureByGeneratingNormalMapWithSmoothness(smoothness: CGFloat; contrast: CGFloat): Pointer; cdecl;
    function textureRect: CGRect; cdecl;
    function usesMipmaps: Boolean; cdecl;
  end;
  TSKTexture = class(TOCGenericImport<SKTextureClass, SKTexture>) end;

  SKUniformClass = interface(NSObjectClass)
    ['{1137027D-7058-45B5-9759-30F4787F13D1}']
    {class} function uniformWithName(name: NSString; floatVector3: GLKVector3): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; floatVector2: GLKVector2): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; matrixFloat4x4: matrix_float4x4): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; floatVector4: GLKVector4): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; floatMatrix4: GLKMatrix4): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; floatMatrix3: GLKMatrix3): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; floatMatrix2: GLKMatrix2): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; matrixFloat3x3: matrix_float3x3): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; float: Single): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; texture: SKTexture): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; vectorFloat2: vector_float2): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; matrixFloat2x2: matrix_float2x2): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; vectorFloat4: vector_float4): Pointer; overload; cdecl;
    {class} function uniformWithName(name: NSString; vectorFloat3: vector_float3): Pointer; overload; cdecl;
  end;

  SKUniform = interface(NSObject)
    ['{3B1F4B4B-6909-4FC3-84B4-C30EA04A243A}']
    function floatMatrix2Value: GLKMatrix2; cdecl;
    function floatMatrix3Value: GLKMatrix3; cdecl;
    function floatMatrix4Value: GLKMatrix4; cdecl;
    function floatValue: Single; cdecl;
    function floatVector2Value: GLKVector2; cdecl;
    function floatVector3Value: GLKVector3; cdecl;
    function floatVector4Value: GLKVector4; cdecl;
    function initWithName(name: NSString; float: Single): Pointer; overload; cdecl;
    function initWithName(name: NSString; floatMatrix4: GLKMatrix4): Pointer; overload; cdecl;
    function initWithName(name: NSString; floatMatrix3: GLKMatrix3): Pointer; overload; cdecl;
    function initWithName(name: NSString): Pointer; overload; cdecl;
    function initWithName(name: NSString; texture: SKTexture): Pointer; overload; cdecl;
    function initWithName(name: NSString; vectorFloat2: vector_float2): Pointer; overload; cdecl;
    function initWithName(name: NSString; matrixFloat3x3: matrix_float3x3): Pointer; overload; cdecl;
    function initWithName(name: NSString; matrixFloat4x4: matrix_float4x4): Pointer; overload; cdecl;
    function initWithName(name: NSString; floatVector2: GLKVector2): Pointer; overload; cdecl;
    function initWithName(name: NSString; vectorFloat3: vector_float3): Pointer; overload; cdecl;
    function initWithName(name: NSString; vectorFloat4: vector_float4): Pointer; overload; cdecl;
    function initWithName(name: NSString; matrixFloat2x2: matrix_float2x2): Pointer; overload; cdecl;
    function initWithName(name: NSString; floatVector3: GLKVector3): Pointer; overload; cdecl;
    function initWithName(name: NSString; floatVector4: GLKVector4): Pointer; overload; cdecl;
    function initWithName(name: NSString; floatMatrix2: GLKMatrix2): Pointer; overload; cdecl;
    function matrixFloat2x2Value: matrix_float2x2; cdecl;
    function matrixFloat3x3Value: matrix_float3x3; cdecl;
    function matrixFloat4x4Value: matrix_float4x4; cdecl;
    function name: NSString; cdecl;
    procedure setFloatMatrix2Value(floatMatrix2Value: GLKMatrix2); cdecl;
    procedure setFloatMatrix3Value(floatMatrix3Value: GLKMatrix3); cdecl;
    procedure setFloatMatrix4Value(floatMatrix4Value: GLKMatrix4); cdecl;
    procedure setFloatValue(floatValue: Single); cdecl;
    procedure setFloatVector2Value(floatVector2Value: GLKVector2); cdecl;
    procedure setFloatVector3Value(floatVector3Value: GLKVector3); cdecl;
    procedure setFloatVector4Value(floatVector4Value: GLKVector4); cdecl;
    procedure setMatrixFloat2x2Value(matrixFloat2x2Value: matrix_float2x2); cdecl;
    procedure setMatrixFloat3x3Value(matrixFloat3x3Value: matrix_float3x3); cdecl;
    procedure setMatrixFloat4x4Value(matrixFloat4x4Value: matrix_float4x4); cdecl;
    procedure setTextureValue(textureValue: SKTexture); cdecl;
    procedure setVectorFloat2Value(vectorFloat2Value: vector_float2); cdecl;
    procedure setVectorFloat3Value(vectorFloat3Value: vector_float3); cdecl;
    procedure setVectorFloat4Value(vectorFloat4Value: vector_float4); cdecl;
    function textureValue: SKTexture; cdecl;
    function uniformType: SKUniformType; cdecl;
    function vectorFloat2Value: vector_float2; cdecl;
    function vectorFloat3Value: vector_float3; cdecl;
    function vectorFloat4Value: vector_float4; cdecl;
  end;
  TSKUniform = class(TOCGenericImport<SKUniformClass, SKUniform>) end;

  SKRendererClass = interface(NSObjectClass)
    ['{77D8D036-AC9B-4AE0-B2C1-E68E7A0A0C40}']
    {class} function rendererWithDevice(device: Pointer): SKRenderer; cdecl;
  end;

  SKRenderer = interface(NSObject)
    ['{15550D5B-662A-4066-AC92-98C74423650E}']
    function ignoresSiblingOrder: Boolean; cdecl;
    procedure renderWithViewport(viewport: CGRect; renderCommandEncoder: Pointer; renderPassDescriptor: MTLRenderPassDescriptor;
      commandQueue: Pointer); overload; cdecl;
    procedure renderWithViewport(viewport: CGRect; commandBuffer: Pointer; renderPassDescriptor: MTLRenderPassDescriptor); overload; cdecl;
    function scene: SKScene; cdecl;
    procedure setIgnoresSiblingOrder(ignoresSiblingOrder: Boolean); cdecl;
    procedure setScene(scene: SKScene); cdecl;
    procedure setShouldCullNonVisibleNodes(shouldCullNonVisibleNodes: Boolean); cdecl;
    procedure setShowsDrawCount(showsDrawCount: Boolean); cdecl;
    procedure setShowsFields(showsFields: Boolean); cdecl;
    procedure setShowsNodeCount(showsNodeCount: Boolean); cdecl;
    procedure setShowsPhysics(showsPhysics: Boolean); cdecl;
    procedure setShowsQuadCount(showsQuadCount: Boolean); cdecl;
    function shouldCullNonVisibleNodes: Boolean; cdecl;
    function showsDrawCount: Boolean; cdecl;
    function showsFields: Boolean; cdecl;
    function showsNodeCount: Boolean; cdecl;
    function showsPhysics: Boolean; cdecl;
    function showsQuadCount: Boolean; cdecl;
    procedure updateAtTime(currentTime: NSTimeInterval); cdecl;
  end;
  TSKRenderer = class(TOCGenericImport<SKRendererClass, SKRenderer>) end;

  SKTileDefinitionClass = interface(NSObjectClass)
    ['{A99B0DAC-CFE5-4864-AF05-6C0550F4A4DF}']
    {class} function tileDefinitionWithTexture(texture: SKTexture; normalTexture: SKTexture; size: CGSize): Pointer; overload; cdecl;
    {class} function tileDefinitionWithTexture(texture: SKTexture; size: CGSize): Pointer; overload; cdecl;
    {class} function tileDefinitionWithTexture(texture: SKTexture): Pointer; overload; cdecl;
    {class} function tileDefinitionWithTextures(textures: NSArray; normalTextures: NSArray; size: CGSize;
      timePerFrame: CGFloat): Pointer; overload; cdecl;
    {class} function tileDefinitionWithTextures(textures: NSArray; size: CGSize; timePerFrame: CGFloat): Pointer; overload; cdecl;
  end;

  SKTileDefinition = interface(NSObject)
    ['{2166D749-03D7-4336-8043-668535193F82}']
    function flipHorizontally: Boolean; cdecl;
    function flipVertically: Boolean; cdecl;
    function initWithTexture(texture: SKTexture): Pointer; overload; cdecl;
    function initWithTexture(texture: SKTexture; normalTexture: SKTexture; size: CGSize): Pointer; overload; cdecl;
    function initWithTexture(texture: SKTexture; size: CGSize): Pointer; overload; cdecl;
    function initWithTextures(textures: NSArray; normalTextures: NSArray; size: CGSize; timePerFrame: CGFloat): Pointer; overload; cdecl;
    function initWithTextures(textures: NSArray; size: CGSize; timePerFrame: CGFloat): Pointer; overload; cdecl;
    function name: NSString; cdecl;
    function normalTextures: NSArray; cdecl;
    function placementWeight: NSUInteger; cdecl;
    function rotation: SKTileDefinitionRotation; cdecl;
    procedure setFlipHorizontally(flipHorizontally: Boolean); cdecl;
    procedure setFlipVertically(flipVertically: Boolean); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setNormalTextures(normalTextures: NSArray); cdecl;
    procedure setPlacementWeight(placementWeight: NSUInteger); cdecl;
    procedure setRotation(rotation: SKTileDefinitionRotation); cdecl;
    procedure setSize(size: CGSize); cdecl;
    procedure setTextures(textures: NSArray); cdecl;
    procedure setTimePerFrame(timePerFrame: CGFloat); cdecl;
    procedure setUserData(userData: NSMutableDictionary); cdecl;
    function size: CGSize; cdecl;
    function textures: NSArray; cdecl;
    function timePerFrame: CGFloat; cdecl;
    function userData: NSMutableDictionary; cdecl;
  end;
  TSKTileDefinition = class(TOCGenericImport<SKTileDefinitionClass, SKTileDefinition>) end;

  SKTileSetClass = interface(NSObjectClass)
    ['{3B430606-B4CC-43E3-80E7-4D5A6E206F6E}']
    {class} function tileSetFromURL(url: NSURL): Pointer; cdecl;
    {class} function tileSetNamed(name: NSString): Pointer; cdecl;
    {class} function tileSetWithTileGroups(tileGroups: NSArray; tileSetType: SKTileSetType): Pointer; overload; cdecl;
    {class} function tileSetWithTileGroups(tileGroups: NSArray): Pointer; overload; cdecl;
  end;

  SKTileSet = interface(NSObject)
    ['{3172E37B-8B66-46C8-9654-05F07E82BCBC}']
    [MethodName('type')]
    function &type: SKTileSetType; cdecl;
    function defaultTileGroup: SKTileGroup; cdecl;
    function defaultTileSize: CGSize; cdecl;
    function initWithTileGroups(tileGroups: NSArray): Pointer; overload; cdecl;
    function initWithTileGroups(tileGroups: NSArray; tileSetType: SKTileSetType): Pointer; overload; cdecl;
    function name: NSString; cdecl;
    procedure setDefaultTileGroup(defaultTileGroup: SKTileGroup); cdecl;
    procedure setDefaultTileSize(defaultTileSize: CGSize); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setTileGroups(tileGroups: NSArray); cdecl;
    procedure setType(&type: SKTileSetType); cdecl;
    function tileGroups: NSArray; cdecl;
  end;
  TSKTileSet = class(TOCGenericImport<SKTileSetClass, SKTileSet>) end;

  SKTileGroupClass = interface(NSObjectClass)
    ['{DA081793-FC34-44B1-96FF-1670D29F4438}']
    {class} function emptyTileGroup: Pointer; cdecl;
    {class} function tileGroupWithRules(rules: NSArray): Pointer; cdecl;
    {class} function tileGroupWithTileDefinition(tileDefinition: SKTileDefinition): Pointer; cdecl;
  end;

  SKTileGroup = interface(NSObject)
    ['{A3E5A66B-E8EC-4FF0-B4CB-84A49447674D}']
    function initWithRules(rules: NSArray): Pointer; cdecl;
    function initWithTileDefinition(tileDefinition: SKTileDefinition): Pointer; cdecl;
    function name: NSString; cdecl;
    function rules: NSArray; cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setRules(rules: NSArray); cdecl;
  end;
  TSKTileGroup = class(TOCGenericImport<SKTileGroupClass, SKTileGroup>) end;

  SKTileGroupRuleClass = interface(NSObjectClass)
    ['{CD593B92-DB85-4B09-8324-56CCB9B449CC}']
    {class} function tileGroupRuleWithAdjacency(adjacency: SKTileAdjacencyMask; tileDefinitions: NSArray): Pointer; cdecl;
  end;

  SKTileGroupRule = interface(NSObject)
    ['{5DECABBA-8EB7-4A65-A58A-2B4BC08FF156}']
    function adjacency: SKTileAdjacencyMask; cdecl;
    function initWithAdjacency(adjacency: SKTileAdjacencyMask; tileDefinitions: NSArray): Pointer; cdecl;
    function name: NSString; cdecl;
    procedure setAdjacency(adjacency: SKTileAdjacencyMask); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setTileDefinitions(tileDefinitions: NSArray); cdecl;
    function tileDefinitions: NSArray; cdecl;
  end;
  TSKTileGroupRule = class(TOCGenericImport<SKTileGroupRuleClass, SKTileGroupRule>) end;

  SKTileMapNodeClass = interface(SKNodeClass)
    ['{C41615E7-087C-439A-B21B-F42F0FD8C038}']
    {class} function tileMapNodeWithTileSet(tileSet: SKTileSet; columns: NSUInteger; rows: NSUInteger; tileSize: CGSize;
      tileGroupLayout: NSArray): Pointer; overload; cdecl;
    {class} function tileMapNodeWithTileSet(tileSet: SKTileSet; columns: NSUInteger; rows: NSUInteger; tileSize: CGSize;
      fillWithTileGroup: SKTileGroup): Pointer; overload; cdecl;
    {class} function tileMapNodeWithTileSet(tileSet: SKTileSet; columns: NSUInteger; rows: NSUInteger; tileSize: CGSize): Pointer; overload; cdecl;
  end;

  SKTileMapNode = interface(SKNode)
    ['{EB92745E-7BC0-480B-8F17-CF061EA22181}']
    function anchorPoint: CGPoint; cdecl;
    function attributeValues: NSDictionary; cdecl;
    function blendMode: SKBlendMode; cdecl;
    function centerOfTileAtColumn(column: NSUInteger; row: NSUInteger): CGPoint; cdecl;
    function color: UIColor; cdecl;
    function colorBlendFactor: CGFloat; cdecl;
    function enableAutomapping: Boolean; cdecl;
    procedure fillWithTileGroup(tileGroup: SKTileGroup); cdecl;
    function initWithTileSet(tileSet: SKTileSet; columns: NSUInteger; rows: NSUInteger; tileSize: CGSize;
      fillWithTileGroup: SKTileGroup): Pointer; overload; cdecl;
    function initWithTileSet(tileSet: SKTileSet; columns: NSUInteger; rows: NSUInteger; tileSize: CGSize): Pointer; overload; cdecl;
    function initWithTileSet(tileSet: SKTileSet; columns: NSUInteger; rows: NSUInteger; tileSize: CGSize;
      tileGroupLayout: NSArray): Pointer; overload; cdecl;
    function lightingBitMask: UInt32; cdecl;
    function mapSize: CGSize; cdecl;
    function numberOfColumns: NSUInteger; cdecl;
    function numberOfRows: NSUInteger; cdecl;
    procedure setAnchorPoint(anchorPoint: CGPoint); cdecl;
    procedure setAttributeValues(attributeValues: NSDictionary); cdecl;
    procedure setBlendMode(blendMode: SKBlendMode); cdecl;
    procedure setColor(color: UIColor); cdecl;
    procedure setColorBlendFactor(colorBlendFactor: CGFloat); cdecl;
    procedure setEnableAutomapping(enableAutomapping: Boolean); cdecl;
    procedure setLightingBitMask(lightingBitMask: UInt32); cdecl;
    procedure setNumberOfColumns(numberOfColumns: NSUInteger); cdecl;
    procedure setNumberOfRows(numberOfRows: NSUInteger); cdecl;
    procedure setShader(shader: SKShader); cdecl;
    procedure setTileGroup(tileGroup: SKTileGroup; forColumn: NSUInteger; row: NSUInteger); overload; cdecl;
    procedure setTileGroup(tileGroup: SKTileGroup; andTileDefinition: SKTileDefinition; forColumn: NSUInteger; row: NSUInteger); overload; cdecl;
    procedure setTileSet(tileSet: SKTileSet); cdecl;
    procedure setTileSize(tileSize: CGSize); cdecl;
    procedure setValue(value: SKAttributeValue; forAttributeNamed: NSString); cdecl;
    function shader: SKShader; cdecl;
    function tileColumnIndexFromPosition(position: CGPoint): NSUInteger; cdecl;
    function tileDefinitionAtColumn(column: NSUInteger; row: NSUInteger): SKTileDefinition; cdecl;
    function tileGroupAtColumn(column: NSUInteger; row: NSUInteger): SKTileGroup; cdecl;
    function tileRowIndexFromPosition(position: CGPoint): NSUInteger; cdecl;
    function tileSet: SKTileSet; cdecl;
    function tileSize: CGSize; cdecl;
    function valueForAttributeNamed(key: NSString): SKAttributeValue; cdecl;
  end;
  TSKTileMapNode = class(TOCGenericImport<SKTileMapNodeClass, SKTileMapNode>) end;

  SKMutableTextureClass = interface(SKTextureClass)
    ['{CB59CF28-F944-4C70-832B-5A55AC39AA21}']
    {class} function mutableTextureWithSize(size: CGSize): Pointer; cdecl;
  end;

  SKMutableTexture = interface(SKTexture)
    ['{91EE1551-A329-473F-8DC7-561B42262399}']
    function initWithSize(size: CGSize; pixelFormat: Integer): Pointer; overload; cdecl;
    function initWithSize(size: CGSize): Pointer; overload; cdecl;
    procedure modifyPixelDataWithBlock(block: TSKMutableTextureBlockMethod1); cdecl;
  end;
  TSKMutableTexture = class(TOCGenericImport<SKMutableTextureClass, SKMutableTexture>) end;

  SKTextureAtlasClass = interface(NSObjectClass)
    ['{B486B3FB-97A4-4F97-AEDE-AFD934C562B1}']
    {class} function atlasNamed(name: NSString): Pointer; cdecl;
    {class} function atlasWithDictionary(properties: NSDictionary): Pointer; cdecl;
    {class} procedure preloadTextureAtlases(textureAtlases: NSArray; withCompletionHandler: TSKTextureAtlasBlockMethod1); cdecl;
    {class} procedure preloadTextureAtlasesNamed(atlasNames: NSArray; withCompletionHandler: TSKTextureAtlasBlockMethod2); cdecl;
  end;

  SKTextureAtlas = interface(NSObject)
    ['{F28FE0D7-0189-4B1D-8925-85FCF94393C5}']
    procedure preloadWithCompletionHandler(completionHandler: TSKTextureAtlasBlockMethod1); cdecl;
    function textureNamed(name: NSString): SKTexture; cdecl;
    function textureNames: NSArray; cdecl;
  end;
  TSKTextureAtlas = class(TOCGenericImport<SKTextureAtlasClass, SKTextureAtlas>) end;

  SKRangeClass = interface(NSObjectClass)
    ['{462171B7-FFB6-4B70-9AD8-4242A64205BE}']
    {class} function rangeWithConstantValue(value: CGFloat): Pointer; cdecl;
    {class} function rangeWithLowerLimit(lower: CGFloat): Pointer; overload; cdecl;
    {class} function rangeWithLowerLimit(lower: CGFloat; upperLimit: CGFloat): Pointer; overload; cdecl;
    {class} function rangeWithNoLimits: Pointer; cdecl;
    {class} function rangeWithUpperLimit(upper: CGFloat): Pointer; cdecl;
    {class} function rangeWithValue(value: CGFloat; variance: CGFloat): Pointer; cdecl;
  end;

  SKRange = interface(NSObject)
    ['{285B4873-1DF6-4D0F-AA44-CEE2516CA4FE}']
    function initWithLowerLimit(lower: CGFloat; upperLimit: CGFloat): Pointer; cdecl;
    function lowerLimit: CGFloat; cdecl;
    procedure setLowerLimit(lowerLimit: CGFloat); cdecl;
    procedure setUpperLimit(upperLimit: CGFloat); cdecl;
    function upperLimit: CGFloat; cdecl;
  end;
  TSKRange = class(TOCGenericImport<SKRangeClass, SKRange>) end;

  SKConstraintClass = interface(NSObjectClass)
    ['{BCEF7660-C88C-40CC-AB7A-E6C39A7A8DB8}']
    {class} function distance(range: SKRange; toNode: SKNode): Pointer; overload; cdecl;
    {class} function distance(range: SKRange; toPoint: CGPoint): Pointer; overload; cdecl;
    {class} function distance(range: SKRange; toPoint: CGPoint; inNode: SKNode): Pointer; overload; cdecl;
    {class} function orientToNode(node: SKNode; offset: SKRange): Pointer; cdecl;
    {class} function orientToPoint(point: CGPoint; offset: SKRange): Pointer; overload; cdecl;
    {class} function orientToPoint(point: CGPoint; inNode: SKNode; offset: SKRange): Pointer; overload; cdecl;
    {class} function positionX(range: SKRange): Pointer; overload; cdecl;
    {class} function positionX(xRange: SKRange; Y: SKRange): Pointer; overload; cdecl;
    {class} function positionY(range: SKRange): Pointer; cdecl;
    {class} function zRotation(zRange: SKRange): Pointer; cdecl;
  end;

  SKConstraint = interface(NSObject)
    ['{7CB160B9-8088-4524-A325-F7CE82254B63}']
    function enabled: Boolean; cdecl;
    function referenceNode: SKNode; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setReferenceNode(referenceNode: SKNode); cdecl;
  end;
  TSKConstraint = class(TOCGenericImport<SKConstraintClass, SKConstraint>) end;

  SKReachConstraintsClass = interface(NSObjectClass)
    ['{636B4E06-F8E9-48ED-9660-2842ADD3D36E}']
  end;

  SKReachConstraints = interface(NSObject)
    ['{4701FD38-4121-441D-8CA8-4338361F81A8}']
    function initWithLowerAngleLimit(lowerAngleLimit: CGFloat; upperAngleLimit: CGFloat): Pointer; cdecl;
    function lowerAngleLimit: CGFloat; cdecl;
    procedure setLowerAngleLimit(lowerAngleLimit: CGFloat); cdecl;
    procedure setUpperAngleLimit(upperAngleLimit: CGFloat); cdecl;
    function upperAngleLimit: CGFloat; cdecl;
  end;
  TSKReachConstraints = class(TOCGenericImport<SKReachConstraintsClass, SKReachConstraints>) end;

  SKPhysicsBodyClass = interface(NSObjectClass)
    ['{CEB4AB66-EC54-4126-ACD9-57587C5F480A}']
    {class} function bodyWithBodies(bodies: NSArray): SKPhysicsBody; cdecl;
    {class} function bodyWithCircleOfRadius(r: CGFloat): SKPhysicsBody; overload; cdecl;
    {class} function bodyWithCircleOfRadius(r: CGFloat; center: CGPoint): SKPhysicsBody; overload; cdecl;
    {class} function bodyWithEdgeChainFromPath(path: CGPathRef): SKPhysicsBody; cdecl;
    {class} function bodyWithEdgeFromPoint(p1: CGPoint; toPoint: CGPoint): SKPhysicsBody; cdecl;
    {class} function bodyWithEdgeLoopFromPath(path: CGPathRef): SKPhysicsBody; cdecl;
    {class} function bodyWithEdgeLoopFromRect(rect: CGRect): SKPhysicsBody; cdecl;
    {class} function bodyWithPolygonFromPath(path: CGPathRef): SKPhysicsBody; cdecl;
    {class} function bodyWithRectangleOfSize(s: CGSize; center: CGPoint): SKPhysicsBody; overload; cdecl;
    {class} function bodyWithRectangleOfSize(s: CGSize): SKPhysicsBody; overload; cdecl;
    {class} function bodyWithTexture(texture: SKTexture; alphaThreshold: Single; size: CGSize): SKPhysicsBody; overload; cdecl;
    {class} function bodyWithTexture(texture: SKTexture; size: CGSize): SKPhysicsBody; overload; cdecl;
  end;

  SKPhysicsBody = interface(NSObject)
    ['{8E515A45-F62E-43F7-979B-C67FEBBD70F7}']
    function affectedByGravity: Boolean; cdecl;
    function allContactedBodies: NSArray; cdecl;
    function allowsRotation: Boolean; cdecl;
    function angularDamping: CGFloat; cdecl;
    function angularVelocity: CGFloat; cdecl;
    procedure applyAngularImpulse(impulse: CGFloat); cdecl;
    procedure applyForce(force: CGVector; atPoint: CGPoint); overload; cdecl;
    procedure applyForce(force: CGVector); overload; cdecl;
    procedure applyImpulse(impulse: CGVector); overload; cdecl;
    procedure applyImpulse(impulse: CGVector; atPoint: CGPoint); overload; cdecl;
    procedure applyTorque(torque: CGFloat); cdecl;
    function area: CGFloat; cdecl;
    function categoryBitMask: UInt32; cdecl;
    function charge: CGFloat; cdecl;
    function collisionBitMask: UInt32; cdecl;
    function contactTestBitMask: UInt32; cdecl;
    function density: CGFloat; cdecl;
    function fieldBitMask: UInt32; cdecl;
    function friction: CGFloat; cdecl;
    function isDynamic: Boolean; cdecl;
    function isResting: Boolean; cdecl;
    function joints: NSArray; cdecl;
    function linearDamping: CGFloat; cdecl;
    function mass: CGFloat; cdecl;
    function node: SKNode; cdecl;
    function pinned: Boolean; cdecl;
    function restitution: CGFloat; cdecl;
    procedure setAffectedByGravity(affectedByGravity: Boolean); cdecl;
    procedure setAllowsRotation(allowsRotation: Boolean); cdecl;
    procedure setAngularDamping(angularDamping: CGFloat); cdecl;
    procedure setAngularVelocity(angularVelocity: CGFloat); cdecl;
    procedure setCategoryBitMask(categoryBitMask: UInt32); cdecl;
    procedure setCharge(charge: CGFloat); cdecl;
    procedure setCollisionBitMask(collisionBitMask: UInt32); cdecl;
    procedure setContactTestBitMask(contactTestBitMask: UInt32); cdecl;
    procedure setDensity(density: CGFloat); cdecl;
    procedure setDynamic(dynamic: Boolean); cdecl;
    procedure setFieldBitMask(fieldBitMask: UInt32); cdecl;
    procedure setFriction(friction: CGFloat); cdecl;
    procedure setLinearDamping(linearDamping: CGFloat); cdecl;
    procedure setMass(mass: CGFloat); cdecl;
    procedure setPinned(pinned: Boolean); cdecl;
    procedure setResting(resting: Boolean); cdecl;
    procedure setRestitution(restitution: CGFloat); cdecl;
    procedure setUsesPreciseCollisionDetection(usesPreciseCollisionDetection: Boolean); cdecl;
    procedure setVelocity(velocity: CGVector); cdecl;
    function usesPreciseCollisionDetection: Boolean; cdecl;
    function velocity: CGVector; cdecl;
  end;
  TSKPhysicsBody = class(TOCGenericImport<SKPhysicsBodyClass, SKPhysicsBody>) end;

  SKPhysicsJointClass = interface(NSObjectClass)
    ['{AAFFF49E-190B-4909-B69A-7C6B3E2A03ED}']
  end;

  SKPhysicsJoint = interface(NSObject)
    ['{85ABF12C-A4D9-4FE9-B062-F99422ACC2F3}']
    function bodyA: SKPhysicsBody; cdecl;
    function bodyB: SKPhysicsBody; cdecl;
    function reactionForce: CGVector; cdecl;
    function reactionTorque: CGFloat; cdecl;
    procedure setBodyA(bodyA: SKPhysicsBody); cdecl;
    procedure setBodyB(bodyB: SKPhysicsBody); cdecl;
  end;
  TSKPhysicsJoint = class(TOCGenericImport<SKPhysicsJointClass, SKPhysicsJoint>) end;

  SKPhysicsJointPinClass = interface(SKPhysicsJointClass)
    ['{3F86012A-EB2B-4ABE-A736-A48FA2BEAD95}']
    {class} function jointWithBodyA(bodyA: SKPhysicsBody; bodyB: SKPhysicsBody; anchor: CGPoint): SKPhysicsJointPin; cdecl;
  end;

  SKPhysicsJointPin = interface(SKPhysicsJoint)
    ['{8DE96C29-AF85-4AEF-AA93-ECAFF936EC44}']
    function frictionTorque: CGFloat; cdecl;
    function lowerAngleLimit: CGFloat; cdecl;
    function rotationSpeed: CGFloat; cdecl;
    procedure setFrictionTorque(frictionTorque: CGFloat); cdecl;
    procedure setLowerAngleLimit(lowerAngleLimit: CGFloat); cdecl;
    procedure setRotationSpeed(rotationSpeed: CGFloat); cdecl;
    procedure setShouldEnableLimits(shouldEnableLimits: Boolean); cdecl;
    procedure setUpperAngleLimit(upperAngleLimit: CGFloat); cdecl;
    function shouldEnableLimits: Boolean; cdecl;
    function upperAngleLimit: CGFloat; cdecl;
  end;
  TSKPhysicsJointPin = class(TOCGenericImport<SKPhysicsJointPinClass, SKPhysicsJointPin>) end;

  SKPhysicsJointSpringClass = interface(SKPhysicsJointClass)
    ['{3BBD7813-928E-4A8E-9B26-E32402E7C903}']
    {class} function jointWithBodyA(bodyA: SKPhysicsBody; bodyB: SKPhysicsBody; anchorA: CGPoint; anchorB: CGPoint): SKPhysicsJointSpring; cdecl;
  end;

  SKPhysicsJointSpring = interface(SKPhysicsJoint)
    ['{91565458-C96A-498E-AEA7-8DE7209CAFAD}']
    function damping: CGFloat; cdecl;
    function frequency: CGFloat; cdecl;
    procedure setDamping(damping: CGFloat); cdecl;
    procedure setFrequency(frequency: CGFloat); cdecl;
  end;
  TSKPhysicsJointSpring = class(TOCGenericImport<SKPhysicsJointSpringClass, SKPhysicsJointSpring>) end;

  SKPhysicsJointFixedClass = interface(SKPhysicsJointClass)
    ['{43C0BF62-A8A3-4066-B5C4-EE3A2B5F164D}']
    {class} function jointWithBodyA(bodyA: SKPhysicsBody; bodyB: SKPhysicsBody; anchor: CGPoint): SKPhysicsJointFixed; cdecl;
  end;

  SKPhysicsJointFixed = interface(SKPhysicsJoint)
    ['{30D9932E-1DE4-4172-8E1B-53EC71112BA5}']
  end;
  TSKPhysicsJointFixed = class(TOCGenericImport<SKPhysicsJointFixedClass, SKPhysicsJointFixed>) end;

  SKPhysicsJointSlidingClass = interface(SKPhysicsJointClass)
    ['{72EA3C51-E145-4633-8F2D-5B398274297F}']
    {class} function jointWithBodyA(bodyA: SKPhysicsBody; bodyB: SKPhysicsBody; anchor: CGPoint; axis: CGVector): SKPhysicsJointSliding; cdecl;
  end;

  SKPhysicsJointSliding = interface(SKPhysicsJoint)
    ['{84636150-CAC6-49A0-9D1D-AAA0AEED4F79}']
    function lowerDistanceLimit: CGFloat; cdecl;
    procedure setLowerDistanceLimit(lowerDistanceLimit: CGFloat); cdecl;
    procedure setShouldEnableLimits(shouldEnableLimits: Boolean); cdecl;
    procedure setUpperDistanceLimit(upperDistanceLimit: CGFloat); cdecl;
    function shouldEnableLimits: Boolean; cdecl;
    function upperDistanceLimit: CGFloat; cdecl;
  end;
  TSKPhysicsJointSliding = class(TOCGenericImport<SKPhysicsJointSlidingClass, SKPhysicsJointSliding>) end;

  SKPhysicsJointLimitClass = interface(SKPhysicsJointClass)
    ['{BFD86759-1B49-4CC8-B62A-3323CA201899}']
    {class} function jointWithBodyA(bodyA: SKPhysicsBody; bodyB: SKPhysicsBody; anchorA: CGPoint; anchorB: CGPoint): SKPhysicsJointLimit; cdecl;
  end;

  SKPhysicsJointLimit = interface(SKPhysicsJoint)
    ['{C57A0D4E-01BF-4116-8F05-F7DD4ABA9EBA}']
    function maxLength: CGFloat; cdecl;
    procedure setMaxLength(maxLength: CGFloat); cdecl;
  end;
  TSKPhysicsJointLimit = class(TOCGenericImport<SKPhysicsJointLimitClass, SKPhysicsJointLimit>) end;

  SKPhysicsContactClass = interface(NSObjectClass)
    ['{B73FA232-8D34-4E33-957E-4459F0005B40}']
  end;

  SKPhysicsContact = interface(NSObject)
    ['{FD34A950-E1DC-4868-8584-621718671FF4}']
    function bodyA: SKPhysicsBody; cdecl;
    function bodyB: SKPhysicsBody; cdecl;
    function collisionImpulse: CGFloat; cdecl;
    function contactNormal: CGVector; cdecl;
    function contactPoint: CGPoint; cdecl;
  end;
  TSKPhysicsContact = class(TOCGenericImport<SKPhysicsContactClass, SKPhysicsContact>) end;

  SKPhysicsContactDelegate = interface(IObjectiveC)
    ['{E480A60D-6497-4131-8536-63A1A7C2F315}']
    procedure didBeginContact(contact: SKPhysicsContact); cdecl;
    procedure didEndContact(contact: SKPhysicsContact); cdecl;
  end;

  SKPhysicsWorldClass = interface(NSObjectClass)
    ['{149667C5-435E-4AEC-9F0F-23E45224128D}']
  end;

  SKPhysicsWorld = interface(NSObject)
    ['{28595B07-1C99-4B13-A2E3-2906DDC9B27F}']
    procedure addJoint(joint: SKPhysicsJoint); cdecl;
    function bodyAlongRayStart(start: CGPoint; &end: CGPoint): SKPhysicsBody; cdecl;
    function bodyAtPoint(point: CGPoint): SKPhysicsBody; cdecl;
    function bodyInRect(rect: CGRect): SKPhysicsBody; cdecl;
    function contactDelegate: Pointer; cdecl;
    procedure enumerateBodiesAlongRayStart(start: CGPoint; &end: CGPoint; usingBlock: TSKPhysicsWorldBlockMethod2); cdecl;
    procedure enumerateBodiesAtPoint(point: CGPoint; usingBlock: TSKPhysicsWorldBlockMethod1); cdecl;
    procedure enumerateBodiesInRect(rect: CGRect; usingBlock: TSKPhysicsWorldBlockMethod1); cdecl;
    function gravity: CGVector; cdecl;
    procedure removeAllJoints; cdecl;
    procedure removeJoint(joint: SKPhysicsJoint); cdecl;
    function sampleFieldsAt(position: vector_float3): vector_float3; cdecl;
    procedure setContactDelegate(contactDelegate: Pointer); cdecl;
    procedure setGravity(gravity: CGVector); cdecl;
    procedure setSpeed(speed: CGFloat); cdecl;
    function speed: CGFloat; cdecl;
  end;
  TSKPhysicsWorld = class(TOCGenericImport<SKPhysicsWorldClass, SKPhysicsWorld>) end;

  SKAudioNodeClass = interface(SKNodeClass)
    ['{F5A72350-1F62-42AF-9408-302F18DF2FA3}']
  end;

  SKAudioNode = interface(SKNode)
    ['{D1E4C90C-3164-47C5-9DF6-C7D5564D4811}']
    function autoplayLooped: Boolean; cdecl;
    function avAudioNode: AVAudioNode; cdecl;
    function initWithAVAudioNode(node: AVAudioNode): Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithFileNamed(name: NSString): Pointer; cdecl;
    function initWithURL(url: NSURL): Pointer; cdecl;
    function isPositional: Boolean; cdecl;
    procedure setAutoplayLooped(autoplayLooped: Boolean); cdecl;
    procedure setAvAudioNode(avAudioNode: AVAudioNode); cdecl;
    procedure setPositional(positional: Boolean); cdecl;
  end;
  TSKAudioNode = class(TOCGenericImport<SKAudioNodeClass, SKAudioNode>) end;

const
  libSpriteKit = '/System/Library/Frameworks/SpriteKit.framework/SpriteKit';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  SpriteKitModule: THandle;
{$ENDIF}

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  SpriteKitModule := dlopen(MarshaledAString(libSpriteKit), RTLD_LAZY);

finalization
  dlclose(SpriteKitModule)
{$ENDIF}

end.