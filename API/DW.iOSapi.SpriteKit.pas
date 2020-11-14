unit DW.iOSapi.SpriteKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics, iOSapi.CoreImage, iOSapi.GLKit,
  // DW
  DW.Macapi.Simd, DW.iOSapi.AVFoundation, DW.iOSapi.Metal;

const
  SKColor = UIColor;
  SKNodeFocusBehaviorNone = 0;
  SKNodeFocusBehaviorOccluding = 1;
  SKNodeFocusBehaviorFocusable = 2;
  SKBlendModeAlpha = 0;
  SKBlendModeAdd = 1;
  SKBlendModeSubtract = 2;
  SKBlendModeMultiply = 3;
  SKBlendModeMultiplyX2 = 4;
  SKBlendModeScreen = 5;
  SKBlendModeReplace = 6;
  SKBlendModeMultiplyAlpha = 7;
  SKActionTimingLinear = 0;
  SKActionTimingEaseIn = 1;
  SKActionTimingEaseOut = 2;
  SKActionTimingEaseInEaseOut = 3;
  SKUniformTypeNone = 0;
  SKUniformTypeFloat = 1;
  SKUniformTypeFloatVector2 = 2;
  SKUniformTypeFloatVector3 = 3;
  SKUniformTypeFloatVector4 = 4;
  SKUniformTypeFloatMatrix2 = 5;
  SKUniformTypeFloatMatrix3 = 6;
  SKUniformTypeFloatMatrix4 = 7;
  SKUniformTypeTexture = 8;
  SKSceneScaleModeFill = 0;
  SKSceneScaleModeAspectFill = 1;
  SKSceneScaleModeAspectFit = 2;
  SKSceneScaleModeResizeFill = 3;
  SKTextureFilteringNearest = 0;
  SKTextureFilteringLinear = 1;
  SKAttributeTypeNone = 0;
  SKAttributeTypeFloat = 1;
  SKAttributeTypeVectorFloat2 = 2;
  SKAttributeTypeVectorFloat3 = 3;
  SKAttributeTypeVectorFloat4 = 4;
  SKAttributeTypeHalfFloat = 5;
  SKAttributeTypeVectorHalfFloat2 = 6;
  SKAttributeTypeVectorHalfFloat3 = 7;
  SKAttributeTypeVectorHalfFloat4 = 8;
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
  SKWarpable = interface;
  SKWarpGeometry = interface;
  SKWarpGeometryGrid = interface;
  SKSpriteNode = interface;
  SKAttribute = interface;
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
  SKAudioNode = interface;
  SKTexture = interface;
  SKUniform = interface;
  SKNode = interface;
  SKShader = interface;
  SKAction = interface;
  SKPhysicsBody = interface;
  SKReachConstraints = interface;
  SKAttributeValue = interface;
  SKEffectNode = interface;
  SKScene = interface;
  SKSceneDelegate = interface;
  SKCameraNode = interface;
  SKView = interface;
  SKViewDelegate = interface;
  SKPhysicsWorld = interface;
  SKPhysicsJoint = interface;
  SKPhysicsJointPin = interface;
  SKPhysicsJointSpring = interface;
  SKPhysicsJointFixed = interface;
  SKPhysicsJointSliding = interface;
  SKPhysicsJointLimit = interface;
  SKPhysicsContact = interface;
  SKPhysicsContactDelegate = interface;
  SKTransition = interface;

  SKActionTimingFunction = function(time: Single): Single of object;
  SKFieldForceEvaluator = function(position: vector_float3; velocity: vector_float3; mass: Single; charge: Single;
    deltaTime: NSTimeInterval): vector_float3 of object;
  SKBlendMode = NSInteger;
  SKNodeFocusBehavior = NSInteger;
  SKActionTimingMode = NSInteger;
  SKLabelVerticalAlignmentMode = NSInteger;
  SKLabelHorizontalAlignmentMode = NSInteger;
  SKTransitionDirection = NSInteger;
  SKTextureFilteringMode = NSInteger;
  SKUniformType = NSInteger;
  SKTileDefinitionRotation = NSInteger;
  SKTileSetType = NSInteger;
  SKTileAdjacencyMask = NSInteger;
  SKSceneScaleMode = NSInteger;
  SKAttributeType = NSInteger;
  SKInterpolationMode = NSInteger;
  SKRepeatMode = NSInteger;
  SKParticleRenderOrder = NSInteger;

  TSKNodeBlockMethod1 = procedure(node: SKNode; stop: PBoolean) of object;
  TSKNodeBlockMethod2 = procedure of object;
  TSKActionBlockMethod1 = procedure(node: SKNode; elapsedTime: CGFloat) of object;
  TSKTextureBlockMethod1 = procedure of object;
  TSKMutableTextureBlockMethod1 = procedure(pixelData: Pointer; lengthInBytes: NativeUInt) of object;
  TSKTextureAtlasBlockMethod1 = procedure of object;
  TSKTextureAtlasBlockMethod2 = procedure(error: NSError; foundAtlases: NSArray) of object;
  TSKPhysicsWorldBlockMethod1 = procedure(body: SKPhysicsBody; stop: PBoolean) of object;
  TSKPhysicsWorldBlockMethod2 = procedure(body: SKPhysicsBody; point: CGPoint; normal: CGVector; stop: PBoolean) of object;

  SKWarpable = interface(IObjectiveC)
    ['{842EE0D1-F9A1-4A10-9D3A-223CC4A642B0}']
    procedure setSubdivisionLevels(subdivisionLevels: NSInteger); cdecl;
    procedure setWarpGeometry(warpGeometry: SKWarpGeometry); cdecl;
    function subdivisionLevels: NSInteger; cdecl;
    function warpGeometry: SKWarpGeometry; cdecl;
  end;

  SKWarpGeometryClass = interface(NSObjectClass)
    ['{898960AD-E1B3-448A-AF9F-28D44E656E62}']
  end;

  SKWarpGeometry = interface(NSObject)
    ['{4E6A8AA0-1124-4A3F-9B15-D81EA6B03E4D}']
  end;
  TSKWarpGeometry = class(TOCGenericImport<SKWarpGeometryClass, SKWarpGeometry>) end;

  SKWarpGeometryGridClass = interface(SKWarpGeometryClass)
    ['{3235C17F-3B22-45A9-8536-CADF02B6F8C7}']
  end;

  SKWarpGeometryGrid = interface(SKWarpGeometry)
    ['{32AF6DF0-C14A-45C2-A28D-3DE38F6C9B53}']
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

  SKAttributeClass = interface(NSObjectClass)
    ['{B5510BB5-4F6D-4B8A-B84C-C5D872E4E42F}']
  end;

  SKAttribute = interface(NSObject)
    ['{DB2798C1-A173-4297-B8E5-8E38859D1FF5}']
    [MethodName('type')]
    function &type: SKAttributeType; cdecl;
    function initWithName(name: NSString; &type: SKAttributeType): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TSKAttribute = class(TOCGenericImport<SKAttributeClass, SKAttribute>) end;

  SKKeyframeSequenceClass = interface(NSObjectClass)
    ['{1010975B-1FEA-43EC-AC73-582AE6207839}']
  end;

  SKKeyframeSequence = interface(NSObject)
    ['{341A8327-BB2E-48DC-88BD-40CCC871E191}']
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

  SKNodeClass = interface(UIResponderClass)
    ['{9D603E2C-BAFC-4BC5-B9F9-86B62863DEA1}']
  end;

  SKNode = interface(UIResponder)
    ['{372CC44D-56A0-4B4D-9C4A-2CD51D83CFC0}']
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

  SKSpriteNodeClass = interface(SKNodeClass)
    ['{9127CAA7-B996-4A7C-926E-C160F4161DC8}']
  end;

  SKSpriteNode = interface(SKNode)
    ['{0021C25A-3A03-4509-9FDF-51597BC70ABA}']
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

  SKEmitterNodeClass = interface(SKNodeClass)
    ['{03593D93-7EA9-442E-BF94-8C6BDF46429F}']
  end;

  SKEmitterNode = interface(SKNode)
    ['{35D19936-CD28-4F7C-B9C2-B177AE7BB68A}']
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
    ['{3258D6D7-0BD6-4AED-B44E-C9151E4A468F}']
  end;

  SKShapeNode = interface(SKNode)
    ['{183DED81-C955-42A6-893F-93CA93A3C3A0}']
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
    ['{CEDA47FB-A117-4932-976E-A59A98ECF90E}']
  end;

  SKFieldNode = interface(SKNode)
    ['{CB266392-7C80-4CC9-B727-4ADA42FE8CEC}']
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
    ['{65D552BF-5609-4282-B567-27E4CC9ED79D}']
  end;

  SKLabelNode = interface(SKNode)
    ['{DB475DC6-CA7E-4102-B02C-BF5CAFE9060C}']
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
    ['{30C56BC6-372F-4818-895F-9FF1FD7EE115}']
  end;

  SKVideoNode = interface(SKNode)
    ['{09EC550C-3890-4C59-99ED-B57169EFCD85}']
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
    ['{D2024C49-E7F9-46F3-872D-3C2AD6029D96}']
  end;

  SKCropNode = interface(SKNode)
    ['{7BA1BEE4-26E4-4278-B66F-104D866FA1E3}']
    function maskNode: SKNode; cdecl;
    procedure setMaskNode(maskNode: SKNode); cdecl;
  end;
  TSKCropNode = class(TOCGenericImport<SKCropNodeClass, SKCropNode>) end;

  SKLightNodeClass = interface(SKNodeClass)
    ['{3180353E-FBEC-43EA-975E-C0C01E4B975B}']
  end;

  SKLightNode = interface(SKNode)
    ['{68ACE791-982F-4C24-AF84-87F7118A80AB}']
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
    ['{5D48ACE5-9E4F-4FFA-9ED9-60DC61CD731B}']
  end;

  SKReferenceNode = interface(SKNode)
    ['{C9F9D2C6-01C6-40FF-B60E-6F6DF8686547}']
    procedure didLoadReferenceNode(node: SKNode); cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithFileNamed(fileName: NSString): Pointer; cdecl;
    function initWithURL(url: NSURL): Pointer; cdecl;
    procedure resolveReferenceNode; cdecl;
  end;
  TSKReferenceNode = class(TOCGenericImport<SKReferenceNodeClass, SKReferenceNode>) end;

  SKTransformNodeClass = interface(SKNodeClass)
    ['{022F36BA-BCE2-42F4-BE11-9672BF9F5764}']
  end;

  SKTransformNode = interface(SKNode)
    ['{277828A1-4216-4D2D-A528-C9001EB0AE2C}']
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
    ['{48D3EB55-1E46-42A2-A425-3DA26A498944}']
  end;

  SKRegion = interface(NSObject)
    ['{2F9E3727-2C1E-4BD7-90C2-462CCADD0F5D}']
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

  SKRendererClass = interface(NSObjectClass)
    ['{5ECF2BA6-283E-4699-AC37-3F4376A4A3BA}']
  end;

  SKRenderer = interface(NSObject)
    ['{FDCB213E-7197-45CD-BDD5-DAFFAA9BEA75}']
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
    ['{1AAB87F3-125D-4673-9570-AC0393E26498}']
  end;

  SKTileDefinition = interface(NSObject)
    ['{95ACAF39-65D6-46A9-95A8-3DADA7416540}']
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
    ['{D67B9B73-64FF-4C8A-AE1E-FE0556ECF745}']
  end;

  SKTileSet = interface(NSObject)
    ['{8AEA3BD5-694D-4F70-9D67-42A4FBEC9656}']
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
    ['{F219D977-96C8-45E8-9E2C-062F0F89A096}']
  end;

  SKTileGroup = interface(NSObject)
    ['{7969A58C-095E-4FE2-B4D4-E8924EB1B4ED}']
    function initWithRules(rules: NSArray): Pointer; cdecl;
    function initWithTileDefinition(tileDefinition: SKTileDefinition): Pointer; cdecl;
    function name: NSString; cdecl;
    function rules: NSArray; cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setRules(rules: NSArray); cdecl;
  end;
  TSKTileGroup = class(TOCGenericImport<SKTileGroupClass, SKTileGroup>) end;

  SKTileGroupRuleClass = interface(NSObjectClass)
    ['{F5A7055F-102B-4C21-A7D0-904112FF89BC}']
  end;

  SKTileGroupRule = interface(NSObject)
    ['{4391AD36-3238-4EBC-8ABD-0D8B8A799FDD}']
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
    ['{2B51B0A9-7F28-46A5-8376-BFBB9C82D3CD}']
  end;

  SKTileMapNode = interface(SKNode)
    ['{A6345253-E514-4E27-9AE7-9C38C7F97429}']
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

  SKTextureClass = interface(NSObjectClass)
    ['{F765593F-2149-455F-8931-7AA460C8AB93}']
  end;

  SKTexture = interface(NSObject)
    ['{28D927EF-8E2E-4047-AFAD-96DF39AF5F2F}']
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

  SKMutableTextureClass = interface(SKTextureClass)
    ['{CFA38BDF-4DD2-41DB-B839-473ABA68503A}']
  end;

  SKMutableTexture = interface(SKTexture)
    ['{7B9323CA-1715-41E2-9FE0-455001F55248}']
    function initWithSize(size: CGSize; pixelFormat: Integer): Pointer; overload; cdecl;
    function initWithSize(size: CGSize): Pointer; overload; cdecl;
    procedure modifyPixelDataWithBlock(block: TSKMutableTextureBlockMethod1); cdecl;
  end;
  TSKMutableTexture = class(TOCGenericImport<SKMutableTextureClass, SKMutableTexture>) end;

  SKTextureAtlasClass = interface(NSObjectClass)
    ['{FC49A719-58C6-4F42-9F2E-8743010A0238}']
  end;

  SKTextureAtlas = interface(NSObject)
    ['{3070B324-E13F-4864-9819-E577C0ED734A}']
    procedure preloadWithCompletionHandler(completionHandler: TSKTextureAtlasBlockMethod1); cdecl;
    function textureNamed(name: NSString): SKTexture; cdecl;
    function textureNames: NSArray; cdecl;
  end;
  TSKTextureAtlas = class(TOCGenericImport<SKTextureAtlasClass, SKTextureAtlas>) end;

  SKRangeClass = interface(NSObjectClass)
    ['{0152A0AA-7FBA-4614-AE22-3D0A6439670B}']
  end;

  SKRange = interface(NSObject)
    ['{640E921B-3D9F-4FB8-BB47-A28D1F8A7F2A}']
    function initWithLowerLimit(lower: CGFloat; upperLimit: CGFloat): Pointer; cdecl;
    function lowerLimit: CGFloat; cdecl;
    procedure setLowerLimit(lowerLimit: CGFloat); cdecl;
    procedure setUpperLimit(upperLimit: CGFloat); cdecl;
    function upperLimit: CGFloat; cdecl;
  end;
  TSKRange = class(TOCGenericImport<SKRangeClass, SKRange>) end;

  SKConstraintClass = interface(NSObjectClass)
    ['{7E840F51-32CE-4406-91F9-6F5C970DB018}']
  end;

  SKConstraint = interface(NSObject)
    ['{58A0FBC8-E3A5-4C92-A583-57692958B8D4}']
    function enabled: Boolean; cdecl;
    function referenceNode: SKNode; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setReferenceNode(referenceNode: SKNode); cdecl;
  end;
  TSKConstraint = class(TOCGenericImport<SKConstraintClass, SKConstraint>) end;

  SKAudioNodeClass = interface(SKNodeClass)
    ['{E04041B1-2CD9-48CE-823E-E76D4A333461}']
  end;

  SKAudioNode = interface(SKNode)
    ['{499D1383-D0F3-4710-9AF1-3107874BA340}']
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

  SKUniformClass = interface(NSObjectClass)
    ['{86D7F0CF-D824-4AAF-9A5A-711E338DAC98}']
  end;

  SKUniform = interface(NSObject)
    ['{AC8778CB-D1C0-428B-806D-76842D672B0D}']
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

  SKShaderClass = interface(NSObjectClass)
    ['{C1107AC8-0E12-4A54-B986-32563085C9CC}']
  end;

  SKShader = interface(NSObject)
    ['{A449193C-FA71-4254-8418-89B9CDF3406A}']
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
    ['{B6E6AE35-0631-4C49-BCFD-AB7CA41747D0}']
  end;

  SKAction = interface(NSObject)
    ['{83B74D11-9E1D-461B-A127-FCDAA5223A5B}']
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

    SKPhysicsBodyClass = interface(NSObjectClass)
    ['{0A15B7A4-9FCB-4B71-890D-9B6829D7FB69}']
  end;

  SKPhysicsBody = interface(NSObject)
    ['{08C25679-E511-4780-8E37-402DA0F49CDD}']
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
    ['{1CD3F80A-9025-41F5-A0CE-F631CF00F6CB}']
  end;

  SKPhysicsJoint = interface(NSObject)
    ['{7DCBF004-CF96-42CE-B855-5D23CFD276A9}']
    function bodyA: SKPhysicsBody; cdecl;
    function bodyB: SKPhysicsBody; cdecl;
    function reactionForce: CGVector; cdecl;
    function reactionTorque: CGFloat; cdecl;
    procedure setBodyA(bodyA: SKPhysicsBody); cdecl;
    procedure setBodyB(bodyB: SKPhysicsBody); cdecl;
  end;
  TSKPhysicsJoint = class(TOCGenericImport<SKPhysicsJointClass, SKPhysicsJoint>) end;

  SKReachConstraintsClass = interface(NSObjectClass)
    ['{3876F7C8-7835-4512-AA7F-DB1F123F4FBB}']
  end;

  SKReachConstraints = interface(NSObject)
    ['{FB326C25-B42D-483C-8E6B-E52DAE93E184}']
    function initWithLowerAngleLimit(lowerAngleLimit: CGFloat; upperAngleLimit: CGFloat): Pointer; cdecl;
    function lowerAngleLimit: CGFloat; cdecl;
    procedure setLowerAngleLimit(lowerAngleLimit: CGFloat); cdecl;
    procedure setUpperAngleLimit(upperAngleLimit: CGFloat); cdecl;
    function upperAngleLimit: CGFloat; cdecl;
  end;
  TSKReachConstraints = class(TOCGenericImport<SKReachConstraintsClass, SKReachConstraints>) end;

  SKAttributeValueClass = interface(NSObjectClass)
    ['{EA545B33-8219-499E-B542-DBE258B71964}']
  end;

  SKAttributeValue = interface(NSObject)
    ['{A6C6FCF3-5678-4E1C-878B-F9D7C7C68100}']
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

  SKEffectNodeClass = interface(SKNodeClass)
    ['{2453D11B-BF35-4C12-93D0-70A028EEDACC}']
  end;

  SKEffectNode = interface(SKNode)
    ['{5B9854DF-D939-4C11-B6C0-40216061A238}']
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
    ['{7A39BA27-EC2D-4BCF-AAA0-268DECF9ED78}']
    procedure didApplyConstraintsForScene(scene: SKScene); cdecl;
    procedure didEvaluateActionsForScene(scene: SKScene); cdecl;
    procedure didFinishUpdateForScene(scene: SKScene); cdecl;
    procedure didSimulatePhysicsForScene(scene: SKScene); cdecl;
    procedure update(currentTime: NSTimeInterval; forScene: SKScene); cdecl;
  end;

  SKSceneClass = interface(SKEffectNodeClass)
    ['{10138F18-0267-4473-BDC0-EFD7C0FEE1A6}']
  end;

  SKScene = interface(SKEffectNode)
    ['{81CED3FE-6D48-4A25-86AF-0D3F58699A5D}']
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

  SKCameraNodeClass = interface(SKNodeClass)
    ['{735FB7BB-4DD2-4ACA-A091-B0D86F2B00AC}']
  end;

  SKCameraNode = interface(SKNode)
    ['{BC965E50-661A-4A49-951F-88C0FC40EBC4}']
    function containedNodeSet: NSSet; cdecl;
    function containsNode(node: SKNode): Boolean; cdecl;
  end;
  TSKCameraNode = class(TOCGenericImport<SKCameraNodeClass, SKCameraNode>) end;

  SKViewDelegate = interface(IObjectiveC)
    ['{733B368A-887A-46F3-919C-7B6C61C8CEE3}']
    function view(view: SKView; shouldRenderAtTime: NSTimeInterval): Boolean; cdecl;
  end;

  SKViewClass = interface(UIViewClass)
    ['{6F6C33A1-EF24-450E-866F-72C3AFBD195A}']
  end;

  SKView = interface(UIView)
    ['{716AB9B5-9226-485F-BE0F-0978271E629E}']
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

  SKPhysicsWorldClass = interface(NSObjectClass)
    ['{3B2F7C99-EF26-42F7-AFEA-EA036EBEF0A6}']
  end;

  SKPhysicsWorld = interface(NSObject)
    ['{A19297C7-5B7F-43BB-8B8C-C41AE262E1DB}']
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

  SKPhysicsJointPinClass = interface(SKPhysicsJointClass)
    ['{E0AE76C7-8067-4B93-91AD-8299A2BD1BE1}']
  end;

  SKPhysicsJointPin = interface(SKPhysicsJoint)
    ['{664DEE43-B8C7-402C-9757-357B2E8B14C0}']
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
    ['{178E0271-2ED1-43CA-8EDC-996C42B4C21B}']
  end;

  SKPhysicsJointSpring = interface(SKPhysicsJoint)
    ['{7E2C5B28-EF96-40BB-9CC1-DA199F3C76B7}']
    function damping: CGFloat; cdecl;
    function frequency: CGFloat; cdecl;
    procedure setDamping(damping: CGFloat); cdecl;
    procedure setFrequency(frequency: CGFloat); cdecl;
  end;
  TSKPhysicsJointSpring = class(TOCGenericImport<SKPhysicsJointSpringClass, SKPhysicsJointSpring>) end;

  SKPhysicsJointFixedClass = interface(SKPhysicsJointClass)
    ['{3C720B37-B7A3-42AD-8027-74974C1992A2}']
  end;

  SKPhysicsJointFixed = interface(SKPhysicsJoint)
    ['{2D8E7072-F4E6-4CA6-B0A1-8BD131D64B23}']
  end;
  TSKPhysicsJointFixed = class(TOCGenericImport<SKPhysicsJointFixedClass, SKPhysicsJointFixed>) end;

  SKPhysicsJointSlidingClass = interface(SKPhysicsJointClass)
    ['{0B8F0C8D-9C32-4D04-88B3-5942A2DFD755}']
  end;

  SKPhysicsJointSliding = interface(SKPhysicsJoint)
    ['{2DA08E1A-BDE3-4991-B511-4D66F1543295}']
    function lowerDistanceLimit: CGFloat; cdecl;
    procedure setLowerDistanceLimit(lowerDistanceLimit: CGFloat); cdecl;
    procedure setShouldEnableLimits(shouldEnableLimits: Boolean); cdecl;
    procedure setUpperDistanceLimit(upperDistanceLimit: CGFloat); cdecl;
    function shouldEnableLimits: Boolean; cdecl;
    function upperDistanceLimit: CGFloat; cdecl;
  end;
  TSKPhysicsJointSliding = class(TOCGenericImport<SKPhysicsJointSlidingClass, SKPhysicsJointSliding>) end;

  SKPhysicsJointLimitClass = interface(SKPhysicsJointClass)
    ['{E4BF27C9-5DD6-4768-BCA1-80D74A2F0681}']
  end;

  SKPhysicsJointLimit = interface(SKPhysicsJoint)
    ['{1F6C2DD3-AD20-4ACB-A64E-F4EA69665244}']
    function maxLength: CGFloat; cdecl;
    procedure setMaxLength(maxLength: CGFloat); cdecl;
  end;
  TSKPhysicsJointLimit = class(TOCGenericImport<SKPhysicsJointLimitClass, SKPhysicsJointLimit>) end;

  SKPhysicsContactClass = interface(NSObjectClass)
    ['{5077AF6F-7A84-4DEA-B877-2873E463C879}']
  end;

  SKPhysicsContact = interface(NSObject)
    ['{909E0ECA-5094-4F1A-AB8D-BBA0721E7739}']
    function bodyA: SKPhysicsBody; cdecl;
    function bodyB: SKPhysicsBody; cdecl;
    function collisionImpulse: CGFloat; cdecl;
    function contactNormal: CGVector; cdecl;
    function contactPoint: CGPoint; cdecl;
  end;
  TSKPhysicsContact = class(TOCGenericImport<SKPhysicsContactClass, SKPhysicsContact>) end;

  SKPhysicsContactDelegate = interface(IObjectiveC)
    ['{6725ED28-6FFE-4C8F-A950-B70743795297}']
    procedure didBeginContact(contact: SKPhysicsContact); cdecl;
    procedure didEndContact(contact: SKPhysicsContact); cdecl;
  end;

  SKTransitionClass = interface(NSObjectClass)
    ['{6D07EFB7-330B-4F25-8D8D-F30DE81C536F}']
  end;

  SKTransition = interface(NSObject)
    ['{996E4E6D-9C90-4C77-98A3-067AAC5A57F6}']
    function pausesIncomingScene: Boolean; cdecl;
    function pausesOutgoingScene: Boolean; cdecl;
    procedure setPausesIncomingScene(pausesIncomingScene: Boolean); cdecl;
    procedure setPausesOutgoingScene(pausesOutgoingScene: Boolean); cdecl;
  end;
  TSKTransition = class(TOCGenericImport<SKTransitionClass, SKTransition>) end;

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