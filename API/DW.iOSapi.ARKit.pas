unit DW.iOSapi.ARKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreLocation, iOSapi.CoreGraphics, iOSapi.UIKit, iOSapi.CoreMedia, iOSapi.CoreVideo,
  iOSapi.AVFoundation,
  // DW
  DW.Macapi.Simd, DW.iOSapi.SceneKit, DW.iOSapi.SpriteKit, DW.iOSapi.Metal, DW.iOSapi.AVFoundation;

const
  ARTrackingStateNotAvailable = 0;
  ARTrackingStateLimited = 1;
  ARTrackingStateNormal = 2;
  ARTrackingStateReasonNone = 0;
  ARTrackingStateReasonInitializing = 1;
  ARTrackingStateReasonExcessiveMotion = 2;
  ARTrackingStateReasonInsufficientFeatures = 3;
  ARTrackingStateReasonRelocalizing = 4;
  ARCoachingGoalTracking = 0;
  ARCoachingGoalHorizontalPlane = 1;
  ARCoachingGoalVerticalPlane = 2;
  ARCoachingGoalAnyPlane = 3;
  ARFrameSemanticNone = 0;
  ARFrameSemanticPersonSegmentation = 1;
  ARFrameSemanticPersonSegmentationWithDepth = 3;
  ARFrameSemanticBodyDetection = 4;
  ARFrameSemanticSceneDepth = 8;
  ARFrameSemanticSmoothedSceneDepth = 16;
  ARWorldAlignmentGravity = 0;
  ARWorldAlignmentGravityAndHeading = 1;
  ARWorldAlignmentCamera = 2;
  AREnvironmentTexturingNone = 0;
  AREnvironmentTexturingManual = 1;
  AREnvironmentTexturingAutomatic = 2;
  ARPlaneDetectionNone = 0;
  ARPlaneDetectionHorizontal = 1;
  ARPlaneDetectionVertical = 2;
  ARSceneReconstructionNone = 0;
  ARSceneReconstructionMesh = 1;
  ARSceneReconstructionMeshWithClassification = 3;
  ARGeoTrackingStateNotAvailable = 0;
  ARGeoTrackingStateInitializing = 1;
  ARGeoTrackingStateLocalizing = 2;
  ARGeoTrackingStateLocalized = 3;
  ARGeoTrackingAccuracyUndetermined = 0;
  ARGeoTrackingAccuracyLow = 1;
  ARGeoTrackingAccuracyMedium = 2;
  ARGeoTrackingAccuracyHigh = 3;
  ARGeoTrackingStateReasonNone = 0;
  ARGeoTrackingStateReasonNotAvailableAtLocation = 1;
  ARGeoTrackingStateReasonNeedLocationPermissions = 2;
  ARGeoTrackingStateReasonWorldTrackingUnstable = 3;
  ARGeoTrackingStateReasonWaitingForLocation = 4;
  ARGeoTrackingStateReasonWaitingForAvailabilityCheck = 5;
  ARGeoTrackingStateReasonGeoDataNotLoaded = 6;
  ARGeoTrackingStateReasonDevicePointedTooLow = 7;
  ARGeoTrackingStateReasonVisualLocalizationFailed = 8;
  ARSessionRunOptionResetTracking = 1;
  ARSessionRunOptionRemoveExistingAnchors = 2;
  ARSessionRunOptionStopTrackedRaycasts = 4;
  ARSessionRunOptionResetSceneReconstruction = 8;
  ARCollaborationDataPriorityCritical = 0;
  ARCollaborationDataPriorityOptional = 1;
  ARConfidenceLevelLow = 0;
  ARConfidenceLevelMedium = 1;
  ARConfidenceLevelHigh = 2;
  ARErrorCodeUnsupportedConfiguration = 100;
  ARErrorCodeSensorUnavailable = 101;
  ARErrorCodeSensorFailed = 102;
  ARErrorCodeCameraUnauthorized = 103;
  ARErrorCodeMicrophoneUnauthorized = 104;
  ARErrorCodeLocationUnauthorized = 105;
  ARErrorCodeWorldTrackingFailed = 200;
  ARErrorCodeGeoTrackingNotAvailableAtLocation = 201;
  ARErrorCodeGeoTrackingFailed = 202;
  ARErrorCodeInvalidReferenceImage = 300;
  ARErrorCodeInvalidReferenceObject = 301;
  ARErrorCodeInvalidWorldMap = 302;
  ARErrorCodeInvalidConfiguration = 303;
  ARErrorCodeCollaborationDataUnavailable = 304;
  ARErrorCodeInvalidCollaborationData = 304;
  ARErrorCodeInsufficientFeatures = 400;
  ARErrorCodeObjectMergeFailed = 401;
  ARErrorCodeFileIOFailed = 500;
  ARErrorCodeRequestFailed = 501;
  ARHitTestResultTypeFeaturePoint = 1;
  ARHitTestResultTypeEstimatedHorizontalPlane = 2;
  ARHitTestResultTypeEstimatedVerticalPlane = 4;
  ARHitTestResultTypeExistingPlane = 8;
  ARHitTestResultTypeExistingPlaneUsingExtent = 16;
  ARHitTestResultTypeExistingPlaneUsingGeometry = 32;
  ARRaycastTargetExistingPlaneGeometry = 0;
  ARRaycastTargetExistingPlaneInfinite = 1;
  ARRaycastTargetEstimatedPlane = 2;
  ARRaycastTargetAlignmentHorizontal = 0;
  ARRaycastTargetAlignmentVertical = 1;
  ARRaycastTargetAlignmentAny = 2;
  ARSegmentationClassNone = 0;
  ARSegmentationClassPerson = -1;
  ARWorldMappingStatusNotAvailable = 0;
  ARWorldMappingStatusLimited = 1;
  ARWorldMappingStatusExtending = 2;
  ARWorldMappingStatusMapped = 3;
  ARAltitudeSourceUnknown = 0;
  ARAltitudeSourceCoarse = 1;
  ARAltitudeSourcePrecise = 2;
  ARAltitudeSourceUserDefined = 3;
  ARGeometryPrimitiveTypeLine = 0;
  ARGeometryPrimitiveTypeTriangle = 1;
  ARMeshClassificationNone = 0;
  ARMeshClassificationWall = 1;
  ARMeshClassificationFloor = 2;
  ARMeshClassificationCeiling = 3;
  ARMeshClassificationTable = 4;
  ARMeshClassificationSeat = 5;
  ARMeshClassificationWindow = 6;
  ARMeshClassificationDoor = 7;
  ARPlaneAnchorAlignmentHorizontal = 0;
  ARPlaneAnchorAlignmentVertical = 1;
  ARPlaneClassificationStatusNotAvailable = 0;
  ARPlaneClassificationStatusUndetermined = 1;
  ARPlaneClassificationStatusUnknown = 2;
  ARPlaneClassificationStatusKnown = 3;
  ARPlaneClassificationNone = 0;
  ARPlaneClassificationWall = 1;
  ARPlaneClassificationFloor = 2;
  ARPlaneClassificationCeiling = 3;
  ARPlaneClassificationTable = 4;
  ARPlaneClassificationSeat = 5;
  ARPlaneClassificationWindow = 6;
  ARPlaneClassificationDoor = 7;
  ARMatteResolutionFull = 0;
  ARMatteResolutionHalf = 1;

type
  ARAnchorCopying = interface;
  ARAnchor = interface;
  ARTrackable = interface;
  ARSkeletonDefinition = interface;
  ARSkeleton = interface;
  ARSkeleton3D = interface;
  ARSkeleton2D = interface;
  ARBody2D = interface;
  ARBodyAnchor = interface;
  ARCamera = interface;
  ARCoachingOverlayView = interface;
  ARCoachingOverlayViewDelegate = interface;
  ARConfiguration = interface;
  ARWorldTrackingConfiguration = interface;
  AROrientationTrackingConfiguration = interface;
  ARFaceTrackingConfiguration = interface;
  ARImageTrackingConfiguration = interface;
  ARObjectScanningConfiguration = interface;
  ARBodyTrackingConfiguration = interface;
  ARPositionalTrackingConfiguration = interface;
  ARGeoTrackingConfiguration = interface;
  ARGeoTrackingStatus = interface;
  ARSession = interface;
  ARSessionObserver = interface;
  ARSessionDelegate = interface;
  ARSessionProviding = interface;
  ARCollaborationData = interface;
  ARDepthData = interface;
  AREnvironmentProbeAnchor = interface;
  ARFaceAnchor = interface;
  ARSCNFaceGeometry = interface;
  ARFaceGeometry = interface;
  ARHitTestResult = interface;
  ARRaycastQuery = interface;
  ARFrame = interface;
  ARGeoAnchor = interface;
  ARImageAnchor = interface;
  ARLightEstimate = interface;
  ARDirectionalLightEstimate = interface;
  ARPointCloud = interface;
  ARRaycastResult = interface;
  ARReferenceImage = interface;
  ARReferenceObject = interface;
  ARTrackedRaycast = interface;
  ARVideoFormat = interface;
  ARWorldMap = interface;
  ARMeshAnchor = interface;
  ARGeometrySource = interface;
  ARGeometryElement = interface;
  ARMeshGeometry = interface;
  ARObjectAnchor = interface;
  ARParticipantAnchor = interface;
  ARPlaneAnchor = interface;
  ARSCNPlaneGeometry = interface;
  ARPlaneGeometry = interface;
  ARMatteGenerator = interface;
  ARQuickLookPreviewItem = interface;
  ARSCNView = interface;
  ARSCNViewDelegate = interface;
  ARSKViewDelegate = interface;
  ARSKView = interface;

  PUInt64 = ^UInt64;
  PInt16 = ^Int16;
  ARSkeletonJointName = NSString;
  ARTrackingState = NSInteger;
  ARTrackingStateReason = NSInteger;
  ARCoachingGoal = NSInteger;
  ARFrameSemantics = NSInteger;
  ARWorldAlignment = NSInteger;
  AREnvironmentTexturing = NSInteger;
  ARPlaneDetection = NSInteger;
  ARSceneReconstruction = NSInteger;
  ARGeoTrackingState = NSInteger;
  ARGeoTrackingAccuracy = NSInteger;
  ARGeoTrackingStateReason = NSInteger;
  ARSessionRunOptions = NSInteger;
  ARCollaborationDataPriority = NSInteger;
  ARConfidenceLevel = NSInteger;
  ARErrorCode = NSInteger;
  ARBlendShapeLocation = NSString;
  ARHitTestResultType = NSInteger;
  ARRaycastTarget = NSInteger;
  ARRaycastTargetAlignment = NSInteger;
  ARSegmentationClass = NSInteger;
  ARWorldMappingStatus = NSInteger;
  ARAltitudeSource = NSInteger;
  ARGeometryPrimitiveType = NSInteger;
  ARMeshClassification = NSInteger;
  ARPlaneAnchorAlignment = NSInteger;
  ARPlaneClassificationStatus = NSInteger;
  ARPlaneClassification = NSInteger;
  ARMatteResolution = NSInteger;
  ARSCNDebugOptions = Integer;
  TARGeoTrackingConfigurationBlockMethod1 = procedure(isAvailable: Boolean; error: NSError) of object;
  TARSessionBlockMethod1 = procedure(worldMap: ARWorldMap; error: NSError) of object;
  TARSessionBlockMethod2 = procedure(referenceObject: ARReferenceObject; error: NSError) of object;
  TARSessionBlockMethod3 = procedure(param1: NSArray) of object;
  TARSessionBlockMethod4 = procedure(coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance; error: NSError) of object;
  TARReferenceImageBlockMethod1 = procedure(error: NSError) of object;

  CGImagePropertyOrientation = NSUInteger;
  VNRecognizedPointKey = NSString;

  ARAnchorCopying = interface(IObjectiveC)
    ['{7A669566-AC2C-48C6-AB8F-1A4658208A86}']
    function initWithAnchor(anchor: ARAnchor): Pointer; cdecl;
  end;

  ARAnchorClass = interface(NSObjectClass)
    ['{CC46CB13-767D-462C-B101-4BFFB708F59B}']
    {class} function new: Pointer; cdecl;
  end;

  ARAnchor = interface(NSObject)
    ['{98AD203F-8479-4C28-8A17-B83CC3380859}']
    function identifier: NSUUID; cdecl;
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
    function name: NSString; cdecl;
    function sessionIdentifier: NSUUID; cdecl;
    function transform: simd_float4x4; cdecl;
  end;
  TARAnchor = class(TOCGenericImport<ARAnchorClass, ARAnchor>) end;

  ARTrackable = interface(IObjectiveC)
    ['{0CC03816-1213-44C5-B980-BF24BA0A1CD3}']
    function isTracked: Boolean; cdecl;
  end;

  ARSkeletonDefinitionClass = interface(NSObjectClass)
    ['{03E9896B-920D-4D3B-AFDB-A37D3491FA68}']
    {class} function defaultBody2DSkeletonDefinition: ARSkeletonDefinition; cdecl;
    {class} function defaultBody3DSkeletonDefinition: ARSkeletonDefinition; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  ARSkeletonDefinition = interface(NSObject)
    ['{70BE8919-A1A3-4CD4-BC3D-60DD5ECE476D}']
    function indexForJointName(jointName: ARSkeletonJointName): NSUInteger; cdecl;
    function jointCount: NSUInteger; cdecl;
    function jointNames: NSArray; cdecl;
    function neutralBodySkeleton3D: ARSkeleton3D; cdecl;
    function parentIndices: NSArray; cdecl;
  end;
  TARSkeletonDefinition = class(TOCGenericImport<ARSkeletonDefinitionClass, ARSkeletonDefinition>) end;

  ARSkeletonClass = interface(NSObjectClass)
    ['{1D36662C-FE2F-4E98-9EB7-D8717AAAC755}']
    {class} function new: Pointer; cdecl;
  end;

  ARSkeleton = interface(NSObject)
    ['{5D62FF93-22F4-497A-A78A-EE30CC80D2D1}']
    function definition: ARSkeletonDefinition; cdecl;
    function isJointTracked(jointIndex: NSInteger): Boolean; cdecl;
    function jointCount: NSUInteger; cdecl;
  end;
  TARSkeleton = class(TOCGenericImport<ARSkeletonClass, ARSkeleton>) end;

  ARSkeleton3DClass = interface(ARSkeletonClass)
    ['{3C46C13D-38F4-4A97-84CD-7BC67E0C8194}']
    {class} function new: Pointer; cdecl;
  end;

  ARSkeleton3D = interface(ARSkeleton)
    ['{EE944363-F28B-4D66-9785-804593F2F5BF}']
    function jointLocalTransforms: Psimd_float4x4; cdecl;
    function jointModelTransforms: Psimd_float4x4; cdecl;
    function localTransformForJointName(jointName: ARSkeletonJointName): simd_float4x4; cdecl;
    function modelTransformForJointName(jointName: ARSkeletonJointName): simd_float4x4; cdecl;
  end;
  TARSkeleton3D = class(TOCGenericImport<ARSkeleton3DClass, ARSkeleton3D>) end;

  ARSkeleton2DClass = interface(ARSkeletonClass)
    ['{E26F49F2-41E1-494F-B877-43D9A238E017}']
    {class} function new: Pointer; cdecl;
  end;

  ARSkeleton2D = interface(ARSkeleton)
    ['{81B77071-1B35-4E90-83B4-4A6406266794}']
    function jointLandmarks: Psimd_float2; cdecl;
    function landmarkForJointNamed(jointName: ARSkeletonJointName): simd_float2; cdecl;
  end;
  TARSkeleton2D = class(TOCGenericImport<ARSkeleton2DClass, ARSkeleton2D>) end;

  ARBody2DClass = interface(NSObjectClass)
    ['{D3B67478-4866-4BEA-BCFC-E605FD97908F}']
    {class} function new: Pointer; cdecl;
  end;

  ARBody2D = interface(NSObject)
    ['{72C253D9-2AB3-49C4-9D06-9C2928B818B2}']
    function skeleton: ARSkeleton2D; cdecl;
  end;
  TARBody2D = class(TOCGenericImport<ARBody2DClass, ARBody2D>) end;

  ARBodyAnchorClass = interface(ARAnchorClass)
    ['{7C1B9DC9-38BA-4A3D-8BC7-CBB463B3D275}']
  end;

  ARBodyAnchor = interface(ARAnchor)
    ['{8594DA91-3D3B-416F-8295-FA293DDCAFF9}']
    function estimatedScaleFactor: CGFloat; cdecl;
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
    function skeleton: ARSkeleton3D; cdecl;
  end;
  TARBodyAnchor = class(TOCGenericImport<ARBodyAnchorClass, ARBodyAnchor>) end;

  ARCameraClass = interface(NSObjectClass)
    ['{BE169F36-EE66-4138-A7ED-5C1CF10C3AB1}']
    {class} function new: Pointer; cdecl;
  end;

  ARCamera = interface(NSObject)
    ['{AD4E6C14-48C0-495D-AE4E-06B02989D495}']
    function eulerAngles: simd_float3; cdecl;
    function exposureDuration: NSTimeInterval; cdecl;
    function exposureOffset: Single; cdecl;
    function imageResolution: CGSize; cdecl;
    function intrinsics: simd_float3x3; cdecl;
    function projectionMatrix: simd_float4x4; cdecl;
    function projectionMatrixForOrientation(orientation: UIInterfaceOrientation; viewportSize: CGSize; zNear: CGFloat; zFar: CGFloat): simd_float4x4; cdecl;
    function projectPoint(point: simd_float3; orientation: UIInterfaceOrientation; viewportSize: CGSize): CGPoint; cdecl;
    function trackingState: ARTrackingState; cdecl;
    function trackingStateReason: ARTrackingStateReason; cdecl;
    function transform: simd_float4x4; cdecl;
    function unprojectPoint(point: CGPoint; ontoPlaneWithTransform: simd_float4x4; orientation: UIInterfaceOrientation; viewportSize: CGSize): simd_float3; cdecl;
    function viewMatrixForOrientation(orientation: UIInterfaceOrientation): simd_float4x4; cdecl;
  end;
  TARCamera = class(TOCGenericImport<ARCameraClass, ARCamera>) end;

  ARCoachingOverlayViewClass = interface(UIViewClass)
    ['{DAA992E1-D9A1-4851-8549-9C7A00172D9F}']
  end;

  ARCoachingOverlayView = interface(UIView)
    ['{1BC8C16E-0F5A-4369-9B00-55C99C5A090A}']
    function activatesAutomatically: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function goal: ARCoachingGoal; cdecl;
    function isActive: Boolean; cdecl;
    function session: ARSession; cdecl;
    function sessionProvider: NSObject; cdecl;
    procedure setActivatesAutomatically(activatesAutomatically: Boolean); cdecl;
    procedure setActive(active: Boolean; animated: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setGoal(goal: ARCoachingGoal); cdecl;
    procedure setSession(session: ARSession); cdecl;
    procedure setSessionProvider(sessionProvider: NSObject); cdecl;
  end;
  TARCoachingOverlayView = class(TOCGenericImport<ARCoachingOverlayViewClass, ARCoachingOverlayView>) end;

  ARCoachingOverlayViewDelegate = interface(IObjectiveC)
    ['{711C3408-F501-459A-AE13-B83549F78DA9}']
    procedure coachingOverlayViewDidDeactivate(coachingOverlayView: ARCoachingOverlayView); cdecl;
    procedure coachingOverlayViewDidRequestSessionReset(coachingOverlayView: ARCoachingOverlayView); cdecl;
    procedure coachingOverlayViewWillActivate(coachingOverlayView: ARCoachingOverlayView); cdecl;
  end;

  ARConfigurationClass = interface(NSObjectClass)
    ['{C113AB01-CAA8-4BFD-B21D-111FFE258E75}']
    {class} function isSupported: Boolean; cdecl;
    {class} function new: Pointer; cdecl;
    {class} function supportedVideoFormats: NSArray; cdecl;
    {class} function supportsFrameSemantics(frameSemantics: ARFrameSemantics): Boolean; cdecl;
  end;

  ARConfiguration = interface(NSObject)
    ['{3FBA7CEF-8A67-4AB2-9DE5-9782DC7858D1}']
    function frameSemantics: ARFrameSemantics; cdecl;
    function isLightEstimationEnabled: Boolean; cdecl;
    function providesAudioData: Boolean; cdecl;
    procedure setFrameSemantics(frameSemantics: ARFrameSemantics); cdecl;
    procedure setLightEstimationEnabled(lightEstimationEnabled: Boolean); cdecl;
    procedure setProvidesAudioData(providesAudioData: Boolean); cdecl;
    procedure setVideoFormat(videoFormat: ARVideoFormat); cdecl;
    procedure setWorldAlignment(worldAlignment: ARWorldAlignment); cdecl;
    function videoFormat: ARVideoFormat; cdecl;
    function worldAlignment: ARWorldAlignment; cdecl;
  end;
  TARConfiguration = class(TOCGenericImport<ARConfigurationClass, ARConfiguration>) end;

  ARWorldTrackingConfigurationClass = interface(ARConfigurationClass)
    ['{EC7A6979-3D5D-4F80-B418-1779C916DE75}']
    {class} function new: Pointer; cdecl;
    {class} function supportsSceneReconstruction(sceneReconstruction: ARSceneReconstruction): Boolean; cdecl;
    {class} function supportsUserFaceTracking: Boolean; cdecl;
  end;

  ARWorldTrackingConfiguration = interface(ARConfiguration)
    ['{26A58D0C-AAC2-459A-B1A3-348F8B6920CE}']
    function automaticImageScaleEstimationEnabled: Boolean; cdecl;
    function detectionImages: NSSet; cdecl;
    function detectionObjects: NSSet; cdecl;
    function environmentTexturing: AREnvironmentTexturing; cdecl;
    function initialWorldMap: ARWorldMap; cdecl;
    function isAutoFocusEnabled: Boolean; cdecl;
    function isCollaborationEnabled: Boolean; cdecl;
    function maximumNumberOfTrackedImages: NSInteger; cdecl;
    function planeDetection: ARPlaneDetection; cdecl;
    function sceneReconstruction: ARSceneReconstruction; cdecl;
    procedure setAutoFocusEnabled(autoFocusEnabled: Boolean); cdecl;
    procedure setAutomaticImageScaleEstimationEnabled(automaticImageScaleEstimationEnabled: Boolean); cdecl;
    procedure setCollaborationEnabled(collaborationEnabled: Boolean); cdecl;
    procedure setDetectionImages(detectionImages: NSSet); cdecl;
    procedure setDetectionObjects(detectionObjects: NSSet); cdecl;
    procedure setEnvironmentTexturing(environmentTexturing: AREnvironmentTexturing); cdecl;
    procedure setInitialWorldMap(initialWorldMap: ARWorldMap); cdecl;
    procedure setMaximumNumberOfTrackedImages(maximumNumberOfTrackedImages: NSInteger); cdecl;
    procedure setPlaneDetection(planeDetection: ARPlaneDetection); cdecl;
    procedure setSceneReconstruction(sceneReconstruction: ARSceneReconstruction); cdecl;
    procedure setUserFaceTrackingEnabled(userFaceTrackingEnabled: Boolean); cdecl;
    procedure setWantsHDREnvironmentTextures(wantsHDREnvironmentTextures: Boolean); cdecl;
    function userFaceTrackingEnabled: Boolean; cdecl;
    function wantsHDREnvironmentTextures: Boolean; cdecl;
  end;
  TARWorldTrackingConfiguration = class(TOCGenericImport<ARWorldTrackingConfigurationClass, ARWorldTrackingConfiguration>) end;

  AROrientationTrackingConfigurationClass = interface(ARConfigurationClass)
    ['{ECDAAA79-757E-4592-B098-DBC8D685C958}']
    {class} function new: Pointer; cdecl;
  end;

  AROrientationTrackingConfiguration = interface(ARConfiguration)
    ['{8897613E-8C51-472D-9339-C473FD327645}']
    function isAutoFocusEnabled: Boolean; cdecl;
    procedure setAutoFocusEnabled(autoFocusEnabled: Boolean); cdecl;
  end;
  TAROrientationTrackingConfiguration = class(TOCGenericImport<AROrientationTrackingConfigurationClass, AROrientationTrackingConfiguration>) end;

  ARFaceTrackingConfigurationClass = interface(ARConfigurationClass)
    ['{DEB938FA-4729-4496-BDA3-BDC6BDB7B293}']
    {class} function new: Pointer; cdecl;
    {class} function supportedNumberOfTrackedFaces: NSInteger; cdecl;
    {class} function supportsWorldTracking: Boolean; cdecl;
  end;

  ARFaceTrackingConfiguration = interface(ARConfiguration)
    ['{17B67F6E-0445-49BF-8DF9-96FE681992B8}']
    function isWorldTrackingEnabled: Boolean; cdecl;
    function maximumNumberOfTrackedFaces: NSInteger; cdecl;
    procedure setMaximumNumberOfTrackedFaces(maximumNumberOfTrackedFaces: NSInteger); cdecl;
    procedure setWorldTrackingEnabled(worldTrackingEnabled: Boolean); cdecl;
  end;
  TARFaceTrackingConfiguration = class(TOCGenericImport<ARFaceTrackingConfigurationClass, ARFaceTrackingConfiguration>) end;

  ARImageTrackingConfigurationClass = interface(ARConfigurationClass)
    ['{AED9F46C-3108-4812-BAE7-E141E01AC6B3}']
    {class} function new: Pointer; cdecl;
  end;

  ARImageTrackingConfiguration = interface(ARConfiguration)
    ['{39E26EB8-28BD-4002-8439-A7CE750FA79D}']
    function isAutoFocusEnabled: Boolean; cdecl;
    function maximumNumberOfTrackedImages: NSInteger; cdecl;
    procedure setAutoFocusEnabled(autoFocusEnabled: Boolean); cdecl;
    procedure setMaximumNumberOfTrackedImages(maximumNumberOfTrackedImages: NSInteger); cdecl;
    procedure setTrackingImages(trackingImages: NSSet); cdecl;
    function trackingImages: NSSet; cdecl;
  end;
  TARImageTrackingConfiguration = class(TOCGenericImport<ARImageTrackingConfigurationClass, ARImageTrackingConfiguration>) end;

  ARObjectScanningConfigurationClass = interface(ARConfigurationClass)
    ['{601D9334-47CA-4A87-B7C6-8FDD07B0DA11}']
    {class} function new: Pointer; cdecl;
  end;

  ARObjectScanningConfiguration = interface(ARConfiguration)
    ['{C65A54AE-9006-4FFB-87DB-14066C174C8F}']
    function isAutoFocusEnabled: Boolean; cdecl;
    function planeDetection: ARPlaneDetection; cdecl;
    procedure setAutoFocusEnabled(autoFocusEnabled: Boolean); cdecl;
    procedure setPlaneDetection(planeDetection: ARPlaneDetection); cdecl;
  end;
  TARObjectScanningConfiguration = class(TOCGenericImport<ARObjectScanningConfigurationClass, ARObjectScanningConfiguration>) end;

  ARBodyTrackingConfigurationClass = interface(ARConfigurationClass)
    ['{514FD58F-71C0-4FA0-880C-94B6E8092EE2}']
    {class} function new: Pointer; cdecl;
  end;

  ARBodyTrackingConfiguration = interface(ARConfiguration)
    ['{0D4310AC-F2B4-4B2A-B910-A8921765B9C4}']
    function automaticImageScaleEstimationEnabled: Boolean; cdecl;
    function automaticSkeletonScaleEstimationEnabled: Boolean; cdecl;
    function detectionImages: NSSet; cdecl;
    function environmentTexturing: AREnvironmentTexturing; cdecl;
    function initialWorldMap: ARWorldMap; cdecl;
    function isAutoFocusEnabled: Boolean; cdecl;
    function maximumNumberOfTrackedImages: NSInteger; cdecl;
    function planeDetection: ARPlaneDetection; cdecl;
    procedure setAutoFocusEnabled(autoFocusEnabled: Boolean); cdecl;
    procedure setAutomaticImageScaleEstimationEnabled(automaticImageScaleEstimationEnabled: Boolean); cdecl;
    procedure setAutomaticSkeletonScaleEstimationEnabled(automaticSkeletonScaleEstimationEnabled: Boolean); cdecl;
    procedure setDetectionImages(detectionImages: NSSet); cdecl;
    procedure setEnvironmentTexturing(environmentTexturing: AREnvironmentTexturing); cdecl;
    procedure setInitialWorldMap(initialWorldMap: ARWorldMap); cdecl;
    procedure setMaximumNumberOfTrackedImages(maximumNumberOfTrackedImages: NSInteger); cdecl;
    procedure setPlaneDetection(planeDetection: ARPlaneDetection); cdecl;
    procedure setWantsHDREnvironmentTextures(wantsHDREnvironmentTextures: Boolean); cdecl;
    function wantsHDREnvironmentTextures: Boolean; cdecl;
  end;
  TARBodyTrackingConfiguration = class(TOCGenericImport<ARBodyTrackingConfigurationClass, ARBodyTrackingConfiguration>) end;

  ARPositionalTrackingConfigurationClass = interface(ARConfigurationClass)
    ['{7A1B9481-6C4D-4862-8F1F-250BF7C87CE1}']
    {class} function new: Pointer; cdecl;
  end;

  ARPositionalTrackingConfiguration = interface(ARConfiguration)
    ['{76B4155B-839E-48B7-B920-0D2C0E5E64ED}']
    function initialWorldMap: ARWorldMap; cdecl;
    function planeDetection: ARPlaneDetection; cdecl;
    procedure setInitialWorldMap(initialWorldMap: ARWorldMap); cdecl;
    procedure setPlaneDetection(planeDetection: ARPlaneDetection); cdecl;
  end;
  TARPositionalTrackingConfiguration = class(TOCGenericImport<ARPositionalTrackingConfigurationClass, ARPositionalTrackingConfiguration>) end;

  ARGeoTrackingConfigurationClass = interface(ARConfigurationClass)
    ['{39B62E23-3532-40F5-AFBB-9197C7C311C0}']
    {class} procedure checkAvailabilityAtCoordinate(coordinate: CLLocationCoordinate2D; completionHandler: TARGeoTrackingConfigurationBlockMethod1); cdecl;
    {class} procedure checkAvailabilityWithCompletionHandler(completionHandler: TARGeoTrackingConfigurationBlockMethod1); cdecl;
    {class} function new: Pointer; cdecl;
  end;

  ARGeoTrackingConfiguration = interface(ARConfiguration)
    ['{32C0B7FE-93EA-42F7-8E98-C9E8F35D8F38}']
    function automaticImageScaleEstimationEnabled: Boolean; cdecl;
    function detectionImages: NSSet; cdecl;
    function detectionObjects: NSSet; cdecl;
    function environmentTexturing: AREnvironmentTexturing; cdecl;
    function maximumNumberOfTrackedImages: NSInteger; cdecl;
    function planeDetection: ARPlaneDetection; cdecl;
    procedure setAutomaticImageScaleEstimationEnabled(automaticImageScaleEstimationEnabled: Boolean); cdecl;
    procedure setDetectionImages(detectionImages: NSSet); cdecl;
    procedure setDetectionObjects(detectionObjects: NSSet); cdecl;
    procedure setEnvironmentTexturing(environmentTexturing: AREnvironmentTexturing); cdecl;
    procedure setMaximumNumberOfTrackedImages(maximumNumberOfTrackedImages: NSInteger); cdecl;
    procedure setPlaneDetection(planeDetection: ARPlaneDetection); cdecl;
    procedure setWantsHDREnvironmentTextures(wantsHDREnvironmentTextures: Boolean); cdecl;
    procedure setWorldAlignment(worldAlignment: ARWorldAlignment); cdecl;
    function wantsHDREnvironmentTextures: Boolean; cdecl;
    function worldAlignment: ARWorldAlignment; cdecl;
  end;
  TARGeoTrackingConfiguration = class(TOCGenericImport<ARGeoTrackingConfigurationClass, ARGeoTrackingConfiguration>) end;

  ARGeoTrackingStatusClass = interface(NSObjectClass)
    ['{9B78896E-19AA-4877-A1DC-DB8EC920819A}']
    {class} function new: Pointer; cdecl;
  end;

  ARGeoTrackingStatus = interface(NSObject)
    ['{5C336C2D-7307-451C-AD9F-A05B15A40821}']
    function accuracy: ARGeoTrackingAccuracy; cdecl;
    function state: ARGeoTrackingState; cdecl;
    function stateReason: ARGeoTrackingStateReason; cdecl;
  end;
  TARGeoTrackingStatus = class(TOCGenericImport<ARGeoTrackingStatusClass, ARGeoTrackingStatus>) end;

  ARSessionClass = interface(NSObjectClass)
    ['{D54D400A-20E0-47A6-871F-FA39DE4CDD69}']
  end;

  ARSession = interface(NSObject)
    ['{BEEB6966-2533-4E00-9A55-770B8582C03D}']
    procedure addAnchor(anchor: ARAnchor); cdecl;
    function configuration: ARConfiguration; cdecl;
    procedure createReferenceObjectWithTransform(transform: simd_float4x4; center: simd_float3; extent: simd_float3; completionHandler: TARSessionBlockMethod2); cdecl;
    function currentFrame: ARFrame; cdecl;
    function delegate: Pointer; cdecl;
    function delegateQueue: dispatch_queue_t; cdecl;
    procedure getCurrentWorldMapWithCompletionHandler(completionHandler: TARSessionBlockMethod1); cdecl;
    procedure getGeoLocationForPoint(position: simd_float3; completionHandler: TARSessionBlockMethod4); cdecl;
    function identifier: NSUUID; cdecl;
    procedure pause; cdecl;
    function raycast(query: ARRaycastQuery): NSArray; cdecl;
    procedure removeAnchor(anchor: ARAnchor); cdecl;
    procedure runWithConfiguration(configuration: ARConfiguration); overload; cdecl;
    procedure runWithConfiguration(configuration: ARConfiguration; options: ARSessionRunOptions); overload; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDelegateQueue(delegateQueue: dispatch_queue_t); cdecl;
    procedure setWorldOrigin(relativeTransform: simd_float4x4); cdecl;
    function trackedRaycast(query: ARRaycastQuery; updateHandler: TARSessionBlockMethod3): ARTrackedRaycast; cdecl;
    procedure updateWithCollaborationData(collaborationData: ARCollaborationData); cdecl;
  end;
  TARSession = class(TOCGenericImport<ARSessionClass, ARSession>) end;

  ARSessionObserver = interface(IObjectiveC)
    ['{E1FC0C27-F9F0-4FBD-ADBB-8730612A46F6}']
    procedure session(session: ARSession; didOutputAudioSampleBuffer: CMSampleBufferRef); overload; cdecl;
    procedure session(session: ARSession; didOutputCollaborationData: ARCollaborationData); overload; cdecl;
    procedure session(session: ARSession; didChangeGeoTrackingStatus: ARGeoTrackingStatus); overload; cdecl;
    procedure session(session: ARSession; cameraDidChangeTrackingState: ARCamera); overload; cdecl;
    procedure session(session: ARSession; didFailWithError: NSError); overload; cdecl;
    procedure sessionInterruptionEnded(session: ARSession); cdecl;
    function sessionShouldAttemptRelocalization(session: ARSession): Boolean; cdecl;
    procedure sessionWasInterrupted(session: ARSession); cdecl;
  end;

  ARSessionDelegate = interface(IObjectiveC)
    ['{DEEF9BBE-A44E-4E48-AEF8-7F1572E76D6A}']
    [MethodName('session:didAddAnchors:')]
    procedure sessionDidAddAnchors(session: ARSession; didAddAnchors: NSArray); cdecl;
    [MethodName('session:didRemoveAnchors:')]
    procedure sessionDidRemoveAnchors(session: ARSession; didRemoveAnchors: NSArray); cdecl;
    [MethodName('session:didUpdateAnchors:')]
    procedure sessionDidUpdateAnchors(session: ARSession; didUpdateAnchors: NSArray); cdecl;
    [MethodName('session:didUpdateFrame:')]
    procedure sessionDidUpdateFrame(session: ARSession; didUpdateFrame: ARFrame); cdecl;
  end;

  ARSessionProviding = interface(IObjectiveC)
    ['{54E1AFD0-4BEE-464D-9A8B-2A3FFE70FFB2}']
    function session: ARSession; cdecl;
  end;

  ARCollaborationDataClass = interface(NSObjectClass)
    ['{1868B80F-CD1A-4B5C-B348-6BF78C0DB86B}']
    {class} function new: Pointer; cdecl;
  end;

  ARCollaborationData = interface(NSObject)
    ['{B4C8A5E0-E7FC-4726-853F-F41A9E1EF49D}']
    function priority: ARCollaborationDataPriority; cdecl;
  end;
  TARCollaborationData = class(TOCGenericImport<ARCollaborationDataClass, ARCollaborationData>) end;

  ARDepthDataClass = interface(NSObjectClass)
    ['{51C55FF1-2A28-4EAB-855E-2F78A6A11B71}']
  end;

  ARDepthData = interface(NSObject)
    ['{3268387F-7E20-4AC3-AB03-E0EA3724F31C}']
    function confidenceMap: CVPixelBufferRef; cdecl;
    function depthMap: CVPixelBufferRef; cdecl;
  end;
  TARDepthData = class(TOCGenericImport<ARDepthDataClass, ARDepthData>) end;

  AREnvironmentProbeAnchorClass = interface(ARAnchorClass)
    ['{18BD95BE-EBAA-4F75-A6CA-3028F241BDD8}']
  end;

  AREnvironmentProbeAnchor = interface(ARAnchor)
    ['{AA947F72-8D8C-4998-964F-3879BBCD1256}']
    function environmentTexture: Pointer; cdecl;
    function extent: simd_float3; cdecl;
    function initWithName(name: NSString; transform: simd_float4x4; extent: simd_float3): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4; extent: simd_float3): Pointer; cdecl;
  end;
  TAREnvironmentProbeAnchor = class(TOCGenericImport<AREnvironmentProbeAnchorClass, AREnvironmentProbeAnchor>) end;

  ARFaceAnchorClass = interface(ARAnchorClass)
    ['{8B76C49B-8D1C-480E-932F-55AA1BC9FFD7}']
  end;

  ARFaceAnchor = interface(ARAnchor)
    ['{DD1A00C6-EC30-4C72-85EE-A391FCDC7FD3}']
    function blendShapes: NSDictionary; cdecl;
    function geometry: ARFaceGeometry; cdecl;
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
    function leftEyeTransform: simd_float4x4; cdecl;
    function lookAtPoint: simd_float3; cdecl;
    function rightEyeTransform: simd_float4x4; cdecl;
  end;
  TARFaceAnchor = class(TOCGenericImport<ARFaceAnchorClass, ARFaceAnchor>) end;

  ARSCNFaceGeometryClass = interface(SCNGeometryClass)
    ['{1F40A8C2-6211-4E93-B9EC-1AAA805CBE26}']
    {class} function faceGeometryWithDevice(device: Pointer; fillMesh: Boolean): Pointer; overload; cdecl;
    {class} function faceGeometryWithDevice(device: Pointer): Pointer; overload; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  ARSCNFaceGeometry = interface(SCNGeometry)
    ['{532CDF0A-1F0E-44AD-9BD5-1CE6AA6D13E5}']
    procedure updateFromFaceGeometry(faceGeometry: ARFaceGeometry); cdecl;
  end;
  TARSCNFaceGeometry = class(TOCGenericImport<ARSCNFaceGeometryClass, ARSCNFaceGeometry>) end;

  ARFaceGeometryClass = interface(NSObjectClass)
    ['{FAC59975-252E-4B51-8E04-4A1E625071D3}']
    {class} function new: Pointer; cdecl;
  end;

  ARFaceGeometry = interface(NSObject)
    ['{0FC1101E-56EF-46F9-A6F2-3A98F6C67DF0}']
    function initWithBlendShapes(blendShapes: NSDictionary): Pointer; cdecl;
    function textureCoordinateCount: NSUInteger; cdecl;
    function textureCoordinates: Psimd_float2; cdecl;
    function triangleCount: NSUInteger; cdecl;
    function triangleIndices: PInt16; cdecl;
    function vertexCount: NSUInteger; cdecl;
    function vertices: Psimd_float3; cdecl;
  end;
  TARFaceGeometry = class(TOCGenericImport<ARFaceGeometryClass, ARFaceGeometry>) end;

  ARHitTestResultClass = interface(NSObjectClass)
    ['{7E1844B9-4AC9-4DEA-83A5-73138F9D0B04}']
    {class} function new: Pointer; cdecl;
  end;

  ARHitTestResult = interface(NSObject)
    ['{B06121BE-B6F4-484F-877A-81CEACCE53BF}']
    [MethodName('type')]
    function &type: ARHitTestResultType; cdecl;
    function anchor: ARAnchor; cdecl;
    function distance: CGFloat; cdecl;
    function localTransform: simd_float4x4; cdecl;
    function worldTransform: simd_float4x4; cdecl;
  end;
  TARHitTestResult = class(TOCGenericImport<ARHitTestResultClass, ARHitTestResult>) end;

  ARRaycastQueryClass = interface(NSObjectClass)
    ['{72E8CB92-D82C-4598-843F-848CE0775A9D}']
  end;

  ARRaycastQuery = interface(NSObject)
    ['{2824A741-1F6C-40D5-84AF-E3B6B301D177}']
    function direction: simd_float3; cdecl;
    function initWithOrigin(origin: simd_float3; direction: simd_float3; allowingTarget: ARRaycastTarget; alignment: ARRaycastTargetAlignment): Pointer; cdecl;
    function new: Pointer; cdecl;
    function origin: simd_float3; cdecl;
    function target: ARRaycastTarget; cdecl;
    function targetAlignment: ARRaycastTargetAlignment; cdecl;
  end;
  TARRaycastQuery = class(TOCGenericImport<ARRaycastQueryClass, ARRaycastQuery>) end;

  ARFrameClass = interface(NSObjectClass)
    ['{C9C8A304-7B3D-4245-8F60-ECF44AE0977F}']
    {class} function new: Pointer; cdecl;
  end;

  ARFrame = interface(NSObject)
    ['{756403F4-3BC0-48AD-8812-B6C3A1B67058}']
    function anchors: NSArray; cdecl;
    function camera: ARCamera; cdecl;
    function cameraGrainIntensity: Single; cdecl;
    function cameraGrainTexture: Pointer; cdecl;
    function capturedDepthData: AVDepthData; cdecl;
    function capturedDepthDataTimestamp: NSTimeInterval; cdecl;
    function capturedImage: CVPixelBufferRef; cdecl;
    function detectedBody: ARBody2D; cdecl;
    function displayTransformForOrientation(orientation: UIInterfaceOrientation; viewportSize: CGSize): CGAffineTransform; cdecl;
    function estimatedDepthData: CVPixelBufferRef; cdecl;
    function geoTrackingStatus: ARGeoTrackingStatus; cdecl;
    function hitTest(point: CGPoint; types: ARHitTestResultType): NSArray; cdecl; // API_DEPRECATED("Use [ARSession raycast:]", ios(11.0, 14.0))
    function lightEstimate: ARLightEstimate; cdecl;
    function rawFeaturePoints: ARPointCloud; cdecl;
    function raycastQueryFromPoint(point: CGPoint; allowingTarget: ARRaycastTarget; alignment: ARRaycastTargetAlignment): ARRaycastQuery; cdecl;
    function sceneDepth: ARDepthData; cdecl;
    function segmentationBuffer: CVPixelBufferRef; cdecl;
    function smoothedSceneDepth: ARDepthData; cdecl;
    function timestamp: NSTimeInterval; cdecl;
    function worldMappingStatus: ARWorldMappingStatus; cdecl;
  end;
  TARFrame = class(TOCGenericImport<ARFrameClass, ARFrame>) end;

  ARGeoAnchorClass = interface(ARAnchorClass)
    ['{BF58163C-EE3A-4B97-8A90-647AB5BDE2DC}']
  end;

  ARGeoAnchor = interface(ARAnchor)
    ['{DB25730E-8108-4796-B1E5-0044CC82A8B5}']
    function altitude: CLLocationDistance; cdecl;
    function altitudeSource: ARAltitudeSource; cdecl;
    function coordinate: CLLocationCoordinate2D; cdecl;
    function initWithCoordinate(coordinate: CLLocationCoordinate2D): Pointer; overload; cdecl;
    function initWithCoordinate(coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance): Pointer; overload; cdecl;
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; overload; cdecl;
    function initWithName(name: NSString; coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance): Pointer; overload; cdecl;
    function initWithName(name: NSString; coordinate: CLLocationCoordinate2D): Pointer; overload; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
  end;
  TARGeoAnchor = class(TOCGenericImport<ARGeoAnchorClass, ARGeoAnchor>) end;

  ARImageAnchorClass = interface(ARAnchorClass)
    ['{3F6E5C43-E448-4F41-A0BD-B577E32B25E9}']
  end;

  ARImageAnchor = interface(ARAnchor)
    ['{96168FA0-BB22-446D-99E4-0678DDE8A954}']
    function estimatedScaleFactor: CGFloat; cdecl;
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
    function referenceImage: ARReferenceImage; cdecl;
  end;
  TARImageAnchor = class(TOCGenericImport<ARImageAnchorClass, ARImageAnchor>) end;

  ARLightEstimateClass = interface(NSObjectClass)
    ['{A4EA5130-D935-4239-84F4-579148C6B2E6}']
    {class} function new: Pointer; cdecl;
  end;

  ARLightEstimate = interface(NSObject)
    ['{D5BA4B7F-08DA-4A43-A47A-3772D36D2E70}']
    function ambientColorTemperature: CGFloat; cdecl;
    function ambientIntensity: CGFloat; cdecl;
  end;
  TARLightEstimate = class(TOCGenericImport<ARLightEstimateClass, ARLightEstimate>) end;

  ARDirectionalLightEstimateClass = interface(ARLightEstimateClass)
    ['{403FFAF1-6C0B-42F4-BBCE-B91DC7BAE5F1}']
  end;

  ARDirectionalLightEstimate = interface(ARLightEstimate)
    ['{E8FFDA07-040C-43A4-9A12-D91DF4BAE891}']
    function primaryLightDirection: simd_float3; cdecl;
    function primaryLightIntensity: CGFloat; cdecl;
    function sphericalHarmonicsCoefficients: NSData; cdecl;
  end;
  TARDirectionalLightEstimate = class(TOCGenericImport<ARDirectionalLightEstimateClass, ARDirectionalLightEstimate>) end;

  ARPointCloudClass = interface(NSObjectClass)
    ['{FB6600BA-9B3C-4883-9606-736524D1F848}']
    {class} function new: Pointer; cdecl;
  end;

  ARPointCloud = interface(NSObject)
    ['{01AF90CC-C6C0-45F9-B147-0B86636217B0}']
    function count: NSUInteger; cdecl;
    function identifiers: PUInt64; cdecl;
    function points: Psimd_float3; cdecl;
  end;
  TARPointCloud = class(TOCGenericImport<ARPointCloudClass, ARPointCloud>) end;

  ARRaycastResultClass = interface(NSObjectClass)
    ['{2A24D8C0-FD45-43DB-8ACC-CD4C612936C3}']
  end;

  ARRaycastResult = interface(NSObject)
    ['{12461C99-624E-4C73-9AA4-D17FC8CF071F}']
    function anchor: ARAnchor; cdecl;
    function new: Pointer; cdecl;
    function target: ARRaycastTarget; cdecl;
    function targetAlignment: ARRaycastTargetAlignment; cdecl;
    function worldTransform: simd_float4x4; cdecl;
  end;
  TARRaycastResult = class(TOCGenericImport<ARRaycastResultClass, ARRaycastResult>) end;

  ARReferenceImageClass = interface(NSObjectClass)
    ['{E47BEFA8-0E06-4132-A90D-9A473A97F75E}']
    {class} function new: Pointer; cdecl;
    {class} function referenceImagesInGroupNamed(name: NSString; bundle: NSBundle): NSSet; cdecl;
  end;

  ARReferenceImage = interface(NSObject)
    ['{24EEDAA2-2B8A-4ED5-B474-BC1D67797027}']
    function initWithCGImage(image: CGImageRef; orientation: CGImagePropertyOrientation; physicalWidth: CGFloat): Pointer; cdecl;
    function initWithPixelBuffer(pixelBuffer: CVPixelBufferRef; orientation: CGImagePropertyOrientation; physicalWidth: CGFloat): Pointer; cdecl;
    function name: NSString; cdecl;
    function physicalSize: CGSize; cdecl;
    function resourceGroupName: NSString; cdecl;
    procedure setName(name: NSString); cdecl;
    procedure validateWithCompletionHandler(completionHandler: TARReferenceImageBlockMethod1); cdecl;
  end;
  TARReferenceImage = class(TOCGenericImport<ARReferenceImageClass, ARReferenceImage>) end;

  ARReferenceObjectClass = interface(NSObjectClass)
    ['{6EFF685D-64FC-4741-B242-F395153BBDA8}']
    {class} function new: Pointer; cdecl;
    {class} function referenceObjectsInGroupNamed(name: NSString; bundle: NSBundle): NSSet; cdecl;
  end;

  ARReferenceObject = interface(NSObject)
    ['{73D46022-6F4A-4586-9A7E-06A2162B32B7}']
    function center: simd_float3; cdecl;
    function exportObjectToURL(url: NSURL; previewImage: UIImage; error: PPointer): Boolean; cdecl;
    function extent: simd_float3; cdecl;
    function initWithArchiveURL(url: NSURL; error: PPointer): Pointer; cdecl;
    function name: NSString; cdecl;
    function rawFeaturePoints: ARPointCloud; cdecl;
    function referenceObjectByApplyingTransform(transform: simd_float4x4): ARReferenceObject; cdecl;
    function referenceObjectByMergingObject(&object: ARReferenceObject; error: PPointer): ARReferenceObject; cdecl;
    function resourceGroupName: NSString; cdecl;
    function scale: simd_float3; cdecl;
    procedure setName(name: NSString); cdecl;
  end;
  TARReferenceObject = class(TOCGenericImport<ARReferenceObjectClass, ARReferenceObject>) end;

  ARTrackedRaycastClass = interface(NSObjectClass)
    ['{0C963224-FEDC-4E67-8398-75BDF048C6C2}']
  end;

  ARTrackedRaycast = interface(NSObject)
    ['{4B59DEDC-82D0-4499-A7C3-C3B17E0A6F0D}']
    function new: Pointer; cdecl;
    procedure stopTracking; cdecl;
  end;
  TARTrackedRaycast = class(TOCGenericImport<ARTrackedRaycastClass, ARTrackedRaycast>) end;

  ARVideoFormatClass = interface(NSObjectClass)
    ['{16398F05-9426-494B-9B23-95E4AEEF7EFF}']
    {class} function new: Pointer; cdecl;
  end;

  ARVideoFormat = interface(NSObject)
    ['{3F9AA7BA-F6E8-438F-95E0-A3B39FDCD7BA}']
    function captureDevicePosition: AVCaptureDevicePosition; cdecl;
    function framesPerSecond: NSInteger; cdecl;
    function imageResolution: CGSize; cdecl;
  end;
  TARVideoFormat = class(TOCGenericImport<ARVideoFormatClass, ARVideoFormat>) end;

  ARWorldMapClass = interface(NSObjectClass)
    ['{7364AD89-A2F0-48AF-9984-49FBC7FB901F}']
    {class} function new: Pointer; cdecl;
  end;

  ARWorldMap = interface(NSObject)
    ['{CDF82182-9DD0-4FDA-BF6F-D3C2F159AEE0}']
    function anchors: NSArray; cdecl;
    function center: simd_float3; cdecl;
    function extent: simd_float3; cdecl;
    function rawFeaturePoints: ARPointCloud; cdecl;
    procedure setAnchors(anchors: NSArray); cdecl;
  end;
  TARWorldMap = class(TOCGenericImport<ARWorldMapClass, ARWorldMap>) end;

  ARMeshAnchorClass = interface(ARAnchorClass)
    ['{FAEFC2A0-9604-4769-8D28-B29898151C9E}']
  end;

  ARMeshAnchor = interface(ARAnchor)
    ['{87B2B4F1-9432-49C4-9EC5-8CC0B9E33D95}']
    function geometry: ARMeshGeometry; cdecl;
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
  end;
  TARMeshAnchor = class(TOCGenericImport<ARMeshAnchorClass, ARMeshAnchor>) end;

  ARGeometrySourceClass = interface(NSObjectClass)
    ['{91C7802D-B089-4CF1-A537-34F809910536}']
    {class} function new: Pointer; cdecl;
  end;

  ARGeometrySource = interface(NSObject)
    ['{8EA38F23-C0DC-48B8-9DD1-93100877F0C8}']
    function buffer: Pointer; cdecl;
    function componentsPerVector: NSInteger; cdecl;
    function count: NSInteger; cdecl;
    function format: MTLVertexFormat; cdecl;
    function offset: NSInteger; cdecl;
    function stride: NSInteger; cdecl;
  end;
  TARGeometrySource = class(TOCGenericImport<ARGeometrySourceClass, ARGeometrySource>) end;

  ARGeometryElementClass = interface(NSObjectClass)
    ['{DA0E29E7-CFF9-49E1-A9C6-5543A0643FA4}']
    {class} function new: Pointer; cdecl;
  end;

  ARGeometryElement = interface(NSObject)
    ['{F08742B6-D661-4D5E-BB53-CE954BA042AC}']
    function buffer: Pointer; cdecl;
    function bytesPerIndex: NSInteger; cdecl;
    function count: NSInteger; cdecl;
    function indexCountPerPrimitive: NSInteger; cdecl;
    function primitiveType: ARGeometryPrimitiveType; cdecl;
  end;
  TARGeometryElement = class(TOCGenericImport<ARGeometryElementClass, ARGeometryElement>) end;

  ARMeshGeometryClass = interface(NSObjectClass)
    ['{7E427394-D878-4847-9B9E-66EF12A95731}']
    {class} function new: Pointer; cdecl;
  end;

  ARMeshGeometry = interface(NSObject)
    ['{5D5B6D95-3785-4FB6-B526-6F77F4CC416C}']
    function classification: ARGeometrySource; cdecl;
    function faces: ARGeometryElement; cdecl;
    function normals: ARGeometrySource; cdecl;
    function vertices: ARGeometrySource; cdecl;
  end;
  TARMeshGeometry = class(TOCGenericImport<ARMeshGeometryClass, ARMeshGeometry>) end;

  ARObjectAnchorClass = interface(ARAnchorClass)
    ['{CAB8FFC0-A7E6-4A72-9A35-0ABC37F101C8}']
  end;

  ARObjectAnchor = interface(ARAnchor)
    ['{93B3CFE2-7F3B-4D22-848D-E1A4804F0D8C}']
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
    function referenceObject: ARReferenceObject; cdecl;
  end;
  TARObjectAnchor = class(TOCGenericImport<ARObjectAnchorClass, ARObjectAnchor>) end;

  ARParticipantAnchorClass = interface(ARAnchorClass)
    ['{637D19A4-2E6E-424D-8CFC-9AA591544345}']
  end;

  ARParticipantAnchor = interface(ARAnchor)
    ['{7A26758A-5CDC-40A2-AB9B-CF2647537E97}']
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
  end;
  TARParticipantAnchor = class(TOCGenericImport<ARParticipantAnchorClass, ARParticipantAnchor>) end;

  ARPlaneAnchorClass = interface(ARAnchorClass)
    ['{35F10944-E7B5-4671-997B-9056150FC019}']
    {class} function isClassificationSupported: Boolean; cdecl;
  end;

  ARPlaneAnchor = interface(ARAnchor)
    ['{7E12B3FA-6A5D-4847-A22C-5A21DD198D5A}']
    function alignment: ARPlaneAnchorAlignment; cdecl;
    function center: simd_float3; cdecl;
    function classification: ARPlaneClassification; cdecl;
    function classificationStatus: ARPlaneClassificationStatus; cdecl;
    function extent: simd_float3; cdecl;
    function geometry: ARPlaneGeometry; cdecl;
    function initWithName(name: NSString; transform: simd_float4x4): Pointer; cdecl;
    function initWithTransform(transform: simd_float4x4): Pointer; cdecl;
  end;
  TARPlaneAnchor = class(TOCGenericImport<ARPlaneAnchorClass, ARPlaneAnchor>) end;

  ARPlaneGeometryClass = interface(NSObjectClass)
    ['{F9C828B2-ACD9-45F2-8112-0C8A036F45CD}']
    {class} function new: Pointer; cdecl;
  end;

  ARPlaneGeometry = interface(NSObject)
    ['{F9DB9864-DA78-48AD-977B-ADA1DB75040F}']
    function boundaryVertexCount: NSUInteger; cdecl;
    function boundaryVertices: Psimd_float3; cdecl;
    function textureCoordinateCount: NSUInteger; cdecl;
    function textureCoordinates: Psimd_float2; cdecl;
    function triangleCount: NSUInteger; cdecl;
    function triangleIndices: PInt16; cdecl;
    function vertexCount: NSUInteger; cdecl;
    function vertices: Psimd_float3; cdecl;
  end;
  TARPlaneGeometry = class(TOCGenericImport<ARPlaneGeometryClass, ARPlaneGeometry>) end;

  ARMatteGeneratorClass = interface(NSObjectClass)
    ['{BE843C0C-ADD3-445D-8548-D63654FD78E1}']
    {class} function new: Pointer; cdecl;
  end;

  ARMatteGenerator = interface(NSObject)
    ['{E1015586-CF29-43B1-95E0-6F857F7789BE}']
    function generateDilatedDepthFromFrame(frame: ARFrame; commandBuffer: Pointer): Pointer; cdecl;
    function generateMatteFromFrame(frame: ARFrame; commandBuffer: Pointer): Pointer; cdecl;
    function initWithDevice(device: Pointer; matteResolution: ARMatteResolution): Pointer; cdecl;
  end;
  TARMatteGenerator = class(TOCGenericImport<ARMatteGeneratorClass, ARMatteGenerator>) end;

  ARQuickLookPreviewItemClass = interface(NSObjectClass)
    ['{C9B42245-4E58-47BB-8B8F-1E36D3A86D08}']
    {class} function new: Pointer; cdecl;
  end;

  ARQuickLookPreviewItem = interface(NSObject)
    ['{331113CD-F715-424C-BFFE-8A1EEA59CE32}']
    function allowsContentScaling: Boolean; cdecl;
    function canonicalWebPageURL: NSURL; cdecl;
    function initWithFileAtURL(url: NSURL): Pointer; cdecl;
    procedure setAllowsContentScaling(allowsContentScaling: Boolean); cdecl;
    procedure setCanonicalWebPageURL(canonicalWebPageURL: NSURL); cdecl;
  end;
  TARQuickLookPreviewItem = class(TOCGenericImport<ARQuickLookPreviewItemClass, ARQuickLookPreviewItem>) end;

  ARSCNViewDelegate = interface(IObjectiveC)
    ['{3B821E35-284E-4073-98A7-F0C13D69FEE5}']
    [MethodName('renderer:didAddNode:forAnchor:')]
    procedure rendererDidAddNode(renderer: Pointer; didAddNode: Pointer; forAnchor: ARAnchor); cdecl;
    [MethodName('renderer:didRemoveNode:forAnchor:')]
    procedure rendererDidRemoveNode(renderer: Pointer; didRemoveNode: Pointer; forAnchor: ARAnchor); cdecl;
    [MethodName('renderer:didUpdateNode:forAnchor:')]
    procedure rendererDidUpdateNode(renderer: Pointer; didUpdateNode: Pointer; forAnchor: ARAnchor); cdecl;
    [MethodName('renderer:nodeForAnchor:')]
    function rendererNodeForAnchor(renderer: Pointer; nodeForAnchor: ARAnchor): Pointer; cdecl;
    [MethodName('renderer:willUpdateNode:forAnchor:')]
    procedure rendererWillUpdateNode(renderer: Pointer; willUpdateNode: Pointer; forAnchor: ARAnchor); cdecl;
  end;

  ARSKViewDelegate = interface(IObjectiveC)
    ['{E69ECD79-2656-4635-8BA2-D82C439F0EE5}']
    [MethodName('view:didAddNode:forAnchor:')]
    procedure viewDidAddNode(view: ARSKView; didAddNode: Pointer; forAnchor: ARAnchor); cdecl;
    [MethodName('view:didRemoveNode:forAnchor:')]
    procedure viewDidRemoveNode(view: ARSKView; didRemoveNode: Pointer; forAnchor: ARAnchor); cdecl;
    [MethodName('view:didUpdateNode:forAnchor:')]
    procedure viewDidUpdateNode(view: ARSKView; didUpdateNode: Pointer; forAnchor: ARAnchor); cdecl;
    [MethodName('view:nodeForAnchor:')]
    function viewNodeForAnchor(view: ARSKView; nodeForAnchor: ARAnchor): Pointer; cdecl;
    [MethodName('view:willUpdateNode:forAnchor:')]
    procedure viewWillUpdateNode(view: ARSKView; willUpdateNode: Pointer; forAnchor: ARAnchor); cdecl;
  end;

  ARSCNPlaneGeometryClass = interface(SCNGeometryClass)
    ['{3A2A83D2-47E9-406C-AA33-DD6386E209BF}']
    {class} function planeGeometryWithDevice(device: Pointer): Pointer; cdecl;
  end;

  ARSCNPlaneGeometry = interface(SCNGeometry)
    ['{8A261488-98F7-42EF-B400-E564F74EE7E5}']
    procedure updateFromPlaneGeometry(planeGeometry: ARPlaneGeometry); cdecl;
  end;
  TARSCNPlaneGeometry = class(TOCGenericImport<ARSCNPlaneGeometryClass, ARSCNPlaneGeometry>) end;

  ARSCNViewClass = interface(SCNViewClass)
    ['{761FF3D6-FC19-41E6-ABFD-FAD1F4072C07}']
  end;

  ARSCNView = interface(SCNView)
    ['{F97CA7F3-13D6-4139-A0FD-5A0FC7CA2F89}']
    function anchorForNode(node: SCNNode): ARAnchor; cdecl;
    function automaticallyUpdatesLighting: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function hitTest(point: CGPoint; types: ARHitTestResultType): NSArray; cdecl; // API_DEPRECATED("Use [ARSCNView raycastQueryFromPoint:allowingTarget:alignment]", ios(11.0, 14.0))
    function nodeForAnchor(anchor: ARAnchor): SCNNode; cdecl;
    function raycastQueryFromPoint(point: CGPoint; allowingTarget: ARRaycastTarget; alignment: ARRaycastTargetAlignment): ARRaycastQuery; cdecl;
    function rendersCameraGrain: Boolean; cdecl;
    function rendersMotionBlur: Boolean; cdecl;
    function scene: SCNScene; cdecl;
    function session: ARSession; cdecl;
    procedure setAutomaticallyUpdatesLighting(automaticallyUpdatesLighting: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setRendersCameraGrain(rendersCameraGrain: Boolean); cdecl;
    procedure setRendersMotionBlur(rendersMotionBlur: Boolean); cdecl;
    procedure setScene(scene: SCNScene); cdecl;
    procedure setSession(session: ARSession); cdecl;
    function unprojectPoint(point: CGPoint; ontoPlaneWithTransform: simd_float4x4): simd_float3; cdecl;
  end;
  TARSCNView = class(TOCGenericImport<ARSCNViewClass, ARSCNView>) end;

  ARSKViewClass = interface(SKViewClass)
    ['{C0747212-E877-41E8-8053-9DE68851CC68}']
  end;

  ARSKView = interface(SKView)
    ['{78771C97-961E-485B-B1CB-958068B80236}']
    function anchorForNode(node: SKNode): ARAnchor; cdecl;
    function delegate: NSObject; cdecl;
    function hitTest(point: CGPoint; types: ARHitTestResultType): NSArray; cdecl; // API_DEPRECATED("Use raycasting", ios(11.0, 14.0))
    function nodeForAnchor(anchor: ARAnchor): SKNode; cdecl;
    function session: ARSession; cdecl;
    procedure setDelegate(delegate: NSObject); cdecl;
    procedure setSession(session: ARSession); cdecl;
  end;
  TARSKView = class(TOCGenericImport<ARSKViewClass, ARSKView>) end;

function ARSkeletonJointNameRoot: ARSkeletonJointName;
function ARSkeletonJointNameHead: ARSkeletonJointName;
function ARSkeletonJointNameLeftHand: ARSkeletonJointName;
function ARSkeletonJointNameRightHand: ARSkeletonJointName;
function ARSkeletonJointNameLeftFoot: ARSkeletonJointName;
function ARSkeletonJointNameRightFoot: ARSkeletonJointName;
function ARSkeletonJointNameLeftShoulder: ARSkeletonJointName;
function ARSkeletonJointNameRightShoulder: ARSkeletonJointName;
function ARErrorDomain: NSString;
function ARBlendShapeLocationBrowDownLeft: ARBlendShapeLocation;
function ARBlendShapeLocationBrowDownRight: ARBlendShapeLocation;
function ARBlendShapeLocationBrowInnerUp: ARBlendShapeLocation;
function ARBlendShapeLocationBrowOuterUpLeft: ARBlendShapeLocation;
function ARBlendShapeLocationBrowOuterUpRight: ARBlendShapeLocation;
function ARBlendShapeLocationCheekPuff: ARBlendShapeLocation;
function ARBlendShapeLocationCheekSquintLeft: ARBlendShapeLocation;
function ARBlendShapeLocationCheekSquintRight: ARBlendShapeLocation;
function ARBlendShapeLocationEyeBlinkLeft: ARBlendShapeLocation;
function ARBlendShapeLocationEyeBlinkRight: ARBlendShapeLocation;
function ARBlendShapeLocationEyeLookDownLeft: ARBlendShapeLocation;
function ARBlendShapeLocationEyeLookDownRight: ARBlendShapeLocation;
function ARBlendShapeLocationEyeLookInLeft: ARBlendShapeLocation;
function ARBlendShapeLocationEyeLookInRight: ARBlendShapeLocation;
function ARBlendShapeLocationEyeLookOutLeft: ARBlendShapeLocation;
function ARBlendShapeLocationEyeLookOutRight: ARBlendShapeLocation;
function ARBlendShapeLocationEyeLookUpLeft: ARBlendShapeLocation;
function ARBlendShapeLocationEyeLookUpRight: ARBlendShapeLocation;
function ARBlendShapeLocationEyeSquintLeft: ARBlendShapeLocation;
function ARBlendShapeLocationEyeSquintRight: ARBlendShapeLocation;
function ARBlendShapeLocationEyeWideLeft: ARBlendShapeLocation;
function ARBlendShapeLocationEyeWideRight: ARBlendShapeLocation;
function ARBlendShapeLocationJawForward: ARBlendShapeLocation;
function ARBlendShapeLocationJawLeft: ARBlendShapeLocation;
function ARBlendShapeLocationJawOpen: ARBlendShapeLocation;
function ARBlendShapeLocationJawRight: ARBlendShapeLocation;
function ARBlendShapeLocationMouthClose: ARBlendShapeLocation;
function ARBlendShapeLocationMouthDimpleLeft: ARBlendShapeLocation;
function ARBlendShapeLocationMouthDimpleRight: ARBlendShapeLocation;
function ARBlendShapeLocationMouthFrownLeft: ARBlendShapeLocation;
function ARBlendShapeLocationMouthFrownRight: ARBlendShapeLocation;
function ARBlendShapeLocationMouthFunnel: ARBlendShapeLocation;
function ARBlendShapeLocationMouthLeft: ARBlendShapeLocation;
function ARBlendShapeLocationMouthLowerDownLeft: ARBlendShapeLocation;
function ARBlendShapeLocationMouthLowerDownRight: ARBlendShapeLocation;
function ARBlendShapeLocationMouthPressLeft: ARBlendShapeLocation;
function ARBlendShapeLocationMouthPressRight: ARBlendShapeLocation;
function ARBlendShapeLocationMouthPucker: ARBlendShapeLocation;
function ARBlendShapeLocationMouthRight: ARBlendShapeLocation;
function ARBlendShapeLocationMouthRollLower: ARBlendShapeLocation;
function ARBlendShapeLocationMouthRollUpper: ARBlendShapeLocation;
function ARBlendShapeLocationMouthShrugLower: ARBlendShapeLocation;
function ARBlendShapeLocationMouthShrugUpper: ARBlendShapeLocation;
function ARBlendShapeLocationMouthSmileLeft: ARBlendShapeLocation;
function ARBlendShapeLocationMouthSmileRight: ARBlendShapeLocation;
function ARBlendShapeLocationMouthStretchLeft: ARBlendShapeLocation;
function ARBlendShapeLocationMouthStretchRight: ARBlendShapeLocation;
function ARBlendShapeLocationMouthUpperUpLeft: ARBlendShapeLocation;
function ARBlendShapeLocationMouthUpperUpRight: ARBlendShapeLocation;
function ARBlendShapeLocationNoseSneerLeft: ARBlendShapeLocation;
function ARBlendShapeLocationNoseSneerRight: ARBlendShapeLocation;
function ARBlendShapeLocationTongueOut: ARBlendShapeLocation;
function ARReferenceObjectArchiveExtension: NSString;
function ARSCNDebugOptionShowWorldOrigin: Integer;
function ARSCNDebugOptionShowFeaturePoints: Integer;

const
  libARKit = '/System/Library/Frameworks/ARKit.framework/ARKit';

function ARSkeletonJointNameForRecognizedPointKey(recognizedPointKey: VNRecognizedPointKey): ARSkeletonJointName; cdecl;
  external libARKit name _PU + 'ARSkeletonJointNameForRecognizedPointKey';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  ARKitModule: THandle;
{$ENDIF}

function ARSkeletonJointNameRoot: ARSkeletonJointName;
begin
  Result := CocoaNSStringConst(libARKit, 'ARSkeletonJointNameRoot');
end;

function ARSkeletonJointNameHead: ARSkeletonJointName;
begin
  Result := CocoaNSStringConst(libARKit, 'ARSkeletonJointNameHead');
end;

function ARSkeletonJointNameLeftHand: ARSkeletonJointName;
begin
  Result := CocoaNSStringConst(libARKit, 'ARSkeletonJointNameLeftHand');
end;

function ARSkeletonJointNameRightHand: ARSkeletonJointName;
begin
  Result := CocoaNSStringConst(libARKit, 'ARSkeletonJointNameRightHand');
end;

function ARSkeletonJointNameLeftFoot: ARSkeletonJointName;
begin
  Result := CocoaNSStringConst(libARKit, 'ARSkeletonJointNameLeftFoot');
end;

function ARSkeletonJointNameRightFoot: ARSkeletonJointName;
begin
  Result := CocoaNSStringConst(libARKit, 'ARSkeletonJointNameRightFoot');
end;

function ARSkeletonJointNameLeftShoulder: ARSkeletonJointName;
begin
  Result := CocoaNSStringConst(libARKit, 'ARSkeletonJointNameLeftShoulder');
end;

function ARSkeletonJointNameRightShoulder: ARSkeletonJointName;
begin
  Result := CocoaNSStringConst(libARKit, 'ARSkeletonJointNameRightShoulder');
end;

function ARErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libARKit, 'ARErrorDomain');
end;

function ARBlendShapeLocationBrowDownLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationBrowDownLeft');
end;

function ARBlendShapeLocationBrowDownRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationBrowDownRight');
end;

function ARBlendShapeLocationBrowInnerUp: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationBrowInnerUp');
end;

function ARBlendShapeLocationBrowOuterUpLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationBrowOuterUpLeft');
end;

function ARBlendShapeLocationBrowOuterUpRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationBrowOuterUpRight');
end;

function ARBlendShapeLocationCheekPuff: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationCheekPuff');
end;

function ARBlendShapeLocationCheekSquintLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationCheekSquintLeft');
end;

function ARBlendShapeLocationCheekSquintRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationCheekSquintRight');
end;

function ARBlendShapeLocationEyeBlinkLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeBlinkLeft');
end;

function ARBlendShapeLocationEyeBlinkRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeBlinkRight');
end;

function ARBlendShapeLocationEyeLookDownLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeLookDownLeft');
end;

function ARBlendShapeLocationEyeLookDownRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeLookDownRight');
end;

function ARBlendShapeLocationEyeLookInLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeLookInLeft');
end;

function ARBlendShapeLocationEyeLookInRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeLookInRight');
end;

function ARBlendShapeLocationEyeLookOutLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeLookOutLeft');
end;

function ARBlendShapeLocationEyeLookOutRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeLookOutRight');
end;

function ARBlendShapeLocationEyeLookUpLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeLookUpLeft');
end;

function ARBlendShapeLocationEyeLookUpRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeLookUpRight');
end;

function ARBlendShapeLocationEyeSquintLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeSquintLeft');
end;

function ARBlendShapeLocationEyeSquintRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeSquintRight');
end;

function ARBlendShapeLocationEyeWideLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeWideLeft');
end;

function ARBlendShapeLocationEyeWideRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationEyeWideRight');
end;

function ARBlendShapeLocationJawForward: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationJawForward');
end;

function ARBlendShapeLocationJawLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationJawLeft');
end;

function ARBlendShapeLocationJawOpen: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationJawOpen');
end;

function ARBlendShapeLocationJawRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationJawRight');
end;

function ARBlendShapeLocationMouthClose: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthClose');
end;

function ARBlendShapeLocationMouthDimpleLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthDimpleLeft');
end;

function ARBlendShapeLocationMouthDimpleRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthDimpleRight');
end;

function ARBlendShapeLocationMouthFrownLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthFrownLeft');
end;

function ARBlendShapeLocationMouthFrownRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthFrownRight');
end;

function ARBlendShapeLocationMouthFunnel: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthFunnel');
end;

function ARBlendShapeLocationMouthLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthLeft');
end;

function ARBlendShapeLocationMouthLowerDownLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthLowerDownLeft');
end;

function ARBlendShapeLocationMouthLowerDownRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthLowerDownRight');
end;

function ARBlendShapeLocationMouthPressLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthPressLeft');
end;

function ARBlendShapeLocationMouthPressRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthPressRight');
end;

function ARBlendShapeLocationMouthPucker: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthPucker');
end;

function ARBlendShapeLocationMouthRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthRight');
end;

function ARBlendShapeLocationMouthRollLower: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthRollLower');
end;

function ARBlendShapeLocationMouthRollUpper: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthRollUpper');
end;

function ARBlendShapeLocationMouthShrugLower: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthShrugLower');
end;

function ARBlendShapeLocationMouthShrugUpper: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthShrugUpper');
end;

function ARBlendShapeLocationMouthSmileLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthSmileLeft');
end;

function ARBlendShapeLocationMouthSmileRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthSmileRight');
end;

function ARBlendShapeLocationMouthStretchLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthStretchLeft');
end;

function ARBlendShapeLocationMouthStretchRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthStretchRight');
end;

function ARBlendShapeLocationMouthUpperUpLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthUpperUpLeft');
end;

function ARBlendShapeLocationMouthUpperUpRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationMouthUpperUpRight');
end;

function ARBlendShapeLocationNoseSneerLeft: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationNoseSneerLeft');
end;

function ARBlendShapeLocationNoseSneerRight: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationNoseSneerRight');
end;

function ARBlendShapeLocationTongueOut: ARBlendShapeLocation;
begin
  Result := CocoaNSStringConst(libARKit, 'ARBlendShapeLocationTongueOut');
end;

function ARReferenceObjectArchiveExtension: NSString;
begin
  Result := CocoaNSStringConst(libARKit, 'ARReferenceObjectArchiveExtension');
end;

function ARSCNDebugOptionShowWorldOrigin: Integer;
begin
  Result := CocoaIntegerConst(libARKit, 'ARSCNDebugOptionShowWorldOrigin');
end;

function ARSCNDebugOptionShowFeaturePoints: Integer;
begin
  Result := CocoaIntegerConst(libARKit, 'ARSCNDebugOptionShowFeaturePoints');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  ARKitModule := dlopen(MarshaledAString(libARKit), RTLD_LAZY);

finalization
  dlclose(ARKitModule)
{$ENDIF}

end.