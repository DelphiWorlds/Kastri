unit DW.iOSapi.CoreMotion;

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
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.Foundation;

const
  CMAuthorizationStatusNotDetermined = 0;
  CMAuthorizationStatusRestricted = 1;
  CMAuthorizationStatusDenied = 2;
  CMAuthorizationStatusAuthorized = 3;
  CMAttitudeReferenceFrameXArbitraryZVertical = 1;
  CMAttitudeReferenceFrameXArbitraryCorrectedZVertical = 2;
  CMAttitudeReferenceFrameXMagneticNorthZVertical = 4;
  CMAttitudeReferenceFrameXTrueNorthZVertical = 8;
  CMDeviceMotionSensorLocationDefault = 0;
  CMDeviceMotionSensorLocationHeadphoneLeft = 1;
  CMDeviceMotionSensorLocationHeadphoneRight = 2;
  CMMagneticFieldCalibrationAccuracyUncalibrated = -1;
  CMMagneticFieldCalibrationAccuracyLow = 0;
  CMMagneticFieldCalibrationAccuracyMedium = 1;
  CMMagneticFieldCalibrationAccuracyHigh = 2;
  CMErrorNULL = 100;
  CMErrorDeviceRequiresMovement = 101;
  CMErrorTrueNorthNotAvailable = 102;
  CMErrorUnknown = 103;
  CMErrorMotionActivityNotAvailable = 104;
  CMErrorMotionActivityNotAuthorized = 105;
  CMErrorMotionActivityNotEntitled = 106;
  CMErrorInvalidParameter = 107;
  CMErrorInvalidAction = 108;
  CMErrorNotAvailable = 109;
  CMErrorNotEntitled = 110;
  CMErrorNotAuthorized = 111;
  CMErrorNilData = 112;
  CMErrorSize = 113;
  CMFallDetectionEventUserResolutionConfirmed = 0;
  CMFallDetectionEventUserResolutionDismissed = 1;
  CMFallDetectionEventUserResolutionRejected = 2;
  CMFallDetectionEventUserResolutionUnresponsive = 3;
  CMMotionActivityConfidenceLow = 0;
  CMMotionActivityConfidenceMedium = 1;
  CMMotionActivityConfidenceHigh = 2;
  CMPedometerEventTypePause = 0;
  CMPedometerEventTypeResume = 1;
  CMWaterSubmersionStateUnknown = 0;
  CMWaterSubmersionStateNotSubmerged = 1;
  CMWaterSubmersionStateSubmerged = 2;
  CMWaterSubmersionDepthStateUnknown = 0;
  CMWaterSubmersionDepthStateNotSubmerged = 100;
  CMWaterSubmersionDepthStateSubmergedShallow = 200;
  CMWaterSubmersionDepthStateSubmergedDeep = 300;
  CMWaterSubmersionDepthStateApproachingMaxDepth = 400;
  CMWaterSubmersionDepthStatePastMaxDepth = 500;
  CMWaterSubmersionDepthStateSensorDepthError = 600;

type
  CMLogItem = interface;
  CMAccelerometerData = interface;
  CMAltitudeData = interface;
  CMAbsoluteAltitudeData = interface;
  CMAltimeter = interface;
  CMAttitude = interface;
  CMGyroData = interface;
  CMMagnetometerData = interface;
  CMDeviceMotion = interface;
  CMFallDetectionEvent = interface;
  CMFallDetectionManager = interface;
  CMFallDetectionDelegate = interface;
  CMMotionActivity = interface;
  CMMotionActivityManager = interface;
  CMMotionManager = interface;
  CMHeadphoneMotionManager = interface;
  CMHeadphoneMotionManagerDelegate = interface;
  CMDyskineticSymptomResult = interface;
  CMTremorResult = interface;
  CMMovementDisorderManager = interface;
  CMPedometerData = interface;
  CMPedometerEvent = interface;
  CMPedometer = interface;
  CMRotationRateData = interface;
  CMRecordedRotationRateData = interface;
  CMStepCounter = interface;
  CMRecordedAccelerometerData = interface;
  CMSensorDataList = interface;
  CMSensorRecorder = interface;
  CMAmbientPressureData = interface;
  CMRecordedPressureData = interface;
  CMWaterSubmersionEvent = interface;
  CMWaterSubmersionMeasurement = interface;
  CMWaterTemperature = interface;
  CMWaterSubmersionManagerDelegate = interface;
  CMWaterSubmersionManager = interface;

  PCMAcceleration = ^CMAcceleration;
  PCMRotationMatrix = ^CMRotationMatrix;
  PCMQuaternion = ^CMQuaternion;
  PCMRotationRate = ^CMRotationRate;
  PCMMagneticField = ^CMMagneticField;
  PCMCalibratedMagneticField = ^CMCalibratedMagneticField;

  CMAcceleration = record
    x: Double;
    y: Double;
    z: Double;
  end;

  CMAuthorizationStatus = NSInteger;

  CMAltitudeHandler = procedure(altitudeData: CMAltitudeData; error: NSError) of object;

  CMAbsoluteAltitudeHandler = procedure(altitudeData: CMAbsoluteAltitudeData; error: NSError) of object;

  CMRotationMatrix = record
    m11: Double;
    m12: Double;
    m13: Double;
    m21: Double;
    m22: Double;
    m23: Double;
    m31: Double;
    m32: Double;
    m33: Double;
  end;

  CMQuaternion = record
    x: Double;
    y: Double;
    z: Double;
    w: Double;
  end;

  CMAttitudeReferenceFrame = NSInteger;

  CMRotationRate = record
    x: Double;
    y: Double;
    z: Double;
  end;

  CMMagneticField = record
    x: Double;
    y: Double;
    z: Double;
  end;

  CMDeviceMotionSensorLocation = NSInteger;
  CMMagneticFieldCalibrationAccuracy = NSInteger;

  CMCalibratedMagneticField = record
    field: CMMagneticField;
    accuracy: CMMagneticFieldCalibrationAccuracy;
  end;

  CMFallDetectionEventUserResolution = NSInteger;
  CMMotionActivityConfidence = NSInteger;

  CMMotionActivityHandler = procedure(activity: CMMotionActivity) of object;

  CMMotionActivityQueryHandler = procedure(activities: NSArray; error: NSError) of object;

  CMAccelerometerHandler = procedure(accelerometerData: CMAccelerometerData; error: NSError) of object;

  CMGyroHandler = procedure(gyroData: CMGyroData; error: NSError) of object;

  CMDeviceMotionHandler = procedure(motion: CMDeviceMotion; error: NSError) of object;

  CMMagnetometerHandler = procedure(magnetometerData: CMMagnetometerData; error: NSError) of object;

  CMHeadphoneDeviceMotionHandler = procedure(motion: CMDeviceMotion; error: NSError) of object;

  CMDyskineticSymptomResultHandler = procedure(dyskineticSymptomResult: NSArray; error: NSError) of object;

  CMTremorResultHandler = procedure(tremorResult: NSArray; error: NSError) of object;
  CMPedometerEventType = NSInteger;

  CMPedometerHandler = procedure(pedometerData: CMPedometerData; error: NSError) of object;

  CMPedometerEventHandler = procedure(pedometerEvent: CMPedometerEvent; error: NSError) of object;

  CMStepQueryHandler = procedure(numberOfSteps: NSInteger; error: NSError) of object;

  CMStepUpdateHandler = procedure(numberOfSteps: NSInteger; timestamp: NSDate; error: NSError) of object;
  CMWaterSubmersionState = NSInteger;
  CMWaterSubmersionDepthState = NSInteger;
  TCMFallDetectionManagerBlockMethod1 = procedure(status: CMAuthorizationStatus) of object;
  TCMFallDetectionDelegateBlockMethod1 = procedure of object;

  CMLogItemClass = interface(NSObjectClass)
    ['{646EA8CA-AAF7-40BE-96C0-2844D5D6D399}']
  end;

  CMLogItem = interface(NSObject)
    ['{E80D13AC-5D57-4CED-BF7E-74BDDE637322}']
    function timestamp: NSTimeInterval; cdecl;
  end;
  TCMLogItem = class(TOCGenericImport<CMLogItemClass, CMLogItem>) end;

  CMAccelerometerDataClass = interface(CMLogItemClass)
    ['{0BC96439-663A-47C1-8F11-8C157F34E3F2}']
  end;

  CMAccelerometerData = interface(CMLogItem)
    ['{EE201BC7-AD06-4104-8A47-0F592D1738E8}']
    function acceleration: CMAcceleration; cdecl;
  end;
  TCMAccelerometerData = class(TOCGenericImport<CMAccelerometerDataClass, CMAccelerometerData>) end;

  CMAltitudeDataClass = interface(CMLogItemClass)
    ['{F9FA408B-5C64-4107-BA02-CC6DE8A3B8D1}']
  end;

  CMAltitudeData = interface(CMLogItem)
    ['{30F968E9-A139-433E-8068-CED36FBC8A9E}']
    function pressure: NSNumber; cdecl;
    function relativeAltitude: NSNumber; cdecl;
  end;
  TCMAltitudeData = class(TOCGenericImport<CMAltitudeDataClass, CMAltitudeData>) end;

  CMAbsoluteAltitudeDataClass = interface(CMLogItemClass)
    ['{8841860E-1F2A-4BE8-BE29-F85FF31A6C26}']
  end;

  CMAbsoluteAltitudeData = interface(CMLogItem)
    ['{97FCE0F9-F621-4BF1-891B-E7064B850F0C}']
    function accuracy: Double; cdecl;
    function altitude: Double; cdecl;
    function precision: Double; cdecl;
  end;
  TCMAbsoluteAltitudeData = class(TOCGenericImport<CMAbsoluteAltitudeDataClass, CMAbsoluteAltitudeData>) end;

  CMAltimeterClass = interface(NSObjectClass)
    ['{7AED1D97-3A05-4C33-B195-C29D82FF0722}']
    {class} function authorizationStatus: CMAuthorizationStatus; cdecl;
    {class} function isAbsoluteAltitudeAvailable: Boolean; cdecl;
    {class} function isRelativeAltitudeAvailable: Boolean; cdecl;
  end;

  CMAltimeter = interface(NSObject)
    ['{E477ADE8-FCAD-4FC7-A72A-6D5375D1EC06}']
    procedure startAbsoluteAltitudeUpdatesToQueue(queue: NSOperationQueue; withHandler: CMAbsoluteAltitudeHandler); cdecl;
    procedure startRelativeAltitudeUpdatesToQueue(queue: NSOperationQueue; withHandler: CMAltitudeHandler); cdecl;
    procedure stopAbsoluteAltitudeUpdates; cdecl;
    procedure stopRelativeAltitudeUpdates; cdecl;
  end;
  TCMAltimeter = class(TOCGenericImport<CMAltimeterClass, CMAltimeter>) end;

  CMAttitudeClass = interface(NSObjectClass)
    ['{F0C53DF5-02A1-465B-B15B-C42259E45A26}']
  end;

  CMAttitude = interface(NSObject)
    ['{A1729040-68B8-4658-90D5-7EFF750AE753}']
    procedure multiplyByInverseOfAttitude(attitude: CMAttitude); cdecl;
    function pitch: Double; cdecl;
    function quaternion: CMQuaternion; cdecl;
    function roll: Double; cdecl;
    function rotationMatrix: CMRotationMatrix; cdecl;
    function yaw: Double; cdecl;
  end;
  TCMAttitude = class(TOCGenericImport<CMAttitudeClass, CMAttitude>) end;

  CMGyroDataClass = interface(CMLogItemClass)
    ['{B404C9AE-71DC-4946-93DA-14186DBB6E51}']
  end;

  CMGyroData = interface(CMLogItem)
    ['{0FBACFD8-B955-462B-8A59-CCDFBA07CD09}']
    function rotationRate: CMRotationRate; cdecl;
  end;
  TCMGyroData = class(TOCGenericImport<CMGyroDataClass, CMGyroData>) end;

  CMMagnetometerDataClass = interface(CMLogItemClass)
    ['{D520F2F1-E349-406D-8ACB-39F59189E83A}']
  end;

  CMMagnetometerData = interface(CMLogItem)
    ['{6C0A5F16-360A-45BB-9CB4-B9A81A055993}']
    function magneticField: CMMagneticField; cdecl;
  end;
  TCMMagnetometerData = class(TOCGenericImport<CMMagnetometerDataClass, CMMagnetometerData>) end;

  CMDeviceMotionClass = interface(CMLogItemClass)
    ['{7F36B5BB-016D-437F-B7BF-0AEDE71BA85D}']
  end;

  CMDeviceMotion = interface(CMLogItem)
    ['{5A0AE511-BA95-4B6F-9559-6168E7D9445A}']
    function attitude: CMAttitude; cdecl;
    function gravity: CMAcceleration; cdecl;
    function heading: Double; cdecl;
    function magneticField: CMCalibratedMagneticField; cdecl;
    function rotationRate: CMRotationRate; cdecl;
    function sensorLocation: CMDeviceMotionSensorLocation; cdecl;
    function userAcceleration: CMAcceleration; cdecl;
  end;
  TCMDeviceMotion = class(TOCGenericImport<CMDeviceMotionClass, CMDeviceMotion>) end;

  CMFallDetectionEventClass = interface(NSObjectClass)
    ['{E85924A6-D673-4322-BCB2-737DD1E7577D}']
  end;

  CMFallDetectionEvent = interface(NSObject)
    ['{01B26767-EDAD-448D-85C9-E4F61E54C33D}']
    function date: NSDate; cdecl;
    function resolution: CMFallDetectionEventUserResolution; cdecl;
  end;
  TCMFallDetectionEvent = class(TOCGenericImport<CMFallDetectionEventClass, CMFallDetectionEvent>) end;

  CMFallDetectionManagerClass = interface(NSObjectClass)
    ['{936910E5-5406-489B-AEBB-5A73B968A181}']
    {class} function isAvailable: Boolean; cdecl;
  end;

  CMFallDetectionManager = interface(NSObject)
    ['{A48D50C6-9A19-4A18-87E2-B51766775B13}']
    function authorizationStatus: CMAuthorizationStatus; cdecl;
    function delegate: Pointer; cdecl;
    procedure requestAuthorizationWithHandler(handler: TCMFallDetectionManagerBlockMethod1); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCMFallDetectionManager = class(TOCGenericImport<CMFallDetectionManagerClass, CMFallDetectionManager>) end;

  CMFallDetectionDelegate = interface(IObjectiveC)
    ['{EFE62995-78F1-4913-9F76-13E3C11D7C96}']
    procedure fallDetectionManager(fallDetectionManager: CMFallDetectionManager; didDetectEvent: CMFallDetectionEvent;
      completionHandler: Pointer); cdecl;
    procedure fallDetectionManagerDidChangeAuthorization(fallDetectionManager: CMFallDetectionManager); cdecl;
  end;

  CMMotionActivityClass = interface(CMLogItemClass)
    ['{05581BD0-54A2-4E54-9ED5-53307A9DEB86}']
  end;

  CMMotionActivity = interface(CMLogItem)
    ['{1DE27BFB-5870-4ADD-9991-FD3CA4BEEBCF}']
    function automotive: Boolean; cdecl;
    function confidence: CMMotionActivityConfidence; cdecl;
    function cycling: Boolean; cdecl;
    function running: Boolean; cdecl;
    function startDate: NSDate; cdecl;
    function stationary: Boolean; cdecl;
    function unknown: Boolean; cdecl;
    function walking: Boolean; cdecl;
  end;
  TCMMotionActivity = class(TOCGenericImport<CMMotionActivityClass, CMMotionActivity>) end;

  CMMotionActivityManagerClass = interface(NSObjectClass)
    ['{6CC978A3-03C4-4247-8A26-CD78651BD6A5}']
    {class} function authorizationStatus: CMAuthorizationStatus; cdecl;
    {class} function isActivityAvailable: Boolean; cdecl;
  end;

  CMMotionActivityManager = interface(NSObject)
    ['{645036D4-404B-458E-803C-5F0DC875FD61}']
    procedure queryActivityStartingFromDate(start: NSDate; toDate: NSDate; toQueue: NSOperationQueue;
      withHandler: CMMotionActivityQueryHandler); cdecl;
    procedure startActivityUpdatesToQueue(queue: NSOperationQueue; withHandler: CMMotionActivityHandler); cdecl;
    procedure stopActivityUpdates; cdecl;
  end;
  TCMMotionActivityManager = class(TOCGenericImport<CMMotionActivityManagerClass, CMMotionActivityManager>) end;

  CMMotionManagerClass = interface(NSObjectClass)
    ['{3B757652-6284-42E0-967C-DC422D963008}']
    {class} function availableAttitudeReferenceFrames: CMAttitudeReferenceFrame; cdecl;
  end;

  CMMotionManager = interface(NSObject)
    ['{C16BCAB9-4EA9-40D9-B6D3-50E18FC4A697}']
    function accelerometerData: CMAccelerometerData; cdecl;
    function accelerometerUpdateInterval: NSTimeInterval; cdecl;
    function attitudeReferenceFrame: CMAttitudeReferenceFrame; cdecl;
    function deviceMotion: CMDeviceMotion; cdecl;
    function deviceMotionUpdateInterval: NSTimeInterval; cdecl;
    function gyroData: CMGyroData; cdecl;
    function gyroUpdateInterval: NSTimeInterval; cdecl;
    function isAccelerometerActive: Boolean; cdecl;
    function isAccelerometerAvailable: Boolean; cdecl;
    function isDeviceMotionActive: Boolean; cdecl;
    function isDeviceMotionAvailable: Boolean; cdecl;
    function isGyroActive: Boolean; cdecl;
    function isGyroAvailable: Boolean; cdecl;
    function isMagnetometerActive: Boolean; cdecl;
    function isMagnetometerAvailable: Boolean; cdecl;
    function magnetometerData: CMMagnetometerData; cdecl;
    function magnetometerUpdateInterval: NSTimeInterval; cdecl;
    procedure setAccelerometerUpdateInterval(accelerometerUpdateInterval: NSTimeInterval); cdecl;
    procedure setDeviceMotionUpdateInterval(deviceMotionUpdateInterval: NSTimeInterval); cdecl;
    procedure setGyroUpdateInterval(gyroUpdateInterval: NSTimeInterval); cdecl;
    procedure setMagnetometerUpdateInterval(magnetometerUpdateInterval: NSTimeInterval); cdecl;
    procedure setShowsDeviceMovementDisplay(showsDeviceMovementDisplay: Boolean); cdecl;
    function showsDeviceMovementDisplay: Boolean; cdecl;
    procedure startAccelerometerUpdates; cdecl;
    procedure startAccelerometerUpdatesToQueue(queue: NSOperationQueue; withHandler: CMAccelerometerHandler); cdecl;
    procedure startDeviceMotionUpdates; cdecl;
    procedure startDeviceMotionUpdatesToQueue(queue: NSOperationQueue; withHandler: CMDeviceMotionHandler); cdecl;
    procedure startDeviceMotionUpdatesUsingReferenceFrame(referenceFrame: CMAttitudeReferenceFrame; toQueue: NSOperationQueue;
      withHandler: CMDeviceMotionHandler); overload; cdecl;
    procedure startDeviceMotionUpdatesUsingReferenceFrame(referenceFrame: CMAttitudeReferenceFrame); overload; cdecl;
    procedure startGyroUpdates; cdecl;
    procedure startGyroUpdatesToQueue(queue: NSOperationQueue; withHandler: CMGyroHandler); cdecl;
    procedure startMagnetometerUpdates; cdecl;
    procedure startMagnetometerUpdatesToQueue(queue: NSOperationQueue; withHandler: CMMagnetometerHandler); cdecl;
    procedure stopAccelerometerUpdates; cdecl;
    procedure stopDeviceMotionUpdates; cdecl;
    procedure stopGyroUpdates; cdecl;
    procedure stopMagnetometerUpdates; cdecl;
  end;
  TCMMotionManager = class(TOCGenericImport<CMMotionManagerClass, CMMotionManager>) end;

  CMHeadphoneMotionManagerClass = interface(NSObjectClass)
    ['{F2FC78D4-D725-4FB7-BF2B-31BA404E8E63}']
    {class} function authorizationStatus: CMAuthorizationStatus; cdecl;
  end;

  CMHeadphoneMotionManager = interface(NSObject)
    ['{C383FFD6-3CEE-4875-9C71-8BBFC96AA0B7}']
    function delegate: Pointer; cdecl;
    function deviceMotion: CMDeviceMotion; cdecl;
    function isDeviceMotionActive: Boolean; cdecl;
    function isDeviceMotionAvailable: Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure startDeviceMotionUpdates; cdecl;
    procedure startDeviceMotionUpdatesToQueue(queue: NSOperationQueue; withHandler: CMHeadphoneDeviceMotionHandler); cdecl;
    procedure stopDeviceMotionUpdates; cdecl;
  end;
  TCMHeadphoneMotionManager = class(TOCGenericImport<CMHeadphoneMotionManagerClass, CMHeadphoneMotionManager>) end;

  CMHeadphoneMotionManagerDelegate = interface(IObjectiveC)
    ['{54FE3F50-C16E-40A3-A94E-0F66360066B1}']
    procedure headphoneMotionManagerDidConnect(manager: CMHeadphoneMotionManager); cdecl;
    procedure headphoneMotionManagerDidDisconnect(manager: CMHeadphoneMotionManager); cdecl;
  end;

  CMDyskineticSymptomResultClass = interface(NSObjectClass)
    ['{5677A343-187A-4230-A606-4930777C1B18}']
  end;

  CMDyskineticSymptomResult = interface(NSObject)
    ['{03EB0136-DE19-4A8C-BD08-76BB069F0FB6}']
    function endDate: NSDate; cdecl;
    function percentLikely: Single; cdecl;
    function percentUnlikely: Single; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TCMDyskineticSymptomResult = class(TOCGenericImport<CMDyskineticSymptomResultClass, CMDyskineticSymptomResult>) end;

  CMTremorResultClass = interface(NSObjectClass)
    ['{9263E1F6-76FB-4C69-A516-C542CA1D1C47}']
  end;

  CMTremorResult = interface(NSObject)
    ['{8717F384-BE21-4620-9665-6172C4917EFA}']
    function endDate: NSDate; cdecl;
    function percentMild: Single; cdecl;
    function percentModerate: Single; cdecl;
    function percentNone: Single; cdecl;
    function percentSlight: Single; cdecl;
    function percentStrong: Single; cdecl;
    function percentUnknown: Single; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TCMTremorResult = class(TOCGenericImport<CMTremorResultClass, CMTremorResult>) end;

  CMMovementDisorderManagerClass = interface(NSObjectClass)
    ['{27E3A595-07F5-4C89-BAE7-55DDA27C25D7}']
    {class} function authorizationStatus: CMAuthorizationStatus; cdecl;
    {class} function isAvailable: Boolean; cdecl;
    {class} function version: NSString; cdecl;
  end;

  CMMovementDisorderManager = interface(NSObject)
    ['{B67088AB-3040-4C6B-A426-AB4D2E2DDAF2}']
    function lastProcessedDate: NSDate; cdecl;
    function monitorKinesiasExpirationDate: NSDate; cdecl;
    procedure monitorKinesiasForDuration(duration: NSTimeInterval); cdecl;
    procedure queryDyskineticSymptomFromDate(fromDate: NSDate; toDate: NSDate; withHandler: CMDyskineticSymptomResultHandler); cdecl;
    procedure queryTremorFromDate(fromDate: NSDate; toDate: NSDate; withHandler: CMTremorResultHandler); cdecl;
  end;
  TCMMovementDisorderManager = class(TOCGenericImport<CMMovementDisorderManagerClass, CMMovementDisorderManager>) end;

  CMPedometerDataClass = interface(NSObjectClass)
    ['{57C110AB-B4FE-488A-8D7B-D16FD4A8BB45}']
  end;

  CMPedometerData = interface(NSObject)
    ['{11925067-464E-4D4B-96FD-3849214845A6}']
    function averageActivePace: NSNumber; cdecl;
    function currentCadence: NSNumber; cdecl;
    function currentPace: NSNumber; cdecl;
    function distance: NSNumber; cdecl;
    function endDate: NSDate; cdecl;
    function floorsAscended: NSNumber; cdecl;
    function floorsDescended: NSNumber; cdecl;
    function numberOfSteps: NSNumber; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TCMPedometerData = class(TOCGenericImport<CMPedometerDataClass, CMPedometerData>) end;

  CMPedometerEventClass = interface(NSObjectClass)
    ['{5ECADC72-2584-482A-ABD9-738A5BB21436}']
  end;

  CMPedometerEvent = interface(NSObject)
    ['{709B59CC-309A-48E3-909E-3198115E4295}']
    function &type: CMPedometerEventType; cdecl;
    function date: NSDate; cdecl;
  end;
  TCMPedometerEvent = class(TOCGenericImport<CMPedometerEventClass, CMPedometerEvent>) end;

  CMPedometerClass = interface(NSObjectClass)
    ['{19EEC18C-6460-4DF7-9E1F-BBA382954EF2}']
    {class} function authorizationStatus: CMAuthorizationStatus; cdecl;
    {class} function isCadenceAvailable: Boolean; cdecl;
    {class} function isDistanceAvailable: Boolean; cdecl;
    {class} function isFloorCountingAvailable: Boolean; cdecl;
    {class} function isPaceAvailable: Boolean; cdecl;
    {class} function isPedometerEventTrackingAvailable: Boolean; cdecl;
    {class} function isStepCountingAvailable: Boolean; cdecl;
  end;

  CMPedometer = interface(NSObject)
    ['{F6F46820-A089-4C4C-8A66-F65143F11109}']
    procedure queryPedometerDataFromDate(start: NSDate; toDate: NSDate; withHandler: CMPedometerHandler); cdecl;
    procedure startPedometerEventUpdatesWithHandler(handler: CMPedometerEventHandler); cdecl;
    procedure startPedometerUpdatesFromDate(start: NSDate; withHandler: CMPedometerHandler); cdecl;
    procedure stopPedometerEventUpdates; cdecl;
    procedure stopPedometerUpdates; cdecl;
  end;
  TCMPedometer = class(TOCGenericImport<CMPedometerClass, CMPedometer>) end;

  CMRotationRateDataClass = interface(CMLogItemClass)
    ['{235EA64B-D4A2-4657-B034-8D868F09CE9D}']
  end;

  CMRotationRateData = interface(CMLogItem)
    ['{9DEAC85D-E9D6-47BD-8B2B-0ADC8488CACC}']
    function rotationRate: CMRotationRate; cdecl;
  end;
  TCMRotationRateData = class(TOCGenericImport<CMRotationRateDataClass, CMRotationRateData>) end;

  CMRecordedRotationRateDataClass = interface(CMRotationRateDataClass)
    ['{9D3AD693-2033-4E0B-8915-08514DD46707}']
  end;

  CMRecordedRotationRateData = interface(CMRotationRateData)
    ['{2BAEB51D-98C4-4CA4-8E93-EB50E9C47958}']
    function startDate: NSDate; cdecl;
  end;
  TCMRecordedRotationRateData = class(TOCGenericImport<CMRecordedRotationRateDataClass, CMRecordedRotationRateData>) end;

  CMStepCounterClass = interface(NSObjectClass)
    ['{E48BBE18-2E8B-41C9-B330-891F02D991A4}']
    {class} function isStepCountingAvailable: Boolean; cdecl;
  end;

  CMStepCounter = interface(NSObject)
    ['{85D1D2F5-D0D2-4250-B227-8899DAD72527}']
    procedure queryStepCountStartingFrom(start: NSDate; &to: NSDate; toQueue: NSOperationQueue; withHandler: CMStepQueryHandler); cdecl;
    procedure startStepCountingUpdatesToQueue(queue: NSOperationQueue; updateOn: NSInteger; withHandler: CMStepUpdateHandler); cdecl;
    procedure stopStepCountingUpdates; cdecl;
  end;
  TCMStepCounter = class(TOCGenericImport<CMStepCounterClass, CMStepCounter>) end;

  CMRecordedAccelerometerDataClass = interface(CMAccelerometerDataClass)
    ['{8466462C-22E4-4124-904F-B3DF61AC6A05}']
  end;

  CMRecordedAccelerometerData = interface(CMAccelerometerData)
    ['{A2EBA67C-E32E-4DF5-9120-EDD82FC1DC19}']
    function identifier: UInt64; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TCMRecordedAccelerometerData = class(TOCGenericImport<CMRecordedAccelerometerDataClass, CMRecordedAccelerometerData>) end;

  CMSensorDataListClass = interface(NSObjectClass)
    ['{DF435791-35F7-4D9D-B910-E8574F29E3FF}']
  end;

  CMSensorDataList = interface(NSObject)
    ['{EAE50FC1-0DF1-4D68-95CF-B667C02C3A96}']
  end;
  TCMSensorDataList = class(TOCGenericImport<CMSensorDataListClass, CMSensorDataList>) end;

  CMSensorRecorderClass = interface(NSObjectClass)
    ['{F652CE3F-B7F9-4DC7-A20F-66D76D6B11ED}']
    {class} function authorizationStatus: CMAuthorizationStatus; cdecl;
    {class} function isAccelerometerRecordingAvailable: Boolean; cdecl;
    {class} function isAuthorizedForRecording: Boolean; cdecl;
  end;

  CMSensorRecorder = interface(NSObject)
    ['{C2EEF10A-845A-47C3-9991-7735D3B0DF1D}']
    function accelerometerDataFromDate(fromDate: NSDate; toDate: NSDate): CMSensorDataList; cdecl;
    procedure recordAccelerometerForDuration(duration: NSTimeInterval); cdecl;
  end;
  TCMSensorRecorder = class(TOCGenericImport<CMSensorRecorderClass, CMSensorRecorder>) end;

  CMAmbientPressureDataClass = interface(CMLogItemClass)
    ['{B0AEAC38-3AA7-466C-B9A2-AD43DE688C69}']
  end;

  CMAmbientPressureData = interface(CMLogItem)
    ['{32760627-308C-4371-9C8A-0724E3C09A33}']
    function pressure: NSMeasurement; cdecl;
    function temperature: NSMeasurement; cdecl;
  end;
  TCMAmbientPressureData = class(TOCGenericImport<CMAmbientPressureDataClass, CMAmbientPressureData>) end;

  CMRecordedPressureDataClass = interface(CMAmbientPressureDataClass)
    ['{0F41502E-22DB-46B2-9580-D08D1C161621}']
  end;

  CMRecordedPressureData = interface(CMAmbientPressureData)
    ['{9560AC48-5234-411A-9FF5-E280128B185D}']
    function identifier: UInt64; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TCMRecordedPressureData = class(TOCGenericImport<CMRecordedPressureDataClass, CMRecordedPressureData>) end;

  CMWaterSubmersionEventClass = interface(NSObjectClass)
    ['{59EAA6AE-98BE-494F-9EBE-3D883947E822}']
  end;

  CMWaterSubmersionEvent = interface(NSObject)
    ['{4AA7A921-63C0-47F8-B98C-9B2ACC89140F}']
    function date: NSDate; cdecl;
    function state: CMWaterSubmersionState; cdecl;
  end;
  TCMWaterSubmersionEvent = class(TOCGenericImport<CMWaterSubmersionEventClass, CMWaterSubmersionEvent>) end;

  CMWaterSubmersionMeasurementClass = interface(NSObjectClass)
    ['{7F9BD9CB-63E5-4626-8530-A8AA9F7B7475}']
  end;

  CMWaterSubmersionMeasurement = interface(NSObject)
    ['{D3B60AF5-F850-4E04-B6DA-B2818B393279}']
    function date: NSDate; cdecl;
    function depth: NSMeasurement; cdecl;
    function pressure: NSMeasurement; cdecl;
    function submersionState: CMWaterSubmersionDepthState; cdecl;
    function surfacePressure: NSMeasurement; cdecl;
  end;
  TCMWaterSubmersionMeasurement = class(TOCGenericImport<CMWaterSubmersionMeasurementClass, CMWaterSubmersionMeasurement>) end;

  CMWaterTemperatureClass = interface(NSObjectClass)
    ['{33F94BE5-1D2C-4345-9686-33F47E9DA225}']
  end;

  CMWaterTemperature = interface(NSObject)
    ['{07002FB2-83C7-42DE-AFB2-D6E1BA2123D4}']
    function date: NSDate; cdecl;
    function temperature: NSMeasurement; cdecl;
    function temperatureUncertainty: NSMeasurement; cdecl;
  end;
  TCMWaterTemperature = class(TOCGenericImport<CMWaterTemperatureClass, CMWaterTemperature>) end;

  CMWaterSubmersionManagerDelegate = interface(IObjectiveC)
    ['{B889F8AD-1FE4-4364-B5CA-ABD2166540A9}']
    procedure manager(manager: CMWaterSubmersionManager; errorOccurred: NSError); overload; cdecl;
    procedure manager(manager: CMWaterSubmersionManager; didUpdateTemperature: CMWaterTemperature); overload; cdecl;
    procedure manager(manager: CMWaterSubmersionManager; didUpdateMeasurement: CMWaterSubmersionMeasurement); overload; cdecl;
    procedure manager(manager: CMWaterSubmersionManager; didUpdateEvent: CMWaterSubmersionEvent); overload; cdecl;
  end;

  CMWaterSubmersionManagerClass = interface(NSObjectClass)
    ['{67BA4058-287E-4AD3-9F62-303047A8F4C3}']
    {class} function authorizationStatus: CMAuthorizationStatus; cdecl;
    {class} function waterSubmersionAvailable: Boolean; cdecl;
  end;

  CMWaterSubmersionManager = interface(NSObject)
    ['{F37E91AF-75DB-4D02-B59A-2482915AB125}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCMWaterSubmersionManager = class(TOCGenericImport<CMWaterSubmersionManagerClass, CMWaterSubmersionManager>) end;

function CMErrorDomain: NSString;

const
  libCoreMotion = '/System/Library/Frameworks/CoreMotion.framework/CoreMotion';

implementation

uses
  Posix.Dlfcn;

var
  CoreMotionModule: THandle;

function CMErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCoreMotion, 'CMErrorDomain');
end;

initialization
  CoreMotionModule := dlopen(MarshaledAString(libCoreMotion), RTLD_LAZY);

finalization
  dlclose(CoreMotionModule);

end.