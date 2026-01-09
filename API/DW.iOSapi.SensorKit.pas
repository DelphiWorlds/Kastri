unit DW.iOSapi.SensorKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreLocation, iOSapi.CoreMedia,
  // DW
  DW.iOSapi.Foundation, DW.iOSapi.Speech, DW.iOSapi.SoundAnalysis;

const
  SRAuthorizationStatusNotDetermined = 0;
  SRAuthorizationStatusAuthorized = 1;
  SRAuthorizationStatusDenied = 2;
  SRErrorInvalidEntitlement = 0;
  SRErrorNoAuthorization = 1;
  SRErrorDataInaccessible = 2;
  SRErrorFetchRequestInvalid = 3;
  SRErrorPromptDeclined = 4;
  SRAmbientLightSensorPlacementUnknown = 0;
  SRAmbientLightSensorPlacementFrontTop = 1;
  SRAmbientLightSensorPlacementFrontBottom = 2;
  SRAmbientLightSensorPlacementFrontRight = 3;
  SRAmbientLightSensorPlacementFrontLeft = 4;
  SRAmbientLightSensorPlacementFrontTopRight = 5;
  SRAmbientLightSensorPlacementFrontTopLeft = 6;
  SRAmbientLightSensorPlacementFrontBottomRight = 7;
  SRAmbientLightSensorPlacementFrontBottomLeft = 8;
  SRLocationCategoryUnknown = 0;
  SRLocationCategoryHome = 1;
  SRLocationCategoryWork = 2;
  SRLocationCategorySchool = 3;
  SRLocationCategoryGym = 4;
  SRTextInputSessionTypeKeyboard = 1;
  SRTextInputSessionTypeThirdPartyKeyboard = 2;
  SRTextInputSessionTypePencil = 3;
  SRTextInputSessionTypeDictation = 4;
  SRNotificationEventUnknown = 0;
  SRNotificationEventReceived = 1;
  SRNotificationEventDefaultAction = 2;
  SRNotificationEventSupplementaryAction = 3;
  SRNotificationEventClear = 4;
  SRNotificationEventNotificationCenterClearAll = 5;
  SRNotificationEventRemoved = 6;
  SRNotificationEventHide = 7;
  SRNotificationEventLongLook = 8;
  SRNotificationEventSilence = 9;
  SRNotificationEventAppLaunch = 10;
  SRNotificationEventExpired = 11;
  SRNotificationEventBannerPulldown = 12;
  SRNotificationEventTapCoalesce = 13;
  SRNotificationEventDeduped = 14;
  SRNotificationEventDeviceActivated = 15;
  SRNotificationEventDeviceUnlocked = 16;
  SRKeyboardMetricsSentimentCategoryAbsolutist = 0;
  SRKeyboardMetricsSentimentCategoryDown = 1;
  SRKeyboardMetricsSentimentCategoryDeath = 2;
  SRKeyboardMetricsSentimentCategoryAnxiety = 3;
  SRKeyboardMetricsSentimentCategoryAnger = 4;
  SRKeyboardMetricsSentimentCategoryHealth = 5;
  SRKeyboardMetricsSentimentCategoryPositive = 6;
  SRKeyboardMetricsSentimentCategorySad = 7;
  SRKeyboardMetricsSentimentCategoryLowEnergy = 8;
  SRKeyboardMetricsSentimentCategoryConfused = 9;
  SRDeletionReasonUserInitiated = 0;
  SRDeletionReasonLowDiskSpace = 1;
  SRDeletionReasonAgeLimit = 2;
  SRDeletionReasonNoInterestedClients = 3;
  SRDeletionReasonSystemInitiated = 4;
  SRWristLocationLeft = 0;
  SRWristLocationRight = 1;
  SRCrownOrientationLeft = 0;
  SRCrownOrientationRight = 1;
  SRWristTemperatureConditionNone = 0;
  SRWristTemperatureConditionOffWrist = 1;
  SRWristTemperatureConditionOnCharger = 2;
  SRWristTemperatureConditionInMotion = 4;
  SRMediaEventOnScreen = 1;
  SRMediaEventOffScreen = 2;
  SRSpeechMetricsSessionFlagsDefault = 0;
  SRSpeechMetricsSessionFlagsBypassVoiceProcessing = 1 shl 0;
  SRFaceMetricsContextDeviceUnlock = 1;
  SRFaceMetricsContextMessagingAppUsage = 2;
  SRElectrocardiogramSessionStateBegin = 1;
  SRElectrocardiogramSessionStateActive = 2;
  SRElectrocardiogramSessionStateEnd = 3;
  SRElectrocardiogramSessionGuidanceGuided = 1;
  SRElectrocardiogramSessionGuidanceUnguided = 2;
  SRElectrocardiogramDataFlagsNone = 0;
  SRElectrocardiogramDataFlagsSignalInvalid = 1;
  SRElectrocardiogramDataFlagsCrownTouched = 2;
  SRElectrocardiogramLeadRightArmMinusLeftArm = 1;
  SRElectrocardiogramLeadLeftArmMinusRightArm = 2;

type
  SRFetchResult = interface;
  SRDevice = interface;
  SRFetchRequest = interface;
  SRSensorReaderDelegate = interface;
  SRSensorReader = interface;
  SRAmbientLightSample = interface;
  SRVisit = interface;
  SRSupplementalCategory = interface;
  SRDeviceUsageReport = interface;
  SRTextInputSession = interface;
  SRApplicationUsage = interface;
  SRNotificationUsage = interface;
  SRWebUsage = interface;
  SRMessagesUsageReport = interface;
  SRPhoneUsageReport = interface;
  SRKeyboardMetrics = interface;
  SRKeyboardProbabilityMetric = interface;
  SRDeletionRecord = interface;
  SRWristDetection = interface;
  SRWristTemperature = interface;
  SRWristTemperatureSession = interface;
  SRMediaEvent = interface;
  SRSpeechExpression = interface;
  SRAudioLevel = interface;
  SRSpeechMetrics = interface;
  SRFaceMetricsExpression = interface;
  SRFaceMetrics = interface;
  SRElectrocardiogramSession = interface;
  SRElectrocardiogramData = interface;
  SRElectrocardiogramSample = interface;
  SRPhotoplethysmogramOpticalSample = interface;
  SRPhotoplethysmogramAccelerometerSample = interface;
  SRPhotoplethysmogramSample = interface;

  PSRAmbientLightChromaticity = ^SRAmbientLightChromaticity;

  SRSensor = NSString;
  SRAbsoluteTime = CFTimeInterval;
  SRAuthorizationStatus = NSInteger;
  SRErrorCode = NSInteger;
  SRAmbientLightSensorPlacement = NSInteger;

  SRAmbientLightChromaticity = record
    x: Float32;
    y: Float32;
  end;

  SRLocationCategory = NSInteger;
  SRDeviceUsageCategoryKey = NSString;
  SRTextInputSessionType = NSInteger;
  SRNotificationEvent = NSInteger;
  SRKeyboardMetricsSentimentCategory = NSInteger;
  SRDeletionReason = NSInteger;
  SRWristLocation = NSInteger;
  SRCrownOrientation = NSInteger;
  SRWristTemperatureCondition = NSInteger;
  SRMediaEventType = NSInteger;
  SRSpeechMetricsSessionFlags = NSInteger;
  SRFaceMetricsContext = NSInteger;
  SRElectrocardiogramSessionState = NSInteger;
  SRElectrocardiogramSessionGuidance = NSInteger;
  SRElectrocardiogramDataFlags = NSInteger;
  SRElectrocardiogramLead = NSInteger;
  SRPhotoplethysmogramOpticalSampleCondition = NSString;
  SRPhotoplethysmogramSampleUsage = NSString;
  TSRSensorReaderBlockMethod1 = procedure(error: NSError) of object;

  SRFetchResultClass = interface(NSObjectClass)
    ['{16BD49D0-399D-44E9-8561-20CBC7AC85EB}']
    {class} function new: Pointer; cdecl;
  end;

  SRFetchResult = interface(NSObject)
    ['{1F8586D1-1CFB-4987-9837-ECF86F788AE6}']
    function sample: Pointer; cdecl; // :SampleType; cdecl;
    function timestamp: SRAbsoluteTime; cdecl;
  end;
  TSRFetchResult = class(TOCGenericImport<SRFetchResultClass, SRFetchResult>) end;

  SRDeviceClass = interface(NSObjectClass)
    ['{126F9A55-BB01-4F10-ACF4-EEC02EE8DDBB}']
    {class} function currentDevice: SRDevice; cdecl;
  end;

  SRDevice = interface(NSObject)
    ['{C3FA1953-3322-47C6-995A-DF5774379262}']
    function model: NSString; cdecl;
    function name: NSString; cdecl;
    function productType: NSString; cdecl;
    function systemName: NSString; cdecl;
    function systemVersion: NSString; cdecl;
  end;
  TSRDevice = class(TOCGenericImport<SRDeviceClass, SRDevice>) end;

  SRFetchRequestClass = interface(NSObjectClass)
    ['{00B653FF-DB63-4490-BF19-D1E908C12760}']
  end;

  SRFetchRequest = interface(NSObject)
    ['{837B7686-E215-4667-B002-0C19FBEEF7B9}']
    function &to: SRAbsoluteTime; cdecl;
    function device: SRDevice; cdecl;
    function from: SRAbsoluteTime; cdecl;
    procedure setDevice(device: SRDevice); cdecl;
    procedure setFrom(from: SRAbsoluteTime); cdecl;
    procedure setTo(&to: SRAbsoluteTime); cdecl;
  end;
  TSRFetchRequest = class(TOCGenericImport<SRFetchRequestClass, SRFetchRequest>) end;

  SRSensorReaderDelegate = interface(IObjectiveC)
    ['{515730A9-3CD6-4240-938D-0ADCFF7B1A10}']
    procedure sensorReader(reader: SRSensorReader; didChangeAuthorizationStatus: SRAuthorizationStatus); overload; cdecl;
    procedure sensorReader(reader: SRSensorReader; didFetchDevices: NSArray); overload; cdecl;
    procedure sensorReader(reader: SRSensorReader; startRecordingFailedWithError: NSError); overload; cdecl;
    function sensorReader(reader: SRSensorReader; fetchingRequest: SRFetchRequest; didFetchResult: SRFetchResult): Boolean; overload; cdecl;
    procedure sensorReader(reader: SRSensorReader; didCompleteFetch: SRFetchRequest); overload; cdecl;
    procedure sensorReader(reader: SRSensorReader; fetchingRequest: SRFetchRequest; failedWithError: NSError); overload; cdecl;
    procedure sensorReaderDidStopRecording(reader: SRSensorReader); cdecl;
    [MethodName('sensorReader:fetchDevicesDidFailWithError:')]
    procedure sensorReaderFetchDevicesDidFailWithError(reader: SRSensorReader; fetchDevicesDidFailWithError: NSError); cdecl;
    [MethodName('sensorReader:stopRecordingFailedWithError:')]
    procedure sensorReaderStopRecordingFailedWithError(reader: SRSensorReader; stopRecordingFailedWithError: NSError); cdecl;
    procedure sensorReaderWillStartRecording(reader: SRSensorReader); cdecl;
  end;

  SRSensorReaderClass = interface(NSObjectClass)
    ['{B82A519D-DF3F-4625-B687-A9C52EFEE37C}']
    {class} function new: Pointer; cdecl;
    {class} procedure requestAuthorizationForSensors(sensors: NSSet; completion: TSRSensorReaderBlockMethod1); cdecl;
  end;

  SRSensorReader = interface(NSObject)
    ['{ED0A9FC8-82E5-4D51-AE6B-5C63819ABCDD}']
    function authorizationStatus: SRAuthorizationStatus; cdecl;
    function delegate: Pointer; cdecl;
    procedure fetch(request: SRFetchRequest); cdecl;
    procedure fetchDevices; cdecl;
    function initWithSensor(sensor: SRSensor): Pointer; cdecl;
    function sensor: SRSensor; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure startRecording; cdecl;
    procedure stopRecording; cdecl;
  end;
  TSRSensorReader = class(TOCGenericImport<SRSensorReaderClass, SRSensorReader>) end;

  SRAmbientLightSampleClass = interface(NSObjectClass)
    ['{2EBE16DD-C1C4-484E-BDAB-B584FFC78EDC}']
  end;

  SRAmbientLightSample = interface(NSObject)
    ['{74A3553E-6506-4803-9F8E-22FF0BC32ADD}']
    function chromaticity: SRAmbientLightChromaticity; cdecl;
    function lux: NSMeasurement; cdecl;
    function placement: SRAmbientLightSensorPlacement; cdecl;
  end;
  TSRAmbientLightSample = class(TOCGenericImport<SRAmbientLightSampleClass, SRAmbientLightSample>) end;

  SRVisitClass = interface(NSObjectClass)
    ['{0B73D192-8EB5-4749-9453-60A67E593767}']
  end;

  SRVisit = interface(NSObject)
    ['{A8BEC33F-100C-49F7-B403-D826A552592B}']
    function arrivalDateInterval: NSDateInterval; cdecl;
    function departureDateInterval: NSDateInterval; cdecl;
    function distanceFromHome: CLLocationDistance; cdecl;
    function identifier: NSUUID; cdecl;
    function locationCategory: SRLocationCategory; cdecl;
  end;
  TSRVisit = class(TOCGenericImport<SRVisitClass, SRVisit>) end;

  SRSupplementalCategoryClass = interface(NSObjectClass)
    ['{75A12D65-7F3F-4761-AF2D-8ECBF7CBF547}']
    {class} function new: Pointer; cdecl;
  end;

  SRSupplementalCategory = interface(NSObject)
    ['{41E4C524-3358-4D8B-BC59-99DE1B7346D8}']
    function identifier: NSString; cdecl;
  end;
  TSRSupplementalCategory = class(TOCGenericImport<SRSupplementalCategoryClass, SRSupplementalCategory>) end;

  SRDeviceUsageReportClass = interface(NSObjectClass)
    ['{AE2AB023-157D-47D3-8E1B-CF65BB739E57}']
  end;

  SRDeviceUsageReport = interface(NSObject)
    ['{1986FAFA-A455-4C01-A85D-36A809113706}']
    function applicationUsageByCategory: NSDictionary; cdecl;
    function duration: NSTimeInterval; cdecl;
    function notificationUsageByCategory: NSDictionary; cdecl;
    function totalScreenWakes: NSInteger; cdecl;
    function totalUnlockDuration: NSTimeInterval; cdecl;
    function totalUnlocks: NSInteger; cdecl;
    function version: NSString; cdecl;
    function webUsageByCategory: NSDictionary; cdecl;
  end;
  TSRDeviceUsageReport = class(TOCGenericImport<SRDeviceUsageReportClass, SRDeviceUsageReport>) end;

  SRTextInputSessionClass = interface(NSObjectClass)
    ['{55127601-FDB4-4EE3-97B5-2298493E25E0}']
  end;

  SRTextInputSession = interface(NSObject)
    ['{2A47BC58-9A9D-4CF9-8AB2-B0B60F04D638}']
    function duration: NSTimeInterval; cdecl;
    function sessionIdentifier: NSString; cdecl;
    function sessionType: SRTextInputSessionType; cdecl;
  end;
  TSRTextInputSession = class(TOCGenericImport<SRTextInputSessionClass, SRTextInputSession>) end;

  SRApplicationUsageClass = interface(NSObjectClass)
    ['{2A2B1F36-B2D9-4992-9F3B-07261958F61C}']
  end;

  SRApplicationUsage = interface(NSObject)
    ['{27581D39-49E1-4CF8-995F-1CC80C26452C}']
    function bundleIdentifier: NSString; cdecl;
    function relativeStartTime: NSTimeInterval; cdecl;
    function reportApplicationIdentifier: NSString; cdecl;
    function supplementalCategories: NSArray; cdecl;
    function textInputSessions: NSArray; cdecl;
    function usageTime: NSTimeInterval; cdecl;
  end;
  TSRApplicationUsage = class(TOCGenericImport<SRApplicationUsageClass, SRApplicationUsage>) end;

  SRNotificationUsageClass = interface(NSObjectClass)
    ['{8E3F65B5-3E75-409A-83C1-D5F29DB83DBB}']
  end;

  SRNotificationUsage = interface(NSObject)
    ['{7F6B4B11-4877-4A81-AADA-1257305CA365}']
    function bundleIdentifier: NSString; cdecl;
    function event: SRNotificationEvent; cdecl;
  end;
  TSRNotificationUsage = class(TOCGenericImport<SRNotificationUsageClass, SRNotificationUsage>) end;

  SRWebUsageClass = interface(NSObjectClass)
    ['{338A29D4-EBC0-47C8-9946-4C716535A0EB}']
  end;

  SRWebUsage = interface(NSObject)
    ['{AF60AE16-A762-4202-83F4-BFA94B19132B}']
    function totalUsageTime: NSTimeInterval; cdecl;
  end;
  TSRWebUsage = class(TOCGenericImport<SRWebUsageClass, SRWebUsage>) end;

  SRMessagesUsageReportClass = interface(NSObjectClass)
    ['{758EFC9F-E4A7-428F-B08A-91A7E27BF9C4}']
  end;

  SRMessagesUsageReport = interface(NSObject)
    ['{53C665AC-25CF-4422-9A10-BF7BA1F326A9}']
    function duration: NSTimeInterval; cdecl;
    function totalIncomingMessages: NSInteger; cdecl;
    function totalOutgoingMessages: NSInteger; cdecl;
    function totalUniqueContacts: NSInteger; cdecl;
  end;
  TSRMessagesUsageReport = class(TOCGenericImport<SRMessagesUsageReportClass, SRMessagesUsageReport>) end;

  SRPhoneUsageReportClass = interface(NSObjectClass)
    ['{78AFAE72-9FDF-4990-BA5F-D3DF44953FC3}']
  end;

  SRPhoneUsageReport = interface(NSObject)
    ['{D5B74895-1736-4C9D-9502-F7994548361B}']
    function duration: NSTimeInterval; cdecl;
    function totalIncomingCalls: NSInteger; cdecl;
    function totalOutgoingCalls: NSInteger; cdecl;
    function totalPhoneCallDuration: NSTimeInterval; cdecl;
    function totalUniqueContacts: NSInteger; cdecl;
  end;
  TSRPhoneUsageReport = class(TOCGenericImport<SRPhoneUsageReportClass, SRPhoneUsageReport>) end;

  SRKeyboardMetricsClass = interface(NSObjectClass)
    ['{2FFD5216-EF00-4D54-8420-E85BDEB5FDC3}']
  end;

  SRKeyboardMetrics = interface(NSObject)
    ['{2CF86690-B4C5-471D-A329-96CAC0BD21EF}']
    function anyTapToCharKey: SRKeyboardProbabilityMetric; cdecl;
    function anyTapToPlaneChangeKey: SRKeyboardProbabilityMetric; cdecl;
    function charKeyToAnyTapKey: SRKeyboardProbabilityMetric; cdecl;
    function charKeyToDelete: SRKeyboardProbabilityMetric; cdecl;
    function charKeyToPlaneChangeKey: SRKeyboardProbabilityMetric; cdecl;
    function charKeyToPrediction: SRKeyboardProbabilityMetric; cdecl;
    function charKeyToSpaceKey: SRKeyboardProbabilityMetric; cdecl;
    function deleteDownErrorDistance: SRKeyboardProbabilityMetric; cdecl;
    function deleteToCharKey: SRKeyboardProbabilityMetric; cdecl;
    function deleteToDelete: SRKeyboardProbabilityMetric; cdecl;
    function deleteToDeletes: NSArray; cdecl;
    function deleteToPath: SRKeyboardProbabilityMetric; cdecl;
    function deleteToPlaneChangeKey: SRKeyboardProbabilityMetric; cdecl;
    function deleteToShiftKey: SRKeyboardProbabilityMetric; cdecl;
    function deleteToSpaceKey: SRKeyboardProbabilityMetric; cdecl;
    function deleteTouchDownUp: SRKeyboardProbabilityMetric; cdecl;
    function deleteUpErrorDistance: SRKeyboardProbabilityMetric; cdecl;
    function downErrorDistance: SRKeyboardProbabilityMetric; cdecl;
    function duration: NSTimeInterval; cdecl;
    function emojiCountForSentimentCategory(category: SRKeyboardMetricsSentimentCategory): NSInteger; cdecl;
    function height: NSMeasurement; cdecl;
    function inputModes: NSArray; cdecl;
    function keyboardIdentifier: NSString; cdecl;
    function longWordDownErrorDistance: NSArray; cdecl;
    function longWordTouchDownDown: NSArray; cdecl;
    function longWordTouchDownUp: NSArray; cdecl;
    function longWordTouchUpDown: NSArray; cdecl;
    function longWordUpErrorDistance: NSArray; cdecl;
    function pathErrorDistanceRatio: NSArray; cdecl;
    function pathToDelete: SRKeyboardProbabilityMetric; cdecl;
    function pathToPath: SRKeyboardProbabilityMetric; cdecl;
    function pathToSpace: SRKeyboardProbabilityMetric; cdecl;
    function pathTypingSpeed: Double; cdecl;
    function planeChangeKeyToCharKey: SRKeyboardProbabilityMetric; cdecl;
    function planeChangeToAnyTap: SRKeyboardProbabilityMetric; cdecl;
    function sessionIdentifiers: NSArray; cdecl;
    function shortWordCharKeyDownErrorDistance: SRKeyboardProbabilityMetric; cdecl;
    function shortWordCharKeyToCharKey: SRKeyboardProbabilityMetric; cdecl;
    function shortWordCharKeyTouchDownUp: SRKeyboardProbabilityMetric; cdecl;
    function shortWordCharKeyUpErrorDistance: SRKeyboardProbabilityMetric; cdecl;
    function spaceDownErrorDistance: SRKeyboardProbabilityMetric; cdecl;
    function spaceToCharKey: SRKeyboardProbabilityMetric; cdecl;
    function spaceToDeleteKey: SRKeyboardProbabilityMetric; cdecl;
    function spaceToPath: SRKeyboardProbabilityMetric; cdecl;
    function spaceToPlaneChangeKey: SRKeyboardProbabilityMetric; cdecl;
    function spaceToPredictionKey: SRKeyboardProbabilityMetric; cdecl;
    function spaceToShiftKey: SRKeyboardProbabilityMetric; cdecl;
    function spaceToSpaceKey: SRKeyboardProbabilityMetric; cdecl;
    function spaceTouchDownUp: SRKeyboardProbabilityMetric; cdecl;
    function spaceUpErrorDistance: SRKeyboardProbabilityMetric; cdecl;
    function totalAlteredWords: NSInteger; cdecl;
    function totalAutoCorrections: NSInteger; cdecl;
    function totalDeletes: NSInteger; cdecl;
    function totalDrags: NSInteger; cdecl;
    function totalEmojis: NSInteger; cdecl;
    function totalHitTestCorrections: NSInteger; cdecl;
    function totalInsertKeyCorrections: NSInteger; cdecl;
    function totalNearKeyCorrections: NSInteger; cdecl;
    function totalPathLength: NSMeasurement; cdecl;
    function totalPathPauses: NSInteger; cdecl;
    function totalPaths: NSInteger; cdecl;
    function totalPathTime: NSTimeInterval; cdecl;
    function totalPauses: NSInteger; cdecl;
    function totalRetroCorrections: NSInteger; cdecl;
    function totalSkipTouchCorrections: NSInteger; cdecl;
    function totalSpaceCorrections: NSInteger; cdecl;
    function totalSubstitutionCorrections: NSInteger; cdecl;
    function totalTaps: NSInteger; cdecl;
    function totalTranspositionCorrections: NSInteger; cdecl;
    function totalTypingDuration: NSTimeInterval; cdecl;
    function totalTypingEpisodes: NSInteger; cdecl;
    function totalWords: NSInteger; cdecl;
    function touchDownDown: SRKeyboardProbabilityMetric; cdecl;
    function touchDownUp: SRKeyboardProbabilityMetric; cdecl;
    function touchUpDown: SRKeyboardProbabilityMetric; cdecl;
    function typingSpeed: Double; cdecl;
    function upErrorDistance: SRKeyboardProbabilityMetric; cdecl;
    function version: NSString; cdecl;
    function width: NSMeasurement; cdecl;
    function wordCountForSentimentCategory(category: SRKeyboardMetricsSentimentCategory): NSInteger; cdecl;
  end;
  TSRKeyboardMetrics = class(TOCGenericImport<SRKeyboardMetricsClass, SRKeyboardMetrics>) end;

  SRKeyboardProbabilityMetricClass = interface(NSObjectClass)
    ['{3171B911-F6C9-46AE-824B-15CA58BFD8CE}']
  end;

  SRKeyboardProbabilityMetric = interface(NSObject)
    ['{635EF556-9842-4C03-BB06-9680004C3C03}']
    function distributionSampleValues: NSArray; cdecl;
  end;
  TSRKeyboardProbabilityMetric = class(TOCGenericImport<SRKeyboardProbabilityMetricClass, SRKeyboardProbabilityMetric>) end;

  SRDeletionRecordClass = interface(NSObjectClass)
    ['{C4EE7599-886F-471F-AE26-7D9E46B1B5B0}']
  end;

  SRDeletionRecord = interface(NSObject)
    ['{63561B98-2B5A-44CC-8B91-E0D38B798E94}']
    function endTime: SRAbsoluteTime; cdecl;
    function reason: SRDeletionReason; cdecl;
    function startTime: SRAbsoluteTime; cdecl;
  end;
  TSRDeletionRecord = class(TOCGenericImport<SRDeletionRecordClass, SRDeletionRecord>) end;

  SRWristDetectionClass = interface(NSObjectClass)
    ['{2594E987-685A-4994-B2D0-170FF1A6FBCA}']
  end;

  SRWristDetection = interface(NSObject)
    ['{688CD3BE-684E-4968-AB47-348ADB286DE8}']
    function crownOrientation: SRCrownOrientation; cdecl;
    function offWristDate: NSDate; cdecl;
    function onWrist: Boolean; cdecl;
    function onWristDate: NSDate; cdecl;
    function wristLocation: SRWristLocation; cdecl;
  end;
  TSRWristDetection = class(TOCGenericImport<SRWristDetectionClass, SRWristDetection>) end;

  SRWristTemperatureClass = interface(NSObjectClass)
    ['{0A6E6EC5-FF59-4DAD-AAE8-6695C42AD522}']
    {class} function new: Pointer; cdecl;
  end;

  SRWristTemperature = interface(NSObject)
    ['{4F4B0B65-DE92-49EC-B849-B86499EBBF10}']
    function condition: SRWristTemperatureCondition; cdecl;
    function errorEstimate: NSMeasurement; cdecl;
    function timestamp: NSDate; cdecl;
    function value: NSMeasurement; cdecl;
  end;
  TSRWristTemperature = class(TOCGenericImport<SRWristTemperatureClass, SRWristTemperature>) end;

  SRWristTemperatureSessionClass = interface(NSObjectClass)
    ['{22354CAF-1774-40E0-BC6F-845882EB91C0}']
    {class} function new: Pointer; cdecl;
  end;

  SRWristTemperatureSession = interface(NSObject)
    ['{85087BEF-BD9D-4EFA-80F1-F72B1444E0A4}']
    function duration: NSTimeInterval; cdecl;
    function startDate: NSDate; cdecl;
    function temperatures: NSEnumerator; cdecl;
    function version: NSString; cdecl;
  end;
  TSRWristTemperatureSession = class(TOCGenericImport<SRWristTemperatureSessionClass, SRWristTemperatureSession>) end;

  SRMediaEventClass = interface(NSObjectClass)
    ['{00DED14D-2928-42D5-9C76-15FE42B0C2B1}']
  end;

  SRMediaEvent = interface(NSObject)
    ['{E8C8093F-8083-4D03-BC78-C85637CA806C}']
    function eventType: SRMediaEventType; cdecl;
    function mediaIdentifier: NSString; cdecl;
  end;
  TSRMediaEvent = class(TOCGenericImport<SRMediaEventClass, SRMediaEvent>) end;

  SRSpeechExpressionClass = interface(NSObjectClass)
    ['{0EFC7E6A-836C-47DA-8745-041C2909AB63}']
    {class} function new: Pointer; cdecl;
  end;

  SRSpeechExpression = interface(NSObject)
    ['{C666B17B-5424-4CA3-B4F5-A74E13516AEB}']
    function activation: Double; cdecl;
    function confidence: Double; cdecl;
    function dominance: Double; cdecl;
    function mood: Double; cdecl;
    function timeRange: CMTimeRange; cdecl;
    function valence: Double; cdecl;
    function version: NSString; cdecl;
  end;
  TSRSpeechExpression = class(TOCGenericImport<SRSpeechExpressionClass, SRSpeechExpression>) end;

  SRAudioLevelClass = interface(NSObjectClass)
    ['{09C577FB-F5BC-499E-A8D0-81C48FB3D8D5}']
    {class} function new: Pointer; cdecl;
  end;

  SRAudioLevel = interface(NSObject)
    ['{815A7761-FA9B-4BAB-ABD1-2A56B502BD73}']
    function loudness: Double; cdecl;
    function timeRange: CMTimeRange; cdecl;
  end;
  TSRAudioLevel = class(TOCGenericImport<SRAudioLevelClass, SRAudioLevel>) end;

  SRSpeechMetricsClass = interface(NSObjectClass)
    ['{1181BB2A-E3B9-417A-809F-4FC3C87C429F}']
    {class} function new: Pointer; cdecl;
  end;

  SRSpeechMetrics = interface(NSObject)
    ['{5B3425C7-2A0F-4057-8232-2912618A8D30}']
    function audioLevel: SRAudioLevel; cdecl;
    function sessionFlags: SRSpeechMetricsSessionFlags; cdecl;
    function sessionIdentifier: NSString; cdecl;
    function soundClassification: SNClassificationResult; cdecl;
    function speechExpression: SRSpeechExpression; cdecl;
    function speechRecognition: SFSpeechRecognitionResult; cdecl;
    function timeSinceAudioStart: NSTimeInterval; cdecl;
    function timestamp: NSDate; cdecl;
  end;
  TSRSpeechMetrics = class(TOCGenericImport<SRSpeechMetricsClass, SRSpeechMetrics>) end;

  SRFaceMetricsExpressionClass = interface(NSObjectClass)
    ['{FC0212B9-FDA4-40AC-8348-EA7FCB220755}']
    {class} function new: Pointer; cdecl;
  end;

  SRFaceMetricsExpression = interface(NSObject)
    ['{89DDC603-9C84-4DA2-A354-F6CBA2824A3A}']
    function identifier: NSString; cdecl;
    function value: Double; cdecl;
  end;
  TSRFaceMetricsExpression = class(TOCGenericImport<SRFaceMetricsExpressionClass, SRFaceMetricsExpression>) end;

  SRFaceMetricsClass = interface(NSObjectClass)
    ['{D621CB7C-B04A-49FF-9532-739C62DBC32A}']
    {class} function new: Pointer; cdecl;
  end;

  SRFaceMetrics = interface(NSObject)
    ['{81B55537-C9E8-4EED-9A7B-D5CEEC79EA50}']
    function context: SRFaceMetricsContext; cdecl;
    function partialFaceExpressions: NSArray; cdecl;
    function sessionIdentifier: NSString; cdecl;
    function version: NSString; cdecl;
    function wholeFaceExpressions: NSArray; cdecl;
  end;
  TSRFaceMetrics = class(TOCGenericImport<SRFaceMetricsClass, SRFaceMetrics>) end;

  SRElectrocardiogramSessionClass = interface(NSObjectClass)
    ['{110C4674-9969-41AB-8118-35DB40C885DF}']
    {class} function new: Pointer; cdecl;
  end;

  SRElectrocardiogramSession = interface(NSObject)
    ['{C4E85D66-6C18-4093-9050-136D280DB7DC}']
    function identifier: NSString; cdecl;
    function sessionGuidance: SRElectrocardiogramSessionGuidance; cdecl;
    function state: SRElectrocardiogramSessionState; cdecl;
  end;
  TSRElectrocardiogramSession = class(TOCGenericImport<SRElectrocardiogramSessionClass, SRElectrocardiogramSession>) end;

  SRElectrocardiogramDataClass = interface(NSObjectClass)
    ['{0F7E5AF0-8B12-4742-B545-953451B99CB7}']
    {class} function new: Pointer; cdecl;
  end;

  SRElectrocardiogramData = interface(NSObject)
    ['{AB982E44-A77F-4DD8-8191-8C70A3BA88C3}']
    function flags: SRElectrocardiogramDataFlags; cdecl;
    function value: NSMeasurement; cdecl;
  end;
  TSRElectrocardiogramData = class(TOCGenericImport<SRElectrocardiogramDataClass, SRElectrocardiogramData>) end;

  SRElectrocardiogramSampleClass = interface(NSObjectClass)
    ['{E47899DC-723A-49F6-9427-A8AC9AD721D4}']
    {class} function new: Pointer; cdecl;
  end;

  SRElectrocardiogramSample = interface(NSObject)
    ['{454BD3E6-33F0-49E3-A73C-F2280ED2259E}']
    function data: NSArray; cdecl;
    function date: NSDate; cdecl;
    function frequency: NSMeasurement; cdecl;
    function lead: SRElectrocardiogramLead; cdecl;
    function session: SRElectrocardiogramSession; cdecl;
  end;
  TSRElectrocardiogramSample = class(TOCGenericImport<SRElectrocardiogramSampleClass, SRElectrocardiogramSample>) end;

  SRPhotoplethysmogramOpticalSampleClass = interface(NSObjectClass)
    ['{C1FABBF6-E7BF-4F95-9DB2-9F74B5EDAD4D}']
    {class} function new: Pointer; cdecl;
  end;

  SRPhotoplethysmogramOpticalSample = interface(NSObject)
    ['{F949A3AF-050A-411A-895C-D781A17829B3}']
    function activePhotodiodeIndexes: NSIndexSet; cdecl;
    function backgroundNoise: NSNumber; cdecl;
    function backgroundNoiseOffset: NSNumber; cdecl;
    function conditions: NSArray; cdecl;
    function effectiveWavelength: NSMeasurement; cdecl;
    function emitter: NSInteger; cdecl;
    function nanosecondsSinceStart: Int64; cdecl;
    function nominalWavelength: NSMeasurement; cdecl;
    function normalizedReflectance: NSNumber; cdecl;
    function pinkNoise: NSNumber; cdecl;
    function samplingFrequency: NSMeasurement; cdecl;
    function signalIdentifier: NSInteger; cdecl;
    function whiteNoise: NSNumber; cdecl;
  end;
  TSRPhotoplethysmogramOpticalSample = class(TOCGenericImport<SRPhotoplethysmogramOpticalSampleClass, SRPhotoplethysmogramOpticalSample>) end;

  SRPhotoplethysmogramAccelerometerSampleClass = interface(NSObjectClass)
    ['{16C01589-7961-42A2-AA98-D06613079149}']
    {class} function new: Pointer; cdecl;
  end;

  SRPhotoplethysmogramAccelerometerSample = interface(NSObject)
    ['{16C2C3AF-6533-43B6-A6FD-81E7C58FEC61}']
    function nanosecondsSinceStart: Int64; cdecl;
    function samplingFrequency: NSMeasurement; cdecl;
    function x: NSMeasurement; cdecl;
    function y: NSMeasurement; cdecl;
    function z: NSMeasurement; cdecl;
  end;
  TSRPhotoplethysmogramAccelerometerSample = class(TOCGenericImport<SRPhotoplethysmogramAccelerometerSampleClass, SRPhotoplethysmogramAccelerometerSample>) end;

  SRPhotoplethysmogramSampleClass = interface(NSObjectClass)
    ['{3A506F98-9FA3-470D-BBAD-B1BBA8F28E01}']
    {class} function new: Pointer; cdecl;
  end;

  SRPhotoplethysmogramSample = interface(NSObject)
    ['{E4E395B2-4A7E-4764-8F3D-7558132E943F}']
    function accelerometerSamples: NSArray; cdecl;
    function nanosecondsSinceStart: Int64; cdecl;
    function opticalSamples: NSArray; cdecl;
    function startDate: NSDate; cdecl;
    function temperature: NSMeasurement; cdecl;
    function usage: NSArray; cdecl;
  end;
  TSRPhotoplethysmogramSample = class(TOCGenericImport<SRPhotoplethysmogramSampleClass, SRPhotoplethysmogramSample>) end;

function SRSensorAmbientLightSensor: SRSensor;
function SRSensorAccelerometer: SRSensor;
function SRSensorRotationRate: SRSensor;
function SRSensorVisits: SRSensor;
function SRSensorPedometerData: SRSensor;
function SRSensorDeviceUsageReport: SRSensor;
function SRSensorMessagesUsageReport: SRSensor;
function SRSensorPhoneUsageReport: SRSensor;
function SRSensorOnWristState: SRSensor;
function SRSensorKeyboardMetrics: SRSensor;
function SRSensorSiriSpeechMetrics: SRSensor;
function SRSensorTelephonySpeechMetrics: SRSensor;
function SRSensorAmbientPressure: SRSensor;
function SRSensorMediaEvents: SRSensor;
function SRSensorWristTemperature: SRSensor;
function SRSensorHeartRate: SRSensor;
function SRSensorFaceMetrics: SRSensor;
function SRSensorOdometer: SRSensor;
function SRSensorElectrocardiogram: SRSensor;
function SRSensorPhotoplethysmogram: SRSensor;
function SRErrorDomain: NSErrorDomain;
function SRDeviceUsageCategoryGames: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryBusiness: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryWeather: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryUtilities: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryTravel: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategorySports: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategorySocialNetworking: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryReference: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryProductivity: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryPhotoAndVideo: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryNews: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryNavigation: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryMusic: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryLifestyle: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryHealthAndFitness: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryFinance: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryEntertainment: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryEducation: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryBooks: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryMedical: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryNewsstand: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryCatalogs: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryKids: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryMiscellaneous: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryFoodAndDrink: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryDeveloperTools: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryGraphicsAndDesign: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryShopping: SRDeviceUsageCategoryKey;
function SRDeviceUsageCategoryStickers: SRDeviceUsageCategoryKey;
function SRPhotoplethysmogramOpticalSampleConditionSignalSaturation: SRPhotoplethysmogramOpticalSampleCondition;
function SRPhotoplethysmogramOpticalSampleConditionUnreliableNoise: SRPhotoplethysmogramOpticalSampleCondition;
function SRPhotoplethysmogramSampleUsageForegroundHeartRate: SRPhotoplethysmogramSampleUsage;
function SRPhotoplethysmogramSampleUsageDeepBreathing: SRPhotoplethysmogramSampleUsage;
function SRPhotoplethysmogramSampleUsageForegroundBloodOxygen: SRPhotoplethysmogramSampleUsage;
function SRPhotoplethysmogramSampleUsageBackgroundSystem: SRPhotoplethysmogramSampleUsage;

const
  libSensorKit = '/System/Library/Frameworks/SensorKit.framework/SensorKit';

function SRAbsoluteTimeGetCurrent: SRAbsoluteTime; cdecl;
  external libSensorKit name _PU + 'SRAbsoluteTimeGetCurrent';

function SRAbsoluteTimeFromContinuousTime(cont: UInt64): SRAbsoluteTime; cdecl;
  external libSensorKit name _PU + 'SRAbsoluteTimeFromContinuousTime';

function SRAbsoluteTimeToCFAbsoluteTime(sr: SRAbsoluteTime): CFAbsoluteTime; cdecl;
  external libSensorKit name _PU + 'SRAbsoluteTimeToCFAbsoluteTime';

function SRAbsoluteTimeFromCFAbsoluteTime(cf: CFAbsoluteTime): SRAbsoluteTime; cdecl;
  external libSensorKit name _PU + 'SRAbsoluteTimeFromCFAbsoluteTime';

implementation

uses
  Posix.Dlfcn;

var
  SensorKitModule: THandle;

function SRSensorAmbientLightSensor: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorAmbientLightSensor');
end;

function SRSensorAccelerometer: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorAccelerometer');
end;

function SRSensorRotationRate: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorRotationRate');
end;

function SRSensorVisits: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorVisits');
end;

function SRSensorPedometerData: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorPedometerData');
end;

function SRSensorDeviceUsageReport: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorDeviceUsageReport');
end;

function SRSensorMessagesUsageReport: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorMessagesUsageReport');
end;

function SRSensorPhoneUsageReport: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorPhoneUsageReport');
end;

function SRSensorOnWristState: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorOnWristState');
end;

function SRSensorKeyboardMetrics: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorKeyboardMetrics');
end;

function SRSensorSiriSpeechMetrics: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorSiriSpeechMetrics');
end;

function SRSensorTelephonySpeechMetrics: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorTelephonySpeechMetrics');
end;

function SRSensorAmbientPressure: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorAmbientPressure');
end;

function SRSensorMediaEvents: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorMediaEvents');
end;

function SRSensorWristTemperature: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorWristTemperature');
end;

function SRSensorHeartRate: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorHeartRate');
end;

function SRSensorFaceMetrics: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorFaceMetrics');
end;

function SRSensorOdometer: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorOdometer');
end;

function SRSensorElectrocardiogram: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorElectrocardiogram');
end;

function SRSensorPhotoplethysmogram: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRSensorPhotoplethysmogram');
end;

function SRErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRErrorDomain');
end;

function SRDeviceUsageCategoryGames: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryGames');
end;

function SRDeviceUsageCategoryBusiness: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryBusiness');
end;

function SRDeviceUsageCategoryWeather: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryWeather');
end;

function SRDeviceUsageCategoryUtilities: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryUtilities');
end;

function SRDeviceUsageCategoryTravel: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryTravel');
end;

function SRDeviceUsageCategorySports: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategorySports');
end;

function SRDeviceUsageCategorySocialNetworking: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategorySocialNetworking');
end;

function SRDeviceUsageCategoryReference: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryReference');
end;

function SRDeviceUsageCategoryProductivity: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryProductivity');
end;

function SRDeviceUsageCategoryPhotoAndVideo: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryPhotoAndVideo');
end;

function SRDeviceUsageCategoryNews: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryNews');
end;

function SRDeviceUsageCategoryNavigation: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryNavigation');
end;

function SRDeviceUsageCategoryMusic: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryMusic');
end;

function SRDeviceUsageCategoryLifestyle: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryLifestyle');
end;

function SRDeviceUsageCategoryHealthAndFitness: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryHealthAndFitness');
end;

function SRDeviceUsageCategoryFinance: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryFinance');
end;

function SRDeviceUsageCategoryEntertainment: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryEntertainment');
end;

function SRDeviceUsageCategoryEducation: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryEducation');
end;

function SRDeviceUsageCategoryBooks: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryBooks');
end;

function SRDeviceUsageCategoryMedical: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryMedical');
end;

function SRDeviceUsageCategoryNewsstand: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryNewsstand');
end;

function SRDeviceUsageCategoryCatalogs: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryCatalogs');
end;

function SRDeviceUsageCategoryKids: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryKids');
end;

function SRDeviceUsageCategoryMiscellaneous: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryMiscellaneous');
end;

function SRDeviceUsageCategoryFoodAndDrink: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryFoodAndDrink');
end;

function SRDeviceUsageCategoryDeveloperTools: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryDeveloperTools');
end;

function SRDeviceUsageCategoryGraphicsAndDesign: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryGraphicsAndDesign');
end;

function SRDeviceUsageCategoryShopping: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryShopping');
end;

function SRDeviceUsageCategoryStickers: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRDeviceUsageCategoryStickers');
end;

function SRPhotoplethysmogramOpticalSampleConditionSignalSaturation: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRPhotoplethysmogramOpticalSampleConditionSignalSaturation');
end;

function SRPhotoplethysmogramOpticalSampleConditionUnreliableNoise: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRPhotoplethysmogramOpticalSampleConditionUnreliableNoise');
end;

function SRPhotoplethysmogramSampleUsageForegroundHeartRate: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRPhotoplethysmogramSampleUsageForegroundHeartRate');
end;

function SRPhotoplethysmogramSampleUsageDeepBreathing: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRPhotoplethysmogramSampleUsageDeepBreathing');
end;

function SRPhotoplethysmogramSampleUsageForegroundBloodOxygen: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRPhotoplethysmogramSampleUsageForegroundBloodOxygen');
end;

function SRPhotoplethysmogramSampleUsageBackgroundSystem: NSString;
begin
  Result := CocoaNSStringConst(libSensorKit, 'SRPhotoplethysmogramSampleUsageBackgroundSystem');
end;

initialization
  SensorKitModule := dlopen(MarshaledAString(libSensorKit), RTLD_LAZY);

finalization
  dlclose(SensorKitModule);

end.