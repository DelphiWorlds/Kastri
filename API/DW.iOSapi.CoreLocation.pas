unit DW.iOSapi.CoreLocation;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.Contacts, DW.iOSapi.Foundation;

const
  kCLErrorLocationUnknown = 0;
  kCLErrorDenied = 1;
  kCLErrorNetwork = 2;
  kCLErrorHeadingFailure = 3;
  kCLErrorRegionMonitoringDenied = 4;
  kCLErrorRegionMonitoringFailure = 5;
  kCLErrorRegionMonitoringSetupDelayed = 6;
  kCLErrorRegionMonitoringResponseDelayed = 7;
  kCLErrorGeocodeFoundNoResult = 8;
  kCLErrorGeocodeFoundPartialResult = 9;
  kCLErrorGeocodeCanceled = 10;
  kCLErrorDeferredFailed = 11;
  kCLErrorDeferredNotUpdatingLocation = 12;
  kCLErrorDeferredAccuracyTooLow = 13;
  kCLErrorDeferredDistanceFiltered = 14;
  kCLErrorDeferredCanceled = 15;
  kCLErrorRangingUnavailable = 16;
  kCLErrorRangingFailure = 17;
  kCLErrorPromptDeclined = 18;
  kCLErrorHistoricalLocationError = 19;
  CLRegionStateUnknown = 0;
  CLRegionStateInside = 1;
  CLRegionStateOutside = 2;
  CLProximityUnknown = 0;
  CLProximityImmediate = 1;
  CLProximityNear = 2;
  CLProximityFar = 3;
  CLLiveUpdateConfigurationDefault = 0;
  CLLiveUpdateConfigurationAutomotiveNavigation = 1;
  CLLiveUpdateConfigurationOtherNavigation = 2;
  CLLiveUpdateConfigurationFitness = 3;
  CLLiveUpdateConfigurationAirborne = 4;
  CLMonitoringStateUnknown = 0;
  CLMonitoringStateSatisfied = 1;
  CLMonitoringStateUnsatisfied = 2;
  CLMonitoringStateUnmonitored = 3;
  CLDeviceOrientationUnknown = 0;
  CLDeviceOrientationPortrait = 1;
  CLDeviceOrientationPortraitUpsideDown = 2;
  CLDeviceOrientationLandscapeLeft = 3;
  CLDeviceOrientationLandscapeRight = 4;
  CLDeviceOrientationFaceUp = 5;
  CLDeviceOrientationFaceDown = 6;
  kCLAuthorizationStatusNotDetermined = 0;
  kCLAuthorizationStatusRestricted = 1;
  kCLAuthorizationStatusDenied = 2;
  kCLAuthorizationStatusAuthorizedAlways = 3;
  kCLAuthorizationStatusAuthorizedWhenInUse = 4;
  kCLAuthorizationStatusAuthorized = kCLAuthorizationStatusAuthorizedAlways;
  CLAccuracyAuthorizationFullAccuracy = 0;
  CLAccuracyAuthorizationReducedAccuracy = 1;
  CLActivityTypeOther = 1;
  CLActivityTypeAutomotiveNavigation = 2;
  CLActivityTypeFitness = 3;
  CLActivityTypeOtherNavigation = 4;
  CLActivityTypeAirborne = 5;
  CLLocationPushServiceErrorUnknown = 0;
  CLLocationPushServiceErrorMissingPushExtension = 1;
  CLLocationPushServiceErrorMissingPushServerEnvironment = 2;
  CLLocationPushServiceErrorMissingEntitlement = 3;
  CLLocationPushServiceErrorUnsupportedPlatform = 4;

type
  CLFloor = interface;
  CLLocationSourceInformation = interface;
  CLLocation = interface;
  CLRegion = interface;
  CLCondition = interface;
  CLCircularRegion = interface;
  CLCircularGeographicCondition = interface;
  CLBeaconIdentityCondition = interface;
  CLBeaconIdentityConstraint = interface;
  CLBeaconRegion = interface;
  CLBeacon = interface;
  CLHeading = interface;
  CLUpdate = interface;
  CLLocationUpdater = interface;
  CLMonitoringEvent = interface;
  CLMonitoringRecord = interface;
  CLMonitorConfiguration = interface;
  CLMonitor = interface;
  CLLocationManager = interface;
  CLVisit = interface;
  CLLocationManagerDelegate = interface;
  CLPlacemark = interface;
  CLGeocoder = interface;
  CLLocationPushServiceExtension = interface;
  CLBackgroundActivitySession = interface;

  PCLLocationCoordinate2D = ^CLLocationCoordinate2D;

  CLError = NSInteger;
  CLLocationDegrees = Double;
  CLLocationAccuracy = Double;
  CLLocationSpeed = Double;
  CLLocationSpeedAccuracy = Double;
  CLLocationDirection = Double;
  CLLocationDirectionAccuracy = Double;

  CLLocationCoordinate2D = record
    latitude: CLLocationDegrees;
    longitude: CLLocationDegrees;
  end;

  CLLocationDistance = Double;
  CLRegionState = NSInteger;
  CLProximity = NSInteger;
  CLBeaconMajorValue = UInt16;
  CLBeaconMinorValue = UInt16;
  CLHeadingComponentValue = Double;
  CLLiveUpdateConfiguration = NSInteger;
  CLMonitoringState = NSInteger;
  CLDeviceOrientation = NSInteger;
  CLAuthorizationStatus = NSInteger;
  CLAccuracyAuthorization = NSInteger;
  CLActivityType = NSInteger;

  CLGeocodeCompletionHandler = procedure(placemarks: NSArray; error: NSError) of object;
  CLLocationPushServiceError = NSInteger;
  TCLLocationUpdaterBlockMethod1 = procedure(update: CLUpdate) of object;
  TCLMonitorConfigurationBlockMethod1 = procedure(monitor: CLMonitor; event: CLMonitoringEvent) of object;
  TCLMonitorConfigurationBlockMethod2 = procedure(param1: CLMonitor; param2: CLMonitoringEvent) of object;
  TCLMonitorBlockMethod1 = procedure(monitor: CLMonitor) of object;
  TCLLocationManagerBlockMethod1 = procedure(param1: NSError) of object;
  TCLLocationManagerBlockMethod2 = procedure(token: NSData; param2: NSError) of object;
  TCLLocationManagerBlockMethod3 = procedure(param1: NSArray; param2: NSError) of object;
  TCLLocationPushServiceExtensionBlockMethod1 = procedure of object;

  CLFloorClass = interface(NSObjectClass)
    ['{0657FC5A-3293-4BFF-8792-E4CCA8309EE0}']
  end;

  CLFloor = interface(NSObject)
    ['{375E6860-098C-4AB7-B615-37F181F24C5E}']
    function level: NSInteger; cdecl;
  end;
  TCLFloor = class(TOCGenericImport<CLFloorClass, CLFloor>) end;

  CLLocationSourceInformationClass = interface(NSObjectClass)
    ['{9F80991E-048A-4904-A07F-FA7DE9274117}']
  end;

  CLLocationSourceInformation = interface(NSObject)
    ['{BDD6D6ED-1751-420D-8E32-F68FF9D92966}']
    function initWithSoftwareSimulationState(isSoftware: Boolean; andExternalAccessoryState: Boolean): Pointer; cdecl;
    function isProducedByAccessory: Boolean; cdecl;
    function isSimulatedBySoftware: Boolean; cdecl;
  end;
  TCLLocationSourceInformation = class(TOCGenericImport<CLLocationSourceInformationClass, CLLocationSourceInformation>) end;

  CLLocationClass = interface(NSObjectClass)
    ['{65D4C865-A808-40B6-96CE-7F2B20075B6A}']
  end;

  CLLocation = interface(NSObject)
    ['{840448C7-EFA8-4050-A253-3840A870A242}']
    function altitude: CLLocationDistance; cdecl;
    function coordinate: CLLocationCoordinate2D; cdecl;
    function course: CLLocationDirection; cdecl;
    function courseAccuracy: CLLocationDirectionAccuracy; cdecl;
    function distanceFromLocation(location: CLLocation): CLLocationDistance; cdecl;
    function ellipsoidalAltitude: CLLocationDistance; cdecl;
    function floor: CLFloor; cdecl;
    function getDistanceFrom(location: CLLocation): CLLocationDistance; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-distanceFromLocation:", ios(2.0, 3.2), macos(10.15, 10.15))
    function horizontalAccuracy: CLLocationAccuracy; cdecl;
    function initWithCoordinate(coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance; horizontalAccuracy: CLLocationAccuracy;
      verticalAccuracy: CLLocationAccuracy; timestamp: NSDate): Pointer; overload; cdecl;
    function initWithCoordinate(coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance; horizontalAccuracy: CLLocationAccuracy;
      verticalAccuracy: CLLocationAccuracy; course: CLLocationDirection; courseAccuracy: CLLocationDirectionAccuracy; speed: CLLocationSpeed;
      speedAccuracy: CLLocationSpeedAccuracy; timestamp: NSDate; sourceInfo: CLLocationSourceInformation): Pointer; overload; cdecl;
    function initWithCoordinate(coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance; horizontalAccuracy: CLLocationAccuracy;
      verticalAccuracy: CLLocationAccuracy; course: CLLocationDirection; courseAccuracy: CLLocationDirectionAccuracy; speed: CLLocationSpeed;
      speedAccuracy: CLLocationSpeedAccuracy; timestamp: NSDate): Pointer; overload; cdecl;
    function initWithCoordinate(coordinate: CLLocationCoordinate2D; altitude: CLLocationDistance; horizontalAccuracy: CLLocationAccuracy;
      verticalAccuracy: CLLocationAccuracy; course: CLLocationDirection; speed: CLLocationSpeed; timestamp: NSDate): Pointer; overload; cdecl;
    function initWithLatitude(latitude: CLLocationDegrees; longitude: CLLocationDegrees): Pointer; cdecl;
    function sourceInformation: CLLocationSourceInformation; cdecl;
    function speed: CLLocationSpeed; cdecl;
    function speedAccuracy: CLLocationSpeedAccuracy; cdecl;
    function timestamp: NSDate; cdecl;
    function verticalAccuracy: CLLocationAccuracy; cdecl;
  end;
  TCLLocation = class(TOCGenericImport<CLLocationClass, CLLocation>) end;

  CLRegionClass = interface(NSObjectClass)
    ['{DCF266C7-7511-447A-B44E-FC6FC2C3BE7E}']
  end;

  CLRegion = interface(NSObject)
    ['{576E04DA-0359-4F09-8EF7-FF93D2709DD8}']
    function center: CLLocationCoordinate2D; cdecl; // API_DEPRECATED("Please see CLCircularRegion", ios(4.0, 7.0), macos(10.7, 10.10))
    function containsCoordinate(coordinate: CLLocationCoordinate2D): Boolean; cdecl; // API_DEPRECATED("Please see CLCircularRegion", ios(4.0, 7.0), macos(10.7, 10.10))
    function identifier: NSString; cdecl;
    function initCircularRegionWithCenter(center: CLLocationCoordinate2D; radius: CLLocationDistance; identifier: NSString): Pointer; cdecl; // API_DEPRECATED("Please see CLCircularRegion", ios(4.0, 7.0), macos(10.7, 10.10))
    function notifyOnEntry: Boolean; cdecl;
    function notifyOnExit: Boolean; cdecl;
    function radius: CLLocationDistance; cdecl; // API_DEPRECATED("Please see CLCircularRegion", ios(4.0, 7.0), macos(10.7, 10.10))
    procedure setNotifyOnEntry(notifyOnEntry: Boolean); cdecl;
    procedure setNotifyOnExit(notifyOnExit: Boolean); cdecl;
  end;
  TCLRegion = class(TOCGenericImport<CLRegionClass, CLRegion>) end;

  CLConditionClass = interface(NSObjectClass)
    ['{6F061FDD-537C-45CE-93CC-4199F7F6048B}']
    {class} function new: Pointer; cdecl;
  end;

  CLCondition = interface(NSObject)
    ['{F5FCA8DB-F62C-4674-8482-0590273A6710}']
  end;
  TCLCondition = class(TOCGenericImport<CLConditionClass, CLCondition>) end;

  CLCircularRegionClass = interface(CLRegionClass)
    ['{E343AD2C-2138-44E3-A92E-7DB4E2F1E55E}']
  end;

  CLCircularRegion = interface(CLRegion)
    ['{41708BE7-1DB8-41A3-B6A9-8E9205F38031}']
    function center: CLLocationCoordinate2D; cdecl;
    function containsCoordinate(coordinate: CLLocationCoordinate2D): Boolean; cdecl;
    function initWithCenter(center: CLLocationCoordinate2D; radius: CLLocationDistance; identifier: NSString): Pointer; cdecl;
    function radius: CLLocationDistance; cdecl;
  end;
  TCLCircularRegion = class(TOCGenericImport<CLCircularRegionClass, CLCircularRegion>) end;

  CLCircularGeographicConditionClass = interface(CLConditionClass)
    ['{5F3F6DB0-0B99-4E50-8856-8D27953CB030}']
  end;

  CLCircularGeographicCondition = interface(CLCondition)
    ['{79007213-5023-403D-A12C-0E981E3011DB}']
    function center: CLLocationCoordinate2D; cdecl;
    function initWithCenter(center: CLLocationCoordinate2D; radius: CLLocationDistance): Pointer; cdecl;
    function radius: CLLocationDistance; cdecl;
  end;
  TCLCircularGeographicCondition = class(TOCGenericImport<CLCircularGeographicConditionClass, CLCircularGeographicCondition>) end;

  CLBeaconIdentityConditionClass = interface(CLConditionClass)
    ['{F4743FBA-6957-4DDE-927C-EA5DA49B65C9}']
  end;

  CLBeaconIdentityCondition = interface(CLCondition)
    ['{0220A668-389B-451F-9C9A-83782C0340CC}']
    function initWithUUID(uuid: NSUUID; major: CLBeaconMajorValue; minor: CLBeaconMinorValue): Pointer; overload; cdecl;
    function initWithUUID(uuid: NSUUID; major: CLBeaconMajorValue): Pointer; overload; cdecl;
    function initWithUUID(uuid: NSUUID): Pointer; overload; cdecl;
    function major: NSNumber; cdecl;
    function minor: NSNumber; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCLBeaconIdentityCondition = class(TOCGenericImport<CLBeaconIdentityConditionClass, CLBeaconIdentityCondition>) end;

  CLBeaconIdentityConstraintClass = interface(CLBeaconIdentityConditionClass)
    ['{D4C1BD13-2270-4227-BCAE-A4C29011643C}']
  end;

  CLBeaconIdentityConstraint = interface(CLBeaconIdentityCondition)
    ['{BD5B1171-0813-4789-8D0C-554B6FA61061}']
  end;
  TCLBeaconIdentityConstraint = class(TOCGenericImport<CLBeaconIdentityConstraintClass, CLBeaconIdentityConstraint>) end;

  CLBeaconRegionClass = interface(CLRegionClass)
    ['{5BE4B1E3-1726-49BF-9FB7-C3344AB1639B}']
  end;

  CLBeaconRegion = interface(CLRegion)
    ['{FC84E492-5B61-4AB1-8354-5B462E90709F}']
    function beaconIdentityConstraint: CLBeaconIdentityConstraint; cdecl;
    function initWithBeaconIdentityConstraint(beaconIdentityConstraint: CLBeaconIdentityConstraint; identifier: NSString): Pointer; cdecl;
    function initWithProximityUUID(proximityUUID: NSUUID; major: CLBeaconMajorValue; minor: CLBeaconMinorValue;
      identifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithUUID:major:identifier:", ios(7.0, 13.0), macos(10.15, 10.15))
    function initWithProximityUUID(proximityUUID: NSUUID; major: CLBeaconMajorValue; identifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithUUID:major:identifier:", ios(7.0, 13.0), macos(10.15, 10.15))
    function initWithProximityUUID(proximityUUID: NSUUID; identifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithUUID:identifier:", ios(7.0, 13.0), macos(10.15, 10.15))
    function initWithUUID(uuid: NSUUID; identifier: NSString): Pointer; overload; cdecl;
    function initWithUUID(uuid: NSUUID; major: CLBeaconMajorValue; minor: CLBeaconMinorValue; identifier: NSString): Pointer; overload; cdecl;
    function initWithUUID(uuid: NSUUID; major: CLBeaconMajorValue; identifier: NSString): Pointer; overload; cdecl;
    function major: NSNumber; cdecl;
    function minor: NSNumber; cdecl;
    function notifyEntryStateOnDisplay: Boolean; cdecl;
    function peripheralDataWithMeasuredPower(measuredPower: NSNumber): NSMutableDictionary; cdecl;
    function proximityUUID: NSUUID; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-UUID", ios(7.0, 13.0), macos(10.15, 10.15))
    procedure setNotifyEntryStateOnDisplay(notifyEntryStateOnDisplay: Boolean); cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCLBeaconRegion = class(TOCGenericImport<CLBeaconRegionClass, CLBeaconRegion>) end;

  CLBeaconClass = interface(NSObjectClass)
    ['{800135CF-B3B6-4844-B625-60B13B53D6A1}']
  end;

  CLBeacon = interface(NSObject)
    ['{D9AF5647-B4E0-4FAC-A0A8-DCF2B7948E67}']
    function accuracy: CLLocationAccuracy; cdecl;
    function major: NSNumber; cdecl;
    function minor: NSNumber; cdecl;
    function proximity: CLProximity; cdecl;
    function proximityUUID: NSUUID; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-UUID", ios(7.0, 13.0), macos(10.15, 10.15))
    function rssi: NSInteger; cdecl;
    function timestamp: NSDate; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCLBeacon = class(TOCGenericImport<CLBeaconClass, CLBeacon>) end;

  CLHeadingClass = interface(NSObjectClass)
    ['{FFA736D9-0B04-45E1-B662-D8394799E6A4}']
  end;

  CLHeading = interface(NSObject)
    ['{CEC682D1-69A2-49A2-B887-87F0FCD43A1A}']
    function headingAccuracy: CLLocationDirection; cdecl;
    function magneticHeading: CLLocationDirection; cdecl;
    function timestamp: NSDate; cdecl;
    function trueHeading: CLLocationDirection; cdecl;
    function x: CLHeadingComponentValue; cdecl;
    function y: CLHeadingComponentValue; cdecl;
    function z: CLHeadingComponentValue; cdecl;
  end;
  TCLHeading = class(TOCGenericImport<CLHeadingClass, CLHeading>) end;

  CLUpdateClass = interface(NSObjectClass)
    ['{F297A44E-0C9D-413E-8DA4-25354E1DDE34}']
  end;

  CLUpdate = interface(NSObject)
    ['{2F03EBF3-CBF3-4FC5-818E-DC8EF0249A05}']
    function isStationary: Boolean; cdecl;
    function location: CLLocation; cdecl;
  end;
  TCLUpdate = class(TOCGenericImport<CLUpdateClass, CLUpdate>) end;

  CLLocationUpdaterClass = interface(NSObjectClass)
    ['{01EC1172-4952-4650-B7B4-24A3C949D099}']
    {class} function liveUpdaterWithConfiguration(configuration: CLLiveUpdateConfiguration; queue: dispatch_queue_t;
      handler: TCLLocationUpdaterBlockMethod1): Pointer; cdecl;
    {class} function liveUpdaterWithQueue(queue: dispatch_queue_t; handler: TCLLocationUpdaterBlockMethod1): Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CLLocationUpdater = interface(NSObject)
    ['{2E49909A-EDCF-4557-984C-24740E89733D}']
    procedure invalidate; cdecl;
    procedure pause; cdecl;
    procedure resume; cdecl;
  end;
  TCLLocationUpdater = class(TOCGenericImport<CLLocationUpdaterClass, CLLocationUpdater>) end;

  CLMonitoringEventClass = interface(NSObjectClass)
    ['{7EBEF135-72C9-470E-89AF-6DEAD822D057}']
    {class} function new: Pointer; cdecl;
  end;

  CLMonitoringEvent = interface(NSObject)
    ['{5267B7F3-32BD-4A2F-BE80-45816D71DEE3}']
    function date: NSDate; cdecl;
    function identifier: NSString; cdecl;
    function refinement: CLCondition; cdecl;
    function state: CLMonitoringState; cdecl;
  end;
  TCLMonitoringEvent = class(TOCGenericImport<CLMonitoringEventClass, CLMonitoringEvent>) end;

  CLMonitoringRecordClass = interface(NSObjectClass)
    ['{BD824C9A-90E8-419A-BF22-91F5C7E8866B}']
    {class} function new: Pointer; cdecl;
  end;

  CLMonitoringRecord = interface(NSObject)
    ['{37963C3B-DBB1-417C-95E9-08F093110168}']
    function condition: CLCondition; cdecl;
    function lastEvent: CLMonitoringEvent; cdecl;
  end;
  TCLMonitoringRecord = class(TOCGenericImport<CLMonitoringRecordClass, CLMonitoringRecord>) end;

  CLMonitorConfigurationClass = interface(NSObjectClass)
    ['{99B5C07E-9957-4527-AC2F-024358E5D2EF}']
    {class} function configWithMonitorName(name: NSString; queue: dispatch_queue_t;
      eventHandler: TCLMonitorConfigurationBlockMethod1): CLMonitorConfiguration; cdecl;
  end;

  CLMonitorConfiguration = interface(NSObject)
    ['{136401D5-322E-4ACD-A710-F590A17F9A20}']
    function eventHandler: TCLMonitorConfigurationBlockMethod2; cdecl;
    function name: NSString; cdecl;
    function queue: dispatch_queue_t; cdecl;
  end;
  TCLMonitorConfiguration = class(TOCGenericImport<CLMonitorConfigurationClass, CLMonitorConfiguration>) end;

  CLMonitorClass = interface(NSObjectClass)
    ['{C7BB9E7C-F7E7-46E8-8968-6D82B835C33F}']
    {class} function new: Pointer; cdecl;
    {class} procedure requestMonitorWithConfiguration(config: CLMonitorConfiguration; completion: TCLMonitorBlockMethod1); cdecl;
  end;

  CLMonitor = interface(NSObject)
    ['{5092811D-B868-4CE0-9077-4D968DC8218E}']
    procedure addConditionForMonitoring(condition: CLCondition; identifier: NSString); overload; cdecl;
    procedure addConditionForMonitoring(condition: CLCondition; identifier: NSString; assumedState: CLMonitoringState); overload; cdecl;
    function monitoredIdentifiers: NSArray; cdecl;
    function monitoringRecordForIdentifier(identifier: NSString): CLMonitoringRecord; cdecl;
    function name: NSString; cdecl;
    procedure removeConditionFromMonitoringWithIdentifier(identifier: NSString); cdecl;
  end;
  TCLMonitor = class(TOCGenericImport<CLMonitorClass, CLMonitor>) end;

  CLLocationManagerClass = interface(NSObjectClass)
    ['{DD9A4DD9-89C2-4C2F-A3A8-51B6F8B2CF0B}']
    {class} function authorizationStatus: CLAuthorizationStatus; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-authorizationStatus", ios(4.2, 14.0), macos(10.7, 11.0), watchos(1.0, 7.0), tvos(9.0, 14.0))
    {class} function deferredLocationUpdatesAvailable: Boolean; cdecl; // API_DEPRECATED("You can remove calls to this method", ios(6.0, 13.0), macos(10.9, 10.15))
    {class} function headingAvailable: Boolean; cdecl;
    {class} function isMonitoringAvailableForClass(regionClass: Pointer): Boolean; cdecl;
    {class} function isRangingAvailable: Boolean; cdecl;
    {class} function locationServicesEnabled: Boolean; cdecl;
    {class} function regionMonitoringAvailable: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+isMonitoringAvailableForClass:", ios(4.0, 7.0), macos(10.8, 10.10))
    {class} function regionMonitoringEnabled: Boolean; cdecl; // API_DEPRECATED("Use +isMonitoringAvailableForClass: and -authorizationStatus instead", ios(4.0, 6.0), macos(10.8, 10.10))
    {class} function significantLocationChangeMonitoringAvailable: Boolean; cdecl;
  end;

  CLLocationManager = interface(NSObject)
    ['{AB64F8AC-B72B-408C-8C34-197167F893D1}']
    function accuracyAuthorization: CLAccuracyAuthorization; cdecl;
    function activityType: CLActivityType; cdecl;
    procedure allowDeferredLocationUpdatesUntilTraveled(distance: CLLocationDistance; timeout: NSTimeInterval); cdecl; // API_DEPRECATED("You can remove calls to this method", ios(6.0, 13.0), macos(10.15, 10.15))
    function allowsBackgroundLocationUpdates: Boolean; cdecl;
    function authorizationStatus: CLAuthorizationStatus; cdecl;
    function delegate: Pointer; cdecl;
    function desiredAccuracy: CLLocationAccuracy; cdecl;
    procedure disallowDeferredLocationUpdates; cdecl; // API_DEPRECATED("You can remove calls to this method", ios(6.0, 13.0), macos(10.15, 10.15))
    procedure dismissHeadingCalibrationDisplay; cdecl;
    function distanceFilter: CLLocationDistance; cdecl;
    function heading: CLHeading; cdecl;
    function headingAvailable: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+headingAvailable", ios(3.0, 4.0), macos(10.15, 10.15))
    function headingFilter: CLLocationDegrees; cdecl;
    function headingOrientation: CLDeviceOrientation; cdecl;
    function isAuthorizedForWidgetUpdates: Boolean; cdecl;
    function location: CLLocation; cdecl;
    function locationServicesEnabled: Boolean; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+locationServicesEnabled", ios(2.0, 4.0), macos(10.15, 10.15))
    function maximumRegionMonitoringDistance: CLLocationDistance; cdecl;
    function monitoredRegions: NSSet; cdecl;
    function pausesLocationUpdatesAutomatically: Boolean; cdecl;
    function purpose: NSString; cdecl; // API_DEPRECATED("Set the purpose string in Info.plist using key NSLocationUsageDescription", ios(3.2, 6.0), macos(10.7, 11.0))
    function rangedBeaconConstraints: NSSet; cdecl;
    function rangedRegions: NSSet; cdecl; // API_DEPRECATED("Use -rangedBeaconConstraints", ios(7.0, 13.0), macos(10.15, 10.15), macCatalyst(14.0, 14.0))
    procedure requestAlwaysAuthorization; cdecl;
    procedure requestHistoricalLocationsWithPurposeKey(purposeKey: NSString; sampleCount: NSInteger;
      completionHandler: TCLLocationManagerBlockMethod3); cdecl;
    procedure requestLocation; cdecl;
    procedure requestStateForRegion(region: CLRegion); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use CLMonitor to track and query the state for monitored constraints", ios(5.0, API_TO_BE_DEPRECATED), macos(10.8, API_TO_BE_DEPRECATED))
    procedure requestTemporaryFullAccuracyAuthorizationWithPurposeKey(purposeKey: NSString;
      completion: TCLLocationManagerBlockMethod1); overload; cdecl;
    procedure requestTemporaryFullAccuracyAuthorizationWithPurposeKey(purposeKey: NSString); overload; cdecl;
    procedure requestWhenInUseAuthorization; cdecl;
    procedure setActivityType(activityType: CLActivityType); cdecl;
    procedure setAllowsBackgroundLocationUpdates(allowsBackgroundLocationUpdates: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDesiredAccuracy(desiredAccuracy: CLLocationAccuracy); cdecl;
    procedure setDistanceFilter(distanceFilter: CLLocationDistance); cdecl;
    procedure setHeadingFilter(headingFilter: CLLocationDegrees); cdecl;
    procedure setHeadingOrientation(headingOrientation: CLDeviceOrientation); cdecl;
    procedure setPausesLocationUpdatesAutomatically(pausesLocationUpdatesAutomatically: Boolean); cdecl;
    procedure setPurpose(purpose: NSString); cdecl; // API_DEPRECATED("Set the purpose string in Info.plist using key NSLocationUsageDescription", ios(3.2, 6.0), macos(10.7, 11.0))
    procedure setShowsBackgroundLocationIndicator(showsBackgroundLocationIndicator: Boolean); cdecl;
    function showsBackgroundLocationIndicator: Boolean; cdecl;
    procedure startMonitoringForRegion(region: CLRegion; desiredAccuracy: CLLocationAccuracy); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-startMonitoringForRegion:", ios(4.0, 6.0), macos(10.15, 10.15))
    procedure startMonitoringForRegion(region: CLRegion); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use CLMonitor to start or stop monitoring constraint", ios(5.0, API_TO_BE_DEPRECATED), macos(10.8, API_TO_BE_DEPRECATED))
    procedure startMonitoringLocationPushesWithCompletion(completion: TCLLocationManagerBlockMethod2); cdecl;
    procedure startMonitoringSignificantLocationChanges; cdecl;
    procedure startMonitoringVisits; cdecl;
    procedure startRangingBeaconsInRegion(region: CLBeaconRegion); cdecl; // API_DEPRECATED("Use -startRangingBeaconsSatisfyingConstraint:", ios(7.0, 13.0), macos(11.0, 11.0), macCatalyst(14.0, 14.0))
    procedure startRangingBeaconsSatisfyingConstraint(constraint: CLBeaconIdentityConstraint); cdecl;
    procedure startUpdatingHeading; cdecl;
    procedure startUpdatingLocation; cdecl;
    procedure stopMonitoringForRegion(region: CLRegion); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use CLMonitor to start or stop monitoring constraint", ios(5.0, API_TO_BE_DEPRECATED), macos(10.8, API_TO_BE_DEPRECATED))
    procedure stopMonitoringLocationPushes; cdecl;
    procedure stopMonitoringSignificantLocationChanges; cdecl;
    procedure stopMonitoringVisits; cdecl;
    procedure stopRangingBeaconsInRegion(region: CLBeaconRegion); cdecl; // API_DEPRECATED("Use -stopRangingBeaconsSatisfyingConstraint:", ios(7.0, 13.0), macos(11.0, 11.0), macCatalyst(14.0, 14.0))
    procedure stopRangingBeaconsSatisfyingConstraint(constraint: CLBeaconIdentityConstraint); cdecl;
    procedure stopUpdatingHeading; cdecl;
    procedure stopUpdatingLocation; cdecl;
  end;
  TCLLocationManager = class(TOCGenericImport<CLLocationManagerClass, CLLocationManager>) end;

  CLVisitClass = interface(NSObjectClass)
    ['{47D4928B-0727-4FB3-A4AE-AF304D657CD2}']
  end;

  CLVisit = interface(NSObject)
    ['{52D45751-8460-44EB-9EAA-C873BCA41C31}']
    function arrivalDate: NSDate; cdecl;
    function coordinate: CLLocationCoordinate2D; cdecl;
    function departureDate: NSDate; cdecl;
    function horizontalAccuracy: CLLocationAccuracy; cdecl;
  end;
  TCLVisit = class(TOCGenericImport<CLVisitClass, CLVisit>) end;

  CLLocationManagerDelegate = interface(IObjectiveC)
    ['{3B425CD5-830C-4A92-BBAB-11CD759833A9}']
    procedure locationManager(manager: CLLocationManager; didEnterRegion: CLRegion); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didFailRangingBeaconsForConstraint: CLBeaconIdentityConstraint;
      error: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didRangeBeacons: NSArray; satisfyingConstraint: CLBeaconIdentityConstraint); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didFailWithError: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didVisit: CLVisit); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didChangeAuthorizationStatus: CLAuthorizationStatus); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-locationManagerDidChangeAuthorization:", ios(4.2, 14.0), macos(10.7, 11.0), watchos(1.0, 7.0), tvos(9.0, 14.0))
    procedure locationManager(manager: CLLocationManager; monitoringDidFailForRegion: CLRegion; withError: NSError); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateHeading: CLHeading); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateLocations: NSArray); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didUpdateToLocation: CLLocation; fromLocation: CLLocation); overload; cdecl; // API_DEPRECATED("Implement -locationManager:didUpdateLocations: instead", ios(2.0, 6.0))
    procedure locationManager(manager: CLLocationManager; didDetermineState: CLRegionState; forRegion: CLRegion); overload; cdecl;
    procedure locationManager(manager: CLLocationManager; didRangeBeacons: NSArray; inRegion: CLBeaconRegion); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use locationManager:didRangeBeacons:satisfyingConstraint:", ios(7.0, 13.0), macos(10.15, 10.15))
    procedure locationManager(manager: CLLocationManager; rangingBeaconsDidFailForRegion: CLBeaconRegion; withError: NSError); overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("Use locationManager:didFailRangingBeaconsForConstraint:error:", ios(7.0, 13.0), macos(10.15, 10.15))
    procedure locationManagerDidChangeAuthorization(manager: CLLocationManager); cdecl;
    [MethodName('locationManager:didExitRegion:')]
    procedure locationManagerDidExitRegion(manager: CLLocationManager; didExitRegion: CLRegion); cdecl;
    [MethodName('locationManager:didFinishDeferredUpdatesWithError:')]
    procedure locationManagerDidFinishDeferredUpdatesWithError(manager: CLLocationManager; didFinishDeferredUpdatesWithError: NSError); cdecl;
    procedure locationManagerDidPauseLocationUpdates(manager: CLLocationManager); cdecl;
    procedure locationManagerDidResumeLocationUpdates(manager: CLLocationManager); cdecl;
    [MethodName('locationManager:didStartMonitoringForRegion:')]
    procedure locationManagerDidStartMonitoringForRegion(manager: CLLocationManager; didStartMonitoringForRegion: CLRegion); cdecl;
    function locationManagerShouldDisplayHeadingCalibration(manager: CLLocationManager): Boolean; cdecl;
  end;

  CLPlacemarkClass = interface(NSObjectClass)
    ['{7E070AE9-F81C-47B2-AABD-EB3384A941C7}']
    {class} function new: Pointer; cdecl;
  end;

  CLPlacemark = interface(NSObject)
    ['{3EEBCC03-7C5C-4733-AF73-FE7C46201505}']
    function addressDictionary: NSDictionary; cdecl; // API_DEPRECATED("Use @properties", macos(10.8, 10.13), ios(5.0, 11.0), watchos(1.0, 4.0))
    function administrativeArea: NSString; cdecl;
    function areasOfInterest: NSArray; cdecl;
    function country: NSString; cdecl;
    function initWithPlacemark(placemark: CLPlacemark): Pointer; cdecl;
    function inlandWater: NSString; cdecl;
    function ISOcountryCode: NSString; cdecl;
    function locality: NSString; cdecl;
    function location: CLLocation; cdecl;
    function name: NSString; cdecl;
    function ocean: NSString; cdecl;
    function postalAddress: CNPostalAddress; cdecl;
    function postalCode: NSString; cdecl;
    function region: CLRegion; cdecl;
    function subAdministrativeArea: NSString; cdecl;
    function subLocality: NSString; cdecl;
    function subThoroughfare: NSString; cdecl;
    function thoroughfare: NSString; cdecl;
    function timeZone: NSTimeZone; cdecl;
  end;
  TCLPlacemark = class(TOCGenericImport<CLPlacemarkClass, CLPlacemark>) end;

  CLGeocoderClass = interface(NSObjectClass)
    ['{1CD690D5-9D87-4073-B541-970BB2370CA1}']
  end;

  CLGeocoder = interface(NSObject)
    ['{0A1AA614-89F7-4F13-B8C7-5D62F78363D7}']
    procedure cancelGeocode; cdecl;
    procedure geocodeAddressDictionary(addressDictionary: NSDictionary; completionHandler: CLGeocodeCompletionHandler); cdecl; // API_DEPRECATED("Use -geocodePostalAddress:completionHandler:", macos(10.8, 10.13), ios(5.0, 11.0), watchos(1.0, 4.0))
    procedure geocodeAddressString(addressString: NSString; inRegion: CLRegion; preferredLocale: NSLocale;
      completionHandler: CLGeocodeCompletionHandler); overload; cdecl;
    procedure geocodeAddressString(addressString: NSString; completionHandler: CLGeocodeCompletionHandler); overload; cdecl;
    procedure geocodeAddressString(addressString: NSString; inRegion: CLRegion; completionHandler: CLGeocodeCompletionHandler); overload; cdecl;
    procedure geocodePostalAddress(postalAddress: CNPostalAddress; completionHandler: CLGeocodeCompletionHandler); overload; cdecl;
    procedure geocodePostalAddress(postalAddress: CNPostalAddress; preferredLocale: NSLocale;
      completionHandler: CLGeocodeCompletionHandler); overload; cdecl;
    function isGeocoding: Boolean; cdecl;
    procedure reverseGeocodeLocation(location: CLLocation; completionHandler: CLGeocodeCompletionHandler); overload; cdecl;
    procedure reverseGeocodeLocation(location: CLLocation; preferredLocale: NSLocale; completionHandler: CLGeocodeCompletionHandler); overload; cdecl;
  end;
  TCLGeocoder = class(TOCGenericImport<CLGeocoderClass, CLGeocoder>) end;

  CLLocationPushServiceExtension = interface(IObjectiveC)
    ['{767A90C8-9C98-4EE9-9C72-A41AB938FC44}']
    procedure didReceiveLocationPushPayload(payload: NSDictionary; completion: Pointer); cdecl;
    procedure serviceExtensionWillTerminate; cdecl;
  end;

  CLBackgroundActivitySessionClass = interface(NSObjectClass)
    ['{5E43990E-F162-4D2D-B835-ADD680FC02CA}']
    {class} function backgroundActivitySession: Pointer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CLBackgroundActivitySession = interface(NSObject)
    ['{52607FDF-615E-455C-8210-BEB9CB3522B0}']
    procedure invalidate; cdecl;
  end;
  TCLBackgroundActivitySession = class(TOCGenericImport<CLBackgroundActivitySessionClass, CLBackgroundActivitySession>) end;

function kCLErrorDomain: NSString;
function kCLErrorUserInfoAlternateRegionKey: NSString;
function kCLDistanceFilterNone: CLLocationDistance;
function kCLLocationAccuracyBestForNavigation: CLLocationAccuracy;
function kCLLocationAccuracyBest: CLLocationAccuracy;
function kCLLocationAccuracyNearestTenMeters: CLLocationAccuracy;
function kCLLocationAccuracyHundredMeters: CLLocationAccuracy;
function kCLLocationAccuracyKilometer: CLLocationAccuracy;
function kCLLocationAccuracyThreeKilometers: CLLocationAccuracy;
function kCLLocationAccuracyReduced: CLLocationAccuracy;
function CLLocationDistanceMax: CLLocationDistance;
function CLTimeIntervalMax: NSTimeInterval;
function kCLHeadingFilterNone: CLLocationDegrees;
function CLLocationPushServiceErrorDomain: NSErrorDomain;

const
  libCoreLocation = '/System/Library/Frameworks/CoreLocation.framework/CoreLocation';

function CLLocationCoordinate2DIsValid(coord: CLLocationCoordinate2D): Boolean; cdecl;
  external libCoreLocation name _PU + 'CLLocationCoordinate2DIsValid';

function CLLocationCoordinate2DMake(latitude: CLLocationDegrees; longitude: CLLocationDegrees): CLLocationCoordinate2D; cdecl;
  external libCoreLocation name _PU + 'CLLocationCoordinate2DMake';

implementation

uses
  Posix.Dlfcn;

var
  CoreLocationModule: THandle;

function kCLErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCoreLocation, 'kCLErrorDomain');
end;

function kCLErrorUserInfoAlternateRegionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreLocation, 'kCLErrorUserInfoAlternateRegionKey');
end;

function kCLDistanceFilterNone: CLLocationDistance;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLDistanceFilterNone');
end;

function kCLLocationAccuracyBestForNavigation: CLLocationAccuracy;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLLocationAccuracyBestForNavigation');
end;

function kCLLocationAccuracyBest: CLLocationAccuracy;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLLocationAccuracyBest');
end;

function kCLLocationAccuracyNearestTenMeters: CLLocationAccuracy;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLLocationAccuracyNearestTenMeters');
end;

function kCLLocationAccuracyHundredMeters: CLLocationAccuracy;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLLocationAccuracyHundredMeters');
end;

function kCLLocationAccuracyKilometer: CLLocationAccuracy;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLLocationAccuracyKilometer');
end;

function kCLLocationAccuracyThreeKilometers: CLLocationAccuracy;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLLocationAccuracyThreeKilometers');
end;

function kCLLocationAccuracyReduced: CLLocationAccuracy;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLLocationAccuracyReduced');
end;

function CLLocationDistanceMax: CLLocationDistance;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'CLLocationDistanceMax');
end;

function CLTimeIntervalMax: NSTimeInterval;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'CLTimeIntervalMax');
end;

function kCLHeadingFilterNone: CLLocationDegrees;
begin
  Result := CocoaDoubleConst(libCoreLocation, 'kCLHeadingFilterNone');
end;

function CLLocationPushServiceErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libCoreLocation, 'CLLocationPushServiceErrorDomain');
end;

initialization
  CoreLocationModule := dlopen(MarshaledAString(libCoreLocation), RTLD_LAZY);

finalization
  dlclose(CoreLocationModule);

end.