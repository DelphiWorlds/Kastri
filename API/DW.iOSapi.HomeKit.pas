unit DW.iOSapi.HomeKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreLocation, iOSapi.UIKit;

const
  HMHomeManagerAuthorizationStatusDetermined = 1;
  HMHomeManagerAuthorizationStatusRestricted = 2;
  HMHomeManagerAuthorizationStatusAuthorized = 4;
  HMHomeHubStateNotAvailable = 0;
  HMHomeHubStateConnected = 1;
  HMHomeHubStateDisconnected = 2;
  HMCharacteristicValueLockMechanismLastKnownActionSecuredUsingPhysicalMovementInterior = 0;
  HMCharacteristicValueLockMechanismLastKnownActionUnsecuredUsingPhysicalMovementInterior = 1;
  HMCharacteristicValueLockMechanismLastKnownActionSecuredUsingPhysicalMovementExterior = 2;
  HMCharacteristicValueLockMechanismLastKnownActionUnsecuredUsingPhysicalMovementExterior = 3;
  HMCharacteristicValueLockMechanismLastKnownActionSecuredWithKeypad = 4;
  HMCharacteristicValueLockMechanismLastKnownActionUnsecuredWithKeypad = 5;
  HMCharacteristicValueLockMechanismLastKnownActionSecuredRemotely = 6;
  HMCharacteristicValueLockMechanismLastKnownActionUnsecuredRemotely = 7;
  HMCharacteristicValueLockMechanismLastKnownActionSecuredWithAutomaticSecureTimeout = 8;
  HMCharacteristicValueLockMechanismLastKnownActionSecuredUsingPhysicalMovement = 9;
  HMCharacteristicValueLockMechanismLastKnownActionUnsecuredUsingPhysicalMovement = 10;
  HMCharacteristicValueAirParticulateSize2_5 = 0;
  HMCharacteristicValueAirParticulateSize10 = 1;
  HMCharacteristicValueAirQualityUnknown = 0;
  HMCharacteristicValueAirQualityExcellent = 1;
  HMCharacteristicValueAirQualityGood = 2;
  HMCharacteristicValueAirQualityFair = 3;
  HMCharacteristicValueAirQualityInferior = 4;
  HMCharacteristicValueAirQualityPoor = 5;
  HMCharacteristicValuePositionStateClosing = 0;
  HMCharacteristicValuePositionStateOpening = 1;
  HMCharacteristicValuePositionStateStopped = 2;
  HMCharacteristicValueCurrentSecuritySystemStateStayArm = 0;
  HMCharacteristicValueCurrentSecuritySystemStateAwayArm = 1;
  HMCharacteristicValueCurrentSecuritySystemStateNightArm = 2;
  HMCharacteristicValueCurrentSecuritySystemStateDisarmed = 3;
  HMCharacteristicValueCurrentSecuritySystemStateTriggered = 4;
  HMCharacteristicValueTargetSecuritySystemStateStayArm = 0;
  HMCharacteristicValueTargetSecuritySystemStateAwayArm = 1;
  HMCharacteristicValueTargetSecuritySystemStateNightArm = 2;
  HMCharacteristicValueTargetSecuritySystemStateDisarm = 3;
  HMCharacteristicValueJammedStatusNone = 0;
  HMCharacteristicValueJammedStatusJammed = 1;
  HMCharacteristicValueTamperedStatusNone = 0;
  HMCharacteristicValueTamperedStatusTampered = 1;
  HMCharacteristicValueLeakStatusNone = 0;
  HMCharacteristicValueLeakStatusDetected = 1;
  HMCharacteristicValueContactStateDetected = 0;
  HMCharacteristicValueContactStateNone = 1;
  HMCharacteristicValueStatusFaultNoFault = 0;
  HMCharacteristicValueStatusFaultGeneralFault = 1;
  HMCharacteristicValueCarbonMonoxideDetectionStatusNotDetected = 0;
  HMCharacteristicValueCarbonMonoxideDetectionStatusDetected = 1;
  HMCharacteristicValueCarbonDioxideDetectionStatusNotDetected = 0;
  HMCharacteristicValueCarbonDioxideDetectionStatusDetected = 1;
  HMCharacteristicValueOccupancyStatusNotOccupied = 0;
  HMCharacteristicValueOccupancyStatusOccupied = 1;
  HMCharacteristicValueSecuritySystemAlarmTypeNoAlarm = 0;
  HMCharacteristicValueSecuritySystemAlarmTypeUnknown = 1;
  HMCharacteristicValueCurrentAirPurifierStateInactive = 0;
  HMCharacteristicValueCurrentAirPurifierStateIdle = 1;
  HMCharacteristicValueCurrentAirPurifierStateActive = 2;
  HMCharacteristicValueTargetAirPurifierStateManual = 0;
  HMCharacteristicValueTargetAirPurifierStateAutomatic = 1;
  HMCharacteristicValueCurrentSlatStateStationary = 0;
  HMCharacteristicValueCurrentSlatStateJammed = 1;
  HMCharacteristicValueCurrentSlatStateOscillating = 2;
  HMCharacteristicValueSlatTypeHorizontal = 0;
  HMCharacteristicValueSlatTypeVertical = 1;
  HMCharacteristicValueFilterChangeNotNeeded = 0;
  HMCharacteristicValueFilterChangeNeeded = 1;
  HMCharacteristicValueLabelNamespaceDot = 0;
  HMCharacteristicValueLabelNamespaceNumeral = 1;
  HMCharacteristicValueProgramModeNotScheduled = 0;
  HMCharacteristicValueProgramModeScheduled = 1;
  HMCharacteristicValueProgramModeScheduleOverriddenToManual = 2;
  HMCharacteristicValueUsageStateNotInUse = 0;
  HMCharacteristicValueUsageStateInUse = 1;
  HMCharacteristicValueValveTypeGenericValve = 0;
  HMCharacteristicValueValveTypeIrrigation = 1;
  HMCharacteristicValueValveTypeShowerHead = 2;
  HMCharacteristicValueValveTypeWaterFaucet = 3;
  HMCharacteristicValueDoorStateOpen = 0;
  HMCharacteristicValueDoorStateClosed = 1;
  HMCharacteristicValueDoorStateOpening = 2;
  HMCharacteristicValueDoorStateClosing = 3;
  HMCharacteristicValueDoorStateStopped = 4;
  HMCharacteristicValueCurrentHeatingCoolingOff = 0;
  HMCharacteristicValueCurrentHeatingCoolingHeat = 1;
  HMCharacteristicValueCurrentHeatingCoolingCool = 2;
  HMCharacteristicValueLockMechanismStateUnsecured = 0;
  HMCharacteristicValueLockMechanismStateSecured = 1;
  HMCharacteristicValueLockMechanismStateJammed = 2;
  HMCharacteristicValueLockMechanismStateUnknown = 3;
  HMCharacteristicValueTargetLockMechanismStateUnsecured = 0;
  HMCharacteristicValueTargetLockMechanismStateSecured = 1;
  HMCharacteristicValueRotationDirectionClockwise = 0;
  HMCharacteristicValueRotationDirectionCounterClockwise = 1;
  HMCharacteristicValueTargetDoorStateOpen = 0;
  HMCharacteristicValueTargetDoorStateClosed = 1;
  HMCharacteristicValueHeatingCoolingOff = 0;
  HMCharacteristicValueHeatingCoolingHeat = 1;
  HMCharacteristicValueHeatingCoolingCool = 2;
  HMCharacteristicValueHeatingCoolingAuto = 3;
  HMCharacteristicValueTemperatureUnitCelsius = 0;
  HMCharacteristicValueTemperatureUnitFahrenheit = 1;
  HMCharacteristicValueInputEventSinglePress = 0;
  HMCharacteristicValueInputEventDoublePress = 1;
  HMCharacteristicValueInputEventLongPress = 2;
  HMCharacteristicValueSmokeDetectionStatusNone = 0;
  HMCharacteristicValueSmokeDetectionStatusDetected = 1;
  HMCharacteristicValueBatteryStatusNormal = 0;
  HMCharacteristicValueBatteryStatusLow = 1;
  HMCharacteristicValueChargingStateNone = 0;
  HMCharacteristicValueChargingStateInProgress = 1;
  HMCharacteristicValueChargingStateNotChargeable = 2;
  HMCharacteristicValueLockPhysicalControlsStateNotLocked = 0;
  HMCharacteristicValueLockPhysicalControlsStateLocked = 1;
  HMCharacteristicValueCurrentFanStateInactive = 0;
  HMCharacteristicValueCurrentFanStateIdle = 1;
  HMCharacteristicValueCurrentFanStateActive = 2;
  HMCharacteristicValueActivationStateInactive = 0;
  HMCharacteristicValueActivationStateActive = 1;
  HMCharacteristicValueCurrentHeaterCoolerStateInactive = 0;
  HMCharacteristicValueCurrentHeaterCoolerStateIdle = 1;
  HMCharacteristicValueCurrentHeaterCoolerStateHeating = 2;
  HMCharacteristicValueCurrentHeaterCoolerStateCooling = 3;
  HMCharacteristicValueTargetHeaterCoolerStateAutomatic = 0;
  HMCharacteristicValueTargetHeaterCoolerStateHeat = 1;
  HMCharacteristicValueTargetHeaterCoolerStateCool = 2;
  HMCharacteristicValueCurrentHumidifierDehumidifierStateInactive = 0;
  HMCharacteristicValueCurrentHumidifierDehumidifierStateIdle = 1;
  HMCharacteristicValueCurrentHumidifierDehumidifierStateHumidifying = 2;
  HMCharacteristicValueCurrentHumidifierDehumidifierStateDehumidifying = 3;
  HMCharacteristicValueTargetHumidifierDehumidifierStateAutomatic = 0;
  HMCharacteristicValueTargetHumidifierDehumidifierStateHumidify = 1;
  HMCharacteristicValueTargetHumidifierDehumidifierStateDehumidify = 2;
  HMCharacteristicValueSwingModeDisabled = 0;
  HMCharacteristicValueSwingModeEnabled = 1;
  HMCharacteristicValueTargetFanStateManual = 0;
  HMCharacteristicValueTargetFanStateAutomatic = 1;
  HMCharacteristicValueConfigurationStateNotConfigured = 0;
  HMCharacteristicValueConfigurationStateConfigured = 1;
  HMEventTriggerActivationStateDisabled = 0;
  HMEventTriggerActivationStateDisabledNoHomeHub = 1;
  HMEventTriggerActivationStateDisabledNoCompatibleHomeHub = 2;
  HMEventTriggerActivationStateDisabledNoLocationServicesAuthorization = 3;
  HMEventTriggerActivationStateEnabled = 4;
  HMErrorCodeUnexpectedError = -1;
  HMErrorCodeAlreadyExists = 1;
  HMErrorCodeNotFound = 2;
  HMErrorCodeInvalidParameter = 3;
  HMErrorCodeAccessoryNotReachable = 4;
  HMErrorCodeReadOnlyCharacteristic = 5;
  HMErrorCodeWriteOnlyCharacteristic = 6;
  HMErrorCodeNotificationNotSupported = 7;
  HMErrorCodeOperationTimedOut = 8;
  HMErrorCodeAccessoryPoweredOff = 9;
  HMErrorCodeAccessDenied = 10;
  HMErrorCodeObjectAssociatedToAnotherHome = 11;
  HMErrorCodeObjectNotAssociatedToAnyHome = 12;
  HMErrorCodeObjectAlreadyAssociatedToHome = 13;
  HMErrorCodeAccessoryIsBusy = 14;
  HMErrorCodeOperationInProgress = 15;
  HMErrorCodeAccessoryOutOfResources = 16;
  HMErrorCodeInsufficientPrivileges = 17;
  HMErrorCodeAccessoryPairingFailed = 18;
  HMErrorCodeInvalidDataFormatSpecified = 19;
  HMErrorCodeNilParameter = 20;
  HMErrorCodeUnconfiguredParameter = 21;
  HMErrorCodeInvalidClass = 22;
  HMErrorCodeOperationCancelled = 23;
  HMErrorCodeRoomForHomeCannotBeInZone = 24;
  HMErrorCodeNoActionsInActionSet = 25;
  HMErrorCodeNoRegisteredActionSets = 26;
  HMErrorCodeMissingParameter = 27;
  HMErrorCodeFireDateInPast = 28;
  HMErrorCodeRoomForHomeCannotBeUpdated = 29;
  HMErrorCodeActionInAnotherActionSet = 30;
  HMErrorCodeObjectWithSimilarNameExistsInHome = 31;
  HMErrorCodeHomeWithSimilarNameExists = 32;
  HMErrorCodeRenameWithSimilarName = 33;
  HMErrorCodeCannotRemoveNonBridgeAccessory = 34;
  HMErrorCodeNameContainsProhibitedCharacters = 35;
  HMErrorCodeNameDoesNotStartWithValidCharacters = 36;
  HMErrorCodeUserIDNotEmailAddress = 37;
  HMErrorCodeUserDeclinedAddingUser = 38;
  HMErrorCodeUserDeclinedRemovingUser = 39;
  HMErrorCodeUserDeclinedInvite = 40;
  HMErrorCodeUserManagementFailed = 41;
  HMErrorCodeRecurrenceTooSmall = 42;
  HMErrorCodeInvalidValueType = 43;
  HMErrorCodeValueLowerThanMinimum = 44;
  HMErrorCodeValueHigherThanMaximum = 45;
  HMErrorCodeStringLongerThanMaximum = 46;
  HMErrorCodeHomeAccessNotAuthorized = 47;
  HMErrorCodeOperationNotSupported = 48;
  HMErrorCodeMaximumObjectLimitReached = 49;
  HMErrorCodeAccessorySentInvalidResponse = 50;
  HMErrorCodeStringShorterThanMinimum = 51;
  HMErrorCodeGenericError = 52;
  HMErrorCodeSecurityFailure = 53;
  HMErrorCodeCommunicationFailure = 54;
  HMErrorCodeMessageAuthenticationFailed = 55;
  HMErrorCodeInvalidMessageSize = 56;
  HMErrorCodeAccessoryDiscoveryFailed = 57;
  HMErrorCodeClientRequestError = 58;
  HMErrorCodeAccessoryResponseError = 59;
  HMErrorCodeNameDoesNotEndWithValidCharacters = 60;
  HMErrorCodeAccessoryIsBlocked = 61;
  HMErrorCodeInvalidAssociatedServiceType = 62;
  HMErrorCodeActionSetExecutionFailed = 63;
  HMErrorCodeActionSetExecutionPartialSuccess = 64;
  HMErrorCodeActionSetExecutionInProgress = 65;
  HMErrorCodeAccessoryOutOfCompliance = 66;
  HMErrorCodeDataResetFailure = 67;
  HMErrorCodeNotificationAlreadyEnabled = 68;
  HMErrorCodeRecurrenceMustBeOnSpecifiedBoundaries = 69;
  HMErrorCodeDateMustBeOnSpecifiedBoundaries = 70;
  HMErrorCodeCannotActivateTriggerTooFarInFuture = 71;
  HMErrorCodeRecurrenceTooLarge = 72;
  HMErrorCodeReadWritePartialSuccess = 73;
  HMErrorCodeReadWriteFailure = 74;
  HMErrorCodeNotSignedIntoiCloud = 75;
  HMErrorCodeKeychainSyncNotEnabled = 76;
  HMErrorCodeCloudDataSyncInProgress = 77;
  HMErrorCodeNetworkUnavailable = 78;
  HMErrorCodeAddAccessoryFailed = 79;
  HMErrorCodeMissingEntitlement = 80;
  HMErrorCodeCannotUnblockNonBridgeAccessory = 81;
  HMErrorCodeDeviceLocked = 82;
  HMErrorCodeCannotRemoveBuiltinActionSet = 83;
  HMErrorCodeLocationForHomeDisabled = 84;
  HMErrorCodeNotAuthorizedForLocationServices = 85;
  HMErrorCodeReferToUserManual = 86;
  HMErrorCodeInvalidOrMissingAuthorizationData = 87;
  HMErrorCodeBridgedAccessoryNotReachable = 88;
  HMErrorCodeNotAuthorizedForMicrophoneAccess = 89;
  HMErrorCodeIncompatibleNetwork = 90;
  HMErrorCodeNoHomeHub = 91;
  HMErrorCodeNoCompatibleHomeHub = 92;
  HMErrorCodeIncompatibleAccessory = 93;
  HMErrorCodeIncompatibleHomeHub = HMErrorCodeNoCompatibleHomeHub;
  HMErrorCodeObjectWithSimilarNameExists = 95;
  HMErrorCodeOwnershipFailure = 96;
  HMErrorCodeMaximumAccessoriesOfTypeInHome = 97;
  HMErrorCodeWiFiCredentialGenerationFailed = 98;
  HMErrorCodeEnterpriseNetworkNotSupported = 99;
  HMErrorCodeTimedOutWaitingForAccessory = 100;
  HMErrorCodeAccessoryCommunicationFailure = 101;
  HMErrorCodeFailedToJoinNetwork = 102;
  HMErrorCodeAccessoryIsSuspended = 103;
  HMPresenceEventTypeEveryEntry = 1;
  HMPresenceEventTypeEveryExit = 2;
  HMPresenceEventTypeFirstEntry = 3;
  HMPresenceEventTypeLastExit = 4;
  HMPresenceEventTypeAtHome = HMPresenceEventTypeFirstEntry;
  HMPresenceEventTypeNotAtHome = HMPresenceEventTypeLastExit;
  HMPresenceEventUserTypeCurrentUser = 1;
  HMPresenceEventUserTypeHomeUsers = 2;
  HMPresenceEventUserTypeCustomUsers = 3;
  HMCameraStreamStateStarting = 1;
  HMCameraStreamStateStreaming = 2;
  HMCameraStreamStateStopping = 3;
  HMCameraStreamStateNotStreaming = 4;
  HMCameraAudioStreamSettingMuted = 1;
  HMCameraAudioStreamSettingIncomingAudioAllowed = 2;
  HMCameraAudioStreamSettingBidirectionalAudioAllowed = 3;

type
  HMHomeManager = interface;
  HMHomeManagerDelegate = interface;
  HMAccessoryBrowser = interface;
  HMAccessoryBrowserDelegate = interface;
  HMHome = interface;
  HMHomeDelegate = interface;
  HMRoom = interface;
  HMZone = interface;
  HMServiceGroup = interface;
  HMAccessory = interface;
  HMAccessoryDelegate = interface;
  HMService = interface;
  HMCharacteristic = interface;
  HMCharacteristicMetadata = interface;
  HMAction = interface;
  HMCharacteristicWriteAction = interface;
  HMActionSet = interface;
  HMTrigger = interface;
  HMTimerTrigger = interface;
  HMUser = interface;
  HMAccessControl = interface;
  HMHomeAccessControl = interface;
  HMAccessoryCategory = interface;
  HMEvent = interface;
  HMCharacteristicEvent = interface;
  HMMutableCharacteristicEvent = interface;
  HMLocationEvent = interface;
  HMMutableLocationEvent = interface;
  HMTimeEvent = interface;
  HMCalendarEvent = interface;
  HMMutableCalendarEvent = interface;
  HMSignificantTimeEvent = interface;
  HMMutableSignificantTimeEvent = interface;
  HMEventTrigger = interface;
  HMAccessoryProfile = interface;
  HMDurationEvent = interface;
  HMMutableDurationEvent = interface;
  HMCharacteristicThresholdRangeEvent = interface;
  HMMutableCharacteristicThresholdRangeEvent = interface;
  HMPresenceEvent = interface;
  HMMutablePresenceEvent = interface;
  HMNumberRange = interface;
  HMAddAccessoryRequest = interface;
  HMNetworkConfigurationProfile = interface;
  HMNetworkConfigurationProfileDelegate = interface;
  HMCameraView = interface;
  HMCameraProfile = interface;
  HMCameraControl = interface;
  HMCameraStreamControl = interface;
  HMCameraStreamControlDelegate = interface;
  HMCameraSource = interface;
  HMCameraStream = interface;
  HMCameraSnapshotControl = interface;
  HMCameraSnapshotControlDelegate = interface;
  HMCameraSnapshot = interface;
  HMCameraSettingsControl = interface;
  HMCameraAudioControl = interface;
  HMAccessorySetupManager = interface;
  HMAccessoryOwnershipToken = interface;
  HMAccessorySetupPayload = interface;
  HMAccessorySetupRequest = interface;
  HMAccessorySetupResult = interface;
  HMMatterHome = interface;
  HMMatterRequestHandler = interface;
  HMMatterRoom = interface;
  HMMatterTopology = interface;

  HMErrorBlock = procedure(error: NSError) of object;
  HMHomeManagerAuthorizationStatus = NSInteger;
  HMHomeHubState = NSInteger;
  HMCharacteristicValueLockMechanismLastKnownAction = NSInteger;
  HMCharacteristicValueAirParticulateSize = NSInteger;
  HMCharacteristicValueAirQuality = NSInteger;
  HMCharacteristicValuePositionState = NSInteger;
  HMCharacteristicValueCurrentSecuritySystemState = NSInteger;
  HMCharacteristicValueTargetSecuritySystemState = NSInteger;
  HMCharacteristicValueJammedStatus = NSInteger;
  HMCharacteristicValueTamperedStatus = NSInteger;
  HMCharacteristicValueLeakStatus = NSInteger;
  HMCharacteristicValueContactState = NSInteger;
  HMCharacteristicValueStatusFault = NSInteger;
  HMCharacteristicValueCarbonMonoxideDetectionStatus = NSInteger;
  HMCharacteristicValueCarbonDioxideDetectionStatus = NSInteger;
  HMCharacteristicValueOccupancyStatus = NSInteger;
  HMCharacteristicValueSecuritySystemAlarmType = NSInteger;
  HMCharacteristicValueCurrentAirPurifierState = NSInteger;
  HMCharacteristicValueTargetAirPurifierState = NSInteger;
  HMCharacteristicValueCurrentSlatState = NSInteger;
  HMCharacteristicValueSlatType = NSInteger;
  HMCharacteristicValueFilterChange = NSInteger;
  HMCharacteristicValueLabelNamespace = NSInteger;
  HMCharacteristicValueProgramMode = NSInteger;
  HMCharacteristicValueUsageState = NSInteger;
  HMCharacteristicValueValveType = NSInteger;
  HMCharacteristicValueDoorState = NSInteger;
  HMCharacteristicValueCurrentHeatingCooling = NSInteger;
  HMCharacteristicValueLockMechanismState = NSInteger;
  HMCharacteristicValueTargetLockMechanismState = NSInteger;
  HMCharacteristicValueRotationDirection = NSInteger;
  HMCharacteristicValueTargetDoorState = NSInteger;
  HMCharacteristicValueHeatingCooling = NSInteger;
  HMCharacteristicValueTemperatureUnit = NSInteger;
  HMCharacteristicValueInputEvent = NSInteger;
  HMCharacteristicValueSmokeDetectionStatus = NSInteger;
  HMCharacteristicValueBatteryStatus = NSInteger;
  HMCharacteristicValueChargingState = NSInteger;
  HMCharacteristicValueLockPhysicalControlsState = NSInteger;
  HMCharacteristicValueCurrentFanState = NSInteger;
  HMCharacteristicValueActivationState = NSInteger;
  HMCharacteristicValueCurrentHeaterCoolerState = NSInteger;
  HMCharacteristicValueTargetHeaterCoolerState = NSInteger;
  HMCharacteristicValueCurrentHumidifierDehumidifierState = NSInteger;
  HMCharacteristicValueTargetHumidifierDehumidifierState = NSInteger;
  HMCharacteristicValueSwingMode = NSInteger;
  HMCharacteristicValueTargetFanState = NSInteger;
  HMCharacteristicValueConfigurationState = NSInteger;
  HMEventTriggerActivationState = NSInteger;
  HMSignificantEvent = NSString;
  HMErrorCode = NSInteger;
  HMPresenceEventType = NSInteger;
  HMPresenceEventUserType = NSInteger;
  HMCameraStreamState = NSInteger;
  HMCameraAudioStreamSetting = NSInteger;
  THMHomeManagerBlockMethod1 = procedure(error: NSError) of object;
  THMHomeManagerBlockMethod2 = procedure(home: HMHome; error: NSError) of object;
  THMHomeBlockMethod1 = procedure(error: NSError) of object;
  THMHomeBlockMethod2 = procedure(accessories: NSArray; error: NSError) of object;
  THMHomeBlockMethod3 = procedure(user: HMUser; error: NSError) of object;
  THMHomeBlockMethod4 = procedure(room: HMRoom; error: NSError) of object;
  THMHomeBlockMethod5 = procedure(zone: HMZone; error: NSError) of object;
  THMHomeBlockMethod6 = procedure(group: HMServiceGroup; error: NSError) of object;
  THMHomeBlockMethod7 = procedure(actionSet: HMActionSet; error: NSError) of object;
  THMRoomBlockMethod1 = procedure(error: NSError) of object;
  THMZoneBlockMethod1 = procedure(error: NSError) of object;
  THMServiceGroupBlockMethod1 = procedure(error: NSError) of object;
  THMAccessoryBlockMethod1 = procedure(error: NSError) of object;
  THMServiceBlockMethod1 = procedure(error: NSError) of object;
  THMCharacteristicBlockMethod1 = procedure(error: NSError) of object;
  THMCharacteristicWriteActionBlockMethod1 = procedure(error: NSError) of object;
  THMTriggerBlockMethod1 = procedure(error: NSError) of object;
  THMTimerTriggerBlockMethod1 = procedure(error: NSError) of object;
  THMCharacteristicEventBlockMethod1 = procedure(error: NSError) of object;
  THMLocationEventBlockMethod1 = procedure(error: NSError) of object;
  THMEventTriggerBlockMethod1 = procedure(error: NSError) of object;
  THMCameraStreamBlockMethod1 = procedure(error: NSError) of object;
  THMAccessorySetupManagerBlockMethod1 = procedure(result: HMAccessorySetupResult; error: NSError) of object;
  THMMatterRequestHandlerBlockMethod1 = procedure(param1: NSArray; param2: NSError) of object;
  THMMatterRequestHandlerBlockMethod2 = procedure(param1: NSError) of object;

  // **** NOTE: It is possible that the following two types are not correctly translated. USE AT YOUR OWN RISK **
  TargetValueType = Pointer;
  TriggerValueType = Pointer;

  HMHomeManagerClass = interface(NSObjectClass)
    ['{592D30EE-F48C-465F-96BC-A63740ED3575}']
  end;

  HMHomeManager = interface(NSObject)
    ['{B1500DC2-3A4F-4400-B521-C6F28C6F7298}']
    procedure addHomeWithName(homeName: NSString; completionHandler: THMHomeManagerBlockMethod2); cdecl;
    function authorizationStatus: HMHomeManagerAuthorizationStatus; cdecl;
    function delegate: Pointer; cdecl;
    function homes: NSArray; cdecl;
    function primaryHome: HMHome; cdecl;
    procedure removeHome(home: HMHome; completionHandler: THMHomeManagerBlockMethod1); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure updatePrimaryHome(home: HMHome; completionHandler: THMHomeManagerBlockMethod1); cdecl;
  end;
  THMHomeManager = class(TOCGenericImport<HMHomeManagerClass, HMHomeManager>) end;

  HMHomeManagerDelegate = interface(IObjectiveC)
    ['{D23FEB53-55D3-44B9-AC0D-556066687577}']
    [MethodName('homeManager:didAddHome:')]
    procedure homeManagerDidAddHome(manager: HMHomeManager; didAddHome: HMHome); cdecl;
    [MethodName('homeManager:didReceiveAddAccessoryRequest:')]
    procedure homeManagerDidReceiveAddAccessoryRequest(manager: HMHomeManager; didReceiveAddAccessoryRequest: HMAddAccessoryRequest); cdecl;
    [MethodName('homeManager:didRemoveHome:')]
    procedure homeManagerDidRemoveHome(manager: HMHomeManager; didRemoveHome: HMHome); cdecl;
    [MethodName('homeManager:didUpdateAuthorizationStatus:')]
    procedure homeManagerDidUpdateAuthorizationStatus(manager: HMHomeManager; didUpdateAuthorizationStatus: HMHomeManagerAuthorizationStatus); cdecl;
    procedure homeManagerDidUpdateHomes(manager: HMHomeManager); cdecl;
    procedure homeManagerDidUpdatePrimaryHome(manager: HMHomeManager); cdecl;
  end;

  HMAccessoryBrowserClass = interface(NSObjectClass)
    ['{A37FF503-2E4E-4120-9B3D-A7E6D71CF32D}']
  end;

  HMAccessoryBrowser = interface(NSObject)
    ['{8B2A45B7-49EC-4268-98A4-09ADCC4DF8D8}']
    function delegate: Pointer; cdecl;
    function discoveredAccessories: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure startSearchingForNewAccessories; cdecl;
    procedure stopSearchingForNewAccessories; cdecl;
  end;
  THMAccessoryBrowser = class(TOCGenericImport<HMAccessoryBrowserClass, HMAccessoryBrowser>) end;

  HMAccessoryBrowserDelegate = interface(IObjectiveC)
    ['{6BCBF622-1AE7-49E5-8F61-ACCCF310C3A7}']
    [MethodName('accessoryBrowser:didFindNewAccessory:')]
    procedure accessoryBrowserDidFindNewAccessory(browser: HMAccessoryBrowser; didFindNewAccessory: HMAccessory); cdecl;
    [MethodName('accessoryBrowser:didRemoveNewAccessory:')]
    procedure accessoryBrowserDidRemoveNewAccessory(browser: HMAccessoryBrowser; didRemoveNewAccessory: HMAccessory); cdecl;
  end;

  HMHomeClass = interface(NSObjectClass)
    ['{69BDA2BC-09AF-4098-A46B-F5DF25FE5450}']
  end;

  HMHome = interface(NSObject)
    ['{2E4F9C8A-6F7E-44E0-9B8B-7DC2A2FB93C9}']
    function accessories: NSArray; cdecl;
    function actionSets: NSArray; cdecl;
    procedure addAccessory(accessory: HMAccessory; completionHandler: THMHomeBlockMethod1); cdecl;
    procedure addActionSetWithName(actionSetName: NSString; completionHandler: THMHomeBlockMethod7); cdecl;
    procedure addAndSetupAccessoriesWithCompletionHandler(completion: THMHomeBlockMethod1); cdecl; // API_DEPRECATED("Use -[HMAccessorySetupManager performAccessorySetupUsingRequest:completionHandler:] instead", ios(10.0, 15.4))
    procedure addAndSetupAccessoriesWithPayload(payload: HMAccessorySetupPayload; completionHandler: THMHomeBlockMethod2); cdecl; // API_DEPRECATED("Use -[HMAccessorySetupManager performAccessorySetupUsingRequest:completionHandler:] instead", ios(11.3, 15.0))
    procedure addRoomWithName(roomName: NSString; completionHandler: THMHomeBlockMethod4); cdecl;
    procedure addServiceGroupWithName(serviceGroupName: NSString; completionHandler: THMHomeBlockMethod6); cdecl;
    procedure addTrigger(trigger: HMTrigger; completionHandler: THMHomeBlockMethod1); cdecl;
    procedure addUserWithCompletionHandler(completion: THMHomeBlockMethod3); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-manageUsersWithCompletionHandler:", ios(8.0, 9.0))
    procedure addZoneWithName(zoneName: NSString; completionHandler: THMHomeBlockMethod5); cdecl;
    procedure assignAccessory(accessory: HMAccessory; toRoom: HMRoom; completionHandler: THMHomeBlockMethod1); cdecl;
    function builtinActionSetOfType(actionSetType: NSString): HMActionSet; cdecl;
    function currentUser: HMUser; cdecl;
    function delegate: Pointer; cdecl;
    procedure executeActionSet(actionSet: HMActionSet; completionHandler: THMHomeBlockMethod1); cdecl;
    function homeAccessControlForUser(user: HMUser): HMHomeAccessControl; cdecl;
    function homeHubState: HMHomeHubState; cdecl;
    function isPrimary: Boolean; cdecl;
    procedure manageUsersWithCompletionHandler(completion: THMHomeBlockMethod1); cdecl;
    function name: NSString; cdecl;
    procedure removeAccessory(accessory: HMAccessory; completionHandler: THMHomeBlockMethod1); cdecl;
    procedure removeActionSet(actionSet: HMActionSet; completionHandler: THMHomeBlockMethod1); cdecl;
    procedure removeRoom(room: HMRoom; completionHandler: THMHomeBlockMethod1); cdecl;
    procedure removeServiceGroup(group: HMServiceGroup; completionHandler: THMHomeBlockMethod1); cdecl;
    procedure removeTrigger(trigger: HMTrigger; completionHandler: THMHomeBlockMethod1); cdecl;
    procedure removeUser(user: HMUser; completionHandler: THMHomeBlockMethod1); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-manageUsersWithCompletionHandler:", ios(8.0, 9.0))
    procedure removeZone(zone: HMZone; completionHandler: THMHomeBlockMethod1); cdecl;
    function roomForEntireHome: HMRoom; cdecl;
    function rooms: NSArray; cdecl;
    function serviceGroups: NSArray; cdecl;
    function servicesWithTypes(serviceTypes: NSArray): NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function supportsAddingNetworkRouter: Boolean; cdecl;
    function triggers: NSArray; cdecl;
    procedure unblockAccessory(accessory: HMAccessory; completionHandler: THMHomeBlockMethod1); cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    procedure updateName(name: NSString; completionHandler: THMHomeBlockMethod1); cdecl;
    function users: NSArray; cdecl; // API_DEPRECATED("No longer supported.", ios(8.0, 9.0))
    function zones: NSArray; cdecl;
  end;
  THMHome = class(TOCGenericImport<HMHomeClass, HMHome>) end;

  HMHomeDelegate = interface(IObjectiveC)
    ['{BA9CBC6F-20DB-4318-A6E6-34AA0C902340}']
    [MethodName('home:didAddAccessory:')]
    procedure homeDidAddAccessory(home: HMHome; didAddAccessory: HMAccessory); cdecl;
    [MethodName('home:didAddActionSet:')]
    procedure homeDidAddActionSet(home: HMHome; didAddActionSet: HMActionSet); cdecl;
    [MethodName('home:didAddRoom:toZone:')]
    procedure homeDidAddRoom(home: HMHome; didAddRoom: HMRoom; toZone: HMZone); overload; cdecl;
    [MethodName('home:didAddRoom:')]
    procedure homeDidAddRoom(home: HMHome; didAddRoom: HMRoom); overload; cdecl;
    [MethodName('home:didAddService:toServiceGroup:')]
    procedure homeDidAddService(home: HMHome; didAddService: HMService; toServiceGroup: HMServiceGroup); cdecl;
    [MethodName('home:didAddServiceGroup:')]
    procedure homeDidAddServiceGroup(home: HMHome; didAddServiceGroup: HMServiceGroup); cdecl;
    [MethodName('home:didAddTrigger:')]
    procedure homeDidAddTrigger(home: HMHome; didAddTrigger: HMTrigger); cdecl;
    [MethodName('home:didAddUser:')]
    procedure homeDidAddUser(home: HMHome; didAddUser: HMUser); cdecl;
    [MethodName('home:didAddZone:')]
    procedure homeDidAddZone(home: HMHome; didAddZone: HMZone); cdecl;
    [MethodName('home:didEncounterError:forAccessory:')]
    procedure homeDidEncounterError(home: HMHome; didEncounterError: NSError; forAccessory: HMAccessory); cdecl;
    [MethodName('home:didRemoveAccessory:')]
    procedure homeDidRemoveAccessory(home: HMHome; didRemoveAccessory: HMAccessory); cdecl;
    [MethodName('home:didRemoveActionSet:')]
    procedure homeDidRemoveActionSet(home: HMHome; didRemoveActionSet: HMActionSet); cdecl;
    [MethodName('home:didRemoveRoom:fromZone:')]
    procedure homeDidRemoveRoom(home: HMHome; didRemoveRoom: HMRoom; fromZone: HMZone); overload; cdecl;
    [MethodName('home:didRemoveRoom:')]
    procedure homeDidRemoveRoom(home: HMHome; didRemoveRoom: HMRoom); overload; cdecl;
    [MethodName('home:didRemoveService:fromServiceGroup:')]
    procedure homeDidRemoveService(home: HMHome; didRemoveService: HMService; fromServiceGroup: HMServiceGroup); cdecl;
    [MethodName('home:didRemoveServiceGroup:')]
    procedure homeDidRemoveServiceGroup(home: HMHome; didRemoveServiceGroup: HMServiceGroup); cdecl;
    [MethodName('home:didRemoveTrigger:')]
    procedure homeDidRemoveTrigger(home: HMHome; didRemoveTrigger: HMTrigger); cdecl;
    [MethodName('home:didRemoveUser:')]
    procedure homeDidRemoveUser(home: HMHome; didRemoveUser: HMUser); cdecl;
    [MethodName('home:didRemoveZone:')]
    procedure homeDidRemoveZone(home: HMHome; didRemoveZone: HMZone); cdecl;
    [MethodName('home:didUnblockAccessory:')]
    procedure homeDidUnblockAccessory(home: HMHome; didUnblockAccessory: HMAccessory); cdecl;
    procedure homeDidUpdateAccessControlForCurrentUser(home: HMHome); cdecl;
    [MethodName('home:didUpdateActionsForActionSet:')]
    procedure homeDidUpdateActionsForActionSet(home: HMHome; didUpdateActionsForActionSet: HMActionSet); cdecl;
    [MethodName('home:didUpdateHomeHubState:')]
    procedure homeDidUpdateHomeHubState(home: HMHome; didUpdateHomeHubState: HMHomeHubState); cdecl;
    procedure homeDidUpdateName(home: HMHome); cdecl;
    [MethodName('home:didUpdateNameForActionSet:')]
    procedure homeDidUpdateNameForActionSet(home: HMHome; didUpdateNameForActionSet: HMActionSet); cdecl;
    [MethodName('home:didUpdateNameForRoom:')]
    procedure homeDidUpdateNameForRoom(home: HMHome; didUpdateNameForRoom: HMRoom); cdecl;
    [MethodName('home:didUpdateNameForServiceGroup:')]
    procedure homeDidUpdateNameForServiceGroup(home: HMHome; didUpdateNameForServiceGroup: HMServiceGroup); cdecl;
    [MethodName('home:didUpdateNameForTrigger:')]
    procedure homeDidUpdateNameForTrigger(home: HMHome; didUpdateNameForTrigger: HMTrigger); cdecl;
    [MethodName('home:didUpdateNameForZone:')]
    procedure homeDidUpdateNameForZone(home: HMHome; didUpdateNameForZone: HMZone); cdecl;
    [MethodName('home:didUpdateRoom:forAccessory:')]
    procedure homeDidUpdateRoom(home: HMHome; didUpdateRoom: HMRoom; forAccessory: HMAccessory); cdecl;
    procedure homeDidUpdateSupportedFeatures(home: HMHome); cdecl;
    [MethodName('home:didUpdateTrigger:')]
    procedure homeDidUpdateTrigger(home: HMHome; didUpdateTrigger: HMTrigger); cdecl;
  end;

  HMRoomClass = interface(NSObjectClass)
    ['{BCFCC1BF-096D-46E2-B1BB-08D3227662CE}']
  end;

  HMRoom = interface(NSObject)
    ['{454BF728-B763-4760-BCCF-962F64A11176}']
    function accessories: NSArray; cdecl;
    function name: NSString; cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    procedure updateName(name: NSString; completionHandler: THMRoomBlockMethod1); cdecl;
  end;
  THMRoom = class(TOCGenericImport<HMRoomClass, HMRoom>) end;

  HMZoneClass = interface(NSObjectClass)
    ['{E365D969-CE6B-4681-AEE9-0A792E8B72F2}']
  end;

  HMZone = interface(NSObject)
    ['{C3FA66B9-FF6D-446E-8211-A5BDFD7BA6EA}']
    procedure addRoom(room: HMRoom; completionHandler: THMZoneBlockMethod1); cdecl;
    function name: NSString; cdecl;
    procedure removeRoom(room: HMRoom; completionHandler: THMZoneBlockMethod1); cdecl;
    function rooms: NSArray; cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    procedure updateName(name: NSString; completionHandler: THMZoneBlockMethod1); cdecl;
  end;
  THMZone = class(TOCGenericImport<HMZoneClass, HMZone>) end;

  HMServiceGroupClass = interface(NSObjectClass)
    ['{7BC140F2-1BDB-42AF-AB68-2AD10B8BEE00}']
  end;

  HMServiceGroup = interface(NSObject)
    ['{891489C4-1F04-400D-AE4E-C97EB7C768AF}']
    procedure addService(service: HMService; completionHandler: THMServiceGroupBlockMethod1); cdecl;
    function name: NSString; cdecl;
    procedure removeService(service: HMService; completionHandler: THMServiceGroupBlockMethod1); cdecl;
    function services: NSArray; cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    procedure updateName(name: NSString; completionHandler: THMServiceGroupBlockMethod1); cdecl;
  end;
  THMServiceGroup = class(TOCGenericImport<HMServiceGroupClass, HMServiceGroup>) end;

  HMAccessoryClass = interface(NSObjectClass)
    ['{D8D0CC1F-8AE0-43EA-ADA6-597AFFCF508D}']
  end;

  HMAccessory = interface(NSObject)
    ['{7A274D15-6296-4418-8CB8-C03152767594}']
    function cameraProfiles: NSArray; cdecl;
    function category: HMAccessoryCategory; cdecl;
    function delegate: Pointer; cdecl;
    function firmwareVersion: NSString; cdecl;
    function identifier: NSUUID; cdecl; // API_DEPRECATED("No longer supported.", ios(8.0, 9.0))
    function identifiersForBridgedAccessories: NSArray; cdecl; // API_DEPRECATED("No longer supported.", ios(8.0, 9.0))
    procedure identifyWithCompletionHandler(completion: THMAccessoryBlockMethod1); cdecl;
    function isBlocked: Boolean; cdecl;
    function isBridged: Boolean; cdecl;
    function isReachable: Boolean; cdecl;
    function manufacturer: NSString; cdecl;
    function model: NSString; cdecl;
    function name: NSString; cdecl;
    function profiles: NSArray; cdecl;
    function room: HMRoom; cdecl;
    function services: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function supportsIdentify: Boolean; cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    function uniqueIdentifiersForBridgedAccessories: NSArray; cdecl;
    procedure updateName(name: NSString; completionHandler: THMAccessoryBlockMethod1); cdecl;
  end;
  THMAccessory = class(TOCGenericImport<HMAccessoryClass, HMAccessory>) end;

  HMAccessoryDelegate = interface(IObjectiveC)
    ['{C48E8281-5BAA-42D2-BE36-E3BC48758AF1}']
    [MethodName('accessory:didAddProfile:')]
    procedure accessoryDidAddProfile(accessory: HMAccessory; didAddProfile: HMAccessoryProfile); cdecl;
    [MethodName('accessory:didRemoveProfile:')]
    procedure accessoryDidRemoveProfile(accessory: HMAccessory; didRemoveProfile: HMAccessoryProfile); cdecl;
    [MethodName('accessory:didUpdateAssociatedServiceTypeForService:')]
    procedure accessoryDidUpdateAssociatedServiceTypeForService(accessory: HMAccessory; didUpdateAssociatedServiceTypeForService: HMService); cdecl;
    [MethodName('accessory:didUpdateFirmwareVersion:')]
    procedure accessoryDidUpdateFirmwareVersion(accessory: HMAccessory; didUpdateFirmwareVersion: NSString); cdecl;
    procedure accessoryDidUpdateName(accessory: HMAccessory); cdecl;
    [MethodName('accessory:didUpdateNameForService:')]
    procedure accessoryDidUpdateNameForService(accessory: HMAccessory; didUpdateNameForService: HMService); cdecl;
    procedure accessoryDidUpdateReachability(accessory: HMAccessory); cdecl;
    procedure accessoryDidUpdateServices(accessory: HMAccessory); cdecl;
    [MethodName('accessory:service:didUpdateValueForCharacteristic:')]
    procedure accessoryService(accessory: HMAccessory; service: HMService; didUpdateValueForCharacteristic: HMCharacteristic); cdecl;
  end;

  HMServiceClass = interface(NSObjectClass)
    ['{83AEA356-3B08-4032-8ED7-2BC147C170FE}']
  end;

  HMService = interface(NSObject)
    ['{D0001098-466E-4F79-B71F-3C8651272735}']
    function accessory: HMAccessory; cdecl;
    function associatedServiceType: NSString; cdecl;
    function characteristics: NSArray; cdecl;
    function isPrimaryService: Boolean; cdecl;
    function isUserInteractive: Boolean; cdecl;
    function linkedServices: NSArray; cdecl;
    function localizedDescription: NSString; cdecl;
    function name: NSString; cdecl;
    function serviceType: NSString; cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    procedure updateAssociatedServiceType(serviceType: NSString; completionHandler: THMServiceBlockMethod1); cdecl;
    procedure updateName(name: NSString; completionHandler: THMServiceBlockMethod1); cdecl;
  end;
  THMService = class(TOCGenericImport<HMServiceClass, HMService>) end;

  HMCharacteristicClass = interface(NSObjectClass)
    ['{C1C80E00-42ED-4C4C-852D-70314365BF5A}']
  end;

  HMCharacteristic = interface(NSObject)
    ['{78FFA6AA-2FFD-4EA9-A7CF-1DEA415A60E6}']
    function characteristicType: NSString; cdecl;
    procedure enableNotification(enable: Boolean; completionHandler: THMCharacteristicBlockMethod1); cdecl;
    function isNotificationEnabled: Boolean; cdecl;
    function localizedDescription: NSString; cdecl;
    function metadata: HMCharacteristicMetadata; cdecl;
    function properties: NSArray; cdecl;
    procedure readValueWithCompletionHandler(completion: THMCharacteristicBlockMethod1); cdecl;
    function service: HMService; cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    procedure updateAuthorizationData(data: NSData; completionHandler: THMCharacteristicBlockMethod1); cdecl;
    function value: Pointer; cdecl;
    procedure writeValue(value: Pointer; completionHandler: THMCharacteristicBlockMethod1); cdecl;
  end;
  THMCharacteristic = class(TOCGenericImport<HMCharacteristicClass, HMCharacteristic>) end;

  HMCharacteristicMetadataClass = interface(NSObjectClass)
    ['{A13D2FCA-AD12-4266-8204-DB609001DB4E}']
  end;

  HMCharacteristicMetadata = interface(NSObject)
    ['{C3FDC94F-B914-4F1C-94E5-DA32F411EFCB}']
    function format: NSString; cdecl;
    function manufacturerDescription: NSString; cdecl;
    function maximumValue: NSNumber; cdecl;
    function maxLength: NSNumber; cdecl;
    function minimumValue: NSNumber; cdecl;
    function stepValue: NSNumber; cdecl;
    function units: NSString; cdecl;
    function validValues: NSArray; cdecl;
  end;
  THMCharacteristicMetadata = class(TOCGenericImport<HMCharacteristicMetadataClass, HMCharacteristicMetadata>) end;

  HMActionClass = interface(NSObjectClass)
    ['{03244E10-59DD-4A07-B939-37300214C836}']
  end;

  HMAction = interface(NSObject)
    ['{53641772-F9FC-48A8-B219-0CF8851B8776}']
    function uniqueIdentifier: NSUUID; cdecl;
  end;
  THMAction = class(TOCGenericImport<HMActionClass, HMAction>) end;

  HMCharacteristicWriteActionClass = interface(HMActionClass)
    ['{1681BFE9-2E90-4163-B5D0-C08FC5384241}']
  end;

  HMCharacteristicWriteAction = interface(HMAction)
    ['{3C19F98F-459A-424F-A10E-124BD6A74AE4}']
    function characteristic: HMCharacteristic; cdecl;
    function initWithCharacteristic(characteristic: HMCharacteristic; targetValue: TargetValueType): Pointer; cdecl;
    function targetValue: TargetValueType; cdecl;
    procedure updateTargetValue(targetValue: TargetValueType; completionHandler: THMCharacteristicWriteActionBlockMethod1); cdecl;
  end;
  THMCharacteristicWriteAction = class(TOCGenericImport<HMCharacteristicWriteActionClass, HMCharacteristicWriteAction>) end;

  HMActionSetClass = interface(NSObjectClass)
    ['{8579E890-D14E-4869-A4AB-4F9FE256BD81}']
  end;

  HMActionSet = interface(NSObject)
    ['{35D43FE6-12EC-4538-B766-B60E4332B48E}']
    function actions: NSSet; cdecl;
    function actionSetType: NSString; cdecl;
    procedure addAction(action: HMAction; completionHandler: HMErrorBlock); cdecl;
    function isExecuting: Boolean; cdecl;
    function lastExecutionDate: NSDate; cdecl;
    function name: NSString; cdecl;
    procedure removeAction(action: HMAction; completionHandler: HMErrorBlock); cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    procedure updateName(name: NSString; completionHandler: HMErrorBlock); cdecl;
  end;
  THMActionSet = class(TOCGenericImport<HMActionSetClass, HMActionSet>) end;

  HMTriggerClass = interface(NSObjectClass)
    ['{3B18267A-86FE-4FCE-89D6-A04AA2AAD876}']
  end;

  HMTrigger = interface(NSObject)
    ['{0600CCE2-3917-4AB7-B884-7041C0A9001E}']
    function actionSets: NSArray; cdecl;
    procedure addActionSet(actionSet: HMActionSet; completionHandler: THMTriggerBlockMethod1); cdecl;
    procedure enable(enable: Boolean; completionHandler: THMTriggerBlockMethod1); cdecl;
    function isEnabled: Boolean; cdecl;
    function lastFireDate: NSDate; cdecl;
    function name: NSString; cdecl;
    procedure removeActionSet(actionSet: HMActionSet; completionHandler: THMTriggerBlockMethod1); cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
    procedure updateName(name: NSString; completionHandler: THMTriggerBlockMethod1); cdecl;
  end;
  THMTrigger = class(TOCGenericImport<HMTriggerClass, HMTrigger>) end;

  HMTimerTriggerClass = interface(HMTriggerClass)
    ['{AC9EE31E-6B2D-498D-9657-BF8BC2F6234D}']
  end;

  HMTimerTrigger = interface(HMTrigger)
    ['{700F59E7-E223-42E7-9DF4-1D3FBAB33D6B}']
    function fireDate: NSDate; cdecl;
    function initWithName(name: NSString; fireDate: NSDate; timeZone: NSTimeZone; recurrence: NSDateComponents;
      recurrenceCalendar: NSCalendar): Pointer; cdecl;
    function recurrence: NSDateComponents; cdecl;
    function recurrenceCalendar: NSCalendar; cdecl;
    function timeZone: NSTimeZone; cdecl;
    procedure updateFireDate(fireDate: NSDate; completionHandler: THMTimerTriggerBlockMethod1); cdecl;
    procedure updateRecurrence(recurrence: NSDateComponents; completionHandler: THMTimerTriggerBlockMethod1); cdecl;
    procedure updateTimeZone(timeZone: NSTimeZone; completionHandler: THMTimerTriggerBlockMethod1); cdecl;
  end;
  THMTimerTrigger = class(TOCGenericImport<HMTimerTriggerClass, HMTimerTrigger>) end;

  HMUserClass = interface(NSObjectClass)
    ['{520BFA52-12C1-4522-9D5A-97B436893526}']
  end;

  HMUser = interface(NSObject)
    ['{AD995B7A-7593-4FA7-8808-87BA7DACF88B}']
    function name: NSString; cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
  end;
  THMUser = class(TOCGenericImport<HMUserClass, HMUser>) end;

  HMAccessControlClass = interface(NSObjectClass)
    ['{98CB8F04-A116-482A-BF0C-6A323C458D28}']
  end;

  HMAccessControl = interface(NSObject)
    ['{9372858B-E114-47F8-A48C-7EE9C1DE32FA}']
  end;
  THMAccessControl = class(TOCGenericImport<HMAccessControlClass, HMAccessControl>) end;

  HMHomeAccessControlClass = interface(HMAccessControlClass)
    ['{A04B6D3C-96AE-40C2-ACF1-6DD10C8A0713}']
  end;

  HMHomeAccessControl = interface(HMAccessControl)
    ['{EAB525B6-A65B-4F5B-9586-3CBF97FA8920}']
    function isAdministrator: Boolean; cdecl;
  end;
  THMHomeAccessControl = class(TOCGenericImport<HMHomeAccessControlClass, HMHomeAccessControl>) end;

  HMAccessoryCategoryClass = interface(NSObjectClass)
    ['{313B4FDE-EE52-4B0A-A73C-4A4C94C74446}']
  end;

  HMAccessoryCategory = interface(NSObject)
    ['{D330EAC7-A3A8-422C-AAD0-D0E21CAEAE44}']
    function categoryType: NSString; cdecl;
    function localizedDescription: NSString; cdecl;
  end;
  THMAccessoryCategory = class(TOCGenericImport<HMAccessoryCategoryClass, HMAccessoryCategory>) end;

  HMEventClass = interface(NSObjectClass)
    ['{C998D252-EE48-4296-82D6-0A411BBDBB0E}']
    {class} function isSupportedForHome(home: HMHome): Boolean; cdecl;
  end;

  HMEvent = interface(NSObject)
    ['{1AF3300B-0218-4423-A59C-E443A10D899A}']
    function uniqueIdentifier: NSUUID; cdecl;
  end;
  THMEvent = class(TOCGenericImport<HMEventClass, HMEvent>) end;

  HMCharacteristicEventClass = interface(HMEventClass)
    ['{9A6527C3-5172-474D-83F1-285F5E23D1C5}']
  end;

  HMCharacteristicEvent = interface(HMEvent)
    ['{E5766DAC-D39F-4793-854E-0E8EE3EF1D74}']
    function characteristic: HMCharacteristic; cdecl;
    function initWithCharacteristic(characteristic: HMCharacteristic; triggerValue: TriggerValueType): Pointer; cdecl;
    function triggerValue: TriggerValueType; cdecl;
    procedure updateTriggerValue(triggerValue: TriggerValueType; completionHandler: THMCharacteristicEventBlockMethod1); cdecl; // API_DEPRECATED("No longer supported.", ios(9.0, 11.0))
  end;
  THMCharacteristicEvent = class(TOCGenericImport<HMCharacteristicEventClass, HMCharacteristicEvent>) end;

  HMMutableCharacteristicEventClass = interface(HMCharacteristicEventClass)
    ['{362947E6-6054-4B7C-8FAB-851A0078A06C}']
  end;

  HMMutableCharacteristicEvent = interface(HMCharacteristicEvent)
    ['{DD2B2E17-1D6B-473B-A6CA-89775EA7EB4E}']
    function characteristic: HMCharacteristic; cdecl;
    procedure setCharacteristic(characteristic: HMCharacteristic); cdecl;
    procedure setTriggerValue(triggerValue: TriggerValueType); cdecl;
    function triggerValue: TriggerValueType; cdecl;
  end;
  THMMutableCharacteristicEvent = class(TOCGenericImport<HMMutableCharacteristicEventClass, HMMutableCharacteristicEvent>) end;

  HMLocationEventClass = interface(HMEventClass)
    ['{5913ED0F-C6F3-451F-905C-34B4ACDBA3D9}']
  end;

  HMLocationEvent = interface(HMEvent)
    ['{0FFF9CD7-F25B-465D-BDF4-DBF80A2AEF0F}']
    function initWithRegion(region: CLRegion): Pointer; cdecl;
    function region: CLRegion; cdecl;
    procedure updateRegion(region: CLRegion; completionHandler: THMLocationEventBlockMethod1); cdecl; // API_DEPRECATED("No longer supported.", ios(9.0, 11.0))
  end;
  THMLocationEvent = class(TOCGenericImport<HMLocationEventClass, HMLocationEvent>) end;

  HMMutableLocationEventClass = interface(HMLocationEventClass)
    ['{8AC7ED67-1AE0-48C8-963E-FBF747D0B89E}']
  end;

  HMMutableLocationEvent = interface(HMLocationEvent)
    ['{6F067B53-D9C6-4C75-8D74-45368B5A3BEC}']
    function region: CLRegion; cdecl;
    procedure setRegion(region: CLRegion); cdecl;
  end;
  THMMutableLocationEvent = class(TOCGenericImport<HMMutableLocationEventClass, HMMutableLocationEvent>) end;

  HMTimeEventClass = interface(HMEventClass)
    ['{0DE83C20-2052-40D8-B787-C70DB5A71972}']
  end;

  HMTimeEvent = interface(HMEvent)
    ['{A991E8AD-46DE-41A5-AFCC-ED0C9CA5FDAC}']
  end;
  THMTimeEvent = class(TOCGenericImport<HMTimeEventClass, HMTimeEvent>) end;

  HMCalendarEventClass = interface(HMTimeEventClass)
    ['{4F5BB34F-0EEE-4DB5-81F0-2FB8D9A7B066}']
  end;

  HMCalendarEvent = interface(HMTimeEvent)
    ['{F550749F-7BBC-40DA-92E7-9C4D44834E5B}']
    function fireDateComponents: NSDateComponents; cdecl;
    function initWithFireDateComponents(fireDateComponents: NSDateComponents): Pointer; cdecl;
  end;
  THMCalendarEvent = class(TOCGenericImport<HMCalendarEventClass, HMCalendarEvent>) end;

  HMMutableCalendarEventClass = interface(HMCalendarEventClass)
    ['{B9DA2BA1-957C-4992-A5FD-79A5B111CD28}']
  end;

  HMMutableCalendarEvent = interface(HMCalendarEvent)
    ['{2C652B31-4912-4900-8B21-22A36018C330}']
    function fireDateComponents: NSDateComponents; cdecl;
    procedure setFireDateComponents(fireDateComponents: NSDateComponents); cdecl;
  end;
  THMMutableCalendarEvent = class(TOCGenericImport<HMMutableCalendarEventClass, HMMutableCalendarEvent>) end;

  HMSignificantTimeEventClass = interface(HMTimeEventClass)
    ['{6FBAFF71-4F16-4408-8339-743DF662538C}']
  end;

  HMSignificantTimeEvent = interface(HMTimeEvent)
    ['{5B3E7015-67E4-4925-8385-FCCD79D390A2}']
    function initWithSignificantEvent(significantEvent: HMSignificantEvent; offset: NSDateComponents): Pointer; cdecl;
    function offset: NSDateComponents; cdecl;
    function significantEvent: HMSignificantEvent; cdecl;
  end;
  THMSignificantTimeEvent = class(TOCGenericImport<HMSignificantTimeEventClass, HMSignificantTimeEvent>) end;

  HMMutableSignificantTimeEventClass = interface(HMSignificantTimeEventClass)
    ['{F76C2697-8B0E-4748-BBCF-382EB02E20C7}']
  end;

  HMMutableSignificantTimeEvent = interface(HMSignificantTimeEvent)
    ['{88AA3D26-C222-4014-B664-FDC195E10640}']
    function offset: NSDateComponents; cdecl;
    procedure setOffset(offset: NSDateComponents); cdecl;
    procedure setSignificantEvent(significantEvent: HMSignificantEvent); cdecl;
    function significantEvent: HMSignificantEvent; cdecl;
  end;
  THMMutableSignificantTimeEvent = class(TOCGenericImport<HMMutableSignificantTimeEventClass, HMMutableSignificantTimeEvent>) end;

  HMEventTriggerClass = interface(HMTriggerClass)
    ['{FBEF0001-755C-4432-B1E8-61A50195763F}']
    {class} function predicateForEvaluatingTriggerOccurringAfterDateWithComponents(dateComponents: NSDateComponents): NSPredicate; cdecl;
    {class} function predicateForEvaluatingTriggerOccurringAfterSignificantEvent(significantEvent: NSString;
      applyingOffset: NSDateComponents): NSPredicate; overload; cdecl; // API_DEPRECATED("Use predicateForEvaluatingTriggerOccurringAfterSignificantEvent: instead", ios(9.0, 11.0))
    {class} function predicateForEvaluatingTriggerOccurringAfterSignificantEvent(significantEvent: HMSignificantTimeEvent): NSPredicate; overload; cdecl;
    {class} function predicateForEvaluatingTriggerOccurringBeforeDateWithComponents(dateComponents: NSDateComponents): NSPredicate; cdecl;
    {class} function predicateForEvaluatingTriggerOccurringBeforeSignificantEvent(significantEvent: NSString;
      applyingOffset: NSDateComponents): NSPredicate; overload; cdecl; // API_DEPRECATED("Use predicateForEvaluatingTriggerOccurringBeforeSignificantEvent: instead", ios(9.0, 11.0))
    {class} function predicateForEvaluatingTriggerOccurringBeforeSignificantEvent(significantEvent: HMSignificantTimeEvent): NSPredicate; overload; cdecl;
    {class} function predicateForEvaluatingTriggerOccurringBetweenDateWithComponents(firstDateComponents: NSDateComponents;
      secondDateWithComponents: NSDateComponents): NSPredicate; cdecl;
    {class} function predicateForEvaluatingTriggerOccurringBetweenSignificantEvent(firstSignificantEvent: HMSignificantTimeEvent;
      secondSignificantEvent: HMSignificantTimeEvent): NSPredicate; cdecl;
    {class} function predicateForEvaluatingTriggerOccurringOnDateWithComponents(dateComponents: NSDateComponents): NSPredicate; cdecl;
    {class} function predicateForEvaluatingTriggerWithCharacteristic(characteristic: HMCharacteristic; relatedBy: NSPredicateOperatorType;
      toValue: Pointer): NSPredicate; cdecl;
    {class} function predicateForEvaluatingTriggerWithPresence(presenceEvent: HMPresenceEvent): NSPredicate; cdecl;
  end;

  HMEventTrigger = interface(HMTrigger)
    ['{780A553C-0A6A-478E-B58B-15702FFF1230}']
    procedure addEvent(event: HMEvent; completionHandler: THMEventTriggerBlockMethod1); cdecl; // API_DEPRECATED("Use updateEvents:completionHandler: instead", ios(9.0, 11.0))
    function endEvents: NSArray; cdecl;
    function events: NSArray; cdecl;
    function executeOnce: Boolean; cdecl;
    function initWithName(name: NSString; events: NSArray; endEvents: NSArray; recurrences: NSArray; predicate: NSPredicate): Pointer; overload; cdecl;
    function initWithName(name: NSString; events: NSArray; predicate: NSPredicate): Pointer; overload; cdecl;
    function predicate: NSPredicate; cdecl;
    function recurrences: NSArray; cdecl;
    procedure removeEvent(event: HMEvent; completionHandler: THMEventTriggerBlockMethod1); cdecl; // API_DEPRECATED("Use updateEvents:completionHandler: instead", ios(9.0, 11.0))
    function triggerActivationState: HMEventTriggerActivationState; cdecl;
    procedure updateEndEvents(endEvents: NSArray; completionHandler: THMEventTriggerBlockMethod1); cdecl;
    procedure updateEvents(events: NSArray; completionHandler: THMEventTriggerBlockMethod1); cdecl;
    procedure updateExecuteOnce(executeOnce: Boolean; completionHandler: THMEventTriggerBlockMethod1); cdecl;
    procedure updatePredicate(predicate: NSPredicate; completionHandler: THMEventTriggerBlockMethod1); cdecl;
    procedure updateRecurrences(recurrences: NSArray; completionHandler: THMEventTriggerBlockMethod1); cdecl;
  end;
  THMEventTrigger = class(TOCGenericImport<HMEventTriggerClass, HMEventTrigger>) end;

  HMAccessoryProfileClass = interface(NSObjectClass)
    ['{A4E53CE2-F130-4BF1-9BE7-8A7DD6C01BEC}']
  end;

  HMAccessoryProfile = interface(NSObject)
    ['{3106B562-D9B2-4148-AC69-7FCEE0258E08}']
    function accessory: HMAccessory; cdecl;
    function services: NSArray; cdecl;
    function uniqueIdentifier: NSUUID; cdecl;
  end;
  THMAccessoryProfile = class(TOCGenericImport<HMAccessoryProfileClass, HMAccessoryProfile>) end;

  HMDurationEventClass = interface(HMTimeEventClass)
    ['{021C111C-18BE-4F5A-8F6C-B06C98EABEF3}']
  end;

  HMDurationEvent = interface(HMTimeEvent)
    ['{E92FF627-9272-4068-BD3C-5C5BE9ED10D5}']
    function duration: NSTimeInterval; cdecl;
    function initWithDuration(duration: NSTimeInterval): Pointer; cdecl;
  end;
  THMDurationEvent = class(TOCGenericImport<HMDurationEventClass, HMDurationEvent>) end;

  HMMutableDurationEventClass = interface(HMDurationEventClass)
    ['{50434E49-DF7C-4FD6-960B-4DC5A465B589}']
  end;

  HMMutableDurationEvent = interface(HMDurationEvent)
    ['{CE248DBF-AC05-4375-AE06-8179C3073C52}']
    function duration: NSTimeInterval; cdecl;
    procedure setDuration(duration: NSTimeInterval); cdecl;
  end;
  THMMutableDurationEvent = class(TOCGenericImport<HMMutableDurationEventClass, HMMutableDurationEvent>) end;

  HMCharacteristicThresholdRangeEventClass = interface(HMEventClass)
    ['{355D8157-1B36-4BF0-8EDB-A040238604CD}']
  end;

  HMCharacteristicThresholdRangeEvent = interface(HMEvent)
    ['{1E72ABB0-0BA3-46F8-9363-FC971C1137EA}']
    function characteristic: HMCharacteristic; cdecl;
    function initWithCharacteristic(characteristic: HMCharacteristic; thresholdRange: HMNumberRange): Pointer; cdecl;
    function thresholdRange: HMNumberRange; cdecl;
  end;
  THMCharacteristicThresholdRangeEvent = class(TOCGenericImport<HMCharacteristicThresholdRangeEventClass, HMCharacteristicThresholdRangeEvent>) end;

  HMMutableCharacteristicThresholdRangeEventClass = interface(HMCharacteristicThresholdRangeEventClass)
    ['{0E56D5B4-EC8D-4CF3-B896-DEB8384044E5}']
  end;

  HMMutableCharacteristicThresholdRangeEvent = interface(HMCharacteristicThresholdRangeEvent)
    ['{5CEABB10-511B-4DF4-9D90-93002BA47E6C}']
    function characteristic: HMCharacteristic; cdecl;
    procedure setCharacteristic(characteristic: HMCharacteristic); cdecl;
    procedure setThresholdRange(thresholdRange: HMNumberRange); cdecl;
    function thresholdRange: HMNumberRange; cdecl;
  end;
  THMMutableCharacteristicThresholdRangeEvent = class(TOCGenericImport<HMMutableCharacteristicThresholdRangeEventClass,
    HMMutableCharacteristicThresholdRangeEvent>) end;

  HMPresenceEventClass = interface(HMEventClass)
    ['{DA2506C3-1F28-46BB-A35C-F8A9A193F8C9}']
  end;

  HMPresenceEvent = interface(HMEvent)
    ['{87C0306B-5B4F-4DE0-8A11-015D682F4BD3}']
    function initWithPresenceEventType(presenceEventType: HMPresenceEventType; presenceUserType: HMPresenceEventUserType): Pointer; cdecl;
    function presenceEventType: HMPresenceEventType; cdecl;
    function presenceUserType: HMPresenceEventUserType; cdecl;
  end;
  THMPresenceEvent = class(TOCGenericImport<HMPresenceEventClass, HMPresenceEvent>) end;

  HMMutablePresenceEventClass = interface(HMPresenceEventClass)
    ['{811C79AF-1EB5-46C4-9B0A-BC3BA9AD7AFB}']
  end;

  HMMutablePresenceEvent = interface(HMPresenceEvent)
    ['{48D42466-39C2-450D-B608-3BA00BBE927C}']
    function presenceEventType: HMPresenceEventType; cdecl;
    function presenceUserType: HMPresenceEventUserType; cdecl;
    procedure setPresenceEventType(presenceEventType: HMPresenceEventType); cdecl;
    procedure setPresenceUserType(presenceUserType: HMPresenceEventUserType); cdecl;
  end;
  THMMutablePresenceEvent = class(TOCGenericImport<HMMutablePresenceEventClass, HMMutablePresenceEvent>) end;

  HMNumberRangeClass = interface(NSObjectClass)
    ['{9E05F7E2-E06E-464B-8935-FC1FCEC90EF2}']
    {class} function numberRangeWithMaxValue(maxValue: NSNumber): Pointer; cdecl;
    {class} function numberRangeWithMinValue(minValue: NSNumber): Pointer; overload; cdecl;
    {class} function numberRangeWithMinValue(minValue: NSNumber; maxValue: NSNumber): Pointer; overload; cdecl;
  end;

  HMNumberRange = interface(NSObject)
    ['{4D2A7A8B-B5B6-482B-9559-D4E1DA7E0AB7}']
    function maxValue: NSNumber; cdecl;
    function minValue: NSNumber; cdecl;
  end;
  THMNumberRange = class(TOCGenericImport<HMNumberRangeClass, HMNumberRange>) end;

  HMAddAccessoryRequestClass = interface(NSObjectClass)
    ['{32E67390-55CE-413C-A476-2C31E30D7744}']
  end;

  HMAddAccessoryRequest = interface(NSObject)
    ['{682215F9-F2A1-4028-9025-3F789DF84A79}']
    function accessoryCategory: HMAccessoryCategory; cdecl;
    function accessoryName: NSString; cdecl;
    function home: HMHome; cdecl;
    function payloadWithOwnershipToken(ownershipToken: HMAccessoryOwnershipToken): HMAccessorySetupPayload; cdecl;
    function payloadWithURL(setupPayloadURL: NSURL; ownershipToken: HMAccessoryOwnershipToken): HMAccessorySetupPayload; cdecl;
    function requiresOwnershipToken: Boolean; cdecl; // API_DEPRECATED("No longer supported", ios(13.0, 13.0))
    function requiresSetupPayloadURL: Boolean; cdecl;
  end;
  THMAddAccessoryRequest = class(TOCGenericImport<HMAddAccessoryRequestClass, HMAddAccessoryRequest>) end;

  HMNetworkConfigurationProfileClass = interface(HMAccessoryProfileClass)
    ['{3A95BB83-826C-4D76-AFBB-95B2A1D9738B}']
  end;

  HMNetworkConfigurationProfile = interface(HMAccessoryProfile)
    ['{9B50C831-014B-45CE-A377-E9C565EDD3FF}']
    function delegate: Pointer; cdecl;
    function isNetworkAccessRestricted: Boolean; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  THMNetworkConfigurationProfile = class(TOCGenericImport<HMNetworkConfigurationProfileClass, HMNetworkConfigurationProfile>) end;

  HMNetworkConfigurationProfileDelegate = interface(IObjectiveC)
    ['{5E9B314F-85BE-4DDE-8A0E-4B75969512EC}']
    procedure profileDidUpdateNetworkAccessMode(profile: HMNetworkConfigurationProfile); cdecl;
  end;

  HMCameraViewClass = interface(UIViewClass)
    ['{273C035C-E192-477A-B1CC-5FEED9DC345F}']
  end;

  HMCameraView = interface(UIView)
    ['{04C1BC1C-608F-4FC2-B647-E8349F11B502}']
    function cameraSource: HMCameraSource; cdecl;
    procedure setCameraSource(cameraSource: HMCameraSource); cdecl;
  end;
  THMCameraView = class(TOCGenericImport<HMCameraViewClass, HMCameraView>) end;

  HMCameraProfileClass = interface(HMAccessoryProfileClass)
    ['{AE524F47-483E-442F-B825-355FD7364BC9}']
  end;

  HMCameraProfile = interface(HMAccessoryProfile)
    ['{B797D105-E027-41D0-AEDA-37E49927C22B}']
    function microphoneControl: HMCameraAudioControl; cdecl;
    function settingsControl: HMCameraSettingsControl; cdecl;
    function snapshotControl: HMCameraSnapshotControl; cdecl;
    function speakerControl: HMCameraAudioControl; cdecl;
    function streamControl: HMCameraStreamControl; cdecl;
  end;
  THMCameraProfile = class(TOCGenericImport<HMCameraProfileClass, HMCameraProfile>) end;

  HMCameraControlClass = interface(NSObjectClass)
    ['{EF4E2BF2-1DD1-4456-BE65-DF46B070808A}']
  end;

  HMCameraControl = interface(NSObject)
    ['{B1A552BA-579A-4448-9D51-CEC763CC9E78}']
  end;
  THMCameraControl = class(TOCGenericImport<HMCameraControlClass, HMCameraControl>) end;

  HMCameraStreamControlClass = interface(HMCameraControlClass)
    ['{C24854D0-3BE0-4140-8563-DD5EDC79E460}']
  end;

  HMCameraStreamControl = interface(HMCameraControl)
    ['{A1EBA0C4-FDA5-4148-86F9-5696738E8C1C}']
    function cameraStream: HMCameraStream; cdecl;
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure startStream; cdecl;
    procedure stopStream; cdecl;
    function streamState: HMCameraStreamState; cdecl;
  end;
  THMCameraStreamControl = class(TOCGenericImport<HMCameraStreamControlClass, HMCameraStreamControl>) end;

  HMCameraStreamControlDelegate = interface(IObjectiveC)
    ['{B06D822F-1681-438A-824F-EE6F4FD83BF1}']
    procedure cameraStreamControl(cameraStreamControl: HMCameraStreamControl; didStopStreamWithError: NSError); cdecl;
    procedure cameraStreamControlDidStartStream(cameraStreamControl: HMCameraStreamControl); cdecl;
  end;

  HMCameraSourceClass = interface(NSObjectClass)
    ['{43C908B1-A46C-4F15-8A69-C5F0DAA4E43D}']
  end;

  HMCameraSource = interface(NSObject)
    ['{81F4A9B3-EB14-4D54-A6B2-3DECF590C636}']
    function aspectRatio: Double; cdecl;
  end;
  THMCameraSource = class(TOCGenericImport<HMCameraSourceClass, HMCameraSource>) end;

  HMCameraStreamClass = interface(HMCameraSourceClass)
    ['{FD8ACA60-9C76-4E50-BA08-2596127B28CD}']
  end;

  HMCameraStream = interface(HMCameraSource)
    ['{405ECBFC-D376-42D2-BFBB-AEFFF54244B3}']
    function audioStreamSetting: HMCameraAudioStreamSetting; cdecl;
    procedure setAudioStreamSetting(audioStreamSetting: HMCameraAudioStreamSetting); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("updateAudioStreamSetting:completionHandler:", ios(10.0, 10.0), watchos(3.0, 3.0))
    procedure updateAudioStreamSetting(audioStreamSetting: HMCameraAudioStreamSetting; completionHandler: THMCameraStreamBlockMethod1); cdecl;
  end;
  THMCameraStream = class(TOCGenericImport<HMCameraStreamClass, HMCameraStream>) end;

  HMCameraSnapshotControlClass = interface(HMCameraControlClass)
    ['{D3811400-1C2F-4E12-A0FA-E9424A6DB23F}']
  end;

  HMCameraSnapshotControl = interface(HMCameraControl)
    ['{1589625E-2031-405D-9DA6-3DCB9E90F9A0}']
    function delegate: Pointer; cdecl;
    function mostRecentSnapshot: HMCameraSnapshot; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure takeSnapshot; cdecl;
  end;
  THMCameraSnapshotControl = class(TOCGenericImport<HMCameraSnapshotControlClass, HMCameraSnapshotControl>) end;

  HMCameraSnapshotControlDelegate = interface(IObjectiveC)
    ['{E068276A-EEEB-482B-A623-689191E84011}']
    procedure cameraSnapshotControl(cameraSnapshotControl: HMCameraSnapshotControl; didTakeSnapshot: HMCameraSnapshot; error: NSError); cdecl;
    procedure cameraSnapshotControlDidUpdateMostRecentSnapshot(cameraSnapshotControl: HMCameraSnapshotControl); cdecl;
  end;

  HMCameraSnapshotClass = interface(HMCameraSourceClass)
    ['{8BA8E972-3F3D-435E-9A41-E1F7D560E9A8}']
  end;

  HMCameraSnapshot = interface(HMCameraSource)
    ['{3667CD98-6E2C-4943-884D-01D469CB5C5C}']
    function captureDate: NSDate; cdecl;
  end;
  THMCameraSnapshot = class(TOCGenericImport<HMCameraSnapshotClass, HMCameraSnapshot>) end;

  HMCameraSettingsControlClass = interface(HMCameraControlClass)
    ['{C3395999-DBAF-43A8-AC8F-4F9ABE4A640D}']
  end;

  HMCameraSettingsControl = interface(HMCameraControl)
    ['{43D76B39-49D5-4B47-8BD2-DDE70FCFA173}']
    function currentHorizontalTilt: HMCharacteristic; cdecl;
    function currentVerticalTilt: HMCharacteristic; cdecl;
    function digitalZoom: HMCharacteristic; cdecl;
    function imageMirroring: HMCharacteristic; cdecl;
    function imageRotation: HMCharacteristic; cdecl;
    function nightVision: HMCharacteristic; cdecl;
    function opticalZoom: HMCharacteristic; cdecl;
    function targetHorizontalTilt: HMCharacteristic; cdecl;
    function targetVerticalTilt: HMCharacteristic; cdecl;
  end;
  THMCameraSettingsControl = class(TOCGenericImport<HMCameraSettingsControlClass, HMCameraSettingsControl>) end;

  HMCameraAudioControlClass = interface(HMCameraControlClass)
    ['{0B5D8339-0B9F-4495-80F7-972B8C3EF09A}']
  end;

  HMCameraAudioControl = interface(HMCameraControl)
    ['{727BB4F2-D1CD-41B1-BD25-404DE5A41B2A}']
    function mute: HMCharacteristic; cdecl;
    function volume: HMCharacteristic; cdecl;
  end;
  THMCameraAudioControl = class(TOCGenericImport<HMCameraAudioControlClass, HMCameraAudioControl>) end;

  HMAccessorySetupManagerClass = interface(NSObjectClass)
    ['{28FD1D04-695C-43CF-A7E9-09018572A887}']
  end;

  HMAccessorySetupManager = interface(NSObject)
    ['{0611BBFA-2976-430C-A879-45B19801BFB3}']
    procedure addAndSetUpAccessoriesForTopology(topology: HMMatterTopology; completionHandler: HMErrorBlock); cdecl; // API_DEPRECATED("Use -[HMAccessorySetupManager performAccessorySetupUsingRequest:topology:completionHandler:] instead", ios(15.0, 15.4))
    procedure performAccessorySetupUsingRequest(request: HMAccessorySetupRequest; completionHandler: THMAccessorySetupManagerBlockMethod1); cdecl;
    procedure performMatterEcosystemAccessorySetupUsingRequest(request: HMAccessorySetupRequest; topology: HMMatterTopology;
      completionHandler: HMErrorBlock); cdecl;
  end;
  THMAccessorySetupManager = class(TOCGenericImport<HMAccessorySetupManagerClass, HMAccessorySetupManager>) end;

  HMAccessoryOwnershipTokenClass = interface(NSObjectClass)
    ['{E0520F41-E099-4341-B283-B5D7E601FC9B}']
    {class} function new: Pointer; cdecl;
  end;

  HMAccessoryOwnershipToken = interface(NSObject)
    ['{936B4997-3135-4A02-8238-F184E4186D71}']
    function initWithData(data: NSData): Pointer; cdecl;
  end;
  THMAccessoryOwnershipToken = class(TOCGenericImport<HMAccessoryOwnershipTokenClass, HMAccessoryOwnershipToken>) end;

  HMAccessorySetupPayloadClass = interface(NSObjectClass)
    ['{6A83FCE3-1869-4E26-85B4-DCB7943078D3}']
    {class} function new: Pointer; cdecl;
  end;

  HMAccessorySetupPayload = interface(NSObject)
    ['{0CAD7DE6-2378-4908-8A6F-4A8F00E97B90}']
    function initWithURL(setupPayloadURL: NSURL): Pointer; overload; cdecl;
    function initWithURL(setupPayloadURL: NSURL; ownershipToken: HMAccessoryOwnershipToken): Pointer; overload; cdecl;
  end;
  THMAccessorySetupPayload = class(TOCGenericImport<HMAccessorySetupPayloadClass, HMAccessorySetupPayload>) end;

  HMAccessorySetupRequestClass = interface(NSObjectClass)
    ['{53E39EBB-4FD9-4229-B474-A0AB86812A56}']
  end;

  HMAccessorySetupRequest = interface(NSObject)
    ['{329DDA09-AD2E-4DF0-AC23-2E9FD1E79038}']
    function homeUniqueIdentifier: NSUUID; cdecl;
    function payload: HMAccessorySetupPayload; cdecl;
    procedure setHomeUniqueIdentifier(homeUniqueIdentifier: NSUUID); cdecl;
    procedure setPayload(payload: HMAccessorySetupPayload); cdecl;
    procedure setSuggestedAccessoryName(suggestedAccessoryName: NSString); cdecl;
    procedure setSuggestedRoomUniqueIdentifier(suggestedRoomUniqueIdentifier: NSUUID); cdecl;
    function suggestedAccessoryName: NSString; cdecl;
    function suggestedRoomUniqueIdentifier: NSUUID; cdecl;
  end;
  THMAccessorySetupRequest = class(TOCGenericImport<HMAccessorySetupRequestClass, HMAccessorySetupRequest>) end;

  HMAccessorySetupResultClass = interface(NSObjectClass)
    ['{09DDB0BC-D864-458A-B081-FA709D64FC1F}']
    {class} function new: Pointer; cdecl;
  end;

  HMAccessorySetupResult = interface(NSObject)
    ['{C343C3AA-C2C9-4D8A-9BD9-1BBFC53FEC4F}']
    function accessoryUniqueIdentifiers: NSArray; cdecl;
    function homeUniqueIdentifier: NSUUID; cdecl;
  end;
  THMAccessorySetupResult = class(TOCGenericImport<HMAccessorySetupResultClass, HMAccessorySetupResult>) end;

  HMMatterHomeClass = interface(NSObjectClass)
    ['{194CE856-C119-446F-BBD7-19A68E667787}']
    {class} function new: Pointer; cdecl;
  end;

  HMMatterHome = interface(NSObject)
    ['{79754D5D-F87E-48B5-9F7F-A739EC3CFB28}']
    function initWithUUID(uuid: NSUUID; name: NSString): Pointer; cdecl;
    function name: NSString; cdecl;
    function uuid: NSUUID; cdecl;
  end;
  THMMatterHome = class(TOCGenericImport<HMMatterHomeClass, HMMatterHome>) end;

  HMMatterRequestHandlerClass = interface(NSObjectClass)
    ['{61A0E19E-B645-405D-A37D-8F311A5029D1}']
  end;

  HMMatterRequestHandler = interface(NSObject)
    ['{2A271A24-F583-47B1-B7A9-E5E624057273}']
    procedure configureAccessoryWithName(accessoryName: NSString; room: HMMatterRoom; completion: THMMatterRequestHandlerBlockMethod2); cdecl;
    procedure fetchRoomsInHome(home: HMMatterHome; completion: THMMatterRequestHandlerBlockMethod1); cdecl;
    procedure pairAccessoryInHome(home: HMMatterHome; onboardingPayload: NSString; completion: THMMatterRequestHandlerBlockMethod2); cdecl;
  end;
  THMMatterRequestHandler = class(TOCGenericImport<HMMatterRequestHandlerClass, HMMatterRequestHandler>) end;

  HMMatterRoomClass = interface(NSObjectClass)
    ['{BC992CA2-886B-4C92-9764-552A12B21B71}']
    {class} function new: Pointer; cdecl;
  end;

  HMMatterRoom = interface(NSObject)
    ['{5188582B-C918-4C5D-957F-C86525110564}']
    function initWithUUID(uuid: NSUUID; name: NSString): Pointer; cdecl;
    function name: NSString; cdecl;
    function uuid: NSUUID; cdecl;
  end;
  THMMatterRoom = class(TOCGenericImport<HMMatterRoomClass, HMMatterRoom>) end;

  HMMatterTopologyClass = interface(NSObjectClass)
    ['{9ABFD874-C729-418B-9292-AED343E80B18}']
    {class} function new: Pointer; cdecl;
  end;

  HMMatterTopology = interface(NSObject)
    ['{D33617A4-E3CF-47A3-83D8-9AC8A2F74E55}']
    function homes: NSArray; cdecl;
    function initWithHomes(homes: NSArray): Pointer; cdecl;
  end;
  THMMatterTopology = class(TOCGenericImport<HMMatterTopologyClass, HMMatterTopology>) end;

function HMUserFailedAccessoriesKey: NSString;
function HMServiceTypeSwitch: NSString;
function HMServiceTypeThermostat: NSString;
function HMServiceTypeOutlet: NSString;
function HMServiceTypeLockManagement: NSString;
function HMServiceTypeAirQualitySensor: NSString;
function HMServiceTypeCarbonDioxideSensor: NSString;
function HMServiceTypeCarbonMonoxideSensor: NSString;
function HMServiceTypeContactSensor: NSString;
function HMServiceTypeDoor: NSString;
function HMServiceTypeHumiditySensor: NSString;
function HMServiceTypeLeakSensor: NSString;
function HMServiceTypeLightSensor: NSString;
function HMServiceTypeMotionSensor: NSString;
function HMServiceTypeOccupancySensor: NSString;
function HMServiceTypeSecuritySystem: NSString;
function HMServiceTypeStatefulProgrammableSwitch: NSString;
function HMServiceTypeStatelessProgrammableSwitch: NSString;
function HMServiceTypeSmokeSensor: NSString;
function HMServiceTypeTemperatureSensor: NSString;
function HMServiceTypeWindow: NSString;
function HMServiceTypeWindowCovering: NSString;
function HMServiceTypeCameraRTPStreamManagement: NSString;
function HMServiceTypeCameraControl: NSString;
function HMServiceTypeMicrophone: NSString;
function HMServiceTypeSpeaker: NSString;
function HMServiceTypeAirPurifier: NSString;
function HMServiceTypeFilterMaintenance: NSString;
function HMServiceTypeSlats: NSString;
function HMServiceTypeLabel: NSString;
function HMServiceTypeIrrigationSystem: NSString;
function HMServiceTypeValve: NSString;
function HMServiceTypeFaucet: NSString;
function HMServiceTypeAccessoryInformation: NSString;
function HMServiceTypeFan: NSString;
function HMServiceTypeGarageDoorOpener: NSString;
function HMServiceTypeLightbulb: NSString;
function HMServiceTypeLockMechanism: NSString;
function HMServiceTypeBattery: NSString;
function HMServiceTypeVentilationFan: NSString;
function HMServiceTypeHeaterCooler: NSString;
function HMServiceTypeHumidifierDehumidifier: NSString;
function HMServiceTypeDoorbell: NSString;
function HMCharacteristicPropertySupportsEventNotification: NSString;
function HMCharacteristicPropertyReadable: NSString;
function HMCharacteristicPropertyWritable: NSString;
function HMCharacteristicPropertyHidden: NSString;
function HMCharacteristicTypeTargetRelativeHumidity: NSString;
function HMCharacteristicTypeOutletInUse: NSString;
function HMCharacteristicTypeLogs: NSString;
function HMCharacteristicTypeAudioFeedback: NSString;
function HMCharacteristicTypeAdminOnlyAccess: NSString;
function HMCharacteristicTypeSecuritySystemAlarmType: NSString;
function HMCharacteristicTypeMotionDetected: NSString;
function HMCharacteristicTypeLockMechanismLastKnownAction: NSString;
function HMCharacteristicTypeLockManagementControlPoint: NSString;
function HMCharacteristicTypeLockManagementAutoSecureTimeout: NSString;
function HMCharacteristicTypeAirParticulateDensity: NSString;
function HMCharacteristicTypeAirParticulateSize: NSString;
function HMCharacteristicTypeAirQuality: NSString;
function HMCharacteristicTypeCarbonDioxideDetected: NSString;
function HMCharacteristicTypeCarbonDioxideLevel: NSString;
function HMCharacteristicTypeCarbonDioxidePeakLevel: NSString;
function HMCharacteristicTypeCarbonMonoxideDetected: NSString;
function HMCharacteristicTypeCarbonMonoxideLevel: NSString;
function HMCharacteristicTypeCarbonMonoxidePeakLevel: NSString;
function HMCharacteristicTypeContactState: NSString;
function HMCharacteristicTypeCurrentHorizontalTilt: NSString;
function HMCharacteristicTypeCurrentPosition: NSString;
function HMCharacteristicTypeCurrentSecuritySystemState: NSString;
function HMCharacteristicTypeCurrentVerticalTilt: NSString;
function HMCharacteristicTypeHoldPosition: NSString;
function HMCharacteristicTypeLeakDetected: NSString;
function HMCharacteristicTypeOccupancyDetected: NSString;
function HMCharacteristicTypeOutputState: NSString;
function HMCharacteristicTypePositionState: NSString;
function HMCharacteristicTypeStatusActive: NSString;
function HMCharacteristicTypeStatusFault: NSString;
function HMCharacteristicTypeStatusJammed: NSString;
function HMCharacteristicTypeStatusTampered: NSString;
function HMCharacteristicTypeTargetHorizontalTilt: NSString;
function HMCharacteristicTypeTargetSecuritySystemState: NSString;
function HMCharacteristicTypeTargetPosition: NSString;
function HMCharacteristicTypeTargetVerticalTilt: NSString;
function HMCharacteristicTypeStreamingStatus: NSString;
function HMCharacteristicTypeSetupStreamEndpoint: NSString;
function HMCharacteristicTypeSupportedVideoStreamConfiguration: NSString;
function HMCharacteristicTypeSupportedRTPConfiguration: NSString;
function HMCharacteristicTypeSelectedStreamConfiguration: NSString;
function HMCharacteristicTypeOpticalZoom: NSString;
function HMCharacteristicTypeDigitalZoom: NSString;
function HMCharacteristicTypeImageRotation: NSString;
function HMCharacteristicTypeImageMirroring: NSString;
function HMCharacteristicTypeLabelNamespace: NSString;
function HMCharacteristicTypeLabelIndex: NSString;
function HMCharacteristicTypeCurrentAirPurifierState: NSString;
function HMCharacteristicTypeTargetAirPurifierState: NSString;
function HMCharacteristicTypeCurrentSlatState: NSString;
function HMCharacteristicTypeFilterChangeIndication: NSString;
function HMCharacteristicTypeFilterLifeLevel: NSString;
function HMCharacteristicTypeFilterResetChangeIndication: NSString;
function HMCharacteristicTypeSlatType: NSString;
function HMCharacteristicTypeCurrentTilt: NSString;
function HMCharacteristicTypeTargetTilt: NSString;
function HMCharacteristicTypeOzoneDensity: NSString;
function HMCharacteristicTypeNitrogenDioxideDensity: NSString;
function HMCharacteristicTypeSulphurDioxideDensity: NSString;
function HMCharacteristicTypePM2_5Density: NSString;
function HMCharacteristicTypePM10Density: NSString;
function HMCharacteristicTypeVolatileOrganicCompoundDensity: NSString;
function HMCharacteristicTypeProgramMode: NSString;
function HMCharacteristicTypeInUse: NSString;
function HMCharacteristicTypeSetDuration: NSString;
function HMCharacteristicTypeRemainingDuration: NSString;
function HMCharacteristicTypeValveType: NSString;
function HMCharacteristicTypeBrightness: NSString;
function HMCharacteristicTypeCoolingThreshold: NSString;
function HMCharacteristicTypeCurrentDoorState: NSString;
function HMCharacteristicTypeCurrentHeatingCooling: NSString;
function HMCharacteristicTypeCurrentRelativeHumidity: NSString;
function HMCharacteristicTypeCurrentTemperature: NSString;
function HMCharacteristicTypeHeatingThreshold: NSString;
function HMCharacteristicTypeHue: NSString;
function HMCharacteristicTypeIdentify: NSString;
function HMCharacteristicTypeCurrentLockMechanismState: NSString;
function HMCharacteristicTypeTargetLockMechanismState: NSString;
function HMCharacteristicTypeManufacturer: NSString;
function HMCharacteristicTypeModel: NSString;
function HMCharacteristicTypeName: NSString;
function HMCharacteristicTypeObstructionDetected: NSString;
function HMCharacteristicTypePowerState: NSString;
function HMCharacteristicTypeRotationDirection: NSString;
function HMCharacteristicTypeRotationSpeed: NSString;
function HMCharacteristicTypeSaturation: NSString;
function HMCharacteristicTypeSerialNumber: NSString;
function HMCharacteristicTypeTargetDoorState: NSString;
function HMCharacteristicTypeTargetHeatingCooling: NSString;
function HMCharacteristicTypeTargetTemperature: NSString;
function HMCharacteristicTypeTemperatureUnits: NSString;
function HMCharacteristicTypeVersion: NSString;
function HMCharacteristicTypeFirmwareVersion: NSString;
function HMCharacteristicTypeHardwareVersion: NSString;
function HMCharacteristicTypeSoftwareVersion: NSString;
function HMCharacteristicTypeBatteryLevel: NSString;
function HMCharacteristicTypeCurrentLightLevel: NSString;
function HMCharacteristicTypeInputEvent: NSString;
function HMCharacteristicTypeSmokeDetected: NSString;
function HMCharacteristicTypeStatusLowBattery: NSString;
function HMCharacteristicTypeChargingState: NSString;
function HMCharacteristicTypeLockPhysicalControls: NSString;
function HMCharacteristicTypeCurrentFanState: NSString;
function HMCharacteristicTypeActive: NSString;
function HMCharacteristicTypeCurrentHeaterCoolerState: NSString;
function HMCharacteristicTypeTargetHeaterCoolerState: NSString;
function HMCharacteristicTypeCurrentHumidifierDehumidifierState: NSString;
function HMCharacteristicTypeTargetHumidifierDehumidifierState: NSString;
function HMCharacteristicTypeWaterLevel: NSString;
function HMCharacteristicTypeSwingMode: NSString;
function HMCharacteristicTypeTargetFanState: NSString;
function HMCharacteristicTypeDehumidifierThreshold: NSString;
function HMCharacteristicTypeHumidifierThreshold: NSString;
function HMCharacteristicTypeColorTemperature: NSString;
function HMCharacteristicTypeIsConfigured: NSString;
function HMCharacteristicTypeSupportedAudioStreamConfiguration: NSString;
function HMCharacteristicTypeVolume: NSString;
function HMCharacteristicTypeMute: NSString;
function HMCharacteristicTypeNightVision: NSString;
function HMCharacteristicMetadataFormatBool: NSString;
function HMCharacteristicMetadataFormatInt: NSString;
function HMCharacteristicMetadataFormatFloat: NSString;
function HMCharacteristicMetadataFormatString: NSString;
function HMCharacteristicMetadataFormatArray: NSString;
function HMCharacteristicMetadataFormatDictionary: NSString;
function HMCharacteristicMetadataFormatUInt8: NSString;
function HMCharacteristicMetadataFormatUInt16: NSString;
function HMCharacteristicMetadataFormatUInt32: NSString;
function HMCharacteristicMetadataFormatUInt64: NSString;
function HMCharacteristicMetadataFormatData: NSString;
function HMCharacteristicMetadataFormatTLV8: NSString;
function HMCharacteristicMetadataUnitsCelsius: NSString;
function HMCharacteristicMetadataUnitsFahrenheit: NSString;
function HMCharacteristicMetadataUnitsPercentage: NSString;
function HMCharacteristicMetadataUnitsArcDegree: NSString;
function HMCharacteristicMetadataUnitsSeconds: NSString;
function HMCharacteristicMetadataUnitsLux: NSString;
function HMCharacteristicMetadataUnitsPartsPerMillion: NSString;
function HMCharacteristicMetadataUnitsMicrogramsPerCubicMeter: NSString;
function HMActionSetTypeWakeUp: NSString;
function HMActionSetTypeSleep: NSString;
function HMActionSetTypeHomeDeparture: NSString;
function HMActionSetTypeHomeArrival: NSString;
function HMActionSetTypeUserDefined: NSString;
function HMActionSetTypeTriggerOwned: NSString;
function HMAccessoryCategoryTypeOther: NSString;
function HMAccessoryCategoryTypeSecuritySystem: NSString;
function HMAccessoryCategoryTypeBridge: NSString;
function HMAccessoryCategoryTypeDoor: NSString;
function HMAccessoryCategoryTypeDoorLock: NSString;
function HMAccessoryCategoryTypeFan: NSString;
function HMAccessoryCategoryTypeGarageDoorOpener: NSString;
function HMAccessoryCategoryTypeIPCamera: NSString;
function HMAccessoryCategoryTypeLightbulb: NSString;
function HMAccessoryCategoryTypeOutlet: NSString;
function HMAccessoryCategoryTypeProgrammableSwitch: NSString;
function HMAccessoryCategoryTypeRangeExtender: NSString;
function HMAccessoryCategoryTypeSensor: NSString;
function HMAccessoryCategoryTypeSwitch: NSString;
function HMAccessoryCategoryTypeThermostat: NSString;
function HMAccessoryCategoryTypeVideoDoorbell: NSString;
function HMAccessoryCategoryTypeWindow: NSString;
function HMAccessoryCategoryTypeWindowCovering: NSString;
function HMAccessoryCategoryTypeAirPurifier: NSString;
function HMAccessoryCategoryTypeAirHeater: NSString;
function HMAccessoryCategoryTypeAirConditioner: NSString;
function HMAccessoryCategoryTypeAirHumidifier: NSString;
function HMAccessoryCategoryTypeAirDehumidifier: NSString;
function HMAccessoryCategoryTypeSprinkler: NSString;
function HMAccessoryCategoryTypeFaucet: NSString;
function HMAccessoryCategoryTypeShowerHead: NSString;
function HMSignificantEventSunrise: HMSignificantEvent;
function HMSignificantEventSunset: HMSignificantEvent;
function HMCharacteristicKeyPath: NSString;
function HMCharacteristicValueKeyPath: NSString;
function HMPresenceKeyPath: NSString;
function HMErrorDomain: NSString;

const
  libHomeKit = '/System/Library/Frameworks/HomeKit.framework/HomeKit';

implementation

uses
  Posix.Dlfcn;

var
  HomeKitModule: THandle;

function HMUserFailedAccessoriesKey: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMUserFailedAccessoriesKey');
end;

function HMServiceTypeSwitch: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeSwitch');
end;

function HMServiceTypeThermostat: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeThermostat');
end;

function HMServiceTypeOutlet: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeOutlet');
end;

function HMServiceTypeLockManagement: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeLockManagement');
end;

function HMServiceTypeAirQualitySensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeAirQualitySensor');
end;

function HMServiceTypeCarbonDioxideSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeCarbonDioxideSensor');
end;

function HMServiceTypeCarbonMonoxideSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeCarbonMonoxideSensor');
end;

function HMServiceTypeContactSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeContactSensor');
end;

function HMServiceTypeDoor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeDoor');
end;

function HMServiceTypeHumiditySensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeHumiditySensor');
end;

function HMServiceTypeLeakSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeLeakSensor');
end;

function HMServiceTypeLightSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeLightSensor');
end;

function HMServiceTypeMotionSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeMotionSensor');
end;

function HMServiceTypeOccupancySensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeOccupancySensor');
end;

function HMServiceTypeSecuritySystem: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeSecuritySystem');
end;

function HMServiceTypeStatefulProgrammableSwitch: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeStatefulProgrammableSwitch');
end;

function HMServiceTypeStatelessProgrammableSwitch: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeStatelessProgrammableSwitch');
end;

function HMServiceTypeSmokeSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeSmokeSensor');
end;

function HMServiceTypeTemperatureSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeTemperatureSensor');
end;

function HMServiceTypeWindow: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeWindow');
end;

function HMServiceTypeWindowCovering: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeWindowCovering');
end;

function HMServiceTypeCameraRTPStreamManagement: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeCameraRTPStreamManagement');
end;

function HMServiceTypeCameraControl: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeCameraControl');
end;

function HMServiceTypeMicrophone: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeMicrophone');
end;

function HMServiceTypeSpeaker: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeSpeaker');
end;

function HMServiceTypeAirPurifier: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeAirPurifier');
end;

function HMServiceTypeFilterMaintenance: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeFilterMaintenance');
end;

function HMServiceTypeSlats: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeSlats');
end;

function HMServiceTypeLabel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeLabel');
end;

function HMServiceTypeIrrigationSystem: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeIrrigationSystem');
end;

function HMServiceTypeValve: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeValve');
end;

function HMServiceTypeFaucet: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeFaucet');
end;

function HMServiceTypeAccessoryInformation: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeAccessoryInformation');
end;

function HMServiceTypeFan: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeFan');
end;

function HMServiceTypeGarageDoorOpener: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeGarageDoorOpener');
end;

function HMServiceTypeLightbulb: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeLightbulb');
end;

function HMServiceTypeLockMechanism: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeLockMechanism');
end;

function HMServiceTypeBattery: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeBattery');
end;

function HMServiceTypeVentilationFan: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeVentilationFan');
end;

function HMServiceTypeHeaterCooler: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeHeaterCooler');
end;

function HMServiceTypeHumidifierDehumidifier: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeHumidifierDehumidifier');
end;

function HMServiceTypeDoorbell: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMServiceTypeDoorbell');
end;

function HMCharacteristicPropertySupportsEventNotification: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicPropertySupportsEventNotification');
end;

function HMCharacteristicPropertyReadable: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicPropertyReadable');
end;

function HMCharacteristicPropertyWritable: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicPropertyWritable');
end;

function HMCharacteristicPropertyHidden: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicPropertyHidden');
end;

function HMCharacteristicTypeTargetRelativeHumidity: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetRelativeHumidity');
end;

function HMCharacteristicTypeOutletInUse: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeOutletInUse');
end;

function HMCharacteristicTypeLogs: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeLogs');
end;

function HMCharacteristicTypeAudioFeedback: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeAudioFeedback');
end;

function HMCharacteristicTypeAdminOnlyAccess: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeAdminOnlyAccess');
end;

function HMCharacteristicTypeSecuritySystemAlarmType: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSecuritySystemAlarmType');
end;

function HMCharacteristicTypeMotionDetected: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeMotionDetected');
end;

function HMCharacteristicTypeLockMechanismLastKnownAction: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeLockMechanismLastKnownAction');
end;

function HMCharacteristicTypeLockManagementControlPoint: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeLockManagementControlPoint');
end;

function HMCharacteristicTypeLockManagementAutoSecureTimeout: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeLockManagementAutoSecureTimeout');
end;

function HMCharacteristicTypeAirParticulateDensity: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeAirParticulateDensity');
end;

function HMCharacteristicTypeAirParticulateSize: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeAirParticulateSize');
end;

function HMCharacteristicTypeAirQuality: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeAirQuality');
end;

function HMCharacteristicTypeCarbonDioxideDetected: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCarbonDioxideDetected');
end;

function HMCharacteristicTypeCarbonDioxideLevel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCarbonDioxideLevel');
end;

function HMCharacteristicTypeCarbonDioxidePeakLevel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCarbonDioxidePeakLevel');
end;

function HMCharacteristicTypeCarbonMonoxideDetected: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCarbonMonoxideDetected');
end;

function HMCharacteristicTypeCarbonMonoxideLevel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCarbonMonoxideLevel');
end;

function HMCharacteristicTypeCarbonMonoxidePeakLevel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCarbonMonoxidePeakLevel');
end;

function HMCharacteristicTypeContactState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeContactState');
end;

function HMCharacteristicTypeCurrentHorizontalTilt: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentHorizontalTilt');
end;

function HMCharacteristicTypeCurrentPosition: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentPosition');
end;

function HMCharacteristicTypeCurrentSecuritySystemState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentSecuritySystemState');
end;

function HMCharacteristicTypeCurrentVerticalTilt: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentVerticalTilt');
end;

function HMCharacteristicTypeHoldPosition: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeHoldPosition');
end;

function HMCharacteristicTypeLeakDetected: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeLeakDetected');
end;

function HMCharacteristicTypeOccupancyDetected: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeOccupancyDetected');
end;

function HMCharacteristicTypeOutputState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeOutputState');
end;

function HMCharacteristicTypePositionState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypePositionState');
end;

function HMCharacteristicTypeStatusActive: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeStatusActive');
end;

function HMCharacteristicTypeStatusFault: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeStatusFault');
end;

function HMCharacteristicTypeStatusJammed: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeStatusJammed');
end;

function HMCharacteristicTypeStatusTampered: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeStatusTampered');
end;

function HMCharacteristicTypeTargetHorizontalTilt: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetHorizontalTilt');
end;

function HMCharacteristicTypeTargetSecuritySystemState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetSecuritySystemState');
end;

function HMCharacteristicTypeTargetPosition: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetPosition');
end;

function HMCharacteristicTypeTargetVerticalTilt: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetVerticalTilt');
end;

function HMCharacteristicTypeStreamingStatus: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeStreamingStatus');
end;

function HMCharacteristicTypeSetupStreamEndpoint: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSetupStreamEndpoint');
end;

function HMCharacteristicTypeSupportedVideoStreamConfiguration: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSupportedVideoStreamConfiguration');
end;

function HMCharacteristicTypeSupportedRTPConfiguration: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSupportedRTPConfiguration');
end;

function HMCharacteristicTypeSelectedStreamConfiguration: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSelectedStreamConfiguration');
end;

function HMCharacteristicTypeOpticalZoom: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeOpticalZoom');
end;

function HMCharacteristicTypeDigitalZoom: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeDigitalZoom');
end;

function HMCharacteristicTypeImageRotation: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeImageRotation');
end;

function HMCharacteristicTypeImageMirroring: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeImageMirroring');
end;

function HMCharacteristicTypeLabelNamespace: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeLabelNamespace');
end;

function HMCharacteristicTypeLabelIndex: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeLabelIndex');
end;

function HMCharacteristicTypeCurrentAirPurifierState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentAirPurifierState');
end;

function HMCharacteristicTypeTargetAirPurifierState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetAirPurifierState');
end;

function HMCharacteristicTypeCurrentSlatState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentSlatState');
end;

function HMCharacteristicTypeFilterChangeIndication: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeFilterChangeIndication');
end;

function HMCharacteristicTypeFilterLifeLevel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeFilterLifeLevel');
end;

function HMCharacteristicTypeFilterResetChangeIndication: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeFilterResetChangeIndication');
end;

function HMCharacteristicTypeSlatType: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSlatType');
end;

function HMCharacteristicTypeCurrentTilt: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentTilt');
end;

function HMCharacteristicTypeTargetTilt: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetTilt');
end;

function HMCharacteristicTypeOzoneDensity: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeOzoneDensity');
end;

function HMCharacteristicTypeNitrogenDioxideDensity: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeNitrogenDioxideDensity');
end;

function HMCharacteristicTypeSulphurDioxideDensity: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSulphurDioxideDensity');
end;

function HMCharacteristicTypePM2_5Density: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypePM2_5Density');
end;

function HMCharacteristicTypePM10Density: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypePM10Density');
end;

function HMCharacteristicTypeVolatileOrganicCompoundDensity: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeVolatileOrganicCompoundDensity');
end;

function HMCharacteristicTypeProgramMode: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeProgramMode');
end;

function HMCharacteristicTypeInUse: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeInUse');
end;

function HMCharacteristicTypeSetDuration: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSetDuration');
end;

function HMCharacteristicTypeRemainingDuration: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeRemainingDuration');
end;

function HMCharacteristicTypeValveType: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeValveType');
end;

function HMCharacteristicTypeBrightness: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeBrightness');
end;

function HMCharacteristicTypeCoolingThreshold: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCoolingThreshold');
end;

function HMCharacteristicTypeCurrentDoorState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentDoorState');
end;

function HMCharacteristicTypeCurrentHeatingCooling: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentHeatingCooling');
end;

function HMCharacteristicTypeCurrentRelativeHumidity: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentRelativeHumidity');
end;

function HMCharacteristicTypeCurrentTemperature: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentTemperature');
end;

function HMCharacteristicTypeHeatingThreshold: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeHeatingThreshold');
end;

function HMCharacteristicTypeHue: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeHue');
end;

function HMCharacteristicTypeIdentify: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeIdentify');
end;

function HMCharacteristicTypeCurrentLockMechanismState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentLockMechanismState');
end;

function HMCharacteristicTypeTargetLockMechanismState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetLockMechanismState');
end;

function HMCharacteristicTypeManufacturer: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeManufacturer');
end;

function HMCharacteristicTypeModel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeModel');
end;

function HMCharacteristicTypeName: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeName');
end;

function HMCharacteristicTypeObstructionDetected: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeObstructionDetected');
end;

function HMCharacteristicTypePowerState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypePowerState');
end;

function HMCharacteristicTypeRotationDirection: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeRotationDirection');
end;

function HMCharacteristicTypeRotationSpeed: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeRotationSpeed');
end;

function HMCharacteristicTypeSaturation: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSaturation');
end;

function HMCharacteristicTypeSerialNumber: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSerialNumber');
end;

function HMCharacteristicTypeTargetDoorState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetDoorState');
end;

function HMCharacteristicTypeTargetHeatingCooling: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetHeatingCooling');
end;

function HMCharacteristicTypeTargetTemperature: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetTemperature');
end;

function HMCharacteristicTypeTemperatureUnits: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTemperatureUnits');
end;

function HMCharacteristicTypeVersion: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeVersion');
end;

function HMCharacteristicTypeFirmwareVersion: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeFirmwareVersion');
end;

function HMCharacteristicTypeHardwareVersion: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeHardwareVersion');
end;

function HMCharacteristicTypeSoftwareVersion: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSoftwareVersion');
end;

function HMCharacteristicTypeBatteryLevel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeBatteryLevel');
end;

function HMCharacteristicTypeCurrentLightLevel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentLightLevel');
end;

function HMCharacteristicTypeInputEvent: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeInputEvent');
end;

function HMCharacteristicTypeSmokeDetected: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSmokeDetected');
end;

function HMCharacteristicTypeStatusLowBattery: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeStatusLowBattery');
end;

function HMCharacteristicTypeChargingState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeChargingState');
end;

function HMCharacteristicTypeLockPhysicalControls: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeLockPhysicalControls');
end;

function HMCharacteristicTypeCurrentFanState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentFanState');
end;

function HMCharacteristicTypeActive: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeActive');
end;

function HMCharacteristicTypeCurrentHeaterCoolerState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentHeaterCoolerState');
end;

function HMCharacteristicTypeTargetHeaterCoolerState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetHeaterCoolerState');
end;

function HMCharacteristicTypeCurrentHumidifierDehumidifierState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeCurrentHumidifierDehumidifierState');
end;

function HMCharacteristicTypeTargetHumidifierDehumidifierState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetHumidifierDehumidifierState');
end;

function HMCharacteristicTypeWaterLevel: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeWaterLevel');
end;

function HMCharacteristicTypeSwingMode: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSwingMode');
end;

function HMCharacteristicTypeTargetFanState: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeTargetFanState');
end;

function HMCharacteristicTypeDehumidifierThreshold: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeDehumidifierThreshold');
end;

function HMCharacteristicTypeHumidifierThreshold: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeHumidifierThreshold');
end;

function HMCharacteristicTypeColorTemperature: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeColorTemperature');
end;

function HMCharacteristicTypeIsConfigured: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeIsConfigured');
end;

function HMCharacteristicTypeSupportedAudioStreamConfiguration: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeSupportedAudioStreamConfiguration');
end;

function HMCharacteristicTypeVolume: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeVolume');
end;

function HMCharacteristicTypeMute: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeMute');
end;

function HMCharacteristicTypeNightVision: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicTypeNightVision');
end;

function HMCharacteristicMetadataFormatBool: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatBool');
end;

function HMCharacteristicMetadataFormatInt: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatInt');
end;

function HMCharacteristicMetadataFormatFloat: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatFloat');
end;

function HMCharacteristicMetadataFormatString: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatString');
end;

function HMCharacteristicMetadataFormatArray: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatArray');
end;

function HMCharacteristicMetadataFormatDictionary: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatDictionary');
end;

function HMCharacteristicMetadataFormatUInt8: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatUInt8');
end;

function HMCharacteristicMetadataFormatUInt16: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatUInt16');
end;

function HMCharacteristicMetadataFormatUInt32: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatUInt32');
end;

function HMCharacteristicMetadataFormatUInt64: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatUInt64');
end;

function HMCharacteristicMetadataFormatData: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatData');
end;

function HMCharacteristicMetadataFormatTLV8: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataFormatTLV8');
end;

function HMCharacteristicMetadataUnitsCelsius: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataUnitsCelsius');
end;

function HMCharacteristicMetadataUnitsFahrenheit: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataUnitsFahrenheit');
end;

function HMCharacteristicMetadataUnitsPercentage: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataUnitsPercentage');
end;

function HMCharacteristicMetadataUnitsArcDegree: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataUnitsArcDegree');
end;

function HMCharacteristicMetadataUnitsSeconds: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataUnitsSeconds');
end;

function HMCharacteristicMetadataUnitsLux: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataUnitsLux');
end;

function HMCharacteristicMetadataUnitsPartsPerMillion: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataUnitsPartsPerMillion');
end;

function HMCharacteristicMetadataUnitsMicrogramsPerCubicMeter: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicMetadataUnitsMicrogramsPerCubicMeter');
end;

function HMActionSetTypeWakeUp: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMActionSetTypeWakeUp');
end;

function HMActionSetTypeSleep: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMActionSetTypeSleep');
end;

function HMActionSetTypeHomeDeparture: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMActionSetTypeHomeDeparture');
end;

function HMActionSetTypeHomeArrival: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMActionSetTypeHomeArrival');
end;

function HMActionSetTypeUserDefined: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMActionSetTypeUserDefined');
end;

function HMActionSetTypeTriggerOwned: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMActionSetTypeTriggerOwned');
end;

function HMAccessoryCategoryTypeOther: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeOther');
end;

function HMAccessoryCategoryTypeSecuritySystem: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeSecuritySystem');
end;

function HMAccessoryCategoryTypeBridge: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeBridge');
end;

function HMAccessoryCategoryTypeDoor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeDoor');
end;

function HMAccessoryCategoryTypeDoorLock: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeDoorLock');
end;

function HMAccessoryCategoryTypeFan: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeFan');
end;

function HMAccessoryCategoryTypeGarageDoorOpener: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeGarageDoorOpener');
end;

function HMAccessoryCategoryTypeIPCamera: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeIPCamera');
end;

function HMAccessoryCategoryTypeLightbulb: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeLightbulb');
end;

function HMAccessoryCategoryTypeOutlet: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeOutlet');
end;

function HMAccessoryCategoryTypeProgrammableSwitch: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeProgrammableSwitch');
end;

function HMAccessoryCategoryTypeRangeExtender: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeRangeExtender');
end;

function HMAccessoryCategoryTypeSensor: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeSensor');
end;

function HMAccessoryCategoryTypeSwitch: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeSwitch');
end;

function HMAccessoryCategoryTypeThermostat: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeThermostat');
end;

function HMAccessoryCategoryTypeVideoDoorbell: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeVideoDoorbell');
end;

function HMAccessoryCategoryTypeWindow: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeWindow');
end;

function HMAccessoryCategoryTypeWindowCovering: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeWindowCovering');
end;

function HMAccessoryCategoryTypeAirPurifier: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeAirPurifier');
end;

function HMAccessoryCategoryTypeAirHeater: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeAirHeater');
end;

function HMAccessoryCategoryTypeAirConditioner: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeAirConditioner');
end;

function HMAccessoryCategoryTypeAirHumidifier: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeAirHumidifier');
end;

function HMAccessoryCategoryTypeAirDehumidifier: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeAirDehumidifier');
end;

function HMAccessoryCategoryTypeSprinkler: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeSprinkler');
end;

function HMAccessoryCategoryTypeFaucet: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeFaucet');
end;

function HMAccessoryCategoryTypeShowerHead: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMAccessoryCategoryTypeShowerHead');
end;

function HMSignificantEventSunrise: HMSignificantEvent;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMSignificantEventSunrise');
end;

function HMSignificantEventSunset: HMSignificantEvent;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMSignificantEventSunset');
end;

function HMCharacteristicKeyPath: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicKeyPath');
end;

function HMCharacteristicValueKeyPath: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMCharacteristicValueKeyPath');
end;

function HMPresenceKeyPath: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMPresenceKeyPath');
end;

function HMErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libHomeKit, 'HMErrorDomain');
end;

initialization
  HomeKitModule := dlopen(MarshaledAString(libHomeKit), RTLD_LAZY);

finalization
  dlclose(HomeKitModule);

end.