unit DW.iOSapi.Intents;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreLocation, iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.Foundation, DW.iOSapi.EventKit;

const
  INShortcutAvailabilityOptionSleepMindfulness = 1;
  INShortcutAvailabilityOptionSleepJournaling = 2;
  INShortcutAvailabilityOptionSleepMusic = 4;
  INShortcutAvailabilityOptionSleepPodcasts = 8;
  INShortcutAvailabilityOptionSleepReading = 16;
  INShortcutAvailabilityOptionSleepWrapUpYourDay = 32;
  INShortcutAvailabilityOptionSleepYogaAndStretching = 64;
  INIntentErrorInteractionOperationNotSupported = 1900;
  INIntentErrorDonatingInteraction = 1901;
  INIntentErrorDeletingAllInteractions = 1902;
  INIntentErrorDeletingInteractionWithIdentifiers = 1903;
  INIntentErrorDeletingInteractionWithGroupIdentifier = 1904;
  INIntentErrorIntentSupportedByMultipleExtension = 2001;
  INIntentErrorRestrictedIntentsNotSupportedByExtension = 2002;
  INIntentErrorNoHandlerProvidedForIntent = 2003;
  INIntentErrorInvalidIntentName = 2004;
  INIntentErrorNoAppAvailable = 2005;
  INIntentErrorRequestTimedOut = 3001;
  INIntentErrorMissingInformation = 3002;
  INIntentErrorInvalidUserVocabularyFileLocation = 4000;
  INIntentErrorExtensionLaunchingTimeout = 5000;
  INIntentErrorExtensionBringUpFailed = 5001;
  INIntentErrorImageGeneric = 6000;
  INIntentErrorImageNoServiceAvailable = 6001;
  INIntentErrorImageStorageFailed = 6002;
  INIntentErrorImageLoadingFailed = 6003;
  INIntentErrorImageRetrievalFailed = 6004;
  INIntentErrorImageProxyLoop = 6005;
  INIntentErrorImageProxyInvalid = 6006;
  INIntentErrorImageProxyTimeout = 6007;
  INIntentErrorImageServiceFailure = 6008;
  INIntentErrorImageScalingFailed = 6009;
  INIntentErrorPermissionDenied = 6010;
  INIntentErrorVoiceShortcutCreationFailed = 7000;
  INIntentErrorVoiceShortcutGetFailed = 7001;
  INIntentErrorVoiceShortcutDeleteFailed = 7002;
  INIntentErrorEncodingGeneric = 8000;
  INIntentErrorEncodingFailed = 8001;
  INIntentErrorDecodingGeneric = 9000;
  INCallCapabilityOptionAudioCall = 1;
  INCallCapabilityOptionVideoCall = 2;
  INCallRecordTypeUnknown = 0;
  INCallRecordTypeOutgoing = 1;
  INCallRecordTypeMissed = 2;
  INCallRecordTypeReceived = 3;
  INCallRecordTypeLatest = 4;
  INCallRecordTypeVoicemail = 5;
  INCallRecordTypeRinging = 6;
  INCallRecordTypeInProgress = 7;
  INCallRecordTypeOnHold = 8;
  INCallRecordTypeOptionOutgoing = 1;
  INCallRecordTypeOptionMissed = 2;
  INCallRecordTypeOptionReceived = 4;
  INCallRecordTypeOptionLatest = 8;
  INCallRecordTypeOptionVoicemail = 16;
  INCallRecordTypeOptionRinging = 32;
  INCallRecordTypeOptionInProgress = 64;
  INCallRecordTypeOptionOnHold = 128;
  INCallDestinationTypeUnknown = 0;
  INCallDestinationTypeNormal = 1;
  INCallDestinationTypeEmergency = 2;
  INCallDestinationTypeVoicemail = 3;
  INCallDestinationTypeRedial = 4;
  INCallDestinationTypeCallBack = 5;
  INCallDestinationTypeNormalDestination = 1;
  INCallDestinationTypeEmergencyDestination = 2;
  INCallDestinationTypeVoicemailDestination = 3;
  INCallDestinationTypeRedialDestination = 4;
  INCallAudioRouteUnknown = 0;
  INCallAudioRouteSpeakerphoneAudioRoute = 1;
  INCallAudioRouteBluetoothAudioRoute = 2;
  INCallCapabilityUnknown = 0;
  INCallCapabilityAudioCall = 1;
  INCallCapabilityVideoCall = 2;
  INCarSignalOptionAudible = 1;
  INCarSignalOptionVisible = 2;
  INCarAudioSourceUnknown = 0;
  INCarAudioSourceCarPlay = 1;
  INCarAudioSourceiPod = 2;
  INCarAudioSourceRadio = 3;
  INCarAudioSourceBluetooth = 4;
  INCarAudioSourceAUX = 5;
  INCarAudioSourceUSB = 6;
  INCarAudioSourceMemoryCard = 7;
  INCarAudioSourceOpticalDrive = 8;
  INCarAudioSourceHardDrive = 9;
  INRelativeReferenceUnknown = 0;
  INRelativeReferenceNext = 1;
  INRelativeReferencePrevious = 2;
  INCarAirCirculationModeUnknown = 0;
  INCarAirCirculationModeFreshAir = 1;
  INCarAirCirculationModeRecirculateAir = 2;
  INCarSeatUnknown = 0;
  INCarSeatDriver = 1;
  INCarSeatPassenger = 2;
  INCarSeatFrontLeft = 3;
  INCarSeatFrontRight = 4;
  INCarSeatFront = 5;
  INCarSeatRearLeft = 6;
  INCarSeatRearRight = 7;
  INCarSeatRear = 8;
  INCarSeatThirdRowLeft = 9;
  INCarSeatThirdRowRight = 10;
  INCarSeatThirdRow = 11;
  INCarSeatAll = 12;
  INRelativeSettingUnknown = 0;
  INRelativeSettingLowest = 1;
  INRelativeSettingLower = 2;
  INRelativeSettingHigher = 3;
  INRelativeSettingHighest = 4;
  INCarDefrosterUnknown = 0;
  INCarDefrosterFront = 1;
  INCarDefrosterRear = 2;
  INCarDefrosterAll = 3;
  INWorkoutGoalUnitTypeUnknown = 0;
  INWorkoutGoalUnitTypeInch = 1;
  INWorkoutGoalUnitTypeMeter = 2;
  INWorkoutGoalUnitTypeFoot = 3;
  INWorkoutGoalUnitTypeMile = 4;
  INWorkoutGoalUnitTypeYard = 5;
  INWorkoutGoalUnitTypeSecond = 6;
  INWorkoutGoalUnitTypeMinute = 7;
  INWorkoutGoalUnitTypeHour = 8;
  INWorkoutGoalUnitTypeJoule = 9;
  INWorkoutGoalUnitTypeKiloCalorie = 10;
  INWorkoutLocationTypeUnknown = 0;
  INWorkoutLocationTypeOutdoor = 1;
  INWorkoutLocationTypeIndoor = 2;
  INPlaybackQueueLocationUnknown = 0;
  INPlaybackQueueLocationNow = 1;
  INPlaybackQueueLocationNext = 2;
  INPlaybackQueueLocationLater = 3;
  INPlaybackRepeatModeUnknown = 0;
  INPlaybackRepeatModeNone = 1;
  INPlaybackRepeatModeAll = 2;
  INPlaybackRepeatModeOne = 3;
  INMediaAffinityTypeUnknown = 0;
  INMediaAffinityTypeLike = 1;
  INMediaAffinityTypeDislike = 2;
  INRadioTypeUnknown = 0;
  INRadioTypeAM = 1;
  INRadioTypeFM = 2;
  INRadioTypeHD = 3;
  INRadioTypeSatellite = 4;
  INRadioTypeDAB = 5;
  INConditionalOperatorAll = 0;
  INConditionalOperatorAny = 1;
  INConditionalOperatorNone = 2;
  INMessageAttributeOptionRead = 1;
  INMessageAttributeOptionUnread = 2;
  INMessageAttributeOptionFlagged = 4;
  INMessageAttributeOptionUnflagged = 8;
  INMessageAttributeOptionPlayed = 16;
  INOutgoingMessageTypeUnknown = 0;
  INOutgoingMessageTypeOutgoingMessageText = 1;
  INOutgoingMessageTypeOutgoingMessageAudio = 2;
  INMessageAttributeUnknown = 0;
  INMessageAttributeRead = 1;
  INMessageAttributeUnread = 2;
  INMessageAttributeFlagged = 3;
  INMessageAttributeUnflagged = 4;
  INMessageAttributePlayed = 5;
  INTaskPriorityUnknown = 0;
  INTaskPriorityNotFlagged = 1;
  INTaskPriorityFlagged = 2;
  INDateSearchTypeUnknown = 0;
  INDateSearchTypeByDueDate = 1;
  INDateSearchTypeByModifiedDate = 2;
  INDateSearchTypeByCreatedDate = 3;
  INLocationSearchTypeUnknown = 0;
  INLocationSearchTypeByLocationTrigger = 1;
  INNotebookItemTypeUnknown = 0;
  INNotebookItemTypeNote = 1;
  INNotebookItemTypeTaskList = 2;
  INNotebookItemTypeTask = 3;
  INTaskStatusUnknown = 0;
  INTaskStatusNotCompleted = 1;
  INTaskStatusCompleted = 2;
  INTemporalEventTriggerTypeOptionNotScheduled = 1;
  INTemporalEventTriggerTypeOptionScheduledNonRecurring = 2;
  INTemporalEventTriggerTypeOptionScheduledRecurring = 4;
  INBillTypeUnknown = 0;
  INBillTypeAutoInsurance = 1;
  INBillTypeCable = 2;
  INBillTypeCarLease = 3;
  INBillTypeCarLoan = 4;
  INBillTypeCreditCard = 5;
  INBillTypeElectricity = 6;
  INBillTypeGas = 7;
  INBillTypeGarbageAndRecycling = 8;
  INBillTypeHealthInsurance = 9;
  INBillTypeHomeInsurance = 10;
  INBillTypeInternet = 11;
  INBillTypeLifeInsurance = 12;
  INBillTypeMortgage = 13;
  INBillTypeMusicStreaming = 14;
  INBillTypePhone = 15;
  INBillTypeRent = 16;
  INBillTypeSewer = 17;
  INBillTypeStudentLoan = 18;
  INBillTypeTrafficTicket = 19;
  INBillTypeTuition = 20;
  INBillTypeUtilities = 21;
  INBillTypeWater = 22;
  INAccountTypeUnknown = 0;
  INAccountTypeChecking = 1;
  INAccountTypeCredit = 2;
  INAccountTypeDebit = 3;
  INAccountTypeInvestment = 4;
  INAccountTypeMortgage = 5;
  INAccountTypePrepaid = 6;
  INAccountTypeSaving = 7;
  INBalanceTypeUnknown = 0;
  INBalanceTypeMoney = 1;
  INBalanceTypePoints = 2;
  INBalanceTypeMiles = 3;
  INPaymentStatusUnknown = 0;
  INPaymentStatusPending = 1;
  INPaymentStatusCompleted = 2;
  INPaymentStatusCanceled = 3;
  INPaymentStatusFailed = 4;
  INPaymentStatusUnpaid = 5;
  INPhotoAttributeOptionPhoto = 1;
  INPhotoAttributeOptionVideo = 2;
  INPhotoAttributeOptionGIF = 4;
  INPhotoAttributeOptionFlash = 8;
  INPhotoAttributeOptionLandscapeOrientation = 16;
  INPhotoAttributeOptionPortraitOrientation = 32;
  INPhotoAttributeOptionFavorite = 64;
  INPhotoAttributeOptionSelfie = 128;
  INPhotoAttributeOptionFrontFacingCamera = 256;
  INPhotoAttributeOptionScreenshot = 512;
  INPhotoAttributeOptionBurstPhoto = 1024;
  INPhotoAttributeOptionHDRPhoto = 2048;
  INPhotoAttributeOptionSquarePhoto = 4096;
  INPhotoAttributeOptionPanoramaPhoto = 8192;
  INPhotoAttributeOptionTimeLapseVideo = 16384;
  INPhotoAttributeOptionSlowMotionVideo = 32768;
  INPhotoAttributeOptionNoirFilter = 65536;
  INPhotoAttributeOptionChromeFilter = 131072;
  INPhotoAttributeOptionInstantFilter = 262144;
  INPhotoAttributeOptionTonalFilter = 524288;
  INPhotoAttributeOptionTransferFilter = 1048576;
  INPhotoAttributeOptionMonoFilter = 2097152;
  INPhotoAttributeOptionFadeFilter = 4194304;
  INPhotoAttributeOptionProcessFilter = 8388608;
  INPhotoAttributeOptionPortraitPhoto = 16777216;
  INPhotoAttributeOptionLivePhoto = 33554432;
  INPhotoAttributeOptionLoopPhoto = 67108864;
  INPhotoAttributeOptionBouncePhoto = 134217728;
  INPhotoAttributeOptionLongExposurePhoto = 268435456;
  INVisualCodeTypeUnknown = 0;
  INVisualCodeTypeContact = 1;
  INVisualCodeTypeRequestPayment = 2;
  INVisualCodeTypeSendPayment = 3;
  INVisualCodeTypeTransit = 4;
  INVisualCodeTypeBus = 5;
  INVisualCodeTypeSubway = 6;
  INIntentHandlingStatusUnspecified = 0;
  INIntentHandlingStatusReady = 1;
  INIntentHandlingStatusInProgress = 2;
  INIntentHandlingStatusSuccess = 3;
  INIntentHandlingStatusFailure = 4;
  INIntentHandlingStatusDeferredToApplication = 5;
  INIntentHandlingStatusUserConfirmationRequired = 6;
  INInteractionDirectionUnspecified = 0;
  INInteractionDirectionOutgoing = 1;
  INInteractionDirectionIncoming = 2;
  INSearchCallHistoryIntentResponseCodeUnspecified = 0;
  INSearchCallHistoryIntentResponseCodeReady = 1;
  INSearchCallHistoryIntentResponseCodeContinueInApp = 2;
  INSearchCallHistoryIntentResponseCodeFailure = 3;
  INSearchCallHistoryIntentResponseCodeFailureRequiringAppLaunch = 4;
  INSearchCallHistoryIntentResponseCodeFailureAppConfigurationRequired = 5;
  INSearchCallHistoryIntentResponseCodeInProgress = 6;
  INSearchCallHistoryIntentResponseCodeSuccess = 7;
  INStartAudioCallIntentResponseCodeUnspecified = 0;
  INStartAudioCallIntentResponseCodeReady = 1;
  INStartAudioCallIntentResponseCodeContinueInApp = 2;
  INStartAudioCallIntentResponseCodeFailure = 3;
  INStartAudioCallIntentResponseCodeFailureRequiringAppLaunch = 4;
  INStartAudioCallIntentResponseCodeFailureAppConfigurationRequired = 5;
  INStartAudioCallIntentResponseCodeFailureCallingServiceNotAvailable = 6;
  INStartAudioCallIntentResponseCodeFailureContactNotSupportedByApp = 7;
  INStartAudioCallIntentResponseCodeFailureNoValidNumber = 8;
  INStartCallIntentResponseCodeUnspecified = 0;
  INStartCallIntentResponseCodeReady = 1;
  INStartCallIntentResponseCodeContinueInApp = 2;
  INStartCallIntentResponseCodeUserConfirmationRequired = 3;
  INStartCallIntentResponseCodeFailure = 4;
  INStartCallIntentResponseCodeFailureRequiringAppLaunch = 5;
  INStartCallIntentResponseCodeFailureCallingServiceNotAvailable = 6;
  INStartCallIntentResponseCodeFailureContactNotSupportedByApp = 7;
  INStartCallIntentResponseCodeFailureAirplaneModeEnabled = 8;
  INStartCallIntentResponseCodeFailureUnableToHandOff = 9;
  INStartCallIntentResponseCodeFailureAppConfigurationRequired = 10;
  INStartCallIntentResponseCodeFailureCallInProgress = 11;
  INStartCallIntentResponseCodeFailureCallRinging = 12;
  INStartVideoCallIntentResponseCodeUnspecified = 0;
  INStartVideoCallIntentResponseCodeReady = 1;
  INStartVideoCallIntentResponseCodeContinueInApp = 2;
  INStartVideoCallIntentResponseCodeFailure = 3;
  INStartVideoCallIntentResponseCodeFailureRequiringAppLaunch = 4;
  INStartVideoCallIntentResponseCodeFailureAppConfigurationRequired = 5;
  INStartVideoCallIntentResponseCodeFailureCallingServiceNotAvailable = 6;
  INStartVideoCallIntentResponseCodeFailureContactNotSupportedByApp = 7;
  INStartVideoCallIntentResponseCodeFailureInvalidNumber = 8;
  INActivateCarSignalIntentResponseCodeUnspecified = 0;
  INActivateCarSignalIntentResponseCodeReady = 1;
  INActivateCarSignalIntentResponseCodeInProgress = 2;
  INActivateCarSignalIntentResponseCodeSuccess = 3;
  INActivateCarSignalIntentResponseCodeFailure = 4;
  INActivateCarSignalIntentResponseCodeFailureRequiringAppLaunch = 5;
  INGetCarLockStatusIntentResponseCodeUnspecified = 0;
  INGetCarLockStatusIntentResponseCodeReady = 1;
  INGetCarLockStatusIntentResponseCodeInProgress = 2;
  INGetCarLockStatusIntentResponseCodeSuccess = 3;
  INGetCarLockStatusIntentResponseCodeFailure = 4;
  INGetCarLockStatusIntentResponseCodeFailureRequiringAppLaunch = 5;
  INGetCarPowerLevelStatusIntentResponseCodeUnspecified = 0;
  INGetCarPowerLevelStatusIntentResponseCodeReady = 1;
  INGetCarPowerLevelStatusIntentResponseCodeInProgress = 2;
  INGetCarPowerLevelStatusIntentResponseCodeSuccess = 3;
  INGetCarPowerLevelStatusIntentResponseCodeFailure = 4;
  INGetCarPowerLevelStatusIntentResponseCodeFailureRequiringAppLaunch = 5;
  INListCarsIntentResponseCodeUnspecified = 0;
  INListCarsIntentResponseCodeReady = 1;
  INListCarsIntentResponseCodeInProgress = 2;
  INListCarsIntentResponseCodeSuccess = 3;
  INListCarsIntentResponseCodeFailure = 4;
  INListCarsIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSaveProfileInCarIntentResponseCodeUnspecified = 0;
  INSaveProfileInCarIntentResponseCodeReady = 1;
  INSaveProfileInCarIntentResponseCodeInProgress = 2;
  INSaveProfileInCarIntentResponseCodeSuccess = 3;
  INSaveProfileInCarIntentResponseCodeFailure = 4;
  INSaveProfileInCarIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetAudioSourceInCarIntentResponseCodeUnspecified = 0;
  INSetAudioSourceInCarIntentResponseCodeReady = 1;
  INSetAudioSourceInCarIntentResponseCodeInProgress = 2;
  INSetAudioSourceInCarIntentResponseCodeSuccess = 3;
  INSetAudioSourceInCarIntentResponseCodeFailure = 4;
  INSetAudioSourceInCarIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetCarLockStatusIntentResponseCodeUnspecified = 0;
  INSetCarLockStatusIntentResponseCodeReady = 1;
  INSetCarLockStatusIntentResponseCodeInProgress = 2;
  INSetCarLockStatusIntentResponseCodeSuccess = 3;
  INSetCarLockStatusIntentResponseCodeFailure = 4;
  INSetCarLockStatusIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetClimateSettingsInCarIntentResponseCodeUnspecified = 0;
  INSetClimateSettingsInCarIntentResponseCodeReady = 1;
  INSetClimateSettingsInCarIntentResponseCodeInProgress = 2;
  INSetClimateSettingsInCarIntentResponseCodeSuccess = 3;
  INSetClimateSettingsInCarIntentResponseCodeFailure = 4;
  INSetClimateSettingsInCarIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetDefrosterSettingsInCarIntentResponseCodeUnspecified = 0;
  INSetDefrosterSettingsInCarIntentResponseCodeReady = 1;
  INSetDefrosterSettingsInCarIntentResponseCodeInProgress = 2;
  INSetDefrosterSettingsInCarIntentResponseCodeSuccess = 3;
  INSetDefrosterSettingsInCarIntentResponseCodeFailure = 4;
  INSetDefrosterSettingsInCarIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetProfileInCarIntentResponseCodeUnspecified = 0;
  INSetProfileInCarIntentResponseCodeReady = 1;
  INSetProfileInCarIntentResponseCodeInProgress = 2;
  INSetProfileInCarIntentResponseCodeSuccess = 3;
  INSetProfileInCarIntentResponseCodeFailure = 4;
  INSetProfileInCarIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetSeatSettingsInCarIntentResponseCodeUnspecified = 0;
  INSetSeatSettingsInCarIntentResponseCodeReady = 1;
  INSetSeatSettingsInCarIntentResponseCodeInProgress = 2;
  INSetSeatSettingsInCarIntentResponseCodeSuccess = 3;
  INSetSeatSettingsInCarIntentResponseCodeFailure = 4;
  INSetSeatSettingsInCarIntentResponseCodeFailureRequiringAppLaunch = 5;
  INCancelWorkoutIntentResponseCodeUnspecified = 0;
  INCancelWorkoutIntentResponseCodeReady = 1;
  INCancelWorkoutIntentResponseCodeContinueInApp = 2;
  INCancelWorkoutIntentResponseCodeFailure = 3;
  INCancelWorkoutIntentResponseCodeFailureRequiringAppLaunch = 4;
  INCancelWorkoutIntentResponseCodeFailureNoMatchingWorkout = 5;
  INCancelWorkoutIntentResponseCodeHandleInApp = 6;
  INCancelWorkoutIntentResponseCodeSuccess = 7;
  INEndWorkoutIntentResponseCodeUnspecified = 0;
  INEndWorkoutIntentResponseCodeReady = 1;
  INEndWorkoutIntentResponseCodeContinueInApp = 2;
  INEndWorkoutIntentResponseCodeFailure = 3;
  INEndWorkoutIntentResponseCodeFailureRequiringAppLaunch = 4;
  INEndWorkoutIntentResponseCodeFailureNoMatchingWorkout = 5;
  INEndWorkoutIntentResponseCodeHandleInApp = 6;
  INEndWorkoutIntentResponseCodeSuccess = 7;
  INPauseWorkoutIntentResponseCodeUnspecified = 0;
  INPauseWorkoutIntentResponseCodeReady = 1;
  INPauseWorkoutIntentResponseCodeContinueInApp = 2;
  INPauseWorkoutIntentResponseCodeFailure = 3;
  INPauseWorkoutIntentResponseCodeFailureRequiringAppLaunch = 4;
  INPauseWorkoutIntentResponseCodeFailureNoMatchingWorkout = 5;
  INPauseWorkoutIntentResponseCodeHandleInApp = 6;
  INPauseWorkoutIntentResponseCodeSuccess = 7;
  INResumeWorkoutIntentResponseCodeUnspecified = 0;
  INResumeWorkoutIntentResponseCodeReady = 1;
  INResumeWorkoutIntentResponseCodeContinueInApp = 2;
  INResumeWorkoutIntentResponseCodeFailure = 3;
  INResumeWorkoutIntentResponseCodeFailureRequiringAppLaunch = 4;
  INResumeWorkoutIntentResponseCodeFailureNoMatchingWorkout = 5;
  INResumeWorkoutIntentResponseCodeHandleInApp = 6;
  INResumeWorkoutIntentResponseCodeSuccess = 7;
  INStartWorkoutIntentResponseCodeUnspecified = 0;
  INStartWorkoutIntentResponseCodeReady = 1;
  INStartWorkoutIntentResponseCodeContinueInApp = 2;
  INStartWorkoutIntentResponseCodeFailure = 3;
  INStartWorkoutIntentResponseCodeFailureRequiringAppLaunch = 4;
  INStartWorkoutIntentResponseCodeFailureOngoingWorkout = 5;
  INStartWorkoutIntentResponseCodeFailureNoMatchingWorkout = 6;
  INStartWorkoutIntentResponseCodeHandleInApp = 7;
  INStartWorkoutIntentResponseCodeSuccess = 8;
  INAddMediaIntentResponseCodeUnspecified = 0;
  INAddMediaIntentResponseCodeReady = 1;
  INAddMediaIntentResponseCodeInProgress = 2;
  INAddMediaIntentResponseCodeSuccess = 3;
  INAddMediaIntentResponseCodeHandleInApp = 4;
  INAddMediaIntentResponseCodeFailure = 5;
  INAddMediaIntentResponseCodeFailureRequiringAppLaunch = 6;
  INPlayMediaIntentResponseCodeUnspecified = 0;
  INPlayMediaIntentResponseCodeReady = 1;
  INPlayMediaIntentResponseCodeContinueInApp = 2;
  INPlayMediaIntentResponseCodeInProgress = 3;
  INPlayMediaIntentResponseCodeSuccess = 4;
  INPlayMediaIntentResponseCodeHandleInApp = 5;
  INPlayMediaIntentResponseCodeFailure = 6;
  INPlayMediaIntentResponseCodeFailureRequiringAppLaunch = 7;
  INPlayMediaIntentResponseCodeFailureUnknownMediaType = 8;
  INPlayMediaIntentResponseCodeFailureNoUnplayedContent = 9;
  INPlayMediaIntentResponseCodeFailureRestrictedContent = 10;
  INSearchForMediaIntentResponseCodeUnspecified = 0;
  INSearchForMediaIntentResponseCodeReady = 1;
  INSearchForMediaIntentResponseCodeContinueInApp = 2;
  INSearchForMediaIntentResponseCodeInProgress = 3;
  INSearchForMediaIntentResponseCodeSuccess = 4;
  INSearchForMediaIntentResponseCodeFailure = 5;
  INSearchForMediaIntentResponseCodeFailureRequiringAppLaunch = 6;
  INUpdateMediaAffinityIntentResponseCodeUnspecified = 0;
  INUpdateMediaAffinityIntentResponseCodeReady = 1;
  INUpdateMediaAffinityIntentResponseCodeInProgress = 2;
  INUpdateMediaAffinityIntentResponseCodeSuccess = 3;
  INUpdateMediaAffinityIntentResponseCodeFailure = 4;
  INUpdateMediaAffinityIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetRadioStationIntentResponseCodeUnspecified = 0;
  INSetRadioStationIntentResponseCodeReady = 1;
  INSetRadioStationIntentResponseCodeInProgress = 2;
  INSetRadioStationIntentResponseCodeSuccess = 3;
  INSetRadioStationIntentResponseCodeFailure = 4;
  INSetRadioStationIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetRadioStationIntentResponseCodeFailureNotSubscribed = 6;
  INSearchForMessagesIntentResponseCodeUnspecified = 0;
  INSearchForMessagesIntentResponseCodeReady = 1;
  INSearchForMessagesIntentResponseCodeInProgress = 2;
  INSearchForMessagesIntentResponseCodeSuccess = 3;
  INSearchForMessagesIntentResponseCodeFailure = 4;
  INSearchForMessagesIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSearchForMessagesIntentResponseCodeFailureMessageServiceNotAvailable = 6;
  INSearchForMessagesIntentResponseCodeFailureMessageTooManyResults = 7;
  INSendMessageIntentResponseCodeUnspecified = 0;
  INSendMessageIntentResponseCodeReady = 1;
  INSendMessageIntentResponseCodeInProgress = 2;
  INSendMessageIntentResponseCodeSuccess = 3;
  INSendMessageIntentResponseCodeFailure = 4;
  INSendMessageIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSendMessageIntentResponseCodeFailureMessageServiceNotAvailable = 6;
  INSetMessageAttributeIntentResponseCodeUnspecified = 0;
  INSetMessageAttributeIntentResponseCodeReady = 1;
  INSetMessageAttributeIntentResponseCodeInProgress = 2;
  INSetMessageAttributeIntentResponseCodeSuccess = 3;
  INSetMessageAttributeIntentResponseCodeFailure = 4;
  INSetMessageAttributeIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetMessageAttributeIntentResponseCodeFailureMessageNotFound = 6;
  INSetMessageAttributeIntentResponseCodeFailureMessageAttributeNotSet = 7;
  INAddTasksIntentResponseCodeUnspecified = 0;
  INAddTasksIntentResponseCodeReady = 1;
  INAddTasksIntentResponseCodeInProgress = 2;
  INAddTasksIntentResponseCodeSuccess = 3;
  INAddTasksIntentResponseCodeFailure = 4;
  INAddTasksIntentResponseCodeFailureRequiringAppLaunch = 5;
  INAppendToNoteIntentResponseCodeUnspecified = 0;
  INAppendToNoteIntentResponseCodeReady = 1;
  INAppendToNoteIntentResponseCodeInProgress = 2;
  INAppendToNoteIntentResponseCodeSuccess = 3;
  INAppendToNoteIntentResponseCodeFailure = 4;
  INAppendToNoteIntentResponseCodeFailureRequiringAppLaunch = 5;
  INAppendToNoteIntentResponseCodeFailureCannotUpdatePasswordProtectedNote = 6;
  INCreateNoteIntentResponseCodeUnspecified = 0;
  INCreateNoteIntentResponseCodeReady = 1;
  INCreateNoteIntentResponseCodeInProgress = 2;
  INCreateNoteIntentResponseCodeSuccess = 3;
  INCreateNoteIntentResponseCodeFailure = 4;
  INCreateNoteIntentResponseCodeFailureRequiringAppLaunch = 5;
  INCreateTaskListIntentResponseCodeUnspecified = 0;
  INCreateTaskListIntentResponseCodeReady = 1;
  INCreateTaskListIntentResponseCodeInProgress = 2;
  INCreateTaskListIntentResponseCodeSuccess = 3;
  INCreateTaskListIntentResponseCodeFailure = 4;
  INCreateTaskListIntentResponseCodeFailureRequiringAppLaunch = 5;
  INDeleteTasksIntentResponseCodeUnspecified = 0;
  INDeleteTasksIntentResponseCodeReady = 1;
  INDeleteTasksIntentResponseCodeInProgress = 2;
  INDeleteTasksIntentResponseCodeSuccess = 3;
  INDeleteTasksIntentResponseCodeFailure = 4;
  INDeleteTasksIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSortTypeUnknown = 0;
  INSortTypeAsIs = 1;
  INSortTypeByDate = 2;
  INSearchForNotebookItemsIntentResponseCodeUnspecified = 0;
  INSearchForNotebookItemsIntentResponseCodeReady = 1;
  INSearchForNotebookItemsIntentResponseCodeInProgress = 2;
  INSearchForNotebookItemsIntentResponseCodeSuccess = 3;
  INSearchForNotebookItemsIntentResponseCodeFailure = 4;
  INSearchForNotebookItemsIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSetTaskAttributeIntentResponseCodeUnspecified = 0;
  INSetTaskAttributeIntentResponseCodeReady = 1;
  INSetTaskAttributeIntentResponseCodeInProgress = 2;
  INSetTaskAttributeIntentResponseCodeSuccess = 3;
  INSetTaskAttributeIntentResponseCodeFailure = 4;
  INSetTaskAttributeIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSnoozeTasksIntentResponseCodeUnspecified = 0;
  INSnoozeTasksIntentResponseCodeReady = 1;
  INSnoozeTasksIntentResponseCodeInProgress = 2;
  INSnoozeTasksIntentResponseCodeSuccess = 3;
  INSnoozeTasksIntentResponseCodeFailure = 4;
  INSnoozeTasksIntentResponseCodeFailureRequiringAppLaunch = 5;
  INPayBillIntentResponseCodeUnspecified = 0;
  INPayBillIntentResponseCodeReady = 1;
  INPayBillIntentResponseCodeInProgress = 2;
  INPayBillIntentResponseCodeSuccess = 3;
  INPayBillIntentResponseCodeFailure = 4;
  INPayBillIntentResponseCodeFailureRequiringAppLaunch = 5;
  INPayBillIntentResponseCodeFailureCredentialsUnverified = 6;
  INPayBillIntentResponseCodeFailureInsufficientFunds = 7;
  INRequestPaymentIntentResponseCodeUnspecified = 0;
  INRequestPaymentIntentResponseCodeReady = 1;
  INRequestPaymentIntentResponseCodeInProgress = 2;
  INRequestPaymentIntentResponseCodeSuccess = 3;
  INRequestPaymentIntentResponseCodeFailure = 4;
  INRequestPaymentIntentResponseCodeFailureRequiringAppLaunch = 5;
  INRequestPaymentIntentResponseCodeFailureCredentialsUnverified = 6;
  INRequestPaymentIntentResponseCodeFailurePaymentsAmountBelowMinimum = 7;
  INRequestPaymentIntentResponseCodeFailurePaymentsAmountAboveMaximum = 8;
  INRequestPaymentIntentResponseCodeFailurePaymentsCurrencyUnsupported = 9;
  INRequestPaymentIntentResponseCodeFailureNoBankAccount = 10;
  INRequestPaymentIntentResponseCodeFailureNotEligible = 11;
  INRequestPaymentIntentResponseCodeFailureTermsAndConditionsAcceptanceRequired = 12;
  INSearchForAccountsIntentResponseCodeUnspecified = 0;
  INSearchForAccountsIntentResponseCodeReady = 1;
  INSearchForAccountsIntentResponseCodeInProgress = 2;
  INSearchForAccountsIntentResponseCodeSuccess = 3;
  INSearchForAccountsIntentResponseCodeFailure = 4;
  INSearchForAccountsIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSearchForAccountsIntentResponseCodeFailureCredentialsUnverified = 6;
  INSearchForAccountsIntentResponseCodeFailureAccountNotFound = 7;
  INSearchForAccountsIntentResponseCodeFailureTermsAndConditionsAcceptanceRequired = 8;
  INSearchForAccountsIntentResponseCodeFailureNotEligible = 9;
  INSearchForBillsIntentResponseCodeUnspecified = 0;
  INSearchForBillsIntentResponseCodeReady = 1;
  INSearchForBillsIntentResponseCodeInProgress = 2;
  INSearchForBillsIntentResponseCodeSuccess = 3;
  INSearchForBillsIntentResponseCodeFailure = 4;
  INSearchForBillsIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSearchForBillsIntentResponseCodeFailureCredentialsUnverified = 6;
  INSearchForBillsIntentResponseCodeFailureBillNotFound = 7;
  INSendPaymentIntentResponseCodeUnspecified = 0;
  INSendPaymentIntentResponseCodeReady = 1;
  INSendPaymentIntentResponseCodeInProgress = 2;
  INSendPaymentIntentResponseCodeSuccess = 3;
  INSendPaymentIntentResponseCodeFailure = 4;
  INSendPaymentIntentResponseCodeFailureRequiringAppLaunch = 5;
  INSendPaymentIntentResponseCodeFailureCredentialsUnverified = 6;
  INSendPaymentIntentResponseCodeFailurePaymentsAmountBelowMinimum = 7;
  INSendPaymentIntentResponseCodeFailurePaymentsAmountAboveMaximum = 8;
  INSendPaymentIntentResponseCodeFailurePaymentsCurrencyUnsupported = 9;
  INSendPaymentIntentResponseCodeFailureInsufficientFunds = 10;
  INSendPaymentIntentResponseCodeFailureNoBankAccount = 11;
  INSendPaymentIntentResponseCodeFailureNotEligible = 12;
  INSendPaymentIntentResponseCodeFailureTermsAndConditionsAcceptanceRequired = 13;
  INTransferMoneyIntentResponseCodeUnspecified = 0;
  INTransferMoneyIntentResponseCodeReady = 1;
  INTransferMoneyIntentResponseCodeInProgress = 2;
  INTransferMoneyIntentResponseCodeSuccess = 3;
  INTransferMoneyIntentResponseCodeFailure = 4;
  INTransferMoneyIntentResponseCodeFailureRequiringAppLaunch = 5;
  INTransferMoneyIntentResponseCodeFailureCredentialsUnverified = 6;
  INTransferMoneyIntentResponseCodeFailureInsufficientFunds = 7;
  INSearchForPhotosIntentResponseCodeUnspecified = 0;
  INSearchForPhotosIntentResponseCodeReady = 1;
  INSearchForPhotosIntentResponseCodeContinueInApp = 2;
  INSearchForPhotosIntentResponseCodeFailure = 3;
  INSearchForPhotosIntentResponseCodeFailureRequiringAppLaunch = 4;
  INSearchForPhotosIntentResponseCodeFailureAppConfigurationRequired = 5;
  INStartPhotoPlaybackIntentResponseCodeUnspecified = 0;
  INStartPhotoPlaybackIntentResponseCodeReady = 1;
  INStartPhotoPlaybackIntentResponseCodeContinueInApp = 2;
  INStartPhotoPlaybackIntentResponseCodeFailure = 3;
  INStartPhotoPlaybackIntentResponseCodeFailureRequiringAppLaunch = 4;
  INStartPhotoPlaybackIntentResponseCodeFailureAppConfigurationRequired = 5;
  INGetReservationDetailsIntentResponseCodeUnspecified = 0;
  INGetReservationDetailsIntentResponseCodeReady = 1;
  INGetReservationDetailsIntentResponseCodeInProgress = 2;
  INGetReservationDetailsIntentResponseCodeSuccess = 3;
  INGetReservationDetailsIntentResponseCodeFailure = 4;
  INGetReservationDetailsIntentResponseCodeFailureRequiringAppLaunch = 5;
  INGetRideStatusIntentResponseCodeUnspecified = 0;
  INGetRideStatusIntentResponseCodeReady = 1;
  INGetRideStatusIntentResponseCodeInProgress = 2;
  INGetRideStatusIntentResponseCodeSuccess = 3;
  INGetRideStatusIntentResponseCodeFailure = 4;
  INGetRideStatusIntentResponseCodeFailureRequiringAppLaunch = 5;
  INGetRideStatusIntentResponseCodeFailureRequiringAppLaunchMustVerifyCredentials = 6;
  INGetRideStatusIntentResponseCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable = 7;
  INListRideOptionsIntentResponseCodeUnspecified = 0;
  INListRideOptionsIntentResponseCodeReady = 1;
  INListRideOptionsIntentResponseCodeInProgress = 2;
  INListRideOptionsIntentResponseCodeSuccess = 3;
  INListRideOptionsIntentResponseCodeFailure = 4;
  INListRideOptionsIntentResponseCodeFailureRequiringAppLaunch = 5;
  INListRideOptionsIntentResponseCodeFailureRequiringAppLaunchMustVerifyCredentials = 6;
  INListRideOptionsIntentResponseCodeFailureRequiringAppLaunchNoServiceInArea = 7;
  INListRideOptionsIntentResponseCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable = 8;
  INListRideOptionsIntentResponseCodeFailureRequiringAppLaunchPreviousRideNeedsCompletion = 9;
  INListRideOptionsIntentResponseCodeFailurePreviousRideNeedsFeedback = 10;
  INRequestRideIntentResponseCodeUnspecified = 0;
  INRequestRideIntentResponseCodeReady = 1;
  INRequestRideIntentResponseCodeInProgress = 2;
  INRequestRideIntentResponseCodeSuccess = 3;
  INRequestRideIntentResponseCodeFailure = 4;
  INRequestRideIntentResponseCodeFailureRequiringAppLaunch = 5;
  INRequestRideIntentResponseCodeFailureRequiringAppLaunchMustVerifyCredentials = 6;
  INRequestRideIntentResponseCodeFailureRequiringAppLaunchNoServiceInArea = 7;
  INRequestRideIntentResponseCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable = 8;
  INRequestRideIntentResponseCodeFailureRequiringAppLaunchPreviousRideNeedsCompletion = 9;
  INRequestRideIntentResponseCodeFailureRequiringAppLaunchRideScheduledTooFar = 10;
  INCancelRideIntentResponseCodeUnspecified = 0;
  INCancelRideIntentResponseCodeReady = 1;
  INCancelRideIntentResponseCodeSuccess = 2;
  INCancelRideIntentResponseCodeFailure = 3;
  INSendRideFeedbackIntentResponseCodeUnspecified = 0;
  INSendRideFeedbackIntentResponseCodeReady = 1;
  INSendRideFeedbackIntentResponseCodeSuccess = 2;
  INSendRideFeedbackIntentResponseCodeFailure = 3;
  INGetVisualCodeIntentResponseCodeUnspecified = 0;
  INGetVisualCodeIntentResponseCodeReady = 1;
  INGetVisualCodeIntentResponseCodeContinueInApp = 2;
  INGetVisualCodeIntentResponseCodeInProgress = 3;
  INGetVisualCodeIntentResponseCodeSuccess = 4;
  INGetVisualCodeIntentResponseCodeFailure = 5;
  INGetVisualCodeIntentResponseCodeFailureRequiringAppLaunch = 6;
  INGetVisualCodeIntentResponseCodeFailureAppConfigurationRequired = 7;
  INAddMediaMediaDestinationUnsupportedReasonPlaylistNameNotFound = 1;
  INAddMediaMediaDestinationUnsupportedReasonPlaylistNotEditable = 2;
  INAddMediaMediaItemUnsupportedReasonLoginRequired = 1;
  INAddMediaMediaItemUnsupportedReasonSubscriptionRequired = 2;
  INAddMediaMediaItemUnsupportedReasonUnsupportedMediaType = 3;
  INAddMediaMediaItemUnsupportedReasonExplicitContentSettings = 4;
  INAddMediaMediaItemUnsupportedReasonCellularDataSettings = 5;
  INAddMediaMediaItemUnsupportedReasonRestrictedContent = 6;
  INAddMediaMediaItemUnsupportedReasonServiceUnavailable = 7;
  INAddMediaMediaItemUnsupportedReasonRegionRestriction = 8;
  INAddTasksTargetTaskListConfirmationReasonListShouldBeCreated = 1;
  INAddTasksTemporalEventTriggerUnsupportedReasonTimeInPast = 1;
  INAddTasksTemporalEventTriggerUnsupportedReasonInvalidRecurrence = 2;
  INAmountTypeUnknown = 0;
  INAmountTypeMinimumDue = 1;
  INAmountTypeAmountDue = 2;
  INAmountTypeCurrentBalance = 3;
  INAmountTypeMaximumTransferAmount = 4;
  INAmountTypeMinimumTransferAmount = 5;
  INAmountTypeStatementBalance = 6;
  INDayOfWeekOptionMonday = 1;
  INDayOfWeekOptionTuesday = 2;
  INDayOfWeekOptionWednesday = 4;
  INDayOfWeekOptionThursday = 8;
  INDayOfWeekOptionFriday = 16;
  INDayOfWeekOptionSaturday = 32;
  INDayOfWeekOptionSunday = 64;
  INDeleteTasksTaskListUnsupportedReasonNoTaskListFound = 1;
  INDeleteTasksTaskUnsupportedReasonNoTasksFound = 1;
  INDeleteTasksTaskUnsupportedReasonNoTasksInApp = 2;
  INMediaDestinationTypeUnknown = 0;
  INMediaDestinationTypeLibrary = 1;
  INMediaDestinationTypePlaylist = 2;
  INMediaItemTypeUnknown = 0;
  INMediaItemTypeSong = 1;
  INMediaItemTypeAlbum = 2;
  INMediaItemTypeArtist = 3;
  INMediaItemTypeGenre = 4;
  INMediaItemTypePlaylist = 5;
  INMediaItemTypePodcastShow = 6;
  INMediaItemTypePodcastEpisode = 7;
  INMediaItemTypePodcastPlaylist = 8;
  INMediaItemTypeMusicStation = 9;
  INMediaItemTypeAudioBook = 10;
  INMediaItemTypeMovie = 11;
  INMediaItemTypeTVShow = 12;
  INMediaItemTypeTVShowEpisode = 13;
  INMediaItemTypeMusicVideo = 14;
  INMediaItemTypePodcastStation = 15;
  INMediaItemTypeRadioStation = 16;
  INMediaItemTypeStation = 17;
  INMediaItemTypeMusic = 18;
  INMediaItemTypeAlgorithmicRadioStation = 19;
  INMediaItemTypeNews = 20;
  INMediaReferenceUnknown = 0;
  INMediaReferenceCurrentlyPlaying = 1;
  INMediaReferenceMy = 2;
  INMediaSortOrderUnknown = 0;
  INMediaSortOrderNewest = 1;
  INMediaSortOrderOldest = 2;
  INMediaSortOrderBest = 3;
  INMediaSortOrderWorst = 4;
  INMediaSortOrderPopular = 5;
  INMediaSortOrderUnpopular = 6;
  INMediaSortOrderTrending = 7;
  INMediaSortOrderRecommended = 8;
  INNoteContentTypeUnknown = 0;
  INNoteContentTypeText = 1;
  INNoteContentTypeImage = 2;
  INPaymentMethodTypeUnknown = 0;
  INPaymentMethodTypeChecking = 1;
  INPaymentMethodTypeSavings = 2;
  INPaymentMethodTypeBrokerage = 3;
  INPaymentMethodTypeDebit = 4;
  INPaymentMethodTypeCredit = 5;
  INPaymentMethodTypePrepaid = 6;
  INPaymentMethodTypeStore = 7;
  INPaymentMethodTypeApplePay = 8;
  INPlayMediaMediaItemUnsupportedReasonLoginRequired = 1;
  INPlayMediaMediaItemUnsupportedReasonSubscriptionRequired = 2;
  INPlayMediaMediaItemUnsupportedReasonUnsupportedMediaType = 3;
  INPlayMediaMediaItemUnsupportedReasonExplicitContentSettings = 4;
  INPlayMediaMediaItemUnsupportedReasonCellularDataSettings = 5;
  INPlayMediaMediaItemUnsupportedReasonRestrictedContent = 6;
  INPlayMediaMediaItemUnsupportedReasonServiceUnavailable = 7;
  INPlayMediaMediaItemUnsupportedReasonRegionRestriction = 8;
  INPlayMediaPlaybackSpeedUnsupportedReasonBelowMinimum = 1;
  INPlayMediaPlaybackSpeedUnsupportedReasonAboveMaximum = 2;
  INRecurrenceFrequencyUnknown = 0;
  INRecurrenceFrequencyMinute = 1;
  INRecurrenceFrequencyHourly = 2;
  INRecurrenceFrequencyDaily = 3;
  INRecurrenceFrequencyWeekly = 4;
  INRecurrenceFrequencyMonthly = 5;
  INRecurrenceFrequencyYearly = 6;
  INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsAmountBelowMinimum = 1;
  INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsAmountAboveMaximum = 2;
  INRequestPaymentCurrencyAmountUnsupportedReasonPaymentsCurrencyUnsupported = 3;
  INRequestPaymentPayerUnsupportedReasonCredentialsUnverified = 1;
  INRequestPaymentPayerUnsupportedReasonNoAccount = 2;
  INRequestPaymentPayerUnsupportedReasonNoValidHandle = 3;
  INReservationActionTypeUnknown = 0;
  INReservationActionTypeCheckIn = 1;
  INReservationStatusUnknown = 0;
  INReservationStatusCanceled = 1;
  INReservationStatusPending = 2;
  INReservationStatusHold = 3;
  INReservationStatusConfirmed = 4;
  INRideFeedbackTypeOptionRate = 1;
  INRideFeedbackTypeOptionTip = 2;
  INRidePhaseUnknown = 0;
  INRidePhaseReceived = 1;
  INRidePhaseConfirmed = 2;
  INRidePhaseOngoing = 3;
  INRidePhaseCompleted = 4;
  INRidePhaseApproachingPickup = 5;
  INRidePhasePickup = 6;
  INSearchForMediaMediaItemUnsupportedReasonLoginRequired = 1;
  INSearchForMediaMediaItemUnsupportedReasonSubscriptionRequired = 2;
  INSearchForMediaMediaItemUnsupportedReasonUnsupportedMediaType = 3;
  INSearchForMediaMediaItemUnsupportedReasonExplicitContentSettings = 4;
  INSearchForMediaMediaItemUnsupportedReasonCellularDataSettings = 5;
  INSearchForMediaMediaItemUnsupportedReasonRestrictedContent = 6;
  INSearchForMediaMediaItemUnsupportedReasonServiceUnavailable = 7;
  INSearchForMediaMediaItemUnsupportedReasonRegionRestriction = 8;
  INSendMessageRecipientUnsupportedReasonNoAccount = 1;
  INSendMessageRecipientUnsupportedReasonOffline = 2;
  INSendMessageRecipientUnsupportedReasonMessagingServiceNotEnabledForRecipient = 3;
  INSendMessageRecipientUnsupportedReasonNoValidHandle = 4;
  INSendMessageRecipientUnsupportedReasonRequestedHandleInvalid = 5;
  INSendMessageRecipientUnsupportedReasonNoHandleForLabel = 6;
  INSendPaymentCurrencyAmountUnsupportedReasonPaymentsAmountBelowMinimum = 1;
  INSendPaymentCurrencyAmountUnsupportedReasonPaymentsAmountAboveMaximum = 2;
  INSendPaymentCurrencyAmountUnsupportedReasonPaymentsCurrencyUnsupported = 3;
  INSendPaymentPayeeUnsupportedReasonCredentialsUnverified = 1;
  INSendPaymentPayeeUnsupportedReasonInsufficientFunds = 2;
  INSendPaymentPayeeUnsupportedReasonNoAccount = 3;
  INSendPaymentPayeeUnsupportedReasonNoValidHandle = 4;
  INSetTaskAttributeTemporalEventTriggerUnsupportedReasonTimeInPast = 1;
  INSetTaskAttributeTemporalEventTriggerUnsupportedReasonInvalidRecurrence = 2;
  INSnoozeTasksTaskUnsupportedReasonNoTasksFound = 1;
  INSpatialEventUnknown = 0;
  INSpatialEventArrive = 1;
  INSpatialEventDepart = 2;
  INStartCallCallCapabilityUnsupportedReasonVideoCallUnsupported = 1;
  INStartCallCallCapabilityUnsupportedReasonMicrophoneNotAccessible = 2;
  INStartCallCallCapabilityUnsupportedReasonCameraNotAccessible = 3;
  INStartCallCallRecordToCallBackUnsupportedReasonNoMatchingCall = 1;
  INStartCallContactUnsupportedReasonNoContactFound = 1;
  INStartCallContactUnsupportedReasonMultipleContactsUnsupported = 2;
  INStartCallContactUnsupportedReasonNoHandleForLabel = 3;
  INStartCallContactUnsupportedReasonInvalidHandle = 4;
  INStartCallContactUnsupportedReasonUnsupportedMmiUssd = 5;
  INStartCallContactUnsupportedReasonNoCallHistoryForRedial = 6;
  INStartCallContactUnsupportedReasonNoUsableHandleForRedial = 7;
  INTaskTypeUnknown = 0;
  INTaskTypeNotCompletable = 1;
  INTaskTypeCompletable = 2;
  INTicketedEventCategoryUnknown = 0;
  INTicketedEventCategoryMovie = 1;
  INUpdateMediaAffinityMediaItemUnsupportedReasonLoginRequired = 1;
  INUpdateMediaAffinityMediaItemUnsupportedReasonSubscriptionRequired = 2;
  INUpdateMediaAffinityMediaItemUnsupportedReasonUnsupportedMediaType = 3;
  INUpdateMediaAffinityMediaItemUnsupportedReasonExplicitContentSettings = 4;
  INUpdateMediaAffinityMediaItemUnsupportedReasonCellularDataSettings = 5;
  INUpdateMediaAffinityMediaItemUnsupportedReasonRestrictedContent = 6;
  INUpdateMediaAffinityMediaItemUnsupportedReasonServiceUnavailable = 7;
  INUpdateMediaAffinityMediaItemUnsupportedReasonRegionRestriction = 8;
  INPersonHandleTypeUnknown = 0;
  INPersonHandleTypeEmailAddress = 1;
  INPersonHandleTypePhoneNumber = 2;
  INPersonSuggestionTypeNone = 0;
  INPersonSuggestionTypeSocialProfile = 1;
  INPersonSuggestionTypeInstantMessageAddress = 2;
  INMessageTypeUnspecified = 0;
  INMessageTypeText = 1;
  INMessageTypeAudio = 2;
  INMessageTypeDigitalTouch = 3;
  INMessageTypeHandwriting = 4;
  INMessageTypeSticker = 5;
  INMessageTypeTapbackLiked = 6;
  INMessageTypeTapbackDisliked = 7;
  INMessageTypeTapbackEmphasized = 8;
  INMessageTypeTapbackLoved = 9;
  INMessageTypeTapbackQuestioned = 10;
  INMessageTypeTapbackLaughed = 11;
  INMessageTypeMediaCalendar = 12;
  INMessageTypeMediaLocation = 13;
  INMessageTypeMediaAddressCard = 14;
  INMessageTypeMediaImage = 15;
  INMessageTypeMediaVideo = 16;
  INMessageTypeMediaPass = 17;
  INMessageTypeMediaAudio = 18;
  INMessageTypePaymentSent = 19;
  INMessageTypePaymentRequest = 20;
  INMessageTypePaymentNote = 21;
  INMessageTypeAnimoji = 22;
  INMessageTypeActivitySnippet = 23;
  INMessageTypeFile = 24;
  INMessageTypeLink = 25;
  INRestaurantReservationUserBookingStatusPending = 0;
  INRestaurantReservationUserBookingStatusConfirmed = 1;
  INRestaurantReservationUserBookingStatusDenied = 2;
  INBookRestaurantReservationIntentCodeSuccess = 0;
  INBookRestaurantReservationIntentCodeDenied = 1;
  INBookRestaurantReservationIntentCodeFailure = 2;
  INBookRestaurantReservationIntentCodeFailureRequiringAppLaunch = 3;
  INBookRestaurantReservationIntentCodeFailureRequiringAppLaunchMustVerifyCredentials = 4;
  INBookRestaurantReservationIntentCodeFailureRequiringAppLaunchServiceTemporarilyUnavailable = 5;
  INGetAvailableRestaurantReservationBookingsIntentCodeSuccess = 0;
  INGetAvailableRestaurantReservationBookingsIntentCodeFailure = 1;
  INGetAvailableRestaurantReservationBookingsIntentCodeFailureRequestUnsatisfiable = 2;
  INGetAvailableRestaurantReservationBookingsIntentCodeFailureRequestUnspecified = 3;
  INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeSuccess = 0;
  INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeFailure = 1;
  INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeFailureRequestUnsatisfiable = 2;
  INGetUserCurrentRestaurantReservationBookingsIntentResponseCodeUnspecified = 3;
  INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeSuccess = 0;
  INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeFailure = 1;
  INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCodeUnspecified = 2;
  INGetRestaurantGuestIntentResponseCodeSuccess = 0;
  INGetRestaurantGuestIntentResponseCodeFailure = 1;
  INVocabularyStringTypeContactName = 1;
  INVocabularyStringTypeContactGroupName = 2;
  INVocabularyStringTypePhotoTag = 100;
  INVocabularyStringTypePhotoAlbumName = 101;
  INVocabularyStringTypeWorkoutActivityName = 200;
  INVocabularyStringTypeCarProfileName = 300;
  INVocabularyStringTypeCarName = 301;
  INVocabularyStringTypePaymentsOrganizationName = 400;
  INVocabularyStringTypePaymentsAccountNickname = 401;
  INVocabularyStringTypeNotebookItemTitle = 500;
  INVocabularyStringTypeNotebookItemGroupName = 501;
  INVocabularyStringTypeMediaPlaylistTitle = 700;
  INVocabularyStringTypeMediaMusicArtistName = 701;
  INVocabularyStringTypeMediaAudiobookTitle = 702;
  INVocabularyStringTypeMediaAudiobookAuthorName = 703;
  INVocabularyStringTypeMediaShowTitle = 704;
  INUpcomingMediaPredictionModeDefault = 0;
  INUpcomingMediaPredictionModeOnlyPredictSuggestedIntents = 1;
  INSiriAuthorizationStatusNotDetermined = 0;
  INSiriAuthorizationStatusRestricted = 1;
  INSiriAuthorizationStatusDenied = 2;
  INSiriAuthorizationStatusAuthorized = 3;
  INMediaUserContextSubscriptionStatusUnknown = 0;
  INMediaUserContextSubscriptionStatusNotSubscribed = 1;
  INMediaUserContextSubscriptionStatusSubscribed = 2;
  INDailyRoutineSituationMorning = 0;
  INDailyRoutineSituationEvening = 1;
  INDailyRoutineSituationHome = 2;
  INDailyRoutineSituationWork = 3;
  INDailyRoutineSituationSchool = 4;
  INDailyRoutineSituationGym = 5;
  INDailyRoutineSituationCommute = 6;
  INDailyRoutineSituationHeadphonesConnected = 7;
  INDailyRoutineSituationActiveWorkout = 8;
  INDailyRoutineSituationPhysicalActivityIncomplete = 9;
  INRelevantShortcutRoleAction = 0;
  INRelevantShortcutRoleInformation = 1;

type
  INIntent = interface;
  INIntentHandlerProviding = interface;
  INIntentResponse = interface;
  INIntentResolutionResult = interface;
  INSearchCallHistoryIntent = interface;
  INSearchCallHistoryIntentHandling = interface;
  INStartAudioCallIntent = interface;
  INStartAudioCallIntentHandling = interface;
  INStartCallIntent = interface;
  INStartCallIntentHandling = interface;
  INStartVideoCallIntent = interface;
  INStartVideoCallIntentHandling = interface;
  INActivateCarSignalIntent = interface;
  INActivateCarSignalIntentHandling = interface;
  INGetCarLockStatusIntent = interface;
  INGetCarLockStatusIntentHandling = interface;
  INGetCarPowerLevelStatusIntent = interface;
  INGetCarPowerLevelStatusIntentHandling = interface;
  INGetCarPowerLevelStatusIntentResponseObserver = interface;
  INListCarsIntent = interface;
  INListCarsIntentHandling = interface;
  INSaveProfileInCarIntent = interface;
  INSaveProfileInCarIntentHandling = interface;
  INSetAudioSourceInCarIntent = interface;
  INSetAudioSourceInCarIntentHandling = interface;
  INSetCarLockStatusIntent = interface;
  INSetCarLockStatusIntentHandling = interface;
  INSetClimateSettingsInCarIntent = interface;
  INSetClimateSettingsInCarIntentHandling = interface;
  INSetDefrosterSettingsInCarIntent = interface;
  INSetDefrosterSettingsInCarIntentHandling = interface;
  INSetProfileInCarIntent = interface;
  INSetProfileInCarIntentHandling = interface;
  INSetSeatSettingsInCarIntent = interface;
  INSetSeatSettingsInCarIntentHandling = interface;
  INCancelWorkoutIntent = interface;
  INCancelWorkoutIntentHandling = interface;
  INEndWorkoutIntent = interface;
  INEndWorkoutIntentHandling = interface;
  INPauseWorkoutIntent = interface;
  INPauseWorkoutIntentHandling = interface;
  INResumeWorkoutIntent = interface;
  INResumeWorkoutIntentHandling = interface;
  INStartWorkoutIntent = interface;
  INStartWorkoutIntentHandling = interface;
  INAddMediaIntent = interface;
  INAddMediaIntentHandling = interface;
  INPlayMediaIntent = interface;
  INPlayMediaIntentHandling = interface;
  INSearchForMediaIntent = interface;
  INSearchForMediaIntentHandling = interface;
  INUpdateMediaAffinityIntent = interface;
  INUpdateMediaAffinityIntentHandling = interface;
  INSetRadioStationIntent = interface;
  INSetRadioStationIntentHandling = interface;
  INSearchForMessagesIntent = interface;
  INSearchForMessagesIntentHandling = interface;
  INSendMessageIntent = interface;
  INSendMessageIntentHandling = interface;
  INSetMessageAttributeIntent = interface;
  INSetMessageAttributeIntentHandling = interface;
  INAddTasksIntent = interface;
  INAddTasksIntentHandling = interface;
  INAppendToNoteIntent = interface;
  INAppendToNoteIntentHandling = interface;
  INCreateNoteIntent = interface;
  INCreateNoteIntentHandling = interface;
  INCreateTaskListIntent = interface;
  INCreateTaskListIntentHandling = interface;
  INDeleteTasksIntent = interface;
  INDeleteTasksIntentHandling = interface;
  INSearchForNotebookItemsIntent = interface;
  INSearchForNotebookItemsIntentHandling = interface;
  INSetTaskAttributeIntent = interface;
  INSetTaskAttributeIntentHandling = interface;
  INSnoozeTasksIntent = interface;
  INSnoozeTasksIntentHandling = interface;
  INPayBillIntent = interface;
  INPayBillIntentHandling = interface;
  INRequestPaymentIntent = interface;
  INRequestPaymentIntentHandling = interface;
  INSearchForAccountsIntent = interface;
  INSearchForAccountsIntentHandling = interface;
  INSearchForBillsIntent = interface;
  INSearchForBillsIntentHandling = interface;
  INSendPaymentIntent = interface;
  INSendPaymentIntentHandling = interface;
  INTransferMoneyIntent = interface;
  INTransferMoneyIntentHandling = interface;
  INSearchForPhotosIntent = interface;
  INSearchForPhotosIntentHandling = interface;
  INStartPhotoPlaybackIntent = interface;
  INStartPhotoPlaybackIntentHandling = interface;
  INGetReservationDetailsIntent = interface;
  INGetRideStatusIntent = interface;
  INGetRideStatusIntentHandling = interface;
  INGetRideStatusIntentResponseObserver = interface;
  INListRideOptionsIntent = interface;
  INListRideOptionsIntentHandling = interface;
  INRequestRideIntent = interface;
  INRequestRideIntentHandling = interface;
  INCancelRideIntent = interface;
  INCancelRideIntentHandling = interface;
  INSendRideFeedbackIntent = interface;
  INSendRideFeedbackIntentHandling = interface;
  INGetVisualCodeIntent = interface;
  INGetVisualCodeIntentHandling = interface;
  INCallsDomainHandling = interface;
  INCarCommandsDomainHandling = interface;
  INCarPlayDomainHandling = interface;
  INWorkoutsDomainHandling = interface;
  INRadioDomainHandling = interface;
  INMessagesDomainHandling = interface;
  INPaymentsDomainHandling = interface;
  INPhotosDomainHandling = interface;
  INRidesharingDomainHandling = interface;
  INNotebookDomainHandling = interface;
  INVisualCodeDomainHandling = interface;
  INInteraction = interface;
  INSpeakable = interface;
  INParameter = interface;
  INObjectSection = interface;
  INObjectCollection = interface;
  INSearchCallHistoryIntentResponse = interface;
  INStartAudioCallIntentResponse = interface;
  INStartCallIntentResponse = interface;
  INStartVideoCallIntentResponse = interface;
  INActivateCarSignalIntentResponse = interface;
  INGetCarLockStatusIntentResponse = interface;
  INGetCarPowerLevelStatusIntentResponse = interface;
  INListCarsIntentResponse = interface;
  INSaveProfileInCarIntentResponse = interface;
  INSetAudioSourceInCarIntentResponse = interface;
  INSetCarLockStatusIntentResponse = interface;
  INSetClimateSettingsInCarIntentResponse = interface;
  INSetDefrosterSettingsInCarIntentResponse = interface;
  INSetProfileInCarIntentResponse = interface;
  INSetSeatSettingsInCarIntentResponse = interface;
  INCancelWorkoutIntentResponse = interface;
  INEndWorkoutIntentResponse = interface;
  INPauseWorkoutIntentResponse = interface;
  INResumeWorkoutIntentResponse = interface;
  INStartWorkoutIntentResponse = interface;
  INAddMediaIntentResponse = interface;
  INPlayMediaIntentResponse = interface;
  INSearchForMediaIntentResponse = interface;
  INUpdateMediaAffinityIntentResponse = interface;
  INSetRadioStationIntentResponse = interface;
  INSearchForMessagesIntentResponse = interface;
  INSendMessageIntentResponse = interface;
  INSetMessageAttributeIntentResponse = interface;
  INAddTasksIntentResponse = interface;
  INAppendToNoteIntentResponse = interface;
  INCreateNoteIntentResponse = interface;
  INCreateTaskListIntentResponse = interface;
  INDeleteTasksIntentResponse = interface;
  INSearchForNotebookItemsIntentResponse = interface;
  INSetTaskAttributeIntentResponse = interface;
  INSnoozeTasksIntentResponse = interface;
  INPayBillIntentResponse = interface;
  INRequestPaymentIntentResponse = interface;
  INSearchForAccountsIntentResponse = interface;
  INSearchForBillsIntentResponse = interface;
  INSendPaymentIntentResponse = interface;
  INTransferMoneyIntentResponse = interface;
  INSearchForPhotosIntentResponse = interface;
  INStartPhotoPlaybackIntentResponse = interface;
  INGetReservationDetailsIntentResponse = interface;
  INGetRideStatusIntentResponse = interface;
  INListRideOptionsIntentResponse = interface;
  INRequestRideIntentResponse = interface;
  INCancelRideIntentResponse = interface;
  INSendRideFeedbackIntentResponse = interface;
  INGetVisualCodeIntentResponse = interface;
  INAccountTypeResolutionResult = interface;
  INMediaDestinationResolutionResult = interface;
  INAddMediaMediaDestinationResolutionResult = interface;
  INMediaItemResolutionResult = interface;
  INAddMediaMediaItemResolutionResult = interface;
  INTaskListResolutionResult = interface;
  INAddTasksTargetTaskListResolutionResult = interface;
  INTemporalEventTriggerResolutionResult = interface;
  INAddTasksTemporalEventTriggerResolutionResult = interface;
  INAirline = interface;
  INAirport = interface;
  INAirportGate = interface;
  INBalanceTypeResolutionResult = interface;
  INBillDetails = interface;
  INBillPayee = interface;
  INBillPayeeResolutionResult = interface;
  INBillTypeResolutionResult = interface;
  INBoatTrip = interface;
  INBusTrip = interface;
  INCallCapabilityResolutionResult = interface;
  INCallDestinationTypeResolutionResult = interface;
  INCallGroup = interface;
  INCallRecord = interface;
  INCallRecordFilter = interface;
  INCallRecordResolutionResult = interface;
  INCallRecordTypeOptionsResolutionResult = interface;
  INCallRecordTypeResolutionResult = interface;
  INCar = interface;
  INCarAirCirculationModeResolutionResult = interface;
  INCarAudioSourceResolutionResult = interface;
  INCarDefrosterResolutionResult = interface;
  INCarHeadUnit = interface;
  INCarSeatResolutionResult = interface;
  INCarSignalOptionsResolutionResult = interface;
  INCurrencyAmountResolutionResult = interface;
  INDateSearchTypeResolutionResult = interface;
  INDeleteTasksTaskListResolutionResult = interface;
  INTaskResolutionResult = interface;
  INDeleteTasksTaskResolutionResult = interface;
  INFileResolutionResult = interface;
  INFlight = interface;
  INLocationSearchTypeResolutionResult = interface;
  INMediaAffinityTypeResolutionResult = interface;
  INMediaDestination = interface;
  INMediaSearch = interface;
  INMessageAttributeOptionsResolutionResult = interface;
  INMessageAttributeResolutionResult = interface;
  INNote = interface;
  INNoteContentResolutionResult = interface;
  INNoteContentTypeResolutionResult = interface;
  INNoteResolutionResult = interface;
  INNotebookItemTypeResolutionResult = interface;
  INOutgoingMessageTypeResolutionResult = interface;
  INPaymentAccount = interface;
  INPaymentAccountResolutionResult = interface;
  INPaymentAmount = interface;
  INPaymentAmountResolutionResult = interface;
  INPaymentMethod = interface;
  INPaymentMethodResolutionResult = interface;
  INPaymentRecord = interface;
  INPaymentStatusResolutionResult = interface;
  INPlayMediaMediaItemResolutionResult = interface;
  INDoubleResolutionResult = interface;
  INPlayMediaPlaybackSpeedResolutionResult = interface;
  INPlaybackQueueLocationResolutionResult = interface;
  INPlaybackRepeatModeResolutionResult = interface;
  INRadioTypeResolutionResult = interface;
  INRelativeReferenceResolutionResult = interface;
  INRelativeSettingResolutionResult = interface;
  INRentalCar = interface;
  INRequestPaymentCurrencyAmountResolutionResult = interface;
  INPersonResolutionResult = interface;
  INRequestPaymentPayerResolutionResult = interface;
  INSearchForMediaMediaItemResolutionResult = interface;
  INSeat = interface;
  INSendMessageRecipientResolutionResult = interface;
  INSendPaymentCurrencyAmountResolutionResult = interface;
  INSendPaymentPayeeResolutionResult = interface;
  INSetTaskAttributeTemporalEventTriggerResolutionResult = interface;
  INSnoozeTasksTaskResolutionResult = interface;
  INSpatialEventTrigger = interface;
  INSpatialEventTriggerResolutionResult = interface;
  INStartCallCallCapabilityResolutionResult = interface;
  INStartCallCallRecordToCallBackResolutionResult = interface;
  INStartCallContactResolutionResult = interface;
  INTask = interface;
  INTaskList = interface;
  INTaskPriorityResolutionResult = interface;
  INTaskStatusResolutionResult = interface;
  INTemporalEventTrigger = interface;
  INTemporalEventTriggerTypeOptionsResolutionResult = interface;
  INTicketedEvent = interface;
  INTrainTrip = interface;
  INUpdateMediaAffinityMediaItemResolutionResult = interface;
  INVisualCodeTypeResolutionResult = interface;
  INWorkoutGoalUnitTypeResolutionResult = interface;
  INWorkoutLocationTypeResolutionResult = interface;
  INExtension = interface;
  INPersonHandle = interface;
  INCurrencyAmount = interface;
  INDateComponentsRange = interface;
  INImage = interface;
  INSpeakableString = interface;
  INObject = interface;
  INPerson = interface;
  INRecurrenceRule = interface;
  INFile = interface;
  INBooleanResolutionResult = interface;
  INDateComponentsRangeResolutionResult = interface;
  INIntegerResolutionResult = interface;
  INPlacemarkResolutionResult = interface;
  INSpeakableStringResolutionResult = interface;
  INStringResolutionResult = interface;
  INTemperatureResolutionResult = interface;
  INDateComponentsResolutionResult = interface;
  INRestaurantResolutionResult = interface;
  INRestaurantGuestResolutionResult = interface;
  INURLResolutionResult = interface;
  INLengthResolutionResult = interface;
  INMassResolutionResult = interface;
  INVolumeResolutionResult = interface;
  INSpeedResolutionResult = interface;
  INEnergyResolutionResult = interface;
  INEnumResolutionResult = interface;
  INObjectResolutionResult = interface;
  INTimeIntervalResolutionResult = interface;
  INMessage = interface;
  INSendMessageAttachment = interface;
  INBalanceAmount = interface;
  INPriceRange = interface;
  INRideOption = interface;
  INRideStatus = interface;
  INRideDriver = interface;
  INRideVehicle = interface;
  INRideFareLineItem = interface;
  INRidePartySizeOption = interface;
  INRideCompletionStatus = interface;
  INReservation = interface;
  INReservationAction = interface;
  INFlightReservation = interface;
  INLodgingReservation = interface;
  INRentalCarReservation = interface;
  INRestaurantReservation = interface;
  INTicketedEventReservation = interface;
  INTrainReservation = interface;
  INBusReservation = interface;
  INBoatReservation = interface;
  INRestaurantGuest = interface;
  INTermsAndConditions = interface;
  INRestaurantGuestDisplayPreferences = interface;
  INRestaurant = interface;
  INRestaurantOffer = interface;
  INRestaurantReservationBooking = interface;
  INRestaurantReservationUserBooking = interface;
  INBookRestaurantReservationIntent = interface;
  INBookRestaurantReservationIntentHandling = interface;
  INGetAvailableRestaurantReservationBookingsIntent = interface;
  INGetAvailableRestaurantReservationBookingsIntentHandling = interface;
  INGetUserCurrentRestaurantReservationBookingsIntent = interface;
  INGetUserCurrentRestaurantReservationBookingsIntentHandling = interface;
  INGetAvailableRestaurantReservationBookingDefaultsIntent = interface;
  INGetAvailableRestaurantReservationBookingDefaultsIntentHandling = interface;
  INBookRestaurantReservationIntentResponse = interface;
  INGetAvailableRestaurantReservationBookingsIntentResponse = interface;
  INGetUserCurrentRestaurantReservationBookingsIntentResponse = interface;
  INGetAvailableRestaurantReservationBookingDefaultsIntentResponse = interface;
  INGetRestaurantGuestIntent = interface;
  INGetRestaurantGuestIntentHandling = interface;
  INGetRestaurantGuestIntentResponse = interface;
  INVocabulary = interface;
  INUpcomingMediaManager = interface;
  INPreferences = interface;
  INUserContext = interface;
  INMediaUserContext = interface;
  INNoteContent = interface;
  INTextNoteContent = interface;
  INImageNoteContent = interface;
  INRelevanceProvider = interface;
  INDateRelevanceProvider = interface;
  INLocationRelevanceProvider = interface;
  INDailyRoutineRelevanceProvider = interface;
  INDefaultCardTemplate = interface;
  INShortcut = interface;
  INRelevantShortcut = interface;
  INRelevantShortcutStore = interface;
  INVoiceShortcut = interface;
  INVoiceShortcutCenter = interface;
  INMediaItem = interface;

  INShortcutAvailabilityOptions = NSInteger;
  INIntentErrorCode = NSInteger;
  INCallCapabilityOptions = NSInteger;
  INCallRecordType = NSInteger;
  INCallRecordTypeOptions = NSInteger;
  INCallDestinationType = NSInteger;
  INCallAudioRoute = NSInteger;
  INCallCapability = NSInteger;
  INCarSignalOptions = NSInteger;
  INCarAudioSource = NSInteger;
  INRelativeReference = NSInteger;
  INCarAirCirculationMode = NSInteger;
  INCarSeat = NSInteger;
  INRelativeSetting = NSInteger;
  INCarDefroster = NSInteger;
  INWorkoutGoalUnitType = NSInteger;
  INWorkoutLocationType = NSInteger;
  INPlaybackQueueLocation = NSInteger;
  INPlaybackRepeatMode = NSInteger;
  INMediaAffinityType = NSInteger;
  INRadioType = NSInteger;
  INConditionalOperator = NSInteger;
  INMessageAttributeOptions = NSInteger;
  INOutgoingMessageType = NSInteger;
  INMessageAttribute = NSInteger;
  INTaskPriority = NSInteger;
  INDateSearchType = NSInteger;
  INLocationSearchType = NSInteger;
  INNotebookItemType = NSInteger;
  INTaskStatus = NSInteger;
  INTemporalEventTriggerTypeOptions = NSInteger;
  INBillType = NSInteger;
  INAccountType = NSInteger;
  INBalanceType = NSInteger;
  INPaymentStatus = NSInteger;
  INPhotoAttributeOptions = NSInteger;
  INVisualCodeType = NSInteger;
  INIntentHandlingStatus = NSInteger;
  INInteractionDirection = NSInteger;
  INSearchCallHistoryIntentResponseCode = NSInteger;
  INStartAudioCallIntentResponseCode = NSInteger;
  INStartCallIntentResponseCode = NSInteger;
  INStartVideoCallIntentResponseCode = NSInteger;
  INActivateCarSignalIntentResponseCode = NSInteger;
  INGetCarLockStatusIntentResponseCode = NSInteger;
  INCarChargingConnectorType = NSString;
  INGetCarPowerLevelStatusIntentResponseCode = NSInteger;
  INListCarsIntentResponseCode = NSInteger;
  INSaveProfileInCarIntentResponseCode = NSInteger;
  INSetAudioSourceInCarIntentResponseCode = NSInteger;
  INSetCarLockStatusIntentResponseCode = NSInteger;
  INSetClimateSettingsInCarIntentResponseCode = NSInteger;
  INSetDefrosterSettingsInCarIntentResponseCode = NSInteger;
  INSetProfileInCarIntentResponseCode = NSInteger;
  INSetSeatSettingsInCarIntentResponseCode = NSInteger;
  INCancelWorkoutIntentResponseCode = NSInteger;
  INEndWorkoutIntentResponseCode = NSInteger;
  INPauseWorkoutIntentResponseCode = NSInteger;
  INResumeWorkoutIntentResponseCode = NSInteger;
  INStartWorkoutIntentResponseCode = NSInteger;
  INAddMediaIntentResponseCode = NSInteger;
  INPlayMediaIntentResponseCode = NSInteger;
  INSearchForMediaIntentResponseCode = NSInteger;
  INUpdateMediaAffinityIntentResponseCode = NSInteger;
  INSetRadioStationIntentResponseCode = NSInteger;
  INSearchForMessagesIntentResponseCode = NSInteger;
  INSendMessageIntentResponseCode = NSInteger;
  INSetMessageAttributeIntentResponseCode = NSInteger;
  INAddTasksIntentResponseCode = NSInteger;
  INAppendToNoteIntentResponseCode = NSInteger;
  INCreateNoteIntentResponseCode = NSInteger;
  INCreateTaskListIntentResponseCode = NSInteger;
  INDeleteTasksIntentResponseCode = NSInteger;
  INSortType = NSInteger;
  INSearchForNotebookItemsIntentResponseCode = NSInteger;
  INSetTaskAttributeIntentResponseCode = NSInteger;
  INSnoozeTasksIntentResponseCode = NSInteger;
  INPayBillIntentResponseCode = NSInteger;
  INRequestPaymentIntentResponseCode = NSInteger;
  INSearchForAccountsIntentResponseCode = NSInteger;
  INSearchForBillsIntentResponseCode = NSInteger;
  INSendPaymentIntentResponseCode = NSInteger;
  INTransferMoneyIntentResponseCode = NSInteger;
  INSearchForPhotosIntentResponseCode = NSInteger;
  INStartPhotoPlaybackIntentResponseCode = NSInteger;
  INGetReservationDetailsIntentResponseCode = NSInteger;
  INGetRideStatusIntentResponseCode = NSInteger;
  INListRideOptionsIntentResponseCode = NSInteger;
  INRequestRideIntentResponseCode = NSInteger;
  INCancelRideIntentResponseCode = NSInteger;
  INSendRideFeedbackIntentResponseCode = NSInteger;
  INGetVisualCodeIntentResponseCode = NSInteger;
  INAddMediaMediaDestinationUnsupportedReason = NSInteger;
  INAddMediaMediaItemUnsupportedReason = NSInteger;
  INAddTasksTargetTaskListConfirmationReason = NSInteger;
  INAddTasksTemporalEventTriggerUnsupportedReason = NSInteger;
  INAmountType = NSInteger;
  INDayOfWeekOptions = NSInteger;
  INDeleteTasksTaskListUnsupportedReason = NSInteger;
  INDeleteTasksTaskUnsupportedReason = NSInteger;
  INMediaDestinationType = NSInteger;
  INMediaItemType = NSInteger;
  INMediaReference = NSInteger;
  INMediaSortOrder = NSInteger;
  INNoteContentType = NSInteger;
  INPaymentMethodType = NSInteger;
  INPersonHandleLabel = NSString;
  INPersonRelationship = NSString;
  INPlayMediaMediaItemUnsupportedReason = NSInteger;
  INPlayMediaPlaybackSpeedUnsupportedReason = NSInteger;
  INRecurrenceFrequency = NSInteger;
  INRequestPaymentCurrencyAmountUnsupportedReason = NSInteger;
  INRequestPaymentPayerUnsupportedReason = NSInteger;
  INReservationActionType = NSInteger;
  INReservationStatus = NSInteger;
  INRideFeedbackTypeOptions = NSInteger;
  INRidePhase = NSInteger;
  INSearchForMediaMediaItemUnsupportedReason = NSInteger;
  INSendMessageRecipientUnsupportedReason = NSInteger;
  INSendPaymentCurrencyAmountUnsupportedReason = NSInteger;
  INSendPaymentPayeeUnsupportedReason = NSInteger;
  INSetTaskAttributeTemporalEventTriggerUnsupportedReason = NSInteger;
  INSnoozeTasksTaskUnsupportedReason = NSInteger;
  INSpatialEvent = NSInteger;
  INStartCallCallCapabilityUnsupportedReason = NSInteger;
  INStartCallCallRecordToCallBackUnsupportedReason = NSInteger;
  INStartCallContactUnsupportedReason = NSInteger;
  INTaskType = NSInteger;
  INTicketedEventCategory = NSInteger;
  INUpdateMediaAffinityMediaItemUnsupportedReason = NSInteger;
  INWorkoutNameIdentifier = NSString;
  INPersonHandleType = NSInteger;
  INPersonSuggestionType = NSInteger;
  INMessageType = NSInteger;
  INRestaurantReservationUserBookingStatus = NSInteger;
  INBookRestaurantReservationIntentCode = NSInteger;
  INGetAvailableRestaurantReservationBookingsIntentCode = NSInteger;
  INGetUserCurrentRestaurantReservationBookingsIntentResponseCode = NSInteger;
  INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode = NSInteger;
  INGetRestaurantGuestIntentResponseCode = NSInteger;
  INVocabularyStringType = NSInteger;
  INUpcomingMediaPredictionMode = NSInteger;
  INSiriAuthorizationStatus = NSInteger;
  INMediaUserContextSubscriptionStatus = NSInteger;
  INDailyRoutineSituation = NSInteger;
  INRelevantShortcutRole = NSInteger;
  TINSearchCallHistoryIntentHandlingBlockMethod1 = procedure(response: INSearchCallHistoryIntentResponse) of object;
  TINSearchCallHistoryIntentHandlingBlockMethod2 = procedure(resolutionResult: INCallRecordTypeResolutionResult) of object;
  TINSearchCallHistoryIntentHandlingBlockMethod3 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINSearchCallHistoryIntentHandlingBlockMethod4 = procedure(resolutionResult: INPersonResolutionResult) of object;
  TINSearchCallHistoryIntentHandlingBlockMethod5 = procedure(resolutionResult: INCallRecordTypeOptionsResolutionResult) of object;
  TINSearchCallHistoryIntentHandlingBlockMethod6 = procedure(resolutionResult: INBooleanResolutionResult) of object;
  TINStartAudioCallIntentHandlingBlockMethod1 = procedure(response: INStartAudioCallIntentResponse) of object;
  TINStartAudioCallIntentHandlingBlockMethod2 = procedure(resolutionResult: INCallDestinationTypeResolutionResult) of object;
  TINStartAudioCallIntentHandlingBlockMethod3 = procedure(resolutionResults: NSArray) of object;
  TINStartCallIntentHandlingBlockMethod1 = procedure(response: INStartCallIntentResponse) of object;
  TINStartCallIntentHandlingBlockMethod2 = procedure(resolutionResult: INCallRecordResolutionResult) of object;
  TINStartCallIntentHandlingBlockMethod3 = procedure(resolutionResult: INCallDestinationTypeResolutionResult) of object;
  TINStartCallIntentHandlingBlockMethod4 = procedure(resolutionResults: NSArray) of object;
  TINStartCallIntentHandlingBlockMethod5 = procedure(resolutionResult: INStartCallCallCapabilityResolutionResult) of object;
  TINStartVideoCallIntentHandlingBlockMethod1 = procedure(response: INStartVideoCallIntentResponse) of object;
  TINStartVideoCallIntentHandlingBlockMethod2 = procedure(resolutionResults: NSArray) of object;
  TINActivateCarSignalIntentHandlingBlockMethod1 = procedure(response: INActivateCarSignalIntentResponse) of object;
  TINActivateCarSignalIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINActivateCarSignalIntentHandlingBlockMethod3 = procedure(resolutionResult: INCarSignalOptionsResolutionResult) of object;
  TINGetCarLockStatusIntentHandlingBlockMethod1 = procedure(response: INGetCarLockStatusIntentResponse) of object;
  TINGetCarLockStatusIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINGetCarPowerLevelStatusIntentHandlingBlockMethod1 = procedure(response: INGetCarPowerLevelStatusIntentResponse) of object;
  TINGetCarPowerLevelStatusIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINListCarsIntentHandlingBlockMethod1 = procedure(response: INListCarsIntentResponse) of object;
  TINSaveProfileInCarIntentHandlingBlockMethod1 = procedure(response: INSaveProfileInCarIntentResponse) of object;
  TINSaveProfileInCarIntentHandlingBlockMethod2 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINSaveProfileInCarIntentHandlingBlockMethod3 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINSetAudioSourceInCarIntentHandlingBlockMethod1 = procedure(response: INSetAudioSourceInCarIntentResponse) of object;
  TINSetAudioSourceInCarIntentHandlingBlockMethod2 = procedure(resolutionResult: INCarAudioSourceResolutionResult) of object;
  TINSetAudioSourceInCarIntentHandlingBlockMethod3 = procedure(resolutionResult: INRelativeReferenceResolutionResult) of object;
  TINSetCarLockStatusIntentHandlingBlockMethod1 = procedure(response: INSetCarLockStatusIntentResponse) of object;
  TINSetCarLockStatusIntentHandlingBlockMethod2 = procedure(resolutionResult: INBooleanResolutionResult) of object;
  TINSetCarLockStatusIntentHandlingBlockMethod3 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod1 = procedure(response: INSetClimateSettingsInCarIntentResponse) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod2 = procedure(resolutionResult: INBooleanResolutionResult) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod3 = procedure(resolutionResult: INCarAirCirculationModeResolutionResult) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod4 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod5 = procedure(resolutionResult: INDoubleResolutionResult) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod6 = procedure(resolutionResult: INRelativeSettingResolutionResult) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod7 = procedure(resolutionResult: INTemperatureResolutionResult) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod8 = procedure(resolutionResult: INCarSeatResolutionResult) of object;
  TINSetClimateSettingsInCarIntentHandlingBlockMethod9 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINSetDefrosterSettingsInCarIntentHandlingBlockMethod1 = procedure(response: INSetDefrosterSettingsInCarIntentResponse) of object;
  TINSetDefrosterSettingsInCarIntentHandlingBlockMethod2 = procedure(resolutionResult: INBooleanResolutionResult) of object;
  TINSetDefrosterSettingsInCarIntentHandlingBlockMethod3 = procedure(resolutionResult: INCarDefrosterResolutionResult) of object;
  TINSetDefrosterSettingsInCarIntentHandlingBlockMethod4 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINSetProfileInCarIntentHandlingBlockMethod1 = procedure(response: INSetProfileInCarIntentResponse) of object;
  TINSetProfileInCarIntentHandlingBlockMethod2 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINSetProfileInCarIntentHandlingBlockMethod3 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINSetProfileInCarIntentHandlingBlockMethod4 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINSetProfileInCarIntentHandlingBlockMethod5 = procedure(resolutionResult: INBooleanResolutionResult) of object;
  TINSetSeatSettingsInCarIntentHandlingBlockMethod1 = procedure(response: INSetSeatSettingsInCarIntentResponse) of object;
  TINSetSeatSettingsInCarIntentHandlingBlockMethod2 = procedure(resolutionResult: INBooleanResolutionResult) of object;
  TINSetSeatSettingsInCarIntentHandlingBlockMethod3 = procedure(resolutionResult: INCarSeatResolutionResult) of object;
  TINSetSeatSettingsInCarIntentHandlingBlockMethod4 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINSetSeatSettingsInCarIntentHandlingBlockMethod5 = procedure(resolutionResult: INRelativeSettingResolutionResult) of object;
  TINSetSeatSettingsInCarIntentHandlingBlockMethod6 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINCancelWorkoutIntentHandlingBlockMethod1 = procedure(response: INCancelWorkoutIntentResponse) of object;
  TINCancelWorkoutIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINEndWorkoutIntentHandlingBlockMethod1 = procedure(response: INEndWorkoutIntentResponse) of object;
  TINEndWorkoutIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINPauseWorkoutIntentHandlingBlockMethod1 = procedure(response: INPauseWorkoutIntentResponse) of object;
  TINPauseWorkoutIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINResumeWorkoutIntentHandlingBlockMethod1 = procedure(response: INResumeWorkoutIntentResponse) of object;
  TINResumeWorkoutIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINStartWorkoutIntentHandlingBlockMethod1 = procedure(response: INStartWorkoutIntentResponse) of object;
  TINStartWorkoutIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINStartWorkoutIntentHandlingBlockMethod3 = procedure(resolutionResult: INDoubleResolutionResult) of object;
  TINStartWorkoutIntentHandlingBlockMethod4 = procedure(resolutionResult: INWorkoutGoalUnitTypeResolutionResult) of object;
  TINStartWorkoutIntentHandlingBlockMethod5 = procedure(resolutionResult: INWorkoutLocationTypeResolutionResult) of object;
  TINStartWorkoutIntentHandlingBlockMethod6 = procedure(resolutionResult: INBooleanResolutionResult) of object;
  TINAddMediaIntentHandlingBlockMethod1 = procedure(response: INAddMediaIntentResponse) of object;
  TINAddMediaIntentHandlingBlockMethod2 = procedure(resolutionResults: NSArray) of object;
  TINAddMediaIntentHandlingBlockMethod3 = procedure(resolutionResult: INAddMediaMediaDestinationResolutionResult) of object;
  TINPlayMediaIntentHandlingBlockMethod1 = procedure(response: INPlayMediaIntentResponse) of object;
  TINPlayMediaIntentHandlingBlockMethod2 = procedure(resolutionResults: NSArray) of object;
  TINPlayMediaIntentHandlingBlockMethod3 = procedure(resolutionResult: INBooleanResolutionResult) of object;
  TINPlayMediaIntentHandlingBlockMethod4 = procedure(resolutionResult: INPlaybackRepeatModeResolutionResult) of object;
  TINPlayMediaIntentHandlingBlockMethod5 = procedure(resolutionResult: INPlaybackQueueLocationResolutionResult) of object;
  TINPlayMediaIntentHandlingBlockMethod6 = procedure(resolutionResult: INPlayMediaPlaybackSpeedResolutionResult) of object;
  TINSearchForMediaIntentHandlingBlockMethod1 = procedure(response: INSearchForMediaIntentResponse) of object;
  TINSearchForMediaIntentHandlingBlockMethod2 = procedure(resolutionResults: NSArray) of object;
  TINUpdateMediaAffinityIntentHandlingBlockMethod1 = procedure(response: INUpdateMediaAffinityIntentResponse) of object;
  TINUpdateMediaAffinityIntentHandlingBlockMethod2 = procedure(resolutionResults: NSArray) of object;
  TINUpdateMediaAffinityIntentHandlingBlockMethod3 = procedure(resolutionResult: INMediaAffinityTypeResolutionResult) of object;
  TINSetRadioStationIntentHandlingBlockMethod1 = procedure(response: INSetRadioStationIntentResponse) of object;
  TINSetRadioStationIntentHandlingBlockMethod2 = procedure(resolutionResult: INRadioTypeResolutionResult) of object;
  TINSetRadioStationIntentHandlingBlockMethod3 = procedure(resolutionResult: INDoubleResolutionResult) of object;
  TINSetRadioStationIntentHandlingBlockMethod4 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINSetRadioStationIntentHandlingBlockMethod5 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINSearchForMessagesIntentHandlingBlockMethod1 = procedure(response: INSearchForMessagesIntentResponse) of object;
  TINSearchForMessagesIntentHandlingBlockMethod2 = procedure(resolutionResults: NSArray) of object;
  TINSearchForMessagesIntentHandlingBlockMethod3 = procedure(resolutionResult: INMessageAttributeOptionsResolutionResult) of object;
  TINSearchForMessagesIntentHandlingBlockMethod4 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINSendMessageIntentHandlingBlockMethod1 = procedure(response: INSendMessageIntentResponse) of object;
  TINSendMessageIntentHandlingBlockMethod2 = procedure(resolutionResults: NSArray) of object;
  TINSendMessageIntentHandlingBlockMethod3 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINSendMessageIntentHandlingBlockMethod4 = procedure(resolutionResult: INOutgoingMessageTypeResolutionResult) of object;
  TINSendMessageIntentHandlingBlockMethod5 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINSetMessageAttributeIntentHandlingBlockMethod1 = procedure(response: INSetMessageAttributeIntentResponse) of object;
  TINSetMessageAttributeIntentHandlingBlockMethod2 = procedure(resolutionResult: INMessageAttributeResolutionResult) of object;
  TINAddTasksIntentHandlingBlockMethod1 = procedure(response: INAddTasksIntentResponse) of object;
  TINAddTasksIntentHandlingBlockMethod2 = procedure(resolutionResult: INTaskListResolutionResult) of object;
  TINAddTasksIntentHandlingBlockMethod3 = procedure(resolutionResult: INAddTasksTargetTaskListResolutionResult) of object;
  TINAddTasksIntentHandlingBlockMethod4 = procedure(resolutionResults: NSArray) of object;
  TINAddTasksIntentHandlingBlockMethod5 = procedure(resolutionResult: INSpatialEventTriggerResolutionResult) of object;
  TINAddTasksIntentHandlingBlockMethod6 = procedure(resolutionResult: INTemporalEventTriggerResolutionResult) of object;
  TINAddTasksIntentHandlingBlockMethod7 = procedure(resolutionResult: INAddTasksTemporalEventTriggerResolutionResult) of object;
  TINAddTasksIntentHandlingBlockMethod8 = procedure(resolutionResult: INTaskPriorityResolutionResult) of object;
  TINAppendToNoteIntentHandlingBlockMethod1 = procedure(response: INAppendToNoteIntentResponse) of object;
  TINAppendToNoteIntentHandlingBlockMethod2 = procedure(resolutionResult: INNoteResolutionResult) of object;
  TINAppendToNoteIntentHandlingBlockMethod3 = procedure(resolutionResult: INNoteContentResolutionResult) of object;
  TINCreateNoteIntentHandlingBlockMethod1 = procedure(response: INCreateNoteIntentResponse) of object;
  TINCreateNoteIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINCreateNoteIntentHandlingBlockMethod3 = procedure(resolutionResult: INNoteContentResolutionResult) of object;
  TINCreateTaskListIntentHandlingBlockMethod1 = procedure(response: INCreateTaskListIntentResponse) of object;
  TINCreateTaskListIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINCreateTaskListIntentHandlingBlockMethod3 = procedure(resolutionResults: NSArray) of object;
  TINDeleteTasksIntentHandlingBlockMethod1 = procedure(response: INDeleteTasksIntentResponse) of object;
  TINDeleteTasksIntentHandlingBlockMethod2 = procedure(resolutionResult: INDeleteTasksTaskListResolutionResult) of object;
  TINDeleteTasksIntentHandlingBlockMethod3 = procedure(resolutionResults: NSArray) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod1 = procedure(response: INSearchForNotebookItemsIntentResponse) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod3 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod4 = procedure(resolutionResult: INNotebookItemTypeResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod5 = procedure(resolutionResult: INTaskStatusResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod6 = procedure(resolutionResult: INPlacemarkResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod7 = procedure(resolutionResult: INLocationSearchTypeResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod8 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod9 = procedure(resolutionResult: INDateSearchTypeResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod10 = procedure(resolutionResult: INTemporalEventTriggerTypeOptionsResolutionResult) of object;
  TINSearchForNotebookItemsIntentHandlingBlockMethod11 = procedure(resolutionResult: INTaskPriorityResolutionResult) of object;
  TINSetTaskAttributeIntentHandlingBlockMethod1 = procedure(response: INSetTaskAttributeIntentResponse) of object;
  TINSetTaskAttributeIntentHandlingBlockMethod2 = procedure(resolutionResult: INTaskResolutionResult) of object;
  TINSetTaskAttributeIntentHandlingBlockMethod3 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINSetTaskAttributeIntentHandlingBlockMethod4 = procedure(resolutionResult: INTaskStatusResolutionResult) of object;
  TINSetTaskAttributeIntentHandlingBlockMethod5 = procedure(resolutionResult: INTaskPriorityResolutionResult) of object;
  TINSetTaskAttributeIntentHandlingBlockMethod6 = procedure(resolutionResult: INSpatialEventTriggerResolutionResult) of object;
  TINSetTaskAttributeIntentHandlingBlockMethod7 = procedure(resolutionResult: INTemporalEventTriggerResolutionResult) of object;
  TINSetTaskAttributeIntentHandlingBlockMethod8 = procedure(resolutionResult: INSetTaskAttributeTemporalEventTriggerResolutionResult) of object;
  TINSnoozeTasksIntentHandlingBlockMethod1 = procedure(response: INSnoozeTasksIntentResponse) of object;
  TINSnoozeTasksIntentHandlingBlockMethod2 = procedure(resolutionResults: NSArray) of object;
  TINSnoozeTasksIntentHandlingBlockMethod3 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINPayBillIntentHandlingBlockMethod1 = procedure(response: INPayBillIntentResponse) of object;
  TINPayBillIntentHandlingBlockMethod2 = procedure(resolutionResult: INBillPayeeResolutionResult) of object;
  TINPayBillIntentHandlingBlockMethod3 = procedure(resolutionResult: INPaymentAccountResolutionResult) of object;
  TINPayBillIntentHandlingBlockMethod4 = procedure(resolutionResult: INPaymentAmountResolutionResult) of object;
  TINPayBillIntentHandlingBlockMethod5 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINPayBillIntentHandlingBlockMethod6 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINPayBillIntentHandlingBlockMethod7 = procedure(resolutionResult: INBillTypeResolutionResult) of object;
  TINRequestPaymentIntentHandlingBlockMethod1 = procedure(response: INRequestPaymentIntentResponse) of object;
  TINRequestPaymentIntentHandlingBlockMethod2 = procedure(resolutionResult: INPersonResolutionResult) of object;
  TINRequestPaymentIntentHandlingBlockMethod3 = procedure(resolutionResult: INRequestPaymentPayerResolutionResult) of object;
  TINRequestPaymentIntentHandlingBlockMethod4 = procedure(resolutionResult: INCurrencyAmountResolutionResult) of object;
  TINRequestPaymentIntentHandlingBlockMethod5 = procedure(resolutionResult: INRequestPaymentCurrencyAmountResolutionResult) of object;
  TINRequestPaymentIntentHandlingBlockMethod6 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINSearchForAccountsIntentHandlingBlockMethod1 = procedure(response: INSearchForAccountsIntentResponse) of object;
  TINSearchForAccountsIntentHandlingBlockMethod2 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINSearchForAccountsIntentHandlingBlockMethod3 = procedure(resolutionResult: INAccountTypeResolutionResult) of object;
  TINSearchForAccountsIntentHandlingBlockMethod4 = procedure(resolutionResult: INBalanceTypeResolutionResult) of object;
  TINSearchForBillsIntentHandlingBlockMethod1 = procedure(response: INSearchForBillsIntentResponse) of object;
  TINSearchForBillsIntentHandlingBlockMethod2 = procedure(resolutionResult: INBillPayeeResolutionResult) of object;
  TINSearchForBillsIntentHandlingBlockMethod3 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINSearchForBillsIntentHandlingBlockMethod4 = procedure(resolutionResult: INBillTypeResolutionResult) of object;
  TINSearchForBillsIntentHandlingBlockMethod5 = procedure(resolutionResult: INPaymentStatusResolutionResult) of object;
  TINSendPaymentIntentHandlingBlockMethod1 = procedure(response: INSendPaymentIntentResponse) of object;
  TINSendPaymentIntentHandlingBlockMethod2 = procedure(resolutionResult: INPersonResolutionResult) of object;
  TINSendPaymentIntentHandlingBlockMethod3 = procedure(resolutionResult: INSendPaymentPayeeResolutionResult) of object;
  TINSendPaymentIntentHandlingBlockMethod4 = procedure(resolutionResult: INCurrencyAmountResolutionResult) of object;
  TINSendPaymentIntentHandlingBlockMethod5 = procedure(resolutionResult: INSendPaymentCurrencyAmountResolutionResult) of object;
  TINSendPaymentIntentHandlingBlockMethod6 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINTransferMoneyIntentHandlingBlockMethod1 = procedure(response: INTransferMoneyIntentResponse) of object;
  TINTransferMoneyIntentHandlingBlockMethod2 = procedure(resolutionResult: INPaymentAccountResolutionResult) of object;
  TINTransferMoneyIntentHandlingBlockMethod3 = procedure(resolutionResult: INPaymentAmountResolutionResult) of object;
  TINTransferMoneyIntentHandlingBlockMethod4 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINTransferMoneyIntentHandlingBlockMethod5 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINSearchForPhotosIntentHandlingBlockMethod1 = procedure(response: INSearchForPhotosIntentResponse) of object;
  TINSearchForPhotosIntentHandlingBlockMethod2 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINSearchForPhotosIntentHandlingBlockMethod3 = procedure(resolutionResult: INPlacemarkResolutionResult) of object;
  TINSearchForPhotosIntentHandlingBlockMethod4 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINSearchForPhotosIntentHandlingBlockMethod5 = procedure(resolutionResults: NSArray) of object;
  TINStartPhotoPlaybackIntentHandlingBlockMethod1 = procedure(response: INStartPhotoPlaybackIntentResponse) of object;
  TINStartPhotoPlaybackIntentHandlingBlockMethod2 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINStartPhotoPlaybackIntentHandlingBlockMethod3 = procedure(resolutionResult: INPlacemarkResolutionResult) of object;
  TINStartPhotoPlaybackIntentHandlingBlockMethod4 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINStartPhotoPlaybackIntentHandlingBlockMethod5 = procedure(resolutionResults: NSArray) of object;
  TINGetRideStatusIntentHandlingBlockMethod1 = procedure(response: INGetRideStatusIntentResponse) of object;
  TINListRideOptionsIntentHandlingBlockMethod1 = procedure(response: INListRideOptionsIntentResponse) of object;
  TINListRideOptionsIntentHandlingBlockMethod2 = procedure(resolutionResult: INPlacemarkResolutionResult) of object;
  TINRequestRideIntentHandlingBlockMethod1 = procedure(response: INRequestRideIntentResponse) of object;
  TINRequestRideIntentHandlingBlockMethod2 = procedure(resolutionResult: INPlacemarkResolutionResult) of object;
  TINRequestRideIntentHandlingBlockMethod3 = procedure(resolutionResult: INSpeakableStringResolutionResult) of object;
  TINRequestRideIntentHandlingBlockMethod4 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINRequestRideIntentHandlingBlockMethod5 = procedure(resolutionResult: INDateComponentsRangeResolutionResult) of object;
  TINCancelRideIntentHandlingBlockMethod1 = procedure(response: INCancelRideIntentResponse) of object;
  TINSendRideFeedbackIntentHandlingBlockMethod1 = procedure(response: INSendRideFeedbackIntentResponse) of object;
  TINGetVisualCodeIntentHandlingBlockMethod1 = procedure(response: INGetVisualCodeIntentResponse) of object;
  TINGetVisualCodeIntentHandlingBlockMethod2 = procedure(resolutionResult: INVisualCodeTypeResolutionResult) of object;
  TINInteractionBlockMethod1 = procedure(error: NSError) of object;
  TINBookRestaurantReservationIntentHandlingBlockMethod1 = procedure(response: INBookRestaurantReservationIntentResponse) of object;
  TINBookRestaurantReservationIntentHandlingBlockMethod2 = procedure(resolutionResult: INRestaurantResolutionResult) of object;
  TINBookRestaurantReservationIntentHandlingBlockMethod3 = procedure(resolutionResult: INDateComponentsResolutionResult) of object;
  TINBookRestaurantReservationIntentHandlingBlockMethod4 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINBookRestaurantReservationIntentHandlingBlockMethod5 = procedure(resolutionResult: INRestaurantGuestResolutionResult) of object;
  TINBookRestaurantReservationIntentHandlingBlockMethod6 = procedure(resolutionResult: INStringResolutionResult) of object;
  TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod1 =
    procedure(response: INGetAvailableRestaurantReservationBookingsIntentResponse) of object;
  TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod2 = procedure(resolutionResult: INRestaurantResolutionResult) of object;
  TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod3 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod4 = procedure(resolutionResult: INDateComponentsResolutionResult) of object;
  TINGetUserCurrentRestaurantReservationBookingsIntentHandlingBlockMethod1 =
    procedure(response: INGetUserCurrentRestaurantReservationBookingsIntentResponse) of object;
  TINGetUserCurrentRestaurantReservationBookingsIntentHandlingBlockMethod2 = procedure(resolutionResult: INRestaurantResolutionResult) of object;
  TINGetAvailableRestaurantReservationBookingDefaultsIntentHandlingBlockMethod1 =
    procedure(response: INGetAvailableRestaurantReservationBookingDefaultsIntentResponse) of object;
  TINGetAvailableRestaurantReservationBookingDefaultsIntentHandlingBlockMethod2 = procedure(resolutionResult: INRestaurantResolutionResult) of object;
  TINGetRestaurantGuestIntentHandlingBlockMethod1 = procedure(response: INGetRestaurantGuestIntentResponse) of object;
  TINPreferencesBlockMethod1 = procedure(status: INSiriAuthorizationStatus) of object;
  TINRelevantShortcutStoreBlockMethod1 = procedure(error: NSError) of object;
  TINVoiceShortcutCenterBlockMethod1 = procedure(voiceShortcuts: NSArray; error: NSError) of object;
  TINVoiceShortcutCenterBlockMethod2 = procedure(voiceShortcut: INVoiceShortcut; error: NSError) of object;

  INIntentClass = interface(NSObjectClass)
    ['{0E70C80E-9E49-4286-933C-91F5B9641CC5}']
  end;

  INIntent = interface(NSObject)
    ['{80AF7CF8-FB59-45BD-9637-8AE51273468D}']
    function identifier: NSString; cdecl;
    function imageForParameterNamed(parameterName: NSString): INImage; cdecl;
    function intentDescription: NSString; cdecl;
    function keyImage: INImage; cdecl;
    procedure setImage(image: INImage; forParameterNamed: NSString); cdecl;
    procedure setShortcutAvailability(shortcutAvailability: INShortcutAvailabilityOptions); cdecl;
    procedure setSuggestedInvocationPhrase(suggestedInvocationPhrase: NSString); cdecl;
    function shortcutAvailability: INShortcutAvailabilityOptions; cdecl;
    function suggestedInvocationPhrase: NSString; cdecl;
  end;
  TINIntent = class(TOCGenericImport<INIntentClass, INIntent>) end;

  INIntentHandlerProviding = interface(IObjectiveC)
    ['{79EE77D6-74AA-43D6-946F-DD18BCA1BD0C}']
    function handlerForIntent(intent: INIntent): Pointer; cdecl;
  end;

  INIntentResponseClass = interface(NSObjectClass)
    ['{B4C85DA7-3F09-4B15-B07B-F42BE1D1E58C}']
  end;

  INIntentResponse = interface(NSObject)
    ['{294CC6A6-333D-4A72-BC11-D35A3D27F56D}']
    procedure setUserActivity(userActivity: NSUserActivity); cdecl;
    function userActivity: NSUserActivity; cdecl;
  end;
  TINIntentResponse = class(TOCGenericImport<INIntentResponseClass, INIntentResponse>) end;

  INIntentResolutionResultClass = interface(NSObjectClass)
    ['{8AFDC91B-A4FF-4970-A1E7-2028E7563F59}']
    {class} function confirmationRequiredWithItemToConfirm(itemToConfirm: Pointer; forReason: NSInteger): Pointer; cdecl;
    {class} function needsValue: Pointer; cdecl;
    {class} function notRequired: Pointer; cdecl;
    {class} function unsupported: Pointer; cdecl;
    {class} function unsupportedWithReason(reason: NSInteger): Pointer; cdecl;
  end;

  INIntentResolutionResult = interface(NSObject)
    ['{3E4AAD30-9011-49FF-BFA8-8CCCD5FAFB5B}']
  end;
  TINIntentResolutionResult = class(TOCGenericImport<INIntentResolutionResultClass, INIntentResolutionResult>) end;

  INSearchCallHistoryIntentClass = interface(INIntentClass)
    ['{8CC9BB47-AF1A-4A10-B9A3-37D52911ED14}']
  end;

  INSearchCallHistoryIntent = interface(INIntent)
    ['{231E19CC-2E90-43D4-8566-E9B96A179FCB}']
    function callCapabilities: INCallCapabilityOptions; cdecl;
    function callType: INCallRecordType; cdecl; // API_DEPRECATED("Use callTypes instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    function callTypes: INCallRecordTypeOptions; cdecl;
    function dateCreated: INDateComponentsRange; cdecl;
    function initWithCallType(callType: INCallRecordType; dateCreated: INDateComponentsRange; recipient: INPerson;
      callCapabilities: INCallCapabilityOptions): Pointer; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    function initWithDateCreated(dateCreated: INDateComponentsRange; recipient: INPerson; callCapabilities: INCallCapabilityOptions;
      callTypes: INCallRecordTypeOptions; unseen: NSNumber): Pointer; cdecl;
    function recipient: INPerson; cdecl;
    function unseen: NSNumber; cdecl;
  end;
  TINSearchCallHistoryIntent = class(TOCGenericImport<INSearchCallHistoryIntentClass, INSearchCallHistoryIntent>) end;

  INSearchCallHistoryIntentHandling = interface(IObjectiveC)
    ['{A764F6C1-7958-47E4-8F06-9D0B8F0D9E91}']
    procedure confirmSearchCallHistory(intent: INSearchCallHistoryIntent; completion: TINSearchCallHistoryIntentHandlingBlockMethod1); cdecl;
    procedure handleSearchCallHistory(intent: INSearchCallHistoryIntent; completion: TINSearchCallHistoryIntentHandlingBlockMethod1); cdecl;
    procedure resolveCallTypeForSearchCallHistory(intent: INSearchCallHistoryIntent;
      withCompletion: TINSearchCallHistoryIntentHandlingBlockMethod2); cdecl; // API_DEPRECATED("resolveCallTypeForSearchCallHistory:withCompletion: is deprecated. Use resolveCallTypesForSearchCallHistory:withCompletion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveCallTypesForSearchCallHistory(intent: INSearchCallHistoryIntent;
      withCompletion: TINSearchCallHistoryIntentHandlingBlockMethod5); cdecl;
    procedure resolveDateCreatedForSearchCallHistory(intent: INSearchCallHistoryIntent;
      withCompletion: TINSearchCallHistoryIntentHandlingBlockMethod3); cdecl;
    procedure resolveRecipientForSearchCallHistory(intent: INSearchCallHistoryIntent;
      withCompletion: TINSearchCallHistoryIntentHandlingBlockMethod4); cdecl;
    procedure resolveUnseenForSearchCallHistory(intent: INSearchCallHistoryIntent;
      withCompletion: TINSearchCallHistoryIntentHandlingBlockMethod6); cdecl;
  end;

  INStartAudioCallIntentClass = interface(INIntentClass)
    ['{F7CDB7EF-6FBA-47B3-81CE-0E2CA4133C7F}']
  end;

  INStartAudioCallIntent = interface(INIntent)
    ['{25A50E39-A39A-41AD-B90E-96C5CB1B4238}']
    function contacts: NSArray; cdecl;
    function destinationType: INCallDestinationType; cdecl;
    function initWithContacts(contacts: NSArray): Pointer; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    function initWithDestinationType(destinationType: INCallDestinationType; contacts: NSArray): Pointer; cdecl;
  end;
  TINStartAudioCallIntent = class(TOCGenericImport<INStartAudioCallIntentClass, INStartAudioCallIntent>) end;

  INStartAudioCallIntentHandling = interface(IObjectiveC)
    ['{3C5713D7-1AD7-4EC4-A536-7AD67509EB00}']
    procedure confirmStartAudioCall(intent: INStartAudioCallIntent; completion: TINStartAudioCallIntentHandlingBlockMethod1); cdecl;
    procedure handleStartAudioCall(intent: INStartAudioCallIntent; completion: TINStartAudioCallIntentHandlingBlockMethod1); cdecl;
    procedure resolveContactsForStartAudioCall(intent: INStartAudioCallIntent; withCompletion: TINStartAudioCallIntentHandlingBlockMethod3); cdecl;
    procedure resolveDestinationTypeForStartAudioCall(intent: INStartAudioCallIntent; withCompletion: TINStartAudioCallIntentHandlingBlockMethod2); cdecl;
  end;

  INStartCallIntentClass = interface(INIntentClass)
    ['{A021D092-BC12-49C5-A14B-10868C984214}']
  end;

  INStartCallIntent = interface(INIntent)
    ['{0C9127B0-DE38-40DD-9BF8-2183D16F5B65}']
    function audioRoute: INCallAudioRoute; cdecl;
    function callCapability: INCallCapability; cdecl;
    function callRecordFilter: INCallRecordFilter; cdecl;
    function callRecordToCallBack: INCallRecord; cdecl;
    function contacts: NSArray; cdecl;
    function destinationType: INCallDestinationType; cdecl;
    function initWithAudioRoute(audioRoute: INCallAudioRoute; destinationType: INCallDestinationType; contacts: NSArray;
      recordTypeForRedialing: INCallRecordType; callCapability: INCallCapability): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithCallRecordFilter:callRecordToCallBack:audioRoute:destinationType:contacts:callCapability:", ios(13.0, 14.0), watchos(6.0, 7.0))
    function initWithCallRecordFilter(callRecordFilter: INCallRecordFilter; callRecordToCallBack: INCallRecord; audioRoute: INCallAudioRoute;
      destinationType: INCallDestinationType; contacts: NSArray; callCapability: INCallCapability): Pointer; cdecl;
    function recordTypeForRedialing: INCallRecordType; cdecl; // API_DEPRECATED("", ios(13.0, 14.0), watchos(6.0, 7.0))
  end;
  TINStartCallIntent = class(TOCGenericImport<INStartCallIntentClass, INStartCallIntent>) end;

  INStartCallIntentHandling = interface(IObjectiveC)
    ['{2FE5A31E-8C19-40F6-9927-CAA99F2F3840}']
    procedure confirmStartCall(intent: INStartCallIntent; completion: TINStartCallIntentHandlingBlockMethod1); cdecl;
    procedure handleStartCall(intent: INStartCallIntent; completion: TINStartCallIntentHandlingBlockMethod1); cdecl;
    procedure resolveCallCapabilityForStartCall(intent: INStartCallIntent; withCompletion: TINStartCallIntentHandlingBlockMethod5); cdecl;
    procedure resolveCallRecordToCallBackForStartCall(intent: INStartCallIntent; withCompletion: TINStartCallIntentHandlingBlockMethod2); cdecl;
    procedure resolveContactsForStartCall(intent: INStartCallIntent; withCompletion: TINStartCallIntentHandlingBlockMethod4); cdecl;
    procedure resolveDestinationTypeForStartCall(intent: INStartCallIntent; withCompletion: TINStartCallIntentHandlingBlockMethod3); cdecl;
  end;

  INStartVideoCallIntentClass = interface(INIntentClass)
    ['{AF21369C-AA83-41D4-9106-FA0639C8234D}']
  end;

  INStartVideoCallIntent = interface(INIntent)
    ['{FB746C75-37FD-4869-8B7B-9F4AC9ACD860}']
    function contacts: NSArray; cdecl;
    function initWithContacts(contacts: NSArray): Pointer; cdecl;
  end;
  TINStartVideoCallIntent = class(TOCGenericImport<INStartVideoCallIntentClass, INStartVideoCallIntent>) end;

  INStartVideoCallIntentHandling = interface(IObjectiveC)
    ['{DA1B24B0-76BD-48E3-91C6-1DBA8E1BB3B4}']
    procedure confirmStartVideoCall(intent: INStartVideoCallIntent; completion: TINStartVideoCallIntentHandlingBlockMethod1); cdecl;
    procedure handleStartVideoCall(intent: INStartVideoCallIntent; completion: TINStartVideoCallIntentHandlingBlockMethod1); cdecl;
    procedure resolveContactsForStartVideoCall(intent: INStartVideoCallIntent; withCompletion: TINStartVideoCallIntentHandlingBlockMethod2); cdecl;
  end;

  INActivateCarSignalIntentClass = interface(INIntentClass)
    ['{0A7E8921-19F7-4FB5-B43F-A5960B30E51F}']
  end;

  INActivateCarSignalIntent = interface(INIntent)
    ['{DF73C682-780B-4964-9FD2-4E2B287E7CDF}']
    function carName: INSpeakableString; cdecl;
    function initWithCarName(carName: INSpeakableString; signals: INCarSignalOptions): Pointer; cdecl;
    function signals: INCarSignalOptions; cdecl;
  end;
  TINActivateCarSignalIntent = class(TOCGenericImport<INActivateCarSignalIntentClass, INActivateCarSignalIntent>) end;

  INActivateCarSignalIntentHandling = interface(IObjectiveC)
    ['{225AE190-2109-43A8-934E-7F1BBBB1AEE7}']
    procedure confirmActivateCarSignal(intent: INActivateCarSignalIntent; completion: TINActivateCarSignalIntentHandlingBlockMethod1); cdecl;
    procedure handleActivateCarSignal(intent: INActivateCarSignalIntent; completion: TINActivateCarSignalIntentHandlingBlockMethod1); cdecl;
    procedure resolveCarNameForActivateCarSignal(intent: INActivateCarSignalIntent;
      withCompletion: TINActivateCarSignalIntentHandlingBlockMethod2); cdecl;
    procedure resolveSignalsForActivateCarSignal(intent: INActivateCarSignalIntent;
      withCompletion: TINActivateCarSignalIntentHandlingBlockMethod3); cdecl;
  end;

  INGetCarLockStatusIntentClass = interface(INIntentClass)
    ['{33309CBC-2147-4BCC-B9E6-E5A5947D16F5}']
  end;

  INGetCarLockStatusIntent = interface(INIntent)
    ['{CC95A2E2-6EA1-476D-A252-7F760250DBC9}']
    function carName: INSpeakableString; cdecl;
    function initWithCarName(carName: INSpeakableString): Pointer; cdecl;
  end;
  TINGetCarLockStatusIntent = class(TOCGenericImport<INGetCarLockStatusIntentClass, INGetCarLockStatusIntent>) end;

  INGetCarLockStatusIntentHandling = interface(IObjectiveC)
    ['{42C36F54-2E97-4221-8B22-E01453CED346}']
    procedure confirmGetCarLockStatus(intent: INGetCarLockStatusIntent; completion: TINGetCarLockStatusIntentHandlingBlockMethod1); cdecl;
    procedure handleGetCarLockStatus(intent: INGetCarLockStatusIntent; completion: TINGetCarLockStatusIntentHandlingBlockMethod1); cdecl;
    procedure resolveCarNameForGetCarLockStatus(intent: INGetCarLockStatusIntent;
      withCompletion: TINGetCarLockStatusIntentHandlingBlockMethod2); cdecl;
  end;

  INGetCarPowerLevelStatusIntentClass = interface(INIntentClass)
    ['{5B85821C-5599-4191-B3B1-6F7039F16EDB}']
  end;

  INGetCarPowerLevelStatusIntent = interface(INIntent)
    ['{5972F9C4-DBF4-44BF-97E6-9C10C734B422}']
    function carName: INSpeakableString; cdecl;
    function initWithCarName(carName: INSpeakableString): Pointer; cdecl;
  end;
  TINGetCarPowerLevelStatusIntent = class(TOCGenericImport<INGetCarPowerLevelStatusIntentClass, INGetCarPowerLevelStatusIntent>) end;

  INGetCarPowerLevelStatusIntentHandling = interface(IObjectiveC)
    ['{2285537E-1D93-451E-B637-CC791A5082AA}']
    procedure confirmGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent;
      completion: TINGetCarPowerLevelStatusIntentHandlingBlockMethod1); cdecl;
    procedure handleGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent;
      completion: TINGetCarPowerLevelStatusIntentHandlingBlockMethod1); cdecl;
    procedure resolveCarNameForGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent;
      withCompletion: TINGetCarPowerLevelStatusIntentHandlingBlockMethod2); cdecl;
    procedure startSendingUpdatesForGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent; toObserver: Pointer); cdecl;
    procedure stopSendingUpdatesForGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent); cdecl;
  end;

  INGetCarPowerLevelStatusIntentResponseObserver = interface(IObjectiveC)
    ['{36BBC241-48A1-4C09-B034-4C8759188248}']
    procedure getCarPowerLevelStatusResponseDidUpdate(response: INGetCarPowerLevelStatusIntentResponse); cdecl;
  end;

  INListCarsIntentClass = interface(INIntentClass)
    ['{76BF622B-70DA-498A-AAC4-0DC8890628C3}']
  end;

  INListCarsIntent = interface(INIntent)
    ['{C5236E65-2233-4463-850D-91C0B88FD2BB}']
  end;
  TINListCarsIntent = class(TOCGenericImport<INListCarsIntentClass, INListCarsIntent>) end;

  INListCarsIntentHandling = interface(IObjectiveC)
    ['{905E39F4-8632-42C4-B0BA-7CF28F07312D}']
    procedure confirmListCars(intent: INListCarsIntent; completion: TINListCarsIntentHandlingBlockMethod1); cdecl;
    procedure handleListCars(intent: INListCarsIntent; completion: TINListCarsIntentHandlingBlockMethod1); cdecl;
  end;

  INSaveProfileInCarIntentClass = interface(INIntentClass)
    ['{AD83219A-A304-4BB8-BA82-0F2904603577}']
  end;

  INSaveProfileInCarIntent = interface(INIntent)
    ['{20A1E185-450C-4137-8F7E-E3153143C1DB}']
    [MethodName('initWithProfileNumber:profileLabel:')]
    function initWithProfileNumberProfileLabel(profileNumber: NSNumber; profileLabel: NSString): Pointer; cdecl; // API_DEPRECATED("Use `-initWithProfileNumber:profileName:` method instead.", ios(10.0, 10.2))
    [MethodName('initWithProfileNumber:profileName:')]
    function initWithProfileNumberProfileName(profileNumber: NSNumber; profileName: NSString): Pointer; cdecl;
    function profileLabel: NSString; cdecl; // API_DEPRECATED("Use `profileName` property instead.", ios(10.0, 10.2))
    function profileName: NSString; cdecl;
    function profileNumber: NSNumber; cdecl;
  end;
  TINSaveProfileInCarIntent = class(TOCGenericImport<INSaveProfileInCarIntentClass, INSaveProfileInCarIntent>) end;

  INSaveProfileInCarIntentHandling = interface(IObjectiveC)
    ['{28CD3055-DCE9-48F4-81A8-02544D7FE8E7}']
    procedure confirmSaveProfileInCar(intent: INSaveProfileInCarIntent; completion: TINSaveProfileInCarIntentHandlingBlockMethod1); cdecl;
    procedure handleSaveProfileInCar(intent: INSaveProfileInCarIntent; completion: TINSaveProfileInCarIntentHandlingBlockMethod1); cdecl;
    procedure resolveProfileNameForSaveProfileInCar(intent: INSaveProfileInCarIntent;
      withCompletion: TINSaveProfileInCarIntentHandlingBlockMethod3); cdecl;
    procedure resolveProfileNumberForSaveProfileInCar(intent: INSaveProfileInCarIntent;
      withCompletion: TINSaveProfileInCarIntentHandlingBlockMethod2); cdecl;
  end;

  INSetAudioSourceInCarIntentClass = interface(INIntentClass)
    ['{88B90716-ABC8-4C29-B0CC-58A6F5ABF97E}']
  end;

  INSetAudioSourceInCarIntent = interface(INIntent)
    ['{6FC7D262-5B3F-4E40-B755-63112869B192}']
    function audioSource: INCarAudioSource; cdecl;
    function initWithAudioSource(audioSource: INCarAudioSource; relativeAudioSourceReference: INRelativeReference): Pointer; cdecl;
    function relativeAudioSourceReference: INRelativeReference; cdecl;
  end;
  TINSetAudioSourceInCarIntent = class(TOCGenericImport<INSetAudioSourceInCarIntentClass, INSetAudioSourceInCarIntent>) end;

  INSetAudioSourceInCarIntentHandling = interface(IObjectiveC)
    ['{57F9D4BD-1F55-46C5-B7D8-0BC82E83CBB5}']
    procedure confirmSetAudioSourceInCar(intent: INSetAudioSourceInCarIntent; completion: TINSetAudioSourceInCarIntentHandlingBlockMethod1); cdecl;
    procedure handleSetAudioSourceInCar(intent: INSetAudioSourceInCarIntent; completion: TINSetAudioSourceInCarIntentHandlingBlockMethod1); cdecl;
    procedure resolveAudioSourceForSetAudioSourceInCar(intent: INSetAudioSourceInCarIntent;
      withCompletion: TINSetAudioSourceInCarIntentHandlingBlockMethod2); cdecl;
    procedure resolveRelativeAudioSourceReferenceForSetAudioSourceInCar(intent: INSetAudioSourceInCarIntent;
      withCompletion: TINSetAudioSourceInCarIntentHandlingBlockMethod3); cdecl;
  end;

  INSetCarLockStatusIntentClass = interface(INIntentClass)
    ['{2ABB0605-C8E2-406C-9DEA-DD767DEDB631}']
  end;

  INSetCarLockStatusIntent = interface(INIntent)
    ['{8DE23AA2-523A-492C-9F25-B18DE05658C4}']
    function carName: INSpeakableString; cdecl;
    function initWithLocked(locked: NSNumber; carName: INSpeakableString): Pointer; cdecl;
    function locked: NSNumber; cdecl;
  end;
  TINSetCarLockStatusIntent = class(TOCGenericImport<INSetCarLockStatusIntentClass, INSetCarLockStatusIntent>) end;

  INSetCarLockStatusIntentHandling = interface(IObjectiveC)
    ['{41BB1F62-14C1-41E1-A1F2-0D60A28BE052}']
    procedure confirmSetCarLockStatus(intent: INSetCarLockStatusIntent; completion: TINSetCarLockStatusIntentHandlingBlockMethod1); cdecl;
    procedure handleSetCarLockStatus(intent: INSetCarLockStatusIntent; completion: TINSetCarLockStatusIntentHandlingBlockMethod1); cdecl;
    procedure resolveCarNameForSetCarLockStatus(intent: INSetCarLockStatusIntent;
      withCompletion: TINSetCarLockStatusIntentHandlingBlockMethod3); cdecl;
    procedure resolveLockedForSetCarLockStatus(intent: INSetCarLockStatusIntent;
      withCompletion: TINSetCarLockStatusIntentHandlingBlockMethod2); cdecl;
  end;

  INSetClimateSettingsInCarIntentClass = interface(INIntentClass)
    ['{4BDCF4B7-9F65-42F8-BA37-A92284EF6E57}']
  end;

  INSetClimateSettingsInCarIntent = interface(INIntent)
    ['{E6BAAFC6-8D6D-4A49-AB90-E7431E20F7D1}']
    function airCirculationMode: INCarAirCirculationMode; cdecl;
    function carName: INSpeakableString; cdecl;
    function climateZone: INCarSeat; cdecl;
    function enableAirConditioner: NSNumber; cdecl;
    function enableAutoMode: NSNumber; cdecl;
    function enableClimateControl: NSNumber; cdecl;
    function enableFan: NSNumber; cdecl;
    function fanSpeedIndex: NSNumber; cdecl;
    function fanSpeedPercentage: NSNumber; cdecl;
    function initWithEnableFan(enableFan: NSNumber; enableAirConditioner: NSNumber; enableClimateControl: NSNumber; enableAutoMode: NSNumber;
      airCirculationMode: INCarAirCirculationMode; fanSpeedIndex: NSNumber; fanSpeedPercentage: NSNumber; relativeFanSpeedSetting: INRelativeSetting;
      temperature: NSMeasurement; relativeTemperatureSetting: INRelativeSetting; climateZone: INCarSeat;
      carName: INSpeakableString): Pointer; overload; cdecl;
    function initWithEnableFan(enableFan: NSNumber; enableAirConditioner: NSNumber; enableClimateControl: NSNumber; enableAutoMode: NSNumber;
      airCirculationMode: INCarAirCirculationMode; fanSpeedIndex: NSNumber; fanSpeedPercentage: NSNumber; relativeFanSpeedSetting: INRelativeSetting;
      temperature: NSMeasurement; relativeTemperatureSetting: INRelativeSetting; climateZone: INCarSeat): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithEnableFan:enableAirConditioner:enableClimateControl:enableAutoMode:airCirculationMode:fanSpeedIndex:fanSpeedPercentage:relativeFanSpeedSetting:temperature:relativeTemperatureSetting:", ios(10.0, 12.0))
    function relativeFanSpeedSetting: INRelativeSetting; cdecl;
    function relativeTemperatureSetting: INRelativeSetting; cdecl;
    function temperature: NSMeasurement; cdecl;
  end;
  TINSetClimateSettingsInCarIntent = class(TOCGenericImport<INSetClimateSettingsInCarIntentClass, INSetClimateSettingsInCarIntent>) end;

  INSetClimateSettingsInCarIntentHandling = interface(IObjectiveC)
    ['{1F34A0D7-EB87-40FF-8257-90C716D85420}']
    procedure confirmSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      completion: TINSetClimateSettingsInCarIntentHandlingBlockMethod1); cdecl;
    procedure handleSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      completion: TINSetClimateSettingsInCarIntentHandlingBlockMethod1); cdecl;
    procedure resolveAirCirculationModeForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod3); cdecl;
    procedure resolveCarNameForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod9); cdecl;
    procedure resolveClimateZoneForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod8); cdecl;
    procedure resolveEnableAirConditionerForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod2); cdecl;
    procedure resolveEnableAutoModeForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod2); cdecl;
    procedure resolveEnableClimateControlForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod2); cdecl;
    procedure resolveEnableFanForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod2); cdecl;
    procedure resolveFanSpeedIndexForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod4); cdecl;
    procedure resolveFanSpeedPercentageForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod5); cdecl;
    procedure resolveRelativeFanSpeedSettingForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod6); cdecl;
    procedure resolveRelativeTemperatureSettingForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod6); cdecl;
    procedure resolveTemperatureForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent;
      withCompletion: TINSetClimateSettingsInCarIntentHandlingBlockMethod7); cdecl;
  end;

  INSetDefrosterSettingsInCarIntentClass = interface(INIntentClass)
    ['{BD35C613-EA03-4439-8607-B5BA50CFE4BC}']
  end;

  INSetDefrosterSettingsInCarIntent = interface(INIntent)
    ['{38045BBC-CDC5-4C0E-8936-6E9E478AAD59}']
    function carName: INSpeakableString; cdecl;
    function defroster: INCarDefroster; cdecl;
    function enable: NSNumber; cdecl;
    function initWithEnable(enable: NSNumber; defroster: INCarDefroster): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithEnable:defroster:carName:", ios(10.0, 12.0))
    function initWithEnable(enable: NSNumber; defroster: INCarDefroster; carName: INSpeakableString): Pointer; overload; cdecl;
  end;
  TINSetDefrosterSettingsInCarIntent = class(TOCGenericImport<INSetDefrosterSettingsInCarIntentClass, INSetDefrosterSettingsInCarIntent>) end;

  INSetDefrosterSettingsInCarIntentHandling = interface(IObjectiveC)
    ['{B051A1A3-88E0-484A-A295-57758CC3D61A}']
    procedure confirmSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent;
      completion: TINSetDefrosterSettingsInCarIntentHandlingBlockMethod1); cdecl;
    procedure handleSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent;
      completion: TINSetDefrosterSettingsInCarIntentHandlingBlockMethod1); cdecl;
    procedure resolveCarNameForSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent;
      withCompletion: TINSetDefrosterSettingsInCarIntentHandlingBlockMethod4); cdecl;
    procedure resolveDefrosterForSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent;
      withCompletion: TINSetDefrosterSettingsInCarIntentHandlingBlockMethod3); cdecl;
    procedure resolveEnableForSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent;
      withCompletion: TINSetDefrosterSettingsInCarIntentHandlingBlockMethod2); cdecl;
  end;

  INSetProfileInCarIntentClass = interface(INIntentClass)
    ['{8D4FE582-6E2B-4999-BB4F-C0259409B356}']
  end;

  INSetProfileInCarIntent = interface(INIntent)
    ['{FF5CEF86-83B8-4B2E-BD14-4BB9736D7E82}']
    function carName: INSpeakableString; cdecl;
    function defaultProfile: NSNumber; cdecl;
    [MethodName('initWithProfileNumber:profileLabel:defaultProfile:')]
    function initWithProfileNumberProfileLabel(profileNumber: NSNumber; profileLabel: NSString; defaultProfile: NSNumber): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithProfileNumber:profileName:defaultProfile:", ios(10.0, 10.2))
    [MethodName('initWithProfileNumber:profileName:defaultProfile:')]
    function initWithProfileNumberProfileName(profileNumber: NSNumber; profileName: NSString; defaultProfile: NSNumber): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithProfileNumber:profileName:defaultProfile:carName:", ios(10.2, 12.0))
    [MethodName('initWithProfileNumber:profileName:defaultProfile:carName:')]
    function initWithProfileNumberProfileName(profileNumber: NSNumber; profileName: NSString; defaultProfile: NSNumber;
      carName: INSpeakableString): Pointer; overload; cdecl;
    function profileLabel: NSString; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("profileName", ios(10.0, 10.2))
    function profileName: NSString; cdecl;
    function profileNumber: NSNumber; cdecl;
  end;
  TINSetProfileInCarIntent = class(TOCGenericImport<INSetProfileInCarIntentClass, INSetProfileInCarIntent>) end;

  INSetProfileInCarIntentHandling = interface(IObjectiveC)
    ['{C8B15F23-BCB0-4FAC-B890-27DA927F1F22}']
    procedure confirmSetProfileInCar(intent: INSetProfileInCarIntent; completion: TINSetProfileInCarIntentHandlingBlockMethod1); cdecl;
    procedure handleSetProfileInCar(intent: INSetProfileInCarIntent; completion: TINSetProfileInCarIntentHandlingBlockMethod1); cdecl;
    procedure resolveCarNameForSetProfileInCar(intent: INSetProfileInCarIntent; withCompletion: TINSetProfileInCarIntentHandlingBlockMethod4); cdecl;
    procedure resolveDefaultProfileForSetProfileInCar(intent: INSetProfileInCarIntent;
      withCompletion: TINSetProfileInCarIntentHandlingBlockMethod5); cdecl; // API_DEPRECATED("The property doesn't need to be resolved", ios(10.0, 11.0))
    procedure resolveProfileNameForSetProfileInCar(intent: INSetProfileInCarIntent;
      withCompletion: TINSetProfileInCarIntentHandlingBlockMethod3); cdecl;
    procedure resolveProfileNumberForSetProfileInCar(intent: INSetProfileInCarIntent;
      withCompletion: TINSetProfileInCarIntentHandlingBlockMethod2); cdecl;
  end;

  INSetSeatSettingsInCarIntentClass = interface(INIntentClass)
    ['{4BB9B797-D051-413A-8D7C-9DA8681E41BD}']
  end;

  INSetSeatSettingsInCarIntent = interface(INIntent)
    ['{7D93FCB2-3027-4B63-A870-DF33C7C42761}']
    function carName: INSpeakableString; cdecl;
    function enableCooling: NSNumber; cdecl;
    function enableHeating: NSNumber; cdecl;
    function enableMassage: NSNumber; cdecl;
    function initWithEnableHeating(enableHeating: NSNumber; enableCooling: NSNumber; enableMassage: NSNumber; seat: INCarSeat; level: NSNumber;
      relativeLevelSetting: INRelativeSetting): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithEnableHeating:enableCooling:enableMassage:seat:level:relativeLevelSetting:relativeLevelSetting:carName:", ios(10.0, 12.0))
    function initWithEnableHeating(enableHeating: NSNumber; enableCooling: NSNumber; enableMassage: NSNumber; seat: INCarSeat; level: NSNumber;
      relativeLevelSetting: INRelativeSetting; carName: INSpeakableString): Pointer; overload; cdecl;
    function level: NSNumber; cdecl;
    function relativeLevelSetting: INRelativeSetting; cdecl;
    function seat: INCarSeat; cdecl;
  end;
  TINSetSeatSettingsInCarIntent = class(TOCGenericImport<INSetSeatSettingsInCarIntentClass, INSetSeatSettingsInCarIntent>) end;

  INSetSeatSettingsInCarIntentHandling = interface(IObjectiveC)
    ['{8536D177-1621-4FEC-A2C0-F1FC39CA8135}']
    procedure confirmSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; completion: TINSetSeatSettingsInCarIntentHandlingBlockMethod1); cdecl;
    procedure handleSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; completion: TINSetSeatSettingsInCarIntentHandlingBlockMethod1); cdecl;
    procedure resolveCarNameForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent;
      withCompletion: TINSetSeatSettingsInCarIntentHandlingBlockMethod6); cdecl;
    procedure resolveEnableCoolingForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent;
      withCompletion: TINSetSeatSettingsInCarIntentHandlingBlockMethod2); cdecl;
    procedure resolveEnableHeatingForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent;
      withCompletion: TINSetSeatSettingsInCarIntentHandlingBlockMethod2); cdecl;
    procedure resolveEnableMassageForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent;
      withCompletion: TINSetSeatSettingsInCarIntentHandlingBlockMethod2); cdecl;
    procedure resolveLevelForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent;
      withCompletion: TINSetSeatSettingsInCarIntentHandlingBlockMethod4); cdecl;
    procedure resolveRelativeLevelSettingForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent;
      withCompletion: TINSetSeatSettingsInCarIntentHandlingBlockMethod5); cdecl;
    procedure resolveSeatForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent;
      withCompletion: TINSetSeatSettingsInCarIntentHandlingBlockMethod3); cdecl;
  end;

  INCancelWorkoutIntentClass = interface(INIntentClass)
    ['{56191F4C-8821-4BC8-BF5C-34D458B49B08}']
  end;

  INCancelWorkoutIntent = interface(INIntent)
    ['{98B6836B-92F4-409C-A0CA-61DF384CAA4B}']
    function initWithWorkoutName(workoutName: INSpeakableString): Pointer; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINCancelWorkoutIntent = class(TOCGenericImport<INCancelWorkoutIntentClass, INCancelWorkoutIntent>) end;

  INCancelWorkoutIntentHandling = interface(IObjectiveC)
    ['{0EA52736-5357-4A7E-9D8E-CB0401F22FBC}']
    procedure confirmCancelWorkout(intent: INCancelWorkoutIntent; completion: TINCancelWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure handleCancelWorkout(intent: INCancelWorkoutIntent; completion: TINCancelWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure resolveWorkoutNameForCancelWorkout(intent: INCancelWorkoutIntent; withCompletion: TINCancelWorkoutIntentHandlingBlockMethod2); cdecl;
  end;

  INEndWorkoutIntentClass = interface(INIntentClass)
    ['{FC47E3D6-5DBF-4186-8CE8-24DA00167BA5}']
  end;

  INEndWorkoutIntent = interface(INIntent)
    ['{450C01D7-D414-4570-BDA0-6E898C20552B}']
    function initWithWorkoutName(workoutName: INSpeakableString): Pointer; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINEndWorkoutIntent = class(TOCGenericImport<INEndWorkoutIntentClass, INEndWorkoutIntent>) end;

  INEndWorkoutIntentHandling = interface(IObjectiveC)
    ['{3F683AC7-04E7-4D8C-9DCB-628C048CDD96}']
    procedure confirmEndWorkout(intent: INEndWorkoutIntent; completion: TINEndWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure handleEndWorkout(intent: INEndWorkoutIntent; completion: TINEndWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure resolveWorkoutNameForEndWorkout(intent: INEndWorkoutIntent; withCompletion: TINEndWorkoutIntentHandlingBlockMethod2); cdecl;
  end;

  INPauseWorkoutIntentClass = interface(INIntentClass)
    ['{F099C32A-3D00-4088-B15D-4FEAC3A71DF6}']
  end;

  INPauseWorkoutIntent = interface(INIntent)
    ['{90D2B6A0-A5B9-4163-B6A9-721C42EC3CDF}']
    function initWithWorkoutName(workoutName: INSpeakableString): Pointer; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINPauseWorkoutIntent = class(TOCGenericImport<INPauseWorkoutIntentClass, INPauseWorkoutIntent>) end;

  INPauseWorkoutIntentHandling = interface(IObjectiveC)
    ['{20720EF7-35E5-49D1-8101-FDBF6505BDCB}']
    procedure confirmPauseWorkout(intent: INPauseWorkoutIntent; completion: TINPauseWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure handlePauseWorkout(intent: INPauseWorkoutIntent; completion: TINPauseWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure resolveWorkoutNameForPauseWorkout(intent: INPauseWorkoutIntent; withCompletion: TINPauseWorkoutIntentHandlingBlockMethod2); cdecl;
  end;

  INResumeWorkoutIntentClass = interface(INIntentClass)
    ['{ABC6C33E-C61C-46E7-80EA-E8D9A9CEEFE5}']
  end;

  INResumeWorkoutIntent = interface(INIntent)
    ['{60D07193-214D-4F81-8E9E-1927664159A8}']
    function initWithWorkoutName(workoutName: INSpeakableString): Pointer; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINResumeWorkoutIntent = class(TOCGenericImport<INResumeWorkoutIntentClass, INResumeWorkoutIntent>) end;

  INResumeWorkoutIntentHandling = interface(IObjectiveC)
    ['{D953E034-3FFD-4D84-AFD0-FD6C049EF873}']
    procedure confirmResumeWorkout(intent: INResumeWorkoutIntent; completion: TINResumeWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure handleResumeWorkout(intent: INResumeWorkoutIntent; completion: TINResumeWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure resolveWorkoutNameForResumeWorkout(intent: INResumeWorkoutIntent; withCompletion: TINResumeWorkoutIntentHandlingBlockMethod2); cdecl;
  end;

  INStartWorkoutIntentClass = interface(INIntentClass)
    ['{DEDC8C31-20D4-489C-A43E-7DA035AA4998}']
  end;

  INStartWorkoutIntent = interface(INIntent)
    ['{359D2C37-C72F-40C1-A941-A5CE1C63F210}']
    function goalValue: NSNumber; cdecl;
    function initWithWorkoutName(workoutName: INSpeakableString; goalValue: NSNumber; workoutGoalUnitType: INWorkoutGoalUnitType;
      workoutLocationType: INWorkoutLocationType; isOpenEnded: NSNumber): Pointer; cdecl;
    function isOpenEnded: NSNumber; cdecl;
    function workoutGoalUnitType: INWorkoutGoalUnitType; cdecl;
    function workoutLocationType: INWorkoutLocationType; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINStartWorkoutIntent = class(TOCGenericImport<INStartWorkoutIntentClass, INStartWorkoutIntent>) end;

  INStartWorkoutIntentHandling = interface(IObjectiveC)
    ['{D0EA85BB-DF4B-4464-A128-82329212F192}']
    procedure confirmStartWorkout(intent: INStartWorkoutIntent; completion: TINStartWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure handleStartWorkout(intent: INStartWorkoutIntent; completion: TINStartWorkoutIntentHandlingBlockMethod1); cdecl;
    procedure resolveGoalValueForStartWorkout(intent: INStartWorkoutIntent; withCompletion: TINStartWorkoutIntentHandlingBlockMethod3); cdecl;
    procedure resolveIsOpenEndedForStartWorkout(intent: INStartWorkoutIntent; withCompletion: TINStartWorkoutIntentHandlingBlockMethod6); cdecl;
    procedure resolveWorkoutGoalUnitTypeForStartWorkout(intent: INStartWorkoutIntent;
      withCompletion: TINStartWorkoutIntentHandlingBlockMethod4); cdecl;
    procedure resolveWorkoutLocationTypeForStartWorkout(intent: INStartWorkoutIntent;
      withCompletion: TINStartWorkoutIntentHandlingBlockMethod5); cdecl;
    procedure resolveWorkoutNameForStartWorkout(intent: INStartWorkoutIntent; withCompletion: TINStartWorkoutIntentHandlingBlockMethod2); cdecl;
  end;

  INAddMediaIntentClass = interface(INIntentClass)
    ['{A7BFBFE4-99CB-40D5-8C8B-0957B93A8078}']
  end;

  INAddMediaIntent = interface(INIntent)
    ['{21EB50ED-8662-43EA-B036-4B505E96B579}']
    function initWithMediaItems(mediaItems: NSArray; mediaSearch: INMediaSearch; mediaDestination: INMediaDestination): Pointer; cdecl;
    function mediaDestination: INMediaDestination; cdecl;
    function mediaItems: NSArray; cdecl;
    function mediaSearch: INMediaSearch; cdecl;
  end;
  TINAddMediaIntent = class(TOCGenericImport<INAddMediaIntentClass, INAddMediaIntent>) end;

  INAddMediaIntentHandling = interface(IObjectiveC)
    ['{2B780EDE-5CBB-428D-9B4B-4B339CF8669C}']
    procedure confirmAddMedia(intent: INAddMediaIntent; completion: TINAddMediaIntentHandlingBlockMethod1); cdecl;
    procedure handleAddMedia(intent: INAddMediaIntent; completion: TINAddMediaIntentHandlingBlockMethod1); cdecl;
    procedure resolveMediaDestinationForAddMedia(intent: INAddMediaIntent; withCompletion: TINAddMediaIntentHandlingBlockMethod3); cdecl;
    procedure resolveMediaItemsForAddMedia(intent: INAddMediaIntent; withCompletion: TINAddMediaIntentHandlingBlockMethod2); cdecl;
  end;

  INPlayMediaIntentClass = interface(INIntentClass)
    ['{301FA935-D21D-4717-A10D-078FF642C96B}']
  end;

  INPlayMediaIntent = interface(INIntent)
    ['{D8803082-9CA4-40AC-AA64-79D674190922}']
    function initWithMediaItems(mediaItems: NSArray; mediaContainer: INMediaItem; playShuffled: NSNumber; playbackRepeatMode: INPlaybackRepeatMode;
      resumePlayback: NSNumber; playbackQueueLocation: INPlaybackQueueLocation; playbackSpeed: NSNumber;
      mediaSearch: INMediaSearch): Pointer; overload; cdecl;
    function initWithMediaItems(mediaItems: NSArray; mediaContainer: INMediaItem; playShuffled: NSNumber; playbackRepeatMode: INPlaybackRepeatMode;
      resumePlayback: NSNumber): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(12.0, 13.0), watchos(5.0, 6.0))
    function mediaContainer: INMediaItem; cdecl;
    function mediaItems: NSArray; cdecl;
    function mediaSearch: INMediaSearch; cdecl;
    function playbackQueueLocation: INPlaybackQueueLocation; cdecl;
    function playbackRepeatMode: INPlaybackRepeatMode; cdecl;
    function playbackSpeed: NSNumber; cdecl;
    function playShuffled: NSNumber; cdecl;
    function resumePlayback: NSNumber; cdecl;
  end;
  TINPlayMediaIntent = class(TOCGenericImport<INPlayMediaIntentClass, INPlayMediaIntent>) end;

  INPlayMediaIntentHandling = interface(IObjectiveC)
    ['{CA59553B-7E0C-4A62-9184-BE3E5E1DF606}']
    procedure confirmPlayMedia(intent: INPlayMediaIntent; completion: TINPlayMediaIntentHandlingBlockMethod1); cdecl;
    procedure handlePlayMedia(intent: INPlayMediaIntent; completion: TINPlayMediaIntentHandlingBlockMethod1); cdecl;
    procedure resolveMediaItemsForPlayMedia(intent: INPlayMediaIntent; withCompletion: TINPlayMediaIntentHandlingBlockMethod2); cdecl;
    procedure resolvePlaybackQueueLocationForPlayMedia(intent: INPlayMediaIntent; withCompletion: TINPlayMediaIntentHandlingBlockMethod5); cdecl;
    procedure resolvePlaybackRepeatModeForPlayMedia(intent: INPlayMediaIntent; withCompletion: TINPlayMediaIntentHandlingBlockMethod4); cdecl;
    procedure resolvePlaybackSpeedForPlayMedia(intent: INPlayMediaIntent; withCompletion: TINPlayMediaIntentHandlingBlockMethod6); cdecl;
    procedure resolvePlayShuffledForPlayMedia(intent: INPlayMediaIntent; withCompletion: TINPlayMediaIntentHandlingBlockMethod3); cdecl;
    procedure resolveResumePlaybackForPlayMedia(intent: INPlayMediaIntent; withCompletion: TINPlayMediaIntentHandlingBlockMethod3); cdecl;
  end;

  INSearchForMediaIntentClass = interface(INIntentClass)
    ['{48F6A003-226C-42B6-B0A1-6AB0E1818AE4}']
  end;

  INSearchForMediaIntent = interface(INIntent)
    ['{886DC984-71AD-4D9D-861A-256BF66472FA}']
    function initWithMediaItems(mediaItems: NSArray; mediaSearch: INMediaSearch): Pointer; cdecl;
    function mediaItems: NSArray; cdecl;
    function mediaSearch: INMediaSearch; cdecl;
  end;
  TINSearchForMediaIntent = class(TOCGenericImport<INSearchForMediaIntentClass, INSearchForMediaIntent>) end;

  INSearchForMediaIntentHandling = interface(IObjectiveC)
    ['{CED1265A-82D3-49E5-BA13-AB54D5547768}']
    procedure confirmSearchForMedia(intent: INSearchForMediaIntent; completion: TINSearchForMediaIntentHandlingBlockMethod1); cdecl;
    procedure handleSearchForMedia(intent: INSearchForMediaIntent; completion: TINSearchForMediaIntentHandlingBlockMethod1); cdecl;
    procedure resolveMediaItemsForSearchForMedia(intent: INSearchForMediaIntent; withCompletion: TINSearchForMediaIntentHandlingBlockMethod2); cdecl;
  end;

  INUpdateMediaAffinityIntentClass = interface(INIntentClass)
    ['{5B64F828-B7B6-494D-B0EB-47E0EE582D6C}']
  end;

  INUpdateMediaAffinityIntent = interface(INIntent)
    ['{438454EC-C30D-4960-8100-D51E32D2B909}']
    function affinityType: INMediaAffinityType; cdecl;
    function initWithMediaItems(mediaItems: NSArray; mediaSearch: INMediaSearch; affinityType: INMediaAffinityType): Pointer; cdecl;
    function mediaItems: NSArray; cdecl;
    function mediaSearch: INMediaSearch; cdecl;
  end;
  TINUpdateMediaAffinityIntent = class(TOCGenericImport<INUpdateMediaAffinityIntentClass, INUpdateMediaAffinityIntent>) end;

  INUpdateMediaAffinityIntentHandling = interface(IObjectiveC)
    ['{1FBAD1DA-5F7D-4941-8D73-FD5860F81B48}']
    procedure confirmUpdateMediaAffinity(intent: INUpdateMediaAffinityIntent; completion: TINUpdateMediaAffinityIntentHandlingBlockMethod1); cdecl;
    procedure handleUpdateMediaAffinity(intent: INUpdateMediaAffinityIntent; completion: TINUpdateMediaAffinityIntentHandlingBlockMethod1); cdecl;
    procedure resolveAffinityTypeForUpdateMediaAffinity(intent: INUpdateMediaAffinityIntent;
      withCompletion: TINUpdateMediaAffinityIntentHandlingBlockMethod3); cdecl;
    procedure resolveMediaItemsForUpdateMediaAffinity(intent: INUpdateMediaAffinityIntent;
      withCompletion: TINUpdateMediaAffinityIntentHandlingBlockMethod2); cdecl;
  end;

  INSetRadioStationIntentClass = interface(INIntentClass)
    ['{F1F066C3-ED6D-494A-98E2-D8B488A32592}']
  end;

  INSetRadioStationIntent = interface(INIntent)
    ['{278FA397-9FC0-4E94-BEF5-314F583D7970}']
    function channel: NSString; cdecl;
    function frequency: NSNumber; cdecl;
    function initWithRadioType(radioType: INRadioType; frequency: NSNumber; stationName: NSString; channel: NSString;
      presetNumber: NSNumber): Pointer; cdecl;
    function presetNumber: NSNumber; cdecl;
    function radioType: INRadioType; cdecl;
    function stationName: NSString; cdecl;
  end;
  TINSetRadioStationIntent = class(TOCGenericImport<INSetRadioStationIntentClass, INSetRadioStationIntent>) end;

  INSetRadioStationIntentHandling = interface(IObjectiveC)
    ['{265008B3-F095-4541-B031-306EA0DEDEE8}']
    procedure confirmSetRadioStation(intent: INSetRadioStationIntent; completion: TINSetRadioStationIntentHandlingBlockMethod1); cdecl;
    procedure handleSetRadioStation(intent: INSetRadioStationIntent; completion: TINSetRadioStationIntentHandlingBlockMethod1); cdecl;
    procedure resolveChannelForSetRadioStation(intent: INSetRadioStationIntent; withCompletion: TINSetRadioStationIntentHandlingBlockMethod4); cdecl;
    procedure resolveFrequencyForSetRadioStation(intent: INSetRadioStationIntent;
      withCompletion: TINSetRadioStationIntentHandlingBlockMethod3); cdecl;
    procedure resolvePresetNumberForSetRadioStation(intent: INSetRadioStationIntent;
      withCompletion: TINSetRadioStationIntentHandlingBlockMethod5); cdecl;
    procedure resolveRadioTypeForSetRadioStation(intent: INSetRadioStationIntent;
      withCompletion: TINSetRadioStationIntentHandlingBlockMethod2); cdecl;
    procedure resolveStationNameForSetRadioStation(intent: INSetRadioStationIntent;
      withCompletion: TINSetRadioStationIntentHandlingBlockMethod4); cdecl;
  end;

  INSearchForMessagesIntentClass = interface(INIntentClass)
    ['{22CE5A98-EF8F-4C19-9E14-6283B1B43BB2}']
  end;

  INSearchForMessagesIntent = interface(INIntent)
    ['{69EC9DA5-FA2E-402E-B59B-7A339CCB40B5}']
    function attributes: INMessageAttributeOptions; cdecl;
    function conversationIdentifiers: NSArray; cdecl;
    function conversationIdentifiersOperator: INConditionalOperator; cdecl;
    function dateTimeRange: INDateComponentsRange; cdecl;
    function groupNames: NSArray; cdecl; // API_DEPRECATED("Use speakableGroupNames instead", ios(10.0, 11.0), watchos(3.2, 4.0), macosx(10.12, 10.13))
    function groupNamesOperator: INConditionalOperator; cdecl; // API_DEPRECATED("Use speakableGroupNamesOperator instead", ios(10.0, 11.0), watchos(3.2, 4.0), macosx(10.12, 10.13))
    function identifiers: NSArray; cdecl;
    function identifiersOperator: INConditionalOperator; cdecl;
    [MethodName('initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:conversationIdentifiers:')]
    function initWithRecipientsSenders(recipients: NSArray; senders: NSArray; searchTerms: NSArray; attributes: INMessageAttributeOptions;
      dateTimeRange: INDateComponentsRange; identifiers: NSArray; notificationIdentifiers: NSArray; speakableGroupNames: NSArray;
      conversationIdentifiers: NSArray): Pointer; overload; cdecl;
    [MethodName('initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:')]
    function initWithRecipientsSenders(recipients: NSArray; senders: NSArray; searchTerms: NSArray; attributes: INMessageAttributeOptions;
      dateTimeRange: INDateComponentsRange; identifiers: NSArray; notificationIdentifiers: NSArray; groupNames: NSArray): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 11.0), watchos(3.2, 4.0), macosx(10.12, 10.13))
    [MethodName('initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:')]
    function initWithRecipientsSenders2(recipients: NSArray; senders: NSArray; searchTerms: NSArray; attributes: INMessageAttributeOptions;
      dateTimeRange: INDateComponentsRange; identifiers: NSArray; notificationIdentifiers: NSArray;
      speakableGroupNames: NSArray): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(11.0, 12.0), watchos(4.0, 5.0), macosx(10.13, 10.14))
    function notificationIdentifiers: NSArray; cdecl;
    function notificationIdentifiersOperator: INConditionalOperator; cdecl;
    function recipients: NSArray; cdecl;
    function recipientsOperator: INConditionalOperator; cdecl;
    function searchTerms: NSArray; cdecl;
    function searchTermsOperator: INConditionalOperator; cdecl;
    function senders: NSArray; cdecl;
    function sendersOperator: INConditionalOperator; cdecl;
    function speakableGroupNames: NSArray; cdecl;
    function speakableGroupNamesOperator: INConditionalOperator; cdecl;
  end;
  TINSearchForMessagesIntent = class(TOCGenericImport<INSearchForMessagesIntentClass, INSearchForMessagesIntent>) end;

  INSearchForMessagesIntentHandling = interface(IObjectiveC)
    ['{CCB36AAB-AD53-43DD-8D31-CCCF678A240B}']
    procedure confirmSearchForMessages(intent: INSearchForMessagesIntent; completion: TINSearchForMessagesIntentHandlingBlockMethod1); cdecl;
    procedure handleSearchForMessages(intent: INSearchForMessagesIntent; completion: TINSearchForMessagesIntentHandlingBlockMethod1); cdecl;
    procedure resolveAttributesForSearchForMessages(intent: INSearchForMessagesIntent;
      withCompletion: TINSearchForMessagesIntentHandlingBlockMethod3); cdecl;
    procedure resolveDateTimeRangeForSearchForMessages(intent: INSearchForMessagesIntent;
      withCompletion: TINSearchForMessagesIntentHandlingBlockMethod4); cdecl;
    procedure resolveGroupNamesForSearchForMessages(intent: INSearchForMessagesIntent;
      withCompletion: TINSearchForMessagesIntentHandlingBlockMethod2); cdecl; // API_DEPRECATED("resolveGroupNamesForSearchForMessages:withCompletion: is deprecated. Use resolveSpeakableGroupNamesForSearchForMessages:withCompletion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveRecipientsForSearchForMessages(intent: INSearchForMessagesIntent;
      withCompletion: TINSearchForMessagesIntentHandlingBlockMethod2); cdecl;
    procedure resolveSendersForSearchForMessages(intent: INSearchForMessagesIntent;
      withCompletion: TINSearchForMessagesIntentHandlingBlockMethod2); cdecl;
    procedure resolveSpeakableGroupNamesForSearchForMessages(intent: INSearchForMessagesIntent;
      withCompletion: TINSearchForMessagesIntentHandlingBlockMethod2); cdecl;
  end;

  INSendMessageIntentClass = interface(INIntentClass)
    ['{312F5A69-6C54-4D23-A7DA-1FD4237D35D0}']
  end;

  INSendMessageIntent = interface(INIntent)
    ['{0BCD0547-EFA3-442E-B5CB-0BA43400ABDD}']
    function attachments: NSArray; cdecl;
    function content: NSString; cdecl;
    function conversationIdentifier: NSString; cdecl;
    function groupName: NSString; cdecl; // API_DEPRECATED("Use speakableGroupNames instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    function initWithRecipients(recipients: NSArray; outgoingMessageType: INOutgoingMessageType; content: NSString;
      speakableGroupName: INSpeakableString; conversationIdentifier: NSString; serviceName: NSString; sender: INPerson;
      attachments: NSArray): Pointer; overload; cdecl;
    function initWithRecipients(recipients: NSArray; content: NSString; groupName: NSString; serviceName: NSString;
      sender: INPerson): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    function initWithRecipients(recipients: NSArray; content: NSString; speakableGroupName: INSpeakableString; conversationIdentifier: NSString;
      serviceName: NSString; sender: INPerson): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer with outgoingMessageType instead", ios(11.0, 14.0), watchos(4.0, 7.0), macosx(10.13, 11.0))
    function initWithRecipients(recipients: NSArray; outgoingMessageType: INOutgoingMessageType; content: NSString;
      speakableGroupName: INSpeakableString; conversationIdentifier: NSString; serviceName: NSString; sender: INPerson): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer with attachments instead instead", ios(14.0, 14.0), watchos(7.0, 7.0), macosx(10.16, 11.0))
    function outgoingMessageType: INOutgoingMessageType; cdecl;
    function recipients: NSArray; cdecl;
    function sender: INPerson; cdecl;
    function serviceName: NSString; cdecl;
    function speakableGroupName: INSpeakableString; cdecl;
  end;
  TINSendMessageIntent = class(TOCGenericImport<INSendMessageIntentClass, INSendMessageIntent>) end;

  INSendMessageIntentHandling = interface(IObjectiveC)
    ['{6D4BE9CC-B909-4833-A45C-80FF72E908AB}']
    procedure confirmSendMessage(intent: INSendMessageIntent; completion: TINSendMessageIntentHandlingBlockMethod1); cdecl;
    procedure handleSendMessage(intent: INSendMessageIntent; completion: TINSendMessageIntentHandlingBlockMethod1); cdecl;
    procedure resolveContentForSendMessage(intent: INSendMessageIntent; withCompletion: TINSendMessageIntentHandlingBlockMethod3); cdecl;
    procedure resolveGroupNameForSendMessage(intent: INSendMessageIntent; withCompletion: TINSendMessageIntentHandlingBlockMethod3); cdecl; // API_DEPRECATED("resolveGroupNameForSendMessage:withCompletion: is deprecated. Use resolveSpeakableGroupNameForSendMessage:withCompletion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveOutgoingMessageTypeForSendMessage(intent: INSendMessageIntent; withCompletion: TINSendMessageIntentHandlingBlockMethod4); cdecl;
    [MethodName('resolveRecipientsForSendMessage:completion:')]
    procedure resolveRecipientsForSendMessageCompletion(intent: INSendMessageIntent; completion: TINSendMessageIntentHandlingBlockMethod2); cdecl;
    [MethodName('resolveRecipientsForSendMessage:withCompletion:')]
    procedure resolveRecipientsForSendMessageWithCompletion(intent: INSendMessageIntent;
      withCompletion: TINSendMessageIntentHandlingBlockMethod2); cdecl; // API_DEPRECATED("resolveRecipientsForSendMessage:withCompletion: is deprecated. Use resolveRecipientsForSendMessage:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveSpeakableGroupNameForSendMessage(intent: INSendMessageIntent; withCompletion: TINSendMessageIntentHandlingBlockMethod5); cdecl;
  end;

  INSetMessageAttributeIntentClass = interface(INIntentClass)
    ['{1390C728-E799-4B6B-8B8F-4AF1D1B29CF2}']
  end;

  INSetMessageAttributeIntent = interface(INIntent)
    ['{5713BA04-CB63-4A16-BABE-E566EF6D018B}']
    function attribute: INMessageAttribute; cdecl;
    function identifiers: NSArray; cdecl;
    function initWithIdentifiers(identifiers: NSArray; attribute: INMessageAttribute): Pointer; cdecl;
  end;
  TINSetMessageAttributeIntent = class(TOCGenericImport<INSetMessageAttributeIntentClass, INSetMessageAttributeIntent>) end;

  INSetMessageAttributeIntentHandling = interface(IObjectiveC)
    ['{960B3D08-295D-44DA-A3C8-9C04C114C063}']
    procedure confirmSetMessageAttribute(intent: INSetMessageAttributeIntent; completion: TINSetMessageAttributeIntentHandlingBlockMethod1); cdecl;
    procedure handleSetMessageAttribute(intent: INSetMessageAttributeIntent; completion: TINSetMessageAttributeIntentHandlingBlockMethod1); cdecl;
    procedure resolveAttributeForSetMessageAttribute(intent: INSetMessageAttributeIntent;
      withCompletion: TINSetMessageAttributeIntentHandlingBlockMethod2); cdecl;
  end;

  INAddTasksIntentClass = interface(INIntentClass)
    ['{15549710-ED33-4E85-9D34-3D151C584029}']
  end;

  INAddTasksIntent = interface(INIntent)
    ['{B5F2EBB3-81AD-4357-884E-81BCDB1ED959}']
    function initWithTargetTaskList(targetTaskList: INTaskList; taskTitles: NSArray; spatialEventTrigger: INSpatialEventTrigger;
      temporalEventTrigger: INTemporalEventTrigger): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithTargetTaskList:taskTitles:spatialEventTrigger:temporalEventTrigger:priority:", ios(11.0, 13.0), watchos(4.0, 6.0))
    function initWithTargetTaskList(targetTaskList: INTaskList; taskTitles: NSArray; spatialEventTrigger: INSpatialEventTrigger;
      temporalEventTrigger: INTemporalEventTrigger; priority: INTaskPriority): Pointer; overload; cdecl;
    function priority: INTaskPriority; cdecl;
    function spatialEventTrigger: INSpatialEventTrigger; cdecl;
    function targetTaskList: INTaskList; cdecl;
    function taskTitles: NSArray; cdecl;
    function temporalEventTrigger: INTemporalEventTrigger; cdecl;
  end;
  TINAddTasksIntent = class(TOCGenericImport<INAddTasksIntentClass, INAddTasksIntent>) end;

  INAddTasksIntentHandling = interface(IObjectiveC)
    ['{D58F3527-16C0-410F-9868-5F607D585532}']
    procedure confirmAddTasks(intent: INAddTasksIntent; completion: TINAddTasksIntentHandlingBlockMethod1); cdecl;
    procedure handleAddTasks(intent: INAddTasksIntent; completion: TINAddTasksIntentHandlingBlockMethod1); cdecl;
    procedure resolvePriorityForAddTasks(intent: INAddTasksIntent; withCompletion: TINAddTasksIntentHandlingBlockMethod8); cdecl;
    procedure resolveSpatialEventTriggerForAddTasks(intent: INAddTasksIntent; withCompletion: TINAddTasksIntentHandlingBlockMethod5); cdecl;
    procedure resolveTargetTaskListForAddTasks(intent: INAddTasksIntent; completion: TINAddTasksIntentHandlingBlockMethod3); overload; cdecl;
    procedure resolveTargetTaskListForAddTasks(intent: INAddTasksIntent; withCompletion: TINAddTasksIntentHandlingBlockMethod2); overload; cdecl; // API_DEPRECATED("resolveTargetTaskListForAddTasks:withCompletion: is deprecated. Use resolveTargetTaskListForAddTasks:completion: instead", ios(11.0, 13.0), watchos(4.0, 6.0))
    procedure resolveTaskTitlesForAddTasks(intent: INAddTasksIntent; withCompletion: TINAddTasksIntentHandlingBlockMethod4); cdecl;
    procedure resolveTemporalEventTriggerForAddTasks(intent: INAddTasksIntent; completion: TINAddTasksIntentHandlingBlockMethod7); overload; cdecl;
    procedure resolveTemporalEventTriggerForAddTasks(intent: INAddTasksIntent;
      withCompletion: TINAddTasksIntentHandlingBlockMethod6); overload; cdecl; // API_DEPRECATED("resolveTemporalEventTriggerForAddTasks:withCompletion: is deprecated. Use resolveTemporalEventTriggerForAddTasks:completion: instead", ios(11.0, 13.0), watchos(4.0, 6.0))
  end;

  INAppendToNoteIntentClass = interface(INIntentClass)
    ['{290DC467-570C-49D9-845D-EA4769D91E54}']
  end;

  INAppendToNoteIntent = interface(INIntent)
    ['{80250A94-4F20-4D9D-A68D-865E0FE8B65A}']
    function content: INNoteContent; cdecl;
    function initWithTargetNote(targetNote: INNote; content: INNoteContent): Pointer; cdecl;
    function targetNote: INNote; cdecl;
  end;
  TINAppendToNoteIntent = class(TOCGenericImport<INAppendToNoteIntentClass, INAppendToNoteIntent>) end;

  INAppendToNoteIntentHandling = interface(IObjectiveC)
    ['{AACF54F3-8D5A-4D66-8960-D5C672F91E2E}']
    procedure confirmAppendToNote(intent: INAppendToNoteIntent; completion: TINAppendToNoteIntentHandlingBlockMethod1); cdecl;
    procedure handleAppendToNote(intent: INAppendToNoteIntent; completion: TINAppendToNoteIntentHandlingBlockMethod1); cdecl;
    procedure resolveContentForAppendToNote(intent: INAppendToNoteIntent; withCompletion: TINAppendToNoteIntentHandlingBlockMethod3); cdecl;
    procedure resolveTargetNoteForAppendToNote(intent: INAppendToNoteIntent; withCompletion: TINAppendToNoteIntentHandlingBlockMethod2); cdecl;
  end;

  INCreateNoteIntentClass = interface(INIntentClass)
    ['{FB1744E5-E85B-4FFD-844A-DF67407630EA}']
  end;

  INCreateNoteIntent = interface(INIntent)
    ['{6CC2F717-23AD-4CE6-8EA4-C3A23E2F4F0E}']
    function content: INNoteContent; cdecl;
    function groupName: INSpeakableString; cdecl;
    function initWithTitle(title: INSpeakableString; content: INNoteContent; groupName: INSpeakableString): Pointer; cdecl;
    function title: INSpeakableString; cdecl;
  end;
  TINCreateNoteIntent = class(TOCGenericImport<INCreateNoteIntentClass, INCreateNoteIntent>) end;

  INCreateNoteIntentHandling = interface(IObjectiveC)
    ['{A6FF7259-ECC2-4764-A2C7-426B3AC49FF9}']
    procedure confirmCreateNote(intent: INCreateNoteIntent; completion: TINCreateNoteIntentHandlingBlockMethod1); cdecl;
    procedure handleCreateNote(intent: INCreateNoteIntent; completion: TINCreateNoteIntentHandlingBlockMethod1); cdecl;
    procedure resolveContentForCreateNote(intent: INCreateNoteIntent; withCompletion: TINCreateNoteIntentHandlingBlockMethod3); cdecl;
    procedure resolveGroupNameForCreateNote(intent: INCreateNoteIntent; withCompletion: TINCreateNoteIntentHandlingBlockMethod2); cdecl;
    procedure resolveTitleForCreateNote(intent: INCreateNoteIntent; withCompletion: TINCreateNoteIntentHandlingBlockMethod2); cdecl;
  end;

  INCreateTaskListIntentClass = interface(INIntentClass)
    ['{B649A92A-E144-4EB7-8F74-CA455BC84E1F}']
  end;

  INCreateTaskListIntent = interface(INIntent)
    ['{97982953-516C-44C6-BC8E-B0F3F03C4BD4}']
    function groupName: INSpeakableString; cdecl;
    function initWithTitle(title: INSpeakableString; taskTitles: NSArray; groupName: INSpeakableString): Pointer; cdecl;
    function taskTitles: NSArray; cdecl;
    function title: INSpeakableString; cdecl;
  end;
  TINCreateTaskListIntent = class(TOCGenericImport<INCreateTaskListIntentClass, INCreateTaskListIntent>) end;

  INCreateTaskListIntentHandling = interface(IObjectiveC)
    ['{59DDE9C3-4AA6-47C8-8287-6FE7255E1DCE}']
    procedure confirmCreateTaskList(intent: INCreateTaskListIntent; completion: TINCreateTaskListIntentHandlingBlockMethod1); cdecl;
    procedure handleCreateTaskList(intent: INCreateTaskListIntent; completion: TINCreateTaskListIntentHandlingBlockMethod1); cdecl;
    procedure resolveGroupNameForCreateTaskList(intent: INCreateTaskListIntent; withCompletion: TINCreateTaskListIntentHandlingBlockMethod2); cdecl;
    procedure resolveTaskTitlesForCreateTaskList(intent: INCreateTaskListIntent; withCompletion: TINCreateTaskListIntentHandlingBlockMethod3); cdecl;
    procedure resolveTitleForCreateTaskList(intent: INCreateTaskListIntent; withCompletion: TINCreateTaskListIntentHandlingBlockMethod2); cdecl;
  end;

  INDeleteTasksIntentClass = interface(INIntentClass)
    ['{B3950966-417A-4E77-99A0-6089A9DB2655}']
  end;

  INDeleteTasksIntent = interface(INIntent)
    ['{5BBE7026-96FD-4C53-9738-CAE826B6810F}']
    function all: NSNumber; cdecl;
    function initWithTaskList(taskList: INTaskList; tasks: NSArray; all: NSNumber): Pointer; cdecl;
    function taskList: INTaskList; cdecl;
    function tasks: NSArray; cdecl;
  end;
  TINDeleteTasksIntent = class(TOCGenericImport<INDeleteTasksIntentClass, INDeleteTasksIntent>) end;

  INDeleteTasksIntentHandling = interface(IObjectiveC)
    ['{32EA00A7-3932-45DD-9A56-CB739E174B50}']
    procedure confirmDeleteTasks(intent: INDeleteTasksIntent; completion: TINDeleteTasksIntentHandlingBlockMethod1); cdecl;
    procedure handleDeleteTasks(intent: INDeleteTasksIntent; completion: TINDeleteTasksIntentHandlingBlockMethod1); cdecl;
    procedure resolveTaskListForDeleteTasks(intent: INDeleteTasksIntent; withCompletion: TINDeleteTasksIntentHandlingBlockMethod2); cdecl;
    procedure resolveTasksForDeleteTasks(intent: INDeleteTasksIntent; withCompletion: TINDeleteTasksIntentHandlingBlockMethod3); cdecl;
  end;

  INSearchForNotebookItemsIntentClass = interface(INIntentClass)
    ['{BA53AB91-C404-4812-88E6-00BF8A03A573}']
  end;

  INSearchForNotebookItemsIntent = interface(INIntent)
    ['{BCE21C6E-1535-4F72-9E58-F995054E1C0E}']
    function content: NSString; cdecl;
    function dateSearchType: INDateSearchType; cdecl;
    function dateTime: INDateComponentsRange; cdecl;
    function initWithTitle(title: INSpeakableString; content: NSString; itemType: INNotebookItemType; status: INTaskStatus; location: CLPlacemark;
      locationSearchType: INLocationSearchType; dateTime: INDateComponentsRange; dateSearchType: INDateSearchType;
      notebookItemIdentifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(11.2, 13.0), watchos(4.2, 6.0))
    function initWithTitle(title: INSpeakableString; content: NSString; itemType: INNotebookItemType; status: INTaskStatus;
      location: CLPlacemark; locationSearchType: INLocationSearchType; dateTime: INDateComponentsRange;
      dateSearchType: INDateSearchType): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(11.0, 11.2), watchos(4.0, 4.2))
    function initWithTitle(title: INSpeakableString; content: NSString; itemType: INNotebookItemType; status: INTaskStatus; location: CLPlacemark;
      locationSearchType: INLocationSearchType; dateTime: INDateComponentsRange; dateSearchType: INDateSearchType;
      temporalEventTriggerTypes: INTemporalEventTriggerTypeOptions; taskPriority: INTaskPriority;
      notebookItemIdentifier: NSString): Pointer; overload; cdecl;
    function itemType: INNotebookItemType; cdecl;
    function location: CLPlacemark; cdecl;
    function locationSearchType: INLocationSearchType; cdecl;
    function notebookItemIdentifier: NSString; cdecl;
    function status: INTaskStatus; cdecl;
    function taskPriority: INTaskPriority; cdecl;
    function temporalEventTriggerTypes: INTemporalEventTriggerTypeOptions; cdecl;
    function title: INSpeakableString; cdecl;
  end;
  TINSearchForNotebookItemsIntent = class(TOCGenericImport<INSearchForNotebookItemsIntentClass, INSearchForNotebookItemsIntent>) end;

  INSearchForNotebookItemsIntentHandling = interface(IObjectiveC)
    ['{EAB459FA-D659-43ED-A090-416F692244EF}']
    procedure confirmSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      completion: TINSearchForNotebookItemsIntentHandlingBlockMethod1); cdecl;
    procedure handleSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      completion: TINSearchForNotebookItemsIntentHandlingBlockMethod1); cdecl;
    procedure resolveContentForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod3); cdecl;
    procedure resolveDateSearchTypeForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod9); cdecl;
    procedure resolveDateTimeForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod8); cdecl;
    procedure resolveItemTypeForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod4); cdecl;
    procedure resolveLocationForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod6); cdecl;
    procedure resolveLocationSearchTypeForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod7); cdecl;
    procedure resolveStatusForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod5); cdecl;
    procedure resolveTaskPriorityForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod11); cdecl;
    procedure resolveTemporalEventTriggerTypesForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod10); cdecl;
    procedure resolveTitleForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent;
      withCompletion: TINSearchForNotebookItemsIntentHandlingBlockMethod2); cdecl;
  end;

  INSetTaskAttributeIntentClass = interface(INIntentClass)
    ['{E6BB461A-106B-4260-83EE-994B7A5C8C8E}']
  end;

  INSetTaskAttributeIntent = interface(INIntent)
    ['{9C18C596-23F2-4B1F-8870-2B0E8956AD07}']
    function initWithTargetTask(targetTask: INTask; status: INTaskStatus; spatialEventTrigger: INSpatialEventTrigger;
      temporalEventTrigger: INTemporalEventTrigger): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(11.0, 13.0), watchos(4.0, 6.0))
    function initWithTargetTask(targetTask: INTask; taskTitle: INSpeakableString; status: INTaskStatus; priority: INTaskPriority;
      spatialEventTrigger: INSpatialEventTrigger; temporalEventTrigger: INTemporalEventTrigger): Pointer; overload; cdecl;
    function priority: INTaskPriority; cdecl;
    function spatialEventTrigger: INSpatialEventTrigger; cdecl;
    function status: INTaskStatus; cdecl;
    function targetTask: INTask; cdecl;
    function taskTitle: INSpeakableString; cdecl;
    function temporalEventTrigger: INTemporalEventTrigger; cdecl;
  end;
  TINSetTaskAttributeIntent = class(TOCGenericImport<INSetTaskAttributeIntentClass, INSetTaskAttributeIntent>) end;

  INSetTaskAttributeIntentHandling = interface(IObjectiveC)
    ['{A9163067-83F2-4D72-9988-3D0E2D244312}']
    procedure confirmSetTaskAttribute(intent: INSetTaskAttributeIntent; completion: TINSetTaskAttributeIntentHandlingBlockMethod1); cdecl;
    procedure handleSetTaskAttribute(intent: INSetTaskAttributeIntent; completion: TINSetTaskAttributeIntentHandlingBlockMethod1); cdecl;
    procedure resolvePriorityForSetTaskAttribute(intent: INSetTaskAttributeIntent;
      withCompletion: TINSetTaskAttributeIntentHandlingBlockMethod5); cdecl;
    procedure resolveSpatialEventTriggerForSetTaskAttribute(intent: INSetTaskAttributeIntent;
      withCompletion: TINSetTaskAttributeIntentHandlingBlockMethod6); cdecl;
    procedure resolveStatusForSetTaskAttribute(intent: INSetTaskAttributeIntent; withCompletion: TINSetTaskAttributeIntentHandlingBlockMethod4); cdecl;
    procedure resolveTargetTaskForSetTaskAttribute(intent: INSetTaskAttributeIntent;
      withCompletion: TINSetTaskAttributeIntentHandlingBlockMethod2); cdecl;
    procedure resolveTaskTitleForSetTaskAttribute(intent: INSetTaskAttributeIntent;
      withCompletion: TINSetTaskAttributeIntentHandlingBlockMethod3); cdecl;
    procedure resolveTemporalEventTriggerForSetTaskAttribute(intent: INSetTaskAttributeIntent;
      completion: TINSetTaskAttributeIntentHandlingBlockMethod8); overload; cdecl;
    procedure resolveTemporalEventTriggerForSetTaskAttribute(intent: INSetTaskAttributeIntent;
      withCompletion: TINSetTaskAttributeIntentHandlingBlockMethod7); overload; cdecl; // API_DEPRECATED("resolveTemporalEventTriggerForSetTaskAttribute:withCompletion: is deprecated. Use resolveTemporalEventTriggerForSetTaskAttribute:completion: instead", ios(11.0, 13.0), watchos(4.0, 6.0))
  end;

  INSnoozeTasksIntentClass = interface(INIntentClass)
    ['{11A2463F-F055-48DC-A04E-F5B98F6D2055}']
  end;

  INSnoozeTasksIntent = interface(INIntent)
    ['{DCF3FF82-D724-42E9-BB12-866B454A2B12}']
    function all: NSNumber; cdecl;
    function initWithTasks(tasks: NSArray; nextTriggerTime: INDateComponentsRange; all: NSNumber): Pointer; cdecl;
    function nextTriggerTime: INDateComponentsRange; cdecl;
    function tasks: NSArray; cdecl;
  end;
  TINSnoozeTasksIntent = class(TOCGenericImport<INSnoozeTasksIntentClass, INSnoozeTasksIntent>) end;

  INSnoozeTasksIntentHandling = interface(IObjectiveC)
    ['{058DE275-2828-4112-B31C-F966AC12C47F}']
    procedure confirmSnoozeTasks(intent: INSnoozeTasksIntent; completion: TINSnoozeTasksIntentHandlingBlockMethod1); cdecl;
    procedure handleSnoozeTasks(intent: INSnoozeTasksIntent; completion: TINSnoozeTasksIntentHandlingBlockMethod1); cdecl;
    procedure resolveNextTriggerTimeForSnoozeTasks(intent: INSnoozeTasksIntent; withCompletion: TINSnoozeTasksIntentHandlingBlockMethod3); cdecl;
    procedure resolveTasksForSnoozeTasks(intent: INSnoozeTasksIntent; withCompletion: TINSnoozeTasksIntentHandlingBlockMethod2); cdecl;
  end;

  INPayBillIntentClass = interface(INIntentClass)
    ['{F5353894-1673-487E-9E67-75DF6140C322}']
  end;

  INPayBillIntent = interface(INIntent)
    ['{97493D33-F329-4EA5-A6F1-E0CE3DB7AC22}']
    function billPayee: INBillPayee; cdecl;
    function billType: INBillType; cdecl;
    function dueDate: INDateComponentsRange; cdecl;
    function fromAccount: INPaymentAccount; cdecl;
    function initWithBillPayee(billPayee: INBillPayee; fromAccount: INPaymentAccount; transactionAmount: INPaymentAmount;
      transactionScheduledDate: INDateComponentsRange; transactionNote: NSString; billType: INBillType;
      dueDate: INDateComponentsRange): Pointer; cdecl;
    function transactionAmount: INPaymentAmount; cdecl;
    function transactionNote: NSString; cdecl;
    function transactionScheduledDate: INDateComponentsRange; cdecl;
  end;
  TINPayBillIntent = class(TOCGenericImport<INPayBillIntentClass, INPayBillIntent>) end;

  INPayBillIntentHandling = interface(IObjectiveC)
    ['{B675FFE4-94A4-479C-806D-4222F8274B4D}']
    procedure confirmPayBill(intent: INPayBillIntent; completion: TINPayBillIntentHandlingBlockMethod1); cdecl;
    procedure handlePayBill(intent: INPayBillIntent; completion: TINPayBillIntentHandlingBlockMethod1); cdecl;
    procedure resolveBillPayeeForPayBill(intent: INPayBillIntent; withCompletion: TINPayBillIntentHandlingBlockMethod2); cdecl;
    procedure resolveBillTypeForPayBill(intent: INPayBillIntent; withCompletion: TINPayBillIntentHandlingBlockMethod7); cdecl;
    procedure resolveDueDateForPayBill(intent: INPayBillIntent; withCompletion: TINPayBillIntentHandlingBlockMethod5); cdecl;
    procedure resolveFromAccountForPayBill(intent: INPayBillIntent; withCompletion: TINPayBillIntentHandlingBlockMethod3); cdecl;
    procedure resolveTransactionAmountForPayBill(intent: INPayBillIntent; withCompletion: TINPayBillIntentHandlingBlockMethod4); cdecl;
    procedure resolveTransactionNoteForPayBill(intent: INPayBillIntent; withCompletion: TINPayBillIntentHandlingBlockMethod6); cdecl;
    procedure resolveTransactionScheduledDateForPayBill(intent: INPayBillIntent; withCompletion: TINPayBillIntentHandlingBlockMethod5); cdecl;
  end;

  INRequestPaymentIntentClass = interface(INIntentClass)
    ['{FF26167C-442B-4A67-AAEF-73FDDAA93759}']
  end;

  INRequestPaymentIntent = interface(INIntent)
    ['{71C8D3FC-AB9D-4336-A911-45526AC63CE9}']
    function currencyAmount: INCurrencyAmount; cdecl;
    function initWithPayer(payer: INPerson; currencyAmount: INCurrencyAmount; note: NSString): Pointer; cdecl;
    function note: NSString; cdecl;
    function payer: INPerson; cdecl;
  end;
  TINRequestPaymentIntent = class(TOCGenericImport<INRequestPaymentIntentClass, INRequestPaymentIntent>) end;

  INRequestPaymentIntentHandling = interface(IObjectiveC)
    ['{42CDDA16-AD6D-41A6-B9C2-1A9A28D55FE4}']
    procedure confirmRequestPayment(intent: INRequestPaymentIntent; completion: TINRequestPaymentIntentHandlingBlockMethod1); cdecl;
    procedure handleRequestPayment(intent: INRequestPaymentIntent; completion: TINRequestPaymentIntentHandlingBlockMethod1); cdecl;
    procedure resolveCurrencyAmountForRequestPayment(intent: INRequestPaymentIntent;
      completion: TINRequestPaymentIntentHandlingBlockMethod5); overload; cdecl;
    procedure resolveCurrencyAmountForRequestPayment(intent: INRequestPaymentIntent;
      withCompletion: TINRequestPaymentIntentHandlingBlockMethod4); overload; cdecl; // API_DEPRECATED("resolveCurrencyAmountForRequestPayment:withCompletion: is deprecated. Use resolveCurrencyAmountForRequestPayment:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveNoteForRequestPayment(intent: INRequestPaymentIntent;
      withCompletion: TINRequestPaymentIntentHandlingBlockMethod6); cdecl;
    procedure resolvePayerForRequestPayment(intent: INRequestPaymentIntent; completion: TINRequestPaymentIntentHandlingBlockMethod3); overload; cdecl;
    procedure resolvePayerForRequestPayment(intent: INRequestPaymentIntent;
      withCompletion: TINRequestPaymentIntentHandlingBlockMethod2); overload; cdecl; // API_DEPRECATED("resolvePayerForRequestPayment:withCompletion: is deprecated. Use resolvePayerForRequestPayment:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INSearchForAccountsIntentClass = interface(INIntentClass)
    ['{EA50B12D-F713-43D5-8B2F-6059379E9B3C}']
  end;

  INSearchForAccountsIntent = interface(INIntent)
    ['{BA3610F1-1D0F-454B-9453-EB774367E25F}']
    function accountNickname: INSpeakableString; cdecl;
    function accountType: INAccountType; cdecl;
    function initWithAccountNickname(accountNickname: INSpeakableString; accountType: INAccountType; organizationName: INSpeakableString;
      requestedBalanceType: INBalanceType): Pointer; cdecl;
    function organizationName: INSpeakableString; cdecl;
    function requestedBalanceType: INBalanceType; cdecl;
  end;
  TINSearchForAccountsIntent = class(TOCGenericImport<INSearchForAccountsIntentClass, INSearchForAccountsIntent>) end;

  INSearchForAccountsIntentHandling = interface(IObjectiveC)
    ['{1E652618-1F23-491F-AFA2-8DB9EADB89EE}']
    procedure confirmSearchForAccounts(intent: INSearchForAccountsIntent; completion: TINSearchForAccountsIntentHandlingBlockMethod1); cdecl;
    procedure handleSearchForAccounts(intent: INSearchForAccountsIntent; completion: TINSearchForAccountsIntentHandlingBlockMethod1); cdecl;
    procedure resolveAccountNicknameForSearchForAccounts(intent: INSearchForAccountsIntent;
      withCompletion: TINSearchForAccountsIntentHandlingBlockMethod2); cdecl;
    procedure resolveAccountTypeForSearchForAccounts(intent: INSearchForAccountsIntent;
      withCompletion: TINSearchForAccountsIntentHandlingBlockMethod3); cdecl;
    procedure resolveOrganizationNameForSearchForAccounts(intent: INSearchForAccountsIntent;
      withCompletion: TINSearchForAccountsIntentHandlingBlockMethod2); cdecl;
    procedure resolveRequestedBalanceTypeForSearchForAccounts(intent: INSearchForAccountsIntent;
      withCompletion: TINSearchForAccountsIntentHandlingBlockMethod4); cdecl;
  end;

  INSearchForBillsIntentClass = interface(INIntentClass)
    ['{54E5EFD8-74CA-4336-8549-9AEACF9C8F46}']
  end;

  INSearchForBillsIntent = interface(INIntent)
    ['{2E7728F9-B4E2-41C8-B46B-0717A212FF8F}']
    function billPayee: INBillPayee; cdecl;
    function billType: INBillType; cdecl;
    function dueDateRange: INDateComponentsRange; cdecl;
    function initWithBillPayee(billPayee: INBillPayee; paymentDateRange: INDateComponentsRange; billType: INBillType; status: INPaymentStatus;
      dueDateRange: INDateComponentsRange): Pointer; cdecl;
    function paymentDateRange: INDateComponentsRange; cdecl;
    function status: INPaymentStatus; cdecl;
  end;
  TINSearchForBillsIntent = class(TOCGenericImport<INSearchForBillsIntentClass, INSearchForBillsIntent>) end;

  INSearchForBillsIntentHandling = interface(IObjectiveC)
    ['{7CB09591-C9E5-4242-AE56-87098FFF221A}']
    procedure confirmSearchForBills(intent: INSearchForBillsIntent; completion: TINSearchForBillsIntentHandlingBlockMethod1); cdecl;
    procedure handleSearchForBills(intent: INSearchForBillsIntent; completion: TINSearchForBillsIntentHandlingBlockMethod1); cdecl;
    procedure resolveBillPayeeForSearchForBills(intent: INSearchForBillsIntent; withCompletion: TINSearchForBillsIntentHandlingBlockMethod2); cdecl;
    procedure resolveBillTypeForSearchForBills(intent: INSearchForBillsIntent; withCompletion: TINSearchForBillsIntentHandlingBlockMethod4); cdecl;
    procedure resolveDueDateRangeForSearchForBills(intent: INSearchForBillsIntent; withCompletion: TINSearchForBillsIntentHandlingBlockMethod3); cdecl;
    procedure resolvePaymentDateRangeForSearchForBills(intent: INSearchForBillsIntent;
      withCompletion: TINSearchForBillsIntentHandlingBlockMethod3); cdecl;
    procedure resolveStatusForSearchForBills(intent: INSearchForBillsIntent; withCompletion: TINSearchForBillsIntentHandlingBlockMethod5); cdecl;
  end;

  INSendPaymentIntentClass = interface(INIntentClass)
    ['{E7F5B896-A36C-4143-8145-997C27C42688}']
  end;

  INSendPaymentIntent = interface(INIntent)
    ['{1EC81132-2954-4596-A374-39305E34E83D}']
    function currencyAmount: INCurrencyAmount; cdecl;
    function initWithPayee(payee: INPerson; currencyAmount: INCurrencyAmount; note: NSString): Pointer; cdecl;
    function note: NSString; cdecl;
    function payee: INPerson; cdecl;
  end;
  TINSendPaymentIntent = class(TOCGenericImport<INSendPaymentIntentClass, INSendPaymentIntent>) end;

  INSendPaymentIntentHandling = interface(IObjectiveC)
    ['{DE3E9502-606B-4BB2-9CCB-8DBC4CBA2DC2}']
    procedure confirmSendPayment(intent: INSendPaymentIntent; completion: TINSendPaymentIntentHandlingBlockMethod1); cdecl;
    procedure handleSendPayment(intent: INSendPaymentIntent; completion: TINSendPaymentIntentHandlingBlockMethod1); cdecl;
    procedure resolveCurrencyAmountForSendPayment(intent: INSendPaymentIntent;
      completion: TINSendPaymentIntentHandlingBlockMethod5); overload; cdecl;
    procedure resolveCurrencyAmountForSendPayment(intent: INSendPaymentIntent;
       withCompletion: TINSendPaymentIntentHandlingBlockMethod4); overload; cdecl; // API_DEPRECATED("resolveCurrencyAmountForSendPayment:withCompletion: is deprecated. Use resolveCurrencyAmountForSendPayment:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveNoteForSendPayment(intent: INSendPaymentIntent; withCompletion: TINSendPaymentIntentHandlingBlockMethod6); cdecl;
    procedure resolvePayeeForSendPayment(intent: INSendPaymentIntent; completion: TINSendPaymentIntentHandlingBlockMethod3); overload; cdecl;
    procedure resolvePayeeForSendPayment(intent: INSendPaymentIntent; withCompletion: TINSendPaymentIntentHandlingBlockMethod2); overload; cdecl; // API_DEPRECATED("resolvePayeeForSendPayment:withCompletion: is deprecated. Use resolvePayeeForSendPayment:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INTransferMoneyIntentClass = interface(INIntentClass)
    ['{A5DEE7B7-E7C7-4E43-926B-A7D03E9D5928}']
  end;

  INTransferMoneyIntent = interface(INIntent)
    ['{8AEF1D0A-840E-40B8-8790-6C791A74E34D}']
    function fromAccount: INPaymentAccount; cdecl;
    function initWithFromAccount(fromAccount: INPaymentAccount; toAccount: INPaymentAccount; transactionAmount: INPaymentAmount;
      transactionScheduledDate: INDateComponentsRange; transactionNote: NSString): Pointer; cdecl;
    function toAccount: INPaymentAccount; cdecl;
    function transactionAmount: INPaymentAmount; cdecl;
    function transactionNote: NSString; cdecl;
    function transactionScheduledDate: INDateComponentsRange; cdecl;
  end;
  TINTransferMoneyIntent = class(TOCGenericImport<INTransferMoneyIntentClass, INTransferMoneyIntent>) end;

  INTransferMoneyIntentHandling = interface(IObjectiveC)
    ['{5C1C27AD-CC92-429A-A265-4BB6CDF8C3B6}']
    procedure confirmTransferMoney(intent: INTransferMoneyIntent; completion: TINTransferMoneyIntentHandlingBlockMethod1); cdecl;
    procedure handleTransferMoney(intent: INTransferMoneyIntent; completion: TINTransferMoneyIntentHandlingBlockMethod1); cdecl;
    procedure resolveFromAccountForTransferMoney(intent: INTransferMoneyIntent; withCompletion: TINTransferMoneyIntentHandlingBlockMethod2); cdecl;
    procedure resolveToAccountForTransferMoney(intent: INTransferMoneyIntent; withCompletion: TINTransferMoneyIntentHandlingBlockMethod2); cdecl;
    procedure resolveTransactionAmountForTransferMoney(intent: INTransferMoneyIntent;
      withCompletion: TINTransferMoneyIntentHandlingBlockMethod3); cdecl;
    procedure resolveTransactionNoteForTransferMoney(intent: INTransferMoneyIntent;
      withCompletion: TINTransferMoneyIntentHandlingBlockMethod5); cdecl;
    procedure resolveTransactionScheduledDateForTransferMoney(intent: INTransferMoneyIntent;
      withCompletion: TINTransferMoneyIntentHandlingBlockMethod4); cdecl;
  end;

  INSearchForPhotosIntentClass = interface(INIntentClass)
    ['{1AFEC76A-612F-44A8-9E32-7D4499535322}']
  end;

  INSearchForPhotosIntent = interface(INIntent)
    ['{7D8EF797-3D5D-4B32-8F38-A3A15337AC79}']
    function albumName: NSString; cdecl;
    function dateCreated: INDateComponentsRange; cdecl;
    function excludedAttributes: INPhotoAttributeOptions; cdecl;
    function includedAttributes: INPhotoAttributeOptions; cdecl;
    function initWithDateCreated(dateCreated: INDateComponentsRange; locationCreated: CLPlacemark; albumName: NSString; searchTerms: NSArray;
      includedAttributes: INPhotoAttributeOptions; excludedAttributes: INPhotoAttributeOptions; peopleInPhoto: NSArray): Pointer; cdecl;
    function locationCreated: CLPlacemark; cdecl;
    function peopleInPhoto: NSArray; cdecl;
    function peopleInPhotoOperator: INConditionalOperator; cdecl;
    function searchTerms: NSArray; cdecl;
    function searchTermsOperator: INConditionalOperator; cdecl;
  end;
  TINSearchForPhotosIntent = class(TOCGenericImport<INSearchForPhotosIntentClass, INSearchForPhotosIntent>) end;

  INSearchForPhotosIntentHandling = interface(IObjectiveC)
    ['{448778F1-E39C-49C7-9B06-E81CD3A1CAE2}']
    procedure confirmSearchForPhotos(intent: INSearchForPhotosIntent; completion: TINSearchForPhotosIntentHandlingBlockMethod1); cdecl;
    procedure handleSearchForPhotos(intent: INSearchForPhotosIntent; completion: TINSearchForPhotosIntentHandlingBlockMethod1); cdecl;
    procedure resolveAlbumNameForSearchForPhotos(intent: INSearchForPhotosIntent; withCompletion: TINSearchForPhotosIntentHandlingBlockMethod4); cdecl;
    procedure resolveDateCreatedForSearchForPhotos(intent: INSearchForPhotosIntent;
      withCompletion: TINSearchForPhotosIntentHandlingBlockMethod2); cdecl;
    procedure resolveLocationCreatedForSearchForPhotos(intent: INSearchForPhotosIntent;
      withCompletion: TINSearchForPhotosIntentHandlingBlockMethod3); cdecl;
    procedure resolvePeopleInPhotoForSearchForPhotos(intent: INSearchForPhotosIntent;
      withCompletion: TINSearchForPhotosIntentHandlingBlockMethod5); cdecl;
    procedure resolveSearchTermsForSearchForPhotos(intent: INSearchForPhotosIntent;
      withCompletion: TINSearchForPhotosIntentHandlingBlockMethod5); cdecl;
  end;

  INStartPhotoPlaybackIntentClass = interface(INIntentClass)
    ['{BEC2BF26-5600-4299-92F1-C420270AD153}']
  end;

  INStartPhotoPlaybackIntent = interface(INIntent)
    ['{E092B985-8D02-4E1A-BFB9-C2921E59A1AE}']
    function albumName: NSString; cdecl;
    function dateCreated: INDateComponentsRange; cdecl;
    function excludedAttributes: INPhotoAttributeOptions; cdecl;
    function includedAttributes: INPhotoAttributeOptions; cdecl;
    function initWithDateCreated(dateCreated: INDateComponentsRange; locationCreated: CLPlacemark; albumName: NSString; searchTerms: NSArray;
      includedAttributes: INPhotoAttributeOptions; excludedAttributes: INPhotoAttributeOptions; peopleInPhoto: NSArray): Pointer; cdecl;
    function locationCreated: CLPlacemark; cdecl;
    function peopleInPhoto: NSArray; cdecl;
    function peopleInPhotoOperator: INConditionalOperator; cdecl;
    function searchTerms: NSArray; cdecl;
    function searchTermsOperator: INConditionalOperator; cdecl;
  end;
  TINStartPhotoPlaybackIntent = class(TOCGenericImport<INStartPhotoPlaybackIntentClass, INStartPhotoPlaybackIntent>) end;

  INStartPhotoPlaybackIntentHandling = interface(IObjectiveC)
    ['{AD8D5EC0-3695-4F9D-9CA6-7B3F506E657A}']
    procedure confirmStartPhotoPlayback(intent: INStartPhotoPlaybackIntent; completion: TINStartPhotoPlaybackIntentHandlingBlockMethod1); cdecl;
    procedure handleStartPhotoPlayback(intent: INStartPhotoPlaybackIntent; completion: TINStartPhotoPlaybackIntentHandlingBlockMethod1); cdecl;
    procedure resolveAlbumNameForStartPhotoPlayback(intent: INStartPhotoPlaybackIntent;
      withCompletion: TINStartPhotoPlaybackIntentHandlingBlockMethod4); cdecl;
    procedure resolveDateCreatedForStartPhotoPlayback(intent: INStartPhotoPlaybackIntent;
      withCompletion: TINStartPhotoPlaybackIntentHandlingBlockMethod2); cdecl;
    procedure resolveLocationCreatedForStartPhotoPlayback(intent: INStartPhotoPlaybackIntent;
      withCompletion: TINStartPhotoPlaybackIntentHandlingBlockMethod3); cdecl;
    procedure resolvePeopleInPhotoForStartPhotoPlayback(intent: INStartPhotoPlaybackIntent;
      withCompletion: TINStartPhotoPlaybackIntentHandlingBlockMethod5); cdecl;
  end;

  INGetReservationDetailsIntentClass = interface(INIntentClass)
    ['{2E5A6DCB-D460-4EE6-9CDA-3C54C4993B3D}']
  end;

  INGetReservationDetailsIntent = interface(INIntent)
    ['{10BCBC75-8B4A-4ACB-B3CD-526DF2E1F413}']
    function initWithReservationContainerReference(reservationContainerReference: INSpeakableString;
      reservationItemReferences: NSArray): Pointer; cdecl;
    function reservationContainerReference: INSpeakableString; cdecl;
    function reservationItemReferences: NSArray; cdecl;
  end;
  TINGetReservationDetailsIntent = class(TOCGenericImport<INGetReservationDetailsIntentClass, INGetReservationDetailsIntent>) end;

  INGetRideStatusIntentClass = interface(INIntentClass)
    ['{D0A6CE7F-A0D2-4309-9F94-1C8C734AC5C2}']
  end;

  INGetRideStatusIntent = interface(INIntent)
    ['{92B61BCB-06CF-4F65-B2F8-4979DF52940D}']
  end;
  TINGetRideStatusIntent = class(TOCGenericImport<INGetRideStatusIntentClass, INGetRideStatusIntent>) end;

  INGetRideStatusIntentHandling = interface(IObjectiveC)
    ['{0AC9771A-9BD1-4893-AC75-3716D9836484}']
    procedure confirmGetRideStatus(intent: INGetRideStatusIntent; completion: TINGetRideStatusIntentHandlingBlockMethod1); cdecl;
    procedure handleGetRideStatus(intent: INGetRideStatusIntent; completion: TINGetRideStatusIntentHandlingBlockMethod1); cdecl;
    procedure startSendingUpdatesForGetRideStatus(intent: INGetRideStatusIntent; toObserver: Pointer); cdecl;
    procedure stopSendingUpdatesForGetRideStatus(intent: INGetRideStatusIntent); cdecl;
  end;

  INGetRideStatusIntentResponseObserver = interface(IObjectiveC)
    ['{B054D8C9-A555-474B-8AA4-A49A6D5FF946}']
    procedure getRideStatusResponseDidUpdate(response: INGetRideStatusIntentResponse); cdecl;
  end;

  INListRideOptionsIntentClass = interface(INIntentClass)
    ['{7A891637-5C8E-43BC-A544-A74FEDF640C8}']
  end;

  INListRideOptionsIntent = interface(INIntent)
    ['{58882866-51D8-4C79-837C-98DDC59EB154}']
    function dropOffLocation: CLPlacemark; cdecl;
    function initWithPickupLocation(pickupLocation: CLPlacemark; dropOffLocation: CLPlacemark): Pointer; cdecl;
    function pickupLocation: CLPlacemark; cdecl;
  end;
  TINListRideOptionsIntent = class(TOCGenericImport<INListRideOptionsIntentClass, INListRideOptionsIntent>) end;

  INListRideOptionsIntentHandling = interface(IObjectiveC)
    ['{1DDD39B7-E6B3-441D-965C-5B0B964537F4}']
    procedure confirmListRideOptions(intent: INListRideOptionsIntent; completion: TINListRideOptionsIntentHandlingBlockMethod1); cdecl;
    procedure handleListRideOptions(intent: INListRideOptionsIntent; completion: TINListRideOptionsIntentHandlingBlockMethod1); cdecl;
    procedure resolveDropOffLocationForListRideOptions(intent: INListRideOptionsIntent;
      withCompletion: TINListRideOptionsIntentHandlingBlockMethod2); cdecl;
    procedure resolvePickupLocationForListRideOptions(intent: INListRideOptionsIntent;
      withCompletion: TINListRideOptionsIntentHandlingBlockMethod2); cdecl;
  end;

  INRequestRideIntentClass = interface(INIntentClass)
    ['{8B855198-1DA6-427B-9730-9DF58FAAB0EA}']
  end;

  INRequestRideIntent = interface(INIntent)
    ['{3CAE0374-7752-4E66-91F4-AB0475A93485}']
    function dropOffLocation: CLPlacemark; cdecl;
    function initWithPickupLocation(pickupLocation: CLPlacemark; dropOffLocation: CLPlacemark; rideOptionName: INSpeakableString;
      partySize: NSNumber; paymentMethod: INPaymentMethod): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.3))
    function initWithPickupLocation(pickupLocation: CLPlacemark; dropOffLocation: CLPlacemark; rideOptionName: INSpeakableString;
      partySize: NSNumber; paymentMethod: INPaymentMethod; scheduledPickupTime: INDateComponentsRange): Pointer; overload; cdecl;
    function partySize: NSNumber; cdecl;
    function paymentMethod: INPaymentMethod; cdecl;
    function pickupLocation: CLPlacemark; cdecl;
    function rideOptionName: INSpeakableString; cdecl;
    function scheduledPickupTime: INDateComponentsRange; cdecl;
  end;
  TINRequestRideIntent = class(TOCGenericImport<INRequestRideIntentClass, INRequestRideIntent>) end;

  INRequestRideIntentHandling = interface(IObjectiveC)
    ['{150F2E38-A465-4511-A9B4-BA30DC3FDF8C}']
    procedure confirmRequestRide(intent: INRequestRideIntent; completion: TINRequestRideIntentHandlingBlockMethod1); cdecl;
    procedure handleRequestRide(intent: INRequestRideIntent; completion: TINRequestRideIntentHandlingBlockMethod1); cdecl;
    procedure resolveDropOffLocationForRequestRide(intent: INRequestRideIntent; withCompletion: TINRequestRideIntentHandlingBlockMethod2); cdecl;
    procedure resolvePartySizeForRequestRide(intent: INRequestRideIntent; withCompletion: TINRequestRideIntentHandlingBlockMethod4); cdecl;
    procedure resolvePickupLocationForRequestRide(intent: INRequestRideIntent; withCompletion: TINRequestRideIntentHandlingBlockMethod2); cdecl;
    procedure resolveRideOptionNameForRequestRide(intent: INRequestRideIntent; withCompletion: TINRequestRideIntentHandlingBlockMethod3); cdecl;
    procedure resolveScheduledPickupTimeForRequestRide(intent: INRequestRideIntent; withCompletion: TINRequestRideIntentHandlingBlockMethod5); cdecl;
  end;

  INCancelRideIntentClass = interface(INIntentClass)
    ['{EAFD5565-A6DD-4E29-8463-A0391391FADF}']
  end;

  INCancelRideIntent = interface(INIntent)
    ['{FDACA211-7802-4F00-9578-99673C562591}']
    function initWithRideIdentifier(rideIdentifier: NSString): Pointer; cdecl;
    function rideIdentifier: NSString; cdecl;
  end;
  TINCancelRideIntent = class(TOCGenericImport<INCancelRideIntentClass, INCancelRideIntent>) end;

  INCancelRideIntentHandling = interface(IObjectiveC)
    ['{6738E4DA-8398-4B47-AEEA-CE00D6FEE667}']
    procedure confirmCancelRide(intent: INCancelRideIntent; completion: TINCancelRideIntentHandlingBlockMethod1); cdecl;
    procedure handleCancelRide(intent: INCancelRideIntent; completion: TINCancelRideIntentHandlingBlockMethod1); cdecl;
  end;

  INSendRideFeedbackIntentClass = interface(INIntentClass)
    ['{6C7E5D85-E4C0-49FE-BF92-11280A132DD2}']
  end;

  INSendRideFeedbackIntent = interface(INIntent)
    ['{5CC3636B-EEF5-481B-AAD7-2C6231F2F396}']
    function initWithRideIdentifier(rideIdentifier: NSString): Pointer; cdecl;
    function rating: NSNumber; cdecl;
    function rideIdentifier: NSString; cdecl;
    procedure setRating(rating: NSNumber); cdecl;
    procedure setTip(tip: INCurrencyAmount); cdecl;
    function tip: INCurrencyAmount; cdecl;
  end;
  TINSendRideFeedbackIntent = class(TOCGenericImport<INSendRideFeedbackIntentClass, INSendRideFeedbackIntent>) end;

  INSendRideFeedbackIntentHandling = interface(IObjectiveC)
    ['{185ABB04-71B4-477F-A710-6EDCEAA3360E}']
    procedure confirmSendRideFeedback(sendRideFeedbackIntent: INSendRideFeedbackIntent;
      completion: TINSendRideFeedbackIntentHandlingBlockMethod1); cdecl;
    procedure handleSendRideFeedback(sendRideFeedbackintent: INSendRideFeedbackIntent;
      completion: TINSendRideFeedbackIntentHandlingBlockMethod1); cdecl;
  end;

  INGetVisualCodeIntentClass = interface(INIntentClass)
    ['{AF182984-7F25-476C-8708-6CD63286D27A}']
  end;

  INGetVisualCodeIntent = interface(INIntent)
    ['{E27F98C3-C1CF-4869-89F7-3CDA980F88E6}']
    function initWithVisualCodeType(visualCodeType: INVisualCodeType): Pointer; cdecl;
    function visualCodeType: INVisualCodeType; cdecl;
  end;
  TINGetVisualCodeIntent = class(TOCGenericImport<INGetVisualCodeIntentClass, INGetVisualCodeIntent>) end;

  INGetVisualCodeIntentHandling = interface(IObjectiveC)
    ['{8D2562CA-D71C-4C7F-A16F-0CED0A1F77B2}']
    procedure confirmGetVisualCode(intent: INGetVisualCodeIntent; completion: TINGetVisualCodeIntentHandlingBlockMethod1); cdecl;
    procedure handleGetVisualCode(intent: INGetVisualCodeIntent; completion: TINGetVisualCodeIntentHandlingBlockMethod1); cdecl;
    procedure resolveVisualCodeTypeForGetVisualCode(intent: INGetVisualCodeIntent; withCompletion: TINGetVisualCodeIntentHandlingBlockMethod2); cdecl;
  end;

  INCallsDomainHandling = interface(IObjectiveC)
    ['{1978DC61-76BF-4D48-B797-DC8D532153D5}']
  end;

  INCarCommandsDomainHandling = interface(IObjectiveC)
    ['{73F27F5F-1238-4EA6-8B8F-FA023B3D843E}']
  end;

  INCarPlayDomainHandling = interface(IObjectiveC)
    ['{6570DEB2-6C55-40BF-9D92-BFFB1D54AED5}']
  end;

  INWorkoutsDomainHandling = interface(IObjectiveC)
    ['{595A9672-08B7-438B-BDE5-4EB236BFE7E7}']
  end;

  INRadioDomainHandling = interface(IObjectiveC)
    ['{D91E4BE8-FEC9-47E7-B890-0560F3B506FA}']
  end;

  INMessagesDomainHandling = interface(IObjectiveC)
    ['{91F2CFD1-0E50-4D16-BE66-0D66001435AE}']
  end;

  INPaymentsDomainHandling = interface(IObjectiveC)
    ['{3978F0A8-0AA8-424A-816E-11B95637B22A}']
  end;

  INPhotosDomainHandling = interface(IObjectiveC)
    ['{21601ADC-7C93-417C-9B89-20B0D61E1DF1}']
  end;

  INRidesharingDomainHandling = interface(IObjectiveC)
    ['{78B2BD00-BDC7-45D3-9EB7-8E4A09EE2674}']
  end;

  INNotebookDomainHandling = interface(IObjectiveC)
    ['{7D282830-7621-4F64-8538-7258C0B3B480}']
  end;

  INVisualCodeDomainHandling = interface(IObjectiveC)
    ['{64DB16F9-D6F2-4A5B-B6E6-837EE35E059E}']
  end;

  INInteractionClass = interface(NSObjectClass)
    ['{E29670AC-D02C-4A00-8516-FD31F74CE8F3}']
    {class} procedure deleteAllInteractionsWithCompletion(completion: TINInteractionBlockMethod1); cdecl;
    {class} procedure deleteInteractionsWithGroupIdentifier(groupIdentifier: NSString; completion: TINInteractionBlockMethod1); cdecl;
    {class} procedure deleteInteractionsWithIdentifiers(identifiers: NSArray; completion: TINInteractionBlockMethod1); cdecl;
  end;

  INInteraction = interface(NSObject)
    ['{37637F7E-6757-47ED-93FE-D4FEBE5E8246}']
    function dateInterval: NSDateInterval; cdecl;
    function direction: INInteractionDirection; cdecl;
    procedure donateInteractionWithCompletion(completion: TINInteractionBlockMethod1); cdecl;
    function groupIdentifier: NSString; cdecl;
    function identifier: NSString; cdecl;
    function initWithIntent(intent: INIntent; response: INIntentResponse): Pointer; cdecl;
    function intent: INIntent; cdecl;
    function intentHandlingStatus: INIntentHandlingStatus; cdecl;
    function intentResponse: INIntentResponse; cdecl;
    function parameterValueForParameter(parameter: INParameter): Pointer; cdecl;
    procedure setDateInterval(dateInterval: NSDateInterval); cdecl;
    procedure setDirection(direction: INInteractionDirection); cdecl;
    procedure setGroupIdentifier(groupIdentifier: NSString); cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
  end;
  TINInteraction = class(TOCGenericImport<INInteractionClass, INInteraction>) end;

  INSpeakable = interface(IObjectiveC)
    ['{43B26E01-0981-4053-99D9-A59029924CF9}']
    function alternativeSpeakableMatches: NSArray; cdecl;
    function identifier: NSString; cdecl; // API_DEPRECATED("Please use vocabularyIdentifier", ios(10.0, 11.0), watchos(3.2, 4.0))
    function pronunciationHint: NSString; cdecl;
    function spokenPhrase: NSString; cdecl;
    function vocabularyIdentifier: NSString; cdecl;
  end;

  INParameterClass = interface(NSObjectClass)
    ['{2471B1A5-AB53-4B99-80A8-18F5107BB2D7}']
    {class} function parameterForClass(aClass: Pointer; keyPath: NSString): Pointer; cdecl;
  end;

  INParameter = interface(NSObject)
    ['{7FFECFCF-3B29-4001-92AB-9A1073D37538}']
    function indexForSubKeyPath(subKeyPath: NSString): NSUInteger; cdecl;
    function isEqualToParameter(parameter: INParameter): Boolean; cdecl;
    function parameterClass: Pointer; cdecl;
    function parameterKeyPath: NSString; cdecl;
    procedure setIndex(index: NSUInteger; forSubKeyPath: NSString); cdecl;
  end;
  TINParameter = class(TOCGenericImport<INParameterClass, INParameter>) end;

  INObjectSectionClass = interface(NSObjectClass)
    ['{78A8C240-9BE2-4FE2-875D-64FAAA6AC029}']
  end;

  INObjectSection = interface(NSObject)
    ['{0896F0B9-FA7E-4452-871C-0BFE3B56FD43}']
    function initWithTitle(title: NSString; items: NSArray): Pointer; cdecl;
    function items: NSArray; cdecl;
    function title: NSString; cdecl;
  end;
  TINObjectSection = class(TOCGenericImport<INObjectSectionClass, INObjectSection>) end;

  INObjectCollectionClass = interface(NSObjectClass)
    ['{3FF4BF7C-9D16-45F1-8A61-9044DDF4B778}']
  end;

  INObjectCollection = interface(NSObject)
    ['{CA77DD69-1035-498B-B1D6-FA8E194DA71B}']
    function allItems: NSArray; cdecl;
    function initWithItems(items: NSArray): Pointer; cdecl;
    function initWithSections(sections: NSArray): Pointer; cdecl;
    function sections: NSArray; cdecl;
    procedure setUsesIndexedCollation(usesIndexedCollation: Boolean); cdecl;
    function usesIndexedCollation: Boolean; cdecl;
  end;
  TINObjectCollection = class(TOCGenericImport<INObjectCollectionClass, INObjectCollection>) end;

  INSearchCallHistoryIntentResponseClass = interface(INIntentResponseClass)
    ['{8CDE4175-C4B3-48CD-A80C-039666157675}']
  end;

  INSearchCallHistoryIntentResponse = interface(INIntentResponse)
    ['{FA753BC8-3664-44E2-9B72-DFF2A969CB8E}']
    function callRecords: NSArray; cdecl;
    function code: INSearchCallHistoryIntentResponseCode; cdecl;
    function initWithCode(code: INSearchCallHistoryIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCallRecords(callRecords: NSArray); cdecl;
  end;
  TINSearchCallHistoryIntentResponse = class(TOCGenericImport<INSearchCallHistoryIntentResponseClass, INSearchCallHistoryIntentResponse>) end;

  INStartAudioCallIntentResponseClass = interface(INIntentResponseClass)
    ['{80817A39-1005-49B2-8F7F-EADCD06D7F72}']
  end;

  INStartAudioCallIntentResponse = interface(INIntentResponse)
    ['{E0F26E10-F8F9-4E65-A839-A1AE4EBF4A0B}']
    function code: INStartAudioCallIntentResponseCode; cdecl;
    function initWithCode(code: INStartAudioCallIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINStartAudioCallIntentResponse = class(TOCGenericImport<INStartAudioCallIntentResponseClass, INStartAudioCallIntentResponse>) end;

  INStartCallIntentResponseClass = interface(INIntentResponseClass)
    ['{228AC6BE-C609-43B2-9EE0-1B6F1AC264B5}']
  end;

  INStartCallIntentResponse = interface(INIntentResponse)
    ['{282142D0-CC3A-4A80-9D8C-08F386F88B21}']
    function code: INStartCallIntentResponseCode; cdecl;
    function initWithCode(code: INStartCallIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINStartCallIntentResponse = class(TOCGenericImport<INStartCallIntentResponseClass, INStartCallIntentResponse>) end;

  INStartVideoCallIntentResponseClass = interface(INIntentResponseClass)
    ['{687BE75A-7005-4DF3-9EA3-7C1D64F8CAB8}']
  end;

  INStartVideoCallIntentResponse = interface(INIntentResponse)
    ['{8FA4B427-C197-44BF-A9CA-E55AF270EC58}']
    function code: INStartVideoCallIntentResponseCode; cdecl;
    function initWithCode(code: INStartVideoCallIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINStartVideoCallIntentResponse = class(TOCGenericImport<INStartVideoCallIntentResponseClass, INStartVideoCallIntentResponse>) end;

  INActivateCarSignalIntentResponseClass = interface(INIntentResponseClass)
    ['{199F4EE8-AE25-43C9-B34B-589948CE0048}']
  end;

  INActivateCarSignalIntentResponse = interface(INIntentResponse)
    ['{259BE8CF-670A-4715-8B87-2C887D23CEEE}']
    function code: INActivateCarSignalIntentResponseCode; cdecl;
    function initWithCode(code: INActivateCarSignalIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setSignals(signals: INCarSignalOptions); cdecl;
    function signals: INCarSignalOptions; cdecl;
  end;
  TINActivateCarSignalIntentResponse = class(TOCGenericImport<INActivateCarSignalIntentResponseClass, INActivateCarSignalIntentResponse>) end;

  INGetCarLockStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{4C7C4873-ACCC-497B-BF7D-65A740E93142}']
  end;

  INGetCarLockStatusIntentResponse = interface(INIntentResponse)
    ['{58A9B07F-6D3A-493C-923E-5A9773F71914}']
    function code: INGetCarLockStatusIntentResponseCode; cdecl;
    function initWithCode(code: INGetCarLockStatusIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function locked: NSNumber; cdecl;
    procedure setLocked(locked: NSNumber); cdecl;
  end;
  TINGetCarLockStatusIntentResponse = class(TOCGenericImport<INGetCarLockStatusIntentResponseClass, INGetCarLockStatusIntentResponse>) end;

  INGetCarPowerLevelStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{4687F9D6-FE9C-41E6-802D-0737F3612A8A}']
  end;

  INGetCarPowerLevelStatusIntentResponse = interface(INIntentResponse)
    ['{EAD1B6D5-03E3-48B5-B476-1B063FD887B5}']
    function activeConnector: INCarChargingConnectorType; cdecl;
    function carIdentifier: NSString; cdecl;
    function chargePercentRemaining: NSNumber; cdecl;
    function charging: NSNumber; cdecl;
    function chargingFormulaArguments: NSDictionary; cdecl;
    function code: INGetCarPowerLevelStatusIntentResponseCode; cdecl;
    function consumptionFormulaArguments: NSDictionary; cdecl;
    function currentBatteryCapacity: NSMeasurement; cdecl;
    function dateOfLastStateUpdate: NSDateComponents; cdecl;
    function distanceRemaining: NSMeasurement; cdecl;
    function distanceRemainingElectric: NSMeasurement; cdecl;
    function distanceRemainingFuel: NSMeasurement; cdecl;
    function fuelPercentRemaining: NSNumber; cdecl;
    function initWithCode(code: INGetCarPowerLevelStatusIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function maximumBatteryCapacity: NSMeasurement; cdecl;
    function maximumDistance: NSMeasurement; cdecl;
    function maximumDistanceElectric: NSMeasurement; cdecl;
    function maximumDistanceFuel: NSMeasurement; cdecl;
    function minimumBatteryCapacity: NSMeasurement; cdecl;
    function minutesToFull: NSNumber; cdecl;
    procedure setActiveConnector(activeConnector: INCarChargingConnectorType); cdecl;
    procedure setCarIdentifier(carIdentifier: NSString); cdecl;
    procedure setChargePercentRemaining(chargePercentRemaining: NSNumber); cdecl;
    procedure setCharging(charging: NSNumber); cdecl;
    procedure setChargingFormulaArguments(chargingFormulaArguments: NSDictionary); cdecl;
    procedure setConsumptionFormulaArguments(consumptionFormulaArguments: NSDictionary); cdecl;
    procedure setCurrentBatteryCapacity(currentBatteryCapacity: NSMeasurement); cdecl;
    procedure setDateOfLastStateUpdate(dateOfLastStateUpdate: NSDateComponents); cdecl;
    procedure setDistanceRemaining(distanceRemaining: NSMeasurement); cdecl;
    procedure setDistanceRemainingElectric(distanceRemainingElectric: NSMeasurement); cdecl;
    procedure setDistanceRemainingFuel(distanceRemainingFuel: NSMeasurement); cdecl;
    procedure setFuelPercentRemaining(fuelPercentRemaining: NSNumber); cdecl;
    procedure setMaximumBatteryCapacity(maximumBatteryCapacity: NSMeasurement); cdecl;
    procedure setMaximumDistance(maximumDistance: NSMeasurement); cdecl;
    procedure setMaximumDistanceElectric(maximumDistanceElectric: NSMeasurement); cdecl;
    procedure setMaximumDistanceFuel(maximumDistanceFuel: NSMeasurement); cdecl;
    procedure setMinimumBatteryCapacity(minimumBatteryCapacity: NSMeasurement); cdecl;
    procedure setMinutesToFull(minutesToFull: NSNumber); cdecl;
  end;
  TINGetCarPowerLevelStatusIntentResponse = class(TOCGenericImport<INGetCarPowerLevelStatusIntentResponseClass,
    INGetCarPowerLevelStatusIntentResponse>) end;

  INListCarsIntentResponseClass = interface(INIntentResponseClass)
    ['{317A3F79-2F20-4F64-92CE-5E41F863BD7D}']
  end;

  INListCarsIntentResponse = interface(INIntentResponse)
    ['{1A94CCFD-108A-441D-B41A-E6563AF6E75D}']
    function cars: NSArray; cdecl;
    function code: INListCarsIntentResponseCode; cdecl;
    function initWithCode(code: INListCarsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCars(cars: NSArray); cdecl;
  end;
  TINListCarsIntentResponse = class(TOCGenericImport<INListCarsIntentResponseClass, INListCarsIntentResponse>) end;

  INSaveProfileInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{3E5391A6-C126-400F-A282-94ED239DD972}']
  end;

  INSaveProfileInCarIntentResponse = interface(INIntentResponse)
    ['{893E210C-441E-4913-935E-3C6BC226636C}']
    function code: INSaveProfileInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSaveProfileInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSaveProfileInCarIntentResponse = class(TOCGenericImport<INSaveProfileInCarIntentResponseClass, INSaveProfileInCarIntentResponse>) end;

  INSetAudioSourceInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{6BAA35F8-B748-4150-A9F1-04AD753781DD}']
  end;

  INSetAudioSourceInCarIntentResponse = interface(INIntentResponse)
    ['{E7BD5D4B-DA52-4E63-9E34-C38CD9E36BD1}']
    function code: INSetAudioSourceInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetAudioSourceInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetAudioSourceInCarIntentResponse = class(TOCGenericImport<INSetAudioSourceInCarIntentResponseClass, INSetAudioSourceInCarIntentResponse>) end;

  INSetCarLockStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{C92B817E-5212-49E3-8306-531745807812}']
  end;

  INSetCarLockStatusIntentResponse = interface(INIntentResponse)
    ['{D410D32B-E900-4542-B694-E951D863333B}']
    function code: INSetCarLockStatusIntentResponseCode; cdecl;
    function initWithCode(code: INSetCarLockStatusIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetCarLockStatusIntentResponse = class(TOCGenericImport<INSetCarLockStatusIntentResponseClass, INSetCarLockStatusIntentResponse>) end;

  INSetClimateSettingsInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{E3A092F0-978C-4A0D-93F0-E9F68CEE5B1F}']
  end;

  INSetClimateSettingsInCarIntentResponse = interface(INIntentResponse)
    ['{F93DFB39-5AB1-4C4F-8ED1-606DB5AB76F1}']
    function code: INSetClimateSettingsInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetClimateSettingsInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetClimateSettingsInCarIntentResponse = class(TOCGenericImport<INSetClimateSettingsInCarIntentResponseClass,
    INSetClimateSettingsInCarIntentResponse>) end;

  INSetDefrosterSettingsInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{0B1A5390-8221-49EE-B7F8-54F1ECF56E86}']
  end;

  INSetDefrosterSettingsInCarIntentResponse = interface(INIntentResponse)
    ['{8D712B54-5108-4E4E-8014-F53C57A3EBA3}']
    function code: INSetDefrosterSettingsInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetDefrosterSettingsInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetDefrosterSettingsInCarIntentResponse = class(TOCGenericImport<INSetDefrosterSettingsInCarIntentResponseClass,
    INSetDefrosterSettingsInCarIntentResponse>) end;

  INSetProfileInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{AB1A985A-80F4-4343-B5F7-8AAD30DD88AC}']
  end;

  INSetProfileInCarIntentResponse = interface(INIntentResponse)
    ['{7424A192-F4EB-4829-A4A0-34776B13645E}']
    function code: INSetProfileInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetProfileInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetProfileInCarIntentResponse = class(TOCGenericImport<INSetProfileInCarIntentResponseClass, INSetProfileInCarIntentResponse>) end;

  INSetSeatSettingsInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{591F3DA4-C471-4096-BE27-0B45B1E8BBB8}']
  end;

  INSetSeatSettingsInCarIntentResponse = interface(INIntentResponse)
    ['{F6AF3180-4E6D-4F70-8C25-1D3A822BB332}']
    function code: INSetSeatSettingsInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetSeatSettingsInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetSeatSettingsInCarIntentResponse = class(TOCGenericImport<INSetSeatSettingsInCarIntentResponseClass, INSetSeatSettingsInCarIntentResponse>) end;

  INCancelWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{342E9EB3-7004-4758-9F72-292D269909E9}']
  end;

  INCancelWorkoutIntentResponse = interface(INIntentResponse)
    ['{1D0037E2-5770-4A3F-BC01-5D5B6E1CA5B6}']
    function code: INCancelWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INCancelWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINCancelWorkoutIntentResponse = class(TOCGenericImport<INCancelWorkoutIntentResponseClass, INCancelWorkoutIntentResponse>) end;

  INEndWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{AFF30457-A38E-4EAA-96AF-F10A4FAE5658}']
  end;

  INEndWorkoutIntentResponse = interface(INIntentResponse)
    ['{C1C2C03F-BFD3-4E28-ACD8-8DDD93E47866}']
    function code: INEndWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INEndWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINEndWorkoutIntentResponse = class(TOCGenericImport<INEndWorkoutIntentResponseClass, INEndWorkoutIntentResponse>) end;

  INPauseWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{184C3D2B-1D88-4C85-8F80-34338689AF4D}']
  end;

  INPauseWorkoutIntentResponse = interface(INIntentResponse)
    ['{DCED72D0-335D-4B46-8162-CBBF1475AC97}']
    function code: INPauseWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INPauseWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINPauseWorkoutIntentResponse = class(TOCGenericImport<INPauseWorkoutIntentResponseClass, INPauseWorkoutIntentResponse>) end;

  INResumeWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{6C3DB8CD-5341-4BBC-BA9E-9422AC494482}']
  end;

  INResumeWorkoutIntentResponse = interface(INIntentResponse)
    ['{662FEDA8-5E2D-4A56-A3E2-CE3FE288F590}']
    function code: INResumeWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INResumeWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINResumeWorkoutIntentResponse = class(TOCGenericImport<INResumeWorkoutIntentResponseClass, INResumeWorkoutIntentResponse>) end;

  INStartWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{3823C576-A2FC-4DFB-8DD0-CD78D33B9972}']
  end;

  INStartWorkoutIntentResponse = interface(INIntentResponse)
    ['{7EB7451E-7D40-4FA9-A773-CC96D5D084A7}']
    function code: INStartWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INStartWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINStartWorkoutIntentResponse = class(TOCGenericImport<INStartWorkoutIntentResponseClass, INStartWorkoutIntentResponse>) end;

  INAddMediaIntentResponseClass = interface(INIntentResponseClass)
    ['{E75B5835-0A62-4B0A-AEE4-727E4904A679}']
  end;

  INAddMediaIntentResponse = interface(INIntentResponse)
    ['{FB0C2D2C-9967-462D-B219-F6B5AEC79B07}']
    function code: INAddMediaIntentResponseCode; cdecl;
    function initWithCode(code: INAddMediaIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINAddMediaIntentResponse = class(TOCGenericImport<INAddMediaIntentResponseClass, INAddMediaIntentResponse>) end;

  INPlayMediaIntentResponseClass = interface(INIntentResponseClass)
    ['{A770403E-1B72-4A5A-8323-6CF187F09E5C}']
  end;

  INPlayMediaIntentResponse = interface(INIntentResponse)
    ['{72417F8D-0262-4E33-BDFE-5091FE99FBFE}']
    function code: INPlayMediaIntentResponseCode; cdecl;
    function initWithCode(code: INPlayMediaIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function nowPlayingInfo: NSDictionary; cdecl;
    procedure setNowPlayingInfo(nowPlayingInfo: NSDictionary); cdecl;
  end;
  TINPlayMediaIntentResponse = class(TOCGenericImport<INPlayMediaIntentResponseClass, INPlayMediaIntentResponse>) end;

  INSearchForMediaIntentResponseClass = interface(INIntentResponseClass)
    ['{88C5091C-B4BE-4766-B433-03A19398D7DD}']
  end;

  INSearchForMediaIntentResponse = interface(INIntentResponse)
    ['{A0010987-44D2-459E-9261-8FDA022DDB1F}']
    function code: INSearchForMediaIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForMediaIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function mediaItems: NSArray; cdecl;
    procedure setMediaItems(mediaItems: NSArray); cdecl;
  end;
  TINSearchForMediaIntentResponse = class(TOCGenericImport<INSearchForMediaIntentResponseClass, INSearchForMediaIntentResponse>) end;

  INUpdateMediaAffinityIntentResponseClass = interface(INIntentResponseClass)
    ['{0A88DDC2-8F50-47F8-825B-0A8D49FC424C}']
  end;

  INUpdateMediaAffinityIntentResponse = interface(INIntentResponse)
    ['{C6207D5C-8B84-46EB-9D3D-55892EEE1A5F}']
    function code: INUpdateMediaAffinityIntentResponseCode; cdecl;
    function initWithCode(code: INUpdateMediaAffinityIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINUpdateMediaAffinityIntentResponse = class(TOCGenericImport<INUpdateMediaAffinityIntentResponseClass, INUpdateMediaAffinityIntentResponse>) end;

  INSetRadioStationIntentResponseClass = interface(INIntentResponseClass)
    ['{DEF80991-F3EC-4985-8017-DD6A0D2A4D7A}']
  end;

  INSetRadioStationIntentResponse = interface(INIntentResponse)
    ['{C5ACC441-6341-46B2-9862-8CCE4EFE3901}']
    function code: INSetRadioStationIntentResponseCode; cdecl;
    function initWithCode(code: INSetRadioStationIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetRadioStationIntentResponse = class(TOCGenericImport<INSetRadioStationIntentResponseClass, INSetRadioStationIntentResponse>) end;

  INSearchForMessagesIntentResponseClass = interface(INIntentResponseClass)
    ['{A40525E2-DD56-406A-B317-4991F4B47303}']
  end;

  INSearchForMessagesIntentResponse = interface(INIntentResponse)
    ['{6AF295A2-28FE-482F-91A9-DB58A66795F5}']
    function code: INSearchForMessagesIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForMessagesIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function messages: NSArray; cdecl;
    procedure setMessages(messages: NSArray); cdecl;
  end;
  TINSearchForMessagesIntentResponse = class(TOCGenericImport<INSearchForMessagesIntentResponseClass, INSearchForMessagesIntentResponse>) end;

  INSendMessageIntentResponseClass = interface(INIntentResponseClass)
    ['{28146FF1-13FB-4CBC-B175-9DF6C1D4E41E}']
  end;

  INSendMessageIntentResponse = interface(INIntentResponse)
    ['{4EB849CA-5BC4-4F4E-B66D-99AA9984EBDB}']
    function code: INSendMessageIntentResponseCode; cdecl;
    function initWithCode(code: INSendMessageIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function sentMessage: INMessage; cdecl;
    procedure setSentMessage(sentMessage: INMessage); cdecl;
  end;
  TINSendMessageIntentResponse = class(TOCGenericImport<INSendMessageIntentResponseClass, INSendMessageIntentResponse>) end;

  INSetMessageAttributeIntentResponseClass = interface(INIntentResponseClass)
    ['{14FC2C01-1A89-48C2-82EA-16EC8EE77161}']
  end;

  INSetMessageAttributeIntentResponse = interface(INIntentResponse)
    ['{6B77DDBA-63FD-4E63-9836-5E840BEFCF32}']
    function code: INSetMessageAttributeIntentResponseCode; cdecl;
    function initWithCode(code: INSetMessageAttributeIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetMessageAttributeIntentResponse = class(TOCGenericImport<INSetMessageAttributeIntentResponseClass, INSetMessageAttributeIntentResponse>) end;

  INAddTasksIntentResponseClass = interface(INIntentResponseClass)
    ['{8F67C1BC-4EAA-418B-9C08-DB34D1D2A325}']
  end;

  INAddTasksIntentResponse = interface(INIntentResponse)
    ['{5A1BF125-7BE5-4A04-B9E7-7E2DF6BDAD5E}']
    function addedTasks: NSArray; cdecl;
    function code: INAddTasksIntentResponseCode; cdecl;
    function initWithCode(code: INAddTasksIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function modifiedTaskList: INTaskList; cdecl;
    procedure setAddedTasks(addedTasks: NSArray); cdecl;
    procedure setModifiedTaskList(modifiedTaskList: INTaskList); cdecl;
  end;
  TINAddTasksIntentResponse = class(TOCGenericImport<INAddTasksIntentResponseClass, INAddTasksIntentResponse>) end;

  INAppendToNoteIntentResponseClass = interface(INIntentResponseClass)
    ['{976C38B3-1A3B-4E29-A1E5-6CCDA4EDCE91}']
  end;

  INAppendToNoteIntentResponse = interface(INIntentResponse)
    ['{A6A09C1E-CF41-4F2B-9DD7-97D9DA9E558F}']
    function code: INAppendToNoteIntentResponseCode; cdecl;
    function initWithCode(code: INAppendToNoteIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function note: INNote; cdecl;
    procedure setNote(note: INNote); cdecl;
  end;
  TINAppendToNoteIntentResponse = class(TOCGenericImport<INAppendToNoteIntentResponseClass, INAppendToNoteIntentResponse>) end;

  INCreateNoteIntentResponseClass = interface(INIntentResponseClass)
    ['{B396CF92-5B51-4B77-9610-9A6269029F92}']
  end;

  INCreateNoteIntentResponse = interface(INIntentResponse)
    ['{A6F73783-7D76-43D4-BF62-B9DF56978035}']
    function code: INCreateNoteIntentResponseCode; cdecl;
    function createdNote: INNote; cdecl;
    function initWithCode(code: INCreateNoteIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCreatedNote(createdNote: INNote); cdecl;
  end;
  TINCreateNoteIntentResponse = class(TOCGenericImport<INCreateNoteIntentResponseClass, INCreateNoteIntentResponse>) end;

  INCreateTaskListIntentResponseClass = interface(INIntentResponseClass)
    ['{A92E7AFF-04E0-43CD-9D08-1122785127E0}']
  end;

  INCreateTaskListIntentResponse = interface(INIntentResponse)
    ['{A2C9B42B-B1E8-4CF7-982E-7767CBE056E9}']
    function code: INCreateTaskListIntentResponseCode; cdecl;
    function createdTaskList: INTaskList; cdecl;
    function initWithCode(code: INCreateTaskListIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCreatedTaskList(createdTaskList: INTaskList); cdecl;
  end;
  TINCreateTaskListIntentResponse = class(TOCGenericImport<INCreateTaskListIntentResponseClass, INCreateTaskListIntentResponse>) end;

  INDeleteTasksIntentResponseClass = interface(INIntentResponseClass)
    ['{06A13EA2-5CC0-4BC7-8651-B779A767ADB1}']
  end;

  INDeleteTasksIntentResponse = interface(INIntentResponse)
    ['{C125967A-56A3-4018-97D3-C5C1088F52F6}']
    function code: INDeleteTasksIntentResponseCode; cdecl;
    function deletedTasks: NSArray; cdecl;
    function initWithCode(code: INDeleteTasksIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setDeletedTasks(deletedTasks: NSArray); cdecl;
  end;
  TINDeleteTasksIntentResponse = class(TOCGenericImport<INDeleteTasksIntentResponseClass, INDeleteTasksIntentResponse>) end;

  INSearchForNotebookItemsIntentResponseClass = interface(INIntentResponseClass)
    ['{FC044015-74EA-4945-9272-2E3BC32AE00F}']
  end;

  INSearchForNotebookItemsIntentResponse = interface(INIntentResponse)
    ['{24D75C9C-89EA-4AE4-A979-98C25BA5FE49}']
    function code: INSearchForNotebookItemsIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForNotebookItemsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function notes: NSArray; cdecl;
    procedure setNotes(notes: NSArray); cdecl;
    procedure setSortType(sortType: INSortType); cdecl;
    procedure setTaskLists(taskLists: NSArray); cdecl;
    procedure setTasks(tasks: NSArray); cdecl;
    function sortType: INSortType; cdecl;
    function taskLists: NSArray; cdecl;
    function tasks: NSArray; cdecl;
  end;
  TINSearchForNotebookItemsIntentResponse = class(TOCGenericImport<INSearchForNotebookItemsIntentResponseClass,
    INSearchForNotebookItemsIntentResponse>) end;

  INSetTaskAttributeIntentResponseClass = interface(INIntentResponseClass)
    ['{6FCBAAEA-E5E4-4C89-9E67-C25271032044}']
  end;

  INSetTaskAttributeIntentResponse = interface(INIntentResponse)
    ['{B1D78A23-72A6-489B-A70A-441BCE4B0FF7}']
    function code: INSetTaskAttributeIntentResponseCode; cdecl;
    function initWithCode(code: INSetTaskAttributeIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function modifiedTask: INTask; cdecl;
    procedure setModifiedTask(modifiedTask: INTask); cdecl;
  end;
  TINSetTaskAttributeIntentResponse = class(TOCGenericImport<INSetTaskAttributeIntentResponseClass, INSetTaskAttributeIntentResponse>) end;

  INSnoozeTasksIntentResponseClass = interface(INIntentResponseClass)
    ['{81309FE7-9C41-4707-B7C1-341BA71B42D4}']
  end;

  INSnoozeTasksIntentResponse = interface(INIntentResponse)
    ['{E77C89F2-8162-409A-92E0-09159A6D4298}']
    function code: INSnoozeTasksIntentResponseCode; cdecl;
    function initWithCode(code: INSnoozeTasksIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setSnoozedTasks(snoozedTasks: NSArray); cdecl;
    function snoozedTasks: NSArray; cdecl;
  end;
  TINSnoozeTasksIntentResponse = class(TOCGenericImport<INSnoozeTasksIntentResponseClass, INSnoozeTasksIntentResponse>) end;

  INPayBillIntentResponseClass = interface(INIntentResponseClass)
    ['{48CF0B7F-8095-4C6A-81FE-38A3E9A2F635}']
  end;

  INPayBillIntentResponse = interface(INIntentResponse)
    ['{5150039F-1A95-441C-B067-E92AC657237F}']
    function billDetails: INBillDetails; cdecl;
    function code: INPayBillIntentResponseCode; cdecl;
    function fromAccount: INPaymentAccount; cdecl;
    function initWithCode(code: INPayBillIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setBillDetails(billDetails: INBillDetails); cdecl;
    procedure setFromAccount(fromAccount: INPaymentAccount); cdecl;
    procedure setTransactionAmount(transactionAmount: INPaymentAmount); cdecl;
    procedure setTransactionNote(transactionNote: NSString); cdecl;
    procedure setTransactionScheduledDate(transactionScheduledDate: INDateComponentsRange); cdecl;
    function transactionAmount: INPaymentAmount; cdecl;
    function transactionNote: NSString; cdecl;
    function transactionScheduledDate: INDateComponentsRange; cdecl;
  end;
  TINPayBillIntentResponse = class(TOCGenericImport<INPayBillIntentResponseClass, INPayBillIntentResponse>) end;

  INRequestPaymentIntentResponseClass = interface(INIntentResponseClass)
    ['{454A7440-09E9-46D3-85D7-63041D14D206}']
  end;

  INRequestPaymentIntentResponse = interface(INIntentResponse)
    ['{28BA0C27-E3A3-4D62-AB47-C52B6FDEC9CF}']
    function code: INRequestPaymentIntentResponseCode; cdecl;
    function initWithCode(code: INRequestPaymentIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function paymentRecord: INPaymentRecord; cdecl;
    procedure setPaymentRecord(paymentRecord: INPaymentRecord); cdecl;
  end;
  TINRequestPaymentIntentResponse = class(TOCGenericImport<INRequestPaymentIntentResponseClass, INRequestPaymentIntentResponse>) end;

  INSearchForAccountsIntentResponseClass = interface(INIntentResponseClass)
    ['{055C4E60-C776-4685-9D4D-CE4FAF0253CA}']
  end;

  INSearchForAccountsIntentResponse = interface(INIntentResponse)
    ['{B72D5270-A745-4489-9EFC-3A76EC300F8D}']
    function accounts: NSArray; cdecl;
    function code: INSearchForAccountsIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForAccountsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setAccounts(accounts: NSArray); cdecl;
  end;
  TINSearchForAccountsIntentResponse = class(TOCGenericImport<INSearchForAccountsIntentResponseClass, INSearchForAccountsIntentResponse>) end;

  INSearchForBillsIntentResponseClass = interface(INIntentResponseClass)
    ['{5DA0B992-CF4E-482A-8572-24C6655B8501}']
  end;

  INSearchForBillsIntentResponse = interface(INIntentResponse)
    ['{20B5B083-C687-4953-AE92-A9744E4052B2}']
    function bills: NSArray; cdecl;
    function code: INSearchForBillsIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForBillsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setBills(bills: NSArray); cdecl;
  end;
  TINSearchForBillsIntentResponse = class(TOCGenericImport<INSearchForBillsIntentResponseClass, INSearchForBillsIntentResponse>) end;

  INSendPaymentIntentResponseClass = interface(INIntentResponseClass)
    ['{7CD00CB7-F693-4707-B2D8-E87C143FD103}']
  end;

  INSendPaymentIntentResponse = interface(INIntentResponse)
    ['{78452436-DD2C-4B90-A317-B115DE987B0C}']
    function code: INSendPaymentIntentResponseCode; cdecl;
    function initWithCode(code: INSendPaymentIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function paymentRecord: INPaymentRecord; cdecl;
    procedure setPaymentRecord(paymentRecord: INPaymentRecord); cdecl;
  end;
  TINSendPaymentIntentResponse = class(TOCGenericImport<INSendPaymentIntentResponseClass, INSendPaymentIntentResponse>) end;

  INTransferMoneyIntentResponseClass = interface(INIntentResponseClass)
    ['{CAFCC194-5AF6-4C46-AA51-B8B8D584EED0}']
  end;

  INTransferMoneyIntentResponse = interface(INIntentResponse)
    ['{4247275D-6FEE-4A62-BA20-C3D1127A9A69}']
    function code: INTransferMoneyIntentResponseCode; cdecl;
    function fromAccount: INPaymentAccount; cdecl;
    function initWithCode(code: INTransferMoneyIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setFromAccount(fromAccount: INPaymentAccount); cdecl;
    procedure setToAccount(toAccount: INPaymentAccount); cdecl;
    procedure setTransactionAmount(transactionAmount: INPaymentAmount); cdecl;
    procedure setTransactionNote(transactionNote: NSString); cdecl;
    procedure setTransactionScheduledDate(transactionScheduledDate: INDateComponentsRange); cdecl;
    procedure setTransferFee(transferFee: INCurrencyAmount); cdecl;
    function toAccount: INPaymentAccount; cdecl;
    function transactionAmount: INPaymentAmount; cdecl;
    function transactionNote: NSString; cdecl;
    function transactionScheduledDate: INDateComponentsRange; cdecl;
    function transferFee: INCurrencyAmount; cdecl;
  end;
  TINTransferMoneyIntentResponse = class(TOCGenericImport<INTransferMoneyIntentResponseClass, INTransferMoneyIntentResponse>) end;

  INSearchForPhotosIntentResponseClass = interface(INIntentResponseClass)
    ['{6363024A-3CEE-48ED-B9ED-7AE4C9DB95B2}']
  end;

  INSearchForPhotosIntentResponse = interface(INIntentResponse)
    ['{D31857FB-38D7-4251-9224-21F9AB13320E}']
    function code: INSearchForPhotosIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForPhotosIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function searchResultsCount: NSNumber; cdecl;
    procedure setSearchResultsCount(searchResultsCount: NSNumber); cdecl;
  end;
  TINSearchForPhotosIntentResponse = class(TOCGenericImport<INSearchForPhotosIntentResponseClass, INSearchForPhotosIntentResponse>) end;

  INStartPhotoPlaybackIntentResponseClass = interface(INIntentResponseClass)
    ['{81C75704-D438-4D40-80DE-8454CE713253}']
  end;

  INStartPhotoPlaybackIntentResponse = interface(INIntentResponse)
    ['{A3D0FC8E-E935-47C5-AE39-FF8813E50DCB}']
    function code: INStartPhotoPlaybackIntentResponseCode; cdecl;
    function initWithCode(code: INStartPhotoPlaybackIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function searchResultsCount: NSNumber; cdecl;
    procedure setSearchResultsCount(searchResultsCount: NSNumber); cdecl;
  end;
  TINStartPhotoPlaybackIntentResponse = class(TOCGenericImport<INStartPhotoPlaybackIntentResponseClass, INStartPhotoPlaybackIntentResponse>) end;

  INGetReservationDetailsIntentResponseClass = interface(INIntentResponseClass)
    ['{4FC2D3E5-C9E6-4093-AF6E-969806EBFD19}']
  end;

  INGetReservationDetailsIntentResponse = interface(INIntentResponse)
    ['{08B6E178-6925-4CE6-ADAB-2959C3D5892A}']
    function code: INGetReservationDetailsIntentResponseCode; cdecl;
    function initWithCode(code: INGetReservationDetailsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function reservations: NSArray; cdecl;
    procedure setReservations(reservations: NSArray); cdecl;
  end;
  TINGetReservationDetailsIntentResponse = class(TOCGenericImport<INGetReservationDetailsIntentResponseClass,
    INGetReservationDetailsIntentResponse>) end;

  INGetRideStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{BB3483AB-57A2-452B-9553-5039FE70D4F4}']
  end;

  INGetRideStatusIntentResponse = interface(INIntentResponse)
    ['{D2D2AEC2-6097-48AD-839C-AAFC99055513}']
    function code: INGetRideStatusIntentResponseCode; cdecl;
    function initWithCode(code: INGetRideStatusIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function rideStatus: INRideStatus; cdecl;
    procedure setRideStatus(rideStatus: INRideStatus); cdecl;
  end;
  TINGetRideStatusIntentResponse = class(TOCGenericImport<INGetRideStatusIntentResponseClass, INGetRideStatusIntentResponse>) end;

  INListRideOptionsIntentResponseClass = interface(INIntentResponseClass)
    ['{DCBF2AD3-706A-4A4D-B257-D73A971BCC62}']
  end;

  INListRideOptionsIntentResponse = interface(INIntentResponse)
    ['{95103387-8C66-48ED-B374-A0282C32F12F}']
    function code: INListRideOptionsIntentResponseCode; cdecl;
    function expirationDate: NSDate; cdecl;
    function initWithCode(code: INListRideOptionsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function paymentMethods: NSArray; cdecl;
    function rideOptions: NSArray; cdecl;
    procedure setExpirationDate(expirationDate: NSDate); cdecl;
    procedure setPaymentMethods(paymentMethods: NSArray); cdecl;
    procedure setRideOptions(rideOptions: NSArray); cdecl;
  end;
  TINListRideOptionsIntentResponse = class(TOCGenericImport<INListRideOptionsIntentResponseClass, INListRideOptionsIntentResponse>) end;

  INRequestRideIntentResponseClass = interface(INIntentResponseClass)
    ['{92BD196C-DA7D-4A52-8F19-D4877C3B4675}']
  end;

  INRequestRideIntentResponse = interface(INIntentResponse)
    ['{A024B02D-55B4-4D7C-AC0B-A2D26F40413B}']
    function code: INRequestRideIntentResponseCode; cdecl;
    function initWithCode(code: INRequestRideIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function rideStatus: INRideStatus; cdecl;
    procedure setRideStatus(rideStatus: INRideStatus); cdecl;
  end;
  TINRequestRideIntentResponse = class(TOCGenericImport<INRequestRideIntentResponseClass, INRequestRideIntentResponse>) end;

  INCancelRideIntentResponseClass = interface(INIntentResponseClass)
    ['{CEF4FCDD-B772-4A66-8494-2EC0EB3D14AB}']
  end;

  INCancelRideIntentResponse = interface(INIntentResponse)
    ['{DB06E1BE-7A47-4F9F-B95D-62D989C0B1D2}']
    function cancellationFee: INCurrencyAmount; cdecl;
    function cancellationFeeThreshold: NSDateComponents; cdecl;
    function code: INCancelRideIntentResponseCode; cdecl;
    function initWithCode(code: INCancelRideIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCancellationFee(cancellationFee: INCurrencyAmount); cdecl;
    procedure setCancellationFeeThreshold(cancellationFeeThreshold: NSDateComponents); cdecl;
  end;
  TINCancelRideIntentResponse = class(TOCGenericImport<INCancelRideIntentResponseClass, INCancelRideIntentResponse>) end;

  INSendRideFeedbackIntentResponseClass = interface(INIntentResponseClass)
    ['{B877752D-80B3-4AA6-B450-5903A386C3E6}']
  end;

  INSendRideFeedbackIntentResponse = interface(INIntentResponse)
    ['{3120260F-8C42-41CF-B422-3C16602AC44E}']
    function code: INSendRideFeedbackIntentResponseCode; cdecl;
    function initWithCode(code: INSendRideFeedbackIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSendRideFeedbackIntentResponse = class(TOCGenericImport<INSendRideFeedbackIntentResponseClass, INSendRideFeedbackIntentResponse>) end;

  INGetVisualCodeIntentResponseClass = interface(INIntentResponseClass)
    ['{46A62955-E30D-4248-84C2-455FE0B7373E}']
  end;

  INGetVisualCodeIntentResponse = interface(INIntentResponse)
    ['{6E8F8A39-F679-477B-878D-FB9DE6EAC057}']
    function code: INGetVisualCodeIntentResponseCode; cdecl;
    function initWithCode(code: INGetVisualCodeIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setVisualCodeImage(visualCodeImage: INImage); cdecl;
    function visualCodeImage: INImage; cdecl;
  end;
  TINGetVisualCodeIntentResponse = class(TOCGenericImport<INGetVisualCodeIntentResponseClass, INGetVisualCodeIntentResponse>) end;

  INAccountTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{17D710F5-2C16-4B4B-8B18-824EB20A25F1}']
    {class} function confirmationRequiredWithAccountTypeToConfirm(accountTypeToConfirm: INAccountType): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INAccountType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithAccountTypeToConfirm:", ios(11.0, 11.0), watchos(4.0, 4.0))
    {class} function successWithResolvedAccountType(resolvedAccountType: INAccountType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INAccountType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedAccountType:", ios(11.0, 11.0), watchos(4.0, 4.0))
  end;

  INAccountTypeResolutionResult = interface(INIntentResolutionResult)
    ['{0AD20AAC-A441-46EC-87DE-0AA3696CC051}']
  end;
  TINAccountTypeResolutionResult = class(TOCGenericImport<INAccountTypeResolutionResultClass, INAccountTypeResolutionResult>) end;

  INMediaDestinationResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{ED7E0CAC-2139-4F94-A49B-5DCE5EB37131}']
    {class} function confirmationRequiredWithMediaDestinationToConfirm(mediaDestinationToConfirm: INMediaDestination): Pointer; cdecl;
    {class} function disambiguationWithMediaDestinationsToDisambiguate(mediaDestinationsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedMediaDestination(resolvedMediaDestination: INMediaDestination): Pointer; cdecl;
  end;

  INMediaDestinationResolutionResult = interface(INIntentResolutionResult)
    ['{CA43DC36-13DC-414B-B348-0E1F0135021E}']
  end;
  TINMediaDestinationResolutionResult = class(TOCGenericImport<INMediaDestinationResolutionResultClass, INMediaDestinationResolutionResult>) end;

  INAddMediaMediaDestinationResolutionResultClass = interface(INMediaDestinationResolutionResultClass)
    ['{21C03C5B-A2EB-4887-9803-665025685060}']
    {class} function unsupportedForReason(reason: INAddMediaMediaDestinationUnsupportedReason): Pointer; cdecl;
  end;

  INAddMediaMediaDestinationResolutionResult = interface(INMediaDestinationResolutionResult)
    ['{31E094B7-B935-428C-BD45-300E149BAF98}']
    function initWithMediaDestinationResolutionResult(mediaDestinationResolutionResult: INMediaDestinationResolutionResult): Pointer; cdecl;
  end;
  TINAddMediaMediaDestinationResolutionResult = class(TOCGenericImport<INAddMediaMediaDestinationResolutionResultClass,
    INAddMediaMediaDestinationResolutionResult>) end;

  INMediaItemResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{906F9D7F-A73B-4344-88E0-61E6CCBD6B62}']
    {class} function confirmationRequiredWithMediaItemToConfirm(mediaItemToConfirm: INMediaItem): Pointer; cdecl;
    {class} function disambiguationWithMediaItemsToDisambiguate(mediaItemsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function successWithResolvedMediaItem(resolvedMediaItem: INMediaItem): Pointer; cdecl;
  end;

  INMediaItemResolutionResult = interface(INIntentResolutionResult)
    ['{ED7E7590-56FB-4965-9AB4-BF21CA9B52D3}']
  end;
  TINMediaItemResolutionResult = class(TOCGenericImport<INMediaItemResolutionResultClass, INMediaItemResolutionResult>) end;

  INAddMediaMediaItemResolutionResultClass = interface(INMediaItemResolutionResultClass)
    ['{4DDF847F-0A20-43F9-A7DF-520B9562E930}']
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function unsupportedForReason(reason: INAddMediaMediaItemUnsupportedReason): Pointer; cdecl;
  end;

  INAddMediaMediaItemResolutionResult = interface(INMediaItemResolutionResult)
    ['{AB6F0279-5033-441C-8589-4BA8D0638F8A}']
    function initWithMediaItemResolutionResult(mediaItemResolutionResult: INMediaItemResolutionResult): Pointer; cdecl;
  end;
  TINAddMediaMediaItemResolutionResult = class(TOCGenericImport<INAddMediaMediaItemResolutionResultClass, INAddMediaMediaItemResolutionResult>) end;

  INTaskListResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{2DC0032D-D65B-4EC3-B624-0F34FD6259A5}']
    {class} function confirmationRequiredWithTaskListToConfirm(taskListToConfirm: INTaskList): Pointer; cdecl;
    {class} function disambiguationWithTaskListsToDisambiguate(taskListsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedTaskList(resolvedTaskList: INTaskList): Pointer; cdecl;
  end;

  INTaskListResolutionResult = interface(INIntentResolutionResult)
    ['{D8D183AB-DBE9-4B03-B865-585889EE4363}']
  end;
  TINTaskListResolutionResult = class(TOCGenericImport<INTaskListResolutionResultClass, INTaskListResolutionResult>) end;

  INAddTasksTargetTaskListResolutionResultClass = interface(INTaskListResolutionResultClass)
    ['{960B3829-AC23-48A3-B5CC-4358996D0413}']
    {class} function confirmationRequiredWithTaskListToConfirm(taskListToConfirm: INTaskList;
      forReason: INAddTasksTargetTaskListConfirmationReason): Pointer; cdecl;
  end;

  INAddTasksTargetTaskListResolutionResult = interface(INTaskListResolutionResult)
    ['{0F42ACDB-877B-4774-8F4E-D0B6EEF08A70}']
    function initWithTaskListResolutionResult(taskListResolutionResult: INTaskListResolutionResult): Pointer; cdecl;
  end;
  TINAddTasksTargetTaskListResolutionResult = class(TOCGenericImport<INAddTasksTargetTaskListResolutionResultClass,
    INAddTasksTargetTaskListResolutionResult>) end;

  INTemporalEventTriggerResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{14C08DA2-2289-49AC-8069-415EF91CC201}']
    {class} function confirmationRequiredWithTemporalEventTriggerToConfirm(temporalEventTriggerToConfirm: INTemporalEventTrigger): Pointer; cdecl;
    {class} function disambiguationWithTemporalEventTriggersToDisambiguate(temporalEventTriggersToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedTemporalEventTrigger(resolvedTemporalEventTrigger: INTemporalEventTrigger): Pointer; cdecl;
  end;

  INTemporalEventTriggerResolutionResult = interface(INIntentResolutionResult)
    ['{A7B3D2E0-22F1-4421-BBE4-62A6A68F9D38}']
  end;
  TINTemporalEventTriggerResolutionResult = class(TOCGenericImport<INTemporalEventTriggerResolutionResultClass,
    INTemporalEventTriggerResolutionResult>) end;

  INAddTasksTemporalEventTriggerResolutionResultClass = interface(INTemporalEventTriggerResolutionResultClass)
    ['{88440081-6482-4333-BD7A-4262AE739472}']
    {class} function unsupportedForReason(reason: INAddTasksTemporalEventTriggerUnsupportedReason): Pointer; cdecl;
  end;

  INAddTasksTemporalEventTriggerResolutionResult = interface(INTemporalEventTriggerResolutionResult)
    ['{63C02BE4-3F6F-4A8C-A3F4-59C1096A45B2}']
    function initWithTemporalEventTriggerResolutionResult(temporalEventTriggerResolutionResult: INTemporalEventTriggerResolutionResult): Pointer; cdecl;
  end;
  TINAddTasksTemporalEventTriggerResolutionResult = class(TOCGenericImport<INAddTasksTemporalEventTriggerResolutionResultClass,
    INAddTasksTemporalEventTriggerResolutionResult>) end;

  INAirlineClass = interface(NSObjectClass)
    ['{5D5BEB41-A98D-4477-AE5F-B817A1A58308}']
  end;

  INAirline = interface(NSObject)
    ['{FBADDA3B-FCB0-4D84-809D-AF151B551BBE}']
    function iataCode: NSString; cdecl;
    function icaoCode: NSString; cdecl;
    function initWithName(name: NSString; iataCode: NSString; icaoCode: NSString): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TINAirline = class(TOCGenericImport<INAirlineClass, INAirline>) end;

  INAirportClass = interface(NSObjectClass)
    ['{C0250A55-7E9C-425B-A70D-20FCEA5C9B54}']
  end;

  INAirport = interface(NSObject)
    ['{1E5B8D2C-C06E-4225-B9C1-B60459475400}']
    function iataCode: NSString; cdecl;
    function icaoCode: NSString; cdecl;
    function initWithName(name: NSString; iataCode: NSString; icaoCode: NSString): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TINAirport = class(TOCGenericImport<INAirportClass, INAirport>) end;

  INAirportGateClass = interface(NSObjectClass)
    ['{2FDB2D3F-9C6F-48C6-9C18-8C065143D38E}']
  end;

  INAirportGate = interface(NSObject)
    ['{9CB4FBAE-6B56-41A5-BC92-B475AF0632D6}']
    function airport: INAirport; cdecl;
    function gate: NSString; cdecl;
    function initWithAirport(airport: INAirport; terminal: NSString; gate: NSString): Pointer; cdecl;
    function terminal: NSString; cdecl;
  end;
  TINAirportGate = class(TOCGenericImport<INAirportGateClass, INAirportGate>) end;

  INBalanceTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{9EBD428E-024C-4323-A391-4EAE36E1053E}']
    {class} function confirmationRequiredWithBalanceTypeToConfirm(balanceTypeToConfirm: INBalanceType): Pointer; cdecl;
    {class} function successWithResolvedBalanceType(resolvedBalanceType: INBalanceType): Pointer; cdecl;
  end;

  INBalanceTypeResolutionResult = interface(INIntentResolutionResult)
    ['{070BDD61-59FD-47F8-A153-4AA92F9BBA2A}']
  end;
  TINBalanceTypeResolutionResult = class(TOCGenericImport<INBalanceTypeResolutionResultClass, INBalanceTypeResolutionResult>) end;

  INBillDetailsClass = interface(NSObjectClass)
    ['{1D604EA8-3CE9-46BB-85D4-1C594A852DBD}']
  end;

  INBillDetails = interface(NSObject)
    ['{AA615821-FCC8-4FC2-90B2-A12866C3DE80}']
    function amountDue: INCurrencyAmount; cdecl;
    function billPayee: INBillPayee; cdecl;
    function billType: INBillType; cdecl;
    function dueDate: NSDateComponents; cdecl;
    function initWithBillType(billType: INBillType; paymentStatus: INPaymentStatus; billPayee: INBillPayee; amountDue: INCurrencyAmount;
      minimumDue: INCurrencyAmount; lateFee: INCurrencyAmount; dueDate: NSDateComponents; paymentDate: NSDateComponents): Pointer; cdecl;
    function lateFee: INCurrencyAmount; cdecl;
    function minimumDue: INCurrencyAmount; cdecl;
    function paymentDate: NSDateComponents; cdecl;
    function paymentStatus: INPaymentStatus; cdecl;
    procedure setAmountDue(amountDue: INCurrencyAmount); cdecl;
    procedure setBillPayee(billPayee: INBillPayee); cdecl;
    procedure setBillType(billType: INBillType); cdecl;
    procedure setDueDate(dueDate: NSDateComponents); cdecl;
    procedure setLateFee(lateFee: INCurrencyAmount); cdecl;
    procedure setMinimumDue(minimumDue: INCurrencyAmount); cdecl;
    procedure setPaymentDate(paymentDate: NSDateComponents); cdecl;
    procedure setPaymentStatus(paymentStatus: INPaymentStatus); cdecl;
  end;
  TINBillDetails = class(TOCGenericImport<INBillDetailsClass, INBillDetails>) end;

  INBillPayeeClass = interface(NSObjectClass)
    ['{12827868-67CA-4380-9774-CECF5B85CE21}']
  end;

  INBillPayee = interface(NSObject)
    ['{25A72D7E-F7C3-42D4-BBCF-66747A8311AC}']
    function accountNumber: NSString; cdecl;
    function initWithNickname(nickname: INSpeakableString; number: NSString; organizationName: INSpeakableString): Pointer; cdecl;
    function nickname: INSpeakableString; cdecl;
    function organizationName: INSpeakableString; cdecl;
  end;
  TINBillPayee = class(TOCGenericImport<INBillPayeeClass, INBillPayee>) end;

  INBillPayeeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{8398350C-8CCF-488D-90ED-3F411A078C81}']
    {class} function confirmationRequiredWithBillPayeeToConfirm(billPayeeToConfirm: INBillPayee): Pointer; cdecl;
    {class} function disambiguationWithBillPayeesToDisambiguate(billPayeesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedBillPayee(resolvedBillPayee: INBillPayee): Pointer; cdecl;
  end;

  INBillPayeeResolutionResult = interface(INIntentResolutionResult)
    ['{54F4D43A-EF2D-416C-ADC2-A0EA7E64F34D}']
  end;
  TINBillPayeeResolutionResult = class(TOCGenericImport<INBillPayeeResolutionResultClass, INBillPayeeResolutionResult>) end;

  INBillTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{853E3A13-B786-4C95-A808-A0969D2A1FFE}']
    {class} function confirmationRequiredWithBillTypeToConfirm(billTypeToConfirm: INBillType): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INBillType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithBillTypeToConfirm:", ios(10.3, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedBillType(resolvedBillType: INBillType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INBillType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedBillType:", ios(10.3, 11.0), watchos(3.2, 4.0))
  end;

  INBillTypeResolutionResult = interface(INIntentResolutionResult)
    ['{A8ADF85A-65A2-40D4-B676-B0487C12AF9D}']
  end;
  TINBillTypeResolutionResult = class(TOCGenericImport<INBillTypeResolutionResultClass, INBillTypeResolutionResult>) end;

  INBoatTripClass = interface(NSObjectClass)
    ['{B4D0D4E6-C7B1-495D-844D-FA431DF9D660}']
  end;

  INBoatTrip = interface(NSObject)
    ['{9808ACF7-16D0-487B-9886-34D8808FA9E4}']
    function arrivalBoatTerminalLocation: CLPlacemark; cdecl;
    function boatName: NSString; cdecl;
    function boatNumber: NSString; cdecl;
    function departureBoatTerminalLocation: CLPlacemark; cdecl;
    function initWithProvider(provider: NSString; boatName: NSString; boatNumber: NSString; tripDuration: INDateComponentsRange;
      departureBoatTerminalLocation: CLPlacemark; arrivalBoatTerminalLocation: CLPlacemark): Pointer; cdecl;
    function provider: NSString; cdecl;
    function tripDuration: INDateComponentsRange; cdecl;
  end;
  TINBoatTrip = class(TOCGenericImport<INBoatTripClass, INBoatTrip>) end;

  INBusTripClass = interface(NSObjectClass)
    ['{FBB3A3D0-5833-440B-A11D-A6933267CD69}']
  end;

  INBusTrip = interface(NSObject)
    ['{9862E7B2-AB23-4A48-BD92-36320FB26632}']
    function arrivalBusStopLocation: CLPlacemark; cdecl;
    function arrivalPlatform: NSString; cdecl;
    function busName: NSString; cdecl;
    function busNumber: NSString; cdecl;
    function departureBusStopLocation: CLPlacemark; cdecl;
    function departurePlatform: NSString; cdecl;
    function initWithProvider(provider: NSString; busName: NSString; busNumber: NSString; tripDuration: INDateComponentsRange;
      departureBusStopLocation: CLPlacemark; departurePlatform: NSString; arrivalBusStopLocation: CLPlacemark;
      arrivalPlatform: NSString): Pointer; cdecl;
    function provider: NSString; cdecl;
    function tripDuration: INDateComponentsRange; cdecl;
  end;
  TINBusTrip = class(TOCGenericImport<INBusTripClass, INBusTrip>) end;

  INCallCapabilityResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{BA099CA2-C751-4580-9DEE-51CD4D5D0463}']
    {class} function confirmationRequiredWithCallCapabilityToConfirm(callCapabilityToConfirm: INCallCapability): Pointer; cdecl;
    {class} function successWithResolvedCallCapability(resolvedCallCapability: INCallCapability): Pointer; cdecl;
  end;

  INCallCapabilityResolutionResult = interface(INIntentResolutionResult)
    ['{F4E4EC2F-6FA2-44E6-9BB6-372E5DDF748B}']
  end;
  TINCallCapabilityResolutionResult = class(TOCGenericImport<INCallCapabilityResolutionResultClass, INCallCapabilityResolutionResult>) end;

  INCallDestinationTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{BA52258F-5D0E-433E-AA1C-8C875113CEDD}']
    {class} function confirmationRequiredWithCallDestinationTypeToConfirm(callDestinationTypeToConfirm: INCallDestinationType): Pointer; cdecl;
    {class} function successWithResolvedCallDestinationType(resolvedCallDestinationType: INCallDestinationType): Pointer; cdecl;
  end;

  INCallDestinationTypeResolutionResult = interface(INIntentResolutionResult)
    ['{C5563402-82BB-49B7-886C-B98DA945CB46}']
  end;
  TINCallDestinationTypeResolutionResult = class(TOCGenericImport<INCallDestinationTypeResolutionResultClass,
    INCallDestinationTypeResolutionResult>) end;

  INCallGroupClass = interface(NSObjectClass)
    ['{55BAEB7A-B05B-4153-A6FB-0BBB84CEC109}']
  end;

  INCallGroup = interface(NSObject)
    ['{C147FF5F-4839-4C60-955D-35A82850D568}']
    function groupId: NSString; cdecl;
    function groupName: NSString; cdecl;
    function initWithGroupName(groupName: NSString; groupId: NSString): Pointer; cdecl;
  end;
  TINCallGroup = class(TOCGenericImport<INCallGroupClass, INCallGroup>) end;

  INCallRecordClass = interface(NSObjectClass)
    ['{2E9D1164-5EFE-44C6-9BA0-CCAB13EE07E0}']
  end;

  INCallRecord = interface(NSObject)
    ['{63CC8CAF-086D-4152-AA27-35D71D449384}']
    function callCapability: INCallCapability; cdecl;
    function callDuration: NSNumber; cdecl;
    function caller: INPerson; cdecl; // API_DEPRECATED("", ios(11.0, 14.5), watchos(4.0, 7.3))
    function callRecordType: INCallRecordType; cdecl;
    function dateCreated: NSDate; cdecl;
    function identifier: NSString; cdecl;
    function initWithIdentifier(identifier: NSString; dateCreated: NSDate; caller: INPerson; callRecordType: INCallRecordType;
      callCapability: INCallCapability; callDuration: NSNumber; unseen: NSNumber; numberOfCalls: NSNumber): Pointer; overload; cdecl; // API_DEPRECATED("Replaced by -initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked", ios(13.0, 14.5), watchos(6.0, 7.3), macosx(10.15, 11.3))
    function initWithIdentifier(identifier: NSString; dateCreated: NSDate; caller: INPerson; callRecordType: INCallRecordType;
      callCapability: INCallCapability; callDuration: NSNumber; unseen: NSNumber): Pointer; overload; cdecl; // API_DEPRECATED("Replaced by -initWithIdentifier:dateCreated:callRecordType:callCapability:callDuration:unseen:participants:numberOfCalls:isCallerIdBlocked", ios(11.0, 14.5), watchos(4.0, 7.3), macosx(10.13, 11.3))
    function initWithIdentifier(identifier: NSString; dateCreated: NSDate; callRecordType: INCallRecordType; callCapability: INCallCapability;
      callDuration: NSNumber; unseen: NSNumber; participants: NSArray; numberOfCalls: NSNumber; isCallerIdBlocked: NSNumber): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; dateCreated: NSDate; callRecordType: INCallRecordType; callCapability: INCallCapability;
      callDuration: NSNumber; unseen: NSNumber; numberOfCalls: NSNumber): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; dateCreated: NSDate; callRecordType: INCallRecordType; callCapability: INCallCapability;
      callDuration: NSNumber; unseen: NSNumber): Pointer; overload; cdecl;
    function isCallerIdBlocked: NSNumber; cdecl;
    function numberOfCalls: NSNumber; cdecl;
    function participants: NSArray; cdecl;
    function unseen: NSNumber; cdecl;
  end;
  TINCallRecord = class(TOCGenericImport<INCallRecordClass, INCallRecord>) end;

  INCallRecordFilterClass = interface(NSObjectClass)
    ['{90B39C5B-BFA0-4FF3-A9B6-23B1B563D1AB}']
  end;

  INCallRecordFilter = interface(NSObject)
    ['{5ABD4F47-062E-46E0-80C3-E908825C9787}']
    function callCapability: INCallCapability; cdecl;
    function callTypes: INCallRecordTypeOptions; cdecl;
    function initWithParticipants(participants: NSArray; callTypes: INCallRecordTypeOptions; callCapability: INCallCapability): Pointer; cdecl;
    function participants: NSArray; cdecl;
  end;
  TINCallRecordFilter = class(TOCGenericImport<INCallRecordFilterClass, INCallRecordFilter>) end;

  INCallRecordResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{5CAD6C30-6B39-4735-BCFC-41C957060CE8}']
    {class} function confirmationRequiredWithCallRecordToConfirm(callRecordToConfirm: INCallRecord): Pointer; cdecl;
    {class} function disambiguationWithCallRecordsToDisambiguate(callRecordsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedCallRecord(resolvedCallRecord: INCallRecord): Pointer; cdecl;
  end;

  INCallRecordResolutionResult = interface(INIntentResolutionResult)
    ['{F227A1B8-BAF4-41CB-B3E7-BA32C0046DF3}']
  end;
  TINCallRecordResolutionResult = class(TOCGenericImport<INCallRecordResolutionResultClass, INCallRecordResolutionResult>) end;

  INCallRecordTypeOptionsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{A5C5FF2B-45E4-4F29-805F-6AB2C0D98FEC}']
    {class} function confirmationRequiredWithCallRecordTypeOptionsToConfirm(callRecordTypeOptionsToConfirm: INCallRecordTypeOptions): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCallRecordTypeOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCallRecordTypeOptionsToConfirm:", ios(11.0, 11.0), watchos(4.0, 4.0))
    {class} function successWithResolvedCallRecordTypeOptions(resolvedCallRecordTypeOptions: INCallRecordTypeOptions): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCallRecordTypeOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCallRecordTypeOptions:", ios(11.0, 11.0), watchos(4.0, 4.0))
  end;

  INCallRecordTypeOptionsResolutionResult = interface(INIntentResolutionResult)
    ['{25CB5E9D-998B-4AEC-8138-27685DFD9BC1}']
  end;
  TINCallRecordTypeOptionsResolutionResult = class(TOCGenericImport<INCallRecordTypeOptionsResolutionResultClass,
    INCallRecordTypeOptionsResolutionResult>) end;

  INCallRecordTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{95AC5D9D-6510-4452-9D91-B2EDCCE73005}']
    {class} function confirmationRequiredWithCallRecordTypeToConfirm(callRecordTypeToConfirm: INCallRecordType): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCallRecordType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCallRecordTypeToConfirm:", ios(11.0, 11.0), watchos(4.0, 4.0))
    {class} function successWithResolvedCallRecordType(resolvedCallRecordType: INCallRecordType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCallRecordType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCallRecordType:", ios(11.0, 11.0), watchos(4.0, 4.0))
  end;

  INCallRecordTypeResolutionResult = interface(INIntentResolutionResult)
    ['{FF44B240-2C0C-4F3F-8AEF-B2543AF8784A}']
  end;
  TINCallRecordTypeResolutionResult = class(TOCGenericImport<INCallRecordTypeResolutionResultClass, INCallRecordTypeResolutionResult>) end;

  INCarClass = interface(NSObjectClass)
    ['{5BDD2FD9-C0F7-48EB-BCF4-23B1DA0DB823}']
  end;

  INCar = interface(NSObject)
    ['{00D1598D-ED14-4D6B-B287-E7A533F6C1BE}']
    function carIdentifier: NSString; cdecl;
    function color: CGColorRef; cdecl;
    function displayName: NSString; cdecl;
    function headUnit: INCarHeadUnit; cdecl;
    function initWithCarIdentifier(carIdentifier: NSString; displayName: NSString; year: NSString; make: NSString; model: NSString; color: CGColorRef;
      headUnit: INCarHeadUnit; supportedChargingConnectors: NSArray): Pointer; cdecl;
    function make: NSString; cdecl;
    function maximumPowerForChargingConnectorType(chargingConnectorType: INCarChargingConnectorType): NSMeasurement; cdecl;
    function model: NSString; cdecl;
    procedure setMaximumPower(power: NSMeasurement; forChargingConnectorType: INCarChargingConnectorType); cdecl;
    function supportedChargingConnectors: NSArray; cdecl;
    function year: NSString; cdecl;
  end;
  TINCar = class(TOCGenericImport<INCarClass, INCar>) end;

  INCarAirCirculationModeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{877994FB-421E-4EB7-9D8F-67083C3B5CDE}']
    {class} function confirmationRequiredWithCarAirCirculationModeToConfirm(carAirCirculationModeToConfirm: INCarAirCirculationMode): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarAirCirculationMode): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarAirCirculationModeToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedCarAirCirculationMode(resolvedCarAirCirculationMode: INCarAirCirculationMode): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarAirCirculationMode): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarAirCirculationMode:", ios(10.0, 11.0))
  end;

  INCarAirCirculationModeResolutionResult = interface(INIntentResolutionResult)
    ['{3DA014D1-9A0B-42BB-9755-F1D59E09BD98}']
  end;
  TINCarAirCirculationModeResolutionResult = class(TOCGenericImport<INCarAirCirculationModeResolutionResultClass,
    INCarAirCirculationModeResolutionResult>) end;

  INCarAudioSourceResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{4D9BEFB1-F2BF-4289-868F-A574CCA68D97}']
    {class} function confirmationRequiredWithCarAudioSourceToConfirm(carAudioSourceToConfirm: INCarAudioSource): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarAudioSource): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarAudioSourceToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedCarAudioSource(resolvedCarAudioSource: INCarAudioSource): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarAudioSource): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarAudioSource:", ios(10.0, 11.0))
  end;

  INCarAudioSourceResolutionResult = interface(INIntentResolutionResult)
    ['{CF4E22DA-25F1-4694-87EE-1C255F0480B0}']
  end;
  TINCarAudioSourceResolutionResult = class(TOCGenericImport<INCarAudioSourceResolutionResultClass, INCarAudioSourceResolutionResult>) end;

  INCarDefrosterResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{254AFE8E-AAAD-45FA-92AA-10E1BCD597AA}']
    {class} function confirmationRequiredWithCarDefrosterToConfirm(carDefrosterToConfirm: INCarDefroster): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarDefroster): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarDefrosterToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedCarDefroster(resolvedCarDefroster: INCarDefroster): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarDefroster): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarDefroster:", ios(10.0, 11.0))
  end;

  INCarDefrosterResolutionResult = interface(INIntentResolutionResult)
    ['{10C1356E-4A29-4665-B09B-0A842B43AC9F}']
  end;
  TINCarDefrosterResolutionResult = class(TOCGenericImport<INCarDefrosterResolutionResultClass, INCarDefrosterResolutionResult>) end;

  INCarHeadUnitClass = interface(NSObjectClass)
    ['{F54E37D1-A950-45F8-A24D-85168AB95570}']
  end;

  INCarHeadUnit = interface(NSObject)
    ['{C40A971C-B4E4-475F-9F8C-BB2E2858CE9C}']
    function bluetoothIdentifier: NSString; cdecl;
    function iAP2Identifier: NSString; cdecl;
    function initWithBluetoothIdentifier(bluetoothIdentifier: NSString; iAP2Identifier: NSString): Pointer; cdecl;
  end;
  TINCarHeadUnit = class(TOCGenericImport<INCarHeadUnitClass, INCarHeadUnit>) end;

  INCarSeatResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{0D08E6B5-93DB-4E3F-BF91-9B6CE6E07839}']
    {class} function confirmationRequiredWithCarSeatToConfirm(carSeatToConfirm: INCarSeat): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarSeat): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarSeatToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedCarSeat(resolvedCarSeat: INCarSeat): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarSeat): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarSeat:", ios(10.0, 11.0))
  end;

  INCarSeatResolutionResult = interface(INIntentResolutionResult)
    ['{249DC267-6C0A-4B8F-9384-0F15D5EF8541}']
  end;
  TINCarSeatResolutionResult = class(TOCGenericImport<INCarSeatResolutionResultClass, INCarSeatResolutionResult>) end;

  INCarSignalOptionsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{5BCC0F6F-7860-4C7B-9639-3FB6FE8008B8}']
    {class} function confirmationRequiredWithCarSignalOptionsToConfirm(carSignalOptionsToConfirm: INCarSignalOptions): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarSignalOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarSignalOptionsToConfirm:", ios(10.3, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedCarSignalOptions(resolvedCarSignalOptions: INCarSignalOptions): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarSignalOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarSignalOptions:", ios(10.3, 11.0), watchos(3.2, 4.0))
  end;

  INCarSignalOptionsResolutionResult = interface(INIntentResolutionResult)
    ['{18F7B33F-4FEC-41A7-961F-B6C3C24D301D}']
  end;
  TINCarSignalOptionsResolutionResult = class(TOCGenericImport<INCarSignalOptionsResolutionResultClass, INCarSignalOptionsResolutionResult>) end;

  INCurrencyAmountResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{2B09ECCE-844B-463E-8642-082EE869732F}']
    {class} function confirmationRequiredWithCurrencyAmountToConfirm(currencyAmountToConfirm: INCurrencyAmount): Pointer; cdecl;
    {class} function disambiguationWithCurrencyAmountsToDisambiguate(currencyAmountsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedCurrencyAmount(resolvedCurrencyAmount: INCurrencyAmount): Pointer; cdecl;
  end;

  INCurrencyAmountResolutionResult = interface(INIntentResolutionResult)
    ['{821204DB-FC7B-4E8C-9C65-F20138C42492}']
  end;
  TINCurrencyAmountResolutionResult = class(TOCGenericImport<INCurrencyAmountResolutionResultClass, INCurrencyAmountResolutionResult>) end;

  INDateSearchTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{49B327D3-3BB3-4DE3-8C73-76CC8AEE4846}']
    {class} function confirmationRequiredWithDateSearchTypeToConfirm(dateSearchTypeToConfirm: INDateSearchType): Pointer; cdecl;
    {class} function successWithResolvedDateSearchType(resolvedDateSearchType: INDateSearchType): Pointer; cdecl;
  end;

  INDateSearchTypeResolutionResult = interface(INIntentResolutionResult)
    ['{0C3F0CB1-7DC8-46A5-8BCD-AE20A3F0DEDA}']
  end;
  TINDateSearchTypeResolutionResult = class(TOCGenericImport<INDateSearchTypeResolutionResultClass, INDateSearchTypeResolutionResult>) end;

  INDeleteTasksTaskListResolutionResultClass = interface(INTaskListResolutionResultClass)
    ['{2D4C537B-9F4B-43B4-8AC3-AB90BB1B8993}']
    {class} function unsupportedForReason(reason: INDeleteTasksTaskListUnsupportedReason): Pointer; cdecl;
  end;

  INDeleteTasksTaskListResolutionResult = interface(INTaskListResolutionResult)
    ['{CDB5BEDE-71B0-47D5-93D4-08A7AC812FDD}']
    function initWithTaskListResolutionResult(taskListResolutionResult: INTaskListResolutionResult): Pointer; cdecl;
  end;
  TINDeleteTasksTaskListResolutionResult = class(TOCGenericImport<INDeleteTasksTaskListResolutionResultClass,
    INDeleteTasksTaskListResolutionResult>) end;

  INTaskResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{0209BB5F-E174-4618-888F-AE4815248957}']
    {class} function confirmationRequiredWithTaskToConfirm(taskToConfirm: INTask): Pointer; cdecl;
    {class} function disambiguationWithTasksToDisambiguate(tasksToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedTask(resolvedTask: INTask): Pointer; cdecl;
  end;

  INTaskResolutionResult = interface(INIntentResolutionResult)
    ['{6DC708ED-6958-4C06-9779-C810043C61B2}']
  end;
  TINTaskResolutionResult = class(TOCGenericImport<INTaskResolutionResultClass, INTaskResolutionResult>) end;

  INDeleteTasksTaskResolutionResultClass = interface(INTaskResolutionResultClass)
    ['{B0B7EC87-C14F-44C1-AFC7-C982DD9BC0E6}']
    {class} function unsupportedForReason(reason: INDeleteTasksTaskUnsupportedReason): Pointer; cdecl;
  end;

  INDeleteTasksTaskResolutionResult = interface(INTaskResolutionResult)
    ['{669F09D0-8A9C-418C-AA85-D9907B21D640}']
    function initWithTaskResolutionResult(taskResolutionResult: INTaskResolutionResult): Pointer; cdecl;
  end;
  TINDeleteTasksTaskResolutionResult = class(TOCGenericImport<INDeleteTasksTaskResolutionResultClass, INDeleteTasksTaskResolutionResult>) end;

  INFileResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{E57BE154-6297-4581-833D-8F7A449CFEFC}']
    {class} function confirmationRequiredWithFileToConfirm(fileToConfirm: INFile): Pointer; cdecl;
    {class} function disambiguationWithFilesToDisambiguate(filesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedFile(resolvedFile: INFile): Pointer; cdecl;
  end;

  INFileResolutionResult = interface(INIntentResolutionResult)
    ['{FCC768BC-FC41-47CC-90C6-532990EA6AC4}']
  end;
  TINFileResolutionResult = class(TOCGenericImport<INFileResolutionResultClass, INFileResolutionResult>) end;

  INFlightClass = interface(NSObjectClass)
    ['{BCDF52B9-845F-4722-8A95-56144A8E83E5}']
  end;

  INFlight = interface(NSObject)
    ['{5909001A-0377-4239-B902-C2CD85BC6E0C}']
    function airline: INAirline; cdecl;
    function arrivalAirportGate: INAirportGate; cdecl;
    function boardingTime: INDateComponentsRange; cdecl;
    function departureAirportGate: INAirportGate; cdecl;
    function flightDuration: INDateComponentsRange; cdecl;
    function flightNumber: NSString; cdecl;
    function initWithAirline(airline: INAirline; flightNumber: NSString; boardingTime: INDateComponentsRange; flightDuration: INDateComponentsRange;
      departureAirportGate: INAirportGate; arrivalAirportGate: INAirportGate): Pointer; cdecl;
  end;
  TINFlight = class(TOCGenericImport<INFlightClass, INFlight>) end;

  INLocationSearchTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{AA8FE334-B9B5-45FC-A9CB-37A7AB26F0F7}']
    {class} function confirmationRequiredWithLocationSearchTypeToConfirm(locationSearchTypeToConfirm: INLocationSearchType): Pointer; cdecl;
    {class} function successWithResolvedLocationSearchType(resolvedLocationSearchType: INLocationSearchType): Pointer; cdecl;
  end;

  INLocationSearchTypeResolutionResult = interface(INIntentResolutionResult)
    ['{DF442765-5BCC-4B78-845F-9F906A8A7579}']
  end;
  TINLocationSearchTypeResolutionResult = class(TOCGenericImport<INLocationSearchTypeResolutionResultClass, INLocationSearchTypeResolutionResult>) end;

  INMediaAffinityTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{8CC518DB-0A5D-4B4A-A9E0-3F55715F056A}']
    {class} function confirmationRequiredWithMediaAffinityTypeToConfirm(mediaAffinityTypeToConfirm: INMediaAffinityType): Pointer; cdecl;
    {class} function successWithResolvedMediaAffinityType(resolvedMediaAffinityType: INMediaAffinityType): Pointer; cdecl;
  end;

  INMediaAffinityTypeResolutionResult = interface(INIntentResolutionResult)
    ['{C6A90211-36A2-480C-ADEC-33772C8FDB9A}']
  end;
  TINMediaAffinityTypeResolutionResult = class(TOCGenericImport<INMediaAffinityTypeResolutionResultClass, INMediaAffinityTypeResolutionResult>) end;

  INMediaDestinationClass = interface(NSObjectClass)
    ['{9FC5DADF-70F4-4671-A4F4-062F67929CE9}']
    {class} function libraryDestination: Pointer; cdecl;
    {class} function playlistDestinationWithName(playlistName: NSString): Pointer; cdecl;
  end;

  INMediaDestination = interface(NSObject)
    ['{547D61E2-0B74-4531-83E7-9EF3C984FA17}']
    function mediaDestinationType: INMediaDestinationType; cdecl;
    function playlistName: NSString; cdecl;
  end;
  TINMediaDestination = class(TOCGenericImport<INMediaDestinationClass, INMediaDestination>) end;

  INMediaSearchClass = interface(NSObjectClass)
    ['{FC299B4E-F97C-44BE-85F9-F3C2A5ACC4F0}']
  end;

  INMediaSearch = interface(NSObject)
    ['{AFC3CB74-7881-449E-8C2C-C9E6C6931011}']
    function activityNames: NSArray; cdecl; // API_DEPRECATED("Use `moodNames` property instead.", ios(13.0, 13.0), watchos(6.0, 6.0))
    function albumName: NSString; cdecl;
    function artistName: NSString; cdecl;
    function genreNames: NSArray; cdecl;
    function initWithMediaType(mediaType: INMediaItemType; sortOrder: INMediaSortOrder; mediaName: NSString; artistName: NSString;
      albumName: NSString; genreNames: NSArray; moodNames: NSArray; releaseDate: INDateComponentsRange; reference: INMediaReference;
      mediaIdentifier: NSString): Pointer; cdecl;
    function mediaIdentifier: NSString; cdecl;
    function mediaName: NSString; cdecl;
    function mediaType: INMediaItemType; cdecl;
    function moodNames: NSArray; cdecl;
    function reference: INMediaReference; cdecl;
    function releaseDate: INDateComponentsRange; cdecl;
    function sortOrder: INMediaSortOrder; cdecl;
  end;
  TINMediaSearch = class(TOCGenericImport<INMediaSearchClass, INMediaSearch>) end;

  INMessageAttributeOptionsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{A79D8DB7-518C-41DE-9A39-49026729D959}']
    {class} function confirmationRequiredWithMessageAttributeOptionsToConfirm(messageAttributeOptionsToConfirm: INMessageAttributeOptions): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INMessageAttributeOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithMessageAttributeOptionsToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedMessageAttributeOptions(resolvedMessageAttributeOptions: INMessageAttributeOptions): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INMessageAttributeOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedMessageAttributeOptions:", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INMessageAttributeOptionsResolutionResult = interface(INIntentResolutionResult)
    ['{4C6BF666-187D-4002-AD0F-6F4E9FA2D946}']
  end;
  TINMessageAttributeOptionsResolutionResult = class(TOCGenericImport<INMessageAttributeOptionsResolutionResultClass,
    INMessageAttributeOptionsResolutionResult>) end;

  INMessageAttributeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{27A7D592-A008-4B1E-B48D-69193671F3E6}']
    {class} function confirmationRequiredWithMessageAttributeToConfirm(messageAttributeToConfirm: INMessageAttribute): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INMessageAttribute): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithMessageAttributeToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedMessageAttribute(resolvedMessageAttribute: INMessageAttribute): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INMessageAttribute): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedMessageAttribute:", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INMessageAttributeResolutionResult = interface(INIntentResolutionResult)
    ['{039ED95F-3C00-4775-9350-01783FB9FF97}']
  end;
  TINMessageAttributeResolutionResult = class(TOCGenericImport<INMessageAttributeResolutionResultClass, INMessageAttributeResolutionResult>) end;

  INNoteClass = interface(NSObjectClass)
    ['{5686D806-BEFA-463C-8131-E64CC3A8A509}']
  end;

  INNote = interface(NSObject)
    ['{2570EF9B-17F6-4B6F-AE8C-59E950AFD3D1}']
    function contents: NSArray; cdecl;
    function createdDateComponents: NSDateComponents; cdecl;
    function groupName: INSpeakableString; cdecl;
    function identifier: NSString; cdecl;
    function initWithTitle(title: INSpeakableString; contents: NSArray; groupName: INSpeakableString; createdDateComponents: NSDateComponents;
      modifiedDateComponents: NSDateComponents; identifier: NSString): Pointer; cdecl;
    function modifiedDateComponents: NSDateComponents; cdecl;
    function title: INSpeakableString; cdecl;
  end;
  TINNote = class(TOCGenericImport<INNoteClass, INNote>) end;

  INNoteContentResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{87D28612-E7AE-4D30-B3D3-C378AFC9284F}']
    {class} function confirmationRequiredWithNoteContentToConfirm(noteContentToConfirm: INNoteContent): Pointer; cdecl;
    {class} function disambiguationWithNoteContentsToDisambiguate(noteContentsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedNoteContent(resolvedNoteContent: INNoteContent): Pointer; cdecl;
  end;

  INNoteContentResolutionResult = interface(INIntentResolutionResult)
    ['{B477F863-985F-409A-BB33-99C191CFDD20}']
  end;
  TINNoteContentResolutionResult = class(TOCGenericImport<INNoteContentResolutionResultClass, INNoteContentResolutionResult>) end;

  INNoteContentTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{AFB35040-4A89-490D-9D6F-27FD951716C4}']
    {class} function confirmationRequiredWithNoteContentTypeToConfirm(noteContentTypeToConfirm: INNoteContentType): Pointer; cdecl;
    {class} function successWithResolvedNoteContentType(resolvedNoteContentType: INNoteContentType): Pointer; cdecl;
  end;

  INNoteContentTypeResolutionResult = interface(INIntentResolutionResult)
    ['{C53C4D75-9117-4444-9E41-C1E3BC67658D}']
  end;
  TINNoteContentTypeResolutionResult = class(TOCGenericImport<INNoteContentTypeResolutionResultClass, INNoteContentTypeResolutionResult>) end;

  INNoteResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{F23E4881-B552-4945-B4E5-9C062B12F7FF}']
    {class} function confirmationRequiredWithNoteToConfirm(noteToConfirm: INNote): Pointer; cdecl;
    {class} function disambiguationWithNotesToDisambiguate(notesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedNote(resolvedNote: INNote): Pointer; cdecl;
  end;

  INNoteResolutionResult = interface(INIntentResolutionResult)
    ['{94EF44B3-A689-447E-B0D9-C43D5E9FC1F1}']
  end;
  TINNoteResolutionResult = class(TOCGenericImport<INNoteResolutionResultClass, INNoteResolutionResult>) end;

  INNotebookItemTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{1D4AA195-9B42-4753-817D-1FAC2D7147CE}']
    {class} function confirmationRequiredWithNotebookItemTypeToConfirm(notebookItemTypeToConfirm: INNotebookItemType): Pointer; cdecl;
    {class} function disambiguationWithNotebookItemTypesToDisambiguate(notebookItemTypesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedNotebookItemType(resolvedNotebookItemType: INNotebookItemType): Pointer; cdecl;
  end;

  INNotebookItemTypeResolutionResult = interface(INIntentResolutionResult)
    ['{EFC64DAF-DCC7-4098-A835-F0B329355B49}']
  end;
  TINNotebookItemTypeResolutionResult = class(TOCGenericImport<INNotebookItemTypeResolutionResultClass, INNotebookItemTypeResolutionResult>) end;

  INOutgoingMessageTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{F4747AAD-D626-4464-9272-6C62E956ABC3}']
    {class} function confirmationRequiredWithOutgoingMessageTypeToConfirm(outgoingMessageTypeToConfirm: INOutgoingMessageType): Pointer; cdecl;
    {class} function successWithResolvedOutgoingMessageType(resolvedOutgoingMessageType: INOutgoingMessageType): Pointer; cdecl;
  end;

  INOutgoingMessageTypeResolutionResult = interface(INIntentResolutionResult)
    ['{689886F3-B97F-4DE2-AA63-499F2B7EEA46}']
  end;
  TINOutgoingMessageTypeResolutionResult = class(TOCGenericImport<INOutgoingMessageTypeResolutionResultClass,
    INOutgoingMessageTypeResolutionResult>) end;

  INPaymentAccountClass = interface(NSObjectClass)
    ['{67B7CCC5-B4FC-405A-B782-E1C4DCB4376A}']
  end;

  INPaymentAccount = interface(NSObject)
    ['{12018A18-9A50-434D-88E3-EB2E26B0ECB1}']
    function accountNumber: NSString; cdecl;
    function accountType: INAccountType; cdecl;
    function balance: INBalanceAmount; cdecl;
    function initWithNickname(nickname: INSpeakableString; number: NSString; accountType: INAccountType;
      organizationName: INSpeakableString): Pointer; overload; cdecl; // API_DEPRECATED("Please use 'initWithNickname:number:accountType:organizationName:balance:secondaryBalance:' instead", ios(10.3, 11.0), watchos(3.2, 4.0))
    function initWithNickname(nickname: INSpeakableString; number: NSString; accountType: INAccountType;
      organizationName: INSpeakableString; balance: INBalanceAmount; secondaryBalance: INBalanceAmount): Pointer; overload; cdecl;
    function nickname: INSpeakableString; cdecl;
    function organizationName: INSpeakableString; cdecl;
    function secondaryBalance: INBalanceAmount; cdecl;
  end;
  TINPaymentAccount = class(TOCGenericImport<INPaymentAccountClass, INPaymentAccount>) end;

  INPaymentAccountResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{9DECD4E4-94D7-4872-AD28-47133754034B}']
    {class} function confirmationRequiredWithPaymentAccountToConfirm(paymentAccountToConfirm: INPaymentAccount): Pointer; cdecl;
    {class} function disambiguationWithPaymentAccountsToDisambiguate(paymentAccountsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPaymentAccount(resolvedPaymentAccount: INPaymentAccount): Pointer; cdecl;
  end;

  INPaymentAccountResolutionResult = interface(INIntentResolutionResult)
    ['{10731B4C-5929-4239-AB23-47316623C789}']
  end;
  TINPaymentAccountResolutionResult = class(TOCGenericImport<INPaymentAccountResolutionResultClass, INPaymentAccountResolutionResult>) end;

  INPaymentAmountClass = interface(NSObjectClass)
    ['{2A70F23D-76F5-47C3-9D26-0105521EBA9D}']
  end;

  INPaymentAmount = interface(NSObject)
    ['{5BD546D8-C2D3-42CE-B1C5-9C3E5552027F}']
    function amount: INCurrencyAmount; cdecl;
    function amountType: INAmountType; cdecl;
    function initWithAmountType(amountType: INAmountType; amount: INCurrencyAmount): Pointer; cdecl;
  end;
  TINPaymentAmount = class(TOCGenericImport<INPaymentAmountClass, INPaymentAmount>) end;

  INPaymentAmountResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{34D87D32-F047-4372-AF1F-FCD99B843B95}']
    {class} function confirmationRequiredWithPaymentAmountToConfirm(paymentAmountToConfirm: INPaymentAmount): Pointer; cdecl;
    {class} function disambiguationWithPaymentAmountsToDisambiguate(paymentAmountsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPaymentAmount(resolvedPaymentAmount: INPaymentAmount): Pointer; cdecl;
  end;

  INPaymentAmountResolutionResult = interface(INIntentResolutionResult)
    ['{6E4AE9B0-9C60-487C-AA6A-A6891E55120D}']
  end;
  TINPaymentAmountResolutionResult = class(TOCGenericImport<INPaymentAmountResolutionResultClass, INPaymentAmountResolutionResult>) end;

  INPaymentMethodClass = interface(NSObjectClass)
    ['{0D49D1D5-EE0B-463E-99B0-16303720A5C3}']
    {class} function applePayPaymentMethod: Pointer; cdecl;
  end;

  INPaymentMethod = interface(NSObject)
    ['{7C29C7A8-1EC1-46C6-A4C1-465056F8CFA6}']
    [MethodName('type')]
    function &type: INPaymentMethodType; cdecl;
    function icon: INImage; cdecl;
    function identificationHint: NSString; cdecl;
    function initWithType(&type: INPaymentMethodType; name: NSString; identificationHint: NSString; icon: INImage): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TINPaymentMethod = class(TOCGenericImport<INPaymentMethodClass, INPaymentMethod>) end;

  INPaymentMethodResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{F75DCB71-5893-4734-95A8-894C8D0C647D}']
    {class} function confirmationRequiredWithPaymentMethodToConfirm(paymentMethodToConfirm: INPaymentMethod): Pointer; cdecl;
    {class} function disambiguationWithPaymentMethodsToDisambiguate(paymentMethodsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPaymentMethod(resolvedPaymentMethod: INPaymentMethod): Pointer; cdecl;
  end;

  INPaymentMethodResolutionResult = interface(INIntentResolutionResult)
    ['{A4E19CD8-625F-4437-878F-1E43FCA383CD}']
  end;
  TINPaymentMethodResolutionResult = class(TOCGenericImport<INPaymentMethodResolutionResultClass, INPaymentMethodResolutionResult>) end;

  INPaymentRecordClass = interface(NSObjectClass)
    ['{3C601F4E-1967-4A7E-B619-F4488F938150}']
  end;

  INPaymentRecord = interface(NSObject)
    ['{57D13247-CD84-4F0D-B10A-87A46C498526}']
    function currencyAmount: INCurrencyAmount; cdecl;
    function feeAmount: INCurrencyAmount; cdecl;
    function initWithPayee(payee: INPerson; payer: INPerson; currencyAmount: INCurrencyAmount; paymentMethod: INPaymentMethod; note: NSString;
      status: INPaymentStatus; feeAmount: INCurrencyAmount): Pointer; overload; cdecl;
    function initWithPayee(payee: INPerson; payer: INPerson; currencyAmount: INCurrencyAmount; paymentMethod: INPaymentMethod; note: NSString;
      status: INPaymentStatus): Pointer; overload; cdecl;
    function note: NSString; cdecl;
    function payee: INPerson; cdecl;
    function payer: INPerson; cdecl;
    function paymentMethod: INPaymentMethod; cdecl;
    function status: INPaymentStatus; cdecl;
  end;
  TINPaymentRecord = class(TOCGenericImport<INPaymentRecordClass, INPaymentRecord>) end;

  INPaymentStatusResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{3D94C730-4F54-4FA1-AF3D-BA2C377D7B74}']
    {class} function confirmationRequiredWithPaymentStatusToConfirm(paymentStatusToConfirm: INPaymentStatus): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INPaymentStatus): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithPaymentStatusToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedPaymentStatus(resolvedPaymentStatus: INPaymentStatus): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INPaymentStatus): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedPaymentStatus:", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INPaymentStatusResolutionResult = interface(INIntentResolutionResult)
    ['{34FA1359-2C70-4BC0-A95F-6C4D0D576831}']
  end;
  TINPaymentStatusResolutionResult = class(TOCGenericImport<INPaymentStatusResolutionResultClass, INPaymentStatusResolutionResult>) end;

  INPlayMediaMediaItemResolutionResultClass = interface(INMediaItemResolutionResultClass)
    ['{D5F0FC58-478B-4410-8CA8-B5A64BB36290}']
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function unsupportedForReason(reason: INPlayMediaMediaItemUnsupportedReason): Pointer; cdecl;
  end;

  INPlayMediaMediaItemResolutionResult = interface(INMediaItemResolutionResult)
    ['{7B20981F-8D95-479B-849E-3C9770922665}']
    function initWithMediaItemResolutionResult(mediaItemResolutionResult: INMediaItemResolutionResult): Pointer; cdecl;
  end;
  TINPlayMediaMediaItemResolutionResult = class(TOCGenericImport<INPlayMediaMediaItemResolutionResultClass, INPlayMediaMediaItemResolutionResult>) end;

  INDoubleResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{5DECA2DA-9F41-4F79-8C75-5938D7136681}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: NSNumber): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: Double): Pointer; cdecl;
  end;

  INDoubleResolutionResult = interface(INIntentResolutionResult)
    ['{C2EFC61B-6F65-4B66-BBC5-AC5F51C53FF5}']
  end;
  TINDoubleResolutionResult = class(TOCGenericImport<INDoubleResolutionResultClass, INDoubleResolutionResult>) end;

  INPlayMediaPlaybackSpeedResolutionResultClass = interface(INDoubleResolutionResultClass)
    ['{B7E153CA-7121-4C5F-AC4B-F0B0A7C1DF29}']
    {class} function unsupportedForReason(reason: INPlayMediaPlaybackSpeedUnsupportedReason): Pointer; cdecl;
  end;

  INPlayMediaPlaybackSpeedResolutionResult = interface(INDoubleResolutionResult)
    ['{C8145284-E839-45B7-9442-F20A0F5A824C}']
    function initWithDoubleResolutionResult(doubleResolutionResult: INDoubleResolutionResult): Pointer; cdecl;
  end;
  TINPlayMediaPlaybackSpeedResolutionResult = class(TOCGenericImport<INPlayMediaPlaybackSpeedResolutionResultClass,
    INPlayMediaPlaybackSpeedResolutionResult>) end;

  INPlaybackQueueLocationResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{50CC12E7-95D6-4E2B-B7BC-4EF23DA894DA}']
    {class} function confirmationRequiredWithPlaybackQueueLocationToConfirm(playbackQueueLocationToConfirm: INPlaybackQueueLocation): Pointer; cdecl;
    {class} function successWithResolvedPlaybackQueueLocation(resolvedPlaybackQueueLocation: INPlaybackQueueLocation): Pointer; cdecl;
  end;

  INPlaybackQueueLocationResolutionResult = interface(INIntentResolutionResult)
    ['{942B7451-62E1-4B8A-B6E7-C0C870E39EB7}']
  end;
  TINPlaybackQueueLocationResolutionResult = class(TOCGenericImport<INPlaybackQueueLocationResolutionResultClass,
    INPlaybackQueueLocationResolutionResult>) end;

  INPlaybackRepeatModeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{E54DE98B-2B8C-427C-BD7C-FA4D1A0ED1C4}']
    {class} function confirmationRequiredWithPlaybackRepeatModeToConfirm(playbackRepeatModeToConfirm: INPlaybackRepeatMode): Pointer; cdecl;
    {class} function successWithResolvedPlaybackRepeatMode(resolvedPlaybackRepeatMode: INPlaybackRepeatMode): Pointer; cdecl;
  end;

  INPlaybackRepeatModeResolutionResult = interface(INIntentResolutionResult)
    ['{4A407A9B-89F8-423D-896D-19BFF0D09E3C}']
  end;
  TINPlaybackRepeatModeResolutionResult = class(TOCGenericImport<INPlaybackRepeatModeResolutionResultClass, INPlaybackRepeatModeResolutionResult>) end;

  INRadioTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{5A790201-C11F-4C6C-BAD0-A6A41C4FCBB7}']
    {class} function confirmationRequiredWithRadioTypeToConfirm(radioTypeToConfirm: INRadioType): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INRadioType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithRadioTypeToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedRadioType(resolvedRadioType: INRadioType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INRadioType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedRadioType:", ios(10.0, 11.0))
  end;

  INRadioTypeResolutionResult = interface(INIntentResolutionResult)
    ['{0BBAFE17-C25C-4EBD-9FAE-252A42D151FC}']
  end;
  TINRadioTypeResolutionResult = class(TOCGenericImport<INRadioTypeResolutionResultClass, INRadioTypeResolutionResult>) end;

  INRelativeReferenceResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{307165D8-D438-45F0-96AF-A8C33BEFD6F5}']
    {class} function confirmationRequiredWithRelativeReferenceToConfirm(relativeReferenceToConfirm: INRelativeReference): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INRelativeReference): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithRelativeReferenceToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedRelativeReference(resolvedRelativeReference: INRelativeReference): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INRelativeReference): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedRelativeReference:", ios(10.0, 11.0))
  end;

  INRelativeReferenceResolutionResult = interface(INIntentResolutionResult)
    ['{7B8F9FA8-DAF2-4B7D-8E94-91196BE97A7B}']
  end;
  TINRelativeReferenceResolutionResult = class(TOCGenericImport<INRelativeReferenceResolutionResultClass, INRelativeReferenceResolutionResult>) end;

  INRelativeSettingResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{A4E89CAB-274F-488F-80A3-665759AD07CA}']
    {class} function confirmationRequiredWithRelativeSettingToConfirm(relativeSettingToConfirm: INRelativeSetting): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INRelativeSetting): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithRelativeSettingToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedRelativeSetting(resolvedRelativeSetting: INRelativeSetting): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INRelativeSetting): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedRelativeSetting:", ios(10.0, 11.0))
  end;

  INRelativeSettingResolutionResult = interface(INIntentResolutionResult)
    ['{F6138A75-3C79-4734-A93F-C6090150C8BB}']
  end;
  TINRelativeSettingResolutionResult = class(TOCGenericImport<INRelativeSettingResolutionResultClass, INRelativeSettingResolutionResult>) end;

  INRentalCarClass = interface(NSObjectClass)
    ['{38E38A9E-8AF0-423D-8018-D134336A05BD}']
  end;

  INRentalCar = interface(NSObject)
    ['{710477C4-B8EC-4185-8C1B-0AE104AD0392}']
    [MethodName('type')]
    function &type: NSString; cdecl;
    function initWithRentalCompanyName(rentalCompanyName: NSString; &type: NSString; make: NSString; model: NSString;
      rentalCarDescription: NSString): Pointer; cdecl;
    function make: NSString; cdecl;
    function model: NSString; cdecl;
    function rentalCarDescription: NSString; cdecl;
    function rentalCompanyName: NSString; cdecl;
  end;
  TINRentalCar = class(TOCGenericImport<INRentalCarClass, INRentalCar>) end;

  INRequestPaymentCurrencyAmountResolutionResultClass = interface(INCurrencyAmountResolutionResultClass)
    ['{4BE1CBA8-852E-4222-A824-20AFFD7C1B88}']
    {class} function unsupportedForReason(reason: INRequestPaymentCurrencyAmountUnsupportedReason): Pointer; cdecl;
  end;

  INRequestPaymentCurrencyAmountResolutionResult = interface(INCurrencyAmountResolutionResult)
    ['{004B5CCA-8024-477B-83E2-4438D89D670C}']
    function initWithCurrencyAmountResolutionResult(currencyAmountResolutionResult: INCurrencyAmountResolutionResult): Pointer; cdecl;
  end;
  TINRequestPaymentCurrencyAmountResolutionResult = class(TOCGenericImport<INRequestPaymentCurrencyAmountResolutionResultClass,
    INRequestPaymentCurrencyAmountResolutionResult>) end;

  INPersonResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{6FC5D058-4348-4237-9009-F430F83BC3FF}']
    {class} function confirmationRequiredWithPersonToConfirm(personToConfirm: INPerson): Pointer; cdecl;
    {class} function disambiguationWithPeopleToDisambiguate(peopleToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPerson(resolvedPerson: INPerson): Pointer; cdecl;
  end;

  INPersonResolutionResult = interface(INIntentResolutionResult)
    ['{14A4051B-41A7-48CF-99C8-4E3690184C20}']
  end;
  TINPersonResolutionResult = class(TOCGenericImport<INPersonResolutionResultClass, INPersonResolutionResult>) end;

  INRequestPaymentPayerResolutionResultClass = interface(INPersonResolutionResultClass)
    ['{2582FC81-1E5C-4B43-8233-01973D3644C0}']
    {class} function unsupportedForReason(reason: INRequestPaymentPayerUnsupportedReason): Pointer; cdecl;
  end;

  INRequestPaymentPayerResolutionResult = interface(INPersonResolutionResult)
    ['{7D36B1AB-EBF9-4BBC-B74E-B2B2BA60964B}']
    function initWithPersonResolutionResult(personResolutionResult: INPersonResolutionResult): Pointer; cdecl;
  end;
  TINRequestPaymentPayerResolutionResult = class(TOCGenericImport<INRequestPaymentPayerResolutionResultClass,
    INRequestPaymentPayerResolutionResult>) end;

  INSearchForMediaMediaItemResolutionResultClass = interface(INMediaItemResolutionResultClass)
    ['{C3233A38-DEE2-44D8-BF64-4D6688F45A0D}']
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function unsupportedForReason(reason: INSearchForMediaMediaItemUnsupportedReason): Pointer; cdecl;
  end;

  INSearchForMediaMediaItemResolutionResult = interface(INMediaItemResolutionResult)
    ['{062ED448-24AE-4C1A-8BC6-1F1386AB1E66}']
    function initWithMediaItemResolutionResult(mediaItemResolutionResult: INMediaItemResolutionResult): Pointer; cdecl;
  end;
  TINSearchForMediaMediaItemResolutionResult = class(TOCGenericImport<INSearchForMediaMediaItemResolutionResultClass,
    INSearchForMediaMediaItemResolutionResult>) end;

  INSeatClass = interface(NSObjectClass)
    ['{EBB5ED5E-DFF3-45C0-A29E-78FE06DA4C59}']
  end;

  INSeat = interface(NSObject)
    ['{EAB1A079-D89B-4C1A-86A0-3957E0AE924F}']
    function initWithSeatSection(seatSection: NSString; seatRow: NSString; seatNumber: NSString; seatingType: NSString): Pointer; cdecl;
    function seatingType: NSString; cdecl;
    function seatNumber: NSString; cdecl;
    function seatRow: NSString; cdecl;
    function seatSection: NSString; cdecl;
  end;
  TINSeat = class(TOCGenericImport<INSeatClass, INSeat>) end;

  INSendMessageRecipientResolutionResultClass = interface(INPersonResolutionResultClass)
    ['{D8F6A01C-67FE-4FF6-BAE9-369AB68BA8F0}']
    {class} function unsupportedForReason(reason: INSendMessageRecipientUnsupportedReason): Pointer; cdecl;
  end;

  INSendMessageRecipientResolutionResult = interface(INPersonResolutionResult)
    ['{6973D516-553E-4725-AA01-324B94A285E6}']
    function initWithPersonResolutionResult(personResolutionResult: INPersonResolutionResult): Pointer; cdecl;
  end;
  TINSendMessageRecipientResolutionResult = class(TOCGenericImport<INSendMessageRecipientResolutionResultClass,
    INSendMessageRecipientResolutionResult>) end;

  INSendPaymentCurrencyAmountResolutionResultClass = interface(INCurrencyAmountResolutionResultClass)
    ['{1B1A327C-619B-4B5C-9DF8-39D0066765DB}']
    {class} function unsupportedForReason(reason: INSendPaymentCurrencyAmountUnsupportedReason): Pointer; cdecl;
  end;

  INSendPaymentCurrencyAmountResolutionResult = interface(INCurrencyAmountResolutionResult)
    ['{DEE45482-3714-41D7-B489-26740584C63D}']
    function initWithCurrencyAmountResolutionResult(currencyAmountResolutionResult: INCurrencyAmountResolutionResult): Pointer; cdecl;
  end;
  TINSendPaymentCurrencyAmountResolutionResult = class(TOCGenericImport<INSendPaymentCurrencyAmountResolutionResultClass,
    INSendPaymentCurrencyAmountResolutionResult>) end;

  INSendPaymentPayeeResolutionResultClass = interface(INPersonResolutionResultClass)
    ['{268B25B8-298E-4CE0-BC69-521A9F627AD7}']
    {class} function unsupportedForReason(reason: INSendPaymentPayeeUnsupportedReason): Pointer; cdecl;
  end;

  INSendPaymentPayeeResolutionResult = interface(INPersonResolutionResult)
    ['{DC4E0271-9A65-4AFA-947C-FC9A3457C1E9}']
    function initWithPersonResolutionResult(personResolutionResult: INPersonResolutionResult): Pointer; cdecl;
  end;
  TINSendPaymentPayeeResolutionResult = class(TOCGenericImport<INSendPaymentPayeeResolutionResultClass, INSendPaymentPayeeResolutionResult>) end;

  INSetTaskAttributeTemporalEventTriggerResolutionResultClass = interface(INTemporalEventTriggerResolutionResultClass)
    ['{FFF4ECAA-E31A-4694-ACE0-2885EA0B6D5A}']
    {class} function unsupportedForReason(reason: INSetTaskAttributeTemporalEventTriggerUnsupportedReason): Pointer; cdecl;
  end;

  INSetTaskAttributeTemporalEventTriggerResolutionResult = interface(INTemporalEventTriggerResolutionResult)
    ['{94F03CB5-BCB3-4CE6-AD37-A1D68A1BB890}']
    function initWithTemporalEventTriggerResolutionResult(temporalEventTriggerResolutionResult: INTemporalEventTriggerResolutionResult): Pointer; cdecl;
  end;
  TINSetTaskAttributeTemporalEventTriggerResolutionResult = class(TOCGenericImport<INSetTaskAttributeTemporalEventTriggerResolutionResultClass,
    INSetTaskAttributeTemporalEventTriggerResolutionResult>) end;

  INSnoozeTasksTaskResolutionResultClass = interface(INTaskResolutionResultClass)
    ['{8156CBF8-75EB-4D20-AE0D-161FAF172978}']
    {class} function unsupportedForReason(reason: INSnoozeTasksTaskUnsupportedReason): Pointer; cdecl;
  end;

  INSnoozeTasksTaskResolutionResult = interface(INTaskResolutionResult)
    ['{C0A9E6C0-CE83-4101-8131-CE91B25477F2}']
    function initWithTaskResolutionResult(taskResolutionResult: INTaskResolutionResult): Pointer; cdecl;
  end;
  TINSnoozeTasksTaskResolutionResult = class(TOCGenericImport<INSnoozeTasksTaskResolutionResultClass, INSnoozeTasksTaskResolutionResult>) end;

  INSpatialEventTriggerClass = interface(NSObjectClass)
    ['{D05F2102-4CF6-41F8-B7D3-C1B34E2CF127}']
  end;

  INSpatialEventTrigger = interface(NSObject)
    ['{406CAE90-6CA9-405F-8C4B-CF34706071A4}']
    function event: INSpatialEvent; cdecl;
    function initWithPlacemark(placemark: CLPlacemark; event: INSpatialEvent): Pointer; cdecl;
    function placemark: CLPlacemark; cdecl;
  end;
  TINSpatialEventTrigger = class(TOCGenericImport<INSpatialEventTriggerClass, INSpatialEventTrigger>) end;

  INSpatialEventTriggerResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{EE6BEE40-63C9-4FC6-9FDD-180A038E75F4}']
    {class} function confirmationRequiredWithSpatialEventTriggerToConfirm(spatialEventTriggerToConfirm: INSpatialEventTrigger): Pointer; cdecl;
    {class} function disambiguationWithSpatialEventTriggersToDisambiguate(spatialEventTriggersToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedSpatialEventTrigger(resolvedSpatialEventTrigger: INSpatialEventTrigger): Pointer; cdecl;
  end;

  INSpatialEventTriggerResolutionResult = interface(INIntentResolutionResult)
    ['{C35722AE-54DE-4B60-9A18-4680A945E93A}']
  end;
  TINSpatialEventTriggerResolutionResult = class(TOCGenericImport<INSpatialEventTriggerResolutionResultClass,
    INSpatialEventTriggerResolutionResult>) end;

  INStartCallCallCapabilityResolutionResultClass = interface(INCallCapabilityResolutionResultClass)
    ['{5DC02CC8-7860-459B-B0F7-036439FB42C7}']
    {class} function unsupportedForReason(reason: INStartCallCallCapabilityUnsupportedReason): Pointer; cdecl;
  end;

  INStartCallCallCapabilityResolutionResult = interface(INCallCapabilityResolutionResult)
    ['{A92383EB-0992-4F83-A4D4-119CEEB6AC90}']
    function initWithCallCapabilityResolutionResult(callCapabilityResolutionResult: INCallCapabilityResolutionResult): Pointer; cdecl;
  end;
  TINStartCallCallCapabilityResolutionResult = class(TOCGenericImport<INStartCallCallCapabilityResolutionResultClass,
    INStartCallCallCapabilityResolutionResult>) end;

  INStartCallCallRecordToCallBackResolutionResultClass = interface(INCallRecordResolutionResultClass)
    ['{82BC70B9-E2AB-48B0-8AF8-4327CDAAFFFB}']
    {class} function unsupportedForReason(reason: INStartCallCallRecordToCallBackUnsupportedReason): Pointer; cdecl;
  end;

  INStartCallCallRecordToCallBackResolutionResult = interface(INCallRecordResolutionResult)
    ['{536B5258-867F-4081-91CB-E871858AD455}']
    function initWithCallRecordResolutionResult(callRecordResolutionResult: INCallRecordResolutionResult): Pointer; cdecl;
  end;
  TINStartCallCallRecordToCallBackResolutionResult = class(TOCGenericImport<INStartCallCallRecordToCallBackResolutionResultClass,
    INStartCallCallRecordToCallBackResolutionResult>) end;

  INStartCallContactResolutionResultClass = interface(INPersonResolutionResultClass)
    ['{E838B5C9-AC5A-4738-91CB-F489BDD0DB20}']
    {class} function unsupportedForReason(reason: INStartCallContactUnsupportedReason): Pointer; cdecl;
  end;

  INStartCallContactResolutionResult = interface(INPersonResolutionResult)
    ['{48604515-3FF2-408F-B172-59BF610258A6}']
    function initWithPersonResolutionResult(personResolutionResult: INPersonResolutionResult): Pointer; cdecl;
  end;
  TINStartCallContactResolutionResult = class(TOCGenericImport<INStartCallContactResolutionResultClass, INStartCallContactResolutionResult>) end;

  INTaskClass = interface(NSObjectClass)
    ['{EC882752-349F-4F60-AA3A-2B0BFB7BFB27}']
  end;

  INTask = interface(NSObject)
    ['{F4BF8073-75AC-47A8-8AEF-5EB29B46D74E}']
    function createdDateComponents: NSDateComponents; cdecl;
    function identifier: NSString; cdecl;
    function initWithTitle(title: INSpeakableString; status: INTaskStatus; taskType: INTaskType; spatialEventTrigger: INSpatialEventTrigger;
      temporalEventTrigger: INTemporalEventTrigger; createdDateComponents: NSDateComponents; modifiedDateComponents: NSDateComponents;
      identifier: NSString; priority: INTaskPriority): Pointer; overload; cdecl;
    function initWithTitle(title: INSpeakableString; status: INTaskStatus; taskType: INTaskType; spatialEventTrigger: INSpatialEventTrigger;
      temporalEventTrigger: INTemporalEventTrigger; createdDateComponents: NSDateComponents; modifiedDateComponents: NSDateComponents;
      identifier: NSString): Pointer; overload; cdecl;
    function modifiedDateComponents: NSDateComponents; cdecl;
    function priority: INTaskPriority; cdecl;
    function spatialEventTrigger: INSpatialEventTrigger; cdecl;
    function status: INTaskStatus; cdecl;
    function taskType: INTaskType; cdecl;
    function temporalEventTrigger: INTemporalEventTrigger; cdecl;
    function title: INSpeakableString; cdecl;
  end;
  TINTask = class(TOCGenericImport<INTaskClass, INTask>) end;

  INTaskListClass = interface(NSObjectClass)
    ['{F8C90BEE-B3FD-4321-81CE-789A3F15B374}']
  end;

  INTaskList = interface(NSObject)
    ['{8ABE86B1-C732-490F-862E-265FA042CDE8}']
    function createdDateComponents: NSDateComponents; cdecl;
    function groupName: INSpeakableString; cdecl;
    function identifier: NSString; cdecl;
    function initWithTitle(title: INSpeakableString; tasks: NSArray; groupName: INSpeakableString; createdDateComponents: NSDateComponents;
      modifiedDateComponents: NSDateComponents; identifier: NSString): Pointer; cdecl;
    function modifiedDateComponents: NSDateComponents; cdecl;
    function tasks: NSArray; cdecl;
    function title: INSpeakableString; cdecl;
  end;
  TINTaskList = class(TOCGenericImport<INTaskListClass, INTaskList>) end;

  INTaskPriorityResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{1A90A421-C123-4D03-868F-FCA03FFBC87E}']
    {class} function confirmationRequiredWithTaskPriorityToConfirm(taskPriorityToConfirm: INTaskPriority): Pointer; cdecl;
    {class} function successWithResolvedTaskPriority(resolvedTaskPriority: INTaskPriority): Pointer; cdecl;
  end;

  INTaskPriorityResolutionResult = interface(INIntentResolutionResult)
    ['{9AA3D3EE-7256-410F-A9A1-7F1CB667653F}']
  end;
  TINTaskPriorityResolutionResult = class(TOCGenericImport<INTaskPriorityResolutionResultClass, INTaskPriorityResolutionResult>) end;

  INTaskStatusResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{6A9AAE5E-8D40-4233-A773-92C4C0A543E2}']
    {class} function confirmationRequiredWithTaskStatusToConfirm(taskStatusToConfirm: INTaskStatus): Pointer; cdecl;
    {class} function successWithResolvedTaskStatus(resolvedTaskStatus: INTaskStatus): Pointer; cdecl;
  end;

  INTaskStatusResolutionResult = interface(INIntentResolutionResult)
    ['{F8747278-B936-4935-BAC7-A10F908CF9F3}']
  end;
  TINTaskStatusResolutionResult = class(TOCGenericImport<INTaskStatusResolutionResultClass, INTaskStatusResolutionResult>) end;

  INTemporalEventTriggerClass = interface(NSObjectClass)
    ['{B3AF5393-EAE4-45FF-877B-783F635E467A}']
  end;

  INTemporalEventTrigger = interface(NSObject)
    ['{A8766B28-F357-4B05-A86B-96F76A3E855F}']
    function dateComponentsRange: INDateComponentsRange; cdecl;
    function initWithDateComponentsRange(dateComponentsRange: INDateComponentsRange): Pointer; cdecl;
  end;
  TINTemporalEventTrigger = class(TOCGenericImport<INTemporalEventTriggerClass, INTemporalEventTrigger>) end;

  INTemporalEventTriggerTypeOptionsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{00779A55-F56D-45B8-8C58-33DA82FAFCF5}']
    {class} function confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm(temporalEventTriggerTypeOptionsToConfirm: INTemporalEventTriggerTypeOptions): Pointer; cdecl;
    {class} function successWithResolvedTemporalEventTriggerTypeOptions(resolvedTemporalEventTriggerTypeOptions: INTemporalEventTriggerTypeOptions): Pointer; cdecl;
  end;

  INTemporalEventTriggerTypeOptionsResolutionResult = interface(INIntentResolutionResult)
    ['{9346BB9E-6C3E-4A40-9488-598A5E8E2EF0}']
  end;
  TINTemporalEventTriggerTypeOptionsResolutionResult = class(TOCGenericImport<INTemporalEventTriggerTypeOptionsResolutionResultClass,
    INTemporalEventTriggerTypeOptionsResolutionResult>) end;

  INTicketedEventClass = interface(NSObjectClass)
    ['{C05E0BB2-0201-4618-9B8F-D8E823DA956F}']
  end;

  INTicketedEvent = interface(NSObject)
    ['{A145F428-B722-4088-B50E-2C25598F2D77}']
    function category: INTicketedEventCategory; cdecl;
    function eventDuration: INDateComponentsRange; cdecl;
    function initWithCategory(category: INTicketedEventCategory; name: NSString; eventDuration: INDateComponentsRange;
      location: CLPlacemark): Pointer; cdecl;
    function location: CLPlacemark; cdecl;
    function name: NSString; cdecl;
  end;
  TINTicketedEvent = class(TOCGenericImport<INTicketedEventClass, INTicketedEvent>) end;

  INTrainTripClass = interface(NSObjectClass)
    ['{78821986-09C9-44C1-810F-A3D14BE10984}']
  end;

  INTrainTrip = interface(NSObject)
    ['{E77DFBBA-9687-46D0-85D7-7AE4A2FC2F53}']
    function arrivalPlatform: NSString; cdecl;
    function arrivalStationLocation: CLPlacemark; cdecl;
    function departurePlatform: NSString; cdecl;
    function departureStationLocation: CLPlacemark; cdecl;
    function initWithProvider(provider: NSString; trainName: NSString; trainNumber: NSString; tripDuration: INDateComponentsRange;
      departureStationLocation: CLPlacemark; departurePlatform: NSString; arrivalStationLocation: CLPlacemark;
      arrivalPlatform: NSString): Pointer; cdecl;
    function provider: NSString; cdecl;
    function trainName: NSString; cdecl;
    function trainNumber: NSString; cdecl;
    function tripDuration: INDateComponentsRange; cdecl;
  end;
  TINTrainTrip = class(TOCGenericImport<INTrainTripClass, INTrainTrip>) end;

  INUpdateMediaAffinityMediaItemResolutionResultClass = interface(INMediaItemResolutionResultClass)
    ['{DDCE1381-F501-4BF4-8005-61B202D0A785}']
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function unsupportedForReason(reason: INUpdateMediaAffinityMediaItemUnsupportedReason): Pointer; cdecl;
  end;

  INUpdateMediaAffinityMediaItemResolutionResult = interface(INMediaItemResolutionResult)
    ['{9B49D3A5-B40A-4D05-80C8-846698F127FF}']
    function initWithMediaItemResolutionResult(mediaItemResolutionResult: INMediaItemResolutionResult): Pointer; cdecl;
  end;
  TINUpdateMediaAffinityMediaItemResolutionResult = class(TOCGenericImport<INUpdateMediaAffinityMediaItemResolutionResultClass,
    INUpdateMediaAffinityMediaItemResolutionResult>) end;

  INVisualCodeTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{989EF6EA-77E3-4DE6-AB15-C266A98F5E1F}']
    {class} function confirmationRequiredWithVisualCodeTypeToConfirm(visualCodeTypeToConfirm: INVisualCodeType): Pointer; cdecl;
    {class} function successWithResolvedVisualCodeType(resolvedVisualCodeType: INVisualCodeType): Pointer; cdecl;
  end;

  INVisualCodeTypeResolutionResult = interface(INIntentResolutionResult)
    ['{EBE65766-299E-410A-9B7B-8F4353F388E4}']
  end;
  TINVisualCodeTypeResolutionResult = class(TOCGenericImport<INVisualCodeTypeResolutionResultClass, INVisualCodeTypeResolutionResult>) end;

  INWorkoutGoalUnitTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{E0A75439-D3B0-4BA4-8ED0-F20672B19C0C}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INWorkoutGoalUnitType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function confirmationRequiredWithWorkoutGoalUnitTypeToConfirm(workoutGoalUnitTypeToConfirm: INWorkoutGoalUnitType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INWorkoutGoalUnitType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedWorkoutGoalUnitType:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedWorkoutGoalUnitType(resolvedWorkoutGoalUnitType: INWorkoutGoalUnitType): Pointer; cdecl;
  end;

  INWorkoutGoalUnitTypeResolutionResult = interface(INIntentResolutionResult)
    ['{216F51AF-B45E-4C66-8BD9-13D1E741D737}']
  end;
  TINWorkoutGoalUnitTypeResolutionResult = class(TOCGenericImport<INWorkoutGoalUnitTypeResolutionResultClass,
    INWorkoutGoalUnitTypeResolutionResult>) end;

  INWorkoutLocationTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{91F49F45-C17B-483D-930A-0CE199B7B552}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INWorkoutLocationType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithWorkoutLocationTypeToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function confirmationRequiredWithWorkoutLocationTypeToConfirm(workoutLocationTypeToConfirm: INWorkoutLocationType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INWorkoutLocationType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedWorkoutLocationType:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedWorkoutLocationType(resolvedWorkoutLocationType: INWorkoutLocationType): Pointer; cdecl;
  end;

  INWorkoutLocationTypeResolutionResult = interface(INIntentResolutionResult)
    ['{1461930D-3071-4CBA-8661-32EF0C8EBAF3}']
  end;
  TINWorkoutLocationTypeResolutionResult = class(TOCGenericImport<INWorkoutLocationTypeResolutionResultClass,
   INWorkoutLocationTypeResolutionResult>) end;

  INExtensionClass = interface(NSObjectClass)
    ['{346F8EA0-D75B-4B8F-880E-7B310639B9D6}']
  end;

  INExtension = interface(NSObject)
    ['{E1DDEB9A-446B-4B98-A415-C5CA87737227}']
  end;
  TINExtension = class(TOCGenericImport<INExtensionClass, INExtension>) end;

  INPersonHandleClass = interface(NSObjectClass)
    ['{40E1367A-581C-41A7-8F8A-A0F968912696}']
  end;

  INPersonHandle = interface(NSObject)
    ['{80940CDC-0DE5-4D14-BF7C-D9DB529C0218}']
    [MethodName('label')]
    function &label: INPersonHandleLabel; cdecl;
    [MethodName('type')]
    function &type: INPersonHandleType; cdecl;
    function initWithValue(value: NSString; &type: INPersonHandleType): Pointer; overload; cdecl;
    function initWithValue(value: NSString; &type: INPersonHandleType; &label: INPersonHandleLabel): Pointer; overload; cdecl;
    function value: NSString; cdecl;
  end;
  TINPersonHandle = class(TOCGenericImport<INPersonHandleClass, INPersonHandle>) end;

  INCurrencyAmountClass = interface(NSObjectClass)
    ['{43E4F5C7-79F7-4BB7-BFF5-98F300E24B57}']
  end;

  INCurrencyAmount = interface(NSObject)
    ['{3AF2D84C-B117-4766-8965-656A8838BA82}']
    function amount: NSDecimalNumber; cdecl;
    function currencyCode: NSString; cdecl;
    function initWithAmount(amount: NSDecimalNumber; currencyCode: NSString): Pointer; cdecl;
  end;
  TINCurrencyAmount = class(TOCGenericImport<INCurrencyAmountClass, INCurrencyAmount>) end;

  INDateComponentsRangeClass = interface(NSObjectClass)
    ['{4B59A364-764D-4241-8946-4DF577A36828}']
  end;

  INDateComponentsRange = interface(NSObject)
    ['{0732F4EE-1B98-4382-A434-64A3F3455810}']
    function EKRecurrenceRule: EKRecurrenceRule; cdecl;
    function endDateComponents: NSDateComponents; cdecl;
    function initWithEKRecurrenceRule(recurrenceRule: EKRecurrenceRule): Pointer; cdecl;
    function initWithStartDateComponents(startDateComponents: NSDateComponents; endDateComponents: NSDateComponents): Pointer; overload; cdecl;
    function initWithStartDateComponents(startDateComponents: NSDateComponents; endDateComponents: NSDateComponents;
      recurrenceRule: INRecurrenceRule): Pointer; overload; cdecl;
    function recurrenceRule: INRecurrenceRule; cdecl;
    function startDateComponents: NSDateComponents; cdecl;
  end;
  TINDateComponentsRange = class(TOCGenericImport<INDateComponentsRangeClass, INDateComponentsRange>) end;

  INImageClass = interface(NSObjectClass)
    ['{EE0ADEC9-4965-410C-975A-E3C43810A1E6}']
    {class} function imageNamed(name: NSString): Pointer; cdecl;
    {class} function imageWithImageData(imageData: NSData): Pointer; cdecl;
    {class} function imageWithURL(URL: NSURL; width: Double; height: Double): Pointer; overload; cdecl;
    {class} function imageWithURL(URL: NSURL): Pointer; overload; cdecl;
    {class} function systemImageNamed(systemImageName: NSString): Pointer; cdecl;
  end;

  INImage = interface(NSObject)
    ['{1B83BE11-87A9-4270-827A-8D7987E102C0}']
  end;
  TINImage = class(TOCGenericImport<INImageClass, INImage>) end;

  INSpeakableStringClass = interface(NSObjectClass)
    ['{F0739DB6-7CA6-41BC-A469-7F61D4D4C4DB}']
  end;

  INSpeakableString = interface(NSObject)
    ['{8EDD0557-0BDA-40BA-AE1A-38967C31129E}']
    function initWithIdentifier(identifier: NSString; spokenPhrase: NSString; pronunciationHint: NSString): Pointer; cdecl; // API_DEPRECATED("Please use -initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:", ios(10.0, 11.0), watchos(3.2, 4.0))
    function initWithSpokenPhrase(spokenPhrase: NSString): Pointer; cdecl;
    function initWithVocabularyIdentifier(vocabularyIdentifier: NSString; spokenPhrase: NSString; pronunciationHint: NSString): Pointer; cdecl;
  end;
  TINSpeakableString = class(TOCGenericImport<INSpeakableStringClass, INSpeakableString>) end;

  INObjectClass = interface(NSObjectClass)
    ['{7FC4D61C-CB71-4253-B970-6AF31CDF7266}']
  end;

  INObject = interface(NSObject)
    ['{2891ABD8-2A4A-4FB2-8D11-947439AF2954}']
    function alternativeSpeakableMatches: NSArray; cdecl;
    function displayImage: INImage; cdecl;
    function displayString: NSString; cdecl;
    function identifier: NSString; cdecl;
    function initWithIdentifier(identifier: NSString; displayString: NSString; pronunciationHint: NSString): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; displayString: NSString): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; displayString: NSString; subtitleString: NSString;
      displayImage: INImage): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; displayString: NSString; pronunciationHint: NSString; subtitleString: NSString;
      displayImage: INImage): Pointer; overload; cdecl;
    function pronunciationHint: NSString; cdecl;
    procedure setAlternativeSpeakableMatches(alternativeSpeakableMatches: NSArray); cdecl;
    procedure setDisplayImage(displayImage: INImage); cdecl;
    procedure setSubtitleString(subtitleString: NSString); cdecl;
    function subtitleString: NSString; cdecl;
  end;
  TINObject = class(TOCGenericImport<INObjectClass, INObject>) end;

  INPersonClass = interface(NSObjectClass)
    ['{B32DBAD9-6224-4700-946E-9E0D65EF2C09}']
  end;

  INPerson = interface(NSObject)
    ['{DB168B5A-0348-4EDF-9185-2EAA108A7892}']
    function aliases: NSArray; cdecl;
    function contactIdentifier: NSString; cdecl;
    function customIdentifier: NSString; cdecl;
    function displayName: NSString; cdecl;
    function handle: NSString; cdecl; // API_DEPRECATED("Use personHandle instead", ios(10.0, 10.0), macos(10.12, 10.12))
    function image: INImage; cdecl;
    function initWithHandle(handle: NSString; nameComponents: NSPersonNameComponents; displayName: NSString; image: INImage;
      contactIdentifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.0), macos(10.12, 10.12))
    function initWithHandle(handle: NSString; displayName: NSString; contactIdentifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.0), macos(10.12, 10.12))
    function initWithHandle(handle: NSString; nameComponents: NSPersonNameComponents; contactIdentifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.0), macos(10.12, 10.12))
    function initWithPersonHandle(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString; image: INImage;
      contactIdentifier: NSString; customIdentifier: NSString): Pointer; overload; cdecl;
    function initWithPersonHandle(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString; image: INImage;
      contactIdentifier: NSString; customIdentifier: NSString; aliases: NSArray; suggestionType: INPersonSuggestionType): Pointer; overload; cdecl;
    function initWithPersonHandle(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString; image: INImage;
      contactIdentifier: NSString; customIdentifier: NSString; relationship: INPersonRelationship): Pointer; overload; cdecl;
    function initWithPersonHandle(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString; image: INImage;
      contactIdentifier: NSString; customIdentifier: NSString; isMe: Boolean): Pointer; overload; cdecl;
    function isMe: Boolean; cdecl;
    function nameComponents: NSPersonNameComponents; cdecl;
    function personHandle: INPersonHandle; cdecl;
    function relationship: INPersonRelationship; cdecl;
    function siriMatches: NSArray; cdecl;
    function suggestionType: INPersonSuggestionType; cdecl;
  end;
  TINPerson = class(TOCGenericImport<INPersonClass, INPerson>) end;

  INRecurrenceRuleClass = interface(NSObjectClass)
    ['{D3770C29-D10F-409A-882B-8279CD713B35}']
  end;

  INRecurrenceRule = interface(NSObject)
    ['{D51716A5-8205-4E75-ABF7-926DCBAC5684}']
    function frequency: INRecurrenceFrequency; cdecl;
    function initWithInterval(interval: NSUInteger; frequency: INRecurrenceFrequency;
      weeklyRecurrenceDays: INDayOfWeekOptions): Pointer; overload; cdecl;
    function initWithInterval(interval: NSUInteger; frequency: INRecurrenceFrequency): Pointer; overload; cdecl;
    function interval: NSUInteger; cdecl;
    function weeklyRecurrenceDays: INDayOfWeekOptions; cdecl;
  end;
  TINRecurrenceRule = class(TOCGenericImport<INRecurrenceRuleClass, INRecurrenceRule>) end;

  INFileClass = interface(NSObjectClass)
    ['{1302EB50-8D4E-4503-91D6-45A6365BF680}']
    {class} function fileWithData(data: NSData; filename: NSString; typeIdentifier: NSString): INFile; cdecl;
    {class} function fileWithFileURL(fileURL: NSURL; filename: NSString; typeIdentifier: NSString): INFile; cdecl;
  end;

  INFile = interface(NSObject)
    ['{976F68B8-2DC9-418A-9F9E-7014B08A6BCD}']
    function data: NSData; cdecl;
    function filename: NSString; cdecl;
    function fileURL: NSURL; cdecl;
    procedure setFilename(filename: NSString); cdecl;
    function typeIdentifier: NSString; cdecl;
  end;
  TINFile = class(TOCGenericImport<INFileClass, INFile>) end;

  INBooleanResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{BC9F6C2D-4D4C-4358-954C-9133B6A039F4}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: NSNumber): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: Boolean): Pointer; cdecl;
  end;

  INBooleanResolutionResult = interface(INIntentResolutionResult)
    ['{3B6CFEDF-020B-4E2D-AB70-5061A85B3BDA}']
  end;
  TINBooleanResolutionResult = class(TOCGenericImport<INBooleanResolutionResultClass, INBooleanResolutionResult>) end;

  INDateComponentsRangeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{BF0125FC-8749-4384-8991-34634F1F082F}']
    {class} function confirmationRequiredWithDateComponentsRangeToConfirm(dateComponentsRangeToConfirm: INDateComponentsRange): Pointer; cdecl;
    {class} function disambiguationWithDateComponentsRangesToDisambiguate(dateComponentsRangesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedDateComponentsRange(resolvedDateComponentsRange: INDateComponentsRange): Pointer; cdecl;
  end;

  INDateComponentsRangeResolutionResult = interface(INIntentResolutionResult)
    ['{2FCF3506-FA25-4EBD-8CB0-9092BCE0F908}']
  end;
  TINDateComponentsRangeResolutionResult = class(TOCGenericImport<INDateComponentsRangeResolutionResultClass,
    INDateComponentsRangeResolutionResult>) end;

  INIntegerResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{8444F152-5DF4-4474-AE30-5A3F201D0EEE}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: NSNumber): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: NSInteger): Pointer; cdecl;
  end;

  INIntegerResolutionResult = interface(INIntentResolutionResult)
    ['{D2925657-7F4E-4D71-BD21-8338B23FFC56}']
  end;
  TINIntegerResolutionResult = class(TOCGenericImport<INIntegerResolutionResultClass, INIntegerResolutionResult>) end;

  INPlacemarkResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{130635DF-2902-401C-B622-096CF263A6D3}']
    {class} function confirmationRequiredWithPlacemarkToConfirm(placemarkToConfirm: CLPlacemark): Pointer; cdecl;
    {class} function disambiguationWithPlacemarksToDisambiguate(placemarksToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPlacemark(resolvedPlacemark: CLPlacemark): Pointer; cdecl;
  end;

  INPlacemarkResolutionResult = interface(INIntentResolutionResult)
    ['{3354EDDD-7118-4768-8C8F-A5536D6CD341}']
  end;
  TINPlacemarkResolutionResult = class(TOCGenericImport<INPlacemarkResolutionResultClass, INPlacemarkResolutionResult>) end;

  INSpeakableStringResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{C1C67BAE-C68B-4B30-BF29-5E9E70D9F569}']
    {class} function confirmationRequiredWithStringToConfirm(stringToConfirm: INSpeakableString): Pointer; cdecl;
    {class} function disambiguationWithStringsToDisambiguate(stringsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedString(resolvedString: INSpeakableString): Pointer; cdecl;
  end;

  INSpeakableStringResolutionResult = interface(INIntentResolutionResult)
    ['{B1A6D220-EF78-4757-B1C1-9AF54908DD9D}']
  end;
  TINSpeakableStringResolutionResult = class(TOCGenericImport<INSpeakableStringResolutionResultClass, INSpeakableStringResolutionResult>) end;

  INStringResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{D89F2DE8-245A-4C50-B15B-B94364230678}']
    {class} function confirmationRequiredWithStringToConfirm(stringToConfirm: NSString): Pointer; cdecl;
    {class} function disambiguationWithStringsToDisambiguate(stringsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedString(resolvedString: NSString): Pointer; cdecl;
  end;

  INStringResolutionResult = interface(INIntentResolutionResult)
    ['{32F9855A-86C7-4047-AAF8-A5CB9264A6CD}']
  end;
  TINStringResolutionResult = class(TOCGenericImport<INStringResolutionResultClass, INStringResolutionResult>) end;

  INTemperatureResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{932BB306-40FE-4E9B-B5A4-EB2AFB3C8ACB}']
    {class} function confirmationRequiredWithTemperatureToConfirm(temperatureToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithTemperaturesToDisambiguate(temperaturesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedTemperature(resolvedTemperature: NSMeasurement): Pointer; cdecl;
  end;

  INTemperatureResolutionResult = interface(INIntentResolutionResult)
    ['{29A348AD-D269-42CC-AFD2-3E0C68ECF9C8}']
  end;
  TINTemperatureResolutionResult = class(TOCGenericImport<INTemperatureResolutionResultClass, INTemperatureResolutionResult>) end;

  INDateComponentsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{D9B90AAD-1FEB-4BB1-BC73-20321C21E050}']
    {class} function confirmationRequiredWithDateComponentsToConfirm(dateComponentsToConfirm: NSDateComponents): Pointer; cdecl;
    {class} function disambiguationWithDateComponentsToDisambiguate(dateComponentsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedDateComponents(resolvedDateComponents: NSDateComponents): Pointer; cdecl;
  end;

  INDateComponentsResolutionResult = interface(INIntentResolutionResult)
    ['{E5D3CB92-C2BA-43B7-93BA-3F86E155FBC9}']
  end;
  TINDateComponentsResolutionResult = class(TOCGenericImport<INDateComponentsResolutionResultClass, INDateComponentsResolutionResult>) end;

  INRestaurantResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{2CDB32D3-6854-4A7D-9225-5D0F1310ABE8}']
    {class} function confirmationRequiredWithRestaurantToConfirm(restaurantToConfirm: INRestaurant): Pointer; cdecl;
    {class} function disambiguationWithRestaurantsToDisambiguate(restaurantsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedRestaurant(resolvedRestaurant: INRestaurant): Pointer; cdecl;
  end;

  INRestaurantResolutionResult = interface(INIntentResolutionResult)
    ['{CC57A20E-5793-460B-B76A-8D77BAA81AF8}']
  end;
  TINRestaurantResolutionResult = class(TOCGenericImport<INRestaurantResolutionResultClass, INRestaurantResolutionResult>) end;

  INRestaurantGuestResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{AE52E6B8-9002-43C4-84F7-AE78EFC11AFB}']
    {class} function confirmationRequiredWithRestaurantGuestToConfirm(restaurantGuestToConfirm: INRestaurantGuest): Pointer; cdecl;
    {class} function disambiguationWithRestaurantGuestsToDisambiguate(restaurantGuestsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedRestaurantGuest(resolvedRestaurantGuest: INRestaurantGuest): Pointer; cdecl;
  end;

  INRestaurantGuestResolutionResult = interface(INIntentResolutionResult)
    ['{A20D42E4-1C72-4A3D-9A61-B4C260F5032E}']
  end;
  TINRestaurantGuestResolutionResult = class(TOCGenericImport<INRestaurantGuestResolutionResultClass, INRestaurantGuestResolutionResult>) end;

  INURLResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{AFBD1A7B-8BEE-4D39-ACC7-2A11820D5A58}']
    {class} function confirmationRequiredWithURLToConfirm(urlToConfirm: NSURL): Pointer; cdecl;
    {class} function disambiguationWithURLsToDisambiguate(urlsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedURL(resolvedURL: NSURL): Pointer; cdecl;
  end;

  INURLResolutionResult = interface(INIntentResolutionResult)
    ['{E3BE944D-FF51-490D-A29F-52A4896CDA16}']
  end;
  TINURLResolutionResult = class(TOCGenericImport<INURLResolutionResultClass, INURLResolutionResult>) end;

  INLengthResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{A98E750A-0DB1-4BF0-9172-4466F4F96499}']
    {class} function confirmationRequiredWithLengthToConfirm(lengthToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithLengthsToDisambiguate(lengthsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedLength(resolvedLength: NSMeasurement): Pointer; cdecl;
  end;

  INLengthResolutionResult = interface(INIntentResolutionResult)
    ['{1DB177C8-DE2C-4CEE-B2F7-40AF96802C25}']
  end;
  TINLengthResolutionResult = class(TOCGenericImport<INLengthResolutionResultClass, INLengthResolutionResult>) end;

  INMassResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{7ACDBF64-0D07-43E7-83FB-C751348B327C}']
    {class} function confirmationRequiredWithMassToConfirm(massToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithMassToDisambiguate(massToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedMass(resolvedMass: NSMeasurement): Pointer; cdecl;
  end;

  INMassResolutionResult = interface(INIntentResolutionResult)
    ['{204DEC66-72E1-46B6-AC3E-A6BE02D87461}']
  end;
  TINMassResolutionResult = class(TOCGenericImport<INMassResolutionResultClass, INMassResolutionResult>) end;

  INVolumeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{B4C9643B-A943-4C5A-ACF7-3093B3BA69F0}']
    {class} function confirmationRequiredWithVolumeToConfirm(volumeToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithVolumeToDisambiguate(volumeToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedVolume(resolvedVolume: NSMeasurement): Pointer; cdecl;
  end;

  INVolumeResolutionResult = interface(INIntentResolutionResult)
    ['{667B32D1-CECC-4CAA-826E-1D9EAEC67E9D}']
  end;
  TINVolumeResolutionResult = class(TOCGenericImport<INVolumeResolutionResultClass, INVolumeResolutionResult>) end;

  INSpeedResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{FF69685A-EE79-4639-B3BE-35AF9BE82B7B}']
    {class} function confirmationRequiredWithSpeedToConfirm(speedToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithSpeedToDisambiguate(speedToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedSpeed(resolvedSpeed: NSMeasurement): Pointer; cdecl;
  end;

  INSpeedResolutionResult = interface(INIntentResolutionResult)
    ['{65E0B424-26FE-440F-8DFC-EBEBA063FA5B}']
  end;
  TINSpeedResolutionResult = class(TOCGenericImport<INSpeedResolutionResultClass, INSpeedResolutionResult>) end;

  INEnergyResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{6EBAD6D1-7AB7-4C17-9880-FFA9DE086AA7}']
    {class} function confirmationRequiredWithEnergyToConfirm(energyToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithEnergyToDisambiguate(energyToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedEnergy(resolvedEnergy: NSMeasurement): Pointer; cdecl;
  end;

  INEnergyResolutionResult = interface(INIntentResolutionResult)
    ['{02B0DAD6-9D48-4AF0-8FAD-29A14F12F75F}']
  end;
  TINEnergyResolutionResult = class(TOCGenericImport<INEnergyResolutionResultClass, INEnergyResolutionResult>) end;

  INEnumResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{1FAB10D7-6FA9-4FF1-9231-E849BB13BC21}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: NSInteger): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: NSInteger): Pointer; cdecl;
  end;

  INEnumResolutionResult = interface(INIntentResolutionResult)
    ['{628E7DAA-E51E-4292-8468-457E232FC73D}']
  end;
  TINEnumResolutionResult = class(TOCGenericImport<INEnumResolutionResultClass, INEnumResolutionResult>) end;

  INObjectResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{1E0E807B-B048-40C0-9B2C-91EBACB9A9DF}']
    {class} function confirmationRequiredWithObjectToConfirm(objectToConfirm: INObject): Pointer; cdecl;
    {class} function disambiguationWithObjectsToDisambiguate(objectsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedObject(resolvedObject: INObject): Pointer; cdecl;
  end;

  INObjectResolutionResult = interface(INIntentResolutionResult)
    ['{7F11E381-B98E-4D2E-915D-AA182C3D287D}']
  end;
  TINObjectResolutionResult = class(TOCGenericImport<INObjectResolutionResultClass, INObjectResolutionResult>) end;

  INTimeIntervalResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{417E442C-C5B1-4453-BDF0-94CD0D955DF3}']
    {class} function confirmationRequiredWithTimeIntervalToConfirm(timeIntervalToConfirm: NSTimeInterval): Pointer; cdecl;
    {class} function successWithResolvedTimeInterval(resolvedTimeInterval: NSTimeInterval): Pointer; cdecl;
  end;

  INTimeIntervalResolutionResult = interface(INIntentResolutionResult)
    ['{D344C555-21C9-4D99-87FC-F1B84E1C092B}']
  end;
  TINTimeIntervalResolutionResult = class(TOCGenericImport<INTimeIntervalResolutionResultClass, INTimeIntervalResolutionResult>) end;

  INMessageClass = interface(NSObjectClass)
    ['{AB4A1C9F-18C6-4185-85A3-C0F148D39A65}']
  end;

  INMessage = interface(NSObject)
    ['{DCA8E68A-FF77-4816-B1A1-1F29A8378F17}']
    function content: NSString; cdecl;
    function conversationIdentifier: NSString; cdecl;
    function dateSent: NSDate; cdecl;
    function groupName: INSpeakableString; cdecl;
    function identifier: NSString; cdecl;
    function initWithIdentifier(identifier: NSString; conversationIdentifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray; groupName: INSpeakableString; messageType: INMessageType; serviceName: NSString): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; conversationIdentifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray; groupName: INSpeakableString; messageType: INMessageType): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; conversationIdentifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray; messageType: INMessageType): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray): Pointer; overload; cdecl;
    function messageType: INMessageType; cdecl;
    function recipients: NSArray; cdecl;
    function sender: INPerson; cdecl;
    function serviceName: NSString; cdecl;
  end;
  TINMessage = class(TOCGenericImport<INMessageClass, INMessage>) end;

  INSendMessageAttachmentClass = interface(NSObjectClass)
    ['{F2F90DF2-7289-4AFF-8DED-04F46FC62541}']
    {class} function attachmentWithAudioMessageFile(audioMessageFile: INFile): INSendMessageAttachment; cdecl;
  end;

  INSendMessageAttachment = interface(NSObject)
    ['{5BEF52E1-2F3D-4AA4-8664-F2C0D9C7B3B1}']
    function audioMessageFile: INFile; cdecl;
  end;
  TINSendMessageAttachment = class(TOCGenericImport<INSendMessageAttachmentClass, INSendMessageAttachment>) end;

  INBalanceAmountClass = interface(NSObjectClass)
    ['{9ABA2EA9-7240-4938-A485-5A1863EDBC54}']
  end;

  INBalanceAmount = interface(NSObject)
    ['{5B40130C-37B9-49FD-B419-D50B14FFFC0F}']
    function amount: NSDecimalNumber; cdecl;
    function balanceType: INBalanceType; cdecl;
    function currencyCode: NSString; cdecl;
    function initWithAmount(amount: NSDecimalNumber; balanceType: INBalanceType): Pointer; overload; cdecl;
    function initWithAmount(amount: NSDecimalNumber; currencyCode: NSString): Pointer; overload; cdecl;
  end;
  TINBalanceAmount = class(TOCGenericImport<INBalanceAmountClass, INBalanceAmount>) end;

  INPriceRangeClass = interface(NSObjectClass)
    ['{56227661-F2EC-4F9C-A62E-485A5E51E719}']
  end;

  INPriceRange = interface(NSObject)
    ['{F945C9FE-9F4C-45E9-A342-FD5B690C9675}']
    function currencyCode: NSString; cdecl;
    function initWithMaximumPrice(maximumPrice: NSDecimalNumber; currencyCode: NSString): Pointer; cdecl;
    function initWithMinimumPrice(minimumPrice: NSDecimalNumber; currencyCode: NSString): Pointer; cdecl;
    function initWithPrice(price: NSDecimalNumber; currencyCode: NSString): Pointer; cdecl;
    function initWithRangeBetweenPrice(firstPrice: NSDecimalNumber; andPrice: NSDecimalNumber; currencyCode: NSString): Pointer; cdecl;
    function maximumPrice: NSDecimalNumber; cdecl;
    function minimumPrice: NSDecimalNumber; cdecl;
  end;
  TINPriceRange = class(TOCGenericImport<INPriceRangeClass, INPriceRange>) end;

  INRideOptionClass = interface(NSObjectClass)
    ['{10680D0D-F8D1-44A7-8897-4BF793F1EC28}']
  end;

  INRideOption = interface(NSObject)
    ['{09D7AF6C-6245-4555-9743-395C49996CB3}']
    function availablePartySizeOptions: NSArray; cdecl;
    function availablePartySizeOptionsSelectionPrompt: NSString; cdecl;
    function disclaimerMessage: NSString; cdecl;
    function estimatedPickupDate: NSDate; cdecl;
    function fareLineItems: NSArray; cdecl;
    function identifier: NSString; cdecl;
    function initWithCoder(decoder: NSCoder): Pointer; cdecl;
    function initWithName(name: NSString; estimatedPickupDate: NSDate): Pointer; cdecl;
    function name: NSString; cdecl;
    function priceRange: INPriceRange; cdecl;
    procedure setAvailablePartySizeOptions(availablePartySizeOptions: NSArray); cdecl;
    procedure setAvailablePartySizeOptionsSelectionPrompt(availablePartySizeOptionsSelectionPrompt: NSString); cdecl;
    procedure setDisclaimerMessage(disclaimerMessage: NSString); cdecl;
    procedure setEstimatedPickupDate(estimatedPickupDate: NSDate); cdecl;
    procedure setFareLineItems(fareLineItems: NSArray); cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setPriceRange(priceRange: INPriceRange); cdecl;
    procedure setSpecialPricing(specialPricing: NSString); cdecl;
    procedure setSpecialPricingBadgeImage(specialPricingBadgeImage: INImage); cdecl;
    procedure setUserActivityForBookingInApplication(userActivityForBookingInApplication: NSUserActivity); cdecl;
    procedure setUsesMeteredFare(usesMeteredFare: NSNumber); cdecl;
    function specialPricing: NSString; cdecl;
    function specialPricingBadgeImage: INImage; cdecl;
    function userActivityForBookingInApplication: NSUserActivity; cdecl;
    function usesMeteredFare: NSNumber; cdecl;
  end;
  TINRideOption = class(TOCGenericImport<INRideOptionClass, INRideOption>) end;

  INRideStatusClass = interface(NSObjectClass)
    ['{F90A771E-38A7-4A52-9E93-B1468B857122}']
  end;

  INRideStatus = interface(NSObject)
    ['{EBDA6289-8817-4966-967A-07AD6622A272}']
    function additionalActionActivities: NSArray; cdecl;
    function completionStatus: INRideCompletionStatus; cdecl;
    function driver: INRideDriver; cdecl;
    function dropOffLocation: CLPlacemark; cdecl;
    function estimatedDropOffDate: NSDate; cdecl;
    function estimatedPickupDate: NSDate; cdecl;
    function estimatedPickupEndDate: NSDate; cdecl;
    function phase: INRidePhase; cdecl;
    function pickupLocation: CLPlacemark; cdecl;
    function rideIdentifier: NSString; cdecl;
    function rideOption: INRideOption; cdecl;
    function scheduledPickupTime: INDateComponentsRange; cdecl;
    procedure setAdditionalActionActivities(additionalActionActivities: NSArray); cdecl;
    procedure setCompletionStatus(completionStatus: INRideCompletionStatus); cdecl;
    procedure setDriver(driver: INRideDriver); cdecl;
    procedure setDropOffLocation(dropOffLocation: CLPlacemark); cdecl;
    procedure setEstimatedDropOffDate(estimatedDropOffDate: NSDate); cdecl;
    procedure setEstimatedPickupDate(estimatedPickupDate: NSDate); cdecl;
    procedure setEstimatedPickupEndDate(estimatedPickupEndDate: NSDate); cdecl;
    procedure setPhase(phase: INRidePhase); cdecl;
    procedure setPickupLocation(pickupLocation: CLPlacemark); cdecl;
    procedure setRideIdentifier(rideIdentifier: NSString); cdecl;
    procedure setRideOption(rideOption: INRideOption); cdecl;
    procedure setScheduledPickupTime(scheduledPickupTime: INDateComponentsRange); cdecl;
    procedure setUserActivityForCancelingInApplication(userActivityForCancelingInApplication: NSUserActivity); cdecl;
    procedure setVehicle(vehicle: INRideVehicle); cdecl;
    procedure setWaypoints(waypoints: NSArray); cdecl;
    function userActivityForCancelingInApplication: NSUserActivity; cdecl;
    function vehicle: INRideVehicle; cdecl;
    function waypoints: NSArray; cdecl;
  end;
  TINRideStatus = class(TOCGenericImport<INRideStatusClass, INRideStatus>) end;

  INRideDriverClass = interface(INPersonClass)
    ['{BBDDF7EE-84A1-499E-9CB1-4AD2CC972A43}']
  end;

  INRideDriver = interface(INPerson)
    ['{34FCF69D-33A5-4FBE-BC30-799A0E578042}']
    function initWithHandle(handle: NSString; displayName: NSString; image: INImage; rating: NSString;
      phoneNumber: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.0))
    function initWithHandle(handle: NSString; nameComponents: NSPersonNameComponents; image: INImage; rating: NSString;
      phoneNumber: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.0))
    function initWithPersonHandle(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString; image: INImage;
      rating: NSString; phoneNumber: NSString): Pointer; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.2))
    function initWithPhoneNumber(phoneNumber: NSString; nameComponents: NSPersonNameComponents; displayName: NSString; image: INImage;
      rating: NSString): Pointer; cdecl;
    function phoneNumber: NSString; cdecl;
    function rating: NSString; cdecl;
  end;
  TINRideDriver = class(TOCGenericImport<INRideDriverClass, INRideDriver>) end;

  INRideVehicleClass = interface(NSObjectClass)
    ['{6D57EBD0-8D30-4ED7-B049-98243B4AEC32}']
  end;

  INRideVehicle = interface(NSObject)
    ['{26AB1ED3-49EA-4FA5-9730-152DC6AD3EA6}']
    function location: CLLocation; cdecl;
    function manufacturer: NSString; cdecl;
    function mapAnnotationImage: INImage; cdecl;
    function model: NSString; cdecl;
    function registrationPlate: NSString; cdecl;
    procedure setLocation(location: CLLocation); cdecl;
    procedure setManufacturer(manufacturer: NSString); cdecl;
    procedure setMapAnnotationImage(mapAnnotationImage: INImage); cdecl;
    procedure setModel(model: NSString); cdecl;
    procedure setRegistrationPlate(registrationPlate: NSString); cdecl;
  end;
  TINRideVehicle = class(TOCGenericImport<INRideVehicleClass, INRideVehicle>) end;

  INRideFareLineItemClass = interface(NSObjectClass)
    ['{B82B96D8-25ED-4091-B766-6E2196A99163}']
  end;

  INRideFareLineItem = interface(NSObject)
    ['{977AE3F1-0BF5-4CC9-980A-5AE69BCBB5C6}']
    function currencyCode: NSString; cdecl;
    function initWithTitle(title: NSString; price: NSDecimalNumber; currencyCode: NSString): Pointer; cdecl;
    function price: NSDecimalNumber; cdecl;
    function title: NSString; cdecl;
  end;
  TINRideFareLineItem = class(TOCGenericImport<INRideFareLineItemClass, INRideFareLineItem>) end;

  INRidePartySizeOptionClass = interface(NSObjectClass)
    ['{5F735B02-02BF-4D3E-8E10-5AB1051A22BE}']
  end;

  INRidePartySizeOption = interface(NSObject)
    ['{B4B00E60-68F7-4C0E-AA8E-DE54DF3C5BB2}']
    function initWithPartySizeRange(partySizeRange: NSRange; sizeDescription: NSString; priceRange: INPriceRange): Pointer; cdecl;
    function partySizeRange: NSRange; cdecl;
    function priceRange: INPriceRange; cdecl;
    function sizeDescription: NSString; cdecl;
  end;
  TINRidePartySizeOption = class(TOCGenericImport<INRidePartySizeOptionClass, INRidePartySizeOption>) end;

  INRideCompletionStatusClass = interface(NSObjectClass)
    ['{791E5D40-598A-46E1-9B8D-8DD7F8E33FA7}']
    {class} function canceledByService: Pointer; cdecl;
    {class} function canceledByUser: Pointer; cdecl;
    {class} function canceledMissedPickup: Pointer; cdecl;
    {class} function completed: Pointer; cdecl;
    {class} function completedWithOutstandingFeedbackType(feedbackType: INRideFeedbackTypeOptions): Pointer; cdecl;
    {class} function completedWithOutstandingPaymentAmount(outstandingPaymentAmount: INCurrencyAmount): Pointer; cdecl;
    {class} function completedWithSettledPaymentAmount(settledPaymentAmount: INCurrencyAmount): Pointer; cdecl;
  end;

  INRideCompletionStatus = interface(NSObject)
    ['{631A156B-77CE-434F-829C-3C988497BA99}']
    function completionUserActivity: NSUserActivity; cdecl;
    function defaultTippingOptions: NSSet; cdecl;
    function feedbackType: INRideFeedbackTypeOptions; cdecl;
    function isCanceled: Boolean; cdecl;
    function isCompleted: Boolean; cdecl;
    function isMissedPickup: Boolean; cdecl;
    function isOutstanding: Boolean; cdecl;
    function paymentAmount: INCurrencyAmount; cdecl;
    procedure setCompletionUserActivity(completionUserActivity: NSUserActivity); cdecl;
    procedure setDefaultTippingOptions(defaultTippingOptions: NSSet); cdecl;
  end;
  TINRideCompletionStatus = class(TOCGenericImport<INRideCompletionStatusClass, INRideCompletionStatus>) end;

  INReservationClass = interface(NSObjectClass)
    ['{AB4FE241-DAB6-46FD-BB99-BF260DCDE480}']
  end;

  INReservation = interface(NSObject)
    ['{5141E36E-D0FF-4762-857F-E280E1180D70}']
    function actions: NSArray; cdecl;
    function bookingTime: NSDate; cdecl;
    function itemReference: INSpeakableString; cdecl;
    function reservationHolderName: NSString; cdecl;
    function reservationNumber: NSString; cdecl;
    function reservationStatus: INReservationStatus; cdecl;
    [MethodName('URL')]
    function URL: NSURL; overload; cdecl;
    [MethodName('url')]
    function URL2: NSURL; overload; cdecl; // API_DEPRECATED("Use URL instead", ios(14.0, 14.0), macos(10.16, 11.0), watchos(7.0, 7.0))
  end;
  TINReservation = class(TOCGenericImport<INReservationClass, INReservation>) end;

  INReservationActionClass = interface(NSObjectClass)
    ['{BCAF6741-DB3F-4F91-9620-A099341B70B0}']
  end;

  INReservationAction = interface(NSObject)
    ['{03B67819-A050-421C-9E44-193D9213D19D}']
    [MethodName('type')]
    function &type: INReservationActionType; cdecl;
    function initWithType(&type: INReservationActionType; validDuration: INDateComponentsRange; userActivity: NSUserActivity): Pointer; cdecl;
    function userActivity: NSUserActivity; cdecl;
    function validDuration: INDateComponentsRange; cdecl;
  end;
  TINReservationAction = class(TOCGenericImport<INReservationActionClass, INReservationAction>) end;

  INFlightReservationClass = interface(INReservationClass)
    ['{CC3A8B89-9D10-4E64-91FA-898B63185478}']
  end;

  INFlightReservation = interface(INReservation)
    ['{716A781C-FF0D-48A8-B17C-2BAB9EF15EFB}']
    function flight: INFlight; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; reservedSeat: INSeat;
      flight: INFlight): Pointer; overload; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; reservedSeat: INSeat;
      flight: INFlight): Pointer; overload; cdecl;
    function reservedSeat: INSeat; cdecl;
  end;
  TINFlightReservation = class(TOCGenericImport<INFlightReservationClass, INFlightReservation>) end;

  INLodgingReservationClass = interface(INReservationClass)
    ['{46AE0E8C-25CE-423C-8433-54ECA9FB4E6A}']
  end;

  INLodgingReservation = interface(INReservation)
    ['{77C4D90F-4780-4717-B492-3D8306EAB32D}']
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; lodgingBusinessLocation: CLPlacemark;
      reservationDuration: INDateComponentsRange; numberOfAdults: NSNumber; numberOfChildren: NSNumber): Pointer; overload; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; lodgingBusinessLocation: CLPlacemark;
      reservationDuration: INDateComponentsRange; numberOfAdults: NSNumber; numberOfChildren: NSNumber): Pointer; overload; cdecl;
    function lodgingBusinessLocation: CLPlacemark; cdecl;
    function numberOfAdults: NSNumber; cdecl;
    function numberOfChildren: NSNumber; cdecl;
    function reservationDuration: INDateComponentsRange; cdecl;
  end;
  TINLodgingReservation = class(TOCGenericImport<INLodgingReservationClass, INLodgingReservation>) end;

  INRentalCarReservationClass = interface(INReservationClass)
    ['{502F6C4A-452A-416F-93CD-AE0FF8BDCF20}']
  end;

  INRentalCarReservation = interface(INReservation)
    ['{7D7CAC14-20B5-425A-AFAC-F7F0E890A49D}']
    function dropOffLocation: CLPlacemark; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; rentalCar: INRentalCar;
      rentalDuration: INDateComponentsRange; pickupLocation: CLPlacemark; dropOffLocation: CLPlacemark): Pointer; overload; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; rentalCar: INRentalCar;
      rentalDuration: INDateComponentsRange; pickupLocation: CLPlacemark; dropOffLocation: CLPlacemark): Pointer; overload; cdecl;
    function pickupLocation: CLPlacemark; cdecl;
    function rentalCar: INRentalCar; cdecl;
    function rentalDuration: INDateComponentsRange; cdecl;
  end;
  TINRentalCarReservation = class(TOCGenericImport<INRentalCarReservationClass, INRentalCarReservation>) end;

  INRestaurantReservationClass = interface(INReservationClass)
    ['{698D42D3-4E3F-4C2B-B11D-D749050788BE}']
  end;

  INRestaurantReservation = interface(INReservation)
    ['{E84BC428-02A5-4092-BCF9-A0A3CC284F0D}']
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; reservationDuration: INDateComponentsRange;
      partySize: NSNumber; restaurantLocation: CLPlacemark): Pointer; overload; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL;
      reservationDuration: INDateComponentsRange; partySize: NSNumber; restaurantLocation: CLPlacemark): Pointer; overload; cdecl;
    function partySize: NSNumber; cdecl;
    function reservationDuration: INDateComponentsRange; cdecl;
    function restaurantLocation: CLPlacemark; cdecl;
  end;
  TINRestaurantReservation = class(TOCGenericImport<INRestaurantReservationClass, INRestaurantReservation>) end;

  INTicketedEventReservationClass = interface(INReservationClass)
    ['{7707EE61-A862-47F5-8B77-91EA7E91ECE2}']
  end;

  INTicketedEventReservation = interface(INReservation)
    ['{F8BB0C3A-F373-4A4D-A5CA-F29DB764C00E}']
    function event: INTicketedEvent; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; reservedSeat: INSeat;
      event: INTicketedEvent): Pointer; overload; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; reservedSeat: INSeat;
      event: INTicketedEvent): Pointer; overload; cdecl;
    function reservedSeat: INSeat; cdecl;
  end;
  TINTicketedEventReservation = class(TOCGenericImport<INTicketedEventReservationClass, INTicketedEventReservation>) end;

  INTrainReservationClass = interface(INReservationClass)
    ['{238A4D18-1DF1-4566-85B2-9475B283C3E9}']
  end;

  INTrainReservation = interface(INReservation)
    ['{A048AC53-9866-48FD-AD3D-8A5AEEF8DAD2}']
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; reservedSeat: INSeat;
      trainTrip: INTrainTrip): Pointer; overload; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; reservedSeat: INSeat;
      trainTrip: INTrainTrip): Pointer; overload; cdecl;
    function reservedSeat: INSeat; cdecl;
    function trainTrip: INTrainTrip; cdecl;
  end;
  TINTrainReservation = class(TOCGenericImport<INTrainReservationClass, INTrainReservation>) end;

  INBusReservationClass = interface(INReservationClass)
    ['{E14FD9AC-3D84-4D6C-8E43-C2953BEEEE38}']
  end;

  INBusReservation = interface(INReservation)
    ['{AFE9CB25-A985-4CF0-A9F4-C3D3A351B2D0}']
    function busTrip: INBusTrip; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; reservedSeat: INSeat;
      busTrip: INBusTrip): Pointer; cdecl;
    function reservedSeat: INSeat; cdecl;
  end;
  TINBusReservation = class(TOCGenericImport<INBusReservationClass, INBusReservation>) end;

  INBoatReservationClass = interface(INReservationClass)
    ['{40E426DC-C168-4321-9CDC-E0928F06E54B}']
  end;

  INBoatReservation = interface(INReservation)
    ['{89F61700-0ADD-43EE-A5DD-0183259459E9}']
    function boatTrip: INBoatTrip; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; reservedSeat: INSeat;
      boatTrip: INBoatTrip): Pointer; cdecl;
    function reservedSeat: INSeat; cdecl;
  end;
  TINBoatReservation = class(TOCGenericImport<INBoatReservationClass, INBoatReservation>) end;

  INRestaurantGuestClass = interface(INPersonClass)
    ['{FB7F176F-F79E-46BE-9497-4290FEA0958B}']
  end;

  INRestaurantGuest = interface(INPerson)
    ['{FF1A5F59-B1EB-4A23-B3C9-D8C8F1DF5886}']
    function emailAddress: NSString; cdecl;
    function initWithNameComponents(nameComponents: NSPersonNameComponents; phoneNumber: NSString; emailAddress: NSString): Pointer; cdecl;
    function phoneNumber: NSString; cdecl;
    procedure setEmailAddress(emailAddress: NSString); cdecl;
    procedure setPhoneNumber(phoneNumber: NSString); cdecl;
  end;
  TINRestaurantGuest = class(TOCGenericImport<INRestaurantGuestClass, INRestaurantGuest>) end;

  INTermsAndConditionsClass = interface(NSObjectClass)
    ['{64B26BBF-F082-417B-B23F-E4996FBF4CCB}']
  end;

  INTermsAndConditions = interface(NSObject)
    ['{A3DF583B-A2CE-438C-8E95-1B7AFF6E7B5E}']
    function initWithLocalizedTermsAndConditionsText(localizedTermsAndConditionsText: NSString; privacyPolicyURL: NSURL;
      termsAndConditionsURL: NSURL): Pointer; cdecl;
    function localizedTermsAndConditionsText: NSString; cdecl;
    function privacyPolicyURL: NSURL; cdecl;
    function termsAndConditionsURL: NSURL; cdecl;
  end;
  TINTermsAndConditions = class(TOCGenericImport<INTermsAndConditionsClass, INTermsAndConditions>) end;

  INRestaurantGuestDisplayPreferencesClass = interface(NSObjectClass)
    ['{73EB29C5-FCD5-447F-9243-034766B4D5E9}']
  end;

  INRestaurantGuestDisplayPreferences = interface(NSObject)
    ['{302F226C-C874-486B-B8B4-0FF8ED23FF6D}']
    function emailAddressEditable: Boolean; cdecl;
    function emailAddressFieldShouldBeDisplayed: Boolean; cdecl;
    function nameEditable: Boolean; cdecl;
    function nameFieldFirstNameOptional: Boolean; cdecl;
    function nameFieldLastNameOptional: Boolean; cdecl;
    function nameFieldShouldBeDisplayed: Boolean; cdecl;
    function phoneNumberEditable: Boolean; cdecl;
    function phoneNumberFieldShouldBeDisplayed: Boolean; cdecl;
    procedure setEmailAddressEditable(emailAddressEditable: Boolean); cdecl;
    procedure setEmailAddressFieldShouldBeDisplayed(emailAddressFieldShouldBeDisplayed: Boolean); cdecl;
    procedure setNameEditable(nameEditable: Boolean); cdecl;
    procedure setNameFieldFirstNameOptional(nameFieldFirstNameOptional: Boolean); cdecl;
    procedure setNameFieldLastNameOptional(nameFieldLastNameOptional: Boolean); cdecl;
    procedure setNameFieldShouldBeDisplayed(nameFieldShouldBeDisplayed: Boolean); cdecl;
    procedure setPhoneNumberEditable(phoneNumberEditable: Boolean); cdecl;
    procedure setPhoneNumberFieldShouldBeDisplayed(phoneNumberFieldShouldBeDisplayed: Boolean); cdecl;
  end;
  TINRestaurantGuestDisplayPreferences = class(TOCGenericImport<INRestaurantGuestDisplayPreferencesClass, INRestaurantGuestDisplayPreferences>) end;

  INRestaurantClass = interface(NSObjectClass)
    ['{D350C568-5078-4520-AB11-129CEE7628D6}']
  end;

  INRestaurant = interface(NSObject)
    ['{86883415-0E10-4A03-8A00-2B21C654FAF6}']
    function initWithLocation(location: CLLocation; name: NSString; vendorIdentifier: NSString; restaurantIdentifier: NSString): Pointer; cdecl;
    function location: CLLocation; cdecl;
    function name: NSString; cdecl;
    function restaurantIdentifier: NSString; cdecl;
    procedure setLocation(location: CLLocation); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setRestaurantIdentifier(restaurantIdentifier: NSString); cdecl;
    procedure setVendorIdentifier(vendorIdentifier: NSString); cdecl;
    function vendorIdentifier: NSString; cdecl;
  end;
  TINRestaurant = class(TOCGenericImport<INRestaurantClass, INRestaurant>) end;

  INRestaurantOfferClass = interface(NSObjectClass)
    ['{DF162BAA-FB28-4554-9C66-18921C42056E}']
  end;

  INRestaurantOffer = interface(NSObject)
    ['{D168C20E-41C9-4BC0-88EC-D4EF7B995C9F}']
    function offerDetailText: NSString; cdecl;
    function offerIdentifier: NSString; cdecl;
    function offerTitleText: NSString; cdecl;
    procedure setOfferDetailText(offerDetailText: NSString); cdecl;
    procedure setOfferIdentifier(offerIdentifier: NSString); cdecl;
    procedure setOfferTitleText(offerTitleText: NSString); cdecl;
  end;
  TINRestaurantOffer = class(TOCGenericImport<INRestaurantOfferClass, INRestaurantOffer>) end;

  INRestaurantReservationBookingClass = interface(NSObjectClass)
    ['{1724B4C2-F278-4021-8557-83181486BF9B}']
  end;

  INRestaurantReservationBooking = interface(NSObject)
    ['{8C357ABD-EC69-40BA-909F-7D206810B534}']
    function bookingDate: NSDate; cdecl;
    function bookingDescription: NSString; cdecl;
    function bookingIdentifier: NSString; cdecl;
    function initWithRestaurant(restaurant: INRestaurant; bookingDate: NSDate; partySize: NSUInteger; bookingIdentifier: NSString): Pointer; cdecl;
    function isBookingAvailable: Boolean; cdecl;
    function offers: NSArray; cdecl;
    function partySize: NSUInteger; cdecl;
    function requiresEmailAddress: Boolean; cdecl;
    function requiresManualRequest: Boolean; cdecl;
    function requiresName: Boolean; cdecl;
    function requiresPhoneNumber: Boolean; cdecl;
    function restaurant: INRestaurant; cdecl;
    procedure setBookingAvailable(bookingAvailable: Boolean); cdecl;
    procedure setBookingDate(bookingDate: NSDate); cdecl;
    procedure setBookingDescription(bookingDescription: NSString); cdecl;
    procedure setBookingIdentifier(bookingIdentifier: NSString); cdecl;
    procedure setOffers(offers: NSArray); cdecl;
    procedure setPartySize(partySize: NSUInteger); cdecl;
    procedure setRequiresEmailAddress(requiresEmailAddress: Boolean); cdecl;
    procedure setRequiresManualRequest(requiresManualRequest: Boolean); cdecl;
    procedure setRequiresName(requiresName: Boolean); cdecl;
    procedure setRequiresPhoneNumber(requiresPhoneNumber: Boolean); cdecl;
    procedure setRestaurant(restaurant: INRestaurant); cdecl;
  end;
  TINRestaurantReservationBooking = class(TOCGenericImport<INRestaurantReservationBookingClass, INRestaurantReservationBooking>) end;

  INRestaurantReservationUserBookingClass = interface(INRestaurantReservationBookingClass)
    ['{775950C5-6B26-412D-A374-C94C2B906404}']
  end;

  INRestaurantReservationUserBooking = interface(INRestaurantReservationBooking)
    ['{0E9B19AA-BCDE-4A92-AF34-C2DCCBA7264A}']
    function advisementText: NSString; cdecl;
    function dateStatusModified: NSDate; cdecl;
    function guest: INRestaurantGuest; cdecl;
    function guestProvidedSpecialRequestText: NSString; cdecl;
    function initWithRestaurant(restaurant: INRestaurant; bookingDate: NSDate; partySize: NSUInteger; bookingIdentifier: NSString;
      guest: INRestaurantGuest; status: INRestaurantReservationUserBookingStatus; dateStatusModified: NSDate): Pointer; cdecl;
    function selectedOffer: INRestaurantOffer; cdecl;
    procedure setAdvisementText(advisementText: NSString); cdecl;
    procedure setDateStatusModified(dateStatusModified: NSDate); cdecl;
    procedure setGuest(guest: INRestaurantGuest); cdecl;
    procedure setGuestProvidedSpecialRequestText(guestProvidedSpecialRequestText: NSString); cdecl;
    procedure setSelectedOffer(selectedOffer: INRestaurantOffer); cdecl;
    procedure setStatus(status: INRestaurantReservationUserBookingStatus); cdecl;
    function status: INRestaurantReservationUserBookingStatus; cdecl;
  end;
  TINRestaurantReservationUserBooking = class(TOCGenericImport<INRestaurantReservationUserBookingClass, INRestaurantReservationUserBooking>) end;

  INBookRestaurantReservationIntentClass = interface(INIntentClass)
    ['{554B1EF1-DE82-467E-A929-84E6BF41F861}']
  end;

  INBookRestaurantReservationIntent = interface(INIntent)
    ['{E42269E0-60E8-4C75-93C2-8FF616B65309}']
    function bookingDateComponents: NSDateComponents; cdecl;
    function bookingIdentifier: NSString; cdecl;
    function guest: INRestaurantGuest; cdecl;
    function guestProvidedSpecialRequestText: NSString; cdecl;
    function initWithRestaurant(restaurant: INRestaurant; bookingDateComponents: NSDateComponents; partySize: NSUInteger; bookingIdentifier: NSString;
      guest: INRestaurantGuest; selectedOffer: INRestaurantOffer; guestProvidedSpecialRequestText: NSString): Pointer; cdecl;
    function partySize: NSUInteger; cdecl;
    function restaurant: INRestaurant; cdecl;
    function selectedOffer: INRestaurantOffer; cdecl;
    procedure setBookingDateComponents(bookingDateComponents: NSDateComponents); cdecl;
    procedure setBookingIdentifier(bookingIdentifier: NSString); cdecl;
    procedure setGuest(guest: INRestaurantGuest); cdecl;
    procedure setGuestProvidedSpecialRequestText(guestProvidedSpecialRequestText: NSString); cdecl;
    procedure setPartySize(partySize: NSUInteger); cdecl;
    procedure setRestaurant(restaurant: INRestaurant); cdecl;
    procedure setSelectedOffer(selectedOffer: INRestaurantOffer); cdecl;
  end;
  TINBookRestaurantReservationIntent = class(TOCGenericImport<INBookRestaurantReservationIntentClass, INBookRestaurantReservationIntent>) end;

  INBookRestaurantReservationIntentHandling = interface(IObjectiveC)
    ['{CD5B4246-E7DB-4DE0-8E13-A7957EBDA23A}']
    procedure confirmBookRestaurantReservation(intent: INBookRestaurantReservationIntent;
      completion: TINBookRestaurantReservationIntentHandlingBlockMethod1); cdecl;
    procedure handleBookRestaurantReservation(intent: INBookRestaurantReservationIntent;
      completion: TINBookRestaurantReservationIntentHandlingBlockMethod1); cdecl;
    procedure resolveBookingDateComponentsForBookRestaurantReservation(intent: INBookRestaurantReservationIntent;
      withCompletion: TINBookRestaurantReservationIntentHandlingBlockMethod3); cdecl;
    procedure resolveGuestForBookRestaurantReservation(intent: INBookRestaurantReservationIntent;
      withCompletion: TINBookRestaurantReservationIntentHandlingBlockMethod5); cdecl;
    procedure resolveGuestProvidedSpecialRequestTextForBookRestaurantReservation(intent: INBookRestaurantReservationIntent;
      withCompletion: TINBookRestaurantReservationIntentHandlingBlockMethod6); cdecl;
    procedure resolvePartySizeForBookRestaurantReservation(intent: INBookRestaurantReservationIntent;
      withCompletion: TINBookRestaurantReservationIntentHandlingBlockMethod4); cdecl;
    procedure resolveRestaurantForBookRestaurantReservation(intent: INBookRestaurantReservationIntent;
      withCompletion: TINBookRestaurantReservationIntentHandlingBlockMethod2); cdecl;
  end;

  INGetAvailableRestaurantReservationBookingsIntentClass = interface(INIntentClass)
    ['{74C345AB-9592-4C7D-9F04-3E67DE566B45}']
  end;

  INGetAvailableRestaurantReservationBookingsIntent = interface(INIntent)
    ['{D294A208-906E-460A-80F2-717B79A5CC01}']
    function earliestBookingDateForResults: NSDate; cdecl;
    function initWithRestaurant(restaurant: INRestaurant; partySize: NSUInteger; preferredBookingDateComponents: NSDateComponents;
      maximumNumberOfResults: NSNumber; earliestBookingDateForResults: NSDate; latestBookingDateForResults: NSDate): Pointer; cdecl;
    function latestBookingDateForResults: NSDate; cdecl;
    function maximumNumberOfResults: NSNumber; cdecl;
    function partySize: NSUInteger; cdecl;
    function preferredBookingDateComponents: NSDateComponents; cdecl;
    function restaurant: INRestaurant; cdecl;
    procedure setEarliestBookingDateForResults(earliestBookingDateForResults: NSDate); cdecl;
    procedure setLatestBookingDateForResults(latestBookingDateForResults: NSDate); cdecl;
    procedure setMaximumNumberOfResults(maximumNumberOfResults: NSNumber); cdecl;
    procedure setPartySize(partySize: NSUInteger); cdecl;
    procedure setPreferredBookingDateComponents(preferredBookingDateComponents: NSDateComponents); cdecl;
    procedure setRestaurant(restaurant: INRestaurant); cdecl;
  end;
  TINGetAvailableRestaurantReservationBookingsIntent = class(TOCGenericImport<INGetAvailableRestaurantReservationBookingsIntentClass,
    INGetAvailableRestaurantReservationBookingsIntent>) end;

  INGetAvailableRestaurantReservationBookingsIntentHandling = interface(IObjectiveC)
    ['{0044AC27-6B34-455B-A366-60640635E8AD}']
    procedure confirmGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent;
      completion: TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod1); cdecl;
    procedure handleGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent;
      completion: TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod1); cdecl;
    procedure resolvePartySizeForGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent;
      withCompletion: TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod3); cdecl;
    procedure resolvePreferredBookingDateComponentsForGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent;
      withCompletion: TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod4); cdecl;
    procedure resolveRestaurantForGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent;
      withCompletion: TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod2); cdecl;
  end;

  INGetUserCurrentRestaurantReservationBookingsIntentClass = interface(INIntentClass)
    ['{FF745430-6F00-4750-97DB-D89BB7F81B18}']
  end;

  INGetUserCurrentRestaurantReservationBookingsIntent = interface(INIntent)
    ['{93251F53-A63E-4725-A494-ACABF12D9D16}']
    function earliestBookingDateForResults: NSDate; cdecl;
    function initWithRestaurant(restaurant: INRestaurant; reservationIdentifier: NSString; maximumNumberOfResults: NSNumber;
      earliestBookingDateForResults: NSDate): Pointer; cdecl;
    function maximumNumberOfResults: NSNumber; cdecl;
    function reservationIdentifier: NSString; cdecl;
    function restaurant: INRestaurant; cdecl;
    procedure setEarliestBookingDateForResults(earliestBookingDateForResults: NSDate); cdecl;
    procedure setMaximumNumberOfResults(maximumNumberOfResults: NSNumber); cdecl;
    procedure setReservationIdentifier(reservationIdentifier: NSString); cdecl;
    procedure setRestaurant(restaurant: INRestaurant); cdecl;
  end;
  TINGetUserCurrentRestaurantReservationBookingsIntent = class(TOCGenericImport<INGetUserCurrentRestaurantReservationBookingsIntentClass,
    INGetUserCurrentRestaurantReservationBookingsIntent>) end;

  INGetUserCurrentRestaurantReservationBookingsIntentHandling = interface(IObjectiveC)
    ['{678E13D5-6C1F-4DEB-A8EF-6EF0694DD7E7}']
    procedure confirmGetUserCurrentRestaurantReservationBookings(intent: INGetUserCurrentRestaurantReservationBookingsIntent;
      completion: TINGetUserCurrentRestaurantReservationBookingsIntentHandlingBlockMethod1); cdecl;
    procedure handleGetUserCurrentRestaurantReservationBookings(intent: INGetUserCurrentRestaurantReservationBookingsIntent;
      completion: TINGetUserCurrentRestaurantReservationBookingsIntentHandlingBlockMethod1); cdecl;
    procedure resolveRestaurantForGetUserCurrentRestaurantReservationBookings(intent: INGetUserCurrentRestaurantReservationBookingsIntent;
      withCompletion: TINGetUserCurrentRestaurantReservationBookingsIntentHandlingBlockMethod2); cdecl;
  end;

  INGetAvailableRestaurantReservationBookingDefaultsIntentClass = interface(INIntentClass)
    ['{041673E7-3FC8-48A8-92D6-4EBFC81F92EC}']
  end;

  INGetAvailableRestaurantReservationBookingDefaultsIntent = interface(INIntent)
    ['{0615A0AA-8F4F-4BC3-B686-EBF503413D42}']
    function initWithRestaurant(restaurant: INRestaurant): Pointer; cdecl;
    function restaurant: INRestaurant; cdecl;
    procedure setRestaurant(restaurant: INRestaurant); cdecl;
  end;
  TINGetAvailableRestaurantReservationBookingDefaultsIntent = class(TOCGenericImport<INGetAvailableRestaurantReservationBookingDefaultsIntentClass,
    INGetAvailableRestaurantReservationBookingDefaultsIntent>) end;

  INGetAvailableRestaurantReservationBookingDefaultsIntentHandling = interface(IObjectiveC)
    ['{F9024E01-703C-4A49-A229-76ED27FAEF95}']
    procedure confirmGetAvailableRestaurantReservationBookingDefaults(intent: INGetAvailableRestaurantReservationBookingDefaultsIntent;
      completion: TINGetAvailableRestaurantReservationBookingDefaultsIntentHandlingBlockMethod1); cdecl;
    procedure handleGetAvailableRestaurantReservationBookingDefaults(intent: INGetAvailableRestaurantReservationBookingDefaultsIntent;
      completion: TINGetAvailableRestaurantReservationBookingDefaultsIntentHandlingBlockMethod1); cdecl;
    procedure resolveRestaurantForGetAvailableRestaurantReservationBookingDefaults(intent: INGetAvailableRestaurantReservationBookingDefaultsIntent;
      withCompletion: TINGetAvailableRestaurantReservationBookingDefaultsIntentHandlingBlockMethod2); cdecl;
  end;

  INBookRestaurantReservationIntentResponseClass = interface(INIntentResponseClass)
    ['{BFF7D1C7-A532-4FF9-8E54-0A29FB2688AF}']
  end;

  INBookRestaurantReservationIntentResponse = interface(INIntentResponse)
    ['{073D855B-AE15-45EC-9197-19C7426DD4DC}']
    function code: INBookRestaurantReservationIntentCode; cdecl;
    function initWithCode(code: INBookRestaurantReservationIntentCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setUserBooking(userBooking: INRestaurantReservationUserBooking); cdecl;
    function userBooking: INRestaurantReservationUserBooking; cdecl;
  end;
  TINBookRestaurantReservationIntentResponse = class(TOCGenericImport<INBookRestaurantReservationIntentResponseClass,
    INBookRestaurantReservationIntentResponse>) end;

  INGetAvailableRestaurantReservationBookingsIntentResponseClass = interface(INIntentResponseClass)
    ['{669AF24D-B0C9-44F7-B9D4-7DEF9FC899E4}']
  end;

  INGetAvailableRestaurantReservationBookingsIntentResponse = interface(INIntentResponse)
    ['{7A88179E-F8B6-4F06-835D-632A73BE8399}']
    function availableBookings: NSArray; cdecl;
    function code: INGetAvailableRestaurantReservationBookingsIntentCode; cdecl;
    function initWithAvailableBookings(availableBookings: NSArray; code: INGetAvailableRestaurantReservationBookingsIntentCode;
      userActivity: NSUserActivity): Pointer; cdecl;
    function localizedBookingAdvisementText: NSString; cdecl;
    function localizedRestaurantDescriptionText: NSString; cdecl;
    procedure setLocalizedBookingAdvisementText(localizedBookingAdvisementText: NSString); cdecl;
    procedure setLocalizedRestaurantDescriptionText(localizedRestaurantDescriptionText: NSString); cdecl;
    procedure setTermsAndConditions(termsAndConditions: INTermsAndConditions); cdecl;
    function termsAndConditions: INTermsAndConditions; cdecl;
  end;
  TINGetAvailableRestaurantReservationBookingsIntentResponse = class(TOCGenericImport<INGetAvailableRestaurantReservationBookingsIntentResponseClass,
    INGetAvailableRestaurantReservationBookingsIntentResponse>) end;

  INGetUserCurrentRestaurantReservationBookingsIntentResponseClass = interface(INIntentResponseClass)
    ['{54DA9552-4CF1-4A54-923C-FCF9C94513D5}']
  end;

  INGetUserCurrentRestaurantReservationBookingsIntentResponse = interface(INIntentResponse)
    ['{206E25F1-26F1-4BD3-833A-287C4BE20F25}']
    function code: INGetUserCurrentRestaurantReservationBookingsIntentResponseCode; cdecl;
    function initWithUserCurrentBookings(userCurrentBookings: NSArray; code: INGetUserCurrentRestaurantReservationBookingsIntentResponseCode;
      userActivity: NSUserActivity): Pointer; cdecl;
    procedure setUserCurrentBookings(userCurrentBookings: NSArray); cdecl;
    function userCurrentBookings: NSArray; cdecl;
  end;
  TINGetUserCurrentRestaurantReservationBookingsIntentResponse = class(TOCGenericImport<INGetUserCurrentRestaurantReservationBookingsIntentResponseClass,
    INGetUserCurrentRestaurantReservationBookingsIntentResponse>) end;

  INGetAvailableRestaurantReservationBookingDefaultsIntentResponseClass = interface(INIntentResponseClass)
    ['{F838D58A-7551-4095-BF45-8F8889518065}']
  end;

  INGetAvailableRestaurantReservationBookingDefaultsIntentResponse = interface(INIntentResponse)
    ['{C89F5B83-3910-4A3C-99A9-BD51CA0A6FC1}']
    function code: INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode; cdecl;
    function defaultBookingDate: NSDate; cdecl;
    function defaultPartySize: NSUInteger; cdecl;
    function initWithDefaultPartySize(defaultPartySize: NSUInteger; defaultBookingDate: NSDate;
      code: INGetAvailableRestaurantReservationBookingDefaultsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function maximumPartySize: NSNumber; cdecl;
    function minimumPartySize: NSNumber; cdecl;
    function providerImage: INImage; cdecl;
    procedure setMaximumPartySize(maximumPartySize: NSNumber); cdecl;
    procedure setMinimumPartySize(minimumPartySize: NSNumber); cdecl;
    procedure setProviderImage(providerImage: INImage); cdecl;
  end;
  TINGetAvailableRestaurantReservationBookingDefaultsIntentResponse = class(TOCGenericImport<INGetAvailableRestaurantReservationBookingDefaultsIntentResponseClass,
    INGetAvailableRestaurantReservationBookingDefaultsIntentResponse>) end;

  INGetRestaurantGuestIntentClass = interface(INIntentClass)
    ['{DFC9A4F5-1ECA-484D-BE99-65E885DFC238}']
  end;

  INGetRestaurantGuestIntent = interface(INIntent)
    ['{3949BD95-5209-4A45-AB6A-DA16E0438FD1}']
  end;
  TINGetRestaurantGuestIntent = class(TOCGenericImport<INGetRestaurantGuestIntentClass, INGetRestaurantGuestIntent>) end;

  INGetRestaurantGuestIntentHandling = interface(IObjectiveC)
    ['{098CE933-5C28-4BEA-A070-303B2830BC7D}']
    procedure confirmGetRestaurantGuest(guestIntent: INGetRestaurantGuestIntent; completion: TINGetRestaurantGuestIntentHandlingBlockMethod1); cdecl;
    procedure handleGetRestaurantGuest(intent: INGetRestaurantGuestIntent; completion: TINGetRestaurantGuestIntentHandlingBlockMethod1); cdecl;
  end;

  INGetRestaurantGuestIntentResponseClass = interface(INIntentResponseClass)
    ['{57120CB1-4A2A-4175-AA72-8E0E3B6D2E6E}']
  end;

  INGetRestaurantGuestIntentResponse = interface(INIntentResponse)
    ['{419F316E-F42E-46A3-9D0C-A38969800400}']
    function code: INGetRestaurantGuestIntentResponseCode; cdecl;
    function guest: INRestaurantGuest; cdecl;
    function guestDisplayPreferences: INRestaurantGuestDisplayPreferences; cdecl;
    function initWithCode(code: INGetRestaurantGuestIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setGuest(guest: INRestaurantGuest); cdecl;
    procedure setGuestDisplayPreferences(guestDisplayPreferences: INRestaurantGuestDisplayPreferences); cdecl;
  end;
  TINGetRestaurantGuestIntentResponse = class(TOCGenericImport<INGetRestaurantGuestIntentResponseClass, INGetRestaurantGuestIntentResponse>) end;

  INVocabularyClass = interface(NSObjectClass)
    ['{47EAAA25-FBAF-46EF-A144-8D1D50F53FFE}']
    {class} function sharedVocabulary: Pointer; cdecl;
  end;

  INVocabulary = interface(NSObject)
    ['{6807BEF3-FDE2-4489-9AC8-37099388FE88}']
    procedure removeAllVocabularyStrings; cdecl;
    procedure setVocabulary(vocabulary: NSOrderedSet; ofType: INVocabularyStringType); cdecl;
    procedure setVocabularyStrings(vocabulary: NSOrderedSet; ofType: INVocabularyStringType); cdecl;
  end;
  TINVocabulary = class(TOCGenericImport<INVocabularyClass, INVocabulary>) end;

  INUpcomingMediaManagerClass = interface(NSObjectClass)
    ['{8E2F0F33-8E79-47FF-8C6D-5BDCBCF41B64}']
    {class} function sharedManager: INUpcomingMediaManager; cdecl;
  end;

  INUpcomingMediaManager = interface(NSObject)
    ['{4925AC5A-F693-4A70-A7A4-7C5CADC982A4}']
    procedure setPredictionMode(mode: INUpcomingMediaPredictionMode; forType: INMediaItemType); cdecl;
    procedure setSuggestedMediaIntents(intents: NSOrderedSet); cdecl;
  end;
  TINUpcomingMediaManager = class(TOCGenericImport<INUpcomingMediaManagerClass, INUpcomingMediaManager>) end;

  INPreferencesClass = interface(NSObjectClass)
    ['{E3CF7093-B287-4A9F-8BD3-3F2C4C3E83F9}']
    {class} procedure requestSiriAuthorization(handler: TINPreferencesBlockMethod1); cdecl;
    {class} function siriAuthorizationStatus: INSiriAuthorizationStatus; cdecl;
    {class} function siriLanguageCode: NSString; cdecl;
  end;

  INPreferences = interface(NSObject)
    ['{1EC8AA9B-2A15-474C-89B3-17B09DC136A8}']
  end;
  TINPreferences = class(TOCGenericImport<INPreferencesClass, INPreferences>) end;

  INUserContextClass = interface(NSObjectClass)
    ['{F80DE226-CCE3-4981-99C9-B86C16DF29B5}']
  end;

  INUserContext = interface(NSObject)
    ['{D9D01F18-F295-4582-8E27-D5C7C7A72B33}']
    procedure becomeCurrent; cdecl;
  end;
  TINUserContext = class(TOCGenericImport<INUserContextClass, INUserContext>) end;

  INMediaUserContextClass = interface(INUserContextClass)
    ['{03E06D66-6750-4D59-811C-1FDA891C3B3A}']
  end;

  INMediaUserContext = interface(INUserContext)
    ['{EC1AF574-D9F6-4CC0-9F24-627CE8781BA7}']
    function numberOfLibraryItems: NSNumber; cdecl;
    procedure setNumberOfLibraryItems(numberOfLibraryItems: NSNumber); cdecl;
    procedure setSubscriptionStatus(subscriptionStatus: INMediaUserContextSubscriptionStatus); cdecl;
    function subscriptionStatus: INMediaUserContextSubscriptionStatus; cdecl;
  end;
  TINMediaUserContext = class(TOCGenericImport<INMediaUserContextClass, INMediaUserContext>) end;

  INNoteContentClass = interface(NSObjectClass)
    ['{4C2A0E30-A64B-48B2-84D9-953AABBBC223}']
  end;

  INNoteContent = interface(NSObject)
    ['{ADBE2432-EF1D-4B02-BF43-BAF81D5FB7E2}']
  end;
  TINNoteContent = class(TOCGenericImport<INNoteContentClass, INNoteContent>) end;

  INTextNoteContentClass = interface(INNoteContentClass)
    ['{FC852D54-B3BB-4D6B-9CC0-6389B73C960A}']
  end;

  INTextNoteContent = interface(INNoteContent)
    ['{E892A564-57F1-4C8D-B875-53F614702461}']
    function initWithText(text: NSString): Pointer; cdecl;
    function text: NSString; cdecl;
  end;
  TINTextNoteContent = class(TOCGenericImport<INTextNoteContentClass, INTextNoteContent>) end;

  INImageNoteContentClass = interface(INNoteContentClass)
    ['{E6CF95B2-7ED7-4872-A880-FA1D407B57F9}']
  end;

  INImageNoteContent = interface(INNoteContent)
    ['{6DFDE172-4EA5-4865-8D27-F2B3DC842E00}']
    function image: INImage; cdecl;
    function initWithImage(image: INImage): Pointer; cdecl;
  end;
  TINImageNoteContent = class(TOCGenericImport<INImageNoteContentClass, INImageNoteContent>) end;

  INRelevanceProviderClass = interface(NSObjectClass)
    ['{9521F9DC-9F23-40AA-8387-B624EF9B91C7}']
  end;

  INRelevanceProvider = interface(NSObject)
    ['{2CDEF496-23E3-4F03-A351-796F8D9D4021}']
  end;
  TINRelevanceProvider = class(TOCGenericImport<INRelevanceProviderClass, INRelevanceProvider>) end;

  INDateRelevanceProviderClass = interface(INRelevanceProviderClass)
    ['{2B863DD7-B314-4941-9C6F-43BD2B78BB1D}']
  end;

  INDateRelevanceProvider = interface(INRelevanceProvider)
    ['{7B4A40A2-2A61-4FDF-BD99-618FF2E88C90}']
    function endDate: NSDate; cdecl;
    function initWithStartDate(startDate: NSDate; endDate: NSDate): Pointer; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TINDateRelevanceProvider = class(TOCGenericImport<INDateRelevanceProviderClass, INDateRelevanceProvider>) end;

  INLocationRelevanceProviderClass = interface(INRelevanceProviderClass)
    ['{AED2B295-0C11-42A2-88D2-F3038D7C5DFF}']
  end;

  INLocationRelevanceProvider = interface(INRelevanceProvider)
    ['{C983B2AB-F68C-4E34-A6FC-DA022F517AF1}']
    function initWithRegion(region: CLRegion): Pointer; cdecl;
    function region: CLRegion; cdecl;
  end;
  TINLocationRelevanceProvider = class(TOCGenericImport<INLocationRelevanceProviderClass, INLocationRelevanceProvider>) end;

  INDailyRoutineRelevanceProviderClass = interface(INRelevanceProviderClass)
    ['{04929DB7-0EA8-45E6-A5FF-99573EFD9DCB}']
  end;

  INDailyRoutineRelevanceProvider = interface(INRelevanceProvider)
    ['{43A91406-FC47-4F7B-BD65-9D9C4D1B9E98}']
    function initWithSituation(situation: INDailyRoutineSituation): Pointer; cdecl;
    function situation: INDailyRoutineSituation; cdecl;
  end;
  TINDailyRoutineRelevanceProvider = class(TOCGenericImport<INDailyRoutineRelevanceProviderClass, INDailyRoutineRelevanceProvider>) end;

  INDefaultCardTemplateClass = interface(NSObjectClass)
    ['{708DCC63-8460-4535-AFAE-674EB83CE3B9}']
  end;

  INDefaultCardTemplate = interface(NSObject)
    ['{F2C1312F-7F10-4577-B12E-DB81C8941CAF}']
    function image: INImage; cdecl;
    function initWithTitle(title: NSString): Pointer; cdecl;
    procedure setImage(image: INImage); cdecl;
    procedure setSubtitle(subtitle: NSString); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function subtitle: NSString; cdecl;
    function title: NSString; cdecl;
  end;
  TINDefaultCardTemplate = class(TOCGenericImport<INDefaultCardTemplateClass, INDefaultCardTemplate>) end;

  INShortcutClass = interface(NSObjectClass)
    ['{1775CB23-5E6E-4387-8333-49FF0F55C09D}']
    {class} function new: Pointer; cdecl;
  end;

  INShortcut = interface(NSObject)
    ['{700BCC03-BC58-4531-A402-B56473256F2C}']
    function initWithIntent(intent: INIntent): Pointer; cdecl;
    function initWithUserActivity(userActivity: NSUserActivity): Pointer; cdecl;
    function intent: INIntent; cdecl;
    function userActivity: NSUserActivity; cdecl;
  end;
  TINShortcut = class(TOCGenericImport<INShortcutClass, INShortcut>) end;

  INRelevantShortcutClass = interface(NSObjectClass)
    ['{876FC156-82A4-4716-9546-AE70E2F936A5}']
  end;

  INRelevantShortcut = interface(NSObject)
    ['{C59AE168-9DBE-4E3B-A720-B621EC705E66}']
    function initWithShortcut(shortcut: INShortcut): Pointer; cdecl;
    function relevanceProviders: NSArray; cdecl;
    procedure setRelevanceProviders(relevanceProviders: NSArray); cdecl;
    procedure setShortcutRole(shortcutRole: INRelevantShortcutRole); cdecl;
    procedure setWatchTemplate(watchTemplate: INDefaultCardTemplate); cdecl;
    function shortcut: INShortcut; cdecl;
    function shortcutRole: INRelevantShortcutRole; cdecl;
    function watchTemplate: INDefaultCardTemplate; cdecl;
  end;
  TINRelevantShortcut = class(TOCGenericImport<INRelevantShortcutClass, INRelevantShortcut>) end;

  INRelevantShortcutStoreClass = interface(NSObjectClass)
    ['{AA134637-F1C7-412B-84F1-6557B0CA1EDD}']
    {class} function defaultStore: INRelevantShortcutStore; cdecl;
  end;

  INRelevantShortcutStore = interface(NSObject)
    ['{E5D5A3F3-8535-4FD5-B29C-D279AB9ACCD5}']
    procedure setRelevantShortcuts(shortcuts: NSArray; completionHandler: TINRelevantShortcutStoreBlockMethod1); cdecl;
  end;
  TINRelevantShortcutStore = class(TOCGenericImport<INRelevantShortcutStoreClass, INRelevantShortcutStore>) end;

  INVoiceShortcutClass = interface(NSObjectClass)
    ['{BCD3D74F-63A1-4214-A658-940D9D396F00}']
    {class} function new: Pointer; cdecl;
  end;

  INVoiceShortcut = interface(NSObject)
    ['{6B9FEF15-8D6B-4B0F-8489-15FF606A773A}']
    function identifier: NSUUID; cdecl;
    function invocationPhrase: NSString; cdecl;
    function shortcut: INShortcut; cdecl;
  end;
  TINVoiceShortcut = class(TOCGenericImport<INVoiceShortcutClass, INVoiceShortcut>) end;

  INVoiceShortcutCenterClass = interface(NSObjectClass)
    ['{ACF9EC12-9BD0-4DBE-9470-7754F811BD52}']
    {class} function new: Pointer; cdecl;
    {class} function sharedCenter: INVoiceShortcutCenter; cdecl;
  end;

  INVoiceShortcutCenter = interface(NSObject)
    ['{444C718D-C383-418E-A1AA-B9D9318B261F}']
    procedure getAllVoiceShortcutsWithCompletion(completionHandler: TINVoiceShortcutCenterBlockMethod1); cdecl;
    procedure getVoiceShortcutWithIdentifier(identifier: NSUUID; completion: TINVoiceShortcutCenterBlockMethod2); cdecl;
    procedure setShortcutSuggestions(suggestions: NSArray); cdecl;
  end;
  TINVoiceShortcutCenter = class(TOCGenericImport<INVoiceShortcutCenterClass, INVoiceShortcutCenter>) end;

  INMediaItemClass = interface(NSObjectClass)
    ['{DEE836C9-23D6-45B6-A73D-BB2F6E54376D}']
  end;

  INMediaItem = interface(NSObject)
    ['{C31A56CA-2EC3-4878-874F-9FF55F5669BF}']
    [MethodName('type')]
    function &type: INMediaItemType; cdecl;
    function artist: NSString; cdecl;
    function artwork: INImage; cdecl;
    function identifier: NSString; cdecl;
    function initWithIdentifier(identifier: NSString; title: NSString; &type: INMediaItemType; artwork: INImage;
      artist: NSString): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; title: NSString; &type: INMediaItemType; artwork: INImage): Pointer; overload; cdecl;
    function title: NSString; cdecl;
  end;
  TINMediaItem = class(TOCGenericImport<INMediaItemClass, INMediaItem>) end;

function IntentsVersionNumber: Double;
// Exported const IntentsVersionString has an unsupported type: const unsigned char []
function INIntentErrorDomain: NSString;
function INStartAudioCallIntentIdentifier: NSString;
function INStartVideoCallIntentIdentifier: NSString;
function INSearchCallHistoryIntentIdentifier: NSString;
function INStartCallIntentIdentifier: NSString;
function INSetAudioSourceInCarIntentIdentifier: NSString;
function INSetClimateSettingsInCarIntentIdentifier: NSString;
function INSetDefrosterSettingsInCarIntentIdentifier: NSString;
function INSetSeatSettingsInCarIntentIdentifier: NSString;
function INSetProfileInCarIntentIdentifier: NSString;
function INSaveProfileInCarIntentIdentifier: NSString;
function INStartWorkoutIntentIdentifier: NSString;
function INPauseWorkoutIntentIdentifier: NSString;
function INEndWorkoutIntentIdentifier: NSString;
function INCancelWorkoutIntentIdentifier: NSString;
function INResumeWorkoutIntentIdentifier: NSString;
function INSetRadioStationIntentIdentifier: NSString;
function INSendMessageIntentIdentifier: NSString;
function INSearchForMessagesIntentIdentifier: NSString;
function INSetMessageAttributeIntentIdentifier: NSString;
function INSendPaymentIntentIdentifier: NSString;
function INRequestPaymentIntentIdentifier: NSString;
function INSearchForPhotosIntentIdentifier: NSString;
function INStartPhotoPlaybackIntentIdentifier: NSString;
function INListRideOptionsIntentIdentifier: NSString;
function INRequestRideIntentIdentifier: NSString;
function INGetRideStatusIntentIdentifier: NSString;
function INCarChargingConnectorTypeJ1772: INCarChargingConnectorType;
function INCarChargingConnectorTypeCCS1: INCarChargingConnectorType;
function INCarChargingConnectorTypeCCS2: INCarChargingConnectorType;
function INCarChargingConnectorTypeCHAdeMO: INCarChargingConnectorType;
function INCarChargingConnectorTypeGBTAC: INCarChargingConnectorType;
function INCarChargingConnectorTypeGBTDC: INCarChargingConnectorType;
function INCarChargingConnectorTypeTesla: INCarChargingConnectorType;
function INCarChargingConnectorTypeMennekes: INCarChargingConnectorType;
function INPersonHandleLabelHome: INPersonHandleLabel;
function INPersonHandleLabelWork: INPersonHandleLabel;
function INPersonHandleLabeliPhone: INPersonHandleLabel;
function INPersonHandleLabelMobile: INPersonHandleLabel;
function INPersonHandleLabelMain: INPersonHandleLabel;
function INPersonHandleLabelHomeFax: INPersonHandleLabel;
function INPersonHandleLabelWorkFax: INPersonHandleLabel;
function INPersonHandleLabelPager: INPersonHandleLabel;
function INPersonHandleLabelOther: INPersonHandleLabel;
function INPersonHandleLabelSchool: INPersonHandleLabel;
function INPersonRelationshipFather: INPersonRelationship;
function INPersonRelationshipMother: INPersonRelationship;
function INPersonRelationshipParent: INPersonRelationship;
function INPersonRelationshipBrother: INPersonRelationship;
function INPersonRelationshipSister: INPersonRelationship;
function INPersonRelationshipChild: INPersonRelationship;
function INPersonRelationshipFriend: INPersonRelationship;
function INPersonRelationshipSpouse: INPersonRelationship;
function INPersonRelationshipPartner: INPersonRelationship;
function INPersonRelationshipAssistant: INPersonRelationship;
function INPersonRelationshipManager: INPersonRelationship;
function INPersonRelationshipSon: INPersonRelationship;
function INPersonRelationshipDaughter: INPersonRelationship;
function INWorkoutNameIdentifierRun: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierSit: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierSteps: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierStand: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierMove: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierWalk: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierYoga: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierDance: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierCrosstraining: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierElliptical: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierRower: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierCycle: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierStairs: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierOther: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierIndoorrun: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierIndoorcycle: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierIndoorwalk: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierExercise: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierHike: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierHighIntensityIntervalTraining: INWorkoutNameIdentifier;
function INWorkoutNameIdentifierSwim: INWorkoutNameIdentifier;

const
  libIntents = '/System/Library/Frameworks/Intents.framework/Intents';

implementation

uses
  // Posix
  Posix.Dlfcn,
  // DW
  DW.Macapi.Helpers;

var
  IntentsModule: THandle;

function IntentsVersionNumber: Double;
begin
  Result := CocoaDoubleConst(libIntents, 'IntentsVersionNumber');
end;

function INIntentErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INIntentErrorDomain');
end;

function INStartAudioCallIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INStartAudioCallIntentIdentifier');
end;

function INStartVideoCallIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INStartVideoCallIntentIdentifier');
end;

function INSearchCallHistoryIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSearchCallHistoryIntentIdentifier');
end;

function INStartCallIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INStartCallIntentIdentifier');
end;

function INSetAudioSourceInCarIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSetAudioSourceInCarIntentIdentifier');
end;

function INSetClimateSettingsInCarIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSetClimateSettingsInCarIntentIdentifier');
end;

function INSetDefrosterSettingsInCarIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSetDefrosterSettingsInCarIntentIdentifier');
end;

function INSetSeatSettingsInCarIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSetSeatSettingsInCarIntentIdentifier');
end;

function INSetProfileInCarIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSetProfileInCarIntentIdentifier');
end;

function INSaveProfileInCarIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSaveProfileInCarIntentIdentifier');
end;

function INStartWorkoutIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INStartWorkoutIntentIdentifier');
end;

function INPauseWorkoutIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INPauseWorkoutIntentIdentifier');
end;

function INEndWorkoutIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INEndWorkoutIntentIdentifier');
end;

function INCancelWorkoutIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INCancelWorkoutIntentIdentifier');
end;

function INResumeWorkoutIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INResumeWorkoutIntentIdentifier');
end;

function INSetRadioStationIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSetRadioStationIntentIdentifier');
end;

function INSendMessageIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSendMessageIntentIdentifier');
end;

function INSearchForMessagesIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSearchForMessagesIntentIdentifier');
end;

function INSetMessageAttributeIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSetMessageAttributeIntentIdentifier');
end;

function INSendPaymentIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSendPaymentIntentIdentifier');
end;

function INRequestPaymentIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INRequestPaymentIntentIdentifier');
end;

function INSearchForPhotosIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INSearchForPhotosIntentIdentifier');
end;

function INStartPhotoPlaybackIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INStartPhotoPlaybackIntentIdentifier');
end;

function INListRideOptionsIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INListRideOptionsIntentIdentifier');
end;

function INRequestRideIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INRequestRideIntentIdentifier');
end;

function INGetRideStatusIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INGetRideStatusIntentIdentifier');
end;

function INCarChargingConnectorTypeJ1772: INCarChargingConnectorType;
begin
  Result := CocoaNSStringConst(libIntents, 'INCarChargingConnectorTypeJ1772');
end;

function INCarChargingConnectorTypeCCS1: INCarChargingConnectorType;
begin
  Result := CocoaNSStringConst(libIntents, 'INCarChargingConnectorTypeCCS1');
end;

function INCarChargingConnectorTypeCCS2: INCarChargingConnectorType;
begin
  Result := CocoaNSStringConst(libIntents, 'INCarChargingConnectorTypeCCS2');
end;

function INCarChargingConnectorTypeCHAdeMO: INCarChargingConnectorType;
begin
  Result := CocoaNSStringConst(libIntents, 'INCarChargingConnectorTypeCHAdeMO');
end;

function INCarChargingConnectorTypeGBTAC: INCarChargingConnectorType;
begin
  Result := CocoaNSStringConst(libIntents, 'INCarChargingConnectorTypeGBTAC');
end;

function INCarChargingConnectorTypeGBTDC: INCarChargingConnectorType;
begin
  Result := CocoaNSStringConst(libIntents, 'INCarChargingConnectorTypeGBTDC');
end;

function INCarChargingConnectorTypeTesla: INCarChargingConnectorType;
begin
  Result := CocoaNSStringConst(libIntents, 'INCarChargingConnectorTypeTesla');
end;

function INCarChargingConnectorTypeMennekes: INCarChargingConnectorType;
begin
  Result := CocoaNSStringConst(libIntents, 'INCarChargingConnectorTypeMennekes');
end;

function INPersonHandleLabelHome: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelHome');
end;

function INPersonHandleLabelWork: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelWork');
end;

function INPersonHandleLabeliPhone: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabeliPhone');
end;

function INPersonHandleLabelMobile: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelMobile');
end;

function INPersonHandleLabelMain: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelMain');
end;

function INPersonHandleLabelHomeFax: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelHomeFax');
end;

function INPersonHandleLabelWorkFax: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelWorkFax');
end;

function INPersonHandleLabelPager: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelPager');
end;

function INPersonHandleLabelOther: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelOther');
end;

function INPersonHandleLabelSchool: INPersonHandleLabel;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonHandleLabelSchool');
end;

function INPersonRelationshipFather: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipFather');
end;

function INPersonRelationshipMother: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipMother');
end;

function INPersonRelationshipParent: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipParent');
end;

function INPersonRelationshipBrother: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipBrother');
end;

function INPersonRelationshipSister: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipSister');
end;

function INPersonRelationshipChild: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipChild');
end;

function INPersonRelationshipFriend: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipFriend');
end;

function INPersonRelationshipSpouse: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipSpouse');
end;

function INPersonRelationshipPartner: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipPartner');
end;

function INPersonRelationshipAssistant: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipAssistant');
end;

function INPersonRelationshipManager: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipManager');
end;

function INPersonRelationshipSon: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipSon');
end;

function INPersonRelationshipDaughter: INPersonRelationship;
begin
  Result := CocoaNSStringConst(libIntents, 'INPersonRelationshipDaughter');
end;

function INWorkoutNameIdentifierRun: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierRun');
end;

function INWorkoutNameIdentifierSit: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierSit');
end;

function INWorkoutNameIdentifierSteps: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierSteps');
end;

function INWorkoutNameIdentifierStand: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierStand');
end;

function INWorkoutNameIdentifierMove: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierMove');
end;

function INWorkoutNameIdentifierWalk: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierWalk');
end;

function INWorkoutNameIdentifierYoga: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierYoga');
end;

function INWorkoutNameIdentifierDance: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierDance');
end;

function INWorkoutNameIdentifierCrosstraining: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierCrosstraining');
end;

function INWorkoutNameIdentifierElliptical: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierElliptical');
end;

function INWorkoutNameIdentifierRower: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierRower');
end;

function INWorkoutNameIdentifierCycle: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierCycle');
end;

function INWorkoutNameIdentifierStairs: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierStairs');
end;

function INWorkoutNameIdentifierOther: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierOther');
end;

function INWorkoutNameIdentifierIndoorrun: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierIndoorrun');
end;

function INWorkoutNameIdentifierIndoorcycle: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierIndoorcycle');
end;

function INWorkoutNameIdentifierIndoorwalk: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierIndoorwalk');
end;

function INWorkoutNameIdentifierExercise: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierExercise');
end;

function INWorkoutNameIdentifierHike: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierHike');
end;

function INWorkoutNameIdentifierHighIntensityIntervalTraining: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierHighIntensityIntervalTraining');
end;

function INWorkoutNameIdentifierSwim: INWorkoutNameIdentifier;
begin
  Result := CocoaNSStringConst(libIntents, 'INWorkoutNameIdentifierSwim');
end;

initialization
  IntentsModule := dlopen(MarshaledAString(libIntents), RTLD_LAZY);

finalization
  dlclose(IntentsModule)

end.