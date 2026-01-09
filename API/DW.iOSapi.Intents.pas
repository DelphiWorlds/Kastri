unit DW.iOSapi.Intents;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreLocation, iOSapi.UserNotifications, iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.Foundation, DW.iOSapi.EventKitExtra;

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
  INCallAudioRouteUnknown = 0;
  INCallAudioRouteSpeakerphoneAudioRoute = 1;
  INCallAudioRouteBluetoothAudioRoute = 2;
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
  INAnswerCallIntentResponseCodeUnspecified = 0;
  INAnswerCallIntentResponseCodeReady = 1;
  INAnswerCallIntentResponseCodeContinueInApp = 2;
  INAnswerCallIntentResponseCodeInProgress = 3;
  INAnswerCallIntentResponseCodeSuccess = 4;
  INAnswerCallIntentResponseCodeFailure = 5;
  INAnswerCallIntentResponseCodeFailureRequiringAppLaunch = 6;
  INHangUpCallIntentResponseCodeUnspecified = 0;
  INHangUpCallIntentResponseCodeReady = 1;
  INHangUpCallIntentResponseCodeInProgress = 2;
  INHangUpCallIntentResponseCodeSuccess = 3;
  INHangUpCallIntentResponseCodeFailure = 4;
  INHangUpCallIntentResponseCodeFailureRequiringAppLaunch = 5;
  INHangUpCallIntentResponseCodeFailureNoCallToHangUp = 6;
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
  INShareFocusStatusIntentResponseCodeUnspecified = 0;
  INShareFocusStatusIntentResponseCodeReady = 1;
  INShareFocusStatusIntentResponseCodeInProgress = 2;
  INShareFocusStatusIntentResponseCodeSuccess = 3;
  INShareFocusStatusIntentResponseCodeFailure = 4;
  INShareFocusStatusIntentResponseCodeFailureRequiringAppLaunch = 5;
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
  INPlayMediaIntentResponseCodeFailureMaxStreamLimitReached = 11;
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
  INFocusStatusAuthorizationStatusNotDetermined = 0;
  INFocusStatusAuthorizationStatusRestricted = 1;
  INFocusStatusAuthorizationStatusDenied = 2;
  INFocusStatusAuthorizationStatusAuthorized = 3;
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
  INAnswerCallIntent = interface;
  INAnswerCallIntentHandling = interface;
  INHangUpCallIntent = interface;
  INHangUpCallIntentHandling = interface;
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
  INShareFocusStatusIntent = interface;
  INShareFocusStatusIntentHandling = interface;
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
  INAnswerCallIntentResponse = interface;
  INHangUpCallIntentResponse = interface;
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
  INShareFocusStatusIntentResponse = interface;
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
  INFocusStatus = interface;
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
  INIntentDonationMetadata = interface;
  INSendMessageIntentDonationMetadata = interface;
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
  INFocusStatusCenter = interface;
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
  INCallAudioRoute = NSInteger;
  INCallCapabilityOptions = NSInteger;
  INCallRecordType = NSInteger;
  INCallRecordTypeOptions = NSInteger;
  INCallDestinationType = NSInteger;
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
  INAnswerCallIntentResponseCode = NSInteger;
  INHangUpCallIntentResponseCode = NSInteger;
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
  INShareFocusStatusIntentResponseCode = NSInteger;
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
  INFocusStatusAuthorizationStatus = NSInteger;
  INMediaUserContextSubscriptionStatus = NSInteger;
  INDailyRoutineSituation = NSInteger;
  INRelevantShortcutRole = NSInteger;
  TINAnswerCallIntentHandlingBlockMethod1 = procedure(response: INAnswerCallIntentResponse) of object;
  TINHangUpCallIntentHandlingBlockMethod1 = procedure(response: INHangUpCallIntentResponse) of object;
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
  TINShareFocusStatusIntentHandlingBlockMethod1 = procedure(response: INShareFocusStatusIntentResponse) of object;
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
  TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod1 = procedure(response: INGetAvailableRestaurantReservationBookingsIntentResponse) of object;
  TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod2 = procedure(resolutionResult: INRestaurantResolutionResult) of object;
  TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod3 = procedure(resolutionResult: INIntegerResolutionResult) of object;
  TINGetAvailableRestaurantReservationBookingsIntentHandlingBlockMethod4 = procedure(resolutionResult: INDateComponentsResolutionResult) of object;
  TINGetUserCurrentRestaurantReservationBookingsIntentHandlingBlockMethod1 = procedure(response: INGetUserCurrentRestaurantReservationBookingsIntentResponse) of object;
  TINGetUserCurrentRestaurantReservationBookingsIntentHandlingBlockMethod2 = procedure(resolutionResult: INRestaurantResolutionResult) of object;
  TINGetAvailableRestaurantReservationBookingDefaultsIntentHandlingBlockMethod1 = procedure(response: INGetAvailableRestaurantReservationBookingDefaultsIntentResponse) of object;
  TINGetAvailableRestaurantReservationBookingDefaultsIntentHandlingBlockMethod2 = procedure(resolutionResult: INRestaurantResolutionResult) of object;
  TINGetRestaurantGuestIntentHandlingBlockMethod1 = procedure(response: INGetRestaurantGuestIntentResponse) of object;
  TINPreferencesBlockMethod1 = procedure(status: INSiriAuthorizationStatus) of object;
  TINFocusStatusCenterBlockMethod1 = procedure(status: INFocusStatusAuthorizationStatus) of object;
  TINRelevantShortcutStoreBlockMethod1 = procedure(error: NSError) of object;
  TINVoiceShortcutCenterBlockMethod1 = procedure(voiceShortcuts: NSArray; error: NSError) of object;
  TINVoiceShortcutCenterBlockMethod2 = procedure(voiceShortcut: INVoiceShortcut; error: NSError) of object;

  INIntentClass = interface(NSObjectClass)
    ['{2751981C-E973-4995-AED6-2A0F14C46FA4}']
  end;

  INIntent = interface(NSObject)
    ['{BD801A93-701A-4A5E-BFBB-C2DB37BA6F79}']
    function donationMetadata: INIntentDonationMetadata; cdecl;
    function identifier: NSString; cdecl;
    function imageForParameterNamed(parameterName: NSString): INImage; cdecl;
    function intentDescription: NSString; cdecl;
    function keyImage: INImage; cdecl;
    procedure setDonationMetadata(donationMetadata: INIntentDonationMetadata); cdecl;
    procedure setImage(image: INImage; forParameterNamed: NSString); cdecl;
    procedure setShortcutAvailability(shortcutAvailability: INShortcutAvailabilityOptions); cdecl;
    procedure setSuggestedInvocationPhrase(suggestedInvocationPhrase: NSString); cdecl;
    function shortcutAvailability: INShortcutAvailabilityOptions; cdecl;
    function suggestedInvocationPhrase: NSString; cdecl;
  end;
  TINIntent = class(TOCGenericImport<INIntentClass, INIntent>) end;

  INIntentHandlerProviding = interface(IObjectiveC)
    ['{E747F59A-B243-4E9C-8739-4498933E2B1F}']
    function handlerForIntent(intent: INIntent): Pointer; cdecl;
  end;

  INIntentResponseClass = interface(NSObjectClass)
    ['{027401F0-48D5-4642-A385-26A7ED167069}']
  end;

  INIntentResponse = interface(NSObject)
    ['{1691C760-92B9-40B0-8084-3A84C27E4965}']
    procedure setUserActivity(userActivity: NSUserActivity); cdecl;
    function userActivity: NSUserActivity; cdecl;
  end;
  TINIntentResponse = class(TOCGenericImport<INIntentResponseClass, INIntentResponse>) end;

  INIntentResolutionResultClass = interface(NSObjectClass)
    ['{F5A53A13-1B43-408D-ABD1-FA4BEBBC78CB}']
    {class} function confirmationRequiredWithItemToConfirm(itemToConfirm: Pointer; forReason: NSInteger): Pointer; cdecl;
    {class} function needsValue: Pointer; cdecl;
    {class} function notRequired: Pointer; cdecl;
    {class} function unsupported: Pointer; cdecl;
    {class} function unsupportedWithReason(reason: NSInteger): Pointer; cdecl;
  end;

  INIntentResolutionResult = interface(NSObject)
    ['{1C77B86F-1056-42F7-811D-B28A1DA11829}']
  end;
  TINIntentResolutionResult = class(TOCGenericImport<INIntentResolutionResultClass, INIntentResolutionResult>) end;

  INAnswerCallIntentClass = interface(INIntentClass)
    ['{6BF842E6-07B3-4D83-90FF-B0A9AE0DF8CE}']
  end;

  INAnswerCallIntent = interface(INIntent)
    ['{E70A9D93-CEB0-47A5-8476-F22BB9569FFB}']
    function audioRoute: INCallAudioRoute; cdecl;
    function callIdentifier: NSString; cdecl;
    function initWithAudioRoute(audioRoute: INCallAudioRoute; callIdentifier: NSString): Pointer; cdecl;
  end;
  TINAnswerCallIntent = class(TOCGenericImport<INAnswerCallIntentClass, INAnswerCallIntent>) end;

  INAnswerCallIntentHandling = interface(IObjectiveC)
    ['{9177A61F-D071-4FFE-8F6A-91CD2DC4259F}']
    procedure confirmAnswerCall(intent: INAnswerCallIntent; completion: Pointer); cdecl;
    procedure handleAnswerCall(intent: INAnswerCallIntent; completion: Pointer); cdecl;
  end;

  INHangUpCallIntentClass = interface(INIntentClass)
    ['{E29FC2D6-8D3A-46ED-9021-6C69DF8DD4D3}']
  end;

  INHangUpCallIntent = interface(INIntent)
    ['{614E6178-BD58-4339-80B6-747D16B226CD}']
    function callIdentifier: NSString; cdecl;
    function initWithCallIdentifier(callIdentifier: NSString): Pointer; cdecl;
  end;
  TINHangUpCallIntent = class(TOCGenericImport<INHangUpCallIntentClass, INHangUpCallIntent>) end;

  INHangUpCallIntentHandling = interface(IObjectiveC)
    ['{AB934C95-D25F-4E5A-A1DE-A3C3D070FD34}']
    procedure confirmHangUpCall(intent: INHangUpCallIntent; completion: Pointer); cdecl;
    procedure handleHangUpCall(intent: INHangUpCallIntent; completion: Pointer); cdecl;
  end;

  INSearchCallHistoryIntentClass = interface(INIntentClass)
    ['{8ACEC255-970D-4943-A9C5-007B760F5DAC}']
  end;

  INSearchCallHistoryIntent = interface(INIntent)
    ['{BAAADBE0-5A25-4660-B593-985A1B85F5D6}']
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
    ['{5B76FB93-171D-4AAA-A255-4D083FA2EAC0}']
    procedure confirmSearchCallHistory(intent: INSearchCallHistoryIntent; completion: Pointer); cdecl;
    procedure handleSearchCallHistory(intent: INSearchCallHistoryIntent; completion: Pointer); cdecl;
    procedure resolveCallTypeForSearchCallHistory(intent: INSearchCallHistoryIntent; withCompletion: Pointer); cdecl;
    procedure resolveCallTypesForSearchCallHistory(intent: INSearchCallHistoryIntent; withCompletion: Pointer); cdecl;
    procedure resolveDateCreatedForSearchCallHistory(intent: INSearchCallHistoryIntent; withCompletion: Pointer); cdecl;
    procedure resolveRecipientForSearchCallHistory(intent: INSearchCallHistoryIntent; withCompletion: Pointer); cdecl;
    procedure resolveUnseenForSearchCallHistory(intent: INSearchCallHistoryIntent; withCompletion: Pointer); cdecl;
  end;

  INStartAudioCallIntentClass = interface(INIntentClass)
    ['{CB658367-0438-48B2-AE81-34E17BE0FCED}']
  end;

  INStartAudioCallIntent = interface(INIntent)
    ['{E69DAEE1-2778-4627-8D51-DE5BF3BB5AD1}']
    function contacts: NSArray; cdecl;
    function destinationType: INCallDestinationType; cdecl;
    function initWithContacts(contacts: NSArray): Pointer; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    function initWithDestinationType(destinationType: INCallDestinationType; contacts: NSArray): Pointer; cdecl;
  end;
  TINStartAudioCallIntent = class(TOCGenericImport<INStartAudioCallIntentClass, INStartAudioCallIntent>) end;

  INStartAudioCallIntentHandling = interface(IObjectiveC)
    ['{88B54954-E63C-4F7E-8EEE-2B38CF081810}']
    procedure confirmStartAudioCall(intent: INStartAudioCallIntent; completion: Pointer); cdecl;
    procedure handleStartAudioCall(intent: INStartAudioCallIntent; completion: Pointer); cdecl;
    procedure resolveContactsForStartAudioCall(intent: INStartAudioCallIntent; withCompletion: Pointer); cdecl;
    procedure resolveDestinationTypeForStartAudioCall(intent: INStartAudioCallIntent; withCompletion: Pointer); cdecl;
  end;

  INStartCallIntentClass = interface(INIntentClass)
    ['{7F539E35-79D6-4F51-ABC3-0497382089FC}']
  end;

  INStartCallIntent = interface(INIntent)
    ['{070EEFBD-73C5-4B3C-9C98-E6DF10918A15}']
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
    ['{11030634-28FE-4D34-AAC5-6C79B34FBBF9}']
    procedure confirmStartCall(intent: INStartCallIntent; completion: Pointer); cdecl;
    procedure handleStartCall(intent: INStartCallIntent; completion: Pointer); cdecl;
    procedure resolveCallCapabilityForStartCall(intent: INStartCallIntent; withCompletion: Pointer); cdecl;
    procedure resolveCallRecordToCallBackForStartCall(intent: INStartCallIntent; withCompletion: Pointer); cdecl;
    procedure resolveContactsForStartCall(intent: INStartCallIntent; withCompletion: Pointer); cdecl;
    procedure resolveDestinationTypeForStartCall(intent: INStartCallIntent; withCompletion: Pointer); cdecl;
  end;

  INStartVideoCallIntentClass = interface(INIntentClass)
    ['{B7D8C00B-1219-4547-ABA9-908586F85BC0}']
  end;

  INStartVideoCallIntent = interface(INIntent)
    ['{E698BBF0-DED2-4FCF-9425-9FC3C97741E0}']
    function contacts: NSArray; cdecl;
    function initWithContacts(contacts: NSArray): Pointer; cdecl;
  end;
  TINStartVideoCallIntent = class(TOCGenericImport<INStartVideoCallIntentClass, INStartVideoCallIntent>) end;

  INStartVideoCallIntentHandling = interface(IObjectiveC)
    ['{ABB50F71-71F1-4FE6-AF9C-0F0FC9D7FB07}']
    procedure confirmStartVideoCall(intent: INStartVideoCallIntent; completion: Pointer); cdecl;
    procedure handleStartVideoCall(intent: INStartVideoCallIntent; completion: Pointer); cdecl;
    procedure resolveContactsForStartVideoCall(intent: INStartVideoCallIntent; withCompletion: Pointer); cdecl;
  end;

  INActivateCarSignalIntentClass = interface(INIntentClass)
    ['{F89A7D46-B0BA-4000-ABD0-D9D9DED29CDA}']
  end;

  INActivateCarSignalIntent = interface(INIntent)
    ['{7EA561CE-F324-4F32-A595-7902BE6E46D0}']
    function carName: INSpeakableString; cdecl;
    function initWithCarName(carName: INSpeakableString; signals: INCarSignalOptions): Pointer; cdecl;
    function signals: INCarSignalOptions; cdecl;
  end;
  TINActivateCarSignalIntent = class(TOCGenericImport<INActivateCarSignalIntentClass, INActivateCarSignalIntent>) end;

  INActivateCarSignalIntentHandling = interface(IObjectiveC)
    ['{BE04E72B-47FA-4BBE-B5FC-470CC26CC09D}']
    procedure confirmActivateCarSignal(intent: INActivateCarSignalIntent; completion: Pointer); cdecl;
    procedure handleActivateCarSignal(intent: INActivateCarSignalIntent; completion: Pointer); cdecl;
    procedure resolveCarNameForActivateCarSignal(intent: INActivateCarSignalIntent; withCompletion: Pointer); cdecl;
    procedure resolveSignalsForActivateCarSignal(intent: INActivateCarSignalIntent; withCompletion: Pointer); cdecl;
  end;

  INGetCarLockStatusIntentClass = interface(INIntentClass)
    ['{CDAF3463-7AF7-450E-9A57-56C7600501DB}']
  end;

  INGetCarLockStatusIntent = interface(INIntent)
    ['{38C61D36-2D3B-46AF-B540-240D0044CBB2}']
    function carName: INSpeakableString; cdecl;
    function initWithCarName(carName: INSpeakableString): Pointer; cdecl;
  end;
  TINGetCarLockStatusIntent = class(TOCGenericImport<INGetCarLockStatusIntentClass, INGetCarLockStatusIntent>) end;

  INGetCarLockStatusIntentHandling = interface(IObjectiveC)
    ['{BB12A38D-949E-4AD1-A6F1-016EC794A833}']
    procedure confirmGetCarLockStatus(intent: INGetCarLockStatusIntent; completion: Pointer); cdecl;
    procedure handleGetCarLockStatus(intent: INGetCarLockStatusIntent; completion: Pointer); cdecl;
    procedure resolveCarNameForGetCarLockStatus(intent: INGetCarLockStatusIntent; withCompletion: Pointer); cdecl;
  end;

  INGetCarPowerLevelStatusIntentClass = interface(INIntentClass)
    ['{815AC8F5-4834-4217-BFB4-07A3EAA500FA}']
  end;

  INGetCarPowerLevelStatusIntent = interface(INIntent)
    ['{41249345-7E0B-4F74-BE5C-DDCC63D2DB44}']
    function carName: INSpeakableString; cdecl;
    function initWithCarName(carName: INSpeakableString): Pointer; cdecl;
  end;
  TINGetCarPowerLevelStatusIntent = class(TOCGenericImport<INGetCarPowerLevelStatusIntentClass, INGetCarPowerLevelStatusIntent>) end;

  INGetCarPowerLevelStatusIntentHandling = interface(IObjectiveC)
    ['{8F83CAE7-2D6E-407A-8308-DBDA2E39649F}']
    procedure confirmGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent; completion: Pointer); cdecl;
    procedure handleGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent; completion: Pointer); cdecl;
    procedure resolveCarNameForGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent; withCompletion: Pointer); cdecl;
    procedure startSendingUpdatesForGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent; toObserver: Pointer); cdecl;
    procedure stopSendingUpdatesForGetCarPowerLevelStatus(intent: INGetCarPowerLevelStatusIntent); cdecl;
  end;

  INGetCarPowerLevelStatusIntentResponseObserver = interface(IObjectiveC)
    ['{2D84CCDF-96BA-4AA4-9D1D-15AE8359A5E5}']
    procedure getCarPowerLevelStatusResponseDidUpdate(response: INGetCarPowerLevelStatusIntentResponse); cdecl;
  end;

  INListCarsIntentClass = interface(INIntentClass)
    ['{9F23545F-163D-47F7-A912-D759D594A3FB}']
  end;

  INListCarsIntent = interface(INIntent)
    ['{8D550C1E-BE00-4E5B-A171-3AD1B38FC952}']
  end;
  TINListCarsIntent = class(TOCGenericImport<INListCarsIntentClass, INListCarsIntent>) end;

  INListCarsIntentHandling = interface(IObjectiveC)
    ['{94FCCF8B-B876-497B-9013-296B40116FA9}']
    procedure confirmListCars(intent: INListCarsIntent; completion: Pointer); cdecl;
    procedure handleListCars(intent: INListCarsIntent; completion: Pointer); cdecl;
  end;

  INSaveProfileInCarIntentClass = interface(INIntentClass)
    ['{AD5FACEF-C08B-43BE-BA86-59960385C1A0}']
  end;

  INSaveProfileInCarIntent = interface(INIntent)
    ['{D1D3BABF-2DC1-4D27-842C-9B56B68C8B54}']
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
    ['{D914F6FF-3902-4694-9E0F-0E5C0E53193F}']
    procedure confirmSaveProfileInCar(intent: INSaveProfileInCarIntent; completion: Pointer); cdecl;
    procedure handleSaveProfileInCar(intent: INSaveProfileInCarIntent; completion: Pointer); cdecl;
    procedure resolveProfileNameForSaveProfileInCar(intent: INSaveProfileInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveProfileNumberForSaveProfileInCar(intent: INSaveProfileInCarIntent; withCompletion: Pointer); cdecl;
  end;

  INSetAudioSourceInCarIntentClass = interface(INIntentClass)
    ['{CEC6319B-40D9-4D16-A255-CC603539A307}']
  end;

  INSetAudioSourceInCarIntent = interface(INIntent)
    ['{1ECD0A31-5C32-46DB-B61E-EEA1A4EA6035}']
    function audioSource: INCarAudioSource; cdecl;
    function initWithAudioSource(audioSource: INCarAudioSource; relativeAudioSourceReference: INRelativeReference): Pointer; cdecl;
    function relativeAudioSourceReference: INRelativeReference; cdecl;
  end;
  TINSetAudioSourceInCarIntent = class(TOCGenericImport<INSetAudioSourceInCarIntentClass, INSetAudioSourceInCarIntent>) end;

  INSetAudioSourceInCarIntentHandling = interface(IObjectiveC)
    ['{AD21211A-E75A-41F7-A47B-F33782A71FE3}']
    procedure confirmSetAudioSourceInCar(intent: INSetAudioSourceInCarIntent; completion: Pointer); cdecl;
    procedure handleSetAudioSourceInCar(intent: INSetAudioSourceInCarIntent; completion: Pointer); cdecl;
    procedure resolveAudioSourceForSetAudioSourceInCar(intent: INSetAudioSourceInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveRelativeAudioSourceReferenceForSetAudioSourceInCar(intent: INSetAudioSourceInCarIntent; withCompletion: Pointer); cdecl;
  end;

  INSetCarLockStatusIntentClass = interface(INIntentClass)
    ['{303BC514-A256-4E1F-8BC0-8EF80B28C634}']
  end;

  INSetCarLockStatusIntent = interface(INIntent)
    ['{03D6524C-AF68-4EB5-B24B-4CBDE4808567}']
    function carName: INSpeakableString; cdecl;
    function initWithLocked(locked: NSNumber; carName: INSpeakableString): Pointer; cdecl;
    function locked: NSNumber; cdecl;
  end;
  TINSetCarLockStatusIntent = class(TOCGenericImport<INSetCarLockStatusIntentClass, INSetCarLockStatusIntent>) end;

  INSetCarLockStatusIntentHandling = interface(IObjectiveC)
    ['{3163492E-13CA-451C-BD0A-01B25EE8E932}']
    procedure confirmSetCarLockStatus(intent: INSetCarLockStatusIntent; completion: Pointer); cdecl;
    procedure handleSetCarLockStatus(intent: INSetCarLockStatusIntent; completion: Pointer); cdecl;
    procedure resolveCarNameForSetCarLockStatus(intent: INSetCarLockStatusIntent; withCompletion: Pointer); cdecl;
    procedure resolveLockedForSetCarLockStatus(intent: INSetCarLockStatusIntent; withCompletion: Pointer); cdecl;
  end;

  INSetClimateSettingsInCarIntentClass = interface(INIntentClass)
    ['{C09F5BE9-A376-4ADA-B17C-0D19540083CD}']
  end;

  INSetClimateSettingsInCarIntent = interface(INIntent)
    ['{9F2B0D41-35B1-4FB0-9203-CC098337030E}']
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
    ['{FC3627FC-BF1F-4798-94D1-3A464923F9C7}']
    procedure confirmSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; completion: Pointer); cdecl;
    procedure handleSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; completion: Pointer); cdecl;
    procedure resolveAirCirculationModeForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveCarNameForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveClimateZoneForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveEnableAirConditionerForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveEnableAutoModeForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveEnableClimateControlForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveEnableFanForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveFanSpeedIndexForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveFanSpeedPercentageForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveRelativeFanSpeedSettingForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveRelativeTemperatureSettingForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveTemperatureForSetClimateSettingsInCar(intent: INSetClimateSettingsInCarIntent; withCompletion: Pointer); cdecl;
  end;

  INSetDefrosterSettingsInCarIntentClass = interface(INIntentClass)
    ['{F404E17A-DD79-4B8C-9957-4751D313FB7E}']
  end;

  INSetDefrosterSettingsInCarIntent = interface(INIntent)
    ['{2132B731-A14B-4F09-80B1-7F5C9D805EDD}']
    function carName: INSpeakableString; cdecl;
    function defroster: INCarDefroster; cdecl;
    function enable: NSNumber; cdecl;
    function initWithEnable(enable: NSNumber; defroster: INCarDefroster): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-initWithEnable:defroster:carName:", ios(10.0, 12.0))
    function initWithEnable(enable: NSNumber; defroster: INCarDefroster; carName: INSpeakableString): Pointer; overload; cdecl;
  end;
  TINSetDefrosterSettingsInCarIntent = class(TOCGenericImport<INSetDefrosterSettingsInCarIntentClass, INSetDefrosterSettingsInCarIntent>) end;

  INSetDefrosterSettingsInCarIntentHandling = interface(IObjectiveC)
    ['{06147C45-3AB5-4B97-85DA-620EEAE79B0D}']
    procedure confirmSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent; completion: Pointer); cdecl;
    procedure handleSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent; completion: Pointer); cdecl;
    procedure resolveCarNameForSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveDefrosterForSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveEnableForSetDefrosterSettingsInCar(intent: INSetDefrosterSettingsInCarIntent; withCompletion: Pointer); cdecl;
  end;

  INSetProfileInCarIntentClass = interface(INIntentClass)
    ['{AFC31B73-A98F-417A-831B-03F8B3CBA493}']
  end;

  INSetProfileInCarIntent = interface(INIntent)
    ['{4CAE0D42-000D-4DE8-B21B-35E81B225356}']
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
    ['{FC85B89D-8B02-491D-A247-F79E3C1D5EDE}']
    procedure confirmSetProfileInCar(intent: INSetProfileInCarIntent; completion: Pointer); cdecl;
    procedure handleSetProfileInCar(intent: INSetProfileInCarIntent; completion: Pointer); cdecl;
    procedure resolveCarNameForSetProfileInCar(intent: INSetProfileInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveDefaultProfileForSetProfileInCar(intent: INSetProfileInCarIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("The property doesn't need to be resolved", ios(10.0, 11.0))
    procedure resolveProfileNameForSetProfileInCar(intent: INSetProfileInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveProfileNumberForSetProfileInCar(intent: INSetProfileInCarIntent; withCompletion: Pointer); cdecl;
  end;

  INSetSeatSettingsInCarIntentClass = interface(INIntentClass)
    ['{09FF833C-EF17-41E5-A674-4B8A2BE3448A}']
  end;

  INSetSeatSettingsInCarIntent = interface(INIntent)
    ['{4183328B-5FFE-482D-8F46-D2B5D0E08420}']
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
    ['{824B2E6C-DF92-47BA-AD29-11DCE063A8C0}']
    procedure confirmSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; completion: Pointer); cdecl;
    procedure handleSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; completion: Pointer); cdecl;
    procedure resolveCarNameForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveEnableCoolingForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveEnableHeatingForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveEnableMassageForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveLevelForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveRelativeLevelSettingForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; withCompletion: Pointer); cdecl;
    procedure resolveSeatForSetSeatSettingsInCar(intent: INSetSeatSettingsInCarIntent; withCompletion: Pointer); cdecl;
  end;

  INCancelWorkoutIntentClass = interface(INIntentClass)
    ['{E557C932-DD3B-48E3-BB89-6EED269EB037}']
  end;

  INCancelWorkoutIntent = interface(INIntent)
    ['{50E65C8A-F543-4BD5-B576-8FB361AF70A1}']
    function initWithWorkoutName(workoutName: INSpeakableString): Pointer; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINCancelWorkoutIntent = class(TOCGenericImport<INCancelWorkoutIntentClass, INCancelWorkoutIntent>) end;

  INCancelWorkoutIntentHandling = interface(IObjectiveC)
    ['{232E78F4-69ED-4FC4-AEEF-991D4982686C}']
    procedure confirmCancelWorkout(intent: INCancelWorkoutIntent; completion: Pointer); cdecl;
    procedure handleCancelWorkout(intent: INCancelWorkoutIntent; completion: Pointer); cdecl;
    procedure resolveWorkoutNameForCancelWorkout(intent: INCancelWorkoutIntent; withCompletion: Pointer); cdecl;
  end;

  INEndWorkoutIntentClass = interface(INIntentClass)
    ['{97EF8C96-1E86-4955-AD37-07ECC6175A6C}']
  end;

  INEndWorkoutIntent = interface(INIntent)
    ['{CAB286EF-DDC3-4CF4-B588-B08BCEDB9197}']
    function initWithWorkoutName(workoutName: INSpeakableString): Pointer; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINEndWorkoutIntent = class(TOCGenericImport<INEndWorkoutIntentClass, INEndWorkoutIntent>) end;

  INEndWorkoutIntentHandling = interface(IObjectiveC)
    ['{7D638386-CF8C-4D6F-BD49-58A06D4F4F68}']
    procedure confirmEndWorkout(intent: INEndWorkoutIntent; completion: Pointer); cdecl;
    procedure handleEndWorkout(intent: INEndWorkoutIntent; completion: Pointer); cdecl;
    procedure resolveWorkoutNameForEndWorkout(intent: INEndWorkoutIntent; withCompletion: Pointer); cdecl;
  end;

  INPauseWorkoutIntentClass = interface(INIntentClass)
    ['{BF9DB04F-2E27-4435-AE6E-332E74F81AA7}']
  end;

  INPauseWorkoutIntent = interface(INIntent)
    ['{BAAAFFE3-2AE1-4DC3-AA6E-9C25DB9D2B72}']
    function initWithWorkoutName(workoutName: INSpeakableString): Pointer; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINPauseWorkoutIntent = class(TOCGenericImport<INPauseWorkoutIntentClass, INPauseWorkoutIntent>) end;

  INPauseWorkoutIntentHandling = interface(IObjectiveC)
    ['{524DC151-EF8A-4183-9DE9-840A84B0E3E2}']
    procedure confirmPauseWorkout(intent: INPauseWorkoutIntent; completion: Pointer); cdecl;
    procedure handlePauseWorkout(intent: INPauseWorkoutIntent; completion: Pointer); cdecl;
    procedure resolveWorkoutNameForPauseWorkout(intent: INPauseWorkoutIntent; withCompletion: Pointer); cdecl;
  end;

  INResumeWorkoutIntentClass = interface(INIntentClass)
    ['{7C92F4E3-1F27-4693-816D-A71285CB4DA8}']
  end;

  INResumeWorkoutIntent = interface(INIntent)
    ['{FA15848D-DD8C-4F70-866C-629AE1B1C2F8}']
    function initWithWorkoutName(workoutName: INSpeakableString): Pointer; cdecl;
    function workoutName: INSpeakableString; cdecl;
  end;
  TINResumeWorkoutIntent = class(TOCGenericImport<INResumeWorkoutIntentClass, INResumeWorkoutIntent>) end;

  INResumeWorkoutIntentHandling = interface(IObjectiveC)
    ['{CFC4C24D-137A-476E-BB18-38701B012D15}']
    procedure confirmResumeWorkout(intent: INResumeWorkoutIntent; completion: Pointer); cdecl;
    procedure handleResumeWorkout(intent: INResumeWorkoutIntent; completion: Pointer); cdecl;
    procedure resolveWorkoutNameForResumeWorkout(intent: INResumeWorkoutIntent; withCompletion: Pointer); cdecl;
  end;

  INStartWorkoutIntentClass = interface(INIntentClass)
    ['{7D5054AF-B0F4-4EE1-86D3-6D41C3F16D56}']
  end;

  INStartWorkoutIntent = interface(INIntent)
    ['{83DCA231-45FE-4BE9-A60D-0D2F3578BB85}']
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
    ['{691556C1-39B0-4A76-BB8F-9BEE446FEFEE}']
    procedure confirmStartWorkout(intent: INStartWorkoutIntent; completion: Pointer); cdecl;
    procedure handleStartWorkout(intent: INStartWorkoutIntent; completion: Pointer); cdecl;
    procedure resolveGoalValueForStartWorkout(intent: INStartWorkoutIntent; withCompletion: Pointer); cdecl;
    procedure resolveIsOpenEndedForStartWorkout(intent: INStartWorkoutIntent; withCompletion: Pointer); cdecl;
    procedure resolveWorkoutGoalUnitTypeForStartWorkout(intent: INStartWorkoutIntent; withCompletion: Pointer); cdecl;
    procedure resolveWorkoutLocationTypeForStartWorkout(intent: INStartWorkoutIntent; withCompletion: Pointer); cdecl;
    procedure resolveWorkoutNameForStartWorkout(intent: INStartWorkoutIntent; withCompletion: Pointer); cdecl;
  end;

  INShareFocusStatusIntentClass = interface(INIntentClass)
    ['{6ABC0509-E293-4D9D-B7C9-5195C60B2468}']
  end;

  INShareFocusStatusIntent = interface(INIntent)
    ['{1C7AC53E-6A50-4AB1-9419-3D4ED9BFB48E}']
    function focusStatus: INFocusStatus; cdecl;
    function initWithFocusStatus(focusStatus: INFocusStatus): Pointer; cdecl;
  end;
  TINShareFocusStatusIntent = class(TOCGenericImport<INShareFocusStatusIntentClass, INShareFocusStatusIntent>) end;

  INShareFocusStatusIntentHandling = interface(IObjectiveC)
    ['{E9FEE3B8-8827-49DB-A7D2-76BED2CC7C02}']
    procedure confirmShareFocusStatus(intent: INShareFocusStatusIntent; completion: Pointer); cdecl;
    procedure handleShareFocusStatus(intent: INShareFocusStatusIntent; completion: Pointer); cdecl;
  end;

  INAddMediaIntentClass = interface(INIntentClass)
    ['{84C5240E-4FBA-4F14-8BC8-E7086449AFF6}']
  end;

  INAddMediaIntent = interface(INIntent)
    ['{C3DE4D13-8FC0-4CF3-85D3-B7BE4A35B638}']
    function initWithMediaItems(mediaItems: NSArray; mediaSearch: INMediaSearch; mediaDestination: INMediaDestination): Pointer; cdecl;
    function mediaDestination: INMediaDestination; cdecl;
    function mediaItems: NSArray; cdecl;
    function mediaSearch: INMediaSearch; cdecl;
  end;
  TINAddMediaIntent = class(TOCGenericImport<INAddMediaIntentClass, INAddMediaIntent>) end;

  INAddMediaIntentHandling = interface(IObjectiveC)
    ['{CBFB7C68-8B74-4B82-8112-EBDF07C66029}']
    procedure confirmAddMedia(intent: INAddMediaIntent; completion: Pointer); cdecl;
    procedure handleAddMedia(intent: INAddMediaIntent; completion: Pointer); cdecl;
    procedure resolveMediaDestinationForAddMedia(intent: INAddMediaIntent; withCompletion: Pointer); cdecl;
    procedure resolveMediaItemsForAddMedia(intent: INAddMediaIntent; withCompletion: Pointer); cdecl;
  end;

  INPlayMediaIntentClass = interface(INIntentClass)
    ['{F896B86F-5EE1-47F6-9B7F-69A6F79B7582}']
  end;

  INPlayMediaIntent = interface(INIntent)
    ['{BCC4A88A-C172-4188-BADD-6AAA7C5C0052}']
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
    ['{C7B31EDF-17FF-406F-82B6-FE03090909D3}']
    procedure confirmPlayMedia(intent: INPlayMediaIntent; completion: Pointer); cdecl;
    procedure handlePlayMedia(intent: INPlayMediaIntent; completion: Pointer); cdecl;
    procedure resolveMediaItemsForPlayMedia(intent: INPlayMediaIntent; withCompletion: Pointer); cdecl;
    procedure resolvePlaybackQueueLocationForPlayMedia(intent: INPlayMediaIntent; withCompletion: Pointer); cdecl;
    procedure resolvePlaybackRepeatModeForPlayMedia(intent: INPlayMediaIntent; withCompletion: Pointer); cdecl;
    procedure resolvePlaybackSpeedForPlayMedia(intent: INPlayMediaIntent; withCompletion: Pointer); cdecl;
    procedure resolvePlayShuffledForPlayMedia(intent: INPlayMediaIntent; withCompletion: Pointer); cdecl;
    procedure resolveResumePlaybackForPlayMedia(intent: INPlayMediaIntent; withCompletion: Pointer); cdecl;
  end;

  INSearchForMediaIntentClass = interface(INIntentClass)
    ['{A3DC5485-83AC-4493-BF0E-68A59F672AB2}']
  end;

  INSearchForMediaIntent = interface(INIntent)
    ['{92E082B3-4E76-439F-8728-B938A10A0B9C}']
    function initWithMediaItems(mediaItems: NSArray; mediaSearch: INMediaSearch): Pointer; cdecl;
    function mediaItems: NSArray; cdecl;
    function mediaSearch: INMediaSearch; cdecl;
  end;
  TINSearchForMediaIntent = class(TOCGenericImport<INSearchForMediaIntentClass, INSearchForMediaIntent>) end;

  INSearchForMediaIntentHandling = interface(IObjectiveC)
    ['{235C1EBE-8F0C-41C2-B8EB-E7C7DE651A8D}']
    procedure confirmSearchForMedia(intent: INSearchForMediaIntent; completion: Pointer); cdecl;
    procedure handleSearchForMedia(intent: INSearchForMediaIntent; completion: Pointer); cdecl;
    procedure resolveMediaItemsForSearchForMedia(intent: INSearchForMediaIntent; withCompletion: Pointer); cdecl;
  end;

  INUpdateMediaAffinityIntentClass = interface(INIntentClass)
    ['{75549258-C7F7-4820-AC82-483CED628C9E}']
  end;

  INUpdateMediaAffinityIntent = interface(INIntent)
    ['{F1CC49D8-40C7-4AB2-8E71-FB3B47C8CD81}']
    function affinityType: INMediaAffinityType; cdecl;
    function initWithMediaItems(mediaItems: NSArray; mediaSearch: INMediaSearch; affinityType: INMediaAffinityType): Pointer; cdecl;
    function mediaItems: NSArray; cdecl;
    function mediaSearch: INMediaSearch; cdecl;
  end;
  TINUpdateMediaAffinityIntent = class(TOCGenericImport<INUpdateMediaAffinityIntentClass, INUpdateMediaAffinityIntent>) end;

  INUpdateMediaAffinityIntentHandling = interface(IObjectiveC)
    ['{1A47E403-CE76-4BFB-A185-69B1DD25ACA3}']
    procedure confirmUpdateMediaAffinity(intent: INUpdateMediaAffinityIntent; completion: Pointer); cdecl;
    procedure handleUpdateMediaAffinity(intent: INUpdateMediaAffinityIntent; completion: Pointer); cdecl;
    procedure resolveAffinityTypeForUpdateMediaAffinity(intent: INUpdateMediaAffinityIntent; withCompletion: Pointer); cdecl;
    procedure resolveMediaItemsForUpdateMediaAffinity(intent: INUpdateMediaAffinityIntent; withCompletion: Pointer); cdecl;
  end;

  INSetRadioStationIntentClass = interface(INIntentClass)
    ['{A4090DE8-B11B-46ED-B78A-6CAAA9795FEF}']
  end;

  INSetRadioStationIntent = interface(INIntent)
    ['{89BEE3AF-C5EC-416E-82D0-38DA4DE4B0E0}']
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
    ['{721482F9-0FD4-45B8-BD24-4F2B22B4F1C1}']
    procedure confirmSetRadioStation(intent: INSetRadioStationIntent; completion: Pointer); cdecl;
    procedure handleSetRadioStation(intent: INSetRadioStationIntent; completion: Pointer); cdecl;
    procedure resolveChannelForSetRadioStation(intent: INSetRadioStationIntent; withCompletion: Pointer); cdecl;
    procedure resolveFrequencyForSetRadioStation(intent: INSetRadioStationIntent; withCompletion: Pointer); cdecl;
    procedure resolvePresetNumberForSetRadioStation(intent: INSetRadioStationIntent; withCompletion: Pointer); cdecl;
    procedure resolveRadioTypeForSetRadioStation(intent: INSetRadioStationIntent; withCompletion: Pointer); cdecl;
    procedure resolveStationNameForSetRadioStation(intent: INSetRadioStationIntent; withCompletion: Pointer); cdecl;
  end;

  INSearchForMessagesIntentClass = interface(INIntentClass)
    ['{521BCC34-4D4D-4144-B89B-DF7385CF342E}']
  end;

  INSearchForMessagesIntent = interface(INIntent)
    ['{EC123A19-A6A8-46A0-92B1-0186731002A5}']
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
      conversationIdentifiers: NSArray): Pointer; cdecl;
    // [MethodName('initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:groupNames:')]
    // function initWithRecipientsSenders(recipients: NSArray; senders: NSArray; searchTerms: NSArray; attributes: INMessageAttributeOptions; dateTimeRange: INDateComponentsRange; identifiers: NSArray; notificationIdentifiers: NSArray; groupNames: NSArray): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 11.0), watchos(3.2, 4.0), macosx(10.12, 10.13))
    // [MethodName('initWithRecipients:senders:searchTerms:attributes:dateTimeRange:identifiers:notificationIdentifiers:speakableGroupNames:')]
    // function initWithRecipientsSenders(recipients: NSArray; senders: NSArray; searchTerms: NSArray; attributes: INMessageAttributeOptions; dateTimeRange: INDateComponentsRange; identifiers: NSArray; notificationIdentifiers: NSArray; speakableGroupNames: NSArray): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(11.0, 12.0), watchos(4.0, 5.0), macosx(10.13, 10.14))
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
    ['{FF1DB889-FC47-4FF4-ACBA-418F4B5EDF32}']
    procedure confirmSearchForMessages(intent: INSearchForMessagesIntent; completion: Pointer); cdecl;
    procedure handleSearchForMessages(intent: INSearchForMessagesIntent; completion: Pointer); cdecl;
    procedure resolveAttributesForSearchForMessages(intent: INSearchForMessagesIntent; withCompletion: Pointer); cdecl;
    procedure resolveDateTimeRangeForSearchForMessages(intent: INSearchForMessagesIntent; withCompletion: Pointer); cdecl;
    procedure resolveGroupNamesForSearchForMessages(intent: INSearchForMessagesIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolveGroupNamesForSearchForMessages:withCompletion: is deprecated. Use resolveSpeakableGroupNamesForSearchForMessages:withCompletion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveRecipientsForSearchForMessages(intent: INSearchForMessagesIntent; withCompletion: Pointer); cdecl;
    procedure resolveSendersForSearchForMessages(intent: INSearchForMessagesIntent; withCompletion: Pointer); cdecl;
    procedure resolveSpeakableGroupNamesForSearchForMessages(intent: INSearchForMessagesIntent; withCompletion: Pointer); cdecl;
  end;

  INSendMessageIntentClass = interface(INIntentClass)
    ['{2C3BA03A-446C-4935-B160-EFDDC86FBA59}']
  end;

  INSendMessageIntent = interface(INIntent)
    ['{B37BDD40-D4A2-4A6D-AF01-D26209BF4757}']
    function attachments: NSArray; cdecl;
    function content: NSString; cdecl;
    function conversationIdentifier: NSString; cdecl;
    function groupName: NSString; cdecl; // API_DEPRECATED("Use speakableGroupNames instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    function initWithRecipients(recipients: NSArray; outgoingMessageType: INOutgoingMessageType; content: NSString;
      speakableGroupName: INSpeakableString; conversationIdentifier: NSString; serviceName: NSString; sender: INPerson; attachments: NSArray): Pointer; overload; cdecl;
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
    ['{6312316C-CFBC-4DB7-8142-EEE05C474BF8}']
    procedure confirmSendMessage(intent: INSendMessageIntent; completion: Pointer); cdecl;
    procedure handleSendMessage(intent: INSendMessageIntent; completion: Pointer); cdecl;
    procedure resolveContentForSendMessage(intent: INSendMessageIntent; withCompletion: Pointer); cdecl;
    procedure resolveGroupNameForSendMessage(intent: INSendMessageIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolveGroupNameForSendMessage:withCompletion: is deprecated. Use resolveSpeakableGroupNameForSendMessage:withCompletion: instead", ios(10.0, 11.0), macos(12.0, 12.0), watchos(3.2, 4.0))
    procedure resolveOutgoingMessageTypeForSendMessage(intent: INSendMessageIntent; withCompletion: Pointer); cdecl;
    [MethodName('resolveRecipientsForSendMessage:completion:')]
    procedure resolveRecipientsForSendMessageCompletion(intent: INSendMessageIntent; completion: Pointer); cdecl;
    [MethodName('resolveRecipientsForSendMessage:withCompletion:')]
    procedure resolveRecipientsForSendMessageWithCompletion(intent: INSendMessageIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolveRecipientsForSendMessage:withCompletion: is deprecated. Use resolveRecipientsForSendMessage:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveSpeakableGroupNameForSendMessage(intent: INSendMessageIntent; withCompletion: Pointer); cdecl;
  end;

  INSetMessageAttributeIntentClass = interface(INIntentClass)
    ['{5BF70F9F-3ECE-4DC2-9227-3008FD3B19D0}']
  end;

  INSetMessageAttributeIntent = interface(INIntent)
    ['{ADC16693-E396-42BB-8D6A-165E3269D80E}']
    function attribute: INMessageAttribute; cdecl;
    function identifiers: NSArray; cdecl;
    function initWithIdentifiers(identifiers: NSArray; attribute: INMessageAttribute): Pointer; cdecl;
  end;
  TINSetMessageAttributeIntent = class(TOCGenericImport<INSetMessageAttributeIntentClass, INSetMessageAttributeIntent>) end;

  INSetMessageAttributeIntentHandling = interface(IObjectiveC)
    ['{D4143106-5EE0-4A1D-9A78-F01E89D886AA}']
    procedure confirmSetMessageAttribute(intent: INSetMessageAttributeIntent; completion: Pointer); cdecl;
    procedure handleSetMessageAttribute(intent: INSetMessageAttributeIntent; completion: Pointer); cdecl;
    procedure resolveAttributeForSetMessageAttribute(intent: INSetMessageAttributeIntent; withCompletion: Pointer); cdecl;
  end;

  INAddTasksIntentClass = interface(INIntentClass)
    ['{27098382-B94E-4946-8DC1-CE91F1B35566}']
  end;

  INAddTasksIntent = interface(INIntent)
    ['{0B36A346-5497-444E-A0EC-0DB60A902A57}']
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
    ['{7824DE24-CEFC-4C27-8D10-A0CA4AEF5756}']
    procedure confirmAddTasks(intent: INAddTasksIntent; completion: Pointer); cdecl;
    procedure handleAddTasks(intent: INAddTasksIntent; completion: Pointer); cdecl;
    procedure resolvePriorityForAddTasks(intent: INAddTasksIntent; withCompletion: Pointer); cdecl;
    procedure resolveSpatialEventTriggerForAddTasks(intent: INAddTasksIntent; withCompletion: Pointer); cdecl;
    [MethodName('resolveTargetTaskListForAddTasks:completion:')]
    procedure resolveTargetTaskListForAddTasksCompletion(intent: INAddTasksIntent; completion: Pointer); cdecl;
    [MethodName('resolveTargetTaskListForAddTasks:withCompletion:')]
    procedure resolveTargetTaskListForAddTasksWithCompletion(intent: INAddTasksIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolveTargetTaskListForAddTasks:withCompletion: is deprecated. Use resolveTargetTaskListForAddTasks:completion: instead", ios(11.0, 13.0), watchos(4.0, 6.0))
    procedure resolveTaskTitlesForAddTasks(intent: INAddTasksIntent; withCompletion: Pointer); cdecl;
    [MethodName('resolveTemporalEventTriggerForAddTasks:completion:')]
    procedure resolveTemporalEventTriggerForAddTasksCompletion(intent: INAddTasksIntent; completion: Pointer); cdecl;
    [MethodName('resolveTemporalEventTriggerForAddTasks:withCompletion:')]
    procedure resolveTemporalEventTriggerForAddTasksWithCompletion(intent: INAddTasksIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolveTemporalEventTriggerForAddTasks:withCompletion: is deprecated. Use resolveTemporalEventTriggerForAddTasks:completion: instead", ios(11.0, 13.0), watchos(4.0, 6.0))
  end;

  INAppendToNoteIntentClass = interface(INIntentClass)
    ['{08E51899-B392-407F-876D-432AABB0A4BD}']
  end;

  INAppendToNoteIntent = interface(INIntent)
    ['{268FFA12-26E9-4E7C-A2DD-6FD22964BB1A}']
    function content: INNoteContent; cdecl;
    function initWithTargetNote(targetNote: INNote; content: INNoteContent): Pointer; cdecl;
    function targetNote: INNote; cdecl;
  end;
  TINAppendToNoteIntent = class(TOCGenericImport<INAppendToNoteIntentClass, INAppendToNoteIntent>) end;

  INAppendToNoteIntentHandling = interface(IObjectiveC)
    ['{73064530-D741-4AFA-A941-B1D01502953F}']
    procedure confirmAppendToNote(intent: INAppendToNoteIntent; completion: Pointer); cdecl;
    procedure handleAppendToNote(intent: INAppendToNoteIntent; completion: Pointer); cdecl;
    procedure resolveContentForAppendToNote(intent: INAppendToNoteIntent; withCompletion: Pointer); cdecl;
    procedure resolveTargetNoteForAppendToNote(intent: INAppendToNoteIntent; withCompletion: Pointer); cdecl;
  end;

  INCreateNoteIntentClass = interface(INIntentClass)
    ['{C98F5546-2504-4A58-BB15-8E880F83FDE1}']
  end;

  INCreateNoteIntent = interface(INIntent)
    ['{60050A07-C885-4503-ABC4-EF2B00ED3A95}']
    function content: INNoteContent; cdecl;
    function groupName: INSpeakableString; cdecl;
    function initWithTitle(title: INSpeakableString; content: INNoteContent; groupName: INSpeakableString): Pointer; cdecl;
    function title: INSpeakableString; cdecl;
  end;
  TINCreateNoteIntent = class(TOCGenericImport<INCreateNoteIntentClass, INCreateNoteIntent>) end;

  INCreateNoteIntentHandling = interface(IObjectiveC)
    ['{F57D5D0B-E381-4E7F-B097-AADC31540F3F}']
    procedure confirmCreateNote(intent: INCreateNoteIntent; completion: Pointer); cdecl;
    procedure handleCreateNote(intent: INCreateNoteIntent; completion: Pointer); cdecl;
    procedure resolveContentForCreateNote(intent: INCreateNoteIntent; withCompletion: Pointer); cdecl;
    procedure resolveGroupNameForCreateNote(intent: INCreateNoteIntent; withCompletion: Pointer); cdecl;
    procedure resolveTitleForCreateNote(intent: INCreateNoteIntent; withCompletion: Pointer); cdecl;
  end;

  INCreateTaskListIntentClass = interface(INIntentClass)
    ['{56607C0E-48EC-4E5A-A73F-338D25000331}']
  end;

  INCreateTaskListIntent = interface(INIntent)
    ['{CD20A512-935D-494E-81DF-2FF447520379}']
    function groupName: INSpeakableString; cdecl;
    function initWithTitle(title: INSpeakableString; taskTitles: NSArray; groupName: INSpeakableString): Pointer; cdecl;
    function taskTitles: NSArray; cdecl;
    function title: INSpeakableString; cdecl;
  end;
  TINCreateTaskListIntent = class(TOCGenericImport<INCreateTaskListIntentClass, INCreateTaskListIntent>) end;

  INCreateTaskListIntentHandling = interface(IObjectiveC)
    ['{29EE40E7-D208-416D-AE58-62113268922A}']
    procedure confirmCreateTaskList(intent: INCreateTaskListIntent; completion: Pointer); cdecl;
    procedure handleCreateTaskList(intent: INCreateTaskListIntent; completion: Pointer); cdecl;
    procedure resolveGroupNameForCreateTaskList(intent: INCreateTaskListIntent; withCompletion: Pointer); cdecl;
    procedure resolveTaskTitlesForCreateTaskList(intent: INCreateTaskListIntent; withCompletion: Pointer); cdecl;
    procedure resolveTitleForCreateTaskList(intent: INCreateTaskListIntent; withCompletion: Pointer); cdecl;
  end;

  INDeleteTasksIntentClass = interface(INIntentClass)
    ['{B6DFE3D1-69C0-40B3-B395-76E632557B76}']
  end;

  INDeleteTasksIntent = interface(INIntent)
    ['{DEAF2155-A70A-4384-B725-2019E20D7F4D}']
    function all: NSNumber; cdecl;
    function initWithTaskList(taskList: INTaskList; tasks: NSArray; all: NSNumber): Pointer; cdecl;
    function taskList: INTaskList; cdecl;
    function tasks: NSArray; cdecl;
  end;
  TINDeleteTasksIntent = class(TOCGenericImport<INDeleteTasksIntentClass, INDeleteTasksIntent>) end;

  INDeleteTasksIntentHandling = interface(IObjectiveC)
    ['{C6CA34B8-1A7B-4128-89D6-E8C0AF934C27}']
    procedure confirmDeleteTasks(intent: INDeleteTasksIntent; completion: Pointer); cdecl;
    procedure handleDeleteTasks(intent: INDeleteTasksIntent; completion: Pointer); cdecl;
    procedure resolveTaskListForDeleteTasks(intent: INDeleteTasksIntent; withCompletion: Pointer); cdecl;
    procedure resolveTasksForDeleteTasks(intent: INDeleteTasksIntent; withCompletion: Pointer); cdecl;
  end;

  INSearchForNotebookItemsIntentClass = interface(INIntentClass)
    ['{61607666-3224-4068-B01A-3B2C97F6A6A8}']
  end;

  INSearchForNotebookItemsIntent = interface(INIntent)
    ['{6746BB37-44BF-4DE5-9A0D-1925886F1338}']
    function content: NSString; cdecl;
    function dateSearchType: INDateSearchType; cdecl;
    function dateTime: INDateComponentsRange; cdecl;
    function initWithTitle(title: INSpeakableString; content: NSString; itemType: INNotebookItemType; status: INTaskStatus; location: CLPlacemark;
      locationSearchType: INLocationSearchType; dateTime: INDateComponentsRange; dateSearchType: INDateSearchType;
      notebookItemIdentifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(11.2, 13.0), watchos(4.2, 6.0))
    function initWithTitle(title: INSpeakableString; content: NSString; itemType: INNotebookItemType; status: INTaskStatus; location: CLPlacemark;
      locationSearchType: INLocationSearchType; dateTime: INDateComponentsRange; dateSearchType: INDateSearchType): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(11.0, 11.2), watchos(4.0, 4.2))
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
    ['{F32799FC-703C-4E0F-98E5-651A27ACA6C6}']
    procedure confirmSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; completion: Pointer); cdecl;
    procedure handleSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; completion: Pointer); cdecl;
    procedure resolveContentForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveDateSearchTypeForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveDateTimeForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveItemTypeForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveLocationForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveLocationSearchTypeForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveStatusForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveTaskPriorityForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveTemporalEventTriggerTypesForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
    procedure resolveTitleForSearchForNotebookItems(intent: INSearchForNotebookItemsIntent; withCompletion: Pointer); cdecl;
  end;

  INSetTaskAttributeIntentClass = interface(INIntentClass)
    ['{26E9E9A1-AF17-40E4-8E51-3665C2806822}']
  end;

  INSetTaskAttributeIntent = interface(INIntent)
    ['{91A59967-4628-4DB3-AD8B-57E0ED7CD3A3}']
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
    ['{516B4B9D-255E-4E03-8D7F-93659A0F373B}']
    procedure confirmSetTaskAttribute(intent: INSetTaskAttributeIntent; completion: Pointer); cdecl;
    procedure handleSetTaskAttribute(intent: INSetTaskAttributeIntent; completion: Pointer); cdecl;
    procedure resolvePriorityForSetTaskAttribute(intent: INSetTaskAttributeIntent; withCompletion: Pointer); cdecl;
    procedure resolveSpatialEventTriggerForSetTaskAttribute(intent: INSetTaskAttributeIntent; withCompletion: Pointer); cdecl;
    procedure resolveStatusForSetTaskAttribute(intent: INSetTaskAttributeIntent; withCompletion: Pointer); cdecl;
    procedure resolveTargetTaskForSetTaskAttribute(intent: INSetTaskAttributeIntent; withCompletion: Pointer); cdecl;
    procedure resolveTaskTitleForSetTaskAttribute(intent: INSetTaskAttributeIntent; withCompletion: Pointer); cdecl;
    [MethodName('resolveTemporalEventTriggerForSetTaskAttribute:completion:')]
    procedure resolveTemporalEventTriggerForSetTaskAttributeCompletion(intent: INSetTaskAttributeIntent; completion: Pointer); cdecl;
    [MethodName('resolveTemporalEventTriggerForSetTaskAttribute:withCompletion:')]
    procedure resolveTemporalEventTriggerForSetTaskAttributeWithCompletion(intent: INSetTaskAttributeIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolveTemporalEventTriggerForSetTaskAttribute:withCompletion: is deprecated. Use resolveTemporalEventTriggerForSetTaskAttribute:completion: instead", ios(11.0, 13.0), watchos(4.0, 6.0))
  end;

  INSnoozeTasksIntentClass = interface(INIntentClass)
    ['{0DD3EE57-ADAF-46C8-9BD6-B2BF84010039}']
  end;

  INSnoozeTasksIntent = interface(INIntent)
    ['{BD3F74AC-496D-4DBA-9725-771ABE933D41}']
    function all: NSNumber; cdecl;
    function initWithTasks(tasks: NSArray; nextTriggerTime: INDateComponentsRange; all: NSNumber): Pointer; cdecl;
    function nextTriggerTime: INDateComponentsRange; cdecl;
    function tasks: NSArray; cdecl;
  end;
  TINSnoozeTasksIntent = class(TOCGenericImport<INSnoozeTasksIntentClass, INSnoozeTasksIntent>) end;

  INSnoozeTasksIntentHandling = interface(IObjectiveC)
    ['{4FA3FE47-B094-495C-ABE4-45CD4D7BCFE2}']
    procedure confirmSnoozeTasks(intent: INSnoozeTasksIntent; completion: Pointer); cdecl;
    procedure handleSnoozeTasks(intent: INSnoozeTasksIntent; completion: Pointer); cdecl;
    procedure resolveNextTriggerTimeForSnoozeTasks(intent: INSnoozeTasksIntent; withCompletion: Pointer); cdecl;
    procedure resolveTasksForSnoozeTasks(intent: INSnoozeTasksIntent; withCompletion: Pointer); cdecl;
  end;

  INPayBillIntentClass = interface(INIntentClass)
    ['{DD2296CB-E261-411D-9F5B-AE3314B28509}']
  end;

  INPayBillIntent = interface(INIntent)
    ['{CC08A18D-5E9A-4198-B025-13FBD7004A77}']
    function billPayee: INBillPayee; cdecl;
    function billType: INBillType; cdecl;
    function dueDate: INDateComponentsRange; cdecl;
    function fromAccount: INPaymentAccount; cdecl;
    function initWithBillPayee(billPayee: INBillPayee; fromAccount: INPaymentAccount; transactionAmount: INPaymentAmount;
      transactionScheduledDate: INDateComponentsRange; transactionNote: NSString; billType: INBillType; dueDate: INDateComponentsRange): Pointer; cdecl;
    function transactionAmount: INPaymentAmount; cdecl;
    function transactionNote: NSString; cdecl;
    function transactionScheduledDate: INDateComponentsRange; cdecl;
  end;
  TINPayBillIntent = class(TOCGenericImport<INPayBillIntentClass, INPayBillIntent>) end;

  INPayBillIntentHandling = interface(IObjectiveC)
    ['{E6502E10-9B7A-45FA-BEE1-411E89230369}']
    procedure confirmPayBill(intent: INPayBillIntent; completion: Pointer); cdecl;
    procedure handlePayBill(intent: INPayBillIntent; completion: Pointer); cdecl;
    procedure resolveBillPayeeForPayBill(intent: INPayBillIntent; withCompletion: Pointer); cdecl;
    procedure resolveBillTypeForPayBill(intent: INPayBillIntent; withCompletion: Pointer); cdecl;
    procedure resolveDueDateForPayBill(intent: INPayBillIntent; withCompletion: Pointer); cdecl;
    procedure resolveFromAccountForPayBill(intent: INPayBillIntent; withCompletion: Pointer); cdecl;
    procedure resolveTransactionAmountForPayBill(intent: INPayBillIntent; withCompletion: Pointer); cdecl;
    procedure resolveTransactionNoteForPayBill(intent: INPayBillIntent; withCompletion: Pointer); cdecl;
    procedure resolveTransactionScheduledDateForPayBill(intent: INPayBillIntent; withCompletion: Pointer); cdecl;
  end;

  INRequestPaymentIntentClass = interface(INIntentClass)
    ['{EAFB77EA-BD7A-4E6B-9AE1-CF349A0CEEE7}']
  end;

  INRequestPaymentIntent = interface(INIntent)
    ['{C3AD021C-5723-47D2-A88F-2CB02B4DD3E6}']
    function currencyAmount: INCurrencyAmount; cdecl;
    function initWithPayer(payer: INPerson; currencyAmount: INCurrencyAmount; note: NSString): Pointer; cdecl;
    function note: NSString; cdecl;
    function payer: INPerson; cdecl;
  end;
  TINRequestPaymentIntent = class(TOCGenericImport<INRequestPaymentIntentClass, INRequestPaymentIntent>) end;

  INRequestPaymentIntentHandling = interface(IObjectiveC)
    ['{F77AB608-C7B6-404A-B44A-48B125B83652}']
    procedure confirmRequestPayment(intent: INRequestPaymentIntent; completion: Pointer); cdecl;
    procedure handleRequestPayment(intent: INRequestPaymentIntent; completion: Pointer); cdecl;
    [MethodName('resolveCurrencyAmountForRequestPayment:completion:')]
    procedure resolveCurrencyAmountForRequestPaymentCompletion(intent: INRequestPaymentIntent; completion: Pointer); cdecl;
    [MethodName('resolveCurrencyAmountForRequestPayment:withCompletion:')]
    procedure resolveCurrencyAmountForRequestPaymentWithCompletion(intent: INRequestPaymentIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolveCurrencyAmountForRequestPayment:withCompletion: is deprecated. Use resolveCurrencyAmountForRequestPayment:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveNoteForRequestPayment(intent: INRequestPaymentIntent; withCompletion: Pointer); cdecl;
    [MethodName('resolvePayerForRequestPayment:completion:')]
    procedure resolvePayerForRequestPaymentCompletion(intent: INRequestPaymentIntent; completion: Pointer); cdecl;
    [MethodName('resolvePayerForRequestPayment:withCompletion:')]
    procedure resolvePayerForRequestPaymentWithCompletion(intent: INRequestPaymentIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolvePayerForRequestPayment:withCompletion: is deprecated. Use resolvePayerForRequestPayment:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INSearchForAccountsIntentClass = interface(INIntentClass)
    ['{42C0AC68-AA01-4613-A2B2-E6AEFB3B72F2}']
  end;

  INSearchForAccountsIntent = interface(INIntent)
    ['{D98C1DB7-E7D7-4C8C-9EC6-9F974D59D806}']
    function accountNickname: INSpeakableString; cdecl;
    function accountType: INAccountType; cdecl;
    function initWithAccountNickname(accountNickname: INSpeakableString; accountType: INAccountType; organizationName: INSpeakableString;
      requestedBalanceType: INBalanceType): Pointer; cdecl;
    function organizationName: INSpeakableString; cdecl;
    function requestedBalanceType: INBalanceType; cdecl;
  end;
  TINSearchForAccountsIntent = class(TOCGenericImport<INSearchForAccountsIntentClass, INSearchForAccountsIntent>) end;

  INSearchForAccountsIntentHandling = interface(IObjectiveC)
    ['{43625D10-5377-4E20-98E9-C3132BE15CFD}']
    procedure confirmSearchForAccounts(intent: INSearchForAccountsIntent; completion: Pointer); cdecl;
    procedure handleSearchForAccounts(intent: INSearchForAccountsIntent; completion: Pointer); cdecl;
    procedure resolveAccountNicknameForSearchForAccounts(intent: INSearchForAccountsIntent; withCompletion: Pointer); cdecl;
    procedure resolveAccountTypeForSearchForAccounts(intent: INSearchForAccountsIntent; withCompletion: Pointer); cdecl;
    procedure resolveOrganizationNameForSearchForAccounts(intent: INSearchForAccountsIntent; withCompletion: Pointer); cdecl;
    procedure resolveRequestedBalanceTypeForSearchForAccounts(intent: INSearchForAccountsIntent; withCompletion: Pointer); cdecl;
  end;

  INSearchForBillsIntentClass = interface(INIntentClass)
    ['{5FC0612B-877A-427B-9731-504FA5C69564}']
  end;

  INSearchForBillsIntent = interface(INIntent)
    ['{12C47B06-3A88-4571-9C3A-D93AEB6B4509}']
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
    ['{E3A1CC99-8397-44C8-AE93-070263349A59}']
    procedure confirmSearchForBills(intent: INSearchForBillsIntent; completion: Pointer); cdecl;
    procedure handleSearchForBills(intent: INSearchForBillsIntent; completion: Pointer); cdecl;
    procedure resolveBillPayeeForSearchForBills(intent: INSearchForBillsIntent; withCompletion: Pointer); cdecl;
    procedure resolveBillTypeForSearchForBills(intent: INSearchForBillsIntent; withCompletion: Pointer); cdecl;
    procedure resolveDueDateRangeForSearchForBills(intent: INSearchForBillsIntent; withCompletion: Pointer); cdecl;
    procedure resolvePaymentDateRangeForSearchForBills(intent: INSearchForBillsIntent; withCompletion: Pointer); cdecl;
    procedure resolveStatusForSearchForBills(intent: INSearchForBillsIntent; withCompletion: Pointer); cdecl;
  end;

  INSendPaymentIntentClass = interface(INIntentClass)
    ['{A2CF0146-80FA-4E12-A8BB-2170512C9406}']
  end;

  INSendPaymentIntent = interface(INIntent)
    ['{13CC5421-3285-4CB7-B6FA-73487A3DF13D}']
    function currencyAmount: INCurrencyAmount; cdecl;
    function initWithPayee(payee: INPerson; currencyAmount: INCurrencyAmount; note: NSString): Pointer; cdecl;
    function note: NSString; cdecl;
    function payee: INPerson; cdecl;
  end;
  TINSendPaymentIntent = class(TOCGenericImport<INSendPaymentIntentClass, INSendPaymentIntent>) end;

  INSendPaymentIntentHandling = interface(IObjectiveC)
    ['{B9EC7396-B46C-4C8A-9352-C69FEA8DE5A8}']
    procedure confirmSendPayment(intent: INSendPaymentIntent; completion: Pointer); cdecl;
    procedure handleSendPayment(intent: INSendPaymentIntent; completion: Pointer); cdecl;
    [MethodName('resolveCurrencyAmountForSendPayment:completion:')]
    procedure resolveCurrencyAmountForSendPaymentCompletion(intent: INSendPaymentIntent; completion: Pointer); cdecl;
    [MethodName('resolveCurrencyAmountForSendPayment:withCompletion:')]
    procedure resolveCurrencyAmountForSendPaymentWithCompletion(intent: INSendPaymentIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolveCurrencyAmountForSendPayment:withCompletion: is deprecated. Use resolveCurrencyAmountForSendPayment:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
    procedure resolveNoteForSendPayment(intent: INSendPaymentIntent; withCompletion: Pointer); cdecl;
    [MethodName('resolvePayeeForSendPayment:completion:')]
    procedure resolvePayeeForSendPaymentCompletion(intent: INSendPaymentIntent; completion: Pointer); cdecl;
    [MethodName('resolvePayeeForSendPayment:withCompletion:')]
    procedure resolvePayeeForSendPaymentWithCompletion(intent: INSendPaymentIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("resolvePayeeForSendPayment:withCompletion: is deprecated. Use resolvePayeeForSendPayment:completion: instead", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INTransferMoneyIntentClass = interface(INIntentClass)
    ['{24A27321-5F73-4899-AD2D-125ADFCAB519}']
  end;

  INTransferMoneyIntent = interface(INIntent)
    ['{311A6E56-EE50-4326-8793-53EE15BBD770}']
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
    ['{F01BA7EB-B451-4B1F-8944-FF3AB3D788E1}']
    procedure confirmTransferMoney(intent: INTransferMoneyIntent; completion: Pointer); cdecl;
    procedure handleTransferMoney(intent: INTransferMoneyIntent; completion: Pointer); cdecl;
    procedure resolveFromAccountForTransferMoney(intent: INTransferMoneyIntent; withCompletion: Pointer); cdecl;
    procedure resolveToAccountForTransferMoney(intent: INTransferMoneyIntent; withCompletion: Pointer); cdecl;
    procedure resolveTransactionAmountForTransferMoney(intent: INTransferMoneyIntent; withCompletion: Pointer); cdecl;
    procedure resolveTransactionNoteForTransferMoney(intent: INTransferMoneyIntent; withCompletion: Pointer); cdecl;
    procedure resolveTransactionScheduledDateForTransferMoney(intent: INTransferMoneyIntent; withCompletion: Pointer); cdecl;
  end;

  INSearchForPhotosIntentClass = interface(INIntentClass)
    ['{09E92D10-C9EC-40AB-8C2A-F3F20F123692}']
  end;

  INSearchForPhotosIntent = interface(INIntent)
    ['{52C46AB0-C6B7-4946-AEE5-CCC5DFB0E3C5}']
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
    ['{55E01987-9790-4A31-95EC-99067C8E18EC}']
    procedure confirmSearchForPhotos(intent: INSearchForPhotosIntent; completion: Pointer); cdecl;
    procedure handleSearchForPhotos(intent: INSearchForPhotosIntent; completion: Pointer); cdecl;
    procedure resolveAlbumNameForSearchForPhotos(intent: INSearchForPhotosIntent; withCompletion: Pointer); cdecl;
    procedure resolveDateCreatedForSearchForPhotos(intent: INSearchForPhotosIntent; withCompletion: Pointer); cdecl;
    procedure resolveLocationCreatedForSearchForPhotos(intent: INSearchForPhotosIntent; withCompletion: Pointer); cdecl;
    procedure resolvePeopleInPhotoForSearchForPhotos(intent: INSearchForPhotosIntent; withCompletion: Pointer); cdecl;
    procedure resolveSearchTermsForSearchForPhotos(intent: INSearchForPhotosIntent; withCompletion: Pointer); cdecl; // API_DEPRECATED("", ios(11.0, 15.0), watchos(4.0, 8.0))
  end;

  INStartPhotoPlaybackIntentClass = interface(INIntentClass)
    ['{BB2C5989-EEF5-42BF-84A9-36A6317A260D}']
  end;

  INStartPhotoPlaybackIntent = interface(INIntent)
    ['{9A5F1F3E-4766-4BF2-9FBE-6B1EBF34F01D}']
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
    ['{6025F89E-8792-49A8-B37D-7A8FF9F23920}']
    procedure confirmStartPhotoPlayback(intent: INStartPhotoPlaybackIntent; completion: Pointer); cdecl;
    procedure handleStartPhotoPlayback(intent: INStartPhotoPlaybackIntent; completion: Pointer); cdecl;
    procedure resolveAlbumNameForStartPhotoPlayback(intent: INStartPhotoPlaybackIntent; withCompletion: Pointer); cdecl;
    procedure resolveDateCreatedForStartPhotoPlayback(intent: INStartPhotoPlaybackIntent; withCompletion: Pointer); cdecl;
    procedure resolveLocationCreatedForStartPhotoPlayback(intent: INStartPhotoPlaybackIntent; withCompletion: Pointer); cdecl;
    procedure resolvePeopleInPhotoForStartPhotoPlayback(intent: INStartPhotoPlaybackIntent; withCompletion: Pointer); cdecl;
  end;

  INGetReservationDetailsIntentClass = interface(INIntentClass)
    ['{9A4F2B57-DBC1-4768-B9B6-9910ADFF9B28}']
  end;

  INGetReservationDetailsIntent = interface(INIntent)
    ['{D9BED388-4A9D-4DBD-945B-FEFA82513FEB}']
    function initWithReservationContainerReference(reservationContainerReference: INSpeakableString;
      reservationItemReferences: NSArray): Pointer; cdecl;
    function reservationContainerReference: INSpeakableString; cdecl;
    function reservationItemReferences: NSArray; cdecl;
  end;
  TINGetReservationDetailsIntent = class(TOCGenericImport<INGetReservationDetailsIntentClass, INGetReservationDetailsIntent>) end;

  INGetRideStatusIntentClass = interface(INIntentClass)
    ['{A2042BBA-F69B-4D0D-9725-5CDF434503BB}']
  end;

  INGetRideStatusIntent = interface(INIntent)
    ['{FA0911C3-8E1E-4039-B97E-360611B011AB}']
  end;
  TINGetRideStatusIntent = class(TOCGenericImport<INGetRideStatusIntentClass, INGetRideStatusIntent>) end;

  INGetRideStatusIntentHandling = interface(IObjectiveC)
    ['{2B2DB928-F00A-4D63-8D7A-F2318147487A}']
    procedure confirmGetRideStatus(intent: INGetRideStatusIntent; completion: Pointer); cdecl;
    procedure handleGetRideStatus(intent: INGetRideStatusIntent; completion: Pointer); cdecl;
    procedure startSendingUpdatesForGetRideStatus(intent: INGetRideStatusIntent; toObserver: Pointer); cdecl;
    procedure stopSendingUpdatesForGetRideStatus(intent: INGetRideStatusIntent); cdecl;
  end;

  INGetRideStatusIntentResponseObserver = interface(IObjectiveC)
    ['{8A336900-3834-45B5-97F8-819CDE7E119E}']
    procedure getRideStatusResponseDidUpdate(response: INGetRideStatusIntentResponse); cdecl;
  end;

  INListRideOptionsIntentClass = interface(INIntentClass)
    ['{EC0D4900-FD2D-4BF4-82A7-73829FDCE867}']
  end;

  INListRideOptionsIntent = interface(INIntent)
    ['{516D2618-3C5C-41D8-A87F-6E81544AA062}']
    function dropOffLocation: CLPlacemark; cdecl;
    function initWithPickupLocation(pickupLocation: CLPlacemark; dropOffLocation: CLPlacemark): Pointer; cdecl;
    function pickupLocation: CLPlacemark; cdecl;
  end;
  TINListRideOptionsIntent = class(TOCGenericImport<INListRideOptionsIntentClass, INListRideOptionsIntent>) end;

  INListRideOptionsIntentHandling = interface(IObjectiveC)
    ['{1E09DF5C-9952-4C86-9E7B-EA67B0DFE13B}']
    procedure confirmListRideOptions(intent: INListRideOptionsIntent; completion: Pointer); cdecl;
    procedure handleListRideOptions(intent: INListRideOptionsIntent; completion: Pointer); cdecl;
    procedure resolveDropOffLocationForListRideOptions(intent: INListRideOptionsIntent; withCompletion: Pointer); cdecl;
    procedure resolvePickupLocationForListRideOptions(intent: INListRideOptionsIntent; withCompletion: Pointer); cdecl;
  end;

  INRequestRideIntentClass = interface(INIntentClass)
    ['{13D08599-6C40-4A92-920F-E04CF71A1DDC}']
  end;

  INRequestRideIntent = interface(INIntent)
    ['{00EBC970-B64A-44F3-B3D0-1DA555FEDCE3}']
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
    ['{717380C2-030E-415B-A99C-7B81A25235F9}']
    procedure confirmRequestRide(intent: INRequestRideIntent; completion: Pointer); cdecl;
    procedure handleRequestRide(intent: INRequestRideIntent; completion: Pointer); cdecl;
    procedure resolveDropOffLocationForRequestRide(intent: INRequestRideIntent; withCompletion: Pointer); cdecl;
    procedure resolvePartySizeForRequestRide(intent: INRequestRideIntent; withCompletion: Pointer); cdecl;
    procedure resolvePickupLocationForRequestRide(intent: INRequestRideIntent; withCompletion: Pointer); cdecl;
    procedure resolveRideOptionNameForRequestRide(intent: INRequestRideIntent; withCompletion: Pointer); cdecl;
    procedure resolveScheduledPickupTimeForRequestRide(intent: INRequestRideIntent; withCompletion: Pointer); cdecl;
  end;

  INCancelRideIntentClass = interface(INIntentClass)
    ['{3BCDA8E1-4F6A-4EF7-91EC-12E382CDA26E}']
  end;

  INCancelRideIntent = interface(INIntent)
    ['{E97C0FE3-25E4-4CFF-90B9-A53C5BFBFEF5}']
    function initWithRideIdentifier(rideIdentifier: NSString): Pointer; cdecl;
    function rideIdentifier: NSString; cdecl;
  end;
  TINCancelRideIntent = class(TOCGenericImport<INCancelRideIntentClass, INCancelRideIntent>) end;

  INCancelRideIntentHandling = interface(IObjectiveC)
    ['{8723A27F-0011-4CE5-9118-AC11B961A647}']
    procedure confirmCancelRide(intent: INCancelRideIntent; completion: Pointer); cdecl;
    procedure handleCancelRide(intent: INCancelRideIntent; completion: Pointer); cdecl;
  end;

  INSendRideFeedbackIntentClass = interface(INIntentClass)
    ['{E4734DFC-0713-4229-9855-472492418F07}']
  end;

  INSendRideFeedbackIntent = interface(INIntent)
    ['{65E6D156-62D1-41DA-9133-252F268EA970}']
    function initWithRideIdentifier(rideIdentifier: NSString): Pointer; cdecl;
    function rating: NSNumber; cdecl;
    function rideIdentifier: NSString; cdecl;
    procedure setRating(rating: NSNumber); cdecl;
    procedure setTip(tip: INCurrencyAmount); cdecl;
    function tip: INCurrencyAmount; cdecl;
  end;
  TINSendRideFeedbackIntent = class(TOCGenericImport<INSendRideFeedbackIntentClass, INSendRideFeedbackIntent>) end;

  INSendRideFeedbackIntentHandling = interface(IObjectiveC)
    ['{18BE26BA-4354-4AD0-808F-9E6B280F2D63}']
    procedure confirmSendRideFeedback(sendRideFeedbackIntent: INSendRideFeedbackIntent; completion: Pointer); cdecl;
    procedure handleSendRideFeedback(sendRideFeedbackintent: INSendRideFeedbackIntent; completion: Pointer); cdecl;
  end;

  INGetVisualCodeIntentClass = interface(INIntentClass)
    ['{E7E0F115-BEDA-4C08-A780-A22AC2CC3915}']
  end;

  INGetVisualCodeIntent = interface(INIntent)
    ['{F3A8D568-9C81-4836-B906-17C6D6AFF00C}']
    function initWithVisualCodeType(visualCodeType: INVisualCodeType): Pointer; cdecl;
    function visualCodeType: INVisualCodeType; cdecl;
  end;
  TINGetVisualCodeIntent = class(TOCGenericImport<INGetVisualCodeIntentClass, INGetVisualCodeIntent>) end;

  INGetVisualCodeIntentHandling = interface(IObjectiveC)
    ['{D08B7EF3-6A12-4F36-BF1F-93A4DC9E758E}']
    procedure confirmGetVisualCode(intent: INGetVisualCodeIntent; completion: Pointer); cdecl;
    procedure handleGetVisualCode(intent: INGetVisualCodeIntent; completion: Pointer); cdecl;
    procedure resolveVisualCodeTypeForGetVisualCode(intent: INGetVisualCodeIntent; withCompletion: Pointer); cdecl;
  end;

  INCallsDomainHandling = interface(IObjectiveC)
    ['{DA9F4076-E164-414C-9598-B273A0C6DD6A}']
  end;

  INCarCommandsDomainHandling = interface(IObjectiveC)
    ['{5BD0C920-5B1D-485B-9040-6A1E70CAC649}']
  end;

  INCarPlayDomainHandling = interface(IObjectiveC)
    ['{99913F81-F7DF-4B09-8CEE-AB788FD6B765}']
  end;

  INWorkoutsDomainHandling = interface(IObjectiveC)
    ['{998CAF49-4F02-4B04-953D-33F71CDA2EC2}']
  end;

  INRadioDomainHandling = interface(IObjectiveC)
    ['{1F16FF0E-A686-46A2-881B-E6E51DA6903E}']
  end;

  INMessagesDomainHandling = interface(IObjectiveC)
    ['{C51BB669-597B-4800-AB3D-5C4D67FEECE9}']
  end;

  INPaymentsDomainHandling = interface(IObjectiveC)
    ['{1EA68B24-1859-44E0-BB40-55C1E348A01E}']
  end;

  INPhotosDomainHandling = interface(IObjectiveC)
    ['{EF067A61-C555-4AAB-AB2A-43A233DABD44}']
  end;

  INRidesharingDomainHandling = interface(IObjectiveC)
    ['{8EEF4B2A-560F-437A-B145-8A801DB54AEE}']
  end;

  INNotebookDomainHandling = interface(IObjectiveC)
    ['{334C2127-E1D5-4046-A6C6-2C653F9777BE}']
  end;

  INVisualCodeDomainHandling = interface(IObjectiveC)
    ['{C7DABB55-3D73-45CF-B6AB-E0F725D3F53C}']
  end;

  INInteractionClass = interface(NSObjectClass)
    ['{A0D72AD0-4D6D-4299-AF77-AA68D2982E8E}']
    {class} procedure deleteAllInteractionsWithCompletion(completion: TINInteractionBlockMethod1); cdecl;
    {class} procedure deleteInteractionsWithGroupIdentifier(groupIdentifier: NSString; completion: TINInteractionBlockMethod1); cdecl;
    {class} procedure deleteInteractionsWithIdentifiers(identifiers: NSArray; completion: TINInteractionBlockMethod1); cdecl;
  end;

  INInteraction = interface(NSObject)
    ['{2501C4EE-F168-4600-BF1B-1FB81BA3BEBC}']
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
    ['{F44B32DA-93D7-4E35-8085-8F81B539692E}']
    function alternativeSpeakableMatches: NSArray; cdecl;
    function identifier: NSString; cdecl; // API_DEPRECATED("Please use vocabularyIdentifier", ios(10.0, 11.0), watchos(3.2, 4.0))
    function pronunciationHint: NSString; cdecl;
    function spokenPhrase: NSString; cdecl;
    function vocabularyIdentifier: NSString; cdecl;
  end;

  INParameterClass = interface(NSObjectClass)
    ['{1EC57131-0EFC-4B03-8999-5EE54AD079A6}']
    {class} function parameterForClass(aClass: Pointer; keyPath: NSString): Pointer; cdecl;
  end;

  INParameter = interface(NSObject)
    ['{6F4DA1DE-6DBC-40E6-86C4-701D812FAA9C}']
    function indexForSubKeyPath(subKeyPath: NSString): NSUInteger; cdecl;
    function isEqualToParameter(parameter: INParameter): Boolean; cdecl;
    function parameterClass: Pointer; cdecl;
    function parameterKeyPath: NSString; cdecl;
    procedure setIndex(index: NSUInteger; forSubKeyPath: NSString); cdecl;
  end;
  TINParameter = class(TOCGenericImport<INParameterClass, INParameter>) end;

  INObjectSectionClass = interface(NSObjectClass)
    ['{B42BB7A9-19FA-45EE-B7C2-F6280A72F8B6}']
  end;

  INObjectSection = interface(NSObject)
    ['{574CB53D-658A-4D7D-A651-C440B6756117}']
    function initWithTitle(title: NSString; items: NSArray): Pointer; cdecl;
    function items: NSArray; cdecl;
    function title: NSString; cdecl;
  end;
  TINObjectSection = class(TOCGenericImport<INObjectSectionClass, INObjectSection>) end;

  INObjectCollectionClass = interface(NSObjectClass)
    ['{00D8FEF5-C7F9-4FDB-B24D-F25699A56FCC}']
  end;

  INObjectCollection = interface(NSObject)
    ['{8A91ED7B-2E49-40A0-A49A-DEF13EC6F07F}']
    function allItems: NSArray; cdecl;
    function initWithItems(items: NSArray): Pointer; cdecl;
    function initWithSections(sections: NSArray): Pointer; cdecl;
    function sections: NSArray; cdecl;
    procedure setUsesIndexedCollation(usesIndexedCollation: Boolean); cdecl;
    function usesIndexedCollation: Boolean; cdecl;
  end;
  TINObjectCollection = class(TOCGenericImport<INObjectCollectionClass, INObjectCollection>) end;

  INAnswerCallIntentResponseClass = interface(INIntentResponseClass)
    ['{76179579-6B78-47F6-8C04-0971A62393B2}']
  end;

  INAnswerCallIntentResponse = interface(INIntentResponse)
    ['{9360B31D-FA35-423C-8F6C-D507C7A7CA5D}']
    function callRecords: NSArray; cdecl;
    function code: INAnswerCallIntentResponseCode; cdecl;
    function initWithCode(code: INAnswerCallIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCallRecords(callRecords: NSArray); cdecl;
  end;
  TINAnswerCallIntentResponse = class(TOCGenericImport<INAnswerCallIntentResponseClass, INAnswerCallIntentResponse>) end;

  INHangUpCallIntentResponseClass = interface(INIntentResponseClass)
    ['{A075B0DF-D680-4DAC-9F95-4C76B587999A}']
  end;

  INHangUpCallIntentResponse = interface(INIntentResponse)
    ['{FFAD8DFC-F308-4C45-805A-F3D65CD7FBF0}']
    function code: INHangUpCallIntentResponseCode; cdecl;
    function initWithCode(code: INHangUpCallIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINHangUpCallIntentResponse = class(TOCGenericImport<INHangUpCallIntentResponseClass, INHangUpCallIntentResponse>) end;

  INSearchCallHistoryIntentResponseClass = interface(INIntentResponseClass)
    ['{3A73BB33-CFD1-44B8-BC44-1C0D70099880}']
  end;

  INSearchCallHistoryIntentResponse = interface(INIntentResponse)
    ['{51DA06B0-D860-4B4C-987D-83838CCEFFE3}']
    function callRecords: NSArray; cdecl;
    function code: INSearchCallHistoryIntentResponseCode; cdecl;
    function initWithCode(code: INSearchCallHistoryIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCallRecords(callRecords: NSArray); cdecl;
  end;
  TINSearchCallHistoryIntentResponse = class(TOCGenericImport<INSearchCallHistoryIntentResponseClass, INSearchCallHistoryIntentResponse>) end;

  INStartAudioCallIntentResponseClass = interface(INIntentResponseClass)
    ['{87D1154F-B961-438A-AD34-0B4649B4CD33}']
  end;

  INStartAudioCallIntentResponse = interface(INIntentResponse)
    ['{175E0574-BD3F-4009-894D-B87AF78E1948}']
    function code: INStartAudioCallIntentResponseCode; cdecl;
    function initWithCode(code: INStartAudioCallIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINStartAudioCallIntentResponse = class(TOCGenericImport<INStartAudioCallIntentResponseClass, INStartAudioCallIntentResponse>) end;

  INStartCallIntentResponseClass = interface(INIntentResponseClass)
    ['{C4428AC1-A9F7-48E6-86A3-85CF45F6092A}']
  end;

  INStartCallIntentResponse = interface(INIntentResponse)
    ['{44EBA66F-2727-4A0F-9FB0-0CCE606E6717}']
    function code: INStartCallIntentResponseCode; cdecl;
    function initWithCode(code: INStartCallIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINStartCallIntentResponse = class(TOCGenericImport<INStartCallIntentResponseClass, INStartCallIntentResponse>) end;

  INStartVideoCallIntentResponseClass = interface(INIntentResponseClass)
    ['{FC920A64-83DA-4CC0-8C5A-C12CEA19E0B6}']
  end;

  INStartVideoCallIntentResponse = interface(INIntentResponse)
    ['{6ED776DF-0F11-4FB6-82B3-78E82E247750}']
    function code: INStartVideoCallIntentResponseCode; cdecl;
    function initWithCode(code: INStartVideoCallIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINStartVideoCallIntentResponse = class(TOCGenericImport<INStartVideoCallIntentResponseClass, INStartVideoCallIntentResponse>) end;

  INActivateCarSignalIntentResponseClass = interface(INIntentResponseClass)
    ['{FF582985-7862-4891-96A5-0E0AB8A7FA0F}']
  end;

  INActivateCarSignalIntentResponse = interface(INIntentResponse)
    ['{C32D63D3-E81A-43D7-B64B-4019C2AF583E}']
    function code: INActivateCarSignalIntentResponseCode; cdecl;
    function initWithCode(code: INActivateCarSignalIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setSignals(signals: INCarSignalOptions); cdecl;
    function signals: INCarSignalOptions; cdecl;
  end;
  TINActivateCarSignalIntentResponse = class(TOCGenericImport<INActivateCarSignalIntentResponseClass, INActivateCarSignalIntentResponse>) end;

  INGetCarLockStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{5F767E3A-2B71-4CDE-9C2E-C4DAD7FEBAE5}']
  end;

  INGetCarLockStatusIntentResponse = interface(INIntentResponse)
    ['{A0AFDCD0-2D2E-4726-9B13-6BE8A25258F4}']
    function code: INGetCarLockStatusIntentResponseCode; cdecl;
    function initWithCode(code: INGetCarLockStatusIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function locked: NSNumber; cdecl;
    procedure setLocked(locked: NSNumber); cdecl;
  end;
  TINGetCarLockStatusIntentResponse = class(TOCGenericImport<INGetCarLockStatusIntentResponseClass, INGetCarLockStatusIntentResponse>) end;

  INGetCarPowerLevelStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{D97BAF13-FE42-4CD6-BD30-F7DE355EC475}']
  end;

  INGetCarPowerLevelStatusIntentResponse = interface(INIntentResponse)
    ['{0819235F-2788-4B6D-80C7-385E941C03F1}']
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
    ['{90494B83-55E6-456C-8398-4569B1C1C4CE}']
  end;

  INListCarsIntentResponse = interface(INIntentResponse)
    ['{30BCAC93-7167-43EE-82C0-FE114F6549FC}']
    function cars: NSArray; cdecl;
    function code: INListCarsIntentResponseCode; cdecl;
    function initWithCode(code: INListCarsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCars(cars: NSArray); cdecl;
  end;
  TINListCarsIntentResponse = class(TOCGenericImport<INListCarsIntentResponseClass, INListCarsIntentResponse>) end;

  INSaveProfileInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{CFC200B2-34D8-49CA-9A36-B836878D04C6}']
  end;

  INSaveProfileInCarIntentResponse = interface(INIntentResponse)
    ['{E5D35B12-7A82-4139-86DA-D5D3B50B0A65}']
    function code: INSaveProfileInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSaveProfileInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSaveProfileInCarIntentResponse = class(TOCGenericImport<INSaveProfileInCarIntentResponseClass, INSaveProfileInCarIntentResponse>) end;

  INSetAudioSourceInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{583F55CA-DA31-4623-A31F-5F95E107FC2E}']
  end;

  INSetAudioSourceInCarIntentResponse = interface(INIntentResponse)
    ['{3756ACE0-F09F-4EB1-9782-5D9F974414ED}']
    function code: INSetAudioSourceInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetAudioSourceInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetAudioSourceInCarIntentResponse = class(TOCGenericImport<INSetAudioSourceInCarIntentResponseClass, INSetAudioSourceInCarIntentResponse>) end;

  INSetCarLockStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{25EEBF62-AD3E-4861-98B6-C158D31D3D41}']
  end;

  INSetCarLockStatusIntentResponse = interface(INIntentResponse)
    ['{DFCC182D-A6DD-4041-A4F2-2398FD38D279}']
    function code: INSetCarLockStatusIntentResponseCode; cdecl;
    function initWithCode(code: INSetCarLockStatusIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetCarLockStatusIntentResponse = class(TOCGenericImport<INSetCarLockStatusIntentResponseClass, INSetCarLockStatusIntentResponse>) end;

  INSetClimateSettingsInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{5570422F-B6BE-4B51-B3FD-6AF1714E7C0E}']
  end;

  INSetClimateSettingsInCarIntentResponse = interface(INIntentResponse)
    ['{5D0ED466-C4A0-48B1-9726-C77555235984}']
    function code: INSetClimateSettingsInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetClimateSettingsInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetClimateSettingsInCarIntentResponse = class(TOCGenericImport<INSetClimateSettingsInCarIntentResponseClass,
    INSetClimateSettingsInCarIntentResponse>) end;

  INSetDefrosterSettingsInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{3B158203-7EF0-49B1-8530-6588D57CB55F}']
  end;

  INSetDefrosterSettingsInCarIntentResponse = interface(INIntentResponse)
    ['{92EC5BE7-95A1-410D-8D00-2860A32E3AC2}']
    function code: INSetDefrosterSettingsInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetDefrosterSettingsInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetDefrosterSettingsInCarIntentResponse = class(TOCGenericImport<INSetDefrosterSettingsInCarIntentResponseClass,
    INSetDefrosterSettingsInCarIntentResponse>) end;

  INSetProfileInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{A9C450A5-155F-4B0A-96F9-74CE405A1689}']
  end;

  INSetProfileInCarIntentResponse = interface(INIntentResponse)
    ['{8DCF9ED1-71BA-4839-B3A1-B5CF2C661A21}']
    function code: INSetProfileInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetProfileInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetProfileInCarIntentResponse = class(TOCGenericImport<INSetProfileInCarIntentResponseClass, INSetProfileInCarIntentResponse>) end;

  INSetSeatSettingsInCarIntentResponseClass = interface(INIntentResponseClass)
    ['{944276B8-2A5D-4001-AA81-5CA134174F26}']
  end;

  INSetSeatSettingsInCarIntentResponse = interface(INIntentResponse)
    ['{F7FACAFA-48B7-4B13-9804-1B957562810E}']
    function code: INSetSeatSettingsInCarIntentResponseCode; cdecl;
    function initWithCode(code: INSetSeatSettingsInCarIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetSeatSettingsInCarIntentResponse = class(TOCGenericImport<INSetSeatSettingsInCarIntentResponseClass, INSetSeatSettingsInCarIntentResponse>) end;

  INCancelWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{A9DA3939-5180-4A71-BB94-00E51ED5BD7F}']
  end;

  INCancelWorkoutIntentResponse = interface(INIntentResponse)
    ['{8F48AFFC-B574-4695-9B1F-0FD3B13E11F9}']
    function code: INCancelWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INCancelWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINCancelWorkoutIntentResponse = class(TOCGenericImport<INCancelWorkoutIntentResponseClass, INCancelWorkoutIntentResponse>) end;

  INEndWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{F13E35E2-A743-444F-88D1-4AAB6A73AC5E}']
  end;

  INEndWorkoutIntentResponse = interface(INIntentResponse)
    ['{C3F3F537-5CC0-426B-9EBC-7D918A6BD784}']
    function code: INEndWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INEndWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINEndWorkoutIntentResponse = class(TOCGenericImport<INEndWorkoutIntentResponseClass, INEndWorkoutIntentResponse>) end;

  INPauseWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{DB49F049-4401-418F-AB8D-B171232C0C73}']
  end;

  INPauseWorkoutIntentResponse = interface(INIntentResponse)
    ['{37D12083-E4C7-4CFA-B3F7-A477D4DF39BD}']
    function code: INPauseWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INPauseWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINPauseWorkoutIntentResponse = class(TOCGenericImport<INPauseWorkoutIntentResponseClass, INPauseWorkoutIntentResponse>) end;

  INResumeWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{361A6C04-8D01-467A-9BAF-4AEC0EA8EF86}']
  end;

  INResumeWorkoutIntentResponse = interface(INIntentResponse)
    ['{868D37B9-E634-4848-BA89-477FC4CFC368}']
    function code: INResumeWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INResumeWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINResumeWorkoutIntentResponse = class(TOCGenericImport<INResumeWorkoutIntentResponseClass, INResumeWorkoutIntentResponse>) end;

  INStartWorkoutIntentResponseClass = interface(INIntentResponseClass)
    ['{D9B88DEC-FA74-4C18-9032-E2F39C8C6C4C}']
  end;

  INStartWorkoutIntentResponse = interface(INIntentResponse)
    ['{7EDA07B7-10E6-4BF5-AB29-3364BAB3359A}']
    function code: INStartWorkoutIntentResponseCode; cdecl;
    function initWithCode(code: INStartWorkoutIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINStartWorkoutIntentResponse = class(TOCGenericImport<INStartWorkoutIntentResponseClass, INStartWorkoutIntentResponse>) end;

  INShareFocusStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{00BA6B15-BBCA-4E1C-A03C-6FC489E2408A}']
  end;

  INShareFocusStatusIntentResponse = interface(INIntentResponse)
    ['{3DB7B059-1EFA-48DD-9C44-5E6DB52E81BC}']
    function code: INShareFocusStatusIntentResponseCode; cdecl;
    function initWithCode(code: INShareFocusStatusIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINShareFocusStatusIntentResponse = class(TOCGenericImport<INShareFocusStatusIntentResponseClass, INShareFocusStatusIntentResponse>) end;

  INAddMediaIntentResponseClass = interface(INIntentResponseClass)
    ['{2EDA985B-881A-4F78-BE68-8057F2F67D52}']
  end;

  INAddMediaIntentResponse = interface(INIntentResponse)
    ['{1844A54C-78B4-46A7-9FFA-898C5FEA2C50}']
    function code: INAddMediaIntentResponseCode; cdecl;
    function initWithCode(code: INAddMediaIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINAddMediaIntentResponse = class(TOCGenericImport<INAddMediaIntentResponseClass, INAddMediaIntentResponse>) end;

  INPlayMediaIntentResponseClass = interface(INIntentResponseClass)
    ['{637D8079-89C1-4F6B-A580-79470CED277F}']
  end;

  INPlayMediaIntentResponse = interface(INIntentResponse)
    ['{2ED85831-F0C7-4759-B04F-4241A09618DB}']
    function code: INPlayMediaIntentResponseCode; cdecl;
    function initWithCode(code: INPlayMediaIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function nowPlayingInfo: NSDictionary; cdecl;
    procedure setNowPlayingInfo(nowPlayingInfo: NSDictionary); cdecl;
  end;
  TINPlayMediaIntentResponse = class(TOCGenericImport<INPlayMediaIntentResponseClass, INPlayMediaIntentResponse>) end;

  INSearchForMediaIntentResponseClass = interface(INIntentResponseClass)
    ['{38ACFEC5-4F1D-421A-94DD-21F0F0662661}']
  end;

  INSearchForMediaIntentResponse = interface(INIntentResponse)
    ['{84A78467-4DBA-47BD-82AA-E2DCA505FC08}']
    function code: INSearchForMediaIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForMediaIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function mediaItems: NSArray; cdecl;
    procedure setMediaItems(mediaItems: NSArray); cdecl;
  end;
  TINSearchForMediaIntentResponse = class(TOCGenericImport<INSearchForMediaIntentResponseClass, INSearchForMediaIntentResponse>) end;

  INUpdateMediaAffinityIntentResponseClass = interface(INIntentResponseClass)
    ['{74162DBC-70D6-4C10-86D6-37D863E2C3AF}']
  end;

  INUpdateMediaAffinityIntentResponse = interface(INIntentResponse)
    ['{82A41AEB-6954-4B7B-BF9E-826A182C05D7}']
    function code: INUpdateMediaAffinityIntentResponseCode; cdecl;
    function initWithCode(code: INUpdateMediaAffinityIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINUpdateMediaAffinityIntentResponse = class(TOCGenericImport<INUpdateMediaAffinityIntentResponseClass, INUpdateMediaAffinityIntentResponse>) end;

  INSetRadioStationIntentResponseClass = interface(INIntentResponseClass)
    ['{4274D31A-88DE-458F-9C1C-594D6DE6F424}']
  end;

  INSetRadioStationIntentResponse = interface(INIntentResponse)
    ['{44186C67-FB19-481E-9863-2F94C623587C}']
    function code: INSetRadioStationIntentResponseCode; cdecl;
    function initWithCode(code: INSetRadioStationIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetRadioStationIntentResponse = class(TOCGenericImport<INSetRadioStationIntentResponseClass, INSetRadioStationIntentResponse>) end;

  INSearchForMessagesIntentResponseClass = interface(INIntentResponseClass)
    ['{1B31BE5C-08AD-47DD-8701-93385022760B}']
  end;

  INSearchForMessagesIntentResponse = interface(INIntentResponse)
    ['{45342E71-3257-47B3-9A48-14B8A2FD753E}']
    function code: INSearchForMessagesIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForMessagesIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function messages: NSArray; cdecl;
    procedure setMessages(messages: NSArray); cdecl;
  end;
  TINSearchForMessagesIntentResponse = class(TOCGenericImport<INSearchForMessagesIntentResponseClass, INSearchForMessagesIntentResponse>) end;

  INSendMessageIntentResponseClass = interface(INIntentResponseClass)
    ['{39DAF1A4-FE52-4BF1-8509-0088D2341349}']
  end;

  INSendMessageIntentResponse = interface(INIntentResponse)
    ['{3002A322-3D38-4908-889A-8746CB6B4813}']
    function code: INSendMessageIntentResponseCode; cdecl;
    function initWithCode(code: INSendMessageIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function sentMessage: INMessage; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("sentMessages", ios(10.3, 16.0), watchos(3.2, 9.0))
    function sentMessages: NSArray; cdecl;
    procedure setSentMessage(sentMessage: INMessage); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("sentMessages", ios(10.3, 16.0), watchos(3.2, 9.0))
    procedure setSentMessages(sentMessages: NSArray); cdecl;
  end;
  TINSendMessageIntentResponse = class(TOCGenericImport<INSendMessageIntentResponseClass, INSendMessageIntentResponse>) end;

  INSetMessageAttributeIntentResponseClass = interface(INIntentResponseClass)
    ['{00530C54-362D-49AF-94F7-C4753F4E9F87}']
  end;

  INSetMessageAttributeIntentResponse = interface(INIntentResponse)
    ['{119F0BA0-9A6D-4511-AC46-14372ECE3909}']
    function code: INSetMessageAttributeIntentResponseCode; cdecl;
    function initWithCode(code: INSetMessageAttributeIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSetMessageAttributeIntentResponse = class(TOCGenericImport<INSetMessageAttributeIntentResponseClass, INSetMessageAttributeIntentResponse>) end;

  INAddTasksIntentResponseClass = interface(INIntentResponseClass)
    ['{6C2EA069-8855-421E-910C-1878E5901F6F}']
  end;

  INAddTasksIntentResponse = interface(INIntentResponse)
    ['{AF36E187-3380-44CF-A2A6-45A8CFD3E3EA}']
    function addedTasks: NSArray; cdecl;
    function code: INAddTasksIntentResponseCode; cdecl;
    function initWithCode(code: INAddTasksIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function modifiedTaskList: INTaskList; cdecl;
    procedure setAddedTasks(addedTasks: NSArray); cdecl;
    procedure setModifiedTaskList(modifiedTaskList: INTaskList); cdecl;
  end;
  TINAddTasksIntentResponse = class(TOCGenericImport<INAddTasksIntentResponseClass, INAddTasksIntentResponse>) end;

  INAppendToNoteIntentResponseClass = interface(INIntentResponseClass)
    ['{ACF35278-56F4-4603-B72E-C1DAAC358F0B}']
  end;

  INAppendToNoteIntentResponse = interface(INIntentResponse)
    ['{94F8D637-0753-46D3-85BB-6EB31FEC3838}']
    function code: INAppendToNoteIntentResponseCode; cdecl;
    function initWithCode(code: INAppendToNoteIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function note: INNote; cdecl;
    procedure setNote(note: INNote); cdecl;
  end;
  TINAppendToNoteIntentResponse = class(TOCGenericImport<INAppendToNoteIntentResponseClass, INAppendToNoteIntentResponse>) end;

  INCreateNoteIntentResponseClass = interface(INIntentResponseClass)
    ['{1C8B61A1-9F3F-44A5-816E-B5BCD60420E2}']
  end;

  INCreateNoteIntentResponse = interface(INIntentResponse)
    ['{F59E4A54-4A92-4A3F-A374-9477A21D1F4E}']
    function code: INCreateNoteIntentResponseCode; cdecl;
    function createdNote: INNote; cdecl;
    function initWithCode(code: INCreateNoteIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCreatedNote(createdNote: INNote); cdecl;
  end;
  TINCreateNoteIntentResponse = class(TOCGenericImport<INCreateNoteIntentResponseClass, INCreateNoteIntentResponse>) end;

  INCreateTaskListIntentResponseClass = interface(INIntentResponseClass)
    ['{A470DC0C-E1D8-4B9A-A66C-9E4D28DDFA47}']
  end;

  INCreateTaskListIntentResponse = interface(INIntentResponse)
    ['{28985EEF-92AF-475D-98AD-FDADB8A9B46B}']
    function code: INCreateTaskListIntentResponseCode; cdecl;
    function createdTaskList: INTaskList; cdecl;
    function initWithCode(code: INCreateTaskListIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCreatedTaskList(createdTaskList: INTaskList); cdecl;
  end;
  TINCreateTaskListIntentResponse = class(TOCGenericImport<INCreateTaskListIntentResponseClass, INCreateTaskListIntentResponse>) end;

  INDeleteTasksIntentResponseClass = interface(INIntentResponseClass)
    ['{42C39BBA-F53D-4BE8-9A13-B4A561E6730F}']
  end;

  INDeleteTasksIntentResponse = interface(INIntentResponse)
    ['{0FB0425B-2E3A-428F-B713-D05BCD022819}']
    function code: INDeleteTasksIntentResponseCode; cdecl;
    function deletedTasks: NSArray; cdecl;
    function initWithCode(code: INDeleteTasksIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setDeletedTasks(deletedTasks: NSArray); cdecl;
  end;
  TINDeleteTasksIntentResponse = class(TOCGenericImport<INDeleteTasksIntentResponseClass, INDeleteTasksIntentResponse>) end;

  INSearchForNotebookItemsIntentResponseClass = interface(INIntentResponseClass)
    ['{77A4F6D3-8D18-44B1-BC8E-16429FBA01F6}']
  end;

  INSearchForNotebookItemsIntentResponse = interface(INIntentResponse)
    ['{EA7AD8A7-03B2-4A9C-B526-606440CC5EF8}']
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
    ['{AED2726F-F33B-4A27-89B0-A3EFCC92988A}']
  end;

  INSetTaskAttributeIntentResponse = interface(INIntentResponse)
    ['{DA238A07-512D-468A-83E2-A3A670431B74}']
    function code: INSetTaskAttributeIntentResponseCode; cdecl;
    function initWithCode(code: INSetTaskAttributeIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function modifiedTask: INTask; cdecl;
    procedure setModifiedTask(modifiedTask: INTask); cdecl;
  end;
  TINSetTaskAttributeIntentResponse = class(TOCGenericImport<INSetTaskAttributeIntentResponseClass, INSetTaskAttributeIntentResponse>) end;

  INSnoozeTasksIntentResponseClass = interface(INIntentResponseClass)
    ['{1181D4E7-FE76-472F-8C98-EBE575B2DCC6}']
  end;

  INSnoozeTasksIntentResponse = interface(INIntentResponse)
    ['{7F80BBF9-18EC-416C-B944-075CFD875BC5}']
    function code: INSnoozeTasksIntentResponseCode; cdecl;
    function initWithCode(code: INSnoozeTasksIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setSnoozedTasks(snoozedTasks: NSArray); cdecl;
    function snoozedTasks: NSArray; cdecl;
  end;
  TINSnoozeTasksIntentResponse = class(TOCGenericImport<INSnoozeTasksIntentResponseClass, INSnoozeTasksIntentResponse>) end;

  INPayBillIntentResponseClass = interface(INIntentResponseClass)
    ['{0E7419EA-E7EC-4D60-A443-DE59953E18FE}']
  end;

  INPayBillIntentResponse = interface(INIntentResponse)
    ['{FEA95954-C467-4168-B4C3-1BC135811FC1}']
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
    ['{8DB1E49F-AD3A-4ECD-B233-8CE614DB8D40}']
  end;

  INRequestPaymentIntentResponse = interface(INIntentResponse)
    ['{30207288-C771-4DC2-8FB7-CC426879FC91}']
    function code: INRequestPaymentIntentResponseCode; cdecl;
    function initWithCode(code: INRequestPaymentIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function paymentRecord: INPaymentRecord; cdecl;
    procedure setPaymentRecord(paymentRecord: INPaymentRecord); cdecl;
  end;
  TINRequestPaymentIntentResponse = class(TOCGenericImport<INRequestPaymentIntentResponseClass, INRequestPaymentIntentResponse>) end;

  INSearchForAccountsIntentResponseClass = interface(INIntentResponseClass)
    ['{8BA224FB-B156-4125-967D-D8707A386555}']
  end;

  INSearchForAccountsIntentResponse = interface(INIntentResponse)
    ['{FBEED5A5-9D3A-4EC4-92A3-0A4BE5B00A5E}']
    function accounts: NSArray; cdecl;
    function code: INSearchForAccountsIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForAccountsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setAccounts(accounts: NSArray); cdecl;
  end;
  TINSearchForAccountsIntentResponse = class(TOCGenericImport<INSearchForAccountsIntentResponseClass, INSearchForAccountsIntentResponse>) end;

  INSearchForBillsIntentResponseClass = interface(INIntentResponseClass)
    ['{28E7DF80-A96D-4A5D-B26D-3D9F9016BF83}']
  end;

  INSearchForBillsIntentResponse = interface(INIntentResponse)
    ['{3E9C26B2-DF35-45ED-AB12-B2F941BBE732}']
    function bills: NSArray; cdecl;
    function code: INSearchForBillsIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForBillsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setBills(bills: NSArray); cdecl;
  end;
  TINSearchForBillsIntentResponse = class(TOCGenericImport<INSearchForBillsIntentResponseClass, INSearchForBillsIntentResponse>) end;

  INSendPaymentIntentResponseClass = interface(INIntentResponseClass)
    ['{9FBE1BCF-F69D-4184-B24B-94B98C7F2F09}']
  end;

  INSendPaymentIntentResponse = interface(INIntentResponse)
    ['{BE2382C2-7BD2-4158-B42B-AB6C5CDEA4ED}']
    function code: INSendPaymentIntentResponseCode; cdecl;
    function initWithCode(code: INSendPaymentIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function paymentRecord: INPaymentRecord; cdecl;
    procedure setPaymentRecord(paymentRecord: INPaymentRecord); cdecl;
  end;
  TINSendPaymentIntentResponse = class(TOCGenericImport<INSendPaymentIntentResponseClass, INSendPaymentIntentResponse>) end;

  INTransferMoneyIntentResponseClass = interface(INIntentResponseClass)
    ['{74D01FC1-BE1E-47B9-A107-AC86956A88CA}']
  end;

  INTransferMoneyIntentResponse = interface(INIntentResponse)
    ['{26474D5A-D52D-4C93-9DCC-BAD4A76BED7C}']
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
    ['{14EAA915-3200-4BDD-8B40-27DE3DC10F38}']
  end;

  INSearchForPhotosIntentResponse = interface(INIntentResponse)
    ['{7E270276-1CF2-4CB6-A1E0-2783B1888D02}']
    function code: INSearchForPhotosIntentResponseCode; cdecl;
    function initWithCode(code: INSearchForPhotosIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function searchResultsCount: NSNumber; cdecl;
    procedure setSearchResultsCount(searchResultsCount: NSNumber); cdecl;
  end;
  TINSearchForPhotosIntentResponse = class(TOCGenericImport<INSearchForPhotosIntentResponseClass, INSearchForPhotosIntentResponse>) end;

  INStartPhotoPlaybackIntentResponseClass = interface(INIntentResponseClass)
    ['{97E2E783-B6EE-4A2C-8424-320D72FCDFB7}']
  end;

  INStartPhotoPlaybackIntentResponse = interface(INIntentResponse)
    ['{AE22BAA5-086B-4731-BAB3-06724F16C060}']
    function code: INStartPhotoPlaybackIntentResponseCode; cdecl;
    function initWithCode(code: INStartPhotoPlaybackIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function searchResultsCount: NSNumber; cdecl;
    procedure setSearchResultsCount(searchResultsCount: NSNumber); cdecl;
  end;
  TINStartPhotoPlaybackIntentResponse = class(TOCGenericImport<INStartPhotoPlaybackIntentResponseClass, INStartPhotoPlaybackIntentResponse>) end;

  INGetReservationDetailsIntentResponseClass = interface(INIntentResponseClass)
    ['{A14A038E-2624-4C98-9E07-0AFA4E7D4034}']
  end;

  INGetReservationDetailsIntentResponse = interface(INIntentResponse)
    ['{3586A0D4-DC9D-4930-9D9F-1332CEB3B5BC}']
    function code: INGetReservationDetailsIntentResponseCode; cdecl;
    function initWithCode(code: INGetReservationDetailsIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function reservations: NSArray; cdecl;
    procedure setReservations(reservations: NSArray); cdecl;
  end;
  TINGetReservationDetailsIntentResponse = class(TOCGenericImport<INGetReservationDetailsIntentResponseClass,
    INGetReservationDetailsIntentResponse>) end;

  INGetRideStatusIntentResponseClass = interface(INIntentResponseClass)
    ['{A6BDC0AE-D231-41D3-B612-76A4EF7C77D7}']
  end;

  INGetRideStatusIntentResponse = interface(INIntentResponse)
    ['{4494FBE4-5A94-4D4D-9427-84C3A08D80F9}']
    function code: INGetRideStatusIntentResponseCode; cdecl;
    function initWithCode(code: INGetRideStatusIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function rideStatus: INRideStatus; cdecl;
    procedure setRideStatus(rideStatus: INRideStatus); cdecl;
  end;
  TINGetRideStatusIntentResponse = class(TOCGenericImport<INGetRideStatusIntentResponseClass, INGetRideStatusIntentResponse>) end;

  INListRideOptionsIntentResponseClass = interface(INIntentResponseClass)
    ['{5FF2984B-274F-4353-8D93-5B6FCEE04940}']
  end;

  INListRideOptionsIntentResponse = interface(INIntentResponse)
    ['{24E15237-DA2C-4428-AEE6-360BD88F3110}']
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
    ['{C0BC2B0D-FEAA-470A-B1D2-ED7A57A8975C}']
  end;

  INRequestRideIntentResponse = interface(INIntentResponse)
    ['{F2D90B35-ED66-4289-8304-75B0A74371C4}']
    function code: INRequestRideIntentResponseCode; cdecl;
    function initWithCode(code: INRequestRideIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    function rideStatus: INRideStatus; cdecl;
    procedure setRideStatus(rideStatus: INRideStatus); cdecl;
  end;
  TINRequestRideIntentResponse = class(TOCGenericImport<INRequestRideIntentResponseClass, INRequestRideIntentResponse>) end;

  INCancelRideIntentResponseClass = interface(INIntentResponseClass)
    ['{00465366-F2B1-4EFB-B7CE-66F3D0F0A26D}']
  end;

  INCancelRideIntentResponse = interface(INIntentResponse)
    ['{D874606E-45E3-4294-A60E-B44BF9607D39}']
    function cancellationFee: INCurrencyAmount; cdecl;
    function cancellationFeeThreshold: NSDateComponents; cdecl;
    function code: INCancelRideIntentResponseCode; cdecl;
    function initWithCode(code: INCancelRideIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setCancellationFee(cancellationFee: INCurrencyAmount); cdecl;
    procedure setCancellationFeeThreshold(cancellationFeeThreshold: NSDateComponents); cdecl;
  end;
  TINCancelRideIntentResponse = class(TOCGenericImport<INCancelRideIntentResponseClass, INCancelRideIntentResponse>) end;

  INSendRideFeedbackIntentResponseClass = interface(INIntentResponseClass)
    ['{FF23F012-964D-49B4-8D5B-9BEA0B534BC8}']
  end;

  INSendRideFeedbackIntentResponse = interface(INIntentResponse)
    ['{D254EA76-C424-4BFC-AADD-C3E30EBE946F}']
    function code: INSendRideFeedbackIntentResponseCode; cdecl;
    function initWithCode(code: INSendRideFeedbackIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
  end;
  TINSendRideFeedbackIntentResponse = class(TOCGenericImport<INSendRideFeedbackIntentResponseClass, INSendRideFeedbackIntentResponse>) end;

  INGetVisualCodeIntentResponseClass = interface(INIntentResponseClass)
    ['{9E66511B-FB25-4F71-A7D9-D4D97A4BDD6C}']
  end;

  INGetVisualCodeIntentResponse = interface(INIntentResponse)
    ['{7864B3A9-57AE-4117-931A-3F066511DB78}']
    function code: INGetVisualCodeIntentResponseCode; cdecl;
    function initWithCode(code: INGetVisualCodeIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setVisualCodeImage(visualCodeImage: INImage); cdecl;
    function visualCodeImage: INImage; cdecl;
  end;
  TINGetVisualCodeIntentResponse = class(TOCGenericImport<INGetVisualCodeIntentResponseClass, INGetVisualCodeIntentResponse>) end;

  INAccountTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{192AD6EF-C655-44D7-8014-6E9C170D9EEA}']
    {class} function confirmationRequiredWithAccountTypeToConfirm(accountTypeToConfirm: INAccountType): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INAccountType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithAccountTypeToConfirm:", ios(11.0, 11.0), watchos(4.0, 4.0))
    {class} function successWithResolvedAccountType(resolvedAccountType: INAccountType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INAccountType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedAccountType:", ios(11.0, 11.0), watchos(4.0, 4.0))
  end;

  INAccountTypeResolutionResult = interface(INIntentResolutionResult)
    ['{BB5850DC-12BB-4BA5-AFBA-E7EF80F3E303}']
  end;
  TINAccountTypeResolutionResult = class(TOCGenericImport<INAccountTypeResolutionResultClass, INAccountTypeResolutionResult>) end;

  INMediaDestinationResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{7EBC8552-11A1-40AE-9AE8-6ECFDBABBFBF}']
    {class} function confirmationRequiredWithMediaDestinationToConfirm(mediaDestinationToConfirm: INMediaDestination): Pointer; cdecl;
    {class} function disambiguationWithMediaDestinationsToDisambiguate(mediaDestinationsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedMediaDestination(resolvedMediaDestination: INMediaDestination): Pointer; cdecl;
  end;

  INMediaDestinationResolutionResult = interface(INIntentResolutionResult)
    ['{70C5E9E0-69F1-43CA-BE09-7C7DE70496A7}']
  end;
  TINMediaDestinationResolutionResult = class(TOCGenericImport<INMediaDestinationResolutionResultClass, INMediaDestinationResolutionResult>) end;

  INAddMediaMediaDestinationResolutionResultClass = interface(INMediaDestinationResolutionResultClass)
    ['{831584AA-DD54-48F8-B09F-CD3584821DDD}']
    {class} function unsupportedForReason(reason: INAddMediaMediaDestinationUnsupportedReason): Pointer; cdecl;
  end;

  INAddMediaMediaDestinationResolutionResult = interface(INMediaDestinationResolutionResult)
    ['{FABB5DFD-5178-437E-AC7C-4EE01F5AFCDB}']
    function initWithMediaDestinationResolutionResult(mediaDestinationResolutionResult: INMediaDestinationResolutionResult): Pointer; cdecl;
  end;
  TINAddMediaMediaDestinationResolutionResult = class(TOCGenericImport<INAddMediaMediaDestinationResolutionResultClass,
    INAddMediaMediaDestinationResolutionResult>) end;

  INMediaItemResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{2CEB7E1A-381D-4550-AD33-30390A782EF6}']
    {class} function confirmationRequiredWithMediaItemToConfirm(mediaItemToConfirm: INMediaItem): Pointer; cdecl;
    {class} function disambiguationWithMediaItemsToDisambiguate(mediaItemsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function successWithResolvedMediaItem(resolvedMediaItem: INMediaItem): Pointer; cdecl;
  end;

  INMediaItemResolutionResult = interface(INIntentResolutionResult)
    ['{2CF54AFD-1AF2-4750-BF5D-B3D075747496}']
  end;
  TINMediaItemResolutionResult = class(TOCGenericImport<INMediaItemResolutionResultClass, INMediaItemResolutionResult>) end;

  INAddMediaMediaItemResolutionResultClass = interface(INMediaItemResolutionResultClass)
    ['{22C67AAD-BA29-4155-AA6A-E9D24DA657CA}']
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function unsupportedForReason(reason: INAddMediaMediaItemUnsupportedReason): Pointer; cdecl;
  end;

  INAddMediaMediaItemResolutionResult = interface(INMediaItemResolutionResult)
    ['{AFFC7206-1C5F-4AAB-8BFC-13CA707CEEF4}']
    function initWithMediaItemResolutionResult(mediaItemResolutionResult: INMediaItemResolutionResult): Pointer; cdecl;
  end;
  TINAddMediaMediaItemResolutionResult = class(TOCGenericImport<INAddMediaMediaItemResolutionResultClass, INAddMediaMediaItemResolutionResult>) end;

  INTaskListResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{FB7B2BC0-148A-4ECA-9583-BC4E3F79436C}']
    {class} function confirmationRequiredWithTaskListToConfirm(taskListToConfirm: INTaskList): Pointer; cdecl;
    {class} function disambiguationWithTaskListsToDisambiguate(taskListsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedTaskList(resolvedTaskList: INTaskList): Pointer; cdecl;
  end;

  INTaskListResolutionResult = interface(INIntentResolutionResult)
    ['{BCFB96D4-9D4E-48F2-9BC4-3BD4EC9FF3BE}']
  end;
  TINTaskListResolutionResult = class(TOCGenericImport<INTaskListResolutionResultClass, INTaskListResolutionResult>) end;

  INAddTasksTargetTaskListResolutionResultClass = interface(INTaskListResolutionResultClass)
    ['{D7AD9254-4935-4486-B333-7407B24B8C8C}']
    {class} function confirmationRequiredWithTaskListToConfirm(taskListToConfirm: INTaskList;
      forReason: INAddTasksTargetTaskListConfirmationReason): Pointer; cdecl;
  end;

  INAddTasksTargetTaskListResolutionResult = interface(INTaskListResolutionResult)
    ['{C3EB2AB5-EDE4-4B72-868D-A1B47ADAED2D}']
    function initWithTaskListResolutionResult(taskListResolutionResult: INTaskListResolutionResult): Pointer; cdecl;
  end;
  TINAddTasksTargetTaskListResolutionResult = class(TOCGenericImport<INAddTasksTargetTaskListResolutionResultClass,
    INAddTasksTargetTaskListResolutionResult>) end;

  INTemporalEventTriggerResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{FC32CB32-932F-4049-B51B-A846B4B5EC47}']
    {class} function confirmationRequiredWithTemporalEventTriggerToConfirm(temporalEventTriggerToConfirm: INTemporalEventTrigger): Pointer; cdecl;
    {class} function disambiguationWithTemporalEventTriggersToDisambiguate(temporalEventTriggersToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedTemporalEventTrigger(resolvedTemporalEventTrigger: INTemporalEventTrigger): Pointer; cdecl;
  end;

  INTemporalEventTriggerResolutionResult = interface(INIntentResolutionResult)
    ['{0D1581C4-6323-4990-8422-1D0F951CB2AD}']
  end;
  TINTemporalEventTriggerResolutionResult = class(TOCGenericImport<INTemporalEventTriggerResolutionResultClass,
    INTemporalEventTriggerResolutionResult>) end;

  INAddTasksTemporalEventTriggerResolutionResultClass = interface(INTemporalEventTriggerResolutionResultClass)
    ['{E43B7743-296E-4B1C-90CB-E26B383B882F}']
    {class} function unsupportedForReason(reason: INAddTasksTemporalEventTriggerUnsupportedReason): Pointer; cdecl;
  end;

  INAddTasksTemporalEventTriggerResolutionResult = interface(INTemporalEventTriggerResolutionResult)
    ['{083A239D-7FA2-425F-BC12-6024BF130C16}']
    function initWithTemporalEventTriggerResolutionResult(temporalEventTriggerResolutionResult: INTemporalEventTriggerResolutionResult): Pointer; cdecl;
  end;
  TINAddTasksTemporalEventTriggerResolutionResult = class(TOCGenericImport<INAddTasksTemporalEventTriggerResolutionResultClass,
    INAddTasksTemporalEventTriggerResolutionResult>) end;

  INAirlineClass = interface(NSObjectClass)
    ['{52DCF574-894F-4BEB-B437-CA8D1FE32AB1}']
  end;

  INAirline = interface(NSObject)
    ['{AFAA1357-FE3D-45D1-9231-CC555AFB7688}']
    function iataCode: NSString; cdecl;
    function icaoCode: NSString; cdecl;
    function initWithName(name: NSString; iataCode: NSString; icaoCode: NSString): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TINAirline = class(TOCGenericImport<INAirlineClass, INAirline>) end;

  INAirportClass = interface(NSObjectClass)
    ['{C828C12D-D177-4D2F-ACDE-FB3921E34476}']
  end;

  INAirport = interface(NSObject)
    ['{46B63DFD-91FC-4897-8238-FC171F19E54C}']
    function iataCode: NSString; cdecl;
    function icaoCode: NSString; cdecl;
    function initWithName(name: NSString; iataCode: NSString; icaoCode: NSString): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TINAirport = class(TOCGenericImport<INAirportClass, INAirport>) end;

  INAirportGateClass = interface(NSObjectClass)
    ['{8AE4D1DE-FFD3-498C-9BCA-44C6CE35F59E}']
  end;

  INAirportGate = interface(NSObject)
    ['{05F06EA1-5A77-4968-87E2-730D5D5C8DBD}']
    function airport: INAirport; cdecl;
    function gate: NSString; cdecl;
    function initWithAirport(airport: INAirport; terminal: NSString; gate: NSString): Pointer; cdecl;
    function terminal: NSString; cdecl;
  end;
  TINAirportGate = class(TOCGenericImport<INAirportGateClass, INAirportGate>) end;

  INBalanceTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{89779901-29F0-4663-BA1A-39958F45CB44}']
    {class} function confirmationRequiredWithBalanceTypeToConfirm(balanceTypeToConfirm: INBalanceType): Pointer; cdecl;
    {class} function successWithResolvedBalanceType(resolvedBalanceType: INBalanceType): Pointer; cdecl;
  end;

  INBalanceTypeResolutionResult = interface(INIntentResolutionResult)
    ['{3F8C5E67-10B0-475D-9B4E-782A6DCE0E75}']
  end;
  TINBalanceTypeResolutionResult = class(TOCGenericImport<INBalanceTypeResolutionResultClass, INBalanceTypeResolutionResult>) end;

  INBillDetailsClass = interface(NSObjectClass)
    ['{153348D8-9D6C-4BD3-A304-457933A57BA3}']
  end;

  INBillDetails = interface(NSObject)
    ['{408FCF82-0AFC-4649-8C48-FF99DE0E474C}']
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
    ['{DE9FBC69-BC5B-4B3F-9F48-86A0535A2CEA}']
  end;

  INBillPayee = interface(NSObject)
    ['{4EFE5EB9-DE0F-4167-AE7B-80F1D7AC6E3E}']
    function accountNumber: NSString; cdecl;
    function initWithNickname(nickname: INSpeakableString; number: NSString; organizationName: INSpeakableString): Pointer; cdecl;
    function nickname: INSpeakableString; cdecl;
    function organizationName: INSpeakableString; cdecl;
  end;
  TINBillPayee = class(TOCGenericImport<INBillPayeeClass, INBillPayee>) end;

  INBillPayeeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{FBCD2B19-B067-4721-9BAC-977989D7537E}']
    {class} function confirmationRequiredWithBillPayeeToConfirm(billPayeeToConfirm: INBillPayee): Pointer; cdecl;
    {class} function disambiguationWithBillPayeesToDisambiguate(billPayeesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedBillPayee(resolvedBillPayee: INBillPayee): Pointer; cdecl;
  end;

  INBillPayeeResolutionResult = interface(INIntentResolutionResult)
    ['{B0A5E371-33A6-4FD6-B408-F6864E088869}']
  end;
  TINBillPayeeResolutionResult = class(TOCGenericImport<INBillPayeeResolutionResultClass, INBillPayeeResolutionResult>) end;

  INBillTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{A3DF7C3B-449A-4E09-8345-E9B9B5F5A199}']
    {class} function confirmationRequiredWithBillTypeToConfirm(billTypeToConfirm: INBillType): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INBillType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithBillTypeToConfirm:", ios(10.3, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedBillType(resolvedBillType: INBillType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INBillType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedBillType:", ios(10.3, 11.0), watchos(3.2, 4.0))
  end;

  INBillTypeResolutionResult = interface(INIntentResolutionResult)
    ['{7107A016-1533-4B0E-B31B-26D621DEB52D}']
  end;
  TINBillTypeResolutionResult = class(TOCGenericImport<INBillTypeResolutionResultClass, INBillTypeResolutionResult>) end;

  INBoatTripClass = interface(NSObjectClass)
    ['{5C42EE67-ABCA-4638-B987-25338E677FCD}']
  end;

  INBoatTrip = interface(NSObject)
    ['{AB88A6B6-EFAA-4D6D-A4D2-A55983C4E890}']
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
    ['{DF928B2D-254C-4BF0-80D6-15F94E5076DE}']
  end;

  INBusTrip = interface(NSObject)
    ['{669D2DAF-5880-4E7B-8144-C11E968A9C92}']
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
    ['{4E5C03E3-3B38-4124-A087-158B92E79A27}']
    {class} function confirmationRequiredWithCallCapabilityToConfirm(callCapabilityToConfirm: INCallCapability): Pointer; cdecl;
    {class} function successWithResolvedCallCapability(resolvedCallCapability: INCallCapability): Pointer; cdecl;
  end;

  INCallCapabilityResolutionResult = interface(INIntentResolutionResult)
    ['{D7E83EAB-E7AE-428B-BE7E-19819A80E8FB}']
  end;
  TINCallCapabilityResolutionResult = class(TOCGenericImport<INCallCapabilityResolutionResultClass, INCallCapabilityResolutionResult>) end;

  INCallDestinationTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{7DCCA341-B76F-4D3C-A638-F5C416FBFB37}']
    {class} function confirmationRequiredWithCallDestinationTypeToConfirm(callDestinationTypeToConfirm: INCallDestinationType): Pointer; cdecl;
    {class} function successWithResolvedCallDestinationType(resolvedCallDestinationType: INCallDestinationType): Pointer; cdecl;
  end;

  INCallDestinationTypeResolutionResult = interface(INIntentResolutionResult)
    ['{3DC627AC-6E87-470C-931B-2B3559663CEA}']
  end;
  TINCallDestinationTypeResolutionResult = class(TOCGenericImport<INCallDestinationTypeResolutionResultClass,
    INCallDestinationTypeResolutionResult>) end;

  INCallGroupClass = interface(NSObjectClass)
    ['{8ACC283A-72E5-4C79-9976-497B25502092}']
  end;

  INCallGroup = interface(NSObject)
    ['{3F7525F3-168B-4B55-A1A0-954D85FD510F}']
    function groupId: NSString; cdecl;
    function groupName: NSString; cdecl;
    function initWithGroupName(groupName: NSString; groupId: NSString): Pointer; cdecl;
  end;
  TINCallGroup = class(TOCGenericImport<INCallGroupClass, INCallGroup>) end;

  INCallRecordClass = interface(NSObjectClass)
    ['{4CAFB77F-077C-4E7E-AD01-DE39E213CC3A}']
  end;

  INCallRecord = interface(NSObject)
    ['{B402306E-FE0F-4631-822A-3D6BF6DE93CE}']
    function callCapability: INCallCapability; cdecl;
    function callDuration: NSNumber; cdecl;
    function caller: INPerson; cdecl; // API_DEPRECATED("", ios(11.0, 14.5), macos(12.0, 12.0), watchos(4.0, 7.3))
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
    ['{BD55F06E-659F-4A68-BCB9-4752CC5E3DEF}']
  end;

  INCallRecordFilter = interface(NSObject)
    ['{22E05FEA-B427-4002-AE08-EB346AF0E262}']
    function callCapability: INCallCapability; cdecl;
    function callTypes: INCallRecordTypeOptions; cdecl;
    function initWithParticipants(participants: NSArray; callTypes: INCallRecordTypeOptions; callCapability: INCallCapability): Pointer; cdecl;
    function participants: NSArray; cdecl;
  end;
  TINCallRecordFilter = class(TOCGenericImport<INCallRecordFilterClass, INCallRecordFilter>) end;

  INCallRecordResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{996ADC62-F111-4BA8-8B63-4AA9556A3995}']
    {class} function confirmationRequiredWithCallRecordToConfirm(callRecordToConfirm: INCallRecord): Pointer; cdecl;
    {class} function disambiguationWithCallRecordsToDisambiguate(callRecordsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedCallRecord(resolvedCallRecord: INCallRecord): Pointer; cdecl;
  end;

  INCallRecordResolutionResult = interface(INIntentResolutionResult)
    ['{DB5EA8BA-B67F-4282-B715-E00BCC0533C1}']
  end;
  TINCallRecordResolutionResult = class(TOCGenericImport<INCallRecordResolutionResultClass, INCallRecordResolutionResult>) end;

  INCallRecordTypeOptionsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{E44EF3CE-D4ED-44D9-9125-3CB97E7E3829}']
    {class} function confirmationRequiredWithCallRecordTypeOptionsToConfirm(callRecordTypeOptionsToConfirm: INCallRecordTypeOptions): Pointer; cdecl;
    {class} function successWithResolvedCallRecordTypeOptions(resolvedCallRecordTypeOptions: INCallRecordTypeOptions): Pointer; cdecl;
  end;

  INCallRecordTypeOptionsResolutionResult = interface(INIntentResolutionResult)
    ['{0ED891D2-C7DF-480F-8C70-20D801CAA8A5}']
  end;
  TINCallRecordTypeOptionsResolutionResult = class(TOCGenericImport<INCallRecordTypeOptionsResolutionResultClass,
    INCallRecordTypeOptionsResolutionResult>) end;

  INCallRecordTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{C82E7EA8-7146-401E-8A76-A19B2FDF242C}']
    {class} function confirmationRequiredWithCallRecordTypeToConfirm(callRecordTypeToConfirm: INCallRecordType): Pointer; cdecl;
    {class} function successWithResolvedCallRecordType(resolvedCallRecordType: INCallRecordType): Pointer; cdecl;
  end;

  INCallRecordTypeResolutionResult = interface(INIntentResolutionResult)
    ['{EC496434-2C7E-4678-9AE2-7236E7A67D36}']
  end;
  TINCallRecordTypeResolutionResult = class(TOCGenericImport<INCallRecordTypeResolutionResultClass, INCallRecordTypeResolutionResult>) end;

  INCarClass = interface(NSObjectClass)
    ['{31ACEB29-4B23-494E-9359-37EF70B40251}']
  end;

  INCar = interface(NSObject)
    ['{B6593657-FD7C-4187-AD3F-B7CBB2F7BAF4}']
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
    ['{7C8245F2-EFC2-4256-9AC2-95498B3FE626}']
    {class} function confirmationRequiredWithCarAirCirculationModeToConfirm(carAirCirculationModeToConfirm: INCarAirCirculationMode): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarAirCirculationMode): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarAirCirculationModeToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedCarAirCirculationMode(resolvedCarAirCirculationMode: INCarAirCirculationMode): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarAirCirculationMode): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarAirCirculationMode:", ios(10.0, 11.0))
  end;

  INCarAirCirculationModeResolutionResult = interface(INIntentResolutionResult)
    ['{BC232C46-C5FD-4CC6-9536-806E30789BF7}']
  end;
  TINCarAirCirculationModeResolutionResult = class(TOCGenericImport<INCarAirCirculationModeResolutionResultClass,
    INCarAirCirculationModeResolutionResult>) end;

  INCarAudioSourceResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{94AF9797-EAAA-4A63-8F5D-40D19B4E9C87}']
    {class} function confirmationRequiredWithCarAudioSourceToConfirm(carAudioSourceToConfirm: INCarAudioSource): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarAudioSource): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarAudioSourceToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedCarAudioSource(resolvedCarAudioSource: INCarAudioSource): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarAudioSource): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarAudioSource:", ios(10.0, 11.0))
  end;

  INCarAudioSourceResolutionResult = interface(INIntentResolutionResult)
    ['{FE7C0CB2-5878-4DB9-B496-4FD75A942030}']
  end;
  TINCarAudioSourceResolutionResult = class(TOCGenericImport<INCarAudioSourceResolutionResultClass, INCarAudioSourceResolutionResult>) end;

  INCarDefrosterResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{5D265FD7-0655-4DBF-A6F9-D921EEB57635}']
    {class} function confirmationRequiredWithCarDefrosterToConfirm(carDefrosterToConfirm: INCarDefroster): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarDefroster): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarDefrosterToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedCarDefroster(resolvedCarDefroster: INCarDefroster): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarDefroster): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarDefroster:", ios(10.0, 11.0))
  end;

  INCarDefrosterResolutionResult = interface(INIntentResolutionResult)
    ['{FAA8D470-781F-459A-B1A7-37A6A1956A59}']
  end;
  TINCarDefrosterResolutionResult = class(TOCGenericImport<INCarDefrosterResolutionResultClass, INCarDefrosterResolutionResult>) end;

  INCarHeadUnitClass = interface(NSObjectClass)
    ['{B2AA7A3F-EB17-4536-894A-7749A9E472A8}']
  end;

  INCarHeadUnit = interface(NSObject)
    ['{4D973AD3-1C97-4EB3-B31D-AB0B7A9BCC0E}']
    function bluetoothIdentifier: NSString; cdecl;
    function iAP2Identifier: NSString; cdecl;
    function initWithBluetoothIdentifier(bluetoothIdentifier: NSString; iAP2Identifier: NSString): Pointer; cdecl;
  end;
  TINCarHeadUnit = class(TOCGenericImport<INCarHeadUnitClass, INCarHeadUnit>) end;

  INCarSeatResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{9072E0AE-DFE2-48CD-98C7-4496EA466A67}']
    {class} function confirmationRequiredWithCarSeatToConfirm(carSeatToConfirm: INCarSeat): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarSeat): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarSeatToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedCarSeat(resolvedCarSeat: INCarSeat): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarSeat): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarSeat:", ios(10.0, 11.0))
  end;

  INCarSeatResolutionResult = interface(INIntentResolutionResult)
    ['{1E11F7B9-8360-451D-B49D-3327CC0FE845}']
  end;
  TINCarSeatResolutionResult = class(TOCGenericImport<INCarSeatResolutionResultClass, INCarSeatResolutionResult>) end;

  INCarSignalOptionsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{82204BA8-E2C0-41C0-850E-0F6FCA60BCDE}']
    {class} function confirmationRequiredWithCarSignalOptionsToConfirm(carSignalOptionsToConfirm: INCarSignalOptions): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INCarSignalOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithCarSignalOptionsToConfirm:", ios(10.3, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedCarSignalOptions(resolvedCarSignalOptions: INCarSignalOptions): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INCarSignalOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedCarSignalOptions:", ios(10.3, 11.0), watchos(3.2, 4.0))
  end;

  INCarSignalOptionsResolutionResult = interface(INIntentResolutionResult)
    ['{85755EB5-B4C6-40AB-A94D-0A681ECD41CA}']
  end;
  TINCarSignalOptionsResolutionResult = class(TOCGenericImport<INCarSignalOptionsResolutionResultClass, INCarSignalOptionsResolutionResult>) end;

  INCurrencyAmountResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{9474D2C0-0B7C-4813-857A-4915454E792A}']
    {class} function confirmationRequiredWithCurrencyAmountToConfirm(currencyAmountToConfirm: INCurrencyAmount): Pointer; cdecl;
    {class} function disambiguationWithCurrencyAmountsToDisambiguate(currencyAmountsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedCurrencyAmount(resolvedCurrencyAmount: INCurrencyAmount): Pointer; cdecl;
  end;

  INCurrencyAmountResolutionResult = interface(INIntentResolutionResult)
    ['{AA280662-4653-42B8-8596-E39ACDC7E285}']
  end;
  TINCurrencyAmountResolutionResult = class(TOCGenericImport<INCurrencyAmountResolutionResultClass, INCurrencyAmountResolutionResult>) end;

  INDateSearchTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{6084100E-5217-4D61-91B4-E181075350C4}']
    {class} function confirmationRequiredWithDateSearchTypeToConfirm(dateSearchTypeToConfirm: INDateSearchType): Pointer; cdecl;
    {class} function successWithResolvedDateSearchType(resolvedDateSearchType: INDateSearchType): Pointer; cdecl;
  end;

  INDateSearchTypeResolutionResult = interface(INIntentResolutionResult)
    ['{707D8A50-0167-4F85-A054-43A82A101C5F}']
  end;
  TINDateSearchTypeResolutionResult = class(TOCGenericImport<INDateSearchTypeResolutionResultClass, INDateSearchTypeResolutionResult>) end;

  INDeleteTasksTaskListResolutionResultClass = interface(INTaskListResolutionResultClass)
    ['{FA38154F-BA0B-47C5-B7F8-28DB941FACEF}']
    {class} function unsupportedForReason(reason: INDeleteTasksTaskListUnsupportedReason): Pointer; cdecl;
  end;

  INDeleteTasksTaskListResolutionResult = interface(INTaskListResolutionResult)
    ['{FDF739C1-2868-4E9B-889A-2F6402595963}']
    function initWithTaskListResolutionResult(taskListResolutionResult: INTaskListResolutionResult): Pointer; cdecl;
  end;
  TINDeleteTasksTaskListResolutionResult = class(TOCGenericImport<INDeleteTasksTaskListResolutionResultClass,
    INDeleteTasksTaskListResolutionResult>) end;

  INTaskResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{76689D16-B2A3-44B9-A70B-B1AE52FA936A}']
    {class} function confirmationRequiredWithTaskToConfirm(taskToConfirm: INTask): Pointer; cdecl;
    {class} function disambiguationWithTasksToDisambiguate(tasksToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedTask(resolvedTask: INTask): Pointer; cdecl;
  end;

  INTaskResolutionResult = interface(INIntentResolutionResult)
    ['{A914BF0E-9831-4803-B3AB-6CF9D70CBE71}']
  end;
  TINTaskResolutionResult = class(TOCGenericImport<INTaskResolutionResultClass, INTaskResolutionResult>) end;

  INDeleteTasksTaskResolutionResultClass = interface(INTaskResolutionResultClass)
    ['{091A27CB-4E60-45ED-B6AF-34C7063DC25C}']
    {class} function unsupportedForReason(reason: INDeleteTasksTaskUnsupportedReason): Pointer; cdecl;
  end;

  INDeleteTasksTaskResolutionResult = interface(INTaskResolutionResult)
    ['{A094D39D-76F1-4945-9ADE-C25D7AB7C1F6}']
    function initWithTaskResolutionResult(taskResolutionResult: INTaskResolutionResult): Pointer; cdecl;
  end;
  TINDeleteTasksTaskResolutionResult = class(TOCGenericImport<INDeleteTasksTaskResolutionResultClass, INDeleteTasksTaskResolutionResult>) end;

  INFileResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{B61B6E2F-C391-41E6-BA25-29D888293DAF}']
    {class} function confirmationRequiredWithFileToConfirm(fileToConfirm: INFile): Pointer; cdecl;
    {class} function disambiguationWithFilesToDisambiguate(filesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedFile(resolvedFile: INFile): Pointer; cdecl;
  end;

  INFileResolutionResult = interface(INIntentResolutionResult)
    ['{FD7C09A7-C3C2-4072-B76E-9FD7A1CC8E44}']
  end;
  TINFileResolutionResult = class(TOCGenericImport<INFileResolutionResultClass, INFileResolutionResult>) end;

  INFlightClass = interface(NSObjectClass)
    ['{2858AFDA-9954-40F9-A5FC-5285B532A8D0}']
  end;

  INFlight = interface(NSObject)
    ['{949BEB44-DBBA-4F67-B8C4-DA517E099004}']
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

  INFocusStatusClass = interface(NSObjectClass)
    ['{8C80423B-D03B-42E3-8A65-98432DB2EAE5}']
  end;

  INFocusStatus = interface(NSObject)
    ['{E8595E89-6739-4F49-8E74-63C9885BD2D6}']
    function initWithIsFocused(isFocused: NSNumber): Pointer; cdecl;
    function isFocused: NSNumber; cdecl;
  end;
  TINFocusStatus = class(TOCGenericImport<INFocusStatusClass, INFocusStatus>) end;

  INLocationSearchTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{FBCA47DA-377E-45EC-80F2-5FF95E4C4F08}']
    {class} function confirmationRequiredWithLocationSearchTypeToConfirm(locationSearchTypeToConfirm: INLocationSearchType): Pointer; cdecl;
    {class} function successWithResolvedLocationSearchType(resolvedLocationSearchType: INLocationSearchType): Pointer; cdecl;
  end;

  INLocationSearchTypeResolutionResult = interface(INIntentResolutionResult)
    ['{1E43D89E-94B6-419A-BA28-013D7866F018}']
  end;
  TINLocationSearchTypeResolutionResult = class(TOCGenericImport<INLocationSearchTypeResolutionResultClass, INLocationSearchTypeResolutionResult>) end;

  INMediaAffinityTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{6463E0E4-D56B-4F63-9FC4-D57DCD1AD58A}']
    {class} function confirmationRequiredWithMediaAffinityTypeToConfirm(mediaAffinityTypeToConfirm: INMediaAffinityType): Pointer; cdecl;
    {class} function successWithResolvedMediaAffinityType(resolvedMediaAffinityType: INMediaAffinityType): Pointer; cdecl;
  end;

  INMediaAffinityTypeResolutionResult = interface(INIntentResolutionResult)
    ['{CF9FF705-5A41-4A82-A637-626DF2C4BBC5}']
  end;
  TINMediaAffinityTypeResolutionResult = class(TOCGenericImport<INMediaAffinityTypeResolutionResultClass, INMediaAffinityTypeResolutionResult>) end;

  INMediaDestinationClass = interface(NSObjectClass)
    ['{912F1660-19C8-4D64-9322-E986CA22C1E8}']
    {class} function libraryDestination: Pointer; cdecl;
    {class} function playlistDestinationWithName(playlistName: NSString): Pointer; cdecl;
  end;

  INMediaDestination = interface(NSObject)
    ['{36903945-3889-45CE-BBA2-BBB7715D9FC8}']
    function mediaDestinationType: INMediaDestinationType; cdecl;
    function playlistName: NSString; cdecl;
  end;
  TINMediaDestination = class(TOCGenericImport<INMediaDestinationClass, INMediaDestination>) end;

  INMediaSearchClass = interface(NSObjectClass)
    ['{68A41B7E-0142-463C-8B94-A2B329AD9272}']
  end;

  INMediaSearch = interface(NSObject)
    ['{90A53BB3-1D15-4B6C-AB8F-AD81838338A4}']
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
    ['{BCA16752-E4A6-4734-B901-71FFA6D930E3}']
    {class} function confirmationRequiredWithMessageAttributeOptionsToConfirm(messageAttributeOptionsToConfirm: INMessageAttributeOptions): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INMessageAttributeOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithMessageAttributeOptionsToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedMessageAttributeOptions(resolvedMessageAttributeOptions: INMessageAttributeOptions): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INMessageAttributeOptions): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedMessageAttributeOptions:", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INMessageAttributeOptionsResolutionResult = interface(INIntentResolutionResult)
    ['{972AE862-7B31-44E7-96BE-E29D6608B572}']
  end;
  TINMessageAttributeOptionsResolutionResult = class(TOCGenericImport<INMessageAttributeOptionsResolutionResultClass,
    INMessageAttributeOptionsResolutionResult>) end;

  INMessageAttributeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{CC0C2EE5-67C8-4E7F-A143-C9819172017F}']
    {class} function confirmationRequiredWithMessageAttributeToConfirm(messageAttributeToConfirm: INMessageAttribute): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INMessageAttribute): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithMessageAttributeToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedMessageAttribute(resolvedMessageAttribute: INMessageAttribute): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INMessageAttribute): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedMessageAttribute:", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INMessageAttributeResolutionResult = interface(INIntentResolutionResult)
    ['{DB9C7FD1-9199-4E0D-AA11-A40607FE1D2A}']
  end;
  TINMessageAttributeResolutionResult = class(TOCGenericImport<INMessageAttributeResolutionResultClass, INMessageAttributeResolutionResult>) end;

  INNoteClass = interface(NSObjectClass)
    ['{4EF95E8B-5D12-436B-9326-F3E4FAA9E05C}']
  end;

  INNote = interface(NSObject)
    ['{711D16AA-4357-45DC-AC9F-2236D6DC456B}']
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
    ['{83A8A0F5-2EA5-4B98-AD38-71A15002577E}']
    {class} function confirmationRequiredWithNoteContentToConfirm(noteContentToConfirm: INNoteContent): Pointer; cdecl;
    {class} function disambiguationWithNoteContentsToDisambiguate(noteContentsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedNoteContent(resolvedNoteContent: INNoteContent): Pointer; cdecl;
  end;

  INNoteContentResolutionResult = interface(INIntentResolutionResult)
    ['{2E6AFE66-240C-498F-BD5D-B9ED5EC21F84}']
  end;
  TINNoteContentResolutionResult = class(TOCGenericImport<INNoteContentResolutionResultClass, INNoteContentResolutionResult>) end;

  INNoteContentTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{C960A187-A835-4CD9-A4C8-EED529384751}']
    {class} function confirmationRequiredWithNoteContentTypeToConfirm(noteContentTypeToConfirm: INNoteContentType): Pointer; cdecl;
    {class} function successWithResolvedNoteContentType(resolvedNoteContentType: INNoteContentType): Pointer; cdecl;
  end;

  INNoteContentTypeResolutionResult = interface(INIntentResolutionResult)
    ['{C5C63D58-7B08-4BF1-95E2-84D85DB83398}']
  end;
  TINNoteContentTypeResolutionResult = class(TOCGenericImport<INNoteContentTypeResolutionResultClass, INNoteContentTypeResolutionResult>) end;

  INNoteResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{76ECABC9-B230-447F-97B9-D4680FCD0DA0}']
    {class} function confirmationRequiredWithNoteToConfirm(noteToConfirm: INNote): Pointer; cdecl;
    {class} function disambiguationWithNotesToDisambiguate(notesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedNote(resolvedNote: INNote): Pointer; cdecl;
  end;

  INNoteResolutionResult = interface(INIntentResolutionResult)
    ['{3BD8CE62-26C8-4940-A746-EC089D37CC8B}']
  end;
  TINNoteResolutionResult = class(TOCGenericImport<INNoteResolutionResultClass, INNoteResolutionResult>) end;

  INNotebookItemTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{414A753E-533A-4EC6-9E43-3E8B09E31B23}']
    {class} function confirmationRequiredWithNotebookItemTypeToConfirm(notebookItemTypeToConfirm: INNotebookItemType): Pointer; cdecl;
    {class} function disambiguationWithNotebookItemTypesToDisambiguate(notebookItemTypesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedNotebookItemType(resolvedNotebookItemType: INNotebookItemType): Pointer; cdecl;
  end;

  INNotebookItemTypeResolutionResult = interface(INIntentResolutionResult)
    ['{2756CBC5-43ED-4711-9D43-C2D16C5F9307}']
  end;
  TINNotebookItemTypeResolutionResult = class(TOCGenericImport<INNotebookItemTypeResolutionResultClass, INNotebookItemTypeResolutionResult>) end;

  INOutgoingMessageTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{BD22D59A-3E3F-49BC-9DAE-0CF83CE21053}']
    {class} function confirmationRequiredWithOutgoingMessageTypeToConfirm(outgoingMessageTypeToConfirm: INOutgoingMessageType): Pointer; cdecl;
    {class} function successWithResolvedOutgoingMessageType(resolvedOutgoingMessageType: INOutgoingMessageType): Pointer; cdecl;
  end;

  INOutgoingMessageTypeResolutionResult = interface(INIntentResolutionResult)
    ['{5F408B53-9AC8-47C2-B91D-CD25D0D5D8A5}']
  end;
  TINOutgoingMessageTypeResolutionResult = class(TOCGenericImport<INOutgoingMessageTypeResolutionResultClass,
    INOutgoingMessageTypeResolutionResult>) end;

  INPaymentAccountClass = interface(NSObjectClass)
    ['{7264155D-84FF-4581-A761-529F71AC298D}']
  end;

  INPaymentAccount = interface(NSObject)
    ['{E3A2873C-1A45-411E-ADE0-47B9538FF75B}']
    function accountNumber: NSString; cdecl;
    function accountType: INAccountType; cdecl;
    function balance: INBalanceAmount; cdecl;
    function initWithNickname(nickname: INSpeakableString; number: NSString; accountType: INAccountType;
      organizationName: INSpeakableString): Pointer; overload; cdecl; // API_DEPRECATED("Please use 'initWithNickname:number:accountType:organizationName:balance:secondaryBalance:' instead", ios(10.3, 11.0), watchos(3.2, 4.0))
    function initWithNickname(nickname: INSpeakableString; number: NSString; accountType: INAccountType; organizationName: INSpeakableString;
      balance: INBalanceAmount; secondaryBalance: INBalanceAmount): Pointer; overload; cdecl;
    function nickname: INSpeakableString; cdecl;
    function organizationName: INSpeakableString; cdecl;
    function secondaryBalance: INBalanceAmount; cdecl;
  end;
  TINPaymentAccount = class(TOCGenericImport<INPaymentAccountClass, INPaymentAccount>) end;

  INPaymentAccountResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{FEDC5B2A-9AE2-470F-B1F2-C3AC0121BF08}']
    {class} function confirmationRequiredWithPaymentAccountToConfirm(paymentAccountToConfirm: INPaymentAccount): Pointer; cdecl;
    {class} function disambiguationWithPaymentAccountsToDisambiguate(paymentAccountsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPaymentAccount(resolvedPaymentAccount: INPaymentAccount): Pointer; cdecl;
  end;

  INPaymentAccountResolutionResult = interface(INIntentResolutionResult)
    ['{9842E83C-2082-4912-85C5-A8DD11E4ECB8}']
  end;
  TINPaymentAccountResolutionResult = class(TOCGenericImport<INPaymentAccountResolutionResultClass, INPaymentAccountResolutionResult>) end;

  INPaymentAmountClass = interface(NSObjectClass)
    ['{67BD0FD1-B3A3-4997-BEF6-1D5DFAF9C0EC}']
  end;

  INPaymentAmount = interface(NSObject)
    ['{A6780B73-90EF-424C-BEA2-3332105AA4A0}']
    function amount: INCurrencyAmount; cdecl;
    function amountType: INAmountType; cdecl;
    function initWithAmountType(amountType: INAmountType; amount: INCurrencyAmount): Pointer; cdecl;
  end;
  TINPaymentAmount = class(TOCGenericImport<INPaymentAmountClass, INPaymentAmount>) end;

  INPaymentAmountResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{52C39BD7-5648-4F18-8451-18D76A47CFAF}']
    {class} function confirmationRequiredWithPaymentAmountToConfirm(paymentAmountToConfirm: INPaymentAmount): Pointer; cdecl;
    {class} function disambiguationWithPaymentAmountsToDisambiguate(paymentAmountsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPaymentAmount(resolvedPaymentAmount: INPaymentAmount): Pointer; cdecl;
  end;

  INPaymentAmountResolutionResult = interface(INIntentResolutionResult)
    ['{37B2A93A-2BAD-4CA8-9280-CD3558B73226}']
  end;
  TINPaymentAmountResolutionResult = class(TOCGenericImport<INPaymentAmountResolutionResultClass, INPaymentAmountResolutionResult>) end;

  INPaymentMethodClass = interface(NSObjectClass)
    ['{CDD3481D-EB92-471A-A2A5-2F495F5512D3}']
    {class} function applePayPaymentMethod: Pointer; cdecl;
  end;

  INPaymentMethod = interface(NSObject)
    ['{54AA05E4-6CBF-4A13-8BA8-1EA3C7642A49}']
    function &type: INPaymentMethodType; cdecl;
    function icon: INImage; cdecl;
    function identificationHint: NSString; cdecl;
    function initWithType(&type: INPaymentMethodType; name: NSString; identificationHint: NSString; icon: INImage): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TINPaymentMethod = class(TOCGenericImport<INPaymentMethodClass, INPaymentMethod>) end;

  INPaymentMethodResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{205BA0D2-F002-407F-9413-DBF5901F8A47}']
    {class} function confirmationRequiredWithPaymentMethodToConfirm(paymentMethodToConfirm: INPaymentMethod): Pointer; cdecl;
    {class} function disambiguationWithPaymentMethodsToDisambiguate(paymentMethodsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPaymentMethod(resolvedPaymentMethod: INPaymentMethod): Pointer; cdecl;
  end;

  INPaymentMethodResolutionResult = interface(INIntentResolutionResult)
    ['{3BCF7857-019B-46BC-A0F8-448CF2C57F2B}']
  end;
  TINPaymentMethodResolutionResult = class(TOCGenericImport<INPaymentMethodResolutionResultClass, INPaymentMethodResolutionResult>) end;

  INPaymentRecordClass = interface(NSObjectClass)
    ['{9CD91A31-1373-47D3-A95E-BC32948D864C}']
  end;

  INPaymentRecord = interface(NSObject)
    ['{1928385E-DACE-4F87-B9B2-6BF5EA7B7AE5}']
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
    ['{470FE120-10D0-4AFA-B0CD-BE1E488630B6}']
    {class} function confirmationRequiredWithPaymentStatusToConfirm(paymentStatusToConfirm: INPaymentStatus): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INPaymentStatus): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithPaymentStatusToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedPaymentStatus(resolvedPaymentStatus: INPaymentStatus): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INPaymentStatus): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedPaymentStatus:", ios(10.0, 11.0), watchos(3.2, 4.0))
  end;

  INPaymentStatusResolutionResult = interface(INIntentResolutionResult)
    ['{18E70D47-0081-453D-98B1-498CA54C2FDE}']
  end;
  TINPaymentStatusResolutionResult = class(TOCGenericImport<INPaymentStatusResolutionResultClass, INPaymentStatusResolutionResult>) end;

  INPlayMediaMediaItemResolutionResultClass = interface(INMediaItemResolutionResultClass)
    ['{2CE83543-3D43-4E9D-8C6A-88E927B26BCD}']
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function unsupportedForReason(reason: INPlayMediaMediaItemUnsupportedReason): Pointer; cdecl;
  end;

  INPlayMediaMediaItemResolutionResult = interface(INMediaItemResolutionResult)
    ['{20F25F42-8DA8-408F-8A5A-17328BBC83CF}']
    function initWithMediaItemResolutionResult(mediaItemResolutionResult: INMediaItemResolutionResult): Pointer; cdecl;
  end;
  TINPlayMediaMediaItemResolutionResult = class(TOCGenericImport<INPlayMediaMediaItemResolutionResultClass, INPlayMediaMediaItemResolutionResult>) end;

  INDoubleResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{D4E9C74D-4BBF-4131-9C90-8FB11FE8AE05}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: NSNumber): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: Double): Pointer; cdecl;
  end;

  INDoubleResolutionResult = interface(INIntentResolutionResult)
    ['{FE2683AF-692C-45C8-89AF-6D002FF712CF}']
  end;
  TINDoubleResolutionResult = class(TOCGenericImport<INDoubleResolutionResultClass, INDoubleResolutionResult>) end;

  INPlayMediaPlaybackSpeedResolutionResultClass = interface(INDoubleResolutionResultClass)
    ['{000ECFF6-F888-4D07-B5BC-C9E0BBC76FEB}']
    {class} function unsupportedForReason(reason: INPlayMediaPlaybackSpeedUnsupportedReason): Pointer; cdecl;
  end;

  INPlayMediaPlaybackSpeedResolutionResult = interface(INDoubleResolutionResult)
    ['{2F83B88E-3E0F-4409-8F71-568566E47BD1}']
    function initWithDoubleResolutionResult(doubleResolutionResult: INDoubleResolutionResult): Pointer; cdecl;
  end;
  TINPlayMediaPlaybackSpeedResolutionResult = class(TOCGenericImport<INPlayMediaPlaybackSpeedResolutionResultClass,
    INPlayMediaPlaybackSpeedResolutionResult>) end;

  INPlaybackQueueLocationResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{61D47899-4D51-48E0-858C-91553924C020}']
    {class} function confirmationRequiredWithPlaybackQueueLocationToConfirm(playbackQueueLocationToConfirm: INPlaybackQueueLocation): Pointer; cdecl;
    {class} function successWithResolvedPlaybackQueueLocation(resolvedPlaybackQueueLocation: INPlaybackQueueLocation): Pointer; cdecl;
  end;

  INPlaybackQueueLocationResolutionResult = interface(INIntentResolutionResult)
    ['{2074CF1C-3E59-48F4-8C40-3BB1DBFD6F11}']
  end;
  TINPlaybackQueueLocationResolutionResult = class(TOCGenericImport<INPlaybackQueueLocationResolutionResultClass,
    INPlaybackQueueLocationResolutionResult>) end;

  INPlaybackRepeatModeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{BC50B82C-AE78-493B-9F03-8D590821074E}']
    {class} function confirmationRequiredWithPlaybackRepeatModeToConfirm(playbackRepeatModeToConfirm: INPlaybackRepeatMode): Pointer; cdecl;
    {class} function successWithResolvedPlaybackRepeatMode(resolvedPlaybackRepeatMode: INPlaybackRepeatMode): Pointer; cdecl;
  end;

  INPlaybackRepeatModeResolutionResult = interface(INIntentResolutionResult)
    ['{E3F83A93-C77A-43D0-A1F7-242022376101}']
  end;
  TINPlaybackRepeatModeResolutionResult = class(TOCGenericImport<INPlaybackRepeatModeResolutionResultClass, INPlaybackRepeatModeResolutionResult>) end;

  INRadioTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{8E56C5FC-1DB8-4D40-9ADB-14604BD61AC1}']
    {class} function confirmationRequiredWithRadioTypeToConfirm(radioTypeToConfirm: INRadioType): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INRadioType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithRadioTypeToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedRadioType(resolvedRadioType: INRadioType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INRadioType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedRadioType:", ios(10.0, 11.0))
  end;

  INRadioTypeResolutionResult = interface(INIntentResolutionResult)
    ['{D3A21ED6-08E4-4B68-910D-F8259A68DD7A}']
  end;
  TINRadioTypeResolutionResult = class(TOCGenericImport<INRadioTypeResolutionResultClass, INRadioTypeResolutionResult>) end;

  INRelativeReferenceResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{2487AE83-AFA0-460B-8989-72B0020D64E3}']
    {class} function confirmationRequiredWithRelativeReferenceToConfirm(relativeReferenceToConfirm: INRelativeReference): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INRelativeReference): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithRelativeReferenceToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedRelativeReference(resolvedRelativeReference: INRelativeReference): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INRelativeReference): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedRelativeReference:", ios(10.0, 11.0))
  end;

  INRelativeReferenceResolutionResult = interface(INIntentResolutionResult)
    ['{55534A53-BA7E-4FBF-8754-7F4F07975511}']
  end;
  TINRelativeReferenceResolutionResult = class(TOCGenericImport<INRelativeReferenceResolutionResultClass, INRelativeReferenceResolutionResult>) end;

  INRelativeSettingResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{DC6DC2F4-A24B-401F-8F35-81B3A24927DB}']
    {class} function confirmationRequiredWithRelativeSettingToConfirm(relativeSettingToConfirm: INRelativeSetting): Pointer; cdecl;
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INRelativeSetting): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithRelativeSettingToConfirm:", ios(10.0, 11.0))
    {class} function successWithResolvedRelativeSetting(resolvedRelativeSetting: INRelativeSetting): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INRelativeSetting): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedRelativeSetting:", ios(10.0, 11.0))
  end;

  INRelativeSettingResolutionResult = interface(INIntentResolutionResult)
    ['{536439F1-97ED-4FD2-8D35-5AAE4CBAD0BD}']
  end;
  TINRelativeSettingResolutionResult = class(TOCGenericImport<INRelativeSettingResolutionResultClass, INRelativeSettingResolutionResult>) end;

  INRentalCarClass = interface(NSObjectClass)
    ['{E45E00E9-D6D8-47CE-8461-006239CFA41F}']
  end;

  INRentalCar = interface(NSObject)
    ['{6B4C8F12-9CD6-49FE-9AFF-ABFEBD42EC20}']
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
    ['{1C981625-2859-46C6-B9F5-BBE33FCFEF79}']
    {class} function unsupportedForReason(reason: INRequestPaymentCurrencyAmountUnsupportedReason): Pointer; cdecl;
  end;

  INRequestPaymentCurrencyAmountResolutionResult = interface(INCurrencyAmountResolutionResult)
    ['{FDDE38B2-A6DF-4341-B449-36E4281CD01C}']
    function initWithCurrencyAmountResolutionResult(currencyAmountResolutionResult: INCurrencyAmountResolutionResult): Pointer; cdecl;
  end;
  TINRequestPaymentCurrencyAmountResolutionResult = class(TOCGenericImport<INRequestPaymentCurrencyAmountResolutionResultClass,
    INRequestPaymentCurrencyAmountResolutionResult>) end;

  INPersonResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{2CE42095-38F6-41E8-8A0E-45FEF712BC6B}']
    {class} function confirmationRequiredWithPersonToConfirm(personToConfirm: INPerson): Pointer; cdecl;
    {class} function disambiguationWithPeopleToDisambiguate(peopleToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPerson(resolvedPerson: INPerson): Pointer; cdecl;
  end;

  INPersonResolutionResult = interface(INIntentResolutionResult)
    ['{BD51E702-3370-423F-ABD5-F5ADBE27FA1E}']
  end;
  TINPersonResolutionResult = class(TOCGenericImport<INPersonResolutionResultClass, INPersonResolutionResult>) end;

  INRequestPaymentPayerResolutionResultClass = interface(INPersonResolutionResultClass)
    ['{F412E665-09D8-451A-A21B-A7E02232CBAB}']
    {class} function unsupportedForReason(reason: INRequestPaymentPayerUnsupportedReason): Pointer; cdecl;
  end;

  INRequestPaymentPayerResolutionResult = interface(INPersonResolutionResult)
    ['{BC2F75DC-089F-48A5-9CF6-8C29DC4F6223}']
    function initWithPersonResolutionResult(personResolutionResult: INPersonResolutionResult): Pointer; cdecl;
  end;
  TINRequestPaymentPayerResolutionResult = class(TOCGenericImport<INRequestPaymentPayerResolutionResultClass,
    INRequestPaymentPayerResolutionResult>) end;

  INSearchForMediaMediaItemResolutionResultClass = interface(INMediaItemResolutionResultClass)
    ['{CAB79B4F-8F68-4A4F-BB0B-A27E9E151632}']
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function unsupportedForReason(reason: INSearchForMediaMediaItemUnsupportedReason): Pointer; cdecl;
  end;

  INSearchForMediaMediaItemResolutionResult = interface(INMediaItemResolutionResult)
    ['{0E2AF437-B5EB-4C92-82AF-3BEA3173C7F5}']
    function initWithMediaItemResolutionResult(mediaItemResolutionResult: INMediaItemResolutionResult): Pointer; cdecl;
  end;
  TINSearchForMediaMediaItemResolutionResult = class(TOCGenericImport<INSearchForMediaMediaItemResolutionResultClass,
    INSearchForMediaMediaItemResolutionResult>) end;

  INSeatClass = interface(NSObjectClass)
    ['{CDB7192F-91C2-4CF1-81B7-D86DE7BF825D}']
  end;

  INSeat = interface(NSObject)
    ['{8BA08CE3-807D-4B03-A906-25C00C462773}']
    function initWithSeatSection(seatSection: NSString; seatRow: NSString; seatNumber: NSString; seatingType: NSString): Pointer; cdecl;
    function seatingType: NSString; cdecl;
    function seatNumber: NSString; cdecl;
    function seatRow: NSString; cdecl;
    function seatSection: NSString; cdecl;
  end;
  TINSeat = class(TOCGenericImport<INSeatClass, INSeat>) end;

  INSendMessageRecipientResolutionResultClass = interface(INPersonResolutionResultClass)
    ['{0BAE52BD-EACE-4212-83B1-8247E7A359A6}']
    {class} function unsupportedForReason(reason: INSendMessageRecipientUnsupportedReason): Pointer; cdecl;
  end;

  INSendMessageRecipientResolutionResult = interface(INPersonResolutionResult)
    ['{7E30D412-44B2-43CC-8A87-63845593EC96}']
    function initWithPersonResolutionResult(personResolutionResult: INPersonResolutionResult): Pointer; cdecl;
  end;
  TINSendMessageRecipientResolutionResult = class(TOCGenericImport<INSendMessageRecipientResolutionResultClass,
    INSendMessageRecipientResolutionResult>) end;

  INSendPaymentCurrencyAmountResolutionResultClass = interface(INCurrencyAmountResolutionResultClass)
    ['{95D4046A-94DA-4F4F-91D0-2A74265BCEEE}']
    {class} function unsupportedForReason(reason: INSendPaymentCurrencyAmountUnsupportedReason): Pointer; cdecl;
  end;

  INSendPaymentCurrencyAmountResolutionResult = interface(INCurrencyAmountResolutionResult)
    ['{ADBAC7DE-5BB3-4134-8373-4A773EB6C0E5}']
    function initWithCurrencyAmountResolutionResult(currencyAmountResolutionResult: INCurrencyAmountResolutionResult): Pointer; cdecl;
  end;
  TINSendPaymentCurrencyAmountResolutionResult = class(TOCGenericImport<INSendPaymentCurrencyAmountResolutionResultClass,
    INSendPaymentCurrencyAmountResolutionResult>) end;

  INSendPaymentPayeeResolutionResultClass = interface(INPersonResolutionResultClass)
    ['{8112EEA0-1965-454D-9FFD-C9B8CADBFF0C}']
    {class} function unsupportedForReason(reason: INSendPaymentPayeeUnsupportedReason): Pointer; cdecl;
  end;

  INSendPaymentPayeeResolutionResult = interface(INPersonResolutionResult)
    ['{9346003E-0C77-450C-9F75-B217B51F62BE}']
    function initWithPersonResolutionResult(personResolutionResult: INPersonResolutionResult): Pointer; cdecl;
  end;
  TINSendPaymentPayeeResolutionResult = class(TOCGenericImport<INSendPaymentPayeeResolutionResultClass, INSendPaymentPayeeResolutionResult>) end;

  INSetTaskAttributeTemporalEventTriggerResolutionResultClass = interface(INTemporalEventTriggerResolutionResultClass)
    ['{3931EABF-385B-4F57-8C72-337121547C2E}']
    {class} function unsupportedForReason(reason: INSetTaskAttributeTemporalEventTriggerUnsupportedReason): Pointer; cdecl;
  end;

  INSetTaskAttributeTemporalEventTriggerResolutionResult = interface(INTemporalEventTriggerResolutionResult)
    ['{01826D26-6E0E-4AEC-A33D-BEC5391C5E2D}']
    function initWithTemporalEventTriggerResolutionResult(temporalEventTriggerResolutionResult: INTemporalEventTriggerResolutionResult): Pointer; cdecl;
  end;
  TINSetTaskAttributeTemporalEventTriggerResolutionResult = class(TOCGenericImport<INSetTaskAttributeTemporalEventTriggerResolutionResultClass,
    INSetTaskAttributeTemporalEventTriggerResolutionResult>) end;

  INSnoozeTasksTaskResolutionResultClass = interface(INTaskResolutionResultClass)
    ['{0B7C414B-BC66-4729-BB6F-FAD533B60188}']
    {class} function unsupportedForReason(reason: INSnoozeTasksTaskUnsupportedReason): Pointer; cdecl;
  end;

  INSnoozeTasksTaskResolutionResult = interface(INTaskResolutionResult)
    ['{4E0405C9-D98C-4B79-A301-646549591265}']
    function initWithTaskResolutionResult(taskResolutionResult: INTaskResolutionResult): Pointer; cdecl;
  end;
  TINSnoozeTasksTaskResolutionResult = class(TOCGenericImport<INSnoozeTasksTaskResolutionResultClass, INSnoozeTasksTaskResolutionResult>) end;

  INSpatialEventTriggerClass = interface(NSObjectClass)
    ['{CE109A9F-1671-4F23-B6DE-DA553657816B}']
  end;

  INSpatialEventTrigger = interface(NSObject)
    ['{20F20B6C-E636-4385-8858-A7B3B06FDDC1}']
    function event: INSpatialEvent; cdecl;
    function initWithPlacemark(placemark: CLPlacemark; event: INSpatialEvent): Pointer; cdecl;
    function placemark: CLPlacemark; cdecl;
  end;
  TINSpatialEventTrigger = class(TOCGenericImport<INSpatialEventTriggerClass, INSpatialEventTrigger>) end;

  INSpatialEventTriggerResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{0CA59825-2F16-4A84-8E52-766D67E97C8D}']
    {class} function confirmationRequiredWithSpatialEventTriggerToConfirm(spatialEventTriggerToConfirm: INSpatialEventTrigger): Pointer; cdecl;
    {class} function disambiguationWithSpatialEventTriggersToDisambiguate(spatialEventTriggersToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedSpatialEventTrigger(resolvedSpatialEventTrigger: INSpatialEventTrigger): Pointer; cdecl;
  end;

  INSpatialEventTriggerResolutionResult = interface(INIntentResolutionResult)
    ['{CCB002DB-60A0-4ECC-8B51-4BC4C74B124E}']
  end;
  TINSpatialEventTriggerResolutionResult = class(TOCGenericImport<INSpatialEventTriggerResolutionResultClass,
    INSpatialEventTriggerResolutionResult>) end;

  INStartCallCallCapabilityResolutionResultClass = interface(INCallCapabilityResolutionResultClass)
    ['{CE8CF740-A3F2-4225-8586-2403C1D161DC}']
    {class} function unsupportedForReason(reason: INStartCallCallCapabilityUnsupportedReason): Pointer; cdecl;
  end;

  INStartCallCallCapabilityResolutionResult = interface(INCallCapabilityResolutionResult)
    ['{EDC5521E-D9B5-46AC-AD28-089C6E509E3D}']
    function initWithCallCapabilityResolutionResult(callCapabilityResolutionResult: INCallCapabilityResolutionResult): Pointer; cdecl;
  end;
  TINStartCallCallCapabilityResolutionResult = class(TOCGenericImport<INStartCallCallCapabilityResolutionResultClass,
    INStartCallCallCapabilityResolutionResult>) end;

  INStartCallCallRecordToCallBackResolutionResultClass = interface(INCallRecordResolutionResultClass)
    ['{AB68E98C-2463-4F89-A6C9-A0FE29F271A1}']
    {class} function unsupportedForReason(reason: INStartCallCallRecordToCallBackUnsupportedReason): Pointer; cdecl;
  end;

  INStartCallCallRecordToCallBackResolutionResult = interface(INCallRecordResolutionResult)
    ['{7C1F9460-FFE2-4BBA-BE44-F359C3557A57}']
    function initWithCallRecordResolutionResult(callRecordResolutionResult: INCallRecordResolutionResult): Pointer; cdecl;
  end;
  TINStartCallCallRecordToCallBackResolutionResult = class(TOCGenericImport<INStartCallCallRecordToCallBackResolutionResultClass,
    INStartCallCallRecordToCallBackResolutionResult>) end;

  INStartCallContactResolutionResultClass = interface(INPersonResolutionResultClass)
    ['{8CB49321-2E65-46A0-A95C-2E4E31322EDB}']
    {class} function unsupportedForReason(reason: INStartCallContactUnsupportedReason): Pointer; cdecl;
  end;

  INStartCallContactResolutionResult = interface(INPersonResolutionResult)
    ['{FFE50A01-4F8A-463A-AE5C-89C3E9255F9D}']
    function initWithPersonResolutionResult(personResolutionResult: INPersonResolutionResult): Pointer; cdecl;
  end;
  TINStartCallContactResolutionResult = class(TOCGenericImport<INStartCallContactResolutionResultClass, INStartCallContactResolutionResult>) end;

  INTaskClass = interface(NSObjectClass)
    ['{E43AF0C8-C4A3-4FF1-A3CA-90F4842120F6}']
  end;

  INTask = interface(NSObject)
    ['{DF48E4F0-757A-4ECB-9399-26AD18951F7C}']
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
    ['{F0C2F4E1-A16C-45F3-87DB-5826708DF059}']
  end;

  INTaskList = interface(NSObject)
    ['{D956F632-7217-434F-BB53-7D44CCDD6408}']
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
    ['{E4C6C538-D569-43A5-A0F6-8373EE1E89C8}']
    {class} function confirmationRequiredWithTaskPriorityToConfirm(taskPriorityToConfirm: INTaskPriority): Pointer; cdecl;
    {class} function successWithResolvedTaskPriority(resolvedTaskPriority: INTaskPriority): Pointer; cdecl;
  end;

  INTaskPriorityResolutionResult = interface(INIntentResolutionResult)
    ['{1714E87D-5E8D-48F5-A510-FC05500673AD}']
  end;
  TINTaskPriorityResolutionResult = class(TOCGenericImport<INTaskPriorityResolutionResultClass, INTaskPriorityResolutionResult>) end;

  INTaskStatusResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{43D5F957-ADF5-4C84-8EE3-29A5F21EEC6C}']
    {class} function confirmationRequiredWithTaskStatusToConfirm(taskStatusToConfirm: INTaskStatus): Pointer; cdecl;
    {class} function successWithResolvedTaskStatus(resolvedTaskStatus: INTaskStatus): Pointer; cdecl;
  end;

  INTaskStatusResolutionResult = interface(INIntentResolutionResult)
    ['{EFFFB36A-DDDA-4FBE-AEBC-1C86E9C30C3B}']
  end;
  TINTaskStatusResolutionResult = class(TOCGenericImport<INTaskStatusResolutionResultClass, INTaskStatusResolutionResult>) end;

  INTemporalEventTriggerClass = interface(NSObjectClass)
    ['{08DE0143-173B-49DB-80FD-429DF71D2018}']
  end;

  INTemporalEventTrigger = interface(NSObject)
    ['{D02C96E0-CE41-435C-BA02-C46298329F8F}']
    function dateComponentsRange: INDateComponentsRange; cdecl;
    function initWithDateComponentsRange(dateComponentsRange: INDateComponentsRange): Pointer; cdecl;
  end;
  TINTemporalEventTrigger = class(TOCGenericImport<INTemporalEventTriggerClass, INTemporalEventTrigger>) end;

  INTemporalEventTriggerTypeOptionsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{89727DDC-0D4A-4048-B049-FBDE74E7E498}']
    {class} function confirmationRequiredWithTemporalEventTriggerTypeOptionsToConfirm(temporalEventTriggerTypeOptionsToConfirm: INTemporalEventTriggerTypeOptions): Pointer; cdecl;
    {class} function successWithResolvedTemporalEventTriggerTypeOptions(resolvedTemporalEventTriggerTypeOptions: INTemporalEventTriggerTypeOptions): Pointer; cdecl;
  end;

  INTemporalEventTriggerTypeOptionsResolutionResult = interface(INIntentResolutionResult)
    ['{C4AC8B06-B807-402C-AB78-C2830AB34E69}']
  end;
  TINTemporalEventTriggerTypeOptionsResolutionResult = class(TOCGenericImport<INTemporalEventTriggerTypeOptionsResolutionResultClass,
    INTemporalEventTriggerTypeOptionsResolutionResult>) end;

  INTicketedEventClass = interface(NSObjectClass)
    ['{F3823B76-C195-4B9B-AE06-996694869C79}']
  end;

  INTicketedEvent = interface(NSObject)
    ['{3B73FEA6-6057-4C48-9F35-3D34D144A118}']
    function category: INTicketedEventCategory; cdecl;
    function eventDuration: INDateComponentsRange; cdecl;
    function initWithCategory(category: INTicketedEventCategory; name: NSString; eventDuration: INDateComponentsRange;
      location: CLPlacemark): Pointer; cdecl;
    function location: CLPlacemark; cdecl;
    function name: NSString; cdecl;
  end;
  TINTicketedEvent = class(TOCGenericImport<INTicketedEventClass, INTicketedEvent>) end;

  INTrainTripClass = interface(NSObjectClass)
    ['{018E403A-751D-4BA7-A092-BA733F2C8D89}']
  end;

  INTrainTrip = interface(NSObject)
    ['{57F54299-5B0D-4909-B548-991CBCBBB2A3}']
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
    ['{C5E20907-A35A-4DDD-96D8-A1088673E5F4}']
    {class} function successesWithResolvedMediaItems(resolvedMediaItems: NSArray): NSArray; cdecl;
    {class} function unsupportedForReason(reason: INUpdateMediaAffinityMediaItemUnsupportedReason): Pointer; cdecl;
  end;

  INUpdateMediaAffinityMediaItemResolutionResult = interface(INMediaItemResolutionResult)
    ['{E13D0765-949A-4546-A2D4-72869AEE90FE}']
    function initWithMediaItemResolutionResult(mediaItemResolutionResult: INMediaItemResolutionResult): Pointer; cdecl;
  end;
  TINUpdateMediaAffinityMediaItemResolutionResult = class(TOCGenericImport<INUpdateMediaAffinityMediaItemResolutionResultClass,
    INUpdateMediaAffinityMediaItemResolutionResult>) end;

  INVisualCodeTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{E4CF3551-457E-41DC-932B-99508E831B53}']
    {class} function confirmationRequiredWithVisualCodeTypeToConfirm(visualCodeTypeToConfirm: INVisualCodeType): Pointer; cdecl;
    {class} function successWithResolvedVisualCodeType(resolvedVisualCodeType: INVisualCodeType): Pointer; cdecl;
  end;

  INVisualCodeTypeResolutionResult = interface(INIntentResolutionResult)
    ['{8F55E7B8-2424-4F1C-85E3-4BA5B14FD259}']
  end;
  TINVisualCodeTypeResolutionResult = class(TOCGenericImport<INVisualCodeTypeResolutionResultClass, INVisualCodeTypeResolutionResult>) end;

  INWorkoutGoalUnitTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{47DC2F77-2861-4086-9295-FA7E359D1A97}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INWorkoutGoalUnitType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithWorkoutGoalUnitTypeToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function confirmationRequiredWithWorkoutGoalUnitTypeToConfirm(workoutGoalUnitTypeToConfirm: INWorkoutGoalUnitType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INWorkoutGoalUnitType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedWorkoutGoalUnitType:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedWorkoutGoalUnitType(resolvedWorkoutGoalUnitType: INWorkoutGoalUnitType): Pointer; cdecl;
  end;

  INWorkoutGoalUnitTypeResolutionResult = interface(INIntentResolutionResult)
    ['{347C20B0-ED76-45A1-ADD1-96FA5EE92B12}']
  end;
  TINWorkoutGoalUnitTypeResolutionResult = class(TOCGenericImport<INWorkoutGoalUnitTypeResolutionResultClass,
    INWorkoutGoalUnitTypeResolutionResult>) end;

  INWorkoutLocationTypeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{E70329E7-2EF1-4C6E-B530-D06BC6FDFB56}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: INWorkoutLocationType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+confirmationRequiredWithWorkoutLocationTypeToConfirm:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function confirmationRequiredWithWorkoutLocationTypeToConfirm(workoutLocationTypeToConfirm: INWorkoutLocationType): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: INWorkoutLocationType): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("+successWithResolvedWorkoutLocationType:", ios(10.0, 11.0), watchos(3.2, 4.0))
    {class} function successWithResolvedWorkoutLocationType(resolvedWorkoutLocationType: INWorkoutLocationType): Pointer; cdecl;
  end;

  INWorkoutLocationTypeResolutionResult = interface(INIntentResolutionResult)
    ['{6F8AC2B4-CF66-4C44-90F2-BE3A31A792B0}']
  end;
  TINWorkoutLocationTypeResolutionResult = class(TOCGenericImport<INWorkoutLocationTypeResolutionResultClass,
    INWorkoutLocationTypeResolutionResult>) end;

  INIntentDonationMetadataClass = interface(NSObjectClass)
    ['{3EA4DFE1-216F-4ACF-8D95-9F184424ABFE}']
  end;

  INIntentDonationMetadata = interface(NSObject)
    ['{C9275265-71D7-43AF-88F8-89F2BB9E8C10}']
  end;
  TINIntentDonationMetadata = class(TOCGenericImport<INIntentDonationMetadataClass, INIntentDonationMetadata>) end;

  INSendMessageIntentDonationMetadataClass = interface(INIntentDonationMetadataClass)
    ['{3F43FE2A-AF6B-4B67-82EB-44270F76AAEF}']
  end;

  INSendMessageIntentDonationMetadata = interface(INIntentDonationMetadata)
    ['{0CFE08B1-8D1C-4C7D-A870-8AA0BDBA9511}']
    function isReplyToCurrentUser: Boolean; cdecl;
    function mentionsCurrentUser: Boolean; cdecl;
    function notifyRecipientAnyway: Boolean; cdecl;
    function recipientCount: NSUInteger; cdecl;
    procedure setMentionsCurrentUser(mentionsCurrentUser: Boolean); cdecl;
    procedure setNotifyRecipientAnyway(notifyRecipientAnyway: Boolean); cdecl;
    procedure setRecipientCount(recipientCount: NSUInteger); cdecl;
    procedure setReplyToCurrentUser(replyToCurrentUser: Boolean); cdecl;
  end;
  TINSendMessageIntentDonationMetadata = class(TOCGenericImport<INSendMessageIntentDonationMetadataClass, INSendMessageIntentDonationMetadata>) end;

  INExtensionClass = interface(NSObjectClass)
    ['{687377D9-17B0-4A8F-B17D-E08A273A7AAF}']
  end;

  INExtension = interface(NSObject)
    ['{51360959-517B-4AB3-A5EA-0C83AA5BB9DD}']
  end;
  TINExtension = class(TOCGenericImport<INExtensionClass, INExtension>) end;

  INPersonHandleClass = interface(NSObjectClass)
    ['{B230232D-3B23-4D28-86AE-FEB5CAA251B3}']
  end;

  INPersonHandle = interface(NSObject)
    ['{477F5A18-50B8-4400-BBB2-83ABD8A53F9C}']
    function &label: INPersonHandleLabel; cdecl;
    function &type: INPersonHandleType; cdecl;
    function initWithValue(value: NSString; &type: INPersonHandleType): Pointer; overload; cdecl;
    function initWithValue(value: NSString; &type: INPersonHandleType; &label: INPersonHandleLabel): Pointer; overload; cdecl;
    function value: NSString; cdecl;
  end;
  TINPersonHandle = class(TOCGenericImport<INPersonHandleClass, INPersonHandle>) end;

  INCurrencyAmountClass = interface(NSObjectClass)
    ['{AF39FD79-DFC5-4968-87CF-EA961307BB5A}']
  end;

  INCurrencyAmount = interface(NSObject)
    ['{26989B18-EF18-4E65-BE56-C2676C06AA90}']
    function amount: NSDecimalNumber; cdecl;
    function currencyCode: NSString; cdecl;
    function initWithAmount(amount: NSDecimalNumber; currencyCode: NSString): Pointer; cdecl;
  end;
  TINCurrencyAmount = class(TOCGenericImport<INCurrencyAmountClass, INCurrencyAmount>) end;

  INDateComponentsRangeClass = interface(NSObjectClass)
    ['{2C59ED54-1E45-47C7-8921-00DCBF8BA6A2}']
  end;

  INDateComponentsRange = interface(NSObject)
    ['{B225F374-D39B-44E8-B04C-B195689218FB}']
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
    ['{99F66A84-32B2-4DB9-8035-803156A79D10}']
    {class} function imageNamed(name: NSString): Pointer; cdecl;
    {class} function imageWithImageData(imageData: NSData): Pointer; cdecl;
    {class} function imageWithURL(URL: NSURL; width: Double; height: Double): Pointer; overload; cdecl;
    {class} function imageWithURL(URL: NSURL): Pointer; overload; cdecl;
    {class} function systemImageNamed(systemImageName: NSString): Pointer; cdecl;
  end;

  INImage = interface(NSObject)
    ['{9CF4D743-2854-4053-A596-3CD6E664B5BF}']
  end;
  TINImage = class(TOCGenericImport<INImageClass, INImage>) end;

  INSpeakableStringClass = interface(NSObjectClass)
    ['{3D91E95A-8A13-4675-9B93-B4C9AA68ED08}']
  end;

  INSpeakableString = interface(NSObject)
    ['{D3C8404F-B67A-4430-B22C-105296F9D3BA}']
    function initWithIdentifier(identifier: NSString; spokenPhrase: NSString; pronunciationHint: NSString): Pointer; cdecl; // API_DEPRECATED("Please use -initWithVocabularyIdentifier:spokenPhrase:pronunciationHint:", ios(10.0, 11.0), watchos(3.2, 4.0))
    function initWithSpokenPhrase(spokenPhrase: NSString): Pointer; cdecl;
    function initWithVocabularyIdentifier(vocabularyIdentifier: NSString; spokenPhrase: NSString; pronunciationHint: NSString): Pointer; cdecl;
  end;
  TINSpeakableString = class(TOCGenericImport<INSpeakableStringClass, INSpeakableString>) end;

  INObjectClass = interface(NSObjectClass)
    ['{98EA542C-E768-4EDE-81BB-CABFDC27F7B7}']
  end;

  INObject = interface(NSObject)
    ['{5C1FD66A-0CE1-417B-B595-D346F7C7AA53}']
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
    ['{EEA3C216-62C1-4FF2-8D4D-5E1FB28C8205}']
  end;

  INPerson = interface(NSObject)
    ['{114FFDD2-2384-45D2-ABFF-24CBC1142E3F}']
    function aliases: NSArray; cdecl;
    function contactIdentifier: NSString; cdecl;
    function customIdentifier: NSString; cdecl;
    function displayName: NSString; cdecl;
    function handle: NSString; cdecl; // API_DEPRECATED("Use personHandle instead", ios(10.0, 10.0), macos(10.12, 10.12))
    function image: INImage; cdecl;
    function initWithHandle(handle: NSString; nameComponents: NSPersonNameComponents; contactIdentifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.0), macos(10.12, 10.12))
    function initWithHandle(handle: NSString; displayName: NSString; contactIdentifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.0), macos(10.12, 10.12))
    function initWithHandle(handle: NSString; nameComponents: NSPersonNameComponents; displayName: NSString; image: INImage;
      contactIdentifier: NSString): Pointer; overload; cdecl; // API_DEPRECATED("Use the designated initializer instead", ios(10.0, 10.0), macos(10.12, 10.12))
    [MethodName('initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:aliases:suggestionType:')]
    function initWithPersonHandleNameComponents(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString;
      image: INImage; contactIdentifier: NSString; customIdentifier: NSString; aliases: NSArray;
      suggestionType: INPersonSuggestionType): Pointer; overload; cdecl;
    [MethodName('initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:')]
    function initWithPersonHandleNameComponents(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString;
      image: INImage; contactIdentifier: NSString; customIdentifier: NSString): Pointer; overload; cdecl;
    [MethodName('initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isContactSuggestion:suggestionType:')]
    function initWithPersonHandleNameComponents(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString;
      image: INImage; contactIdentifier: NSString; customIdentifier: NSString; isContactSuggestion: Boolean;
      suggestionType: INPersonSuggestionType): Pointer; overload; cdecl;
    [MethodName('initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:suggestionType:')]
    function initWithPersonHandleNameComponentsIsMe(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString;
      image: INImage; contactIdentifier: NSString; customIdentifier: NSString; isMe: Boolean; suggestionType: INPersonSuggestionType): Pointer; cdecl;
    [MethodName('initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:isMe:')]
    function initWithPersonHandleNameComponents(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString;
      image: INImage; contactIdentifier: NSString; customIdentifier: NSString; isMe: Boolean): Pointer; overload; cdecl;
    [MethodName('initWithPersonHandle:nameComponents:displayName:image:contactIdentifier:customIdentifier:relationship:')]
    function initWithPersonHandleNameComponents(personHandle: INPersonHandle; nameComponents: NSPersonNameComponents; displayName: NSString;
      image: INImage; contactIdentifier: NSString; customIdentifier: NSString; relationship: INPersonRelationship): Pointer; overload; cdecl;
    function isContactSuggestion: Boolean; cdecl;
    function isMe: Boolean; cdecl;
    function nameComponents: NSPersonNameComponents; cdecl;
    function personHandle: INPersonHandle; cdecl;
    function relationship: INPersonRelationship; cdecl;
    function siriMatches: NSArray; cdecl;
    function suggestionType: INPersonSuggestionType; cdecl;
  end;
  TINPerson = class(TOCGenericImport<INPersonClass, INPerson>) end;

  INRecurrenceRuleClass = interface(NSObjectClass)
    ['{0F57F32F-D02B-457D-A8F9-20F5A3CAB811}']
  end;

  INRecurrenceRule = interface(NSObject)
    ['{82FD2260-E23F-45E4-86DB-F14C97777F09}']
    function frequency: INRecurrenceFrequency; cdecl;
    function initWithInterval(interval: NSUInteger; frequency: INRecurrenceFrequency;
      weeklyRecurrenceDays: INDayOfWeekOptions): Pointer; overload; cdecl;
    function initWithInterval(interval: NSUInteger; frequency: INRecurrenceFrequency): Pointer; overload; cdecl;
    function interval: NSUInteger; cdecl;
    function weeklyRecurrenceDays: INDayOfWeekOptions; cdecl;
  end;
  TINRecurrenceRule = class(TOCGenericImport<INRecurrenceRuleClass, INRecurrenceRule>) end;

  INFileClass = interface(NSObjectClass)
    ['{033E76D2-1EC8-4383-B25E-C80F947FA464}']
    {class} function fileWithData(data: NSData; filename: NSString; typeIdentifier: NSString): INFile; cdecl;
    {class} function fileWithFileURL(fileURL: NSURL; filename: NSString; typeIdentifier: NSString): INFile; cdecl;
  end;

  INFile = interface(NSObject)
    ['{142EEFE8-9396-48E1-9173-FD6C5DC0D705}']
    function data: NSData; cdecl;
    function filename: NSString; cdecl;
    function fileURL: NSURL; cdecl;
    function removedOnCompletion: Boolean; cdecl;
    procedure setFilename(filename: NSString); cdecl;
    procedure setRemovedOnCompletion(removedOnCompletion: Boolean); cdecl;
    function typeIdentifier: NSString; cdecl;
  end;
  TINFile = class(TOCGenericImport<INFileClass, INFile>) end;

  INBooleanResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{2B4908AB-81C2-42BE-A56C-715A2184998A}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: NSNumber): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: Boolean): Pointer; cdecl;
  end;

  INBooleanResolutionResult = interface(INIntentResolutionResult)
    ['{5DBD3567-EF38-4FE0-AC23-7A1B2A7A8491}']
  end;
  TINBooleanResolutionResult = class(TOCGenericImport<INBooleanResolutionResultClass, INBooleanResolutionResult>) end;

  INDateComponentsRangeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{0B78E9F3-A147-4DF3-B58C-CB31E2710E80}']
    {class} function confirmationRequiredWithDateComponentsRangeToConfirm(dateComponentsRangeToConfirm: INDateComponentsRange): Pointer; cdecl;
    {class} function disambiguationWithDateComponentsRangesToDisambiguate(dateComponentsRangesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedDateComponentsRange(resolvedDateComponentsRange: INDateComponentsRange): Pointer; cdecl;
  end;

  INDateComponentsRangeResolutionResult = interface(INIntentResolutionResult)
    ['{D35E91A5-20D5-4474-B3A3-21839FC40382}']
  end;
  TINDateComponentsRangeResolutionResult = class(TOCGenericImport<INDateComponentsRangeResolutionResultClass,
    INDateComponentsRangeResolutionResult>) end;

  INIntegerResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{CD120469-BCAC-4C8A-91C1-5A592297A9EC}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: NSNumber): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: NSInteger): Pointer; cdecl;
  end;

  INIntegerResolutionResult = interface(INIntentResolutionResult)
    ['{EF75EC77-7624-4166-876D-57E5B7D9D34D}']
  end;
  TINIntegerResolutionResult = class(TOCGenericImport<INIntegerResolutionResultClass, INIntegerResolutionResult>) end;

  INPlacemarkResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{8AED7374-37B5-4DAB-9C5A-D06806662965}']
    {class} function confirmationRequiredWithPlacemarkToConfirm(placemarkToConfirm: CLPlacemark): Pointer; cdecl;
    {class} function disambiguationWithPlacemarksToDisambiguate(placemarksToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedPlacemark(resolvedPlacemark: CLPlacemark): Pointer; cdecl;
  end;

  INPlacemarkResolutionResult = interface(INIntentResolutionResult)
    ['{02D3A32E-6006-4A0A-BC96-D81C2E0091E3}']
  end;
  TINPlacemarkResolutionResult = class(TOCGenericImport<INPlacemarkResolutionResultClass, INPlacemarkResolutionResult>) end;

  INSpeakableStringResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{E784D7FF-5859-4BA0-B0A4-6FB6EF5B8FDC}']
    {class} function confirmationRequiredWithStringToConfirm(stringToConfirm: INSpeakableString): Pointer; cdecl;
    {class} function disambiguationWithStringsToDisambiguate(stringsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedString(resolvedString: INSpeakableString): Pointer; cdecl;
  end;

  INSpeakableStringResolutionResult = interface(INIntentResolutionResult)
    ['{74D12320-6300-4150-A5CF-A51A4B612540}']
  end;
  TINSpeakableStringResolutionResult = class(TOCGenericImport<INSpeakableStringResolutionResultClass, INSpeakableStringResolutionResult>) end;

  INStringResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{E2378482-27EF-4DDE-A4FD-C961C0A7714F}']
    {class} function confirmationRequiredWithStringToConfirm(stringToConfirm: NSString): Pointer; cdecl;
    {class} function disambiguationWithStringsToDisambiguate(stringsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedString(resolvedString: NSString): Pointer; cdecl;
  end;

  INStringResolutionResult = interface(INIntentResolutionResult)
    ['{E3B7E075-396B-46D4-9AE6-D20133237255}']
  end;
  TINStringResolutionResult = class(TOCGenericImport<INStringResolutionResultClass, INStringResolutionResult>) end;

  INTemperatureResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{3834A728-A4E5-4552-92BE-00407596FD6C}']
    {class} function confirmationRequiredWithTemperatureToConfirm(temperatureToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithTemperaturesToDisambiguate(temperaturesToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedTemperature(resolvedTemperature: NSMeasurement): Pointer; cdecl;
  end;

  INTemperatureResolutionResult = interface(INIntentResolutionResult)
    ['{B9E94A39-2520-4C63-A3DE-FD250785BFD7}']
  end;
  TINTemperatureResolutionResult = class(TOCGenericImport<INTemperatureResolutionResultClass, INTemperatureResolutionResult>) end;

  INDateComponentsResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{8E0472C5-17D8-4E10-B298-3D10724D6A61}']
    {class} function confirmationRequiredWithDateComponentsToConfirm(dateComponentsToConfirm: NSDateComponents): Pointer; cdecl;
    {class} function disambiguationWithDateComponentsToDisambiguate(dateComponentsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedDateComponents(resolvedDateComponents: NSDateComponents): Pointer; cdecl;
  end;

  INDateComponentsResolutionResult = interface(INIntentResolutionResult)
    ['{33EA529F-93B4-43CD-935E-6327F44AC305}']
  end;
  TINDateComponentsResolutionResult = class(TOCGenericImport<INDateComponentsResolutionResultClass, INDateComponentsResolutionResult>) end;

  INRestaurantResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{FB830C2A-8746-4CF0-8BD6-7D3C6C9D0FA2}']
    {class} function confirmationRequiredWithRestaurantToConfirm(restaurantToConfirm: INRestaurant): Pointer; cdecl;
    {class} function disambiguationWithRestaurantsToDisambiguate(restaurantsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedRestaurant(resolvedRestaurant: INRestaurant): Pointer; cdecl;
  end;

  INRestaurantResolutionResult = interface(INIntentResolutionResult)
    ['{DD163DAD-09F5-402D-8F9B-8A32661C924A}']
  end;
  TINRestaurantResolutionResult = class(TOCGenericImport<INRestaurantResolutionResultClass, INRestaurantResolutionResult>) end;

  INRestaurantGuestResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{2DF5F24A-2C7C-408A-B707-44F902EAEFB0}']
    {class} function confirmationRequiredWithRestaurantGuestToConfirm(restaurantGuestToConfirm: INRestaurantGuest): Pointer; cdecl;
    {class} function disambiguationWithRestaurantGuestsToDisambiguate(restaurantGuestsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedRestaurantGuest(resolvedRestaurantGuest: INRestaurantGuest): Pointer; cdecl;
  end;

  INRestaurantGuestResolutionResult = interface(INIntentResolutionResult)
    ['{5954EE67-AE46-4655-B54C-9069A4061816}']
  end;
  TINRestaurantGuestResolutionResult = class(TOCGenericImport<INRestaurantGuestResolutionResultClass, INRestaurantGuestResolutionResult>) end;

  INURLResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{A68D0D49-04A7-4820-956B-E4562F3D4AA5}']
    {class} function confirmationRequiredWithURLToConfirm(urlToConfirm: NSURL): Pointer; cdecl;
    {class} function disambiguationWithURLsToDisambiguate(urlsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedURL(resolvedURL: NSURL): Pointer; cdecl;
  end;

  INURLResolutionResult = interface(INIntentResolutionResult)
    ['{A16C7B66-BE51-4B71-942F-3359EB62E926}']
  end;
  TINURLResolutionResult = class(TOCGenericImport<INURLResolutionResultClass, INURLResolutionResult>) end;

  INLengthResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{BD10C66B-045C-46AC-9014-A38277B2CE3F}']
    {class} function confirmationRequiredWithLengthToConfirm(lengthToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithLengthsToDisambiguate(lengthsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedLength(resolvedLength: NSMeasurement): Pointer; cdecl;
  end;

  INLengthResolutionResult = interface(INIntentResolutionResult)
    ['{7FDDD3F4-C7B1-42DD-97CF-8067966A1DD1}']
  end;
  TINLengthResolutionResult = class(TOCGenericImport<INLengthResolutionResultClass, INLengthResolutionResult>) end;

  INMassResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{7E035E52-3A1C-4928-B16A-95C13AD705CB}']
    {class} function confirmationRequiredWithMassToConfirm(massToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithMassToDisambiguate(massToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedMass(resolvedMass: NSMeasurement): Pointer; cdecl;
  end;

  INMassResolutionResult = interface(INIntentResolutionResult)
    ['{6A0DC4C8-B7F6-4440-BAC1-A26624D0F5CA}']
  end;
  TINMassResolutionResult = class(TOCGenericImport<INMassResolutionResultClass, INMassResolutionResult>) end;

  INVolumeResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{AB48C91A-3848-4CAE-AB2C-2C9479597775}']
    {class} function confirmationRequiredWithVolumeToConfirm(volumeToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithVolumeToDisambiguate(volumeToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedVolume(resolvedVolume: NSMeasurement): Pointer; cdecl;
  end;

  INVolumeResolutionResult = interface(INIntentResolutionResult)
    ['{B875D5C2-133C-4263-863B-FDB4EEFD54DA}']
  end;
  TINVolumeResolutionResult = class(TOCGenericImport<INVolumeResolutionResultClass, INVolumeResolutionResult>) end;

  INSpeedResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{44931984-5D59-46BD-B85F-EE88CFDF56B9}']
    {class} function confirmationRequiredWithSpeedToConfirm(speedToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithSpeedToDisambiguate(speedToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedSpeed(resolvedSpeed: NSMeasurement): Pointer; cdecl;
  end;

  INSpeedResolutionResult = interface(INIntentResolutionResult)
    ['{F4E29290-388E-4BD4-B874-6B8B2A4A382F}']
  end;
  TINSpeedResolutionResult = class(TOCGenericImport<INSpeedResolutionResultClass, INSpeedResolutionResult>) end;

  INEnergyResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{F0326077-A1C3-45C5-B4C1-A60233B1F63A}']
    {class} function confirmationRequiredWithEnergyToConfirm(energyToConfirm: NSMeasurement): Pointer; cdecl;
    {class} function disambiguationWithEnergyToDisambiguate(energyToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedEnergy(resolvedEnergy: NSMeasurement): Pointer; cdecl;
  end;

  INEnergyResolutionResult = interface(INIntentResolutionResult)
    ['{32309802-904F-4FC2-8A21-A37B3C805081}']
  end;
  TINEnergyResolutionResult = class(TOCGenericImport<INEnergyResolutionResultClass, INEnergyResolutionResult>) end;

  INEnumResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{AD7CB237-3BAC-466B-B3F3-E9CE50D900B1}']
    {class} function confirmationRequiredWithValueToConfirm(valueToConfirm: NSInteger): Pointer; cdecl;
    {class} function successWithResolvedValue(resolvedValue: NSInteger): Pointer; cdecl;
  end;

  INEnumResolutionResult = interface(INIntentResolutionResult)
    ['{88FEC144-DE17-4BE1-806A-E7C4DA8CB15C}']
  end;
  TINEnumResolutionResult = class(TOCGenericImport<INEnumResolutionResultClass, INEnumResolutionResult>) end;

  INObjectResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{10E0D27F-76CF-46A2-A9C6-3AE1028D0469}']
    {class} function confirmationRequiredWithObjectToConfirm(objectToConfirm: INObject): Pointer; cdecl;
    {class} function disambiguationWithObjectsToDisambiguate(objectsToDisambiguate: NSArray): Pointer; cdecl;
    {class} function successWithResolvedObject(resolvedObject: INObject): Pointer; cdecl;
  end;

  INObjectResolutionResult = interface(INIntentResolutionResult)
    ['{E5C9A2BF-3B21-41AD-A28E-4C05C4CF74EE}']
  end;
  TINObjectResolutionResult = class(TOCGenericImport<INObjectResolutionResultClass, INObjectResolutionResult>) end;

  INTimeIntervalResolutionResultClass = interface(INIntentResolutionResultClass)
    ['{DEFB53BE-9FDD-485B-AD38-54C13EBD942E}']
    {class} function confirmationRequiredWithTimeIntervalToConfirm(timeIntervalToConfirm: NSTimeInterval): Pointer; cdecl;
    {class} function successWithResolvedTimeInterval(resolvedTimeInterval: NSTimeInterval): Pointer; cdecl;
  end;

  INTimeIntervalResolutionResult = interface(INIntentResolutionResult)
    ['{ED5237ED-471A-4F30-96DE-327E560E2A50}']
  end;
  TINTimeIntervalResolutionResult = class(TOCGenericImport<INTimeIntervalResolutionResultClass, INTimeIntervalResolutionResult>) end;

  INMessageClass = interface(NSObjectClass)
    ['{954247B0-F02E-47B0-B801-4798015CD7B1}']
  end;

  INMessage = interface(NSObject)
    ['{86A2192F-A935-49C3-8D5F-9EC54289250B}']
    function audioMessageFile: INFile; cdecl;
    function content: NSString; cdecl;
    function conversationIdentifier: NSString; cdecl;
    function dateSent: NSDate; cdecl;
    function groupName: INSpeakableString; cdecl;
    function identifier: NSString; cdecl;
    function initWithIdentifier(identifier: NSString; conversationIdentifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray; groupName: INSpeakableString; messageType: INMessageType; serviceName: NSString;
      audioMessageFile: INFile): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; conversationIdentifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray; groupName: INSpeakableString; messageType: INMessageType): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; conversationIdentifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray; messageType: INMessageType): Pointer; overload; cdecl;
    function initWithIdentifier(identifier: NSString; conversationIdentifier: NSString; content: NSString; dateSent: NSDate; sender: INPerson;
      recipients: NSArray; groupName: INSpeakableString; messageType: INMessageType; serviceName: NSString): Pointer; overload; cdecl;
    function messageType: INMessageType; cdecl;
    function recipients: NSArray; cdecl;
    function sender: INPerson; cdecl;
    function serviceName: NSString; cdecl;
  end;
  TINMessage = class(TOCGenericImport<INMessageClass, INMessage>) end;

  INSendMessageAttachmentClass = interface(NSObjectClass)
    ['{BA52AD62-5477-49B9-8CBA-5C57B2C53EF1}']
    {class} function attachmentWithAudioMessageFile(audioMessageFile: INFile): INSendMessageAttachment; cdecl;
  end;

  INSendMessageAttachment = interface(NSObject)
    ['{464AE6CB-DFA7-4F17-8447-02CF214699C5}']
    function audioMessageFile: INFile; cdecl;
  end;
  TINSendMessageAttachment = class(TOCGenericImport<INSendMessageAttachmentClass, INSendMessageAttachment>) end;

  INBalanceAmountClass = interface(NSObjectClass)
    ['{CC0629DB-E307-44DB-986F-2665F81D5DC0}']
  end;

  INBalanceAmount = interface(NSObject)
    ['{5EB64EE4-32B7-49A0-8673-A558C2512638}']
    function amount: NSDecimalNumber; cdecl;
    function balanceType: INBalanceType; cdecl;
    function currencyCode: NSString; cdecl;
    function initWithAmount(amount: NSDecimalNumber; balanceType: INBalanceType): Pointer; overload; cdecl;
    function initWithAmount(amount: NSDecimalNumber; currencyCode: NSString): Pointer; overload; cdecl;
  end;
  TINBalanceAmount = class(TOCGenericImport<INBalanceAmountClass, INBalanceAmount>) end;

  INPriceRangeClass = interface(NSObjectClass)
    ['{A8F526E3-EF6C-4DBD-8B80-D6E40E230C0B}']
  end;

  INPriceRange = interface(NSObject)
    ['{BA3362DA-7A17-4544-8E99-B36C6A344D8E}']
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
    ['{5D61D408-9BE4-4B76-BF56-0EB7A041A3B8}']
  end;

  INRideOption = interface(NSObject)
    ['{AF32329B-0AF1-46DE-8DAA-892FB69269B6}']
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
    ['{41E45574-E555-4977-9A17-5D0E2E9C7609}']
  end;

  INRideStatus = interface(NSObject)
    ['{3B97B7BA-8429-464B-8C6C-4CA482F6D33E}']
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
    ['{9BDDEA7A-66D1-4895-A34F-9B1080397696}']
  end;

  INRideDriver = interface(INPerson)
    ['{A9093AB8-177A-48A5-B941-E11F180E7BA9}']
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
    ['{5506D182-0582-4CD0-B556-046331AA7DC2}']
  end;

  INRideVehicle = interface(NSObject)
    ['{D2B8CAC3-EC72-4EA2-AEBC-8F30E4EC4743}']
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
    ['{CCEE4C8C-8DAF-47FF-8AD4-4FC068306C61}']
  end;

  INRideFareLineItem = interface(NSObject)
    ['{2C284A98-061B-49B5-82B1-CB7D7256C6E8}']
    function currencyCode: NSString; cdecl;
    function initWithTitle(title: NSString; price: NSDecimalNumber; currencyCode: NSString): Pointer; cdecl;
    function price: NSDecimalNumber; cdecl;
    function title: NSString; cdecl;
  end;
  TINRideFareLineItem = class(TOCGenericImport<INRideFareLineItemClass, INRideFareLineItem>) end;

  INRidePartySizeOptionClass = interface(NSObjectClass)
    ['{0CD2558C-3AB8-467E-86DD-754D4E58C94F}']
  end;

  INRidePartySizeOption = interface(NSObject)
    ['{9E2EA987-FC51-4F9E-B445-71E8F7CEC566}']
    function initWithPartySizeRange(partySizeRange: NSRange; sizeDescription: NSString; priceRange: INPriceRange): Pointer; cdecl;
    function partySizeRange: NSRange; cdecl;
    function priceRange: INPriceRange; cdecl;
    function sizeDescription: NSString; cdecl;
  end;
  TINRidePartySizeOption = class(TOCGenericImport<INRidePartySizeOptionClass, INRidePartySizeOption>) end;

  INRideCompletionStatusClass = interface(NSObjectClass)
    ['{EFE57BC2-088D-4104-9AC2-248E8E9FF3B8}']
    {class} function canceledByService: Pointer; cdecl;
    {class} function canceledByUser: Pointer; cdecl;
    {class} function canceledMissedPickup: Pointer; cdecl;
    {class} function completed: Pointer; cdecl;
    {class} function completedWithOutstandingFeedbackType(feedbackType: INRideFeedbackTypeOptions): Pointer; cdecl;
    {class} function completedWithOutstandingPaymentAmount(outstandingPaymentAmount: INCurrencyAmount): Pointer; cdecl;
    {class} function completedWithSettledPaymentAmount(settledPaymentAmount: INCurrencyAmount): Pointer; cdecl;
  end;

  INRideCompletionStatus = interface(NSObject)
    ['{C8C2DB47-0C6D-47FD-94C0-4E268B985BD1}']
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
    ['{F8B77F68-E9B0-4AD2-B4ED-E0816A254EF0}']
  end;

  INReservation = interface(NSObject)
    ['{9F9F5B92-0602-48FA-8F05-2BE18776C44B}']
    function actions: NSArray; cdecl;
    function bookingTime: NSDate; cdecl;
    function itemReference: INSpeakableString; cdecl;
    function reservationHolderName: NSString; cdecl;
    function reservationNumber: NSString; cdecl;
    function reservationStatus: INReservationStatus; cdecl;
    function URL: NSURL; cdecl;
  end;
  TINReservation = class(TOCGenericImport<INReservationClass, INReservation>) end;

  INReservationActionClass = interface(NSObjectClass)
    ['{9643EA88-F05E-454E-9193-470A66E9E8EA}']
  end;

  INReservationAction = interface(NSObject)
    ['{E0342511-CE8F-4315-937F-D82A737122A6}']
    function &type: INReservationActionType; cdecl;
    function initWithType(&type: INReservationActionType; validDuration: INDateComponentsRange; userActivity: NSUserActivity): Pointer; cdecl;
    function userActivity: NSUserActivity; cdecl;
    function validDuration: INDateComponentsRange; cdecl;
  end;
  TINReservationAction = class(TOCGenericImport<INReservationActionClass, INReservationAction>) end;

  INFlightReservationClass = interface(INReservationClass)
    ['{3F2A3E91-7B7F-40F2-972F-3A57F9A0D305}']
  end;

  INFlightReservation = interface(INReservation)
    ['{9803FF6E-F8CE-481A-9C28-D1B8BBA726A6}']
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
    ['{AB00C270-8B39-455C-B489-5088D26427B6}']
  end;

  INLodgingReservation = interface(INReservation)
    ['{FF3AC175-938F-4F29-BCF7-56F95283465D}']
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
    ['{3E365229-62F6-4123-BCC0-A15F8D6B69E0}']
  end;

  INRentalCarReservation = interface(INReservation)
    ['{E2162F17-735A-467B-8BDF-799AD189CFDA}']
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
    ['{B7C1A276-1AF1-41B7-9C82-2208FBB7BADE}']
  end;

  INRestaurantReservation = interface(INReservation)
    ['{FE6C264B-7285-492D-83E0-43748C5988F6}']
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
    ['{6CD72C68-B527-4AA5-9F95-0325E1E65C0F}']
  end;

  INTicketedEventReservation = interface(INReservation)
    ['{89E7094D-140A-423A-BAA0-E52861518DA5}']
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
    ['{4B394B8F-B7F9-4F2D-9A75-1133DB852AC8}']
  end;

  INTrainReservation = interface(INReservation)
    ['{7BF6BAE3-5EDB-4FF6-8F01-5910A74F627F}']
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
    ['{BB4D7127-497B-460C-9015-730617C95751}']
  end;

  INBusReservation = interface(INReservation)
    ['{9F5EC37C-B015-4D35-BDD4-195601E565B8}']
    function busTrip: INBusTrip; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; reservedSeat: INSeat;
      busTrip: INBusTrip): Pointer; cdecl;
    function reservedSeat: INSeat; cdecl;
  end;
  TINBusReservation = class(TOCGenericImport<INBusReservationClass, INBusReservation>) end;

  INBoatReservationClass = interface(INReservationClass)
    ['{CB2F16E8-A5B7-493B-8496-C10C57ECC808}']
  end;

  INBoatReservation = interface(INReservation)
    ['{0FC51BAC-4444-4DE1-84DF-565907E44C5C}']
    function boatTrip: INBoatTrip; cdecl;
    function initWithItemReference(itemReference: INSpeakableString; reservationNumber: NSString; bookingTime: NSDate;
      reservationStatus: INReservationStatus; reservationHolderName: NSString; actions: NSArray; URL: NSURL; reservedSeat: INSeat;
      boatTrip: INBoatTrip): Pointer; cdecl;
    function reservedSeat: INSeat; cdecl;
  end;
  TINBoatReservation = class(TOCGenericImport<INBoatReservationClass, INBoatReservation>) end;

  INRestaurantGuestClass = interface(INPersonClass)
    ['{156972CD-3A01-4E96-9EBA-02067974AD43}']
  end;

  INRestaurantGuest = interface(INPerson)
    ['{C00B855B-0E49-4ABF-A439-7FFC9446EF55}']
    function emailAddress: NSString; cdecl;
    function initWithNameComponents(nameComponents: NSPersonNameComponents; phoneNumber: NSString; emailAddress: NSString): Pointer; cdecl;
    function phoneNumber: NSString; cdecl;
    procedure setEmailAddress(emailAddress: NSString); cdecl;
    procedure setPhoneNumber(phoneNumber: NSString); cdecl;
  end;
  TINRestaurantGuest = class(TOCGenericImport<INRestaurantGuestClass, INRestaurantGuest>) end;

  INTermsAndConditionsClass = interface(NSObjectClass)
    ['{901DEA14-2E9E-48DD-A814-761A2F450845}']
  end;

  INTermsAndConditions = interface(NSObject)
    ['{A35231D1-1C21-4216-B156-DE825CF9B5B2}']
    function initWithLocalizedTermsAndConditionsText(localizedTermsAndConditionsText: NSString; privacyPolicyURL: NSURL;
      termsAndConditionsURL: NSURL): Pointer; cdecl;
    function localizedTermsAndConditionsText: NSString; cdecl;
    function privacyPolicyURL: NSURL; cdecl;
    function termsAndConditionsURL: NSURL; cdecl;
  end;
  TINTermsAndConditions = class(TOCGenericImport<INTermsAndConditionsClass, INTermsAndConditions>) end;

  INRestaurantGuestDisplayPreferencesClass = interface(NSObjectClass)
    ['{A49E96E2-A76E-4DEF-9100-0F9A98BB888D}']
  end;

  INRestaurantGuestDisplayPreferences = interface(NSObject)
    ['{BC599E81-F915-4FDD-8BD8-E68F13E70629}']
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
    ['{E3F259B7-05AB-4109-AF6B-2CD17710C9AE}']
  end;

  INRestaurant = interface(NSObject)
    ['{A896DE40-9351-4386-BBC6-EDDC01748220}']
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
    ['{CFC42F88-856C-443E-8406-121BD8A99E5D}']
  end;

  INRestaurantOffer = interface(NSObject)
    ['{BEACE27B-1F70-490C-98A2-A0F81A0C83E8}']
    function offerDetailText: NSString; cdecl;
    function offerIdentifier: NSString; cdecl;
    function offerTitleText: NSString; cdecl;
    procedure setOfferDetailText(offerDetailText: NSString); cdecl;
    procedure setOfferIdentifier(offerIdentifier: NSString); cdecl;
    procedure setOfferTitleText(offerTitleText: NSString); cdecl;
  end;
  TINRestaurantOffer = class(TOCGenericImport<INRestaurantOfferClass, INRestaurantOffer>) end;

  INRestaurantReservationBookingClass = interface(NSObjectClass)
    ['{2C3A22B5-74F2-4CE3-BCEA-D3297D98D23C}']
  end;

  INRestaurantReservationBooking = interface(NSObject)
    ['{BFA6C342-8306-4EE5-B8E9-DFC863DCDDCD}']
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
    ['{A8C90405-FB55-4EB8-8CD5-0C3B479DE748}']
  end;

  INRestaurantReservationUserBooking = interface(INRestaurantReservationBooking)
    ['{511C37B0-AE9E-4FAC-99D4-C61133BC0565}']
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
    ['{BC4E3C59-F61C-4C77-89DD-028AEAFD6354}']
  end;

  INBookRestaurantReservationIntent = interface(INIntent)
    ['{67058441-AC2A-4531-98EC-C9A773B4522E}']
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
    ['{5D6692ED-2B37-4518-B2C7-5CA0758D2591}']
    procedure confirmBookRestaurantReservation(intent: INBookRestaurantReservationIntent; completion: Pointer); cdecl;
    procedure handleBookRestaurantReservation(intent: INBookRestaurantReservationIntent; completion: Pointer); cdecl;
    procedure resolveBookingDateComponentsForBookRestaurantReservation(intent: INBookRestaurantReservationIntent; withCompletion: Pointer); cdecl;
    procedure resolveGuestForBookRestaurantReservation(intent: INBookRestaurantReservationIntent; withCompletion: Pointer); cdecl;
    procedure resolveGuestProvidedSpecialRequestTextForBookRestaurantReservation(intent: INBookRestaurantReservationIntent;
      withCompletion: Pointer); cdecl;
    procedure resolvePartySizeForBookRestaurantReservation(intent: INBookRestaurantReservationIntent; withCompletion: Pointer); cdecl;
    procedure resolveRestaurantForBookRestaurantReservation(intent: INBookRestaurantReservationIntent; withCompletion: Pointer); cdecl;
  end;

  INGetAvailableRestaurantReservationBookingsIntentClass = interface(INIntentClass)
    ['{F1399403-9638-44A6-8B02-FA05A862CA0B}']
  end;

  INGetAvailableRestaurantReservationBookingsIntent = interface(INIntent)
    ['{3F9F3F66-256E-42BD-B9D9-A473976AFFBE}']
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
    ['{458B2A09-911A-4B14-82D4-8F5D4C6A1F7C}']
    procedure confirmGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent; completion: Pointer); cdecl;
    procedure handleGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent; completion: Pointer); cdecl;
    procedure resolvePartySizeForGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent;
      withCompletion: Pointer); cdecl;
    procedure resolvePreferredBookingDateComponentsForGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent; withCompletion: Pointer); cdecl;
    procedure resolveRestaurantForGetAvailableRestaurantReservationBookings(intent: INGetAvailableRestaurantReservationBookingsIntent;
      withCompletion: Pointer); cdecl;
  end;

  INGetUserCurrentRestaurantReservationBookingsIntentClass = interface(INIntentClass)
    ['{D6E5A173-4B82-4D51-924E-A37EEBE56327}']
  end;

  INGetUserCurrentRestaurantReservationBookingsIntent = interface(INIntent)
    ['{C3188458-7706-4F59-9B8B-127B2BF5CC29}']
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
    ['{58A554A4-9F44-4A22-814A-66099B9C9783}']
    procedure confirmGetUserCurrentRestaurantReservationBookings(intent: INGetUserCurrentRestaurantReservationBookingsIntent; completion: Pointer); cdecl;
    procedure handleGetUserCurrentRestaurantReservationBookings(intent: INGetUserCurrentRestaurantReservationBookingsIntent; completion: Pointer); cdecl;
    procedure resolveRestaurantForGetUserCurrentRestaurantReservationBookings(intent: INGetUserCurrentRestaurantReservationBookingsIntent;
      withCompletion: Pointer); cdecl;
  end;

  INGetAvailableRestaurantReservationBookingDefaultsIntentClass = interface(INIntentClass)
    ['{912E53CD-526C-415D-BB8D-ACB4217D697F}']
  end;

  INGetAvailableRestaurantReservationBookingDefaultsIntent = interface(INIntent)
    ['{5B780E24-00A9-4C7B-83E9-218D419573AB}']
    function initWithRestaurant(restaurant: INRestaurant): Pointer; cdecl;
    function restaurant: INRestaurant; cdecl;
    procedure setRestaurant(restaurant: INRestaurant); cdecl;
  end;
  TINGetAvailableRestaurantReservationBookingDefaultsIntent = class(TOCGenericImport<INGetAvailableRestaurantReservationBookingDefaultsIntentClass, INGetAvailableRestaurantReservationBookingDefaultsIntent>) end;

  INGetAvailableRestaurantReservationBookingDefaultsIntentHandling = interface(IObjectiveC)
    ['{68B6E14B-4032-4E36-9BF3-335713459667}']
    procedure confirmGetAvailableRestaurantReservationBookingDefaults(intent: INGetAvailableRestaurantReservationBookingDefaultsIntent; completion: Pointer); cdecl;
    procedure handleGetAvailableRestaurantReservationBookingDefaults(intent: INGetAvailableRestaurantReservationBookingDefaultsIntent; completion: Pointer); cdecl;
    procedure resolveRestaurantForGetAvailableRestaurantReservationBookingDefaults(intent: INGetAvailableRestaurantReservationBookingDefaultsIntent; withCompletion: Pointer); cdecl;
  end;

  INBookRestaurantReservationIntentResponseClass = interface(INIntentResponseClass)
    ['{9F6DCE8B-FDBA-4511-AB09-B6DB61A8EFBE}']
  end;

  INBookRestaurantReservationIntentResponse = interface(INIntentResponse)
    ['{488BC27E-B819-49EC-8991-9C283BBFF56D}']
    function code: INBookRestaurantReservationIntentCode; cdecl;
    function initWithCode(code: INBookRestaurantReservationIntentCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setUserBooking(userBooking: INRestaurantReservationUserBooking); cdecl;
    function userBooking: INRestaurantReservationUserBooking; cdecl;
  end;
  TINBookRestaurantReservationIntentResponse = class(TOCGenericImport<INBookRestaurantReservationIntentResponseClass, INBookRestaurantReservationIntentResponse>) end;

  INGetAvailableRestaurantReservationBookingsIntentResponseClass = interface(INIntentResponseClass)
    ['{14F0501E-52D1-425F-ADD7-38EFA7EA6AF3}']
  end;

  INGetAvailableRestaurantReservationBookingsIntentResponse = interface(INIntentResponse)
    ['{553D6068-B01D-4B0B-86E5-64A2A3021E9B}']
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
    ['{4350AE75-A7C2-4FDA-9343-CF4B0D7267B8}']
  end;

  INGetUserCurrentRestaurantReservationBookingsIntentResponse = interface(INIntentResponse)
    ['{46C01E75-243B-4890-8664-9E42B39B2D9F}']
    function code: INGetUserCurrentRestaurantReservationBookingsIntentResponseCode; cdecl;
    function initWithUserCurrentBookings(userCurrentBookings: NSArray; code: INGetUserCurrentRestaurantReservationBookingsIntentResponseCode;
      userActivity: NSUserActivity): Pointer; cdecl;
    procedure setUserCurrentBookings(userCurrentBookings: NSArray); cdecl;
    function userCurrentBookings: NSArray; cdecl;
  end;
  TINGetUserCurrentRestaurantReservationBookingsIntentResponse = class(TOCGenericImport<INGetUserCurrentRestaurantReservationBookingsIntentResponseClass,
    INGetUserCurrentRestaurantReservationBookingsIntentResponse>) end;

  INGetAvailableRestaurantReservationBookingDefaultsIntentResponseClass = interface(INIntentResponseClass)
    ['{6C32E5F5-80F7-4064-8F07-C037628FE4DB}']
  end;

  INGetAvailableRestaurantReservationBookingDefaultsIntentResponse = interface(INIntentResponse)
    ['{EBB56008-CF4B-441A-B523-86E04FD5BA66}']
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
    ['{C5CE3D9A-FB70-40BC-AE4D-B022F971AB91}']
  end;

  INGetRestaurantGuestIntent = interface(INIntent)
    ['{BEF45E56-16F4-4F43-A420-838319B0F289}']
  end;
  TINGetRestaurantGuestIntent = class(TOCGenericImport<INGetRestaurantGuestIntentClass, INGetRestaurantGuestIntent>) end;

  INGetRestaurantGuestIntentHandling = interface(IObjectiveC)
    ['{78E60A7C-CA1C-4CAC-A404-CAE0A0364E54}']
    procedure confirmGetRestaurantGuest(guestIntent: INGetRestaurantGuestIntent; completion: Pointer); cdecl;
    procedure handleGetRestaurantGuest(intent: INGetRestaurantGuestIntent; completion: Pointer); cdecl;
  end;

  INGetRestaurantGuestIntentResponseClass = interface(INIntentResponseClass)
    ['{142E264D-F5FB-4D89-A5DF-C176EDFF1D9F}']
  end;

  INGetRestaurantGuestIntentResponse = interface(INIntentResponse)
    ['{9F5F2407-6A2A-46CF-A604-F4B11006AC04}']
    function code: INGetRestaurantGuestIntentResponseCode; cdecl;
    function guest: INRestaurantGuest; cdecl;
    function guestDisplayPreferences: INRestaurantGuestDisplayPreferences; cdecl;
    function initWithCode(code: INGetRestaurantGuestIntentResponseCode; userActivity: NSUserActivity): Pointer; cdecl;
    procedure setGuest(guest: INRestaurantGuest); cdecl;
    procedure setGuestDisplayPreferences(guestDisplayPreferences: INRestaurantGuestDisplayPreferences); cdecl;
  end;
  TINGetRestaurantGuestIntentResponse = class(TOCGenericImport<INGetRestaurantGuestIntentResponseClass, INGetRestaurantGuestIntentResponse>) end;

  INVocabularyClass = interface(NSObjectClass)
    ['{015210B7-1CC5-4DDF-9785-A46619C1AC33}']
    {class} function sharedVocabulary: Pointer; cdecl;
  end;

  INVocabulary = interface(NSObject)
    ['{00AF50C3-0C31-4017-B574-91B135B5051A}']
    procedure removeAllVocabularyStrings; cdecl;
    procedure setVocabulary(vocabulary: NSOrderedSet; ofType: INVocabularyStringType); cdecl;
    procedure setVocabularyStrings(vocabulary: NSOrderedSet; ofType: INVocabularyStringType); cdecl;
  end;
  TINVocabulary = class(TOCGenericImport<INVocabularyClass, INVocabulary>) end;

  INUpcomingMediaManagerClass = interface(NSObjectClass)
    ['{3BDF204E-9E40-4335-81C1-036322DDEBE1}']
    {class} function sharedManager: INUpcomingMediaManager; cdecl;
  end;

  INUpcomingMediaManager = interface(NSObject)
    ['{4D8DDB78-FA5D-4E56-AB04-F58D56434B70}']
    procedure setPredictionMode(mode: INUpcomingMediaPredictionMode; forType: INMediaItemType); cdecl;
    procedure setSuggestedMediaIntents(intents: NSOrderedSet); cdecl;
  end;
  TINUpcomingMediaManager = class(TOCGenericImport<INUpcomingMediaManagerClass, INUpcomingMediaManager>) end;

  INPreferencesClass = interface(NSObjectClass)
    ['{7C10A86A-5774-4A28-9440-4E28BF6B0540}']
    {class} procedure requestSiriAuthorization(handler: TINPreferencesBlockMethod1); cdecl;
    {class} function siriAuthorizationStatus: INSiriAuthorizationStatus; cdecl;
    {class} function siriLanguageCode: NSString; cdecl;
  end;

  INPreferences = interface(NSObject)
    ['{58FFCF17-7AD6-4084-927E-015CE6C58119}']
  end;
  TINPreferences = class(TOCGenericImport<INPreferencesClass, INPreferences>) end;

  INFocusStatusCenterClass = interface(NSObjectClass)
    ['{EFAC5C92-9030-47A1-9E14-2071F9CF927D}']
    {class} function defaultCenter: INFocusStatusCenter; cdecl;
  end;

  INFocusStatusCenter = interface(NSObject)
    ['{662211E1-2939-4D74-AF43-C1B64D1E50DE}']
    function authorizationStatus: INFocusStatusAuthorizationStatus; cdecl;
    function focusStatus: INFocusStatus; cdecl;
    procedure requestAuthorizationWithCompletionHandler(completionHandler: TINFocusStatusCenterBlockMethod1); cdecl;
  end;
  TINFocusStatusCenter = class(TOCGenericImport<INFocusStatusCenterClass, INFocusStatusCenter>) end;

  INUserContextClass = interface(NSObjectClass)
    ['{FE5E7B51-D9BF-4281-B8B0-32B9D21B853D}']
  end;

  INUserContext = interface(NSObject)
    ['{515AB712-D11D-45A1-812D-9CD516D12AF1}']
    procedure becomeCurrent; cdecl;
  end;
  TINUserContext = class(TOCGenericImport<INUserContextClass, INUserContext>) end;

  INMediaUserContextClass = interface(INUserContextClass)
    ['{B552BFDB-1472-4869-94F5-24931EB16AF1}']
  end;

  INMediaUserContext = interface(INUserContext)
    ['{0CD3B02B-9E11-46FC-A981-D2A9F046A935}']
    function numberOfLibraryItems: NSNumber; cdecl;
    procedure setNumberOfLibraryItems(numberOfLibraryItems: NSNumber); cdecl;
    procedure setSubscriptionStatus(subscriptionStatus: INMediaUserContextSubscriptionStatus); cdecl;
    function subscriptionStatus: INMediaUserContextSubscriptionStatus; cdecl;
  end;
  TINMediaUserContext = class(TOCGenericImport<INMediaUserContextClass, INMediaUserContext>) end;

  INNoteContentClass = interface(NSObjectClass)
    ['{06C53C54-D300-4AC1-ADB7-641F42766AEC}']
  end;

  INNoteContent = interface(NSObject)
    ['{87877B50-94BC-4F58-BF96-6B37BAC7E02A}']
  end;
  TINNoteContent = class(TOCGenericImport<INNoteContentClass, INNoteContent>) end;

  INTextNoteContentClass = interface(INNoteContentClass)
    ['{63751C1E-7121-44AC-A379-1A4FA76061B3}']
  end;

  INTextNoteContent = interface(INNoteContent)
    ['{36F2B5B1-B221-42D2-94F0-708F6C2BBF2E}']
    function initWithText(text: NSString): Pointer; cdecl;
    function text: NSString; cdecl;
  end;
  TINTextNoteContent = class(TOCGenericImport<INTextNoteContentClass, INTextNoteContent>) end;

  INImageNoteContentClass = interface(INNoteContentClass)
    ['{F74BB4F5-7697-46C3-AAAF-ABDDE4D5599D}']
  end;

  INImageNoteContent = interface(INNoteContent)
    ['{3762CE90-1383-41CF-AAF4-43D166E95134}']
    function image: INImage; cdecl;
    function initWithImage(image: INImage): Pointer; cdecl;
  end;
  TINImageNoteContent = class(TOCGenericImport<INImageNoteContentClass, INImageNoteContent>) end;

  INRelevanceProviderClass = interface(NSObjectClass)
    ['{0B1B4460-2A7E-4882-A415-9C352B0DD93F}']
  end;

  INRelevanceProvider = interface(NSObject)
    ['{2190B245-2675-46B3-85AC-C9EA4D04BFFC}']
  end;
  TINRelevanceProvider = class(TOCGenericImport<INRelevanceProviderClass, INRelevanceProvider>) end;

  INDateRelevanceProviderClass = interface(INRelevanceProviderClass)
    ['{B102698D-874A-4321-99BD-07EF8F870534}']
  end;

  INDateRelevanceProvider = interface(INRelevanceProvider)
    ['{6718518A-4983-4682-B1D7-D1FF94492148}']
    function endDate: NSDate; cdecl;
    function initWithStartDate(startDate: NSDate; endDate: NSDate): Pointer; cdecl;
    function startDate: NSDate; cdecl;
  end;
  TINDateRelevanceProvider = class(TOCGenericImport<INDateRelevanceProviderClass, INDateRelevanceProvider>) end;

  INLocationRelevanceProviderClass = interface(INRelevanceProviderClass)
    ['{39B7831D-8E65-4BAF-99E7-B4CF83D7EAAF}']
  end;

  INLocationRelevanceProvider = interface(INRelevanceProvider)
    ['{267E8E24-9BD3-4A49-9505-B26EDF4FC325}']
    function initWithRegion(region: CLRegion): Pointer; cdecl;
    function region: CLRegion; cdecl;
  end;
  TINLocationRelevanceProvider = class(TOCGenericImport<INLocationRelevanceProviderClass, INLocationRelevanceProvider>) end;

  INDailyRoutineRelevanceProviderClass = interface(INRelevanceProviderClass)
    ['{D67C7FAB-2C8A-457E-B9BC-1BDB915516BC}']
  end;

  INDailyRoutineRelevanceProvider = interface(INRelevanceProvider)
    ['{C5E240B3-22DE-48B4-A709-42A668544C5A}']
    function initWithSituation(situation: INDailyRoutineSituation): Pointer; cdecl;
    function situation: INDailyRoutineSituation; cdecl;
  end;
  TINDailyRoutineRelevanceProvider = class(TOCGenericImport<INDailyRoutineRelevanceProviderClass, INDailyRoutineRelevanceProvider>) end;

  INDefaultCardTemplateClass = interface(NSObjectClass)
    ['{8D2C7C80-060E-4A50-AD14-1F5571227731}']
  end;

  INDefaultCardTemplate = interface(NSObject)
    ['{767D14F2-2C9F-445F-AC91-E482AB6DD333}']
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
    ['{DDDCB054-EC08-4B52-8E91-549D847E0F18}']
    {class} function new: Pointer; cdecl;
  end;

  INShortcut = interface(NSObject)
    ['{4E92037B-E93C-43F4-B767-79F6AFAAA1B6}']
    function initWithIntent(intent: INIntent): Pointer; cdecl;
    function initWithUserActivity(userActivity: NSUserActivity): Pointer; cdecl;
    function intent: INIntent; cdecl;
    function userActivity: NSUserActivity; cdecl;
  end;
  TINShortcut = class(TOCGenericImport<INShortcutClass, INShortcut>) end;

  INRelevantShortcutClass = interface(NSObjectClass)
    ['{2FA31DBE-B89C-4948-A7BE-B7280A2B00B4}']
  end;

  INRelevantShortcut = interface(NSObject)
    ['{BFB1A3A6-EA46-45D6-8CB2-B3C3175CD1B6}']
    function initWithShortcut(shortcut: INShortcut): Pointer; cdecl;
    function relevanceProviders: NSArray; cdecl;
    procedure setRelevanceProviders(relevanceProviders: NSArray); cdecl;
    procedure setShortcutRole(shortcutRole: INRelevantShortcutRole); cdecl;
    procedure setWatchTemplate(watchTemplate: INDefaultCardTemplate); cdecl;
    procedure setWidgetKind(widgetKind: NSString); cdecl;
    function shortcut: INShortcut; cdecl;
    function shortcutRole: INRelevantShortcutRole; cdecl;
    function watchTemplate: INDefaultCardTemplate; cdecl;
    function widgetKind: NSString; cdecl;
  end;
  TINRelevantShortcut = class(TOCGenericImport<INRelevantShortcutClass, INRelevantShortcut>) end;

  INRelevantShortcutStoreClass = interface(NSObjectClass)
    ['{1E2DDC23-5734-477B-8500-C11916383BA7}']
    {class} function defaultStore: INRelevantShortcutStore; cdecl;
  end;

  INRelevantShortcutStore = interface(NSObject)
    ['{E9DC5F2F-E790-4033-9239-7CFCE4C2739E}']
    procedure setRelevantShortcuts(shortcuts: NSArray; completionHandler: TINRelevantShortcutStoreBlockMethod1); cdecl;
  end;
  TINRelevantShortcutStore = class(TOCGenericImport<INRelevantShortcutStoreClass, INRelevantShortcutStore>) end;

  INVoiceShortcutClass = interface(NSObjectClass)
    ['{26C9B775-7D7E-4735-8F65-6C9DF14390F0}']
    {class} function new: Pointer; cdecl;
  end;

  INVoiceShortcut = interface(NSObject)
    ['{84604C7F-0736-4138-A519-92C7228D3D7A}']
    function identifier: NSUUID; cdecl;
    function invocationPhrase: NSString; cdecl;
    function shortcut: INShortcut; cdecl;
  end;
  TINVoiceShortcut = class(TOCGenericImport<INVoiceShortcutClass, INVoiceShortcut>) end;

  INVoiceShortcutCenterClass = interface(NSObjectClass)
    ['{301FBD76-C6C5-4786-A18E-029217E565B9}']
    {class} function new: Pointer; cdecl;
    {class} function sharedCenter: INVoiceShortcutCenter; cdecl;
  end;

  INVoiceShortcutCenter = interface(NSObject)
    ['{C7DEACEC-CD8B-4DFE-9C54-F1536672A560}']
    procedure getAllVoiceShortcutsWithCompletion(completionHandler: TINVoiceShortcutCenterBlockMethod1); cdecl;
    procedure getVoiceShortcutWithIdentifier(identifier: NSUUID; completion: TINVoiceShortcutCenterBlockMethod2); cdecl;
    procedure setShortcutSuggestions(suggestions: NSArray); cdecl;
  end;
  TINVoiceShortcutCenter = class(TOCGenericImport<INVoiceShortcutCenterClass, INVoiceShortcutCenter>) end;

  INMediaItemClass = interface(NSObjectClass)
    ['{7DB67492-7554-4B20-8AA1-5246DE3D3420}']
  end;

  INMediaItem = interface(NSObject)
    ['{A4D86FD4-15E3-4449-8930-9F54E58B8B09}']
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
function INIntentErrorDomain: NSString;
function INStartAudioCallIntentIdentifier: NSString;
function INStartVideoCallIntentIdentifier: NSString;
function INSearchCallHistoryIntentIdentifier: NSString;
function INStartCallIntentIdentifier: NSString;
function INAnswerCallIntentIdentifier: NSString;
function INHangUpCallIntentIdentifier: NSString;
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
  Posix.Dlfcn;

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

function INAnswerCallIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INAnswerCallIntentIdentifier');
end;

function INHangUpCallIntentIdentifier: NSString;
begin
  Result := CocoaNSStringConst(libIntents, 'INHangUpCallIntentIdentifier');
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
  dlclose(IntentsModule);

end.