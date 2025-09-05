unit DW.iOSapi.CallKit;

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
  iOSapi.CocoaTypes,
  {$IF CompilerVersion < 37} iOSapi.AVFoundation, {$ELSE} iOSapi.AVFAudio, {$ENDIF}
  iOSapi.Foundation;

const
  CXErrorCodeUnknownError = 0;
  CXErrorCodeUnentitled = 1;
  CXErrorCodeInvalidArgument = 2;
  CXErrorCodeMissingVoIPBackgroundMode = 3;
  CXErrorCodeIncomingCallErrorUnknown = 0;
  CXErrorCodeIncomingCallErrorUnentitled = 1;
  CXErrorCodeIncomingCallErrorCallUUIDAlreadyExists = 2;
  CXErrorCodeIncomingCallErrorFilteredByDoNotDisturb = 3;
  CXErrorCodeIncomingCallErrorFilteredByBlockList = 4;
  CXErrorCodeIncomingCallErrorFilteredDuringRestrictedSharingMode = 5;
  CXErrorCodeRequestTransactionErrorUnknown = 0;
  CXErrorCodeRequestTransactionErrorUnentitled = 1;
  CXErrorCodeRequestTransactionErrorUnknownCallProvider = 2;
  CXErrorCodeRequestTransactionErrorEmptyTransaction = 3;
  CXErrorCodeRequestTransactionErrorUnknownCallUUID = 4;
  CXErrorCodeRequestTransactionErrorCallUUIDAlreadyExists = 5;
  CXErrorCodeRequestTransactionErrorInvalidAction = 6;
  CXErrorCodeRequestTransactionErrorMaximumCallGroupsReached = 7;
  CXErrorCodeCallDirectoryManagerErrorUnknown = 0;
  CXErrorCodeCallDirectoryManagerErrorNoExtensionFound = 1;
  CXErrorCodeCallDirectoryManagerErrorLoadingInterrupted = 2;
  CXErrorCodeCallDirectoryManagerErrorEntriesOutOfOrder = 3;
  CXErrorCodeCallDirectoryManagerErrorDuplicateEntries = 4;
  CXErrorCodeCallDirectoryManagerErrorMaximumEntriesExceeded = 5;
  CXErrorCodeCallDirectoryManagerErrorExtensionDisabled = 6;
  CXErrorCodeCallDirectoryManagerErrorCurrentlyLoading = 7;
  CXErrorCodeCallDirectoryManagerErrorUnexpectedIncrementalRemoval = 8;
  CXErrorCodeNotificationServiceExtensionErrorUnknown = 0;
  CXErrorCodeNotificationServiceExtensionErrorInvalidClientProcess = 1;
  CXErrorCodeNotificationServiceExtensionErrorMissingNotificationFilteringEntitlement = 2;
  CXHandleTypeGeneric = 1;
  CXHandleTypePhoneNumber = 2;
  CXHandleTypeEmailAddress = 3;
  CXPlayDTMFCallActionTypeSingleTone = 1;
  CXPlayDTMFCallActionTypeSoftPause = 2;
  CXPlayDTMFCallActionTypeHardPause = 3;
  CXCallEndedReasonFailed = 1;
  CXCallEndedReasonRemoteEnded = 2;
  CXCallEndedReasonUnanswered = 3;
  CXCallEndedReasonAnsweredElsewhere = 4;
  CXCallEndedReasonDeclinedElsewhere = 5;
  CXCallDirectoryEnabledStatusUnknown = 0;
  CXCallDirectoryEnabledStatusDisabled = 1;
  CXCallDirectoryEnabledStatusEnabled = 2;

type
  CXAction = interface;
  CXTransaction = interface;
  CXCallUpdate = interface;
  CXHandle = interface;
  CXCallAction = interface;
  CXStartCallAction = interface;
  CXAnswerCallAction = interface;
  CXEndCallAction = interface;
  CXSetHeldCallAction = interface;
  CXSetMutedCallAction = interface;
  CXSetGroupCallAction = interface;
  CXPlayDTMFCallAction = interface;
  CXProviderDelegate = interface;
  CXProvider = interface;
  CXProviderConfiguration = interface;
  CXCall = interface;
  CXCallObserverDelegate = interface;
  CXCallObserver = interface;
  CXCallController = interface;
  CXCallDirectoryManager = interface;
  CXCallDirectoryProvider = interface;
  CXCallDirectoryExtensionContextDelegate = interface;
  CXCallDirectoryExtensionContext = interface;

  CXErrorCode = NSInteger;
  CXErrorCodeIncomingCallError = NSInteger;
  CXErrorCodeRequestTransactionError = NSInteger;
  CXErrorCodeCallDirectoryManagerError = NSInteger;
  CXErrorCodeNotificationServiceExtensionError = NSInteger;
  CXHandleType = NSInteger;
  CXPlayDTMFCallActionType = NSInteger;
  CXCallEndedReason = NSInteger;
  CXCallDirectoryPhoneNumber = Int64;
  CXCallDirectoryEnabledStatus = NSInteger;

  NSErrorDomain = NSString;

  TCXProviderBlockMethod1 = procedure(error: NSError) of object;
  TCXProviderBlockMethod2 = procedure(param1: NSError) of object;
  TCXCallControllerBlockMethod1 = procedure(error: NSError) of object;
  TCXCallDirectoryManagerBlockMethod1 = procedure(error: NSError) of object;
  TCXCallDirectoryManagerBlockMethod2 = procedure(enabledStatus: CXCallDirectoryEnabledStatus; error: NSError) of object;
  TCXCallDirectoryExtensionContextBlockMethod1 = procedure(expired: Boolean) of object;

  CXActionClass = interface(NSObjectClass)
    ['{808DEE02-E462-4F11-A9B2-F558DDBC32F4}']
  end;

  CXAction = interface(NSObject)
    ['{D238957E-AF0B-4182-B600-27E802547A07}']
    procedure fail; cdecl;
    procedure fulfill; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function isComplete: Boolean; cdecl;
    function timeoutDate: NSDate; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCXAction = class(TOCGenericImport<CXActionClass, CXAction>) end;

  CXTransactionClass = interface(NSObjectClass)
    ['{E828AC79-F61B-4249-A7C5-F344028FF21C}']
  end;

  CXTransaction = interface(NSObject)
    ['{370F88D9-7420-49C7-BE29-670E82B9D8CB}']
    function actions: NSArray; cdecl;
    procedure addAction(action: CXAction); cdecl;
    function initWithAction(action: CXAction): Pointer; cdecl;
    function initWithActions(actions: NSArray): Pointer; cdecl;
    function isComplete: Boolean; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCXTransaction = class(TOCGenericImport<CXTransactionClass, CXTransaction>) end;

  CXCallUpdateClass = interface(NSObjectClass)
    ['{F64ED29F-D47F-4EDE-88DA-FF39B6647CBC}']
  end;

  CXCallUpdate = interface(NSObject)
    ['{4A27650B-0273-48AF-A1F9-B8E7D2018947}']
    function hasVideo: Boolean; cdecl;
    function localizedCallerName: NSString; cdecl;
    function remoteHandle: CXHandle; cdecl;
    procedure setHasVideo(hasVideo: Boolean); cdecl;
    procedure setLocalizedCallerName(localizedCallerName: NSString); cdecl;
    procedure setRemoteHandle(remoteHandle: CXHandle); cdecl;
    procedure setSupportsDTMF(supportsDTMF: Boolean); cdecl;
    procedure setSupportsGrouping(supportsGrouping: Boolean); cdecl;
    procedure setSupportsHolding(supportsHolding: Boolean); cdecl;
    procedure setSupportsUngrouping(supportsUngrouping: Boolean); cdecl;
    function supportsDTMF: Boolean; cdecl;
    function supportsGrouping: Boolean; cdecl;
    function supportsHolding: Boolean; cdecl;
    function supportsUngrouping: Boolean; cdecl;
  end;
  TCXCallUpdate = class(TOCGenericImport<CXCallUpdateClass, CXCallUpdate>) end;

  CXHandleClass = interface(NSObjectClass)
    ['{C0733FFA-870A-4A3E-B9AF-84E247F5E741}']
  end;

  CXHandle = interface(NSObject)
    ['{7560D908-DEEF-4EBA-8DB4-F818FF31DA98}']
    function &type: CXHandleType; cdecl;
    function initWithType(&type: CXHandleType; value: NSString): Pointer; cdecl;
    function isEqualToHandle(handle: CXHandle): Boolean; cdecl;
    function value: NSString; cdecl;
  end;
  TCXHandle = class(TOCGenericImport<CXHandleClass, CXHandle>) end;

  CXCallActionClass = interface(CXActionClass)
    ['{F0968238-54A1-48AC-988A-8170104088C5}']
  end;

  CXCallAction = interface(CXAction)
    ['{18CF20F2-D864-4BA3-93A9-2BB4E22E5D9F}']
    function callUUID: NSUUID; cdecl;
    function initWithCallUUID(callUUID: NSUUID): Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
  end;
  TCXCallAction = class(TOCGenericImport<CXCallActionClass, CXCallAction>) end;

  CXStartCallActionClass = interface(CXCallActionClass)
    ['{5D20D385-241C-44E6-85AA-86E3FEED4D4A}']
  end;

  CXStartCallAction = interface(CXCallAction)
    ['{FE14BDDC-50CD-4004-BC75-4854BAF7ED32}']
    function contactIdentifier: NSString; cdecl;
    procedure fulfillWithDateStarted(dateStarted: NSDate); cdecl;
    function handle: CXHandle; cdecl;
    function initWithCallUUID(callUUID: NSUUID; handle: CXHandle): Pointer; overload; cdecl;
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function isVideo: Boolean; cdecl;
    procedure setContactIdentifier(contactIdentifier: NSString); cdecl;
    procedure setHandle(handle: CXHandle); cdecl;
    procedure setVideo(video: Boolean); cdecl;
  end;
  TCXStartCallAction = class(TOCGenericImport<CXStartCallActionClass, CXStartCallAction>) end;

  CXAnswerCallActionClass = interface(CXCallActionClass)
    ['{369D17A5-D9E4-4DC9-9418-97116834696C}']
  end;

  CXAnswerCallAction = interface(CXCallAction)
    ['{3875B204-2ECA-49D8-B1C8-6C737DFF52B2}']
    procedure fulfillWithDateConnected(dateConnected: NSDate); cdecl;
  end;
  TCXAnswerCallAction = class(TOCGenericImport<CXAnswerCallActionClass, CXAnswerCallAction>) end;

  CXEndCallActionClass = interface(CXCallActionClass)
    ['{591F27CE-64A5-4053-9308-54CC1EADCDBF}']
  end;

  CXEndCallAction = interface(CXCallAction)
    ['{EA6D2B72-BB60-4415-B4D0-DFD4783B9244}']
    procedure fulfillWithDateEnded(dateEnded: NSDate); cdecl;
  end;
  TCXEndCallAction = class(TOCGenericImport<CXEndCallActionClass, CXEndCallAction>) end;

  CXSetHeldCallActionClass = interface(CXCallActionClass)
    ['{00821443-B143-4CF3-9286-9A0F2554553F}']
  end;

  CXSetHeldCallAction = interface(CXCallAction)
    ['{EA254ADC-11DA-4510-9739-9D3688AA6CAB}']
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    function initWithCallUUID(callUUID: NSUUID; onHold: Boolean): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function isOnHold: Boolean; cdecl;
    procedure setOnHold(onHold: Boolean); cdecl;
  end;
  TCXSetHeldCallAction = class(TOCGenericImport<CXSetHeldCallActionClass, CXSetHeldCallAction>) end;

  CXSetMutedCallActionClass = interface(CXCallActionClass)
    ['{529B3161-447E-4376-B4D1-F879D69BB1B0}']
  end;

  CXSetMutedCallAction = interface(CXCallAction)
    ['{2A7680CF-EEC4-40A5-9F77-5F6A4CAE9E2A}']
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    function initWithCallUUID(callUUID: NSUUID; muted: Boolean): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function isMuted: Boolean; cdecl;
    procedure setMuted(muted: Boolean); cdecl;
  end;
  TCXSetMutedCallAction = class(TOCGenericImport<CXSetMutedCallActionClass, CXSetMutedCallAction>) end;

  CXSetGroupCallActionClass = interface(CXCallActionClass)
    ['{EE21C9E1-5F99-4A14-B519-5C04DEDF65A1}']
  end;

  CXSetGroupCallAction = interface(CXCallAction)
    ['{3973E64A-1822-45B0-BEB4-5194BF30C153}']
    function callUUIDToGroupWith: NSUUID; cdecl;
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    function initWithCallUUID(callUUID: NSUUID; callUUIDToGroupWith: NSUUID): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    procedure setCallUUIDToGroupWith(callUUIDToGroupWith: NSUUID); cdecl;
  end;
  TCXSetGroupCallAction = class(TOCGenericImport<CXSetGroupCallActionClass, CXSetGroupCallAction>) end;

  CXPlayDTMFCallActionClass = interface(CXCallActionClass)
    ['{2217AA45-B4D7-4DAF-AD06-C8BD58233A0A}']
  end;

  CXPlayDTMFCallAction = interface(CXCallAction)
    ['{119742BF-0AC2-4877-B75E-D36574B4E4E9}']
    function &type: CXPlayDTMFCallActionType; cdecl;
    function digits: NSString; cdecl;
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    function initWithCallUUID(callUUID: NSUUID; digits: NSString; &type: CXPlayDTMFCallActionType): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    procedure setDigits(digits: NSString); cdecl;
    procedure setType(&type: CXPlayDTMFCallActionType); cdecl;
  end;
  TCXPlayDTMFCallAction = class(TOCGenericImport<CXPlayDTMFCallActionClass, CXPlayDTMFCallAction>) end;

  CXProviderDelegate = interface(IObjectiveC)
    ['{A804F9B5-DC0A-4A5E-8F55-D31A29380FBE}']
    procedure provider(provider: CXProvider; didActivateAudioSession: AVAudioSession); overload; cdecl;
    function provider(provider: CXProvider; executeTransaction: CXTransaction): Boolean; overload; cdecl;
    procedure provider(provider: CXProvider; performAnswerCallAction: CXAnswerCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performEndCallAction: CXEndCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performPlayDTMFCallAction: CXPlayDTMFCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performSetGroupCallAction: CXSetGroupCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performSetHeldCallAction: CXSetHeldCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performSetMutedCallAction: CXSetMutedCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; performStartCallAction: CXStartCallAction); overload; cdecl;
    procedure provider(provider: CXProvider; timedOutPerformingAction: CXAction); overload; cdecl;
    procedure providerDidBegin(provider: CXProvider); cdecl;
    [MethodName('provider:didDeactivateAudioSession:')]
    procedure providerDidDeactivateAudioSession(provider: CXProvider; didDeactivateAudioSession: AVAudioSession); cdecl;
    procedure providerDidReset(provider: CXProvider); cdecl;
  end;

  CXProviderClass = interface(NSObjectClass)
    ['{8F618640-09C3-41CD-B53F-020F97867BC3}']
    {class} function new: Pointer; cdecl;
    {class} procedure reportNewIncomingVoIPPushPayload(dictionaryPayload: NSDictionary; completion: TCXProviderBlockMethod2); cdecl;
  end;

  CXProvider = interface(NSObject)
    ['{C84B5415-7C52-44DB-8B1A-2E4EEBC2FA2C}']
    function configuration: CXProviderConfiguration; cdecl;
    function initWithConfiguration(configuration: CXProviderConfiguration): Pointer; cdecl;
    procedure invalidate; cdecl;
    function pendingCallActionsOfClass(callActionClass: Pointer; withCallUUID: NSUUID): NSArray; cdecl;
    function pendingTransactions: NSArray; cdecl;
    procedure reportCallWithUUID(UUID: NSUUID; updated: CXCallUpdate); overload; cdecl;
    procedure reportCallWithUUID(UUID: NSUUID; endedAtDate: NSDate; reason: CXCallEndedReason); overload; cdecl;
    procedure reportNewIncomingCallWithUUID(UUID: NSUUID; update: CXCallUpdate; completion: TCXProviderBlockMethod1); cdecl;
    procedure reportOutgoingCallWithUUID(UUID: NSUUID; startedConnectingAtDate: NSDate); cdecl;
    [MethodName('reportOutgoingCallWithUUID:connectedAtDate:')]
    procedure reportOutgoingCallWithUUIDConnectedAtDate(UUID: NSUUID; connectedAtDate: NSDate); cdecl;
    procedure setConfiguration(configuration: CXProviderConfiguration); cdecl;
    procedure setDelegate(delegate: Pointer; queue: dispatch_queue_t); cdecl;
  end;
  TCXProvider = class(TOCGenericImport<CXProviderClass, CXProvider>) end;

  CXProviderConfigurationClass = interface(NSObjectClass)
    ['{DD0145B3-FBEB-4965-9AF4-EC082B078FDC}']
  end;

  CXProviderConfiguration = interface(NSObject)
    ['{FBF8824E-EB76-4F7E-B7DA-FE40601FE2B0}']
    function iconTemplateImageData: NSData; cdecl;
    function includesCallsInRecents: Boolean; cdecl;
    function initWithLocalizedName(localizedName: NSString): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("init", ios(10.0, 14.0), macCatalyst(13.0, 14.0), macos(11.0, 11.0))
    function localizedName: NSString; cdecl; // API_DEPRECATED("No longer supported", ios(10.0, 14.0), macCatalyst(13.0, 14.0), macos(11.0, 11.0))
    function maximumCallGroups: NSUInteger; cdecl;
    function maximumCallsPerCallGroup: NSUInteger; cdecl;
    function ringtoneSound: NSString; cdecl;
    procedure setIconTemplateImageData(iconTemplateImageData: NSData); cdecl;
    procedure setIncludesCallsInRecents(includesCallsInRecents: Boolean); cdecl;
    procedure setMaximumCallGroups(maximumCallGroups: NSUInteger); cdecl;
    procedure setMaximumCallsPerCallGroup(maximumCallsPerCallGroup: NSUInteger); cdecl;
    procedure setRingtoneSound(ringtoneSound: NSString); cdecl;
    procedure setSupportedHandleTypes(supportedHandleTypes: NSSet); cdecl;
    procedure setSupportsVideo(supportsVideo: Boolean); cdecl;
    function supportedHandleTypes: NSSet; cdecl;
    function supportsVideo: Boolean; cdecl;
  end;
  TCXProviderConfiguration = class(TOCGenericImport<CXProviderConfigurationClass, CXProviderConfiguration>) end;

  CXCallClass = interface(NSObjectClass)
    ['{EF896431-D53D-4897-A4CE-319A2B9B06BC}']
  end;

  CXCall = interface(NSObject)
    ['{2CC6A1DE-88AF-4B62-8392-54FCCBE73654}']
    function hasConnected: Boolean; cdecl;
    function hasEnded: Boolean; cdecl;
    function isEqualToCall(call: CXCall): Boolean; cdecl;
    function isOnHold: Boolean; cdecl;
    function isOutgoing: Boolean; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCXCall = class(TOCGenericImport<CXCallClass, CXCall>) end;

  CXCallObserverDelegate = interface(IObjectiveC)
    ['{EB2FC593-6A13-4A45-A834-4ECF70C04EC4}']
    procedure callObserver(callObserver: CXCallObserver; callChanged: CXCall); cdecl;
  end;

  CXCallObserverClass = interface(NSObjectClass)
    ['{BD06FEAB-4567-46A6-A3E5-8F3CA12BC304}']
  end;

  CXCallObserver = interface(NSObject)
    ['{CB0CED99-2D74-429F-A5DC-838F1FCF5404}']
    function calls: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer; queue: dispatch_queue_t); cdecl;
  end;
  TCXCallObserver = class(TOCGenericImport<CXCallObserverClass, CXCallObserver>) end;

  CXCallControllerClass = interface(NSObjectClass)
    ['{59918814-A06D-4FDC-86A3-4AFBE91480C8}']
  end;

  CXCallController = interface(NSObject)
    ['{20DCC1BD-ECB1-4444-989A-E62BB5830635}']
    function callObserver: CXCallObserver; cdecl;
    function initWithQueue(queue: dispatch_queue_t): Pointer; cdecl;
    procedure requestTransaction(transaction: CXTransaction; completion: TCXCallControllerBlockMethod1); cdecl;
    procedure requestTransactionWithAction(action: CXAction; completion: TCXCallControllerBlockMethod1); cdecl;
    procedure requestTransactionWithActions(actions: NSArray; completion: TCXCallControllerBlockMethod1); cdecl;
  end;
  TCXCallController = class(TOCGenericImport<CXCallControllerClass, CXCallController>) end;

  CXCallDirectoryManagerClass = interface(NSObjectClass)
    ['{374AB20F-9019-4E94-872A-5CA76E75BD67}']
    {class} function sharedInstance: CXCallDirectoryManager; cdecl;
  end;

  CXCallDirectoryManager = interface(NSObject)
    ['{25F48B83-5457-4A30-A4DC-53BF9BF5E7A6}']
    procedure getEnabledStatusForExtensionWithIdentifier(identifier: NSString; completionHandler: TCXCallDirectoryManagerBlockMethod2); cdecl;
    procedure openSettingsWithCompletionHandler(completion: TCXCallDirectoryManagerBlockMethod1); cdecl;
    procedure reloadExtensionWithIdentifier(identifier: NSString; completionHandler: TCXCallDirectoryManagerBlockMethod1); cdecl;
  end;
  TCXCallDirectoryManager = class(TOCGenericImport<CXCallDirectoryManagerClass, CXCallDirectoryManager>) end;

  CXCallDirectoryProviderClass = interface(NSObjectClass)
    ['{6D54A2CD-212E-47F8-80A4-3F5C82CB82E2}']
  end;

  CXCallDirectoryProvider = interface(NSObject)
    ['{25089246-270B-411E-84BB-BD1B935AF9C9}']
    procedure beginRequestWithExtensionContext(context: CXCallDirectoryExtensionContext); cdecl;
  end;
  TCXCallDirectoryProvider = class(TOCGenericImport<CXCallDirectoryProviderClass, CXCallDirectoryProvider>) end;

  CXCallDirectoryExtensionContextDelegate = interface(IObjectiveC)
    ['{5E3A5987-A934-4B18-8D9F-E6B70C3D07E3}']
    procedure requestFailedForExtensionContext(extensionContext: CXCallDirectoryExtensionContext; withError: NSError); cdecl;
  end;

  CXCallDirectoryExtensionContextClass = interface(NSExtensionContextClass)
    ['{7267D205-2CAD-4E17-B4F6-485832F38F47}']
  end;

  CXCallDirectoryExtensionContext = interface(NSExtensionContext)
    ['{4F1BD324-E537-4EB9-97A2-F2C474B225BB}']
    procedure addBlockingEntryWithNextSequentialPhoneNumber(phoneNumber: CXCallDirectoryPhoneNumber); cdecl;
    procedure addIdentificationEntryWithNextSequentialPhoneNumber(phoneNumber: CXCallDirectoryPhoneNumber; &label: NSString); cdecl;
    procedure completeRequestReturningItems(items: NSArray; completionHandler: TCXCallDirectoryExtensionContextBlockMethod1); cdecl;
    procedure completeRequestWithCompletionHandler(completion: TCXCallDirectoryExtensionContextBlockMethod1); cdecl;
    function delegate: Pointer; cdecl;
    function isIncremental: Boolean; cdecl;
    procedure removeAllBlockingEntries; cdecl;
    procedure removeAllIdentificationEntries; cdecl;
    procedure removeBlockingEntryWithPhoneNumber(phoneNumber: CXCallDirectoryPhoneNumber); cdecl;
    procedure removeIdentificationEntryWithPhoneNumber(phoneNumber: CXCallDirectoryPhoneNumber); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TCXCallDirectoryExtensionContext = class(TOCGenericImport<CXCallDirectoryExtensionContextClass, CXCallDirectoryExtensionContext>) end;

function CXErrorDomain: NSErrorDomain;
function CXErrorDomainIncomingCall: NSErrorDomain;
function CXErrorDomainRequestTransaction: NSErrorDomain;
function CXErrorDomainCallDirectoryManager: NSErrorDomain;
function CXErrorDomainNotificationServiceExtension: NSErrorDomain;

const
  libCallKit = '/System/Library/Frameworks/CallKit.framework/CallKit';

implementation

uses
  Posix.Dlfcn;

var
  CallKitModule: THandle;

function CXErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libCallKit, 'CXErrorDomain');
end;

function CXErrorDomainIncomingCall: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libCallKit, 'CXErrorDomainIncomingCall');
end;

function CXErrorDomainRequestTransaction: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libCallKit, 'CXErrorDomainRequestTransaction');
end;

function CXErrorDomainCallDirectoryManager: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libCallKit, 'CXErrorDomainCallDirectoryManager');
end;

function CXErrorDomainNotificationServiceExtension: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libCallKit, 'CXErrorDomainNotificationServiceExtension');
end;

initialization
  CallKitModule := dlopen(MarshaledAString(libCallKit), RTLD_LAZY);

finalization
  dlclose(CallKitModule);

end.