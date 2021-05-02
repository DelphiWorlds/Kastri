unit DW.iOSapi.CallKit;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.AVFoundation;

const
  CXErrorCodeUnknownError = 0;
  CXErrorCodeIncomingCallErrorUnknown = 0;
  CXErrorCodeIncomingCallErrorUnentitled = 1;
  CXErrorCodeIncomingCallErrorCallUUIDAlreadyExists = 2;
  CXErrorCodeIncomingCallErrorFilteredByDoNotDisturb = 3;
  CXErrorCodeIncomingCallErrorFilteredByBlockList = 4;
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
  CXHandleType = NSInteger;
  CXPlayDTMFCallActionType = NSInteger;
  CXCallEndedReason = NSInteger;
  CXCallDirectoryPhoneNumber = Int64;
  CXCallDirectoryEnabledStatus = NSInteger;
  NSErrorDomain = NSString;

  TCXProviderBlockMethod1 = procedure(error: NSError) of object;
  TCXCallControllerBlockMethod1 = procedure(error: NSError) of object;
  TCXCallDirectoryManagerBlockMethod1 = procedure(error: NSError) of object;
  TCXCallDirectoryManagerBlockMethod2 = procedure(enabledStatus: CXCallDirectoryEnabledStatus; error: NSError) of object;
  TCXCallDirectoryExtensionContextBlockMethod1 = procedure(expired: Boolean) of object;

  CXActionClass = interface(NSObjectClass)
    ['{0BC6FA95-53FA-47E0-96ED-5CCD98E5810D}']
  end;

  CXAction = interface(NSObject)
    ['{601E4B2B-F9C9-49D9-9289-6E475E80C745}']
    procedure fail; cdecl;
    procedure fulfill; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function isComplete: Boolean; cdecl;
    function timeoutDate: NSDate; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCXAction = class(TOCGenericImport<CXActionClass, CXAction>) end;

  CXTransactionClass = interface(NSObjectClass)
    ['{B2000E61-3A4D-486A-96FC-9759033BD35A}']
  end;

  CXTransaction = interface(NSObject)
    ['{1F562EF3-F025-4D65-85C4-4007A8D78D0F}']
    function actions: NSArray; cdecl;
    procedure addAction(action: CXAction); cdecl;
    function initWithAction(action: CXAction): Pointer; cdecl;
    function initWithActions(actions: NSArray): Pointer; cdecl;
    function isComplete: Boolean; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCXTransaction = class(TOCGenericImport<CXTransactionClass, CXTransaction>) end;

  CXCallUpdateClass = interface(NSObjectClass)
    ['{C188915D-7DED-4A2A-9FDF-DF100369AA3E}']
  end;

  CXCallUpdate = interface(NSObject)
    ['{9C231A14-0495-4CA3-9717-014F6F8ECE9B}']
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
    ['{6537340C-1650-4CE8-8B8B-4696EE6A2483}']
  end;

  CXHandle = interface(NSObject)
    ['{AA749A4D-0911-40D7-A0BF-81EAD0B1688C}']
    function &type: CXHandleType; cdecl;
    [MethodName('initWithType:value:')]
    function initWithType(&type: CXHandleType; value: NSString): Pointer; cdecl;
    function isEqualToHandle(handle: CXHandle): Boolean; cdecl;
    function value: NSString; cdecl;
  end;
  TCXHandle = class(TOCGenericImport<CXHandleClass, CXHandle>) end;

  CXCallActionClass = interface(CXActionClass)
    ['{5CD966CA-FF28-4C3E-B789-0562F69276AA}']
  end;

  CXCallAction = interface(CXAction)
    ['{0640511A-C60E-43CF-8FA3-84BC932275BE}']
    function callUUID: NSUUID; cdecl;
    function initWithCallUUID(callUUID: NSUUID): Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
  end;
  TCXCallAction = class(TOCGenericImport<CXCallActionClass, CXCallAction>) end;

  CXStartCallActionClass = interface(CXCallActionClass)
    ['{2A5E2FA0-6313-40BD-9734-E2FF192B003D}']
  end;

  CXStartCallAction = interface(CXCallAction)
    ['{DEC52AFA-91D6-42D3-AFA4-3618778B1EA2}']
    function contactIdentifier: NSString; cdecl;
    procedure fulfillWithDateStarted(dateStarted: NSDate); cdecl;
    function handle: CXHandle; cdecl;
    [MethodName('initWithCallUUID:handle:')]
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
    ['{ABE5ED2B-E7EB-4009-8C36-F06E26AE4084}']
  end;

  CXAnswerCallAction = interface(CXCallAction)
    ['{B20D92F4-3BBA-41EB-B7BB-ED1D9FBB9A50}']
    procedure fulfillWithDateConnected(dateConnected: NSDate); cdecl;
  end;
  TCXAnswerCallAction = class(TOCGenericImport<CXAnswerCallActionClass, CXAnswerCallAction>) end;

  CXEndCallActionClass = interface(CXCallActionClass)
    ['{D820A11D-29B9-49C4-A0AB-DDAA9F911A3A}']
  end;

  CXEndCallAction = interface(CXCallAction)
    ['{E202410F-B756-4DF7-891D-0616B987B78C}']
    procedure fulfillWithDateEnded(dateEnded: NSDate); cdecl;
  end;
  TCXEndCallAction = class(TOCGenericImport<CXEndCallActionClass, CXEndCallAction>) end;

  CXSetHeldCallActionClass = interface(CXCallActionClass)
    ['{92428B28-E7DB-431A-B8D1-BA799B914585}']
  end;

  CXSetHeldCallAction = interface(CXCallAction)
    ['{69C091F8-D01C-4791-ACC0-DEC0E1D445FD}']
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    [MethodName('initWithCallUUID:onHold:')]
    function initWithCallUUID(callUUID: NSUUID; onHold: Boolean): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function isOnHold: Boolean; cdecl;
    procedure setOnHold(onHold: Boolean); cdecl;
  end;
  TCXSetHeldCallAction = class(TOCGenericImport<CXSetHeldCallActionClass, CXSetHeldCallAction>) end;

  CXSetMutedCallActionClass = interface(CXCallActionClass)
    ['{BEC9BC45-BE5B-4CAC-A390-3B7DE1DBB77B}']
  end;

  CXSetMutedCallAction = interface(CXCallAction)
    ['{02A82FC4-9B64-4393-8BAA-7051A3A0DF77}']
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    [MethodName('initWithCallUUID:muted:')]
    function initWithCallUUID(callUUID: NSUUID; muted: Boolean): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function isMuted: Boolean; cdecl;
    procedure setMuted(muted: Boolean); cdecl;
  end;
  TCXSetMutedCallAction = class(TOCGenericImport<CXSetMutedCallActionClass, CXSetMutedCallAction>) end;

  CXSetGroupCallActionClass = interface(CXCallActionClass)
    ['{7BEA7EBD-0E88-42DA-8F49-37F7BA7C5282}']
  end;

  CXSetGroupCallAction = interface(CXCallAction)
    ['{950121EB-281C-4C68-BEA6-8B650D285ECD}']
    function callUUIDToGroupWith: NSUUID; cdecl;
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    [MethodName('initWithCallUUID:callUUIDToGroupWith:')]
    function initWithCallUUID(callUUID: NSUUID; callUUIDToGroupWith: NSUUID): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    procedure setCallUUIDToGroupWith(callUUIDToGroupWith: NSUUID); cdecl;
  end;
  TCXSetGroupCallAction = class(TOCGenericImport<CXSetGroupCallActionClass, CXSetGroupCallAction>) end;

  CXPlayDTMFCallActionClass = interface(CXCallActionClass)
    ['{F1CFE59F-8145-4AF7-9D90-D99DD123444A}']
  end;

  CXPlayDTMFCallAction = interface(CXCallAction)
    ['{8B144D1E-BE04-4BC8-A549-ED2F42210850}']
    function &type: CXPlayDTMFCallActionType; cdecl;
    function digits: NSString; cdecl;
    function initWithCallUUID(callUUID: NSUUID): Pointer; overload; cdecl;
    [MethodName('initWithCallUUID:digits:type:')]
    function initWithCallUUID(callUUID: NSUUID; digits: NSString; &type: CXPlayDTMFCallActionType): Pointer; overload; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    procedure setDigits(digits: NSString); cdecl;
    procedure setType(&type: CXPlayDTMFCallActionType); cdecl;
  end;
  TCXPlayDTMFCallAction = class(TOCGenericImport<CXPlayDTMFCallActionClass, CXPlayDTMFCallAction>) end;

  CXProviderDelegate = interface(IObjectiveC)
    ['{6807D9E8-1E00-4A14-AF94-159CBB4D1ED1}']
    [MethodName('provider:didActivateAudioSession:')]
    procedure providerDidActivateAudioSession(provider: CXProvider; audioSession: AVAudioSession); cdecl;
    procedure providerDidBegin(provider: CXProvider); cdecl;
    [MethodName('provider:didDeactivateAudioSession:')]
    procedure providerDidDeactivateAudioSession(provider: CXProvider; audioSession: AVAudioSession); cdecl;
    procedure providerDidReset(provider: CXProvider); cdecl;
    [MethodName('provider:executeTransaction:')]
    function providerExecuteTransaction(provider: CXProvider; transaction: CXTransaction): Boolean; cdecl;
    [MethodName('provider:performAnswerCallAction:')]
    procedure providerPerformAnswerCallAction(provider: CXProvider; action: CXAnswerCallAction); cdecl;
    [MethodName('provider:performEndCallAction:')]
    procedure providerPerformEndCallAction(provider: CXProvider; action: CXEndCallAction); cdecl;
    [MethodName('provider:performPlayDTMFCallAction:')]
    procedure providerPerformPlayDTMFCallAction(provider: CXProvider; action: CXPlayDTMFCallAction); cdecl;
    [MethodName('provider:performSetGroupCallAction:')]
    procedure providerPerformSetGroupCallAction(provider: CXProvider; action: CXSetGroupCallAction); cdecl;
    [MethodName('provider:performSetHeldCallAction:')]
    procedure providerPerformSetHeldCallAction(provider: CXProvider; action: CXSetHeldCallAction); cdecl;
    [MethodName('provider:performSetMutedCallAction:')]
    procedure providerPerformSetMutedCallAction(provider: CXProvider; action: CXSetMutedCallAction); cdecl;
    [MethodName('provider:performStartCallAction:')]
    procedure providerPerformStartCallAction(provider: CXProvider; action: CXStartCallAction); cdecl;
    [MethodName('provider:timedOutPerformingAction:')]
    procedure providerTimedOutPerformingAction(provider: CXProvider; action: CXAction); cdecl;
  end;

  CXProviderClass = interface(NSObjectClass)
    ['{7BF8DF68-25EF-4A4F-8CDE-913FE27D41D4}']
  end;

  CXProvider = interface(NSObject)
    ['{484DE01D-A986-4541-8E65-F7BA440A0FBB}']
    function configuration: CXProviderConfiguration; cdecl;
    function initWithConfiguration(configuration: CXProviderConfiguration): Pointer; cdecl;
    procedure invalidate; cdecl;
    [MethodName('pendingCallActionsOfClass:withCallUUID:')]
    function pendingCallActionsOfClass(callActionClass: Pointer; callUUID: NSUUID): NSArray; cdecl;
    function pendingTransactions: NSArray; cdecl;
    [MethodName('reportCallWithUUID:endedAtDate:reason:')]
    procedure reportCallWithUUID(UUID: NSUUID; dateEnded: NSDate; endedReason: CXCallEndedReason); overload; cdecl;
    [MethodName('reportCallWithUUID:updated:')]
    procedure reportCallWithUUID(UUID: NSUUID; update: CXCallUpdate); overload; cdecl;
    [MethodName('reportNewIncomingCallWithUUID:update:completion:')]
    procedure reportNewIncomingCallWithUUID(UUID: NSUUID; update: CXCallUpdate; completion: TCXProviderBlockMethod1); cdecl;
    [MethodName('reportOutgoingCallWithUUID:connectedAtDate:')]
    procedure reportOutgoingCallWithUUIDConnectedAtDate(UUID: NSUUID; dateConnected: NSDate); cdecl;
    [MethodName('reportOutgoingCallWithUUID:startedConnectingAtDate:')]
    procedure reportOutgoingCallWithUUIDStartedConnectingAtDate(UUID: NSUUID; dateStartedConnecting: NSDate); cdecl;
    procedure setConfiguration(configuration: CXProviderConfiguration); cdecl;
    [MethodName('setDelegate:queue:')]
    procedure setDelegate(delegate: Pointer; queue: dispatch_queue_t); cdecl;
  end;
  TCXProvider = class(TOCGenericImport<CXProviderClass, CXProvider>) end;

  CXProviderConfigurationClass = interface(NSObjectClass)
    ['{99BB3897-A3E2-4D31-BD59-096400D5D6F4}']
  end;

  CXProviderConfiguration = interface(NSObject)
    ['{39D4B0DE-5B53-4CF2-9C57-240516DD6391}']
    function iconTemplateImageData: NSData; cdecl;
    function includesCallsInRecents: Boolean; cdecl;
    function initWithLocalizedName(localizedName: NSString): Pointer; cdecl;
    function localizedName: NSString; cdecl;
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
    ['{35901EB7-6436-4F0E-8DAF-C5DE81140DE1}']
  end;

  CXCall = interface(NSObject)
    ['{5D8DD558-129F-48FA-BDD1-BFB8289DD875}']
    function hasConnected: Boolean; cdecl;
    function hasEnded: Boolean; cdecl;
    function isEqualToCall(call: CXCall): Boolean; cdecl;
    function isOnHold: Boolean; cdecl;
    function isOutgoing: Boolean; cdecl;
    function UUID: NSUUID; cdecl;
  end;
  TCXCall = class(TOCGenericImport<CXCallClass, CXCall>) end;

  CXCallObserverDelegate = interface(IObjectiveC)
    ['{26587E79-C708-4EA0-8232-23C1152D3D0D}']
    [MethodName('callObserver:callChanged:')]
    procedure callObserverCallChanged(callObserver: CXCallObserver; call: CXCall); cdecl;
  end;

  CXCallObserverClass = interface(NSObjectClass)
    ['{4D7F0CC6-1872-4641-81B8-409AFCEF2306}']
  end;

  CXCallObserver = interface(NSObject)
    ['{00FC5D81-86A1-452D-B9ED-AFA4C59D9316}']
    function calls: NSArray; cdecl;
    [MethodName('setDelegate:queue:')]
    procedure setDelegate(delegate: Pointer; queue: dispatch_queue_t); cdecl;
  end;
  TCXCallObserver = class(TOCGenericImport<CXCallObserverClass, CXCallObserver>) end;

  CXCallControllerClass = interface(NSObjectClass)
    ['{AA8C91AF-348F-4EAD-B34D-B8E73490CBD7}']
  end;

  CXCallController = interface(NSObject)
    ['{6CDB9032-EAD6-48F4-9D27-53B484AAB8C0}']
    function callObserver: CXCallObserver; cdecl;
    function initWithQueue(queue: dispatch_queue_t): Pointer; cdecl;
    [MethodName('requestTransaction:completion:')]
    procedure requestTransaction(transaction: CXTransaction; completion: TCXCallControllerBlockMethod1); cdecl;
    [MethodName('requestTransactionWithAction:completion:')]
    procedure requestTransactionWithAction(action: CXAction; completion: TCXCallControllerBlockMethod1); cdecl;
    [MethodName('requestTransactionWithActions:completion:')]
    procedure requestTransactionWithActions(actions: NSArray; completion: TCXCallControllerBlockMethod1); cdecl;
  end;
  TCXCallController = class(TOCGenericImport<CXCallControllerClass, CXCallController>) end;

  CXCallDirectoryManagerClass = interface(NSObjectClass)
    ['{8D8B3FB0-6AB8-4FE3-883B-DB12A4ECA4CF}']
    {class} function sharedInstance: CXCallDirectoryManager; cdecl;
  end;

  CXCallDirectoryManager = interface(NSObject)
    ['{8D15F50E-B44C-460A-BD5C-1786750E14FB}']
    [MethodName('getEnabledStatusForExtensionWithIdentifier:completionHandler:')]
    procedure getEnabledStatusForExtensionWithIdentifier(identifier: NSString; completion: TCXCallDirectoryManagerBlockMethod2); cdecl;
    [MethodName('reloadExtensionWithIdentifier:completionHandler:')]
    procedure reloadExtensionWithIdentifier(identifier: NSString; completion: TCXCallDirectoryManagerBlockMethod1); cdecl;
  end;
  TCXCallDirectoryManager = class(TOCGenericImport<CXCallDirectoryManagerClass, CXCallDirectoryManager>) end;

  CXCallDirectoryProviderClass = interface(NSObjectClass)
    ['{A830B6EB-E16E-44D5-A29E-44BCF5F30A4A}']
  end;

  CXCallDirectoryProvider = interface(NSObject)
    ['{14144AEF-DAC0-4D3A-99D8-1DB419BB8A26}']
    procedure beginRequestWithExtensionContext(context: CXCallDirectoryExtensionContext); cdecl;
  end;
  TCXCallDirectoryProvider = class(TOCGenericImport<CXCallDirectoryProviderClass, CXCallDirectoryProvider>) end;

  CXCallDirectoryExtensionContextDelegate = interface(IObjectiveC)
    ['{3C0C1015-A050-45ED-8041-32ADD79B13E8}']
    [MethodName('requestFailedForExtensionContext:withError:')]
    procedure requestFailedForExtensionContext(extensionContext: CXCallDirectoryExtensionContext; error: NSError); cdecl;
  end;

  CXCallDirectoryExtensionContextClass = interface(NSExtensionContextClass)
    ['{B8A10D37-089D-46CF-AA45-834100DCED51}']
  end;

  CXCallDirectoryExtensionContext = interface(NSExtensionContext)
    ['{3D06F744-421D-4C3F-B045-EC142E7C9559}']
    procedure addBlockingEntryWithNextSequentialPhoneNumber(phoneNumber: CXCallDirectoryPhoneNumber); cdecl;
    [MethodName('addIdentificationEntryWithNextSequentialPhoneNumber:label:')]
    procedure addIdentificationEntryWithNextSequentialPhoneNumber(phoneNumber: CXCallDirectoryPhoneNumber; &label: NSString); cdecl;
    [MethodName('completeRequestReturningItems:completionHandler:')]
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

const
  libCallKit = '/System/Library/Frameworks/CallKit.framework/CallKit';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  CallKitModule: THandle;
{$ENDIF}

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

procedure CallKitLoader; cdecl; external libCallKit;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  CallKitModule := dlopen(MarshaledAString(libCallKit), RTLD_LAZY);

finalization
  dlclose(CallKitModule)
{$ENDIF}

end.