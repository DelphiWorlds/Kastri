unit DW.iOSapi.CloudKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.ObjCRuntime,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.Foundation;

const
  CKReferenceActionNone = 0;
  CKReferenceActionDeleteSelf = 1;
  CKSubscriptionTypeQuery = 1;
  CKSubscriptionTypeRecordZone = 2;
  CKSubscriptionTypeDatabase = 3;
  CKQuerySubscriptionOptionsFiresOnRecordCreation = 1;
  CKQuerySubscriptionOptionsFiresOnRecordUpdate = 2;
  CKQuerySubscriptionOptionsFiresOnRecordDeletion = 4;
  CKQuerySubscriptionOptionsFiresOnce = 8;
  CKDatabaseScopePublic = 1;
  CKDatabaseScopePrivate = 2;
  CKDatabaseScopeShared = 3;
  CKAccountStatusCouldNotDetermine = 0;
  CKAccountStatusAvailable = 1;
  CKAccountStatusRestricted = 2;
  CKAccountStatusNoAccount = 3;
  CKAccountStatusTemporarilyUnavailable = 4;
  CKApplicationPermissionUserDiscoverability = 1;
  CKApplicationPermissionStatusInitialState = 0;
  CKApplicationPermissionStatusCouldNotComplete = 1;
  CKApplicationPermissionStatusDenied = 2;
  CKApplicationPermissionStatusGranted = 3;
  CKErrorInternalError = 1;
  CKErrorPartialFailure = 2;
  CKErrorNetworkUnavailable = 3;
  CKErrorNetworkFailure = 4;
  CKErrorBadContainer = 5;
  CKErrorServiceUnavailable = 6;
  CKErrorRequestRateLimited = 7;
  CKErrorMissingEntitlement = 8;
  CKErrorNotAuthenticated = 9;
  CKErrorPermissionFailure = 10;
  CKErrorUnknownItem = 11;
  CKErrorInvalidArguments = 12;
  CKErrorResultsTruncated = 13;
  CKErrorServerRecordChanged = 14;
  CKErrorServerRejectedRequest = 15;
  CKErrorAssetFileNotFound = 16;
  CKErrorAssetFileModified = 17;
  CKErrorIncompatibleVersion = 18;
  CKErrorConstraintViolation = 19;
  CKErrorOperationCancelled = 20;
  CKErrorChangeTokenExpired = 21;
  CKErrorBatchRequestFailed = 22;
  CKErrorZoneBusy = 23;
  CKErrorBadDatabase = 24;
  CKErrorQuotaExceeded = 25;
  CKErrorZoneNotFound = 26;
  CKErrorLimitExceeded = 27;
  CKErrorUserDeletedZone = 28;
  CKErrorTooManyParticipants = 29;
  CKErrorAlreadyShared = 30;
  CKErrorReferenceViolation = 31;
  CKErrorManagedAccountRestricted = 32;
  CKErrorParticipantMayNeedVerification = 33;
  CKErrorServerResponseLost = 34;
  CKErrorAssetNotAvailable = 35;
  CKErrorAccountTemporarilyUnavailable = 36;
  CKNotificationTypeQuery = 1;
  CKNotificationTypeRecordZone = 2;
  CKNotificationTypeReadNotification = 3;
  CKNotificationTypeDatabase = 4;
  CKQueryNotificationReasonRecordCreated = 1;
  CKQueryNotificationReasonRecordUpdated = 2;
  CKQueryNotificationReasonRecordDeleted = 3;
  CKRecordZoneCapabilityFetchChanges = 1;
  CKRecordZoneCapabilityAtomic = 2;
  CKRecordZoneCapabilitySharing = 4;
  CKRecordZoneCapabilityZoneWideSharing = 8;
  CKShareParticipantAcceptanceStatusUnknown = 0;
  CKShareParticipantAcceptanceStatusPending = 1;
  CKShareParticipantAcceptanceStatusAccepted = 2;
  CKShareParticipantAcceptanceStatusRemoved = 3;
  CKShareParticipantPermissionUnknown = 0;
  CKShareParticipantPermissionNone = 1;
  CKShareParticipantPermissionReadOnly = 2;
  CKShareParticipantPermissionReadWrite = 3;
  CKShareParticipantRoleUnknown = 0;
  CKShareParticipantRoleOwner = 1;
  CKShareParticipantRolePrivateUser = 3;
  CKShareParticipantRolePublicUser = 4;
  CKShareParticipantTypeUnknown = 0;
  CKShareParticipantTypeOwner = 1;
  CKShareParticipantTypePrivateUser = 3;
  CKShareParticipantTypePublicUser = 4;
  CKRecordSaveIfServerRecordUnchanged = 0;
  CKRecordSaveChangedKeys = 1;
  CKRecordSaveAllKeys = 2;
  CKOperationGroupTransferSizeUnknown = 0;
  CKOperationGroupTransferSizeKilobytes = 1;
  CKOperationGroupTransferSizeMegabytes = 2;
  CKOperationGroupTransferSizeTensOfMegabytes = 3;
  CKOperationGroupTransferSizeHundredsOfMegabytes = 4;
  CKOperationGroupTransferSizeGigabytes = 5;
  CKOperationGroupTransferSizeTensOfGigabytes = 6;
  CKOperationGroupTransferSizeHundredsOfGigabytes = 7;
  CKSharingParticipantAccessOptionAnyoneWithLink = 1;
  CKSharingParticipantAccessOptionSpecifiedRecipientsOnly = 2;
  CKSharingParticipantAccessOptionAny = 3;
  CKSharingParticipantPermissionOptionReadOnly = 1;
  CKSharingParticipantPermissionOptionReadWrite = 2;
  CKSharingParticipantPermissionOptionAny = 3;
  CKSyncEnginePendingRecordZoneChangeTypeSaveRecord = 0;
  CKSyncEnginePendingRecordZoneChangeTypeDeleteRecord = 1;
  CKSyncEnginePendingDatabaseChangeTypeSaveZone = 0;
  CKSyncEnginePendingDatabaseChangeTypeDeleteZone = 1;
  CKSyncEngineSyncReasonScheduled = 0;
  CKSyncEngineSyncReasonManual = 1;
  CKSyncEngineEventTypeStateUpdate = 0;
  CKSyncEngineEventTypeAccountChange = 1;
  CKSyncEngineEventTypeFetchedDatabaseChanges = 2;
  CKSyncEngineEventTypeFetchedRecordZoneChanges = 3;
  CKSyncEngineEventTypeSentDatabaseChanges = 4;
  CKSyncEngineEventTypeSentRecordZoneChanges = 5;
  CKSyncEngineEventTypeWillFetchChanges = 6;
  CKSyncEngineEventTypeWillFetchRecordZoneChanges = 7;
  CKSyncEngineEventTypeDidFetchRecordZoneChanges = 8;
  CKSyncEngineEventTypeDidFetchChanges = 9;
  CKSyncEngineEventTypeWillSendChanges = 10;
  CKSyncEngineEventTypeDidSendChanges = 11;
  CKSyncEngineAccountChangeTypeSignIn = 0;
  CKSyncEngineAccountChangeTypeSignOut = 1;
  CKSyncEngineAccountChangeTypeSwitchAccounts = 2;
  CKSyncEngineZoneDeletionReasonDeleted = 0;
  CKSyncEngineZoneDeletionReasonPurged = 1;
  CKSyncEngineZoneDeletionReasonEncryptedDataReset = 2;

type
  CKContainer = interface;
  CKDatabase = interface;
  CKDatabaseOperation = interface;
  CKNotificationInfo = interface;
  CKOperation = interface;
  CKOperationConfiguration = interface;
  CKOperationGroup = interface;
  CKQuery = interface;
  CKRecord = interface;
  CKRecordID = interface;
  CKRecordZone = interface;
  CKRecordZoneID = interface;
  CKReference = interface;
  CKShare = interface;
  CKShareMetadata = interface;
  CKShareParticipant = interface;
  CKSubscription = interface;
  CKUserIdentity = interface;
  CKUserIdentityLookupInfo = interface;

  CKAccountStatus = NSInteger;
  CKOperationID = NSString;
  CKOperationGroupTransferSize = NSInteger;
  CKShareParticipantType = NSInteger;
  CKShareParticipantAcceptanceStatus = NSInteger;
  CKShareParticipantPermission = NSInteger;
  CKShareParticipantRole = NSInteger;
  CKDatabaseScope = NSInteger;
  CKSubscriptionID = NSString;
  CKSubscriptionType = NSInteger;
  CKRecordZoneCapabilities = NSInteger;
  CKRecordType = NSString;
  CKReferenceAction = NSInteger;
  CKRecordFieldKey = NSString;
  CKApplicationPermissions = NSInteger;
  CKApplicationPermissionStatus = NSInteger;
  CKQuerySubscriptionOptions = NSInteger;

  CKErrorCode = NSInteger;
  CKNotificationType = NSInteger;
  CKQueryNotificationReason = NSInteger;
  CKRecordSavePolicy = NSInteger;

  CKApplicationPermissionBlock = procedure(applicationPermissionStatus: CKApplicationPermissionStatus; error: NSError) of object;

  TCKContainerBlockMethod1 = procedure(accountStatus: CKAccountStatus; error: NSError) of object;
  TCKContainerBlockMethod2 = procedure(recordID: CKRecordID; error: NSError) of object;
  TCKContainerBlockMethod3 = procedure(userIdentities: NSArray; error: NSError) of object;
  TCKContainerBlockMethod4 = procedure(userInfo: CKUserIdentity; error: NSError) of object;
  TCKContainerBlockMethod5 = procedure(shareParticipant: CKShareParticipant; error: NSError) of object;
  TCKContainerBlockMethod6 = procedure(metadata: CKShareMetadata; error: NSError) of object;
  TCKContainerBlockMethod7 = procedure(acceptedShare: CKShare; error: NSError) of object;
  TCKContainerBlockMethod8 = procedure(outstandingOperationIDs: NSArray; error: NSError) of object;
  TCKContainerBlockMethod9 = procedure(outstandingOperation: CKOperation; error: NSError) of object;
  TCKOperationBlockMethod1 = procedure of object;
  TCKDatabaseBlockMethod1 = procedure(&record: CKRecord; error: NSError) of object;
  TCKDatabaseBlockMethod2 = procedure(recordID: CKRecordID; error: NSError) of object;
  TCKDatabaseBlockMethod3 = procedure(results: NSArray; error: NSError) of object;
  TCKDatabaseBlockMethod4 = procedure(zones: NSArray; error: NSError) of object;
  TCKDatabaseBlockMethod5 = procedure(zone: CKRecordZone; error: NSError) of object;
  TCKDatabaseBlockMethod6 = procedure(zoneID: CKRecordZoneID; error: NSError) of object;
  TCKDatabaseBlockMethod7 = procedure(subscription: CKSubscription; error: NSError) of object;
  TCKDatabaseBlockMethod8 = procedure(subscriptions: NSArray; error: NSError) of object;
  TCKDatabaseBlockMethod9 = procedure(subscriptionID: CKSubscriptionID; error: NSError) of object;

  CKReferenceClass = interface(NSObjectClass)
    ['{CF422A25-48DF-440D-A5DD-74499338A5AB}']
    {class} function new: Pointer; cdecl;
  end;

  CKReference = interface(NSObject)
    ['{6CE5A757-6BB3-4170-9D2E-EF90E7FCF9BE}']
    function initWithRecord(&record: CKRecord; action: CKReferenceAction): Pointer; cdecl;
    function initWithRecordID(recordID: CKRecordID; action: CKReferenceAction): Pointer; cdecl;
    function recordID: CKRecordID; cdecl;
    function referenceAction: CKReferenceAction; cdecl;
  end;
  TCKReference = class(TOCGenericImport<CKReferenceClass, CKReference>) end;

  CKNotificationInfoClass = interface(NSObjectClass)
    ['{559BCDBE-EBCA-4A73-8EFE-63CE2816386B}']
  end;

  CKNotificationInfo = interface(NSObject)
    ['{0561ADF2-1BF7-479E-8B6F-901FA50856DC}']
    function alertActionLocalizationKey: NSString; cdecl;
    function alertBody: NSString; cdecl;
    function alertLaunchImage: NSString; cdecl;
    function alertLocalizationArgs: NSArray; cdecl;
    function alertLocalizationKey: NSString; cdecl;
    function category: NSString; cdecl;
    function collapseIDKey: NSString; cdecl;
    function desiredKeys: NSArray; cdecl;
    procedure setAlertActionLocalizationKey(alertActionLocalizationKey: NSString); cdecl;
    procedure setAlertBody(alertBody: NSString); cdecl;
    procedure setAlertLaunchImage(alertLaunchImage: NSString); cdecl;
    procedure setAlertLocalizationArgs(alertLocalizationArgs: NSArray); cdecl;
    procedure setAlertLocalizationKey(alertLocalizationKey: NSString); cdecl;
    procedure setCategory(category: NSString); cdecl;
    procedure setCollapseIDKey(collapseIDKey: NSString); cdecl;
    procedure setDesiredKeys(desiredKeys: NSArray); cdecl;
    procedure setShouldBadge(shouldBadge: Boolean); cdecl;
    procedure setShouldSendContentAvailable(shouldSendContentAvailable: Boolean); cdecl;
    procedure setShouldSendMutableContent(shouldSendMutableContent: Boolean); cdecl;
    procedure setSoundName(soundName: NSString); cdecl;
    procedure setSubtitle(subtitle: NSString); cdecl;
    procedure setSubtitleLocalizationArgs(subtitleLocalizationArgs: NSArray); cdecl;
    procedure setSubtitleLocalizationKey(subtitleLocalizationKey: NSString); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setTitleLocalizationArgs(titleLocalizationArgs: NSArray); cdecl;
    procedure setTitleLocalizationKey(titleLocalizationKey: NSString); cdecl;
    function shouldBadge: Boolean; cdecl;
    function shouldSendContentAvailable: Boolean; cdecl;
    function shouldSendMutableContent: Boolean; cdecl;
    function soundName: NSString; cdecl;
    function subtitle: NSString; cdecl;
    function subtitleLocalizationArgs: NSArray; cdecl;
    function subtitleLocalizationKey: NSString; cdecl;
    function title: NSString; cdecl;
    function titleLocalizationArgs: NSArray; cdecl;
    function titleLocalizationKey: NSString; cdecl;
  end;
  TCKNotificationInfo = class(TOCGenericImport<CKNotificationInfoClass, CKNotificationInfo>) end;

  CKSubscriptionClass = interface(NSObjectClass)
    ['{1B1BB59A-915D-4C74-A6AA-E85662CCD44B}']
    {class} function new: Pointer; cdecl;
  end;

  CKSubscription = interface(NSObject)
    ['{EFA32B86-1BD5-4C2C-A01E-253D01BACE67}']
    function notificationInfo: CKNotificationInfo; cdecl;
    procedure setNotificationInfo(notificationInfo: CKNotificationInfo); cdecl;
    function subscriptionID: CKSubscriptionID; cdecl;
    function subscriptionType: CKSubscriptionType; cdecl;
  end;
  TCKSubscription = class(TOCGenericImport<CKSubscriptionClass, CKSubscription>) end;

  CKRecordZoneClass = interface(NSObjectClass)
    ['{64A6FB84-E96E-46B6-8940-6FF4722C0FD6}']
    {class} function defaultRecordZone: CKRecordZone; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CKRecordZone = interface(NSObject)
    ['{B7D3ECAD-6695-477F-BE02-79CA2361286E}']
    function capabilities: CKRecordZoneCapabilities; cdecl;
    function initWithZoneID(zoneID: CKRecordZoneID): Pointer; cdecl;
    function initWithZoneName(zoneName: NSString): Pointer; cdecl;
    function share: CKReference; cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKRecordZone = class(TOCGenericImport<CKRecordZoneClass, CKRecordZone>) end;

  CKRecordZoneIDClass = interface(NSObjectClass)
    ['{9D29DF6F-A87C-4276-A097-F1BF3A703E90}']
    {class} function new: Pointer; cdecl;
  end;

  CKRecordZoneID = interface(NSObject)
    ['{C94752EE-16BE-4B81-9EA9-1235BB563936}']
    function initWithZoneName(zoneName: NSString; ownerName: NSString): Pointer; cdecl;
    function ownerName: NSString; cdecl;
    function zoneName: NSString; cdecl;
  end;
  TCKRecordZoneID = class(TOCGenericImport<CKRecordZoneIDClass, CKRecordZoneID>) end;

  CKRecordIDClass = interface(NSObjectClass)
    ['{BBC618B4-2118-4595-9085-95EB2F83DA43}']
    {class} function new: Pointer; cdecl;
  end;

  CKRecordID = interface(NSObject)
    ['{7477144D-9FF2-48E3-B072-7007D1C63840}']
    function initWithRecordName(recordName: NSString; zoneID: CKRecordZoneID): Pointer; overload; cdecl;
    function initWithRecordName(recordName: NSString): Pointer; overload; cdecl;
    function recordName: NSString; cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKRecordID = class(TOCGenericImport<CKRecordIDClass, CKRecordID>) end;

  CKRecordClass = interface(NSObjectClass)
    ['{63F6B8C5-8D43-4F3E-A70C-5F1F6E2E1721}']
    {class} function new: Pointer; cdecl;
  end;

  CKRecord = interface(NSObject)
    ['{A7F881AB-7300-489E-A325-68F2037E72A7}']
    function allKeys: NSArray; cdecl;
    function allTokens: NSArray; cdecl;
    function changedKeys: NSArray; cdecl;
    function creationDate: NSDate; cdecl;
    function creatorUserRecordID: CKRecordID; cdecl;
    procedure encodeSystemFieldsWithCoder(coder: NSCoder); cdecl;
    function encryptedValues: Pointer; cdecl;
    function initWithRecordType(recordType: CKRecordType; zoneID: CKRecordZoneID): Pointer; overload; cdecl;
    function initWithRecordType(recordType: CKRecordType; recordID: CKRecordID): Pointer; overload; cdecl;
    function initWithRecordType(recordType: CKRecordType): Pointer; overload; cdecl;
    function lastModifiedUserRecordID: CKRecordID; cdecl;
    function modificationDate: NSDate; cdecl;
    function objectForKey(key: CKRecordFieldKey): id; cdecl;
    function objectForKeyedSubscript(key: CKRecordFieldKey): id; cdecl;
    function parent: CKReference; cdecl;
    function recordChangeTag: NSString; cdecl;
    function recordID: CKRecordID; cdecl;
    function recordType: CKRecordType; cdecl;
    procedure setObject(&object: id; forKey: CKRecordFieldKey); cdecl;
    [MethodName('setObject:forKeyedSubscript:')]
    procedure setObjectForKeyedSubscript(&object: id; forKeyedSubscript: CKRecordFieldKey); cdecl;
    procedure setParent(parent: CKReference); cdecl;
    procedure setParentReferenceFromRecord(parentRecord: CKRecord); cdecl;
    procedure setParentReferenceFromRecordID(parentRecordID: CKRecordID); cdecl;
    function share: CKReference; cdecl;
  end;
  TCKRecord = class(TOCGenericImport<CKRecordClass, CKRecord>) end;

  CKOperationClass = interface(NSOperationClass)
    ['{A7EBB6EE-C72F-401F-A541-83A11C31B27D}']
  end;

  CKOperation = interface(NSOperation)
    ['{002B14EB-DB0C-4CA9-8DF6-AC431F87F4E5}']
    function allowsCellularAccess: Boolean; cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.10, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    function configuration: CKOperationConfiguration; cdecl;
    function container: CKContainer; cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.10, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    function group: CKOperationGroup; cdecl;
    function isLongLived: Boolean; cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.12, 10.13), ios(9.3, 11.0), tvos(9.2, 11.0), watchos(3.0, 4.0))
    function longLivedOperationWasPersistedBlock: TCKOperationBlockMethod1; cdecl;
    function operationID: CKOperationID; cdecl;
    procedure setAllowsCellularAccess(allowsCellularAccess: Boolean); cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.10, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setConfiguration(configuration: CKOperationConfiguration); cdecl;
    procedure setContainer(container: CKContainer); cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.10, 10.13), ios(8.0, 11.0), tvos(9.0, 11.0), watchos(3.0, 4.0))
    procedure setGroup(group: CKOperationGroup); cdecl;
    procedure setLongLived(longLived: Boolean); cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.12, 10.13), ios(9.3, 11.0), tvos(9.2, 11.0), watchos(3.0, 4.0))
    procedure setLongLivedOperationWasPersistedBlock(longLivedOperationWasPersistedBlock: TCKOperationBlockMethod1); cdecl;
    procedure setTimeoutIntervalForRequest(timeoutIntervalForRequest: NSTimeInterval); cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.12, 10.13), ios(10.0, 11.0), tvos(10.0, 11.0), watchos(3.0, 4.0))
    procedure setTimeoutIntervalForResource(timeoutIntervalForResource: NSTimeInterval); cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.12, 10.13), ios(10.0, 11.0), tvos(10.0, 11.0), watchos(3.0, 4.0))
    function timeoutIntervalForRequest: NSTimeInterval; cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.12, 10.13), ios(10.0, 11.0), tvos(10.0, 11.0), watchos(3.0, 4.0))
    function timeoutIntervalForResource: NSTimeInterval; cdecl; // API_DEPRECATED("Use CKOperationConfiguration", macos(10.12, 10.13), ios(10.0, 11.0), tvos(10.0, 11.0), watchos(3.0, 4.0))
  end;
  TCKOperation = class(TOCGenericImport<CKOperationClass, CKOperation>) end;

  CKDatabaseOperationClass = interface(CKOperationClass)
    ['{B77E94E6-E07A-4449-90FD-46CBD4EC001A}']
  end;

  CKDatabaseOperation = interface(CKOperation)
    ['{0B0418F3-9B5D-4286-A305-0C0FDF240D1C}']
    function database: CKDatabase; cdecl;
    procedure setDatabase(database: CKDatabase); cdecl;
  end;
  TCKDatabaseOperation = class(TOCGenericImport<CKDatabaseOperationClass, CKDatabaseOperation>) end;

  CKQueryClass = interface(NSObjectClass)
    ['{0EFB5FEB-2BB8-4E12-A987-3077E07606E3}']
    {class} function new: Pointer; cdecl;
  end;

  CKQuery = interface(NSObject)
    ['{EB55C6B3-E4DC-4F9C-A06E-41DEE3E022B6}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithRecordType(recordType: CKRecordType; predicate: NSPredicate): Pointer; cdecl;
    function predicate: NSPredicate; cdecl;
    function recordType: CKRecordType; cdecl;
    procedure setSortDescriptors(sortDescriptors: NSArray); cdecl;
    function sortDescriptors: NSArray; cdecl;
  end;
  TCKQuery = class(TOCGenericImport<CKQueryClass, CKQuery>) end;

  CKDatabaseClass = interface(NSObjectClass)
    ['{2844347D-EF86-4443-AC13-345BB7927958}']
    {class} function new: Pointer; cdecl;
  end;

  CKDatabase = interface(NSObject)
    ['{944C9827-9BA9-4A0B-B37E-7F3E0D0C373C}']
    procedure addOperation(operation: CKDatabaseOperation); cdecl;
    function databaseScope: CKDatabaseScope; cdecl;
    procedure deleteRecordWithID(recordID: CKRecordID; completionHandler: TCKDatabaseBlockMethod2); cdecl;
    procedure deleteRecordZoneWithID(zoneID: CKRecordZoneID; completionHandler: TCKDatabaseBlockMethod6); cdecl;
    procedure deleteSubscriptionWithID(subscriptionID: CKSubscriptionID; completionHandler: TCKDatabaseBlockMethod9); cdecl;
    procedure fetchAllRecordZonesWithCompletionHandler(completionHandler: TCKDatabaseBlockMethod4); cdecl;
    procedure fetchAllSubscriptionsWithCompletionHandler(completionHandler: TCKDatabaseBlockMethod8); cdecl;
    procedure fetchRecordWithID(recordID: CKRecordID; completionHandler: TCKDatabaseBlockMethod1); cdecl;
    procedure fetchRecordZoneWithID(zoneID: CKRecordZoneID; completionHandler: TCKDatabaseBlockMethod5); cdecl;
    procedure fetchSubscriptionWithID(subscriptionID: CKSubscriptionID; completionHandler: TCKDatabaseBlockMethod7); cdecl;
    procedure performQuery(query: CKQuery; inZoneWithID: CKRecordZoneID; completionHandler: TCKDatabaseBlockMethod3); cdecl;
    procedure saveRecord(&record: CKRecord; completionHandler: TCKDatabaseBlockMethod1); cdecl;
    procedure saveRecordZone(zone: CKRecordZone; completionHandler: TCKDatabaseBlockMethod5); cdecl;
    procedure saveSubscription(subscription: CKSubscription; completionHandler: TCKDatabaseBlockMethod7); cdecl;
  end;
  TCKDatabase = class(TOCGenericImport<CKDatabaseClass, CKDatabase>) end;

  CKUserIdentityLookupInfoClass = interface(NSObjectClass)
    ['{53C50FC6-8284-4A93-A595-B63F15CF1757}']
    {class} function lookupInfosWithEmails(emails: NSArray): NSArray; cdecl;
    {class} function lookupInfosWithPhoneNumbers(phoneNumbers: NSArray): NSArray; cdecl;
    {class} function lookupInfosWithRecordIDs(recordIDs: NSArray): NSArray; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CKUserIdentityLookupInfo = interface(NSObject)
    ['{54875297-DC8D-4AD2-8467-5060CEF1A8EA}']
    function emailAddress: NSString; cdecl;
    function initWithEmailAddress(emailAddress: NSString): Pointer; cdecl;
    function initWithPhoneNumber(phoneNumber: NSString): Pointer; cdecl;
    function initWithUserRecordID(userRecordID: CKRecordID): Pointer; cdecl;
    function phoneNumber: NSString; cdecl;
    function userRecordID: CKRecordID; cdecl;
  end;
  TCKUserIdentityLookupInfo = class(TOCGenericImport<CKUserIdentityLookupInfoClass, CKUserIdentityLookupInfo>) end;

  CKUserIdentityClass = interface(NSObjectClass)
    ['{35739C01-9570-4F29-A620-61EF1E395865}']
    {class} function new: Pointer; cdecl;
  end;

  CKUserIdentity = interface(NSObject)
    ['{7E0D0F58-B917-4F52-9AF4-8572D04F5B98}']
    function contactIdentifiers: NSArray; cdecl;
    function hasiCloudAccount: Boolean; cdecl;
    function lookupInfo: CKUserIdentityLookupInfo; cdecl;
    function nameComponents: NSPersonNameComponents; cdecl;
    function userRecordID: CKRecordID; cdecl;
  end;
  TCKUserIdentity = class(TOCGenericImport<CKUserIdentityClass, CKUserIdentity>) end;

  CKOperationConfigurationClass = interface(NSObjectClass)
    ['{80B773A7-C4E7-4C9F-AE47-29A46F0FEC07}']
  end;

  CKOperationConfiguration = interface(NSObject)
    ['{DA5B4D46-32CD-4A0F-A03C-5F5F68B7438C}']
    function allowsCellularAccess: Boolean; cdecl;
    function container: CKContainer; cdecl;
    function isLongLived: Boolean; cdecl;
    function qualityOfService: NSQualityOfService; cdecl;
    procedure setAllowsCellularAccess(allowsCellularAccess: Boolean); cdecl;
    procedure setContainer(container: CKContainer); cdecl;
    procedure setLongLived(longLived: Boolean); cdecl;
    procedure setQualityOfService(qualityOfService: NSQualityOfService); cdecl;
    procedure setTimeoutIntervalForRequest(timeoutIntervalForRequest: NSTimeInterval); cdecl;
    procedure setTimeoutIntervalForResource(timeoutIntervalForResource: NSTimeInterval); cdecl;
    function timeoutIntervalForRequest: NSTimeInterval; cdecl;
    function timeoutIntervalForResource: NSTimeInterval; cdecl;
  end;
  TCKOperationConfiguration = class(TOCGenericImport<CKOperationConfigurationClass, CKOperationConfiguration>) end;

  CKOperationGroupClass = interface(NSObjectClass)
    ['{8415E43D-D3F8-468C-816E-1E27FC216ABC}']
  end;

  CKOperationGroup = interface(NSObject)
    ['{25BBD5B6-BCED-4DCA-881D-B91D84F9AF77}']
    function defaultConfiguration: CKOperationConfiguration; cdecl;
    function expectedReceiveSize: CKOperationGroupTransferSize; cdecl;
    function expectedSendSize: CKOperationGroupTransferSize; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function name: NSString; cdecl;
    function operationGroupID: NSString; cdecl;
    function quantity: NSUInteger; cdecl;
    procedure setDefaultConfiguration(defaultConfiguration: CKOperationConfiguration); cdecl;
    procedure setExpectedReceiveSize(expectedReceiveSize: CKOperationGroupTransferSize); cdecl;
    procedure setExpectedSendSize(expectedSendSize: CKOperationGroupTransferSize); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setQuantity(quantity: NSUInteger); cdecl;
  end;
  TCKOperationGroup = class(TOCGenericImport<CKOperationGroupClass, CKOperationGroup>) end;

  CKShareParticipantClass = interface(NSObjectClass)
    ['{CD44037B-F43F-49E3-A0B7-40ACAA0BDA19}']
    {class} function new: Pointer; cdecl;
  end;

  CKShareParticipant = interface(NSObject)
    ['{0F82AE4E-8EBC-47FE-9232-269AB18A2EC7}']
    function &type: CKShareParticipantType; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("role", macos(10.12, 10.14), ios(10.0, 12.0), tvos(10.0, 12.0), watchos(3.0, 5.0))
    function acceptanceStatus: CKShareParticipantAcceptanceStatus; cdecl;
    function permission: CKShareParticipantPermission; cdecl;
    function role: CKShareParticipantRole; cdecl;
    procedure setPermission(permission: CKShareParticipantPermission); cdecl;
    procedure setRole(role: CKShareParticipantRole); cdecl;
    procedure setType(&type: CKShareParticipantType); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("role", macos(10.12, 10.14), ios(10.0, 12.0), tvos(10.0, 12.0), watchos(3.0, 5.0))
    function userIdentity: CKUserIdentity; cdecl;
  end;
  TCKShareParticipant = class(TOCGenericImport<CKShareParticipantClass, CKShareParticipant>) end;

  CKContainerClass = interface(NSObjectClass)
    ['{35475A51-17BD-4255-B636-0D60327729B9}']
    {class} function containerWithIdentifier(containerIdentifier: NSString): CKContainer; cdecl;
    {class} function defaultContainer: CKContainer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CKContainer = interface(NSObject)
    ['{6AD3E7D0-CE25-4251-B1C4-010B26A00778}']
    procedure acceptShareMetadata(metadata: CKShareMetadata; completionHandler: TCKContainerBlockMethod7); cdecl;
    procedure accountStatusWithCompletionHandler(completionHandler: TCKContainerBlockMethod1); cdecl;
    procedure addOperation(operation: CKOperation); cdecl;
    function containerIdentifier: NSString; cdecl;
    function databaseWithDatabaseScope(databaseScope: CKDatabaseScope): CKDatabase; cdecl;
    procedure discoverAllIdentitiesWithCompletionHandler(completionHandler: TCKContainerBlockMethod3); cdecl; // API_DEPRECATED("No longer supported. Please see Sharing CloudKit Data with Other iCloud Users.", macos(10.12, 14.0), ios(10.0, 17.0), watchos(3.0, 10.0))
    procedure discoverUserIdentityWithEmailAddress(email: NSString; completionHandler: TCKContainerBlockMethod4); cdecl; // API_DEPRECATED("No longer supported. Please see Sharing CloudKit Data with Other iCloud Users.", macos(10.12, 14.0), ios(10.0, 17.0), tvos(10.0, 17.0), watchos(3.0, 10.0))
    procedure discoverUserIdentityWithPhoneNumber(phoneNumber: NSString; completionHandler: TCKContainerBlockMethod4); cdecl; // API_DEPRECATED("No longer supported. Please see Sharing CloudKit Data with Other iCloud Users.", macos(10.12, 14.0), ios(10.0, 17.0), tvos(10.0, 17.0), watchos(3.0, 10.0))
    procedure discoverUserIdentityWithUserRecordID(userRecordID: CKRecordID; completionHandler: TCKContainerBlockMethod4); cdecl; // API_DEPRECATED("No longer supported. Please see Sharing CloudKit Data with Other iCloud Users.", macos(10.12, 14.0), ios(10.0, 17.0), tvos(10.0, 17.0), watchos(3.0, 10.0))
    procedure fetchAllLongLivedOperationIDsWithCompletionHandler(completionHandler: TCKContainerBlockMethod8); cdecl;
    procedure fetchLongLivedOperationWithID(operationID: CKOperationID; completionHandler: TCKContainerBlockMethod9); cdecl;
    procedure fetchShareMetadataWithURL(url: NSURL; completionHandler: TCKContainerBlockMethod6); cdecl;
    procedure fetchShareParticipantWithEmailAddress(emailAddress: NSString; completionHandler: TCKContainerBlockMethod5); cdecl;
    procedure fetchShareParticipantWithPhoneNumber(phoneNumber: NSString; completionHandler: TCKContainerBlockMethod5); cdecl;
    procedure fetchShareParticipantWithUserRecordID(userRecordID: CKRecordID; completionHandler: TCKContainerBlockMethod5); cdecl;
    procedure fetchUserRecordIDWithCompletionHandler(completionHandler: TCKContainerBlockMethod2); cdecl;
    function privateCloudDatabase: CKDatabase; cdecl;
    function publicCloudDatabase: CKDatabase; cdecl;
    procedure requestApplicationPermission(applicationPermission: CKApplicationPermissions; completionHandler: CKApplicationPermissionBlock); cdecl; // API_DEPRECATED("No longer supported. Please see Sharing CloudKit Data with Other iCloud Users.", macos(10.0, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
    function sharedCloudDatabase: CKDatabase; cdecl;
    procedure statusForApplicationPermission(applicationPermission: CKApplicationPermissions; completionHandler: CKApplicationPermissionBlock); cdecl; // API_DEPRECATED("No longer supported. Please see Sharing CloudKit Data with Other iCloud Users.", macos(10.0, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
  end;
  TCKContainer = class(TOCGenericImport<CKContainerClass, CKContainer>) end;

  CKShareClass = interface(CKRecordClass)
    ['{CD05210B-B673-4DA1-B286-5B027E453140}']
    {class} function new: Pointer; cdecl;
  end;

  CKShare = interface(CKRecord)
    ['{035A0A47-7FC6-4BEB-BF2D-6F9334021992}']
    procedure addParticipant(participant: CKShareParticipant); cdecl;
    function currentUserParticipant: CKShareParticipant; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithRecordType(recordType: CKRecordType): Pointer; overload; cdecl;
    function initWithRecordType(recordType: CKRecordType; recordID: CKRecordID): Pointer; overload; cdecl;
    function initWithRecordType(recordType: CKRecordType; zoneID: CKRecordZoneID): Pointer; overload; cdecl;
    function initWithRecordZoneID(recordZoneID: CKRecordZoneID): Pointer; cdecl;
    function initWithRootRecord(rootRecord: CKRecord): Pointer; overload; cdecl;
    function initWithRootRecord(rootRecord: CKRecord; shareID: CKRecordID): Pointer; overload; cdecl;
    function owner: CKShareParticipant; cdecl;
    function participants: NSArray; cdecl;
    function publicPermission: CKShareParticipantPermission; cdecl;
    procedure removeParticipant(participant: CKShareParticipant); cdecl;
    procedure setPublicPermission(publicPermission: CKShareParticipantPermission); cdecl;
    function URL: NSURL; cdecl;
  end;
  TCKShare = class(TOCGenericImport<CKShareClass, CKShare>) end;

  CKShareMetadataClass = interface(NSObjectClass)
    ['{3F0761D8-3F73-405E-997A-4AD48739B3CE}']
    {class} function new: Pointer; cdecl;
  end;

  CKShareMetadata = interface(NSObject)
    ['{B77339B3-E557-43FB-8B84-EF30A7610BB4}']
    function containerIdentifier: NSString; cdecl;
    function hierarchicalRootRecordID: CKRecordID; cdecl;
    function ownerIdentity: CKUserIdentity; cdecl;
    function participantPermission: CKShareParticipantPermission; cdecl;
    function participantRole: CKShareParticipantRole; cdecl;
    function participantStatus: CKShareParticipantAcceptanceStatus; cdecl;
    function participantType: CKShareParticipantType; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("participantRole", macos(10.12, 10.14), ios(10.0, 12.0), tvos(10.0, 12.0), watchos(3.0, 5.0))
    function rootRecord: CKRecord; cdecl;
    function rootRecordID: CKRecordID; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("hierarchicalRootRecordID", macos(10.12, 13.0), ios(10.0, 16.0), tvos(10.0, 16.0), watchos(3.0, 9.0))
    function share: CKShare; cdecl;
  end;
  TCKShareMetadata = class(TOCGenericImport<CKShareMetadataClass, CKShareMetadata>) end;

function CKRecordTypeUserRecord: CKRecordType;
function CKRecordRecordIDKey: CKRecordFieldKey;
function CKRecordCreatorUserRecordIDKey: CKRecordFieldKey;
function CKRecordCreationDateKey: CKRecordFieldKey;
function CKRecordLastModifiedUserRecordIDKey: CKRecordFieldKey;
function CKRecordModificationDateKey: CKRecordFieldKey;
function CKRecordParentKey: CKRecordFieldKey;
function CKRecordShareKey: CKRecordFieldKey;
function CKCurrentUserDefaultName: NSString;
function CKOwnerDefaultName: NSString;
function CKAccountChangedNotification: NSString;
function CKErrorDomain: NSString;
function CKPartialErrorsByItemIDKey: NSString;
function CKRecordChangedErrorAncestorRecordKey: NSString;
function CKRecordChangedErrorServerRecordKey: NSString;
function CKRecordChangedErrorClientRecordKey: NSString;
function CKErrorUserDidResetEncryptedDataKey: NSString;
function CKErrorRetryAfterKey: NSString;
function CKRecordZoneDefaultName: NSString;
function CKRecordTypeShare: CKRecordType;
function CKRecordNameZoneWideShare: NSString;
function CKShareTitleKey: CKRecordFieldKey;
function CKShareThumbnailImageDataKey: CKRecordFieldKey;
function CKShareTypeKey: CKRecordFieldKey;

const
  libCloudKit = '/System/Library/Frameworks/CloudKit.framework/CloudKit';

implementation

uses
  Posix.Dlfcn;

var
  CloudKitModule: THandle;

function CKRecordTypeUserRecord: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordTypeUserRecord');
end;

function CKRecordRecordIDKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordRecordIDKey');
end;

function CKRecordCreatorUserRecordIDKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordCreatorUserRecordIDKey');
end;

function CKRecordCreationDateKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordCreationDateKey');
end;

function CKRecordLastModifiedUserRecordIDKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordLastModifiedUserRecordIDKey');
end;

function CKRecordModificationDateKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordModificationDateKey');
end;

function CKRecordParentKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordParentKey');
end;

function CKRecordShareKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordShareKey');
end;

function CKCurrentUserDefaultName: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKCurrentUserDefaultName');
end;

function CKOwnerDefaultName: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKOwnerDefaultName');
end;

function CKAccountChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKAccountChangedNotification');
end;

function CKErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKErrorDomain');
end;

function CKPartialErrorsByItemIDKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKPartialErrorsByItemIDKey');
end;

function CKRecordChangedErrorAncestorRecordKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordChangedErrorAncestorRecordKey');
end;

function CKRecordChangedErrorServerRecordKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordChangedErrorServerRecordKey');
end;

function CKRecordChangedErrorClientRecordKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordChangedErrorClientRecordKey');
end;

function CKErrorUserDidResetEncryptedDataKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKErrorUserDidResetEncryptedDataKey');
end;

function CKErrorRetryAfterKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKErrorRetryAfterKey');
end;

function CKRecordZoneDefaultName: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordZoneDefaultName');
end;

function CKRecordTypeShare: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordTypeShare');
end;

function CKRecordNameZoneWideShare: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordNameZoneWideShare');
end;

function CKShareTitleKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKShareTitleKey');
end;

function CKShareThumbnailImageDataKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKShareThumbnailImageDataKey');
end;

function CKShareTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKShareTypeKey');
end;

initialization
  CloudKitModule := dlopen(MarshaledAString(libCloudKit), RTLD_LAZY);

finalization
  dlclose(CloudKitModule);

end.