unit DW.Macapi.CloudKit;

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
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation, Macapi.CoreLocation,
  // DW
  DW.Macapi.Foundation;

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

type
  CKAsset = interface;
  CKReference = interface;
  CKRecordValue = interface;
  CKRecord = interface;
  CKRecordKeyValueSetting = interface;
  CKSubscription = interface;
  CKQuerySubscription = interface;
  CKRecordZoneSubscription = interface;
  CKDatabaseSubscription = interface;
  CKNotificationInfo = interface;
  CKDatabase = interface;
  CKOperation = interface;
  CKOperationConfiguration = interface;
  CKContainer = interface;
  CKLocationSortDescriptor = interface;
  CKNotificationID = interface;
  CKNotification = interface;
  CKQueryNotification = interface;
  CKRecordZoneNotification = interface;
  CKDatabaseNotification = interface;
  CKQuery = interface;
  CKRecordZone = interface;
  CKRecordID = interface;
  CKRecordZoneID = interface;
  CKDatabaseOperation = interface;
  CKServerChangeToken = interface;
  CKShareParticipant = interface;
  CKShare = interface;
  CKShareMetadata = interface;
  CKUserIdentity = interface;
  CKUserIdentityLookupInfo = interface;
  CKAcceptSharesOperation = interface;
  CKDiscoverAllUserIdentitiesOperation = interface;
  CKDiscoverUserIdentitiesOperation = interface;
  CKFetchDatabaseChangesOperation = interface;
  CKFetchNotificationChangesOperation = interface;
  CKFetchRecordChangesOperation = interface;
  CKFetchRecordsOperation = interface;
  CKFetchRecordZoneChangesOperation = interface;
  CKFetchRecordZoneChangesConfiguration = interface;
  CKFetchRecordZoneChangesOptions = interface;
  CKFetchRecordZonesOperation = interface;
  CKFetchShareMetadataOperation = interface;
  CKFetchShareParticipantsOperation = interface;
  CKFetchSubscriptionsOperation = interface;
  CKFetchWebAuthTokenOperation = interface;
  CKMarkNotificationsReadOperation = interface;
  CKModifyBadgeOperation = interface;
  CKModifyRecordsOperation = interface;
  CKModifyRecordZonesOperation = interface;
  CKModifySubscriptionsOperation = interface;
  CKOperationGroup = interface;
  CKQueryCursor = interface;
  CKQueryOperation = interface;
  CKAllowedSharingOptions = interface;
  CKSystemSharingUIObserver = interface;

  CKReferenceAction = NSInteger;
  CKRecordType = NSString;
  CKRecordFieldKey = NSString;
  CKSubscriptionType = NSInteger;
  CKSubscriptionID = NSString;
  CKQuerySubscriptionOptions = NSInteger;
  CKDatabaseScope = NSInteger;
  CKOperationID = NSString;
  CKAccountStatus = NSInteger;
  CKApplicationPermissions = NSInteger;
  CKApplicationPermissionStatus = NSInteger;

  CKApplicationPermissionBlock = procedure(applicationPermissionStatus: CKApplicationPermissionStatus; error: NSError) of object;
  CKErrorCode = NSInteger;
  CKNotificationType = NSInteger;
  CKQueryNotificationReason = NSInteger;
  CKRecordZoneCapabilities = NSInteger;
  CKShareParticipantAcceptanceStatus = NSInteger;
  CKShareParticipantPermission = NSInteger;
  CKShareParticipantRole = NSInteger;
  CKShareParticipantType = NSInteger;
  CKRecordSavePolicy = NSInteger;
  CKOperationGroupTransferSize = NSInteger;

  CKSharePreparationCompletionHandler = procedure(p1: CKShare; p2: NSError) of object;

  CKSharePreparationHandler = procedure(p1: CKSharePreparationCompletionHandler) of object;
  CKSharingParticipantAccessOption = NSInteger;
  CKSharingParticipantPermissionOption = NSInteger;
  TCKDatabaseBlockMethod1 = procedure(&record: CKRecord; error: NSError) of object;
  TCKDatabaseBlockMethod2 = procedure(recordID: CKRecordID; error: NSError) of object;
  TCKDatabaseBlockMethod3 = procedure(results: NSArray; error: NSError) of object;
  TCKDatabaseBlockMethod4 = procedure(zones: NSArray; error: NSError) of object;
  TCKDatabaseBlockMethod5 = procedure(zone: CKRecordZone; error: NSError) of object;
  TCKDatabaseBlockMethod6 = procedure(zoneID: CKRecordZoneID; error: NSError) of object;
  TCKDatabaseBlockMethod7 = procedure(subscription: CKSubscription; error: NSError) of object;
  TCKDatabaseBlockMethod8 = procedure(subscriptions: NSArray; error: NSError) of object;
  TCKDatabaseBlockMethod9 = procedure(subscriptionID: CKSubscriptionID; error: NSError) of object;
  TCKOperationBlockMethod1 = procedure of object;
  TCKContainerBlockMethod1 = procedure(accountStatus: CKAccountStatus; error: NSError) of object;
  TCKContainerBlockMethod2 = procedure(recordID: CKRecordID; error: NSError) of object;
  TCKContainerBlockMethod3 = procedure(userIdentities: NSArray; error: NSError) of object;
  TCKContainerBlockMethod4 = procedure(userInfo: CKUserIdentity; error: NSError) of object;
  TCKContainerBlockMethod5 = procedure(shareParticipant: CKShareParticipant; error: NSError) of object;
  TCKContainerBlockMethod6 = procedure(metadata: CKShareMetadata; error: NSError) of object;
  TCKContainerBlockMethod7 = procedure(acceptedShare: CKShare; error: NSError) of object;
  TCKContainerBlockMethod8 = procedure(outstandingOperationIDs: NSArray; error: NSError) of object;
  TCKContainerBlockMethod9 = procedure(outstandingOperation: CKOperation; error: NSError) of object;
  TCKAcceptSharesOperationBlockMethod1 = procedure(param1: CKShareMetadata; param2: CKShare; param3: NSError) of object;
  TCKAcceptSharesOperationBlockMethod2 = procedure() of object;
  TCKAcceptSharesOperationBlockMethod3 = procedure(param1: NSError) of object;
  TCKDiscoverAllUserIdentitiesOperationBlockMethod1 = procedure(param1: CKUserIdentity) of object;
  TCKDiscoverAllUserIdentitiesOperationBlockMethod2 = procedure() of object;
  TCKDiscoverAllUserIdentitiesOperationBlockMethod3 = procedure(param1: NSError) of object;
  TCKDiscoverUserIdentitiesOperationBlockMethod1 = procedure(param1: CKUserIdentity; param2: CKUserIdentityLookupInfo) of object;
  TCKDiscoverUserIdentitiesOperationBlockMethod2 = procedure() of object;
  TCKDiscoverUserIdentitiesOperationBlockMethod3 = procedure(param1: NSError) of object;
  TCKFetchDatabaseChangesOperationBlockMethod1 = procedure(param1: CKRecordZoneID) of object;
  TCKFetchDatabaseChangesOperationBlockMethod2 = procedure() of object;
  TCKFetchDatabaseChangesOperationBlockMethod3 = procedure(param1: CKRecordZoneID) of object;
  TCKFetchDatabaseChangesOperationBlockMethod4 = procedure(param1: CKRecordZoneID) of object;
  TCKFetchDatabaseChangesOperationBlockMethod5 = procedure(param1: CKRecordZoneID) of object;
  TCKFetchDatabaseChangesOperationBlockMethod6 = procedure(param1: CKServerChangeToken) of object;
  TCKFetchDatabaseChangesOperationBlockMethod7 = procedure(param1: CKServerChangeToken; param2: Boolean; param3: NSError) of object;
  TCKFetchNotificationChangesOperationBlockMethod1 = procedure(param1: CKNotification) of object;
  TCKFetchNotificationChangesOperationBlockMethod2 = procedure() of object;
  TCKFetchNotificationChangesOperationBlockMethod3 = procedure(param1: CKServerChangeToken; param2: NSError) of object;
  TCKFetchRecordChangesOperationBlockMethod1 = procedure(param1: CKRecord) of object;
  TCKFetchRecordChangesOperationBlockMethod2 = procedure() of object;
  TCKFetchRecordChangesOperationBlockMethod3 = procedure(param1: CKRecordID) of object;
  TCKFetchRecordChangesOperationBlockMethod4 = procedure(param1: CKServerChangeToken; param2: NSData; param3: NSError) of object;
  TCKFetchRecordsOperationBlockMethod1 = procedure(param1: CKRecordID; param2: Double) of object;
  TCKFetchRecordsOperationBlockMethod2 = procedure() of object;
  TCKFetchRecordsOperationBlockMethod3 = procedure(param1: CKRecord; param2: CKRecordID; param3: NSError) of object;
  TCKFetchRecordsOperationBlockMethod4 = procedure(param1: NSDictionary; param2: NSError) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod1 = procedure(param1: CKRecord) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod2 = procedure() of object;
  TCKFetchRecordZoneChangesOperationBlockMethod3 = procedure(param1: CKRecordID; param2: CKRecord; param3: NSError) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod4 = procedure(param1: CKRecordID; param2: CKRecordType) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod5 = procedure(param1: CKRecordZoneID; param2: CKServerChangeToken; param3: NSData) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod6 = procedure(param1: CKRecordZoneID; param2: CKServerChangeToken; param3: NSData; param4: Boolean; param5: NSError) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod7 = procedure(param1: NSError) of object;
  TCKFetchRecordZonesOperationBlockMethod1 = procedure(param1: CKRecordZoneID; param2: CKRecordZone; param3: NSError) of object;
  TCKFetchRecordZonesOperationBlockMethod2 = procedure() of object;
  TCKFetchRecordZonesOperationBlockMethod3 = procedure(param1: NSDictionary; param2: NSError) of object;
  TCKFetchShareMetadataOperationBlockMethod1 = procedure(param1: NSURL; param2: CKShareMetadata; param3: NSError) of object;
  TCKFetchShareMetadataOperationBlockMethod2 = procedure() of object;
  TCKFetchShareMetadataOperationBlockMethod3 = procedure(param1: NSError) of object;
  TCKFetchShareParticipantsOperationBlockMethod1 = procedure(param1: CKShareParticipant) of object;
  TCKFetchShareParticipantsOperationBlockMethod2 = procedure() of object;
  TCKFetchShareParticipantsOperationBlockMethod3 = procedure(param1: CKUserIdentityLookupInfo; param2: CKShareParticipant; param3: NSError) of object;
  TCKFetchShareParticipantsOperationBlockMethod4 = procedure(param1: NSError) of object;
  TCKFetchSubscriptionsOperationBlockMethod1 = procedure(param1: CKSubscriptionID; param2: CKSubscription; param3: NSError) of object;
  TCKFetchSubscriptionsOperationBlockMethod2 = procedure() of object;
  TCKFetchSubscriptionsOperationBlockMethod3 = procedure(param1: NSDictionary; param2: NSError) of object;
  TCKFetchWebAuthTokenOperationBlockMethod1 = procedure(param1: NSString; param2: NSError) of object;
  TCKFetchWebAuthTokenOperationBlockMethod2 = procedure() of object;
  TCKMarkNotificationsReadOperationBlockMethod1 = procedure(param1: NSArray; param2: NSError) of object;
  TCKMarkNotificationsReadOperationBlockMethod2 = procedure() of object;
  TCKModifyBadgeOperationBlockMethod1 = procedure(param1: NSError) of object;
  TCKModifyBadgeOperationBlockMethod2 = procedure() of object;
  TCKModifyRecordsOperationBlockMethod1 = procedure(param1: CKRecord; param2: Double) of object;
  TCKModifyRecordsOperationBlockMethod2 = procedure() of object;
  TCKModifyRecordsOperationBlockMethod3 = procedure(param1: CKRecord; param2: NSError) of object;
  TCKModifyRecordsOperationBlockMethod4 = procedure(param1: CKRecordID; param2: CKRecord; param3: NSError) of object;
  TCKModifyRecordsOperationBlockMethod5 = procedure(param1: CKRecordID; param2: NSError) of object;
  TCKModifyRecordsOperationBlockMethod6 = procedure(param1: NSArray; param2: NSArray; param3: NSError) of object;
  TCKModifyRecordZonesOperationBlockMethod1 = procedure(param1: CKRecordZoneID; param2: CKRecordZone; param3: NSError) of object;
  TCKModifyRecordZonesOperationBlockMethod2 = procedure() of object;
  TCKModifyRecordZonesOperationBlockMethod3 = procedure(param1: CKRecordZoneID; param2: NSError) of object;
  TCKModifyRecordZonesOperationBlockMethod4 = procedure(param1: NSArray; param2: NSArray; param3: NSError) of object;
  TCKModifySubscriptionsOperationBlockMethod1 = procedure(param1: CKSubscriptionID; param2: CKSubscription; param3: NSError) of object;
  TCKModifySubscriptionsOperationBlockMethod2 = procedure() of object;
  TCKModifySubscriptionsOperationBlockMethod3 = procedure(param1: CKSubscriptionID; param2: NSError) of object;
  TCKModifySubscriptionsOperationBlockMethod4 = procedure(param1: NSArray; param2: NSArray; param3: NSError) of object;
  TCKQueryOperationBlockMethod1 = procedure(param1: CKRecord) of object;
  TCKQueryOperationBlockMethod2 = procedure() of object;
  TCKQueryOperationBlockMethod3 = procedure(param1: CKRecordID; param2: CKRecord; param3: NSError) of object;
  TCKQueryOperationBlockMethod4 = procedure(param1: CKQueryCursor; param2: NSError) of object;
  TCKSystemSharingUIObserverBlockMethod1 = procedure(param1: CKRecordID; param2: CKShare; param3: NSError) of object;
  TCKSystemSharingUIObserverBlockMethod2 = procedure() of object;
  TCKSystemSharingUIObserverBlockMethod3 = procedure(param1: CKRecordID; param2: NSError) of object;

  CKAssetClass = interface(NSObjectClass)
    ['{4E12CA76-85F3-4573-97C4-D930207543AF}']
    {class} function new: Pointer; cdecl;
  end;

  CKAsset = interface(NSObject)
    ['{CA59A635-4B65-4ED2-B985-22C4BA521A2B}']
    function fileURL: NSURL; cdecl;
    function initWithFileURL(fileURL: NSURL): Pointer; cdecl;
  end;
  TCKAsset = class(TOCGenericImport<CKAssetClass, CKAsset>) end;

  CKReferenceClass = interface(NSObjectClass)
    ['{6487F538-4B5C-4089-823A-B2134F03FCC3}']
    {class} function new: Pointer; cdecl;
  end;

  CKReference = interface(NSObject)
    ['{317CF40A-9565-41EF-93D3-717344F0F831}']
    function initWithRecord(&record: CKRecord; action: CKReferenceAction): Pointer; cdecl;
    function initWithRecordID(recordID: CKRecordID; action: CKReferenceAction): Pointer; cdecl;
    function recordID: CKRecordID; cdecl;
    function referenceAction: CKReferenceAction; cdecl;
  end;
  TCKReference = class(TOCGenericImport<CKReferenceClass, CKReference>) end;

  CKRecordValue = interface(IObjectiveC)
    ['{735B0216-59E4-4BDA-8406-175005A96825}']
  end;

  CKRecordClass = interface(NSObjectClass)
    ['{36446DEC-549E-420E-8305-89F67940DD6E}']
    {class} function new: Pointer; cdecl;
  end;

  CKRecord = interface(NSObject)
    ['{3FB96D69-6471-47FC-98B2-ABF664BD8E2A}']
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
    [MethodName('setObject:forKey:')]
    procedure setObjectForKey(&object: id; forKey: CKRecordFieldKey); cdecl;
    [MethodName('setObject:forKeyedSubscript:')]
    procedure setObjectForKeyedSubscript(&object: id; forKeyedSubscript: CKRecordFieldKey); cdecl;
    procedure setParent(parent: CKReference); cdecl;
    procedure setParentReferenceFromRecord(parentRecord: CKRecord); cdecl;
    procedure setParentReferenceFromRecordID(parentRecordID: CKRecordID); cdecl;
    function share: CKReference; cdecl;
  end;
  TCKRecord = class(TOCGenericImport<CKRecordClass, CKRecord>) end;

  CKRecordKeyValueSetting = interface(IObjectiveC)
    ['{5210DC6E-119A-4791-B754-1B1045AFA3DE}']
    function allKeys: NSArray; cdecl;
    function changedKeys: NSArray; cdecl;
    function objectForKey(key: CKRecordFieldKey): id; cdecl;
    function objectForKeyedSubscript(key: CKRecordFieldKey): id; cdecl;
    [MethodName('setObject:forKey:')]
    procedure setObjectForKey(&object: id; forKey: CKRecordFieldKey); cdecl;
    [MethodName('setObject:forKeyedSubscript:')]
    procedure setObjectForKeyedSubscript(&object: id; forKeyedSubscript: CKRecordFieldKey); cdecl;
  end;

  CKSubscriptionClass = interface(NSObjectClass)
    ['{68C2B938-BAAE-4FC8-8015-B85E6F4EDE64}']
    {class} function new: Pointer; cdecl;
  end;

  CKSubscription = interface(NSObject)
    ['{B80A014F-0CC2-4EC1-B7A8-9CB1CA44C040}']
    function notificationInfo: CKNotificationInfo; cdecl;
    procedure setNotificationInfo(notificationInfo: CKNotificationInfo); cdecl;
    function subscriptionID: CKSubscriptionID; cdecl;
    function subscriptionType: CKSubscriptionType; cdecl;
  end;
  TCKSubscription = class(TOCGenericImport<CKSubscriptionClass, CKSubscription>) end;

  CKQuerySubscriptionClass = interface(CKSubscriptionClass)
    ['{5CAAA31B-837E-4BFC-9FFF-6C5299C06E89}']
  end;

  CKQuerySubscription = interface(CKSubscription)
    ['{A25E04A9-9E6E-442B-940F-4B5A3677BC10}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithRecordType(recordType: CKRecordType; predicate: NSPredicate; options: CKQuerySubscriptionOptions): Pointer; overload; cdecl;
    function initWithRecordType(recordType: CKRecordType; predicate: NSPredicate; subscriptionID: CKSubscriptionID;
      options: CKQuerySubscriptionOptions): Pointer; overload; cdecl;
    function predicate: NSPredicate; cdecl;
    function querySubscriptionOptions: CKQuerySubscriptionOptions; cdecl;
    function recordType: CKRecordType; cdecl;
    procedure setZoneID(zoneID: CKRecordZoneID); cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKQuerySubscription = class(TOCGenericImport<CKQuerySubscriptionClass, CKQuerySubscription>) end;

  CKRecordZoneSubscriptionClass = interface(CKSubscriptionClass)
    ['{2B829F70-E387-482D-BBE6-85D026861355}']
  end;

  CKRecordZoneSubscription = interface(CKSubscription)
    ['{09017734-527B-4FCC-94BE-FE32AB23579E}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithZoneID(zoneID: CKRecordZoneID): Pointer; overload; cdecl;
    function initWithZoneID(zoneID: CKRecordZoneID; subscriptionID: CKSubscriptionID): Pointer; overload; cdecl;
    function recordType: CKRecordType; cdecl;
    procedure setRecordType(recordType: CKRecordType); cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKRecordZoneSubscription = class(TOCGenericImport<CKRecordZoneSubscriptionClass, CKRecordZoneSubscription>) end;

  CKDatabaseSubscriptionClass = interface(CKSubscriptionClass)
    ['{A71A825C-509C-4A54-A8B6-B88146E49239}']
    {class} function new: Pointer; cdecl;
  end;

  CKDatabaseSubscription = interface(CKSubscription)
    ['{19A15A3E-FCFA-49B7-BF8F-36338526844E}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithSubscriptionID(subscriptionID: CKSubscriptionID): Pointer; cdecl;
    function recordType: CKRecordType; cdecl;
    procedure setRecordType(recordType: CKRecordType); cdecl;
  end;
  TCKDatabaseSubscription = class(TOCGenericImport<CKDatabaseSubscriptionClass, CKDatabaseSubscription>) end;

  CKNotificationInfoClass = interface(NSObjectClass)
    ['{FE43C1B7-E971-4708-95AC-464F01B9E5E2}']
  end;

  CKNotificationInfo = interface(NSObject)
    ['{6F763948-2CC9-4411-ACD0-EDE1AA4B64B1}']
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

  CKDatabaseClass = interface(NSObjectClass)
    ['{579C83F7-52B3-4BE3-8D01-B4A13FD9017B}']
    {class} function new: Pointer; cdecl;
  end;

  CKDatabase = interface(NSObject)
    ['{D91F0D1C-0A45-4BB6-AA45-D7BAFA8DD72A}']
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

  CKOperationClass = interface(NSOperationClass)
    ['{76A5C3A7-C388-4491-8D33-7BEBBEF3AFE3}']
  end;

  CKOperation = interface(NSOperation)
    ['{D40ED676-9216-4CC2-9F8A-7E868B4C7DD7}']
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

  CKOperationConfigurationClass = interface(NSObjectClass)
    ['{15C4CF1B-76D0-4CD1-9C21-EBE75D9746E2}']
  end;

  CKOperationConfiguration = interface(NSObject)
    ['{71551DA8-75B1-4D01-9F6F-15F1044A068D}']
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

  CKContainerClass = interface(NSObjectClass)
    ['{91BF681A-09AF-4F35-A4A1-BDEFB9F9F562}']
    {class} function containerWithIdentifier(containerIdentifier: NSString): CKContainer; cdecl;
    {class} function defaultContainer: CKContainer; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CKContainer = interface(NSObject)
    ['{01B399ED-EC13-46E1-A3DA-404CB2D0C670}']
    procedure acceptShareMetadata(metadata: CKShareMetadata; completionHandler: TCKContainerBlockMethod7); cdecl;
    procedure accountStatusWithCompletionHandler(completionHandler: TCKContainerBlockMethod1); cdecl;
    procedure addOperation(operation: CKOperation); cdecl;
    function containerIdentifier: NSString; cdecl;
    function databaseWithDatabaseScope(databaseScope: CKDatabaseScope): CKDatabase; cdecl;
    procedure discoverAllIdentitiesWithCompletionHandler(completionHandler: TCKContainerBlockMethod3); cdecl;
    procedure discoverUserIdentityWithEmailAddress(email: NSString; completionHandler: TCKContainerBlockMethod4); cdecl;
    procedure discoverUserIdentityWithPhoneNumber(phoneNumber: NSString; completionHandler: TCKContainerBlockMethod4); cdecl;
    procedure discoverUserIdentityWithUserRecordID(userRecordID: CKRecordID; completionHandler: TCKContainerBlockMethod4); cdecl;
    procedure fetchAllLongLivedOperationIDsWithCompletionHandler(completionHandler: TCKContainerBlockMethod8); cdecl;
    procedure fetchLongLivedOperationWithID(operationID: CKOperationID; completionHandler: TCKContainerBlockMethod9); cdecl;
    procedure fetchShareMetadataWithURL(url: NSURL; completionHandler: TCKContainerBlockMethod6); cdecl;
    procedure fetchShareParticipantWithEmailAddress(emailAddress: NSString; completionHandler: TCKContainerBlockMethod5); cdecl;
    procedure fetchShareParticipantWithPhoneNumber(phoneNumber: NSString; completionHandler: TCKContainerBlockMethod5); cdecl;
    procedure fetchShareParticipantWithUserRecordID(userRecordID: CKRecordID; completionHandler: TCKContainerBlockMethod5); cdecl;
    procedure fetchUserRecordIDWithCompletionHandler(completionHandler: TCKContainerBlockMethod2); cdecl;
    function privateCloudDatabase: CKDatabase; cdecl;
    function publicCloudDatabase: CKDatabase; cdecl;
    procedure requestApplicationPermission(applicationPermission: CKApplicationPermissions; completionHandler: CKApplicationPermissionBlock); cdecl;
    function sharedCloudDatabase: CKDatabase; cdecl;
    procedure statusForApplicationPermission(applicationPermission: CKApplicationPermissions; completionHandler: CKApplicationPermissionBlock); cdecl;
  end;
  TCKContainer = class(TOCGenericImport<CKContainerClass, CKContainer>) end;

  CKLocationSortDescriptorClass = interface(NSSortDescriptorClass)
    ['{03F379EE-1115-42B2-B7BA-92B8C7C70EB2}']
    {class} function new: Pointer; cdecl;
  end;

  CKLocationSortDescriptor = interface(NSSortDescriptor)
    ['{51727CA3-15BA-4992-A0EC-048146EC6907}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithKey(key: NSString; relativeLocation: CLLocation): Pointer; cdecl;
    function relativeLocation: CLLocation; cdecl;
  end;
  TCKLocationSortDescriptor = class(TOCGenericImport<CKLocationSortDescriptorClass, CKLocationSortDescriptor>) end;

  CKNotificationIDClass = interface(NSObjectClass)
    ['{794779EE-992C-4F00-A7EC-08ED7D861D4B}']
  end;

  CKNotificationID = interface(NSObject)
    ['{7EF21038-92A7-4BFF-9632-13A84198D5E6}']
  end;
  TCKNotificationID = class(TOCGenericImport<CKNotificationIDClass, CKNotificationID>) end;

  CKNotificationClass = interface(NSObjectClass)
    ['{84389E9A-60C5-4664-A7FC-461DAE5B84DD}']
    {class} function new: Pointer; cdecl;
    {class} function notificationFromRemoteNotificationDictionary(notificationDictionary: NSDictionary): Pointer; cdecl;
  end;

  CKNotification = interface(NSObject)
    ['{5EE4B31C-43B2-48DF-9DDD-155F9235CD34}']
    function alertActionLocalizationKey: NSString; cdecl;
    function alertBody: NSString; cdecl;
    function alertLaunchImage: NSString; cdecl;
    function alertLocalizationArgs: NSArray; cdecl;
    function alertLocalizationKey: NSString; cdecl;
    function badge: NSNumber; cdecl;
    function category: NSString; cdecl;
    function containerIdentifier: NSString; cdecl;
    function isPruned: Boolean; cdecl;
    function notificationID: CKNotificationID; cdecl;
    function notificationType: CKNotificationType; cdecl;
    function soundName: NSString; cdecl;
    function subscriptionID: CKSubscriptionID; cdecl;
    function subscriptionOwnerUserRecordID: CKRecordID; cdecl;
    function subtitle: NSString; cdecl;
    function subtitleLocalizationArgs: NSArray; cdecl;
    function subtitleLocalizationKey: NSString; cdecl;
    function title: NSString; cdecl;
    function titleLocalizationArgs: NSArray; cdecl;
    function titleLocalizationKey: NSString; cdecl;
  end;
  TCKNotification = class(TOCGenericImport<CKNotificationClass, CKNotification>) end;

  CKQueryNotificationClass = interface(CKNotificationClass)
    ['{54B54E36-4610-4F66-BC69-BD01665CC3E0}']
  end;

  CKQueryNotification = interface(CKNotification)
    ['{A6155069-0EF0-4E74-A7E6-00339A4CA90F}']
    function databaseScope: CKDatabaseScope; cdecl;
    function queryNotificationReason: CKQueryNotificationReason; cdecl;
    function recordFields: NSDictionary; cdecl;
    function recordID: CKRecordID; cdecl;
  end;
  TCKQueryNotification = class(TOCGenericImport<CKQueryNotificationClass, CKQueryNotification>) end;

  CKRecordZoneNotificationClass = interface(CKNotificationClass)
    ['{7226B0E0-8627-4B34-A8BF-C5F240F1E9E7}']
  end;

  CKRecordZoneNotification = interface(CKNotification)
    ['{B1C88D77-63D8-4150-A074-DE9EC1792871}']
    function databaseScope: CKDatabaseScope; cdecl;
    function recordZoneID: CKRecordZoneID; cdecl;
  end;
  TCKRecordZoneNotification = class(TOCGenericImport<CKRecordZoneNotificationClass, CKRecordZoneNotification>) end;

  CKDatabaseNotificationClass = interface(CKNotificationClass)
    ['{C8F66E18-EEAB-42BE-8405-BC526E8BE996}']
  end;

  CKDatabaseNotification = interface(CKNotification)
    ['{167DA8DD-4106-4895-9BDF-E1B0581E05BB}']
    function databaseScope: CKDatabaseScope; cdecl;
  end;
  TCKDatabaseNotification = class(TOCGenericImport<CKDatabaseNotificationClass, CKDatabaseNotification>) end;

  CKQueryClass = interface(NSObjectClass)
    ['{2C4B2F52-C5BD-44AF-A7CD-8CA2F3C9A559}']
    {class} function new: Pointer; cdecl;
  end;

  CKQuery = interface(NSObject)
    ['{CFCFEDD0-A953-49F0-9FE0-5ADCB7E973A1}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithRecordType(recordType: CKRecordType; predicate: NSPredicate): Pointer; cdecl;
    function predicate: NSPredicate; cdecl;
    function recordType: CKRecordType; cdecl;
    procedure setSortDescriptors(sortDescriptors: NSArray); cdecl;
    function sortDescriptors: NSArray; cdecl;
  end;
  TCKQuery = class(TOCGenericImport<CKQueryClass, CKQuery>) end;

  CKRecordZoneClass = interface(NSObjectClass)
    ['{BB6C7D52-7307-4875-A2E9-CA669B7590F8}']
    {class} function defaultRecordZone: CKRecordZone; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CKRecordZone = interface(NSObject)
    ['{B5B4C9F1-FB18-4744-BCA9-890559E588D3}']
    function capabilities: CKRecordZoneCapabilities; cdecl;
    function initWithZoneID(zoneID: CKRecordZoneID): Pointer; cdecl;
    function initWithZoneName(zoneName: NSString): Pointer; cdecl;
    function share: CKReference; cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKRecordZone = class(TOCGenericImport<CKRecordZoneClass, CKRecordZone>) end;

  CKRecordIDClass = interface(NSObjectClass)
    ['{C0846825-44EF-48D8-A33A-1BEC4F2009C6}']
    {class} function new: Pointer; cdecl;
  end;

  CKRecordID = interface(NSObject)
    ['{6AC496AB-51E6-4A86-B43E-CBB1294C8337}']
    function initWithRecordName(recordName: NSString; zoneID: CKRecordZoneID): Pointer; overload; cdecl;
    function initWithRecordName(recordName: NSString): Pointer; overload; cdecl;
    function recordName: NSString; cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKRecordID = class(TOCGenericImport<CKRecordIDClass, CKRecordID>) end;

  CKRecordZoneIDClass = interface(NSObjectClass)
    ['{04441167-F805-4732-8127-2A719028F6BD}']
    {class} function new: Pointer; cdecl;
  end;

  CKRecordZoneID = interface(NSObject)
    ['{2D42484F-28DA-4275-B755-6F607B9781EE}']
    function initWithZoneName(zoneName: NSString; ownerName: NSString): Pointer; cdecl;
    function ownerName: NSString; cdecl;
    function zoneName: NSString; cdecl;
  end;
  TCKRecordZoneID = class(TOCGenericImport<CKRecordZoneIDClass, CKRecordZoneID>) end;

  CKDatabaseOperationClass = interface(CKOperationClass)
    ['{29206666-68F2-4E53-B9B0-7AD7B55DD353}']
  end;

  CKDatabaseOperation = interface(CKOperation)
    ['{E05CBDC7-C06A-46F6-A1EA-A1E6B5669FA4}']
    function database: CKDatabase; cdecl;
    procedure setDatabase(database: CKDatabase); cdecl;
  end;
  TCKDatabaseOperation = class(TOCGenericImport<CKDatabaseOperationClass, CKDatabaseOperation>) end;

  CKServerChangeTokenClass = interface(NSObjectClass)
    ['{BE6E5D23-3EC8-4D9B-A404-3CACAE09A234}']
    {class} function new: Pointer; cdecl;
  end;

  CKServerChangeToken = interface(NSObject)
    ['{139740E4-9B2A-47AE-90A5-1CF1E20064FE}']
  end;
  TCKServerChangeToken = class(TOCGenericImport<CKServerChangeTokenClass, CKServerChangeToken>) end;

  CKShareParticipantClass = interface(NSObjectClass)
    ['{CE47D023-8549-4C14-ACA5-F38623652AD1}']
    {class} function new: Pointer; cdecl;
  end;

  CKShareParticipant = interface(NSObject)
    ['{E8D56E84-F4EF-4FE7-8B90-7F6FBA505553}']
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

  CKShareClass = interface(CKRecordClass)
    ['{F7310B61-2CF7-406C-8C09-8E5E6B4DE8A3}']
    {class} function new: Pointer; cdecl;
  end;

  CKShare = interface(CKRecord)
    ['{CF47A391-41F4-42A2-993E-7329C0F09792}']
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
    ['{BF791ABD-2EC8-4022-9517-ABD4FCCD6D4A}']
  end;

  CKShareMetadata = interface(NSObject)
    ['{B8B02D17-A0B2-48DC-AE27-712168076D74}']
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

  CKUserIdentityClass = interface(NSObjectClass)
    ['{84563905-DF5E-4548-84A3-CBCE66BE7DF1}']
    {class} function new: Pointer; cdecl;
  end;

  CKUserIdentity = interface(NSObject)
    ['{D807B813-FE3E-48BD-8B5C-220E4CC351BA}']
    function contactIdentifiers: NSArray; cdecl;
    function hasiCloudAccount: Boolean; cdecl;
    function lookupInfo: CKUserIdentityLookupInfo; cdecl;
    function nameComponents: NSPersonNameComponents; cdecl;
    function userRecordID: CKRecordID; cdecl;
  end;
  TCKUserIdentity = class(TOCGenericImport<CKUserIdentityClass, CKUserIdentity>) end;

  CKUserIdentityLookupInfoClass = interface(NSObjectClass)
    ['{A215110D-B89C-453F-86EA-23F3C8CDE839}']
    {class} function lookupInfosWithEmails(emails: NSArray): NSArray; cdecl;
    {class} function lookupInfosWithPhoneNumbers(phoneNumbers: NSArray): NSArray; cdecl;
    {class} function lookupInfosWithRecordIDs(recordIDs: NSArray): NSArray; cdecl;
    {class} function new: Pointer; cdecl;
  end;

  CKUserIdentityLookupInfo = interface(NSObject)
    ['{E15BA26A-D0F5-4047-8BE3-09D747C9ED77}']
    function emailAddress: NSString; cdecl;
    function initWithEmailAddress(emailAddress: NSString): Pointer; cdecl;
    function initWithPhoneNumber(phoneNumber: NSString): Pointer; cdecl;
    function initWithUserRecordID(userRecordID: CKRecordID): Pointer; cdecl;
    function phoneNumber: NSString; cdecl;
    function userRecordID: CKRecordID; cdecl;
  end;
  TCKUserIdentityLookupInfo = class(TOCGenericImport<CKUserIdentityLookupInfoClass, CKUserIdentityLookupInfo>) end;

  CKAcceptSharesOperationClass = interface(CKOperationClass)
    ['{035AC3BE-2547-4195-A215-8A7C2AF060B0}']
  end;

  CKAcceptSharesOperation = interface(CKOperation)
    ['{1DFCADAC-8414-4B8B-87B9-96B3519D1A11}']
    function acceptSharesCompletionBlock: TCKAcceptSharesOperationBlockMethod3; cdecl;
    function initWithShareMetadatas(shareMetadatas: NSArray): Pointer; cdecl;
    function perShareCompletionBlock: TCKAcceptSharesOperationBlockMethod1; cdecl;
    procedure setAcceptSharesCompletionBlock(acceptSharesCompletionBlock: TCKAcceptSharesOperationBlockMethod2); cdecl;
    procedure setPerShareCompletionBlock(perShareCompletionBlock: TCKAcceptSharesOperationBlockMethod2); cdecl;
    procedure setShareMetadatas(shareMetadatas: NSArray); cdecl;
    function shareMetadatas: NSArray; cdecl;
  end;
  TCKAcceptSharesOperation = class(TOCGenericImport<CKAcceptSharesOperationClass, CKAcceptSharesOperation>) end;

  CKDiscoverAllUserIdentitiesOperationClass = interface(CKOperationClass)
    ['{2AFAC4F9-7676-4B81-AF93-0DDEE6B883D4}']
  end;

  CKDiscoverAllUserIdentitiesOperation = interface(CKOperation)
    ['{5A615BE8-27DA-45F8-86FB-99D8B4969003}']
    function discoverAllUserIdentitiesCompletionBlock: TCKDiscoverAllUserIdentitiesOperationBlockMethod3; cdecl;
    procedure setDiscoverAllUserIdentitiesCompletionBlock(
      discoverAllUserIdentitiesCompletionBlock: TCKDiscoverAllUserIdentitiesOperationBlockMethod2); cdecl;
    procedure setUserIdentityDiscoveredBlock(userIdentityDiscoveredBlock: TCKDiscoverAllUserIdentitiesOperationBlockMethod2); cdecl;
    function userIdentityDiscoveredBlock: TCKDiscoverAllUserIdentitiesOperationBlockMethod1; cdecl;
  end;
  TCKDiscoverAllUserIdentitiesOperation = class(TOCGenericImport<CKDiscoverAllUserIdentitiesOperationClass, CKDiscoverAllUserIdentitiesOperation>) end;

  CKDiscoverUserIdentitiesOperationClass = interface(CKOperationClass)
    ['{1659284F-F430-42AB-BBD9-0ED37F02E5BA}']
  end;

  CKDiscoverUserIdentitiesOperation = interface(CKOperation)
    ['{9F3C8402-5AD5-4213-8CE4-7904B04738E6}']
    function discoverUserIdentitiesCompletionBlock: TCKDiscoverUserIdentitiesOperationBlockMethod3; cdecl;
    function initWithUserIdentityLookupInfos(userIdentityLookupInfos: NSArray): Pointer; cdecl;
    procedure setDiscoverUserIdentitiesCompletionBlock(discoverUserIdentitiesCompletionBlock: TCKDiscoverUserIdentitiesOperationBlockMethod2); cdecl;
    procedure setUserIdentityDiscoveredBlock(userIdentityDiscoveredBlock: TCKDiscoverUserIdentitiesOperationBlockMethod2); cdecl;
    procedure setUserIdentityLookupInfos(userIdentityLookupInfos: NSArray); cdecl;
    function userIdentityDiscoveredBlock: TCKDiscoverUserIdentitiesOperationBlockMethod1; cdecl;
    function userIdentityLookupInfos: NSArray; cdecl;
  end;
  TCKDiscoverUserIdentitiesOperation = class(TOCGenericImport<CKDiscoverUserIdentitiesOperationClass, CKDiscoverUserIdentitiesOperation>) end;

  CKFetchDatabaseChangesOperationClass = interface(CKDatabaseOperationClass)
    ['{FB2023CC-1904-46A0-A8D6-12FC087916E6}']
  end;

  CKFetchDatabaseChangesOperation = interface(CKDatabaseOperation)
    ['{ADCE8DAE-99C0-486D-8075-E3B56C2FD67E}']
    function changeTokenUpdatedBlock: TCKFetchDatabaseChangesOperationBlockMethod6; cdecl;
    function fetchAllChanges: Boolean; cdecl;
    function fetchDatabaseChangesCompletionBlock: TCKFetchDatabaseChangesOperationBlockMethod7; cdecl;
    function initWithPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken): Pointer; cdecl;
    function previousServerChangeToken: CKServerChangeToken; cdecl;
    function recordZoneWithIDChangedBlock: TCKFetchDatabaseChangesOperationBlockMethod1; cdecl;
    function recordZoneWithIDWasDeletedBlock: TCKFetchDatabaseChangesOperationBlockMethod1; cdecl;
    function recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock: TCKFetchDatabaseChangesOperationBlockMethod1; cdecl;
    function recordZoneWithIDWasPurgedBlock: TCKFetchDatabaseChangesOperationBlockMethod1; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setChangeTokenUpdatedBlock(changeTokenUpdatedBlock: TCKFetchDatabaseChangesOperationBlockMethod2); cdecl;
    procedure setFetchAllChanges(fetchAllChanges: Boolean); cdecl;
    procedure setFetchDatabaseChangesCompletionBlock(fetchDatabaseChangesCompletionBlock: TCKFetchDatabaseChangesOperationBlockMethod2); cdecl;
    procedure setPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken); cdecl;
    procedure setRecordZoneWithIDChangedBlock(recordZoneWithIDChangedBlock: TCKFetchDatabaseChangesOperationBlockMethod2); cdecl;
    procedure setRecordZoneWithIDWasDeletedBlock(recordZoneWithIDWasDeletedBlock: TCKFetchDatabaseChangesOperationBlockMethod2); cdecl;
    procedure setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock(
      recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock: TCKFetchDatabaseChangesOperationBlockMethod2); cdecl;
    procedure setRecordZoneWithIDWasPurgedBlock(recordZoneWithIDWasPurgedBlock: TCKFetchDatabaseChangesOperationBlockMethod2); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchDatabaseChangesOperation = class(TOCGenericImport<CKFetchDatabaseChangesOperationClass, CKFetchDatabaseChangesOperation>) end;

  CKFetchNotificationChangesOperationClass = interface(CKOperationClass)
    ['{85BC170F-C132-4F8F-8731-A55B8AF94CC3}']
  end;

  CKFetchNotificationChangesOperation = interface(CKOperation)
    ['{18D04C11-0886-46F6-8A81-0C9DD1674B25}']
    function fetchNotificationChangesCompletionBlock: TCKFetchNotificationChangesOperationBlockMethod3; cdecl;
    function initWithPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken): Pointer; cdecl;
    function moreComing: Boolean; cdecl;
    function notificationChangedBlock: TCKFetchNotificationChangesOperationBlockMethod1; cdecl;
    function previousServerChangeToken: CKServerChangeToken; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setFetchNotificationChangesCompletionBlock(
      fetchNotificationChangesCompletionBlock: TCKFetchNotificationChangesOperationBlockMethod2); cdecl;
    procedure setNotificationChangedBlock(notificationChangedBlock: TCKFetchNotificationChangesOperationBlockMethod2); cdecl;
    procedure setPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchNotificationChangesOperation = class(TOCGenericImport<CKFetchNotificationChangesOperationClass, CKFetchNotificationChangesOperation>) end;

  CKFetchRecordChangesOperationClass = interface(CKDatabaseOperationClass)
    ['{6E7C3E01-5D20-4852-B1BB-E7FDDBBAA132}']
  end;

  CKFetchRecordChangesOperation = interface(CKDatabaseOperation)
    ['{0D8B964E-BDC7-4830-AB04-6DFF990EAA4D}']
    function desiredKeys: NSArray; cdecl;
    function fetchRecordChangesCompletionBlock: TCKFetchRecordChangesOperationBlockMethod4; cdecl;
    function initWithRecordZoneID(recordZoneID: CKRecordZoneID; previousServerChangeToken: CKServerChangeToken): Pointer; cdecl;
    function moreComing: Boolean; cdecl;
    function previousServerChangeToken: CKServerChangeToken; cdecl;
    function recordChangedBlock: TCKFetchRecordChangesOperationBlockMethod1; cdecl;
    function recordWithIDWasDeletedBlock: TCKFetchRecordChangesOperationBlockMethod3; cdecl;
    function recordZoneID: CKRecordZoneID; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setDesiredKeys(desiredKeys: NSArray); cdecl;
    procedure setFetchRecordChangesCompletionBlock(fetchRecordChangesCompletionBlock: TCKFetchRecordChangesOperationBlockMethod2); cdecl;
    procedure setPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken); cdecl;
    procedure setRecordChangedBlock(recordChangedBlock: TCKFetchRecordChangesOperationBlockMethod2); cdecl;
    procedure setRecordWithIDWasDeletedBlock(recordWithIDWasDeletedBlock: TCKFetchRecordChangesOperationBlockMethod2); cdecl;
    procedure setRecordZoneID(recordZoneID: CKRecordZoneID); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchRecordChangesOperation = class(TOCGenericImport<CKFetchRecordChangesOperationClass, CKFetchRecordChangesOperation>) end;

  CKFetchRecordsOperationClass = interface(CKDatabaseOperationClass)
    ['{A0368241-A33D-45D7-B260-0FB43E5FD73E}']
    {class} function fetchCurrentUserRecordOperation: Pointer; cdecl;
  end;

  CKFetchRecordsOperation = interface(CKDatabaseOperation)
    ['{065799C2-DBFD-4B65-8769-3A2BFB8DE3EB}']
    function desiredKeys: NSArray; cdecl;
    function fetchRecordsCompletionBlock: TCKFetchRecordsOperationBlockMethod4; cdecl;
    function initWithRecordIDs(recordIDs: NSArray): Pointer; cdecl;
    function perRecordCompletionBlock: TCKFetchRecordsOperationBlockMethod3; cdecl;
    function perRecordProgressBlock: TCKFetchRecordsOperationBlockMethod1; cdecl;
    function recordIDs: NSArray; cdecl;
    procedure setDesiredKeys(desiredKeys: NSArray); cdecl;
    procedure setFetchRecordsCompletionBlock(fetchRecordsCompletionBlock: TCKFetchRecordsOperationBlockMethod2); cdecl;
    procedure setPerRecordCompletionBlock(perRecordCompletionBlock: TCKFetchRecordsOperationBlockMethod2); cdecl;
    procedure setPerRecordProgressBlock(perRecordProgressBlock: TCKFetchRecordsOperationBlockMethod2); cdecl;
    procedure setRecordIDs(recordIDs: NSArray); cdecl;
  end;
  TCKFetchRecordsOperation = class(TOCGenericImport<CKFetchRecordsOperationClass, CKFetchRecordsOperation>) end;

  CKFetchRecordZoneChangesOperationClass = interface(CKDatabaseOperationClass)
    ['{1A8EB59E-9FD6-47CE-BBC6-FB345618885B}']
  end;

  CKFetchRecordZoneChangesOperation = interface(CKDatabaseOperation)
    ['{6728FB49-072A-4F4C-8599-82107BCDFE20}']
    function configurationsByRecordZoneID: NSDictionary; cdecl;
    function fetchAllChanges: Boolean; cdecl;
    function fetchRecordZoneChangesCompletionBlock: TCKFetchRecordZoneChangesOperationBlockMethod7; cdecl;
    [MethodName('initWithRecordZoneIDs:configurationsByRecordZoneID:')]
    function initWithRecordZoneIDsConfigurationsByRecordZoneID(recordZoneIDs: NSArray; configurationsByRecordZoneID: NSDictionary): Pointer; cdecl;
    [MethodName('initWithRecordZoneIDs:optionsByRecordZoneID:')]
    function initWithRecordZoneIDsOptionsByRecordZoneID(recordZoneIDs: NSArray; optionsByRecordZoneID: NSDictionary): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("initWithRecordZoneIDs:configurationsByRecordZoneID:", macos(10.12, 10.14), ios(10.0, 12.0), tvos(10.0, 12.0), watchos(3.0, 5.0))
    function optionsByRecordZoneID: NSDictionary; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("configurationsByRecordZoneID", macos(10.12, 10.14), ios(10.0, 12.0), tvos(10.0, 12.0), watchos(3.0, 5.0))
    function recordChangedBlock: TCKFetchRecordZoneChangesOperationBlockMethod1; cdecl; // API_DEPRECATED("Use recordWasChangedBlock instead, which surfaces per-record errors", macos(10.12, 12.0), ios(10.0, 15.0), tvos(10.0, 15.0), watchos(3.0, 8.0))
    function recordWasChangedBlock: TCKFetchRecordZoneChangesOperationBlockMethod3; cdecl;
    function recordWithIDWasDeletedBlock: TCKFetchRecordZoneChangesOperationBlockMethod4; cdecl;
    function recordZoneChangeTokensUpdatedBlock: TCKFetchRecordZoneChangesOperationBlockMethod5; cdecl;
    function recordZoneFetchCompletionBlock: TCKFetchRecordZoneChangesOperationBlockMethod6; cdecl;
    function recordZoneIDs: NSArray; cdecl;
    procedure setConfigurationsByRecordZoneID(configurationsByRecordZoneID: NSDictionary); cdecl;
    procedure setFetchAllChanges(fetchAllChanges: Boolean); cdecl;
    procedure setFetchRecordZoneChangesCompletionBlock(fetchRecordZoneChangesCompletionBlock: TCKFetchRecordZoneChangesOperationBlockMethod2); cdecl;
    procedure setOptionsByRecordZoneID(optionsByRecordZoneID: NSDictionary); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("configurationsByRecordZoneID", macos(10.12, 10.14), ios(10.0, 12.0), tvos(10.0, 12.0), watchos(3.0, 5.0))
    procedure setRecordChangedBlock(recordChangedBlock: TCKFetchRecordZoneChangesOperationBlockMethod2); cdecl; // API_DEPRECATED("Use recordWasChangedBlock instead, which surfaces per-record errors", macos(10.12, 12.0), ios(10.0, 15.0), tvos(10.0, 15.0), watchos(3.0, 8.0))
    procedure setRecordWasChangedBlock(recordWasChangedBlock: TCKFetchRecordZoneChangesOperationBlockMethod2); cdecl;
    procedure setRecordWithIDWasDeletedBlock(recordWithIDWasDeletedBlock: TCKFetchRecordZoneChangesOperationBlockMethod2); cdecl;
    procedure setRecordZoneChangeTokensUpdatedBlock(recordZoneChangeTokensUpdatedBlock: TCKFetchRecordZoneChangesOperationBlockMethod2); cdecl;
    procedure setRecordZoneFetchCompletionBlock(recordZoneFetchCompletionBlock: TCKFetchRecordZoneChangesOperationBlockMethod2); cdecl;
    procedure setRecordZoneIDs(recordZoneIDs: NSArray); cdecl;
  end;
  TCKFetchRecordZoneChangesOperation = class(TOCGenericImport<CKFetchRecordZoneChangesOperationClass, CKFetchRecordZoneChangesOperation>) end;

  CKFetchRecordZoneChangesConfigurationClass = interface(NSObjectClass)
    ['{85C13A90-D847-428E-9215-3A1462F765DB}']
  end;

  CKFetchRecordZoneChangesConfiguration = interface(NSObject)
    ['{7B90FF5C-46BB-4570-A610-E0DFE6876C71}']
    function desiredKeys: NSArray; cdecl;
    function previousServerChangeToken: CKServerChangeToken; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setDesiredKeys(desiredKeys: NSArray); cdecl;
    procedure setPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchRecordZoneChangesConfiguration = class(TOCGenericImport<CKFetchRecordZoneChangesConfigurationClass,
    CKFetchRecordZoneChangesConfiguration>) end;

  CKFetchRecordZoneChangesOptionsClass = interface(NSObjectClass)
    ['{54D86F3F-0FB4-4750-9236-4D84E90A120B}']
  end;

  CKFetchRecordZoneChangesOptions = interface(NSObject)
    ['{1D67C039-09FF-4541-9B36-B62DB73D3803}']
    function desiredKeys: NSArray; cdecl;
    function previousServerChangeToken: CKServerChangeToken; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setDesiredKeys(desiredKeys: NSArray); cdecl;
    procedure setPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchRecordZoneChangesOptions = class(TOCGenericImport<CKFetchRecordZoneChangesOptionsClass, CKFetchRecordZoneChangesOptions>) end;

  CKFetchRecordZonesOperationClass = interface(CKDatabaseOperationClass)
    ['{22023A31-3CB9-4310-89F1-4E9F43E75326}']
    {class} function fetchAllRecordZonesOperation: Pointer; cdecl;
  end;

  CKFetchRecordZonesOperation = interface(CKDatabaseOperation)
    ['{3EA68A5F-BC86-49DB-B543-D0BC10D03091}']
    function fetchRecordZonesCompletionBlock: TCKFetchRecordZonesOperationBlockMethod3; cdecl;
    function initWithRecordZoneIDs(zoneIDs: NSArray): Pointer; cdecl;
    function perRecordZoneCompletionBlock: TCKFetchRecordZonesOperationBlockMethod1; cdecl;
    function recordZoneIDs: NSArray; cdecl;
    procedure setFetchRecordZonesCompletionBlock(fetchRecordZonesCompletionBlock: TCKFetchRecordZonesOperationBlockMethod2); cdecl;
    procedure setPerRecordZoneCompletionBlock(perRecordZoneCompletionBlock: TCKFetchRecordZonesOperationBlockMethod2); cdecl;
    procedure setRecordZoneIDs(recordZoneIDs: NSArray); cdecl;
  end;
  TCKFetchRecordZonesOperation = class(TOCGenericImport<CKFetchRecordZonesOperationClass, CKFetchRecordZonesOperation>) end;

  CKFetchShareMetadataOperationClass = interface(CKOperationClass)
    ['{1405479E-774E-4D27-AFD4-78343F42F3A6}']
  end;

  CKFetchShareMetadataOperation = interface(CKOperation)
    ['{9FF43C08-CB8C-442E-B1E9-5B2424E1213F}']
    function fetchShareMetadataCompletionBlock: TCKFetchShareMetadataOperationBlockMethod3; cdecl;
    function initWithShareURLs(shareURLs: NSArray): Pointer; cdecl;
    function perShareMetadataBlock: TCKFetchShareMetadataOperationBlockMethod1; cdecl;
    function rootRecordDesiredKeys: NSArray; cdecl;
    procedure setFetchShareMetadataCompletionBlock(fetchShareMetadataCompletionBlock: TCKFetchShareMetadataOperationBlockMethod2); cdecl;
    procedure setPerShareMetadataBlock(perShareMetadataBlock: TCKFetchShareMetadataOperationBlockMethod2); cdecl;
    procedure setRootRecordDesiredKeys(rootRecordDesiredKeys: NSArray); cdecl;
    procedure setShareURLs(shareURLs: NSArray); cdecl;
    procedure setShouldFetchRootRecord(shouldFetchRootRecord: Boolean); cdecl;
    function shareURLs: NSArray; cdecl;
    function shouldFetchRootRecord: Boolean; cdecl;
  end;
  TCKFetchShareMetadataOperation = class(TOCGenericImport<CKFetchShareMetadataOperationClass, CKFetchShareMetadataOperation>) end;

  CKFetchShareParticipantsOperationClass = interface(CKOperationClass)
    ['{981EE2CC-1C19-4E07-93D4-377B3A31A535}']
  end;

  CKFetchShareParticipantsOperation = interface(CKOperation)
    ['{E16E5C6C-5EA2-47E1-8F1D-1CEF5E448DEE}']
    function fetchShareParticipantsCompletionBlock: TCKFetchShareParticipantsOperationBlockMethod4; cdecl;
    function initWithUserIdentityLookupInfos(userIdentityLookupInfos: NSArray): Pointer; cdecl;
    function perShareParticipantCompletionBlock: TCKFetchShareParticipantsOperationBlockMethod3; cdecl;
    procedure setFetchShareParticipantsCompletionBlock(fetchShareParticipantsCompletionBlock: TCKFetchShareParticipantsOperationBlockMethod2); cdecl;
    procedure setPerShareParticipantCompletionBlock(perShareParticipantCompletionBlock: TCKFetchShareParticipantsOperationBlockMethod2); cdecl;
    procedure setShareParticipantFetchedBlock(shareParticipantFetchedBlock: TCKFetchShareParticipantsOperationBlockMethod2); cdecl; // API_DEPRECATED("Use perShareParticipantCompletionBlock instead, which surfaces per-share-participant errors", macos(10.12, 12.0), ios(10.0, 15.0), tvos(10.0, 15.0), watchos(3.0, 8.0))
    procedure setUserIdentityLookupInfos(userIdentityLookupInfos: NSArray); cdecl;
    function shareParticipantFetchedBlock: TCKFetchShareParticipantsOperationBlockMethod1; cdecl; // API_DEPRECATED("Use perShareParticipantCompletionBlock instead, which surfaces per-share-participant errors", macos(10.12, 12.0), ios(10.0, 15.0), tvos(10.0, 15.0), watchos(3.0, 8.0))
    function userIdentityLookupInfos: NSArray; cdecl;
  end;
  TCKFetchShareParticipantsOperation = class(TOCGenericImport<CKFetchShareParticipantsOperationClass, CKFetchShareParticipantsOperation>) end;

  CKFetchSubscriptionsOperationClass = interface(CKDatabaseOperationClass)
    ['{9583D380-34A5-42F1-94E4-2A787859AAD3}']
    {class} function fetchAllSubscriptionsOperation: Pointer; cdecl;
  end;

  CKFetchSubscriptionsOperation = interface(CKDatabaseOperation)
    ['{E712A9A1-7474-4543-BFE5-93FF792CC2B1}']
    function fetchSubscriptionCompletionBlock: TCKFetchSubscriptionsOperationBlockMethod3; cdecl;
    function initWithSubscriptionIDs(subscriptionIDs: NSArray): Pointer; cdecl;
    function perSubscriptionCompletionBlock: TCKFetchSubscriptionsOperationBlockMethod1; cdecl;
    procedure setFetchSubscriptionCompletionBlock(fetchSubscriptionCompletionBlock: TCKFetchSubscriptionsOperationBlockMethod2); cdecl;
    procedure setPerSubscriptionCompletionBlock(perSubscriptionCompletionBlock: TCKFetchSubscriptionsOperationBlockMethod2); cdecl;
    procedure setSubscriptionIDs(subscriptionIDs: NSArray); cdecl;
    function subscriptionIDs: NSArray; cdecl;
  end;
  TCKFetchSubscriptionsOperation = class(TOCGenericImport<CKFetchSubscriptionsOperationClass, CKFetchSubscriptionsOperation>) end;

  CKFetchWebAuthTokenOperationClass = interface(CKDatabaseOperationClass)
    ['{06500D75-3EB8-4888-A09C-D4A0606FF9C5}']
  end;

  CKFetchWebAuthTokenOperation = interface(CKDatabaseOperation)
    ['{00CDF537-4DE2-4985-8B3F-79A7B8B479BF}']
    function APIToken: NSString; cdecl;
    function fetchWebAuthTokenCompletionBlock: TCKFetchWebAuthTokenOperationBlockMethod1; cdecl;
    function initWithAPIToken(APIToken: NSString): Pointer; cdecl;
    procedure setAPIToken(APIToken: NSString); cdecl;
    procedure setFetchWebAuthTokenCompletionBlock(fetchWebAuthTokenCompletionBlock: TCKFetchWebAuthTokenOperationBlockMethod2); cdecl;
  end;
  TCKFetchWebAuthTokenOperation = class(TOCGenericImport<CKFetchWebAuthTokenOperationClass, CKFetchWebAuthTokenOperation>) end;

  CKMarkNotificationsReadOperationClass = interface(CKOperationClass)
    ['{3F768B82-72AD-4D7A-A2AE-B9A927D83F84}']
  end;

  CKMarkNotificationsReadOperation = interface(CKOperation)
    ['{7CE3A2A2-FA08-4C67-9ADB-8634B659482D}']
    function initWithNotificationIDsToMarkRead(notificationIDs: NSArray): Pointer; cdecl;
    function markNotificationsReadCompletionBlock: TCKMarkNotificationsReadOperationBlockMethod1; cdecl;
    function notificationIDs: NSArray; cdecl;
    procedure setMarkNotificationsReadCompletionBlock(markNotificationsReadCompletionBlock: TCKMarkNotificationsReadOperationBlockMethod2); cdecl;
    procedure setNotificationIDs(notificationIDs: NSArray); cdecl;
  end;
  TCKMarkNotificationsReadOperation = class(TOCGenericImport<CKMarkNotificationsReadOperationClass, CKMarkNotificationsReadOperation>) end;

  CKModifyBadgeOperationClass = interface(CKOperationClass)
    ['{AE798117-5053-4394-81D8-9EA9E9AD2F7A}']
  end;

  CKModifyBadgeOperation = interface(CKOperation)
    ['{BDD77487-BF0C-4DAA-A155-807951283E1B}']
    function badgeValue: NSUInteger; cdecl;
    function initWithBadgeValue(badgeValue: NSUInteger): Pointer; cdecl;
    function modifyBadgeCompletionBlock: TCKModifyBadgeOperationBlockMethod1; cdecl;
    procedure setBadgeValue(badgeValue: NSUInteger); cdecl;
    procedure setModifyBadgeCompletionBlock(modifyBadgeCompletionBlock: TCKModifyBadgeOperationBlockMethod2); cdecl;
  end;
  TCKModifyBadgeOperation = class(TOCGenericImport<CKModifyBadgeOperationClass, CKModifyBadgeOperation>) end;

  CKModifyRecordsOperationClass = interface(CKDatabaseOperationClass)
    ['{F6E22A2D-E044-4C7D-BB72-6D8594547B9D}']
  end;

  CKModifyRecordsOperation = interface(CKDatabaseOperation)
    ['{4BD87909-BB4F-491B-AD07-FDA7FEA2916A}']
    function atomic: Boolean; cdecl;
    function clientChangeTokenData: NSData; cdecl;
    function initWithRecordsToSave(records: NSArray; recordIDsToDelete: NSArray): Pointer; cdecl;
    function modifyRecordsCompletionBlock: TCKModifyRecordsOperationBlockMethod6; cdecl;
    function perRecordCompletionBlock: TCKModifyRecordsOperationBlockMethod3; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("perRecordSaveBlock", macos(10.10, 12.0), ios(8.0, 15.0), tvos(9.0, 15.0), watchos(3.0, 8.0))
    function perRecordDeleteBlock: TCKModifyRecordsOperationBlockMethod5; cdecl;
    function perRecordProgressBlock: TCKModifyRecordsOperationBlockMethod1; cdecl;
    function perRecordSaveBlock: TCKModifyRecordsOperationBlockMethod4; cdecl;
    function recordIDsToDelete: NSArray; cdecl;
    function recordsToSave: NSArray; cdecl;
    function savePolicy: CKRecordSavePolicy; cdecl;
    procedure setAtomic(atomic: Boolean); cdecl;
    procedure setClientChangeTokenData(clientChangeTokenData: NSData); cdecl;
    procedure setModifyRecordsCompletionBlock(modifyRecordsCompletionBlock: TCKModifyRecordsOperationBlockMethod2); cdecl;
    procedure setPerRecordCompletionBlock(perRecordCompletionBlock: TCKModifyRecordsOperationBlockMethod2); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("perRecordSaveBlock", macos(10.10, 12.0), ios(8.0, 15.0), tvos(9.0, 15.0), watchos(3.0, 8.0))
    procedure setPerRecordDeleteBlock(perRecordDeleteBlock: TCKModifyRecordsOperationBlockMethod2); cdecl;
    procedure setPerRecordProgressBlock(perRecordProgressBlock: TCKModifyRecordsOperationBlockMethod2); cdecl;
    procedure setPerRecordSaveBlock(perRecordSaveBlock: TCKModifyRecordsOperationBlockMethod2); cdecl;
    procedure setRecordIDsToDelete(recordIDsToDelete: NSArray); cdecl;
    procedure setRecordsToSave(recordsToSave: NSArray); cdecl;
    procedure setSavePolicy(savePolicy: CKRecordSavePolicy); cdecl;
  end;
  TCKModifyRecordsOperation = class(TOCGenericImport<CKModifyRecordsOperationClass, CKModifyRecordsOperation>) end;

  CKModifyRecordZonesOperationClass = interface(CKDatabaseOperationClass)
    ['{C8DF4EE1-4A18-40EC-B27B-52AF5C4211DD}']
  end;

  CKModifyRecordZonesOperation = interface(CKDatabaseOperation)
    ['{D83E29EA-17CF-4230-B343-CF85AEEB0421}']
    function initWithRecordZonesToSave(recordZonesToSave: NSArray; recordZoneIDsToDelete: NSArray): Pointer; cdecl;
    function modifyRecordZonesCompletionBlock: TCKModifyRecordZonesOperationBlockMethod4; cdecl;
    function perRecordZoneDeleteBlock: TCKModifyRecordZonesOperationBlockMethod3; cdecl;
    function perRecordZoneSaveBlock: TCKModifyRecordZonesOperationBlockMethod1; cdecl;
    function recordZoneIDsToDelete: NSArray; cdecl;
    function recordZonesToSave: NSArray; cdecl;
    procedure setModifyRecordZonesCompletionBlock(modifyRecordZonesCompletionBlock: TCKModifyRecordZonesOperationBlockMethod2); cdecl;
    procedure setPerRecordZoneDeleteBlock(perRecordZoneDeleteBlock: TCKModifyRecordZonesOperationBlockMethod2); cdecl;
    procedure setPerRecordZoneSaveBlock(perRecordZoneSaveBlock: TCKModifyRecordZonesOperationBlockMethod2); cdecl;
    procedure setRecordZoneIDsToDelete(recordZoneIDsToDelete: NSArray); cdecl;
    procedure setRecordZonesToSave(recordZonesToSave: NSArray); cdecl;
  end;
  TCKModifyRecordZonesOperation = class(TOCGenericImport<CKModifyRecordZonesOperationClass, CKModifyRecordZonesOperation>) end;

  CKModifySubscriptionsOperationClass = interface(CKDatabaseOperationClass)
    ['{503D8DB5-46D9-4FBE-BBCC-B6031DDC6949}']
  end;

  CKModifySubscriptionsOperation = interface(CKDatabaseOperation)
    ['{B08F9A69-9D4F-40D0-9529-846D7AF43EE9}']
    function initWithSubscriptionsToSave(subscriptionsToSave: NSArray; subscriptionIDsToDelete: NSArray): Pointer; cdecl;
    function modifySubscriptionsCompletionBlock: TCKModifySubscriptionsOperationBlockMethod4; cdecl;
    function perSubscriptionDeleteBlock: TCKModifySubscriptionsOperationBlockMethod3; cdecl;
    function perSubscriptionSaveBlock: TCKModifySubscriptionsOperationBlockMethod1; cdecl;
    procedure setModifySubscriptionsCompletionBlock(modifySubscriptionsCompletionBlock: TCKModifySubscriptionsOperationBlockMethod2); cdecl;
    procedure setPerSubscriptionDeleteBlock(perSubscriptionDeleteBlock: TCKModifySubscriptionsOperationBlockMethod2); cdecl;
    procedure setPerSubscriptionSaveBlock(perSubscriptionSaveBlock: TCKModifySubscriptionsOperationBlockMethod2); cdecl;
    procedure setSubscriptionIDsToDelete(subscriptionIDsToDelete: NSArray); cdecl;
    procedure setSubscriptionsToSave(subscriptionsToSave: NSArray); cdecl;
    function subscriptionIDsToDelete: NSArray; cdecl;
    function subscriptionsToSave: NSArray; cdecl;
  end;
  TCKModifySubscriptionsOperation = class(TOCGenericImport<CKModifySubscriptionsOperationClass, CKModifySubscriptionsOperation>) end;

  CKOperationGroupClass = interface(NSObjectClass)
    ['{89BA9B7B-D360-4FAF-A11E-4F761E3FDD1B}']
  end;

  CKOperationGroup = interface(NSObject)
    ['{3982C8C3-7FD5-454D-AB8F-C1263E83F5F9}']
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

  CKQueryCursorClass = interface(NSObjectClass)
    ['{1D2AD547-AD1A-48D7-93E7-F6DE9DD1CAFA}']
    {class} function new: Pointer; cdecl;
  end;

  CKQueryCursor = interface(NSObject)
    ['{87C6D65C-3ECC-4164-A318-3728C50314C5}']
  end;
  TCKQueryCursor = class(TOCGenericImport<CKQueryCursorClass, CKQueryCursor>) end;

  CKQueryOperationClass = interface(CKDatabaseOperationClass)
    ['{C274E38D-871E-4E3C-AFAA-526FC444C7F5}']
  end;

  CKQueryOperation = interface(CKDatabaseOperation)
    ['{EBF2F78F-C7DC-40B3-8511-6B08EA411D51}']
    function cursor: CKQueryCursor; cdecl;
    function desiredKeys: NSArray; cdecl;
    function initWithCursor(cursor: CKQueryCursor): Pointer; cdecl;
    function initWithQuery(query: CKQuery): Pointer; cdecl;
    function query: CKQuery; cdecl;
    function queryCompletionBlock: TCKQueryOperationBlockMethod4; cdecl;
    function recordFetchedBlock: TCKQueryOperationBlockMethod1; cdecl; // API_DEPRECATED("Use recordMatchedBlock instead, which surfaces per-record errors", macos(10.10, 12.0), ios(8.0, 15.0), tvos(9.0, 15.0), watchos(3.0, 8.0))
    function recordMatchedBlock: TCKQueryOperationBlockMethod3; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setCursor(cursor: CKQueryCursor); cdecl;
    procedure setDesiredKeys(desiredKeys: NSArray); cdecl;
    procedure setQuery(query: CKQuery); cdecl;
    procedure setQueryCompletionBlock(queryCompletionBlock: TCKQueryOperationBlockMethod2); cdecl;
    procedure setRecordFetchedBlock(recordFetchedBlock: TCKQueryOperationBlockMethod2); cdecl; // API_DEPRECATED("Use recordMatchedBlock instead, which surfaces per-record errors", macos(10.10, 12.0), ios(8.0, 15.0), tvos(9.0, 15.0), watchos(3.0, 8.0))
    procedure setRecordMatchedBlock(recordMatchedBlock: TCKQueryOperationBlockMethod2); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
    procedure setZoneID(zoneID: CKRecordZoneID); cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKQueryOperation = class(TOCGenericImport<CKQueryOperationClass, CKQueryOperation>) end;

  CKAllowedSharingOptionsClass = interface(NSObjectClass)
    ['{3001B103-5D1A-4D78-B4C8-2B4B5D3946AA}']
    {class} function standardOptions: CKAllowedSharingOptions; cdecl;
  end;

  CKAllowedSharingOptions = interface(NSObject)
    ['{1297996D-F001-4DB8-940A-2DAC20AD1F04}']
    function allowedParticipantAccessOptions: CKSharingParticipantAccessOption; cdecl;
    function allowedParticipantPermissionOptions: CKSharingParticipantPermissionOption; cdecl;
    function initWithAllowedParticipantPermissionOptions(allowedParticipantPermissionOptions: CKSharingParticipantPermissionOption;
      allowedParticipantAccessOptions: CKSharingParticipantAccessOption): Pointer; cdecl;
    procedure setAllowedParticipantAccessOptions(allowedParticipantAccessOptions: CKSharingParticipantAccessOption); cdecl;
    procedure setAllowedParticipantPermissionOptions(allowedParticipantPermissionOptions: CKSharingParticipantPermissionOption); cdecl;
  end;
  TCKAllowedSharingOptions = class(TOCGenericImport<CKAllowedSharingOptionsClass, CKAllowedSharingOptions>) end;

  CKSystemSharingUIObserverClass = interface(NSObjectClass)
    ['{656A06A3-7E23-4CA4-BCD6-CCC9739ACAD7}']
    {class} function new: Pointer; cdecl;
  end;

  CKSystemSharingUIObserver = interface(NSObject)
    ['{9994E719-9DF9-44F5-A32C-AFCBD44D959E}']
    function initWithContainer(container: CKContainer): Pointer; cdecl;
    procedure setSystemSharingUIDidSaveShareBlock(systemSharingUIDidSaveShareBlock: TCKSystemSharingUIObserverBlockMethod2); cdecl;
    procedure setSystemSharingUIDidStopSharingBlock(systemSharingUIDidStopSharingBlock: TCKSystemSharingUIObserverBlockMethod2); cdecl;
    function systemSharingUIDidSaveShareBlock: TCKSystemSharingUIObserverBlockMethod1; cdecl;
    function systemSharingUIDidStopSharingBlock: TCKSystemSharingUIObserverBlockMethod3; cdecl;
  end;
  TCKSystemSharingUIObserver = class(TOCGenericImport<CKSystemSharingUIObserverClass, CKSystemSharingUIObserver>) end;

function CKRecordTypeUserRecord: CKRecordType;
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
// Exported const CKQueryOperationMaximumResults has an unsupported type: const NSUInteger

const
  libCloudKit = '/System/Library/Frameworks/CloudKit.framework/CloudKit';

implementation

uses
  System.SysUtils;

var
  CloudKitModule: THandle;

function CKRecordTypeUserRecord: CKRecordType;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordTypeUserRecord');
end;

function CKRecordParentKey: CKRecordFieldKey;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordParentKey');
end;

function CKRecordShareKey: CKRecordFieldKey;
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

function CKRecordTypeShare: CKRecordType;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordTypeShare');
end;

function CKRecordNameZoneWideShare: NSString;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKRecordNameZoneWideShare');
end;

function CKShareTitleKey: CKRecordFieldKey;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKShareTitleKey');
end;

function CKShareThumbnailImageDataKey: CKRecordFieldKey;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKShareThumbnailImageDataKey');
end;

function CKShareTypeKey: CKRecordFieldKey;
begin
  Result := CocoaNSStringConst(libCloudKit, 'CKShareTypeKey');
end;

initialization
  CloudKitModule := LoadLibrary(libCloudKit);

finalization
  if CloudKitModule <> 0 then
    FreeLibrary(CloudKitModule);

end.