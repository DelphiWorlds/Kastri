unit DW.iOSapi.CloudKitExtra;

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
  Macapi.ObjectiveC, Macapi.ObjCRuntime,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreLocation,
  // DW
  DW.iOSapi.CloudKit;

type
  CKAsset = interface;
  CKRecordValue = interface;
  CKRecordKeyValueSetting = interface;
  CKQuerySubscription = interface;
  CKRecordZoneSubscription = interface;
  CKDatabaseSubscription = interface;
  CKLocationSortDescriptor = interface;
  CKNotificationID = interface;
  CKNotification = interface;
  CKQueryNotification = interface;
  CKRecordZoneNotification = interface;
  CKDatabaseNotification = interface;
  CKServerChangeToken = interface;
  CKShare = interface;
  CKShareMetadata = interface;
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
  CKQueryCursor = interface;
  CKQueryOperation = interface;
  CKAllowedSharingOptions = interface;
  CKSystemSharingUIObserver = interface;
  CKSyncEngineState = interface;
  CKSyncEngineStateSerialization = interface;
  CKSyncEnginePendingRecordZoneChange = interface;
  CKSyncEnginePendingDatabaseChange = interface;
  CKSyncEnginePendingZoneSave = interface;
  CKSyncEnginePendingZoneDelete = interface;
  CKSyncEngineRecordZoneChangeBatch = interface;
  CKSyncEngine = interface;
  CKSyncEngineDelegate = interface;
  CKSyncEngineFetchChangesOptions = interface;
  CKSyncEngineFetchChangesScope = interface;
  CKSyncEngineSendChangesOptions = interface;
  CKSyncEngineSendChangesScope = interface;
  CKSyncEngineFetchChangesContext = interface;
  CKSyncEngineSendChangesContext = interface;
  CKSyncEngineConfiguration = interface;
  CKSyncEngineEvent = interface;
  CKSyncEngineStateUpdateEvent = interface;
  CKSyncEngineAccountChangeEvent = interface;
  CKSyncEngineFetchedDatabaseChangesEvent = interface;
  CKSyncEngineFetchedRecordZoneChangesEvent = interface;
  CKSyncEngineSentDatabaseChangesEvent = interface;
  CKSyncEngineSentRecordZoneChangesEvent = interface;
  CKSyncEngineWillFetchChangesEvent = interface;
  CKSyncEngineWillFetchRecordZoneChangesEvent = interface;
  CKSyncEngineDidFetchRecordZoneChangesEvent = interface;
  CKSyncEngineDidFetchChangesEvent = interface;
  CKSyncEngineWillSendChangesEvent = interface;
  CKSyncEngineDidSendChangesEvent = interface;
  CKSyncEngineFetchedRecordDeletion = interface;
  CKSyncEngineFetchedZoneDeletion = interface;
  CKSyncEngineFailedRecordSave = interface;
  CKSyncEngineFailedZoneSave = interface;

  CKSharePreparationCompletionHandler = procedure(p1: CKShare; p2: NSError) of object;
  CKSharePreparationHandler = procedure(p1: CKSharePreparationCompletionHandler) of object;

  CKSharingParticipantAccessOption = NSInteger;
  CKSharingParticipantPermissionOption = NSInteger;
  CKSyncEnginePendingRecordZoneChangeType = NSInteger;
  CKSyncEnginePendingDatabaseChangeType = NSInteger;
  CKSyncEngineSyncReason = NSInteger;
  CKSyncEngineEventType = NSInteger;
  CKSyncEngineAccountChangeType = NSInteger;
  CKSyncEngineZoneDeletionReason = NSInteger;
  TCKAcceptSharesOperationBlockMethod1 = procedure(param1: CKShareMetadata; param2: CKShare; param3: NSError) of object;
  TCKAcceptSharesOperationBlockMethod2 = procedure of object;
  TCKAcceptSharesOperationBlockMethod3 = procedure(param1: NSError) of object;
  TCKDiscoverAllUserIdentitiesOperationBlockMethod1 = procedure(param1: CKUserIdentity) of object;
  TCKDiscoverAllUserIdentitiesOperationBlockMethod2 = procedure of object;
  TCKDiscoverAllUserIdentitiesOperationBlockMethod3 = procedure(param1: NSError) of object;
  TCKDiscoverUserIdentitiesOperationBlockMethod1 = procedure(param1: CKUserIdentity; param2: CKUserIdentityLookupInfo) of object;
  TCKDiscoverUserIdentitiesOperationBlockMethod2 = procedure of object;
  TCKDiscoverUserIdentitiesOperationBlockMethod3 = procedure(param1: NSError) of object;
  TCKFetchDatabaseChangesOperationBlockMethod1 = procedure(param1: CKRecordZoneID) of object;
  TCKFetchDatabaseChangesOperationBlockMethod2 = procedure of object;
  TCKFetchDatabaseChangesOperationBlockMethod3 = procedure(param1: CKRecordZoneID) of object;
  TCKFetchDatabaseChangesOperationBlockMethod4 = procedure(param1: CKRecordZoneID) of object;
  TCKFetchDatabaseChangesOperationBlockMethod5 = procedure(param1: CKRecordZoneID) of object;
  TCKFetchDatabaseChangesOperationBlockMethod6 = procedure(param1: CKServerChangeToken) of object;
  TCKFetchDatabaseChangesOperationBlockMethod7 = procedure(param1: CKServerChangeToken; param2: Boolean; param3: NSError) of object;
  TCKFetchNotificationChangesOperationBlockMethod1 = procedure(param1: CKNotification) of object;
  TCKFetchNotificationChangesOperationBlockMethod2 = procedure of object;
  TCKFetchNotificationChangesOperationBlockMethod3 = procedure(param1: CKServerChangeToken; param2: NSError) of object;
  TCKFetchRecordChangesOperationBlockMethod1 = procedure(param1: CKRecord) of object;
  TCKFetchRecordChangesOperationBlockMethod2 = procedure of object;
  TCKFetchRecordChangesOperationBlockMethod3 = procedure(param1: CKRecordID) of object;
  TCKFetchRecordChangesOperationBlockMethod4 = procedure(param1: CKServerChangeToken; param2: NSData; param3: NSError) of object;
  TCKFetchRecordsOperationBlockMethod1 = procedure(param1: CKRecordID; param2: Double) of object;
  TCKFetchRecordsOperationBlockMethod2 = procedure of object;
  TCKFetchRecordsOperationBlockMethod3 = procedure(param1: CKRecord; param2: CKRecordID; param3: NSError) of object;
  TCKFetchRecordsOperationBlockMethod4 = procedure(param1: NSDictionary; param2: NSError) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod1 = procedure(param1: CKRecord) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod2 = procedure of object;
  TCKFetchRecordZoneChangesOperationBlockMethod3 = procedure(param1: CKRecordID; param2: CKRecord; param3: NSError) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod4 = procedure(param1: CKRecordID; param2: CKRecordType) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod5 = procedure(param1: CKRecordZoneID; param2: CKServerChangeToken; param3: NSData) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod6 = procedure(param1: CKRecordZoneID; param2: CKServerChangeToken; param3: NSData; param4: Boolean;
    param5: NSError) of object;
  TCKFetchRecordZoneChangesOperationBlockMethod7 = procedure(param1: NSError) of object;
  TCKFetchRecordZonesOperationBlockMethod1 = procedure(param1: CKRecordZoneID; param2: CKRecordZone; param3: NSError) of object;
  TCKFetchRecordZonesOperationBlockMethod2 = procedure of object;
  TCKFetchRecordZonesOperationBlockMethod3 = procedure(param1: NSDictionary; param2: NSError) of object;
  TCKFetchShareMetadataOperationBlockMethod1 = procedure(param1: NSURL; param2: CKShareMetadata; param3: NSError) of object;
  TCKFetchShareMetadataOperationBlockMethod2 = procedure of object;
  TCKFetchShareMetadataOperationBlockMethod3 = procedure(param1: NSError) of object;
  TCKFetchShareParticipantsOperationBlockMethod1 = procedure(param1: CKShareParticipant) of object;
  TCKFetchShareParticipantsOperationBlockMethod2 = procedure of object;
  TCKFetchShareParticipantsOperationBlockMethod3 = procedure(param1: CKUserIdentityLookupInfo; param2: CKShareParticipant; param3: NSError) of object;
  TCKFetchShareParticipantsOperationBlockMethod4 = procedure(param1: NSError) of object;
  TCKFetchSubscriptionsOperationBlockMethod1 = procedure(param1: CKSubscriptionID; param2: CKSubscription; param3: NSError) of object;
  TCKFetchSubscriptionsOperationBlockMethod2 = procedure of object;
  TCKFetchSubscriptionsOperationBlockMethod3 = procedure(param1: NSDictionary; param2: NSError) of object;
  TCKFetchWebAuthTokenOperationBlockMethod1 = procedure(param1: NSString; param2: NSError) of object;
  TCKFetchWebAuthTokenOperationBlockMethod2 = procedure of object;
  TCKMarkNotificationsReadOperationBlockMethod1 = procedure(param1: NSArray; param2: NSError) of object;
  TCKMarkNotificationsReadOperationBlockMethod2 = procedure of object;
  TCKModifyBadgeOperationBlockMethod1 = procedure(param1: NSError) of object;
  TCKModifyBadgeOperationBlockMethod2 = procedure of object;
  TCKModifyRecordsOperationBlockMethod1 = procedure(param1: CKRecord; param2: Double) of object;
  TCKModifyRecordsOperationBlockMethod2 = procedure of object;
  TCKModifyRecordsOperationBlockMethod3 = procedure(param1: CKRecord; param2: NSError) of object;
  TCKModifyRecordsOperationBlockMethod4 = procedure(param1: CKRecordID; param2: CKRecord; param3: NSError) of object;
  TCKModifyRecordsOperationBlockMethod5 = procedure(param1: CKRecordID; param2: NSError) of object;
  TCKModifyRecordsOperationBlockMethod6 = procedure(param1: NSArray; param2: NSArray; param3: NSError) of object;
  TCKModifyRecordZonesOperationBlockMethod1 = procedure(param1: CKRecordZoneID; param2: CKRecordZone; param3: NSError) of object;
  TCKModifyRecordZonesOperationBlockMethod2 = procedure of object;
  TCKModifyRecordZonesOperationBlockMethod3 = procedure(param1: CKRecordZoneID; param2: NSError) of object;
  TCKModifyRecordZonesOperationBlockMethod4 = procedure(param1: NSArray; param2: NSArray; param3: NSError) of object;
  TCKModifySubscriptionsOperationBlockMethod1 = procedure(param1: CKSubscriptionID; param2: CKSubscription; param3: NSError) of object;
  TCKModifySubscriptionsOperationBlockMethod2 = procedure of object;
  TCKModifySubscriptionsOperationBlockMethod3 = procedure(param1: CKSubscriptionID; param2: NSError) of object;
  TCKModifySubscriptionsOperationBlockMethod4 = procedure(param1: NSArray; param2: NSArray; param3: NSError) of object;
  TCKQueryOperationBlockMethod1 = procedure(param1: CKRecord) of object;
  TCKQueryOperationBlockMethod2 = procedure of object;
  TCKQueryOperationBlockMethod3 = procedure(param1: CKRecordID; param2: CKRecord; param3: NSError) of object;
  TCKQueryOperationBlockMethod4 = procedure(param1: CKQueryCursor; param2: NSError) of object;
  TCKSystemSharingUIObserverBlockMethod1 = procedure(param1: CKRecordID; param2: CKShare; param3: NSError) of object;
  TCKSystemSharingUIObserverBlockMethod2 = procedure of object;
  TCKSystemSharingUIObserverBlockMethod3 = procedure(param1: CKRecordID; param2: NSError) of object;
  TCKSyncEngineRecordZoneChangeBatchBlockMethod1 = procedure(recordID: CKRecordID) of object;
  TCKSyncEngineBlockMethod1 = procedure(error: NSError) of object;
  TCKSyncEngineBlockMethod2 = procedure of object;

  CKAssetClass = interface(NSObjectClass)
    ['{ABE070D5-B0C1-4D45-8040-348BA8259D6E}']
    {class} function new: Pointer; cdecl;
  end;

  CKAsset = interface(NSObject)
    ['{7759A1C6-E09C-4570-B67F-CA7A0D68C72D}']
    function fileURL: NSURL; cdecl;
    function initWithFileURL(fileURL: NSURL): Pointer; cdecl;
  end;
  TCKAsset = class(TOCGenericImport<CKAssetClass, CKAsset>) end;

  CKRecordValue = interface(IObjectiveC)
    ['{51DD75C1-8F1F-42D5-832B-1C26F8FD5AAA}']
  end;

  CKRecordKeyValueSetting = interface(IObjectiveC)
    ['{64032E38-4534-4491-9480-CB7C5FD43A45}']
    function allKeys: NSArray; cdecl;
    function changedKeys: NSArray; cdecl;
    function objectForKey(key: CKRecordFieldKey): id; cdecl;
    function objectForKeyedSubscript(key: CKRecordFieldKey): id; cdecl;
    procedure setObject(&object: id; forKey: CKRecordFieldKey); cdecl;
    [MethodName('setObject:forKeyedSubscript:')]
    procedure setObjectForKeyedSubscript(&object: id; forKeyedSubscript: CKRecordFieldKey); cdecl;
  end;

  CKQuerySubscriptionClass = interface(CKSubscriptionClass)
    ['{EDAF375E-2BF6-4FA8-BC4C-50713EC1CFEA}']
  end;

  CKQuerySubscription = interface(CKSubscription)
    ['{3BA43178-7FF4-4D85-B7A7-7AC02FC8D07D}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithRecordType(recordType: CKRecordType; predicate: NSPredicate;
      options: CKQuerySubscriptionOptions): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("initWithRecordType:predicate:subscriptionID:options:", macos(10.12, 10.12), ios(10.0, 10.0), tvos(10.0, 10.0), watchos(6.0, 6.0))
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
    ['{B2B39AD4-FA58-4C7E-A210-FE361A52A693}']
  end;

  CKRecordZoneSubscription = interface(CKSubscription)
    ['{A16DB1A8-372C-47BA-9132-6629027011CE}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithZoneID(zoneID: CKRecordZoneID): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("initWithZoneID:subscriptionID:", macos(10.12, 10.12), ios(10.0, 10.0), tvos(10.0, 10.0), watchos(6.0, 6.0))
    function initWithZoneID(zoneID: CKRecordZoneID; subscriptionID: CKSubscriptionID): Pointer; overload; cdecl;
    function recordType: CKRecordType; cdecl;
    procedure setRecordType(recordType: CKRecordType); cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKRecordZoneSubscription = class(TOCGenericImport<CKRecordZoneSubscriptionClass, CKRecordZoneSubscription>) end;

  CKDatabaseSubscriptionClass = interface(CKSubscriptionClass)
    ['{FFC57855-0270-46FC-A328-3FE1668080DB}']
    {class} function new: Pointer; cdecl;
  end;

  CKDatabaseSubscription = interface(CKSubscription)
    ['{A36C291B-5011-4E31-BA83-DD5715A071EA}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithSubscriptionID(subscriptionID: CKSubscriptionID): Pointer; cdecl;
    function recordType: CKRecordType; cdecl;
    procedure setRecordType(recordType: CKRecordType); cdecl;
  end;
  TCKDatabaseSubscription = class(TOCGenericImport<CKDatabaseSubscriptionClass, CKDatabaseSubscription>) end;

  CKLocationSortDescriptorClass = interface(NSSortDescriptorClass)
    ['{99030A8E-B262-4889-ABE2-7C1728AD23CE}']
    {class} function new: Pointer; cdecl;
  end;

  CKLocationSortDescriptor = interface(NSSortDescriptor)
    ['{07713227-4927-4197-ACB3-0684B73A4D59}']
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithKey(key: NSString; relativeLocation: CLLocation): Pointer; cdecl;
    function relativeLocation: CLLocation; cdecl;
  end;
  TCKLocationSortDescriptor = class(TOCGenericImport<CKLocationSortDescriptorClass, CKLocationSortDescriptor>) end;

  CKNotificationIDClass = interface(NSObjectClass)
    ['{033FDDBB-E7C5-4A74-BEC2-E859DC34B4BC}']
  end;

  CKNotificationID = interface(NSObject)
    ['{D412C884-5978-46EE-82DE-B01D6173199F}']
  end;
  TCKNotificationID = class(TOCGenericImport<CKNotificationIDClass, CKNotificationID>) end;

  CKNotificationClass = interface(NSObjectClass)
    ['{AA86F58D-81D0-49C2-85D2-34F49C2868EE}']
    {class} function new: Pointer; cdecl;
    {class} function notificationFromRemoteNotificationDictionary(notificationDictionary: NSDictionary): Pointer; cdecl;
  end;

  CKNotification = interface(NSObject)
    ['{E8B9FBE1-97F8-42C5-86D3-D5FF428EC9F3}']
    function alertActionLocalizationKey: NSString; cdecl; // API_DEPRECATED_BEGIN("Interact with UI elements of a CloudKit-server-generated push message via UserNotifications.framework", macos(10.10, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
    function alertBody: NSString; cdecl; // API_DEPRECATED_BEGIN("Interact with UI elements of a CloudKit-server-generated push message via UserNotifications.framework", macos(10.10, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
    function alertLaunchImage: NSString; cdecl; // API_DEPRECATED_BEGIN("Interact with UI elements of a CloudKit-server-generated push message via UserNotifications.framework", macos(10.10, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
    function alertLocalizationArgs: NSArray; cdecl; // API_DEPRECATED_BEGIN("Interact with UI elements of a CloudKit-server-generated push message via UserNotifications.framework", macos(10.10, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
    function alertLocalizationKey: NSString; cdecl; // API_DEPRECATED_BEGIN("Interact with UI elements of a CloudKit-server-generated push message via UserNotifications.framework", macos(10.10, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
    function badge: NSNumber; cdecl; // API_DEPRECATED_BEGIN("Interact with UI elements of a CloudKit-server-generated push message via UserNotifications.framework", macos(10.10, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
    function category: NSString; cdecl;
    function containerIdentifier: NSString; cdecl;
    function isPruned: Boolean; cdecl;
    function notificationID: CKNotificationID; cdecl;
    function notificationType: CKNotificationType; cdecl;
    function soundName: NSString; cdecl; // API_DEPRECATED_BEGIN("Interact with UI elements of a CloudKit-server-generated push message via UserNotifications.framework", macos(10.10, 14.0), ios(8.0, 17.0), tvos(9.0, 17.0), watchos(3.0, 10.0))
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
    ['{7DD27B8E-AA98-40B8-A924-DA792072908C}']
  end;

  CKQueryNotification = interface(CKNotification)
    ['{0E7053DC-BCFF-417E-9316-087F9037336D}']
    function databaseScope: CKDatabaseScope; cdecl;
    function queryNotificationReason: CKQueryNotificationReason; cdecl;
    function recordFields: NSDictionary; cdecl;
    function recordID: CKRecordID; cdecl;
  end;
  TCKQueryNotification = class(TOCGenericImport<CKQueryNotificationClass, CKQueryNotification>) end;

  CKRecordZoneNotificationClass = interface(CKNotificationClass)
    ['{0A136D0A-E8D1-4EAA-B371-3AFA2B4531C3}']
  end;

  CKRecordZoneNotification = interface(CKNotification)
    ['{8DFAD42A-0C7D-4ABA-934A-2531F2102FF7}']
    function databaseScope: CKDatabaseScope; cdecl;
    function recordZoneID: CKRecordZoneID; cdecl;
  end;
  TCKRecordZoneNotification = class(TOCGenericImport<CKRecordZoneNotificationClass, CKRecordZoneNotification>) end;

  CKDatabaseNotificationClass = interface(CKNotificationClass)
    ['{6043DC1E-351C-4452-81AF-4DF10DF1610F}']
  end;

  CKDatabaseNotification = interface(CKNotification)
    ['{B03FA6C9-4FCC-446D-9F48-4EEE567494F7}']
    function databaseScope: CKDatabaseScope; cdecl;
  end;
  TCKDatabaseNotification = class(TOCGenericImport<CKDatabaseNotificationClass, CKDatabaseNotification>) end;

  CKServerChangeTokenClass = interface(NSObjectClass)
    ['{9BB27907-0141-49B4-94BF-88AE1651F0CB}']
    {class} function new: Pointer; cdecl;
  end;

  CKServerChangeToken = interface(NSObject)
    ['{2EB4D200-4FF2-42EF-85A5-B60A1B8E76A0}']
  end;
  TCKServerChangeToken = class(TOCGenericImport<CKServerChangeTokenClass, CKServerChangeToken>) end;

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

  CKAcceptSharesOperationClass = interface(CKOperationClass)
    ['{82BFD875-2C77-43B4-82BD-0D2135B835CF}']
  end;

  CKAcceptSharesOperation = interface(CKOperation)
    ['{B176B57C-D2C3-411A-8D04-E058B5B885C2}']
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
    ['{1F80F894-C3AC-40A0-B522-F1AF323A11DC}']
  end;

  CKDiscoverAllUserIdentitiesOperation = interface(CKOperation)
    ['{7DB03875-3781-4E9B-B77B-50FFD6D97FF2}']
    function discoverAllUserIdentitiesCompletionBlock: TCKDiscoverAllUserIdentitiesOperationBlockMethod3; cdecl;
    procedure setDiscoverAllUserIdentitiesCompletionBlock(discoverAllUserIdentitiesCompletionBlock: TCKDiscoverAllUserIdentitiesOperationBlockMethod2); cdecl;
    procedure setUserIdentityDiscoveredBlock(userIdentityDiscoveredBlock: TCKDiscoverAllUserIdentitiesOperationBlockMethod2); cdecl;
    function userIdentityDiscoveredBlock: TCKDiscoverAllUserIdentitiesOperationBlockMethod1; cdecl;
  end;
  TCKDiscoverAllUserIdentitiesOperation = class(TOCGenericImport<CKDiscoverAllUserIdentitiesOperationClass, CKDiscoverAllUserIdentitiesOperation>) end;

  CKDiscoverUserIdentitiesOperationClass = interface(CKOperationClass)
    ['{E472FC4B-97F0-4CBB-8715-91E033FD99C0}']
  end;

  CKDiscoverUserIdentitiesOperation = interface(CKOperation)
    ['{3B20BAF9-3448-4FB8-9E47-40D0E221134A}']
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
    ['{D4A73F92-D785-45DC-874F-6064A67C02C0}']
  end;

  CKFetchDatabaseChangesOperation = interface(CKDatabaseOperation)
    ['{93809002-79D0-43C4-9114-4C6596B5FE17}']
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
    procedure setRecordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock(recordZoneWithIDWasDeletedDueToUserEncryptedDataResetBlock: TCKFetchDatabaseChangesOperationBlockMethod2); cdecl;
    procedure setRecordZoneWithIDWasPurgedBlock(recordZoneWithIDWasPurgedBlock: TCKFetchDatabaseChangesOperationBlockMethod2); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchDatabaseChangesOperation = class(TOCGenericImport<CKFetchDatabaseChangesOperationClass, CKFetchDatabaseChangesOperation>) end;

  CKFetchNotificationChangesOperationClass = interface(CKOperationClass)
    ['{1CBDDB14-09BC-4B7A-820D-C9FA864CCF34}']
  end;

  CKFetchNotificationChangesOperation = interface(CKOperation)
    ['{6C3F6C14-2B54-4599-8E8B-FC8E174593FA}']
    function fetchNotificationChangesCompletionBlock: TCKFetchNotificationChangesOperationBlockMethod3; cdecl;
    function initWithPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken): Pointer; cdecl;
    function moreComing: Boolean; cdecl;
    function notificationChangedBlock: TCKFetchNotificationChangesOperationBlockMethod1; cdecl;
    function previousServerChangeToken: CKServerChangeToken; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setFetchNotificationChangesCompletionBlock(fetchNotificationChangesCompletionBlock: TCKFetchNotificationChangesOperationBlockMethod2); cdecl;
    procedure setNotificationChangedBlock(notificationChangedBlock: TCKFetchNotificationChangesOperationBlockMethod2); cdecl;
    procedure setPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchNotificationChangesOperation = class(TOCGenericImport<CKFetchNotificationChangesOperationClass, CKFetchNotificationChangesOperation>) end;

  CKFetchRecordChangesOperationClass = interface(CKDatabaseOperationClass)
    ['{EFCDAB9D-AE8E-4168-B5AA-071E596B1014}']
  end;

  CKFetchRecordChangesOperation = interface(CKDatabaseOperation)
    ['{A27BB719-1E69-4D07-8E20-5EEACD816510}']
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
    ['{D2785739-0C08-4844-83EA-F57C8DA4A936}']
    {class} function fetchCurrentUserRecordOperation: Pointer; cdecl;
  end;

  CKFetchRecordsOperation = interface(CKDatabaseOperation)
    ['{5D847289-AB5F-4A6D-BDCA-9BAB6C7AF58E}']
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
    ['{C849A81F-36D3-4EB6-8CF8-CD4706C771E3}']
  end;

  CKFetchRecordZoneChangesOperation = interface(CKDatabaseOperation)
    ['{7636B1CE-D559-4732-B46C-AEEC6E40EE91}']
    function configurationsByRecordZoneID: NSDictionary; cdecl;
    function fetchAllChanges: Boolean; cdecl;
    function fetchRecordZoneChangesCompletionBlock: TCKFetchRecordZoneChangesOperationBlockMethod7; cdecl;
    function initWithRecordZoneIDs(recordZoneIDs: NSArray; configurationsByRecordZoneID: NSDictionary): Pointer; cdecl;
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
    ['{7AB13767-CD67-4957-8A04-2226C9E0A9FA}']
  end;

  CKFetchRecordZoneChangesConfiguration = interface(NSObject)
    ['{9585190C-DF9C-4B54-B3C6-4E332BA52188}']
    function desiredKeys: NSArray; cdecl;
    function previousServerChangeToken: CKServerChangeToken; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setDesiredKeys(desiredKeys: NSArray); cdecl;
    procedure setPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchRecordZoneChangesConfiguration = class(TOCGenericImport<CKFetchRecordZoneChangesConfigurationClass, CKFetchRecordZoneChangesConfiguration>) end;

  CKFetchRecordZoneChangesOptionsClass = interface(NSObjectClass)
    ['{299DB836-1C5A-4B53-A0E6-F7F097AF9137}']
  end;

  CKFetchRecordZoneChangesOptions = interface(NSObject)
    ['{9549C8F0-7D71-49FB-9B26-6B386FF53259}']
    function desiredKeys: NSArray; cdecl;
    function previousServerChangeToken: CKServerChangeToken; cdecl;
    function resultsLimit: NSUInteger; cdecl;
    procedure setDesiredKeys(desiredKeys: NSArray); cdecl;
    procedure setPreviousServerChangeToken(previousServerChangeToken: CKServerChangeToken); cdecl;
    procedure setResultsLimit(resultsLimit: NSUInteger); cdecl;
  end;
  TCKFetchRecordZoneChangesOptions = class(TOCGenericImport<CKFetchRecordZoneChangesOptionsClass, CKFetchRecordZoneChangesOptions>) end;

  CKFetchRecordZonesOperationClass = interface(CKDatabaseOperationClass)
    ['{C3B2557D-DA1A-4F85-B65B-1B8FABEE5A84}']
    {class} function fetchAllRecordZonesOperation: Pointer; cdecl;
  end;

  CKFetchRecordZonesOperation = interface(CKDatabaseOperation)
    ['{6404BFA9-F761-4CD6-AE78-F43A3B32AA52}']
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
    ['{F79F38BC-FA8C-4EC9-85B4-6C0468F44AB3}']
  end;

  CKFetchShareMetadataOperation = interface(CKOperation)
    ['{57D700AE-8826-4B9F-A714-680D95360736}']
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
    ['{91FD2009-E2C5-47E5-973B-093F77B692B4}']
  end;

  CKFetchShareParticipantsOperation = interface(CKOperation)
    ['{0C73E413-947D-4486-84BE-38956E5F9D0D}']
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
    ['{443DDF2D-8683-4418-85C8-A9709CEE73BA}']
    {class} function fetchAllSubscriptionsOperation: Pointer; cdecl;
  end;

  CKFetchSubscriptionsOperation = interface(CKDatabaseOperation)
    ['{BBA7AF79-77CF-4192-BD87-D7914F585C5D}']
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
    ['{BD2A5CDA-69AA-4CB6-82DB-5820F9854652}']
  end;

  CKFetchWebAuthTokenOperation = interface(CKDatabaseOperation)
    ['{79B970B5-4B8A-4B67-813B-26CB62B94695}']
    function APIToken: NSString; cdecl;
    function fetchWebAuthTokenCompletionBlock: TCKFetchWebAuthTokenOperationBlockMethod1; cdecl;
    function initWithAPIToken(APIToken: NSString): Pointer; cdecl;
    procedure setAPIToken(APIToken: NSString); cdecl;
    procedure setFetchWebAuthTokenCompletionBlock(fetchWebAuthTokenCompletionBlock: TCKFetchWebAuthTokenOperationBlockMethod2); cdecl;
  end;
  TCKFetchWebAuthTokenOperation = class(TOCGenericImport<CKFetchWebAuthTokenOperationClass, CKFetchWebAuthTokenOperation>) end;

  CKMarkNotificationsReadOperationClass = interface(CKOperationClass)
    ['{0FA66587-A560-45CC-8E0F-EE8400981601}']
  end;

  CKMarkNotificationsReadOperation = interface(CKOperation)
    ['{473D2DD2-3FE5-4CA0-8711-8953AA02F513}']
    function initWithNotificationIDsToMarkRead(notificationIDs: NSArray): Pointer; cdecl;
    function markNotificationsReadCompletionBlock: TCKMarkNotificationsReadOperationBlockMethod1; cdecl;
    function notificationIDs: NSArray; cdecl;
    procedure setMarkNotificationsReadCompletionBlock(markNotificationsReadCompletionBlock: TCKMarkNotificationsReadOperationBlockMethod2); cdecl;
    procedure setNotificationIDs(notificationIDs: NSArray); cdecl;
  end;
  TCKMarkNotificationsReadOperation = class(TOCGenericImport<CKMarkNotificationsReadOperationClass, CKMarkNotificationsReadOperation>) end;

  CKModifyBadgeOperationClass = interface(CKOperationClass)
    ['{C44F2BEC-8F85-4939-89D3-E46AE8071AAB}']
  end;

  CKModifyBadgeOperation = interface(CKOperation)
    ['{41E9C50A-3A21-4FFA-9EE8-3FA7A200E2DC}']
    function badgeValue: NSUInteger; cdecl;
    function initWithBadgeValue(badgeValue: NSUInteger): Pointer; cdecl;
    function modifyBadgeCompletionBlock: TCKModifyBadgeOperationBlockMethod1; cdecl;
    procedure setBadgeValue(badgeValue: NSUInteger); cdecl;
    procedure setModifyBadgeCompletionBlock(modifyBadgeCompletionBlock: TCKModifyBadgeOperationBlockMethod2); cdecl;
  end;
  TCKModifyBadgeOperation = class(TOCGenericImport<CKModifyBadgeOperationClass, CKModifyBadgeOperation>) end;

  CKModifyRecordsOperationClass = interface(CKDatabaseOperationClass)
    ['{8F62B4B0-66F2-46EA-9EE8-3178890E1F5A}']
  end;

  CKModifyRecordsOperation = interface(CKDatabaseOperation)
    ['{09A8D078-1A29-45C3-A15B-68DC6F302975}']
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
    ['{95FE11F6-159F-4559-987C-45DEE8CEA36B}']
  end;

  CKModifyRecordZonesOperation = interface(CKDatabaseOperation)
    ['{C9F380C0-83CA-4B34-94B0-CAB68205CFA4}']
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
    ['{3652B00B-1A3C-4504-A31B-049780E27801}']
  end;

  CKModifySubscriptionsOperation = interface(CKDatabaseOperation)
    ['{0ED713A9-15E4-4A19-8315-AAAD3A2D674D}']
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

  CKQueryCursorClass = interface(NSObjectClass)
    ['{558ABB8D-A2A1-4915-A3A2-FABC93356A78}']
    {class} function new: Pointer; cdecl;
  end;

  CKQueryCursor = interface(NSObject)
    ['{7D245A12-DE73-465A-8491-4F1BDEC3E072}']
  end;
  TCKQueryCursor = class(TOCGenericImport<CKQueryCursorClass, CKQueryCursor>) end;

  CKQueryOperationClass = interface(CKDatabaseOperationClass)
    ['{E32375E0-663E-4628-9D22-052AA09EEDB3}']
  end;

  CKQueryOperation = interface(CKDatabaseOperation)
    ['{849FFD76-18BA-4059-B54A-23F6C0D94560}']
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
    ['{AABE7544-D1E8-435E-91E7-CC06C9B1EDA4}']
    {class} function standardOptions: CKAllowedSharingOptions; cdecl;
  end;

  CKAllowedSharingOptions = interface(NSObject)
    ['{D431BE8E-B5EF-4B84-B973-BCB763876B61}']
    function allowedParticipantAccessOptions: CKSharingParticipantAccessOption; cdecl;
    function allowedParticipantPermissionOptions: CKSharingParticipantPermissionOption; cdecl;
    function initWithAllowedParticipantPermissionOptions(allowedParticipantPermissionOptions: CKSharingParticipantPermissionOption;
      allowedParticipantAccessOptions: CKSharingParticipantAccessOption): Pointer; cdecl;
    procedure setAllowedParticipantAccessOptions(allowedParticipantAccessOptions: CKSharingParticipantAccessOption); cdecl;
    procedure setAllowedParticipantPermissionOptions(allowedParticipantPermissionOptions: CKSharingParticipantPermissionOption); cdecl;
  end;
  TCKAllowedSharingOptions = class(TOCGenericImport<CKAllowedSharingOptionsClass, CKAllowedSharingOptions>) end;

  CKSystemSharingUIObserverClass = interface(NSObjectClass)
    ['{2E251582-7925-4892-8551-058996864D5D}']
    {class} function new: Pointer; cdecl;
  end;

  CKSystemSharingUIObserver = interface(NSObject)
    ['{626B8B59-07B6-42E5-A293-35D7489516B4}']
    function initWithContainer(container: CKContainer): Pointer; cdecl;
    procedure setSystemSharingUIDidSaveShareBlock(systemSharingUIDidSaveShareBlock: TCKSystemSharingUIObserverBlockMethod2); cdecl;
    procedure setSystemSharingUIDidStopSharingBlock(systemSharingUIDidStopSharingBlock: TCKSystemSharingUIObserverBlockMethod2); cdecl;
    function systemSharingUIDidSaveShareBlock: TCKSystemSharingUIObserverBlockMethod1; cdecl;
    function systemSharingUIDidStopSharingBlock: TCKSystemSharingUIObserverBlockMethod3; cdecl;
  end;
  TCKSystemSharingUIObserver = class(TOCGenericImport<CKSystemSharingUIObserverClass, CKSystemSharingUIObserver>) end;

  CKSyncEngineStateClass = interface(NSObjectClass)
    ['{DCD6B5D8-EECF-4E94-955B-E676E0DA4745}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineState = interface(NSObject)
    ['{2D9A76AA-040B-464D-A311-74D756E5BBE5}']
    procedure addPendingDatabaseChanges(changes: NSArray); cdecl;
    procedure addPendingRecordZoneChanges(changes: NSArray); cdecl;
    function hasPendingUntrackedChanges: Boolean; cdecl;
    function pendingDatabaseChanges: NSArray; cdecl;
    function pendingRecordZoneChanges: NSArray; cdecl;
    procedure removePendingDatabaseChanges(changes: NSArray); cdecl;
    procedure removePendingRecordZoneChanges(changes: NSArray); cdecl;
    procedure setHasPendingUntrackedChanges(hasPendingUntrackedChanges: Boolean); cdecl;
    function zoneIDsWithUnfetchedServerChanges: NSArray; cdecl;
  end;
  TCKSyncEngineState = class(TOCGenericImport<CKSyncEngineStateClass, CKSyncEngineState>) end;

  CKSyncEngineStateSerializationClass = interface(NSObjectClass)
    ['{348D8450-50A2-4961-B7DF-7F8EF50E1FF5}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineStateSerialization = interface(NSObject)
    ['{61338EB9-4B4C-4159-BF1F-04DA4F8DA1EB}']
  end;
  TCKSyncEngineStateSerialization = class(TOCGenericImport<CKSyncEngineStateSerializationClass, CKSyncEngineStateSerialization>) end;

  CKSyncEnginePendingRecordZoneChangeClass = interface(NSObjectClass)
    ['{1EF10D6B-357B-4A06-9068-4E23860B24C8}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEnginePendingRecordZoneChange = interface(NSObject)
    ['{A46E4C25-F7BB-442D-AD26-E7AB444723B9}']
    function &type: CKSyncEnginePendingRecordZoneChangeType; cdecl;
    function initWithRecordID(recordID: CKRecordID; &type: CKSyncEnginePendingRecordZoneChangeType): Pointer; cdecl;
    function recordID: CKRecordID; cdecl;
  end;
  TCKSyncEnginePendingRecordZoneChange = class(TOCGenericImport<CKSyncEnginePendingRecordZoneChangeClass, CKSyncEnginePendingRecordZoneChange>) end;

  CKSyncEnginePendingDatabaseChangeClass = interface(NSObjectClass)
    ['{940656FA-32BB-48EE-B115-B73E1882AECB}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEnginePendingDatabaseChange = interface(NSObject)
    ['{A526C74A-3CF0-457A-BA7A-9EE280EEA184}']
    function &type: CKSyncEnginePendingDatabaseChangeType; cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKSyncEnginePendingDatabaseChange = class(TOCGenericImport<CKSyncEnginePendingDatabaseChangeClass, CKSyncEnginePendingDatabaseChange>) end;

  CKSyncEnginePendingZoneSaveClass = interface(CKSyncEnginePendingDatabaseChangeClass)
    ['{60B4AA1B-23B6-4391-989B-F056080A94D7}']
  end;

  CKSyncEnginePendingZoneSave = interface(CKSyncEnginePendingDatabaseChange)
    ['{B156AEFD-901D-4597-80CB-25D452936CA3}']
    function initWithZone(zone: CKRecordZone): Pointer; cdecl;
    function zone: CKRecordZone; cdecl;
  end;
  TCKSyncEnginePendingZoneSave = class(TOCGenericImport<CKSyncEnginePendingZoneSaveClass, CKSyncEnginePendingZoneSave>) end;

  CKSyncEnginePendingZoneDeleteClass = interface(CKSyncEnginePendingDatabaseChangeClass)
    ['{1A869EC0-AE73-4E12-893A-A6D0ED8C4D7B}']
  end;

  CKSyncEnginePendingZoneDelete = interface(CKSyncEnginePendingDatabaseChange)
    ['{A475CC1C-3D30-4C9A-BDD8-EA0D2B012593}']
    function initWithZoneID(zoneID: CKRecordZoneID): Pointer; cdecl;
  end;
  TCKSyncEnginePendingZoneDelete = class(TOCGenericImport<CKSyncEnginePendingZoneDeleteClass, CKSyncEnginePendingZoneDelete>) end;

  CKSyncEngineRecordZoneChangeBatchClass = interface(NSObjectClass)
    ['{EB042411-614B-4B29-9C73-C552E4890CF0}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineRecordZoneChangeBatch = interface(NSObject)
    ['{0A005578-C87F-415D-8387-340746356D8E}']
    function atomicByZone: Boolean; cdecl;
    function initWithPendingChanges(pendingChanges: NSArray; recordProvider: TCKSyncEngineRecordZoneChangeBatchBlockMethod1): Pointer; cdecl;
    function initWithRecordsToSave(recordsToSave: NSArray; recordIDsToDelete: NSArray; atomicByZone: Boolean): Pointer; cdecl;
    function recordIDsToDelete: NSArray; cdecl;
    function recordsToSave: NSArray; cdecl;
    procedure setAtomicByZone(atomicByZone: Boolean); cdecl;
  end;
  TCKSyncEngineRecordZoneChangeBatch = class(TOCGenericImport<CKSyncEngineRecordZoneChangeBatchClass, CKSyncEngineRecordZoneChangeBatch>) end;

  CKSyncEngineClass = interface(NSObjectClass)
    ['{A28631F2-A444-4647-99BA-24A2A0BE3B9F}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngine = interface(NSObject)
    ['{1F836162-EE8E-4B42-8B90-F6874A4830D0}']
    procedure cancelOperationsWithCompletionHandler(completionHandler: TCKSyncEngineBlockMethod2); cdecl;
    function database: CKDatabase; cdecl;
    procedure fetchChangesWithCompletionHandler(completionHandler: TCKSyncEngineBlockMethod1); cdecl;
    procedure fetchChangesWithOptions(options: CKSyncEngineFetchChangesOptions; completionHandler: TCKSyncEngineBlockMethod1); cdecl;
    function initWithConfiguration(configuration: CKSyncEngineConfiguration): Pointer; cdecl;
    procedure sendChangesWithCompletionHandler(completionHandler: TCKSyncEngineBlockMethod1); cdecl;
    procedure sendChangesWithOptions(options: CKSyncEngineSendChangesOptions; completionHandler: TCKSyncEngineBlockMethod1); cdecl;
    function state: CKSyncEngineState; cdecl;
  end;
  TCKSyncEngine = class(TOCGenericImport<CKSyncEngineClass, CKSyncEngine>) end;

  CKSyncEngineDelegate = interface(IObjectiveC)
    ['{F34B4D10-BF91-4EA1-A394-E1F740566DE7}']
    function syncEngine(syncEngine: CKSyncEngine;
      nextFetchChangesOptionsForContext: CKSyncEngineFetchChangesContext): CKSyncEngineFetchChangesOptions; overload; cdecl;
    function syncEngine(syncEngine: CKSyncEngine;
      nextRecordZoneChangeBatchForContext: CKSyncEngineSendChangesContext): CKSyncEngineRecordZoneChangeBatch; overload; cdecl;
    procedure syncEngine(syncEngine: CKSyncEngine; handleEvent: CKSyncEngineEvent); overload; cdecl;
  end;

  CKSyncEngineFetchChangesOptionsClass = interface(NSObjectClass)
    ['{F0F18CE4-CD8B-4E31-BCCD-DB4B268E92DE}']
  end;

  CKSyncEngineFetchChangesOptions = interface(NSObject)
    ['{2BAB5D27-F95C-4913-BA7E-895A691442CF}']
    function initWithScope(scope: CKSyncEngineFetchChangesScope): Pointer; cdecl;
    function operationGroup: CKOperationGroup; cdecl;
    function prioritizedZoneIDs: NSArray; cdecl;
    function scope: CKSyncEngineFetchChangesScope; cdecl;
    procedure setOperationGroup(operationGroup: CKOperationGroup); cdecl;
    procedure setPrioritizedZoneIDs(prioritizedZoneIDs: NSArray); cdecl;
    procedure setScope(scope: CKSyncEngineFetchChangesScope); cdecl;
  end;
  TCKSyncEngineFetchChangesOptions = class(TOCGenericImport<CKSyncEngineFetchChangesOptionsClass, CKSyncEngineFetchChangesOptions>) end;

  CKSyncEngineFetchChangesScopeClass = interface(NSObjectClass)
    ['{6C9031E3-FF12-44E3-B6C7-72BFB776297D}']
  end;

  CKSyncEngineFetchChangesScope = interface(NSObject)
    ['{65E299AA-B6C3-4DAC-9220-8E82EAB64075}']
    function containsZoneID(zoneID: CKRecordZoneID): Boolean; cdecl;
    function excludedZoneIDs: NSSet; cdecl;
    function initWithExcludedZoneIDs(zoneIDs: NSSet): Pointer; cdecl;
    function initWithZoneIDs(zoneIDs: NSSet): Pointer; cdecl;
    function zoneIDs: NSSet; cdecl;
  end;
  TCKSyncEngineFetchChangesScope = class(TOCGenericImport<CKSyncEngineFetchChangesScopeClass, CKSyncEngineFetchChangesScope>) end;

  CKSyncEngineSendChangesOptionsClass = interface(NSObjectClass)
    ['{5FBF6A08-97A0-4B24-BA5B-3E183E40162B}']
  end;

  CKSyncEngineSendChangesOptions = interface(NSObject)
    ['{54608F09-45D5-4BD8-AED2-28119492870C}']
    function initWithScope(scope: CKSyncEngineSendChangesScope): Pointer; cdecl;
    function operationGroup: CKOperationGroup; cdecl;
    function scope: CKSyncEngineSendChangesScope; cdecl;
    procedure setOperationGroup(operationGroup: CKOperationGroup); cdecl;
    procedure setScope(scope: CKSyncEngineSendChangesScope); cdecl;
  end;
  TCKSyncEngineSendChangesOptions = class(TOCGenericImport<CKSyncEngineSendChangesOptionsClass, CKSyncEngineSendChangesOptions>) end;

  CKSyncEngineSendChangesScopeClass = interface(NSObjectClass)
    ['{8F103DC2-F55F-46DC-9E8B-66F217931347}']
  end;

  CKSyncEngineSendChangesScope = interface(NSObject)
    ['{7859C6A6-56BC-44CA-954D-C14C662BF5FD}']
    function containsPendingRecordZoneChange(pendingRecordZoneChange: CKSyncEnginePendingRecordZoneChange): Boolean; cdecl;
    function containsRecordID(recordID: CKRecordID): Boolean; cdecl;
    function excludedZoneIDs: NSSet; cdecl;
    function initWithExcludedZoneIDs(excludedZoneIDs: NSSet): Pointer; cdecl;
    function initWithRecordIDs(recordIDs: NSSet): Pointer; cdecl;
    function initWithZoneIDs(zoneIDs: NSSet): Pointer; cdecl;
    function recordIDs: NSSet; cdecl;
    function zoneIDs: NSSet; cdecl;
  end;
  TCKSyncEngineSendChangesScope = class(TOCGenericImport<CKSyncEngineSendChangesScopeClass, CKSyncEngineSendChangesScope>) end;

  CKSyncEngineFetchChangesContextClass = interface(NSObjectClass)
    ['{D681B6D3-E5CE-45FA-AF9B-BC4B640803F2}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineFetchChangesContext = interface(NSObject)
    ['{D4FD2289-4188-429A-BD75-3E167AD96E75}']
    function options: CKSyncEngineFetchChangesOptions; cdecl;
    function reason: CKSyncEngineSyncReason; cdecl;
  end;
  TCKSyncEngineFetchChangesContext = class(TOCGenericImport<CKSyncEngineFetchChangesContextClass, CKSyncEngineFetchChangesContext>) end;

  CKSyncEngineSendChangesContextClass = interface(NSObjectClass)
    ['{62F71CE5-A272-43C8-973E-B87B1E92629F}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineSendChangesContext = interface(NSObject)
    ['{71CC4DF5-F5ED-4015-BBE6-8949C799CD7A}']
    function options: CKSyncEngineSendChangesOptions; cdecl;
    function reason: CKSyncEngineSyncReason; cdecl;
  end;
  TCKSyncEngineSendChangesContext = class(TOCGenericImport<CKSyncEngineSendChangesContextClass, CKSyncEngineSendChangesContext>) end;

  CKSyncEngineConfigurationClass = interface(NSObjectClass)
    ['{D4D7D48A-B834-45FB-A69D-9ED43F88A022}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineConfiguration = interface(NSObject)
    ['{443036CD-11ED-430D-8013-BF796F3B11F9}']
    function automaticallySync: Boolean; cdecl;
    function database: CKDatabase; cdecl;
    function delegate: Pointer; cdecl;
    function initWithDatabase(database: CKDatabase; stateSerialization: CKSyncEngineStateSerialization; delegate: Pointer): Pointer; cdecl;
    procedure setAutomaticallySync(automaticallySync: Boolean); cdecl;
    procedure setDatabase(database: CKDatabase); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setStateSerialization(stateSerialization: CKSyncEngineStateSerialization); cdecl;
    procedure setSubscriptionID(subscriptionID: CKSubscriptionID); cdecl;
    function stateSerialization: CKSyncEngineStateSerialization; cdecl;
    function subscriptionID: CKSubscriptionID; cdecl;
  end;
  TCKSyncEngineConfiguration = class(TOCGenericImport<CKSyncEngineConfigurationClass, CKSyncEngineConfiguration>) end;

  CKSyncEngineEventClass = interface(NSObjectClass)
    ['{C0F78579-6706-4AB6-8660-1701ABF0D6D6}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineEvent = interface(NSObject)
    ['{49BB902F-2F44-44BB-ACD5-2EA583C9E548}']
    function &type: CKSyncEngineEventType; cdecl;
    function accountChangeEvent: CKSyncEngineAccountChangeEvent; cdecl;
    function didFetchChangesEvent: CKSyncEngineDidFetchChangesEvent; cdecl;
    function didFetchRecordZoneChangesEvent: CKSyncEngineDidFetchRecordZoneChangesEvent; cdecl;
    function didSendChangesEvent: CKSyncEngineDidSendChangesEvent; cdecl;
    function fetchedDatabaseChangesEvent: CKSyncEngineFetchedDatabaseChangesEvent; cdecl;
    function fetchedRecordZoneChangesEvent: CKSyncEngineFetchedRecordZoneChangesEvent; cdecl;
    function sentDatabaseChangesEvent: CKSyncEngineSentDatabaseChangesEvent; cdecl;
    function sentRecordZoneChangesEvent: CKSyncEngineSentRecordZoneChangesEvent; cdecl;
    function stateUpdateEvent: CKSyncEngineStateUpdateEvent; cdecl;
    function willFetchChangesEvent: CKSyncEngineWillFetchChangesEvent; cdecl;
    function willFetchRecordZoneChangesEvent: CKSyncEngineWillFetchRecordZoneChangesEvent; cdecl;
    function willSendChangesEvent: CKSyncEngineWillSendChangesEvent; cdecl;
  end;
  TCKSyncEngineEvent = class(TOCGenericImport<CKSyncEngineEventClass, CKSyncEngineEvent>) end;

  CKSyncEngineStateUpdateEventClass = interface(CKSyncEngineEventClass)
    ['{124445A8-E335-45B3-878A-F7082FE1FCA8}']
  end;

  CKSyncEngineStateUpdateEvent = interface(CKSyncEngineEvent)
    ['{C31A281D-CD8F-4796-AB9E-555C5DC87ECD}']
    function stateSerialization: CKSyncEngineStateSerialization; cdecl;
  end;
  TCKSyncEngineStateUpdateEvent = class(TOCGenericImport<CKSyncEngineStateUpdateEventClass, CKSyncEngineStateUpdateEvent>) end;

  CKSyncEngineAccountChangeEventClass = interface(CKSyncEngineEventClass)
    ['{972B6605-C113-4F87-9C48-59B58EA27150}']
  end;

  CKSyncEngineAccountChangeEvent = interface(CKSyncEngineEvent)
    ['{C0A82742-312C-4B26-9B8B-2A299478562F}']
    function changeType: CKSyncEngineAccountChangeType; cdecl;
    function currentUser: CKRecordID; cdecl;
    function previousUser: CKRecordID; cdecl;
  end;
  TCKSyncEngineAccountChangeEvent = class(TOCGenericImport<CKSyncEngineAccountChangeEventClass, CKSyncEngineAccountChangeEvent>) end;

  CKSyncEngineFetchedDatabaseChangesEventClass = interface(CKSyncEngineEventClass)
    ['{E499568D-D087-4CA8-8174-2DA7D800631A}']
  end;

  CKSyncEngineFetchedDatabaseChangesEvent = interface(CKSyncEngineEvent)
    ['{3DA2A67A-2752-4E5F-A048-161B895548EE}']
    function deletions: NSArray; cdecl;
    function modifications: NSArray; cdecl;
  end;
  TCKSyncEngineFetchedDatabaseChangesEvent = class(TOCGenericImport<CKSyncEngineFetchedDatabaseChangesEventClass, CKSyncEngineFetchedDatabaseChangesEvent>) end;

  CKSyncEngineFetchedRecordZoneChangesEventClass = interface(CKSyncEngineEventClass)
    ['{02835BE8-3DDB-45C1-95C0-96B563D011B4}']
  end;

  CKSyncEngineFetchedRecordZoneChangesEvent = interface(CKSyncEngineEvent)
    ['{50B68332-26DA-4CCB-B3D9-6313921B84BA}']
    function deletions: NSArray; cdecl;
    function modifications: NSArray; cdecl;
  end;
  TCKSyncEngineFetchedRecordZoneChangesEvent = class(TOCGenericImport<CKSyncEngineFetchedRecordZoneChangesEventClass, CKSyncEngineFetchedRecordZoneChangesEvent>) end;

  CKSyncEngineSentDatabaseChangesEventClass = interface(CKSyncEngineEventClass)
    ['{EB17B94E-4C94-4F3E-9D4D-67E0BC26871C}']
  end;

  CKSyncEngineSentDatabaseChangesEvent = interface(CKSyncEngineEvent)
    ['{F175B39D-1FB0-4108-81A1-DAD54C65C52C}']
    function deletedZoneIDs: NSArray; cdecl;
    function failedZoneDeletes: NSDictionary; cdecl;
    function failedZoneSaves: NSArray; cdecl;
    function savedZones: NSArray; cdecl;
  end;
  TCKSyncEngineSentDatabaseChangesEvent = class(TOCGenericImport<CKSyncEngineSentDatabaseChangesEventClass, CKSyncEngineSentDatabaseChangesEvent>) end;

  CKSyncEngineSentRecordZoneChangesEventClass = interface(CKSyncEngineEventClass)
    ['{A0CCDA49-CD73-4FDB-921D-8F733226DAFB}']
  end;

  CKSyncEngineSentRecordZoneChangesEvent = interface(CKSyncEngineEvent)
    ['{043725A3-685A-46F1-99ED-3F8E3D4C5D0A}']
    function deletedRecordIDs: NSArray; cdecl;
    function failedRecordDeletes: NSDictionary; cdecl;
    function failedRecordSaves: NSArray; cdecl;
    function savedRecords: NSArray; cdecl;
  end;
  TCKSyncEngineSentRecordZoneChangesEvent = class(TOCGenericImport<CKSyncEngineSentRecordZoneChangesEventClass, CKSyncEngineSentRecordZoneChangesEvent>) end;

  CKSyncEngineWillFetchChangesEventClass = interface(CKSyncEngineEventClass)
    ['{901AA06D-881D-45B6-8CAB-6A8799B06A42}']
  end;

  CKSyncEngineWillFetchChangesEvent = interface(CKSyncEngineEvent)
    ['{8AD4F72D-0091-4CA2-852C-528D8E4D0156}']
    function context: CKSyncEngineFetchChangesContext; cdecl;
  end;
  TCKSyncEngineWillFetchChangesEvent = class(TOCGenericImport<CKSyncEngineWillFetchChangesEventClass, CKSyncEngineWillFetchChangesEvent>) end;

  CKSyncEngineWillFetchRecordZoneChangesEventClass = interface(CKSyncEngineEventClass)
    ['{CA078F02-1111-4FFA-B207-FEBAB2F30AB7}']
  end;

  CKSyncEngineWillFetchRecordZoneChangesEvent = interface(CKSyncEngineEvent)
    ['{35D2464A-EC26-413E-B7D5-0275A527710A}']
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKSyncEngineWillFetchRecordZoneChangesEvent = class(TOCGenericImport<CKSyncEngineWillFetchRecordZoneChangesEventClass, CKSyncEngineWillFetchRecordZoneChangesEvent>) end;

  CKSyncEngineDidFetchRecordZoneChangesEventClass = interface(CKSyncEngineEventClass)
    ['{1A962532-17A4-4A69-94FD-5AD11D2B5102}']
  end;

  CKSyncEngineDidFetchRecordZoneChangesEvent = interface(CKSyncEngineEvent)
    ['{6BB0C307-CA11-47EA-A3BA-FED4F043D801}']
    function error: NSError; cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKSyncEngineDidFetchRecordZoneChangesEvent = class(TOCGenericImport<CKSyncEngineDidFetchRecordZoneChangesEventClass, CKSyncEngineDidFetchRecordZoneChangesEvent>) end;

  CKSyncEngineDidFetchChangesEventClass = interface(CKSyncEngineEventClass)
    ['{FAC900C7-180F-4DCB-9623-83F8DDC62873}']
  end;

  CKSyncEngineDidFetchChangesEvent = interface(CKSyncEngineEvent)
    ['{CB66A58A-A83E-4D86-9751-09A3C9728BAF}']
    function context: CKSyncEngineFetchChangesContext; cdecl;
  end;
  TCKSyncEngineDidFetchChangesEvent = class(TOCGenericImport<CKSyncEngineDidFetchChangesEventClass, CKSyncEngineDidFetchChangesEvent>) end;

  CKSyncEngineWillSendChangesEventClass = interface(CKSyncEngineEventClass)
    ['{1ABBD134-B17B-41C0-8624-2CF0198A1EC5}']
  end;

  CKSyncEngineWillSendChangesEvent = interface(CKSyncEngineEvent)
    ['{17B794B0-A186-41B5-8DF5-42DE9AF4258E}']
    function context: CKSyncEngineSendChangesContext; cdecl;
  end;
  TCKSyncEngineWillSendChangesEvent = class(TOCGenericImport<CKSyncEngineWillSendChangesEventClass, CKSyncEngineWillSendChangesEvent>) end;

  CKSyncEngineDidSendChangesEventClass = interface(CKSyncEngineEventClass)
    ['{90A21C3D-B5B5-4BE8-BC48-91E6064F8AC9}']
  end;

  CKSyncEngineDidSendChangesEvent = interface(CKSyncEngineEvent)
    ['{B6B4D3C1-9248-453D-9D55-9CDC463F39C9}']
    function context: CKSyncEngineSendChangesContext; cdecl;
  end;
  TCKSyncEngineDidSendChangesEvent = class(TOCGenericImport<CKSyncEngineDidSendChangesEventClass, CKSyncEngineDidSendChangesEvent>) end;

  CKSyncEngineFetchedRecordDeletionClass = interface(NSObjectClass)
    ['{014A1E56-302B-4289-8417-F15AF0C11348}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineFetchedRecordDeletion = interface(NSObject)
    ['{F7D85E59-7910-43ED-B429-6FD2883114C8}']
    function recordID: CKRecordID; cdecl;
    function recordType: CKRecordType; cdecl;
  end;
  TCKSyncEngineFetchedRecordDeletion = class(TOCGenericImport<CKSyncEngineFetchedRecordDeletionClass, CKSyncEngineFetchedRecordDeletion>) end;

  CKSyncEngineFetchedZoneDeletionClass = interface(NSObjectClass)
    ['{3A77EC6C-95B8-4715-BF47-59305181DD6F}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineFetchedZoneDeletion = interface(NSObject)
    ['{BB7DD90E-FD09-419D-BEEC-DA871F333978}']
    function reason: CKSyncEngineZoneDeletionReason; cdecl;
    function zoneID: CKRecordZoneID; cdecl;
  end;
  TCKSyncEngineFetchedZoneDeletion = class(TOCGenericImport<CKSyncEngineFetchedZoneDeletionClass, CKSyncEngineFetchedZoneDeletion>) end;

  CKSyncEngineFailedRecordSaveClass = interface(NSObjectClass)
    ['{75134994-FB19-42DD-9BB3-4A2339D67F0D}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineFailedRecordSave = interface(NSObject)
    ['{4F3FFB89-0AEA-4332-B865-E55F3CBB8586}']
    function &record: CKRecord; cdecl;
    function error: NSError; cdecl;
  end;
  TCKSyncEngineFailedRecordSave = class(TOCGenericImport<CKSyncEngineFailedRecordSaveClass, CKSyncEngineFailedRecordSave>) end;

  CKSyncEngineFailedZoneSaveClass = interface(NSObjectClass)
    ['{DFE9CCF2-CFAE-4C20-8596-EC6DC5757D0F}']
    {class} function new: Pointer; cdecl;
  end;

  CKSyncEngineFailedZoneSave = interface(NSObject)
    ['{2D939809-CADD-431A-8120-9773FBAFB188}']
    function error: NSError; cdecl;
    function recordZone: CKRecordZone; cdecl;
  end;
  TCKSyncEngineFailedZoneSave = class(TOCGenericImport<CKSyncEngineFailedZoneSaveClass, CKSyncEngineFailedZoneSave>) end;

implementation

end.