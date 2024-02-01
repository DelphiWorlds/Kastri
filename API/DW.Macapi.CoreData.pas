unit DW.Macapi.CoreData;

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

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.CocoaTypes, Macapi.Foundation,
  // DW
  DW.Macapi.Foundation, DW.Macapi.CloudKit, DW.Macapi.CoreSpotlight;

const
  NSCoreDataVersionNumber10_4 = 46.0;
  NSCoreDataVersionNumber10_4_3 = 77.0;
  NSCoreDataVersionNumber10_5 = 185.0;
  NSCoreDataVersionNumber10_5_3 = 186.0;
  NSCoreDataVersionNumber10_6 = 246.0;
  NSCoreDataVersionNumber10_6_2 = 250.0;
  NSCoreDataVersionNumber10_6_3 = 251.0;
  NSCoreDataVersionNumber10_7 = 358.4;
  NSCoreDataVersionNumber10_7_2 = 358.12;
  NSCoreDataVersionNumber10_7_3 = 358.13;
  NSCoreDataVersionNumber10_7_4 = 358.14;
  NSCoreDataVersionNumber10_8 = 407.5;
  NSCoreDataVersionNumber10_8_2 = 407.7;
  NSCoreDataVersionNumber10_9 = 481.0;
  NSCoreDataVersionNumber10_9_2 = 481.1;
  NSCoreDataVersionNumber10_9_3 = 481.3;
  NSCoreDataVersionNumber10_10 = 526.0;
  NSCoreDataVersionNumber10_10_2 = 526.1;
  NSCoreDataVersionNumber10_10_3 = 526.2;
  NSCoreDataVersionNumber10_11 = 640.0;
  NSCoreDataVersionNumber10_11_3 = 641.3;
  NSCoreDataVersionNumber_iPhoneOS_3_0 = 241.0;
  NSCoreDataVersionNumber_iPhoneOS_3_1 = 248.0;
  NSCoreDataVersionNumber_iPhoneOS_3_2 = 310.2;
  NSCoreDataVersionNumber_iPhoneOS_4_0 = 320.5;
  NSCoreDataVersionNumber_iPhoneOS_4_1 = 320.11;
  NSCoreDataVersionNumber_iPhoneOS_4_2 = 320.15;
  NSCoreDataVersionNumber_iPhoneOS_4_3 = 320.17;
  NSCoreDataVersionNumber_iPhoneOS_5_0 = 386.1;
  NSCoreDataVersionNumber_iPhoneOS_5_1 = 386.5;
  NSCoreDataVersionNumber_iPhoneOS_6_0 = 419.0;
  NSCoreDataVersionNumber_iPhoneOS_6_1 = 419.1;
  NSCoreDataVersionNumber_iPhoneOS_7_0 = 479.1;
  NSCoreDataVersionNumber_iPhoneOS_7_1 = 479.3;
  NSCoreDataVersionNumber_iPhoneOS_8_0 = 519.0;
  NSCoreDataVersionNumber_iPhoneOS_8_3 = 519.15;
  NSCoreDataVersionNumber_iPhoneOS_9_0 = 640.0;
  NSCoreDataVersionNumber_iPhoneOS_9_2 = 641.4;
  NSCoreDataVersionNumber_iPhoneOS_9_3 = 641.6;
  NSManagedObjectValidationError = 1550;
  NSManagedObjectConstraintValidationError = 1551;
  NSValidationMultipleErrorsError = 1560;
  NSValidationMissingMandatoryPropertyError = 1570;
  NSValidationRelationshipLacksMinimumCountError = 1580;
  NSValidationRelationshipExceedsMaximumCountError = 1590;
  NSValidationRelationshipDeniedDeleteError = 1600;
  NSValidationNumberTooLargeError = 1610;
  NSValidationNumberTooSmallError = 1620;
  NSValidationDateTooLateError = 1630;
  NSValidationDateTooSoonError = 1640;
  NSValidationInvalidDateError = 1650;
  NSValidationStringTooLongError = 1660;
  NSValidationStringTooShortError = 1670;
  NSValidationStringPatternMatchingError = 1680;
  NSValidationInvalidURIError = 1690;
  NSManagedObjectContextLockingError = 132000;
  NSPersistentStoreCoordinatorLockingError = 132010;
  NSManagedObjectReferentialIntegrityError = 133000;
  NSManagedObjectExternalRelationshipError = 133010;
  NSManagedObjectMergeError = 133020;
  NSManagedObjectConstraintMergeError = 133021;
  NSPersistentStoreInvalidTypeError = 134000;
  NSPersistentStoreTypeMismatchError = 134010;
  NSPersistentStoreIncompatibleSchemaError = 134020;
  NSPersistentStoreSaveError = 134030;
  NSPersistentStoreIncompleteSaveError = 134040;
  NSPersistentStoreSaveConflictsError = 134050;
  NSCoreDataError = 134060;
  NSPersistentStoreOperationError = 134070;
  NSPersistentStoreOpenError = 134080;
  NSPersistentStoreTimeoutError = 134090;
  NSPersistentStoreUnsupportedRequestTypeError = 134091;
  NSPersistentStoreIncompatibleVersionHashError = 134100;
  NSMigrationError = 134110;
  NSMigrationConstraintViolationError = 134111;
  NSMigrationCancelledError = 134120;
  NSMigrationMissingSourceModelError = 134130;
  NSMigrationMissingMappingModelError = 134140;
  NSMigrationManagerSourceStoreError = 134150;
  NSMigrationManagerDestinationStoreError = 134160;
  NSEntityMigrationPolicyError = 134170;
  NSSQLiteError = 134180;
  NSInferredMappingModelError = 134190;
  NSExternalRecordImportError = 134200;
  NSPersistentHistoryTokenExpiredError = 134301;
  NSManagedObjectModelReferenceNotFoundError = 134504;
  NSStagedMigrationFrameworkVersionMismatchError = 134505;
  NSStagedMigrationBackwardMigrationError = 134506;
  NSUndefinedAttributeType = 0;
  NSInteger16AttributeType = 100;
  NSInteger32AttributeType = 200;
  NSInteger64AttributeType = 300;
  NSDecimalAttributeType = 400;
  NSDoubleAttributeType = 500;
  NSFloatAttributeType = 600;
  NSStringAttributeType = 700;
  NSBooleanAttributeType = 800;
  NSDateAttributeType = 900;
  NSBinaryDataAttributeType = 1000;
  NSUUIDAttributeType = 1100;
  NSURIAttributeType = 1200;
  NSTransformableAttributeType = 1800;
  NSObjectIDAttributeType = 2000;
  NSCompositeAttributeType = 2100;
  NSNoActionDeleteRule = 0;
  NSNullifyDeleteRule = 1;
  NSCascadeDeleteRule = 2;
  NSDenyDeleteRule = 3;
  NSFetchIndexElementTypeBinary = 0;
  NSFetchIndexElementTypeRTree = 1;
  NSFetchRequestType = 1;
  NSSaveRequestType = 2;
  NSBatchInsertRequestType = 5;
  NSBatchUpdateRequestType = 6;
  NSBatchDeleteRequestType = 7;
  NSSnapshotEventUndoInsertion = 2;
  NSSnapshotEventUndoDeletion = 4;
  NSSnapshotEventUndoUpdate = 8;
  NSSnapshotEventRollback = 16;
  NSSnapshotEventRefresh = 32;
  NSSnapshotEventMergePolicy = 64;
  NSManagedObjectResultType = 0;
  NSManagedObjectIDResultType = 1;
  NSDictionaryResultType = 2;
  NSCountResultType = 4;
  NSConfinementConcurrencyType = 0;
  NSPrivateQueueConcurrencyType = 1;
  NSMainQueueConcurrencyType = 2;
  NSPersistentStoreUbiquitousTransitionTypeAccountAdded = 1;
  NSPersistentStoreUbiquitousTransitionTypeAccountRemoved = 2;
  NSPersistentStoreUbiquitousTransitionTypeContentRemoved = 3;
  NSPersistentStoreUbiquitousTransitionTypeInitialImportCompleted = 4;
  NSUndefinedEntityMappingType = 0;
  NSCustomEntityMappingType = 1;
  NSAddEntityMappingType = 2;
  NSRemoveEntityMappingType = 3;
  NSCopyEntityMappingType = 4;
  NSTransformEntityMappingType = 5;
  NSBatchInsertRequestResultTypeStatusOnly = 0;
  NSBatchInsertRequestResultTypeObjectIDs = 1;
  NSBatchInsertRequestResultTypeCount = 2;
  NSStatusOnlyResultType = 0;
  NSUpdatedObjectIDsResultType = 1;
  NSUpdatedObjectsCountResultType = 2;
  NSBatchDeleteResultTypeStatusOnly = 0;
  NSBatchDeleteResultTypeObjectIDs = 1;
  NSBatchDeleteResultTypeCount = 2;
  NSPersistentHistoryResultTypeStatusOnly = 0;
  NSPersistentHistoryResultTypeObjectIDs = 1;
  NSPersistentHistoryResultTypeCount = 2;
  NSPersistentHistoryResultTypeTransactionsOnly = 3;
  NSPersistentHistoryResultTypeChangesOnly = 4;
  NSPersistentHistoryResultTypeTransactionsAndChanges = 5;
  NSPersistentCloudKitContainerEventResultTypeEvents = 0;
  NSPersistentCloudKitContainerEventResultTypeCountEvents = 1;
  NSErrorMergePolicyType = 0;
  NSMergeByPropertyStoreTrumpMergePolicyType = 1;
  NSMergeByPropertyObjectTrumpMergePolicyType = 2;
  NSOverwriteMergePolicyType = 3;
  NSRollbackMergePolicyType = 4;
  NSFetchedResultsChangeInsert = 1;
  NSFetchedResultsChangeDelete = 2;
  NSFetchedResultsChangeMove = 3;
  NSFetchedResultsChangeUpdate = 4;
  NSPersistentHistoryChangeTypeInsert = 0;
  NSPersistentHistoryChangeTypeUpdate = 1;
  NSPersistentHistoryChangeTypeDelete = 2;
  NSPersistentCloudKitContainerSchemaInitializationOptionsNone = 0;
  NSPersistentCloudKitContainerSchemaInitializationOptionsDryRun = 2;
  NSPersistentCloudKitContainerSchemaInitializationOptionsPrintSchema = 4;
  NSPersistentCloudKitContainerEventTypeSetup = 0;
  NSPersistentCloudKitContainerEventTypeImport = 1;
  NSPersistentCloudKitContainerEventTypeExport = 2;

type
  NSPropertyDescription = interface;
  NSAttributeDescription = interface;
  NSDerivedAttributeDescription = interface;
  NSCompositeAttributeDescription = interface;
  NSEntityDescription = interface;
  NSFetchedPropertyDescription = interface;
  NSExpressionDescription = interface;
  NSRelationshipDescription = interface;
  NSFetchIndexDescription = interface;
  NSFetchIndexElementDescription = interface;
  NSPersistentStoreRequest = interface;
  NSManagedObject = interface;
  NSManagedObjectID = interface;
  NSFetchRequestResult = interface;
  NSFetchRequest = interface;
  NSAsynchronousFetchRequest = interface;
  NSFetchRequestExpression = interface;
  NSManagedObjectModel = interface;
  NSManagedObjectContext = interface;
  NSPersistentStoreCoordinator = interface;
  NSPersistentStore = interface;
  NSAtomicStoreCacheNode = interface;
  NSAtomicStore = interface;
  NSEntityMigrationPolicy = interface;
  NSMappingModel = interface;
  NSEntityMapping = interface;
  NSPropertyMapping = interface;
  NSMigrationManager = interface;
  NSIncrementalStore = interface;
  NSIncrementalStoreNode = interface;
  NSPersistentStoreResult = interface;
  NSPersistentStoreAsynchronousResult = interface;
  NSAsynchronousFetchResult = interface;
  NSBatchInsertResult = interface;
  NSBatchUpdateResult = interface;
  NSBatchDeleteResult = interface;
  NSPersistentHistoryResult = interface;
  NSPersistentCloudKitContainerEventResult = interface;
  NSSaveChangesRequest = interface;
  NSBatchUpdateRequest = interface;
  NSBatchDeleteRequest = interface;
  NSBatchInsertRequest = interface;
  NSMergeConflict = interface;
  NSConstraintConflict = interface;
  NSMergePolicy = interface;
  NSFetchedResultsController = interface;
  NSFetchedResultsSectionInfo = interface;
  NSFetchedResultsControllerDelegate = interface;
  NSQueryGenerationToken = interface;
  NSPersistentStoreDescription = interface;
  NSPersistentContainer = interface;
  NSPersistentHistoryChange = interface;
  NSPersistentHistoryChangeRequest = interface;
  NSPersistentHistoryToken = interface;
  NSPersistentHistoryTransaction = interface;
  NSPersistentCloudKitContainer = interface;
  NSPersistentCloudKitContainerOptions = interface;
  NSPersistentCloudKitContainerEvent = interface;
  NSPersistentCloudKitContainerEventRequest = interface;
  NSStagedMigrationManager = interface;
  NSMigrationStage = interface;
  NSCustomMigrationStage = interface;
  NSLightweightMigrationStage = interface;
  NSManagedObjectModelReference = interface;
  NSCoreDataCoreSpotlightDelegate = interface;

  PPointer = ^Pointer;
  NSAttributeType = NSInteger;
  NSDeleteRule = NSInteger;
  NSFetchIndexElementType = NSInteger;
  NSPersistentStoreRequestType = NSInteger;
  NSSnapshotEventType = NSInteger;
  NSFetchRequestResultType = NSInteger;

  NSPersistentStoreAsynchronousFetchResultCompletionBlock = procedure(result: NSAsynchronousFetchResult) of object;
  NSManagedObjectContextConcurrencyType = NSInteger;
  NSPersistentStoreUbiquitousTransitionType = NSInteger;
  NSEntityMappingType = NSInteger;
  NSBatchInsertRequestResultType = NSInteger;
  NSBatchUpdateRequestResultType = NSInteger;
  NSBatchDeleteRequestResultType = NSInteger;
  NSPersistentHistoryResultType = NSInteger;
  NSPersistentCloudKitContainerEventResultType = NSInteger;
  NSMergePolicyType = NSInteger;
  NSFetchedResultsChangeType = NSInteger;
  NSPersistentHistoryChangeType = NSInteger;
  NSPersistentCloudKitContainerSchemaInitializationOptions = NSInteger;
  NSPersistentCloudKitContainerEventType = NSInteger;
  TNSAsynchronousFetchRequestBlockMethod1 = procedure(param1: NSAsynchronousFetchResult) of object;
  TNSManagedObjectContextBlockMethod1 = procedure of object;
  TNSPersistentStoreCoordinatorBlockMethod1 = procedure(param1: NSPersistentStoreDescription; param2: NSError) of object;
  TNSPersistentStoreCoordinatorBlockMethod2 = procedure of object;
  TNSBatchInsertRequestBlockMethod1 = procedure(obj: NSMutableDictionary) of object;
  TNSBatchInsertRequestBlockMethod2 = procedure(obj: NSManagedObject) of object;
  TNSBatchInsertRequestBlockMethod3 = function(param1: NSMutableDictionary): Boolean of object;
  TNSBatchInsertRequestBlockMethod4 = procedure of object;
  TNSBatchInsertRequestBlockMethod5 = function(param1: NSManagedObject): Boolean of object;
  TNSPersistentContainerBlockMethod1 = procedure(param1: NSPersistentStoreDescription; param2: NSError) of object;
  TNSPersistentContainerBlockMethod2 = procedure(param1: NSManagedObjectContext) of object;
  TNSCustomMigrationStageBlockMethod1 = function(param1: NSStagedMigrationManager; param2: NSCustomMigrationStage;
    param3: PPointer): Boolean of object;
  TNSCustomMigrationStageBlockMethod2 = procedure of object;
  TNSCustomMigrationStageBlockMethod3 = function(param1: NSStagedMigrationManager; param2: NSCustomMigrationStage;
    param3: PPointer): Boolean of object;
  TNSCoreDataCoreSpotlightDelegateBlockMethod1 = procedure(error: NSError) of object;
  TNSCoreDataCoreSpotlightDelegateBlockMethod2 = procedure of object;

  NSPropertyDescriptionClass = interface(NSObjectClass)
    ['{2721FD22-FB12-49E8-B796-325139504411}']
  end;

  NSPropertyDescription = interface(NSObject)
    ['{179CFCA3-0240-4A34-AED4-DC166E3065A5}']
    function entity: NSEntityDescription; cdecl;
    function isIndexed: Boolean; cdecl; // API_DEPRECATED( "Use NSEntityDescription.indexes instead", macosx(10.5,10.13),ios(3.0,11.0),tvos(9.0, 11.0),watchos(2.0, 4.0))
    function isIndexedBySpotlight: Boolean; cdecl;
    function isOptional: Boolean; cdecl;
    function isStoredInExternalRecord: Boolean; cdecl; // API_DEPRECATED("Spotlight integration is deprecated. Use CoreSpotlight integration instead.", macosx(10.6,10.13),ios(3.0,11.0))
    function isTransient: Boolean; cdecl;
    function name: NSString; cdecl;
    function renamingIdentifier: NSString; cdecl;
    procedure setIndexed(indexed: Boolean); cdecl; // API_DEPRECATED( "Use NSEntityDescription.indexes instead", macosx(10.5,10.13),ios(3.0,11.0),tvos(9.0, 11.0),watchos(2.0, 4.0))
    procedure setIndexedBySpotlight(indexedBySpotlight: Boolean); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setOptional(optional: Boolean); cdecl;
    procedure setRenamingIdentifier(renamingIdentifier: NSString); cdecl;
    procedure setStoredInExternalRecord(storedInExternalRecord: Boolean); cdecl; // API_DEPRECATED("Spotlight integration is deprecated. Use CoreSpotlight integration instead.", macosx(10.6,10.13),ios(3.0,11.0))
    procedure setTransient(transient: Boolean); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    procedure setValidationPredicates(validationPredicates: NSArray; withValidationWarnings: NSArray); cdecl;
    procedure setVersionHashModifier(versionHashModifier: NSString); cdecl;
    function userInfo: NSDictionary; cdecl;
    function validationPredicates: NSArray; cdecl;
    function validationWarnings: NSArray; cdecl;
    function versionHash: NSData; cdecl;
    function versionHashModifier: NSString; cdecl;
  end;
  TNSPropertyDescription = class(TOCGenericImport<NSPropertyDescriptionClass, NSPropertyDescription>) end;

  NSAttributeDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{752E620C-462D-4486-8241-8F6CFE0F7789}']
  end;

  NSAttributeDescription = interface(NSPropertyDescription)
    ['{271000A4-1FD3-4283-A63F-4C10D4CFD7EB}']
    function allowsCloudEncryption: Boolean; cdecl;
    function allowsExternalBinaryDataStorage: Boolean; cdecl;
    function attributeType: NSAttributeType; cdecl;
    function attributeValueClassName: NSString; cdecl;
    function defaultValue: Pointer; cdecl;
    function preservesValueInHistoryOnDeletion: Boolean; cdecl;
    procedure setAllowsCloudEncryption(allowsCloudEncryption: Boolean); cdecl;
    procedure setAllowsExternalBinaryDataStorage(allowsExternalBinaryDataStorage: Boolean); cdecl;
    procedure setAttributeType(attributeType: NSAttributeType); cdecl;
    procedure setAttributeValueClassName(attributeValueClassName: NSString); cdecl;
    procedure setDefaultValue(defaultValue: Pointer); cdecl;
    procedure setPreservesValueInHistoryOnDeletion(preservesValueInHistoryOnDeletion: Boolean); cdecl;
    procedure setValueTransformerName(valueTransformerName: NSString); cdecl;
    function valueTransformerName: NSString; cdecl;
    function versionHash: NSData; cdecl;
  end;
  TNSAttributeDescription = class(TOCGenericImport<NSAttributeDescriptionClass, NSAttributeDescription>) end;

  NSDerivedAttributeDescriptionClass = interface(NSAttributeDescriptionClass)
    ['{D3AE3324-7046-452A-A613-0AF0028E9005}']
  end;

  NSDerivedAttributeDescription = interface(NSAttributeDescription)
    ['{CF517433-7F3D-4A6B-971F-16440D911519}']
    function derivationExpression: NSExpression; cdecl;
    procedure setDerivationExpression(derivationExpression: NSExpression); cdecl;
  end;
  TNSDerivedAttributeDescription = class(TOCGenericImport<NSDerivedAttributeDescriptionClass, NSDerivedAttributeDescription>) end;

  NSCompositeAttributeDescriptionClass = interface(NSAttributeDescriptionClass)
    ['{9D289B28-3C3D-4211-AF24-33BBB8FE4569}']
  end;

  NSCompositeAttributeDescription = interface(NSAttributeDescription)
    ['{30C28924-9282-46BD-B471-27EF9F0D0A1A}']
    function elements: NSArray; cdecl;
    procedure setElements(elements: NSArray); cdecl;
  end;
  TNSCompositeAttributeDescription = class(TOCGenericImport<NSCompositeAttributeDescriptionClass, NSCompositeAttributeDescription>) end;

  NSEntityDescriptionClass = interface(NSObjectClass)
    ['{4D91062B-1027-4BEB-BF75-8B154C661EB8}']
    {class} function entityForName(entityName: NSString; inManagedObjectContext: NSManagedObjectContext): NSEntityDescription; cdecl;
    {class} function insertNewObjectForEntityForName(entityName: NSString; inManagedObjectContext: NSManagedObjectContext): NSManagedObject; cdecl;
  end;

  NSEntityDescription = interface(NSObject)
    ['{C6E34630-AE01-43BF-85F2-47DC8ED1BDBE}']
    function attributesByName: NSDictionary; cdecl;
    function compoundIndexes: NSArray; cdecl; // API_DEPRECATED( "Use NSEntityDescription.indexes instead", macosx(10.5,10.13),ios(3.0,11.0),tvos(9.0, 11.0),watchos(2.0, 4.0))
    function coreSpotlightDisplayNameExpression: NSExpression; cdecl;
    function indexes: NSArray; cdecl;
    function isAbstract: Boolean; cdecl;
    function isKindOfEntity(entity: NSEntityDescription): Boolean; cdecl;
    function managedObjectClassName: NSString; cdecl;
    function managedObjectModel: NSManagedObjectModel; cdecl;
    function name: NSString; cdecl;
    function properties: NSArray; cdecl;
    function propertiesByName: NSDictionary; cdecl;
    function relationshipsByName: NSDictionary; cdecl;
    function relationshipsWithDestinationEntity(entity: NSEntityDescription): NSArray; cdecl;
    function renamingIdentifier: NSString; cdecl;
    procedure setAbstract(abstract: Boolean); cdecl;
    procedure setCompoundIndexes(compoundIndexes: NSArray); cdecl; // API_DEPRECATED( "Use NSEntityDescription.indexes instead", macosx(10.5,10.13),ios(3.0,11.0),tvos(9.0, 11.0),watchos(2.0, 4.0))
    procedure setCoreSpotlightDisplayNameExpression(coreSpotlightDisplayNameExpression: NSExpression); cdecl;
    procedure setIndexes(indexes: NSArray); cdecl;
    procedure setManagedObjectClassName(managedObjectClassName: NSString); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setProperties(properties: NSArray); cdecl;
    procedure setRenamingIdentifier(renamingIdentifier: NSString); cdecl;
    procedure setSubentities(subentities: NSArray); cdecl;
    procedure setUniquenessConstraints(uniquenessConstraints: NSArray); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    procedure setVersionHashModifier(versionHashModifier: NSString); cdecl;
    function subentities: NSArray; cdecl;
    function subentitiesByName: NSDictionary; cdecl;
    function superentity: NSEntityDescription; cdecl;
    function uniquenessConstraints: NSArray; cdecl;
    function userInfo: NSDictionary; cdecl;
    function versionHash: NSData; cdecl;
    function versionHashModifier: NSString; cdecl;
  end;
  TNSEntityDescription = class(TOCGenericImport<NSEntityDescriptionClass, NSEntityDescription>) end;

  NSFetchedPropertyDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{6E06B008-43CA-429D-9DD3-9610B7395FF5}']
  end;

  NSFetchedPropertyDescription = interface(NSPropertyDescription)
    ['{78973D0D-3CE7-45DA-8FBB-3C2B68241330}']
    function fetchRequest: NSFetchRequest; cdecl;
    procedure setFetchRequest(fetchRequest: NSFetchRequest); cdecl;
  end;
  TNSFetchedPropertyDescription = class(TOCGenericImport<NSFetchedPropertyDescriptionClass, NSFetchedPropertyDescription>) end;

  NSExpressionDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{39DD799E-77D5-465F-9740-F29567F4F16C}']
  end;

  NSExpressionDescription = interface(NSPropertyDescription)
    ['{E355DC73-76AE-4FF8-842F-3896CF374EDA}']
    function expression: NSExpression; cdecl;
    function expressionResultType: NSAttributeType; cdecl;
    procedure setExpression(expression: NSExpression); cdecl;
    procedure setExpressionResultType(expressionResultType: NSAttributeType); cdecl;
  end;
  TNSExpressionDescription = class(TOCGenericImport<NSExpressionDescriptionClass, NSExpressionDescription>) end;

  NSRelationshipDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{2E66C5D4-0895-4C39-B88A-7A1CF95F4F25}']
  end;

  NSRelationshipDescription = interface(NSPropertyDescription)
    ['{211C14B5-01BD-4816-B37D-1E58B74A706F}']
    function deleteRule: NSDeleteRule; cdecl;
    function destinationEntity: NSEntityDescription; cdecl;
    function inverseRelationship: NSRelationshipDescription; cdecl;
    function isOrdered: Boolean; cdecl;
    function isToMany: Boolean; cdecl;
    function maxCount: NSUInteger; cdecl;
    function minCount: NSUInteger; cdecl;
    procedure setDeleteRule(deleteRule: NSDeleteRule); cdecl;
    procedure setDestinationEntity(destinationEntity: NSEntityDescription); cdecl;
    procedure setInverseRelationship(inverseRelationship: NSRelationshipDescription); cdecl;
    procedure setMaxCount(maxCount: NSUInteger); cdecl;
    procedure setMinCount(minCount: NSUInteger); cdecl;
    procedure setOrdered(ordered: Boolean); cdecl;
    function versionHash: NSData; cdecl;
  end;
  TNSRelationshipDescription = class(TOCGenericImport<NSRelationshipDescriptionClass, NSRelationshipDescription>) end;

  NSFetchIndexDescriptionClass = interface(NSObjectClass)
    ['{6BC4C15B-A583-4205-A1CC-BFF14C052B5B}']
  end;

  NSFetchIndexDescription = interface(NSObject)
    ['{55DB530E-C92F-440C-8A1F-AA76B0CFA405}']
    function elements: NSArray; cdecl;
    function entity: NSEntityDescription; cdecl;
    function initWithName(name: NSString; elements: NSArray): Pointer; cdecl;
    function name: NSString; cdecl;
    function partialIndexPredicate: NSPredicate; cdecl;
    procedure setElements(elements: NSArray); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setPartialIndexPredicate(partialIndexPredicate: NSPredicate); cdecl;
  end;
  TNSFetchIndexDescription = class(TOCGenericImport<NSFetchIndexDescriptionClass, NSFetchIndexDescription>) end;

  NSFetchIndexElementDescriptionClass = interface(NSObjectClass)
    ['{1ADBA197-6AD6-4A9B-9612-FD6CEFFECFCD}']
  end;

  NSFetchIndexElementDescription = interface(NSObject)
    ['{C3B51B4D-8DF8-43E8-91B3-98E7E51CE2F8}']
    function &property: NSPropertyDescription; cdecl;
    function collationType: NSFetchIndexElementType; cdecl;
    function indexDescription: NSFetchIndexDescription; cdecl;
    function initWithProperty(&property: NSPropertyDescription; collationType: NSFetchIndexElementType): Pointer; cdecl;
    function isAscending: Boolean; cdecl;
    function propertyName: NSString; cdecl;
    procedure setAscending(ascending: Boolean); cdecl;
    procedure setCollationType(collationType: NSFetchIndexElementType); cdecl;
  end;
  TNSFetchIndexElementDescription = class(TOCGenericImport<NSFetchIndexElementDescriptionClass, NSFetchIndexElementDescription>) end;

  NSPersistentStoreRequestClass = interface(NSObjectClass)
    ['{FC3CAD9A-D6E9-4489-B6B0-54D8C5C2685D}']
  end;

  NSPersistentStoreRequest = interface(NSObject)
    ['{16E285BB-6ED2-41FD-8F27-0BA21D1BA4A9}']
    function affectedStores: NSArray; cdecl;
    function requestType: NSPersistentStoreRequestType; cdecl;
    procedure setAffectedStores(affectedStores: NSArray); cdecl;
  end;
  TNSPersistentStoreRequest = class(TOCGenericImport<NSPersistentStoreRequestClass, NSPersistentStoreRequest>) end;

  NSManagedObjectClass = interface(NSObjectClass)
    ['{1DB15635-FC1E-427C-869A-B25F2454969F}']
    {class} function contextShouldIgnoreUnmodeledPropertyChanges: Boolean; cdecl;
    {class} function entity: NSEntityDescription; cdecl;
    {class} function fetchRequest: NSFetchRequest; cdecl;
  end;

  NSManagedObject = interface(NSObject)
    ['{DDBC9DD8-643E-45B2-95C3-6FD0DC17B3D2}']
    procedure awakeFromFetch; cdecl;
    procedure awakeFromInsert; cdecl;
    procedure awakeFromSnapshotEvents(flags: NSSnapshotEventType); cdecl;
    function changedValues: NSDictionary; cdecl;
    function changedValuesForCurrentEvent: NSDictionary; cdecl;
    function committedValuesForKeys(keys: NSArray): NSDictionary; cdecl;
    procedure didAccessValueForKey(key: NSString); cdecl;
    procedure didChangeValueForKey(inKey: NSString; withSetMutation: NSKeyValueSetMutationKind; usingObjects: NSSet); overload; cdecl;
    procedure didChangeValueForKey(key: NSString); overload; cdecl;
    procedure didSave; cdecl;
    procedure didTurnIntoFault; cdecl;
    function entity: NSEntityDescription; cdecl;
    function faultingState: NSUInteger; cdecl;
    function hasChanges: Boolean; cdecl;
    function hasFaultForRelationshipNamed(key: NSString): Boolean; cdecl;
    function hasPersistentChangedValues: Boolean; cdecl;
    function initWithContext(moc: NSManagedObjectContext): Pointer; cdecl;
    function initWithEntity(entity: NSEntityDescription; insertIntoManagedObjectContext: NSManagedObjectContext): NSManagedObject; cdecl;
    function isDeleted: Boolean; cdecl;
    function isFault: Boolean; cdecl;
    function isInserted: Boolean; cdecl;
    function isUpdated: Boolean; cdecl;
    function managedObjectContext: NSManagedObjectContext; cdecl;
    function objectID: NSManagedObjectID; cdecl;
    function objectIDsForRelationshipNamed(key: NSString): NSArray; cdecl;
    function observationInfo: Pointer; cdecl;
    procedure prepareForDeletion; cdecl;
    function primitiveValueForKey(key: NSString): Pointer; cdecl;
    procedure setObservationInfo(inObservationInfo: Pointer); cdecl;
    procedure setPrimitiveValue(value: Pointer; forKey: NSString); cdecl;
    procedure setValue(value: Pointer; forKey: NSString); cdecl;
    function validateForDelete(error: PPointer): Boolean; cdecl;
    function validateForInsert(error: PPointer): Boolean; cdecl;
    function validateForUpdate(error: PPointer): Boolean; cdecl;
    function validateValue(value: PPointer; forKey: NSString; error: PPointer): Boolean; cdecl;
    function valueForKey(key: NSString): Pointer; cdecl;
    procedure willAccessValueForKey(key: NSString); cdecl;
    procedure willChangeValueForKey(key: NSString); overload; cdecl;
    procedure willChangeValueForKey(inKey: NSString; withSetMutation: NSKeyValueSetMutationKind; usingObjects: NSSet); overload; cdecl;
    procedure willSave; cdecl;
    procedure willTurnIntoFault; cdecl;
  end;
  TNSManagedObject = class(TOCGenericImport<NSManagedObjectClass, NSManagedObject>) end;

  NSManagedObjectIDClass = interface(NSObjectClass)
    ['{30232B24-121C-47B1-B65B-CBC3DEE606E9}']
  end;

  NSManagedObjectID = interface(NSObject)
    ['{77DE07AE-CB61-4D6D-A163-F7EE5C2FC45A}']
    function entity: NSEntityDescription; cdecl;
    function isTemporaryID: Boolean; cdecl;
    function persistentStore: NSPersistentStore; cdecl;
    function URIRepresentation: NSURL; cdecl;
  end;
  TNSManagedObjectID = class(TOCGenericImport<NSManagedObjectIDClass, NSManagedObjectID>) end;

  NSFetchRequestResult = interface(IObjectiveC)
    ['{AC2DFA1C-8D04-49C3-91AA-C57650B52385}']
  end;

  NSFetchRequestClass = interface(NSPersistentStoreRequestClass)
    ['{6AE37570-EAFC-4012-A877-16FCC9267275}']
    {class} function fetchRequestWithEntityName(entityName: NSString): Pointer; cdecl;
  end;

  NSFetchRequest = interface(NSPersistentStoreRequest)
    ['{846643D8-5059-4BA4-B34F-4C025157C1D7}']
    function affectedStores: NSArray; cdecl;
    function entity: NSEntityDescription; cdecl;
    function entityName: NSString; cdecl;
    function execute(error: PPointer): NSArray; cdecl;
    function fetchBatchSize: NSUInteger; cdecl;
    function fetchLimit: NSUInteger; cdecl;
    function fetchOffset: NSUInteger; cdecl;
    function havingPredicate: NSPredicate; cdecl;
    function includesPendingChanges: Boolean; cdecl;
    function includesPropertyValues: Boolean; cdecl;
    function includesSubentities: Boolean; cdecl;
    function initWithEntityName(entityName: NSString): Pointer; cdecl;
    function predicate: NSPredicate; cdecl;
    function propertiesToFetch: NSArray; cdecl;
    function propertiesToGroupBy: NSArray; cdecl;
    function relationshipKeyPathsForPrefetching: NSArray; cdecl;
    function resultType: NSFetchRequestResultType; cdecl;
    function returnsDistinctResults: Boolean; cdecl;
    function returnsObjectsAsFaults: Boolean; cdecl;
    procedure setAffectedStores(affectedStores: NSArray); cdecl;
    procedure setEntity(entity: NSEntityDescription); cdecl;
    procedure setFetchBatchSize(fetchBatchSize: NSUInteger); cdecl;
    procedure setFetchLimit(fetchLimit: NSUInteger); cdecl;
    procedure setFetchOffset(fetchOffset: NSUInteger); cdecl;
    procedure setHavingPredicate(havingPredicate: NSPredicate); cdecl;
    procedure setIncludesPendingChanges(includesPendingChanges: Boolean); cdecl;
    procedure setIncludesPropertyValues(includesPropertyValues: Boolean); cdecl;
    procedure setIncludesSubentities(includesSubentities: Boolean); cdecl;
    procedure setPredicate(predicate: NSPredicate); cdecl;
    procedure setPropertiesToFetch(propertiesToFetch: NSArray); cdecl;
    procedure setPropertiesToGroupBy(propertiesToGroupBy: NSArray); cdecl;
    procedure setRelationshipKeyPathsForPrefetching(relationshipKeyPathsForPrefetching: NSArray); cdecl;
    procedure setResultType(resultType: NSFetchRequestResultType); cdecl;
    procedure setReturnsDistinctResults(returnsDistinctResults: Boolean); cdecl;
    procedure setReturnsObjectsAsFaults(returnsObjectsAsFaults: Boolean); cdecl;
    procedure setShouldRefreshRefetchedObjects(shouldRefreshRefetchedObjects: Boolean); cdecl;
    procedure setSortDescriptors(sortDescriptors: NSArray); cdecl;
    function shouldRefreshRefetchedObjects: Boolean; cdecl;
    function sortDescriptors: NSArray; cdecl;
  end;
  TNSFetchRequest = class(TOCGenericImport<NSFetchRequestClass, NSFetchRequest>) end;

  NSAsynchronousFetchRequestClass = interface(NSPersistentStoreRequestClass)
    ['{930E9A8B-C4F6-45A1-AB32-EE2C02282068}']
  end;

  NSAsynchronousFetchRequest = interface(NSPersistentStoreRequest)
    ['{C7BC34B4-EAEE-40CE-A9C0-A294CC56CF5C}']
    function completionBlock: NSPersistentStoreAsynchronousFetchResultCompletionBlock; cdecl;
    function estimatedResultCount: NSInteger; cdecl;
    function fetchRequest: NSFetchRequest; cdecl;
    function initWithFetchRequest(request: NSFetchRequest; completionBlock: TNSAsynchronousFetchRequestBlockMethod1): Pointer; cdecl;
    procedure setEstimatedResultCount(estimatedResultCount: NSInteger); cdecl;
  end;
  TNSAsynchronousFetchRequest = class(TOCGenericImport<NSAsynchronousFetchRequestClass, NSAsynchronousFetchRequest>) end;

  NSFetchRequestExpressionClass = interface(NSExpressionClass)
    ['{482276EC-21B6-4735-ADC9-6812749BC196}']
    {class} function expressionForFetch(fetch: NSExpression; context: NSExpression; countOnly: Boolean): NSExpression; cdecl;
  end;

  NSFetchRequestExpression = interface(NSExpression)
    ['{CA5E37EA-7565-4C69-BA43-4513067FB683}']
    function contextExpression: NSExpression; cdecl;
    function isCountOnlyRequest: Boolean; cdecl;
    function requestExpression: NSExpression; cdecl;
  end;
  TNSFetchRequestExpression = class(TOCGenericImport<NSFetchRequestExpressionClass, NSFetchRequestExpression>) end;

  NSManagedObjectModelClass = interface(NSObjectClass)
    ['{DB997C46-BFFD-4F58-B4EC-4B685D707FDE}']
    {class} function checksumsForVersionedModelAtURL(modelURL: NSURL; error: PPointer): NSDictionary; cdecl;
    {class} function mergedModelFromBundles(bundles: NSArray; forStoreMetadata: NSDictionary): NSManagedObjectModel; overload; cdecl;
    {class} function mergedModelFromBundles(bundles: NSArray): NSManagedObjectModel; overload; cdecl;
    {class} function modelByMergingModels(models: NSArray; forStoreMetadata: NSDictionary): NSManagedObjectModel; overload; cdecl;
    {class} function modelByMergingModels(models: NSArray): NSManagedObjectModel; overload; cdecl;
  end;

  NSManagedObjectModel = interface(NSObject)
    ['{3C375A21-E8AC-4D56-9438-D649B82B77A2}']
    function configurations: NSArray; cdecl;
    function entities: NSArray; cdecl;
    function entitiesByName: NSDictionary; cdecl;
    function entitiesForConfiguration(configuration: NSString): NSArray; cdecl;
    function entityVersionHashesByName: NSDictionary; cdecl;
    function fetchRequestFromTemplateWithName(name: NSString; substitutionVariables: NSDictionary): NSFetchRequest; cdecl;
    function fetchRequestTemplateForName(name: NSString): NSFetchRequest; cdecl;
    function fetchRequestTemplatesByName: NSDictionary; cdecl;
    function initWithContentsOfURL(url: NSURL): Pointer; cdecl;
    function isConfiguration(configuration: NSString; compatibleWithStoreMetadata: NSDictionary): Boolean; cdecl;
    function localizationDictionary: NSDictionary; cdecl;
    procedure setEntities(entities: NSArray); overload; cdecl;
    procedure setEntities(entities: NSArray; forConfiguration: NSString); overload; cdecl;
    procedure setFetchRequestTemplate(fetchRequestTemplate: NSFetchRequest; forName: NSString); cdecl;
    procedure setLocalizationDictionary(localizationDictionary: NSDictionary); cdecl;
    procedure setVersionIdentifiers(versionIdentifiers: NSSet); cdecl;
    function versionChecksum: NSString; cdecl;
    function versionIdentifiers: NSSet; cdecl;
  end;
  TNSManagedObjectModel = class(TOCGenericImport<NSManagedObjectModelClass, NSManagedObjectModel>) end;

  NSManagedObjectContextClass = interface(NSObjectClass)
    ['{6C1E4118-53A3-4463-BC84-C51A9919464D}']
    {class} procedure mergeChangesFromRemoteContextSave(changeNotificationData: NSDictionary; intoContexts: NSArray); cdecl;
    {class} function new: Pointer; cdecl; // API_DEPRECATED( "Use -initWithConcurrencyType: instead", macosx(10.4,10.11), ios(3.0,9.0))
  end;

  NSManagedObjectContext = interface(NSObject)
    ['{14F314BC-94A9-4F44-92A8-B874A81DF9A8}']
    procedure assignObject(&object: Pointer; toPersistentStore: NSPersistentStore); cdecl;
    function automaticallyMergesChangesFromParent: Boolean; cdecl;
    function concurrencyType: NSManagedObjectContextConcurrencyType; cdecl;
    function countForFetchRequest(request: NSFetchRequest; error: PPointer): NSUInteger; cdecl;
    function deletedObjects: NSSet; cdecl;
    procedure deleteObject(&object: NSManagedObject); cdecl;
    procedure detectConflictsForObject(&object: NSManagedObject); cdecl;
    function executeFetchRequest(request: NSFetchRequest; error: PPointer): NSArray; cdecl;
    function executeRequest(request: NSPersistentStoreRequest; error: PPointer): NSPersistentStoreResult; cdecl;
    function existingObjectWithID(objectID: NSManagedObjectID; error: PPointer): NSManagedObject; cdecl;
    function hasChanges: Boolean; cdecl;
    function initWithConcurrencyType(ct: NSManagedObjectContextConcurrencyType): Pointer; cdecl;
    function insertedObjects: NSSet; cdecl;
    procedure insertObject(&object: NSManagedObject); cdecl;
    procedure lock; cdecl; // API_DEPRECATED( "Use a queue style context and -performBlockAndWait: instead", macosx(10.4,10.10), ios(3.0,8.0))
    procedure mergeChangesFromContextDidSaveNotification(notification: NSNotification); cdecl;
    function mergePolicy: Pointer; cdecl;
    function name: NSString; cdecl;
    function objectRegisteredForID(objectID: NSManagedObjectID): NSManagedObject; cdecl;
    function objectWithID(objectID: NSManagedObjectID): NSManagedObject; cdecl;
    procedure observeValueForKeyPath(keyPath: NSString; ofObject: Pointer; change: NSDictionary; context: Pointer); cdecl;
    function obtainPermanentIDsForObjects(objects: NSArray; error: PPointer): Boolean; cdecl;
    function parentContext: NSManagedObjectContext; cdecl;
    procedure performBlock(block: TNSManagedObjectContextBlockMethod1); cdecl;
    procedure performBlockAndWait(block: TNSManagedObjectContextBlockMethod1); cdecl;
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; cdecl;
    procedure processPendingChanges; cdecl;
    function propagatesDeletesAtEndOfEvent: Boolean; cdecl;
    function queryGenerationToken: NSQueryGenerationToken; cdecl;
    procedure redo; cdecl;
    procedure refreshAllObjects; cdecl;
    procedure refreshObject(&object: NSManagedObject; mergeChanges: Boolean); cdecl;
    function registeredObjects: NSSet; cdecl;
    procedure reset; cdecl;
    function retainsRegisteredObjects: Boolean; cdecl;
    procedure rollback; cdecl;
    function save(error: PPointer): Boolean; cdecl;
    procedure setAutomaticallyMergesChangesFromParent(automaticallyMergesChangesFromParent: Boolean); cdecl;
    procedure setMergePolicy(mergePolicy: Pointer); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setParentContext(parentContext: NSManagedObjectContext); cdecl;
    procedure setPersistentStoreCoordinator(persistentStoreCoordinator: NSPersistentStoreCoordinator); cdecl;
    procedure setPropagatesDeletesAtEndOfEvent(propagatesDeletesAtEndOfEvent: Boolean); cdecl;
    function setQueryGenerationFromToken(generation: NSQueryGenerationToken; error: PPointer): Boolean; cdecl;
    procedure setRetainsRegisteredObjects(retainsRegisteredObjects: Boolean); cdecl;
    procedure setShouldDeleteInaccessibleFaults(shouldDeleteInaccessibleFaults: Boolean); cdecl;
    procedure setStalenessInterval(stalenessInterval: NSTimeInterval); cdecl;
    procedure setTransactionAuthor(transactionAuthor: NSString); cdecl;
    procedure setUndoManager(undoManager: NSUndoManager); cdecl;
    function shouldDeleteInaccessibleFaults: Boolean; cdecl;
    function shouldHandleInaccessibleFault(fault: NSManagedObject; forObjectID: NSManagedObjectID;
      triggeredByProperty: NSPropertyDescription): Boolean; cdecl;
    function stalenessInterval: NSTimeInterval; cdecl;
    function transactionAuthor: NSString; cdecl;
    function tryLock: Boolean; cdecl; // API_DEPRECATED( "Use a queue style context and -performBlock: instead", macosx(10.4,10.10), ios(3.0,8.0))
    procedure undo; cdecl;
    function undoManager: NSUndoManager; cdecl;
    procedure unlock; cdecl; // API_DEPRECATED( "Use a queue style context and -performBlockAndWait: instead", macosx(10.4,10.10), ios(3.0,8.0))
    function updatedObjects: NSSet; cdecl;
    function userInfo: NSMutableDictionary; cdecl;
  end;
  TNSManagedObjectContext = class(TOCGenericImport<NSManagedObjectContextClass, NSManagedObjectContext>) end;

  NSPersistentStoreCoordinatorClass = interface(NSObjectClass)
    ['{CF4F22D2-78CC-465C-835E-8F91C4026E11}']
    {class} function elementsDerivedFromExternalRecordURL(fileURL: NSURL): NSDictionary; cdecl; // API_DEPRECATED("Spotlight integration is deprecated. Use CoreSpotlight integration instead.", macosx(10.6,10.13))
    {class} function metadataForPersistentStoreOfType(storeType: NSString; URL: NSURL; options: NSDictionary;
      error: PPointer): NSDictionary; overload; cdecl;
    {class} function metadataForPersistentStoreOfType(storeType: NSString; URL: NSURL; error: PPointer): NSDictionary; overload; cdecl; // API_DEPRECATED("Use -metadataForPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType", macosx(10.5,10.11), ios(3.0,9.0))
    {class} function metadataForPersistentStoreWithURL(url: NSURL; error: PPointer): NSDictionary; cdecl; // API_DEPRECATED("Use -metadataForPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType", macosx(10.4,10.5))
    {class} function registeredStoreTypes: NSDictionary; cdecl;
    {class} procedure registerStoreClass(storeClass: Pointer; forStoreType: NSString); cdecl;
    {class} function removeUbiquitousContentAndPersistentStoreAtURL(storeURL: NSURL; options: NSDictionary; error: PPointer): Boolean; cdecl; // API_DEPRECATED("Please see the release notes and Core Data documentation.", macosx(10.7,10.12), ios(5.0,10.0))
    {class} function setMetadata(metadata: NSDictionary; forPersistentStoreOfType: NSString; URL: NSURL; error: PPointer): Boolean; overload; cdecl; // API_DEPRECATED("Use  -setMetadata:forPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType", macosx(10.5,10.11), ios(3.0,9.0))
    {class} function setMetadata(metadata: NSDictionary; forPersistentStoreOfType: NSString; URL: NSURL; options: NSDictionary;
      error: PPointer): Boolean; overload; cdecl;
  end;

  NSPersistentStoreCoordinator = interface(NSObject)
    ['{76AB4A00-EB85-4673-BF01-1EA689CF541F}']
    procedure addPersistentStoreWithDescription(storeDescription: NSPersistentStoreDescription;
      completionHandler: TNSPersistentStoreCoordinatorBlockMethod1); cdecl;
    function addPersistentStoreWithType(storeType: NSString; configuration: NSString; URL: NSURL; options: NSDictionary;
      error: PPointer): NSPersistentStore; cdecl;
    function currentPersistentHistoryTokenFromStores(stores: NSArray): NSPersistentHistoryToken; cdecl;
    function destroyPersistentStoreAtURL(url: NSURL; withType: NSString; options: NSDictionary; error: PPointer): Boolean; cdecl;
    function executeRequest(request: NSPersistentStoreRequest; withContext: NSManagedObjectContext; error: PPointer): Pointer; cdecl;
    function finishDeferredLightweightMigration(error: PPointer): Boolean; cdecl;
    function finishDeferredLightweightMigrationTask(error: PPointer): Boolean; cdecl;
    function importStoreWithIdentifier(storeIdentifier: NSString; fromExternalRecordsDirectory: NSURL; toURL: NSURL; options: NSDictionary;
      withType: NSString; error: PPointer): NSPersistentStore; cdecl; // API_DEPRECATED("Spotlight integration is deprecated. Use CoreSpotlight integration instead.", macosx(10.6,10.13))
    function initWithManagedObjectModel(model: NSManagedObjectModel): Pointer; cdecl;
    procedure lock; cdecl; // API_DEPRECATED( "Use -performBlockAndWait: instead", macosx(10.4,10.10), ios(3.0,8.0))
    function managedObjectIDForURIRepresentation(url: NSURL): NSManagedObjectID; cdecl;
    function managedObjectModel: NSManagedObjectModel; cdecl;
    function metadataForPersistentStore(store: NSPersistentStore): NSDictionary; cdecl;
    function migratePersistentStore(store: NSPersistentStore; toURL: NSURL; options: NSDictionary; withType: NSString;
      error: PPointer): NSPersistentStore; cdecl;
    function name: NSString; cdecl;
    procedure performBlock(block: TNSPersistentStoreCoordinatorBlockMethod2); cdecl;
    procedure performBlockAndWait(block: TNSPersistentStoreCoordinatorBlockMethod2); cdecl;
    function persistentStoreForURL(URL: NSURL): NSPersistentStore; cdecl;
    function persistentStores: NSArray; cdecl;
    function removePersistentStore(store: NSPersistentStore; error: PPointer): Boolean; cdecl;
    function replacePersistentStoreAtURL(destinationURL: NSURL; destinationOptions: NSDictionary; withPersistentStoreFromURL: NSURL;
      sourceOptions: NSDictionary; storeType: NSString; error: PPointer): Boolean; cdecl;
    procedure setMetadata(metadata: NSDictionary; forPersistentStore: NSPersistentStore); cdecl;
    procedure setName(name: NSString); cdecl;
    function setURL(url: NSURL; forPersistentStore: NSPersistentStore): Boolean; cdecl;
    function tryLock: Boolean; cdecl; // API_DEPRECATED( "Use -performBlock: instead", macosx(10.4,10.10), ios(3.0,8.0))
    procedure unlock; cdecl; // API_DEPRECATED( "Use -performBlockAndWait: instead", macosx(10.4,10.10), ios(3.0,8.0))
    function URLForPersistentStore(store: NSPersistentStore): NSURL; cdecl;
  end;
  TNSPersistentStoreCoordinator = class(TOCGenericImport<NSPersistentStoreCoordinatorClass, NSPersistentStoreCoordinator>) end;

  NSPersistentStoreClass = interface(NSObjectClass)
    ['{2D02F17A-062E-42B8-9169-EF17C1FC34E5}']
    {class} function metadataForPersistentStoreWithURL(url: NSURL; error: PPointer): NSDictionary; cdecl;
    {class} function migrationManagerClass: Pointer; cdecl;
    {class} function setMetadata(metadata: NSDictionary; forPersistentStoreWithURL: NSURL; error: PPointer): Boolean; cdecl;
  end;

  NSPersistentStore = interface(NSObject)
    ['{295A8FEA-AE9F-4AAA-ADFD-AC55488D09CD}']
    function &type: NSString; cdecl;
    function configurationName: NSString; cdecl;
    function coreSpotlightExporter: NSCoreDataCoreSpotlightDelegate; cdecl;
    procedure didAddToPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator); cdecl;
    function identifier: NSString; cdecl;
    function initWithPersistentStoreCoordinator(root: NSPersistentStoreCoordinator; configurationName: NSString; URL: NSURL;
      options: NSDictionary): Pointer; cdecl;
    function isReadOnly: Boolean; cdecl;
    function loadMetadata(error: PPointer): Boolean; cdecl;
    function metadata: NSDictionary; cdecl;
    function options: NSDictionary; cdecl;
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; cdecl;
    procedure setIdentifier(identifier: NSString); cdecl;
    procedure setMetadata(metadata: NSDictionary); cdecl;
    procedure setReadOnly(readOnly: Boolean); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    function URL: NSURL; cdecl;
    procedure willRemoveFromPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator); cdecl;
  end;
  TNSPersistentStore = class(TOCGenericImport<NSPersistentStoreClass, NSPersistentStore>) end;

  NSAtomicStoreCacheNodeClass = interface(NSObjectClass)
    ['{87BCCD06-CAC9-4E88-ADDC-DA10605C2DF3}']
  end;

  NSAtomicStoreCacheNode = interface(NSObject)
    ['{EE84164D-3AB6-44D2-BFC4-03612017D521}']
    function initWithObjectID(moid: NSManagedObjectID): Pointer; cdecl;
    function objectID: NSManagedObjectID; cdecl;
    function propertyCache: NSMutableDictionary; cdecl;
    procedure setPropertyCache(propertyCache: NSMutableDictionary); cdecl;
    procedure setValue(value: Pointer; forKey: NSString); cdecl;
    function valueForKey(key: NSString): Pointer; cdecl;
  end;
  TNSAtomicStoreCacheNode = class(TOCGenericImport<NSAtomicStoreCacheNodeClass, NSAtomicStoreCacheNode>) end;

  NSAtomicStoreClass = interface(NSPersistentStoreClass)
    ['{3F3870E5-E231-41BF-8FF8-580218FD8FC9}']
  end;

  NSAtomicStore = interface(NSPersistentStore)
    ['{377D7D4A-12E7-4268-A0AE-43049C2A8E25}']
    procedure addCacheNodes(cacheNodes: NSSet); cdecl;
    function cacheNodeForObjectID(objectID: NSManagedObjectID): NSAtomicStoreCacheNode; cdecl;
    function cacheNodes: NSSet; cdecl;
    function initWithPersistentStoreCoordinator(coordinator: NSPersistentStoreCoordinator; configurationName: NSString; URL: NSURL;
      options: NSDictionary): Pointer; cdecl;
    function load(error: PPointer): Boolean; cdecl;
    function newCacheNodeForManagedObject(managedObject: NSManagedObject): NSAtomicStoreCacheNode; cdecl;
    function newReferenceObjectForManagedObject(managedObject: NSManagedObject): Pointer; cdecl;
    function objectIDForEntity(entity: NSEntityDescription; referenceObject: Pointer): NSManagedObjectID; cdecl;
    function referenceObjectForObjectID(objectID: NSManagedObjectID): Pointer; cdecl;
    function save(error: PPointer): Boolean; cdecl;
    procedure updateCacheNode(node: NSAtomicStoreCacheNode; fromManagedObject: NSManagedObject); cdecl;
    procedure willRemoveCacheNodes(cacheNodes: NSSet); cdecl;
  end;
  TNSAtomicStore = class(TOCGenericImport<NSAtomicStoreClass, NSAtomicStore>) end;

  NSEntityMigrationPolicyClass = interface(NSObjectClass)
    ['{B15BF297-418D-4607-A540-F0C1283DA86E}']
  end;

  NSEntityMigrationPolicy = interface(NSObject)
    ['{2A5D4550-C27A-421C-800D-9E201872C3A5}']
    function beginEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: PPointer): Boolean; cdecl;
    function createDestinationInstancesForSourceInstance(sInstance: NSManagedObject; entityMapping: NSEntityMapping; manager: NSMigrationManager;
      error: PPointer): Boolean; cdecl;
    function createRelationshipsForDestinationInstance(dInstance: NSManagedObject; entityMapping: NSEntityMapping; manager: NSMigrationManager;
      error: PPointer): Boolean; cdecl;
    function endEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: PPointer): Boolean; cdecl;
    function endInstanceCreationForEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: PPointer): Boolean; cdecl;
    function endRelationshipCreationForEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: PPointer): Boolean; cdecl;
    function performCustomValidationForEntityMapping(mapping: NSEntityMapping; manager: NSMigrationManager; error: PPointer): Boolean; cdecl;
  end;
  TNSEntityMigrationPolicy = class(TOCGenericImport<NSEntityMigrationPolicyClass, NSEntityMigrationPolicy>) end;

  NSMappingModelClass = interface(NSObjectClass)
    ['{20D433ED-3640-438C-A790-FAAD28269A22}']
    {class} function inferredMappingModelForSourceModel(sourceModel: NSManagedObjectModel; destinationModel: NSManagedObjectModel;
      error: PPointer): NSMappingModel; cdecl;
    {class} function mappingModelFromBundles(bundles: NSArray; forSourceModel: NSManagedObjectModel;
      destinationModel: NSManagedObjectModel): NSMappingModel; cdecl;
  end;

  NSMappingModel = interface(NSObject)
    ['{046A8DCA-DA03-4C38-B326-144136503E5E}']
    function entityMappings: NSArray; cdecl;
    function entityMappingsByName: NSDictionary; cdecl;
    function initWithContentsOfURL(url: NSURL): Pointer; cdecl;
    procedure setEntityMappings(entityMappings: NSArray); cdecl;
  end;
  TNSMappingModel = class(TOCGenericImport<NSMappingModelClass, NSMappingModel>) end;

  NSEntityMappingClass = interface(NSObjectClass)
    ['{60CFA8EB-51A1-45B3-B6D1-9434DB686F6A}']
  end;

  NSEntityMapping = interface(NSObject)
    ['{E73C858D-7DAD-477E-9CF8-39D6852CFDE4}']
    function attributeMappings: NSArray; cdecl;
    function destinationEntityName: NSString; cdecl;
    function destinationEntityVersionHash: NSData; cdecl;
    function entityMigrationPolicyClassName: NSString; cdecl;
    function mappingType: NSEntityMappingType; cdecl;
    function name: NSString; cdecl;
    function relationshipMappings: NSArray; cdecl;
    procedure setAttributeMappings(attributeMappings: NSArray); cdecl;
    procedure setDestinationEntityName(destinationEntityName: NSString); cdecl;
    procedure setDestinationEntityVersionHash(destinationEntityVersionHash: NSData); cdecl;
    procedure setEntityMigrationPolicyClassName(entityMigrationPolicyClassName: NSString); cdecl;
    procedure setMappingType(mappingType: NSEntityMappingType); cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setRelationshipMappings(relationshipMappings: NSArray); cdecl;
    procedure setSourceEntityName(sourceEntityName: NSString); cdecl;
    procedure setSourceEntityVersionHash(sourceEntityVersionHash: NSData); cdecl;
    procedure setSourceExpression(sourceExpression: NSExpression); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    function sourceEntityName: NSString; cdecl;
    function sourceEntityVersionHash: NSData; cdecl;
    function sourceExpression: NSExpression; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TNSEntityMapping = class(TOCGenericImport<NSEntityMappingClass, NSEntityMapping>) end;

  NSPropertyMappingClass = interface(NSObjectClass)
    ['{4DBFF3B6-59B0-4A36-B48A-1062E8BF20B7}']
  end;

  NSPropertyMapping = interface(NSObject)
    ['{86919D13-625A-41A1-9EDB-3322B9ED3B9B}']
    function name: NSString; cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    procedure setValueExpression(valueExpression: NSExpression); cdecl;
    function userInfo: NSDictionary; cdecl;
    function valueExpression: NSExpression; cdecl;
  end;
  TNSPropertyMapping = class(TOCGenericImport<NSPropertyMappingClass, NSPropertyMapping>) end;

  NSMigrationManagerClass = interface(NSObjectClass)
    ['{296C245C-4C89-4163-B56D-7EC36199D60F}']
  end;

  NSMigrationManager = interface(NSObject)
    ['{0897216D-880B-4C9C-ADE1-0B718C5F78C5}']
    procedure associateSourceInstance(sourceInstance: NSManagedObject; withDestinationInstance: NSManagedObject;
      forEntityMapping: NSEntityMapping); cdecl;
    procedure cancelMigrationWithError(error: NSError); cdecl;
    function currentEntityMapping: NSEntityMapping; cdecl;
    function destinationContext: NSManagedObjectContext; cdecl;
    function destinationEntityForEntityMapping(mEntity: NSEntityMapping): NSEntityDescription; cdecl;
    function destinationInstancesForEntityMappingNamed(mappingName: NSString; sourceInstances: NSArray): NSArray; cdecl;
    function destinationModel: NSManagedObjectModel; cdecl;
    function initWithSourceModel(sourceModel: NSManagedObjectModel; destinationModel: NSManagedObjectModel): Pointer; cdecl;
    function mappingModel: NSMappingModel; cdecl;
    function migrateStoreFromURL(sourceURL: NSURL; &type: NSString; options: NSDictionary; withMappingModel: NSMappingModel; toDestinationURL: NSURL;
      destinationType: NSString; destinationOptions: NSDictionary; error: PPointer): Boolean; cdecl;
    function migrationProgress: Single; cdecl;
    procedure reset; cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    procedure setUsesStoreSpecificMigrationManager(usesStoreSpecificMigrationManager: Boolean); cdecl;
    function sourceContext: NSManagedObjectContext; cdecl;
    function sourceEntityForEntityMapping(mEntity: NSEntityMapping): NSEntityDescription; cdecl;
    function sourceInstancesForEntityMappingNamed(mappingName: NSString; destinationInstances: NSArray): NSArray; cdecl;
    function sourceModel: NSManagedObjectModel; cdecl;
    function userInfo: NSDictionary; cdecl;
    function usesStoreSpecificMigrationManager: Boolean; cdecl;
  end;
  TNSMigrationManager = class(TOCGenericImport<NSMigrationManagerClass, NSMigrationManager>) end;

  NSIncrementalStoreClass = interface(NSPersistentStoreClass)
    ['{72A85C30-4D8C-43D3-9B3E-1AAA1F17997D}']
    {class} function identifierForNewStoreAtURL(storeURL: NSURL): Pointer; cdecl;
  end;

  NSIncrementalStore = interface(NSPersistentStore)
    ['{013D2697-A523-4653-83C4-E25A87BCC85D}']
    function executeRequest(request: NSPersistentStoreRequest; withContext: NSManagedObjectContext; error: PPointer): Pointer; cdecl;
    function loadMetadata(error: PPointer): Boolean; cdecl;
    procedure managedObjectContextDidRegisterObjectsWithIDs(objectIDs: NSArray); cdecl;
    procedure managedObjectContextDidUnregisterObjectsWithIDs(objectIDs: NSArray); cdecl;
    function newObjectIDForEntity(entity: NSEntityDescription; referenceObject: Pointer): NSManagedObjectID; cdecl;
    function newValueForRelationship(relationship: NSRelationshipDescription; forObjectWithID: NSManagedObjectID;
      withContext: NSManagedObjectContext; error: PPointer): Pointer; cdecl;
    function newValuesForObjectWithID(objectID: NSManagedObjectID; withContext: NSManagedObjectContext;
      error: PPointer): NSIncrementalStoreNode; cdecl;
    function obtainPermanentIDsForObjects(&array: NSArray; error: PPointer): NSArray; cdecl;
    function referenceObjectForObjectID(objectID: NSManagedObjectID): Pointer; cdecl;
  end;
  TNSIncrementalStore = class(TOCGenericImport<NSIncrementalStoreClass, NSIncrementalStore>) end;

  NSIncrementalStoreNodeClass = interface(NSObjectClass)
    ['{17B17D14-5BEB-4A8B-AACC-8536745F50FC}']
  end;

  NSIncrementalStoreNode = interface(NSObject)
    ['{DA02B860-311B-49EA-ADD8-5B1E63E269BB}']
    function initWithObjectID(objectID: NSManagedObjectID; withValues: NSDictionary; version: UInt64): Pointer; cdecl;
    function objectID: NSManagedObjectID; cdecl;
    procedure updateWithValues(values: NSDictionary; version: UInt64); cdecl;
    function valueForPropertyDescription(prop: NSPropertyDescription): Pointer; cdecl;
    function version: UInt64; cdecl;
  end;
  TNSIncrementalStoreNode = class(TOCGenericImport<NSIncrementalStoreNodeClass, NSIncrementalStoreNode>) end;

  NSPersistentStoreResultClass = interface(NSObjectClass)
    ['{3F933C6E-350C-4728-8CDC-163DD73A4750}']
  end;

  NSPersistentStoreResult = interface(NSObject)
    ['{5D41D34A-BEDD-4B1E-9E5B-118FEBCD2CCF}']
  end;
  TNSPersistentStoreResult = class(TOCGenericImport<NSPersistentStoreResultClass, NSPersistentStoreResult>) end;

  NSPersistentStoreAsynchronousResultClass = interface(NSPersistentStoreResultClass)
    ['{839CF446-6D6B-45C6-9A8F-0D0617378244}']
  end;

  NSPersistentStoreAsynchronousResult = interface(NSPersistentStoreResult)
    ['{DC0878F6-CA20-4F68-A62F-1A94C971C573}']
    procedure cancel; cdecl;
    function managedObjectContext: NSManagedObjectContext; cdecl;
    function operationError: NSError; cdecl;
    function progress: NSProgress; cdecl;
  end;
  TNSPersistentStoreAsynchronousResult = class(TOCGenericImport<NSPersistentStoreAsynchronousResultClass, NSPersistentStoreAsynchronousResult>) end;

  NSAsynchronousFetchResultClass = interface(NSPersistentStoreAsynchronousResultClass)
    ['{DF9C7378-1E23-4C09-A0A5-4F6DE25A6DB0}']
  end;

  NSAsynchronousFetchResult = interface(NSPersistentStoreAsynchronousResult)
    ['{EBCED0FA-4A7D-4ED4-A29B-B5870CDFB24A}']
    function fetchRequest: NSAsynchronousFetchRequest; cdecl;
    function finalResult: NSArray; cdecl;
  end;
  TNSAsynchronousFetchResult = class(TOCGenericImport<NSAsynchronousFetchResultClass, NSAsynchronousFetchResult>) end;

  NSBatchInsertResultClass = interface(NSPersistentStoreResultClass)
    ['{2C7DE350-BF2B-4353-AC42-68AEA56DDDB0}']
  end;

  NSBatchInsertResult = interface(NSPersistentStoreResult)
    ['{3782F443-5FBD-422B-87D8-9E31C1D4EA14}']
    function result: Pointer; cdecl;
    function resultType: NSBatchInsertRequestResultType; cdecl;
  end;
  TNSBatchInsertResult = class(TOCGenericImport<NSBatchInsertResultClass, NSBatchInsertResult>) end;

  NSBatchUpdateResultClass = interface(NSPersistentStoreResultClass)
    ['{9D7E05D9-2713-471B-B94D-0174CA1C30B4}']
  end;

  NSBatchUpdateResult = interface(NSPersistentStoreResult)
    ['{EAFC981E-5947-4CFD-ADEB-B2BA58A226C3}']
    function result: Pointer; cdecl;
    function resultType: NSBatchUpdateRequestResultType; cdecl;
  end;
  TNSBatchUpdateResult = class(TOCGenericImport<NSBatchUpdateResultClass, NSBatchUpdateResult>) end;

  NSBatchDeleteResultClass = interface(NSPersistentStoreResultClass)
    ['{CBE245BB-FD23-4137-ACCB-06D53DB31B8F}']
  end;

  NSBatchDeleteResult = interface(NSPersistentStoreResult)
    ['{97DC4EF0-4B3C-4B58-ABF3-08183D8D04CA}']
    function result: Pointer; cdecl;
    function resultType: NSBatchDeleteRequestResultType; cdecl;
  end;
  TNSBatchDeleteResult = class(TOCGenericImport<NSBatchDeleteResultClass, NSBatchDeleteResult>) end;

  NSPersistentHistoryResultClass = interface(NSPersistentStoreResultClass)
    ['{31202F82-4CA7-4D56-A586-015D8D956E1A}']
  end;

  NSPersistentHistoryResult = interface(NSPersistentStoreResult)
    ['{10B46E55-1178-482F-B6E6-DBC2F85052CE}']
    function result: Pointer; cdecl;
    function resultType: NSPersistentHistoryResultType; cdecl;
  end;
  TNSPersistentHistoryResult = class(TOCGenericImport<NSPersistentHistoryResultClass, NSPersistentHistoryResult>) end;

  NSPersistentCloudKitContainerEventResultClass = interface(NSPersistentStoreResultClass)
    ['{680D576C-F8FF-4C98-8F19-97EE35DDAB90}']
    {class} function new: Pointer; cdecl;
  end;

  NSPersistentCloudKitContainerEventResult = interface(NSPersistentStoreResult)
    ['{B9F765FB-F212-43B6-AF9B-3D57D1B657D6}']
    function result: Pointer; cdecl;
    function resultType: NSPersistentCloudKitContainerEventResultType; cdecl;
  end;
  TNSPersistentCloudKitContainerEventResult = class(TOCGenericImport<NSPersistentCloudKitContainerEventResultClass,
    NSPersistentCloudKitContainerEventResult>) end;

  NSSaveChangesRequestClass = interface(NSPersistentStoreRequestClass)
    ['{6FC32851-AA4B-4F5A-94FA-50ABD39A786D}']
  end;

  NSSaveChangesRequest = interface(NSPersistentStoreRequest)
    ['{BB029F22-EC42-4C86-B14D-2A92887866A7}']
    function deletedObjects: NSSet; cdecl;
    function initWithInsertedObjects(insertedObjects: NSSet; updatedObjects: NSSet; deletedObjects: NSSet; lockedObjects: NSSet): Pointer; cdecl;
    function insertedObjects: NSSet; cdecl;
    function lockedObjects: NSSet; cdecl;
    function updatedObjects: NSSet; cdecl;
  end;
  TNSSaveChangesRequest = class(TOCGenericImport<NSSaveChangesRequestClass, NSSaveChangesRequest>) end;

  NSBatchUpdateRequestClass = interface(NSPersistentStoreRequestClass)
    ['{F5CB501E-A43C-435A-BDAC-A98A206891F2}']
    {class} function batchUpdateRequestWithEntityName(entityName: NSString): Pointer; cdecl;
  end;

  NSBatchUpdateRequest = interface(NSPersistentStoreRequest)
    ['{FB10B412-17A3-47CD-81A0-250364026CF3}']
    function entity: NSEntityDescription; cdecl;
    function entityName: NSString; cdecl;
    function includesSubentities: Boolean; cdecl;
    function initWithEntity(entity: NSEntityDescription): Pointer; cdecl;
    function initWithEntityName(entityName: NSString): Pointer; cdecl;
    function predicate: NSPredicate; cdecl;
    function propertiesToUpdate: NSDictionary; cdecl;
    function resultType: NSBatchUpdateRequestResultType; cdecl;
    procedure setIncludesSubentities(includesSubentities: Boolean); cdecl;
    procedure setPredicate(predicate: NSPredicate); cdecl;
    procedure setPropertiesToUpdate(propertiesToUpdate: NSDictionary); cdecl;
    procedure setResultType(resultType: NSBatchUpdateRequestResultType); cdecl;
  end;
  TNSBatchUpdateRequest = class(TOCGenericImport<NSBatchUpdateRequestClass, NSBatchUpdateRequest>) end;

  NSBatchDeleteRequestClass = interface(NSPersistentStoreRequestClass)
    ['{2BBCF92F-33FD-47FD-BAD0-66A5BEF07F22}']
  end;

  NSBatchDeleteRequest = interface(NSPersistentStoreRequest)
    ['{C5879E31-0567-4B4D-9619-756F44BC4B22}']
    function fetchRequest: NSFetchRequest; cdecl;
    function initWithFetchRequest(fetch: NSFetchRequest): Pointer; cdecl;
    function initWithObjectIDs(objects: NSArray): Pointer; cdecl;
    function resultType: NSBatchDeleteRequestResultType; cdecl;
    procedure setResultType(resultType: NSBatchDeleteRequestResultType); cdecl;
  end;
  TNSBatchDeleteRequest = class(TOCGenericImport<NSBatchDeleteRequestClass, NSBatchDeleteRequest>) end;

  NSBatchInsertRequestClass = interface(NSPersistentStoreRequestClass)
    ['{3002B385-EFC2-4BFD-9F4E-BBB90E249EB2}']
    {class} function batchInsertRequestWithEntityName(entityName: NSString;
      managedObjectHandler: TNSBatchInsertRequestBlockMethod2): Pointer; overload; cdecl;
    {class} function batchInsertRequestWithEntityName(entityName: NSString;
      ictionaryHandler: TNSBatchInsertRequestBlockMethod1): Pointer; overload; cdecl;
    {class} function batchInsertRequestWithEntityName(entityName: NSString; objects: NSArray): Pointer; overload; cdecl;
  end;

  NSBatchInsertRequest = interface(NSPersistentStoreRequest)
    ['{F0BE40B2-AFD6-4C88-880C-F5C9A2AA8C70}']
    function dictionaryHandler: TNSBatchInsertRequestBlockMethod3; cdecl;
    function entity: NSEntityDescription; cdecl;
    function entityName: NSString; cdecl;
    function initWithEntity(entity: NSEntityDescription; objects: NSArray): Pointer; overload; cdecl;
    function initWithEntity(entity: NSEntityDescription; managedObjectHandler: TNSBatchInsertRequestBlockMethod2): Pointer; overload; cdecl;
    function initWithEntity(entity: NSEntityDescription; dictionaryHandler: TNSBatchInsertRequestBlockMethod1): Pointer; overload; cdecl;
    function initWithEntityName(entityName: NSString; objects: NSArray): Pointer; overload; cdecl;
    function initWithEntityName(entityName: NSString; managedObjectHandler: TNSBatchInsertRequestBlockMethod2): Pointer; overload; cdecl;
    function initWithEntityName(entityName: NSString; dictionaryHandler: TNSBatchInsertRequestBlockMethod1): Pointer; overload; cdecl;
    function managedObjectHandler: TNSBatchInsertRequestBlockMethod5; cdecl;
    function objectsToInsert: NSArray; cdecl;
    function resultType: NSBatchInsertRequestResultType; cdecl;
    procedure setDictionaryHandler(dictionaryHandler: TNSBatchInsertRequestBlockMethod4); cdecl;
    procedure setManagedObjectHandler(managedObjectHandler: TNSBatchInsertRequestBlockMethod4); cdecl;
    procedure setObjectsToInsert(objectsToInsert: NSArray); cdecl;
    procedure setResultType(resultType: NSBatchInsertRequestResultType); cdecl;
  end;
  TNSBatchInsertRequest = class(TOCGenericImport<NSBatchInsertRequestClass, NSBatchInsertRequest>) end;

  NSMergeConflictClass = interface(NSObjectClass)
    ['{2ADA1C42-E8DD-46E2-970B-DA708F28AD47}']
  end;

  NSMergeConflict = interface(NSObject)
    ['{1611007D-96B9-4191-A6B7-1360879708D5}']
    function cachedSnapshot: NSDictionary; cdecl;
    function initWithSource(srcObject: NSManagedObject; newVersion: NSUInteger; oldVersion: NSUInteger; cachedSnapshot: NSDictionary;
      persistedSnapshot: NSDictionary): Pointer; cdecl;
    function newVersionNumber: NSUInteger; cdecl;
    function objectSnapshot: NSDictionary; cdecl;
    function oldVersionNumber: NSUInteger; cdecl;
    function persistedSnapshot: NSDictionary; cdecl;
    function sourceObject: NSManagedObject; cdecl;
  end;
  TNSMergeConflict = class(TOCGenericImport<NSMergeConflictClass, NSMergeConflict>) end;

  NSConstraintConflictClass = interface(NSObjectClass)
    ['{AFA0014B-0D05-4C00-AA05-3B3A40CC2C64}']
  end;

  NSConstraintConflict = interface(NSObject)
    ['{66CA96DA-EFF7-4E53-8109-738E6E965F42}']
    function conflictingObjects: NSArray; cdecl;
    function conflictingSnapshots: NSArray; cdecl;
    function constraint: NSArray; cdecl;
    function constraintValues: NSDictionary; cdecl;
    function databaseObject: NSManagedObject; cdecl;
    function databaseSnapshot: NSDictionary; cdecl;
    function initWithConstraint(contraint: NSArray; databaseObject: NSManagedObject; databaseSnapshot: NSDictionary; conflictingObjects: NSArray;
      conflictingSnapshots: NSArray): Pointer; cdecl;
  end;
  TNSConstraintConflict = class(TOCGenericImport<NSConstraintConflictClass, NSConstraintConflict>) end;

  NSMergePolicyClass = interface(NSObjectClass)
    ['{A6D4EA82-A959-49F3-8200-DF9838C1E126}']
    {class} function errorMergePolicy: NSMergePolicy; cdecl;
    {class} function mergeByPropertyObjectTrumpMergePolicy: NSMergePolicy; cdecl;
    {class} function mergeByPropertyStoreTrumpMergePolicy: NSMergePolicy; cdecl;
    {class} function overwriteMergePolicy: NSMergePolicy; cdecl;
    {class} function rollbackMergePolicy: NSMergePolicy; cdecl;
  end;

  NSMergePolicy = interface(NSObject)
    ['{B32E1AAE-33B2-4D3B-91A8-9B4980943842}']
    function initWithMergeType(ty: NSMergePolicyType): Pointer; cdecl;
    function mergeType: NSMergePolicyType; cdecl;
    function resolveConflicts(list: NSArray; error: PPointer): Boolean; cdecl;
    function resolveConstraintConflicts(list: NSArray; error: PPointer): Boolean; cdecl;
    function resolveOptimisticLockingVersionConflicts(list: NSArray; error: PPointer): Boolean; cdecl;
  end;
  TNSMergePolicy = class(TOCGenericImport<NSMergePolicyClass, NSMergePolicy>) end;

  NSFetchedResultsControllerClass = interface(NSObjectClass)
    ['{A362BBC4-7074-4A4F-AAB0-89A440021E46}']
    {class} procedure deleteCacheWithName(name: NSString); cdecl;
  end;

  NSFetchedResultsController = interface(NSObject)
    ['{1B3E022C-60DE-46EB-B601-ED39772C5A65}']
    function cacheName: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function fetchedObjects: NSArray; cdecl;
    function fetchRequest: NSFetchRequest; cdecl;
    function indexPathForObject(&object: Pointer): NSIndexPath; cdecl;
    function initWithFetchRequest(fetchRequest: NSFetchRequest; managedObjectContext: NSManagedObjectContext; sectionNameKeyPath: NSString;
      cacheName: NSString): Pointer; cdecl;
    function managedObjectContext: NSManagedObjectContext; cdecl;
    function objectAtIndexPath(indexPath: NSIndexPath): Pointer; cdecl;
    function performFetch(error: PPointer): Boolean; cdecl;
    function sectionForSectionIndexTitle(title: NSString; atIndex: NSInteger): NSInteger; cdecl;
    function sectionIndexTitleForSectionName(sectionName: NSString): NSString; cdecl;
    function sectionIndexTitles: NSArray; cdecl;
    function sectionNameKeyPath: NSString; cdecl;
    function sections: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TNSFetchedResultsController = class(TOCGenericImport<NSFetchedResultsControllerClass, NSFetchedResultsController>) end;

  NSFetchedResultsSectionInfo = interface(IObjectiveC)
    ['{07CB7351-1E53-4338-A0FD-0B65F4F35697}']
    function indexTitle: NSString; cdecl;
    function name: NSString; cdecl;
    function numberOfObjects: NSUInteger; cdecl;
    function objects: NSArray; cdecl;
  end;

  NSFetchedResultsControllerDelegate = interface(IObjectiveC)
    ['{96EE84E1-002C-45EA-8FB6-7FF084A2E562}']
    function controller(controller: NSFetchedResultsController; sectionIndexTitleForSectionName: NSString): NSString; overload; cdecl;
    procedure controller(controller: NSFetchedResultsController; didChangeSection: Pointer; atIndex: NSUInteger;
      forChangeType: NSFetchedResultsChangeType); overload; cdecl;
    procedure controller(controller: NSFetchedResultsController; didChangeObject: Pointer; atIndexPath: NSIndexPath;
      forChangeType: NSFetchedResultsChangeType; newIndexPath: NSIndexPath); overload; cdecl;
    // TODO: Circular ref with AppKit
    // procedure controller(controller: NSFetchedResultsController; didChangeContentWithSnapshot: NSDiffableDataSourceSnapshot); overload; cdecl;
    // TODO: Circular ref with AppKit
    // procedure controller(controller: NSFetchedResultsController; didChangeContentWithDifference: NSOrderedCollectionDifference); overload; cdecl;
    procedure controllerDidChangeContent(controller: NSFetchedResultsController); cdecl;
    procedure controllerWillChangeContent(controller: NSFetchedResultsController); cdecl;
  end;

  NSQueryGenerationTokenClass = interface(NSObjectClass)
    ['{C8C1A6F1-9CA4-4110-A92F-E6064215147E}']
    {class} function currentQueryGenerationToken: NSQueryGenerationToken; cdecl;
  end;

  NSQueryGenerationToken = interface(NSObject)
    ['{C6196428-4961-4089-9B9B-49BA80631D4C}']
  end;
  TNSQueryGenerationToken = class(TOCGenericImport<NSQueryGenerationTokenClass, NSQueryGenerationToken>) end;

  NSPersistentStoreDescriptionClass = interface(NSObjectClass)
    ['{78C4207C-240D-4464-A083-EBB35A4BC065}']
    {class} function persistentStoreDescriptionWithURL(URL: NSURL): Pointer; cdecl;
  end;

  NSPersistentStoreDescription = interface(NSObject)
    ['{4E7ED758-2777-447A-B096-DDCA67064E6F}']
    function &type: NSString; cdecl;
    function cloudKitContainerOptions: NSPersistentCloudKitContainerOptions; cdecl;
    function configuration: NSString; cdecl;
    function initWithURL(url: NSURL): Pointer; cdecl;
    function isReadOnly: Boolean; cdecl;
    function options: NSDictionary; cdecl;
    procedure setCloudKitContainerOptions(cloudKitContainerOptions: NSPersistentCloudKitContainerOptions); cdecl;
    procedure setConfiguration(configuration: NSString); cdecl;
    procedure setOption(option: NSObject; forKey: NSString); cdecl;
    procedure setReadOnly(readOnly: Boolean); cdecl;
    procedure setShouldAddStoreAsynchronously(shouldAddStoreAsynchronously: Boolean); cdecl;
    procedure setShouldInferMappingModelAutomatically(shouldInferMappingModelAutomatically: Boolean); cdecl;
    procedure setShouldMigrateStoreAutomatically(shouldMigrateStoreAutomatically: Boolean); cdecl;
    procedure setTimeout(timeout: NSTimeInterval); cdecl;
    procedure setType(&type: NSString); cdecl;
    procedure setURL(URL: NSURL); cdecl;
    procedure setValue(value: NSObject; forPragmaNamed: NSString); cdecl;
    function shouldAddStoreAsynchronously: Boolean; cdecl;
    function shouldInferMappingModelAutomatically: Boolean; cdecl;
    function shouldMigrateStoreAutomatically: Boolean; cdecl;
    function sqlitePragmas: NSDictionary; cdecl;
    function timeout: NSTimeInterval; cdecl;
    function URL: NSURL; cdecl;
  end;
  TNSPersistentStoreDescription = class(TOCGenericImport<NSPersistentStoreDescriptionClass, NSPersistentStoreDescription>) end;

  NSPersistentContainerClass = interface(NSObjectClass)
    ['{78661F19-AE76-4E12-99AD-A08807067FB6}']
    {class} function defaultDirectoryURL: NSURL; cdecl;
    {class} function persistentContainerWithName(name: NSString; managedObjectModel: NSManagedObjectModel): Pointer; overload; cdecl;
    {class} function persistentContainerWithName(name: NSString): Pointer; overload; cdecl;
  end;

  NSPersistentContainer = interface(NSObject)
    ['{990F2AB5-25AF-4826-81D8-6BF320268BF1}']
    function initWithName(name: NSString): Pointer; overload; cdecl;
    function initWithName(name: NSString; managedObjectModel: NSManagedObjectModel): Pointer; overload; cdecl;
    procedure loadPersistentStoresWithCompletionHandler(block: TNSPersistentContainerBlockMethod1); cdecl;
    function managedObjectModel: NSManagedObjectModel; cdecl;
    function name: NSString; cdecl;
    function newBackgroundContext: NSManagedObjectContext; cdecl;
    procedure performBackgroundTask(block: TNSPersistentContainerBlockMethod2); cdecl;
    function persistentStoreCoordinator: NSPersistentStoreCoordinator; cdecl;
    function persistentStoreDescriptions: NSArray; cdecl;
    procedure setPersistentStoreDescriptions(persistentStoreDescriptions: NSArray); cdecl;
    function viewContext: NSManagedObjectContext; cdecl;
  end;
  TNSPersistentContainer = class(TOCGenericImport<NSPersistentContainerClass, NSPersistentContainer>) end;

  NSPersistentHistoryChangeClass = interface(NSObjectClass)
    ['{FC456A9B-28A6-4FFF-AAF5-218C573713BE}']
    {class} function entityDescription: NSEntityDescription; cdecl;
    {class} function entityDescriptionWithContext(context: NSManagedObjectContext): NSEntityDescription; cdecl;
    {class} function fetchRequest: NSFetchRequest; cdecl;
  end;

  NSPersistentHistoryChange = interface(NSObject)
    ['{21623F04-20D4-4267-852C-12756839459F}']
    function changedObjectID: NSManagedObjectID; cdecl;
    function changeID: Int64; cdecl;
    function changeType: NSPersistentHistoryChangeType; cdecl;
    function tombstone: NSDictionary; cdecl;
    function transaction: NSPersistentHistoryTransaction; cdecl;
    function updatedProperties: NSSet; cdecl;
  end;
  TNSPersistentHistoryChange = class(TOCGenericImport<NSPersistentHistoryChangeClass, NSPersistentHistoryChange>) end;

  NSPersistentHistoryChangeRequestClass = interface(NSPersistentStoreRequestClass)
    ['{8F6F5A6D-EDA1-4F18-B5C6-5E091C398318}']
    {class} function deleteHistoryBeforeDate(date: NSDate): Pointer; cdecl;
    {class} function deleteHistoryBeforeToken(token: NSPersistentHistoryToken): Pointer; cdecl;
    {class} function deleteHistoryBeforeTransaction(transaction: NSPersistentHistoryTransaction): Pointer; cdecl;
    {class} function fetchHistoryAfterDate(date: NSDate): Pointer; cdecl;
    {class} function fetchHistoryAfterToken(token: NSPersistentHistoryToken): Pointer; cdecl;
    {class} function fetchHistoryAfterTransaction(transaction: NSPersistentHistoryTransaction): Pointer; cdecl;
    {class} function fetchHistoryWithFetchRequest(fetchRequest: NSFetchRequest): Pointer; cdecl;
  end;

  NSPersistentHistoryChangeRequest = interface(NSPersistentStoreRequest)
    ['{A8C37956-E763-4EBC-AA78-CD15F47F832A}']
    function fetchRequest: NSFetchRequest; cdecl;
    function resultType: NSPersistentHistoryResultType; cdecl;
    procedure setFetchRequest(fetchRequest: NSFetchRequest); cdecl;
    procedure setResultType(resultType: NSPersistentHistoryResultType); cdecl;
    function token: NSPersistentHistoryToken; cdecl;
  end;
  TNSPersistentHistoryChangeRequest = class(TOCGenericImport<NSPersistentHistoryChangeRequestClass, NSPersistentHistoryChangeRequest>) end;

  NSPersistentHistoryTokenClass = interface(NSObjectClass)
    ['{E7ADF70E-662D-44FC-82E0-74FB6A60838D}']
  end;

  NSPersistentHistoryToken = interface(NSObject)
    ['{BA18F301-CC8C-476C-BF97-3D0AB36AEC51}']
  end;
  TNSPersistentHistoryToken = class(TOCGenericImport<NSPersistentHistoryTokenClass, NSPersistentHistoryToken>) end;

  NSPersistentHistoryTransactionClass = interface(NSObjectClass)
    ['{8BC260ED-79C5-4DE5-96AE-C5E6E124F4CA}']
    {class} function entityDescription: NSEntityDescription; cdecl;
    {class} function entityDescriptionWithContext(context: NSManagedObjectContext): NSEntityDescription; cdecl;
    {class} function fetchRequest: NSFetchRequest; cdecl;
  end;

  NSPersistentHistoryTransaction = interface(NSObject)
    ['{929E817E-E412-49AD-B9C6-BAEA76151BF7}']
    function author: NSString; cdecl;
    function bundleID: NSString; cdecl;
    function changes: NSArray; cdecl;
    function contextName: NSString; cdecl;
    function objectIDNotification: NSNotification; cdecl;
    function processID: NSString; cdecl;
    function storeID: NSString; cdecl;
    function timestamp: NSDate; cdecl;
    function token: NSPersistentHistoryToken; cdecl;
    function transactionNumber: Int64; cdecl;
  end;
  TNSPersistentHistoryTransaction = class(TOCGenericImport<NSPersistentHistoryTransactionClass, NSPersistentHistoryTransaction>) end;

  NSPersistentCloudKitContainerClass = interface(NSPersistentContainerClass)
    ['{82AB46FA-2CFE-444D-B5B4-AF76F990BCA9}']
  end;

  NSPersistentCloudKitContainer = interface(NSPersistentContainer)
    ['{8AFF0D00-8198-4A53-90A6-B01B3FBD453E}']
    function canDeleteRecordForManagedObjectWithID(objectID: NSManagedObjectID): Boolean; cdecl;
    function canModifyManagedObjectsInStore(store: NSPersistentStore): Boolean; cdecl;
    function canUpdateRecordForManagedObjectWithID(objectID: NSManagedObjectID): Boolean; cdecl;
    function initializeCloudKitSchemaWithOptions(options: NSPersistentCloudKitContainerSchemaInitializationOptions; error: PPointer): Boolean; cdecl;
    function recordForManagedObjectID(managedObjectID: NSManagedObjectID): CKRecord; cdecl;
    function recordIDForManagedObjectID(managedObjectID: NSManagedObjectID): CKRecordID; cdecl;
    function recordIDsForManagedObjectIDs(managedObjectIDs: NSArray): NSDictionary; cdecl;
    function recordsForManagedObjectIDs(managedObjectIDs: NSArray): NSDictionary; cdecl;
  end;
  TNSPersistentCloudKitContainer = class(TOCGenericImport<NSPersistentCloudKitContainerClass, NSPersistentCloudKitContainer>) end;

  NSPersistentCloudKitContainerOptionsClass = interface(NSObjectClass)
    ['{B0549C47-FACE-4382-AB2E-F953C3277E2E}']
  end;

  NSPersistentCloudKitContainerOptions = interface(NSObject)
    ['{E72E3AF0-95D6-419C-91B0-142B033A342D}']
    function containerIdentifier: NSString; cdecl;
    function databaseScope: CKDatabaseScope; cdecl;
    function initWithContainerIdentifier(containerIdentifier: NSString): Pointer; cdecl;
    procedure setDatabaseScope(databaseScope: CKDatabaseScope); cdecl;
  end;
  TNSPersistentCloudKitContainerOptions = class(TOCGenericImport<NSPersistentCloudKitContainerOptionsClass, NSPersistentCloudKitContainerOptions>) end;

  NSPersistentCloudKitContainerEventClass = interface(NSObjectClass)
    ['{68F62D9A-51B4-4285-B667-B3BB790D55F6}']
    {class} function new: Pointer; cdecl;
  end;

  NSPersistentCloudKitContainerEvent = interface(NSObject)
    ['{48778274-62E4-431D-B387-57F3A09B9D93}']
    function &type: NSPersistentCloudKitContainerEventType; cdecl;
    function endDate: NSDate; cdecl;
    function error: NSError; cdecl;
    function identifier: NSUUID; cdecl;
    function startDate: NSDate; cdecl;
    function storeIdentifier: NSString; cdecl;
    function succeeded: Boolean; cdecl;
  end;
  TNSPersistentCloudKitContainerEvent = class(TOCGenericImport<NSPersistentCloudKitContainerEventClass, NSPersistentCloudKitContainerEvent>) end;

  NSPersistentCloudKitContainerEventRequestClass = interface(NSPersistentStoreRequestClass)
    ['{80610FDA-825A-4319-9F29-EFD20852DB81}']
    {class} function fetchEventsAfterDate(date: NSDate): Pointer; cdecl;
    {class} function fetchEventsAfterEvent(event: NSPersistentCloudKitContainerEvent): Pointer; cdecl;
    {class} function fetchEventsMatchingFetchRequest(fetchRequest: NSFetchRequest): Pointer; cdecl;
    {class} function fetchRequestForEvents: NSFetchRequest; cdecl;
  end;

  NSPersistentCloudKitContainerEventRequest = interface(NSPersistentStoreRequest)
    ['{087253F5-E03A-46BB-BACA-436E26D65BE8}']
    function resultType: NSPersistentCloudKitContainerEventResultType; cdecl;
    procedure setResultType(resultType: NSPersistentCloudKitContainerEventResultType); cdecl;
  end;
  TNSPersistentCloudKitContainerEventRequest = class(TOCGenericImport<NSPersistentCloudKitContainerEventRequestClass,
    NSPersistentCloudKitContainerEventRequest>) end;

  NSStagedMigrationManagerClass = interface(NSObjectClass)
    ['{895B9044-6C24-4F0B-B168-4437A3926AFE}']
  end;

  NSStagedMigrationManager = interface(NSObject)
    ['{BB3838ED-B139-45E1-B67F-1745B02B33C3}']
    function container: NSPersistentContainer; cdecl;
    function initWithMigrationStages(stages: NSArray): Pointer; cdecl;
    function stages: NSArray; cdecl;
  end;
  TNSStagedMigrationManager = class(TOCGenericImport<NSStagedMigrationManagerClass, NSStagedMigrationManager>) end;

  NSMigrationStageClass = interface(NSObjectClass)
    ['{15FCCCD6-4236-4B77-89E5-66B6A6678D8D}']
  end;

  NSMigrationStage = interface(NSObject)
    ['{5E690CAE-CA77-46D8-B6F9-B1A64D4139B8}']
    function &label: NSString; cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;
  TNSMigrationStage = class(TOCGenericImport<NSMigrationStageClass, NSMigrationStage>) end;

  NSCustomMigrationStageClass = interface(NSMigrationStageClass)
    ['{FD68E369-D193-4DDE-8685-0FCA8FE696E4}']
  end;

  NSCustomMigrationStage = interface(NSMigrationStage)
    ['{1FA6FC9B-6E5A-4BC2-9FE4-1304002AA3A2}']
    function currentModel: NSManagedObjectModelReference; cdecl;
    function didMigrateHandler: TNSCustomMigrationStageBlockMethod1; cdecl;
    function initWithCurrentModelReference(currentModel: NSManagedObjectModelReference;
      nextModelReference: NSManagedObjectModelReference): Pointer; cdecl;
    function nextModel: NSManagedObjectModelReference; cdecl;
    procedure setDidMigrateHandler(didMigrateHandler: TNSCustomMigrationStageBlockMethod2); cdecl;
    procedure setWillMigrateHandler(willMigrateHandler: TNSCustomMigrationStageBlockMethod2); cdecl;
    function willMigrateHandler: TNSCustomMigrationStageBlockMethod1; cdecl;
  end;
  TNSCustomMigrationStage = class(TOCGenericImport<NSCustomMigrationStageClass, NSCustomMigrationStage>) end;

  NSLightweightMigrationStageClass = interface(NSMigrationStageClass)
    ['{882D7EA4-8DB7-4100-9848-09EDF8FA20F8}']
  end;

  NSLightweightMigrationStage = interface(NSMigrationStage)
    ['{C6AF38CD-73A8-43E0-83C0-AF0953553E4C}']
    function initWithVersionChecksums(versionChecksums: NSArray): Pointer; cdecl;
    function versionChecksums: NSArray; cdecl;
  end;
  TNSLightweightMigrationStage = class(TOCGenericImport<NSLightweightMigrationStageClass, NSLightweightMigrationStage>) end;

  NSManagedObjectModelReferenceClass = interface(NSObjectClass)
    ['{A720D1F8-8DC1-47B1-93D8-68AD61C7845B}']
  end;

  NSManagedObjectModelReference = interface(NSObject)
    ['{4C684A89-3390-45D4-8C61-F9BA9CB49BD8}']
    function initWithEntityVersionHashes(versionHash: NSDictionary; inBundle: NSBundle; versionChecksum: NSString): Pointer; cdecl;
    function initWithFileURL(fileURL: NSURL; versionChecksum: NSString): Pointer; cdecl;
    function initWithModel(model: NSManagedObjectModel; versionChecksum: NSString): Pointer; cdecl;
    function initWithName(modelName: NSString; inBundle: NSBundle; versionChecksum: NSString): Pointer; cdecl;
    function resolvedModel: NSManagedObjectModel; cdecl;
    function versionChecksum: NSString; cdecl;
  end;
  TNSManagedObjectModelReference = class(TOCGenericImport<NSManagedObjectModelReferenceClass, NSManagedObjectModelReference>) end;

  NSCoreDataCoreSpotlightDelegateClass = interface(NSObjectClass)
    ['{37DF33E7-6377-4755-95EF-01CD2B88509C}']
  end;

  NSCoreDataCoreSpotlightDelegate = interface(NSObject)
    ['{99F648F4-208C-4CF9-A4B0-F4D97D1014F1}']
    function attributeSetForObject(&object: NSManagedObject): CSSearchableItemAttributeSet; cdecl;
    procedure deleteSpotlightIndexWithCompletionHandler(completionHandler: TNSCoreDataCoreSpotlightDelegateBlockMethod1); cdecl;
    function domainIdentifier: NSString; cdecl;
    function indexName: NSString; cdecl;
    function initForStoreWithDescription(description: NSPersistentStoreDescription;
      coordinator: NSPersistentStoreCoordinator): Pointer; overload; cdecl;
    function initForStoreWithDescription(description: NSPersistentStoreDescription; model: NSManagedObjectModel): Pointer; overload; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("initForStoreWithDescription:coordinator:", macosx(10.13,12.0),ios(11.0,15.0))
    function isIndexingEnabled: Boolean; cdecl;
    procedure searchableIndex(searchableIndex: CSSearchableIndex;
      reindexAllSearchableItemsWithAcknowledgementHandler: TNSCoreDataCoreSpotlightDelegateBlockMethod2); overload; cdecl;
    procedure searchableIndex(searchableIndex: CSSearchableIndex; reindexSearchableItemsWithIdentifiers: NSArray;
      acknowledgementHandler: TNSCoreDataCoreSpotlightDelegateBlockMethod2); overload; cdecl;
    procedure startSpotlightIndexing; cdecl;
    procedure stopSpotlightIndexing; cdecl;
  end;
  TNSCoreDataCoreSpotlightDelegate = class(TOCGenericImport<NSCoreDataCoreSpotlightDelegateClass, NSCoreDataCoreSpotlightDelegate>) end;

function NSCoreDataVersionNumber: Double;
function NSDetailedErrorsKey: NSString;
function NSValidationObjectErrorKey: NSString;
function NSValidationKeyErrorKey: NSString;
function NSValidationPredicateErrorKey: NSString;
function NSValidationValueErrorKey: NSString;
function NSAffectedStoresErrorKey: NSString;
function NSAffectedObjectsErrorKey: NSString;
function NSPersistentStoreSaveConflictsErrorKey: NSString;
function NSSQLiteErrorDomain: NSString;
function NSManagedObjectContextWillSaveNotification: NSString;
function NSManagedObjectContextDidSaveNotification: NSString;
function NSManagedObjectContextObjectsDidChangeNotification: NSString;
function NSManagedObjectContextDidSaveObjectIDsNotification: NSString;
function NSManagedObjectContextDidMergeChangesObjectIDsNotification: NSString;
function NSInsertedObjectsKey: NSString;
function NSUpdatedObjectsKey: NSString;
function NSDeletedObjectsKey: NSString;
function NSRefreshedObjectsKey: NSString;
function NSInvalidatedObjectsKey: NSString;
function NSManagedObjectContextQueryGenerationKey: NSString;
function NSInvalidatedAllObjectsKey: NSString;
function NSInsertedObjectIDsKey: NSString;
function NSUpdatedObjectIDsKey: NSString;
function NSDeletedObjectIDsKey: NSString;
function NSRefreshedObjectIDsKey: NSString;
function NSInvalidatedObjectIDsKey: NSString;
function NSErrorMergePolicy: Pointer;
function NSMergeByPropertyStoreTrumpMergePolicy: Pointer;
function NSMergeByPropertyObjectTrumpMergePolicy: Pointer;
function NSOverwriteMergePolicy: Pointer;
function NSRollbackMergePolicy: Pointer;
function NSSQLiteStoreType: NSString;
function NSXMLStoreType: NSString;
function NSBinaryStoreType: NSString;
function NSInMemoryStoreType: NSString;
function NSStoreTypeKey: NSString;
function NSStoreUUIDKey: NSString;
function NSPersistentStoreCoordinatorStoresWillChangeNotification: NSString;
function NSPersistentStoreCoordinatorStoresDidChangeNotification: NSString;
function NSPersistentStoreCoordinatorWillRemoveStoreNotification: NSString;
function NSAddedPersistentStoresKey: NSString;
function NSRemovedPersistentStoresKey: NSString;
function NSUUIDChangedPersistentStoresKey: NSString;
function NSReadOnlyPersistentStoreOption: NSString;
function NSValidateXMLStoreOption: NSString;
function NSPersistentStoreTimeoutOption: NSString;
function NSSQLitePragmasOption: NSString;
function NSSQLiteAnalyzeOption: NSString;
function NSSQLiteManualVacuumOption: NSString;
function NSIgnorePersistentStoreVersioningOption: NSString;
function NSMigratePersistentStoresAutomaticallyOption: NSString;
function NSInferMappingModelAutomaticallyOption: NSString;
function NSStoreModelVersionHashesKey: NSString;
function NSStoreModelVersionIdentifiersKey: NSString;
function NSPersistentStoreOSCompatibility: NSString;
function NSPersistentStoreConnectionPoolMaxSizeKey: NSString;
function NSCoreDataCoreSpotlightExporter: NSString;
function NSPersistentStoreStagedMigrationManagerOptionKey: NSString;
function NSXMLExternalRecordType: NSString;
function NSBinaryExternalRecordType: NSString;
function NSExternalRecordsFileFormatOption: NSString;
function NSExternalRecordsDirectoryOption: NSString;
function NSExternalRecordExtensionOption: NSString;
function NSEntityNameInPathKey: NSString;
function NSStoreUUIDInPathKey: NSString;
function NSStorePathKey: NSString;
function NSModelPathKey: NSString;
function NSObjectURIKey: NSString;
function NSPersistentStoreForceDestroyOption: NSString;
function NSPersistentStoreFileProtectionKey: NSString;
function NSPersistentHistoryTrackingKey: NSString;
function NSBinaryStoreSecureDecodingClasses: NSString;
function NSBinaryStoreInsecureDecodingCompatibilityOption: NSString;
function NSPersistentStoreRemoteChangeNotificationPostOptionKey: NSString;
function NSPersistentStoreRemoteChangeNotification: NSString;
function NSPersistentStoreURLKey: NSString;
function NSPersistentHistoryTokenKey: NSString;
function NSPersistentStoreDeferredLightweightMigrationOptionKey: NSString;
function NSPersistentStoreUbiquitousContentNameKey: NSString;
function NSPersistentStoreUbiquitousContentURLKey: NSString;
function NSPersistentStoreDidImportUbiquitousContentChangesNotification: NSString;
function NSPersistentStoreUbiquitousTransitionTypeKey: NSString;
function NSPersistentStoreUbiquitousPeerTokenOption: NSString;
function NSPersistentStoreRemoveUbiquitousMetadataOption: NSString;
function NSPersistentStoreUbiquitousContainerIdentifierKey: NSString;
function NSPersistentStoreRebuildFromUbiquitousContentOption: NSString;
function NSMigrationManagerKey: NSString;
function NSMigrationSourceObjectKey: NSString;
function NSMigrationDestinationObjectKey: NSString;
function NSMigrationEntityMappingKey: NSString;
function NSMigrationPropertyMappingKey: NSString;
function NSMigrationEntityPolicyKey: NSString;

const
  libCoreData = '/System/Library/Frameworks/CoreData.framework/CoreData';

implementation

uses
  System.SysUtils;

var
  CoreDataModule: THandle;

function NSCoreDataVersionNumber: Double;
begin
  Result := CocoaDoubleConst(libCoreData, 'NSCoreDataVersionNumber');
end;

function NSDetailedErrorsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSDetailedErrorsKey');
end;

function NSValidationObjectErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSValidationObjectErrorKey');
end;

function NSValidationKeyErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSValidationKeyErrorKey');
end;

function NSValidationPredicateErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSValidationPredicateErrorKey');
end;

function NSValidationValueErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSValidationValueErrorKey');
end;

function NSAffectedStoresErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSAffectedStoresErrorKey');
end;

function NSAffectedObjectsErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSAffectedObjectsErrorKey');
end;

function NSPersistentStoreSaveConflictsErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreSaveConflictsErrorKey');
end;

function NSSQLiteErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSSQLiteErrorDomain');
end;

function NSManagedObjectContextWillSaveNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSManagedObjectContextWillSaveNotification');
end;

function NSManagedObjectContextDidSaveNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSManagedObjectContextDidSaveNotification');
end;

function NSManagedObjectContextObjectsDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSManagedObjectContextObjectsDidChangeNotification');
end;

function NSManagedObjectContextDidSaveObjectIDsNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSManagedObjectContextDidSaveObjectIDsNotification');
end;

function NSManagedObjectContextDidMergeChangesObjectIDsNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSManagedObjectContextDidMergeChangesObjectIDsNotification');
end;

function NSInsertedObjectsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSInsertedObjectsKey');
end;

function NSUpdatedObjectsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSUpdatedObjectsKey');
end;

function NSDeletedObjectsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSDeletedObjectsKey');
end;

function NSRefreshedObjectsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSRefreshedObjectsKey');
end;

function NSInvalidatedObjectsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSInvalidatedObjectsKey');
end;

function NSManagedObjectContextQueryGenerationKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSManagedObjectContextQueryGenerationKey');
end;

function NSInvalidatedAllObjectsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSInvalidatedAllObjectsKey');
end;

function NSInsertedObjectIDsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSInsertedObjectIDsKey');
end;

function NSUpdatedObjectIDsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSUpdatedObjectIDsKey');
end;

function NSDeletedObjectIDsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSDeletedObjectIDsKey');
end;

function NSRefreshedObjectIDsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSRefreshedObjectIDsKey');
end;

function NSInvalidatedObjectIDsKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSInvalidatedObjectIDsKey');
end;

function NSErrorMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSErrorMergePolicy');
end;

function NSMergeByPropertyStoreTrumpMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSMergeByPropertyStoreTrumpMergePolicy');
end;

function NSMergeByPropertyObjectTrumpMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSMergeByPropertyObjectTrumpMergePolicy');
end;

function NSOverwriteMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSOverwriteMergePolicy');
end;

function NSRollbackMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSRollbackMergePolicy');
end;

function NSSQLiteStoreType: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSSQLiteStoreType');
end;

function NSXMLStoreType: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSXMLStoreType');
end;

function NSBinaryStoreType: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSBinaryStoreType');
end;

function NSInMemoryStoreType: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSInMemoryStoreType');
end;

function NSStoreTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSStoreTypeKey');
end;

function NSStoreUUIDKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSStoreUUIDKey');
end;

function NSPersistentStoreCoordinatorStoresWillChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreCoordinatorStoresWillChangeNotification');
end;

function NSPersistentStoreCoordinatorStoresDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreCoordinatorStoresDidChangeNotification');
end;

function NSPersistentStoreCoordinatorWillRemoveStoreNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreCoordinatorWillRemoveStoreNotification');
end;

function NSAddedPersistentStoresKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSAddedPersistentStoresKey');
end;

function NSRemovedPersistentStoresKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSRemovedPersistentStoresKey');
end;

function NSUUIDChangedPersistentStoresKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSUUIDChangedPersistentStoresKey');
end;

function NSReadOnlyPersistentStoreOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSReadOnlyPersistentStoreOption');
end;

function NSValidateXMLStoreOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSValidateXMLStoreOption');
end;

function NSPersistentStoreTimeoutOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreTimeoutOption');
end;

function NSSQLitePragmasOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSSQLitePragmasOption');
end;

function NSSQLiteAnalyzeOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSSQLiteAnalyzeOption');
end;

function NSSQLiteManualVacuumOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSSQLiteManualVacuumOption');
end;

function NSIgnorePersistentStoreVersioningOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSIgnorePersistentStoreVersioningOption');
end;

function NSMigratePersistentStoresAutomaticallyOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSMigratePersistentStoresAutomaticallyOption');
end;

function NSInferMappingModelAutomaticallyOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSInferMappingModelAutomaticallyOption');
end;

function NSStoreModelVersionHashesKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSStoreModelVersionHashesKey');
end;

function NSStoreModelVersionIdentifiersKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSStoreModelVersionIdentifiersKey');
end;

function NSPersistentStoreOSCompatibility: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreOSCompatibility');
end;

function NSPersistentStoreConnectionPoolMaxSizeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreConnectionPoolMaxSizeKey');
end;

function NSCoreDataCoreSpotlightExporter: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSCoreDataCoreSpotlightExporter');
end;

function NSPersistentStoreStagedMigrationManagerOptionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreStagedMigrationManagerOptionKey');
end;

function NSXMLExternalRecordType: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSXMLExternalRecordType');
end;

function NSBinaryExternalRecordType: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSBinaryExternalRecordType');
end;

function NSExternalRecordsFileFormatOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSExternalRecordsFileFormatOption');
end;

function NSExternalRecordsDirectoryOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSExternalRecordsDirectoryOption');
end;

function NSExternalRecordExtensionOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSExternalRecordExtensionOption');
end;

function NSEntityNameInPathKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSEntityNameInPathKey');
end;

function NSStoreUUIDInPathKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSStoreUUIDInPathKey');
end;

function NSStorePathKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSStorePathKey');
end;

function NSModelPathKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSModelPathKey');
end;

function NSObjectURIKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSObjectURIKey');
end;

function NSPersistentStoreForceDestroyOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreForceDestroyOption');
end;

function NSPersistentStoreFileProtectionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreFileProtectionKey');
end;

function NSPersistentHistoryTrackingKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentHistoryTrackingKey');
end;

function NSBinaryStoreSecureDecodingClasses: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSBinaryStoreSecureDecodingClasses');
end;

function NSBinaryStoreInsecureDecodingCompatibilityOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSBinaryStoreInsecureDecodingCompatibilityOption');
end;

function NSPersistentStoreRemoteChangeNotificationPostOptionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreRemoteChangeNotificationPostOptionKey');
end;

function NSPersistentStoreRemoteChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreRemoteChangeNotification');
end;

function NSPersistentStoreURLKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreURLKey');
end;

function NSPersistentHistoryTokenKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentHistoryTokenKey');
end;

function NSPersistentStoreDeferredLightweightMigrationOptionKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreDeferredLightweightMigrationOptionKey');
end;

function NSPersistentStoreUbiquitousContentNameKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreUbiquitousContentNameKey');
end;

function NSPersistentStoreUbiquitousContentURLKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreUbiquitousContentURLKey');
end;

function NSPersistentStoreDidImportUbiquitousContentChangesNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreDidImportUbiquitousContentChangesNotification');
end;

function NSPersistentStoreUbiquitousTransitionTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreUbiquitousTransitionTypeKey');
end;

function NSPersistentStoreUbiquitousPeerTokenOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreUbiquitousPeerTokenOption');
end;

function NSPersistentStoreRemoveUbiquitousMetadataOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreRemoveUbiquitousMetadataOption');
end;

function NSPersistentStoreUbiquitousContainerIdentifierKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreUbiquitousContainerIdentifierKey');
end;

function NSPersistentStoreRebuildFromUbiquitousContentOption: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentStoreRebuildFromUbiquitousContentOption');
end;

function NSMigrationManagerKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSMigrationManagerKey');
end;

function NSMigrationSourceObjectKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSMigrationSourceObjectKey');
end;

function NSMigrationDestinationObjectKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSMigrationDestinationObjectKey');
end;

function NSMigrationEntityMappingKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSMigrationEntityMappingKey');
end;

function NSMigrationPropertyMappingKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSMigrationPropertyMappingKey');
end;

function NSMigrationEntityPolicyKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSMigrationEntityPolicyKey');
end;

(*
function NSErrorMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSErrorMergePolicy');
end;

function NSMergeByPropertyStoreTrumpMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSMergeByPropertyStoreTrumpMergePolicy');
end;

function NSMergeByPropertyObjectTrumpMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSMergeByPropertyObjectTrumpMergePolicy');
end;

function NSOverwriteMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSOverwriteMergePolicy');
end;

function NSRollbackMergePolicy: Pointer;
begin
  Result := CocoaPointerConst(libCoreData, 'NSRollbackMergePolicy');
end;
*)

function NSPersistentCloudKitContainerEventChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentCloudKitContainerEventChangedNotification');
end;

function NSPersistentCloudKitContainerEventUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentCloudKitContainerEventUserInfoKey');
end;

function NSCoreDataCoreSpotlightDelegateIndexDidUpdateNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSCoreDataCoreSpotlightDelegateIndexDidUpdateNotification');
end;

initialization
  CoreDataModule := LoadLibrary(libCoreData);

finalization
  if CoreDataModule <> 0 then
    FreeLibrary(CoreDataModule);

end.