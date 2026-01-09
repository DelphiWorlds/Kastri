unit DW.iOSapi.CoreData;

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
  iOSapi.CocoaTypes, iOSapi.Foundation,
  //
  DW.iOSapi.Foundation, DW.iOSapi.CloudKit, DW.iOSapi.UIKitExtra;

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

  PPointer = ^Pointer;
  NSAttributeType = NSInteger;
  NSDeleteRule = NSInteger;
  NSFetchIndexElementType = NSInteger;
  NSPersistentStoreRequestType = NSInteger;
  NSSnapshotEventType = NSInteger;
  NSFetchRequestResultType = NSInteger;

  NSPersistentStoreAsynchronousFetchResultCompletionBlock = procedure(result: NSAsynchronousFetchResult) of object;
  NSManagedObjectContextConcurrencyType = NSInteger;
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

  NSPropertyDescriptionClass = interface(NSObjectClass)
    ['{5A0F5AC9-3D9E-46E5-91A5-12945641251E}']
  end;

  NSPropertyDescription = interface(NSObject)
    ['{DB60BF11-1E09-4E59-8F3E-525B9E160C8A}']
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
    ['{B4970BCF-72DC-4045-BE49-D754B70DF793}']
  end;

  NSAttributeDescription = interface(NSPropertyDescription)
    ['{534445A2-19B1-404D-805C-A02E0EA491A1}']
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
    ['{1061E1D0-DF3C-4C72-800F-9A4FA618DD2D}']
  end;

  NSDerivedAttributeDescription = interface(NSAttributeDescription)
    ['{5745EAD5-AD4B-4C61-88C9-4578C6DD7BE6}']
    function derivationExpression: NSExpression; cdecl;
    procedure setDerivationExpression(derivationExpression: NSExpression); cdecl;
  end;
  TNSDerivedAttributeDescription = class(TOCGenericImport<NSDerivedAttributeDescriptionClass, NSDerivedAttributeDescription>) end;

  NSCompositeAttributeDescriptionClass = interface(NSAttributeDescriptionClass)
    ['{0909D954-8ADD-42C7-A5F1-4F7B26CAA8C8}']
  end;

  NSCompositeAttributeDescription = interface(NSAttributeDescription)
    ['{37C826B2-42A4-440D-B810-7B794E18463F}']
    function elements: NSArray; cdecl;
    procedure setElements(elements: NSArray); cdecl;
  end;
  TNSCompositeAttributeDescription = class(TOCGenericImport<NSCompositeAttributeDescriptionClass, NSCompositeAttributeDescription>) end;

  NSEntityDescriptionClass = interface(NSObjectClass)
    ['{75CE9CE2-B884-4BBB-9EA5-E3681D78BFCC}']
    {class} function entityForName(entityName: NSString; inManagedObjectContext: NSManagedObjectContext): NSEntityDescription; cdecl;
    {class} function insertNewObjectForEntityForName(entityName: NSString; inManagedObjectContext: NSManagedObjectContext): NSManagedObject; cdecl;
  end;

  NSEntityDescription = interface(NSObject)
    ['{E6375CFB-F3C6-4732-841C-78527E08D774}']
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
    ['{E6CACA87-F79C-4706-BD0A-9C6AC43DA5D8}']
  end;

  NSFetchedPropertyDescription = interface(NSPropertyDescription)
    ['{9087141F-0FE2-4929-A3A9-BE8FC39DEDC6}']
    function fetchRequest: NSFetchRequest; cdecl;
    procedure setFetchRequest(fetchRequest: NSFetchRequest); cdecl;
  end;
  TNSFetchedPropertyDescription = class(TOCGenericImport<NSFetchedPropertyDescriptionClass, NSFetchedPropertyDescription>) end;

  NSExpressionDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{43D60C62-8F57-4198-A5E2-2FDD25DAA0BE}']
  end;

  NSExpressionDescription = interface(NSPropertyDescription)
    ['{96F8C2D9-E4EA-4A8E-86BF-586022DC777E}']
    function expression: NSExpression; cdecl;
    function expressionResultType: NSAttributeType; cdecl;
    procedure setExpression(expression: NSExpression); cdecl;
    procedure setExpressionResultType(expressionResultType: NSAttributeType); cdecl;
  end;
  TNSExpressionDescription = class(TOCGenericImport<NSExpressionDescriptionClass, NSExpressionDescription>) end;

  NSRelationshipDescriptionClass = interface(NSPropertyDescriptionClass)
    ['{CCD0B78D-FE3D-4F68-BEB3-460F16908622}']
  end;

  NSRelationshipDescription = interface(NSPropertyDescription)
    ['{AE6BE0CD-5C38-4344-BE85-A01BF15930AD}']
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
    ['{ED955A97-B36A-4B6A-BAAD-25F2C991E71D}']
  end;

  NSFetchIndexDescription = interface(NSObject)
    ['{440ECABA-A907-42C8-B5BF-1C852B2060AB}']
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
    ['{1BBD1FEA-3864-49CE-BE3B-BE083548DF59}']
  end;

  NSFetchIndexElementDescription = interface(NSObject)
    ['{F94A1F51-50AB-4C73-87C8-AD3F72EC5007}']
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
    ['{8AB2D8B9-BB99-4526-BDD1-D47F31730032}']
  end;

  NSPersistentStoreRequest = interface(NSObject)
    ['{FF1D7617-2696-4001-A2E5-0A637DBE39DE}']
    function affectedStores: NSArray; cdecl;
    function requestType: NSPersistentStoreRequestType; cdecl;
    procedure setAffectedStores(affectedStores: NSArray); cdecl;
  end;
  TNSPersistentStoreRequest = class(TOCGenericImport<NSPersistentStoreRequestClass, NSPersistentStoreRequest>) end;

  NSManagedObjectClass = interface(NSObjectClass)
    ['{F44A8508-A099-46DD-88A9-45ED9599918F}']
    {class} function contextShouldIgnoreUnmodeledPropertyChanges: Boolean; cdecl;
    {class} function entity: NSEntityDescription; cdecl;
    {class} function fetchRequest: NSFetchRequest; cdecl;
  end;

  NSManagedObject = interface(NSObject)
    ['{771D4534-236E-48B4-A648-83E13247EE7B}']
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
    procedure prepareForDeletion; cdecl;
    function primitiveValueForKey(key: NSString): Pointer; cdecl;
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
    ['{08325E86-E116-434E-B9FB-F894069525AF}']
  end;

  NSManagedObjectID = interface(NSObject)
    ['{E7F3310E-78BA-42EF-9778-C65DC6891AD6}']
    function entity: NSEntityDescription; cdecl;
    function isTemporaryID: Boolean; cdecl;
    function persistentStore: NSPersistentStore; cdecl;
    function URIRepresentation: NSURL; cdecl;
  end;
  TNSManagedObjectID = class(TOCGenericImport<NSManagedObjectIDClass, NSManagedObjectID>) end;

  NSFetchRequestResult = interface(IObjectiveC)
    ['{560B4410-5653-4B97-B53E-799B0A71D7C1}']
  end;

  NSFetchRequestClass = interface(NSPersistentStoreRequestClass)
    ['{1A85D54B-EA44-47C7-A42A-FED559C389A3}']
    {class} function fetchRequestWithEntityName(entityName: NSString): Pointer; cdecl;
  end;

  NSFetchRequest = interface(NSPersistentStoreRequest)
    ['{89313CB4-62CC-4E97-AA7E-F86F46D0E586}']
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
    ['{B90C43A0-852B-4F37-811F-9F9456E81500}']
  end;

  NSAsynchronousFetchRequest = interface(NSPersistentStoreRequest)
    ['{29F2E610-FC01-4F4B-82F4-05BDFDEF3670}']
    function completionBlock: NSPersistentStoreAsynchronousFetchResultCompletionBlock; cdecl;
    function estimatedResultCount: NSInteger; cdecl;
    function fetchRequest: NSFetchRequest; cdecl;
    function initWithFetchRequest(request: NSFetchRequest; completionBlock: TNSAsynchronousFetchRequestBlockMethod1): Pointer; cdecl;
    procedure setEstimatedResultCount(estimatedResultCount: NSInteger); cdecl;
  end;
  TNSAsynchronousFetchRequest = class(TOCGenericImport<NSAsynchronousFetchRequestClass, NSAsynchronousFetchRequest>) end;

  NSFetchRequestExpressionClass = interface(NSExpressionClass)
    ['{7802E81F-8FB6-4EAC-A854-EC74176ED1FE}']
    {class} function expressionForFetch(fetch: NSExpression; context: NSExpression; countOnly: Boolean): NSExpression; cdecl;
  end;

  NSFetchRequestExpression = interface(NSExpression)
    ['{36728FF2-814A-45EE-9EC3-3702C3A88AA8}']
    function contextExpression: NSExpression; cdecl;
    function isCountOnlyRequest: Boolean; cdecl;
    function requestExpression: NSExpression; cdecl;
  end;
  TNSFetchRequestExpression = class(TOCGenericImport<NSFetchRequestExpressionClass, NSFetchRequestExpression>) end;

  NSManagedObjectModelClass = interface(NSObjectClass)
    ['{15A8BA82-FE15-46FB-803D-404B17887616}']
    {class} function checksumsForVersionedModelAtURL(modelURL: NSURL; error: PPointer): NSDictionary; cdecl;
    {class} function mergedModelFromBundles(bundles: NSArray; forStoreMetadata: NSDictionary): NSManagedObjectModel; overload; cdecl;
    {class} function mergedModelFromBundles(bundles: NSArray): NSManagedObjectModel; overload; cdecl;
    {class} function modelByMergingModels(models: NSArray; forStoreMetadata: NSDictionary): NSManagedObjectModel; overload; cdecl;
    {class} function modelByMergingModels(models: NSArray): NSManagedObjectModel; overload; cdecl;
  end;

  NSManagedObjectModel = interface(NSObject)
    ['{A99DEC2F-6FA9-4623-93ED-406FBD05FFBC}']
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
    ['{CEA4C517-7256-4CD2-866C-F4B672AFC58E}']
    {class} procedure mergeChangesFromRemoteContextSave(changeNotificationData: NSDictionary; intoContexts: NSArray); cdecl;
    {class} function new: Pointer; cdecl; // API_DEPRECATED( "Use -initWithConcurrencyType: instead", macosx(10.4,10.11), ios(3.0,9.0))
  end;

  NSManagedObjectContext = interface(NSObject)
    ['{EBF5FB08-110C-40A9-A744-34C7ABDEE8D2}']
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
    procedure undo; cdecl;
    function undoManager: NSUndoManager; cdecl;
    function updatedObjects: NSSet; cdecl;
    function userInfo: NSMutableDictionary; cdecl;
  end;
  TNSManagedObjectContext = class(TOCGenericImport<NSManagedObjectContextClass, NSManagedObjectContext>) end;

  NSPersistentStoreCoordinatorClass = interface(NSObjectClass)
    ['{37BCFD61-E5E0-4448-B0EE-8243E0B5B3E0}']
    {class} function metadataForPersistentStoreOfType(storeType: NSString; URL: NSURL; error: PPointer): NSDictionary; overload; cdecl; // API_DEPRECATED("Use -metadataForPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType", macosx(10.5,10.11), ios(3.0,9.0))
    {class} function metadataForPersistentStoreOfType(storeType: NSString; URL: NSURL; options: NSDictionary;
      error: PPointer): NSDictionary; overload; cdecl;
    {class} function registeredStoreTypes: NSDictionary; cdecl;
    {class} procedure registerStoreClass(storeClass: Pointer; forStoreType: NSString); cdecl;
    {class} function setMetadata(metadata: NSDictionary; forPersistentStoreOfType: NSString; URL: NSURL; error: PPointer): Boolean; overload; cdecl; // API_DEPRECATED("Use  -setMetadata:forPersistentStoreOfType:URL:options:error: and pass in an options dictionary matching addPersistentStoreWithType", macosx(10.5,10.11), ios(3.0,9.0))
    {class} function setMetadata(metadata: NSDictionary; forPersistentStoreOfType: NSString; URL: NSURL; options: NSDictionary;
      error: PPointer): Boolean; overload; cdecl;
  end;

  NSPersistentStoreCoordinator = interface(NSObject)
    ['{D2D8BCC9-DFC9-441F-8F94-60D7E5AC1776}']
    procedure addPersistentStoreWithDescription(storeDescription: NSPersistentStoreDescription;
      completionHandler: TNSPersistentStoreCoordinatorBlockMethod1); cdecl;
    function addPersistentStoreWithType(storeType: NSString; configuration: NSString; URL: NSURL; options: NSDictionary;
      error: PPointer): NSPersistentStore; cdecl;
    function currentPersistentHistoryTokenFromStores(stores: NSArray): NSPersistentHistoryToken; cdecl;
    function destroyPersistentStoreAtURL(url: NSURL; withType: NSString; options: NSDictionary; error: PPointer): Boolean; cdecl;
    function executeRequest(request: NSPersistentStoreRequest; withContext: NSManagedObjectContext; error: PPointer): Pointer; cdecl;
    function finishDeferredLightweightMigration(error: PPointer): Boolean; cdecl;
    function finishDeferredLightweightMigrationTask(error: PPointer): Boolean; cdecl;
    function initWithManagedObjectModel(model: NSManagedObjectModel): Pointer; cdecl;
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
    function URLForPersistentStore(store: NSPersistentStore): NSURL; cdecl;
  end;
  TNSPersistentStoreCoordinator = class(TOCGenericImport<NSPersistentStoreCoordinatorClass, NSPersistentStoreCoordinator>) end;

  NSPersistentStoreClass = interface(NSObjectClass)
    ['{1A5851CC-4305-40EF-B7C2-F4C44F20B653}']
    {class} function metadataForPersistentStoreWithURL(url: NSURL; error: PPointer): NSDictionary; cdecl;
    {class} function migrationManagerClass: Pointer; cdecl;
    {class} function setMetadata(metadata: NSDictionary; forPersistentStoreWithURL: NSURL; error: PPointer): Boolean; cdecl;
  end;

  NSPersistentStore = interface(NSObject)
    ['{F295AFEC-5D7A-4B46-897E-F9B48F7394FC}']
    function &type: NSString; cdecl;
    function configurationName: NSString; cdecl;
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
    ['{447CBACD-2F2D-4DD3-8C6C-D47B309BE5AA}']
  end;

  NSAtomicStoreCacheNode = interface(NSObject)
    ['{E7B0379E-7F5E-4BB5-97C7-0D19B347BF6C}']
    function initWithObjectID(moid: NSManagedObjectID): Pointer; cdecl;
    function objectID: NSManagedObjectID; cdecl;
    function propertyCache: NSMutableDictionary; cdecl;
    procedure setPropertyCache(propertyCache: NSMutableDictionary); cdecl;
    procedure setValue(value: Pointer; forKey: NSString); cdecl;
    function valueForKey(key: NSString): Pointer; cdecl;
  end;
  TNSAtomicStoreCacheNode = class(TOCGenericImport<NSAtomicStoreCacheNodeClass, NSAtomicStoreCacheNode>) end;

  NSAtomicStoreClass = interface(NSPersistentStoreClass)
    ['{D2BB9E6D-3594-4FC5-BEBE-DC26855E6CE5}']
  end;

  NSAtomicStore = interface(NSPersistentStore)
    ['{061AFD59-2AF8-4338-9C0F-505E0A8FF812}']
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
    ['{CA6AABBC-54FF-450F-AC7C-01BD46B17025}']
  end;

  NSEntityMigrationPolicy = interface(NSObject)
    ['{6EF9864B-ED54-409E-9394-67371894C573}']
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
    ['{A914741A-F19F-46BF-AC7A-03D1E0CD3913}']
    {class} function inferredMappingModelForSourceModel(sourceModel: NSManagedObjectModel; destinationModel: NSManagedObjectModel;
      error: PPointer): NSMappingModel; cdecl;
    {class} function mappingModelFromBundles(bundles: NSArray; forSourceModel: NSManagedObjectModel;
      destinationModel: NSManagedObjectModel): NSMappingModel; cdecl;
  end;

  NSMappingModel = interface(NSObject)
    ['{3C6E75B9-C743-42BC-985D-5BDE9161789A}']
    function entityMappings: NSArray; cdecl;
    function entityMappingsByName: NSDictionary; cdecl;
    function initWithContentsOfURL(url: NSURL): Pointer; cdecl;
    procedure setEntityMappings(entityMappings: NSArray); cdecl;
  end;
  TNSMappingModel = class(TOCGenericImport<NSMappingModelClass, NSMappingModel>) end;

  NSEntityMappingClass = interface(NSObjectClass)
    ['{24DF50CA-A5D4-420C-B470-7BF0E062EEE5}']
  end;

  NSEntityMapping = interface(NSObject)
    ['{C3C75E9C-2017-4463-83B5-8230863F058D}']
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
    ['{33D6B263-AD94-40DC-8315-5DABE242A048}']
  end;

  NSPropertyMapping = interface(NSObject)
    ['{8424789D-2032-4F86-AAA3-CF74715350A1}']
    function name: NSString; cdecl;
    procedure setName(name: NSString); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    procedure setValueExpression(valueExpression: NSExpression); cdecl;
    function userInfo: NSDictionary; cdecl;
    function valueExpression: NSExpression; cdecl;
  end;
  TNSPropertyMapping = class(TOCGenericImport<NSPropertyMappingClass, NSPropertyMapping>) end;

  NSMigrationManagerClass = interface(NSObjectClass)
    ['{396D9FA3-BD62-4FB0-8F93-9FE96389431A}']
  end;

  NSMigrationManager = interface(NSObject)
    ['{D0BCE30E-1DAA-4AD1-B89E-FCF1E482B823}']
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
    ['{A14E3E25-58FB-467C-A658-66244DF071E2}']
    {class} function identifierForNewStoreAtURL(storeURL: NSURL): Pointer; cdecl;
  end;

  NSIncrementalStore = interface(NSPersistentStore)
    ['{38725A1C-9659-402C-9C73-36D4454B9016}']
    function executeRequest(request: NSPersistentStoreRequest; withContext: NSManagedObjectContext; error: PPointer): Pointer; cdecl;
    function loadMetadata(error: PPointer): Boolean; cdecl;
    procedure managedObjectContextDidRegisterObjectsWithIDs(objectIDs: NSArray); cdecl;
    procedure managedObjectContextDidUnregisterObjectsWithIDs(objectIDs: NSArray); cdecl;
    function newObjectIDForEntity(entity: NSEntityDescription; referenceObject: Pointer): NSManagedObjectID; cdecl;
    function newValueForRelationship(relationship: NSRelationshipDescription; forObjectWithID: NSManagedObjectID; withContext: NSManagedObjectContext;
      error: PPointer): Pointer; cdecl;
    function newValuesForObjectWithID(objectID: NSManagedObjectID; withContext: NSManagedObjectContext;
      error: PPointer): NSIncrementalStoreNode; cdecl;
    function obtainPermanentIDsForObjects(&array: NSArray; error: PPointer): NSArray; cdecl;
    function referenceObjectForObjectID(objectID: NSManagedObjectID): Pointer; cdecl;
  end;
  TNSIncrementalStore = class(TOCGenericImport<NSIncrementalStoreClass, NSIncrementalStore>) end;

  NSIncrementalStoreNodeClass = interface(NSObjectClass)
    ['{DEB600A9-AE51-4273-9812-37D0EC51D219}']
  end;

  NSIncrementalStoreNode = interface(NSObject)
    ['{D39FAFB8-1006-45AA-804A-33BF6E12F53E}']
    function initWithObjectID(objectID: NSManagedObjectID; withValues: NSDictionary; version: UInt64): Pointer; cdecl;
    function objectID: NSManagedObjectID; cdecl;
    procedure updateWithValues(values: NSDictionary; version: UInt64); cdecl;
    function valueForPropertyDescription(prop: NSPropertyDescription): Pointer; cdecl;
    function version: UInt64; cdecl;
  end;
  TNSIncrementalStoreNode = class(TOCGenericImport<NSIncrementalStoreNodeClass, NSIncrementalStoreNode>) end;

  NSPersistentStoreResultClass = interface(NSObjectClass)
    ['{C0E98520-F8C3-45A3-AFD1-F0382FEDE0C5}']
  end;

  NSPersistentStoreResult = interface(NSObject)
    ['{E2A38734-2E24-4933-97B2-31BC0DB26EA9}']
  end;
  TNSPersistentStoreResult = class(TOCGenericImport<NSPersistentStoreResultClass, NSPersistentStoreResult>) end;

  NSPersistentStoreAsynchronousResultClass = interface(NSPersistentStoreResultClass)
    ['{4D03F42B-62EF-4DE6-B133-75422A6C879B}']
  end;

  NSPersistentStoreAsynchronousResult = interface(NSPersistentStoreResult)
    ['{41114541-B8EE-4111-8217-D0433372D126}']
    procedure cancel; cdecl;
    function managedObjectContext: NSManagedObjectContext; cdecl;
    function operationError: NSError; cdecl;
    function progress: NSProgress; cdecl;
  end;
  TNSPersistentStoreAsynchronousResult = class(TOCGenericImport<NSPersistentStoreAsynchronousResultClass, NSPersistentStoreAsynchronousResult>) end;

  NSAsynchronousFetchResultClass = interface(NSPersistentStoreAsynchronousResultClass)
    ['{DCD4D863-C6F5-4E7D-9F4E-83B949C4797D}']
  end;

  NSAsynchronousFetchResult = interface(NSPersistentStoreAsynchronousResult)
    ['{94978533-2F16-463E-86AE-A300A16A32AB}']
    function fetchRequest: NSAsynchronousFetchRequest; cdecl;
    function finalResult: NSArray; cdecl;
  end;
  TNSAsynchronousFetchResult = class(TOCGenericImport<NSAsynchronousFetchResultClass, NSAsynchronousFetchResult>) end;

  NSBatchInsertResultClass = interface(NSPersistentStoreResultClass)
    ['{898D92D6-5D0A-4795-A9B2-46F6426E67E8}']
  end;

  NSBatchInsertResult = interface(NSPersistentStoreResult)
    ['{03008D8F-A0F0-41E2-BA18-C3BA5CF92AFD}']
    function result: Pointer; cdecl;
    function resultType: NSBatchInsertRequestResultType; cdecl;
  end;
  TNSBatchInsertResult = class(TOCGenericImport<NSBatchInsertResultClass, NSBatchInsertResult>) end;

  NSBatchUpdateResultClass = interface(NSPersistentStoreResultClass)
    ['{9017D18B-76A3-4F0B-9912-4765AFB4099C}']
  end;

  NSBatchUpdateResult = interface(NSPersistentStoreResult)
    ['{1994F828-5D11-469D-AC14-A35F180144A8}']
    function result: Pointer; cdecl;
    function resultType: NSBatchUpdateRequestResultType; cdecl;
  end;
  TNSBatchUpdateResult = class(TOCGenericImport<NSBatchUpdateResultClass, NSBatchUpdateResult>) end;

  NSBatchDeleteResultClass = interface(NSPersistentStoreResultClass)
    ['{4F0FD839-A84A-4F2F-B3C5-4A4794C0ECB6}']
  end;

  NSBatchDeleteResult = interface(NSPersistentStoreResult)
    ['{63A9651C-82EC-4635-A074-CCA1C1BC7239}']
    function result: Pointer; cdecl;
    function resultType: NSBatchDeleteRequestResultType; cdecl;
  end;
  TNSBatchDeleteResult = class(TOCGenericImport<NSBatchDeleteResultClass, NSBatchDeleteResult>) end;

  NSPersistentHistoryResultClass = interface(NSPersistentStoreResultClass)
    ['{827BDEEC-01E4-4EB8-BBF0-415AEBACF10F}']
  end;

  NSPersistentHistoryResult = interface(NSPersistentStoreResult)
    ['{D7BA9C4E-D411-40D1-8449-9FF37787DEF6}']
    function result: Pointer; cdecl;
    function resultType: NSPersistentHistoryResultType; cdecl;
  end;
  TNSPersistentHistoryResult = class(TOCGenericImport<NSPersistentHistoryResultClass, NSPersistentHistoryResult>) end;

  NSPersistentCloudKitContainerEventResultClass = interface(NSPersistentStoreResultClass)
    ['{140C1734-EC25-46EF-A2BF-64257933533B}']
    {class} function new: Pointer; cdecl;
  end;

  NSPersistentCloudKitContainerEventResult = interface(NSPersistentStoreResult)
    ['{FEB3A162-23F4-470A-8031-BDAC1F30A8A7}']
    function result: Pointer; cdecl;
    function resultType: NSPersistentCloudKitContainerEventResultType; cdecl;
  end;
  TNSPersistentCloudKitContainerEventResult = class(TOCGenericImport<NSPersistentCloudKitContainerEventResultClass, NSPersistentCloudKitContainerEventResult>) end;

  NSSaveChangesRequestClass = interface(NSPersistentStoreRequestClass)
    ['{0834F193-F9ED-4496-A364-F7A968AA9C11}']
  end;

  NSSaveChangesRequest = interface(NSPersistentStoreRequest)
    ['{90C5EBCF-500D-481B-8E89-70843A4ED34D}']
    function deletedObjects: NSSet; cdecl;
    function initWithInsertedObjects(insertedObjects: NSSet; updatedObjects: NSSet; deletedObjects: NSSet; lockedObjects: NSSet): Pointer; cdecl;
    function insertedObjects: NSSet; cdecl;
    function lockedObjects: NSSet; cdecl;
    function updatedObjects: NSSet; cdecl;
  end;
  TNSSaveChangesRequest = class(TOCGenericImport<NSSaveChangesRequestClass, NSSaveChangesRequest>) end;

  NSBatchUpdateRequestClass = interface(NSPersistentStoreRequestClass)
    ['{824A19DD-4DE9-4044-89F4-2545142B867E}']
    {class} function batchUpdateRequestWithEntityName(entityName: NSString): Pointer; cdecl;
  end;

  NSBatchUpdateRequest = interface(NSPersistentStoreRequest)
    ['{B2C1A500-A0B9-4539-BAFC-24DEF541E3B2}']
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
    ['{5F722F14-AE56-4A70-85F9-0232D4A5A5F1}']
  end;

  NSBatchDeleteRequest = interface(NSPersistentStoreRequest)
    ['{0B3259F2-E854-4958-856C-8B3D92919658}']
    function fetchRequest: NSFetchRequest; cdecl;
    function initWithFetchRequest(fetch: NSFetchRequest): Pointer; cdecl;
    function initWithObjectIDs(objects: NSArray): Pointer; cdecl;
    function resultType: NSBatchDeleteRequestResultType; cdecl;
    procedure setResultType(resultType: NSBatchDeleteRequestResultType); cdecl;
  end;
  TNSBatchDeleteRequest = class(TOCGenericImport<NSBatchDeleteRequestClass, NSBatchDeleteRequest>) end;

  NSBatchInsertRequestClass = interface(NSPersistentStoreRequestClass)
    ['{3BF8F01D-C9F7-4521-81C4-898ECC63666E}']
    {class} function batchInsertRequestWithEntityName(entityName: NSString;
      managedObjectHandler: TNSBatchInsertRequestBlockMethod2): Pointer; overload; cdecl;
    {class} function batchInsertRequestWithEntityName(entityName: NSString;
      dictionaryHandler: TNSBatchInsertRequestBlockMethod1): Pointer; overload; cdecl;
    {class} function batchInsertRequestWithEntityName(entityName: NSString; objects: NSArray): Pointer; overload; cdecl;
  end;

  NSBatchInsertRequest = interface(NSPersistentStoreRequest)
    ['{C7CCEF63-9E48-4189-BE2E-379E8BAAC387}']
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
    ['{A1C19B46-CFD4-48AD-86B4-1AB33AF57ED8}']
  end;

  NSMergeConflict = interface(NSObject)
    ['{162DB5B7-9CE9-4348-8CEB-EE89EFBA2458}']
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
    ['{760E9546-AD91-4368-9D6F-9009B261F84F}']
  end;

  NSConstraintConflict = interface(NSObject)
    ['{80B66FE3-B2F1-48B3-B9E3-B2C5F4D054F7}']
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
    ['{D7D5C864-AC28-4326-9CBD-0FD8801D2855}']
    {class} function errorMergePolicy: NSMergePolicy; cdecl;
    {class} function mergeByPropertyObjectTrumpMergePolicy: NSMergePolicy; cdecl;
    {class} function mergeByPropertyStoreTrumpMergePolicy: NSMergePolicy; cdecl;
    {class} function overwriteMergePolicy: NSMergePolicy; cdecl;
    {class} function rollbackMergePolicy: NSMergePolicy; cdecl;
  end;

  NSMergePolicy = interface(NSObject)
    ['{41E00831-2AD8-4D86-8301-5E7A0ED869F8}']
    function initWithMergeType(ty: NSMergePolicyType): Pointer; cdecl;
    function mergeType: NSMergePolicyType; cdecl;
    function resolveConflicts(list: NSArray; error: PPointer): Boolean; cdecl;
    function resolveConstraintConflicts(list: NSArray; error: PPointer): Boolean; cdecl;
    function resolveOptimisticLockingVersionConflicts(list: NSArray; error: PPointer): Boolean; cdecl;
  end;
  TNSMergePolicy = class(TOCGenericImport<NSMergePolicyClass, NSMergePolicy>) end;

  NSFetchedResultsControllerClass = interface(NSObjectClass)
    ['{28BAD719-0B7E-4ED6-A9D1-787A8D0FBFE6}']
    {class} procedure deleteCacheWithName(name: NSString); cdecl;
  end;

  NSFetchedResultsController = interface(NSObject)
    ['{1978006F-36CA-48F0-BFDB-86575F7BFE35}']
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
    ['{3490511D-AC84-45E5-8165-6A32D0178A00}']
    function indexTitle: NSString; cdecl;
    function name: NSString; cdecl;
    function numberOfObjects: NSUInteger; cdecl;
    function objects: NSArray; cdecl;
  end;

  NSFetchedResultsControllerDelegate = interface(IObjectiveC)
    ['{3AF06929-9A36-420F-AC1D-526456CA1702}']
    function controller(controller: NSFetchedResultsController; sectionIndexTitleForSectionName: NSString): NSString; overload; cdecl;
    procedure controller(controller: NSFetchedResultsController; didChangeSection: Pointer; atIndex: NSUInteger;
      forChangeType: NSFetchedResultsChangeType); overload; cdecl;
    procedure controller(controller: NSFetchedResultsController; didChangeObject: Pointer; atIndexPath: NSIndexPath;
      forChangeType: NSFetchedResultsChangeType; newIndexPath: NSIndexPath); overload; cdecl;
    procedure controller(controller: NSFetchedResultsController; didChangeContentWithSnapshot: NSDiffableDataSourceSnapshot); overload; cdecl;
    procedure controller(controller: NSFetchedResultsController; didChangeContentWithDifference: NSOrderedCollectionDifference); overload; cdecl;
    procedure controllerDidChangeContent(controller: NSFetchedResultsController); cdecl;
    procedure controllerWillChangeContent(controller: NSFetchedResultsController); cdecl;
  end;

  NSQueryGenerationTokenClass = interface(NSObjectClass)
    ['{94CDA406-F5DB-459E-88DD-3CBA0D6327D4}']
    {class} function currentQueryGenerationToken: NSQueryGenerationToken; cdecl;
  end;

  NSQueryGenerationToken = interface(NSObject)
    ['{0D9B9433-9AFF-424E-A6D4-E9EA7064F549}']
  end;
  TNSQueryGenerationToken = class(TOCGenericImport<NSQueryGenerationTokenClass, NSQueryGenerationToken>) end;

  NSPersistentStoreDescriptionClass = interface(NSObjectClass)
    ['{8D113042-5EF6-4ADF-B36C-A0D08BFFADBA}']
    {class} function persistentStoreDescriptionWithURL(URL: NSURL): Pointer; cdecl;
  end;

  NSPersistentStoreDescription = interface(NSObject)
    ['{5211B9B2-7D63-4317-B970-D0DEF5DB6E79}']
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
    ['{C94BB9D6-B04D-41DB-937A-6B478E074CCD}']
    {class} function defaultDirectoryURL: NSURL; cdecl;
    {class} function persistentContainerWithName(name: NSString; managedObjectModel: NSManagedObjectModel): Pointer; overload; cdecl;
    {class} function persistentContainerWithName(name: NSString): Pointer; overload; cdecl;
  end;

  NSPersistentContainer = interface(NSObject)
    ['{F65AB170-7510-4E89-92F6-98B55989EF44}']
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
    ['{0D409D7C-7AA2-41A5-A966-05C8E33DDB77}']
    {class} function entityDescription: NSEntityDescription; cdecl;
    {class} function entityDescriptionWithContext(context: NSManagedObjectContext): NSEntityDescription; cdecl;
    {class} function fetchRequest: NSFetchRequest; cdecl;
  end;

  NSPersistentHistoryChange = interface(NSObject)
    ['{0A909507-1F92-4C93-BFC4-239798F6663F}']
    function changedObjectID: NSManagedObjectID; cdecl;
    function changeID: Int64; cdecl;
    function changeType: NSPersistentHistoryChangeType; cdecl;
    function tombstone: NSDictionary; cdecl;
    function transaction: NSPersistentHistoryTransaction; cdecl;
    function updatedProperties: NSSet; cdecl;
  end;
  TNSPersistentHistoryChange = class(TOCGenericImport<NSPersistentHistoryChangeClass, NSPersistentHistoryChange>) end;

  NSPersistentHistoryChangeRequestClass = interface(NSPersistentStoreRequestClass)
    ['{573C046B-17EB-40AE-B10A-C887B9BE37D2}']
    {class} function deleteHistoryBeforeDate(date: NSDate): Pointer; cdecl;
    {class} function deleteHistoryBeforeToken(token: NSPersistentHistoryToken): Pointer; cdecl;
    {class} function deleteHistoryBeforeTransaction(transaction: NSPersistentHistoryTransaction): Pointer; cdecl;
    {class} function fetchHistoryAfterDate(date: NSDate): Pointer; cdecl;
    {class} function fetchHistoryAfterToken(token: NSPersistentHistoryToken): Pointer; cdecl;
    {class} function fetchHistoryAfterTransaction(transaction: NSPersistentHistoryTransaction): Pointer; cdecl;
    {class} function fetchHistoryWithFetchRequest(fetchRequest: NSFetchRequest): Pointer; cdecl;
  end;

  NSPersistentHistoryChangeRequest = interface(NSPersistentStoreRequest)
    ['{EDF1269A-5DF2-46E6-BCD9-26892DBFB241}']
    function fetchRequest: NSFetchRequest; cdecl;
    function resultType: NSPersistentHistoryResultType; cdecl;
    procedure setFetchRequest(fetchRequest: NSFetchRequest); cdecl;
    procedure setResultType(resultType: NSPersistentHistoryResultType); cdecl;
    function token: NSPersistentHistoryToken; cdecl;
  end;
  TNSPersistentHistoryChangeRequest = class(TOCGenericImport<NSPersistentHistoryChangeRequestClass, NSPersistentHistoryChangeRequest>) end;

  NSPersistentHistoryTokenClass = interface(NSObjectClass)
    ['{982020EE-7BA7-4B74-9D39-68F95A4A4E20}']
  end;

  NSPersistentHistoryToken = interface(NSObject)
    ['{2B8C793D-9086-41C9-9788-1C0018AD1496}']
  end;
  TNSPersistentHistoryToken = class(TOCGenericImport<NSPersistentHistoryTokenClass, NSPersistentHistoryToken>) end;

  NSPersistentHistoryTransactionClass = interface(NSObjectClass)
    ['{2CE26A36-9B7F-407A-8A28-EBDEEB1FF0C5}']
    {class} function entityDescription: NSEntityDescription; cdecl;
    {class} function entityDescriptionWithContext(context: NSManagedObjectContext): NSEntityDescription; cdecl;
    {class} function fetchRequest: NSFetchRequest; cdecl;
  end;

  NSPersistentHistoryTransaction = interface(NSObject)
    ['{988FA729-4AD3-408E-BEDE-3E06851B3976}']
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
    ['{E9D3FFB8-94A8-4C55-AEDD-24A20D3292F5}']
  end;

  NSPersistentCloudKitContainer = interface(NSPersistentContainer)
    ['{74C18AEE-2278-44E5-8B41-C7C718B13D8A}']
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
    ['{60A2E749-C5BA-4373-AEBE-452B00108EC9}']
  end;

  NSPersistentCloudKitContainerOptions = interface(NSObject)
    ['{4333A96F-C86C-4CE1-A217-986CF70A1ACF}']
    function containerIdentifier: NSString; cdecl;
    function databaseScope: CKDatabaseScope; cdecl;
    function initWithContainerIdentifier(containerIdentifier: NSString): Pointer; cdecl;
    procedure setDatabaseScope(databaseScope: CKDatabaseScope); cdecl;
  end;
  TNSPersistentCloudKitContainerOptions = class(TOCGenericImport<NSPersistentCloudKitContainerOptionsClass, NSPersistentCloudKitContainerOptions>) end;

  NSPersistentCloudKitContainerEventClass = interface(NSObjectClass)
    ['{8C99E4BA-F806-434B-A556-3F02474CAFCE}']
    {class} function new: Pointer; cdecl;
  end;

  NSPersistentCloudKitContainerEvent = interface(NSObject)
    ['{897C8554-DDB2-4344-9243-59E22771C774}']
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
    ['{FAFB28B7-C2EF-4C22-AE4B-CEB87AFF3364}']
    {class} function fetchEventsAfterDate(date: NSDate): Pointer; cdecl;
    {class} function fetchEventsAfterEvent(event: NSPersistentCloudKitContainerEvent): Pointer; cdecl;
    {class} function fetchEventsMatchingFetchRequest(fetchRequest: NSFetchRequest): Pointer; cdecl;
    {class} function fetchRequestForEvents: NSFetchRequest; cdecl;
  end;

  NSPersistentCloudKitContainerEventRequest = interface(NSPersistentStoreRequest)
    ['{AF9545A5-7669-441B-9864-D1C2D4263BC0}']
    function resultType: NSPersistentCloudKitContainerEventResultType; cdecl;
    procedure setResultType(resultType: NSPersistentCloudKitContainerEventResultType); cdecl;
  end;
  TNSPersistentCloudKitContainerEventRequest = class(TOCGenericImport<NSPersistentCloudKitContainerEventRequestClass, NSPersistentCloudKitContainerEventRequest>) end;

  NSStagedMigrationManagerClass = interface(NSObjectClass)
    ['{471261E3-30D0-45F0-93D5-B0CA96885219}']
  end;

  NSStagedMigrationManager = interface(NSObject)
    ['{048CF696-E8F0-40FB-A534-84A5EC039844}']
    function container: NSPersistentContainer; cdecl;
    function initWithMigrationStages(stages: NSArray): Pointer; cdecl;
    function stages: NSArray; cdecl;
  end;
  TNSStagedMigrationManager = class(TOCGenericImport<NSStagedMigrationManagerClass, NSStagedMigrationManager>) end;

  NSMigrationStageClass = interface(NSObjectClass)
    ['{70DB36B5-1412-429C-8FD9-7C66FF696D0A}']
  end;

  NSMigrationStage = interface(NSObject)
    ['{88B841B7-58B9-45A7-A50C-416A64B72D45}']
    function &label: NSString; cdecl;
    procedure setLabel(&label: NSString); cdecl;
  end;
  TNSMigrationStage = class(TOCGenericImport<NSMigrationStageClass, NSMigrationStage>) end;

  NSCustomMigrationStageClass = interface(NSMigrationStageClass)
    ['{E1412BE5-C3FC-4CEC-955A-A6629B8C2981}']
  end;

  NSCustomMigrationStage = interface(NSMigrationStage)
    ['{0F78C080-A838-4260-AD27-F20C9A7D8AA8}']
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
    ['{D69F61BD-EBE8-4103-B718-4B040CC2CD84}']
  end;

  NSLightweightMigrationStage = interface(NSMigrationStage)
    ['{02428D66-EB66-4FDC-8381-05B7D6F201E3}']
    function initWithVersionChecksums(versionChecksums: NSArray): Pointer; cdecl;
    function versionChecksums: NSArray; cdecl;
  end;
  TNSLightweightMigrationStage = class(TOCGenericImport<NSLightweightMigrationStageClass, NSLightweightMigrationStage>) end;

  NSManagedObjectModelReferenceClass = interface(NSObjectClass)
    ['{E4525559-8B57-4BBE-BE0B-E36BA1F759F3}']
  end;

  NSManagedObjectModelReference = interface(NSObject)
    ['{EEFFA548-3DFC-4DC5-AC1B-6DE6BDA47702}']
    function initWithEntityVersionHashes(versionHash: NSDictionary; inBundle: NSBundle; versionChecksum: NSString): Pointer; cdecl;
    function initWithFileURL(fileURL: NSURL; versionChecksum: NSString): Pointer; cdecl;
    function initWithModel(model: NSManagedObjectModel; versionChecksum: NSString): Pointer; cdecl;
    function initWithName(modelName: NSString; inBundle: NSBundle; versionChecksum: NSString): Pointer; cdecl;
    function resolvedModel: NSManagedObjectModel; cdecl;
    function versionChecksum: NSString; cdecl;
  end;
  TNSManagedObjectModelReference = class(TOCGenericImport<NSManagedObjectModelReferenceClass, NSManagedObjectModelReference>) end;

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
function NSMigrationManagerKey: NSString;
function NSMigrationSourceObjectKey: NSString;
function NSMigrationDestinationObjectKey: NSString;
function NSMigrationEntityMappingKey: NSString;
function NSMigrationPropertyMappingKey: NSString;
function NSMigrationEntityPolicyKey: NSString;
function NSPersistentCloudKitContainerEventChangedNotification: NSNotificationName;
function NSPersistentCloudKitContainerEventUserInfoKey: NSString;

const
  libCoreData = '/System/Library/Frameworks/CoreData.framework/CoreData';

implementation

uses
  Posix.Dlfcn;

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

function NSPersistentCloudKitContainerEventChangedNotification: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentCloudKitContainerEventChangedNotification');
end;

function NSPersistentCloudKitContainerEventUserInfoKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreData, 'NSPersistentCloudKitContainerEventUserInfoKey');
end;

initialization
  CoreDataModule := dlopen(MarshaledAString(libCoreData), RTLD_LAZY);

finalization
  dlclose(CoreDataModule);

end.