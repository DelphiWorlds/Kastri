unit DW.iOSapi.Contacts;

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
  Macapi.ObjectiveC, iOSapi.CocoaTypes,
  // iOS 
  iOSapi.Foundation;

const
  CNContactTypePerson = 0;
  CNContactTypeOrganization = 1;
  CNContactSortOrderNone = 0;
  CNContactSortOrderUserDefault = 1;
  CNContactSortOrderGivenName = 2;
  CNContactSortOrderFamilyName = 3;
  CNContactFormatterStyleFullName = 0;
  CNContactFormatterStylePhoneticFullName = 1;
  CNContactDisplayNameOrderUserDefault = 0;
  CNContactDisplayNameOrderGivenNameFirst = 1;
  CNContactDisplayNameOrderFamilyNameFirst = 2;
  CNEntityTypeContacts = 0;
  CNAuthorizationStatusNotDetermined = 0;
  CNAuthorizationStatusRestricted = 1;
  CNAuthorizationStatusDenied = 2;
  CNAuthorizationStatusAuthorized = 3;
  CNContainerTypeUnassigned = 0;
  CNContainerTypeLocal = 1;
  CNContainerTypeExchange = 2;
  CNContainerTypeCardDAV = 3;
  CNErrorCodeCommunicationError = 1;
  CNErrorCodeDataAccessError = 2;
  CNErrorCodeAuthorizationDenied = 100;
  CNErrorCodeNoAccessableWritableContainers = 101;
  CNErrorCodeUnauthorizedKeys = 102;
  CNErrorCodeFeatureDisabledByUser = 103;
  CNErrorCodeRecordDoesNotExist = 200;
  CNErrorCodeInsertedRecordAlreadyExists = 201;
  CNErrorCodeContainmentCycle = 202;
  CNErrorCodeContainmentScope = 203;
  CNErrorCodeParentRecordDoesNotExist = 204;
  CNErrorCodeRecordIdentifierInvalid = 205;
  CNErrorCodeRecordNotWritable = 206;
  CNErrorCodeParentContainerNotWritable = 207;
  CNErrorCodeValidationMultipleErrors = 300;
  CNErrorCodeValidationTypeMismatch = 301;
  CNErrorCodeValidationConfigurationError = 302;
  CNErrorCodePredicateInvalid = 400;
  CNErrorCodePolicyViolation = 500;
  CNErrorCodeClientIdentifierInvalid = 600;
  CNErrorCodeClientIdentifierDoesNotExist = 601;
  CNErrorCodeClientIdentifierCollision = 602;
  CNErrorCodeChangeHistoryExpired = 603;
  CNErrorCodeChangeHistoryInvalidAnchor = 604;
  CNErrorCodeVCardMalformed = 700;
  CNErrorCodeVCardSummarizationError = 701;
  CNPostalAddressFormatterStyleMailingAddress = 0;

type
  CNChangeHistoryEvent = interface;
  CNChangeHistoryDropEverythingEvent = interface;
  CNChangeHistoryAddContactEvent = interface;
  CNChangeHistoryUpdateContactEvent = interface;
  CNChangeHistoryDeleteContactEvent = interface;
  CNChangeHistoryAddGroupEvent = interface;
  CNChangeHistoryUpdateGroupEvent = interface;
  CNChangeHistoryDeleteGroupEvent = interface;
  CNChangeHistoryAddMemberToGroupEvent = interface;
  CNChangeHistoryRemoveMemberFromGroupEvent = interface;
  CNChangeHistoryAddSubgroupToGroupEvent = interface;
  CNChangeHistoryRemoveSubgroupFromGroupEvent = interface;
  CNChangeHistoryEventVisitor = interface;
  CNFetchRequest = interface;
  CNChangeHistoryFetchRequest = interface;
  CNLabeledValue = interface;
  CNPhoneNumber = interface;
  CNPostalAddress = interface;
  CNContactRelation = interface;
  CNSocialProfile = interface;
  CNInstantMessageAddress = interface;
  CNKeyDescriptor = interface;
  CNContact = interface;
  CNContactFetchRequest = interface;
  CNContactFormatter = interface;
  CNContactProperty = interface;
  CNContactStore = interface;
  CNContactsUserDefaults = interface;
  CNContactVCardSerialization = interface;
  CNContainer = interface;
  CNFetchResult = interface;
  CNGroup = interface;
  CNMutableContact = interface;
  CNMutableGroup = interface;
  CNMutablePostalAddress = interface;
  CNPostalAddressFormatter = interface;
  CNSaveRequest = interface;

  PBoolean = ^Boolean;
  CNContactType = NSInteger;
  CNContactSortOrder = NSInteger;
  CNContactFormatterStyle = NSInteger;
  CNContactDisplayNameOrder = NSInteger;
  CNEntityType = NSInteger;
  CNAuthorizationStatus = NSInteger;
  CNContainerType = NSInteger;
  CNErrorCode = NSInteger;
  CNPostalAddressFormatterStyle = NSInteger;
  TCNContactStoreBlockMethod1 = procedure(granted: Boolean; error: NSError) of object;
  TCNContactStoreBlockMethod2 = procedure(contact: CNContact; stop: PBoolean) of object;

  CNChangeHistoryEventClass = interface(NSObjectClass)
    ['{0F9B7CE5-A072-44C5-ABC9-D29835A42B73}']
  end;

  CNChangeHistoryEvent = interface(NSObject)
    ['{F4BFBC70-A2E7-4380-8ABE-F62751635298}']
    procedure acceptEventVisitor(visitor: Pointer); cdecl;
  end;
  TCNChangeHistoryEvent = class(TOCGenericImport<CNChangeHistoryEventClass, CNChangeHistoryEvent>) end;

  CNChangeHistoryDropEverythingEventClass = interface(CNChangeHistoryEventClass)
    ['{F139624F-D17B-4F89-9108-8B3E2F6F2820}']
  end;

  CNChangeHistoryDropEverythingEvent = interface(CNChangeHistoryEvent)
    ['{18E7A140-2BD2-4E09-BE14-B46E0CC9EA5C}']
  end;
  TCNChangeHistoryDropEverythingEvent = class(TOCGenericImport<CNChangeHistoryDropEverythingEventClass, CNChangeHistoryDropEverythingEvent>) end;

  CNChangeHistoryAddContactEventClass = interface(CNChangeHistoryEventClass)
    ['{CBCBE42B-4358-46AD-AEF3-933D86559E2D}']
  end;

  CNChangeHistoryAddContactEvent = interface(CNChangeHistoryEvent)
    ['{0DB83384-8CCD-461A-BD66-8A04DD131D41}']
    function contact: CNContact; cdecl;
    function containerIdentifier: NSString; cdecl;
  end;
  TCNChangeHistoryAddContactEvent = class(TOCGenericImport<CNChangeHistoryAddContactEventClass, CNChangeHistoryAddContactEvent>) end;

  CNChangeHistoryUpdateContactEventClass = interface(CNChangeHistoryEventClass)
    ['{DE5DB8E0-3D81-4CFE-9C97-F33BA945FF71}']
  end;

  CNChangeHistoryUpdateContactEvent = interface(CNChangeHistoryEvent)
    ['{26D77938-9841-458C-8B2F-CDF014231A7F}']
    function contact: CNContact; cdecl;
  end;
  TCNChangeHistoryUpdateContactEvent = class(TOCGenericImport<CNChangeHistoryUpdateContactEventClass, CNChangeHistoryUpdateContactEvent>) end;

  CNChangeHistoryDeleteContactEventClass = interface(CNChangeHistoryEventClass)
    ['{8C178A95-DE35-4C8E-9DF5-0025123384B7}']
  end;

  CNChangeHistoryDeleteContactEvent = interface(CNChangeHistoryEvent)
    ['{673F207E-1BEB-42A1-856E-FAF476260775}']
    function contactIdentifier: NSString; cdecl;
  end;
  TCNChangeHistoryDeleteContactEvent = class(TOCGenericImport<CNChangeHistoryDeleteContactEventClass, CNChangeHistoryDeleteContactEvent>) end;

  CNChangeHistoryAddGroupEventClass = interface(CNChangeHistoryEventClass)
    ['{18AA5447-FF38-41C3-8D4E-A2FB73F95D5D}']
  end;

  CNChangeHistoryAddGroupEvent = interface(CNChangeHistoryEvent)
    ['{28857769-F6FE-4006-A773-6CA448DBF841}']
    function containerIdentifier: NSString; cdecl;
    function group: CNGroup; cdecl;
  end;
  TCNChangeHistoryAddGroupEvent = class(TOCGenericImport<CNChangeHistoryAddGroupEventClass, CNChangeHistoryAddGroupEvent>) end;

  CNChangeHistoryUpdateGroupEventClass = interface(CNChangeHistoryEventClass)
    ['{59633DCA-8DD8-4162-B2A9-40B82FD17F1B}']
  end;

  CNChangeHistoryUpdateGroupEvent = interface(CNChangeHistoryEvent)
    ['{B0B06974-DA66-4FAC-9BD8-EEF8EB9833BD}']
    function group: CNGroup; cdecl;
  end;
  TCNChangeHistoryUpdateGroupEvent = class(TOCGenericImport<CNChangeHistoryUpdateGroupEventClass, CNChangeHistoryUpdateGroupEvent>) end;

  CNChangeHistoryDeleteGroupEventClass = interface(CNChangeHistoryEventClass)
    ['{CFCF8C5B-CEEB-4876-B5CF-B531F5FA16F7}']
  end;

  CNChangeHistoryDeleteGroupEvent = interface(CNChangeHistoryEvent)
    ['{ACE15F5D-98DD-4C8C-8E60-0FF8F97D4DCC}']
    function groupIdentifier: NSString; cdecl;
  end;
  TCNChangeHistoryDeleteGroupEvent = class(TOCGenericImport<CNChangeHistoryDeleteGroupEventClass, CNChangeHistoryDeleteGroupEvent>) end;

  CNChangeHistoryAddMemberToGroupEventClass = interface(CNChangeHistoryEventClass)
    ['{A69C7880-2017-4183-A4DD-C19F2B96C908}']
  end;

  CNChangeHistoryAddMemberToGroupEvent = interface(CNChangeHistoryEvent)
    ['{E4559BB8-F976-4028-9068-CF25C4402521}']
    function group: CNGroup; cdecl;
    function member: CNContact; cdecl;
  end;
  TCNChangeHistoryAddMemberToGroupEvent = class(TOCGenericImport<CNChangeHistoryAddMemberToGroupEventClass, CNChangeHistoryAddMemberToGroupEvent>) end;

  CNChangeHistoryRemoveMemberFromGroupEventClass = interface(CNChangeHistoryEventClass)
    ['{F3079E10-ED94-41BE-8F8E-00B34EB437F1}']
  end;

  CNChangeHistoryRemoveMemberFromGroupEvent = interface(CNChangeHistoryEvent)
    ['{F473E5A4-513B-45B5-AB04-FA58BC554972}']
    function group: CNGroup; cdecl;
    function member: CNContact; cdecl;
  end;
  TCNChangeHistoryRemoveMemberFromGroupEvent = class(TOCGenericImport<CNChangeHistoryRemoveMemberFromGroupEventClass, CNChangeHistoryRemoveMemberFromGroupEvent>) end;

  CNChangeHistoryAddSubgroupToGroupEventClass = interface(CNChangeHistoryEventClass)
    ['{9522AB45-B43A-4B2F-A1F1-4BA664D2C15C}']
  end;

  CNChangeHistoryAddSubgroupToGroupEvent = interface(CNChangeHistoryEvent)
    ['{0B026FDD-3EFB-4488-8EB8-54F0BA1AACDB}']
    function group: CNGroup; cdecl;
    function subgroup: CNGroup; cdecl;
  end;
  TCNChangeHistoryAddSubgroupToGroupEvent = class(TOCGenericImport<CNChangeHistoryAddSubgroupToGroupEventClass, CNChangeHistoryAddSubgroupToGroupEvent>) end;

  CNChangeHistoryRemoveSubgroupFromGroupEventClass = interface(CNChangeHistoryEventClass)
    ['{8F63A7E7-E1FF-48DB-9E6D-2519A05B3C3C}']
  end;

  CNChangeHistoryRemoveSubgroupFromGroupEvent = interface(CNChangeHistoryEvent)
    ['{29699C80-469C-407E-929E-8EA77AEF2AC6}']
    function group: CNGroup; cdecl;
    function subgroup: CNGroup; cdecl;
  end;
  TCNChangeHistoryRemoveSubgroupFromGroupEvent = class(TOCGenericImport<CNChangeHistoryRemoveSubgroupFromGroupEventClass, CNChangeHistoryRemoveSubgroupFromGroupEvent>) end;

  CNChangeHistoryEventVisitor = interface(IObjectiveC)
    ['{04FBE05F-5A83-4B10-B064-EFE153ECBC15}']
    procedure visitAddContactEvent(event: CNChangeHistoryAddContactEvent); cdecl;
    procedure visitAddGroupEvent(event: CNChangeHistoryAddGroupEvent); cdecl;
    procedure visitAddMemberToGroupEvent(event: CNChangeHistoryAddMemberToGroupEvent); cdecl;
    procedure visitAddSubgroupToGroupEvent(event: CNChangeHistoryAddSubgroupToGroupEvent); cdecl;
    procedure visitDeleteContactEvent(event: CNChangeHistoryDeleteContactEvent); cdecl;
    procedure visitDeleteGroupEvent(event: CNChangeHistoryDeleteGroupEvent); cdecl;
    procedure visitDropEverythingEvent(event: CNChangeHistoryDropEverythingEvent); cdecl;
    procedure visitRemoveMemberFromGroupEvent(event: CNChangeHistoryRemoveMemberFromGroupEvent); cdecl;
    procedure visitRemoveSubgroupFromGroupEvent(event: CNChangeHistoryRemoveSubgroupFromGroupEvent); cdecl;
    procedure visitUpdateContactEvent(event: CNChangeHistoryUpdateContactEvent); cdecl;
    procedure visitUpdateGroupEvent(event: CNChangeHistoryUpdateGroupEvent); cdecl;
  end;

  CNFetchRequestClass = interface(NSObjectClass)
    ['{9DFBCFF3-CCAC-4F81-9E62-4087E6509113}']
  end;

  CNFetchRequest = interface(NSObject)
    ['{3C4E97FA-CCBB-4294-A71D-49757FEF8AEA}']
  end;
  TCNFetchRequest = class(TOCGenericImport<CNFetchRequestClass, CNFetchRequest>) end;

  CNChangeHistoryFetchRequestClass = interface(CNFetchRequestClass)
    ['{0EDD0219-2A33-445D-8AAB-DD5B5D496553}']
  end;

  CNChangeHistoryFetchRequest = interface(CNFetchRequest)
    ['{22179860-56F8-4A25-AD60-1D9743879EB5}']
    function additionalContactKeyDescriptors: NSArray; cdecl;
    function excludedTransactionAuthors: NSArray; cdecl;
    function includeGroupChanges: Boolean; cdecl;
    function mutableObjects: Boolean; cdecl;
    procedure setAdditionalContactKeyDescriptors(additionalContactKeyDescriptors: NSArray); cdecl;
    procedure setExcludedTransactionAuthors(excludedTransactionAuthors: NSArray); cdecl;
    procedure setIncludeGroupChanges(includeGroupChanges: Boolean); cdecl;
    procedure setMutableObjects(mutableObjects: Boolean); cdecl;
    procedure setShouldUnifyResults(shouldUnifyResults: Boolean); cdecl;
    procedure setStartingToken(startingToken: NSData); cdecl;
    function shouldUnifyResults: Boolean; cdecl;
    function startingToken: NSData; cdecl;
  end;
  TCNChangeHistoryFetchRequest = class(TOCGenericImport<CNChangeHistoryFetchRequestClass, CNChangeHistoryFetchRequest>) end;

  CNLabeledValueClass = interface(NSObjectClass)
    ['{BDB1C043-532B-4E7D-B7A0-EEBB76E4102A}']
    [MethodName('labeledValueWithLabel:value:')]
    {class} function labeledValueWithLabel(&label: NSString; value: Pointer): Pointer; cdecl;
    {class} function localizedStringForLabel(&label: NSString): NSString; cdecl;
  end;

  CNLabeledValue = interface(NSObject)
    ['{77E38056-B709-4441-8344-A3E23EABB10C}']
    function &label: NSString; cdecl;
    function identifier: NSString; cdecl;
    [MethodName('initWithLabel:value:')]
    function initWithLabel(&label: NSString; value: Pointer): Pointer; cdecl;
    [MethodName('labeledValueBySettingLabel:value:')]
    function labeledValueBySettingLabel(&label: NSString; value: Pointer): Pointer; overload; cdecl;
    function labeledValueBySettingLabel(&label: NSString): Pointer; overload; cdecl;
    function labeledValueBySettingValue(value: Pointer): Pointer; cdecl;
    function value: Pointer; cdecl;
  end;
  TCNLabeledValue = class(TOCGenericImport<CNLabeledValueClass, CNLabeledValue>) end;

  CNPhoneNumberClass = interface(NSObjectClass)
    ['{D38FA3FF-F4EE-4BA9-B21F-0BD0FE8CA9A6}']
    {class} function new: Pointer; cdecl;
    {class} function phoneNumberWithStringValue(stringValue: NSString): Pointer; cdecl;
  end;

  CNPhoneNumber = interface(NSObject)
    ['{1662B76B-E02F-4CAE-869E-C4F6EC720C14}']
    function initWithStringValue(&string: NSString): Pointer; cdecl;
    function stringValue: NSString; cdecl;
  end;
  TCNPhoneNumber = class(TOCGenericImport<CNPhoneNumberClass, CNPhoneNumber>) end;

  CNPostalAddressClass = interface(NSObjectClass)
    ['{2D49B65A-4DEC-4F43-B1A1-CABDD78391B4}']
    {class} function localizedStringForKey(key: NSString): NSString; cdecl;
  end;

  CNPostalAddress = interface(NSObject)
    ['{E73AC9A1-33B1-40FB-A4C8-7D7527FF5CE2}']
    function city: NSString; cdecl;
    function country: NSString; cdecl;
    function ISOCountryCode: NSString; cdecl;
    function postalCode: NSString; cdecl;
    function state: NSString; cdecl;
    function street: NSString; cdecl;
    function subAdministrativeArea: NSString; cdecl;
    function subLocality: NSString; cdecl;
  end;
  TCNPostalAddress = class(TOCGenericImport<CNPostalAddressClass, CNPostalAddress>) end;

  CNContactRelationClass = interface(NSObjectClass)
    ['{0FEFE222-A158-4601-8D39-66BC4631031A}']
    {class} function contactRelationWithName(name: NSString): Pointer; cdecl;
  end;

  CNContactRelation = interface(NSObject)
    ['{0BD98124-0F9F-4E6A-BB7E-BBE988B905AE}']
    function initWithName(name: NSString): Pointer; cdecl;
    function name: NSString; cdecl;
  end;
  TCNContactRelation = class(TOCGenericImport<CNContactRelationClass, CNContactRelation>) end;

  CNSocialProfileClass = interface(NSObjectClass)
    ['{6BE305F8-2432-4AA3-9EDF-5F857F6D7B5D}']
    {class} function localizedStringForKey(key: NSString): NSString; cdecl;
    {class} function localizedStringForService(service: NSString): NSString; cdecl;
  end;

  CNSocialProfile = interface(NSObject)
    ['{DCC4D8F8-8F00-4946-8631-82A3D777FCA7}']
    [MethodName('initWithUrlString:username:userIdentifier:service:')]
    function initWithUrlString(urlString: NSString; username: NSString; userIdentifier: NSString; service: NSString): Pointer; cdecl;
    function service: NSString; cdecl;
    function urlString: NSString; cdecl;
    function userIdentifier: NSString; cdecl;
    function username: NSString; cdecl;
  end;
  TCNSocialProfile = class(TOCGenericImport<CNSocialProfileClass, CNSocialProfile>) end;

  CNInstantMessageAddressClass = interface(NSObjectClass)
    ['{27A64662-E133-4CA4-B929-1E11A085019B}']
    {class} function localizedStringForKey(key: NSString): NSString; cdecl;
    {class} function localizedStringForService(service: NSString): NSString; cdecl;
  end;

  CNInstantMessageAddress = interface(NSObject)
    ['{97187A9E-F4A1-4D75-9148-C2419891E812}']
    [MethodName('initWithUsername:service:')]
    function initWithUsername(username: NSString; service: NSString): Pointer; cdecl;
    function service: NSString; cdecl;
    function username: NSString; cdecl;
  end;
  TCNInstantMessageAddress = class(TOCGenericImport<CNInstantMessageAddressClass, CNInstantMessageAddress>) end;

  CNKeyDescriptor = interface(IObjectiveC)
    ['{310771ED-0D76-4C39-91FB-401F4121AA54}']
  end;

  CNContactClass = interface(NSObjectClass)
    ['{7EA9FB00-C55C-4EE6-8420-EDA37563620D}']
    {class} function comparatorForNameSortOrder(sortOrder: CNContactSortOrder): NSComparator; cdecl;
    {class} function descriptorForAllComparatorKeys: Pointer; cdecl;
    {class} function localizedStringForKey(key: NSString): NSString; cdecl;
    {class} function predicateForContactsInContainerWithIdentifier(containerIdentifier: NSString): NSPredicate; cdecl;
    {class} function predicateForContactsInGroupWithIdentifier(groupIdentifier: NSString): NSPredicate; cdecl;
    {class} function predicateForContactsMatchingEmailAddress(emailAddress: NSString): NSPredicate; cdecl;
    {class} function predicateForContactsMatchingName(name: NSString): NSPredicate; cdecl;
    {class} function predicateForContactsMatchingPhoneNumber(phoneNumber: CNPhoneNumber): NSPredicate; cdecl;
    {class} function predicateForContactsWithIdentifiers(identifiers: NSArray): NSPredicate; cdecl;
  end;

  CNContact = interface(NSObject)
    ['{EFD2923A-B4DB-4A1C-B05B-06EE3F66AF65}']
    function areKeysAvailable(keyDescriptors: NSArray): Boolean; cdecl;
    function birthday: NSDateComponents; cdecl;
    function contactRelations: NSArray; cdecl;
    function contactType: CNContactType; cdecl;
    function dates: NSArray; cdecl;
    function departmentName: NSString; cdecl;
    function emailAddresses: NSArray; cdecl;
    function familyName: NSString; cdecl;
    function givenName: NSString; cdecl;
    function identifier: NSString; cdecl;
    function imageData: NSData; cdecl;
    function imageDataAvailable: Boolean; cdecl;
    function instantMessageAddresses: NSArray; cdecl;
    function isKeyAvailable(key: NSString): Boolean; cdecl;
    function isUnifiedWithContactWithIdentifier(contactIdentifier: NSString): Boolean; cdecl;
    function jobTitle: NSString; cdecl;
    function middleName: NSString; cdecl;
    function namePrefix: NSString; cdecl;
    function nameSuffix: NSString; cdecl;
    function nickname: NSString; cdecl;
    function nonGregorianBirthday: NSDateComponents; cdecl;
    function note: NSString; cdecl;
    function organizationName: NSString; cdecl;
    function phoneNumbers: NSArray; cdecl;
    function phoneticFamilyName: NSString; cdecl;
    function phoneticGivenName: NSString; cdecl;
    function phoneticMiddleName: NSString; cdecl;
    function phoneticOrganizationName: NSString; cdecl;
    function postalAddresses: NSArray; cdecl;
    function previousFamilyName: NSString; cdecl;
    function socialProfiles: NSArray; cdecl;
    function thumbnailImageData: NSData; cdecl;
    function urlAddresses: NSArray; cdecl;
  end;
  TCNContact = class(TOCGenericImport<CNContactClass, CNContact>) end;

  CNContactFetchRequestClass = interface(CNFetchRequestClass)
    ['{6418ADBC-F3EB-4377-9F01-D4F984A68BF6}']
    {class} function new: Pointer; cdecl;
  end;

  CNContactFetchRequest = interface(CNFetchRequest)
    ['{BC59BC10-DBC3-4A8A-B4F2-C54164702066}']
    function initWithKeysToFetch(keysToFetch: NSArray): Pointer; cdecl;
    function keysToFetch: NSArray; cdecl;
    function mutableObjects: Boolean; cdecl;
    function predicate: NSPredicate; cdecl;
    procedure setKeysToFetch(keysToFetch: NSArray); cdecl;
    procedure setMutableObjects(mutableObjects: Boolean); cdecl;
    procedure setPredicate(predicate: NSPredicate); cdecl;
    procedure setSortOrder(sortOrder: CNContactSortOrder); cdecl;
    procedure setUnifyResults(unifyResults: Boolean); cdecl;
    function sortOrder: CNContactSortOrder; cdecl;
    function unifyResults: Boolean; cdecl;
  end;
  TCNContactFetchRequest = class(TOCGenericImport<CNContactFetchRequestClass, CNContactFetchRequest>) end;

  CNContactFormatterClass = interface(NSFormatterClass)
    ['{CEEB870F-AD01-43DA-994B-A473B0EF4A01}']
    [MethodName('attributedStringFromContact:style:defaultAttributes:')]
    {class} function attributedStringFromContact(contact: CNContact; style: CNContactFormatterStyle; attributes: NSDictionary): NSAttributedString; cdecl;
    {class} function delimiterForContact(contact: CNContact): NSString; cdecl;
    {class} function descriptorForRequiredKeysForStyle(style: CNContactFormatterStyle): Pointer; cdecl;
    {class} function nameOrderForContact(contact: CNContact): CNContactDisplayNameOrder; cdecl;
    [MethodName('stringFromContact:style:')]
    {class} function stringFromContact(contact: CNContact; style: CNContactFormatterStyle): NSString; cdecl;
  end;

  CNContactFormatter = interface(NSFormatter)
    ['{9DD38F03-9E85-4F0A-A3C2-E8E8BBBAD857}']
    [MethodName('attributedStringFromContact:defaultAttributes:')]
    function attributedStringFromContact(contact: CNContact; attributes: NSDictionary): NSAttributedString; cdecl;
    procedure setStyle(style: CNContactFormatterStyle); cdecl;
    function stringFromContact(contact: CNContact): NSString; cdecl;
    function style: CNContactFormatterStyle; cdecl;
  end;
  TCNContactFormatter = class(TOCGenericImport<CNContactFormatterClass, CNContactFormatter>) end;

  CNContactPropertyClass = interface(NSObjectClass)
    ['{1AF4D2EB-54FA-4EC1-A081-6AC970E5294E}']
  end;

  CNContactProperty = interface(NSObject)
    ['{D12B1BD3-3F6C-4C98-87DD-5A0C913C5CCC}']
    function &label: NSString; cdecl;
    function contact: CNContact; cdecl;
    function identifier: NSString; cdecl;
    function key: NSString; cdecl;
    function value: Pointer; cdecl;
  end;
  TCNContactProperty = class(TOCGenericImport<CNContactPropertyClass, CNContactProperty>) end;

  CNContactStoreClass = interface(NSObjectClass)
    ['{DDDE7973-47B8-42B2-9949-033DF117048F}']
    {class} function authorizationStatusForEntityType(entityType: CNEntityType): CNAuthorizationStatus; cdecl;
  end;

  CNContactStore = interface(NSObject)
    ['{97B14C5E-4513-4CFE-99F4-0302EE3ACE15}']
    [MethodName('containersMatchingPredicate:error:')]
    function containersMatchingPredicate(predicate: NSPredicate; error: PPointer): NSArray; cdecl;
    function currentHistoryToken: NSData; cdecl;
    function defaultContainerIdentifier: NSString; cdecl;
    [MethodName('enumerateContactsWithFetchRequest:error:usingBlock:')]
    function enumerateContactsWithFetchRequest(fetchRequest: CNContactFetchRequest; error: PPointer; block: TCNContactStoreBlockMethod2): Boolean; cdecl;
    [MethodName('enumeratorForChangeHistoryFetchRequest:error:')]
    function enumeratorForChangeHistoryFetchRequest(request: CNChangeHistoryFetchRequest; error: PPointer): CNFetchResult; cdecl;
    [MethodName('enumeratorForContactFetchRequest:error:')]
    function enumeratorForContactFetchRequest(request: CNContactFetchRequest; error: PPointer): CNFetchResult; cdecl;
    [MethodName('executeSaveRequest:error:')]
    function executeSaveRequest(saveRequest: CNSaveRequest; error: PPointer): Boolean; cdecl;
    [MethodName('groupsMatchingPredicate:error:')]
    function groupsMatchingPredicate(predicate: NSPredicate; error: PPointer): NSArray; cdecl;
    [MethodName('requestAccessForEntityType:completionHandler:')]
    procedure requestAccessForEntityType(entityType: CNEntityType; completionHandler: TCNContactStoreBlockMethod1); cdecl;
    [MethodName('unifiedContactsMatchingPredicate:keysToFetch:error:')]
    function unifiedContactsMatchingPredicate(predicate: NSPredicate; keys: NSArray; error: PPointer): NSArray; cdecl;
    [MethodName('unifiedContactWithIdentifier:keysToFetch:error:')]
    function unifiedContactWithIdentifier(identifier: NSString; keys: NSArray; error: PPointer): CNContact; cdecl;
  end;
  TCNContactStore = class(TOCGenericImport<CNContactStoreClass, CNContactStore>) end;

  CNContactsUserDefaultsClass = interface(NSObjectClass)
    ['{B991AF01-BABF-4222-8927-D8F03813F3E8}']
    {class} function sharedDefaults: Pointer; cdecl;
  end;

  CNContactsUserDefaults = interface(NSObject)
    ['{959A0611-D72C-46C8-8AD4-2E4B131D9864}']
    function countryCode: NSString; cdecl;
    function sortOrder: CNContactSortOrder; cdecl;
  end;
  TCNContactsUserDefaults = class(TOCGenericImport<CNContactsUserDefaultsClass, CNContactsUserDefaults>) end;

  CNContactVCardSerializationClass = interface(NSObjectClass)
    ['{B25918C2-62DB-43C9-B334-4813F3C8DD6E}']
    [MethodName('contactsWithData:error:')]
    {class} function contactsWithData(data: NSData; error: PPointer): NSArray; cdecl;
    [MethodName('dataWithContacts:error:')]
    {class} function dataWithContacts(contacts: NSArray; error: PPointer): NSData; cdecl;
    {class} function descriptorForRequiredKeys: Pointer; cdecl;
  end;

  CNContactVCardSerialization = interface(NSObject)
    ['{59C680DB-67CC-4010-9010-51A13CD5D173}']
  end;
  TCNContactVCardSerialization = class(TOCGenericImport<CNContactVCardSerializationClass, CNContactVCardSerialization>) end;

  CNContainerClass = interface(NSObjectClass)
    ['{03E255EF-0A83-4031-B1E9-B557000C0A0C}']
    {class} function predicateForContainerOfContactWithIdentifier(contactIdentifier: NSString): NSPredicate; cdecl;
    {class} function predicateForContainerOfGroupWithIdentifier(groupIdentifier: NSString): NSPredicate; cdecl;
    {class} function predicateForContainersWithIdentifiers(identifiers: NSArray): NSPredicate; cdecl;
  end;

  CNContainer = interface(NSObject)
    ['{A4062259-E638-43E3-8C34-81BC5C293D93}']
    function &type: CNContainerType; cdecl;
    function identifier: NSString; cdecl;
    function name: NSString; cdecl;
  end;
  TCNContainer = class(TOCGenericImport<CNContainerClass, CNContainer>) end;

  CNFetchResultClass = interface(NSObjectClass)
    ['{4EACD9F3-F832-40D2-AE04-0C8A5C15175A}']
    {class} function new: Pointer; cdecl;
  end;

  CNFetchResult = interface(NSObject)
    ['{DE5504BB-D493-40E8-AA55-C2503B71C009}']
    function currentHistoryToken: NSData; cdecl;
    function value: Pointer; cdecl;
  end;
  TCNFetchResult = class(TOCGenericImport<CNFetchResultClass, CNFetchResult>) end;

  CNGroupClass = interface(NSObjectClass)
    ['{15471CC0-75BB-4D0E-BA67-6C5619B97796}']
    {class} function predicateForGroupsInContainerWithIdentifier(containerIdentifier: NSString): NSPredicate; cdecl;
    {class} function predicateForGroupsWithIdentifiers(identifiers: NSArray): NSPredicate; cdecl;
  end;

  CNGroup = interface(NSObject)
    ['{C49066EF-F1F0-4919-9A89-65B66036A714}']
    function identifier: NSString; cdecl;
    function name: NSString; cdecl;
  end;
  TCNGroup = class(TOCGenericImport<CNGroupClass, CNGroup>) end;

  CNMutableContactClass = interface(CNContactClass)
    ['{02635457-6F63-4637-AC10-09A686A18EDF}']
  end;

  CNMutableContact = interface(CNContact)
    ['{B1E21ACA-DC55-4316-9608-2E2FB09535D7}']
    function birthday: NSDateComponents; cdecl;
    function contactRelations: NSArray; cdecl;
    function contactType: CNContactType; cdecl;
    function dates: NSArray; cdecl;
    function departmentName: NSString; cdecl;
    function emailAddresses: NSArray; cdecl;
    function familyName: NSString; cdecl;
    function givenName: NSString; cdecl;
    function imageData: NSData; cdecl;
    function instantMessageAddresses: NSArray; cdecl;
    function jobTitle: NSString; cdecl;
    function middleName: NSString; cdecl;
    function namePrefix: NSString; cdecl;
    function nameSuffix: NSString; cdecl;
    function nickname: NSString; cdecl;
    function nonGregorianBirthday: NSDateComponents; cdecl;
    function note: NSString; cdecl;
    function organizationName: NSString; cdecl;
    function phoneNumbers: NSArray; cdecl;
    function phoneticFamilyName: NSString; cdecl;
    function phoneticGivenName: NSString; cdecl;
    function phoneticMiddleName: NSString; cdecl;
    function phoneticOrganizationName: NSString; cdecl;
    function postalAddresses: NSArray; cdecl;
    function previousFamilyName: NSString; cdecl;
    procedure setBirthday(birthday: NSDateComponents); cdecl;
    procedure setContactRelations(contactRelations: NSArray); cdecl;
    procedure setContactType(contactType: CNContactType); cdecl;
    procedure setDates(dates: NSArray); cdecl;
    procedure setDepartmentName(departmentName: NSString); cdecl;
    procedure setEmailAddresses(emailAddresses: NSArray); cdecl;
    procedure setFamilyName(familyName: NSString); cdecl;
    procedure setGivenName(givenName: NSString); cdecl;
    procedure setImageData(imageData: NSData); cdecl;
    procedure setInstantMessageAddresses(instantMessageAddresses: NSArray); cdecl;
    procedure setJobTitle(jobTitle: NSString); cdecl;
    procedure setMiddleName(middleName: NSString); cdecl;
    procedure setNamePrefix(namePrefix: NSString); cdecl;
    procedure setNameSuffix(nameSuffix: NSString); cdecl;
    procedure setNickname(nickname: NSString); cdecl;
    procedure setNonGregorianBirthday(nonGregorianBirthday: NSDateComponents); cdecl;
    procedure setNote(note: NSString); cdecl;
    procedure setOrganizationName(organizationName: NSString); cdecl;
    procedure setPhoneNumbers(phoneNumbers: NSArray); cdecl;
    procedure setPhoneticFamilyName(phoneticFamilyName: NSString); cdecl;
    procedure setPhoneticGivenName(phoneticGivenName: NSString); cdecl;
    procedure setPhoneticMiddleName(phoneticMiddleName: NSString); cdecl;
    procedure setPhoneticOrganizationName(phoneticOrganizationName: NSString); cdecl;
    procedure setPostalAddresses(postalAddresses: NSArray); cdecl;
    procedure setPreviousFamilyName(previousFamilyName: NSString); cdecl;
    procedure setSocialProfiles(socialProfiles: NSArray); cdecl;
    procedure setUrlAddresses(urlAddresses: NSArray); cdecl;
    function socialProfiles: NSArray; cdecl;
    function urlAddresses: NSArray; cdecl;
  end;
  TCNMutableContact = class(TOCGenericImport<CNMutableContactClass, CNMutableContact>) end;

  CNMutableGroupClass = interface(CNGroupClass)
    ['{A07F5B7B-1FEA-460A-8EEA-125FD2215E28}']
  end;

  CNMutableGroup = interface(CNGroup)
    ['{D0C92062-4A69-4A10-8B28-FF78F0611C99}']
    function name: NSString; cdecl;
    procedure setName(name: NSString); cdecl;
  end;
  TCNMutableGroup = class(TOCGenericImport<CNMutableGroupClass, CNMutableGroup>) end;

  CNMutablePostalAddressClass = interface(CNPostalAddressClass)
    ['{D2899BF8-0229-46E8-8B8C-669B53B22E23}']
  end;

  CNMutablePostalAddress = interface(CNPostalAddress)
    ['{D2F2CFB7-98FE-493A-946B-0BC9EBC0F3A6}']
    function city: NSString; cdecl;
    function country: NSString; cdecl;
    function ISOCountryCode: NSString; cdecl;
    function postalCode: NSString; cdecl;
    procedure setCity(city: NSString); cdecl;
    procedure setCountry(country: NSString); cdecl;
    procedure setISOCountryCode(ISOCountryCode: NSString); cdecl;
    procedure setPostalCode(postalCode: NSString); cdecl;
    procedure setState(state: NSString); cdecl;
    procedure setStreet(street: NSString); cdecl;
    procedure setSubAdministrativeArea(subAdministrativeArea: NSString); cdecl;
    procedure setSubLocality(subLocality: NSString); cdecl;
    function state: NSString; cdecl;
    function street: NSString; cdecl;
    function subAdministrativeArea: NSString; cdecl;
    function subLocality: NSString; cdecl;
  end;
  TCNMutablePostalAddress = class(TOCGenericImport<CNMutablePostalAddressClass, CNMutablePostalAddress>) end;

  CNPostalAddressFormatterClass = interface(NSFormatterClass)
    ['{6DAADDBB-B1EA-4E0E-8B79-0C769DC5FD82}']
    [MethodName('attributedStringFromPostalAddress:style:withDefaultAttributes:')]
    {class} function attributedStringFromPostalAddress(postalAddress: CNPostalAddress; style: CNPostalAddressFormatterStyle; attributes: NSDictionary): NSAttributedString; cdecl;
    [MethodName('stringFromPostalAddress:style:')]
    {class} function stringFromPostalAddress(postalAddress: CNPostalAddress; style: CNPostalAddressFormatterStyle): NSString; cdecl;
  end;

  CNPostalAddressFormatter = interface(NSFormatter)
    ['{C3FD211A-217E-44C5-9BD1-1B4A66779A8C}']
    [MethodName('attributedStringFromPostalAddress:withDefaultAttributes:')]
    function attributedStringFromPostalAddress(postalAddress: CNPostalAddress; attributes: NSDictionary): NSAttributedString; cdecl;
    procedure setStyle(style: CNPostalAddressFormatterStyle); cdecl;
    function stringFromPostalAddress(postalAddress: CNPostalAddress): NSString; cdecl;
    function style: CNPostalAddressFormatterStyle; cdecl;
  end;
  TCNPostalAddressFormatter = class(TOCGenericImport<CNPostalAddressFormatterClass, CNPostalAddressFormatter>) end;

  CNSaveRequestClass = interface(NSObjectClass)
    ['{5E084EA6-E678-46FD-A062-C4DF878FEB73}']
  end;

  CNSaveRequest = interface(NSObject)
    ['{045DA7DD-C3DB-44D5-8900-AD33C1FDE758}']
    [MethodName('addContact:toContainerWithIdentifier:')]
    procedure addContact(contact: CNMutableContact; identifier: NSString); cdecl;
    [MethodName('addGroup:toContainerWithIdentifier:')]
    procedure addGroup(group: CNMutableGroup; identifier: NSString); cdecl;
    [MethodName('addMember:toGroup:')]
    procedure addMember(contact: CNContact; group: CNGroup); cdecl;
    procedure deleteContact(contact: CNMutableContact); cdecl;
    procedure deleteGroup(group: CNMutableGroup); cdecl;
    [MethodName('removeMember:fromGroup:')]
    procedure removeMember(contact: CNContact; group: CNGroup); cdecl;
    procedure updateContact(contact: CNMutableContact); cdecl;
    procedure updateGroup(group: CNMutableGroup); cdecl;
  end;
  TCNSaveRequest = class(TOCGenericImport<CNSaveRequestClass, CNSaveRequest>) end;

function CNLabelHome: NSString;
function CNLabelWork: NSString;
function CNLabelSchool: NSString;
function CNLabelOther: NSString;
function CNLabelEmailiCloud: NSString;
function CNLabelURLAddressHomePage: NSString;
function CNLabelDateAnniversary: NSString;
function CNLabelPhoneNumberiPhone: NSString;
function CNLabelPhoneNumberMobile: NSString;
function CNLabelPhoneNumberMain: NSString;
function CNLabelPhoneNumberHomeFax: NSString;
function CNLabelPhoneNumberWorkFax: NSString;
function CNLabelPhoneNumberOtherFax: NSString;
function CNLabelPhoneNumberPager: NSString;
function CNPostalAddressStreetKey: NSString;
function CNPostalAddressSubLocalityKey: NSString;
function CNPostalAddressCityKey: NSString;
function CNPostalAddressSubAdministrativeAreaKey: NSString;
function CNPostalAddressStateKey: NSString;
function CNPostalAddressPostalCodeKey: NSString;
function CNPostalAddressCountryKey: NSString;
function CNPostalAddressISOCountryCodeKey: NSString;
function CNLabelContactRelationAssistant: NSString;
function CNLabelContactRelationManager: NSString;
function CNLabelContactRelationColleague: NSString;
function CNLabelContactRelationTeacher: NSString;
function CNLabelContactRelationSibling: NSString;
function CNLabelContactRelationYoungerSibling: NSString;
function CNLabelContactRelationElderSibling: NSString;
function CNLabelContactRelationSister: NSString;
function CNLabelContactRelationYoungerSister: NSString;
function CNLabelContactRelationYoungestSister: NSString;
function CNLabelContactRelationElderSister: NSString;
function CNLabelContactRelationEldestSister: NSString;
function CNLabelContactRelationBrother: NSString;
function CNLabelContactRelationYoungerBrother: NSString;
function CNLabelContactRelationYoungestBrother: NSString;
function CNLabelContactRelationElderBrother: NSString;
function CNLabelContactRelationEldestBrother: NSString;
function CNLabelContactRelationFriend: NSString;
function CNLabelContactRelationMaleFriend: NSString;
function CNLabelContactRelationFemaleFriend: NSString;
function CNLabelContactRelationSpouse: NSString;
function CNLabelContactRelationWife: NSString;
function CNLabelContactRelationHusband: NSString;
function CNLabelContactRelationPartner: NSString;
function CNLabelContactRelationMalePartner: NSString;
function CNLabelContactRelationFemalePartner: NSString;
function CNLabelContactRelationGirlfriendOrBoyfriend: NSString;
function CNLabelContactRelationGirlfriend: NSString;
function CNLabelContactRelationBoyfriend: NSString;
function CNLabelContactRelationParent: NSString;
function CNLabelContactRelationMother: NSString;
function CNLabelContactRelationFather: NSString;
function CNLabelContactRelationChild: NSString;
function CNLabelContactRelationDaughter: NSString;
function CNLabelContactRelationSon: NSString;
function CNLabelContactRelationGrandparent: NSString;
function CNLabelContactRelationGrandmother: NSString;
function CNLabelContactRelationGrandmotherMothersMother: NSString;
function CNLabelContactRelationGrandmotherFathersMother: NSString;
function CNLabelContactRelationGrandfather: NSString;
function CNLabelContactRelationGrandfatherMothersFather: NSString;
function CNLabelContactRelationGrandfatherFathersFather: NSString;
function CNLabelContactRelationGreatGrandparent: NSString;
function CNLabelContactRelationGreatGrandmother: NSString;
function CNLabelContactRelationGreatGrandfather: NSString;
function CNLabelContactRelationGrandchild: NSString;
function CNLabelContactRelationGranddaughter: NSString;
function CNLabelContactRelationGranddaughterDaughtersDaughter: NSString;
function CNLabelContactRelationGranddaughterSonsDaughter: NSString;
function CNLabelContactRelationGrandson: NSString;
function CNLabelContactRelationGrandsonDaughtersSon: NSString;
function CNLabelContactRelationGrandsonSonsSon: NSString;
function CNLabelContactRelationGreatGrandchild: NSString;
function CNLabelContactRelationGreatGranddaughter: NSString;
function CNLabelContactRelationGreatGrandson: NSString;
function CNLabelContactRelationParentInLaw: NSString;
function CNLabelContactRelationMotherInLaw: NSString;
function CNLabelContactRelationMotherInLawWifesMother: NSString;
function CNLabelContactRelationMotherInLawHusbandsMother: NSString;
function CNLabelContactRelationFatherInLaw: NSString;
function CNLabelContactRelationFatherInLawWifesFather: NSString;
function CNLabelContactRelationFatherInLawHusbandsFather: NSString;
function CNLabelContactRelationCoParentInLaw: NSString;
function CNLabelContactRelationCoMotherInLaw: NSString;
function CNLabelContactRelationCoFatherInLaw: NSString;
function CNLabelContactRelationSiblingInLaw: NSString;
function CNLabelContactRelationYoungerSiblingInLaw: NSString;
function CNLabelContactRelationElderSiblingInLaw: NSString;
function CNLabelContactRelationSisterInLaw: NSString;
function CNLabelContactRelationYoungerSisterInLaw: NSString;
function CNLabelContactRelationElderSisterInLaw: NSString;
function CNLabelContactRelationSisterInLawSpousesSister: NSString;
function CNLabelContactRelationSisterInLawWifesSister: NSString;
function CNLabelContactRelationSisterInLawHusbandsSister: NSString;
function CNLabelContactRelationSisterInLawBrothersWife: NSString;
function CNLabelContactRelationSisterInLawYoungerBrothersWife: NSString;
function CNLabelContactRelationSisterInLawElderBrothersWife: NSString;
function CNLabelContactRelationBrotherInLaw: NSString;
function CNLabelContactRelationYoungerBrotherInLaw: NSString;
function CNLabelContactRelationElderBrotherInLaw: NSString;
function CNLabelContactRelationBrotherInLawSpousesBrother: NSString;
function CNLabelContactRelationBrotherInLawHusbandsBrother: NSString;
function CNLabelContactRelationBrotherInLawWifesBrother: NSString;
function CNLabelContactRelationBrotherInLawSistersHusband: NSString;
function CNLabelContactRelationBrotherInLawYoungerSistersHusband: NSString;
function CNLabelContactRelationBrotherInLawElderSistersHusband: NSString;
function CNLabelContactRelationSisterInLawWifesBrothersWife: NSString;
function CNLabelContactRelationSisterInLawHusbandsBrothersWife: NSString;
function CNLabelContactRelationBrotherInLawWifesSistersHusband: NSString;
function CNLabelContactRelationBrotherInLawHusbandsSistersHusband: NSString;
function CNLabelContactRelationCoSiblingInLaw: NSString;
function CNLabelContactRelationCoSisterInLaw: NSString;
function CNLabelContactRelationCoBrotherInLaw: NSString;
function CNLabelContactRelationChildInLaw: NSString;
function CNLabelContactRelationDaughterInLaw: NSString;
function CNLabelContactRelationSonInLaw: NSString;
function CNLabelContactRelationCousin: NSString;
function CNLabelContactRelationYoungerCousin: NSString;
function CNLabelContactRelationElderCousin: NSString;
function CNLabelContactRelationMaleCousin: NSString;
function CNLabelContactRelationFemaleCousin: NSString;
function CNLabelContactRelationCousinParentsSiblingsChild: NSString;
function CNLabelContactRelationCousinParentsSiblingsSon: NSString;
function CNLabelContactRelationYoungerCousinParentsSiblingsSon: NSString;
function CNLabelContactRelationElderCousinParentsSiblingsSon: NSString;
function CNLabelContactRelationCousinParentsSiblingsDaughter: NSString;
function CNLabelContactRelationYoungerCousinParentsSiblingsDaughter: NSString;
function CNLabelContactRelationElderCousinParentsSiblingsDaughter: NSString;
function CNLabelContactRelationCousinMothersSistersDaughter: NSString;
function CNLabelContactRelationYoungerCousinMothersSistersDaughter: NSString;
function CNLabelContactRelationElderCousinMothersSistersDaughter: NSString;
function CNLabelContactRelationCousinMothersSistersSon: NSString;
function CNLabelContactRelationYoungerCousinMothersSistersSon: NSString;
function CNLabelContactRelationElderCousinMothersSistersSon: NSString;
function CNLabelContactRelationCousinMothersBrothersDaughter: NSString;
function CNLabelContactRelationYoungerCousinMothersBrothersDaughter: NSString;
function CNLabelContactRelationElderCousinMothersBrothersDaughter: NSString;
function CNLabelContactRelationCousinMothersBrothersSon: NSString;
function CNLabelContactRelationYoungerCousinMothersBrothersSon: NSString;
function CNLabelContactRelationElderCousinMothersBrothersSon: NSString;
function CNLabelContactRelationCousinFathersSistersDaughter: NSString;
function CNLabelContactRelationYoungerCousinFathersSistersDaughter: NSString;
function CNLabelContactRelationElderCousinFathersSistersDaughter: NSString;
function CNLabelContactRelationCousinFathersSistersSon: NSString;
function CNLabelContactRelationYoungerCousinFathersSistersSon: NSString;
function CNLabelContactRelationElderCousinFathersSistersSon: NSString;
function CNLabelContactRelationCousinFathersBrothersDaughter: NSString;
function CNLabelContactRelationYoungerCousinFathersBrothersDaughter: NSString;
function CNLabelContactRelationElderCousinFathersBrothersDaughter: NSString;
function CNLabelContactRelationCousinFathersBrothersSon: NSString;
function CNLabelContactRelationYoungerCousinFathersBrothersSon: NSString;
function CNLabelContactRelationElderCousinFathersBrothersSon: NSString;
function CNLabelContactRelationCousinGrandparentsSiblingsChild: NSString;
function CNLabelContactRelationCousinGrandparentsSiblingsDaughter: NSString;
function CNLabelContactRelationCousinGrandparentsSiblingsSon: NSString;
function CNLabelContactRelationYoungerCousinMothersSiblingsSonOrFathersSistersSon: NSString;
function CNLabelContactRelationElderCousinMothersSiblingsSonOrFathersSistersSon: NSString;
function CNLabelContactRelationYoungerCousinMothersSiblingsDaughterOrFathersSistersDaughter: NSString;
function CNLabelContactRelationElderCousinMothersSiblingsDaughterOrFathersSistersDaughter: NSString;
function CNLabelContactRelationParentsSibling: NSString;
function CNLabelContactRelationParentsYoungerSibling: NSString;
function CNLabelContactRelationParentsElderSibling: NSString;
function CNLabelContactRelationParentsSiblingMothersSibling: NSString;
function CNLabelContactRelationParentsSiblingMothersYoungerSibling: NSString;
function CNLabelContactRelationParentsSiblingMothersElderSibling: NSString;
function CNLabelContactRelationParentsSiblingFathersSibling: NSString;
function CNLabelContactRelationParentsSiblingFathersYoungerSibling: NSString;
function CNLabelContactRelationParentsSiblingFathersElderSibling: NSString;
function CNLabelContactRelationAunt: NSString;
function CNLabelContactRelationAuntParentsSister: NSString;
function CNLabelContactRelationAuntParentsYoungerSister: NSString;
function CNLabelContactRelationAuntParentsElderSister: NSString;
function CNLabelContactRelationAuntFathersSister: NSString;
function CNLabelContactRelationAuntFathersYoungerSister: NSString;
function CNLabelContactRelationAuntFathersElderSister: NSString;
function CNLabelContactRelationAuntFathersBrothersWife: NSString;
function CNLabelContactRelationAuntFathersYoungerBrothersWife: NSString;
function CNLabelContactRelationAuntFathersElderBrothersWife: NSString;
function CNLabelContactRelationAuntMothersSister: NSString;
function CNLabelContactRelationAuntMothersYoungerSister: NSString;
function CNLabelContactRelationAuntMothersElderSister: NSString;
function CNLabelContactRelationAuntMothersBrothersWife: NSString;
function CNLabelContactRelationGrandaunt: NSString;
function CNLabelContactRelationUncle: NSString;
function CNLabelContactRelationUncleParentsBrother: NSString;
function CNLabelContactRelationUncleParentsYoungerBrother: NSString;
function CNLabelContactRelationUncleParentsElderBrother: NSString;
function CNLabelContactRelationUncleMothersBrother: NSString;
function CNLabelContactRelationUncleMothersYoungerBrother: NSString;
function CNLabelContactRelationUncleMothersElderBrother: NSString;
function CNLabelContactRelationUncleMothersSistersHusband: NSString;
function CNLabelContactRelationUncleFathersBrother: NSString;
function CNLabelContactRelationUncleFathersYoungerBrother: NSString;
function CNLabelContactRelationUncleFathersElderBrother: NSString;
function CNLabelContactRelationUncleFathersSistersHusband: NSString;
function CNLabelContactRelationUncleFathersYoungerSistersHusband: NSString;
function CNLabelContactRelationUncleFathersElderSistersHusband: NSString;
function CNLabelContactRelationGranduncle: NSString;
function CNLabelContactRelationSiblingsChild: NSString;
function CNLabelContactRelationNiece: NSString;
function CNLabelContactRelationNieceSistersDaughter: NSString;
function CNLabelContactRelationNieceBrothersDaughter: NSString;
function CNLabelContactRelationNieceSistersDaughterOrWifesSiblingsDaughter: NSString;
function CNLabelContactRelationNieceBrothersDaughterOrHusbandsSiblingsDaughter: NSString;
function CNLabelContactRelationNephew: NSString;
function CNLabelContactRelationNephewSistersSon: NSString;
function CNLabelContactRelationNephewBrothersSon: NSString;
function CNLabelContactRelationNephewBrothersSonOrHusbandsSiblingsSon: NSString;
function CNLabelContactRelationNephewSistersSonOrWifesSiblingsSon: NSString;
function CNLabelContactRelationGrandniece: NSString;
function CNLabelContactRelationGrandnieceSistersGranddaughter: NSString;
function CNLabelContactRelationGrandnieceBrothersGranddaughter: NSString;
function CNLabelContactRelationGrandnephew: NSString;
function CNLabelContactRelationGrandnephewSistersGrandson: NSString;
function CNLabelContactRelationGrandnephewBrothersGrandson: NSString;
function CNLabelContactRelationStepparent: NSString;
function CNLabelContactRelationStepfather: NSString;
function CNLabelContactRelationStepmother: NSString;
function CNLabelContactRelationStepchild: NSString;
function CNLabelContactRelationStepson: NSString;
function CNLabelContactRelationStepdaughter: NSString;
function CNLabelContactRelationStepbrother: NSString;
function CNLabelContactRelationStepsister: NSString;
function CNLabelContactRelationMotherInLawOrStepmother: NSString;
function CNLabelContactRelationFatherInLawOrStepfather: NSString;
function CNLabelContactRelationDaughterInLawOrStepdaughter: NSString;
function CNLabelContactRelationSonInLawOrStepson: NSString;
function CNLabelContactRelationCousinOrSiblingsChild: NSString;
function CNLabelContactRelationNieceOrCousin: NSString;
function CNLabelContactRelationNephewOrCousin: NSString;
function CNLabelContactRelationGrandchildOrSiblingsChild: NSString;
function CNLabelContactRelationGreatGrandchildOrSiblingsGrandchild: NSString;
function CNLabelContactRelationDaughterInLawOrSisterInLaw: NSString;
function CNLabelContactRelationSonInLawOrBrotherInLaw: NSString;
function CNSocialProfileURLStringKey: NSString;
function CNSocialProfileUsernameKey: NSString;
function CNSocialProfileUserIdentifierKey: NSString;
function CNSocialProfileServiceKey: NSString;
function CNSocialProfileServiceFacebook: NSString;
function CNSocialProfileServiceFlickr: NSString;
function CNSocialProfileServiceLinkedIn: NSString;
function CNSocialProfileServiceMySpace: NSString;
function CNSocialProfileServiceSinaWeibo: NSString;
function CNSocialProfileServiceTencentWeibo: NSString;
function CNSocialProfileServiceTwitter: NSString;
function CNSocialProfileServiceYelp: NSString;
function CNSocialProfileServiceGameCenter: NSString;
function CNInstantMessageAddressUsernameKey: NSString;
function CNInstantMessageAddressServiceKey: NSString;
function CNInstantMessageServiceAIM: NSString;
function CNInstantMessageServiceFacebook: NSString;
function CNInstantMessageServiceGaduGadu: NSString;
function CNInstantMessageServiceGoogleTalk: NSString;
function CNInstantMessageServiceICQ: NSString;
function CNInstantMessageServiceJabber: NSString;
function CNInstantMessageServiceMSN: NSString;
function CNInstantMessageServiceQQ: NSString;
function CNInstantMessageServiceSkype: NSString;
function CNInstantMessageServiceYahoo: NSString;
function CNContactPropertyNotFetchedExceptionName: NSString;
function CNContactIdentifierKey: NSString;
function CNContactNamePrefixKey: NSString;
function CNContactGivenNameKey: NSString;
function CNContactMiddleNameKey: NSString;
function CNContactFamilyNameKey: NSString;
function CNContactPreviousFamilyNameKey: NSString;
function CNContactNameSuffixKey: NSString;
function CNContactNicknameKey: NSString;
function CNContactOrganizationNameKey: NSString;
function CNContactDepartmentNameKey: NSString;
function CNContactJobTitleKey: NSString;
function CNContactPhoneticGivenNameKey: NSString;
function CNContactPhoneticMiddleNameKey: NSString;
function CNContactPhoneticFamilyNameKey: NSString;
function CNContactPhoneticOrganizationNameKey: NSString;
function CNContactBirthdayKey: NSString;
function CNContactNonGregorianBirthdayKey: NSString;
function CNContactNoteKey: NSString;
function CNContactImageDataKey: NSString;
function CNContactThumbnailImageDataKey: NSString;
function CNContactImageDataAvailableKey: NSString;
function CNContactTypeKey: NSString;
function CNContactPhoneNumbersKey: NSString;
function CNContactEmailAddressesKey: NSString;
function CNContactPostalAddressesKey: NSString;
function CNContactDatesKey: NSString;
function CNContactUrlAddressesKey: NSString;
function CNContactRelationsKey: NSString;
function CNContactSocialProfilesKey: NSString;
function CNContactInstantMessageAddressesKey: NSString;
function CNContactPropertyAttribute: NSString;
function CNContactStoreDidChangeNotification: NSString;
function CNContainerIdentifierKey: NSString;
function CNContainerNameKey: NSString;
function CNContainerTypeKey: NSString;
function CNErrorDomain: NSString;
function CNErrorUserInfoAffectedRecordsKey: NSString;
function CNErrorUserInfoAffectedRecordIdentifiersKey: NSString;
function CNErrorUserInfoValidationErrorsKey: NSString;
function CNErrorUserInfoKeyPathsKey: NSString;
function CNGroupIdentifierKey: NSString;
function CNGroupNameKey: NSString;
function CNPostalAddressPropertyAttribute: NSString;
function CNPostalAddressLocalizedPropertyNameAttribute: NSString;

const
  libContacts = '/System/Library/Frameworks/Contacts.framework/Contacts';

implementation

uses
  Posix.Dlfcn;

var
  ContactsModule: THandle;

function CNLabelHome: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelHome');
end;

function CNLabelWork: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelWork');
end;

function CNLabelSchool: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelSchool');
end;

function CNLabelOther: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelOther');
end;

function CNLabelEmailiCloud: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelEmailiCloud');
end;

function CNLabelURLAddressHomePage: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelURLAddressHomePage');
end;

function CNLabelDateAnniversary: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelDateAnniversary');
end;

function CNLabelPhoneNumberiPhone: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelPhoneNumberiPhone');
end;

function CNLabelPhoneNumberMobile: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelPhoneNumberMobile');
end;

function CNLabelPhoneNumberMain: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelPhoneNumberMain');
end;

function CNLabelPhoneNumberHomeFax: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelPhoneNumberHomeFax');
end;

function CNLabelPhoneNumberWorkFax: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelPhoneNumberWorkFax');
end;

function CNLabelPhoneNumberOtherFax: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelPhoneNumberOtherFax');
end;

function CNLabelPhoneNumberPager: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelPhoneNumberPager');
end;

function CNPostalAddressStreetKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressStreetKey');
end;

function CNPostalAddressSubLocalityKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressSubLocalityKey');
end;

function CNPostalAddressCityKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressCityKey');
end;

function CNPostalAddressSubAdministrativeAreaKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressSubAdministrativeAreaKey');
end;

function CNPostalAddressStateKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressStateKey');
end;

function CNPostalAddressPostalCodeKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressPostalCodeKey');
end;

function CNPostalAddressCountryKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressCountryKey');
end;

function CNPostalAddressISOCountryCodeKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressISOCountryCodeKey');
end;

function CNLabelContactRelationAssistant: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAssistant');
end;

function CNLabelContactRelationManager: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationManager');
end;

function CNLabelContactRelationColleague: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationColleague');
end;

function CNLabelContactRelationTeacher: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationTeacher');
end;

function CNLabelContactRelationSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSibling');
end;

function CNLabelContactRelationYoungerSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerSibling');
end;

function CNLabelContactRelationElderSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderSibling');
end;

function CNLabelContactRelationSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSister');
end;

function CNLabelContactRelationYoungerSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerSister');
end;

function CNLabelContactRelationYoungestSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungestSister');
end;

function CNLabelContactRelationElderSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderSister');
end;

function CNLabelContactRelationEldestSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationEldestSister');
end;

function CNLabelContactRelationBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrother');
end;

function CNLabelContactRelationYoungerBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerBrother');
end;

function CNLabelContactRelationYoungestBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungestBrother');
end;

function CNLabelContactRelationElderBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderBrother');
end;

function CNLabelContactRelationEldestBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationEldestBrother');
end;

function CNLabelContactRelationFriend: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFriend');
end;

function CNLabelContactRelationMaleFriend: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationMaleFriend');
end;

function CNLabelContactRelationFemaleFriend: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFemaleFriend');
end;

function CNLabelContactRelationSpouse: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSpouse');
end;

function CNLabelContactRelationWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationWife');
end;

function CNLabelContactRelationHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationHusband');
end;

function CNLabelContactRelationPartner: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationPartner');
end;

function CNLabelContactRelationMalePartner: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationMalePartner');
end;

function CNLabelContactRelationFemalePartner: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFemalePartner');
end;

function CNLabelContactRelationGirlfriendOrBoyfriend: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGirlfriendOrBoyfriend');
end;

function CNLabelContactRelationGirlfriend: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGirlfriend');
end;

function CNLabelContactRelationBoyfriend: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBoyfriend');
end;

function CNLabelContactRelationParent: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParent');
end;

function CNLabelContactRelationMother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationMother');
end;

function CNLabelContactRelationFather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFather');
end;

function CNLabelContactRelationChild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationChild');
end;

function CNLabelContactRelationDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationDaughter');
end;

function CNLabelContactRelationSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSon');
end;

function CNLabelContactRelationGrandparent: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandparent');
end;

function CNLabelContactRelationGrandmother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandmother');
end;

function CNLabelContactRelationGrandmotherMothersMother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandmotherMothersMother');
end;

function CNLabelContactRelationGrandmotherFathersMother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandmotherFathersMother');
end;

function CNLabelContactRelationGrandfather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandfather');
end;

function CNLabelContactRelationGrandfatherMothersFather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandfatherMothersFather');
end;

function CNLabelContactRelationGrandfatherFathersFather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandfatherFathersFather');
end;

function CNLabelContactRelationGreatGrandparent: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGreatGrandparent');
end;

function CNLabelContactRelationGreatGrandmother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGreatGrandmother');
end;

function CNLabelContactRelationGreatGrandfather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGreatGrandfather');
end;

function CNLabelContactRelationGrandchild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandchild');
end;

function CNLabelContactRelationGranddaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGranddaughter');
end;

function CNLabelContactRelationGranddaughterDaughtersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGranddaughterDaughtersDaughter');
end;

function CNLabelContactRelationGranddaughterSonsDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGranddaughterSonsDaughter');
end;

function CNLabelContactRelationGrandson: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandson');
end;

function CNLabelContactRelationGrandsonDaughtersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandsonDaughtersSon');
end;

function CNLabelContactRelationGrandsonSonsSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandsonSonsSon');
end;

function CNLabelContactRelationGreatGrandchild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGreatGrandchild');
end;

function CNLabelContactRelationGreatGranddaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGreatGranddaughter');
end;

function CNLabelContactRelationGreatGrandson: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGreatGrandson');
end;

function CNLabelContactRelationParentInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentInLaw');
end;

function CNLabelContactRelationMotherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationMotherInLaw');
end;

function CNLabelContactRelationMotherInLawWifesMother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationMotherInLawWifesMother');
end;

function CNLabelContactRelationMotherInLawHusbandsMother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationMotherInLawHusbandsMother');
end;

function CNLabelContactRelationFatherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFatherInLaw');
end;

function CNLabelContactRelationFatherInLawWifesFather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFatherInLawWifesFather');
end;

function CNLabelContactRelationFatherInLawHusbandsFather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFatherInLawHusbandsFather');
end;

function CNLabelContactRelationCoParentInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCoParentInLaw');
end;

function CNLabelContactRelationCoMotherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCoMotherInLaw');
end;

function CNLabelContactRelationCoFatherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCoFatherInLaw');
end;

function CNLabelContactRelationSiblingInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSiblingInLaw');
end;

function CNLabelContactRelationYoungerSiblingInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerSiblingInLaw');
end;

function CNLabelContactRelationElderSiblingInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderSiblingInLaw');
end;

function CNLabelContactRelationSisterInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLaw');
end;

function CNLabelContactRelationYoungerSisterInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerSisterInLaw');
end;

function CNLabelContactRelationElderSisterInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderSisterInLaw');
end;

function CNLabelContactRelationSisterInLawSpousesSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLawSpousesSister');
end;

function CNLabelContactRelationSisterInLawWifesSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLawWifesSister');
end;

function CNLabelContactRelationSisterInLawHusbandsSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLawHusbandsSister');
end;

function CNLabelContactRelationSisterInLawBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLawBrothersWife');
end;

function CNLabelContactRelationSisterInLawYoungerBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLawYoungerBrothersWife');
end;

function CNLabelContactRelationSisterInLawElderBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLawElderBrothersWife');
end;

function CNLabelContactRelationBrotherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLaw');
end;

function CNLabelContactRelationYoungerBrotherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerBrotherInLaw');
end;

function CNLabelContactRelationElderBrotherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderBrotherInLaw');
end;

function CNLabelContactRelationBrotherInLawSpousesBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLawSpousesBrother');
end;

function CNLabelContactRelationBrotherInLawHusbandsBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLawHusbandsBrother');
end;

function CNLabelContactRelationBrotherInLawWifesBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLawWifesBrother');
end;

function CNLabelContactRelationBrotherInLawSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLawSistersHusband');
end;

function CNLabelContactRelationBrotherInLawYoungerSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLawYoungerSistersHusband');
end;

function CNLabelContactRelationBrotherInLawElderSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLawElderSistersHusband');
end;

function CNLabelContactRelationSisterInLawWifesBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLawWifesBrothersWife');
end;

function CNLabelContactRelationSisterInLawHusbandsBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSisterInLawHusbandsBrothersWife');
end;

function CNLabelContactRelationBrotherInLawWifesSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLawWifesSistersHusband');
end;

function CNLabelContactRelationBrotherInLawHusbandsSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationBrotherInLawHusbandsSistersHusband');
end;

function CNLabelContactRelationCoSiblingInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCoSiblingInLaw');
end;

function CNLabelContactRelationCoSisterInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCoSisterInLaw');
end;

function CNLabelContactRelationCoBrotherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCoBrotherInLaw');
end;

function CNLabelContactRelationChildInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationChildInLaw');
end;

function CNLabelContactRelationDaughterInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationDaughterInLaw');
end;

function CNLabelContactRelationSonInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSonInLaw');
end;

function CNLabelContactRelationCousin: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousin');
end;

function CNLabelContactRelationYoungerCousin: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousin');
end;

function CNLabelContactRelationElderCousin: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousin');
end;

function CNLabelContactRelationMaleCousin: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationMaleCousin');
end;

function CNLabelContactRelationFemaleCousin: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFemaleCousin');
end;

function CNLabelContactRelationCousinParentsSiblingsChild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinParentsSiblingsChild');
end;

function CNLabelContactRelationCousinParentsSiblingsSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinParentsSiblingsSon');
end;

function CNLabelContactRelationYoungerCousinParentsSiblingsSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinParentsSiblingsSon');
end;

function CNLabelContactRelationElderCousinParentsSiblingsSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinParentsSiblingsSon');
end;

function CNLabelContactRelationCousinParentsSiblingsDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinParentsSiblingsDaughter');
end;

function CNLabelContactRelationYoungerCousinParentsSiblingsDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinParentsSiblingsDaughter');
end;

function CNLabelContactRelationElderCousinParentsSiblingsDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinParentsSiblingsDaughter');
end;

function CNLabelContactRelationCousinMothersSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinMothersSistersDaughter');
end;

function CNLabelContactRelationYoungerCousinMothersSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinMothersSistersDaughter');
end;

function CNLabelContactRelationElderCousinMothersSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinMothersSistersDaughter');
end;

function CNLabelContactRelationCousinMothersSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinMothersSistersSon');
end;

function CNLabelContactRelationYoungerCousinMothersSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinMothersSistersSon');
end;

function CNLabelContactRelationElderCousinMothersSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinMothersSistersSon');
end;

function CNLabelContactRelationCousinMothersBrothersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinMothersBrothersDaughter');
end;

function CNLabelContactRelationYoungerCousinMothersBrothersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinMothersBrothersDaughter');
end;

function CNLabelContactRelationElderCousinMothersBrothersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinMothersBrothersDaughter');
end;

function CNLabelContactRelationCousinMothersBrothersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinMothersBrothersSon');
end;

function CNLabelContactRelationYoungerCousinMothersBrothersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinMothersBrothersSon');
end;

function CNLabelContactRelationElderCousinMothersBrothersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinMothersBrothersSon');
end;

function CNLabelContactRelationCousinFathersSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinFathersSistersDaughter');
end;

function CNLabelContactRelationYoungerCousinFathersSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinFathersSistersDaughter');
end;

function CNLabelContactRelationElderCousinFathersSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinFathersSistersDaughter');
end;

function CNLabelContactRelationCousinFathersSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinFathersSistersSon');
end;

function CNLabelContactRelationYoungerCousinFathersSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinFathersSistersSon');
end;

function CNLabelContactRelationElderCousinFathersSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinFathersSistersSon');
end;

function CNLabelContactRelationCousinFathersBrothersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinFathersBrothersDaughter');
end;

function CNLabelContactRelationYoungerCousinFathersBrothersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinFathersBrothersDaughter');
end;

function CNLabelContactRelationElderCousinFathersBrothersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinFathersBrothersDaughter');
end;

function CNLabelContactRelationCousinFathersBrothersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinFathersBrothersSon');
end;

function CNLabelContactRelationYoungerCousinFathersBrothersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinFathersBrothersSon');
end;

function CNLabelContactRelationElderCousinFathersBrothersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinFathersBrothersSon');
end;

function CNLabelContactRelationCousinGrandparentsSiblingsChild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinGrandparentsSiblingsChild');
end;

function CNLabelContactRelationCousinGrandparentsSiblingsDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinGrandparentsSiblingsDaughter');
end;

function CNLabelContactRelationCousinGrandparentsSiblingsSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinGrandparentsSiblingsSon');
end;

function CNLabelContactRelationYoungerCousinMothersSiblingsSonOrFathersSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinMothersSiblingsSonOrFathersSistersSon');
end;

function CNLabelContactRelationElderCousinMothersSiblingsSonOrFathersSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinMothersSiblingsSonOrFathersSistersSon');
end;

function CNLabelContactRelationYoungerCousinMothersSiblingsDaughterOrFathersSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationYoungerCousinMothersSiblingsDaughterOrFathersSistersDaughter');
end;

function CNLabelContactRelationElderCousinMothersSiblingsDaughterOrFathersSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationElderCousinMothersSiblingsDaughterOrFathersSistersDaughter');
end;

function CNLabelContactRelationParentsSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsSibling');
end;

function CNLabelContactRelationParentsYoungerSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsYoungerSibling');
end;

function CNLabelContactRelationParentsElderSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsElderSibling');
end;

function CNLabelContactRelationParentsSiblingMothersSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsSiblingMothersSibling');
end;

function CNLabelContactRelationParentsSiblingMothersYoungerSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsSiblingMothersYoungerSibling');
end;

function CNLabelContactRelationParentsSiblingMothersElderSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsSiblingMothersElderSibling');
end;

function CNLabelContactRelationParentsSiblingFathersSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsSiblingFathersSibling');
end;

function CNLabelContactRelationParentsSiblingFathersYoungerSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsSiblingFathersYoungerSibling');
end;

function CNLabelContactRelationParentsSiblingFathersElderSibling: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationParentsSiblingFathersElderSibling');
end;

function CNLabelContactRelationAunt: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAunt');
end;

function CNLabelContactRelationAuntParentsSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntParentsSister');
end;

function CNLabelContactRelationAuntParentsYoungerSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntParentsYoungerSister');
end;

function CNLabelContactRelationAuntParentsElderSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntParentsElderSister');
end;

function CNLabelContactRelationAuntFathersSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntFathersSister');
end;

function CNLabelContactRelationAuntFathersYoungerSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntFathersYoungerSister');
end;

function CNLabelContactRelationAuntFathersElderSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntFathersElderSister');
end;

function CNLabelContactRelationAuntFathersBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntFathersBrothersWife');
end;

function CNLabelContactRelationAuntFathersYoungerBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntFathersYoungerBrothersWife');
end;

function CNLabelContactRelationAuntFathersElderBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntFathersElderBrothersWife');
end;

function CNLabelContactRelationAuntMothersSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntMothersSister');
end;

function CNLabelContactRelationAuntMothersYoungerSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntMothersYoungerSister');
end;

function CNLabelContactRelationAuntMothersElderSister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntMothersElderSister');
end;

function CNLabelContactRelationAuntMothersBrothersWife: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationAuntMothersBrothersWife');
end;

function CNLabelContactRelationGrandaunt: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandaunt');
end;

function CNLabelContactRelationUncle: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncle');
end;

function CNLabelContactRelationUncleParentsBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleParentsBrother');
end;

function CNLabelContactRelationUncleParentsYoungerBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleParentsYoungerBrother');
end;

function CNLabelContactRelationUncleParentsElderBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleParentsElderBrother');
end;

function CNLabelContactRelationUncleMothersBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleMothersBrother');
end;

function CNLabelContactRelationUncleMothersYoungerBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleMothersYoungerBrother');
end;

function CNLabelContactRelationUncleMothersElderBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleMothersElderBrother');
end;

function CNLabelContactRelationUncleMothersSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleMothersSistersHusband');
end;

function CNLabelContactRelationUncleFathersBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleFathersBrother');
end;

function CNLabelContactRelationUncleFathersYoungerBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleFathersYoungerBrother');
end;

function CNLabelContactRelationUncleFathersElderBrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleFathersElderBrother');
end;

function CNLabelContactRelationUncleFathersSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleFathersSistersHusband');
end;

function CNLabelContactRelationUncleFathersYoungerSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleFathersYoungerSistersHusband');
end;

function CNLabelContactRelationUncleFathersElderSistersHusband: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationUncleFathersElderSistersHusband');
end;

function CNLabelContactRelationGranduncle: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGranduncle');
end;

function CNLabelContactRelationSiblingsChild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSiblingsChild');
end;

function CNLabelContactRelationNiece: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNiece');
end;

function CNLabelContactRelationNieceSistersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNieceSistersDaughter');
end;

function CNLabelContactRelationNieceBrothersDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNieceBrothersDaughter');
end;

function CNLabelContactRelationNieceSistersDaughterOrWifesSiblingsDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNieceSistersDaughterOrWifesSiblingsDaughter');
end;

function CNLabelContactRelationNieceBrothersDaughterOrHusbandsSiblingsDaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNieceBrothersDaughterOrHusbandsSiblingsDaughter');
end;

function CNLabelContactRelationNephew: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNephew');
end;

function CNLabelContactRelationNephewSistersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNephewSistersSon');
end;

function CNLabelContactRelationNephewBrothersSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNephewBrothersSon');
end;

function CNLabelContactRelationNephewBrothersSonOrHusbandsSiblingsSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNephewBrothersSonOrHusbandsSiblingsSon');
end;

function CNLabelContactRelationNephewSistersSonOrWifesSiblingsSon: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNephewSistersSonOrWifesSiblingsSon');
end;

function CNLabelContactRelationGrandniece: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandniece');
end;

function CNLabelContactRelationGrandnieceSistersGranddaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandnieceSistersGranddaughter');
end;

function CNLabelContactRelationGrandnieceBrothersGranddaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandnieceBrothersGranddaughter');
end;

function CNLabelContactRelationGrandnephew: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandnephew');
end;

function CNLabelContactRelationGrandnephewSistersGrandson: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandnephewSistersGrandson');
end;

function CNLabelContactRelationGrandnephewBrothersGrandson: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandnephewBrothersGrandson');
end;

function CNLabelContactRelationStepparent: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationStepparent');
end;

function CNLabelContactRelationStepfather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationStepfather');
end;

function CNLabelContactRelationStepmother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationStepmother');
end;

function CNLabelContactRelationStepchild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationStepchild');
end;

function CNLabelContactRelationStepson: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationStepson');
end;

function CNLabelContactRelationStepdaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationStepdaughter');
end;

function CNLabelContactRelationStepbrother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationStepbrother');
end;

function CNLabelContactRelationStepsister: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationStepsister');
end;

function CNLabelContactRelationMotherInLawOrStepmother: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationMotherInLawOrStepmother');
end;

function CNLabelContactRelationFatherInLawOrStepfather: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationFatherInLawOrStepfather');
end;

function CNLabelContactRelationDaughterInLawOrStepdaughter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationDaughterInLawOrStepdaughter');
end;

function CNLabelContactRelationSonInLawOrStepson: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSonInLawOrStepson');
end;

function CNLabelContactRelationCousinOrSiblingsChild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationCousinOrSiblingsChild');
end;

function CNLabelContactRelationNieceOrCousin: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNieceOrCousin');
end;

function CNLabelContactRelationNephewOrCousin: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationNephewOrCousin');
end;

function CNLabelContactRelationGrandchildOrSiblingsChild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGrandchildOrSiblingsChild');
end;

function CNLabelContactRelationGreatGrandchildOrSiblingsGrandchild: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationGreatGrandchildOrSiblingsGrandchild');
end;

function CNLabelContactRelationDaughterInLawOrSisterInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationDaughterInLawOrSisterInLaw');
end;

function CNLabelContactRelationSonInLawOrBrotherInLaw: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNLabelContactRelationSonInLawOrBrotherInLaw');
end;

function CNSocialProfileURLStringKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileURLStringKey');
end;

function CNSocialProfileUsernameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileUsernameKey');
end;

function CNSocialProfileUserIdentifierKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileUserIdentifierKey');
end;

function CNSocialProfileServiceKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceKey');
end;

function CNSocialProfileServiceFacebook: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceFacebook');
end;

function CNSocialProfileServiceFlickr: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceFlickr');
end;

function CNSocialProfileServiceLinkedIn: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceLinkedIn');
end;

function CNSocialProfileServiceMySpace: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceMySpace');
end;

function CNSocialProfileServiceSinaWeibo: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceSinaWeibo');
end;

function CNSocialProfileServiceTencentWeibo: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceTencentWeibo');
end;

function CNSocialProfileServiceTwitter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceTwitter');
end;

function CNSocialProfileServiceYelp: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceYelp');
end;

function CNSocialProfileServiceGameCenter: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNSocialProfileServiceGameCenter');
end;

function CNInstantMessageAddressUsernameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageAddressUsernameKey');
end;

function CNInstantMessageAddressServiceKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageAddressServiceKey');
end;

function CNInstantMessageServiceAIM: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceAIM');
end;

function CNInstantMessageServiceFacebook: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceFacebook');
end;

function CNInstantMessageServiceGaduGadu: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceGaduGadu');
end;

function CNInstantMessageServiceGoogleTalk: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceGoogleTalk');
end;

function CNInstantMessageServiceICQ: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceICQ');
end;

function CNInstantMessageServiceJabber: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceJabber');
end;

function CNInstantMessageServiceMSN: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceMSN');
end;

function CNInstantMessageServiceQQ: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceQQ');
end;

function CNInstantMessageServiceSkype: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceSkype');
end;

function CNInstantMessageServiceYahoo: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNInstantMessageServiceYahoo');
end;

function CNContactPropertyNotFetchedExceptionName: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPropertyNotFetchedExceptionName');
end;

function CNContactIdentifierKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactIdentifierKey');
end;

function CNContactNamePrefixKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactNamePrefixKey');
end;

function CNContactGivenNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactGivenNameKey');
end;

function CNContactMiddleNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactMiddleNameKey');
end;

function CNContactFamilyNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactFamilyNameKey');
end;

function CNContactPreviousFamilyNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPreviousFamilyNameKey');
end;

function CNContactNameSuffixKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactNameSuffixKey');
end;

function CNContactNicknameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactNicknameKey');
end;

function CNContactOrganizationNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactOrganizationNameKey');
end;

function CNContactDepartmentNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactDepartmentNameKey');
end;

function CNContactJobTitleKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactJobTitleKey');
end;

function CNContactPhoneticGivenNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPhoneticGivenNameKey');
end;

function CNContactPhoneticMiddleNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPhoneticMiddleNameKey');
end;

function CNContactPhoneticFamilyNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPhoneticFamilyNameKey');
end;

function CNContactPhoneticOrganizationNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPhoneticOrganizationNameKey');
end;

function CNContactBirthdayKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactBirthdayKey');
end;

function CNContactNonGregorianBirthdayKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactNonGregorianBirthdayKey');
end;

function CNContactNoteKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactNoteKey');
end;

function CNContactImageDataKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactImageDataKey');
end;

function CNContactThumbnailImageDataKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactThumbnailImageDataKey');
end;

function CNContactImageDataAvailableKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactImageDataAvailableKey');
end;

function CNContactTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactTypeKey');
end;

function CNContactPhoneNumbersKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPhoneNumbersKey');
end;

function CNContactEmailAddressesKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactEmailAddressesKey');
end;

function CNContactPostalAddressesKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPostalAddressesKey');
end;

function CNContactDatesKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactDatesKey');
end;

function CNContactUrlAddressesKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactUrlAddressesKey');
end;

function CNContactRelationsKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactRelationsKey');
end;

function CNContactSocialProfilesKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactSocialProfilesKey');
end;

function CNContactInstantMessageAddressesKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactInstantMessageAddressesKey');
end;

function CNContactPropertyAttribute: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactPropertyAttribute');
end;

function CNContactStoreDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContactStoreDidChangeNotification');
end;

function CNContainerIdentifierKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContainerIdentifierKey');
end;

function CNContainerNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContainerNameKey');
end;

function CNContainerTypeKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNContainerTypeKey');
end;

function CNErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNErrorDomain');
end;

function CNErrorUserInfoAffectedRecordsKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNErrorUserInfoAffectedRecordsKey');
end;

function CNErrorUserInfoAffectedRecordIdentifiersKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNErrorUserInfoAffectedRecordIdentifiersKey');
end;

function CNErrorUserInfoValidationErrorsKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNErrorUserInfoValidationErrorsKey');
end;

function CNErrorUserInfoKeyPathsKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNErrorUserInfoKeyPathsKey');
end;

function CNGroupIdentifierKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNGroupIdentifierKey');
end;

function CNGroupNameKey: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNGroupNameKey');
end;

function CNPostalAddressPropertyAttribute: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressPropertyAttribute');
end;

function CNPostalAddressLocalizedPropertyNameAttribute: NSString;
begin
  Result := CocoaNSStringConst(libContacts, 'CNPostalAddressLocalizedPropertyNameAttribute');
end;

initialization
  ContactsModule := dlopen(MarshaledAString(libContacts), RTLD_LAZY);

finalization
  dlclose(ContactsModule)

end.