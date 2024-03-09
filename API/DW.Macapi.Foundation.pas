unit DW.Macapi.Foundation;

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
  Macapi.Foundation, Macapi.ObjectiveC, Macapi.CocoaTypes;

const
  NSJSONReadingMutableContainers = 1 shl 0;
  NSJSONReadingMutableLeaves = 1 shl 1;
  NSJSONReadingFragmentsAllowed = 1 shl 2;
  NSJSONReadingJSON5Allowed = 1 shl 3;
  NSJSONReadingTopLevelDictionaryAssumed = 1 shl 4;
  NSJSONReadingAllowFragments = NSJSONReadingFragmentsAllowed;
  NSJSONWritingPrettyPrinted = 1 shl 0;
  NSJSONWritingSortedKeys = 1 shl 1;
  NSJSONWritingFragmentsAllowed = 1 shl 2;
  NSJSONWritingWithoutEscapingSlashes = 1 shl 3;

type
  NSData = interface;
  NSJSONSerialization = interface;
  NSLocale = interface;
  NSNetService = interface;
  NSNetServiceBrowser = interface;
  NSNetServiceDelegate = interface;
  NSNetServiceBrowserDelegate = interface;
  NSPersonNameComponents = interface;
  NSProgress = interface;

  NSDataReadingOptions = NSInteger;
  NSDataWritingOptions = NSInteger;
  NSDataSearchOptions = NSInteger;
  NSDataBase64EncodingOptions = NSInteger;
  NSDataBase64DecodingOptions = NSInteger;
  NSDataCompressionAlgorithm = NSInteger;
  NSLocaleKey = NSString;
  NSLocaleLanguageDirection = NSInteger;
  PNSInputStream = ^NSInputStream;
  PNSOutputStream = ^NSOutputStream;
  NSRunLoopMode = NSString;
  NSJSONReadingOptions = NSInteger;
  NSJSONWritingOptions = NSInteger;
  NSProgressKind = NSString;
  NSProgressUserInfoKey = NSString;
  NSProgressFileOperationKind = NSString;
  NSQualityOfService = NSInteger;
  NSUserActivityPersistentIdentifier = NSString;
  PNSRange = ^NSRange;
  NSRangePointer = PNSRange;
  NSFileProtectionType = NSString;
  NSAttributedStringKey = NSString;
  NSKeyValueSetMutationKind = NSInteger;

  NSProgressUnpublishingHandler = procedure of object;
  NSProgressPublishingHandler = function(progress: NSProgress): NSProgressUnpublishingHandler of object;

  TNSProgressBlockMethod1 = procedure of object;
  TNSProgressBlockMethod2 = procedure of object;
  TNSProgressBlockMethod3 = procedure of object;
  TNSProgressBlockMethod4 = procedure of object;
  TNSDataBlockMethod1 = procedure(bytes: Pointer; byteRange: NSRange; stop: PBoolean) of object;
  TNSDataBlockMethod2 = procedure(bytes: Pointer; length: NSUInteger) of object;
  TNSUserActivityBlockMethod1 = procedure(inputStream: NSInputStream; outputStream: NSOutputStream; error: NSError) of object;
  TNSUserActivityBlockMethod2 = procedure of object;

  NSDataClass = interface(NSObjectClass)
    ['{D4B36676-DE03-44A5-B04E-D4CC6BB4E9B2}']
    {class} function data: Pointer; cdecl;
    [MethodName('dataWithBytes:length:')]
    {class} function dataWithBytes(bytes: Pointer; length: NSUInteger): Pointer; cdecl;
    [MethodName('dataWithBytesNoCopy:length:freeWhenDone:')]
    {class} function dataWithBytesNoCopy(bytes: Pointer; length: NSUInteger; b: Boolean): Pointer; overload; cdecl;
    [MethodName('dataWithBytesNoCopy:length:')]
    {class} function dataWithBytesNoCopy(bytes: Pointer; length: NSUInteger): Pointer; overload; cdecl;
    {class} function dataWithContentsOfFile(path: NSString): Pointer; overload; cdecl;
    [MethodName('dataWithContentsOfFile:options:error:')]
    {class} function dataWithContentsOfFile(path: NSString; readOptionsMask: NSDataReadingOptions; errorPtr: PPointer): Pointer; overload; cdecl;
    {class} function dataWithContentsOfMappedFile(path: NSString): Pointer; cdecl; // API_DEPRECATED("Use +dataWithContentsOfURL:options:error: and NSDataReadingMappedIfSafe or NSDataReadingMappedAlways instead.", macos(10.0,10.10), ios(2.0,8.0), watchos(2.0,2.0), tvos(9.0,9.0))
    {class} function dataWithContentsOfURL(url: NSURL): Pointer; overload; cdecl;
    [MethodName('dataWithContentsOfURL:options:error:')]
    {class} function dataWithContentsOfURL(url: NSURL; readOptionsMask: NSDataReadingOptions; errorPtr: PPointer): Pointer; overload; cdecl;
    {class} function dataWithData(data: NSData): Pointer; cdecl;
  end;

  NSData = interface(NSObject)
    ['{57E65112-1521-466E-A55A-1A0C26ECA74D}']
    function base64EncodedDataWithOptions(options: NSDataBase64EncodingOptions): NSData; cdecl;
    function base64EncodedStringWithOptions(options: NSDataBase64EncodingOptions): NSString; cdecl;
    function base64Encoding: NSString; cdecl; // API_DEPRECATED("Use base64EncodedStringWithOptions instead", macos(10.6,10.9), ios(4.0,7.0), watchos(2.0,2.0), tvos(9.0,9.0))
    function bytes: Pointer; cdecl;
    [MethodName('compressedDataUsingAlgorithm:error:')]
    function compressedDataUsingAlgorithm(algorithm: NSDataCompressionAlgorithm; error: PPointer): Pointer; cdecl;
    [MethodName('decompressedDataUsingAlgorithm:error:')]
    function decompressedDataUsingAlgorithm(algorithm: NSDataCompressionAlgorithm; error: PPointer): Pointer; cdecl;
    function description: NSString; cdecl;
    procedure enumerateByteRangesUsingBlock(block: TNSDataBlockMethod1); cdecl;
    [MethodName('getBytes:length:')]
    procedure getBytes(buffer: Pointer; length: NSUInteger); overload; cdecl;
    procedure getBytes(buffer: Pointer); overload; cdecl; // API_DEPRECATED("This method is unsafe because it could potentially cause buffer overruns. Use -getBytes:length: instead.", macos(10.0,10.10), ios(2.0,8.0), watchos(2.0,2.0), tvos(9.0,9.0))
    [MethodName('getBytes:range:')]
    procedure getBytes(buffer: Pointer; range: NSRange); overload; cdecl;
    [MethodName('initWithBase64EncodedData:options:')]
    function initWithBase64EncodedData(base64Data: NSData; options: NSDataBase64DecodingOptions): Pointer; cdecl;
    [MethodName('initWithBase64EncodedString:options:')]
    function initWithBase64EncodedString(base64String: NSString; options: NSDataBase64DecodingOptions): Pointer; cdecl;
    function initWithBase64Encoding(base64String: NSString): Pointer; cdecl; // API_DEPRECATED("Use initWithBase64EncodedString instead", macos(10.6,10.9), ios(4.0,7.0), watchos(2.0,2.0), tvos(9.0,9.0))
    [MethodName('initWithBytes:length:')]
    function initWithBytes(bytes: Pointer; length: NSUInteger): Pointer; cdecl;
    [MethodName('initWithBytesNoCopy:length:deallocator:')]
    function initWithBytesNoCopy(bytes: Pointer; length: NSUInteger; deallocator: TNSDataBlockMethod2): Pointer; overload; cdecl;
    [MethodName('initWithBytesNoCopy:length:freeWhenDone:')]
    function initWithBytesNoCopy(bytes: Pointer; length: NSUInteger; b: Boolean): Pointer; overload; cdecl;
    [MethodName('initWithBytesNoCopy:length:')]
    function initWithBytesNoCopy(bytes: Pointer; length: NSUInteger): Pointer; overload; cdecl;
    function initWithContentsOfFile(path: NSString): Pointer; overload; cdecl;
    [MethodName('initWithContentsOfFile:options:error:')]
    function initWithContentsOfFile(path: NSString; readOptionsMask: NSDataReadingOptions; errorPtr: PPointer): Pointer; overload; cdecl;
    function initWithContentsOfMappedFile(path: NSString): Pointer; cdecl; // API_DEPRECATED("Use -initWithContentsOfURL:options:error: and NSDataReadingMappedIfSafe or NSDataReadingMappedAlways instead.", macos(10.0,10.10), ios(2.0,8.0), watchos(2.0,2.0), tvos(9.0,9.0))
    [MethodName('initWithContentsOfURL:options:error:')]
    function initWithContentsOfURL(url: NSURL; readOptionsMask: NSDataReadingOptions; errorPtr: PPointer): Pointer; overload; cdecl;
    function initWithContentsOfURL(url: NSURL): Pointer; overload; cdecl;
    function initWithData(data: NSData): Pointer; cdecl;
    function isEqualToData(other: NSData): Boolean; cdecl;
    function length: NSUInteger; cdecl;
    [MethodName('rangeOfData:options:range:')]
    function rangeOfData(dataToFind: NSData; mask: NSDataSearchOptions; searchRange: NSRange): NSRange; cdecl;
    function subdataWithRange(range: NSRange): NSData; cdecl;
    [MethodName('writeToFile:options:error:')]
    function writeToFile(path: NSString; writeOptionsMask: NSDataWritingOptions; errorPtr: PPointer): Boolean; overload; cdecl;
    [MethodName('writeToFile:atomically:')]
    function writeToFile(path: NSString; useAuxiliaryFile: Boolean): Boolean; overload; cdecl;
    [MethodName('writeToURL:atomically:')]
    function writeToURL(url: NSURL; atomically: Boolean): Boolean; overload; cdecl;
    [MethodName('writeToURL:options:error:')]
    function writeToURL(url: NSURL; writeOptionsMask: NSDataWritingOptions; errorPtr: PPointer): Boolean; overload; cdecl;
  end;
  TNSData = class(TOCGenericImport<NSDataClass, NSData>) end;

  NSLocaleClass = interface(NSObjectClass)
    ['{4597A459-6F9B-49F4-8C80-3F8ED8FDB9D1}']
    {class} function autoupdatingCurrentLocale: NSLocale; cdecl;
    {class} function availableLocaleIdentifiers: NSArray; cdecl;
    {class} function canonicalLanguageIdentifierFromString(&string: NSString): NSString; cdecl;
    {class} function canonicalLocaleIdentifierFromString(&string: NSString): NSString; cdecl;
    {class} function characterDirectionForLanguage(isoLangCode: NSString): NSLocaleLanguageDirection; cdecl;
    {class} function commonISOCurrencyCodes: NSArray; cdecl;
    {class} function componentsFromLocaleIdentifier(&string: NSString): NSDictionary; cdecl;
    {class} function currentLocale: Pointer; cdecl;
    {class} function ISOCountryCodes: NSArray; cdecl;
    {class} function ISOCurrencyCodes: NSArray; cdecl;
    {class} function ISOLanguageCodes: NSArray; cdecl;
    {class} function lineDirectionForLanguage(isoLangCode: NSString): NSLocaleLanguageDirection; cdecl;
    {class} function localeIdentifierFromComponents(dict: NSDictionary): NSString; cdecl;
    {class} function localeIdentifierFromWindowsLocaleCode(lcid: UInt32): NSString; cdecl;
    {class} function localeWithLocaleIdentifier(ident: NSString): Pointer; cdecl;
    {class} function preferredLanguages: NSArray; cdecl;
    {class} function systemLocale: NSLocale; cdecl;
    {class} function windowsLocaleCodeFromLocaleIdentifier(localeIdentifier: NSString): UInt32; cdecl;
  end;

  NSLocale = interface(NSObject)
    ['{6D772B7D-BE16-4960-A10B-D5BCEE8D3B94}']
    function alternateQuotationBeginDelimiter: NSString; cdecl;
    function alternateQuotationEndDelimiter: NSString; cdecl;
    function calendarIdentifier: NSString; cdecl;
    function collationIdentifier: NSString; cdecl;
    function collatorIdentifier: NSString; cdecl;
    function countryCode: NSString; cdecl;
    function currencyCode: NSString; cdecl;
    function currencySymbol: NSString; cdecl;
    function decimalSeparator: NSString; cdecl;
    [MethodName('displayNameForKey:value:')]
    function displayNameForKey(key: NSLocaleKey; value: Pointer): NSString; cdecl;
    function exemplarCharacterSet: NSCharacterSet; cdecl;
    function groupingSeparator: NSString; cdecl;
    function init: Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithLocaleIdentifier(&string: NSString): Pointer; cdecl;
    function languageCode: NSString; cdecl;
    function localeIdentifier: NSString; cdecl;
    function localizedStringForCalendarIdentifier(calendarIdentifier: NSString): NSString; cdecl;
    function localizedStringForCollationIdentifier(collationIdentifier: NSString): NSString; cdecl;
    function localizedStringForCollatorIdentifier(collatorIdentifier: NSString): NSString; cdecl;
    function localizedStringForCountryCode(countryCode: NSString): NSString; cdecl;
    function localizedStringForCurrencyCode(currencyCode: NSString): NSString; cdecl;
    function localizedStringForLanguageCode(languageCode: NSString): NSString; cdecl;
    function localizedStringForLocaleIdentifier(localeIdentifier: NSString): NSString; cdecl;
    function localizedStringForScriptCode(scriptCode: NSString): NSString; cdecl;
    function localizedStringForVariantCode(variantCode: NSString): NSString; cdecl;
    function objectForKey(key: NSLocaleKey): Pointer; cdecl;
    function quotationBeginDelimiter: NSString; cdecl;
    function quotationEndDelimiter: NSString; cdecl;
    function scriptCode: NSString; cdecl;
    function usesMetricSystem: Boolean; cdecl;
    function variantCode: NSString; cdecl;
  end;
  TNSLocale = class(TOCGenericImport<NSLocaleClass, NSLocale>) end;

  NSNetServiceClass = interface(NSObjectClass)
    ['{3E78C456-0906-4BDB-98A3-946ACF2A36B7}']
    {class} function dataFromTXTRecordDictionary(txtDictionary: NSDictionary): NSData; cdecl;
    {class} function dictionaryFromTXTRecordData(txtData: NSData): NSDictionary; cdecl;
  end;

  NSNetService = interface(NSObject)
    ['{64E3A9B9-03F2-4E69-8586-FF6AAB14E94E}']
    function &type: NSString; cdecl;
    function addresses: NSArray; cdecl;
    function delegate: Pointer; cdecl;
    function domain: NSString; cdecl;
    function getInputStream(inputStream: PNSInputStream; outputStream: PNSOutputStream): Boolean; cdecl;
    function hostName: NSString; cdecl;
    function includesPeerToPeer: Boolean; cdecl;
    function initWithDomain(domain: NSString; &type: NSString; name: NSString): Pointer; overload; cdecl;
    function initWithDomain(domain: NSString; &type: NSString; name: NSString; port: Integer): Pointer; overload; cdecl;
    function name: NSString; cdecl;
    function port: NSInteger; cdecl;
    procedure publish; cdecl;
    procedure publishWithOptions(options: NSNetServiceOptions); cdecl;
    procedure removeFromRunLoop(aRunLoop: NSRunLoop; forMode: NSRunLoopMode); cdecl;
    procedure resolveWithTimeout(timeout: NSTimeInterval); cdecl;
    procedure scheduleInRunLoop(aRunLoop: NSRunLoop; forMode: NSRunLoopMode); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setIncludesPeerToPeer(includesPeerToPeer: Boolean); cdecl;
    function setTXTRecordData(recordData: NSData): Boolean; cdecl;
    procedure startMonitoring; cdecl;
    procedure stop; cdecl;
    procedure stopMonitoring; cdecl;
    function TXTRecordData: NSData; cdecl;
  end;
  TNSNetService = class(TOCGenericImport<NSNetServiceClass, NSNetService>) end;

  NSNetServiceBrowserClass = interface(NSObjectClass)
    ['{3DF1D675-454A-4382-8E1F-33386913CC39}']
  end;

  NSNetServiceBrowser = interface(NSObject)
    ['{37E423C2-4941-486A-B0B1-13DBAF19E086}']
    function delegate: Pointer; cdecl;
    function includesPeerToPeer: Boolean; cdecl;
    procedure removeFromRunLoop(aRunLoop: NSRunLoop; forMode: NSRunLoopMode); cdecl;
    procedure scheduleInRunLoop(aRunLoop: NSRunLoop; forMode: NSRunLoopMode); cdecl;
    procedure searchForBrowsableDomains; cdecl;
    procedure searchForRegistrationDomains; cdecl;
    procedure searchForServicesOfType(&type: NSString; inDomain: NSString); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setIncludesPeerToPeer(includesPeerToPeer: Boolean); cdecl;
    procedure stop; cdecl;
  end;
  TNSNetServiceBrowser = class(TOCGenericImport<NSNetServiceBrowserClass, NSNetServiceBrowser>) end;

  NSNetServiceDelegate = interface(IObjectiveC)
    ['{00728682-DC04-47C4-9497-43D2BBA88F2F}']
    procedure netServiceDidAcceptConnectionWithInputStream(sender: NSNetService; didAcceptConnectionWithInputStream: NSInputStream;
      outputStream: NSOutputStream); cdecl;
    procedure netServiceDidNotPublish(sender: NSNetService; didNotPublish: NSDictionary); cdecl;
    procedure netServiceDidNotResolve(sender: NSNetService; didNotResolve: NSDictionary); cdecl;
    procedure netServiceDidPublish(sender: NSNetService); cdecl;
    procedure netServiceDidResolveAddress(sender: NSNetService); cdecl;
    procedure netServiceDidStop(sender: NSNetService); cdecl;
    procedure netServiceDidUpdateTXTRecordData(sender: NSNetService; didUpdateTXTRecordData: NSData); cdecl;
    procedure netServiceWillPublish(sender: NSNetService); cdecl;
    procedure netServiceWillResolve(sender: NSNetService); cdecl;
  end;

  NSNetServiceBrowserDelegate = interface(IObjectiveC)
    ['{CAB8636E-EDE2-4456-91AD-657247F3EB5A}']
    procedure netServiceBrowserDidFindDomain(browser: NSNetServiceBrowser; didFindDomain: NSString; moreComing: Boolean); cdecl;
    procedure netServiceBrowserDidFindService(browser: NSNetServiceBrowser; didFindService: NSNetService; moreComing: Boolean); cdecl;
    procedure netServiceBrowserDidNotSearch(browser: NSNetServiceBrowser; didNotSearch: NSDictionary); cdecl;
    procedure netServiceBrowserDidRemoveDomain(browser: NSNetServiceBrowser; didRemoveDomain: NSString; moreComing: Boolean); cdecl;
    procedure netServiceBrowserDidRemoveService(browser: NSNetServiceBrowser; didRemoveService: NSNetService; moreComing: Boolean); cdecl;
    procedure netServiceBrowserDidStopSearch(browser: NSNetServiceBrowser); cdecl;
    procedure netServiceBrowserWillSearch(browser: NSNetServiceBrowser); cdecl;
  end;

  NSJSONSerializationClass = interface(NSObjectClass)
    ['{5181AA58-9005-4CD1-B264-AA9DCFB00DF5}']
    {class} function dataWithJSONObject(obj: Pointer; options: NSJSONWritingOptions; error: PPointer): Macapi.Foundation.NSData; cdecl;
    {class} function isValidJSONObject(obj: Pointer): Boolean; cdecl;
    {class} function JSONObjectWithData(data: Macapi.Foundation.NSData; options: NSJSONReadingOptions; error: PPointer): Pointer; cdecl;
    {class} function JSONObjectWithStream(stream: NSInputStream; options: NSJSONReadingOptions; error: PPointer): Pointer; cdecl;
    {class} function writeJSONObject(obj: Pointer; toStream: NSOutputStream; options: NSJSONWritingOptions; error: PPointer): NSInteger; cdecl;
  end;

  NSJSONSerialization = interface(NSObject)
    ['{A65615A3-5578-4D4A-B7F3-17CDF5201312}']
  end;
  TNSJSONSerialization = class(TOCGenericImport<NSJSONSerializationClass, NSJSONSerialization>) end;

  NSProgressClass = interface(NSObjectClass)
    ['{6EBC0156-DD11-4365-A02A-7F4786D92263}']
    {class} function addSubscriberForFileURL(url: NSURL; withPublishingHandler: NSProgressPublishingHandler): Pointer; cdecl;
    {class} function currentProgress: NSProgress; cdecl;
    {class} function discreteProgressWithTotalUnitCount(unitCount: Int64): NSProgress; cdecl;
    {class} function progressWithTotalUnitCount(unitCount: Int64): NSProgress; overload; cdecl;
    {class} function progressWithTotalUnitCount(unitCount: Int64; parent: NSProgress; pendingUnitCount: Int64): NSProgress; overload; cdecl;
    {class} procedure removeSubscriber(subscriber: Pointer); cdecl;
  end;

  NSProgress = interface(NSObject)
    ['{36D2AACC-6F67-4189-AB09-F75329A10F1B}']
    procedure addChild(child: NSProgress; withPendingUnitCount: Int64); cdecl;
    procedure becomeCurrentWithPendingUnitCount(unitCount: Int64); cdecl;
    procedure cancel; cdecl;
    function cancellationHandler: TNSProgressBlockMethod1; cdecl;
    function completedUnitCount: Int64; cdecl;
    function estimatedTimeRemaining: NSNumber; cdecl;
    function fileCompletedCount: NSNumber; cdecl;
    function fileOperationKind: NSProgressFileOperationKind; cdecl;
    function fileTotalCount: NSNumber; cdecl;
    function fileURL: NSURL; cdecl;
    function fractionCompleted: Double; cdecl;
    function initWithParent(parentProgressOrNil: NSProgress; userInfo: NSDictionary): Pointer; cdecl;
    function isCancellable: Boolean; cdecl;
    function isCancelled: Boolean; cdecl;
    function isFinished: Boolean; cdecl;
    function isIndeterminate: Boolean; cdecl;
    function isOld: Boolean; cdecl;
    function isPausable: Boolean; cdecl;
    function isPaused: Boolean; cdecl;
    function kind: NSProgressKind; cdecl;
    function localizedAdditionalDescription: NSString; cdecl;
    function localizedDescription: NSString; cdecl;
    procedure pause; cdecl;
    function pausingHandler: TNSProgressBlockMethod1; cdecl;
    procedure performAsCurrentWithPendingUnitCount(unitCount: Int64; usingBlock: TNSProgressBlockMethod1); cdecl;
    procedure publish; cdecl;
    procedure resignCurrent; cdecl;
    procedure resume; cdecl;
    function resumingHandler: TNSProgressBlockMethod1; cdecl;
    procedure setCancellable(cancellable: Boolean); cdecl;
    procedure setCancellationHandler(cancellationHandler: TNSProgressBlockMethod1); cdecl;
    procedure setCompletedUnitCount(completedUnitCount: Int64); cdecl;
    procedure setEstimatedTimeRemaining(estimatedTimeRemaining: NSNumber); cdecl;
    procedure setFileCompletedCount(fileCompletedCount: NSNumber); cdecl;
    procedure setFileOperationKind(fileOperationKind: NSProgressFileOperationKind); cdecl;
    procedure setFileTotalCount(fileTotalCount: NSNumber); cdecl;
    procedure setFileURL(fileURL: NSURL); cdecl;
    procedure setKind(kind: NSProgressKind); cdecl;
    procedure setLocalizedAdditionalDescription(localizedAdditionalDescription: NSString); cdecl;
    procedure setLocalizedDescription(localizedDescription: NSString); cdecl;
    procedure setPausable(pausable: Boolean); cdecl;
    procedure setPausingHandler(pausingHandler: TNSProgressBlockMethod1); cdecl;
    procedure setResumingHandler(resumingHandler: TNSProgressBlockMethod1); cdecl;
    procedure setThroughput(throughput: NSNumber); cdecl;
    procedure setTotalUnitCount(totalUnitCount: Int64); cdecl;
    procedure setUserInfoObject(objectOrNil: Pointer; forKey: NSProgressUserInfoKey); cdecl;
    function throughput: NSNumber; cdecl;
    function totalUnitCount: Int64; cdecl;
    procedure unpublish; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TNSProgress = class(TOCGenericImport<NSProgressClass, NSProgress>) end;

  NSPersonNameComponentsClass = interface(NSObjectClass)
    ['{181A864A-3BAC-47AD-8EA5-3A9860F063D0}']
  end;

  NSPersonNameComponents = interface(NSObject)
    ['{B7FEB33B-30A3-4492-9599-FEFA3C20CB90}']
    function familyName: NSString; cdecl;
    function givenName: NSString; cdecl;
    function middleName: NSString; cdecl;
    function namePrefix: NSString; cdecl;
    function nameSuffix: NSString; cdecl;
    function nickname: NSString; cdecl;
    function phoneticRepresentation: NSPersonNameComponents; cdecl;
    procedure setFamilyName(familyName: NSString); cdecl;
    procedure setGivenName(givenName: NSString); cdecl;
    procedure setMiddleName(middleName: NSString); cdecl;
    procedure setNamePrefix(namePrefix: NSString); cdecl;
    procedure setNameSuffix(nameSuffix: NSString); cdecl;
    procedure setNickname(nickname: NSString); cdecl;
    procedure setPhoneticRepresentation(phoneticRepresentation: NSPersonNameComponents); cdecl;
  end;
  TNSPersonNameComponents = class(TOCGenericImport<NSPersonNameComponentsClass, NSPersonNameComponents>) end;

  NSUserActivityClass = interface(NSObjectClass)
    ['{412EAEBF-5927-4D01-B83F-69D3B5DFE7B5}']
    {class} procedure deleteAllSavedUserActivitiesWithCompletionHandler(handler: TNSUserActivityBlockMethod2); cdecl;
    [MethodName('deleteSavedUserActivitiesWithPersistentIdentifiers:completionHandler:')]
    {class} procedure deleteSavedUserActivitiesWithPersistentIdentifiers(persistentIdentifiers: NSArray; handler: TNSUserActivityBlockMethod2); cdecl;
  end;

  NSUserActivity = interface(NSObject)
    ['{B8C2F6C9-31FE-4282-B7CA-98C96E163033}']
    function activityType: NSString; cdecl;
    procedure addUserInfoEntriesFromDictionary(otherDictionary: NSDictionary); cdecl;
    procedure becomeCurrent; cdecl;
    function delegate: Pointer; cdecl;
    function expirationDate: NSDate; cdecl;
    procedure getContinuationStreamsWithCompletionHandler(completionHandler: TNSUserActivityBlockMethod1); cdecl;
    function initWithActivityType(activityType: NSString): Pointer; cdecl;
    procedure invalidate; cdecl;
    function isEligibleForHandoff: Boolean; cdecl;
    function isEligibleForPrediction: Boolean; cdecl;
    function isEligibleForPublicIndexing: Boolean; cdecl;
    function isEligibleForSearch: Boolean; cdecl;
    function keywords: NSSet; cdecl;
    function needsSave: Boolean; cdecl;
    function persistentIdentifier: NSUserActivityPersistentIdentifier; cdecl;
    function referrerURL: NSURL; cdecl;
    function requiredUserInfoKeys: NSSet; cdecl;
    procedure resignCurrent; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setEligibleForHandoff(eligibleForHandoff: Boolean); cdecl;
    procedure setEligibleForPrediction(eligibleForPrediction: Boolean); cdecl;
    procedure setEligibleForPublicIndexing(eligibleForPublicIndexing: Boolean); cdecl;
    procedure setEligibleForSearch(eligibleForSearch: Boolean); cdecl;
    procedure setExpirationDate(expirationDate: NSDate); cdecl;
    procedure setKeywords(keywords: NSSet); cdecl;
    procedure setNeedsSave(needsSave: Boolean); cdecl;
    procedure setPersistentIdentifier(persistentIdentifier: NSUserActivityPersistentIdentifier); cdecl;
    procedure setReferrerURL(referrerURL: NSURL); cdecl;
    procedure setRequiredUserInfoKeys(requiredUserInfoKeys: NSSet); cdecl;
    procedure setSupportsContinuationStreams(supportsContinuationStreams: Boolean); cdecl;
    procedure setTargetContentIdentifier(targetContentIdentifier: NSString); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    procedure setWebpageURL(webpageURL: NSURL); cdecl;
    function supportsContinuationStreams: Boolean; cdecl;
    function targetContentIdentifier: NSString; cdecl;
    function title: NSString; cdecl;
    function userInfo: NSDictionary; cdecl;
    function webpageURL: NSURL; cdecl;
  end;
  TNSUserActivity = class(TOCGenericImport<NSUserActivityClass, NSUserActivity>) end;

implementation

end.
