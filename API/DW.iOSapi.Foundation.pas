unit DW.iOSapi.Foundation;

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
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes;

const
  NSLocaleLanguageDirectionUnknown = 0;
  NSLocaleLanguageDirectionLeftToRight = 1;
  NSLocaleLanguageDirectionRightToLeft = 2;
  NSLocaleLanguageDirectionTopToBottom = 3;
  NSLocaleLanguageDirectionBottomToTop = 4;
  NSPersonNameComponentsFormatterStyleDefault = 0;
  NSPersonNameComponentsFormatterStyleShort = 1;
  NSPersonNameComponentsFormatterStyleMedium = 2;
  NSPersonNameComponentsFormatterStyleLong = 3;
  NSPersonNameComponentsFormatterStyleAbbreviated = 4;
  NSPersonNameComponentsFormatterPhonetic = 2;

type
  NSLocale = interface;
  NSMeasurement = interface;
  NSPersonNameComponents = interface;
  NSProgress = interface;
  NSUnit = interface;
  NSUserActivity = interface;

  NSErrorDomain = NSString;
  NSErrorUserInfoKey = NSString;
  NSLocaleKey = NSString;
  NSNotificationName = NSString;
  NSLocaleLanguageDirection = NSInteger;
  NSUserActivityPersistentIdentifier = NSString;
  NSProgressKind = NSString;
  NSProgressUserInfoKey = NSString;
  NSProgressFileOperationKind = NSString;
  NSItemProviderRepresentationVisibility = NSInteger;
  NSItemProviderFileOptions = NSInteger;

  NSProgressUnpublishingHandler = procedure of object;
  NSProgressPublishingHandler = function(progress: NSProgress): NSProgressUnpublishingHandler of object;

  TNSProgressBlockMethod1 = procedure of object;
  TNSProgressBlockMethod2 = procedure of object;
  TNSProgressBlockMethod3 = procedure of object;
  TNSProgressBlockMethod4 = procedure of object;
  TNSUserActivityBlockMethod1 = procedure(inputStream: NSInputStream; outputStream: NSOutputStream; error: NSError) of object;
  TNSUserActivityBlockMethod2 = procedure of object;

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

  NSPersonNameComponentsClass = interface(NSObjectClass)
    ['{C29B6599-AB14-44B4-9FE2-639F74583F65}']
  end;

  NSPersonNameComponents = interface(NSObject)
    ['{3BCD57D6-3B07-4328-8CF5-2BCF5EC8B026}']
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

  NSProgressClass = interface(NSObjectClass)
    ['{3C858ECE-7543-4485-90A3-EFAEB626797D}']
    [MethodName('addSubscriberForFileURL:withPublishingHandler:')]
    {class} function addSubscriberForFileURL(url: NSURL; publishingHandler: NSProgressPublishingHandler): Pointer; cdecl;
    {class} function currentProgress: NSProgress; cdecl;
    {class} function discreteProgressWithTotalUnitCount(unitCount: Int64): NSProgress; cdecl;
    {class} function progressWithTotalUnitCount(unitCount: Int64): NSProgress; overload; cdecl;
    [MethodName('progressWithTotalUnitCount:parent:pendingUnitCount:')]
    {class} function progressWithTotalUnitCount(unitCount: Int64; parent: NSProgress; portionOfParentTotalUnitCount: Int64): NSProgress; overload; cdecl;
    {class} procedure removeSubscriber(subscriber: Pointer); cdecl;
  end;

  NSProgress = interface(NSObject)
    ['{5A99845A-FED5-419C-B87F-9F7A6A215A71}']
    [MethodName('addChild:withPendingUnitCount:')]
    procedure addChild(child: NSProgress; inUnitCount: Int64); cdecl;
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
    [MethodName('initWithParent:userInfo:')]
    function initWithParent(parentProgressOrNil: NSProgress; userInfoOrNil: NSDictionary): Pointer; cdecl;
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
    [MethodName('performAsCurrentWithPendingUnitCount:usingBlock:')]
    procedure performAsCurrentWithPendingUnitCount(unitCount: Int64; work: TNSProgressBlockMethod1); cdecl;
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
    [MethodName('setUserInfoObject:forKey:')]
    procedure setUserInfoObject(objectOrNil: Pointer; key: NSProgressUserInfoKey); cdecl;
    function throughput: NSNumber; cdecl;
    function totalUnitCount: Int64; cdecl;
    procedure unpublish; cdecl;
    function userInfo: NSDictionary; cdecl;
  end;
  TNSProgress = class(TOCGenericImport<NSProgressClass, NSProgress>) end;

  NSProgressReporting = interface(IObjectiveC)
    ['{4C4F3C4A-B12D-42D9-B92C-936AE2122120}']
    function progress: NSProgress; cdecl;
  end;

  NSUnitClass = interface(NSObjectClass)
    ['{E58B92C1-F0D4-496B-8C43-397E478C10AA}']
    {class} function new: Pointer; cdecl;
  end;

  NSUnit = interface(NSObject)
    ['{4FAA83A3-6D6B-4BF9-BFD6-9EE2C0F02382}']
    function initWithSymbol(symbol: NSString): Pointer; cdecl;
    function symbol: NSString; cdecl;
  end;
  TNSUnit = class(TOCGenericImport<NSUnitClass, NSUnit>) end;

  NSMeasurementClass = interface(NSObjectClass)
    ['{5992431E-64D1-4813-B918-D656927DC518}']
  end;

  NSMeasurement = interface(NSObject)
    ['{AA2F9B58-7C87-4632-9A4B-8865D26F8B83}']
    [MethodName('unit')]
    function &unit: Pointer; cdecl;  // UnitType
    function canBeConvertedToUnit(&unit: NSUnit): Boolean; cdecl;
    function doubleValue: Double; cdecl;
    function initWithDoubleValue(doubleValue: Double; &unit: Pointer): Pointer; cdecl; // &unit: UnitType
    function measurementByAddingMeasurement(measurement: NSMeasurement): NSMeasurement; cdecl;
    function measurementByConvertingToUnit(&unit: NSUnit): NSMeasurement; cdecl;
    function measurementBySubtractingMeasurement(measurement: NSMeasurement): NSMeasurement; cdecl;
  end;
  TNSMeasurement = class(TOCGenericImport<NSMeasurementClass, NSMeasurement>) end;

function NSUserActivityTypeBrowsingWeb: NSString;

implementation

const
  libFoundation = '/System/Library/Frameworks/Foundation.framework/Foundation';

function NSUserActivityTypeBrowsingWeb: NSString;
begin
  Result := CocoaNSStringConst(libFoundation, 'NSUserActivityTypeBrowsingWeb');
end;

end.
