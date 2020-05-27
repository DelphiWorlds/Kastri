unit DW.Macapi.Foundation;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // macOS
  Macapi.Foundation, Macapi.ObjectiveC, Macapi.CocoaTypes;

type
  NSData = interface;

  NSDataReadingOptions = NSInteger;
  NSDataWritingOptions = NSInteger;
  NSDataSearchOptions = NSInteger;
  NSDataBase64EncodingOptions = NSInteger;
  NSDataBase64DecodingOptions = NSInteger;
  NSDataCompressionAlgorithm = NSInteger;
  NSLocaleKey = NSString;
  NSLocaleLanguageDirection = NSInteger;

  TNSDataBlockMethod1 = procedure(bytes: Pointer; byteRange: NSRange; stop: PBoolean) of object;
  TNSDataBlockMethod2 = procedure(bytes: Pointer; length: NSUInteger) of object;

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

implementation

end.
