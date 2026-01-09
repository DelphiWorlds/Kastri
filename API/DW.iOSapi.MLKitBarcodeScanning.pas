unit DW.iOSapi.MLKitBarcodeScanning;

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
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.MLKitVision;

const
  MLKBarcodeFormatUnknown = 0;
  MLKBarcodeFormatAll = 65535;
  MLKBarcodeFormatCode128 = 1;
  MLKBarcodeFormatCode39 = 2;
  MLKBarcodeFormatCode93 = 4;
  MLKBarcodeFormatCodaBar = 8;
  MLKBarcodeFormatDataMatrix = 16;
  MLKBarcodeFormatEAN13 = 32;
  MLKBarcodeFormatEAN8 = 64;
  MLKBarcodeFormatITF = 128;
  MLKBarcodeFormatQRCode = 256;
  MLKBarcodeFormatUPCA = 512;
  MLKBarcodeFormatUPCE = 1024;
  MLKBarcodeFormatPDF417 = 2048;
  MLKBarcodeFormatAztec = 4096;

type
  MLKBarcodeScannerOptions = interface;
  MLKBarcodeAddress = interface;
  MLKBarcodeCalendarEvent = interface;
  MLKBarcodeDriverLicense = interface;
  MLKBarcodeEmail = interface;
  MLKBarcodeGeoPoint = interface;
  MLKBarcodePersonName = interface;
  MLKBarcodePhone = interface;
  MLKBarcodeSMS = interface;
  MLKBarcodeURLBookmark = interface;
  MLKBarcodeWiFi = interface;
  MLKBarcodeContactInfo = interface;
  MLKBarcode = interface;
  MLKBarcodeScanner = interface;

  MLKBarcodeFormat = NSInteger;
  MLKBarcodeValueType = NSInteger;
  MLKBarcodeAddressType = NSInteger;
  MLKBarcodeEmailType = NSInteger;
  MLKBarcodePhoneType = NSInteger;
  MLKBarcodeWiFiEncryptionType = NSInteger;

  MLKBarcodeScanningCallback = procedure(barcodes: NSArray; error: NSError) of object;

  MLKBarcodeScannerOptionsClass = interface(NSObjectClass)
    ['{17E21E0C-95BE-42F4-B427-0FD14413C7BA}']
  end;

  MLKBarcodeScannerOptions = interface(NSObject)
    ['{CC97412F-BFF7-4F22-94A4-1089EA52BA7F}']
    function formats: MLKBarcodeFormat; cdecl;
    function initWithFormats(formats: MLKBarcodeFormat): Pointer; cdecl;
  end;
  TMLKBarcodeScannerOptions = class(TOCGenericImport<MLKBarcodeScannerOptionsClass, MLKBarcodeScannerOptions>) end;

  MLKBarcodeAddressClass = interface(NSObjectClass)
    ['{867F8F14-CB86-4D2C-9412-A468287B9189}']
  end;

  MLKBarcodeAddress = interface(NSObject)
    ['{1E295593-CCF4-4DB5-B835-BCB82DBD167F}']
    [MethodName('type')]
    function &type: MLKBarcodeAddressType; cdecl;
    function addressLines: NSArray; cdecl;
  end;
  TMLKBarcodeAddress = class(TOCGenericImport<MLKBarcodeAddressClass, MLKBarcodeAddress>) end;

  MLKBarcodeCalendarEventClass = interface(NSObjectClass)
    ['{B832AA10-569E-4215-A801-C648732E6167}']
  end;

  MLKBarcodeCalendarEvent = interface(NSObject)
    ['{298CF6D2-953F-4829-AFD5-B7D6182FB436}']
    [MethodName('end')]
    function &end: NSDate; cdecl;
    function eventDescription: NSString; cdecl;
    function location: NSString; cdecl;
    function organizer: NSString; cdecl;
    function start: NSDate; cdecl;
    function status: NSString; cdecl;
    function summary: NSString; cdecl;
  end;
  TMLKBarcodeCalendarEvent = class(TOCGenericImport<MLKBarcodeCalendarEventClass, MLKBarcodeCalendarEvent>) end;

  MLKBarcodeDriverLicenseClass = interface(NSObjectClass)
    ['{EF2E0BCE-E011-4662-AC89-1F044460D079}']
  end;

  MLKBarcodeDriverLicense = interface(NSObject)
    ['{E3111463-0067-4575-A0DB-8B938B2B33ED}']
    function addressCity: NSString; cdecl;
    function addressState: NSString; cdecl;
    function addressStreet: NSString; cdecl;
    function addressZip: NSString; cdecl;
    function birthDate: NSString; cdecl;
    function documentType: NSString; cdecl;
    function expiryDate: NSString; cdecl;
    function firstName: NSString; cdecl;
    function gender: NSString; cdecl;
    function issuingCountry: NSString; cdecl;
    function issuingDate: NSString; cdecl;
    function lastName: NSString; cdecl;
    function licenseNumber: NSString; cdecl;
    function middleName: NSString; cdecl;
  end;
  TMLKBarcodeDriverLicense = class(TOCGenericImport<MLKBarcodeDriverLicenseClass, MLKBarcodeDriverLicense>) end;

  MLKBarcodeEmailClass = interface(NSObjectClass)
    ['{BC7A602F-6745-4F9E-91C6-86DF55330B82}']
  end;

  MLKBarcodeEmail = interface(NSObject)
    ['{7CBB27C6-8E5F-441D-BA10-C3FCE4ED836D}']
    [MethodName('type')]
    function &type: MLKBarcodeEmailType; cdecl;
    function address: NSString; cdecl;
    function body: NSString; cdecl;
    function subject: NSString; cdecl;
  end;
  TMLKBarcodeEmail = class(TOCGenericImport<MLKBarcodeEmailClass, MLKBarcodeEmail>) end;

  MLKBarcodeGeoPointClass = interface(NSObjectClass)
    ['{39F7970C-2095-43E1-A469-4F04A7390CD0}']
  end;

  MLKBarcodeGeoPoint = interface(NSObject)
    ['{EFFFF079-ACB5-4145-929C-3A1623254665}']
    function latitude: Double; cdecl;
    function longitude: Double; cdecl;
  end;
  TMLKBarcodeGeoPoint = class(TOCGenericImport<MLKBarcodeGeoPointClass, MLKBarcodeGeoPoint>) end;

  MLKBarcodePersonNameClass = interface(NSObjectClass)
    ['{D097AB3D-8F63-417D-92AF-B6FB931C0F0F}']
  end;

  MLKBarcodePersonName = interface(NSObject)
    ['{137B0CE2-FDC2-4C77-A6E2-2509988B9C94}']
    function first: NSString; cdecl;
    function formattedName: NSString; cdecl;
    function last: NSString; cdecl;
    function middle: NSString; cdecl;
    function prefix: NSString; cdecl;
    function pronunciation: NSString; cdecl;
    function suffix: NSString; cdecl;
  end;
  TMLKBarcodePersonName = class(TOCGenericImport<MLKBarcodePersonNameClass, MLKBarcodePersonName>) end;

  MLKBarcodePhoneClass = interface(NSObjectClass)
    ['{4F920911-B92C-407D-B37F-E0DCE861976C}']
  end;

  MLKBarcodePhone = interface(NSObject)
    ['{04D517EE-1C58-46E3-A60B-AF7FBEA70427}']
    [MethodName('type')]
    function &type: MLKBarcodePhoneType; cdecl;
    function number: NSString; cdecl;
  end;
  TMLKBarcodePhone = class(TOCGenericImport<MLKBarcodePhoneClass, MLKBarcodePhone>) end;

  MLKBarcodeSMSClass = interface(NSObjectClass)
    ['{73B92DEA-5235-4F78-AFB9-1F4394A262C0}']
  end;

  MLKBarcodeSMS = interface(NSObject)
    ['{FE3A9DEF-65B8-4AD4-A9F5-606E0D72E421}']
    function message: NSString; cdecl;
    function phoneNumber: NSString; cdecl;
  end;
  TMLKBarcodeSMS = class(TOCGenericImport<MLKBarcodeSMSClass, MLKBarcodeSMS>) end;

  MLKBarcodeURLBookmarkClass = interface(NSObjectClass)
    ['{FD50EB77-C5F4-4AA8-9103-633E52484163}']
  end;

  MLKBarcodeURLBookmark = interface(NSObject)
    ['{E90B7B8E-783E-4945-A910-FB67D8B32BA0}']
    function title: NSString; cdecl;
    function url: NSString; cdecl;
  end;
  TMLKBarcodeURLBookmark = class(TOCGenericImport<MLKBarcodeURLBookmarkClass, MLKBarcodeURLBookmark>) end;

  MLKBarcodeWiFiClass = interface(NSObjectClass)
    ['{F95DEBD8-4331-4893-8F49-780AE3B9E7CE}']
  end;

  MLKBarcodeWiFi = interface(NSObject)
    ['{8824E4A3-99B8-4161-AE3E-25B7DCC1EA86}']
    [MethodName('type')]
    function &type: MLKBarcodeWiFiEncryptionType; cdecl;
    function password: NSString; cdecl;
    function ssid: NSString; cdecl;
  end;
  TMLKBarcodeWiFi = class(TOCGenericImport<MLKBarcodeWiFiClass, MLKBarcodeWiFi>) end;

  MLKBarcodeContactInfoClass = interface(NSObjectClass)
    ['{F5253172-D48E-4DB2-8B22-88F5FA1C11C4}']
  end;

  MLKBarcodeContactInfo = interface(NSObject)
    ['{7234CDA7-B06C-4607-97F3-992EFBFA35AF}']
    function addresses: NSArray; cdecl;
    function emails: NSArray; cdecl;
    function jobTitle: NSString; cdecl;
    function name: MLKBarcodePersonName; cdecl;
    function organization: NSString; cdecl;
    function phones: NSArray; cdecl;
    function urls: NSArray; cdecl;
  end;
  TMLKBarcodeContactInfo = class(TOCGenericImport<MLKBarcodeContactInfoClass, MLKBarcodeContactInfo>) end;

  MLKBarcodeClass = interface(NSObjectClass)
    ['{4AF43572-7756-4FC1-8061-A2A7D09D98F3}']
  end;

  MLKBarcode = interface(NSObject)
    ['{1B4B90C8-AFA9-4759-8EFD-33839E4231EB}']
    function calendarEvent: MLKBarcodeCalendarEvent; cdecl;
    function contactInfo: MLKBarcodeContactInfo; cdecl;
    function cornerPoints: NSArray; cdecl;
    function displayValue: NSString; cdecl;
    function driverLicense: MLKBarcodeDriverLicense; cdecl;
    function email: MLKBarcodeEmail; cdecl;
    function format: MLKBarcodeFormat; cdecl;
    function frame: CGRect; cdecl;
    function geoPoint: MLKBarcodeGeoPoint; cdecl;
    function phone: MLKBarcodePhone; cdecl;
    function rawData: NSData; cdecl;
    function rawValue: NSString; cdecl;
    function sms: MLKBarcodeSMS; cdecl;
    function URL: MLKBarcodeURLBookmark; cdecl;
    function valueType: MLKBarcodeValueType; cdecl;
    function wifi: MLKBarcodeWiFi; cdecl;
  end;
  TMLKBarcode = class(TOCGenericImport<MLKBarcodeClass, MLKBarcode>) end;

  MLKBarcodeScannerClass = interface(NSObjectClass)
    ['{FCE1F8D8-89A9-4A03-8420-0996D9F4B6C3}']
    {class} function barcodeScanner: Pointer; cdecl;
    {class} function barcodeScannerWithOptions(options: MLKBarcodeScannerOptions): Pointer; cdecl;
  end;

  MLKBarcodeScanner = interface(NSObject)
    ['{14F4ABBA-29F0-42F7-8003-CB0C4EB37BE6}']
    procedure processImage(image: MLKVisionImage; completion: MLKBarcodeScanningCallback); cdecl;
    function resultsInImage(image: MLKVisionImage; error: PPointer): NSArray; cdecl;
  end;
  TMLKBarcodeScanner = class(TOCGenericImport<MLKBarcodeScannerClass, MLKBarcodeScanner>) end;

const
  libMLKitBarcodeScanning = 'MLKitBarcodeScanning';

implementation

uses
  // DW
  DW.iOSapi.MLKitCommon;

procedure MLKitBarcodeScanningLoader; cdecl; external framework libMLKitBarcodeScanning dependency 'c++';

end.