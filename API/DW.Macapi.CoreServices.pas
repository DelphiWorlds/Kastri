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


// NOTE: This is not a full import of the CoreServices framework (which is quite extensive)
// Please file an issue at:
//   https://github.com/DelphiWorlds/Kastri/issues
// if you want anything added here.

unit DW.Macapi.CoreServices;

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Foundation, Macapi.CocoaTypes,
  // Posix
  Posix.SysTypes;

const
  kAutoGenerateReturnID = -1;
  kAnyTransactionID = 0;

  kAEDataArray = 0;
  kAEPackedArray = 1;
  kAEDescArray = 3;
  kAEKeyDescArray = 4;
  kAEHandleArray = 2;
  kAENormalPriority = 0;
  kAEHighPriority = 1;
  kAENoReply = 1;
  kAEQueueReply = 2;
  kAEWaitReply = 3;
  kAEDontReconnect = 128;
  kAEWantReceipt = 512;
  kAENeverInteract = 16;
  kAECanInteract = 32;
  kAEAlwaysInteract = 48;
  kAECanSwitchLayer = 64;
  kAEDontRecord = 4096;
  kAEDontExecute = 8192;
  kAEProcessNonReplyEvents = 32768;
  kAEDoNotAutomaticallyAddAnnotationsToEvent = 65536;
  kAEDefaultTimeout = -1;

  libCoreServices = '/System/Library/Frameworks/CoreServices.framework/CoreServices';

type
  AESendMode = SInt32;
  NSAppleEventSendOptions = NSInteger;

  AEDescList = AEDesc;
  AERecord = AEDescList;
  AppleEvent = AERecord;
  PAppleEvent = ^AppleEvent;

  NSAppleEventDescriptor = interface;

  NSAppleEventDescriptorClass = interface(NSObjectClass)
    ['{B87A697A-AB44-4119-AF04-CCF81B6A2DD8}']
    {class} function appleEventWithEventClass(eventClass: AEEventClass; eventID: AEEventID; targetDescriptor: NSAppleEventDescriptor;
      returnID: AEReturnID; transactionID: AETransactionID): NSAppleEventDescriptor; cdecl;
    {class} function currentProcessDescriptor: NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithApplicationURL(applicationURL: NSURL): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithBoolean(boolean: Boolean): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithBundleIdentifier(bundleIdentifier: NSString): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithDate(date: NSDate): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithDescriptorType(descriptorType: DescType; bytes: Pointer; length: NSUInteger): NSAppleEventDescriptor; overload; cdecl;
    {class} function descriptorWithDescriptorType(descriptorType: DescType; data: NSData): NSAppleEventDescriptor; overload; cdecl;
    {class} function descriptorWithDouble(doubleValue: Double): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithEnumCode(enumerator: OSType): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithFileURL(fileURL: NSURL): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithInt32(signedInt: SInt32): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithProcessIdentifier(processIdentifier: pid_t): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithString(&string: NSString): NSAppleEventDescriptor; cdecl;
    {class} function descriptorWithTypeCode(typeCode: OSType): NSAppleEventDescriptor; cdecl;
    {class} function listDescriptor: NSAppleEventDescriptor; cdecl;
    {class} function nullDescriptor: NSAppleEventDescriptor; cdecl;
    {class} function recordDescriptor: NSAppleEventDescriptor; cdecl;
  end;

  NSAppleEventDescriptor = interface(NSObject)
    ['{B2196BCD-75CB-464A-B099-FA37F949E663}']
    function aeDesc: PAEDesc; cdecl;
    function attributeDescriptorForKeyword(keyword: AEKeyword): NSAppleEventDescriptor; cdecl;
    function booleanValue: Boolean; cdecl;
    function coerceToDescriptorType(descriptorType: DescType): NSAppleEventDescriptor; cdecl;
    function data: NSData; cdecl;
    function dateValue: NSDate; cdecl;
    function descriptorAtIndex(index: NSInteger): NSAppleEventDescriptor; cdecl;
    function descriptorForKeyword(keyword: AEKeyword): NSAppleEventDescriptor; cdecl;
    function descriptorType: DescType; cdecl;
    function doubleValue: Double; cdecl;
    function enumCodeValue: OSType; cdecl;
    function eventClass: AEEventClass; cdecl;
    function eventID: AEEventID; cdecl;
    function fileURLValue: NSURL; cdecl;
    function initListDescriptor: Pointer; cdecl;
    function initRecordDescriptor: Pointer; cdecl;
    function initWithAEDescNoCopy(aeDesc: PAEDesc): Pointer; cdecl;
    function initWithDescriptorType(descriptorType: DescType; data: NSData): Pointer; overload; cdecl;
    function initWithDescriptorType(descriptorType: DescType; bytes: Pointer; length: NSUInteger): Pointer; overload; cdecl;
    function initWithEventClass(eventClass: AEEventClass; eventID: AEEventID; targetDescriptor: NSAppleEventDescriptor; returnID: AEReturnID;
      transactionID: AETransactionID): Pointer; cdecl;
    procedure insertDescriptor(descriptor: NSAppleEventDescriptor; atIndex: NSInteger); cdecl;
    function int32Value: SInt32; cdecl;
    function isRecordDescriptor: Boolean; cdecl;
    function keywordForDescriptorAtIndex(index: NSInteger): AEKeyword; cdecl;
    function numberOfItems: NSInteger; cdecl;
    function paramDescriptorForKeyword(keyword: AEKeyword): NSAppleEventDescriptor; cdecl;
    procedure removeDescriptorAtIndex(index: NSInteger); cdecl;
    procedure removeDescriptorWithKeyword(keyword: AEKeyword); cdecl;
    procedure removeParamDescriptorWithKeyword(keyword: AEKeyword); cdecl;
    function returnID: AEReturnID; cdecl;
    function sendEventWithOptions(sendOptions: NSAppleEventSendOptions; timeout: NSTimeInterval; error: PPointer): NSAppleEventDescriptor; cdecl;
    procedure setAttributeDescriptor(descriptor: NSAppleEventDescriptor; forKeyword: AEKeyword); cdecl;
    procedure setDescriptor(descriptor: NSAppleEventDescriptor; forKeyword: AEKeyword); cdecl;
    procedure setParamDescriptor(descriptor: NSAppleEventDescriptor; forKeyword: AEKeyword); cdecl;
    function stringValue: NSString; cdecl;
    function transactionID: AETransactionID; cdecl;
    function typeCodeValue: OSType; cdecl;
  end;
  TNSAppleEventDescriptor = class(TOCGenericImport<NSAppleEventDescriptorClass, NSAppleEventDescriptor>) end;

function AESendMessage(event: PAppleEvent; reply: PAppleEvent; sendMode: AESendMode; timeOutInTicks: Integer): OSStatus; cdecl;
  external libCoreServices name _PU + 'AESendMessage';

implementation

uses
  System.SysUtils;

var
  CoreServicesModule: THandle;

initialization
  CoreServicesModule := LoadLibrary(libCoreServices);

finalization
  if CoreServicesModule <> 0 then
    FreeLibrary(CoreServicesModule);

end.