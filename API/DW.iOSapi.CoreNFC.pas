unit DW.iOSapi.CoreNFC;

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
  Macapi.CoreFoundation, Macapi.CoreServices, Macapi.Dispatch, Macapi.Mach, Macapi.ObjCRuntime, Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

const
  NFCReaderErrorUnsupportedFeature = 1;
  NFCReaderErrorSecurityViolation = 2;
  NFCReaderTransceiveErrorTagConnectionLost = 100;
  NFCReaderTransceiveErrorRetryExceeded = 101;
  NFCReaderTransceiveErrorTagResponseError = 102;
  NFCReaderSessionInvalidationErrorUserCanceled = 200;
  NFCReaderSessionInvalidationErrorSessionTimeout = 201;
  NFCReaderSessionInvalidationErrorSessionTerminatedUnexpectedly = 202;
  NFCReaderSessionInvalidationErrorSystemIsBusy = 203;
  NFCReaderSessionInvalidationErrorFirstNDEFTagRead = 204;
  NFCTagCommandConfigurationErrorInvalidParameters = 300;
  NFCTagTypeISO15693 = 1;
  NFCTypeNameFormatEmpty = 0;
  NFCTypeNameFormatNFCWellKnown = 1;
  NFCTypeNameFormatMedia = 2;
  NFCTypeNameFormatAbsoluteURI = 3;
  NFCTypeNameFormatNFCExternal = 4;
  NFCTypeNameFormatUnknown = 5;
  NFCTypeNameFormatUnchanged = 6;

type
  NFCReaderSession = interface;
  NFCTag = interface;
  NFCReaderSessionDelegate = interface;
  NFCTagCommandConfiguration = interface;
  NFCNDEFReaderSession = interface;
  NFCNDEFPayload = interface;
  NFCNDEFMessage = interface;
  NFCNDEFReaderSessionDelegate = interface;
  NFCISO15693CustomCommandConfiguration = interface;
  NFCISO15693ReadMultipleBlocksConfiguration = interface;
  NFCISO15693Tag = interface;
  NFCISO15693ReaderSession = interface;

  NFCReaderError = NSInteger;
  NFCTagType = NSUInteger;
  NFCTypeNameFormat = Byte;

  _NSRange = record
    location: NSUInteger;
    length: NSUInteger;
  end;

  TCoreNFCCompletionHandler = procedure(param1: NSData; param2: NSError) of object;

  NFCReaderSessionClass = interface(NSObjectClass)
    ['{A2270724-00F6-41D5-AE52-648FA29D0EC8}']
  end;

  NFCReaderSession = interface(NSObject)
    ['{C96B4C66-BF8B-48F9-8353-6E0AFA2B4DB7}']
    function isReady: Boolean; cdecl;
    procedure setAlertMessage(alertMessage: NSString); cdecl;
    function alertMessage: NSString; cdecl;
    procedure beginSession; cdecl;
    procedure invalidateSession; cdecl;
    function delegate: Pointer; cdecl;
    function sessionQueue: dispatch_queue_t; cdecl;
  end;
  TNFCReaderSession = class(TOCGenericImport<NFCReaderSessionClass, NFCReaderSession>)
  end;

  NFCTagCommandConfigurationClass = interface(NSObjectClass)
    ['{405F7488-F607-463D-A8B4-B6605BB335FF}']
  end;

  NFCTagCommandConfiguration = interface(NSObject)
    ['{1E5145F3-2972-4AEE-829D-E95C7B1ED033}']
    procedure setMaximumRetries(maximumRetries: NSUInteger); cdecl;
    function maximumRetries: NSUInteger; cdecl;
    procedure setRetryInterval(retryInterval: NSTimeInterval); cdecl;
    function retryInterval: NSTimeInterval; cdecl;
  end;
  TNFCTagCommandConfiguration = class(TOCGenericImport<NFCTagCommandConfigurationClass, NFCTagCommandConfiguration>)
  end;

  NFCNDEFReaderSessionClass = interface(NFCReaderSessionClass)
    ['{9A6298C7-E555-450B-B8DE-149696530FF7}']
    function readingAvailable: Boolean; cdecl;
  end;

  NFCNDEFReaderSession = interface(NFCReaderSession)
    ['{E79D6F63-D139-401C-A891-6055103D6782}']
    function initWithDelegate(delegate: Pointer; queue: dispatch_queue_t; invalidateAfterFirstRead: Boolean): Pointer; cdecl;
  end;
  TNFCNDEFReaderSession = class(TOCGenericImport<NFCNDEFReaderSessionClass, NFCNDEFReaderSession>)
  end;

  NFCNDEFPayloadClass = interface(NSObjectClass)
    ['{C7888424-A0F1-4ACA-8559-A435E514EFAF}']
  end;

  NFCNDEFPayload = interface(NSObject)
    ['{758E4F81-25B0-4CA8-A5C2-C08FA755D570}']
    procedure setTypeNameFormat(typeNameFormat: NFCTypeNameFormat); cdecl;
    function typeNameFormat: NFCTypeNameFormat; cdecl;
    procedure setType(&type: NSData); cdecl;
    function &type: NSData; cdecl;
    procedure setIdentifier(identifier: NSData); cdecl;
    function identifier: NSData; cdecl;
    procedure setPayload(payload: NSData); cdecl;
    function payload: NSData; cdecl;
  end;
  TNFCNDEFPayload = class(TOCGenericImport<NFCNDEFPayloadClass, NFCNDEFPayload>)
  end;

  NFCNDEFMessageClass = interface(NSObjectClass)
    ['{944720C2-6554-4068-931A-2AA6315BBE56}']
  end;

  NFCNDEFMessage = interface(NSObject)
    ['{82DCD535-0DE5-4878-AED7-AD8E569C7B5D}']
    procedure setRecords(records: NSArray); cdecl;
    function records: NSArray; cdecl;
  end;
  TNFCNDEFMessage = class(TOCGenericImport<NFCNDEFMessageClass, NFCNDEFMessage>)
  end;

  NFCISO15693CustomCommandConfigurationClass = interface
    (NFCTagCommandConfigurationClass)
    ['{F1E5C282-0A9D-4719-9CFD-5724DCF363AB}']
  end;

  NFCISO15693CustomCommandConfiguration = interface(NFCTagCommandConfiguration)
    ['{D954FF02-AB5C-424B-9E42-2CBA42EF82E5}']
    procedure setManufacturerCode(manufacturerCode: NSUInteger); cdecl;
    function manufacturerCode: NSUInteger; cdecl;
    procedure setCustomCommandCode(customCommandCode: NSUInteger); cdecl;
    function customCommandCode: NSUInteger; cdecl;
    procedure setRequestParameters(requestParameters: NSData); cdecl;
    function requestParameters: NSData; cdecl;
    [MethodName('initWithManufacturerCode:customCommandCode:requestParameters:')]
    function initWithManufacturerCodeCustomCommandCodeRequestParameters(manufacturerCode: NSUInteger; customCommandCode: NSUInteger;
      requestParameters: NSData): Pointer; cdecl;
    [MethodName('initWithManufacturerCode:customCommandCode:requestParameters:maximumRetries:retryInterval:')]
    function initWithManufacturerCodeCustomCommandCodeRequestParametersMaximumRetriesRetryInterval(manufacturerCode: NSUInteger;
      customCommandCode: NSUInteger; requestParameters: NSData; maximumRetries: NSUInteger; retryInterval: NSTimeInterval): Pointer; cdecl;
  end;
  TNFCISO15693CustomCommandConfiguration = class(TOCGenericImport<NFCISO15693CustomCommandConfigurationClass, NFCISO15693CustomCommandConfiguration>)
  end;

  NFCISO15693ReadMultipleBlocksConfigurationClass = interface
    (NFCTagCommandConfigurationClass)
    ['{6EF84AAF-409C-4686-949B-0AF650E9F0C1}']
  end;

  NFCISO15693ReadMultipleBlocksConfiguration = interface
    (NFCTagCommandConfiguration)
    ['{C6ADB127-7962-4947-9341-4417E84BA013}']
    procedure setRange(range: NSRange); cdecl;
    function range: NSRange; cdecl;
    procedure setChunkSize(chunkSize: NSUInteger); cdecl;
    function chunkSize: NSUInteger; cdecl;
    [MethodName('initWithRange:chunkSize:')]
    function initWithRangeChunkSize(range: NSRange; chunkSize: NSUInteger): Pointer; cdecl;
    [MethodName('initWithRange:chunkSize:maximumRetries:retryInterval:')]
    function initWithRangeChunkSizeMaximumRetriesRetryInterval(range: NSRange; chunkSize: NSUInteger; maximumRetries: NSUInteger;
      retryInterval: NSTimeInterval): Pointer; cdecl;
  end;
  TNFCISO15693ReadMultipleBlocksConfiguration = class(TOCGenericImport<NFCISO15693ReadMultipleBlocksConfigurationClass, NFCISO15693ReadMultipleBlocksConfiguration>)
  end;

  NFCISO15693ReaderSessionClass = interface(NFCReaderSessionClass)
    ['{541BEE49-EEEF-4F62-84DA-6745FA5F63C9}']
  end;

  NFCISO15693ReaderSession = interface(NFCReaderSession)
    ['{B4D21CF2-A0A2-4C5C-8C65-1C7B1059C7B3}']
    procedure setReadingAvailable(readingAvailable: Boolean); cdecl;
    function readingAvailable: Boolean; cdecl;
    function initWithDelegate(delegate: Pointer; queue: dispatch_queue_t): Pointer; cdecl;
    procedure restartPolling; cdecl;
  end;
  TNFCISO15693ReaderSession = class(TOCGenericImport<NFCISO15693ReaderSessionClass, NFCISO15693ReaderSession>)
  end;

  NFCTag = interface(IObjectiveC)
    ['{A952D420-4A9E-4435-8175-F1822FDCFE1F}']
    function &type: NFCTagType; cdecl;
    function session: Pointer; cdecl;
    function isAvailable: Boolean; cdecl;
  end;

  NFCReaderSessionDelegate = interface(IObjectiveC)
    ['{C50CC9AA-7E19-4995-81CA-D35830A34266}']
    procedure readerSessionDidBecomeActive(session: NFCReaderSession); cdecl;
    [MethodName('readerSession:didDetectTags:')]
    procedure readerSessionDidDetectTags(session: NFCReaderSession; didDetectTags: NSArray); cdecl;
    [MethodName('readerSession:didInvalidateWithError:')]
    procedure readerSessionDidInvalidateWithError(session: NFCReaderSession; didInvalidateWithError: NSError); cdecl;
  end;

  NFCNDEFReaderSessionDelegate = interface(IObjectiveC)
    ['{93BE6978-C3CF-44A9-AF7B-BE565F760602}']
    [MethodName('readerSession:didInvalidateWithError:')]
    procedure readerSessionDidInvalidateWithError(session: NFCNDEFReaderSession; didInvalidateWithError: NSError); cdecl;
    [MethodName('readerSession:didDetectNDEFs:')]
    procedure readerSessionDidDetectNDEFs(session: NFCNDEFReaderSession; didDetectNDEFs: NSArray); cdecl;
  end;

  NFCISO15693Tag = interface(IObjectiveC)
    ['{7BC44DDA-69C8-47D5-9CBC-9862ABC66350}']
    function identifier: NSData; cdecl;
    function icManufacturerCode: NSUInteger; cdecl;
    function icSerialNumber: NSData; cdecl;
    procedure sendCustomCommandWithConfiguration(commandConfiguration: NFCISO15693CustomCommandConfiguration;
      completionHandler: TCoreNFCCompletionHandler); cdecl;
    procedure readMultipleBlocksWithConfiguration(readConfiguration: NFCISO15693ReadMultipleBlocksConfiguration;
      completionHandler: TCoreNFCCompletionHandler); cdecl;
  end;

function NFCErrorDomain: NSString;
function NFCISO15693TagResponseErrorKey: NSString;

const
  libCoreNFC = '/System/Library/Frameworks/CoreNFC.framework/CoreNFC';

implementation

{$IF defined(IOS) and NOT defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  CoreNFCModule: THandle;
{$ENDIF IOS}

function NFCErrorDomain: NSString;
begin
  Result := CocoaNSStringConst(libCoreNFC, 'NFCErrorDomain');
end;

function NFCISO15693TagResponseErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreNFC, 'NFCISO15693TagResponseErrorKey');
end;

procedure CoreNFCLoader; cdecl; external libCoreNFC;

{$IF defined(IOS) and NOT defined(CPUARM)}
initialization
  CoreNFCModule := dlopen(MarshaledAString(libCoreNFC), RTLD_LAZY);

finalization
  dlclose(CoreNFCModule);
{$ENDIF IOS}

end.
