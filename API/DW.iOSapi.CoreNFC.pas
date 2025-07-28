unit DW.iOSapi.CoreNFC;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation,
  // DW
  DW.iOSapi.Foundation;

const
  NFCReaderErrorUnsupportedFeature = 1;
  NFCReaderErrorSecurityViolation = 2;
  NFCReaderErrorInvalidParameter = 3;
  NFCReaderErrorInvalidParameterLength = 4;
  NFCReaderErrorParameterOutOfBound = 5;
  NFCReaderErrorRadioDisabled = 6;
  NFCReaderTransceiveErrorTagConnectionLost = 100;
  NFCReaderTransceiveErrorRetryExceeded = 101;
  NFCReaderTransceiveErrorTagResponseError = 102;
  NFCReaderTransceiveErrorSessionInvalidated = 103;
  NFCReaderTransceiveErrorTagNotConnected = 104;
  NFCReaderTransceiveErrorPacketTooLong = 105;
  NFCReaderSessionInvalidationErrorUserCanceled = 200;
  NFCReaderSessionInvalidationErrorSessionTimeout = 201;
  NFCReaderSessionInvalidationErrorSessionTerminatedUnexpectedly = 202;
  NFCReaderSessionInvalidationErrorSystemIsBusy = 203;
  NFCReaderSessionInvalidationErrorFirstNDEFTagRead = 204;
  NFCTagCommandConfigurationErrorInvalidParameters = 300;
  NFCNdefReaderSessionErrorTagNotWritable = 400;
  NFCNdefReaderSessionErrorTagUpdateFailure = 401;
  NFCNdefReaderSessionErrorTagSizeTooSmall = 402;
  NFCNdefReaderSessionErrorZeroLengthMessage = 403;
  NFCTagTypeISO15693 = 1;
  NFCTagTypeFeliCa = 2;
  NFCTagTypeISO7816Compatible = 3;
  NFCTagTypeMiFare = 4;
  NFCPollingISO14443 = 1;
  NFCPollingISO15693 = 2;
  NFCPollingISO18092 = 4;
  NFCPollingPACE = 8;
  NFCISO15693RequestFlagDualSubCarriers = 1;
  NFCISO15693RequestFlagHighDataRate = 2;
  NFCISO15693RequestFlagProtocolExtension = 8;
  NFCISO15693RequestFlagSelect = 16;
  NFCISO15693RequestFlagAddress = 32;
  NFCISO15693RequestFlagOption = 64;
  NFCISO15693RequestFlagCommandSpecificBit8 = -128;
  RequestFlagDualSubCarriers = NFCISO15693RequestFlagDualSubCarriers;
  RequestFlagHighDataRate = NFCISO15693RequestFlagHighDataRate;
  RequestFlagProtocolExtension = NFCISO15693RequestFlagProtocolExtension;
  RequestFlagSelect = NFCISO15693RequestFlagSelect;
  RequestFlagAddress = NFCISO15693RequestFlagAddress;
  RequestFlagOption = NFCISO15693RequestFlagOption;
  NFCISO15693ResponseFlagError = 1;
  NFCISO15693ResponseFlagResponseBufferValid = 2;
  NFCISO15693ResponseFlagFinalResponse = 4;
  NFCISO15693ResponseFlagProtocolExtension = 8;
  NFCISO15693ResponseFlagBlockSecurityStatusBit5 = 16;
  NFCISO15693ResponseFlagBlockSecurityStatusBit6 = 32;
  NFCISO15693ResponseFlagWaitTimeExtension = 64;
  NFCNDEFStatusNotSupported = 1;
  NFCNDEFStatusReadWrite = 2;
  NFCNDEFStatusReadOnly = 3;
  NFCFeliCaPollingRequestCodeNoRequest = 0;
  NFCFeliCaPollingRequestCodeSystemCode = 1;
  NFCFeliCaPollingRequestCodeCommunicationPerformance = 2;
  PollingRequestCodeNoRequest = NFCFeliCaPollingRequestCodeNoRequest;
  PollingRequestCodeSystemCode = NFCFeliCaPollingRequestCodeSystemCode;
  PollingRequestCodeCommunicationPerformance = NFCFeliCaPollingRequestCodeCommunicationPerformance;
  NFCFeliCaPollingTimeSlotMax1 = 0;
  NFCFeliCaPollingTimeSlotMax2 = 1;
  NFCFeliCaPollingTimeSlotMax4 = 3;
  NFCFeliCaPollingTimeSlotMax8 = 7;
  NFCFeliCaPollingTimeSlotMax16 = 15;
  PollingTimeSlotMax1 = NFCFeliCaPollingTimeSlotMax1;
  PollingTimeSlotMax2 = NFCFeliCaPollingTimeSlotMax2;
  PollingTimeSlotMax4 = NFCFeliCaPollingTimeSlotMax4;
  PollingTimeSlotMax8 = NFCFeliCaPollingTimeSlotMax8;
  PollingTimeSlotMax16 = NFCFeliCaPollingTimeSlotMax16;
  NFCFeliCaEncryptionIdAES = 79;
  NFCFeliCaEncryptionIdAES_DES = 65;
  EncryptionIdAES = NFCFeliCaEncryptionIdAES;
  EncryptionIdAES_DES = NFCFeliCaEncryptionIdAES_DES;
  NFCMiFareUnknown = 1;
  NFCMiFareUltralight = 2;
  NFCMiFarePlus = 3;
  NFCMiFareDESFire = 4;
  NFCTypeNameFormatEmpty = 0;
  NFCTypeNameFormatNFCWellKnown = 1;
  NFCTypeNameFormatMedia = 2;
  NFCTypeNameFormatAbsoluteURI = 3;
  NFCTypeNameFormatNFCExternal = 4;
  NFCTypeNameFormatUnknown = 5;
  NFCTypeNameFormatUnchanged = 6;
  NFCVASModeURLOnly = 0;
  NFCVASModeNormal = 1;
  VASModeURLOnly = NFCVASModeURLOnly;
  VASModeNormal = NFCVASModeNormal;
  NFCVASErrorCodeSuccess = 36864;
  NFCVASErrorCodeDataNotFound = 27267;
  NFCVASErrorCodeDataNotActivated = 25223;
  NFCVASErrorCodeWrongParameters = 27392;
  NFCVASErrorCodeWrongLCField = 26368;
  NFCVASErrorCodeUserIntervention = 27012;
  NFCVASErrorCodeIncorrectData = 27264;
  NFCVASErrorCodeUnsupportedApplicationVersion = 25408;
  VASErrorCodeSuccess = NFCVASErrorCodeSuccess;
  VASErrorCodeDataNotFound = NFCVASErrorCodeDataNotFound;
  VASErrorCodeDataNotActivated = NFCVASErrorCodeDataNotActivated;
  VASErrorCodeWrongParameters = NFCVASErrorCodeWrongParameters;
  VASErrorCodeWrongLCField = NFCVASErrorCodeWrongLCField;
  VASErrorCodeUserIntervention = NFCVASErrorCodeUserIntervention;
  VASErrorCodeIncorrectData = NFCVASErrorCodeIncorrectData;
  VASErrorCodeUnsupportedApplicationVersion = NFCVASErrorCodeUnsupportedApplicationVersion;

type
  NFCReaderSession = interface;
  NFCReaderSessionDelegate = interface;
  NFCTag = interface;
  NFCTagCommandConfiguration = interface;
  NFCTagReaderSessionDelegate = interface;
  NFCTagReaderSession = interface;
  NFCNDEFReaderSessionDelegate = interface;
  NFCNDEFReaderSession = interface;
  NFCISO15693CustomCommandConfiguration = interface;
  NFCISO15693ReadMultipleBlocksConfiguration = interface;
  NFCISO15693Tag = interface;
  NFCISO15693ReaderSession = interface;
  NFCNDEFTag = interface;
  NFCFeliCaTag = interface;
  NFCISO7816APDU = interface;
  NFCISO7816Tag = interface;
  NFCMiFareTag = interface;
  NFCNDEFPayload = interface;
  NFCNDEFMessage = interface;
  NFCVASCommandConfiguration = interface;
  NFCVASResponse = interface;
  NFCVASReaderSessionDelegate = interface;
  NFCVASReaderSession = interface;

  NFCReaderError = NSInteger;
  NFCTagType = NSInteger;
  NFCPollingOption = NSInteger;
  NFCISO15693RequestFlag = NSInteger;
  RequestFlag = Integer;
  NFCISO15693ResponseFlag = NSInteger;
  NFCNDEFStatus = NSInteger;
  NFCFeliCaPollingRequestCode = NSInteger;
  PollingRequestCode = Integer;
  NFCFeliCaPollingTimeSlot = NSInteger;
  PollingTimeSlot = Integer;
  NFCFeliCaEncryptionId = NSInteger;
  EncryptionId = Integer;
  NFCMiFareFamily = NSInteger;
  NFCTypeNameFormat = NSInteger;
  NFCVASMode = NSInteger;
  VASMode = Integer;
  NFCVASErrorCode = NSInteger;
  VASErrorCode = Integer;
  PNSLocale = ^NSLocale;

  TNFCTagReaderSessionBlockMethod1 = procedure(error: NSError) of object;
  TNFCNDEFReaderSessionBlockMethod1 = procedure(error: NSError) of object;
  TNFCISO15693TagBlockMethod1 = procedure(customResponseParameters: NSData; error: NSError) of object;
  TNFCISO15693TagBlockMethod2 = procedure(data: NSData; error: NSError) of object;
  TNFCISO15693TagBlockMethod3 = procedure(error: NSError) of object;
  TNFCISO15693TagBlockMethod4 = procedure(dataBlocks: NSArray; error: NSError) of object;
  TNFCISO15693TagBlockMethod5 = procedure(dsfid: NSInteger; afi: NSInteger; blockSize: NSInteger; blockCount: NSInteger; icReference: NSInteger;
    error: NSError) of object;
  TNFCISO15693TagBlockMethod6 = procedure(uid: NSData; dsfid: NSInteger; afi: NSInteger; blockSize: NSInteger; blockCount: NSInteger;
    icReference: NSInteger; error: NSError) of object;
  TNFCISO15693TagBlockMethod7 = procedure(securityStatus: NSArray; error: NSError) of object;
  TNFCISO15693TagBlockMethod8 = procedure(responseFlag: NFCISO15693ResponseFlag; response: NSData; error: NSError) of object;
  TNFCISO15693TagBlockMethod9 = procedure(responseFlag: NFCISO15693ResponseFlag; data: NSData; error: NSError) of object;
  TNFCNDEFTagBlockMethod1 = procedure(status: NFCNDEFStatus; capacity: NSUInteger; error: NSError) of object;
  TNFCNDEFTagBlockMethod2 = procedure(param1: NFCNDEFMessage; param2: NSError) of object;
  TNFCNDEFTagBlockMethod3 = procedure(param1: NSError) of object;
  TNFCFeliCaTagBlockMethod1 = procedure(pmm: NSData; requestData: NSData; error: NSError) of object;
  TNFCFeliCaTagBlockMethod2 = procedure(nodeKeyVersionList: NSArray; error: NSError) of object;
  TNFCFeliCaTagBlockMethod3 = procedure(mode: NSInteger; error: NSError) of object;
  TNFCFeliCaTagBlockMethod4 = procedure(statusFlag1: NSInteger; statusFlag2: NSInteger; blockData: NSArray; error: NSError) of object;
  TNFCFeliCaTagBlockMethod5 = procedure(statusFlag1: NSInteger; statusFlag2: NSInteger; error: NSError) of object;
  TNFCFeliCaTagBlockMethod6 = procedure(systemCodeList: NSArray; error: NSError) of object;
  TNFCFeliCaTagBlockMethod7 = procedure(statusFlag1: NSInteger; statusFlag2: NSInteger; encryptionIdentifier: NFCFeliCaEncryptionId;
    nodeKeyVersionListAES: NSArray; nodeKeyVersionListDES: NSArray; error: NSError) of object;
  TNFCFeliCaTagBlockMethod8 = procedure(statusFlag1: NSInteger; statusFlag2: NSInteger; basicVersion: NSData; optionVersion: NSData;
    error: NSError) of object;
  TNFCFeliCaTagBlockMethod9 = procedure(responsePacket: NSData; error: NSError) of object;
  TNFCISO7816TagBlockMethod1 = procedure(responseData: NSData; sw1: UInt8; sw2: UInt8; error: NSError) of object;
  TNFCMiFareTagBlockMethod1 = procedure(response: NSData; error: NSError) of object;
  TNFCMiFareTagBlockMethod2 = procedure(responseData: NSData; sw1: UInt8; sw2: UInt8; error: NSError) of object;

  NFCReaderSessionDelegate = interface(IObjectiveC)
    ['{6E129976-693D-4E2B-BC74-8E8CA3785AC6}']
    procedure readerSession(session: NFCReaderSession; didDetectTags: NSArray); overload; cdecl;
    procedure readerSession(session: NFCReaderSession; didInvalidateWithError: NSError); overload; cdecl;
    procedure readerSessionDidBecomeActive(session: NFCReaderSession); cdecl;
  end;

  NFCReaderSessionClass = interface(NSObjectClass)
    ['{968750B4-5EFA-4DB1-83D4-3E72BC345891}']
    {class} function readingAvailable: Boolean; cdecl;
  end;

  NFCReaderSession = interface(NSObject)
    ['{78DE8ADC-5954-4DA2-9A8E-2B6C221E3479}']
    function alertMessage: NSString; cdecl;
    function delegate: Pointer; cdecl;
    procedure beginSession; cdecl;
    procedure invalidateSession; cdecl;
    procedure invalidateSessionWithErrorMessage(errorMessage: NSString); cdecl;
    function isReady: Boolean; cdecl;
    function sessionQueue: dispatch_queue_t; cdecl;
    procedure setAlertMessage(alertMessage: NSString); cdecl;
  end;
  TNFCReaderSession = class(TOCGenericImport<NFCReaderSessionClass, NFCReaderSession>) end;

  NFCTag = interface(IObjectiveC)
    ['{AD3A62DF-EC66-4EB1-AA2C-271E676BEA81}']
    function &type: NFCTagType; cdecl;
    function asNFCFeliCaTag: Pointer; cdecl;
    function asNFCISO15693Tag: Pointer; cdecl;
    function asNFCISO7816Tag: Pointer; cdecl;
    function asNFCMiFareTag: Pointer; cdecl;
    function isAvailable: Boolean; cdecl;
    function session: Pointer; cdecl;
  end;

  NFCTagCommandConfigurationClass = interface(NSObjectClass)
    ['{D5707A5D-AC18-41AD-98AF-B67367E64E55}']
  end;

  NFCTagCommandConfiguration = interface(NSObject)
    ['{6F6D6376-7562-4662-A3C2-D72747F8F5B0}']
    function maximumRetries: NSUInteger; cdecl;
    function retryInterval: NSTimeInterval; cdecl;
    procedure setMaximumRetries(maximumRetries: NSUInteger); cdecl;
    procedure setRetryInterval(retryInterval: NSTimeInterval); cdecl;
  end;
  TNFCTagCommandConfiguration = class(TOCGenericImport<NFCTagCommandConfigurationClass, NFCTagCommandConfiguration>) end;

  NFCTagReaderSessionDelegate = interface(IObjectiveC)
    ['{093E7A29-A53C-4378-AF0C-7DCEA0034CD5}']
    procedure tagReaderSession(session: NFCTagReaderSession; didInvalidateWithError: NSError); overload; cdecl;
    procedure tagReaderSession(session: NFCTagReaderSession; didDetectTags: NSArray); overload; cdecl;
    procedure tagReaderSessionDidBecomeActive(session: NFCTagReaderSession); cdecl;
  end;

  NFCTagReaderSessionClass = interface(NFCReaderSessionClass)
    ['{9E318490-8651-4D4F-A425-19FE051E4BDD}']
  end;

  NFCTagReaderSession = interface(NFCReaderSession)
    ['{3DD20365-DDDD-405D-B37D-E75588C63E06}']
    function connectedTag: Pointer; cdecl;
    procedure connectToTag(tag: Pointer; completionHandler: TNFCTagReaderSessionBlockMethod1); cdecl;
    function initWithPollingOption(pollingOption: NFCPollingOption; delegate: Pointer; queue: dispatch_queue_t): Pointer; cdecl;
    procedure restartPolling; cdecl;
  end;
  TNFCTagReaderSession = class(TOCGenericImport<NFCTagReaderSessionClass, NFCTagReaderSession>) end;

  NFCNDEFReaderSessionDelegate = interface(IObjectiveC)
    ['{DAA366EA-4AFA-4E2E-9753-0CE21F47BC97}']
    procedure readerSession(session: NFCNDEFReaderSession; didDetectNDEFs: NSArray); overload; cdecl;
    procedure readerSession(session: NFCNDEFReaderSession; didInvalidateWithError: NSError); overload; cdecl;
    procedure readerSessionDidBecomeActive(session: NFCNDEFReaderSession); cdecl;
    [MethodName('readerSession:didDetectTags:')]
    procedure readerSessionDidDetectTags(session: NFCNDEFReaderSession; didDetectTags: NSArray); cdecl;
  end;

  NFCNDEFReaderSessionClass = interface(NFCReaderSessionClass)
    ['{572CAE04-AFC4-4757-A273-B73395307831}']
  end;

  NFCNDEFReaderSession = interface(NFCReaderSession)
    ['{523181DE-B9ED-4057-B743-D78B056D027C}']
    procedure connectToTag(tag: Pointer; completionHandler: TNFCNDEFReaderSessionBlockMethod1); cdecl;
    function initWithDelegate(delegate: Pointer; queue: dispatch_queue_t; invalidateAfterFirstRead: Boolean): Pointer; cdecl;
    procedure restartPolling; cdecl;
  end;
  TNFCNDEFReaderSession = class(TOCGenericImport<NFCNDEFReaderSessionClass, NFCNDEFReaderSession>) end;

  NFCISO15693CustomCommandConfigurationClass = interface(NFCTagCommandConfigurationClass)
    ['{32DE3A4F-D29D-4F9E-86C3-0F553D17BB2B}']
  end;

  NFCISO15693CustomCommandConfiguration = interface(NFCTagCommandConfiguration)
    ['{7DC7D08C-8FD2-43BB-92CF-7C242452F39D}']
    function customCommandCode: NSUInteger; cdecl;
    function initWithManufacturerCode(manufacturerCode: NSUInteger; customCommandCode: NSUInteger; requestParameters: NSData;
      maximumRetries: NSUInteger; retryInterval: NSTimeInterval): Pointer; overload; cdecl;
    function initWithManufacturerCode(manufacturerCode: NSUInteger; customCommandCode: NSUInteger; requestParameters: NSData): Pointer; overload; cdecl;
    function manufacturerCode: NSUInteger; cdecl;
    function requestParameters: NSData; cdecl;
    procedure setCustomCommandCode(customCommandCode: NSUInteger); cdecl;
    procedure setManufacturerCode(manufacturerCode: NSUInteger); cdecl;
    procedure setRequestParameters(requestParameters: NSData); cdecl;
  end;
  TNFCISO15693CustomCommandConfiguration = class(TOCGenericImport<NFCISO15693CustomCommandConfigurationClass, NFCISO15693CustomCommandConfiguration>) end;

  NFCISO15693ReadMultipleBlocksConfigurationClass = interface(NFCTagCommandConfigurationClass)
    ['{131F9F98-A39E-43C4-88B2-880A07B91EEA}']
  end;

  NFCISO15693ReadMultipleBlocksConfiguration = interface(NFCTagCommandConfiguration)
    ['{3BC74ABB-C18C-46FD-9461-5DF4E2FE5D05}']
    function chunkSize: NSUInteger; cdecl;
    function initWithRange(range: NSRange; chunkSize: NSUInteger; maximumRetries: NSUInteger; retryInterval: NSTimeInterval): Pointer; overload; cdecl;
    function initWithRange(range: NSRange; chunkSize: NSUInteger): Pointer; overload; cdecl;
    function range: NSRange; cdecl;
    procedure setChunkSize(chunkSize: NSUInteger); cdecl;
    procedure setRange(range: NSRange); cdecl;
  end;
  TNFCISO15693ReadMultipleBlocksConfiguration = class(TOCGenericImport<NFCISO15693ReadMultipleBlocksConfigurationClass, NFCISO15693ReadMultipleBlocksConfiguration>) end;

  NFCISO15693Tag = interface(IObjectiveC)
    ['{DEA5EBA5-8C42-44E9-8902-E0E85E5B20E0}']
    procedure authenticateWithRequestFlags(flags: NFCISO15693RequestFlag; cryptoSuiteIdentifier: NSInteger; message: NSData;
      completionHandler: Pointer); cdecl;
    procedure challengeWithRequestFlags(flags: NFCISO15693RequestFlag; cryptoSuiteIdentifier: NSInteger; message: NSData;
      completionHandler: Pointer); cdecl;
    procedure customCommandWithRequestFlag(flags: NFCISO15693RequestFlag; customCommandCode: NSInteger; customRequestParameters: NSData;
      completionHandler: Pointer); cdecl;
    procedure extendedFastReadMultipleBlocksWithRequestFlag(flags: NFCISO15693RequestFlag; blockRange: NSRange; completionHandler: Pointer); cdecl;
    procedure extendedGetMultipleBlockSecurityStatusWithRequestFlag(flags: NFCISO15693RequestFlag; blockRange: NSRange;
      completionHandler: Pointer); cdecl;
    procedure extendedLockBlockWithRequestFlags(flags: NFCISO15693RequestFlag; blockNumber: NSInteger; completionHandler: Pointer); cdecl;
    procedure extendedReadMultipleBlocksWithRequestFlags(flags: NFCISO15693RequestFlag; blockRange: NSRange; completionHandler: Pointer); cdecl;
    procedure extendedReadSingleBlockWithRequestFlags(flags: NFCISO15693RequestFlag; blockNumber: NSInteger; completionHandler: Pointer); cdecl;
    procedure extendedWriteMultipleBlocksWithRequestFlags(flags: NFCISO15693RequestFlag; blockRange: NSRange; dataBlocks: NSArray;
      completionHandler: Pointer); cdecl;
    procedure extendedWriteSingleBlockWithRequestFlags(flags: NFCISO15693RequestFlag; blockNumber: NSInteger; dataBlock: NSData;
      completionHandler: Pointer); cdecl;
    procedure fastReadMultipleBlocksWithRequestFlag(flags: NFCISO15693RequestFlag; blockRange: NSRange; completionHandler: Pointer); cdecl;
    procedure getMultipleBlockSecurityStatusWithRequestFlag(flags: NFCISO15693RequestFlag; blockRange: NSRange; completionHandler: Pointer); cdecl;
    procedure getSystemInfoAndUIDWithRequestFlag(flags: NFCISO15693RequestFlag; completionHandler: Pointer); cdecl;
    procedure getSystemInfoWithRequestFlag(flags: NFCISO15693RequestFlag; completionHandler: Pointer); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("getSystemInfoAndUIDWithRequestFlag:completionHandler:", ios(13.0, 14.0))
    function icManufacturerCode: NSUInteger; cdecl;
    function icSerialNumber: NSData; cdecl;
    function identifier: NSData; cdecl;
    procedure keyUpdateWithRequestFlags(flags: NFCISO15693RequestFlag; keyIdentifier: NSInteger; message: NSData; completionHandler: Pointer); cdecl;
    procedure lockAFIWithRequestFlag(flags: NFCISO15693RequestFlag; completionHandler: Pointer); cdecl;
    procedure lockBlockWithRequestFlags(flags: NFCISO15693RequestFlag; blockNumber: UInt8; completionHandler: Pointer); cdecl;
    procedure lockDFSIDWithRequestFlag(flags: NFCISO15693RequestFlag; completionHandler: Pointer); cdecl; // API_DEPRECATED_WITH_REPLACEMENT("lockDSFIDWithRequestFlag:completionHandler:", ios(13.0, 14.0))
    procedure lockDSFIDWithRequestFlag(flags: NFCISO15693RequestFlag; completionHandler: Pointer); cdecl;
    procedure readBufferWithRequestFlags(flags: NFCISO15693RequestFlag; completionHandler: Pointer); cdecl;
    procedure readMultipleBlocksWithConfiguration(readConfiguration: NFCISO15693ReadMultipleBlocksConfiguration; completionHandler: Pointer); cdecl;
    procedure readMultipleBlocksWithRequestFlags(flags: NFCISO15693RequestFlag; blockRange: NSRange; completionHandler: Pointer); cdecl;
    procedure readSingleBlockWithRequestFlags(flags: NFCISO15693RequestFlag; blockNumber: UInt8; completionHandler: Pointer); cdecl;
    procedure resetToReadyWithRequestFlags(flags: NFCISO15693RequestFlag; completionHandler: Pointer); cdecl;
    procedure selectWithRequestFlags(flags: NFCISO15693RequestFlag; completionHandler: Pointer); cdecl;
    procedure sendCustomCommandWithConfiguration(commandConfiguration: NFCISO15693CustomCommandConfiguration; completionHandler: Pointer); cdecl;
    procedure sendRequestWithFlag(flags: NSInteger; commandCode: NSInteger; data: NSData; completionHandler: Pointer); cdecl;
    procedure stayQuietWithCompletionHandler(completionHandler: Pointer); cdecl;
    procedure writeAFIWithRequestFlag(flags: NFCISO15693RequestFlag; afi: UInt8; completionHandler: Pointer); cdecl;
    procedure writeDSFIDWithRequestFlag(flags: NFCISO15693RequestFlag; dsfid: UInt8; completionHandler: Pointer); cdecl;
    procedure writeMultipleBlocksWithRequestFlags(flags: NFCISO15693RequestFlag; blockRange: NSRange; dataBlocks: NSArray;
      completionHandler: Pointer); cdecl;
    procedure writeSingleBlockWithRequestFlags(flags: NFCISO15693RequestFlag; blockNumber: UInt8; dataBlock: NSData;
      completionHandler: Pointer); cdecl;
  end;

  NFCISO15693ReaderSessionClass = interface(NFCReaderSessionClass)
    ['{66591F75-4D38-4960-ABBF-FAC70D58DDF2}']
  end;

  NFCISO15693ReaderSession = interface(NFCReaderSession)
    ['{7A8CB679-194A-4FBE-A815-096EB2C7A2E5}']
    function initWithDelegate(delegate: Pointer; queue: dispatch_queue_t): Pointer; cdecl; // API_DEPRECATED("No longer supported", ios(11.0, 17.0))
    procedure restartPolling; cdecl; // API_DEPRECATED("No longer supported", ios(11.0, 17.0))
  end;
  TNFCISO15693ReaderSession = class(TOCGenericImport<NFCISO15693ReaderSessionClass, NFCISO15693ReaderSession>) end;

  NFCNDEFTag = interface(IObjectiveC)
    ['{4A7EFC32-37B3-4812-B832-928FEECBC1ED}']
    function isAvailable: Boolean; cdecl;
    procedure queryNDEFStatusWithCompletionHandler(completionHandler: TNFCNDEFTagBlockMethod1); cdecl;
    procedure readNDEFWithCompletionHandler(completionHandler: TNFCNDEFTagBlockMethod2); cdecl;
    procedure writeLockWithCompletionHandler(completionHandler: TNFCNDEFTagBlockMethod3); cdecl;
    procedure writeNDEF(ndefMessage: NFCNDEFMessage; completionHandler: TNFCNDEFTagBlockMethod3); cdecl;
  end;

  NFCFeliCaTag = interface(IObjectiveC)
    ['{2D822FAA-B820-41D8-B963-523668BC5B88}']
    function currentIDm: NSData; cdecl;
    function currentSystemCode: NSData; cdecl;
    procedure pollingWithSystemCode(systemCode: NSData; requestCode: NFCFeliCaPollingRequestCode; timeSlot: NFCFeliCaPollingTimeSlot;
      completionHandler: Pointer); cdecl;
    procedure readWithoutEncryptionWithServiceCodeList(serviceCodeList: NSArray; blockList: NSArray; completionHandler: Pointer); cdecl;
    procedure requestResponseWithCompletionHandler(completionHandler: Pointer); cdecl;
    procedure requestServiceV2WithNodeCodeList(nodeCodeList: NSArray; completionHandler: Pointer); cdecl;
    procedure requestServiceWithNodeCodeList(nodeCodeList: NSArray; completionHandler: Pointer); cdecl;
    procedure requestSpecificationVersionWithCompletionHandler(completionHandler: Pointer); cdecl;
    procedure requestSystemCodeWithCompletionHandler(completionHandler: Pointer); cdecl;
    procedure resetModeWithCompletionHandler(completionHandler: Pointer); cdecl;
    procedure sendFeliCaCommandPacket(commandPacket: NSData; completionHandler: Pointer); cdecl;
    procedure writeWithoutEncryptionWithServiceCodeList(serviceCodeList: NSArray; blockList: NSArray; blockData: NSArray;
      completionHandler: Pointer); cdecl;
  end;

  NFCISO7816APDUClass = interface(NSObjectClass)
    ['{271DDACA-87D5-40EB-9B7A-E08928FA14FB}']
  end;

  NFCISO7816APDU = interface(NSObject)
    ['{F82AB22D-B528-4808-8D36-1F32971AA9F3}']
    function data: NSData; cdecl;
    function expectedResponseLength: NSInteger; cdecl;
    function initWithData(data: NSData): Pointer; cdecl;
    function initWithInstructionClass(instructionClass: UInt8; instructionCode: UInt8; p1Parameter: UInt8; p2Parameter: UInt8; data: NSData;
      expectedResponseLength: NSInteger): Pointer; cdecl;
    function instructionClass: UInt8; cdecl;
    function instructionCode: UInt8; cdecl;
    function p1Parameter: UInt8; cdecl;
    function p2Parameter: UInt8; cdecl;
  end;
  TNFCISO7816APDU = class(TOCGenericImport<NFCISO7816APDUClass, NFCISO7816APDU>) end;

  NFCISO7816Tag = interface(IObjectiveC)
    ['{1DBE9370-6943-44C7-8D38-979781259952}']
    function applicationData: NSData; cdecl;
    function historicalBytes: NSData; cdecl;
    function identifier: NSData; cdecl;
    function initialSelectedAID: NSString; cdecl;
    function proprietaryApplicationDataCoding: Boolean; cdecl;
    procedure sendCommandAPDU(apdu: NFCISO7816APDU; completionHandler: Pointer); cdecl;
  end;

  NFCMiFareTag = interface(IObjectiveC)
    ['{8FE46506-1E17-4B2C-9955-30D2E91DC8DD}']
    function historicalBytes: NSData; cdecl;
    function identifier: NSData; cdecl;
    function mifareFamily: NFCMiFareFamily; cdecl;
    procedure sendMiFareCommand(command: NSData; completionHandler: Pointer); cdecl;
    procedure sendMiFareISO7816Command(apdu: NFCISO7816APDU; completionHandler: Pointer); cdecl;
  end;

  NFCNDEFPayloadClass = interface(NSObjectClass)
    ['{26B8622C-501D-46C6-A8CA-5224C3C1928C}']
    {class} function wellKnownTypeTextPayloadWithString(text: NSString; locale: NSLocale): Pointer; cdecl;
    {class} function wellKnownTypeURIPayloadWithString(uri: NSString): Pointer; cdecl;
    {class} function wellKnownTypeURIPayloadWithURL(url: NSURL): Pointer; cdecl;
    {class} function wellKnowTypeTextPayloadWithString(text: NSString; locale: NSLocale): Pointer; cdecl; // API_DEPRECATED_WITH_REPLACEMENT("-wellKnownTypeTextPayloadWithString:locale", ios(13.0, 13.0))
  end;

  NFCNDEFPayload = interface(NSObject)
    ['{974E7EDE-873C-4ED7-AD8E-907B8DA7BC5C}']
    function &type: NSData; cdecl;
    function identifier: NSData; cdecl;
    function initWithFormat(format: NFCTypeNameFormat; &type: NSData; identifier: NSData; payload: NSData; chunkSize: NativeUInt): Pointer; overload; cdecl;
    function initWithFormat(format: NFCTypeNameFormat; &type: NSData; identifier: NSData; payload: NSData): Pointer; overload; cdecl;
    function payload: NSData; cdecl;
    procedure setIdentifier(identifier: NSData); cdecl;
    procedure setPayload(payload: NSData); cdecl;
    procedure setType(&type: NSData); cdecl;
    procedure setTypeNameFormat(typeNameFormat: NFCTypeNameFormat); cdecl;
    function typeNameFormat: NFCTypeNameFormat; cdecl;
    function wellKnownTypeTextPayloadWithLocale(locale: PNSLocale): NSString; cdecl;
    function wellKnownTypeURIPayload: NSURL; cdecl;
  end;
  TNFCNDEFPayload = class(TOCGenericImport<NFCNDEFPayloadClass, NFCNDEFPayload>) end;

  NFCNDEFMessageClass = interface(NSObjectClass)
    ['{895C146F-9CC9-4212-BBA9-F07EAA7D9569}']
    {class} function ndefMessageWithData(data: NSData): Pointer; cdecl;
  end;

  NFCNDEFMessage = interface(NSObject)
    ['{8191C72C-C9B3-4AA5-A4C6-A0B21D0A9EB3}']
    function initWithNDEFRecords(records: NSArray): Pointer; cdecl;
    function length: NSUInteger; cdecl;
    function records: NSArray; cdecl;
    procedure setRecords(records: NSArray); cdecl;
  end;
  TNFCNDEFMessage = class(TOCGenericImport<NFCNDEFMessageClass, NFCNDEFMessage>) end;

  NFCVASCommandConfigurationClass = interface(NSObjectClass)
    ['{93550C22-B034-442C-BA2B-8C70CED0F8B8}']
  end;

  NFCVASCommandConfiguration = interface(NSObject)
    ['{F39594F9-9079-47E0-BA1E-E09630BF33F1}']
    function initWithVASMode(mode: NFCVASMode; passTypeIdentifier: NSString; url: NSURL): Pointer; cdecl;
    function mode: NFCVASMode; cdecl;
    function passTypeIdentifier: NSString; cdecl;
    procedure setMode(mode: NFCVASMode); cdecl;
    procedure setPassTypeIdentifier(passTypeIdentifier: NSString); cdecl;
    procedure setUrl(url: NSURL); cdecl;
    function url: NSURL; cdecl;
  end;
  TNFCVASCommandConfiguration = class(TOCGenericImport<NFCVASCommandConfigurationClass, NFCVASCommandConfiguration>) end;

  NFCVASResponseClass = interface(NSObjectClass)
    ['{394E1A9B-A6D6-46F5-B8CB-AC69422C06BA}']
  end;

  NFCVASResponse = interface(NSObject)
    ['{A807C402-05F0-4C24-93B8-B91276D71100}']
    function mobileToken: NSData; cdecl;
    function status: NFCVASErrorCode; cdecl;
    function vasData: NSData; cdecl;
  end;
  TNFCVASResponse = class(TOCGenericImport<NFCVASResponseClass, NFCVASResponse>) end;

  NFCVASReaderSessionDelegate = interface(IObjectiveC)
    ['{12500345-A98A-47B7-BE24-40F39FD14471}']
    procedure readerSession(session: NFCVASReaderSession; didReceiveVASResponses: NSArray); overload; cdecl;
    procedure readerSession(session: NFCVASReaderSession; didInvalidateWithError: NSError); overload; cdecl;
    procedure readerSessionDidBecomeActive(session: NFCVASReaderSession); cdecl;
  end;

  NFCVASReaderSessionClass = interface(NFCReaderSessionClass)
    ['{9C9428F8-4B3B-42C1-83A7-7E661B100780}']
  end;

  NFCVASReaderSession = interface(NFCReaderSession)
    ['{4400E772-4426-4D1C-B92A-A4FF59D7302D}']
    function initWithVASCommandConfigurations(commandConfigurations: NSArray; delegate: Pointer; queue: dispatch_queue_t): Pointer; cdecl;
  end;
  TNFCVASReaderSession = class(TOCGenericImport<NFCVASReaderSessionClass, NFCVASReaderSession>) end;

function NFCErrorDomain: NSErrorDomain;
function NFCISO15693TagResponseErrorKey: NSString;
function NFCTagResponseUnexpectedLengthErrorKey: NSString;

const
  libCoreNFC = '/System/Library/Frameworks/CoreNFC.framework/CoreNFC';

implementation

uses
  Posix.Dlfcn;

var
  CoreNFCModule: THandle;

function NFCErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libCoreNFC, 'NFCErrorDomain');
end;

function NFCISO15693TagResponseErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreNFC, 'NFCISO15693TagResponseErrorKey');
end;

function NFCTagResponseUnexpectedLengthErrorKey: NSString;
begin
  Result := CocoaNSStringConst(libCoreNFC, 'NFCTagResponseUnexpectedLengthErrorKey');
end;

initialization
  CoreNFCModule := dlopen(MarshaledAString(libCoreNFC), RTLD_LAZY);

finalization
  dlclose(CoreNFCModule);

end.