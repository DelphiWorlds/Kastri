unit DW.Androidapi.JNI.EMDK;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net,
  Androidapi.JNI.Os;

type
  JEMDKBase = interface;
  JEMDKManager = interface;
  JEMDKManager_EMDKListener = interface;
  JEMDKManager_FEATURE_TYPE = interface;
  JEMDKManager_StatusData = interface;
  JEMDKManager_StatusListener = interface;
  JEMDKResults = interface;
  JEMDKResults_EXTENDED_STATUS_CODE = interface;
  JEMDKResults_STATUS_CODE = interface;
  JBarcodeManager = interface;
  JBarcodeManager_ConnectionState = interface;
  JBarcodeManager_DeviceIdentifier = interface;
  JBarcodeManager_ScannerConnectionListener = interface;
  JScanDataCollection = interface;
  JScanDataCollection_LabelType = interface;
  JScanDataCollection_ScanData = interface;
  Jbarcode_Scanner = interface;
  JScanner_DataListener = interface;
  JScanner_StatusListener = interface;
  JScanner_TriggerType = interface;
  JScannerException = interface;
  JScannerResults = interface;
  JStatusData = interface;
  JStatusData_ScannerStates = interface;

  JEMDKBaseClass = interface(JObjectClass)
    ['{37740510-2F94-480B-81D5-0615CB88945D}']
  end;

  [JavaSignature('com/symbol/emdk/EMDKBase')]
  JEMDKBase = interface(JObject)
    ['{AA17013B-FDC8-44EB-8F97-9799E0FB49E1}']
    function getType: JEMDKManager_FEATURE_TYPE; cdecl;
  end;
  TJEMDKBase = class(TJavaGenericImport<JEMDKBaseClass, JEMDKBase>) end;

  JEMDKManagerClass = interface(JObjectClass)
    ['{F4F6C087-03CB-4200-A64D-642BFC055942}']
    {class} function getEMDKManager(context: JContext; listener: JEMDKManager_EMDKListener): JEMDKResults; cdecl;
  end;

  [JavaSignature('com/symbol/emdk/EMDKManager')]
  JEMDKManager = interface(JObject)
    ['{AA56EF08-EF56-4603-8A8E-CDEA50513443}']
    function getInstance(featureType: JEMDKManager_FEATURE_TYPE): JEMDKBase; cdecl;
    procedure release; cdecl; overload;
    procedure release(featureType: JEMDKManager_FEATURE_TYPE); cdecl; overload;
  end;
  TJEMDKManager = class(TJavaGenericImport<JEMDKManagerClass, JEMDKManager>) end;

  JEMDKResultsClass = interface(JObjectClass)
    ['{F8190B95-DA31-44A8-90E8-F8F43D381F30}']
  end;

  JEMDKManager_EMDKListenerClass = interface(IJavaClass)
    ['{8C37F8D3-8C58-4F0A-954D-B8C0BA178AE2}']
  end;

  [JavaSignature('com/symbol/emdk/EMDKManager$EMDKListener')]
  JEMDKManager_EMDKListener = interface(IJavaInstance)
    ['{516A65CA-FAE4-43DD-9306-6A1BA3F34FF2}']
    procedure onClosed; cdecl;
    procedure onOpened(manager: JEMDKManager); cdecl;
  end;
  TJEMDKManager_EMDKListener = class(TJavaGenericImport<JEMDKManager_EMDKListenerClass, JEMDKManager_EMDKListener>) end;

  JEMDKManager_FEATURE_TYPEClass = interface(JEnumClass)
    ['{33C6053D-52CB-4239-BB6D-07DDF35CE573}']
    {class} function _GetBARCODE: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetNOTIFICATION: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetPAYMENT: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetPERSONALSHOPPER: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetPROFILE: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetSCANANDPAIR: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetSECURENFC: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetSERIALCOMM: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetSIMULSCAN: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function _GetVERSION: JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function valueOf(key: JString): JEMDKManager_FEATURE_TYPE; cdecl;
    {class} function values: TJavaObjectArray<JEMDKManager_FEATURE_TYPE>; cdecl;
    {class} property BARCODE: JEMDKManager_FEATURE_TYPE read _GetBARCODE;
    {class} property NOTIFICATION: JEMDKManager_FEATURE_TYPE read _GetNOTIFICATION;
    {class} property PAYMENT: JEMDKManager_FEATURE_TYPE read _GetPAYMENT;
    {class} property PERSONALSHOPPER: JEMDKManager_FEATURE_TYPE read _GetPERSONALSHOPPER;
    {class} property PROFILE: JEMDKManager_FEATURE_TYPE read _GetPROFILE;
    {class} property SCANANDPAIR: JEMDKManager_FEATURE_TYPE read _GetSCANANDPAIR;
    {class} property SECURENFC: JEMDKManager_FEATURE_TYPE read _GetSECURENFC;
    {class} property SERIALCOMM: JEMDKManager_FEATURE_TYPE read _GetSERIALCOMM;
    {class} property SIMULSCAN: JEMDKManager_FEATURE_TYPE read _GetSIMULSCAN;
    {class} property VERSION: JEMDKManager_FEATURE_TYPE read _GetVERSION;
  end;

  [JavaSignature('com/symbol/emdk/EMDKManager$FEATURE_TYPE')]
  JEMDKManager_FEATURE_TYPE = interface(JEnum)
    ['{C7A2C599-7FFF-4A61-8976-33AA8FE07A22}']
  end;
  TJEMDKManager_FEATURE_TYPE = class(TJavaGenericImport<JEMDKManager_FEATURE_TYPEClass, JEMDKManager_FEATURE_TYPE>) end;

  JEMDKManager_StatusDataClass = interface(JObjectClass)
    ['{5485B6AF-780D-4934-A21C-2395A4E6C56B}']
  end;

  [JavaSignature('com/symbol/emdk/EMDKManager$StatusData')]
  JEMDKManager_StatusData = interface(JObject)
    ['{59E4B228-9AA5-4AAD-A738-A41FBDD4313B}']
    function getFeatureType: JEMDKManager_FEATURE_TYPE; cdecl;
    function getResult: JEMDKResults_STATUS_CODE; cdecl;
  end;
  TJEMDKManager_StatusData = class(TJavaGenericImport<JEMDKManager_StatusDataClass, JEMDKManager_StatusData>) end;

  JEMDKManager_StatusListenerClass = interface(IJavaClass)
    ['{9DF82353-A55F-4A4A-BED1-7B1F646A2B31}']
  end;

  [JavaSignature('com/symbol/emdk/EMDKManager$StatusListener')]
  JEMDKManager_StatusListener = interface(IJavaInstance)
    ['{26FA534E-79A8-4CCF-B5DB-E6F5166E2268}']
    procedure onStatus(statusData: JEMDKManager_StatusData; emdk: JEMDKBase); cdecl;
  end;
  TJEMDKManager_StatusListener = class(TJavaGenericImport<JEMDKManager_StatusListenerClass, JEMDKManager_StatusListener>) end;

  [JavaSignature('com/symbol/emdk/EMDKResults')]
  JEMDKResults = interface(JObject)
    ['{A0F49913-A0D3-4A90-837B-305771D9AE6B}']
    function _GetextendedStatusCode: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    function _GetstatusCode: JEMDKResults_STATUS_CODE; cdecl;
    procedure _SetextendedStatusCode(value: JEMDKResults_EXTENDED_STATUS_CODE); cdecl;
    procedure _SetstatusCode(value: JEMDKResults_STATUS_CODE); cdecl;
    function getExtendedStatusMessage: JString; cdecl;
    function getStatusString: JString; cdecl;
    function getSuccessFeaturesCount: Integer; cdecl;
    function getTotalFeaturesCount: Integer; cdecl;
    property extendedStatusCode: JEMDKResults_EXTENDED_STATUS_CODE read _GetextendedStatusCode write _SetextendedStatusCode;
    property statusCode: JEMDKResults_STATUS_CODE read _GetstatusCode write _SetstatusCode;
  end;
  TJEMDKResults = class(TJavaGenericImport<JEMDKResultsClass, JEMDKResults>) end;

  JEMDKResults_EXTENDED_STATUS_CODEClass = interface(JEnumClass)
    ['{C4009469-C7B0-42EA-AA69-395F61292593}']
    {class} function _GetACTIVITY_SELECTION_MERGING_NOT_SUPPORTED: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetAPP_NOT_ALLOWED_TO_SUBMIT_XML: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetDEPENDACY_COMPONENT_FAILURE: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetFEATURE_NAME_NOT_FOUND_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetFEATURE_NAME_NOT_FOUND_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetFEATURE_NOT_UNIQUE_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetFEATURE_NOT_UNIQUE_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetFEATURE_TYPE_NOT_FOUND_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetFEATURE_TYPE_NOT_FOUND_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetGENERAL_EXCEPTION_OCCURED: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetINVALID_PROFILE_CONFIGURATION: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetINVALID_VALUE: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetNAMEVALUE_MISMATCH_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetNAMEVALUE_MISSMATCH_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetNONE: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetPROFILE_NAME_FORMAT_ERROR: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetPROFILE_NOT_FOUND_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function _GetPROFILE_NOT_FOUND_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function valueOf(key: JString): JEMDKResults_EXTENDED_STATUS_CODE; cdecl;
    {class} function values: TJavaObjectArray<JEMDKResults_EXTENDED_STATUS_CODE>; cdecl;
    {class} property ACTIVITY_SELECTION_MERGING_NOT_SUPPORTED: JEMDKResults_EXTENDED_STATUS_CODE read _GetACTIVITY_SELECTION_MERGING_NOT_SUPPORTED;
    {class} property APP_NOT_ALLOWED_TO_SUBMIT_XML: JEMDKResults_EXTENDED_STATUS_CODE read _GetAPP_NOT_ALLOWED_TO_SUBMIT_XML;
    {class} property DEPENDACY_COMPONENT_FAILURE: JEMDKResults_EXTENDED_STATUS_CODE read _GetDEPENDACY_COMPONENT_FAILURE;
    {class} property FEATURE_NAME_NOT_FOUND_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE read _GetFEATURE_NAME_NOT_FOUND_IN_CONFIG;
    {class} property FEATURE_NAME_NOT_FOUND_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE read _GetFEATURE_NAME_NOT_FOUND_IN_EXTRADATA;
    {class} property FEATURE_NOT_UNIQUE_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE read _GetFEATURE_NOT_UNIQUE_IN_CONFIG;
    {class} property FEATURE_NOT_UNIQUE_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE read _GetFEATURE_NOT_UNIQUE_IN_EXTRADATA;
    {class} property FEATURE_TYPE_NOT_FOUND_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE read _GetFEATURE_TYPE_NOT_FOUND_IN_CONFIG;
    {class} property FEATURE_TYPE_NOT_FOUND_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE read _GetFEATURE_TYPE_NOT_FOUND_IN_EXTRADATA;
    {class} property GENERAL_EXCEPTION_OCCURED: JEMDKResults_EXTENDED_STATUS_CODE read _GetGENERAL_EXCEPTION_OCCURED;
    {class} property INVALID_PROFILE_CONFIGURATION: JEMDKResults_EXTENDED_STATUS_CODE read _GetINVALID_PROFILE_CONFIGURATION;
    {class} property INVALID_VALUE: JEMDKResults_EXTENDED_STATUS_CODE read _GetINVALID_VALUE;
    {class} property NAMEVALUE_MISMATCH_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE read _GetNAMEVALUE_MISMATCH_IN_EXTRADATA;
    {class} property NAMEVALUE_MISSMATCH_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE read _GetNAMEVALUE_MISSMATCH_IN_CONFIG;
    {class} property NONE: JEMDKResults_EXTENDED_STATUS_CODE read _GetNONE;
    {class} property PROFILE_NAME_FORMAT_ERROR: JEMDKResults_EXTENDED_STATUS_CODE read _GetPROFILE_NAME_FORMAT_ERROR;
    {class} property PROFILE_NOT_FOUND_IN_CONFIG: JEMDKResults_EXTENDED_STATUS_CODE read _GetPROFILE_NOT_FOUND_IN_CONFIG;
    {class} property PROFILE_NOT_FOUND_IN_EXTRADATA: JEMDKResults_EXTENDED_STATUS_CODE read _GetPROFILE_NOT_FOUND_IN_EXTRADATA;
  end;

  [JavaSignature('com/symbol/emdk/EMDKResults$EXTENDED_STATUS_CODE')]
  JEMDKResults_EXTENDED_STATUS_CODE = interface(JEnum)
    ['{2087B607-E224-410E-81C6-5089E9EC3A9E}']
  end;
  TJEMDKResults_EXTENDED_STATUS_CODE = class(TJavaGenericImport<JEMDKResults_EXTENDED_STATUS_CODEClass, JEMDKResults_EXTENDED_STATUS_CODE>) end;

  JEMDKResults_STATUS_CODEClass = interface(JEnumClass)
    ['{054AD540-AA3D-476E-A433-7E5B6C1BDB0F}']
    {class} function _GetCHECK_XML: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetEMDK_NOT_OPENED: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetEMPTY_PROFILENAME: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetFAILURE: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetFEATURE_NOT_READY_TO_USE: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetFEATURE_NOT_SUPPORTED: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetNO_DATA_LISTENER: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetNULL_POINTER: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetPREVIOUS_REQUEST_IN_PROGRESS: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetPROCESSING: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetSUCCESS: JEMDKResults_STATUS_CODE; cdecl;
    {class} function _GetUNKNOWN: JEMDKResults_STATUS_CODE; cdecl;
    {class} function valueOf(key: JString): JEMDKResults_STATUS_CODE; cdecl;
    {class} function values: TJavaObjectArray<JEMDKResults_STATUS_CODE>; cdecl;
    {class} property CHECK_XML: JEMDKResults_STATUS_CODE read _GetCHECK_XML;
    {class} property EMDK_NOT_OPENED: JEMDKResults_STATUS_CODE read _GetEMDK_NOT_OPENED;
    {class} property EMPTY_PROFILENAME: JEMDKResults_STATUS_CODE read _GetEMPTY_PROFILENAME;
    {class} property FAILURE: JEMDKResults_STATUS_CODE read _GetFAILURE;
    {class} property FEATURE_NOT_READY_TO_USE: JEMDKResults_STATUS_CODE read _GetFEATURE_NOT_READY_TO_USE;
    {class} property FEATURE_NOT_SUPPORTED: JEMDKResults_STATUS_CODE read _GetFEATURE_NOT_SUPPORTED;
    {class} property NO_DATA_LISTENER: JEMDKResults_STATUS_CODE read _GetNO_DATA_LISTENER;
    {class} property NULL_POINTER: JEMDKResults_STATUS_CODE read _GetNULL_POINTER;
    {class} property PREVIOUS_REQUEST_IN_PROGRESS: JEMDKResults_STATUS_CODE read _GetPREVIOUS_REQUEST_IN_PROGRESS;
    {class} property PROCESSING: JEMDKResults_STATUS_CODE read _GetPROCESSING;
    {class} property SUCCESS: JEMDKResults_STATUS_CODE read _GetSUCCESS;
    {class} property UNKNOWN: JEMDKResults_STATUS_CODE read _GetUNKNOWN;
  end;

  [JavaSignature('com/symbol/emdk/EMDKResults$STATUS_CODE')]
  JEMDKResults_STATUS_CODE = interface(JEnum)
    ['{EE0678B0-2721-461F-8FA3-7ACBFE96F9F3}']
  end;
  TJEMDKResults_STATUS_CODE = class(TJavaGenericImport<JEMDKResults_STATUS_CODEClass, JEMDKResults_STATUS_CODE>) end;

  JBarcodeManagerClass = interface(JEMDKBaseClass)
    ['{D707AE9E-E532-416B-B595-855F2C597A6D}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/BarcodeManager')]
  JBarcodeManager = interface(JEMDKBase)
    ['{8FA8EE1C-45A4-4267-AE58-4B9069D45DEB}']
    procedure addConnectionListener(listener: JBarcodeManager_ScannerConnectionListener); cdecl;
    function getDevice(identifier: JBarcodeManager_DeviceIdentifier): Jbarcode_Scanner; cdecl; // overload;
    // function getDevice(info: JScannerInfo): Jbarcode_Scanner; cdecl; overload;
    function getSupportedDevicesInfo: JList; cdecl;
    procedure removeConnectionListener(P1: JBarcodeManager_ScannerConnectionListener); cdecl;
  end;
  TJBarcodeManager = class(TJavaGenericImport<JBarcodeManagerClass, JBarcodeManager>) end;

  JBarcodeManager_ConnectionStateClass = interface(JEnumClass)
    ['{BAEB9CAB-C99E-4A3C-90D8-6179D917AEE3}']
    {class} function _GetCONNECTED: JBarcodeManager_ConnectionState; cdecl;
    {class} function _GetDISCONNECTED: JBarcodeManager_ConnectionState; cdecl;
    {class} function valueOf(key: JString): JBarcodeManager_ConnectionState; cdecl;
    {class} function values: TJavaObjectArray<JBarcodeManager_ConnectionState>; cdecl;
    {class} property CONNECTED: JBarcodeManager_ConnectionState read _GetCONNECTED;
    {class} property DISCONNECTED: JBarcodeManager_ConnectionState read _GetDISCONNECTED;
  end;

  [JavaSignature('com/symbol/emdk/barcode/BarcodeManager$ConnectionState')]
  JBarcodeManager_ConnectionState = interface(JEnum)
    ['{CF50B53A-5D58-4933-A37A-4DE42BBA6570}']
  end;
  TJBarcodeManager_ConnectionState = class(TJavaGenericImport<JBarcodeManager_ConnectionStateClass, JBarcodeManager_ConnectionState>) end;

  JBarcodeManager_DeviceIdentifierClass = interface(JEnumClass)
    ['{7C0492DD-3A60-401B-977E-B350E3CA34A3}']
    {class} function _GetBLUETOOTH_IMAGER1: JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function _GetBLUETOOTH_IMAGER_RS6000: JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function _GetDEFAULT: JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function _GetINTERNAL_CAMERA1: JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function _GetINTERNAL_IMAGER1: JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function _GetINTERNAL_LASER1: JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function _GetPLUGGABLE_LASER1: JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function _GetUNDEFINED: JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function valueOf(key: JString): JBarcodeManager_DeviceIdentifier; cdecl;
    {class} function values: TJavaObjectArray<JBarcodeManager_DeviceIdentifier>; cdecl;
    {class} property BLUETOOTH_IMAGER1: JBarcodeManager_DeviceIdentifier read _GetBLUETOOTH_IMAGER1;
    {class} property BLUETOOTH_IMAGER_RS6000: JBarcodeManager_DeviceIdentifier read _GetBLUETOOTH_IMAGER_RS6000;
    {class} property DEFAULT: JBarcodeManager_DeviceIdentifier read _GetDEFAULT;
    {class} property INTERNAL_CAMERA1: JBarcodeManager_DeviceIdentifier read _GetINTERNAL_CAMERA1;
    {class} property INTERNAL_IMAGER1: JBarcodeManager_DeviceIdentifier read _GetINTERNAL_IMAGER1;
    {class} property INTERNAL_LASER1: JBarcodeManager_DeviceIdentifier read _GetINTERNAL_LASER1;
    {class} property PLUGGABLE_LASER1: JBarcodeManager_DeviceIdentifier read _GetPLUGGABLE_LASER1;
    {class} property UNDEFINED: JBarcodeManager_DeviceIdentifier read _GetUNDEFINED;
  end;

  [JavaSignature('com/symbol/emdk/barcode/BarcodeManager$DeviceIdentifier')]
  JBarcodeManager_DeviceIdentifier = interface(JEnum)
    ['{5E6C7E45-6975-4BD7-8B67-5F65D44647C8}']
  end;
  TJBarcodeManager_DeviceIdentifier = class(TJavaGenericImport<JBarcodeManager_DeviceIdentifierClass, JBarcodeManager_DeviceIdentifier>) end;

  JBarcodeManager_ScannerConnectionListenerClass = interface(IJavaClass)
    ['{D8512ACE-3020-4247-AB7D-FB826C464212}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/BarcodeManager$ScannerConnectionListener')]
  JBarcodeManager_ScannerConnectionListener = interface(IJavaInstance)
    ['{D4B603B3-CDEA-45B9-BBE4-0456247578F3}']
    // procedure onConnectionChange(info: JScannerInfo; state: JBarcodeManager_ConnectionState); cdecl;
  end;
  TJBarcodeManager_ScannerConnectionListener = class(TJavaGenericImport<JBarcodeManager_ScannerConnectionListenerClass, JBarcodeManager_ScannerConnectionListener>) end;

  JInterfaceConfigClass = interface(JObjectClass)
    ['{BBC65BD8-1B2D-4DA9-AF0C-EC7C65E60C2E}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/InterfaceConfig')]
  JInterfaceConfig = interface(JObject)
    ['{82BEB4C5-665A-4977-A7CB-0BF6E9EF4CD4}']
    function _GetconnectionEstablishTime: Integer; cdecl;
    function _GetdisplayBluetoothAddressBarcode: Boolean; cdecl;
    procedure _SetconnectionEstablishTime(value: Integer); cdecl;
    procedure _SetdisplayBluetoothAddressBarcode(value: Boolean); cdecl;
    property connectionEstablishTime: Integer read _GetconnectionEstablishTime write _SetconnectionEstablishTime;
    property displayBluetoothAddressBarcode: Boolean read _GetdisplayBluetoothAddressBarcode write _SetdisplayBluetoothAddressBarcode;
  end;
  TJInterfaceConfig = class(TJavaGenericImport<JInterfaceConfigClass, JInterfaceConfig>) end;

  JScanDataCollectionClass = interface(JObjectClass)
    ['{CB2291DF-EC10-4FBB-8F64-E0D5F6FD09B7}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/ScanDataCollection')]
  JScanDataCollection = interface(JObject)
    ['{4906EB82-59F8-485A-BF49-F38BA3D5ADAC}']
    function getFriendlyName: JString; cdecl;
    function getResult: JScannerResults; cdecl;
    function getScanData: JArrayList; cdecl;
  end;
  TJScanDataCollection = class(TJavaGenericImport<JScanDataCollectionClass, JScanDataCollection>) end;

  JScanDataCollection_LabelTypeClass = interface(JEnumClass)
    ['{8CDB2293-49EE-4897-80A8-0AA941685DDB}']
    {class} function _GetAUSPOSTAL: JScanDataCollection_LabelType; cdecl;
    {class} function _GetAZTEC: JScanDataCollection_LabelType; cdecl;
    {class} function _GetBOOKLAND: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCANPOSTAL: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCHINESE_2OF5: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCODABAR: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCODE11: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCODE128: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCODE32: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCODE39: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCODE93: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCOMPOSITE_AB: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCOMPOSITE_C: JScanDataCollection_LabelType; cdecl;
    {class} function _GetCOUPON: JScanDataCollection_LabelType; cdecl;
    {class} function _GetD2OF5: JScanDataCollection_LabelType; cdecl;
    {class} function _GetDATABAR_COUPON: JScanDataCollection_LabelType; cdecl;
    {class} function _GetDATAMATRIX: JScanDataCollection_LabelType; cdecl;
    {class} function _GetDUTCHPOSTAL: JScanDataCollection_LabelType; cdecl;
    {class} function _GetEAN128: JScanDataCollection_LabelType; cdecl;
    {class} function _GetEAN13: JScanDataCollection_LabelType; cdecl;
    {class} function _GetEAN8: JScanDataCollection_LabelType; cdecl;
    {class} function _GetGS1_DATABAR: JScanDataCollection_LabelType; cdecl;
    {class} function _GetGS1_DATABAR_EXP: JScanDataCollection_LabelType; cdecl;
    {class} function _GetGS1_DATABAR_LIM: JScanDataCollection_LabelType; cdecl;
    {class} function _GetHANXIN: JScanDataCollection_LabelType; cdecl;
    {class} function _GetI2OF5: JScanDataCollection_LabelType; cdecl;
    {class} function _GetIATA2OF5: JScanDataCollection_LabelType; cdecl;
    {class} function _GetISBT128: JScanDataCollection_LabelType; cdecl;
    {class} function _GetJAPPOSTAL: JScanDataCollection_LabelType; cdecl;
    {class} function _GetKOREAN_3OF5: JScanDataCollection_LabelType; cdecl;
    {class} function _GetMAILMARK: JScanDataCollection_LabelType; cdecl;
    {class} function _GetMATRIX_2OF5: JScanDataCollection_LabelType; cdecl;
    {class} function _GetMAXICODE: JScanDataCollection_LabelType; cdecl;
    {class} function _GetMICROPDF: JScanDataCollection_LabelType; cdecl;
    {class} function _GetMICROQR: JScanDataCollection_LabelType; cdecl;
    {class} function _GetMSI: JScanDataCollection_LabelType; cdecl;
    {class} function _GetOCR: JScanDataCollection_LabelType; cdecl;
    {class} function _GetPDF417: JScanDataCollection_LabelType; cdecl;
    {class} function _GetQRCODE: JScanDataCollection_LabelType; cdecl;
    {class} function _GetSIGNATURE: JScanDataCollection_LabelType; cdecl;
    {class} function _GetTLC39: JScanDataCollection_LabelType; cdecl;
    {class} function _GetTRIOPTIC39: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUKPOSTAL: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUNDEFINED: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUPCA: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUPCE0: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUPCE1: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUS4STATE: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUS4STATE_FICS: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUSPLANET: JScanDataCollection_LabelType; cdecl;
    {class} function _GetUSPOSTNET: JScanDataCollection_LabelType; cdecl;
    {class} function _GetWEBCODE: JScanDataCollection_LabelType; cdecl;
    {class} function valueOf(key: JString): JScanDataCollection_LabelType; cdecl;
    {class} function values: TJavaObjectArray<JScanDataCollection_LabelType>; cdecl;
    {class} property AUSPOSTAL: JScanDataCollection_LabelType read _GetAUSPOSTAL;
    {class} property AZTEC: JScanDataCollection_LabelType read _GetAZTEC;
    {class} property BOOKLAND: JScanDataCollection_LabelType read _GetBOOKLAND;
    {class} property CANPOSTAL: JScanDataCollection_LabelType read _GetCANPOSTAL;
    {class} property CHINESE_2OF5: JScanDataCollection_LabelType read _GetCHINESE_2OF5;
    {class} property CODABAR: JScanDataCollection_LabelType read _GetCODABAR;
    {class} property CODE11: JScanDataCollection_LabelType read _GetCODE11;
    {class} property CODE128: JScanDataCollection_LabelType read _GetCODE128;
    {class} property CODE32: JScanDataCollection_LabelType read _GetCODE32;
    {class} property CODE39: JScanDataCollection_LabelType read _GetCODE39;
    {class} property CODE93: JScanDataCollection_LabelType read _GetCODE93;
    {class} property COMPOSITE_AB: JScanDataCollection_LabelType read _GetCOMPOSITE_AB;
    {class} property COMPOSITE_C: JScanDataCollection_LabelType read _GetCOMPOSITE_C;
    {class} property COUPON: JScanDataCollection_LabelType read _GetCOUPON;
    {class} property D2OF5: JScanDataCollection_LabelType read _GetD2OF5;
    {class} property DATABAR_COUPON: JScanDataCollection_LabelType read _GetDATABAR_COUPON;
    {class} property DATAMATRIX: JScanDataCollection_LabelType read _GetDATAMATRIX;
    {class} property DUTCHPOSTAL: JScanDataCollection_LabelType read _GetDUTCHPOSTAL;
    {class} property EAN128: JScanDataCollection_LabelType read _GetEAN128;
    {class} property EAN13: JScanDataCollection_LabelType read _GetEAN13;
    {class} property EAN8: JScanDataCollection_LabelType read _GetEAN8;
    {class} property GS1_DATABAR: JScanDataCollection_LabelType read _GetGS1_DATABAR;
    {class} property GS1_DATABAR_EXP: JScanDataCollection_LabelType read _GetGS1_DATABAR_EXP;
    {class} property GS1_DATABAR_LIM: JScanDataCollection_LabelType read _GetGS1_DATABAR_LIM;
    {class} property HANXIN: JScanDataCollection_LabelType read _GetHANXIN;
    {class} property I2OF5: JScanDataCollection_LabelType read _GetI2OF5;
    {class} property IATA2OF5: JScanDataCollection_LabelType read _GetIATA2OF5;
    {class} property ISBT128: JScanDataCollection_LabelType read _GetISBT128;
    {class} property JAPPOSTAL: JScanDataCollection_LabelType read _GetJAPPOSTAL;
    {class} property KOREAN_3OF5: JScanDataCollection_LabelType read _GetKOREAN_3OF5;
    {class} property MAILMARK: JScanDataCollection_LabelType read _GetMAILMARK;
    {class} property MATRIX_2OF5: JScanDataCollection_LabelType read _GetMATRIX_2OF5;
    {class} property MAXICODE: JScanDataCollection_LabelType read _GetMAXICODE;
    {class} property MICROPDF: JScanDataCollection_LabelType read _GetMICROPDF;
    {class} property MICROQR: JScanDataCollection_LabelType read _GetMICROQR;
    {class} property MSI: JScanDataCollection_LabelType read _GetMSI;
    {class} property OCR: JScanDataCollection_LabelType read _GetOCR;
    {class} property PDF417: JScanDataCollection_LabelType read _GetPDF417;
    {class} property QRCODE: JScanDataCollection_LabelType read _GetQRCODE;
    {class} property SIGNATURE: JScanDataCollection_LabelType read _GetSIGNATURE;
    {class} property TLC39: JScanDataCollection_LabelType read _GetTLC39;
    {class} property TRIOPTIC39: JScanDataCollection_LabelType read _GetTRIOPTIC39;
    {class} property UKPOSTAL: JScanDataCollection_LabelType read _GetUKPOSTAL;
    {class} property UNDEFINED: JScanDataCollection_LabelType read _GetUNDEFINED;
    {class} property UPCA: JScanDataCollection_LabelType read _GetUPCA;
    {class} property UPCE0: JScanDataCollection_LabelType read _GetUPCE0;
    {class} property UPCE1: JScanDataCollection_LabelType read _GetUPCE1;
    {class} property US4STATE: JScanDataCollection_LabelType read _GetUS4STATE;
    {class} property US4STATE_FICS: JScanDataCollection_LabelType read _GetUS4STATE_FICS;
    {class} property USPLANET: JScanDataCollection_LabelType read _GetUSPLANET;
    {class} property USPOSTNET: JScanDataCollection_LabelType read _GetUSPOSTNET;
    {class} property WEBCODE: JScanDataCollection_LabelType read _GetWEBCODE;
  end;

  [JavaSignature('com/symbol/emdk/barcode/ScanDataCollection$LabelType')]
  JScanDataCollection_LabelType = interface(JEnum)
    ['{17D0EEE7-C162-4625-929A-8373335FFEFC}']
  end;
  TJScanDataCollection_LabelType = class(TJavaGenericImport<JScanDataCollection_LabelTypeClass, JScanDataCollection_LabelType>) end;

  JScanDataCollection_ScanDataClass = interface(JObjectClass)
    ['{1151BA2B-20BB-4304-90A3-0F442CEBAFB5}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/ScanDataCollection$ScanData')]
  JScanDataCollection_ScanData = interface(JObject)
    ['{561A9D78-E0F5-42D5-A42A-458596CEFFC1}']
    function getData: JString; cdecl;
    function getLabelType: JScanDataCollection_LabelType; cdecl;
    function getRawData: TJavaArray<Byte>; cdecl;
    function getTimeStamp: JString; cdecl;
  end;
  TJScanDataCollection_ScanData = class(TJavaGenericImport<JScanDataCollection_ScanDataClass, JScanDataCollection_ScanData>) end;

  Jbarcode_ScannerClass = interface(JObjectClass)
    ['{5CF9C28D-BD12-49C0-B505-CACC2714EB8E}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/Scanner')]
  Jbarcode_Scanner = interface(JObject)
    ['{E3A16E14-6EFA-40C1-8586-E3368DCF6737}']
    function _GettriggerType: JScanner_TriggerType; cdecl;
    procedure _SettriggerType(Value: JScanner_TriggerType); cdecl;
    procedure addDataListener(listener: JScanner_DataListener); cdecl;
    procedure addStatusListener(listener: JScanner_StatusListener); cdecl;
    procedure cancelRead; cdecl;
    procedure disable; cdecl;
    procedure enable; cdecl;
    function getInterfaceConfig: JInterfaceConfig; cdecl;
    function isEnabled: Boolean; cdecl;
    function isReadPending: Boolean; cdecl;
    procedure read; cdecl;
    procedure release; cdecl;
    procedure removeDataListener(listener: JScanner_DataListener); cdecl;
    procedure removeStatusListener(listener: JScanner_StatusListener); cdecl;
    procedure setInterfaceConfig(config: JInterfaceConfig); cdecl;
    property triggerType: JScanner_TriggerType read _GettriggerType write _SettriggerType;
  end;
  TJbarcode_Scanner = class(TJavaGenericImport<Jbarcode_ScannerClass, Jbarcode_Scanner>) end;

  JScanner_DataListenerClass = interface(IJavaClass)
    ['{B1FE91F4-98FE-4E25-AFCE-EF76D2C139B1}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/Scanner$DataListener')]
  JScanner_DataListener = interface(IJavaInstance)
    ['{62D20381-08EF-4205-8F78-E30944C6D14D}']
    procedure onData(dataCollection: JScanDataCollection); cdecl;
  end;
  TJScanner_DataListener = class(TJavaGenericImport<JScanner_DataListenerClass, JScanner_DataListener>) end;

  JScanner_StatusListenerClass = interface(IJavaClass)
    ['{E223B949-A1DF-4BC6-911C-8E22646DD852}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/Scanner$StatusListener')]
  JScanner_StatusListener = interface(IJavaInstance)
    ['{CCD85D33-2819-47FC-B935-A29DC4AF7A93}']
    procedure onStatus(statusData: JStatusData); cdecl;
  end;
  TJScanner_StatusListener = class(TJavaGenericImport<JScanner_StatusListenerClass, JScanner_StatusListener>) end;

  JScanner_TriggerTypeClass = interface(JEnumClass)
    ['{0AEDBF35-CA94-47AF-81E9-9D0ADC2A8D2F}']
    {class} function _GetHARD: JScanner_TriggerType; cdecl;
    {class} function _GetSOFT_ALWAYS: JScanner_TriggerType; cdecl;
    {class} function _GetSOFT_ONCE: JScanner_TriggerType; cdecl;
    {class} function valueOf(key: JString): JScanner_TriggerType; cdecl;
    {class} function values: TJavaObjectArray<JScanner_TriggerType>; cdecl;
    {class} property HARD: JScanner_TriggerType read _GetHARD;
    {class} property SOFT_ALWAYS: JScanner_TriggerType read _GetSOFT_ALWAYS;
    {class} property SOFT_ONCE: JScanner_TriggerType read _GetSOFT_ONCE;
  end;

  [JavaSignature('com/symbol/emdk/barcode/Scanner$TriggerType')]
  JScanner_TriggerType = interface(JEnum)
    ['{D783D40A-909D-44C4-97B1-03C1045CEF54}']
  end;
  TJScanner_TriggerType = class(TJavaGenericImport<JScanner_TriggerTypeClass, JScanner_TriggerType>) end;

  JScannerExceptionClass = interface(JExceptionClass)
    ['{E1815F48-E0FB-4A4E-8CD6-EC2F9DE6D02B}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/ScannerException')]
  JScannerException = interface(JException)
    ['{5EAFDC4C-75CC-4DCD-AF30-59583DF1198B}']
    function getResult: JScannerResults; cdecl;
  end;
  TJScannerException = class(TJavaGenericImport<JScannerExceptionClass, JScannerException>) end;

  JScannerResultsClass = interface(JEnumClass)
    ['{D4875F60-5C56-4A3C-AF45-A492C6BEADAC}']
    {class} function _GetALREADY_SCANNING: JScannerResults; cdecl;
    {class} function _GetFAILURE: JScannerResults; cdecl;
    {class} function _GetFEATURE_NOT_SUPPORTED: JScannerResults; cdecl;
    {class} function _GetINVALID_OBJECT: JScannerResults; cdecl;
    {class} function _GetINVALID_VALUE: JScannerResults; cdecl;
    {class} function _GetLENGTH_MISMATCH: JScannerResults; cdecl;
    {class} function _GetNO_DATA_LISTENER: JScannerResults; cdecl;
    {class} function _GetSCANNER_DEINIT_FAILURE: JScannerResults; cdecl;
    {class} function _GetSCANNER_INIT_FAILURE: JScannerResults; cdecl;
    {class} function _GetSCANNER_IN_USE: JScannerResults; cdecl;
    {class} function _GetSCANNER_NOT_CONNECTED: JScannerResults; cdecl;
    {class} function _GetSCANNER_NOT_ENABLED: JScannerResults; cdecl;
    {class} function _GetSCANNER_NOT_SUPPORTED: JScannerResults; cdecl;
    {class} function _GetSCANNER_OPERATION_FAILURE: JScannerResults; cdecl;
    {class} function _GetSCANNER_TIMED_OUT: JScannerResults; cdecl;
    {class} function _GetSCAN_DATA_FAILURE: JScannerResults; cdecl;
    {class} function _GetSCAN_PARAM_NOT_SUPPORTED: JScannerResults; cdecl;
    {class} function _GetSCAN_PARAM_READ_ONLY: JScannerResults; cdecl;
    {class} function _GetSUCCESS: JScannerResults; cdecl;
    {class} function _GetTRIGGER_KEY_IN_USE: JScannerResults; cdecl;
    {class} function _GetTRIGGER_KEY_REG_FAILED: JScannerResults; cdecl;
    {class} function _GetTRIGGER_KEY_UNREG_FAILED: JScannerResults; cdecl;
    {class} function _GetUNDEFINED: JScannerResults; cdecl;
    {class} function _GetVF_ERROR: JScannerResults; cdecl;
    {class} function valueOf(key: JString): JScannerResults; cdecl;
    {class} function values: TJavaObjectArray<JScannerResults>; cdecl;
    {class} property ALREADY_SCANNING: JScannerResults read _GetALREADY_SCANNING;
    {class} property FAILURE: JScannerResults read _GetFAILURE;
    {class} property FEATURE_NOT_SUPPORTED: JScannerResults read _GetFEATURE_NOT_SUPPORTED;
    {class} property INVALID_OBJECT: JScannerResults read _GetINVALID_OBJECT;
    {class} property INVALID_VALUE: JScannerResults read _GetINVALID_VALUE;
    {class} property LENGTH_MISMATCH: JScannerResults read _GetLENGTH_MISMATCH;
    {class} property NO_DATA_LISTENER: JScannerResults read _GetNO_DATA_LISTENER;
    {class} property SCANNER_DEINIT_FAILURE: JScannerResults read _GetSCANNER_DEINIT_FAILURE;
    {class} property SCANNER_INIT_FAILURE: JScannerResults read _GetSCANNER_INIT_FAILURE;
    {class} property SCANNER_IN_USE: JScannerResults read _GetSCANNER_IN_USE;
    {class} property SCANNER_NOT_CONNECTED: JScannerResults read _GetSCANNER_NOT_CONNECTED;
    {class} property SCANNER_NOT_ENABLED: JScannerResults read _GetSCANNER_NOT_ENABLED;
    {class} property SCANNER_NOT_SUPPORTED: JScannerResults read _GetSCANNER_NOT_SUPPORTED;
    {class} property SCANNER_OPERATION_FAILURE: JScannerResults read _GetSCANNER_OPERATION_FAILURE;
    {class} property SCANNER_TIMED_OUT: JScannerResults read _GetSCANNER_TIMED_OUT;
    {class} property SCAN_DATA_FAILURE: JScannerResults read _GetSCAN_DATA_FAILURE;
    {class} property SCAN_PARAM_NOT_SUPPORTED: JScannerResults read _GetSCAN_PARAM_NOT_SUPPORTED;
    {class} property SCAN_PARAM_READ_ONLY: JScannerResults read _GetSCAN_PARAM_READ_ONLY;
    {class} property SUCCESS: JScannerResults read _GetSUCCESS;
    {class} property TRIGGER_KEY_IN_USE: JScannerResults read _GetTRIGGER_KEY_IN_USE;
    {class} property TRIGGER_KEY_REG_FAILED: JScannerResults read _GetTRIGGER_KEY_REG_FAILED;
    {class} property TRIGGER_KEY_UNREG_FAILED: JScannerResults read _GetTRIGGER_KEY_UNREG_FAILED;
    {class} property UNDEFINED: JScannerResults read _GetUNDEFINED;
    {class} property VF_ERROR: JScannerResults read _GetVF_ERROR;
  end;

  [JavaSignature('com/symbol/emdk/barcode/ScannerResults')]
  JScannerResults = interface(JEnum)
    ['{1121ECAC-03FB-4105-9E72-648D234C76C8}']
    function getDescription: JString; cdecl;
    function getValue: Integer; cdecl;
  end;
  TJScannerResults = class(TJavaGenericImport<JScannerResultsClass, JScannerResults>) end;

  JStatusDataClass = interface(JObjectClass)
    ['{6AA9444F-16E4-4DF9-8A74-2CCA592792B6}']
  end;

  [JavaSignature('com/symbol/emdk/barcode/StatusData')]
  JStatusData = interface(JObject)
    ['{CD36631D-2EC5-4484-969B-1C914AD49580}']
    function getFriendlyName: JString; cdecl;
    function getState: JStatusData_ScannerStates; cdecl;
  end;
  TJStatusData = class(TJavaGenericImport<JStatusDataClass, JStatusData>) end;

  JStatusData_ScannerStatesClass = interface(JEnumClass)
    ['{11BB1FAF-3B27-41DA-8D41-0EB4241CE86A}']
    {class} function _GetDISABLED: JStatusData_ScannerStates; cdecl;
    {class} function _GetERROR: JStatusData_ScannerStates; cdecl;
    {class} function _GetIDLE: JStatusData_ScannerStates; cdecl;
    {class} function _GetSCANNING: JStatusData_ScannerStates; cdecl;
    {class} function _GetWAITING: JStatusData_ScannerStates; cdecl;
    {class} function valueOf(key: JString): JStatusData_ScannerStates; cdecl;
    {class} function values: TJavaObjectArray<JStatusData_ScannerStates>; cdecl;
    {class} property DISABLED: JStatusData_ScannerStates read _GetDISABLED;
    {class} property ERROR: JStatusData_ScannerStates read _GetERROR;
    {class} property IDLE: JStatusData_ScannerStates read _GetIDLE;
    {class} property SCANNING: JStatusData_ScannerStates read _GetSCANNING;
    {class} property WAITING: JStatusData_ScannerStates read _GetWAITING;
  end;

  [JavaSignature('com/symbol/emdk/barcode/StatusData$ScannerStates')]
  JStatusData_ScannerStates = interface(JEnum)
    ['{5D01BF65-C0DB-4F2C-9B24-0ECD62B4434A}']
  end;
  TJStatusData_ScannerStates = class(TJavaGenericImport<JStatusData_ScannerStatesClass, JStatusData_ScannerStates>) end;

implementation

end.

