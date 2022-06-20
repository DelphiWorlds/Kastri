unit DW.Androidapi.JNI.DataCollection;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os;

type
  JIExecutor = interface;//com.honeywell.IExecutor
  Jhoneywell_Message = interface;//com.honeywell.Message
  JAidcException = interface;//com.honeywell.aidc.AidcException
  JAidcManager = interface;//com.honeywell.aidc.AidcManager
  JEventListener = interface;//java.util.EventListener
  JAidcManager_BarcodeDeviceListener = interface;//com.honeywell.aidc.AidcManager$BarcodeDeviceListener
  JAidcManager_CreatedCallback = interface;//com.honeywell.aidc.AidcManager$CreatedCallback
  JEventObject = interface;//java.util.EventObject
  JBarcodeDeviceConnectionEvent = interface;//com.honeywell.aidc.BarcodeDeviceConnectionEvent
  JBarcodeFailureEvent = interface;//com.honeywell.aidc.BarcodeFailureEvent
  JBarcodeReadEvent = interface;//com.honeywell.aidc.BarcodeReadEvent
  JBarcodeReader = interface;//com.honeywell.aidc.BarcodeReader
  JBarcodeReader_BarcodeListener = interface;//com.honeywell.aidc.BarcodeReader$BarcodeListener
  JBarcodeReader_TriggerListener = interface;//com.honeywell.aidc.BarcodeReader$TriggerListener
  JBarcodeReaderInfo = interface;//com.honeywell.aidc.BarcodeReaderInfo
  Jaidc_BuildConfig = interface;//com.honeywell.aidc.BuildConfig
  JDcsJsonRpcHelper = interface;//com.honeywell.aidc.DcsJsonRpcHelper
  JDebugLog = interface;//com.honeywell.aidc.DebugLog
  JDecodeIntent = interface;//com.honeywell.aidc.DecodeIntent
  JJsonUtil = interface;//com.honeywell.aidc.JsonUtil
  JScannerNotClaimedException = interface;//com.honeywell.aidc.ScannerNotClaimedException
  JScannerUnavailableException = interface;//com.honeywell.aidc.ScannerUnavailableException
  Jaidc_Signature = interface;//com.honeywell.aidc.Signature
  JSignatureParameters = interface;//com.honeywell.aidc.SignatureParameters
  JTriggerStateChangeEvent = interface;//com.honeywell.aidc.TriggerStateChangeEvent
  JUnsupportedPropertyException = interface;//com.honeywell.aidc.UnsupportedPropertyException

  JIExecutorClass = interface(JIInterfaceClass)
    ['{E3745EF9-792C-4008-895F-CE18524851BB}']
  end;

  [JavaSignature('com/honeywell/IExecutor')]
  JIExecutor = interface(JIInterface)
    ['{FEED9299-C080-46C4-88D5-B656FBBDF357}']
    function execute(message: Jhoneywell_Message): Jhoneywell_Message; cdecl;
    procedure executeAsync(message: Jhoneywell_Message; iExecutor: JIExecutor); cdecl;
  end;
  TJIExecutor = class(TJavaGenericImport<JIExecutorClass, JIExecutor>) end;

  Jhoneywell_MessageClass = interface(JParcelableClass)
    ['{BD6DAAFC-59E8-4B1E-AF0E-D1D6BD884F4B}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function init: Jhoneywell_Message; cdecl; overload;
    {class} function init(string_: JString): Jhoneywell_Message; cdecl; overload;
    {class} function init(string_: JString; map: JMap): Jhoneywell_Message; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('com/honeywell/Message')]
  Jhoneywell_Message = interface(JParcelable)
    ['{3C8A68DD-C4EE-4825-AAE6-5CFE6F37A8D9}']
    function _Getaction: JString; cdecl;
    procedure _Setaction(Value: JString); cdecl;
    function _Getextras: JMap; cdecl;
    procedure _Setextras(Value: JMap); cdecl;
    function describeContents: Integer; cdecl;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
    property action: JString read _Getaction write _Setaction;
    property extras: JMap read _Getextras write _Setextras;
  end;
  TJhoneywell_Message = class(TJavaGenericImport<Jhoneywell_MessageClass, Jhoneywell_Message>) end;

  JAidcExceptionClass = interface(JExceptionClass)
    ['{E53A7451-654B-4DED-85E4-795E11478B81}']
  end;

  [JavaSignature('com/honeywell/aidc/AidcException')]
  JAidcException = interface(JException)
    ['{65F41E26-6F0F-412A-8519-598E200D0629}']
  end;
  TJAidcException = class(TJavaGenericImport<JAidcExceptionClass, JAidcException>) end;

  JAidcManagerClass = interface(JObjectClass)
    ['{0363F1A9-957C-4908-BE2A-8182282C7821}']
    {class} function _GetBARCODE_DEVICE_CONNECTED: Integer; cdecl;
    {class} function _GetBARCODE_DEVICE_DISCONNECTED: Integer; cdecl;
    {class} procedure create(context: JContext; createdCallback: JAidcManager_CreatedCallback); cdecl;
    {class} function init(context: JContext; serviceConnection: JServiceConnection; iExecutor: JIExecutor): JAidcManager; cdecl;
    {class} property BARCODE_DEVICE_CONNECTED: Integer read _GetBARCODE_DEVICE_CONNECTED;
    {class} property BARCODE_DEVICE_DISCONNECTED: Integer read _GetBARCODE_DEVICE_DISCONNECTED;
  end;

  [JavaSignature('com/honeywell/aidc/AidcManager')]
  JAidcManager = interface(JObject)
    ['{612AF36C-2883-4B4E-81B4-106EEE87CE96}']
    procedure addBarcodeDeviceListener(barcodeDeviceListener: JAidcManager_BarcodeDeviceListener); cdecl;
    procedure close; cdecl;
    function createBarcodeReader: JBarcodeReader; cdecl; overload;
    function createBarcodeReader(string_: JString): JBarcodeReader; cdecl; overload;
    function execute(message: Jhoneywell_Message): Jhoneywell_Message; cdecl;
    procedure finalize; cdecl;
    function getContext: JContext; cdecl;
    function listBarcodeDevices: JList; cdecl;
    function listConnectedBarcodeDevices: JList; cdecl;
    procedure removeBarcodeDeviceListener(barcodeDeviceListener: JAidcManager_BarcodeDeviceListener); cdecl;
  end;
  TJAidcManager = class(TJavaGenericImport<JAidcManagerClass, JAidcManager>) end;

  JEventListenerClass = interface(IJavaClass)
    ['{48BD1D07-BCE3-4F68-982B-617B523C7116}']
  end;

  [JavaSignature('java/util/EventListener')]
  JEventListener = interface(IJavaInstance)
    ['{D9ADB67A-5217-4762-9961-710BF18CAD39}']
  end;
  TJEventListener = class(TJavaGenericImport<JEventListenerClass, JEventListener>) end;

  JAidcManager_BarcodeDeviceListenerClass = interface(JEventListenerClass)
    ['{BFD7FC0E-4603-4358-A691-27AE805D2E7A}']
  end;

  [JavaSignature('com/honeywell/aidc/AidcManager$BarcodeDeviceListener')]
  JAidcManager_BarcodeDeviceListener = interface(JEventListener)
    ['{A79921C7-20CE-48C8-A680-E95B75CE9639}']
    procedure onBarcodeDeviceConnectionEvent(barcodeDeviceConnectionEvent: JBarcodeDeviceConnectionEvent); cdecl;
  end;
  TJAidcManager_BarcodeDeviceListener = class(TJavaGenericImport<JAidcManager_BarcodeDeviceListenerClass, JAidcManager_BarcodeDeviceListener>) end;

  JAidcManager_CreatedCallbackClass = interface(IJavaClass)
    ['{CADBD466-15A1-455C-BBF7-B37F21453E91}']
  end;

  [JavaSignature('com/honeywell/aidc/AidcManager$CreatedCallback')]
  JAidcManager_CreatedCallback = interface(IJavaInstance)
    ['{487990B1-7751-4DAE-9C3D-AEB524A9726D}']
    procedure onCreated(aidcManager: JAidcManager); cdecl;
  end;
  TJAidcManager_CreatedCallback = class(TJavaGenericImport<JAidcManager_CreatedCallbackClass, JAidcManager_CreatedCallback>) end;

  JEventObjectClass = interface(JObjectClass)
    ['{7E0A105A-36FF-4B7F-B767-9EDE80FF9713}']
    {class} function init(source: JObject): JEventObject; cdecl;//Deprecated
  end;

  [JavaSignature('java/util/EventObject')]
  JEventObject = interface(JObject)
    ['{0585C0FC-A188-474F-AB06-7FF7FE50D868}']
    function getSource: JObject; cdecl;
    function toString: JString; cdecl;
  end;
  TJEventObject = class(TJavaGenericImport<JEventObjectClass, JEventObject>) end;

  JBarcodeDeviceConnectionEventClass = interface(JEventObjectClass)
    ['{365E50BE-1DD4-436C-A385-8D4EDE7E564B}']
  end;

  [JavaSignature('com/honeywell/aidc/BarcodeDeviceConnectionEvent')]
  JBarcodeDeviceConnectionEvent = interface(JEventObject)
    ['{9752EFE0-A506-4F3F-A5B2-8FE471D38DE0}']
    function getBarcodeReaderInfo: JBarcodeReaderInfo; cdecl;
    function getConnectionStatus: Integer; cdecl;
  end;
  TJBarcodeDeviceConnectionEvent = class(TJavaGenericImport<JBarcodeDeviceConnectionEventClass, JBarcodeDeviceConnectionEvent>) end;

  JBarcodeFailureEventClass = interface(JEventObjectClass)
    ['{751A3E44-BB51-4019-BC84-79F99F26F103}']
  end;

  [JavaSignature('com/honeywell/aidc/BarcodeFailureEvent')]
  JBarcodeFailureEvent = interface(JEventObject)
    ['{853EA2AA-C885-4856-875F-C2F4127B890C}']
    function getTimestamp: JString; cdecl;
  end;
  TJBarcodeFailureEvent = class(TJavaGenericImport<JBarcodeFailureEventClass, JBarcodeFailureEvent>) end;

  JBarcodeReadEventClass = interface(JEventObjectClass)
    ['{5441ED0A-9974-4776-8EF8-9CB49349A7AB}']
  end;

  [JavaSignature('com/honeywell/aidc/BarcodeReadEvent')]
  JBarcodeReadEvent = interface(JEventObject)
    ['{ABCC231F-8847-4738-8BAE-4115B76940C9}']
    function getAimId: JString; cdecl;
    function getBarcodeBounds: JList; cdecl;
    function getBarcodeData: JString; cdecl;
    function getCharset: JCharset; cdecl;
    function getCodeId: JString; cdecl;
    function getTimestamp: JString; cdecl;
  end;
  TJBarcodeReadEvent = class(TJavaGenericImport<JBarcodeReadEventClass, JBarcodeReadEvent>) end;

  JBarcodeReaderClass = interface(JParcelableClass)
    ['{535D981B-752F-4A32-9101-306A5BA72A5A}']
    {class} function _GetBAD_READ_NOTIFICATION: JString; cdecl;
    {class} function _GetCODABAR_CHECK_DIGIT_MODE_CHECK: JString; cdecl;
    {class} function _GetCODABAR_CHECK_DIGIT_MODE_CHECK_AND_STRIP: JString; cdecl;
    {class} function _GetCODABAR_CHECK_DIGIT_MODE_NO_CHECK: JString; cdecl;
    {class} function _GetCODE_11_CHECK_DIGIT_MODE_DOUBLE_DIGIT_CHECK: JString; cdecl;
    {class} function _GetCODE_11_CHECK_DIGIT_MODE_DOUBLE_DIGIT_CHECK_AND_STRIP: JString; cdecl;
    {class} function _GetCODE_11_CHECK_DIGIT_MODE_SINGLE_DIGIT_CHECK: JString; cdecl;
    {class} function _GetCODE_11_CHECK_DIGIT_MODE_SINGLE_DIGIT_CHECK_AND_STRIP: JString; cdecl;
    {class} function _GetCODE_39_CHECK_DIGIT_MODE_CHECK: JString; cdecl;
    {class} function _GetCODE_39_CHECK_DIGIT_MODE_CHECK_AND_STRIP: JString; cdecl;
    {class} function _GetCODE_39_CHECK_DIGIT_MODE_NO_CHECK: JString; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetDATA_PROCESSOR_SYMBOLOGY_ID_AIM: JString; cdecl;
    {class} function _GetDATA_PROCESSOR_SYMBOLOGY_ID_HONEYWELL: JString; cdecl;
    {class} function _GetDATA_PROCESSOR_SYMBOLOGY_ID_NONE: JString; cdecl;
    {class} function _GetDEC_ID_PROP_USE_ROI_DISABLE: JString; cdecl;
    {class} function _GetDEC_ID_PROP_USE_ROI_DPM_AIMER_CENTERED: JString; cdecl;
    {class} function _GetDEC_ID_PROP_USE_ROI_KIOSK_OR_PRESENTATION: JString; cdecl;
    {class} function _GetDEC_ID_PROP_USE_ROI_STANDARD: JString; cdecl;
    {class} function _GetDEC_ID_PROP_USE_ROI_STANDARD_AIMER_CENTERED: JString; cdecl;
    {class} function _GetEANUCC_EMULATION_MODE_GS1_128_EMULATION: JString; cdecl;
    {class} function _GetEANUCC_EMULATION_MODE_GS1_CODE_EXPANSION_OFF: JString; cdecl;
    {class} function _GetEANUCC_EMULATION_MODE_GS1_DATABAR_EMULATION: JString; cdecl;
    {class} function _GetEANUCC_EMULATION_MODE_GS1_EAN8_TO_EAN13_CONVERSION: JString; cdecl;
    {class} function _GetEANUCC_EMULATION_MODE_GS1_EMULATION_OFF: JString; cdecl;
    {class} function _GetGOOD_READ_NOTIFICATION: JString; cdecl;
    {class} function _GetIMAGER_EXPOSURE_MODE_AUTO_EXPOSURE: JString; cdecl;
    {class} function _GetIMAGER_EXPOSURE_MODE_AUTO_SENSOR: JString; cdecl;
    {class} function _GetIMAGER_EXPOSURE_MODE_CONTEXT_SENSITIVE: JString; cdecl;
    {class} function _GetIMAGER_EXPOSURE_MODE_FIXED: JString; cdecl;
    {class} function _GetIMAGER_SAMPLE_METHOD_CENTER: JString; cdecl;
    {class} function _GetIMAGER_SAMPLE_METHOD_CENTER_WEIGHTED: JString; cdecl;
    {class} function _GetIMAGER_SAMPLE_METHOD_UNIFORM: JString; cdecl;
    {class} function _GetINTERLEAVED_25_CHECK_DIGIT_MODE_CHECK: JString; cdecl;
    {class} function _GetINTERLEAVED_25_CHECK_DIGIT_MODE_CHECK_AND_STRIP: JString; cdecl;
    {class} function _GetINTERLEAVED_25_CHECK_DIGIT_MODE_NO_CHECK: JString; cdecl;
    {class} function _GetMSI_CHECK_DIGIT_MODE_DOUBLE_MOD_10_CHECK: JString; cdecl;
    {class} function _GetMSI_CHECK_DIGIT_MODE_DOUBLE_MOD_10_CHECK_AND_STRIP: JString; cdecl;
    {class} function _GetMSI_CHECK_DIGIT_MODE_NO_CHECK: JString; cdecl;
    {class} function _GetMSI_CHECK_DIGIT_MODE_SINGLE_MOD_10_CHECK: JString; cdecl;
    {class} function _GetMSI_CHECK_DIGIT_MODE_SINGLE_MOD_10_CHECK_AND_STRIP: JString; cdecl;
    {class} function _GetMSI_CHECK_DIGIT_MODE_SINGLE_MOD_11_PLUS_MOD_10_CHECK: JString; cdecl;
    {class} function _GetMSI_CHECK_DIGIT_MODE_SINGLE_MOD_11_PLUS_MOD_10_CHECK_AND_STRIP: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_AUSTRALIA: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_BPO: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_CANADA: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_DUTCH: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_INFOMAIL: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_INFOMAIL_AND_BPO: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_JAPAN: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_NONE: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_AND_USPS: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_AND_USPS_PLUS_BNB: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_PLUS_BNB: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_USPS: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_USPS_PLUS_BNB: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_PLUS_BNB: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_UPU: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_UPU_AND_USPS: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_PLANET_AND_USPS: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_POSTNET: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_POSTNET_AND_UPU: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_POSTNET_AND_UPU_AND_USPS: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_POSTNET_AND_UPU_AND_USPS_PLUS_BNB: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_POSTNET_AND_UPU_PLUS_BNB: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_POSTNET_AND_USPS: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_POSTNET_AND_USPS_PLUS_BNB: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_POSTNET_PLUS_BNB: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_UPU: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_UPU_AND_USPS: JString; cdecl;
    {class} function _GetPOSTAL_2D_MODE_USPS: JString; cdecl;
    {class} function _GetPOSTAL_OCR_MODE_INVERSE: JString; cdecl;
    {class} function _GetPOSTAL_OCR_MODE_NORMAL: JString; cdecl;
    {class} function _GetPOSTAL_OCR_MODE_NORMAL_AND_INVERSE: JString; cdecl;
    {class} function _GetPOSTAL_OCR_MODE_OFF: JString; cdecl;
    {class} function _GetPROPERTY_AZTEC_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_AZTEC_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_AZTEC_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CENTER_DECODE: JString; cdecl;
    {class} function _GetPROPERTY_CHINA_POST_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CHINA_POST_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CHINA_POST_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODABAR_CHECK_DIGIT_MODE: JString; cdecl;
    {class} function _GetPROPERTY_CODABAR_CONCAT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODABAR_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODABAR_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODABAR_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODABAR_START_STOP_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODABLOCK_A_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODABLOCK_A_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODABLOCK_A_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODABLOCK_F_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODABLOCK_F_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODABLOCK_F_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_11_CHECK_DIGIT_MODE: JString; cdecl;
    {class} function _GetPROPERTY_CODE_11_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODE_11_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_11_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_128_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODE_128_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_128_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_128_SHORT_MARGIN: JString; cdecl;
    {class} function _GetPROPERTY_CODE_39_BASE_32_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODE_39_CHECK_DIGIT_MODE: JString; cdecl;
    {class} function _GetPROPERTY_CODE_39_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODE_39_FULL_ASCII_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODE_39_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_39_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_39_START_STOP_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODE_93_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODE_93_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_93_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_DOTCODE_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_CODE_DOTCODE_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_CODE_DOTCODE_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_COMBINE_COMPOSITES: JString; cdecl;
    {class} function _GetPROPERTY_COMPOSITE_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_COMPOSITE_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_COMPOSITE_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_COMPOSITE_WITH_UPC_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_DATAMATRIX_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_DATAMATRIX_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_DATAMATRIX_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_CHARSET: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT_ACTION: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT_CATEGORY: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT_CLASS_NAME: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT_PACKAGE_NAME: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_EDIT_DATA_PLUGIN: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_LAUNCH_BROWSER: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_LAUNCH_EZ_CONFIG: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_PREFIX: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_SCAN_TO_INTENT: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_SUFFIX: JString; cdecl;
    {class} function _GetPROPERTY_DATA_PROCESSOR_SYMBOLOGY_PREFIX: JString; cdecl;
    {class} function _GetPROPERTY_DECODE_WINDOW_BOTTOM: JString; cdecl;
    {class} function _GetPROPERTY_DECODE_WINDOW_LEFT: JString; cdecl;
    {class} function _GetPROPERTY_DECODE_WINDOW_RIGHT: JString; cdecl;
    {class} function _GetPROPERTY_DECODE_WINDOW_TOP: JString; cdecl;
    {class} function _GetPROPERTY_DEC_CODE93_HIGH_DENSITY: JString; cdecl;
    {class} function _GetPROPERTY_DEC_ID_PROP_USE_ROI: JString; cdecl;
    {class} function _GetPROPERTY_EANUCC_EMULATION_MODE: JString; cdecl;
    {class} function _GetPROPERTY_EAN_13_ADDENDA_REQUIRED_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_13_ADDENDA_SEPARATOR_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_13_CHECK_DIGIT_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_13_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_13_FIVE_CHAR_ADDENDA_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_13_TWO_CHAR_ADDENDA_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_8_ADDENDA_REQUIRED_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_8_ADDENDA_SEPARATOR_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_8_CHECK_DIGIT_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_8_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_8_FIVE_CHAR_ADDENDA_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_EAN_8_TWO_CHAR_ADDENDA_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_GRIDMATRIX_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_GRIDMATRIX_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_GRIDMATRIX_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_GROUP_DATA_PROCESSING: JString; cdecl;
    {class} function _GetPROPERTY_GROUP_IMAGER: JString; cdecl;
    {class} function _GetPROPERTY_GROUP_NOTIFICATION: JString; cdecl;
    {class} function _GetPROPERTY_GROUP_SYMBOLOGY: JString; cdecl;
    {class} function _GetPROPERTY_GROUP_TRIGGER: JString; cdecl;
    {class} function _GetPROPERTY_GS1_128_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_GS1_128_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_GS1_128_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_HAX_XIN_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_HAX_XIN_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_HAX_XIN_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_IATA_25_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_IATA_25_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_IATA_25_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_EXPOSURE: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_EXPOSURE_MODE: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_GAIN: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_LIGHT_INTENSITY: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_MAXIMUM_EXPOSURE: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_MAXIMUM_GAIN: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_REJECTION_LIMIT: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_SAMPLE_METHOD: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_TARGET_ACCEPTABLE_OFFSET: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_TARGET_PERCENTILE: JString; cdecl;
    {class} function _GetPROPERTY_IMAGER_TARGET_VALUE: JString; cdecl;
    {class} function _GetPROPERTY_INTERLEAVED_25_CHECK_DIGIT_MODE: JString; cdecl;
    {class} function _GetPROPERTY_INTERLEAVED_25_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_INTERLEAVED_25_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_INTERLEAVED_25_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_ISBT_128_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_KOREAN_POST_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_KOREAN_POST_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_KOREAN_POST_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_LINEAR_DAMAGE_IMPROVEMENTS: JString; cdecl;
    {class} function _GetPROPERTY_MATRIX_25_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_MATRIX_25_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_MATRIX_25_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_MAXICODE_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_MAXICODE_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_MAXICODE_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_MICRO_PDF_417_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_MICRO_PDF_417_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_MICRO_PDF_417_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_MSI_CHECK_DIGIT_MODE: JString; cdecl;
    {class} function _GetPROPERTY_MSI_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_MSI_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_MSI_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_MSI_OUT_OF_SPEC_SYMBOL: JString; cdecl;
    {class} function _GetPROPERTY_MSI_SHORT_MARGIN: JString; cdecl;
    {class} function _GetPROPERTY_NOTIFICATION_BAD_READ_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_NOTIFICATION_GOOD_READ_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_NOTIFICATION_VIBRATE_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_OCR_ACTIVE_TEMPLATE: JString; cdecl;
    {class} function _GetPROPERTY_OCR_MODE: JString; cdecl;
    {class} function _GetPROPERTY_OCR_TEMPLATE: JString; cdecl;
    {class} function _GetPROPERTY_PDF_417_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_PDF_417_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_PDF_417_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_POSTAL_2D_MODE: JString; cdecl;
    {class} function _GetPROPERTY_POSTAL_2D_PLANET_CHECK_DIGIT_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_POSTAL_2D_POSTNET_CHECK_DIGIT_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_QR_CODE_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_QR_CODE_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_QR_CODE_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_RSS_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_RSS_EXPANDED_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_RSS_EXPANDED_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_RSS_EXPANDED_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_RSS_LIMITED_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_STANDARD_25_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_STANDARD_25_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_STANDARD_25_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_TELEPEN_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_TELEPEN_MAXIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_TELEPEN_MINIMUM_LENGTH: JString; cdecl;
    {class} function _GetPROPERTY_TELEPEN_OLD_STYLE_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_TLC_39_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_TRIGGER_AUTO_MODE_TIMEOUT: JString; cdecl;
    {class} function _GetPROPERTY_TRIGGER_CONTROL_MODE: JString; cdecl;
    {class} function _GetPROPERTY_TRIGGER_SCAN_DELAY: JString; cdecl;
    {class} function _GetPROPERTY_TRIGGER_SCAN_MODE: JString; cdecl;
    {class} function _GetPROPERTY_TRIGGER_SCAN_SAME_SYMBOL_TIMEOUT: JString; cdecl;
    {class} function _GetPROPERTY_TRIGGER_SCAN_SAME_SYMBOL_TIMEOUT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_TRIOPTIC_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_ADDENDA_REQUIRED_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_ADDENDA_SEPARATOR_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_CHECK_DIGIT_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_COMBINE_COUPON_CODE_MODE_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_COUPON_CODE_MODE_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_ENABLE: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_FIVE_CHAR_ADDENDA_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_NUMBER_SYSTEM_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_TRANSLATE_EAN13: JString; cdecl;
    {class} function _GetPROPERTY_UPC_A_TWO_CHAR_ADDENDA_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_ADDENDA_REQUIRED_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_ADDENDA_SEPARATOR_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_CHECK_DIGIT_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_E1_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_EXPAND_TO_UPC_A: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_FIVE_CHAR_ADDENDA_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_NUMBER_SYSTEM_TRANSMIT_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_UPC_E_TWO_CHAR_ADDENDA_ENABLED: JString; cdecl;
    {class} function _GetPROPERTY_VIDEO_REVERSE_ENABLED: JString; cdecl;
    {class} function _GetSHORT_MARGIN_DISABLED: JString; cdecl;
    {class} function _GetSHORT_MARGIN_ENABLED: JString; cdecl;
    {class} function _GetSHORT_MARGIN_ENABLE_BOTH_ENDS: JString; cdecl;
    {class} function _GetTRIGGER_CONTROL_MODE_AUTO_CONTROL: JString; cdecl;
    {class} function _GetTRIGGER_CONTROL_MODE_CLIENT_CONTROL: JString; cdecl;
    {class} function _GetTRIGGER_CONTROL_MODE_DISABLE: JString; cdecl;
    {class} function _GetTRIGGER_SCAN_MODE_CONTINUOUS: JString; cdecl;
    {class} function _GetTRIGGER_SCAN_MODE_ONESHOT: JString; cdecl;
    {class} function _GetTRIGGER_SCAN_MODE_READ_ON_RELEASE: JString; cdecl;
    {class} function _GetTRIGGER_SCAN_MODE_READ_ON_SECOND_TRIGGER_PRESS: JString; cdecl;
    {class} function _GetVIDEO_REVERSE_ENABLED_BOTH: JString; cdecl;
    {class} function _GetVIDEO_REVERSE_ENABLED_INVERSE: JString; cdecl;
    {class} function _GetVIDEO_REVERSE_ENABLED_NORMAL: JString; cdecl;
    {class} function init(parcel: JParcel): JBarcodeReader; cdecl; overload;
    {class} property BAD_READ_NOTIFICATION: JString read _GetBAD_READ_NOTIFICATION;
    {class} property CODABAR_CHECK_DIGIT_MODE_CHECK: JString read _GetCODABAR_CHECK_DIGIT_MODE_CHECK;
    {class} property CODABAR_CHECK_DIGIT_MODE_CHECK_AND_STRIP: JString read _GetCODABAR_CHECK_DIGIT_MODE_CHECK_AND_STRIP;
    {class} property CODABAR_CHECK_DIGIT_MODE_NO_CHECK: JString read _GetCODABAR_CHECK_DIGIT_MODE_NO_CHECK;
    {class} property CODE_11_CHECK_DIGIT_MODE_DOUBLE_DIGIT_CHECK: JString read _GetCODE_11_CHECK_DIGIT_MODE_DOUBLE_DIGIT_CHECK;
    {class} property CODE_11_CHECK_DIGIT_MODE_DOUBLE_DIGIT_CHECK_AND_STRIP: JString read _GetCODE_11_CHECK_DIGIT_MODE_DOUBLE_DIGIT_CHECK_AND_STRIP;
    {class} property CODE_11_CHECK_DIGIT_MODE_SINGLE_DIGIT_CHECK: JString read _GetCODE_11_CHECK_DIGIT_MODE_SINGLE_DIGIT_CHECK;
    {class} property CODE_11_CHECK_DIGIT_MODE_SINGLE_DIGIT_CHECK_AND_STRIP: JString read _GetCODE_11_CHECK_DIGIT_MODE_SINGLE_DIGIT_CHECK_AND_STRIP;
    {class} property CODE_39_CHECK_DIGIT_MODE_CHECK: JString read _GetCODE_39_CHECK_DIGIT_MODE_CHECK;
    {class} property CODE_39_CHECK_DIGIT_MODE_CHECK_AND_STRIP: JString read _GetCODE_39_CHECK_DIGIT_MODE_CHECK_AND_STRIP;
    {class} property CODE_39_CHECK_DIGIT_MODE_NO_CHECK: JString read _GetCODE_39_CHECK_DIGIT_MODE_NO_CHECK;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property DATA_PROCESSOR_SYMBOLOGY_ID_AIM: JString read _GetDATA_PROCESSOR_SYMBOLOGY_ID_AIM;
    {class} property DATA_PROCESSOR_SYMBOLOGY_ID_HONEYWELL: JString read _GetDATA_PROCESSOR_SYMBOLOGY_ID_HONEYWELL;
    {class} property DATA_PROCESSOR_SYMBOLOGY_ID_NONE: JString read _GetDATA_PROCESSOR_SYMBOLOGY_ID_NONE;
    {class} property DEC_ID_PROP_USE_ROI_DISABLE: JString read _GetDEC_ID_PROP_USE_ROI_DISABLE;
    {class} property DEC_ID_PROP_USE_ROI_DPM_AIMER_CENTERED: JString read _GetDEC_ID_PROP_USE_ROI_DPM_AIMER_CENTERED;
    {class} property DEC_ID_PROP_USE_ROI_KIOSK_OR_PRESENTATION: JString read _GetDEC_ID_PROP_USE_ROI_KIOSK_OR_PRESENTATION;
    {class} property DEC_ID_PROP_USE_ROI_STANDARD: JString read _GetDEC_ID_PROP_USE_ROI_STANDARD;
    {class} property DEC_ID_PROP_USE_ROI_STANDARD_AIMER_CENTERED: JString read _GetDEC_ID_PROP_USE_ROI_STANDARD_AIMER_CENTERED;
    {class} property EANUCC_EMULATION_MODE_GS1_128_EMULATION: JString read _GetEANUCC_EMULATION_MODE_GS1_128_EMULATION;
    {class} property EANUCC_EMULATION_MODE_GS1_CODE_EXPANSION_OFF: JString read _GetEANUCC_EMULATION_MODE_GS1_CODE_EXPANSION_OFF;
    {class} property EANUCC_EMULATION_MODE_GS1_DATABAR_EMULATION: JString read _GetEANUCC_EMULATION_MODE_GS1_DATABAR_EMULATION;
    {class} property EANUCC_EMULATION_MODE_GS1_EAN8_TO_EAN13_CONVERSION: JString read _GetEANUCC_EMULATION_MODE_GS1_EAN8_TO_EAN13_CONVERSION;
    {class} property EANUCC_EMULATION_MODE_GS1_EMULATION_OFF: JString read _GetEANUCC_EMULATION_MODE_GS1_EMULATION_OFF;
    {class} property GOOD_READ_NOTIFICATION: JString read _GetGOOD_READ_NOTIFICATION;
    {class} property IMAGER_EXPOSURE_MODE_AUTO_EXPOSURE: JString read _GetIMAGER_EXPOSURE_MODE_AUTO_EXPOSURE;
    {class} property IMAGER_EXPOSURE_MODE_AUTO_SENSOR: JString read _GetIMAGER_EXPOSURE_MODE_AUTO_SENSOR;
    {class} property IMAGER_EXPOSURE_MODE_CONTEXT_SENSITIVE: JString read _GetIMAGER_EXPOSURE_MODE_CONTEXT_SENSITIVE;
    {class} property IMAGER_EXPOSURE_MODE_FIXED: JString read _GetIMAGER_EXPOSURE_MODE_FIXED;
    {class} property IMAGER_SAMPLE_METHOD_CENTER: JString read _GetIMAGER_SAMPLE_METHOD_CENTER;
    {class} property IMAGER_SAMPLE_METHOD_CENTER_WEIGHTED: JString read _GetIMAGER_SAMPLE_METHOD_CENTER_WEIGHTED;
    {class} property IMAGER_SAMPLE_METHOD_UNIFORM: JString read _GetIMAGER_SAMPLE_METHOD_UNIFORM;
    {class} property INTERLEAVED_25_CHECK_DIGIT_MODE_CHECK: JString read _GetINTERLEAVED_25_CHECK_DIGIT_MODE_CHECK;
    {class} property INTERLEAVED_25_CHECK_DIGIT_MODE_CHECK_AND_STRIP: JString read _GetINTERLEAVED_25_CHECK_DIGIT_MODE_CHECK_AND_STRIP;
    {class} property INTERLEAVED_25_CHECK_DIGIT_MODE_NO_CHECK: JString read _GetINTERLEAVED_25_CHECK_DIGIT_MODE_NO_CHECK;
    {class} property MSI_CHECK_DIGIT_MODE_DOUBLE_MOD_10_CHECK: JString read _GetMSI_CHECK_DIGIT_MODE_DOUBLE_MOD_10_CHECK;
    {class} property MSI_CHECK_DIGIT_MODE_DOUBLE_MOD_10_CHECK_AND_STRIP: JString read _GetMSI_CHECK_DIGIT_MODE_DOUBLE_MOD_10_CHECK_AND_STRIP;
    {class} property MSI_CHECK_DIGIT_MODE_NO_CHECK: JString read _GetMSI_CHECK_DIGIT_MODE_NO_CHECK;
    {class} property MSI_CHECK_DIGIT_MODE_SINGLE_MOD_10_CHECK: JString read _GetMSI_CHECK_DIGIT_MODE_SINGLE_MOD_10_CHECK;
    {class} property MSI_CHECK_DIGIT_MODE_SINGLE_MOD_10_CHECK_AND_STRIP: JString read _GetMSI_CHECK_DIGIT_MODE_SINGLE_MOD_10_CHECK_AND_STRIP;
    {class} property MSI_CHECK_DIGIT_MODE_SINGLE_MOD_11_PLUS_MOD_10_CHECK: JString read _GetMSI_CHECK_DIGIT_MODE_SINGLE_MOD_11_PLUS_MOD_10_CHECK;
    {class} property MSI_CHECK_DIGIT_MODE_SINGLE_MOD_11_PLUS_MOD_10_CHECK_AND_STRIP: JString read _GetMSI_CHECK_DIGIT_MODE_SINGLE_MOD_11_PLUS_MOD_10_CHECK_AND_STRIP;
    {class} property POSTAL_2D_MODE_AUSTRALIA: JString read _GetPOSTAL_2D_MODE_AUSTRALIA;
    {class} property POSTAL_2D_MODE_BPO: JString read _GetPOSTAL_2D_MODE_BPO;
    {class} property POSTAL_2D_MODE_CANADA: JString read _GetPOSTAL_2D_MODE_CANADA;
    {class} property POSTAL_2D_MODE_DUTCH: JString read _GetPOSTAL_2D_MODE_DUTCH;
    {class} property POSTAL_2D_MODE_INFOMAIL: JString read _GetPOSTAL_2D_MODE_INFOMAIL;
    {class} property POSTAL_2D_MODE_INFOMAIL_AND_BPO: JString read _GetPOSTAL_2D_MODE_INFOMAIL_AND_BPO;
    {class} property POSTAL_2D_MODE_JAPAN: JString read _GetPOSTAL_2D_MODE_JAPAN;
    {class} property POSTAL_2D_MODE_NONE: JString read _GetPOSTAL_2D_MODE_NONE;
    {class} property POSTAL_2D_MODE_PLANET: JString read _GetPOSTAL_2D_MODE_PLANET;
    {class} property POSTAL_2D_MODE_PLANET_AND_POSTNET: JString read _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET;
    {class} property POSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU: JString read _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU;
    {class} property POSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_AND_USPS: JString read _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_AND_USPS;
    {class} property POSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_AND_USPS_PLUS_BNB: JString read _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_AND_USPS_PLUS_BNB;
    {class} property POSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_PLUS_BNB: JString read _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_UPU_PLUS_BNB;
    {class} property POSTAL_2D_MODE_PLANET_AND_POSTNET_AND_USPS: JString read _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_USPS;
    {class} property POSTAL_2D_MODE_PLANET_AND_POSTNET_AND_USPS_PLUS_BNB: JString read _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_AND_USPS_PLUS_BNB;
    {class} property POSTAL_2D_MODE_PLANET_AND_POSTNET_PLUS_BNB: JString read _GetPOSTAL_2D_MODE_PLANET_AND_POSTNET_PLUS_BNB;
    {class} property POSTAL_2D_MODE_PLANET_AND_UPU: JString read _GetPOSTAL_2D_MODE_PLANET_AND_UPU;
    {class} property POSTAL_2D_MODE_PLANET_AND_UPU_AND_USPS: JString read _GetPOSTAL_2D_MODE_PLANET_AND_UPU_AND_USPS;
    {class} property POSTAL_2D_MODE_PLANET_AND_USPS: JString read _GetPOSTAL_2D_MODE_PLANET_AND_USPS;
    {class} property POSTAL_2D_MODE_POSTNET: JString read _GetPOSTAL_2D_MODE_POSTNET;
    {class} property POSTAL_2D_MODE_POSTNET_AND_UPU: JString read _GetPOSTAL_2D_MODE_POSTNET_AND_UPU;
    {class} property POSTAL_2D_MODE_POSTNET_AND_UPU_AND_USPS: JString read _GetPOSTAL_2D_MODE_POSTNET_AND_UPU_AND_USPS;
    {class} property POSTAL_2D_MODE_POSTNET_AND_UPU_AND_USPS_PLUS_BNB: JString read _GetPOSTAL_2D_MODE_POSTNET_AND_UPU_AND_USPS_PLUS_BNB;
    {class} property POSTAL_2D_MODE_POSTNET_AND_UPU_PLUS_BNB: JString read _GetPOSTAL_2D_MODE_POSTNET_AND_UPU_PLUS_BNB;
    {class} property POSTAL_2D_MODE_POSTNET_AND_USPS: JString read _GetPOSTAL_2D_MODE_POSTNET_AND_USPS;
    {class} property POSTAL_2D_MODE_POSTNET_AND_USPS_PLUS_BNB: JString read _GetPOSTAL_2D_MODE_POSTNET_AND_USPS_PLUS_BNB;
    {class} property POSTAL_2D_MODE_POSTNET_PLUS_BNB: JString read _GetPOSTAL_2D_MODE_POSTNET_PLUS_BNB;
    {class} property POSTAL_2D_MODE_UPU: JString read _GetPOSTAL_2D_MODE_UPU;
    {class} property POSTAL_2D_MODE_UPU_AND_USPS: JString read _GetPOSTAL_2D_MODE_UPU_AND_USPS;
    {class} property POSTAL_2D_MODE_USPS: JString read _GetPOSTAL_2D_MODE_USPS;
    {class} property POSTAL_OCR_MODE_INVERSE: JString read _GetPOSTAL_OCR_MODE_INVERSE;
    {class} property POSTAL_OCR_MODE_NORMAL: JString read _GetPOSTAL_OCR_MODE_NORMAL;
    {class} property POSTAL_OCR_MODE_NORMAL_AND_INVERSE: JString read _GetPOSTAL_OCR_MODE_NORMAL_AND_INVERSE;
    {class} property POSTAL_OCR_MODE_OFF: JString read _GetPOSTAL_OCR_MODE_OFF;
    {class} property PROPERTY_AZTEC_ENABLED: JString read _GetPROPERTY_AZTEC_ENABLED;
    {class} property PROPERTY_AZTEC_MAXIMUM_LENGTH: JString read _GetPROPERTY_AZTEC_MAXIMUM_LENGTH;
    {class} property PROPERTY_AZTEC_MINIMUM_LENGTH: JString read _GetPROPERTY_AZTEC_MINIMUM_LENGTH;
    {class} property PROPERTY_CENTER_DECODE: JString read _GetPROPERTY_CENTER_DECODE;
    {class} property PROPERTY_CHINA_POST_ENABLED: JString read _GetPROPERTY_CHINA_POST_ENABLED;
    {class} property PROPERTY_CHINA_POST_MAXIMUM_LENGTH: JString read _GetPROPERTY_CHINA_POST_MAXIMUM_LENGTH;
    {class} property PROPERTY_CHINA_POST_MINIMUM_LENGTH: JString read _GetPROPERTY_CHINA_POST_MINIMUM_LENGTH;
    {class} property PROPERTY_CODABAR_CHECK_DIGIT_MODE: JString read _GetPROPERTY_CODABAR_CHECK_DIGIT_MODE;
    {class} property PROPERTY_CODABAR_CONCAT_ENABLED: JString read _GetPROPERTY_CODABAR_CONCAT_ENABLED;
    {class} property PROPERTY_CODABAR_ENABLED: JString read _GetPROPERTY_CODABAR_ENABLED;
    {class} property PROPERTY_CODABAR_MAXIMUM_LENGTH: JString read _GetPROPERTY_CODABAR_MAXIMUM_LENGTH;
    {class} property PROPERTY_CODABAR_MINIMUM_LENGTH: JString read _GetPROPERTY_CODABAR_MINIMUM_LENGTH;
    {class} property PROPERTY_CODABAR_START_STOP_TRANSMIT_ENABLED: JString read _GetPROPERTY_CODABAR_START_STOP_TRANSMIT_ENABLED;
    {class} property PROPERTY_CODABLOCK_A_ENABLED: JString read _GetPROPERTY_CODABLOCK_A_ENABLED;
    {class} property PROPERTY_CODABLOCK_A_MAXIMUM_LENGTH: JString read _GetPROPERTY_CODABLOCK_A_MAXIMUM_LENGTH;
    {class} property PROPERTY_CODABLOCK_A_MINIMUM_LENGTH: JString read _GetPROPERTY_CODABLOCK_A_MINIMUM_LENGTH;
    {class} property PROPERTY_CODABLOCK_F_ENABLED: JString read _GetPROPERTY_CODABLOCK_F_ENABLED;
    {class} property PROPERTY_CODABLOCK_F_MAXIMUM_LENGTH: JString read _GetPROPERTY_CODABLOCK_F_MAXIMUM_LENGTH;
    {class} property PROPERTY_CODABLOCK_F_MINIMUM_LENGTH: JString read _GetPROPERTY_CODABLOCK_F_MINIMUM_LENGTH;
    {class} property PROPERTY_CODE_11_CHECK_DIGIT_MODE: JString read _GetPROPERTY_CODE_11_CHECK_DIGIT_MODE;
    {class} property PROPERTY_CODE_11_ENABLED: JString read _GetPROPERTY_CODE_11_ENABLED;
    {class} property PROPERTY_CODE_11_MAXIMUM_LENGTH: JString read _GetPROPERTY_CODE_11_MAXIMUM_LENGTH;
    {class} property PROPERTY_CODE_11_MINIMUM_LENGTH: JString read _GetPROPERTY_CODE_11_MINIMUM_LENGTH;
    {class} property PROPERTY_CODE_128_ENABLED: JString read _GetPROPERTY_CODE_128_ENABLED;
    {class} property PROPERTY_CODE_128_MAXIMUM_LENGTH: JString read _GetPROPERTY_CODE_128_MAXIMUM_LENGTH;
    {class} property PROPERTY_CODE_128_MINIMUM_LENGTH: JString read _GetPROPERTY_CODE_128_MINIMUM_LENGTH;
    {class} property PROPERTY_CODE_128_SHORT_MARGIN: JString read _GetPROPERTY_CODE_128_SHORT_MARGIN;
    {class} property PROPERTY_CODE_39_BASE_32_ENABLED: JString read _GetPROPERTY_CODE_39_BASE_32_ENABLED;
    {class} property PROPERTY_CODE_39_CHECK_DIGIT_MODE: JString read _GetPROPERTY_CODE_39_CHECK_DIGIT_MODE;
    {class} property PROPERTY_CODE_39_ENABLED: JString read _GetPROPERTY_CODE_39_ENABLED;
    {class} property PROPERTY_CODE_39_FULL_ASCII_ENABLED: JString read _GetPROPERTY_CODE_39_FULL_ASCII_ENABLED;
    {class} property PROPERTY_CODE_39_MAXIMUM_LENGTH: JString read _GetPROPERTY_CODE_39_MAXIMUM_LENGTH;
    {class} property PROPERTY_CODE_39_MINIMUM_LENGTH: JString read _GetPROPERTY_CODE_39_MINIMUM_LENGTH;
    {class} property PROPERTY_CODE_39_START_STOP_TRANSMIT_ENABLED: JString read _GetPROPERTY_CODE_39_START_STOP_TRANSMIT_ENABLED;
    {class} property PROPERTY_CODE_93_ENABLED: JString read _GetPROPERTY_CODE_93_ENABLED;
    {class} property PROPERTY_CODE_93_MAXIMUM_LENGTH: JString read _GetPROPERTY_CODE_93_MAXIMUM_LENGTH;
    {class} property PROPERTY_CODE_93_MINIMUM_LENGTH: JString read _GetPROPERTY_CODE_93_MINIMUM_LENGTH;
    {class} property PROPERTY_CODE_DOTCODE_ENABLED: JString read _GetPROPERTY_CODE_DOTCODE_ENABLED;
    {class} property PROPERTY_CODE_DOTCODE_MAXIMUM_LENGTH: JString read _GetPROPERTY_CODE_DOTCODE_MAXIMUM_LENGTH;
    {class} property PROPERTY_CODE_DOTCODE_MINIMUM_LENGTH: JString read _GetPROPERTY_CODE_DOTCODE_MINIMUM_LENGTH;
    {class} property PROPERTY_COMBINE_COMPOSITES: JString read _GetPROPERTY_COMBINE_COMPOSITES;
    {class} property PROPERTY_COMPOSITE_ENABLED: JString read _GetPROPERTY_COMPOSITE_ENABLED;
    {class} property PROPERTY_COMPOSITE_MAXIMUM_LENGTH: JString read _GetPROPERTY_COMPOSITE_MAXIMUM_LENGTH;
    {class} property PROPERTY_COMPOSITE_MINIMUM_LENGTH: JString read _GetPROPERTY_COMPOSITE_MINIMUM_LENGTH;
    {class} property PROPERTY_COMPOSITE_WITH_UPC_ENABLED: JString read _GetPROPERTY_COMPOSITE_WITH_UPC_ENABLED;
    {class} property PROPERTY_DATAMATRIX_ENABLED: JString read _GetPROPERTY_DATAMATRIX_ENABLED;
    {class} property PROPERTY_DATAMATRIX_MAXIMUM_LENGTH: JString read _GetPROPERTY_DATAMATRIX_MAXIMUM_LENGTH;
    {class} property PROPERTY_DATAMATRIX_MINIMUM_LENGTH: JString read _GetPROPERTY_DATAMATRIX_MINIMUM_LENGTH;
    {class} property PROPERTY_DATA_PROCESSOR_CHARSET: JString read _GetPROPERTY_DATA_PROCESSOR_CHARSET;
    {class} property PROPERTY_DATA_PROCESSOR_DATA_INTENT: JString read _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT;
    {class} property PROPERTY_DATA_PROCESSOR_DATA_INTENT_ACTION: JString read _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT_ACTION;
    {class} property PROPERTY_DATA_PROCESSOR_DATA_INTENT_CATEGORY: JString read _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT_CATEGORY;
    {class} property PROPERTY_DATA_PROCESSOR_DATA_INTENT_CLASS_NAME: JString read _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT_CLASS_NAME;
    {class} property PROPERTY_DATA_PROCESSOR_DATA_INTENT_PACKAGE_NAME: JString read _GetPROPERTY_DATA_PROCESSOR_DATA_INTENT_PACKAGE_NAME;
    {class} property PROPERTY_DATA_PROCESSOR_EDIT_DATA_PLUGIN: JString read _GetPROPERTY_DATA_PROCESSOR_EDIT_DATA_PLUGIN;
    {class} property PROPERTY_DATA_PROCESSOR_LAUNCH_BROWSER: JString read _GetPROPERTY_DATA_PROCESSOR_LAUNCH_BROWSER;
    {class} property PROPERTY_DATA_PROCESSOR_LAUNCH_EZ_CONFIG: JString read _GetPROPERTY_DATA_PROCESSOR_LAUNCH_EZ_CONFIG;
    {class} property PROPERTY_DATA_PROCESSOR_PREFIX: JString read _GetPROPERTY_DATA_PROCESSOR_PREFIX;
    {class} property PROPERTY_DATA_PROCESSOR_SCAN_TO_INTENT: JString read _GetPROPERTY_DATA_PROCESSOR_SCAN_TO_INTENT;
    {class} property PROPERTY_DATA_PROCESSOR_SUFFIX: JString read _GetPROPERTY_DATA_PROCESSOR_SUFFIX;
    {class} property PROPERTY_DATA_PROCESSOR_SYMBOLOGY_PREFIX: JString read _GetPROPERTY_DATA_PROCESSOR_SYMBOLOGY_PREFIX;
    {class} property PROPERTY_DECODE_WINDOW_BOTTOM: JString read _GetPROPERTY_DECODE_WINDOW_BOTTOM;
    {class} property PROPERTY_DECODE_WINDOW_LEFT: JString read _GetPROPERTY_DECODE_WINDOW_LEFT;
    {class} property PROPERTY_DECODE_WINDOW_RIGHT: JString read _GetPROPERTY_DECODE_WINDOW_RIGHT;
    {class} property PROPERTY_DECODE_WINDOW_TOP: JString read _GetPROPERTY_DECODE_WINDOW_TOP;
    {class} property PROPERTY_DEC_CODE93_HIGH_DENSITY: JString read _GetPROPERTY_DEC_CODE93_HIGH_DENSITY;
    {class} property PROPERTY_DEC_ID_PROP_USE_ROI: JString read _GetPROPERTY_DEC_ID_PROP_USE_ROI;
    {class} property PROPERTY_EANUCC_EMULATION_MODE: JString read _GetPROPERTY_EANUCC_EMULATION_MODE;
    {class} property PROPERTY_EAN_13_ADDENDA_REQUIRED_ENABLED: JString read _GetPROPERTY_EAN_13_ADDENDA_REQUIRED_ENABLED;
    {class} property PROPERTY_EAN_13_ADDENDA_SEPARATOR_ENABLED: JString read _GetPROPERTY_EAN_13_ADDENDA_SEPARATOR_ENABLED;
    {class} property PROPERTY_EAN_13_CHECK_DIGIT_TRANSMIT_ENABLED: JString read _GetPROPERTY_EAN_13_CHECK_DIGIT_TRANSMIT_ENABLED;
    {class} property PROPERTY_EAN_13_ENABLED: JString read _GetPROPERTY_EAN_13_ENABLED;
    {class} property PROPERTY_EAN_13_FIVE_CHAR_ADDENDA_ENABLED: JString read _GetPROPERTY_EAN_13_FIVE_CHAR_ADDENDA_ENABLED;
    {class} property PROPERTY_EAN_13_TWO_CHAR_ADDENDA_ENABLED: JString read _GetPROPERTY_EAN_13_TWO_CHAR_ADDENDA_ENABLED;
    {class} property PROPERTY_EAN_8_ADDENDA_REQUIRED_ENABLED: JString read _GetPROPERTY_EAN_8_ADDENDA_REQUIRED_ENABLED;
    {class} property PROPERTY_EAN_8_ADDENDA_SEPARATOR_ENABLED: JString read _GetPROPERTY_EAN_8_ADDENDA_SEPARATOR_ENABLED;
    {class} property PROPERTY_EAN_8_CHECK_DIGIT_TRANSMIT_ENABLED: JString read _GetPROPERTY_EAN_8_CHECK_DIGIT_TRANSMIT_ENABLED;
    {class} property PROPERTY_EAN_8_ENABLED: JString read _GetPROPERTY_EAN_8_ENABLED;
    {class} property PROPERTY_EAN_8_FIVE_CHAR_ADDENDA_ENABLED: JString read _GetPROPERTY_EAN_8_FIVE_CHAR_ADDENDA_ENABLED;
    {class} property PROPERTY_EAN_8_TWO_CHAR_ADDENDA_ENABLED: JString read _GetPROPERTY_EAN_8_TWO_CHAR_ADDENDA_ENABLED;
    {class} property PROPERTY_GRIDMATRIX_ENABLED: JString read _GetPROPERTY_GRIDMATRIX_ENABLED;
    {class} property PROPERTY_GRIDMATRIX_MAXIMUM_LENGTH: JString read _GetPROPERTY_GRIDMATRIX_MAXIMUM_LENGTH;
    {class} property PROPERTY_GRIDMATRIX_MINIMUM_LENGTH: JString read _GetPROPERTY_GRIDMATRIX_MINIMUM_LENGTH;
    {class} property PROPERTY_GROUP_DATA_PROCESSING: JString read _GetPROPERTY_GROUP_DATA_PROCESSING;
    {class} property PROPERTY_GROUP_IMAGER: JString read _GetPROPERTY_GROUP_IMAGER;
    {class} property PROPERTY_GROUP_NOTIFICATION: JString read _GetPROPERTY_GROUP_NOTIFICATION;
    {class} property PROPERTY_GROUP_SYMBOLOGY: JString read _GetPROPERTY_GROUP_SYMBOLOGY;
    {class} property PROPERTY_GROUP_TRIGGER: JString read _GetPROPERTY_GROUP_TRIGGER;
    {class} property PROPERTY_GS1_128_ENABLED: JString read _GetPROPERTY_GS1_128_ENABLED;
    {class} property PROPERTY_GS1_128_MAXIMUM_LENGTH: JString read _GetPROPERTY_GS1_128_MAXIMUM_LENGTH;
    {class} property PROPERTY_GS1_128_MINIMUM_LENGTH: JString read _GetPROPERTY_GS1_128_MINIMUM_LENGTH;
    {class} property PROPERTY_HAX_XIN_ENABLED: JString read _GetPROPERTY_HAX_XIN_ENABLED;
    {class} property PROPERTY_HAX_XIN_MAXIMUM_LENGTH: JString read _GetPROPERTY_HAX_XIN_MAXIMUM_LENGTH;
    {class} property PROPERTY_HAX_XIN_MINIMUM_LENGTH: JString read _GetPROPERTY_HAX_XIN_MINIMUM_LENGTH;
    {class} property PROPERTY_IATA_25_ENABLED: JString read _GetPROPERTY_IATA_25_ENABLED;
    {class} property PROPERTY_IATA_25_MAXIMUM_LENGTH: JString read _GetPROPERTY_IATA_25_MAXIMUM_LENGTH;
    {class} property PROPERTY_IATA_25_MINIMUM_LENGTH: JString read _GetPROPERTY_IATA_25_MINIMUM_LENGTH;
    {class} property PROPERTY_IMAGER_EXPOSURE: JString read _GetPROPERTY_IMAGER_EXPOSURE;
    {class} property PROPERTY_IMAGER_EXPOSURE_MODE: JString read _GetPROPERTY_IMAGER_EXPOSURE_MODE;
    {class} property PROPERTY_IMAGER_GAIN: JString read _GetPROPERTY_IMAGER_GAIN;
    {class} property PROPERTY_IMAGER_LIGHT_INTENSITY: JString read _GetPROPERTY_IMAGER_LIGHT_INTENSITY;
    {class} property PROPERTY_IMAGER_MAXIMUM_EXPOSURE: JString read _GetPROPERTY_IMAGER_MAXIMUM_EXPOSURE;
    {class} property PROPERTY_IMAGER_MAXIMUM_GAIN: JString read _GetPROPERTY_IMAGER_MAXIMUM_GAIN;
    {class} property PROPERTY_IMAGER_REJECTION_LIMIT: JString read _GetPROPERTY_IMAGER_REJECTION_LIMIT;
    {class} property PROPERTY_IMAGER_SAMPLE_METHOD: JString read _GetPROPERTY_IMAGER_SAMPLE_METHOD;
    {class} property PROPERTY_IMAGER_TARGET_ACCEPTABLE_OFFSET: JString read _GetPROPERTY_IMAGER_TARGET_ACCEPTABLE_OFFSET;
    {class} property PROPERTY_IMAGER_TARGET_PERCENTILE: JString read _GetPROPERTY_IMAGER_TARGET_PERCENTILE;
    {class} property PROPERTY_IMAGER_TARGET_VALUE: JString read _GetPROPERTY_IMAGER_TARGET_VALUE;
    {class} property PROPERTY_INTERLEAVED_25_CHECK_DIGIT_MODE: JString read _GetPROPERTY_INTERLEAVED_25_CHECK_DIGIT_MODE;
    {class} property PROPERTY_INTERLEAVED_25_ENABLED: JString read _GetPROPERTY_INTERLEAVED_25_ENABLED;
    {class} property PROPERTY_INTERLEAVED_25_MAXIMUM_LENGTH: JString read _GetPROPERTY_INTERLEAVED_25_MAXIMUM_LENGTH;
    {class} property PROPERTY_INTERLEAVED_25_MINIMUM_LENGTH: JString read _GetPROPERTY_INTERLEAVED_25_MINIMUM_LENGTH;
    {class} property PROPERTY_ISBT_128_ENABLED: JString read _GetPROPERTY_ISBT_128_ENABLED;
    {class} property PROPERTY_KOREAN_POST_ENABLED: JString read _GetPROPERTY_KOREAN_POST_ENABLED;
    {class} property PROPERTY_KOREAN_POST_MAXIMUM_LENGTH: JString read _GetPROPERTY_KOREAN_POST_MAXIMUM_LENGTH;
    {class} property PROPERTY_KOREAN_POST_MINIMUM_LENGTH: JString read _GetPROPERTY_KOREAN_POST_MINIMUM_LENGTH;
    {class} property PROPERTY_LINEAR_DAMAGE_IMPROVEMENTS: JString read _GetPROPERTY_LINEAR_DAMAGE_IMPROVEMENTS;
    {class} property PROPERTY_MATRIX_25_ENABLED: JString read _GetPROPERTY_MATRIX_25_ENABLED;
    {class} property PROPERTY_MATRIX_25_MAXIMUM_LENGTH: JString read _GetPROPERTY_MATRIX_25_MAXIMUM_LENGTH;
    {class} property PROPERTY_MATRIX_25_MINIMUM_LENGTH: JString read _GetPROPERTY_MATRIX_25_MINIMUM_LENGTH;
    {class} property PROPERTY_MAXICODE_ENABLED: JString read _GetPROPERTY_MAXICODE_ENABLED;
    {class} property PROPERTY_MAXICODE_MAXIMUM_LENGTH: JString read _GetPROPERTY_MAXICODE_MAXIMUM_LENGTH;
    {class} property PROPERTY_MAXICODE_MINIMUM_LENGTH: JString read _GetPROPERTY_MAXICODE_MINIMUM_LENGTH;
    {class} property PROPERTY_MICRO_PDF_417_ENABLED: JString read _GetPROPERTY_MICRO_PDF_417_ENABLED;
    {class} property PROPERTY_MICRO_PDF_417_MAXIMUM_LENGTH: JString read _GetPROPERTY_MICRO_PDF_417_MAXIMUM_LENGTH;
    {class} property PROPERTY_MICRO_PDF_417_MINIMUM_LENGTH: JString read _GetPROPERTY_MICRO_PDF_417_MINIMUM_LENGTH;
    {class} property PROPERTY_MSI_CHECK_DIGIT_MODE: JString read _GetPROPERTY_MSI_CHECK_DIGIT_MODE;
    {class} property PROPERTY_MSI_ENABLED: JString read _GetPROPERTY_MSI_ENABLED;
    {class} property PROPERTY_MSI_MAXIMUM_LENGTH: JString read _GetPROPERTY_MSI_MAXIMUM_LENGTH;
    {class} property PROPERTY_MSI_MINIMUM_LENGTH: JString read _GetPROPERTY_MSI_MINIMUM_LENGTH;
    {class} property PROPERTY_MSI_OUT_OF_SPEC_SYMBOL: JString read _GetPROPERTY_MSI_OUT_OF_SPEC_SYMBOL;
    {class} property PROPERTY_MSI_SHORT_MARGIN: JString read _GetPROPERTY_MSI_SHORT_MARGIN;
    {class} property PROPERTY_NOTIFICATION_BAD_READ_ENABLED: JString read _GetPROPERTY_NOTIFICATION_BAD_READ_ENABLED;
    {class} property PROPERTY_NOTIFICATION_GOOD_READ_ENABLED: JString read _GetPROPERTY_NOTIFICATION_GOOD_READ_ENABLED;
    {class} property PROPERTY_NOTIFICATION_VIBRATE_ENABLED: JString read _GetPROPERTY_NOTIFICATION_VIBRATE_ENABLED;
    {class} property PROPERTY_OCR_ACTIVE_TEMPLATE: JString read _GetPROPERTY_OCR_ACTIVE_TEMPLATE;
    {class} property PROPERTY_OCR_MODE: JString read _GetPROPERTY_OCR_MODE;
    {class} property PROPERTY_OCR_TEMPLATE: JString read _GetPROPERTY_OCR_TEMPLATE;
    {class} property PROPERTY_PDF_417_ENABLED: JString read _GetPROPERTY_PDF_417_ENABLED;
    {class} property PROPERTY_PDF_417_MAXIMUM_LENGTH: JString read _GetPROPERTY_PDF_417_MAXIMUM_LENGTH;
    {class} property PROPERTY_PDF_417_MINIMUM_LENGTH: JString read _GetPROPERTY_PDF_417_MINIMUM_LENGTH;
    {class} property PROPERTY_POSTAL_2D_MODE: JString read _GetPROPERTY_POSTAL_2D_MODE;
    {class} property PROPERTY_POSTAL_2D_PLANET_CHECK_DIGIT_TRANSMIT_ENABLED: JString read _GetPROPERTY_POSTAL_2D_PLANET_CHECK_DIGIT_TRANSMIT_ENABLED;
    {class} property PROPERTY_POSTAL_2D_POSTNET_CHECK_DIGIT_TRANSMIT_ENABLED: JString read _GetPROPERTY_POSTAL_2D_POSTNET_CHECK_DIGIT_TRANSMIT_ENABLED;
    {class} property PROPERTY_QR_CODE_ENABLED: JString read _GetPROPERTY_QR_CODE_ENABLED;
    {class} property PROPERTY_QR_CODE_MAXIMUM_LENGTH: JString read _GetPROPERTY_QR_CODE_MAXIMUM_LENGTH;
    {class} property PROPERTY_QR_CODE_MINIMUM_LENGTH: JString read _GetPROPERTY_QR_CODE_MINIMUM_LENGTH;
    {class} property PROPERTY_RSS_ENABLED: JString read _GetPROPERTY_RSS_ENABLED;
    {class} property PROPERTY_RSS_EXPANDED_ENABLED: JString read _GetPROPERTY_RSS_EXPANDED_ENABLED;
    {class} property PROPERTY_RSS_EXPANDED_MAXIMUM_LENGTH: JString read _GetPROPERTY_RSS_EXPANDED_MAXIMUM_LENGTH;
    {class} property PROPERTY_RSS_EXPANDED_MINIMUM_LENGTH: JString read _GetPROPERTY_RSS_EXPANDED_MINIMUM_LENGTH;
    {class} property PROPERTY_RSS_LIMITED_ENABLED: JString read _GetPROPERTY_RSS_LIMITED_ENABLED;
    {class} property PROPERTY_STANDARD_25_ENABLED: JString read _GetPROPERTY_STANDARD_25_ENABLED;
    {class} property PROPERTY_STANDARD_25_MAXIMUM_LENGTH: JString read _GetPROPERTY_STANDARD_25_MAXIMUM_LENGTH;
    {class} property PROPERTY_STANDARD_25_MINIMUM_LENGTH: JString read _GetPROPERTY_STANDARD_25_MINIMUM_LENGTH;
    {class} property PROPERTY_TELEPEN_ENABLED: JString read _GetPROPERTY_TELEPEN_ENABLED;
    {class} property PROPERTY_TELEPEN_MAXIMUM_LENGTH: JString read _GetPROPERTY_TELEPEN_MAXIMUM_LENGTH;
    {class} property PROPERTY_TELEPEN_MINIMUM_LENGTH: JString read _GetPROPERTY_TELEPEN_MINIMUM_LENGTH;
    {class} property PROPERTY_TELEPEN_OLD_STYLE_ENABLED: JString read _GetPROPERTY_TELEPEN_OLD_STYLE_ENABLED;
    {class} property PROPERTY_TLC_39_ENABLED: JString read _GetPROPERTY_TLC_39_ENABLED;
    {class} property PROPERTY_TRIGGER_AUTO_MODE_TIMEOUT: JString read _GetPROPERTY_TRIGGER_AUTO_MODE_TIMEOUT;
    {class} property PROPERTY_TRIGGER_CONTROL_MODE: JString read _GetPROPERTY_TRIGGER_CONTROL_MODE;
    {class} property PROPERTY_TRIGGER_SCAN_DELAY: JString read _GetPROPERTY_TRIGGER_SCAN_DELAY;
    {class} property PROPERTY_TRIGGER_SCAN_MODE: JString read _GetPROPERTY_TRIGGER_SCAN_MODE;
    {class} property PROPERTY_TRIGGER_SCAN_SAME_SYMBOL_TIMEOUT: JString read _GetPROPERTY_TRIGGER_SCAN_SAME_SYMBOL_TIMEOUT;
    {class} property PROPERTY_TRIGGER_SCAN_SAME_SYMBOL_TIMEOUT_ENABLED: JString read _GetPROPERTY_TRIGGER_SCAN_SAME_SYMBOL_TIMEOUT_ENABLED;
    {class} property PROPERTY_TRIOPTIC_ENABLED: JString read _GetPROPERTY_TRIOPTIC_ENABLED;
    {class} property PROPERTY_UPC_A_ADDENDA_REQUIRED_ENABLED: JString read _GetPROPERTY_UPC_A_ADDENDA_REQUIRED_ENABLED;
    {class} property PROPERTY_UPC_A_ADDENDA_SEPARATOR_ENABLED: JString read _GetPROPERTY_UPC_A_ADDENDA_SEPARATOR_ENABLED;
    {class} property PROPERTY_UPC_A_CHECK_DIGIT_TRANSMIT_ENABLED: JString read _GetPROPERTY_UPC_A_CHECK_DIGIT_TRANSMIT_ENABLED;
    {class} property PROPERTY_UPC_A_COMBINE_COUPON_CODE_MODE_ENABLED: JString read _GetPROPERTY_UPC_A_COMBINE_COUPON_CODE_MODE_ENABLED;
    {class} property PROPERTY_UPC_A_COUPON_CODE_MODE_ENABLED: JString read _GetPROPERTY_UPC_A_COUPON_CODE_MODE_ENABLED;
    {class} property PROPERTY_UPC_A_ENABLE: JString read _GetPROPERTY_UPC_A_ENABLE;
    {class} property PROPERTY_UPC_A_FIVE_CHAR_ADDENDA_ENABLED: JString read _GetPROPERTY_UPC_A_FIVE_CHAR_ADDENDA_ENABLED;
    {class} property PROPERTY_UPC_A_NUMBER_SYSTEM_TRANSMIT_ENABLED: JString read _GetPROPERTY_UPC_A_NUMBER_SYSTEM_TRANSMIT_ENABLED;
    {class} property PROPERTY_UPC_A_TRANSLATE_EAN13: JString read _GetPROPERTY_UPC_A_TRANSLATE_EAN13;
    {class} property PROPERTY_UPC_A_TWO_CHAR_ADDENDA_ENABLED: JString read _GetPROPERTY_UPC_A_TWO_CHAR_ADDENDA_ENABLED;
    {class} property PROPERTY_UPC_E_ADDENDA_REQUIRED_ENABLED: JString read _GetPROPERTY_UPC_E_ADDENDA_REQUIRED_ENABLED;
    {class} property PROPERTY_UPC_E_ADDENDA_SEPARATOR_ENABLED: JString read _GetPROPERTY_UPC_E_ADDENDA_SEPARATOR_ENABLED;
    {class} property PROPERTY_UPC_E_CHECK_DIGIT_TRANSMIT_ENABLED: JString read _GetPROPERTY_UPC_E_CHECK_DIGIT_TRANSMIT_ENABLED;
    {class} property PROPERTY_UPC_E_E1_ENABLED: JString read _GetPROPERTY_UPC_E_E1_ENABLED;
    {class} property PROPERTY_UPC_E_ENABLED: JString read _GetPROPERTY_UPC_E_ENABLED;
    {class} property PROPERTY_UPC_E_EXPAND_TO_UPC_A: JString read _GetPROPERTY_UPC_E_EXPAND_TO_UPC_A;
    {class} property PROPERTY_UPC_E_FIVE_CHAR_ADDENDA_ENABLED: JString read _GetPROPERTY_UPC_E_FIVE_CHAR_ADDENDA_ENABLED;
    {class} property PROPERTY_UPC_E_NUMBER_SYSTEM_TRANSMIT_ENABLED: JString read _GetPROPERTY_UPC_E_NUMBER_SYSTEM_TRANSMIT_ENABLED;
    {class} property PROPERTY_UPC_E_TWO_CHAR_ADDENDA_ENABLED: JString read _GetPROPERTY_UPC_E_TWO_CHAR_ADDENDA_ENABLED;
    {class} property PROPERTY_VIDEO_REVERSE_ENABLED: JString read _GetPROPERTY_VIDEO_REVERSE_ENABLED;
    {class} property SHORT_MARGIN_DISABLED: JString read _GetSHORT_MARGIN_DISABLED;
    {class} property SHORT_MARGIN_ENABLED: JString read _GetSHORT_MARGIN_ENABLED;
    {class} property SHORT_MARGIN_ENABLE_BOTH_ENDS: JString read _GetSHORT_MARGIN_ENABLE_BOTH_ENDS;
    {class} property TRIGGER_CONTROL_MODE_AUTO_CONTROL: JString read _GetTRIGGER_CONTROL_MODE_AUTO_CONTROL;
    {class} property TRIGGER_CONTROL_MODE_CLIENT_CONTROL: JString read _GetTRIGGER_CONTROL_MODE_CLIENT_CONTROL;
    {class} property TRIGGER_CONTROL_MODE_DISABLE: JString read _GetTRIGGER_CONTROL_MODE_DISABLE;
    {class} property TRIGGER_SCAN_MODE_CONTINUOUS: JString read _GetTRIGGER_SCAN_MODE_CONTINUOUS;
    {class} property TRIGGER_SCAN_MODE_ONESHOT: JString read _GetTRIGGER_SCAN_MODE_ONESHOT;
    {class} property TRIGGER_SCAN_MODE_READ_ON_RELEASE: JString read _GetTRIGGER_SCAN_MODE_READ_ON_RELEASE;
    {class} property TRIGGER_SCAN_MODE_READ_ON_SECOND_TRIGGER_PRESS: JString read _GetTRIGGER_SCAN_MODE_READ_ON_SECOND_TRIGGER_PRESS;
    {class} property VIDEO_REVERSE_ENABLED_BOTH: JString read _GetVIDEO_REVERSE_ENABLED_BOTH;
    {class} property VIDEO_REVERSE_ENABLED_INVERSE: JString read _GetVIDEO_REVERSE_ENABLED_INVERSE;
    {class} property VIDEO_REVERSE_ENABLED_NORMAL: JString read _GetVIDEO_REVERSE_ENABLED_NORMAL;
  end;

  [JavaSignature('com/honeywell/aidc/BarcodeReader')]
  JBarcodeReader = interface(JParcelable)
    ['{51886A8A-6FCF-49F5-8005-2D0E49516AD4}']
    procedure addBarcodeListener(barcodeListener: JBarcodeReader_BarcodeListener); cdecl;
    procedure addTriggerListener(triggerListener: JBarcodeReader_TriggerListener); cdecl;
    procedure aim(b: Boolean); cdecl;
    function captureImage: JBitmap; cdecl;
    procedure claim; cdecl;
    procedure close; cdecl;
    procedure decode(b: Boolean); cdecl;
    function describeContents: Integer; cdecl;
    function execute(message: Jhoneywell_Message): Jhoneywell_Message; cdecl;
    function getAllDefaultProperties: JMap; cdecl;
    function getAllProperties: JMap; cdecl;
    function getBooleanProperty(string_: JString): Boolean; cdecl;
    function getInfo: JBarcodeReaderInfo; cdecl;
    function getIntProperty(string_: JString): Integer; cdecl;
    function getProfileNames: JList; cdecl;
    function getProperties(set_: JSet): JMap; cdecl;
    function getSignature(signatureParameters: JSignatureParameters): Jaidc_Signature; cdecl;
    function getStringProperty(string_: JString): JString; cdecl;
    procedure light(b: Boolean); cdecl;
    function loadProfile(string_: JString): Boolean; cdecl;
    procedure notify(string_: JString); cdecl;
    procedure release; cdecl;
    procedure removeBarcodeListener(barcodeListener: JBarcodeReader_BarcodeListener); cdecl;
    procedure removeTriggerListener(triggerListener: JBarcodeReader_TriggerListener); cdecl;
    procedure setProperties(map: JMap); cdecl;
    procedure setProperty(string_: JString; string_1: JString); cdecl; overload;
    procedure setProperty(string_: JString; b: Boolean); cdecl; overload;
    procedure setProperty(string_: JString; i: Integer); cdecl; overload;
    procedure softwareTrigger(b: Boolean); cdecl;
    procedure startPropertyEditor(context: JContext); cdecl; overload;
    procedure startPropertyEditor(context: JContext; string_: JString; string_1: JString); cdecl; overload;
    procedure writeToParcel(parcel: JParcel; i: Integer); cdecl;
  end;
  TJBarcodeReader = class(TJavaGenericImport<JBarcodeReaderClass, JBarcodeReader>) end;

  JBarcodeReader_BarcodeListenerClass = interface(JEventListenerClass)
    ['{D6E5DDA6-402A-4BB7-A39C-B7F2E17B8AB4}']
  end;

  [JavaSignature('com/honeywell/aidc/BarcodeReader$BarcodeListener')]
  JBarcodeReader_BarcodeListener = interface(JEventListener)
    ['{10DAC2D1-07A3-4FC6-B6D5-4ED72E8B5BDA}']
    procedure onBarcodeEvent(barcodeReadEvent: JBarcodeReadEvent); cdecl;
    procedure onFailureEvent(barcodeFailureEvent: JBarcodeFailureEvent); cdecl;
  end;
  TJBarcodeReader_BarcodeListener = class(TJavaGenericImport<JBarcodeReader_BarcodeListenerClass, JBarcodeReader_BarcodeListener>) end;

  JBarcodeReader_TriggerListenerClass = interface(JEventListenerClass)
    ['{87B824F1-8F6E-42A5-B862-45F8BDF9BF7D}']
  end;

  [JavaSignature('com/honeywell/aidc/BarcodeReader$TriggerListener')]
  JBarcodeReader_TriggerListener = interface(JEventListener)
    ['{8B252B4A-751A-4A98-ACD4-23E5982191A6}']
    procedure onTriggerEvent(triggerStateChangeEvent: JTriggerStateChangeEvent); cdecl;
  end;
  TJBarcodeReader_TriggerListener = class(TJavaGenericImport<JBarcodeReader_TriggerListenerClass, JBarcodeReader_TriggerListener>) end;

  JBarcodeReaderInfoClass = interface(JObjectClass)
    ['{056FF114-9271-4A8C-8861-2F2A2CCFB2C9}']
    {class} function init(string_: JString; string_1: JString; string_2: JString; string_3: JString; string_4: JString; string_5: JString;
      string_6: JString; i: Integer; i1: Integer): JBarcodeReaderInfo; cdecl;
  end;

  [JavaSignature('com/honeywell/aidc/BarcodeReaderInfo')]
  JBarcodeReaderInfo = interface(JObject)
    ['{CF978424-A6F8-41F7-ABF4-FDDCCEA2F4C7}']
    function getControlLogicVersion: JString; cdecl;
    function getFastDecodeVersion: JString; cdecl;
    function getFrameHeight: Integer; cdecl;
    function getFrameWidth: Integer; cdecl;
    function getFriendlyName: JString; cdecl;
    function getFullDecodeVersion: JString; cdecl;
    function getName: JString; cdecl;
    function getScannerId: JString; cdecl;
    function getScannerVersionNumber: JString; cdecl;
  end;
  TJBarcodeReaderInfo = class(TJavaGenericImport<JBarcodeReaderInfoClass, JBarcodeReaderInfo>) end;

  Jaidc_BuildConfigClass = interface(JObjectClass)
    ['{EF8DB67D-FA77-44EE-BDE1-C095182CFC1F}']
    {class} function _GetAPPLICATION_ID: JString; cdecl;
    {class} function _GetBUILD_TYPE: JString; cdecl;
    {class} function _GetDEBUG: Boolean; cdecl;
    {class} function _GetFLAVOR: JString; cdecl;
    {class} function _GetVERSION_CODE: Integer; cdecl;
    {class} function _GetVERSION_NAME: JString; cdecl;
    {class} function init: Jaidc_BuildConfig; cdecl;
    {class} property APPLICATION_ID: JString read _GetAPPLICATION_ID;
    {class} property BUILD_TYPE: JString read _GetBUILD_TYPE;
    {class} property DEBUG: Boolean read _GetDEBUG;
    {class} property FLAVOR: JString read _GetFLAVOR;
    {class} property VERSION_CODE: Integer read _GetVERSION_CODE;
    {class} property VERSION_NAME: JString read _GetVERSION_NAME;
  end;

  [JavaSignature('com/honeywell/aidc/BuildConfig')]
  Jaidc_BuildConfig = interface(JObject)
    ['{9F0DA05A-5D3C-4A96-BB1A-7BFE89BA964A}']
  end;
  TJaidc_BuildConfig = class(TJavaGenericImport<Jaidc_BuildConfigClass, Jaidc_BuildConfig>) end;

  JDcsJsonRpcHelperClass = interface(JObjectClass)
    ['{0E746EF4-91DA-4C4B-B725-E33191730236}']
    {class} function build(string_: JString): Jhoneywell_Message; cdecl; overload;
    {class} function build(string_: JString; map: JMap): Jhoneywell_Message; cdecl; overload;
    {class} function build(string_: JString; string_1: JString; object_: JObject): Jhoneywell_Message; cdecl; overload;
    {class} procedure checkRuntimeError(message: Jhoneywell_Message); cdecl;
    {class} procedure checkScannerNotClaimedException(message: Jhoneywell_Message); cdecl;
    {class} procedure checkScannerUnavailable(message: Jhoneywell_Message); cdecl;
    {class} function getEvent(object_: JObject; message: Jhoneywell_Message): JEventObject; cdecl;
    {class} function init: JDcsJsonRpcHelper; cdecl;
  end;

  [JavaSignature('com/honeywell/aidc/DcsJsonRpcHelper')]
  JDcsJsonRpcHelper = interface(JObject)
    ['{F515FC5D-3817-4B6F-B759-4D0C2DC0474D}']
  end;
  TJDcsJsonRpcHelper = class(TJavaGenericImport<JDcsJsonRpcHelperClass, JDcsJsonRpcHelper>) end;

  JDebugLogClass = interface(JObjectClass)
    ['{D009564A-DBD5-47D1-BE35-59D4BD48B427}']
    {class} procedure d(string_: JString); cdecl;
  end;

  [JavaSignature('com/honeywell/aidc/DebugLog')]
  JDebugLog = interface(JObject)
    ['{1CB94655-4DEC-41CD-88FF-29BBDC8F5B9F}']
  end;
  TJDebugLog = class(TJavaGenericImport<JDebugLogClass, JDebugLog>) end;

  JDecodeIntentClass = interface(JObjectClass)
    ['{AFB07479-549A-4BE5-BA55-3FC38557F552}']
    {class} function _GetACTION_EDIT_PROFILE: JString; cdecl;
    {class} function _GetACTION_EDIT_PROFILE_LIST: JString; cdecl;
    {class} function _GetACTION_EDIT_SETTINGS: JString; cdecl;
    {class} function _GetACTION_EXECUTE: JString; cdecl;
    {class} function _GetEXTRA_REQUEST: JString; cdecl;
    {class} function _GetEXTRA_RESPONSE: JString; cdecl;
    {class} function init: JDecodeIntent; cdecl;//Deprecated
    {class} property ACTION_EDIT_PROFILE: JString read _GetACTION_EDIT_PROFILE;
    {class} property ACTION_EDIT_PROFILE_LIST: JString read _GetACTION_EDIT_PROFILE_LIST;
    {class} property ACTION_EDIT_SETTINGS: JString read _GetACTION_EDIT_SETTINGS;
    {class} property ACTION_EXECUTE: JString read _GetACTION_EXECUTE;
    {class} property EXTRA_REQUEST: JString read _GetEXTRA_REQUEST;
    {class} property EXTRA_RESPONSE: JString read _GetEXTRA_RESPONSE;
  end;

  [JavaSignature('com/honeywell/aidc/DecodeIntent')]
  JDecodeIntent = interface(JObject)
    ['{D370B207-D8FC-465A-A0B4-F0757E3CDE4C}']
  end;
  TJDecodeIntent = class(TJavaGenericImport<JDecodeIntentClass, JDecodeIntent>) end;

  JJsonUtilClass = interface(JObjectClass)
    ['{90168D52-BC26-4D47-AB07-CDE5DB02826B}']
    {class} function arrayToJson(object_: JObject): JJSONArray; cdecl;
    {class} function collectionToJson(collection: JCollection): JJSONArray; cdecl;
  end;

  [JavaSignature('com/honeywell/aidc/JsonUtil')]
  JJsonUtil = interface(JObject)
    ['{BA372CD3-B49E-4602-BCE5-2B4361693F23}']
  end;
  TJJsonUtil = class(TJavaGenericImport<JJsonUtilClass, JJsonUtil>) end;

  JScannerNotClaimedExceptionClass = interface(JAidcExceptionClass)
    ['{918CA976-786F-40EB-AF0D-9BD56D04B31E}']
  end;

  [JavaSignature('com/honeywell/aidc/ScannerNotClaimedException')]
  JScannerNotClaimedException = interface(JAidcException)
    ['{25C72895-6514-45E7-A264-BDF491F41575}']
  end;
  TJScannerNotClaimedException = class(TJavaGenericImport<JScannerNotClaimedExceptionClass, JScannerNotClaimedException>) end;

  JScannerUnavailableExceptionClass = interface(JAidcExceptionClass)
    ['{2323FF76-CC6E-43AB-9704-921FD614036B}']
  end;

  [JavaSignature('com/honeywell/aidc/ScannerUnavailableException')]
  JScannerUnavailableException = interface(JAidcException)
    ['{4098988F-F63F-47EF-ABC6-AE8833F3876F}']
  end;
  TJScannerUnavailableException = class(TJavaGenericImport<JScannerUnavailableExceptionClass, JScannerUnavailableException>) end;

  Jaidc_SignatureClass = interface(JObjectClass)
    ['{6AEE7683-400E-42AC-B22F-30E8EB842D28}']
    {class} function _GetGUIDANCE_MOVE_DOWN: JString; cdecl;
    {class} function _GetGUIDANCE_MOVE_LEFT: JString; cdecl;
    {class} function _GetGUIDANCE_MOVE_OUT: JString; cdecl;
    {class} function _GetGUIDANCE_MOVE_RIGHT: JString; cdecl;
    {class} function _GetGUIDANCE_MOVE_UP: JString; cdecl;
    {class} function _GetGUIDANCE_SUCCESS: JString; cdecl;
    {class} function _GetGUIDANCE_UNSUPPORTED_SYMBOLOGY: JString; cdecl;
    {class} function init(string_: JString; bitmap: JBitmap): Jaidc_Signature; cdecl;//Deprecated
    {class} property GUIDANCE_MOVE_DOWN: JString read _GetGUIDANCE_MOVE_DOWN;
    {class} property GUIDANCE_MOVE_LEFT: JString read _GetGUIDANCE_MOVE_LEFT;
    {class} property GUIDANCE_MOVE_OUT: JString read _GetGUIDANCE_MOVE_OUT;
    {class} property GUIDANCE_MOVE_RIGHT: JString read _GetGUIDANCE_MOVE_RIGHT;
    {class} property GUIDANCE_MOVE_UP: JString read _GetGUIDANCE_MOVE_UP;
    {class} property GUIDANCE_SUCCESS: JString read _GetGUIDANCE_SUCCESS;
    {class} property GUIDANCE_UNSUPPORTED_SYMBOLOGY: JString read _GetGUIDANCE_UNSUPPORTED_SYMBOLOGY;
  end;

  [JavaSignature('com/honeywell/aidc/Signature')]
  Jaidc_Signature = interface(JObject)
    ['{5C01DFF0-F769-4D88-B1AB-3C5C80C8BEA9}']
    function getGuidance: JString; cdecl;
    function getImage: JBitmap; cdecl;
  end;
  TJaidc_Signature = class(TJavaGenericImport<Jaidc_SignatureClass, Jaidc_Signature>) end;

  JSignatureParametersClass = interface(JObjectClass)
    ['{2EDBBD82-7DB1-4E8B-81AE-F6C89C13967B}']
    {class} function init: JSignatureParameters; cdecl; overload;
    {class} function init(i: Integer; i1: Integer; i2: Integer; i3: Integer; i4: Integer; i5: Integer;
      b: Boolean): JSignatureParameters; cdecl; overload;
  end;

  [JavaSignature('com/honeywell/aidc/SignatureParameters')]
  JSignatureParameters = interface(JObject)
    ['{BC6E7DFF-6A3F-4BF0-BECF-01FCBDE9BE31}']
    function getAspectRatio: Integer; cdecl;
    function getHeight: Integer; cdecl;
    function getHorizontalOffset: Integer; cdecl;
    function getResolution: Integer; cdecl;
    function getVerticalOffset: Integer; cdecl;
    function getWidth: Integer; cdecl;
    function isBinarized: Boolean; cdecl;
    procedure setAspectRatio(i: Integer); cdecl;
    procedure setBinarized(b: Boolean); cdecl;
    procedure setHeight(i: Integer); cdecl;
    procedure setHorizontalOffset(i: Integer); cdecl;
    procedure setResolution(i: Integer); cdecl;
    procedure setVerticalOffset(i: Integer); cdecl;
    procedure setWidth(i: Integer); cdecl;
  end;
  TJSignatureParameters = class(TJavaGenericImport<JSignatureParametersClass, JSignatureParameters>) end;

  JTriggerStateChangeEventClass = interface(JEventObjectClass)
    ['{8CEFAFD3-8A5C-4E40-A8F7-2A8C7F9279F8}']
  end;

  [JavaSignature('com/honeywell/aidc/TriggerStateChangeEvent')]
  JTriggerStateChangeEvent = interface(JEventObject)
    ['{B9123EB5-3B98-42D2-ABB4-8AA0ECCB2193}']
    function getState: Boolean; cdecl;
  end;
  TJTriggerStateChangeEvent = class(TJavaGenericImport<JTriggerStateChangeEventClass, JTriggerStateChangeEvent>) end;

  JUnsupportedPropertyExceptionClass = interface(JAidcExceptionClass)
    ['{870CB714-1703-4BF3-8049-865CE3B8B71F}']
  end;

  [JavaSignature('com/honeywell/aidc/UnsupportedPropertyException')]
  JUnsupportedPropertyException = interface(JAidcException)
    ['{09FEA076-E452-4C71-931E-3FE1F8232D42}']
  end;
  TJUnsupportedPropertyException = class(TJavaGenericImport<JUnsupportedPropertyExceptionClass, JUnsupportedPropertyException>) end;

implementation

end.
