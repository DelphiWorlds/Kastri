unit DW.Androidapi.JNI.Hardware.Usb;

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
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os;

type
  JUsbAccessory = interface;
  JUsbConfiguration = interface;
  JUsbConstants = interface;
  JUsbDevice = interface;
  JUsbDeviceConnection = interface;
  JUsbEndpoint = interface;
  JUsbInterface = interface;
  JUsbManager = interface;
  JUsbRequest = interface;

  JUsbAccessoryClass = interface(JObjectClass)
    ['{1D9B9887-3355-48AD-9E48-30EA6B124537}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/hardware/usb/UsbAccessory')]
  JUsbAccessory = interface(JObject)
    ['{083B13FB-2A1A-4659-BECD-9C245475B724}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getDescription: JString; cdecl;
    function getManufacturer: JString; cdecl;
    function getVersion: JString; cdecl;
    function getModel: JString; cdecl;
    function getSerial: JString; cdecl;
    function getUri: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJUsbAccessory = class(TJavaGenericImport<JUsbAccessoryClass, JUsbAccessory>) end;

  JUsbConfigurationClass = interface(JObjectClass)
    ['{98469519-6EAB-4A27-9B06-2C4A07DC51C8}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/hardware/usb/UsbConfiguration')]
  JUsbConfiguration = interface(JObject)
    ['{3CAF57A3-D977-4A31-BD15-1CFDE08316F5}']
    function describeContents: Integer; cdecl;
    function getId: Integer; cdecl;
    function getInterface(index: Integer): JUsbInterface; cdecl;
    function getInterfaceCount: Integer; cdecl;
    function getMaxPower: Integer; cdecl;
    function getName: JString; cdecl;
    function isRemoteWakeup: Boolean; cdecl;
    function isSelfPowered: Boolean; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJUsbConfiguration = class(TJavaGenericImport<JUsbConfigurationClass, JUsbConfiguration>) end;

  JUsbConstantsClass = interface(JObjectClass)
    ['{75BBB590-CD1C-4059-A7F9-D29A2EF3FA08}']
    {class} function _GetUSB_CLASS_APP_SPEC: Integer; cdecl;
    {class} function _GetUSB_CLASS_AUDIO: Integer; cdecl;
    {class} function _GetUSB_CLASS_CDC_DATA: Integer; cdecl;
    {class} function _GetUSB_CLASS_COMM: Integer; cdecl;
    {class} function _GetUSB_CLASS_CONTENT_SEC: Integer; cdecl;
    {class} function _GetUSB_CLASS_CSCID: Integer; cdecl;
    {class} function _GetUSB_CLASS_HID: Integer; cdecl;
    {class} function _GetUSB_CLASS_HUB: Integer; cdecl;
    {class} function _GetUSB_CLASS_MASS_STORAGE: Integer; cdecl;
    {class} function _GetUSB_CLASS_MISC: Integer; cdecl;
    {class} function _GetUSB_CLASS_PER_INTERFACE: Integer; cdecl;
    {class} function _GetUSB_CLASS_PHYSICA: Integer; cdecl;
    {class} function _GetUSB_CLASS_PRINTER: Integer; cdecl;
    {class} function _GetUSB_CLASS_STILL_IMAGE: Integer; cdecl;
    {class} function _GetUSB_CLASS_VENDOR_SPEC: Integer; cdecl;
    {class} function _GetUSB_CLASS_VIDEO: Integer; cdecl;
    {class} function _GetUSB_CLASS_WIRELESS_CONTROLLER: Integer; cdecl;
    {class} function _GetUSB_DIR_IN: Integer; cdecl;
    {class} function _GetUSB_DIR_OUT: Integer; cdecl;
    {class} function _GetUSB_ENDPOINT_DIR_MASK: Integer; cdecl;
    {class} function _GetUSB_ENDPOINT_NUMBER_MASK: Integer; cdecl;
    {class} function _GetUSB_ENDPOINT_XFERTYPE_MASK: Integer; cdecl;
    {class} function _GetUSB_ENDPOINT_XFER_BULK: Integer; cdecl;
    {class} function _GetUSB_ENDPOINT_XFER_CONTROL: Integer; cdecl;
    {class} function _GetUSB_ENDPOINT_XFER_INT: Integer; cdecl;
    {class} function _GetUSB_ENDPOINT_XFER_ISOC: Integer; cdecl;
    {class} function _GetUSB_INTERFACE_SUBCLASS_BOOT: Integer; cdecl;
    {class} function _GetUSB_SUBCLASS_VENDOR_SPEC: Integer; cdecl;
    {class} function _GetUSB_TYPE_CLASS: Integer; cdecl;
    {class} function _GetUSB_TYPE_MASK: Integer; cdecl;
    {class} function _GetUSB_TYPE_RESERVED: Integer; cdecl;
    {class} function _GetUSB_TYPE_STANDARD: Integer; cdecl;
    {class} function _GetUSB_TYPE_VENDOR: Integer; cdecl;
    {class} function init: JUsbConstants; cdecl;
    {class} property USB_CLASS_APP_SPEC: Integer read _GetUSB_CLASS_APP_SPEC;
    {class} property USB_CLASS_AUDIO: Integer read _GetUSB_CLASS_AUDIO;
    {class} property USB_CLASS_CDC_DATA: Integer read _GetUSB_CLASS_CDC_DATA;
    {class} property USB_CLASS_COMM: Integer read _GetUSB_CLASS_COMM;
    {class} property USB_CLASS_CONTENT_SEC: Integer read _GetUSB_CLASS_CONTENT_SEC;
    {class} property USB_CLASS_CSCID: Integer read _GetUSB_CLASS_CSCID;
    {class} property USB_CLASS_HID: Integer read _GetUSB_CLASS_HID;
    {class} property USB_CLASS_HUB: Integer read _GetUSB_CLASS_HUB;
    {class} property USB_CLASS_MASS_STORAGE: Integer read _GetUSB_CLASS_MASS_STORAGE;
    {class} property USB_CLASS_MISC: Integer read _GetUSB_CLASS_MISC;
    {class} property USB_CLASS_PER_INTERFACE: Integer read _GetUSB_CLASS_PER_INTERFACE;
    {class} property USB_CLASS_PHYSICA: Integer read _GetUSB_CLASS_PHYSICA;
    {class} property USB_CLASS_PRINTER: Integer read _GetUSB_CLASS_PRINTER;
    {class} property USB_CLASS_STILL_IMAGE: Integer read _GetUSB_CLASS_STILL_IMAGE;
    {class} property USB_CLASS_VENDOR_SPEC: Integer read _GetUSB_CLASS_VENDOR_SPEC;
    {class} property USB_CLASS_VIDEO: Integer read _GetUSB_CLASS_VIDEO;
    {class} property USB_CLASS_WIRELESS_CONTROLLER: Integer read _GetUSB_CLASS_WIRELESS_CONTROLLER;
    {class} property USB_DIR_IN: Integer read _GetUSB_DIR_IN;
    {class} property USB_DIR_OUT: Integer read _GetUSB_DIR_OUT;
    {class} property USB_ENDPOINT_DIR_MASK: Integer read _GetUSB_ENDPOINT_DIR_MASK;
    {class} property USB_ENDPOINT_NUMBER_MASK: Integer read _GetUSB_ENDPOINT_NUMBER_MASK;
    {class} property USB_ENDPOINT_XFERTYPE_MASK: Integer read _GetUSB_ENDPOINT_XFERTYPE_MASK;
    {class} property USB_ENDPOINT_XFER_BULK: Integer read _GetUSB_ENDPOINT_XFER_BULK;
    {class} property USB_ENDPOINT_XFER_CONTROL: Integer read _GetUSB_ENDPOINT_XFER_CONTROL;
    {class} property USB_ENDPOINT_XFER_INT: Integer read _GetUSB_ENDPOINT_XFER_INT;
    {class} property USB_ENDPOINT_XFER_ISOC: Integer read _GetUSB_ENDPOINT_XFER_ISOC;
    {class} property USB_INTERFACE_SUBCLASS_BOOT: Integer read _GetUSB_INTERFACE_SUBCLASS_BOOT;
    {class} property USB_SUBCLASS_VENDOR_SPEC: Integer read _GetUSB_SUBCLASS_VENDOR_SPEC;
    {class} property USB_TYPE_CLASS: Integer read _GetUSB_TYPE_CLASS;
    {class} property USB_TYPE_MASK: Integer read _GetUSB_TYPE_MASK;
    {class} property USB_TYPE_RESERVED: Integer read _GetUSB_TYPE_RESERVED;
    {class} property USB_TYPE_STANDARD: Integer read _GetUSB_TYPE_STANDARD;
    {class} property USB_TYPE_VENDOR: Integer read _GetUSB_TYPE_VENDOR;
  end;

  [JavaSignature('android/hardware/usb/UsbConstants')]
  JUsbConstants = interface(JObject)
    ['{6713F699-13BB-420A-993F-4B9F326B88EE}']
  end;
  TJUsbConstants = class(TJavaGenericImport<JUsbConstantsClass, JUsbConstants>) end;

  JUsbDeviceClass = interface(JObjectClass)
    ['{23359F82-699F-48E1-B1DD-43DD18455D2D}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function getDeviceId(name: JString): Integer; cdecl; overload;
    {class} function getDeviceName(id: Integer): JString; cdecl; overload;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/hardware/usb/UsbDevice')]
  JUsbDevice = interface(JObject)
    ['{EAD5113C-AC54-4131-BED2-46F449FFD4B7}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getConfiguration(index: Integer): JUsbConfiguration; cdecl;
    function getConfigurationCount: Integer; cdecl;
    function getDeviceClass: Integer; cdecl;
    function getDeviceId: Integer; cdecl; overload;
    function getDeviceName: JString; cdecl; overload;
    function getDeviceProtocol: Integer; cdecl;
    function getDeviceSubclass: Integer; cdecl;
    function getInterface(index: Integer): JUsbInterface; cdecl;
    function getInterfaceCount: Integer; cdecl;
    function getManufacturerName: JString; cdecl;
    function getProductId: Integer; cdecl;
    function getProductName: JString; cdecl;
    function getSerialNumber: JString; cdecl;
    function getVendorId: Integer; cdecl;
    function getVersion: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJUsbDevice = class(TJavaGenericImport<JUsbDeviceClass, JUsbDevice>) end;

  JUsbDeviceConnectionClass = interface(JObjectClass)
    ['{83888555-657F-42A1-9BC1-8B23E2F69899}']
  end;

  [JavaSignature('android/hardware/usb/UsbDeviceConnection')]
  JUsbDeviceConnection = interface(JObject)
    ['{6CC94621-8592-4C7F-B28A-2E644692B85A}']
    function bulkTransfer(endpoint: JUsbEndpoint; buffer: TJavaArray<Byte>; length: Integer; timeout: Integer): Integer; cdecl; overload;
    function bulkTransfer(endpoint: JUsbEndpoint; buffer: TJavaArray<Byte>; offset: Integer; length: Integer;
      timeout: Integer): Integer; cdecl; overload;
    function claimInterface(intf: JUsbInterface; force: Boolean): Boolean; cdecl;
    procedure close; cdecl;
    function controlTransfer(requestType: Integer; request: Integer; value: Integer; index: Integer; buffer: TJavaArray<Byte>; length: Integer;
      timeout: Integer): Integer; cdecl; overload;
    function controlTransfer(requestType: Integer; request: Integer; value: Integer; index: Integer; buffer: TJavaArray<Byte>; offset: Integer;
      length: Integer; timeout: Integer): Integer; cdecl; overload;
    function releaseInterface(intf: JUsbInterface): Boolean; cdecl;
    function requestWait: JUsbRequest; cdecl;
    function getFileDescriptor: Integer; cdecl;
    function getRawDescriptors: TJavaArray<Byte>; cdecl;
    function getSerial: JString; cdecl;
    function setConfiguration(configuration: JUsbConfiguration): Boolean; cdecl;
    function setInterface(intf: JUsbInterface): Boolean; cdecl;
  end;
  TJUsbDeviceConnection = class(TJavaGenericImport<JUsbDeviceConnectionClass, JUsbDeviceConnection>) end;

  JUsbEndpointClass = interface(JObjectClass)
    ['{53DC559E-4126-4589-9CDF-681B6A461496}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/hardware/usb/UsbEndpoint')]
  JUsbEndpoint = interface(JObject)
    ['{A9A1F612-B537-4C37-8523-1B7AEADB1D43}']
    function describeContents: Integer; cdecl;
    function getAddress: Integer; cdecl;
    function getAttributes: Integer; cdecl;
    function getDirection: Integer; cdecl;
    function getEndpointNumber: Integer; cdecl;
    function getInterval: Integer; cdecl;
    function getMaxPacketSize: Integer; cdecl;
    function getType: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJUsbEndpoint = class(TJavaGenericImport<JUsbEndpointClass, JUsbEndpoint>) end;

  JUsbInterfaceClass = interface(JObjectClass)
    ['{59313EE2-7603-4BBC-ACBC-4BC863D31B6C}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/hardware/usb/UsbInterface')]
  JUsbInterface = interface(JObject)
    ['{026556E6-07DF-422D-AF28-BC06795B91E7}']
    function describeContents: Integer; cdecl;
    function getAlternateSetting: Integer; cdecl;
    function getEndpoint(index: Integer): JUsbEndpoint; cdecl;
    function getEndpointCount: Integer; cdecl;
    function getId: Integer; cdecl;
    function getInterfaceClass: Integer; cdecl;
    function getInterfaceProtocol: Integer; cdecl;
    function getInterfaceSubclass: Integer; cdecl;
    function getName: JString; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJUsbInterface = class(TJavaGenericImport<JUsbInterfaceClass, JUsbInterface>) end;

  JUsbManagerClass = interface(JObjectClass)
    ['{1048A6E9-E1B5-4DA5-A168-ED91E8DE5284}']
    {class} function _GetACTION_USB_ACCESSORY_ATTACHED: JString; cdecl;
    {class} function _GetACTION_USB_ACCESSORY_DETACHED: JString; cdecl;
    {class} function _GetACTION_USB_DEVICE_ATTACHED: JString; cdecl;
    {class} function _GetACTION_USB_DEVICE_DETACHED: JString; cdecl;
    {class} function _GetEXTRA_ACCESSORY: JString; cdecl;
    {class} function _GetEXTRA_DEVICE: JString; cdecl;
    {class} function _GetEXTRA_PERMISSION_GRANTED: JString; cdecl;
    {class} property ACTION_USB_ACCESSORY_ATTACHED: JString read _GetACTION_USB_ACCESSORY_ATTACHED;
    {class} property ACTION_USB_ACCESSORY_DETACHED: JString read _GetACTION_USB_ACCESSORY_DETACHED;
    {class} property ACTION_USB_DEVICE_ATTACHED: JString read _GetACTION_USB_DEVICE_ATTACHED;
    {class} property ACTION_USB_DEVICE_DETACHED: JString read _GetACTION_USB_DEVICE_DETACHED;
    {class} property EXTRA_ACCESSORY: JString read _GetEXTRA_ACCESSORY;
    {class} property EXTRA_DEVICE: JString read _GetEXTRA_DEVICE;
    {class} property EXTRA_PERMISSION_GRANTED: JString read _GetEXTRA_PERMISSION_GRANTED;
  end;

  [JavaSignature('android/hardware/usb/UsbManager')]
  JUsbManager = interface(JObject)
    ['{6F603A25-E816-4012-9B23-054B428A4A75}']
    function getAccessoryList: TJavaObjectArray<JUsbAccessory>; cdecl;
    function getDeviceList: JHashMap; cdecl;
    function hasPermission(device: JUsbDevice): Boolean; cdecl; overload;
    function hasPermission(accessory: JUsbAccessory): Boolean; cdecl; overload;
    function openAccessory(accessory: JUsbAccessory): JParcelFileDescriptor; cdecl;
    function openDevice(device: JUsbDevice): JUsbDeviceConnection; cdecl;
    procedure requestPermission(accessory: JUsbAccessory; pi: JPendingIntent); cdecl; overload;
    procedure requestPermission(device: JUsbDevice; pi: JPendingIntent); cdecl; overload;
  end;
  TJUsbManager = class(TJavaGenericImport<JUsbManagerClass, JUsbManager>) end;

  JUsbRequestClass = interface(JObjectClass)
    ['{8A8E6489-7B33-4CCC-B25E-2847FD29DA80}']
    {class} function init: JUsbRequest; cdecl;
  end;

  [JavaSignature('android/hardware/usb/UsbRequest')]
  JUsbRequest = interface(JObject)
    ['{C192EBAE-64F9-46FD-9E81-CB44E9D42FB1}']
    function cancel: Boolean; cdecl;
    procedure close; cdecl;
    function getClientData: JObject; cdecl;
    function getEndpoint: JUsbEndpoint; cdecl;
    function initialize(connection: JUsbDeviceConnection; endpoint: JUsbEndpoint): Boolean; cdecl;
    function queue(buffer: JByteBuffer; length: Integer): Boolean; cdecl;
    procedure setClientData(data: JObject); cdecl;
  end;
  TJUsbRequest = class(TJavaGenericImport<JUsbRequestClass, JUsbRequest>) end;

implementation

end.

