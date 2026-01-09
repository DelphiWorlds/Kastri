unit DW.Androidapi.JNI.Print;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  Androidapi.JNI.Print;

type
  JPrintAttributes_Builder = interface;
  JPrintDocumentInfo_Builder = interface;
  JPrintJob = interface;
  JPrintJobId = interface;
  JPrintJobInfo = interface;
  JPrintJobInfo_Builder = interface;
  JPrintManager = interface;
  JPrinterCapabilitiesInfo = interface;
  JPrinterCapabilitiesInfo_Builder = interface;
  JPrinterId = interface;
  JPrinterInfo = interface;
  JPrinterInfo_Builder = interface;
  JPrintedPdfDocument = interface;

  JPrintAttributes_BuilderClass = interface(JObjectClass)
    ['{79629ED4-5EDA-4072-BD78-DCC49606072C}']
    {class} function init: JPrintAttributes_Builder; cdecl;
  end;

  [JavaSignature('android/print/PrintAttributes$Builder')]
  JPrintAttributes_Builder = interface(JObject)
    ['{33BC1A30-EED3-4196-A465-1A314C19FB78}']
    function build: JPrintAttributes; cdecl;
    function setColorMode(colorMode: Integer): JPrintAttributes_Builder; cdecl;
    function setDuplexMode(duplexMode: Integer): JPrintAttributes_Builder; cdecl;
    function setMediaSize(mediaSize: JPrintAttributes_MediaSize): JPrintAttributes_Builder; cdecl;
    function setMinMargins(margins: JPrintAttributes_Margins): JPrintAttributes_Builder; cdecl;
    function setResolution(resolution: JPrintAttributes_Resolution): JPrintAttributes_Builder; cdecl;
  end;
  TJPrintAttributes_Builder = class(TJavaGenericImport<JPrintAttributes_BuilderClass, JPrintAttributes_Builder>) end;

  JPrintDocumentInfo_BuilderClass = interface(JObjectClass)
    ['{F4DCAD61-419F-4801-918A-D736078103BD}']
    {class} function init(name: JString): JPrintDocumentInfo_Builder; cdecl;
  end;

  [JavaSignature('android/print/PrintDocumentInfo$Builder')]
  JPrintDocumentInfo_Builder = interface(JObject)
    ['{5F9C0E2F-0488-4DEF-A945-99F035C99873}']
    function build: JPrintDocumentInfo; cdecl;
    function setContentType(&type: Integer): JPrintDocumentInfo_Builder; cdecl;
    function setPageCount(pageCount: Integer): JPrintDocumentInfo_Builder; cdecl;
  end;
  TJPrintDocumentInfo_Builder = class(TJavaGenericImport<JPrintDocumentInfo_BuilderClass, JPrintDocumentInfo_Builder>) end;

  JPrintJobClass = interface(JObjectClass)
    ['{D8A5E947-A5B7-4CDF-9EC2-57D79C784BFF}']
  end;

  [JavaSignature('android/print/PrintJob')]
  JPrintJob = interface(JObject)
    ['{EC156C16-8DFC-4918-AA0B-7533FC1B9C45}']
    procedure cancel; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getId: JPrintJobId; cdecl;
    function getInfo: JPrintJobInfo; cdecl;
    function hashCode: Integer; cdecl;
    function isBlocked: Boolean; cdecl;
    function isCancelled: Boolean; cdecl;
    function isCompleted: Boolean; cdecl;
    function isFailed: Boolean; cdecl;
    function isQueued: Boolean; cdecl;
    function isStarted: Boolean; cdecl;
    procedure restart; cdecl;
  end;
  TJPrintJob = class(TJavaGenericImport<JPrintJobClass, JPrintJob>) end;

  JPrintJobIdClass = interface(JObjectClass)
    ['{6415A03C-CB91-458B-9FF2-6DA85A160B5C}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/print/PrintJobId')]
  JPrintJobId = interface(JObject)
    ['{693A2DB3-29A2-4560-9CC6-2B3CEFE0B4FD}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function hashCode: Integer; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJPrintJobId = class(TJavaGenericImport<JPrintJobIdClass, JPrintJobId>) end;

  JPrintJobInfoClass = interface(JObjectClass)
    ['{07A34C36-2D6F-4AC2-AF19-8ECC3DDEA1CB}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetSTATE_BLOCKED: Integer; cdecl;
    {class} function _GetSTATE_CANCELED: Integer; cdecl;
    {class} function _GetSTATE_COMPLETED: Integer; cdecl;
    {class} function _GetSTATE_CREATED: Integer; cdecl;
    {class} function _GetSTATE_FAILED: Integer; cdecl;
    {class} function _GetSTATE_QUEUED: Integer; cdecl;
    {class} function _GetSTATE_STARTED: Integer; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property STATE_BLOCKED: Integer read _GetSTATE_BLOCKED;
    {class} property STATE_CANCELED: Integer read _GetSTATE_CANCELED;
    {class} property STATE_COMPLETED: Integer read _GetSTATE_COMPLETED;
    {class} property STATE_CREATED: Integer read _GetSTATE_CREATED;
    {class} property STATE_FAILED: Integer read _GetSTATE_FAILED;
    {class} property STATE_QUEUED: Integer read _GetSTATE_QUEUED;
    {class} property STATE_STARTED: Integer read _GetSTATE_STARTED;
  end;

  [JavaSignature('android/print/PrintJobInfo')]
  JPrintJobInfo = interface(JObject)
    ['{04D5BE43-1AC1-4D95-A0D0-70E488DF5745}']
    function describeContents: Integer; cdecl;
    function getAttributes: JPrintAttributes; cdecl;
    function getCopies: Integer; cdecl;
    function getCreationTime: Int64; cdecl;
    function getId: JPrintJobId; cdecl;
    function getLabel: JString; cdecl;
    function getPages: TJavaObjectArray<JPageRange>; cdecl;
    function getPrinterId: JPrinterId; cdecl;
    function getState: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJPrintJobInfo = class(TJavaGenericImport<JPrintJobInfoClass, JPrintJobInfo>) end;

  JPrintJobInfo_BuilderClass = interface(JObjectClass)
    ['{1F0CFB3D-003E-4B65-8510-CD5E452FAE92}']
    {class} function init(prototype: JPrintJobInfo): JPrintJobInfo_Builder; cdecl;
  end;

  [JavaSignature('android/print/PrintJobInfo$Builder')]
  JPrintJobInfo_Builder = interface(JObject)
    ['{0F01F981-B85F-4C57-938D-C300567BFAA8}']
    function build: JPrintJobInfo; cdecl;
    procedure putAdvancedOption(key: JString; value: JString); cdecl; overload;
    procedure putAdvancedOption(key: JString; value: Integer); cdecl; overload;
    procedure setAttributes(attributes: JPrintAttributes); cdecl;
    procedure setCopies(copies: Integer); cdecl;
    procedure setPages(pages: TJavaObjectArray<JPageRange>); cdecl;
  end;
  TJPrintJobInfo_Builder = class(TJavaGenericImport<JPrintJobInfo_BuilderClass, JPrintJobInfo_Builder>) end;

  JPrintManagerClass = interface(JObjectClass)
    ['{05466283-B608-48DD-A687-6C092767E363}']
  end;

  [JavaSignature('android/print/PrintManager')]
  JPrintManager = interface(JObject)
    ['{D31DF1A5-D7DD-4C63-A47C-5C8E68EAB2C2}']
    function getPrintJobs: JList; cdecl;
    function print(printJobName: JString; documentAdapter: JPrintDocumentAdapter; attributes: JPrintAttributes): JPrintJob; cdecl;
  end;
  TJPrintManager = class(TJavaGenericImport<JPrintManagerClass, JPrintManager>) end;

  JPrinterCapabilitiesInfoClass = interface(JObjectClass)
    ['{9CA9FA0C-4B1F-462E-8EA0-88F518B4D07C}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/print/PrinterCapabilitiesInfo')]
  JPrinterCapabilitiesInfo = interface(JObject)
    ['{EC0A795D-283E-4CC0-9F7A-640B4EE0AF50}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getColorModes: Integer; cdecl;
    function getDefaults: JPrintAttributes; cdecl;
    function getDuplexModes: Integer; cdecl;
    function getMediaSizes: JList; cdecl;
    function getMinMargins: JPrintAttributes_Margins; cdecl;
    function getResolutions: JList; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJPrinterCapabilitiesInfo = class(TJavaGenericImport<JPrinterCapabilitiesInfoClass, JPrinterCapabilitiesInfo>) end;

  JPrinterCapabilitiesInfo_BuilderClass = interface(JObjectClass)
    ['{985B350B-9B06-41A8-83C7-AB817A5B993A}']
    {class} function init(printerId: JPrinterId): JPrinterCapabilitiesInfo_Builder; cdecl;
  end;

  [JavaSignature('android/print/PrinterCapabilitiesInfo$Builder')]
  JPrinterCapabilitiesInfo_Builder = interface(JObject)
    ['{4FBADB23-D2DD-4647-8478-7EDF75DDD884}']
    function addMediaSize(mediaSize: JPrintAttributes_MediaSize; isDefault: Boolean): JPrinterCapabilitiesInfo_Builder; cdecl;
    function addResolution(resolution: JPrintAttributes_Resolution; isDefault: Boolean): JPrinterCapabilitiesInfo_Builder; cdecl;
    function build: JPrinterCapabilitiesInfo; cdecl;
    function setColorModes(colorModes: Integer; defaultColorMode: Integer): JPrinterCapabilitiesInfo_Builder; cdecl;
    function setDuplexModes(duplexModes: Integer; defaultDuplexMode: Integer): JPrinterCapabilitiesInfo_Builder; cdecl;
    function setMinMargins(margins: JPrintAttributes_Margins): JPrinterCapabilitiesInfo_Builder; cdecl;
  end;
  TJPrinterCapabilitiesInfo_Builder = class(TJavaGenericImport<JPrinterCapabilitiesInfo_BuilderClass, JPrinterCapabilitiesInfo_Builder>) end;

  JPrinterIdClass = interface(JObjectClass)
    ['{FC8F361A-7BDF-4DBB-AFDE-7F67077D8D1A}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/print/PrinterId')]
  JPrinterId = interface(JObject)
    ['{0D873A9B-DE35-4534-BE3C-AA1685F37029}']
    function describeContents: Integer; cdecl;
    function equals(&object: JObject): Boolean; cdecl;
    function getLocalId: JString; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJPrinterId = class(TJavaGenericImport<JPrinterIdClass, JPrinterId>) end;

  JPrinterInfoClass = interface(JObjectClass)
    ['{2B754928-F372-4E49-B764-F895E6819280}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetSTATUS_BUSY: Integer; cdecl;
    {class} function _GetSTATUS_IDLE: Integer; cdecl;
    {class} function _GetSTATUS_UNAVAILABLE: Integer; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property STATUS_BUSY: Integer read _GetSTATUS_BUSY;
    {class} property STATUS_IDLE: Integer read _GetSTATUS_IDLE;
    {class} property STATUS_UNAVAILABLE: Integer read _GetSTATUS_UNAVAILABLE;
  end;

  [JavaSignature('android/print/PrinterInfo')]
  JPrinterInfo = interface(JObject)
    ['{D5C7AD8B-12F1-416B-BC37-11F031EB5447}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getCapabilities: JPrinterCapabilitiesInfo; cdecl;
    function getDescription: JString; cdecl;
    function getId: JPrinterId; cdecl;
    function getName: JString; cdecl;
    function getStatus: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(parcel: JParcel; flags: Integer); cdecl;
  end;
  TJPrinterInfo = class(TJavaGenericImport<JPrinterInfoClass, JPrinterInfo>) end;

  JPrinterInfo_BuilderClass = interface(JObjectClass)
    ['{8018C50E-172A-40CE-A427-7906B488C711}']
    {class} function init(printerId: JPrinterId; name: JString; status: Integer): JPrinterInfo_Builder; cdecl; overload;
    {class} function init(other: JPrinterInfo): JPrinterInfo_Builder; cdecl; overload;
  end;

  [JavaSignature('android/print/PrinterInfo$Builder')]
  JPrinterInfo_Builder = interface(JObject)
    ['{368A0728-F67A-4540-8BF6-474C09A67BAD}']
    function build: JPrinterInfo; cdecl;
    function setCapabilities(capabilities: JPrinterCapabilitiesInfo): JPrinterInfo_Builder; cdecl;
    function setDescription(description: JString): JPrinterInfo_Builder; cdecl;
    function setName(name: JString): JPrinterInfo_Builder; cdecl;
    function setStatus(status: Integer): JPrinterInfo_Builder; cdecl;
  end;
  TJPrinterInfo_Builder = class(TJavaGenericImport<JPrinterInfo_BuilderClass, JPrinterInfo_Builder>) end;

  JPrintedPdfDocumentClass = interface(JPdfDocumentClass)
    ['{9B25F738-1AD6-4196-8CFA-76C5F6E0D89A}']
    {class} function init(context: JContext; attributes: JPrintAttributes): JPrintedPdfDocument; cdecl;
  end;

  [JavaSignature('android/print/pdf/PrintedPdfDocument')]
  JPrintedPdfDocument = interface(JPdfDocument)
    ['{F4F02413-2665-4F41-BB7E-D80483BFC3F5}']
    function getPageContentRect: JRect; cdecl;
    function getPageHeight: Integer; cdecl;
    function getPageWidth: Integer; cdecl;
    function startPage(pageNumber: Integer): JPdfDocument_Page; cdecl;
  end;
  TJPrintedPdfDocument = class(TJavaGenericImport<JPrintedPdfDocumentClass, JPrintedPdfDocument>) end;

implementation

end.

