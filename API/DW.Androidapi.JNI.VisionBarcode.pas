unit DW.Androidapi.JNI.VisionBarcode;

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


// NOTE: Barcode (JBarcode) is NOT a complete import, as it omits the subclasses. These might be added later
//
// In order to use this code, you will need to:
//   1. Obtain the Google Play Services package using the Android SDK Manager
//   2. Extract classes.jar from each of:
//        \play-services-basement\x.y.z\play-services-basement-x.y.z.aar
//        \play-services-vision\x.y.z\play-services-vision-x.y.z.aar
//      Located in <SDKRoot>\extras\google\m2repository\com\google\android\gms, where x.y.z is the version number, e.g. 11.0.4
//   3. Rename each classes.jar appropriately, and add them to the Libraries node for the Android platform for the project in the Delphi Project Manager
//   4. Disable the other google-play-xxxx.dex.jar packages in the Delphi Project Manager (the project will otherwise not compile)
//   5. Add this meta-data tag to your AndroidManifest.template.xml inside the <application> node:
//        <meta-data android:name="com.google.android.gms.version" android:value="nnnnnnnn"/>
//      Where nnnnnnnn is the Google Play Services version that corresponds to the packages being used. (11.0.4 has a version number of 10298000)
//
// Example code:
//    var
//      LDetector: JBarcodeDetector;
//      LDetectorBuilder: JBarcodeDetector_Builder;
//      LFrameBuilder: JFrame_Builder;
//      LResults: JSparseArray;
//      LBarcode: JBarcode;
//      LObject: JObject;
//      LCode: string;
//    begin
//      LDetectorBuilder := TJBarcodeDetector_Builder.JavaClass.init(TAndroidHelper.Context);
//      LDetectorBuilder.setBarcodeFormats(TJBarcode.JavaClass.QR_CODE);
//      LDetector := LDetectorBuilder.build;
//      LFrameBuilder := TJFrame_Builder.Create;
//      LFrameBuilder.setBitmap(ABitmap); // <---- ABitmap is an instance of a JBitmap that you will need to create from the acquired image
//      LResults := FDetector.detect(LFrameBuilder.build);
//      if LResults.size > 0 then
//      begin
//        LObject := LResults.get(LResults.keyAt(0));
//        LBarcode := TJBarcode.Wrap((LObject as ILocalObject).GetObjectID);
//        LCode := JStringToString(LBarcode.displayValue); // <---- LCode now has the scanned code
//      end;
//    end;

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os, Androidapi.JNI.Util;

type
  JDetector = interface;
  JDetector_Detections = interface;
  JDetector_Processor = interface;
  JMultiProcessor = interface;
  JFrame = interface;
  JFrame_Builder = interface;
  JFrame_Metadata = interface;
  JBarcodeDetector = interface;
  JBarcodeDetector_Builder = interface;
  JBarcode = interface;

  JDetectorClass = interface(JObjectClass)
    ['{0B3D27AA-F6B9-486F-90FF-221CFE33E05C}']
    {class} function init: JDetector; cdecl;
  end;

  [JavaSignature('com/google/android/gms/vision/Detector')]
  JDetector = interface(JObject)
    ['{57FF4389-3FB8-479F-8E5D-78DE91A3DC6E}']
    function detect(frame: JFrame): JSparseArray; cdecl;
    function isOperational: Boolean; cdecl;
    procedure receiveFrame(frame: JFrame); cdecl;
    procedure release; cdecl;
    function setFocus(id: Integer): Boolean; cdecl;
    procedure setProcessor(processor: JDetector_Processor); cdecl;
  end;
  TJDetector = class(TJavaGenericImport<JDetectorClass, JDetector>) end;

  JDetector_DetectionsClass = interface(JObjectClass)
    ['{BA2033BA-DEBF-452C-AE6E-CEB84679600D}']
    {class} function init(P1: JSparseArray; P2: JFrame_Metadata; P3: Boolean): JDetector_Detections; cdecl;
  end;

  [JavaSignature('com/google/android/gms/vision/Detector$Detections')]
  JDetector_Detections = interface(JObject)
    ['{8B1C17D7-903D-4190-BAF8-E7D09A100C63}']
    function detectorIsOperational: Boolean; cdecl;
    function getDetectedItems: JSparseArray; cdecl;
    function getFrameMetadata: JFrame_Metadata; cdecl;
  end;
  TJDetector_Detections = class(TJavaGenericImport<JDetector_DetectionsClass, JDetector_Detections>) end;

  JDetector_ProcessorClass = interface(IJavaClass)
    ['{7D041558-B37F-4464-94B2-171A40AC56A2}']
  end;

  [JavaSignature('com/google/android/gms/vision/Detector$Processor')]
  JDetector_Processor = interface(IJavaInstance)
    ['{50EB5935-9F01-479E-9896-60E6D34B66B1}']
    procedure receiveDetections(detections: JDetector_Detections); cdecl;
    procedure release; cdecl;
  end;
  TJDetector_Processor = class(TJavaGenericImport<JDetector_ProcessorClass, JDetector_Processor>) end;

  JMultiProcessorClass = interface(JDetector_ProcessorClass)
    ['{334BF1A8-69F1-4550-ACBC-162E661961BD}']
  end;

  [JavaSignature('com/google/android/gms/vision/MultiProcessor')]
  JMultiProcessor = interface(JDetector_Processor)
    ['{45FD25EF-53F3-43AE-9A80-B303156BCB90}']
    procedure receiveDetections(detections: JDetector_Detections); cdecl;
    procedure release; cdecl;
  end;
  TJMultiProcessor = class(TJavaGenericImport<JMultiProcessorClass, JMultiProcessor>) end;

  JFrameClass = interface(JObjectClass)
    ['{0F3080D2-2351-4044-AC47-4A08FD68285E}']
    {class} function _GetROTATION_0: Integer; cdecl;
    {class} function _GetROTATION_180: Integer; cdecl;
    {class} function _GetROTATION_270: Integer; cdecl;
    {class} function _GetROTATION_90: Integer; cdecl;
    {class} function getBitmap: JBitmap; cdecl;
    {class} function getGrayscaleImageData: JByteBuffer; cdecl;
    {class} function getMetadata: JFrame_Metadata; cdecl;
    {class} property ROTATION_0: Integer read _GetROTATION_0;
    {class} property ROTATION_180: Integer read _GetROTATION_180;
    {class} property ROTATION_270: Integer read _GetROTATION_270;
    {class} property ROTATION_90: Integer read _GetROTATION_90;
  end;

  [JavaSignature('com/google/android/gms/vision/Frame')]
  JFrame = interface(JObject)
    ['{DA586455-C0B5-4F21-8A62-B4496AF66855}']
  end;
  TJFrame = class(TJavaGenericImport<JFrameClass, JFrame>) end;

  JFrame_BuilderClass = interface(JObjectClass)
    ['{CA1D4337-CAA3-475A-8878-E405E242181D}']
  end;

  [JavaSignature('com/google/android/gms/vision/Frame$Builder')]
  JFrame_Builder = interface(JObject)
    ['{2D580C98-4C18-4F4E-9BA2-E40F5EA3C92B}']
    function build: JFrame; cdecl;
    function setBitmap(bitmap: JBitmap): JFrame_Builder; cdecl;
    function setId(id: Integer): JFrame_Builder; cdecl;
    function setImageData(byteBuffer: JByteBuffer; width, height, format: Integer): JFrame_Builder; cdecl;
    function setRotation(rotation: Integer): JFrame_Builder;
    function setTimestampMillis(timestampMillis: Int64): JFrame_Builder;
  end;
  TJFrame_Builder = class(TJavaGenericImport<JFrame_BuilderClass, JFrame_Builder>) end;

  JFrame_MetadataClass = interface(JObjectClass)
    ['{9C6A5A09-3136-4A88-A43E-BD63322FABCA}']
    {class} function init: JFrame_Metadata; cdecl; overload;
    {class} function init(metadata: JFrame_Metadata): JFrame_Metadata; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/vision/Frame$Metadata')]
  JFrame_Metadata = interface(JObject)
    ['{5A1A9337-FCFE-43DE-A0D0-7A94D801A0A8}']
    function getFormat: Integer; cdecl;
    function getHeight: Integer; cdecl;
    function getId: Integer; cdecl;
    function getRotation: Integer; cdecl;
    function getTimestampMillis: Int64; cdecl;
    function getWidth: Integer; cdecl;
  end;
  TJFrame_Metadata = class(TJavaGenericImport<JFrame_MetadataClass, JFrame_Metadata>) end;

  JBarcodeDetectorClass = interface(JDetectorClass)
    ['{0247501A-71B1-4C21-864A-95842B80EAE5}']
  end;

  [JavaSignature('com/google/android/gms/vision/barcode/BarcodeDetector')]
  JBarcodeDetector = interface(JDetector)
    ['{E8CB9B69-C160-4166-81AE-EE45DFC7D992}']
    function detect(frame: JFrame): JSparseArray; cdecl;
    function isOperational: Boolean; cdecl;
    procedure release; cdecl;
  end;
  TJBarcodeDetector = class(TJavaGenericImport<JBarcodeDetectorClass, JBarcodeDetector>) end;

  JBarcodeDetector_BuilderClass = interface(JObjectClass)
    ['{7A9FEC25-6CC7-4DF8-8A0B-38BD0FF1EF30}']
    {class} function init(context: JContext): JBarcodeDetector_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/vision/barcode/BarcodeDetector$Builder')]
  JBarcodeDetector_Builder = interface(JObject)
    ['{51906397-DF7E-4885-8404-9091B69F9A5B}']
    function build: JBarcodeDetector; cdecl;
    function setBarcodeFormats(format: Integer): JBarcodeDetector_Builder; cdecl;
  end;
  TJBarcodeDetector_Builder = class(TJavaGenericImport<JBarcodeDetector_BuilderClass, JBarcodeDetector_Builder>) end;

  JBarcodeClass = interface(JObjectClass)
    ['{A9EE85A9-DDAE-45D3-B0D7-101C60982B7D}']
    function _GetALL_FORMATS: Integer; cdecl;
    function _GetAZTEC: Integer; cdecl;
    function _GetCALENDAR_EVENT: Integer; cdecl;
    function _GetCODABAR: Integer; cdecl;
    function _GetCODE_128: Integer; cdecl;
    function _GetCODE_39: Integer; cdecl;
    function _GetCODE_93: Integer; cdecl;
    function _GetCONTACT_INFO: Integer; cdecl;
    function _GetCREATOR: JParcelable_Creator; cdecl;
    function _GetDATA_MATRIX: Integer; cdecl;
    function _GetDRIVER_LICENSE: Integer; cdecl;
    function _GetEAN_13: Integer; cdecl;
    function _GetEAN_8: Integer; cdecl;
    function _GetEMAIL: Integer; cdecl;
    function _GetGEO: Integer; cdecl;
    function _GetISBN: Integer; cdecl;
    function _GetITF: Integer; cdecl;
    function _GetPDF417: Integer; cdecl;
    function _GetPHONE: Integer; cdecl;
    function _GetPRODUCT: Integer; cdecl;
    function _GetQR_CODE: Integer; cdecl;
    function _GetSMS: Integer; cdecl;
    function _GetTEXT: Integer; cdecl;
    function _GetUPC_A: Integer; cdecl;
    function _GetUPC_E: Integer; cdecl;
    function _GetURL: Integer; cdecl;
    function _GetWIFI: Integer; cdecl;
    function init: JBarcode; cdecl; overload;
    property ALL_FORMATS: Integer read _GetALL_FORMATS;
    property AZTEC: Integer read _GetAZTEC;
    property CALENDAR_EVENT: Integer read _GetCALENDAR_EVENT;
    property CODABAR: Integer read _GetCODABAR;
    property CODE_128: Integer read _GetCODE_128;
    property CODE_39: Integer read _GetCODE_39;
    property CODE_93: Integer read _GetCODE_93;
    property CONTACT_INFO: Integer read _GetCONTACT_INFO;
    property CREATOR: JParcelable_Creator read _GetCREATOR;
    property DATA_MATRIX: Integer read _GetDATA_MATRIX;
    property DRIVER_LICENSE: Integer read _GetDRIVER_LICENSE;
    property EAN_13: Integer read _GetEAN_13;
    property EAN_8: Integer read _GetEAN_8;
    property EMAIL: Integer read _GetEMAIL;
    property GEO: Integer read _GetGEO;
    property ISBN: Integer read _GetISBN;
    property ITF: Integer read _GetITF;
    property PDF417: Integer read _GetPDF417;
    property PHONE: Integer read _GetPHONE;
    property PRODUCT: Integer read _GetPRODUCT;
    property QR_CODE: Integer read _GetQR_CODE;
    property SMS: Integer read _GetSMS;
    property TEXT: Integer read _GetTEXT;
    property UPC_A: Integer read _GetUPC_A;
    property UPC_E: Integer read _GetUPC_E;
    property URL: Integer read _GetURL;
    property WIFI: Integer read _GetWIFI;
  end;

  [JavaSignature('com/google/android/gms/vision/barcode/Barcode')]
  JBarcode = interface(JObject)
    ['{44040A5E-2326-4756-834C-39361B82567C}']
    function _GetcornerPoints: TJavaArray<JPoint>; cdecl;
    function _GetdisplayValue: JString; cdecl;
    function _Getformat: Integer; cdecl;
    function _GetrawValue: JString; cdecl;
    function _GetvalueFormat: Integer; cdecl;
    procedure _SetcornerPoints(Value: TJavaArray<JPoint>); cdecl;
    procedure _SetdisplayValue(Value: JString); cdecl;
    procedure _Setformat(Value: Integer); cdecl;
    procedure _SetrawValue(Value: JString); cdecl;
    procedure _SetvalueFormat(Value: Integer); cdecl;
    function getBoundingBox: JRect; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
    property cornerPoints: TJavaArray<JPoint> read _GetcornerPoints write _SetcornerPoints;
    property displayValue: JString read _GetdisplayValue write _SetdisplayValue;
    property format: Integer read _Getformat write _Setformat;
    property rawValue: JString read _GetrawValue write _SetrawValue;
    property valueFormat: Integer read _GetvalueFormat write _SetvalueFormat;
  end;
  TJBarcode = class(TJavaGenericImport<JBarcodeClass, JBarcode>)
  end;

implementation

end.

