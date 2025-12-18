unit DW.Androidapi.JNI.PlayServices.AgeSignals;

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

// NOTES:
//   This unit requires the .jar file for Age Signals to be added to the Libraries node of the Android platform.
//   As at 19-DEC-2025, this is age-signals-0.0.2.jar in the ThirdParty/Android folder
//   The Age Signals service should be live on 01-JAN-2026, so AgeSignalsManager will error until that time.
//   Use FakeAgeSignalsManager for testing

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Java.Security, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os, Androidapi.JNI.PlayServices.Tasks;

type
  JAgeSignalsManager = interface;
  JAgeSignalsManagerFactory = interface;
  JAgeSignalsRequest = interface;
  JAgeSignalsRequest_Builder = interface;
  JAgeSignalsResult = interface;
  JAgeSignalsResult_Builder = interface;
  JAgeSignalsErrorCode = interface;
  JAgeSignalsVerificationStatus = interface;
  JIAgeSignalsService = interface;
  JIAgeSignalsServiceCallback = interface;
  JFakeAgeSignalsManager = interface;

  JAgeSignalsManagerClass = interface(IJavaClass)
    ['{50F291A9-5114-4155-A17F-BC52B2773536}']
  end;

  [JavaSignature('com/google/android/play/agesignals/AgeSignalsManager')]
  JAgeSignalsManager = interface(IJavaInstance)
    ['{59657CCC-0F08-41E9-9746-333AF0B84C27}']
    function checkAgeSignals(ageSignalsRequest: JAgeSignalsRequest): JTask; cdecl;
  end;
  TJAgeSignalsManager = class(TJavaGenericImport<JAgeSignalsManagerClass, JAgeSignalsManager>) end;

  JAgeSignalsManagerFactoryClass = interface(JObjectClass)
    ['{BEC89430-7B88-4E3A-8FDC-8430D516781B}']
    {class} function create(context: JContext): JAgeSignalsManager; cdecl;
  end;

  [JavaSignature('com/google/android/play/agesignals/AgeSignalsManagerFactory')]
  JAgeSignalsManagerFactory = interface(JObject)
    ['{96BEBB46-B31A-49C2-8AF1-629352BED87D}']
  end;
  TJAgeSignalsManagerFactory = class(TJavaGenericImport<JAgeSignalsManagerFactoryClass, JAgeSignalsManagerFactory>) end;

  JAgeSignalsRequestClass = interface(JObjectClass)
    ['{487B2B24-A197-43BB-BC19-AA7D74905921}']
    {class} function builder: JAgeSignalsRequest_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/agesignals/AgeSignalsRequest')]
  JAgeSignalsRequest = interface(JObject)
    ['{B0E4F006-66B1-4A5D-A619-CEC27E5ED043}']
  end;
  TJAgeSignalsRequest = class(TJavaGenericImport<JAgeSignalsRequestClass, JAgeSignalsRequest>) end;

  JAgeSignalsRequest_BuilderClass = interface(JObjectClass)
    ['{D156A999-0D79-4D49-A260-C7CBD6E511E5}']
  end;

  [JavaSignature('com/google/android/play/agesignals/AgeSignalsRequest$Builder')]
  JAgeSignalsRequest_Builder = interface(JObject)
    ['{C325D141-0E19-48C9-AE6A-DA011CCC2C38}']
    function build: JAgeSignalsRequest; cdecl;
  end;
  TJAgeSignalsRequest_Builder = class(TJavaGenericImport<JAgeSignalsRequest_BuilderClass, JAgeSignalsRequest_Builder>) end;

  JAgeSignalsResultClass = interface(JObjectClass)
    ['{F6DA8BD2-D5A7-4240-B1BF-3B4BAC689B5C}']
    {class} function init: JAgeSignalsResult; cdecl;
    {class} function builder: JAgeSignalsResult_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/agesignals/AgeSignalsResult')]
  JAgeSignalsResult = interface(JObject)
    ['{1AD1114C-2965-407A-A832-C84DBEFD6B4A}']
    function ageLower: JInteger; cdecl;
    function ageUpper: JInteger; cdecl;
    function installId: JString; cdecl;
    function mostRecentApprovalDate: JDate; cdecl;
    function userStatus: JInteger; cdecl;
  end;
  TJAgeSignalsResult = class(TJavaGenericImport<JAgeSignalsResultClass, JAgeSignalsResult>) end;

  JAgeSignalsResult_BuilderClass = interface(JObjectClass)
    ['{D7E91CB0-A2F1-4A7E-93EC-E0B0BEABCC11}']
    {class} function init: JAgeSignalsResult_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/play/agesignals/AgeSignalsResult$Builder')]
  JAgeSignalsResult_Builder = interface(JObject)
    ['{E9F9AA85-5019-42DC-8DF6-E2961E61D349}']
    function build: JAgeSignalsResult; cdecl;
    function setAgeLower(integer_: JInteger): JAgeSignalsResult_Builder; cdecl;
    function setAgeUpper(integer_: JInteger): JAgeSignalsResult_Builder; cdecl;
    function setInstallId(string_: JString): JAgeSignalsResult_Builder; cdecl;
    function setMostRecentApprovalDate(date: JDate): JAgeSignalsResult_Builder; cdecl;
    function setUserStatus(integer_: JInteger): JAgeSignalsResult_Builder; cdecl;
  end;
  TJAgeSignalsResult_Builder = class(TJavaGenericImport<JAgeSignalsResult_BuilderClass, JAgeSignalsResult_Builder>) end;

  JAgeSignalsErrorCodeClass = interface(JAnnotationClass)
    ['{20E3974D-FB34-4617-A54D-C8936CD0B11F}']
    {class} function _GetAPI_NOT_AVAILABLE: Integer; cdecl;
    {class} function _GetAPP_NOT_OWNED: Integer; cdecl;
    {class} function _GetCANNOT_BIND_TO_SERVICE: Integer; cdecl;
    {class} function _GetCLIENT_TRANSIENT_ERROR: Integer; cdecl;
    {class} function _GetINTERNAL_ERROR: Integer; cdecl;
    {class} function _GetNETWORK_ERROR: Integer; cdecl;
    {class} function _GetNO_ERROR: Integer; cdecl;
    {class} function _GetPLAY_SERVICES_NOT_FOUND: Integer; cdecl;
    {class} function _GetPLAY_SERVICES_VERSION_OUTDATED: Integer; cdecl;
    {class} function _GetPLAY_STORE_NOT_FOUND: Integer; cdecl;
    {class} function _GetPLAY_STORE_VERSION_OUTDATED: Integer; cdecl;
    {class} property API_NOT_AVAILABLE: Integer read _GetAPI_NOT_AVAILABLE;
    {class} property APP_NOT_OWNED: Integer read _GetAPP_NOT_OWNED;
    {class} property CANNOT_BIND_TO_SERVICE: Integer read _GetCANNOT_BIND_TO_SERVICE;
    {class} property CLIENT_TRANSIENT_ERROR: Integer read _GetCLIENT_TRANSIENT_ERROR;
    {class} property INTERNAL_ERROR: Integer read _GetINTERNAL_ERROR;
    {class} property NETWORK_ERROR: Integer read _GetNETWORK_ERROR;
    {class} property NO_ERROR: Integer read _GetNO_ERROR;
    {class} property PLAY_SERVICES_NOT_FOUND: Integer read _GetPLAY_SERVICES_NOT_FOUND;
    {class} property PLAY_SERVICES_VERSION_OUTDATED: Integer read _GetPLAY_SERVICES_VERSION_OUTDATED;
    {class} property PLAY_STORE_NOT_FOUND: Integer read _GetPLAY_STORE_NOT_FOUND;
    {class} property PLAY_STORE_VERSION_OUTDATED: Integer read _GetPLAY_STORE_VERSION_OUTDATED;
  end;

  [JavaSignature('com/google/android/play/agesignals/model/AgeSignalsErrorCode')]
  JAgeSignalsErrorCode = interface(JAnnotation)
    ['{E1C81F4C-3777-4AC9-A40C-B9E09489B954}']
  end;
  TJAgeSignalsErrorCode = class(TJavaGenericImport<JAgeSignalsErrorCodeClass, JAgeSignalsErrorCode>) end;

  JAgeSignalsVerificationStatusClass = interface(JAnnotationClass)
    ['{AADD02F8-500A-42EC-BD0F-649C736C30C3}']
    {class} function _GetSUPERVISED: Integer; cdecl;
    {class} function _GetSUPERVISED_APPROVAL_DENIED: Integer; cdecl;
    {class} function _GetSUPERVISED_APPROVAL_PENDING: Integer; cdecl;
    {class} function _GetUNKNOWN: Integer; cdecl;
    {class} function _GetVERIFIED: Integer; cdecl;
    {class} property SUPERVISED: Integer read _GetSUPERVISED;
    {class} property SUPERVISED_APPROVAL_DENIED: Integer read _GetSUPERVISED_APPROVAL_DENIED;
    {class} property SUPERVISED_APPROVAL_PENDING: Integer read _GetSUPERVISED_APPROVAL_PENDING;
    {class} property UNKNOWN: Integer read _GetUNKNOWN;
    {class} property VERIFIED: Integer read _GetVERIFIED;
  end;

  [JavaSignature('com/google/android/play/agesignals/model/AgeSignalsVerificationStatus')]
  JAgeSignalsVerificationStatus = interface(JAnnotation)
    ['{767A2547-B9F5-46D0-A923-D98DA1B4B01E}']
  end;
  TJAgeSignalsVerificationStatus = class(TJavaGenericImport<JAgeSignalsVerificationStatusClass, JAgeSignalsVerificationStatus>) end;

  JIAgeSignalsServiceClass = interface(JIInterfaceClass)
    ['{B389CA65-7D95-4DAB-BC0F-C04BB0BF6BA7}']
  end;

  [JavaSignature('com/google/android/play/agesignals/protocol/IAgeSignalsService')]
  JIAgeSignalsService = interface(JIInterface)
    ['{47703EEC-5554-4C23-A776-6A4FA9A04509}']
    procedure checkAgeRange(string_: JString; bundle: JBundle; iAgeSignalsServiceCallback: JIAgeSignalsServiceCallback); cdecl;
  end;
  TJIAgeSignalsService = class(TJavaGenericImport<JIAgeSignalsServiceClass, JIAgeSignalsService>) end;

  JIAgeSignalsServiceCallbackClass = interface(JIInterfaceClass)
    ['{56623359-68E6-4F3D-B320-4D48087DC6C2}']
  end;

  [JavaSignature('com/google/android/play/agesignals/protocol/IAgeSignalsServiceCallback')]
  JIAgeSignalsServiceCallback = interface(JIInterface)
    ['{AB7D05C5-B471-4EFD-A4B4-304FF2CF54BB}']
    procedure onCompleteCheckAgeSignals(bundle: JBundle); cdecl;
    procedure onError(bundle: JBundle); cdecl;
  end;
  TJIAgeSignalsServiceCallback = class(TJavaGenericImport<JIAgeSignalsServiceCallbackClass, JIAgeSignalsServiceCallback>) end;

  JFakeAgeSignalsManagerClass = interface(JObjectClass)
    ['{C5F4D35C-BC77-4148-97BB-436A908D2E27}']
    {class} function init: JFakeAgeSignalsManager; cdecl;
  end;

  [JavaSignature('com/google/android/play/agesignals/testing/FakeAgeSignalsManager')]
  JFakeAgeSignalsManager = interface(JObject)
    ['{411A0939-619A-4929-BC4F-3647375A6D5A}']
    function checkAgeSignals(ageSignalsRequest: JAgeSignalsRequest): JTask; cdecl;
    procedure setNextAgeSignalsResult(ageSignalsResult: JAgeSignalsResult); cdecl;
  end;
  TJFakeAgeSignalsManager = class(TJavaGenericImport<JFakeAgeSignalsManagerClass, JFakeAgeSignalsManager>) end;

implementation

end.
