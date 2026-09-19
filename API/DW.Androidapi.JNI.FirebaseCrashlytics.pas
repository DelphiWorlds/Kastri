unit DW.Androidapi.JNI.FirebaseCrashlytics;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{  Contributed 2025 by Denis Hrastnik                   }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os,
  Androidapi.JNI.Util, Androidapi.JNI.PlayServices.Tasks;

type
  JCrash = interface;
  JFirebaseCrashlytics = interface;
  JCustomKeysAndValues = interface;
  JCustomKeysAndValues_Builder = interface;

  { Crashytics }

  JCrashClass = interface(JObjectClass)
    ['{4798A4C8-B033-49E7-97B9-349874ACABC9}']
    function init: JCrash; cdecl;
  end;

  [JavaSignature('com/crash/Crash')]
  JCrash = interface(JObject)
    ['{07ABF6E5-B235-4B43-A723-717C54CB837F}']
  end;
  TJCrash = class(TJavaGenericImport<JCrashClass, JCrash>) end;

  JCustomKeysAndValuesClass = interface(JObjectClass)
    ['{D28E72D9-AEA6-46AD-827A-3AA8276D4E2C}']
    {class} function init(builder: JCustomKeysAndValues_Builder): JCustomKeysAndValues; cdecl;
  end;

  [JavaSignature('com/google/firebase/crashlytics/CustomKeysAndValues')]
  JCustomKeysAndValues = interface(JObject)
    ['{6AAA6866-A552-4459-A708-BD54BE24E5BF}']
    function _GetkeysAndValues: JMap; cdecl;
    property keysAndValues: JMap read _GetkeysAndValues;
  end;
  TJCustomKeysAndValues = class(TJavaGenericImport<JCustomKeysAndValuesClass, JCustomKeysAndValues>) end;

  JCustomKeysAndValues_BuilderClass = interface(JObjectClass)
    ['{6C5766EE-6057-45BA-9BE8-B827F031B02D}']
    {class} function init: JCustomKeysAndValues_Builder; cdecl;
  end;

  [JavaSignature('com/google/firebase/crashlytics/CustomKeysAndValues$Builder')]
  JCustomKeysAndValues_Builder = interface(JObject)
    ['{BFE805B3-51CF-43D6-B76B-A9E097A1EA99}']
    function build: JCustomKeysAndValues; cdecl;
    function putBoolean(string_: JString; b: Boolean): JCustomKeysAndValues_Builder; cdecl;
    function putDouble(string_: JString; d: Double): JCustomKeysAndValues_Builder; cdecl;
    function putFloat(string_: JString; f: Single): JCustomKeysAndValues_Builder; cdecl;
    function putInt(string_: JString; i: Integer): JCustomKeysAndValues_Builder; cdecl;
    function putLong(string_: JString; l: Int64): JCustomKeysAndValues_Builder; cdecl;
    function putString(string_: JString; string_1: JString): JCustomKeysAndValues_Builder; cdecl;
  end;
  TJCustomKeysAndValues_Builder = class(TJavaGenericImport<JCustomKeysAndValues_BuilderClass, JCustomKeysAndValues_Builder>) end;

  JFirebaseCrashlyticsClass = interface(JObjectClass)
    ['{0CBE882E-FE74-4FB4-B9EB-F6B45A96A0F4}']
    {class} function getInstance: JFirebaseCrashlytics; cdecl;
  end;

  [JavaSignature('com/google/firebase/crashlytics/FirebaseCrashlytics')]
  JFirebaseCrashlytics = interface(JObject)
    ['{53A94CAE-3EB5-4AAE-A031-BA6B602575C8}']
    // function checkForUnsentReports: JTask; cdecl;
    procedure deleteUnsentReports; cdecl;
    function  didCrashOnPreviousExecution: Boolean; cdecl;
    procedure log(msg: JString); cdecl;
    procedure recordException(throwable: JThrowable); cdecl;
    procedure sendUnsentReports; cdecl;
    procedure setCrashlyticsCollectionEnabled(enabled: Boolean); cdecl; overload;
    procedure setCrashlyticsCollectionEnabled(enabled: JBoolean); cdecl; overload;
    procedure setCustomKey(key: JString; value: JString); cdecl; overload;
    procedure setCustomKey(key: JString; value: Single); cdecl; overload;
    procedure setCustomKey(key: JString; value: Double); cdecl; overload;
    procedure setCustomKey(key: JString; value: Boolean); cdecl; overload;
    procedure setCustomKey(key: JString; value: Int64); cdecl; overload;
    procedure setCustomKey(key: JString; value: Integer); cdecl; overload;
    procedure setCustomKeys(keysAndValues: JCustomKeysAndValues); cdecl;
    procedure setUserId(identifier: JString); cdecl;
  end;
  TJFirebaseCrashlytics = class(TJavaGenericImport<JFirebaseCrashlyticsClass, JFirebaseCrashlytics>) end;

implementation

end.

