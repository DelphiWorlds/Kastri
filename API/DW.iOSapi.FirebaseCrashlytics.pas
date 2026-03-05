unit DW.iOSapi.FirebaseCrashlytics;

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
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

type
  FIRCrashlyticsReport = interface;
  FIRStackFrame = interface;
  FIRExceptionModel = interface;
  FIRCrashlytics = interface;

  TFIRCrashlyticsBlockMethod1 = procedure(param1: Boolean) of object;
  TFIRCrashlyticsBlockMethod2 = procedure(param1: FIRCrashlyticsReport) of object;

  FIRCrashlyticsReportClass = interface(NSObjectClass)
    ['{2255AD74-84A4-4100-B2C1-8930011C910B}']
  end;

  FIRCrashlyticsReport = interface(NSObject)
    ['{F706B155-1434-42D1-BC1F-EFC32AB68EDA}']
    function dateCreated: NSDate; cdecl;
    function hasCrash: Boolean; cdecl;
    procedure log(msg: NSString); cdecl;
    procedure logWithFormat(format: NSString); overload; cdecl;
    procedure logWithFormat(format: NSString; arguments: Pointer); overload; cdecl;
    function reportID: NSString; cdecl;
    procedure setCustomKeysAndValues(keysAndValues: NSDictionary); cdecl;
    procedure setCustomValue(value: Pointer; forKey: NSString); cdecl;
    procedure setUserID(userID: NSString); cdecl;
  end;
  TFIRCrashlyticsReport = class(TOCGenericImport<FIRCrashlyticsReportClass, FIRCrashlyticsReport>) end;

  FIRStackFrameClass = interface(NSObjectClass)
    ['{A167B19C-2365-4BD7-B2C2-2999EE701F51}']
    {class} function stackFrameWithAddress(address: NSUInteger): Pointer; cdecl;
    {class} function stackFrameWithSymbol(symbol: NSString; &file: NSString; line: NSInteger): Pointer; cdecl;
  end;

  FIRStackFrame = interface(NSObject)
    ['{45C392C0-1ABF-4B2F-A2CE-66532DDFE22B}']
    function initWithSymbol(symbol: NSString; &file: NSString; line: NSInteger): Pointer; cdecl;
  end;
  TFIRStackFrame = class(TOCGenericImport<FIRStackFrameClass, FIRStackFrame>) end;

  FIRExceptionModelClass = interface(NSObjectClass)
    ['{FB6D78A7-7DA3-4627-AF8D-4355EB5AED19}']
    {class} function exceptionModelWithName(name: NSString; reason: NSString): Pointer; cdecl;
  end;

  FIRExceptionModel = interface(NSObject)
    ['{41F402E6-7042-4B55-88CF-1F6C7F2FE311}']
    function initWithName(name: NSString; reason: NSString): Pointer; cdecl;
    procedure setStackTrace(stackTrace: NSArray); cdecl;
    function stackTrace: NSArray; cdecl;
  end;
  TFIRExceptionModel = class(TOCGenericImport<FIRExceptionModelClass, FIRExceptionModel>) end;

  FIRCrashlyticsClass = interface(NSObjectClass)
    ['{0E10D380-78D7-409E-B72C-BBC289B53190}']
    {class} function crashlytics: Pointer; cdecl;
  end;

  FIRCrashlytics = interface(NSObject)
    ['{96005344-DF3A-47BE-BA0B-2C3B2B152753}']
    procedure checkAndUpdateUnsentReportsWithCompletion(completion: TFIRCrashlyticsBlockMethod2); cdecl;
    procedure checkForUnsentReportsWithCompletion(completion: TFIRCrashlyticsBlockMethod1); cdecl;
    procedure deleteUnsentReports; cdecl;
    function didCrashDuringPreviousExecution: Boolean; cdecl;
    function isCrashlyticsCollectionEnabled: Boolean; cdecl;
    procedure log(msg: NSString); cdecl;
    procedure logWithFormat(format: NSString); overload; cdecl;
    procedure logWithFormat(format: NSString; arguments: Pointer); overload; cdecl;
    procedure recordError(error: NSError); overload; cdecl;
    procedure recordError(error: NSError; userInfo: NSDictionary); overload; cdecl;
    procedure recordExceptionModel(exceptionModel: FIRExceptionModel); cdecl;
    procedure sendUnsentReports; cdecl;
    procedure setCrashlyticsCollectionEnabled(enabled: Boolean); cdecl;
    procedure setCustomKeysAndValues(keysAndValues: NSDictionary); cdecl;
    procedure setCustomValue(value: Pointer; forKey: NSString); cdecl;
    procedure setUserID(userID: NSString); cdecl;
  end;
  TFIRCrashlytics = class(TOCGenericImport<FIRCrashlyticsClass, FIRCrashlytics>) end;

implementation

procedure FirebaseCrashlyticsLoader; cdecl; external framework 'FirebaseCrashlytics';

end.