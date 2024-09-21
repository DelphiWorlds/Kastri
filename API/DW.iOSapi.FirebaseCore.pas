unit DW.iOSapi.FirebaseCore;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, iOSapi.CocoaTypes, iOSapi.Foundation;

const
  FIRLoggerLevelError = 3;
  FIRLoggerLevelWarning = 4;
  FIRLoggerLevelNotice = 5;
  FIRLoggerLevelInfo = 6;
  FIRLoggerLevelDebug = 7;
  FIRLoggerLevelMin = FIRLoggerLevelError;
  FIRLoggerLevelMax = FIRLoggerLevelDebug;

type
  FIRApp = interface;
  FIRConfiguration = interface;
  FIROptions = interface;

  FIRAppVoidBoolCallback = procedure(success: Boolean) of object;
  FIRLoggerLevel = NSInteger;

  FIRAppClass = interface(NSObjectClass)
    ['{4B9FC595-6540-4A95-89A5-5D1F8D871525}']
    {class} function allApps: NSDictionary; cdecl;
    {class} function appNamed(name: NSString): FIRApp; cdecl;
    {class} procedure configure; cdecl;
    {class} procedure configureWithName(name: NSString; options: FIROptions); cdecl;
    {class} procedure configureWithOptions(options: FIROptions); cdecl;
    {class} function defaultApp: FIRApp; cdecl;
  end;

  FIRApp = interface(NSObject)
    ['{37F6D0F5-4AE1-4A2B-9797-484FD7624896}']
    procedure deleteApp(completion: FIRAppVoidBoolCallback); cdecl;
    function isDataCollectionDefaultEnabled: Boolean; cdecl;
    function name: NSString; cdecl;
    function options: FIROptions; cdecl;
    procedure setDataCollectionDefaultEnabled(dataCollectionDefaultEnabled: Boolean); cdecl;
  end;
  TFIRApp = class(TOCGenericImport<FIRAppClass, FIRApp>) end;

  FIRConfigurationClass = interface(NSObjectClass)
    ['{6368EC08-E4B6-4411-98E6-885E70B15D6F}']
    {class} function sharedInstance: FIRConfiguration; cdecl;
  end;

  FIRConfiguration = interface(NSObject)
    ['{6535ED63-2551-4943-AFAA-97FC507DB159}']
    procedure setLoggerLevel(loggerLevel: FIRLoggerLevel); cdecl;
  end;
  TFIRConfiguration = class(TOCGenericImport<FIRConfigurationClass, FIRConfiguration>) end;

  FIROptionsClass = interface(NSObjectClass)
    ['{E0B22339-4FE7-4117-94C3-B35D0CFDDC63}']
    {class} function defaultOptions: FIROptions; cdecl;
  end;

  FIROptions = interface(NSObject)
    ['{211D64B4-8D4B-4157-899C-30E2F8B7DC8C}']
    function androidClientID: NSString; cdecl;
    function APIKey: NSString; cdecl;
    function appGroupID: NSString; cdecl;
    function bundleID: NSString; cdecl;
    function clientID: NSString; cdecl;
    function databaseURL: NSString; cdecl;
    function deepLinkURLScheme: NSString; cdecl;
    function GCMSenderID: NSString; cdecl;
    function googleAppID: NSString; cdecl;
    function initWithContentsOfFile(plistPath: NSString): Pointer; cdecl;
    function initWithGoogleAppID(googleAppID: NSString; GCMSenderID: NSString): Pointer; cdecl;
    function projectID: NSString; cdecl;
    procedure setAndroidClientID(androidClientID: NSString); cdecl;
    procedure setAPIKey(APIKey: NSString); cdecl;
    procedure setAppGroupID(appGroupID: NSString); cdecl;
    procedure setBundleID(bundleID: NSString); cdecl;
    procedure setClientID(clientID: NSString); cdecl;
    procedure setDatabaseURL(databaseURL: NSString); cdecl;
    procedure setDeepLinkURLScheme(deepLinkURLScheme: NSString); cdecl;
    procedure setGCMSenderID(GCMSenderID: NSString); cdecl;
    procedure setGoogleAppID(googleAppID: NSString); cdecl;
    procedure setProjectID(projectID: NSString); cdecl;
    procedure setStorageBucket(storageBucket: NSString); cdecl;
    procedure setTrackingID(trackingID: NSString); cdecl;
    function storageBucket: NSString; cdecl;
    function trackingID: NSString; cdecl;
  end;
  TFIROptions = class(TOCGenericImport<FIROptionsClass, FIROptions>) end;

implementation

uses
  // iOS
  iOSapi.StoreKit,
  // DW
  DW.iOSapi.SwiftCompat;

const
  libSystemConfiguration = '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration';
  libSwiftUI = '/System/Library/Frameworks/SwiftUI.framework/SwiftUI';

procedure CLangRTLoader; cdecl;
  {$IF not Defined(IOSSIMULATOR)}
  external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a'; // Fixes linker error: ___isOSVersionAtLeast missing (iOS SDK 12.x)
  {$ELSE}
  external '/usr/lib/clang/lib/darwin/libclang_rt.iossim.a'; // Fixes linker error: ___isOSVersionAtLeast missing (iOS SDK 12.x)
  {$ENDIF}
procedure FBLPromisesLoader; cdecl; external framework 'FBLPromises';
procedure FirebaseAnalyticsLoader; cdecl; external framework 'FirebaseAnalytics';
procedure FirebaseCoreLoader; cdecl; external framework 'FirebaseCore';
procedure FirebaseCoreInternalLoader; cdecl; external framework 'FirebaseCoreInternal';
procedure FirebaseInstallationsLoader cdecl; external framework 'FirebaseInstallations';
procedure FoundationLoader; cdecl; external libFoundation;
procedure GoogleAppMeasurementLoader; cdecl; external framework 'GoogleAppMeasurement' dependency 'sqlite3';
procedure GoogleUtilitiesLoader; cdecl; external framework 'GoogleUtilities';
procedure nanoPBLoader; cdecl; external framework 'nanoPB';
procedure SystemConfigurationLoader; cdecl; external libSystemConfiguration;
procedure StoreKitLoader; cdecl; external libStoreKit;
procedure SwiftUILoader; cdecl; external libSwiftUI;

end.