unit iOSapi.FirebaseCommon;

{*******************************************************}
{                                                       }
{            CodeGear Delphi Runtime Library            }
{                                                       }
{ Copyright(c) 2010-2023 Embarcadero Technologies, Inc. }
{                  All rights reserved                  }
{                                                       }
{*******************************************************}

interface

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes, iOSapi.Foundation;

const
  FIRInstanceIDErrorUnknown = 0;
  FIRInstanceIDErrorAuthentication = 1;
  FIRInstanceIDErrorNoAccess = 2;
  FIRInstanceIDErrorTimeout = 3;
  FIRInstanceIDErrorNetwork = 4;
  FIRInstanceIDErrorOperationInProgress = 5;
  FIRInstanceIDErrorInvalidRequest = 7;
  FIRInstanceIDAPNSTokenTypeUnknown = 0;
  FIRInstanceIDAPNSTokenTypeSandbox = 1;
  FIRInstanceIDAPNSTokenTypeProd = 2;

type
  FIRInstanceIDError = NSUInteger;

  FIRAppClass = interface(NSObjectClass)
    ['{B8962096-555F-498E-B102-8EC66E871EF2}']
    {class} procedure configure; cdecl;
  end;

  FIRApp = interface(NSObject)
    ['{FFF4B247-25C6-47B8-BBC5-893D2170EFA5}']
  end;
  TFIRApp = class(TOCGenericImport<FIRAppClass, FIRApp>) end;

  FIRInstanceIDClass = interface(NSObjectClass)
    ['{4A9F1C85-AEDE-4284-A7DC-0EF9111504B1}']
    {class} function instanceID: pointer; cdecl;
  end;

  FIRInstanceID = interface(NSObject)
    ['{2967A1F9-98F5-40E6-8BDA-A25D3C699ED3}']
    function token: NSString; cdecl;
  end;
  TFIRInstanceID = class(TOCGenericImport<FIRInstanceIDClass, FIRInstanceID>) end;

implementation

uses
  System.Sqlite, System.ZLib,
  iOSapi.StoreKit;

const
  libSystemConfiguration = '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration';

// DW - Added support for iOS simulator
procedure ClangRTLoader; cdecl;
  {$IF not Defined(IOSSIMULATOR)}
  external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
  {$ELSE}
  external '/usr/lib/clang/lib/darwin/libclang_rt.iossim.a';
  {$ENDIF}
procedure FirebaseAnalyticsLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'FirebaseAnalytics';
procedure FirebaseCoreLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'FirebaseCore';
procedure FirebaseCoreDiagnosticsLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'FirebaseCoreDiagnostics';
procedure FirebaseInstallationsLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'FirebaseInstallations';
procedure FoundationLoader; cdecl; external {$IFDEF IOS32}libFoundation{$ELSE}framework 'Foundation'{$ENDIF};
procedure GoogleAppMeasurementLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleAppMeasurement';
procedure GoogleDataTransportLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleDataTransport';
procedure GoogleUtilitiesLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'GoogleUtilities';
procedure nanopbLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'nanopb';
procedure PromisesObjCLoader; cdecl; external {$IFNDEF IOS32}framework{$ENDIF} 'PromisesObjC';
procedure SystemConfigurationLoader; cdecl; external {$IFDEF IOS32}libSystemConfiguration{$ELSE}framework 'SystemConfiguration'{$ENDIF};
procedure StoreKitLoader; cdecl; external {$IFDEF IOS32}libStoreKit{$ELSE}framework 'StoreKit'{$ENDIF};

end.
