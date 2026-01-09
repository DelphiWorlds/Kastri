unit DW.iOSapi.DeviceCheck;

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


// **** NOTE: At present, this unit is UNTESTED **** Please use with care
// You will need to add the DeviceCheck framework to the iOS SDK(s) using the SDK Manager in the IDE

interface

uses
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

const
  DCErrorUnknownSystemFailure = 0;
  DCErrorFeatureUnsupported = 1;
  DCErrorInvalidInput = 2;
  DCErrorInvalidKey = 3;
  DCErrorServerUnavailable = 4;

type
  DCDevice = interface;
  DCAppAttestService = interface;

  DCError = NSInteger;
  NSErrorDomain = NSString;

  TDCDeviceBlockMethod1 = procedure(token: NSData; error: NSError) of object;
  TDCAppAttestServiceBlockMethod1 = procedure(keyId: NSString; error: NSError) of object;
  TDCAppAttestServiceBlockMethod2 = procedure(attestationObject: NSData; error: NSError) of object;
  TDCAppAttestServiceBlockMethod3 = procedure(assertionObject: NSData; error: NSError) of object;

  DCDeviceClass = interface(NSObjectClass)
    ['{EDA6CAA0-0DCE-4AD4-9C80-12FFFD77D661}']
    {class} function currentDevice: DCDevice; cdecl;
  end;

  DCDevice = interface(NSObject)
    ['{1E9B372E-704C-4422-9A3B-D0160549FA98}']
    procedure generateTokenWithCompletionHandler(completion: TDCDeviceBlockMethod1); cdecl;
    function isSupported: Boolean; cdecl;
  end;
  TDCDevice = class(TOCGenericImport<DCDeviceClass, DCDevice>) end;

  DCAppAttestServiceClass = interface(NSObjectClass)
    ['{415136B6-0089-42DF-B16F-6CFC138F8611}']
    {class} function sharedService: DCAppAttestService; cdecl;
  end;

  DCAppAttestService = interface(NSObject)
    ['{86688179-0226-4A4E-B4FE-0BEDAAFA795B}']
    procedure attestKey(keyId: NSString; clientDataHash: NSData; completionHandler: TDCAppAttestServiceBlockMethod2); cdecl;
    procedure generateAssertion(keyId: NSString; clientDataHash: NSData; completionHandler: TDCAppAttestServiceBlockMethod3); cdecl;
    procedure generateKeyWithCompletionHandler(completionHandler: TDCAppAttestServiceBlockMethod1); cdecl;
    function isSupported: Boolean; cdecl;
  end;
  TDCAppAttestService = class(TOCGenericImport<DCAppAttestServiceClass, DCAppAttestService>) end;

function DCErrorDomain: NSErrorDomain;

const
  libDeviceCheck = '/System/Library/Frameworks/DeviceCheck.framework/DeviceCheck';

implementation

uses
  Posix.Dlfcn;

var
  DeviceCheckModule: THandle;

function DCErrorDomain: NSErrorDomain;
begin
  Result := CocoaNSStringConst(libDeviceCheck, 'DCErrorDomain');
end;

initialization
  DeviceCheckModule := dlopen(MarshaledAString(libDeviceCheck), RTLD_LAZY);

finalization
  dlclose(DeviceCheckModule);

end.
