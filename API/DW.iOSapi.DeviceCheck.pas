unit DW.iOSapi.DeviceCheck;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

// **** NOTE: At present, this unit is UNTESTED **** Please use with care
// As at Delphi 10.2.3, you cannot use this unit with the iOS simulator, as iOS 11.x simulators are *not supported* (this may change in Delphi 10.3)
// You will need to add the DeviceCheck framework to the iOS 11.x SDK(s) using the SDK Manager in the IDE

interface

uses
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation;

const
  DCErrorUnknownSystemFailure = 0;
  DCErrorFeatureUnsupported = 1;

type
  DCDevice = interface;
  NSErrorDomain = NSString;
  DCError = NSInteger;

  TDeviceCheckCompletion = procedure(token: NSData; error: NSError) of object;

  DCDeviceClass = interface(NSObjectClass)
    ['{EDA6CAA0-0DCE-4AD4-9C80-12FFFD77D661}']
  end;

  DCDevice = interface(NSObject)
    ['{1E9B372E-704C-4422-9A3B-D0160549FA98}']
    function currentDevice: DCDevice; cdecl;
    function isSupported: Boolean; cdecl;
    procedure generateTokenWithCompletionHandler(completion: TDeviceCheckCompletion); cdecl;
  end;
  TDCDevice = class(TOCGenericImport<DCDeviceClass, DCDevice>)
  end;

function DCErrorDomain: Pointer;

const
  libDeviceCheck = '/System/Library/Frameworks/DeviceCheck.framework/DeviceCheck';

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  DeviceCheckModule: THandle;
{$ENDIF}

function DCErrorDomain: Pointer;
begin
  Result := CocoaPointerConst(libDeviceCheck, 'DCErrorDomain');
end;

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  DeviceCheckModule := dlopen(MarshaledAString(libDeviceCheck), RTLD_LAZY);

finalization
  dlclose(DeviceCheckModule);
{$ENDIF}

end.
