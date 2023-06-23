unit DW.iOSapi.AppTrackingTransparency;

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

// NOTE: To use this unit, please ensure that you add the AppTrackingTransparency framework via the Delphi SDK Manager

interface

uses
  Macapi.ObjectiveC, Macapi.CoreFoundation, iOSapi.CocoaTypes, iOSapi.Foundation;

const
  ATTrackingManagerAuthorizationStatusNotDetermined = 0;
  ATTrackingManagerAuthorizationStatusRestricted = 1;
  ATTrackingManagerAuthorizationStatusDenied = 2;
  ATTrackingManagerAuthorizationStatusAuthorized = 3;

type
  ATTrackingManager = interface;

  ATTrackingManagerAuthorizationStatus = NSInteger;
  TATTrackingManagerBlockMethod1 = procedure(status: ATTrackingManagerAuthorizationStatus) of object;

  ATTrackingManagerClass = interface(NSObjectClass)
    ['{C5C7B450-DD02-42BB-9305-81CAF602B8A6}']
    {class} procedure requestTrackingAuthorizationWithCompletionHandler(completion: TATTrackingManagerBlockMethod1); cdecl;
    {class} function trackingAuthorizationStatus: ATTrackingManagerAuthorizationStatus; cdecl;
  end;

  ATTrackingManager = interface(NSObject)
    ['{3060B032-7E0F-4F36-B811-212D2D233505}']
  end;
  TATTrackingManager = class(TOCGenericImport<ATTrackingManagerClass, ATTrackingManager>) end;

function AppTrackingTransparencyVersionNumber: Double;

const
  libAppTrackingTransparency = '/System/Library/Frameworks/AppTrackingTransparency.framework/AppTrackingTransparency';

implementation

uses
  Posix.Dlfcn;

var
  AppTrackingTransparencyModule: THandle;

function CocoaDoubleConst(const Fwk: string; const ConstStr: string): Double;
var
  LObj: Pointer;
begin
  LObj := CocoaPointerConst(Fwk, ConstStr);
  if LObj <> nil then
    Result := Double(LObj^)
  else
    Result := 0;
end;

function AppTrackingTransparencyVersionNumber: Double;
begin
  Result := CocoaDoubleConst(libAppTrackingTransparency, 'AppTrackingTransparencyVersionNumber');
end;

procedure AppTrackingTransparencyLoader; cdecl; external libAppTrackingTransparency;

initialization
  AppTrackingTransparencyModule := dlopen(MarshaledAString(libAppTrackingTransparency), RTLD_LAZY);

finalization
  dlclose(AppTrackingTransparencyModule);

end.