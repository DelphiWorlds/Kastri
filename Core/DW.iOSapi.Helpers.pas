unit DW.iOSapi.Helpers;

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

interface

uses
  // RTL
  System.Sensors,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.UIKit;

type
  UIApplication = interface(iOSapi.UIKit.UIApplication)
    ['{7228BEAE-1B3A-4EBC-A87C-03982C8EC742}']
    function isRegisteredForRemoteNotifications: Boolean; cdecl;
  end;
  TUIApplication = class(TOCGenericImport<UIApplicationClass, UIApplication>) end;

  TiOSHelperEx = record
  public
    class procedure AddObserver(const AObserver: Pointer; const AMethod: MarshaledAString; const AName: NSString;
      const AObject: NSObject = nil); overload; static;
    class procedure AddObserver(const AObserver: Pointer; const AMethod: MarshaledAString; const AName: string;
      const AObject: NSObject = nil); overload; static;
    class function GetLocationManagerAuthorization: TAuthorizationType; static;
    class function HasBackgroundMode(const AMode: string): Boolean; static;
    class function IsBackground: Boolean; static;
    class function IsIPhoneX: Boolean; static;
    //!!! ADictionary must be a JSON dictionary!!
    class function NSDictionaryToJSON(const ADictionary: NSDictionary): string; static;
    class function NSDictionaryToString(const ADictionary: NSDictionary): string; static;
    class function SharedApplication: UIApplication; static;
    class function StandardUserDefaults: NSUserDefaults; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // iOS
  iOSapi.CoreLocation, iOSapi.Helpers,
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers;

class procedure TiOSHelperEx.AddObserver(const AObserver: Pointer; const AMethod: MarshaledAString; const AName: NSString; const AObject: NSObject = nil);
var
  LObject: Pointer;
begin
  if AObject <> nil then
    LObject := NSObjectToID(AObject)
  else
    LObject := nil;
  TiOSHelper.DefaultNotificationCenter.addObserver(AObserver, sel_getUid(AMethod), NSObjectToID(AName), LObject);
end;

class procedure TiOSHelperEx.AddObserver(const AObserver: Pointer; const AMethod: MarshaledAString; const AName: string; const AObject: NSObject = nil);
begin
  AddObserver(AObserver, AMethod, StrToNSStr(AName), AObject);
end;

class function TiOSHelperEx.GetLocationManagerAuthorization: TAuthorizationType;
begin
  case TCLLocationManager.OCClass.authorizationStatus of
    kCLAuthorizationStatusNotDetermined:
      Result := TAuthorizationType.atNotSpecified;
    kCLAuthorizationStatusDenied,
    kCLAuthorizationStatusRestricted:
      Result := TAuthorizationType.atUnauthorized;
    kCLAuthorizationStatusAuthorizedWhenInUse,
    kCLAuthorizationStatusAuthorized:
      Result := TAuthorizationType.atAuthorized;
  else
    Result := TAuthorizationType.atNotSpecified;
  end;
end;

class function TiOSHelperEx.HasBackgroundMode(const AMode: string): Boolean;
var
  LBundle: NSBundle;
  LPointer: Pointer;
  LModesArray: NSArray;
  LModeString: string;
  I: Integer;
begin
  Result := False;
  LBundle := TiOSHelper.MainBundle;
  LPointer := LBundle.infoDictionary.valueForKey(StrToNSStr('UIBackgroundModes')); // Do not localise
  if LPointer = nil then
    Exit; // <======
  LModesArray := TNSArray.Wrap(LPointer);
  for I := 0 to LModesArray.count - 1 do
  begin
    LModeString := NSStrToStr(TNSString.Wrap(LModesArray.objectAtIndex(I)));
    if AMode.Equals(LModeString) then
      Exit(True); // <======
  end;
end;

class function TiOSHelperEx.IsBackground: Boolean;
begin
  Result := SharedApplication.applicationState = UIApplicationStateBackground;
end;

class function TiOSHelperEx.IsIPhoneX: Boolean;
const
  cIPhoneXHeight = 812;
var
  LOrientation: UIInterfaceOrientation;
begin
  Result := False;
  // Might be safe enough to just use statusBarOrientation
  if SharedApplication.keyWindow = nil then
    LOrientation := SharedApplication.statusBarOrientation
  else
    LOrientation := SharedApplication.keyWindow.rootViewController.interfaceOrientation;
  case LOrientation of
    UIInterfaceOrientationPortrait, UIInterfaceOrientationPortraitUpsideDown:
      Result := TiOSHelper.MainScreen.bounds.size.height = cIPhoneXHeight;
    UIInterfaceOrientationLandscapeLeft, UIInterfaceOrientationLandscapeRight:
      Result := TiOSHelper.MainScreen.bounds.size.width = cIPhoneXHeight;
  end;
end;

class function TiOSHelperEx.NSDictionaryToJSON(const ADictionary: NSDictionary): string;
var
  LData: NSData;
  LString: NSString;
  LError: NSError;
begin
  LData := TNSJSONSerialization.OCClass.dataWithJSONObject(NSObjectToID(ADictionary), 0, Addr(LError));
  if (LData <> nil) and (LError = nil) then
  begin
    LString := TNSString.Wrap(TNSString.Alloc.initWithData(LData, NSUTF8StringEncoding));
    Result :=  NSStrToStr(LString);
  end
  else
    Result := '';
end;

class function TiOSHelperEx.NSDictionaryToString(const ADictionary: NSDictionary): string;
var
  I: Integer;
begin
  // Just dumps the keys for now
  Result := '';
  for I := 0 to ADictionary.allKeys.count - 1 do
  begin
    if I > 0 then
      Result := Result + #13#10;
    Result := Result + NSStrToStr(TNSString.Wrap(ADictionary.allKeys.objectAtIndex(I)));
  end;
end;

class function TiOSHelperEx.SharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
end;

class function TiOSHelperEx.StandardUserDefaults: NSUserDefaults;
begin
  Result := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
end;

end.
