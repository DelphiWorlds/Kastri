unit DW.UniversalLinks.iOS;

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

implementation

uses
  // RTL
  System.SysUtils, System.Messaging,
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.Foundation, iOSapi.UIKit,
  // FMX
  FMX.Platform,
  // DW
  DW.Macapi.ObjCRuntime, DW.iOSapi.Foundation, DW.UniversalLinks;

type
  TPlatformUniversalLinks = class(TObject)
  private
    class procedure AddDelegateMethod;
    class function ContinueUserActivityRestorationHandler(self: Pointer; _cmd: Pointer; application: PUIApplication;
      userActivity: Pointer; restorationHandler: Pointer; restorableObjects: Pointer): Boolean; cdecl; static;
  end;

{ TPlatformUniversalLinks }

class procedure TPlatformUniversalLinks.AddDelegateMethod;
begin
  AddClassMethod('DelphiAppDelegate', 'application:continueUserActivity:restorationHandler:', @ContinueUserActivityRestorationHandler, 'B@:@@@@');
end;

class function TPlatformUniversalLinks.ContinueUserActivityRestorationHandler(self, _cmd: Pointer; application: PUIApplication; userActivity,
  restorationHandler, restorableObjects: Pointer): Boolean;
var
  LUserActivity: NSUserActivity;
  LURL: string;
  LMessage: TApplicationEventMessage;
  LContext: TObject;
begin
  Result := False;
  if userActivity <> nil then
  begin
    LUserActivity := TNSUserActivity.Wrap(userActivity);
    if LUserActivity.activityType.compare(NSUserActivityTypeBrowsingWeb) = NSOrderedSame then
    begin
      if LUserActivity.webpageURL <> nil then
        LURL := NSStrToStr(LUserActivity.webpageURL.absoluteString)
      else
        LURL := string.Empty;
      LContext := TOpenApplicationContext.Create(LURL);
      LMessage := TApplicationEventMessage.Create(TApplicationEventData.Create(TApplicationEvent.OpenURL, LContext));
      TMessageManager.DefaultManager.SendMessage(nil, LMessage);
    end;
  end;
end;

initialization
  TPlatformUniversalLinks.AddDelegateMethod;

end.
