unit DW.iOSapi.UIKit;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.QuartzCore, iOSapi.UIKit;

const
  UIAlertActionStyleDefault = 0;
  UIAlertActionStyleCancel = 1;
  UIAlertActionStyleDestructive = 2;
  UIAlertControllerStyleActionSheet = 0;
  UIAlertControllerStyleAlert = 1;

type
  UIAlertActionStyle = NSInteger;
  UIAlertControllerStyle = NSInteger;

  UIAlertAction = interface;

  TUIAlertActionHandler = procedure(alertaction: Pointer) of object;
  TUIAlertControllerConfigurationHandler = procedure(textfield: UITextField) of object;

  UIAlertActionClass = interface(NSObjectClass)
    ['{953DC1CC-483A-451B-BB66-101217A9C6BF}']
    { class } function actionWithTitle(title: NSString; style: UIAlertActionStyle; handler: TUIAlertActionHandler): Pointer; cdecl;
  end;

  UIAlertAction = interface(NSObject)
    ['{D3CAF467-5D53-48A1-9507-7F934814E1AB}']
    function isEnabled: Boolean; cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setStyle(style: UIAlertActionStyle); cdecl;
    function style: UIAlertActionStyle; cdecl;
    function title: NSString; cdecl;
  end;
  TUIAlertAction = class(TOCGenericImport<UIAlertActionClass, UIAlertAction>)
  end;

  UIAlertControllerClass = interface(NSObjectClass)
    ['{DA597BCE-6687-4BCC-8B3F-57205A141193}']
    { class } function alertControllerWithTitle(title: NSString; message: NSString; preferredStyle: UIAlertControllerStyle) : Pointer; cdecl;
  end;

  UIAlertController = interface(UIViewController)
    ['{68C4436A-A75E-4825-A578-8D26894B0469}']
    function actions: NSArray; cdecl;
    procedure addAction(action: UIAlertAction); cdecl;
    procedure addTextFieldWithConfigurationHandler(configurationHandler: TUIAlertControllerConfigurationHandler); cdecl;
    function message: NSString; cdecl;
    function preferredAction: UIAlertAction; cdecl;
    function preferredStyle: UIAlertControllerStyle; cdecl;
    procedure setMessage(message: NSString); cdecl;
    procedure setPreferredAction(preferredAction: UIAlertAction); cdecl;
    procedure setTitle(title: NSString); cdecl;
    function textFields: NSArray; cdecl;
    function title: NSString; cdecl;
  end;
  TUIAlertController = class(TOCGenericImport<UIAlertControllerClass, UIAlertController>)
  end;

  TBackgroundTaskHandler = procedure of object;
  TOpenURLCompletionHandler = procedure(success: Boolean) of object;

  UIApplication = interface(iOSapi.UIKit.UIApplication)
    ['{EC184C87-9109-4D61-9BCA-291F70D90D15}']
    function beginBackgroundTaskWithExpirationHandler(handler: TBackgroundTaskHandler): UIBackgroundTaskIdentifier; cdecl;
    procedure endBackgroundTask(identifier: UIBackgroundTaskIdentifier); cdecl;
    function isRegisteredForRemoteNotifications: Boolean; cdecl;
    [MethodName('openURL:options:completionHandler:')]
    procedure openURLOptionsCompletionHandler(url: NSURL; options: NSDictionary; completionHandler: TOpenURLCompletionHandler); cdecl;
    procedure setMinimumBackgroundFetchInterval(minimumBackgroundFetchInterval: NSTimeInterval); cdecl;
  end;
  TUIApplication = class(TOCGenericImport<UIApplicationClass, UIApplication>) end;

implementation

end.
