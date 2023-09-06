unit DW.iOSapi.IntentsUI;

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

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation, 
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit, iOSapi.CoreGraphics,
  // DW
  DW.iOSapi.Intents;

const
  INUIAddVoiceShortcutButtonStyleWhite = 0;
  INUIAddVoiceShortcutButtonStyleWhiteOutline = 1;
  INUIAddVoiceShortcutButtonStyleBlack = 2;
  INUIAddVoiceShortcutButtonStyleBlackOutline = 3;
  INUIAddVoiceShortcutButtonStyleAutomatic = 4;
  INUIAddVoiceShortcutButtonStyleAutomaticOutline = 5;
  INUIHostedViewContextSiriSnippet = 0;
  INUIHostedViewContextMapsCard = 1;
  INUIInteractiveBehaviorNone = 0;
  INUIInteractiveBehaviorNextView = 1;
  INUIInteractiveBehaviorLaunch = 2;
  INUIInteractiveBehaviorGenericAction = 3;

type
  INUIAddVoiceShortcutButtonDelegate = interface;
  INUIAddVoiceShortcutButton = interface;
  INUIAddVoiceShortcutViewController = interface;
  INUIAddVoiceShortcutViewControllerDelegate = interface;
  INUIEditVoiceShortcutViewController = interface;
  INUIEditVoiceShortcutViewControllerDelegate = interface;
  INUIHostedViewControlling = interface;
  INUIHostedViewSiriProviding = interface;

  INUIAddVoiceShortcutButtonStyle = NSInteger;
  INUIHostedViewContext = NSInteger;
  INUIInteractiveBehavior = NSInteger;
  TINImageBlockMethod1 = procedure(image: UIImage) of object;
  TINUIHostedViewControllingBlockMethod1 = procedure(desiredSize: CGSize) of object;
  TINUIHostedViewControllingBlockMethod2 = procedure(success: Boolean; configuredParameters: NSSet; desiredSize: CGSize) of object;

  INUIAddVoiceShortcutButtonDelegate = interface(IObjectiveC)
    ['{7230E2EB-6FCA-474E-BADD-78B2FAFDEBB7}']
    procedure presentAddVoiceShortcutViewController(addVoiceShortcutViewController: INUIAddVoiceShortcutViewController;
      forAddVoiceShortcutButton: INUIAddVoiceShortcutButton); cdecl;
    procedure presentEditVoiceShortcutViewController(editVoiceShortcutViewController: INUIEditVoiceShortcutViewController;
      forAddVoiceShortcutButton: INUIAddVoiceShortcutButton); cdecl;
  end;

  INUIAddVoiceShortcutButtonClass = interface(UIButtonClass)
    ['{F31DC06A-9552-4638-A7DE-3DB762FC0EFC}']
  end;

  INUIAddVoiceShortcutButton = interface(UIButton)
    ['{6825BF11-5003-4813-B38A-673A9ADA8BFE}']
    function cornerRadius: CGFloat; cdecl;
    function delegate: Pointer; cdecl;
    function initWithFrame(frame: CGRect): Pointer; cdecl;
    function initWithStyle(style: INUIAddVoiceShortcutButtonStyle): Pointer; cdecl;
    procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setShortcut(shortcut: INShortcut); cdecl;
    procedure setStyle(style: INUIAddVoiceShortcutButtonStyle); cdecl;
    function shortcut: INShortcut; cdecl;
    function style: INUIAddVoiceShortcutButtonStyle; cdecl;
  end;
  TINUIAddVoiceShortcutButton = class(TOCGenericImport<INUIAddVoiceShortcutButtonClass, INUIAddVoiceShortcutButton>) end;

  INUIAddVoiceShortcutViewControllerClass = interface(UIViewControllerClass)
    ['{F5629ABF-9E6B-4B0D-9898-225D4304AF7D}']
  end;

  INUIAddVoiceShortcutViewController = interface(UIViewController)
    ['{70F3C430-12C5-45DB-B346-5A35D843A9AF}']
    function delegate: Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSString; bundle: NSBundle): Pointer; cdecl;
    function initWithShortcut(shortcut: INShortcut): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TINUIAddVoiceShortcutViewController = class(TOCGenericImport<INUIAddVoiceShortcutViewControllerClass, INUIAddVoiceShortcutViewController>) end;

  INUIAddVoiceShortcutViewControllerDelegate = interface(IObjectiveC)
    ['{8B2D78F7-00EC-4078-BFBB-EBB5BFBF3335}']
    procedure addVoiceShortcutViewController(controller: INUIAddVoiceShortcutViewController; didFinishWithVoiceShortcut: INVoiceShortcut;
      error: NSError); cdecl;
    procedure addVoiceShortcutViewControllerDidCancel(controller: INUIAddVoiceShortcutViewController); cdecl;
  end;

  INUIEditVoiceShortcutViewControllerClass = interface(UIViewControllerClass)
    ['{0F7C1EA7-DF71-4DAB-9E09-8AF9B77AADFE}']
  end;

  INUIEditVoiceShortcutViewController = interface(UIViewController)
    ['{95424E19-A8E5-4F03-9B7C-E883B8939536}']
    function delegate: Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSString; bundle: NSBundle): Pointer; cdecl;
    function initWithVoiceShortcut(voiceShortcut: INVoiceShortcut): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TINUIEditVoiceShortcutViewController = class(TOCGenericImport<INUIEditVoiceShortcutViewControllerClass, INUIEditVoiceShortcutViewController>) end;

  INUIEditVoiceShortcutViewControllerDelegate = interface(IObjectiveC)
    ['{2C128FA6-B1F6-4FF7-9678-5D82697B9133}']
    procedure editVoiceShortcutViewController(controller: INUIEditVoiceShortcutViewController;
      didDeleteVoiceShortcutWithIdentifier: NSUUID); overload; cdecl;
    procedure editVoiceShortcutViewController(controller: INUIEditVoiceShortcutViewController; didUpdateVoiceShortcut: INVoiceShortcut;
      error: NSError); overload; cdecl;
    procedure editVoiceShortcutViewControllerDidCancel(controller: INUIEditVoiceShortcutViewController); cdecl;
  end;

  INUIHostedViewControlling = interface(IObjectiveC)
    ['{750905FC-A5A6-4374-8F14-8F37D8FE7CDC}']
    procedure configureViewForParameters(parameters: NSSet; ofInteraction: INInteraction; interactiveBehavior: INUIInteractiveBehavior;
      context: INUIHostedViewContext; completion: Pointer); cdecl;
    procedure configureWithInteraction(interaction: INInteraction; context: INUIHostedViewContext; completion: Pointer); cdecl;
  end;

  INUIHostedViewSiriProviding = interface(IObjectiveC)
    ['{4E434AEA-523D-43F5-96D1-8B2FB4EEC4BC}']
    function displaysMap: Boolean; cdecl;
    function displaysMessage: Boolean; cdecl;
    function displaysPaymentTransaction: Boolean; cdecl;
  end;

function IntentsUIVersionNumber: Double;

const
  libIntentsUI = '/System/Library/Frameworks/IntentsUI.framework/IntentsUI';

implementation

uses
  Posix.Dlfcn;

var
  IntentsUIModule: THandle;

function IntentsUIVersionNumber: Double;
begin
  Result := CocoaDoubleConst(libIntentsUI, 'IntentsUIVersionNumber');
end;

initialization
  IntentsUIModule := dlopen(MarshaledAString(libIntentsUI), RTLD_LAZY);

finalization
  dlclose(IntentsUIModule);

end.