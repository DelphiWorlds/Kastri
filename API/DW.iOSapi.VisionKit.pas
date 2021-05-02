unit DW.iOSapi.VisionKit;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

// NOTE: If you use this unit, you need to import the following frameworks using SDK Manager in Delphi:
//   VisionKit ($(SDKRoot)\System\Library\Frameworks)
//   DocumentCamera ($(SDKRoot)\System\Library\PrivateFrameworks)
// For help regarding adding the frameworks, please refer to:
//   https://delphiworlds.com/2013/10/adding-other-ios-frameworks-to-the-sdk-manager/

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CoreFoundation,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit;

type
  VNDocumentCameraViewController = interface;
  VNDocumentCameraViewControllerDelegate = interface;
  VNDocumentCameraScan = interface;

  VNDocumentCameraViewControllerClass = interface(UIViewControllerClass)
    ['{D4B35D33-CCC0-4B2A-B808-C669BBE60464}']
    {class} function isSupported: Boolean; cdecl;
  end;

  VNDocumentCameraViewController = interface(UIViewController)
    ['{24581E0B-2E46-4A24-86CE-776D9204090D}']
    function delegate: Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSString; bundle: NSBundle): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TVNDocumentCameraViewController = class(TOCGenericImport<VNDocumentCameraViewControllerClass, VNDocumentCameraViewController>) end;

  VNDocumentCameraViewControllerDelegate = interface(IObjectiveC)
    ['{E94F1BF5-0423-45D8-B2F2-307A9CF5FF47}']
    procedure documentCameraViewController(controller: VNDocumentCameraViewController; didFinishWithScan: VNDocumentCameraScan); overload; cdecl;
    procedure documentCameraViewController(controller: VNDocumentCameraViewController; didFailWithError: NSError); overload; cdecl;
    procedure documentCameraViewControllerDidCancel(controller: VNDocumentCameraViewController); cdecl;
  end;

  VNDocumentCameraScanClass = interface(NSObjectClass)
    ['{EF4FD634-A3AB-4204-ACB2-CE050B26B6A8}']
    {class} function new: Pointer; cdecl;
  end;

  VNDocumentCameraScan = interface(NSObject)
    ['{1CC6D9A7-7525-49C4-A317-5C3846CF4661}']
    function imageOfPageAtIndex(index: NSUInteger): UIImage; cdecl;
    function pageCount: NSUInteger; cdecl;
    function title: NSString; cdecl;
  end;
  TVNDocumentCameraScan = class(TOCGenericImport<VNDocumentCameraScanClass, VNDocumentCameraScan>) end;

const
  libVisionKit = '/System/Library/Frameworks/VisionKit.framework/VisionKit';

procedure VisionKitLoader; cdecl; external libVisionKit;

implementation

{$IF Defined(IOS) and not Defined(CPUARM)}
uses
  Posix.Dlfcn;

var
  VisionKitModule: THandle;
{$ENDIF}

{$IF Defined(IOS) and not Defined(CPUARM)}
initialization
  VisionKitModule := dlopen(MarshaledAString(libVisionKit), RTLD_LAZY);

finalization
  dlclose(VisionKitModule)
{$ENDIF}

end.