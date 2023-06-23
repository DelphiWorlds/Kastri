unit DW.iOSapi.QuickLook;

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
  // Mac
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CoreGraphics, iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit;

const
  QLPreviewItemEditingModeDisabled = 0;
  QLPreviewItemEditingModeUpdateContents = 1;
  QLPreviewItemEditingModeCreateCopy = 2;

type
  QLPreviewController = interface;
  QLPreviewControllerDataSource = interface;
  QLPreviewControllerDelegate = interface;
  QLPreviewingController = interface;
  QLPreviewItem = interface;
  QLThumbnailProvider = interface;
  QLThumbnailReply = interface;
  QLFileThumbnailRequest = interface;

  QLPreviewItemEditingMode = NSInteger;
  TQLPreviewingControllerBlockMethod1 = procedure(param1: NSError) of object;
  TQLThumbnailProviderBlockMethod1 = procedure(reply: QLThumbnailReply; error: NSError) of object;
  TQLThumbnailReplyBlockMethod1 = procedure(context: CGContextRef) of object;
  TQLThumbnailReplyBlockMethod2 = procedure of object;

  QLPreviewControllerClass = interface(UIViewControllerClass)
    ['{7826CF15-8B65-4EF2-B7B3-336D2C8EA3C6}']
    {class} function canPreviewItem(item: Pointer): Boolean; cdecl;
  end;

  QLPreviewController = interface(UIViewController)
    ['{B85CB601-EA5F-4C62-AE12-107451428E0F}']
    function currentPreviewItem: Pointer; cdecl;
    function currentPreviewItemIndex: NSInteger; cdecl;
    function dataSource: Pointer; cdecl;
    function delegate: Pointer; cdecl;
    procedure refreshCurrentPreviewItem; cdecl;
    procedure reloadData; cdecl;
    procedure setCurrentPreviewItemIndex(currentPreviewItemIndex: NSInteger); cdecl;
    procedure setDataSource(dataSource: Pointer); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TQLPreviewController = class(TOCGenericImport<QLPreviewControllerClass, QLPreviewController>) end;

  QLPreviewControllerDataSource = interface(IObjectiveC)
    ['{30B8E63F-D696-4127-B410-BC1B389FF49C}']
    function numberOfPreviewItemsInPreviewController(controller: QLPreviewController): NSInteger; cdecl;
    function previewController(controller: QLPreviewController; previewItemAtIndex: NSInteger): QLPreviewItem; cdecl; // Pointer
  end;

  QLPreviewControllerDelegate = interface(IObjectiveC)
    ['{E6EC18DD-1655-4C98-8673-9A7A3D2DFC61}']
    procedure previewControllerDidDismiss(controller: QLPreviewController); cdecl;
    [MethodName('previewController:didSaveEditedCopyOfPreviewItem:atURL:')]
    procedure previewControllerDidSaveEditedCopyOfPreviewItem(controller: QLPreviewController; didSaveEditedCopyOfPreviewItem: Pointer; atURL: NSURL); cdecl;
    [MethodName('previewController:didUpdateContentsOfPreviewItem:')]
    procedure previewControllerDidUpdateContentsOfPreviewItem(controller: QLPreviewController; didUpdateContentsOfPreviewItem: Pointer); cdecl;
    [MethodName('previewController:editingModeForPreviewItem:')]
    function previewControllerEditingModeForPreviewItem(controller: QLPreviewController; editingModeForPreviewItem: Pointer): QLPreviewItemEditingMode; cdecl;
    [MethodName('previewController:frameForPreviewItem:inSourceView:')]
    function previewControllerFrameForPreviewItem(controller: QLPreviewController; frameForPreviewItem: Pointer; var inSourceView: Pointer): CGRect; cdecl;
    [MethodName('previewController:shouldOpenURL:forPreviewItem:')]
    function previewControllerShouldOpenURL(controller: QLPreviewController; shouldOpenURL: NSURL; forPreviewItem: Pointer): Boolean; cdecl;
    [MethodName('previewController:transitionImageForPreviewItem:contentRect:')]
    function previewControllerTransitionImageForPreviewItem(controller: QLPreviewController; transitionImageForPreviewItem: Pointer; contentRect: PCGRect): UIImage; cdecl;
    [MethodName('previewController:transitionViewForPreviewItem:')]
    function previewControllerTransitionViewForPreviewItem(controller: QLPreviewController; transitionViewForPreviewItem: Pointer): UIView; cdecl;
    procedure previewControllerWillDismiss(controller: QLPreviewController); cdecl;
  end;

  QLPreviewingController = interface(IObjectiveC)
    ['{597A6B60-9DF5-4C81-A547-D565A59F48B1}']
    procedure preparePreviewOfFileAtURL(url: NSURL; completionHandler: TQLPreviewingControllerBlockMethod1); cdecl;
    procedure preparePreviewOfSearchableItemWithIdentifier(identifier: NSString; queryString: NSString; completionHandler: TQLPreviewingControllerBlockMethod1); cdecl;
  end;

  QLPreviewItem = interface(IObjectiveC)
    ['{C660B8B3-8D11-4249-A2E0-D0DA98B92C12}']
    function previewItemTitle: NSString; cdecl;
    function previewItemURL: NSURL; cdecl;
  end;

  QLThumbnailProviderClass = interface(NSObjectClass)
    ['{FC35C777-7D40-43A8-AC3F-6759F34DCB18}']
  end;

  QLThumbnailProvider = interface(NSObject)
    ['{BB7059CC-8469-4171-81DE-BC5DE33E3441}']
    procedure provideThumbnailForFileRequest(request: QLFileThumbnailRequest; completionHandler: TQLThumbnailProviderBlockMethod1); cdecl;
  end;
  TQLThumbnailProvider = class(TOCGenericImport<QLThumbnailProviderClass, QLThumbnailProvider>) end;

  QLThumbnailReplyClass = interface(NSObjectClass)
    ['{3371384A-E0B4-45F4-A41A-3108CE5DDF30}']
    {class} function replyWithContextSize(contextSize: CGSize; currentContextDrawingBlock: TQLThumbnailReplyBlockMethod2): Pointer; overload; cdecl;
    {class} function replyWithContextSize(contextSize: CGSize; drawingBlock: TQLThumbnailReplyBlockMethod1): Pointer; overload; cdecl;
    {class} function replyWithImageFileURL(fileURL: NSURL): Pointer; cdecl;
  end;

  QLThumbnailReply = interface(NSObject)
    ['{FC739FFA-73A2-4759-8664-8ACB81115D84}']
  end;
  TQLThumbnailReply = class(TOCGenericImport<QLThumbnailReplyClass, QLThumbnailReply>) end;

  QLFileThumbnailRequestClass = interface(NSObjectClass)
    ['{6AF2A9A8-5E0F-4A45-8AF4-AA3152EFFF50}']
  end;

  QLFileThumbnailRequest = interface(NSObject)
    ['{5C800633-023F-4C6E-8678-36AB74AD93E3}']
    function fileURL: NSURL; cdecl;
    function maximumSize: CGSize; cdecl;
    function minimumSize: CGSize; cdecl;
    function scale: CGFloat; cdecl;
  end;
  TQLFileThumbnailRequest = class(TOCGenericImport<QLFileThumbnailRequestClass, QLFileThumbnailRequest>) end;

const
  libQuickLook = '/System/Library/Frameworks/QuickLook.framework/QuickLook';

implementation

uses
  // Posix
  Posix.Dlfcn;

var
  QuickLookModule: THandle;

initialization
  QuickLookModule := dlopen(MarshaledAString(libQuickLook), RTLD_LAZY);

finalization
  dlclose(QuickLookModule);

end.
