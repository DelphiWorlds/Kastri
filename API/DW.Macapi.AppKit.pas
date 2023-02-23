unit DW.Macapi.AppKit;

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

interface

uses
  // macOS
  Macapi.Foundation, Macapi.CocoaTypes, Macapi.ObjectiveC, Macapi.AppKit;

type
  NSOpenPanel = interface;
  NSSavePanel = interface;
  NSStoryboard = interface;
  NSViewController = interface;

  NSModalResponse = NSInteger;
  NSNibName = NSString;
  NSPopoverBehavior = NSInteger;
  NSViewControllerTransitionOptions = NSInteger;
  NSStoryboardName = NSString;
  NSStoryboardSceneIdentifier = NSString;

  NSStoryboardControllerCreator = function(coder: NSCoder): Pointer of object;

  TNSSavePanelBlockMethod1 = procedure(result: NSModalResponse) of object;
  TNSViewControllerBlockMethod1 = procedure of object;

  NSSavePanelClass = interface(NSPanelClass)
    ['{E810D910-C0A9-4EFE-AFAE-FD417F913EEB}']
    {class} function savePanel: NSSavePanel; cdecl;
  end;

  NSSavePanel = interface(NSPanel)
    ['{FA70EDA2-6CE6-484F-AB17-FB138E680978}']
    function accessoryView: NSView; cdecl;
    function allowedFileTypes: NSArray; cdecl;
    function allowsOtherFileTypes: Boolean; cdecl;
    [MethodName('beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:')]
    procedure beginSheetForDirectory(path: NSString; name: NSString; docWindow: NSWindow; delegate: Pointer; didEndSelector: SEL;
      contextInfo: Pointer); cdecl; // API_DEPRECATED("Use beginSheetModalForWindow:completionHandler: instead. The following parameters are replaced by properties: 'path' is replaced by 'directoryURL' and 'name' by 'nameFieldStringValue'.", macos(10.0,10.6))
    [MethodName('beginSheetModalForWindow:completionHandler:')]
    procedure beginSheetModalForWindow(window: NSWindow; handler: TNSSavePanelBlockMethod1); cdecl;
    procedure beginWithCompletionHandler(handler: TNSSavePanelBlockMethod1); cdecl;
    procedure cancel(sender: Pointer); cdecl;
    function canCreateDirectories: Boolean; cdecl;
    function canSelectHiddenExtension: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function directory: NSString; cdecl; // API_DEPRECATED("Use -directoryURL instead", macos(10.0,10.6))
    function directoryURL: NSURL; cdecl;
    function filename: NSString; cdecl; // API_DEPRECATED("Use -URL instead", macos(10.0,10.6))
    function isExpanded: Boolean; cdecl;
    function isExtensionHidden: Boolean; cdecl;
    function message: NSString; cdecl;
    function nameFieldLabel: NSString; cdecl;
    function nameFieldStringValue: NSString; cdecl;
    procedure ok(sender: Pointer); cdecl;
    function prompt: NSString; cdecl;
    function requiredFileType: NSString; cdecl; // API_DEPRECATED("Use -allowedFileTypes instead", macos(10.0,10.6))
    function runModal: NSModalResponse; cdecl;
    [MethodName('runModalForDirectory:file:')]
    function runModalForDirectory(path: NSString; name: NSString): NSInteger; cdecl; // API_DEPRECATED("Use -runModal instead. The following parameters are replaced by properties: 'path' is replaced by 'directoryURL' and 'name' by 'nameFieldStringValue'.", macos(10.0,10.6))
    procedure selectText(sender: Pointer); cdecl; // API_DEPRECATED("Default implementation does nothing.", macos(10.0,10.3))
    procedure setAccessoryView(accessoryView: NSView); cdecl;
    procedure setAllowedFileTypes(allowedFileTypes: NSArray); cdecl;
    procedure setAllowsOtherFileTypes(allowsOtherFileTypes: Boolean); cdecl;
    procedure setCanCreateDirectories(canCreateDirectories: Boolean); cdecl;
    procedure setCanSelectHiddenExtension(canSelectHiddenExtension: Boolean); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDirectory(path: NSString); cdecl; // API_DEPRECATED("Use -setDirectoryURL: instead", macos(10.0,10.6))
    procedure setDirectoryURL(directoryURL: NSURL); cdecl;
    procedure setExtensionHidden(extensionHidden: Boolean); cdecl;
    procedure setMessage(message: NSString); cdecl;
    procedure setNameFieldLabel(nameFieldLabel: NSString); cdecl;
    procedure setNameFieldStringValue(nameFieldStringValue: NSString); cdecl;
    procedure setPrompt(prompt: NSString); cdecl;
    procedure setRequiredFileType(&type: NSString); cdecl; // API_DEPRECATED("Use -setAllowedFileTypes: instead", macos(10.0,10.6))
    procedure setShowsHiddenFiles(showsHiddenFiles: Boolean); cdecl;
    procedure setShowsTagField(showsTagField: Boolean); cdecl;
    procedure setTagNames(tagNames: NSArray); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setTreatsFilePackagesAsDirectories(treatsFilePackagesAsDirectories: Boolean); cdecl;
    function showsHiddenFiles: Boolean; cdecl;
    function showsTagField: Boolean; cdecl;
    function tagNames: NSArray; cdecl;
    function title: NSString; cdecl;
    function treatsFilePackagesAsDirectories: Boolean; cdecl;
    function URL: NSURL; cdecl;
    procedure validateVisibleColumns; cdecl;
  end;
  TNSSavePanel = class(TOCGenericImport<NSSavePanelClass, NSSavePanel>) end;

  NSOpenSavePanelDelegate = interface(IObjectiveC)
    ['{9050433D-C976-4C36-AC96-E51F8176F039}']
    [MethodName('panel:didChangeToDirectoryURL:')]
    procedure panelDidChangeToDirectoryURL(sender: Pointer; url: NSURL); cdecl;
    procedure panelSelectionDidChange(sender: Pointer); cdecl;
    [MethodName('panel:shouldEnableURL:')]
    function panelShouldEnableURL(sender: Pointer; url: NSURL): Boolean; cdecl;
    [MethodName('panel:userEnteredFilename:confirmed:')]
    function panelUserEnteredFilename(sender: Pointer; filename: NSString; okFlag: Boolean): NSString; cdecl;
    [MethodName('panel:validateURL:error:')]
    function panelValidateURL(sender: Pointer; url: NSURL; outError: PPointer): Boolean; cdecl;
    [MethodName('panel:willExpand:')]
    procedure panelWillExpand(sender: Pointer; expanding: Boolean); cdecl;
  end;

  NSOpenPanelClass = interface(NSSavePanelClass)
    ['{FBF31A4B-7C94-45BA-BCC4-F266AD4B8695}']
    {class} function openPanel: NSOpenPanel; cdecl;
  end;

  NSOpenPanel = interface(NSSavePanel)
    ['{4D11EF4C-D9B7-43F6-8288-60675540FDCB}']
    function allowsMultipleSelection: Boolean; cdecl;
    [MethodName('beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:')]
    procedure beginForDirectory(path: NSString; name: NSString; fileTypes: NSArray; delegate: Pointer; didEndSelector: SEL; contextInfo: Pointer); cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    [MethodName('beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:')]
    procedure beginSheetForDirectory(path: NSString; name: NSString; fileTypes: NSArray; docWindow: NSWindow; delegate: Pointer; didEndSelector:
      SEL; contextInfo: Pointer); cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    function canChooseDirectories: Boolean; cdecl;
    function canChooseFiles: Boolean; cdecl;
    function canDownloadUbiquitousContents: Boolean; cdecl;
    function canResolveUbiquitousConflicts: Boolean; cdecl;
    function filenames: NSArray; cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    function isAccessoryViewDisclosed: Boolean; cdecl;
    function resolvesAliases: Boolean; cdecl;
    [MethodName('runModalForDirectory:file:types:')]
    function runModalForDirectory(path: NSString; name: NSString; fileTypes: NSArray): NSInteger; cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    function runModalForTypes(fileTypes: NSArray): NSInteger; cdecl; // API_DEPRECATED("", macos(10.0,10.6))
    procedure setAccessoryViewDisclosed(accessoryViewDisclosed: Boolean); cdecl;
    procedure setAllowsMultipleSelection(allowsMultipleSelection: Boolean); cdecl;
    procedure setCanChooseDirectories(canChooseDirectories: Boolean); cdecl;
    procedure setCanChooseFiles(canChooseFiles: Boolean); cdecl;
    procedure setCanDownloadUbiquitousContents(canDownloadUbiquitousContents: Boolean); cdecl;
    procedure setCanResolveUbiquitousConflicts(canResolveUbiquitousConflicts: Boolean); cdecl;
    procedure setResolvesAliases(resolvesAliases: Boolean); cdecl;
    function URLs: NSArray; cdecl;
  end;
  TNSOpenPanel = class(TOCGenericImport<NSOpenPanelClass, NSOpenPanel>) end;

  NSStoryboardClass = interface(NSObjectClass)
    ['{51860B2C-1299-44AF-86AF-E4264002E95C}']
    {class} function mainStoryboard: NSStoryboard; cdecl;
    {class} function storyboardWithName(name: NSStoryboardName; bundle: NSBundle): Pointer; cdecl;
  end;

  NSStoryboard = interface(NSObject)
    ['{58CA900E-0ED2-4F58-B586-E5F0A6537C27}']
    function instantiateControllerWithIdentifier(identifier: NSStoryboardSceneIdentifier): Pointer; overload; cdecl;
    function instantiateControllerWithIdentifier(identifier: NSStoryboardSceneIdentifier; creator: NSStoryboardControllerCreator): Pointer; overload; cdecl;
    function instantiateInitialController: Pointer; cdecl;
    function instantiateInitialControllerWithCreator(block: NSStoryboardControllerCreator): Pointer; cdecl;
  end;
  TNSStoryboard = class(TOCGenericImport<NSStoryboardClass, NSStoryboard>) end;

  NSViewControllerClass = interface(NSResponderClass)
    ['{586C8CDC-6893-405C-89E1-305C8C64DEAC}']
  end;

  NSViewController = interface(NSResponder)
    ['{B9802D1E-21A6-4ABD-9343-48AB899C21E6}']
    procedure addChildViewController(childViewController: NSViewController); cdecl;
    function childViewControllers: NSArray; cdecl;
    function commitEditing: Boolean; cdecl;
    procedure commitEditingWithDelegate(delegate: Pointer; didCommitSelector: SEL; contextInfo: Pointer); cdecl;
    procedure discardEditing; cdecl;
    procedure dismissController(sender: Pointer); cdecl;
    procedure dismissViewController(viewController: NSViewController); cdecl;
    function extensionContext: NSExtensionContext; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSNibName; bundle: NSBundle): Pointer; cdecl;
    procedure insertChildViewController(childViewController: NSViewController; atIndex: NSInteger); cdecl;
    function isViewLoaded: Boolean; cdecl;
    procedure loadView; cdecl;
    function nibBundle: NSBundle; cdecl;
    function nibName: NSNibName; cdecl;
    function parentViewController: NSViewController; cdecl;
    function preferredContentSize: NSSize; cdecl;
    procedure preferredContentSizeDidChangeForViewController(viewController: NSViewController); cdecl;
    function preferredMaximumSize: NSSize; cdecl;
    function preferredMinimumSize: NSSize; cdecl;
    function preferredScreenOrigin: NSPoint; cdecl;
    function presentedViewControllers: NSArray; cdecl;
    function presentingViewController: NSViewController; cdecl;
    procedure presentViewController(viewController: NSViewController; asPopoverRelativeToRect: NSRect; ofView: NSView; preferredEdge: NSRectEdge;
      behavior: NSPopoverBehavior); overload; cdecl;
    procedure presentViewController(viewController: NSViewController; animator: Pointer); overload; cdecl;
    procedure presentViewControllerAsModalWindow(viewController: NSViewController); cdecl;
    procedure presentViewControllerAsSheet(viewController: NSViewController); cdecl;
    procedure removeChildViewControllerAtIndex(index: NSInteger); cdecl;
    procedure removeFromParentViewController; cdecl;
    function representedObject: Pointer; cdecl;
    procedure setChildViewControllers(childViewControllers: NSArray); cdecl;
    procedure setPreferredContentSize(preferredContentSize: NSSize); cdecl;
    procedure setPreferredScreenOrigin(preferredScreenOrigin: NSPoint); cdecl;
    procedure setRepresentedObject(representedObject: Pointer); cdecl;
    procedure setSourceItemView(sourceItemView: NSView); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setView(view: NSView); cdecl;
    function sourceItemView: NSView; cdecl;
    function storyboard: NSStoryboard; cdecl;
    function title: NSString; cdecl;
    procedure transitionFromViewController(fromViewController: NSViewController; toViewController: NSViewController;
      options: NSViewControllerTransitionOptions; completionHandler: TNSViewControllerBlockMethod1); cdecl;
    procedure updateViewConstraints; cdecl;
    function view: NSView; cdecl;
    procedure viewDidAppear; cdecl;
    procedure viewDidDisappear; cdecl;
    procedure viewDidLayout; cdecl;
    procedure viewDidLoad; cdecl;
    procedure viewWillAppear; cdecl;
    procedure viewWillDisappear; cdecl;
    procedure viewWillLayout; cdecl;
    procedure viewWillTransitionToSize(newSize: NSSize); cdecl;
  end;
  TNSViewController = class(TOCGenericImport<NSViewControllerClass, NSViewController>) end;

implementation

end.
