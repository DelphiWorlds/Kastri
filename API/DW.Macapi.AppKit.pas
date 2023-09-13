unit DW.Macapi.AppKit;

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
  Macapi.Foundation, Macapi.CocoaTypes, Macapi.ObjectiveC, Macapi.AppKit,
  // DW
  DW.Macapi.CloudKit;

type
  NSCloudSharingServiceDelegate = interface;
  NSEvent = interface;
  NSOpenPanel = interface;
  NSSavePanel = interface;
  NSSharingService = interface;
  NSSharingServiceDelegate = interface;
  NSSharingServicePicker = interface;
  NSSharingServicePickerDelegate = interface;
  NSSharingServicePickerTouchBarItem = interface;
  NSSharingServicePickerTouchBarItemDelegate = interface;
  NSStoryboard = interface;
  NSViewController = interface;

  NSEventButtonMask = NSInteger;
  NSEventMask = NSInteger;
  NSEventModifierFlags = NSInteger;
  NSEventPhase = NSInteger;
  NSEventSubtype = NSInteger;
  NSEventSwipeTrackingOptions = NSInteger;
  NSEventType = NSInteger;
  NSModalResponse = NSInteger;
  NSNibName = NSString;
  NSPopoverBehavior = NSInteger;
  NSPressureBehavior = NSInteger;
  NSViewControllerTransitionOptions = NSInteger;
  NSStoryboardName = NSString;
  NSStoryboardSceneIdentifier = NSString;
  NSSharingServiceName = NSString;
  NSSharingContentScope = NSInteger;
  NSCloudKitSharingServiceOptions = NSInteger;
  NSTouchBarItemIdentifier = NSString;
  NSTouchBarItemPriority = Single;
  NSTouchBarCustomizationIdentifier = NSString;

  PNSRectEdge = ^NSRectEdge;
  PNSSharingContentScope = ^NSSharingContentScope;

  NSStoryboardControllerCreator = function(coder: NSCoder): Pointer of object;

  TNSEventBlockMethod1 = procedure(gestureAmount: CGFloat; phase: NSEventPhase; isComplete: Boolean; stop: PBoolean) of object;
  TNSEventBlockMethod2 = procedure(event: NSEvent) of object;
  TNSSavePanelBlockMethod1 = procedure(result: NSModalResponse) of object;
  TNSViewControllerBlockMethod1 = procedure of object;
  TNSSharingServiceBlockMethod1 = procedure of object;

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

  NSTouchBarItemClass = interface(NSObjectClass)
    ['{041F7F3D-F751-4BFF-B2E6-0260E04074DC}']
  end;

  NSTouchBarItem = interface(NSObject)
    ['{83084682-A41E-47AA-B926-80D000342690}']
    function customizationLabel: NSString; cdecl;
    function identifier: NSTouchBarItemIdentifier; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithIdentifier(identifier: NSTouchBarItemIdentifier): Pointer; cdecl;
    function isVisible: Boolean; cdecl;
    procedure setVisibilityPriority(visibilityPriority: NSTouchBarItemPriority); cdecl;
    function view: NSView; cdecl;
    function viewController: NSViewController; cdecl;
    function visibilityPriority: NSTouchBarItemPriority; cdecl;
  end;
  TNSTouchBarItem = class(TOCGenericImport<NSTouchBarItemClass, NSTouchBarItem>) end;

  NSTouchBarClass = interface(NSObjectClass)
    ['{64066B30-F06B-4D47-A1B2-DFB06D7F8530}']
    {class} function isAutomaticCustomizeTouchBarMenuItemEnabled: Boolean; cdecl;
    {class} procedure setAutomaticCustomizeTouchBarMenuItemEnabled(automaticCustomizeTouchBarMenuItemEnabled: Boolean); cdecl;
  end;

  NSTouchBar = interface(NSObject)
    ['{ADD03BB5-E627-4434-A1E3-5F9AC778B451}']
    function customizationAllowedItemIdentifiers: NSArray; cdecl;
    function customizationIdentifier: NSTouchBarCustomizationIdentifier; cdecl;
    function customizationRequiredItemIdentifiers: NSArray; cdecl;
    function defaultItemIdentifiers: NSArray; cdecl;
    function delegate: Pointer; cdecl;
    function escapeKeyReplacementItemIdentifier: NSTouchBarItemIdentifier; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function isVisible: Boolean; cdecl;
    function itemForIdentifier(identifier: NSTouchBarItemIdentifier): NSTouchBarItem; cdecl;
    function itemIdentifiers: NSArray; cdecl;
    function principalItemIdentifier: NSTouchBarItemIdentifier; cdecl;
    procedure setCustomizationAllowedItemIdentifiers(customizationAllowedItemIdentifiers: NSArray); cdecl;
    procedure setCustomizationIdentifier(customizationIdentifier: NSTouchBarCustomizationIdentifier); cdecl;
    procedure setCustomizationRequiredItemIdentifiers(customizationRequiredItemIdentifiers: NSArray); cdecl;
    procedure setDefaultItemIdentifiers(defaultItemIdentifiers: NSArray); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setEscapeKeyReplacementItemIdentifier(escapeKeyReplacementItemIdentifier: NSTouchBarItemIdentifier); cdecl;
    procedure setPrincipalItemIdentifier(principalItemIdentifier: NSTouchBarItemIdentifier); cdecl;
    procedure setTemplateItems(templateItems: NSSet); cdecl;
    function templateItems: NSSet; cdecl;
  end;
  TNSTouchBar = class(TOCGenericImport<NSTouchBarClass, NSTouchBar>) end;

  NSTouchBarDelegate = interface(IObjectiveC)
    ['{CFA01AE9-49A8-443A-9CC7-54D9EEB696CA}']
    function touchBar(touchBar: NSTouchBar; makeItemForIdentifier: NSTouchBarItemIdentifier): NSTouchBarItem; cdecl;
  end;

  NSTouchBarProvider = interface(IObjectiveC)
    ['{B9A09F5A-1CF3-4D6E-9E3A-5D52A257B524}']
    function touchBar: NSTouchBar; cdecl;
  end;

  NSSharingServiceClass = interface(NSObjectClass)
    ['{9B90CABC-293D-464A-9F95-59CD848E0398}']
    {class} function sharingServiceNamed(serviceName: NSSharingServiceName): NSSharingService; cdecl;
    {class} function sharingServicesForItems(items: NSArray): NSArray; cdecl;
  end;

  NSSharingService = interface(NSObject)
    ['{7F54B0F8-D909-424C-A3CD-FF8B2877E66B}']
    function accountName: NSString; cdecl;
    function alternateImage: NSImage; cdecl;
    function attachmentFileURLs: NSArray; cdecl;
    function canPerformWithItems(items: NSArray): Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function image: NSImage; cdecl;
    function initWithTitle(title: NSString; image: NSImage; alternateImage: NSImage; handler: TNSSharingServiceBlockMethod1): Pointer; cdecl;
    function menuItemTitle: NSString; cdecl;
    function messageBody: NSString; cdecl;
    procedure performWithItems(items: NSArray); cdecl;
    function permanentLink: NSURL; cdecl;
    function recipients: NSArray; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setMenuItemTitle(menuItemTitle: NSString); cdecl;
    procedure setRecipients(recipients: NSArray); cdecl;
    procedure setSubject(subject: NSString); cdecl;
    function subject: NSString; cdecl;
    function title: NSString; cdecl;
  end;
  TNSSharingService = class(TOCGenericImport<NSSharingServiceClass, NSSharingService>) end;

  NSSharingServiceDelegate = interface(IObjectiveC)
    ['{1D3510B3-93CE-4EFD-B2BD-04824B265300}']
    function anchoringViewForSharingService(sharingService: NSSharingService; showRelativeToRect: PNSRect; preferredEdge: PNSRectEdge): NSView; cdecl;
    [MethodName('sharingService:didFailToShareItems:error:')]
    procedure sharingServiceDidFailToShareItems(sharingService: NSSharingService; didFailToShareItems: NSArray; error: NSError); cdecl;
    [MethodName('sharingService:didShareItems:')]
    procedure sharingServiceDidShareItems(sharingService: NSSharingService; didShareItems: NSArray); cdecl;
    [MethodName('sharingService:sourceFrameOnScreenForShareItem:')]
    function sharingServiceSourceFrameOnScreenForShareItem(sharingService: NSSharingService; sourceFrameOnScreenForShareItem: Pointer): NSRect; cdecl;
    [MethodName('sharingService:sourceWindowForShareItems:sharingContentScope:')]
    function sharingServiceSourceWindowForShareItems(sharingService: NSSharingService; sourceWindowForShareItems: NSArray;
      sharingContentScope: PNSSharingContentScope): NSWindow; cdecl;
    [MethodName('sharingService:transitionImageForShareItem:contentRect:')]
    function sharingServiceTransitionImageForShareItem(sharingService: NSSharingService; transitionImageForShareItem: Pointer;
      contentRect: PNSRect): NSImage; cdecl;
    [MethodName('sharingService:willShareItems:')]
    procedure sharingServiceWillShareItems(sharingService: NSSharingService; willShareItems: NSArray); cdecl;
  end;

  NSCloudSharingServiceDelegate = interface(IObjectiveC)
    ['{D68CD534-AF5D-44C1-B48A-F7A16015A377}']
    function optionsForSharingService(cloudKitSharingService: NSSharingService; shareProvider: NSItemProvider): NSCloudKitSharingServiceOptions; cdecl;
    [MethodName('sharingService:didCompleteForItems:error:')]
    procedure sharingServiceDidCompleteForItems(sharingService: NSSharingService; didCompleteForItems: NSArray; error: NSError); cdecl;
    [MethodName('sharingService:didSaveShare:')]
    procedure sharingServiceDidSaveShare(sharingService: NSSharingService; didSaveShare: CKShare); cdecl;
    [MethodName('sharingService:didStopSharing:')]
    procedure sharingServiceDidStopSharing(sharingService: NSSharingService; didStopSharing: CKShare); cdecl;
  end;

  NSSharingServicePickerClass = interface(NSObjectClass)
    ['{B663F2C7-8A68-4752-A142-64B4BE96FD20}']
  end;

  NSSharingServicePicker = interface(NSObject)
    ['{87DAABDD-2E57-48C0-97C4-A3335FA91D3F}']
    function delegate: Pointer; cdecl;
    function initWithItems(items: NSArray): Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure showRelativeToRect(rect: NSRect; ofView: NSView; preferredEdge: NSRectEdge); cdecl;
  end;
  TNSSharingServicePicker = class(TOCGenericImport<NSSharingServicePickerClass, NSSharingServicePicker>) end;

  NSSharingServicePickerDelegate = interface(IObjectiveC)
    ['{35E5A733-DF7C-40AA-8A1B-6B7DB4E293D3}']
    [MethodName('sharingServicePicker:delegateForSharingService:')]
    function sharingServicePickerDelegateForSharingService(sharingServicePicker: NSSharingServicePicker;
      delegateForSharingService: NSSharingService): Pointer; cdecl;
    [MethodName('sharingServicePicker:didChooseSharingService:')]
    procedure sharingServicePickerDidChooseSharingService(sharingServicePicker: NSSharingServicePicker;
      didChooseSharingService: NSSharingService); cdecl;
    [MethodName('sharingServicePicker:sharingServicesForItems:proposedSharingServices:')]
    function sharingServicePickerSharingServicesForItems(sharingServicePicker: NSSharingServicePicker; sharingServicesForItems: NSArray;
      proposedSharingServices: NSArray): NSArray; cdecl;
  end;

  NSSharingServicePickerTouchBarItemClass = interface(NSTouchBarItemClass)
    ['{9DADD75B-7C53-4C2C-A50C-584ECC9A8F69}']
  end;

  NSSharingServicePickerTouchBarItem = interface(NSTouchBarItem)
    ['{02ED0F42-6E92-4712-A936-B2A27D516F7B}']
    function buttonImage: NSImage; cdecl;
    function buttonTitle: NSString; cdecl;
    function delegate: Pointer; cdecl;
    function isEnabled: Boolean; cdecl;
    procedure setButtonImage(buttonImage: NSImage); cdecl;
    procedure setButtonTitle(buttonTitle: NSString); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
  end;
  TNSSharingServicePickerTouchBarItem = class(TOCGenericImport<NSSharingServicePickerTouchBarItemClass, NSSharingServicePickerTouchBarItem>) end;

  NSSharingServicePickerTouchBarItemDelegate = interface(IObjectiveC)
    ['{DD2F8398-5CB1-4D68-A483-ACAE4EB47223}']
    function itemsForSharingServicePickerTouchBarItem(pickerTouchBarItem: NSSharingServicePickerTouchBarItem): NSArray; cdecl;
  end;

  NSEventClass = interface(NSObjectClass)
    ['{2C522522-2141-40BA-A61D-16C0362EFB95}']
    {class} function addGlobalMonitorForEventsMatchingMask(mask: NSEventMask; handler: TNSEventBlockMethod2): Pointer; cdecl;
    {class} function addLocalMonitorForEventsMatchingMask(mask: NSEventMask; handler: TNSEventBlockMethod2): Pointer; cdecl;
    {class} function doubleClickInterval: NSTimeInterval; cdecl;
    {class} function enterExitEventWithType(&type: NSEventType; location: NSPoint; modifierFlags: NSEventModifierFlags; timestamp: NSTimeInterval;
      windowNumber: NSInteger; context: NSGraphicsContext; eventNumber: NSInteger; trackingNumber: NSInteger; userData: Pointer): NSEvent; cdecl;
    {class} function eventWithCGEvent(cgEvent: CGEventRef): NSEvent; cdecl;
    {class} function eventWithEventRef(eventRef: Pointer): NSEvent; cdecl;
    {class} function isMouseCoalescingEnabled: Boolean; cdecl;
    {class} function isSwipeTrackingFromScrollEventsEnabled: Boolean; cdecl;
    {class} function keyEventWithType(&type: NSEventType; location: NSPoint; modifierFlags: NSEventModifierFlags; timestamp: NSTimeInterval;
      windowNumber: NSInteger; context: NSGraphicsContext; characters: NSString; charactersIgnoringModifiers: NSString; isARepeat: Boolean;
      keyCode: Word): NSEvent; cdecl;
    {class} function keyRepeatDelay: NSTimeInterval; cdecl;
    {class} function keyRepeatInterval: NSTimeInterval; cdecl;
    {class} function modifierFlags: NSEventModifierFlags; cdecl;
    {class} function mouseEventWithType(&type: NSEventType; location: NSPoint; modifierFlags: NSEventModifierFlags; timestamp: NSTimeInterval;
      windowNumber: NSInteger; context: NSGraphicsContext; eventNumber: NSInteger; clickCount: NSInteger; pressure: Single): NSEvent; cdecl;
    {class} function mouseLocation: NSPoint; cdecl;
    {class} function otherEventWithType(&type: NSEventType; location: NSPoint; modifierFlags: NSEventModifierFlags; timestamp: NSTimeInterval;
      windowNumber: NSInteger; context: NSGraphicsContext; subtype: Smallint; data1: NSInteger; data2: NSInteger): NSEvent; cdecl;
    {class} function pressedMouseButtons: NSUInteger; cdecl;
    {class} procedure removeMonitor(eventMonitor: Pointer); cdecl;
    {class} procedure setMouseCoalescingEnabled(mouseCoalescingEnabled: Boolean); cdecl;
    {class} procedure startPeriodicEventsAfterDelay(delay: NSTimeInterval; withPeriod: NSTimeInterval); cdecl;
    {class} procedure stopPeriodicEvents; cdecl;
  end;

  NSEvent = interface(NSObject)
    ['{66D96C14-65F4-4A71-AC65-246823864F5D}']
    function absoluteX: NSInteger; cdecl;
    function absoluteY: NSInteger; cdecl;
    function absoluteZ: NSInteger; cdecl;
    function allTouches: NSSet; cdecl;
    function associatedEventsMask: NSEventMask; cdecl;
    function buttonMask: NSEventButtonMask; cdecl;
    function buttonNumber: NSInteger; cdecl;
    function capabilityMask: NSUInteger; cdecl;
    function CGEvent: CGEventRef; cdecl;
    function characters: NSString; cdecl;
    function charactersByApplyingModifiers(modifiers: NSEventModifierFlags): NSString; cdecl;
    function charactersIgnoringModifiers: NSString; cdecl;
    function clickCount: NSInteger; cdecl;
    function coalescedTouchesForTouch(touch: NSTouch): NSArray; cdecl;
    function context: NSGraphicsContext; cdecl;
    function data1: NSInteger; cdecl;
    function data2: NSInteger; cdecl;
    function deltaX: CGFloat; cdecl;
    function deltaY: CGFloat; cdecl;
    function deltaZ: CGFloat; cdecl;
    function deviceID: NSUInteger; cdecl;
    function eventNumber: NSInteger; cdecl;
    function eventRef: Pointer; cdecl;
    function hasPreciseScrollingDeltas: Boolean; cdecl;
    function isARepeat: Boolean; cdecl;
    function isDirectionInvertedFromDevice: Boolean; cdecl;
    function isEnteringProximity: Boolean; cdecl;
    function keyCode: Word; cdecl;
    function locationInWindow: NSPoint; cdecl;
    function magnification: CGFloat; cdecl;
    function modifierFlags: NSEventModifierFlags; cdecl;
    function momentumPhase: NSEventPhase; cdecl;
    function phase: NSEventPhase; cdecl;
    function pointingDeviceID: NSUInteger; cdecl;
    function pointingDeviceSerialNumber: NSUInteger; cdecl;
    function pointingDeviceType: NSPointingDeviceType; cdecl;
    function pressure: Single; cdecl;
    function pressureBehavior: NSPressureBehavior; cdecl;
    function rotation: Single; cdecl;
    function scrollingDeltaX: CGFloat; cdecl;
    function scrollingDeltaY: CGFloat; cdecl;
    function stage: NSInteger; cdecl;
    function stageTransition: CGFloat; cdecl;
    function subtype: NSEventSubtype; cdecl;
    function systemTabletID: NSUInteger; cdecl;
    function tabletID: NSUInteger; cdecl;
    function tangentialPressure: Single; cdecl;
    function tilt: NSPoint; cdecl;
    function timestamp: NSTimeInterval; cdecl;
    function touchesForView(view: NSView): NSSet; cdecl;
    function touchesMatchingPhase(phase: NSTouchPhase; inView: NSView): NSSet; cdecl;
    function trackingArea: NSTrackingArea; cdecl;
    function trackingNumber: NSInteger; cdecl;
    procedure trackSwipeEventWithOptions(options: NSEventSwipeTrackingOptions; dampenAmountThresholdMin: CGFloat; max: CGFloat;
      usingHandler: TNSEventBlockMethod1); cdecl;
    function &type: NSEventType; cdecl;
    function uniqueID: UInt64; cdecl;
    function userData: Pointer; cdecl;
    function vendorDefined: Pointer; cdecl;
    function vendorID: NSUInteger; cdecl;
    function vendorPointingDeviceType: NSUInteger; cdecl;
    function window: NSWindow; cdecl;
    function windowNumber: NSInteger; cdecl;
  end;
  TNSEvent = class(TOCGenericImport<NSEventClass, NSEvent>) end;

implementation

end.
