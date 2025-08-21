unit DW.StatusBarMenu.Mac;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.TypInfo, System.Classes, System.Generics.Collections, System.Types,
  // Posix
  Posix.SysTypes,
  // macOS
  Macapi.ObjectiveC, Macapi.Foundation, Macapi.AppKit, Macapi.CocoaTypes,
  // FMX
  FMX.Menus;

type
  NSEvent = interface;
  NSMenu = interface;
  NSMenuDelegate = interface;
  NSMenuItem = interface;
  NSStatusBar = interface;
  NSStatusItem = interface;
  NSStatusBarButton = interface;

  NSEventMask = NSInteger;
  NSEventButtonMask = NSInteger;
  NSEventPhase = NSInteger;
  NSEventGestureAxis = NSInteger;
  NSEventSwipeTrackingOptions = NSInteger;
  NSEventSubtype = NSInteger;
  NSPressureBehavior = NSInteger;
  NSStatusItemAutosaveName = NSString;
  NSStatusItemBehavior = NSInteger;

  TNSEventBlockMethod1 = procedure(gestureAmount: CGFloat; phase: NSEventPhase; isComplete: Boolean; stop: PBoolean) of object;
  TNSEventBlockMethod2 = procedure(event: NSEvent) of object;

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
    function &type: NSEventType; cdecl;
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
    function uniqueID: UInt64; cdecl;
    function userData: Pointer; cdecl;
    function vendorDefined: Pointer; cdecl;
    function vendorID: NSUInteger; cdecl;
    function vendorPointingDeviceType: NSUInteger; cdecl;
    function window: NSWindow; cdecl;
    function windowNumber: NSInteger; cdecl;
  end;
  TNSEvent = class(TOCGenericImport<NSEventClass, NSEvent>) end;

  NSMenuClass = interface(NSObjectClass)
    ['{466FF4ED-D50E-4325-A35C-11BA7A96EF1E}']
    {class} function menuBarVisible: Boolean; cdecl;
    {class} function menuZone: Pointer; cdecl;
    {class} procedure popUpContextMenu(menu: NSMenu; withEvent: NSEvent; forView: NSView); cdecl; overload;
    {class} procedure popUpContextMenu(menu: NSMenu; withEvent: NSEvent; forView: NSView; withFont: NSFont); cdecl; overload;
    {class} procedure setMenuBarVisible(visible: Boolean); cdecl;
    {class} procedure setMenuZone(aZone: Pointer); cdecl;
  end;

  NSMenu = interface(NSObject)
    ['{737A3D97-4B32-409B-A0E2-DB13288F93A8}']
    procedure addItem(newItem: NSMenuItem); cdecl;
    function addItemWithTitle(aString: NSString; action: SEL; keyEquivalent: NSString): NSMenuItem; cdecl;
    function allowsContextMenuPlugIns: Boolean; cdecl;
    function attachedMenu: NSMenu; cdecl;
    function autoenablesItems: Boolean; cdecl;
    procedure cancelTracking; cdecl;
    procedure cancelTrackingWithoutAnimation; cdecl;
    function contextMenuRepresentation: Pointer; cdecl;
    function delegate: NSMenuDelegate; cdecl;
    function font: NSFont; cdecl;
    procedure helpRequested(eventPtr: NSEvent); cdecl;
    function highlightedItem: NSMenuItem; cdecl;
    function indexOfItem(index: NSMenuItem): NSInteger; cdecl;
    function indexOfItemWithRepresentedObject(object_: Pointer): NSInteger; cdecl;
    function indexOfItemWithSubmenu(submenu: NSMenu): NSInteger; cdecl;
    function indexOfItemWithTag(aTag: NSInteger): NSInteger; cdecl;
    function indexOfItemWithTarget(target: Pointer; andAction: SEL): NSInteger; cdecl;
    function indexOfItemWithTitle(aTitle: NSString): NSInteger; cdecl;
    function initWithTitle(aTitle: NSString): Pointer; cdecl;
    procedure insertItem(newItem: NSMenuItem; atIndex: NSInteger); cdecl;
    function insertItemWithTitle(aString: NSString; action: SEL; keyEquivalent: NSString; atIndex: NSInteger): NSMenuItem; cdecl;
    function isAttached: Boolean; cdecl;
    function isTornOff: Boolean; cdecl;
    function itemArray: NSArray; cdecl;
    function itemAtIndex(index: NSInteger): NSMenuItem; cdecl;
    procedure itemChanged(item: NSMenuItem); cdecl;
    function itemWithTag(tag: NSInteger): NSMenuItem; cdecl;
    function itemWithTitle(aTitle: NSString): NSMenuItem; cdecl;
    function locationForSubmenu(aSubmenu: NSMenu): NSPoint; cdecl;
    function menuBarHeight: CGFloat; cdecl;
    function menuChangedMessagesEnabled: Boolean; cdecl;
    function menuRepresentation: Pointer; cdecl;
    function minimumWidth: CGFloat; cdecl;
    function numberOfItems: NSInteger; cdecl;
    procedure performActionForItemAtIndex(index: NSInteger); cdecl;
    function performKeyEquivalent(theEvent: NSEvent): Boolean; cdecl;
    function popUpMenuPositioningItem(item: NSMenuItem; atLocation: NSPoint; inView: NSView): Boolean; cdecl;
    function propertiesToUpdate: NSMenuProperties; cdecl;
    procedure removeAllItems; cdecl;
    procedure removeItem(item: NSMenuItem); cdecl;
    procedure removeItemAtIndex(index: NSInteger); cdecl;
    procedure setAllowsContextMenuPlugIns(allows: Boolean); cdecl;
    procedure setAutoenablesItems(flag: Boolean); cdecl;
    procedure setContextMenuRepresentation(menuRep: Pointer); cdecl;
    procedure setDelegate(anObject: NSMenuDelegate); cdecl;
    procedure setFont(font: NSFont); cdecl;
    procedure setMenuChangedMessagesEnabled(flag: Boolean); cdecl;
    procedure setMenuRepresentation(menuRep: Pointer); cdecl;
    procedure setMinimumWidth(width: CGFloat); cdecl;
    procedure setShowsStateColumn(showsState: Boolean); cdecl;
    procedure setSubmenu(aMenu: NSMenu; forItem: NSMenuItem); cdecl;
    procedure setSupermenu(supermenu: NSMenu); cdecl;
    procedure setTearOffMenuRepresentation(menuRep: Pointer); cdecl;
    procedure setTitle(aString: NSString); cdecl;
    function showsStateColumn: Boolean; cdecl;
    function size: NSSize; cdecl;
    procedure sizeToFit; cdecl;
    procedure submenuAction(sender: Pointer); cdecl;
    function supermenu: NSMenu; cdecl;
    function tearOffMenuRepresentation: Pointer; cdecl;
    function title: NSString; cdecl;
    procedure update; cdecl;
  end;
  TNSMenu = class(TOCGenericImport<NSMenuClass, NSMenu>)  end;

  NSMenuDelegate = interface(IObjectiveC)
    ['{035FCFD5-0D0E-4C84-B42A-5D4D4FCBB36E}']
    procedure menuWillOpen(menu: NSMenu); cdecl;
    procedure menuDidClose(menu: NSMenu); cdecl;
  end;

  NSMenuItemClass = interface(NSObjectClass)
    ['{29EAC351-981B-40F1-B0FB-B5F0136B5CF9}']
    {class} function separatorItem: Pointer; cdecl;
    {class} procedure setUsesUserKeyEquivalents(flag: Boolean); cdecl;
    {class} function usesUserKeyEquivalents: Boolean; cdecl;
  end;

  NSMenuItem = interface(NSObject)
    ['{0811DAFB-297C-452B-B90E-13A7BF7B2506}']
    function action: SEL; cdecl;
    function attributedTitle: NSAttributedString; cdecl;
    function hasSubmenu: Boolean; cdecl;
    function image: NSImage; cdecl;
    function indentationLevel: NSInteger; cdecl;
    function initWithTitle(aString: NSString; action: SEL; keyEquivalent: NSString): Pointer; cdecl;
    function isAlternate: Boolean; cdecl;
    function isEnabled: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isHiddenOrHasHiddenAncestor: Boolean; cdecl;
    function isHighlighted: Boolean; cdecl;
    function isSeparatorItem: Boolean; cdecl;
    function keyEquivalent: NSString; cdecl;
    function keyEquivalentModifierMask: NSUInteger; cdecl;
    function menu: NSMenu; cdecl;
    function mixedStateImage: NSImage; cdecl;
    function mnemonic: NSString; cdecl;
    function mnemonicLocation: NSUInteger; cdecl;
    function offStateImage: NSImage; cdecl;
    function onStateImage: NSImage; cdecl;
    function parentItem: NSMenuItem; cdecl;
    function representedObject: Pointer; cdecl;
    procedure setAction(aSelector: SEL); cdecl;
    procedure setAlternate(isAlternate: Boolean); cdecl;
    procedure setAttributedTitle(string_: NSAttributedString); cdecl;
    procedure setEnabled(flag: Boolean); cdecl;
    procedure setHidden(hidden: Boolean); cdecl;
    procedure setImage(menuImage: NSImage); cdecl;
    procedure setIndentationLevel(indentationLevel: NSInteger); cdecl;
    procedure setKeyEquivalent(aKeyEquivalent: NSString); cdecl;
    procedure setKeyEquivalentModifierMask(mask: NSUInteger); cdecl;
    procedure setMenu(menu: NSMenu); cdecl;
    procedure setMixedStateImage(image: NSImage); cdecl;
    procedure setMnemonicLocation(location: NSUInteger); cdecl;
    procedure setOffStateImage(image: NSImage); cdecl;
    procedure setOnStateImage(image: NSImage); cdecl;
    procedure setRepresentedObject(anObject: Pointer); cdecl;
    procedure setState(state: NSInteger); cdecl;
    procedure setSubmenu(submenu: NSMenu); cdecl;
    procedure setTag(anInt: NSInteger); cdecl;
    procedure setTarget(anObject: Pointer); cdecl;
    procedure setTitle(aString: NSString); cdecl;
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); cdecl;
    procedure setToolTip(toolTip: NSString); cdecl;
    procedure setView(view: NSView); cdecl;
    function state: NSInteger; cdecl;
    function submenu: NSMenu; cdecl;
    function tag: NSInteger; cdecl;
    function target: Pointer; cdecl;
    function title: NSString; cdecl;
    function toolTip: NSString; cdecl;
    function userKeyEquivalent: NSString; cdecl;
    function view: NSView; cdecl;
  end;
  TNSMenuItem = class(TOCGenericImport<NSMenuItemClass, NSMenuItem>)  end;

  NSRunningApplicationClass = interface(NSObjectClass)
    ['{D1021A09-B8E3-4E18-9223-6ACB97808F66}']
    {class} function currentApplication: NSRunningApplication; cdecl;
    {class} function runningApplicationsWithBundleIdentifier(bundleIdentifier: NSString): NSArray; cdecl;
    {class} function runningApplicationWithProcessIdentifier(pid: pid_t): Pointer; cdecl;
    {class} procedure terminateAutomaticallyTerminableApplications; cdecl;
  end;

  NSRunningApplication = interface(NSObject)
    ['{D80ACF06-634E-41A6-94D7-1D68FECB14CA}']
    function activateWithOptions(options: NSApplicationActivationOptions): Boolean; cdecl;
    function activationPolicy: NSApplicationActivationPolicy; cdecl;
    function bundleIdentifier: NSString; cdecl;
    function bundleURL: NSURL; cdecl;
    function executableArchitecture: NSInteger; cdecl;
    function executableURL: NSURL; cdecl;
    function forceTerminate: Boolean; cdecl;
    function hide: Boolean; cdecl;
    function icon: NSImage; cdecl;
    function isActive: Boolean; cdecl;
    function isFinishedLaunching: Boolean; cdecl;
    function isHidden: Boolean; cdecl;
    function isTerminated: Boolean; cdecl;
    function launchDate: NSDate; cdecl;
    function localizedName: NSString; cdecl;
    function ownsMenuBar: Boolean; cdecl;
    function processIdentifier: pid_t; cdecl;
    function terminate: Boolean; cdecl;
    function unhide: Boolean; cdecl;
  end;
  TNSRunningApplication = class(TOCGenericImport<NSRunningApplicationClass, NSRunningApplication>) end;

  NSStatusBarClass = interface(NSObjectClass)
    ['{1345A486-3890-4622-A2E8-06DABA543211}']
    {class} function systemStatusBar: Pointer; cdecl;
  end;

  NSStatusBar = interface(NSObject)
    ['{980D0624-6813-4FE7-8516-D687EB8BFC38}']
    function isVertical: Boolean; cdecl;
    procedure removeStatusItem(item: NSStatusItem); cdecl;
    function statusItemWithLength(length: CGFloat): NSStatusItem; cdecl;
    function thickness: CGFloat; cdecl;
  end;
  TNSStatusBar = class(TOCGenericImport<NSStatusBarClass, NSStatusBar>)  end;

  NSStatusItemClass = interface(NSObjectClass)
    ['{680C1E42-4733-43E0-A1F9-590AED73D248}']
  end;

  NSStatusItem = interface(NSObject)
    ['{77F1A846-8084-4145-97B2-D8348185A7D9}']
    function action: SEL; cdecl;
    function alternateImage: NSImage; cdecl;
    function attributedTitle: NSAttributedString; cdecl;
    function autosaveName: NSStatusItemAutosaveName; cdecl;
    function behavior: NSStatusItemBehavior; cdecl;
    function button: NSStatusBarButton; cdecl;
    function doubleAction: SEL; cdecl;
    procedure drawStatusBarBackgroundInRect(rect: NSRect; withHighlight: Boolean); cdecl;
    function highlightMode: Boolean; cdecl;
    function image: NSImage; cdecl;
    function isEnabled: Boolean; cdecl;
    function isVisible: Boolean; cdecl;
    function length: CGFloat; cdecl;
    function menu: NSMenu; cdecl;
    procedure popUpStatusItemMenu(menu: NSMenu); cdecl;
    function sendActionOn(mask: NSEventMask): NSInteger; cdecl;
    procedure setAction(action: SEL); cdecl;
    procedure setAlternateImage(alternateImage: NSImage); cdecl;
    procedure setAttributedTitle(attributedTitle: NSAttributedString); cdecl;
    procedure setAutosaveName(autosaveName: NSStatusItemAutosaveName); cdecl;
    procedure setBehavior(behavior: NSStatusItemBehavior); cdecl;
    procedure setDoubleAction(doubleAction: SEL); cdecl;
    procedure setEnabled(enabled: Boolean); cdecl;
    procedure setHighlightMode(highlightMode: Boolean); cdecl;
    procedure setImage(image: NSImage); cdecl;
    procedure setLength(length: CGFloat); cdecl;
    procedure setMenu(menu: NSMenu); cdecl;
    procedure setTarget(target: Pointer); cdecl;
    procedure setTitle(title: NSString); cdecl;
    procedure setToolTip(toolTip: NSString); cdecl;
    procedure setView(view: NSView); cdecl;
    procedure setVisible(visible: Boolean); cdecl;
    function statusBar: NSStatusBar; cdecl;
    function target: Pointer; cdecl;
    function title: NSString; cdecl;
    function toolTip: NSString; cdecl;
    function view: NSView; cdecl;
  end;
  TNSStatusItem = class(TOCGenericImport<NSStatusItemClass, NSStatusItem>) end;

  NSStatusBarButtonClass = interface(NSButtonClass)
    ['{EF3FF56D-2FC7-4749-9E10-B8826CA4B68A}']
  end;

  NSStatusBarButton = interface(NSButton)
    ['{C3FC50F0-DA89-42E5-96EE-D0D7725DF832}']
    function appearsDisabled: Boolean; cdecl;
    procedure setAppearsDisabled(appearsDisabled: Boolean); cdecl;
  end;
  TNSStatusBarButton = class(TOCGenericImport<NSStatusBarButtonClass, NSStatusBarButton>) end;

  TStatusBarMenu = class;

  IMacOSMenuItem = interface(NSObject)
    ['{612FEE9D-CA01-407E-9B1F-FDF9A630F6E5}']
    procedure Execute(Sender: Pointer); cdecl;
  end;

  TMacOSMenuItem = class(TOCLocal)
  private
    FFMXItem: TMenuItem;
    FNSMenuItem: NSMenuItem;
    FStatusBarMenu: TStatusBarMenu;
    FSubMenu: NSMenu;
    procedure CreateMenuItem(const AMenu: NSMenu; const AItem: TMenuItem);
    function GetSubMenu: NSMenu;
  protected
    procedure Refresh;
    property FMXItem: TMenuItem read FFMXItem;
    property StatusBarMenu: TStatusBarMenu read FStatusBarMenu;
    property SubMenu: NSMenu read GetSubMenu;
  public
    { IMacOSMenuItem }
    procedure Execute(Sender: Pointer); cdecl;
  public
    constructor Create(const AStatusBarMenu: TStatusBarMenu; const AItem: TMenuItem); overload;
    constructor Create(const AMacOSMenuItem: TMacOSMenuItem; const AItem: TMenuItem); overload;
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
  end;

  IMacOSStatusItem = interface(NSObject)
    ['{D9EA9551-35B9-446C-B4E9-3591CEB023C8}']
  end;

  TMacOSStatusItem = class(TOCLocal)
  private
    FFMXItem: TMenuItem;
    FNSStatusItem: NSStatusItem;
    FStatusBarMenu: TStatusBarMenu;
  protected
    procedure Click;
    procedure Refresh;
    property NSStatusItem: NSStatusItem read FNSStatusItem;
  public
    constructor Create(const AStatusBarMenu: TStatusBarMenu; const AItem: TMenuItem);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
  end;

  IApplicationNotifications = interface(NSObject)
    ['{06EC4A82-8943-4E82-B422-947927955F61}']
    procedure applicationActivated(notification: NSNotification); cdecl;
  end;

  TApplicationNotifications = class(TOCLocal)
  private
    FStatusBarMenu: TStatusBarMenu;
  public
    { IApplicationNotifications }
    procedure applicationActivated(notification: NSNotification); cdecl;
  public
    constructor Create(const AStatusBarMenu: TStatusBarMenu);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
  end;

  TMenuDelegate = class(TOCLocal, NSMenuDelegate)
  private
    FStatusBarMenu: TStatusBarMenu;
  public
    { NSMenuDelegate }
    procedure menuDidClose(menu: NSMenu); cdecl;
    procedure menuWillOpen(menu: NSMenu); cdecl;
  public
    constructor Create(const AStatusBarMenu: TStatusBarMenu);
  end;

  TMacOSMenuItems = class(TList<TMacOSMenuItem>)
  protected
    function IndexOfFMXItem(const AComponent: TComponent): Integer;
  end;

  TStatusBarMenu = class(TComponent)
  private
    FAppNotifications: TApplicationNotifications;
    FMenu: NSMenu;
    FMenuDelegate: NSMenuDelegate;
    FMenuItems: TMacOSMenuItems;
    FPopupMenu: TPopupMenu;
    FStatusItem: TMacOSStatusItem;
    FUpdateCount: Integer;
    FOnMenuClose: TNotifyEvent;
    FOnMenuOpen: TNotifyEvent;
    procedure AddMenuItem(const AItem: TMenuItem);
    procedure AddSubMenuItem(const AMacOSMenuItem: TMacOSMenuItem; const AItem: TMenuItem);
    procedure ClearMenuItems;
    function GetRect: TRectF;
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure UpdateMenuItem(const AItem: TMenuItem);
  protected
    class function StatusBar: NSStatusBar;
  protected
    procedure MenuDidClose;
    procedure MenuWillOpen;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Menu: NSMenu read FMenu;
    property MenuItems: TMacOSMenuItems read FMenuItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RecreateMenu;
    procedure Refresh;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Rect: TRectF read GetRect;
    property OnMenuClose: TNotifyEvent read FOnMenuClose write FOnMenuClose;
    property OnMenuOpen: TNotifyEvent read FOnMenuOpen write FOnMenuOpen;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjCRuntime,
  // FMX
  FMX.Helpers.Mac, FMX.Graphics;

const
  NSEventTypeLeftMouseDown = 1;
  NSEventTypeLeftMouseUp = 2;
  NSEventTypeRightMouseDown = 3;
  NSEventTypeRightMouseUp = 4;
  NSEventTypeMouseMoved = 5;
  NSEventTypeLeftMouseDragged = 6;
  NSEventTypeRightMouseDragged = 7;
  NSEventTypeMouseEntered = 8;
  NSEventTypeMouseExited = 9;
  NSEventTypeKeyDown = 10;
  NSEventTypeKeyUp = 11;
  NSEventTypeFlagsChanged = 12;
  NSEventTypeAppKitDefined = 13;
  NSEventTypeSystemDefined = 14;
  NSEventTypeApplicationDefined = 15;
  NSEventTypePeriodic = 16;
  NSEventTypeCursorUpdate = 17;
  NSEventTypeScrollWheel = 22;
  NSEventTypeTabletPoint = 23;
  NSEventTypeTabletProximity = 24;
  NSEventTypeOtherMouseDown = 25;
  NSEventTypeOtherMouseUp = 26;
  NSEventTypeOtherMouseDragged = 27;
  NSEventTypeGesture = 29;
  NSEventTypeMagnify = 30;
  NSEventTypeSwipe = 31;
  NSEventTypeRotate = 18;
  NSEventTypeBeginGesture = 19;
  NSEventTypeEndGesture = 20;
  NSEventTypeSmartMagnify = 32;
  NSEventTypeQuickLook = 33;
  NSEventTypePressure = 34;
  NSEventTypeDirectTouch = 37;
  NSEventTypeChangeMode = 38;
  NSEventMaskLeftMouseDown = 2;
  NSEventMaskLeftMouseUp = 4;
  NSEventMaskRightMouseDown = 8;
  NSEventMaskRightMouseUp = 16;
  NSEventMaskMouseMoved = 32;
  NSEventMaskLeftMouseDragged = 64;
  NSEventMaskRightMouseDragged = 128;
  NSEventMaskMouseEntered = 256;
  NSEventMaskMouseExited = 512;
  NSEventMaskKeyDown = 1024;
  NSEventMaskKeyUp = 2048;
  NSEventMaskFlagsChanged = 4096;
  NSEventMaskAppKitDefined = 8192;
  NSEventMaskSystemDefined = 16384;
  NSEventMaskApplicationDefined = 32768;
  NSEventMaskPeriodic = 65536;
  NSEventMaskCursorUpdate = 131072;
  NSEventMaskScrollWheel = 4194304;
  NSEventMaskTabletPoint = 8388608;
  NSEventMaskTabletProximity = 16777216;
  NSEventMaskOtherMouseDown = 33554432;
  NSEventMaskOtherMouseUp = 67108864;
  NSEventMaskOtherMouseDragged = 134217728;
  NSEventMaskGesture = 536870912;
  NSEventMaskMagnify = 1073741824;
  NSEventMaskSwipe = 2147483648;
  NSEventMaskRotate = 262144;
  NSEventMaskBeginGesture = 524288;
  NSEventMaskEndGesture = 1048576;
  NSEventMaskSmartMagnify = 4294967296;
  NSEventMaskPressure = 17179869184;
  NSEventMaskDirectTouch = 137438953472;
  NSEventMaskChangeMode = 274877906944;
  NSEventMaskAny = -1;
  NSEventModifierFlagCapsLock = 65536;
  NSEventModifierFlagShift = 131072;
  NSEventModifierFlagControl = 262144;
  NSEventModifierFlagOption = 524288;
  NSEventModifierFlagCommand = 1048576;
  NSEventModifierFlagNumericPad = 2097152;
  NSEventModifierFlagHelp = 4194304;
  NSEventModifierFlagFunction = 8388608;
  NSEventModifierFlagDeviceIndependentFlagsMask = 4294901760;
  NSPressureBehaviorUnknown = -1;
  NSPressureBehaviorPrimaryDefault = 0;
  NSPressureBehaviorPrimaryClick = 1;
  NSPressureBehaviorPrimaryGeneric = 2;
  NSPressureBehaviorPrimaryAccelerator = 3;
  NSPressureBehaviorPrimaryDeepClick = 5;
  NSPressureBehaviorPrimaryDeepDrag = 6;

  libAppKit = '/System/Library/Frameworks/AppKit.framework/AppKit';

type
  TPopupMenuHelper = class helper for TPopupMenu
  public
    function VisibleItemCount: Integer;
  end;

function NSWorkspaceApplicationKey: NSString;
begin
  Result := CocoaNSStringConst(libAppKit, 'NSWorkspaceApplicationKey');
end;

function NSWorkspaceDidActivateApplicationNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAppKit, 'NSWorkspaceDidActivateApplicationNotification');
end;

function NSWorkspaceDidDeactivateApplicationNotification: NSNotificationName;
begin
  Result := CocoaNSStringConst(libAppKit, 'NSWorkspaceDidDeactivateApplicationNotification');
end;

{ TPopupMenuHelper }

function TPopupMenuHelper.VisibleItemCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ItemsCount - 1 do
  begin
    if Items[I].Visible then
      Inc(Result);
  end;
end;

function SharedApplication: NSApplication;
begin
  Result := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
end;

function SharedWorkspace: NSWorkspace;
begin
  Result := TNSWorkspace.Wrap(TNSWorkspace.OCClass.sharedWorkspace);
end;

function GetMenuImage(const AFMXItem: TMenuItem): NSImage;
var
  LBitmap: TBitmap;
begin
  LBitmap := nil;
  if AFMXItem.Images <> nil then
    LBitmap := AFMXItem.Images.Bitmap(TSizeF.Create(16, 16), AFMXItem.ImageIndex);
  if LBitmap = nil then
    LBitmap := AFMXItem.Bitmap;
  if (LBitmap <> nil) and not LBitmap.IsEmpty then
    Result := BitmapToMenuBitmap(LBitmap)
  else
    Result := nil;
end;

procedure UpdateNSMenuItemImage(const ANSItem: NSMenuItem; const AFMXItem: TMenuItem);
var
  LImage: NSImage;
begin
  LImage := GetMenuImage(AFMXItem);
  try
    ANSItem.setImage(LImage);
  finally
    if LImage <> nil then
      LImage.release;
  end;
end;

procedure UpdateNSStatusItemImage(const ANSStatusItem: NSStatusItem; const AFMXItem: TMenuItem);
var
  LImage: NSImage;
begin
  LImage := GetMenuImage(AFMXItem);
  try
    ANSStatusItem.setImage(LImage);
  finally
    if LImage <> nil then
      LImage.release;
  end;
end;

{ TMacOSMenuItems }

function TMacOSMenuItems.IndexOfFMXItem(const AComponent: TComponent): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].FMXItem = AComponent then
      Exit(I);
  end;
end;

{ TMacOSMenuItem }

constructor TMacOSMenuItem.Create(const AStatusBarMenu: TStatusBarMenu; const AItem: TMenuItem);
begin
  inherited Create;
  FStatusBarMenu := AStatusBarMenu;
  CreateMenuItem(FStatusBarMenu.Menu, AItem);
  FStatusBarMenu.MenuItems.Add(Self);
end;

constructor TMacOSMenuItem.Create(const AMacOSMenuItem: TMacOSMenuItem; const AItem: TMenuItem);
begin
  inherited Create;
  CreateMenuItem(AMacOSMenuItem.SubMenu, AItem);
  AMacOSMenuItem.StatusBarMenu.MenuItems.Add(Self);
end;

procedure TMacOSMenuItem.CreateMenuItem(const AMenu: NSMenu; const AItem: TMenuItem);
begin
  FFMXItem := AItem;
  FNSMenuItem := TNSMenuItem.Create;
  if not AItem.Text.Equals('-') then
  begin
    FNSMenuItem := TNSMenuItem.Wrap(FNSMenuItem.initWithTitle(StrToNSStr(FFMXItem.Text), sel_getUid(MarshaledAString('Execute:')), StrToNSStr('')));
    FNSMenuItem.setTarget(GetObjectID);
    UpdateNSMenuItemImage(FNSMenuItem, FFMXItem);
  end
  else
    FNSMenuItem := TNSMenuItem.Wrap(TNSMenuItem.OCClass.separatorItem);
  AMenu.addItem(FNSMenuItem);
end;

destructor TMacOSMenuItem.Destroy;
begin
  FFMXItem := nil;
  FStatusBarMenu := nil;
  if FNSMenuItem <> nil then
  begin
    if FNSMenuItem.menu <> nil then
      FNSMenuItem.menu.removeItem(FNSMenuItem);
    FNSMenuItem.release;
  end;
  FNSMenuItem := nil;
  inherited;
end;

function TMacOSMenuItem.GetSubMenu: NSMenu;
begin
  if FSubMenu = nil then
  begin
    FSubMenu := TNSMenu.Create;
    FNSMenuItem.setSubmenu(FSubMenu);
  end;
  Result := FSubMenu;
end;

procedure TMacOSMenuItem.Refresh;
begin
  if not FFMXItem.Text.Equals('-') then
  begin
    FNSMenuItem.setTitle(StrToNSStr(FFMXItem.Text));
    UpdateNSMenuItemImage(FNSMenuItem, FFMXItem);
  end;
end;

function TMacOSMenuItem.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IMacOSMenuItem);
end;

procedure TMacOSMenuItem.Execute(Sender: Pointer);
begin
  if (FFMXItem <> nil) and Assigned(FFMXItem.OnClick) then
    FFMXItem.OnClick(FFMXItem);
end;

{ TMacOSStatusItem }

constructor TMacOSStatusItem.Create(const AStatusBarMenu: TStatusBarMenu; const AItem: TMenuItem);
begin
  inherited Create;
  FStatusBarMenu := AStatusBarMenu;
  FFMXItem := AItem;
  FNSStatusItem := FStatusBarMenu.StatusBar.statusItemWithLength(NSVariableStatusItemLength);
  FNSStatusItem.setMenu(FStatusBarMenu.Menu);
  FNSStatusItem.setHighlightMode(True);
  Refresh;
end;

destructor TMacOSStatusItem.Destroy;
begin
  FNSStatusItem.setMenu(nil);
  FStatusBarMenu.StatusBar.removeStatusItem(FNSStatusItem);
  FNSStatusItem.release;
  inherited;
end;

procedure TMacOSStatusItem.Click;
begin
  if (FFMXItem <> nil) and Assigned(FFMXItem.OnClick) then
    FFMXItem.OnClick(FFMXItem);
end;

function TMacOSStatusItem.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IMacOSStatusItem);
end;

procedure TMacOSStatusItem.Refresh;
begin
  UpdateNSStatusItemImage(FNSStatusItem, FFMXItem);
end;

{ TMenuDelegate }

constructor TMenuDelegate.Create(const AStatusBarMenu: TStatusBarMenu);
begin
  inherited Create;
  FStatusBarMenu := AStatusBarMenu;
end;

procedure TMenuDelegate.menuDidClose(menu: NSMenu);
begin
  // No action here because it seems to be called even when the menu could still be considered "open"
end;

procedure TMenuDelegate.menuWillOpen(menu: NSMenu);
begin
  FStatusBarMenu.MenuWillOpen;
end;

{ TApplicationNotifications }

constructor TApplicationNotifications.Create(const AStatusBarMenu: TStatusBarMenu);
begin
  inherited Create;
  FStatusBarMenu := AStatusBarMenu;
  SharedWorkspace.notificationCenter.addObserver(GetObjectID, sel_getUid('applicationActivated:'),
    NSStringToID(NSWorkspaceDidActivateApplicationNotification), nil);
end;

destructor TApplicationNotifications.Destroy;
begin
  //
  inherited;
end;

procedure TApplicationNotifications.applicationActivated(notification: NSNotification);
var
  LApplication: NSRunningApplication;
begin
  LApplication := TNSRunningApplication.Wrap(notification.userInfo.objectForKey(NSStringToID(NSWorkspaceApplicationKey)));
  // Menu is considered "closed" when focus shifts to another app
  if TNSRunningApplication.OCClass.currentApplication.processIdentifier <> LApplication.processIdentifier then
    FStatusBarMenu.MenuDidClose;
end;

function TApplicationNotifications.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IApplicationNotifications);
end;

{ TStatusBarMenu }

constructor TStatusBarMenu.Create(AOwner: TComponent);
begin
  inherited;
  FMenuItems := TMacOSMenuItems.Create;
  FMenuDelegate := TMenuDelegate.Create(Self);
  FMenu := TNSMenu.Create;
  FMenu.setDelegate(FMenuDelegate);
  FAppNotifications := TApplicationNotifications.Create(Self);
end;

destructor TStatusBarMenu.Destroy;
begin
  FAppNotifications.Free;
  ClearMenuItems;
  FMenuItems.Free;
  FMenu.release;
  FMenu := nil;
  inherited;
end;

procedure TStatusBarMenu.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TStatusBarMenu.EndUpdate;
begin
  if FUpdateCount > 0 then
    Dec(FUpdateCount);
end;

function TStatusBarMenu.GetRect: TRectF;
begin
  Result := TNSView.Wrap(FStatusItem.NSStatusItem.button.superview).window.frame.ToRectF;
end;

procedure TStatusBarMenu.MenuDidClose;
begin
  if Assigned(FOnMenuClose) then
    FOnMenuClose(Self);
end;

procedure TStatusBarMenu.MenuWillOpen;
begin
  if Assigned(FOnMenuOpen) then
    FOnMenuOpen(Self);
  if FStatusItem <> nil then
    FStatusItem.Click;
end;

procedure TStatusBarMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = TOperation.opRemove then
  begin
    if AComponent = FPopupMenu then
    begin
      AComponent.RemoveFreeNotification(Self);
      PopupMenu := nil;
    end
    else if FMenuItems.IndexOfFMXItem(AComponent) > -1 then
    begin
      AComponent.RemoveFreeNotification(Self);
      if FUpdateCount = 0 then
        RecreateMenu;
    end;
  end;
end;

procedure TStatusBarMenu.RecreateMenu;
var
  I: Integer;
begin
  ClearMenuItems;
  if (FPopupMenu <> nil) and (FPopupMenu.VisibleItemCount > 0) then
  begin
    FStatusItem := TMacOSStatusItem.Create(Self, FPopupMenu.Items[0]);
    for I := 1 to FPopupMenu.ItemsCount - 1 do
      AddMenuItem(FPopupMenu.Items[I]);
  end;
end;

procedure TStatusBarMenu.Refresh;
var
  I: Integer;
begin
  if (FPopupMenu <> nil) and (FPopupMenu.VisibleItemCount > 0) then
  begin
    FStatusItem.Refresh;
    for I := 1 to FPopupMenu.ItemsCount - 1 do
      UpdateMenuItem(FPopupMenu.Items[I]);
  end;
end;

procedure TStatusBarMenu.UpdateMenuItem(const AItem: TMenuItem);
var
  I, LIndex: Integer;
begin
  LIndex := FMenuItems.IndexOfFMXItem(AItem);
  if LIndex > -1 then
    MenuItems.Items[LIndex].Refresh;
  for I := 0 to AItem.ItemsCount - 1 do
    UpdateMenuItem(AItem.Items[I]);
end;

procedure TStatusBarMenu.AddMenuItem(const AItem: TMenuItem);
var
  I: Integer;
  LMacOSMenuItem: TMacOSMenuItem;
begin
  if AItem.Visible then
  begin
    AItem.FreeNotification(Self);
    LMacOSMenuItem := TMacOSMenuItem.Create(Self, AItem);
    for I := 0 to AItem.ItemsCount - 1 do
      AddSubMenuItem(LMacOSMenuItem, AItem.Items[I]);
  end;
end;

procedure TStatusBarMenu.AddSubMenuItem(const AMacOSMenuItem: TMacOSMenuItem; const AItem: TMenuItem);
var
  I: Integer;
  LSubItem: TMacOSMenuItem;
begin
  if AItem.Visible then
  begin
    AItem.FreeNotification(Self);
    LSubItem := TMacOSMenuItem.Create(AMacOSMenuItem, AItem);
    for I := 0 to AItem.ItemsCount - 1 do
      AddSubMenuItem(LSubItem, AItem.Items[I]);
  end;
end;

procedure TStatusBarMenu.ClearMenuItems;
var
  LItem: TMacOSMenuItem;
  LIndex: Integer;
begin
  while FMenuItems.Count > 0 do
  begin
    LIndex := FMenuItems.Count - 1;
    LItem := FMenuItems.Items[LIndex];
    if LItem.FMXItem <> nil then
      LItem.FMXItem.RemoveFreeNotification(Self);
    FMenuItems.Delete(LIndex);
    LItem.Free;
  end;
  FStatusItem.Free;
end;

procedure TStatusBarMenu.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    if FPopupMenu <> nil then
      FPopupMenu.RemoveFreeNotification(Self);
    FPopupMenu := Value;
    if FPopupMenu <> nil then
      FPopupMenu.FreeNotification(Self);
    RecreateMenu;
  end;
end;

class function TStatusBarMenu.StatusBar: NSStatusBar;
begin
  Result := TNSStatusBar.Wrap(TNSStatusBar.OCClass.systemStatusBar);
end;

end.
