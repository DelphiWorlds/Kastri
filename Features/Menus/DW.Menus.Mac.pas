unit DW.Menus.Mac;

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
  System.Classes, System.TypInfo,
  // macOS
  Macapi.ObjectiveC, Macapi.Foundation, Macapi.AppKit, Macapi.CocoaTypes,
  // FMX
  FMX.Graphics,
  // DW
  DW.Macapi.AppKit;

type
  NSMenu = interface;
  NSMenuDelegate = interface;
  NSMenuItem = interface;
  NSStatusBar = interface;
  NSStatusBarButton = interface;
  NSStatusItem = interface;

  NSStatusItemAutosaveName = NSString;
  NSStatusItemBehavior = NSInteger;

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

  NSStatusBarButtonClass = interface(NSButtonClass)
    ['{EF3FF56D-2FC7-4749-9E10-B8826CA4B68A}']
  end;

  NSStatusBarButton = interface(NSButton)
    ['{C3FC50F0-DA89-42E5-96EE-D0D7725DF832}']
    function appearsDisabled: Boolean; cdecl;
    procedure setAppearsDisabled(appearsDisabled: Boolean); cdecl;
  end;
  TNSStatusBarButton = class(TOCGenericImport<NSStatusBarButtonClass, NSStatusBarButton>) end;

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

  IMacOSMenuItem = interface(NSObject)
    ['{612FEE9D-CA01-407E-9B1F-FDF9A630F6E5}']
    procedure Execute(Sender: Pointer); cdecl;
  end;

  TPlatformMenuItem = class;

  TPlatformMenuItems = TArray<TPlatformMenuItem>;

  TPlatformMenuItem = class(TOCLocal)
  private
    FIndex: Integer;
    FItem: NSMenuItem;
    FSubItems: TPlatformMenuItems;
    FSubMenu: NSMenu;
    FOnExecute: TNotifyEvent;
    procedure DestroySubItems;
    function GetSubMenu: NSMenu;
    function GetVisible: Boolean;
    procedure Reindex;
    procedure SetVisible(const Value: Boolean);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    procedure SetIndex(const Value: Integer);
    property SubMenu: NSMenu read GetSubMenu;
  public
    { IMacOSMenuItem }
    procedure Execute(Sender: Pointer); cdecl;
  public
    constructor Create(const ATitle: string; const AMenu: NSMenu);
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer;
    function CreateSeparator: TPlatformMenuItem;
    function CreateSubItem(const ATitle: string; const AExecuteHandler: TNotifyEvent = nil): TPlatformMenuItem;
    procedure DeleteSubItem(const AIndex: Integer);
    procedure SetImage(const ABitmap: TBitmap);
    property Index: Integer read FIndex;
    property Title: string read GetTitle write SetTitle;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
  end;

  IMacOSStatusItem = interface(NSObject)
    ['{D9EA9551-35B9-446C-B4E9-3591CEB023C8}']
  end;

  TPlatformStatusItem = class(TOCLocal)
  private
    class function StatusBar: NSStatusBar; static;
  private
    FStatusItem: NSStatusItem;
    FOnClick: TNotifyEvent;
  protected
    procedure Click;
    function GetObjectiveCClass: PTypeInfo; override;
    property StatusItem: NSStatusItem read FStatusItem;
  public
    constructor Create(const AMenu: NSMenu);
    destructor Destroy; override;
    procedure SetImage(const ABitmap: TBitmap);
  end;

  TPlatformMenu = class;

  TPlatformMenuDelegate = class(TOCLocal, NSMenuDelegate)
  private
    FPlatformMenu: TPlatformMenu;
  public
    { NSMenuDelegate }
    procedure menuDidClose(menu: NSMenu); cdecl;
    procedure menuWillOpen(menu: NSMenu); cdecl;
  public
    constructor Create(const APlatformMenu: TPlatformMenu);
  end;

  TPlatformMenu = class(TObject)
  private
    FDelegate: NSMenuDelegate;
    FItems: TPlatformMenuItems;
    FMenu: NSMenu;
    FOnMenuClose: TNotifyEvent;
    FOnMenuOpen: TNotifyEvent;
    procedure AddItem(const AItem: TPlatformMenuItem);
    procedure DestroyItems;
  protected
    procedure DidClose;
    procedure WillOpen;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateItem(const ATitle: string; const AExecuteHandler: TNotifyEvent = nil): TPlatformMenuItem; overload;
    function CreateItem(const ATitle: string; const AImage: TBitmap; const AExecuteHandler: TNotifyEvent = nil): TPlatformMenuItem; overload;
    procedure CreateSeparator;
    function Count: Integer;
    property Menu: NSMenu read FMenu;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers,
  // FMX
  FMX.Helpers.Mac;

{ TPlatformMenuItem }

constructor TPlatformMenuItem.Create(const ATitle: string; const AMenu: NSMenu);
var
  LSelector: SEL;
begin
  inherited Create;
  if not ATitle.Equals('-') then
  begin
    LSelector := sel_getUid(MarshaledAString('Execute:'));
    FItem := TNSMenuItem.Wrap(TNSMenuItem.Alloc.initWithTitle(StrToNSStr(ATitle), LSelector, StrToNSStr('')));
    FItem.setTarget(GetObjectID);
  end
  else
    FItem := TNSMenuItem.Wrap(TNSMenuItem.OCClass.separatorItem);
  AMenu.addItem(FItem);
end;

destructor TPlatformMenuItem.Destroy;
begin
  DestroySubItems;
  if FItem <> nil then
  begin
    if FItem.menu <> nil then
      FItem.menu.removeItem(FItem);
    // Following line causes an AV??
    // FItem.release;
  end;
  FItem := nil;
  inherited;
end;

procedure TPlatformMenuItem.DestroySubItems;
begin
  while Length(FSubItems) > 0 do
  begin
    FSubItems[0].Free;
    Delete(FSubItems, 0, 1);
  end;
end;

function TPlatformMenuItem.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IMacOSMenuItem);
end;

function TPlatformMenuItem.GetSubMenu: NSMenu;
begin
  if FSubMenu = nil then
  begin
    FSubMenu := TNSMenu.Create;
    FItem.setSubmenu(FSubMenu);
  end;
  Result := FSubMenu;
end;

function TPlatformMenuItem.GetTitle: string;
begin
  Result := NSStrToStr(FItem.title);
end;

function TPlatformMenuItem.GetVisible: Boolean;
begin
  Result := not FItem.isHidden;
end;

procedure TPlatformMenuItem.Reindex;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FSubItems[I].SetIndex(I);
end;

procedure TPlatformMenuItem.SetImage(const ABitmap: TBitmap);
begin
  FItem.setImage(BitmapToMenuBitmap(ABitmap));
end;

procedure TPlatformMenuItem.SetIndex(const Value: Integer);
begin
  FIndex := Value;
end;

procedure TPlatformMenuItem.SetTitle(const Value: string);
begin
  FItem.setTitle(StrToNSStr(Value));
end;

procedure TPlatformMenuItem.SetVisible(const Value: Boolean);
begin
  FItem.setHidden(Value);
end;

procedure TPlatformMenuItem.Clear;
begin
  DestroySubItems;
end;

function TPlatformMenuItem.Count: Integer;
begin
  Result := Length(FSubItems);
end;

procedure TPlatformMenuItem.Execute(Sender: Pointer);
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

function TPlatformMenuItem.CreateSeparator: TPlatformMenuItem;
begin
  Result := CreateSubItem('-');
end;

function TPlatformMenuItem.CreateSubItem(const ATitle: string; const AExecuteHandler: TNotifyEvent = nil): TPlatformMenuItem;
begin
  Result := TPlatformMenuItem.Create(ATitle, SubMenu);
  if Assigned(AExecuteHandler) then
    Result.OnExecute := AExecuteHandler;
  FSubItems := FSubItems + [Result];
  Result.SetIndex(Count - 1);
end;

procedure TPlatformMenuItem.DeleteSubItem(const AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    FSubItems[AIndex].Free;
    Delete(FSubItems, AIndex, 1);
    Reindex;
  end;
end;

{ TPlatformStatusItem }

constructor TPlatformStatusItem.Create(const AMenu: NSMenu);
begin
  inherited Create;
  FStatusItem := StatusBar.statusItemWithLength(NSVariableStatusItemLength);
  FStatusItem.setMenu(AMenu);
  FStatusItem.setHighlightMode(True);
end;

destructor TPlatformStatusItem.Destroy;
begin
  FStatusItem.setMenu(nil);
  FStatusItem := nil;
  inherited;
end;

procedure TPlatformStatusItem.SetImage(const ABitmap: TBitmap);
begin
  FStatusItem.setImage(BitmapToMenuBitmap(ABitmap));
end;

class function TPlatformStatusItem.StatusBar: NSStatusBar;
begin
  Result := TNSStatusBar.Wrap(TNSStatusBar.OCClass.systemStatusBar);
end;

procedure TPlatformStatusItem.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TPlatformStatusItem.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IMacOSStatusItem);
end;

{ TPlatformMenuDelegate }

constructor TPlatformMenuDelegate.Create(const APlatformMenu: TPlatformMenu);
begin
  inherited Create;
  FPlatformMenu := APlatformMenu;
end;

procedure TPlatformMenuDelegate.menuDidClose(menu: NSMenu);
begin
  FPlatformMenu.DidClose;
end;

procedure TPlatformMenuDelegate.menuWillOpen(menu: NSMenu);
begin
  FPlatformMenu.WillOpen;
end;

{ TPlatformMenu }

constructor TPlatformMenu.Create;
begin
  inherited Create;
  FDelegate := TPlatformMenuDelegate.Create(Self);
  FMenu := TNSMenu.Create;
  FMenu.setDelegate(FDelegate);
end;

destructor TPlatformMenu.Destroy;
begin
  DestroyItems;
  FMenu.release;
  FMenu := nil;
  inherited;
end;

procedure TPlatformMenu.CreateSeparator;
begin
  AddItem(TPlatformMenuItem.Create('-', FMenu));
end;

function TPlatformMenu.CreateItem(const ATitle: string; const AExecuteHandler: TNotifyEvent = nil): TPlatformMenuItem;
begin
  Result := TPlatformMenuItem.Create(ATitle, FMenu);
  if Assigned(AExecuteHandler) then
    Result.OnExecute := AExecuteHandler;
end;

function TPlatformMenu.CreateItem(const ATitle: string; const AImage: TBitmap; const AExecuteHandler: TNotifyEvent = nil): TPlatformMenuItem;
begin
  Result := CreateItem(ATitle, AExecuteHandler);
  Result.SetImage(AImage);
end;

procedure TPlatformMenu.AddItem(const AItem: TPlatformMenuItem);
begin
  FItems := FItems + [AItem];
end;

function TPlatformMenu.Count: Integer;
begin
  Result := Length(FItems);
end;

procedure TPlatformMenu.DestroyItems;
begin
  while Length(FItems) > 0 do
  begin
    FItems[0].Free;
    Delete(FItems, 0, 1);
  end;
end;

procedure TPlatformMenu.DidClose;
begin
  if Assigned(FOnMenuClose) then
    FOnMenuClose(Self);
end;

procedure TPlatformMenu.WillOpen;
begin
  if Assigned(FOnMenuOpen) then
    FOnMenuOpen(Self);
end;

end.
