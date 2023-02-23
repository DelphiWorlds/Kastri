unit DW.Toolbar.Mac;

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
  // RTL
  System.TypInfo, System.Classes, System.Generics.Collections, System.Types,
  // macOS
  Macapi.AppKit, Macapi.Foundation, Macapi.ObjectiveC, Macapi.CocoaTypes,
  // FMX
  FMX.Forms;

type
  NSToolbarDisplayMode = NSInteger;
  NSToolbarSizeMode = NSInteger;
  NSToolbarIdentifier = NSString;
  NSToolbarItemIdentifier = NSString;
  NSWindowToolbarStyle = NSInteger;

  TItemValidateEvent = procedure(Sender: TObject; var Enable: Boolean) of object;

  TMacOSToolbarItem = class(TObject)
  private
    FCaption: string;
    FHint: string;
    FImageName: string;
    FID: string;
    FIsDefault: Boolean;
    FToolbarItem: NSToolbarItem;
    FOnClick: TNotifyEvent;
    FOnValidate: TItemValidateEvent;
    procedure SetCaption(const Value: string);
    procedure SetHint(const Value: string);
    procedure SetImageName(const Value: string);
  protected
    procedure DoClick;
    function DoValidate: Boolean;
    property ToolbarItem: NSToolbarItem read FToolbarItem;
  public
    constructor Create(const AID: string);
    destructor Destroy; override;
    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write SetHint;
    property ID: string read FID;
    property ImageName: string read FImageName write SetImageName;
    property IsDefault: Boolean read FIsDefault write FIsDefault;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnValidate: TItemValidateEvent read FOnValidate write FOnValidate;
  end;

  TMacOSToolbarItems = TObjectList<TMacOSToolbarItem>;

  TMacOSToolbarItemMap = TDictionary<string, TMacOSToolbarItem>;

  TMacOSToolbarSizeMode = (Default, Regular, Small);

  TMacOSToolbarOption = (AllowUserCustomization, AutosavesConfiguration);
  TMacOSToolbarOptions = set of TMacOSToolbarOption;

  TMacOSToolbar = class;

  NSToolbarClass = interface(NSObjectClass)
    ['{3F24E2B5-712C-4F81-8F5C-A5989E8871DC}']
  end;

  NSToolbar = interface(NSObject)
    ['{C6C070F9-170F-45C9-9804-78A4D0F9340C}']
    function allowsExtensionItems: Boolean; cdecl;
    function allowsUserCustomization: Boolean; cdecl;
    function autosavesConfiguration: Boolean; cdecl;
    function centeredItemIdentifier: NSToolbarItemIdentifier; cdecl;
    function configurationDictionary: NSDictionary; cdecl;
    function customizationPaletteIsRunning: Boolean; cdecl;
    function delegate: Pointer; cdecl;
    function displayMode: NSToolbarDisplayMode; cdecl;
    function fullScreenAccessoryView: NSView; cdecl;
    function fullScreenAccessoryViewMaxHeight: CGFloat; cdecl;
    function fullScreenAccessoryViewMinHeight: CGFloat; cdecl;
    function identifier: NSToolbarIdentifier; cdecl;
    function initWithIdentifier(identifier: NSToolbarIdentifier): Pointer; cdecl;
    procedure insertItemWithItemIdentifier(itemIdentifier: NSToolbarItemIdentifier; atIndex: NSInteger); cdecl;
    function isVisible: Boolean; cdecl;
    function items: NSArray; cdecl;
    procedure removeItemAtIndex(index: NSInteger); cdecl;
    procedure runCustomizationPalette(sender: Pointer); cdecl;
    function selectedItemIdentifier: NSToolbarItemIdentifier; cdecl;
    procedure setAllowsExtensionItems(allowsExtensionItems: Boolean); cdecl;
    procedure setAllowsUserCustomization(allowsUserCustomization: Boolean); cdecl;
    procedure setAutosavesConfiguration(autosavesConfiguration: Boolean); cdecl;
    procedure setCenteredItemIdentifier(centeredItemIdentifier: NSToolbarItemIdentifier); cdecl;
    procedure setConfigurationFromDictionary(configDict: NSDictionary); cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure setDisplayMode(displayMode: NSToolbarDisplayMode); cdecl;
    procedure setFullScreenAccessoryView(fullScreenAccessoryView: NSView); cdecl;
    procedure setFullScreenAccessoryViewMaxHeight(fullScreenAccessoryViewMaxHeight: CGFloat); cdecl;
    procedure setFullScreenAccessoryViewMinHeight(fullScreenAccessoryViewMinHeight: CGFloat); cdecl;
    procedure setSelectedItemIdentifier(selectedItemIdentifier: NSToolbarItemIdentifier); cdecl;
    procedure setShowsBaselineSeparator(showsBaselineSeparator: Boolean); cdecl;
    procedure setSizeMode(sizeMode: NSToolbarSizeMode); cdecl;
    procedure setVisible(visible: Boolean); cdecl;
    function showsBaselineSeparator: Boolean; cdecl;
    function sizeMode: NSToolbarSizeMode; cdecl;
    procedure validateVisibleItems; cdecl;
    function visibleItems: NSArray; cdecl;
  end;
  TNSToolbar = class(TOCGenericImport<NSToolbarClass, NSToolbar>) end;

  // Declaration here just for reference
  NSToolbarDelegate = interface(IObjectiveC)
    ['{02D35995-BDF8-4BEA-889A-40C0D079F6BB}']
    function toolbar(toolbar: NSToolbar; itemForItemIdentifier: NSToolbarItemIdentifier; willBeInsertedIntoToolbar: Boolean): NSToolbarItem; cdecl;
    function toolbarAllowedItemIdentifiers(toolbar: NSToolbar): NSArray; cdecl;
    function toolbarDefaultItemIdentifiers(toolbar: NSToolbar): NSArray; cdecl;
    procedure toolbarDidRemoveItem(notification: NSNotification); cdecl;
    function toolbarSelectableItemIdentifiers(toolbar: NSToolbar): NSArray; cdecl;
    procedure toolbarWillAddItem(notification: NSNotification); cdecl;
  end;

  IToolbarDelegate = interface(IObjectiveC)
    ['{648C11BD-D306-4168-9F7C-2C078C5B293F}']
    { IToolbarDelegate }
    procedure itemClicked(item: NSToolbarItem); cdecl;
    { NSToolbarDelegate }
    function toolbar(toolbar: NSToolbar; itemForItemIdentifier: NSToolbarItemIdentifier; willBeInsertedIntoToolbar: Boolean): NSToolbarItem; cdecl;
    function toolbarAllowedItemIdentifiers(toolbar: NSToolbar): NSArray; cdecl;
    function toolbarDefaultItemIdentifiers(toolbar: NSToolbar): NSArray; cdecl;
  end;

  TToolbarDelegate = class(TOCLocal, IToolbarDelegate)
  private
    FToolbar: TMacOSToolbar;
  public
    { IToolbarDelegate }
    procedure itemClicked(item: NSToolbarItem); cdecl;
    { NSToolbarDelegate }
    function toolbar(toolbar: NSToolbar; itemForItemIdentifier: NSToolbarItemIdentifier; willBeInsertedIntoToolbar: Boolean): NSToolbarItem; cdecl;
    function toolbarAllowedItemIdentifiers(toolbar: NSToolbar): NSArray; cdecl;
    function toolbarDefaultItemIdentifiers(toolbar: NSToolbar): NSArray; cdecl;
  public
    constructor Create(const AToolbar: TMacOSToolbar); reintroduce;
  end;

  TMacOSToolbar = class(TObject)
  private
    FDelegate: TToolbarDelegate;
    FForm: TCommonCustomForm;
    FHideSystemButtons: Boolean;
    FItemMap: TMacOSToolbarItemMap;
    FItems: TMacOSToolbarItems;
    FOptions: TMacOSToolbarOptions;
    FSpacerCount: Integer;
    FToolbar: NSToolbar;
    FSizeMode: TMacOSToolbarSizeMode;
    procedure DoAddItem(const AItemMapID: string; AItem: TMacOSToolbarItem);
    function GetItem(const AItemID: string): TMacOSToolbarItem;
    procedure InternalAddSpaceItem(const AItemName: string; const AIsDefault: Boolean);
    procedure PropertiesChanged;
    procedure SetForm(const Value: TCommonCustomForm);
    procedure SetOptions(const Value: TMacOSToolbarOptions);
    procedure SetSizeMode(const Value: TMacOSToolbarSizeMode);
    procedure SetHideSystemButtons(const Value: Boolean);
  protected
    function ItemIdentifiers(const ADefaultOnly: Boolean): NSArray;
  public
    constructor Create(const AToolbarID: string);
    destructor Destroy; override;
    function AddItem(const AItemID: string): TMacOSToolbarItem;
    procedure AddFlexibleSpaceItem(AIsDefault: Boolean = True);
    procedure AddSpaceItem(AIsDefault: Boolean = True);
    function GetPopupPoint: TPointF;
    property Form: TCommonCustomForm read FForm write SetForm;
    property HideSystemButtons: Boolean read FHideSystemButtons write SetHideSystemButtons;
    property Items[const AItemID: string]: TMacOSToolbarItem read GetItem; default;
    property Options: TMacOSToolbarOptions read FOptions write SetOptions;
    property SizeMode: TMacOSToolbarSizeMode read FSizeMode write SetSizeMode;
    property Toolbar: NSToolbar read FToolbar;
  end;

implementation

uses
  System.SysUtils,
  Macapi.ObjCRuntime, Macapi.CoreFoundation, Macapi.Helpers,
  FMX.Platform.Mac, FMX.Helpers.Mac,
  DW.Macapi.Helpers;

const
  NSWindowTitleVisible = 0;
  NSWindowTitleHidden = 1;

  NSWindowCloseButton = 0;
  NSWindowMiniaturizeButton = 1;
  NSWindowZoomButton = 2;
  NSWindowToolbarButton = 3;
  NSWindowDocumentIconButton = 4;
  NSWindowDocumentVersionsButton = 6;

type
  NSWindowTitleVisibility = NSInteger;

  NSWindowClass = interface(NSResponderClass)
    ['{6E25AAEE-B1B7-4185-85FA-AA6FEBF87294}']
  end;

  NSWindow = interface(NSResponder)
    ['{110F8D27-1D2C-480F-84C4-0FAD931FA44E}']
    procedure setTitleVisibility(titleVisibility: NSWindowTitleVisibility); cdecl;
    procedure setToolbar(toolbar: NSToolbar); cdecl;
    procedure setToolbarStyle(toolbarStyle: NSWindowToolbarStyle); cdecl;
    function standardWindowButton(b: NSWindowButton): NSButton; cdecl;
  end;
  TNSWindow = class(TOCGenericImport<NSWindowClass, NSWindow>) end;

// Wrap the NSWindow reference so the one declared here can be used
function GetWindow(const AForm: TCommonCustomForm): NSWindow;
begin
  Result := TNSWindow.Wrap(NSObjectToID(WindowHandleToPlatform(AForm.Handle).Wnd));
end;

{ TToolbarDelegate }

constructor TToolbarDelegate.Create(const AToolbar: TMacOSToolbar);
begin
  inherited Create;
  FToolbar := AToolbar;
end;

procedure TToolbarDelegate.itemClicked(item: NSToolbarItem);
begin
  FToolbar.Items[NSStrToStr(item.itemIdentifier)].DoClick;
end;

function TToolbarDelegate.toolbar(toolbar: NSToolbar; itemForItemIdentifier: NSToolbarItemIdentifier; willBeInsertedIntoToolbar: Boolean): NSToolbarItem;
begin
  Result := FToolbar.Items[NSStrToStr(itemForItemIdentifier)].ToolbarItem;
  Result.setTarget(GetObjectID);
  Result.setAction(sel_getUid('itemClicked:'));
end;

function TToolbarDelegate.toolbarAllowedItemIdentifiers(toolbar: NSToolbar): NSArray;
begin
  Result := FToolbar.ItemIdentifiers(False);
end;

function TToolbarDelegate.toolbarDefaultItemIdentifiers(toolbar: NSToolbar): NSArray;
begin
  Result := FToolbar.ItemIdentifiers(True);
end;

{ TMacOSToolbarItem }

constructor TMacOSToolbarItem.Create(const AID: string);
begin
  inherited Create;
  FID := AID;
  FToolbarItem := TNSToolbarItem.Wrap(TNSToolbarItem.OCClass.alloc);
  FToolbarItem := TNSToolbarItem.Wrap(FToolbarItem.initWithItemIdentifier(StrToNSStr(FID)));
  FIsDefault := True;
end;

destructor TMacOSToolbarItem.Destroy;
begin
  FToolbarItem := nil;
  inherited;
end;

procedure TMacOSToolbarItem.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TMacOSToolbarItem.DoValidate: Boolean;
begin
  Result := True;
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

procedure TMacOSToolbarItem.SetCaption(const Value: string);
begin
  FCaption := Value;
  FToolbarItem.setLabel(StrToNSStr(FCaption));
  FToolbarItem.setPaletteLabel(StrToNSStr(FCaption));
end;

procedure TMacOSToolbarItem.SetHint(const Value: string);
begin
  FHint := Value;
  FToolbarItem.setTooltip(StrToNSStr(FHint));
end;

procedure TMacOSToolbarItem.SetImageName(const Value: string);
begin
  FImageName := Value;
  FToolbarItem.setImage(TNSImage.Wrap(TNSImage.OCClass.imageNamed(StrToNSStr(FImageName))));
end;

{ TMacOSToolbar }

constructor TMacOSToolbar.Create(const AToolbarID: string);
begin
  inherited Create;
  FItemMap := TMacOSToolbarItemMap.Create;
  FItems := TMacOSToolbarItems.Create;
  FDelegate := TToolbarDelegate.Create(Self);
  FToolbar := TNSToolbar.Wrap(TNSToolbar.OCClass.alloc);
  FToolbar := TNSToolbar.Wrap(FToolbar.initWithIdentifier(StrToNSStr(AToolbarID)));
  FToolbar.setDelegate(FDelegate.GetObjectID);
  SetOptions([TMacOSToolbarOption.AllowUserCustomization, TMacOSToolbarOption.AutosavesConfiguration]);
  SetSizeMode(TMacOSToolbarSizeMode.Small);
end;

destructor TMacOSToolbar.Destroy;
begin
  FItemMap.Free;
  FItems.Free;
  FToolbar := nil;
  inherited;
end;

function TMacOSToolbar.ItemIdentifiers(const ADefaultOnly: Boolean): NSArray;
var
  LItem: TMacOSToolbarItem;
  LIdents: TArray<string>;
begin
  LIdents := [];
  for LItem in FItems do
  begin
    if not ADefaultOnly or LItem.IsDefault then
      LIdents := LIdents + [LItem.ID];
  end;
  Result := StringArrayToNSArray(LIdents);
end;

procedure TMacOSToolbar.SetForm(const Value: TCommonCustomForm);
var
  LWindow: NSWindow;
begin
  if Value <> FForm then
  begin
    if FForm <> nil then
    begin
      LWindow := GetWindow(FForm);
      LWindow.setToolbar(nil);
      LWindow.setTitleVisibility(NSWindowTitleVisible);
      PropertiesChanged;
    end;
    FForm := Value;
    if FForm <> nil then
    begin
      LWindow := GetWindow(FForm);
      LWindow.setTitleVisibility(NSWindowTitleHidden);
      LWindow.setToolbar(FToolbar);
      PropertiesChanged;
    end;
  end;
end;

procedure TMacOSToolbar.PropertiesChanged;
var
  LWindow: NSWindow;
begin
  if FForm <> nil then
  begin
    LWindow := GetWindow(FForm);
    LWindow.standardWindowButton(NSWindowCloseButton).setHidden(FHideSystemButtons);
    LWindow.standardWindowButton(NSWindowMiniaturizeButton).setHidden(FHideSystemButtons);
    LWindow.standardWindowButton(NSWindowZoomButton).setHidden(FHideSystemButtons);
  end;
end;

procedure TMacOSToolbar.SetHideSystemButtons(const Value: Boolean);
begin
  if Value <> FHideSystemButtons then
  begin
    FHideSystemButtons := Value;
    PropertiesChanged;
  end;
end;

procedure TMacOSToolbar.SetOptions(const Value: TMacOSToolbarOptions);
begin
  FOptions := Value;
  if TMacOSToolbarOption.AllowUserCustomization in FOptions then
    FToolbar.setAllowsUserCustomization(True);
  if TMacOSToolbarOption.AutosavesConfiguration in FOptions then
    FToolbar.setAutosavesConfiguration(True);
end;

procedure TMacOSToolbar.SetSizeMode(const Value: TMacOSToolbarSizeMode);
begin
  FSizeMode := Value;
  FToolbar.setSizeMode(Ord(FSizeMode));
end;

procedure TMacOSToolbar.InternalAddSpaceItem(const AItemName: string; const AIsDefault: Boolean);
var
  LItem: TMacOSToolbarItem;
begin
  Inc(FSpacerCount);
  LItem := TMacOSToolbarItem.Create(AItemName);
  LItem.IsDefault := AIsDefault;
  DoAddItem('_SpaceItem' + IntToStr(FSpacerCount), LItem);
end;

procedure TMacOSToolbar.AddFlexibleSpaceItem(AIsDefault: Boolean = True);
begin
  InternalAddSpaceItem('NSToolbarFlexibleSpaceItem', AIsDefault);
end;

procedure TMacOSToolbar.AddSpaceItem(AIsDefault: Boolean = True);
begin
  InternalAddSpaceItem('NSToolbarSpaceItem', AIsDefault);
end;

function TMacOSToolbar.AddItem(const AItemID: string): TMacOSToolbarItem;
begin
  Result := TMacOSToolbarItem.Create(AItemID);
  DoAddItem(AItemID, Result);
end;

procedure TMacOSToolbar.DoAddItem(const AItemMapID: string; AItem: TMacOSToolbarItem);
begin
  FItems.Add(AItem);
  FItemMap.Add(AItemMapID, AItem);
end;

function TMacOSToolbar.GetItem(const AItemID: string): TMacOSToolbarItem;
begin
  FItemMap.TryGetValue(AItemID, Result);
end;

function TMacOSToolbar.GetPopupPoint: TPointF;
begin
  Result := TNSEvent.OCClass.mouseLocation.ToPointF;
  Result.y := MainScreenHeight - Result.y;
end;

end.
