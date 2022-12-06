unit DW.StatusBarMenu.Mac;

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
  // RTL
  System.TypInfo, System.Classes, System.Generics.Collections,
  // macOS
  Macapi.ObjectiveC, Macapi.Foundation, Macapi.AppKit,
  // FMX
  FMX.Menus;

type
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

  TMacOSMenuItems = class(TList<TMacOSMenuItem>)
  protected
    function IndexOfFMXItem(const AComponent: TComponent): Integer;
  end;

  TStatusBarMenu = class(TComponent)
  private
    FMenu: NSMenu;
    FMenuItems: TMacOSMenuItems;
    FStatusItem: NSStatusItem;
    FPopupMenu: TPopupMenu;
    FUpdateCount: Integer;
    procedure AddMenuItem(const AItem: TMenuItem);
    procedure AddSubMenuItem(const AMacOSMenuItem: TMacOSMenuItem; const AItem: TMenuItem);
    procedure ClearMenuItems;
    procedure RemoveStatusItem;
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure UpdateMenuItem(const AItem: TMenuItem);
  protected
    class function StatusBar: NSStatusBar;
  protected
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
  end;

implementation

uses
  // RTL
  System.Types, System.SysUtils,
  // macOS
  Macapi.Helpers, Macapi.ObjCRuntime, Macapi.CocoaTypes,
  // FMX
  FMX.Helpers.Mac, FMX.Graphics;

type
  TPopupMenuHelper = class helper for TPopupMenu
  public
    function VisibleItemCount: Integer;
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

{ TStatusBarMenu }

constructor TStatusBarMenu.Create(AOwner: TComponent);
begin
  inherited;
  FMenuItems := TMacOSMenuItems.Create;
  FMenu := TNSMenu.Create;
  FStatusItem := StatusBar.statusItemWithLength(NSVariableStatusItemLength);
  FStatusItem.setMenu(FMenu);
  FStatusItem.setHighlightMode(True);
end;

destructor TStatusBarMenu.Destroy;
begin
  ClearMenuItems;
  RemoveStatusItem;
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

procedure TStatusBarMenu.RemoveStatusItem;
begin
  if FStatusItem <> nil then
  begin
    FStatusItem.setMenu(nil);
    StatusBar.removeStatusItem(FStatusItem);
    FStatusItem.release;
  end;
  FStatusItem := nil;
end;

procedure TStatusBarMenu.RecreateMenu;
var
  I: Integer;
begin
  ClearMenuItems;
  if (FPopupMenu <> nil) and (FPopupMenu.VisibleItemCount > 0) then
  begin
    UpdateNSStatusItemImage(FStatusItem, FPopupMenu.Items[0]);
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
    UpdateNSStatusItemImage(FStatusItem, FPopupMenu.Items[0]);
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
