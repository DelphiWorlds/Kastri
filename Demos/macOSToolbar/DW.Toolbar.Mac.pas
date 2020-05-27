unit DW.Toolbar.Mac;

{$SCOPEDENUMS ON}

interface

uses
  System.Classes, System.Generics.Collections,
  Macapi.AppKit, Macapi.Foundation,
  FMX.Forms;

type
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

  TMacOSToolbar = class(TObject)
  private
    FDelegate: NSToolbarDelegate;
    FForm: TCommonCustomForm;
    FItemMap: TMacOSToolbarItemMap;
    FItems: TMacOSToolbarItems;
    FOptions: TMacOSToolbarOptions;
    FSpacerCount: Integer;
    FToolbar: NSToolbar;
    FSizeMode: TMacOSToolbarSizeMode;
    procedure DoAddItem(const AItemMapID: string; AItem: TMacOSToolbarItem);
    function GetItem(const AItemID: string): TMacOSToolbarItem;
    procedure InternalAddSpaceItem(const AItemName: string; const AIsDefault: Boolean);
    procedure SetForm(const Value: TCommonCustomForm);
    procedure SetOptions(const Value: TMacOSToolbarOptions);
    procedure SetSizeMode(const Value: TMacOSToolbarSizeMode);
  protected
    function ItemIdentifiers(const ADefaultOnly: Boolean): NSArray;
  public
    constructor Create(const AToolbarID: string);
    destructor Destroy; override;
    function AddItem(const AItemID: string): TMacOSToolbarItem;
    procedure AddFlexibleSpaceItem(AIsDefault: Boolean = True);
    procedure AddSpaceItem(AIsDefault: Boolean = True);
    // procedure Attach(AForm: TCommonCustomForm);
    property Form: TCommonCustomForm read FForm write SetForm;
    property Items[const AItemID: string]: TMacOSToolbarItem read GetItem; default;
    property Options: TMacOSToolbarOptions read FOptions write SetOptions;
    property SizeMode: TMacOSToolbarSizeMode read FSizeMode write SetSizeMode;
    property Toolbar: NSToolbar read FToolbar;
  end;

implementation

uses
  System.TypInfo, System.SysUtils,
  Macapi.ObjectiveC, Macapi.ObjCRuntime, Macapi.CoreFoundation, Macapi.Helpers,
  FMX.Platform.Mac;

type
  IToolbarDelegate = interface(NSObject)
    ['{648C11BD-D306-4168-9F7C-2C078C5B293F}']
    procedure itemClicked(item: NSToolbarItem); cdecl;
    function toolbarAllowedItemIdentifiers(toolbar: Pointer): NSArray; cdecl;
    function toolbarDefaultItemIdentifiers(toolbar: Pointer): NSArray; cdecl;
    function toolbar(toolbar: Pointer; itemForItemIdentifier: CFStringRef; willBeInsertedIntoToolbar: Boolean): NSToolbarItem; cdecl;
    function validateToolbarItem(theItem: NSToolbarItem): Boolean; cdecl;
  end;

  TNSToolbarDelegate = class(TOCLocal, NSToolbarDelegate)
  private
    FToolbar: TMacOSToolbar;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    { IToolbarDelegate }
    procedure itemClicked(item: NSToolbarItem); cdecl;
    function toolbarAllowedItemIdentifiers(toolbar: Pointer): NSArray; cdecl;
    function toolbarDefaultItemIdentifiers(toolbar: Pointer): NSArray; cdecl;
    function toolbar(toolbar: Pointer; itemForItemIdentifier: CFStringRef; willBeInsertedIntoToolbar: Boolean): NSToolbarItem; cdecl;
    function validateToolbarItem(theItem: NSToolbarItem): Boolean; cdecl;
  public
    constructor Create(const AToolbar: TMacOSToolbar); reintroduce;
  end;

{ TNSToolbarDelegate }

constructor TNSToolbarDelegate.Create(const AToolbar: TMacOSToolbar);
begin
  inherited Create;
  FToolbar := AToolbar;
end;

function TNSToolbarDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IToolbarDelegate);
end;

procedure TNSToolbarDelegate.itemClicked(item: NSToolbarItem);
begin
  FToolbar.Items[NSStrToStr(item.itemIdentifier)].DoClick;
end;

function TNSToolbarDelegate.toolbar(toolbar: Pointer; itemForItemIdentifier: CFStringRef; willBeInsertedIntoToolbar: Boolean): NSToolbarItem;
begin
  Result := FToolbar.Items[CFStringRefToStr(itemForItemIdentifier)].ToolbarItem;
  Result.setTarget(GetObjectID);
  Result.setAction(sel_getUid('itemClicked:'));
end;

function TNSToolbarDelegate.toolbarAllowedItemIdentifiers(toolbar: Pointer): NSArray;
begin
  Result := FToolbar.ItemIdentifiers(False);
end;

function TNSToolbarDelegate.toolbarDefaultItemIdentifiers(toolbar: Pointer): NSArray;
begin
  Result := FToolbar.ItemIdentifiers(True);
end;

function TNSToolbarDelegate.validateToolbarItem(theItem: NSToolbarItem): Boolean;
begin
  Result := FToolbar.Items[NSStrToStr(theItem.itemIdentifier)].DoValidate;
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
  FDelegate := TNSToolbarDelegate.Create(Self);
  FToolbar := TNSToolbar.Wrap(TNSToolbar.OCClass.alloc);
  FToolbar := TNSToolbar.Wrap(FToolbar.initWithIdentifier(StrToNSStr(AToolbarID)));
  FToolbar.setDelegate(FDelegate);
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
  LIdents: TArray<CFStringRef>;
  I: Integer;
  LLength: Integer;
begin
  for LItem in FItems do
  begin
    if not ADefaultOnly or LItem.IsDefault then
      LIdents := LIdents + [CFStringCreateWithCharacters(nil, PChar(LItem.ID), Length(LItem.ID))];
  end;
  LLength := Length(LIdents);
  Result := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LIdents[0], LLength)); // Length(LIdents)));
  for I := 0 to Length(LIdents) do
    CFRelease(LIdents[I]);
end;

procedure TMacOSToolbar.SetForm(const Value: TCommonCustomForm);
begin
  if Value <> FForm then
  begin
    if FForm <> nil then
      WindowHandleToPlatform(FForm.Handle).Wnd.setToolbar(nil);
    FForm := Value;
    if FForm <> nil then
      WindowHandleToPlatform(FForm.Handle).Wnd.setToolbar(FToolbar);
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

end.
