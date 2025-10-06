unit DW.ShareItems.iOS;

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
  // Mac
  Macapi.ObjectiveC,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation,
  // FMX
  FMX.Controls,
  // DW
  DW.ShareItems;

type
  // "Fill-in" declarations for what's missing in iOSapi.UIKit
  UIActivityType = NSString;

  UIActivityViewControllerCompletionWithItemsHandler = procedure(activityType: UIActivityType; completed: Boolean; returnedItems: NSArray;
    activityError: NSError) of object;

  UIActivityViewController = interface(iOSapi.UIKit.UIActivityViewController)
    ['{2B343620-0366-4513-8E54-44F68136756A}']
    function completionWithItemsHandler: UIActivityViewControllerCompletionWithItemsHandler; cdecl;
    function excludedActivityTypes: NSArray; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    function initWithNibName(nibNameOrNil: NSString; bundle: NSBundle): Pointer; cdecl;
    procedure setCompletionWithItemsHandler(completionWithItemsHandler: UIActivityViewControllerCompletionWithItemsHandler); cdecl;
    procedure setExcludedActivityTypes(excludedActivityTypes: NSArray); cdecl;
  end;
  TUIActivityViewController = class(TOCGenericImport<UIActivityViewControllerClass, UIActivityViewController>) end;

  TPlatformShareItems = class(TCustomPlatformShareItems)
  private
    FActivityViewController: UIActivityViewController;
    FActivities: array[TShareActivity] of NSString;
    FPopoverController: UIPopoverController;
    procedure ActivityViewControllerCompletionWithItemsHandler(activityType: UIActivityType; completed: Boolean; returnedItems: NSArray;
      activityError: NSError);
    function GetExcludedActivityTypes(const AExcludedActivities: TShareActivities): NSArray;
    function GetShareActivity(const AActivityType: NSString): TShareActivity;
    procedure ReleaseControllers;
    procedure SetupActivities;
    procedure ShowForIPad(const AControl: TControl; const AWindow: UIWindow);
  protected
    procedure Share(const AControl: TControl; const AExcludedActivities: TShareActivities); override;
  public
    constructor Create(const AShareItems: TShareItems); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Types,
  // Mac
  Macapi.Helpers,
  // iOS
  iOSapi.Helpers, iOSapi.CoreGraphics,
  // FMX
  FMX.Helpers.iOS;

function UIActivityTypePostToFacebook: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypePostToFacebook');
end;

function UIActivityTypePostToTwitter: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypePostToTwitter');
end;

function UIActivityTypePostToWeibo: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypePostToWeibo');
end;

function UIActivityTypeMessage: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeMessage');
end;

function UIActivityTypeMail: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeMail');
end;

function UIActivityTypePrint: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypePrint');
end;

function UIActivityTypeCopyToPasteboard: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeCopyToPasteboard');
end;

function UIActivityTypeAssignToContact: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeAssignToContact');
end;

function UIActivityTypeSaveToCameraRoll: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeSaveToCameraRoll');
end;

function UIActivityTypeAddToReadingList: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeAddToReadingList');
end;

function UIActivityTypePostToFlickr: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypePostToFlickr');
end;

function UIActivityTypePostToVimeo: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypePostToVimeo');
end;

function UIActivityTypePostToTencentWeibo: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypePostToTencentWeibo');
end;

function UIActivityTypeAirDrop: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeAirDrop');
end;

function UIActivityTypeOpenInIBooks: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeOpenInIBooks');
end;

function UIActivityTypeMarkupAsPDF: UIActivityType;
begin
  Result := CocoaNSStringConst(libUIKit, 'UIActivityTypeMarkupAsPDF');
end;

{ TPlatformShareItems }

constructor TPlatformShareItems.Create(const AShareItems: TShareItems);
begin
  inherited;
  SetupActivities;
end;

destructor TPlatformShareItems.Destroy;
begin
  ReleaseControllers;
  inherited;
end;

procedure TPlatformShareItems.ReleaseControllers;
begin
  if FPopoverController <> nil then
  begin
    FPopoverController.release;
    FPopoverController := nil;
  end;
  if FActivityViewController <> nil then
  begin
    FActivityViewController.release;
    FActivityViewController := nil;
  end;
end;

procedure TPlatformShareItems.SetupActivities;
begin
  FActivities[TShareActivity.PostToFacebook] := UIActivityTypePostToFacebook;
  FActivities[TShareActivity.PostToTwitter] := UIActivityTypePostToTwitter;
  FActivities[TShareActivity.PostToWeibo] := UIActivityTypePostToWeibo;
  FActivities[TShareActivity.Message] := UIActivityTypeMessage;
  FActivities[TShareActivity.Mail] := UIActivityTypeMail;
  FActivities[TShareActivity.Print] := UIActivityTypePrint;
  FActivities[TShareActivity.CopyToPasteboard] := UIActivityTypeCopyToPasteboard;
  FActivities[TShareActivity.AssignToContact] := UIActivityTypeAssignToContact;
  FActivities[TShareActivity.SaveToCameraRoll] := UIActivityTypeSaveToCameraRoll;
  FActivities[TShareActivity.AddToReadingList] := UIActivityTypeAddToReadingList;
  FActivities[TShareActivity.PostToFlickr] := UIActivityTypePostToFlickr;
  FActivities[TShareActivity.PostToVimeo] := UIActivityTypePostToVimeo;
  FActivities[TShareActivity.PostToTencentWeibo] := UIActivityTypePostToTencentWeibo;
  FActivities[TShareActivity.AirDrop] := UIActivityTypeAirDrop;
  FActivities[TShareActivity.OpenInIBooks] := UIActivityTypeOpenInIBooks;
  FActivities[TShareActivity.MarkupAsPDF] := UIActivityTypeMarkupAsPDF;
end;

function TPlatformShareItems.GetExcludedActivityTypes(const AExcludedActivities: TShareActivities): NSArray;
var
  LActivities: NSMutableArray;
  LActivity: TShareActivity;
begin
  LActivities := TNSMutableArray.Create;
  for LActivity := Low(TShareActivity) to High(TShareActivity) do
  begin
    if LActivity in AExcludedActivities then
      LActivities.addObject(NSObjectToID(FActivities[LActivity]));
  end;
  Result := LActivities;
end;

function TPlatformShareItems.GetShareActivity(const AActivityType: NSString): TShareActivity;
var
  LActivity: TShareActivity;
begin
  for LActivity := Succ(Low(TShareActivity)) to Pred(High(TShareActivity)) do
  begin
    if FActivities[LActivity].isEqualToString(AActivityType) then
      Exit(LActivity)
  end;
  Result := TShareActivity.Unknown;
end;

procedure TPlatformShareItems.Share(const AControl: TControl; const AExcludedActivities: TShareActivities);
var
  LActivityItems: NSMutableArray;
  LItem: TSharingItem;
  LWindow: UIWindow;
begin
  LActivityItems := TNSMutableArray.Create;
  for LItem in Items do
  begin
    if LItem is TSharingItemText then
      LActivityItems.addObject(StringToID(TSharingItemText(LItem).Text))
    else if LItem is TSharingItemFile then
    begin
      {$IF CompilerVersion < 37}
      LActivityItems.addObject(TNSURL.OCClass.fileURLWithPath(StrToNSStr(TSharingItemFile(LItem).Text)))
      {$ELSE}
      LActivityItems.addObject(NSObjectToID(TNSURL.OCClass.fileURLWithPath(StrToNSStr(TSharingItemFile(LItem).Text))))
      {$ENDIF}
    end
    else if LItem is TSharingItemImage then
      LActivityItems.addObject(NSObjectToID(BitmapToUIImage(TSharingItemImage(LItem).Image)))
  end;
  ReleaseControllers;
  FActivityViewController := TUIActivityViewController.alloc;
  FActivityViewController.initWithActivityItems(LActivityItems , nil);
  FActivityViewController.setCompletionWithItemsHandler(ActivityViewControllerCompletionWithItemsHandler);
  FActivityViewController.setExcludedActivityTypes(GetExcludedActivityTypes(AExcludedActivities));
  LWindow := TiOSHelper.SharedApplication.keyWindow;
  if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
  begin
    if IsPad then
      ShowForIPad(AControl, LWindow)
    else
      LWindow.rootViewController.presentModalViewController(FActivityViewController, True);
  end;
end;

procedure TPlatformShareItems.ShowForIPad(const AControl: TControl; const AWindow: UIWindow);
var
  LRect: CGRect;
  LPos: TPointF;
begin
  if AControl <> nil then
  begin
    LPos := AControl.LocalToAbsolute(PointF(0, 0));
    if AControl.Scene <> nil then
      LPos := AControl.Scene.LocalToScreen(LPos);
    LRect := CGRectMake(LPos.X, LPos.Y, AControl.Width, AControl.Height);
  end
  else
    LRect := CGRectMake(0, 0, 0, 0);
  FPopoverController := TUIPopoverController.Alloc;
  FPopoverController.initWithContentViewController(FActivityViewController);
  FPopoverController.presentPopoverFromRect(LRect, AWindow.rootViewController.View, UIPopoverArrowDirectionAny, True);
end;

procedure TPlatformShareItems.ActivityViewControllerCompletionWithItemsHandler(activityType: UIActivityType; completed: Boolean;
  returnedItems: NSArray; activityError: NSError);
var
  LError: string;
begin
  // returnedItems is an NSArray of NSExtensionItem, which describes anything that was changed
  if activityError <> nil then
    LError := NSStrToStr(activityError.localizedDescription)
  else
    LError := '';
  if completed then
    DoShareCompleted(GetShareActivity(activityType), LError)
  else
    DoShareCompleted(TShareActivity.None, LError);
  Items.Clear;
end;

end.
