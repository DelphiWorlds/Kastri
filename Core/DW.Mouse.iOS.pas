unit DW.Mouse.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // RTL
  System.TypInfo, System.Types, System.Classes,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.CocoaTypes, iOSapi.UIKit,
  // FMX
  FMX.Types;

const
  UIScrollTypeMaskDiscrete = 1;
  UIScrollTypeMaskContinuous = 2;
  UIScrollTypeMaskAll = 3;

type
  UIContextMenuInteraction = interface;
  UIHoverGestureRecognizer = interface;
  UIPanGestureRecognizer = interface;

  UIContextMenuInteractionAppearance = NSInteger;
  UIScrollTypeMask = NSInteger;

  TUIContextMenuInteractionBlockMethod1 = procedure(visibleMenu: UIMenu) of object;

  UIContextMenuInteractionClass = interface(NSObjectClass)
    ['{379FCD9F-72F6-4163-95FE-5D823F86D844}']
    {class} function new: Pointer; cdecl;
  end;

  UIContextMenuInteraction = interface(NSObject)
    ['{04C4A4E7-8168-4D58-B003-68DD403B7037}']
    function delegate: Pointer; cdecl;
    procedure dismissMenu; cdecl;
    function initWithDelegate(delegate: Pointer): Pointer; cdecl;
    function locationInView(view: UIView): CGPoint; cdecl;
    function menuAppearance: UIContextMenuInteractionAppearance; cdecl;
    procedure updateVisibleMenuWithBlock(block: TUIContextMenuInteractionBlockMethod1); cdecl;
  end;
  TUIContextMenuInteraction = class(TOCGenericImport<UIContextMenuInteractionClass, UIContextMenuInteraction>) end;

  UIHoverGestureRecognizerClass = interface(UIGestureRecognizerClass)
    ['{727E6B16-BD89-4E68-8B1B-1334316E6E3B}']
  end;

  UIHoverGestureRecognizer = interface(UIGestureRecognizer)
    ['{20850220-B7BC-4FFB-B173-A5C8414D0869}']
    function altitudeAngle: CGFloat; cdecl;
    function azimuthAngleInView(view: UIView): CGFloat; cdecl;
    function azimuthUnitVectorInView(view: UIView): CGVector; cdecl;
    function rollAngle: CGFloat; cdecl;
    function zOffset: CGFloat; cdecl;
  end;
  TUIHoverGestureRecognizer = class(TOCGenericImport<UIHoverGestureRecognizerClass, UIHoverGestureRecognizer>) end;

  UIPanGestureRecognizerClass = interface(UIGestureRecognizerClass)
    ['{96BB363B-A2A0-438F-937F-032988DCBBE1}']
  end;

  UIPanGestureRecognizer = interface(UIGestureRecognizer)
    ['{64BE1AC0-3F15-486D-B45C-1BD7F94FFABB}']
    function allowedScrollTypesMask: UIScrollTypeMask; cdecl;
    function maximumNumberOfTouches: NSUInteger; cdecl;
    function minimumNumberOfTouches: NSUInteger; cdecl;
    procedure setAllowedScrollTypesMask(allowedScrollTypesMask: UIScrollTypeMask); cdecl;
    procedure setMaximumNumberOfTouches(maximumNumberOfTouches: NSUInteger); cdecl;
    procedure setMinimumNumberOfTouches(minimumNumberOfTouches: NSUInteger); cdecl;
    procedure setTranslation(translation: CGPoint; inView: UIView); cdecl;
    function translationInView(view: UIView): CGPoint; cdecl;
    function velocityInView(view: UIView): CGPoint; cdecl;
  end;
  TUIPanGestureRecognizer = class(TOCGenericImport<UIPanGestureRecognizerClass, UIPanGestureRecognizer>) end;

  IContextMenuInteractionOwner = interface(IInterface)
    ['{54E82C5A-F192-45D7-A4C2-9368354083BA}']
    procedure MouseRightClick(const APoint: CGPoint);
  end;

  // Shortened version, as only one function is needed
  UIContextMenuInteractionDelegate = interface(IObjectiveC)
    ['{BE4F436F-787E-47C5-BD00-E9EC8045BBF6}']
    function contextMenuInteraction(interaction: UIContextMenuInteraction;
      configurationForMenuAtLocation: CGPoint): UIContextMenuConfiguration; cdecl;
  end;

  TContextMenuInteractionDelegate = class(TOCLocal, UIContextMenuInteractionDelegate)
  private
    FOwner: IContextMenuInteractionOwner;
  public
    { UIContextMenuInteractionDelegate }
    function contextMenuInteraction(interaction: UIContextMenuInteraction;
      configurationForMenuAtLocation: CGPoint): UIContextMenuConfiguration; cdecl;
  public
    constructor Create(AOwner: IContextMenuInteractionOwner);
  end;

  IGestureHandlers = interface(NSObject)
    ['{89C3B35D-96FB-4BCD-A97D-A9D927FFD43E}']
    procedure handleHoverGesture(gestureRecognizer: UIHoverGestureRecognizer); cdecl;
    procedure handleScrollWheelGesture(gestureRecognizer: UIPanGestureRecognizer); cdecl;
  end;

  TMouseRightClickEvent = procedure(Sender: TObject; X, Y: Single) of object;

  TPlatformMouse = class(TOCLocal, IContextMenuInteractionOwner)
  private
    FContextMenuInteraction: Pointer;
    FContextMenuInteractionDelegate: TContextMenuInteractionDelegate;
    FHoverRecognizer: UIHoverGestureRecognizer;
    FPanRecognizer: UIPanGestureRecognizer;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseRightClick: TMouseRightClickEvent;
    FOnMouseWheel: TMouseWheelEvent;
    procedure AddContextMenuInteraction;
    procedure AddHoverRecognizer;
    procedure AddPanRecognizer;
    function GetScale: Single;
    function GetView: UIView;
    procedure MouseMoved(const X, Y: Single);
    procedure ScrollChanged(const X, Y: Single);
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    { IMouseNotifications }
    procedure handleHoverGesture(gestureRecognizer: UIHoverGestureRecognizer); cdecl;
    procedure handleScrollWheelGesture(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    { IContextMenuInteractionOwner }
    procedure MouseRightClick(const APoint: CGPoint);
  public
    constructor Create;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseRightClick: TMouseRightClickEvent read FOnMouseRightClick write FOnMouseRightClick;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers;

type
  UIViewEx = interface(UIView)
    ['{ECAD7470-4B44-4561-AC82-394361132936}']
    procedure addInteraction(interaction: Pointer); cdecl;
  end;
  TUIViewEx = class(TOCGenericImport<UIViewClass, UIViewEx>) end;

function SharedApplication: UIApplication;
begin
  {$IF (CompilerVersion < 37)}
  Result := TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication);
  {$ELSE}
  Result := TUIApplication.OCClass.sharedApplication;
  {$ENDIF}
end;

{ TContextMenuInteractionDelegate }

constructor TContextMenuInteractionDelegate.Create(AOwner: IContextMenuInteractionOwner);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TContextMenuInteractionDelegate.contextMenuInteraction(interaction: UIContextMenuInteraction;
  configurationForMenuAtLocation: CGPoint): UIContextMenuConfiguration;
begin
  FOwner.MouseRightClick(configurationForMenuAtLocation);
  Result := nil;
end;

{ TPlatformMouse }

constructor TPlatformMouse.Create;
begin
  inherited;
  AddContextMenuInteraction;
  AddHoverRecognizer;
  AddPanRecognizer;
end;

procedure TPlatformMouse.AddContextMenuInteraction;
var
  LView: UIView;
begin
  LView := GetView;
  if FContextMenuInteraction = nil then
  begin
    if FContextMenuInteractionDelegate = nil then
      FContextMenuInteractionDelegate := TContextMenuInteractionDelegate.Create(Self);
    FContextMenuInteraction := TUIContextMenuInteraction.Alloc.initWithDelegate(FContextMenuInteractionDelegate.GetObjectID);
    TUIViewEx.Wrap(NSObjectToID(LView)).addInteraction(FContextMenuInteraction);
  end;
end;

procedure TPlatformMouse.AddHoverRecognizer;
var
  LView: UIView;
begin
  LView := GetView;
  if FHoverRecognizer = nil then
  begin
    FHoverRecognizer := TUIHoverGestureRecognizer.Alloc;
    FHoverRecognizer.initWithTarget(GetObjectID, sel_getUid('handleHoverGesture:'));
    LView.addGestureRecognizer(FHoverRecognizer);
  end;
end;

procedure TPlatformMouse.AddPanRecognizer;
var
  LView: UIView;
begin
  LView := GetView;
  if FPanRecognizer = nil then
  begin
    FPanRecognizer := TUIPanGestureRecognizer.Alloc;
    FPanRecognizer.initWithTarget(GetObjectID, sel_getUid('handleScrollWheelGesture:'));
    FPanRecognizer.setAllowedScrollTypesMask(UIScrollTypeMaskDiscrete);
    FPanRecognizer.setMaximumNumberOfTouches(0);
    LView.addGestureRecognizer(FPanRecognizer);
  end;
end;

function TPlatformMouse.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IGestureHandlers);
end;

function TPlatformMouse.GetScale: Single;
begin
  {$IF (CompilerVersion < 37)}
  Result := TUIScreen.Wrap(TUIScreen.OCClass.mainScreen).nativeScale;
  {$ELSE}
  Result := TUIScreen.OCClass.mainScreen.nativeScale;
  {$ENDIF}
end;

function TPlatformMouse.GetView: UIView;
var
  LController: UIViewController;
begin
  Result := nil;
  LController := SharedApplication.keyWindow.rootViewController;
  if LController <> nil then
    Result := LController.view;
end;

procedure TPlatformMouse.MouseMoved(const X, Y: Single);
var
  LScale: Double;
begin
  LScale := GetScale;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, [], X / LScale, Y / LScale);
end;

procedure TPlatformMouse.MouseRightClick(const APoint: CGPoint);
var
  LScale: Double;
begin
  LScale := GetScale;
  if Assigned(FOnMouseRightClick) then
    FOnMouseRightClick(Self, APoint.x / LScale, APoint.y / LScale);
end;

procedure TPlatformMouse.ScrollChanged(const X, Y: Single);
var
  LDelta: Single;
  LShift: TShiftState;
  LHandled: Boolean;
  LYDebug: Single;
begin
  LDelta := 0;
  LShift := [];
  LHandled := False;
  if Y <> 0 then
    LDelta := Y
  else if X <> 0 then
  begin
    LDelta := X;
    LShift := LShift + [ssHorizontal];
  end;
  if (LDelta <> 0) and Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, LShift, Round(LDelta * 2), LHandled);
end;

procedure TPlatformMouse.handleHoverGesture(gestureRecognizer: UIHoverGestureRecognizer);
var
  LView: UIView;
  LPoint: NSPoint;
begin
  if gestureRecognizer.state = UIGestureRecognizerStateChanged then
  begin
    LView := GetView;
    if LView <> nil then
    begin
      LPoint := gestureRecognizer.locationInView(LView);
      MouseMoved(LPoint.X, LPoint.Y);
    end;
  end;
end;

procedure TPlatformMouse.handleScrollWheelGesture(gestureRecognizer: UIPanGestureRecognizer);
var
  LView: UIView;
  LPoint: NSPoint;
begin
  LView := GetView;
  if LView <> nil then
  begin
    LPoint := gestureRecognizer.translationInView(LView);
    ScrollChanged(LPoint.x, LPoint.y);
  end;
end;

end.
