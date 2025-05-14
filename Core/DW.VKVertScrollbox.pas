unit DW.VKVertScrollbox;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

// For an example of how to use this unit, please refer to the demo, here:
//   https://github.com/DelphiWorlds/KastriFree/tree/master/Demos/VKVertScrollbox

interface

uses
  // RTL
  System.Types, System.Classes, System.Messaging,
  // FMX
  FMX.Layouts, FMX.Controls;

type
  TVertScrollBox = class(FMX.Layouts.TVertScrollBox)
  private
    FCaretPos: TPointF;
    FControlsLayout: TControl;
    FFocusedControl: TControl;
    FRestoreFocusControl: TControl;
    FNeedsIsElasticUpdate: Boolean;
    FRestoreViewport: Boolean;
    FStoredHeight: Single;
    FStoredIsElastic: Boolean;
    FVerticalOffset: Single;
    FViewportOffset: Single;
    FVKRect: TRect;
    function IsFocusedObject: Boolean;
    function HasCaretPosChanged: Boolean;
    procedure IdleMessageHandler(const Sender: TObject; const M: TMessage);
    procedure MoveControls;
    procedure OrientationChangedMessageHandler(const Sender: TObject; const M: TMessage);
    procedure Restore(const AIgnoreRestoreViewport: Boolean);
    procedure RestoreControls;
    procedure SetControlsLayout(const Value: TControl);
    procedure VKStateChangeMessageHandler(const Sender: TObject; const M: TMessage);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   Call this method when a condition may require control positions to be updated
    /// </summary>
    procedure ControlsChanged;
    /// <summary>
    ///   The layout which will resize in order for the scrollbox to be scrolled
    /// </summary>
    property ControlsLayout: TControl read FControlsLayout write SetControlsLayout;
    /// <summary>
    ///   Determines whether the viewport is restored when the VK disappears
    /// </summary>
    /// <remarks>
    ///   If the user has "scrolled" the view to a certain position before setting focus on a control,
    ///   When the VK is dismissed, the viewport will be set back to its topmost position.
    ///   You will likely never need to set this to True
    /// </remarks>
    property RestoreViewport: Boolean read FRestoreViewport write FRestoreViewport;
    /// <summary>
    ///   The amount of extra vertical space allowed for the control to be clear of the virtual keyboard
    /// </summary>
    property VerticalOffset: Single read FVerticalOffset write FVerticalOffset;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // iOS
{$IF Defined(IOS)}
  iOSapi.Helpers,
{$ENDIF}
  // FMX
  FMX.Forms, FMX.Types, FMX.Edit, FMX.Memo,
  // DW
  DW.VirtualKeyboard.Helpers, DW.ElasticLayout;

const
  cVerticalOffsetDefault = 2;

{ TVertScrollBox }

constructor TVertScrollBox.Create(AOwner: TComponent);
begin
  inherited;
  FVerticalOffset := cVerticalOffsetDefault;
  FNeedsIsElasticUpdate := True;
  FVKRect := TRect.Empty;
  TMessageManager.DefaultManager.SubscribeToMessage(TOrientationChangedMessage, OrientationChangedMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, VKStateChangeMessageHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage, IdleMessageHandler);
end;

destructor TVertScrollBox.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TOrientationChangedMessage, OrientationChangedMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, VKStateChangeMessageHandler);
  TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, IdleMessageHandler);
  inherited;
end;

procedure TVertScrollBox.ControlsChanged;
begin
  if not FVKRect.IsEmpty then
    MoveControls;
end;

procedure TVertScrollBox.SetControlsLayout(const Value: TControl);
begin
  if Value = FControlsLayout then
    Exit; // <======
  if FControlsLayout <> nil then
    FControlsLayout.RemoveFreeNotification(Self);
  FControlsLayout := Value;
  if FControlsLayout <> nil then
    FControlsLayout.FreeNotification(Self);
  FNeedsIsElasticUpdate := True;
end;

procedure TVertScrollBox.VKStateChangeMessageHandler(const Sender: TObject; const M: TMessage);
begin
  if FControlsLayout <> nil then
  begin
    if TVKStateChangeMessage(M).KeyboardVisible then
    begin
      if FVKRect <> TVKStateChangeMessage(M).KeyboardBounds then
      begin
        FVKRect := TVKStateChangeMessage(M).KeyboardBounds;
        MoveControls;
      end;
    end
    else
      RestoreControls;
  end;
end;

function TVertScrollBox.HasCaretPosChanged: Boolean;
var
  LMemo: TCustomMemo;
begin
  Result := False;
  if IsFocusedObject and (FFocusedControl is TCustomMemo) then
  begin
    LMemo := TCustomMemo(FFocusedControl);
    if Trunc(LMemo.Caret.Pos.Y) <> Trunc(FCaretPos.Y) then
    begin
      FCaretPos.Y := Trunc(LMemo.Caret.Pos.Y);
      Result := True;
    end;
  end
  else
    FCaretPos := TPointF.Zero;
end;

procedure TVertScrollBox.IdleMessageHandler(const Sender: TObject; const M: TMessage);
begin
  // Check if the focused control has changed, or a caret position has changed. This may happen without the VK hiding/showing
  if TVirtualKeyboard.IsVisible and (HasCaretPosChanged or not IsFocusedObject) then
    MoveControls;
  // Restore focused control after an orientation change
  if FRestoreFocusControl <> nil then
  begin
    Root.Focused := FRestoreFocusControl;
    TVirtualKeyboard.Show(FRestoreFocusControl);
    FRestoreFocusControl := nil;
  end;
  // Change viewport after VK change
  if FViewportOffset > 0 then
  begin
    ViewportPosition := PointF(0, FViewportOffset);
    FViewportOffset := 0;
  end;
end;

function TVertScrollBox.IsFocusedObject: Boolean;
begin
  Result := (Root <> nil) and (Root.Focused <> nil) and (Root.Focused.GetObject = FFocusedControl);
end;

procedure TVertScrollBox.MoveControls;
var
  LOffset, LStatusBarHeight: Single;
  LControlBottom: Single;
  LControlPosition: TPointF;
  LMemo: TCustomMemo;
begin
  FFocusedControl := nil;
  if (FControlsLayout = nil) or (Root = nil) or (Root.Focused = nil) or not (Root.Focused.GetObject is TControl) then
    Exit; // <======
  LStatusBarHeight := 0;
  if Root.GetObject is TCommonCustomForm then
    LStatusBarHeight := Screen.Height - TCommonCustomForm(Root.GetObject).Height;
  if FStoredHeight = 0 then
    FStoredHeight := FControlsLayout.Height;
  if FNeedsIsElasticUpdate and (FControlsLayout is DW.ElasticLayout.TFlowLayout) then
  begin
    FStoredIsElastic := DW.ElasticLayout.TFlowLayout(FControlsLayout).IsElastic;
    FNeedsIsElasticUpdate := False;
    DW.ElasticLayout.TFlowLayout(FControlsLayout).IsElastic := False;
  end;
  FControlsLayout.Height := Height + FVKRect.Height + ViewportPosition.Y;
  FFocusedControl := TControl(Root.Focused.GetObject);
  // Find control position relative to the layout
  LControlPosition := FFocusedControl.LocalToAbsolute(PointF(0,0));
  LControlPosition.Y := LControlPosition.Y + ViewportPosition.Y;
  // Find the "bottom" of the control
  LControlBottom := 0;
  // For TCustomMemo controls, get the caret position
  if FFocusedControl is TCustomEdit then
    LControlBottom := LControlPosition.Y + FFocusedControl.AbsoluteHeight
  else if FFocusedControl is TCustomMemo then
  begin
    LMemo := TCustomMemo(FFocusedControl);
    LControlBottom := LControlPosition.Y + (LMemo.Caret.Pos.Y - LMemo.ViewportPosition.Y) + (LMemo.Caret.size.Height * 2) + 6;
  end;
  // FVerticalOffset gives a bit more clearance between the control "bottom" and the VK
  LOffset := (LControlBottom + FVerticalOffset + LStatusBarHeight - FVKRect.Top) / Scale.Y;
  if Trunc(LOffset) > 0 then
    FViewportOffset := LOffset
  else
    FViewportOffset := 0;
  // Viewport will be updated in IdleMessageHandler
end;

procedure TVertScrollBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = TOperation.opRemove then
  begin
    if AComponent = FControlsLayout then
      FControlsLayout := nil;
    if AComponent = FFocusedControl then
      FFocusedControl := nil;
  end;
end;

procedure TVertScrollBox.OrientationChangedMessageHandler(const Sender: TObject; const M: TMessage);
begin
  FRestoreFocusControl := nil;
  if not TVirtualKeyboard.GetBounds.IsEmpty then
  begin
    FVKRect := TVirtualKeyboard.GetBounds;
    MoveControls;
  end
  else
  begin
    // Retain focus (for iOS)
    if (Root <> nil) and (Root.Focused = nil) and (FFocusedControl <> nil) then
    begin
      Restore(True);
      FRestoreFocusControl := FFocusedControl;
      // Focus will be restored in IdleMessageHandler
    end;
  end;
end;

procedure TVertScrollBox.RestoreControls;
begin
  FVKRect := TRect.Empty;
  if FControlsLayout <> nil then
    Restore(False);
end;

procedure TVertScrollBox.Restore(const AIgnoreRestoreViewport: Boolean);
begin
  if FStoredHeight > 0 then
    FControlsLayout.Height := FStoredHeight;
  FStoredHeight := 0;
  if FRestoreViewport or AIgnoreRestoreViewport then
    ViewportPosition := PointF(0, 0);
  if FControlsLayout is DW.ElasticLayout.TFlowLayout then
  begin
    DW.ElasticLayout.TFlowLayout(FControlsLayout).IsElastic := FStoredIsElastic;
    FNeedsIsElasticUpdate := True;
  end;
end;

end.
