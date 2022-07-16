unit DW.NativeControl.iOS;

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
  // iOS
  iOSapi.UIKit,
  // FMX
  FMX.Presentation.iOS;

type
  TNativeControl = class(TiOSNativeControl)
  private
    FLongPressRecognizer: UILongPressGestureRecognizer;
  protected
    procedure DoLongPress; virtual;
  public
    function canBecomeFirstResponder: Boolean; cdecl;
    procedure HandleLongPress(gestureRecognizer: UILongPressGestureRecognizer); cdecl;
  public
    constructor Create; override;
  end;

implementation

uses
  // macOS
  Macapi.ObjCRuntime;

{ TNativeControl }

constructor TNativeControl.Create;
begin
  inherited;
  FLongPressRecognizer := TUILongPressGestureRecognizer.Alloc;
  FLongPressRecognizer.initWithTarget(GetObjectID, sel_getUid('HandleLongPress:'));
  FLongPressRecognizer.setDelaysTouchesBegan(False);
  FLongPressRecognizer.setCancelsTouchesInView(True);
  View.addGestureRecognizer(FLongPressRecognizer);
end;

function TNativeControl.canBecomeFirstResponder: Boolean;
begin
  Result := UIView(super).canBecomeFirstResponder and Control.CanFocus and Control.HitTest;
end;

procedure TNativeControl.HandleLongPress(gestureRecognizer: UILongPressGestureRecognizer);
begin
  DoLongPress;
end;

procedure TNativeControl.DoLongPress;
begin
  //
end;

end.
