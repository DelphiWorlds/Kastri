unit DW.NativeControl.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText,
  // FMX
  FMX.Presentation.Android;

type
  TNativeControl = class;

  TLongClickListener = class(TJavaLocal, JView_OnLongClickListener)
  private
    FNativeControl: TNativeControl;
  public
    { JView_OnLongClickListener }
    function onLongClick(view: JView): Boolean; cdecl;
    function onLongClickUseDefaultHapticFeedback(v: JView): Boolean; cdecl;
  public
    constructor Create(const ANativeControl: TNativeControl);
  end;

  TNativeControl = class(TAndroidNativeView)
  private
    FLongClickListener: JView_OnLongClickListener;
  protected
    procedure DoLongPress; virtual;
  public
    constructor Create; override;
  end;

implementation

{ TLongClickListener }

constructor TLongClickListener.Create(const ANativeControl: TNativeControl);
begin
  inherited Create;
  FNativeControl := ANativeControl;
end;

function TLongClickListener.onLongClick(view: JView): Boolean;
begin
  Result := True;
  FNativeControl.DoLongPress;
end;

function TLongClickListener.onLongClickUseDefaultHapticFeedback(v: JView): Boolean;
begin
  Result := True;
  FNativeControl.DoLongPress;
end;

{ TNativeControl }

constructor TNativeControl.Create;
begin
  inherited;
  FLongClickListener := TLongClickListener.Create(Self);
  View.setOnLongClickListener(FLongClickListener);
end;

procedure TNativeControl.DoLongPress;
begin
  //
end;

end.
