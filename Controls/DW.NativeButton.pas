unit DW.NativeButton;

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
  System.Classes, System.Types,
  // FMX
  FMX.Controls.Presentation, FMX.Controls.Model, FMX.StdCtrls, FMX.Types,
  // DW
  DW.NativeControlModel;

const
  MM_SET_TEXT = MM_USER + 1;

type
  TCustomNativeButtonModel = class(TNativeControlModel)
  private
    FText: string;
    procedure SetText(const Value: string);
  public
    property Text: string read FText write SetText;
  end;

  TCustomNativeButton = class(TPresentedTextControl)
  private
    function GetModel: TCustomNativeButtonModel; overload;
    function GetOnLongPress: TNotifyEvent;
    procedure SetOnLongPress(const Value: TNotifyEvent);
  protected
    function DefineModelClass: TDataModelClass; override;
    procedure Paint; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
    procedure SetText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Model: TCustomNativeButtonModel read GetModel;
    property OnLongPress: TNotifyEvent read GetOnLongPress write SetOnLongPress;
  end;

  TCustomButtonEx = class(FMX.StdCtrls.TCustomButton)
  private
    FOnLongPress: TNotifyEvent;
  public
    property OnLongPress: TNotifyEvent read FOnLongPress write FOnLongPress;
  end;

  {$IF CompilerVersion < 35}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroid32Arm or pidAndroid64Arm or pidWin32 or pidWin64)]
  {$ELSE}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroidArm32 or pidAndroidArm64 or pidWin32 or pidWin64)]
  {$ENDIF}
  {$IF Defined(IOS) or Defined(ANDROID)}
  TNativeButton = class(TCustomNativeButton)
  {$ELSE}
  TNativeButton = class(TCustomButtonEx)
  {$ENDIF}
  published
    property Action;
    property Align default TAlignLayout.None;
    property Anchors;
    property Enabled;
    property Height;
    property HitTest;
    property Margins;
    property Padding;
    property Position;
    property Size;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible default True;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnLongPress;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnResized;
  end;

procedure Register;

implementation

uses
{$IF Defined(IOS)}
  DW.NativeButton.iOS,
{$ENDIF}
{$IF Defined(ANDROID)}
  DW.NativeButton.Android,
{$ENDIF}
  FMX.Controls, FMX.Graphics;

procedure Register;
begin
  RegisterComponents('Kastri FMX', [TNativeButton]);
end;

{ TCustomNativeButtonModel }

procedure TCustomNativeButtonModel.SetText(const Value: string);
begin
  FText := Value;
  SendMessage<string>(MM_SET_TEXT, FText);
end;

{ TCustomNativeButton }

constructor TCustomNativeButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
end;

function TCustomNativeButton.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeButtonModel;
end;

function TCustomNativeButton.GetModel: TCustomNativeButtonModel;
begin
  Result := inherited GetModel<TCustomNativeButtonModel>;
end;

function TCustomNativeButton.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

procedure TCustomNativeButton.SetText(const Value: string);
begin
  inherited;
  Model.Text := Value;
end;

function TCustomNativeButton.GetOnLongPress: TNotifyEvent;
begin
  Result := Model.OnLongPress;
end;

procedure TCustomNativeButton.Paint;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    Canvas.Font.Assign(TextSettings.Font);
    Canvas.Fill.Color := TextSettings.FontColor;
    Canvas.FillText(TRectF.Create(0, 0, Width, Height), Text, TextSettings.WordWrap, 1, [], TextSettings.HorzAlign, TextSettings.VertAlign);
  end;
end;

procedure TCustomNativeButton.SetOnLongPress(const Value: TNotifyEvent);
begin
  Model.OnLongPress := Value;
end;

end.
