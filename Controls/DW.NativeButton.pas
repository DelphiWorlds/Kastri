unit DW.NativeButton;

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
  System.Classes, System.Types,
  // FMX
  FMX.Controls.Presentation, FMX.Controls.Model, FMX.StdCtrls, FMX.Types;

const
  MM_SET_TEXT = MM_USER + 1;

type
  TCustomNativeButtonModel = class(TDataModel)
  private
    FText: string;
    procedure SetText(const Value: string);
  public
    property Text: string read FText write SetText;
  end;

  TCustomNativeButton = class(TPresentedTextControl)
  private
    function GetModel: TCustomNativeButtonModel; overload;
  protected
    function DefineModelClass: TDataModelClass; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
    procedure SetText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Model: TCustomNativeButtonModel read GetModel;
  end;

  [ComponentPlatformsAttribute(pfidiOS or pidAndroid32Arm or pidAndroid64Arm or pidWin32 or pidWin64)]
  {$IF Defined(IOS) or Defined(ANDROID)}
  TNativeButton = class(TCustomNativeButton)
  {$ELSE}
  TNativeButton = class(TCustomButton)
  {$ENDIF}
  published
    property Action;
    property Align default TAlignLayout.None;
    property Anchors;
    property Enabled;
    property Height;
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
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnResized;
  end;

procedure Register;

implementation

{$IF Defined(IOS)}
uses
  DW.NativeButton.iOS;
{$ENDIF}
{$IF Defined(ANDROID)}
uses
  DW.NativeButton.Android;
{$ENDIF}

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

end.
