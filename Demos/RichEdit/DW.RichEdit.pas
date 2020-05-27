unit DW.RichEdit;

{*******************************************************}
{                                                       }
{                    Kastri Free                        }
{                                                       }
{          DelphiWorlds Cross-Platform Library          }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.UITypes, System.Classes, System.SysUtils,
  // FMX
  FMX.Memo, FMX.Types, FMX.Graphics, FMX.Controls.Model, FMX.Controls.Presentation;

const
  PM_RICHEDIT_COLOR_CHANGED = PM_USER + 1;

type
  TLoadFromStreamEvent = procedure(Sender: TObject; const Stream: TStream; const Encoding: TEncoding) of object;

  TCustomRichEditModel = class(TCustomMemoModel)
  private
    FPlainText: Boolean;
    FOnLoadFromStream: TLoadFromStreamEvent;
  protected
    procedure LoadFromStream(const AStream: TStream; const AEncoding: TEncoding);
  public
    property PlainText: Boolean read FPlainText;
    property OnLoadFromStream: TLoadFromStreamEvent read FOnLoadFromStream write FOnLoadFromStream;
  end;

  TCustomRichEdit = class(TCustomMemo)
  private
    FColor: TAlphaColor;
    function GetModel: TCustomRichEditModel; overload;
    procedure SetColor(const Value: TAlphaColor);
  protected
    function DefineModelClass: TDataModelClass; override;
    function DefinePresentationName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const AFileName: string; const AEncoding: TEncoding);
    procedure LoadFromStream(const AStream: TStream; const AEncoding: TEncoding);
    property Color: TAlphaColor read FColor write SetColor;
    property Model: TCustomRichEditModel read GetModel;
  end;

  /// <summary>
  ///   "FMX-ified" RichEdit control
  /// </summary>
  /// <remarks>
  ///   **** FOR WINDOWS ONLY **** (at present) i.e. IT WILL NOT WORK ON ANY OTHER PLATFORM, SO DO NOT EXPECT IT TO
  ///   Also, it does not have all the features of the VCL equivalent
  /// </remarks>
  TRichEdit = class(TCustomRichEdit)
  published
    property AutoHide;
    property AutoSelect default False;
    property Caret;
    property CheckSpelling default False;
    property DataDetectorTypes;
    property DisableMouseWheel;
    property HideSelectionOnExit default True;
    property ImeMode default TImeMode.imDontCare;
    property KeyboardType default TVirtualKeyboardType.Default;
    property Lines;
    property MaxLength default 0;
    property ReadOnly default False;
    property ShowScrollBars default True;
    property ShowSizeGrip;
    property StyledSettings;
    property TextSettings;
    property OnChange;
    property OnChangeTracking;
    property OnValidating;
    property OnValidate;
    { inherited }
    property Align;
    property Anchors;
    property Bounces;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren;
    property ClipParent;
//    property ControlType;
    property Cursor default crIBeam;
    property DisableFocusEffect;
    property DragMode;
    property Enabled;
    property EnabledScroll;
    property EnableDragHighlight;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property StyleLookup;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property ParentShowHint;
    property ShowHint;
    { Events }
    property OnApplyStyleLookup;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property OnEnter;
    property OnExit;
    property OnKeyUp;
    property OnKeyDown;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnViewportPositionChange;
    property OnPresentationNameChoosing;
  end;

implementation

uses
  // DW
  {$IF Defined(MSWINDOWS)}
  DW.RichEdit.Win,
  {$ENDIF}
  // RTL
  System.Types;

{ TCustomRichEditModel }

procedure TCustomRichEditModel.LoadFromStream(const AStream: TStream; const AEncoding: TEncoding);
begin
  if Assigned(FOnLoadFromStream) then
    FOnLoadFromStream(Self, AStream, AEncoding);
end;

{ TCustomRichEdit }

constructor TCustomRichEdit.Create(AOwner: TComponent);
begin
  FColor := TAlphaColors.White;
  inherited;
  ControlType := TControlType.Platform;
end;

function TCustomRichEdit.DefineModelClass: TDataModelClass;
begin
  Result := TCustomRichEditModel;
end;

function TCustomRichEdit.DefinePresentationName: string;
begin
  Result := 'RichEdit-' + GetPresentationSuffix;
end;

function TCustomRichEdit.GetModel: TCustomRichEditModel;
begin
  Result := GetModel<TCustomRichEditModel>;
end;

procedure TCustomRichEdit.LoadFromFile(const AFileName: string; const AEncoding: TEncoding);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(LStream, AEncoding);
  finally
    LStream.Free;
  end;
end;

procedure TCustomRichEdit.LoadFromStream(const AStream: TStream; const AEncoding: TEncoding);
begin
  Model.LoadFromStream(AStream, AEncoding);
end;

procedure TCustomRichEdit.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if HasPresentationProxy then
      PresentationProxy.SendMessage(PM_RICHEDIT_COLOR_CHANGED);
  end;
end;

end.
