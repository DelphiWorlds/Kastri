unit DW.NativeImage;

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
  System.Classes, System.Types, System.UITypes,
  // FMX
  FMX.Controls.Presentation, FMX.Controls.Model, FMX.Controls, FMX.Graphics, FMX.Types,
  //
  DW.NativeControlModel;

const
  MM_NATIVEIMAGE_LOADFROMFILE = MM_USER + 1;
  MM_NATIVEIMAGE_LOADFROMRESOURCE = MM_USER + 2;
  MM_NATIVEIMAGE_LOADFROMSTREAM = MM_USER + 3;
  MM_NATIVEIMAGE_LOADFROMNATIVE = MM_USER + 4;
  MM_NATIVEIMAGE_TEXTCHANGED = MM_USER + 5;
  MM_NATIVEIMAGE_TEXTSETTINGSCHANGED = MM_USER + 6;
  MM_NATIVEIMAGE_BACKGROUNDCOLORCHANGED = MM_USER + 7;
  MM_NATIVEIMAGE_IMAGECHANGED = MM_USER + 8;

type
  TCustomNativeImageModel = class(TNativeControlModel)
  private
    FBackgroundColor: TAlphaColor;
    FImage: TBitmap;
    FText: string;
    FTextSettingsInfo: TTextSettingsInfo;
    FOnTextSettingsChanged: TNotifyEvent;
    procedure BitmapChangedHandler(Sender: TObject);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetImage(const Value: TBitmap);
    procedure SetText(const Value: string);
  protected
    procedure Loaded;
    procedure LoadFromBitmap;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromResource(const AResourceName: string);
    procedure LoadFromStream(const AStream: TStream);
    procedure TextSettingsChanged;
    property OnTextSettingsChanged: TNotifyEvent read FOnTextSettingsChanged write FOnTextSettingsChanged;
  public
    constructor Create(const AOwner: TComponent); override;
    destructor Destroy; override;
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor;
    property Image: TBitmap read FImage write SetImage;
    property Text: string read FText write SetText;
    property TextSettingsInfo: TTextSettingsInfo read FTextSettingsInfo;
  end;

  TCustomNativeImage = class(TPresentedControl)
  private
    function GetBackgroundColor: TAlphaColor;
    function GetImage: TBitmap;
    function GetModel: TCustomNativeImageModel; overload;
    function GetOnLongPress: TNotifyEvent;
    function GetStyledSettings: TStyledSettings;
    function GetText: string;
    function GetTextSettings: TTextSettings;
    procedure ModelTextSettingsChangedHandler(Sender: TObject);
    procedure SetBackgroundColor(const Value: TAlphaColor);
    procedure SetImage(const Value: TBitmap);
    procedure SetOnLongPress(const Value: TNotifyEvent);
    procedure SetStyledSettings(const Value: TStyledSettings);
    procedure SetText(const Value: string);
    procedure SetTextSettings(const Value: TTextSettings);
    function StyledSettingsStored: Boolean;
  protected
    procedure AfterPaint; override;
    function DefineModelClass: TDataModelClass; override;
    procedure Loaded; override;
    procedure Paint; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromResource(const AResourceName: string);
    procedure LoadFromStream(const AStream: TStream);
    property BackgroundColor: TAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property Image: TBitmap read GetImage write SetImage;
    property Model: TCustomNativeImageModel read GetModel;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings stored StyledSettingsStored nodefault;
    property Text: string read GetText write SetText;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property OnLongPress: TNotifyEvent read GetOnLongPress write SetOnLongPress;
  end;

  {$IF CompilerVersion >= 35}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroidArm32 or pidAndroidArm64)]
  {$ELSE}
  [ComponentPlatformsAttribute(pfidiOS or pidAndroid32Arm or pidAndroid64Arm)]
  {$ENDIF}
  TNativeImage = class(TCustomNativeImage)
  published
    property Align;
    property Anchors;
    property BackgroundColor;
    property Height;
    property Image;
    property Margins;
    property Position;
    property Size;
    property StyledSettings;
    property Text;
    property TextSettings;
    property Visible default True;
    property Width;
    property OnClick;
    property OnDblClick;
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
  DW.NativeImage.iOS,
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.NativeImage.Android,
  {$ENDIF}
  // RTL
  System.SysUtils, System.IOUtils,
  // FMX
  FMX.Consts;
  // DW
//  {$IF Defined(MSWINDOWS)}
//  DW.NativeImage.Win;
//  {$ENDIF}

procedure Register;
begin
  RegisterComponents('Kastri FMX', [TNativeImage]);
end;

type
  TNativeImageTextSettings = class(TTextSettingsInfo.TCustomTextSettings)
  public
    constructor Create(const AOwner: TPersistent); override;
  published
    property Font;
    property FontColor;
    property HorzAlign default TTextAlign.Leading;
    property VertAlign default TTextAlign.Center;
  end;

  TNativeImageSettingsInfo = class (TTextSettingsInfo)
  private
    FModel: TCustomNativeImageModel;
  protected
    procedure DoCalculatedTextSettings; override;
  public
    constructor Create(AOwner: TPersistent; ATextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass); override;
  end;

{ TNativeImageTextSettings }

constructor TNativeImageTextSettings.Create(const AOwner: TPersistent);
begin
  inherited;
  WordWrap := False;
  HorzAlign := TTextAlign.Leading;
  VertAlign := TTextAlign.Center;
end;

{ TNativeImageSettingsInfo }

constructor TNativeImageSettingsInfo.Create(AOwner: TPersistent; ATextSettingsClass: TTextSettingsInfo.TCustomTextSettingsClass);
begin
  inherited;
  if AOwner is TCustomNativeImageModel then
    FModel := TCustomNativeImageModel(AOwner)
  else
    raise EArgumentException.CreateFMT(SEUseHeirs, [TCustomNativeImageModel.ClassName]);
end;

procedure TNativeImageSettingsInfo.DoCalculatedTextSettings;
begin
  inherited;
  FModel.TextSettingsChanged;
end;

{ TCustomNativeImageModel }

constructor TCustomNativeImageModel.Create(const AOwner: TComponent);
begin
  inherited;
  FImage := TBitmap.Create;
  FImage.OnChange := BitmapChangedHandler;
  FTextSettingsInfo := TNativeImageSettingsInfo.Create(Self, TNativeImageTextSettings);
end;

destructor TCustomNativeImageModel.Destroy;
begin
  FImage.Free;
  FTextSettingsInfo.Free;
  inherited;
end;

procedure TCustomNativeImageModel.BitmapChangedHandler(Sender: TObject);
begin
  SendMessage(MM_NATIVEIMAGE_IMAGECHANGED);
end;

procedure TCustomNativeImageModel.Loaded;
begin
  SendMessage(PM_INIT);
end;

procedure TCustomNativeImageModel.LoadFromBitmap;
var
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    if not FImage.IsEmpty then
      FImage.SaveToStream(LStream);
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TCustomNativeImageModel.LoadFromFile(const AFileName: string);
begin
  if TFile.Exists(AFileName) then
    SendMessage<string>(MM_NATIVEIMAGE_LOADFROMFILE, AFileName);
end;

procedure TCustomNativeImageModel.LoadFromResource(const AResourceName: string);
var
  LStream: TStream;
begin
  if FindResource(HInstance, PChar(AResourceName), RT_RCDATA) > 0 then
  begin
    LStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
    try
      LoadFromStream(LStream);
    finally
      LStream.Free;
    end;
  end;
end;

procedure TCustomNativeImageModel.LoadFromStream(const AStream: TStream);
begin
  SendMessage<TStream>(MM_NATIVEIMAGE_LOADFROMSTREAM, AStream);
end;

procedure TCustomNativeImageModel.SetBackgroundColor(const Value: TAlphaColor);
begin
  FBackgroundColor := Value;
  SendMessage(MM_NATIVEIMAGE_BACKGROUNDCOLORCHANGED);
end;

procedure TCustomNativeImageModel.SetImage(const Value: TBitmap);
begin
  FImage.Assign(Value);
  LoadFromBitmap;
end;

procedure TCustomNativeImageModel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    SendMessage<string>(MM_NATIVEIMAGE_TEXTCHANGED, FText);
  end;
end;

procedure TCustomNativeImageModel.TextSettingsChanged;
begin
  SendMessage(MM_NATIVEIMAGE_TEXTSETTINGSCHANGED);
  if Assigned(FOnTextSettingsChanged) then
    FOnTextSettingsChanged(Self);
end;

{ TCustomNativeImage }

constructor TCustomNativeImage.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
  Model.OnTextSettingsChanged := ModelTextSettingsChangedHandler;
end;

function TCustomNativeImage.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeImageModel;
end;

function TCustomNativeImage.GetBackgroundColor: TAlphaColor;
begin
  Result := Model.BackgroundColor;
end;

function TCustomNativeImage.GetImage: TBitmap;
begin
  Result := Model.Image;
end;

function TCustomNativeImage.GetModel: TCustomNativeImageModel;
begin
  Result := inherited GetModel<TCustomNativeImageModel>;
end;

function TCustomNativeImage.GetOnLongPress: TNotifyEvent;
begin
  Result := Model.OnLongPress;
end;

function TCustomNativeImage.GetStyledSettings: TStyledSettings;
begin
  Result := Model.TextSettingsInfo.StyledSettings;
end;

function TCustomNativeImage.GetText: string;
begin
  Result := Model.Text;
end;

function TCustomNativeImage.GetTextSettings: TTextSettings;
begin
  Result := Model.TextSettingsInfo.TextSettings;
end;

procedure TCustomNativeImage.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    Model.LoadFromBitmap;
    Model.Loaded;
  end;
end;

procedure TCustomNativeImage.LoadFromFile(const AFileName: string);
begin
  Model.LoadFromFile(AFileName);
end;

procedure TCustomNativeImage.LoadFromResource(const AResourceName: string);
begin
  Model.LoadFromResource(AResourceName);
end;

procedure TCustomNativeImage.LoadFromStream(const AStream: TStream);
begin
  Model.LoadFromStream(AStream);
end;

procedure TCustomNativeImage.ModelTextSettingsChangedHandler(Sender: TObject);
begin
  if csDesigning in ComponentState then
    Repaint;
end;

procedure TCustomNativeImage.AfterPaint;
begin
  //
end;

procedure TCustomNativeImage.Paint;
var
  LRect: TRectF;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    Canvas.Fill.Color := Model.BackgroundColor;
    Canvas.FillRect(TRectF.Create(0, 0, Width, Height), 1);
    LRect := TRectF.Create(0, 0, Image.Width, Image.Height);
    Canvas.DrawBitmap(Image, LRect, LRect.FitInto(TRectF.Create(0, 0, Width, Height)), AbsoluteOpacity);
    Canvas.Font.Assign(TextSettings.Font);
    Canvas.Fill.Color := TextSettings.FontColor;
    Canvas.FillText(TRectF.Create(0, 0, Width, Height), Text, TextSettings.WordWrap, 1, [], TextSettings.HorzAlign, TextSettings.VertAlign);
  end;
end;

function TCustomNativeImage.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

procedure TCustomNativeImage.SetBackgroundColor(const Value: TAlphaColor);
begin
  Model.BackgroundColor := Value;
end;

procedure TCustomNativeImage.SetImage(const Value: TBitmap);
begin
  Model.Image := Value;
end;

procedure TCustomNativeImage.SetOnLongPress(const Value: TNotifyEvent);
begin
  Model.OnLongPress := Value;
end;

procedure TCustomNativeImage.SetStyledSettings(const Value: TStyledSettings);
begin
  Model.TextSettingsInfo.StyledSettings := Value;
end;

procedure TCustomNativeImage.SetText(const Value: string);
begin
  Model.Text := Value;
  if csDesigning in ComponentState then
    Repaint;
end;

procedure TCustomNativeImage.SetTextSettings(const Value: TTextSettings);
begin
  Model.TextSettingsInfo.TextSettings.Assign(Value);
  if csDesigning in ComponentState then
    Repaint;
end;

function TCustomNativeImage.StyledSettingsStored: Boolean;
begin
  Result := StyledSettings <> DefaultStyledSettings;
end;

end.
