unit DW.NativeImage.iOS;

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

implementation

uses
  // RTL
  System.TypInfo, System.Classes, System.SysUtils, System.UITypes, System.Types, System.Math,
  // macOS
  Macapi.Helpers, Macapi.CoreFoundation, Macapi.ObjectiveC,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation, iOSapi.CoreText, iOSapi.CocoaTypes,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.iOS, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  FMX.Graphics, FMX.Helpers.iOS, FMX.Types,
  // DW
  DW.NativeImage;

type
  INativeImageView = interface(UIImageView)
    ['{9258442E-A4D6-46A9-9340-E7EADC9FFEA2}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  TiOSNativeImage = class(TiOSNativeControl)
  private
    FAttributedString: NSMutableAttributedString;
    FLabel: UILabel;
    function GetImageControl: TCustomNativeImage;
    function GetModel: TCustomNativeImageModel;
    function GetView: UIImageView;
    procedure MMImageChanged(var AMessage: TDispatchMessage); message MM_NATIVEIMAGE_IMAGECHANGED;
    procedure MMLoadFromFile(var AMessage: TDispatchMessageWithValue<string>); message MM_NATIVEIMAGE_LOADFROMFILE;
    procedure MMLoadFromStream(var AMessage: TDispatchMessageWithValue<TStream>); message MM_NATIVEIMAGE_LOADFROMSTREAM;
    procedure MMTextChanged(var AMessage: TDispatchMessageWithValue<string>); message MM_NATIVEIMAGE_TEXTCHANGED;
    procedure MMTextSettingsChanged(var AMessage: TDispatchMessage); message MM_NATIVEIMAGE_TEXTSETTINGSCHANGED;
    procedure PMInit(var AMessage: TDispatchMessage); message PM_INIT;
    procedure UpdateText;
    procedure UpdateTextSettings;
    procedure UpdateTextVerticalAlignment;
  protected
    function DefineModelClass: TDataModelClass; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure SetSize(const ASize: TSizeF); override;
    property ImageControl: TCustomNativeImage read GetImageControl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeImageModel read GetModel;
    property View: UIImageView read GetView;
  end;

  UILabelEx = interface(UIView)
    ['{2432005B-CCAA-4070-8A36-B097A70D0386}']
    procedure setAttributedText(attributedText: NSAttributedString); cdecl;
    procedure setPreferredMaxLayoutWidth(value: CGFloat); cdecl;
  end;
  TUILabelEx = class(TOCGenericImport<UILabelClass, UILabelEx>)  end;


{ TiOSNativeImage }

constructor TiOSNativeImage.Create;
begin
  inherited;
  View.setUserInteractionEnabled(True);
end;

destructor TiOSNativeImage.Destroy;
begin
  //
  inherited;
end;

function TiOSNativeImage.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeImageModel;
end;

function TiOSNativeImage.GetImageControl: TCustomNativeImage;
begin
  Result := TCustomNativeImage(Control);
end;

function TiOSNativeImage.GetModel: TCustomNativeImageModel;
begin
  Result := inherited GetModel<TCustomNativeImageModel>;
end;

function TiOSNativeImage.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(INativeImageView);
end;

function TiOSNativeImage.GetView: UIImageView;
begin
  Result := inherited GetView<UIImageView>;
  if FLabel = nil then
  begin
    FLabel := TUILabel.Alloc;
    FLabel := TUILabel.Wrap(FLabel.initWithFrame(Result.frame));
    FLabel.setNumberOfLines(0);
    Result.addSubview(FLabel);
  end;
end;

procedure TiOSNativeImage.MMImageChanged(var AMessage: TDispatchMessage);
begin
  View.setImage(BitmapToUIImage(Model.Image));
end;

procedure TiOSNativeImage.MMLoadFromFile(var AMessage: TDispatchMessageWithValue<string>);
begin
  View.setImage(TUIImage.Wrap(TUIImage.OCClass.imageNamed(StrToNSStr(AMessage.Value))));
end;

procedure TiOSNativeImage.MMLoadFromStream(var AMessage: TDispatchMessageWithValue<TStream>);
var
  LData: NSData;
  LStream: TCustomMemoryStream;
begin
  if AMessage.Value is TCustomMemoryStream then
  begin
    LStream := TCustomMemoryStream(AMessage.Value);
    LData := TNSData.Wrap(TNSData.OCClass.dataWithBytes(LStream.Memory, LStream.Size));
    View.setImage(TUIImage.Wrap(TUIImage.OCClass.imageWithData(LData)));
  end;
end;

procedure TiOSNativeImage.MMTextChanged(var AMessage: TDispatchMessageWithValue<string>);
begin
  UpdateText;
end;

procedure TiOSNativeImage.MMTextSettingsChanged(var AMessage: TDispatchMessage);
begin
  UpdateTextSettings;
end;

procedure TiOSNativeImage.PMInit(var AMessage: TDispatchMessage);
begin
  UpdateText;
  UpdateTextSettings;
end;

procedure TiOSNativeImage.SetSize(const ASize: TSizeF);
begin
  inherited;
  UpdateTextVerticalAlignment;
end;

procedure TiOSNativeImage.UpdateText;
begin
  FLabel.setText(StrToNSStr(Model.Text));
end;

procedure TiOSNativeImage.UpdateTextSettings;
var
  LTextSettings: TTextSettings;
  LTextRange: NSRange;
  LFontRef: CTFontRef;
  LUnderline: CFNumberRef;
  LValue: Cardinal;
begin
  LTextSettings := Model.TextSettingsInfo.TextSettings;
  if FAttributedString <> nil then
    FAttributedString.release;
  FAttributedString := TNSMutableAttributedString.Alloc;
  FAttributedString := TNSMutableAttributedString.Wrap(FAttributedString.initWithString(StrToNSStr(Model.Text)));
  FAttributedString.beginEditing;
  try
    LTextRange := NSMakeRange(0, Model.Text.Length);
    LFontRef := FontToCTFontRef(LTextSettings.Font);
    if LFontRef <> nil then
    try
      FAttributedString.addAttribute(TNSString.Wrap(kCTFontAttributeName), LFontRef, LTextRange);
    finally
      CFRelease(LFontRef);
    end;
    if TFontStyle.fsUnderline in LTextSettings.Font.Style then
    begin
      LValue := kCTUnderlineStyleSingle;
      LUnderline := CFNumberCreate(nil, kCFNumberSInt32Type, @LValue);
      try
        FAttributedString.addAttribute(TNSString.Wrap(kCTUnderlineStyleAttributeName), LUnderline, LTextRange);
      finally
        CFRelease(LUnderline);
      end;
    end;
  finally
    FAttributedString.endEditing;
  end;
  TUILabelEx.Wrap(NSObjectToID(FLabel)).setAttributedText(FAttributedString);
  FLabel.setTextColor(AlphaColorToUIColor(LTextSettings.FontColor));
  FLabel.setFont(FontToUIFont(LTextSettings.Font));
  FLabel.setTextAlignment(TextAlignToUITextAlignment(LTextSettings.HorzAlign));
  if LTextSettings.WordWrap then
    FLabel.setLineBreakMode(NSLineBreakByWordWrapping)
  else
    FLabel.setLineBreakMode(NSLineBreakByClipping);
  UpdateTextVerticalAlignment;
end;

procedure TiOSNativeImage.UpdateTextVerticalAlignment;
var
  LAlign: TTextAlign;
  LFrame: NSRect;
  LHeightDiff: Single;
begin
  LAlign := Model.TextSettingsInfo.TextSettings.VertAlign;
  LFrame := View.frame;
  TUILabelEx.Wrap(NSObjectToID(FLabel)).setPreferredMaxLayoutWidth(View.frame.size.width);
  FLabel.sizeToFit;
  FLabel.setNeedsDisplay;
  LHeightDiff := LFrame.size.height - FLabel.frame.size.height;
  case LAlign of
    TTextAlign.Center:
    begin
      // If the label is smaller, center it
      if LHeightDiff > 0 then
      begin
        LFrame.origin.y := LFrame.origin.y + (LHeightDiff / 2);
        LFrame.size.height := FLabel.frame.size.height;
      end;
      FLabel.setFrame(LFrame);
    end;
    TTextAlign.Leading:
    begin
      // Label goes at the top, and the height is the smaller of the label height, or the parent view height
      LFrame.size.height := Min(FLabel.frame.size.height, LFrame.size.height);
      FLabel.setFrame(LFrame);
    end;
    TTextAlign.Trailing:
    begin
      // If the label is smaller, push it down to the bottom
      if LHeightDiff > 0 then
      begin
        LFrame.origin.y := LFrame.origin.y + LHeightDiff;
        LFrame.size.height := FLabel.frame.size.height;
      end;
      FLabel.setFrame(LFrame);
    end;
  end;
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeImage, TControlType.Platform, TiOSPresentationProxy<TiOSNativeImage>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeImage, TControlType.Platform, TiOSPresentationProxy<TiOSNativeImage>);

end.
