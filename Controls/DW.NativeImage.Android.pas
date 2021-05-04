unit DW.NativeImage.Android;

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

implementation

uses
  // RTL
  System.TypInfo, System.Classes, System.SysUtils, System.UITypes,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Widget, Androidapi.Helpers, Androidapi.JNI.Net, Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge, Androidapi.JNI, Androidapi.JNI.Support, Androidapi.JNI.Util,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.Android, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  FMX.Graphics, FMX.Types,
  // DW
  DW.NativeImage;

type
  TAndroidNativeImage = class(TAndroidNativeView)
  private
    FImageView: JImageView;
    FTextView: JTextView;
    FView: JViewGroup;
    function GetImageControl: TCustomNativeImage;
    function GetModel: TCustomNativeImageModel;
    procedure MMLoadFromFile(var AMessage: TDispatchMessageWithValue<string>); message MM_NATIVEIMAGE_LOADFROMFILE;
    procedure MMLoadFromStream(var AMessage: TDispatchMessageWithValue<TStream>); message MM_NATIVEIMAGE_LOADFROMSTREAM;
    procedure MMTextChanged(var AMessage: TDispatchMessageWithValue<string>); message MM_NATIVEIMAGE_TEXTCHANGED;
    procedure MMTextSettingsChanged(var AMessage: TDispatchMessage); message MM_NATIVEIMAGE_TEXTSETTINGSCHANGED;
    procedure PMInit(var AMessage: TDispatchMessage); message PM_INIT;
    procedure UpdateText;
    procedure UpdateTextSettings;
  protected
    function CreateView: JView; override;
    function DefineModelClass: TDataModelClass; override;
    property ImageControl: TCustomNativeImage read GetImageControl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeImageModel read GetModel;
    property View: JViewGroup read FView;
  end;

// Move these out to a common unit
function VertTextAlignToGravity(const AAlign: TTextAlign): Integer;
begin
  case AAlign of
    TTextAlign.Center:
      Result := TJGravity.JavaClass.CENTER_VERTICAL;
    TTextAlign.Leading:
      Result := TJGravity.JavaClass.TOP;
    TTextAlign.Trailing:
      Result := TJGravity.JavaClass.BOTTOM;
  else
    Result := TJGravity.JavaClass.CENTER_VERTICAL;
  end;
end;

function HorzTextAlignToGravity(const AAlign: TTextAlign): Integer;
begin
  case AAlign of
    TTextAlign.Center:
      Result := TJGravity.JavaClass.CENTER;
    TTextAlign.Leading:
      Result := TJGravity.JavaClass.LEFT;
    TTextAlign.Trailing:
      Result := TJGravity.JavaClass.RIGHT;
  else
    Result := TJGravity.JavaClass.CENTER;
  end;
end;

function TFontStylesToStyle(const AStyle: TFontStyles): Integer;
begin
  if (TFontStyle.fsBold in AStyle) and (TFontStyle.fsItalic in AStyle) then
    Result := TJTypeface.JavaClass.BOLD_ITALIC
  else if TFontStyle.fsBold in AStyle then
    Result := TJTypeface.JavaClass.BOLD
  else if TFontStyle.fsItalic in AStyle then
    Result := TJTypeface.JavaClass.ITALIC
  else
    Result := TJTypeface.JavaClass.NORMAL;
end;

{ TAndroidNativeImage }

constructor TAndroidNativeImage.Create;
begin
  inherited;
  //
end;

destructor TAndroidNativeImage.Destroy;
begin
  //
  inherited;
end;

function TAndroidNativeImage.CreateView: JView;
var
  LLayoutParams: JRelativeLayout_LayoutParams;
begin
  LLayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT,
    TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FView := TJRelativeLayout.JavaClass.init(TAndroidHelper.Context);
  FImageView := TJImageView.JavaClass.init(TAndroidHelper.Context);
  FImageView.setScaleType(TJImageView_ScaleType.JavaClass.CENTER_CROP);
  FView.addView(FImageView, LLayoutParams);
  FTextView := TJTextView.JavaClass.init(TAndroidHelper.Context);
  // Make the image show through
  FTextView.setBackgroundColor(TJColor.JavaClass.TRANSPARENT);
  FView.addView(FTextView, LLayoutParams);
  Result := FView;
end;

function TAndroidNativeImage.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeImageModel;
end;

function TAndroidNativeImage.GetImageControl: TCustomNativeImage;
begin
  Result := TCustomNativeImage(Control)
end;

function TAndroidNativeImage.GetModel: TCustomNativeImageModel;
begin
  Result := inherited GetModel<TCustomNativeImageModel>;
end;

procedure TAndroidNativeImage.MMLoadFromFile(var AMessage: TDispatchMessageWithValue<string>);
begin
  FImageView.setImageURI(TAndroidHelper.JFileToJURI(TJFile.JavaClass.init(StringToJString(AMessage.Value))));
end;

procedure TAndroidNativeImage.MMLoadFromStream(var AMessage: TDispatchMessageWithValue<TStream>);
var
  LBytes: TBytes;
  LJBytes: TJavaArray<Byte>;
begin
  SetLength(LBytes, AMessage.Value.Size);
  AMessage.Value.Position := 0;
  AMessage.Value.Read(LBytes, 0, Length(LBytes));
  LJBytes := TAndroidHelper.TBytesToTJavaArray(LBytes);
  FImageView.setImageBitmap(TJBitmapFactory.JavaClass.decodeByteArray(LJBytes, 0, LJBytes.Length));
end;

procedure TAndroidNativeImage.MMTextChanged(var AMessage: TDispatchMessageWithValue<string>);
begin
  UpdateText;
end;

procedure TAndroidNativeImage.MMTextSettingsChanged(var AMessage: TDispatchMessage);
begin
  UpdateTextSettings;
end;

procedure TAndroidNativeImage.PMInit(var AMessage: TDispatchMessage);
begin
  UpdateText;
  UpdateTextSettings;
end;

procedure TAndroidNativeImage.UpdateText;
begin
  FTextView.setText(StrToJCharSequence(Model.Text), TJTextView_BufferType.JavaClass.NORMAL);
end;

procedure TAndroidNativeImage.UpdateTextSettings;
var
  LTextSettings: TTextSettings;
begin
  LTextSettings := Model.TextSettingsInfo.TextSettings;
  FTextView.setTextColor(TAndroidHelper.AlphaColorToJColor(LTextSettings.FontColor));
  FTextView.setTextSize(TJTypedValue.JavaClass.COMPLEX_UNIT_DIP, LTextSettings.Font.Size);
  FTextView.setGravity(VertTextAlignToGravity(LTextSettings.VertAlign) or HorzTextAlignToGravity(LTextSettings.HorzAlign));
  FTextView.setTypeface(TJTypeface.JavaClass.create(StringToJString(LTextSettings.Font.Family),
    TFontStylesToStyle(LTextSettings.Font.Style)));
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeImage, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeImage>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeImage, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeImage>);

end.
