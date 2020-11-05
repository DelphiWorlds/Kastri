unit DW.NativeImage.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

implementation

uses
  // RTL
  System.TypInfo, System.Classes, System.SysUtils,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Widget, Androidapi.Helpers, Androidapi.JNI.Net, Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge, Androidapi.JNI, Androidapi.JNI.Support,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.Android, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  // DW
  DW.NativeImage;

type
  TAndroidNativeImage = class(TAndroidNativeView)
  private
    FView: JImageView;
    function GetImageControl: TCustomNativeImage;
    function GetModel: TCustomNativeImageModel;
    procedure MMLoadFromFile(var AMessage: TDispatchMessageWithValue<string>); message MM_NATIVEIMAGE_LOADFROMFILE;
    procedure MMLoadFromStream(var AMessage: TDispatchMessageWithValue<TStream>); message MM_NATIVEIMAGE_LOADFROMSTREAM;
  protected
    function CreateView: JView; override;
    function DefineModelClass: TDataModelClass; override;
    property ImageControl: TCustomNativeImage read GetImageControl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeImageModel read GetModel;
    property View: JImageView read FView;
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
begin
  FView := TJImageView.JavaClass.init(TAndroidHelper.Context);
  FView.setScaleType(TJImageView_ScaleType.JavaClass.CENTER_CROP);
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
  View.setImageURI(TAndroidHelper.JFileToJURI(TJFile.JavaClass.init(StringToJString(AMessage.Value))));
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
  View.setImageBitmap(TJBitmapFactory.JavaClass.decodeByteArray(LJBytes, 0, LJBytes.Length));
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeImage, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeImage>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeImage, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeImage>);

end.
