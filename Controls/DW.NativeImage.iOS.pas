unit DW.NativeImage.iOS;

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
  System.TypInfo, System.Classes,
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.iOS, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
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
    function GetImageControl: TCustomNativeImage;
    function GetModel: TCustomNativeImageModel;
    function GetView: UIImageView;
    procedure MMLoadFromFile(var AMessage: TDispatchMessageWithValue<string>); message MM_NATIVEIMAGE_LOADFROMFILE;
    procedure MMLoadFromStream(var AMessage: TDispatchMessageWithValue<TStream>); message MM_NATIVEIMAGE_LOADFROMSTREAM;
  protected
    function DefineModelClass: TDataModelClass; override;
    function GetObjectiveCClass: PTypeInfo; override;
    property ImageControl: TCustomNativeImage read GetImageControl;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeImageModel read GetModel;
    property View: UIImageView read GetView;
  end;

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
  Result := TCustomNativeImage(Control)
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

initialization
  TPresentationProxyFactory.Current.Register(TNativeImage, TControlType.Platform, TiOSPresentationProxy<TiOSNativeImage>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeImage, TControlType.Platform, TiOSPresentationProxy<TiOSNativeImage>);

end.
