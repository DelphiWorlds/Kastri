unit DW.NativeImage.Win;

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
  System.Classes,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Presentation.Win,
  FMX.Presentation.Win.Style, FMX.Controls.Model,
  // DW
  DW.NativeImage;

type
  TWinNativeImage = class(TWinStyledPresentation)
  private
    // [Weak] FModel: TCustomNativeImageModel;
    procedure MMLoadFromFile(var AMessage: TDispatchMessageWithValue<string>); message MM_NATIVEIMAGE_LOADFROMFILE;
    procedure MMLoadFromStream(var AMessage: TDispatchMessageWithValue<TStream>); message MM_NATIVEIMAGE_LOADFROMSTREAM;
  protected
    function DefineModelClass: TDataModelClass; override;
  end;

{ TWinNativeImage }

function TWinNativeImage.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeImageModel;
end;

procedure TWinNativeImage.MMLoadFromFile(var AMessage: TDispatchMessageWithValue<string>);
begin
  // TODO
end;

procedure TWinNativeImage.MMLoadFromStream(var AMessage: TDispatchMessageWithValue<TStream>);
begin
  // TODO
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeImage, TControlType.Platform, TWinPresentationProxy<TWinNativeImage>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeImage, TControlType.Platform, TWinPresentationProxy<TWinNativeImage>);

end.
