unit DW.NativeButton.iOS;

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
  System.TypInfo, System.Classes, System.SysUtils, System.Types,
  // macOS
  Macapi.Helpers,
  // iOS
  iOSapi.UIKit, iOSapi.Foundation,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.iOS, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  // DW
  DW.NativeButton;

type
  INativeButton = interface(UIButton)
    ['{9A55BD8B-6C46-47DD-9647-0B957EDFAAA7}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
  end;

  TiOSNativeButton = class(TiOSNativeControl)
  private
    function GetModel: TCustomNativeButtonModel;
    function GetView: UIButton;
    procedure MMSetText(var AMessage: TDispatchMessageWithValue<string>); message MM_SET_TEXT;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    function DefineModelClass: TDataModelClass; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeButtonModel read GetModel;
    property View: UIButton read GetView;
  end;

{ TiOSNativeButton }

constructor TiOSNativeButton.Create;
begin
  inherited;
  View.setUserInteractionEnabled(True);
end;

function TiOSNativeButton.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeButtonModel;
end;

destructor TiOSNativeButton.Destroy;
begin
  //
  inherited;
end;

function TiOSNativeButton.GetModel: TCustomNativeButtonModel;
begin
  Result := inherited GetModel<TCustomNativeButtonModel>;
end;

function TiOSNativeButton.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(INativeButton);
end;

function TiOSNativeButton.GetView: UIButton;
begin
  Result := inherited GetView<UIButton>;
end;

procedure TiOSNativeButton.MMSetText(var AMessage: TDispatchMessageWithValue<string>);
begin
  View.setTitle(StrToNSStr(AMessage.Value), UIControlStateNormal);
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeButton, TControlType.Platform, TiOSPresentationProxy<TiOSNativeButton>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeButton, TControlType.Platform, TiOSPresentationProxy<TiOSNativeButton>);

end.
