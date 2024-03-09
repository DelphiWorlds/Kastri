unit DW.NativeButton.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

implementation

uses
  // RTL
  System.TypInfo, System.Classes, System.SysUtils, System.Types,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Widget, Androidapi.Helpers, Androidapi.JNI.Net, Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge, Androidapi.JNI, Androidapi.JNI.Support,
  // FMX
  FMX.Presentation.Messages, FMX.Presentation.Android, FMX.Presentation.Factory, FMX.Controls, FMX.Controls.Presentation, FMX.Controls.Model,
  // DW
  DW.NativeButton, DW.NativeControl.Android;

type
  TAndroidNativeButton = class(TNativeControl)
  private
    FView: JButton;
    function GetModel: TCustomNativeButtonModel;
    procedure MMSetText(var AMessage: TDispatchMessageWithValue<string>); message MM_SET_TEXT;
  protected
    function CreateView: JView; override;
    function DefineModelClass: TDataModelClass; override;
    procedure DoLongPress; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Model: TCustomNativeButtonModel read GetModel;
    property View: JButton read FView;
  end;

{ TAndroidNativeButton }

constructor TAndroidNativeButton.Create;
begin
  inherited;
  //
end;

function TAndroidNativeButton.DefineModelClass: TDataModelClass;
begin
  Result := TCustomNativeButtonModel;
end;

destructor TAndroidNativeButton.Destroy;
begin
  //
  inherited;
end;

procedure TAndroidNativeButton.DoLongPress;
begin
  Model.DoLongPress;
end;

function TAndroidNativeButton.GetModel: TCustomNativeButtonModel;
begin
  Result := inherited GetModel<TCustomNativeButtonModel>;
end;

procedure TAndroidNativeButton.MMSetText(var AMessage: TDispatchMessageWithValue<string>);
begin
  View.setText(StrToJCharSequence(AMessage.Value));
end;

function TAndroidNativeButton.CreateView: JView;
begin
  FView := TJButton.JavaClass.init(TAndroidHelper.Context);
  FView.setAllCaps(False);
  Result := FView;
end;

initialization
  TPresentationProxyFactory.Current.Register(TNativeButton, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeButton>);

finalization
  TPresentationProxyFactory.Current.Unregister(TNativeButton, TControlType.Platform, TAndroidPresentationProxy<TAndroidNativeButton>);

end.
