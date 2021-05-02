unit DW.AppleIDButton.iOS;

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
  System.TypInfo,
  // iOS
  iOSapi.Foundation,
  // FMX
  FMX.Presentation.iOS, FMX.Presentation.Messages, FMX.Controls.Model,
  // DW
  DW.AppleIDButton, DW.iOSapi.AuthenticationServices;

type
  IAppleIDButton = interface(ASAuthorizationAppleIDButton)
    ['{332B340D-21DF-4C0E-86A5-B3418ED3D9FF}']
    procedure ButtonClicked(sender: Pointer); cdecl;
  end;

  TiOSAppleIDButton = class(TiOSNativeControl)
  private
    function GetModel: TAppleIDButtonModel;
    function GetView: ASAuthorizationAppleIDButton;
  protected
    function DefineModelClass: TDataModelClass; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure InitView; override;
    procedure MMSetButtonStyle(var AMessage: TDispatchMessageWithValue<TAppleIDButtonStyle>); message MM_SET_BUTTON_STYLE;
    procedure MMSetButtonType(var AMessage: TDispatchMessageWithValue<TAppleIDButtonType>); message MM_SET_BUTTON_TYPE;
    procedure MMSetCornerRadius(var AMessage: TDispatchMessageWithValue<Double>); message MM_SET_CORNER_RADIUS;
  public
    { IAppleIDButton }
    procedure ButtonClicked(sender: Pointer); cdecl;
  public
    constructor Create; override;
    property Model: TAppleIDButtonModel read GetModel;
    property View: ASAuthorizationAppleIDButton read GetView;
  end;

implementation

uses
  // iOS
  iOSapi.UIKit,
  // FMX
  FMX.Presentation.Factory, FMX.Controls,
  // DW
  DW.OSLog,
  DW.AuthenticationServices.iOS;

{ TiOSAppleIDButton }

constructor TiOSAppleIDButton.Create;
begin
  inherited;
  RegisterNativeEventHandler('ButtonClicked:', UIControlEventTouchDown);
end;

function TiOSAppleIDButton.DefineModelClass: TDataModelClass;
begin
  Result := TAppleIDButtonModel;
end;

function TiOSAppleIDButton.GetModel: TAppleIDButtonModel;
begin
  Result := inherited GetModel<TAppleIDButtonModel>;
end;

function TiOSAppleIDButton.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IAppleIDButton);
end;

function TiOSAppleIDButton.GetView: ASAuthorizationAppleIDButton;
begin
  Result := inherited GetView<ASAuthorizationAppleIDButton>;
end;

procedure TiOSAppleIDButton.InitView;
const
  cButtonStyles: array[TAppleIDButtonStyle] of ASAuthorizationAppleIDButtonStyle = (
    ASAuthorizationAppleIDButtonStyleWhite, ASAuthorizationAppleIDButtonStyleWhiteOutline, ASAuthorizationAppleIDButtonStyleBlack
  );
  cButtonTypes: array[TAppleIDButtonType] of ASAuthorizationAppleIDButtonType = (
    ASAuthorizationAppleIDButtonTypeSignIn, ASAuthorizationAppleIDButtonTypeContinue, ASAuthorizationAppleIDButtonTypeSignUp
  );
var
  LViewPtr: Pointer;
begin
  LViewPtr := View.initWithAuthorizationButtonType(cButtonTypes[Model.ButtonType], cButtonStyles[Model.ButtonStyle]);
  if GetObjectID <> LViewPtr then
    UpdateObjectID(LViewPtr);
end;

procedure TiOSAppleIDButton.MMSetButtonStyle(var AMessage: TDispatchMessageWithValue<TAppleIDButtonStyle>);
begin
  InitView;
end;

procedure TiOSAppleIDButton.MMSetButtonType(var AMessage: TDispatchMessageWithValue<TAppleIDButtonType>);
begin
  InitView;
end;

procedure TiOSAppleIDButton.MMSetCornerRadius(var AMessage: TDispatchMessageWithValue<Double>);
begin
  View.setCornerRadius(AMessage.Value);
end;

procedure TiOSAppleIDButton.ButtonClicked(sender: Pointer);
begin
  TAuthenticationServices.Current.RequestAuthorization;
end;

initialization
  TPresentationProxyFactory.Current.Register(TAppleIDButton, TControlType.Platform, TiOSPresentationProxy<TiOSAppleIDButton>);

finalization
  TPresentationProxyFactory.Current.Unregister(TAppleIDButton, TControlType.Platform, TiOSPresentationProxy<TiOSAppleIDButton>);

end.
