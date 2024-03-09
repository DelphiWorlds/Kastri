unit DW.AppleIDButton;

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

uses
  // RTL
  System.Messaging, System.Classes, System.Types,
  // FMX
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Controls.Model,
  // DW
  DW.AuthenticationServices.Types;

const
  MM_SET_CORNER_RADIUS = MM_USER + 1;
  MM_SET_BUTTON_STYLE = MM_USER + 2;
  MM_SET_BUTTON_TYPE = MM_USER + 3;

type
  TAppleIDAuthorizationResponseEvent = procedure(Sender: TObject; const Response: TAppleIDAuthorizationResponse) of object;

  TAppleIDButtonType = (SignIn, Continue, SignUp);
  TAppleIDButtonStyle = (White, WhiteOutline, Black);

  TAppleIDButtonModel = class(TDataModel)
  private
    FButtonStyle: TAppleIDButtonStyle;
    FButtonType: TAppleIDButtonType;
    FCornerRadius: Double;
    procedure SetCornerRadius(const Value: Double);
    procedure SetButtonStyle(const Value: TAppleIDButtonStyle);
    procedure SetButtonType(const Value: TAppleIDButtonType);
  public
    property ButtonStyle: TAppleIDButtonStyle read FButtonStyle write SetButtonStyle;
    property ButtonType: TAppleIDButtonType read FButtonType write SetButtonType;
    property CornerRadius: Double read FCornerRadius write SetCornerRadius;
  end;

  TAppleIDButton = class(TButton)
  private
    FOnAuthorizationResponse: TAppleIDAuthorizationResponseEvent;
    procedure AppleIDAuthorizationResponseMessageHandler(const Sender: TObject; const AMsg: TMessage);
    function GetModel: TAppleIDButtonModel; overload;
    procedure SetCornerRadius(const Value: Double);
    procedure SetButtonStyle(const Value: TAppleIDButtonStyle);
    procedure SetButtonType(const Value: TAppleIDButtonType);
    function GetButtonStyle: TAppleIDButtonStyle;
    function GetButtonType: TAppleIDButtonType;
    function GetCornerRadius: Double;
  protected
    function DefineModelClass: TDataModelClass; override;
    function RecommendSize(const AWishedSize: TSizeF): TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ButtonStyle: TAppleIDButtonStyle read GetButtonStyle write SetButtonStyle;
    property ButtonType: TAppleIDButtonType read GetButtonType write SetButtonType;
    property CornerRadius: Double read GetCornerRadius write SetCornerRadius;
    property Model: TAppleIDButtonModel read GetModel;
    property OnAuthorizationResponse: TAppleIDAuthorizationResponseEvent read FOnAuthorizationResponse write FOnAuthorizationResponse;
  end;

implementation

{$IF Defined(IOS)}
uses
  // DW
  DW.OSLog,
  DW.AppleIDButton.iOS;
{$ENDIF}

{ TAppleIDButtonModel }

procedure TAppleIDButtonModel.SetButtonStyle(const Value: TAppleIDButtonStyle);
begin
  if Value <> FButtonStyle then
  begin
    FButtonStyle := Value;
    SendMessage<TAppleIDButtonStyle>(MM_SET_BUTTON_STYLE, FButtonStyle);
  end;
end;

procedure TAppleIDButtonModel.SetButtonType(const Value: TAppleIDButtonType);
begin
  if Value <> FButtonType then
  begin
    FButtonType := Value;
    SendMessage<TAppleIDButtonType>(MM_SET_BUTTON_STYLE, FButtonType);
  end;
end;

procedure TAppleIDButtonModel.SetCornerRadius(const Value: Double);
begin
  if Value <> FCornerRadius then
  begin
    FCornerRadius := Value;
    SendMessage<Double>(MM_SET_CORNER_RADIUS, FCornerRadius);
  end;
end;

{ TAppleIDButton }

constructor TAppleIDButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlType := TControlType.Platform;
  Width := 200;
  Height := 50;
  TMessageManager.DefaultManager.SubscribeToMessage(TAppleIDAuthorizationResponseMessage, AppleIDAuthorizationResponseMessageHandler);
end;

destructor TAppleIDButton.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TAppleIDAuthorizationResponseMessage, AppleIDAuthorizationResponseMessageHandler);
  inherited;
end;

function TAppleIDButton.DefineModelClass: TDataModelClass;
begin
  Result := TAppleIDButtonModel;
end;

function TAppleIDButton.GetButtonStyle: TAppleIDButtonStyle;
begin
  Result := Model.ButtonStyle;
end;

function TAppleIDButton.GetButtonType: TAppleIDButtonType;
begin
  Result := Model.ButtonType;
end;

function TAppleIDButton.GetCornerRadius: Double;
begin
  Result := Model.CornerRadius;
end;

function TAppleIDButton.GetModel: TAppleIDButtonModel;
begin
  Result := GetModel<TAppleIDButtonModel>;
end;

function TAppleIDButton.RecommendSize(const AWishedSize: TSizeF): TSizeF;
begin
  Result := AWishedSize;
end;

procedure TAppleIDButton.SetButtonStyle(const Value: TAppleIDButtonStyle);
begin
  Model.ButtonStyle := Value;
end;

procedure TAppleIDButton.SetButtonType(const Value: TAppleIDButtonType);
begin
  Model.ButtonType := Value;
end;

procedure TAppleIDButton.SetCornerRadius(const Value: Double);
begin
  Model.CornerRadius := Value;
end;

procedure TAppleIDButton.AppleIDAuthorizationResponseMessageHandler(const Sender: TObject; const AMsg: TMessage);
var
  LResponse: TAppleIDAuthorizationResponse;
begin
  LResponse := TAppleIDAuthorizationResponseMessage(AMsg).Value;
  if Assigned(FOnAuthorizationResponse) then
    FOnAuthorizationResponse(Self, LResponse);
end;

end.
