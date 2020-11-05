unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls,
  DW.AppleIDButton, DW.AuthenticationServices.Types;

type
  TForm1 = class(TForm)
  private
    FAppleIDButton: TAppleIDButton;
    procedure AppleIDButtonAuthorizationResponseHandler(Sender: TObject; const AResponse: TAppleIDAuthorizationResponse);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FAppleIDButton := TAppleIDButton.Create(Self);
  FAppleIDButton.ButtonType := TAppleIDButtonType.SignIn;
  FAppleIDButton.ButtonStyle := TAppleIDButtonStyle.White;
  FAppleIDButton.CornerRadius := 30;
  FAppleIDButton.Align := TAlignLayout.Center;
  FAppleIDButton.OnAuthorizationResponse := AppleIDButtonAuthorizationResponseHandler;
  FAppleIDButton.Parent := Self;
end;

procedure TForm1.AppleIDButtonAuthorizationResponseHandler(Sender: TObject; const AResponse: TAppleIDAuthorizationResponse);
begin
  // Handle the response here
  //   If the authorization failed or the user canceled, AResponse.FailReason will be something other than TAppleIDAuthorizationFailReason.None
  //   If the authorization succeeded, the values in AResponse.Credentials will be valid, and will depend on whether the user opted for AppleID or Password
  if not AResponse.IsAuthorized then
  begin
    case AResponse.FailReason of
      TAppleIDAuthorizationFailReason.Canceled:
        ShowMessage('You canceled!');
    else
      ShowMessage('Signin failed');
    end;
  end
  else
    ShowMessage('Authorized!');
end;

end.
