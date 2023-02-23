unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  DW.Biometric;

type
  TfrmMain = class(TForm)
    BiometricImage: TImage;
    TouchMeLabel: TLabel;
    ContentLayout: TLayout;
    CancelButton: TButton;
    ResetButton: TButton;
    KindLabel: TLabel;
    procedure BiometricImageClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
  private
    FBiometric: TBiometric;
    procedure Listening(const AIsListening: Boolean);
    procedure VerificationFailResultHandler(const AFailResult: TBiometricFailResult; const AResultMessage: string);
    procedure VerificationSuccessResultHandler;
    procedure Verify;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses
  FMX.Platform;

const
  cTouchMeCaption = 'Now touch the fingerprint sensor!';

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  Listening(False);
  if TBiometric.IsSupported then
  begin
    FBiometric := TBiometric.Current;
    FBiometric.AllowedAttempts := 1;
    case FBiometric.GetBiometryKind of
      TBiometryKind.None:
        KindLabel.Text := 'Biometry kind: None, or not implemented';
      TBiometryKind.Face:
        KindLabel.Text := 'Biometry kind: Face';
      TBiometryKind.Touch:
        KindLabel.Text := 'Biometry kind: Touch';
    end;
    // FBiometric.KeyName := ChangeFileExt(ExtractFileName(ParamStr(0)), '') + '.key';
  end
  else
  begin
    BiometricImage.Visible := False;
    KindLabel.Text := 'Biometry kind: Not supported on this device';
  end;
end;

destructor TfrmMain.Destroy;
begin
  //
  inherited;
end;

procedure TfrmMain.Listening(const AIsListening: Boolean);
begin
  if AIsListening then
    TouchMeLabel.Text := cTouchMeCaption
  else
    TouchMeLabel.Text := '';
  CancelButton.Visible := AIsListening;
  BiometricImage.Enabled := not AIsListening;
end;

procedure TfrmMain.ResetButtonClick(Sender: TObject);
begin
  if FBiometric <> nil then
    FBiometric.Reset;
  ShowMessage('Authentication has been reset, if supported');
end;

procedure TfrmMain.CancelButtonClick(Sender: TObject);
begin
  if FBiometric <> nil then
    FBiometric.Cancel;
end;

procedure TfrmMain.BiometricImageClick(Sender: TObject);
begin
  if FBiometric <> nil then
  begin
    if FBiometric.CanVerify then
      Verify
    else if FBiometric.IsBiometryLockedOut then
      FBiometric.RestoreBiometry(VerificationSuccessResultHandler, VerificationFailResultHandler) // or perhaps use a separate handler for Biometry
    else
      ShowMessage('Unable to verify. If on Android, check that the USE_FINGERPRINT permission has been added to the manifest');
  end;
end;

procedure TfrmMain.Verify;
begin
  if FBiometric <> nil then
  begin
    if not FBiometric.HasUserInterface then
      Listening(True);
    FBiometric.Verify(cTouchMeCaption, VerificationSuccessResultHandler, VerificationFailResultHandler);
  end;
end;

procedure TfrmMain.VerificationFailResultHandler(const AFailResult: TBiometricFailResult; const AResultMessage: string);
begin
  case AFailResult of
    TBiometricFailResult.Cancelled:
    begin
      Listening(False);
      ShowMessage('You cancelled');
    end;
    TBiometricFailResult.Denied:
      ShowMessage(Format('Denied! Probably the wrong face/finger: %s', [AResultMessage]));
    TBiometricFailResult.Error:
      ShowMessage(Format('Error: %s', [AResultMessage]));
    TBiometricFailResult.Fallback:
      ShowMessage('You chose the fallback');
    TBiometricFailResult.Help:
      ShowMessage(Format('You may need help: %s', [AResultMessage]));
    TBiometricFailResult.LockedOut:
      ShowMessage(Format('You may need to wait for the biometry to unlock: %s', [AResultMessage]));
  else
    ShowMessage('Unhandled result!');
  end;
end;

procedure TfrmMain.VerificationSuccessResultHandler;
begin
  Listening(False);
  ShowMessage('Success!');
end;

end.
