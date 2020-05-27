unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  DW.Biometric;

type
  TfrmMain = class(TForm)
    BiometricImage: TImage;
    TouchMeLabel: TLabel;
    ContentLayout: TLayout;
    CancelButton: TButton;
    ResetButton: TButton;
    procedure BiometricImageClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
  private
    procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
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
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  Listening(False);
  if not TBiometric.IsSupported then
  begin
    BiometricImage.Visible := False;
    ShowMessage('Biometric not supported');
  end
  else
    TBiometric.Current.KeyName := ChangeFileExt(ExtractFileName(ParamStr(0)), '') + '.key';
end;

destructor TfrmMain.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
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
  TBiometric.Current.Reset;
  ShowMessage('Authentication has been reset, if supported');
end;

procedure TfrmMain.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  case TApplicationEventMessage(M).Value.Event of
    TApplicationEvent.EnteredBackground:
    begin
      // When your app goes into the background, you should cancel listening for verification!
      TBiometric.Current.Cancel;
    end;
  end;
end;

procedure TfrmMain.CancelButtonClick(Sender: TObject);
begin
  TBiometric.Current.Cancel;
end;

procedure TfrmMain.BiometricImageClick(Sender: TObject);
begin
  if TBiometric.Current.CanVerify then
    Verify
  else if TBiometric.Current.IsBiometryLockedOut then
    TBiometric.Current.RestoreBiometry(VerificationSuccessResultHandler, VerificationFailResultHandler) // or perhaps use a separate handler for Biometry
  else
    ShowMessage('Unable to verify. If on Android, check that the USE_FINGERPRINT permission has been added to the manifest');
end;

procedure TfrmMain.Verify;
begin
  if not TBiometric.Current.HasUserInterface then
    Listening(True);
  TBiometric.Current.Verify(cTouchMeCaption, VerificationSuccessResultHandler, VerificationFailResultHandler);
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
      ShowMessage(Format('Denied! Probably the wrong finger: %s', [AResultMessage]));
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
