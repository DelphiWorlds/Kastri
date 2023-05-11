unit MainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Layouts,
  DW.SpeechRecognition, DW.Types;

type
  TfrmMain = class(TForm)
    RecordButton: TButton;
    Memo: TMemo;
    MessageLabel: TLabel;
    BottomLayout: TLayout;
    procedure RecordButtonClick(Sender: TObject);
  private
    FSpeech: TSpeechRecognition;
    FText: string;
    procedure DoStopped;
    procedure SpeechAuthorizationHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
    procedure SpeechRecordingHandler(Sender: TObject; const IsRecording: Boolean);
    procedure SpeechTextHandler(Sender: TObject; const AText: string);
    procedure SpeechStoppedHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

{ TForm1 }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  FSpeech := TSpeechRecognition.Create;
  FSpeech.StopInterval := 1500;
  FSpeech.WantPartialResults := True;
  FSpeech.OnAuthorizationStatus := SpeechAuthorizationHandler;
  FSpeech.OnRecording := SpeechRecordingHandler;
  FSpeech.OnStopped := SpeechStoppedHandler;
  FSpeech.OnText := SpeechTextHandler;
  // FSpeech.Language := 'es-MX'; // For example, if you are expecting Spanish (Mexican)
  DoStopped;
end;

destructor TfrmMain.Destroy;
begin
  FSpeech.Free;
  inherited;
end;

procedure TfrmMain.DoStopped;
begin
  MessageLabel.Text := 'Tap/click the button, and say something!';
end;

procedure TfrmMain.SpeechAuthorizationHandler(Sender: TObject; const AStatus: TAuthorizationStatus);
begin
  RecordButton.Enabled := False;
  case AStatus of
    TAuthorizationStatus.Authorized:
    begin
      RecordButton.Enabled := True;
      Memo.Lines.Add('Authorized!');
    end;
    TAuthorizationStatus.Restricted, TAuthorizationStatus.Denied:
    begin
      RecordButton.Enabled := False;
      Memo.Lines.Add('Not Authorized!');
    end;
  end;
end;

procedure TfrmMain.SpeechRecordingHandler(Sender: TObject; const IsRecording: Boolean);
const
  cRecordCaptions: array[Boolean] of string = ('Record', 'Stop');
begin
  RecordButton.Text := cRecordCaptions[IsRecording];
  if IsRecording then
    MessageLabel.Text := 'Now say something!'
  else
    DoStopped;
end;

procedure TfrmMain.SpeechStoppedHandler(Sender: TObject);
begin
  // Show text at the end, if not using partial results
  if not FSpeech.WantPartialResults then
    Memo.Lines.Text := FText;
  FText := '';
end;

procedure TfrmMain.SpeechTextHandler(Sender: TObject; const AText: string);
begin
  FText := AText;
  // Show text immediately, if using partial results
  if FSpeech.WantPartialResults then
    Memo.Lines.Text := FText;
end;

procedure TfrmMain.RecordButtonClick(Sender: TObject);
begin
  if FSpeech.IsRecording then
    FSpeech.StopRecording
  else
  begin
    FText := '';
    FSpeech.StartRecording;
  end;
end;

end.
