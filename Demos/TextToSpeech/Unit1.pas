unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  DW.TextToSpeech, FMX.ListBox;

type
  TForm1 = class(TForm)
    BottomLayout: TLayout;
    SpeakButton: TButton;
    StopButton: TButton;
    TextMemo: TMemo;
    LogMemo: TMemo;
    LangComboBox: TComboBox;
    LangLayout: TLayout;
    UseExampleCheckBox: TCheckBox;
    procedure SpeakButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure LangComboBoxChange(Sender: TObject);
    procedure UseExampleCheckBoxChange(Sender: TObject);
  private
    FSpeaker: TTextToSpeech;
    procedure SpeakerSpeechStartedHandler(Sender: TObject);
    procedure SpeakerSpeechFinishedHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

const
  cExampleText: array[0..3] of string = (
    'Now is the time for all good men to come to the aid of their country',
    'Il est maintenant temps pour tous les hommes de bien de venir en aide à leur pays',
    'Jetzt ist es an der Zeit, dass alle guten Männer ihrem Land zu Hilfe kommen',
    'Ora è il momento per tutti gli uomini buoni di venire in aiuto del loro paese'
  );

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FSpeaker := TTextToSpeech.Create;
  FSpeaker.OnSpeechStarted := SpeakerSpeechStartedHandler;
  FSpeaker.OnSpeechFinished := SpeakerSpeechFinishedHandler;
  TextMemo.Text := cExampleText[LangComboBox.ItemIndex];
end;

destructor TForm1.Destroy;
begin
  FSpeaker.Free;
  inherited;
end;

procedure TForm1.LangComboBoxChange(Sender: TObject);
begin
  FSpeaker.Language := LangComboBox.Items[LangComboBox.ItemIndex];
  if UseExampleCheckBox.IsChecked then
    TextMemo.Text := cExampleText[LangComboBox.ItemIndex];
end;

procedure TForm1.SpeakButtonClick(Sender: TObject);
begin
  FSpeaker.Speak(TextMemo.Text);
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  FSpeaker.Stop;
end;

procedure TForm1.UseExampleCheckBoxChange(Sender: TObject);
begin
  if not UseExampleCheckBox.IsChecked then
    TextMemo.Text := '';
end;

procedure TForm1.SpeakerSpeechFinishedHandler(Sender: TObject);
begin
  LogMemo.Lines.Add('Speaking finished');
end;

procedure TForm1.SpeakerSpeechStartedHandler(Sender: TObject);
begin
  LogMemo.Lines.Add('Speaking started');
end;

end.
