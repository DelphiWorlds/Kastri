unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  DW.TextToSpeech;

type
  TForm1 = class(TForm)
    BottomLayout: TLayout;
    SpeakButton: TButton;
    StopButton: TButton;
    TextMemo: TMemo;
    LogMemo: TMemo;
    procedure SpeakButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
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

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FSpeaker := TTextToSpeech.Create;
  FSpeaker.OnSpeechStarted := SpeakerSpeechStartedHandler;
  FSpeaker.OnSpeechFinished := SpeakerSpeechFinishedHandler;
end;

destructor TForm1.Destroy;
begin
  FSpeaker.Free;
  inherited;
end;

procedure TForm1.SpeakButtonClick(Sender: TObject);
begin
  FSpeaker.Speak(TextMemo.Text);
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  FSpeaker.Stop;
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
