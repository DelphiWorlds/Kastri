unit DW.TextToSpeech;

interface

uses
  System.Classes;

type
  TTextToSpeech = class;

  TCustomPlatformTextToSpeech = class(TObject)
  private
    FCanSpeak: Boolean;
    FTextToSpeech: TTextToSpeech;
  protected
    procedure DoSpeechStarted;
    procedure DoSpeechFinished;
    function IsSpeaking: Boolean; virtual;
    function Speak(const AText: string): Boolean; virtual;
    procedure Stop; virtual;
    property CanSpeak: Boolean read FCanSpeak write FCanSpeak;
  public
    constructor Create(const ATextToSpeech: TTextToSpeech); virtual;
  end;

  TTextToSpeech = class(TObject)
  private
    FPlatformTextToSpeech: TCustomPlatformTextToSpeech;
    FOnCanSpeakChanged: TNotifyEvent;
    FOnSpeechStarted: TNotifyEvent;
    FOnSpeechFinished: TNotifyEvent;
  protected
    procedure DoCanSpeakChanged;
    procedure DoSpeechStarted;
    procedure DoSpeechFinished;
  public
    constructor Create;
    function CanSpeak: Boolean;
    function IsSpeaking: Boolean;
    function Speak(const AText: String): Boolean;
    procedure Stop;
    property OnSpeechStarted: TNotifyEvent read FOnSpeechStarted write FOnSpeechStarted;
    property OnSpeechFinished: TNotifyEvent read FOnSpeechFinished write FOnSpeechFinished;
  end;

implementation

uses
  {$IF Defined(IOS)}
  DW.TextToSpeech.iOS;
  {$ELSEIF Defined(ANDROID)}
  DW.TextToSpeech.Android;
  {$ELSEIF Defined(MSWINDOWS)}
  DW.TextToSpeech.Win;
  {$ELSEIF Defined(MACOS)}
  DW.TextToSpeech.Mac;
  {$ENDIF}

{ TCustomPlatformTextToSpeech }

constructor TCustomPlatformTextToSpeech.Create(const ATextToSpeech: TTextToSpeech);
begin
  inherited Create;
  FTextToSpeech := ATextToSpeech;
end;

procedure TCustomPlatformTextToSpeech.DoSpeechFinished;
begin
  FTextToSpeech.DoSpeechFinished;
end;

procedure TCustomPlatformTextToSpeech.DoSpeechStarted;
begin
  FTextToSpeech.DoSpeechStarted;
end;

function TCustomPlatformTextToSpeech.IsSpeaking: Boolean;
begin
  Result := False;
end;

function TCustomPlatformTextToSpeech.Speak(const AText: String): Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformTextToSpeech.Stop;
begin
  //
end;

{ TTextToSpeech }

constructor TTextToSpeech.Create;
begin
  inherited;
  FPlatformTextToSpeech := TPlatformTextToSpeech.Create(Self);
end;

function TTextToSpeech.CanSpeak: Boolean;
begin
  Result := FPlatformTextToSpeech.CanSpeak;
end;

procedure TTextToSpeech.DoCanSpeakChanged;
begin
  if Assigned(FOnCanSpeakChanged) then
    FOnCanSpeakChanged(Self);
end;

procedure TTextToSpeech.DoSpeechFinished;
begin
  if Assigned(FOnSpeechFinished) then
    FOnSpeechFinished(Self);
end;

procedure TTextToSpeech.DoSpeechStarted;
begin
  if Assigned(FOnSpeechStarted) then
    FOnSpeechStarted(Self);
end;

function TTextToSpeech.IsSpeaking: Boolean;
begin
  Result := FPlatformTextToSpeech.IsSpeaking;
end;

function TTextToSpeech.Speak(const AText: String): Boolean;
begin
  Result := FPlatformTextToSpeech.Speak(AText);
end;

procedure TTextToSpeech.Stop;
begin
  FPlatformTextToSpeech.Stop;
end;

end.
