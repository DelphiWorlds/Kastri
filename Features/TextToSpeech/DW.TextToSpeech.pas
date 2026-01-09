unit DW.TextToSpeech;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  System.Classes;

type
  TTextToSpeech = class;

  TCustomPlatformTextToSpeech = class(TObject)
  private
    FAvailableVoices: TArray<string>;
    FCanSpeak: Boolean;
    FLanguage: string;
    FTextToSpeech: TTextToSpeech;
    FUnavailableVoices: TArray<string>;
    procedure SetCanSpeak(const Value: Boolean);
  protected
    procedure DoCheckDataComplete;
    procedure DoSpeechStarted;
    procedure DoSpeechFinished;
    function CheckData: Boolean; virtual;
    function IsSpeaking: Boolean; virtual;
    function Speak(const AText: string): Boolean; virtual;
    procedure Stop; virtual;
    property AvailableVoices: TArray<string> read FAvailableVoices write FAvailableVoices;
    property CanSpeak: Boolean read FCanSpeak write SetCanSpeak;
    property Language: string read FLanguage write FLanguage;
    property UnavailableVoices: TArray<string> read FUnavailableVoices write FUnavailableVoices;
  public
    constructor Create(const ATextToSpeech: TTextToSpeech); virtual;
  end;

  TTextToSpeech = class(TObject)
  private
    FPlatformTextToSpeech: TCustomPlatformTextToSpeech;
    FOnCanSpeakChanged: TNotifyEvent;
    FOnCheckDataComplete: TNotifyEvent;
    FOnSpeechStarted: TNotifyEvent;
    FOnSpeechFinished: TNotifyEvent;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function GetAvailableVoices: TArray<string>;
    function GetUnavailableVoices: TArray<string>;
  protected
    procedure DoCanSpeakChanged;
    procedure DoCheckDataComplete;
    procedure DoSpeechStarted;
    procedure DoSpeechFinished;
  public
    constructor Create;
    /// <summary>
    ///   Indicates whether or not the text to speech engine is available
    /// </summary>
    function CanSpeak: Boolean;
    /// <summary>
    ///   Starts the activity responsible for checking TTS data, and if successful, starts the activity that installs the data.
    ///   Returns True if the activity is started.
    /// </summary>
    /// <remarks>
    ///   Applies to Android ONLY
    /// </remarks>
    function CheckData: Boolean;
    /// <summary>
    ///   Indicates whether or not the text to speech engine is in the process of speaking
    /// </summary>
    function IsSpeaking: Boolean;
    /// <summary>
    ///   Invokes the text to speech engine to utter the text in AText
    /// </summary>
    function Speak(const AText: string): Boolean;
    /// <summary>
    ///   Stops the text to speech engine from speaking
    /// </summary>
    procedure Stop;
    property AvailableVoices: TArray<string> read GetAvailableVoices;
    property UnavailableVoices: TArray<string> read GetUnavailableVoices;
    /// <summary>
    ///   The language being used when Speak is called
    /// </summary>
    property Language: string read GetLanguage write SetLanguage;
    property OnCanSpeakChanged: TNotifyEvent read FOnCanSpeakChanged write FOnCanSpeakChanged;
    property OnCheckDataComplete: TNotifyEvent read FOnCheckDataComplete write FOnCheckDataComplete;
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

procedure TCustomPlatformTextToSpeech.DoCheckDataComplete;
begin
  FTextToSpeech.DoCheckDataComplete;
end;

procedure TCustomPlatformTextToSpeech.DoSpeechFinished;
begin
  FTextToSpeech.DoSpeechFinished;
end;

procedure TCustomPlatformTextToSpeech.DoSpeechStarted;
begin
  FTextToSpeech.DoSpeechStarted;
end;

function TCustomPlatformTextToSpeech.CheckData: Boolean;
begin
  Result := False;
end;

function TCustomPlatformTextToSpeech.IsSpeaking: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformTextToSpeech.SetCanSpeak(const Value: Boolean);
begin
  if Value <> FCanSpeak then
  begin
    FCanSpeak := Value;
    FTextToSpeech.DoCanSpeakChanged;
  end;
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

procedure TTextToSpeech.DoCheckDataComplete;
begin
  if Assigned(FOnCheckDataComplete) then
    FOnCheckDataComplete(Self);
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

function TTextToSpeech.GetAvailableVoices: TArray<string>;
begin
  Result := FPlatformTextToSpeech.AvailableVoices;
end;

function TTextToSpeech.GetLanguage: string;
begin
  Result := FPlatformTextToSpeech.Language;
end;

function TTextToSpeech.GetUnavailableVoices: TArray<string>;
begin
  Result := FPlatformTextToSpeech.UnavailableVoices;
end;

function TTextToSpeech.CheckData: Boolean;
begin
  Result := FPlatformTextToSpeech.CheckData;
end;

function TTextToSpeech.IsSpeaking: Boolean;
begin
  Result := FPlatformTextToSpeech.IsSpeaking;
end;

procedure TTextToSpeech.SetLanguage(const Value: string);
begin
  FPlatformTextToSpeech.Language := Value;
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
