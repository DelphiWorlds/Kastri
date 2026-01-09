unit DW.TextToSpeech.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.AVFoundation, iOSapi.CoreAudio,
  // DW
  DW.iOSapi.AVFAudio, DW.TextToSpeech;

type
  TPlatformTextToSpeech = class;

  TSpeechSynthesizerDelegate = class(TOCLocal, AVSpeechSynthesizerDelegate)
  private
    FPlatformTextToSpeech: TPlatformTextToSpeech;
  public
    { AVSpeechSynthesizerDelegate }
    [MethodName('speechSynthesizer:didCancelSpeechUtterance:')]
    procedure speechSynthesizerDidCancelSpeechUtterance(synthesizer: AVSpeechSynthesizer; didCancelSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didContinueSpeechUtterance:')]
    procedure speechSynthesizerDidContinueSpeechUtterance(synthesizer: AVSpeechSynthesizer; didContinueSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didFinishSpeechUtterance:')]
    procedure speechSynthesizerDidFinishSpeechUtterance(synthesizer: AVSpeechSynthesizer; didFinishSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didPauseSpeechUtterance:')]
    procedure speechSynthesizerDidPauseSpeechUtterance(synthesizer: AVSpeechSynthesizer; didPauseSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:didStartSpeechUtterance:')]
    procedure speechSynthesizerDidStartSpeechUtterance(synthesizer: AVSpeechSynthesizer; didStartSpeechUtterance: AVSpeechUtterance); cdecl;
    [MethodName('speechSynthesizer:willSpeakRangeOfSpeechString:utterance:')]
    procedure speechSynthesizerWillSpeakRangeOfSpeechString(synthesizer: AVSpeechSynthesizer; willSpeakRangeOfSpeechString: NSRange;
      utterance: AVSpeechUtterance); cdecl;
  public
    constructor Create(const APlatformTextToSpeech: TPlatformTextToSpeech);
  end;

  TPlatformTextToSpeech = class(TCustomPlatformTextToSpeech)
  private
    FDelegate: TSpeechSynthesizerDelegate;
    FHasSpeakingStarted: Boolean;
    FSpeechSynthesizer: AVSpeechSynthesizer;
  protected
    function IsSpeaking: Boolean; override;
    function Speak(const AText: String): Boolean; override;
    procedure SpeakingStarted;
    procedure SpeakingStopped;
    procedure Stop; override;
  public
    constructor Create(const ATextToSpeech: TTextToSpeech); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers;

const
  AVSpeechBoundaryImmediate = 0;
  AVSpeechBoundaryWord = 1;

{ TSpeechSynthesizerDelegate }

constructor TSpeechSynthesizerDelegate.Create(const APlatformTextToSpeech: TPlatformTextToSpeech);
begin
  inherited Create;
  FPlatformTextToSpeech := APlatformTextToSpeech;
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizerDidCancelSpeechUtterance(synthesizer: AVSpeechSynthesizer;
  didCancelSpeechUtterance: AVSpeechUtterance);
begin
  FPlatformTextToSpeech.SpeakingStopped;
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizerDidContinueSpeechUtterance(synthesizer: AVSpeechSynthesizer;
  didContinueSpeechUtterance: AVSpeechUtterance);
begin
  //
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizerDidFinishSpeechUtterance(synthesizer: AVSpeechSynthesizer;
  didFinishSpeechUtterance: AVSpeechUtterance);
begin
  FPlatformTextToSpeech.SpeakingStopped;
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizerDidPauseSpeechUtterance(synthesizer: AVSpeechSynthesizer;
  didPauseSpeechUtterance: AVSpeechUtterance);
begin
  //
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizerDidStartSpeechUtterance(synthesizer: AVSpeechSynthesizer;
  didStartSpeechUtterance: AVSpeechUtterance);
begin
  FPlatformTextToSpeech.SpeakingStarted;
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizerWillSpeakRangeOfSpeechString(synthesizer: AVSpeechSynthesizer;
  willSpeakRangeOfSpeechString: NSRange; utterance: AVSpeechUtterance);
begin
  FPlatformTextToSpeech.SpeakingStarted;
end;

{ TPlatformTextToSpeech }

constructor TPlatformTextToSpeech.Create(const ATextToSpeech: TTextToSpeech);
begin
  inherited;
  FSpeechSynthesizer := TAVSpeechSynthesizer.Create;
  FDelegate := TSpeechSynthesizerDelegate.Create(Self);
  FSpeechSynthesizer.setDelegate(FDelegate.GetObjectID);
  CanSpeak := True;
end;

destructor TPlatformTextToSpeech.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

function TPlatformTextToSpeech.IsSpeaking: Boolean;
begin
  Result := FSpeechSynthesizer.isSpeaking;
end;

function TPlatformTextToSpeech.Speak(const AText: String): Boolean;
var
  LUtterance: AVSpeechUtterance;
begin
  Stop;
  FHasSpeakingStarted := False;
  LUtterance := TAVSpeechUtterance.Wrap(TAVSpeechUtterance.OCClass.speechUtteranceWithString(StrToNSStr(AText)));
  if not Language.IsEmpty then
    LUtterance.setVoice(TAVSpeechSynthesisVoice.OCClass.voiceWithLanguage(StrToNSStr(Language)));
  FSpeechSynthesizer.speakUtterance(LUtterance);
  Result := True;
end;

procedure TPlatformTextToSpeech.SpeakingStarted;
begin
  if not FHasSpeakingStarted then
  begin
    FHasSpeakingStarted := True;
    DoSpeechStarted;
  end;
end;

procedure TPlatformTextToSpeech.SpeakingStopped;
begin
  FHasSpeakingStarted := False;
  DoSpeechFinished;
end;

procedure TPlatformTextToSpeech.Stop;
begin
  if FSpeechSynthesizer.isSpeaking then
    FSpeechSynthesizer.stopSpeakingAtBoundary(AVSpeechBoundaryImmediate);
end;

end.
