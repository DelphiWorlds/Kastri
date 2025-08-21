unit DW.TextToSpeech.Mac;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // macOS
  Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.Foundation,
  // DW
  DW.TextToSpeech;

type
  // Redeclaring types from Macapi.AppKit because too much is missing or wrong
  NSSpeechSynthesizer = interface;
  NSSpeechSynthesizerDelegate = interface;

  NSSpeechSynthesizerVoiceName = NSString;
  NSSpeechPropertyKey = NSString;
  NSSpeechBoundary = NSInteger;
  NSSpeechMode = NSString;
  NSSpeechStatusKey = NSString;
  NSSpeechErrorKey = NSString;
  NSSpeechSynthesizerInfoKey = NSString;
  NSSpeechPhonemeInfoKey = NSString;
  NSSpeechCommandDelimiterKey = NSString;

  NSSpeechSynthesizerClass = interface(NSObjectClass)
    ['{6BFC028F-3893-471A-A84D-9D1AD7E975D4}']
    {class} function attributesForVoice(voice: NSSpeechSynthesizerVoiceName): NSDictionary; cdecl;
    {class} function availableVoices: NSArray; cdecl;
    {class} function defaultVoice: NSSpeechSynthesizerVoiceName; cdecl;
    {class} function isAnyApplicationSpeaking: Boolean; cdecl;
  end;

  NSSpeechSynthesizer = interface(NSObject)
    ['{23222A42-2003-4471-8DBF-B5834A3A8A2A}']
    procedure addSpeechDictionary(speechDictionary: NSDictionary); cdecl;
    procedure continueSpeaking; cdecl;
    function delegate: Pointer; cdecl;
    function initWithVoice(voice: NSSpeechSynthesizerVoiceName): Pointer; cdecl;
    function isSpeaking: Boolean; cdecl;
    function objectForProperty(&property: NSSpeechPropertyKey; error: PPointer): Pointer; cdecl;
    procedure pauseSpeakingAtBoundary(boundary: NSSpeechBoundary); cdecl;
    function phonemesFromText(text: NSString): NSString; cdecl;
    function rate: Single; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function setObject(&object: Pointer; forProperty: NSSpeechPropertyKey; error: PPointer): Boolean; cdecl;
    procedure setRate(rate: Single); cdecl;
    procedure setUsesFeedbackWindow(usesFeedbackWindow: Boolean); cdecl;
    function setVoice(voice: NSSpeechSynthesizerVoiceName): Boolean; cdecl;
    procedure setVolume(volume: Single); cdecl;
    function startSpeakingString(&string: NSString): Boolean; overload; cdecl;
    function startSpeakingString(&string: NSString; toURL: NSURL): Boolean; overload; cdecl;
    procedure stopSpeaking; cdecl;
    procedure stopSpeakingAtBoundary(boundary: NSSpeechBoundary); cdecl;
    function usesFeedbackWindow: Boolean; cdecl;
    function voice: NSSpeechSynthesizerVoiceName; cdecl;
    function volume: Single; cdecl;
  end;
  TNSSpeechSynthesizer = class(TOCGenericImport<NSSpeechSynthesizerClass, NSSpeechSynthesizer>) end;

  NSSpeechSynthesizerDelegate = interface(IObjectiveC)
    ['{AB1B2A52-290C-47FD-B0BA-2DD600FA6C52}']
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; didEncounterSyncMessage: NSString); overload; cdecl;
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; didEncounterErrorAtIndex: NSUInteger; ofString: NSString;
      message: NSString); overload; cdecl;
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; willSpeakPhoneme: Smallint); overload; cdecl;
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; willSpeakWord: NSRange; ofString: NSString); overload; cdecl;
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; didFinishSpeaking: Boolean); overload; cdecl;
  end;

  TPlatformTextToSpeech = class;

  TSpeechSynthesizerDelegate = class(TOCLocal, NSSpeechSynthesizerDelegate)
  private
    FPlatformTextToSpeech: TPlatformTextToSpeech;
  public
    { NSSpeechSynthesizerDelegate }
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; didEncounterSyncMessage: NSString); overload; cdecl;
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; didEncounterErrorAtIndex: NSUInteger; ofString: NSString;
      message: NSString); overload; cdecl;
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; willSpeakPhoneme: Smallint); overload; cdecl;
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; willSpeakWord: NSRange; ofString: NSString); overload; cdecl;
    procedure speechSynthesizer(sender: NSSpeechSynthesizer; didFinishSpeaking: Boolean); overload; cdecl;
  public
    constructor Create(const APlatformTextToSpeech: TPlatformTextToSpeech);
  end;

  TPlatformTextToSpeech = class(TCustomPlatformTextToSpeech)
  private
    FHasStartedSpeaking: Boolean;
    FSpeechSynthesizer: NSSpeechSynthesizer;
    FDelegate: TSpeechSynthesizerDelegate;
    function GetVoice: NSString;
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
  DW.OSLog,
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers;

const
  libAppKit = '/System/Library/Frameworks/AppKit.framework/AppKit';

type
  NSVoiceAttributeKey = NSString;

function NSVoiceLocaleIdentifier: NSVoiceAttributeKey;
begin
  Result := CocoaNSStringConst(libAppKit, 'NSVoiceLocaleIdentifier');
end;

{ TSpeechSynthesizerDelegate }

constructor TSpeechSynthesizerDelegate.Create(const APlatformTextToSpeech: TPlatformTextToSpeech);
begin
  inherited Create;
  FPlatformTextToSpeech := APlatformTextToSpeech;
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizer(sender: NSSpeechSynthesizer; didEncounterErrorAtIndex: NSUInteger; ofString,
  message: NSString);
begin
  // TODO
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizer(sender: NSSpeechSynthesizer; didEncounterSyncMessage: NSString);
begin
  // TODO
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizer(sender: NSSpeechSynthesizer; willSpeakPhoneme: Smallint);
begin
  FPlatformTextToSpeech.SpeakingStarted;
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizer(sender: NSSpeechSynthesizer; willSpeakWord: NSRange; ofString: NSString);
begin
  FPlatformTextToSpeech.SpeakingStarted;
end;

procedure TSpeechSynthesizerDelegate.speechSynthesizer(sender: NSSpeechSynthesizer; didFinishSpeaking: Boolean);
begin
  FPlatformTextToSpeech.SpeakingStopped;
end;

{ TPlatformTextToSpeech }

constructor TPlatformTextToSpeech.Create(const ATextToSpeech: TTextToSpeech);
begin
  inherited;
  FSpeechSynthesizer := TNSSpeechSynthesizer.Create;
  FDelegate := TSpeechSynthesizerDelegate.Create(Self);
  FSpeechSynthesizer.setDelegate(FDelegate.GetObjectID);
end;

destructor TPlatformTextToSpeech.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

function TPlatformTextToSpeech.GetVoice: NSString;
var
  LVoices: NSArray;
  I: Integer;
  LAttributes: NSDictionary;
  LVoiceName, LLocale: NSString;
begin
  Result := nil;
  LVoices := TNSSpeechSynthesizer.OCClass.availableVoices;
  for I := 0 to LVoices.count - 1 do
  begin
    LVoiceName := TNSString.Wrap(LVoices.objectAtIndex(I));
    LAttributes := TNSSpeechSynthesizer.OCClass.attributesForVoice(LVoiceName);
    LLocale := TNSString.Wrap(LAttributes.objectForKey(NSObjectToID(NSVoiceLocaleIdentifier)));
    if NSStrToStr(LLocale).Equals(Language.Replace('-', '_')) then
      Result := LVoiceName;
  end;
end;

function TPlatformTextToSpeech.IsSpeaking: Boolean;
begin
  Result := FSpeechSynthesizer.isSpeaking;
end;

function TPlatformTextToSpeech.Speak(const AText: String): Boolean;
begin
  FHasStartedSpeaking := False;
  if not Language.IsEmpty then
    FSpeechSynthesizer.setVoice(GetVoice)
  else
    FSpeechSynthesizer.setVoice(nil);
  Result := FSpeechSynthesizer.startSpeakingString(StrToNSStr(AText));
end;

procedure TPlatformTextToSpeech.Stop;
begin
  FSpeechSynthesizer.stopSpeaking;
end;

procedure TPlatformTextToSpeech.SpeakingStarted;
begin
  if not FHasStartedSpeaking then
  begin
    FHasStartedSpeaking := True;
    DoSpeechStarted;
  end;
end;

procedure TPlatformTextToSpeech.SpeakingStopped;
begin
  FHasStartedSpeaking := False;
  DoSpeechFinished;
end;

end.
