unit DW.TextToSpeech.Win;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2021 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Win
  Winapi.Windows, Winapi.ActiveX,
  // DW
  DW.TextToSpeech;

const
  CLASS_SpVoice: TGUID = '{96749377-3391-11D2-9EE3-00C04F797396}';

type
  SPVISEMES = TOleEnum;
  SPVPRIORITY = TOleEnum;
  SPEVENTENUM = TOleEnum;

  {$ALIGN 8}
  PSPEVENT = ^SPEVENT;

  SPEVENT = record
    eEventId: Word;
    elParamType: Word;
    ulStreamNum: ULONG;
    ullAudioStreamOffset: ULONGLONG;
    wParam: WPARAM;
    lParam: LPARAM;
  end;

  {$ALIGN 8}
  PSPEVENTSOURCEINFO = ^SPEVENTSOURCEINFO;

  SPEVENTSOURCEINFO = record
    ullEventInterest: ULONGLONG;
    ullQueuedInterest: ULONGLONG;
    ulCount: ULONG;
  end;

  {$ALIGN 4}
  PSPVOICESTATUS = ^SPVOICESTATUS;

  SPVOICESTATUS = record
    ulCurrentStream: ULONG;
    ulLastStreamQueued: ULONG;
    hrLastResult: HResult;
    dwRunningState: LongWord;
    ulInputWordPos: ULONG;
    ulInputWordLen: ULONG;
    ulInputSentPos: ULONG;
    ulInputSentLen: ULONG;
    lBookmarkId: LONG;
    PhonemeId: WideChar;
    VisemeId: SPVISEMES;
    dwReserved1: LongWord;
    dwReserved2: LongWord;
  end;

  SPNOTIFYCALLBACK = procedure(wParam: WPARAM; lParam: LPARAM); stdcall;

  ISpNotifySink = interface(IUnknown)
    ['{259684DC-37C3-11D2-9603-00C04F8EE628}']
    function Notify: HResult; stdcall;
  end;

  ISpNotifySource = interface(IUnknown)
    ['{5EFF4AEF-8487-11D2-961C-00C04F8EE628}']
    function SetNotifySink(const pNotifySink: ISpNotifySink): HResult; stdcall;
    function SetNotifyWindowMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): HResult; stdcall;
    function SetNotifyCallbackFunction(pfnCallback: SPNOTIFYCALLBACK; wParam: WPARAM; lParam: LPARAM): HResult; stdcall;
    function SetNotifyCallbackInterface(pSpCallback: Pointer; wParam: WPARAM; lParam: LPARAM): HResult; stdcall;
    function SetNotifyWin32Event: HResult; stdcall;
    function WaitForNotifyEvent(dwMilliseconds: LongWord): HResult; stdcall;
    function GetNotifyEventHandle: Pointer; stdcall;
  end;

  ISpEventSource = interface(ISpNotifySource)
    ['{BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}']
    function SetInterest(ullEventInterest: ULONGLONG; ullQueuedInterest: ULONGLONG): HResult; stdcall;
    function GetEvents(ulCount: ULONG; pEventArray: PSPEVENT; out pulFetched: ULONG): HResult; stdcall;
    function GetInfo(out pInfo: SPEVENTSOURCEINFO): HResult; stdcall;
  end;

  ISpVoice = interface(ISpEventSource)
    ['{6C44DF74-72B9-4992-A1EC-EF996E0422D4}']
    function SetOutput(const pUnkOutput: IUnknown; fAllowFormatChanges: BOOL): HResult; stdcall;
    function GetOutputObjectToken(out ppObjectToken: IUnknown): HResult; stdcall;
    function GetOutputStream(out ppStream: IUnknown): HResult; stdcall;
    function Pause: HResult; stdcall;
    function Resume: HResult; stdcall;
    function SetVoice(const pToken: IUnknown): HResult; stdcall;
    function GetVoice(out ppToken: IUnknown): HResult; stdcall;
    function Speak(pwcs: LPCWSTR; dwFlags: LongWord; pulStreamNumber: PULONG): HResult; stdcall;
    function SpeakStream(const pStream: IUnknown; dwFlags: LongWord; out pulStreamNumber: LongWord): HResult; stdcall;
    function GetStatus(out pStatus: SPVOICESTATUS; ppszLastBookmark: PPWideChar): HResult; stdcall;
    function Skip(pItemType: LPCWSTR; lNumItems: Integer; out pulNumSkipped: ULONG): HResult; stdcall;
    function SetPriority(ePriority: SPVPRIORITY): HResult; stdcall;
    function GetPriority(out pePriority: SPVPRIORITY): HResult; stdcall;
    function SetAlertBoundary(eBoundary: SPEVENTENUM): HResult; stdcall;
    function GetAlertBoundary(out peBoundary: SPEVENTENUM): HResult; stdcall;
    function SetRate(RateAdjust: Integer): HResult; stdcall;
    function GetRate(out pRateAdjust: Integer): HResult; stdcall;
    function SetVolume(usVolume: Word): HResult; stdcall;
    function GetVolume(out pusVolume: Word): HResult; stdcall;
    function WaitUntilDone(msTimeout: LongWord): HResult; stdcall;
    function SetSyncSpeakTimeout(msTimeout: LongWord): HResult; stdcall;
    function GetSyncSpeakTimeout(out pmsTimeout: LongWord): HResult; stdcall;
    function SpeakCompleteEvent: Pointer; stdcall;
    function IsUISupported(pszTypeOfUI: PWideChar; pvExtraData: Pointer; cbExtraData: LongWord; out pfSupported: Integer): HResult; stdcall;
    function DisplayUI(hWndParent: HWND; pszTitle: PWideChar; pszTypeOfUI: PWideChar; pvExtraData: Pointer; cbExtraData: LongWord): HResult; stdcall;
  end;

const
  SPEI_UNDEFINED = $00000000;
  SPEI_START_INPUT_STREAM = $00000001;
  SPEI_END_INPUT_STREAM = $00000002;
  SPEI_VOICE_CHANGE = $00000003;
  SPEI_TTS_BOOKMARK = $00000004;
  SPEI_WORD_BOUNDARY = $00000005;
  SPEI_PHONEME = $00000006;
  SPEI_SENTENCE_BOUNDARY = $00000007;
  SPEI_VISEME = $00000008;
  SPEI_TTS_AUDIO_LEVEL = $00000009;
  SPEI_TTS_PRIVATE = $0000000F;
  SPEI_MIN_TTS = $00000001;
  SPEI_MAX_TTS = $0000000F;
  SPEI_END_SR_STREAM = $00000022;
  SPEI_SOUND_START = $00000023;
  SPEI_SOUND_END = $00000024;
  SPEI_PHRASE_START = $00000025;
  SPEI_RECOGNITION = $00000026;
  SPEI_HYPOTHESIS = $00000027;
  SPEI_SR_BOOKMARK = $00000028;
  SPEI_PROPERTY_NUM_CHANGE = $00000029;
  SPEI_PROPERTY_STRING_CHANGE = $0000002A;
  SPEI_FALSE_RECOGNITION = $0000002B;
  SPEI_INTERFERENCE = $0000002C;
  SPEI_REQUEST_UI = $0000002D;
  SPEI_RECO_STATE_CHANGE = $0000002E;
  SPEI_ADAPTATION = $0000002F;
  SPEI_START_SR_STREAM = $00000030;
  SPEI_RECO_OTHER_CONTEXT = $00000031;
  SPEI_SR_AUDIO_LEVEL = $00000032;
  SPEI_SR_RETAINEDAUDIO = $00000033;
  SPEI_SR_PRIVATE = $00000034;
  SPEI_ACTIVE_CATEGORY_CHANGED = $00000035;
  SPEI_RESERVED5 = $00000036;
  SPEI_RESERVED6 = $00000037;
  SPEI_MIN_SR = $00000022;
  SPEI_MAX_SR = $00000037;
  SPEI_RESERVED1 = $0000001E;
  SPEI_RESERVED2 = $00000021;
  SPEI_RESERVED3 = $0000003F;
  SPRS_DONE = 1 shl 0;
  SPRS_IS_SPEAKING = 1 shl 1;
  SPF_DEFAULT = 0;
  SPF_ASYNC = 1 shl 0;
  SPF_PURGEBEFORESPEAK = 1 shl 1;
  SPF_IS_FILENAME = 1 shl 2;
  SPF_IS_XML = 1 shl 3;
  SPF_IS_NOT_XML = 1 shl 4;
  SPF_PERSIST_XML = 1 shl 5;
  SPF_NLP_SPEAK_PUNC = 1 shl 6;
  SPF_PARSE_SAPI = 1 shl 7;
  SPF_PARSE_SSML = 1 shl 8;
  SPF_PARSE_AUTODETECT = 0;
  SPF_NLP_MASK = SPF_NLP_SPEAK_PUNC;
  SPF_PARSE_MASK = SPF_PARSE_SAPI or SPF_PARSE_SSML;
  SPF_VOICE_MASK = SPF_ASYNC or SPF_PURGEBEFORESPEAK or SPF_IS_FILENAME or SPF_IS_XML or SPF_IS_NOT_XML or SPF_NLP_MASK or SPF_PERSIST_XML
    or SPF_PARSE_MASK;
  SPF_UNUSED_FLAGS = not SPF_VOICE_MASK;

type
  TPlatformTextToSpeech = class(TCustomPlatformTextToSpeech)
  private
    class procedure VoiceCallback(wParam: WParam; lParam: LParam); stdcall; static;
  private
    FVoice: ISpVoice;
  protected
    function IsSpeaking: Boolean; override;
    function Speak(const AText: string): Boolean; override;
    procedure Stop; override;
    procedure VoiceEvent;
  public
    constructor Create(const ATextToSpeech: TTextToSpeech); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Win.ComObj;

const
  SPFEI_FLAGCHECK = (UInt64(1) shl SPEI_RESERVED1) or (UInt64(1) shl SPEI_RESERVED2);

function SPFEI(const AFlag: Longword): UInt64; inline;
begin
  Result := (UInt64(1) shl AFlag) or SPFEI_FLAGCHECK;
end;

{ TPlatformTextToSpeech }

constructor TPlatformTextToSpeech.Create;
var
  LEvents: ULONGLONG;
begin
  inherited;
  FVoice := CreateComObject(CLASS_SpVoice) as ISpVoice;
  if FVoice <> nil then
  begin
    LEvents := SPFEI(SPEI_START_INPUT_STREAM) or SPFEI(SPEI_END_INPUT_STREAM);
    OleCheck(FVoice.SetInterest(LEvents, LEvents));
    OleCheck(FVoice.SetNotifyCallbackFunction(VoiceCallback, 0, NativeInt(Self)));
    CanSpeak := True;
  end;
end;

destructor TPlatformTextToSpeech.Destroy;
begin
  if FVoice <> nil then
  begin
    FVoice.SetNotifyCallbackFunction(nil, 0, 0);
    FVoice.SetNotifySink(nil);
  end;
  inherited;
end;

class procedure TPlatformTextToSpeech.VoiceCallback(wParam: WParam; lParam: LParam);
begin
  TPlatformTextToSpeech(lParam).VoiceEvent;
end;

procedure TPlatformTextToSpeech.VoiceEvent;
var
  LEvent: SPEVENT;
  LFetched: ULONG;
begin
  if FVoice <> nil then
  begin
    FillChar(LEvent, SizeOf(LEvent), 0);
    while FVoice.GetEvents(1, @LEvent, LFetched) = S_OK do
    begin
      case LEvent.eEventId of
        SPEI_START_INPUT_STREAM:
          DoSpeechStarted;
        SPEI_END_INPUT_STREAM:
          DoSpeechFinished;
      end;
      FillChar(LEvent, SizeOf(LEvent), 0);
    end;
  end;
end;

function TPlatformTextToSpeech.IsSpeaking: Boolean;
var
  LStatus: SPVOICESTATUS;
begin
  if (FVoice <> nil) and (FVoice.GetStatus(LStatus, nil) = S_OK) then
    Result := ((LStatus.dwRunningState and SPRS_IS_SPEAKING) <> 0) and ((LStatus.dwRunningState and SPRS_DONE) = 0)
  else
    Result := False;
end;

function TPlatformTextToSpeech.Speak(const AText: String): Boolean;
begin
  if IsSpeaking then
    Stop;
  if FVoice <> nil then
    Result := FVoice.Speak(PWideChar(AText), SPF_ASYNC, nil) = S_OK
  else
    Result := False;
end;

procedure TPlatformTextToSpeech.Stop;
var
  LNumSkipped: ULONG;
begin
  if FVoice <> nil then
    FVoice.Skip('SENTENCE', MaxInt, LNumSkipped);
end;

end.
