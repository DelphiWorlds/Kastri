unit DW.TextToSpeech.Android;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  {$IF RTLVersion >= 31} Androidapi.JNI.Speech,{$ELSE} Androidapi.JNI.GraphicsContentViewText, {$ENDIF}
  // DW
  DW.TextToSpeech;

type
  TPlatformTextToSpeech = class;

  TInitListener = class(TJavaLocal, JTextToSpeech_OnInitListener)
  private
    FPlatformTextToSpeech: TPlatformTextToSpeech;
  public
    { JTextToSpeech_OnInitListener }
    procedure onInit(status: Integer); cdecl;
  public
    constructor Create(const APlatformTextToSpeech: TPlatformTextToSpeech);
  end;

  TUtteranceCompletedListener = class(TJavaLocal, JTextToSpeech_OnUtteranceCompletedListener)
  private
    FPlatformTextToSpeech: TPlatformTextToSpeech;
  public
    { JTextToSpeech_OnUtteranceCompletedListener }
    procedure onUtteranceCompleted(utteranceId: JString); cdecl;
  public
    constructor Create(const APlatformTextToSpeech: TPlatformTextToSpeech);
  end;

  TPlatformTextToSpeech = class(TCustomPlatformTextToSpeech)
  private
    FInitListener: JTextToSpeech_OnInitListener;
    FTextToSpeech: JTextToSpeech;
    FParams: JHashMap;
    FSpeechStarted: Boolean;
    FUtteranceCompletedListener: JTextToSpeech_OnUtteranceCompletedListener;
  protected
    procedure Init(const AStatus: Integer);
    function IsSpeaking: Boolean; override;
    function Speak(const AText: String): Boolean; override;
    procedure Stop; override;
    procedure UtteranceCompleted(const AUtteranceId: JString);
  public
    constructor Create(const ATextToSpeech: TTextToSpeech); override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers,
  // DW
  DW.OSLog;

{ TInitListener }

constructor TInitListener.Create(const APlatformTextToSpeech: TPlatformTextToSpeech);
begin
  inherited Create;
  FPlatformTextToSpeech := APlatformTextToSpeech;
end;

procedure TInitListener.onInit(status: Integer);
begin
  FPlatformTextToSpeech.Init(status);
end;

{ TUtteranceCompletedListener }

constructor TUtteranceCompletedListener.Create(const APlatformTextToSpeech: TPlatformTextToSpeech);
begin
  inherited Create;
  FPlatformTextToSpeech := APlatformTextToSpeech;
end;

procedure TUtteranceCompletedListener.onUtteranceCompleted(utteranceId: JString);
begin
  FPlatformTextToSpeech.UtteranceCompleted(utteranceId);
end;

{ TPlatformTextToSpeech }

constructor TPlatformTextToSpeech.Create(const ATextToSpeech: TTextToSpeech);
begin
  inherited;
  FUtteranceCompletedListener := TUtteranceCompletedListener.Create(Self);
  FInitListener := TInitListener.Create(Self);
  FTextToSpeech := TJTextToSpeech.JavaClass.init(TAndroidHelper.Context, FInitListener);
end;

procedure TPlatformTextToSpeech.Init(const AStatus: Integer);
begin
  if AStatus = TJTextToSpeech.JavaClass.SUCCESS then
  begin
    CanSpeak := True;
    FParams := TJHashMap.Create;
    FParams.put(TJTextToSpeech_Engine.JavaClass.KEY_PARAM_UTTERANCE_ID, StringToJString('DWUtteranceId'));
    FTextToSpeech.setOnUtteranceCompletedListener(FUtteranceCompletedListener);
  end;
end;

function TPlatformTextToSpeech.IsSpeaking: Boolean;
begin
  Result := FSpeechStarted;
end;

function TPlatformTextToSpeech.Speak(const AText: string): Boolean;
var
  LLocale: JLocale;
  LLanguageResult: Integer;
begin
  LLocale := nil;
  if not Language.IsEmpty then
    LLocale := TJLocale.JavaClass.forLanguageTag(StringToJString(Language));
  if LLocale = nil then
    LLocale := TJLocale.JavaClass.getDefault;
  LLanguageResult := FTextToSpeech.setLanguage(LLocale);
  if LLanguageResult = TJTextToSpeech.JavaClass.LANG_MISSING_DATA then
    TOSLog.w('No language data for %s', [Language])
  else if LLanguageResult = TJTextToSpeech.JavaClass.LANG_MISSING_DATA then
    TOSLog.w('Language %s in not supported', [Language]);
  Result := FTextToSpeech.speak(StringToJString(AText), TJTextToSpeech.JavaClass.QUEUE_FLUSH, FParams) = TJTextToSpeech.JavaClass.SUCCESS;
  if Result then
    DoSpeechStarted;
end;

procedure TPlatformTextToSpeech.Stop;
begin
  FTextToSpeech.stop;
end;

procedure TPlatformTextToSpeech.UtteranceCompleted(const AUtteranceId: JString);
begin
  DoSpeechFinished;
end;

end.
