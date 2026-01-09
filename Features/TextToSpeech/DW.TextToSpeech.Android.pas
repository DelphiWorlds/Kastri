unit DW.TextToSpeech.Android;

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
  // RTL
  System.Messaging,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Speech, Androidapi.JNI.GraphicsContentViewText,
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
    function InstallData: Boolean;
    procedure MessageResultNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
  protected
    function CheckData: Boolean; override;
    procedure Init(const AStatus: Integer);
    function IsSpeaking: Boolean; override;
    function Speak(const AText: String): Boolean; override;
    procedure Stop; override;
    procedure UtteranceCompleted(const AUtteranceId: JString);
  public
    constructor Create(const ATextToSpeech: TTextToSpeech); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App,
  // DW
  DW.OSLog;

const
  cCheckTTSDataRequestCode = 2222;

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
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, MessageResultNotificationMessageHandler);
  FUtteranceCompletedListener := TUtteranceCompletedListener.Create(Self);
  FInitListener := TInitListener.Create(Self);
  FTextToSpeech := TJTextToSpeech.JavaClass.init(TAndroidHelper.Context, FInitListener);
end;

destructor TPlatformTextToSpeech.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, MessageResultNotificationMessageHandler);
  inherited;
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

function TPlatformTextToSpeech.InstallData: Boolean;
var
  LIntent: JIntent;
  LResolveInfo: JResolveInfo;
begin
  Result := False;
  LIntent := TJIntent.JavaClass.init(TJTextToSpeech_Engine.JavaClass.ACTION_INSTALL_TTS_DATA);
  LResolveInfo := TAndroidHelper.Context.getPackageManager.resolveActivity(LIntent, TJPackageManager.JavaClass.MATCH_DEFAULT_ONLY);
  if LResolveInfo <> nil then
  begin
    TAndroidHelper.Context.startActivity(LIntent);
    Result := True;
  end;
end;

function TPlatformTextToSpeech.CheckData: Boolean;
var
  LIntent: JIntent;
  LResolveInfo: JResolveInfo;
begin
  Result := False;
  LIntent := TJIntent.JavaClass.init(TJTextToSpeech_Engine.JavaClass.ACTION_CHECK_TTS_DATA);
  LResolveInfo := TAndroidHelper.Context.getPackageManager.resolveActivity(LIntent, TJPackageManager.JavaClass.MATCH_DEFAULT_ONLY);
  if LResolveInfo <> nil then
  begin
    AvailableVoices := [];
    UnavailableVoices := [];
    TAndroidHelper.Activity.startActivityForResult(LIntent, cCheckTTSDataRequestCode);
    Result := True;
  end;
end;

procedure TPlatformTextToSpeech.MessageResultNotificationMessageHandler(const Sender: TObject; const AMsg: TMessage);
var
  LMessage: TMessageResultNotification;
  LList: JArrayList;
  LVoice: string;
  I: Integer;
begin
  LMessage := TMessageResultNotification(AMsg);
  if LMessage.RequestCode = cCheckTTSDataRequestCode then
  begin
    LList := LMessage.Value.getStringArrayListExtra(TJTextToSpeech_Engine.JavaClass.EXTRA_AVAILABLE_VOICES);
    if LList <> nil then
    begin
      for I := 0 to LList.size - 1 do
      begin
        LVoice := JStringToString(TJString.Wrap(TAndroidHelper.JObjectToID(LList.get(I)))).Trim;
        if not LVoice.IsEmpty then
          AvailableVoices := AvailableVoices + [LVoice];
      end;
    end;
    LList := LMessage.Value.getStringArrayListExtra(TJTextToSpeech_Engine.JavaClass.EXTRA_UNAVAILABLE_VOICES);
    if LList <> nil then
    begin
      for I := 0 to LList.size - 1 do
      begin
        LVoice := JStringToString(TJString.Wrap(TAndroidHelper.JObjectToID(LList.get(I)))).Trim;
        if not LVoice.IsEmpty then
          UnavailableVoices := UnavailableVoices + [LVoice];
      end;
    end;
    DoCheckDataComplete;
    if LMessage.ResultCode = TJTextToSpeech_Engine.JavaClass.CHECK_VOICE_DATA_PASS then
      InstallData;
    // Else notify that data is not available, or whatever
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
