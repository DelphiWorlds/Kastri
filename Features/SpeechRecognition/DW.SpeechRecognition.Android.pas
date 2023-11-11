unit DW.SpeechRecognition.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Messaging,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Speech, Androidapi.JNIBridge,  Androidapi.JNI.Os,
  // DW
  DW.SpeechRecognition;

type
  TPlatformSpeechRecognition = class;

  TSpeechRecognitionListener = class(TJavaLocal, JRecognitionListener)
  private
    FPlatformSpeech: TPlatformSpeechRecognition;
  public
    { JRecognitionListener }
    procedure onBeginningOfSpeech; cdecl;
    procedure onBufferReceived(buffer: TJavaArray<Byte>); cdecl;
    {$IF CompilerVersion > 35}
    procedure onEndOfSegmentedSession; cdecl;
    {$ENDIF}
    procedure onEndOfSpeech; cdecl;
    procedure onError(error: Integer); cdecl;
    procedure onEvent(eventType: Integer; params: JBundle); cdecl;
    procedure onPartialResults(partialResults: JBundle); cdecl;
    procedure onReadyForSpeech(params: JBundle); cdecl;
    procedure onResults(results: JBundle); cdecl;
    procedure onRmsChanged(rmsdB: Single); cdecl;
    {$IF CompilerVersion > 35}
    procedure onSegmentResults(segmentResults: JBundle); cdecl;
    {$ENDIF}
  public
    constructor Create(const APlatformSpeech: TPlatformSpeechRecognition);
  end;

  TPlatformSpeechRecognition = class(TCustomPlatformSpeechRecognition)
  private
    const cRequestCodeSpeechInput = 100;
  private
    FIsRecording: Boolean;
    FIsRecordingPending: Boolean;
    FListener: JRecognitionListener;
    FResultsText: string;
    FSpeechRecognizer: JSpeechRecognizer;
    FText: string;
    function CanRestart: Boolean;
    function IsTextTerminated: Boolean;
    procedure DoRestartRecording;
    procedure DoStartRecording;
    procedure RequestAuthorization;
    function TerminateText: Boolean;
  protected
    procedure BeginningOfSpeech;
    procedure BufferReceived(buffer: TJavaArray<Byte>);
    procedure EndOfSpeech;
    procedure EndOfSegmentedSession;
    procedure Error(error: Integer);
    procedure Event(eventType: Integer; params: JBundle);
    function IsRecording: Boolean; override;
    procedure PartialResults(partialResults: JBundle);
    procedure ReadyForSpeech(params: JBundle);
    procedure RequestPermission; override;
    procedure Results(results: JBundle);
    procedure RmsChanged(rmsdB: Single);
    procedure SegmentResults(segmentResults: JBundle);
    procedure StartRecording; override;
    procedure StopRecording; override;
  public
    class function IsSupported: Boolean;
  public
    constructor Create(const ASpeech: TSpeechRecognition); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.Permissions,
  // Android
  Androidapi.Helpers, Androidapi.JNI.App, Androidapi.JNI.JavaTypes,
  // DW
  DW.OSLog, DW.Consts.Android, DW.Types, DW.Permissions.Helpers;

{ TSpeechRecognitionListener }

constructor TSpeechRecognitionListener.Create(const APlatformSpeech: TPlatformSpeechRecognition);
begin
  inherited Create;
  FPlatformSpeech := APlatformSpeech;
end;

procedure TSpeechRecognitionListener.onBeginningOfSpeech;
begin
  FPlatformSpeech.BeginningOfSpeech;
end;

procedure TSpeechRecognitionListener.onBufferReceived(buffer: TJavaArray<Byte>);
begin
  FPlatformSpeech.BufferReceived(buffer);
end;

procedure TSpeechRecognitionListener.onEndOfSegmentedSession;
begin
  FPlatformSpeech.EndOfSegmentedSession;
end;

procedure TSpeechRecognitionListener.onEndOfSpeech;
begin
  FPlatformSpeech.EndOfSpeech;
end;

procedure TSpeechRecognitionListener.onError(error: Integer);
begin
  FPlatformSpeech.Error(error);
end;

procedure TSpeechRecognitionListener.onEvent(eventType: Integer; params: JBundle);
begin
  FPlatformSpeech.Event(eventType, params);
end;

procedure TSpeechRecognitionListener.onPartialResults(partialResults: JBundle);
begin
  FPlatformSpeech.PartialResults(partialResults);
end;

procedure TSpeechRecognitionListener.onReadyForSpeech(params: JBundle);
begin
  FPlatformSpeech.ReadyForSpeech(params);
end;

procedure TSpeechRecognitionListener.onResults(results: JBundle);
begin
  FPlatformSpeech.Results(results);
end;

procedure TSpeechRecognitionListener.onRmsChanged(rmsdB: Single);
begin
  FPlatformSpeech.RmsChanged(rmsDB);
end;

procedure TSpeechRecognitionListener.onSegmentResults(segmentResults: JBundle);
begin
  FPlatformSpeech.SegmentResults(segmentResults);
end;

{ TPlatformSpeechRecognition }

constructor TPlatformSpeechRecognition.Create(const ASpeech: TSpeechRecognition);
begin
  inherited;
  FListener := TSpeechRecognitionListener.Create(Self);
  FSpeechRecognizer := TJSpeechRecognizer.JavaClass.createSpeechRecognizer(TAndroidHelper.Context);
  FSpeechRecognizer.setRecognitionListener(FListener);
end;

destructor TPlatformSpeechRecognition.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformSpeechRecognition.RequestAuthorization;
begin
  PermissionsService.RequestPermissions([cPermissionRecordAudio],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      case AGrantResults[0] of
        TPermissionStatus.Granted:
        begin
          DoAuthorizationStatus(TAuthorizationStatus.Authorized);
          if FIsRecordingPending then
            DoStartRecording;
        end;
        TPermissionStatus.Denied:
          DoAuthorizationStatus(TAuthorizationStatus.Denied);
        {$IF CompilerVersion < 36}
        TPermissionStatus.PermanentlyDenied:
          DoAuthorizationStatus(TAuthorizationStatus.Restricted);
        {$ENDIF}
      end;
      FIsRecordingPending := False;
    end
  );
end;

procedure TPlatformSpeechRecognition.RequestPermission;
begin
  FIsRecordingPending := False;
  RequestAuthorization;
end;

procedure TPlatformSpeechRecognition.DoRestartRecording;
begin
  if TerminateText then
    DoRecognition(FText, False);
  // TOSLog.d('> Restarting with FText: %s', [FText]);
  DoStartRecording;
end;

procedure TPlatformSpeechRecognition.DoStartRecording;
var
  LIntent: JIntent;
begin
  LIntent := TJIntent.JavaClass.init(TJRecognizerIntent.JavaClass.ACTION_RECOGNIZE_SPEECH);
  LIntent.putExtra(TJRecognizerIntent.JavaClass.EXTRA_PARTIAL_RESULTS, Speech.WantPartialResults);
  LIntent.putExtra(TJRecognizerIntent.JavaClass.EXTRA_LANGUAGE_MODEL, TJRecognizerIntent.JavaClass.LANGUAGE_MODEL_FREE_FORM);
  if Speech.Language.IsEmpty then
    LIntent.putExtra(TJRecognizerIntent.JavaClass.EXTRA_LANGUAGE, TJLocale.JavaClass.getDefault.toString)
  else
    LIntent.putExtra(TJRecognizerIntent.JavaClass.EXTRA_LANGUAGE, StringToJString(Speech.Language));
  LIntent.putExtra(TJRecognizerIntent.JavaClass.EXTRA_PROMPT, StringToJString(Speech.Prompt));
  FSpeechRecognizer.startListening(LIntent);
end;

procedure TPlatformSpeechRecognition.SegmentResults(segmentResults: JBundle);
begin
  // Yet to be implemented
end;

procedure TPlatformSpeechRecognition.StartRecording;
begin
  FText := '';
  if not Speech.IsAuthorized then
  begin
    FIsRecordingPending := True;
    RequestAuthorization;
  end
  else
    DoStartRecording;
end;

procedure TPlatformSpeechRecognition.StopRecording;
begin
  FSpeechRecognizer.stopListening;
  FIsRecording := False;
  DoRecordingStatusChanged;
end;

procedure TPlatformSpeechRecognition.BeginningOfSpeech;
begin
  //
end;

procedure TPlatformSpeechRecognition.BufferReceived(buffer: TJavaArray<Byte>);
begin
  // Partial results?
end;

procedure TPlatformSpeechRecognition.EndOfSegmentedSession;
begin
  // Yet to be implemented
end;

procedure TPlatformSpeechRecognition.EndOfSpeech;
begin
  if Speech.StopInterval > 0 then
  begin
    FIsRecording := False;
    DoRecordingStatusChanged;
  end;
end;

procedure TPlatformSpeechRecognition.Error(error: Integer);
begin
  TOSLog.d('TPlatformSpeechRecognition.Error: %d', [error]);
  if (error = TJSpeechRecognizer.JavaClass.ERROR_NO_MATCH) and CanRestart then
    DoRestartRecording;
end;

procedure TPlatformSpeechRecognition.Event(eventType: Integer; params: JBundle);
begin
  //
end;

procedure TPlatformSpeechRecognition.ReadyForSpeech(params: JBundle);
begin
  FIsRecording := True;
  DoRecordingStatusChanged;
end;

procedure TPlatformSpeechRecognition.Results(results: JBundle);
var
  LArrayList: JArrayList;
  LResult: JString;
  I: Integer;
begin
  if not Speech.WantPartialResults then
  begin
    LArrayList := results.getStringArrayList(TJSpeechRecognizer.JavaClass.RESULTS_RECOGNITION);
    for I := 0 to LArrayList.size - 1 do
      LResult := TJString.Wrap(TAndroidHelper.JObjectToID(LArrayList.get(I)));
    if Speech.StopInterval > 0 then
    begin
      FIsRecording := False;
      DoRecordingStatusChanged;
    end;
    DoRecognition(JStringToString(LResult), True);
  end
  else if CanRestart then
  begin
    // https://github.com/DelphiWorlds/Kastri/issues/189
    FText := FText + FResultsText;
    DoStartRecording;
  end
  else
    Stopped;
end;

procedure TPlatformSpeechRecognition.RmsChanged(rmsdB: Single);
begin
  //
end;

function TPlatformSpeechRecognition.IsRecording: Boolean;
begin
  Result := FIsRecording;
end;

class function TPlatformSpeechRecognition.IsSupported: Boolean;
begin
  Result := TJSpeechRecognizer.JavaClass.isRecognitionAvailable(TAndroidHelper.Context);
end;

function TPlatformSpeechRecognition.IsTextTerminated: Boolean;
begin
  Result := not FText.IsEmpty and (FText.LastIndexOf('.') = Length(FText) - 1);
end;

function TPlatformSpeechRecognition.CanRestart: Boolean;
begin
  Result := FIsRecording and (Speech.StopInterval = 0);
end;

function TPlatformSpeechRecognition.TerminateText: Boolean;
begin
  Result := False;
  if not FText.IsEmpty and not IsTextTerminated then
  begin
    FText := FText + '.';
    Result := True;
  end;
end;

procedure TPlatformSpeechRecognition.PartialResults(partialResults: JBundle);
var
  LList: JArrayList;
begin
  if FResultsText.IsEmpty then
    TerminateText;
  LList := partialResults.getStringArrayList(TJSpeechRecognizer.JavaClass.RESULTS_RECOGNITION);
  FResultsText := JStringToString(TJString.Wrap(LList.get(0)));
  if not FResultsText.IsEmpty then
  begin
    if FText.EndsWith('.') then
      FText := FText + ' ';
    DoRecognition(FText + FResultsText, False);
  end;
end;

end.
