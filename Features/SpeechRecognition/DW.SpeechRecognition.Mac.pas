unit DW.SpeechRecognition.Mac;

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
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.Foundation, Macapi.AVFoundation,
  // DW
  DW.Macapi.Speech, DW.Macapi.AVFoundation,
  DW.SpeechRecognition.Cocoa, DW.SpeechRecognition, DW.Types;

type
  TPlatformSpeechRecognition = class(TCocoaSpeechRecognition)
  private
    FAudioEngine: AVAudioEngine;
    FInputNode: AVAudioInputNode;
    FRecognizer: SFSpeechRecognizer;
    FRequest: SFSpeechAudioBufferRecognitionRequest;
    FTask: SFSpeechRecognitionTask;
    function GetRecordAuthorizationStatus(const APlatformStatus: AVAuthorizationStatus): TAuthorizationStatus;
    function GetSpeechAuthorizationStatus(const APlatformStatus: SFSpeechRecognizerAuthorizationStatus): TAuthorizationStatus;
    procedure InputNodeInstallTapOnBusHandler(buffer: AVAudioPCMBuffer; when: AVAudioTime);
    procedure RecognitionRequestSpeechResultHandler(result: SFSpeechRecognitionResult; error: NSError);
    procedure RequestAuthorizationHandler(status: SFSpeechRecognizerAuthorizationStatus);
  protected
    procedure DoStartRecording; override;
    procedure RequestAuthorization; override;
    procedure RequestRecordAudio; override;
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
  System.Classes,
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers;

function AVMediaTypeAudio: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVMediaTypeAudio');
end;

{ TPlatformSpeechRecognition }

constructor TPlatformSpeechRecognition.Create(const ASpeech: TSpeechRecognition);
var
  LSpeechStatus, LRecordStatus: TAuthorizationStatus;
begin
  inherited;
  LRecordStatus := GetRecordAuthorizationStatus(TAVCaptureDevice.OCClass.authorizationStatusForMediaType(AVMediaTypeAudio));
  LSpeechStatus := GetSpeechAuthorizationStatus(TSFSpeechRecognizer.OCClass.authorizationStatus);
  if (LSpeechStatus = TAuthorizationStatus.Denied) or (LRecordStatus = TAuthorizationStatus.Denied) then
    DoAuthorizationStatus(TAuthorizationStatus.Denied)
  else if (LSpeechStatus = TAuthorizationStatus.Restricted) or (LRecordStatus = TAuthorizationStatus.Restricted) then
    DoAuthorizationStatus(TAuthorizationStatus.Restricted)
  else if (LSpeechStatus = TAuthorizationStatus.Authorized) and (LRecordStatus = TAuthorizationStatus.Authorized) then
    DoAuthorizationStatus(TAuthorizationStatus.Authorized)
  else if (LSpeechStatus = TAuthorizationStatus.NotDetermined) and (LRecordStatus = TAuthorizationStatus.NotDetermined) then
    DoAuthorizationStatus(TAuthorizationStatus.NotDetermined);
end;

destructor TPlatformSpeechRecognition.Destroy;
begin
  if FRecognizer <> nil then
    FRecognizer.release;
  FRecognizer := nil;
  if FAudioEngine <> nil then
    FAudioEngine.release;
  FAudioEngine := nil;
  inherited;
end;

function TPlatformSpeechRecognition.GetRecordAuthorizationStatus(const APlatformStatus: AVAuthorizationStatus): TAuthorizationStatus;
begin
  case APlatformStatus of
    AVAuthorizationStatusAuthorized:
      Result := TAuthorizationStatus.Authorized;
    AVAuthorizationStatusDenied:
      Result := TAuthorizationStatus.Denied;
    AVAuthorizationStatusRestricted:
      Result := TAuthorizationStatus.Restricted;
    AVAuthorizationStatusNotDetermined:
      Result := TAuthorizationStatus.NotDetermined;
  else
    Result := TAuthorizationStatus.NotDetermined;
  end;
end;

function TPlatformSpeechRecognition.GetSpeechAuthorizationStatus(const APlatformStatus: SFSpeechRecognizerAuthorizationStatus): TAuthorizationStatus;
begin
  case APlatformStatus of
    SFSpeechRecognizerAuthorizationStatusAuthorized:
      Result := TAuthorizationStatus.Authorized;
    SFSpeechRecognizerAuthorizationStatusDenied:
      Result := TAuthorizationStatus.Denied;
    SFSpeechRecognizerAuthorizationStatusRestricted:
      Result := TAuthorizationStatus.Restricted;
    SFSpeechRecognizerAuthorizationStatusNotDetermined:
      Result := TAuthorizationStatus.NotDetermined;
  else
    Result := TAuthorizationStatus.NotDetermined;
  end;
end;

procedure TPlatformSpeechRecognition.DoStartRecording;
var
  LAudioSession: AVAudioSession;
  LLocale: NSLocale;
  LPointer: Pointer;
begin
  LPointer := nil;
  if FRecognizer = nil then
  begin
    FRecognizer := TSFSpeechRecognizer.Create;
    LLocale := nil;
    if not Speech.Language.IsEmpty then
      LLocale := TNSLocale.Wrap(TNSLocale.OCClass.localeWithLocaleIdentifier(StrToNSStr(Speech.Language)));
    if LLocale = nil then
      LLocale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
    FRecognizer.initWithLocale(LLocale);
  end;
  if FAudioEngine = nil then
    FAudioEngine := TAVAudioEngine.Create;
  LAudioSession := TAVAudioSession.OCClass.sharedInstance;
  LAudioSession.setCategory(AVAudioSessionCategoryRecord,  @LPointer);
  // LAudioSession.setMode(AVAudioSessionModeMeasurement, @LPointer);
  // LAudioSession.setActiveWithOptionsError(True, AVAudioSessionSetActiveOptionNotifyOthersOnDeactivation, @LPointer);
  FRequest := TSFSpeechAudioBufferRecognitionRequest.Create;
  FInputNode := FAudioEngine.inputNode;
  // Check LRequest and LInputNode for nil?
  FRequest.setShouldReportPartialResults(Speech.WantPartialResults);
  FTask := FRecognizer.recognitionTaskWithRequest(FRequest, RecognitionRequestSpeechResultHandler);
  FInputNode.installTapOnBus(0, 4096, FInputNode.outputFormatForBus(0), InputNodeInstallTapOnBusHandler);
  FAudioEngine.prepare;
  FAudioEngine.startAndReturnError;
  StartedRecording;
end;

procedure TPlatformSpeechRecognition.StopRecording;
var
  LWasRunning: Boolean;
begin
  LWasRunning := FAudioEngine.isRunning;
  if LWasRunning then
  begin
    FAudioEngine.stop;
    FRequest.endAudio;
  end;
  if FInputNode <> nil then
    FInputNode.removeTapOnBus(0);
  FRequest := nil;
  FTask := nil;
  FInputNode := nil;
  if LWasRunning then
    StoppedRecording;
end;

procedure TPlatformSpeechRecognition.InputNodeInstallTapOnBusHandler(buffer: AVAudioPCMBuffer; when: AVAudioTime);
begin
  FRequest.appendAudioPCMBuffer(buffer);
end;

class function TPlatformSpeechRecognition.IsSupported: Boolean;
begin
  Result := TOSVersion.Check(10);
end;

procedure TPlatformSpeechRecognition.RecognitionRequestSpeechResultHandler(result: SFSpeechRecognitionResult; error: NSError);
var
  LText: string;
  LFinished: Boolean;
begin
  LFinished := False;
  if IsRecording and (error = nil) then
  begin
    if result <> nil then
    begin
      Timer.Restart(Speech.StopInterval);
      LText := NSStrToStr(result.bestTranscription.formattedString);
      LFinished := result.isFinal;
      TThread.Queue(nil,
        procedure
        begin
          DoRecognition(LText, False);
        end
      );
    end;
  end;
  if (error <> nil) or LFinished or not IsRecording then
  begin
    Timer.IsEnabled := False;
    Finished;
  end;
end;

procedure TPlatformSpeechRecognition.RequestAuthorization;
begin
  TSFSpeechRecognizer.OCClass.requestAuthorization(RequestAuthorizationHandler);
end;

procedure TPlatformSpeechRecognition.RequestAuthorizationHandler(status: SFSpeechRecognizerAuthorizationStatus);
var
  LStatus: TAuthorizationStatus;
begin
  LStatus := GetSpeechAuthorizationStatus(status);
  QueueAuthorizationStatus(LStatus);
  if (LStatus = TAuthorizationStatus.Authorized) and IsRecordingPending then
    DoStartRecording;
end;

procedure TPlatformSpeechRecognition.RequestRecordAudio;
begin
  TAVCaptureDevice.OCClass.requestAccessForMediaType(AVMediaTypeAudio, RequestRecordAudioHandler);
end;

end.
