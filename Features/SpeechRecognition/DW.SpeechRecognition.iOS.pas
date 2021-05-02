unit DW.SpeechRecognition.iOS;

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
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.Foundation, iOSapi.AVFoundation,
  // DW
  DW.iOSapi.Speech, DW.iOSapi.AVFoundation, DW.SpeechRecognition, DW.Types;

type
  IMacOSTimer = interface(IObjectiveC)
    ['{C3817E03-699C-4ACC-9D2E-E88CA6763B09}']
    procedure timerEvent(timer: NSTimer); cdecl;
  end;

  TMacOSTimer = class(TOCLocal, IMacOSTimer)
  private
    FInterval: Integer;
    FIsEnabled: Boolean;
    FTimer: NSTimer;
    FTimerProc: TProc;
    procedure CreateTimer;
    procedure DestroyTimer;
    procedure SetInterval(const AValue: Integer);
    procedure SetIsEnabled(const AValue: Boolean);
  public
    { IMacOSTimer }
    procedure timerEvent(timer: NSTimer); cdecl;
  public
    constructor Create(const ATimerProc: TProc; const AInterval: Integer = 1000; const AEnable: Boolean = False);
    destructor Destroy; override;
    procedure Restart; overload;
    procedure Restart(const AInterval: Integer); overload;
    property Interval: Integer read FInterval write SetInterval;
    property IsEnabled: Boolean read FIsEnabled write SetIsEnabled;
  end;

  TPlatformSpeechRecognition = class(TCustomPlatformSpeechRecognition)
  private
    FAudioEngine: AVAudioEngine;
    FInputNode: AVAudioInputNode;
    FIsRecording: Boolean;
    FIsRecordingPending: Boolean;
    FIsStopped: Boolean;
    FRecognizer: SFSpeechRecognizer;
    FRequest: SFSpeechAudioBufferRecognitionRequest;
    FTask: SFSpeechRecognitionTask;
    FTimer: TMacOSTimer;
    procedure DoStartRecording;
    function GetRecordAuthorizationStatus(const APlatformStatus: AVAuthorizationStatus): TAuthorizationStatus;
    function GetSpeechAuthorizationStatus(const APlatformStatus: SFSpeechRecognizerAuthorizationStatus): TAuthorizationStatus;
    procedure InputNodeInstallTapOnBusHandler(buffer: AVAudioPCMBuffer; when: AVAudioTime);
    procedure QueueAuthorizationStatus(const AStatus: TAuthorizationStatus);
    procedure QueueRecordingStatusChanged;
    procedure RecognitionRequestSpeechResultHandler(result: SFSpeechRecognitionResult; error: NSError);
    procedure RequestAuthorization;
    procedure RequestAuthorizationHandler(status: SFSpeechRecognizerAuthorizationStatus);
    procedure RequestRecordAudio;
    procedure RequestRecordAudioHandler(granted: Boolean);
    procedure TimerHandler;
  protected
    function IsRecording: Boolean; override;
    procedure RequestPermission; override;
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
  DW.OSLog,
  // RTL
  System.Classes,
  // macOS
  Macapi.ObjCRuntime, Macapi.Helpers;

function AVMediaTypeAudio: NSString;
begin
  Result := CocoaNSStringConst(libAVFoundation, 'AVMediaTypeAudio');
end;

{ TMacOSTimer }

constructor TMacOSTimer.Create(const ATimerProc: TProc; const AInterval: Integer = 1000; const AEnable: Boolean = False);
begin
  inherited Create;
  FTimerProc := ATimerProc;
  FInterval := AInterval;
  IsEnabled := AEnable;
end;

destructor TMacOSTimer.Destroy;
begin
  DestroyTimer;
  inherited;
end;

procedure TMacOSTimer.CreateTimer;
begin
  if FInterval > 0 then
  begin
    FTimer := TNSTimer.Wrap(TNSTimer.OCClass.scheduledTimerWithTimeInterval(FInterval / 1000, GetObjectID, sel_getUid('timerEvent:'), nil, True));
    FIsEnabled := True;
  end;
end;

procedure TMacOSTimer.DestroyTimer;
begin
  if FTimer <> nil then
    FTimer.invalidate;
  FTimer := nil;
  FIsEnabled := False;
end;

procedure TMacOSTimer.Restart(const AInterval: Integer);
begin
  DestroyTimer;
  Interval := AInterval;
  CreateTimer;
end;

procedure TMacOSTimer.Restart;
begin
  DestroyTimer;
  CreateTimer;
end;

procedure TMacOSTimer.SetInterval(const AValue: Integer);
var
  LWasEnabled: Boolean;
begin
  if AValue <> FInterval then
  begin
    LWasEnabled := IsEnabled;
    IsEnabled := False;
    FInterval := AValue;
    IsEnabled := LWasEnabled;
  end;
end;

procedure TMacOSTimer.SetIsEnabled(const AValue: Boolean);
begin
  if AValue <> FIsEnabled then
  begin
    if AValue then
      CreateTimer
    else
      DestroyTimer;
  end
end;

procedure TMacOSTimer.timerEvent(timer: NSTimer);
begin
  if FIsEnabled and Assigned(FTimerProc) then
    FTimerProc;
end;

{ TPlatformSpeechRecognition }

constructor TPlatformSpeechRecognition.Create(const ASpeech: TSpeechRecognition);
var
  LSpeechStatus, LRecordStatus: TAuthorizationStatus;
begin
  inherited;
  FIsStopped := True;
  FTimer := TMacOSTimer.Create(TimerHandler);
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
  FTimer.Free;
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

procedure TPlatformSpeechRecognition.TimerHandler;
begin
  FTimer.IsEnabled := False;
  StopRecording;
  Stopped;
end;

procedure TPlatformSpeechRecognition.DoStartRecording;
var
  LAudioSession: AVAudioSession;
  LPointer: Pointer;
begin
  FIsRecordingPending := False;
  LPointer := nil;
  if FRecognizer = nil then
  begin
    FRecognizer := TSFSpeechRecognizer.Create;
    FRecognizer.initWithLocale(TNSLocale.Wrap(TNSLocale.OCClass.currentLocale));
  end;
  if FAudioEngine = nil then
    FAudioEngine := TAVAudioEngine.Create;
  LAudioSession := TAVAudioSession.Wrap(TAVAudioSession.OCClass.sharedInstance);
  LAudioSession.setCategoryError(AVAudioSessionCategoryRecord, @LPointer);
  // LAudioSession.setMode(AVAudioSessionModeMeasurement, @LPointer);
  // LAudioSession.setActiveWithOptionsError(True, AVAudioSessionSetActiveOptionNotifyOthersOnDeactivation, @LPointer);
  FRequest := TSFSpeechAudioBufferRecognitionRequest.Create;
  FInputNode := FAudioEngine.inputNode;
  // Check LRequest and LInputNode for nil?
  FRequest.setShouldReportPartialResults(True);
  FTask := FRecognizer.recognitionTaskWithRequest(FRequest, RecognitionRequestSpeechResultHandler);
  FInputNode.installTapOnBus(0, 4096, FInputNode.outputFormatForBus(0), InputNodeInstallTapOnBusHandler);
  FAudioEngine.prepare;
  FAudioEngine.startAndReturnError(@LPointer);
  FIsRecording := True;
  FIsStopped := False;
  QueueRecordingStatusChanged;
end;

procedure TPlatformSpeechRecognition.StartRecording;
begin
  FIsRecordingPending := False;
  if not Speech.IsAuthorized then
  begin
    FIsRecordingPending := True;
    RequestRecordAudio;
  end
  else
    DoStartRecording;
end;

procedure TPlatformSpeechRecognition.StopRecording;
begin
  if FIsStopped then
    Exit; // <=======
  if FAudioEngine.isRunning then
  begin
    FAudioEngine.stop;
    FRequest.endAudio;
  end;
  if FInputNode <> nil then
    FInputNode.removeTapOnBus(0);
  FRequest := nil;
  FTask := nil;
  FInputNode := nil;
  FIsRecording := False;
  FIsStopped := True;
  QueueRecordingStatusChanged;
end;

procedure TPlatformSpeechRecognition.InputNodeInstallTapOnBusHandler(buffer: AVAudioPCMBuffer; when: AVAudioTime);
begin
  FRequest.appendAudioPCMBuffer(buffer);
end;

function TPlatformSpeechRecognition.IsRecording: Boolean;
begin
  Result := FIsRecording;
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
  if FIsRecording and (error = nil) then
  begin
    if result <> nil then
    begin
      FTimer.Restart(Speech.StopInterval);
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
  if (error <> nil) or LFinished or not FIsRecording then
  begin
    FTimer.IsEnabled := False;
    StopRecording;
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
  if (LStatus = TAuthorizationStatus.Authorized) and FIsRecordingPending then
    DoStartRecording;
end;

procedure TPlatformSpeechRecognition.RequestPermission;
begin
  RequestRecordAudio;
end;

procedure TPlatformSpeechRecognition.RequestRecordAudio;
begin
  TAVCaptureDevice.OCClass.requestAccessForMediaType(AVMediaTypeAudio, RequestRecordAudioHandler);
end;

procedure TPlatformSpeechRecognition.RequestRecordAudioHandler(granted: Boolean);
begin
  if granted then
    RequestAuthorization
  else
    QueueAuthorizationStatus(TAuthorizationStatus.Denied);
end;

procedure TPlatformSpeechRecognition.QueueAuthorizationStatus(const AStatus: TAuthorizationStatus);
begin
  TThread.Queue(nil,
    procedure
    begin
      DoAuthorizationStatus(AStatus);
    end
  );
end;

procedure TPlatformSpeechRecognition.QueueRecordingStatusChanged;
begin
  TThread.Queue(nil, DoRecordingStatusChanged);
end;

end.
