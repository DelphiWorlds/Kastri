unit DW.SpeechRecognition.Cocoa;

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
  // RTL
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC,
  {$IF Defined(OSX)}
  Macapi.Foundation,
  {$ENDIF}
  {$IF Defined(IOS)}
  // iOS
  iOSapi.Foundation,
  {$ENDIF}
  // DW
  DW.Types, DW.SpeechRecognition;

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

  TCocoaSpeechRecognition = class(TCustomPlatformSpeechRecognition)
  private
    FIsFinished: Boolean;
    FIsRecording: Boolean;
    FIsRecordingPending: Boolean;
    FIsStopped: Boolean;
    FTimer: TMacOSTimer;
  protected
    procedure DoStartRecording; virtual;
    procedure Finished;
    function IsRecording: Boolean; override;
    procedure QueueAuthorizationStatus(const AStatus: TAuthorizationStatus);
    procedure QueueRecordingStatusChanged;
    procedure RequestAuthorization; virtual;
    procedure RequestPermission; override;
    procedure RequestRecordAudio; virtual;
    procedure RequestRecordAudioHandler(granted: Boolean);
    procedure StartRecording; override;
    procedure StartedRecording;
    procedure StoppedRecording;
    procedure TimerHandler;
    property IsRecordingPending: Boolean read FIsRecordingPending;
    property IsFinished: Boolean read FIsFinished;
    property IsStopped: Boolean read FIsStopped;
    property Timer: TMacOSTimer read FTimer;
  public
    constructor Create(const ASpeech: TSpeechRecognition); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Classes,
  // macOS
  Macapi.ObjCRuntime;

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

{ TCocoaSpeechRecognition }

constructor TCocoaSpeechRecognition.Create(const ASpeech: TSpeechRecognition);
begin
  inherited;
  FIsStopped := True;
  FTimer := TMacOSTimer.Create(TimerHandler);
end;

destructor TCocoaSpeechRecognition.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TCocoaSpeechRecognition.DoStartRecording;
begin
  //
end;

procedure TCocoaSpeechRecognition.Finished;
begin
  if not FIsFinished then
  begin
    FIsFinished := True;
    if IsStopped then
      Stopped
    else
      StopRecording;
  end;
end;

function TCocoaSpeechRecognition.IsRecording: Boolean;
begin
  Result := FIsRecording;
end;

procedure TCocoaSpeechRecognition.QueueAuthorizationStatus(const AStatus: TAuthorizationStatus);
begin
  TThread.Queue(nil,
    procedure
    begin
      DoAuthorizationStatus(AStatus);
    end
  );
end;

procedure TCocoaSpeechRecognition.QueueRecordingStatusChanged;
begin
  TThread.Queue(nil, DoRecordingStatusChanged);
end;

procedure TCocoaSpeechRecognition.RequestAuthorization;
begin
  //
end;

procedure TCocoaSpeechRecognition.RequestPermission;
begin
  RequestRecordAudio;
end;

procedure TCocoaSpeechRecognition.RequestRecordAudio;
begin
  //
end;

procedure TCocoaSpeechRecognition.RequestRecordAudioHandler(granted: Boolean);
begin
  if granted then
    RequestAuthorization
  else
    QueueAuthorizationStatus(TAuthorizationStatus.Denied);
end;

procedure TCocoaSpeechRecognition.StartRecording;
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

procedure TCocoaSpeechRecognition.StartedRecording;
begin
  FIsRecordingPending := False;
  FIsRecording := True;
  FIsStopped := False;
  FIsFinished := False;
  QueueRecordingStatusChanged;
end;

procedure TCocoaSpeechRecognition.StoppedRecording;
begin
  FIsRecording := False;
  FIsStopped := True;
  QueueRecordingStatusChanged;
end;

procedure TCocoaSpeechRecognition.TimerHandler;
begin
  FTimer.IsEnabled := False;
  StopRecording;
end;

end.
