unit DW.SpeechRecognition;

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
  System.Classes,
  // DW
  DW.Types;

type
  TSpeechRecognitionTextEvent = procedure(Sender: TObject; const Text: string) of object;

  TSpeechRecognitionRecordingEvent = procedure(Sender: TObject; const IsRecording: Boolean) of object;

  TSpeechRecognition = class;

  TCustomPlatformSpeechRecognition = class(TObject)
  private
    FSpeech: TSpeechRecognition;
  protected
    procedure DoRecognition(const AText: string; const AStop: Boolean);
    procedure DoAuthorizationStatus(const AStatus: TAuthorizationStatus);
    procedure DoRecordingStatusChanged;
    function IsRecording: Boolean; virtual; abstract;
    procedure RequestPermission; virtual; abstract;
    procedure StartRecording; virtual; abstract;
    procedure StopRecording; virtual; abstract;
    procedure Stopped;
    property Speech: TSpeechRecognition read FSpeech;
  public
    constructor Create(const ASpeech: TSpeechRecognition); virtual;
  end;

  TSpeechRecognition = class(TObject)
  private
    FAuthorizationStatus: TAuthorizationStatus;
    FLanguage: string;
    FPlatformSpeech: TCustomPlatformSpeechRecognition;
    FPrompt: string;
    FStopInterval: Integer;
    FText: string;
    FOnAuthorizationStatus: TAuthorizationStatusEvent;
    FOnRecording: TSpeechRecognitionRecordingEvent;
    FOnStopped: TNotifyEvent;
    FOnText: TSpeechRecognitionTextEvent;
    function GetIsAuthorized: Boolean;
    function GetIsRecording: Boolean;
    procedure RecordingStatusChanged;
  protected
    procedure DoAuthorizationStatus(const AStatus: TAuthorizationStatus);
    procedure DoRecognition(const AText: string; const AStop: Boolean);
    procedure Stopped;
  public
    /// <summary>
    ///   Indicates whether or not speech recognition is supported
    /// </summary>
    class function IsSupported: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RequestPermission;
    /// <summary>
    ///   Starts recording
    /// </summary>
    procedure StartRecording;
    /// <summary>
    ///   Stops recording
    /// </summary>
    procedure StopRecording;
    /// <summary>
    ///   Indicates status of authorization
    /// </summary>
    property AuthorizationStatus: TAuthorizationStatus read FAuthorizationStatus;
    /// <summary>
    ///   Indicates whether or not speech recognition has been authorized
    /// </summary>
    property IsAuthorized: Boolean read GetIsAuthorized;
    /// <summary>
    ///   Indicates whether or not speech recognition is active (but not recording)
    /// </summary>
    property IsRecording: Boolean read GetIsRecording;
    /// <summary>
    ///   Determines which language is used
    /// </summary>
    property Language: string read FLanguage write FLanguage;
    /// <summary>
    ///   Prompt that is displayed when recording (Android only at present)
    /// </summary>
    property Prompt: string read FPrompt write FPrompt;
    /// <summary>
    ///   Interval (in milliseconds) where recording is stopped after which no speech is detected.
    ///   Applies to iOS ONLY - Android detects end of recording
    /// </summary>
    /// <remarks>
    ///   Use a value of 0 to control stopping of recording yourself
    /// </remarks>
    property StopInterval: Integer read FStopInterval write FStopInterval;
    /// <summary>
    ///   Text from recording
    /// </summary>
    property Text: string read FText;
    /// <summary>
    ///   Event fired when an authorization request has returned
    /// </summary>
    /// <remarks>
    ///   NOTE: On iOS, the user can choose to authorize speech recognition, but deny audio recording
    /// </remarks>
    property OnAuthorizationStatus: TAuthorizationStatusEvent read FOnAuthorizationStatus write FOnAuthorizationStatus;
    /// <summary>
    ///   Event fired when recording has started/stopped
    /// </summary>
    property OnRecording: TSpeechRecognitionRecordingEvent read FOnRecording write FOnRecording;
    /// <summary>
    ///   Event fired when the interval ends
    /// </summary>
    property OnStopped: TNotifyEvent read FOnStopped write FOnStopped;
    /// <summary>
    ///   Event fired when speech is recognised and part of the text is available
    /// </summary>
    property OnText: TSpeechRecognitionTextEvent read FOnText write FOnText;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // DW
  {$IF Defined(IOS)}
  DW.SpeechRecognition.iOS;
  {$ELSEIF Defined(MACOS)}
  DW.SpeechRecognition.Mac;
  {$ELSEIF Defined(Android)}
  DW.SpeechRecognition.Android;
  {$ELSE}
  DW.SpeechRecognition.Default;
  {$ENDIF}

{ TCustomPlatformSpeechRecognition }

constructor TCustomPlatformSpeechRecognition.Create(const ASpeech: TSpeechRecognition);
begin
  inherited Create;
  FSpeech := ASpeech;
end;

procedure TCustomPlatformSpeechRecognition.DoAuthorizationStatus(const AStatus: TAuthorizationStatus);
begin
  FSpeech.DoAuthorizationStatus(AStatus);
end;

procedure TCustomPlatformSpeechRecognition.DoRecognition(const AText: string; const AStop: Boolean);
begin
  FSpeech.DoRecognition(AText, AStop);
end;

procedure TCustomPlatformSpeechRecognition.DoRecordingStatusChanged;
begin
  FSpeech.RecordingStatusChanged;
end;

procedure TCustomPlatformSpeechRecognition.Stopped;
begin
  FSpeech.Stopped;
end;

{ TSpeechRecognition }

constructor TSpeechRecognition.Create;
begin
  inherited;
  FAuthorizationStatus := TAuthorizationStatus.NotDetermined;
  FPlatformSpeech := TPlatformSpeechRecognition.Create(Self);
end;

destructor TSpeechRecognition.Destroy;
begin
  FPlatformSpeech.Free;
  inherited;
end;

procedure TSpeechRecognition.DoAuthorizationStatus(const AStatus: TAuthorizationStatus);
begin
  FAuthorizationStatus := AStatus;
  if Assigned(FOnAuthorizationStatus) then
    FOnAuthorizationStatus(Self, FAuthorizationStatus);
end;

procedure TSpeechRecognition.DoRecognition(const AText: string; const AStop: Boolean);
begin
  FText := AText;
  if Assigned(FOnText) then
    FOnText(Self, AText);
  if AStop then
    Stopped;
end;

procedure TSpeechRecognition.RecordingStatusChanged;
begin
  if Assigned(FOnRecording) then
    FOnRecording(Self, IsRecording);
end;

procedure TSpeechRecognition.RequestPermission;
begin
  FPlatformSpeech.RequestPermission;
end;

function TSpeechRecognition.GetIsAuthorized: Boolean;
begin
  Result := FAuthorizationStatus = TAuthorizationStatus.Authorized;
end;

function TSpeechRecognition.GetIsRecording: Boolean;
begin
  Result := FPlatformSpeech.IsRecording;
end;

class function TSpeechRecognition.IsSupported: Boolean;
begin
  Result := TPlatformSpeechRecognition.IsSupported;
end;

procedure TSpeechRecognition.StartRecording;
begin
  FPlatformSpeech.StartRecording;
end;

procedure TSpeechRecognition.Stopped;
begin
  if Assigned(FOnStopped) then
    FOnStopped(Self);
end;

procedure TSpeechRecognition.StopRecording;
begin
  FPlatformSpeech.StopRecording;
end;

end.


