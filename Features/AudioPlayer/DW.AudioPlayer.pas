unit DW.AudioPlayer;

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

type
  TAudioPlayer = class;

  /// <summary>
  ///   State of audio
  /// </summary>
  /// <remarks>
  ///   Ready:      Audio has been preprared for playing
  ///   PlayStart:  Play has been called, but the audio has not necessarily started playing
  ///   Playing:    Audio has actually started to play (may be a slight delay after Play is called)
  ///   Stopped:    Audio has finished playing
  /// </remarks>
  TAudioState = (None, Ready, PlayStart, Playing, Paused, Stopped);

  TAudioStateChangeEvent = procedure(Sender: TObject; const State: TAudioState) of object;

  TCustomPlatformAudioPlayer = class(TObject)
  private
    FAudioPlayer: TAudioPlayer;
    FAudioState: TAudioState;
    FIsReady: Boolean;
  protected
    procedure DoAudioStateChange(const AState: TAudioState); virtual;
    procedure DoPlay;
    function GetDelay: Integer; virtual;
    procedure LoadFromFile(const AFileName: string); virtual;
    procedure Pause; virtual;
    procedure Play; virtual;
    procedure SeekTo(const AMilliseconds: Int64); virtual;
    procedure SetIsReady(const AValue: Boolean);
    procedure Stop; virtual;
    property AudioPlayer: TAudioPlayer read FAudioPlayer;
    property AudioState: TAudioState read FAudioState;
    property IsReady: Boolean read FIsReady;
  public
    constructor Create(const AAudioPlayer: TAudioPlayer); virtual;
    destructor Destroy; override;
  end;

  TAudioPlayer = class(TObject)
  private
    FFileName: string;
    FPlatformAudioPlayer: TCustomPlatformAudioPlayer;
    FOnAudioStateChange: TAudioStateChangeEvent;
    function GetDelay: Integer;
    function GetAudioState: TAudioState;
  protected
    procedure DoAudioStateChange;
  public
    constructor Create;
    destructor Destroy; override;
    function IsPaused: Boolean;
    function IsPlaying: Boolean;
    procedure LoadFromFile(const AFileName: string);
    procedure Pause;
    procedure Play;
    procedure SeekTo(const AMilliseconds: Int64);
    procedure Stop;
    property AudioState: TAudioState read GetAudioState;
    /// <summary>
    ///   Represents the delay in milliseconds between the call to Play, and when the audio actually starts playing
    /// </summary>
    /// <remarks>
    ///   At present, implemented only on Android and Windows
    ///   The Android MediaPlayer class seems to have a lag when calling the play method, even after having already called prepare
    /// </remarks>
    property Delay: Integer read GetDelay;
    property FileName: string read FFileName;
    property OnAudioStateChange: TAudioStateChangeEvent read FOnAudioStateChange write FOnAudioStateChange;
  end;

implementation

{$IF Defined(IOS)}
uses
  DW.AudioPlayer.iOS;
{$ELSEIF Defined(ANDROID)}
uses
  DW.AudioPlayer.Android;
{$ELSEIF Defined(MSWINDOWS)}
uses
  DW.AudioPlayer.Win;
{$ELSE}

type
  TPlatformAudioPlayer = class(TCustomPlatformAudioPlayer);
{$ENDIF}

{ TCustomPlatformAudioPlayer }

constructor TCustomPlatformAudioPlayer.Create(const AAudioPlayer: TAudioPlayer);
begin
  inherited Create;
  FAudioState := TAudioState.Stopped;
  FAudioPlayer := AAudioPlayer;
end;

destructor TCustomPlatformAudioPlayer.Destroy;
begin
  //
  inherited;
end;

procedure TCustomPlatformAudioPlayer.DoAudioStateChange(const AState: TAudioState);
begin
  FAudioState := AState;
  FAudioPlayer.DoAudioStateChange;
end;

procedure TCustomPlatformAudioPlayer.DoPlay;
begin
  DoAudioStateChange(TAudioState.PlayStart);
  Play;
end;

function TCustomPlatformAudioPlayer.GetDelay: Integer;
begin
  Result := 0;
end;

procedure TCustomPlatformAudioPlayer.LoadFromFile(const AFileName: string);
begin
  //
end;

procedure TCustomPlatformAudioPlayer.Pause;
begin
  //
end;

procedure TCustomPlatformAudioPlayer.Play;
begin
  //
end;

procedure TCustomPlatformAudioPlayer.SeekTo(const AMilliseconds: Int64);
begin
  //
end;

procedure TCustomPlatformAudioPlayer.SetIsReady(const AValue: Boolean);
begin
  if AValue <> FIsReady then
  begin
    FIsReady := AValue;
    DoAudioStateChange(TAudioState.Ready);
  end;
end;

procedure TCustomPlatformAudioPlayer.Stop;
begin
  //
end;

{ TAudioPlayer }

constructor TAudioPlayer.Create;
begin
  inherited;
  FPlatformAudioPlayer := TPlatformAudioPlayer.Create(Self);
end;

destructor TAudioPlayer.Destroy;
begin
  FPlatformAudioPlayer.Free;
  inherited;
end;

procedure TAudioPlayer.DoAudioStateChange;
begin
  if Assigned(FOnAudioStateChange) then
    FOnAudioStateChange(Self, AudioState);
end;

function TAudioPlayer.GetAudioState: TAudioState;
begin
  Result := FPlatformAudioPlayer.AudioState;
end;

function TAudioPlayer.GetDelay: Integer;
begin
  Result := FPlatformAudioPlayer.GetDelay;
end;

function TAudioPlayer.IsPaused: Boolean;
begin
  Result := AudioState = TAudioState.Paused;
end;

function TAudioPlayer.IsPlaying: Boolean;
begin
  Result := AudioState in [TAudioState.PlayStart, TAudioState.Playing];
end;

procedure TAudioPlayer.LoadFromFile(const AFileName: string);
begin
  FFileName := AFileName;
  FPlatformAudioPlayer.LoadFromFile(FFileName);
end;

procedure TAudioPlayer.Pause;
begin
  FPlatformAudioPlayer.Pause;
end;

procedure TAudioPlayer.Play;
begin
  FPlatformAudioPlayer.DoPlay;
end;

procedure TAudioPlayer.SeekTo(const AMilliseconds: Int64);
begin
  FPlatformAudioPlayer.SeekTo(AMilliseconds);
end;

procedure TAudioPlayer.Stop;
begin
  if IsPlaying or IsPaused then
    FPlatformAudioPlayer.Stop;
end;

end.
