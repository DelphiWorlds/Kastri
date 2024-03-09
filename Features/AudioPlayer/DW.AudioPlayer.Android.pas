unit DW.AudioPlayer.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNI.Media, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,
  // DW
  DW.AudioPlayer, DW.Android.Helpers;

type
  TPlatformAudioPlayer = class;

  TAudioPlayerWatcher = class(TCustomRunnable, JRunnable)
  private
    FHandler: JHandler;
    FIsRunning: Boolean;
    FPlayer: TPlatformAudioPlayer;
  protected
    procedure DoRun; override;
    procedure PlayStarted;
  public
    constructor Create(const APlayer: TPlatformAudioPlayer);
    procedure Start;
    procedure Stop;
  end;

  TPlatformAudioPlayer = class(TCustomPlatformAudioPlayer)
  private
    FAudioManager: JAudioManager;
    FDelay: Integer;
    FIsPlayRequested: Boolean;
    FMediaPlayer: JMediaPlayer;
    FOnPreparedListener: JMediaPlayer_OnPreparedListener;
    FPlayStartTime: TDateTime;
    FWatcher: TAudioPlayerWatcher;
  protected
    procedure DoAudioStateChange(const AState: TAudioState); override;
    function GetDelay: Integer; override;
    procedure LoadFromFile(const AFileName: string); override;
    procedure MediaPlayerItemPrepared;
    procedure Pause; override;
    procedure Play; override;
    procedure PlayStarted;
    procedure SeekTo(const AMilliseconds: Int64); override;
    procedure Stop; override;
    property MediaPlayer: JMediaPlayer read FMediaPlayer;
  public
    constructor Create(const AAudioPlayer: TAudioPlayer); override;
    destructor Destroy; override;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.Classes, System.SysUtils, System.DateUtils, System.Math,
  // Android
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.App;

type
  TOnPreparedListener = class(TJavaLocal, JMediaPlayer_OnPreparedListener)
  private
    FPlayer: TPlatformAudioPlayer;
  public
    { JMediaPlayer_OnPreparedListener }
    procedure onPrepared(mp: JMediaPlayer); cdecl;
  public
    constructor Create(const APlayer: TPlatformAudioPlayer);
  end;

  TOnCompletionListener = class(TJavaLocal, JMediaPlayer_OnCompletionListener)
  private
    FPlayer: TPlatformAudioPlayer;
  public
    { JMediaPlayer_OnCompletionListener }
    procedure onCompletion(mp: JMediaPlayer); cdecl;
  public
    constructor Create(const APlayer: TPlatformAudioPlayer);
  end;

{ TAudioPlayerWatcher }

constructor TAudioPlayerWatcher.Create(const APlayer: TPlatformAudioPlayer);
begin
  inherited Create;
  FPlayer := APlayer;
  FHandler := TJHandler.JavaClass.init;
end;

procedure TAudioPlayerWatcher.DoRun;
var
  LIsStarted: Boolean;
begin
  LIsStarted := False;
  while FIsRunning do
  begin
    if FPlayer.MediaPlayer.getCurrentPosition > 0 then
    begin
      LIsStarted := True;
      Stop;
    end
    else
      Sleep(5);
  end;
  if LIsStarted then
    PlayStarted;
end;

procedure TAudioPlayerWatcher.PlayStarted;
begin
  TThread.Queue(nil, FPlayer.PlayStarted);
end;

procedure TAudioPlayerWatcher.Start;
begin
  FIsRunning := True;
  FHandler.post(Self);
end;

procedure TAudioPlayerWatcher.Stop;
begin
  FIsRunning := False;
end;

{ TOnPreparedListener }

constructor TOnPreparedListener.Create(const APlayer: TPlatformAudioPlayer);
begin
  inherited Create;
  FPlayer := APlayer;
end;

procedure TOnPreparedListener.onPrepared(mp: JMediaPlayer);
begin
  FPlayer.MediaPlayerItemPrepared;
end;

{ TOnCompletionListener }

constructor TOnCompletionListener.Create(const APlayer: TPlatformAudioPlayer);
begin
  inherited Create;
  FPlayer := APlayer;
end;

procedure TOnCompletionListener.onCompletion(mp: JMediaPlayer);
begin
  FPlayer.DoAudioStateChange(TAudioState.Stopped);
end;

{ TPlatformAudioPlayer }

constructor TPlatformAudioPlayer.Create(const AAudioPlayer: TAudioPlayer);
var
  LService: JObject;
begin
  inherited;
  LService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  FAudioManager := TJAudioManager.Wrap(TAndroidHelper.JObjectToID(LService));
  FOnPreparedListener := TOnPreparedListener.Create(Self);
  FWatcher := TAudioPlayerWatcher.Create(Self);
end;

destructor TPlatformAudioPlayer.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformAudioPlayer.DoAudioStateChange(const AState: TAudioState);
begin
  inherited;
end;

function TPlatformAudioPlayer.GetDelay: Integer;
begin
  Result := FDelay;
end;

procedure TPlatformAudioPlayer.LoadFromFile(const AFileName: string);
begin
  FMediaPlayer := nil;
  FMediaPlayer := TJMediaPlayer.JavaClass.init;
  FMediaPlayer.setDataSource(StringToJString(AFileName));
  FMediaPlayer.setOnPreparedListener(FOnPreparedListener);
  FMediaPlayer.prepareAsync;
end;

procedure TPlatformAudioPlayer.MediaPlayerItemPrepared;
begin
  SetIsReady(True);
  if FIsPlayRequested then
    Play;
end;

procedure TPlatformAudioPlayer.Pause;
begin
  if (FMediaPlayer <> nil) and FMediaPlayer.isPlaying then
  begin
    FMediaPlayer.pause;
    DoAudioStateChange(TAudioState.Paused);
  end;
end;

procedure TPlatformAudioPlayer.Play;
begin
  if FMediaPlayer <> nil then
  begin
    if IsReady then
    begin
      FIsPlayRequested := False;
      if AudioState <> TAudioState.Paused then
      begin
        FWatcher.Start;
        FMediaPlayer.seekTo(0);
        FPlayStartTime := Now;
        DoAudioStateChange(TAudioState.PlayStart);
      end;
      FMediaPlayer.start;
    end
    else
      FIsPlayRequested := True;
  end;
end;

procedure TPlatformAudioPlayer.PlayStarted;
begin
  FDelay := MilliSecondsBetween(Now, FPlayStartTime);
  // TOSLog.d('Delay: %dms', [FDelay]);
  DoAudioStateChange(TAudioState.Playing);
end;

procedure TPlatformAudioPlayer.SeekTo(const AMilliseconds: Int64);
begin
  FMediaPlayer.seekTo(AMilliseconds, TJMediaPlayer.JavaClass.SEEK_CLOSEST_SYNC);
end;

procedure TPlatformAudioPlayer.Stop;
begin
  FMediaPlayer.stop;
  DoAudioStateChange(TAudioState.Stopped);
end;

end.
