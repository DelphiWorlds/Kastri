unit DW.VideoPlayer.Android;

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
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  // DW
  DW.VideoPlayer, DW.VideoPlayer.View.Android,
  DW.Androidapi.JNI.AndroidX.Media3.ExoPlayer, DW.Androidapi.JNI.AndroidX.Media3.UI, DW.Androidapi.JNI.AndroidX.Media3.Common;

type
  TPlatformVideoPlayer = class;

  TPlayerListener = class(TJavaLocal, JPlayer_Listener)
  private
    FPlatformVideoPlayer: TPlatformVideoPlayer;
  public
    { JPlayer_Listener }
    procedure onAudioAttributesChanged(audioattributes: JAudioAttributes); cdecl;
    procedure onAudioSessionIdChanged(int: Integer); cdecl;
    procedure onAvailableCommandsChanged(commands: JPlayer_Commands); cdecl;
    procedure onCues(list: JList); overload; cdecl;
    procedure onCues(cuegroup: JCueGroup); overload; cdecl;
    procedure onDeviceInfoChanged(deviceinfo: JDeviceInfo); cdecl;
    procedure onDeviceVolumeChanged(int: Integer; boolean: Boolean); cdecl;
    procedure onEvents(player: JPlayer; events: JPlayer_Events); cdecl;
    procedure onIsLoadingChanged(boolean: Boolean); cdecl;
    procedure onIsPlayingChanged(boolean: Boolean); cdecl;
    procedure onLoadingChanged(boolean: Boolean); cdecl;
    procedure onMaxSeekToPreviousPositionChanged(long: Int64); cdecl;
    procedure onMediaItemTransition(mediaitem: JMediaItem; int: Integer); cdecl;
    procedure onMediaMetadataChanged(mediametadata: JMediaMetadata); cdecl;
    procedure onMetadata(metadata: JMetadata); cdecl;
    procedure onPlayWhenReadyChanged(boolean: Boolean; int_1: Integer); cdecl;
    procedure onPlaybackParametersChanged(playbackparameters: JPlaybackParameters); cdecl;
    procedure onPlaybackStateChanged(int: Integer); cdecl;
    procedure onPlaybackSuppressionReasonChanged(int: Integer); cdecl;
    procedure onPlayerError(playbackexception: JPlaybackException); cdecl;
    procedure onPlayerErrorChanged(playbackexception: JPlaybackException); cdecl;
    procedure onPlayerStateChanged(boolean: Boolean; int_1: Integer); cdecl;
    procedure onPlaylistMetadataChanged(mediametadata: JMediaMetadata); cdecl;
    procedure onPositionDiscontinuity(positioninfo: JPlayer_PositionInfo; positioninfo_1: JPlayer_PositionInfo; int: Integer); overload; cdecl;
    procedure onPositionDiscontinuity(int: Integer); overload; cdecl;
    procedure onRenderedFirstFrame; cdecl;
    procedure onRepeatModeChanged(int: Integer); cdecl;
    procedure onSeekBackIncrementChanged(long: Int64); cdecl;
    procedure onSeekForwardIncrementChanged(long: Int64); cdecl;
    procedure onShuffleModeEnabledChanged(boolean: Boolean); cdecl;
    procedure onSkipSilenceEnabledChanged(boolean: Boolean); cdecl;
    procedure onSurfaceSizeChanged(int: Integer; int_1: Integer); cdecl;
    procedure onTimelineChanged(timeline: JTimeline; int: Integer); cdecl;
    procedure onTrackSelectionParametersChanged(trackselectionparameters: JTrackSelectionParameters); cdecl;
    procedure onTracksChanged(tracks: JTracks); cdecl;
    procedure onVideoSizeChanged(videosize: JVideoSize); cdecl;
    procedure onVolumeChanged(float: Single); cdecl;
  public
    constructor Create(const APlatformVideoPlayer: TPlatformVideoPlayer);
  end;

  TPlatformVideoPlayer = class(TCustomPlatformVideoPlayer)
  private
    FCurrentMediaItemIndex: Integer;
    FCurrentPosition: Int64;
    FPlayer: JExoPlayer;
    FPlayerListener: JPlayer_Listener;
    FPlayerView: JPlayerView;
    FPlayWhenReady: Boolean;
    FPresentation: TAndroidExoPlayerView;
    procedure CreatePlayer;
    function HasPlayer: Boolean;
    procedure PlayerViewTouchHandler(view: JView; event: JMotionEvent);
    procedure ReleasePlayer;
  protected
    procedure BecameActive; override;
    procedure Pause; override;
    procedure Play(const AURL: string = ''); override;
    procedure PlaybackStateChanged(const APlaybackState: Integer);
    procedure Prepare(const AURL: string); override;
    procedure SetControllerTimeout(const Value: Integer); override;
    procedure SetUseController(const Value: Boolean); override;
    procedure Stop; override;
    procedure ShowController(const Value: Boolean); override;
    procedure WillBecomeInactive; override;
  public
    constructor Create(const AVideoPlayer: TVideoPlayer); override;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers,
  // DW
  DW.Android.Helpers, DW.VideoPlayer.View;

{ TPlayerListener }

constructor TPlayerListener.Create(const APlatformVideoPlayer: TPlatformVideoPlayer);
begin
  inherited Create;
  FPlatformVideoPlayer := APlatformVideoPlayer;
end;

procedure TPlayerListener.onAudioAttributesChanged(audioattributes: JAudioAttributes);
begin

end;

procedure TPlayerListener.onAudioSessionIdChanged(int: Integer);
begin

end;

procedure TPlayerListener.onAvailableCommandsChanged(commands: JPlayer_Commands);
begin

end;

procedure TPlayerListener.onCues(list: JList);
begin

end;

procedure TPlayerListener.onCues(cuegroup: JCueGroup);
begin

end;

procedure TPlayerListener.onDeviceInfoChanged(deviceinfo: JDeviceInfo);
begin

end;

procedure TPlayerListener.onDeviceVolumeChanged(int: Integer; boolean: Boolean);
begin

end;

procedure TPlayerListener.onEvents(player: JPlayer; events: JPlayer_Events);
begin

end;

procedure TPlayerListener.onIsLoadingChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onIsPlayingChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onLoadingChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onMaxSeekToPreviousPositionChanged(long: Int64);
begin

end;

procedure TPlayerListener.onMediaItemTransition(mediaitem: JMediaItem; int: Integer);
begin

end;

procedure TPlayerListener.onMediaMetadataChanged(mediametadata: JMediaMetadata);
begin

end;

procedure TPlayerListener.onMetadata(metadata: JMetadata);
begin

end;

procedure TPlayerListener.onPlaybackParametersChanged(playbackparameters: JPlaybackParameters);
begin

end;

procedure TPlayerListener.onPlaybackStateChanged(int: Integer);
begin
  FPlatformVideoPlayer.PlaybackStateChanged(int);
end;

procedure TPlayerListener.onPlaybackSuppressionReasonChanged(int: Integer);
begin

end;

procedure TPlayerListener.onPlayerError(playbackexception: JPlaybackException);
begin

end;

procedure TPlayerListener.onPlayerErrorChanged(playbackexception: JPlaybackException);
begin

end;

procedure TPlayerListener.onPlayerStateChanged(boolean: Boolean; int_1: Integer);
begin

end;

procedure TPlayerListener.onPlaylistMetadataChanged(mediametadata: JMediaMetadata);
begin

end;

procedure TPlayerListener.onPlayWhenReadyChanged(boolean: Boolean; int_1: Integer);
begin

end;

procedure TPlayerListener.onPositionDiscontinuity(int: Integer);
begin

end;

procedure TPlayerListener.onPositionDiscontinuity(positioninfo, positioninfo_1: JPlayer_PositionInfo; int: Integer);
begin

end;

procedure TPlayerListener.onRenderedFirstFrame;
begin

end;

procedure TPlayerListener.onRepeatModeChanged(int: Integer);
begin

end;

procedure TPlayerListener.onSeekBackIncrementChanged(long: Int64);
begin

end;

procedure TPlayerListener.onSeekForwardIncrementChanged(long: Int64);
begin

end;

procedure TPlayerListener.onShuffleModeEnabledChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onSkipSilenceEnabledChanged(boolean: Boolean);
begin

end;

procedure TPlayerListener.onSurfaceSizeChanged(int, int_1: Integer);
begin

end;

procedure TPlayerListener.onTimelineChanged(timeline: JTimeline; int: Integer);
begin

end;

procedure TPlayerListener.onTracksChanged(tracks: JTracks);
begin

end;

procedure TPlayerListener.onTrackSelectionParametersChanged(trackselectionparameters: JTrackSelectionParameters);
begin

end;

procedure TPlayerListener.onVideoSizeChanged(videosize: JVideoSize);
begin

end;

procedure TPlayerListener.onVolumeChanged(float: Single);
begin

end;

{ TPlatformVideoPlayer }

constructor TPlatformVideoPlayer.Create(const AVideoPlayer: TVideoPlayer);
begin
  inherited;
  FPlayWhenReady := True;
  FPlayerListener := TPlayerListener.Create(Self);
  FPresentation := TAndroidExoPlayerView(View.Presentation);
  FPresentation.OnTouch := PlayerViewTouchHandler;
  FPlayerView := FPresentation.View;
  FControllerTimeout := FPlayerView.getControllerShowTimeoutMs;
  FUseController := FPlayerView.getUseController;
end;

procedure TPlatformVideoPlayer.CreatePlayer;
begin
  if not HasPlayer then
  begin
    FPlayer := TJExoPlayer_Builder.JavaClass.init(TAndroidHelper.Context)
      // .setTrackSelector(FTrackSelector)
      .build;
    FPlayer.addListener(FPlayerListener);
    FPlayerView.setPlayer(FPlayer);
    SetPlayerState(TPlayerState.Stopped);
  end;
end;

procedure TPlatformVideoPlayer.PlayerViewTouchHandler(view: JView; event: JMotionEvent);
begin
  ViewTouched;
  if FPlayerView.isControllerFullyVisible then
    FPlayerView.hideController
  else
    FPlayerView.showController;
end;

procedure TPlatformVideoPlayer.Pause;
begin
  if HasPlayer then
  begin
    FPlayer.pause;
    SetPlayerState(TPlayerState.Paused);
  end
  else
    SetPlayerState(TPlayerState.NoPlayer);
end;

procedure TPlatformVideoPlayer.Play(const AURL: string = '');
begin
  if not AURL.IsEmpty then
    Prepare(AURL);
  // FPlayer.setPlayWhenReady(FPlayWhenReady);
  // FPlayer.seekTo(FCurrentMediaItemIndex, FCurrentPosition);
  if HasPlayer then
  begin
    FPlayer.play;
    SetPlayerState(TPlayerState.Playing);
  end;
end;

procedure TPlatformVideoPlayer.PlaybackStateChanged(const APlaybackState: Integer);
begin
//  ExoPlayer.STATE_IDLE = 1
//  ExoPlayer.STATE_BUFFERING = 2
//  ExoPlayer.STATE_READY = 3
//  ExoPlayer.STATE_ENDED = 4
//!!!!
  if APlaybackState = 3 then
    FPlayerView.setKeepScreenOn(True)
  else
    FPlayerView.setKeepScreenOn(False);
  if APlaybackState = 4 then
    SetPlayerState(TPlayerState.Completed);
end;

procedure TPlatformVideoPlayer.Prepare(const AURL: string);
var
  LMediaItem: JMediaItem;
begin
  CreatePlayer;
  LMediaItem := TJMediaItem.JavaClass.fromUri(StringToJString(AURL));
  FPlayer.setMediaItem(LMediaItem);
  FPlayer.prepare;
  // FPlayerView.setUseController(FUseController);
  SetPlayerState(TPlayerState.Prepared);
end;

function TPlatformVideoPlayer.HasPlayer: Boolean;
begin
  Result := FPlayer <> nil;
end;

procedure TPlatformVideoPlayer.ReleasePlayer;
begin
  FCurrentPosition := FPlayer.getCurrentPosition;
  FCurrentMediaItemIndex := FPlayer.getCurrentMediaItemIndex;
  FPlayWhenReady := FPlayer.getPlayWhenReady;
  // FPlayer.removeListener(FPlayerListener);
  FPlayer.release;
  FPlayer := nil;
  SetPlayerState(TPlayerState.NoPlayer);
end;

procedure TPlatformVideoPlayer.SetControllerTimeout(const Value: Integer);
begin
  inherited;
  FPlayerView.setControllerShowTimeoutMs(Value);
end;

procedure TPlatformVideoPlayer.SetUseController(const Value: Boolean);
begin
  inherited;
  FPlayerView.setUseController(Value);
end;

procedure TPlatformVideoPlayer.ShowController(const Value: Boolean);
begin
  if Value then
    FPlayerView.showController
  else
    FPlayerView.hideController;
end;

procedure TPlatformVideoPlayer.Stop;
begin
  if HasPlayer then
  begin
    FPlayer.setPlayWhenReady(False);
    FPlayer.stop;
    FPlayer.seekTo(0);
  end;
end;

procedure TPlatformVideoPlayer.BecameActive;
begin
  inherited;
  CreatePlayer; // and resume, if needed?
end;

procedure TPlatformVideoPlayer.WillBecomeInactive;
begin
  inherited;
  ReleasePlayer;
end;

end.
