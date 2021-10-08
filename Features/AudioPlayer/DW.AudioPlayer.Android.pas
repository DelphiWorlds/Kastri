unit DW.AudioPlayer.Android;

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
  Androidapi.JNI.Media, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.AudioPlayer;

type
  TPlatformAudioPlayer = class(TCustomPlatformAudioPlayer)
  private
    FAudioManager: JAudioManager;
    FIsPlayRequested: Boolean;
    FMediaPlayer: JMediaPlayer;
    FOnPreparedListener: JMediaPlayer_OnPreparedListener;
  protected
    procedure LoadFromFile(const AFileName: string); override;
    procedure MediaPlayerItemPrepared;
    procedure Pause; override;
    procedure Play; override;
  public
    constructor Create(const AAudioPlayer: TAudioPlayer); override;
    destructor Destroy; override;
  end;

implementation

uses
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNIBridge;

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

{ TPlatformAudioPlayer }

constructor TPlatformAudioPlayer.Create(const AAudioPlayer: TAudioPlayer);
var
  LService: JObject;
begin
  inherited;
  LService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  FAudioManager := TJAudioManager.Wrap(TAndroidHelper.JObjectToID(LService));
  FOnPreparedListener := TOnPreparedListener.Create(Self);
end;

destructor TPlatformAudioPlayer.Destroy;
begin
  //
  inherited;
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
    FMediaPlayer.pause;
end;

procedure TPlatformAudioPlayer.Play;
begin
  if FMediaPlayer <> nil then
  begin
    if IsReady then
    begin
      FIsPlayRequested := False;
      FMediaPlayer.seekTo(0);
      FMediaPlayer.start;
    end
    else
      FIsPlayRequested := True;
  end;
end;

end.
