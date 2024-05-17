unit DW.AudioPlayer.iOS;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  iOSapi.AVFoundation, iOSapi.Foundation,
  // DW
  DW.AudioPlayer;

type
  TPlatformAudioPlayer = class;

  AVAudioPlayerDelegate = interface(IObjectiveC)
    ['{9C250EB2-E494-4AC5-BD88-E9245356A4E3}']
    procedure audioPlayerDecodeErrorDidOccur(player: AVAudioPlayer; error: NSError); cdecl;
    procedure audioPlayerDidFinishPlaying(player: AVAudioPlayer; successfully: Boolean); cdecl;
  end;

  TAVAudioPlayerDelegate = class(TOCLocal, AVAudioPlayerDelegate)
  private
    FPlatformAudioPlayer: TPlatformAudioPlayer;
  public
    { AVAudioPlayerDelegate }
    procedure audioPlayerDecodeErrorDidOccur(player: AVAudioPlayer; error: NSError); cdecl;
    procedure audioPlayerDidFinishPlaying(player: AVAudioPlayer; successfully: Boolean); cdecl;
  public
    constructor Create(const APlatformAudioPlayer: TPlatformAudioPlayer);
  end;

  TPlatformAudioPlayer = class(TCustomPlatformAudioPlayer)
  private
    FDelegate: TAVAudioPlayerDelegate;
    FPlayer: AVAudioPlayer;
  protected
    procedure DoAudioStateChange(const AState: TAudioState); override;
    procedure LoadFromFile(const AFileName: string); override;
    procedure Pause; override;
    procedure Play; override;
    procedure SeekTo(const AMilliseconds: Int64); override;
    procedure Stop; override;
  public
    constructor Create(const AAudioPlayer: TAudioPlayer); override;
    destructor Destroy; override;
  end;

implementation

uses
  // macOS
  Macapi.Helpers;

{ TAVAudioPlayerDelegate }

constructor TAVAudioPlayerDelegate.Create(const APlatformAudioPlayer: TPlatformAudioPlayer);
begin
  inherited Create;
  FPlatformAudioPlayer := APlatformAudioPlayer;
end;

procedure TAVAudioPlayerDelegate.audioPlayerDecodeErrorDidOccur(player: AVAudioPlayer; error: NSError);
begin
  //
end;

procedure TAVAudioPlayerDelegate.audioPlayerDidFinishPlaying(player: AVAudioPlayer; successfully: Boolean);
begin
  FPlatformAudioPlayer.DoAudioStateChange(TAudioState.Stopped);
end;

{ TPlatformAudioPlayer }

constructor TPlatformAudioPlayer.Create(const AAudioPlayer: TAudioPlayer);
begin
  inherited;
  FDelegate := TAVAudioPlayerDelegate.Create(Self);
end;

destructor TPlatformAudioPlayer.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

procedure TPlatformAudioPlayer.DoAudioStateChange(const AState: TAudioState);
begin
  inherited;
end;

procedure TPlatformAudioPlayer.LoadFromFile(const AFileName: string);
var
  LURL: NSURL;
  LPointer: Pointer;
begin
  LPointer := nil;
  LURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(AFileName)));
  FPlayer := nil;
  FPlayer := TAVAudioPlayer.Wrap(TAVAudioPlayer.Alloc.initWithContentsOfURL(LURL, @LPointer));
  FPlayer.setDelegate(FDelegate.GetObjectID);
  FPlayer.prepareToPlay;
  DoAudioStateChange(TAudioState.Ready);
end;

procedure TPlatformAudioPlayer.Pause;
begin
  if (FPlayer <> nil) and (FPlayer.rate > 0) then
  begin
    FPlayer.pause;
    DoAudioStateChange(TAudioState.Paused);
  end;
end;

procedure TPlatformAudioPlayer.Play;
begin
  if FPlayer <> nil then
  begin
    FPlayer.play;
    DoAudioStateChange(TAudioState.Playing);
  end;
end;

procedure TPlatformAudioPlayer.SeekTo(const AMilliseconds: Int64);
begin
  if FPlayer <> nil then
    FPlayer.setCurrentTime(AMilliseconds / 1000);
end;

procedure TPlatformAudioPlayer.Stop;
begin
  if (FPlayer <> nil) and (FPlayer.rate > 0) then
  begin
    FPlayer.stop;
    DoAudioStateChange(TAudioState.Stopped);
  end;
end;

end.
