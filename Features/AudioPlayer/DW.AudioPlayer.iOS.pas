unit DW.AudioPlayer.iOS;

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
    procedure LoadFromFile(const AFileName: string); override;
    procedure Pause; override;
    procedure Play; override;
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
  //
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
  if (FPlayer <> nil) and (FPlayer.Rate > 0) then
    FPlayer.pause;
end;

procedure TPlatformAudioPlayer.Play;
begin
  if FPlayer <> nil then
  begin
    FPlayer.play;
    DoAudioStateChange(TAudioState.Playing);
  end;
end;

end.
