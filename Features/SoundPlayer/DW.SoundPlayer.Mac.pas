unit DW.SoundPlayer.Mac;

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
  {$IF not Defined(IOS)}
  Macapi.CocoaTypes, Macapi.Foundation, Macapi.AVFoundation,
  {$ELSE}
  // iOS
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.AVFoundation,
  {$ENDIF}
  // DW
  DW.SoundPlayer;

type
  TPlatformSoundPlayer = class;

  AVAudioPlayerDelegate = interface(IObjectiveC)
    ['{9C250EB2-E494-4AC5-BD88-E9245356A4E3}']
    procedure audioPlayerDecodeErrorDidOccur(player: AVAudioPlayer; error: NSError); cdecl;
    procedure audioPlayerDidFinishPlaying(player: AVAudioPlayer; successfully: Boolean); cdecl;
  end;

  TAVAudioPlayerDelegate = class(TOCLocal, AVAudioPlayerDelegate)
  private
    FPlatformSoundPlayer: TPlatformSoundPlayer;
  public
    { AVAudioPlayerDelegate }
    procedure audioPlayerDecodeErrorDidOccur(player: AVAudioPlayer; error: NSError); cdecl;
    procedure audioPlayerDidFinishPlaying(player: AVAudioPlayer; successfully: Boolean); cdecl;
  public
    constructor Create(const APlatformSoundPlayer: TPlatformSoundPlayer);
  end;

  TPlatformSoundPlayer = class(TCustomPlatformSoundPlayer)
  private
    FDelegate: TAVAudioPlayerDelegate;
    FPlayer: AVAudioPlayer;
  protected
    function AddSound(const AFileName, ASoundName: string): Integer; override;
    procedure PlaySound(const AItem: TSoundItem; const ARate: Single); override;
  public
    constructor Create(const ASoundPlayer: TSoundPlayer); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.IOUtils,
  // macOS
  Macapi.Helpers,
  // DW
  {$IF not Defined(IOS)}
  DW.Macapi.AVFoundation;
  {$ELSE}
  DW.iOSapi.AVFoundation;
  {$ENDIF}

{ TAVAudioPlayerDelegate }

constructor TAVAudioPlayerDelegate.Create(const APlatformSoundPlayer: TPlatformSoundPlayer);
begin
  inherited Create;
  FPlatformSoundPlayer := APlatformSoundPlayer;
end;

procedure TAVAudioPlayerDelegate.audioPlayerDecodeErrorDidOccur(player: AVAudioPlayer; error: NSError);
begin
  //
end;

procedure TAVAudioPlayerDelegate.audioPlayerDidFinishPlaying(player: AVAudioPlayer; successfully: Boolean);
begin
  //
end;

{ TPlatformSoundPlayer }

constructor TPlatformSoundPlayer.Create(const ASoundPlayer: TSoundPlayer);
begin
  inherited;
  FDelegate := TAVAudioPlayerDelegate.Create(Self);
end;

destructor TPlatformSoundPlayer.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

function TPlatformSoundPlayer.AddSound(const AFileName, ASoundName: string): Integer;
begin
  Result := DoAddSound(AFileName, ASoundName, 0);
end;

procedure TPlatformSoundPlayer.PlaySound(const AItem: TSoundItem; const ARate: Single);
var
  LURL: NSURL;
  LPointer: Pointer;
begin
  LPointer := nil;
  LURL := TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(AItem.FileName)));
  FPlayer := nil;
  FPlayer := TAVAudioPlayer.Wrap(TAVAudioPlayer.Alloc.initWithContentsOfURL(LURL, @LPointer));
  FPlayer.setDelegate(FDelegate.GetObjectID);
  FPlayer.play;
end;

end.
