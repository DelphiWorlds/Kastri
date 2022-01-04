unit DW.AudioPlayer.Win;

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
  // FMX
  FMX.Media,
  // DW
  DW.AudioPlayer;

type
  TPlatformAudioPlayer = class;

  TPlatformAudioPlayer = class(TCustomPlatformAudioPlayer)
  private
    FPlayer: TMediaPlayer;
  protected
    procedure LoadFromFile(const AFileName: string); override;
    procedure Pause; override;
    procedure Play; override;
  public
    constructor Create(const AAudioPlayer: TAudioPlayer); override;
    destructor Destroy; override;
  end;

implementation

{ TPlatformAudioPlayer }

constructor TPlatformAudioPlayer.Create(const AAudioPlayer: TAudioPlayer);
begin
  inherited;
  FPlayer := TMediaPlayer.Create(nil);
end;

destructor TPlatformAudioPlayer.Destroy;
begin
  FPlayer.Free;
  inherited;
end;

procedure TPlatformAudioPlayer.LoadFromFile(const AFileName: string);
begin
  FPlayer.FileName := AFileName;
  DoAudioStateChange(TAudioState.Ready);
end;

procedure TPlatformAudioPlayer.Pause;
begin
  FPlayer.Stop;
end;

procedure TPlatformAudioPlayer.Play;
begin
  FPlayer.Play;
  DoAudioStateChange(TAudioState.Playing);
end;

end.
