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

  TCustomPlatformAudioPlayer = class(TObject)
  private
    FAudioPlayer: TAudioPlayer;
    FIsReady: Boolean;
  protected
    procedure LoadFromFile(const AFileName: string); virtual;
    procedure SetIsReady(const AValue: Boolean);
    procedure Pause; virtual;
    procedure Play; virtual;
    property AudioPlayer: TAudioPlayer read FAudioPlayer;
    property IsReady: Boolean read FIsReady;
  public
    constructor Create(const AAudioPlayer: TAudioPlayer); virtual;
    destructor Destroy; override;
  end;

  TAudioPlayer = class(TObject)
  private
    FPlatformAudioPlayer: TCustomPlatformAudioPlayer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    procedure Pause;
    procedure Play;
  end;

implementation

{$IF Defined(IOS)}
uses
  DW.AudioPlayer.iOS;
{$ENDIF}
{$IF Defined(ANDROID)}
uses
  DW.AudioPlayer.Android;
{$ENDIF}

{$IF Defined(MSWINDOWS)}
type
  TPlatformAudioPlayer = class(TCustomPlatformAudioPlayer);
{$ENDIF}

{ TCustomPlatformAudioPlayer }

constructor TCustomPlatformAudioPlayer.Create(const AAudioPlayer: TAudioPlayer);
begin
  inherited Create;
  FAudioPlayer := AAudioPlayer;
end;

destructor TCustomPlatformAudioPlayer.Destroy;
begin
  //
  inherited;
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

procedure TCustomPlatformAudioPlayer.SetIsReady(const AValue: Boolean);
begin
  FIsReady := AValue;
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

procedure TAudioPlayer.LoadFromFile(const AFileName: string);
begin
  FPlatformAudioPlayer.LoadFromFile(AFileName);
end;

procedure TAudioPlayer.Pause;
begin
  FPlatformAudioPlayer.Pause;
end;

procedure TAudioPlayer.Play;
begin
  FPlatformAudioPlayer.Play;
end;

end.
