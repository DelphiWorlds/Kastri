unit DW.SoundPlayer.Android;

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
  Androidapi.JNI.Media, Androidapi.JNI.GraphicsContentViewText,
  // DW
  DW.SoundPlayer;

type
  TPlatformSoundPlayer = class(TCustomPlatformSoundPlayer)
  private
    FAudioManager: JAudioManager;
    FSoundPool: JSoundPool;
    procedure CreateSoundPool;
    function GetNativeAudioContentType: Integer;
    function GetNativeAudioUsage: Integer;
  protected
    function AddSound(const AFileName, ASoundName: string): Integer; override;
    procedure NeedsUpdate; override;
    procedure PlaySound(const AItem: TSoundItem; const ARate: Single); override;
  public
    constructor Create(const ASoundPlayer: TSoundPlayer); override;
    destructor Destroy; override;
  end;

implementation

uses
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.Helpers;

{ TPlatformSoundPlayer }

constructor TPlatformSoundPlayer.Create(const ASoundPlayer: TSoundPlayer);
var
  LService: JObject;
begin
  inherited;
  FMaxStreams := 4;
  LService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  FAudioManager := TJAudioManager.Wrap(TAndroidHelper.JObjectToID(LService));
end;

procedure TPlatformSoundPlayer.CreateSoundPool;
var
  LAudioAttributes: JAudioAttributes;
begin
  FSoundPool := nil;
  LAudioAttributes := TJAudioAttributes_Builder.JavaClass.init
    .setUsage(GetNativeAudioUsage)
    .setContentType(GetNativeAudioContentType)
    .build;
  FSoundPool := TJSoundPool_Builder.JavaClass.init
    .setAudioAttributes(LAudioAttributes)
    .setMaxStreams(MaxStreams)
    .build;
end;

destructor TPlatformSoundPlayer.Destroy;
begin
  //
  inherited;
end;

function TPlatformSoundPlayer.GetNativeAudioContentType: Integer;
begin
  case AudioContentType of
    TAudioContentType.Music:
      Result := TJAudioAttributes.JavaClass.CONTENT_TYPE_MUSIC;
    TAudioContentType.Sound:
      Result := TJAudioAttributes.JavaClass.CONTENT_TYPE_SONIFICATION;
    TAudioContentType.Movie:
      Result := TJAudioAttributes.JavaClass.CONTENT_TYPE_MOVIE;
    TAudioContentType.Speech:
      Result := TJAudioAttributes.JavaClass.CONTENT_TYPE_SPEECH;
  else
    Result := TJAudioAttributes.JavaClass.CONTENT_TYPE_SONIFICATION;
  end;
end;

function TPlatformSoundPlayer.GetNativeAudioUsage: Integer;
begin
  case AudioUsage of
    TAudioUsage.Media:
      Result := TJAudioAttributes.JavaClass.USAGE_MEDIA;
    TAudioUsage.Game:
      Result := TJAudioAttributes.JavaClass.USAGE_GAME;
    TAudioUsage.Alarm:
      Result := TJAudioAttributes.JavaClass.USAGE_ALARM;
    TAudioUsage.Ringtone:
      Result := TJAudioAttributes.JavaClass.USAGE_NOTIFICATION_RINGTONE;
  else
    Result := TJAudioAttributes.JavaClass.USAGE_MEDIA;
  end;
end;

procedure TPlatformSoundPlayer.NeedsUpdate;
var
  LItems: TSoundItems;
  LItem: TSoundItem;
begin
  LItems := Copy(SoundItems, 0, Length(SoundItems));
  ClearSoundItems;
  for LItem in LItems do
    AddSound(LItem.FileName, LItem.Name);
end;

procedure TPlatformSoundPlayer.PlaySound(const AItem: TSoundItem; const ARate: Single);
var
  LCurrentVolume, LMaxVolume, LVolume: Double;
begin
  LCurrentVolume := FAudioManager.getStreamVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
  LMaxVolume  := FAudioManager.getStreamMaxVolume(TJAudioManager.JavaClass.STREAM_MUSIC);
  LVolume :=  LCurrentVolume / LMaxVolume;
  FSoundPool.play(AItem.ID, LVolume, LVolume, 1, 0, ARate);
end;

function TPlatformSoundPlayer.AddSound(const AFileName, ASoundName: string): Integer;
begin
  if FSoundPool = nil then
    CreateSoundPool;
  Result := DoAddSound(AFileName, ASoundName, FSoundPool.load(StringToJString(AFileName), 0));
end;

end.
