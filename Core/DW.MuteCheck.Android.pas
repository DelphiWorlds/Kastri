unit DW.MuteCheck.Android;

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
  DW.MuteCheck;

type
  TPlatformMuteCheck = class(TCustomPlatformMuteCheck)
  protected
    procedure Check; override;
  public
    constructor Create(const AMuteCheck: TMuteCheck); override;
    destructor Destroy; override;
  end;

implementation

uses
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Media, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers;

{ TPlatformMuteCheck }

constructor TPlatformMuteCheck.Create(const AMuteCheck: TMuteCheck);
begin
  inherited;
  //
end;

destructor TPlatformMuteCheck.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformMuteCheck.Check;
var
  LService: JObject;
  LAudioManager: JAudioManager;
  LRingerMode: Integer;
begin
  LService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.AUDIO_SERVICE);
  LAudioManager := TJAudioManager.Wrap(TAndroidHelper.JObjectToID(LService));
  LRingerMode := LAudioManager.getRingerMode;
  HandleResult((LRingerMode = TJAudioManager.JavaClass.RINGER_MODE_SILENT) or (LRingerMode = TJAudioManager.JavaClass.RINGER_MODE_VIBRATE));
end;

end.
