unit DW.MuteCheck.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // DW
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
  // RTL
  System.SysUtils,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Media, Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNI.App;

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
