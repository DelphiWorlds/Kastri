unit DW.OSTimer.Win;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Win
  Winapi.Windows,
  // DW
  DW.OSTimer;

type
  TPlatformOSTimer = class(TCustomPlatformOSTimer)
  private
    FRepeating: Boolean;
    class procedure TimerCallback(window_hwnd: HWND; Msg: Longint; idEvent: UINT; dwTime: Longint); stdcall; static;
  protected
    procedure StartTimer(const ARepeating: Boolean); override;
    procedure StopTimer; override;
    procedure TimerEvent;
  end;

implementation

{ TPlatformOSTimer }

procedure TPlatformOSTimer.StartTimer(const ARepeating: Boolean);
begin
  FRepeating := ARepeating;
  TimerID := SetTimer(0, 0, Interval, @TimerCallback);
end;

procedure TPlatformOSTimer.StopTimer;
begin
  KillTimer(0, TimerID);
  TimerID := 0;
end;

class procedure TPlatformOSTimer.TimerCallback(window_hwnd: HWND; Msg: Longint; idEvent: UINT; dwTime: Longint);
var
  LTimer: TPlatformOSTimer;
begin
  LTimer := TPlatformOSTimer(GetTimer(idEvent));
  if LTimer <> nil then
    LTimer.TimerEvent;
end;

procedure TPlatformOSTimer.TimerEvent;
begin
  DoInterval;
  if not FRepeating then
    StopTimer;
end;

end.
