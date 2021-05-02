unit DW.OSTimer.Linux;

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
  // Posix
  Posix.SysTypes,
  // DW
  DW.OSTimer;

type
  TPlatformOSTimer = class;

  TPlatformOSTimer = class(TCustomPlatformOSTimer)
  private
    FHandle: timer_t;
  protected
    procedure StartTimer; override;
    procedure StopTimer; override;
    procedure TimerEvent;
  public
    constructor Create(const AOSTimer: TOSTimer); override;
    destructor Destroy; override;
  end;

implementation

uses
  // Posix
  Posix.Signal, Posix.Time, Posix.Base;

const
  librt = 'librt.so';

function timer_create(ClockId: Integer; SignalEvent: Psigevent; TimerId: Ptimer_t): Integer; cdecl;
  external librt name _PU + 'timer_create';
function timer_delete(TimerId: timer_t): Integer; cdecl;
  external librt name _PU + 'timer_delete';
function timer_settime(TimerId: timer_t; Flags: Integer; const [Ref] ValueIn: itimerspec; PValueOut: Pitimerspec): Integer; cdecl;
  external librt name _PU + 'timer_settime';

{ TPlatformOSTimer }

constructor TPlatformOSTimer.Create(const AOSTimer: TOSTimer);
begin
  inherited;
  //
end;

destructor TPlatformOSTimer.Destroy;
begin
  //
  inherited;
end;

procedure SigEvThreadFunction(ASigVal: sigval); cdecl;
var
  LTimer: TPlatformOSTimer;
begin
  LTimer := TPlatformOSTimer(TPlatformOSTimer.GetTimer(ASigVal.sival_int));
  if LTimer <> nil then
    LTimer.TimerEvent;
end;

procedure TPlatformOSTimer.StartTimer;
const
  SIGEV_THREAD = 2;
var
  LValue: itimerspec;
  LSpec: timespec;
  LSigEvent: sigevent;
begin
  LSigEvent.sigev_notify := SIGEV_THREAD;
  LSigEvent.sigev_value.sival_int := TimerID;
  LSigEvent._sigev_thread._function := SigEvThreadFunction;
  LSigEvent._sigev_thread._attribute := nil;
  timer_create(CLOCK_REALTIME, @LSigEvent, @FHandle);
  LSpec.tv_sec := Interval div 1000;
  LSpec.tv_nsec := (Interval mod 1000) * 1000000;
  LValue.it_interval := LSpec;
  LValue.it_value := LSpec;
  timer_settime(FHandle, 0, LValue, nil);
end;

procedure TPlatformOSTimer.StopTimer;
begin
  timer_delete(FHandle);
end;

procedure TPlatformOSTimer.TimerEvent;
begin
  DoInterval;
end;

end.
