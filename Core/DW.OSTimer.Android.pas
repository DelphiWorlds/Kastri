unit DW.OSTimer.Android;

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
  // Android
  Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  // DW
  DW.OSTimer;

type
  TPlatformOSTimer = class;

  TTimerRunnable = class(TJavaLocal, JRunnable)
  private
    FTimer: TPlatformOSTimer;
  public
    { JRunnable }
    procedure run; cdecl;
  public
    constructor Create(const ATimer: TPlatformOSTimer);
  end;

  TPlatformOSTimer = class(TCustomPlatformOSTimer)
  private
    FHandler: JHandler;
    FIsRepeating: Boolean;
    FRunnable: JRunnable;
  protected
    procedure StartTimer(const ARepeating: Boolean); override;
    procedure StopTimer; override;
    procedure TimerEvent;
  public
    constructor Create(const AOSTimer: TOSTimer); override;
    destructor Destroy; override;
  end;

implementation

// Note:
// As per the documentation for postDelayed:
//
//   https://developer.android.com/reference/android/os/Handler.html#postDelayed(java.lang.Runnable, long)
//
// The runnable's run method is called in the same thread as where postDelayed was called from, so there should be no need to synchronize

{ TTimerRunnable }

constructor TTimerRunnable.Create(const ATimer: TPlatformOSTimer);
begin
  inherited Create;
  FTimer := ATimer;
end;

procedure TTimerRunnable.run;
begin
  FTimer.TimerEvent;
end;

{ TPlatformOSTimer }

constructor TPlatformOSTimer.Create(const AOSTimer: TOSTimer);
begin
  inherited;
  FHandler := TJHandler.JavaClass.init;
  FRunnable := TTimerRunnable.Create(Self);
end;

destructor TPlatformOSTimer.Destroy;
begin
  StopTimer;
  FHandler := nil;
  FRunnable := nil;
  inherited;
end;

procedure TPlatformOSTimer.StartTimer(const ARepeating: Boolean);
begin
  FIsRepeating := ARepeating;
  FHandler.postDelayed(FRunnable, Interval);
end;

procedure TPlatformOSTimer.StopTimer;
begin
  FHandler.removeCallbacks(FRunnable);
end;

procedure TPlatformOSTimer.TimerEvent;
begin
  if OSTimer.Enabled then
  begin
    if FIsRepeating then
      StartTimer(FIsRepeating);
    DoInterval;
  end;
end;

end.
