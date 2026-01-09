unit DW.OSTimer.Mac;

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
  // macOS
  Macapi.ObjectiveC,
  // iOS
  {$IF Defined(IOS)}
  iOSapi.Foundation,
  {$ELSEIF Defined(MACOS)}
  Macapi.Foundation,
  {$ENDIF}
  // DW
  DW.OSTimer;

type
  TPlatformOSTimer = class;

  IOSTimerDelegate = interface(IObjectiveC)
    ['{C3817E03-699C-4ACC-9D2E-E88CA6763B09}']
    procedure timerEvent(timer: NSTimer); cdecl;
  end;

  TOSTimerDelegate = class(TOCLocal, IOSTimerDelegate)
  private
    FPlatformOSTimer: TPlatformOSTimer;
  public
    { IOSTimerDelegate }
    procedure timerEvent(timer: NSTimer); cdecl;
  public
    constructor Create(const APlatformOSTimer: TPlatformOSTimer);
  end;

  TPlatformOSTimer = class(TCustomPlatformOSTimer)
  private
    FDelegate: TOSTimerDelegate;
    FTimer: NSTimer;
  protected
    procedure StartTimer(const ARepeating: Boolean); override;
    procedure StopTimer; override;
    procedure TimerEvent;
  public
    constructor Create(const AOSTimer: TOSTimer); override;
    destructor Destroy; override;
  end;

implementation

uses
  // macOS
  Macapi.ObjCRuntime;

{ TOSTimerDelegate }

constructor TOSTimerDelegate.Create(const APlatformOSTimer: TPlatformOSTimer);
begin
  inherited Create;
  FPlatformOSTimer := APlatformOSTimer;
end;

procedure TOSTimerDelegate.timerEvent(timer: NSTimer);
begin
  FPlatformOSTimer.TimerEvent;
end;

{ TPlatformOSTimer }

constructor TPlatformOSTimer.Create(const AOSTimer: TOSTimer);
begin
  inherited;
  FDelegate := TOSTimerDelegate.Create(Self);
end;

destructor TPlatformOSTimer.Destroy;
begin
  FDelegate.Free;
  inherited;
end;

procedure TPlatformOSTimer.StartTimer(const ARepeating: Boolean);
var
  LInt: Double;
begin
  LInt := Interval / 1000;
  {$IF (CompilerVersion < 37) or Defined(OSX)}
  FTimer := TNSTimer.Wrap(TNSTimer.OCClass.scheduledTimerWithTimeInterval(LInt, FDelegate.GetObjectID, sel_getUid('timerEvent:'), nil, ARepeating));
  {$ELSE}
  FTimer := TNSTimer.OCClass.scheduledTimerWithTimeInterval(LInt, FDelegate.GetObjectID, sel_getUid('timerEvent:'), nil, ARepeating);
  {$ENDIF}
end;

procedure TPlatformOSTimer.StopTimer;
begin
  if FTimer <> nil then
    FTimer.invalidate;
  FTimer := nil;
end;

procedure TPlatformOSTimer.TimerEvent;
begin
  DoInterval;
end;

end.
