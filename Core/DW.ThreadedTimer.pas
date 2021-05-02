unit DW.ThreadedTimer;

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
  // RTL
  System.Classes;

type
  TThreadedTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FIntervalCheck: TDateTime;
    FThread: TThread;
    FOnTimer: TNotifyEvent;
    procedure DoTimer;
    function IsMainThread: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure CheckInterval;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Integer read FInterval write FInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

  TTimerThread = class(TThread)
  private
    FTimer: TThreadedTimer;
  protected
    procedure Execute; override;
  public
    constructor Create(const ATimer: TThreadedTimer);
  end;

implementation

uses
  DW.OSLog, // Debug
  System.SysUtils, System.DateUtils;

const
  cSleepInterval = 20;

{ TThreadedTimer }

constructor TThreadedTimer.Create(AOwner: TComponent);
begin
  inherited;
  FThread := TTimerThread.Create(Self);
end;

destructor TThreadedTimer.Destroy;
begin
  FThread.Free;
  inherited;
end;

function TThreadedTimer.IsMainThread: Boolean;
var
  LIsService: Boolean;
begin
  {$IF Defined(ANDROID) or Defined(ANDROID64)}
  LIsService := System.DelphiActivity = nil;
  {$ELSE}
  LIsService := False;
  {$ENDIF}
  Result := (TThread.CurrentThread.ThreadID = MainThreadID) or LIsService;
end;

procedure TThreadedTimer.CheckInterval;
begin
  if FEnabled and (MillisecondsBetween(Now, FIntervalCheck) > FInterval) then
  begin
    FIntervalCheck := Now;
    if not IsMainThread then
      TThread.Synchronize(nil, DoTimer)
    else
      DoTimer;
  end;
end;

procedure TThreadedTimer.DoTimer;
begin
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TThreadedTimer.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
      FIntervalCheck := Now;
  end;
end;

{ TTimerThread }

constructor TTimerThread.Create(const ATimer: TThreadedTimer);
begin
  inherited Create;
  FTimer := ATimer;
end;

procedure TTimerThread.Execute;
begin
  while not Terminated do
  begin
    Sleep(cSleepInterval);
    FTimer.CheckInterval;
  end;
end;

end.
