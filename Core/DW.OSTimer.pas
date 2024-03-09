unit DW.OSTimer;

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
  // RTL
  System.Classes, System.SysUtils;

type
  TOSTimer = class;

  TCustomPlatformOSTimer = class(TObject)
  private
    class var FTimers: array of TCustomPlatformOSTimer;
    class procedure FreeTimer(const ATimer: TCustomPlatformOSTimer);
  private
    FInterval: Integer;
    FOSTimer: TOSTimer;
    FTimerID: Cardinal;
    procedure RemoveTimer;
  protected
    class function GetTimer(const ATimerID: Cardinal): TCustomPlatformOSTimer;
  protected
    FHandler: TNotifyEvent;
    FProc: TProc;
    procedure DoHandler;
    procedure DoInterval;
    procedure DoProc;
    procedure StartTimer(const ARepeating: Boolean); virtual; abstract;
    procedure StopTimer; virtual; abstract;
    property Interval: Integer read FInterval write FInterval;
    property OSTimer: TOSTimer read FOSTimer;
    property TimerID: Cardinal read FTimerID write FTimerID;
  public
    constructor Create(const AOSTimer: TOSTimer); virtual;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Platform independent timer class.
  /// </summary>
  /// <remarks>
  ///   Provided as an alternative to the FMX TTimer class, which is over-complex, and entrenched into FMX
  ///   **** TOSTimer is not dependent on FMX ****
  /// </remarks>
  TOSTimer = class(TObject)
  private
    FEnabled: Boolean;
    FPlatformOSTimer: TCustomPlatformOSTimer;
    FOnInterval: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Integer);
    function GetInterval: Integer;
  protected
    procedure DoInterval;
  public
    /// <summary>
    ///   Creates a "one-shot" timer that fires at the desired interval, and is destroyed after it fires
    /// </summary>
    class procedure FireOnce(const AInterval: Integer; const AHandler: TNotifyEvent); overload;
    class procedure FireOnce(const AInterval: Integer; const AHandler: TProc); overload;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Calls the event handler that is normally called after the interval has expired
    /// </summary>
    procedure Fire;
    /// <summary>
    ///   Equivalent to Enabled in TTimer
    /// </summary>
    property Enabled: Boolean read FEnabled write SetEnabled;
    /// <summary>
    ///   Equivalent to Interval in TTimer
    /// </summary>
    property Interval: Integer read GetInterval write SetInterval;
    /// <summary>
    ///   Equivalent to OnTimer in TTimer, whereas this property is more appropriately named
    /// </summary>
    property OnInterval: TNotifyEvent read FOnInterval write FOnInterval;
  end;

implementation

uses
  // DW
  {$IF Defined(MACOS)}
  DW.OSTimer.Mac;
  {$ENDIF}
  {$IF Defined(MSWINDOWS)}
  DW.OSTimer.Win;
  {$ENDIF}
  {$IF Defined(ANDROID)}
  DW.OSTimer.Android;
  {$ENDIF}
  {$IF Defined(LINUX)}
  DW.OSTimer.Linux;
  {$ENDIF}

{ TCustomPlatformOSTimer }

constructor TCustomPlatformOSTimer.Create(const AOSTimer: TOSTimer);
begin
  inherited Create;
  FOSTimer := AOSTimer;
  SetLength(FTimers, Length(FTimers) + 1);
  FTimers[Length(FTimers) - 1] := Self;
  FTimerID := Length(FTimers);
end;

destructor TCustomPlatformOSTimer.Destroy;
begin
  RemoveTimer;
  inherited;
end;

procedure TCustomPlatformOSTimer.DoInterval;
begin
  if FOSTimer <> nil then
  begin
    if FOSTimer.Enabled then
      FOSTimer.DoInterval;
  end
  else if Assigned(FHandler) then
    DoHandler
  else if Assigned(FProc) then
    DoProc;
end;

procedure TCustomPlatformOSTimer.DoHandler;
var
  LHandler: TNotifyEvent;
begin
  // Nil FHandler in case it takes > Interval
  LHandler := FHandler;
  FHandler := nil;
  LHandler(Self);
  TCustomPlatformOSTimer.FreeTimer(Self);
end;

procedure TCustomPlatformOSTimer.DoProc;
var
  LProc: TProc;
begin
  // Nil FProc in case it takes > Interval
  LProc := FProc;
  FProc := nil;
  LProc;
  TCustomPlatformOSTimer.FreeTimer(Self);
end;

class procedure TCustomPlatformOSTimer.FreeTimer(const ATimer: TCustomPlatformOSTimer);
var
  LTimer: TCustomPlatformOSTimer;
begin
  LTimer := GetTimer(ATimer.TimerID);
  LTimer.RemoveTimer;
  LTimer.Free;
end;

class function TCustomPlatformOSTimer.GetTimer(const ATimerID: Cardinal): TCustomPlatformOSTimer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(FTimers) - 1 do
  begin
    if FTimers[I].TimerID = ATimerID then
      Exit(FTimers[I]); // <======
  end;
end;

procedure TCustomPlatformOSTimer.RemoveTimer;
var
  I: Integer;
begin
  for I := 0 to Length(FTimers) - 1 do
  begin
    if FTimers[I] = Self then
    begin
      Delete(FTimers, I, 1);
      Break;
    end;
  end;
end;

{ TOSTimer }

constructor TOSTimer.Create;
begin
  inherited;
  FPlatformOSTimer := TPlatformOSTimer.Create(Self);
  FPlatformOSTimer.Interval := 1000;
end;

destructor TOSTimer.Destroy;
begin
  FPlatformOSTimer.Free;
  inherited;
end;

procedure TOSTimer.DoInterval;
begin
  if Assigned(FOnInterval) then
    FOnInterval(Self);
end;

procedure TOSTimer.Fire;
begin
  DoInterval;
end;

class procedure TOSTimer.FireOnce(const AInterval: Integer; const AHandler: TProc);
var
  LTimer: TPlatformOSTimer;
begin
  LTimer := TPlatformOSTimer.Create(nil);
  LTimer.Interval := AInterval;
  LTimer.FProc := AHandler;
  LTimer.StartTimer(False);
end;

class procedure TOSTimer.FireOnce(const AInterval: Integer; const AHandler: TNotifyEvent);
var
  LTimer: TPlatformOSTimer;
begin
  LTimer := TPlatformOSTimer.Create(nil);
  LTimer.Interval := AInterval;
  LTimer.FHandler := AHandler;
  LTimer.StartTimer(False);
end;

function TOSTimer.GetInterval: Integer;
begin
  Result := FPlatformOSTimer.Interval;
end;

procedure TOSTimer.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    if FEnabled then
      FPlatformOSTimer.StartTimer(True)
    else
      FPlatformOSTimer.StopTimer;
  end;
end;

procedure TOSTimer.SetInterval(const Value: Integer);
begin
  if Value <> Interval then
  begin
    if FEnabled then
      FPlatformOSTimer.StopTimer;
    FPlatformOSTimer.Interval := Value;
    if FEnabled then
      FPlatformOSTimer.StartTimer(True);
  end;
end;

end.
