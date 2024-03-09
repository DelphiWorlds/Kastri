unit DW.TimerTask.Android;

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
  System.Classes,
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  // DW
  DW.Androidapi.JNI.DWTimerTask;

type
  TTimerTask = class;

  TTimerTaskDelegate = class(TJavaLocal, JDWTimerTaskDelegate)
  private
    FTimerTask: TTimerTask;
  public
    { JDWTimerTaskDelegate }
    procedure run; cdecl;
  public
    constructor Create(const ATimerTask: TTimerTask);
  end;

  TTimerTask = class(TObject)
  private
    FTimer: JTimer;
    FTimerTask: JDWTimerTask;
    FTimerTaskDelegate: JDWTimerTaskDelegate;
    FOnRun: TNotifyEvent;
  protected
    procedure DoRun; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    procedure Schedule(const AInterval: Int64);
    property OnRun: TNotifyEvent read FOnRun write FOnRun;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Android
  Androidapi.Helpers;

{ TTimerTaskDelegate }

constructor TTimerTaskDelegate.Create(const ATimerTask: TTimerTask);
begin
  inherited Create;
  FTimerTask := ATimerTask;
end;

procedure TTimerTaskDelegate.run;
begin
  FTimerTask.DoRun;
end;

{ TTimerTask }

constructor TTimerTask.Create;
begin
  inherited Create;
  FTimer := TJTImer.JavaClass.init;
  FTimerTaskDelegate := TTimerTaskDelegate.Create(Self);
  FTimerTask := TJDWTimerTask.JavaClass.init(FTimerTaskDelegate);
end;

destructor TTimerTask.Destroy;
begin
  Cancel;
  inherited;
end;

procedure TTimerTask.DoRun;
begin
  if Assigned(FOnRun) then
    FOnRun(Self);
end;

procedure TTimerTask.Cancel;
begin
  FTimer.cancel; // Cancels any scheduled tasks
end;

procedure TTimerTask.Schedule(const AInterval: Int64);
begin
  FTimer.schedule(TJTimerTask.Wrap(TAndroidHelper.JObjectToID(FTimerTask)), AInterval, AInterval);
end;

end.
