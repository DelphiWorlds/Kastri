unit DW.RunProcess;

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

type
  TProcessOutputEvent = procedure(Sender: TObject; const Output: string) of object;
  TProcessTerminatedEvent = procedure(Sender: TObject; const ExitCode: Cardinal) of object;

  /// <summary>
  ///   Common class for running processes on any supported platform
  /// </summary>
  TBaseRunProcess = class(TObject)
  private
    FCurrentPath: string;
    FExitCode: Cardinal;
    FShowExecuting: Boolean;
    FShowCommandInLog: Boolean;
    FWaitInterval: Integer;
    FOnProcessPartialOutput: TProcessOutputEvent;
    FOnProcessOutput: TProcessOutputEvent;
    FOnProcessTerminated: TProcessTerminatedEvent;
  protected
    FCapturedOutput: TArray<string>;
    procedure DoOutput(const AOutput: string); virtual;
    procedure DoPartialOutput(const AOutput: string); virtual;
    procedure DoTerminated(const AExitCode: Cardinal); virtual;
    function GetIsRunning: Boolean; virtual;
    procedure InternalDoOutput(const AOutput: string);
    procedure InternalDoTerminated(const AExitCode: Cardinal);
    property OnProcessOutput: TProcessOutputEvent read FOnProcessOutput write FOnProcessOutput;
    property OnProcessPartialOutput: TProcessOutputEvent read FOnProcessPartialOutput write FOnProcessPartialOutput;
    property OnProcessTerminated: TProcessTerminatedEvent read FOnProcessTerminated write FOnProcessTerminated;
  public
    constructor Create;
    /// <summary>
    ///   Run the process asynchronously. Returns True if the process was started
    /// </summary>
    function Run: Boolean; virtual;
    /// <summary>
    ///   Run the process synchronously. Returns 0 if the process was started, -1 if it was not started, and 1 if timeout
    /// </summary>
    function RunAndWait(const ATimeout: Cardinal = 0): Integer; virtual; // 0 = infinite
    /// <summary>
    ///   Terminate the process
    /// </summary>
    procedure Terminate; virtual;
    /// <summary>
    ///   Path that the process should run from
    /// </summary>
    property CurrentPath: string read FCurrentPath write FCurrentPath;
    /// <summary>
    ///   Indicates whether or not the process is running
    /// </summary>
    property IsRunning: Boolean read GetIsRunning;
    /// <summary>
    ///   Output captured from the process
    /// </summary>
    property CapturedOutput: TArray<string> read FCapturedOutput;
    /// <summary>
    ///   Exit code of the process when it terminated
    /// </summary>
    property ExitCode: Cardinal read FExitCode;
    /// <summary>
    ///   Amount of time (in ms) the wait cycle should wait for
    /// </summary>
    property WaitInterval: Integer read FWaitInterval write FWaitInterval;
    /// <summary>
    ///   Indicates that the output should show what the command is executing
    /// </summary>
    property ShowExecuting: Boolean read FShowExecuting write FShowExecuting;
    /// <summary>
    ///   Indicates that what the command is executing should be emitted as a system log entry
    /// </summary>
    property ShowCommandInLog: Boolean read FShowCommandInLog write FShowCommandInLog;
  end;

implementation

uses
  // RTL
  System.Classes;

const
  cWaitIntervalDefault = 100;

{ TBaseRunProcess }

constructor TBaseRunProcess.Create;
begin
  inherited;
  FWaitInterval := cWaitIntervalDefault;
end;

procedure TBaseRunProcess.InternalDoOutput(const AOutput: string);
var
  LStrings: TStrings;
begin
  if Assigned(FOnProcessOutput) then
  begin
    LStrings := TStringList.Create;
    try
      LStrings.Text := AOutput;
      while LStrings.Count > 0 do
      begin
        FOnProcessOutput(Self, LStrings[0]);
        LStrings.Delete(0);
      end;
    finally
      LStrings.Free;
    end;
  end;
end;

procedure TBaseRunProcess.InternalDoTerminated(const AExitCode: Cardinal);
begin
  FExitCode := AExitCode;
  if Assigned(FOnProcessTerminated) then
    FOnProcessTerminated(Self, AExitCode);
end;

procedure TBaseRunProcess.DoOutput(const AOutput: string);
begin
  InternalDoOutput(AOutput);
end;

procedure TBaseRunProcess.DoPartialOutput(const AOutput: string);
begin
  if Assigned(FOnProcessPartialOutput) then
    FOnProcessPartialOutput(Self, AOutput);
end;

procedure TBaseRunProcess.DoTerminated(const AExitCode: Cardinal);
begin
  InternalDoTerminated(AExitCode);
end;

function TBaseRunProcess.GetIsRunning: Boolean;
begin
  Result := False;
end;

function TBaseRunProcess.Run: Boolean;
begin
  Result := False;
end;

function TBaseRunProcess.RunAndWait(const ATimeout: Cardinal = 0): Integer;
begin
  Result := -1; // Failed to start
end;

procedure TBaseRunProcess.Terminate;
begin
  //
end;

end.
