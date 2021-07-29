unit DW.RunProcess.Win;

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
  System.Classes,
  // JVCL
  JvCreateProcess,
  // DW
  DW.RunProcess;

type
  /// <summary>
  ///   Custom class for running Windows processes
  /// </summary>
  /// <remarks>
  ///   Derive from this class when event handlers are not required
  /// </remarks>
  TCustomRunProcess = class(TBaseRunProcess)
  private
    FIsRunning: Boolean;
    FOutput: string;
    FProcess: TJvCreateProcess;
    procedure ProcessMessage;
    procedure ProcessReadHandler(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
    procedure ProcessTerminateHandler(Sender: TObject; ExitCode: Cardinal);
  protected
    procedure DoOutput(const AOutput: string); override;
    function GetIsRunning: Boolean; override;
    function InternalRun: Boolean;
    function MakeWorkingPath: string;
    property Process: TJvCreateProcess read FProcess;
  public
    constructor Create;
    destructor Destroy; override;
    function RunAndWait(const ATimeout: Integer = 0): Boolean; override;
    procedure Terminate; override;
  end;

  /// <summary>
  ///   Concrete class for running Windows processes
  /// </summary>
  TRunProcess = class(TCustomRunProcess)
  private
    function GetCommandLine: string;
    procedure SetCommandLine(const Value: string);
  public
    function Run: Boolean; override;
    procedure WriteInput(const AInput: string; const AIncludeReturn: Boolean = True);
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property OnProcessOutput;
    property OnProcessPartialOutput;
    property OnProcessTerminated;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils, System.DateUtils,
  // Windows
  Winapi.Windows,
  // DW
  DW.OSLog;

{ TCustomRunProcess }

constructor TCustomRunProcess.Create;
begin
  inherited;
  FProcess := TJvCreateProcess.Create(nil);
  FProcess.ConsoleOptions := [TJvConsoleOption.coRedirect];
  FProcess.StartupInfo.DefaultWindowState := False;
  FProcess.StartupInfo.ShowWindow := TJvCPSShowWindow.swHide;
  FProcess.OnRead := ProcessReadHandler;
  FProcess.OnTerminate := ProcessTerminateHandler;
end;

destructor TCustomRunProcess.Destroy;
begin
  Terminate;
  FProcess.Free;
  inherited;
end;

procedure TCustomRunProcess.DoOutput(const AOutput: string);
begin
  FCapturedOutput := FCapturedOutput + [AOutput];
  inherited;
end;

procedure TCustomRunProcess.ProcessMessage;
var
  LMsg: TMsg;
begin
  if GetMessage(LMsg, 0, 0, 0) then
  begin
    TranslateMessage(LMsg);
    DispatchMessage(LMsg);
  end;
end;

procedure TCustomRunProcess.ProcessReadHandler(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
begin
  if (FOutput <> '') and StartsOnNewLine then
    DoOutput(FOutput);
  DoPartialOutput(S);
  FOutput := S;
end;

procedure TCustomRunProcess.ProcessTerminateHandler(Sender: TObject; ExitCode: Cardinal);
begin
  FIsRunning := False;
  DoTerminated(ExitCode);
end;

function TCustomRunProcess.RunAndWait(const ATimeout: Integer): Boolean;
var
  LStart: TDateTime;
begin
  Result := False;
  if InternalRun then
  begin
    LStart := Now;
    while IsRunning and (MilliSecondsBetween(Now, LStart) < ATimeout) do
      ProcessMessage;
    Result := not IsRunning;
  end;
end;

procedure TCustomRunProcess.Terminate;
begin
  if IsRunning then
    FProcess.Terminate;
end;

function TCustomRunProcess.GetIsRunning: Boolean;
begin
  Result := FIsRunning;
end;

function TCustomRunProcess.InternalRun: Boolean;
var
  LExecuting: string;
begin
  FCapturedOutput := [];
  LExecuting := 'Executing: ' + FProcess.CommandLine;
  if ShowExecuting then
    DoOutput(LExecuting);
  if ShowCommandInLog then
    TOSLog.d(LExecuting);
  FProcess.Run;
  FIsRunning := True;
  Result := True;
end;

function TCustomRunProcess.MakeWorkingPath: string;
var
  LGUID: string;
begin
  LGUID := TGUID.NewGuid.ToString;
  Result := TPath.Combine(TPath.GetTempPath, LGUID.Substring(1, LGUID.Length - 2));
  ForceDirectories(Result);
  if not TDirectory.Exists(Result) then
    TOSLog.d('Unable to create working path: %s', [Result]);
end;

{ TRunProcess }

function TRunProcess.Run: Boolean;
begin
  Result := InternalRun;
end;

procedure TRunProcess.SetCommandLine(const Value: string);
begin
  FProcess.CommandLine := Value;
end;

procedure TRunProcess.WriteInput(const AInput: string; const AIncludeReturn: Boolean = True);
begin
  if AIncludeReturn then
    FProcess.WriteLn(AInput)
  else
    FProcess.Write(AInput);
end;

function TRunProcess.GetCommandLine: string;
begin
  Result := FProcess.CommandLine;
end;

end.
