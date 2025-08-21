unit DW.RunProcess.Android;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}


// NOTE: This unit is a work in progress, hence a number of log statements remaining
//   It is unknown whether the Write and WriteLine methods actually work

interface

uses
  // Android
  Androidapi.JNI.JavaTypes,
  // DW
  DW.RunProcess, DW.Android.Helpers, DW.Androidapi.JNI.Lang, DW.Androidapi.JNI.JavaTypes;

type
  TProcessRunnable = class;

  TInputStreamRunnable = class(THandlerRunnable)
  private
    FReader: JBufferedReader;
    FProcessRunnable: TProcessRunnable;
  protected
    procedure DoRun; override;
    function GetInputStream: JInputStream; virtual;
    procedure Execute; override;
    property ProcessRunnable: TProcessRunnable read FProcessRunnable;
  public
    constructor Create(const AProcessRunnable: TProcessRunnable);
  end;

  TStdOutRunnable = class(TInputStreamRunnable, JRunnable)
  protected
    function GetInputStream: JInputStream; override;
  end;

  TStdErrRunnable = class(TInputStreamRunnable, JRunnable)
  protected
    function GetInputStream: JInputStream; override;
  end;

  TRunProcess = class;

  TProcessRunnable = class(THandlerRunnable, JRunnable)
  private
    FCommandLine: string;
    FProcess: JProcess;
    FRunProcess: TRunProcess;
    FStdErrRunnable: TStdErrRunnable;
    FStdOutRunnable: TStdOutRunnable;
    procedure DoTerminated(const AExitCode: Integer);
  protected
    procedure DoOutput(const AOutput: string);
    procedure DoRun; override;
    procedure Execute; override;
    procedure Write(const AText: string; const ANewLine: Boolean);
    property Process: JProcess read FProcess;
  public
    constructor Create(const ARunProcess: TRunProcess);
    destructor Destroy; override;
  end;

  TRunProcess = class(TBaseRunProcess)
  private
    FCmdLine: string;
    FIsRunning: Boolean;
    FIsTerminated: Boolean;
    FParams: string;
    FProcessRunnable: TProcessRunnable;
  protected
    function GetIsRunning: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Run: Boolean; override;
    procedure Terminate; override;
    procedure Write(const AText: string; const ANewLine: Boolean = False);
    procedure WriteLine(const AText: string);
    property CommandLine: string read FCmdLine write FCmdLine;
    property IsTerminated: Boolean read FIsTerminated;
    property Params: string read FParams write FParams;
    property OnProcessOutput;
    property OnProcessTerminated;
  end;

implementation

uses
  DW.OSLog,
  // RTL
  System.SysUtils, System.Classes,
  // Android
  Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI;

{ TProcessRunnable }

constructor TProcessRunnable.Create(const ARunProcess: TRunProcess);
begin
  inherited Create;
  FRunProcess := ARunProcess;
  FStdOutRunnable := TStdOutRunnable.Create(Self);
  FStdErrRunnable := TStdErrRunnable.Create(Self);
end;

destructor TProcessRunnable.Destroy;
begin
  FStdOutRunnable.Free;
  FStdErrRunnable.Free;
  inherited;
end;

procedure TProcessRunnable.DoRun;
var
  LParams: TArray<string>;
  LExecParams: TJavaObjectArray<JString>;
  I: Integer;
begin
  TOSLog.d('%s.DoRun', [ClassName]);
  LParams := FCommandLine.Split([' '], '"');
  LExecParams := TJavaObjectArray<JString>.Create(Length(LParams));
  try
    for I := 0 to LExecParams.Length - 1 do
    begin
      TOSLog.d('> Param %d: %s', [I, LParams[I]]);
      LExecParams.Items[I] := StringToJString(LParams[I]);
    end;
    FProcess := TJRuntime.JavaClass.getRuntime.exec(LExecParams);
    TOSLog.d('> Created Process');
  finally
    LExecParams.Free;
  end;
  FStdOutRunnable.Execute;
  FStdErrRunnable.Execute;
  TOSLog.d('> WaitFor');
  DoTerminated(FProcess.waitFor);
  TOSLog.d('> Finished');
end;

procedure TProcessRunnable.DoOutput(const AOutput: string);
begin
  TOSLog.d('%s.DoOutput: %s', [ClassName, AOutput]);
  TThread.Queue(nil, procedure begin FRunProcess.DoOutput(AOutput); end);
end;

procedure TProcessRunnable.DoTerminated(const AExitCode: Integer);
begin
  TThread.Queue(nil, procedure begin FRunProcess.DoTerminated(AExitCode); end);
end;

procedure TProcessRunnable.Execute;
begin
  FCommandLine := FRunProcess.CommandLine;
  inherited;
end;

procedure TProcessRunnable.Write(const AText: string; const ANewLine: Boolean);
var
  LTextBytes: TBytes;
  LJavaBytes: TJavaArray<System.Byte>;
begin
  if ANewLine then
    LTextBytes := TEncoding.Default.GetBytes(AText + #13) // + #10?
  else
    LTextBytes := TEncoding.Default.GetBytes(AText);
  LJavaBytes := TBytesToTJavaArray(LTextBytes);
  try
    FProcess.getOutputStream.write(LJavaBytes);
  finally
    LJavaBytes.Free;
  end;
end;

{ TInputStreamRunnable }

constructor TInputStreamRunnable.Create(const AProcessRunnable: TProcessRunnable);
begin
  inherited Create;
  FProcessRunnable := AProcessRunnable;
end;

procedure TInputStreamRunnable.DoRun;
var
  LLine: JString;
begin
  TOSLog.d('%s.DoRun', [ClassName]);
  repeat
    LLine := FReader.readLine;
    if LLine <> nil then
      ProcessRunnable.DoOutput(JStringToString(LLine));
  until LLine = nil;
end;

procedure TInputStreamRunnable.Execute;
var
  LInputStream: JInputStream;
begin
  TOSLog.d('%s.Execute', [ClassName]);
  LInputStream := GetInputStream;
  if LInputStream <> nil then
  begin
    FReader := TJBufferedReader.JavaClass.init(TJInputStreamReader.JavaClass.init(GetInputStream));
    inherited;
  end
  else
    TOSLog.d('> Could not obtain input stream');
  inherited;
end;

function TInputStreamRunnable.GetInputStream: JInputStream;
begin
  Result := nil;
end;

{ TStdOutRunnable }

function TStdOutRunnable.GetInputStream: JInputStream;
begin
  if FProcessRunnable.Process <> nil then
    Result := FProcessRunnable.Process.getInputStream
  else
    Result := inherited;
end;

{ TStdErrRunnable }

function TStdErrRunnable.GetInputStream: JInputStream;
begin
  if FProcessRunnable.Process <> nil then
    Result := FProcessRunnable.Process.getErrorStream
  else
    Result := inherited;
end;

{ TRunProcess }

constructor TRunProcess.Create;
begin
  inherited;
  FProcessRunnable := TProcessRunnable.Create(Self);
end;

destructor TRunProcess.Destroy;
begin
  FProcessRunnable.Free;
  inherited;
end;

function TRunProcess.GetIsRunning: Boolean;
begin
  Result := FIsRunning;
end;

function TRunProcess.Run: Boolean;
begin
  FProcessRunnable.Execute;
  FIsRunning := True;
  Result := FIsRunning;
end;

procedure TRunProcess.Terminate;
begin
  // To be implemented in the future
end;

procedure TRunProcess.Write(const AText: string; const ANewLine: Boolean = False);
begin
  if FIsRunning then
    FProcessRunnable.Write(AText, ANewLine);
end;

procedure TRunProcess.WriteLine(const AText: string);
begin
  Write(AText, True);
end;

end.
