unit DW.RunProcess.Win;

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

{$SCOPEDENUMS ON}

uses
  // RTL
  System.Classes,
  // Windows
  Winapi.Windows, Winapi.Messages,
  // DW
  DW.RunProcess;

type
  TWindowsProcess = class;

  TProcessRawReadEvent = procedure(Sender: TObject; const Data: string) of object;
  TProcessReadEvent = procedure(Sender: TObject; const Data: string; const StartsOnNewLine: Boolean) of object;
  TProcessTerminateEvent = procedure(Sender: TObject; ExitCode: DWORD) of object;

  TProcessReader = class(TObject)
  private
    FConsoleOutput: TStrings;
    FProcess: TWindowsProcess;
    FOnRawRead: TProcessRawReadEvent;
    FOnRead: TProcessReadEvent;
    function GetConsoleOutput: TStrings;
  public
    constructor Create(const AProcess: TWindowsProcess); virtual;
    destructor Destroy; override;
    property ConsoleOutput: TStrings read GetConsoleOutput;
    property OnRead: TProcessReadEvent read FOnRead write FOnRead;
    property OnRawRead: TProcessRawReadEvent read FOnRawRead write FOnRawRead;
  end;

  TProcessState = (Ready, Running, Waiting);
  TProcessFlag = (DefaultErrorMode, NewConsole, NewProcGroup, SeparateWdm, SharedWdm, Suspended, Unicode, Detached);
  TProcessFlags = set of TProcessFlag;
  TProcessPriority = (Idle, Normal, High, RealTime, BelowNormal, AboveNormal);

  TWindowsProcess = class(TObject)
  private
    FApplicationName: string;
    FCommandLine: string;
    FCreationFlags: TProcessFlags;
    FCurrentDirectory: string;
    FErrorReader: TProcessReader;
    FExitCode: Cardinal;
    FHandle: THandle;
    FInputReader: TProcessReader;
    FPriority: TProcessPriority;
    FProcessInfo: TProcessInformation;
    FRunningThreadCount: Integer;
    FState: TProcessState;
    FWaitForTerminate: Boolean;
    FWaitThread: TThread;
    FOnTerminate: TProcessTerminateEvent;
    function GetConsoleOutput: TStrings;
    function GetHandle: THandle;
    function GetOnErrorRawRead: TProcessRawReadEvent;
    function GetOnErrorRead: TProcessReadEvent;
    function GetOnRawRead: TProcessRawReadEvent;
    function GetOnRead: TProcessReadEvent;
    procedure GotoReadyState;
    procedure GotoWaitState(const AThreadCount: Integer);
    procedure SetCommandLine(const Value: string);
    procedure SetOnErrorRawRead(const Value: TProcessRawReadEvent);
    procedure SetOnErrorRead(const Value: TProcessReadEvent);
    procedure SetOnRawRead(const Value: TProcessRawReadEvent);
    procedure SetOnRead(const Value: TProcessReadEvent);
    procedure SetWaitForTerminate(const Value: Boolean);
    procedure WaitThreadTerminated(Sender: TObject);
  protected
    procedure CheckReady;
    procedure CheckRunning;
    procedure CheckNotWaiting;
    procedure CloseProcessHandles;
    procedure CloseRead;
    procedure HandleReadEvent(Sender: TObject);
    procedure HandleThreadTerminated;
    procedure TerminateWaitThread;
    procedure WndProc(var AMsg: TMessage);
    property Handle: THandle read GetHandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseWrite;
    procedure Run;
    procedure StopWaiting;
    procedure Terminate;
    function Write(const S: AnsiString): Boolean;
    function WriteLn(const S: AnsiString): Boolean;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property CommandLine: string read FCommandLine write SetCommandLine;
    property ConsoleOutput: TStrings read GetConsoleOutput;
    property CurrentDirectory: string read FCurrentDirectory write FCurrentDirectory;
    property CreationFlags: TProcessFlags read FCreationFlags write FCreationFlags default [TProcessFlag.Unicode];
    property ErrorReader: TProcessReader read FErrorReader;
    property InputReader: TProcessReader read FInputReader;
    property Priority: TProcessPriority read FPriority write FPriority default TProcessPriority.Normal;
    property ProcessInfo: TProcessInformation read FProcessInfo;
    property State: TProcessState read FState;
    property WaitForTerminate: Boolean read FWaitForTerminate write SetWaitForTerminate default True;
    property OnErrorRead: TProcessReadEvent read GetOnErrorRead write SetOnErrorRead;
    property OnErrorRawRead: TProcessRawReadEvent read GetOnErrorRawRead write SetOnErrorRawRead;
    property OnRead: TProcessReadEvent read GetOnRead write SetOnRead;
    property OnRawRead: TProcessRawReadEvent read GetOnRawRead write SetOnRawRead;
    property OnTerminate: TProcessTerminateEvent read FOnTerminate write FOnTerminate;
  end;

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
    FProcess: TWindowsProcess;
    function ProcessMessage(var AMsg: TMsg): Boolean; overload;
    procedure ProcessMessage; overload;
    procedure ProcessMessages;
    procedure ProcessReadHandler(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
    procedure ProcessTerminateHandler(Sender: TObject; ExitCode: Cardinal);
  protected
    procedure DoOutput(const AOutput: string); override;
    function GetIsRunning: Boolean; override;
    function InternalRun: Boolean;
    property Process: TWindowsProcess read FProcess;
  public
    class function MakeWorkingPath: string;
  public
    constructor Create;
    destructor Destroy; override;
    function RunAndWait(const ATimeout: Cardinal = 0): Integer; override;
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
    // procedure WriteInput(const AInput: string; const AIncludeReturn: Boolean = True);
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property OnProcessOutput;
    property OnProcessPartialOutput;
    property OnProcessTerminated;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils, System.DateUtils, System.Math, System.SyncObjs,
  // DW
  DW.OSLog;

resourcestring
  SErrorProcessIsRunning = 'Cannot perform this operation when process is running';
  SErrorProcessNotRunning = 'Process is not running';

const
  CRLF = #13#10;
  CR = #13;
  LF = #10;
  Backspace = #8;
  Tab = #9;
  Esc = #27;
  Del = #127;

  CM_READ = WM_USER + 1;
  CM_THREADTERMINATED = WM_USER + 2;

  cProcessPriorityValues: array [TProcessPriority] of DWORD = (
    IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS, BELOW_NORMAL_PRIORITY_CLASS,
    ABOVE_NORMAL_PRIORITY_CLASS
  );

  cUtilWindowExClass: TWndClass = (
    style: 0;
    lpfnWndProc: nil;
    cbClsExtra: 0;
    cbWndExtra: SizeOf(TMethod);
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindowEx'
  );

  cBufferSize = 1024;
  cBufferSizeMax = 65536;

type
  {$IFDEF CPU32BITS}
  SizeInt = Integer;
  {$ENDIF}
  {$IFDEF CPU64BITS}
  SizeInt = NativeInt;
  {$ENDIF}

  ERunProcessError = Exception;

  TProcessBuffer = array[0..cBufferSize - 1] of AnsiChar;

  TProcessWaitThread = class(TThread)
  private
    FExitCode: DWORD;
    FCloseEvent: THandle;
    FProcessHandle: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(const AProcessHandle: DWORD);
    destructor Destroy; override;
    procedure TerminateThread;
  end;

  TProcessWriteThread = class(TProcessWaitThread)
  private
    FOutputBuffer: TProcessBuffer;
    FOutputBufferEnd: Cardinal;
    FWriteHandle: THandle;
    FWriteEvent: THandle;
    FWriteLock: TCriticalSection;
  protected
    procedure Execute; override;
    function TryWrite: Boolean;
  public
    constructor Create(const AProcessHandle: DWORD; const AWriteHandle: THandle);
    destructor Destroy; override;
    function Write(const AData: AnsiString): Boolean;
    procedure CloseWrite;
  end;

  TProcessReadThread = class(TThread)
  private
    FDestHandle: THandle;
    FInputBuffer: PAnsiChar;
    FInputBufferEnd: Cardinal;
    FInputBufferSize: Cardinal;
    FOwner: TObject;
    FPreBuffer: PAnsiChar;
    FReadHandle: THandle;
    FReadLock: TCriticalSection;
  protected
    procedure CopyToBuffer(const ABuffer: PAnsiChar; const ASize: Cardinal);
    procedure Execute; override;
  public
    constructor Create(const AOwner: TObject; const AReadHandle, ADestHandle: THandle);
    destructor Destroy; override;
    procedure CloseRead;
    function ReadBuffer(var ABuffer: TProcessBuffer; out ABufferSize: Cardinal): Boolean;
    procedure TerminateThread;
  end;

  TProcessOutputReader = class(TProcessReader)
  private
    FCurrentLine: AnsiString;
    FCursorPosition: Integer;
    FStartsOnNewLine: Boolean;
    FParseBuffer: TProcessBuffer;
    FThread: TProcessReadThread;
    procedure ThreadTerminated(Sender: TObject);
  protected
    procedure DoReadEvent(const EndsWithNewLine: Boolean);
    procedure DoRawReadEvent(Data: PAnsiChar; const ASize: Cardinal);
    procedure ParseConsoleOutput(const AData: PAnsiChar; const ASize: Cardinal);
    procedure HandleReadEvent;
  public
    procedure CreateThread(const AReadHandle: THandle);
    procedure CloseRead;
    procedure Terminate;
  end;

  TIOHandles = class(TObject)
  private
    FHandle: array [0..2] of THandle;
    function GetHandle(const AIndex: Integer): THandle;
    procedure SetHandle(const AIndex: Integer; const AValue: THandle);
    function ExtractHandle(const AIndex: Integer): THandle;
  public
    destructor Destroy; override;
    procedure Clear;
    property ExtractRead: THandle index 0 read ExtractHandle;
    property ExtractWrite: THandle index 1 read ExtractHandle;
    property ExtractError: THandle index 2 read ExtractHandle;
    property Read: THandle index 0 read GetHandle write SetHandle;
    property Write: THandle index 1 read GetHandle write SetHandle;
    property Error: THandle index 2 read GetHandle write SetHandle;
  end;

  TCreateDuplicateKind = (InheritableKeepSourceOpen, NotInheritableCloseSource);

function StdWndProc(AWindow: THandle; AMessage, AWParam: WPARAM; ALParam: LPARAM): LRESULT; stdcall;
var
  LMsg: TMessage;
  LWndProc: TWndMethod;
begin
  TMethod(LWndProc).Code := Pointer(GetWindowLongPtr(AWindow, 0));
  TMethod(LWndProc).Data := Pointer(GetWindowLongPtr(AWindow, SizeOf(Pointer)));
  if Assigned(LWndProc) then
  begin
    LMsg.Msg := AMessage;
    LMsg.WParam := AWParam;
    LMsg.LParam := ALParam;
    LMsg.Result := 0;
    LWndProc(LMsg);
    Result := LMsg.Result;
  end
  else
    Result := DefWindowProc(AWindow, AMessage, AWParam, ALParam);
end;

function AllocateHWndEx(const AMethod: TWndMethod; const AClassName: string = ''): THandle;
var
  LWndClass: TWndClass;
  LUtilWindowExClass: TWndClass;
  LIsClassRegistered: Boolean;
begin
  LUtilWindowExClass := cUtilWindowExClass;
  LUtilWindowExClass.hInstance := HInstance;
  LUtilWindowExClass.lpfnWndProc := @DefWindowProc;
  if not AClassName.IsEmpty then
    LUtilWindowExClass.lpszClassName := PChar(AClassName);
  LIsClassRegistered := GetClassInfo(HInstance, LUtilWindowExClass.lpszClassName, LWndClass);
  if not LIsClassRegistered or (LWndClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if LIsClassRegistered then
      UnregisterClass(LUtilWindowExClass.lpszClassName, HInstance);
    RegisterClass(LUtilWindowExClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, LUtilWindowExClass.lpszClassName, '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if Assigned(AMethod) then
  begin
    SetWindowLongPtr(Result, 0, LONG_PTR(TMethod(AMethod).Code));
    SetWindowLongPtr(Result, SizeOf(TMethod(AMethod).Code), LONG_PTR(TMethod(AMethod).Data));
    SetWindowLongPtr(Result, GWLP_WNDPROC, LONG_PTR(@StdWndProc));
  end;
end;

procedure DeallocateHWndEx(Wnd: THandle);
begin
  DestroyWindow(Wnd);
end;

function OSCheck(RetVal: Boolean): Boolean;
begin
  if not RetVal then
    RaiseLastOSError;
  Result := RetVal;
end;

function InternalTerminateProcess(ProcessID: DWORD): Boolean;
var
  LProcessHandle: THandle;
begin
  LProcessHandle := OpenProcess(PROCESS_TERMINATE, False, ProcessID);
  OSCheck(LProcessHandle <> 0);
  Result := TerminateProcess(LProcessHandle, 0);
  CloseHandle(LProcessHandle);
end;

function SafeCloseHandle(var AHandle: THandle): Boolean;
begin
  if AHandle <> 0 then
  begin
    Result := CloseHandle(AHandle);
    if Result then
      AHandle := 0;
  end
  else
    Result := True;
end;

function CreateDuplicate(AHandle: THandle; const Kind: TCreateDuplicateKind): THandle;
const
  cCloseActions: array [TCreateDuplicateKind] of DWORD = (0, DUPLICATE_CLOSE_SOURCE);
begin
  OSCheck(DuplicateHandle(GetCurrentProcess, AHandle, GetCurrentProcess, @Result, 0, Kind = TCreateDuplicateKind.InheritableKeepSourceOpen,
    DUPLICATE_SAME_ACCESS or cCloseActions[Kind]));
end;

procedure ConstructPipe(const ALocalHandles, AConsoleHandles: TIOHandles; const ASeparateError: Boolean);
var
  LSecAttr: TSecurityAttributes;
  LSecDescr: TSecurityDescriptor;
  LReadHandle, LWriteHandle: THandle;
begin
  FillChar(LSecAttr, SizeOf(TSecurityAttributes), 0);
  LSecAttr.nLength := SizeOf(TSecurityAttributes);
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    InitializeSecurityDescriptor(@LSecDescr, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@LSecDescr, True, nil, False);
    LSecAttr.lpSecurityDescriptor := @LSecDescr;
  end
  else
    LSecAttr.lpSecurityDescriptor := nil;
  LSecAttr.bInheritHandle := True;
  if AConsoleHandles.Write = 0 then
  begin
    OSCheck(CreatePipe(LReadHandle, LWriteHandle, @LSecAttr, 0));
    AConsoleHandles.Write := LWriteHandle;
    ALocalHandles.Read := CreateDuplicate(LReadHandle, TCreateDuplicateKind.NotInheritableCloseSource);
  end;
  if AConsoleHandles.Error = 0 then
  begin
    if ASeparateError then
    begin
      OSCheck(CreatePipe(LReadHandle, LWriteHandle, @LSecAttr, 0));
      AConsoleHandles.Error := LWriteHandle;
      ALocalHandles.Error := CreateDuplicate(LReadHandle, TCreateDuplicateKind.NotInheritableCloseSource);
    end
    else
      AConsoleHandles.Error := CreateDuplicate(AConsoleHandles.Write, TCreateDuplicateKind.InheritableKeepSourceOpen);
  end;
  if AConsoleHandles.Read = 0 then
  begin
    OSCheck(CreatePipe(LReadHandle, LWriteHandle, @LSecAttr, 0));
    AConsoleHandles.Read := LReadHandle;
    ALocalHandles.Write := CreateDuplicate(LWriteHandle, TCreateDuplicateKind.NotInheritableCloseSource);
  end;
end;

{ TIOHandles }

destructor TIOHandles.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TIOHandles.Clear;
begin
  Read := 0;
  Write := 0;
  Error := 0;
end;

function TIOHandles.ExtractHandle(const AIndex: Integer): THandle;
begin
  Result := FHandle[AIndex];
  FHandle[AIndex] := 0;
end;

function TIOHandles.GetHandle(const AIndex: Integer): THandle;
begin
  Result := FHandle[AIndex];
end;

procedure TIOHandles.SetHandle(const AIndex: Integer; const AValue: THandle);
begin
  if AValue <> FHandle[AIndex] then
  begin
    if FHandle[AIndex] <> 0 then
      CloseHandle(FHandle[AIndex]);
    FHandle[AIndex] := AValue;
  end;
end;

{ TProcessReader }

constructor TProcessReader.Create(const AProcess: TWindowsProcess);
begin
  inherited Create;
  FProcess := AProcess;
  FConsoleOutput := TStringList.Create;
end;

destructor TProcessReader.Destroy;
begin
  FConsoleOutput.Free;
  inherited Destroy;
end;

function TProcessReader.GetConsoleOutput: TStrings;
begin
  Result := FConsoleOutput;
end;

{ TProcessWaitThread }

constructor TProcessWaitThread.Create(const AProcessHandle: DWORD);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Priority := tpLower;
  FCloseEvent := CreateEvent(nil, True, False, nil);
  FProcessHandle := AProcessHandle;
end;

destructor TProcessWaitThread.Destroy;
begin
  SafeCloseHandle(FCloseEvent);
  inherited Destroy;
end;

procedure TProcessWaitThread.Execute;
var
  LWaitHandles: array [0..1] of THandle;
begin
  LWaitHandles[0] := FCloseEvent;
  LWaitHandles[1] := FProcessHandle;
  WaitForInputIdle(FProcessHandle, INFINITE);
  case WaitForMultipleObjects(2, PWOHandleArray(@LWaitHandles[0]), False, INFINITE) of
    WAIT_OBJECT_0:
      FExitCode := MAXDWORD;
    WAIT_OBJECT_0 + 1:
      GetExitCodeProcess(FProcessHandle, FExitCode);
  else
    RaiseLastOSError;
  end;
end;

procedure TProcessWaitThread.TerminateThread;
begin
  Terminate;
  SetEvent(FCloseEvent);
end;

{ TProcessReadThread }

constructor TProcessReadThread.Create(const AOwner: TObject; const AReadHandle, ADestHandle: THandle);
begin
  inherited Create(True);
  FOwner := AOwner;
  FreeOnTerminate := True;
  Priority := tpLower;
  FReadLock := TCriticalSection.Create;
  FReadHandle := AReadHandle;
  FDestHandle := ADestHandle;
  FInputBuffer := nil;
  FInputBufferSize := cBufferSize;
  FInputBufferEnd := 0;
  ReallocMem(FInputBuffer, FInputBufferSize * SizeOf(Byte));
  GetMem(FPreBuffer, cBufferSize);
end;

destructor TProcessReadThread.Destroy;
begin
  SafeCloseHandle(FReadHandle);
  inherited Destroy;
  FreeMem(FInputBuffer);
  FReadLock.Free;
  FreeMem(FPreBuffer);
end;

procedure TProcessReadThread.CloseRead;
begin
  FReadLock.Acquire;
  try
    SafeCloseHandle(FReadHandle);
  finally
    FReadLock.Release;
  end;
end;

procedure TProcessReadThread.CopyToBuffer(const ABuffer: PAnsiChar; const ASize: Cardinal);
begin
  FReadLock.Acquire;
  try
    if FInputBufferEnd + ASize > FInputBufferSize then
    begin
      if FInputBufferSize > cBufferSizeMax then
        FInputBufferEnd := 0
      else
      begin
        FInputBufferSize := FInputBufferSize * 2;
        ReallocMem(FInputBuffer, FInputBufferSize * SizeOf(Byte));
      end;
    end;
    Move(ABuffer[0], FInputBuffer[FInputBufferEnd], ASize);
    Inc(FInputBufferEnd, ASize);
  finally
    FReadLock.Release;
  end;
  PostMessage(FDestHandle, CM_READ, WPARAM(FOwner), 0);
end;

procedure TProcessReadThread.Execute;
var
  LBytesRead: Cardinal;
begin
  while not Terminated do
  begin
    if not ReadFile(FReadHandle, FPreBuffer[0], cBufferSize, LBytesRead, nil) then
    begin
      if GetLastError = ERROR_BROKEN_PIPE then
        Break;
    end
    else
      CopyToBuffer(FPreBuffer, LBytesRead);
  end;
end;

function TProcessReadThread.ReadBuffer(var ABuffer: TProcessBuffer; out ABufferSize: Cardinal): Boolean;
begin
  FReadLock.Acquire;
  try
    Result := FInputBufferEnd > 0;
    if Result then
    begin
      ABufferSize := Min(FInputBufferEnd, cBufferSize);
      Move(FInputBuffer[0], ABuffer[0], ABufferSize * SizeOf(Byte));
      if FInputBufferEnd > ABufferSize then
        Move(FInputBuffer[ABufferSize], FInputBuffer[0], (FInputBufferEnd - ABufferSize) * SizeOf(Byte));
      Dec(FInputBufferEnd, ABufferSize);
    end;
  finally
    FReadLock.Release;
  end;
end;

procedure TProcessReadThread.TerminateThread;
begin
  Terminate;
  CloseRead;
end;

{ TProcessWriteThread }

constructor TProcessWriteThread.Create(const AProcessHandle: DWORD; const AWriteHandle: THandle);
begin
  inherited Create(AProcessHandle);
  FWriteLock := TCriticalSection.Create;
  FWriteHandle := AWriteHandle;
  FWriteEvent := CreateEvent(nil, True, False, nil);
end;

destructor TProcessWriteThread.Destroy;
begin
  SafeCloseHandle(FWriteHandle);
  SafeCloseHandle(FWriteEvent);
  inherited Destroy;
  FWriteLock.Free;
end;

procedure TProcessWriteThread.CloseWrite;
begin
  FWriteLock.Acquire;
  try
    SafeCloseHandle(FWriteHandle);
  finally
    FWriteLock.Release;
  end;
end;

procedure TProcessWriteThread.Execute;
var
  LWaitHandles: array [0..2] of THandle;
  LHandleCount: Cardinal;
begin
  LWaitHandles[0] := FCloseEvent;
  LWaitHandles[1] := FProcessHandle;
  LWaitHandles[2] := FWriteEvent;
  LHandleCount := Length(LWaitHandles);
  WaitForInputIdle(FProcessHandle, INFINITE);
  while not Terminated do
  begin
    case WaitForMultipleObjects(LHandleCount, PWOHandleArray(@LWaitHandles[0]), False, INFINITE) of
      WAIT_OBJECT_0:
      begin
        FExitCode := MAXDWORD;
        Break;
      end;
      WAIT_OBJECT_0 + 1:
      begin
        GetExitCodeProcess(FProcessHandle, FExitCode);
        Break;
      end;
      WAIT_OBJECT_0 + 2:
      begin
        if not TryWrite then
          LHandleCount := 2;
      end;
    else
      Break;
    end;
  end;
end;

function TProcessWriteThread.TryWrite: Boolean;
var
  LBytesWritten: Cardinal;
  LBytesToWrite: Cardinal;
begin
  FWriteLock.Acquire;
  try
    try
      if (FWriteHandle <> 0) and (FOutputBufferEnd > 0) then
      begin
        LBytesToWrite := FOutputBufferEnd;
        if not WriteFile(FWriteHandle, FOutputBuffer, LBytesToWrite, LBytesWritten, nil) then
        begin
          if (GetLastError = ERROR_NO_DATA) or (GetLastError = ERROR_BROKEN_PIPE) then
            SafeCloseHandle(FWriteHandle);
        end
        else if (LBytesWritten > 0) and (LBytesWritten < LBytesToWrite) then
          Move(FOutputBuffer[LBytesWritten], FOutputBuffer[0], LBytesToWrite - LBytesWritten);
        Dec(FOutputBufferEnd, LBytesWritten);
      end;
    finally
      Result := FWriteHandle <> 0;
      if FOutputBufferEnd = 0 then
        ResetEvent(FWriteEvent);
    end;
  finally
    FWriteLock.Release;
  end;
end;

function TProcessWriteThread.Write(const AData: AnsiString): Boolean;
begin
  if Length(AData) > 0 then
  begin
    FWriteLock.Acquire;
    try
      Result := FWriteHandle <> 0;
      if Result then
      begin
        Result := Cardinal(Length(AData)) + FOutputBufferEnd <= cBufferSize;
        if Result then
        begin
          Move(PAnsiChar(AData)^, FOutputBuffer[FOutputBufferEnd], Length(AData));
          Inc(FOutputBufferEnd, Length(AData));
          if FOutputBufferEnd > 0 then
            SetEvent(FWriteEvent);
        end;
      end;
    finally
      FWriteLock.Release;
    end;
  end
  else
    Result := True;
end;

{ TProcessOutputReader }

procedure TProcessOutputReader.CloseRead;
begin
  if Assigned(FThread) then
    FThread.CloseRead;
end;

procedure TProcessOutputReader.CreateThread(const AReadHandle: THandle);
begin
  FStartsOnNewLine := True;
  FCurrentLine := '';
  FCursorPosition := 0;
  FThread := TProcessReadThread.Create(Self, AReadHandle, FProcess.Handle);
  FThread.OnTerminate := ThreadTerminated;
  FThread.Start;
end;

procedure TProcessOutputReader.DoRawReadEvent(Data: PAnsiChar; const ASize: Cardinal);
var
  LString: AnsiString;
begin
  if Assigned(FOnRawRead) then
  begin
    SetString(LString, Data, ASize);
    FOnRawRead(FProcess, string(LString));
  end;
end;

procedure TProcessOutputReader.DoReadEvent(const EndsWithNewLine: Boolean);
begin
  if FStartsOnNewLine or (ConsoleOutput.Count = 0) then
    ConsoleOutput.Add(string(FCurrentLine))
  else
    ConsoleOutput[ConsoleOutput.Count - 1] := string(FCurrentLine);
  if Assigned(FOnRead) then
    FOnRead(FProcess, string(FCurrentLine), FStartsOnNewLine);
  if EndsWithNewLine then
  begin
    FCurrentLine := '';
    FCursorPosition := 0;
  end;
  FStartsOnNewLine := EndsWithNewLine;
end;

procedure TProcessOutputReader.HandleReadEvent;
var
  LSize: Cardinal;
begin
  while (FThread <> nil) and FThread.ReadBuffer(FParseBuffer, LSize) do
    ParseConsoleOutput(FParseBuffer, LSize);
end;

procedure TProcessOutputReader.ParseConsoleOutput(const AData: PAnsiChar; const ASize: Cardinal);
var
  P, Q: PAnsiChar;

  procedure DoOutput;
    { Copy chunk [Q..P) to the current line & Update cursor position }
  var
    ChunkSize: Integer;
  begin
    ChunkSize := P - Q;
    if ChunkSize <= 0 then
      Exit;

    // Does the chunck fit on the current line..
    if Length(FCurrentLine) < FCursorPosition + ChunkSize then
      // .. if not resize current line
      SetLength(FCurrentLine, FCursorPosition + ChunkSize);

    // Move the chunk to the current line
    Move(Q^, (PAnsiChar(FCurrentLine) + FCursorPosition)^, ChunkSize);

    // Update the cursor
    Inc(FCursorPosition, ChunkSize);
  end;

  procedure DoTab;
  begin
    // Does the chunck (8 spaces) fit on the current line..
    if Length(FCurrentLine) < FCursorPosition + 8 then
      // .. if not resize current line }
      SetLength(FCurrentLine, FCursorPosition + 8);

    // Fill 8 spaces on the currentline at the cursor position
    FillChar((PAnsiChar(FCurrentLine) + FCursorPosition)^, 8, #32);

    // Update the cursor
    Inc(FCursorPosition, 8);
  end;

begin
  DoRawReadEvent(AData, ASize);

  P := AData;
  Q := AData;

  while Cardinal(P - AData) < ASize do
    case P^ of
      #0, #7: // NULL and BELL
        begin
          // Replace with space
          P^ := #32;
          Inc(P);
        end;
      Backspace:
        begin
          DoOutput;
          Dec(FCursorPosition);
          if FCursorPosition < 0 then
            FCursorPosition := 0;
          Inc(P);
          Q := P;
        end;
      Tab:
        begin
          // Replace with 8 spaces
          DoOutput;
          DoTab;
          Inc(P);
          Q := P;
        end;
      Lf:
        begin
          DoOutput;
          DoReadEvent(True);
          Inc(P);
          Q := P;
        end;
      Cr:
        begin
          DoOutput;
          FCursorPosition := 0;
          Inc(P);
          Q := P;
        end;
    else
      Inc(P);
    end;
  DoOutput;
  DoReadEvent(False);
end;

procedure TProcessOutputReader.Terminate;
begin
  if FThread <> nil then
  begin
    FThread.OnTerminate := nil;
    FThread.TerminateThread;
    FThread := nil;
  end;
end;

procedure TProcessOutputReader.ThreadTerminated(Sender: TObject);
begin
  HandleReadEvent;
  if FCurrentLine <> '' then
    DoReadEvent(False);
  FThread := nil;
  PostMessage(FProcess.Handle, CM_THREADTERMINATED, 0, 0);
end;

{ TWindowsProcess }

constructor TWindowsProcess.Create;
begin
  inherited Create;
  FCreationFlags := [TProcessFlag.Unicode];
  FPriority := TProcessPriority.Normal;
  FState := TProcessState.Ready;
  FWaitForTerminate := True;
  FErrorReader := TProcessOutputReader.Create(Self);
  FInputReader := TProcessOutputReader.Create(Self);
end;

destructor TWindowsProcess.Destroy;
begin
  TerminateWaitThread;
  FErrorReader.Free;
  FInputReader.Free;
  if FHandle <> 0 then
    DeallocateHWndEx(FHandle);
  CloseProcessHandles;
  inherited Destroy;
end;

procedure TWindowsProcess.CheckNotWaiting;
begin
  if (FState = TProcessState.Waiting) and (FRunningThreadCount > 0) then
    raise ERunProcessError.CreateRes(@SErrorProcessIsRunning);
end;

procedure TWindowsProcess.CheckReady;
begin
  if FState <> TProcessState.Ready then
    raise ERunProcessError.CreateRes(@SErrorProcessIsRunning);
end;

procedure TWindowsProcess.CheckRunning;
begin
  if FState = TProcessState.Ready then
    raise ERunProcessError.CreateRes(@SErrorProcessNotRunning);
end;

procedure TWindowsProcess.CloseProcessHandles;
begin
  OSCheck(SafeCloseHandle(FProcessInfo.hProcess));
  OSCheck(SafeCloseHandle(FProcessInfo.hThread));
end;

procedure TWindowsProcess.CloseRead;
begin
  TProcessOutputReader(FInputReader).CloseRead;
  TProcessOutputReader(FErrorReader).CloseRead;
end;

procedure TWindowsProcess.CloseWrite;
begin
  if FWaitThread is TProcessWriteThread then
    TProcessWriteThread(FWaitThread).CloseWrite;
end;

procedure TWindowsProcess.HandleThreadTerminated;
begin
  if FState = TProcessState.Waiting then
  begin
    Dec(FRunningThreadCount);
    if FRunningThreadCount = 0 then
    begin
      GotoReadyState;
      if Assigned(FOnTerminate) then
        FOnTerminate(Self, FExitCode);
    end;
  end;
end;

function TWindowsProcess.GetConsoleOutput: TStrings;
begin
  Result := FInputReader.ConsoleOutput;
end;

function TWindowsProcess.GetHandle: THandle;
begin
  if FHandle = 0 then
    FHandle := AllocateHWndEx(WndProc);
  Result := FHandle;
end;

function TWindowsProcess.GetOnErrorRawRead: TProcessRawReadEvent;
begin
  Result := FErrorReader.OnRawRead;
end;

function TWindowsProcess.GetOnErrorRead: TProcessReadEvent;
begin
  Result := FErrorReader.OnRead;
end;

function TWindowsProcess.GetOnRawRead: TProcessRawReadEvent;
begin
  Result := FInputReader.OnRawRead;
end;

function TWindowsProcess.GetOnRead: TProcessReadEvent;
begin
  Result := FInputReader.OnRead;
end;

procedure TWindowsProcess.GotoReadyState;
begin
  CheckNotWaiting;
  FState := TProcessState.Ready;
  CloseProcessHandles;
  FRunningThreadCount := 0;
end;

procedure TWindowsProcess.GotoWaitState(const AThreadCount: Integer);
begin
  CheckReady;
  FState := TProcessState.Waiting;
  FRunningThreadCount := AThreadCount;
end;

procedure TWindowsProcess.HandleReadEvent(Sender: TObject);
begin
  TProcessOutputReader(Sender).HandleReadEvent;
end;

// TODO: Clean up
procedure TWindowsProcess.Run;
const
  cCreateFlagsValues: array [TProcessFlag] of DWORD = (
    CREATE_DEFAULT_ERROR_MODE, CREATE_NEW_CONSOLE, CREATE_NEW_PROCESS_GROUP, CREATE_SEPARATE_WOW_VDM, CREATE_SHARED_WOW_VDM, CREATE_SUSPENDED,
    CREATE_UNICODE_ENVIRONMENT, DETACHED_PROCESS
  );
var
  ConsoleHandles: TIOHandles;
  LocalHandles: TIOHandles;
  LStartupInfo: TStartupInfo;
  Flags: DWORD;
  F: TProcessFlag;
  AppName, CurrDir: PChar;
begin
  GotoReadyState;
  FillChar(FProcessInfo, SizeOf(FProcessInfo), #0);
  Flags := cProcessPriorityValues[FPriority];
  for F := Low(TProcessFlag) to High(TProcessFlag) do
    if F in FCreationFlags then
      Inc(Flags, cCreateFlagsValues[F]);
  AppName := Pointer(Trim(FApplicationName));
  CurrDir := Pointer(Trim(FCurrentDirectory));
  LocalHandles := TIOHandles.Create;
  ConsoleHandles := TIOHandles.Create;
  try
    ConstructPipe(LocalHandles, ConsoleHandles, False);
    ZeroMemory(@LStartupInfo, SizeOf(LStartupInfo));
    LStartupInfo.cb := SizeOf(LStartupInfo);
    LStartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    LStartupInfo.wShowWindow := SW_HIDE;
    LStartupInfo.dwFlags := LStartupInfo.dwFlags or STARTF_USESTDHANDLES;
    LStartupInfo.hStdOutput := ConsoleHandles.Write;
    LStartupInfo.hStdInput := ConsoleHandles.Read;
    LStartupInfo.hStdError := ConsoleHandles.Error;
    if not CreateProcess(AppName, PChar(FCommandLine), nil, nil, True, Flags, nil, CurrDir, LStartupInfo, FProcessInfo) then
    begin
      CloseProcessHandles;
      RaiseLastOSError;
    end;
    GotoWaitState(2);
    FWaitThread := TProcessWriteThread.Create(FProcessInfo.hProcess, LocalHandles.ExtractWrite);
    FWaitThread.OnTerminate := WaitThreadTerminated;
    FWaitThread.Start;
    TProcessOutputReader(FInputReader).CreateThread(LocalHandles.ExtractRead);
  finally
    ConsoleHandles.Free;
    LocalHandles.Free;
  end;
end;

procedure TWindowsProcess.SetCommandLine(const Value: string);
begin
  FCommandLine := Value;
  if StringRefCount(FCommandLine) = -1 then
    FCommandLine := Copy(FCommandLine, 1, MaxInt);
end;

procedure TWindowsProcess.SetOnErrorRawRead(const Value: TProcessRawReadEvent);
begin
  FErrorReader.OnRawRead := Value;
end;

procedure TWindowsProcess.SetOnErrorRead(const Value: TProcessReadEvent);
begin
  FErrorReader.OnRead := Value;
end;

procedure TWindowsProcess.SetOnRawRead(const Value: TProcessRawReadEvent);
begin
  FInputReader.OnRawRead := Value;
end;

procedure TWindowsProcess.SetOnRead(const Value: TProcessReadEvent);
begin
  FInputReader.OnRead := Value;
end;

procedure TWindowsProcess.SetWaitForTerminate(const Value: Boolean);
begin
  GotoReadyState;
  FWaitForTerminate := Value;
end;

procedure TWindowsProcess.StopWaiting;
begin
  TerminateWaitThread;
end;

procedure TWindowsProcess.Terminate;
begin
  if FState <> TProcessState.Ready then
    InternalTerminateProcess(FProcessInfo.dwProcessId);
end;

procedure TWindowsProcess.TerminateWaitThread;
begin
  if FState = TProcessState.Waiting then
  begin
    if Assigned(FWaitThread) then
    begin
      FWaitThread.OnTerminate := nil;
      TProcessWaitThread(FWaitThread).TerminateThread;
      FWaitThread := nil;
    end;
    TProcessOutputReader(FInputReader).Terminate;
    TProcessOutputReader(FErrorReader).Terminate;
    FRunningThreadCount := 0;
    GotoReadyState;
  end;
end;

procedure TWindowsProcess.WaitThreadTerminated(Sender: TObject);
begin
  FExitCode := TProcessWaitThread(Sender).FExitCode;
  FWaitThread := nil;
  PostMessage(Handle, CM_THREADTERMINATED, 0, 0);
end;

procedure TWindowsProcess.WndProc(var AMsg: TMessage);
begin
  try
    case AMsg.Msg of
      CM_READ:
        HandleReadEvent(TObject(AMsg.WParam));
      CM_THREADTERMINATED:
        HandleThreadTerminated;
    else
      AMsg.Result := DefWindowProc(Handle, AMsg.Msg, AMsg.WParam, AMsg.LParam);
    end;
  except
    if Assigned(ApplicationHandleException) then
      ApplicationHandleException(Self);
  end;
end;

function TWindowsProcess.Write(const S: AnsiString): Boolean;
begin
  Result := (FWaitThread is TProcessWriteThread) and TProcessWriteThread(FWaitThread).Write(S);
end;

function TWindowsProcess.WriteLn(const S: AnsiString): Boolean;
begin
  Result := Write(S + sLineBreak);
end;

{ TCustomRunProcess }

constructor TCustomRunProcess.Create;
begin
  inherited;
  FProcess := TWindowsProcess.Create;
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

procedure TCustomRunProcess.ProcessMessages;
var
  LMsg: TMsg;
begin
  if not IsConsole then
    ProcessMessage
  else
    while ProcessMessage(LMsg) do {loop};
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

function TCustomRunProcess.ProcessMessage(var AMsg: TMsg): Boolean;
var
  LIsUnicode: Boolean;
  LMsgExists: Boolean;
begin
  Result := False;
  if PeekMessage(AMsg, 0, 0, 0, PM_NOREMOVE) then
  begin
    LIsUnicode := (AMsg.hwnd = 0) or IsWindowUnicode(AMsg.hwnd);
    if LIsUnicode then
      LMsgExists := PeekMessageW(AMsg, 0, 0, 0, PM_REMOVE)
    else
      LMsgExists := PeekMessageA(AMsg, 0, 0, 0, PM_REMOVE);
    if LMsgExists then
    begin
      Result := True;
      if AMsg.Message <> WM_QUIT then
      begin
        TranslateMessage(AMsg);
        if LIsUnicode then
          DispatchMessageW(AMsg)
        else
          DispatchMessageA(AMsg);
      end;
    end;
  end
  else if IsConsole then
    CheckSynchronize;
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

function TCustomRunProcess.RunAndWait(const ATimeout: Cardinal): Integer;
var
  LStart: TDateTime;
begin
  if InternalRun then
  begin
    LStart := Now;
    while IsRunning and ((ATimeout = 0) or (MilliSecondsBetween(Now, LStart) < ATimeout)) do
      ProcessMessages;
    if IsRunning then
      Result := 1  // Still running - timed out
    else
      Result := 0; // Successfully ran without timing out
  end
  else
    Result := -1;  // Failed to start
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
  FProcess.CurrentDirectory := CurrentPath;
  LExecuting := 'Executing: ' + FProcess.CommandLine;
  if ShowExecuting then
    DoOutput(LExecuting);
  if ShowCommandInLog then
    TOSLog.d(LExecuting);
  FProcess.Run;
  FIsRunning := True;
  Result := True;
end;

class function TCustomRunProcess.MakeWorkingPath: string;
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

//procedure TRunProcess.WriteInput(const AInput: string; const AIncludeReturn: Boolean = True);
//begin
//  if AIncludeReturn then
//    FProcess.WriteLn(AInput)
//  else
//    FProcess.Write(AInput);
//end;

function TRunProcess.GetCommandLine: string;
begin
  Result := FProcess.CommandLine;
end;

end.
