unit DW.RunProcess.Mac;

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


// Portions of code by Sebastian Zierer:
//   https://www.codenewsfast.com/cnf/article/1692649891/permalink.art-ng2027q23047

interface

uses
  // Mac
  Macapi.Foundation, Macapi.ObjectiveC, Macapi.CocoaTypes,
  // DW
  DW.RunProcess;

type
  NSTask = interface;
  NSPipe = interface;
  NSFileHandle = interface;

  NSQualityOfService = NSInteger;
  NSTaskTerminationReason = NSInteger;

  TNSTaskBlockMethod1 = procedure(param1: NSTask) of object;
  TNSFileHandleBlockMethod1 = procedure(handle: NSFileHandle) of object;

  NSTaskClass = interface(NSObjectClass)
    ['{DC12890F-2CB6-4E13-9BBD-D6EF41E62359}']
    [MethodName('launchedTaskWithExecutableURL:arguments:error:terminationHandler:')]
    {class} function launchedTaskWithExecutableURL(url: NSURL; arguments: NSArray; error: NSError; terminationHandler: TNSTaskBlockMethod1): NSTask; cdecl;
    [MethodName('launchedTaskWithLaunchPath:arguments:')]
    {class} function launchedTaskWithLaunchPath(path: NSString; arguments: NSArray): NSTask; cdecl;
  end;

  NSTask = interface(NSObject)
    ['{61EFE4DE-2BAA-4829-907D-F7FC6E181D54}']
    function arguments: NSArray; cdecl;
    function currentDirectoryPath: NSString; cdecl;
    function currentDirectoryURL: NSURL; cdecl;
    function environment: NSDictionary; cdecl;
    function executableURL: NSURL; cdecl;
    procedure interrupt; cdecl;
    function isRunning: Boolean; cdecl;
    procedure launch; cdecl;
    function launchAndReturnError(error: PPointer): Boolean; cdecl;
    function launchPath: NSString; cdecl;
    function processIdentifier: Integer; cdecl;
    function qualityOfService: NSQualityOfService; cdecl;
    function resume: Boolean; cdecl;
    procedure setArguments(arguments: NSArray); cdecl;
    procedure setCurrentDirectoryPath(currentDirectoryPath: NSString); cdecl;
    procedure setCurrentDirectoryURL(currentDirectoryURL: NSURL); cdecl;
    procedure setEnvironment(environment: NSDictionary); cdecl;
    procedure setExecutableURL(executableURL: NSURL); cdecl;
    procedure setLaunchPath(launchPath: NSString); cdecl;
    procedure setQualityOfService(qualityOfService: NSQualityOfService); cdecl;
    procedure setStandardError(standardError: Pointer); cdecl;
    procedure setStandardInput(standardInput: Pointer); cdecl;
    procedure setStandardOutput(standardOutput: Pointer); cdecl;
    procedure setTerminationHandler(terminationHandler: TNSTaskBlockMethod1); cdecl;
    function standardError: Pointer; cdecl;
    function standardInput: Pointer; cdecl;
    function standardOutput: Pointer; cdecl;
    function suspend: Boolean; cdecl;
    procedure terminate; cdecl;
    function terminationHandler: TNSTaskBlockMethod1; cdecl;
    function terminationReason: NSTaskTerminationReason; cdecl;
    function terminationStatus: Integer; cdecl;
    procedure waitUntilExit; cdecl;
  end;
  TNSTask = class(TOCGenericImport<NSTaskClass, NSTask>) end;

  NSPipeClass = interface(NSObjectClass)
    ['{5C27AE4E-4A6F-4288-868F-8415CF1E4BAB}']
    {class} function pipe: Pointer; cdecl;
  end;

  NSPipe = interface(NSObject)
    ['{29412D50-5D75-4349-848D-0DECFE9B5C39}']
    function fileHandleForReading: NSFileHandle; cdecl;
    function fileHandleForWriting: NSFileHandle; cdecl;
    function init: Pointer; cdecl;
  end;
  TNSPipe = class(TOCGenericImport<NSPipeClass, NSPipe>)  end;

  NSFileHandleClass = interface(NSObjectClass)
    ['{E826E479-4977-42ED-AE69-E984F06040A2}']
    {class} function fileHandleForReadingAtPath(path: NSString): Pointer; cdecl;
    [MethodName('fileHandleForReadingFromURL:error:')]
    {class} function fileHandleForReadingFromURL(url: NSURL; error: PPointer): Pointer; cdecl;
    {class} function fileHandleForUpdatingAtPath(path: NSString): Pointer; cdecl;
    [MethodName('fileHandleForUpdatingURL:error:')]
    {class} function fileHandleForUpdatingURL(url: NSURL; error: PPointer): Pointer; cdecl;
    {class} function fileHandleForWritingAtPath(path: NSString): Pointer; cdecl;
    [MethodName('fileHandleForWritingToURL:error:')]
    {class} function fileHandleForWritingToURL(url: NSURL; error: PPointer): Pointer; cdecl;
    {class} function fileHandleWithNullDevice: NSFileHandle; cdecl;
    {class} function fileHandleWithStandardError: NSFileHandle; cdecl;
    {class} function fileHandleWithStandardInput: NSFileHandle; cdecl;
    {class} function fileHandleWithStandardOutput: NSFileHandle; cdecl;
  end;

  NSFileHandle = interface(NSObject)
    ['{0E840E13-4910-4B8B-8F9A-4FB6B5C624C0}']
    procedure acceptConnectionInBackgroundAndNotify; cdecl;
    procedure acceptConnectionInBackgroundAndNotifyForModes(modes: NSArray); cdecl;
    function availableData: NSData; cdecl;
    procedure closeFile; cdecl;
    function fileDescriptor: Integer; cdecl;
    function initWithCoder(coder: NSCoder): Pointer; cdecl;
    [MethodName('initWithFileDescriptor:closeOnDealloc:')]
    function initWithFileDescriptor(fd: Integer; closeopt: Boolean): Pointer; overload; cdecl;
    function initWithFileDescriptor(fd: Integer): Pointer; overload; cdecl;
    function offsetInFile: UInt64; cdecl;
    function readabilityHandler: TNSFileHandleBlockMethod1; cdecl;
    function readDataOfLength(length: NSUInteger): NSData; cdecl;
    function readDataToEndOfFile: NSData; cdecl;
    procedure readInBackgroundAndNotify; cdecl;
    procedure readInBackgroundAndNotifyForModes(modes: NSArray); cdecl;
    procedure readToEndOfFileInBackgroundAndNotify; cdecl;
    procedure readToEndOfFileInBackgroundAndNotifyForModes(modes: NSArray); cdecl;
    function seekToEndOfFile: UInt64; cdecl;
    procedure seekToFileOffset(offset: UInt64); cdecl;
    procedure setReadabilityHandler(readabilityHandler: TNSFileHandleBlockMethod1); cdecl;
    procedure setWriteabilityHandler(writeabilityHandler: TNSFileHandleBlockMethod1); cdecl;
    procedure synchronizeFile; cdecl;
    procedure truncateFileAtOffset(offset: UInt64); cdecl;
    procedure waitForDataInBackgroundAndNotify; cdecl;
    procedure waitForDataInBackgroundAndNotifyForModes(modes: NSArray); cdecl;
    function writeabilityHandler: TNSFileHandleBlockMethod1; cdecl;
    procedure writeData(data: NSData); cdecl;
  end;
  TNSFileHandle = class(TOCGenericImport<NSFileHandleClass, NSFileHandle>) end;

  /// <summary>
  ///   Concrete class for running macOS processes
  /// </summary>
  TRunProcess = class(TBaseRunProcess)
  private
    FCmdLine: string;
    FIsDestroyed: Boolean;
    FIsTerminated: Boolean;
    FParams: string;
    FPipeError: NSPipe;
    FPipeOutput: NSPipe;
    FTask: NSTask;
    procedure CreateTask;
    procedure DestroyTask;
    procedure ProcessPipeOutput(AFile: NSFileHandle);
    procedure PipeErrorFileHandleForReadingReadabilityHandler(AFile: NSFileHandle);
    procedure PipeOutputFileHandleForReadingReadabilityHandler(AFile: NSFileHandle);
    procedure TaskTerminationHandler(task: NSTask);
    procedure SetCmdLine(const Value: string);
  protected
    function GetIsRunning: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Run: Boolean; override;
    function RunAndWait(const ATimeout: Cardinal = 0): Integer; override;
    procedure Terminate; override;
    property CommandLine: string read FCmdLine write SetCmdLine;
    property IsTerminated: Boolean read FIsTerminated;
    property Params: string read FParams write FParams;
    property OnProcessOutput;
    property OnProcessTerminated;
  end;

  TRunProcessSync = record
  private
    class function InternalRun(const ACmd, AParams: string; var AOutput: string; const ACurrentDir, AOutputFileName: string): Integer; static;
  public
    class function Run(const ACmd, AParams: string; var AOutput: string; const ACurrentDir: string = ''): Integer; static;
    class function RunRedir(const ACmd, AParams: string; const AOutputFileName: string; const ACurrentDir: string = ''): Integer; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // Mac
  Macapi.Helpers,
  // DW
  DW.OSLog, DW.Macapi.Helpers;

{ TRunProcess }

constructor TRunProcess.Create;
begin
  inherited;
  FIsDestroyed := True;
end;

destructor TRunProcess.Destroy;
begin
  //
  inherited;
end;

procedure TRunProcess.CreateTask;
begin
  if not FIsDestroyed then
    DestroyTask;
  FTask := TNSTask.Create;
  FTask.setTerminationHandler(TaskTerminationHandler);
  FPipeError := TNSPipe.Create;
//  FPipeInput := TNSPipe.Create;
  FPipeOutput := TNSPipe.Create;
  FTask.setStandardError(NSObjectToID(FPipeError));
//  FTask.setStandardInput(NSObjectToID(FPipeInput));
  FTask.setStandardOutput(NSObjectToID(FPipeOutput));
  FPipeOutput.fileHandleForReading.setReadabilityHandler(PipeOutputFileHandleForReadingReadabilityHandler);
  FPipeError.fileHandleForReading.setReadabilityHandler(PipeErrorFileHandleForReadingReadabilityHandler);
  FIsDestroyed := False;
end;

procedure TRunProcess.DestroyTask;
begin
  if FPipeOutput <> nil then
  begin
    FPipeOutput.fileHandleForReading.closeFile;
    FPipeOutput.fileHandleForReading.setReadabilityHandler(nil);
  end;
  if FPipeError <> nil then
  begin
    FPipeError.fileHandleForReading.closeFile;
    FPipeError.fileHandleForReading.setReadabilityHandler(nil);
  end;
  if FPipeError <> nil then
    FPipeError.release;
  FPipeError := nil;
//  FPipeInput.release;
//  FPipeInput := nil;
  if FPipeOutput <> nil then
    FPipeOutput.release;
  FPipeOutput := nil;
  if FTask <> nil then
    FTask.release;
  FTask := nil;
  FIsDestroyed := True;
end;

procedure TRunProcess.PipeErrorFileHandleForReadingReadabilityHandler(AFile: NSFileHandle);
begin
  ProcessPipeOutput(AFile);
end;

procedure TRunProcess.PipeOutputFileHandleForReadingReadabilityHandler(AFile: NSFileHandle);
begin
  ProcessPipeOutput(AFile);
end;

procedure TRunProcess.ProcessPipeOutput(AFile: NSFileHandle);
var
  LOutput: string;
  LString: NSString;
begin
  if not FIsTerminated and OutputFileName.IsEmpty then
  begin
    LString := TNSString.Wrap(TNSString.OCClass.alloc);
    LOutput := NSStrToStr(TNSString.Wrap(LString.initWithData(AFile.availableData, NSUTF8StringEncoding)));
    DoOutput(LOutput);
  end;
end;

function TRunProcess.GetIsRunning: Boolean;
begin
  Result := not FIsTerminated and (FTask <> nil) and FTask.isRunning;
end;

function TRunProcess.Run: Boolean;
var
  LError: Pointer;
begin
  Result := False;
  FIsTerminated := False;
  if TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager).isExecutableFileAtPath(StrToNSStr(FCmdLine)) then
  begin
    CreateTask;
    if not CurrentPath.IsEmpty then
      FTask.setCurrentDirectoryPath(StrToNSStr(CurrentPath));
    FTask.setExecutableURL(TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(FCmdLine))));
    if not FParams.IsEmpty then
      FTask.setArguments(StringArrayToNSArray(FParams.Split([' '], '"'), True));
    Result := FTask.launchAndReturnError(@LError);
    if not Result then
      TOSLog.d('FTask.launchAndReturnError returned FALSE');
  end
  else
    TOSLog.d('isExecutableFileAtPath is FALSE');
end;

function TRunProcess.RunAndWait(const ATimeout: Cardinal = 0): Integer;
begin
  Result := -1; // Has problems on macOS - use TRunProcessSynch
end;

procedure TRunProcess.SetCmdLine(const Value: string);
var
  LParts: TArray<string>;
begin
  LParts := Value.Split([' '], '"');
  if Length(LParts) > 1 then
  begin
    FCmdLine := LParts[0];
    FParams := Value.Substring(Length(LParts[0])).Trim;
  end
  else
    FCmdLine := Value;
end;

procedure TRunProcess.TaskTerminationHandler(task: NSTask);
var
  LStatus: Integer;
begin
  if not FIsDestroyed then
  begin
    if not FIsTerminated then
      LStatus := task.terminationStatus
    else
      LStatus := -1;
    if not OutputFileName.IsEmpty then
      FPipeOutput.fileHandleForReading.readDataToEndOfFile.writeToFile(StrToNSStr(OutputFileName), True);
    DestroyTask;
    DoTerminated(LStatus);
  end;
end;

procedure TRunProcess.Terminate;
begin
  FIsTerminated := True;
  if FTask <> nil then
    FTask.terminate
  else
    TOSLog.d('TRunProcess.Terminate (FTask = nil)');
end;

{ TRunProcessSync }

class function TRunProcessSync.InternalRun(const ACmd, AParams: string; var AOutput: string; const ACurrentDir, AOutputFileName: string): Integer;
var
  LTask: NSTask;
  {LInPipe, } LOutPipe, LErrorPipe: NSPipe;
  LError: Pointer;
  LOutput: NSString;
begin
  Result := -1;
  // LInPipe := TNSPipe.Create;
  LOutPipe := TNSPipe.Create;
  try
    LErrorPipe := TNSPipe.Create;
    try
      LTask := TNSTask.Create;
      try
        LTask.setExecutableURL(TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(ACmd))));
        if not AParams.IsEmpty then
          LTask.setArguments(StringArrayToNSArray(AParams.Split([' '], '"'), True));
        // LTask.setStandardInput(NSObjectToID(LInPipe));
        LTask.setStandardOutput(NSObjectToID(LOutPipe));
        LTask.setStandardError(NSObjectToID(LErrorPipe));
        if LTask.launchAndReturnError(@LError) then
        begin
          if LTask.isRunning then
            LTask.waitUntilExit;
          Result := LTask.terminationStatus;
        end;
      finally
        LTask.release;
      end;
      if AOutputFileName.IsEmpty then
      begin
        LOutput := TNSString.Wrap(TNSString.OCClass.alloc);
        AOutput := NSStrToStr(TNSString.Wrap(LOutput.initWithData(LOutPipe.fileHandleForReading.readDataToEndOfFile, NSUTF8StringEncoding)));
        if (Result <> 0) and AOutput.IsEmpty then
          AOutput := NSStrToStr(TNSString.Wrap(LOutput.initWithData(LErrorPipe.fileHandleForReading.readDataToEndOfFile, NSUTF8StringEncoding)));
      end
      else
        LOutPipe.fileHandleForReading.readDataToEndOfFile.writeToFile(StrToNSStr(AOutputFileName), True);
    finally
      LErrorPipe.release;
    end;
  finally
    LOutPipe.release;
  end;
end;

class function TRunProcessSync.Run(const ACmd, AParams: string; var AOutput: string; const ACurrentDir: string = ''): Integer;
begin
  Result := InternalRun(ACmd, AParams, AOutput, ACurrentDir, '');
end;

class function TRunProcessSync.RunRedir(const ACmd, AParams: string; const AOutputFileName, ACurrentDir: string): Integer;
var
  LOutput: string;
begin
  Result := InternalRun(ACmd, AParams, LOutput, ACurrentDir, AOutputFileName);
end;

end.
