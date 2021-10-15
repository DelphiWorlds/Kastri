unit DW.ADB;

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

// NOTE: This unit is dependent on DW.RunProcess.Win, which is dependent on JCL/JVCL:
//   https://github.com/project-jedi/jcl
//   https://github.com/project-jedi/jvcl

interface

uses
  System.SysUtils, System.Classes, Vcl.ExtCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  DW.RunProcess.Win, DW.ExternalDevice.Win;

const
  cADBLogLevels = 'IDEWVF';
  CRLF = #13#10;

type
  TStringArray = record
    Items: TArray<string>;
    procedure Add(const AValue: string);
    procedure Clear;
    function Count: Integer;
    function GetKey(const AValue: string): string; overload;
    function GetKey(const AIndex: Integer): string; overload;
    function GetValue(const AKey: string): string;
    function IndexOf(const AValue: string): Integer;
    function IndexOfName(const AKey: string): Integer;
    procedure SetValue(const AKey: string; const AValue: string);
    function Text(const ASeparator: string = CRLF): string;
    function Values: TArray<string>;
  end;

  TLogLevel = (Info, Debug, Error, Warning, Verbose, Fatal);
  TLogLevels = set of TLogLevel;
  TLogcatStatus = (Started, Stopped);

  TLogcatOutputEvent = procedure(Sender: TObject; const Output: string) of object;
  TLogcatStatusEvent = procedure(Sender: TObject; const Status: TLogcatStatus) of object;
  TApplyTCPIPEvent = procedure(Sender: TObject; const Success: Boolean; const Device: string) of object;

  TADBModule = class(TDataModule)
    LogFDMemTable: TFDMemTable;
    LogFDMemTableID: TFDAutoIncField;
    LogFDMemTableDeviceID: TStringField;
    LogFDMemTableLogDateTime: TDateTimeField;
    LogFDMemTableLevel: TStringField;
    LogFDMemTableProcessID: TIntegerField;
    LogFDMemTableThreadID: TIntegerField;
    LogFDMemTableApplication: TStringField;
    LogFDMemTableTag: TStringField;
    LogFDMemTableText: TStringField;
    PeriodicTimer: TTimer;
    DeviceCheckTimer: TTimer;
    procedure PeriodicTimerTimer(Sender: TObject);
    procedure DeviceCheckTimerTimer(Sender: TObject);
  private
    FADBDeviceList: TRunProcess;
    FADBEXEPath: string;
    FADBLogcat: TRunProcess;
    FADBProcessList: TRunProcess;
    FADBScreenCapture: TRunProcess;
    FADBCommand: TRunProcess;
    FADBGeneral: TRunProcess;
    FColumnCount: Integer;
    FDeviceList: TStringArray;
    FDevicesCheck: TDateTime;
    FDeviceWatcher: TExternalDeviceWatcher;
    FFilterUpdateCount: Integer;
    FIsProcessListHeading: Boolean;
    FLogLevelsFilter: TLogLevels;
    FNeedsLogcatRestart: Boolean;
    FNeedsProcessCheck: Boolean;
    FProcessList: TStringArray;
    FProcessNameColumnIndex: Integer;
    FProcessNameFilter: string;
    FProcessPIDColumnIndex: Integer;
    FRunningProcessList: TStringArray;
    FRunningProcessListText: string;
    FSelectedDevice: string;
    FTagFilter: string;
    FTCPIPDevice: string;
    FTextFilter: string;
    FViewUpdateCount: Integer;
    FOnApplyTCPIP: TApplyTCPIPEvent;
    FOnDeviceListUpdated: TNotifyEvent;
    FOnFiltersUpdated: TNotifyEvent;
    FOnLogcatOutput: TLogcatOutputEvent;
    FOnLogcatStatus: TLogcatStatusEvent;
    FOnProcessListUpdated: TNotifyEvent;
    FOnRowAdded: TNotifyEvent;
    procedure ADBDeviceListOutputHandler(Sender: TObject; const AOutput: string);
    procedure ADBDeviceListTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
    procedure ADBLogCatOutputHandler(Sender: TObject; const AOutput: string);
    procedure ADBLogCatTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
    procedure ADBProcessListOutputHandler(Sender: TObject; const AOutput: string);
    procedure ADBProcessListTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
    procedure ADBScreenCaptureOutputHandler(Sender: TObject; const AOutput: string);
    procedure ADBScreenCaptureTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
    procedure ADBTCPIPOutputHandler(Sender: TObject; const AOutput: string);
    procedure ADBTCPITerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
    procedure CheckDeviceList;
    procedure DeviceWatcherDeviceChangedHandler(Sender: TObject; const ADevice: TExternalDevice; const AInserted: Boolean; const ADrive: string);
    procedure DoDeviceListUpdated;
    procedure DoFiltersUpdated;
    procedure DoLogcatStatus(const AStatus: TLogcatStatus);
    procedure DoProcessListUpdated;
    procedure DoRowAdded;
    procedure DoStartLogcat;
    function GetIsLogcatRunning: Boolean;
    procedure HandleLogCatOutput(const AOutput: string);
    procedure LogcatStopped;
    procedure ProcessListTerminated;
    function SerialFromSelectedDevice: string;
    function SerialFromDevice(const ADevice: string): string;
    procedure UpdateDeviceList;
    procedure UpdateFilters;
    procedure UpdateProcessList;
    procedure UpdateSelectedDevice;
    procedure SetLogLevelsFilter(const Value: TLogLevels);
    procedure SetProcessNameFilter(const Value: string);
    procedure SetSelectedDevice(const Value: string);
    procedure SetTagFilter(const Value: string);
    procedure SetTextFilter(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    ///   Puts the device into TCPIP mode (e.g. for debugging over a network)
    /// </summary>
    procedure ApplyTCPIPMode(const ADevice: string);
    /// <summary>
    ///   Clears the log in the in-memory table
    /// </summary>
    procedure ClearLog;
    /// <summary>
    ///   Sets a flag to indicate that filters will be set
    /// </summary>
    procedure FilterUpdateBegin;
    /// <summary>
    ///   Sets a flag to indicate that filters have been set, and should now be applied
    /// </summary>
    procedure FilterUpdateEnd;
    procedure Start;
    /// <summary>
    ///   Starts logcat for the selected device (in SelectedDevice property)
    /// </summary>
    procedure StartLogcat;
    /// <summary>
    ///   Stops logcat
    /// </summary>
    procedure StopLogcat;
    /// <summary>
    ///   Sets a flag to indicate that a view is updating from the log
    /// </summary>
    procedure ViewUpdateBegin;
    /// <summary>
    ///   Sets a flag to indicate that a view has finished updating
    /// </summary>
    procedure ViewUpdateEnd;
    /// <summary>
    ///   Path to adb.exe (required for most functions)
    /// </summary>
    property ADBEXEPath: string read FADBEXEPath write FADBEXEPath;
    /// <summary>
    ///   List of devices connected to the machine. Devices are in the format: modelname (serialnumber)
    /// </summary>
    property DeviceList: TStringArray read FDeviceList;
    /// <summary>
    ///   Indicates whether or not logcat is currently running
    /// </summary>
    property IsLogcatRunning: Boolean read GetIsLogcatRunning;
    /// <summary>
    ///   Determines which log levels are filtered. This is applied to the in-memory dataset
    /// </summary>
    property LogLevelsFilter: TLogLevels read FLogLevelsFilter write SetLogLevelsFilter;
    /// <summary>
    ///   List of processes running on the selected device
    /// </summary>
    property ProcessList: TStringArray read FProcessList;
    /// <summary>
    ///   Filters log entries that have an application field where this value matches any part of it. This is applied to the in-memory dataset
    /// </summary>
    property ProcessNameFilter: string read FProcessNameFilter write SetProcessNameFilter;
    /// <summary>
    ///   Currently selected device. Setting this value will periodically update ProcessList
    /// </summary>
    property SelectedDevice: string read FSelectedDevice write SetSelectedDevice;
    /// <summary>
    ///   Filters log entries that have a tag field where this value matches any part of it. This is applied to the in-memory dataset
    /// </summary>
    property TagFilter: string read FTagFilter write SetTagFilter;
    /// <summary>
    ///   Filters log entries that have a text field where this value matches any part of it. This is applied to the in-memory dataset
    /// </summary>
    property TextFilter: string read FTextFilter write SetTextFilter;
    /// <summary>
    ///   Occurs when the ApplyTCPIPMode method completes. Indicates whether it was successful, and which device
    /// </summary>
    property OnApplyTCPIP: TApplyTCPIPEvent read FOnApplyTCPIP write FOnApplyTCPIP;
    /// <summary>
    ///   Occurs when the device list for the machine changes
    /// </summary>
    property OnDeviceListUpdated: TNotifyEvent read FOnDeviceListUpdated write FOnDeviceListUpdated;
    /// <summary>
    ///   Occurs when filters have been applied
    /// </summary>
    property OnFiltersUpdated: TNotifyEvent read FOnFiltersUpdated write FOnFiltersUpdated;
    /// <summary>
    ///   Occurs when logcat output is emitted.
    /// </summary>
    /// <remarks>
    ///   Setting this property means that logcat entries will *not* be added to the in-memory dataset
    /// </remarks>
    property OnLogcatOutput: TLogcatOutputEvent read FOnLogcatOutput write FOnLogcatOutput;
    /// <summary>
    ///   Occurs when logcat status changes
    /// </summary>
    property OnLogcatStatus: TLogcatStatusEvent read FOnLogcatStatus write FOnLogcatStatus;
    /// <summary>
    ///   Occurs when the process list for the device has changed
    /// </summary>
    property OnProcessListUpdated: TNotifyEvent read FOnProcessListUpdated write FOnProcessListUpdated;
    /// <summary>
    ///   Occurs when a row is added to the in-memory dataset
    /// </summary>
    property OnRowAdded: TNotifyEvent read FOnRowAdded write FOnRowAdded;
  end;

var
  ADBModule: TADBModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  System.IOUtils, System.DateUtils,
  DW.OSLog,
  DW.Tokenizers;

const
  cScreencapFileName = '/sdcard/screencap.png';
  cADBLogCatCommand = '%s -s %s logcat -v threadtime';
  cADBLogCatRecentLinesCommand = cADBLogCatCommand + ' -T %d';
  cADBLogCatClearCommand = cADBLogCatCommand + ' -c';
  cADBLogCatClearAllCommand = cADBLogCatCommand + ' -b all -c';
  cADBDevicesCommand = '%s devices -l';
  cADBProcessesCommand = '%s -s %s shell ps'; // -o pid,name';
  cADBForceIdleCommand = '%s -s %s shell dumpsys deviceidle force-idle';
  cADBUnforceIdleCommand = '%s -s %s shell dumpsys deviceidle unforce';
  cADBTCPIPCommand = '%s -s %s tcpip 5555';
  cADBDeviceKeyModel = 'model:';
  cADBDeviceKeyDevice = 'device:';
  cADBScreenCaptureCommand = '%s exec-out screencap -p %s'; // [-d display-id]
  cADBPullCommand = '%s pull %s %s';
  cDeviceListModel = '%s (%s)';
  cProcessListStartsWithPID = 'PID';

function IsMonthFirst: Boolean;
begin
  Result := FormatSettings.ShortDateFormat.LastIndexOf('M') < FormatSettings.ShortDateFormat.LastIndexOf('d');
end;

{ TStringArray }

procedure TStringArray.Add(const AValue: string);
begin
  Items := Items + [AValue];
end;

procedure TStringArray.Clear;
begin
  Items := [];
end;

function TStringArray.Count: Integer;
begin
  Result := Length(Items);
end;

function TStringArray.IndexOf(const AValue: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Equals(AValue) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TStringArray.IndexOfName(const AKey: string): Integer;
var
  I: Integer;
  LParts: TArray<string>;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    LParts := Items[I].Split(['='], 2);
    if (Length(LParts) > 1) and LParts[0].Equals(AKey) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TStringArray.SetValue(const AKey, AValue: string);
var
  I: Integer;
  LParts: TArray<string>;
  LIsFound: Boolean;
begin
  LIsFound := False;
  for I := 0 to Count - 1 do
  begin
    LParts := Items[I].Split(['='], 2);
    if (Length(LParts) > 1) and LParts[0].Equals(AKey) then
    begin
      LIsFound := True;
      Items[I] := Format('%s=%s', [AKey, AValue]);
      Break;
    end;
  end;
  if not LIsFound then
    Add(Format('%s=%s', [AKey, AValue]));
end;

function TStringArray.Text(const ASeparator: string): string;
begin
  Result := string.Join(ASeparator, Items);
end;

function TStringArray.Values: TArray<string>;
var
  I: Integer;
  LParts: TArray<string>;
begin
  Result := [];
  for I := 0 to Count - 1 do
  begin
    LParts := Items[I].Split(['='], 2);
    if Length(LParts) > 1 then
      Result := Result + [LParts[1]];
  end;
end;

function TStringArray.GetKey(const AValue: string): string;
var
  I: Integer;
  LParts: TArray<string>;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    LParts := Items[I].Split(['='], 2);
    if (Length(LParts) > 1) and LParts[1].Equals(AValue) then
    begin
      Result := LParts[0];
      Break;
    end;
  end;
end;

function TStringArray.GetKey(const AIndex: Integer): string;
var
  LParts: TArray<string>;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    LParts := Items[AIndex].Split(['='], 2);
    if Length(LParts) > 0 then
      Result := LParts[0];
  end;
end;

function TStringArray.GetValue(const AKey: string): string;
var
  I: Integer;
  LParts: TArray<string>;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    LParts := Items[I].Split(['='], 2);
    if (Length(LParts) > 1) and LParts[0].Equals(AKey) then
    begin
      Result := LParts[1];
      Break;
    end;
  end;
end;

{ TADBModule }

constructor TADBModule.Create(AOwner: TComponent);
begin
  inherited;
  FDeviceWatcher := TExternalDeviceWatcher.Create;
  FDeviceWatcher.OnDeviceChanged := DeviceWatcherDeviceChangedHandler;
  FADBGeneral := TRunProcess.Create;
  FADBCommand := TRunProcess.Create;
  FADBDeviceList := TRunProcess.Create;
  FADBDeviceList.OnProcessOutput := ADBDeviceListOutputHandler;
  FADBDeviceList.OnProcessTerminated := ADBDeviceListTerminatedHandler;
  FADBProcessList := TRunProcess.Create;
  FADBProcessList.OnProcessOutput := ADBProcessListOutputHandler;
  FADBProcessList.OnProcessTerminated := ADBProcessListTerminatedHandler;
  FADBLogCat := TRunProcess.Create;
  FADBLogCat.OnProcessOutput := ADBLogCatOutputHandler;
  FADBLogCat.OnProcessTerminated := ADBLogCatTerminatedHandler;
  FADBScreenCapture := TRunProcess.Create;
  FADBScreenCapture.OnProcessOutput := ADBScreenCaptureOutputHandler;
  FADBScreenCapture.OnProcessTerminated := ADBScreenCaptureTerminatedHandler;
  LogFDMemTable.Active := True;
end;

destructor TADBModule.Destroy;
begin
  FDeviceWatcher.Free;
  FADBGeneral.Free;
  FADBCommand.Free;
  FADBLogCat.Free;
  FADBDeviceList.Free;
  FADBProcessList.Free;
  FADBScreenCapture.Free;
  inherited;
end;

function TADBModule.SerialFromSelectedDevice: string;
begin
  Result := SerialFromDevice(FSelectedDevice);
end;

function TADBModule.SerialFromDevice(const ADevice: string): string;
var
  LPos: Integer;
begin
  LPos := Pos('(', ADevice);
  Result := ADevice.Substring(LPos, ADevice.Length - LPos - 1);
end;

function TADBModule.GetIsLogcatRunning: Boolean;
begin
  Result := FADBLogcat.IsRunning;
end;

procedure TADBModule.ViewUpdateBegin;
begin
  Inc(FViewUpdateCount);
end;

procedure TADBModule.ViewUpdateEnd;
begin
  if FViewUpdateCount > 0 then
    Dec(FViewUpdateCount);
end;

procedure TADBModule.DeviceCheckTimerTimer(Sender: TObject);
begin
  DeviceCheckTimer.Enabled := False;
  if not FADBDeviceList.IsRunning then
    CheckDeviceList;
end;

procedure TADBModule.DeviceWatcherDeviceChangedHandler(Sender: TObject; const ADevice: TExternalDevice; const AInserted: Boolean;
  const ADrive: string);
begin
  case ADevice of
    TExternalDevice.Android:
    begin
      if not FADBDeviceList.IsRunning then
        DeviceCheckTimer.Enabled := True;
    end;
  end;
end;

procedure TADBModule.ADBDeviceListOutputHandler(Sender: TObject; const AOutput: string);
var
  LModel, LSerial: string;
  LStart: Integer;
begin
  if AOutput.Trim.Contains(cADBDeviceKeyModel) then
  begin
    LSerial := AOutput.Substring(0, Pos(' ', AOutput) - 1);
    LStart := Pos(cADBDeviceKeyModel, AOutput) + cADBDeviceKeyModel.Length - 1;
    LModel := AOutput.Substring(LStart, Pos(cADBDeviceKeyDevice, AOutput) - LStart - 2);
    FDeviceList.Add(Format(cDeviceListModel, [LModel, LSerial]));
  end;
end;

procedure TADBModule.ADBDeviceListTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
begin
  DoDeviceListUpdated;
end;

procedure TADBModule.ADBLogCatOutputHandler(Sender: TObject; const AOutput: string);
begin
  if not AOutput.StartsWith('---') then
  begin
    while FViewUpdateCount > 0 do ;
    if Assigned(FOnLogcatOutput) then
      FOnLogcatOutput(Self, AOutput)
    else
      HandleLogCatOutput(AOutput);
  end;
end;

procedure TADBModule.ADBLogCatTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
begin
  LogcatStopped;
end;

// MC 33
// Header - 'USER      PID   PPID  VSIZE  RSS   WCHAN              PC  NAME'
// Line   - 'logd      389   1     21116  3804  sigsuspend 0000000000 S /system/bin/logd'

procedure TADBModule.ADBProcessListOutputHandler(Sender: TObject; const AOutput: string);
var
  LPID, LName: string;
  LTokenizer: TTokenizer;
  LIndex: Integer;
begin
  if AOutput.StartsWith('root') then
    Exit; // <=======
  if FIsProcessListHeading then
  begin
    FIsProcessListHeading := False;
    FColumnCount := 0;
    LTokenizer := TTokenizer.Create(AOutput);
    try
      LTokenizer.Tokenize(
        procedure(const AToken: string)
        begin
          Inc(FColumnCount);
          if AToken.Equals('PID') then
            FProcessPIDColumnIndex := FColumnCount
          else if AToken.Equals('NAME') then
            FProcessNameColumnIndex := FColumnCount;
        end
      );
    finally
      LTokenizer.Free;
    end;
    if AOutput.Contains('ADDR S') then
      FProcessNameColumnIndex := FProcessNameColumnIndex - 1
    else if AOutput.Contains('PC  NAME') then
      FProcessNameColumnIndex := FProcessNameColumnIndex + 1;
  end
  else
  begin
    LIndex := 0;
    LTokenizer := TTokenizer.Create(AOutput);
    try
      LTokenizer.Tokenize(
        procedure(const AToken: string)
        begin
          Inc(LIndex);
          if LIndex = FProcessPIDColumnIndex then
            LPID := AToken
          else if LIndex = FProcessNameColumnIndex then
          begin
            LName := AToken;
            if (Length(LName) = 1) and (LIndex <= FColumnCount)  then
              FProcessNameColumnIndex := LIndex + 1;
          end;
        end
      );
    finally
      LTokenizer.Free;
    end;
    if not LPID.IsEmpty and not LName.StartsWith('[') and not LName.Contains('@') and LName.Contains('.') then
    begin
      FRunningProcessList.SetValue(LPID, LName);
      // New entries need a check
      if FProcessList.IndexOfName(LPID) = -1 then
      begin
        FNeedsProcessCheck := True;
        FProcessList.SetValue(LPID, LName);
      end;
    end;
  end;
end;

procedure TADBModule.ADBProcessListTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
begin
  ProcessListTerminated;
end;

procedure TADBModule.ADBScreenCaptureOutputHandler(Sender: TObject; const AOutput: string);
begin
  //
end;

procedure TADBModule.ADBScreenCaptureTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
begin
  //
end;

procedure TADBModule.ADBTCPIPOutputHandler(Sender: TObject; const AOutput: string);
begin
  //
end;

procedure TADBModule.ADBTCPITerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
begin
  if Assigned(FOnApplyTCPIP) then
    FOnApplyTCPIP(Self, AExitCode = 0, FTCPIPDevice);
  FTCPIPDevice := '';
end;

procedure TADBModule.ApplyTCPIPMode(const ADevice: string);
begin
  FTCPIPDevice := ADevice;
  FADBGeneral.OnProcessOutput := ADBTCPIPOutputHandler;
  FADBGeneral.OnProcessTerminated := ADBTCPITerminatedHandler;
  FADBGeneral.CommandLine := Format(cADBTCPIPCommand, [FADBEXEPath, SerialFromDevice(FTCPIPDevice)]);
  FADBGeneral.Run;
end;

procedure TADBModule.DoDeviceListUpdated;
begin
  if Assigned(FOnDeviceListUpdated) then
    FOnDeviceListUpdated(Self);
  if FADBLogcat.IsRunning and (FDeviceList.IndexOf(FSelectedDevice) = -1) then
    StopLogcat;
end;

procedure TADBModule.DoFiltersUpdated;
begin
  if Assigned(FOnFiltersUpdated) then
    FOnFiltersUpdated(Self);
end;

procedure TADBModule.DoLogcatStatus(const AStatus: TLogcatStatus);
begin
  if Assigned(FOnLogcatStatus) then
    FOnLogcatStatus(Self, AStatus);
end;

procedure TADBModule.DoProcessListUpdated;
begin
  if Assigned(FOnProcessListUpdated) then
    FOnProcessListUpdated(Self);
end;

procedure TADBModule.DoRowAdded;
begin
  if Assigned(FOnRowAdded) then
    FOnRowAdded(Self);
end;

procedure TADBModule.DoStartLogcat;
begin
  LogFDMemTable.EmptyDataSet;
  if TFile.Exists(FADBEXEPath) then
  begin
    FADBLogCat.CommandLine := Format(cADBLogCatRecentLinesCommand, [FADBEXEPath, SerialFromSelectedDevice, 500]); // Last 500 lines
    FADBLogCat.Run;
    DoLogcatStatus(TLogcatStatus.Started);
  end;
end;

procedure TADBModule.FilterUpdateBegin;
begin
  Inc(FFilterUpdateCount);
end;

procedure TADBModule.FilterUpdateEnd;
begin
  if FFilterUpdateCount > 0 then
  begin
    Dec(FFilterUpdateCount);
    if FFilterUpdateCount = 0 then
      UpdateFilters;
  end;
end;

procedure TADBModule.Start;
begin
  CheckDeviceList;
end;

procedure TADBModule.StartLogcat;
begin
  if not FADBLogCat.IsRunning then
    DoStartLogcat;
end;

procedure TADBModule.StopLogcat;
begin
  if FADBLogCat.IsRunning then
    FADBLogCat.Terminate
  else
    LogcatStopped;
end;

procedure TADBModule.CheckDeviceList;
begin
  FDeviceList.Clear;
  if TFile.Exists(FADBEXEPath) then
  begin
    FDevicesCheck := Now;
    FADBDeviceList.CommandLine := Format(cADBDevicesCommand, [FADBEXEPath]);
    FADBDeviceList.Run;
  end
  else
    UpdateDeviceList;
end;

procedure TADBModule.ClearLog;
begin
  if LogFDMemTable.Active then
    LogFDMemTable.EmptyDataSet;
end;

procedure TADBModule.UpdateDeviceList;
begin
  if FDeviceList.IndexOf(FSelectedDevice) = -1 then
    FSelectedDevice := '';
  UpdateSelectedDevice;
end;

procedure TADBModule.SetLogLevelsFilter(const Value: TLogLevels);
begin
  if FLogLevelsFilter <> Value then
  begin
    FLogLevelsFilter := Value;
    UpdateFilters;
  end;
end;

procedure TADBModule.SetProcessNameFilter(const Value: string);
begin
  if FProcessNameFilter <> Value then
  begin
    FProcessNameFilter := Value;
    UpdateFilters;
  end;
end;

procedure TADBModule.SetTagFilter(const Value: string);
begin
  if FTagFilter <> Value then
  begin
    FTagFilter := Value;
    UpdateFilters;
  end;
end;

procedure TADBModule.SetTextFilter(const Value: string);
begin
  if FTextFilter <> Value then
  begin
    FTextFilter := Value;
    UpdateFilters;
  end;
end;

procedure TADBModule.SetSelectedDevice(const Value: string);
begin
  if (FSelectedDevice <> Value) and (FDeviceList.IndexOf(Value) > -1) then
  begin
    FSelectedDevice := Value;
    UpdateSelectedDevice;
  end;
end;

procedure TADBModule.UpdateSelectedDevice;
begin
  FNeedsLogcatRestart := FADBLogcat.IsRunning and not FSelectedDevice.IsEmpty;
  if FADBLogcat.IsRunning then
    StopLogcat;
end;

procedure TADBModule.LogcatStopped;
begin
  if FNeedsLogcatRestart then
    StartLogcat
  else
    DoLogcatStatus(TLogcatStatus.Stopped);
end;

procedure TADBModule.ProcessListTerminated;
begin
  if not FRunningProcessList.Text.Equals(FRunningProcessListText) then
  begin
    FNeedsProcessCheck := True;
    FRunningProcessListText := FRunningProcessList.Text;
  end;
  if FNeedsProcessCheck then
    DoProcessListUpdated;
  FNeedsProcessCheck := False;
end;

procedure TADBModule.UpdateProcessList;
var
  LSerial: string;
begin
  if TFile.Exists(FADBEXEPath) then
  begin
    FNeedsProcessCheck := False;
    FRunningProcessList.Clear;
    LSerial := SerialFromSelectedDevice;
    if not LSerial.IsEmpty then
    begin
      FIsProcessListHeading := True;
      FADBProcessList.CommandLine := Format(cADBProcessesCommand, [FADBEXEPath, LSerial]);
      FADBProcessList.Run;
    end
    else
      ProcessListTerminated;
  end
end;

procedure TADBModule.PeriodicTimerTimer(Sender: TObject);
begin
  if not FADBProcessList.IsRunning then
    UpdateProcessList;
end;

procedure TADBModule.HandleLogCatOutput(const AOutput: string);
var
  LTokenizer: TTokenizer;
  LTokenIndex: Integer;
  LDate, LDateFirst, LDateSecond: string;
  LDateTime, LYear: TDateTime;
  LDateParts: TArray<string>;
  LID, LProcessID, LThreadID: Integer;
  LText, LLogLevel, LTag: string;
  LTokenized: Boolean;
begin
  LTokenIndex := 0;
  LText := '';
  LTokenizer := TTokenizer.Create(AOutput);
  try
    LTokenized := LTokenizer.Tokenize(
      function(const AToken: string): Boolean
      var
        LDateFirstInt, LDateSecondInt: Integer;
      begin
        Result := True;
        case LTokenIndex of
          0:
          begin
            LYear := Now;
            LDateParts := AToken.Split(['-']);
            if Length(LDateParts) <> 2 then
              Exit(False); // <=======
            if IsMonthFirst then
            begin
              LDateFirst := LDateParts[0];
              LDateSecond := LDateParts[1];
              if TryStrToInt(LDateFirst, LDateFirstInt) then
              begin
                if LDateFirstInt > MonthOf(Now) then
                  LYear := IncYear(Now, -1);
              end
              else
                Exit(False); // <======
            end
            else
            begin
              LDateSecond := LDateParts[0];
              LDateFirst := LDateParts[1];
              if TryStrToInt(LDateSecond, LDateSecondInt) then
              begin
                if LDateSecondInt > MonthOf(Now) then
                  LYear := IncYear(Now, -1);
              end
              else
                Exit(False); // <======
            end;
            LDate := LDateFirst + FormatSettings.DateSeparator + LDateSecond + FormatSettings.DateSeparator + YearOf(LYear).ToString;
          end;
          1:
          begin
            if not TryStrToDateTime(LDate + ' ' + AToken, LDateTime) then
              Exit(False); // <======
          end;
          2:
          begin
            if not TryStrToInt(AToken, LProcessID) then
              Exit(False); // <======
          end;
          3:
          begin
            if not TryStrToInt(AToken, LThreadID) then
              Exit(False); // <======
          end;
          4:
            LLogLevel := AToken;
          5:
          begin
            if AToken.EndsWith(':') then
              LTag := AToken.Substring(0, AToken.Length - 1)
            else
              LTag := AToken;
          end;
        end;
        if (LTokenIndex >= 6) and not AToken.Equals(':') then
          LText := LText + ' ' + AToken;
        Inc(LTokenIndex);
      end
    );
  finally
    LTokenizer.Free;
  end;
  if LTokenized then
  begin
    LogFDMemTable.Append;
    LogFDMemTableLogDateTime.Value := LDateTime;
    LogFDMemTableProcessID.Value := LProcessID;
    LogFDMemTableApplication.AsString := FProcessList.GetValue(LProcessID.ToString);
    LogFDMemTableThreadID.Value := LThreadID;
    LogFDMemTableLevel.AsString := LLogLevel;
    LogFDMemTableTag.AsString := LTag;
    if not LText.IsEmpty then
      LogFDMemTableText.AsString := LText.Trim;
    if HourSpan(Now, LogFDMemTableLogDateTime.Value) < 24 then
    begin
      LID := LogFDMemTableID.Value;
      LogFDMemTable.Post;
      if LogFDMemTable.Locate('ID', LID) then
        DoRowAdded;
    end
    else
      LogFDMemTable.Cancel;
  end;
end;

procedure TADBModule.UpdateFilters;
const
  cAnd: array[Boolean] of string = ('', 'AND ');
var
  LFilters: TStrings;
  LFilter, LTextFilter, LTagFilter: string;
  LProcessIDs: TArray<string>;
  I: Integer;
  LHasFilter: Boolean;
begin
  if FFilterUpdateCount = 0 then
  begin
    LHasFilter := True;
    LTextFilter := StringReplace(FTextFilter, '''', '''''', [rfReplaceAll]).Trim;
    LTagFilter := StringReplace(FTagFilter, '''', '''''', [rfReplaceAll]).Trim;
    LFilter := LogFDMemTable.Filter;
    LFilters := TStringList.Create;
    try
      LFilters.NameValueSeparator := #0;
      if not FProcessNameFilter.IsEmpty  then
        LProcessIDs := [FProcessList.GetKey(FProcessNameFilter)]
      else
        LHasFilter := False;
      if LHasFilter then
      begin
        if Length(LProcessIDs) > 0 then
        begin
          for I := 0 to Length(LProcessIDs) - 1 do
          begin
            if I > 0 then
              LFilters.Add('OR');
            LFilters.Add('ProcessID = ' + LProcessIDs[I]);
          end;
        end
        else
          LFilters.Add('ProcessID = -1');
        if LFilters.Count > 0 then
          LFilters.Text := '(' + LFilters.Text + ')';
      end;
      if not (TLogLevel.Info in FLogLevelsFilter) then
        LFilters.Add(cAnd[LFilters.Count > 0] + 'LEVEL <> ''I''');
      if not (TLogLevel.Debug in FLogLevelsFilter) then
        LFilters.Add(cAnd[LFilters.Count > 0] + 'LEVEL <> ''D''');
      if not (TLogLevel.Warning in FLogLevelsFilter) then
        LFilters.Add(cAnd[LFilters.Count > 0] + 'LEVEL <> ''W''');
      if not (TLogLevel.Error in FLogLevelsFilter) then
        LFilters.Add(cAnd[LFilters.Count > 0] + 'LEVEL <> ''E''');
      if not (TLogLevel.Fatal in FLogLevelsFilter) then
        LFilters.Add(cAnd[LFilters.Count > 0] + 'LEVEL <> ''F''');
      if not (TLogLevel.Verbose in FLogLevelsFilter) then
        LFilters.Add(cAnd[LFilters.Count > 0] + 'LEVEL <> ''V''');
      LFilters.Add(cAnd[LFilters.Count > 0] + 'TEXT LIKE ''%' + LTextFilter + '%''');
      if not LTagFilter.IsEmpty then
        LFilters.Add(cAnd[LFilters.Count > 0] + 'TAG LIKE ''%' + LTagFilter + '%''');
      LogFDMemTable.Filter := StringReplace(LFilters.Text, #13#10, ' ', [rfReplaceAll]);
    finally
      LFilters.Free;
    end;
    LogFDMemTable.Filtered := not LogFDMemTable.Filter.IsEmpty;
    if not LFilter.Equals(LogFDMemTable.Filter) then
      DoFiltersUpdated;
  end;
end;

end.
