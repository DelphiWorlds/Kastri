unit DW.SymbolScanner.Android;

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
  System.Classes, System.Messaging,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Embarcadero, Androidapi.JNIBridge,
  // DW
  DW.Androidapi.JNI.EMDK, DW.SymbolScanner;

// Code based on tutorials found here: http://techdocs.zebra.com/emdk-for-android/4-0/tutorial/

type
  JDWEMDKListenerDelegate = interface;
  JDWEMDKScanner = interface;

  JDWEMDKListenerDelegateClass = interface(IJavaClass)
    ['{087E55A2-682F-4900-89AE-FC40BE620019}']
  end;

  [JavaSignature('com/delphiworlds/kastri/DWEMDKListenerDelegate')]
  JDWEMDKListenerDelegate = interface(IJavaInstance)
    ['{230CCEF9-0352-4A80-9411-30322881E141}']
    procedure managerClosed; cdecl;
    procedure managerOpened; cdecl;
    procedure scannerData(scanDataCollection: JScanDataCollection); cdecl;
    procedure scannerStatus(statusData: JStatusData); cdecl;
  end;
  TJDWEMDKListenerDelegate = class(TJavaGenericImport<JDWEMDKListenerDelegateClass, JDWEMDKListenerDelegate>) end;

  JDWEMDKScannerClass = interface(JObjectClass)
    ['{2216BAD0-0E8F-46B9-A7F9-BE8DC3DDB170}']
    {class} function init(context: JContext; delegate: JDWEMDKListenerDelegate): JDWEMDKScanner; cdecl;
    {class} function isEMDKInstalled(context: JContext): Boolean; cdecl;
  end;

  [JavaSignature('com/delphiworlds/kastri/DWEMDKScanner')]
  JDWEMDKScanner = interface(JObject)
    ['{210351DB-4839-414D-A03C-1D6F8750F6F0}']
    function getIsInitialized: Boolean; cdecl;
    function getScanner: Jbarcode_Scanner; cdecl;
    function enableScanner(enable: Boolean): Boolean; cdecl;
  end;
  TJDWEMDKScanner = class(TJavaGenericImport<JDWEMDKScannerClass, JDWEMDKScanner>) end;

  TPlatformSymbolScanner = class;

  TDWEMDKListenerDelegate = class(TJavaLocal, JDWEMDKListenerDelegate)
  private
    FScanner: TPlatformSymbolScanner;
  public
    { JDWEMDKListenerDelegate }
    procedure managerClosed; cdecl;
    procedure managerOpened; cdecl;
    procedure scannerData(scanDataCollection: JScanDataCollection); cdecl;
    procedure scannerStatus(statusData: JStatusData); cdecl;
  public
    constructor Create(const AScanner: TPlatformSymbolScanner);
  end;

  TPlatformSymbolScanner = class(TCustomPlatformSymbolScanner)
  private
    FDelegate: JDWEMDKListenerDelegate;
    FEMDKScanner: JDWEMDKScanner;
    FIsIdle: Boolean;
    FIsOpen: Boolean;
    procedure CreateEMDKScanner;
    function DoActivate(const AActivate: Boolean): Boolean;
    function IsEnabled: Boolean;
    function ResetHardReading: Boolean;
  protected
    procedure Activate(const AActivate: Boolean); override;
    procedure CancelScan; override;
    function CanScan: Boolean; override;
    function EnableScanner(const AEnable: Boolean): Boolean; override;
    procedure ManagerClosed;
    procedure ManagerOpened;
    function Scan: Boolean; override;
    procedure ScannerData(scanDataCollection: JScanDataCollection);
    procedure ScannerStatus(statusData: JStatusData);
    procedure ScannerTriggerChanged; override;
  public
    class function IsEMDKInstalled: Boolean;
  public
    constructor Create(const ASymbolScanner: TSymbolScanner); override;
    destructor Destroy; override;
  end;

implementation

uses
  // Android
  Androidapi.Helpers,
  // DW
  DW.OSLog;

{ TDWEMDKListenerDelegate }

constructor TDWEMDKListenerDelegate.Create(const AScanner: TPlatformSymbolScanner);
begin
  inherited Create;
  FScanner := AScanner;
end;

procedure TDWEMDKListenerDelegate.managerClosed;
begin
  FScanner.ManagerClosed;
end;

procedure TDWEMDKListenerDelegate.managerOpened;
begin
  FScanner.ManagerOpened;
end;

procedure TDWEMDKListenerDelegate.scannerData(scanDataCollection: JScanDataCollection);
begin
  FScanner.ScannerData(scanDataCollection);
end;

procedure TDWEMDKListenerDelegate.scannerStatus(statusData: JStatusData);
begin
  FScanner.ScannerStatus(statusData);
end;

{ TPlatformSymbolScanner }

constructor TPlatformSymbolScanner.Create(const ASymbolScanner: TSymbolScanner);
begin
  inherited;
  //
end;

destructor TPlatformSymbolScanner.Destroy;
begin
  //
  inherited;
end;

procedure TPlatformSymbolScanner.CreateEMDKScanner;
begin
  FDelegate := TDWEMDKListenerDelegate.Create(Self);
  FEMDKScanner := TJDWEMDKScanner.JavaClass.init(TAndroidHelper.Context, FDelegate);
end;

class function TPlatformSymbolScanner.IsEMDKInstalled: Boolean;
begin
  Result := TJDWEMDKScanner.JavaClass.isEMDKInstalled(TAndroidHelper.Context);
end;

function TPlatformSymbolScanner.IsEnabled: Boolean;
begin
  Result := (FEMDKScanner <> nil) and (FEMDKScanner.getScanner <> nil) and FEMDKScanner.getScanner.isEnabled;
end;

function TPlatformSymbolScanner.CanScan: Boolean;
begin
  Result := IsEnabled and FIsIdle;
end;

procedure TPlatformSymbolScanner.CancelScan;
begin
  if (FEMDKScanner <> nil) and (FEMDKScanner.getScanner <> nil) and FEMDKScanner.getScanner.isReadPending then
    FEMDKScanner.getScanner.cancelRead;
end;

function TPlatformSymbolScanner.DoActivate(const AActivate: Boolean): Boolean;
begin
  if AActivate then
    Result := EnableScanner(True) and ResetHardReading
  else
    Result := EnableScanner(False);
  FIsActive := IsEnabled;
end;

procedure TPlatformSymbolScanner.Activate(const AActivate: Boolean);
begin
  if IsEMDKInstalled then
  begin
    if FEMDKScanner <> nil then
    begin
      if FIsOpen then
        DoActivate(AActivate);
    end
    else
      CreateEMDKScanner;
  end;
end;

function TPlatformSymbolScanner.EnableScanner(const AEnable: Boolean): Boolean;
begin
  Result := False;
  if FEMDKScanner <> nil then
  begin
    if AEnable then
    begin
      if not IsBackground then
      begin
        Result := FEMDKScanner.getIsInitialized and FEMDKScanner.enableScanner(True);
        if not Result then
          TOSLog.e('Unable to enable scanner');
      end
      else
        TOSLog.e('Cannot enable scanner when app is in the background');
    end
    else
      Result := FEMDKScanner.enableScanner(False);
  end;
end;

function TPlatformSymbolScanner.ResetHardReading: Boolean;
begin
  Result := False;
  if FEMDKScanner <> nil then
  begin
    SetScannerTrigger(TScannerTrigger.Hard);
    if IsEnabled and not FEMDKScanner.getScanner.isReadPending then
    begin
      FEMDKScanner.getScanner.read;
      Result := True;
    end;
  end;
end;

procedure TPlatformSymbolScanner.ScannerTriggerChanged;
begin
  inherited;
  if (FEMDKScanner <> nil) and (FEMDKScanner.getScanner <> nil) then
  begin
    TOSLog.d('Updating trigger to: %s', [cScannerTriggerNames[ScannerTrigger]]);
    if FEMDKScanner.getScanner.isReadPending then
      FEMDKScanner.getScanner.cancelRead;
    case ScannerTrigger of
      TScannerTrigger.Soft:
        FEMDKScanner.getScanner.triggerType := TJScanner_TriggerType.JavaClass.SOFT_ALWAYS;
      TScannerTrigger.SoftOnce:
        FEMDKScanner.getScanner.triggerType := TJScanner_TriggerType.JavaClass.SOFT_ONCE;
      TScannerTrigger.Hard:
        FEMDKScanner.getScanner.triggerType := TJScanner_TriggerType.JavaClass.HARD;
    end;
  end;
end;

function TPlatformSymbolScanner.Scan: Boolean;
begin
  Result := False;
  if IsEnabled then
  begin
    SetScannerTrigger(TScannerTrigger.SoftOnce);
    FEMDKScanner.getScanner.read;
    Result := True;
  end;
end;

procedure TPlatformSymbolScanner.ManagerClosed;
begin
  FIsOpen := False;
end;

procedure TPlatformSymbolScanner.ManagerOpened;
begin
  FIsOpen := True;
  DoActivate(True);
end;

procedure TPlatformSymbolScanner.ScannerData(scanDataCollection: JScanDataCollection);
var
  LScanDataList: JArrayList;
  LScanData: JScanDataCollection_ScanData;
  LData, LLabel: string;
  I: Integer;
begin
  if (scanDataCollection <> nil) and scanDataCollection.getResult.equals(TJScannerResults.JavaClass.SUCCESS) then
  begin
    LScanDataList := scanDataCollection.getScanData;
    for I := 0 to LScanDataList.size - 1 do
    begin
      LScanData := TJScanDataCollection_ScanData.Wrap((LScanDataList.get(I) as ILocalObject).GetObjectID);
      LData := JStringToString(LScanData.getData);
      LLabel := JStringToString(LScanData.getLabelType.toString);
      TThread.Synchronize(nil,
        procedure
        begin
          DoDataReceived(LData, LLabel);
        end
      );
    end;
  end;
end;

procedure TPlatformSymbolScanner.ScannerStatus(statusData: JStatusData);
var
  LState: JStatusData_ScannerStates;
  LStatus: TScannerStatus;
begin
  LStatus := TScannerStatus.Unknown;
  LState := statusData.getState;
  if LState.equals(TJStatusData_ScannerStates.JavaClass.SCANNING) then
    LStatus := TScannerStatus.Scanning
  else if LState.equals(TJStatusData_ScannerStates.JavaClass.IDLE) then
    LStatus := TScannerStatus.Idle
  else if LState.equals(TJStatusData_ScannerStates.JavaClass.WAITING) then
    LStatus := TScannerStatus.Waiting
  else if LState.equals(TJStatusData_ScannerStates.JavaClass.DISABLED) then
    LStatus := TScannerStatus.Disabled
  else if LState.equals(TJStatusData_ScannerStates.JavaClass.ERROR) then
    LStatus := TScannerStatus.Error;
  FIsIdle := LStatus in [TScannerStatus.Idle, TScannerStatus.Waiting];
  case LStatus of
    TScannerStatus.Idle, TScannerStatus.Error:
    begin
      if IsActive then
        ResetHardReading;
    end;
  end;
  DoScannerStatus(LStatus);
end;

end.
