unit DW.HoneywellScanner.Android;

interface

uses
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  DW.Androidapi.JNI.DataCollection, DW.SymbolScanner;

type
  TPlatformHoneywellScanner = class;

  TAidcManagerCreatedCallback = class(TJavaLocal, JAidcManager_CreatedCallback)
  private
    FScanner: TPlatformHoneywellScanner;
  public
    { JAidcManager_CreatedCallback }
    procedure onCreated(aidcManager: JAidcManager); cdecl;
  public
    constructor Create(const AScanner: TPlatformHoneywellScanner);
  end;

  TBarcodeReaderListener = class(TJavaLocal, JBarcodeReader_BarcodeListener)
  private
    FScanner: TPlatformHoneywellScanner;
  public
    { JBarcodeReader_BarcodeListener }
    procedure onBarcodeEvent(barcodeReadEvent: JBarcodeReadEvent); cdecl;
    procedure onFailureEvent(barcodeFailureEvent: JBarcodeFailureEvent); cdecl;
  public
    constructor Create(const AScanner: TPlatformHoneywellScanner);
  end;

  TPlatformHoneywellScanner = class(TCustomPlatformHoneywellScanner)
  private
    FBarcodeReader: JBarcodeReader;
    FBarcodeReaderListener: JBarcodeReader_BarcodeListener;
    FManager: JAidcManager;
    FManagerCreatedCallback: JAidcManager_CreatedCallback;
    procedure CreateManager;
    procedure CreateReader;
    procedure DestroyManager;
    procedure DestroyReader;
  protected
    procedure Activate(const AActivate: Boolean); override;
    procedure BarcodeEvent(const AEvent: JBarcodeReadEvent);
    procedure CancelScan; override;
    function CanScan: Boolean; override;
    function EnableScanner(const AEnable: Boolean): Boolean; override;
    procedure FailureEvent(const AEvent: JBarcodeFailureEvent);
    procedure ManagerCreated(const AManager: JAidcManager);
    function Scan: Boolean; override;
    procedure ScannerTriggerChanged; override;
  public
    constructor Create(const AHoneywellScanner: THoneywellScanner); override;
    destructor Destroy; override;
  end;

implementation

uses
  Androidapi.Helpers,
  DW.OSLog;

{ TAidcManagerCreatedCallback }

constructor TAidcManagerCreatedCallback.Create(const AScanner: TPlatformHoneywellScanner);
begin
  inherited Create;
  FScanner := AScanner;
end;

procedure TAidcManagerCreatedCallback.onCreated(aidcManager: JAidcManager);
begin
  TOSLog.d('TAidcManagerCreatedCallback.onCreated');
  FScanner.ManagerCreated(aidcManager);
end;

{ TBarcodeReaderListener }

constructor TBarcodeReaderListener.Create(const AScanner: TPlatformHoneywellScanner);
begin
  inherited Create;
  FScanner := AScanner;
end;

procedure TBarcodeReaderListener.onBarcodeEvent(barcodeReadEvent: JBarcodeReadEvent);
begin
  FScanner.BarcodeEvent(barcodeReadEvent);
end;

procedure TBarcodeReaderListener.onFailureEvent(barcodeFailureEvent: JBarcodeFailureEvent);
begin
  FScanner.FailureEvent(barcodeFailureEvent);
end;

{ TPlatformHoneywellScanner }

constructor TPlatformHoneywellScanner.Create(const AHoneywellScanner: THoneywellScanner);
begin
  inherited;
  FManagerCreatedCallback := TAidcManagerCreatedCallback.Create(Self);
  FBarcodeReaderListener := TBarcodeReaderListener.Create(Self);
end;

procedure TPlatformHoneywellScanner.CreateManager;
begin
  TJAidcManager.JavaClass.create(TAndroidHelper.Context, FManagerCreatedCallback);
end;

procedure TPlatformHoneywellScanner.CreateReader;
begin
  FBarcodeReader := FManager.createBarcodeReader;
  FBarcodeReader.addBarcodeListener(FBarcodeReaderListener);
  FBarcodeReader.setProperty(TJBarcodeReader.JavaClass.PROPERTY_TRIGGER_CONTROL_MODE, TJBarcodeReader.JavaClass.TRIGGER_CONTROL_MODE_AUTO_CONTROL);
  EnableScanner(True);
end;

destructor TPlatformHoneywellScanner.Destroy;
begin
  DestroyReader;
  DestroyManager;
  inherited;
end;

procedure TPlatformHoneywellScanner.DestroyManager;
begin
  if FManager <> nil then
  begin
    FManager.close;
    FManager := nil;
  end;
end;

procedure TPlatformHoneywellScanner.DestroyReader;
begin
  if FBarcodeReader <> nil then
  begin
    FBarcodeReader.release;
    FBarcodeReader.removeBarcodeListener(FBarcodeReaderListener);
    // Trigger too
    FBarcodeReader.close;
    FBarcodeReader := nil;
  end;
end;

procedure TPlatformHoneywellScanner.ManagerCreated(const AManager: JAidcManager);
begin
  FManager := AManager;
  CreateReader;
end;

procedure TPlatformHoneywellScanner.BarcodeEvent(const AEvent: JBarcodeReadEvent);
begin
  TOSLog.d('BarcodeEvent - Code: %s, Data: %s', [JStringToString(AEvent.getAimId), JStringToString(AEvent.getBarcodeData)]);
  // AEvent.getBarcodeData;
  // AEvent.getCodeId
  DoDataReceived(JStringToString(AEvent.getBarcodeData), JStringToString(AEvent.getAimId));
end;

procedure TPlatformHoneywellScanner.FailureEvent(const AEvent: JBarcodeFailureEvent);
begin
  TOSLog.e('FailureEvent: %s', [JStringToString(AEvent.toString)]);
end;

procedure TPlatformHoneywellScanner.Activate(const AActivate: Boolean);
begin
  if FManager = nil then
    CreateManager
  else
    EnableScanner(AActivate);
end;

procedure TPlatformHoneywellScanner.CancelScan;
begin
  //
end;

function TPlatformHoneywellScanner.CanScan: Boolean;
begin
  Result := FBarcodeReader <> nil;
end;

function TPlatformHoneywellScanner.EnableScanner(const AEnable: Boolean): Boolean;
begin
  Result := False;
  if AEnable then
  begin
    if not IsBackground and (FBarcodeReader <> nil) then
      FBarcodeReader.claim
    else
      TOSLog.e('Cannot enable scanner when app is in the background, or reader is nil');
  end
  else
  begin
    if FBarcodeReader <> nil then
      FBarcodeReader.release;
    Result := True;
  end;
end;

function TPlatformHoneywellScanner.Scan: Boolean;
begin
  Result := False;
  if FBarcodeReader <> nil then
  begin
    SetScannerTrigger(TScannerTrigger.SoftOnce);
    // FBarcodeReader. // NFI
    Result := True;
  end;
end;

procedure TPlatformHoneywellScanner.ScannerTriggerChanged;
begin
  // nil
end;

end.
