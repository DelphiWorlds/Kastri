unit DW.SymbolScanner;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2023 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // RTL
  System.Messaging;

type
  TScannerTrigger = (Uninitialised, Soft, SoftOnce, Hard);
  TScannerStatus = (Unknown, Idle, Scanning, Waiting, Disabled, Error);

const
  cScannerTriggerNames: array[TScannerTrigger] of string = ('Uninitialised', 'Soft', 'Soft Once', 'Hard');
  cScannerStatusCaptions: array[TScannerStatus] of string = ('Unknown', 'Idle', 'Scanning', 'Waiting', 'Disabled', 'Error');

type
  TScannerDataReceivedEvent = procedure(Sender: TObject; const Data, LabelType: string) of object;
  TScannerStatusEvent = procedure(Sender: TObject; const Status: TScannerStatus) of object;

  TBaseScanner = class;

  TCustomPlatformBaseScanner = class(TObject)
  private
    FIsBackground: Boolean;
    FScannerTrigger: TScannerTrigger;
    FBaseScanner: TBaseScanner;
    procedure ApplicationEventMessageHandler(const ASender: TObject; const AMsg: TMessage);
  protected
    FIsActive: Boolean;
    procedure Activate(const AActivate: Boolean); virtual; abstract;
    procedure CancelScan; virtual; abstract;
    function CanScan: Boolean; virtual; abstract;
    procedure DoDataReceived(const AData, ALabelType: string);
    procedure DoScannerStatus(const AStatus: TScannerStatus);
    function EnableScanner(const AEnable: Boolean): Boolean; virtual; abstract;
    function Scan: Boolean; virtual; abstract;
    procedure SetIsActive(const Value: Boolean);
    procedure SetScannerTrigger(const Value: TScannerTrigger);
    procedure ScannerTriggerChanged; virtual; abstract;
    property IsActive: Boolean read FIsActive;
    property IsBackground: Boolean read FIsBackground;
    property ScannerTrigger: TScannerTrigger read FScannerTrigger write SetScannerTrigger;
    property BaseScanner: TBaseScanner read FBaseScanner;
  public
    constructor Create(const ABaseScanner: TBaseScanner); virtual;
    destructor Destroy; override;
  end;

  TSymbolScanner = class;

  TCustomPlatformSymbolScanner = class(TCustomPlatformBaseScanner)
  public
    constructor Create(const ASymbolScanner: TSymbolScanner); reintroduce; virtual;
  end;

  THoneywellScanner = class;

  TCustomPlatformHoneywellScanner = class(TCustomPlatformBaseScanner)
  public
    constructor Create(const AHoneywellScanner: THoneywellScanner); reintroduce; virtual;
  end;

  TBaseScanner = class(TObject)
  private
    FOnDataReceived: TScannerDataReceivedEvent;
    FOnStatusChange: TScannerStatusEvent;
    function GetIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);
  protected
    FPlatformBaseScanner: TCustomPlatformBaseScanner;
    procedure DoDataReceived(const AData, ALabelType: string);
    procedure DoScannerStatus(const AStatus: TScannerStatus);
  public
    destructor Destroy; override;
    function CanScan: Boolean;
    function Scan: Boolean;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property OnDataReceived: TScannerDataReceivedEvent read FOnDataReceived write FOnDataReceived;
    property OnStatusChange: TScannerStatusEvent read FOnStatusChange write FOnStatusChange;
  end;

  TSymbolScanner = class(TBaseScanner)
  public
    class function IsEMDKInstalled: Boolean;
  public
    constructor Create;
  end;

  THoneywellScanner = class(TBaseScanner)
  public
    constructor Create;
  end;

implementation

uses
  FMX.Platform,
  {$IF Defined(ANDROID)}
  DW.SymbolScanner.Android,
  DW.HoneywellScanner.Android;
  {$ENDIF}

type
  TSymbolScannerHelper = class helper for TSymbolScanner
  protected
    function PlatformSymbolScanner: TPlatformSymbolScanner;
  end;

{ TCustomPlatformBaseScanner }

constructor TCustomPlatformBaseScanner.Create(const ABaseScanner: TBaseScanner);
begin
  inherited Create;
  FBaseScanner := ABaseScanner;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TCustomPlatformBaseScanner.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TCustomPlatformBaseScanner.DoDataReceived(const AData, ALabelType: string);
begin
  FBaseScanner.DoDataReceived(AData, ALabelType);
end;

procedure TCustomPlatformBaseScanner.DoScannerStatus(const AStatus: TScannerStatus);
begin
  FBaseScanner.DoScannerStatus(AStatus);
end;

procedure TCustomPlatformBaseScanner.SetIsActive(const Value: Boolean);
begin
  if FIsActive <> Value then
    Activate(Value);
end;

procedure TCustomPlatformBaseScanner.SetScannerTrigger(const Value: TScannerTrigger);
begin
  FScannerTrigger := Value;
  ScannerTriggerChanged;
end;

procedure TCustomPlatformBaseScanner.ApplicationEventMessageHandler(const ASender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.WillBecomeInactive:
    begin
      FIsBackground := True;
      EnableScanner(False);
    end;
    TApplicationEvent.BecameActive:
    begin
      FIsBackground := False;
      if FIsActive then
        Activate(True);
    end
  end;
end;

{ TCustomPlatformSymbolScanner }

constructor TCustomPlatformSymbolScanner.Create(const ASymbolScanner: TSymbolScanner);
begin
  inherited Create(ASymbolScanner);
end;

{ TCustomPlatformHoneywellScanner }

constructor TCustomPlatformHoneywellScanner.Create(const AHoneywellScanner: THoneywellScanner);
begin
  inherited Create(AHoneywellScanner);
end;

{ TBaseScanner }

destructor TBaseScanner.Destroy;
begin
  FPlatformBaseScanner.Free;
  inherited;
end;

procedure TBaseScanner.DoDataReceived(const AData, ALabelType: string);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(Self, AData, ALabelType);
end;

procedure TBaseScanner.DoScannerStatus(const AStatus: TScannerStatus);
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self, AStatus);
end;

function TBaseScanner.GetIsActive: Boolean;
begin
  Result := FPlatformBaseScanner.IsActive;
end;

function TBaseScanner.CanScan: Boolean;
begin
  Result := FPlatformBaseScanner.CanScan;
end;

function TBaseScanner.Scan: Boolean;
begin
  Result := FPlatformBaseScanner.Scan;
end;

procedure TBaseScanner.SetIsActive(const Value: Boolean);
begin
  FPlatformBaseScanner.SetIsActive(Value);
end;

{ TSymbolScanner }

constructor TSymbolScanner.Create;
begin
  inherited;
  FPlatformBaseScanner := TPlatformSymbolScanner.Create(Self);
end;

class function TSymbolScanner.IsEMDKInstalled: Boolean;
begin
  Result := TPlatformSymbolScanner.IsEMDKInstalled;
end;

{ TSymbolScannerHelper }

function TSymbolScannerHelper.PlatformSymbolScanner: TPlatformSymbolScanner;
begin
  Result := TPlatformSymbolScanner(FPlatformBaseScanner);
end;

{ THoneywellScanner }

constructor THoneywellScanner.Create;
begin
  inherited;
  FPlatformBaseScanner := TPlatformHoneywellScanner.Create(Self);
end;

end.
