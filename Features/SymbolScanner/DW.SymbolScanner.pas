unit DW.SymbolScanner;

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

  TSymbolScanner = class;

  TCustomPlatformSymbolScanner = class(TObject)
  private
    FIsBackground: Boolean;
    FScannerTrigger: TScannerTrigger;
    FSymbolScanner: TSymbolScanner;
    procedure ApplicationEventMessageHandler(const ASender: TObject; const AMsg: TMessage);
  protected
    FIsActive: Boolean;
    function Activate(const AActivate: Boolean): Boolean; virtual; abstract;
    procedure CancelScan; virtual; abstract;
    function CanScan: Boolean; virtual; abstract;
    procedure DoDataReceived(const AData, ALabelType: string);
    procedure DoScannerStatus(const AStatus: TScannerStatus);
    function EnableScanner(const AEnable: Boolean): Boolean; virtual; abstract;
    function IsEMDKInstalled: Boolean; virtual; abstract;
    function Scan: Boolean; virtual; abstract;
    procedure SetIsActive(const Value: Boolean);
    procedure SetScannerTrigger(const Value: TScannerTrigger);
    procedure ScannerTriggerChanged; virtual; abstract;
    property IsActive: Boolean read FIsActive;
    property IsBackground: Boolean read FIsBackground;
    property ScannerTrigger: TScannerTrigger read FScannerTrigger write SetScannerTrigger;
    property SymbolScanner: TSymbolScanner read FSymbolScanner;
  public
    constructor Create(const ASymbolScanner: TSymbolScanner); virtual;
    destructor Destroy; override;
  end;

  TSymbolScanner = class(TObject)
  private
    FPlatformSymbolScanner: TCustomPlatformSymbolScanner;
    FOnDataReceived: TScannerDataReceivedEvent;
    FOnStatusChange: TScannerStatusEvent;
    function GetIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);
  protected
    procedure DoDataReceived(const AData, ALabelType: string);
    procedure DoScannerStatus(const AStatus: TScannerStatus);
  public
    constructor Create;
    destructor Destroy; override;
    function IsEMDKInstalled: Boolean;
    function CanScan: Boolean;
    function Scan: Boolean;
    property IsActive: Boolean read GetIsActive write SetIsActive;
    property OnDataReceived: TScannerDataReceivedEvent read FOnDataReceived write FOnDataReceived;
    property OnStatusChange: TScannerStatusEvent read FOnStatusChange write FOnStatusChange;
  end;

implementation

uses
  FMX.Platform,
  {$IF Defined(ANDROID)}
  DW.SymbolScanner.Android;
  {$ENDIF}

{ TCustomPlatformSymbolScanner }

constructor TCustomPlatformSymbolScanner.Create(const ASymbolScanner: TSymbolScanner);
begin
  inherited Create;
  FSymbolScanner := ASymbolScanner;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TCustomPlatformSymbolScanner.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TCustomPlatformSymbolScanner.DoDataReceived(const AData, ALabelType: string);
begin
  FSymbolScanner.DoDataReceived(AData, ALabelType);
end;

procedure TCustomPlatformSymbolScanner.DoScannerStatus(const AStatus: TScannerStatus);
begin
  FSymbolScanner.DoScannerStatus(AStatus);
end;

procedure TCustomPlatformSymbolScanner.SetIsActive(const Value: Boolean);
begin
  if FIsActive <> Value then
    Activate(Value);
end;

procedure TCustomPlatformSymbolScanner.SetScannerTrigger(const Value: TScannerTrigger);
begin
  FScannerTrigger := Value;
  ScannerTriggerChanged;
end;

procedure TCustomPlatformSymbolScanner.ApplicationEventMessageHandler(const ASender: TObject; const AMsg: TMessage);
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

{ TSymbolScanner }

constructor TSymbolScanner.Create;
begin
  inherited;
  FPlatformSymbolScanner := TPlatformSymbolScanner.Create(Self);
end;

destructor TSymbolScanner.Destroy;
begin
  FPlatformSymbolScanner.Free;
  inherited;
end;

procedure TSymbolScanner.DoDataReceived(const AData, ALabelType: string);
begin
  if Assigned(FOnDataReceived) then
    FOnDataReceived(Self, AData, ALabelType);
end;

procedure TSymbolScanner.DoScannerStatus(const AStatus: TScannerStatus);
begin
  if Assigned(FOnStatusChange) then
    FOnStatusChange(Self, AStatus);
end;

function TSymbolScanner.GetIsActive: Boolean;
begin
  Result := FPlatformSymbolScanner.IsActive;
end;

function TSymbolScanner.IsEMDKInstalled: Boolean;
begin
  Result := FPlatformSymbolScanner.IsEMDKInstalled;
end;

function TSymbolScanner.CanScan: Boolean;
begin
  Result := FPlatformSymbolScanner.CanScan;
end;

function TSymbolScanner.Scan: Boolean;
begin
  Result := FPlatformSymbolScanner.Scan;
end;

procedure TSymbolScanner.SetIsActive(const Value: Boolean);
begin
  FPlatformSymbolScanner.SetIsActive(Value);
end;

end.
