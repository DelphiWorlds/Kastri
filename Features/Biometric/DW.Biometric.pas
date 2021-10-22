unit DW.Biometric;

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
  System.SysUtils;

type
  TBiometricStrength = (DeviceCredential, Strong, Weak);

  TBiometricStrengths = set of TBiometricStrength;

  TBiometricCapabilityResult = (Unknown, Available, HardwareUnavailable, NoHardware, NoneEnrolled, SecurityUpdateRequired, Unsupported);

  TBiometricFailResult = (Unknown, Cancelled, Fallback, Denied, LockedOut, Error, Help);

  TBiometryKind = (None, Face, Touch);

  TBiometricFailResultMethod = procedure(const FailResult: TBiometricFailResult; const ResultMessage: string) of object;

  TBiometric = class;

  TCustomPlatformBiometric = class(TObject)
  private
    FBiometric: TBiometric;
    FBiometricStrengths: TBiometricStrengths;
    FPromptCancelButtonText: string;
    FPromptConfirmationRequired: Boolean;
    FPromptDescription: string;
    FPromptSubtitle: string;
    FPromptTitle: string;
    procedure SetPromptCancelButtonText(const Value: string);
    procedure SetPromptDescription(const Value: string);
    procedure SetPromptSubtitle(const Value: string);
    procedure SetPromptTitle(const Value: string);
  protected
    procedure Cancel; virtual;
    function CanVerify: Boolean; virtual;
    function GetBiometricCapability: TBiometricCapabilityResult; virtual; abstract;
    function GetKeyName: string; virtual;
    function HasUserInterface: Boolean; virtual;
    function IsBiometryLockedOut: Boolean; virtual;
    procedure Reset; virtual;
    procedure RestoreBiometry(const ASuccessResultMethod: TProc; const AResultMethod: TBiometricFailResultMethod); virtual;
    procedure SetKeyName(const AValue: string); virtual;
    procedure SetReuseTime(const AInterval: Double); virtual;
    procedure Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod); virtual;
    property Biometric: TBiometric read FBiometric;
    property BiometricStrengths: TBiometricStrengths read FBiometricStrengths write FBiometricStrengths;
    property PromptCancelButtonText: string read FPromptCancelButtonText write SetPromptCancelButtonText;
    property PromptDescription: string read FPromptDescription write SetPromptDescription;
    property PromptSubtitle: string read FPromptSubtitle write SetPromptSubtitle;
    property PromptTitle: string read FPromptTitle write SetPromptTitle;
    property PromptConfirmationRequired: Boolean read FPromptConfirmationRequired write FPromptConfirmationRequired;
  public
    class function GetBiometryKind: TBiometryKind; virtual;
    class function IsSupported: Boolean; virtual;
  public
    constructor Create(const ABiometric: TBiometric); virtual;
  end;

  TBiometric = class(TObject)
  private
    class var FCurrent: TBiometric;
    class function GetCurrent: TBiometric; static;
  private
    FPlatformBiometric: TCustomPlatformBiometric;
    function GetPromptCancelButtonText: string;
    function GetPromptConfirmationRequired: Boolean;
    function GetPromptDescription: string;
    function GetPromptSubtitle: string;
    function GetPromptTitle: string;
    procedure SetPromptCancelButtonText(const Value: string);
    procedure SetPromptConfirmationRequired(const Value: Boolean);
    procedure SetPromptDescription(const Value: string);
    procedure SetPromptSubtitle(const Value: string);
    procedure SetPromptTitle(const Value: string);
  public
    class destructor DestroyClass;
    class property Current: TBiometric read GetCurrent;
    class function GetBiometryKind: TBiometryKind;
    class function IsSupported: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    ///   Stops listening for verification
    /// </summary>
    procedure Cancel;
    /// <summary>
    ///   Indicates that touch id verification is available
    /// </summary>
    function CanVerify: Boolean;
    function GetKeyName: string;
    /// <summary>
    ///   Indicates that the platform provides the UI
    /// </summary>
    /// <remarks>
    ///   If this function returns False, the code should provide a prompt for the user.
    ///   On Android, the prompt should follow this guideline: https://material.google.com/patterns/fingerprint.html
    /// </remarks>
    function HasUserInterface: Boolean;
    /// <summary>
    ///   Indicates that the biometry will require a PIN to restore it
    /// </summary>
    function IsBiometryLockedOut: Boolean;
    /// <summary>
    ///   Resets authentication (if supported)
    /// </summary>
    procedure Reset;
    /// <summary>
    ///   Attempts to restore biometry
    /// </summary>
    procedure RestoreBiometry(const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
    procedure SetKeyName(const AValue: string);
    /// <summary>
    ///   Set the amount of time the fingerprint can be used for, in seconds
    /// </summary>
    procedure SetReuseTime(const AInterval: Double);
    /// <summary>
    ///   Verifies the fingerprint, passing the result in AResultMethod
    /// </summary>
    procedure Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
    /// <summary>
    ///   Key name used for key storage (currently applies to Android only - Delphi 10.4.x)
    /// </summary>
    property KeyName: string read GetKeyName write SetKeyName;
    property PromptCancelButtonText: string read GetPromptCancelButtonText write SetPromptCancelButtonText;
    property PromptDescription: string read GetPromptDescription write SetPromptDescription;
    property PromptSubtitle: string read GetPromptSubtitle write SetPromptSubtitle;
    property PromptTitle: string read GetPromptTitle write SetPromptTitle;
    property PromptConfirmationRequired: Boolean read GetPromptConfirmationRequired write SetPromptConfirmationRequired;
  end;

resourcestring
  SBiometricNotImplemented = 'Biometric support is not implemented for this platform';
  SBiometricErrorKeyNameEmpty = 'Keyname cannot be empty';
  SBiometricErrorCannotVerify = 'Unable to perform verification';
  SBiometricErrorKeyGenerationFailed = 'Failed to generate key';
  SBiometricErrorCipherGenerationFailed = 'Failed to generate cipher';
  SBiometricErrorSystemError = 'A system error occurred: %s';
  SBiometricErrorNotAvailable = 'Biometrics not available';
  SBiometricEnterPINToRestore = 'Enter PIN to restore biometry';
  SFingerprintNotImplemented = 'Fingerprints support is not implemented for this platform';
  SFingerprintErrorNotAvailable = 'Fingerprints not available';

implementation

uses
  {$IF Defined(IOS)}
  DW.Biometric.iOS;
  {$ELSEIF Defined(ANDROID)}
  {$IF CompilerVersion >= 35}
  DW.Biometric.Android;
  {$ELSE}
  DW.Biometric.Android.Legacy;
  {$ENDIF}
  {$ELSE}
  DW.Biometric.Default;
  {$ENDIF}

{ TCustomPlatformBiometric }

constructor TCustomPlatformBiometric.Create(const ABiometric: TBiometric);
begin
  inherited Create;
  FBiometric := ABiometric;
  FPromptConfirmationRequired := False;
  FPromptCancelButtonText := 'Cancel';
  FPromptDescription := 'I need to know it''s really you';
  FPromptTitle := 'Authenticate!';
end;

procedure TCustomPlatformBiometric.Cancel;
begin
  //
end;

function TCustomPlatformBiometric.CanVerify: Boolean;
begin
  Result := False;
end;

class function TCustomPlatformBiometric.GetBiometryKind: TBiometryKind;
begin
  Result := TBiometryKind.None;
end;

function TCustomPlatformBiometric.GetKeyName: string;
begin
  Result := '';
end;

function TCustomPlatformBiometric.HasUserInterface: Boolean;
begin
  Result := False;
end;

function TCustomPlatformBiometric.IsBiometryLockedOut: Boolean;
begin
  Result := False;
end;

class function TCustomPlatformBiometric.IsSupported: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformBiometric.Reset;
begin
  //
end;

procedure TCustomPlatformBiometric.RestoreBiometry(const ASuccessResultMethod: TProc; const AResultMethod: TBiometricFailResultMethod);
begin
  if Assigned(AResultMethod) then
    AResultMethod(TBiometricFailResult.Error, SBiometricNotImplemented);
end;

procedure TCustomPlatformBiometric.SetKeyName(const AValue: string);
begin
  //
end;

procedure TCustomPlatformBiometric.SetPromptCancelButtonText(const Value: string);
begin
  FPromptCancelButtonText := Value;
end;

procedure TCustomPlatformBiometric.SetPromptDescription(const Value: string);
begin
  FPromptDescription := Value;
end;

procedure TCustomPlatformBiometric.SetPromptSubtitle(const Value: string);
begin
  FPromptSubtitle := Value;
end;

procedure TCustomPlatformBiometric.SetPromptTitle(const Value: string);
begin
  FPromptTitle := Value;
end;

procedure TCustomPlatformBiometric.SetReuseTime(const AInterval: Double);
begin
  //
end;

procedure TCustomPlatformBiometric.Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
begin
  if Assigned(AFailResultMethod) then
    AFailResultMethod(TBiometricFailResult.Error, SBiometricNotImplemented);
end;

{ TBiometric }

class destructor TBiometric.DestroyClass;
begin
  FCurrent.Free;
  FCurrent := nil;
end;

constructor TBiometric.Create;
begin
  inherited;
  FPlatformBiometric := TPlatformBiometric.Create(Self);
end;

destructor TBiometric.Destroy;
begin
  FPlatformBiometric.Free;
  inherited;
end;

procedure TBiometric.Cancel;
begin
  FPlatformBiometric.Cancel;
end;

function TBiometric.CanVerify: Boolean;
begin
  Result := FPlatformBiometric.CanVerify;
end;

class function TBiometric.GetBiometryKind: TBiometryKind;
begin
  Result := TPlatformBiometric.GetBiometryKind;
end;

class function TBiometric.GetCurrent: TBiometric;
begin
  if FCurrent = nil then
    FCurrent := TBiometric.Create;
  Result := FCurrent;
end;

function TBiometric.GetKeyName: string;
begin
  Result := FPlatformBiometric.GetKeyName;
end;

function TBiometric.GetPromptCancelButtonText: string;
begin
  Result := FPlatformBiometric.PromptCancelButtonText;
end;

function TBiometric.GetPromptConfirmationRequired: Boolean;
begin
  Result := FPlatformBiometric.PromptConfirmationRequired;
end;

function TBiometric.GetPromptDescription: string;
begin
  Result := FPlatformBiometric.PromptDescription;
end;

function TBiometric.GetPromptSubtitle: string;
begin
  Result := FPlatformBiometric.PromptSubtitle;
end;

function TBiometric.GetPromptTitle: string;
begin
  Result := FPlatformBiometric.PromptTitle;
end;

function TBiometric.HasUserInterface: Boolean;
begin
  Result := FPlatformBiometric.HasUserInterface;
end;

function TBiometric.IsBiometryLockedOut: Boolean;
begin
  Result := FPlatformBiometric.IsBiometryLockedOut;
end;

class function TBiometric.IsSupported: Boolean;
begin
  Result := TPlatformBiometric.IsSupported;
end;

procedure TBiometric.Reset;
begin
  FPlatformBiometric.Reset;
end;

procedure TBiometric.RestoreBiometry(const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
begin
  FPlatformBiometric.RestoreBiometry(ASuccessResultMethod, AFailResultMethod);
end;

procedure TBiometric.SetKeyName(const AValue: string);
begin
  FPlatformBiometric.SetKeyName(AValue);
end;

procedure TBiometric.SetPromptCancelButtonText(const Value: string);
begin
  FPlatformBiometric.PromptCancelButtonText:=Value;
end;

procedure TBiometric.SetPromptConfirmationRequired(const Value: Boolean);
begin
  FPlatformBiometric.PromptConfirmationRequired:=Value;
end;

procedure TBiometric.SetPromptDescription(const Value: string);
begin
  FPlatformBiometric.PromptDescription:=Value;
end;

procedure TBiometric.SetPromptSubtitle(const Value: string);
begin
  FPlatformBiometric.PromptSubtitle:=Value;
end;

procedure TBiometric.SetPromptTitle(const Value: string);
begin
  FPlatformBiometric.PromptTitle:=Value;
end;

procedure TBiometric.SetReuseTime(const AInterval: Double);
begin
  FPlatformBiometric.SetReuseTime(AInterval);
end;

procedure TBiometric.Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
begin
  FPlatformBiometric.Verify(AMessage, ASuccessResultMethod, AFailResultMethod);
end;

end.
