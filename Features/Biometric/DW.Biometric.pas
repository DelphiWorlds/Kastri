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
  TBiometricFailResult = (Unknown, Cancelled, Fallback, Denied, LockedOut, Error, Help);

  TBiometryKind = (None, Face, Touch);

  TBiometricFailResultMethod = procedure (const FailResult: TBiometricFailResult; const ResultMessage: string) of object;

  TBiometric = class;

  TCustomPlatformBiometric = class(TObject)
  private
    FBiometric: TBiometric;
  protected
    procedure Cancel; virtual;
    function CanVerify: Boolean; virtual;
    function GetKeyName: string; virtual;
    function HasUserInterface: Boolean; virtual;
    function IsBiometryLockedOut: Boolean; virtual;
    procedure Reset; virtual;
    procedure RestoreBiometry(const ASuccessResultMethod: TProc; const AResultMethod: TBiometricFailResultMethod); virtual;
    procedure SetKeyName(const AValue: string); virtual;
    procedure SetReuseTime(const AInterval: Double); virtual;
    procedure Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod); virtual;
    property Biometric: TBiometric read FBiometric;
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
    ///   Key name used for key storage (currently applies to Android only)
    /// </summary>
    property KeyName: string read GetKeyName write SetKeyName;
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

// DW
{$IF Defined(IOS)}
uses
  DW.Biometric.iOS;
{$ELSEIF Defined(Android)}
uses
  DW.Biometric.Android;
{$ELSE}
type
  TPlatformBiometric = class(TCustomPlatformBiometric);
{$ENDIF}

{ TCustomPlatformBiometric }

constructor TCustomPlatformBiometric.Create(const ABiometric: TBiometric);
begin
  inherited Create;
  FBiometric := ABiometric;
end;

function TCustomPlatformBiometric.CanVerify: Boolean;
begin
  Result := False;
end;

procedure TCustomPlatformBiometric.Cancel;
begin
  //
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
  //
end;

procedure TCustomPlatformBiometric.SetKeyName(const AValue: string);
begin
  //
end;

procedure TCustomPlatformBiometric.SetReuseTime(const AInterval: Double);
begin
  //
end;

procedure TCustomPlatformBiometric.Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
begin
  if Assigned(AFailResultMethod) then
    AFailResultMethod(TBiometricFailResult.Unknown, SBiometricNotImplemented);
end;

{ TBiometric }

constructor TBiometric.Create;
begin
  inherited;
  if TPlatformBiometric.IsSupported then
    FPlatformBiometric := TPlatformBiometric.Create(Self)
  else
    FPlatformBiometric := TCustomPlatformBiometric.Create(Self);
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

procedure TBiometric.SetReuseTime(const AInterval: Double);
begin
  FPlatformBiometric.SetReuseTime(AInterval);
end;

class destructor TBiometric.DestroyClass;
begin
  FCurrent.Free;
  FCurrent := nil;
end;

procedure TBiometric.Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
begin
  FPlatformBiometric.Verify(AMessage, ASuccessResultMethod, AFailResultMethod);
end;

end.
