unit DW.Biometric.iOS;

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
  System.SysUtils,
  // macOS
  Macapi.ObjectiveC, Macapi.Helpers,
  // iOS
  iOSapi.Foundation, iOSapi.CocoaTypes, iOSapi.Security,
  // DW
  DW.Biometric;

const
  LAPolicyDeviceOwnerAuthenticationWithBiometrics = 1;
  LAPolicyDeviceOwnerAuthentication = 2;
  LAPolicyDeviceOwnerAuthenticationWithWatch = 3;
  LAPolicyDeviceOwnerAuthenticationWithBiometricsOrWatch = 4;
  LACredentialTypeApplicationPassword = 0;
  LACredentialTypeSmartCardPIN = -3;
  LAAccessControlOperationCreateItem = 0;
  LAAccessControlOperationUseItem = 1;
  LAAccessControlOperationCreateKey = 2;
  LAAccessControlOperationUseKeySign = 3;
  LAAccessControlOperationUseKeyDecrypt = 4;
  LAAccessControlOperationUseKeyKeyExchange = 5;
  LABiometryTypeNone = 0;
  LABiometryTypeTouchID = 1;
  LABiometryTypeFaceID = 2;
  LAErrorAuthenticationFailed = -1;
  LAErrorUserCancel = -2;
  LAErrorUserFallback = -3;
  LAErrorSystemCancel = -4;
  LAErrorPasscodeNotSet = -5;
  LAErrorTouchIDNotAvailable = -6;
  LAErrorTouchIDNotEnrolled = -7;
  LAErrorTouchIDLockout = -8;
  LAErrorAppCancel = -9;
  LAErrorInvalidContext = -10;
  LAErrorBiometryNotAvailable = -6;
  LAErrorBiometryNotEnrolled = -7;
  LAErrorBiometryLockout = -8;
  LAErrorNotInteractive = -1004;
  LAErrorWatchNotAvailable = -11;

type
  LAContext = interface;

  LAPolicy = NSInteger;
  LACredentialType = NSInteger;
  LAAccessControlOperation = NSInteger;
  LABiometryType = NSInteger;
  LAError = NSInteger;
  SecAccessControlRef = Pointer;

  LAContextEvaluateAccessControlReplyHandler = procedure(success: Boolean; error: Pointer) of object;
  LAContextEvaluatePolicyReplyHandler = procedure(success: Boolean; error: Pointer) of object;

  LAContextClass = interface(NSObjectClass)
    ['{CB05A3B4-0AA5-47FB-9591-4AE4B8955AD4}']
  end;

  LAContext = interface(NSObject)
    ['{0FCAC33A-E0AD-4A03-BD42-C77A11D76641}']
    function biometryType: LABiometryType; cdecl;
    function canEvaluatePolicy(policy: LAPolicy; error: PPointer): Boolean; cdecl;
    procedure evaluateAccessControl(accessControl: SecAccessControlRef; operation: LAAccessControlOperation; localizedReason: NSString;
      reply: LAContextEvaluateAccessControlReplyHandler); cdecl;
    function evaluatedPolicyDomainState: NSData; cdecl;
    procedure evaluatePolicy(policy: LAPolicy; localizedReason: NSString; reply: LAContextEvaluatePolicyReplyHandler); cdecl;
    function interactionNotAllowed: Boolean; cdecl;
    procedure invalidate; cdecl;
    function isCredentialSet(&type: LACredentialType): Boolean; cdecl;
    function localizedCancelTitle: NSString; cdecl;
    function localizedFallbackTitle: NSString; cdecl;
    function localizedReason: NSString; cdecl;
    function maxBiometryFailures: NSNumber; cdecl;
    function setCredential(credential: NSData; &type: LACredentialType): Boolean; cdecl;
    procedure setInteractionNotAllowed(interactionNotAllowed: Boolean); cdecl;
    procedure setLocalizedCancelTitle(localizedCancelTitle: NSString); cdecl;
    procedure setLocalizedFallbackTitle(localizedFallbackTitle: NSString); cdecl;
    procedure setLocalizedReason(localizedReason: NSString); cdecl;
    procedure setMaxBiometryFailures(maxBiometryFailures: NSNumber); cdecl;
    procedure setTouchIDAuthenticationAllowableReuseDuration(touchIDAuthenticationAllowableReuseDuration: NSTimeInterval); cdecl;
    function touchIDAuthenticationAllowableReuseDuration: NSTimeInterval; cdecl;
  end;
  TLAContext = class(TOCGenericImport<LAContextClass, LAContext>) end;

  TPlatformBiometric = class(TCustomPlatformBiometric)
  private
    const cBiometryMode = LAPolicyDeviceOwnerAuthentication;
    class function CheckBiometry: NSError;
  private
    FContext: LAContext;
    FRestoreBiometryFailResultMethod: TBiometricFailResultMethod;
    FRestoreBiometrySuccessResultMethod: TProc;
    FVerifyFailResultMethod: TBiometricFailResultMethod;
    FVerifySuccessResultMethod: TProc;
    procedure CreateContext;
    procedure ReleaseContext;
    procedure DoVerifySuccessResult;
    procedure DoVerifyFailResult(const AResult: TBiometricFailResult; const AResultMessage: string);
    procedure DoRestoreBiometryFailResult(const AResult: TBiometricFailResult; const AResultMessage: string);
    procedure DoRestoreBiometrySuccessResult;
    procedure ProcessFailResult(error: Pointer; var AResult: TBiometricFailResult; var AMessage: string);
    procedure RestoreBiometryReplyHandler(success: Boolean; error: Pointer);
    procedure VerifyReplyHandler(success: Boolean; error: Pointer);
  protected
    procedure Cancel; override;
    function CanVerify: Boolean; override;
    function  GetBiometricCapability: TBiometricCapabilityResult; override;
    function HasUserInterface: Boolean; override;
    function IsBiometryLockedOut: Boolean; override;
    procedure Reset; override;
    procedure RestoreBiometry(const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod); override;
    procedure SetReuseTime(const AInterval: Double); override;
    procedure Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod); override;
  public
    class function GetBiometryKind: TBiometryKind; override;
    class function IsSupported: Boolean; override;
  public
    constructor Create(const ABiometric: TBiometric); override;
    destructor Destroy; override;
  end;

implementation

uses
  // RTL
  System.Classes,
  // DW
  DW.Macapi.Helpers;

const
  libLocalAuthentication = '/System/Library/Frameworks/LocalAuthentication.framework/LocalAuthentication';

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

function LATouchIDAuthenticationMaximumAllowableReuseDuration: Double;
begin
  Result := CocoaDoubleConst(libLocalAuthentication, 'LATouchIDAuthenticationMaximumAllowableReuseDuration');
end;

{ TPlatformBiometric }

constructor TPlatformBiometric.Create(const ABiometric: TBiometric);
begin
  inherited;
  CreateContext;
end;

destructor TPlatformBiometric.Destroy;
begin
  ReleaseContext;
  inherited;
end;

procedure TPlatformBiometric.CreateContext;
begin
  FContext := TLAContext.Create;
end;

procedure TPlatformBiometric.ReleaseContext;
begin
  if FContext <> nil then
    FContext.release;
  FContext := nil;
end;

procedure TPlatformBiometric.Reset;
begin
  ReleaseContext;
  CreateContext;
end;

procedure TPlatformBiometric.DoRestoreBiometryFailResult(const AResult: TBiometricFailResult; const AResultMessage: string);
begin
  if Assigned(FRestoreBiometryFailResultMethod) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        FRestoreBiometryFailResultMethod(AResult, AResultMessage);
      end
    );
  end;
end;

procedure TPlatformBiometric.DoRestoreBiometrySuccessResult;
begin
  if Assigned(FRestoreBiometrySuccessResultMethod) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        FRestoreBiometrySuccessResultMethod;
      end
    );
  end;
end;

procedure TPlatformBiometric.DoVerifyFailResult(const AResult: TBiometricFailResult; const AResultMessage: string);
begin
  if Assigned(FVerifyFailResultMethod) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        FVerifyFailResultMethod(AResult, AResultMessage);
      end
    );
  end;
end;

procedure TPlatformBiometric.DoVerifySuccessResult;
begin
  if Assigned(FVerifySuccessResultMethod) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        FVerifySuccessResultMethod;
      end
    );
  end;
end;

function TPlatformBiometric.GetBiometricCapability: TBiometricCapabilityResult;
begin
  Result := TBiometricCapabilityResult.Unknown;
end;

class function TPlatformBiometric.GetBiometryKind: TBiometryKind;
var
  LContext: LAContext;
begin
  Result := TBiometryKind.None;
  LContext := TLAContext.Create;
  if LContext.canEvaluatePolicy(cBiometryMode, nil) then
  begin
    case LContext.biometryType of
      LABiometryTypeFaceID:
        Result := TBiometryKind.Face;
      LABiometryTypeTouchID:
        Result := TBiometryKind.Touch;
    end;
  end;
end;

function TPlatformBiometric.HasUserInterface: Boolean;
begin
  Result := True;
end;

procedure TPlatformBiometric.ProcessFailResult(error: Pointer; var AResult: TBiometricFailResult; var AMessage: string);
var
  LCode: Int64;
begin
  AMessage := '';
  AResult := TBiometricFailResult.Unknown;
  LCode := TNSError.Wrap(error).code;
  case LCode of
    // User resolvable conditions (possibly)
    LAErrorTouchIDNotEnrolled, LAErrorTouchIDLockout, LAErrorPasscodeNotSet, LAErrorAuthenticationFailed:
    begin
      AResult := TBiometricFailResult.Denied;
    end;
    LAErrorUserCancel:
      AResult := TBiometricFailResult.Cancelled;
    LAErrorUserFallback:
      AResult := TBiometricFailResult.Fallback;
    LAErrorInvalidContext:
    begin
      AResult := TBiometricFailResult.Error;
      AMessage := Format(SBiometricErrorSystemError, ['Invalid Context']);
    end;
    LAErrorSystemCancel:
    begin
      AResult := TBiometricFailResult.Error;
      AMessage := Format(SBiometricErrorSystemError, ['Cancelled by system']);
    end;
    LAErrorTouchIDNotAvailable:
    begin
      // How could it come this far anyway?
      AResult := TBiometricFailResult.Error;
      AMessage := SBiometricErrorNotAvailable;
    end;
  end;
end;

procedure TPlatformBiometric.VerifyReplyHandler(success: Boolean; error: Pointer);
var
  LResult: TBiometricFailResult;
  LMessage: string;
begin
  if not success then
  begin
    ProcessFailResult(error, LResult, LMessage);
    DoVerifyFailResult(LResult, LMessage);
  end
  else
    DoVerifySuccessResult;
end;

procedure TPlatformBiometric.Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
begin
  FVerifySuccessResultMethod := ASuccessResultMethod;
  FVerifyFailResultMethod := AFailResultMethod;
  if CanVerify then
    FContext.evaluatePolicy(cBiometryMode, StrToNSStr(AMessage), VerifyReplyHandler)
  else
    DoVerifyFailResult(TBiometricFailResult.Error, SBiometricErrorCannotVerify);
end;

procedure TPlatformBiometric.Cancel;
begin
  if TOSVersion.Check(9) then
    FContext.invalidate;
end;

function TPlatformBiometric.CanVerify: Boolean;
begin
  Result := FContext.canEvaluatePolicy(cBiometryMode, nil);
end;

function TPlatformBiometric.IsBiometryLockedOut: Boolean;
var
  LError: NSError;
begin
  Result := False;
  LError := CheckBiometry;
  if LError <> nil then
    Result := LError.code = LAErrorTouchIDLockout;
end;

class function TPlatformBiometric.CheckBiometry: NSError;
var
  LPointer: Pointer;
  LContext: LAContext;
begin
  LContext := TLAContext.Create;
  LContext.canEvaluatePolicy(cBiometryMode, @LPointer);
  Result := TNSError.Wrap(LPointer);
end;

class function TPlatformBiometric.IsSupported: Boolean;
var
  LError: NSError;
begin
  Result := False;
  if TOSVersion.Check(8) then
  begin
    LError := CheckBiometry;
    Result := (LError = nil) or (LError.code = 0);
  end;
end;

procedure TPlatformBiometric.RestoreBiometry(const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
begin
  FRestoreBiometryFailResultMethod := AFailResultMethod;
  FRestoreBiometrySuccessResultMethod := ASuccessResultMethod;
  FContext.evaluatePolicy(cBiometryMode, StrToNSStr(SBiometricEnterPINToRestore), RestoreBiometryReplyHandler);
end;

procedure TPlatformBiometric.RestoreBiometryReplyHandler(success: Boolean; error: Pointer);
var
  LResult: TBiometricFailResult;
  LMessage: string;
begin
  if not success then
  begin
    ProcessFailResult(error, LResult, LMessage);
    DoRestoreBiometryFailResult(LResult, LMessage);
  end
  else
    DoRestoreBiometrySuccessResult;
end;

procedure TPlatformBiometric.SetReuseTime(const AInterval: Double);
begin
  if TOSVersion.Check(9) then
    FContext.setTouchIDAuthenticationAllowableReuseDuration(AInterval);
end;

procedure LocalAuthenticationLoader; cdecl; external libLocalAuthentication;

end.

