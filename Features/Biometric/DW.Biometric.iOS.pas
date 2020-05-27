unit DW.Biometric.iOS;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{          Delphi Worlds Cross-Platform Library         }
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
  iOSapi.Foundation, iOSapi.CocoaTypes,
  // DW
  DW.Biometric;

type
  LAPolicy = (notUsed = 0, DeviceOwnerAuthenticationWithBiometrics = 1, DeviceOwnerAuthentication = 2);

  LAContextReply = procedure(success: Pointer; error: Pointer) of object;

  LAContextClass = interface(NSObjectClass)
    ['{5ABCEDE7-FC9E-4F97-AACB-C992FA7AEA25}']
  end;
  LAContext = interface(NSObject)
    ['{481C9FA2-FB24-4BE7-98BC-0337ACCC4C5F}']
    function canEvaluatePolicy(policy: LAPolicy; error: PPointer): Boolean; cdecl;
    procedure evaluatePolicy(policy: LAPolicy; localizedReason: NSString; reply: LAContextReply); cdecl;
    procedure invalidate; cdecl;
    procedure setTouchIDAuthenticationAllowableReuseDuration(touchIDAuthenticationAllowableReuseDuration: NSTimeInterval); cdecl;
    function touchIDAuthenticationAllowableReuseDuration: NSTimeInterval; cdecl;
  end;
  TLAContext = class(TOCGenericImport<LAContextClass, LAContext>)
  end;

  TPlatformBiometric = class(TCustomPlatformBiometric)
  private
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
    procedure RestoreBiometryReply(success: Pointer; error: Pointer);
    procedure BiometricReply(success: Pointer; error: Pointer);
  protected
    procedure Cancel; override;
    function CanVerify: Boolean; override;
    function HasUserInterface: Boolean; override;
    function IsBiometryLockedOut: Boolean; override;
    procedure Reset; override;
    procedure RestoreBiometry(const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod); override;
    procedure SetReuseTime(const AInterval: Double); override;
    procedure Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod); override;
  public
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

  LAPolicyDeviceOwnerAuthenticationWithBiometrics = 1;
  LAPolicyDeviceOwnerAuthentication = 2;
  LACredentialTypeApplicationPassword = 0;
  LAAccessControlOperationCreateItem = 0;
  LAAccessControlOperationUseItem = 1;
  LAAccessControlOperationCreateKey = 2;
  LAAccessControlOperationUseKeySign = 3;
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
  if not Assigned(FRestoreBiometryFailResultMethod) then
    Exit; // <======
  TThread.Queue(nil,
    procedure
    begin
      FRestoreBiometryFailResultMethod(AResult, AResultMessage);
    end
  );
end;

procedure TPlatformBiometric.DoRestoreBiometrySuccessResult;
begin
  if not Assigned(FRestoreBiometrySuccessResultMethod) then
    Exit; // <======
  TThread.Queue(nil,
    procedure
    begin
      FRestoreBiometrySuccessResultMethod;
    end
  );
end;

procedure TPlatformBiometric.DoVerifyFailResult(const AResult: TBiometricFailResult; const AResultMessage: string);
begin
  if not Assigned(FVerifyFailResultMethod) then
    Exit; // <======
  TThread.Queue(nil,
    procedure
    begin
      FVerifyFailResultMethod(AResult, AResultMessage);
    end
  );
end;

procedure TPlatformBiometric.DoVerifySuccessResult;
begin
  if not Assigned(FVerifySuccessResultMethod) then
    Exit; // <======
  TThread.Queue(nil,
    procedure
    begin
      FVerifySuccessResultMethod;
    end
  );
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

procedure TPlatformBiometric.BiometricReply(success, error: Pointer);
var
  LResult: TBiometricFailResult;
  LMessage: string;
begin
  if success = nil then
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
    FContext.evaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, StrToNSStr(AMessage), BiometricReply)
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
  Result := FContext.canEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, nil);
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
  LContext.canEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, @LPointer);
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
  FContext.evaluatePolicy(LAPolicy.DeviceOwnerAuthentication, StrToNSStr(SBiometricEnterPINToRestore), RestoreBiometryReply);
end;

procedure TPlatformBiometric.RestoreBiometryReply(success, error: Pointer);
var
  LResult: TBiometricFailResult;
  LMessage: string;
begin
  if success = nil then
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

