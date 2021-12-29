unit DW.Biometric.Android;

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
  // Android
  Androidapi.JNI.JavaTypes, Androidapi.JNIBridge, Androidapi.Jni.GraphicsContentViewText,
  // DW
  DW.Biometric, DW.Androidapi.JNI.AndroidX.Biometric, DW.MultiReceiver.Android;

type
  TPlatformBiometric = class;
  
  TBiometricFragmentActivityReceiver = class(TMultiReceiver)
  private
    FPlatformBiometric: TPlatformBiometric;
  protected
    procedure ConfigureActions; override;
    procedure Receive(context: JContext; intent: JIntent); override;
  public
    constructor Create(const APlatformBiometric: TPlatformBiometric);
  end;

  TPlatformBiometric = class(TCustomPlatformBiometric)
  private
    FActivityReceiver: TBiometricFragmentActivityReceiver;
    FBiometricManager: JBiometricManager;
    FVerifySuccessResultMethod: TProc;
    FVerifyFailResultMethod: TBiometricFailResultMethod;
    function CanAllowDeviceCredential: Boolean;
    function GetPromptInfoIntent: JIntent;
    procedure InitializeStrengths;
  protected
    function  GetBiometricCapability: TBiometricCapabilityResult; override;
    procedure HandleAuthenticationResult(const AIntent: JIntent);
    function  ShowPrompt: Boolean;
    procedure Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod); override;
  public
    procedure Cancel; override;
    function CanVerify: Boolean; override;
    function HasUserInterface: Boolean; override;
    function IsBiometryLockedOut: Boolean; override;
  public
    class function GetBiometryKind: TBiometryKind; override;
    class function IsSupported: Boolean; override;
  public
    constructor Create(const ABiometric: TBiometric); override;
    destructor Destroy; override;
  end;

implementation

uses
  // Android
  Androidapi.Helpers, Androidapi.JNI.Os,
  // DW
  DW.Androidapi.JNI.DWBiometricFragmentActivity;

const
  cAUTHENTICATION_RESULT_SUCCESS = 0;
  cAUTHENTICATION_RESULT_ERROR = 1;
  cAUTHENTICATION_RESULT_FAILED = 2;

{ TBiometricFragmentActivityReceiver }

constructor TBiometricFragmentActivityReceiver.Create(const APlatformBiometric: TPlatformBiometric);
begin
  inherited Create(True);
  FPlatformBiometric := APlatformBiometric;
end;

procedure TBiometricFragmentActivityReceiver.ConfigureActions;
begin
  IntentFilter.addAction(TJDWBiometricFragmentActivity.JavaClass.ACTION_AUTHENTICATION);
end;

procedure TBiometricFragmentActivityReceiver.Receive(context: JContext; intent: JIntent);
begin
  if (intent.getAction <> nil) and intent.getAction.equals(TJDWBiometricFragmentActivity.JavaClass.ACTION_AUTHENTICATION) then
    FPlatformBiometric.HandleAuthenticationResult(intent);
end;

{ TPlatformBiometric }

constructor TPlatformBiometric.Create(const ABiometric: TBiometric);
begin
  inherited;
  InitializeStrengths;
  FBiometricManager := TJBiometricManager.JavaClass.from(TAndroidHelper.Context);
  FActivityReceiver := TBiometricFragmentActivityReceiver.Create(Self);
end;

destructor TPlatformBiometric.Destroy;
begin
  FActivityReceiver.Free;
  inherited;
end;

procedure TPlatformBiometric.Cancel;
begin
  //
end;

function TPlatformBiometric.CanVerify: Boolean;
begin
  Result:=(GetBiometricCapability = TBiometricCapabilityResult.Available);
end;

function TPlatformBiometric.CanAllowDeviceCredential: Boolean;
var
  LSDK: Integer;
  LDeviceCredentialOnly: Boolean;
begin
  // https://developer.android.com/reference/androidx/biometric/BiometricManager?hl=en#canAuthenticate(int)
  LDeviceCredentialOnly := BiometricStrengths = [TBiometricStrength.DeviceCredential];
  LSDK := TJBuild_VERSION.JavaClass.SDK_INT;
  if LSDK in [28, 29] then
    // Cannot combine Strong and DeviceCredential, or have DeviceCredential only in API 28, 29
    Result := not LDeviceCredentialOnly and (BiometricStrengths <> [TBiometricStrength.DeviceCredential, TBiometricStrength.Strong])
  else if LSDK < 28 then
    // Cannot have DeviceCredential on its own in API < 30
    Result := not LDeviceCredentialOnly
  else
    // API >= 30 can have DeviceCredential in any circumstances
    Result := TBiometricStrength.DeviceCredential in BiometricStrengths;
end;

function TPlatformBiometric.GetBiometricCapability: TBiometricCapabilityResult;
var
  LAuthResult: Integer;
begin
  LAuthResult := FBiometricManager.canAuthenticate;
  if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_SUCCESS then
    Result := TBiometricCapabilityResult.Available
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_HW_UNAVAILABLE then
    Result := TBiometricCapabilityResult.HardwareUnavailable
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_NONE_ENROLLED then
    Result := TBiometricCapabilityResult.NoneEnrolled
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_NO_HARDWARE then
    Result := TBiometricCapabilityResult.NoHardware
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_SECURITY_UPDATE_REQUIRED then
    Result := TBiometricCapabilityResult.SecurityUpdateRequired
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_UNSUPPORTED then
    Result := TBiometricCapabilityResult.Unsupported
  else
    Result := TBiometricCapabilityResult.Unknown;
end;

class function TPlatformBiometric.GetBiometryKind: TBiometryKind;
begin
  if IsSupported then
    Result := TBiometryKind.Touch
  else
    Result := TBiometryKind.None;
end;

(*
// 1.1.0
function TPlatformBiometric.GetBiometricCapability: TBiometricCapabilityResult;
var
  LAuthenticators, LSDK, LAuthResult: Integer;
begin
  LAuthenticators := 0;
  if CanAllowDeviceCredential then
    LAuthenticators := LAuthenticators or TJBiometricManager_Authenticators.JavaClass.DEVICE_CREDENTIAL;
  if TBiometricStrength.Weak in BiometricStrengths then
    LAuthenticators := LAuthenticators or TJBiometricManager_Authenticators.JavaClass.BIOMETRIC_WEAK;
  if TBiometricStrength.Strong in BiometricStrengths then
    LAuthenticators := LAuthenticators or TJBiometricManager_Authenticators.JavaClass.BIOMETRIC_STRONG;
  LAuthResult := FBiometricManager.canAuthenticate(LAuthenticators);
  if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_SUCCESS then
    Result := TBiometricCapabilityResult.Available
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_HW_UNAVAILABLE then
    Result := TBiometricCapabilityResult.HardwareUnavailable
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_NONE_ENROLLED then
    Result := TBiometricCapabilityResult.NoneEnrolled
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_NO_HARDWARE then
    Result := TBiometricCapabilityResult.NoHardware
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_SECURITY_UPDATE_REQUIRED then
    Result := TBiometricCapabilityResult.SecurityUpdateRequired
  else if LAuthResult = TJBiometricManager.JavaClass.BIOMETRIC_ERROR_UNSUPPORTED then
    Result := TBiometricCapabilityResult.Unsupported
  else
    Result := TBiometricCapabilityResult.Unknown;
end;
*)

function TPlatformBiometric.GetPromptInfoIntent: JIntent;
begin
  Result := TJIntent.JavaClass.init;
  Result.putExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_PROMPT_DESCRIPTION, StrToJCharSequence(PromptDescription));
  Result.putExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_PROMPT_SUBTITLE, StrToJCharSequence(PromptSubtitle));
  Result.putExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_PROMPT_TITLE, StrToJCharSequence(PromptTitle));
  Result.putExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_PROMPT_ALLOW_DEVICE_CREDENTIAL, CanAllowDeviceCredential);
  Result.putExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_PROMPT_CANCEL_BUTTON_TEXT, StrToJCharSequence(PromptCancelButtonText));
  Result.putExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_PROMPT_CONFIRMATION_REQUIRED, PromptConfirmationRequired);
end;

procedure TPlatformBiometric.InitializeStrengths;
var
  LSDK: Integer;
  LStrengths: TBiometricStrengths;
begin
  LStrengths := [TBiometricStrength.Strong];
  LSDK := TJBuild_VERSION.JavaClass.SDK_INT;
  if LSDK >= 30 then
    Include(LStrengths, TBiometricStrength.DeviceCredential);
  BiometricStrengths := LStrengths;
end;

function TPlatformBiometric.IsBiometryLockedOut: Boolean;
begin
  Result:=False; //TODO: Check is Android biometry can be locked out or not
end;

class function TPlatformBiometric.IsSupported: Boolean;
begin
  Result := TOSVersion.Check(6);
end;

function TPlatformBiometric.ShowPrompt: Boolean;
begin
  Result := False;
  if GetBiometricCapability = TBiometricCapabilityResult.Available then
  begin
    TJDWBiometricFragmentActivity.JavaClass.start(TAndroidHelper.Context, GetPromptInfoIntent);
    Result := True;
  end;
end;

procedure TPlatformBiometric.Verify(const AMessage: string; const ASuccessResultMethod: TProc;
  const AFailResultMethod: TBiometricFailResultMethod);
begin
  FVerifySuccessResultMethod := ASuccessResultMethod;
  FVerifyFailResultMethod := AFailResultMethod;
  PromptDescription := AMessage;
  if not ShowPrompt and Assigned(FVerifyFailResultMethod) then
    FVerifyFailResultMethod(TBiometricFailResult.Error, 'Biometry not available');
end;

procedure TPlatformBiometric.HandleAuthenticationResult(const AIntent: JIntent);
var
  LAuthRes, LErrorCode: Integer;
  LErrorMsg: String;
begin
  if AIntent.hasExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_AUTHENTICATION_RESULT) then
  begin
    LAuthRes := AIntent.getIntExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_AUTHENTICATION_RESULT, 2);
    if LAuthRes = cAUTHENTICATION_RESULT_SUCCESS then
    begin
      if Assigned(FVerifySuccessResultMethod) then
        FVerifySuccessResultMethod;
    end
    // Called when a biometric (e.g. fingerprint, face, etc.) is presented but not recognized as belonging to the user.
    // No error code or message will be returned but the UI will display the failure reason anyway.
    else if LAuthRes = cAUTHENTICATION_RESULT_ERROR then
    begin
      // See https://developer.android.com/reference/androidx/biometric/BiometricPrompt for error codes
      LErrorCode := AIntent.getIntExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_AUTHENTICATION_ERROR_CODE, 0);
      LErrorMsg := JStringToString(AIntent.getStringExtra(TJDWBiometricFragmentActivity.JavaClass.EXTRA_AUTHENTICATION_ERROR_MESSAGE));

      if Assigned(FVerifyFailResultMethod) then
        FVerifyFailResultMethod(TBiometricFailResult.Error, Format('Code: %d, Message: %s', [LErrorCode, LErrorMsg]));
    end
    // Failed will happen if auth is working but the user couldn't be autheticated.
    // E.g. couldn't match the finger or face. The UI will tell the user what's wrong
    // so the error message and code will always be empty.
    else if LAuthRes = cAUTHENTICATION_RESULT_FAILED then
    begin
      if Assigned(FVerifyFailResultMethod) then
        FVerifyFailResultMethod(TBiometricFailResult.Denied, '');
    end;
  end;
end;

function TPlatformBiometric.HasUserInterface: Boolean;
begin
  Result := True;
end;

end.
