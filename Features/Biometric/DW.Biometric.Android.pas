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
  Androidapi.JNI.Java.Security, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  // DW
  DW.Biometric, DW.Androidapi.JNI.Security, DW.Androidapi.JNI.Hardware, DW.Androidapi.JNI.DWFingerprintAuthenticationCallback;

type
  TPlatformBiometric = class;

  /// <summary>
  ///   This class is how the authentication events are implemented on the Delphi side
  ///   Refer to DWFingerprintAuthenticationCallback.java
  /// <summary>
  TFingerprintAuthenticationCallbackDelegate = class(TJavaLocal, JDWFingerprintAuthenticationCallbackDelegate)
  private
    FCallback: JFingerprintManager_AuthenticationCallback;
    FPlatformBiometric: TPlatformBiometric;
  public
    { JDWFingerprintAuthenticationCallbackDelegate }
    procedure onAuthenticationError(errMsgId: Integer; errString: JCharSequence); cdecl;
    procedure onAuthenticationFailed; cdecl;
    procedure onAuthenticationHelp(helpMsgId: Integer; helpString: JCharSequence); cdecl;
    procedure onAuthenticationSucceeded(result: JFingerprintManager_AuthenticationResult); cdecl;
  public
    constructor Create(const APlatformBiometric: TPlatformBiometric);
    property Callback: JFingerprintManager_AuthenticationCallback read FCallback;
  end;

  /// <summary>
  ///   The Android implementation of Biometric
  /// <summary>
  TPlatformBiometric = class(TCustomPlatformBiometric)
  strict private
    FAuthenticationCallbackDelegate: TFingerprintAuthenticationCallbackDelegate;
    FCancelSignal: JCancellationSignal;
    FFailResultMethod: TBiometricFailResultMethod;
    FFingerprintManager: JFingerprintManager;
    FIsKeyGenerated: Boolean;
    FIsBiometryLockedOut: Boolean;
    FJKeyName: JString;
    FKeyProviderName: JString;
    FKeyStore: JKeyStore;
    FKeyName: string;
    FSuccessResultMethod: TProc;
    procedure DoGenerateKey;
    function GenerateKey: Boolean;
    function GetCipher: JCipher;
    function HasFingerprintPermission: Boolean;
  protected
    procedure DoFailResult(const AFailResult: TBiometricFailResult; const AResultMessage: string);
    procedure DoSuccessResult;
    procedure SetLockedOut(const AValue: Boolean);
  public
    class function GetBiometryKind: TBiometryKind; override;
    class function IsSupported: Boolean; override;
  public
    constructor Create(const ABiometric: TBiometric); override;
    destructor Destroy; override;
    procedure Cancel; override;
    function CanVerify: Boolean; override;
    function GetKeyName: string; override;
    function HasUserInterface: Boolean; override;
    function IsBiometryLockedOut: Boolean; override;
    procedure SetKeyName(const AValue: string); override;
    procedure Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod); override;
  end;

implementation

uses
  // RTL
  System.Classes, System.Permissions,
  // Android
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers,
  // DW
  DW.Consts.Android;

{ TFingerprintAuthenticationCallbackDelegate }

constructor TFingerprintAuthenticationCallbackDelegate.Create(const APlatformBiometric: TPlatformBiometric);
begin
  inherited Create;
  FPlatformBiometric := APlatformBiometric;
  FCallback := TJDWFingerprintAuthenticationCallback.JavaClass.init(Self);
end;

procedure TFingerprintAuthenticationCallbackDelegate.onAuthenticationError(errMsgId: Integer; errString: JCharSequence);
var
  LMessage: string;
begin
  if errMsgId = TJFingerprintManager.JavaClass.FINGERPRINT_ERROR_CANCELED then
    FPlatformBiometric.DoFailResult(TBiometricFailResult.Cancelled, '')
  else if errMsgId = TJFingerprintManager.JavaClass.FINGERPRINT_ERROR_LOCKOUT then
  begin
    FPlatformBiometric.SetLockedOut(True);
    FPlatformBiometric.DoFailResult(TBiometricFailResult.LockedOut, '');
  end
  else
  begin
    LMessage := Format('%d : %s', [errMsgId, JCharSequenceToStr(errString)]);
    FPlatformBiometric.DoFailResult(TBiometricFailResult.Error, LMessage);
  end;
end;

procedure TFingerprintAuthenticationCallbackDelegate.onAuthenticationFailed;
begin
  // Might a hacker be able to re-direct from here to FPlatformBiometric.DoSuccessResult instead?
  FPlatformBiometric.DoFailResult(TBiometricFailResult.Denied, '');
end;

procedure TFingerprintAuthenticationCallbackDelegate.onAuthenticationHelp(helpMsgId: Integer; helpString: JCharSequence);
var
  LMessage: string;
begin
  LMessage := Format('%d : %s', [helpMsgId, JCharSequenceToStr(helpString)]);
  FPlatformBiometric.DoFailResult(TBiometricFailResult.Help, LMessage);
end;

procedure TFingerprintAuthenticationCallbackDelegate.onAuthenticationSucceeded(result: JFingerprintManager_AuthenticationResult);
begin
  FPlatformBiometric.SetLockedOut(False); // Might need to use another API to check this status
  FPlatformBiometric.DoSuccessResult;
end;

{ TPlatformBiometric }

constructor TPlatformBiometric.Create(const ABiometric: TBiometric);
var
  LService: JObject;
begin
  inherited;
  FAuthenticationCallbackDelegate := TFingerprintAuthenticationCallbackDelegate.Create(Self);
  FKeyProviderName := StringToJString('AndroidKeyStore');
  FKeyStore := TJKeyStore.JavaClass.getInstance(FKeyProviderName);
  LService := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.FINGERPRINT_SERVICE);
  FFingerprintManager := TJFingerprintManager.Wrap(TAndroidHelper.JObjectToID(LService));
end;

destructor TPlatformBiometric.Destroy;
begin
  FAuthenticationCallbackDelegate.Free;
  inherited;
end;

procedure TPlatformBiometric.DoFailResult(const AFailResult: TBiometricFailResult; const AResultMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FFailResultMethod) then
        FFailResultMethod(AFailResult, AResultMessage);
    end
  );
end;

procedure TPlatformBiometric.DoSuccessResult;
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FSuccessResultMethod) then
        FSuccessResultMethod;
    end
  );
end;

function TPlatformBiometric.HasFingerprintPermission: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(cPermissionUseFingerprint);
end;

function TPlatformBiometric.HasUserInterface: Boolean;
begin
  Result := False;
end;

function TPlatformBiometric.IsBiometryLockedOut: Boolean;
begin
  Result := FIsBiometryLockedOut;
end;

class function TPlatformBiometric.IsSupported: Boolean;
begin
  Result := TOSVersion.Check(6);
end;

class function TPlatformBiometric.GetBiometryKind: TBiometryKind;
begin
  // NOTE: This implementation supports only Touch on Android. Future versions may support Face
  if IsSupported then
    Result := TBiometryKind.Touch
  else
    Result := TBiometryKind.None;
end;

function TPlatformBiometric.GetKeyName: string;
begin
  Result := FKeyName;
end;

procedure TPlatformBiometric.SetKeyName(const AValue: string);
begin
  FKeyName := AValue;
end;

procedure TPlatformBiometric.SetLockedOut(const AValue: Boolean);
begin
  FIsBiometryLockedOut := AValue;
end;

procedure TPlatformBiometric.DoGenerateKey;
var
  LKeyGenerator: JKeyGenerator;
  LKeyGenParameterSpecBuilder: JKeyGenParameterSpec_Builder;
  LKeyGenParameterSpec: JKeyGenParameterSpec;
  LBlockModes, LPaddings: TJavaObjectArray<JString>;
begin
  LBlockModes := TJavaObjectArray<JString>.Create(1);
  LBlockModes.Items[0] := TJKeyProperties.JavaClass.BLOCK_MODE_CBC;
  LPaddings := TJavaObjectArray<JString>.Create(1);
  LPaddings.Items[0] := TJKeyProperties.JavaClass.ENCRYPTION_PADDING_PKCS7;
  LKeyGenerator := TJKeyGenerator.JavaClass.getInstance(TJKeyProperties.JavaClass.KEY_ALGORITHM_AES, FKeyProviderName);
  LKeyGenParameterSpecBuilder := TJKeyGenParameterSpec_Builder.JavaClass.init(FJKeyName, TJKeyProperties.JavaClass.PURPOSE_ENCRYPT or
    TJKeyProperties.JavaClass.PURPOSE_DECRYPT);
  LKeyGenParameterSpec := LKeyGenParameterSpecBuilder
    .setBlockModes(LBlockModes)
    .setUserAuthenticationRequired(True)
    .setEncryptionPaddings(LPaddings)
    .build;
  LKeyGenerator.init(LKeyGenParameterSpec);
  LKeyGenerator.generateKey;
end;

function TPlatformBiometric.GenerateKey: Boolean;
begin
  if not FIsKeyGenerated then
  try
    DoGenerateKey;
    FIsKeyGenerated := True;
  except
    FIsKeyGenerated := False;
  end;
  Result := FIsKeyGenerated;
end;

function TPlatformBiometric.GetCipher: JCipher;
var
  LTransform, LSep: JString;
  LCipher: JCipher;
  LKey: JKey;
begin
  LSep := StringToJString('/');
  LTransform := TJKeyProperties.JavaClass.KEY_ALGORITHM_AES;
  LTransform := LTransform.concat(LSep.concat(TJKeyProperties.JavaClass.BLOCK_MODE_CBC));
  LTransform := LTransform.concat(LSep.concat(TJKeyProperties.JavaClass.ENCRYPTION_PADDING_PKCS7));
  try
    LCipher := TJCipher.JavaClass.getInstance(LTransform); // NoSuchAlgorithmException | NoSuchPaddingException
    FKeyStore.load(nil);
    LKey := FKeyStore.getKey(FJKeyName, nil);
    LCipher.init(TJCipher.JavaClass.ENCRYPT_MODE, LKey); //
    Result := LCipher;
  except
    // KeyPermanentlyInvalidatedException | KeyStoreException | CertificateException | UnrecoverableKeyException |
    //   IOException | NoSuchAlgorithmException | InvalidKeyException
    Result := nil;
  end;
end;

procedure TPlatformBiometric.Cancel;
begin
  if FCancelSignal <> nil then
  begin
    FCancelSignal.cancel;
    FCancelSignal := nil;
  end;
end;

function TPlatformBiometric.CanVerify: Boolean;
begin
  Result := False;
  if HasFingerprintPermission and FFingerprintManager.hasEnrolledFingerprints then
    Result := True;
end;

procedure TPlatformBiometric.Verify(const AMessage: string; const ASuccessResultMethod: TProc; const AFailResultMethod: TBiometricFailResultMethod);
var
  LCrypto: JFingerprintManager_CryptoObject;
  LCipher: JCipher;
begin
  FSuccessResultMethod := ASuccessResultMethod;
  FFailResultMethod := AFailResultMethod;
  if FKeyName.IsEmpty then
  begin
    DoFailResult(TBiometricFailResult.Error, SBiometricErrorKeyNameEmpty);
    Exit; // <======
  end;
  FJKeyName := StringToJString(FKeyName);
  if not CanVerify then
  begin
    DoFailResult(TBiometricFailResult.Error, SBiometricErrorCannotVerify);
    Exit; // <======
  end;
  if not GenerateKey then
  begin
    DoFailResult(TBiometricFailResult.Error, SBiometricErrorKeyGenerationFailed);
    Exit; // <======
  end;
  LCipher := GetCipher;
  if LCipher = nil then
  begin
    DoFailResult(TBiometricFailResult.Error, SBiometricErrorCipherGenerationFailed);
    Exit; // <======
  end;
  LCrypto := TJFingerprintManager_CryptoObject.JavaClass.init(LCipher);
  FCancelSignal := TJCancellationSignal.JavaClass.init;
  FFingerprintManager.authenticate(LCrypto, FCancelSignal, 0, FAuthenticationCallbackDelegate.Callback, nil);
end;

end.

