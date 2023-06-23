unit DW.Androidapi.JNI.AndroidX.Biometric;

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
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.JavaTypes, Androidapi.JNI.Location, Androidapi.JNI.Media,
  Androidapi.JNI.Net, Androidapi.JNI.Os, Androidapi.JNI.Util, Androidapi.JNI.Widget, Androidapi.JNI.Java.Security,
  // DW
  DW.Androidapi.JNI.Security, DW.Androidapi.JNI.AndroidX.Activity;

type
  JBiometricManager = interface;
  JBiometricManager_Authenticators = interface;
  JBiometricPrompt = interface;
  JBiometricPrompt_AuthenticationCallback = interface;
  JBiometricPrompt_AuthenticationResult = interface;
  JBiometricPrompt_CryptoObject = interface;
  JBiometricPrompt_PromptInfo = interface;
  JPromptInfo_Builder = interface;

  JBiometricManagerClass = interface(JObjectClass)
    ['{ABF581DF-72A6-4158-B3DE-6366F756A5FE}']
    {class} function _GetBIOMETRIC_ERROR_HW_UNAVAILABLE: Integer; cdecl;
    {class} function _GetBIOMETRIC_ERROR_NONE_ENROLLED: Integer; cdecl;
    {class} function _GetBIOMETRIC_ERROR_NO_HARDWARE: Integer; cdecl;
    {class} function _GetBIOMETRIC_ERROR_SECURITY_UPDATE_REQUIRED: Integer; cdecl;
    {class} function _GetBIOMETRIC_ERROR_UNSUPPORTED: Integer; cdecl;
    {class} function _GetBIOMETRIC_STATUS_UNKNOWN: Integer; cdecl;
    {class} function _GetBIOMETRIC_SUCCESS: Integer; cdecl;
    {class} function from(context: JContext): JBiometricManager; cdecl;
    {class} property BIOMETRIC_ERROR_HW_UNAVAILABLE: Integer read _GetBIOMETRIC_ERROR_HW_UNAVAILABLE;
    {class} property BIOMETRIC_ERROR_NONE_ENROLLED: Integer read _GetBIOMETRIC_ERROR_NONE_ENROLLED;
    {class} property BIOMETRIC_ERROR_NO_HARDWARE: Integer read _GetBIOMETRIC_ERROR_NO_HARDWARE;
    {class} property BIOMETRIC_ERROR_SECURITY_UPDATE_REQUIRED: Integer read _GetBIOMETRIC_ERROR_SECURITY_UPDATE_REQUIRED;
    {class} property BIOMETRIC_ERROR_UNSUPPORTED: Integer read _GetBIOMETRIC_ERROR_UNSUPPORTED;
    {class} property BIOMETRIC_STATUS_UNKNOWN: Integer read _GetBIOMETRIC_STATUS_UNKNOWN;
    {class} property BIOMETRIC_SUCCESS: Integer read _GetBIOMETRIC_SUCCESS;
  end;

  [JavaSignature('androidx/biometric/BiometricManager')]
  JBiometricManager = interface(JObject)
    ['{EC8C4516-BE4E-4775-A725-D4F1122F953B}']
    function canAuthenticate: Integer; cdecl; overload;
    function canAuthenticate(authenticators: Integer): Integer; cdecl; overload;
  end;
  TJBiometricManager = class(TJavaGenericImport<JBiometricManagerClass, JBiometricManager>) end;

  JBiometricManager_AuthenticatorsClass = interface(IJavaClass)
    ['{6C3FA5BC-BA07-493A-993F-C962EEEA38AE}']
    {class} function _GetBIOMETRIC_STRONG: Integer; cdecl;
    {class} function _GetBIOMETRIC_WEAK: Integer; cdecl;
    {class} function _GetDEVICE_CREDENTIAL: Integer; cdecl;
    {class} property BIOMETRIC_STRONG: Integer read _GetBIOMETRIC_STRONG;
    {class} property BIOMETRIC_WEAK: Integer read _GetBIOMETRIC_WEAK;
    {class} property DEVICE_CREDENTIAL: Integer read _GetDEVICE_CREDENTIAL;
  end;

  [JavaSignature('androidx/biometric/BiometricManager$Authenticators')]
  JBiometricManager_Authenticators = interface(IJavaInstance)
    ['{2747CDC7-E0CA-44B8-809B-F87548C720F0}']
  end;
  TJBiometricManager_Authenticators = class(TJavaGenericImport<JBiometricManager_AuthenticatorsClass, JBiometricManager_Authenticators>) end;

  JBiometricPromptClass = interface(JObjectClass)
    ['{E00B1BB8-DEC8-4291-81D1-07363D67DCEF}']
    {class} function _GetAUTHENTICATION_RESULT_TYPE_BIOMETRIC: Integer; cdecl;
    {class} function _GetAUTHENTICATION_RESULT_TYPE_DEVICE_CREDENTIAL: Integer; cdecl;
    {class} function _GetAUTHENTICATION_RESULT_TYPE_UNKNOWN: Integer; cdecl;
    {class} function _GetBIOMETRIC_SUCCESS: Integer; cdecl;
    {class} function _GetERROR_CANCELED: Integer; cdecl;
    {class} function _GetERROR_HW_NOT_PRESENT: Integer; cdecl;
    {class} function _GetERROR_HW_UNAVAILABLE: Integer; cdecl;
    {class} function _GetERROR_LOCKOUT: Integer; cdecl;
    {class} function _GetERROR_LOCKOUT_PERMANENT: Integer; cdecl;
    {class} function _GetERROR_NEGATIVE_BUTTON: Integer; cdecl;
    {class} function _GetERROR_NO_BIOMETRICS: Integer; cdecl;
    {class} function _GetERROR_NO_DEVICE_CREDENTIAL: Integer; cdecl;
    {class} function _GetERROR_NO_SPACE: Integer; cdecl;
    {class} function _GetERROR_SECURITY_UPDATE_REQUIRED: Integer; cdecl;
    {class} function _GetERROR_TIMEOUT: Integer; cdecl;
    {class} function _GetERROR_UNABLE_TO_PROCESS: Integer; cdecl;
    {class} function _GetERROR_USER_CANCELED: Integer; cdecl;
    {class} function _GetERROR_VENDOR: Integer; cdecl;
    // {class} function init(fragment: Jfragment_app_Fragment; authenticationCallback: JBiometricPrompt_AuthenticationCallback): JBiometricPrompt; cdecl; overload;
    {class} function init(fragmentActivity: JFragmentActivity; authenticationCallback: JBiometricPrompt_AuthenticationCallback): JBiometricPrompt; cdecl; overload;
    // {class} function init(fragment: Jfragment_app_Fragment; executor: JExecutor; authenticationCallback: JBiometricPrompt_AuthenticationCallback): JBiometricPrompt; cdecl; overload;
    {class} function init(fragmentActivity: JFragmentActivity; executor: JExecutor; authenticationCallback: JBiometricPrompt_AuthenticationCallback): JBiometricPrompt; cdecl; overload;
    {class} property AUTHENTICATION_RESULT_TYPE_BIOMETRIC: Integer read _GetAUTHENTICATION_RESULT_TYPE_BIOMETRIC;
    {class} property AUTHENTICATION_RESULT_TYPE_DEVICE_CREDENTIAL: Integer read _GetAUTHENTICATION_RESULT_TYPE_DEVICE_CREDENTIAL;
    {class} property AUTHENTICATION_RESULT_TYPE_UNKNOWN: Integer read _GetAUTHENTICATION_RESULT_TYPE_UNKNOWN;
    {class} property BIOMETRIC_SUCCESS: Integer read _GetBIOMETRIC_SUCCESS;
    {class} property ERROR_CANCELED: Integer read _GetERROR_CANCELED;
    {class} property ERROR_HW_NOT_PRESENT: Integer read _GetERROR_HW_NOT_PRESENT;
    {class} property ERROR_HW_UNAVAILABLE: Integer read _GetERROR_HW_UNAVAILABLE;
    {class} property ERROR_LOCKOUT: Integer read _GetERROR_LOCKOUT;
    {class} property ERROR_LOCKOUT_PERMANENT: Integer read _GetERROR_LOCKOUT_PERMANENT;
    {class} property ERROR_NEGATIVE_BUTTON: Integer read _GetERROR_NEGATIVE_BUTTON;
    {class} property ERROR_NO_BIOMETRICS: Integer read _GetERROR_NO_BIOMETRICS;
    {class} property ERROR_NO_DEVICE_CREDENTIAL: Integer read _GetERROR_NO_DEVICE_CREDENTIAL;
    {class} property ERROR_NO_SPACE: Integer read _GetERROR_NO_SPACE;
    {class} property ERROR_SECURITY_UPDATE_REQUIRED: Integer read _GetERROR_SECURITY_UPDATE_REQUIRED;
    {class} property ERROR_TIMEOUT: Integer read _GetERROR_TIMEOUT;
    {class} property ERROR_UNABLE_TO_PROCESS: Integer read _GetERROR_UNABLE_TO_PROCESS;
    {class} property ERROR_USER_CANCELED: Integer read _GetERROR_USER_CANCELED;
    {class} property ERROR_VENDOR: Integer read _GetERROR_VENDOR;
  end;

  [JavaSignature('androidx/biometric/BiometricPrompt')]
  JBiometricPrompt = interface(JObject)
    ['{357ECAF2-AC33-4C3B-92B8-FE22810668F5}']
    procedure authenticate(promptInfo: JBiometricPrompt_PromptInfo); cdecl; overload;
    procedure authenticate(promptInfo: JBiometricPrompt_PromptInfo; cryptoObject: JBiometricPrompt_CryptoObject); cdecl; overload;
    procedure cancelAuthentication; cdecl;
  end;
  TJBiometricPrompt = class(TJavaGenericImport<JBiometricPromptClass, JBiometricPrompt>) end;

  JBiometricPrompt_PromptInfoClass = interface(JObjectClass)
    ['{6A4AF01B-DACC-4EE7-8176-55B4ABFCB018}']
  end;

  [JavaSignature('androidx/biometric/BiometricPrompt$PromptInfo')]
  JBiometricPrompt_PromptInfo = interface(JObject)
    ['{4053965E-3DB7-4B76-BBB5-4FE868B244C8}']
    function getAllowedAuthenticators: Integer; cdecl;
    function getDescription: JCharSequence; cdecl;
    function getNegativeButtonText: JCharSequence; cdecl;
    function getSubtitle: JCharSequence; cdecl;
    function getTitle: JCharSequence; cdecl;
    function isConfirmationRequired: Boolean; cdecl;
    function isDeviceCredentialAllowed: Boolean; cdecl;
  end;
  TJBiometricPrompt_PromptInfo = class(TJavaGenericImport<JBiometricPrompt_PromptInfoClass, JBiometricPrompt_PromptInfo>) end;

  JPromptInfo_BuilderClass = interface(JObjectClass)
    ['{AF4576F9-2CBF-4576-BA0D-FDE5184F450D}']
    {class} function init: JPromptInfo_Builder; cdecl;
  end;

  [JavaSignature('androidx/biometric/BiometricPrompt$PromptInfo$Builder')]
  JPromptInfo_Builder = interface(JObject)
    ['{AA76607B-6ED4-4F41-93F4-7209ED661487}']
    function build: JBiometricPrompt_PromptInfo; cdecl;
    function setAllowedAuthenticators(i: Integer): JPromptInfo_Builder; cdecl;
    function setConfirmationRequired(b: Boolean): JPromptInfo_Builder; cdecl;
    function setDescription(charSequence: JCharSequence): JPromptInfo_Builder; cdecl;
    function setDeviceCredentialAllowed(b: Boolean): JPromptInfo_Builder; cdecl;
    function setNegativeButtonText(charSequence: JCharSequence): JPromptInfo_Builder; cdecl;
    function setSubtitle(charSequence: JCharSequence): JPromptInfo_Builder; cdecl;
    function setTitle(charSequence: JCharSequence): JPromptInfo_Builder; cdecl;
  end;
  TJPromptInfo_Builder = class(TJavaGenericImport<JPromptInfo_BuilderClass, JPromptInfo_Builder>) end;

  JBiometricPrompt_AuthenticationCallbackClass = interface(JObjectClass)
    ['{F8C9A096-7AC1-43DD-9999-938BF488BF2A}']
    {class} function init: JBiometricPrompt_AuthenticationCallback; cdecl;
  end;

  [JavaSignature('androidx/biometric/BiometricPrompt$AuthenticationCallback')]
  JBiometricPrompt_AuthenticationCallback = interface(JObject)
    ['{5DEADC58-03A1-4CE8-9708-662FA20B076E}']
    procedure onAuthenticationError(errorCode: Integer; errString: JCharSequence); cdecl;
    procedure onAuthenticationFailed; cdecl;
    procedure onAuthenticationSucceeded(result: JBiometricPrompt_AuthenticationResult); cdecl;
  end;
  TJBiometricPrompt_AuthenticationCallback = class(TJavaGenericImport<JBiometricPrompt_AuthenticationCallbackClass,
    JBiometricPrompt_AuthenticationCallback>) end;

  JBiometricPrompt_AuthenticationResultClass = interface(JObjectClass)
    ['{3C41B4A1-4715-4728-B959-B65FB1408BF9}']
  end;

  [JavaSignature('androidx/biometric/BiometricPrompt$AuthenticationResult')]
  JBiometricPrompt_AuthenticationResult = interface(JObject)
    ['{DB6464C3-6AB0-4C9B-9FB6-ED7590616FF2}']
    function getAuthenticationType: Integer; cdecl;
    function getCryptoObject: JBiometricPrompt_CryptoObject; cdecl;
  end;
  TJBiometricPrompt_AuthenticationResult = class(TJavaGenericImport<JBiometricPrompt_AuthenticationResultClass,
    JBiometricPrompt_AuthenticationResult>) end;

  JBiometricPrompt_CryptoObjectClass = interface(JObjectClass)
    ['{CB50A14B-BD7D-43F1-97F7-4702D752E5D3}']
    {class} function init(signature: Jsecurity_Signature): JBiometricPrompt_CryptoObject; cdecl; overload;
    {class} function init(cipher: JCipher): JBiometricPrompt_CryptoObject; cdecl; overload;
    {class} function init(mac: JMac): JBiometricPrompt_CryptoObject; cdecl; overload;
    {class} function init(identityCredential: JIdentityCredential): JBiometricPrompt_CryptoObject; cdecl; overload; // API level 30
  end;

  [JavaSignature('androidx/biometric/BiometricPrompt$CryptoObject')]
  JBiometricPrompt_CryptoObject = interface(JObject)
    ['{E7BDD67C-7E10-4029-A927-197B54280677}']
    function getCipher: JCipher; cdecl;
    function getIdentityCredential: JIdentityCredential; cdecl; // API level 30
    function getMac: JMac; cdecl;
    function getSignature: Jsecurity_Signature; cdecl;
  end;
  TJBiometricPrompt_CryptoObject = class(TJavaGenericImport<JBiometricPrompt_CryptoObjectClass, JBiometricPrompt_CryptoObject>) end;

implementation

end.
