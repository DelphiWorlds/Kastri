unit DW.Androidapi.JNI.Security;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2024 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Java.Security;

type
  JCipher = interface;
  JExemptionMechanism = interface;
  JKeyGenerator = interface;
  JKeyGenParameterSpec_Builder = interface;
  JKeyPair = interface;
  JKeyProperties = interface;
  JMac = interface;
  JResultData = interface;
  JSecretKey = interface;

  JCipherClass = interface(JObjectClass)
    ['{F8374D1B-F939-4B8F-A356-FBD254F8ADA4}']
    {class} function _GetDECRYPT_MODE: Integer; cdecl;
    {class} function _GetENCRYPT_MODE: Integer; cdecl;
    {class} function _GetPRIVATE_KEY: Integer; cdecl;
    {class} function _GetPUBLIC_KEY: Integer; cdecl;
    {class} function _GetSECRET_KEY: Integer; cdecl;
    {class} function _GetUNWRAP_MODE: Integer; cdecl;
    {class} function _GetWRAP_MODE: Integer; cdecl;
    {class} function doFinal: TJavaArray<Byte>; cdecl; overload;
    {class} function doFinal(output: TJavaArray<Byte>; outputOffset: Integer): Integer; cdecl; overload;
    {class} function doFinal(input: TJavaArray<Byte>; inputOffset: Integer; inputLen: Integer; output: TJavaArray<Byte>;
      outputOffset: Integer): Integer; cdecl; overload;
    {class} function doFinal(input: JByteBuffer; output: JByteBuffer): Integer; cdecl; overload;
    {class} function getAlgorithm: JString; cdecl;
    {class} function getIV: TJavaArray<Byte>; cdecl;
    {class} function getInstance(transformation: JString): JCipher; cdecl; overload;
    {class} function getInstance(transformation: JString; provider: JString): JCipher; cdecl; overload;
    {class} function getInstance(transformation: JString; provider: JProvider): JCipher; cdecl; overload;
    {class} function getMaxAllowedKeyLength(transformation: JString): Integer; cdecl;
    {class} function getMaxAllowedParameterSpec(transformation: JString): JAlgorithmParameterSpec; cdecl;
    {class} function getOutputSize(inputLen: Integer): Integer; cdecl;
    {class} function getParameters: JAlgorithmParameters; cdecl;
    {class} function getProvider: JProvider; cdecl;
    {class} function update(input: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    {class} function update(input: TJavaArray<Byte>; inputOffset: Integer; inputLen: Integer): TJavaArray<Byte>; cdecl; overload;
    {class} function update(input: TJavaArray<Byte>; inputOffset: Integer; inputLen: Integer; output: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} procedure updateAAD(input: TJavaArray<Byte>; inputOffset: Integer; inputLen: Integer); cdecl; overload;
    {class} procedure updateAAD(input: JByteBuffer); cdecl; overload;
    {class} function wrap(key: JKey): TJavaArray<Byte>; cdecl;
    {class} property DECRYPT_MODE: Integer read _GetDECRYPT_MODE;
    {class} property ENCRYPT_MODE: Integer read _GetENCRYPT_MODE;
    {class} property PRIVATE_KEY: Integer read _GetPRIVATE_KEY;
    {class} property PUBLIC_KEY: Integer read _GetPUBLIC_KEY;
    {class} property SECRET_KEY: Integer read _GetSECRET_KEY;
    {class} property UNWRAP_MODE: Integer read _GetUNWRAP_MODE;
    {class} property WRAP_MODE: Integer read _GetWRAP_MODE;
  end;

  [JavaSignature('javax/crypto/Cipher')]
  JCipher = interface(JObject)
    ['{AA09CFF6-B6F5-4D15-8596-69EE0434C739}']
    function doFinal(input: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    function doFinal(input: TJavaArray<Byte>; inputOffset: Integer; inputLen: Integer): TJavaArray<Byte>; cdecl; overload;
    function doFinal(input: TJavaArray<Byte>; inputOffset: Integer; inputLen: Integer; output: TJavaArray<Byte>): Integer; cdecl; overload;
    function getBlockSize: Integer; cdecl;
    function getExemptionMechanism: JExemptionMechanism; cdecl;
    procedure init(opmode: Integer; key: JKey); cdecl; overload;
    procedure init(opmode: Integer; key: JKey; random: JSecureRandom); cdecl; overload;
    procedure init(opmode: Integer; key: JKey; params: JAlgorithmParameterSpec); cdecl; overload;
    procedure init(opmode: Integer; certificate: JCertificate); cdecl; overload;
    procedure init(opmode: Integer; certificate: JCertificate; random: JSecureRandom); cdecl; overload;
    function unwrap(wrappedKey: TJavaArray<Byte>; wrappedKeyAlgorithm: JString; wrappedKeyType: Integer): JKey; cdecl;
    function update(input: TJavaArray<Byte>; inputOffset: Integer; inputLen: Integer; output: TJavaArray<Byte>;
      outputOffset: Integer): Integer; cdecl; overload;
    function update(input: JByteBuffer; output: JByteBuffer): Integer; cdecl; overload;
    procedure updateAAD(input: TJavaArray<Byte>); cdecl; overload;
  end;
  TJCipher = class(TJavaGenericImport<JCipherClass, JCipher>) end;

  JExemptionMechanismClass = interface(JObjectClass)
    ['{1B017172-84F9-4932-9D7A-49448CFB562F}']
    {class} function genExemptionBlob(output: TJavaArray<Byte>): Integer; cdecl; overload;
    {class} function genExemptionBlob(output: TJavaArray<Byte>; outputOffset: Integer): Integer; cdecl; overload;
    {class} function getInstance(algorithm: JString): JExemptionMechanism; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JString): JExemptionMechanism; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JProvider): JExemptionMechanism; cdecl; overload;
    {class} function getOutputSize(inputLen: Integer): Integer; cdecl;
    {class} function getProvider: JProvider; cdecl;
  end;

  [JavaSignature('javax/crypto/ExemptionMechanism')]
  JExemptionMechanism = interface(JObject)
    ['{16F7DF15-7CBA-4E4C-9003-51D68D12543E}']
    function genExemptionBlob: TJavaArray<Byte>; cdecl; overload;
    function getName: JString; cdecl;
    procedure init(key: JKey; param: JAlgorithmParameters); cdecl; overload;
    procedure init(key: JKey; param: JAlgorithmParameterSpec); cdecl; overload;
    function isCryptoAllowed(key: JKey): Boolean; cdecl;
  end;
  TJExemptionMechanism = class(TJavaGenericImport<JExemptionMechanismClass, JExemptionMechanism>) end;

  JKeyGeneratorClass = interface(JObjectClass)
    ['{36A39592-1D46-4092-993A-EFD454F1C1C7}']
    {class} function getInstance(algorithm: JString): JKeyGenerator; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JString): JKeyGenerator; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JProvider): JKeyGenerator; cdecl; overload;
    {class} function getProvider: JProvider; cdecl;
  end;

  [JavaSignature('javax/crypto/KeyGenerator')]
  JKeyGenerator = interface(JObject)
    ['{1E62C8C9-ECA3-4F67-BD1F-8DD9CAECD2D6}']
    function generateKey: JSecretKey; cdecl;
    function getAlgorithm: JString; cdecl;
    procedure init(params: JAlgorithmParameterSpec); cdecl; overload;
    procedure init(params: JAlgorithmParameterSpec; random: JSecureRandom); cdecl; overload;
  end;
  TJKeyGenerator = class(TJavaGenericImport<JKeyGeneratorClass, JKeyGenerator>) end;

  JKeyGenParameterSpecClass = interface(JAlgorithmParameterSpecClass)
    ['{31DF2C6A-50DC-4124-B934-7D3ABFD3C29E}']
  end;

  [JavaSignature('android/security/keystore/KeyGenParameterSpec')]
  JKeyGenParameterSpec = interface(JAlgorithmParameterSpec)
    ['{5CC805D8-2687-4398-BD4D-D1909A1203F0}']
    function getAlgorithmParameterSpec: JAlgorithmParameterSpec; cdecl;
    function getAttestationChallenge: TJavaArray<Byte>; cdecl;
    function getBlockModes: TJavaObjectArray<JString>; cdecl;
    function getCertificateNotAfter: JDate; cdecl;
    function getCertificateNotBefore: JDate; cdecl;
    function getCertificateSerialNumber: JBigInteger; cdecl;
    function getCertificateSubject: JX500Principal; cdecl;
    function getDigests: TJavaObjectArray<JString>; cdecl;
    function getEncryptionPaddings: TJavaObjectArray<JString>; cdecl;
    function getKeySize: Integer; cdecl;
    function getKeyValidityForConsumptionEnd: JDate; cdecl;
    function getKeyValidityForOriginationEnd: JDate; cdecl;
    function getKeyValidityStart: JDate; cdecl;
    function getKeystoreAlias: JString; cdecl;
    function getPurposes: Integer; cdecl;
    function getSignaturePaddings: TJavaObjectArray<JString>; cdecl;
    function getUserAuthenticationValidityDurationSeconds: Integer; cdecl;
    function isDigestsSpecified: boolean; cdecl;
    function isInvalidatedByBiometricEnrollment: boolean; cdecl;
    function isRandomizedEncryptionRequired: boolean; cdecl;
    function isStrongBoxBacked: boolean; cdecl;
    function isUnlockedDeviceRequired: boolean; cdecl;
    function isUserAuthenticationRequired: boolean; cdecl;
    function isUserAuthenticationValidWhileOnBody: boolean; cdecl;
    function isUserConfirmationRequired: boolean; cdecl;
    function isUserPresenceRequired: boolean; cdecl;
  end;
  TJKeyGenParameterSpec = class(TJavaGenericImport<JKeyGenParameterSpecClass, JKeyGenParameterSpec>)
  end;

  JKeyGenParameterSpec_BuilderClass = interface(JObjectClass)
    ['{2FE0B6DC-E4E8-4AF1-B920-8AD15E2C1EF6}']
    function init(keystoreAlias: JString; purposes: Integer): JKeyGenParameterSpec_Builder; cdecl;
  end;

  [JavaSignature('android/security/keystore/KeyGenParameterSpec$Builder')]
  JKeyGenParameterSpec_Builder = interface(JObject)
    ['{5E75A8C8-D4C0-42B3-94B1-C372EAD135E7}']
    function build: JKeyGenParameterSpec; cdecl;
    function setAlgorithmParameterSpec(spec: JAlgorithmParameterSpec): JKeyGenParameterSpec_Builder; cdecl;
    function setAttestationChallenge(attestationChallenge: TJavaArray<Byte>): JKeyGenParameterSpec_Builder; cdecl;
    function setBlockModes(blockModes: TJavaObjectArray<JString>): JKeyGenParameterSpec_Builder; cdecl;
    function setCertificateNotAfter(date: JDate): JKeyGenParameterSpec_Builder; cdecl;
    function setCertificateNotBefore(date: JDate): JKeyGenParameterSpec_Builder; cdecl;
    function setCertificateSerialNumber(serialNumber: JBigInteger): JKeyGenParameterSpec_Builder; cdecl;
    function setCertificateSubject(subject: JX500Principal): JKeyGenParameterSpec_Builder; cdecl;
    function setDigests(digests: TJavaObjectArray<JString>): JKeyGenParameterSpec_Builder; cdecl;
    function setEncryptionPaddings(paddings: TJavaObjectArray<JString>): JKeyGenParameterSpec_Builder; cdecl;
    function setInvalidatedByBiometricEnrollment(invalidateKey: boolean): JKeyGenParameterSpec_Builder; cdecl;
    function setIsStrongBoxBacked(isStrongBoxBacked: boolean): JKeyGenParameterSpec_Builder; cdecl;
    function setKeySize(keySize: Integer): JKeyGenParameterSpec_Builder; cdecl;
    function setKeyValidityEnd(endDate: JDate): JKeyGenParameterSpec_Builder; cdecl;
    function setKeyValidityForConsumptionEnd(endDate: JDate): JKeyGenParameterSpec_Builder; cdecl;
    function setKeyValidityForOriginationEnd(endDate: JDate): JKeyGenParameterSpec_Builder; cdecl;
    function setKeyValidityStart(startDate: JDate): JKeyGenParameterSpec_Builder; cdecl;
    function setRandomizedEncryptionRequired(required: boolean): JKeyGenParameterSpec_Builder; cdecl;
    function setSignaturePaddings(paddings: TJavaObjectArray<JString>): JKeyGenParameterSpec_Builder; cdecl;
    function setUnlockedDeviceRequired(unlockedDeviceRequired: boolean): JKeyGenParameterSpec_Builder; cdecl;
    function setUserAuthenticationRequired(required: boolean): JKeyGenParameterSpec_Builder; cdecl;
    function setUserAuthenticationValidWhileOnBody(remainsValid: boolean): JKeyGenParameterSpec_Builder; cdecl;
    function setUserAuthenticationValidityDurationSeconds(seconds: Integer): JKeyGenParameterSpec_Builder; cdecl;
    function setUserConfirmationRequired(required: boolean): JKeyGenParameterSpec_Builder; cdecl;
    function setUserPresenceRequired(required: boolean): JKeyGenParameterSpec_Builder; cdecl;
  end;
  TJKeyGenParameterSpec_Builder = class(TJavaGenericImport<JKeyGenParameterSpec_BuilderClass, JKeyGenParameterSpec_Builder>)
  end;

  JKeyPropertiesClass = interface(JObjectClass)
    ['{A61ABB04-D7BE-4279-B133-B739BA7CD910}']
    {class} function _GetBLOCK_MODE_CBC: JString; cdecl;
    {class} function _GetBLOCK_MODE_CTR: JString; cdecl;
    {class} function _GetBLOCK_MODE_ECB: JString; cdecl;
    {class} function _GetBLOCK_MODE_GCM: JString; cdecl;
    {class} function _GetDIGEST_MD5: JString; cdecl;
    {class} function _GetDIGEST_NONE: JString; cdecl;
    {class} function _GetDIGEST_SHA1: JString; cdecl;
    {class} function _GetDIGEST_SHA224: JString; cdecl;
    {class} function _GetDIGEST_SHA256: JString; cdecl;
    {class} function _GetDIGEST_SHA384: JString; cdecl;
    {class} function _GetDIGEST_SHA512: JString; cdecl;
    {class} function _GetENCRYPTION_PADDING_NONE: JString; cdecl;
    {class} function _GetENCRYPTION_PADDING_PKCS7: JString; cdecl;
    {class} function _GetENCRYPTION_PADDING_RSA_OAEP: JString; cdecl;
    {class} function _GetENCRYPTION_PADDING_RSA_PKCS1: JString; cdecl;
    {class} function _GetKEY_ALGORITHM_AES: JString; cdecl;
    {class} function _GetKEY_ALGORITHM_EC: JString; cdecl;
    {class} function _GetKEY_ALGORITHM_HMAC_SHA1: JString; cdecl;
    {class} function _GetKEY_ALGORITHM_HMAC_SHA224: JString; cdecl;
    {class} function _GetKEY_ALGORITHM_HMAC_SHA256: JString; cdecl;
    {class} function _GetKEY_ALGORITHM_HMAC_SHA384: JString; cdecl;
    {class} function _GetKEY_ALGORITHM_HMAC_SHA512: JString; cdecl;
    {class} function _GetKEY_ALGORITHM_RSA: JString; cdecl;
    {class} function _GetORIGIN_GENERATED: Integer; cdecl;
    {class} function _GetORIGIN_IMPORTED: Integer; cdecl;
    {class} function _GetORIGIN_UNKNOWN: Integer; cdecl;
    {class} function _GetPURPOSE_DECRYPT: Integer; cdecl;
    {class} function _GetPURPOSE_ENCRYPT: Integer; cdecl;
    {class} function _GetPURPOSE_SIGN: Integer; cdecl;
    {class} function _GetPURPOSE_VERIFY: Integer; cdecl;
    {class} function _GetSIGNATURE_PADDING_RSA_PKCS1: JString; cdecl;
    {class} function _GetSIGNATURE_PADDING_RSA_PSS: JString; cdecl;
    {class} property BLOCK_MODE_CBC: JString read _GetBLOCK_MODE_CBC;
    {class} property BLOCK_MODE_CTR: JString read _GetBLOCK_MODE_CTR;
    {class} property BLOCK_MODE_ECB: JString read _GetBLOCK_MODE_ECB;
    {class} property BLOCK_MODE_GCM: JString read _GetBLOCK_MODE_GCM;
    {class} property DIGEST_MD5: JString read _GetDIGEST_MD5;
    {class} property DIGEST_NONE: JString read _GetDIGEST_NONE;
    {class} property DIGEST_SHA1: JString read _GetDIGEST_SHA1;
    {class} property DIGEST_SHA224: JString read _GetDIGEST_SHA224;
    {class} property DIGEST_SHA256: JString read _GetDIGEST_SHA256;
    {class} property DIGEST_SHA384: JString read _GetDIGEST_SHA384;
    {class} property DIGEST_SHA512: JString read _GetDIGEST_SHA512;
    {class} property ENCRYPTION_PADDING_NONE: JString read _GetENCRYPTION_PADDING_NONE;
    {class} property ENCRYPTION_PADDING_PKCS7: JString read _GetENCRYPTION_PADDING_PKCS7;
    {class} property ENCRYPTION_PADDING_RSA_OAEP: JString read _GetENCRYPTION_PADDING_RSA_OAEP;
    {class} property ENCRYPTION_PADDING_RSA_PKCS1: JString read _GetENCRYPTION_PADDING_RSA_PKCS1;
    {class} property KEY_ALGORITHM_AES: JString read _GetKEY_ALGORITHM_AES;
    {class} property KEY_ALGORITHM_EC: JString read _GetKEY_ALGORITHM_EC;
    {class} property KEY_ALGORITHM_HMAC_SHA1: JString read _GetKEY_ALGORITHM_HMAC_SHA1;
    {class} property KEY_ALGORITHM_HMAC_SHA224: JString read _GetKEY_ALGORITHM_HMAC_SHA224;
    {class} property KEY_ALGORITHM_HMAC_SHA256: JString read _GetKEY_ALGORITHM_HMAC_SHA256;
    {class} property KEY_ALGORITHM_HMAC_SHA384: JString read _GetKEY_ALGORITHM_HMAC_SHA384;
    {class} property KEY_ALGORITHM_HMAC_SHA512: JString read _GetKEY_ALGORITHM_HMAC_SHA512;
    {class} property KEY_ALGORITHM_RSA: JString read _GetKEY_ALGORITHM_RSA;
    {class} property ORIGIN_GENERATED: Integer read _GetORIGIN_GENERATED;
    {class} property ORIGIN_IMPORTED: Integer read _GetORIGIN_IMPORTED;
    {class} property ORIGIN_UNKNOWN: Integer read _GetORIGIN_UNKNOWN;
    {class} property PURPOSE_DECRYPT: Integer read _GetPURPOSE_DECRYPT;
    {class} property PURPOSE_ENCRYPT: Integer read _GetPURPOSE_ENCRYPT;
    {class} property PURPOSE_SIGN: Integer read _GetPURPOSE_SIGN;
    {class} property PURPOSE_VERIFY: Integer read _GetPURPOSE_VERIFY;
    {class} property SIGNATURE_PADDING_RSA_PKCS1: JString read _GetSIGNATURE_PADDING_RSA_PKCS1;
    {class} property SIGNATURE_PADDING_RSA_PSS: JString read _GetSIGNATURE_PADDING_RSA_PSS;
  end;

  [JavaSignature('android/security/keystore/KeyProperties')]
  JKeyProperties = interface(JObject)
    ['{63F347AF-ED22-424F-866B-60B040CA5C35}']
  end;
  TJKeyProperties = class(TJavaGenericImport<JKeyPropertiesClass, JKeyProperties>) end;

  JKeyPairClass = interface(JObjectClass)
    ['{61FF1195-FFFD-440B-BBDC-3D97376B2A55}']
    function init(publicKey : JPublicKey; privateKey : JPrivateKey) : JKeyPair; cdecl;
  end;

  [JavaSignature('java/security/KeyPair')]
  JKeyPair = interface(JObject)
    ['{0E56BB1E-C6F8-491C-9CE0-A43AE3FE3E75}']
    function getPrivate : JPrivateKey; cdecl;
    function getPublic : JPublicKey; cdecl;
  end;
  TJKeyPair = class(TJavaGenericImport<JKeyPairClass, JKeyPair>) end;

  JResultDataClass = interface(JObjectClass)
    ['{347A8EBA-E0EE-4715-BFFB-974B42F2A444}']
    function _GetSTATUS_NOT_IN_REQUEST_MESSAGE: Integer; cdecl;
    function _GetSTATUS_NOT_REQUESTED: Integer; cdecl;
    function _GetSTATUS_NO_ACCESS_CONTROL_PROFILES: Integer; cdecl;
    function _GetSTATUS_NO_SUCH_ENTRY: Integer; cdecl;
    function _GetSTATUS_OK: Integer; cdecl;
    function _GetSTATUS_READER_AUTHENTICATION_FAILED: Integer; cdecl;
    function _GetSTATUS_USER_AUTHENTICATION_FAILED: Integer; cdecl;
    property STATUS_NOT_IN_REQUEST_MESSAGE: Integer read _GetSTATUS_NOT_IN_REQUEST_MESSAGE;
    property STATUS_NOT_REQUESTED: Integer read _GetSTATUS_NOT_REQUESTED;
    property STATUS_NO_ACCESS_CONTROL_PROFILES: Integer read _GetSTATUS_NO_ACCESS_CONTROL_PROFILES;
    property STATUS_NO_SUCH_ENTRY: Integer read _GetSTATUS_NO_SUCH_ENTRY;
    property STATUS_OK: Integer read _GetSTATUS_OK;
    property STATUS_READER_AUTHENTICATION_FAILED: Integer read _GetSTATUS_READER_AUTHENTICATION_FAILED;
    property STATUS_USER_AUTHENTICATION_FAILED: Integer read _GetSTATUS_USER_AUTHENTICATION_FAILED;
  end;

  [JavaSignature('android/security/identity/ResultData')]
  JResultData = interface(JObject)
    ['{CE53B8A5-C27E-440B-9294-AD5357BCCB43}']
    function getAuthenticatedData: TJavaArray<Byte>; cdecl;
    function getEntry(namespaceName: JString; name: JString): TJavaArray<Byte>; cdecl;
    function getEntryNames(namespaceName: JString): JCollection; cdecl;
    function getMessageAuthenticationCode: TJavaArray<Byte>; cdecl;
    function getNamespaces: JCollection; cdecl;
    function getRetrievedEntryNames(namespaceName: JString): JCollection; cdecl;
    function getStaticAuthenticationData: TJavaArray<Byte>; cdecl;
    function getStatus(namespaceName: JString; name: JString): Integer; cdecl;
  end;
  TJResultData = class(TJavaGenericImport<JResultDataClass, JResultData>)
  end;

  // API 30
  JIdentityCredentialClass = interface(JObjectClass)
    ['{144C6375-3BCD-4B16-ADBE-37E02D5771B7}']
  end;

  [JavaSignature('android/security/identity/IdentityCredential')]
  JIdentityCredential = interface(JObject)
    ['{01F87605-B702-4D66-A182-2BF27A024FCD}']
    function createEphemeralKeyPair: JKeyPair; cdecl;
    function decryptMessageFromReader(messageCiphertext: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function encryptMessageToReader(messageCiphertext: TJavaArray<Byte>): TJavaArray<Byte>; cdecl;
    function getAuthKeysNeedingCertification: JCollection; cdecl;
    function getAuthenticationDataUsageCount: TJavaArray<Integer>; cdecl;
    function getCredentialKeyCertificateChain: JCollection; cdecl;
    function getEntries(requestMessage: TJavaArray<Byte>; entriesToRequest: JMap; sessionTranscript: TJavaArray<Byte>;
      readerSignature: TJavaArray<Byte>): JResultData; cdecl;
    procedure setAllowUsingExhaustedKeys(allowUsingExhaustedKeys: boolean); cdecl;
    procedure setAvailableAuthenticationKeys(keyCount: Integer; maxUsesPerKey: Integer); cdecl;
    procedure setReaderEphemeralPublicKey(readerEphemeralPublicKey: JPublicKey); cdecl;
    procedure storeStaticAuthenticationData(authenticationKey: JX509Certificate; staticAuthData: TJavaArray<Byte>); cdecl;
  end;
  TJIdentityCredential = class(TJavaGenericImport<JIdentityCredentialClass, JIdentityCredential>)
  end;

  JMacClass = interface(JObjectClass)
    ['{2B12D705-6AA8-4626-A061-7010A7CC4E55}']
    {class} function getInstance(algorithm: JString): JMac; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JString): JMac; cdecl; overload;
    {class} function getInstance(algorithm: JString; provider: JProvider): JMac; cdecl; overload;
  end;

  [JavaSignature('javax/crypto/Mac')]
  JMac = interface(JObject)
    ['{8A1FC6BF-A5D4-4E63-B611-028E71B251CC}']
    function clone: JObject; cdecl;
    function doFinal: TJavaArray<Byte>; cdecl; overload;
    procedure doFinal(output: TJavaArray<Byte>; outOffset: Integer); cdecl; overload;
    function doFinal(input: TJavaArray<Byte>): TJavaArray<Byte>; cdecl; overload;
    function getAlgorithm: JString; cdecl;
    function getMacLength: Integer; cdecl;
    function getProvider: JProvider; cdecl;
    procedure init(key: JKey; params: JAlgorithmParameterSpec); cdecl; overload;
    procedure init(key: JKey); cdecl; overload;
    procedure reset; cdecl;
    procedure update(input: Byte); cdecl; overload;
    procedure update(input: TJavaArray<Byte>); cdecl; overload;
    procedure update(input: TJavaArray<Byte>; offset: Integer; len: Integer); cdecl; overload;
    procedure update(input: JByteBuffer); cdecl; overload;
  end;
  TJMac = class(TJavaGenericImport<JMacClass, JMac>) end;

  JSecretKeyClass = interface(JKeyClass)
    ['{631CE56D-D41C-4899-A6BF-BEE8992932BF}']
    {class} function _GetserialVersionUID: Int64; cdecl;
    {class} // serialVersionUID is defined in parent interface
  end;

  [JavaSignature('javax/crypto/SecretKey')]
  JSecretKey = interface(JKey)
    ['{1F7C647E-EF1C-42D5-86F5-BCECB950241A}']
  end;
  TJSecretKey = class(TJavaGenericImport<JSecretKeyClass, JSecretKey>) end;

implementation

end.
