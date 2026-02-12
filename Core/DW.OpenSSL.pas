unit DW.OpenSSL;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2026 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

// NOTE: This unit contains the *bare minimum* for signing using SHA256 with RSA, on Windows, iOS, macOS and Android
// Extended with AES encryption/decryption functionality

// Binaries for OpenSSL 3.x are here:
//   https://github.com/TaurusTLS-Developers/OpenSSL-Distribution/releases

interface

uses
  System.SysUtils;

const
  {$IF Defined(MSWINDOWS)}
  {$IF Defined(WIN32)}
  LIB_CRYPTO = 'libcrypto-3.dll';
  {$ELSEIF Defined(WIN64)}
  LIB_CRYPTO = 'libcrypto-3-x64.dll';
  {$ENDIF}
  _PU = '';
  {$ENDIF}
  {$IF Defined(POSIX)}
  LIB_CRYPTO = 'libcrypto.a';
  _PU = '';
  {$ENDIF}

  EVP_PKEY_CTRL_MD = 1;

  EVP_PKEY_OP_SIGN = (1 shl 3);
  EVP_PKEY_OP_VERIFY = (1 shl 4);
  EVP_PKEY_OP_VERIFYRECOVER = (1 shl 5);
  EVP_PKEY_OP_SIGNCTX = (1 shl 6);
  EVP_PKEY_OP_VERIFYCTX = (1 shl 7);
  EVP_PKEY_OP_ENCRYPT = (1 shl 8);
  EVP_PKEY_OP_DECRYPT = (1 shl 9);
  EVP_PKEY_OP_DERIVE = (1 shl 10);
  EVP_PKEY_OP_TYPE_SIG = (EVP_PKEY_OP_SIGN or EVP_PKEY_OP_VERIFY or EVP_PKEY_OP_VERIFYRECOVER or EVP_PKEY_OP_SIGNCTX or EVP_PKEY_OP_VERIFYCTX);

  AES_BLOCK_SIZE = 16;
  AES_KEY_SIZE = 32;

type
  PBIO = Pointer;
  PENGINE = Pointer;
  PEVP_MD = Pointer;
  PEVP_MD_CTX = Pointer;
  PEVP_PKEY = Pointer;
  PEVP_PKEY_CTX = Pointer;
  PPEVP_PKEY = ^PEVP_PKEY;
  PPEVP_PKEY_CTX = ^PEVP_PKEY_CTX;
  PEVP_CIPHER = Pointer;
  PEVP_CIPHER_CTX = Pointer;
  POSSL_LIB_CTX = Pointer;
  POSSL_PARAM = Pointer;

  Ppem_password_cb = function(buf: PUTF8Char; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;

function BIO_free(a: PBIO): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_free';
function BIO_new_mem_buf(buf: Pointer; len: Integer): PBIO; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_new_mem_buf';
function EVP_DigestSignFinal(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: NativeUInt): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSignFinal';
function EVP_DigestSignInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; &type: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSignInit';
function EVP_DigestSignInit_ex(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; mdname: PUTF8Char; libctx: POSSL_LIB_CTX; props: PUTF8Char; pkey: PEVP_PKEY;
  params: POSSL_PARAM): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSignInit_ex';
function EVP_DigestUpdate(ctx: PEVP_MD_CTX; d: Pointer; cnt: NativeUInt): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestUpdate';
procedure EVP_MD_CTX_free(ctx: PEVP_MD_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_CTX_free';
function EVP_MD_CTX_new: PEVP_MD_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_MD_CTX_new';
function EVP_PKEY_CTX_ctrl(ctx: PEVP_PKEY_CTX; keytype: Integer; optype: Integer; cmd: Integer; p1: Integer; p2: Pointer): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_ctrl';
procedure EVP_PKEY_CTX_free(ctx: PEVP_PKEY_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_free';
function EVP_PKEY_CTX_new(pkey: PEVP_PKEY; e: PENGINE): PEVP_PKEY_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_CTX_new';
function EVP_PKEY_CTX_set_signature_md(ctx: PEVP_PKEY_CTX; md: Pointer): integer;
procedure EVP_PKEY_free(pkey: PEVP_PKEY); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_free';
function EVP_PKEY_sign(ctx: PEVP_PKEY_CTX; sig: PByte; var siglen: NativeUInt; tbs: PByte; tbslen: NativeUInt): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_sign';
function EVP_PKEY_sign_init(ctx: PEVP_PKEY_CTX): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_PKEY_sign_init';
function EVP_sha256: PEVP_MD; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_sha256';
function PEM_read_bio_PrivateKey(bp: PBIO; x: PPEVP_PKEY; cb: Ppem_password_cb; u: Pointer): PEVP_PKEY; cdecl;
  external LIB_CRYPTO name _PU + 'PEM_read_bio_PrivateKey';
function EVP_aes_256_cbc: PEVP_CIPHER; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_aes_256_cbc';
function EVP_CIPHER_CTX_new: PEVP_CIPHER_CTX; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_new';
procedure EVP_CIPHER_CTX_free(ctx: PEVP_CIPHER_CTX); cdecl;
  external LIB_CRYPTO name _PU + 'EVP_CIPHER_CTX_free';
function EVP_EncryptInit_ex(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER; engine: PENGINE; key: PByte; iv: PByte): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_EncryptInit_ex';
function EVP_EncryptUpdate(ctx: PEVP_CIPHER_CTX; out_data: PByte; var out_len: Integer; in_data: PByte; in_len: Integer): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_EncryptUpdate';
function EVP_EncryptFinal_ex(ctx: PEVP_CIPHER_CTX; out_data: PByte; var out_len: Integer): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_EncryptFinal_ex';
function EVP_DecryptInit_ex(ctx: PEVP_CIPHER_CTX; cipher: PEVP_CIPHER; engine: PENGINE; key: PByte; iv: PByte): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DecryptInit_ex';
function EVP_DecryptUpdate(ctx: PEVP_CIPHER_CTX; out_data: PByte; var out_len: Integer; in_data: PByte; in_len: Integer): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DecryptUpdate';
function EVP_DecryptFinal_ex(ctx: PEVP_CIPHER_CTX; out_data: PByte; var out_len: Integer): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DecryptFinal_ex';
function ERR_get_error: Cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'ERR_get_error';
function ERR_peek_last_error: Cardinal; cdecl;
  external LIB_CRYPTO name _PU + 'ERR_peek_last_error';
procedure ERR_error_string_n(e: Cardinal; buf: PAnsiChar; len: NativeUInt); cdecl;
  external LIB_CRYPTO name _PU + 'ERR_error_string_n';
function ERR_lib_error_string(e: Cardinal): PAnsiChar; cdecl;
  external LIB_CRYPTO name _PU + 'ERR_lib_error_string';
function ERR_func_error_string(e: Cardinal): PAnsiChar; cdecl;
  external LIB_CRYPTO name _PU + 'ERR_func_error_string';
function ERR_reason_error_string(e: Cardinal): PAnsiChar; cdecl;
  external LIB_CRYPTO name _PU + 'ERR_reason_error_string';
procedure ERR_clear_error; cdecl;
  external LIB_CRYPTO name _PU + 'ERR_clear_error';

function DecryptData(const AData: TBytes; const AKey: TBytes; const AIV: TBytes = []): TBytes;
function EncryptData(const AData: TBytes; const AKey: TBytes; const AIV: TBytes = []): TBytes;
function GetErrorString: string;

implementation

function EVP_PKEY_CTX_set_signature_md(ctx: PEVP_PKEY_CTX; md: Pointer): integer;
begin
  Result := EVP_PKEY_CTX_ctrl(ctx, -1, EVP_PKEY_OP_TYPE_SIG, EVP_PKEY_CTRL_MD, 0, md);
end;

function GetErrorString: string;
var
  LErrorCode: Cardinal;
  LErrorBuffer: array[0..255] of AnsiChar;
begin
  Result := '';
  LErrorCode := ERR_get_error;
  if LErrorCode <> 0 then
  begin
    ERR_error_string_n(LErrorCode, @LErrorBuffer[0], Length(LErrorBuffer));
    Result := string(AnsiString(LErrorBuffer));
  end;
end;

function EncryptData(const AData: TBytes; const AKey: TBytes; const AIV: TBytes = []): TBytes;
var
  LCTX: PEVP_CIPHER_CTX;
  LLen, LFinalLen, LEncryptedLen: Integer;
  LKeyBytes, LIVBytes: TBytes;
begin
  Result := [];
  LKeyBytes := Copy(AKey, 0, AES_KEY_SIZE);
  if Length(LKeyBytes) < AES_KEY_SIZE then
  begin
    SetLength(LKeyBytes, AES_KEY_SIZE);
    FillChar(LKeyBytes[Length(AKey)], AES_KEY_SIZE - Length(AKey), 0);
  end;
  LIVBytes := Copy(AIV, 0, AES_BLOCK_SIZE);
  if Length(LIVBytes) < AES_BLOCK_SIZE then
  begin
    SetLength(LIVBytes, AES_BLOCK_SIZE);
    FillChar(LIVBytes[Length(AIV)], AES_BLOCK_SIZE - Length(AIV), 0);
  end;
  LCTX := EVP_CIPHER_CTX_new;
  if LCTX <> nil then
  try
    if EVP_EncryptInit_ex(LCTX, EVP_aes_256_cbc, nil, PByte(LKeyBytes), PByte(LIVBytes)) = 1 then
    begin
      SetLength(Result, Length(AData) + AES_BLOCK_SIZE);
      if EVP_EncryptUpdate(LCTX, PByte(Result), LLen, PByte(AData), Length(AData)) = 1 then
      begin
        LEncryptedLen := LLen;
        if EVP_EncryptFinal_ex(LCTX, PByte(Result) + LLen, LFinalLen) = 1 then
          SetLength(Result, LEncryptedLen + LFinalLen)
        else
          Result := [];
      end;
    end;
  finally
    EVP_CIPHER_CTX_free(LCTX);
  end;
end;

function DecryptData(const AData: TBytes; const AKey: TBytes; const AIV: TBytes = []): TBytes;
var
  LCTX: PEVP_CIPHER_CTX;
  LLen, LFinalLen, LDecryptedLen: Integer;
  LKeyBytes, LIVBytes: TBytes;
begin
  Result := [];
  LKeyBytes := Copy(AKey, 0, AES_KEY_SIZE);
  if Length(LKeyBytes) < AES_KEY_SIZE then
  begin
    SetLength(LKeyBytes, AES_KEY_SIZE);
    FillChar(LKeyBytes[Length(AKey)], AES_KEY_SIZE - Length(AKey), 0);
  end;
  LIVBytes := Copy(AIV, 0, AES_BLOCK_SIZE);
  if Length(LIVBytes) < AES_BLOCK_SIZE then
  begin
    SetLength(LIVBytes, AES_BLOCK_SIZE);
    FillChar(LIVBytes[Length(AIV)], AES_BLOCK_SIZE - Length(AIV), 0);
  end;
  LCTX := EVP_CIPHER_CTX_new;
  if LCTX <> nil then
  try
    if EVP_DecryptInit_ex(LCTX, EVP_aes_256_cbc, nil, PByte(LKeyBytes), PByte(LIVBytes)) = 1 then
    begin
      SetLength(Result, Length(AData));
      if EVP_DecryptUpdate(LCTX, PByte(Result), LLen, PByte(AData), Length(AData)) = 1 then
      begin
        LDecryptedLen := LLen;
        if EVP_DecryptFinal_ex(LCTX, PByte(Result) + LLen, LFinalLen) = 1 then
          SetLength(Result, LDecryptedLen + LFinalLen)
        else
          Result := [];
      end
      else
        Result := [];
    end;
  finally
    EVP_CIPHER_CTX_free(LCTX);
  end;
end;

end.
