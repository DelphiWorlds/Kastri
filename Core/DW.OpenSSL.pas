unit DW.OpenSSL;

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

// NOTE: This unit contains the bare minimum for signing using SHA256 with RSA, on Windows ONLY
// It may be expanded to support other encryption in the future

// Binaries for OpenSSL 3.x are here:
//   https://github.com/IndySockets/OpenSSL-Binaries/tree/master/openssl-3_x
// To use Open SSL 3.x, please add this conditional define to your project: OpenSSL3
// Otherwise, binaries for OpenSSL 1.1 can be found here:
//   https://github.com/grijjy/DelphiOpenSsl/tree/master/Bin

interface

const
  {$IF Defined(WIN32)}
  {$IF Defined(OPENSSL3)}
  LIB_CRYPTO = 'libcrypto-3.dll';
  {$ELSE}
  LIB_CRYPTO = 'libcrypto-1_1.dll';
  {$ENDIF}
  _PU = '';
  {$ELSEIF Defined(WIN64)}
  {$IF Defined(OPENSSL3)}
  LIB_CRYPTO = 'libcrypto-3-x64.dll';
  {$ELSE}
  LIB_CRYPTO = 'libcrypto-1_1-x64.dll';
  {$ENDIF}
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

type
  PBIO = Pointer;
  PENGINE = Pointer;
  PEVP_MD = Pointer;
  PEVP_MD_CTX = Pointer;
  PEVP_PKEY = Pointer;
  PEVP_PKEY_CTX = Pointer;
  PPEVP_PKEY = ^PEVP_PKEY;
  PPEVP_PKEY_CTX = ^PEVP_PKEY_CTX;

  Ppem_password_cb = function(buf: PUTF8Char; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;

function BIO_free(a: PBIO): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_free';
function BIO_new_mem_buf(buf: Pointer; len: Integer): PBIO; cdecl;
  external LIB_CRYPTO name _PU + 'BIO_new_mem_buf';
function EVP_DigestSignFinal(ctx: PEVP_MD_CTX; sigret: PByte; var siglen: NativeUInt): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSignFinal';
function EVP_DigestSignInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; &type: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): Integer; cdecl;
  external LIB_CRYPTO name _PU + 'EVP_DigestSignInit';
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

implementation

function EVP_PKEY_CTX_set_signature_md(ctx: PEVP_PKEY_CTX; md: Pointer): integer;
begin
  Result := EVP_PKEY_CTX_ctrl(ctx, -1, EVP_PKEY_OP_TYPE_SIG, EVP_PKEY_CTRL_MD, 0, md);
end;

end.
