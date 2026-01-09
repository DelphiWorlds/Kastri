unit DW.Androidapi.JNI.AndroidX.Media3.Decoder;

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

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Media;

type
  JBuffer = interface;
  JCryptoConfig = interface;
  JCryptoInfo = interface;
  JDecoderInputBuffer = interface;

  JCryptoInfoClass = interface(JObjectClass)
    ['{2CC55A2F-3AF8-4133-9F57-6E6F271ADDFE}']
    {class} function init: JCryptoInfo; cdecl;
  end;

  [JavaSignature('androidx/media3/decoder/CryptoInfo')]
  JCryptoInfo = interface(JObject)
    ['{63E4C597-A191-4B13-8D5C-29E8560251E9}']
    function _GetclearBlocks: Integer; cdecl;
    function _GetencryptedBlocks: Integer; cdecl;
    function _Getiv: TJavaArray<Byte>; cdecl;
    function _Getkey: TJavaArray<Byte>; cdecl;
    function _Getmode: Integer; cdecl;
    function _GetnumBytesOfClearData: TJavaArray<Integer>; cdecl;
    function _GetnumBytesOfEncryptedData: TJavaArray<Integer>; cdecl;
    function _GetnumSubSamples: Integer; cdecl;
    function getFrameworkCryptoInfo: JMediaCodec_CryptoInfo; cdecl;
    procedure increaseClearDataFirstSubSampleBy(int: Integer); cdecl;
    procedure &set(int: Integer; ints: TJavaArray<Integer>; ints_1: TJavaArray<Integer>; bytes: TJavaArray<Byte>; bytes_1: TJavaArray<Byte>; int_1: Integer; int_2: Integer; int_3: Integer); cdecl;
    property clearBlocks: Integer read _GetclearBlocks;
    property encryptedBlocks: Integer read _GetencryptedBlocks;
    property iv: TJavaArray<Byte> read _Getiv;
    property key: TJavaArray<Byte> read _Getkey;
    property mode: Integer read _Getmode;
    property numBytesOfClearData: TJavaArray<Integer> read _GetnumBytesOfClearData;
    property numBytesOfEncryptedData: TJavaArray<Integer> read _GetnumBytesOfEncryptedData;
    property numSubSamples: Integer read _GetnumSubSamples;
  end;
  TJCryptoInfo = class(TJavaGenericImport<JCryptoInfoClass, JCryptoInfo>) end;

  JCryptoConfigClass = interface(IJavaClass)
    ['{7347B641-DF7C-4515-93D3-AE708FEB3527}']
  end;

  [JavaSignature('androidx/media3/decoder/CryptoConfig')]
  JCryptoConfig = interface(IJavaInstance)
    ['{0A9007FB-5AA8-4980-AC55-DEF576EEB85E}']
  end;
  TJCryptoConfig = class(TJavaGenericImport<JCryptoConfigClass, JCryptoConfig>) end;

  JBufferClass = interface(JObjectClass)
    ['{888E44A7-91E4-403F-BE9C-55194E8CE5F7}']
    {class} function init: JBuffer; cdecl;
  end;

  [JavaSignature('androidx/media3/decoder/Buffer')]
  JBuffer = interface(JObject)
    ['{47EED4C5-4EB1-4D6D-9461-8031028539F0}']
    procedure addFlag(int: Integer); cdecl;
    procedure clear; cdecl;
    procedure clearFlag(int: Integer); cdecl;
    function hasSupplementalData: Boolean; cdecl;
    function isDecodeOnly: Boolean; cdecl;
    function isEndOfStream: Boolean; cdecl;
    function isFirstSample: Boolean; cdecl;
    function isKeyFrame: Boolean; cdecl;
    function isLastSample: Boolean; cdecl;
    procedure setFlags(int: Integer); cdecl;
  end;
  TJBuffer = class(TJavaGenericImport<JBufferClass, JBuffer>) end;

  JDecoderInputBufferClass = interface(JBufferClass)
    ['{F7DEA483-1786-43E4-9EE7-EE6FB57D9971}']
    {class} function _GetBUFFER_REPLACEMENT_MODE_DIRECT: Integer; cdecl;
    {class} function _GetBUFFER_REPLACEMENT_MODE_DISABLED: Integer; cdecl;
    {class} function _GetBUFFER_REPLACEMENT_MODE_NORMAL: Integer; cdecl;
    {class} function init(int: Integer): JDecoderInputBuffer; cdecl; overload;
    {class} function init(int: Integer; int_1: Integer): JDecoderInputBuffer; cdecl; overload;
    {class} function newNoDataInstance: JDecoderInputBuffer; cdecl;
    {class} property BUFFER_REPLACEMENT_MODE_DIRECT: Integer read _GetBUFFER_REPLACEMENT_MODE_DIRECT;
    {class} property BUFFER_REPLACEMENT_MODE_DISABLED: Integer read _GetBUFFER_REPLACEMENT_MODE_DISABLED;
    {class} property BUFFER_REPLACEMENT_MODE_NORMAL: Integer read _GetBUFFER_REPLACEMENT_MODE_NORMAL;
  end;

  [JavaSignature('androidx/media3/decoder/DecoderInputBuffer')]
  JDecoderInputBuffer = interface(JBuffer)
    ['{8BBA1B13-B323-4886-B020-F59F3859BAD8}']
    function _GetcryptoInfo: JCryptoInfo; cdecl;
    function _Getdata: JByteBuffer; cdecl;
    function _Getformat: JFormat; cdecl;
    function _GetsupplementalData: JByteBuffer; cdecl;
    function _GettimeUs: Int64; cdecl;
    function _GetwaitingForKeys: Boolean; cdecl;
    procedure clear; cdecl;
    procedure ensureSpaceForWrite(int: Integer); cdecl;
    procedure flip; cdecl;
    function isEncrypted: Boolean; cdecl;
    procedure resetSupplementalData(int: Integer); cdecl;
    property cryptoInfo: JCryptoInfo read _GetcryptoInfo;
    property data: JByteBuffer read _Getdata;
    property format: JFormat read _Getformat;
    property supplementalData: JByteBuffer read _GetsupplementalData;
    property timeUs: Int64 read _GettimeUs;
    property waitingForKeys: Boolean read _GetwaitingForKeys;
  end;
  TJDecoderInputBuffer = class(TJavaGenericImport<JDecoderInputBufferClass, JDecoderInputBuffer>) end;

implementation

end.
