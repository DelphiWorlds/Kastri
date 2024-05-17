unit DW.Androidapi.JNI.JavaTypes;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JBufferedInputStream = interface;
  JBufferedOutputStream = interface;
  JBufferedReader = interface;
  JFilterInputStream = interface;
  JFilterOutputStream = interface;
  JInputStreamReader = interface;
  JStringWriter = interface;

  JFilterOutputStreamClass = interface(JOutputStreamClass)
    ['{9D236492-DD2D-4445-8A05-170AEA2891F2}']
    {class} function init(outputstream: JOutputStream): JFilterOutputStream; cdecl;
  end;

  [JavaSignature('java/io/FilterOutputStream')]
  JFilterOutputStream = interface(JOutputStream)
    ['{0A7861B9-1B72-49AF-B218-58F0655FF798}']
    procedure close; cdecl;
    procedure flush; cdecl;
    procedure write(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer); cdecl; overload;
    procedure write(bytes: TJavaArray<Byte>); cdecl; overload;
    procedure write(int: Integer); cdecl; overload;
  end;
  TJFilterOutputStream = class(TJavaGenericImport<JFilterOutputStreamClass, JFilterOutputStream>) end;

  JFilterInputStreamClass = interface(JInputStreamClass)
    ['{DEC5A590-0417-4650-9F16-F640BBFB11FA}']
  end;

  [JavaSignature('java/io/FilterInputStream')]
  JFilterInputStream = interface(JInputStream)
    ['{92423376-849F-46D5-83CC-7E6B3FECE7A2}']
    function available: Integer; cdecl;
    procedure close; cdecl;
    procedure mark(int: Integer); cdecl;
    function markSupported: Boolean; cdecl;
    function read: Integer; cdecl; overload;
    function read(bytes: TJavaArray<Byte>): Integer; cdecl; overload;
    function read(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer): Integer; cdecl; overload;
    procedure reset; cdecl;
    function skip(long: Int64): Int64; cdecl;
  end;
  TJFilterInputStream = class(TJavaGenericImport<JFilterInputStreamClass, JFilterInputStream>) end;

  JBufferedOutputStreamClass = interface(JFilterOutputStreamClass)
    ['{615C34B5-C9A2-430D-9B86-B911A302C64C}']
    {class} function init(outputstream: JOutputStream; int: Integer): JBufferedOutputStream; cdecl; overload;
    {class} function init(outputstream: JOutputStream): JBufferedOutputStream; cdecl; overload;
  end;

  [JavaSignature('java/io/BufferedOutputStream')]
  JBufferedOutputStream = interface(JFilterOutputStream)
    ['{9BB1290A-2B33-4D9A-A004-151B3781E448}']
    procedure flush; cdecl;
    procedure write(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer); cdecl; overload;
    procedure write(int: Integer); cdecl; overload;
  end;
  TJBufferedOutputStream = class(TJavaGenericImport<JBufferedOutputStreamClass, JBufferedOutputStream>) end;

  JBufferedInputStreamClass = interface(JFilterInputStreamClass)
    ['{BB59EA22-36E7-43D0-A621-DC2875C706F4}']
    {class} function init(inputstream: JInputStream): JBufferedInputStream; cdecl; overload;
    {class} function init(inputstream: JInputStream; int: Integer): JBufferedInputStream; cdecl; overload;
  end;

  [JavaSignature('java/io/BufferedInputStream')]
  JBufferedInputStream = interface(JFilterInputStream)
    ['{00751F50-FE29-4D1F-AE2E-699EAFEE7E37}']
    function available: Integer; cdecl;
    procedure close; cdecl;
    procedure mark(int: Integer); cdecl;
    function markSupported: Boolean; cdecl;
    function read(bytes: TJavaArray<Byte>; int: Integer; int_1: Integer): Integer; cdecl; overload;
    function read: Integer; cdecl; overload;
    procedure reset; cdecl;
    function skip(long: Int64): Int64; cdecl;
  end;
  TJBufferedInputStream = class(TJavaGenericImport<JBufferedInputStreamClass, JBufferedInputStream>) end;

  JBufferedReaderClass = interface(JReaderClass)
    ['{ED860F80-BCB7-4BC0-977F-95DE9CACE248}']
    {class} function init(in_: JReader): JBufferedReader; cdecl; overload;
    {class} function init(in_: JReader; size: Integer): JBufferedReader; cdecl; overload;
  end;

  [JavaSignature('java/io/BufferedReader')]
  JBufferedReader = interface(JReader)
    ['{81A25906-BC48-4F21-B1D4-5FDA81218C63}']
    procedure close; cdecl;
    procedure mark(markLimit: Integer); cdecl;
    function markSupported: Boolean; cdecl;
    function read: Integer; cdecl; overload;
    function read(buffer: TJavaArray<Char>; offset: Integer; length: Integer): Integer; cdecl; overload;
    function readLine: JString; cdecl;
    function ready: Boolean; cdecl;
    procedure reset; cdecl;
    function skip(charCount: Int64): Int64; cdecl;
  end;
  TJBufferedReader = class(TJavaGenericImport<JBufferedReaderClass, JBufferedReader>) end;

  JInputStreamReaderClass = interface(JReaderClass)
    ['{E418421B-23F2-4AD7-814B-B18419BA54A4}']
    { class } function init(in_: JInputStream): JInputStreamReader; cdecl;
  end;

  [JavaSignature('java/io/InputStreamReader')]
  JInputStreamReader = interface(JReader)
    ['{15A880FC-8D0E-4480-A05A-BFBEC0BF249D}']
    procedure close; cdecl;
    function getEncoding: JString; cdecl;
    procedure mark(readAheadLimit: Integer); cdecl;
    function read: Integer; cdecl; overload;
    function read(cbuf: TJavaArray<Char>; off: Integer; len: Integer): Integer; cdecl; overload;
    function ready: Boolean; cdecl;
  end;
  TJInputStreamReader = class(TJavaGenericImport<JInputStreamReaderClass, JInputStreamReader>) end;

  JStringWriterClass = interface(JWriterClass)
    ['{CF281518-33EA-42C7-B15D-3E53DF6316BC}']
    {class} function init: JStringWriter; cdecl; overload;
    {class} function init(initialSize: Integer): JStringWriter; cdecl; overload;
  end;

  [JavaSignature('java/io/StringWriter')]
  JStringWriter = interface(JWriter)
    ['{914B33F0-4C43-4730-82AE-985B65D989E5}']
    function append(c: Char): JWriter; cdecl; overload;
    function append(csq: JCharSequence): JWriter; cdecl; overload;
    function append(csq: JCharSequence; start: Integer; end_: Integer): JWriter; cdecl; overload;
    procedure close; cdecl;
    procedure flush; cdecl;
    function getBuffer: JStringBuffer; cdecl;
    function toString: JString; cdecl;
    procedure write(c: Integer); cdecl; overload;
    procedure write(cbuf: TJavaArray<Char>); cdecl; overload;
    procedure write(cbuf: TJavaArray<Char>; off: Integer; len: Integer); cdecl; overload;
    procedure write(str: JString); cdecl; overload;
    procedure write(str: JString; off: Integer; len: Integer); cdecl; overload;
  end;
  TJStringWriter = class(TJavaGenericImport<JStringWriterClass, JStringWriter>) end;

implementation

end.
