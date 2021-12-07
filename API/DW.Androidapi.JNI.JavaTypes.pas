unit DW.Androidapi.JNI.JavaTypes;

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
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JBufferedReader = interface;
  JInputStreamReader = interface;
  JStringWriter = interface;

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
