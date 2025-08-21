unit DW.Androidapi.JNI.Lang;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{  Copyright 2020-2025 Dave Nottage under MIT license   }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JProcess = interface;
  JProcessBuilder = interface;
  JRuntime = interface;

  JProcessClass = interface(JObjectClass)
    ['{F57F6F1A-B4A2-4CD0-9ACB-06CBEBDCA561}']
    {class} function init: JProcess; cdecl;
  end;

  [JavaSignature('java/lang/Process')]
  JProcess = interface(JObject)
    ['{1B505DDF-5BE4-40C8-BC81-75AAA5158B02}']
    procedure destroy; cdecl;
    function exitValue: Integer; cdecl;
    function getErrorStream: JInputStream; cdecl;
    function getInputStream: JInputStream; cdecl;
    function getOutputStream: JOutputStream; cdecl;
    function waitFor: Integer; cdecl;
  end;
  TJProcess = class(TJavaGenericImport<JProcessClass, JProcess>) end;

  JProcessBuilderClass = interface(JObjectClass)
    ['{88477143-F53C-4766-84AB-76685EDD3C13}']
    {class} function init(command: JList): JProcessBuilder; cdecl;
  end;

  [JavaSignature('java/lang/ProcessBuilder')]
  JProcessBuilder = interface(JObject)
    ['{0C076D6C-E2E5-49DD-859E-D50E7C04A9C6}']
    function command: JList; cdecl; overload;
    function command(command: JList): JProcessBuilder; cdecl; overload;
    function directory: JFile; cdecl; overload;
    function directory(directory: JFile): JProcessBuilder; cdecl; overload;
    function environment: JMap; cdecl;
    function redirectErrorStream: Boolean; cdecl; overload;
    function redirectErrorStream(redirectErrorStream: Boolean): JProcessBuilder; cdecl; overload;
    function start: JProcess; cdecl;
  end;
  TJProcessBuilder = class(TJavaGenericImport<JProcessBuilderClass, JProcessBuilder>) end;

  JRuntimeClass = interface(JObjectClass)
    ['{5F789EE2-7243-47D9-B950-F8B638DD3076}']
    {class} function getRuntime: JRuntime; cdecl;
    {class} procedure runFinalizersOnExit(run: Boolean); cdecl;
  end;

  [JavaSignature('java/lang/Runtime')]
  JRuntime = interface(JObject)
    ['{D56F91EA-6432-4391-ABBA-885F22C2E21A}']
    procedure addShutdownHook(hook: JThread); cdecl;
    function availableProcessors: Integer; cdecl;
    procedure gc; cdecl;
    function getLocalizedInputStream(stream: JInputStream): JInputStream; cdecl;
    function getLocalizedOutputStream(stream: JOutputStream): JOutputStream; cdecl;
    function exec(prog: JString): JProcess; cdecl; overload;
    function exec(prog: JString; envp: TJavaObjectArray<JString>): JProcess; cdecl; overload;
    function exec(prog: JString; envp: TJavaObjectArray<JString>; directory: JFile): JProcess; cdecl; overload;
    function exec(progArray: TJavaObjectArray<JString>): JProcess; cdecl; overload;
    function exec(progArray: TJavaObjectArray<JString>; envp: TJavaObjectArray<JString>): JProcess; cdecl; overload;
    function exec(progArray: TJavaObjectArray<JString>; envp: TJavaObjectArray<JString>; directory: JFile): JProcess; cdecl; overload;
    procedure exit(code: Integer); cdecl;
    function freeMemory: Int64; cdecl;
    procedure halt(code: Integer); cdecl;
    procedure load(absolutePath: JString); cdecl;
    procedure loadLibrary(nickname: JString); cdecl;
    function maxMemory: Int64; cdecl;
    function removeShutdownHook(hook: JThread): Boolean; cdecl;
    procedure runFinalization; cdecl;
    function totalMemory: Int64; cdecl;
    procedure traceInstructions(enable: Boolean); cdecl;
    procedure traceMethodCalls(enable: Boolean); cdecl;
  end;
  TJRuntime = class(TJavaGenericImport<JRuntimeClass, JRuntime>) end;

implementation

end.

