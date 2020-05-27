unit DW.Androidapi.JNI.Util;

{*******************************************************}
{                                                       }
{                      Kastri                           }
{                                                       }
{         Delphi Worlds Cross-Platform Library          }
{                                                       }
{    Copyright 2020 Dave Nottage under MIT license      }
{  which is located in the root folder of this library  }
{                                                       }
{*******************************************************}

{$I DW.GlobalDefines.inc}

interface

uses
  // Android
  Androidapi.JNIBridge, Androidapi.JNI.JavaTypes;

type
  JTimerTask = interface;
  JTimer = interface;
  Jutil_Log = interface;

  JTimerClass = interface(JObjectClass)
    ['{07C8270D-52FF-4B70-B364-A4E86A4F3411}']
    function init: JTimer; cdecl; overload;
    function init(name: JString): JTimer; cdecl; overload;
    function init(name: JString; isDaemon: boolean): JTimer; cdecl; overload;
    function init(isDaemon: boolean): JTimer; cdecl; overload;
  end;

  [JavaSignature('java/util/Timer')]
  JTimer = interface(JObject)
    ['{25D25103-F3A3-417F-AE53-7B434258D54D}']
    procedure cancel; cdecl;
    function purge: Integer; cdecl;
    procedure schedule(task: JTimerTask; delay: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; delay: Int64; period: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; firstTime: JDate; period: Int64); cdecl; overload;
    procedure schedule(task: JTimerTask; time: JDate); cdecl; overload;
    procedure scheduleAtFixedRate(task: JTimerTask; delay: Int64; period: Int64); cdecl; overload;
    procedure scheduleAtFixedRate(task: JTimerTask; firstTime: JDate; period: Int64); cdecl; overload;
  end;
  TJTimer = class(TJavaGenericImport<JTimerClass, JTimer>)
  end;

  JTimerTaskClass = interface(JObjectClass)
    ['{8A91DFA8-92B7-49CE-88DB-931B6D4D679F}']
  end;

  [JavaSignature('java/util/TimerTask')]
  JTimerTask = interface(JObject)
    ['{E5CEEEE4-88C0-4488-9E7C-86249258C30E}']
    function cancel: boolean; cdecl;
    procedure run; cdecl;
    function scheduledExecutionTime: Int64; cdecl;
  end;
  TJTimerTask = class(TJavaGenericImport<JTimerTaskClass, JTimerTask>)
  end;

  Jutil_LogClass = interface(JObjectClass)
    ['{62108FE8-1DBB-4C4F-A0C7-35D12BD116DC}']
    {class} function _GetASSERT: Integer; cdecl;
    {class} function _GetDEBUG: Integer; cdecl;
    {class} function _GetERROR: Integer; cdecl;
    {class} function _GetINFO: Integer; cdecl;
    {class} function _GetVERBOSE: Integer; cdecl;
    {class} function _GetWARN: Integer; cdecl;
    {class} function d(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function d(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function e(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function getStackTraceString(tr: JThrowable): JString; cdecl;
    {class} function i(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function i(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function isLoggable(tag: JString; level: Integer): Boolean; cdecl;
    {class} function println(priority: Integer; tag: JString; msg: JString): Integer; cdecl;
    {class} function v(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function v(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function w(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function w(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString): Integer; cdecl; overload;
    {class} function wtf(tag: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} function wtf(tag: JString; msg: JString; tr: JThrowable): Integer; cdecl; overload;
    {class} property ASSERT: Integer read _GetASSERT;
    {class} property DEBUG: Integer read _GetDEBUG;
    {class} property ERROR: Integer read _GetERROR;
    {class} property INFO: Integer read _GetINFO;
    {class} property VERBOSE: Integer read _GetVERBOSE;
    {class} property WARN: Integer read _GetWARN;
  end;

  [JavaSignature('android/util/Log')]
  Jutil_Log = interface(JObject)
    ['{6A5EC34E-CB76-4AB0-A11D-7CCB3B40C571}']
  end;
  TJutil_Log = class(TJavaGenericImport<Jutil_LogClass, Jutil_Log>) end;

implementation

end.
