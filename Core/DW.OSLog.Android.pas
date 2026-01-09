unit DW.OSLog.Android;

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
  Androidapi.JNI.JavaTypes,
  // DW
  DW.OSLog;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSLog = record
  public
    class function GetTrace: string; static;
    class procedure Log(const ALogType: TLogType; const AMsg: string); static;
    class procedure Trace; static;
  end;

implementation

uses
  // RTL
  System.SysUtils, System.IOUtils,
  // Android
  Androidapi.Helpers,
  // DW
  DW.Androidapi.JNI.Android.Util, DW.OSDevice.Android;

{ TPlatformOSLog }

class procedure TPlatformOSLog.Log(const ALogType: TLogType; const AMsg: string);
var
  LTag, LMsg: JString;
begin
  LTag := StringToJString(TOSLog.Tag);
  LMsg := StringToJString(AMsg);
  case ALogType of
    TLogType.Debug:
      TJLog.JavaClass.d(LTag, LMsg);
    TLogType.Error:
      TJLog.JavaClass.e(LTag, LMsg);
    TLogType.Fatal:
      TJLog.JavaClass.wtf(LTag, LMsg);
    TLogType.Info:
      TJLog.JavaClass.i(LTag, LMsg);
    TLogType.Verbose:
      TJLog.JavaClass.v(LTag, LMsg);
    TLogType.Warning:
      TJLog.JavaClass.w(LTag, LMsg);
  end;
end;

class function TPlatformOSLog.GetTrace: string;
begin
  Result := JStringToString(TJLog.JavaClass.getStackTraceString(TJException.JavaClass.init));
end;

class procedure TPlatformOSLog.Trace;
begin
  Log(TLogType.Debug, GetTrace);
end;

end.

