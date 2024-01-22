unit DW.OSLog.Android;

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
  Androidapi.JNI.JavaTypes,
  // DW
  DW.OSLog;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSLog = record
  private
    class var FTag: JString;
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
  LMsg: JString;
begin
  if FTag = nil then
    FTag := StringToJString(TOSLog.Tag);
  LMsg := StringToJString(AMsg);
  case ALogType of
    TLogType.Debug:
      TJLog.JavaClass.d(FTag, LMsg);
    TLogType.Warning:
      TJLog.JavaClass.w(FTag, LMsg);
    TLogType.Error:
      TJLog.JavaClass.e(FTag, LMsg);
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

