unit DW.OSLog.Linux;

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
  // DW
  DW.OSLog;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSLog = record
  public
    class procedure Log(const ALogType: TLogType; const AMsg: string); static;
  end;

implementation

uses
  // RTL
  System.SysUtils;

{ TPlatformOSLog }

class procedure TPlatformOSLog.Log(const ALogType: TLogType; const AMsg: string);
begin
  Writeln(cLogTypeCaptions[ALogType] + ': ' + AMsg);
end;

end.
