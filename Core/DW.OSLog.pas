unit DW.OSLog;

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

{$SCOPEDENUMS ON}

type
  TLogType = (Debug, Warning, Error);

  /// <summary>
  ///   Operating System specific logging
  /// </summary>
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TOSLog = record
  private
    class var FEnabled: Boolean;
    class var FIncludeDeviceSummary: Boolean;
    class var FTag: string;
    /// <summary>
    ///   Timestamps ASrc if prefixed with an '@'
    /// </summary>
    class function ts(const ASrc: string): string; static;
    class function FormatMsg(const AFmt: string; const AParams: array of const): string; static;
    class procedure LogCloud(AFmt: string; const AParams: array of const; const ALogType: TLogType); static;
  public
    /// <summary>
    ///   Replacement functions for IFMXLoggingService
    /// </summary>
    class procedure d(const AFmt: string; const ACloud: Boolean = False); overload; static;
    class procedure d(const AInstance: TObject; const AFmt: string; const ACloud: Boolean = False); overload; static;
    class procedure d(const AFmt: string; const AParams: array of const; const ACloud: Boolean = False); overload; static;
    class procedure e(const AFmt: string; const ACloud: Boolean = False); overload; static;
    class procedure e(const AFmt: string; const AParams: array of const; const ACloud: Boolean = False); overload; static;
    class procedure w(const AFmt: string; const ACloud: Boolean = False); overload; static;
    class procedure w(const AFmt: string; const AParams: array of const; const ACloud: Boolean = False); overload; static;
    /// <summary>
    ///   Retrieves the OS stack trace. ANDROID ONLY at present
    /// </summary>
    /// <remarks>
    ///   Can be useful for working out "how the OS arrived here" when implementing methods of Android interfaces
    /// </remarks>
    class function GetTrace: string; static;
    /// <summary>
    ///   Dumps a stack trace to the OS log. ANDROID ONLY at present
    /// </summary>
    /// <remarks>
    ///   Can be useful for working out "how the OS arrived here" when implementing methods of Android interfaces
    /// </remarks>
    class procedure Trace; static;
    class property Enabled: Boolean read FEnabled write FEnabled;
    class property IncludeDeviceSummary: Boolean read FIncludeDeviceSummary write FIncludeDeviceSummary;
    class property Tag: string read FTag write FTag;
  end;

const
  cLogTypeCaptions: array[TLogType] of string = ('DEBUG', 'WARN', 'ERROR');

implementation

uses
  // RTL
  System.SysUtils,
  System.TypInfo,
  // Grijjy
  {$IF Defined(CLOUDLOGGING)}
  Grijjy.CloudLogging,
  {$ENDIF}
  // DW
  DW.OSDevice,
  {$IF Defined(ANDROID)}
  DW.OSLog.Android;
  {$ELSEIF Defined(MACOS)}
  DW.OSLog.Mac;
  {$ELSEIF Defined(MSWINDOWS)}
  DW.OSLog.Win;
  {$ELSEIF Defined(LINUX)}
  DW.OSLog.Linux;
  {$ENDIF}

{ TOSLog }

class function TOSLog.FormatMsg(const AFmt: string; const AParams: array of const): string;
begin
  if Length(AParams) > 0 then
    Result := Format(AFmt, AParams)
  else
    Result := AFmt;
end;

class function TOSLog.ts(const ASrc: string): string;
var
  LUseTimestamp: Boolean;
begin
  Result := ASrc;
  LUseTimestamp := False;
  if Result.StartsWith('@') then
  begin
    LUseTimestamp := True;
    Result := Result.Substring(1);
  end;
  {$IF not Defined(ANDROID)}
  if not FTag.IsEmpty then
    Result := Format('@%s %s', [FTag, Result]);
  {$ENDIF}
  if LUseTimestamp then
    Result := Format('%s - %s', [FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Now), Result]);
end;

class procedure TOSLog.LogCloud(AFmt: string; const AParams: array of const; const ALogType: TLogType);
{$IF Defined(CLOUDLOGGING)}
var
  LMsg: string;
  LLogLevel: TgoLogLevel;
begin
  if AFmt.StartsWith('@') then
    AFmt := AFmt.Substring(1);
  LMsg := Format(AFmt, AParams);
  if FIncludeDeviceSummary then
    LMsg := TOSDevice.GetDeviceSummary + ': ' + LMsg;
  LLogLevel := TgoLogLevel.Info;
  case ALogType of
    TLogType.Warning:
      LLogLevel := TgoLogLevel.Warning;
    TLogType.Error:
      LLogLevel := TgoLogLevel.Error;
  end;
  GrijjyLog.Send(LMsg, LLogLevel);
end;
{$ELSE}
begin

end;
{$ENDIF}

class procedure TOSLog.d(const AFmt: string; const ACloud: Boolean = False);
begin
  if FEnabled then
    TPlatformOSLog.Log(TLogType.Debug, FormatMsg(ts(AFmt), []));
end;

class procedure TOSLog.d(const AFmt: string; const AParams: array of const; const ACloud: Boolean = False);
begin
  if FEnabled then
  begin
    TPlatformOSLog.Log(TLogType.Debug, FormatMsg(ts(AFmt), AParams));
    if ACloud then
      LogCloud(AFmt, AParams, TLogType.Debug);
   end;
end;

class procedure TOSLog.e(const AFmt: string; const ACloud: Boolean = False);
begin
  if FEnabled then
  begin
    TPlatformOSLog.Log(TLogType.Error, FormatMsg(ts(AFmt), []));
    if ACloud then
      LogCloud(AFmt, [], TLogType.Error);
   end;
end;

class procedure TOSLog.d(const AInstance: TObject; const AFmt: string; const ACloud: Boolean = False);
var
  LTypeData: PTypeData;
begin
  LTypeData := nil;
  if AInstance <> nil then
    LTypeData := GetTypeData(AInstance.ClassInfo);
  if LTypeData <> nil then
    d(AFmt.Substring(0, 1) + LTypeData^.UnitNameFld.ToString + '.' + AInstance.ClassName + '.' + AFmt.Substring(1), ACloud)
  else
    d(AFmt, ACloud);
end;

class procedure TOSLog.e(const AFmt: string; const AParams: array of const; const ACloud: Boolean = False);
begin
  if FEnabled then
  begin
    TPlatformOSLog.Log(TLogType.Error, FormatMsg(ts(AFmt), AParams));
    if ACloud then
      LogCloud(AFmt, AParams, TLogType.Error);
  end;
end;

class procedure TOSLog.w(const AFmt: string; const ACloud: Boolean = False);
begin
  if FEnabled then
  begin
    TPlatformOSLog.Log(TLogType.Warning, FormatMsg(ts(AFmt), []));
    if ACloud then
      LogCloud(AFmt, [], TLogType.Warning);
  end;
end;

class procedure TOSLog.w(const AFmt: string; const AParams: array of const; const ACloud: Boolean = False);
begin
  if FEnabled then
  begin
    TPlatformOSLog.Log(TLogType.Warning, FormatMsg(ts(AFmt), AParams));
    if ACloud then
      LogCloud(AFmt, AParams, TLogType.Warning);
  end;
end;

class function TOSLog.GetTrace: string;
begin
  {$IF Defined(ANDROID)}
  Result := TPlatformOSLog.GetTrace;
  {$ELSE}
  Result := '';
  {$ENDIF}
end;

class procedure TOSLog.Trace;
begin
  {$IF Defined(ANDROID)}
  TPlatformOSLog.Trace;
  {$ENDIF}
end;

initialization
  TOSLog.Enabled := True;

end.
