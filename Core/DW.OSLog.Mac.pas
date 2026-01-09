unit DW.OSLog.Mac;

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
  // DW
  DW.OSLog;

type
  /// <remarks>
  ///   DO NOT ADD ANY FMX UNITS TO THESE FUNCTIONS
  /// </remarks>
  TPlatformOSLog = record
  private
    class var FImageHeader: Pointer;
    class var FLogHandle: Pointer;
  public
    class procedure Log(const ALogType: TLogType; const AMsg: string); static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // macOS
  Macapi.Helpers,
  {$IF Defined(OSX)}
  Macapi.Foundation;
  {$ELSE}
  iOSapi.Foundation;
  {$ENDIF}

const
  libSystem = '/usr/lib/libSystem.dylib';
  libdyld = '/usr/lib/system/libdyld.dylib';

  OS_LOG_TYPE_DEFAULT = $00;
  OS_LOG_TYPE_INFO = $01;
  OS_LOG_TYPE_DEBUG = $02;
  OS_LOG_TYPE_ERROR = $10;
  OS_LOG_TYPE_FAULT = $11;

type
  os_log_t = Pointer;
  os_log_type_t = Byte;

function os_log_create(subsystem: MarshaledAString; category: MarshaledAString): os_log_t; cdecl;
  external libSystem;
function os_log_type_enabled(oslog: os_log_t; type_: os_log_type_t): Boolean; cdecl;
  external libSystem;
procedure _os_log_impl(dso: Pointer; log: os_log_t; type_: os_log_type_t; format: MarshaledAString; buf: PByte; size: Cardinal); cdecl;
  external libSystem;
function _dyld_get_image_header(image_index: Cardinal): Pointer; cdecl;
  external libdyld;

function GetMainBundle: NSBundle;
begin
  {$IF Defined(IOS) and (CompilerVersion > 36)}
  Result := TNSBundle.OCClass.mainBundle;
  {$ELSE}
  Result := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
  {$ENDIF}
end;

function GetBundleIdentifier: string;
var
  LValue: Pointer;
begin
  Result := '';
  LValue := GetMainBundle.infoDictionary.objectForKey(StringToID('CFBundleIdentifier'));
  if LValue <> nil then
    Result := NSStrToStr(TNSString.Wrap(LValue));
end;

{ TPlatformOSLog }

class procedure TPlatformOSLog.Log(const ALogType: TLogType; const AMsg: string);
var
  LBuf: UInt16;
  M: TMarshaller;
begin
  if FLogHandle = nil then
    FLogHandle := os_log_create(M.AsAnsi(GetBundleIdentifier).ToPointer, 'Debug');
  if FImageHeader = nil then
    FImageHeader := _dyld_get_image_header(0);
  LBuf := 0;
  _os_log_impl(FImageHeader, FLogHandle, OS_LOG_TYPE_DEFAULT, M.AsAnsi(cLogTypeCaptions[ALogType] + ': ' + AMsg).ToPointer, @LBuf, SizeOf(LBuf));
end;

end.
