unit DW.SysUtils.Helpers;

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
  // RTL
  System.SysUtils;

type
  TOSVersionHelper = record helper for TOSVersion
  public
    class function IsIOSSimulator: Boolean; static;
    class function IsWindows: Boolean; static;
  end;

implementation

{ TOSVersionHelper }

class function TOSVersionHelper.IsIOSSimulator: Boolean;
begin
  Result := (TOSVersion.Platform = TOSVersion.TPlatform.pfiOS) and
    (TOSVersion.Architecture in [TOSVersion.TArchitecture.arIntelX86, TOSVersion.TArchitecture.arIntelX64]);
end;

class function TOSVersionHelper.IsWindows: Boolean;
begin
  Result := TOSVersion.Platform = TOSVersion.TPlatform.pfWindows;
end;

end.
