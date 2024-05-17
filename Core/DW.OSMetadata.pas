unit DW.OSMetadata;

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

type
  TOSMetadata = record
  public
    class function ContainsKey(const AKey: string): Boolean; static;
    class function GetValue(const AKey: string; var AValue: string): Boolean; overload; static;
    class function GetValue(const AKey: string; var AValue: Int64): Boolean; overload; static;
  end;

implementation

uses
  // RTL
  System.SysUtils,
  // DW
  {$IF Defined(ANDROID)}
  DW.OSMetadata.Android;
  {$ELSEIF Defined(MACOS)}
  DW.OSMetadata.Mac;
  {$ELSEIF Defined(MSWINDOWS)}
  DW.OSMetadata.Win;
  {$ENDIF}

{ TOSMetadata }

class function TOSMetadata.ContainsKey(const AKey: string): Boolean;
begin
  Result := TPlatformOSMetadata.ContainsKey(AKey);
end;

class function TOSMetadata.GetValue(const AKey: string; var AValue: string): Boolean;
begin
  Result := TPlatformOSMetadata.GetValue(AKey, AValue);
end;

class function TOSMetadata.GetValue(const AKey: string; var AValue: Int64): Boolean;
{$IF not Defined(ANDROID)}
var
  LStringValue: string;
{$ENDIF}
begin
  {$IF Defined(ANDROID)}
  Result := TPlatformOSMetadata.GetValue(AKey, AValue);
  {$ELSE}
  Result := TPlatformOSMetadata.GetValue(AKey, LStringValue) and TryStrToInt64(LStringValue, AValue);
  {$ENDIF}
end;

end.