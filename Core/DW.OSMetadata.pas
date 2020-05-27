unit DW.OSMetadata;

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

type
  TOSMetadata = record
  public
    class function ContainsKey(const AKey: string): Boolean; static;
    class function GetValue(const AKey: string; var AValue: string): Boolean; static;
  end;

implementation

uses
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

end.